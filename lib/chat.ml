(** Client and Server mode implementation. Both follow the same simple pattern 
    with separate recursive `tasks` that can asynchronously communicate with 
    each other for sending and listening for messages *)

open Lwt
open Syntax
module Bytes = Base.Bytes
module String = Base.String
module Char = Base.Char

type connection = {
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
  buf : Bytes.t;  (** Buffer that bytes are read into *)
  mailbox : Protocol.event Lwt_mvar.t;
      (** Mailbox used to communicate between processes *)
}
(** Application state *)

(** Filters strings so they may be displayed without affecting the 
     behavior of the console 

     Allow newlines, prinable chars, carriage return, and tab only *)
let sanitize s =
  let is_valid = function
    | '\x20' .. '\x7E' | '\x0A' | '\x0D' | '\x09' -> true
    | _ -> false
  in
  String.filter ~f:is_valid s

(** Server mode *)
module Server = struct
  open Protocol

  (** We only want to allow one client connection at a time *)
  let backlog = 1

  (** Concurrently checks our mailbox for messages that 
      signal changes in state, and listens to stdin for messages. 

      We call Lwt.pick on these processes so that if one resolves (ideally 
      `check_mail`) we can recurse with the new state it receives *)
  let rec send_loop ({ output; mailbox; _ } as state) =
    let rec check_mail () =
      Lwt_mvar.take mailbox >>= function
      | Closed -> check_mail ()
      | New (input, output) -> Lwt.return_some { state with input; output }
      | _unreachable ->
          Lwt_io.eprintl "Internal error: program entered unreachable branch"
          >>= fun () -> Lwt.return_some state
    in
    let rec read_stdin () =
      Lwt_io.(read_line_opt stdin) >>= function
      | Some "quit" -> Lwt.return_none
      | Some msg ->
          let* _ = serialize msg |> send output in
          read_stdin ()
      | None -> raise (Fatal "Stdin closed unexpectedly")
    in
    Lwt.pick [ check_mail (); read_stdin () ] >>= function
    | Some state -> send_loop state
    | None -> Lwt.return_unit

  (** Waits for a new client connecion and converts the returned file descriptor
      to input+output channels *)
  let wait_for_client sock = Lwt_unix.accept sock >|= fst >>= create_channels

  (** Messages sender, waits for a new client, and reinitializes state *)
  let reinitialize sock state =
    let* _ = Lwt_mvar.put state.mailbox Closed in
    let* input, output = wait_for_client sock in
    let new_state = { state with input; output } in
    let* _ = Lwt_mvar.put new_state.mailbox @@ New (input, output) in
    Lwt.return new_state

  (** Reads from connection recursively and handles IO and control flow according 
      to what it receives *)
  let listen_loop (sock, state) =
    let rec listener ({ input; output; buf; _ } as state) () =
      read input buf >>= function
      (* A normal message *)
      | Ok (Received len) ->
          let* _ = Lwt_io.write_int32 output acknowledged in
          let msg = Bytes.To_string.sub buf ~pos:0 ~len |> sanitize in
          let msg = sanitize msg in
          Lwt_io.printl msg >>= listener state
      (* Closed in an acceptable state *)
      | Ok Closed ->
          let* _ =
            Lwt_io.printl "Client closed connection, listening for another. . ."
          in
          let* new_state = reinitialize sock state in
          let* _ = Lwt_io.printl "Accepted connection from new client!" in
          listener new_state ()
      (* Closed in an error state *)
      | Error Closed ->
          let* _ =
            Lwt_io.eprintl "Client connection closed with an unknown error"
          in
          let* new_state = reinitialize sock state in
          let* _ = Lwt_io.printl "Accepted connection from new client" in
          listener new_state ()
      (* This should never happen, but is included for completeness *)
      | Error _unreachable ->
          raise (Fatal "Server encountered an unknown error")
      (* For anything else we recurse *)
      | _ -> listener state ()
    in
    listener state ()

  (** Binds a socket to an address and sets it up for listening  *)
  let bind_socket port fd =
    let open Lwt_unix in
    let _ =
      try%lwt bind fd @@ ADDR_INET (Util.default_address, port)
      with _ ->
        Lwt_io.eprintl @@ Printf.sprintf "Cannot bind to port %d" port
        >>= exit 1
    in
    listen fd backlog;
    Lwt.return fd

  (** Creates socket and binds to port, waits for a connection, initializes state
      with that connection, and then starts the send and listen loops *)
  let init port =
    let* _ =
      Lwt_io.printl
      @@ Printf.sprintf "Starting server on port %d\nType 'quit' to exit\n" port
    in
    let* sock = create_socket () |> bind_socket port in
    let* client, _sockaddr = Lwt_unix.accept sock in
    let* _ = Lwt_io.printl "Accepted connection from client!" in
    let* input, output = create_channels client in
    let mailbox = Lwt_mvar.create_empty () in
    let buf = Bytes.create buffer_size in
    let state = { input; output; mailbox; buf } in
    try%lwt Lwt.pick [ send_loop state; listen_loop (sock, state) ]
    with e -> Lwt_unix.close sock >>= raise e

  (** Entrypoint *)
  let start port = Lwt_main.run @@ init port
end

(** Client mode *)
module Client = struct
  open Protocol

  (* Regular behavior, when started with the "--client" flag *)

  (** Takes time (of sent messsage) and returns a formatted 
      string of the elapsed time*)
  let show_elapsed time =
    let elapsed = Unix.gettimeofday () -. time |> string_of_float in
    Printf.sprintf "> %ss" @@ String.sub elapsed ~pos:0 ~len:6

  (** Reads from stdin, awaits an 'acknowledged' message, 
      and then recurses. 

      Unlike the server mode sender, we don't need to restart with new state 
      when the connection is closed, so this can be done in sequence *)
  let send_loop state =
    let rec sender' ({ output; mailbox; _ } as state) () =
      Lwt_io.(read_line_opt stdin) >>= function
      | Some "quit" -> exit 0
      | Some msg ->
          let start_time = Unix.gettimeofday () in
          let* _sent = serialize msg |> send output in
          let* _ =
            Lwt_mvar.take mailbox >>= function
            | Acknowledged -> Lwt_io.printl (show_elapsed start_time)
            | _ -> sender' state ()
          in
          sender' state ()
      | None -> raise (Fatal "stdin closed unexpectedly")
    in
    sender' state ()

  (** Reads from connection recursively until it is closed. 
      If the 'acknowledged' flag is received we forward it to the sender so it 
      can display the elapsed time. 

      Because we cannot know the state of the server we raise a fatal error if 
      we receive 0 bytes when more were expected *)
  let listen_loop state =
    let rec listener' ({ input; buf; mailbox; _ } as state) () =
      read ~client:true input buf >>= function
      | Ok Acknowledged -> Lwt_mvar.put mailbox Acknowledged >>= listener' state
      | Ok (Received len) ->
          let msg = Bytes.To_string.sub buf ~pos:0 ~len |> sanitize in
          Lwt_io.printl msg >>= listener' state
      | Ok Closed -> Lwt_io.printl "Server closed connection" >>= Lwt.return
      | Error Closed ->
          raise (Fatal "Server connection closed with unknown error")
      | _ -> listener' state ()
    in
    listener' state ()

  (** Initializes a socket, connects to server, and 
      returns an input + output channel *)
  let create_socket addr port =
    let sockaddr = Lwt_unix.ADDR_INET (addr, port) in
    let fd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
    let* _ =
      try%lwt
        let* _ = Lwt_unix.connect fd sockaddr in
        Lwt_io.printl "Connected!"
      with _ ->
        Lwt_io.eprintl
        @@ Printf.sprintf "Cannot connect to server %s:%d"
             (Unix.string_of_inet_addr addr)
             port
    in
    Lwt.return fd

  (** Connects to server and creates initial state then starts listener 
      and sender processses *)
  let init address port =
    let* _ = Lwt_io.printl "Connecting to server..\nType 'quit' to exit\n" in
    let* input, output = create_socket address port >>= create_channels in
    let mailbox = Lwt_mvar.create_empty () in
    let buf = Bytes.create buffer_size in
    let state = { input; output; mailbox; buf } in
    (* Use Lwt.pick so that the program shuts down when the connection is
       closed, use try%lwt to pass any exceptions up the call stack *)
    Lwt.pick [ listen_loop state; send_loop state ]

  (** Entrypoint *)
  let start address port = Lwt_main.run @@ init address port

  (*
      <File sending mode>

      Used when started in client mode with the "--file" flag,
      sends a file, immediately closes the connection and exits
  *)

  (* Size of buffer used *)
  let chunk_size = 1024

  (** opens a file and creates a buffer, reading {chunks_size} bytes into the 
      buffer and then sending them to the server  

      This is non-critical functionality I implemented to see if I could, 
      therefore it is perhaps not as robust as the regular functionality. 
      Error handling could be improved *)
  let send_file input output path =
    (* Initialization *)
    let* fd = Lwt_unix.openfile path [ Unix.O_RDONLY ] 0 in
    let* fsize = Lwt_unix.(stat path >|= fun stats -> stats.st_size) in
    let num_chunks = fsize / chunk_size in
    let file_in = Lwt_io.(of_fd ~mode:Input fd) in
    let buf = Bytes.create chunk_size in
    let start_time = Unix.gettimeofday () in
    (* Send file in chunks *)
    let rec send_chunk i =
      Lwt_io.read_into file_in buf 0 chunk_size >>= function
      | 0 -> Lwt.return_unit (* EOF *)
      | read ->
          let* _prefix = Lwt_io.write_int32 output @@ Int32.of_int read in
          let* _send = Lwt_io.write_from_exactly output buf 0 read in
          (* Ensure we receive acknowledged flag as to not overload the server.
             If we're on the last chunk print the total elapsed time once the
             flag has been received *)
          let* _ = Lwt_io.read_int32 input in
          let* _ =
            if i = num_chunks then Lwt_io.printl @@ show_elapsed start_time
            else Lwt.return_unit
          in
          send_chunk (succ i)
    in
    send_chunk 0

  (** Creates output channel and sends file, catching and printing any 
      exceptions, then closes connection *)
  let init_send_file address port path =
    try%lwt
      let* input, output = create_socket address port >>= create_channels in
      let* _ = send_file input output path in
      Lwt_io.close output
    with e -> Lwt_io.eprintl @@ Printexc.to_string e

  (** Entrypoint for file mode *)
  let start_file address port path =
    Lwt_main.run @@ init_send_file address port path
end
