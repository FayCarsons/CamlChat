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
      (** Mailbox used to communicate between tasks *)
}
(** Application state *)

(** Size of read buffer *)
let buffer_size = 1024 * 50

(** Filters strings so they may be displayed without affecting the 
    behavior of the console 

    Allow newlines, prinable chars, carriage return, and tab only *)
let sanitize s =
  let is_valid = function
    | '\x20' .. '\x7E' | '\x0A' | '\x0D' | '\x09' -> true
    | _ -> false
  in
  String.filter ~f:is_valid s

module Server = struct
  open Protocol

  (** We only want to allow one client connection at a time *)
  let backlog = 1

  (** Concurrently checks the mailbox for messages that 
      signal changes in state, and listens to stdin for messages. 

      We call Lwt.pick on these processes so that if one resolves (`check_mail`
      if no errors have occured) we can recurse with the new state it receives *)
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

  (** Messages sender, waits for a new client, and reinitializes state *)
  let reinitialize sock state =
    let* _ = Lwt_mvar.put state.mailbox Closed in
    let* input, output =
      Lwt.map fst (Lwt_unix.accept sock) >>= create_channels
    in
    let new_state = { state with input; output } in
    let* _ = Lwt_mvar.put new_state.mailbox @@ New (input, output) in
    Lwt.return new_state

  (** Reads from connection and handles IO and control flow *)
  let listen_loop (sock, state) =
    let rec listener ({ input; output; buf; _ } as state) () =
      read input buf >>= function
      | Ok (Received len) ->
          let* _ = Lwt_io.write_int32 output acknowledged in
          let msg = Bytes.To_string.sub buf ~pos:0 ~len |> sanitize in
          let msg = sanitize msg in
          Lwt_io.printl msg >>= listener state
      | Ok Closed ->
          let* _ =
            Lwt_io.printl "Client closed connection, listening for another. . ."
          in
          let* new_state = reinitialize sock state in
          let* _ = Lwt_io.printl "Accepted connection from new client!" in
          listener new_state ()
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
    let* sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) |> bind_socket port in
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

module Client = struct
  open Protocol

  (** Takes time (of sent messsage) and returns a formatted 
      string of the elapsed time *)
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

  (** Reads from connection until it is closed. 
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

  let create_socket addr port =
    let sockaddr = Lwt_unix.ADDR_INET (addr, port) in
    let fd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
    try%lwt
      let* _ = Lwt_unix.connect fd sockaddr in
      let* _ = Lwt_io.printl "Connected!" in
      Lwt.return fd
    with _ ->
      Lwt_io.eprintl
      @@ Printf.sprintf "Cannot connect to server %s:%d"
           (Unix.string_of_inet_addr addr)
           port
      >>= exit 1

  (** Connects to server, creates initial state then starts listen
      and send loops *)
  let init address port =
    let* _ = Lwt_io.printl "Connecting to server..\nType 'quit' to exit\n" in
    let* input, output = create_socket address port >>= create_channels in
    let mailbox = Lwt_mvar.create_empty () in
    let buf = Bytes.create buffer_size in
    let state = { input; output; mailbox; buf } in
    (* Use Lwt.pick so that the program shuts down when the connection is closed *)
    Lwt.pick [ listen_loop state; send_loop state ]

  (** Entrypoint *)
  let start address port = Lwt_main.run @@ init address port
end
