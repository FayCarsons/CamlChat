open Lwt
open Syntax

type connection = {
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
  buf : Bytes.t;  (** Buffer that received bytes are read into *)
  mailbox : event Lwt_mvar.t;
      (** Mailbox used to communicate between processes *)
}
(** Application state *)

(** Events passed from function to function or process to process *)
and event =
  | Received of Bytes.t  (** Message received *)
  | Acknowledged  (** Server acknowledged last message *)
  | Closed  (** Connection closed *)
  | New of connection  (** New connection opened *)

exception Fatal of string
(** raised when we encounter an error we cannot recovr from *)

(** Mode agnostic IO functions, initializers, and constants describing the protocol the app uses *)
module Protocol = struct
  (** Number of bytes in the message's prefix,
      which is either an "Acknowledged" flag or the length of the following message *)
  let prefix_bytes = 4

  (** The four bytes the server sends to the client when a message is received *)
  let acknowledged = 0xBEEFCAFEl

  (** Size of read buffer *)
  let buffer_size = 1024 * 50

  (* returns a file descriptor representing stream/tcp socket *)
  let create_socket () = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

  (** returns two Lwt_io.channel, one input one output *)
  let create_channels socket' =
    let open Lwt_io in
    let ic = of_fd ~mode:Input socket' in
    let oc = of_fd ~mode:Output socket' in
    Lwt.return (ic, oc)

  (** converts message to a Bytes.t and returns it and its length *)
  let serialize msg =
    let msg = Bytes.of_string msg in
    let len = Bytes.length msg |> Int32.of_int in
    (len, msg)

  (** an abstraction over reading/writing bytes recursively. 
      Calls f, a function that take two ints representing an offset and a length and 
      returns a third int representing the number of bytes read/written, recursively 
      until total accumulated bytes equals len or an error is encountered *)
  let io_loop io_fn len =
    let rec io_loop' done_ remaining =
      io_fn done_ remaining >>= function
      | 0 when done_ < len -> Lwt.return_error Closed
      | n when done_ + n = len -> Lwt.return_ok ()
      | n ->
          let done_ = done_ + n in
          let remaining = len - done_ in
          io_loop' done_ remaining
    in
    io_loop' 0 len

  (** converts message string to Bytes.t, 
      writes its length to the output channel, and then recursively writes 
      the message's bytes *)
  let send output msg =
    let len, bytes = serialize msg in
    Lwt_io.write_int32 output len >>= fun () ->
    io_loop (Lwt_io.write_from output bytes) (Int32.to_int len)

  (** reads from the input channel, first attempting to get a 'prefix' representing either 
      the messagess length or the 'acknowldged' flag. If the prefix is not the flag and is 
      greater than zero then we read recursively until we have {prefix} bytes *)
  let read ?(client = false) input buf =
    let read' () =
      Lwt_io.read_int32 input >>= function
      (* If we're in client-mode, check if prefix is the "acknowledged" flag *)
      | n when client && n = acknowledged -> Lwt.return_ok Acknowledged
      (* 0 length lines are still prefixed with length, skip reading entirely if we receive one *)
      | 0l -> Lwt.return_ok @@ Received Bytes.empty
      (* Otherwise read recursively until we've received {prefix} bytes *)
      | prefix -> (
          let len = Int32.to_int prefix in
          io_loop (Lwt_io.read_into input buf) len >>= function
          | Error _ -> Lwt.return_error Closed
          | Ok () ->
              let msg = Bytes.create len in
              Bytes.blit buf 0 msg 0 len;
              Lwt.return_ok @@ Received msg)
    in
    Lwt.catch read' (function
      | End_of_file -> Lwt.return_ok Closed
      | exn -> raise exn)
end

(** Server/host mode listener, sender, initialization and helper functions *)
module Server = struct
  open Protocol

  (** The Sender process, concurrently checks our mailbox for messages that 
      signal changes in state and listens to stdin for messages. 

      We call Lwt.pick on these processes so that if one resolves (typcially `check_mail`)
      we can recurse with the new state it receives *)
  let rec sender ({ input; output; mailbox; _ } as state) =
    let rec check_mail () =
      Lwt_mvar.take mailbox >>= function
      | Closed -> check_mail ()
      | New state -> Lwt.return state
      | _unreachable ->
          Lwt_io.eprintl "Internal error: program entered unreachable branch"
          >>= fun () -> Lwt.return state
    in
    let rec read_stdin () =
      Lwt_io.(read_line_opt stdin) >>= function
      | Some "quit" ->
          let* _ = Lwt_io.close input in
          let* _ = Lwt_io.close output in
          exit 0
      | Some msg ->
          let* _ = send output msg in
          read_stdin ()
      | None -> raise (Fatal "Stdin closed unexpectedly")
    in
    Lwt.pick [ check_mail (); read_stdin () ] >>= fun state -> sender state

  (* We only want to allow one client connection at a time *)
  let backlog = 1

  (** The Listener process, reads from connection recursively and handles variant returned *)
  let listener (sock, state) =
    let rec listener' ({ input; output; buf; mailbox } as state) () =
      read input buf >>= function
      (* A normal message, print and recurse *)
      | Ok (Received msg) ->
          let* _acknowledged = Lwt_io.write_int32 output acknowledged in
          let msg = String.of_bytes msg in
          Lwt_io.printl msg >>= listener' state
      (* Closed in an acceptable state, reinitialize and send messages to sender
         process *)
      | Ok Closed ->
          let* _ =
            Lwt_io.printl "Client closed connection, listening for another. . ."
          in
          let* _ = Lwt_mvar.put mailbox Closed in
          let* new_client, _ = Lwt_unix.accept sock in
          let* _ = Lwt_io.printl "Accepted connection from new client" in
          let* input, output = create_channels new_client in
          let new_state = { state with input; output } in
          let* _ = Lwt_mvar.put mailbox @@ New new_state in
          listener' new_state ()
      (* Closed in an error state *)
      | Error Closed ->
          raise (Fatal "Client connection closed with an unknown error")
      (* This should never happen but is included for completeness *)
      | Error _unreachable ->
          raise (Fatal "Server encountered an unknown error")
      (* For everything else we recurse *)
      | _ -> listener' state ()
    in
    listener' state ()

  (** Binds a socket to an address, sets it up for listening and returns the socket *)
  let bind_socket port fd =
    let open Lwt_unix in
    let* _ = bind fd @@ ADDR_INET (Util.default_address, port) in
    listen fd backlog;
    Lwt.return fd

  (** Creates socket and binds to port, waits for a connection, initializes state, 
      and then starts the sender and listener process with that state *)
  let init port =
    let* _ =
      Lwt_io.printl @@ Printf.sprintf "Starting server on port %d" port
    in
    let* sock = create_socket () |> bind_socket port in
    let* client, _sockaddr = Lwt_unix.accept sock in
    let* input, output = create_channels client in
    let mailbox = Lwt_mvar.create_empty () in
    let buf = Bytes.create buffer_size in
    let state = { input; output; mailbox; buf } in
    Lwt.join [ sender state; listener (sock, state) ]

  (** Entrypoint *)
  let start port = Lwt_main.run @@ init port
end

(** Client listener/sender functionality and helper fns *)
module Client = struct
  open Protocol

  (* Regular behavior, when started with the "--client" flag *)

  (** Collects chars from a string_of_float until a non-zero digit is found *)
  let take_first_nonzero s =
    let rec take i acc =
      if i >= String.length s then acc
      else
        match s.[i] with
        | '1' .. '9' as digit -> acc ^ String.make 1 digit
        | c -> take (succ i) (acc ^ String.make 1 c)
    in
    take 0 ""

  (** takes time (of sent messsage) and returns a formatted 
      string of the elapsd time*)
  let show_elapsed time =
    let elapsed = Unix.gettimeofday () -. time |> string_of_float in
    Printf.sprintf "> %ss" @@ take_first_nonzero elapsed

  (** The sender process, reads from stdin, awaits an 'ackowledged' message, 
      and then recurses. 

      Unlike the server-mode sender, we don't need to restart with new state when the connection 
      is closed so this can be done in sequence *)
  let sender state =
    let rec sender' ({ output; mailbox; input; _ } as state) () =
      Lwt_io.(read_line_opt stdin) >>= function
      | Some "quit" ->
          let* _ = Lwt_io.close output in
          let* _ = Lwt_io.close input in
          exit 0
      | Some msg ->
          let start_time = Unix.gettimeofday () in
          let* _sent = send output msg in
          let* _ =
            Lwt_mvar.take mailbox >>= function
            | Acknowledged -> Lwt_io.printl (show_elapsed start_time)
            | _ -> sender' state ()
          in
          sender' state ()
      | None -> raise (Fatal "stdin closed unexpectedly")
    in
    sender' state ()

  (** The listener process. Reads from connection recursively until it is closed. 
      If the 'acknowleedged' flag is received we forward it to the sender so it can display the elapsed time. 

      Because we cannot know the state of the server we raise a fatal error if the read reads 0 bytes when a message 
      was expected (`Error Closed` is only returned in this case) *)
  let listener state =
    let rec listener' ({ input; buf; mailbox; _ } as state) () =
      read ~client:true input buf >>= function
      | Ok Acknowledged -> Lwt_mvar.put mailbox Acknowledged >>= listener' state
      | Ok (Received bytes) ->
          Lwt_io.printl (String.of_bytes bytes) >>= listener' state
      | Ok Closed -> Lwt_io.printl "Server closed connection" >>= exit 0
      | Error Closed ->
          raise (Fatal "Server connection closed with unknown error")
      | _ -> listener' state ()
    in
    listener' state ()

  (** initializes a socket, connects to server, and 
      returns an input and output channel for that connection *)
  let get_channels addr port =
    let sockaddr = Lwt_unix.ADDR_INET (addr, port) in
    let fd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
    let* _ = Lwt_unix.connect fd sockaddr in
    let* _ = Lwt_io.printl "Connected to server!" in
    create_channels fd

  (** connects to server and creates initial state then starts listener 
      and sender processses *)
  let init address port =
    let* _ = Lwt_io.printl "Connecting to server . . ." in
    let* input, output = get_channels address port in
    let mailbox = Lwt_mvar.create_empty () in
    let buf = Bytes.create buffer_size in
    let state = { input; output; mailbox; buf } in
    Lwt.join [ listener state; sender state ]

  (** Entrypoint *)
  let start address port = Lwt_main.run @@ init address port

  (*
      File sending mode

      Used when started in client mode with the "--file" flag,
      sends a file, immediately closes the connection and exits
  *)

  (* Size of buffer used for file sending *)
  let chunk_size = 1024

  (** opens a file and creates a buffer, reading {chunks_size} bytes into the 
      buffer and then sending them to the server  

      This is non-critical functionality I implemented to see if I could, 
      therefore it is perhaps not as robust as the regular functionality. 
      Error handling could be improved *)
  let send_file input output path =
    let* fd = Lwt_unix.openfile path [ Unix.O_RDONLY ] 0 in
    let* fsize = Lwt_unix.(stat path >|= fun stats -> stats.st_size) in
    let num_chunks = fsize / chunk_size in
    let in_channel = Lwt_io.(of_fd ~mode:Input fd) in
    let buf = Bytes.create chunk_size in
    let start_time = Unix.gettimeofday () in
    let rec send_chunk i =
      Lwt_io.read_into in_channel buf 0 chunk_size >>= function
      | 0 -> Lwt.return_unit
      | read ->
          let* _prefix = Lwt_io.write_int32 output @@ Int32.of_int read in
          let* _send = Lwt_io.write_from_exactly output buf 0 read in
          (* Ensure we receive acknowledged flag as to not overload the server.
             If we're on the last chunk print the total elapsed time once the
             flag has been received *)
          let* _ =
            Lwt_io.read_int32 input >|= ignore
            >>=
            if i < num_chunks then Lwt.return
            else fun () -> Lwt_io.printl (show_elapsed start_time)
          in
          send_chunk (succ i)
    in
    send_chunk 0

  (** Creates output channel and sends file, catching and printing any exceptions *)
  let init_send_file address port path =
    let* input, output = get_channels address port in
    let* _ =
      let open Util in
      Lwt.catch
        (fun () -> send_file input output path)
        (Printexc.to_string >> Lwt_io.eprintl)
    in
    (* Cleanup *)
    Lwt_io.close output

  (** Entrypoint for file mode *)
  let start_file address port path =
    Lwt_main.run (init_send_file address port path)
end
