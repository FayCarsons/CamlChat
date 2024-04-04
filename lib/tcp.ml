open Lwt
open Syntax

type connection = {
  input : Lwt_io.input_channel;
  output : Lwt_io.output_channel;
  buf : Bytes.t;
  mailbox : status Lwt_mvar.t;
}

and status = Received of Bytes.t | Acknowledged | Closed | New of connection

let backlog = 1
let acknowledged = Int32.of_int 0xBEEFCAFE
let prefix_bytes = 4
let create_socket () = Lwt_unix.(socket PF_INET SOCK_STREAM 0)

let create_channels sock =
  let open Lwt_io in
  let* _ = Logs_lwt.debug (fun f -> f "Creating ic & oc") in
  let ic = of_fd ~mode:Input sock in
  let oc = of_fd ~mode:Output sock in
  Lwt.return (ic, oc)

let take_prefix input = Lwt.map Int32.to_int @@ Lwt_io.read_int32 input

let serialize msg =
  let msg = Bytes.of_string msg in
  let len = Bytes.length msg in
  let prefix = Bytes.create prefix_bytes in
  Bytes.set_int32_be prefix 0 (Int32.of_int len);
  (len, Bytes.cat prefix msg)

let send output msg =
  let* _ = Logs_lwt.debug (fun f -> f "Attempting to send message: %s" msg) in
  let len, bytes = serialize msg in
  Lwt_io.write_from_exactly output bytes 0 len

let read ?(wait_ping = false) input buf =
  let* _ = Logs_lwt.debug (fun f -> f "Reading from connection. . .") in
  let* prefix = Lwt_io.read_int32 input in
  let* _ = Logs_lwt.debug (fun f -> f "Got prefix %d" @@ Int32.to_int prefix) in
  if wait_ping && prefix = acknowledged then
    let* _ = Logs_lwt.debug (fun f -> f "Got acknowledged message") in
    Lwt.return_ok Acknowledged
  else
    try
      Lwt_io.read_into_exactly input buf prefix_bytes (Int32.to_int prefix)
      >>= fun () ->
      let* _ =
        Logs_lwt.debug (fun f -> f "Received %d bytes" (Bytes.length buf))
      in
      Lwt.return_ok @@ Received buf
    with
    | End_of_file -> Lwt.return_ok Closed
    | e -> Lwt.return_error @@ Printexc.to_string e

module Server = struct
  type t = Lwt_unix.file_descr * connection

  let update state (input, output) = Lwt.return { state with input; output }

  let close { input; output; _ } =
    Lwt_io.close input >>= fun () -> Lwt_io.close output

  let send_acknowledged output =
    let* _ = Logs_lwt.debug (fun f -> f "Sending acknowledged message") in
    Lwt_io.write_int32 output acknowledged

  let listener (sock, state) =
    let rec listener' ({ input; output; mailbox; buf; _ } as state) () =
      let* _ = Logs_lwt.debug (fun f -> f "Listening to input channel") in
      read input buf >>= function
      | Ok (Received bytes) ->
          let* _ =
            Logs_lwt.debug (fun f ->
                f "Received %d bytes from client" (Bytes.length bytes))
          in
          let* _acknowledged = send_acknowledged output in
          Lwt_io.printl (String.of_bytes bytes) >>= listener' state
      | Ok Closed ->
          let* _ = Logs_lwt.debug (fun f -> f "Client closed connection") in
          let* _ =
            Lwt_io.printl "Client closed connection, listening for another. . ."
          in
          let* _ = close state in
          let* new_client, _ = Lwt_unix.accept sock in
          let* new_conn = create_channels new_client in
          let* new_state = update state new_conn in
          Lwt_mvar.put mailbox (New new_state) >>= listener' new_state
      | Error e ->
          let* _ = Logs_lwt.err (fun f -> f "Error reading: %s" e) in
          Lwt_io.eprintl e >>= listener' state
      | _ -> listener' state ()
    in
    listener' state ()

  let sender (_, state) =
    let rec sender' ({ output; mailbox; _ } as state) =
      Lwt_io.(read_line_opt stdin) >>= function
      | Some msg -> (
          let* _ =
            Logs_lwt.debug (fun f -> f "Got message %s from stdin" msg)
          in
          send output msg >>= fun () ->
          match Lwt_mvar.take_available mailbox with
          | Some Closed -> (
              Lwt_mvar.take mailbox >>= function
              | New conn -> sender' conn
              | _ -> sender' state)
          | Some (New conn) -> sender' conn
          | _ -> sender' state)
      | None -> Lwt.return ()
    in
    sender' state

  let bind_socket port fd =
    let open Lwt_unix in
    let* _ = bind fd @@ ADDR_INET (Util.default_address, port) in
    listen fd backlog;
    let* _ =
      Logs_lwt.debug (fun f -> f "Lwt_unix.accept completed: connected client?")
    in
    Lwt.return fd

  let start port =
    let start' () =
      let* sock = create_socket () |> bind_socket port in
      let* client, _sockaddr = Lwt_unix.accept sock in
      let* input, output = create_channels client in
      let mailbox = Lwt_mvar.create_empty () in
      let buf = Bytes.create @@ (1024 * 50) in
      let state = (sock, { input; output; mailbox; buf }) in
      Lwt.join [ sender state; listener state ]
    in
    Lwt_main.run @@ start' ()
end

module Client = struct
  type t = { conn : connection; mailbox : status Lwt_mvar.t }

  let get_elapsed time = Unix.gettimeofday () -. time

  let format_received msg time =
    let elapsed = get_elapsed time |> string_of_float in
    Printf.sprintf "%s : %ss" msg (String.sub elapsed 0 5)

  let sender { conn; mailbox } =
    let* _ = Logs_lwt.debug (fun f -> f "Client sender alive") in
    let wait_ping () = Lwt_mvar.take mailbox >>= fun _ -> Lwt.return () in
    let rec sender' ({ output; _ } as state) () =
      Lwt_io.(read_line_opt stdin) >>= function
      | Some msg ->
          Logs.debug (fun f -> f "Received text from stdin:  %s" msg);
          let start_time = Unix.gettimeofday () in
          send output msg >>= wait_ping >>= fun () ->
          Lwt_io.printl (format_received msg start_time) >>= sender' state
      | None -> Lwt_io.eprintl "Stdin unexpectedly closed!"
    in
    sender' conn ()

  let listener { conn; mailbox } =
    let* _ = Logs_lwt.debug (fun f -> f "Client listtener alive") in
    let rec listener' ({ input; output; buf; _ } as state) () =
      read ~wait_ping:true input buf >>= function
      | Ok Acknowledged ->
          let* _ =
            Logs_lwt.debug (fun f ->
                f "Received 'Acknowledged' flag from host%!")
          in
          Lwt_mvar.put mailbox Acknowledged >>= listener' state
      | Ok (Received bytes) ->
          let* _ =
            Logs_lwt.debug (fun f ->
                f "Received %d bytes from server%!" (Bytes.length bytes))
          in
          Lwt_io.printl (String.of_bytes bytes) >>= listener' state
      | Ok Closed ->
          Lwt_io.printl "Server closed connection" >>= fun () ->
          Lwt_io.close output >>= fun () -> Lwt_io.close input >>= Lwt.return
      | Error e -> Lwt_io.eprintl e >>= listener' state
      | _ -> listener' state ()
    in
    listener' conn ()

  let connect addr port =
    let sockaddr = Lwt_unix.ADDR_INET (addr, port) in
    let fd = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
    let* _ = Lwt_unix.connect fd sockaddr in
    let* _ = Logs_lwt.debug (fun f -> f "Connected to server!") in
    let* _ =
      Logs_lwt.debug (fun f ->
          f "Connected to server: %s:%d%!" (Unix.string_of_inet_addr addr) port)
    in
    let* input, output = create_channels fd in
    let mailbox = Lwt_mvar.create_empty () in
    let buf = Bytes.create (1024 * 50) in
    Lwt.return { input; output; mailbox; buf }

  let create_client address port =
    let* conn = connect address port in
    let mailbox = Lwt_mvar.create_empty () in
    Lwt.return { conn; mailbox }

  let init address port =
    let* _ =
      Logs_lwt.debug (fun f ->
          f "Starting client: attempting to connect to %s:%d"
            (Unix.string_of_inet_addr address)
            port)
    in
    let* state = create_client address port in
    Lwt.join [ listener state; sender state ]

  let start address port =
    Logs.set_reporter (Logs.reporter ());
    Logs.set_level (Some Logs.Debug);
    Lwt_main.run @@ init address port
end
