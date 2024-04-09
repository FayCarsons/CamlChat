(** Mode agnostic IO functions, initializers, and constants describing the 
    protocol the app uses. 

    The protocol is simple, the first four bytes of a message indicate either 
    its length or the 'acknowledged' flag that the server sends to the client 
    when a message is received. The actual message body follows. 

    We make no assumptions about encoding or the type of data sent and received, 
    dealing only with bytes *)

open Lwt
open Syntax
module String = Base.String

(** Events passed from function to function or process to process *)
type event =
  | Received of int
      (** Message received, with message length so we can blit to string from our 
          read buffer *)
  | Acknowledged  (** Server acknowledged last message *)
  | Closed  (** Connection closed *)
  | New of Lwt_io.input_channel * Lwt_io.output_channel
      (** New connection opened *)

exception Fatal of string

let acknowledged = 0xBEEFCAFEl

let create_channels socket' =
  let open Lwt_io in
  let ic = of_fd ~mode:Input socket' in
  let oc = of_fd ~mode:Output socket' in
  Lwt.return (ic, oc)

(** Converts message to a Bytes.t and returns it and its length *)
let serialize src =
  let bytes = Bytes.of_string src in
  let len = Bytes.length bytes in
  (len, bytes)

(** An abstraction over reading/writing bytes recursively. 
    Calls io_fn, a function that take two ints representing an offset and a length and 
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

(** Writes len to the output channel, and then recursively writes 
    bytes *)
let send output (len, bytes) =
  let* _ = Lwt_io.write_int32 output (Int32.of_int len) in
  io_loop (Lwt_io.write_from output bytes) len

(** Reads from the input channel, first attempting to get prefix. If the prefix
    is not the flag, is zero, or we're not n client mode, then we read 
    recursively until we've read {prefix} bytes. 

    Returns an event describing the result of this operation. *)
let read ?(client = false) input buf =
  try%lwt
    Lwt_io.read_int32 input >>= function
    | n when client && Int32.equal n acknowledged -> Lwt.return_ok Acknowledged
    | 0l -> Lwt.return_ok @@ Received 0
    | prefix -> (
        let len = Int32.to_int prefix in
        io_loop (Lwt_io.read_into input buf) len >>= function
        | Ok () -> Lwt.return_ok @@ Received len
        | Error _ -> Lwt.return_error Closed)
  with
  | End_of_file -> Lwt.return_ok Closed
  | Unix.Unix_error (msg, _, _) -> raise @@ Fatal (Unix.error_message msg)
  | exn -> raise exn
