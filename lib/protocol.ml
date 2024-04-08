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
(** raised when we encounter an error we cannot recover from *)

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
let serialize src =
  let bytes = Bytes.of_string src in
  let len = Bytes.length bytes |> Int32.of_int in
  (len, bytes)

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

(**  Writes len to the output channel, and then recursively writes 
      bytes *)
let send output (len, bytes) =
  let* _ = Lwt_io.write_int32 output len in
  io_loop (Lwt_io.write_from output bytes) (Int32.to_int len)

(** reads from the input channel, first attempting to get a 'prefix' representing either 
      the messagess length or the 'acknowldged' flag. If the prefix is not the flag and is 
      greater than zero then we read recursively until we've read {prefix} bytes. 

      Returns an event describing the result of this operation. 

      @param client 
      determines whether or not we check for the `acknowledged` flag in the prefix *)
let read ?(client = false) input buf =
  let read' () =
    Lwt_io.read_int32 input >>= function
    (* If we're in client-mode, check if prefix is the "acknowledged" flag *)
    | n when client && Int32.equal n acknowledged -> Lwt.return_ok Acknowledged
    (* 0 length lines are still prefixed with length, skip reading entirely if we receive one *)
    | 0l -> Lwt.return_ok @@ Received 0
    (* Otherwise read recursively until we've received {prefix} bytes *)
    | prefix -> (
        let len = Int32.to_int prefix in
        io_loop (Lwt_io.read_into input buf) len >>= function
        | Ok () -> Lwt.return_ok @@ Received len
        | Error _ -> Lwt.return_error Closed)
  in
  (* Catch EOF at prefix read and pass `Closed` event *)
  Lwt.catch read' (function
    | End_of_file -> Lwt.return_ok Closed
    | Unix.Unix_error (msg, _, _) -> raise @@ Fatal (Unix.error_message msg)
    | exn -> raise exn)
