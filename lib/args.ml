open Util

let usage =
  {|[ --server ] with optional port
[ --client ] with optional ipv4 address & port
[ --help ] prints usage|}

(* We don't have many options so simply match on all supported combinations.
   I would refactor this as a fold over args, ideally. *)
let parse = function
  | [ "--help" ] ->
      print_endline usage;
      exit 0
  | [ "--server" ] -> Result.ok @@ StartServer default_port
  | [ "--client" ] -> Result.ok @@ StartClient (default_address, default_port)
  | [ "--client"; addr ] -> (
      match validate_uri addr with
      | Ok (addr, port) -> Result.ok @@ StartClient (addr, port)
      | Error e -> Error (Printf.sprintf "Invalid address/hostname: %s " e))
  | [ "--server"; port ] -> (
      match validate_port port with
      | Ok port -> Result.ok @@ StartServer port
      | Error e -> Error (Printf.sprintf "Invalid port: %s" e))
  | _ ->
      Result.error
      @@ Printf.sprintf
           "Received unknown or malformed arguments.\nExpected:\n\n%s\n" usage

let get_args () =
  let args = List.tl @@ Array.to_list Sys.argv in
  parse args
