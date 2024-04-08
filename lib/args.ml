open Util

let usage =
  {|[ --client ] with optional ipv4 address & port
[ --file ] with a path value (only available in client mode, and requires ipv4 & port)
[ --server ] with optional port
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
  | [ "--client"; addr; "--file"; path ] | [ "--file"; path; "--client"; addr ]
    -> (
      match (validate_uri addr, validate_path path) with
      | Ok (addr, port), Ok path -> Result.ok @@ SendFile (addr, port, path)
      | Error err, _ -> Error (Printf.sprintf "Malformed URI: %s" err)
      | _, Error err -> Error (Printf.sprintf "Invalid file path: %s" err))
  | [ "--client"; "--file"; path ] | [ "file"; path; "--client" ] ->
      validate_path path
      |> Result.map (fun path -> SendFile (default_address, default_port, path))
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
