open Util

let usage =
  {|
  [ --client ] with optional ipv4 address
  [ --file ] with a path value (only available in client mode)
  [ --server ] with optional port
  [ --help ] prints this message 
  |}

(* We don't have many options so simply match on all valid combinations *)
let parse_args = function
  | [ "--help" ] ->
      print_endline usage;
      exit 0
  | [ "--host" ] -> StartServer default_port
  | [ "--client" ] -> StartClient (default_address, default_port)
  | [ "--client"; addr ] -> (
      match validate_uri addr with
      | Ok (addr, port) -> StartClient (addr, port)
      | Error e -> failwith (Printf.sprintf "Invalid address/hostname: %s " e))
  | [ "--client"; addr; "--file"; path ] | [ "--file"; path; "--client"; addr ]
    -> (
      match (validate_uri addr, validate_path path) with
      | Ok (addr, port), Ok path -> SendFile (addr, port, path)
      | Error err, _ -> failwith @@ Printf.sprintf "Malformed URI: %s" err
      | _, Error err -> failwith @@ Printf.sprintf "Invalid file path: %s" err)
  | [ "--server"; port ] -> (
      match validate_port port with
      | Ok port -> StartServer port
      | Error e -> failwith (Printf.sprintf "Invalid port: %s" e))
  | _ ->
      failwith
      @@ Printf.sprintf
           {|
            Received unknown or malformed arguments: 
            Expected %s
          |}
           usage

let get_args () =
  let args = List.tl @@ Array.to_list Sys.argv in
  parse_args args
