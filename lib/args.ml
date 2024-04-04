open Util

let ( let* ) = Result.bind

let validate_port maybe_port =
  let in_range = function
    | n when 0 < n && n <= 65535 -> Ok n
    | _ -> Error "Port not in valid range"
  in
  int_of_string_opt maybe_port
  |> Option.to_result ~none:"Port is not an integer"
  |> Fun.flip Result.bind in_range

let validate_uri maybe_uri =
  match String.split_on_char ':' maybe_uri with
  | [ addr; port ] -> (
      try
        let addr = Unix.inet_addr_of_string addr in
        let* port = validate_port port in
        Ok (addr, port)
      with _ -> Error "Unable to parse ipv4/ipv6 portion of address")
  | _ -> Error "Invalid address format, expecting \'{ipv4|ipv6}:{port}\'"

let get_args () =
  let get_mode = function
    | [ "-h" ] | [ "--host" ] -> StartServer default_port
    | [ "-c" ] | [ "--client" ] -> StartClient (default_address, default_port)
    | [ "--client"; addr ] | [ "-c"; addr ] -> (
        match validate_uri addr with
        | Ok (addr, port) -> StartClient (addr, port)
        | Error e -> failwith (Printf.sprintf "Invalid address: %s " e))
    | [ "--server"; port ] | [ "-s"; port ] -> (
        match validate_port port with
        | Ok port -> StartServer port
        | Error e -> failwith (Printf.sprintf "Invalid port: %s" e))
    | _ ->
        failwith
          {|
            Received unknown or malformed arguments: 
            Expected either [-c / --client] with optional ipv4 address 
            or [-s / --server ] with optional port
          |}
  in
  let args = Array.to_list Sys.argv |> List.tl in
  get_mode args
