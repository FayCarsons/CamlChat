type init =
  | StartServer of port
  | StartClient of (Unix.inet_addr * port)
  | SendFile of (Unix.inet_addr * port * path)

and port = int
and path = string

let default_address = Unix.inet_addr_loopback
let default_port = 8080
let ( >> ) f g x = g @@ f x

(* Arg validation *)

(** Checks that port string is integer and in range of useable ports. 
    Ideally would check if port is open, bu implementing this introduces
    unpredictable behavior wrt port binding in app *)
let validate_port maybe_port =
  let in_range = function
    | n when 0 < n && n <= 65535 -> Ok n
    | _ -> Error "Port not in valid range"
  in
  int_of_string_opt maybe_port
  |> Option.to_result ~none:"Port is not an integer"
  |> Fun.flip Result.bind in_range

(** Checks that uri contains valid inet address and port *)
let validate_uri maybe_uri =
  match String.split_on_char ':' maybe_uri with
  | [ addr; port ] -> (
      try
        let addr = Unix.inet_addr_of_string addr in
        let port =
          match validate_port port with
          | Ok port -> port
          | Error err -> failwith err
        in
        Ok (addr, port)
      with _ -> Error "Unable to parse ipv4/ipv6 portion of address")
  | _ -> Error "Invalid address format, expecting \'{ipv4|ipv6}:{port}\'"

(** Checks that file exists and process has permission access it *)
let validate_path path =
  if Sys.file_exists path then
    try
      Unix.access path [ Unix.R_OK ];
      Ok path
    with Unix.Unix_error (err, _, _) -> Result.error @@ Unix.error_message err
  else Result.error @@ Printf.sprintf "Cannot find file: %s" path
