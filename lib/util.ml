type init =
  | StartServer of int
  | StartClient of (Unix.inet_addr * int)
  | SendFile of (Unix.inet_addr * int * string)

let default_address = Unix.inet_addr_loopback
let default_port = 8080
let ( >> ) f g x = g @@ f x

(* Arg validation *)

let port_in_range = function
  | n when 0 < n && n <= 65535 -> Ok n
  | _ -> Error "Port not in valid range"

(** Checks that port string is integer and in range of useable ports. 
    Ideally would check if port is open, bu implementing this introduces
    unpredictable behavior wrt port binding in app *)
let validate_port maybe_port =
  int_of_string_opt maybe_port
  |> Option.to_result ~none:"Port is not an integer"
  |> Fun.flip Result.bind port_in_range

(** Checks that uri contains either a valid inet_adress or hostname *)
let validate_uri uri_str =
  try
    let maybe_uri = Uri.of_string @@ "//" ^ uri_str in
    match (Uri.host maybe_uri, Uri.port maybe_uri) with
    | Some host, Some port ->
        let addr =
          (Unix.gethostbyname host).h_addr_list |> Fun.flip Array.get 0
        in
        Result.map (fun port -> (addr, port)) @@ port_in_range port
    | _ -> Error "address/hostname must contain a host and a port!"
  with Unix.Unix_error (msg, _, _) ->
    Result.error
    @@ Printf.sprintf "Invalid address/hostname: %s" (Unix.error_message msg)

(** Checks that file exists and process has permission access it *)
let validate_path path =
  if Sys.file_exists path then
    try
      Unix.access path [ Unix.R_OK ];
      Ok path
    with Unix.Unix_error (err, _, _) -> Result.error @@ Unix.error_message err
  else Result.error @@ Printf.sprintf "Cannot find file: %s" path
