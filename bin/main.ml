open Util
module Server = Chat.Server
module Client = Chat.Client

let () =
  match Args.get_args () with
  | Ok (StartServer port) -> Server.start port
  | Ok (StartClient (address, port)) -> Client.start address port
  | Ok (SendFile (address, port, path)) -> Client.start_file address port path
  | Error e ->
      print_endline e;
      exit 0
