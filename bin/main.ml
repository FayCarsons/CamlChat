open Util
module Server = Chat.Server
module Client = Chat.Client

let () =
  match Args.get_args () with
  | Ok (StartServer port) -> Server.start port
  | Ok (StartClient (address, port)) -> Client.start address port
  | Error e ->
      print_endline e;
      exit 1
