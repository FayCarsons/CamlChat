open ChatApp
open Util
module Server = Connection.Server
module Client = Connection.Client

let () =
  match Args.get_args () with
  | StartServer port -> Server.start port
  | StartClient (address, port) -> Client.start address port
  | SendFile (address, port, path) -> Client.start_file address port path
