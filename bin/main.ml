open ChatApp
open Util

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);
  match Args.get_args () with
  | StartServer port -> Tcp.Server.start port
  | StartClient (address, port) -> Tcp.Client.start address port
