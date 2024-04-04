type init = StartServer of port | StartClient of (Unix.inet_addr * port)
and port = int

let default_address = Unix.inet_addr_loopback
let default_port = 8080
let ( >> ) f g x = g @@ f x
