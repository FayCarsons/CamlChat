open Lwt
open Syntax
open OUnit
open OUnitLwt
module App = ChatApp.Connection
module Util = ChatApp.Util
module Args = ChatApp.Args

let message = "Hello, Ahrefs!"

let get_io () =
  let reader, writer = Lwt_unix.pipe () in
  let input = Lwt_io.(of_fd ~mode:Input reader) in
  let output = Lwt_io.(of_fd ~mode:Output writer) in
  (input, output)

let read =
  "Test Protocol.read"
  >:: lwt_wrapper @@ fun _ ->
      let buf = Bytes.create 1024 in

      let input, output = get_io () in

      let* _ =
        Lwt_io.write_int32 output (Int32.of_int @@ String.length message)
      in
      let* _ = Lwt_io.write output message in
      App.Protocol.read input buf >>= function
      | Ok (Received msg) ->
          let msg = String.of_bytes msg in
          assert_equal msg message;
          Lwt.return_unit
      | _ -> assert_failure "Received something besides message sent"

let write =
  "Test Protocol.write"
  >:: lwt_wrapper @@ fun _ ->
      let input, output = get_io () in
      let* _ = App.Protocol.send output message >|= Result.get_ok in
      let* _ = Lwt_io.flush output in

      let* count = Lwt_io.read_int32 input >|= Int32.to_int in
      let* res = Lwt_io.read ~count input in
      assert_equal count (String.length message);
      assert_equal res message;
      Lwt.return_unit

let acknowledged =
  "Test recognization of 'acknowledged' flag in Protocol.read"
  >:: lwt_wrapper @@ fun () ->
      let input, output = get_io () in
      let buf = Bytes.create 1024 in
      let* _ = Lwt_io.write_int32 output 0xBEEFCAFEl in
      let* _ = Lwt_io.flush output in

      App.Protocol.read input buf ~client:true >|= Result.get_ok >>= fun res ->
      assert_equal App.Acknowledged res;
      Lwt.return_unit

let validate_port =
  "Test Util.validate_port" >:: fun _ ->
  let valid = [ "8080"; "3000"; "80"; "443"; "4173"; "5173" ] in
  let res = List.map Util.validate_port valid in
  assert (List.for_all Result.is_ok res);

  let invalid =
    [
      "0";
      "abcd";
      "Unix.Unix_error (Unix.EUNKNOWNERR, \" \", \" \")";
      string_of_int Int.max_int;
    ]
  in
  let res = List.map Util.validate_port invalid in
  assert (List.for_all Result.is_error res)

let validate_uri =
  "Test Util.validate_uri" >:: fun _ ->
  let valid =
    [ "127.0.0.1:8080"; "0.0.0.0:3000"; "ahrefs.com:443"; "google.com:80" ]
  in
  let res = List.map Util.validate_uri valid in
  assert (List.for_all Result.is_ok res);

  let fuzz =
    let bytes = Bytes.create 128 in
    for i = 0 to 127 do
      Bytes.set_uint8 bytes i (Random.int 256)
    done;
    String.of_bytes bytes
  in
  let invalid =
    [
      "A long winding road";
      "0.0.0.0.0.0.0.0.0.0.0.0.0.0";
      "127.0.0.1:0";
      "ahrefs.com:F0o3aR";
      "0.0.0.0" ^ string_of_int Int.max_int;
      fuzz;
    ]
  in
  let res = List.map Util.validate_uri invalid in
  assert (List.for_all Result.is_error res)

let parse_args =
  "Test Args.parse" >:: fun _ ->
  assert_equal (Args.parse [ "--server"; "3000" ]) (Util.StartServer 3000);
  assert_equal (Args.parse [ "--server" ]) (Util.StartServer 8080);
  assert_equal
    (Args.parse [ "--client" ])
    (Util.StartClient (Unix.inet_addr_loopback, 8080));
  assert_equal
    (Args.parse [ "--client"; "127.0.0.1:8080" ])
    (Util.StartClient (Unix.inet_addr_loopback, 8080));
  assert_equal
    (Args.parse [ "--client"; "127.0.0.1:8080"; "--file"; "/dev/null" ])
    (Util.SendFile (Unix.inet_addr_loopback, 8080, "/dev/null"));
  let try_parse arg_list =
    assert (
      try
        Args.parse arg_list |> ignore;
        false
      with Args.ParseError _ -> true)
  in
  try_parse [];
  try_parse [ ""; ""; ""; "" ];
  try_parse [ "password:"; "hunter2" ]

let suite =
  "Chat app tests"
  >::: [ read; write; acknowledged; validate_port; validate_uri; parse_args ]

let _ = OUnit.run_test_tt_main suite
