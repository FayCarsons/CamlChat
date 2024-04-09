open Lwt
open Syntax
open OUnit
open OUnitLwt

let rand_string () =
  let len = Random.int 192 + 64 in
  let bytes = Bytes.create len in
  for i = 0 to pred len do
    Bytes.unsafe_set bytes i (Random.int 230 |> Char.chr)
  done;
  Bytes.unsafe_to_string bytes

(* IO/Server tests *)

let message = Bytes.of_string "Hello, Ahrefs!"
let message_len = Bytes.length message

let get_io () =
  let reader, writer = Lwt_unix.pipe () in
  let input = Lwt_io.(of_fd ~mode:Input reader) in
  let output = Lwt_io.(of_fd ~mode:Output writer) in
  (input, output)

let read =
  "Test Protocol.read"
  >:: lwt_wrapper @@ fun _ ->
      let buf = Bytes.create message_len in

      let input, output = get_io () in

      let* _ = Lwt_io.write_int32 output (Int32.of_int message_len) in
      let* _ = Lwt_io.write_from output message 0 message_len in
      Protocol.read input buf >>= function
      | Ok (Received len) ->
          assert_equal ~cmp:Bytes.equal buf message;
          assert_equal len message_len;
          Lwt.return_unit
      | _ -> assert_failure "Received something besides message sent"

let read_fuzz =
  "Test Protocol.read with large input of arbitrary bytes"
  >:: lwt_wrapper @@ fun _ ->
      let input, output = get_io () in

      let fuzz_len = 1024 * 32 in
      let write_buf =
        let b = Bytes.create fuzz_len in
        for i = 0 to pred fuzz_len do
          Bytes.set b i (Char.chr @@ Random.int 256)
        done;
        b
      in

      let read_buf = Bytes.create fuzz_len in
      let* _ = Lwt_io.write_int32 output (Int32.of_int fuzz_len) in
      let* _ = Lwt_io.write_from output write_buf 0 fuzz_len in
      Protocol.read input read_buf >>= function
      | Ok (Received len) ->
          assert_equal len fuzz_len;
          assert_equal ~cmp:Bytes.equal write_buf read_buf;
          Lwt.return_unit
      | _ -> assert_failure "Bytes altered in transit"

let write =
  "Test Protocol.send"
  >:: lwt_wrapper @@ fun _ ->
      let input, output = get_io () in
      let* _ = Protocol.send output (message_len, message) >|= Result.get_ok in
      let* _ = Lwt_io.flush output in

      let read_buf = Bytes.create message_len in
      let* count = Lwt_io.read_int32 input >|= Int32.to_int in
      let* _ = Lwt_io.read_into input read_buf 0 message_len in
      assert_equal count message_len;
      assert_equal read_buf message;
      Lwt.return_unit

let write_fuzz =
  "Test Protocol.send with large input of arbitrary bytes"
  >:: lwt_wrapper @@ fun () ->
      let input, output = get_io () in
      let fuzz_len = 1024 * 32 in
      let write_buf =
        let b = Bytes.create fuzz_len in
        for i = 0 to pred fuzz_len do
          Bytes.set b i (Char.chr @@ Random.int 256)
        done;
        b
      in

      let* _ = Protocol.send output (fuzz_len, write_buf) >|= Result.get_ok in
      let* _ = Lwt_io.flush output in
      let read_buf = Bytes.create fuzz_len in
      let* count = Lwt_io.read_int32 input in
      let* _ = Lwt_io.read_into_exactly input read_buf 0 fuzz_len in
      assert_equal count (Int32.of_int fuzz_len);
      assert_equal ~cmp:Bytes.equal write_buf read_buf;
      Lwt.return_unit

let acknowledged =
  "Test recognization of 'acknowledged' flag in Protocol.read"
  >:: lwt_wrapper @@ fun () ->
      let input, output = get_io () in
      let buf = Bytes.create 1024 in
      let* _ = Lwt_io.write_int32 output 0xBEEFCAFEl in
      let* _ = Lwt_io.flush output in

      Protocol.read input buf ~client:true >|= Result.get_ok >>= fun res ->
      assert_equal Protocol.Acknowledged res;
      Lwt.return_unit

(* Arg parsing *)

let validate_port =
  "Test Util.validate_port" >:: fun _ ->
  let valid = [ "8080"; "3000"; "80"; "443"; "4173"; "5173" ] in
  let res = List.map Util.validate_port valid in
  assert (List.for_all Result.is_ok res);

  let invalid =
    [
      "0";
      "abcd";
      "Lorem ipsum dolor sit amet, qui minim labore adipisicing minim sint";
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

  let invalid =
    [
      "Camel Camel Camel";
      "0.0.0.0.0.0.0.0.0.0.0.0.0.0";
      "127.0.0.1:0";
      "ahrefs.com:F0o3aR";
      "0.0.0.0" ^ string_of_int Int.max_int;
      rand_string ();
    ]
  in
  let res = List.map Util.validate_uri invalid in
  assert (List.for_all Result.is_error res)

let parse_args =
  "Test Args.parse" >:: fun _ ->
  assert_equal
    (Args.parse [ "--server"; "3000" ])
    (Result.ok @@ Util.StartServer 3000);
  assert_equal (Args.parse [ "--server" ]) (Result.ok @@ Util.StartServer 8080);
  assert_equal
    (Args.parse [ "--client" ])
    (Result.ok @@ Util.StartClient (Unix.inet_addr_loopback, 8080));
  assert_equal
    (Args.parse [ "--client"; "127.0.0.1:8080" ])
    (Result.ok @@ Util.StartClient (Unix.inet_addr_loopback, 8080));
  let try_parse arg_list = assert (Result.is_error @@ Args.parse arg_list) in
  try_parse [];
  try_parse [ ""; ""; ""; "" ];
  try_parse [ "password:"; "hunter2" ];
  let open Util in
  try_parse @@ List.init 4 (ignore >> rand_string)

let suite =
  "Chat app tests"
  >::: [
         read;
         write;
         acknowledged;
         validate_port;
         validate_uri;
         parse_args;
         read_fuzz;
         write_fuzz;
       ]

let _ = OUnit.run_test_tt_main suite
