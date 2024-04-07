open Lwt
open Syntax
open OUnit
open OUnitLwt
module App = ChatApp.Connection

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

let suite = "Chat app tests" >::: [ read; write; acknowledged ]
let _ = OUnit.run_test_tt_main suite
