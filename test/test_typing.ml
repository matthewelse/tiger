open! Core
open! Helpers

let test_typing s = test_typing [%string "let %{s} in () end"]

let%expect_test _ =
  test_typing "type a = { x : int }";
  [%expect {| Unit |}];
  test_typing "type a = { x : int }   var x := nil";
  [%expect {| Unit |}];
  test_typing
    "type tree = int type asdf = { x : int } function treeLeaves(t : tree) : asdf = nil";
  [%expect {| Unit |}];
  test_typing
    "type tree = int type asdf = { x : int } function treeLeaves(t : tree) : asdf = nil";
  [%expect {| Unit |}];
  test_typing "var x := 1 + 2 * 3";
  [%expect {| Unit |}];
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    test_typing "type a = b type b = c type c = a");
  [%expect {|
    (Failure "Illegal cycle in type definition a") |}]
;;
