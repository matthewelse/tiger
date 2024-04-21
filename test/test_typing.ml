open! Core
open! Helpers

let test_typing' s = test_typing [%string "let %{s} in () end"]

let%expect_test _ =
  test_typing' "type a = { x : int }";
  [%expect {| Unit |}];
  test_typing' "type a = { x : int }   var x : a := nil";
  [%expect {| Unit |}];
  test_typing'
    "type tree = int type asdf = { x : int } function treeLeaves(t : tree) : asdf = nil";
  [%expect {| Unit |}];
  test_typing'
    "type tree = int type asdf = { x : int } function treeLeaves(t : tree) : asdf = nil";
  [%expect {| Unit |}];
  test_typing' "var x := 1 + 2 * 3";
  [%expect {| Unit |}];
  Expect_test_helpers_core.require_does_raise [%here] (fun () ->
    test_typing' "type a = b type b = c type c = a");
  [%expect {|
    (Failure "Illegal cycle in type definition a") |}];
  test_typing
    {|let
      type list = { x : int, rest : list }
      var l : list :=  list { x = 1, rest = nil }
  in
  l
end|};
  [%expect
    {|
    (Record
     (fields
      ((x (Name (name int) (type_ (<opaque>))))
       (rest (Name (name list) (type_ (<opaque>))))))
     (id 5)) |}]
;;
