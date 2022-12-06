open! Base
open! Core

let read_lines name ~year ?for_tests () =
  let path = "input/advent_" ^ year ^ "/" ^ name ^ ".in" in
  try Stdio.In_channel.read_lines path with
  _ -> for_tests |> Option.value ~default:[]
;;
