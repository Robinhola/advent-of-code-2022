open! Base
open! Core

let read_lines name ~year ?for_tests () =
  let path = "input/advent_" ^ year ^ "/" ^ name ^ ".in" in
  try Stdio.In_channel.read_lines path with
  _ -> for_tests |> Option.value ~default:[]
;;

let solve (module M: Day.T) =
  print_endline M.name;
  printf "part1:\t%i\n" M.part1;
  printf "part2:\t%i\n" M.part2;
;;

let solve_string (module M: Day.T_string) =
  print_endline M.name;
  printf "part1:\t%s\n" M.part1;
  printf "part2:\t%s\n" M.part2;
;;
