open! Base
open! Core
open Advent_2022

let () = print_endline "Welcome to 2022! Let the advent of code... begin!!!"

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

let () = solve (module Day01.T)
let () = solve (module Day02.T)
let () = solve (module Day03.T)
let () = solve (module Day04.T)
let () = solve_string (module Day05.T)
let () = solve (module Day06.T)