open! Base
open! Core
open Advent_2022

let () = print_endline "Welcome to 2022! Let the advent of code... begin!!!"

let solve (module M: Day.T) = 
  print_endline M.name;
  printf "part1:\t%i\n" M.part1;
  printf "part2:\t%i\n" M.part2;
;;

let () = solve (module Day01.T)
let () = solve (module Day02.T)
let () = solve (module Day03.T)
let () = solve (module Day04.T)
let () = solve (module Day05.T)
let () = solve (module Day06.T)
let () = solve (module Day07.T)
let () = solve (module Day08.T)
let () = solve (module Day09.T)
let () = solve (module Day10.T)
let () = solve (module Day11.T)
let () = solve (module Day12.T)
let () = solve (module Day13.T)
let () = solve (module Day14.T)
let () = solve (module Day15.T)
let () = solve (module Day16.T)
let () = solve (module Day17.T)
let () = solve (module Day20.T)
let () = solve (module Day21.T)
let () = solve (module Day22.T)
let () = solve (module Day23.T)
let () = solve (module Day24.T)
let () = solve (module Day25.T)