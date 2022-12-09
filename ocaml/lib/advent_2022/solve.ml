open! Base
open! Core
open Import

open Advent

let all () =
  print_endline "Welcome to 2022! Let the advent of code... begin!!!";
  solve (module Day01.T);
  solve (module Day02.T);
  solve (module Day03.T);
  solve (module Day04.T);
  solve_string (module Day05.T);
  solve (module Day06.T);
  solve (module Day07.T);
  solve (module Day08.T);
  solve (module Day09.T);
;;
