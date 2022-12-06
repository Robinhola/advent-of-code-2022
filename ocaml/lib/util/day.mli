open! Base
open! Core

module type T = sig
  val name : string
  val part1 : int
  val part2 : int
end

module type T_string = sig
  val name : string
  val part1 : string
  val part2 : string
end