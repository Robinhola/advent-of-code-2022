open! Base
open! Core

module type T = sig
  val name : string
  val part1 : int
  val part2 : int
end