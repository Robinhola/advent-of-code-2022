val read_lines : string -> year:string -> ?for_tests:string list -> unit -> string list

val solve : (module Day.T) -> unit
val solve_string : (module Day.T_string) -> unit