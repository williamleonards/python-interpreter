type value = Int of int | Float of float | String of string 
           | Bool of bool | VList of value list ref 
           | Function of (string * string list * string)
           | NoneVal | Dictionary of (value * value) list ref

type t = (string*value) list

val empty : t 

val insert : string -> value -> t -> t

val find : string -> t -> value option

val member : string -> t -> bool