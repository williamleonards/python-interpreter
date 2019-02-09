open State
open Parser
open Error

(**[to_string] returns the string of a value*)
val to_string : State.value -> string  

(** [index lst] returns the index of the second element of lst in the first 
    element of lst in an integer value, returns Int(-1) if not found.
    The first element of lst must be either a VList or a String **)
val index : value list -> State.value 

val splice : value list -> State.value

val append : value list -> State.value

val assertt : value list -> State.value

val print : value list -> State.value 

val len : value list -> State.value 

val range : value list -> State.value 

val dict : value list -> State.value 
(** Type casts *)
val chr : value list -> State.value 

val bool : value list -> State.value

val float : value list -> State.value

val int : value list -> State.value

val list : value list -> State.value

val to_list : value list -> value list

val replace : value list -> State.value

val built_in_functions : (string * (State.value list -> State.value)) list

(**[if_decider] determines if a non-boolean-value is false or true.*)
val if_decider : State.value -> bool