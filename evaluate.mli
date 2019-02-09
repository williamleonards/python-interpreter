open Parser
open State

(** [evaluate input st] returns the new state with the expression part of 
    input evaluated to a value and added to the state if input has a string. 
    Otherwise, it prints out the value of the expression part of input **)
val evaluate : string option * Parser.expr -> State.t -> State.t

(**[eval] directly evaluates expressions.*)
val eval : expr -> State.t -> State.value

(**[to_bool] evaluates expression to value and determines if it is true
   or false.*)
val to_bool : expr -> State.t -> bool