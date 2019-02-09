open State
open Error

type op = Plus | Minus | Divide | Floor_Divide | Multiply | Modular | Exponent 
        | Equal | Not_Equal | Greater_Than | Less_Than | Greater_Equal 
        | Less_Equal | And | Or | Not | Complement

type expr = Binary of (expr * op * expr) | Unary of (op * expr) 
          | Value of State.value | Variable of string | List of expr list 
          | Function of (string * expr list)
          | Dictionary of expr list
          | ListComp of (expr * string * expr * expr option)

type line_type = Assignment | Expression | If of (expr * string) 
               | Empty | Else | Line of string | Elif of (expr * string) 
               | While of (expr * string) | Def of (string * string list * string)
               | Return of (expr) | For of (expr * string * string)

exception EmptyInput
exception IfMultiline of (expr * string)
exception WhileMultiline of (expr * string)
exception DefMultiline of (string * string list * string)
exception ForMultiline of (expr * string * string)
exception ReturnExpr of expr

val parse_line : string -> string option * expr

val line_type : string -> line_type

val parse_multiline : string -> line_type

val indent_depth : string -> int

val add_depth : string -> int -> string