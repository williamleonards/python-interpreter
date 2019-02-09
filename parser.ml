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

let operators = [[("or", Or)];
                 [("and", And);];
                 [("==", Equal);("!=", Not_Equal)];
                 [("<", Less_Than);("<=", Less_Equal);(">", Greater_Than);
                  (">=", Greater_Equal);];
                 [("not", Not)];
                 [("+", Plus);("-", Minus);];
                 [("%", Modular);("/", Divide);("//", Floor_Divide);
                  ("*", Multiply);];
                 [("**", Exponent);("~", Complement)]]

let reserved_keywords = [
  "False"; "def"; "if"; "raise"; "None"; "del"; "import"; "return"; "True";	
  "elif";	"in";	"try"; "and";	"else";	"is";	"while"; "as"; "except"; "lambda";	
  "with"; "assert";	"finally"; "nonlocal"; "yield"; "break"; "for"; "not"; 
  "class"; "from"; "or"; "continue"; "global"; "pass"]

(** [is_var_name s] is [s] if [s] is a valid variable name.
    Raises SyntaxError otherwise *) 
let is_var_name (s:string) : string = 
  let _ = let num = Char.code s.[0] in 
    if (48 <= num && num <= 57) 
    then raise (SyntaxError "invalid syntax")
    else () in
  let _ = String.map (fun x -> let num = Char.code x in 
                       if (48 <= num && num <= 57) || (65 <= num && num <= 90) 
                          || (97 <= num && num <= 122) || (num = 95) 
                       then x 
                       else raise (SyntaxError "invalid syntax")) s in 
  if List.mem s reserved_keywords 
  then raise (SyntaxError "can't assign to keyword") 
  else s

(** [not_mistaken str op] is true if [str] has not been confused to a similar 
    operator with more characters and is in fact [op]. false otherwise.*)
let not_mistaken str op =
  let oplen = String.length op in
  if String.length str = oplen then true
  (* Will mess up for cases like grand = 3 *)
  else if op = "or" || op = "and" || op = "not" then str.[oplen] = ' '
  else if op <> "*" && op <> "=" && op <> "/" && op <> "<" && op <> ">" 
          && op <> "!" then true
  else str.[oplen] <> '*' && str.[oplen] <> '=' && str.[oplen] <> '/'

(** [get_idx str op] is the index number of the first occurrence of [op] in [str]
    that is not enclosed in quotes, brackets, or parenthesis. *)
let rec get_idx (str:string) (op:string) : int =
  let strlen = String.length str in
  let oplen = String.length op in 
  if strlen < oplen then  -1
  else if String.sub str 0 oplen = op then 
    if not_mistaken str op then 0
    else get_idx_acc str 2 op
  else if str.[0] = '(' then
    (match get_idx (String.sub str 1 (strlen - 1)) ")" with 
     | -1 -> raise (SyntaxError "Missing closing paren") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = ')' then raise (SyntaxError "Missing opening paren") 
  else if str.[0] = '[' then
    (match get_idx (String.sub str 1 (strlen - 1)) "]" with 
     | -1 -> raise (SyntaxError "Missing closing bracket") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = ']' then raise (SyntaxError "Missing opening bracket") 
  else if str.[0] = '"' then
    (match String.index (String.sub str 1 (strlen -1)) '"' with 
     | exception Not_found -> raise (SyntaxError "Missing closing quote") 
     | x -> get_idx_acc str (x+2) op)
  else if str.[0] = '\'' then
    (match String.index (String.sub str 1 (strlen -1)) '\'' with 
     | exception Not_found -> raise (SyntaxError "Missing closing quote") 
     | x -> get_idx_acc str (x+2) op)
  else get_idx_acc str 1 op
and 
  (** [get_idx_acc str num op] is the index number of the first occurrence of 
      [op] in [str] that is not enclosed in quotes, brackets, or parenthesis 
      added to num. *)
  get_idx_acc (str:string) (num:int) (op:string) : int = 
  let acc = get_idx (String.sub str num (String.length str - num)) op in 
  if acc = -1 then -1 else num + acc

(** [valid_paren str] is true if [str] has every open parenthesis closed, false 
    otherwise *)
let valid_paren str = match get_idx str ")" with 
  | exception (SyntaxError x) -> false
  | x -> if x = -1 then true else false

(** [valid_bracket str] is true if [str] has every open bracket closed, false 
    otherwise *)
let valid_bracket str = match get_idx str "]" with 
  | exception (SyntaxError x) -> false
  | x -> if x = -1 then true else false

(** [rev_get_idx line op] is the index of the first occurrence of [op] in [line]
    Requires: [op] does not contain parenthesis or brackets. *)
let rev_get_idx (line:string) (op:string) : int = 
  let rev_str str = (fun int _ -> 
      let new_chr = str.[String.length str -1 -int] in
      if new_chr = '(' then ')' else if new_chr = ')' then '('
      else if new_chr = '[' then ']' else if new_chr = ']' then '['
      else new_chr) in
  let rev_idx =  get_idx (String.mapi (rev_str line) line) (String.mapi (rev_str op) op) in
  if rev_idx = -1 then -1 else String.length line - rev_idx - 1

(** [expr_contains line op] is None, max_int if none of the elements of [op] are
    in [line] or Some (string, op), int is the earliest element from [op] in 
    [line] at the int index. *)
let rec expr_contains(line:string)(op:(string*op) list): (string*op) option*int= 
  match op with
  | [] -> None, max_int
  | h :: t -> let next = expr_contains line t in 
    let current = get_idx line (fst h) in
    if current <> -1 && current < snd next then Some h, current else next

(** [trim str] is [str] with shell spaces and paren removed. *)
let rec trim str : string =
  let str = String.trim str in
  if String.length str = 0 then str
  else if str.[0] = '(' && str.[String.length str - 1] = ')' then 
    if valid_paren (String.sub str 1 (String.length str - 2))
    then trim (String.sub str 1 (String.length str - 2))
    else str
  else str

(** [split_on_char chr line] is [line] split into a list partitioned on each [chr]
    not enclosed in parenthesis, brackets, or quotes. *)
let rec split_on_char (chr:char) (line:string) : string list = 
  match get_idx line (Char.escaped chr) with
  | -1 -> line::[]
  | num -> String.sub line 0 num::
           split_on_char chr (String.sub line (num+1) (String.length line-num-1))

(** [is_assignment line] is true if [line] is an assignment statement, false 
    otherwise. *)
let is_assignment (line:string) : bool =
  let idx = get_idx line "=" in
  if idx <> -1 then 
    let prev = if idx = 0 then raise (SyntaxError "invalid syntax")
      else String.get line (idx-1) in
    let next = if idx + 1 = String.length line 
      then raise (SyntaxError "invalid syntax")
      else String.get line (idx+1) in
    prev <> '>' && prev <> '<' && prev <> '!' && next <> '='
  else false

let list_comp_regex = Str.regexp "\\[\\(.*\\) for \\(.*\\) in \\(.*\\)\\]"
let list_comp_if_regex = Str.regexp "\\[\\(.*\\) for \\(.*\\) in \\(.*\\) if \\(.*\\)\\]"

let is_list_comp line = Str.string_match list_comp_regex line 0
let is_if_list_comp line = Str.string_match list_comp_if_regex line 0

(** [exprlst line chr] is an expr list of [line] partitioned into elements by 
    [chr] *)
let rec exprlst (line:string) (chr:char): expr list =
  if line = "" then []
  else List.map (fun x -> parse_expr x operators) (split_on_char chr line)
(** [parse_expr_helper] is an expr made from [str] based on [op] *)
and parse_expr_helper (str:string) (op:string*op) : expr = 
  let oplen = String.length (fst op) in
  let idx = get_idx str (fst op) in
  let left = String.sub str 0 idx in
  let right = String.sub str (idx + oplen) (String.length str - idx - oplen) in
  if trim left = "" then Unary (snd op, parse_expr right operators) 
  else Binary(parse_expr left operators, snd op, parse_expr right operators) 
(** [parse_expr line oplist] is an expr made up of [line] and the operations in 
    [oplist] *)
and
  parse_expr (line:string) (oplist:(string*op) list list) : expr = 
  let line = trim line in let args = get_idx line "(" in 
  let fstarg =  get_idx line "." in let length = String.length line in 
  if line = "" then Value(String(""))
  else
    match oplist with
    | [] -> 
      if line.[0] = '"' || line.[0] = '\'' 
      then Value(String(String.sub line 1 (length-2)))
      else if line.[0] = '[' && line.[length-1] = ']' 
              && valid_bracket (String.sub line 1 (length-2))
      then if length = 2 then List([])
        else if is_if_list_comp line then ListComp(parse_list_comp_if line)
        else if is_list_comp line then ListComp(parse_list_comp line)
        else List(List.map (fun x -> parse_expr x operators) 
                    (split_on_char ',' (String.sub line 1 (length - 2))))
      else if line.[0] = '{' && line.[length-1] = '}' 
      then 
        if length = 2 then Dictionary([])
        else 
          String.sub line 1 (length-2) |> split_on_char ',' 
          |> List.map (fun x -> split_on_char ':' x) |> List.flatten 
          |> List.map (fun x -> parse_expr x operators) |> (fun x -> Dictionary x)
      else if  int_of_string_opt line <> None then Value(Int(int_of_string line))
      else if float_of_string_opt line <> None then Value(Float(float_of_string line))
      else if "True" = line || "False" = line 
      then Value(Bool(bool_of_string (String.lowercase_ascii line)))
      else if "None" = line then Value(NoneVal)
      else if args <> -1 && fstarg <> -1
      (* TODO: Pass None instead of strings *)
      then Function(String.sub line (fstarg+1) (args-fstarg-1), 
                    if length - args = 2
                    then exprlst(String.sub line 0 (fstarg)) ','
                    else exprlst(String.sub line 0 (fstarg) ^","^ 
                                 String.sub line (args+1) (length-args-2))',')
      else if args <> -1 
      then Function(String.sub line 0 (args),
                    if length - args = 2 then []
                    else exprlst (String.sub line (args+1) (length-args-2))',')
      else if line.[length -1] = ']' then 
        let args = (rev_get_idx (String.sub line 0 (length-1)) "[") in
        Function("splice", exprlst (String.sub line 0 (args) ^":"^ 
                                    String.sub line (args+1) (length-args-2))':')
      else Variable(line)
    | h :: t -> match expr_contains line h with
      | Some x, _ -> parse_expr_helper line x
      | None, _ -> parse_expr line t
and parse_list_comp_if (line:string) = 
  let acc = Str.matched_group 1 line in
  let arg = Str.matched_group 2 line in
  let iter = Str.matched_group 3 line in
  let cond = Str.matched_group 4 line in
  parse_expr acc operators, arg, 
  parse_expr iter operators, Some (parse_expr cond operators)
and parse_list_comp (line:string) = 
  let acc = Str.matched_group 1 line in
  let arg = Str.matched_group 2 line in
  let iter = Str.matched_group 3 line in
  parse_expr acc operators, arg, parse_expr iter operators, None

let struct_regex = Str.regexp "\\(.*\\)\\[\\(.*\\)\\]"
let is_struct_assignment line = (Str.string_match struct_regex line 0)
let parse_struct_assignment (line:string) : string * string = 
  let lst = Str.matched_group 1 line in
  let idx = Str.matched_group 2 line in
  lst, idx

let assign left right =
  if is_struct_assignment left 
  then let lst, idx = parse_struct_assignment left in
    None, Function ("replace", exprlst (lst^","^idx) ','@(right::[]))
  else Some (is_var_name left), right

(** [parse_assignment line] is Some string, expr where the string option 
    contains the variable name that is being assigned to and expr is the rest of
    [line] parsed into an expr. *)
let parse_assignment (line:string) : string option * expr = 
  let eq_idx = get_idx line "=" in
  let right = trim (String.sub line (eq_idx+1) (String.length line-eq_idx-1)) in
  if eq_idx = 1 then Some (is_var_name (Char.escaped line.[0])), parse_expr right operators
  else
    let op_string = 
      let tmp = (String.sub line (eq_idx-2) 2) in 
      get_idx tmp "]" |> 
      (fun x -> if x = -1 then tmp 
        else String.sub tmp (x+1) (String.length tmp-x-1)) in
    match expr_contains op_string (List.flatten operators) with 
    | Some x, _ -> let left = x |> fst |> String.length |> (fun x -> eq_idx - x) 
                              |> String.sub line 0 |> trim in
      assign left (Binary(parse_expr left operators, snd x, parse_expr right operators))
    | None, _ -> let left = trim (String.sub line 0 eq_idx) in
      assign left (parse_expr right operators)

(** Matches if statement *)
let if_regex = Str.regexp "^if \\(.*\\):\\(.*\\)"
let elif_regex = Str.regexp "^elif \\(.*\\):\\(.*\\)"
let else_regex = Str.regexp "^else *: *"
let while_regex = Str.regexp "^while\\(.*\\):\\(.*\\)"
let def_regex = Str.regexp "^def \\(.*\\)(\\(.*\\)) *:\\(.*\\)$"
let for_regex = Str.regexp "^for \\(.*\\) in \\(.*\\) *:\\(.*\\)"
let return_regex = Str.regexp "^return \\(.*\\)"

(** Check if line is an if statement *)
let is_if line = Str.string_match if_regex line 0
let is_else line = Str.string_match else_regex line 0
let is_elif line = Str.string_match elif_regex line 0
let is_while line = Str.string_match while_regex line 0
let is_def line = Str.string_match def_regex line 0
let is_for line = Str.string_match for_regex line 0
let is_return line = Str.string_match return_regex line 0

let parse_if (line: string) : (expr * string) =
  let condition = Str.matched_group 1 line in
  let body = String.trim (Str.matched_group 2 line) in
  (parse_expr condition operators, body)

(** For now, assume single argument in for loop *)
let parse_for (line: string) : (expr * string * string) =
  let args = Str.matched_group 1 line in
  let iterator = Str.matched_group 2 line in
  let body = String.trim (Str.matched_group 3 line) in
  (parse_expr iterator operators, (String.trim args), body)

let parse_return (line: string) : (expr) =
  let return_expr = Str.matched_group 1 line in
  (parse_expr return_expr operators)

let parse_def (line: string) : (string * string list * string) =
  let fn_name = Str.matched_group 1 line in
  let arg_string = Str.matched_group 2 line in
  let args = if (String.trim arg_string = "") then [] 
    else List.map String.trim (String.split_on_char ',' (Str.matched_group 2 line)) in
  let body = String.trim (Str.matched_group 3 line) in
  fn_name, args, body

(** [line_type line] is the line_type of [line] *)
let line_type (line : string) : line_type =
  if String.length line = 0 then Empty
  else if is_assignment line then Assignment
  else if is_if line then If (parse_if line)
  else if is_elif line then Elif (parse_if line)
  else if is_else line then Else
  else if is_for line then For (parse_for line)
  else if is_while line then While (parse_if line)
  else if is_def line then Def (parse_def line)
  else if is_return line then Return (parse_return line)
  else Expression

let parse_line (line : string) : string option * expr = 
  match line_type line with
  | Empty -> raise EmptyInput
  | Assignment -> parse_assignment line
  | Expression -> None, parse_expr line operators
  | If (cond, body) -> raise (IfMultiline (cond, body))
  | For (iter, arg, body) -> raise (ForMultiline (iter, arg, body))
  | Def (name, args, body) -> raise (DefMultiline (name, args, body))
  | Elif (cond, body) -> raise (SyntaxError "Elif statement with no if")
  | Else -> raise (SyntaxError "Else statement with no if")
  | Line l -> None, parse_expr line operators
  | While (cond, body) -> raise (WhileMultiline (cond, body)) 
  | Return (expr) -> raise (ReturnExpr (expr))

let get_str_idx s char =
  try String.index s char with
  | Not_found -> -1

let rec space_depth (line : string) (acc : int) : int =
  if String.length line = 0 then acc
  else if get_str_idx line '\t' = 0 
  then space_depth (String.sub line 1 ((String.length line) - 1)) (acc + 4)
  else if get_str_idx line ' ' = 0
  then space_depth (String.sub line 1 ((String.length line) - 1)) (acc + 1)
  else acc

let indent_depth (line : string) = 
  let spaces = space_depth line 0 in
  if (mod) spaces 4 = 0 then spaces / 4
  else raise (SyntaxError "Must use tab or four spaces for indents")

let rec add_depth (line : string) (depth : int) = 
  match depth with 
  | 0 -> line
  | x -> add_depth ("\t" ^ line) (depth - 1)

let parse_multiline (line: string) : line_type =
  match line_type line with
  | Empty -> Empty
  | Assignment -> Line line
  | Expression -> Line line
  | Line line -> Line line
  | Return (expr) -> Line line
  | If (cond, body) -> If (cond, body)
  | Elif (cond, body) -> Elif (cond, body)
  | While (cond, body) -> While (cond, body)
  | For (iter, arg, body) -> For (iter, arg, body)
  | Def (name, args, body) -> Def (name, args, body)
  | Else -> Else 
