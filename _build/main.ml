open State
open Evaluate
open Parser
open Utils
open Error
open Builtin

(** Read next lines that are part of the if conditional block *)
let rec read_if (conds : expr list) (bodies : string list) 
    (acc : string) (new_line : bool) (lines : string list) (line_nums : int list) =
  if new_line then
    let () = print_string "... " in
    let line = read_line () in
    let depth = indent_depth line in
    if depth = 0 then
      (match parse_multiline line with
       | Empty -> 
         (List.rev (Value(Bool(true))::conds), 
          List.rev (""::(String.trim acc::bodies)), [], [])
       | Line line -> read_if conds bodies (acc ^ "\n" ^ line) 
                      new_line lines line_nums
       | If (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body 
                            new_line lines line_nums
       | Elif (cond, body) -> read_if (cond::conds) (String.trim acc::bodies) body 
                              new_line lines line_nums
       | Else -> read_if (Value(Bool(true))::conds) 
                 (String.trim acc::bodies) "" new_line lines line_nums
       | _ -> raise EmptyInput)
    else 
      let indented_line = add_depth (String.trim line) (depth - 1) in
      read_if conds bodies (acc ^ "\n" ^ indented_line) new_line lines line_nums
  else (match lines, line_nums with
      | [], [] -> (List.rev (Value(Bool(true))::conds), 
                   List.rev (""::(String.trim acc::bodies)), [], [])
      | h::t, n_h::n_t -> 
        let depth = indent_depth h in
        if depth = 0 then
          (match parse_multiline h with
           | Empty -> (List.rev (Value(Bool(true))::conds), 
                       List.rev (""::(String.trim acc::bodies)), lines, line_nums)
           | Line line -> (List.rev (Value(Bool(true))::conds), 
                           List.rev (""::(String.trim acc::bodies)), lines, line_nums)
           | If (cond,body) -> (List.rev (Value(Bool(true))::conds), 
                       List.rev (""::(String.trim acc::bodies)), lines, line_nums)
           (* | If (cond, body) -> read_if (cond::conds) 
                                  (String.trim acc::bodies) body new_line t n_t *)
           | Elif (cond, body) -> read_if (cond::conds) 
                                    (String.trim acc::bodies) body new_line t n_t
           | Else -> read_if (Value(Bool(true))::conds) 
                       (String.trim acc::bodies) "" new_line t n_t
           | _ -> raise EmptyInput)
        else 
          let line = add_depth (String.trim h) (depth - 1) in
          read_if conds bodies (acc ^ "\n" ^ line) new_line t n_t
      | _, _ -> failwith "Lines and line numbers should match"
    )

(** Read next lines that are part of the while loop *)
let rec read_while (cond : expr) (body : string) (lines : string list)
    (line_nums : int list) (new_line : bool) =
  match lines, line_nums with
  | [], [] -> if new_line then (print_string "... "; 
                                read_while cond body [read_line ()] [1] new_line)
    else (cond, String.trim body, [], [])
  | line::t, line_num::n_t -> 
    let depth = indent_depth line in
    if depth = 0 then
      (cond, String.trim body, lines, line_nums)
    else let indent_line = add_depth (String.trim line) (depth - 1) in
      (match parse_multiline indent_line with
       | Empty -> (cond, String.trim body, lines, line_nums)
       | _ -> read_while cond (body ^ "\n" ^ indent_line) t n_t new_line)
  | _, _ -> failwith "Lines and line numbers should match"

(** Read the next lines as part of the body of the function *)
let rec read_function (body : string) (lines : string list) 
    (line_nums : int list) (new_line : bool) =
  match lines, line_nums with
  | [], [] -> if new_line then (print_string "... "; 
                                read_function body [read_line ()] [1] new_line)
    else (String.trim body, [], [])
  | line::t, line_num::n_t -> 
    let depth = indent_depth line in
    if depth = 0 then (String.trim body, lines, line_nums)
    else let indent_line = add_depth (String.trim line) (depth - 1) in
      (match parse_multiline line with
       | Empty -> (String.trim body, lines, line_nums)
       | _ -> read_function (body ^ "\n" ^ indent_line) t n_t new_line)
  | _, _ -> failwith "Lines and line numbers should match"

(** Read the next lines as the body of a for loop. Ends on an empty line
    or a line with indent at the same level as the for loop *)
let rec read_for (body : string) (lines : string list) (new_line : bool) =
  match lines with
  | [] -> if new_line then (print_string "... "; 
                            read_for body [read_line ()] new_line)
    else (String.trim body, [])
  | line::t -> 
    let depth = indent_depth line in
    if depth = 0 then
      (String.trim body, lines)
    else let indent_line = add_depth (String.trim line) (depth - 1) in
      (match parse_multiline indent_line with
       | Empty -> (String.trim body, lines)
       | _ -> read_for (body ^ "\n" ^ indent_line) t new_line)

(** [interpret st lines new_line] runs python [lines] to create a new state. 
    If [new_line] is true, then interface prompts uses for new lines. *)
let rec interpret (st:State.t) (lines: string list) 
    (line_nums: int list) (new_line : bool) : State.t =
  match lines, line_nums with
  | [], [] -> if new_line then (print_string ">>> "; 
                                interpret st [read_line ()] [1] new_line) else st
  | h::t, n_h::n_t -> 
    (match Parser.parse_line h |> (fun x -> Evaluate.evaluate x st) with
     | exception (KeyError x) -> print_error "Key Error" x h n_h; 
       interpret st [] [] new_line
     | exception (SyntaxError x) -> print_error "SyntaxError" x h n_h;
       interpret st [] [] new_line
     | exception (IndexError x) -> print_error "IndexError" x h n_h;
       interpret st [] [] new_line
     | exception (NameError x) -> print_error "NameError" x h n_h;
       interpret st [] [] new_line
     | exception (TypeError x) -> print_error "TypeError" x h n_h;
       interpret st [] [] new_line
     | exception (OverflowError x) -> print_error "OverflowError" x h n_h;
       interpret st [] [] new_line
     | exception (IndentationError x) -> print_error "IndentationError" x h n_h;
       interpret st [] [] new_line
     | exception (ZeroDivisionError x) -> print_error "ZeroDivisionError" x h n_h;
       interpret st [] [] new_line
     | exception (AssertionError) -> print_error "AssertionError" 
                                     "Incorrect" h n_h;
                                     interpret st [] [] new_line
     | exception EmptyInput -> interpret st t n_t new_line
     | exception (IfMultiline (cond, body)) -> 
       (* Create list of conditions with corresponding line bodies *)
       let (conds, bodies, next_lines, next_line_nums) 
         = read_if [cond] [] body (t = []) t n_t in 
       (* Iterate through conditions and run body of code *)
       let new_state = interpret_if conds bodies st in
       interpret new_state next_lines next_line_nums false
     | exception (ForMultiline (iter, arg, body)) -> 
       let (for_body, remaining_lines) = read_for body t (t = []) in 
       let iter_val = to_list [(eval iter st)] in
       let new_state = interpret_for iter_val arg for_body st in
       let new_line_nums = create_int_list (List.length remaining_lines) in
       interpret new_state remaining_lines new_line_nums new_line
     | exception (WhileMultiline (cond, init_body)) -> 
       (* Parse out the loop condition and body, process them in [interpret_while] *)
       let (while_cond, while_body, next_lines, next_line_nums) 
         = read_while cond (String.trim init_body) t n_t new_line in
       let new_state = interpret_while while_cond while_body st in
       interpret new_state next_lines next_line_nums new_line
     | exception (DefMultiline (name, args, init_body)) -> 
       (* Parse the body of the function *)
       let (function_body, next_lines, next_line_nums) = 
         read_function (String.trim init_body) t n_t new_line in
       (* Assign function definition to function name in global state *)
       let new_st = Evaluate.evaluate 
           (Some name, Value(Function(name, args, function_body))) st in
       interpret new_st next_lines next_line_nums new_line
     | newst -> interpret newst t n_t new_line)
  | _, _ -> failwith "Line numbers do not match up with lines"
and interpret_if (conds : expr list) (bodies : string list) (st: State.t) : State.t =
  (* Go through [conds] and respective [bodies] in order. If any condition 
     evaluates to true then we run the corresponding body through the 
     interpreter and throw out the rest *)
  match conds, bodies with
  | cond::c_t, body::b_t -> (match Evaluate.eval cond st |> if_decider with
      | true -> let body_lines = String.split_on_char '\n' body in
        let line_nums = create_int_list (List.length body_lines) in
        interpret st body_lines line_nums false
      | false -> interpret_if c_t b_t st)
  | _, _ -> raise (SyntaxError "Conditional statements and bodies mismatched")
and interpret_while (cond : expr) (body : string) (st: State.t) : State.t = 
  match Evaluate.to_bool cond st with
  | true -> 
    (* If while conditional is true, then we want to interpret the body, and 
       after that, interpret the loop condition until it's false *)
    let new_lines = String.split_on_char '\n' body in
    let new_line_nums = create_int_list (List.length new_lines) in
    let new_state = interpret st new_lines new_line_nums false in 
    interpret_while cond body new_state
  | false -> st
and interpret_for (iter : value list) (arg : string) 
    (body : string) (st: State.t) : State.t = 
  match iter with
  | [] -> st
  | h::t -> 
  (* For each element in the iterator, assign it to the argument and run 
    the body of the for loop *)
    let arg_state = insert arg h st in
    let new_lines = String.split_on_char '\n' body in
    let new_line_nums = create_int_list (List.length new_lines) in
    let new_state = interpret arg_state new_lines new_line_nums false in 
    interpret_for t arg body new_state
