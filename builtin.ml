open State
open Parser
open Error
open Arithmetic

(**[to_string] returns the string representation of a value *)
let rec to_string (value : State.value) : string = 
  match value with
  | VList x -> List.fold_left (fun x y -> x^(to_string y)^", ") "[" !x |> 
               (fun x -> if String.length x = 1 then x ^ "]" 
                 else String.sub x 0 (String.length x -2) ^ "]")
  | Int x -> string_of_int x
  | Float x -> string_of_float x
  | Bool x -> string_of_bool x |> String.capitalize_ascii
  | Function f -> 
    let (name, args, body) = f in
    let address = 2*(Obj.magic (ref f)) in
    "<function " ^ name ^ " at " ^ Printf.sprintf "0x%08x" address ^ ">"
  | String x -> "'" ^ x ^ "'"
  | Dictionary d -> 
    let rec helper lst = begin match lst with
      | [] -> ""
      | (h1,h2) :: [] -> to_string h1 ^" : "^to_string h2 
      | (h1,h2) :: t -> to_string h1 ^" : "^to_string h2 ^", " ^ helper t
    end in "{" ^ helper(!d) ^ "}"
  | NoneVal -> "None"

let rec str_helper (vals : State.value list) =
  match vals with
  | [] -> ""
  | h::t -> to_string h ^ " " ^ str_helper t

let str (vals : State.value list) : State.value =
  let full_str = str_helper vals in
  String(full_str)

(** [dictionary lst] returns a python dictionary with the keys being the
    elements in odd indexes of lst and its respective value is the next element *)
let dict (lst : value list) : State.value = 
  let rec to_assoc lst = 
    begin match lst with
      | [] -> []
      | h1::h2::t -> (h1,h2) :: to_assoc t
      | _ -> raise (TypeError("Operation unsupported"))
    end 
  in Dictionary(ref(to_assoc lst))

(** If lst is in the form [Dictionary h::k::v::[]],[put lst] places 
    a new binding ([k],[v]) to the dictionary h. If [k] is already present,
    the value of that key is replaced with [v]. Returns none. *)
let put (lst : value list) : State.value =
  match lst with
  | Dictionary(h)::key::value::[] -> 
    h := ((List.remove_assoc key !h) @ [(key,value)]); NoneVal
  | _ -> raise (TypeError("Operation unsupported"))

(** If lst is in the form [Dictionary h::k::[]], [get lst] returns the value
    k is bound to, if k is not a key in h, it raises a KeyError*)
let get (lst : value list) : State.value =
  match lst with
  | Dictionary(h)::key::[] -> if List.assoc_opt key !h <> None 
    then List.assoc key !h
    else raise (KeyError (to_string key))
  | _ -> raise (TypeError("Operation unsupported"))

(** [index lst] returns the index of the second element of lst in the first 
    element of lst in an integer value, returns Int(-1) if not found.
    The first element of lst must be either a VList or a String *)
let index (lst : value list): State.value  = let func = function
    | Int x, Int y -> (x = y)
    | Int x, Float y -> (float_of_int x = y)
    | Int x, Bool y -> if y then (x=1) else (x=0)
    | Float x, Int y -> (x = float_of_int y)
    | Float x, Float y -> (x = y)
    | Float x, Bool y -> if y then (x=1.0) else (x=0.0)
    | Bool x, Int y -> if x then (y=1) else (y=0)
    | Bool x, Float y -> if x then (y=1.0) else (y=0.0)
    | Bool x, Bool y -> (x = y)
    | String x, _ | _, String x -> false
    | VList x, _ | _, VList x -> false
    | Function (name1, args1, body1), Function (name2, args2, body2) -> 
      (name1 = name2)
    | _, _ -> false
  in let idx value l = List.find (fun x -> func (x,value)) l 
  in match lst with
  | VList(t)::h::[] -> idx h !t
  | String(s)::String(s1)::[] -> 
    let rec search sub str = if (String.length sub > String.length str) then 
        String.length sub else 
      if String.sub str 0 (String.length sub) = sub then
        0 else (1 + search sub (String.sub str 1 (String.length sub)))
    in if (search s1 s >= String.length s) then Int(-1) else Int(search s1 s)
  | _ -> raise (TypeError ("Operation not supported"))

(** [append lst] appends the second element of lst into the first element, 
    which must either be a list**)
let append (val_list : value list)= 
  match val_list with
  | lst::value::[] -> 
    (match lst with
     | VList x -> x := !x@value::[]; NoneVal
     | _ -> failwith("not a list")
    )
  | _ -> raise (TypeError("requires two arguments"))


let print (val_list : value list) =
  print_endline(List.fold_left (fun acc value -> acc^(to_string value)) "" 
                  val_list);
  NoneVal 

(**[if_decider val] takes in a [State.value] and returns false if the values 
   match a "false" value of a respective type. The "empty" or "zero" of each 
   type results in false, and if "non-empty" or "non-zero" then true *) 
let if_decider = function
  | Int(0) -> false
  | String("") -> false
  | Bool(false) -> false
  | NoneVal -> false
  | Float(0.0)  -> false
  | VList(a) -> !a <> []
  | Dictionary a -> !a <> []
  | _ -> true

let rec assertt (val_list : value list) =
  match val_list with
  | [] -> NoneVal 
  | h::t -> if if_decider h then assertt t else raise AssertionError

(**[len lst] returns the number of characters in a string or the number of
   elements in the first element of lst when lst has exactly a string or a
   list. *)
let len (lst : value list) : State.value = match lst with
  | VList(l)::[] -> Int(List.length !l)
  | String(s)::[] -> Int (String.length s)
  | Dictionary(d)::[] -> Int (List.length !d)
  | _::[] -> raise (TypeError ("Object of that type has no len()"))
  | _ -> raise (TypeError("Length takes exactly one argument"))

let rec helper_range s f i = 
  if i = 0 then raise (ValueError "range() arg 3 must not be zero") 
  else if i > 0 then (if s >= f then [] 
                      else Int(s) :: helper_range (s+i) f i) 
  else (if s <= f then [] else Int(s) :: helper_range (s+i) f i)

(** [range lst] returns:
    If lst contains exactly 3 Int's a,b,c then VList of integers from a up to b-1 
    with stepping of c
    If lst contains exactly 2 Int's a,b then VList of integers from a up to b-1 
    with stepping of 1
    If lst contains exactly 1 Int's a then VList of integers from 0 up to a-1  
    with stepping of 1 *)
let range (lst : value list) : State.value = 
  match lst with
  | Int(a)::[] -> VList (ref(helper_range 0 a 1))
  | _::[] -> raise (TypeError ("Unsupported type for this operation"))

  | Int(a)::Int(b)::[] -> VList (ref(helper_range a b 1))
  | _ ::_::[]-> raise (TypeError ("Unsupported type for this operation"))

  | Int(a)::Int(b)::Int(c)::[] -> VList (ref(helper_range a b c))
  | _::_::_::[]-> raise (TypeError ("Unsupported type for this operation"))

  | _ -> raise (TypeError("Range takes at most three arguments"))

(** [chr val_list] typecasts an integer into a string of the ASCII character
    equivalent, requires an integer input.
    Raise: TypeError if [val_list] has multiple elements or if the the element
    is not an integer value*)
let chr (val_list : value list) =
  match val_list with
  | Int(x)::[] ->if (x>=0) && (x<=1114111) 
    then String(Char.escaped((Uchar.to_char(Uchar.of_int(x))))) 
    else raise (ValueError("chr() arg is not in range"))
  | _ ::[]-> raise (TypeError("an integer is required"))
  | _ -> raise (TypeError("chr() takes exactly 1 argument"))

(** [bool val_list] typecasts any singular input value into a boolean, and it
    requires one input.
    Raise: TypeError if [val_list] contains more than one element*)
let bool (val_list: value list) = 
  match val_list with
  | Bool x::[] -> Bool x
  | Int x::[] ->  Bool (x<>0)
  | Float x::[] -> Bool (x <> 0.)
  | String x::[] -> Bool (x<>"")
  | VList x::[] -> Bool (!x <> [])
  | Dictionary x::[] -> Bool (!x <> [])
  | Function x::[] -> Bool true
  | NoneVal :: [] -> Bool false
  | [] -> Bool false
  | _ -> raise (TypeError("bool() takes at most 1 argument"))

(** [float val_list] is a float if [val_list] can be turned into a float. 
    Raise: Either a ValueError or TypeError depending on what is wrong with 
    [val_list] *)
let float (val_list: value list) =
  match val_list with
  | Int x::[] -> Float(float_of_int(x))
  | Float x::[] -> Float x
  | String x::[] -> if float_of_string_opt(x) <> None 
    then Float(float_of_string(x)) 
    else raise (ValueError "could not convert input to float")
  | _::[] -> raise (TypeError("float() argument must be a string or a number"))
  | [] -> Float(0.0)
  | _ -> raise (TypeError("float() takes at most 1 argument"))

(** [int val_list] is a int if [val_list] can be turned into a int. 
    Raise: Either a ValueError or TypeError depending on what is wrong with 
    [val_list] *)
let int (val_list: value list) = 
  match val_list with
  | Int(x)::[] -> Int(x)
  | Float(x)::[] -> Int(int_of_float(x))
  | String(x)::[] -> if int_of_string_opt(x) <> None then Int(int_of_string(x)) 
    else raise (ValueError("could not convert input to int"))
  | Bool x ::[]-> if x then Int 1 else Int 0
  | [] -> Int(0)
  | _ -> raise (TypeError "int() can't convert more than one argument")

(** [list v] returns the content of v if v contains a single VList, returns
    VList(ref[]) if v is empty, or a VList of a list of characters (in string form) 
    of s if v contains a single element Sitrng(s) *)
let rec list (v : value list) = match v with
  | [] -> VList(ref[])
  | VList(l)::[]-> VList(l)
  | String(s)::[] -> let rec help_list str = if str = "" then [] else 
                       if String.length str = 1 then [String(str)] else 
                         String(String.sub s 0 1) :: (help_list (String.sub s 1 1))
    in VList(ref(help_list s))
  | _ :: [] -> raise (TypeError ("Input type is not iterable"))
  | x -> raise (TypeError ("list() takes at most 1 argument (" 
                           ^ string_of_int (List.length x) ^ " given)"))

let rec to_list (lst : value list) = 
  let vlist = list lst in
  match vlist with
  | VList(l) -> !l
  | _ -> raise (TypeError ("Input type is not iterable"))

(** [quit arg] quits the interpreter.
    Note: quit in actual python can take an arg, it ignores it.*)
let quit arg = exit 0

(** [replace lst]:
    If lst contains a list [l], an int [idx], and a value [x], it replaces the 
    idx-th element of [l] with [x] and returns none. 
    If lst contains a dictionary [d] and values [k] and [v], it replaces the value
    bound to [k] with [v], if [k] is not in the dictionary, it adds the new binding
    [k],[v] *)
let rec replace (v : value list) = match v with
  | VList(l):: Int(idx):: x :: []-> let
    rec replace_help l idx x = begin match l with 
      | [] -> raise (IndexError "list index out of range")
      | h::t -> if idx = 0 then x :: t else h :: replace_help t (idx-1) x 
    end
    in l := (replace_help !l idx x); NoneVal
  | Dictionary(h)::key::valu::[] -> put (Dictionary(h)::key::valu::[])
  | _ -> raise (TypeError (""))

(** [match_bool statevalue] converts a state.value bool into an ocaml bool.*)
let match_bool = function
  |Bool x -> x|_->failwith("not possble")

(** [max v] returns the maximum element of the given list if a list is the 
    only input value, or it returns the greatest value of the multiple elements
    of [v].
    Raise: TypeError if no args or empty sequence is given.*)
let max (v:value list) = match v with
  | VList l :: []-> let get_first l = begin match l with
      | h::_ -> h 
      | [] -> raise(TypeError("max arg is an empty sequence"))
    end in let
      rec max_help l acc = begin match l with
      | [] -> acc
      | h::t -> if match_bool(helper_greater_equal (h,acc))  
      then max_help t h else max_help t acc
    end 
    in let frst =  get_first(!l)
    in (max_help !l frst)
  | x -> let get_first l = begin match l with
      | f::_ -> f
      | [] -> raise(TypeError("max expected 1 arguments, got 0"))
    end in let rec max_assist x acc = begin match x with
      | [] -> acc
      | h::t -> if match_bool(helper_greater_equal (h,acc)) 
        then max_assist t h else max_assist t acc
    end in let frst =  get_first(x) in (max_assist x frst)

(** [min v] returns the minimum element of the given list if a list is the 
    only input value, or it returns the least value of the multiple elements
    of [v].
    Raise: TypeError if no args or empty sequence is given.*)
let min (v:value list) = match v with
  | VList l :: []-> let get_first l = begin match l with
      | h::_ -> h 
      | [] -> raise(TypeError("min arg is an empty sequence"))
    end in let
      rec min_help l acc = begin match l with
      | [] -> acc
      | h::t -> if match_bool(helper_less_equal (h,acc))
       then min_help t h else min_help t acc
    end 
    in let frst =  get_first(!l)
    in (min_help !l frst)
  | x -> let get_first l = begin match l with
      | f::_ -> f
      | [] -> raise(TypeError("min expected 1 arguments, got 0"))
    end in let rec max_assist x acc = begin match x with
      | [] -> acc
      | h::t -> if match_bool(helper_less_equal (h,acc)) then max_assist t h 
        else max_assist t acc
    end in let frst =  get_first(x) in (max_assist x frst)

let rec splice_string (item:string) start stop step = 
  if (step > 0 && start >= stop) || (step < 0 && start <= stop) then ""
  else Char.escaped (String.get item start) ^ 
       splice_string item (start+step) stop step

let rec splice_list (item: value list) start stop step =
  if (step > 0 && start >= stop) || (step < 0 && start <= stop) then []
  else List.nth item start :: splice_list item (start+step) stop step

let convert_to_splice (lst:value list) =
  let empty v = v = String "" || v = NoneVal in
  match lst with
  | VList a1 :: a2 :: a3 :: a4 :: [] -> 
    let a4 = if a4 = Int 0 then raise (ValueError "Third argument must not be zero") 
      else if empty a4 then Int 1 else int (a4::[]) in 
    let a3 = if empty a3 then NoneVal else int (a3::[]) in
    let a2 = if empty a2 then NoneVal else int (a2::[])
    in VList a1, a2, a3, a4
  | String a1 :: a2 :: a3 :: a4 :: [] -> 
    let a4 = if a4 = Int 0 then raise (ValueError "Third argument must not be zero") 
      else if empty a4 then Int 1 else int (a4::[]) in 
    let a3 = if empty a3 then NoneVal else int (a3::[]) in
    let a2 = if empty a2 then NoneVal else int (a2::[])
    in String a1, a2, a3, a4
  | VList a1 :: a2 :: a3 :: [] ->       
    let a4 =  Int 1 in 
    let a3 = if empty a3 then NoneVal else int (a3::[]) in
    let a2 = if empty a2 then NoneVal else int (a2::[])
    in VList a1, a2, a3, a4
  | String a1 :: a2 :: a3 :: [] ->       
    let a4 =  Int 1 in 
    let a3 = if empty a3 then NoneVal else int (a3::[]) in
    let a2 = if empty a2 then NoneVal else int (a2::[])
    in String a1, a2, a3, a4
  | VList a1 :: a2 :: [] -> 
    let length = List.length !a1 in
    let idx = (match int (a2::[]) with 
        | Int x -> if x > length || x < -length 
          then raise (IndexError ("list index out of range"))
          else (x+length) mod length
        | _ -> raise (SyntaxError "invalid syntax")) in
    let a1 = ref(List.nth !a1 idx::[])
    in VList a1, NoneVal, NoneVal, NoneVal
  | Dictionary h :: idx :: [] -> 
    VList (ref(get (Dictionary h::idx::[])::[])), NoneVal, NoneVal, NoneVal
  | String a1 :: a2 :: [] ->
    let length = String.length a1 in
    let idx = (match int (a2::[]) with 
        | Int x -> if x > length || x < -length 
          then raise (IndexError ("list index out of range"))
          else (x+String.length a1) mod (String.length a1)
        | _ -> raise (SyntaxError "invalid syntax")) in
    let a1 = Char.escaped (String.get a1 idx) in
    String a1, NoneVal, NoneVal, NoneVal
  | a1 :: [] -> raise (SyntaxError "invalid syntax")
  | a1 -> raise (SyntaxError "invalid syntax") 

(** The splice function as in Python; the first element of lst is the 
    string/list to splice, the optional second to fourth elements of lst consist 
    of the start, end, and stepping of the index *)
let splice (lst : value list) : State.value = 
  match convert_to_splice lst with 
  | exception (ValueError x) -> raise (TypeError "list indices must be integers or slices, not str")
  | VList a1, NoneVal, NoneVal, NoneVal -> 
    (match !a1 with 
     | a::[] -> a
     | _ -> failwith "something above messed up")
  | VList a1, NoneVal, NoneVal, Int a4 -> 
    let length = List.length !a1 in
    let a2, a3 = if a4 > 0 then 0, length else length-1, -1 in
    VList(ref(splice_list !a1 a2 a3 a4))
  | VList a1, Int a2, NoneVal, Int a4 -> 
    let length = List.length !a1 in
    let a3 = if a4 > 0 then length else -1 in
    let a2 = if a2 > length || a2 < -length 
      then raise (IndexError "list index out of range")
      else if a4 > 0 then (a2+length) mod length
      else (a2+length+1) mod (length+1) in
    VList(ref(splice_list !a1 a2 a3 a4))
  | VList a1, NoneVal, Int a3, Int a4 -> 
    let length = List.length !a1 in
    let a2 = if a4 > 0 then 0 else length -1 in
    let a3 = if a3 > length || a3 < -length
      then raise (IndexError "list index out of range")
      else if a4 > 0 then (a3+length+1) mod (length+1)
      else (a3+length) mod (length) in     
    VList(ref(splice_list !a1 a2 a3 a4))
  | VList a1, Int a2, Int a3, Int a4 -> 
    let length = List.length !a1 in
    let a2, a3 = 
      if a2 > length || a2 < -length || a3 > length || a3 < -length
      then raise (IndexError "list index out of range")
      else if a4 > 0 then (a2+length) mod length, (a3+length+1) mod (length+1)
      else (a2+length+1) mod (length+1), (a3+length) mod (length) in
    VList(ref(splice_list !a1 a2 a3 a4))
  | String a1, NoneVal, NoneVal, NoneVal -> String a1
  | String a1, NoneVal, NoneVal, Int a4 -> 
    let length = String.length a1 in
    let a2, a3 = if a4 > 0 then 0, length else length-1, -1 in
    String(splice_string a1 a2 a3 a4)
  | String a1, Int a2, NoneVal, Int a4 -> 
    let length = String.length a1 in
    let a3 = if a4 > 0 then length else -1 in
    let a2 = if a2 > length || a2 < -length 
      then raise (IndexError "list index out of range")
      else if a4 > 0 then (a2+length) mod length
      else (a2+length+1) mod (length+1) in
    String(splice_string a1 a2 a3 a4)
  | String a1, NoneVal, Int a3, Int a4 -> 
    let length = String.length a1 in
    let a2 = if a4 > 0 then 0 else length -1 in
    let a3 = if a3 > length || a3 < -length
      then raise (IndexError "list index out of range")
      else if a4 > 0 then (a3+length+1) mod (length+1)
      else (a3+length) mod (length) in     
    String(splice_string a1 a2 a3 a4)
  | String a1, Int a2, Int a3, Int a4 -> 
    let length = String.length a1 in
    let a2, a3 = 
      if a2 > length || a2 < -length || a3 > length || a3 < -length
      then raise (IndexError "list index out of range")
      else if a4 > 0 then (a2+length) mod length, (a3+length+1) mod (length+1)
      else (a2+length+1) mod (length+1), (a3+length) mod (length)  in
    String(splice_string a1 a2 a3 a4)
  | _ -> failwith "Something went wrong in splice conversion"

(** A list associating the name of the built-in function to the actual 
    functions *)
let built_in_functions = [("append", append); ("len", len); ("print", print); 
                          ("chr", chr); ("bool", bool); ("float", float); 
                          ("int",int); ("range", range); ("splice", splice); 
                          ("index", index); ("assert", assertt); ("list", list);
                          ("put", put); ("get", get); ("dictionary", dict); 
                          ("replace", replace); ("max", max); ("min", min);
                          ("quit", quit); ("str", str)]
