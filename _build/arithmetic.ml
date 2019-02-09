open State
open Parser
open Error

let rec mul x y acc op = if y = 0 then acc else mul x (y-1) (op acc x) op

let helper_plus = function 
  | Int x, Int y -> Int(x+y)
  | Int x, Float y -> Float (float_of_int x +. y)
  | Int x, Bool y -> if y then Int (x+1) else Int x
  | Int x, String y -> raise (TypeError "unsupported operand type for +")
  | Int x, VList y -> raise (TypeError "unsupported operand type for +")
  | Float x, Int y -> Float (float_of_int y +. x)
  | Float x, Float y -> Float (x +. y)
  | Float x, Bool y -> if y then Float (x +. float_of_int 1) else Float x
  | Float x , String y -> raise (TypeError "unsupported operand type for +")
  | Float x, VList y -> raise (TypeError "unsupported operand type for +")
  | Bool x, Int y -> if x then Int (y+1) else Int  y
  | Bool x, Float y -> if x then Float (y +. float_of_int 1) else Float y
  | Bool x, Bool y -> Int(if x then 1 + (if y then 1 else 0) 
                          else 0 + (if y then 1 else 0))
  | Bool x , String y -> raise (TypeError "unsupported operand type for +")
  | Bool x, VList y -> raise (TypeError "unsupported operand type for +")
  | String x, String y -> String (x ^ y)
  | String x, _ -> raise (TypeError "can only concatenate str to str")
  | VList x, VList y -> VList (ref(!x @ !y))
  | VList x, _-> raise (TypeError "can only concatenate list to list")
  | Function t, _-> raise (TypeError "unsupported operand type function for +")
  | _, Function t-> raise (TypeError "unsupported operand type function for +")
  | _, NoneVal -> raise (TypeError "unsupported operand type function for +")
  | NoneVal, _ -> raise (TypeError "unsupported operand type function for +")
  | Dictionary t, _ | _, Dictionary t -> raise (TypeError "unsupported operand type")

let helper_multiply = function 
  | Int x, Int y -> Int (x * y)
  | Int x, Float y -> Float (float_of_int x *. y)
  | Int x, Bool y -> if y then Int x else Int 0
  | Int x, String y -> raise (TypeError ("unsupported operand type for *"))
  | Int x, VList y -> VList (ref(mul !y x [] (@)))
  | Float x, Int y -> Float (float_of_int y *. x)
  | Float x, Float y -> Float (x *. y)
  | Float x, Bool y -> if y then Float (x *. float_of_int 1) else Float 0.0
  | Float x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | Float x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | Bool x, Int y -> if x then Int y else Int 0
  | Bool x, Float y -> if x then Float (y *. float_of_int 1) else Float 0.0
  | Bool x, Bool y -> if x && y then Int 1 else Int 0
  | Bool x, String y -> if x then String y else String ""
  | Bool x, VList y -> if x then VList y else VList (ref([]))
  | String x, Int y ->  String (mul x y "" (^))
  | String x, Float y -> raise (TypeError "can't multiply sequence by non-int of type 'float'")
  | String x, Bool  y -> if y then String x else String ""
  | String x, String y -> raise (TypeError "can't multiply sequence by non-int of type 'str'")
  | String x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, Int y -> VList (ref(mul !x y [] (@)))
  | VList x, Float y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, Bool y -> if y then VList x else VList (ref([]))
  | VList x, String y -> raise (TypeError "can't multiply sequence by non-int")
  | VList x, VList y -> raise (TypeError "can't multiply sequence by non-int")
  | Function t, _-> raise (TypeError "unsupported operand type function for *")
  | _, Function t-> raise (TypeError "unsupported operand type function for *")
  | _, NoneVal -> raise (TypeError "unsupported operand type function for *")
  | NoneVal, _ -> raise (TypeError "unsupported operand type function for *")
  | Dictionary t, _ | _, Dictionary t -> raise (TypeError "unsupported operand type")

let helper_divide = function 
  | _, Int 0 -> raise (ZeroDivisionError "division by zero")
  | _, Float 0. -> raise (ZeroDivisionError "float division by zero") 
  | _, Bool false -> raise (ZeroDivisionError "float division by zero") 
  | Int x, Int y -> if x mod 2 = 0 then Int(x/y) 
    else Float(float_of_int x /. float_of_int y)
  | Int x, Float y -> Float(float_of_int x /. y)
  | Int x, Bool y -> Float(float_of_int x)
  | Float x, Int y -> Float (x /. float_of_int y)
  | Float x, Float y ->  Float (x /. y)
  | Float x, Bool y -> Float x
  | Bool x, Int y -> if x then Float(1.0/.(float_of_int y)) else Float(0.0)
  | Bool x, Float y-> if x then Float(1.0/.y) else Float 0.
  | Bool x, Bool y -> if x then Float 1.0 else Float 0.
  | String x, _ | _, String x -> raise (TypeError "unsupported operand type for /")
  | VList x, _ | _, VList x -> raise (TypeError "unsupported operand type for /")
  | Function t, _-> raise (TypeError "unsupported operand type function for /")
  | _, Function t-> raise (TypeError "unsupported operand type function for /")
  | _, NoneVal -> raise (TypeError "unsupported operand type function for /")
  | NoneVal, _ -> raise (TypeError "unsupported operand type function for /")
  | Dictionary t, _ | _, Dictionary t -> raise (TypeError "unsupported operand type")

let helper_floor exp = match helper_divide exp with
  | Int x -> Int x
  | Float x -> Int (int_of_float(floor x))
  | _ -> failwith "Not possible?"

let helper_mod = function 
  | _, Int 0 -> raise (ZeroDivisionError "modulo by zero")
  | _, Float 0. -> raise (ZeroDivisionError "modulo by zero")
  | _, Bool false -> raise (ZeroDivisionError "modulo by zero")
  | Int x, Int y -> Int (x mod y) 
  | Int x, Float y -> Float (mod_float (float_of_int x) y)
  | Int x, Bool y ->  Int 0
  | Float x, Int y -> Float (mod_float x (float_of_int y))
  | Float x, Float y -> Float (mod_float x y)
  | Float x, Bool y -> Float (mod_float x 1.)
  | Bool x, Int y -> if x then Int (1 mod y) else Int 0
  | Bool x, Float y -> if x then Float (mod_float 1. y) else Float 0.
  | Bool x, Bool y -> Float 0.
  | String x, _ | _, String x -> 
    raise (TypeError "unsupported operand type for %")
  | VList x, _ | _, VList x -> 
    raise (TypeError "unsupported operand type for %")
  | Function t, _| _, Function t-> 
    raise (TypeError "unsupported operand type function for %")
  | _, NoneVal | NoneVal, _ -> 
    raise (TypeError "unsupported operand type function for %")
  | Dictionary t, _ | _, Dictionary t -> 
    raise (TypeError "unsupported operand type")

let helper_exp = function 
  | Int x, Int y -> Int (int_of_float (float_of_int x ** float_of_int y))
  | Int x, Float y -> Float ((float_of_int x) ** y)
  | Int x, Bool y -> if y then Int x else Int 1
  | Float x, Int y -> Float (x ** (float_of_int y))
  | Float x, Float y -> Float (x ** y)
  | Float x, Bool y -> if y then Float x else Float 1.
  | Bool x, Int y -> if x then Int(int_of_float(1.0 ** float_of_int y)) else Int 0
  | Bool x, Float y -> if x then Float(1.0 ** y) else Float 0.
  | Bool x, Bool y -> if not x && y then Int 0 else Int 1
  | VList x, _ | _, VList x -> 
    raise (TypeError "unsupported operand type for **")
  | String x, _ | _, String x -> 
    raise (TypeError "unsupported operand type for **")
  | Function t, _ | _, Function t-> 
    raise (TypeError "unsupported operand type function for **")
  | _, NoneVal | NoneVal, _ -> 
    raise (TypeError "unsupported operand type function for **")
  | Dictionary t, _ | _, Dictionary t -> 
    raise (TypeError "unsupported operand type")

let helper_and = function
  | Int x, y -> if x = 0 then Int 0 else y
  | Float x, y-> if x = 0. then Float 0. else y
  | Bool x, y -> if not x then Bool(x) else y
  | String x, y -> if x = "" then String "" else y
  | VList x, y -> if !x = [] then VList x else y
  | Function x, y -> y
  | NoneVal, y -> Bool(false)
  | Dictionary x, y -> if !x = [] then Dictionary x else y 

let helper_or = function 
  | Int x, y -> if x<>0 then Int x else y
  | Float x, y -> if x <> 0. then Float x else y
  | Bool x, y -> if x then Bool x else y
  | String x, y -> if x <> "" then String x else y
  | VList x, y -> if !x <> [] then VList x else y
  | Function x, y -> Function x
  | NoneVal, y -> y
  | Dictionary x, y -> failwith("fix this in parse?") 

let helper_equal = function
  | Int x, Int y -> Bool (x = y)
  | Int x, Float y -> Bool (float_of_int x = y)
  | Int x, Bool y -> if y then Bool(x=1) else Bool (x=0)
  | Float x, Int y -> Bool (x = float_of_int y)
  | Float x, Float y -> Bool (x = y)
  | Float x, Bool y -> if y then Bool(x=1.0) else Bool (x=0.0)
  | Bool x, Int y -> if x then Bool(y=1) else Bool (y=0)
  | Bool x, Float y -> if x then Bool(y=1.0) else Bool (y=0.0)
  | Bool x, Bool y -> Bool (x = y)
  | String x, String y -> Bool(x=y)
  | _, String x |String x, _-> Bool false
  | VList x, VList y -> Bool(x=y)    
  | VList x, _ | _, VList x -> Bool false
  | Function (name1, args1, body1), Function (name2, args2, body2) -> 
    Bool (name1 = name2)
  | NoneVal, NoneVal -> Bool true
  | NoneVal, _ | _, NoneVal -> Bool false
  | Function _, _ | _, Function _ -> Bool false
  | Dictionary x, Dictionary y -> Bool(x=y)
  | Dictionary t, _ | _, Dictionary t -> Bool false

let helper_greater_than = function
  | Int x, Int y -> Bool (x > y)
  | Int x, Float y -> Bool (float_of_int x > y)
  | Int x, Bool y -> if y then Bool(x>1) else Bool (x>0)
  | Float x, Int y -> Bool (x > float_of_int y)
  | Float x, Float y -> Bool (x > y)
  | Float x, Bool y -> if y then Bool(x>1.0) else Bool (x>0.0)
  | Bool x, Int y -> if x then Bool(y<1) else Bool (y<0)
  | Bool x, Float y -> if x then Bool(y<1.0) else Bool (y<0.0)
  | Bool x, Bool y -> if x then Bool(not y) else Bool (false)
  | String x, String y ->  Bool (x>y)
  | String x, _ | _, String x -> Bool false
  | VList x, VList y -> Bool(x>y)
  | VList x, _ | _, VList x -> Bool false
  | NoneVal, _ | _, NoneVal -> Bool false
  | Function _, _ | _, Function _ -> Bool false
  | Dictionary t, _ | _, Dictionary t -> Bool false

let helper_greater_equal = function
  | Int x, Int y -> Bool (x >= y)
  | Int x, Float y -> Bool (float_of_int x >= y)
  | Int x, Bool y -> if y then Bool(x>=1) else Bool (x>=0)
  | Float x, Int y -> Bool (x >= float_of_int y)
  | Float x, Float y -> Bool (x >= y)
  | Float x, Bool y -> if y then Bool(x>=1.0) else Bool (x>=0.0)
  | Bool x, Int y -> if x then Bool(y<=1) else Bool (y<=0)
  | Bool x, Float y -> if x then Bool(y<=1.0) else Bool (y<=0.0)
  | Bool x, Bool y -> if x then Bool(true) else Bool (not y)
  | String x, String y -> Bool(x>=y)
  | String x, _ | _, String x -> Bool false
  | VList x, VList y -> Bool(x>=y)
  | VList x, _ | _, VList x -> Bool false
  | NoneVal, _ | _, NoneVal -> Bool false
  | Function _, _ | _, Function _ -> Bool false
  | Dictionary t, _ | _, Dictionary t -> Bool false

let helper_less_than = function
  | Int x, Int y -> Bool (x < y)
  | Int x, Float y -> Bool (float_of_int x < y)
  | Int x, Bool y -> if y then Bool(x<1) else Bool (x<0)
  | Float x, Int y -> Bool (x < float_of_int y)
  | Float x, Float y -> Bool (x < y)
  | Float x, Bool y -> if y then Bool(x<1.0) else Bool (x<0.0)
  | Bool x, Int y -> if x then Bool(y>1) else Bool (y>0)
  | Bool x, Float y -> if x then Bool(y>1.0) else Bool (y>0.0)
  | Bool x, Bool y -> if not x then Bool(true) else Bool (y)
  | String x, String y -> Bool(x<y)
  | String x, _ | _, String x -> Bool false
  | VList x, VList y -> Bool(x<y)
  | VList x, _ | _, VList x -> Bool false
  | NoneVal, _ | _, NoneVal -> Bool false
  | Function _, _ | _, Function _ -> Bool false
  | Dictionary t, _ | _, Dictionary t -> Bool false

let helper_less_equal = function
  | Int x, Int y -> Bool (x <= y)
  | Int x, Float y -> Bool (float_of_int x <= y)
  | Int x, Bool y -> if y then Bool(x<=1) else Bool (x<=0)
  | Float x, Int y -> Bool (x <= float_of_int y)
  | Float x, Float y -> Bool (x <= y)
  | Float x, Bool y -> if y then Bool(x<=1.0) else Bool (x<=0.0)
  | Bool x, Int y -> if x then Bool(y>=1) else Bool (y>=0)
  | Bool x, Float y -> if x then Bool(y>=1.0) else Bool (y>=0.0)
  | Bool x, Bool y -> if not x then Bool(y) else Bool (false)
  | String x, String y -> Bool(x<=y)
  | String x, _ | _, String x -> Bool false
  | VList x, VList y -> Bool(x<=y)
  | VList x, _ | _, VList x -> Bool false
  | NoneVal, _ | _, NoneVal -> Bool false
  | Function _, _ | _, Function _ -> Bool false
  | Dictionary t, _ | _, Dictionary t -> Bool false