open OUnit2
open Parser
open Evaluate
open State
open Builtin
let make_line_test
    (name : string)
    (line : string)
    (state : State.t)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (evaluate (parse_line line) state))

(**** TESTING *****)

let arith_tests = [
  make_line_test "Plus" "x=4+5" [] ["x",Int(9)] ;
  make_line_test "PlusString" "x=\"Hello\"" [] ["x",String("Hello")];
  make_line_test "Subtract" "y=4-5" [] ["y",Int(-1)] ;
  make_line_test "Div" "x=55/5" [] ["x",Float(11.)] ;
  make_line_test "FloorDiv" "x=55//5" [] ["x",Int(11)] ;
  make_line_test "Mult" "x=9*5" [] ["x",Int(45)] ;
  make_line_test "Modular" "x=53%5" [] ["x",Int(3)] ;
  make_line_test "Exp" "x=2**5" [] ["x",Int(32)] ;
  make_line_test "Div" "x=55//5" [] ["x",Int(11)] ;
  make_line_test "Equal" "x=(1==True)" [] ["x",Bool(true)] ;
  make_line_test "notEqual" "x=(4!=3)" [] ["x",Bool(true)] ;
  make_line_test "GreaterThan" "x=55>5" [] ["x",Bool(true)] ;
  make_line_test "LessThan" "x=55<5" [] ["x",Bool(false)] ;
  make_line_test "GreaterEqual" "x=5>=5" [] ["x",Bool(true)] ;
  make_line_test "LessEqual" "x=5<=5" [] ["x",Bool(true)] ;
  make_line_test "And" "x=True and True" [] ["x",Bool(true)] ;
  make_line_test "Or" "x=False or True" [] ["x",Bool(true)] ;
  make_line_test "Not" "x=(not False)" [] ["x",Bool(true)] ;
  (* make_line_test "LessThan" "x=~5" [] ["x",Int(-6)] ; *)
]

let function_tests = [
  make_line_test "Len" "x = len([1,2,3,3])" [] ["x",Int(4)] ;
  make_line_test "Len2" "x = len([])" [] ["x",Int(0)] ;
  make_line_test "chr" "x = chr(77)" [] ["x", String("M")] ;
  make_line_test "chr2" "x = chr(99)" [] ["x", String("c")] ;
  make_line_test "bool" "x = bool(23)" [] ["x", Bool(true)] ;
  make_line_test "bool2" "x = bool(0.0)" [] ["x", Bool(false)] ;
  make_line_test "int" "x = int(\"420\")" [] ["x", Int(420)] ;
  make_line_test "int2" "x = int(1234.02344)" [] ["x", Int(1234)] ;
  make_line_test "float" "x = float(345)" [] ["x", Float(345.)] ;
  (* make_line_test "float2" "x = float([])" [] ["x", Float(0.)] ; *)

  (* make_line_test "append1" "x = [123, 23] == append([123],23)" 
     [] ["x",Bool(true)] ;
     make_line_test "append2" "x = [\"\"] == append([],\"\")" [] ["x", Bool(true)]; *)

  make_line_test "len of []" "x = len([])" [] ["x", Int(0)];
  make_line_test "len of [1,2,3]" "x = len([1,2,3])" [] ["x", Int(3)];
  make_line_test "len of string" "x = len('')" [] ["x", Int(0)];
  make_line_test "range test 1" "x = range(4)" [] 
    ["x", VList(ref([Int(0); Int(1); Int(2); Int(3)]))];
  make_line_test "range test 2" "x = range(1,4)" []
    ["x", VList(ref([Int(1); Int(2); Int(3)]))];
  make_line_test "range test 3" "x = range(1,4,2)" []
    ["x", VList(ref([Int(1); Int(3)]))];
  make_line_test "creating a dict with non-empty state" "x = {'a' : 12, 14 : y}" 
    ["y",Int(10)] 
    ["x", Dictionary(ref ([String("a"), Int(12); Int(14), Int(10)]));
     "y",Int(10)];
  make_line_test "creating a dict" "x = {'a' : 12, 14 : 'q'}" 
    [] ["x", Dictionary(ref ([String("a"), Int(12); Int(14), String("q")]))];
  make_line_test "appending a dict" "x[3] = 4" 
    ["x", Dictionary(ref ([String("a"), Int(12); Int(14), String("q")]))]
    ["x", Dictionary(ref ([String("a"), Int(12); Int(14), String("q"); 
                           Int(3), Int(4)]))];
  make_line_test "get value in a dict" "y = x['a']" 
    ["x", Dictionary(ref ([String("a"), Int(12); Int(14), String("q")]))]
    ["y", Int(12); 
     "x", Dictionary(ref ([String("a"), Int(12); Int(14), String("q")]))];
]


let suite =
  "test suite for A6-A8"  >::: List.flatten [
    arith_tests;
    function_tests
  ]

let _ = run_test_tt_main suite
