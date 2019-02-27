open GT
open Syntax
open Stmt 
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter
     val eval : config -> prg -> config
   Takes a configuration and a program, and returns a configuration as a result
 *)        
                  
let evalOperation (stack, config) operation =
    let (state, input, output) = config in 
      match operation with
          | LD varName -> ((state varName)::stack, config)
          | ST varName -> (match stack with
              | varValue::newStack -> (newStack, (Syntax.Expr.update varName varValue state, input, output)))
          | CONST constant -> (constant::stack, config)
          | BINOP binaryOperation -> (match stack with
              | right::left::newStack -> 
                ((Syntax.Expr.eval state (Binop(binaryOperation, Const left, Const right)))::newStack, config))
          | READ  -> (match input with
              | constant::newInput -> (constant::stack, (state, newInput, output)))
          | WRITE -> (match stack with
              | constant::newStack -> (newStack, (state, input, output @ [constant])))


let rec eval configuration program = List.fold_left evalOperation configuration program


(* Top-level evaluation
     val run : int list -> prg -> int list
   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler
     val compile : Syntax.Stmt.t -> prg
   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

let rec compileExpression expression = match expression with
    | Syntax.Expr.Const  n -> [CONST n]
    | Syntax.Expr.Var v -> [LD v]
    | Syntax.Expr.Binop (op, left, right) -> (compileExpression left) @ (compileExpression right) @ [BINOP op]

let rec compile statement = match statement with
  | Read varName -> [READ; ST varName]
  | Write expression -> (compileExpression expression) @ [WRITE]
  | Assign (varName, expression) -> (compileExpression expression) @ [ST varName]
  | Seq (t1, t2) -> (compile t1) @ (compile t2)