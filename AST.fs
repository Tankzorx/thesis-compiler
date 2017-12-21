namespace Zorx.Frontend

open System

module AST =

    type Exp =
        | N  of int                   (* Integer constant *)
        | B of bool                   (* Boolean constant *)
        | Access of Access            (* Variable access  *)
        | BExp of Exp * BinOp * Exp   (* Binary Operator  *)
        | UExp of Exp * UnaryOp
        
    and Access = 
        | AVar of string

    and BinOp = 
        | Gt
        | Lt
        | Leq
        | Geq
        | Neq
        | Eq
        | Plus
        | Minus
        | And
        | Or
    
    and UnaryOp = 
        | Not

    and Dec =
        | RegDec of string * Typ * PrimTyp

    and Typ  =
        | InPort
        | OutPort
        | StatusSignal
        | ControlSignal
        | Reg
    
    and PrimTyp =
        | Any
        | Boolean
        | Integer

    type Stm = 
        | C of int
        | Ass of string * Exp

    type Transition =
        | T of string * Exp * string list * string

    type Fsm =
        | Fsm of Dec list * Transition list

    type Datapath =
        | Datapath of Dec list * Action list

    and Action =
        | Action of string * Stm list


    type Module = 
        //| M of String * Dec list * Fsm // orig
        M of String * Fsm * Datapath // test

    type Specification = S of Module list
 