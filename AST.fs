namespace Zorx.Frontend

open System

module AST =

    type Exp =
        | C of Const
        | Access of Access            (* Variable access  *)
        | BExp of Exp * BinOp * Exp   (* Binary Operator  *)
        | UExp of Exp * UnaryOp

    and Const =        
        | N  of int                   (* Integer constant *)
        | B of bool                   (* Boolean constant *)

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
        | Dec of string * Typ * PrimTyp

    and Typ  =
        | InPort
        | OutPort
        | StatusSignal
        | Reg
    
    and PrimTyp =
        | Boolean
        | Integer

    type Stm = 
        | Ass of string * Exp

    type Transition =
        | T of string * Exp * string list * string * Stm list

    type Controller =
        | Controller of Dec list * Transition list

    type Datapath =
        | Datapath of Dec list * Action list

    and Action =
        | Action of string * Stm list


    type Module = 
        M of String * Controller * Datapath

    type Specification = S of Module list


