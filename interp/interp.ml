(* File interp.ml *)
type id = string
type op = Plus | Minus | Times | Div
type stm = Stmts of stm * stm
  | Assign of id * exp
  | Print of exp
and exp = ID of id
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp

exception No_Such_Symbol

(* 実行環境の定義 *)
let e0 = fun _ -> raise No_Such_Symbol
let update var value env = fun v -> if v = var then value else env v


(* 文の解析 *)
let rec trans_stmt ast env =
  match ast with
  | Stmts (s1, s2) -> let env' = trans_stmt s1 env in
                      trans_stmt s2 env'
  | Assign (var, e) -> let value = trans_exp e env in
                       update var value env
  | Print e -> let value = trans_exp e env in
               (print_int value; print_string "\n"; env)
(* 式の解析 *)
and trans_exp ast env =
  match ast with
  | ID v -> env v
  | Num n -> n
  | Plus (e1, e2) -> let value1 = trans_exp e1 env in
                     let value2 = trans_exp e2 env in
                     value1 + value2
  | Minus (e1, e2) -> let value1 = trans_exp e1 env in
                      let value2 = trans_exp e2 env in
                      value1 - value2
  | Times (e1, e2) -> let value1 = trans_exp e1 env in
                      let value2 = trans_exp e2 env in
                      value1 * value2
  | Div (e1, e2) -> let value1 = trans_exp e1 env in
                    let value2 = trans_exp e2 env in
                    value1 / value2

(** AST を手打ち *)
let prog = Stmts (Assign ("x", Plus (Num 1, Times (Num 2, Num 3))),
             Stmts (Assign ("y", Div (ID "x", Num 4)), Print (ID "y")))

(* 初期環境でカリー化 *)
let interp ast = trans_stmt ast e0