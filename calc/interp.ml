type id = string

type stmt = Stmts of stmt * stmt | Assign of id * exp | Print of exp

and exp =
  | Id of id
  | Num of int
  | Plus of exp * exp
  | Minus of exp * exp
  | Times of exp * exp
  | Div of exp * exp

exception No_such_symbol

let e0 _ = raise No_such_symbol

let update var vl env v = if v = var then vl else env v

let rec trans_stmt ast env =
  match ast with
  | Stmts (s1, s2) ->
      let env' = trans_stmt s1 env in
      trans_stmt s2 env'
  | Assign (var, e) ->
      let vl = trans_exp e env in
      update var vl env
  | Print e ->
      let vl = trans_exp e env in
      print_int vl ; print_string "\n" ; env

and trans_exp ast env =
  match ast with
  | Id v -> env v
  | Num n -> n
  | Plus (e1, e2) ->
      let vl1 = trans_exp e1 env in
      let vl2 = trans_exp e2 env in
      vl1 + vl2
  | Minus (e1, e2) ->
      let vl1 = trans_exp e1 env in
      let vl2 = trans_exp e2 env in
      vl1 - vl2
  | Times (e1, e2) ->
      let vl1 = trans_exp e1 env in
      let vl2 = trans_exp e2 env in
      vl1 * vl2
  | Div (e1, e2) ->
      let vl1 = trans_exp e1 env in
      let vl2 = trans_exp e2 env in
      vl1 / vl2

let prog =
  Stmts
    ( Assign ("x", Plus (Num 1, Times (Num 2, Num 3)))
    , Stmts (Assign ("y", Div (Id "x", Num 4)), Print (Id "y")) )

let interp ast = trans_stmt ast e0
