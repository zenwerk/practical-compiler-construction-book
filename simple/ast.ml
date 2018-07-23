(* The definition of the abstract syntax tree *)
type id = string

type var =
  | Var of id (* 変数id の格納場所 *)
  | IndexedVar of var * exp (* 配列の値を取得する *)

(* stmt は文 *)
and stmt =
  | Assign of var * exp
  | CallProc of id * exp list
  | Block of dec list * stmt list
  | If of exp * stmt * stmt option
  | While of exp * stmt
  | NilStmt (* 空文 *)

(* 式 *)
and exp =
  | VarExp of var (* 変数 *)
  | StrExp of string (* 文字列定数 *)
  | IntExp of int (* 整数定数 *)
  | CallFunc of id * exp list (* 関数呼び出し *)

(* 宣言 *)
and dec =
  | FuncDec of id * (typ * id) list * typ * stmt (* 関数定義 *)
  | TypeDec of id * typ (* 型宣言 *)
  | VarDec of typ * id (* 変数定義 *)

(* 型 *)
and typ = NameTyp of string | ArrayTyp of int * typ | IntTyp | VoidTyp