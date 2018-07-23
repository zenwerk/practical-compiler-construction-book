(* 記号表 *)

open Types

(* offset: スタック上のオフセット(変数のメモリ位置); level: 変数が宣言された関数の入れ子レベル *)
type varInfo = {offset: int; level: int; ty: ty}

(* formals: 仮引数の型リスト; result: 返り値の型; level: 関数の入れ子レベル *)
type funInfo = {formals: ty list; result: ty; level: int}

type envEntry = VarEntry of varInfo | FunEntry of funInfo

exception No_such_symbol of string

exception SymErr of string

let initTable x = raise (No_such_symbol x)

let update var value table = (fun x -> if x = var then value else table x)
