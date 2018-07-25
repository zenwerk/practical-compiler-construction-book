(* 記号表 *)

open Types

(* offset: スタック上のオフセット(変数のメモリ位置); level: 変数が宣言された関数の入れ子レベル *)
type varInfo = {offset: int; level: int; ty: ty}

(* formals: 仮引数の型リスト; result: 返り値の型; level: 関数の入れ子レベル *)
type funInfo = {formals: ty list; result: ty; level: int}

type envEntry = VarEntry of varInfo | FunEntry of funInfo

exception No_such_symbol of string

exception SymErr of string

(* 一番最初の記号表
   何も登録されていないので例外を返す *)
let initTable x = raise (No_such_symbol x)

(*
 * 記号表を更新する関数
 * var: シンボル
 * vl: 値
 * env: 記号表
 * x: 入力されてきたシンボル
 *
 * update var vl env で関数を部分適用すると, 記号表を更新できる
 * 記号表envは 'a -> 'b な関数
 * 'a -> 'b -> ('a -> 'b) 'a -> 'b な型の関数updateの
 * var('a) vl('b) env('a -> 'b) だけを部分適用すると 'a -> 'b だけが未適用で残り, 'a -> 'b の関数が帰る. この型は記号表と同じ型.
 *
 * つまり以下のような関数が帰る
 * x(未適用)がvar(適用済)のときvl(適用済)返し、異なるときは記号表env(適用済)からx(未適用)を探してして返す
 * else以下の記号表envが部分適用された関数が生成され続けるため、記号表を更新することができる
 *
 * 実際の実装では記号表はhashtableなどのアルゴリズムを用いることが多い
 *)
let update var vl env x = if x = var then vl else env x