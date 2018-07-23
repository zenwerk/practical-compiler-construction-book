(* 型の定義 *)

(* Simple言語は名前等価 *)
type tag = unit ref (* 型の等価性を表すために使用する *)

(* 言語の型を表すバリアント *)
type ty = 
  | INT (* 整数型 *)
  | ARRAY of int * ty * tag (* 要素数, 要素の型, tag *)  (* 配列の型構成子(type constructor) *)
  | NAME of string * ty option ref (* 型識別子, 本来はどのような型式かを表す | NAME("T", Some(t)) のとき名前T は型tを表す *)
  | UNIT (* 値が無いことを表す型 *)
  | NIL