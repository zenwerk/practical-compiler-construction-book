open Ast
open Types
open Table

exception Err of string

exception TypeErr of string

(* 内部表現のバイト数を計算する関数 *)
let rec calc_size ty =
  match ty with
  | ARRAY (n, t, _) -> n * calc_size t
  | INT -> 8
  | _ -> raise (Err "internal error")

(* 型名から実際の型の構成を取得する *)
let actual_ty typ =
  let rec travTy typ' lst =
    match typ' with
    | NAME (s, tyref) -> begin (* 型識別子(型の別名)の解析 *)
      match !tyref with
      | Some actty ->
          (* 型式の循環定義を禁止する *)
          if List.mem actty lst then raise (TypeErr "cyclic type definition")
          else travTy actty (actty :: lst)
      | None -> raise (TypeErr "no actual type")
      end
    | _ -> typ'
  in
  travTy typ [typ]

(* 整数型チェック *)
let check_int ty = if ty != INT then raise (TypeErr "type error 1")

(* 配列型チェック *)
let check_array ty =
  match ty with ARRAY _ -> () | _ -> raise (TypeErr "type error 2")

exception SymErr of string

(* (check redeclaration?) IDの再定義がされていたら例外を投げる？ *)
let rec check_redecl decs tl vl =
  match decs with
  | [] -> ()
  | FuncDec (id, _, _, _) :: rest ->
      if List.mem id vl then raise (SymErr id) else check_redecl rest tl (id :: vl)
  | VarDec (_, id) :: rest ->
      if List.mem id vl then raise (SymErr id) else check_redecl rest tl (id :: vl)
  | TypeDec (id, _) :: rest ->
      if List.mem id tl then raise (SymErr id)
      else check_redecl rest (id :: tl) vl

(* 型ty から型式を生成 *)
let rec create_ty ast tenv =
  match ast with
  | NameTyp id -> tenv id
  | ArrayTyp (size, typ) -> ARRAY (size, create_ty typ tenv, ref ())
  | IntTyp -> INT
  | VoidTyp -> UNIT

(* 実引数は %rbp(フレームポインタ) から +24 のところにある．*)
let savedARG = 24

(* return address, static link, old %rbp *)

(*
 * ----- 型検査関数群の定義 -----
 *
 * ASTを再起で巡回して検査する
 * ASTの設定を表す型 ast ごとに定義した関数で型検査する
 *
 * 共通の引数は以下
 *  1. nest -> 現在の入れ子レベル
 *  2. tenv -> 型記号表
 *  3. env  -> 値記号表
 *)

 (*
  * 個々の宣言の処理; type_desc から呼ばれる 
  * 返り値は (tenv:型記号表, env:値記号表, addr:割当済みのメモリオフセット)
  *)
let rec type_dec ast (nest, addr) tenv env =
  match ast with
  (* 関数定義の処理 *) 
  | FuncDec (func_name, args, rlt, Block (dl, _)) -> (* 関数名の記号表への登録 *)
      (* 仮引数と関数内の局所変数の多重宣言はできないのでチェック *)
      check_redecl (List.map (fun (typ, s) -> VarDec (typ, s)) args @ dl) [] [] ;
      let env' = (* 関数名で値記号表を更新する *)
        update func_name
          (FunEntry
             { formals= List.map (fun (typ, _) -> create_ty typ tenv) args
             ; result= create_ty rlt tenv
             ; level= nest + 1 })
          env
      in
      (tenv, env', addr)
  (* 変数宣言の処理 *)
  | VarDec (typ, symbol) ->
      ( tenv
      , update symbol (* 変数名で値記号表を更新する *)
          (VarEntry {ty= create_ty typ tenv; offset= addr - 8; level= nest})
          env
      , addr - 8 )
  (* 型宣言の処理 *)
  | TypeDec (type_name, typ) ->
      (* 型識別子NAMEをキーとして型記号表を更新して返す *)
      let tenv' = update type_name (NAME (type_name, ref None)) tenv in
      (tenv', env, addr)
  | _ -> raise (Err "internal error")

and type_decs decl nest tenv env = (* 宣言リストの処理 *)
  List.fold_left
    (fun (tenv, env, addr) dec -> type_dec dec (nest, addr) tenv env)
    (tenv, env, 0) decl

(* 
 * 関数の仮引数宣言のリストの処理: emitter.ml で使われる
 * 関数の仮引数の値記号表への登録を行う
 *)
and type_param_dec args nest tenv env = 
  let env', _ =
    List.fold_left begin
        (* 第1引数はアキュムレータ(acc), 第2引数は args の要素 *)
        fun (env, addr) (ty, symbol) -> ( update symbol (VarEntry {offset= addr; level= nest; ty= create_ty ty tenv}) env , addr + 8 )
      end
      (env, savedARG) (* 初期値 *)
      args (* 処理対象 *)
  in
  env'

and type_stmt ast env = (* 文の型検査 *)
  match ast with
  | CallProc ("scan", [arg]) ->
      if type_exp arg env != INT then raise (TypeErr "type error 3")
  | CallProc ("iprint", [arg]) ->
      if type_exp arg env != INT then
        raise (TypeErr "iprint requires int value")
  | CallProc ("return", [arg]) -> () (* result type should be checked *)
  | CallProc ("sprint", _) -> ()
  | CallProc ("new", [VarExp (Var s)]) -> (
      let entry = env s in
      match entry with
      | VarEntry {ty; _} -> check_array (actual_ty ty)
      | _ -> raise (No_such_symbol s) )
  | CallProc (s, el) ->
      let _ = type_exp (CallFunc (s, el)) env in
      ()
  | Block (dl, _) -> check_redecl dl [] []
  | Assign (v, e) ->
      if type_var v env != type_exp e env then raise (TypeErr "type error 4")
  | If (e, _, _) -> type_cond e env
  | While (e, _) -> type_cond e env
  | NilStmt -> ()

and type_var ast env = (* 左辺値の型検査 *)
  match ast with
  | Var s -> (
      let entry = env s in
      match entry with
      | VarEntry {ty; _} -> actual_ty ty
      | _ -> raise (No_such_symbol s) )
  | IndexedVar (v, size) ->
      check_int (type_exp size env) ;
      match type_var v env with
      | ARRAY (_, ty, _) -> actual_ty ty
      | _ -> raise (TypeErr "type error 5")

and type_exp ast env = (* 式の型検査 *)
  match ast with
  | VarExp var -> type_var var env (* 変数定義式 *)
  | IntExp i -> INT
  (* 演算子の型検査 *)
  | CallFunc ("+", [left; right]) ->
      check_int (type_exp left env) ;
      check_int (type_exp right env) ;
      INT
  | CallFunc ("-", [left; right]) ->
      check_int (type_exp left env) ;
      check_int (type_exp right env) ;
      INT
  | CallFunc ("*", [left; right]) ->
      check_int (type_exp left env) ;
      check_int (type_exp right env) ;
      INT
  | CallFunc ("/", [left; right]) ->
      check_int (type_exp left env) ;
      check_int (type_exp right env) ;
      INT
  | CallFunc ("!", [arg]) ->
      check_int (type_exp arg env) ;
      INT
  (* 
   * 関数呼び出しの型検査 
   *
   * 仮引数(定義したときの型)と実引数(実際にプログラム内で渡された型)の型が一致するか検査する
   * 返り値の型を返す
   *)
  | CallFunc (func_name, args) -> (
      (* 関数名から記号表を検索 *)
      let entry = env func_name in
      match entry with
      | FunEntry {formals= fpTyl; result= rltTy; level= _} ->
          if List.length fpTyl == List.length args then (* 引数の数をチェック *)
            let fpTyl' = List.map actual_ty fpTyl
            and apTyl = List.map (fun e -> type_exp e env) args in
            let l = List.combine fpTyl' apTyl in
            if List.for_all (fun (f, a) -> f == a) l then actual_ty rltTy (* 実引数の型チェック *)
            else raise (TypeErr "type error 6")
          else raise (TypeErr "type error 7")
      | _ -> raise (No_such_symbol func_name) )
  | _ -> raise (Err "internal error")

and type_cond ast env = (* 関係演算子の型検査 *)
  match ast with
  | CallFunc (_, [left; right]) ->
      check_int (type_exp left env) ;
      check_int (type_exp right env)
  | _ -> raise (Err "internal error")