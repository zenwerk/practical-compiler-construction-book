(* macOS 向け *)
open Ast
open Printf
open Types
open Table
open Semant

let label = ref 0

let incLabel () =
  label := !label + 1 ;
  !label

(* str を n 回コピーする *)
let rec nCopyStr n str = if n > 0 then str ^ nCopyStr (pred n) str else ""

(* 
 * 呼出し時にcallee(呼び出され側)に静的リンクを渡すコードを生成する関数 
 * srcNestLevel -> 呼び出し側の入れ子レベル
 * dstDestLevel -> 呼び出され側の入れ子レベル
 *)
let passLink srcNestLevel dstNestLevel =
  (* 呼び出し側のネストレベルが,呼び出され側より大きい場合, つまり呼び出し側のほうがネストが深い位置にある *)
  if srcNestLevel >= dstNestLevel then
    let deltaLevel = srcNestLevel - dstNestLevel + 1 in
    "\tmovq %rbp, %rax\n"                           (* フレームポインタをraxに保存する *)
    ^ nCopyStr deltaLevel "\tmovq 16(%rax), %rax\n" (* ネストレベル分だけ静的リンクをたどる *)
    ^ "\tpushq %rax\n"                              (* 現在の静的リンクをフレームにpush する *)
  else "\tpushq %rbp\n"  (* 現在ののフレームポインタを push する *)

let output = ref ""

(* printfやscanfで使う文字列 *)
let io = "IO:\n\t.string \"%lld\"\n" ^ "\t.text\n"

(* main関数の頭 *)
let header =
  "\t.globl _main\n" ^ "_main:\n" ^ "\tpushq %rbp\n"
  (* フレームポインタの保存 *)
  ^ "\tmovq %rsp, %rbp\n"

(* フレームポインタをスタックポインタの位置に *)
(* プロローグとエピローグ *)
let prologue =
  "\tpushq %rbp\n"
  (* フレームポインタの保存 *)
  ^ "\tmovq %rsp, %rbp\n"

(* フレームポインタのスタックポインタ位置への移動 *)
let epilogue = "\tleaveq\n" (* -> movq %ebp, %esp; popl %ebp *)
               ^ "\tretq\n"

(* 呼出し位置の次のアドレスへ戻る *)
(* 宣言部の処理
 * 変数宣言->記号表への格納
 * 関数定義->局所宣言の処理とコード生成
 *)
let rec trans_dec ast nest tenv env =
  match ast with
  (* 関数定義の処理 *)
  | FuncDec (fname, args, _rlt, block) ->
      (* 仮引数を値記号表へ登録する *)
      let env' = type_param_dec args (nest + 1) tenv env in
      (* 関数本体（ブロック内の文）のコード生成 *)
      let code = trans_stmt block (nest + 1) tenv env' in
      (* 関数コードの合成 *)
      output :=
        !output ^ fname ^ ":\n" (* 関数ラベル *)
        ^ prologue (* プロローグ *)
        ^ code (* 本体コード *)
        ^ epilogue
  (* エピローグ *)
  (* 変数宣言の処理 *)
  | VarDec (ty, symbol) -> ()
  (* 型宣言の処理 *)
  | TypeDec (symbol, ty) ->
      let entry = tenv symbol in (* 型記号表から symbol を探す　 *)
      match entry with
      | NAME (_name, ty_opt) -> ty_opt := Some (create_ty ty tenv) (* 型識別子から実際の型を返す *)
      | _ -> raise (Err symbol)

(* 
 * 文の処理 
 * 文は結果を持たない.
 * 文の主な振る舞いは、変数の代入、特殊文、手続き呼出し、制御構造の導入
 *)
and trans_stmt ast nest tenv env =
  type_stmt ast env ; (* まず文の型検査を行う *)
  match ast with
  (* 代入のコード: 代入先フレームをsetVarで求める. *)
  | Assign (v, e) ->
      trans_exp e nest env   (* 右辺の式の結果がフレームトップに保存される *)
      ^ trans_var v nest env (* 変数vのアドレスを rax レジスタに読み込む *)
      ^ "\tpopq (%rax)\n"    (* (フレームトップに保存されている)式の結果を rax の指すアドレスに保存する *)
  (* iprintのコード: 整数の印字 *)
  | CallProc ("iprint", [arg]) ->
      trans_exp arg nest env      (* 式の結果がフレームトップに保存される *)
      ^ "\tpopq  %rsi\n"          (* 第2引数用レジスタ rsi に式の結果を読み込む (cygwin では rdx) *)
      ^ "\tleaq IO(%rip), %rdi\n" (* IOラベルを第1引数用レジスタ rdi に格納する (cygwin では rcx) *)
      ^ "\tmovq $0, %rax\n"       (* rax を即値0 で上書き (Cygwinでは不要) *)
      ^ "\tcallq _printf\n"       (* Cのprintfを呼び出す (linux では printf) *)
  (* sprintのコード: 文字列の印字 *)
  | CallProc ("sprint", [StrExp s]) ->
      let l = incLabel () in
      "\t.data\n"                              (* データ定義開始 *)
      ^ sprintf "L%d:\t.string %s\n" l s       (* 文字列データをラベル Ln で定義する *)
      ^ "\t.text\n"                            (* コード部 *)
      ^ sprintf "\tleaq L%d(%%rip), %%rdi\n" l (* ラベル Ln のデータを第1引数レジスタへ (cygwinでは rcx ) *)
      ^ "\tmovq $0, %rax\n"                    (* cygwin では不要 *)
      ^ "\tcallq _printf\n"                    (* printf の呼出し (linux では printf) *)
  (* scanのコード: 整数の入力 *)
  | CallProc ("scan", [VarExp v]) ->
      trans_var v nest env        (* 変数vのアドレスを取得 *)
      ^ "\tmovq %rax, %rsi\n"     (* 左辺値の結果を第2引数レジスタへ (cygwin では rdx) *)
      ^ "\tleaq IO(%rip), %rdi\n" (* IOラベルのアドレスを第1引数レジスタへ (cygwinではrcx) *)
      ^ "\tmovq $0, %rax\n"       (* cygwin では不要 *)
      ^ "\tcallq _scanf\n"        (* C の scanf 呼出し (linux では scanf) *)
  (* returnのコード *)
  | CallProc ("return", [arg]) ->
      trans_exp arg nest env      (* 式の結果をスタックに push *)
      ^ "\tpopq %rax\n"           (* 結果を再度 pop して rax レジスタへ読み込む *)
  (* 配列の領域確保: ヒープ領域の取得 *)
  | CallProc ("new", [VarExp v]) ->
      let size = calc_size (type_var v env) in
      sprintf "\tmovq $%d, %%rdi\n" size (* 配列のサイズを第1引数レジスタへ *)
      ^ "\tcallq _malloc\n"              (* C の malloc を呼び出す *)
      ^ "\tpushq %rax\n"                 (* malloc 結果をスタックに push *)
      ^ trans_var v nest env             (* new の引数に渡された変数vのアドレスを取得 *)
      ^ "\tpopq (%rax)\n"                (* malloc の結果を変数v へ pop する *)
  (* 
   * 手続き呼出しのコード 
   * 実引数のコード、静的リンクのコード、callq で構成される
   * s: 関数名のシンボル
   * el:実引数リスト
   *)
  | CallProc (s, el) -> (
      let entry = env s in
      match entry with
      | FunEntry {formals= _; result= _; level} ->
          (* 実引数部分のコード生成 *)
          (if List.length el mod 2 = 1 then "" else "\tpushq $0\n") (* 引数が奇数個なら16バイト境界になるよう調整する *)
          ^ List.fold_right
              (fun ast code -> code ^ trans_exp ast nest env) (* 実引数のリスト部分のコード生成 *)
              el ""
          (* 静的リンクを渡す部分のコード生成 *)
          ^ passLink nest level
          (* 手続き s の呼出しコード *)
          ^ "\tcallq " ^ s ^ "\n"
          (* 
           * スタックに積んだ引数+静的リンクを降ろして元に戻す 
           * スタックポインタに加算 -> アドレスを増やしてスタックポインタを巻き戻している
           * 加算するアドレスの計算は, 16byteアライメントの仕様を考慮して以下
           * (len(実引数リスト) + 1(静的リンク) + 1(アライメント分？) / 2) * 16byte
           *)
          ^ sprintf "\taddq $%d, %%rsp\n" (((List.length el + 1 + 1) / 2) * 16)
      | _ -> raise (No_such_symbol s) )
  (* ブロックのコード：文を表すブロックは，関数定義を無視する．*)
  | Block (decLst, stmtLst) ->
      (* 
       * ブロック内で宣言された型や変数の処理
       * 返り値は更新した型記号表tenv' と 値記号表env' と 割り当て済みオフセット addr' *)
      let tenv', env', addr' = type_decs decLst nest tenv env in
      (* 更新した記号表を元に関数定義部のコードを生成する *)
      List.iter (fun d -> trans_dec d nest tenv' env') decLst ;
      (*
       * ブロック内で定義された局所変数の割付けを行うため, スタックフレームをその分だけ拡張する
       * スタックフレームの拡張とは, スタックポインタのアドレスを減算すること
       * 割り当て済みオフセットが addr' で求まっているので, 16byteアライメントを考慮して以下のようになる *)
      let ex_frame = sprintf "\tsubq $%d, %%rsp\n" ((-addr' + 16) / 16 * 16) in
      (* 本体（文列）のコード生成 *)
      let code =
        List.fold_left
          (fun code ast -> code ^ trans_stmt ast nest tenv' env')
          "" stmtLst
      in
      ex_frame ^ code  (* 局所変数分のフレーム拡張の付加 *)
  (* 
   * elseなしif文(if (cond) stmt)のコード生成
   *)
  | If (e, s, None) ->
      let condCode, l = trans_cond e nest env in
      condCode                      (* 条件分岐のコード *)
      ^ trans_stmt s nest tenv env  (* 条件が真のとき実行される文のコード *)
      ^ sprintf "L%d:\n" l          (* 条件が偽のときのジャンプ先ラベル *)
  (* 
   * elseありif文(if (cond) stmt1 else stmt2)のコード生成 
   *)
  | If (e, s1, Some s2) ->
      let condCode, l1 = trans_cond e nest env in
      let l2 = incLabel () in
      condCode (* 条件分岐のコード*)
      ^ trans_stmt s1 nest tenv env  (* 条件が真のとき実行される文のコード *)
      ^ sprintf "\tjmp L%d\n" l2     (* 文を実行したあとにラベルL2にジャンプする *)
      ^ sprintf "L%d:\n" l1          (* 条件が偽のときのジャンプ先ラベル *)
      ^ trans_stmt s2 nest tenv env  (* 条件が偽のとき実行される文のコード *)
      ^ sprintf "L%d:\n" l2
  (* 
   * while文(while (cond) stmt)のコード生成
   *)
  | While (e, s) ->
      let condCode, l1 = trans_cond e nest env in
      let l2 = incLabel () in
      sprintf "L%d:\n" l2    (* whileの先頭ラベル *)
      ^ condCode                    (* 条件分岐のコード *)
      ^ trans_stmt s nest tenv env  (* 条件が真のときに実行される文のコード *)
      ^ sprintf "\tjmp L%d\n" l2    (* while の先頭に戻る *)
      ^ sprintf "L%d:\n" l1  (* ループの脱出先ラベル *)
  (* 空文 *)
  | NilStmt -> ""

(*
 * 参照アドレスの処理
 * 変数の格納先のスタックフレームから取り出す
 *)
and trans_var ast nest env =
  match ast with
  | Var s -> (
      let entry = env s in
      match entry with
      | VarEntry {offset; level; ty= _} -> (* 変数の格納されているアドレスをraxに保存する *)
          "\tmovq %rbp, %rax\n" (* rax にフレームポインタの値を格納 *)
          (* 言語仕様より,16(rbp)は静的リンクの値を指す, それをraxに上書きするコードを nest-level 回数繰り返す.
             -> つまり改装分静的リンクをたどり、ネスト分フレームを移動する *)
          ^ nCopyStr (nest - level) "\tmovq 16(%rax), %rax\n" 
          (* 得られたフレーム(rax)からoffset分先に格納されているアドレス(rax+offset)を rax に格納 *)
          ^ sprintf "\tleaq %d(%%rax), %%rax\n" offset
      | _ -> raise (No_such_symbol s) )
  | IndexedVar (v, size) -> (* 配列要素の左辺値の参照要素 *)
      trans_exp (CallFunc ("*", [IntExp 8; size])) nest env (* 添字(size)分,配列の先頭アドレスからのオフセットを計算する, 結果はスタックのトップに配置される *)
      ^ trans_var v nest env         (* 配列の先頭アドレスを取得する *)
      ^ "\tmovq (%rax), %rax\n"      (* 先頭アドレスに格納されている値をレジスタに読み込む *)
      ^ "\tpopq %rbx\n"              (* スタックポインタの値をレジスタrbxにポップする *)
      ^ "\tleaq (%rax,%rbx), %rax\n" (* 最初のオフセットと配列先頭アドレスを足したアドレスをraxに保存する *)

(* 
 * 式の処理 
 * trans_exp でのASTの処理結果は rax の値をフレームに push して終わる
 *)
and trans_exp ast nest env =
  match ast with
  (* 
   * 整数定数のコード
   * スタックに即値をプッシュすればよい
   *)
  | IntExp i -> sprintf "\tpushq $%d\n" i
  (* 
   * 変数参照のコード：reVarで参照フレームを求める 
   * trans_var で左辺値vのアドレスを %rax に入れて返す
   * (%rax)でアドレスの指す値を取り出し、スタックにプッシュする
   *)
  | VarExp v ->
      trans_var v nest env 
      ^ "\tmovq (%rax), %rax\n"
      ^ "\tpushq %rax\n"
  (* +のコード *)
  | CallFunc ("+", [left; right]) ->
      trans_exp left nest env ^ trans_exp right nest env (* 左右の値をスタックに乗せる *)
      ^ "\tpopq %rax\n"         (* スタックからrightをpopしてraxへ *)
      ^ "\taddq %rax, (%rsp)\n" (* rax(right)とスタック(left)を加算する *)
  (* -のコード *)
  | CallFunc ("-", [left; right]) ->
      trans_exp left nest env 
      ^ trans_exp right nest env 
      ^ "\tpopq %rax\n"
      ^ "\tsubq %rax, (%rsp)\n"
  (* *のコード *)
  | CallFunc ("*", [left; right]) ->
      trans_exp left nest env 
      ^ trans_exp right nest env 
      ^ "\tpopq %rax\n"
      ^ "\timulq (%rsp), %rax\n" (* 乗算命令 *)
      ^ "\tmovq %rax, (%rsp)\n"
  (* /のコード *)
  | CallFunc ("/", [left; right]) ->
      trans_exp left nest env 
      ^ trans_exp right nest env 
      ^ "\tpopq %rbx\n"  (* 割る数 *)
      ^ "\tpopq %rax\n"  (* 割られる数 *)
      ^ "\tcqto\n"       (* rdx は使用しないので rax を符号拡張する *)
      ^ "\tidivq %rbx\n" (* raxに商, rdxに余りが格納される *)
      ^ "\tpushq %rax\n" (* rax に格納された結果をスタックに push *)
  (* 符号反転のコード *)
  | CallFunc ("!", arg :: _) -> 
      trans_exp arg nest env 
      ^ "\tnegq (%rsp)\n"
  (* 関数呼出しのコード *)
  | CallFunc (s, el) ->
      trans_stmt (CallProc (s, el)) nest initTable env (* 返戻値は%raxに入れて返す *)
      ^ "\tpushq %rax\n" (* 関数呼出しの結果をスタックに push *)
  | _ -> raise (Err "internal error")

(* 
 * 関係演算の処理
 * 関係演算のアセンブリコードと比較演算部分のラベル場号を返す
 *)
and trans_cond ast nest env =
  match ast with
  | CallFunc (op, left :: right :: _) -> begin
      let code =
        (* オペランドのコード *)
        trans_exp left nest env ^ trans_exp right nest env
        (* オペランドの値を %rax，%rbxへ *)
        ^ "\tpopq %rax\n" ^ "\tpopq %rbx\n"
        (* cmp命令 *)
        ^ "\tcmpq %rax, %rbx\n"
      in
      let l = incLabel () in (* ラベル番号を一意に採番 *)
      match op with
      (* 条件と分岐の関係は，逆 *)
      | "==" -> (code ^ sprintf "\tjne L%d\n" l, l)
      | "!=" -> (code ^ sprintf "\tje L%d\n" l, l)
      | ">" -> (code ^ sprintf "\tjle L%d\n" l, l)
      | "<" -> (code ^ sprintf "\tjge L%d\n" l, l)
      | ">=" -> (code ^ sprintf "\tjl L%d\n" l, l)
      | "<=" -> (code ^ sprintf "\tjg L%d\n" l, l)
      | _ -> ("", 0)
    end
  | _ -> raise (Err "internal error")

(*
 * プログラム全体の生成の起点
 * プログラムは main関数として呼び出せるようにする
 *)
let trans_prog ast =
  let code = trans_stmt ast 0 initTable initTable in
  io ^ header ^ code ^ epilogue ^ !output