type token =
  | NUM of (int)
  | ID of (string)
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | ASSIGN
  | PRINT
  | LP
  | RP
  | SEMI
  | EOL

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* TIMES *);
  262 (* DIV *);
  263 (* ASSIGN *);
  264 (* PRINT *);
  265 (* LP *);
  266 (* RP *);
  267 (* SEMI *);
  268 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\002\000\003\000\003\000\004\000\001\000\001\000\003\000\003\000\
\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\012\000\000\000\000\000\000\000\
\000\000\001\000\006\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\004\000\007\000\000\000\
\000\000\010\000\011\000"

let yydgoto = "\002\000\
\005\000\006\000\014\000"

let yysindex = "\005\000\
\030\255\000\000\012\255\027\255\000\000\248\254\000\255\000\255\
\030\255\000\000\000\000\000\000\000\255\036\255\017\255\026\255\
\025\255\000\255\000\255\000\255\000\255\000\000\000\000\028\255\
\028\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\255\000\000\033\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\255\
\014\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\037\000\248\255"

let yytablesize = 46
let yytable = "\015\000\
\011\000\012\000\009\000\010\000\017\000\001\000\008\000\008\000\
\013\000\024\000\025\000\026\000\027\000\008\000\008\000\008\000\
\009\000\009\000\007\000\018\000\019\000\020\000\021\000\009\000\
\009\000\009\000\022\000\018\000\019\000\020\000\021\000\003\000\
\020\000\021\000\023\000\008\000\009\000\004\000\018\000\019\000\
\020\000\021\000\003\000\003\000\002\000\016\000"

let yycheck = "\008\000\
\001\001\002\001\011\001\012\001\013\000\001\000\003\001\004\001\
\009\001\018\000\019\000\020\000\021\000\010\001\011\001\012\001\
\003\001\004\001\007\001\003\001\004\001\005\001\006\001\010\001\
\011\001\012\001\010\001\003\001\004\001\005\001\006\001\002\001\
\005\001\006\001\010\001\009\001\011\001\008\001\003\001\004\001\
\005\001\006\001\011\001\012\001\012\001\009\000"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  ASSIGN\000\
  PRINT\000\
  LP\000\
  RP\000\
  SEMI\000\
  EOL\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 's) in
    Obj.repr(
# 13 "parser1.mly"
                           ( _1 )
# 110 "parser1.ml"
               : Interp.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 's) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 's) in
    Obj.repr(
# 16 "parser1.mly"
                           ( Interp.Stmts (_1,_3) )
# 118 "parser1.ml"
               : 's))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 17 "parser1.mly"
                           ( Interp.Assign (_1,_3) )
# 126 "parser1.ml"
               : 's))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 18 "parser1.mly"
                           ( Interp.Print (_3) )
# 133 "parser1.ml"
               : 's))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 21 "parser1.mly"
                           ( Interp.Id _1 )
# 140 "parser1.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 22 "parser1.mly"
                           ( Interp.Num _1 )
# 147 "parser1.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'e) in
    Obj.repr(
# 23 "parser1.mly"
                           ( _2 )
# 154 "parser1.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 24 "parser1.mly"
                           ( Interp.Plus (_1,_3) )
# 162 "parser1.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 25 "parser1.mly"
                           ( Interp.Minus (_1,_3) )
# 170 "parser1.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 26 "parser1.mly"
                           ( Interp.Times (_1,_3) )
# 178 "parser1.ml"
               : 'e))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'e) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'e) in
    Obj.repr(
# 27 "parser1.mly"
                           ( Interp.Div (_1,_3) )
# 186 "parser1.ml"
               : 'e))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Interp.stmt)
;;
# 31 "parser1.mly"

# 213 "parser1.ml"
