type token =
  | NUM of (int)
  | STR of (string)
  | ID of (string)
  | INT
  | IF
  | WHILE
  | SPRINT
  | IPRINT
  | SCAN
  | EQ
  | NEQ
  | GT
  | LT
  | GE
  | LE
  | ELSE
  | RETURN
  | NEW
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LB
  | RB
  | LS
  | RS
  | LP
  | RP
  | ASSIGN
  | SEMI
  | COMMA
  | TYPE
  | VOID

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

open Printf
open Ast

# 44 "parser.ml"
let yytransl_const = [|
  260 (* INT *);
  261 (* IF *);
  262 (* WHILE *);
  263 (* SPRINT *);
  264 (* IPRINT *);
  265 (* SCAN *);
  266 (* EQ *);
  267 (* NEQ *);
  268 (* GT *);
  269 (* LT *);
  270 (* GE *);
  271 (* LE *);
  272 (* ELSE *);
  273 (* RETURN *);
  274 (* NEW *);
  275 (* PLUS *);
  276 (* MINUS *);
  277 (* TIMES *);
  278 (* DIV *);
  279 (* LB *);
  280 (* RB *);
  281 (* LS *);
  282 (* RS *);
  283 (* LP *);
  284 (* RP *);
  285 (* ASSIGN *);
  286 (* SEMI *);
  287 (* COMMA *);
  288 (* TYPE *);
  289 (* VOID *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* STR *);
  259 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\003\000\004\000\004\000\005\000\005\000\
\005\000\005\000\006\000\006\000\007\000\007\000\009\000\009\000\
\010\000\010\000\002\000\002\000\002\000\002\000\002\000\002\000\
\002\000\002\000\002\000\002\000\002\000\002\000\002\000\013\000\
\013\000\014\000\014\000\008\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\012\000\000\000"

let yylen = "\002\000\
\001\000\001\000\004\000\001\000\002\000\000\000\003\000\005\000\
\006\000\006\000\003\000\001\000\000\000\001\000\004\000\002\000\
\002\000\001\000\004\000\007\000\005\000\007\000\005\000\005\000\
\005\000\005\000\005\000\005\000\003\000\001\000\001\000\000\000\
\001\000\003\000\001\000\004\000\001\000\001\000\004\000\004\000\
\003\000\003\000\003\000\003\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\031\000\053\000\001\000\030\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\029\000\000\000\000\000\000\000\000\000\000\000\018\000\
\000\000\005\000\000\000\000\000\000\000\000\000\019\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\046\000\000\000\000\000\043\000\
\044\000\000\000\000\000\000\000\000\000\000\000\000\000\036\000\
\017\000\000\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\023\000\024\000\025\000\026\000\040\000\
\039\000\027\000\000\000\000\000\000\000\000\000\007\000\000\000\
\000\000\000\000\003\000\004\000\000\000\000\000\000\000\000\000\
\000\000\011\000\020\000\022\000\008\000\016\000\000\000\000\000\
\000\000\010\000\000\000\009\000\015\000"

let yydgoto = "\002\000\
\013\000\014\000\118\000\030\000\058\000\087\000\119\000\015\000\
\120\000\059\000\032\000\037\000\033\000\034\000"

let yysindex = "\004\000\
\112\255\000\000\240\254\241\254\244\254\250\254\031\255\050\255\
\007\255\052\255\000\000\000\000\000\000\000\000\000\000\007\255\
\007\255\007\255\007\255\007\255\094\255\007\255\098\255\000\000\
\235\254\007\255\007\255\029\255\099\255\067\255\061\255\016\255\
\077\255\075\255\092\255\192\255\079\255\080\255\100\255\033\255\
\103\255\007\255\007\255\000\000\196\255\007\255\007\255\007\255\
\007\255\000\000\105\255\240\254\102\255\129\255\140\255\000\000\
\141\255\000\000\086\255\120\255\121\255\007\255\000\000\007\255\
\007\255\007\255\007\255\007\255\007\255\112\255\112\255\123\255\
\125\255\134\255\151\255\122\255\000\000\011\255\011\255\000\000\
\000\000\135\255\165\255\146\255\142\255\159\255\254\254\000\000\
\000\000\007\255\000\000\016\255\016\255\016\255\016\255\016\255\
\016\255\016\255\171\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\162\255\053\255\053\255\053\255\000\000\188\255\
\104\255\112\255\000\000\000\000\163\255\189\255\166\255\164\255\
\169\255\000\000\000\000\000\000\000\000\000\000\176\255\053\255\
\176\255\000\000\205\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\181\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\126\255\000\000\000\000\000\000\000\000\000\000\000\000\247\254\
\000\000\182\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\181\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\216\255\000\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\148\255\170\255\000\000\
\000\000\000\000\000\000\000\000\000\000\038\255\000\000\000\000\
\000\000\000\000\000\000\251\254\193\255\194\255\195\255\197\255\
\198\255\199\255\001\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\200\255\200\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\201\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\228\255\226\255\000\000\000\000\000\000\110\000\168\255\
\000\000\000\000\254\255\210\000\188\000\000\000"

let yytablesize = 287
let yytable = "\057\000\
\021\000\056\000\002\000\042\000\001\000\043\000\028\000\024\000\
\016\000\025\000\017\000\019\000\018\000\031\000\020\000\035\000\
\036\000\036\000\035\000\040\000\021\000\035\000\034\000\044\000\
\045\000\034\000\026\000\111\000\112\000\002\000\089\000\048\000\
\049\000\027\000\046\000\047\000\048\000\049\000\130\000\075\000\
\132\000\099\000\100\000\078\000\079\000\080\000\081\000\046\000\
\047\000\048\000\049\000\046\000\047\000\048\000\049\000\116\000\
\053\000\022\000\050\000\092\000\073\000\093\000\094\000\095\000\
\096\000\097\000\098\000\012\000\012\000\052\000\053\000\004\000\
\005\000\006\000\007\000\008\000\023\000\117\000\029\000\046\000\
\047\000\048\000\049\000\009\000\010\000\124\000\060\000\113\000\
\003\000\011\000\004\000\005\000\006\000\007\000\008\000\039\000\
\012\000\131\000\054\000\055\000\041\000\051\000\009\000\010\000\
\061\000\062\000\070\000\071\000\011\000\088\000\046\000\047\000\
\048\000\049\000\003\000\012\000\004\000\005\000\006\000\007\000\
\008\000\063\000\046\000\047\000\048\000\049\000\083\000\072\000\
\009\000\010\000\074\000\084\000\082\000\123\000\011\000\038\000\
\038\000\038\000\038\000\038\000\038\000\012\000\085\000\086\000\
\038\000\038\000\038\000\038\000\090\000\105\000\091\000\038\000\
\101\000\038\000\102\000\038\000\038\000\041\000\041\000\041\000\
\041\000\041\000\041\000\103\000\106\000\107\000\041\000\041\000\
\109\000\046\000\047\000\048\000\049\000\041\000\108\000\041\000\
\104\000\041\000\041\000\042\000\042\000\042\000\042\000\042\000\
\042\000\110\000\114\000\115\000\042\000\042\000\122\000\126\000\
\125\000\127\000\128\000\042\000\129\000\042\000\011\000\042\000\
\042\000\064\000\065\000\066\000\067\000\068\000\069\000\133\000\
\032\000\033\000\046\000\047\000\048\000\049\000\046\000\047\000\
\048\000\049\000\004\000\121\000\047\000\048\000\049\000\077\000\
\050\000\051\000\052\000\013\000\014\000\038\000\076\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\021\000\000\000\021\000\021\000\021\000\
\021\000\021\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\021\000\021\000\000\000\000\000\000\000\000\000\021\000\
\021\000\000\000\000\000\000\000\000\000\000\000\021\000"

let yycheck = "\030\000\
\000\000\030\000\003\001\025\001\001\000\027\001\009\000\001\001\
\025\001\003\001\027\001\027\001\029\001\016\000\027\001\018\000\
\019\000\020\000\028\001\022\000\027\001\031\001\028\001\026\000\
\027\000\031\001\020\001\030\001\031\001\030\001\059\000\021\001\
\022\001\027\001\019\001\020\001\021\001\022\001\127\000\042\000\
\129\000\070\000\071\000\046\000\047\000\048\000\049\000\019\001\
\020\001\021\001\022\001\019\001\020\001\021\001\022\001\003\001\
\004\001\027\001\030\001\062\000\028\001\064\000\065\000\066\000\
\067\000\068\000\069\000\030\001\031\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\027\001\108\000\027\001\019\001\
\020\001\021\001\022\001\017\001\018\001\114\000\026\001\090\000\
\003\001\023\001\005\001\006\001\007\001\008\001\009\001\002\001\
\030\001\128\000\032\001\033\001\003\001\003\001\017\001\018\001\
\028\001\031\001\028\001\028\001\023\001\024\001\019\001\020\001\
\021\001\022\001\003\001\030\001\005\001\006\001\007\001\008\001\
\009\001\030\001\019\001\020\001\021\001\022\001\025\001\028\001\
\017\001\018\001\028\001\003\001\028\001\030\001\023\001\010\001\
\011\001\012\001\013\001\014\001\015\001\030\001\003\001\003\001\
\019\001\020\001\021\001\022\001\029\001\028\001\030\001\026\001\
\030\001\028\001\030\001\030\001\031\001\010\001\011\001\012\001\
\013\001\014\001\015\001\030\001\030\001\001\001\019\001\020\001\
\027\001\019\001\020\001\021\001\022\001\026\001\029\001\028\001\
\026\001\030\001\031\001\010\001\011\001\012\001\013\001\014\001\
\015\001\027\001\016\001\026\001\019\001\020\001\003\001\003\001\
\030\001\028\001\031\001\026\001\028\001\028\001\023\001\030\001\
\031\001\010\001\011\001\012\001\013\001\014\001\015\001\003\001\
\028\001\028\001\019\001\020\001\021\001\022\001\019\001\020\001\
\021\001\022\001\003\001\110\000\028\001\028\001\028\001\028\001\
\028\001\028\001\028\001\028\001\028\001\020\000\043\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\255\255\005\001\006\001\007\001\
\008\001\009\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\018\001\255\255\255\255\255\255\255\255\023\001\
\024\001\255\255\255\255\255\255\255\255\255\255\030\001"

let yynames_const = "\
  INT\000\
  IF\000\
  WHILE\000\
  SPRINT\000\
  IPRINT\000\
  SCAN\000\
  EQ\000\
  NEQ\000\
  GT\000\
  LT\000\
  GE\000\
  LE\000\
  ELSE\000\
  RETURN\000\
  NEW\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIV\000\
  LB\000\
  RB\000\
  LS\000\
  RS\000\
  LP\000\
  RP\000\
  ASSIGN\000\
  SEMI\000\
  COMMA\000\
  TYPE\000\
  VOID\000\
  "

let yynames_block = "\
  NUM\000\
  STR\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 26 "parser.mly"
             (  _1  )
# 290 "parser.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 29 "parser.mly"
           ( IntTyp )
# 296 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 30 "parser.mly"
                     ( ArrayTyp (_3, IntTyp) )
# 303 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 31 "parser.mly"
               ( NameTyp _1 )
# 310 "parser.ml"
               : 'ty))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decs) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 34 "parser.mly"
                ( _1@_2 )
# 318 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    Obj.repr(
# 35 "parser.mly"
                ( [] )
# 324 "parser.ml"
               : 'decs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'ids) in
    Obj.repr(
# 38 "parser.mly"
                     ( List.map (fun x -> VarDec (_1,x)) _2 )
# 332 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    Obj.repr(
# 39 "parser.mly"
                              ( [TypeDec (_2,_4)] )
# 340 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 40 "parser.mly"
                                    ( [FuncDec(_2, _4, _1, _6)] )
# 350 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'fargs_opt) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 41 "parser.mly"
                                      ( [FuncDec(_2, _4, VoidTyp, _6)] )
# 359 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ids) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                       ( _1@[_3] )
# 367 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                       ( [_1]  )
# 374 "parser.ml"
               : 'ids))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                        ( [] )
# 380 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'fargs) in
    Obj.repr(
# 49 "parser.mly"
                        ( _1 )
# 387 "parser.ml"
               : 'fargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'fargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser.mly"
                             ( _1@[(_3,_4)] )
# 396 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'ty) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 53 "parser.mly"
                             ( [(_1,_2)] )
# 404 "parser.ml"
               : 'fargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 56 "parser.mly"
                   ( _1@[_2] )
# 412 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 57 "parser.mly"
                   ( [_1] )
# 419 "parser.ml"
               : 'stmts))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 60 "parser.mly"
                              ( Assign (Var _1, _3) )
# 427 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 61 "parser.mly"
                                       ( Assign (IndexedVar (Var _1, _3), _6) )
# 436 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 62 "parser.mly"
                              ( If (_3, _5, None) )
# 444 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 64 "parser.mly"
                              ( If (_3, _5, Some _7) )
# 453 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'cond) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 65 "parser.mly"
                              ( While (_3, _5) )
# 461 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 66 "parser.mly"
                              ( CallProc ("sprint", [StrExp _3]) )
# 468 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    Obj.repr(
# 67 "parser.mly"
                              ( CallProc ("iprint", [_3]) )
# 475 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 68 "parser.mly"
                           ( CallProc ("scan", [VarExp (Var _3)]) )
# 482 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 69 "parser.mly"
                           ( CallProc ("new", [ VarExp (Var _3)]) )
# 489 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'aargs_opt) in
    Obj.repr(
# 70 "parser.mly"
                                ( CallProc (_1, _3) )
# 497 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
                           ( CallProc ("return", [_2]) )
# 504 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'block) in
    Obj.repr(
# 72 "parser.mly"
             ( _1 )
# 511 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
            ( NilStmt )
# 517 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
                           ( [] )
# 523 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'aargs) in
    Obj.repr(
# 77 "parser.mly"
                           ( _1 )
# 530 "parser.ml"
               : 'aargs_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'aargs) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 80 "parser.mly"
                          ( _1@[_3] )
# 538 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 81 "parser.mly"
                           ( [_1] )
# 545 "parser.ml"
               : 'aargs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decs) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 84 "parser.mly"
                         ( Block (_2, _3) )
# 553 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 87 "parser.mly"
           ( IntExp _1  )
# 560 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
          ( VarExp (Var _1) )
# 567 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'aargs_opt) in
    Obj.repr(
# 89 "parser.mly"
                          ( CallFunc (_1, _3) )
# 575 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                      ( VarExp (IndexedVar (Var _1, _3)) )
# 583 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                      ( CallFunc ("+", [_1; _3]) )
# 591 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                       ( CallFunc ("-", [_1; _3]) )
# 599 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                       ( CallFunc ("*", [_1; _3]) )
# 607 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                     ( CallFunc ("/", [_1; _3]) )
# 615 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                               ( CallFunc("!", [_2]) )
# 622 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                   ( _2 )
# 629 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "parser.mly"
                     ( CallFunc ("==", [_1; _3]) )
# 637 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                     ( CallFunc ("!=", [_1; _3]) )
# 645 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                     ( CallFunc (">", [_1; _3]) )
# 653 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "parser.mly"
                     ( CallFunc ("<", [_1; _3]) )
# 661 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 103 "parser.mly"
                     ( CallFunc (">=", [_1; _3]) )
# 669 "parser.ml"
               : 'cond))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( CallFunc ("<=", [_1; _3]) )
# 677 "parser.ml"
               : 'cond))
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
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.stmt)
;;
