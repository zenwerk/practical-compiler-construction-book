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

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Interp.stmt
