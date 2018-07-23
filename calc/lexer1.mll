(* File lexer1.mll *)
{
  open Parser1  (* トークンの宣言は parser3.mli に記述されている *)
  exception Eof
}

let id = ['a'-'z'] ['a'-'z' '0'-'9']*

rule token = parse
     ['0'-'9']+  as vl { NUM (int_of_string(vl)) }
   | "print"         { PRINT }
   | id	 as str	     { ID str }
   | '='             { ASSIGN }
   | '+'             { PLUS }
   | '-'             { MINUS }
   | '*'             { TIMES }
   | '/'             { DIV }
   | '('             { LP }
   | ')'             { RP }
   | ';'             { SEMI }
   | [' ' '\t'] { token lexbuf }
   | '\n'            { EOL }
   | eof             { raise Eof }
