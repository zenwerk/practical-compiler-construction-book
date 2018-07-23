%token<int> NUM
%token<string> ID
%token PLUS MINUS TIMES DIV ASSIGN PRINT LP RP SEMI EOL
%right SEMI
%left PLUS MINUS        /* 最低優先順位 */
%left TIMES DIV         /* 中間優先順位 */

%start prog
%type <Interp.stmt> prog

%%

prog: s EOL                { $1 }
    ;

s   : s SEMI s             { Interp.Stmts ($1,$3) }
    | ID ASSIGN e          { Interp.Assign ($1,$3) }
    | PRINT LP e RP        { Interp.Print ($3) }
    ;

e   : ID                   { Interp.Id $1 }
    | NUM                  { Interp.Num $1 }
    | LP e RP              { $2 }
    | e PLUS e             { Interp.Plus ($1,$3) }
    | e MINUS e            { Interp.Minus ($1,$3) } 
    | e TIMES e            { Interp.Times ($1,$3) }
    | e DIV e              { Interp.Div ($1,$3) }
    ;

%%