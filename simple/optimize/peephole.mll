let digit = ['0'-'9']
let reg = '%'['a'-'z' '0'-'9']+
let inst = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*
let rand = [^',''\n']*
let offset = ['A'-'Z' '0'-'9']+
let mem = '-'? offset? '(' reg ')'
let iconst = '$' [^',''\n']*
let label = 'L' digit+

rule peephole = parse
| ("\tjg " (label as l1) '\n'
  "\tjmp " (label as l2) '\n'
  (label ':' as l3) as org) { if l1 = l3 then print_string ("\tjle " ^ l2 ^ "\n" ^ l3 ^ ":")
                              else print_string org; peephole lexbuf }
| ("\tjl " (label as l1) '\n'
  "\tjmp " (label as l2) '\n'
  (label as l3) ':'  as org) { if l1 = l3 then print_string ("\tjge " ^ l2 ^ "\n" ^ l3 ^ ":")
                              else print_string org; peephole lexbuf }
| ("\tjge " (label as l1) '\n'
  "\tjmp " (label as l2) '\n'
  (label as l3) ':'  as org) { if l1 = l3 then print_string ("\tjl " ^ l2 ^ "\n" ^ l3 ^ ":")
                              else print_string org; peephole lexbuf }
| ("\tjle " (label as l1) '\n'
  "\tjmp " (label as l2) '\n'
  (label as l3) ':'  as org) { if l1 = l3 then print_string ("\tjg " ^ l2 ^ "\n" ^ l3 ^ ":")
                              else print_string org; peephole lexbuf }
| ("\tje " (label as l1) '\n'
  "\tjmp " (label as l2) '\n'
  (label as l3) ':'  as org) { if l1 = l3 then print_string ("\tjne " ^ l2 ^ "\n" ^ l3 ^ ":")
                              else print_string org; peephole lexbuf }
| ("\tjne " (label as l1) '\n'
  "\tjmp " (label as l2) '\n'
  (label as l3) ':'  as org) { if l1 = l3 then print_string ("\tje " ^ l2 ^ "\n" ^ l3 ^ ":")
                              else print_string org; peephole lexbuf }
| ("\tmovq" ' ' ( reg as src1 ) ", " ( mem as dst1 ) '\n' as line1 ) 
   "\tmovq" ' ' ( mem as src2 ) ", " ( reg as dst2 ) as org
          { if (dst1 = src2) then Printf.printf "%s\tmovq %s, %s" line1 src1 dst2 
            else print_string org; peephole lexbuf }
| "\tmovq " (iconst as src1) ", " (reg as dst1) '\n'
  "\tmovq " (iconst as src2) ", " (reg as dst2) '\n'
  '\t' (inst as op) ' ' (reg as src3) ", " (reg as dst3)
          { if dst1 = src3 && dst2 = dst3 then 
                   match op with
                      "addq" -> let rlt = (int_of_string src1) + (int_of_string src2) in Printf.printf "\tmovq %d, %s" rlt dst3
                     | _ -> ()
            else if dst2 = src3 && dst1 = dst3 then
                   match op with
                      "addq" -> let rlt = (int_of_string src2) + (int_of_string src1) in Printf.printf "\tmovq %d, %s" rlt dst3 
                     | _ -> () }
| ('\t' inst ' ' rand ", " (reg as dst1) '\n' 
  ('\t' inst ' ' ((reg as src2) | '-'? offset? '(' ( reg as src2) ')') ", " (reg as dst2) as line2) as org) 
          { if (dst1 <> src2 && dst1 = dst2) then print_string line2 
            else print_string org; peephole lexbuf }
| '\t' inst ' ' rand ", " ( mem as dst1 ) '\n' 
 ('\t' inst ' ' rand ", " ( mem as dst2 ) as line2)  as org
          { if (dst1 = dst2) then print_string line2 
            else print_string org; peephole lexbuf }
| "\tpushq " ( rand as dst2 ) '\n' 
  "\tpopq " ( rand as dst3 ) 
          { if dst2 <> dst3 then Printf.printf "\tmovq %s, %s" dst2 dst3; peephole lexbuf }
| ('\t' inst ' ' rand ", "  as front) ( reg as dst1 ) '\n' 
   "\tmovq" ' ' ( reg as src2 ) ", " ( reg as dst2 )  as org  
          { if (dst1 = src2) then print_string (front^dst2) else print_string org; peephole lexbuf  }
| eof as org     { print_string org }
| _  as ch             { print_char ch; peephole lexbuf }

{
   let lexbuf = Lexing.from_channel stdin in
          let _ = peephole lexbuf in exit 0
}
