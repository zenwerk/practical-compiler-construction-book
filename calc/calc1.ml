let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let rlt = Parser1.prog Lexer1.token lexbuf in
      let _ = Interp.interp rlt in
      flush stdout
    done
  with Lexer1.Eof -> exit 0
