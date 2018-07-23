(* 生成規則 left=右辺, right=左辺 *)
type rule = {left: string; right: string list}

(* 文法 *)
let grammer1 =
  [ {left= "Z"; right= ["d"]}
  ; {left= "Z"; right= ["X"; "Y"; "Z"]}
  ; {left= "Y"; right= []}
  ; {left= "Y"; right= ["c"]}
  ; {left= "X"; right= ["Y"]}
  ; {left= "X"; right= ["a"]} ]

let grammer2 =
  [ {left= "S'"; right= ["L"; "$"]}
  ; {left= "L"; right= ["S"; ";"; "L"]}
  ; {left= "L"; right= ["S"]}
  ; {left= "S"; right= ["id"; "="; "E"]}
  ; {left= "S"; right= ["print"; "("; "E"; ")"]}
  ; {left= "E"; right= ["T"; "E'"]}
  ; {left= "E'"; right= ["+"; "T"; "E'"]}
  ; {left= "E'"; right= ["-"; "T"; "E'"]}
  ; {left= "E'"; right= []}
  ; {left= "T"; right= ["F"; "T'"]}
  ; {left= "T'"; right= ["*"; "F"; "T'"]}
  ; {left= "T'"; right= ["/"; "F"; "T'"]}
  ; {left= "T'"; right= []}
  ; {left= "F"; right= ["id"]}
  ; {left= "F"; right= ["num"]}
  ; {left= "F"; right= ["("; "E"; ")"]} ]

let grammer3 =
  [ {left= "S'"; right= ["L"; "$"]}
  ; {left= "L"; right= ["S"; "L'"]}
  ; {left= "L'"; right= [";"; "L"]}
  ; {left= "L'"; right= []}
  ; {left= "S"; right= ["id"; "="; "E"]}
  ; {left= "S"; right= ["print"; "("; "E"; ")"]}
  ; {left= "E"; right= ["T"; "E'"]}
  ; {left= "E'"; right= ["+"; "T"; "E'"]}
  ; {left= "E'"; right= ["-"; "T"; "E'"]}
  ; {left= "E'"; right= []}
  ; {left= "T"; right= ["F"; "T'"]}
  ; {left= "T'"; right= ["*"; "F"; "T'"]}
  ; {left= "T'"; right= ["/"; "F"; "T'"]}
  ; {left= "T'"; right= []}
  ; {left= "F"; right= ["id"]}
  ; {left= "F"; right= ["num"]}
  ; {left= "F"; right= ["("; "E"; ")"]} ]

let grammer4 =
  [ {left= "S"; right= ["E"; "$"]}
  ; {left= "E"; right= ["F"; "+"; "T"]}
  ; {left= "E"; right= ["T"]}
  ; {left= "T"; right= ["F"]}
  ; {left= "F"; right= ["-"; "T"]}
  ; {left= "F"; right= ["num"]} ]

let grammer5 =
  [ {left= "S"; right= ["id"; ":="; "E"]}
  ; {left= "S"; right= ["if"; "E"; "then"; "S"; "X"]}
  ; {left= "X"; right= []}
  ; {left= "X"; right= ["else"; "S"]}
  ; {left= "E"; right= ["T"; "E'"]}
  ; {left= "E'"; right= ["+"; "T"; "E'"]}
  ; {left= "E'"; right= []}
  ; {left= "T"; right= ["num"]} ]

let grammer6 =
  [ {left= "S"; right= ["E"; "$"]}
  ; {left= "E"; right= ["id"]}
  ; {left= "E"; right= ["id"; "("; "E"; ")"]}
  ; {left= "E"; right= ["E"; "+"; "id"]} ]

let grammers = [grammer1; grammer2; grammer3; grammer4; grammer5; grammer6]

(** re はリストから重複した要素を削除する関数 *)
let re lst =
  List.fold_left
    (fun rlt x -> if List.mem x rlt then rlt else rlt @ [x])
    [] lst

let rec calcNullable_1 grammer =
  let rec calc lst = function
    | [] -> lst
    | h :: rest ->
        if
          List.length h.right = 0
          || List.for_all (fun symbol -> List.mem symbol lst) h.right
        then calc (h.left :: lst) rest
        else calc lst rest
  in
  calc [] grammer

(* getTerms は文法の終端記号と非終端記号を取得する *)
let getTerms grammer =
  let rec _getTerms nonTerms terms = function
    | [] -> re nonTerms, re terms
    | h :: rest -> _getTerms (h.left :: nonTerms) (terms @ h.right) rest
  in
  _getTerms [] [] grammer


let rec calcNullable grammer =
  let symbols = getSymbols grammer in
  let rec calc =
    grammer





let rec main = function
  | [] -> print_string "\n"
  | h :: rest ->
      let nullable = calcNullable_1 h in
      for i = 0 to List.length nullable - 1 do
        Printf.printf "nullable(%s): true\n" (List.nth nullable i)
        (* if nullable.(i) then print_string "true\n" else print_string "false\n" *)
      done ;
      print_string "-------------------\n" ;
      main rest

let () = main grammers
