let rule1 =
  [["Z"; "d"]; ["Z"; "X"; "Y"; "Z"]; ["Y"]; ["Y"; "c"]; ["X"; "Y"]; ["X"; "a"]]

let rule2 =
  [ ["S'"; "L"; "$"]
  ; ["L"; "S"; ";"; "L"]
  ; ["L"; "S"]
  ; ["S"; "id"; "="; "E"]
  ; ["S"; "print"; "("; "E"; ")"]
  ; ["E"; "T"; "E'"]
  ; ["E'"; "+"; "T"; "E'"]
  ; ["E'"; "-"; "T"; "E'"]
  ; ["E'"]
  ; ["T"; "F"; "T'"]
  ; ["T'"; "*"; "F"; "T'"]
  ; ["T'"; "/"; "F"; "T'"]
  ; ["T'"]
  ; ["F"; "id"]
  ; ["F"; "num"]
  ; ["F"; "("; "E"; ")"] ]

let rule3 =
  [ ["S'"; "L"; "$"]
  ; ["L"; "S"; "L'"]
  ; ["L'"; ";"; "L"]
  ; ["L'"]
  ; ["S"; "id"; "="; "E"]
  ; ["S"; "print"; "("; "E"; ")"]
  ; ["E"; "T"; "E'"]
  ; ["E'"; "+"; "T"; "E'"]
  ; ["E'"; "-"; "T"; "E'"]
  ; ["E'"]
  ; ["T"; "F"; "T'"]
  ; ["T'"; "*"; "F"; "T'"]
  ; ["T'"; "/"; "F"; "T'"]
  ; ["T'"]
  ; ["F"; "id"]
  ; ["F"; "num"]
  ; ["F"; "("; "E"; ")"] ]

let rule4 =
  [ ["S"; "E"; "$"]
  ; ["E"; "F"; "+"; "T"]
  ; ["E"; "T"]
  ; ["T"; "F"]
  ; ["F"; "-"; "T"]
  ; ["F"; "num"] ]

let rule5 =
  [ ["S"; "id"; ":="; "E"]
  ; ["S"; "if"; "E"; "then"; "S"; "X"]
  ; ["X"]
  ; ["X"; "else"; "S"]
  ; ["E"; "T"; "E'"]
  ; ["E'"; "+"; "T"; "E'"]
  ; ["E'"]
  ; ["T"; "num"] ]

let rule6 =
  [ ["S"; "E"; "$"]
  ; ["E"; "id"]
  ; ["E"; "id"; "("; "E"; ")"]
  ; ["E"; "E"; "+"; "id"] ]

exception Error

(* First, Follow集合を計算する関数 *)
let firstFollow rules =
  (** re はリストから重複した要素を削除する関数 *)
  let re lst =
    List.fold_left
      (* x が リストrlt になかったら追加する *)
      (fun rlt x -> if List.mem x rlt then rlt else rlt @ [x])
      [] lst
  in
  (** extNonTerm は 'a list list から先頭要素だけを取得し, 重複要素を外したもの; つまり非終端記号を集める関数 *)
  let getNonTerm lst = re (List.map (fun x -> List.hd x) lst) in
  (** 記号から重複を削除する関数 *)
  let extSym l = re (List.flatten l) in

  let nonTerms = getNonTerm rules in (* 非終端記号 *)
  let terms =  (* 終端記号 *)
    List.filter (fun x -> not (List.mem x nonTerms)) (extSym rules)
  in
  let symbols = nonTerms @ terms in         (* 全部の記号 *)
  let symbolNum = List.length symbols in    (* 記号の数 *)
  let nonTermsNum = List.length nonTerms in (* 非終端記号の数 *)
  let ruleNum = List.length rules in        (* 生成規則の数 *)

  let aRules = Array.of_list (List.map (fun x -> Array.of_list x) rules) in (* Rules を 'a Array Array に変換する *)

  let aSymbols = Array.of_list symbols in      (* symbols を Array にする *)
  let nullable = Array.make symbolNum false in (* 空導出可能な記号を保存するための Array を準備する *)
  let first = Array.make symbolNum [] in       (* FIRST集合を保存するための Array を準備する *)
  let follow = Array.make symbolNum [] in      (* FOLLOW集合を保存するための Array を準備する *)

  (** initializeFirstTable はすべての非終端記号をリストに入れて, FIRST集合の配列につっこむ *)
  let initializeFirstTable () =
    for i = 0 to symbolNum - 1 do
      if List.mem aSymbols.(i) terms then first.(i) <- [aSymbols.(i)]
    done
  in
  (** s2i は記号のリストから sym を検索し, そのインデックスを返す *)
  let s2i symbol =
    let rec search cnt l =
      match l with
      | [] -> raise Error
      | h :: rest -> if h = symbol then cnt else search (cnt + 1) rest
    in
    search 0 symbols
  in
  (** subList は 'a list の index:x から index:y の部分からなるリストを返す *)
  let subList a x y = Array.to_list (Array.sub a x (y - x + 1)) in
  (** isNullable は指定した symbol がNullableかどうかをboolで返す *)
  let isNullable symbol = nullable.(s2i symbol) in

  (* FIRST,FOLLOW集合を計算する関数 *)
  let rec calcFirstFollow () =
    let oldNullable = Array.copy nullable
    and oldFirst = Array.copy first
    and oldFollow = Array.copy follow in
    (* 各生成規則毎にループ処理 *)
    for r = 0 to ruleNum - 1 do
      let rule = aRules.(r) in
      let ruleLength = Array.length rule in
      (*
       * Nullable の更新
       *)
      if (* 生成規則が 'X ->' のような場合(空導出する) or 生成規則の右辺が全て nullable の場合 *)
        ruleLength = 1 || List.for_all isNullable (List.tl (Array.to_list rule))  (* 生成規則の右辺をリスト化 *)
      then (* 生成規則の左辺をnullbaleフラグを立てる *)
        nullable.(s2i rule.(0)) <- true ;


      (* FIRST, FOLLOW に変化があるか確認する処理開始 *)
      if ruleLength > 1 then (* 空導出じゃない生成規則なら *)
        for i = 1 to ruleLength - 1 do (* 生成規則の右辺で回す *)
          (* 
           * FIRST集合の更新 
           *)
          if (* 最初のループ? or 生成規則を頭からループ箇所まで順にnullableか確認する *)
            i = 1 || List.for_all isNullable (subList rule 1 (i - 1))
          then (* FIRST(生成規則の左辺) <- FIRST(生成規則の左辺) @ FIRST(生成規則の右辺[i]) *)
            (* 最初のループ(i=1)のときに, 必ず以下が実行される *)
            first.(s2i rule.(0)) <- re (first.(s2i rule.(0)) @ first.(s2i rule.(i))) ;

          (*
           * FOLLOW集合の更新
           *)
          if (* 最後のループ? or 生成規則をループ場所から最後まで順にnullableか確認する *)
            i = ruleLength - 1 || List.for_all isNullable (subList rule (i + 1) (ruleLength - 1))
          then (* FOLLOW(現在処理中の記号) <- FOLLOW(現在処理中の記号) @ FOLLOW(生成規則の左辺) *)
            follow.(s2i rule.(i)) <- re (follow.(s2i rule.(i)) @ follow.(s2i rule.(0))) ;

          (* ループの最後じゃなければ *)
          if i < ruleLength - 1 then
            (* 現在処理中の記号+1 から最後まで *)
            for j = i + 1 to ruleLength - 1 do
              if (* 最初のループ? or  *)
                j = i + 1 || List.for_all isNullable (subList rule (i + 1) (j - 1))
              then (* FOLLOW(現在処理中の記号) <- FOLLOW(現在処理中の記号) @ FIRST(ループ中の記号) *)
                follow.(s2i rule.(i)) <- re (follow.(s2i rule.(i)) @ first.(s2i rule.(j)))
            done
          (* endif *)
        done
    done ;

    if (* nullbale, first, follow に変化があれば再起を継続 *)
      not (oldNullable = nullable)
      || not (oldFirst = first)
      || not (oldFollow = follow)
    then calcFirstFollow ()
  in

  (* 処理を実施 *)
  initializeFirstTable () ;
  calcFirstFollow () ;

  (* nullable の結果表示 *)
  for i = 0 to nonTermsNum - 1 do
    Printf.printf "nullable(%s):\t" aSymbols.(i) ;
    if nullable.(i) then print_string "true\n" else print_string "false\n"
  done ;
  print_string "\n" ;

  (* FIRST集合の結果表示 *)
  for i = 0 to nonTermsNum - 1 do
    Printf.printf "FIRST(%s):\t" aSymbols.(i) ;
    List.iter (fun x -> Printf.printf "%s " x) first.(i) ;
    print_string "\n"
  done ;

  (* FOLLOW集合の結果表示 *)
  print_string "\n" ;
  for i = 0 to nonTermsNum - 1 do
    Printf.printf "FOLLOW(%s):\t" aSymbols.(i) ;
    List.iter (fun x -> Printf.printf "%s " x) follow.(i) ;
    print_string "\n"
  done

let rec main = function
  | [] -> ()
  | h :: rest -> 
    firstFollow h;
    print_string "--------------------------------\n\n";
    main rest

let () = main [rule1; rule2; rule3; rule4; rule5; rule6]