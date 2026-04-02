let wordsOfCliToken token =
  token |> String.split_on_char '-' |> List.filter (fun word -> word <> "")

let capitalize word =
  if String.length word = 0 then ""
  else
    let firstCharacter = Char.uppercase_ascii word.[0] in
    if String.length word = 1 then String.make 1 firstCharacter
    else
      String.make 1 firstCharacter ^ String.sub word 1 (String.length word - 1)

let uncapitalize word =
  if String.length word = 0 then ""
  else
    let firstCharacter = Char.lowercase_ascii word.[0] in
    if String.length word = 1 then String.make 1 firstCharacter
    else
      String.make 1 firstCharacter ^ String.sub word 1 (String.length word - 1)

let upperCamelCaseOfCliToken token =
  token |> wordsOfCliToken |> List.map capitalize |> String.concat ""

let lowerCamelCaseOfCliToken token =
  match wordsOfCliToken token with
  | [] -> ""
  | firstWord :: remainingWords ->
      uncapitalize firstWord
      ^ String.concat "" (List.map capitalize remainingWords)
