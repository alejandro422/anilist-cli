type t = { text : string }

let make text =
  let trimmedText = String.trim text in
  if trimmedText = "" then invalid_arg "Directive text cannot be empty"
  else if trimmedText.[0] = '@' then { text = trimmedText }
  else { text = "@" ^ trimmedText }

let render directive = directive.text
