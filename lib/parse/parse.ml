open Chasm.Instructions
open Printf

let instruction_regex =
  let prefix, suffix = {|^\s*|}, {|\s*$|} in
  let binary_main_ops = {|(.+?)\s+(:=|\?|\?\?|\+=|-=|\*=|\/=|&=|\|=|\^=|<<=|>>=|\*>>=)\s+(.+?)|} in
  let unary_main_ops = {|(.+?)\s+(->|<-|!)|} in
  let jump = {|jump\s+(?:\((=|!=|<|<=|>|>=|\$<|\$<=|\$>|\$>=)\)\s+)?(.*?)|} in
  let full_exp = sprintf "%s(?:%s|%s|%s)%s" prefix binary_main_ops unary_main_ops jump suffix in
  Re2.create_exn full_exp

let value_of_str s =
  Register s

let jump_details_of_str s =
  Label s

exception InternalParserError of string

let instruction_of_match m =
  let get_match i = Re2.Match.get ~sub:(`Index i) m in
  match get_match 1 with
  | Some left -> (
    (* Binop main instructions case *)
    let infix =
      match get_match 2 with
      | Some op -> op
      | None -> raise (InternalParserError "Binop main case is missing an infix group") in
    let right =
      match get_match 3 with
      | Some r -> r
      | None -> raise (InternalParserError "Binop main case is missing a second operand") in
    let right_v = value_of_str right in
    match infix with
    | ":=" -> Move (left, right_v)
    | "?" -> Compare (value_of_str left, right_v, Bit)
    | "??" -> Compare (value_of_str left, right_v, Diff)
    | "+=" -> Add (left, right_v)
    | "-=" -> Sub (left, right_v)
    | "*=" -> Mul (left, right_v)
    | "/=" -> Div (left, right_v)
    | "&=" -> And (left, right_v)
    | "|=" -> Or (left, right_v)
    | "^=" -> Xor (left, right_v)
    | "<<=" -> ShiftLeft (left, right_v)
    | ">>=" -> ShiftRight (left, right_v, Log)
    | "*>>=" -> ShiftRight (left, right_v, Arith)
    | _ -> raise (InternalParserError "Binop main case infix does not match a binary main chasm instruction")
  )
  | None -> (
    match get_match 4 with
    | Some only -> (
      (* Unop main instructions case *)
      let op =
        match get_match 5 with
        | Some op_ -> op_
        | None -> raise (InternalParserError "Unop main case is missing an operation group") in
      match op with
      | "->" -> Push (value_of_str only)
      | "<-" -> Pop only
      | "!" -> Not only
      | _ -> raise (InternalParserError "Unop main case operation does not match a unary main chasm instruction")
    )
    | None -> (
      (* Jump instructions case *)
      let right =
        match get_match 7 with
        | Some right_ -> right_
        | None -> raise (InternalParserError "Jump case is missing a destination") in
      let jump_details = jump_details_of_str right in
      match get_match 6 with
      | Some infix -> (
        let cond =
          match infix with
          | "=" -> Eq
          | "!=" -> NotEq
          | "<" -> Less
          | "<=" -> LessEq
          | ">" -> Greater
          | ">=" -> GreaterEq
          | "$<" -> SLess
          | "$<=" -> SLessEq
          | "$>" -> SGreater
          | "$>=" -> SGreaterEq
          | _ -> raise (InternalParserError "Jump case condition does not match a chasm jump condition") in
        Jump (cond, jump_details)
      )
      | None -> Jump (Direct, jump_details)
    )
  )

let parse_instruction (asm : string) =
  match Re2.first_match instruction_regex asm with
  | Core.Ok match_ -> Ok (instruction_of_match match_)
  | Core.Error e -> Error e
