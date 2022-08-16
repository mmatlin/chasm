open Chasm.Instructions
open Shared
open Printf

let str_of_value = function
  | Register r -> r
  | Immediate i -> Int64.to_string i

let infix_of_cmp_mode = function
  | Diff -> "??"
  | Bit -> "?"

let infix_of_shift_right_type = function
  | Log -> ">>="
  | Arith -> "*>>="

let infix_of_jump_condition = function
  | Direct -> None
  | Eq -> Some "="
  | NotEq -> Some "!="
  | Less -> Some "<"
  | LessEq -> Some "<="
  | Greater -> Some ">"
  | GreaterEq -> Some ">="
  | SLess -> Some "$<"
  | SLessEq -> Some "$<="
  | SGreater -> Some "$>"
  | SGreaterEq -> Some "$>="

let asm_of_instruction ?(flags = []) ins =
  match ins with
  | Move (r, v) -> sprintf "%s := %s" r (str_of_value v)
  | Push v -> sprintf "%s ->" (str_of_value v)
  | Pop r -> sprintf "%s <-" r
  | Compare (v1, v2, mode) ->
    sprintf "%s %s %s"
      (str_of_value v1)
      (infix_of_cmp_mode mode)
      (str_of_value v2)
  | Add (r, v) -> sprintf "%s += %s" r (str_of_value v)
  | Sub (r, v) -> sprintf "%s -= %s" r (str_of_value v)
  | Mul (r, v) -> sprintf "%s *= %s" r (str_of_value v)
  | Div (r, v) -> sprintf "%s /= %s" r (str_of_value v)
  | And (r, v) -> sprintf "%s &= %s" r (str_of_value v)
  | Or (r, v) -> sprintf "%s |= %s" r (str_of_value v)
  | Xor (r, v) -> sprintf "%s ^= %s" r (str_of_value v)
  | Not r -> sprintf "%s !" r
  | ShiftLeft (r, v) -> sprintf "%s <<= %s" r (str_of_value v)
  | ShiftRight (r, v, t) ->
    sprintf "%s %s %s" r (infix_of_shift_right_type t) (str_of_value v)
  | Jump (cond, details) ->
    let dest =
      match details with
      | Backward i -> sprintf "-%Ld" i
      | Forward i -> sprintf "+%Ld" i
      | Label l -> l in
    match infix_of_jump_condition cond with
    | Some infix -> sprintf "jump (%s) %s" infix dest
    | None -> sprintf "jump %s" dest
