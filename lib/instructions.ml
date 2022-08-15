type comparison_mode =
  | Diff
  | Bit

type shift_right_type =
  | Log
  | Arith

(* type op_signedness =
  | Unsigned
  | Signed *)

type jump_condition =
  | Direct
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | SLess
  | SLessEq
  | SGreater
  | SGreaterEq

type jump_details =
  | Backward of Int64.t
  | Forward of Int64.t
  | Label of string

type register = string

type immediate = Int64.t

type value =
  | Register of register
  | Immediate of immediate

type instruction =
  | Move of register * value
  | Push of value
  | Pop of register option
  | Compare of value * value * comparison_mode
  | Add of register * value
  | Sub of register * value
  | Mul of register * value (* * op_signedness *)
  | Div of register * value (* * op_signedness *)
  | And of register * value
  | Or of register * value
  | Xor of register * value
  | Not of register
  | ShiftLeft of register * value
  | ShiftRight of register * value * shift_right_type
  | Jump of jump_condition * jump_details
