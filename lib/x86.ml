open Instructions
open Printf

let width = 8L

let rax = "rax"
let rbx = "rbx"
let rcx = "rcx"
let rdx = "rdx"
let rsi = "rsi"
let rdi = "rdi"
let rbp = "rbp"
let rsp = "rsp"
let r8 = "r8"
let r9 = "r9"
let r10 = "r10"
let r11 = "r11"
let r12 = "r12"
let r13 = "r13"
let r14 = "r14"
let r15 = "r15"

let str_of_value = function
  | Register r -> r
  | Immediate i -> Int64.to_string i

let ins_name_of_cmp_mode = function
  | Diff -> "cmp"
  | Bit -> "test"

let ins_name_of_shift_right_type = function
  | Log -> "shr"
  | Arith -> "sar"

let ins_name_of_jump_condition = function
  | Direct -> "jmp"
  | Eq -> "je"
  | NotEq -> "jne"
  | Less -> "jb"
  | LessEq -> "jbe"
  | Greater -> "ja"
  | GreaterEq -> "jae"
  | SLess -> "jl"
  | SLessEq -> "jle"
  | SGreater -> "jg"
  | SGreaterEq -> "jge"

let read_flag name flags =
  match List.find_opt (fun (f, _) -> f = name) flags with
  | Some (_, v) -> Some v
  | None -> None

let rec asm_of_instruction flags ins =
  match ins with
  | Move (r, v) -> sprintf "mov %s, %s" r (str_of_value v)
  | Push v -> sprintf "push %s" (str_of_value v)
  | Pop ro ->
      begin
      match ro with
      | Some r -> sprintf "pop %s" r
      | None -> asm_of_instruction flags (Add ("rsp", Immediate width))
      end
  | Compare (v1, v2, mode) ->
      sprintf "%s %s, %s"
        (ins_name_of_cmp_mode mode)
        (str_of_value v1)
        (str_of_value v2)
  | Add (r, v) -> sprintf "add %s, %s" r (str_of_value v)
  | Sub (r, v) -> sprintf "sub %s, %s" r (str_of_value v)
  | Mul (r, v) -> sprintf "imul %s, %s" r (str_of_value v)
  | Div _ -> failwith "x86 div instructions not supported"
  | And (r, v) -> sprintf "and %s, %s" r (str_of_value v)
  | Or (r, v) -> sprintf "or %s, %s" r (str_of_value v)
  | Xor (r, v) -> sprintf "xor %s, %s" r (str_of_value v)
  | Not r -> sprintf "not %s" r
  | ShiftLeft (r, v) -> sprintf "shl %s, %s" r (str_of_value v)
  | ShiftRight (r, v, t) ->
      sprintf "%s %s, %s" (ins_name_of_shift_right_type t) r (str_of_value v)
  | Jump (cond, details) ->
      let jmpdist_part =
        match read_flag "jmpdist" flags with
        | Some dist -> [dist]
        | None -> [] in
      String.concat " " (
        (ins_name_of_jump_condition cond)::(
          jmpdist_part @ [
            match details with
            | Backward i -> sprintf "$-%Ld" i
            | Forward i -> sprintf "$+%Ld" i
            | Label l -> l
          ]
        )
      )
