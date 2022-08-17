open Chasm.Instructions
module E_x86 = Emit.Emit_x86
module E_chasm = Emit.Emit_chasm

let () = List.iter print_endline (
  List.map E_x86.asm_of_instruction [
    (Move (E_x86.rdi, Immediate 100L));
    (Sub (E_x86.rsp, Register E_x86.rdi));
    (Compare (Register E_x86.rax, Register E_x86.rsp, Bit));
    (Jump (SLessEq, (Label "so_true_destie")));
    (Add (E_x86.rsp, Immediate E_x86.width));
  ] @ [
    E_x86.asm_of_instruction
      ~flags:[("jmpdist", "short")]
      (Jump (Direct, (Forward 16L)))
  ]
); print_newline ()

let () = List.iter print_endline (
  List.map E_chasm.asm_of_instruction [
    (Move ("reg1", Immediate 100L));
    (Sub ("reggg", Register "reg1"));
    (Compare (Register "rax", Register "rsp", Bit));
    (Jump (SLessEq, (Label "labelebal")));
    (Pop "rdi");
    (Push (Immediate 10101L));
  ]
)

let () = print_endline (
  let test_str = E_chasm.asm_of_instruction (Move ("reg1", Immediate 100L)) in
  print_endline @@ Printf.sprintf "test_str: \"%s\"" test_str;
  match Parse.parse_instruction test_str with
  | Ok i -> E_chasm.asm_of_instruction i
  | Error e -> Printf.sprintf "error: %s" (Core.Error.to_string_hum e)
); print_newline ()

let testcases = [
  "  abc := def  ";
  "  abc ? def  ";
  "  abc ?? def  ";
  "  abc += def  ";
  "  abc -= def  ";
  "  abc *= def  ";
  "  abc /= def  ";
  "  abc &= def  ";
  "  abc |= def  ";
  "  abc ^= def  ";
  "  abc <<= def  ";
  "  abc >>= def  ";
  "  abc *>>= def  ";

  "  abc ->  ";
  "  abc <-  ";
  "  abc !  ";

  "  jump label  ";
  "  jump (=) label  ";
  "  jump (!=) 100  ";
  "  jump (<) 100  ";
  "  jump (<=) 100  ";
  "  jump (>) 100  ";
  "  jump (>=) 100  ";
  "  jump ($<) 100  ";
  "  jump ($<=) 100  ";
  "  jump ($>) 100  ";
  "  jump ($>=) 100  ";
]

let () = List.iter (
  fun (a_of_i, name) ->
    print_endline @@ Printf.sprintf "Name: %s...\n" name;
    List.iter print_endline (
      List.map (
        fun s ->
          match Parse.parse_instruction s with
          | Ok i -> (try a_of_i i with Emit.Shared.EmitterError s -> Printf.sprintf "(failure: %s)" s)
          | Error e -> Printf.sprintf "error: %s" (Core.Error.to_string_hum e)
      ) testcases
    );
    print_newline ()
) [(E_chasm.asm_of_instruction ~flags:[], "chasm"); (E_x86.asm_of_instruction ~flags:[], "x86")]
