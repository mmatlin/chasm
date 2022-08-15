open Chasm.Instructions
module E_x86 = Emit.Emit_x86
module E_chasm = Emit.Emit_chasm

let () = List.iter print_endline (
    List.map (E_x86.asm_of_instruction []) [
      (Move (E_x86.rdi, Immediate 100L));
      (Sub (E_x86.rsp, Register E_x86.rdi));
      (Compare (Register E_x86.rax, Register E_x86.rsp, Bit));
      (Jump (SLessEq, (Label "so_true_destie")));
      (Pop None);
    ] @ [
      E_x86.asm_of_instruction
        [("jmpdist", "short")]
        (Jump (Direct, (Forward 16L)))
    ]
  );
  print_newline ()

let () = List.iter print_endline (
    List.map (E_chasm.asm_of_instruction []) [
      (Move ("reg1", Immediate 100L));
      (Sub ("reggg", Register "reg1"));
      (Compare (Register "rax", Register "rsp", Bit));
      (Jump (SLessEq, (Label "labelebal")));
      (Pop (Some "rdi"));
      (Push (Immediate 10101L));
    ] @ [
      E_chasm.asm_of_instruction
        [("stack_ptr_reg", "some_rsp"); ("pop_width", "800")]
        (Pop None)
    ]
  )

let () = print_endline @@
  try (E_chasm.asm_of_instruction [] (Pop None)) with
  | Failure msg -> msg
