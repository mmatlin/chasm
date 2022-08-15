open Chasm.Instructions

let () = List.iter print_endline (
    List.map (Chasm.X86.asm_of_instruction []) [
      (Move (Chasm.X86.rdi, Immediate 100L));
      (Sub (Chasm.X86.rsp, Register Chasm.X86.rdi));
      (Compare (Register Chasm.X86.rax, Register Chasm.X86.rsp, Bit));
      (Jump (SLessEq, (Label "so_true_destie")));
      (Pop None);
    ] @ [
      Chasm.X86.asm_of_instruction
        [("jmpdist", "short")]
        (Jump (Direct, (Forward 16L)))
    ]
  )
