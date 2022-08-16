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
  );
  print_newline ()

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
