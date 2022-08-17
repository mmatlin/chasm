exception InternalParserError of string

val parse_instruction : string -> (Chasm.Instructions.instruction, Base.Error.t) result
