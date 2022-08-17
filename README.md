# chasm
**ch**asm's an **as**se**m**bler...?  
**c**hasm's **h**ardly an **as**se**m**bler...?  
**c**an **h**ardly **as**se**m**ble...?

chasm is a RISC-y assembly language not specific to any ISA but which can be translated to other assembly languages. The chasm library exposes types which represent chasm instructions (see the [`Instructions`](./lib/instructions.ml) module) and provides back ends which emit chasm assembly and x86 assembly as well as a chasm assembly parser.
