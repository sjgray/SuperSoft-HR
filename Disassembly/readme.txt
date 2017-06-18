SuperSoft HR Disassembly
------------------------

This disassembly is for the HR40 ROM. Hopefully an HR80 ROM will be added in the future.
The original ROM was a combination EDITOR ROM and SuperSoft ROM. As such, it contained
three parts:

1) $E000-$E7FF - Editor ROM for 40-column PET
2) $E800-$E8FF - Hidden area under I/O space - Contains copywrite text
3) $E900-$EFFF - SuperSoft HR graphics firmware

The main code is in part 3. Part 2 contains text with copywrite info.
Part 1 is standard Editor ROM, however some unused bytes near the end contain some SuperSoft code.

I am using my own CBM-Transfer Disassembler. It is a symbolic interactive disassembler.
The "ASM-PROJ" file contains the entry points, symbols, tables, labels, and comments
used to create the disassembly and the results are saved to the ASM file.

