SuperSoft HR Disassembly
------------------------

This disassembly is for the HR40 ROM. Hopefully an HR80 ROM will be added in the future.
The original ROM was a combination EDITOR ROM and SuperSoft ROM. As such, it contained
three parts:

$E000-$E7FF - Editor ROM for 40-column PET
$E800-$E8FF - Hidden are under I/O space - Contains copywrite text
$E900-$EFFF - SuperSoft HR graphics firmware

For this project we are only interested in the SuperSoft firmware are to make things a
bit easier to work with.

I am using my own CBM-Transfer Disassembler. It is a symbolic interactive disassembler.
The "ASM-PROJ" file contains the entry points, symbols, tables, labels, and comments
used to create the disassembly and the results are saved to the ASM file.

