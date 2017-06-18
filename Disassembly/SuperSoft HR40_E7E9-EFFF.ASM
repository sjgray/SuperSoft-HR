*=$E7E9
;
; ---- Code
;
;================================================================================
; This is located in an unused area at the end of the Editor ROM
;================================================================================
;
L_E7E9:
E7E9: A2 07     LDX #$07                                    
E7EB: A0 07     LDY #$07                                    
;
L_E7ED:
E7ED: B1 B7     LDA (TMPB7),Y                                ;Temp Data Area (64#00A3)
E7EF: 9D F7 87  STA $87F7,X                                 
E7F2: CA        DEX                                         
E7F3: 88        DEY                                         
E7F4: 10 F7     BPL L_E7ED                                  
E7F6: 24 CB     BIT CMP0                                     ;Tape Timing Constants
E7F8: 10 03     BPL L_E7FD                                  
E7FA: 4C D6 EC  JMP L_ECD6                                  
;
L_E7FD:
E7FD: 4C DE EC  JMP L_ECDE                                  
;================================================================================
; This area is normally hidden under the IO area from $E800-E8FF
;================================================================================
;
L_E800:
E800:           !TEXT "COPYRIGHT 1982 SUPERSOFT. 10-14 CANNING ROA" ;Hidden IO Area - Copyright
E82B:           !TEXT "D, WEALDSTONE, HARROW, MIDDLESEX. UNAUTHORI" ;Hidden IO Area - Copyright
E856:           !TEXT "SED REPRODUCTION IS STRICTLY PROHIBITED. SO" ;Hidden IO Area - Copyright
E881:           !TEXT "FTWARE WRITTEN BY NJ RICHMAN. HARWARE DESIG" ;Hidden IO Area - Copyright
E8AC:           !TEXT "NED AND MANUFACTURED BY MG BIGNELL AND M HU" ;Hidden IO Area - Copyright
E8D7:           !TEXT "MPHREY.SERIAL NUMBER OF THIS UNIT =414480" ;Hidden IO Area - Copyright
 
;================================================================================
; Start of SuperSoft Code
;================================================================================
E900: EA        NOP                                         
E901: EA        NOP                                         
;--------------------------------------------------------------------------------
; Entry point for SYS commands 59650
;--------------------------------------------------------------------------------
E902: BA        TSX                                         
E903: 86 B4     STX $B4                                     
E905: 20 F5 BE  JSR TSTCOMMA                                 ;Test comma-
E908: 84 96     STY STATUS                                   ;Kernal I/O Status Word: ST
E90A: A2 0D     LDX #$0D                                    
;
L_E90C:
E90C: DD 2A E9  CMP CMDS,X                                  
E90F: F0 10     BEQ DO_CMD                                  
E911: CA        DEX                                         
E912: 10 F8     BPL L_E90C                                  
E914: 20 D4 C8  JSR EVAL_BP                                  ;Eval Byte Parameter
E917: 8A        TXA                                         
;--------------------------------------------------------------------------------
; If the command was not found use the value for the Control Register
;--------------------------------------------------------------------------------
;
JTTarget10:
E918: 29 0F     AND #$0F                                    
E91A: 8D FF EF  STA CONTROL_REG                              ;Control Register
E91D: 8D 79 02  STA CONTROL_SAVE                             ;Control Register Save
E920: 60        RTS                                         
;
;--------------------------------------------------------------------------------
; Push the target address on the stack and jump to it using RTS
;--------------------------------------------------------------------------------
;
DO_CMD:
E921: BD 38 E9  LDA CMDVEC_HI,X                             
E924: 48        PHA                                         
E925: BD 46 E9  LDA CMDVEC_LO,X                             
E928: 48        PHA                                         
E929: 60        RTS                                         
;
;--------------------------------------------------------------------------------
; Command Table
;--------------------------------------------------------------------------------
;
CMDS:
E92A:           !TEXT "OKP",$A4,$8B,"TMECIRFDS"             ;Command Table
 
;--------------------------------------------------------------------------------
; Command Vectors
;--------------------------------------------------------------------------------
;
CMDVEC_HI:
E938:           !BYTE $E9,$E9,$E9,$EB,$EB,$EB,$ED,$EB       ;command vector HI
E940:           !BYTE $EE,$EE,$E9,$E9,$E9,$E9               ;command vector HI
 
;
CMDVEC_LO:
E946:           !BYTE $53,$53,$71,$CA,$C4,$C7,$73,$A6       ;command vector LO
E94E:           !BYTE $6F,$72,$87,$8A,$8D,$84               ;command vector LO
 
;================================================================================
; "O" and "K" COMMANDS. Unknown. O=Off or K=KILL?? Appears to restore CHRGET routine
;================================================================================
;
CMD_O:
E954: A2 17     LDX #$17                                    
;
L_E956:
E956:           !BYTE $2C                                   ;bit command
 
;
L_E957:
E957: A2 06     LDX #$06                                    
;
L_E959:
E959: BD 99 D3  LDA ROM_CHRGET,X                             ;CHRGET For Zero-page
E95C: 95 70     STA CHRGET,X                                 ;Subroutine: Get Next Byte of BASIC Text
E95E: E0 09     CPX #$09                                    
E960: F0 F5     BEQ L_E957                                  
E962: CA        DEX                                         
E963: 10 F4     BPL L_E959                                  
E965: 78        SEI                                         
E966: A9 55     LDA #$55                                    ; $E455 is default IRQ Vector
E968: A0 E4     LDY #$E4                                    
E96A: 85 90     STA CINV-LO                                  ;Vector: Hardware Interrupt LO
E96C: 84 91     STY CINV-HI                                  ;Vector: Hardware Interrupt HI
E96E: 58        CLI                                         
E96F: 4C 70 00  JMP CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
;================================================================================
; "P" COMMAND. Set Pointer to Character Set
;================================================================================
;
CMD_P:
E972: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
E975: 20 F5 BE  JSR TSTCOMMA                                 ;Test comma-
E978: 20 84 BD  JSR FRMNUM                                   ;Confirm Result
E97B: 20 2D C9  JSR GETADR                                   ;Convert FAC#1 to Integer in LINNUM
E97E: 8C F8 A7  STY CHR_PTR_LO                               ;Character Pointer LO
E981: 8D F9 A7  STA CHR_PTR_HI                               ;Character Pointer HI
E984: 60        RTS                                         
;
;================================================================================
; "S" COMMANDS. +"P"=Set Point. +"L"=Set Line
;================================================================================
;
CMD_S:
E985: A9 00     LDA #$00                                    
;
L_E987:
E987:           !BYTE $2C                                   ;bit command
 
;================================================================================
; "R" COMMAND. Reset Line.
;================================================================================
;
CMD_R:
E988: A9 01     LDA #$01                                    
;
L_E98A:
E98A:           !BYTE $2C                                   ;bit command
 
;================================================================================
; "F" COMMAND. +"L"=Flip Line. +"P"=Flip Point
;================================================================================
;
CMD_F:
E98B: A9 02     LDA #$02                                    
;
L_E98D:
E98D:           !BYTE $2C                                   ;bit command
 
;================================================================================
; "D" COMMAND. Dot Line.
;================================================================================
;
CMD_D:
E98E: A9 03     LDA #$03                                    
E990: 20 F6 EE  JSR L_EEF6                                  
E993: A9 00     LDA #$00                                    
E995: 85 FF     STA FREEFF                                   ;Available ZP location
E997: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
E99A: C9 4C     CMP #$4C                                    ; Check second character. Is it "L"? L=Line.
E99C: F0 06     BEQ CMD_LINE                                
E99E: C6 FF     DEC FREEFF                                   ;Available ZP location
E9A0: C9 50     CMP #$50                                    
E9A2: D0 2F     BNE L_E9D3                                  
;
CMD_LINE:
E9A4: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
E9A7: 20 F5 BE  JSR TSTCOMMA                                 ;Test comma-
E9AA: 20 3D EC  JSR L_EC3D                                  
E9AD: F0 27     BEQ L_E9D6                                  
E9AF: 24 FF     BIT FREEFF                                   ;Available ZP location
E9B1: 30 20     BMI L_E9D3                                  
;
L_E9B3:
E9B3: 20 BD EE  JSR L_EEBD                                  
E9B6: 24 FF     BIT FREEFF                                   ;Available ZP location
E9B8: 30 35     BMI L_E9EF                                  
E9BA: 20 3A EC  JSR L_EC3A                                  
;
L_E9BD:
E9BD: 20 D4 EE  JSR L_EED4                                  
E9C0: 20 CA EE  JSR JTTarget08                              
E9C3: AD EE 93  LDA $93EE                                   
E9C6: 4D F2 93  EOR $93F2                                   
E9C9: 29 01     AND #$01                                    
E9CB: D0 34     BNE JTTarget13                              
E9CD: A9 01     LDA #$01                                    
E9CF: 85 FF     STA FREEFF                                   ;Available ZP location
E9D1: D0 2E     BNE JTTarget13                              
;
L_E9D3:
E9D3: 4C 00 BF  JMP SYNERR                                   ;Output ?SYNTAX Error
;
L_E9D6:
E9D6: 24 FF     BIT FREEFF                                   ;Available ZP location
E9D8: 30 D9     BMI L_E9B3                                  
E9DA: 20 D4 EE  JSR L_EED4                                  
E9DD: AD F1 93  LDA $93F1                                   
E9E0: 8D F2 93  STA $93F2                                   
E9E3: AD EC 93  LDA $93EC                                   
E9E6: AC ED 93  LDY $93ED                                   
E9E9: 20 C3 EE  JSR L_EEC3                                  
E9EC: 4C BD E9  JMP L_E9BD                                  
;
L_E9EF:
E9EF: 20 CA EE  JSR JTTarget08                              
E9F2: 20 C0 EE  JSR JTTarget07                              
E9F5: 4D F1 93  EOR $93F1                                   
E9F8: 29 01     AND #$01                                    
E9FA: F0 02     BEQ L_E9FE                                  
E9FC: E6 FF     INC FREEFF                                   ;Available ZP location
;
L_E9FE:
E9FE: 4C 2C EB  JMP JTTarget14                              
;
JTTarget13:
EA01: A5 FF     LDA FREEFF                                   ;Available ZP location
EA03: 29 01     AND #$01                                    
EA05: 85 FF     STA FREEFF                                   ;Available ZP location
EA07: 38        SEC                                         
EA08: AD EC 93  LDA $93EC                                   
EA0B: ED EE 93  SBC $93EE                                   
EA0E: 85 C0     STA PTR1                                     ;Tape Pass 1 Error Log
EA10: AD ED 93  LDA $93ED                                   
EA13: ED EF 93  SBC $93EF                                   
EA16: 85 C1     STA PTR2                                     ;Tape Pass 2 Error Log
EA18: B0 16     BCS L_EA30                                  
EA1A: 49 FF     EOR #$FF                                    
EA1C: 85 C1     STA PTR2                                     ;Tape Pass 2 Error Log
EA1E: A5 C0     LDA PTR1                                     ;Tape Pass 1 Error Log
EA20: 49 FF     EOR #$FF                                    
EA22: 69 01     ADC #$01                                    
EA24: 85 C0     STA PTR1                                     ;Tape Pass 1 Error Log
EA26: D0 02     BNE L_EA2A                                  
EA28: E6 C1     INC PTR2                                     ;Tape Pass 2 Error Log
;
L_EA2A:
EA2A: A5 FF     LDA FREEFF                                   ;Available ZP location
EA2C: 09 40     ORA #$40                                    
EA2E: 85 FF     STA FREEFF                                   ;Available ZP location
;
L_EA30:
EA30: 38        SEC                                         
EA31: AD F1 93  LDA $93F1                                   
EA34: ED F2 93  SBC $93F2                                   
EA37: 85 CB     STA CMP0                                     ;Tape Timing Constants
EA39: B0 0C     BCS L_EA47                                  
EA3B: 49 FF     EOR #$FF                                    
EA3D: 69 01     ADC #$01                                    
EA3F: 85 CB     STA CMP0                                     ;Tape Timing Constants
EA41: A5 FF     LDA FREEFF                                   ;Available ZP location
EA43: 09 80     ORA #$80                                    
EA45: 85 FF     STA FREEFF                                   ;Available ZP location
;
L_EA47:
EA47: A5 CB     LDA CMP0                                     ;Tape Timing Constants
EA49: F0 10     BEQ L_EA5B                                  
EA4B: A5 C1     LDA PTR2                                     ;Tape Pass 2 Error Log
EA4D: D0 33     BNE L_EA82                                  
EA4F: A5 C0     LDA PTR1                                     ;Tape Pass 1 Error Log
EA51: F0 14     BEQ L_EA67                                  
EA53: C5 CB     CMP CMP0                                     ;Tape Timing Constants
EA55: F0 1C     BEQ L_EA73                                  
EA57: B0 29     BCS L_EA82                                  
EA59: 90 63     BCC L_EABE                                  
;
L_EA5B:
EA5B: 20 2C EB  JSR JTTarget14                              
;
L_EA5E:
EA5E: 20 F6 EA  JSR L_EAF6                                  
EA61: 20 78 EB  JSR L_EB78                                  
EA64: 4C 5E EA  JMP L_EA5E                                  
;
L_EA67:
EA67: 20 2C EB  JSR JTTarget14                              
;
L_EA6A:
EA6A: 20 19 EB  JSR L_EB19                                  
EA6D: 20 78 EB  JSR L_EB78                                  
EA70: 4C 6A EA  JMP L_EA6A                                  
;
L_EA73:
EA73: 20 2C EB  JSR JTTarget14                              
;
L_EA76:
EA76: 20 19 EB  JSR L_EB19                                  
EA79: 20 F9 EA  JSR L_EAF9                                  
EA7C: 20 78 EB  JSR L_EB78                                  
EA7F: 4C 76 EA  JMP L_EA76                                  
;
L_EA82:
EA82: A5 C1     LDA PTR2                                     ;Tape Pass 2 Error Log
EA84: 4A        LSR                                         
EA85: 85 B3     STA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
EA87: A5 C0     LDA PTR1                                     ;Tape Pass 1 Error Log
EA89: 6A        ROR                                         
EA8A: 85 B2     STA DPSW                                     ;Flag: Tape Byte-Received
EA8C: 20 2C EB  JSR JTTarget14                              
;
L_EA8F:
EA8F: 20 F6 EA  JSR L_EAF6                                  
EA92: 18        CLC                                         
EA93: A5 B2     LDA DPSW                                     ;Flag: Tape Byte-Received
EA95: 65 CB     ADC CMP0                                     ;Tape Timing Constants
EA97: 85 B2     STA DPSW                                     ;Flag: Tape Byte-Received
EA99: 90 02     BCC L_EA9D                                  
EA9B: E6 B3     INC TMPB3                                    ;Temporary save eg. logical address or DOS wedge
;
L_EA9D:
EA9D: 38        SEC                                         
EA9E: A5 B2     LDA DPSW                                     ;Flag: Tape Byte-Received
EAA0: E5 C0     SBC PTR1                                     ;Tape Pass 1 Error Log
EAA2: A5 B3     LDA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
EAA4: E5 C1     SBC PTR2                                     ;Tape Pass 2 Error Log
EAA6: 90 10     BCC L_EAB8                                  
EAA8: 20 1C EB  JSR L_EB1C                                  
EAAB: 38        SEC                                         
EAAC: A5 B2     LDA DPSW                                     ;Flag: Tape Byte-Received
EAAE: E5 C0     SBC PTR1                                     ;Tape Pass 1 Error Log
EAB0: 85 B2     STA DPSW                                     ;Flag: Tape Byte-Received
EAB2: A5 B3     LDA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
EAB4: E5 C1     SBC PTR2                                     ;Tape Pass 2 Error Log
EAB6: 85 B3     STA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
;
L_EAB8:
EAB8: 20 78 EB  JSR L_EB78                                  
EABB: 4C 8F EA  JMP L_EA8F                                  
;
L_EABE:
EABE: A5 CB     LDA CMP0                                     ;Tape Timing Constants
EAC0: 4A        LSR                                         
EAC1: 85 B2     STA DPSW                                     ;Flag: Tape Byte-Received
EAC3: A9 00     LDA #$00                                    
EAC5: 85 B3     STA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
EAC7: 20 2C EB  JSR JTTarget14                              
;
L_EACA:
EACA: 20 19 EB  JSR L_EB19                                  
EACD: 18        CLC                                         
EACE: A5 B2     LDA DPSW                                     ;Flag: Tape Byte-Received
EAD0: 65 C0     ADC PTR1                                     ;Tape Pass 1 Error Log
EAD2: 85 B2     STA DPSW                                     ;Flag: Tape Byte-Received
EAD4: A5 B3     LDA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
EAD6: 65 C1     ADC PTR2                                     ;Tape Pass 2 Error Log
EAD8: 85 B3     STA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
EADA: D0 06     BNE L_EAE2                                  
EADC: A5 B2     LDA DPSW                                     ;Flag: Tape Byte-Received
EADE: C5 CB     CMP CMP0                                     ;Tape Timing Constants
EAE0: 90 0E     BCC L_EAF0                                  
;
L_EAE2:
EAE2: 20 F9 EA  JSR L_EAF9                                  
EAE5: 38        SEC                                         
EAE6: A5 B2     LDA DPSW                                     ;Flag: Tape Byte-Received
EAE8: E5 CB     SBC CMP0                                     ;Tape Timing Constants
;
TraceEAEA:
EAEA: 85 B2     STA DPSW                                     ;Flag: Tape Byte-Received
EAEC: B0 02     BCS L_EAF0                                  
EAEE: C6 B3     DEC TMPB3                                    ;Temporary save eg. logical address or DOS wedge
;
L_EAF0:
EAF0: 20 78 EB  JSR L_EB78                                  
EAF3: 4C CA EA  JMP L_EACA                                  
;
L_EAF6:
EAF6: 20 1C EF  JSR L_EF1C                                  
;
L_EAF9:
EAF9: 24 FF     BIT FREEFF                                   ;Available ZP location
EAFB: 70 0E     BVS L_EB0B                                  
EAFD: 46 CC     LSR $CC                                     
EAFF: 90 07     BCC L_EB08                                  
EB01: A9 80     LDA #$80                                    
EB03: 85 CC     STA $CC                                     
EB05: 20 70 EF  JSR JTTarget02                              
;
L_EB08:
EB08: 4C 77 EF  JMP JTTarget05                              
;
L_EB0B:
EB0B: 06 CC     ASL $CC                                     
EB0D: 90 07     BCC L_EB16                                  
EB0F: A9 01     LDA #$01                                    
EB11: 85 CC     STA $CC                                     
EB13: 20 80 EF  JSR JTTarget01                              
;
L_EB16:
EB16: 4C 89 EF  JMP JTTarget06                              
;
L_EB19:
EB19: 20 13 EF  JSR L_EF13                                  
;
L_EB1C:
EB1C: 24 FF     BIT FREEFF                                   ;Available ZP location
EB1E: 30 06     BMI L_EB26                                  
EB20: 20 2D EF  JSR JTTarget12                              
EB23: 4C 53 EF  JMP JTTarget04                              
;
L_EB26:
EB26: 20 2D EF  JSR JTTarget12                              
EB29: 4C 34 EF  JMP JTTarget03                              
;
JTTarget14:
EB2C: AD EF 93  LDA $93EF                                   
EB2F: 85 CA     STA $CA                                     
EB31: AD EE 93  LDA $93EE                                   
EB34: 48        PHA                                         
EB35: 46 CA     LSR $CA                                     
EB37: 6A        ROR                                         
EB38: 4A        LSR                                         
EB39: 4A        LSR                                         
EB3A: 85 C9     STA EAL                                      ;Tape End Addresses/End of Program
EB3C: 20 2D EF  JSR JTTarget12                              
EB3F: 48        PHA                                         
EB40: 4A        LSR                                         
EB41: 4A        LSR                                         
EB42: 4A        LSR                                         
EB43: A8        TAY                                         
EB44: B9 98 E7  LDA $E798,Y                                 
EB47: 48        PHA                                         
EB48: A2 03     LDX #$03                                    
EB4A: C0 14     CPY #$14                                    
EB4C: B0 0B     BCS L_EB59                                  
EB4E: CA        DEX                                         
EB4F: C0 0D     CPY #$0D                                    
EB51: B0 06     BCS L_EB59                                  
EB53: CA        DEX                                         
EB54: C0 07     CPY #$07                                    
EB56: B0 01     BCS L_EB59                                  
EB58: CA        DEX                                         
;
L_EB59:
EB59: 68        PLA                                         
EB5A: 20 08 EF  JSR L_EF08                                  
EB5D: 68        PLA                                         
EB5E: 29 07     AND #$07                                    
EB60: A8        TAY                                         
EB61: B9 B9 EF  LDA TABLE1,Y                                
EB64: 18        CLC                                         
EB65: 20 0E EF  JSR L_EF0E                                  
EB68: 68        PLA                                         
EB69: 29 07     AND #$07                                    
EB6B: A8        TAY                                         
EB6C: C8        INY                                         
EB6D: A5 BB     LDA BUFPNT                                   ;Pointer: Tape I/O Buffer #1
EB6F: C9 04     CMP #$04                                    
EB71: F0 17     BEQ L_EB8A                                  
EB73: 20 95 EF  JSR L_EF95                                  
EB76: 85 CC     STA $CC                                     
;
L_EB78:
EB78: A5 CC     LDA $CC                                     
EB7A: A6 BB     LDX BUFPNT                                   ;Pointer: Tape I/O Buffer #1
EB7C: F0 0D     BEQ L_EB8B                                  
EB7E: E0 01     CPX #$01                                    
EB80: F0 20     BEQ L_EBA2                                  
EB82: E0 03     CPX #$03                                    
EB84: F0 0C     BEQ L_EB92                                  
;
L_EB86:
EB86: 51 C9     EOR (EAL),Y                                  ;Tape End Addresses/End of Program
EB88: 91 C9     STA (EAL),Y                                  ;Tape End Addresses/End of Program
;
L_EB8A:
EB8A: 60        RTS                                         
;
;
L_EB8B:
EB8B: 49 FF     EOR #$FF                                    
EB8D: 31 C9     AND (EAL),Y                                  ;Tape End Addresses/End of Program
EB8F: 91 C9     STA (EAL),Y                                  ;Tape End Addresses/End of Program
EB91: 60        RTS                                         
;
;
L_EB92:
EB92: 48        PHA                                         
EB93: A5 FF     LDA FREEFF                                   ;Available ZP location
EB95: 49 01     EOR #$01                                    
EB97: 85 FF     STA FREEFF                                   ;Available ZP location
EB99: 68        PLA                                         
EB9A: 66 FF     ROR FREEFF                                   ;Available ZP location
EB9C: 08        PHP                                         
EB9D: 26 FF     ROL FREEFF                                   ;Available ZP location
EB9F: 28        PLP                                         
EBA0: 90 E9     BCC L_EB8B                                  
;
L_EBA2:
EBA2: 11 C9     ORA (EAL),Y                                  ;Tape End Addresses/End of Program
EBA4: 91 C9     STA (EAL),Y                                  ;Tape End Addresses/End of Program
EBA6: 60        RTS                                         
;
;================================================================================
; "E" COMMAND. Examine.
;================================================================================
;
CMD_E:
EBA7: 20 F4 EE  JSR JTTarget09                              
EBAA: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBAD: 20 3A EC  JSR L_EC3A                                  
EBB0: D0 3C     BNE L_EBEE                                  
EBB2: 20 BD EE  JSR L_EEBD                                  
EBB5: 20 CA EE  JSR JTTarget08                              
;
JTTarget17:
EBB8: 20 2C EB  JSR JTTarget14                              
EBBB: 20 95 EF  JSR L_EF95                                  
EBBE: 31 C9     AND (EAL),Y                                  ;Tape End Addresses/End of Program
EBC0: D0 02     BNE L_EBC4                                  
EBC2: E6 96     INC STATUS                                   ;Kernal I/O Status Word: ST
;
L_EBC4:
EBC4: 60        RTS                                         
;
;================================================================================
; $8B COMMAND (RVS K). Unknown.
;================================================================================
;
CMD_8B:
EBC5: A9 00     LDA #$00                                    
;
L_EBC7:
EBC7:           !BYTE $2C                                   ;bit command
 
;================================================================================
; "T" COMMAND. Text. +"NH"=Normal Horizontal
;================================================================================
;
CMD_T:
EBC8: A9 FF     LDA #$FF                                    
;
L_EBCA:
EBCA:           !BYTE $2C                                   ;bit command
 
;================================================================================
; $A4 COMMAND (RVS $). Unknown.
;================================================================================
;
CMD_A4:
EBCB: A9 40     LDA #$40                                    
EBCD: 85 BE     STA BITCI                                    ;Cassette Temp (64#00A8)
EBCF: 20 F4 EE  JSR JTTarget09                              
EBD2: A9 FF     LDA #$FF                                    
EBD4: 85 B1     STA PRTY                                     ;Tape Character Parity
EBD6: 85 BD     STA INBIT                                    ;Cassette Temp (64#00A7)
EBD8: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBDB: C9 52     CMP #$52                                    ; Is it "R"?
EBDD: F0 06     BEQ L_EBE5                                  
EBDF: C9 4E     CMP #$4E                                    ; Is it "N"?
EBE1: D0 0B     BNE L_EBEE                                  
EBE3: E6 B1     INC PRTY                                     ;Tape Character Parity
;
L_EBE5:
EBE5: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBE8: C9 48     CMP #$48                                    ; Is it "H"?
EBEA: F0 06     BEQ L_EBF2                                  
EBEC: C9 56     CMP #$56                                    ; Is it "V"?
;
L_EBEE:
EBEE: D0 74     BNE L_EC64                                  
EBF0: E6 BD     INC INBIT                                    ;Cassette Temp (64#00A7)
;
L_EBF2:
EBF2: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBF5: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBF8: 20 98 BD  JSR FRMEVL                                   ;Evaluate Expression in Text
EBFB: 24 07     BIT VAUYP                                    ;Data Type: $FF = String
EBFD: 10 57     BPL L_EC56                                  
EBFF: 20 B8 C7  JSR $C7B8                                   
EC02: C9 00     CMP #$00                                    
EC04: F0 2F     BEQ L_EC35                                  
EC06: 85 B2     STA DPSW                                     ;Flag: Tape Byte-Received
EC08: 86 32     STX FRESPC                                   ;Utility String Pointer
EC0A: 84 33     STY $33                                     
EC0C: 20 76 00  JSR CHRGOT                                   ;Entry to Get Same Byte of Text Again
EC0F: F0 05     BEQ L_EC16                                  
EC11: 20 BA EE  JSR L_EEBA                                  
;
JTTarget18:
EC14: C6 96     DEC STATUS                                   ;Kernal I/O Status Word: ST
;
L_EC16:
EC16: 20 46 ED  JSR L_ED46                                  
EC19: 90 0E     BCC L_EC29                                  
EC1B: 24 96     BIT STATUS                                   ;Kernal I/O Status Word: ST
EC1D: 10 07     BPL L_EC26                                  
EC1F: 20 9F EC  JSR JTTarget19                              
EC22: E6 96     INC STATUS                                   ;Kernal I/O Status Word: ST
EC24: F0 03     BEQ L_EC29                                  
;
L_EC26:
EC26: 20 72 EC  JSR JTTarget11                              
;
L_EC29:
EC29: C6 B2     DEC DPSW                                     ;Flag: Tape Byte-Received
;
L_EC2B:
EC2B: F0 97     BEQ L_EBC4                                  
EC2D: E6 32     INC FRESPC                                   ;Utility String Pointer
EC2F: D0 E5     BNE L_EC16                                  
EC31: E6 33     INC $33                                     
EC33: D0 E1     BNE L_EC16                                  
;
L_EC35:
EC35: A2 FD     LDX #$FD                                    
EC37: 4C DA EE  JMP L_EEDA                                  
;
L_EC3A:
EC3A: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
;
L_EC3D:
EC3D: 20 B1 EE  JSR L_EEB1                                  
EC40: A5 61     LDA $61                                     
EC42: A4 62     LDY $62                                     
EC44: 85 12     STA $12                                     
EC46: 84 11     STY LINNUM                                   ;Temp: Integer Value
EC48: 20 AE EE  JSR L_EEAE                                  
EC4B: A6 62     LDX $62                                     
EC4D: A5 61     LDA $61                                     
EC4F: F0 02     BEQ L_EC53                                  
;
TraceEC51:
EC51: A2 FF     LDX #$FF                                    
;
L_EC53:
EC53: 4C 76 00  JMP CHRGOT                                   ;Entry to Get Same Byte of Text Again
;
L_EC56:
EC56: 20 87 BD  JSR $BD87                                   
EC59: 20 D7 C8  JSR $C8D7                                   
EC5C: 86 BC     STX BUFP2                                    ;Pointer: Tape I/O Buffer #2
EC5E: F0 12     BEQ JTTarget11                              
EC60: C9 2C     CMP #$2C                                    ; Is it COMMA?
EC62: F0 35     BEQ L_EC99                                  
;
L_EC64:
EC64: 4C 00 BF  JMP SYNERR                                   ;Output ?SYNTAX Error
;
L_EC67:
EC67: AC F2 93  LDY $93F2                                   
EC6A: F0 BF     BEQ L_EC2B                                  
EC6C: CE F2 93  DEC $93F2                                   
EC6F: 4C 9F EC  JMP JTTarget19                              
;
JTTarget11:
EC72: A5 BD     LDA INBIT                                    ;Cassette Temp (64#00A7)
EC74: F0 F1     BEQ L_EC67                                  
EC76: 18        CLC                                         
EC77: AD F2 93  LDA $93F2                                   
EC7A: 69 07     ADC #$07                                    
EC7C: 8D F2 93  STA $93F2                                   
EC7F: 18        CLC                                         
EC80: AD EE 93  LDA $93EE                                   
EC83: AC EF 93  LDY $93EF                                   
EC86: 69 08     ADC #$08                                    
EC88: 90 01     BCC L_EC8B                                  
EC8A: C8        INY                                         
;
L_EC8B:
EC8B: C0 00     CPY #$00                                    
EC8D: F0 0D     BEQ L_EC9C                                  
EC8F: C0 01     CPY #$01                                    
EC91: D0 A2     BNE L_EC35                                  
EC93: C9 39     CMP #$39                                    
EC95: B0 9E     BCS L_EC35                                  
EC97: 90 03     BCC L_EC9C                                  
;
L_EC99:
EC99: 20 BA EE  JSR L_EEBA                                  
;
L_EC9C:
EC9C: 20 C3 EE  JSR L_EEC3                                  
;
JTTarget19:
EC9F: A9 00     LDA #$00                                    
ECA1: 85 B8     STA $B8                                     
ECA3: A5 BC     LDA BUFP2                                    ;Pointer: Tape I/O Buffer #2
ECA5: 0A        ASL                                         
ECA6: 26 B8     ROL $B8                                     
ECA8: 0A        ASL                                         
ECA9: 26 B8     ROL $B8                                     
ECAB: 0A        ASL                                         
ECAC: 26 B8     ROL $B8                                     
ECAE: 18        CLC                                         
ECAF: 6D F8 A7  ADC CHR_PTR_LO                               ;Character Pointer LO
ECB2: 85 B7     STA TMPB7                                    ;Temp Data Area (64#00A3)
ECB4: A5 B8     LDA $B8                                     
ECB6: 6D F9 A7  ADC CHR_PTR_HI                               ;Character Pointer HI
ECB9: 85 B8     STA $B8                                     
ECBB: A0 00     LDY #$00                                    
ECBD: 84 CB     STY CMP0                                     ;Tape Timing Constants
ECBF: 2C F9 A7  BIT CHR_PTR_HI                               ;Character Pointer HI
ECC2: 10 0F     BPL L_ECD3                                  
ECC4: C6 CB     DEC CMP0                                     ;Tape Timing Constants
ECC6: BA        TSX                                         
ECC7: 98        TYA                                         
ECC8: 9D FE 00  STA $00FE,X                                 
;
L_ECCB:
ECCB: BD FE 00  LDA $00FE,X                                 
ECCE: F0 FB     BEQ L_ECCB                                  
ECD0: 8C FF EF  STY CONTROL_REG                              ;Control Register
;
L_ECD3:
ECD3: 4C E9 E7  JMP L_E7E9                                  ; Jump to patch area in Editor ROM area
;
L_ECD6:
ECD6: AD 79 02  LDA CONTROL_SAVE                             ;Control Register Save
ECD9: 09 01     ORA #$01                                    
ECDB: 8D FF EF  STA CONTROL_REG                              ;Control Register
;
L_ECDE:
ECDE: 20 2C EB  JSR JTTarget14                              
ECE1: 84 FF     STY FREEFF                                   ;Available ZP location
ECE3: A9 00     LDA #$00                                    
ECE5: AA        TAX                                         
;
L_ECE6:
ECE6: 88        DEY                                         
ECE7: F0 04     BEQ L_ECED                                  
ECE9: 38        SEC                                         
ECEA: 6A        ROR                                         
ECEB: 90 F9     BCC L_ECE6                                  
;
L_ECED:
ECED: 85 CC     STA $CC                                     
;
L_ECEF:
ECEF: B1 C9     LDA (EAL),Y                                  ;Tape End Addresses/End of Program
ECF1: 49 FF     EOR #$FF                                    
ECF3: 24 BE     BIT BITCI                                    ;Cassette Temp (64#00A8)
ECF5: 10 02     BPL L_ECF9                                  
ECF7: 25 CC     AND $CC                                     
;
L_ECF9:
ECF9: 85 B3     STA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
ECFB: 84 FB     STY STAL                                     ;I/O Start Address
ECFD: BD F7 87  LDA $87F7,X                                 
ED00: 45 B1     EOR PRTY                                     ;Tape Character Parity
ED02: A4 FF     LDY FREEFF                                   ;Available ZP location
;
L_ED04:
ED04: 88        DEY                                         
ED05: F0 0E     BEQ L_ED15                                  
ED07: 4A        LSR                                         
ED08: 66 FB     ROR STAL                                     ;I/O Start Address
ED0A: 90 F8     BCC L_ED04                                  
ED0C: EA        NOP                                         
;
L_ED0D:
ED0D: 45 B3     EOR TMPB3                                    ;Temporary save eg. logical address or DOS wedge
ED0F: 50 0A     BVC L_ED1B                                  
;
L_ED11:
ED11: 45 FB     EOR STAL                                     ;I/O Start Address
ED13: 50 19     BVC L_ED2E                                  
;
L_ED15:
ED15: 24 BE     BIT BITCI                                    ;Cassette Temp (64#00A8)
ED17: 50 F4     BVC L_ED0D                                  
ED19: 05 B3     ORA TMPB3                                    ;Temporary save eg. logical address or DOS wedge
;
L_ED1B:
ED1B: 49 FF     EOR #$FF                                    
ED1D: 91 C9     STA (EAL),Y                                  ;Tape End Addresses/End of Program
ED1F: C8        INY                                         
ED20: B1 C9     LDA (EAL),Y                                  ;Tape End Addresses/End of Program
ED22: 24 BE     BIT BITCI                                    ;Cassette Temp (64#00A8)
ED24: 10 02     BPL L_ED28                                  
ED26: 05 CC     ORA $CC                                     
;
L_ED28:
ED28: 49 FF     EOR #$FF                                    
ED2A: 50 E5     BVC L_ED11                                  
ED2C: 05 FB     ORA STAL                                     ;I/O Start Address
;
L_ED2E:
ED2E: 49 FF     EOR #$FF                                    
ED30: 91 C9     STA (EAL),Y                                  ;Tape End Addresses/End of Program
ED32: 88        DEY                                         
ED33: E8        INX                                         
ED34: E0 08     CPX #$08                                    
ED36: D0 02     BNE L_ED3A                                  
;
L_ED38:
ED38: 60        RTS                                         
;
ED39: EA        NOP                                         
;
L_ED3A:
ED3A: 20 2D EF  JSR JTTarget12                              
ED3D: C9 C7     CMP #$C7                                    
ED3F: F0 F7     BEQ L_ED38                                  
ED41: 20 34 EF  JSR JTTarget03                              
ED44: D0 A9     BNE L_ECEF                                  
;
L_ED46:
ED46: A0 00     LDY #$00                                    
ED48: B1 32     LDA (FRESPC),Y                               ;Utility String Pointer
ED4A: 29 7F     AND #$7F                                    
ED4C: C9 12     CMP #$12                                    
ED4E: D0 08     BNE L_ED58                                  
ED50: A9 FF     LDA #$FF                                    
ED52: 45 B1     EOR PRTY                                     ;Tape Character Parity
ED54: 85 B1     STA PRTY                                     ;Tape Character Parity
ED56: 18        CLC                                         
;
L_ED57:
ED57: 60        RTS                                         
;
;
L_ED58:
ED58: C9 20     CMP #$20                                    
ED5A: 90 FB     BCC L_ED57                                  
ED5C: B1 32     LDA (FRESPC),Y                               ;Utility String Pointer
ED5E: C9 40     CMP #$40                                    
ED60: 90 0E     BCC L_ED70                                  
ED62: C9 C0     CMP #$C0                                    
ED64: 90 08     BCC L_ED6E                                  
ED66: C9 FF     CMP #$FF                                    
ED68: 90 02     BCC L_ED6C                                  
ED6A: E9 22     SBC #$22                                    
;
L_ED6C:
ED6C: E9 40     SBC #$40                                    
;
L_ED6E:
ED6E: E9 3F     SBC #$3F                                    
;
L_ED70:
ED70: 85 BC     STA BUFP2                                    ;Pointer: Tape I/O Buffer #2
ED72: 38        SEC                                         
ED73: 60        RTS                                         
;
;================================================================================
; "M" COMMAND. Map Erase, Map Fill.
;================================================================================
;
CMD_M:
ED74: 20 F4 EE  JSR JTTarget09                              
ED77: A9 FF     LDA #$FF                                    
ED79: 85 FB     STA STAL                                     ;I/O Start Address
ED7B: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
ED7E: C9 46     CMP #$46                                    
ED80: F0 09     BEQ L_ED8B                                  
ED82: C9 45     CMP #$45                                    
ED84: F0 03     BEQ L_ED89                                  
ED86: 4C 00 BF  JMP SYNERR                                   ;Output ?SYNTAX Error
;
L_ED89:
ED89: E6 FB     INC STAL                                     ;I/O Start Address
;
L_ED8B:
ED8B: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
ED8E: F0 03     BEQ JTTarget15                              
ED90: 20 BA EE  JSR L_EEBA                                  
;
JTTarget15:
ED93: AD EE 93  LDA $93EE                                   
ED96: AC EF 93  LDY $93EF                                   
ED99: 85 CE     STA BITTS                                    ;Cassette Temp (64#00B4): Tape read timer flag=IRQ enabled for Timer 1
ED9B: 84 CF     STY EOTRD                                    ;End of tape read
ED9D: 20 2C EB  JSR JTTarget14                              
EDA0: 20 9E EF  JSR L_EF9E                                  
EDA3: 20 95 EF  JSR L_EF95                                  
EDA6: 85 CC     STA $CC                                     
EDA8: 85 FF     STA FREEFF                                   ;Available ZP location
;
L_EDAA:
EDAA: 20 B2 EF  JSR L_EFB2                                  
EDAD: D0 12     BNE L_EDC1                                  
EDAF: 20 2D EF  JSR JTTarget12                              
EDB2: C9 C7     CMP #$C7                                    
EDB4: F0 32     BEQ L_EDE8                                  
EDB6: 20 A7 EF  JSR L_EFA7                                  
EDB9: 20 34 EF  JSR JTTarget03                              
EDBC: 20 9E EF  JSR L_EF9E                                  
EDBF: D0 E9     BNE L_EDAA                                  
;
L_EDC1:
EDC1: A5 CE     LDA BITTS                                    ;Cassette Temp (64#00B4): Tape read timer flag=IRQ enabled for Timer 1
EDC3: 8D EE 93  STA $93EE                                   
EDC6: A5 CF     LDA EOTRD                                    ;End of tape read
EDC8: 8D EF 93  STA $93EF                                   
EDCB: 20 2D EF  JSR JTTarget12                              
EDCE: F0 12     BEQ L_EDE2                                  
EDD0: 20 A7 EF  JSR L_EFA7                                  
EDD3: 20 53 EF  JSR JTTarget04                              
EDD6: 20 9E EF  JSR L_EF9E                                  
EDD9: A5 FF     LDA FREEFF                                   ;Available ZP location
EDDB: 85 CC     STA $CC                                     
EDDD: 20 B2 EF  JSR L_EFB2                                  
EDE0: F0 06     BEQ L_EDE8                                  
;
L_EDE2:
EDE2: 60        RTS                                         
;
;
L_EDE3:
EDE3: 20 B2 EF  JSR L_EFB2                                  
EDE6: D0 56     BNE L_EE3E                                  
;
L_EDE8:
EDE8: AD EF 93  LDA $93EF                                   
EDEB: D0 05     BNE L_EDF2                                  
EDED: AD EE 93  LDA $93EE                                   
EDF0: F0 35     BEQ L_EE27                                  
;
L_EDF2:
EDF2: 20 89 EF  JSR JTTarget06                              
EDF5: 06 CC     ASL $CC                                     
EDF7: 90 EA     BCC L_EDE3                                  
;
L_EDF9:
EDF9: 20 80 EF  JSR JTTarget01                              
EDFC: B1 C9     LDA (EAL),Y                                  ;Tape End Addresses/End of Program
EDFE: C5 FB     CMP STAL                                     ;I/O Start Address
EE00: D0 1C     BNE L_EE1E                                  
EE02: AD EF 93  LDA $93EF                                   
EE05: D0 07     BNE L_EE0E                                  
EE07: AD EE 93  LDA $93EE                                   
EE0A: C9 07     CMP #$07                                    
EE0C: F0 16     BEQ L_EE24                                  
;
L_EE0E:
EE0E: 38        SEC                                         
EE0F: AD EE 93  LDA $93EE                                   
EE12: E9 08     SBC #$08                                    
EE14: 8D EE 93  STA $93EE                                   
EE17: B0 E0     BCS L_EDF9                                  
EE19: CE EF 93  DEC $93EF                                   
EE1C: F0 DB     BEQ L_EDF9                                  
;
L_EE1E:
EE1E: A9 01     LDA #$01                                    
EE20: 85 CC     STA $CC                                     
EE22: D0 BF     BNE L_EDE3                                  
;
L_EE24:
EE24: 8C EE 93  STY $93EE                                   
;
L_EE27:
EE27: B1 C9     LDA (EAL),Y                                  ;Tape End Addresses/End of Program
EE29: C5 FB     CMP STAL                                     ;I/O Start Address
EE2B: F0 2D     BEQ L_EE5A                                  
;
L_EE2D:
EE2D: A9 80     LDA #$80                                    
EE2F: 85 CC     STA $CC                                     
;
L_EE31:
EE31: 20 B2 EF  JSR L_EFB2                                  
EE34: F0 03     BEQ L_EE39                                  
;
L_EE36:
EE36: 4C C1 ED  JMP L_EDC1                                  
;
L_EE39:
EE39: A5 CC     LDA $CC                                     
EE3B: 20 86 EB  JSR L_EB86                                  
;
L_EE3E:
EE3E: 20 77 EF  JSR JTTarget05                              
EE41: 46 CC     LSR $CC                                     
EE43: 90 EC     BCC L_EE31                                  
;
L_EE45:
EE45: 20 70 EF  JSR JTTarget02                              
EE48: AD EF 93  LDA $93EF                                   
EE4B: F0 07     BEQ L_EE54                                  
EE4D: AD EE 93  LDA $93EE                                   
EE50: C9 40     CMP #$40                                    
EE52: B0 E2     BCS L_EE36                                  
;
L_EE54:
EE54: B1 C9     LDA (EAL),Y                                  ;Tape End Addresses/End of Program
EE56: C5 FB     CMP STAL                                     ;I/O Start Address
EE58: D0 D3     BNE L_EE2D                                  
;
L_EE5A:
EE5A: A9 FF     LDA #$FF                                    
EE5C: 45 FB     EOR STAL                                     ;I/O Start Address
EE5E: 91 C9     STA (EAL),Y                                  ;Tape End Addresses/End of Program
EE60: 18        CLC                                         
EE61: AD EE 93  LDA $93EE                                   
EE64: 69 08     ADC #$08                                    
EE66: 8D EE 93  STA $93EE                                   
EE69: 90 DA     BCC L_EE45                                  
EE6B: EE EF 93  INC $93EF                                   
EE6E: D0 D5     BNE L_EE45                                  
;================================================================================
; "C" COMMAND. Clear
;================================================================================
;
CMD_C:
EE70: A9 00     LDA #$00                                    
;================================================================================
; "I" COMMAND. Invert
;================================================================================
;
CMD_I:
EE72:           !BYTE $2C                                   ;bit command
 
EE73: A9 FF     LDA #$FF                                    
EE75: 85 FF     STA FREEFF                                   ;Available ZP location
EE77: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EE7A: 20 F8 EE  JSR L_EEF8                                  
;
JTTarget16:
EE7D: A9 00     LDA #$00                                    
EE7F: A0 90     LDY #$90                                    
EE81: 85 C9     STA EAL                                      ;Tape End Addresses/End of Program
EE83: 84 CA     STY $CA                                     
EE85: A9 19     LDA #$19                                    
EE87: 85 FD     STA MEMUSS                                   ;Tape Load Temps
;
L_EE89:
EE89: A2 08     LDX #$08                                    
;
L_EE8B:
EE8B: A0 00     LDY #$00                                    
;
L_EE8D:
EE8D: A9 FF     LDA #$FF                                    
EE8F: 24 FF     BIT FREEFF                                   ;Available ZP location
EE91: 50 02     BVC L_EE95                                  
EE93: 51 C9     EOR (EAL),Y                                  ;Tape End Addresses/End of Program
;
L_EE95:
EE95: 91 C9     STA (EAL),Y                                  ;Tape End Addresses/End of Program
EE97: C8        INY                                         
EE98: C0 28     CPY #$28                                    
EE9A: D0 F1     BNE L_EE8D                                  
EE9C: 20 3D EF  JSR L_EF3D                                  
EE9F: CA        DEX                                         
EEA0: D0 E9     BNE L_EE8B                                  
EEA2: 20 45 EF  JSR L_EF45                                  
EEA5: E9 04     SBC #$04                                    
EEA7: 85 CA     STA $CA                                     
EEA9: C6 FD     DEC MEMUSS                                   ;Tape Load Temps
EEAB: D0 DC     BNE L_EE89                                  
EEAD: 60        RTS                                         
;
;
L_EEAE:
EEAE: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
;
L_EEB1:
EEB1: 20 84 BD  JSR FRMNUM                                   ;Confirm Result
EEB4: 20 51 CD  JSR ROUND                                    ;Round FAC#1
EEB7: 4C EA C2  JMP AYINT                                    ;FAC#1 to Positive Integer
;
L_EEBA:
EEBA: 20 3A EC  JSR L_EC3A                                  
;
L_EEBD:
EEBD: 20 D4 EE  JSR L_EED4                                  
;
JTTarget07:
EEC0: 8E F2 93  STX $93F2                                   
;
L_EEC3:
EEC3: 8D EE 93  STA $93EE                                   
EEC6: 8C EF 93  STY $93EF                                   
EEC9: 60        RTS                                         
;
;
JTTarget08:
EECA: 8E F1 93  STX $93F1                                   
EECD: 8D EC 93  STA $93EC                                   
EED0: 8C ED 93  STY $93ED                                   
EED3: 60        RTS                                         
;
;
L_EED4:
EED4: E0 C8     CPX #$C8                                    
EED6: 90 0B     BCC L_EEE3                                  
;
L_EED8:
EED8: A2 FF     LDX #$FF                                    
;
L_EEDA:
EEDA: 86 96     STX STATUS                                   ;Kernal I/O Status Word: ST
EEDC: 20 83 B8  JSR DO_BRAK                                  ;Perform []
;
L_EEDF:
EEDF: A6 B4     LDX $B4                                     
EEE1: 9A        TXS                                         
EEE2: 60        RTS                                         
;
;
L_EEE3:
EEE3: A5 11     LDA LINNUM                                   ;Temp: Integer Value
EEE5: A4 12     LDY $12                                     
EEE7: C0 00     CPY #$00                                    
EEE9: F0 08     BEQ L_EEF3                                  
EEEB: C0 01     CPY #$01                                    
EEED: D0 E9     BNE L_EED8                                  
EEEF: C9 40     CMP #$40                                    
EEF1: B0 E5     BCS L_EED8                                  
;
L_EEF3:
EEF3: 60        RTS                                         
;
;
JTTarget09:
EEF4: A9 04     LDA #$04                                    
;
L_EEF6:
EEF6: 85 BB     STA BUFPNT                                   ;Pointer: Tape I/O Buffer #1
;
L_EEF8:
EEF8: A2 FE     LDX #$FE                                    
EEFA: AD F3 93  LDA $93F3                                   
EEFD: 49 FF     EOR #$FF                                    
EEFF: 8D F3 93  STA $93F3                                   
EF02: CD F3 93  CMP $93F3                                   
EF05: D0 D3     BNE L_EEDA                                  
EF07: 60        RTS                                         
;
;
L_EF08:
EF08: 18        CLC                                         
EF09: 65 C9     ADC EAL                                      ;Tape End Addresses/End of Program
EF0B: 85 C9     STA EAL                                      ;Tape End Addresses/End of Program
EF0D: 8A        TXA                                         
;
L_EF0E:
EF0E: 65 CA     ADC $CA                                     
EF10: 85 CA     STA $CA                                     
EF12: 60        RTS                                         
;
;
L_EF13:
EF13: AD F2 93  LDA $93F2                                   
EF16: CD F1 93  CMP $93F1                                   
EF19: F0 C4     BEQ L_EEDF                                  
EF1B: 60        RTS                                         
;
;
L_EF1C:
EF1C: AD EF 93  LDA $93EF                                   
EF1F: CD ED 93  CMP $93ED                                   
EF22: D0 08     BNE L_EF2C                                  
EF24: AD EE 93  LDA $93EE                                   
EF27: CD EC 93  CMP $93EC                                   
EF2A: F0 B3     BEQ L_EEDF                                  
;
L_EF2C:
EF2C: 60        RTS                                         
;
;
JTTarget12:
EF2D: 38        SEC                                         
EF2E: A9 C7     LDA #$C7                                    
EF30: ED F2 93  SBC $93F2                                   
EF33: 60        RTS                                         
;
;
JTTarget03:
EF34: CE F2 93  DEC $93F2                                   
EF37: 29 07     AND #$07                                    
EF39: C9 07     CMP #$07                                    
EF3B: F0 08     BEQ L_EF45                                  
;
L_EF3D:
EF3D: 18        CLC                                         
EF3E: A5 CA     LDA $CA                                     
EF40: 69 04     ADC #$04                                    
EF42: 85 CA     STA $CA                                     
EF44: 60        RTS                                         
;
;
L_EF45:
EF45: 38        SEC                                         
EF46: A5 C9     LDA EAL                                      ;Tape End Addresses/End of Program
EF48: E9 D8     SBC #$D8                                    
EF4A: 85 C9     STA EAL                                      ;Tape End Addresses/End of Program
EF4C: A5 CA     LDA $CA                                     
EF4E: E9 1B     SBC #$1B                                    
EF50: 85 CA     STA $CA                                     
EF52: 60        RTS                                         
;
;
JTTarget04:
EF53: EE F2 93  INC $93F2                                   
EF56: 29 07     AND #$07                                    
EF58: F0 08     BEQ L_EF62                                  
EF5A: 38        SEC                                         
EF5B: A5 CA     LDA $CA                                     
EF5D: E9 04     SBC #$04                                    
EF5F: 85 CA     STA $CA                                     
EF61: 60        RTS                                         
;
;
L_EF62:
EF62: 18        CLC                                         
EF63: A5 C9     LDA EAL                                      ;Tape End Addresses/End of Program
EF65: 69 D8     ADC #$D8                                    
EF67: 85 C9     STA EAL                                      ;Tape End Addresses/End of Program
EF69: A5 CA     LDA $CA                                     
EF6B: 69 1B     ADC #$1B                                    
EF6D: 85 CA     STA $CA                                     
EF6F: 60        RTS                                         
;
;
JTTarget02:
EF70: E6 C9     INC EAL                                      ;Tape End Addresses/End of Program
EF72: D0 02     BNE L_EF76                                  
EF74: E6 CA     INC $CA                                     
;
L_EF76:
EF76: 60        RTS                                         
;
;
JTTarget05:
EF77: EE EE 93  INC $93EE                                   
EF7A: D0 03     BNE L_EF7F                                  
EF7C: EE EF 93  INC $93EF                                   
;
L_EF7F:
EF7F: 60        RTS                                         
;
;
JTTarget01:
EF80: A5 C9     LDA EAL                                      ;Tape End Addresses/End of Program
EF82: D0 02     BNE L_EF86                                  
EF84: C6 CA     DEC $CA                                     
;
L_EF86:
EF86: C6 C9     DEC EAL                                      ;Tape End Addresses/End of Program
EF88: 60        RTS                                         
;
;
JTTarget06:
EF89: AD EE 93  LDA $93EE                                   
EF8C: D0 03     BNE L_EF91                                  
EF8E: CE EF 93  DEC $93EF                                   
;
L_EF91:
EF91: CE EE 93  DEC $93EE                                   
EF94: 60        RTS                                         
;
;
L_EF95:
EF95: A9 80     LDA #$80                                    
;
L_EF97:
EF97: 88        DEY                                         
EF98: F0 03     BEQ L_EF9D                                  
EF9A: 4A        LSR                                         
EF9B: 90 FA     BCC L_EF97                                  
;
L_EF9D:
EF9D: 60        RTS                                         
;
;
L_EF9E:
EF9E: A5 C9     LDA EAL                                      ;Tape End Addresses/End of Program
EFA0: 85 C0     STA PTR1                                     ;Tape Pass 1 Error Log
EFA2: A5 CA     LDA $CA                                     
EFA4: 85 C1     STA PTR2                                     ;Tape Pass 2 Error Log
EFA6: 60        RTS                                         
;
;
L_EFA7:
EFA7: 48        PHA                                         
EFA8: A5 C0     LDA PTR1                                     ;Tape Pass 1 Error Log
EFAA: 85 C9     STA EAL                                      ;Tape End Addresses/End of Program
EFAC: A5 C1     LDA PTR2                                     ;Tape Pass 2 Error Log
EFAE: 85 CA     STA $CA                                     
EFB0: 68        PLA                                         
EFB1: 60        RTS                                         
;
;
L_EFB2:
EFB2: B1 C9     LDA (EAL),Y                                  ;Tape End Addresses/End of Program
EFB4: 45 FB     EOR STAL                                     ;I/O Start Address
EFB6: 24 CC     BIT $CC                                     
EFB8: 60        RTS                                         
;
;================================================================================
; These appear to be offsets into the BITMAP space
;================================================================================
;
TABLE1:
EFB9:           !BYTE $90,$94,$98,$9C,$A0,$A4,$A8,$AC       ;unknown
 
;
L_EFC1:
EFC1:           !BYTE $EA,$EA,$EA,$EA,$EA,$EA               ;filler
 
;================================================================================
; JUMP TABLE
;================================================================================
EFC7: 4C 80 EF  JMP JTTarget01                              
EFCA: 4C 70 EF  JMP JTTarget02                              
EFCD: 4C 34 EF  JMP JTTarget03                              
EFD0: 4C 53 EF  JMP JTTarget04                              
EFD3: 4C 77 EF  JMP JTTarget05                              
EFD6: 4C 89 EF  JMP JTTarget06                              
EFD9: 4C C0 EE  JMP JTTarget07                              
EFDC: 4C CA EE  JMP JTTarget08                              
EFDF: 4C F4 EE  JMP JTTarget09                              
EFE2: 4C 18 E9  JMP JTTarget10                              
EFE5: 4C 72 EC  JMP JTTarget11                              
EFE8: 4C 2D EF  JMP JTTarget12                              
EFEB: 4C 01 EA  JMP JTTarget13                              
EFEE: 4C 2C EB  JMP JTTarget14                              
EFF1: 4C 93 ED  JMP JTTarget15                              
EFF4: 4C 7D EE  JMP JTTarget16                              
EFF7: 4C B8 EB  JMP JTTarget17                              
EFFA: 4C 14 EC  JMP JTTarget18                              
EFFD: 4C 9F EC  JMP JTTarget19                              
