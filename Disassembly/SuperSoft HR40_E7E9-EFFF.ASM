*=$E7E9
;
; ---- Code
;
;===================================================================================================
; This is located in an unused area at the end of the Editor ROM
;===================================================================================================
;
MOVEBYTES:
E7E9: A2 07     LDX #$07                                    
E7EB: A0 07     LDY #$07                                    
;
GETBYTE1:
E7ED: B1 B7     LDA (CHARPOINT),Y                            ;Zero Page Storage B7
E7EF: 9D F7 87  STA CHARSTORE,X                              ;Character Storage
E7F2: CA        DEX                                         
E7F3: 88        DEY                                         
E7F4: 10 F7     BPL GETBYTE1                                
E7F6: 24 CB     BIT YG                                       ;Zero Page Storage CB
E7F8: 10 03     BPL RETURNTEXT                              
E7FA: 4C D6 EC  JMP LATCHREPLACE                            
;
RETURNTEXT:
E7FD: 4C DE EC  JMP WRITETEXT                               
;===================================================================================================
; This area is normally hidden under the IO area from $E800-E8FF
;===================================================================================================
;
L_E800:
E800:           !TEXT "COPYRIGHT 1982 SUPERSOFT. 10-14 CANNING ROA" ;Hidden IO Area - Copyright
E82B:           !TEXT "D, WEALDSTONE, HARROW, MIDDLESEX. UNAUTHORI" ;Hidden IO Area - Copyright
E856:           !TEXT "SED REPRODUCTION IS STRICTLY PROHIBITED. SO" ;Hidden IO Area - Copyright
E881:           !TEXT "FTWARE WRITTEN BY NJ RICHMAN. HARWARE DESIG" ;Hidden IO Area - Copyright
E8AC:           !TEXT "NED AND MANUFACTURED BY MG BIGNELL AND M HU" ;Hidden IO Area - Copyright
E8D7:           !TEXT "MPHREY.SERIAL NUMBER OF THIS UNIT =414480" ;Hidden IO Area - Copyright
 
;===================================================================================================
; Start of Main Code - Upper 2K of Editor ROM Space
;===================================================================================================
E900: EA        NOP                                         
E901: EA        NOP                                         
;---------------------------------------------------------------------------------------------------
; Entry point for SYS commands 59650
;---------------------------------------------------------------------------------------------------
;
SYSENTRY:
E902: BA        TSX                                         
E903: 86 B4     STX STACKCOPY                                ;Zero Page Storage B4
E905: 20 F5 BE  JSR CHECKCOMMA                               ;Test for comma
E908: 84 96     STY STATUSWORD                               ;Kernal I/O Status Word: ST
E90A: A2 0D     LDX #$0D                                    
;
COMMANDLOOP:
E90C: DD 2A E9  CMP COMMAND,X                               
E90F: F0 10     BEQ COMMANDFOUND                            
E911: CA        DEX                                         
E912: 10 F8     BPL COMMANDLOOP                             
E914: 20 D4 C8  JSR GETNUMERIC                               ;Eval Byte Parameter
E917: 8A        TXA                                         
;---------------------------------------------------------------------------------------------------
; If the command was not found use the value for the Control Register
;---------------------------------------------------------------------------------------------------
;
SET_CONTROL:
E918: 29 0F     AND #$0F                                    
E91A: 8D FF EF  STA LATCH                                    ;Control Register
E91D: 8D 79 02  STA LATCHCOPY                                ;Copy of Control Latch contents
E920: 60        RTS                                         
;
;---------------------------------------------------------------------------------------------------
; Push the target address on the stack and jump to it using RTS
;---------------------------------------------------------------------------------------------------
;
COMMANDFOUND:
E921: BD 38 E9  LDA HICOMMAND,X                             
E924: 48        PHA                                         
E925: BD 46 E9  LDA LOCOMMAND,X                             
E928: 48        PHA                                         
E929: 60        RTS                                         
;
;---------------------------------------------------------------------------------------------------
; Command Table
;---------------------------------------------------------------------------------------------------
;
COMMAND:
E92A:           !TEXT "OKP",$A4,$8B,"TMECIRFDS"             ;Command Table
 
;---------------------------------------------------------------------------------------------------
; Command Vectors
;---------------------------------------------------------------------------------------------------
;
HICOMMAND:
E938:           !BYTE $E9,$E9,$E9,$EB,$EB,$EB,$ED,$EB       ;command vector HI
E940:           !BYTE $EE,$EE,$E9,$E9,$E9,$E9               ;command vector HI
 
;
LOCOMMAND:
E946:           !BYTE $53,$53,$71,$CA,$C4,$C7,$73,$A6       ;command vector LO
E94E:           !BYTE $6F,$72,$87,$8A,$8D,$84               ;command vector LO
 
;===================================================================================================
; "O" and "K" COMMANDS. OFF/KILL. Restore CHRGET routine.
;===================================================================================================
;
KILL:
E954: A2 17     LDX #$17                                    
;
L_E956:
E956:           !BYTE $2C                                   ;bit command
 
;
MOVELOOP:
E957: A2 06     LDX #$06                                    
;
MOVERTN:
E959: BD 99 D3  LDA ROUTINE,X                                ;CHRGET For Zero-page
E95C: 95 70     STA CHRGET,X                                 ;Subroutine: Get Next Byte of BASIC Text
E95E: E0 09     CPX #$09                                    
E960: F0 F5     BEQ MOVELOOP                                
E962: CA        DEX                                         
E963: 10 F4     BPL MOVERTN                                 
E965: 78        SEI                                         
E966: A9 55     LDA #$55                                    ; $E455 is default IRQ Vector
E968: A0 E4     LDY #$E4                                    
E96A: 85 90     STA CINV-LO                                  ;Vector: Hardware Interrupt LO
E96C: 84 91     STY CINV-HI                                  ;Vector: Hardware Interrupt HI
E96E: 58        CLI                                         
E96F: 4C 70 00  JMP CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
;===================================================================================================
; "P" COMMAND. Set Pointer to Character Set
;===================================================================================================
;
CHARSET:
E972: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
E975: 20 F5 BE  JSR CHECKCOMMA                               ;Test for comma
E978: 20 84 BD  JSR TESTTYPE                                 ;Confirm Result
E97B: 20 2D C9  JSR FLPINT                                   ;Convert FAC#1 to Integer in LINNUM
E97E: 8C F8 A7  STY CHARBLOCKSTART                           ;Character Pointer LO
E981: 8D F9 A7  STA CHARBLOCKSTART+1                         ;Character Pointer HI
E984: 60        RTS                                         
;
;===================================================================================================
; "S" COMMANDS. +"P"=Set Point. +"L"=Set Line
;===================================================================================================
;
SET:
E985: A9 00     LDA #$00                                    
;
RESET:
E987:           !BYTE $2C                                   ;bit command
 
;===================================================================================================
; "R" COMMAND. Reset Line.
;===================================================================================================
;
RESET2:
E988: A9 01     LDA #$01                                    
;
FLIP:
E98A:           !BYTE $2C                                   ;bit command
 
;===================================================================================================
; "F" COMMAND. +"L"=Flip Line. +"P"=Flip Point
;===================================================================================================
;
FLIP2:
E98B: A9 02     LDA #$02                                    
;
DOT2:
E98D:           !BYTE $2C                                   ;bit command
 
;===================================================================================================
; "D" COMMAND. Dot Line.
;===================================================================================================
;
DOT:
E98E: A9 03     LDA #$03                                    
E990: 20 F6 EE  JSR CHECKRAMON2                             
E993: A9 00     LDA #$00                                    
E995: 85 FF     STA FLAG                                     ;Zero Page Storage FF
E997: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
E99A: C9 4C     CMP #$4C                                    ; Check second character. Is it "L"? L=Line.
E99C: F0 06     BEQ INPUT                                   
E99E: C6 FF     DEC FLAG                                     ;Zero Page Storage FF
E9A0: C9 50     CMP #$50                                    
E9A2: D0 2F     BNE SYNTAX                                  
;
INPUT:
E9A4: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
E9A7: 20 F5 BE  JSR CHECKCOMMA                               ;Test for comma
E9AA: 20 3D EC  JSR NUMBERIN3                               
E9AD: F0 27     BEQ ONESET                                  
E9AF: 24 FF     BIT FLAG                                     ;Zero Page Storage FF
E9B1: 30 20     BMI SYNTAX                                  
;
CHECK1:
E9B3: 20 BD EE  JSR JUST2                                   
E9B6: 24 FF     BIT FLAG                                     ;Zero Page Storage FF
E9B8: 30 35     BMI SETPOINT                                
E9BA: 20 3A EC  JSR NUMBERIN                                
;
SECOND:
E9BD: 20 D4 EE  JSR CHECKY                                  
E9C0: 20 CA EE  JSR STOREND                                 
E9C3: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
E9C6: 4D F2 93  EOR YPOINT                                   ;Storage at $93F2
E9C9: 29 01     AND #$01                                    
E9CB: D0 34     BNE START                                   
E9CD: A9 01     LDA #$01                                    
E9CF: 85 FF     STA FLAG                                     ;Zero Page Storage FF
E9D1: D0 2E     BNE START                                   
;
SYNTAX:
E9D3: 4C 00 BF  JMP SYNERR                                   ;Output ?SYNTAX Error
;
ONESET:
E9D6: 24 FF     BIT FLAG                                     ;Zero Page Storage FF
E9D8: 30 D9     BMI CHECK1                                  
E9DA: 20 D4 EE  JSR CHECKY                                  
E9DD: AD F1 93  LDA YEND                                     ;Storage at $93F1
E9E0: 8D F2 93  STA YPOINT                                   ;Storage at $93F2
E9E3: AD EC 93  LDA XEND                                     ;Storage at $93EC
E9E6: AC ED 93  LDY XEND+1                                   ;Storage at $93ED
E9E9: 20 C3 EE  JSR L_EEC3                                  
E9EC: 4C BD E9  JMP SECOND                                  
;
SETPOINT:
E9EF: 20 CA EE  JSR STOREND                                 
E9F2: 20 C0 EE  JSR STOREPOINT                              
E9F5: 4D F1 93  EOR YEND                                     ;Storage at $93F1
E9F8: 29 01     AND #$01                                    
E9FA: F0 02     BEQ PLOTJMP                                 
E9FC: E6 FF     INC FLAG                                     ;Zero Page Storage FF
;
PLOTJMP:
E9FE: 4C 2C EB  JMP PLOT1                                   
;
START:
EA01: A5 FF     LDA FLAG                                     ;Zero Page Storage FF
EA03: 29 01     AND #$01                                    
EA05: 85 FF     STA FLAG                                     ;Zero Page Storage FF
EA07: 38        SEC                                         
EA08: AD EC 93  LDA XEND                                     ;Storage at $93EC
EA0B: ED EE 93  SBC XPOINT                                   ;Storage at $93EE
EA0E: 85 C0     STA XG                                       ;Zero Page Storage C0
EA10: AD ED 93  LDA XEND+1                                   ;Storage at $93ED
EA13: ED EF 93  SBC XPOINT+1                                 ;Storage at $93EF
EA16: 85 C1     STA XG+1                                     ;Zero Page Storage C1
EA18: B0 16     BCS YTEST                                   
EA1A: 49 FF     EOR #$FF                                    
EA1C: 85 C1     STA XG+1                                     ;Zero Page Storage C1
EA1E: A5 C0     LDA XG                                       ;Zero Page Storage C0
EA20: 49 FF     EOR #$FF                                    
EA22: 69 01     ADC #$01                                    
EA24: 85 C0     STA XG                                       ;Zero Page Storage C0
EA26: D0 02     BNE YTESTA                                  
EA28: E6 C1     INC XG+1                                     ;Zero Page Storage C1
;
YTESTA:
EA2A: A5 FF     LDA FLAG                                     ;Zero Page Storage FF
EA2C: 09 40     ORA #$40                                    
EA2E: 85 FF     STA FLAG                                     ;Zero Page Storage FF
;
YTEST:
EA30: 38        SEC                                         
EA31: AD F1 93  LDA YEND                                     ;Storage at $93F1
EA34: ED F2 93  SBC YPOINT                                   ;Storage at $93F2
EA37: 85 CB     STA YG                                       ;Zero Page Storage CB
EA39: B0 0C     BCS GRAD0                                   
EA3B: 49 FF     EOR #$FF                                    
EA3D: 69 01     ADC #$01                                    
EA3F: 85 CB     STA YG                                       ;Zero Page Storage CB
EA41: A5 FF     LDA FLAG                                     ;Zero Page Storage FF
EA43: 09 80     ORA #$80                                    
EA45: 85 FF     STA FLAG                                     ;Zero Page Storage FF
;
GRAD0:
EA47: A5 CB     LDA YG                                       ;Zero Page Storage CB
EA49: F0 10     BEQ PLOTORIZ                                
EA4B: A5 C1     LDA XG+1                                     ;Zero Page Storage C1
EA4D: D0 33     BNE TILTX                                   
EA4F: A5 C0     LDA XG                                       ;Zero Page Storage C0
EA51: F0 14     BEQ PLOTVERT                                
EA53: C5 CB     CMP YG                                       ;Zero Page Storage CB
EA55: F0 1C     BEQ PLOT45                                  
EA57: B0 29     BCS TILTX                                   
EA59: 90 63     BCC TILTY                                   
;
PLOTORIZ:
EA5B: 20 2C EB  JSR PLOT1                                   
;
TESTXH:
EA5E: 20 F6 EA  JSR XCOMPARE                                
EA61: 20 78 EB  JSR PLOT2                                   
EA64: 4C 5E EA  JMP TESTXH                                  
;
PLOTVERT:
EA67: 20 2C EB  JSR PLOT1                                   
;
TESTYV:
EA6A: 20 19 EB  JSR YCOMPARE                                
EA6D: 20 78 EB  JSR PLOT2                                   
EA70: 4C 6A EA  JMP TESTYV                                  
;
PLOT45:
EA73: 20 2C EB  JSR PLOT1                                   
;
TESTXY:
EA76: 20 19 EB  JSR YCOMPARE                                
EA79: 20 F9 EA  JSR ALTERX                                  
EA7C: 20 78 EB  JSR PLOT2                                   
EA7F: 4C 76 EA  JMP TESTXY                                  
;
TILTX:
EA82: A5 C1     LDA XG+1                                     ;Zero Page Storage C1
EA84: 4A        LSR                                         
EA85: 85 B3     STA TWOBYTE+1                                ;Zero Page Storage B3
EA87: A5 C0     LDA XG                                       ;Zero Page Storage C0
EA89: 6A        ROR                                         
EA8A: 85 B2     STA TWOBYTE                                  ;Zero Page Storage B2
;
PLOTX:
EA8C: 20 2C EB  JSR PLOT1                                   
;
TESTXX:
EA8F: 20 F6 EA  JSR XCOMPARE                                
EA92: 18        CLC                                         
EA93: A5 B2     LDA TWOBYTE                                  ;Zero Page Storage B2
EA95: 65 CB     ADC YG                                       ;Zero Page Storage CB
EA97: 85 B2     STA TWOBYTE                                  ;Zero Page Storage B2
EA99: 90 02     BCC NOINCTB1                                
EA9B: E6 B3     INC TWOBYTE+1                                ;Zero Page Storage B3
;
NOINCTB1:
EA9D: 38        SEC                                         
EA9E: A5 B2     LDA TWOBYTE                                  ;Zero Page Storage B2
EAA0: E5 C0     SBC XG                                       ;Zero Page Storage C0
EAA2: A5 B3     LDA TWOBYTE+1                                ;Zero Page Storage B3
EAA4: E5 C1     SBC XG+1                                     ;Zero Page Storage C1
EAA6: 90 10     BCC PLOTX2                                  
EAA8: 20 1C EB  JSR ALTERY                                  
EAAB: 38        SEC                                         
EAAC: A5 B2     LDA TWOBYTE                                  ;Zero Page Storage B2
EAAE: E5 C0     SBC XG                                       ;Zero Page Storage C0
EAB0: 85 B2     STA TWOBYTE                                  ;Zero Page Storage B2
EAB2: A5 B3     LDA TWOBYTE+1                                ;Zero Page Storage B3
EAB4: E5 C1     SBC XG+1                                     ;Zero Page Storage C1
EAB6: 85 B3     STA TWOBYTE+1                                ;Zero Page Storage B3
;
PLOTX2:
EAB8: 20 78 EB  JSR PLOT2                                   
EABB: 4C 8F EA  JMP TESTXX                                  
;
TILTY:
EABE: A5 CB     LDA YG                                       ;Zero Page Storage CB
EAC0: 4A        LSR                                         
EAC1: 85 B2     STA TWOBYTE                                  ;Zero Page Storage B2
EAC3: A9 00     LDA #$00                                    
EAC5: 85 B3     STA TWOBYTE+1                                ;Zero Page Storage B3
;
PLOTY:
EAC7: 20 2C EB  JSR PLOT1                                   
;
TESTYY:
EACA: 20 19 EB  JSR YCOMPARE                                
EACD: 18        CLC                                         
EACE: A5 B2     LDA TWOBYTE                                  ;Zero Page Storage B2
EAD0: 65 C0     ADC XG                                       ;Zero Page Storage C0
EAD2: 85 B2     STA TWOBYTE                                  ;Zero Page Storage B2
EAD4: A5 B3     LDA TWOBYTE+1                                ;Zero Page Storage B3
EAD6: 65 C1     ADC XG+1                                     ;Zero Page Storage C1
EAD8: 85 B3     STA TWOBYTE+1                                ;Zero Page Storage B3
EADA: D0 06     BNE INCXPLOT                                
EADC: A5 B2     LDA TWOBYTE                                  ;Zero Page Storage B2
EADE: C5 CB     CMP YG                                       ;Zero Page Storage CB
EAE0: 90 0E     BCC PLOTY2                                  
;
INCXPLOT:
EAE2: 20 F9 EA  JSR ALTERX                                  
EAE5: 38        SEC                                         
EAE6: A5 B2     LDA TWOBYTE                                  ;Zero Page Storage B2
EAE8: E5 CB     SBC YG                                       ;Zero Page Storage CB
;
TraceEAEA:
EAEA: 85 B2     STA TWOBYTE                                  ;Zero Page Storage B2
EAEC: B0 02     BCS PLOTY2                                  
EAEE: C6 B3     DEC TWOBYTE+1                                ;Zero Page Storage B3
;
PLOTY2:
EAF0: 20 78 EB  JSR PLOT2                                   
EAF3: 4C CA EA  JMP TESTYY                                  
;
XCOMPARE:
EAF6: 20 1C EF  JSR CMPXEND                                 
;
ALTERX:
EAF9: 24 FF     BIT FLAG                                     ;Zero Page Storage FF
EAFB: 70 0E     BVS DECX                                    
EAFD: 46 CC     LSR MASKONE                                  ;Zero Page Storage CC
EAFF: 90 07     BCC INCX1                                   
EB01: A9 80     LDA #$80                                    
EB03: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
EB05: 20 70 EF  JSR UPBYTEX                                 
;
INCX1:
EB08: 4C 77 EF  JMP UPPOINTX                                
;
DECX:
EB0B: 06 CC     ASL MASKONE                                  ;Zero Page Storage CC
EB0D: 90 07     BCC DECX0                                   
EB0F: A9 01     LDA #$01                                    
EB11: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
EB13: 20 80 EF  JSR DOWNBYTEX                               
;
DECX0:
EB16: 4C 89 EF  JMP DOWNPOINTX                              
;
YCOMPARE:
EB19: 20 13 EF  JSR CMPYEND                                 
;
ALTERY:
EB1C: 24 FF     BIT FLAG                                     ;Zero Page Storage FF
EB1E: 30 06     BMI DECY                                    
EB20: 20 2D EF  JSR COMPAREY                                
EB23: 4C 53 EF  JMP UPBYTEY                                 
;
DECY:
EB26: 20 2D EF  JSR COMPAREY                                
EB29: 4C 34 EF  JMP DOWNBYTEY                               
;
PLOT1:
EB2C: AD EF 93  LDA XPOINT+1                                 ;Storage at $93EF
EB2F: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EB31: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EB34: 48        PHA                                         
EB35: 46 CA     LSR BYTE+1                                   ;Tape End Addresses/End of Program
EB37: 6A        ROR                                         
EB38: 4A        LSR                                         
EB39: 4A        LSR                                         
EB3A: 85 C9     STA BYTE                                     ;Tape End Addresses/End of Program
EB3C: 20 2D EF  JSR COMPAREY                                
EB3F: 48        PHA                                         
EB40: 4A        LSR                                         
EB41: 4A        LSR                                         
EB42: 4A        LSR                                         
EB43: A8        TAY                                         
EB44: B9 98 E7  LDA LOWTABLE,Y                              ; $E798 is the TEXT SCREEN Line Address Table LO
EB47: 48        PHA                                         
EB48: A2 03     LDX #$03                                    
EB4A: C0 14     CPY #$14                                    
EB4C: B0 0B     BCS FOUNDY                                  
EB4E: CA        DEX                                         
EB4F: C0 0D     CPY #$0D                                    
EB51: B0 06     BCS FOUNDY                                  
EB53: CA        DEX                                         
EB54: C0 07     CPY #$07                                    
EB56: B0 01     BCS FOUNDY                                  
EB58: CA        DEX                                         
;
FOUNDY:
EB59: 68        PLA                                         
EB5A: 20 08 EF  JSR ADDBYTE                                 
EB5D: 68        PLA                                         
EB5E: 29 07     AND #$07                                    
EB60: A8        TAY                                         
EB61: B9 B9 EF  LDA HITABLE,Y                               
EB64: 18        CLC                                         
EB65: 20 0E EF  JSR ADDHIBYTE                               
EB68: 68        PLA                                         
EB69: 29 07     AND #$07                                    
EB6B: A8        TAY                                         
EB6C: C8        INY                                         
EB6D: A5 BB     LDA ZP_BB                                    ;Zero Page Storage BB
EB6F: C9 04     CMP #$04                                    
EB71: F0 17     BEQ RETURN1                                 
EB73: 20 95 EF  JSR LSRMASK                                 
;
SETBYTE:
EB76: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
;
PLOT2:
EB78: A5 CC     LDA MASKONE                                  ;Zero Page Storage CC
EB7A: A6 BB     LDX ZP_BB                                    ;Zero Page Storage BB
EB7C: F0 0D     BEQ SETLINE                                 
EB7E: E0 01     CPX #$01                                    
EB80: F0 20     BEQ RESETLINE                               
EB82: E0 03     CPX #$03                                    
EB84: F0 0C     BEQ DOTLINE                                 
;
FLIPLINE:
EB86: 51 C9     EOR (BYTE),Y                                 ;Tape End Addresses/End of Program
EB88: 91 C9     STA (BYTE),Y                                 ;Tape End Addresses/End of Program
;
RETURN1:
EB8A: 60        RTS                                         
;
;
SETLINE:
EB8B: 49 FF     EOR #$FF                                    
EB8D: 31 C9     AND (BYTE),Y                                 ;Tape End Addresses/End of Program
EB8F: 91 C9     STA (BYTE),Y                                 ;Tape End Addresses/End of Program
EB91: 60        RTS                                         
;
;
DOTLINE:
EB92: 48        PHA                                         
EB93: A5 FF     LDA FLAG                                     ;Zero Page Storage FF
EB95: 49 01     EOR #$01                                    
EB97: 85 FF     STA FLAG                                     ;Zero Page Storage FF
EB99: 68        PLA                                         
EB9A: 66 FF     ROR FLAG                                     ;Zero Page Storage FF
EB9C: 08        PHP                                         
EB9D: 26 FF     ROL FLAG                                     ;Zero Page Storage FF
EB9F: 28        PLP                                         
EBA0: 90 E9     BCC SETLINE                                 
;
RESETLINE:
EBA2: 11 C9     ORA (BYTE),Y                                 ;Tape End Addresses/End of Program
EBA4: 91 C9     STA (BYTE),Y                                 ;Tape End Addresses/End of Program
EBA6: 60        RTS                                         
;
;===================================================================================================
; "E" COMMAND. Examine.
;===================================================================================================
;
CMD_E:
EBA7: 20 F4 EE  JSR CHECKRAMON                              
EBAA: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBAD: 20 3A EC  JSR NUMBERIN                                
EBB0: D0 3C     BNE SYNTAX18K                               
EBB2: 20 BD EE  JSR JUST2                                   
EBB5: 20 CA EE  JSR STOREND                                 
;
EXAMINEIN:
EBB8: 20 2C EB  JSR PLOT1                                   
EBBB: 20 95 EF  JSR LSRMASK                                 
;
OUTMASK1:
EBBE: 31 C9     AND (BYTE),Y                                 ;Tape End Addresses/End of Program
EBC0: D0 02     BNE NOTSET                                  
EBC2: E6 96     INC STATUSWORD                               ;Kernal I/O Status Word: ST
;
NOTSET:
EBC4: 60        RTS                                         
;
;===================================================================================================
; $8B COMMAND (RVS K). FLIP Text
;===================================================================================================
;
FLIPTEXT:
EBC5: A9 00     LDA #$00                                    
;
TEXT:
EBC7:           !BYTE $2C                                   ;bit command
 
;===================================================================================================
; "T" COMMAND. Text. +"NH"=Normal Horizontal
;===================================================================================================
;
CMD_T:
EBC8: A9 FF     LDA #$FF                                    
;
L_EBCA:
EBCA:           !BYTE $2C                                   ;bit command
 
;===================================================================================================
; $A4 COMMAND (RVS $). TEXT Routine
;===================================================================================================
;
OVERLAY:
EBCB: A9 40     LDA #$40                                    
EBCD: 85 BE     STA TEXTYPE                                  ;Zero Page Storage BE
EBCF: 20 F4 EE  JSR CHECKRAMON                              
EBD2: A9 FF     LDA #$FF                                    
EBD4: 85 B1     STA ONEBYTE                                  ;Zero Page Storage B1
EBD6: 85 BD     STA PLOTDIR                                  ;Zero Page Storage BD
EBD8: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBDB: C9 52     CMP #$52                                    ; Is it "R"?
EBDD: F0 06     BEQ IPBYTE                                  
EBDF: C9 4E     CMP #$4E                                    ; Is it "N"?
EBE1: D0 0B     BNE SYNTAX18K                               
EBE3: E6 B1     INC ONEBYTE                                  ;Zero Page Storage B1
;
IPBYTE:
EBE5: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBE8: C9 48     CMP #$48                                    ; Is it "H"?
EBEA: F0 06     BEQ IPBYTE1                                 
EBEC: C9 56     CMP #$56                                    ; Is it "V"?
;
SYNTAX18K:
EBEE: D0 74     BNE SYNTAX1                                 
;
SAVECHAR:
EBF0: E6 BD     INC PLOTDIR                                  ;Zero Page Storage BD
;
IPBYTE1:
EBF2: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBF5: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EBF8: 20 98 BD  JSR FRMEVL                                   ;Evaluate Expression in Text
EBFB: 24 07     BIT VAUYP                                    ;Data Type: $FF = String
EBFD: 10 57     BPL NUMERIC                                 
EBFF: 20 B8 C7  JSR $C7B8                                   
EC02: C9 00     CMP #$00                                    
EC04: F0 2F     BEQ NUMBERIN1                               
EC06: 85 B2     STA TWOBYTE                                  ;Zero Page Storage B2
EC08: 86 32     STX STRINGPTR                                ;Utility String Pointer
EC0A: 84 33     STY ZP_33                                    ;Zero Page Storage 33
EC0C: 20 76 00  JSR CHRGOT                                   ;Entry to Get Same Byte of Text Again
EC0F: F0 05     BEQ CONTEXT1                                
EC11: 20 BA EE  JSR ALL3                                    
;
CONTEXT:
EC14: C6 96     DEC STATUSWORD                               ;Kernal I/O Status Word: ST
;
CONTEXT1:
EC16: 20 46 ED  JSR GETSTRCHAR                              
EC19: 90 0E     BCC NEXTEXT                                 
EC1B: 24 96     BIT STATUSWORD                               ;Kernal I/O Status Word: ST
EC1D: 10 07     BPL UPTEXT                                  
EC1F: 20 9F EC  JSR BACKIN2                                 
EC22: E6 96     INC STATUSWORD                               ;Kernal I/O Status Word: ST
EC24: F0 03     BEQ NEXTEXT                                 
;
UPTEXT:
EC26: 20 72 EC  JSR CONTLINE                                
;
NEXTEXT:
EC29: C6 B2     DEC TWOBYTE                                  ;Zero Page Storage B2
;
NOTSKIP:
EC2B: F0 97     BEQ NOTSET                                  
EC2D: E6 32     INC STRINGPTR                                ;Utility String Pointer
EC2F: D0 E5     BNE CONTEXT1                                
EC31: E6 33     INC ZP_33                                    ;Zero Page Storage 33
EC33: D0 E1     BNE CONTEXT1                                
;
NUMBERIN1:
EC35: A2 FD     LDX #$FD                                    
EC37: 4C DA EE  JMP ERROREXIT                               
;
NUMBERIN:
EC3A: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
;
NUMBERIN3:
EC3D: 20 B1 EE  JSR L_EEB1                                  
EC40: A5 61     LDA ZP_61                                    ;Zero Page Storage 61
EC42: A4 62     LDY ZP_62                                    ;Zero Page Storage 62
EC44: 85 12     STA ZP_12                                    ;Zero Page Storage 12
EC46: 84 11     STY LINNUM                                   ;Temp: Integer Value
EC48: 20 AE EE  JSR GETINT                                  
EC4B: A6 62     LDX ZP_62                                    ;Zero Page Storage 62
EC4D: A5 61     LDA ZP_61                                    ;Zero Page Storage 61
EC4F: F0 02     BEQ YOK                                     
;
TraceEC51:
EC51: A2 FF     LDX #$FF                                    
;
YOK:
EC53: 4C 76 00  JMP CHRGOT                                   ;Entry to Get Same Byte of Text Again
;
NUMERIC:
EC56: 20 87 BD  JSR $BD87                                   
EC59: 20 D7 C8  JSR $C8D7                                   
EC5C: 86 BC     STX CHARNUMBER                               ;Zero Page Storage BC
EC5E: F0 12     BEQ CONTLINE                                
EC60: C9 2C     CMP #$2C                                    ; Is it COMMA?
EC62: F0 35     BEQ CONT1                                   
;
SYNTAX1:
EC64: 4C 00 BF  JMP SYNERR                                   ;Output ?SYNTAX Error
;
VERTICAL:
EC67: AC F2 93  LDY YPOINT                                   ;Storage at $93F2
EC6A: F0 BF     BEQ NOTSKIP                                 
EC6C: CE F2 93  DEC YPOINT                                   ;Storage at $93F2
EC6F: 4C 9F EC  JMP BACKIN2                                 
;
CONTLINE:
EC72: A5 BD     LDA PLOTDIR                                  ;Zero Page Storage BD
EC74: F0 F1     BEQ VERTICAL                                
;
HORIZ:
EC76: 18        CLC                                         
EC77: AD F2 93  LDA YPOINT                                   ;Storage at $93F2
EC7A: 69 07     ADC #$07                                    
EC7C: 8D F2 93  STA YPOINT                                   ;Storage at $93F2
EC7F: 18        CLC                                         
EC80: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EC83: AC EF 93  LDY XPOINT+1                                 ;Storage at $93EF
EC86: 69 08     ADC #$08                                    
EC88: 90 01     BCC SKIPY                                   
EC8A: C8        INY                                         
;
SKIPY:
EC8B: C0 00     CPY #$00                                    
EC8D: F0 0D     BEQ BACKIN                                  
EC8F: C0 01     CPY #$01                                    
EC91: D0 A2     BNE NUMBERIN1                               
EC93: C9 39     CMP #$39                                    
EC95: B0 9E     BCS NUMBERIN1                               
EC97: 90 03     BCC BACKIN                                  
;
CONT1:
EC99: 20 BA EE  JSR ALL3                                    
;
BACKIN:
EC9C: 20 C3 EE  JSR L_EEC3                                  
;
BACKIN2:
EC9F: A9 00     LDA #$00                                    
ECA1: 85 B8     STA ZP_B8                                    ;Zero Page Storage B8
ECA3: A5 BC     LDA CHARNUMBER                               ;Zero Page Storage BC
ECA5: 0A        ASL                                         
ECA6: 26 B8     ROL ZP_B8                                    ;Zero Page Storage B8
ECA8: 0A        ASL                                         
ECA9: 26 B8     ROL ZP_B8                                    ;Zero Page Storage B8
ECAB: 0A        ASL                                         
ECAC: 26 B8     ROL ZP_B8                                    ;Zero Page Storage B8
ECAE: 18        CLC                                         
ECAF: 6D F8 A7  ADC CHARBLOCKSTART                           ;Character Pointer LO
ECB2: 85 B7     STA CHARPOINT                                ;Zero Page Storage B7
ECB4: A5 B8     LDA ZP_B8                                    ;Zero Page Storage B8
ECB6: 6D F9 A7  ADC CHARBLOCKSTART+1                         ;Character Pointer HI
ECB9: 85 B8     STA ZP_B8                                    ;Zero Page Storage B8
ECBB: A0 00     LDY #$00                                    
ECBD: 84 CB     STY YG                                       ;Zero Page Storage CB
ECBF: 2C F9 A7  BIT CHARBLOCKSTART+1                         ;Character Pointer HI
ECC2: 10 0F     BPL SETXY                                   
ECC4: C6 CB     DEC YG                                       ;Zero Page Storage CB
;
RETRACE:
ECC6: BA        TSX                                         
ECC7: 98        TYA                                         
ECC8: 9D FE 00  STA ZP_FE,X                                  ;Zero Page Storage FE (note use of ABSOLUTE addressing mode!)
;
STACKLOOP:
ECCB: BD FE 00  LDA ZP_FE,X                                  ;Zero Page Storage FE (note use of ABSOLUTE addressing mode!)
ECCE: F0 FB     BEQ STACKLOOP                               
ECD0: 8C FF EF  STY LATCH                                    ;Control Register
;
SETXY:
ECD3: 4C E9 E7  JMP MOVEBYTES                               ; Jump to patch area in Editor ROM area
;
LATCHREPLACE:
ECD6: AD 79 02  LDA LATCHCOPY                                ;Copy of Control Latch contents
ECD9: 09 01     ORA #$01                                    
ECDB: 8D FF EF  STA LATCH                                    ;Control Register
;
WRITETEXT:
ECDE: 20 2C EB  JSR PLOT1                                   
ECE1: 84 FF     STY FLAG                                     ;Zero Page Storage FF
ECE3: A9 00     LDA #$00                                    
ECE5: AA        TAX                                         
;
LOOPDEC1:
ECE6: 88        DEY                                         
ECE7: F0 04     BEQ MASK1                                   
ECE9: 38        SEC                                         
ECEA: 6A        ROR                                         
ECEB: 90 F9     BCC LOOPDEC1                                
;
MASK1:
ECED: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
;
LOOPCHAR:
ECEF: B1 C9     LDA (BYTE),Y                                 ;Tape End Addresses/End of Program
ECF1: 49 FF     EOR #$FF                                    
ECF3: 24 BE     BIT TEXTYPE                                  ;Zero Page Storage BE
ECF5: 10 02     BPL STOREBYTE1                              
ECF7: 25 CC     AND MASKONE                                  ;Zero Page Storage CC
;
STOREBYTE1:
ECF9: 85 B3     STA TWOBYTE+1                                ;Zero Page Storage B3
ECFB: 84 FB     STY CHAR2                                    ;Zero Page Storage FB
ECFD: BD F7 87  LDA CHARSTORE,X                              ;Character Storage
ED00: 45 B1     EOR ONEBYTE                                  ;Zero Page Storage B1
ED02: A4 FF     LDY FLAG                                     ;Zero Page Storage FF
;
LOOPDEC2:
ED04: 88        DEY                                         
ED05: F0 0E     BEQ MASK2                                   
ED07: 4A        LSR                                         
ED08: 66 FB     ROR CHAR2                                    ;Zero Page Storage FB
ED0A: 90 F8     BCC LOOPDEC2                                
ED0C: EA        NOP                                         
;
FLIPTEXT1:
ED0D: 45 B3     EOR TWOBYTE+1                                ;Zero Page Storage B3
ED0F: 50 0A     BVC BITS                                    
;
FLIPTEXT2:
ED11: 45 FB     EOR CHAR2                                    ;Zero Page Storage FB
ED13: 50 19     BVC INVERT2                                 
;
MASK2:
ED15: 24 BE     BIT TEXTYPE                                  ;Zero Page Storage BE
ED17: 50 F4     BVC FLIPTEXT1                               
ED19: 05 B3     ORA TWOBYTE+1                                ;Zero Page Storage B3
;
BITS:
ED1B: 49 FF     EOR #$FF                                    
ED1D: 91 C9     STA (BYTE),Y                                 ;Tape End Addresses/End of Program
ED1F: C8        INY                                         
ED20: B1 C9     LDA (BYTE),Y                                 ;Tape End Addresses/End of Program
ED22: 24 BE     BIT TEXTYPE                                  ;Zero Page Storage BE
ED24: 10 02     BPL INVERTBYTE                              
ED26: 05 CC     ORA MASKONE                                  ;Zero Page Storage CC
;
INVERTBYTE:
ED28: 49 FF     EOR #$FF                                    
ED2A: 50 E5     BVC FLIPTEXT2                               
ED2C: 05 FB     ORA CHAR2                                    ;Zero Page Storage FB
;
INVERT2:
ED2E: 49 FF     EOR #$FF                                    
ED30: 91 C9     STA (BYTE),Y                                 ;Tape End Addresses/End of Program
ED32: 88        DEY                                         
ED33: E8        INX                                         
ED34: E0 08     CPX #$08                                    
ED36: D0 02     BNE NEXTLINE                                
;
END1:
ED38: 60        RTS                                         
;
ED39: EA        NOP                                         
;
NEXTLINE:
ED3A: 20 2D EF  JSR COMPAREY                                
ED3D: C9 C7     CMP #$C7                                    
ED3F: F0 F7     BEQ END1                                    
ED41: 20 34 EF  JSR DOWNBYTEY                               
;
LOOPBACK:
ED44: D0 A9     BNE LOOPCHAR                                
;
GETSTRCHAR:
ED46: A0 00     LDY #$00                                    
ED48: B1 32     LDA (STRINGPTR),Y                            ;Utility String Pointer
ED4A: 29 7F     AND #$7F                                    
ED4C: C9 12     CMP #$12                                    
ED4E: D0 08     BNE CONCONT                                 
;
RVSFLIP:
ED50: A9 FF     LDA #$FF                                    
ED52: 45 B1     EOR ONEBYTE                                  ;Zero Page Storage B1
ED54: 85 B1     STA ONEBYTE                                  ;Zero Page Storage B1
ED56: 18        CLC                                         
;
FLIPBACK:
ED57: 60        RTS                                         
;
;
CONCONT:
ED58: C9 20     CMP #$20                                    
ED5A: 90 FB     BCC FLIPBACK                                
ED5C: B1 32     LDA (STRINGPTR),Y                            ;Utility String Pointer
ED5E: C9 40     CMP #$40                                    
ED60: 90 0E     BCC STORE                                   
ED62: C9 C0     CMP #$C0                                    
ED64: 90 08     BCC SUBTRACT40                              
ED66: C9 FF     CMP #$FF                                    
ED68: 90 02     BCC SUBTRACT80                              
ED6A: E9 22     SBC #$22                                    
;
SUBTRACT80:
ED6C: E9 40     SBC #$40                                    
;
SUBTRACT40:
ED6E: E9 3F     SBC #$3F                                    
;
STORE:
ED70: 85 BC     STA CHARNUMBER                               ;Zero Page Storage BC
ED72: 38        SEC                                         
ED73: 60        RTS                                         
;
;===================================================================================================
; "M" COMMAND. Map Erase, Map Fill.
;===================================================================================================
;
MAP:
ED74: 20 F4 EE  JSR CHECKRAMON                              
ED77: A9 FF     LDA #$FF                                    
ED79: 85 FB     STA CHAR2                                    ;Zero Page Storage FB
ED7B: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
ED7E: C9 46     CMP #$46                                    
ED80: F0 09     BEQ MAPIN                                   
ED82: C9 45     CMP #$45                                    
ED84: F0 03     BEQ RESMAP                                  
ED86: 4C 00 BF  JMP SYNERR                                   ;Output ?SYNTAX Error
;
RESMAP:
ED89: E6 FB     INC CHAR2                                    ;Zero Page Storage FB
;
MAPIN:
ED8B: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
ED8E: F0 03     BEQ MAPOLDSET                               
ED90: 20 BA EE  JSR ALL3                                    
;
MAPOLDSET:
ED93: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
ED96: AC EF 93  LDY XPOINT+1                                 ;Storage at $93EF
ED99: 85 CE     STA ZP_CE                                    ;Zero Page Storage CE
ED9B: 84 CF     STY ZP_CF                                    ;Zero Page Storage CF
ED9D: 20 2C EB  JSR PLOT1                                   
EDA0: 20 9E EF  JSR BYTETOCOPY                              
EDA3: 20 95 EF  JSR LSRMASK                                 
;
TESTBIT1:
EDA6: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
EDA8: 85 FF     STA FLAG                                     ;Zero Page Storage FF
;
TESTBIT2:
EDAA: 20 B2 EF  JSR EORMAP                                  
EDAD: D0 12     BNE NEXTLINECHECK                           
EDAF: 20 2D EF  JSR COMPAREY                                
EDB2: C9 C7     CMP #$C7                                    
EDB4: F0 32     BEQ LOOPMASK1A                              
;
DOWNY:
EDB6: 20 A7 EF  JSR COPYTOBYTE                              
EDB9: 20 34 EF  JSR DOWNBYTEY                               
EDBC: 20 9E EF  JSR BYTETOCOPY                              
EDBF: D0 E9     BNE TESTBIT2                                
;
NEXTLINECHECK:
EDC1: A5 CE     LDA ZP_CE                                    ;Zero Page Storage CE
EDC3: 8D EE 93  STA XPOINT                                   ;Storage at $93EE
EDC6: A5 CF     LDA ZP_CF                                    ;Zero Page Storage CF
EDC8: 8D EF 93  STA XPOINT+1                                 ;Storage at $93EF
EDCB: 20 2D EF  JSR COMPAREY                                
EDCE: F0 12     BEQ RETFILL                                 
EDD0: 20 A7 EF  JSR COPYTOBYTE                              
EDD3: 20 53 EF  JSR UPBYTEY                                 
EDD6: 20 9E EF  JSR BYTETOCOPY                              
;
SECONDCHECK:
EDD9: A5 FF     LDA FLAG                                     ;Zero Page Storage FF
EDDB: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
EDDD: 20 B2 EF  JSR EORMAP                                  
EDE0: F0 06     BEQ LOOPMASK1A                              
;
RETFILL:
EDE2: 60        RTS                                         
;
;
LOOPMASK1:
EDE3: 20 B2 EF  JSR EORMAP                                  
EDE6: D0 56     BNE L_EE3E                                  
;
LOOPMASK1A:
EDE8: AD EF 93  LDA XPOINT+1                                 ;Storage at $93EF
EDEB: D0 05     BNE DECX1                                   
EDED: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EDF0: F0 35     BEQ LHWALL                                  
;
DECX1:
EDF2: 20 89 EF  JSR DOWNPOINTX                              
EDF5: 06 CC     ASL MASKONE                                  ;Zero Page Storage CC
EDF7: 90 EA     BCC LOOPMASK1                               
;
LEFTLOOP:
EDF9: 20 80 EF  JSR DOWNBYTEX                               
EDFC: B1 C9     LDA (BYTE),Y                                 ;Tape End Addresses/End of Program
EDFE: C5 FB     CMP CHAR2                                    ;Zero Page Storage FB
EE00: D0 1C     BNE MASKOUT                                 
EE02: AD EF 93  LDA XPOINT+1                                 ;Storage at $93EF
EE05: D0 07     BNE DECX2                                   
EE07: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EE0A: C9 07     CMP #$07                                    
EE0C: F0 16     BEQ LHWALL1                                 
;
DECX2:
EE0E: 38        SEC                                         
EE0F: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EE12: E9 08     SBC #$08                                    
EE14: 8D EE 93  STA XPOINT                                   ;Storage at $93EE
EE17: B0 E0     BCS LEFTLOOP                                
EE19: CE EF 93  DEC XPOINT+1                                 ;Storage at $93EF
EE1C: F0 DB     BEQ LEFTLOOP                                
;
MASKOUT:
EE1E: A9 01     LDA #$01                                    
EE20: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
EE22: D0 BF     BNE LOOPMASK1                               
;
LHWALL1:
EE24: 8C EE 93  STY XPOINT                                   ;Storage at $93EE
;
LHWALL:
EE27: B1 C9     LDA (BYTE),Y                                 ;Tape End Addresses/End of Program
EE29: C5 FB     CMP CHAR2                                    ;Zero Page Storage FB
EE2B: F0 2D     BEQ SETWHOLEBYTE                            
;
SETBYTE1:
EE2D: A9 80     LDA #$80                                    
EE2F: 85 CC     STA MASKONE                                  ;Zero Page Storage CC
;
LOOPMASK2:
EE31: 20 B2 EF  JSR EORMAP                                  
EE34: F0 03     BEQ SKIPSKIP                                
;
NLCSKIP:
EE36: 4C C1 ED  JMP NEXTLINECHECK                           
;
SKIPSKIP:
EE39: A5 CC     LDA MASKONE                                  ;Zero Page Storage CC
EE3B: 20 86 EB  JSR FLIPLINE                                
;
L_EE3E:
EE3E: 20 77 EF  JSR UPPOINTX                                
;
XUP:
EE41: 46 CC     LSR MASKONE                                  ;Zero Page Storage CC
EE43: 90 EC     BCC LOOPMASK2                               
;
LOOPMAKS2END:
EE45: 20 70 EF  JSR UPBYTEX                                 
;
BYTEUP:
EE48: AD EF 93  LDA XPOINT+1                                 ;Storage at $93EF
EE4B: F0 07     BEQ CONTLOOP                                
EE4D: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EE50: C9 40     CMP #$40                                    
EE52: B0 E2     BCS NLCSKIP                                 
;
CONTLOOP:
EE54: B1 C9     LDA (BYTE),Y                                 ;Tape End Addresses/End of Program
EE56: C5 FB     CMP CHAR2                                    ;Zero Page Storage FB
EE58: D0 D3     BNE SETBYTE1                                
;
SETWHOLEBYTE:
EE5A: A9 FF     LDA #$FF                                    
EE5C: 45 FB     EOR CHAR2                                    ;Zero Page Storage FB
EE5E: 91 C9     STA (BYTE),Y                                 ;Tape End Addresses/End of Program
EE60: 18        CLC                                         
EE61: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EE64: 69 08     ADC #$08                                    
EE66: 8D EE 93  STA XPOINT                                   ;Storage at $93EE
EE69: 90 DA     BCC LOOPMAKS2END                            
EE6B: EE EF 93  INC XPOINT+1                                 ;Storage at $93EF
EE6E: D0 D5     BNE LOOPMAKS2END                            
;===================================================================================================
; "C" COMMAND. Clear
;===================================================================================================
;
CMD_C:
EE70: A9 00     LDA #$00                                    
;===================================================================================================
; "I" COMMAND. Invert
;===================================================================================================
;
INVERT:
EE72:           !BYTE $2C                                   ;bit command
 
EE73: A9 FF     LDA #$FF                                    
EE75: 85 FF     STA FLAG                                     ;Zero Page Storage FF
EE77: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
EE7A: 20 F8 EE  JSR CHECKRAMON3                             
;
CLEARIN:
EE7D: A9 00     LDA #$00                                    ; $9000 - Start of Bitmap Data
EE7F: A0 90     LDY #$90                                    
EE81: 85 C9     STA BYTE                                     ;Tape End Addresses/End of Program
EE83: 84 CA     STY BYTE+1                                   ;Tape End Addresses/End of Program
EE85: A9 19     LDA #$19                                    
EE87: 85 FD     STA RESULT                                   ;Results from graphics routine
;
LOOPSK:
EE89: A2 08     LDX #$08                                    
;
LOOP1K:
EE8B: A0 00     LDY #$00                                    
;
LOOPLINE:
EE8D: A9 FF     LDA #$FF                                    
EE8F: 24 FF     BIT FLAG                                     ;Zero Page Storage FF
EE91: 50 02     BVC CLEARSCREEN                             
EE93: 51 C9     EOR (BYTE),Y                                 ;Tape End Addresses/End of Program
;
CLEARSCREEN:
EE95: 91 C9     STA (BYTE),Y                                 ;Tape End Addresses/End of Program
EE97: C8        INY                                         
EE98: C0 28     CPY #$28                                    ; 40 columns?
EE9A: D0 F1     BNE LOOPLINE                                
EE9C: 20 3D EF  JSR ADD1K                                   
EE9F: CA        DEX                                         
EEA0: D0 E9     BNE LOOP1K                                  
EEA2: 20 45 EF  JSR NOT1K                                   
EEA5: E9 04     SBC #$04                                    
EEA7: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EEA9: C6 FD     DEC RESULT                                   ;Results from graphics routine
EEAB: D0 DC     BNE LOOPSK                                  
EEAD: 60        RTS                                         
;
;
GETINT:
EEAE: 20 70 00  JSR CHRGET                                   ;Subroutine: Get Next Byte of BASIC Text
;
L_EEB1:
EEB1: 20 84 BD  JSR TESTTYPE                                 ;Confirm Result
EEB4: 20 51 CD  JSR ROUND                                    ;Round FAC#1
EEB7: 4C EA C2  JMP AYINT                                    ;FAC#1 to Positive Integer
;
ALL3:
EEBA: 20 3A EC  JSR NUMBERIN                                
;
JUST2:
EEBD: 20 D4 EE  JSR CHECKY                                  
;
STOREPOINT:
EEC0: 8E F2 93  STX YPOINT                                   ;Storage at $93F2
;
L_EEC3:
EEC3: 8D EE 93  STA XPOINT                                   ;Storage at $93EE
EEC6: 8C EF 93  STY XPOINT+1                                 ;Storage at $93EF
EEC9: 60        RTS                                         
;
;
STOREND:
EECA: 8E F1 93  STX YEND                                     ;Storage at $93F1
EECD: 8D EC 93  STA XEND                                     ;Storage at $93EC
EED0: 8C ED 93  STY XEND+1                                   ;Storage at $93ED
EED3: 60        RTS                                         
;
;
CHECKY:
EED4: E0 C8     CPX #$C8                                    
EED6: 90 0B     BCC CHECKX                                  
;
OUTOFRANGE:
EED8: A2 FF     LDX #$FF                                    
;
ERROREXIT:
EEDA: 86 96     STX STATUSWORD                               ;Kernal I/O Status Word: ST
EEDC: 20 83 B8  JSR DO_BRAK                                  ;Perform []
;
OKEXIT:
EEDF: A6 B4     LDX STACKCOPY                                ;Zero Page Storage B4
EEE1: 9A        TXS                                         
EEE2: 60        RTS                                         
;
;
CHECKX:
EEE3: A5 11     LDA LINNUM                                   ;Temp: Integer Value
EEE5: A4 12     LDY ZP_12                                    ;Zero Page Storage 12
;
CHECKX2:
EEE7: C0 00     CPY #$00                                    
EEE9: F0 08     BEQ RETURN                                  
EEEB: C0 01     CPY #$01                                    
EEED: D0 E9     BNE OUTOFRANGE                              
EEEF: C9 40     CMP #$40                                    
EEF1: B0 E5     BCS OUTOFRANGE                              
;
RETURN:
EEF3: 60        RTS                                         
;
;
CHECKRAMON:
EEF4: A9 04     LDA #$04                                    
;
CHECKRAMON2:
EEF6: 85 BB     STA ZP_BB                                    ;Zero Page Storage BB
;
CHECKRAMON3:
EEF8: A2 FE     LDX #$FE                                    
EEFA: AD F3 93  LDA STOR_F3                                  ;Storage F3
EEFD: 49 FF     EOR #$FF                                    
EEFF: 8D F3 93  STA STOR_F3                                  ;Storage F3
EF02: CD F3 93  CMP STOR_F3                                  ;Storage F3
EF05: D0 D3     BNE ERROREXIT                               
;
CHECKOK:
EF07: 60        RTS                                         
;
;
ADDBYTE:
EF08: 18        CLC                                         
EF09: 65 C9     ADC BYTE                                     ;Tape End Addresses/End of Program
EF0B: 85 C9     STA BYTE                                     ;Tape End Addresses/End of Program
EF0D: 8A        TXA                                         
;
ADDHIBYTE:
EF0E: 65 CA     ADC BYTE+1                                   ;Tape End Addresses/End of Program
EF10: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EF12: 60        RTS                                         
;
;
CMPYEND:
EF13: AD F2 93  LDA YPOINT                                   ;Storage at $93F2
EF16: CD F1 93  CMP YEND                                     ;Storage at $93F1
EF19: F0 C4     BEQ OKEXIT                                  
EF1B: 60        RTS                                         
;
;
CMPXEND:
EF1C: AD EF 93  LDA XPOINT+1                                 ;Storage at $93EF
EF1F: CD ED 93  CMP XEND+1                                   ;Storage at $93ED
EF22: D0 08     BNE RETXEND                                 
EF24: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EF27: CD EC 93  CMP XEND                                     ;Storage at $93EC
EF2A: F0 B3     BEQ OKEXIT                                  
;
RETXEND:
EF2C: 60        RTS                                         
;
;
COMPAREY:
EF2D: 38        SEC                                         
EF2E: A9 C7     LDA #$C7                                    
EF30: ED F2 93  SBC YPOINT                                   ;Storage at $93F2
EF33: 60        RTS                                         
;
;
DOWNBYTEY:
EF34: CE F2 93  DEC YPOINT                                   ;Storage at $93F2
EF37: 29 07     AND #$07                                    
EF39: C9 07     CMP #$07                                    
EF3B: F0 08     BEQ NOT1K                                   
;
ADD1K:
EF3D: 18        CLC                                         
EF3E: A5 CA     LDA BYTE+1                                   ;Tape End Addresses/End of Program
EF40: 69 04     ADC #$04                                    
EF42: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EF44: 60        RTS                                         
;
;
NOT1K:
EF45: 38        SEC                                         
EF46: A5 C9     LDA BYTE                                     ;Tape End Addresses/End of Program
EF48: E9 D8     SBC #$D8                                    
EF4A: 85 C9     STA BYTE                                     ;Tape End Addresses/End of Program
EF4C: A5 CA     LDA BYTE+1                                   ;Tape End Addresses/End of Program
EF4E: E9 1B     SBC #$1B                                    
EF50: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EF52: 60        RTS                                         
;
;
UPBYTEY:
EF53: EE F2 93  INC YPOINT                                   ;Storage at $93F2
EF56: 29 07     AND #$07                                    
EF58: F0 08     BEQ CROSSLINE2                              
EF5A: 38        SEC                                         
EF5B: A5 CA     LDA BYTE+1                                   ;Tape End Addresses/End of Program
EF5D: E9 04     SBC #$04                                    
EF5F: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EF61: 60        RTS                                         
;
;
CROSSLINE2:
EF62: 18        CLC                                         
EF63: A5 C9     LDA BYTE                                     ;Tape End Addresses/End of Program
EF65: 69 D8     ADC #$D8                                    
EF67: 85 C9     STA BYTE                                     ;Tape End Addresses/End of Program
EF69: A5 CA     LDA BYTE+1                                   ;Tape End Addresses/End of Program
EF6B: 69 1B     ADC #$1B                                    
EF6D: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EF6F: 60        RTS                                         
;
;
UPBYTEX:
EF70: E6 C9     INC BYTE                                     ;Tape End Addresses/End of Program
EF72: D0 02     BNE BYTERET                                 
EF74: E6 CA     INC BYTE+1                                   ;Tape End Addresses/End of Program
;
BYTERET:
EF76: 60        RTS                                         
;
;
UPPOINTX:
EF77: EE EE 93  INC XPOINT                                   ;Storage at $93EE
EF7A: D0 03     BNE POINTRET                                
EF7C: EE EF 93  INC XPOINT+1                                 ;Storage at $93EF
;
POINTRET:
EF7F: 60        RTS                                         
;
;
DOWNBYTEX:
EF80: A5 C9     LDA BYTE                                     ;Tape End Addresses/End of Program
EF82: D0 02     BNE DECBYTE                                 
EF84: C6 CA     DEC BYTE+1                                   ;Tape End Addresses/End of Program
;
DECBYTE:
EF86: C6 C9     DEC BYTE                                     ;Tape End Addresses/End of Program
EF88: 60        RTS                                         
;
;
DOWNPOINTX:
EF89: AD EE 93  LDA XPOINT                                   ;Storage at $93EE
EF8C: D0 03     BNE DECPOINT                                
EF8E: CE EF 93  DEC XPOINT+1                                 ;Storage at $93EF
;
DECPOINT:
EF91: CE EE 93  DEC XPOINT                                   ;Storage at $93EE
EF94: 60        RTS                                         
;
;
LSRMASK:
EF95: A9 80     LDA #$80                                    
;
LOOPDEC:
EF97: 88        DEY                                         
EF98: F0 03     BEQ LSRRET                                  
EF9A: 4A        LSR                                         
EF9B: 90 FA     BCC LOOPDEC                                 
;
LSRRET:
EF9D: 60        RTS                                         
;
;
BYTETOCOPY:
EF9E: A5 C9     LDA BYTE                                     ;Tape End Addresses/End of Program
EFA0: 85 C0     STA XG                                       ;Zero Page Storage C0
EFA2: A5 CA     LDA BYTE+1                                   ;Tape End Addresses/End of Program
EFA4: 85 C1     STA XG+1                                     ;Zero Page Storage C1
EFA6: 60        RTS                                         
;
;
COPYTOBYTE:
EFA7: 48        PHA                                         
EFA8: A5 C0     LDA XG                                       ;Zero Page Storage C0
EFAA: 85 C9     STA BYTE                                     ;Tape End Addresses/End of Program
EFAC: A5 C1     LDA XG+1                                     ;Zero Page Storage C1
EFAE: 85 CA     STA BYTE+1                                   ;Tape End Addresses/End of Program
EFB0: 68        PLA                                         
EFB1: 60        RTS                                         
;
;
EORMAP:
EFB2: B1 C9     LDA (BYTE),Y                                 ;Tape End Addresses/End of Program
EFB4: 45 FB     EOR CHAR2                                    ;Zero Page Storage FB
EFB6: 24 CC     BIT MASKONE                                  ;Zero Page Storage CC
EFB8: 60        RTS                                         
;
;===================================================================================================
; HI BYTE of start of each character scanline
;===================================================================================================
;
HITABLE:
EFB9:           !BYTE $90,$94,$98,$9C,$A0,$A4,$A8,$AC       ;Bitmap HI byte for each character scanline
 
;===================================================================================================
; FILLER
;===================================================================================================
;
L_EFC1:
EFC1:           !BYTE $EA,$EA,$EA,$EA,$EA,$EA               ;filler
 
;===================================================================================================
; JUMP TABLE
;===================================================================================================
EFC7: 4C 80 EF  JMP DOWNBYTEX                               
EFCA: 4C 70 EF  JMP UPBYTEX                                 
EFCD: 4C 34 EF  JMP DOWNBYTEY                               
EFD0: 4C 53 EF  JMP UPBYTEY                                 
EFD3: 4C 77 EF  JMP UPPOINTX                                
EFD6: 4C 89 EF  JMP DOWNPOINTX                              
EFD9: 4C C0 EE  JMP STOREPOINT                              
EFDC: 4C CA EE  JMP STOREND                                 
EFDF: 4C F4 EE  JMP CHECKRAMON                              
EFE2: 4C 18 E9  JMP SET_CONTROL                             
EFE5: 4C 72 EC  JMP CONTLINE                                
EFE8: 4C 2D EF  JMP COMPAREY                                
EFEB: 4C 01 EA  JMP START                                   
EFEE: 4C 2C EB  JMP PLOT1                                   
EFF1: 4C 93 ED  JMP MAPOLDSET                               
EFF4: 4C 7D EE  JMP CLEARIN                                 
EFF7: 4C B8 EB  JMP EXAMINEIN                               
EFFA: 4C 14 EC  JMP CONTEXT                                 
EFFD: 4C 9F EC  JMP BACKIN2                                 
