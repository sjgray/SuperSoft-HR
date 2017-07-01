; TEKENING 2SS.ASM
; ================
; PET/CBM Software, originally written for Dubbel-Word Graphics Board.
; Disassembled and Adapted/Converted to SuperSoft HR Board by Steve J. Gray.
;
; Original loaded to $033A and placed the copy routine in an unused area. Why? Perhaps so BASIC program can be edited
; without touching the machine code.
;

;---- Storage

SRC    = $B1					; Source Pointer
DEST   = $E1					; Destination Pointer
CCOUNT = $DA					; Character Counter (0-999)
RASTER = $FF					; Raster Counter    (0-7)

SSMEM  = $9000					; Address of SuperSoft RAM

;---- Assembler Output

!TO "tekening2ss.prg",cbm

;---- START OF CODE
; This program will load at the PET/CBM's conventional load address of $0401 (1025 decimal)
; Assemble using ACME, with load address at start!

*=$0401

;---- This is the BASIC code  "10 SYS1040"

START_BASIC
		!BYTE <END_BASIC,>END_BASIC	; Link to next line of BASIC. In this case the End.
		!BYTE $0A,$00			; 10   (line number)
		!BYTE $9E			; SYS  (token)
		!PET "1040"			; 1040 (decimal address)
END_BASIC
		!BYTE $00,$00,$00               ; End of BASIC code

;---- Start of Machine Language

ML_START
		NOP				; 1037 Extra NOPs to pad out for SYS target
		NOP				; 1038
		NOP				; 1039
		LDA #<DATA			; Address of Source Data
		STA SRC				; Store LO byte in Source pointer    
		LDA #>DATA                      ; HI byte of Source address
		STA SRC+1                       ; Store HI byte in Source pointer    
		LDA #>SSMEM			; HI byte of Destination (LO must be ZERO)
		STA DEST+1                      ; Put it in Destination pointer HI
		LDY #$00			; Index for SRC/DEST ZP pointers                        
		STY RASTER			; First Raster
RAST_START
		LDX #$00 			; Start Count at zero
		STX DEST                        ; LO byte of Destination must start on 00 boundry
		STX CCOUNT			; Store it
		STX CCOUNT+1			; Store it
COPY_LOOP
		LDA (SRC),Y			; Read source byte
		TAX				; Use byte value as pointer into translation table
		LDA TRANS,X			; Translate it
		STA (DEST),Y                    ; Copy to destination

		INC SRC				; Increment Source pointer LO byte
		BNE skip1			; Has it wrapped around to 0? L_0356                        
		INC SRC+1                       ; Yes, increment Source pointer HI byte    
skip1
		INC DEST			; Increment Destination LO byte
		BNE skip2			; Has it wrapped to 0?   L_035C                        
		INC DEST+1                      ; Yes, Increment Destination HI byte     
skip2
		INC CCOUNT			; Increment Character Count LO
		BNE TEST_COUNT
		INC CCOUNT+1			; Increment Character Count HI
TEST_COUNT
		LDA CCOUNT+1			; Get the Character Count HI
		CMP #3                          ; Is Count on Page 3?
		BNE COPY_LOOP                   ; No, jump back up for more		
		LDA CCOUNT                      ; Get the Character Count LO
		CMP #$E8			; Is Count at $E8? ($03E8=1000 characters)
		BNE COPY_LOOP                   ; No, jump back up for more                     
RAST_DONE
		INC DEST+1			; Jump ahead to next HI byte!!! (SKIP last 24 bytes!)
		INC RASTER		        ; Next Raster
		LDA RASTER			; What Raster are we on?
		CMP #8				; Is it 8?
		BNE RAST_START			; No, go start another Raster block
		RTS                               

;---- Pixel Translation Table
; This is a pixel translation table for reversing the order of bits
; in the byte plus inverting them for SuperSoft

TRANS
!BYTE 255,127,191,63,223,95,159,31,239,111,175,47,207,79,143,15
!BYTE 247,119,183,55,215,87,151,23,231,103,167,39,199,71,135,7
!BYTE 251,123,187,59,219,91,155,27,235,107,171,43,203,75,139,11
!BYTE 243,115,179,51,211,83,147,19,227,99,163,35,195,67,131,3
!BYTE 253,125,189,61,221,93,157,29,237,109,173,45,205,77,141,13
!BYTE 245,117,181,53,213,85,149,21,229,101,165,37,197,69,133,5
!BYTE 249,121,185,57,217,89,153,25,233,105,169,41,201,73,137,9
!BYTE 241,113,177,49,209,81,145,17,225,97,161,33,193,65,129,1
!BYTE 254,126,190,62,222,94,158,30,238,110,174,46,206,78,142,14
!BYTE 246,118,182,54,214,86,150,22,230,102,166,38,198,70,134,6
!BYTE 250,122,186,58,218,90,154,26,234,106,170,42,202,74,138,10
!BYTE 242,114,178,50,210,82,146,18,226,98,162,34,194,66,130,2
!BYTE 252,124,188,60,220,92,156,28,236,108,172,44,204,76,140,12
!BYTE 244,116,180,52,212,84,148,20,228,100,164,36,196,68,132,4
!BYTE 248,120,184,56,216,88,152,24,232,104,168,40,200,72,136,8
!BYTE 240,112,176,48,208,80,144,16,224,96,160,32,192,64,128,0

!BYTE 0,0,0,0,0,0,0,0 ;filler

;---- Graphics Data
; Graphics Data should consist of 8 chunks of 1000 bytes
; Each chunk is the data for a single RASTER from every character on the screen
; Dubbel-W and SuperSoft pixels are rendered in opposite directions!

DATA
!BYTE $30,$30,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$FE ;Graphics Data
!BYTE $AF,$F7,$76,$33,$F7,$FC,$F9,$FF ;Graphics Data
!BYTE $07,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$F0,$7F,$55 ;Graphics Data
!BYTE $29,$6D,$93,$99,$19,$E3,$F8,$E0 ;Graphics Data
!BYTE $FF,$FB,$0F,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$70,$A7,$54,$A9 ;Graphics Data
!BYTE $A4,$24,$91,$88,$88,$31,$86,$83 ;Graphics Data
!BYTE $0F,$00,$F8,$FE,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$40,$63,$28,$A0,$42 ;Graphics Data
!BYTE $4A,$92,$44,$22,$22,$C4,$10,$06 ;Graphics Data
!BYTE $07,$FC,$FF,$1F,$F7,$03,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$04,$A2,$50,$81,$42 ;Graphics Data
!BYTE $42,$92,$40,$20,$22,$42,$08,$C3 ;Graphics Data
!BYTE $C0,$00,$3F,$E0,$87,$F9,$03,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$41,$80,$8A,$0A,$04 ;Graphics Data
!BYTE $04,$00,$12,$08,$00,$88,$10,$84 ;Graphics Data
!BYTE $C1,$C0,$01,$00,$00,$9C,$DD,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$A0,$10,$01,$00,$20,$10 ;Graphics Data
!BYTE $90,$00,$09,$04,$22,$02,$84,$10 ;Graphics Data
!BYTE $04,$02,$06,$80,$FF,$00,$23,$3D ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $80,$24,$09,$00,$22,$22,$40,$08 ;Graphics Data
!BYTE $82,$40,$80,$01,$00,$00,$0C,$A9 ;Graphics Data
!BYTE $03,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$40,$00,$40,$00 ;Graphics Data
!BYTE $00,$00,$10,$40,$00,$00,$04,$00 ;Graphics Data
!BYTE $02,$20,$20,$00,$0E,$80,$03,$22 ;Graphics Data
!BYTE $12,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $10,$00,$00,$00,$80,$88,$00,$01 ;Graphics Data
!BYTE $04,$01,$40,$00,$06,$00,$30,$10 ;Graphics Data
!BYTE $D4,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$04,$11,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$08,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$02,$02,$60,$00,$30,$20 ;Graphics Data
!BYTE $02,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$01,$04,$04,$80 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $08,$00,$00,$80,$00,$00,$00,$08 ;Graphics Data
!BYTE $80,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$11,$10,$02,$00 ;Graphics Data
!BYTE $00,$00,$00,$20,$04,$00,$80,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$02,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$20,$02,$42,$80 ;Graphics Data
!BYTE $00,$20,$00,$00,$00,$00,$11,$01 ;Graphics Data
!BYTE $00,$10,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $10,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$98,$80,$90 ;Graphics Data
!BYTE $24,$10,$00,$10,$00,$00,$00,$80 ;Graphics Data
!BYTE $00,$02,$00,$40,$00,$00,$00,$04 ;Graphics Data
!BYTE $28,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$DC,$0A ;Graphics Data
!BYTE $49,$12,$80,$00,$10,$02,$44,$44 ;Graphics Data
!BYTE $08,$01,$10,$10,$00,$38,$00,$11 ;Graphics Data
!BYTE $07,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$F8 ;Graphics Data
!BYTE $4A,$49,$02,$11,$01,$42,$88,$88 ;Graphics Data
!BYTE $10,$82,$60,$C0,$01,$00,$C7,$3A ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $C0,$AB,$24,$89,$84,$08,$21,$CC ;Graphics Data
!BYTE $08,$61,$30,$E0,$FF,$8F,$7D,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$F0,$DF,$32,$11,$31,$84 ;Graphics Data
!BYTE $C1,$38,$3C,$00,$F8,$0F,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$F0,$7F,$F7 ;Graphics Data
!BYTE $7E,$FE,$7F,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$F8,$FF,$3F,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$80,$FF ;Graphics Data
!BYTE $FF,$AD,$CD,$EE,$CE,$F3,$C7,$FF ;Graphics Data
!BYTE $3F,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$F8,$AD,$AA ;Graphics Data
!BYTE $96,$92,$4C,$66,$C6,$18,$87,$1F ;Graphics Data
!BYTE $00,$FC,$3F,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$F8,$59,$01,$14 ;Graphics Data
!BYTE $52,$92,$48,$64,$46,$8C,$61,$70 ;Graphics Data
!BYTE $C0,$FF,$1F,$F7,$03,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$E0,$9C,$52,$55,$08 ;Graphics Data
!BYTE $21,$49,$20,$11,$11,$23,$0C,$C1 ;Graphics Data
!BYTE $C0,$03,$00,$E0,$D9,$07,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$FA,$04,$05,$14,$08 ;Graphics Data
!BYTE $09,$00,$24,$12,$11,$21,$84,$20 ;Graphics Data
!BYTE $30,$F0,$00,$00,$78,$A6,$07,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$80,$24,$00,$20,$80 ;Graphics Data
!BYTE $90,$24,$81,$44,$44,$04,$08,$43 ;Graphics Data
!BYTE $20,$30,$00,$FF,$7F,$60,$F2,$01 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$20,$00,$08,$54,$80,$00 ;Graphics Data
!BYTE $02,$00,$40,$20,$00,$20,$40,$00 ;Graphics Data
!BYTE $02,$81,$01,$78,$00,$0F,$4C,$32 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$10,$20,$00,$20,$10 ;Graphics Data
!BYTE $10,$00,$40,$20,$00,$00,$02,$00 ;Graphics Data
!BYTE $41,$20,$60,$00,$F0,$07,$30,$52 ;Graphics Data
!BYTE $02,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$40,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $20,$00,$80,$00,$20,$22,$40,$08 ;Graphics Data
!BYTE $41,$10,$10,$80,$01,$00,$0C,$44 ;Graphics Data
!BYTE $3D,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$04,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$10,$08,$00,$08,$00 ;Graphics Data
!BYTE $00,$40,$20,$80,$01,$00,$C0,$20 ;Graphics Data
!BYTE $81,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$02,$04,$02,$00,$08 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$80 ;Graphics Data
!BYTE $00,$04,$00,$00,$10,$00,$40,$00 ;Graphics Data
!BYTE $90,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$90,$04,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$4E,$05,$10,$20 ;Graphics Data
!BYTE $10,$00,$10,$00,$00,$08,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$02,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$E0,$A9,$00,$12 ;Graphics Data
!BYTE $40,$00,$00,$04,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$04,$20,$00,$80,$40 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$78,$2A,$04 ;Graphics Data
!BYTE $00,$02,$11,$01,$42,$08,$00,$00 ;Graphics Data
!BYTE $10,$80,$40,$00,$00,$00,$00,$40 ;Graphics Data
!BYTE $12,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$38,$55 ;Graphics Data
!BYTE $02,$00,$11,$10,$01,$80,$00,$80 ;Graphics Data
!BYTE $00,$40,$20,$20,$00,$00,$80,$48 ;Graphics Data
!BYTE $02,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$F0 ;Graphics Data
!BYTE $15,$92,$24,$22,$22,$84,$10,$11 ;Graphics Data
!BYTE $21,$04,$81,$00,$1E,$F0,$20,$1D ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$5F,$49,$12,$09,$11,$42,$10 ;Graphics Data
!BYTE $31,$86,$C1,$01,$00,$70,$1F,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$7F,$4D,$66,$C6,$18 ;Graphics Data
!BYTE $06,$C7,$C3,$FF,$FF,$03,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$FE,$FF ;Graphics Data
!BYTE $FF,$FF,$07,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$F0,$FF,$FF,$FF,$1F,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$E0,$DF ;Graphics Data
!BYTE $57,$5B,$B2,$99,$39,$8F,$3F,$E0 ;Graphics Data
!BYTE $FF,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$FC,$56,$55 ;Graphics Data
!BYTE $69,$49,$32,$99,$31,$C7,$70,$F0 ;Graphics Data
!BYTE $FF,$7F,$FF,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$FC,$A6,$AA,$4A ;Graphics Data
!BYTE $09,$49,$24,$13,$31,$42,$18,$0E ;Graphics Data
!BYTE $3E,$00,$E0,$FB,$07,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$A0,$23,$05,$00,$A5 ;Graphics Data
!BYTE $04,$00,$92,$88,$88,$10,$C2,$30 ;Graphics Data
!BYTE $38,$80,$FF,$0F,$EE,$0F,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$06,$49,$00,$40,$21 ;Graphics Data
!BYTE $20,$49,$02,$81,$08,$10,$42,$18 ;Graphics Data
!BYTE $0C,$0C,$80,$FF,$80,$D9,$0F,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$0E,$01,$50,$80,$10 ;Graphics Data
!BYTE $02,$00,$48,$00,$22,$42,$84,$20 ;Graphics Data
!BYTE $18,$0C,$F0,$00,$80,$87,$AD,$03 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$20,$00,$01,$05,$42 ;Graphics Data
!BYTE $40,$92,$00,$02,$11,$11,$02,$08 ;Graphics Data
!BYTE $81,$40,$00,$07,$00,$70,$90,$6C ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$01,$00,$80,$00 ;Graphics Data
!BYTE $02,$00,$00,$02,$11,$11,$20,$84 ;Graphics Data
!BYTE $20,$10,$10,$00,$0F,$78,$40,$A4 ;Graphics Data
!BYTE $07,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$20 ;Graphics Data
!BYTE $04,$00,$00,$04,$02,$00,$02,$00 ;Graphics Data
!BYTE $20,$00,$08,$60,$00,$00,$30,$08 ;Graphics Data
!BYTE $22,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$03,$00,$80,$00,$00 ;Graphics Data
!BYTE $00,$80,$04,$00,$00,$00,$00,$10 ;Graphics Data
!BYTE $80,$20,$00,$40,$00,$00,$00,$01 ;Graphics Data
!BYTE $4A,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$20,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$20,$00,$04 ;Graphics Data
!BYTE $10,$00,$00,$01,$08,$00,$80,$40 ;Graphics Data
!BYTE $40,$03,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$60,$50,$21,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$20,$80,$80,$04 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$08,$00,$00,$00,$80,$00,$00 ;Graphics Data
!BYTE $20,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$40,$04,$10,$00 ;Graphics Data
!BYTE $00,$04,$40,$80,$00,$02,$00,$20 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $80,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$30,$01,$21 ;Graphics Data
!BYTE $40,$00,$00,$20,$00,$80,$88,$08 ;Graphics Data
!BYTE $00,$04,$00,$80,$00,$00,$00,$02 ;Graphics Data
!BYTE $24,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$F0,$82 ;Graphics Data
!BYTE $90,$24,$02,$01,$20,$04,$88,$08 ;Graphics Data
!BYTE $10,$82,$40,$C0,$00,$00,$60,$A4 ;Graphics Data
!BYTE $03,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$E0 ;Graphics Data
!BYTE $AB,$04,$40,$00,$40,$00,$20,$22 ;Graphics Data
!BYTE $42,$08,$02,$03,$E0,$0F,$98,$0E ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$BE,$96,$24,$12,$22,$84,$20 ;Graphics Data
!BYTE $42,$08,$06,$1E,$00,$CF,$0E,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$BE,$B6,$89,$18,$61 ;Graphics Data
!BYTE $38,$38,$FC,$FB,$FF,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$FE ;Graphics Data
!BYTE $FF,$03,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $80,$FF,$FF,$FF,$FF,$FF,$07,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$F8,$7F ;Graphics Data
!BYTE $BD,$B6,$6D,$76,$E6,$78,$FC,$FF ;Graphics Data
!BYTE $FF,$07,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$7F,$AB,$AA ;Graphics Data
!BYTE $94,$24,$C9,$44,$CC,$38,$0E,$0F ;Graphics Data
!BYTE $F8,$8F,$FF,$01,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$06,$49,$55,$A1 ;Graphics Data
!BYTE $A4,$24,$92,$88,$88,$31,$86,$C1 ;Graphics Data
!BYTE $01,$FE,$3F,$DC,$0F,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$70,$4C,$A8,$AA,$10 ;Graphics Data
!BYTE $92,$24,$09,$44,$44,$8C,$31,$0C ;Graphics Data
!BYTE $06,$7C,$00,$F0,$B1,$1F,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$12,$12,$AA,$0A,$84 ;Graphics Data
!BYTE $04,$00,$90,$08,$80,$08,$21,$04 ;Graphics Data
!BYTE $83,$03,$7C,$00,$1F,$6E,$0F,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$40,$11,$08,$05,$05,$42 ;Graphics Data
!BYTE $40,$92,$00,$22,$00,$20,$40,$10 ;Graphics Data
!BYTE $04,$02,$0C,$00,$00,$18,$52,$03 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$60,$00,$42,$00,$10,$00 ;Graphics Data
!BYTE $08,$00,$24,$10,$00,$00,$21,$84 ;Graphics Data
!BYTE $60,$30,$C0,$00,$00,$80,$61,$7B ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$08,$50,$00,$00 ;Graphics Data
!BYTE $00,$92,$04,$10,$00,$00,$01,$02 ;Graphics Data
!BYTE $10,$08,$0C,$E0,$00,$80,$83,$49 ;Graphics Data
!BYTE $06,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$00,$00,$00,$00,$01 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$20,$84 ;Graphics Data
!BYTE $00,$08,$04,$18,$00,$00,$C0,$90 ;Graphics Data
!BYTE $38,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$10,$20,$80,$00,$00,$80,$00 ;Graphics Data
!BYTE $02,$00,$10,$20,$00,$00,$00,$42 ;Graphics Data
!BYTE $A0,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$14,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$02,$02,$00 ;Graphics Data
!BYTE $00,$00,$01,$00,$04,$00,$00,$01 ;Graphics Data
!BYTE $04,$02,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$20,$02,$00,$00,$10 ;Graphics Data
!BYTE $08,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$40,$00,$00,$00,$10 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$94,$20,$00,$00 ;Graphics Data
!BYTE $00,$11,$01,$00,$00,$80,$08,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$80,$53,$85,$04 ;Graphics Data
!BYTE $09,$40,$04,$00,$10,$00,$00,$00 ;Graphics Data
!BYTE $84,$00,$08,$00,$40,$00,$40,$00 ;Graphics Data
!BYTE $62,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$E0,$54,$08 ;Graphics Data
!BYTE $09,$20,$22,$02,$04,$10,$00,$00 ;Graphics Data
!BYTE $21,$00,$81,$00,$03,$00,$80,$21 ;Graphics Data
!BYTE $19,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$E0,$29 ;Graphics Data
!BYTE $24,$40,$20,$22,$42,$00,$01,$00 ;Graphics Data
!BYTE $21,$04,$81,$00,$03,$00,$18,$D2 ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$80 ;Graphics Data
!BYTE $57,$29,$89,$44,$04,$08,$41,$40 ;Graphics Data
!BYTE $84,$10,$0C,$0C,$00,$00,$66,$07 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$F8,$2B,$49,$64,$44,$08,$41 ;Graphics Data
!BYTE $8C,$31,$38,$E0,$FF,$B8,$07,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$F0,$5B,$36,$23,$86 ;Graphics Data
!BYTE $C1,$C0,$07,$C4,$3F,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $F0,$FF,$FF,$FF,$FF,$FF,$7F,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$FC,$AB ;Graphics Data
!BYTE $6A,$6D,$9B,$CD,$19,$E7,$E3,$FF ;Graphics Data
!BYTE $FF,$1F,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$80,$9F,$54,$55 ;Graphics Data
!BYTE $4A,$9A,$24,$32,$23,$86,$E1,$E0 ;Graphics Data
!BYTE $07,$F0,$FB,$07,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$FF,$96,$2A,$14 ;Graphics Data
!BYTE $12,$12,$49,$44,$44,$8C,$61,$38 ;Graphics Data
!BYTE $F0,$01,$C0,$E7,$1F,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$D8,$91,$42,$01,$42 ;Graphics Data
!BYTE $48,$92,$44,$22,$22,$42,$08,$82 ;Graphics Data
!BYTE $81,$03,$00,$00,$CE,$3A,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$6D,$A4,$00,$A0,$10 ;Graphics Data
!BYTE $92,$24,$09,$44,$44,$84,$18,$82 ;Graphics Data
!BYTE $40,$C0,$03,$00,$E0,$91,$1E,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$C0,$20,$42,$00,$10,$08 ;Graphics Data
!BYTE $09,$00,$24,$10,$11,$11,$02,$08 ;Graphics Data
!BYTE $82,$81,$03,$C0,$1F,$E0,$AC,$06 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$40,$10,$00,$40,$08 ;Graphics Data
!BYTE $01,$00,$00,$81,$88,$08,$10,$42 ;Graphics Data
!BYTE $10,$08,$30,$00,$00,$00,$86,$F4 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$20,$00,$00,$04,$01,$42 ;Graphics Data
!BYTE $40,$00,$20,$00,$88,$08,$10,$40 ;Graphics Data
!BYTE $00,$04,$02,$18,$00,$00,$0C,$12 ;Graphics Data
!BYTE $0D,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$24,$09,$20,$00,$11,$00,$00 ;Graphics Data
!BYTE $10,$04,$02,$04,$00,$00,$00,$21 ;Graphics Data
!BYTE $45,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$40,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$44,$00,$00 ;Graphics Data
!BYTE $40,$10,$08,$10,$00,$00,$00,$84 ;Graphics Data
!BYTE $14,$01,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$8C,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$00,$02,$00,$00,$82 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$40,$01,$00,$01,$00 ;Graphics Data
!BYTE $00,$80,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$20,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $02,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$0C,$0A,$04,$00 ;Graphics Data
!BYTE $02,$00,$00,$02,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$08,$00,$00,$02 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$80,$09,$20,$20 ;Graphics Data
!BYTE $80,$00,$00,$00,$00,$20,$22,$02 ;Graphics Data
!BYTE $00,$20,$00,$08,$80,$00,$20,$20 ;Graphics Data
!BYTE $88,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$C0,$0B,$42 ;Graphics Data
!BYTE $80,$04,$00,$00,$80,$00,$11,$11 ;Graphics Data
!BYTE $00,$08,$02,$01,$04,$00,$40,$90 ;Graphics Data
!BYTE $12,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$C0,$57 ;Graphics Data
!BYTE $09,$09,$04,$00,$04,$08,$10,$11 ;Graphics Data
!BYTE $02,$08,$02,$01,$1C,$00,$07,$A9 ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $AF,$42,$12,$88,$88,$10,$02,$84 ;Graphics Data
!BYTE $08,$61,$30,$F0,$00,$E0,$D9,$07 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$E0,$55,$92,$88,$88,$10,$86 ;Graphics Data
!BYTE $11,$C2,$C1,$01,$00,$F7,$03,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$80,$EF,$C9,$CC,$39 ;Graphics Data
!BYTE $0E,$0F,$F8,$BF,$0F,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $FF,$FF,$FF,$FE,$FE,$FF,$FF,$07 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$FF,$FD ;Graphics Data
!BYTE $D5,$92,$64,$32,$E7,$1C,$1F,$FE ;Graphics Data
!BYTE $FF,$7F,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$C0,$6F,$AB,$2A ;Graphics Data
!BYTE $25,$65,$92,$89,$98,$71,$1C,$1E ;Graphics Data
!BYTE $F8,$FF,$FC,$0F,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$1F,$29,$80,$4A ;Graphics Data
!BYTE $49,$80,$24,$32,$33,$42,$18,$06 ;Graphics Data
!BYTE $0F,$F0,$7F,$78,$7F,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$38,$26,$14,$54,$08 ;Graphics Data
!BYTE $01,$49,$20,$11,$11,$21,$84,$61 ;Graphics Data
!BYTE $70,$80,$FF,$FF,$70,$7F,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$83,$00,$54,$05,$42 ;Graphics Data
!BYTE $40,$92,$40,$20,$22,$42,$04,$61 ;Graphics Data
!BYTE $30,$30,$00,$FC,$01,$66,$37,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$40,$10,$00,$40,$20 ;Graphics Data
!BYTE $00,$49,$00,$81,$88,$08,$21,$84 ;Graphics Data
!BYTE $41,$40,$00,$3E,$E0,$03,$D1,$0F ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$07,$00,$00,$00,$20 ;Graphics Data
!BYTE $20,$49,$02,$08,$00,$80,$08,$01 ;Graphics Data
!BYTE $08,$04,$0C,$C0,$FF,$1F,$18,$C9 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$20,$00,$01,$04,$00 ;Graphics Data
!BYTE $08,$00,$00,$81,$00,$80,$00,$21 ;Graphics Data
!BYTE $08,$02,$01,$06,$00,$00,$30,$A4 ;Graphics Data
!BYTE $0E,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$20,$00,$00,$00,$00 ;Graphics Data
!BYTE $80,$00,$40,$00,$10,$00,$01,$42 ;Graphics Data
!BYTE $00,$00,$00,$02,$00,$3E,$00,$02 ;Graphics Data
!BYTE $70,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$11,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$01,$40,$00,$04,$08 ;Graphics Data
!BYTE $01,$00,$00,$08,$00,$00,$00,$08 ;Graphics Data
!BYTE $C0,$01,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$82,$10,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$20,$00 ;Graphics Data
!BYTE $00,$02,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $20,$02,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$C0,$08,$08,$48,$02 ;Graphics Data
!BYTE $80,$08,$00,$00,$80,$00,$00,$00 ;Graphics Data
!BYTE $00,$40,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $10,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$68,$00,$21,$49 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$10 ;Graphics Data
!BYTE $00,$00,$00,$02,$00,$00,$00,$80 ;Graphics Data
!BYTE $00,$01,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$A7,$0A,$01 ;Graphics Data
!BYTE $10,$08,$00,$08,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$10,$10,$00,$03,$18,$10 ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$80,$A7,$10 ;Graphics Data
!BYTE $12,$40,$04,$44,$00,$20,$00,$00 ;Graphics Data
!BYTE $42,$00,$00,$02,$08,$00,$20,$08 ;Graphics Data
!BYTE $0C,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$80,$AF ;Graphics Data
!BYTE $42,$92,$40,$44,$80,$10,$22,$22 ;Graphics Data
!BYTE $40,$10,$04,$06,$E0,$FF,$C0,$D4 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $5E,$95,$24,$11,$11,$21,$84,$08 ;Graphics Data
!BYTE $11,$82,$C0,$00,$FF,$1F,$A6,$03 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$80,$AF,$25,$13,$11,$63,$08 ;Graphics Data
!BYTE $62,$1C,$06,$FE,$FF,$FC,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$FE,$7F,$33,$C6 ;Graphics Data
!BYTE $70,$F0,$07,$FF,$01,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$C0 ;Graphics Data
!BYTE $FF,$7F,$DB,$EF,$BD,$FF,$FF,$3F ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$80,$BF,$57 ;Graphics Data
!BYTE $2B,$6D,$9B,$CD,$98,$E3,$F0,$01 ;Graphics Data
!BYTE $C0,$FF,$01,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$E0,$B3,$55,$95 ;Graphics Data
!BYTE $92,$92,$49,$64,$46,$08,$C3,$C1 ;Graphics Data
!BYTE $07,$00,$DF,$3F,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$80,$61,$52,$55,$21 ;Graphics Data
!BYTE $24,$49,$12,$89,$08,$31,$86,$C1 ;Graphics Data
!BYTE $80,$0F,$80,$8F,$FD,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$68,$48,$A1,$02,$A5 ;Graphics Data
!BYTE $24,$00,$12,$88,$88,$10,$43,$18 ;Graphics Data
!BYTE $0C,$78,$00,$00,$8F,$FD,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$09,$49,$01,$10,$08 ;Graphics Data
!BYTE $09,$00,$24,$12,$11,$21,$C2,$10 ;Graphics Data
!BYTE $0C,$0C,$F0,$03,$7E,$98,$7D,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$80,$84,$AA,$02,$01 ;Graphics Data
!BYTE $24,$00,$12,$08,$00,$80,$10,$42 ;Graphics Data
!BYTE $20,$30,$C0,$01,$00,$1C,$26,$0F ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$80,$04,$AA,$02,$01 ;Graphics Data
!BYTE $04,$00,$90,$40,$44,$44,$00,$20 ;Graphics Data
!BYTE $04,$02,$03,$38,$00,$E0,$60,$32 ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$10,$08 ;Graphics Data
!BYTE $00,$00,$00,$08,$40,$04,$08,$00 ;Graphics Data
!BYTE $04,$81,$00,$01,$00,$00,$40,$48 ;Graphics Data
!BYTE $19,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$40,$00,$00,$00,$00,$80 ;Graphics Data
!BYTE $00,$00,$00,$00,$01,$00,$10,$00 ;Graphics Data
!BYTE $08,$02,$01,$01,$E0,$C1,$03,$44 ;Graphics Data
!BYTE $4A,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$02,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$04,$00,$00,$FE,$03,$00 ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$48,$28,$00,$00,$01 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$80,$00,$00,$01,$00,$00,$04 ;Graphics Data
!BYTE $08,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$80,$A4,$02,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$01,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$10,$45,$00,$00 ;Graphics Data
!BYTE $20,$00,$00,$40,$00,$01,$00,$00 ;Graphics Data
!BYTE $40,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $40,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$1E,$40,$48 ;Graphics Data
!BYTE $02,$80,$88,$00,$21,$04,$00,$40 ;Graphics Data
!BYTE $08,$01,$00,$00,$00,$1C,$07,$00 ;Graphics Data
!BYTE $70,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$5B,$85 ;Graphics Data
!BYTE $04,$09,$40,$00,$08,$01,$02,$22 ;Graphics Data
!BYTE $00,$10,$04,$04,$70,$00,$1C,$44 ;Graphics Data
!BYTE $09,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$5B ;Graphics Data
!BYTE $94,$00,$89,$80,$08,$00,$04,$40 ;Graphics Data
!BYTE $84,$20,$08,$08,$00,$00,$20,$6A ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $F8,$2A,$49,$22,$22,$42,$08,$11 ;Graphics Data
!BYTE $22,$0C,$03,$03,$00,$80,$F9,$01 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$FF,$DA,$24,$22,$84,$30 ;Graphics Data
!BYTE $8C,$61,$78,$00,$C0,$7F,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$E0,$B7,$DD,$39 ;Graphics Data
!BYTE $8F,$0F,$F8,$3E,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$F8 ;Graphics Data
!BYTE $FF,$DE,$BF,$DD,$7B,$FF,$FE,$FF ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$E0,$DB,$AA ;Graphics Data
!BYTE $D6,$92,$64,$22,$66,$1C,$0F,$FF ;Graphics Data
!BYTE $FF,$FF,$03,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$F0,$5C,$AA,$42 ;Graphics Data
!BYTE $49,$49,$20,$13,$30,$C6,$38,$3C ;Graphics Data
!BYTE $F0,$FF,$E7,$7F,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$C0,$9F,$85,$0A,$94 ;Graphics Data
!BYTE $90,$24,$89,$44,$C4,$08,$61,$38 ;Graphics Data
!BYTE $78,$00,$00,$F0,$FE,$01,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$9C,$11,$0A,$28,$10 ;Graphics Data
!BYTE $90,$24,$89,$44,$44,$8C,$30,$04 ;Graphics Data
!BYTE $03,$07,$C0,$1F,$70,$F6,$01,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$30,$12,$20,$40,$21 ;Graphics Data
!BYTE $20,$49,$00,$81,$88,$10,$21,$08 ;Graphics Data
!BYTE $02,$03,$0E,$00,$80,$63,$EA,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$40,$00,$20,$00,$08,$84 ;Graphics Data
!BYTE $00,$24,$80,$40,$44,$44,$08,$21 ;Graphics Data
!BYTE $18,$0C,$38,$00,$00,$E0,$D8,$1E ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$80,$08,$80,$00,$08,$84 ;Graphics Data
!BYTE $00,$00,$00,$04,$00,$00,$84,$10 ;Graphics Data
!BYTE $00,$81,$00,$00,$00,$00,$83,$C4 ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $01,$49,$00,$00,$04,$40,$80,$10 ;Graphics Data
!BYTE $80,$40,$C0,$00,$F0,$7F,$80,$91 ;Graphics Data
!BYTE $1C,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$80,$00,$00,$00,$04 ;Graphics Data
!BYTE $00,$00,$00,$02,$00,$00,$00,$20 ;Graphics Data
!BYTE $00,$80,$80,$00,$18,$00,$0C,$88 ;Graphics Data
!BYTE $20,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$4A,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$04,$00,$40,$00 ;Graphics Data
!BYTE $20,$08,$00,$04,$80,$01,$0C,$10 ;Graphics Data
!BYTE $28,$01,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$80,$20,$00 ;Graphics Data
!BYTE $00,$00,$08,$00,$00,$00,$00,$40 ;Graphics Data
!BYTE $00,$00,$40,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $01,$01,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$03,$40,$00,$00 ;Graphics Data
!BYTE $01,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$F0,$10,$08,$00 ;Graphics Data
!BYTE $04,$02,$22,$00,$08,$10,$00,$00 ;Graphics Data
!BYTE $02,$00,$04,$00,$10,$00,$00,$01 ;Graphics Data
!BYTE $84,$01,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$64,$15,$02 ;Graphics Data
!BYTE $00,$01,$00,$00,$00,$40,$44,$04 ;Graphics Data
!BYTE $00,$40,$20,$20,$00,$E0,$00,$88 ;Graphics Data
!BYTE $44,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$26,$20 ;Graphics Data
!BYTE $20,$80,$08,$88,$00,$40,$20,$00 ;Graphics Data
!BYTE $84,$20,$08,$08,$80,$C7,$03,$A2 ;Graphics Data
!BYTE $04,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$3C ;Graphics Data
!BYTE $21,$24,$10,$08,$10,$21,$40,$04 ;Graphics Data
!BYTE $08,$41,$10,$30,$00,$00,$18,$75 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $F0,$55,$92,$44,$40,$84,$10,$22 ;Graphics Data
!BYTE $C4,$10,$0C,$1C,$00,$70,$F6,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$7C,$25,$C9,$CC,$08,$43 ;Graphics Data
!BYTE $30,$86,$83,$FF,$BF,$1F,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$FF,$EF,$CE ;Graphics Data
!BYTE $F1,$F1,$FF,$07,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$00,$00,$00,$00,$00,$00,$00 ;Graphics Data
!BYTE $00,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
!BYTE $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA ;Graphics Data
 