
;
; ---- Equates
;

imagelow = $01   ;USR Address Low Byte / High Byte
imagecount = $03   ;Search Character
screenlow = $04   ;Flag: Scan for Quote at End of String
screenhigh  = $05   ;Input Buffer Pointer / No. of Subscripts
nolines= $06   ;Flag: Default Array DiMension / array name initial / AND
mstore  = $07   ;Data Type: $FF = String
bcount = $08   ;Data Type: $80 = Integer

xstore= $CF   ;End of tape read
screencount= $D4   ;Read character error




!TO "mounthr40s.prg",cbm


*=$0401

;---- This is the BASIC code  "10 SYS1037"

START_BASIC
		!BYTE <END_BASIC,>END_BASIC	; Link to next line of BASIC. In this case the End.
		!BYTE $0A,$00			; 10   (line number)
		!BYTE $9E			; SYS  (token)
		!PET "1037"			; 1040 (decimal address)
END_BASIC
		!BYTE $00,$00,$00               ; End of BASIC code


; ---- Code
;     

L_0519:        LDA #$09			; 11 images to display one after other
               STA imagecount ; image counter
L_051D:        

L_051F:        

L_052C:        LDX imagecount   ;load the actual image number we are to display
               LDA IMAGEL,X	; get the low byte of the image address in memory
               STA imagelow 	; low byte of image address
               LDA IMAGEH,X	; get high byte of image address in memory
               STA $02		; high byte of image address
               LDA #$00		; set low byte for screen ram to 0
               STA screenlow 	; set low byte for screen ram
	       STA screencount  ; offset that will be added to base 1K address to set high byte for that ram block
               LDA #$17		; set number of lines to display on screen = 25 in this case
               STA nolines    	; no lines to display = 25
               LDY #$00		
               LDA (imagelow),Y ; get low byte of image and test if 0
               TAX
	       CMP #$FF		; set in X as used to say how many to display
               BNE L_0556	
		INX 
              SEC		; for some reason it reduced address by 1
               LDA imagelow 	; image Address Low Byte / High Byte
               SBC #$01		; if byte is 0 then lower image address by 1 - it is compressed 
               STA imagelow 	; image Address Low Byte / High Byte
               BCS L_0556
               DEC $02		; reduce high byte if low byte went past 0

L_0556:        LDA #$00
               STA mstore 	; counter for number of bits - lines in character block = 8

L_055A:        LDY #$27		; set no characters to display in row to 40
	       STX xstore	; save X 
	       LDX mstore	; load counter for number of bits in line
	       LDA SCREENP,X	; get supersoft memory high byte address
	       CLC 
               ADC screencount	; add offset
               STA screenhigh 	; set high screen byte
	       LDX xstore	; restore x

L_0561:        LDA #$FF		; load blank - supersoft this is FF

L_0563:        DEX		; image is compressed. So if x = 0 the dex will set it to 255 and display 255 characters 		
               BEQ L_05A7	; if x is zero then display real character
               STA (screenlow),Y ;store blank in screen ram at address
               DEY		; decrease character by 1
               BPL L_0563	; if still > 0 then repeat

L_056B:        INC mstore 	; increase to next bit
               LDA #$08		; check displayed 8 bits
               CMP mstore 	;
               BNE L_055A  	; repeat for next bit if not displayed all 8
               CLC
               LDA #$28		; increase screen ram low byte by 40
               ADC screenlow 	; add 40 to low byte of screen ram
               STA screenlow 	; save back to memory	
               BCC L_057E	; if gone over FF then set high byte counter
               INC screencount 	; increase high byte offset from base high byte address (1K)

L_057E:        DEC nolines    	; have we displayed 25 lines. if not then display more
               BNE L_0556


L_0591:       
 		DEC imagecount 	; decrease image counter by 1 
               BPL L_051D	; display next image on screen

	       LDA #$FF
    		CMP $97          ;Current Key Pressed: 255 = No Key
     		BEQ L_0519
	;	BNE L_059E	;end program if key pressed
  ;		JMP L_0519

L_059E:        		
               RTS

L_05A7:        INX		; routine to display an actual bit on screen

L_05A8:        STY bcount 	; save Y
               LDY #$00
               INC imagelow 	; image Address Low Byte 
               BNE L_05B2
               INC $02

L_05B2:        LDA (imagelow),Y ; image Address Low Byte 
               CMP #$FF
		BEQ L_05BF
 	       LDY bcount 	; restore Y which is character count              
L_05B3:        STA (screenlow),Y ;save bit on screen
               DEY		; reduce Y by 1
               BPL L_05A8	; repeat if more characters to display
               BMI L_056B	; go back to next line in byte

L_05BF:        INC imagelow 	; image Address Low Byte 
               BNE L_05C5
               INC $02

L_05C5:        LDA (imagelow),Y ; image Address  High Byte
               LDY bcount 
               TAX
	       CMP #$FF 
               BNE L_0561	; display next bit/kline in character block
		INX
               SEC
               LDA imagelow 	;image Address Low Byte
               SBC #$01
               STA imagelow 	;image Address Low Byte 
               BCS L_0561
               DEC $02
               BNE L_0561
	       RTS



IMAGEL
!BYTE $00,$CA,$E6       ; low byte for image table in ram
!BYTE $7B,$7B,$97
!BYTE $CF,$DF,$A1
!BYTE $08,$00,$00
!BYTE $00,$00,$00,$00

SCREENP
!BYTE $90,$94,$98,$9C,$A0,$A4    ; high byte for 1K blocks on screen ram on supersoft
!BYTE $A8,$AC,$00,$00,$00,$00,$00,$00,$00,$00

IMAGEH
!BYTE $06,$10,$1B,$27,$33,$3F,$4B,$57,$63,$6F,$00,$00 ; high bytes for image table in ram
