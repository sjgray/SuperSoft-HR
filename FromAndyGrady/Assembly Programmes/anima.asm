; This code is split into two sections. They were build as separte pieces of code and merged 
; so there is scope for some variable optimisation.
; The compression routine works as follows. Each Character block is an 8 x 8 bit matrix for the SS board.
; the horizontal bits (8) and vertical bits (8) make up the single byte stored in the SS memory. 
; The screen is 40 columns so there are 40 bytes per line The vertical rows
; are split according to memory address. $9000, $9400, $9800 etc (8 of them)
; so the code starts from RIGHT to LEFT and works in a unidirection mode counting sequention FF's which are blanks. 
; When 255 are counted then FF gets written to memory, the counter is reset and starts again. If another 255 are counted the the cycle is repeated.
; IF less than 255 are counted then FF followed by the no of FF's counted are written to memory. The cycle of FF counts must be terminated by FF 
; and no of FF counted even if it is 0. eg FF FF FF 05 (here we have counted 2 x 255 FF's and 5 FF, or FF FF FF 00 (here we counted only 2 x 255 FF's ... and terminated with FF 00)
; 
; Setup:- image count is stored at $1f00. the low address byte of the compressed image at $1f01, image count and high byte at $1f31, image count
; These must be set before calling the compression routing for the first time
;
; ---- Setup variable locations
;

imagelow = $01   ;USR Address Low Byte / High Byte
imagehi=$02
screenlow = $04   ;Flag: Scan for Quote at End of String
screenhigh  = $05   ;Input Buffer Pointer / No. of Subscripts
nolines= $027A   ;Flag: Default Array DiMension / array name initial / AND
screencount= $027B   ;Data Type: $FF = String
ycount = $027C   ;Data Type: $80 = Integer
imagecounta = $027D
ffcount = $027E   ;End of tape read
screenbyte=$027F   ;Read character error
imagecount =$1F00
imageaddrlow=$1F01
imageaddrhi=$1F31
icounter=$ff


mstore  = $0280   ;Data Type: $FF = String
bcount = $0281  ;Data Type: $80 = Integer

xstore= $0282   ;End of tape read




!TO "anima.prg",cbm


*=$7E10

;---- This is the compression routine
;---- Code
;     
	       JSR L_store		; save contents of zero page ram as approprite

L_051D:        LDA #$00
	       STA ffcount		; zero FF blank count
L_051F:        TAY			; LDY #$00

L_052C:        LDX imagecount		; get image count		
	       LDA imageaddrlow,X	; get low address from the table for where to store the next image 
               STA imagelow 		; low byte of image address
	       
	       LDA imageaddrhi,X	; get high address from the table for where to store the next image
	       STA imagehi		; high byte of image address
	       TYA
	       TAX
               STA screenlow 		; set low byte for screen ram
	       STA screencount  	; offset that will be added to base 1K address to set high byte for that ram block

	       STA screenbyte   	; image pixel store
              
               LDA #$19			; set number of lines to display on screen = 25 in this case
               STA nolines    		; no lines to display = 25
               	        

L_055A:        LDY #$27			; set no characters to display in row to 40

	       LDA SCREENP,X		; get supersoft memory high byte address
	       CLC 
               ADC screencount		; add offset
               STA screenhigh 		; set high screen byte

L_0561:        LDA (screenlow),Y	; get character from image
	       STA screenbyte		; store pixel

L_0561a:      STY ycount		; store y pointer to image pixel
		LDY #$00		; zero Y
L_0563:        CMP #$FF		; if blank then inc count and get next byte
               BNE L_0567	; if not blank then check if already counted blanks

	       INC ffcount	; inc blank counter by 1

L_0565:        LDA #$FF		; test if 255 - if so the write to memory
	       CMP ffcount		; check how many FF's counted. 
	    	BEQ L_0574	; if not 255 then 

		BNE L_0575

L_0567:         ; pixel is not a blank to store any counted blanks and then the pixel
		
		LDA #$00		; have we counted any FF's
	        CMP ffcount		
	        BNE L_0570	; yes we have counted any FF's so write to memory first

		JSR L_sub	; check for last FF write as if doen then 
		LDA (imagelow),Y
		CMP #$FF	; have to write FF 00 otherwise character will be interpreted
		BNE L_0569	; as part of the number of blanks
		JSR imageinc
		BNE L_0570

L_0569:		JSR imageinc
		BNE L_0572

L_0570:        	LDA #$FF	; so write FF to memory
		STA (imagelow),y
		JSR imageinc

		LDA ffcount	; then write number of FF's
		STA (imagelow),Y
		JSR imageinc
	
L_0572:	       	LDA screenbyte	; get screen byte
L_0574:		


	       STA (imagelow),y  ; store pixel byte and increase memory store by 1
	 
	       LDA #$00		; reset FF count
	       STA ffcount

 	       JSR imageinc	; increase mem counter by one

L_0575:		CLC
		LDY ycount
		DEY		; have we done 40 pixels. if not do more
		BPL L_0561


L_057E:		INX		; go to next image map (1K) 
		CPX #$08	; have we done all 8 K blocks

		BNE L_055A
		
L_057F:          ; done 40 pixels so do next row of pixels
               LDA #$28		; increase screen ram low byte by 40
	       CLC 
               ADC screenlow 	; add 40 to low byte of screen ram
               STA screenlow 	; save back to memory	
               BCC L_0580	; if gone over FF then set high byte counter
               INC screencount 	; increase high byte offset from base high byte address (1K)

L_0580:	       
		LDA #$00

		LDX #$00
		DEC nolines	; how many lines have we processed
		BEQ L_0585	; if less than 25 lines then do next line otherwise stop
		BNE L_055A
L_0585:		LDY #$00			; finished
	       TYA		;LDA #$00		; have we counted any FF's
	       CMP ffcount		
	       BEQ L_0586	; no we havent counted any FF's so write pixel to memory

	
		LDA #$FF	; have we written FF already to memory on last write
		STA (imagelow),y
		JSR imageinc	; restore memory pointer
		LDA ffcount	; save count number to memory
		STA (imagelow),y
		JSR imageinc

L_0586	     	LDA screenbyte	; get screen byte
		CMP #$FF
		BEQ L_finish
	        STA (imagelow),y  ; store pixel byte and increase memory store by 1
	       	JSR imageinc
L_finish:	
		LDA #$AA
		STA (imagelow),y
		JSR imageinc
		LDA #$AA
		STA (imagelow),y
		JSR imageinc
		INC imagecount
		LDX imagecount
		LDA imagelow
		
		STA imageaddrlow,X
		LDA imagehi
		STA imageaddrhi,X

		JSR L_restore

		RTS


imageinc:      INC imagelow 	; image Address Low Byte 
               BNE incimage2
               INC imagehi
incimage2:     RTS

L_sub:		SEC		; decrease memeory counter by one
               LDA imagelow 	;image Address Low Byte
               SBC #$01
               STA imagelow 	;image Address Low Byte 
               BCS L_0568
               DEC imagehi
	
L_0568:		
		RTS


; ---- animation Code
;     

        	JSR L_store
La_0519:	LDA imagecount		; 11 images to display one after other
                STA imagecounta		; image counter
		LDA #$FF	
		STA icounter
La_051D:        INC icounter
La_052C:        LDX icounter   ;load the actual image number we are to display            
               LDA imageaddrlow,X	; get the low byte of the image address in memory
               STA imagelow 	; low byte of image address
               LDA imageaddrhi,X	; get high byte of image address in memory
               STA imagehi		; high byte of image address
               LDA #$00		; set low byte for screen ram to 0
		TAY
		TAX
		STA screenlow 	; set low byte for screen ram
	       STA screencount  ; offset that will be added to base 1K address to set high byte for that ram block
               LDA #$19		; set number of lines to display on screen = 25 in this case
               STA nolines    	; no lines to display = 25
	
                LDA (imagelow),Y ; get low byte of image and test if 0
	        CMP #$FF		; is first chara a blank 
                BEQ La_0550		; no so draw it on screen
		;LDX #$00
		BNE La_0554

La_0550:	JSR imageinc		; inc image pointer to see if next is FF or a no
	        LDA (imagelow),Y ; get low byte of image and test if 0
		TAX
		CMP #$FF		; set in X as used to say how many to display
		BNE La_0555
La_0554:	JSR L_sub		; dec image pointer as next is FF

La_0555:	INX
La_0556:        LDA #$00
               STA mstore 	; counter for number of bits - lines in character block = 8

La_055A:        LDY #$27		; set no characters to display in row to 40
	       STX xstore	; save X 
	       LDX mstore	; load counter for number of bits in line
	       LDA SCREENP,X	; get supersoft memory high byte address
	       CLC 
               ADC screencount	; add offset
               STA screenhigh 	; set high screen byte
	       LDX xstore	; restore x

La_0561:        LDA #$FF		; load blank - supersoft this is FF

La_0563:       DEX		; image is compressed. So if x = 0 the dex will set it to 255 and display 255 characters 		
               BEQ La_05A7	; if x is zero then display real character
               STA (screenlow),Y ;store blank in screen ram at address
               DEY		; decrease character by 1
               BPL La_0563	; if still > 0 then repeat

La_056B:        INC mstore 	; increase to next bit
               LDA #$08		; check displayed 8 bits
               CMP mstore 	; have we done 8 lines (bits)
               BNE La_055A  	; repeat for next bit if not displayed all 8
               CLC
               LDA #$28		; increase screen ram low byte by 40
               ADC screenlow 	; add 40 to low byte of screen ram
               STA screenlow 	; save back to memory	
               BCC La_057E	; if gone over FF then set high byte counter
               INC screencount 	; increase high byte offset from base high byte address (1K)

La_057E:        DEC nolines    	; have we displayed 25 lines. if not then display more
               BNE La_0556

La_0591:       
 		DEC imagecounta	; decrease image counter by 1 

		BPL La_051D	; display next image on screen

	       LDA #$FF
    		CMP $97          ;Current Key Pressed: 255 = No Key
     		BNE La_05A0
		JMP La_0519
La_05A0:               JSR L_restore		
               RTS

La_05A7:        INX		; routine to display an actual bit on screen

La_05A8:        JSR imageinc
La_05B1:	STY bcount 	; save Y
                LDY #$00


La_05B2:        LDA (imagelow),Y ; image Address Low Byte 
                CMP #$FF
		BEQ La_05BF

		LDY bcount 	; restore Y which is character count              
La_05B3:        STA (screenlow),Y ;save bit on screen
                DEY		; reduce Y by 1
      		 
		BPL La_05A8	; repeat if more characters to display
                BMI La_056B	; go back to next line in byte

La_05BF:        JSR imageinc

La_05C5:      
		LDA (imagelow),Y ; image Address  High Byte
		
La_05C6:        LDY bcount 
                TAX
		INX

	        CMP #$FF 
                BNE La_0561	; display next bit/kline in character block
		
		JSR L_sub
		JMP La_0561


L_store:	LDA imagelow ;USR Function Jump Instr (4C);
		STA $03FA
	     	LDA imagehi		;USR Address Low Byte / High Byte
		STA $03FB
		LDA screenlow
		STA $03FC
		LDA screenhigh
		STA $03FD

		RTS

L_restore:

		LDA $03FA
	     	STA imagelow	;USR Address Low Byte / High Byte

		LDA $03FB
		STA imagehi

		LDA $03FC
		STA screenlow

		LDA $03FD
		STA screenhigh


		RTS
SCREENP
!BYTE $90,$94,$98,$9C    ; high byte for 1K blocks on screen ram on supersoft
!BYTE $A0,$A4,$A8,$AC


