; This code is split into three sections and is designed for the 64k addon board only.
; (1)	Compress Blanks-as majority bit will be a blank rather than an image bit.
; (2) 	Cycle through images
; (3)	Add-on Mon for the 64k addon board
; They were build as separte pieces of code and merged 
; so there is scope for some variable optimisation.
; The compression routine works as follows. Each Character block is an 8 x 8 bit matrix for the SS board.
; the horizontal bits (8) and vertical bits (8) make up the single byte stored in the SS memory. 
; The screen is 40 columns so there are 40 bytes per line The vertical rows
; are split according to memory address. $9000, $9400, $9800 etc (8 of them)
; so the code startes from RIGHT to LEFT and works in a unidirection mode counting sequention FF's which are blanks. 
; When 255 are counted then FF gets written to memory, the counter is reset and starts again. If another 255 are counted the the cycle is repeated.
; If less than 255 are counted then FF followed by the no of FF's counted are written to memory. The cycle of FF counts must be terminated by FF 
; and no of FF counted even if it is 0. eg FF FF FF 05 (here we have counted 2 x 255 FF's and 5 FF, or FF FF FF 00 (here we counted only 2 x 255 FF's ... and terminated with FF 00)
; 
; Setup:- image count is stored at $7000. the low address byte of the compressed image at $7001, image count and high byte at $7031, image count
; These must be set before calling the compression routing for the first time
;
; ---- Setup variable locations
;

imagelow = $01   		; image storage low byte address
imagehi=$02			; image sgtorage high byte address
screenlow = $04   		; HR-40 memory low byte address
screenhigh  = $05   		; HR-40 memory high byte address
nolines= $027A   		; no lines displayed
screencount= $027B   		; screen count used to set hr-40high byte address as page through memory banks
ycount = $027C   		; Tempstorage for Y
imagecounta = $027D		; temp image count for animation routine
ffcount = $027E   		; Counter for no blanks
screenbyte=$027F   		; Read character 
lastff=$0282			; used to check if last bytes writen were a batch of FF's = 255
imagecount =$7F00		; number of images compressed
imageaddrlow=$7F01		; image address low byte
imageaddrhi=$7F40		; image high byte
icounter=$ff
memcode=$0283			; addon board memory bank


mstore  = $0280   		; temp store 
bcount = $0284  		; bit counter
xstore= $0281   		; temp x store 




!TO "animah.prg",cbm


*=$7600

;---- This is the compression routine
;---- Code
;     
	       JSR L_store		; save contents of zero page ram as approprite
	       SEI			; stop interupts

L_051D:        LDA #$00			; turn off64k addon board 
	       STA ffcount		; zero FF blank count
	       STA lastff
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

	       STA screenbyte   	; image byte pixel store
              
               LDA #$19			; set number of lines to display on screen = 25 in this case
               STA nolines    		; no lines to display = 25 but can be reduced if want to speed up animation
               	        

L_055A:        LDY #$27			; set no characters to display in row to 40

	       LDA SCREENP,X		; get supersoft memory high byte address
	       CLC 
               ADC screencount		; add offset
               STA screenhigh 		; set high screen byte

L_0561:        	LDA #%00000000		; turn off 64k board
		STA $FFF0
		LDA (screenlow),Y	; get character from image
	       STA screenbyte		; store pixel value

L_0561a:      STY ycount		; store y pointer to image pixel
		LDY #$00		; zero Y
L_0563:        CMP #$FF			; if blank then inc count and get next byte
               BNE L_0567		; if not blank then check if already counted blanks

	       INC ffcount		; inc blank counter by 1
		
L_0565:        LDA #$FF			; test if 255 - if so the write to memory
	       
		CMP ffcount		; check how many FF's counted. 
	    	BNE L_0575

		LDA memcode		; enable 64K board memory bank
  		STA $FFF0

		LDA #$FF		; save 255 blanks counted to image map
		BNE L_0574


L_0567:         ; pixel is not a blank so store any counted blanks and then the pixel value
		LDA memcode		; enable banks on 64K board
  		STA $FFF0
		LDA #$00		; have we counted any FF's
	        CMP ffcount		
	        BNE L_0570		; yes we have counted any FF's so write to memory first


		LDA lastff		; check if last byte written is FF as need to terminate count properly
		CMP #$FF		; have to write FF 00 otherwise character will be interpreted

L_0569:
		BNE L_0572

L_0570:        	LDA #$FF		; so write FF to memory
		STA (imagelow),y
		JSR imageinc

		LDA ffcount		; then write number of FF's
		STA (imagelow),Y
		JSR imageinc
	
L_0572:	       	LDA screenbyte		; get screen byte
L_0574:		STA (imagelow),y  	; store pixel byte and increase memory store by 1
		STA lastff		; save if FF written or not	 
	       LDA #$00			; reset FF count
	       STA ffcount

 	       JSR imageinc		; increase mem counter by one

L_0575:		CLC
		LDY ycount
		DEY			; have we done 40 pixels. if not do more
		BPL L_0561


L_057E:		INX			; go to next image map (1K) 
		CPX #$08		; have we done all 8 K blocks

		BNE L_055A
		
L_057F:          ; done 40 bytes (8 bits per byte) so do next row of pixels
                LDA #$28		; increase screen ram low byte by 40
	        CLC 
                ADC screenlow 		; add 40 to low byte of screen ram
                STA screenlow 		; save back to memory	
                BCC L_0580		; if gone over FF then set high byte counter
                INC screencount 	; increase high byte offset from base high byte address (1K)

L_0580:	       
		LDA #$00
		TAX
		DEC nolines		; how many lines have we processed
		BEQ L_0585		; if less than 25 lines then do next line otherwise stop
		JMP L_055A		; process next bit memory bank in row
L_0585:		LDA memcode		; enable 0 & 2 on 64K board
  		STA $FFF0
		LDY #$00		; finished
	        TYA			; have we counted any FF's
	        CMP ffcount		
	        BEQ L_0586		; no we havent counted any FF's so write pixel to memory

	
		LDA #$FF		; have we written FF already to memory on last write
		STA (imagelow),y
		JSR imageinc		
		LDA ffcount		; save count number to memory
		STA (imagelow),y
		JSR imageinc

L_0586	     	LDA screenbyte		; get screen byte
		CMP #$FF
		BEQ L_finish
	        STA (imagelow),y  	; store pixel byte and increase memory store by 1
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
		LDA #%00000000		; turn off 64k board
		STA $FFF0
		CLI
		JSR L_restore

		RTS


imageinc:      	INC imagelow 		; image Address Low Byte 
               	BNE incimage2
               	INC imagehi
	       	LDA imagehi		; add-mon uses IRQ/NMI and sets address points in Expansion memory
	       	CMP #$FF		; for speed of animation when get to $FF00 then move to next banks rather than test both low and high bytes
	       	BEQ incmemcode		; if gone over $FFFF then switch to next memory bank 1/3
incimage2:     	RTS

incmemcode	LDA #%10001100		; switch memory ban to 1/3
		STA memcode
		STA $FFF0

		LDA #$80
		STA imagehi

		LDA #$00		; set image low and high byte addresses for next bank
		STA imagelow

		RTS

; ---- animation Code
;     

        	JSR L_store
La_0519:	SEI		
		LDA #%10000000		; enable bank 0/2
		STA memcode

		LDA imagecount		; 11 images to display one after other
                STA imagecounta		; image counter - no images to cycle
		LDA #$FF	
		STA icounter
La_051D:        INC icounter
La_052C:        LDX icounter   		; load the actual image number we are to display            
               LDA imageaddrlow,X	; get the low byte of the image address in memory
               STA imagelow 		; low byte of image address
               LDA imageaddrhi,X	; get high byte of image address in memory
               STA imagehi		; high byte of image address
               LDA #$00			; set low byte for screen ram to 0
	    	STA lastff
		TAY
		TAX
		STA screenlow 		; set low byte for screen ram
	        STA screencount  	; offset that will be added to base 1K address to set high byte for that ram block
                LDA #$19		; set number of lines to display on screen = 25 in this case
                STA nolines    		; no lines to display = 25
		LDA memcode		; enable 0 & 2 on 64K board
		STA $FFF0

                LDA (imagelow),Y 	; get low byte of image and test if 0
	        CMP #$FF		; is first chara a blank 
		BNE La_0555

La_0550:	JSR imageinc		; inc image pointer to see if next is FF or a no
	        LDA (imagelow),Y 	; get low byte of image and test if 0
		TAX

La_0555:	STA lastff		; dec image pointer as next is FF

La_0555n:	INX
La_0556:        LDA #$00
               STA mstore 		; counter for number of bits - lines in character block = 8

La_055A:        LDY #$27		; set no characters to display in row to 40
	       STX xstore		; save X 
	       LDX mstore		; load counter for number of bits in line
	       LDA SCREENP,X		; get supersoft memory high byte address
	       CLC 
               ADC screencount		; add offset
               STA screenhigh 		; set high screen byte
	       LDX xstore		; restore x

La_0561:        LDA #%00000000		; disable 0 & 2 on 64K board
	       STA $FFF0
		LDA #$FF		; load blank - supersoft this is FF

La_0563:       DEX			; image is compressed. So if x = 0 the dex will set it to 255 and display 255 characters 		
               BEQ La_05A7		; if x is zero then display real character
               STA (screenlow),Y 	;store blank in screen ram at address
               DEY			; decrease character by 1
               BPL La_0563		; if still > 0 then repeat

La_056B:    ; 	STA lastff
		INC mstore 		; increase to next bit
               LDA #$08			; check displayed 8 bits
               CMP mstore 		; have we done 8 lines (bits)
               BNE La_055A  		; repeat for next bit if not displayed all 8
               CLC
               LDA #$28			; increase screen ram low byte by 40
               ADC screenlow 		; add 40 to low byte of screen ram
               STA screenlow 		; save back to memory	
               BCC La_057E		; if gone over FF then set high byte counter
               INC screencount 		; increase high byte offset from base high byte address (1K)

La_057E:        DEC nolines    		; have we displayed 25 lines. if not then display more
               BNE La_0556

La_0591:       
 		DEC imagecounta		; decrease image counter by 1 

		BPL La_051D		; display next image on screen
		LDA #%00000000		; disable  64K board
		STA $FFF0
		CLI
	       LDA #$FF
    		CMP $97          	;Current Key Pressed: 255 = No Key
     		BNE La_05A0
		JMP La_0519

La_05A0:        JSR L_restore		
               RTS

La_05A7:        INX
		LDA lastff
		CMP #$FF
		BEQ La_05B1		; routine to display an actual bit on screen

La_05A8:        JSR imageinc
La_05B1:	STY bcount 		; save Y
                LDY #$00


La_05B2:        LDA memcode		; enable 64K board
	        STA $FFF0
		LDA (imagelow),Y 	; image Address Low Byte 
                CMP #$FF		; is it a blankl
		BEQ La_05BF
		STA screenbyte

		LDA #%00000000		; disable 64K board
	        STA $FFF0
		LDA screenbyte
		LDY bcount 		; restore Y which is character count              
                STA (screenlow),Y 	; save bit on screen

                DEY			; reduce Y by 1
      		 
		BPL La_05A8		; repeat if more characters to display
                JMP La_056B		; go back to next line in byte

La_05BF:        JSR imageinc		; blank so get how many to write

La_05C5:    
		LDA (imagelow),Y 	; image Address  High Byte
		STA lastff		; save no FF's to write as if 255 then process next byte differently
La_05C6:        LDY bcount 
                TAX
		INX
		JMP La_0561


L_store:	LDA imagelow 		; store zero page
		STA $03FA
	     	LDA imagehi		
		STA $03FB
		LDA screenlow
		STA $03FC
		LDA screenhigh
		STA $03FD

		RTS

L_restore:

		LDA $03FA
	     	STA imagelow		; restore zero page

		LDA $03FB
		STA imagehi

		LDA $03FC
		STA screenlow

		LDA $03FD
		STA screenhigh


		RTS
SCREENP
!BYTE $90,$94,$98,$9C    		; high byte for 1K blocks on screen ram on supersoft
!BYTE $A0,$A4,$A8,$AC


;*****************************************
;*                                       *
;*  AAA  DDDD  DDDD       OOO  N   N     *
;* A   A D   D D   D     O   O NN  N     *
;* A   A D   D D   D *** O   O N N N     *
;* AAAAA D   D D   D *** O   O N N N     *
;* A   A D   D D   D     O   O N  NN     *
;* A   A DDDD  DDDD       OOO  N   N     *
;*                                       *
;*       M   M  OOO  N   N               *
;*       MM MM O   O NN  N               *
;*  ***  M M M O   O N N N               *
;*  ***  M   M O   O N N N               *
;*       M   M O   O N  NN               *
;*       M   M  OOO  N   N               *
;*                                       *
;****************************************


;*****************************************
;*                                       *
;*  64K-ADD-ON MONITOR                   *
;*                                       *
;*  THIS PROGRAM ALLOWS A USER TO        *
;*  EXAMINE, MODIFY, AND RUN PROGRAMS    *
;*  WITH BREAKPOINTS IN THE 64K ADD-ON   *
;*  MEMORY.  IT IS FUNCTIONALLY THE SAME *
;*  AS THE PET RESIDENT MONITOR.         *
;*                                       *
;*  29JAN81 RJF         29JUL81 RJF      *
;*                                       *
;*  CBM PART # 118005 REV B              *
;*                                       *
;*****************************************

; Modified by Andy Grady Feb 2018 
; fixed issue where start addrress ignored
; Added X exit command though sometimes syntax errors for some reason
;****************************************



;
;VIRTUAL REGISTERS
;

	PCH = $00
	PCL = $01
	FLGS = $02 	;PROCESSOR STATUS
	ACC = $03		;ACCUMULATOR
	XR = $04		;X INDEX
	YR = $05 		;Y INDEX
	SP = $06		;STACK POINTER
	INVH = $07	;USER  IRQ VECTOR
	INVL = $08
	MEMMAP =$09	;ADD-ON CONTROL REG
;
;INDIRECT POINTERS
;
	STAL = $0A  ;SAVE STORE POINTER
	STAH =$0B
;
	SAL =$0C
	SAH =$0D
;
	EAL =$0E
	EAH =$0F
;
	TMP0 =$10 ;MONITOR INDIRECTS
;
	TMP2 =$12
;
;WORKING VARIABLES
;
	TMPC =$14
	SAVX =$15
	WRAP =$16 ;WRAP FLAG FOR DISPLYM
;
	TMPA =$17 ;.A SAVE FOR IRQ
	TMPPS =$18 ;.P SAVE FOR IRQ
;
	CINV =$90
	CBINV =$92
	BUF =$200
	FNADR =$DA
	FNLEN =$D1
	CR =$D
	BAD =$100
	STATUS =$96
	FA =$D4
	SA =$D3

	RCLRCH =$F2A6
	RLISTN =$F0D5
	RSECND =$F143
	RCIOUT =$F19E
	RUNLSN =$F1B9
	RTALK =$F0D2
	RTKSA =$F193
	RACPTR =$F1C0
	RUNTLK =$F1AE
	ROPENI =$F4A5
	STKEY =$9B
	NDX =$9E


; jump to basic

	basicready = $B3FF

; Assembler Ouput

;ML Start
;
;INITIALIZE INTERRUPT PROCESS
; save zero page values 
;so can restore on exit
;
ZSAVE	LDX #$00
ZSAVE1	LDA $00,X
	STA ZPAGE,X
	INX
	CPX #$19
	BNE ZSAVE1
	JSR INIT
	LDA MEMMAP
	STA $FFF0
	JMP TIMC

INIT 	SEI
	LDA #%10000000
	STA MEMMAP
	STA $FFF0
	JSR SETIRQ
	LDA #%10001000
	STA $FFF0
	JSR SETIRQ
	LDA #%00000000
	STA $FFF0
	CLI
	RTS

SETIRQ 	LDA #<IRQ
	STA $FFFE
	LDA #>IRQ
	STA $FFFF
	LDA #<NMI
	STA $FFFA
	LDA #>NMI
	STA $FFFB
	RTS
	 
;PROCESS IRQ
;
IRQ 	STA TMPA ;PRESERVE .A
;
	PLA
	PHA
	STA TMPPS
;
	LDA #%00000000
	STA $FFF0
;
;PUSH RETURN FROM INTERRUPT ADDRESS
;
	LDA #>RTIP
	PHA
	LDA #<RTIP
	PHA
;
	LDA TMPPS
	PHA ;PUSH DUMMY STATUS
;
	LDA TMPA ;RESTORE .A
;
;GO TO ROM IRQ SERVICE
;
	JMP ($FFFE)
;PROCESS NMI
;
;PROCESS NMI
;
NMI 	STA TMPA ;PRESERVE .A
;
	PLA
	PHA
	STA TMPPS
;
;
	LDA #%00000000
	STA $FFF0
;
;PUSH RETURN FROM INTERRUPT ADDRESS
;
	LDA #>RTIP
	PHA
	LDA #<RTIP
	PHA
;
	LDA TMPPS
	PHA ;PUSH DUMMY STATUS
;
	LDA TMPA
;
;GO TO ROM IRQ SERVICE
;
	JMP ($FFFA)
	
;RETURN FROM INTERRUPT PROCESS
;
RTIP 	PHA               
;
;MAP BACK TO ORIGINAL RAM
;
	LDA MEMMAP
	STA $FFF0
;
;RESTORE OLD .A
;
	PLA
;
;BACK TO USER
;
	RTI
	
;************************************************
;*                                              *
;* KERNAL MONITOR                               *
;*                                              *
;* ENTRY VIA CALL (JMP) OR BREAKPOINT (BRK)     *
;* ---FUNCTIONS---                              *
;* <:>      ALTER MEMORY                        *
;* <;>      ALTER REGISTERS                     *
;* <R>      DISPLAY REGISTERS                   *
;* <M>      DISPLAY MEMORY                      *
;* <G>      START EXECUTION OF CODE             *
;* <L>      LOAD MEMORY                         *
;* <S>      SAVE MEMORY                         *
;* <@>      DISK COMMAND                        *
;* <*>      ADD-ON CONTROL REGISTER             *
;* <X>      Exit to basic ... added by A Grady  *
;* <OTHER>  LOAD AND EXECUTE FROM DISK          *
;*                                              *
;* FOR SYNTAX & SEMANTICS SEE CBM KERNAL MANUAL *
;* COPYRIGHT (C) 1980 BY CBM                    *
;************************************************



;*****CALL ENTRY*****
;
TIMC
	LDA #<TIMB
	STA CBINV
	LDA #>TIMB
	STA CBINV+1
	LDA #MS34-MS1 ;CALL ENTRY
	STA TMPC
	BNE B3 ;BRANCH ALWAYS
;
;*****BREAK ENTRY*****
;
TIMB 	JSR CLRCH ;CLR CHANNELS
	LDA #MS36-MS1 ;BREAK ENTRY
	STA TMPC
	CLD 
;
;SAVE .Y,.X,.A,FLAGS, AND PC
;
	LDX #5
B1 	PLA
	STA PCH,X
	DEX
	BPL B1
;
B3 	LDA CINV
	STA INVL 	;SAVE IRQ LOW
	LDA CINV+1
	STA INVH 	;SAVE IRQ HIGH
;
	TSX
	STX SP 		;SAVE ORIGINAL SP
	CLI 		;CLEAR INTS
;
B5 	LDY TMPC 	;MESSAGE CODE
	JSR MSG 	;PRINT BREAK/CALL
;
	LDA #"R" 	;DISPLAY REGS ON ENTRY
	BNE S0 		;BRANCH ALWAYS
;
;*****ERROR ENTRY*****
;
ERROPR 	JSR OUTQST
	PLA
	PLA
;
;*****COMMAND INTERPRETER ENTRY*****
;
	STRTM1=*-1
	LDX #<BUF  ;PLACE TO PUT FILE NAME
	LDY #>BUF
	STX FNADR
	STY FNADR+1
	JSR CRLF
;
ST1 	JSR BASIN ;READ COMMAND
	CMP #$20
	BEQ ST1 ;SPAN BLANKS
;
;COMMAND INTERPRETER
;
S0 	LDX #0
	STX WRAP
	STX FNLEN
	TAY ;SAVE CURRENT COMMAND
;
;PUT RETURN ADDRESS FOR COMMANDS ON STACK
;
	LDA #>STRTM1
	PHA
	LDA #<STRTM1
	PHA
;
	TYA ;CURRENT COMMAND IN .A
;
S1 	CMP CMDS,X  ;IS IT THIS ONE?
	BNE S2 ;NOTIT
;
	STA SAVX ;SAVE CURRENT COMMAND
;
;INDIRECT JMP FROM TABLE
;
	LDA CMDS+1,X
	STA TMP0
	LDA CMDS+2,X
	STA TMP0+1
	JMP (TMP0)
;
;EACH TABLE ENTRY IS 3 LONG---SKIP TO NEXT
;
S2 	INX
	INX
	INX
	CPX #CMDEND-CMDS
	BCC S1 ;LOOP FOR ALL COMMANDS
;
;COMMAND NOT IN TABLE...LOOK ON DISK.
;COMMAND NAME CAN BE ANY LENGTH AND
;HAVE PARAMETERS.
;
	LDX #0 ;LENGTH TO ZERO
S3 	CMP #$D ;END OF NAME?
	BEQ S4 ;YES...
	CMP #$20 ;BLANK?
	BEQ S4 ;YES
	STA BUF,X
	JSR BASIN ;GET NEXT
	INX ;COUNT CHAR
	BNE S3 ;AND CONTINUE
;
S4 	STA TMPC
	TXA ;COUNT
	BEQ S6 ;IS ZERO
;
	STA FNLEN
	LDA #8
	STA FA ;WILL USE DEVICE #8
	JSR LOAD ;TRY TO LOAD COMMAND
	BCS S6 ;BAD LOAD...
;
	LDA TMPC ;PASS LAST CHARACTER
	JMP (STAL) ;GO DO IT
;
S6   	RTS
CMDS 	!BYTE ":" ;ALTER MEMORY
	!WORD ALTM
	!BYTE ";" ;ALTER REGISTERS
	!WORD ALTR
	!BYTE "R" ;DISPLAY REGISTERS
	!WORD DSPLYR
	!BYTE "M" ;DISPLAY MEMORY
	!WORD DSPLYM
	!BYTE "G" ;START EXECUTION
	!WORD GO
	!BYTE "L" ;LOAD MEMORY
	!WORD LD
	!BYTE "S" ;SAVE MEMORY
	!WORD LD
	!BYTE "*" ;MAP MEMORY
	!WORD MAPPER
	!BYTE "@" ;DISK COMMAND (ALTERNATE)
	!WORD DISK
	!BYTE "X" ;Exit MEMORY
	!WORD EXIT
CMDEND
PUTP 	LDA TMP0 ;MOVE TMP0 TO PCH,PCL
	STA PCL
	LDA TMP0+1
	STA PCH
	RTS

SETR 	LDA #<FLGS ;SET TO ACCESS REGS
	STA TMP0
	LDA #>FLGS
	STA TMP0+1
	LDA #5
	RTS

;PRINTS ':' OR ';' BEFORE DATA TO PERMIT
;ALTER AFTER 'M' OR 'R' COMMAND
;
ALTRIT PHA ;PRESERVE ALTER CHARACTER
		JSR CRLF
		PLA
		JSR BSOUT
SPACE 	LDA #$20 ;OUTPUT A SPACE
		!BYTE $2C ;SKIP TWO BYTES

OUTQST 	LDA #"?" ;OUTPUT QUESTION
		!BYTE $2C ;SKIP TWO BYTES

CRLF 		LDA #$D ;DO CARRIAGE RETURN
		JMP BSOUT

;DATA FOR REGISTER DISPLAY HEADING
;
REGK 	!BYTE CR,$20,$20 ;3 SPACES
	!PET " pc "," irq "," sr ac xr yr sp"

;DISPLAY REGISTER FUNCTION

;

DSPLYR 	LDX #0
D2 		LDA REGK,X
		JSR BSOUT ;PRINT HEADING
		INX
		CPX #DSPLYR-REGK ;MAX LENGTH
		BNE D2
		LDA #";"
		JSR ALTRIT ;ALLOW ALTER AFTER DISPLAY
		LDX PCH
		LDY PCL
		JSR WROA  ;PRINT PROGRAM COUNTER
		JSR SPACE
		LDX INVH
		LDY INVL
		JSR WROA ;PRINT IRQ VECTOR
		JSR SETR ;SET TO PRINT .P,.A,.X,.Y,.S
;
;DISPLAY MEMORY SUBROUTINE
;

DM 	STA TMPC ;BYTE COUNT
	LDY #0 ;INDIRECT INDEX
DM1 	JSR SPACE ;SPACE TWEEN BYTES
	LDA (TMP0),Y
	JSR WROB ;WRITE BYTE OF MEMORY
;
;INCREMENT INDIRECT
;
	INC TMP0
	BNE DM2
	INC TMP0+1
	BNE DM2
	INC WRAP
;
DM2 	DEC TMPC ;COUNT BYTES
	BNE DM1 ;UNTIL ZERO
	RTS

;DISPLAY MEMORY FUNCTION
;
DSPLYM JSR RDOA ;READ START ADR
	BCS ERRS1 ;ERR IF NO SA
	JSR T2T2 ;SA TO TMP2
;
;ALLOW USER TO TYPE JUST ONE ADDRESS
;
	JSR RDOA ;READ END ADR
	BCC DSP10 ;GOOD...NO DEFAULT
;
	LDA TMP2
	STA TMP0 ;DEFAULT LOW BYTE
	LDA TMP2+1
	STA TMP0+1 ;DEFAULT HI BYTE
;
DSP10 JSR T2T2 ;SA TO TMP0, EA TO TMP2
DSP1 JSR STOP ;STOP KEY?
	BEQ BEQS1 ;YES...BREAK LIST
;
	LDA #":"
	JSR ALTRIT ;ALLOW ALTER
	LDX TMP0+1
	LDY TMP0
	JSR WROA ;WRITE START ADDRESS
	LDA #8 ;COUNT OF BYTES
	JSR DM ;DISPLAY BYTES
	LDA WRAP
	BNE BEQS1
;
;CHECK FOR END OF DISPLAY
;

	SEC
	LDA TMP2
	SBC TMP0
	LDA TMP2+1
	SBC TMP0+1
	BCS DSP1  ;END >= START
;
BEQS1 	RTS 		;A.O.K. EXIT
;
ERRS1 	JMP ERROPR ;SYNTAX ERROR
;ALTER REGISTER FUNCTION
;
ALTR JSR RDOA ;READ NEW PC
	BCS ERRS1 ;ERROR...NO ADDRESS
;
	JSR PUTP ;ALTER PC
;
	JSR RDOA ;READ NEW IRQ
	BCS ERRS1 ;ERROR...NO ADDRESS
;
	LDA TMP0
	STA INVL ;ALTER IRQ VECTOR
	LDA TMP0+1
	STA INVH
;
	JSR SETR ;SET TO ALTER R'S
	BNE A4 ;BRANCH ALWAYS

;ALTER MEMORY - READ ADR AND DATA
;
ALTM 	JSR RDOA ;READ ALTER ADR
	BCS ERRS1 ;IF SPACE,ERR
;
	LDA #8 ;ALLOW 8 BYTES CHANGE
;
;COMMON CODE FOR ':' AND ';'
;
A4 	STA TMPC ;NUMBER OF BYTES TO CHANGE
;

A5 	JSR RDOB ;READ BYTE
	BCS A9 ;NONE...END OF LINE
;
	LDX #0
	STA (TMP0,X) ;STORE IT AWAY
;
;INCREMENT STORE ADDRESS
;
	INC TMP0
	BNE A6
	INC TMP0+1
;

A6 	DEC TMPC ;COUNT BYTE
	BNE A5 ;UNTIL ZERO
;
A9 	RTS

;MAP MEMORY
;

MAPPER JSR RDOB
	STA MEMMAP
	RTS

;START EXECUTION FUNCTION
;
GO 	JSR RDOC ;SEE IF DEFAULT
	BEQ G1 ;YES...PC IS ADDRESS
;
	JSR RDOA ;NO...GET NEW ADDR
	BCS ERRL ;ERROR...ADDRESS SCREWED UP
;
	JSR PUTP ;MOVE ADDR TO P.C.
;
G1 	LDX SP
	TXS ;ORIG OR NEW SP VALUE TO SP
;
	SEI ;PREVENT DISASTER
;
	LDA INVH
	STA CINV+1  ;SET UP IRQ VECTOR
	LDA INVL
	STA CINV
;
;GET FLAGS,PCH,PCL,.A,.X,.Y
;
	LDX #0
G2 	LDA PCH,X
	PHA ;EVERYBODY ON STACK
	INX
	CPX #6
	BNE G2
;
;INTERRUPT RETURN SETS EVERYBODY UP
;FROM DATA ON STACK
;
PREND PLA
	TAY
	PLA
	TAX
	PLA
	RTI

ERRL JMP ERROPR ;SYNTAX ERROR JUMP

;LOAD RAM FUNCTION
;
LD 	LDY #1
	STY FA ;DEFAULT DEVICE #1
	DEY ;.Y=0 TO COUNT NAME LENGTH
;

L1 	JSR RDOC ;DEFAULT?
	BEQ L5 ;YES...TRY LOAD
;
	CMP #$20
	BEQ L1 ;SPAN BLANKS
;
	CMP #$22 ;STRING NEXT?
L2 	BNE ERRL ;NO FILE NAME...
;
L3 	JSR RDOC ;GET CHARACTER OF NAME
	BEQ L5 ;END...ASSSUME LOAD
;
	CMP #$22 ;END OF STRING?
	BEQ L8 ;YES...COULD STILL BE 'L' OR 'S'
;
	STA (FNADR),Y ;STORE NAME
	INC FNLEN
	INY
	CPY #16 ;MAX FILE NAME LENGTH
;
L4 	BEQ ERRL ;FILE NAME TOO LONG
	BNE L3 ;BRANCH ALWAYS
;
;SEE IF WE GOT A LOAD
;
L5 	LDA SAVX ;GET LAST COMMAND
	CMP #"L"
	BNE L2 ;NO..NOT A LOAD..ERROR
;
	JMP LOAD ;YES...DO LOAD
;
L8 	JSR RDOC ;MORE STUFF?
	BEQ L5 ;NO...DEFUALT LOAD
;
	CMP #"," ;DELEIMETER?
L9 	BNE L2 ;NO...BAD SYNTAX
;
	JSR RDOB ;YES...GET NEXT PARM
	BCS L15 ;NOT GOOD
;
	STA FA
;
	JSR RDOC ;MORE PARMS?
	BEQ L5 ;NO...DEFAULT LOAD
;
	CMP #"," ;DELIMETER?
L12 	BNE L9 ;NO...BAD SYNTAX
;
	JSR RDOA ;START ADDRESS?
	BCS L15 ;NO...BAD
;
	LDA TMP0
	STA STAL
	LDA TMP0+1
	STA STAH
	JSR T2T2 ;PRESERVE START
	JSR BASIN ;DELIMETER?
	CMP #","
L13 	BNE L12 ;NO...
	JSR RDOA ;TRY TO READ END
	BCS L15 ;NONE...ERROR
;
;SET UP END SAVE ADDRESS
;
	LDA TMP0
	STA EAL
	LDA TMP0+1
	STA EAH
	JSR T2T2
;
L20 JSR BASIN
	CMP #$20
	BEQ L20 ;SPAN BLANKS
;
	CMP #CR
L14 	BNE L13 ;MISSING CR AT END
	LDA SAVX ;WAS COMMAND SAVE?
	CMP #"S"
	BNE L14 ;NO...LOAD CAN'T HAVE PARMS
	JMP SAVE
;
L15 	JMP ERROPR

;WRITE ADR FROM TMP0 STORES
;
WROA TXA ;HI-BYTE
	JSR WROB
	TYA ;LOW-BYTE

;WRITE BYTE --- A = BYTE
;UNPACK BYTE DATA INTO TWO ASCII
;CHARACTERS. A=BYTE; X,A=CHARS
WROB PHA
	LSR 
	LSR 
	LSR 
	LSR 
	JSR ASCII ;CONVERT TO ASCII
	TAX
	PLA
	AND #$0F

;CONVERT NYBBLE IN A TO ASCII AND
;PRINT IT
;
ASCII 	CLC
	ADC #$F6
	BCC ASC1
	ADC #$06
ASC1 	ADC #$3A
	JMP BSOUT

;EXCHANGE TEMPORARIES
;
T2T2 	LDX #2
T2T21 	LDA TMP0-1,X
	PHA
	LDA TMP2-1,X
	STA TMP0-1,X 
	PLA
	STA TMP2-1,X
	DEX
	BNE T2T21
	RTS

;READ HEX ADR,RETURN HI IN TMP0,
;LO IN TMP0+1,AND CY=1
;IF SP CY=0
;

RDOA 	JSR RDOB ;READ 2-CHAR BYTE
	BCS RDOA2 ;SPACE
	STA TMP0+1
	JSR RDOB
	STA TMP0
RDOA2 	RTS

;READ HEX BYTE AND RETURN IN A
;AND CY=0 IF SP CY=1
RDOB 	LDA #0 ;SPACE
	STA BAD ;READ NEXT CHAR
	JSR RDOC
	BEQ RDOB4 ;FAIL ON CR
	CMP #$20  ;BLANK?
	BEQ RDOB ;SPAN BLANKS...
;
	JSR HEXIT ;CONVERT TO HEX NYBBLE
	ASL 
	ASL 
	ASL 
	ASL 
	STA BAD
	JSR RDOC ;2ND CHAR ASSUMED HEX
	BEQ RDOB4 ;FAIL ON CR
	JSR HEXIT
	ORA BAD
;
RDOB4 	RTS

;CONVERT ASCII CHAR TO HEX NYBBLE
;
HEXIT CMP #$3A
	PHP ;SAVE FLAGS
	AND #$0F
	PLP
	BCC HEX09 ;0-9
	ADC #8 ;ALPHA ADD 8+CY=9
HEX09 RTS

;GET CHARACTER AND TEST FOR CR
;

RDOC JSR BASIN
	CMP #$0D ;IS IT A CR
	RTS ;RETURN WITH FLAGS

;SEND DISK COMMAND OR READ STATUS
;
DISK  LDA #0 ;CLEAR STATUS @ I/O BEGIN
	STA STATUS
;
	JSR RDOC ;SEE IF STATUS CHECK
	BEQ DISK20 ;YES
;
	PHA
	LDA #8 ;FLOPPY IS DEVICE #8
	JSR LISTN ;TELL FLOPPY TO RECEIVE
	LDA #15+$60
	JSR SECND ;ON COMMAND CHANNEL
;
	PLA
	LDX STATUS ;ERROR?
	BPL DISK15 ;NO...OK
;
DISK5 JMP ERROR5 ;DEVICE NOT PRESENT
;
DISK10 JSR BASIN ;GET A CHARACTER
DISK15 CMP #$D ;SEE IF END
	PHP ;SAVE FOR LATER
	JSR CIOUT ;OUT TO FLOPPY
	PLP ;END?
	BNE DISK10 ;NO...CONTINUE
;
	JMP UNLSN ;YES...FLOPPY DONE
;
DISK20 JSR CRLF
	LDA #8 ;FLOPPY IS DEVICE #8
	JSR TALK ;TELL FLOPPY TO SPEAK
	LDA #15+$60
	JSR TKSA ;FROM ERROR CHANNEL
;
	LDA STATUS ;AN ERROR?
	BMI DISK5 ;YES...
;
DISK25 JSR ACPTR ;GET A CHARACTER
	CMP #$D ;SEE IF END
	PHP ;TEST LATER
	JSR BSOUT ;OUT TO SCREEN
	PLP ;END?
	BNE DISK25 ;NO...
	JMP UNTLK ;YES...FLOPPY DONE

;**********************************
;* LOAD RAM FUNCTION              *
;*                                *
;* LOADS FROM CASSETTE 1 OR 2, OR *
;* IEEE BUS DEVICES >=4 TO 31 AS  *
;* DETERMINED BY CONTENTS OF      *
;* VARIABLE FA. VERIFY FLAG IN .A *
;* HIGH LOAD RETURN IN X,Y.       *
;* .A=0 PERFORMS LOAD,<> IS VERIFY*
;*                                *
;**********************************

LOAD
	LDA #0
	STA STATUS
;
	LDA FA ;CHECK DEVICE NUMBER
	CMP #4
	BCS LD20
;
LD10 JMP ERROR9 ;BAD DEVICE #
;
LD20
;
;LOAD FROM CBM IEEE DEVICE
;
	LDA #$60 ;SPECIAL LOAD COMMAND
	STA SA
;
	LDY FNLEN ;MUST HAVE FILE NAME
	BNE LD25 ;YES...OK
;
	JMP ERROR8 ;MISSING FILE NAME
;
LD25 JSR LUKING ;TELL USER LOOKING
	JSR OPENI ;OPEN THE FILE
;
	LDA FA
	JSR TALK ;ESTABLISH THE CHANNEL
	LDA SA
	JSR TKSA ;TELL IT TO LOAD
;
	JSR ACPTR ;GET FIRST BYTE
	STA EAL
	STA STAL
;
	LDA STATUS ;TEST STATUS FOR ERROR
	LSR 
	LSR 
	BCS LD90 ;FILE NOT FOUND...
	JSR ACPTR
	STA EAH
	STA STAH
;
	JSR LODING ;TELL USER LOADING
;
LD40 	LDA #$FD ;MASK OFF TIMEOUT
	AND STATUS
	STA STATUS
;
	JSR STOP ;STOP KEY?
	BNE LD45 ;NO...
;
	JMP BREAK ;STOP KEY PRESSED
;
LD45 	JSR ACPTR ;GET BYTE OFF IEEE
	TAX
	LDA STATUS ;WAS THERE A TIMEOUT?
	LSR 
	LSR 
	BCS LD40 ;YES...TRY AGAIN
	TXA
	LDY #0
	STA (EAL),Y
LD60 	INC EAL ;INCREMENT STORE ADDR
	BNE LD64
	INC EAH
LD64 	BIT STATUS ;EOI?
	BVC LD40 ;NO...CONTINUE LOAD
;
	JSR UNTLK ;CLOSE CHANNEL
	JSR CLSEI ;CLOSE THE FILE
	BCC LD180 ;BRANCH ALWAYS
;
LD90 	JMP ERROR4 ;FILE NOT FOUND
;
;
LD180 CLC ;GOOD EXIT
;
;SET UP END LOAD ADDRESS
;
	LDX EAL
	LDY EAH
;
LD190 	RTS

;SUBROUTINE TO PRINT TO CONSOLE:
;
;SEARCHING [FOR NAME]
;
LUKING
	LDY #MS5-MS1 ;"SEARCHING"
	JSR MSG
	LDA FNLEN
	BEQ LD115
	LDY #MS6-MS1 ;"FOR"
	JSR MSG
	
;SUBROUTINE TO OUTPUT FILE NAME
;
OUTFN
   	LDY #0
LD110 	LDA (FNADR),Y
	JSR BSOUT
	INY
	CPY FNLEN
	BNE LD110
;
LD115 RTS

;SUBROUTINE TO PRINT:
;
;LOADING/VERIFING
;
LODING 	LDY #MS10-MS1 ;ASSUME 'LOADING'
LD410 	JMP SPMSG

;***********************************
;* SAVE                            *
;*                                 *
;* SAVES TO CASSETTE 1 OR 2, OR    *
;* IEEE DEVICES 4>=N>=31 AS SELECT-*
;* ED BY VARIABLE FA.              *
;*                                 *
;*START OF SAVE IS MEMSTR...END OF*
;*SAVE IS .X,.Y                   *
;***********************************

SAVE 	LDA FA  ;***MONITOR ENTRY
	CMP #4
	BCS SV20
;
SV10 	JMP ERROR9 ;BAD DEVICE #
;
SV20
	LDA #$61
	STA SA
	LDY FNLEN
	BNE SV25
;
	JMP ERROR8 ;MISSING FILE NAME
;
SV25 JSR OPENI
	JSR SAVING
	LDA FA
	JSR LISTN
	LDA SA
	JSR SECND
	LDY #0
	LDA STAH
	STA SAH
	LDA STAL
	STA SAL
	JSR CIOUT
	LDA SAH
	JSR CIOUT
SV30 	SEC
	LDA SAL
	SBC EAL
	LDA SAH
	SBC EAH
	BCS SV50 ;HAVE REACHED END
	LDA (SAL),Y
	JSR CIOUT
	JSR STOP
	BNE SV40
;
BREAK 	JSR CLSEI
	LDA #0
	SEC
	RTS
;
SV40 	INC SAL
	BNE INCR
	INC SAH
INCR
	BNE SV30
SV50 	JSR UNLSN
CLSEI 	BIT SA
	BMI CLSEI2
	LDA FA
	JSR LISTN
	LDA SA
	AND #$EF
	ORA #$E0
	JSR SECND
	JSR UNLSN
;
CLSEI2 	CLC
	RTS

;SUBROUTINE TO OUTPUT:
;'SAVING <FILE NAME>'
;
SAVING
;
	LDY #MS11-MS1 ;'SAVING'
	JSR MSG
	JSR OUTFN ;<FILE NAME>
;
SAV100 	RTS
	
;***************************************
;* STOP -- CHECK STOP KEY FLAG AND     *
;* RETURN Z FLAG SET IF FLAG TRUE.     *
;* ALSO CLOSES ACTIVE CHANNELS AND     *
;* FLUSHES KEYBOARD QUEUE.             *
;* ALSO RETURNS KEY DOWNS FROM LAST    *
;* KEYBOARD ROW IN .A.                 *
;***************************************

STOP 	LDA STKEY ;VALUE OF LAST ROW
	CMP #$EF ;CHECK STOP KEY POSITION
	BNE STOP2 ;NOT DOWN
	PHP
	JSR CLRCH ;CLEAR CHANNELS
	LDA #0
	STA NDX ;FLUSH QUEUE
	PLP
STOP2 	RTS

;************************************
;*                                  *
;* ERROR HANDLER                    *
;*                                  *
;* WITH ERROR # IN .A AND CARRY.    *
;*                                  *
;************************************
;
ERROR1 LDA #1 ;TOO MANY FILES
	!BYTE $2C
ERROR2 LDA #2 ;FILE OPEN
	!BYTE $2C
ERROR3 LDA #3 ;FILE NOT OPEN
	!BYTE $2C
ERROR4 LDA #4 ;FILE NOT FOUND
	!BYTE $2C
ERROR5 LDA #5 ;DEVICE NOT PRESENT
	!BYTE $2C
ERROR6 LDA #6 ;NOT INPUT FILE
	!BYTE $2C
ERROR7 LDA #7 ;NOT OUTPUT FILE
	!BYTE $2C
ERROR8 LDA #8 ;MISSING FILE NAME
	!BYTE $2C
ERROR9 LDA #9 ;BAD DEVICE #
;
	PHA ;ERROR NUMBER ON STACK
	JSR CLRCH ;RESTORE I/O CHANNELS
;
	LDY #MS1-MS1
;
	JSR MSG ;PRINT "CBM I/O ERROR #"
	PLA
	PHA
	ORA #$30 ;MAKE ERROR # ASCII
	JSR BSOUT ;PRINT IT
;
EREXIT  PLA
	SEC
	RTS


EXIT	SEI
	LDA #$00
	STA $FFF0	

	LDA #$78
	STA CBINV
	LDA #$D4
	STA CBINV+1

	LDA #$E4
	STA CINV+1  ;SET UP IRQ VECTOR
	LDA #$55
	STA CINV
;
;restore zero page values
;
ZREST	LDX #$00
ZREST1	LDA ZPAGE,X
	STA $00,X
	INX
	CPX #$19
	BNE ZREST1	
	
	PHP
	JSR CLRCH ;CLEAR CHANNELS
	LDA #0
	STA NDX ;FLUSH QUEUE
	PLP

	PLA
	PLA
	
	CLI
	JMP basicready



;
BASIN
	LDA #0
	STA $FFF0
	JSR $FFCF
	PHA
	LDA MEMMAP
	STA $FFF0
	PLA
	RTS
BSOUT
	PHA
	LDA #0
	STA $FFF0
	PLA
	JSR $FFD2
	LDA MEMMAP
	STA $FFF0
	RTS
CLRCH 	LDA #0
	STA $FFF0
	JSR RCLRCH
	LDA MEMMAP
	STA $FFF0
	RTS
 
LISTN 	STA FA
	LDA #0
	STA $FFF0
	JSR RLISTN
	LDA MEMMAP
	STA $FFF0
	RTS

SECND 	PHA
	LDA #0
	STA $FFF0
	PLA
	JSR RSECND
	LDA MEMMAP
	STA $FFF0
	RTS

CIOUT 	PHA
	LDA #0
	STA $FFF0
	PLA
	JSR RCIOUT
	LDA MEMMAP
	STA $FFF0
	RTS
UNLSN
	LDA #0
	STA $FFF0
	JSR RUNLSN
	LDA MEMMAP
	STA $FFF0
	RTS

TALK  STA FA
	LDA #0
	STA $FFF0
	JSR RTALK
	LDA MEMMAP
	STA $FFF0
	RTS
TKSA 	PHA
	LDA #0
	STA $FFF0
	PLA
	JSR RTKSA
	LDA MEMMAP
	STA $FFF0
	RTS
ACPTR
	LDA #0
	STA $FFF0
	JSR RACPTR
	PHA
	LDA MEMMAP
	STA $FFF0
	PLA
	RTS
UNTLK
	LDA #0
	STA $FFF0
	JSR RUNTLK
	LDA MEMMAP
	STA $FFF0
	RTS
OPENI PHA
	LDA #0
	STA $FFF0
	PLA
	JSR ROPENI
	LDA MEMMAP
	STA $FFF0
	RTS

;PRINT MESSAGE TO SCREEN ONLY IF
;OUTPUT ENABLED
;

SPMSG
MSG 	LDA MS1,Y
	PHP
	AND #$7F
	JSR BSOUT
	INY
	PLP
	BPL MSG
MSG10 	CLC
	RTS

MS1 	!PET $0D,"i/o error ", $A3
MS5 	!PET $0D,"searching",$A0
MS6 	!PET "for",$A0	
MS7 	!PET $0D,"press play",$A0	
MS8 	!PET "& record",$A0	
MS9 	!PET "on tape ",$A3	
MS10 	!PET $0D,"loading",$C7
MS11 	!PET $0D,"saving",$A0
MS21 	!PET $0D,"verifying",$C7
MS17 	!PET $0D,"found",$A0
MS18 	!PET $0D,"ok",$8D	
MS34 	!PET $0D,"**** cbm monitor 1.1 ***",$8D	
MS36 	!PET $0D,"brea",$CB

ZPAGE	!BYTE $00,$00,$00,$00,%00,$00,$00,$00
	!BYTE $00,$00,$00,$00,$00,$00,$00,$00
	!BYTE $00,$00,$00,$00,$00,$00,$00,$00
	!BYTE $00,$00	
