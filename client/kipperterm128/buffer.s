	;;  called after inbount character has been converted to petscii
	;;  accumulator holds byte to be pushed to buffer
	;;  buffer pointer must be a zero page address pair
	;;  2B9 is given location of low byte of the zero page pair
	;;  uses a temp varirable to hold the character to write to the buffer
	;;  3 bytes are used to hold the buffer size in DECIMAL format

.export buffer_toggle
.export buffer_init
.export write_to_buf
.export buffer_flag
.export view_buffer
.export clear_buffer
.export print_buffer
.export print_screen
		

.include "windows.s"
.import ascii_to_petscii


	
.segment "APP_SCRATCH" 
buffer_flag: .res 1
char_to_buff: .res 1
buff_size: .res 3
screen_buff:	.res 4096
temp_x:	.res 1
temp_y:	.res 1
temp_attributes:	.res 1
X2605:	.res 1
X2046:	.res 1
X2047:	.res 1
X2048:	.res 1
buffer_callback:	.res 2
.zeropage
kbuffer_ptr:	.res 2
kbuffer_view_ptr:	.res 2
	
.code


display_buffer_flag:
  pha
  txa
  pha
  tya
  pha
  ldx #$12            		
  lda #$08
  jsr $cdcc
  ldx #$13
  lda #$52
  jsr $cdcc
  ldy #$02
  lda buffer_flag
  bne :+
  ldy #$20
:
  tya	
  ldx #$1f                      
  jsr $cdcc
  pla
  tay
  pla
  tax
  pla
  rts





display_buffer_size:
  pha
  txa
  pha
  tya
  pha
  ldx #$12            		
  lda #$08
  jsr $cdcc
  ldx #$13
  lda #$56
  jsr $cdcc   			; move pointer to buffer size
  lda buff_size+2
  eor #$30                     	; display 10,000 digit
  ldx #$1f                      
  jsr $cdcc
  lda buff_size+1
  lsr
  lsr
  lsr
  lsr
  eor #$30                     	; display 1,000 digit
  jsr $cdcc
  lda buff_size+1               ; display 100 digits
  and #$0f
  eor #$30
  jsr $cdcc
  lda buff_size
  lsr
  lsr
  lsr
  lsr
  eor #$30                     	; display 10 digit
  jsr $cdcc
  lda buff_size               ; display 1 digits
  and #$0f
  eor #$30
  jsr $cdcc
	;; :
  tya	
  pla
  tay
  pla
  tax
  pla
  rts
	
buffer_init:

	lda #$00
	sta buffer_flag
	sta kbuffer_ptr
	lda #$04
	sta kbuffer_ptr+1
	;; 48128
	lda #$56
	sta buff_size		;
	lda #$42
	sta buff_size+1
	lda #$06
	sta buff_size+2
	jsr display_buffer_size
	rts
	
buffer_toggle:        ;Toggle Buffer Routine
	lda	kbuffer_ptr+1 ;if buffer full don't allow toggle, keep it off
	cmp	#$FF
	beq	:+  ; Buffer full skip toggling buffer on/off
BT:
	lda	buffer_flag ; BUFFER VARIABLE TOGGLE IT ON OR OFF
	eor	#$01
	sta	buffer_flag
	jsr     display_buffer_flag
:
	;; 	jsr	L253B ; print the STATUS BAR
	rts
	

write_to_buf:                ; write character to buffer
  sta	char_to_buff
  txa
  pha
  tya
  pha
  lda	#kbuffer_ptr
	;; 	#$83 ; 83/84 are the pointers to where to put the next byte in the buffer
  sta	$02B9  ; set 02b9 to hex 83 (tell kernel location to put the accumlator is pointed to in 83/84
  lda	char_to_buff  ; get the character we are going to write to the buffer?
  cmp	#$14   ; is it a 20? (delete) goto 2900 remove 1 from buffer
  beq	deleteCharFromBuffer
  cmp	#$9D   ; is it a 157? (Cursor left) if so goto 2900 remove 1 byte from buffer
  beq	deleteCharFromBuffer
  cmp	#$93   ; is it a 147? (Clear screen) if so goto 28f8 (Don't buffer it or remove anything from buffer, completely ignored)
  beq	:+
  ldx	#$01   ; Bank to write to (bank 1)
  ldy	#$00   ; offset to use with 83/84 
  jsr	$FF77 ; write byte to another bank JINSTA 
  sed           ; this is about displayig the bytes left in the buffer on the status line i am guessing
  sec          ; subtract 1 in decimal from BUFFER SIZE (1ef9,fa,fb) 3 bytes to represent the value in decimal
  lda	buff_size   ; changing values  
  sbc	#$01    ; in DECIMAL mode, we are subtracting 1 from 1EF9
  sta	buff_size   ; and returning it to 1EF9
  lda	buff_size+1   ; decrement 1 from 1EfA if taking 1 away from f9 caused a carry
  sbc	#$00
  sta	buff_size+1   ; store it
  lda	buff_size+2   ; decrement 1 from 1EFB if taking the carry away from 1EFA caused another carry
  sbc	#$00
  sta	buff_size+2  ; put it back
  cld            ; turn of decimal math ode
  inc	kbuffer_ptr ; update pointers in 83/84
  lda	kbuffer_ptr
  cmp	#$00
  bne	exit_buff
  inc	kbuffer_ptr+1
  lda   kbuffer_ptr+1
  cmp	#$FF   ; has buffer hit $FF00?  its full'
  bne   exit_buff  ; no, go to 28f8
  jsr	BT  ; otherwise gosub 288a (turn buffer off)
exit_buff:
  jsr display_buffer_size
  pla            ; restore everything and return
  tay
  pla
  tax
  lda	char_to_buff
  rts
				;
deleteCharFromBuffer:                  ; called whenever the character to be buffered is a 20 or a 157 not sure why  (DELETE, Cursor left)
							; basically remove last character from buffer and fix the pointers
  ldx	#$00
  ldy	#$04
  cpx	kbuffer_ptr
  bne	:+   ; 83 != 00 gogo 290f
  cpy	kbuffer_ptr+1
  bne	:+   ; 84 != 04 goto 290F
exit:				;L290C
  jmp	exit_buff  ; if buffer pointer equal 0400 exitBuff (nothing in buffer to delete)
				;
:
  dec	kbuffer_ptr  ; dec low byte of buffer pointer
  lda 	kbuffer_ptr
  cmp	#$FF   ; did we loop past 00?
  bne	:+  ; no goto 2919
  dec	kbuffer_ptr+1  ; yes, decrement hi byte
:
  clc            ; add 1 using decimal to 1ef9,fa,fb 
  sed
  lda	buff_size
  adc	#$01
  sta	buff_size
  lda	buff_size+1
  adc	#$00
  sta	buff_size+1
  lda	buff_size+2
  adc	#$00
  sta   buff_size+2
  cld
  jmp	exit




	
view_buffer:
	;; 	L4E16: ; VIEW BUFFER
	pha
	txa
	pha
	tya
	pha
        lda  kbuffer_ptr
	bne @continue  		; low byte not zero so obviously something since buffer starts and ends with 00
	lda  kbuffer_ptr+1
	cmp #$04
	bne @continue
	jsr	save_screen
	jmp     view_buffer_exit

@continue:
	jsr	save_screen	; store the screen and all attributes, cursor coordinates, etc
;;; 	 	lda	#$13
;;; 	 	jsr	L1D8B  		; send an XOFF (1d8b is send a character)
	;; 		jsr	L4D43 ; set zeropage ptr to start of buffer
	lda #$00
        sta kbuffer_view_ptr
	lda #$04
        sta kbuffer_view_ptr+1

	lda	#$93  ; clear screen
        jsr	$FFD2 ; output a 93
	lda	#$05
	jsr	$FFD2 ; output white
:	            ;L4E33:

	ldx	#$01
	ldy	#$00
	lda	#kbuffer_view_ptr; (8d/8e) zero pointer to be used for get byte from other bank
        jsr	$FF74 ; read a byte from another bank
	;; 	tax
	;; 	lda ascii_to_petscii,x
	;; 	beq   @skip_if_special  ; Probably should print everything...
	;;

	jsr print_a
	;; 	lda #$20
	;;         jsr	$FFD2		; output the byte
	;; @skip_if_special:
	;; 	cpx #$0d
	;; 	bne @continue1
	;; 	txa
	;; 	jsr $ffd2
	
@continue1:
	sec
	jsr $fff0		  ; plot	
	cpx	#$17 ; are we on line 24?
		      ;		X4E45:		
	bne :++	 ; no goto L4E63
	jsr	wait_for_keypress ; otherwise L4E8A
	;; 	lda	$034A ; get a character from the keyboard buffer
	cmp	#$1B ; did we get a 27 (Escape) 
	bne	:+ ; nope goto 4e59
	lda	#$93  ; otherwise output a 147 (clear screen)
	jsr	$FFD2
        jmp	view_buffer_exit ;goto 4e87 (restore screen?)
				;
:	;	L4E59:
	lda	#$93 ; clear screen
	jsr	$FFD2
	lda	#$05 ; white character
				;
        jsr $ffd2 
:	;L4E63
	inc	kbuffer_view_ptr ; increment 8d (not sure what 8d holds)
	lda	kbuffer_view_ptr
	bne	:+ ; if not zero goto 4e6b
        inc	kbuffer_view_ptr+1 ; otherwise increment 8e (8d/8e) are a pointer 
:	;L4E6B:	      
        lda	kbuffer_ptr+1 ; believe 83/84 are current end of buffer looks like 8d/8e are pointer into buffer
	cmp	kbuffer_view_ptr+1 ; compare hi bytes
	bne :----	    ;   L4E33 if they don't match, no worries keep on trucking
	lda	kbuffer_ptr
	cmp	kbuffer_view_ptr ; if they do, compare low bytes
        bne :----	 ; no match, keep on trucking L4E33
	lda	#$0D  ; we hit the end of the buffer output a carriage return
	jsr	$FFD2
	jsr	wait_for_keypress ; goto 4e8a
	lda	#$93  ; clear screen
	jsr	$FFD2
view_buffer_exit:
	;; 	jsr	L504E ; calculate size of buffer available and put into 1ef9,fa,fb
	;; 	jsr	L347B ; set Window to full screen

	jsr	restore_screen ; restore screen data


	;; 	lda	#$11
	;; 	jsr	L1D8B ; send a 17 (XON)
	pla
	tay
	pla
        tax
	pla
	rts
				;







clear_buffer:      ;clear Buffer
	lda #$00
	sta kbuffer_ptr
	lda #$04
	sta kbuffer_ptr+1
	;; 48128  ; reset buffer size 
	lda #$56
	sta buff_size		;
	lda #$42
	sta buff_size+1
	lda #$06
	sta buff_size+2
	jsr display_buffer_size
	rts


save_screen:	;; save the screen and its attributes
	pha
        lda	buffer_flag
	sta	X2605 ; store buffer flag to 2605 forf some reason
	lda	#$01
	sta	buffer_flag ; turn buffer on?  Not sure why
	txa
	pha
	tya
        pha
	lda	#<screen_buff ; point FA/FB to 52F3 (storage area for screen save)
	sta	kbuffer_view_ptr 
	lda	#>screen_buff ; 
	sta	kbuffer_view_ptr+1
	sec ;  set the carry
	jsr	$FFF0 ; call plot
        stx	temp_x ; save the x and y of cursorX52F1
	sty	temp_y ;X52F2
	lda	$F1
        sta	temp_attributes ; X52F0save attributes to 52f0... 
        lda	#$00
	ldx	#$12
	jsr	$CDCC ; write to vdc X = register a = value
	inx
	jsr	$CDCC ; write to vdc x = register a = value  (put VDC to 0000)
	jsr	store_vdc ; copy 8 pages to the buffer from the VDC
	lda	#$10
	ldx	#$12
	jsr	$CDCC ; point VDC to $1000 (default attributes start here)
	inx
	lda	#$00
	jsr     $CDCC

	jsr	store_vdc ; save attributes
	lda	X2605 ; restore buffer flag
	sta	buffer_flag
	pla ; restore y,x,a and return
        tay
        pla
	tax
        pla
        rts


store_vdc:	; VDC is set to where we want to start copying from and fa/fb are pointing to where we want to store the data
     ldy	#$00 ; read 8 pages of data from the VDC and stor it where fa,fb is pointing to
     sty	X2046 ;is a working variable used for page count (this routine saves 8 pages)
:	;L525C:
	ldx	#$1F
	jsr	$CDDA ; read a byte from the VDC (X = register, a is value)L3463
        sta	(kbuffer_view_ptr),y ; FA points to some buffer, and putting value into it
	iny
        cpy	#$00 ; if we hit zero  increment high bit of zp pointer FB
	bne	:-
	inc	kbuffer_view_ptr+1
	inc	X2046 ; also increment 2046 (counter for number of pages)
	lda	X2046
	cmp	#$08 ; if we hit 8 pages stored, exit)
	bne	:-   ; 525c
        rts
	


restore_screen:  ; restore screen and all its attributes
	pha
	lda	buffer_flag  ; take buffer flag and store itX1EF8
	sta	X2605  ; another variable not sure what it is (TEMP I assume)
	lda	#$01
	sta	buffer_flag  ; set flag buffer flag to 1
	txa
	pha
	tya
	pha            ; store x and y
	lda	#<screen_buff
        sta	kbuffer_view_ptr
	lda	#>screen_buff
        sta	kbuffer_view_ptr+1  ; point FA/FB to 52f3
	lda	#$00
	ldx	#$12
	jsr	$CDCC  ; set VDC to 0000
	inx
	jsr	$CDCC


	jsr	restore_vdc  ; puts the character data back into the VDC

	lda	#$10
        ldx	#$12
	jsr	$CDCC ; set VDC to $1000
	inx
	lda	#$00
	jsr	$CDCC

	jsr	restore_vdc  ; puts attributes back into the VDC


	lda	temp_attributes
	sta	$F1  ; reset attributes
	ldx	temp_x
	ldy	temp_y
	clc
	jsr	$FFF0  ; reset cursor
	lda	X2605
	sta	buffer_flag  ; reset Buffer flag
	pla
	tay   ; restore x,y & a
	pla
	tax
	pla
	rts

	
restore_vdc:	;L52D2:
	ldy	#$00
	sty	X2046  ;(2046 page count)

:	
        ldx	#$1F      ;31 write register of VDC
	lda	(kbuffer_view_ptr),y ; get byte from buffer

	jsr	$CDCC
	iny
        cpy	#$00 ; reach a page boundary (256?)
	bne	:- ;nope, keep going
        inc	kbuffer_view_ptr+1 ; yes, increment hi byte of zero page
	inc	X2046 ; increment page counter
        lda	X2046
        cmp	#$08  ; have we moved 8 pages?
	bne	:- ; nope keep going
	rts ; yes return
	


L4D59:            ; PRINT BUFFER
print_buffer:	
	pha
	txa
	pha
	tya
	pha
	jsr save_screen 	;	L51FA           ; store the screen
	;; 	jsr	L4D43           ; set the buffer view pointer to the start of the buffer
		lda #$00
	        sta kbuffer_view_ptr
		lda #$04
	        sta kbuffer_view_ptr+1
	 	lda	#$13	 
	;; 	jsr	L1D8B	
	jsr	$FFCC   	; clear the channels, set IO back to defaults
	lda	#$1F          	; 31
	sta	left		; window Left
	lda	#$0C            ; 12
	sta	top           ; window top
	lda	#$0B
	sta	color    	; purple??  
	ldx	#$30            ;48  ; window right
	ldy	#$0E            ;14  ; window bottom
	jsr	draw_window	; draws the window set by values in 2041, 42, x & y 2043 holds the attribute value to use to draw
	;;  the window with
	jsr	$FF7D  		; print immediately after untio you get to a 0
.byte  $70,$52,$49,$4E,$54,$49,$4E,$47,$20 ;Printing Buffer
.byte  $62,$55,$46,$46,$45,$53,$00

	;; 	jsr	L1E65  		;  Disable RS232/Swiftlink
	lda	#$03            ; open 3, 4, 7
	ldx	#$04
	ldy	#$07
	jsr	$FFBA   	; set LFS a = logical file, x = device, y secondary file
	lda	#$00
	jsr	$FFBD           ; SET NAME A = LENGTH, X,Y POINT TO NAME (NO NAME FOR PRINTER)
	jsr	$FFC0           ; OPEN the file (open 3,4,7)
	ldx	#$03
	jsr	$FFC9          	; change stdout to go to file #3 which we just opened (printer)
	bcc	L4DB3           ; if carry clear open successful
	jmp	L4DE9 ; open failed goto 4de9 accumulator holds error code
L4DB3:
 	cmp	#$05  		;  not sure why we check for a 5 here, but if it is 5 we go to same line as if
	bne	L4DBA           ; open had failed
	jmp	L4DE9
				;
L4DBA:
	ldx	#$01           	; bank number
	ldy	#$00            ; offset
	lda	#kbuffer_view_ptr; (8d/8e) zero pointer to be used for get byte from other bank
        jsr	$FF74 ; read a byte from another bank.
	jsr	$FFD2 ; print the character to the printer
	inc	kbuffer_view_ptr ; inc $8d
	lda	kbuffer_ptr+1	   ;	X0084      
	cmp	kbuffer_view_ptr+1 ;	X008E
	bne	L4DD7              ;    if high bytes don't match keep going, no need to check low bytes
	lda 	kbuffer_ptr		;	X0083
	cmp	kbuffer_view_ptr	;	X008D
	bne	L4DBA			; low bytes and high bytes don't match so loop again (skipping escape check for
	;;                              last page of buffer)
	jmp	L4DE9                   ; otherwise we are done printing so exit
				;
L4DD7:
	jsr $ffe4  		; get in get input cHAR
				;
	cmp	#$1B 		; is it an escape?
	beq	L4DE9          	; if so 4de9 (exit the printing)
	lda	kbuffer_view_ptr	;X008D
	cmp	#$00			; hit zero page on low byte of view pointer
	bne	L4DBA			; if not loop
	inc	kbuffer_view_ptr+1	;X008E   if so increment high bit
	jmp	L4DBA
				;
L4DE9:
	jsr	$FFCC ; reset the channels
	lda	#$03  ; close  channel 3
	jsr	$FFC3

L4DFC:
	;; 	jsr	L504E ; calculate buffer size and load to counter bytes
	jsr	max_window		;	Window to full screen 
	jsr     restore_screen; 	L5275 restore VDC Screen
	;; 	jsr	L1E56 ; enable RS232/swiftlink?
	;; 4E08 : A9 11 		"  "		lda	#$11
	;; 4E0A : 20 8B 1D 	"   "		jsr	L1D8B ; send xon
	
	jsr	$FFCC  		; reset channels.
	pla
	tay
	pla
	tax
	pla
	rts

	

	;;  PRINT THE SCREEN

print_screen:          ; PRINT THE SCREEN
	;; 	lda	#$01
	;; 	sta	X1EF8 ;set buffer flag, not sure why (flag must trigger something, no needed?)
	jsr	$FFCC ;clear the chanels
	;; 	lda	#$13
	;; 	jsr	L1D8B ;send xoff
	jsr	$FFCC  ;clear the channel again... 
	lda	#$03
	ldx	#$04
	ldy	#$07
	jsr	$FFBA ; set FS 3,4,7
	ldy	#$00
	sty	X2046 		; clearing temp variables
	sty	X2047  		;7/8 count variables 6 holds screen peek for temp
	sty	X2048 ;
	tya
	jsr	$FFBD ; set name to nothing (printer
	jsr	$FFC0 ;  OPEN
	ldx	#$03
	jsr	$FFC9 ; set output to channel 3 printer
	bcc	@L4C58 ; no errors continue
	jmp	@done
				;
@L4C58:
	cmp	#$05 ; check error code for 5, not sure why but if 5 exit
	bne	@L4C5F
	jmp	@done
@L4C5F:
	lda	#$0D ; output a carriage return first, to make sure printer is on a clean line
        jsr     JBSOUT

	ldx	#$12 ; point VDC to 0000 start of screen
	lda     #$00        
	jsr	$CDCC ; write to VDC
	inx
	jsr	$CDCC; write to VDC.
	sta	X2047 ;
	sta	X2048 ; 
@loop:
	LDX     #$1f
	jsr	$CDDA ; read from the VDC.
	jsr	@peek_to_char ; @L4CAA conver tot print character
	jsr	JBSOUT ; print it out
	inc	X2047 ; increment low byte of counter  (2000 times we have to lo)
	lda	X2048
	cmp	#$07  ;high byte not 7  
	bne	@L4C95 ; if we aren't in last page continue
	lda	X2047 ; otherwise compare low byte to counter 
	cmp	#$D0
	bne	@loop ; if we haven't hit the end get next byte
	jmp	@done_printing ; otherwise we are done

@L4C95:
	lda	#$00 ;increment 2048 on page cross, ugly.. should be simpler
	cmp	X2047  
	bne	@loop
	inc	X2048
	jmp	@loop
				;
@done_printing: ;   not sure why but we print out a space when we are done
	lda	#$20
	jsr	JBSOUT
	jmp	@done

	;;  so poke value to chr$ algorithm is:
	;;  take original value
	;;  turn off its high 2 bits
	;;  
	;;  
@peek_to_char:		  ;@L4CAA:
	sta	X2046 ; change peeked value to print value I think
	and	#$3F  ; turn off upper 2 bits of accumulator
	asl	X2046 ; move original value left 1 bit
	bit	X2046 ; modififed value (high 2 bits off, then shifted left 1 bit) with the original value and set the registers
	;;  bit performs and between the Acc and The value at the location
	;;  but it doesn't store the result of the and, just
	;;  affects the flags as follows:
	;;  bit puts bit 7 of 2046 inot Negative flag
	;;  bit puts bit 6 of 2046 into Overflow flag
	;;  bit puts 1 in the zero flag if and between A and value at 2046
	;;  is equal to zero, otherwise it will be 
	bpl	@L4CB9		;if 2046 has bit 7 set turn bit 7 on in accumulator

	
	
	ora	#$80  		; turn on bit 7 in accumulator
@L4CB9:
	bvs	@L4CBD 		; if 2046 doesn't have bit 6 set
	ora	#$40  		; turn bit 6 on in accumulator
@L4CBD:
	rts
				;
@done:
	jsr	$FFCC ; clear the channel
	lda	#$03
	jsr	$FFC3 ; close the channel
	;; 	lda	#$11
	;; 	jsr	L1D8B ;send xon
	jsr	$FFCC ; clear the channel
	;; 	lda	#$00  ;
	;; 	sta	X1EF8 ; clear the buffer flag?
	rts
	


L4DF4:                  ; close channel 4?  (4 is used in disk operations not part of the print bufer routine... 
	jsr	$FFCC   ; reset the channels
	lda	#$04    ; close channel 4  
	jsr	$FFC3   ; close
	

