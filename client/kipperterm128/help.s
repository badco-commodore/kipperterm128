
.export view_help
	
.code

view_help:
	pha
	txa
	pha
	tya
	pha

  lda #$32
  sta $0a2b 			;should turn cursor on.. but doesn't
  ldx #$0A
  jsr $cdcc

	
	jsr save_screen 	;	L51FA           ; store the screen
	lda	#$1b         	; 28
	sta	left		; window Left
	lda	#$05            ; 6
	sta	top           ; window top
	lda	#$0B
	sta	color    	; purple??  
	ldx	#$35            ;48  ; window right
	ldy	#$14            ;14  ; window bottom
	jsr	draw_window	; draws the window set by values in 2041, 42, x 
        jsr     $ff7d
help:	
.byte 05,"     kipperterm 128",13
.byte "       help menu",13
.byte 13
.byte 13,"  c= p - print buffer"
.byte 13,"  c= s - screen print"
.byte 13,"  c= c - clearn buffer"
.byte 13,"  c= b - toggle buffer"
.byte 13,"  c= v - view  buffer"	
.byte 13,"  c= e - toggle echo"
.byte 13,"  f1   - download menu"
.byte 13,13,32,0

	jsr	wait_for_keypress ; otherwise L4E8A
	jsr	max_window		;	Window to full screen 
	jsr	restore_screen ; restore screen data
	
  lda #$40
  sta $0a2b 			;should turn cursor on.. but doesn't
  ldx #$0A
  jsr $cdcc

	pla
	tay
	pla
        tax
	pla
	rts
