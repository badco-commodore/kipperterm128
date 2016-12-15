; vt100 emulation for C64
; vt100 emulation for C64
; originally from CaTer - Copyright Lars Stollenwerk 2003
; CaTer homepage is http://formica.nusseis.de/Cater/
; converted for use with ip65 by Jonno Downes, 2009.
; this version is for C64 only
; CaTer originally licensed under GPL
; Lars Stollenwerk has agreed to relicense the code in this file under MPL (Oct 2009)
;
; to use:
; 1) call vt100_init_terminal
; 2) for every 'inbound' data (received from remote host), call "vt100_process_inbound_char" - this will update the screen
; 3) pass every keypress into vt100_transform_outbound_char. on return from this call,
;      Y = 0 means don't send anything as a result of this keypress
;      Y = 1 means A contains single character to send to remote host
;      Y = 2 means AX points at null terminated string to send to remote host (e.g. an ANSI escape sequence)




.include "../inc/common.i"


.export vt100_init_terminal
.export vt100_process_inbound_char
.export vt100_transform_outbound_char
.export outbound_data
.export outbound_data_len

.export ascii_to_petscii
.import write_to_buf
.import buffer_flag
.import telnet_use_native_charset	
.import beep
.import print_a
.import tcp_send_data_len
.import tcp_send
.import scratch_buffer

; --- colour values ---
	;; 128 colors			
col_black       = $07
col_white       = $0f
col_red         = $08
col_cyan        = $06
col_purple      = $0a
col_green       = $04
col_blue        = $02
col_yellow      = $0d
col_orange      = $08
col_brown       = $0C
col_light_red   = $09
col_gray_1      = $0e
col_gray_2      = $01
col_light_green = $05
col_light_blue  = $03
col_gray_3      = $07
	

; --- colours ---
; vanilla     f  bold 1
; underline   e       3
; blink       5       d
; blink uline a       7
charmode_vanilla = $80
charmode_bold = $80  
charmode_underline = $A0
charmode_underline_bold = $A0
charmode_blink = $90
charmode_blink_bold = $90
charmode_blink_underline = $B0
charmode_blink_underline_bold = $B0

; text background 
text_background_colour = col_black


.segment "APP_SCRATCH" 
escape_buffer: .res $100
wrap_mode: .res 1

eightieth:	.res 1
scroll_region_start: .res 1
scroll_region_end: .res 1
	;; outbound_data:	.res $20
outbound_data_len:	.res 1
digit:	.res 1
digits:	.res 2
tempx:	.res 1
tempy:	.res 1
	
	
.zeropage

; --- esc mode ---
; $00 = normal
; $0f = esc mode
; $ff = esc [ mode
; $f0 = ignore one char
escape_mode: .res 1
	;;  --- line wrap ---
	;;  0 no
	;;  1 yes

; --- Vector ---
; four vectors in zeropage 
; for temporary use
temp_ptr_z: .res 2
temp_ptr_y: .res 2
temp_ptr_x: .res 2
	;; temp_ptr_w: .res 2
	;; temp_ptr_a: .res 2
temp_ptr_b: .res 2
	
escape_buffer_length: .res 1  ; points to first free position
escape_parameter: .res 1       ; numeric parameter in esc sequence


; font_mode contains three bits
; bit 0 = bold
; bit 1 = underline
; bit 2 = blink
; bit 7 = direct ANSI ESC colour
font_mode: .res 1

direct_colour: .res 1

; --- crsr save area ---
; here is crsr info saved with 
; ESC 7 and restored from with
; ESC 8

saved_font_mode:  .res 1
saved_reverse_mode:  .res 1
saved_row:  .res 1
saved_column:  .res 1

print_ptr: .res 2 ;temp vector for printing to screen





; -------------------------------------
; memory map
;
; -------------------------------------

; --- screen --- 
; $0400 - $07ff
Screen = $0400
outbound_data= $0400
insert_mode= $f6
; --- escape buffer --- 
; $0800 - $0bff

; --- char --- 
; $2000 - $27ff
font_table = $2000

; -------------------------------------
; constant declaration
;
; -------------------------------------

scbot = $e4
sctop = $e5
sclf = $e6
scrt = $e7
esc = $1b
brace = $5b
row = $eb
col = $ec

.code


        ;; accumlator holds number to convert
BINTODEC:	
BINBCD8:
	        sta digit
		SED		; Switch to decimal mode
		LDA #0		; Ensure the result is clear
		STA digits+0
		STA digits+1
		LDX #8		; The number of source bits

CNVBIT:		ASL digit		; Shift out one bit
		LDA digits+0	; And add into result
		ADC digits+0
		STA digits+0
	        CLC
		DEX		; And repeat for next bit
		BNE CNVBIT
		CLD		; Back to binary
	;; now have DECMIMAL representation in BCD
	;;  so lets move 10s digit into BDC+1
	        lda digits
	        and #$f0
	        lsr
	        lsr
	        lsr
	        lsr
	        sta digits+1
	        lda digits
	        and #$0f
	        sta digits
	        rts


	
;intialize VT100 emulation state
;inputs: none
;outputs: none
	;;  carry set = VT100
	;;  Clear Carry means use ANSI/IBM ASCII
vt100_init_terminal:
           bcs :+ 
	   ldax #ibm_to_petscii
	   stax temp_ptr_b
	   jsr $ff62 			; reset char set to petscii
	   jmp @cont1
:	
	   ldax #ascii_to_petscii
	   stax temp_ptr_b
	   jsr initialise_font		 ;init font
  
@cont1:
	;;   jsr initialise_font		 ;init font
  jsr initialise_variables ; init memory variables

  jsr initialise_screen ; init screen
  rts
  
;process incoming character
;inputs:
; A is inbound character
;outputs: 
; none, but screen and/or terminal state is updated.

vt100_process_inbound_char:
  tay
  lda escape_mode   ; handle esc mode
  beq :+            ; to far for branch to escape
  jmp handle_escape_char
:
  lda(temp_ptr_b),y
  beq @done         ; ignore non-printing chars     
  cmp #$01          ; something special?
  beq handle_special_char
  	
  jsr print_to_screen ; print to screen        
@done:
  rts



do_cr:
  ldx $eb     ; get row
  ldy #$00    ; set col=0
  jsr cursor_plot   ; set crsr
  rts
                     
do_line_feed:
  ldx $eb     ; crsr line
  cpx scroll_region_end     ; end scroll region?
  bne down_one_line    ;  no -> go on

  jsr scroll_up_scrollregion   ;  yes -> scroll up
  rts

down_one_line:
  cpx #$18    ; end of screen?
  bne @not_end_of_screen     ;  no -> go on
  rts         ;  yes -> do nothing
@not_end_of_screen:
  inx         ; next line
  ldy $ec     ; get col
  jsr cursor_plot   ; set crsr
  rts        

handle_special_char:
  tya         ; restore original char
  cmp #$0d    ; CR?
	;;   beq do_cr
  bne :++
  ldy buffer_flag  		;buffer write
  beq :+
  jsr write_to_buf
:	
  jmp do_cr
:	
  cmp #$08    ; BS?
  bne @not_bs
  ldy $ec     ; get col
  beq @bs_done   ; stop at left margin
  dey         ; dec column
  ldx $eb     ; get row
  jsr cursor_plot   ; set crsr
@bs_done:
  rts
@not_bs:
  cmp #$1b    ; esc?
  bne @not_escape
  lda #$0f    ; set esc mode
  sta escape_mode
  rts
@not_escape:
  cmp #$07    ; BEL?
  bne @not_bell  
  jsr beep
  rts
@not_bell:
  cmp #$0a    ; LF?
  beq do_line_feed
 
@not_lf:
  cmp #$09    ; TAB?
  bne @not_tab
  lda $ec     ; crsr col
  and #$f8    ; (col DIV 8) * 8
  clc         ; col + 8
  adc #$08
  cmp #$50    ; col=40?
  bne @not_last_col     ; no -> skip
  lda #$4f    ; yes -> col=39
@not_last_col:
  tay         ; col to y
  ldx $eb     ; line to x
  jsr cursor_plot   ; set crsr
  rts
	;; Commodore editor routines go into QUOTE mode whenever a " is output to the screen, so we
	;; must handle it as a special character and turn the quote mode off explicitely after the print
@not_tab:
  cmp #$22
  bne @not_quote
  jsr $ffd2
  lda #$00
  sta $f4
	
@not_quote:
    ;rts	default into checking for ibm extended 			


;; IBM TO PETSCII SPECIAL CHARACTERS
	;; IBM to PETSCII conversion requires both char sets to fully support
	;; SO ONLY GO THROUGH THESE IF we are in ANSI mode
	;; 
	;; test for ansi mode, if not go away
	;; if so look handle the case
@ibm_extended:
 ldx telnet_use_native_charset 	; rename this, 0 = vt100, 1=petscii 2=ansi/ibm
 cpx #$02
 beq @ibm_mode
 rts

@ibm_mode: 			; we are in IBM MODE, so check
 tya		                ; the hash for matcha and do what we should
 ldx #$00
	;;  stx $0550
:	
 cmp ibm_extended_hash,x  	; is the char value = to key
 beq @found
 inx
 cpx #$0c
 bne :-
 rts  				; no special case found should never happen
	                        ; but covering our bases.

@found:                
 lda ibm_extended_value,x
 tay
 cpx #$09
 bpl @reverse
 lda $f1
 and #$7f			;	(non alternat character set)
 sta $f1
 tya
	;; change attributes to non alternate char set
 jsr $ffd2
 lda $f1
 eor  #$80			;	(back to alternate character set)
 sta $f1
	;; change attributes back to alternate char set
 rts

@reverse: 			; character is reversed
 lda $f1
 eor #$40
 sta $f1
 tya
 jsr $ffd2
 lda $f1
 eor #$40
 sta $f1
 rts
	


	
; esc mode
; data in Y
; escape_mode <> $00 in A
handle_escape_char:
  tax         ; save escape_mode
  and #$0f    ; escape_mode = $0f?
  bne @not_discard_mode

; --- discard mode --- escape_mode = $f0
;discard char
  lda #$00    ; reset escape_mode
  sta escape_mode
  rts

@not_discard_mode:
  txa         ; restore escape_mode
  and #$f0    ; escape_mode = $ff?
  beq @short_escape_mode    ; no -> short Emode
  jmp long_escape_mode    ; yes -> long Emode
  
; short esc mode
; escape_mode = $0f
; process first char
@short_escape_mode:
  tya         ; restore char
; --- [ ---
  cmp #brace  ; [ ?
  bne @not_brace
  lda #$ff    ; set esc [ mode
  sta escape_mode
  rts
; --- ( ---
@not_brace:
        cmp #$28    ; ( ?
        bne :+
        jmp set_discard_mode
; --- ) ---
: 
  cmp #$29    ; ) ?
  bne :+
  jmp set_discard_mode
; --- # ---
:
  cmp #$23    ; # ?
  bne :+
  jmp set_discard_mode
; --- D --- index 
:
    cmp #$44    ; D ?
    bne :+
    jsr do_line_feed      ; same as LF
    jmp done_escape
; --- M --- reverse index
:
  cmp #$4d    ; M ?
  bne @not_M
  ldx $eb     ; get crsr row
  cpx scroll_region_start     ; top of scroll reg?
  bne :+

  jsr scroll_down_scrollregion  
  jmp done_escape
:
  cpx #$00    ; top of screen?
  bne :+
  jmp done_escape    ; yes -> do nothing
:     
  dex         ; one line up
  ldy $ec     ; get crsr col
  jsr cursor_plot   ; set crsr
  jmp done_escape


	
@not_M:
; --- E --- next line
  cmp #$45    ; E ?
  bne @not_E
  jsr do_cr
  jsr do_line_feed
  jmp done_escape
@not_E:
; --- 7 --- save crsr
  cmp #$37    ; 7?
  bne @not_7
	;;   lda font_mode			 save font
  lda $f1
  sta saved_font_mode
  lda $f3     ; save reverse mode
  sta saved_reverse_mode
  ldx $eb     ; save position
  ldy $ec
  stx saved_row
  sty saved_column
  jmp done_escape
@not_7:      
; --- 8 --- restore crsr
  cmp #$38    ; 8?
  bne @not_8
  ldx saved_row ; restore pos
  ldy saved_column
  jsr cursor_plot
  lda saved_reverse_mode   ; restore ..
  sta $f3     ; .. reverse mode
  ldx saved_font_mode   ; restore font
	;;   stx font_mode
	;;   lda font_attribute_table,x
	;;   and $f1
	;;   sta $f1   ; set colour
  stx $f1
  jmp done_escape

; --- unknown ---
@not_8:
  cmp #$1b  			; ESC? (ESC FOLLOWED BY ESCAPE, do nothing
	;; just stay in escape mode (return);
  bne done_escape
  rts	;

; --- reset ESC mode ---
done_escape:
  lda #$00    ; reset escape_mode
  sta escape_mode
  rts 

; --- set Discard mode ---
set_discard_mode:
  lda #$f0    ; set esc mode $f0
  sta escape_mode
  rts


; -------------------------------------
; [ esc mode
;
; escape_mode = $ff
; -------------------------------------
  
long_escape_mode:
  tya         ; restore char
  ldy escape_buffer_length
  sta escape_buffer,y  ; store char
  iny
  sty escape_buffer_length   ; inc esc buffer  
  jsr test_if_letter   ; test letter
  bcs :+     ; process command
  rts

; --- process esc command ---
; A = last char
; Y = escape_buffer_length
; X counts processed command chars
:
	;;   sta $0520 			; hold onto 
  ldx #$00    ; first char 

; --- A --- crsr up       
  cmp #$41    ; A?
  bne @not_A
  jsr get_number_from_esc_seq  ; get argument
  lda escape_parameter    ; escape_parameter = 0...
  bne :+
  inc escape_parameter    ; .. means 1
:
  lda $eb     ; get crsr row        
  sec
  sbc escape_parameter    ; row = row - up
  cmp scroll_region_start     ; stop at top of ..
  bpl :+    ; ..scroll region
  lda scroll_region_start    
:
  tax         ; x is row
  ldy $ec     ; y is col
  jsr cursor_plot   ; set crsr
  jmp @end_escape_seq
  
; --- B --- crsr down
@not_A:
  cmp #$42    ; B?
  bne @not_B
  jsr get_number_from_esc_seq  ; get argument
  lda escape_parameter    ; escape_parameter = 0...
  bne :+
  inc escape_parameter    ; .. means 1        
:
  lda $eb     ; get crsr row        
  clc
  adc escape_parameter    ; row = row + down
  bcs :+		  ; if we looped over 255 obviously out of range
  cmp scroll_region_end     ; outside scrregion?
  bcs :+    ; yes -> branch
  tax         ; x is row
  jmp @skip
:
  ldx scroll_region_end     ; x = row = scroll_region_end   
@skip:
  ldy $ec     ; y is col
  jsr cursor_plot   ; set crsr
  jmp @end_escape_seq

; --- C --- crsr right
@not_B:
  cmp #$43    ; C?
  bne @not_C
  jsr get_number_from_esc_seq  ; get argument        
  lda escape_parameter    ; escape_parameter = 0...
  bne :+
  inc escape_parameter    ; .. means 1
:
  lda $ec     ; get crsr col        
  clc
  adc escape_parameter    ; col = col + right
  bcs :+		  ; over 255 so obviously off screen
  cmp #$4f    ; outside screen?
  bcs :+    ; yes -> branch
  tay
  jmp @skip2
:    
  ldy #$4f    ; y=col=left margin
@skip2:
  ldx $eb     ; x is row
  jsr cursor_plot   ; set crsr
  jmp @end_escape_seq

; --- D --- crsr left
@not_C:
  cmp #$44    ; D?
  bne @not_D
  jsr get_number_from_esc_seq  ; get argument
  lda escape_parameter    ; escape_parameter = 0...
  bne :+
  inc escape_parameter    ; .. means 1        
:
  lda $ec     ; get crsr col        
  sec
  sbc escape_parameter    ; col = col - left
  bpl :+      ; stop at left..
  lda #$00    ; ..margin
:
  tay         ; y is col
  ldx $eb     ; x is row
  jsr cursor_plot   ; set crsr
  jmp @end_escape_seq

; --- m ---  font attributes
@not_D:
  cmp #$6d    ; m?
  beq @next_font_attribute
  jmp @not_m
@next_font_attribute:
  jsr get_number_from_esc_seq
  pha         ; save nondigit char (character following the number)
  lda escape_parameter    ; parameter to A  (number in esca pe sequence)
  ; -- 0 --
  bne :+    ; 0?
  sta font_mode    ; set font = vanilla  (This won't work since I ignore 0
	;; 	in font_mode assuming that the value may have been just color
	;; 	change, so we are going to explicitly set the $f1 here)
	
  sta $f3     ; reverse off  0= all attributes off

  lda $f1 			; set attributes (except for upper/lower case)
				;; all off 
  and #$0f 			; turn off upper bits
  eor #charmode_vanilla	
  sta $f1   ; set  attributes to upper nibble
	
  jmp @end_font_attribute    ; jmp next par
  ; -- 1 -- bold
:
  cmp #$01
  bne :+
  lda font_mode    ; set bold
  ora #$01
  sta font_mode
  jmp @end_font_attribute    ; next char
  ; -- 4 -- underline
:
  cmp #$04
  bne :+
  lda font_mode    ; set u_line
  ora #$02
  sta font_mode
  jmp @end_font_attribute    ; next char
  ; -- 5 -- blink
: 
  cmp #$05
  bne :+
  lda font_mode    ; set blink
  ora #$04
  sta font_mode
  jmp @end_font_attribute    ; next char
  ; -- 7 -- reverse
:
  cmp #$07
  bne :+
  lda #$01    ; set revers
  sta $f3
  jmp @end_font_attribute    ; next char
:
  ; -- 30 - 37 --  TEXT COLORS
  cmp #38     ; >= 38?
  bcs @end_font_attribute
  cmp #30     ; < 30?
  bcc @end_font_attribute
  sbc #30     ; pointer for table
  sta direct_colour
  lda #$80 			;set direct color
  ora font_mode          	; set top bit for color change flag
  sta font_mode

	;; 40 - 47 (BACKGROUND COLORS)

	;;  VDC does not support independent background, but could be in the string
	;;  and could screw up how it exits (appears to set insert mode somehow of text editor, at least
	;;  at times  may not be related.. but need to think it through
	
  	
@end_font_attribute:  ; -- next char --
  pla         ; get nondigit char
  cmp #$3b    ; is semicolon?
  beq @next_font_attribute    ; then next char
  ; -- set colour --
  lda font_mode    ;  7th bit is flagged when color change
	;;  is part of the sequence, and the color reference is stored in
	;; direct color
	;;   bmi :+    ; bit 7->direct col
  and #$07  ; just the attribute bits
  beq :+    ; no attributes to change, go to direct color
  tax         ; font to colour
  lda $f1
  and #$0f 			; turn off upper bits
  sta $f1
  lda font_attribute_table,x
  ora $f1
  sta $f1   ; set  attributes to upper nibble
	;;     jmp @end_escape_seq		
:
  lda font_mode
  bpl :+            		; no color change 
  ; -- set direct colour --
  lda $f1
  and #$f0        		; Turn off Lower Bits
  sta $f1
  ldx direct_colour ; colour maping
  lda direct_colour_table,x
  ora $f1
  sta $f1   ; set colour
:
  lda #$00
  sta font_mode
  jmp @end_escape_seq


	
; --- K --- erase line
@not_m:
  cmp #$4b      ; K?
  bne @not_K
  jsr get_number_from_esc_seq    ; get parameter
  lda escape_parameter      ; in A
  ; -- 0 -- crsr to end of line
  bne :+

  jsr erase_to_end_of_line    ; erase end line
  jmp @end_escape_seq
  ; -- 1 -- begin to crsr
:
  cmp #$01
  bne :+

  jsr erase_line_to_cursor    ; erase beg line
  jmp @end_escape_seq
  ; -- 2 -- whole line
:
  cmp #$02
  bne :+      ; par undefined

  ldx $eb       ; line in X
  jsr erase_line_by_number      ; erase line  (

	;; 	sta $0a29       ; del char ..
                ; ..under crsr
:
  jmp @end_escape_seq        
  
; --- f --- same as H
	; Incorrectly assumes you will always get x;yf or x;yH (both are equivalent)
	;;  however while bad form, you can get code with just xf or ;yf and in those case the x or y should
	;;  just be treated as if the value is 1 in the ANSI (0 for the screen editor routines) (0 vs 1 based counting)
	;;  FIX!!!
@not_K:
  cmp #$66
  bne @not_f
  jmp @set_cursor_position      ; same as H

; --- H --- cursor position
@not_f:
  cmp #$48
  bne @not_H
@set_cursor_position: 
  cpy #$01    ; No parameters just plot to 0,0 (1 represents just H length is total number of bytes after [ 
  bne :+
  ; -- home --
  ldx #$00
  ldy #$00
  jsr cursor_plot   ; set crsr
  jmp @end_escape_seq

	
  ; -- row, col --
:
  jsr get_number_from_esc_seq  	;escape param is number, a is 1st character after numeric that is not a digit
	;;   cmp #$3b    ; is ;?
	;;   bne @end_set_cursor_position 	; no semi assume error (INCORRECT) if no semi, should handle it as the row value and column should be 1/0


  ; -- prepare row --
  ldy escape_parameter    ; get row
  bne :+    ; 0 means 1
  iny	    ; if its zero make it 1, because ANSI is 1 based counting, screen editor is 0 based
:       
  dey         ; line 1 -> line 0

  cpy #$19    ; >= 25?..  (Should be variable, not hard coded, same with #$50 below)
  bcs @end_set_cursor_position    ; ..error!  Outside the screen ignore the plot call
  sty temp_ptr_x ; save row
  ; -- prepare col
  cmp #$3b
  beq @2digits
  ldy #$00
  jmp @continue1
@2digits:
  jsr get_number_from_esc_seq
  ldy escape_parameter    ; get col
@continue1:
  bne :+    ; 0 means 1
  iny
:
  dey         ; line 1 -> line 0        
  cpy #$7f    ; >= 80?..
  bcs @end_set_cursor_position    ; ..error!        
  ldx temp_ptr_x ; restore row to X
  jsr cursor_plot   ; set crsr
@end_set_cursor_position:
  jmp @end_escape_seq
           

; --- J --- erase screen
@not_H:
  cmp #$4a      ;J?
  bne @not_J
  jsr get_number_from_esc_seq    ; get parameter
  lda escape_parameter      ; in A
  ; -- 0 -- crsr to end

  bne @not_cursor_to_end
  jsr $ca9f		;
  jmp @end_escape_seq     ; then end

  ; -- 1 -- beg of screen to crsr
@not_cursor_to_end:
  cmp #$01                    	; cursor to start of screen
  bne @not_start_to_cursor
  jsr erase_line_to_cursor    ; del start of ln
  ldx $eb       ; get crsr line
:
  dex           ; previous line
  bpl @continue	
  jmp @end_escape_seq     ; neg line -> end
@continue:	
  txa
  pha           ; save X
  jsr erase_line_by_number      ; erase line
  pla
  tax           ; restore X
  jmp :-
  ; -- 2 -- del screen
@not_start_to_cursor:		;erase entire screen, cursor stays put... sure a better way, but will leave as is for now
  cmp #$02      ; unknown?
  bne :+
  lda #$93
  jsr $ffd2
:
  jmp @end_escape_seq     ; then ingnore
	;; :
	;;   ldx #$18      ; start at ln 24
	;; :
	;;   txa
	;; 	  pha			;	save X
	;;   jsr erase_line_by_number      ; erase line
	;;   pla
	;;   tax           ; restore X
	;;   dex           ; previous line
	;;   bpl :-
	;  jmp @end_escape_seq


; --- r ---  set scroll region                 Use 128 WINDOW  0,line number to 24,line number
@not_J:
  cmp #$72    ; r?
  bne @not_r
  ; -- prepare top --
  jsr get_number_from_esc_seq
  cmp #$3b    ; is ;?
  bne @error_in_escape_seq   ; no -> error
  ldy escape_parameter    ; get top
  dey         ; line 1 -> line 0
  cpy #$19    ; >=25?..
  bcs @error_in_escape_seq   ; ..error!
  sty temp_ptr_x ; save top      
  ; -- prepare bottom --
  jsr get_number_from_esc_seq
  ldy escape_parameter    ; get bottom
  dey         ; line 1 -> line 0
  cpy #$19    ; >=25?..
  bcs @error_in_escape_seq   ; ..error! 
  sty temp_ptr_y ; save bottom       
  ; -- validate lines --
  lda temp_ptr_x ; restore top
  cmp temp_ptr_y ; >= bottom?..
  bcs @error_in_escape_seq   ; ..error!
	
  sta scroll_region_start     ; top -> SRStart

  sty scroll_region_end     ; bottom -> SREnd

  ; -- home crsr
  ldx #$00
  ldy #$00
  jsr cursor_plot
@error_in_escape_seq:
  jmp @end_escape_seq        
        

@not_r:
	;; remember X is the pointer into escape sequence,
	;; preserver it it you muck with it.
  cmp #$6c    ; ? l
  bne @not_l
	;; Handle the modes only support wrap for
  jsr get_number_from_esc_seq
  ldy escape_parameter
  cpy #$07
  bne :+
  lda #$00
  sta wrap_mode	
:
  cpy #$04         		; 4l = replace
  bne :+
  lda #$00
  sta insert_mode
:	
  jmp @end_escape_seq
@not_l:
  cmp #$68  			; ?h
  bne @not_h
  jsr get_number_from_esc_seq
  ldy escape_parameter
  cpy #$07
  bne :+
  lda #$01
  sta wrap_mode
:
  cpy #$04                      ; 4h = insert
  bne :+
  lda #$80			; bit 7 is insert flag
  sta insert_mode
:	
  jmp @end_escape_seq
	
@not_h:

				; send back ok when we get a check
	;;  used by sites for ANSI detection
; --- unknown ---
  cmp #$6e  			; n
  beq @_n
  jmp @not_n
@_n:

  jsr get_number_from_esc_seq
  ldy escape_parameter
  cpy #$5 			; asks for status, always respond OK (ESC[  0 n )
  bne :+
  ldy outbound_data_len
  lda #$1b
  sta outbound_data,y
  iny
  lda #$5b
  sta outbound_data,y
  iny
  lda #$30
  sta outbound_data,y
  iny
  lda #$6f
  sta outbound_data,y
  iny
  sty outbound_data_len
  jmp @end_escape_seq
:

  cpy #$06 			; asking for cursor location respond esc row;columnR (R is not a typo, that's the command)
   
  bne :+

  sec
  jsr $fff0		;	$c01b	; x = row, y - col
  inx
  iny
  stx tempx
  sty tempy
	;;    inc outbound_data_len
	;;    inc outbound_data
  ldy outbound_data_len
   lda #$1b
   sta outbound_data,y
   iny
   lda #$5b
   sta outbound_data,y
   iny
   lda tempx
   jsr BINTODEC
   lda digits+1			;
   eor #$30
   sta outbound_data,y
   iny
   lda digits
   eor #$30
   sta outbound_data,y
   iny
   lda #$3B
   sta outbound_data,y
   iny
   lda tempy
   jsr BINTODEC
   lda digits+1
   eor #$30
   sta outbound_data,y
   iny
   lda digits
   eor #$30
   sta outbound_data,y
   iny
   lda #$52
   sta outbound_data,y
   iny
   sty outbound_data_len  

:
 jmp @end_escape_seq
	

@not_n:
; --- S --- save crsr
  cmp #$73    ; s?
  bne @not_s
	;;   lda font_mode			 save font
  ldx $eb     ; save position
  ldy $ec
  stx saved_row
  sty saved_column
  jmp @end_escape_seq

@not_s:
	
; --- U --- restore crsr
  cmp #$75    ; u?
  bne @not_u
  ldx saved_row ; restore pos
  ldy saved_column
  jsr cursor_plot
  jmp @end_escape_seq


@not_u:	
	;; unknown
	
@end_escape_seq:
	;;   lda $f4
	;;   beq :+
	;;   brk				;
	;;   sta $0510
	;;   lda $f5
	;;   sta $0512
	;; 	  lda $f6
	;;   sta $0514
	;; :
  lda #$00
  sta escape_buffer_length   ; reset esc buffer
  sta escape_mode   ; reset esc mode
  rts



; -------------------------------------
; Test letter
;
; char in A
; returns carry = 1 for A = letter
; -------------------------------------
test_if_letter:
  cmp #$41    ; smaller then A?
  bcs :+     ; no -> go on
  rts         ; return no letter
:
  cmp #$5b    ; smaller then Z+1?
  bcs :+     ; no -> go on
  sec         ; return letter
  rts
:  
  cmp #$61    ; smaller then a?
  bcs :+     ; no -> go on
  rts         ; return no letter
:  
  cmp #$7b    ; smaller then z+1?        
  bcs :+     ; no -> go on
  sec         ; return letter
  rts
:
  clc         ; return no letter
  rts        
        


; -------------------------------------
; test digit
;
; char in A
; returns carry = 1 for A = digit
; -------------------------------------

test_if_digit:
  cmp #$30    ; smaller then 0?
  bcs :+     ; no -> go on
  rts         ; return no digit
:
  cmp #$3a    ; smaller then 9+1?
  bcs :+     ; no -> go on
  sec         ; return digit
  rts
:
  clc         ; return no digit
  rts


; -------------------------------------
; get decimal number from esc sequence
;
; esc sequence in escape_buffer
; first index to process in X
; returns: number escape_parameter
;          first non digit char in  A
; -------------------------------------
get_number_from_esc_seq:
  lda #$00    ; assume $00
  sta escape_parameter
	;; Need some error checking to make sure X doesn't go past escape buffer length
@next_digit:
  lda escape_buffer,x  ; get next char
  inx
  jsr test_if_digit   ; digit?
  bcc @done     ; no -> return
  sbc #$30    ; ascii to #
  pha         ; save digit
  ; old value * 10
  ; 10a = ( 4a + a ) * 2
  lda escape_parameter
  asl         
  asl         ; ( 4a
  clc
  adc escape_parameter    ; + a )
  asl         ; *2 
  sta escape_parameter    ; = 10a
  ; add new digit
  pla         ; resore new digit
  clc
  adc escape_parameter
  sta escape_parameter
  jmp @next_digit     ; next char        
@done:
  rts        


        
; *************************************
; *
; * outgoing data
; *
; *************************************
; -------------------------------------
; given a single char (read from keyboard)
; work out what data should be sent to the remote host.
; input:
; A = keypress
; output:
; Y=0 - no data to be sent (i.e. ignore keypress)
; Y=1 - A contains single byte to send
; Y=2 - AX points to null terminated string to send
; -------------------------------------

;Y=0 nothing to send
;Y=1 A = char to send
;Y=2 AX=pointer to asciiz string to send

vt100_transform_outbound_char: 
  tay
  lda petscii_to_ascii,y   ; PETSCII to ASCII
  bne :+
  ldy #0  ; ignore key
  rts
:                
  cmp #$ff
  beq output_string
  cmp #$fe
  beq command_key  ; command key
  ;default - send (possibly transformed) single char 
  ldy #1      ;means A contains single byte to send
@done:
rts



; -------------------------------------
; create an ansi control sequence
; -------------------------------------


output_string:
  tya         ; restore original key

; --- crsr U ---
  cmp #$91    ; test crsr U
  bne @not_U
  ldax  #ansi_cursor_up
  ldy   #2
  rts
; --- crsr L ---
@not_U:      
  cmp #$9d    ; test crsr L
  bne @not_L
  ldax #ansi_cursor_left
  ldy #2
  rts
@not_L:
  cmp #$0d  ;test CR
  bne @not_CR
  ldax #crlf
  ldy #2
  rts

@not_CR:
  ldy #0  ;must be some kind of error 
  rts


; -------------------------------------
; keypress was a command key
; -------------------------------------

command_key:  
        tya         ; restore character

; --- crsr R ---
; ---   ^]   ---
; both events send $1d
  cmp #$1d
  bne @not_crsr_R
  lda #$04    ; test control Key
  bit $028d
  beq @cursor_right   ; not pressed
  ; control ] is pressed
  tya         ; send ^]
  ldy #1
  rts

; crsr R 
@cursor_right:
  ldax #ansi_cursor_right
  ldy #2
  rts

; --- crsr D ---
; ---   ^Q   ---
; both events send char $11
@not_crsr_R:
  cmp #$11    ;^Q / crsr down
  bne @not_crsr_D
  lda #$04    ; test control Key
  bit $028d
  beq @cursor_down   ; not pressed
  ; control Q is pressed
  tya         ; send ^Q
  ldy #1
  rts
        
  ; crsr down is pressed        
@cursor_down:
  ldax #ansi_cursor_down
  ldy #2
  rts

; --- HOME key ---
; ---    ^S    ---
; both events send char $13
@not_crsr_D:
  cmp #$13    ;^S / HOME
  bne @not_home
  lda #$04    ; test control Key
  bit $028d
  beq @home  ; not pressed
  ; control S is pressed
  tya         ; send ^S
  ldy #1
  rts

@home: 
  lda #$09 ; send TAB
  ldy #1
  rts

; --- DEL key ---
; ---    ^T    ---
; both events send char $14
@not_home:
  cmp #$14    ;^T / DEL
  bne @not_del 
  lda #$04    ; test control Key
  bit $028d
  beq @del   ; not pressed
  ; control T is pressed
  tya         ; send ^T
  ldy #1
  rts
  
  ; send DEL
@del:
  lda #$08
  ldy #1
  rts


; --- unknown C=-Key ---
@not_del:
      ldy #0 ;means don't send anything
      rts

; *************************************
; *
; * screen handling
; *
; *************************************

; --- these variables become updated ---
;     on crsr movement.
;
; $e0 $e1  start of screen line
; $ec      crsr column
; $eb      crsr row
; $e2 $e3  start of colour line
; $f1    colour

; --- these variables become updated ---
;     on crsr switching.
;
; $0a27    crsr flag, 0 = on
; $0a28    crsr blink counter
; $0a29    char under crsr
; $0a26    crsr blink phase, 0 normal
; $0a2a  colour under crsr
; $0a2b is cursor for vdc




; -------------------------------------
; moves the crsr to column Y
; and line X
; the crsr ist turned off during 
; operation
; destroys all registers
; -------------------------------------
cursor_plot:
	;;	sty $ec
	;; 	stx $eb			
	clc
	jsr $fff0		;	$c01b	;
	rts
        

; -------------------------------------
; Print char in A to screen
; being aware of the crsr state
; -------------------------------------

print_to_screen:
  jsr plot_char
  rts

; -------------------------------------
; print char to screen
; char = $ff means no output
; chr in A
; X and Y unaffected
; -------------------------------------

plot_char:
  sta temp_ptr_x ; save char
  txa         ; save registers
  pha
  tya
  pha

  lda wrap_mode
  bne printchar
  lda $ec	
  cmp $ee    			
  bne printchar          		; if not last column just print the character
	;;  WE ARE IN THE LAST COLUMN
  lda $eb
  cmp $ed
  	
  bne :+                        ; if last column and last row, jsut print the character
  lda eightieth
  bne dd                        ; set we are in 80th, and just print the character
  inc eightieth
  lda temp_ptr_x
	;;   jsr $ffd2
  jsr print_a
  jmp end_plot_char
dd:
  jsr do_line_feed              ; have been in 80th and last row before, so now scroll up and print char on next line
  jsr do_cr
  jmp printchar
	;;  last column, not last row
: 	
;;if 80th flag set, we've already printed out last character and moved cursor back, we need to move on to next row first column
  lda eightieth 		; not in eightieth, so just print
  beq @seventyninth       			; branch if EIGHTIETH NOT SET		

  lda #$1d  			; cursor right
	;;   jsr $ffd2
  jsr print_a
  jmp printchar              
	
@seventyninth:
  lda temp_ptr_x ; restore char
  jsr $ffd2
  lda #$9d	 ; move cursor back
  jsr $ffd2
  inc eightieth
  jmp end_plot_char
	
printchar:
  lda #$00
  sta eightieth
	;; Are we in insert mode? If so, we need to put a space at the end of
	;; of the current line and then output the character
  lda insert_mode
  beq @done       		; if no insert mode no worries
  lda #$EA
  cmp #$4f                     
  bne @done                     ; if there is not a character in the last
	;; column of the current line just output the character, otherwise
	;; we have to clear the last column before the print in order for
	;; insert to work properly.
  clc
  lda $e0
  adc #$4f
  sta temp_ptr_z
  lda #$e1
  adc #$00
  sta temp_ptr_z+1
  lda temp_ptr_z
  ldx #$12
  jsr $CDCC
  inx
  lda temp_ptr_z+1
  jsr $cdcc     		; point VDC to $3000 (overwrite uppercase/lowerc
  ldx #$1f
  lda #$20
  jsr $cdcc


@done:	
  lda temp_ptr_x ; restore char
  ldx buffer_flag  		;buffer write
  beq :+
  jsr write_to_buf
  jsr print_a	;;   jsr $ffd2	 ;
  jmp end_plot_char	;
	
; PETSCII to ScreenCode (SC)
; --- $c0-$ff ---   - illegal -
  cmp #$c0
  bcc :+
  jmp end_plot_char   ; no output
; --- $a0-$bf ---   C=(latin-1) chars
:       
  cmp #$a0
  bcc :+
				;  sbc #$40    ; SC = PET - $40
  jmp @check_for_reverse
; --- $80-$9f ---   - illegal -
:       
  cmp #$80
  bcc :+
  jmp end_plot_char   ; no output
; --- $60-$7f ---  kapital letters        
:
  cmp #$60
  bcc :+
	;;   sbc #$20    ; SC = PET - $20
  jmp @check_for_reverse
; --- $40-$5f ---  small letters
:
  cmp #$40
  bcc :+
				;  sbc #$40    ; SC = PET - $40
  jmp @check_for_reverse
; --- $20-$3f ---  interpunction
:
  cmp #$20
  bcc :+
  jmp @check_for_reverse   ; SC = PET
; --- $00-$1f ---  - illegal -
:
  jmp end_plot_char   ; no output

; --- handle reverse mode---
@check_for_reverse:
	;;   ldx $f3     ; reverse mode? $f3 on 128
	;;   beq @put_char
	;;   ora #$80    ; reverse char

; --- put char to screen ---
@put_char:
	;;   ldy $ec     ; get crsr col   $ec on 128
	;; #$ 28 (40 columns)
;;;   cpy #$50  ;col = 40  ; make 80
;;;   bcc @no_line_wrap		;
              ;the only way we end up trying to write to column 40 should
              ;be if we skipped the normal line wrap after writing to col 39
              ;because we are at the end of the scroll region
              ;that means we should do a scroll up and then write this char at col 0
;;;   pha
	;;   jsr scroll_up_scrollregion
	;;   pla
	;;   ldy #$00    ; begin of line
	;;   sty $ec     ; $ec on 128
	;;   jsr cursor_plot		;
	;;   jsr $ffd2			; this will auto scroll unless turned off... will have to look into

	
@no_line_wrap:
	jsr $ffd2		;  sta ($e0),y ; char to screen  $e0 on 128
	;;   lda $f1   ; get colour   $f1 on 128
	;; 	  sta ($e2),y		set colour      $e2 on 128 
       jmp end_plot_char	;
; --- move on crsr ---
  
  ldx $eb     ; get crsr row    $EB on 128
  cpx scroll_region_end     ; end of scroll reg?  
  beq @dont_scroll_yet     ; we don't want to trigger a scroll of the whole screen unless
                                    ; we are actually writing a char. we shouldn't scroll just when
                                    ; writing to the bottom right hand screen (else e.g. the title bar 
                                    ; in 'nano' gets pushed off the top of the screen.
                                    ;
  cpy #$4f			    ; 79
  beq move_to_next_line   ; yes -> new line
@dont_scroll_yet:  
  iny         ; move on
  sty $ec     ; $EC on 128
  
end_plot_char:
  pla         ; restore registers
  tay
  pla
  tax
  rts
        
; -------------------------------------
; subtask of plot_char
; ends at end_plot_char
; -------------------------------------
move_to_next_line:
  ldx $eb     ; get crsr row
  cpx scroll_region_end     ; end of scroll reg?
  beq @scroll_up     ; yes -> branche
  cpx #$18    ; line 24?
  beq end_plot_char   ; yes -> crsr stays
; --- normal wrap ---
  inx         ; increase line
  stx $eb
  ldy #$00    ; begin of line
  sty $ec
  jsr set_line_vectors
  ldx temp_ptr_x ; set screen line
  ldy temp_ptr_x+1
  stx $e0
  sty $e1
  ldx temp_ptr_y ; set colour line
  ldy temp_ptr_y+1
  stx $e2
  sty $e3        
  jmp end_plot_char
; --- scroll up ---        
@scroll_up:
  jsr scroll_up_scrollregion
  ldy #$00    ; begin of line
  sty $ec
  jmp end_plot_char
  


scroll_up_scrollregion:
	;; set window to scroll region
	;; startline,0 endline,24
        pha
	lda scbot
	pha
	lda sctop
	pha
	lda sclf
	pha
	lda scrt
	pha
	lda #$0
	sta sclf
	lda #$4f
	sta scrt
	lda scroll_region_start
	sta sctop
	lda scroll_region_end
	sta scbot
	;; call scroll down

	jsr $cabc
	
	pla
	sta scrt
	pla
	sta sclf
	pla
	sta sctop
	pla
	sta scbot
	pla
				;
	rts			;
	
  

scroll_down_scrollregion:

	;; set window to scroll region
	;; startline,0 endline,24
        pha
	lda scbot
	pha
	lda sctop
	pha
	lda sclf
	pha
	lda scrt
	pha
	lda #$0
	sta sclf
	lda #$4f
	sta scrt
	lda scroll_region_start
	sta sctop
	lda scroll_region_end
	sta scbot

	;; call scroll down
	jsr $caca		;

	pla
	sta scrt
	pla
	sta sclf
	pla
	sta sctop
	pla
	sta scbot
	pla

	rts			;
	


; -------------------------------------
; print string to screen
; string: chars, terminated by $00
; start lo in x
; start hi in y
; affects A
; takes care of crsr
; the string must be smaller 
;   than 255 chrs
; -------------------------------------


plot_string:
  stx print_ptr   ; store start vector
  sty print_ptr+1
  ldy #$00
@next_char:
  lda (print_ptr),y
  beq @end_string      ; $00 terminates string
  jsr plot_char        
  iny
  jmp @next_char

@end_string:
  rts



; -------------------------------------
; delete screen line and keep cursor in same position after erase 
; (Erase Line)
;
; line number in X
;
; destroys all registers
; -------------------------------------

erase_line_by_number:
    
  	

  lda $eb     			;save x,y
  pha
  lda $ec
  pha


  ldy #0
  jsr cursor_plot
				; move cursor to line	
  jsr erase_to_end_of_line	; clear line
  pla			        ; cursor to position before delete line
  tay				;  sta $ec			;
  pla
  tax				;sta $eb			
  jsr cursor_plot
  rts
        


; -------------------------------------
; delete screen line from crsr to end
; (Erase End of Line)
; destroys all registers
; -------------------------------------

erase_to_end_of_line:
	jsr $ca76
	rts


; -------------------------------------
; delete screen line up to crsr
; (Erase Begin of Line)
; destroys all registers
; -------------------------------------
erase_line_to_cursor:
	jsr $ca8b
	rts

; -------------------------------------
; set line vectors
;
; line no in X
; destroys A and Y
;
; sets start of screen line in temp_ptr_x
; sets start of colour line in temp_ptr_y
	;;  40 column setup... Hi Byte is just determining which row  (0400) is default... 
; -------------------------------------

set_line_vectors:
  lda $c033,x   ; get lo byte  $c033 128
  sta temp_ptr_x
  sta temp_ptr_y
  ; determin hi byte
  ldy #$04      ; hi byte
  cpx #$07      ; line < 7?  if row  7 or more high byte increment y  (6*40)= 240 (7*40) = 280 
  bcc @got_line_vector
  iny           
  cpx #$0d      ; line < 13?
  bcc @got_line_vector  	; if 13 or more increment high byte again 12*40 = 480  13*40 =520
  iny
  cpx #$14      ; line < 20?    ; if 20 or more increment high byte again 19*40 = 768  20*40 = 800
  bcc @got_line_vector
  iny           ; line 20-24
@got_line_vector:
  sty temp_ptr_x+1
  tya
  clc           ; colour RAM =
  adc #$d4      ; video RAM + d4  ; add high byte and store color... Assume all is done this way to save cycles... not documented way
  sta temp_ptr_y+1        
  rts



; -------------------------------------
; init routines
;
; -------------------------------------
	

wait:	
  ldx #$0
loop:	
  lda init_screen_msg,x
	tay
;;; 	lda ascii_to_petscii,y
	;; 	 	lda ibm_to_petscii,y
	lda (temp_ptr_b),y
  	beq @done_print  
	jsr $ffd2
	inx
	jmp loop

  @done_print:

	;; @loop:				;

	;;   jsr $ffe4
	;;   beq @loop
rts
	


  

	
initialise_screen:

 ;--- set background ---
;;;   lda #text_background_colour	
	;;   sta $d021			;
; --- disable Shift C= ---





	;; lda #$40
	;; sta $0a2b 			;should turn cursor on.. but doesn't
	;; ldx #$0A
	;; jsr $cdcc
	
  lda #$80
  sta $f7                    	;  disable character change (Shift C= key) $f7 on 128, but other bits matter on 128 and do not on 64

; --- erase screen ---
	;;   ldx #$18      ; start at ln 24        
@erase_one_line:
	;;   txa
	;;   pha           ; save X
	;;   jsr erase_line_by_number      ; erase line
	;;   pla
	;;   tax           ; restore X
	;;   dex           ; previous line
	;;   bpl @erase_one_line

  lda $f1
  	
  and #$0f
  ora  #charmode_vanilla     ; load vanilla
  sta $f1		    ; $F1 for 128
 jsr wait
; --- put crsr ---
  jsr do_cr
  jsr do_line_feed
  jsr do_line_feed

  rts
  


initialise_variables:
  lda #$00
  sta escape_mode
  sta escape_buffer_length
  sta scroll_region_start
  sta font_mode
  sta saved_font_mode
  sta saved_reverse_mode
  sta saved_row
  sta saved_column
  sta eightieth
  sta outbound_data_len
  sta wrap_mode
  sta insert_mode  

  lda #$18    ; last line
  sta scroll_region_end     ; = 24
  jsr $caf2		    ; cursor full block
	;;   jsr $cd9f  	 		; cursor off
  jsr $cd6f			;cursor on
  lda #$c0
  sta $f8  			;turn off linking and scrolling
	;;   lda #$00        		; debugging stuff... remove
	;;   sta temp_ptr_a
	;;   lda #$06
	;;   sta temp_ptr_a+1
  rts


reverse_font_table = ROM_FONT + $0400

	
initialise_font:  
  sei
  ldx #<ROM_FONT ; font_mode in temp_ptr_z
  ldy #>ROM_FONT
  stx temp_ptr_z
  sty temp_ptr_z+1
  ldx #<font_table    
  ldy #>font_table
  stx temp_ptr_y
  sty temp_ptr_y+1
  ldx #<reverse_font_table 
  ldy #>reverse_font_table
  stx temp_ptr_x
  sty temp_ptr_x+1
  
; copy font

  lda #$30      ; copy 4 pages = 1KB (was $30 for some reason)
  ldy #$00
  ldx #$12
  jsr $CDCC
  inx
  lda #$00
  jsr $cdcc     		; point VDC to $3000 (overwrite uppercase/lowercase character set)
:
  lda (temp_ptr_z),y            ; copy 8 bytes
  jsr $cdca
  iny				;
  cpy #$08
  bne :-
  lda #$00
  ldy #$00	
@8byteVDC:			; fill extra 8 bytes with 0
  jsr $cdca
  iny
  cpy #$08
  bne @8byteVDC

  clc
  lda #$08
  adc temp_ptr_z		;
  sta temp_ptr_z
  lda #$00
  adc temp_ptr_z+1
  sta temp_ptr_z+1    
  ldy #$00	
  cmp #>reverse_font_table
  bne :-
  lda #<reverse_font_table
  cmp temp_ptr_z
  bne :-
	;; 
	;; repeat for reverse characters  SOmething off here...
	
  ldx #<ROM_FONT ; font_mode in temp_ptr_z
  ldy #>ROM_FONT
  stx temp_ptr_z
  sty temp_ptr_z+1
  ldy #$00	
:
  lda (temp_ptr_z),y            ; copy 8 bytes
  eor #$ff
  jsr $cdca
  iny				;
  cpy #$08
  bne :-
  lda #$00
  ldy #$00	
@8byteVDC1:			; fill extra 8 bytes with 0
  jsr $cdca
  iny
  cpy #$08
  bne @8byteVDC1

  clc
  lda #$08
  adc temp_ptr_z		;
  sta temp_ptr_z
  lda #$00
  adc temp_ptr_z+1
  sta temp_ptr_z+1    
  ldy #$00	  	
  cmp #>reverse_font_table
  bne :-
  lda #<reverse_font_table
  cmp temp_ptr_z
  bne :-


  cli
  rts

	
.rodata
font_attribute_table:    ; bits mean blink, underline, bold
.byte charmode_vanilla, charmode_bold                           ; 000 000
.byte charmode_underline, charmode_underline_bold               ; 010 010
.byte charmode_blink, charmode_blink_bold                       ; 001 001
.byte charmode_blink_underline, charmode_blink_underline_bold   ; 011 011

direct_colour_table:
;ANSI 30   31 32 32   34 35 36 37 
;    blk   rd gr ye  blu mg cy wh
.byte  01, $08, 4, $0d, $02, $0a, 7, $0f

ansi_cursor_up:     .byte esc, brace, $41, $00 ; esc [ A 
ansi_cursor_down:   .byte esc, brace, $42, $00 ; esc [ B
ansi_cursor_right:  .byte esc, brace, $43, $00 ; esc [ C 
ansi_cursor_left:   .byte esc, brace, $44, $00 ; esc [ D

crlf:  .byte $0d,$0a,0

; -------------------------------------
; table ASCII  to PETSCII 
;
; these characters cat be printed
;
; pet=$00 means ignore the char
; pet=$01 means do something complicated
; -------------------------------------
ascii_to_petscii:
  .byte $00   ; $00
  .byte $00   ; $01
  .byte $00   ; $02
  .byte $00   ; $03
  .byte $00   ; $04
  .byte $00   ; $05
  .byte $00   ; $06
  .byte $01   ; $07 BEL
  .byte $01   ; $08 BS/DEL
  .byte $01   ; $09 TAB
  .byte $01   ; $0a LF
  .byte $00   ; $0b
  .byte $00   ; $0c
  .byte $01   ; $0d CR
  .byte $00   ; $0e
  .byte $00   ; $0f
  .byte $00   ; $10
  .byte $00   ; $11
  .byte $00   ; $12
  .byte $00   ; $13 
  .byte $00   ; $14
  .byte $00   ; $15
  .byte $00   ; $16
  .byte $00   ; $17
  .byte $00   ; $18
  .byte $00   ; $19
  .byte $00   ; $1a
  .byte $01   ; $1b ESC
  .byte $00   ; $1c
  .byte $00   ; $1d
  .byte $00   ; $1e
  .byte $00   ; $1f
  .byte $20   ; $20  1:1
  .byte $21   ; $21  1:1
  .byte $01   ; $22    (handle special case of QUOTE mode)
  .byte $23   ; $23  1:1
  .byte $24   ; $24  1:1
  .byte $25   ; $25  1:1
  .byte $26   ; $26  1:1
  .byte $27   ; $27  1:1
  .byte $28   ; $28  1:1
  .byte $29   ; $29  1:1
  .byte $2a   ; $2a  1:1
  .byte $2b   ; $2b  1:1
  .byte $2c   ; $2c  1:1
  .byte $2d   ; $2d  1:1
  .byte $2e   ; $2e  1:1
  .byte $2f   ; $2f  1:1
  .byte $30   ; $30  1:1
  .byte $31   ; $31  1:1
  .byte $32   ; $32  1:1
  .byte $33   ; $33  1:1
  .byte $34   ; $34  1:1
  .byte $35   ; $35  1:1
  .byte $36   ; $36  1:1
  .byte $37   ; $37  1:1
  .byte $38   ; $38  1:1
  .byte $39   ; $39  1:1
  .byte $3a   ; $3a  1:1
  .byte $3b   ; $3b  1:1
  .byte $3c   ; $3c  1:1
  .byte $3d   ; $3d  1:1
  .byte $3e   ; $3e  1:1
  .byte $3f   ; $3f  1:1
  .byte $40   ; $40  1:1
  .byte $61   ; $41 -----
  .byte $62   ; $42
  .byte $63   ; $43
  .byte $64   ; $44 capital
  .byte $65   ; $45
  .byte $66   ; $46
  .byte $67   ; $47
  .byte $68   ; $48
  .byte $69   ; $49
  .byte $6a   ; $4a
  .byte $6b   ; $4b
  .byte $6c   ; $4c
  .byte $6d   ; $4d letters
  .byte $6e   ; $4e
  .byte $6f   ; $4f
  .byte $70   ; $50
  .byte $71   ; $51
  .byte $72   ; $52
  .byte $73   ; $53
  .byte $74   ; $54
  .byte $75   ; $55
  .byte $76   ; $56
  .byte $77   ; $57
  .byte $78   ; $58
  .byte $79   ; $59
  .byte $7a   ; $5a -----
  .byte $5b   ; $5b  1:1
  .byte $5c   ; $5c  1:1
  .byte $5d   ; $5d  1:1
  .byte $5e   ; $5e  1:1
  .byte $5f   ; $5f  1:1
  .byte $60   ; $60  1:1
  .byte $41   ; $61 -----
  .byte $42   ; $62
  .byte $43   ; $63
  .byte $44   ; $64 small
  .byte $45   ; $65
  .byte $46   ; $66
  .byte $47   ; $67
  .byte $48   ; $68
  .byte $49   ; $69
  .byte $4a   ; $6a
  .byte $4b   ; $6b letters
  .byte $4c   ; $6c
  .byte $4d   ; $6d
  .byte $4e   ; $6e
  .byte $4f   ; $6f
  .byte $50   ; $70
  .byte $51   ; $71
  .byte $52   ; $72
  .byte $53   ; $73
  .byte $54   ; $74
  .byte $55   ; $75
  .byte $56   ; $76
  .byte $57   ; $77
  .byte $58   ; $78
  .byte $59   ; $79
  .byte $5a   ; $7a -----
  .byte $7b   ; $7b  1:1 {
  .byte $7c   ; $7c  1:1 |
  .byte $7d   ; $7d  1:1 }
  .byte $7e   ; $7e  1:1 ~
  .byte $00   ; $7f
  .byte $00   ; $80
  .byte $00   ; $81
  .byte $00   ; $82
  .byte $00   ; $83
  .byte $00   ; $84
  .byte $00   ; $85
  .byte $00   ; $86
  .byte $00   ; $87
  .byte $00   ; $88
  .byte $00   ; $89
  .byte $00   ; $8a
  .byte $00   ; $8b
  .byte $00   ; $8c
  .byte $00   ; $8d
  .byte $00   ; $8e
  .byte $00   ; $8f
  .byte $00   ; $90
  .byte $00   ; $91
  .byte $00   ; $92
  .byte $00   ; $93
  .byte $00   ; $94
  .byte $00   ; $95
  .byte $00   ; $96
  .byte $00   ; $97
  .byte $00   ; $98
  .byte $00   ; $99
  .byte $00   ; $9a
  .byte $00   ; $9b
  .byte $00   ; $9c
  .byte $00   ; $9d
  .byte $00   ; $9e
  .byte $00   ; $9f
  .byte $20   ; $a0
  .byte $7f   ; $a1
  .byte $7f   ; $a2
  .byte $bf   ; $a3
  .byte $be   ; $a4
  .byte $7f   ; $a5
  .byte $73   ; $a6
  .byte $b5   ; $a7
  .byte $53   ; $a8
  .byte $bb   ; $a9
  .byte $7f   ; $aa
  .byte $bc   ; $ab
  .byte $7f   ; $ac
  .byte $2d   ; $ad
  .byte $7f   ; $ae
  .byte $7f   ; $af
  .byte $ba   ; $b0
  .byte $b8   ; $b1
  .byte $b6   ; $b2
  .byte $b7   ; $b3
  .byte $7a   ; $b4
  .byte $b9   ; $b5
  .byte $7f   ; $b6
  .byte $7f   ; $b7
  .byte $5a   ; $b8
  .byte $7f   ; $b9
  .byte $7f   ; $ba
  .byte $bd   ; $bb
  .byte $b0   ; $bc
  .byte $b0   ; $bd
  .byte $79   ; $be
  .byte $7f   ; $bf
  .byte $a5   ; $c0
  .byte $61   ; $c1
  .byte $a4   ; $c2
  .byte $61   ; $c3
  .byte $a3   ; $c4
  .byte $a4   ; $c5
  .byte $7f   ; $c6
  .byte $63   ; $c7
  .byte $ad   ; $c8
  .byte $ab   ; $c9
  .byte $ac   ; $ca
  .byte $65   ; $cb
  .byte $69   ; $cc
  .byte $69   ; $cd
  .byte $69   ; $ce
  .byte $69   ; $cf
  .byte $64   ; $d0
  .byte $6e   ; $d1
  .byte $6f   ; $d2
  .byte $6f   ; $d3
  .byte $b1   ; $d4
  .byte $6f   ; $d5
  .byte $af   ; $d6
  .byte $7f   ; $d7
  .byte $6f   ; $d8
  .byte $75   ; $d9
  .byte $75   ; $da
  .byte $75   ; $db
  .byte $b3   ; $dc
  .byte $79   ; $dd
  .byte $7f   ; $de
  .byte $b4   ; $df
  .byte $a2   ; $e0
  .byte $41   ; $e1
  .byte $a1   ; $e2
  .byte $41   ; $e3
  .byte $a0   ; $e4
  .byte $a1   ; $e5
  .byte $7f   ; $e6
  .byte $a6   ; $e7
  .byte $aa   ; $e8
  .byte $a8   ; $e9
  .byte $a9   ; $ea
  .byte $a7   ; $eb
  .byte $49   ; $ec
  .byte $49   ; $ed
  .byte $49   ; $ee
  .byte $49   ; $ef
  .byte $7f   ; $f0
  .byte $4e   ; $f1
  .byte $4f   ; $f2
  .byte $4f   ; $f3
  .byte $b1   ; $f4
  .byte $4f   ; $f5
  .byte $ae   ; $f6
  .byte $7f   ; $f7
  .byte $4f   ; $f8
  .byte $55   ; $f9
  .byte $55   ; $fa
  .byte $55   ; $fb
  .byte $b2   ; $fc
  .byte $59   ; $fd
  .byte $7f   ; $fe
  .byte $59   ; $ff


;IBM_EXTENDED_HASH (Used in conjunction with ibm_to_petscii to display characters
; that are in the uppercase char set or reversed, or both
ibm_extended_hash:
;; upper case characters
 .byte $01
 .byte $02
 .byte $03
 .byte $04
 .byte $05
 .byte $06
 .byte $5c	               	; backslash
 .byte $e2
 .byte $e3
; from this byte forward character reverse
 .byte $db
 .byte $de
 .byte $df
; from thsi byte forward character reversed and alternate set 
ibm_extended_value:
;these are characters that need alternate character set 
 .byte $77
 .byte $71
 .byte $73
 .byte $7a
 .byte $78
 .byte $61
 .byte $6d 			; backslash
 .byte $6f
 .byte $7e
; these are the characters that need reversed
 .byte $A0		      ; use a0 to distinguish reserse from normal space
 .byte $A1
 .byte $A2
; these are the characters that need alternate character set AND reversed
; -------------------------------------
; table IBM  to PETSCII 
;
; these characters cat be printed
;
; pet=$00 means ignore the char
; pet=$01 means do something complicated
; -------------------------------------
ibm_to_petscii:
  .byte $00   ; $00
  .byte $01   ; $01
  .byte $01   ; $02
  .byte $01   ; $03
  .byte $01   ; $04
  .byte $01   ; $05
  .byte $01   ; $06
  .byte $01   ; $07 BEL
  .byte $01   ; $08 BS/DEL
  .byte $01   ; $09 TAB
  .byte $01   ; $0a LF
  .byte $00   ; $0b
  .byte $00   ; $0c
  .byte $01   ; $0d CR
  .byte $00   ; $0e
  .byte $00   ; $0f
  .byte $00   ; $10
  .byte $00   ; $11
  .byte $00   ; $12
  .byte $00   ; $13 
  .byte $00   ; $14
  .byte $00   ; $15
  .byte $00   ; $16
  .byte $00   ; $17
  .byte $00   ; $18
  .byte $00   ; $19
  .byte $00   ; $1a
  .byte $01   ; $1b ESC
  .byte $00   ; $1c
  .byte $00   ; $1d
  .byte $00   ; $1e
  .byte $00   ; $1f
  .byte $20   ; $20  1:1
  .byte $21   ; $21  1:1
  .byte $01   ; $22  1:1
  .byte $23   ; $23  1:1
  .byte $24   ; $24  1:1
  .byte $25   ; $25  1:1
  .byte $26   ; $26  1:1
  .byte $27   ; $27  1:1
  .byte $28   ; $28  1:1
  .byte $29   ; $29  1:1
  .byte $2a   ; $2a  1:1
  .byte $2b   ; $2b  1:1
  .byte $2c   ; $2c  1:1
  .byte $2d   ; $2d  1:1
  .byte $2e   ; $2e  1:1
  .byte $2f   ; $2f  1:1
  .byte $30   ; $30  1:1
  .byte $31   ; $31  1:1
  .byte $32   ; $32  1:1
  .byte $33   ; $33  1:1
  .byte $34   ; $34  1:1
  .byte $35   ; $35  1:1
  .byte $36   ; $36  1:1
  .byte $37   ; $37  1:1
  .byte $38   ; $38  1:1
  .byte $39   ; $39  1:1
  .byte $3a   ; $3a  1:1
  .byte $3b   ; $3b  1:1
  .byte $3c   ; $3c  1:1
  .byte $3d   ; $3d  1:1
  .byte $3e   ; $3e  1:1
  .byte $3f   ; $3f  1:1
  .byte $40   ; $40  1:1
  .byte $61   ; $41 -----
  .byte $62   ; $42
  .byte $63   ; $43
  .byte $64   ; $44 capital
  .byte $65   ; $45
  .byte $66   ; $46
  .byte $67   ; $47
  .byte $68   ; $48
  .byte $69   ; $49
  .byte $6a   ; $4a
  .byte $6b   ; $4b
  .byte $6c   ; $4c
  .byte $6d   ; $4d letters
  .byte $6e   ; $4e
  .byte $6f   ; $4f
  .byte $70   ; $50
  .byte $71   ; $51
  .byte $72   ; $52
  .byte $73   ; $53
  .byte $74   ; $54
  .byte $75   ; $55
  .byte $76   ; $56
  .byte $77   ; $57
  .byte $78   ; $58
  .byte $79   ; $59
  .byte $7a   ; $5a -----
  .byte $5b   ; $5b  1:1
  .byte $01   ; $5c  1:1
  .byte $5d   ; $5d  1:1
  .byte $5e   ; $5e  1:1
  .byte $a4   ; $5f  1:1
  .byte $60   ; $60  1:1
  .byte $41   ; $61 -----
  .byte $42   ; $62
  .byte $43   ; $63
  .byte $44   ; $64 small
  .byte $45   ; $65
  .byte $46   ; $66
  .byte $47   ; $67
  .byte $48   ; $68
  .byte $49   ; $69
  .byte $4a   ; $6a
  .byte $4b   ; $6b letters
  .byte $4c   ; $6c
  .byte $4d   ; $6d
  .byte $4e   ; $6e
  .byte $4f   ; $6f
  .byte $50   ; $70
  .byte $51   ; $71
  .byte $52   ; $72
  .byte $53   ; $73
  .byte $54   ; $74
  .byte $55   ; $75
  .byte $56   ; $76
  .byte $57   ; $77
  .byte $58   ; $78
  .byte $59   ; $79
  .byte $5a   ; $7a -----
  .byte $7b   ; $7b  1:1 {
  .byte $7c   ; $7c  1:1 |
  .byte $7d   ; $7d  1:1 }
  .byte $7e   ; $7e  1:1 ~
  .byte $00   ; $7f
  .byte $c3   ; $80
  .byte $55   ; $81
  .byte $45   ; $82
  .byte $41   ; $83
  .byte $41   ; $84
  .byte $41   ; $85
  .byte $41   ; $86
  .byte $43   ; $87
  .byte $45   ; $88
  .byte $45   ; $89
  .byte $45   ; $8a
  .byte $49   ; $8b
  .byte $49   ; $8c
  .byte $49   ; $8d
  .byte $c1   ; $8e
  .byte $c1   ; $8f
  .byte $c5   ; $90
  .byte $00   ; $91
  .byte $00   ; $92
  .byte $4f   ; $93
  .byte $4f   ; $94
  .byte $4f   ; $95
  .byte $55   ; $96
  .byte $55   ; $97
  .byte $59   ; $98
  .byte $cf   ; $99
  .byte $d5   ; $9a
  .byte $43   ; $9b
  .byte $5C ; $9c
  .byte $d9   ; $9d
  .byte $00   ; $9e
  .byte $46   ; $9f
  .byte $41   ; $a0
  .byte $49   ; $a1
  .byte $4f   ; $a2
  .byte $55   ; $a3
  .byte $4e   ; $a4
  .byte $ce   ; $a5
  .byte $41   ; $a6
  .byte $4f   ; $a7
  .byte $3f   ; $a8
  .byte $f0   ; $a9
  .byte $ee   ; $aa
  .byte $bc   ; $ab
  .byte $7f   ; $ac
  .byte $21   ; $ad
  .byte $3c   ; $ae
  .byte $3e   ; $af
  .byte $e9   ; $b0
  .byte $df   ; $b1
  .byte $de   ; $b2
  .byte $7D   ; $b3
  .byte $B3   ; $b4
  .byte $B3   ; $b5
  .byte $B3   ; $b6
  .byte $AE   ; $b7
  .byte $AE   ; $b8
  .byte $B3   ; $b9
  .byte $7D   ; $ba
  .byte $AE   ; $bb
  .byte $BD   ; $bc
  .byte $BD   ; $bd
  .byte $BD   ; $be
  .byte $AE   ; $bf
  .byte $AD   ; $c0
  .byte $B1   ; $c1
  .byte $B2   ; $c2
  .byte $AB   ; $c3
  .byte $60   ; $c4
  .byte $7B   ; $c5
  .byte $AB   ; $c6
  .byte $AB   ; $c7
  .byte $AD   ; $c8
  .byte $B0   ; $c9
  .byte $B1   ; $ca
  .byte $B2   ; $cb
  .byte $AB   ; $cc
  .byte $60   ; $cd
  .byte $7B   ; $ce
  .byte $B1   ; $cf
  .byte $B1   ; $d0
  .byte $B2   ; $d1
  .byte $B2   ; $d2
  .byte $AD   ; $d3
  .byte $AD   ; $d4
  .byte $B0   ; $d5
  .byte $B0   ; $d6
  .byte $7B   ; $d7
  .byte $7B   ; $d8
  .byte $BD   ; $d9
  .byte $B0   ; $da
  .byte $01   ; $db ; should be reversed space
  .byte $A2   ; $dc
  .byte $A1   ; $dd
  .byte $01   ; $de  ;should be reverse of other half, use closest
  .byte $01   ; $df  ; should be revers of other half use closest
  .byte $41   ; $e0
  .byte $c2   ; $e1
  .byte $01   ; $e2  ; upper case $6f 
  .byte $01   ; $e3  ; uper case $7e
  .byte $a0   ; $e4
  .byte $a1   ; $e5
  .byte $7f   ; $e6
  .byte $a6   ; $e7
  .byte $aa   ; $e8
  .byte $a8   ; $e9
  .byte $a9   ; $ea
  .byte $a7   ; $eb
  .byte $49   ; $ec
  .byte $49   ; $ed
  .byte $49   ; $ee
  .byte $49   ; $ef
  .byte $7f   ; $f0
  .byte $4e   ; $f1
  .byte $4f   ; $f2
  .byte $4f   ; $f3
  .byte $b1   ; $f4
  .byte $4f   ; $f5
  .byte $ae   ; $f6
  .byte $7f   ; $f7
  .byte $4f   ; $f8
  .byte $55   ; $f9
  .byte $55   ; $fa
  .byte $ba   ; $fb
  .byte $b2   ; $fc
  .byte $59   ; $fd
  .byte $7f   ; $fe
  .byte $59   ; $ff
	
	
; -------------------------------------
; table PETSCII  to ASCII 
;
; these characters can be typed with 
; the keyboard
;
; ascii = $00 means ignore key
; ascii = $ff menas send string
; ascii = $fe means do something 
;             complicated (command key)
; -------------------------------------
petscii_to_ascii:
  .byte $00   ; $00
  .byte $01   ; $01
  .byte $02   ; $02
  .byte $03   ; $03
  .byte $04   ; $04
  .byte $05   ; $05
  .byte $06   ; $06
  .byte $07   ; $07
  .byte $08   ; $08 DEL
  .byte $09   ; $09 TAB
  .byte $0a   ; $0a
  .byte $0b   ; $0b
  .byte $0c   ; $0c
  .byte $ff   ; $0d CR
  .byte $0e   ; $0e
  .byte $0f   ; $0f
  .byte $10   ; $10
  .byte $fe   ; $11 ^Q (crsr down)
  .byte $12   ; $12
  .byte $fe   ; $13 ^S TAB (HOME)
  .byte $fe   ; $14 ^T BS  (DEL)
  .byte $15   ; $15
  .byte $16   ; $16
  .byte $17   ; $17
  .byte $18   ; $18
  .byte $19   ; $19
  .byte $1a   ; $1a
  .byte $1b   ; $1b ESC
  .byte $1c   ; $1c
  .byte $fe   ; $1d ^](crsr right)
  .byte $1e   ; $1e
  .byte $1f   ; $1f
  .byte $20   ; $20 SPACE
  .byte $21   ; $21 !
  .byte $22   ; $22 "
  .byte $23   ; $23 #
  .byte $24   ; $24 $
  .byte $25   ; $25 %
  .byte $26   ; $26 &
  .byte $27   ; $27 '
  .byte $28   ; $28 (
  .byte $29   ; $29 )
  .byte $2a   ; $2a *
  .byte $2b   ; $2b +
  .byte $2c   ; $2c ,
  .byte $2d   ; $2d -
  .byte $2e   ; $2e .
  .byte $2f   ; $2f /
  .byte $30   ; $30 0
  .byte $31   ; $31 1
  .byte $32   ; $32 2
  .byte $33   ; $33 3
  .byte $34   ; $34 4
  .byte $35   ; $35 5
  .byte $36   ; $36 6
  .byte $37   ; $37 7
  .byte $38   ; $38 8
  .byte $39   ; $39 9
  .byte $3a   ; $3a :
  .byte $3b   ; $3b ;
  .byte $3c   ; $3c <
  .byte $3d   ; $3d =
  .byte $3e   ; $3e >
  .byte $3f   ; $3f ?
  .byte $40   ; $40 @
  .byte $61   ; $41 a
  .byte $62   ; $42 b
  .byte $63   ; $43 c
  .byte $64   ; $44 d
  .byte $65   ; $45 e
  .byte $66   ; $46 f
  .byte $67   ; $47 g
  .byte $68   ; $48 h
  .byte $69   ; $49 i
  .byte $6a   ; $4a j
  .byte $6b   ; $4b k
  .byte $6c   ; $4c l
  .byte $6d   ; $4d m
  .byte $6e   ; $4e n
  .byte $6f   ; $4f o
  .byte $70   ; $50 p
  .byte $71   ; $51 q
  .byte $72   ; $52 r
  .byte $73   ; $53 s
  .byte $74   ; $54 t
  .byte $75   ; $55 u
  .byte $76   ; $56 v
  .byte $77   ; $57 w
  .byte $78   ; $58 x
  .byte $79   ; $59 y
  .byte $7a   ; $5a z
  .byte $5b   ; $5b [
  .byte $5c   ; $5c \ (Pound)
  .byte $5d   ; $5d ]
  .byte $5e   ; $5e ^
  .byte $1b   ; $5f ESC ( <- )
  .byte $00   ; $60 
  .byte $41   ; $61 A
  .byte $42   ; $62 B
  .byte $43   ; $63 C
  .byte $44   ; $64 D
  .byte $45   ; $65 E
  .byte $46   ; $66 F
  .byte $47   ; $67 G
  .byte $48   ; $68 H
  .byte $49   ; $69 I
  .byte $4a   ; $6a J
  .byte $4b   ; $6b K
  .byte $4c   ; $6c L
  .byte $4d   ; $6d M
  .byte $4e   ; $6e N
  .byte $4f   ; $6f O
  .byte $50   ; $70 P
  .byte $51   ; $71 Q
  .byte $52   ; $72 R
  .byte $53   ; $73 S
  .byte $54   ; $74 T
  .byte $55   ; $75 U
  .byte $56   ; $76 V
  .byte $57   ; $77 W
  .byte $58   ; $78 X
  .byte $59   ; $79 Y
  .byte $5a   ; $7a Z
  .byte $00   ; $7b
  .byte $00   ; $7c
  .byte $00   ; $7d
  .byte $00   ; $7e
  .byte $00   ; $7f
  .byte $00   ; $80
  .byte $00   ; $81
  .byte $00   ; $82
  .byte $00   ; $83
  .byte $00   ; $84
  .byte $00   ; $85 (f1)
  .byte $00   ; $86 (f3)
  .byte $00   ; $87 (f5)
  .byte $00   ; $88 (f7)
  .byte $00   ; $89 (f2)
  .byte $00   ; $8a (f4)
  .byte $00   ; $8b (f6)
  .byte $00   ; $8c (f8)
  .byte $00   ; $8d (Shift RET)
  .byte $00   ; $8e
  .byte $00   ; $8f
  .byte $00   ; $90
  .byte $ff   ; $91 (crsr up)
  .byte $00   ; $92
  .byte $00   ; $93 (Shift Clr/Home)
  .byte $7f   ; $94 DEL (Shift Ins/Del)
  .byte $00   ; $95
  .byte $00   ; $96
  .byte $00   ; $97
  .byte $00   ; $98
  .byte $00   ; $99
  .byte $00   ; $9a
  .byte $00   ; $9b
  .byte $00   ; $9c
  .byte $ff   ; $9d (crsr left)
  .byte $00   ; $9e
  .byte $00   ; $9f
  .byte $00   ; $a0 (Shift Space)
  .byte $00   ; $a1
  .byte $00   ; $a2
  .byte $00   ; $a3
  .byte $00   ; $a4
  .byte $00   ; $a5
  .byte $00   ; $a6
  .byte $00   ; $a7
  .byte $00   ; $a8
  .byte $7c   ; $a9 | (Shift Pound)
  .byte $00   ; $aa
  .byte $00   ; $ab
  .byte $fe   ; $ac  C= D
  .byte $00   ; $ad
  .byte $fe   ; $ae  C= S
  .byte $00   ; $af
  .byte $fe   ; $b0  C= A
  .byte $00   ; $b1
  .byte $fe   ; $b2  C= R
  .byte $00   ; $b3
  .byte $00   ; $b4
  .byte $00   ; $b5
  .byte $fe   ; $b6  C= L
  .byte $00   ; $b7
  .byte $00   ; $b8
  .byte $00   ; $b9
  .byte $60   ; $ba ` ( Shift @ )
  .byte $00   ; $bb
  .byte $fe   ; $bc  C= C
  .byte $00   ; $bd
  .byte $00   ; $be
  .byte $fe   ; $bf  C= B
  .byte $5f   ; $c0 _ ( Shift * )
  .byte $41   ; $c1 -----
  .byte $42   ; $c2
  .byte $43   ; $c3 capital
  .byte $44   ; $c4
  .byte $45   ; $c5 letters
  .byte $46   ; $c6
  .byte $47   ; $c7 generate
  .byte $48   ; $c8 
  .byte $49   ; $c9 these 
  .byte $4a   ; $ca
  .byte $4b   ; $cb codes
  .byte $4c   ; $cc
  .byte $4d   ; $cd
  .byte $4e   ; $ce
  .byte $4f   ; $cf
  .byte $50   ; $d0
  .byte $51   ; $d1
  .byte $52   ; $d2
  .byte $53   ; $d3
  .byte $54   ; $d4
  .byte $55   ; $d5
  .byte $56   ; $d6
  .byte $57   ; $d7
  .byte $58   ; $d8
  .byte $59   ; $d9
  .byte $5a   ; $da -----
  .byte $7b   ; $db { ( Shift + )
  .byte $00   ; $dc   ( C= -   )
  .byte $7d   ; $dd } ( Shift - )
  .byte $7e   ; $de ~ ( Pi )
  .byte $00   ; $df
  .byte $00   ; $e0
  .byte $00   ; $e1
  .byte $00   ; $e2
  .byte $00   ; $e3
  .byte $00   ; $e4
  .byte $00   ; $e5
  .byte $00   ; $e6
  .byte $00   ; $e7
  .byte $00   ; $e8
  .byte $00   ; $e9
  .byte $00   ; $ea 
  .byte $00   ; $eb
  .byte $00   ; $ec
  .byte $00   ; $ed
  .byte $00   ; $ee
  .byte $00   ; $ef
  .byte $00   ; $f0
  .byte $00   ; $f1
  .byte $00   ; $f2
  .byte $00   ; $f3
  .byte $00   ; $f4
  .byte $00   ; $f5
  .byte $00   ; $f6
  .byte $00   ; $f7
  .byte $00   ; $f8
  .byte $00   ; $f9
  .byte $00   ; $fa
  .byte $00   ; $fb
  .byte $00   ; $fc
  .byte $00   ; $fd
  .byte $00   ; $fe
  .byte $00   ; $ff

init_screen_msg:
.byte 10,10,10
.byte "KipperTerm 128",13
.byte "(c) 2015 Sean Peck",13
.byte 0



	
ROM_FONT:
.incbin "../inc/vt100_font.bin"

;-- LICENSE FOR c64_vt100.s --
; The contents of this file are subject to the Mozilla Public License
; Version 1.1 (the "License"); you may not use this file except in
; compliance with the License. You may obtain a copy of the License at
; http://www.mozilla.org/MPL/
; 
; Software distributed under the License is distributed on an "AS IS"
; basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
; License for the specific language governing rights and limitations
; under the License.
; 
; The Initial Developer of the Original Code is Lars Stollenwerk.
; 
; Portions created by the Initial Developer are Copyright (C) 2003
; Lars Stollenwerk. All Rights Reserved.  
;
;Contributor(s): Jonno Downes
; -- LICENSE END --
