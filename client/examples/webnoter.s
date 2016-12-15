
.include "../inc/common.i"

.ifndef KPR_API_VERSION_NUMBER
  .define EQU     =
  .include "../inc/kipper_constants.i"
.endif

print_a = $ffd2


.export musicdata
.export musicdata_size
.export sprite_font
.export html
.export scroll_template_1
.export scroll_template_2

.import  __DATA6K_LOAD__
.import  __DATA6K_RUN__
.import  __DATA6K_SIZE__

SCREEN_RAM 	= $0400
COLOUR_RAM 	= $d800
VIC_CTRL_A	= $d011
VIC_RASTER_REG 	= $d012
VIC_CTRL_B 	= $d016
VIC_MEMORY_CTRL=$d018
VIC_IRQ_FLAG	= $d019

copy_src  = $24


IRQ_VECTOR=$fffe

MUSIC_BASE=$1000 ;where we relocate our music routine to
PLAYER_INIT=0
PLAYER_PLAY=3

BORDER_COLOR	= $d020
BACKGROUND_COLOR_0 = $d021
SCROLL_DELAY=4
CHARS_PER_LINE	= 40
SCROLL_LINE=12
SCROLL_RAM 	= SCREEN_RAM+(SCROLL_LINE*CHARS_PER_LINE)

TOP_BORDER_SCAN_LINES = 50


BLACK = 0
WHITE  = 1
RED  = 2
CYAN  = 3
PURPLE  = 4
GREEN  = 5
BLUE  = 6
YELLOW  = 7
ORANGE  = 8
BROWN  = 9
LIGHT_RED  = 10
DARK_GRAY =  11
GRAY  = 12
LIGHT_GREEN  = 13
LIGHT_BLUE  = 14
LIGHT_GRAY  = 15

.macro  start_irq
  pha
  txa
  pha
  tya
  pha
  
.endmacro


.macro  end_irq
  pla
  tay
  pla
  tax
  pla  
.endmacro

.macro wait_next_raster
	lda	VIC_RASTER_REG
@loop:
	cmp	VIC_RASTER_REG
	beq	@loop
.endmacro

.macro  kippercall function_number
  ldy function_number
  jsr KPR_DISPATCH_VECTOR   
.endmacro

.zeropage
temp_buff: .res 2
pptr: .res 2

.segment "STARTUP"    ;this is what gets put at the start of the file on the C64

.word basicstub		; load address

basicstub:
	.word @nextline
	.word 2003
	.byte $9e 
	.byte <(((init / 1000) .mod 10) + $30)
	.byte <(((init / 100 ) .mod 10) + $30)
	.byte <(((init / 10  ) .mod 10) + $30)
	.byte <(((init       ) .mod 10) + $30)
	.byte 0
@nextline:
	.word 0

init:

  ldax #KPR_CART_SIGNATURE  ;where signature should be in cartridge (if cart is banked in)
look_for_signature:
  stax temp_buff
  ldy #5
@check_one_byte:
  lda (temp_buff),y
  cmp kipper_signature,y
  bne @bad_match  
  dey 
  bpl @check_one_byte  
  jmp @found_kipper_signature
  
@bad_match:
  ldax #kipper_api_not_found_message
  jsr print
@loop:
  jmp @loop
  rts
@found_kipper_signature:

  ldax #init_msg
  jsr print
  kippercall #KPR_INITIALIZE
	bcc @init_ok
  jsr print_cr
  ldax #failed_msg
  jsr print
  jsr print_cr
  jsr print_errorcode
  jmp reset_after_keypress    
@init_ok:

;if we got here, we have found the KIPPER API and initialised the IP stack

  
  ;copy our runtime data to where it lives
  ldax #__DATA6K_LOAD__
  stax param_buffer+KPR_BLOCK_SRC
  ldax #__DATA6K_RUN__
  stax param_buffer+KPR_BLOCK_DEST
  ldax #__DATA6K_SIZE__
  stax param_buffer+KPR_BLOCK_SIZE    
  ldax #param_buffer
  kippercall #KPR_BLOCK_COPY



  ;copy our music data up to a temp buffer (in case it gets overwritten by 
  ;one of the other unpacking moves)
  ldax #musicdata+2 ;skip over the 2 byte address
  stax param_buffer+KPR_BLOCK_SRC
  ldax #download_buffer
  stax param_buffer+KPR_BLOCK_DEST
  ldax #musicdata_size-2 ;don't copy  the 2 byte address
  stax param_buffer+KPR_BLOCK_SIZE    
  ldax #param_buffer
  kippercall #KPR_BLOCK_COPY

  ;copy our font data and the sprite data to $3000..$3fff
  ldax #charset_font
  stax param_buffer+KPR_BLOCK_SRC
  ldax #$3000
  stax param_buffer+KPR_BLOCK_DEST
  ldax #MUSIC_BASE
  stax param_buffer+KPR_BLOCK_SIZE    
  ldax #param_buffer
  kippercall #KPR_BLOCK_COPY
  
  ;should now be now safe to copy the music data back down 
  ;copy our music data to $1000..$1fff
  ldax #download_buffer
  stax param_buffer+KPR_BLOCK_SRC
  ldax #MUSIC_BASE
  stax param_buffer+KPR_BLOCK_DEST
  ldax #musicdata_size-2 ;don't copy  the 2 byte address
  stax param_buffer+KPR_BLOCK_SIZE    
  ldax #param_buffer
  kippercall #KPR_BLOCK_COPY


  ldax #scroll_template_1
  jsr setup_static_scroll_text  
  
	lda 	#0
	jsr	clear_screen
	lda	#DARK_GRAY
	sta	BORDER_COLOR
  lda #YELLOW
  sta BACKGROUND_COLOR_0

  lda #$1c ; use charset at $3000
  sta VIC_MEMORY_CTRL


	sei		;disable maskable IRQs

	lda	#$7f
	sta 	$dc0d	;disable timer interrupts which can be generated by the two CIA chips
	sta	$dd0d	;the kernal uses such an interrupt to flash the cursor and scan the keyboard, so we better
			;stop it.

	lda	$dc0d	;by reading this two registers we negate any pending CIA irqs.
	lda	$dd0d	;if we don't do this, a pending CIA irq might occur after we finish setting up our irq.
			;we don't want that to happen.

	lda	#$01	;this is how to tell the VICII to generate a raster interrupt
	sta	$d01a

	lda	#$00	;this is how to tell at which rasterline we want the irq to be triggered
	sta	VIC_RASTER_REG


  ;copy KERNAL to the RAM underneath, in case any ip65 routines need it
  ldax #$e000
  stax param_buffer+KPR_BLOCK_SRC
  stax param_buffer+KPR_BLOCK_DEST
  ldax #$1FFF
  stax param_buffer+KPR_BLOCK_SIZE    
  ldax #param_buffer
  kippercall #KPR_BLOCK_COPY

  ;copy KIPPER cart to the RAM underneath, so we can swap it out and modify the IRQ vector
  ldax #$8000
  stax param_buffer+KPR_BLOCK_SRC
  stax param_buffer+KPR_BLOCK_DEST
  ldax #$4000
  stax param_buffer+KPR_BLOCK_SIZE    
  ldax #param_buffer
  kippercall #KPR_BLOCK_COPY


	lda	#$35   	;we turn off the BASIC and KERNAL rom here, so we can overwrite the IRQ vector at $fffe
	sta 	$01	  ;the cpu now sees RAM everywhere except at $d000-$e000, where still the registers of
              ;SID/VICII/etc are visible

  lda #((SPRITE_FRAMES-1)*8)
  sta sprite_text_frame
  jsr setup_sprites
  
  jsr setup_music

  
	jsr	set_next_irq_jump
	cli		;enable maskable interrupts again


;position for the first text

  jsr reset_input_buffer



start_web_server:
  ldax #httpd_callback
  kippercall #KPR_HTTPD_START ;this will only return if there is an error
  rts

	
reset_input_buffer:  

  lda new_message
  beq @no_message_yet
  ldax #scroll_template_2
  jsr setup_static_scroll_text
  ldx #0
  stx new_message
  
@no_message_yet:  
  ldax #scroll_buffer
  stax current_input_ptr  
  rts
  
httpd_callback:
  lda #'h'
  kippercall #KPR_HTTPD_GET_VAR_VALUE
  bcs @not_complete_fields
  stax  copy_src  
  ldy #0
  lda (copy_src),y
  beq @not_complete_fields
       
@copy_handle_loop:  
  lda (copy_src),y
  beq @end_of_handle
  sta handle,y
  iny
  bne @copy_handle_loop
@end_of_handle:
  lda #0
  sta handle,y
  lda #'m'
  kippercall #KPR_HTTPD_GET_VAR_VALUE  
  bcs @not_complete_fields
  stax  copy_src  
  ldy #0
  lda (copy_src),y
  beq @not_complete_fields

@copy_message_loop:
  lda (copy_src),y
  beq @end_of_message
  sta message_text,y
  iny
  bne @copy_message_loop
@end_of_message: 
  lda #0
  sta message_text,y
  inc new_message

@not_complete_fields:

  ldax #html
  ldy #2 ;text/html
  clc
  rts  


;set up the  tune
setup_music:
  lda #$00                    ;init subtune 0
  jsr MUSIC_BASE+PLAYER_INIT
  rts

setup_sprites:

  ;turn on all 8 sprites
  lda #$FF
  sta $d015
  
  ldx #$07
@setup_sprite:  
  ;position each sprite
  txa
  asl
  tay
  lda  sprite_x_pos,x
  sta $d000,y   ;sprite 0 X pos (LSB)
  lda  #0
  sta $d001,y   ;sprite 0 Y pos 
  
  ;colour sprite 0
  lda #BLUE
  sta $d027,x ;sprite 0 color
  
  ;select bitmap for sprite
  dex
  bpl @setup_sprite
  
  lda sprite_x_msb
  sta $d010
  
  ;turn on multicolor mode for all 8 sprites
  lda #$FF
  sta $d01c
  
  sta sprite_ticker
  
  lda #DARK_GRAY
  sta $d025 ;sprite multicolor register 0
  lda #LIGHT_GRAY
  sta $d026 ;sprite multicolor register 1
  
  rts


clear_screen:
  
	ldx	#$00
	lda	#$20

:
	sta	SCREEN_RAM,x
	sta	SCREEN_RAM+$100,x
	sta	SCREEN_RAM+$200,x
	sta	SCREEN_RAM+$300,x
	sta	COLOUR_RAM,x
	sta	COLOUR_RAM+$100,x
	sta	COLOUR_RAM+$200,x
	sta	COLOUR_RAM+$300,x

	dex
	bne	:-
	rts		

set_next_irq_jump:
	inc	jump_counter

	lda	jump_counter
	asl
	asl
load_next_raster_entry:
	tax
	lda	raster_jump_table,x	;bit 9 of raster to trigger on

	cmp	#$ff
	bne	not_last_entry
	lda	#0
	sta	jump_counter
	jmp	load_next_raster_entry
not_last_entry:
	ora	#$18	;turn on bits 3 & 4 
	sta	VIC_CTRL_A
	lda	raster_jump_table+1,x	;bits 0..7 of raster to trigger on
	sta 	VIC_RASTER_REG
	lda	raster_jump_table+2,x	;LSB of IRQ handler
	sta	IRQ_VECTOR
	lda	raster_jump_table+3,x	;LSB of IRQ handler
	sta	IRQ_VECTOR+1
	rts

exit_from_irq:
	jsr	set_next_irq_jump
	lda #$ff	;this is the orthodox and safe way of clearing the interrupt condition of the VICII.
	sta VIC_IRQ_FLAG;if you don't do this the interrupt condition will be present all the time and you end
			;up having the CPU running the interrupt code all the time, as when it exists the
			;interrupt, the interrupt request from the VICII will be there again regardless of the
			;rasterline counter.

  end_irq
	rti



scroll_text_irq:
  start_irq
	lda	scroll_timer
	bne	done_scrolling_message
	lda	#SCROLL_DELAY
	sta	scroll_timer
	ldx	#1
	ldy	#0
scroll_1_char:
	lda	SCROLL_RAM,x
	sta	SCROLL_RAM,y
	lda	SCROLL_RAM+40,x
	sta	SCROLL_RAM+40,y

	inx
	iny
	cpx	#CHARS_PER_LINE
	bne	scroll_1_char
	
	
	jsr get_a
  cmp #0
	beq	last_char_in_message
	sta	SCROLL_RAM+CHARS_PER_LINE-1
	clc
	adc	#$80
	sta	SCROLL_RAM+CHARS_PER_LINE+40-1

	jmp	done_scrolling_message


last_char_in_message:
	jsr reset_input_buffer

done_scrolling_message:
	dec	scroll_timer

	jmp	exit_from_irq


scroll_timer:		.byte 	1
scroll_counter:		.byte 	0


pixel_scroll_irq:	
  start_irq
  wait_next_raster
; set X scroll offset
	lda	scroll_timer
	clc
	asl
	clc
	and	#$07
	sta	VIC_CTRL_B

	jmp	exit_from_irq

move_sprites_irq:
  start_irq
  wait_next_raster

  inc sprite_ticker
  bne @sprite_not_offscreen
  clc
  lda sprite_text_frame
  adc #8
  cmp #8*SPRITE_FRAMES
  bne @not_last_frame
  lda #0
@not_last_frame:  
  sta sprite_text_frame
  ldx #$07
@set_sprite_text:  
  clc
  txa 
  adc sprite_text_frame
  tay
  lda sprite_text,y
  cmp #' '
  bne @not_space
  lda #'`'  ;map a space in the sprite text to 0x60,so it ends up with font offset 0x1F after subbing 'A'
@not_space:  
  sec
  sbc #'A'
  clc
  adc #<($3800/64)  ;sprite font should be relocated here
  sta SCREEN_RAM+$03f8,x
  dex
  bpl @set_sprite_text

@sprite_not_offscreen:
  ldy sprite_ticker
  ldx #$0e    ;7*2  
@position_sprite:  
  lda sprite_y_pos,y      
  sta $d001,x   ;sprite 0 Y pos 

  ;change colour when the sprite goes off screen
;  cmp #$ff  ;are we off the screen?
;  bne @not_off_screen
;  txa
;  pha
;  lsr
;  tax
;  inc $d027,x ;sprite  color
;  pla
;  tax
;@not_off_screen:
  
  txa
  beq @y_set
  dex
  dex
@mod_y:
  iny
  iny
  iny
  dex
  dex
  bpl @mod_y
@y_set:
  
  tax
  dex
  dex  
  bpl @position_sprite
  
	jmp	exit_from_irq


setup_static_scroll_text:  
  stax current_input_ptr
  lda #0
  sta new_message
  ldax #scroll_buffer
  stax current_output_ptr
@next_byte:
  jsr get_a
  beq @next_byte  
  cmp #'%'
  beq @operator
  pha
  jsr emit_a
  pla
  bne @next_byte  
  rts
  
@operator:  
  jsr get_a
  cmp #'m'
  bne @not_message
  ldx #0
:  
  lda message_text,x
  beq @next_byte  
  cmp #$20
  bpl @not_ctrl_char
  lda #' '
@not_ctrl_char:  
  jsr emit_a
  inx
  bne :-
  
@not_message:
  cmp #'h'
  bne @not_handle
  ldx #0
  :  
  lda handle,x  
  beq @next_byte  
  jsr emit_a
  inx
  bne :-
  
@not_handle:

  cmp #'i'
  bne @not_ip
  lda #KPR_CFG_IP
  jmp @loaded_offset
@not_ip:
  cmp #'g'
  bne @not_gateway
  lda #KPR_CFG_GATEWAY
  jmp @loaded_offset
@not_gateway:
  cmp #'d'
  bne @not_dns
  lda #KPR_CFG_DNS_SERVER
  jmp @loaded_offset
@not_dns:
  jmp @next_byte

@loaded_offset:  
  sta param_offset
  kippercall #KPR_GET_IP_CONFIG
  adc param_offset
  bcc :+
  inx
:  
  jsr emit_dotted_quad
  

  jmp @next_byte

;emit the 4 bytes pointed at by AX as dotted decimals
emit_dotted_quad:
  sta pptr
	stx pptr + 1
  ldy #0
  lda (pptr),y
  jsr emit_decimal 
  lda #'.'
  jsr emit_a

  ldy #1
  lda (pptr),y
  jsr emit_decimal 
  lda #'.'
  jsr emit_a

  ldy #2
  lda (pptr),y
  jsr emit_decimal 
  lda #'.'
  jsr emit_a

  ldy #3
  lda (pptr),y
  jsr emit_decimal
  
  rts

emit_decimal:  ;emit byte in A as a decimal number
  pha
  sta temp_bin   ;save 
  sed       ; Switch to decimal mode
  lda #0		; Ensure the result is clear
  sta temp_bcd
  sta temp_bcd+1
  ldx #8  ; The number of source bits		
  :
  asl temp_bin+0		; Shift out one bit
	lda temp_bcd+0	; And add into result
  adc temp_bcd+0
  sta temp_bcd+0
  lda temp_bcd+1	; propagating any carry
  adc temp_bcd+1
  sta temp_bcd+1
  dex		; And repeat for next bit
	bne :-
  
  cld   ;back to binary
      
  pla       ;get back the original passed in number
  bmi @emit_hundreds ; if N is set, the number is >=128 so emit all 3 digits
  cmp #10
  bmi @emit_units
  cmp #100
  bmi @emit_tens
@emit_hundreds:
  lda temp_bcd+1   ;get the most significant digit
  and #$0f
  clc
  adc #'0'
  jsr emit_a

@emit_tens:
  lda temp_bcd
  lsr
  lsr
  lsr
  lsr
  clc
  adc #'0'
  jsr emit_a
@emit_units:
  lda temp_bcd
  and #$0f
  clc
  adc #'0'
  jsr emit_a
  
  rts

reset_after_keypress:
  ldax #press_a_key_to_continue    
  jsr print
@wait_key:
  jsr $f142 ;not officially documented - where F13E (GETIN) falls through to if device # is 0 (KEYBD)
  beq @wait_key
  jmp $fce2   ;do a cold start

print_errorcode:
  ldax #error_code
  jsr print
  kippercall #KPR_GET_LAST_ERROR
  kippercall #KPR_PRINT_HEX
  jmp print_cr





play_music_irq:
;	inc	BORDER_COLOR
  start_irq
  jsr MUSIC_BASE+PLAYER_PLAY  
;  dec	BORDER_COLOR
	jmp	exit_from_irq


print:
	sta pptr
	stx pptr + 1
	
@print_loop:
  ldy #0
  lda (pptr),y
	beq @done_print
	jsr print_a
	inc pptr
	bne @print_loop
  inc pptr+1
  bne @print_loop ;if we ever get to $ffff, we've probably gone far enough ;-)
@done_print:
  rts

print_cr:
  lda #13
  jmp print_a

.data

sprite_ticker: .byte 0

emit_a:
current_output_ptr=emit_a+1
  sta $ffff
  inc current_output_ptr
  bne :+
  inc current_output_ptr+1
:  
  rts

get_a:
current_input_ptr=get_a+1
  lda $ffff
  inc current_input_ptr
  bne :+
  inc current_input_ptr+1
:  
  rts


raster_jump_table:
	;format:
	;offset	meaning
	; $00	BIT 9 OF RASTER TO TRIGGER ON ($00  if bit 8 =0 , $80 if bit 8 =1)
	; $01	BITS 0..7 OF RASTER TO TRIGGER ON
	; $02	LSB OF ROUTINE TO JUMP TO
	; $03	MSB OF ROUTINE TO JUMP TO
	;table needs to end with a single byte $ff
	;table needs to be sorted by scanlines
	.byte	$0,$01
  .word scroll_text_irq
 	.byte	$0,$20
  .word pixel_scroll_irq

 
  .byte	$0,$80
  .word play_music_irq

 	.byte	$80,$05
    .word move_sprites_irq

	.byte	$ff	;end of list

jump_counter:	.byte	0

.data
kipper_api_not_found_message:
  .byte "ERROR - KIPPER API NOT FOUND.",13,0

failed_msg:
	.byte "FAILED", 0

ok_msg:
	.byte "OK", 0
 

init_msg:
  .byte " INITIALIZING ",0

press_a_key_to_continue:
  .byte "PRESS A KEY TO CONTINUE",13,0

kipper_signature:
.byte $4B,$49,$50,$50,$45,$52 ; "KIPPER"

error_code:  
  .asciiz "ERROR CODE: "

charset_font:
	 .incbin "font16x8.bin"
sprite_font:
  .incbin "spud_letters.spr"

musicdata:
.incbin "powertrain.bin"
musicdata_size=*-musicdata


.segment "DATA6K"


sprite_x_pos:
.byte  $34,$54,$78,$90,$Bb,$Db,$Fb,$1b
  
sprite_x_msb:
.byte $80

sprite_y_pos:
.repeat 128
  .byte 0
.endrep
.include "sine_data.i"

;.include "sine_data.i"


html:
  .incbin "form.html"
.byte 0

scroll_template_1:
.byte "WebNoter - go to http://%i/ and bang away! -"
.byte " ",0

scroll_template_2:
.byte "http://%i/"
.byte " - %h sez '%m' -"
.byte " ",0

sprite_text:
;[\]^_`=0,29?
.byte "WEBNOTER"
.byte "A KIPPER"
.byte " POWERED"
.byte "PRODUCT "
.byte "TUNE IS "
.byte "BY JCH  "
.byte "FROM THE"
.byte "VIBRANTS"
.byte "GREETZ ]"
.byte "ALL AT  "
.byte "DA BEACH"
.byte "PARTY I["
SPRITE_FRAMES=(*-sprite_text)/8












.segment "BSS4K"
;we want our variables to start at $4000, out of the way of our music player and the font data

handle: .res 256
message_text: .res 256

new_message:
.res 1
message_buffer: 
.res 256

param_offset: .res 1

temp_bin: .res 1
temp_bcd: .res 2


param_buffer: .res $20  

.res 10 ;filler
scroll_buffer:
  .res 1000
sprite_text_frame: .res 1
string_offset: .res 1

download_buffer:
download_buffer_length=6000
 .res download_buffer_length

