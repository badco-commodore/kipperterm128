.export draw_window
.export top
.export left
.export color



.segment "APP_SCRATCH" 

right: .res 1
left:  .res 1
top:   .res 1
bottom: .res 1
color: .res 1
height:	 .res 1
width:	 .res 1
X204A:	.res 1
; -------------------------------------
; constant declaration
;
; -------------------------------------

scbot = $e4
sctop = $e5
sclf = $e6
scrt = $e7
esc = $1b
attribute = $f1
JBSOUT = $ffd2
PLOT = $fff0
	
.code

	;; 				L347B:  ; Screen Window  to full screen #18,0,0,#47  t=0 b=#$18(24) l=0 r=#$4f(79)  
max_window:	
	lda	#$00     ;clear e5/e6
	sta	sctop
	sta	sclf
	lda	#$4F
	sta	scrt   ; #$ 47 into E7
	lda	#$18
	sta	scbot   ;#$18 int E4
	rts

draw_window:     ; DRAW THE WINDOW to the screen and set 128 windowing routines to boundaries
					   ; some damned ugly code, but works
					   ; 2041 is WINDOW LEFT, 2042 is Window top
					   ; 2043 is ATTRIBUTE to draw window lines with
					   ; X is window right  stored in 2046
 					   ; Y is window bottom  store in 2047

	pha
        txa
	pha
	tya
	pha
	lda	attribute  ; store attributes and x,y
	pha
	stx	right  ; window right
	sty	bottom  ; window botom
	ldy	left
	ldx	top
	clc
	jsr	PLOT  ; plot to values 2042,2041
	lda	color
	sta	attribute  ; set attributes to value at 2043
	lda	#$B0
	jsr	JBSOUT  ; output a 176 (upper right corner
	lda	right  ; get window RIGHT setting
	sec
	sbc	left  ; take away window LEFT setting
	tax     ; put result in x
	dex     ; decrease by 1
	stx	width  ; save it in 2048  ; (size of window left to right
	jsr	draw_line   ; draw horizontal line
        lda	#$AE   ; print upper right bracket
	jsr	JBSOUT
	lda	bottom  ; get bottom of Window
	sec
	sbc	top  ; take away  top of Window
	tax
	dex           ; dec 1 from it
	stx	height ; store in 2049 (HEIGHT OF WINDOW)
	ldx	top ; grab 2042  ; TOP
	inx           ; add 1 to it
	stx	X204A ; put the new value in 204a  (Top+1, top of text area?)  ; 204A is current row
	;; 	lda	#$00
	;; 	sta	X204B  ; clear 204b  (use as counter for number of rows)  ; 204B is counter 
@loop:
	ldy	left  ; y = left
	ldx	X204A  ; x = top of text area
	clc
	jsr	PLOT  ; plot   
	lda	#$DD   ; print vertical bar "|"
	jsr	JBSOUT
	ldy	right  ; plot to 204A,2046  (top of text area, right)
	ldx	X204A  ; 
	clc
	jsr	PLOT
	lda	#$DD   ; print vertical bar "|"
	jsr	JBSOUT
	inc	X204A  ;increment 204a (row)
	;; 	inc	X204B  ; increment 204b
	;; 	lda	X204B
	lda     X204A
	;	cmp	height  ; have we done the loop hight of window times
        cmp     bottom
	bne	@loop  ; have we drawn all the verticals?  If not loop back to 5110
	clc
	ldx	bottom  ; bottom
	ldy	left  ; left
	jsr	PLOT  ; plot to 2047,2041
	lda	#$AD
	jsr	JBSOUT ; print lower left bracket
	jsr	draw_line ; print horizontal line
	lda	#$BD  ; print right lower bracket
	jsr	JBSOUT
	ldx	bottom ; set window size for c128 windowing routines
	dex         ; take 1 away for window
	stx	scbot ; Bottom Row
	ldx	top
	inx           ; add 1 window for line
	stx	sctop  ; Top Row
	ldx	left
	inx            ; add 1 for window line
	stx	sclf  ; LEFT col
	ldx	right
	dex            ; dec 1 for window line
	stx	scrt  ; right col
	pla
	sta	attribute   ; restore attributes
	lda	#$93    ; clear the new window
	jsr	JBSOUT
	pla     ; restore registers and exit
	tay
	pla
	tax
	pla
	rts
				;
	
draw_line:              ; draw horizontal line for windows
						    ; 2048 holds the number of horizontal lines to print
						    ; why not just pass in with x and dec?
	ldx	width
@loop:
	lda	#$C0
	jsr	JBSOUT
        dex
	bne	@loop
	rts          
	
