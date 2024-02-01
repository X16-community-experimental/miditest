; Inspired greatly by PetDraw
; (https://github.com/commanderx16/x16-demo/blob/master/petdrawx16/petdrawx16.asm)

.proc cursor_plot
  TEMP = r15
cursor_plot:
	LDA	cursor_layer
	STA	VERA_addr_high	;INCREMENT SET TO ZERO
	LDA	cursor_y
	STA	VERA_addr_med
	LDA	cursor_x
	ASL		;MULT BY 2
	CLC
	ADC	#01
	STA	VERA_addr_low
	LDA	VERA_data0	;GET COLOR
	STA	cursor_old_color
	STA TEMP
	ASL			;INVERT COLORS
	ASL
	ASL
	ASL
	LSR	TEMP
	LSR	TEMP
	LSR	TEMP
	LSR	TEMP
	ORA	TEMP
	STA	VERA_data0
	RTS
.endproc

.proc cursor_unplot
cursor_unplot:
	LDA	cursor_layer
	STA	VERA_addr_high	;INCREMENT SET TO ZERO
	LDA	cursor_y
	STA	VERA_addr_med
	LDA	cursor_x
	ASL		;MULT BY 2
	CLC
	ADC	#01
	STA	VERA_addr_low
	LDA	cursor_old_color
	STA	VERA_data0
	RTS
.endproc
