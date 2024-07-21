; Largely pulled from PetDraw (which I used to draw the status UI elements)
.proc load_screen
load_screen:
	LDA	#%00010000	;INCREMENT SET TO ONE
	STA	VERA_addr_high
	LDX	#00
	LDY	#00
	LDA	#<file_data
	STA	$FB
	LDA	#>file_data
	STA	$FC
CBTS01:
	LDA	#0
	STA	VERA_addr_low
	STX	VERA_addr_med
CBTS02:
	LDA	($FB),Y
	STA	VERA_data0
	INY
	CPY	#SCREEN_X   ; width of 80
	BNE	CBTS02
	LDY	#$00
	LDA	$FB
	CLC
	ADC	#SCREEN_X   ; width of 80
	STA	$FB
	LDA	$FC
	ADC	#00
	STA	$FC
	INX
	CPX	#SCREEN_Y
	BNE	CBTS01
	RTS

.endproc
