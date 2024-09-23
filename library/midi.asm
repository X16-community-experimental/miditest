.scope midi

	CC_MAX = $7F

	.scope message
		CLOCK = $F8
		PROG_CHANGE = $C0		; Nibble = channel
		CC_CHANGE = $B0			; Nibble = channel
		RESET = $FF

		.scope cc
			BANK_SELECT 				= $00
			MODWHEEL 						= $01
			PORTAMENTO_TIME 		= $05
			VOLUME 							= $07
			PAN									= $0A
			EXPRESSION					= $0B
			SUSTAIN							= $40
			PORTAMENT_ENABLE		= $41
			SOSTENUTO						= $42
			SOFT                = $43
			EFFECTS_LEVEL				= $5B			; Reverb for SAM
			CHORUS_LEVEL 				= $5D
			
		.endscope
	.endscope

	.scope sam2695
		.scope cc
			REVERB_PROGRAM			= $50
			CHORUS_PROGRAM			= $51
			REVERB_LEVEL				= $5B			; Reverb for SAM
		.endscope

		.scope startup
			REVERB_SEND_LEVEL		= $00
			CHORUS_SEND_LEVEL		= $00
		.endscope

		.proc reset
			ldy #TX_HOLDING_OFFSET
			lda #midi::message::RESET
			sta (zp_WAVETABLE_IO_BASE),y
			stz zp_CURRENT_PATCH
			jsr change_patch
			rts
		.endproc
	.endscope

	; channel = zp_MIDI_CHANNEL
	; a = value
	; x = cc number
	.proc send_cc
		pha
		phx

		ldy #TX_HOLDING_OFFSET

		lda #midi::message::CC_CHANGE
		ora zp_MIDI_CHANNEL
		sta (zp_WAVETABLE_IO_BASE),y

		; CC number
		pla
		sta (zp_WAVETABLE_IO_BASE),y

		; CC value
		pla 
		sta (zp_WAVETABLE_IO_BASE),y

		rts
	.endproc

	.proc read_midi_in
		; Check LSR0 and if 1, we have data to read
		; We do it this way in case a $00 value comes in
		; which can happen during a number of cases (e.g. NOTE-ON with zero volume)
		ldy #LINE_STATUS_OFFSET
		lda (zp_CURRENT_CARD),y
		and #%00000001
		bne @byte
		rts
	@byte:
		; We haz byte, so grab the byte and set the flag
		ldy #RX_BUFFER_OFFSET
		lda (zp_CURRENT_CARD),y
		sta zp_MIDI_IN_BYTE
		lda #$01
		sta zp_MIDI_IN_FLAG
		rts
	.endproc

	.proc forward_to_wavetable
		lda zp_MIDI_IN_BYTE
		ldy #TX_HOLDING_OFFSET
		sta (zp_WAVETABLE_IO_BASE),y
		stz zp_MIDI_IN_BYTE
		stz zp_MIDI_IN_FLAG
		rts
	.endproc

.endscope