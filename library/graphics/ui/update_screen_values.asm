; Update the screen values
.proc update_screen_values

@print_midi_io_base:
	lda #$18
	ldy #$04
	jsr graphics::drawing::goto_xy
	lda #>MIDI_IO_BASE
	jsr graphics::drawing::print_hex
	lda #<MIDI_IO_BASE
	jsr graphics::drawing::print_hex

@print_baud_rate:
	lda #$18
	ldy #$05
	jsr graphics::drawing::goto_xy
	lda zp_BAUD_RATE + 1
	jsr graphics::drawing::print_hex
	lda zp_BAUD_RATE 
	jsr graphics::drawing::print_hex

@print_scratch_values:
	lda #$18
	ldy #$06
	jsr graphics::drawing::goto_xy
	lda zp_SCRATCH_VALUE
  pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$06
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

	lda #$18
	ldy #$07
	jsr graphics::drawing::goto_xy
	lda SCRATCH
  pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$07
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

@modem_status:
	lda #$18
	ldy #$08
	jsr graphics::drawing::goto_xy
	lda MODEM_STATUS
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$08
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

@fifo_shadow:
	lda #$18
	ldy #$09
	jsr graphics::drawing::goto_xy
	lda zp_FIFO_SHADOW
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$09
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

@interrupt_enable:
	lda #$18
	ldy #$0A
	jsr graphics::drawing::goto_xy
	lda INTERRUPT_ENABLE
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$0A
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

@interrupt_status:
	lda #$18
	ldy #$0B
	jsr graphics::drawing::goto_xy
	lda INTERRUPT_IDENT
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$0B
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

@line_status:
	lda #$18
	ldy #$0C
	jsr graphics::drawing::goto_xy
	lda LINE_STATUS
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$0C
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

@rx_buffer:
	lda #$18
	ldy #$0D
	jsr graphics::drawing::goto_xy
	lda zp_RX_BUFFER_SHADOW
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$0D
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary
.endproc