.include "memory/zeropage.asm"
.include "memory/golden.inc"
.include "library/preamble.asm"
.include "library/x16.inc"
.include "library/keyboard.inc"
.include "library/screencodes.inc"
.include "library/variables.inc"
.include "library/macros.inc"
.include "library/graphics/main.asm"
.include "library/files/main.asm"


	; 24 pulses per quarter note
	; Each row is 1/32nd note so 24/4 = 6
	; Just use the song speed, dummy.
	;PULSES_PER_ROW=$06


	; MIDI Default per Kevin, IO6/Low (2Mhz)
	IO6_ADDRESS=$9FC0
	; IO3 (8MHz)
	;IO_ADDRESS=$9F60
	IO7_ADDRESS=$9FE0

	; Offsets for the base I/O address, which lives in the ZP
	; (zp_MIDI_BASE)
	; These are used for indirect-indexed addressing
	RX_BUFFER_OFFSET=$00           ; Read Only
	TX_HOLDING_OFFSET=$00         ; Write Only
	INTERRUPT_ENABLE_OFFSET=$01
	INTERRUPT_IDENT_OFFSET=$02 ; Read only
	FIFO_CONTROL_OFFSET=$02    ; Write only
	LINE_CONTROL_OFFSET=$03
	MODEM_CONTROL_OFFSET=$04
	LINE_STATUS_OFFSET=$05
	MODEM_STATUS_OFFSET=$06
	SCRATCH_OFFSET=$07
	DIVISOR_LATCH_LOW = $00
	DIVISOR_LATCH_HI = $01

	; 32 because
	; 16 Mhz  / 31250 * 16
	MIDI_BAUD_RATE_DIVISOR = $0020

	; Disable Interupts by default
	; (We'll enable in ISRs)
	INTR_SETUP  = %00000000
	;INTR_ENABLE = %00000001
	INTR_ENABLE = %00000101

	LCR_SETUP  = %00000011
	FIFO_CLEAR = %00000110
	FIFO_SETUP = %00000001
	MODEM_SETUP = %00000000
	MODEM_INT_ENABLE = %00001000


;; Scratch Register Test Value
SCRATCH_TEST_VALUE = $2F

; MIDI Values

MIDI_CLOCK = $F8

LEFT_COL = $13
RIGHT_COL = $1E

cursor_old_color: .byte $00
cursor_x: .byte $00
cursor_y: .byte $00
cursor_layer: .byte $00

palette:
.byte $04,$00     ; super dark blue 00
.byte $FF,$0F     ; white 01
.byte $00,$0F     ; red 02
.byte $DF,$0D     ; cyan 03
.byte $0A,$0F     ; purple 04
.byte $40,$00     ; dark green 05
.byte $0F,$00     ; blue 06
.byte $F0,$0F     ; yellow 07
.byte $50,$0F     ; orange 08
.byte $30,$0A     ; brown 09 
.byte $55,$0F     ; light red 0A
.byte $22,$02     ; dark grey 0B
.byte $44,$04     ; grey 0C
.byte $F5,$05     ; light green 0D
.byte $9F,$0A     ; light blue 0E
.byte $88,$08     ; light gray 0F

;; Setup screen and IO card
start:
	rombank #$00
	stz VERA_ctrl      ; Select primary VRAM address
	stz VERA_addr_med  ; Set primary address med byte to 0
	stz VERA_addr_low  ; Set Primary address low byte to 0
	stz VERA_addr_high ; Set primary address bank to 0, stride to 0

	lda #RES128x64x16      ; L0 is the pattern scroll, instrument edit, env edit space
	sta VERA_L0_config
	lda #RES128x64x16       ; L1 is the UI
	sta VERA_L1_config

	; L0 = Pattern Data 
	; ($10000 start of HiVRAM)
	lda #L0_MAPBASE
	sta VERA_L0_mapbase
	; L1 = UI
	; ($00000 start of LoVRAM)
	stz VERA_L1_mapbase

	; Set the default character tiles
	lda #TILEBASE
	sta VERA_L0_tilebase
	sta VERA_L1_tilebase

	; Turn on both layers
	; Field : Sprit E : L1 E : L2 E : NC : Chroma/HV : Output x2
	lda VERA_dc_video
	ora #%00110000
	sta VERA_dc_video
		; Clear pattern VRAM area
	stz r1
	lda #$80
	sta r2
	lda #%00000001 ; Hi RAM
	jsr graphics::vera::clear_vram

	; Clear pattern VRAM area
	stz r1
	lda #$80
	sta r2
	lda #%00000000 ; Lo RAM
	jsr graphics::vera::clear_vram

	lda #%00010000
	sta VERA_addr_high

  jsr graphics::vera::load_palette_16
	lda #$01
	sta zp_TEXT_COLOR

	lda #$10
	ldy #$10
	jsr graphics::drawing::goto_xy


	lda #<IO6_ADDRESS
	sta zp_MIDI_IO_BASE1
	lda #>IO6_ADDRESS
	sta zp_MIDI_IO_BASE1 + 1

	lda #<IO7_ADDRESS
	sta zp_MIDI_IO_BASE2
	lda #>IO7_ADDRESS
	sta zp_MIDI_IO_BASE2 + 1


	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	sta zp_USER_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	sta zp_USER_CARD + 1
	jsr init

	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr init

	print_string_macro text_strings

	lda #$1F
	sta zp_SCREEN_Y
	stz zp_SCREEN_X
	stz zp_LAST_NMI_COUNTER
	stz zp_NMI_COUNTER

@print_addresses:
	lda #LEFT_COL + 1
	ldy #$03
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1 + 1
	jsr graphics::printing::print_hex
	lda zp_MIDI_IO_BASE1
	jsr graphics::printing::print_hex

	lda #RIGHT_COL + 1
	ldy #$03
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2 + 1
	jsr graphics::printing::print_hex
	lda zp_MIDI_IO_BASE2
	jsr graphics::printing::print_hex

	cli


;; Infinite Read Loop
loop:
  jsr GETIN  ;keyboard
	bne @check_keys
	sta zp_KEY_PRESSED
	jmp print_stuff
@check_keys:
@quit:
	cmp #KEY_Q
	bne @one
	jmp exit
@one:
	cmp #KEY_1
	bne @two
	jmp set_card_one
@two:
	cmp #KEY_2
	bne @a
	jmp set_card_two
@a:
	cmp #KEY_A
	bne @i
	lda zp_SCREEN_X
	ldy zp_SCREEN_Y
	jsr graphics::drawing::goto_xy
	lda #SCREENCODE_A
	jsr graphics::printing::print_alpha_char
	jsr inc_screen_position
	bra print_stuff
@i:
	cmp #KEY_I
	bne @m
	lda #INTR_ENABLE
	ldy #INTERRUPT_ENABLE_OFFSET
	sta (zp_USER_CARD),y
	bra print_stuff
@m:
	cmp #KEY_M
	bne @r
	lda #MODEM_INT_ENABLE
	ldy #MODEM_CONTROL_OFFSET
	sta (zp_USER_CARD),y
	bra print_stuff
@r:
	cmp #KEY_R
	bne @t
	lda zp_SCREEN_X
	ldy zp_SCREEN_Y
	jsr graphics::drawing::goto_xy
	ldy #RX_BUFFER_OFFSET
  lda (zp_USER_CARD),y
	jsr graphics::printing::print_hex
	jsr inc_screen_position
	jsr inc_screen_position
	bra print_stuff
@t:
	cmp #KEY_T
	bne @n

	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr init

	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr init

	bra print_stuff
@n:
	cmp #KEY_N
	bne print_stuff
	jsr enable_nmi
	;bra print_stuff

print_stuff:
@print_interrupt_enable:
	lda #LEFT_COL
	ldy #$04
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_interrupt_enable

	lda #RIGHT_COL
	ldy #$04
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_interrupt_ident

@print_interrupt_ident:
	lda #LEFT_COL
	ldy #$05
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_interrupt_ident

	lda #RIGHT_COL
	ldy #$05
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_interrupt_ident

@print_fifo_control:
	lda #LEFT_COL
	ldy #$06
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_fifo_control

	lda #RIGHT_COL
	ldy #$06
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_fifo_control

@print_line_control:
	lda #LEFT_COL
	ldy #$07
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_line_control

	lda #RIGHT_COL
	ldy #$07
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_line_control

@print_modem_control:
	lda #LEFT_COL
	ldy #$08
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_modem_control

	lda #RIGHT_COL
	ldy #$08
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_modem_control

@print_line_status:
	lda #LEFT_COL
	ldy #$09
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_line_status

	lda #RIGHT_COL
	ldy #$09
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_line_status

@print_modem_status:
	lda #LEFT_COL
	ldy #$0A
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_modem_status

	lda #RIGHT_COL
	ldy #$0A
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE2
	sta zp_CURRENT_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_CURRENT_CARD + 1
	jsr print_modem_status

@print_current_card_address:
	lda #LEFT_COL
	ldy #$0D
	jsr graphics::drawing::goto_xy
	lda zp_USER_CARD + 1
	jsr graphics::printing::print_hex
	lda zp_USER_CARD
	jsr graphics::printing::print_hex
	
@print_nmi:
	jsr print_nmi_counter

@check_for_read:
	lda zp_LAST_NMI_COUNTER
	cmp zp_NMI_COUNTER
	blt @read
	bra @loop_end

@read:
@read_card1_byte:
	inc zp_LAST_NMI_COUNTER
	lda zp_CARD1_BYTE
	bne @card1_output
	bra @read_card2_byte
@card1_output:
	lda zp_SCREEN_X
	ldy zp_SCREEN_Y
	jsr graphics::drawing::goto_xy
	lda zp_CARD1_BYTE
	jsr graphics::printing::print_hex
	jsr inc_screen_position
	jsr inc_screen_position
@read_card2_byte:
	lda zp_CARD2_BYTE
	bne @card2_output
	bra @loop_end
@card2_output:
	lda zp_SCREEN_X
	ldy zp_SCREEN_Y
	jsr graphics::drawing::goto_xy
	lda zp_CARD2_BYTE
	jsr graphics::printing::print_hex
	jsr inc_screen_position
	jsr inc_screen_position

@loop_end:
	jmp loop

; Exit the program
exit:
  ; Restore ROM bank 7
  ;rombank #$04
  ;clc
  ;jmp ENTER_BASIC
  ldx #$42  ; System Management Controller
  ldy #$02  ; magic location for system reset
  lda #$00  ; magic value for system poweroff
  jmp i2c_write_byte ; power off the system

.proc init
	; Enable Divisor Latch
	lda #%10000000
	ldy #LINE_CONTROL_OFFSET
	sta (zp_CURRENT_CARD),y

	lda #<MIDI_BAUD_RATE_DIVISOR
	ldy #DIVISOR_LATCH_LOW
	sta (zp_CURRENT_CARD),y

	lda #>MIDI_BAUD_RATE_DIVISOR
	ldy #DIVISOR_LATCH_HI
	sta (zp_CURRENT_CARD),y

	; Disable Divisor Latch & Set word length
	lda #LCR_SETUP
	ldy #LINE_CONTROL_OFFSET
	sta (zp_CURRENT_CARD),y

	; Setup FIFO
	lda #FIFO_CLEAR
	ldy #FIFO_CONTROL_OFFSET
	sta (zp_CURRENT_CARD),y

	lda #FIFO_SETUP
	ldy #FIFO_CONTROL_OFFSET
	sta (zp_CURRENT_CARD),y
	
	; Setup Modem (Disable)
	lda #MODEM_SETUP
	ldy #MODEM_CONTROL_OFFSET
	sta (zp_CURRENT_CARD),y
	
	; Setup Interrupts (Disable)
	lda #INTR_SETUP
	ldy #INTERRUPT_ENABLE_OFFSET
	sta (zp_CURRENT_CARD),y

	rts
.endproc

.proc enable_nmi
	sei
	lda #<clock_isr_nmi
	sta $0318
	lda #>clock_isr_nmi
	sta $0319
	cli
	rts
.endproc

.proc clock_isr_nmi
	phy
	pha

	lda zp_NMI_COUNTER
	sta zp_LAST_NMI_COUNTER
	inc zp_NMI_COUNTER

  ldy #RX_BUFFER_OFFSET
  lda (zp_MIDI_IO_BASE1),y
	sta zp_CARD1_BYTE

  ldy #LINE_STATUS_OFFSET
  lda (zp_MIDI_IO_BASE1),y
	sta zp_TMP1

  ldy #RX_BUFFER_OFFSET
  lda (zp_MIDI_IO_BASE2),y
	sta zp_CARD2_BYTE

  ldy #LINE_STATUS_OFFSET
  lda (zp_MIDI_IO_BASE2),y
	sta zp_TMP3

@cleanup:
	pla
	ply

	; KERNAL's NMI set this (__nmi)
	pla
	sta ROM_BANK
	pla

  rti
.endproc

.proc print_interrupt_enable
	ldy #INTERRUPT_ENABLE_OFFSET
	lda (zp_CURRENT_CARD),y
	jsr graphics::printing::print_binary
	rts
.endproc

.proc print_interrupt_ident
	ldy #INTERRUPT_IDENT_OFFSET
	lda (zp_CURRENT_CARD),y
	jsr graphics::printing::print_binary
	rts
.endproc

.proc print_fifo_control
	ldy #FIFO_CONTROL_OFFSET
	lda (zp_CURRENT_CARD),y
	jsr graphics::printing::print_binary
	rts
.endproc

.proc print_line_control
	ldy #LINE_CONTROL_OFFSET
	lda (zp_CURRENT_CARD),y
	jsr graphics::printing::print_binary
	rts
.endproc

.proc print_modem_control
	ldy #MODEM_CONTROL_OFFSET
	lda (zp_CURRENT_CARD),y
	jsr graphics::printing::print_binary
	rts
.endproc

.proc print_line_status
	ldy #LINE_STATUS_OFFSET
	lda (zp_CURRENT_CARD),y
	jsr graphics::printing::print_binary
	rts
.endproc

.proc print_modem_status
	ldy #MODEM_STATUS_OFFSET
	lda (zp_CURRENT_CARD),y
	jsr graphics::printing::print_binary
	rts
.endproc

.proc print_nmi_counter
	lda #$0E
	ldy #$0E
	jsr graphics::drawing::goto_xy
	lda zp_NMI_COUNTER
	jsr graphics::printing::print_hex
	rts
.endproc

.proc set_card_one
	lda zp_MIDI_IO_BASE1
	sta zp_USER_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_USER_CARD + 1
	jmp print_stuff
.endproc

.proc set_card_two
	lda zp_MIDI_IO_BASE2
	sta zp_USER_CARD
	lda zp_MIDI_IO_BASE2 + 1
	sta zp_USER_CARD + 1
	jmp print_stuff
.endproc

.proc inc_screen_position
	lda zp_SCREEN_X
	cmp #$50
	beq @wrap
	inc zp_SCREEN_X
	rts
@wrap:
	stz zp_SCREEN_X
	inc zp_SCREEN_Y
	rts
.endproc

text_strings: 
	.byte SCREENCODE_XY,$00,$00
	.byte "midi tester magic",SCREENCODE_RETURN
	.byte SCREENCODE_XY,$01,$01,SCREENCODE_RETURN
	.byte "                  card 1   | card 2",SCREENCODE_RETURN
	.byte "address:          $XXXX    | $XXXX",SCREENCODE_RETURN
	.byte "interrupt enable: xxxxxxxx | xxxxxxxx",SCREENCODE_RETURN
	.byte "interrupt ident:  xxxxxxxx | xxxxxxxx",SCREENCODE_RETURN
	.byte "fifo register:    xxxxxxxx | xxxxxxxx",SCREENCODE_RETURN
	.byte "line control:     xxxxxxxx | xxxxxxxx",SCREENCODE_RETURN
	.byte "modem control:    xxxxxxxx | xxxxxxxx",SCREENCODE_RETURN
	.byte "line status:      xxxxxxxx | xxxxxxxx",SCREENCODE_RETURN
	.byte "modem status:     xxxxxxxx | xxxxxxxx",SCREENCODE_RETURN
	.byte SCREENCODE_RETURN,SCREENCODE_RETURN
	.byte "selected card: ",SCREENCODE_RETURN
	.byte "nmi counter: ",SCREENCODE_RETURN
	.byte SCREENCODE_RETURN,SCREENCODE_RETURN
	.byte "key commands:",SCREENCODE_RETURN
	.byte "q: quit (reset system)",SCREENCODE_RETURN
	.byte "1: select card 1",SCREENCODE_RETURN
	.byte "2: select card 2",SCREENCODE_RETURN
	.byte "a: print letter a",SCREENCODE_RETURN
	.byte "i: set read buffer and line interrupts",SCREENCODE_RETURN
	.byte "m: enable hardware interrupts",SCREENCODE_RETURN
	.byte "r: read byte buffer",SCREENCODE_RETURN
	.byte "t: reinit cards (both cards, disables interrupts, flushes read buffer and fifo)",SCREENCODE_RETURN
	.byte "n: load nmi routine (both cards)",SCREENCODE_RETURN
	.byte "typical test sequence would be i,n,m",SCREENCODE_RETURN
	.byte SCREENCODE_XY,$00,$1E
	.byte "output:",SCREENCODE_RETURN
	.byte 0
