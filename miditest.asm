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

; The MIDI IO base, which depends on the IO and Hi/Lo jumper settings on the card.
; Base address is silkscreen on the card.
MIDI_IO_BASE=$9F68
RX_BUFFER=MIDI_IO_BASE           ; Read Only
TX_HOLDING=MIDI_IO_BASE          ; Write Only
INTERRUPT_ENABLE=MIDI_IO_BASE + 1
INTERRUPT_IDENT=MIDI_IO_BASE + 2 ; Read only
FIFO_CONTROL=MIDI_IO_BASE + 2    ; Write only
LINE_CONTROL=MIDI_IO_BASE + 3
MODEM_CONTROL=MIDI_IO_BASE + 4
LINE_STATUS=MIDI_IO_BASE + 5
MODEM_STATUS=MIDI_IO_BASE + 6
SCRATCH=MIDI_IO_BASE + 7

; Used to set baud rate by setting Divsor Latch to 1 (LINE_STATUS)
DIVISOR_LATCH_LOW = MIDI_IO_BASE
DIVISOR_LATCH_HI = MIDI_IO_BASE + 1

;; Line Control Register Flags
; Bit 7				: Divisor Latch
; Bit 6				: Break Control
; Bit 5				: Sticky Parity
; Bit 4				: Parity Select
; Bit 3				: Parity Enable (0 for none)
; Bit 2				: Stop Bits (0 to 1 stop bit)
; Bits 0 & 1	: Word Length (both to 1 = 8-bits)
; No Partity, 1 Stop, 8-Bits
LCR_SETUP  = %00000011

;; Interrupt Enable Flags
; Bits 7-4		: Unused (always cleared)
; Bit 3				: 1 = Enable Modem Status Interrupt
; Bit 2				: 1 = Enable Receiver Line Status Intterupt
; Bit 1				: 1 = Enable THRE (Transmission Holding Register) Interrupt
; Bit 0				: 1 = Enable Received Data Available Interrupt
INTR_SETUP = %00000000

;; FIFO Control Register Flags
; Bits 7-6		: Buffer size (00 = $01, 01 = $04, 10 = $08, 11 = $0E)
; Bits 5-4		: Reserved
; Bit 3				: When FCR0 set, assert !RXRDY and !TXRDY pins
; Bit 2				: Clears TX FIFO and counter
; Bit 1				: Clears RX FIFO and counter
; Bit 0				: Enable FIFO Buffers
FIFO_SETUP = %00000111

;; MIDI Baud Rate
; MIDI Baud per specification	: 31.25k (31250)
; Crystal											: 18.432 MHz
; Crystal In Hz								: 18432000
; Divisor 										: Hz / (MIDI Baud * 16)														
; Result: 										: 37, or $25
MIDI_BAUD_DIV_LO = $25
MIDI_BAUD_DIV_HI = $00

;; Scratch Register Test Value
SCRATCH_TEST_VALUE = $23


;; Display code (lifted from Dreamtracker's setup)
FILE_FILENAME:
    .byte "scr/main.scr"
FILE_FILENAME_LENGTH = $0C

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

	lda #FILE_FILENAME_LENGTH
	ldx #<FILE_FILENAME
	ldy #>FILE_FILENAME
	jsr files::load_to_vram

	; Set Baud
	; Enable Divisor Latch
	lda #%10000000
	sta LINE_CONTROL
	lda #MIDI_BAUD_DIV_LO
	sta DIVISOR_LATCH_LOW
	lda #MIDI_BAUD_DIV_HI
	sta DIVISOR_LATCH_HI

	; Setup
	lda #FIFO_SETUP
	sta FIFO_CONTROL
	sta zp_FIFO_SHADOW

	; Disable Divisor Latch & Set word length
	lda #LCR_SETUP
	sta LINE_CONTROL

	stz MODEM_CONTROL

	lda #INTR_SETUP
	sta INTERRUPT_ENABLE

	lda #$01
	sta zp_TEXT_COLOR
	;; Scratch
	; This writes a value to the scratch registers and then reads it back
	; Helps to make sure the card is connected and seemingly working.
	lda #$18
	ldy #$04
	jsr graphics::drawing::goto_xy
	lda #SCRATCH_TEST_VALUE
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$04
	jsr graphics::drawing::goto_xy
	lda #SCRATCH_TEST_VALUE
	jsr graphics::drawing::print_binary

	lda #SCRATCH_TEST_VALUE
	sta SCRATCH
	lda #$18
	ldy #$05
	jsr graphics::drawing::goto_xy
	lda SCRATCH
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$05
	jsr graphics::drawing::goto_xy
	lda SCRATCH
	jsr graphics::drawing::print_binary

;; Infinite Read Loop
@read:
  jsr GETIN  ;keyboard
  beq @continue
  sta zp_KEY_PRESSED
@midi_spam_toggle:
	cmp #KEY_O
	bne @interrupts_toggle
	jsr toggle_midi_out
@interrupts_toggle:
	cmp #KEY_I
	bne @fifo_toggle
	jsr toggle_interrupts
@fifo_toggle:
	cmp #KEY_F
	bne @fifo_buffer_1
	jsr toggle_fifo
@fifo_buffer_1:
	cmp #KEY_1
	bne @fifo_buffer_4
	jmp @fifo_buffer_jump_1
@fifo_buffer_4:
	cmp #KEY_4
	bne @fifo_buffer_8
	jmp @fifo_buffer_jump_4
@fifo_buffer_8:
	cmp #KEY_8
	bne @fifo_buffer_e
	jmp @fifo_buffer_jump_8
@fifo_buffer_e:
	cmp #KEY_E
	bne @quit
	jmp @fifo_buffer_jump_e
@quit:
	cmp #KEY_Q
	bne @continue
	jmp exit

@fifo_buffer_jump_1:
	jsr set_fifo_buffer_to_1
	jmp @continue
@fifo_buffer_jump_4:
	jsr set_fifo_buffer_to_4
	jmp @continue
@fifo_buffer_jump_8:
	jsr set_fifo_buffer_to_8
	jmp @continue
@fifo_buffer_jump_e:
	jsr set_fifo_buffer_to_e

@continue:
@spam_midi:
	lda zp_MIDI_OUT_TOGGLE
	beq @modem_status
	;; Spam the MIDI Clock, $F8, to MIDI Out
	lda #$F8
	sta TX_HOLDING

@modem_status:
	lda #$18
	ldy #$06
	jsr graphics::drawing::goto_xy
	lda MODEM_STATUS
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
	lda zp_FIFO_SHADOW
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$07
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary


	lda #$18
	ldy #$08
	jsr graphics::drawing::goto_xy
	lda INTERRUPT_ENABLE
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$08
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

	lda #$18
	ldy #$09
	jsr graphics::drawing::goto_xy
	lda INTERRUPT_IDENT
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$09
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

	lda #$18
	ldy #$0A
	jsr graphics::drawing::goto_xy
	lda LINE_STATUS
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$0A
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary

	lda #$18
	ldy #$0B
	jsr graphics::drawing::goto_xy
	lda RX_BUFFER
	pha
	jsr graphics::drawing::print_hex
	lda #$1B
	ldy #$0B
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::drawing::print_binary
	jmp @read

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


.proc toggle_midi_out
	lda zp_MIDI_OUT_TOGGLE
	beq @turn_on
@turn_off:
	stz zp_MIDI_OUT_TOGGLE
	print_null_terminated_string_macro off_string, #$46, #$04, #$01
	rts
@turn_on:
	inc zp_MIDI_OUT_TOGGLE
	print_null_terminated_string_macro on_string, #$46, #$04, #$01
	rts
.endproc

.proc toggle_interrupts
	lda zp_INTTERUPTS_TOGGLE
	beq @turn_on
@turn_off:
	stz zp_INTTERUPTS_TOGGLE
	stz INTERRUPT_ENABLE
	print_null_terminated_string_macro off_string, #$46, #$05, #$01
	rts
@turn_on:
	inc zp_INTTERUPTS_TOGGLE
	lda #%00000111
	sta INTERRUPT_ENABLE
	print_null_terminated_string_macro on_string, #$46, #$05, #$01
	rts
.endproc 

.proc toggle_fifo
	lda zp_FIFO_TOGGLE
	beq @turn_on
@turn_off:
	stz zp_FIFO_TOGGLE
	lda zp_FIFO_SHADOW
	and #%11111110
	sta zp_FIFO_SHADOW
	sta FIFO_CONTROL
	print_null_terminated_string_macro off_string, #$46, #$06, #$01
	rts
@turn_on:
	inc zp_FIFO_TOGGLE
	lda zp_FIFO_SHADOW
	ora #%00000001
	sta zp_FIFO_SHADOW
	sta FIFO_CONTROL
	print_null_terminated_string_macro on_string, #$46, #$06, #$01
	rts
.endproc 


.proc set_fifo_buffer_to_1
	lda zp_FIFO_SHADOW
	and #%00111111
	sta zp_FIFO_SHADOW
	sta FIFO_CONTROL
	rts
.endproc 

.proc set_fifo_buffer_to_4
	lda zp_FIFO_SHADOW
	and #%01111111
	ora #%01000000
	sta zp_FIFO_SHADOW
	sta FIFO_CONTROL
	rts
.endproc 

.proc set_fifo_buffer_to_8
	lda zp_FIFO_SHADOW
	and #%10111111
	ora #%10000000
	sta zp_FIFO_SHADOW
	sta FIFO_CONTROL
	rts
.endproc 

.proc set_fifo_buffer_to_e
	lda zp_FIFO_SHADOW
	ora #%11000000
	sta zp_FIFO_SHADOW
	sta FIFO_CONTROL
	rts
.endproc 

on_string: .byte "on ",0
off_string: .byte "off",0
