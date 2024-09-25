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
;MIDI_IO_BASE=$9F60
; "Standard" MIDI I/O (IO6/Low)
;MIDI_IO_BASE=$9FC0
; Testing 9-pin
MIDI_IO_BASE=$9F68
;MIDI_IO_BASE=$9FE0
;MIDI_IO_BASE=$9F80
;MIDI_IO_BASE=$9FE0

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
INTR_SETUP = %00000001

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

; ####
; Newest MIDI specific card seems to be a divisor of $1F
; ####
MIDI_BAUD_RATE_DIVISOR = $001F

;MIDI_BAUD_RATE_DIVISOR = $0025
; 9600 (for testing 9-pin RS232)
;MIDI_BAUD_RATE_DIVISOR = $0079
; 2400
; MIDI_BAUD_RATE_DIVISOR = $01E0
; 9600 (for testing 9-pin RS232)
;MIDI_BAUD_RATE_DIVISOR = $0079
; For Kevin's new MIDI card, which uses a different crystal
;MIDI_BAUD_RATE_DIVISOR = $0020

;; Scratch Register Test Value
SCRATCH_TEST_VALUE = $2F

; MIDI Values

MIDI_CLOCK = $F8


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

	;; Setup Toggles
	; We start with the opposite value of what we want
	; and call the toggle function so we get the results
	; printed to screen
	lda #$01
	sta zp_MIDI_OUT_TOGGLE
	sta zp_INTTERUPTS_TOGGLE
	stz zp_FIFO_TOGGLE
	stz zp_READ_LOOP_TOGGLE
	jsr toggle_midi_out
	jsr toggle_interrupts
	jsr toggle_fifo
	jsr toggle_read_loop
	jsr toggle_echoback

	;; UART Setup
	; Set Default Baud
	lda #<MIDI_BAUD_RATE_DIVISOR
	sta zp_BAUD_RATE
	lda #>MIDI_BAUD_RATE_DIVISOR
	sta zp_BAUD_RATE + 1
	jsr set_baud_rate

	; Load scratch value
	lda #SCRATCH_TEST_VALUE
	sta zp_SCRATCH_VALUE
	sta SCRATCH
	lda #FIFO_SETUP
	sta FIFO_CONTROL
	sta zp_FIFO_SHADOW
	stz MODEM_CONTROL
	lda #INTR_SETUP
	sta INTERRUPT_ENABLE

	lda #$01
	sta zp_TEXT_COLOR

	lda #$10
	ldy #$11
	jsr graphics::drawing::goto_xy


;; Infinite Read Loop
@loop:
  jsr GETIN  ;keyboard
  bne @midi_spam_toggle
	jmp @continue
  sta zp_KEY_PRESSED
@midi_spam_toggle:
	cmp #KEY_O
	bne @interrupts_toggle
	jsr toggle_midi_out
	jmp @continue

@interrupts_toggle:
	cmp #KEY_I
	bne @read_loop_toggle
	jsr toggle_interrupts
	jmp @continue
@read_loop_toggle:
	cmp #KEY_R
	bne @echoback_toggle
	jsr toggle_read_loop
	jmp @continue
@echoback_toggle:
	cmp #KEY_B
	bne @fifo_toggle
	jsr toggle_echoback
	jmp @continue
@fifo_toggle:
	cmp #KEY_F
	bne @fifo_buffer_1
	jsr toggle_fifo
	jmp @continue
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
	bne @decrease_base_div
	jmp @fifo_buffer_jump_e
@decrease_base_div:
	cmp #KEY_BRACKET_LEFT
	bne @increase_base_div
	jmp @decrease_base_div_jump
@increase_base_div:
	cmp #KEY_BRACKET_RIGHT
	bne @quit
	jmp @increase_base_div_jump
@quit:
	cmp #KEY_Q
	bne @continue
	jmp exit

; Jumps due to 6502 page size
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
	jmp @continue
@decrease_base_div_jump:
	jsr decrease_base_divisor
	jmp @continue
@increase_base_div_jump:
	jsr increase_base_divisor

@continue:
@spam_midi:
	lda zp_MIDI_OUT_TOGGLE
	beq @echoback
	;; Spam the MIDI Clock, $F8, to MIDI Out
	lda #$F8
	sta TX_HOLDING
@echoback:
	lda zp_ECHOBACK_TOGGLE
	beq @read_buffer
	jsr echoback
@read_buffer:
	lda zp_READ_LOOP_TOGGLE
	beq @update_screen
	lda RX_BUFFER
	sta zp_RX_BUFFER_SHADOW
@update_screen:
	jsr update_screen_values
@loop_end:
	jmp @loop

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
	;lda #%00000111
	; Just enable read interrupt
	lda #INTR_SETUP
	sta INTERRUPT_ENABLE
	print_null_terminated_string_macro on_string, #$46, #$05, #$01
	rts
.endproc 

.proc toggle_read_loop
	lda zp_READ_LOOP_TOGGLE
	beq @turn_on
@turn_off:
	stz zp_READ_LOOP_TOGGLE
	print_null_terminated_string_macro off_string, #$46, #$06, #$01
	rts
@turn_on:
	inc zp_READ_LOOP_TOGGLE
	print_null_terminated_string_macro on_string, #$46, #$06, #$01
	rts
.endproc 

.proc toggle_echoback
	lda zp_ECHOBACK_TOGGLE
	beq @turn_on
@turn_off:
	stz zp_ECHOBACK_TOGGLE
	print_null_terminated_string_macro off_string, #$46, #$07, #$01
	rts
@turn_on:
	inc zp_ECHOBACK_TOGGLE
	print_null_terminated_string_macro on_string, #$46, #$07, #$01
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
	print_null_terminated_string_macro off_string, #$46, #$08, #$01
	rts
@turn_on:
	inc zp_FIFO_TOGGLE
	lda zp_FIFO_SHADOW
	ora #%00000001
	sta zp_FIFO_SHADOW
	sta FIFO_CONTROL
	print_null_terminated_string_macro on_string, #$46, #$08, #$01
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

.proc increase_base_divisor
	add16to8 zp_BAUD_RATE, #$1, zp_BAUD_RATE
	jsr set_baud_rate
	rts
.endproc

.proc decrease_base_divisor
	sub16from8 zp_BAUD_RATE, #$1, zp_BAUD_RATE
	jsr set_baud_rate
	rts
.endproc

.proc set_baud_rate
	; Enable Divisor Latch
	lda #%10000000
	sta LINE_CONTROL
	lda zp_BAUD_RATE
	sta DIVISOR_LATCH_LOW
	lda zp_BAUD_RATE + 1
	sta DIVISOR_LATCH_HI

	; Disable Divisor Latch & Set word length
	lda #LCR_SETUP
	sta LINE_CONTROL
	rts
.endproc

.proc echoback
	lda INTERRUPT_IDENT
	and #%00000001
	beq @read
	rts
@read:
	lda RX_BUFFER
	sta TX_HOLDING
	rts
.endproc


; Update the screen values
.proc update_screen_values

@print_midi_io_base:
	lda #$18
	ldy #$04
	jsr graphics::drawing::goto_xy
	lda #>MIDI_IO_BASE
	jsr graphics::printing::print_hex
	lda #<MIDI_IO_BASE
	jsr graphics::printing::print_hex

@print_baud_rate:
	lda #$18
	ldy #$05
	jsr graphics::drawing::goto_xy
	lda zp_BAUD_RATE + 1
	jsr graphics::printing::print_hex
	lda zp_BAUD_RATE 
	jsr graphics::printing::print_hex

@print_scratch_values:
	lda #$18
	ldy #$06
	jsr graphics::drawing::goto_xy
	lda zp_SCRATCH_VALUE
  pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$06
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

	lda #$18
	ldy #$07
	jsr graphics::drawing::goto_xy
	lda SCRATCH
  pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$07
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

@modem_status:
	lda #$18
	ldy #$08
	jsr graphics::drawing::goto_xy
	lda MODEM_STATUS
	pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$08
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

@fifo_shadow:
	lda #$18
	ldy #$09
	jsr graphics::drawing::goto_xy
	lda zp_FIFO_SHADOW
	pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$09
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

@interrupt_enable:
	lda #$18
	ldy #$0A
	jsr graphics::drawing::goto_xy
	lda INTERRUPT_ENABLE
	pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$0A
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

@interrupt_status:
	lda #$18
	ldy #$0B
	jsr graphics::drawing::goto_xy
	lda INTERRUPT_IDENT
	pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$0B
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

@line_status:
	lda #$18
	ldy #$0C
	jsr graphics::drawing::goto_xy
	lda LINE_STATUS
	pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$0C
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

@rx_buffer:
	lda #$18
	ldy #$0D
	jsr graphics::drawing::goto_xy
	lda zp_RX_BUFFER_SHADOW
	pha
	jsr graphics::printing::print_hex
	lda #$1B
	ldy #$0D
	jsr graphics::drawing::goto_xy
	pla
	jsr graphics::printing::print_binary

	rts
.endproc

on_string: .byte "on ",0
off_string: .byte "off",0

