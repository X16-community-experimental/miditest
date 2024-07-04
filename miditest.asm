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
MIDI_IO_BASE=$9F60
; Testing 9-pin
;MIDI_IO_BASE=$9F60
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
;INTR_SETUP = %00000101
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

	; Setup our MIDI clock interrupt
	jsr enable_clock_irq

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

	; Setup FIFO
	lda #FIFO_SETUP
	sta FIFO_CONTROL
	sta zp_FIFO_SHADOW
	stz MODEM_CONTROL

	; Setup Interrupts
	lda #INTR_SETUP
	sta INTERRUPT_ENABLE

	lda #$01
	sta zp_TEXT_COLOR

	lda #$10
	ldy #$10
	jsr graphics::drawing::goto_xy

	lda SCRATCH
	jsr graphics::drawing::print_hex
	
	cli

;; Infinite Read Loop
@loop:
  jsr GETIN  ;keyboard
	beq @continue
	sta zp_KEY_PRESSED
@check_keys:
@quit:
	cmp #KEY_Q
	bne @a
	jmp exit
@a:
	cmp #KEY_A
	bne @i
	jsr graphics::drawing::print_alpha_char
	;lda #$69
	;sta zp_TMP0
@i:
	cmp #KEY_I
	bne @continue
	lda INTERRUPT_IDENT
	jsr graphics::drawing::print_hex
	;lda #$69
	;sta zp_TMP0

@continue:
	lda zp_TMP0
	beq @loop_end
@read_value:
	jsr graphics::drawing::print_hex
	stz zp_TMP0
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

.proc enable_clock_irq
  @enable_irq:
    ; Setup irq handler
    ; We load the address of our interrupt handler into a special memory
    ;   location. Basically when an interrupt triggers, this is the
    ;   routine the CPU will execute.
    ldx #$00
    lda ISR_HANDLER,x
    sta PREVIOUS_ISR,x
    lda #<clock_isr
    sta ISR_HANDLER,x
    inx
    lda ISR_HANDLER,x
    sta PREVIOUS_ISR,x
    lda #>clock_isr
    sta ISR_HANDLER,x
  @end:
    rts
.endproc

.proc clock_isr
  push_state_disable_interrupts

	;lda #SCREENCODE_B
	;jsr graphics::drawing::print_alpha_char

	lda INTERRUPT_IDENT
	and #%00000001
	; If zero, interrupt is pending
	bne @end
@read_buffer:
	lda RX_BUFFER
	sta zp_TMP0
@end:
	;ply
	;plx
	;pla
	plp
	jmp (PREVIOUS_ISR)        ; Pass control to the previous handler

  rti
.endproc


on_string: .byte "on ",0
off_string: .byte "off",0

