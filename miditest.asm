.include "memory/zeropage.asm"
.include "library/preamble.asm"
.include "library/x16.inc"
.include "library/keyboard.inc"
.include "library/screencodes.inc"
.include "library/variables.inc"
.include "library/macros.inc"
.include "library/graphics/main.asm"


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

; 
; Bit 7 = Divisor Latch
; Bit 6 = Break Control
; Bit 5 = Sticky Parity
; Bit 4 = Parity Select
; Bit 3 = Parity Enable (0 for none)
; Bit 2 = Stop Bits (0 to 1 stop bit)
; Bits 0 & 1 = Word Length (both to 1 = 8-bits)
; No Partity, 1 Stop, 8-Bits
LCR_SETUP  = %00000011

;INTR_SETUP = %00000111
INTR_SETUP = %00000000

FIFO_SETUP = %00000111

; MIDI SPEED 31.25k
; Crystal: 18.432 MHz
; In Hz: 18432000
; MIDI Baud: 31.25k
; Divisor = Hz / (MIDI Baud * 16)
; 37, or $25
MIDI_BAUD_DIV_LO = $25
MIDI_BAUD_DIV_HI = $00

cursor_old_color: .byte $00
cursor_x: .byte $00
cursor_y: .byte $00
cursor_layer: .byte $00

palette:
.byte $02,$00     ; super dark blue
.byte $FF,$0F     ; white
.byte $00,$0F     ; red
.byte $4F,$04     ; cyan
.byte $04,$0A     ; purple
.byte $40,$00     ; dark green
.byte $0F,$00     ; blue
.byte $F0,$0F     ; yellow
.byte $50,$0F     ; orange
.byte $30,$0A     ; brown
.byte $55,$0F     ; light red
.byte $22,$02     ; dark grey
.byte $44,$04     ; grey
.byte $F5,$05     ; light green
.byte $9F,$0A     ; light blue
.byte $88,$08     ; light gray

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

  ; Disable Divisor Latch & Set word length
  lda #LCR_SETUP
  sta LINE_CONTROL

  stz MODEM_CONTROL

  lda #INTR_SETUP
  sta INTERRUPT_ENABLE


  lda #$BF
  sta zp_TEXT_COLOR

  lda #$05
  ldy #$05
  jsr graphics::drawing::goto_xy

  ; Setup display
  print_string_macro scratch_string, #$09, #$00, #$05, #$BF
  print_string_macro modem_string, #$07, #$00, #$06, #$BF
  print_string_macro interruptr_string, #$06, #$00, #$07, #$BF
  print_string_macro line_string, #$16, #$00, #$08, #$BF
  print_string_macro received_string, #$0A, #$00, #$09, #$BF


  ; Scratch (test)
  lda #$55
  sta SCRATCH

  lda #$18
  ldy #$05
  jsr graphics::drawing::goto_xy
  lda SCRATCH
  jsr graphics::drawing::print_hex

@read:
  ; Just testing some shit out
  ; (MIDI clock out)
  ;lda #$F8
  ;sta TX_HOLDING
  ; (This worked)

  lda #$18
  ldy #$06
  jsr graphics::drawing::goto_xy
  lda MODEM_STATUS
  jsr graphics::drawing::print_hex

  lda #$18
  ldy #$07
  jsr graphics::drawing::goto_xy
  lda INTERRUPT_IDENT
  jsr graphics::drawing::print_hex


  lda #$18
  ldy #$08
  jsr graphics::drawing::goto_xy
  lda LINE_STATUS
  jsr graphics::drawing::print_hex

  lda #$18
  ldy #$09
  jsr graphics::drawing::goto_xy
  lda RX_BUFFER
  jsr graphics::drawing::print_hex
  jmp @read
@end:
  rts

scratch_string: .byte "scratch: "
modem_string: .byte "modem: "
interruptr_string: .byte "intr: "
line_string: .byte "line status register: "
received_string: .byte "received: "

