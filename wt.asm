; This echos out anything on the MIDI IN over to the Wavetable

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
	IO6_WT_ADDRESS=$9FC8
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
MIDI_PROG_CHANGE_CHANNEL_0 = $C0
MIDI_RESET = $FF


; Drawing Constants
ADDRESS_Y = $04
PATCH_NAME_LENGTH = $20
CURRENT_PATCH_X = $10
CURRENT_PATCH_Y = $06


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

	; Lowercase Charset
  ;lda #$05
  ;jsr screen_set_charset

; Setup I/O Addresses
	lda #<IO6_ADDRESS
	sta zp_MIDI_IO_BASE1
	lda #>IO6_ADDRESS
	sta zp_MIDI_IO_BASE1 + 1

	lda #<IO6_WT_ADDRESS
	sta zp_WAVETABLE_IO_BASE
	lda #>IO6_WT_ADDRESS
	sta zp_WAVETABLE_IO_BASE + 1

	; Init Wavetable MIDI
	lda zp_WAVETABLE_IO_BASE
	sta zp_CURRENT_CARD
	sta zp_USER_CARD
	lda zp_WAVETABLE_IO_BASE + 1
	sta zp_CURRENT_CARD + 1
	sta zp_USER_CARD + 1
	jsr init

	; Init External MIDI (and leave zp_CURRENT_CARD as ext)
	lda zp_MIDI_IO_BASE1
	sta zp_CURRENT_CARD
	sta zp_USER_CARD
	lda zp_MIDI_IO_BASE1 + 1
	sta zp_CURRENT_CARD + 1
	sta zp_USER_CARD + 1
	jsr init

; Print stuff
; Print UI and addresses
	print_string_macro text_strings
	lda #LEFT_COL 
	ldy #ADDRESS_Y
	jsr graphics::drawing::goto_xy
	lda zp_MIDI_IO_BASE1 + 1
	jsr graphics::printing::print_hex
	lda zp_MIDI_IO_BASE1
	jsr graphics::printing::print_hex

	lda #RIGHT_COL 
	ldy #ADDRESS_Y
	jsr graphics::drawing::goto_xy
	lda zp_WAVETABLE_IO_BASE + 1
	jsr graphics::printing::print_hex
	lda zp_WAVETABLE_IO_BASE
	jsr graphics::printing::print_hex

	stz zp_MIDI_IN_BYTE
	; $00 would be grand piano
	stz zp_CURRENT_PATCH
	jsr change_patch

;; Infinite Read Loop
loop:
  jsr GETIN  ;keyboard
	bne @check_keys
	jmp @check_midi

@check_keys:
@quit:
	cmp #KEY_Q
	bne @reset_wavetable
	jmp exit
@reset_wavetable:
	cmp #KEY_R
	bne @dec_patch
	jsr reset_wavetable
	bra @check_midi
@dec_patch:
	cmp #KEY_MINUS
	bne @inc_patch
	dec zp_CURRENT_PATCH
	jsr change_patch
	bra @check_midi
@inc_patch:
	cmp #KEY_EQUALS		; Basically plus but no shift
	bne @check_midi
	inc zp_CURRENT_PATCH
	jsr change_patch
	bra @check_midi
@check_midi:
	jsr read_midi_in
	lda zp_MIDI_IN_BYTE
	bne @forward_to_wavetable
	bra @loop_end
@forward_to_wavetable:
	jsr forward_to_wavetable
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

.proc read_midi_in
	ldy #RX_BUFFER_OFFSET
  lda (zp_CURRENT_CARD),y
	bne @byte
	rts
@byte:
	sta zp_MIDI_IN_BYTE
	rts
.endproc

.proc forward_to_wavetable
	lda zp_MIDI_IN_BYTE
	ldy #TX_HOLDING_OFFSET
  sta (zp_WAVETABLE_IO_BASE),y
	stz zp_MIDI_IN_BYTE
	rts
.endproc

.proc change_patch
	ldy #TX_HOLDING_OFFSET
	lda #MIDI_PROG_CHANGE_CHANNEL_0
  sta (zp_WAVETABLE_IO_BASE),y

	lda zp_CURRENT_PATCH
	cmp #$80
	bge @too_high
	bra @store
@too_high:
	stz zp_CURRENT_PATCH

@store:
	sta (zp_WAVETABLE_IO_BASE),y

@update_patch_name:
	lda #CURRENT_PATCH_X
	ldy #CURRENT_PATCH_Y
	jsr graphics::drawing::goto_xy

; Get the base address and then calculate the offset
; into the list by multiplying the string length
; by the patch number
	lda #<general_midi_names
	sta zp_ARG0
	lda #>general_midi_names
	sta zp_ARG0 + 1

	lda #PATCH_NAME_LENGTH
	sta zp_ARG1
	stz zp_ARG1 + 1

	lda zp_CURRENT_PATCH
	jsr graphics::printing::print_hex

	lda #SCREENCODE_COLON
	jsr graphics::printing::print_alpha_char

; Find address of patch in list
	ldx zp_CURRENT_PATCH
	beq @zero	; Zero case
@address_loop:
	add16to8 zp_ARG0, zp_ARG1, zp_ARG0
	dex
	bne @address_loop

; Print it!
@zero:
	ldy #$00
@print_patch_name:
	lda (zp_ARG0),y
	jsr graphics::printing::print_alpha_char
	iny
	cpy #PATCH_NAME_LENGTH 
	bne @print_patch_name


	rts
.endproc

.proc reset_wavetable
	ldy #TX_HOLDING_OFFSET
	lda #MIDI_RESET
  sta (zp_WAVETABLE_IO_BASE),y
	stz zp_CURRENT_PATCH
	jsr change_patch
	rts
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
	.byte "midi keyboard to wavetable",SCREENCODE_RETURN
	.byte "by tim soderstrom (the dreamtracker guy)",SCREENCODE_RETURN
	.byte SCREENCODE_RETURN
	.byte "                  ext midi | wavetable",SCREENCODE_RETURN
	.byte "address:          $XXXX    | $XXXX",SCREENCODE_RETURN
	.byte SCREENCODE_RETURN
	.byte "current patch:",SCREENCODE_RETURN
	.byte SCREENCODE_RETURN
	.byte "key commands:",SCREENCODE_RETURN
	.byte "r: reset wavetable (panic)",SCREENCODE_RETURN
	.byte "q: quit (reset system)",SCREENCODE_RETURN
	.byte 0


general_midi_names:
.byte "acoustic grand                  " 
.byte "bright acoustic                 " 
.byte "electric grand                  " 
.byte "honky-tonk                      " 
.byte "electric piano 1                " 
.byte "electric piano 2                " 
.byte "harpsichord                     " 
.byte "clav                            " 
.byte "celesta                         " 
.byte "glockenspiel                    " 
.byte "music box                       " 
.byte "vibraphone                      " 
.byte "marimba                         " 
.byte "xylophone                       " 
.byte "tubular bells                   " 
.byte "dulcimer                        " 
.byte "drawbar organ                   " 
.byte "percussive organ                " 
.byte "rock organ                      " 
.byte "church organ                    " 
.byte "reed organ                      " 
.byte "accoridan                       " 
.byte "harmonica                       " 
.byte "tango accordian                 " 
.byte "acoustic guitar(nylon)          " 
.byte "acoustic guitar(steel)          " 
.byte "electric guitar(jazz)           " 
.byte "electric guitar(clean)          " 
.byte "electric guitar(muted)          " 
.byte "overdriven guitar               " 
.byte "distortion guitar               " 
.byte "guitar harmonics                " 
.byte "acoustic bass                   " 
.byte "electric bass(finger)           " 
.byte "electric bass(pick)             " 
.byte "fretless bass                   " 
.byte "slap bass 1                     " 
.byte "slap bass 2                     " 
.byte "synth bass 1                    " 
.byte "synth bass 2                    " 
.byte "violin                          " 
.byte "viola                           " 
.byte "cello                           " 
.byte "contrabass                      " 
.byte "tremolo strings                 " 
.byte "pizzicato strings               " 
.byte "orchestral strings              " 
.byte "timpani                         " 
.byte "string ensemble 1               " 
.byte "string ensemble 2               " 
.byte "synthstrings 1                  " 
.byte "synthstrings 2                  " 
.byte "choir aahs                      " 
.byte "voice oohs                      " 
.byte "synth voice                     " 
.byte "orchestra hit                   " 
.byte "trumpet                         " 
.byte "trombone                        " 
.byte "tuba                            " 
.byte "muted trumpet                   " 
.byte "french horn                     " 
.byte "brass section                   " 
.byte "synthbrass 1                    " 
.byte "synthbrass 2                    " 
.byte "soprano sax                     " 
.byte "alto sax                        " 
.byte "tenor sax                       " 
.byte "baritone sax                    " 
.byte "oboe                            " 
.byte "english horn                    " 
.byte "bassoon                         " 
.byte "clarinet                        " 
.byte "piccolo                         " 
.byte "flute                           " 
.byte "recorder                        " 
.byte "pan flute                       " 
.byte "blown bottle                    " 
.byte "shakuhachi                      " 
.byte "whistle                         " 
.byte "ocarina                         " 
.byte "lead 1 (square)                 " 
.byte "lead 2 (sawtooth)               " 
.byte "lead 3 (calliope)               " 
.byte "lead 4 (chiff)                  " 
.byte "lead 5 (charang)                " 
.byte "lead 6 (voice)                  " 
.byte "lead 7 (fifths)                 " 
.byte "lead 8 (bass+lead)              " 
.byte "pad 1 (new age)                 " 
.byte "pad 2 (warm)                    " 
.byte "pad 3 (polysynth)               " 
.byte "pad 4 (choir)                   " 
.byte "pad 5 (bowed)                   " 
.byte "pad 6 (metallic)                " 
.byte "pad 7 (halo)                    " 
.byte "pad 8 (sweep)                   " 
.byte "fx 1 (rain)                     " 
.byte "fx 2 (soundtrack)               " 
.byte "fx 3 (crystal)                  " 
.byte "fx 4 (atmosphere)               " 
.byte "fx 5 (brightness)               " 
.byte "fx 6 (goblins)                  " 
.byte "fx 7 (echoes)                   " 
.byte "fx 8 (sci-fi)                   " 
.byte "sitar                           " 
.byte "banjo                           " 
.byte "shamisen                        " 
.byte "koto                            " 
.byte "kalimba                         " 
.byte "bagpipe                         " 
.byte "fiddle                          " 
.byte "shanai                          " 
.byte "tinkle bell                     " 
.byte "agogo                           " 
.byte "steel drums                     " 
.byte "woodblock                       " 
.byte "taiko drum                      " 
.byte "melodic tom                     " 
.byte "synth drum                      " 
.byte "reverse cymbal                  " 
.byte "guitar fret noise               " 
.byte "breath noise                    " 
.byte "seashore                        " 
.byte "bird tweet                      " 
.byte "telephone ring                  " 
.byte "helicopter                      " 
.byte "applause                        " 
.byte "gunshot                         " 
