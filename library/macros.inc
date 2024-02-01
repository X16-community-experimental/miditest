.macro print_string_macro msg, length, xpos, ypos, color
  lda #<msg
  sta zp_ARG0
  lda #>msg
  sta zp_ARG0+1
  lda length
  sta zp_ARG1
  lda xpos
  sta zp_ARG2
  lda ypos
  sta zp_ARG3
  lda color
  sta zp_TEXT_COLOR
  jsr graphics::drawing::print_string
.endmacro

.macro vera_stride stride
  pha
  lda VERA_addr_high
  and #%00001111
  ora stride
  sta VERA_addr_high
  pla
.endmacro

.macro vera_layer0_256_color
  lda #RES128x64x256
  sta VERA_L0_config
.endmacro

.macro vera_layer0_16_color
  lda #RES128x64x16
  sta VERA_L0_config
.endmacro

.macro set_text_color color
  lda color
  sta zp_TEXT_COLOR
.endmacro

.macro print_char_with_color char, color
  lda char
  sta VERA_data0
  lda color
  sta VERA_data0
.endmacro

.macro set_background_foregound_text_color background_color, foreground_color
  pha
  lda foreground_color
  sta r0
  lda background_color
  sta r1
  jsr math::set_top_nibble
  lda r2
  sta zp_TEXT_COLOR
  pla
.endmacro

.macro clear_screen
    lda #WHITE_ON_BLACK
    sta zp_TMP1
    jsr	graphics::drawing::fill_screen
.endmacro

; Print a 16-bit hex number
; There's very likely a more efficent way to do this
.macro printhex16 x_pos, y_pos, text_color, address
    ; Start pattern address
    lda x_pos
    ldy y_pos
    jsr graphics::drawing::goto_xy
    ; Color
    set_text_color text_color
    ; Why +1 first? Little endian.
    lda address + 1
    jsr graphics::drawing::print_hex
    lda x_pos + 2
    ldy y_pos
    jsr graphics::drawing::goto_xy
    lda address
    jsr graphics::drawing::print_hex
.endmacro

.macro acc16b
  ; Reset m flag (set A/memory to 16-bit)
  rep #P_MEMORY
  .a16
.endmacro

.macro acc8b
  ; Set m flag (set A/memory to 8-bit)
  sep #P_MEMORY
  .a8
.endmacro

.IF X16_816_FULL
; Assumes native mode
.macro add16 num1, num2, result
  ; If we're in 8-bit mode, we need to go to 16-bit mode
  acc16b

  ; Do the actual addition
  clc
  lda num1
  adc num2
  sta result

  acc8b
.endmacro

.macro sub16 num1, num2, result
  ; If we're in 8-bit mode, we need to go to 16-bit mode
  acc16b

  ; Do the actual subtraction
  sec
  lda num1
  sbc num2
  sta result

  acc8b
.endmacro

; Copy a 16-bit value from one memory location to another
.macro cpm16 source, dest
  acc16b
  lda source
  sta dest
  acc8b
.endmacro

; Set native 816 mode
.macro native
  ; Set native mode
  clc
  xce
.endmacro

; Set emulated C02 mode
.macro emulated
  sec
  xce
  .a8
.endmacro

.ELSEIF X16_C02
; Add two 16-bit numbers
.macro add16 num1, num2, result
    clc
    lda num1
    adc num2
    sta result
    lda num1+1
    adc num2+1
    sta result+1
.endmacro

.macro sub16 num1, num2, result
    sec
    lda num1
    sbc num2
    sta result
    lda num1+1
    sbc num2+1
    sta result+1
.endmacro

; Copy a 16-bit value from one memory location to another
.macro cpm16 source, dest
    lda source
    sta dest
    lda source + 1
    sta dest + 1
.endmacro

; Set native 816 mode
.macro native
.endmacro

; Set emulated C02 mode
.macro emulated
.endmacro

.ELSE 
; do nothing (error)

.ENDIF

; Add an 8-bit number to a 16-bit one
; num1 = 16-bit
; num2 = 8-bit
.macro add16to8 num1, num2, result
    clc
    lda num1
    adc num2
    sta result
    lda num1+1
    adc #$00
    sta result+1
.endmacro

; Subtract an 8-bit number from a 16-bit one
; num1 = 16-bit
; num2 = 8-bit
.macro sub16from8 num1, num2, result
  sec
  lda num1
  sbc num2
  sta result
  lda num1+1
  sbc #$00
  sta result+1
.endmacro

; 16-bit ASL. 
.macro asl16 value
  ; Shift high byte first
  lda value + 1
  asl
  sta value + 1
  
  ; Shift low
  clc
  lda value
  asl
  sta value
  ; Add the carry
  lda value + 1
  adc #$00
  sta value + 1
.endmacro


; Copy 8-bit value from one memory location to another
.macro cpm source, dest
    lda source
    sta dest
.endmacro

; I got tired of having to remember the sec dance
.macro sub source, amount
    sec
    lda source
    sbc amount
.endmacro

.macro suba amount
    sec
    sbc amount
.endmacro


; I got tired of having to remember the clc dance
.macro add source, amount
  clc
  lda source
  adc amount
.endmacro

.macro adda amount
  clc
  adc amount
.endmacro


; Branch Less Than (BCC)
.macro blt destination
  bcc destination
.endmacro

; Branch Greater Than Or Equal (BCS)
.macro bge destination
  bcs destination
.endmacro

; Branch Greater Than
.macro bgt continue, branch
  beq continue
  bcs branch
.endmacro

; Multiply a by 4
.macro mul4
  asl
  asl
.endmacro

; Multipl a by 16
.macro mul16
  asl
  asl
  asl
  asl
.endmacro

.macro ror2
  ror
  ror
.endmacro

.macro ror3
  ror
  ror
  ror
.endmacro

.macro ror4
  ror
  ror
  ror
  ror
.endmacro

.macro ror5
  ror
  ror
  ror
  ror
  ror
.endmacro

.macro ror6
  ror
  ror
  ror
  ror
  ror
  ror
.endmacro

.macro ror7
  ror
  ror
  ror
  ror
  ror
  ror
  ror
.endmacro

.macro asl2
  asl
  asl
.endmacro

.macro asl3
  asl
  asl
  asl
.endmacro

.macro asl4
  asl
  asl
  asl
  asl
.endmacro

.macro asl5
  asl
  asl
  asl
  asl
  asl
.endmacro

.macro asl6
  asl
  asl
  asl
  asl
  asl
  asl
.endmacro

.macro lsr2
  lsr
  lsr
.endmacro

.macro lsr3
  lsr
  lsr
  lsr
.endmacro

.macro lsr4
  lsr
  lsr
  lsr
  lsr
.endmacro

.macro lsr5
  lsr
  lsr
  lsr
  lsr
  lsr
.endmacro

.macro lsr6
  lsr
  lsr
  lsr
  lsr
  lsr
  lsr
.endmacro

; Basically just php and sei
; This just helps me to remember what's going on
.macro push_state_disable_interrupts
  php
  sei
.endmacro

;; For 65816 support, simulates the bit# instructions of the 65C02S
;; Used with minor modifications from the x16-rom mac.inc
.macro bbs bit_position, data, destination
	.if (bit_position = 7)
		bit data
		bmi destination
	.else
		.if (bit_position = 6)
			bit data
			bvs destination
		.else
			lda data
			and #1 << bit_position
			bne destination
		.endif
  .endif
.endmacro

.macro bbr bit_position, data, destination
	.if (bit_position = 7)
		bit data
		bpl destination
	.else
		.if (bit_position = 6)
			bit data
			bvc destination
		.else
			lda data
			and #1 << bit_position
			beq destination
		.endif
	.endif
.endmacro

.macro rmb bit, destination
  lda #$1 << bit
  trb destination
.endmacro

.macro smb bit, destination
	lda #$1 << bit
  tsb destination
.endmacro
;; 


; Load HiMem Bank
.macro rambank bank
  lda bank
  sta RAM_BANK
.endmacro

; Load Kernal/Cart RAM/ROM Bank
.macro rombank bank
  lda bank
  sta ROM_BANK
.endmacro

; Turn on mouse pointer
.macro mouse_on
  lda #$01
  ldx #SCREEN_RESOLUTION_X
  ldy #SCREEN_RESOLUTION_Y
  jsr MOUSE_CONFIG
.endmacro

; Turn off mouse pointer
.macro mouse_off
  lda #$00
  jsr MOUSE_CONFIG
.endmacro

; FM (YM2151) related macros
; Self-modifying code disclaimer!
; https://bumbershootsoft.wordpress.com/2021/01/02/a-grimoire-of-8-bit-implementation-patterns/
; https://discord.com/channels/547559626024157184/629863245934755860/1152760668886417409
.macro fm_get_op operator_offset, address_lo, address_hi
.local OPERATOR_ADDRESS
  phx
  ldx operator_offset
  lda address_lo,x
  sta OPERATOR_ADDRESS
  lda address_hi,x
  sta OPERATOR_ADDRESS+1
  plx
  ; $FFFF is a placeholder. OPERATOR_ADDRESS' value will get put there
  lda $FFFF,x
OPERATOR_ADDRESS = * - 2
.endmacro

.macro fm_store_op operator_offset, address_lo, address_hi
.local OPERATOR_ADDRESS
  pha
  phx
  ldx operator_offset
  lda address_lo,x
  sta OPERATOR_ADDRESS
  lda address_hi,x
  sta OPERATOR_ADDRESS+1
  plx
  pla
  ; $FFFF is a placeholder. OPERATOR_ADDRESS' value will get put there
  sta $FFFF,x
OPERATOR_ADDRESS = * - 2
.endmacro

.macro turn_off_layer_0
  lda VERA_dc_video
  and #%11101111
  sta VERA_dc_video
.endmacro

.macro turn_on_layer_0
  lda VERA_dc_video
  ora #%00010000
  sta VERA_dc_video
.endmacro