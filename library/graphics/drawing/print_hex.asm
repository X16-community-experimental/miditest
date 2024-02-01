; Print an 8-bit hex value stored in the accumulator to the screen

; Uses:
; zp_TEXT_COLOR
; .include "x16.inc"

; First we shift 4-bits to the right. This is so we can print the first
;   character of the number.
.proc print_hex
print_hex:
  phx
  pha
  jsr @main
  pla
  plx
  rts

@main:
  tax
  lsr
  lsr
  lsr
  lsr
  jsr print_single_hex
  txa
  and #$0f
.endproc

; Print a single character. If it's 0-9, add the value of the '0' symbol
;   in PETSCII. Since numbers are sequential, it maps nicely.
;   Otherwise, map it to the 'A' key and do the same thing. Note the
;   subtraction looks a bit weird because of how carry works.
;   We also actually start at the '@' character but subtract down to a
;   minimum value of 1 to avoid having to treat 0 as a special case.
;   The next character in PETSCII after '@' is 'A'.
.proc print_single_hex
@printc:    cmp #$0A
            bge @printl
@printn:    adc #CHAR0
            jsr print_alpha_char
            rts

@printl:    clc
            sbc #$08
            clc
            adc #CHARAT
            jsr print_alpha_char
            rts

.endproc
