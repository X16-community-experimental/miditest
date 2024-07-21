; Print an 8-bit value stored in the accumulator to the screen in binary

; Uses:
; zp_TEXT_COLOR (print_alpha_char)

.proc print_binary
  ldx #$00
@loop:
  asl
  pha
  bcs @set
@unset:
  lda #SCREENCODE_0
  jsr print_alpha_char
  jmp @loop_end
@set:
  lda #SCREENCODE_1
  jsr print_alpha_char
@loop_end:
  pla
  inx
  cpx #$08
  bne @loop
  rts
.endproc
