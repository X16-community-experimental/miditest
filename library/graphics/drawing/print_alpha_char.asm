; Print letter to VERA (which requires simple math)
; a = char value
.proc print_alpha_char
@print_alpha_char:
  pha
  vera_stride #$10
  cmp #$40               ; Only subtract if it's A-Z
  bmi @nosub
  sec                   ; Converting from PETSCII to Screen Codes
  sbc #$40
  clc
@nosub:
  sta VERA_data0
  lda zp_TEXT_COLOR
  sta VERA_data0
  pla
  rts
.endproc
