; Print character at screen position

; a = chracter
; x = x-position
; y = y-position

.proc print_character
  pha
  txa
  jsr graphics::drawing::goto_xy
  pla
  sta VERA_data0
  rts
.endproc