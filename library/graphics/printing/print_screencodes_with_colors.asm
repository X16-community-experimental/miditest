; This is silly inefficient since we can do an if-check on print string 
; to decide whether or not to subtract char values to convert from petscii but 
; eh.
.proc print_screencodes_with_colors
  STRING_POINTER = zp_ARG0
  COLOR_POINTER = zp_ARG1
  X_POS = zp_ARG2
  Y_POS = zp_ARG3
@print:
  ; Set stride to 1
  vera_stride #%00010000
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  lda Y_POS        ; y coord
  sta VERA_addr_med
  ldy #0
@loop:
  lda (STRING_POINTER),y
  ; If we see a null terminated string, end
  bne @continue
  rts
@continue:
  cmp #SCREENCODE_RETURN  ; Our special screencode value to indicate return
  beq @return
  bra @nosub
@return:
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  ldx Y_POS        ; y coord
  inx
  stx VERA_addr_med
  stx Y_POS
  bra @loop_end
@nosub:
  sta VERA_data0
  lda (COLOR_POINTER),y
  sta VERA_data0
@loop_end:
  iny
  bne @loop
@end:
  rts
.endproc
