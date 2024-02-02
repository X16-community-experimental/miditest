.proc print_string
  STRING_POINTER = zp_ARG0
  LENGTH = zp_ARG1
  X_POS = zp_ARG2
  Y_POS = zp_ARG3
print:
  lda #%00010000	; Inc set to 1, low ram
  sta VERA_addr_high ; Set primary address bank to 0, stride to 1
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  lda Y_POS        ; y coord
  sta VERA_addr_med
  ldy #0
@loop:
  lda (STRING_POINTER),y
  cmp #SCREENCODE_RETURN  ; Our special screencode value to indicate return
  beq @return
  cmp #$40               ; Only subtract if it's A-Z
  bmi @nosub
  sec                   ; Converting from PETSCII to Screen Codes
  sbc #$40
  jmp @nosub
@return:
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  ldx Y_POS        ; y coord
  inx
  stx VERA_addr_med
  stx Y_POS
  jmp @loop_end
@nosub:
  sta VERA_data0
  lda zp_TEXT_COLOR
  sta VERA_data0
@loop_end:
  iny
  cpy LENGTH
  bne @loop
@end:
  rts
.endproc


.proc print_null_terminated_string
  STRING_POINTER = zp_ARG0
  X_POS = zp_ARG2
  Y_POS = zp_ARG3
print:
  lda #%00010000	; Inc set to 1, low ram
  sta VERA_addr_high ; Set primary address bank to 0, stride to 1
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  lda Y_POS        ; y coord
  sta VERA_addr_med
  ldy #0
@loop:
  lda (STRING_POINTER),y
  cmp #SCREENCODE_RETURN  ; Our special screencode value to indicate return
  beq @return
  cmp #SCREENCODE_NULL
  beq @end
  cmp #$40               ; Only subtract if it's A-Z
  bmi @nosub
  sec                   ; Converting from PETSCII to Screen Codes
  sbc #$40
  jmp @nosub
@return:
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  ldx Y_POS        ; y coord
  inx
  stx VERA_addr_med
  stx Y_POS
  jmp @loop_end
@nosub:
  sta VERA_data0
  lda zp_TEXT_COLOR
  sta VERA_data0
@loop_end:
  iny
  bne @loop
@end:
  rts
.endproc
