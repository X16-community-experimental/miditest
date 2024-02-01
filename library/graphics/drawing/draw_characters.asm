; Draw Characters from Byte Array
; The array format is:
; x,y,char,color
; with x=FF signaling the end of data
;
; Parameters:
; r0 = pointer to data
; r1 = offset to add for after reading 4 bytes
;       used by add16 routine
; r2 = 16-bit add result
.proc draw_characters
  DATA = r0
  OFFSET = r1
  RESULT = r2
draw_panel:
  lda #04
  sta OFFSET
@draw_char:
  ldy #0
  lda (DATA),y      ; x coord
  cmp #$FF          ; if the value is FF, we are at the end so we're done.
  beq @end
  asl               ; shift because X coords are by two
  sta VERA_addr_low
  iny
  lda (DATA),y      ; y coord
  sta VERA_addr_med
  iny
  lda (DATA),y      ; char
  sta VERA_data0
  iny
  lda (DATA),y      ; color
  sta VERA_data0

; Move the reference of the character array by 4 bytes
@skip_pointer:
  add16 DATA, OFFSET, RESULT
  lda RESULT
  sta DATA
  lda RESULT+1
  sta DATA+1
  jmp @draw_char
@end:
  rts
.endproc
