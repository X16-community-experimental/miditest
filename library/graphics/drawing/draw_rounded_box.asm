; Draw a framed box
.proc draw_rounded_box
  ; Constants
  TOP_LEFT_CORNER = $55
  TOP_RIGHT_CORNER = $49
  BOTTOM_LEFT_CORNER = $4A
  BOTTOM_RIGHT_CORNER = $4B
  VERTICAL_LINE = $42

  ; Vars
  BOX_X1 = zp_ARG0
  BOX_Y1 = zp_ARG1
  BOX_X2 = zp_ARG2
  BOX_Y2 = zp_ARG3
  COLOR = zp_ARG4

  CURRENT_CHAR = zp_TMP0
  COUNT = zp_TMP1
  LENGTH = zp_TMP2  ; used by draw_horizontal_line
@draw_rounded_box:
  vera_stride #$10          ; Set stride to 1
; First line
@top_left_corner:
  lda BOX_X1
  ldy BOX_Y1
  jsr goto_xy
  print_char_with_color #TOP_LEFT_CORNER, COLOR
@top_line:
  ; Length
  lda BOX_X2
  ;sbc #$00
  sbc BOX_X1
  ; Start X-Pos
  ldx BOX_X1
  inx
  ; Start Y-Pos
  ldy BOX_Y1
  jsr draw_horizontal_line
@top_right_corner:
  print_char_with_color #TOP_RIGHT_CORNER, COLOR

@sides:
  ldy BOX_Y1
  iny
  jsr goto_xy
@sides_loop:
  lda BOX_X1
  jsr goto_xy
  print_char_with_color #VERTICAL_LINE, COLOR
  lda BOX_X2
  jsr goto_xy
  print_char_with_color #VERTICAL_LINE, COLOR
  iny
  cpy BOX_Y2
  bne @sides_loop
  ;lda #VERTICAL_LINE
  ;jsr

@bottom_left_corner:
  lda BOX_X1
  ldy BOX_Y2
  jsr goto_xy
  print_char_with_color #BOTTOM_LEFT_CORNER, COLOR
@bottom_line:
  ; Length
  lda BOX_X2
  sbc BOX_X1
  ; X-Pos
  ldx BOX_X1
  inx
  ; Y-Pos
  ldy BOX_Y2
  jsr draw_horizontal_line
@bottom_right_corner:
  print_char_with_color #BOTTOM_RIGHT_CORNER, COLOR

@end:
  rts
.endproc
