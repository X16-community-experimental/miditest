; Draw a solid box
.proc draw_solid_box
  ; Constants
  SPACE = $20
  ; Vars
  BOX_X1 = zp_ARG0
  BOX_Y1 = zp_ARG1
  BOX_X2 = zp_ARG2
  BOX_Y2 = zp_ARG3
  COLOR = zp_ARG4
  ; Temp
  COUNT_Y = zp_TMP0
  ;BOX_Y_END = r7

  draw_solid_box:
    vera_stride #$10          ; Set stride to 1
    inc BOX_X2
    inc BOX_Y2
    ldy BOX_Y1
  @loop_y:
    lda BOX_X1
  @line:
    pha
    asl
    sta VERA_addr_low
    pla
    sty VERA_addr_med
  @line_loop:
    ldx #SPACE
    stx VERA_data0 ; Write chracter
    ldx COLOR
    stx VERA_data0 ; Write color
    inc
    cmp BOX_X2
    bne @line_loop
@end_line_loop:
    iny
    cpy BOX_Y2
    bne @loop_y
    rts
.endproc
