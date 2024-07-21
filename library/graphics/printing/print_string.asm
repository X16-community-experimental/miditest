; Print a null-terminated string

; #SCREENCODE_RETURN ($0D), jump to first X position, Y+1
; #SCREENCODE_COLOR ($01), next byte sets color
; #SCREENCODE_XY ($02), next 2 bytes set X,Y screen position
; #SCREENCODE_PRINT_MODE ($04), whether or not to do PETSCII conversion
;   if next byte is 0 do PETSCII conversion
;   if 1, assume screencodes

.proc print_string
  STRING_POINTER = zp_ARG0
  X_POS = zp_ARG1
  Y_POS = zp_ARG2
  NEW_COLOR_FLAG = zp_MATH0
  GOTO_XY_FLAG = zp_MATH1
  PRINT_MODE_FLAG = zp_MATH2
  PRINT_MODE = zp_MATH3
@print:
  stz NEW_COLOR_FLAG
  stz GOTO_XY_FLAG
  stz PRINT_MODE_FLAG
  stz PRINT_MODE
  
  ; Set stride to 1
  vera_stride #%00010000
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  lda Y_POS        ; y coord
  sta VERA_addr_med
@loop:
@check_new_color:
  lda NEW_COLOR_FLAG
  beq @check_goto_xy
@found_new_color:
  lda (STRING_POINTER)
  sta zp_TEXT_COLOR
  stz NEW_COLOR_FLAG
  jmp @loop_end
@check_goto_xy:
  lda GOTO_XY_FLAG
  beq @check_print_mode
@found_goto_xy:
  lda (STRING_POINTER)
  sta X_POS
  pha
  add16to8 STRING_POINTER, #$01, STRING_POINTER
  lda (STRING_POINTER)
  ; This updates the return/newline
  tay
  sty Y_POS
  pla
  jsr graphics::drawing::goto_xy
  stz GOTO_XY_FLAG
  bra @loop_end
@check_print_mode:
  lda PRINT_MODE_FLAG
  beq @check_null
@change_print_mode:
  lda (STRING_POINTER)
  sta PRINT_MODE
  stz PRINT_MODE_FLAG
  bra @loop_end
@check_null:
  lda (STRING_POINTER)
  ; If we see a null terminated string, end
  bne @continue
  rts
@continue:
  cmp #SCREENCODE_PRINT_MODE
  beq @print_mode
  cmp #SCREENCODE_RETURN  ; Our special screencode value to indicate return
  beq @return
  cmp #SCREENCODE_COLOR ; If we read a color change value, update color
  beq @new_color
  cmp #SCREENCODE_XY    ; If we read an XY change value, update position
  beq @goto_xy
@check_subtract_az:
  lda PRINT_MODE
  beq @subtract_az
  lda (STRING_POINTER)
  bra @nosub
@subtract_az:
  lda (STRING_POINTER)
  cmp #$40               ; Only subtract if it's A-Z
  bmi @nosub
  sec                   ; Converting from PETSCII to Screen Codes
  sbc #$40
  bra @nosub
; If we saw a new color code, set the flag and return.
; On next loop around we'll update the color
@new_color:
  lda #$01
  sta NEW_COLOR_FLAG
  bra @loop_end
@goto_xy:
  lda #$01
  sta GOTO_XY_FLAG
  bra @loop_end
@return:
  lda X_POS
  asl               ; shift because second byte is color
  sta VERA_addr_low
  ldx Y_POS        ; y coord
  inx
  stx VERA_addr_med
  stx Y_POS
  bra @loop_end
@print_mode:
  lda #$01
  sta PRINT_MODE_FLAG
  bra @loop_end
@nosub:
  sta VERA_data0
  lda zp_TEXT_COLOR
  sta VERA_data0
@loop_end:
  add16to8 STRING_POINTER, #$01, STRING_POINTER
  jmp @loop
.endproc
