; Load 16 color palette
.proc load_palette_16
load_palette_16:
  lda #$11
  sta VERA_addr_high
  lda #$FA
  sta VERA_addr_med
  lda #$00
  sta VERA_addr_low
  ldx #$00
@loop:
  lda palette,x
  sta VERA_data0
  inx
  cpx #$20
  bne @loop
  rts
.endproc
