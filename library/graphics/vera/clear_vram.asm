; Clear VRAM
; Clear VRAM in 255 (low address) chunks
; a = Vera Bank
; r1 = med address start
; r2 = med address end
.proc clear_vram
START = r1
END = r2
clear_vram:
  ; Set stride
  ora #%00010000
  sta VERA_addr_high
  
  lda START
@loop:
  sta VERA_addr_med
  jsr clear_vram_low
  cmp END
  beq @done
  adc #$01
  jmp @loop
@done:
  rts

; Clear all low address range of VRAM
clear_vram_low:
  pha
  ldx #$FF
  stz VERA_addr_low
@loop:
  stz VERA_data0
  stz VERA_data0
  dex
  beq @done
  jmp @loop
@done:
  pla
  rts
.endproc
