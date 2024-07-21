; Graphic routines
.segment "CODE"

.scope graphics
  ; Vera specific routines
  .scope vera
    .include "library/graphics/vera/clear_vram.asm"
    .include "library/graphics/vera/load_palette16.asm"
  .endscope

  .include "library/graphics/drawing/main.asm"
  .include "library/graphics/printing/main.asm"

.endscope
