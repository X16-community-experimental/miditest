; Graphic routines

.scope graphics
  ; Vera specific routines
  .scope vera
    .include "library/graphics/vera/clear_vram.asm"
    .include "library/graphics/vera/load_palette16.asm"
  .endscope

  ; Drawing rountines
  .scope drawing
    .include "library/graphics/drawing/goto_xy.asm"
    .include "library/graphics/drawing/print_character.asm"
    .include "library/graphics/drawing/print_hex.asm"
    .include "library/graphics/drawing/print_alpha_char.asm"
    .include "library/graphics/drawing/print_string.asm"
    .include "library/graphics/drawing/cursor.asm"
    .include "library/graphics/drawing/chars_to_number.asm"
  .endscope

.endscope
