.proc clear_screen
  CLEAR = $93 ; PETSCI value to clear the screen
  clear_screen:
     ; set background color in a dirty manner (we did not hear that from Greg King)
     ; https://www.commanderx16.com/forum/index.php?/topic/469-change-background-color-using-vpoke/&do=findComment&comment=3084
     lda #$01
     sta $376
     lda #CLEAR
     jsr CHROUT
     rts
.endproc
