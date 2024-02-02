; Loads an 80x60 screen off disk previously made with PetdrawX
; Useful for showing help screens and things
;
; Note we are not using the LOAD direct into VRAM here (that happens when A=2) 
; becuase the viewing window is smaller than the map size, but this does
; mean loading the screens off the SD card is a bit slow.

.proc load_to_vram
; a = file length
; x/y = file pointer

FILE_NUMBER = $01
DISK_DEVICE = $08
OPEN_FOR_READ = $00   ; Open file for reading

BYTES_PER_LINE = $A0    ; 160 (80 columns * 2, for char and color)

@start:
    ; Open file (a = length, x/y = pointer)
	jsr	SETNAM	;SETNAM A=FILE NAME LENGTH X/Y=POINTER TO FILENAME

    lda	#FILE_NUMBER
	ldx	#DISK_DEVICE
	ldy	#OPEN_FOR_READ
	jsr	SETLFS	;SETLFS A=LOGICAL NUMBER X=DEVICE NUMBER Y=SECONDARY
    jsr OPEN


    ldx #FILE_NUMBER
    jsr CHKIN

    ; Skip 2 byte header
    jsr CHRIN
    jsr CHRIN

    ; Setup VERA
    lda #%00010000	; Inc set to 1, low ram
	sta	VERA_addr_high
	stz	VERA_addr_low
	stz	VERA_addr_med

@load_start:
    ldy #$00
; Use MACPTR to read all the bytes for each row off disk at once
; Was preivously using CHRIN but that was *slow*
; LOAD would be faster, however it loads the entire file
; contiguously which doesn't work here because we need to skip
; VRAM which isn't in the current viewing window
; (The map is larger than the window)
@read_row:
    phy
    sec                  ; do not increment address
    lda #BYTES_PER_LINE
    ldx #<VERA_data0     ; low byte
    ldy #>VERA_data0     ; high byte
    jsr MACPTR           ; MACPTR FTW!
    ply
@next_column:
    iny
    sty VERA_addr_med
    stz VERA_addr_low
    cpy #SCREEN_Y
    bne @read_row
@end:
    lda #FILE_NUMBER
    jsr CLOSE
    jsr CLRCHN
	rts	
.endproc