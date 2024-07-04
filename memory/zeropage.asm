.zeropage

; Previous ISR
PREVIOUS_ISR: .word $0000

; The IO Address range for MIDI
zp_MIDI_IO_BASE: .word $0000

; Value to set UART divisor baud rate
zp_BAUD_RATE: .word $0000

; Value to write to UART scratch register
zp_SCRATCH_VALUE: .byte 0

; Color of text for drawing things
zp_TEXT_COLOR: .byte 0

; Keyboard input from user
zp_KEY_PRESSED: .byte 0

; Settings toggles
zp_MIDI_OUT_TOGGLE: .byte 0
zp_INTTERUPTS_TOGGLE: .byte 0
zp_FIFO_TOGGLE: .byte 0
zp_READ_LOOP_TOGGLE: .byte 0
zp_ECHOBACK_TOGGLE: .byte 0

zp_FIFO_SHADOW: .byte 0
zp_RX_BUFFER_SHADOW: .byte 0
zp_ECHO_BYTE: .byte 0

; Our own temporary variables
; Function call arguments
zp_ARG0: .word $00
zp_ARG1: .word $00
zp_ARG2: .word $00
zp_ARG3: .word $00
zp_ARG4: .word $00
zp_ARG5: .word $00
zp_ARG6: .word $00
zp_ARG7: .word $00
zp_ARG8: .word $00

; Temporary variables
zp_TMP0: .byte $00
zp_TMP1: .byte $00
zp_TMP2: .byte $00
zp_TMP3: .byte $00
zp_TMP4: .byte $00
zp_TMP5: .byte $00
zp_TMP6: .byte $00
zp_TMP7: .byte $00
zp_TMP8: .byte $00
zp_TMP9: .byte $00
zp_TMPA: .byte $00

; Temp vars for math operations
zp_MATH0: .word $0000
zp_MATH1: .word $0000
zp_MATH2: .word $0000
zp_MATH3: .word $0000
zp_MATH4: .word $0000
zp_MATH5: .word $0000

; Temp vars for address manipulation
zp_ADDR0: .word $0000
zp_ADDR1: .word $0000
zp_ADDR2: .word $0000
; Special value for returning from a jump table
; (e.g. the effects table)
zp_ADDR_RETURN: .word $0000

.segment "EXTZP"




