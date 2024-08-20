#!/bin/bash

ROM="rom.bin"
SCALE=2
MHZ=8
PRG="WT.PRG"
#PRG="MIDITEST.PRG"
#PRG="VIATEST.PRG"

make
./x16emu -rom $ROM -scale $SCALE -prg $PRG -ram 512  -run -mhz $MHZ
#./box16 -rom $ROM -scale $SCALE -prg $PRG -ram 512  -run 
