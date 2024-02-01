#!/bin/bash

ROM="rom.bin"
SCALE=2
PRG="MIDITEST.PRG"

make
./x16emu -rom $ROM -scale $SCALE -prg $PRG -ram 512  -run 
