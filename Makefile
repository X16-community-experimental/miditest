all:
	cl65 -t cx16 -C cx16.cfg -l viatest.lst -o VIATEST.PRG viatest.asm
#	cl65 -t cx16 -C cx16.cfg -l miditest.lst -o MIDITEST.PRG miditest.asm
#	cl65 -t cx16 -C cx16.cfg -l wt.lst -o WT.PRG wt.asm
#	cl65 -t cx16 -C cx16.cfg -o SERIALTEST.PRG serialtest.asm
clean:
	rm *.PRG *.o *.zip
zip:
	zip miditest.zip MIDITEST.PRG SCR/*
