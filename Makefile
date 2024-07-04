all:
	cl65 -t cx16 -C cx16.cfg -o MIDITEST.PRG miditest.asm
	cl65 -t cx16 -C cx16.cfg -o SERIALTEST.PRG serialtest.asm
clean:
	rm *.PRG *.o *.zip
zip:
	zip miditest.zip MIDITEST.PRG SCR/*
