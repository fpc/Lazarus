SOURCES := $(wildcard *.pas)
OPTIONS := -O2
OBJ := *.o *.or *.ppu *.rsj

gir2pascal: gir2pascal.lpr $(SOURCES)
	fpc $(OPTIONS) $<

clean:
	rm -f gir2pascal $(OBJ)
