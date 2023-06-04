SOURCES=$(wildcard *.pas)
TARGET_CPU=$(shell fpc -iTP)
TARGET_OS=$(shell fpc -iTO)
UNITS_DIR=lib/tools/gir2pascal/${TARGET_CPU}-${TARGET_OS}
OPTIONS=\
 -MObjFPC\
 -Scghi\
 -Cg\
 -O1\
 -gw2\
 -godwarfsets\
 -gl\
 -l\
 -vewnhibq\
 -Fi${UNITS_DIR}\
 -Fu${UNITS_DIR}\
 -FU${UNITS_DIR}\
 -FE.\

OBJ=*.o *.or *.ppu *.rsj

gir2pas:gir2pascal.lpr $(SOURCES)
	fpc $(OPTIONS) $< -o$@

clean:
	${RM} gir2pas ${UNITS_DIR} $(OBJ)

.PHONY: clean
