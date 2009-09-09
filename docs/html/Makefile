ifndef MKDIR
MKDIR=mkdir -p
endif
ifndef CP
CP=cp -Rfpl
endif
RM:=rm -rf

ifndef LAZBUILD
LAZBUILD=$(firstword $(wildcard ../../lazbuild) $(wildcard /usr/bin/lazbuild))
endif

ifndef FPC
FPC=/usr/bin/fpc
endif

build: htmldocs

htmldocs: build_lcl_docs
	./$< --outfmt html

chmdocs: build_lcl_docs
	./$< --outfmt chm

build_lcl_docs: build_lcl_docs.lpi build_lcl_docs.lpr
ifeq (${LAZBUILD},)
	echo 'Executable "lazbuild" not found, trying direct FPC call.'
	${FPC} -o$@ build_lcl_docs.lpr
else
	${LAZBUILD} $<
endif

clean:
	${RM} build_lcl_docs lcl

htmlinstall chminstall:
	install -d ${INSTALL_DOCDIR}
	${CP} -t ${INSTALL_DOCDIR} \
		lcl

.PHONY: build clean install htmldocs chmdocs
