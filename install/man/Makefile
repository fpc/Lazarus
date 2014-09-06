##################################
# Man page installation for unix
#

PREFIX=/usr
GINSTALL=/usr/bin/install

INSTALL_MANDIR=$(PREFIX)/share/man/man1

SRC_FILES=$(wildcard man1/*.1)
COMPRESSED_FILES=$(SRC_FILES:.1=.1.gz)

$(INSTALL_MANDIR):
	$(GINSTALL) -d -m 755 $(INSTALL_MANDIR)

%.1.gz: %.1
	cat $< | gzip > $@

install: $(COMPRESSED_FILES) $(INSTALL_MANDIR)
	$(GINSTALL) -m 644 $(COMPRESSED_FILES) $(INSTALL_MANDIR)
	$(RM) $(COMPRESSED_FILES)
