##################################
# Man page installation for unix
#

override PATH:=$(subst \,/,$(PATH))
SEARCHPATH:=$(filter-out .,$(subst :, ,$(PATH)))


ifndef INSTALL_PREFIX
INSTALL_PREFIX=/usr/local
endif

ifndef GINSTALL
GINSTALL:=$(strip $(wildcard $(addsuffix /ginstall,$(SEARCHPATH))))
ifeq ($(GINSTALL),)
GINSTALL:=$(strip $(wildcard $(addsuffix /install,$(SEARCHPATH))))
ifeq ($(GINSTALL),)
GINSTALL=
else
GINSTALL:=$(firstword $(GINSTALL))
endif
else
GINSTALL:=$(firstword $(GINSTALL))
endif
endif

INSTALL_MANDIR=$(INSTALL_PREFIX)/man

installman:
	$(GINSTALL) -d -m 755 $(INSTALL_MANDIR)/man1
#	$(GINSTALL) -d -m 755 $(INSTALL_MANDIR)/man5
	$(GINSTALL) -c -m 644 man1/*.1 $(INSTALL_MANDIR)/man1
#	$(GINSTALL) -c -m 644 man5/*.5 $(INSTALL_MANDIR)/man5
