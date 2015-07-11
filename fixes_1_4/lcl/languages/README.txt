The lcl/languages directory contains all the stuff for internationalization of
the lcl.

All text and messages used in the lcl should be placed into the unit
lcl/lclstrconsts.pas. This unit uses a resourcestring section, so that the
compiler will create the lcl/units/lclstrconsts.rst file.
Since this is a fpc-only format it must be converted with the rstconv program:

cd lcl/languages
rstconv -i ../units/lclstrconsts.rst -o lcl.po

This will create the file lcl.po, which should be translated in all required
languages to a lcl.xx.po file. For the xx see the gettext unit in the procedure
TranslateResourceStrings.

german: lcl.de.po


After the translation, the lcl.xx.po file is converted to an .mo file with the
msgfmt program:

msgfmt -v -o lcl.xx.mo lcl.xx.po

where xx is the language id.


