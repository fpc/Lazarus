The <lazarusdir>/components/codetools/languages directory contains all the stuff
for internationalization of the codetools.

All text and messages used in the codetools should be placed into the unit
codetoolsstrconsts.pas. This unit uses a resourcestring section, so that the
compiler will create the
<lazarusdir>/components/units/codetoolsstrconsts.rst file.

The following is a in detail description of updating the language files for
the codetools. Normally you can just execute the <lazarusdir>/localize.sh
script.

Since this is a fpc-only format it must be converted with the rstconv program:

cd <lazarusdir>/components/codetools/languages
rstconv -i ../../units/codetoolsstrconsts.rst -o codetools.po

This will create the file codetools.po, which should be translated in all
required languages to a codetools.xx.po file. For the xx see the gettext unit
in the procedure TranslateResourceStrings.

german: codetools.de.po


After the translation, the codetools.xx.po file is converted to an .mo file
with the msgfmt program:

msgfmt -v -o codetools.xx.mo codetools.xx.po

where xx is the language id.


