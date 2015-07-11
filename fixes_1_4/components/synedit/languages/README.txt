The <lazarusdir>/components/synedit/languages directory contains all the stuff
for localization of synedit.

All text and messages used in synedit should be placed into the unit
syneditstrconst.pp. This unit uses resourcestring sections, so that the
compiler will create the
<lazarusdir>/components/units/syneditstrconst.rst file.
<lazarusdir>/components/units/synmacrorecorder.rst file.

The following is a in detail description of updating the language files for
synedit. Normally you can just execute the <lazarusdir>/localize.sh script.

Because this is a fpc-only format it must be converted with the rstconv program:

cd <lazarusdir>/components/synedit/languages
rstconv -i ../../units/syneditstrconst.rst -o synedit.po
rstconv -i ../../units/synmacrorecorder.rst -o synmacrorecorder.po

This will create the files synedit.po and synmacrorecorder.po, which should be
translated in all required languages to a synedit.xx.po and a
synmacrorecorder.xx.po file. For the xx see the gettext unit in the procedure
TranslateResourceStrings of the IDE.

german: synedit.de.po and synmacrorecorder.de.po


After the translation, the both po file are converted to .mo files with the
msgfmt program:

msgfmt -v -o synedit.xx.mo synedit.xx.po
msgfmt -v -o synmacrorecorder.xx.mo synmacrorecorder.xx.po

where xx is the language id.


