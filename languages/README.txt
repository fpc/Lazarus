Quick Start for translators:

For example Finnish translation:
Search for all *.fi.po files. They are simple text files with an easy format.
Edit them with programs like poEdit (www.poedit.net) or KBabel. Do not edit them manually.
Run 'sh localize.sh' (Linux) or 'localize.bat'(Windows) to update translations.

Check your translated .po files with PoChecker tool (lazarus/components/pochecker/) and fix all reported errors.

Post the updated xxx.fi.po files to bug tracker (http://bugs.freepascal.org).
Do NOT post diffs for .po files.


Now the background:

The <lazarusdir>/languages directory contains all the stuff for
internationalization of the lazarus IDE.

There are a few other directories for the codetools and the lcl. They work
all the same. All language files can easily be updated with the
<lazarusdir>/localize.sh or <lazarusdir>\localize.bat script.


All text and messages used in the IDE (except the special designer units)
should be placed into the unit lazarusidestrconsts.pas. This unit uses a
resourcestring section, so that the compiler will create the
<lazarusdir>/lazarusidestrconsts.rst file.
Since this is a fpc-only format it must be converted with the rstconv program:

cd <lazarusdir>/languages
rstconv -i ../lazarusidestrconsts.rst -o lazaruside.po

Hint: this is done by <lazarusdir>/localize.sh or <lazarusdir>\localize.bat.

This will create the file lazaruside.po, which should be translated in all
required languages to a lazaruside.xx.po file (it should have UTF-8 encoding). For the xx see the gettext unit
in the procedure TranslateResourceStrings.

For example:

German:      lazaruside.de.po
Russian:     lazaruside.ru.po
Spanish:     lazaruside.es.po
French:      lazaruside.fr.po
Italian:     lazaruside.it.po
