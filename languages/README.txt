Quick Start for translators:

For example Finnish translation:

1. Make sure that you have up to date Lazarus from SVN trunk.
2. Search for all *.fi.po files.
3. If PO file belongs to some package, make sure that this package is installed in IDE.
4. Rebuild Lazarus clean. This will update most packages translations.
5. Run 'sh localize.sh' (Linux) or 'localize.bat' (Windows) to update all remaining translations.
6. PO files are simple text files with an easy format.
   Edit them with programs like poEdit (www.poedit.net) or KBabel. Do not edit them manually.
7. Check your translated PO files with PoChecker tool (lazarus/components/pochecker/) and fix all
   reported errors (you need to run only basic tests).
8. Post the updated xxx.fi.po files to bug tracker (http://bugs.freepascal.org).
   Do NOT post diffs for PO files.

Now the background:

The <lazarusdir>/languages directory contains stuff for internationalization of the Lazarus IDE.

There are other directories for CodeTools, LCL and various packages.
All language files can easily be updated by rebuilding Lazarus (make sure that packages,
for which you need to update translations, are installed in IDE) and then running
<lazarusdir>/localize.sh or <lazarusdir>\localize.bat script.

All text and messages used in the IDE (except special designer units)
should be placed into the unit lazarusidestrconsts.pas. This unit uses a
resourcestring section, so that the compiler will create the
<lazarusdir>/lazarusidestrconsts.rst file.
Since this is a FPC-only format it must be converted with the rstconv program:

cd <lazarusdir>/languages
rstconv -i ../lazarusidestrconsts.rst -o lazaruside.po

Hint: this is done by <lazarusdir>/localize.sh or <lazarusdir>\localize.bat.

This will create the file lazaruside.po, which should be translated in all
required languages to a lazaruside.xx.po file (it should have UTF-8 encoding).
For the xx see the gettext unit in the procedure TranslateResourceStrings.

For example:

German:      lazaruside.de.po
Russian:     lazaruside.ru.po
Spanish:     lazaruside.es.po
French:      lazaruside.fr.po
Italian:     lazaruside.it.po
