Quick Start for translators
===========================

### Basic workflow

Finnish translation is used as an example:

1. Make sure that you have up-to-date Lazarus from Git (main branch).
2. Search for all *.pot files ([PoChecker](../components/pochecker) addon in `<lazarusdir>/components/pochecker` directory can help you with this).
3. If POT file belongs to some package, make sure that this package is installed in IDE.
4. Rebuild Lazarus clean. This will ensure that POT files for all installed packages are up-to-date.
5. Run `sh localize.sh` (Linux) or `localize.bat` (Windows) to update IDE translations.
6. Edit PO files (*.fi.po) with programs like Poedit (www.poedit.net) or Lokalize. Do NOT edit them manually because this WILL lead to breakage.
   If .fi.po file is missing for particular component, use template (POT file) to create it.
7. Check your translated PO files with PoChecker tool and fix all
   reported errors (you need to run all tests, there should be no errors shown in
   General Info tab of Results window).
8. Inspect visually resulting file differences, refresh translation files if needed manually as described below.
9. If refreshing has been performed, check your translated PO files with PoChecker tool one more time to ensure that there are no new untranslated strings (which may have appeared after refresing). Edit your translations if needed and then perform the previous and this step again.
10. Create a merge request (preferred) or post updated xxx.fi.po files to the Lazarus issue tracker.  
   Do NOT post diffs for PO files. Due to their nature, PO files are prone to conflicts and therefore diffs can rather quickly get very hard or plain impossible to apply.

### Translation families and how to find them

Translation files are organized into families.

Each translation family consists of template and actual translations into various languages (in UTF-8 encoding).
Template has .pot extension, translations have .po extensions and include ISO language code in their file names.

Template is used as a base for updating actual translation files and starting translation into new languages.  
Note that if POT files of packages are updated, all translations will be updated too. If POT files have already been up-to-date, translations won't be touched. In this case you may need to refresh (regenerate) them manually as described below.

Example for the Lazarus IDE translation family:

| Language     | File             |
| ---          | ---              |
| _Template_   | _lazaruside.pot_ |
| German       | lazaruside.de.po |
| Russian      | lazaruside.ru.po |
| Spanish      | lazaruside.es.po |
| French       | lazaruside.fr.po |
| Italian      | lazaruside.it.po |

The `<lazarusdir>/languages` directory contains translation family for Lazarus IDE.
There are also other directories for CodeTools, LCL and various packages.

You can scan Lazarus directory with PoChecker tool in order to find all translations (including
missing ones) for a given language.

### Refreshing (regenerating) translations

If you know what you are doing, you can use PoChecker tool to refresh (regenerate) translation families (i.e. synchronize
actual translations to template files). In order to get an access to this function, you should
run tests for your translation language, open Graphical Summary window (via 'Show statistics graph' button)
and click on any translation family with right mouse button while pressing SHIFT key.

Note that refresh function REQUIRES translation files to be accessible for WRITING. 

It is IMPORTANT to use only [PoChecker](../components/pochecker) or up-to-date [updatepofiles](../tools/updatepofiles.lpi) tool for refresing translation families. Using other tools like built-in into your PO editor can lead to breakage.

Make sure to rebuild first all packages whose translations will be refreshed (follow the steps from basic workflow described above). This is needed to make sure that all POT files are up-to-date.

Regular refreshing (regenerating) translations as described above and examining the resulting differences is important for the following reasons:
1. PO files can be outdated (i.e. not matching its respective .pot file) for some reason, for example when you are applying a translation sent by an external contributor.
2. PO file can contain strings with formatting arguments mismatches. Such strings (which are ignored by Lazarus translation system anyway because using them will lead to crashes) will be marked fuzzy during refreshing (regenerating) thus allowing to spot and correct them.
3. PO file can have meaningless changes like wrapping of lines, removal of trailing newlines at end of file, flag order changes etc. They will be removed during refreshing (regenerating) thus cleaning up diffs and simpifying review process.
4. PO file can be malformed. In general it is impossible to fix all types of such errors automatically, but refresing will allow to spot such occurrences and take the correct action manually.

### Resolving conflicts

While you work on your translations, it may happen that the files you modified have received updates in upstream repository.
In this case trying to update your working copy can likely lead to conflicts (as already was mentioned earlier, due to their nature, PO files are prone to conflicts).
Do NOT try to resolve these conflicts interactively because this WILL lead to breakage. The safe method of updating is the following:
1. Save your translations and backup them.
2. Revert all your changes in working copy.
3. Update your working copy.
4. Replace relevant translation files in updated working copy with the ones from your backup.
5. Rebuild Lazarus as described above.
6. Refresh (regenerate) translation files using PoChecker.
7. Scan your translation files with PoChecker one more time and review the results.
8. Edit as usual.
