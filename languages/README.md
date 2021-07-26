Quick Start for translators
===========================

For example Finnish translation:

1. Make sure that you have up-to-date Lazarus from Git (main branch).
2. Search for all *.pot files (PoChecker tool in `<lazarusdir>/components/pochecker` directory can help you with this).
3. If POT file belongs to some package, make sure that this package is installed in IDE.
4. Rebuild Lazarus clean. This will update translations for most packages.
5. Run `sh localize.sh` (Linux) or `localize.bat` (Windows) to update all remaining translations.
6. Edit PO files (*.fi.po) with programs like poEdit (www.poedit.net) or Lokalize. Do not edit them manually.
   If .fi.po file is missing for particular component, use template (POT file) to create it.
7. Check your translated PO files with PoChecker tool and fix all
   reported errors (you need to run all tests, there should be no errors shown in
   General Info tab of Results window).
8. Post updated xxx.fi.po files to the Lazarus issue tracker.
   Do NOT post diffs for PO files.

Some notes:

The `<lazarusdir>/languages` directory contains stuff for internationalization of the Lazarus IDE.
There are also other directories for CodeTools, LCL and various packages.

You can scan Lazarus directory with PoChecker tool in order to find all translations (including
missing ones) for a given language.

Each translation family consists of template and actual translations to various languages (in UTF-8 encoding).
Template has .pot extension, translations have .po extensions and include ISO language code in their file names.

Template is used as a base for updating actual translation files and starting translation into new languages.

Example for the IDE translation family:

| Language     | File             |
| ---          | ---              |
| _Template_   | _lazaruside.pot_ |
| German       | lazaruside.de.po |
| Russian      | lazaruside.ru.po |
| Spanish      | lazaruside.es.po |
| French       | lazaruside.fr.po |
| Italian      | lazaruside.it.po |

If you KNOW what you are DOING, you can use PoChecker tool to refresh translation families (i.e. synchronize
actual translations with template files). In order to get an access to this function, you should
run tests on your translation, open Graphical Summary window (via 'Show statistics graph' button)
and click on any translation family with right mouse button while pressing SHIFT key.
Note that refresh function REQUIRES translation files to be accessible for WRITING.