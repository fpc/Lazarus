# Fast unit open

This package registers a shortcut key for a dialog in which you can type a
unit name. The IDE will then search for the given name in the search path,
and will open the unit.

It is similar to the 'Open file at cursor' functionality but it can be used
to open a file which is not in a uses clause (or in the sources) without
having to look for it.

The dialog is registered below the 'Search' menu, and the shortcut is set to
Ctrl-Shift-Enter. This can be changed in the keymap settings.
