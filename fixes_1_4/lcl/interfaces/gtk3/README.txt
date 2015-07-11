TODO:
* Cursors
* Themes (gtk3themes)
* Paint system (brush is critical)
* Paint system coordinates and transformations.
* Paint system text
* RawImage (bitmap, device)
* Other missing ws controls (trayicon, dragimagelist, splitter etc).
* Clipboard
* Caret
* Dialogs
* Designer
* Fix modal windows behaviour under kwin (metacity and xfwm works ok for now).
* For beta status move all signal handlers into separate unit, so gtk3widgets stay
  clean.
* Keep interface as simple as possible ! *
* If there's any problem which is fixed unlogical code,
  check if it's gtk3 bug first.



*KNOWN PROBLEMS:
1. GtkTextView inside GtkScrolledWindow with disabled scrollbars (GTK_POLICY_NONE)
   grows memo control. Seem that it's fixed in 3.8.2
   https://bugzilla.gnome.org/show_bug.cgi?id=688472
   https://bugzilla.gnome.org/show_bug.cgi?id=658928
   POSSIBLE SOLUTION IS TO OVERRIDE PGtkTextViewClass() for get_preferred_size

2. cairo clip rect in paint event have strange bounds when it paints only part
   of control (added temporary workaround after TGtk3Widget.GtkEventPaint call).

3. Window state isn't accurate (GtkWindow active/inactive property isn't updated
   correctly by gtk.

4. GtkNotebook switch-page cannot control is we forbid page switch when user clicks
   on it (tab).

Some links about gtk3:
http://igurublog.wordpress.com/tag/gtk3/


*NAMING OF UNITS* inside of gtk3bindings subdirectory:
All units are prefixed with laz eg. lazgtk3.pas to avoid
clashes with future versions of fpc which will probably have package named gtk3
and gtk3.pas unit inside.



