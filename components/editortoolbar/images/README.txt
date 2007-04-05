This directory contains all images needed by the Editor Toolbar package.
Images are stored in lazarus resource files (.lrs). Each icon is stored
in xpm format and in lowercase.

toolbar.lrs - All the icons used in Editor Toolbar package.


How to update the image resources:

Creating a lazarus resource can easily be done by the lazres program.
If you have not yet compiled lazres, go to the tools directory and type make.


[ toolbar.lrs ]

cd <packagedir>/images/
<lazarusdir>/tools/lazres ../toolbar.lrs *.xpm

