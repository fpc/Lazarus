This directory contains all images needed by the Editor Toolbar package.
Images are stored in FPC resource files (.res). Each icon is currently stored
in xpm format but also png can be used.

toolbar.res - All the icons used in Editor Toolbar package.


How to update the image resources:

Creating a lazarus resource can easily be done by the lazres program.
If you have not yet compiled lazres, go to the tools directory and type make.


[ toolbar.res ]

cd <packagedir>/images/
<lazarusdir>/tools/lazres ../toolbar.res @toolbar.txt

