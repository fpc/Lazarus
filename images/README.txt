This directory contains all images needed by the IDE.
Images are stored in lazarus resource files (.lrs). Each icon is stored
in xpm format.

bookmark.lrs - Icons for bookmarks
codetoolsdefines.lrs - Icons for CodeTools Defines Editor
components_images.lrs - Icons and pics for lcl components
editoroptions.lrs - Icons for editor options
mainicon.lrs - This is the IDE program icon.
laz_images.lrs - The icons for the speedbuttons, the menu (Open, Save,...),
                 package windows, codeexplorer
splash.lrs - the lazarus logo at IDE start
unitdependencies.lrs - Icons for the unit dependencies.


How to update the image resources:

Creating a lazarus resource can easily be done by the lazres program.
If you have not yet compiled lazres, go to the tools directory and type make.


1. bookmark.lrs

cd <lazarusdir>/images/sourceeditor/
../../tools/lazres ../bookmark.lrs *.xpm


2. codetoolsdefines.lrs

cd <lazarusdir>/images/codetoolsdefines
../../tools/lazres ../../codetoolsdefines.lrs *.xpm


3. components_images.lrs

cd <lazarusdir>/images/components/
../../tools/lazres ../components_images.lrs *.xpm


4. editoroptions.lrs

cd <lazarusdir>/images/
../tools/lazres ../editoroptions.lrs keymaprelation.xpm keymapcategory.xpm


5. mainicon.lrs

cd <lazarusdir>/images/
../tools/lazres mainicon.lrs mainicon.xpm


6. laz_images.lrs

cd <lazarusdir>/images/
../tools/lazres laz_images.lrs btn_*.xpm pkg_*.xpm arrow_*.xpm menu/menu_*.xpm menu/menu_*.png codeexplorer/*.xpm designer/*.png debugger/*.png packages/*.png
rm ../main.ppu


7. splash.lrs

cd <lazarusdir>/
./tools/lazres splash.lrs images/splash_logo.xpm


8. unitdependencies.lrs

cd <lazarusdir>/images/unitdependencies
../../tools/lazres ../../unitdependencies.lrs *.xpm



