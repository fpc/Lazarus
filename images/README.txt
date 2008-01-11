This directory contains all images needed by the IDE.
Images are stored in lazarus resource files (.lrs). Each icon is stored
in xpm format.

bookmark.lrs - Icons for bookmarks
codetoolsdefines.lrs - Icons for CodeTools Defines Editor
components_images.lrs - Icons and pics for lcl components
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

cd <lazarusdir>/images/
../tools/lazres components_images.lrs @components_images_list.txt

4. mainicon.lrs

cd <lazarusdir>/images/
../tools/lazres mainicon.lrs mainicon.xpm


5. laz_images.lrs

cd <lazarusdir>/images/
../tools/lazres laz_images.lrs @laz_images_list.txt
rm ../main.ppu


6. splash.lrs

cd <lazarusdir>/
./tools/lazres splash.lrs images/splash_logo.xpm


7. unitdependencies.lrs

cd <lazarusdir>/images/unitdependencies
../../tools/lazres ../../unitdependencies.lrs *.xpm


Others

in form ide/editoroptions_new.lfm
images/keymaprelation.png, images/keymapcategory.png

in form ide/fpdoceditwindow.lfm
images/lazdoc/*.png

