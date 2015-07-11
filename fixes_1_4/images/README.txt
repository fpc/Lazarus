This directory contains all images needed by the IDE.
Images are stored in FPC resource files (.res). Each icon is stored
in png or xpm format.

bookmark.res - Icons for bookmarks
codetoolsdefines.res - Icons for CodeTools Defines Editor
components_images.res - Icons and pics for lcl components
laz_images.res - The icons for the speedbuttons, the menu (Open, Save,...),
                 package windows, codeexplorer
splash.res - the lazarus logo at IDE start
unitdependencies.res - Icons for the unit dependencies.

How to update the image resources:

Creating a lazarus resource can easily be done by the lazres program.
If you have not yet compiled lazres, go to the tools directory and type make.

1. bookmark.res

cd <lazarusdir>/images/
../tools/lazres bookmark.res sourceeditor/*.png


2. components_images.res

cd <lazarusdir>/images/
../tools/lazres components_images.res @components_images_list.txt


3. laz_images.res

cd <lazarusdir>/images/
../tools/lazres laz_images.res @laz_images_list.txt
rm ../main.ppu


4. splash.res

cd <lazarusdir>/images
./tools/lazres splash_logo.res splash_logo.png


5. lazdoc.res

cd <lazarusdir>/images/
../../tools/lazres ../ide/lazdoc.res @lazdoc.txt


