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



ADDING IMAGES TO LAZARUS POLICY
-------------------------------
Images in Lazarus can be under CC, GPL, LGPL, public domain or under the
LGPL-modified license.
When you are adding images to Lazarus add information about their origin in SVN
commit log. If the icons are under CC or need an acknowledgement, be sure to
update the Help->About Lazarus->Acknowledgements info.

Please use Tango icons whenever possible because they are SVG and exist in
various PNG resolutions - it will make updating icons for high-DPI easy in the
future.
You can download Tango icons from
https://packages.debian.org/sid/tango-icon-theme or directly
http://http.debian.net/debian/pool/main/t/tango-icon-theme/tango-icon-theme_0.8.90.orig.tar.gz
Use supplied ready-to-go PNGs in the needed resolution.

Please note that policy for LCL images is different (more strict).
See lcl/images/README.txt for info about LCL icon policy.

HIGH-DPI ICONS
--------------

Every icon can have its 150% and 200% version. For a 16px, the sizes are:
24px and 32px respectively.
To use a high-DPI icon, register the icon with the suffix "_150" (150%) or
"_200" (200%). See e.g. actions/laz_save_150.png

