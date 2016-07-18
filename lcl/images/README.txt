This directory contains all images needed by the LCL.
Images are stored in FPC resource files (.res). Each icon is stored
in xpm or png format.

lcl_dbgrid_images.res - Icons, included by dbgrids.pas
lcl_dbnav_images.res - Icons, included by dbctrls.pas


How to update the image resources:

Creating a lazarus resource can easily be done by the lazres program.
If you have not yet compiled lazres, go to the tools directory and type make.


Under windows use script files: lcl_dbgrid_images.bat, lcl_dbnav_images.bat,
Under other systems use similar script - just replace directory separator.

Please see copyright.txt for copyright and license information.



ADDING IMAGES TO LCL POLICY
---------------------------
All images in the LCL must be public domain or especially created for the LCL and under the LCL license.
When you are adding images to the LCL add information about their origin here in the list and also in SVN commit log.

!!! Do not add images under other licenses (CC, GPL, LGPL ...) or unknown origin into the LCL !!!

Please use Tango icons whenever possible because they are SVG and exist in various PNG resolutions - it will make updating LCL icons for high-DPI easy in the future.
You can download Tango icons from https://packages.debian.org/sid/tango-icon-theme or directly http://http.debian.net/debian/pool/main/t/tango-icon-theme/tango-icon-theme_0.8.90.orig.tar.gz
Use supplied ready-to-go PNGs in the needed resolution.

Please note that policy for Lazarus images is different (less strict). See images/README.txt for info about Lazarus icon policy.