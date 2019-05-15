Notes for this example targeting 32 bits Windows:

This cairocanvas example require the Cairo Graphics and Pango libraries.

There are instructions for downloading precompiled binaries for windows 
in the web page: https://www.cairographics.org/download/

In this example the binaries for The Glade/GTK+ for Windows Toolkit will be used, 
Because they are available as a single GTK+ setup file that install only the 
most basic runtime libraries needed to run GTK+ applications, including cairo
applications. 

Probably more rencent files can be obtained from the official GTK+ for windows
web page, but that would require installing either MSYS2 or vcpkg, which is 
overkill for the purpose of this example. At the time of this writing
(14-may-2019), the glade binaries are still available in the following 
link: http://gladewin32.sourceforge.net/ 

Where the next version is selected:

"
(2008 Apr 20 02:58 HKT) Updated cairo, freetype, glade, libglade to their latest stable release:

Gtk+ 2.12.9 Runtime Environment Revision 2 (7,206)
"

This is a setup program that installs some old GTK files in some user chosen directory, but
that for purposes of this example it should be fine. There are instructions for installing this
files globaly in a shared location, but for purposes of this example we choose to make a 
local copy of the needed files.

From the installed 'bin' directory copy the following dll files into the current directory:

libpango-1.0-0.dll
libpangocairo-1.0-0.dll
libpangoft2-1.0-0.dll
libpangowin32-1.0-0.dll
pango-basic-fc.dll
pango-basic-win32.dll
libcairo-2.dll
libfreetype-6.dll
intl.dll
libglib-2.0-0.dll
libgmodule-2.0-0.dll
libgobject-2.0-0.dll
libxml2.dll
libpng12.dll
libfontconfig-1.dll
zlib1.dll
iconv.dll

From the installed root directory, copy the following directories:

etc\
lib\

Probably not everything inside the 'etc' or 'lib' directories is needed, but being
less than 1 MB it should not matter too much.
