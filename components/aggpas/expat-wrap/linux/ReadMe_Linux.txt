	         How to build Expat under Linux
                     (tested on RedHat 7.3)

1) After having extracted expat-1.95.6.tar to the expat directory,
   or checked the 1.95.6 release out of the CVS repository, run
   the configure script "./buildconf.sh" from the build directory.
   This will need autoconf 2.52 or later and libtool 1.4 installed,
   which are not part of the standard installation of RedHat 7.3!

2a) Build for UTF8 output:
   From the build directory) run "./configure",  followed by "make",
   then change to root (su root) and run "make install".

2b) Build for UTF16 output:
   - maybe run "make clean" first
   - for 16 bit wchar_t output (incl. version and message strings),
     run
       ./configure CFLAGS="-g -O2 -fshort-wchar" CPPFLAGS=-DXML_UNICODE_WCHAR_T
     or for unsigned short output (but version and message strings as 8bit chars),
     run
       ./configure CPPFLAGS=-DXML_UNICODE
   - Then edit the MakeFile: change "LIBRARY = libexpat.la" to
     "LIBRARY = libexpatw.la"
   - Finally, run "make buildlib" and "make installlib"
    (the latter with root privileges)

Notes: 
   - The libraries are installed as: "libexpat.so" (UTF8) and "libexpatw.so" (UTF16),
     which are usually symlinks pointing to the active version of Expat, which
     in this case would be "libexpat.so.0.4.0" and "libexpatw.so.0.4.0".
   - don't forget to run /sbin/ldconfig to configure the dynamic linker
   - one can combine two of the steps by running "make buildlib LIBRARY=libexpatw.la"

