
fpGUI Toolkit
=============

The official fpGUI website is:

  http://fpgui.sourceforge.net


Getting the source code
-----------------------
The Lazarus svn does not contain the fpGUI sources. You can get the
latest stable fpGUI sources via the Git repository as follows:

  git clone git://github.com/graemeg/fpgui.git fpgui-code
  cd fpgui-code
  git checkout maint

...or you can download a zip archive of the latest stable code:

  https://github.com/graemeg/fpGUI/archive/maint.zip


Setup to compile LCL-fpGUI
--------------------------

 Option 1)
   Copy the 'src' directory from the fpGUI sources into your Lazarus
   directory as follows:

     <fpgui>/src/  ==>  <lazarus>/lcl/interfaces/fpgui/src/

 Option 2)
   Use your file systems's symbolic linking function to simply link
   the fpGUI src directory to the correct location in Lazarus.

   For example:
     cd /opt/lazarus/lcl/interfaces/fpgui
     ln -s /path/to/fpgui/src src


For more details see:
http://wiki.freepascal.org/fpGUI_Interface

