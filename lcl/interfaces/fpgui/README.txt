The Lazarus svn does not contain the fpGUI sources.
You can get the fpGUI sources with

Old versions:
-------------
svn co https://fpgui.svn.sourceforge.net/svnroot/fpgui/trunk fpgui

Recent versions (post may 2010):
--------------------------------
Using GIT: git clone git://fpgui.git.sourceforge.net/gitroot/fpgui/fpgui
Web access: http://sourceforge.net/projects/fpgui/files/

Copy or link directories
<fpGUI dir>/src/gui ⇒ lazarus/lcl/interfaces/fpgui/gui
<fpGUI dir>/src/corelib ⇒ lazarus/lcl/interfaces/fpgui/corelib

For more details see:
http://wiki.lazarus.freepascal.org/fpGUI_Interface

Current code tested using fpgui 0.7.rc2
