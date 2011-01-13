The Lazarus svn does not contain the fpGUI sources.
You can get the fpGUI sources with

git (State may 2010):
--------------------------------
Using GIT: git clone git://fpgui.git.sourceforge.net/gitroot/fpgui/fpgui
Web access: http://sourceforge.net/projects/fpgui/files/

Copy or link directories
<fpGUI dir>/src/gui ⇒ lazarus/lcl/interfaces/fpgui/gui
<fpGUI dir>/src/corelib ⇒ lazarus/lcl/interfaces/fpgui/corelib
<fpGUI dir>/src/VERSION_FILE.inc ⇒ lazarus/lcl/interfaces/fpgui/VERSION_FILE.inc

For example via symlinks:
cd lazarus/lcl/interfaces/fpgui
export FPGUI_DIR=/path/to/fpgui
rm -f corelib gui VERSION_FILE.inc
ln -s $FPGUI_DIR/src/gui .
ln -s $FPGUI_DIR/src/corelib .
ln -s $FPGUI_DIR/src/VERSION_FILE.inc .

For more details see:
http://wiki.lazarus.freepascal.org/fpGUI_Interface

Current code tested using fpgui 0.7.rc2
