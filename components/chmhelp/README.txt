lhelp is a program written entirely using FreePascal and the LCL to read .chm help files.

This is a basic HOWTO for integrating lhelp into the Lazarus IDE.


1 ) Start Lazarus

2 ) Install Packages:

    In the Components Menu choose "Open Package File"
    Browse to the lazarus/components/chmhelp/packages/help/ directory and 
    open "lhelpcontrolpkg.lpk"

    You do not need to directly install this package.

    With the lhelpcontrolpkg package still open, in the Components Menu choose 
   "Open Package File". Browse to the lazarus/components/chmhelp/packages/idehelp 
    directory and open chmhelppkg.lpk

3 ) Now click "Install" this will automatically install the first package we opened.

4 ) Restart Lazarus(if it didn't automatically)

5 ) Open the lhelp project in lazarus/components/chmhelp/lhelp/lhelp.lpi
    Compile lhelp.

5 ) Configure the paths for the lhelp:

    From the Help menu choose "Configure Help"
    Change to the "Viewers" tab and select "CHM Help Viewer"

    HelpEXE:
    For the "HelpEXE" entry browse to the lazarus/components/chmhelp/lhelp/ folder 
    and select the lhelp executable.

    HelpFilesPath:
    This is the directory that contains the lcl.chm fcl.chm and rtl.chm files.

    HelpLabel Name and Tag do not need to be altered.
    The HelpLabel is the name of the named pipe that lazarus will use to comunicate with lhelp.

6 ) Configure the DataBases

    Choose the DataBases tab.

    RTLUnits:
    this should be "rtl.chm://"
    FCLUnits:
    this should be "fcl.chm://"
    LCLUnits:
    this should be "lcl.chm://"

    NOTE if you have only a single lcl-fcl-rtl.chm file then then paths become:
    "lcl-fcl-rtl.chm://rtl/"
    "lcl-fcl-rtl.chm://fcl/"
    "lcl-fcl-rtl.chm://lcl/"

Now close this window and check out the integrated help :)

Enjoy

