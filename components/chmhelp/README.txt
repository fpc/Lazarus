lhelp is a program written entirely using FreePascal and the LCL to read .chm help files.

This is a basic HOWTO for integrating lhelp into the Lazarus IDE.


1 ) Start Lazarus

2 ) Install Package:

    In the Components Menu choose "Open Package File"
    Browse to the lazarus/components/chmhelp/packages/idehelp directory and
    open "chmhelppkg.lpk"

3 ) Now click "Install".

4 ) Restart Lazarus(if it didn't automatically)

5 ) Open the lhelp project in lazarus/components/chmhelp/lhelp/lhelp.lpi
    Compile lhelp.

6 ) Configure the paths for the lhelp:

    From the Tools menu choose "Options"
    Change to Help / Help options.
    Change to the "Viewers" tab and select "CHM Help Viewer"

    HelpEXE:
    Leave it empty to use the default: $(LazarusDir)/components/chmhelp/lhelp/lhelp(.exe)

    HelpFilesPath:
    This is the search path where to search for chm files.
    Default is $(LazarusDir)/docs/html;$(LazarusDir)/docs/html/lcl;$(LazarusDir)/docs/chm

    You can download the lcl.chm, fcl.chm, rtl.chm from
    http://sourceforge.net/projects/freepascal/files/Documentation/
    the lcl.chm, rtl.chm, fcl.chm, prog.chm from
    http://www.stack.nl/~marcov/doc-chm.zip

    HelpLabel Name and Tag do not need to be altered.
    The HelpLabel is the name of the named pipe that lazarus will use to communicate with lhelp.

7 ) Configure the Databases

    Choose the Databases tab.

    RTLUnits:
    Leave it empty to use the default: "rtl.chm://"
    FCLUnits:
    Leave it empty to use the default: "fcl.chm://"

    NOTE if you have only a single lcl-fcl-rtl.chm file then paths become:
    "lcl-fcl-rtl.chm://rtl/"
    "lcl-fcl-rtl.chm://fcl/"
    "lcl-fcl-rtl.chm://lcl/"

Now close this window and check out the integrated help :)

Enjoy

