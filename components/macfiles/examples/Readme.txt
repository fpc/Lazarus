The createmacapplication.lpi is more than an example.
It is a tool to create the hidden directories, needed by graphical applications
on MacOSX.
When you start a native carbon application in Lazarus, it creates the sources
and .lpi files, and compiling will give you the executable.

Example:
Open the createmacapplication.lpi project and compile it.

cd /your/project/
/path/to/lazarus/components/macfiles/examples/createmacapplication projectname
ln -s ../../../projectname ./projectname.app/Contents/MacOS/projectname


