This package (demopackagewithhelp.lpk) demonstrates how to register help in the
IDE for the sources of a package.

The help itself is in fpdoc format.
Here is an example how to create the basic fpdoc xml file from the source:

makeskel --package=demopackagewithhelp --input=pkghelpdemounit1.pas --output=xml/pkghelpdemounit1.xml

Use the program LazDE (lazarus/doceditor/lazde.lpi)
or LazDoc tool in the IDE to write some documentation.

Then create the html files from the xml files:

cd html && fpdoc --package=demopackagewithhelp --descr=../xml/pkghelpdemounit1.xml --input=../pkghelpdemounit1.pas


