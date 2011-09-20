rem Call fpdoc for the given LCL module <%1> . <%2>
rem PATH=\repos\fpc-2.5.trunk\utils\fpdoc
rem del /S /Q test
fpdoc --import=\LazDocs\rtl.xct,../rtl/ --import=\LazDocs\fcl.xct,../fcl/ --import=\LazDocs\lcl.xct,../test/ --package=lcl --output=test --input="..\..\lcl\%1.%2 -Fi..\..\lcl\include" --descr=..\xml\lcl\%1.xml --format=html --css-file=fpdoc.css
