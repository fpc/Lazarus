rem Call makeskel for the given LCL module <%1> . <%2>
rem PATH=\repos\fpc-2.5.trunk\utils\fpdoc
makeskel --package=lcl --input="..\..\lcl\%1.%2 -Fi..\..\lcl\include" --descr=lcl\%1.xml --update --output=lcl\%1.upd.xml

