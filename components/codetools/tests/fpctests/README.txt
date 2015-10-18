This directory contains tests from the FPC test/src directory, extended with
marks for the ../finddeclarationtest.lpi.

Testing all these files:
./finddeclarationtest --format=plain --suite=TestFindDeclaration_FPCTests

Testing one file:
./finddeclarationtest --format=plain --suite=TestFindDeclaration_FPCTests --filemask=tchlp41.pp

Testing a bunch of files:
./finddeclarationtest --format=plain --suite=TestFindDeclaration_FPCTests --filemask=tchlp*.pp

