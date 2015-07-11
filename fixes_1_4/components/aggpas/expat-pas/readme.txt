================================================================================

 Expat XML Parser - version 2.0.0
 Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
                                and Clark Cooper
 Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.

 Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
 Pascal Port By: Milan Marusinec alias Milano
                 milan@marusinec.sk
                 http://www.pasports.org/pasexpat
                 Copyright (c) 2006

================================================================================
 Introduction
================================================================================

 Expat is an XML parser library written in C. It is a stream-oriented parser 
 in which an application registers handlers for things the parser might find 
 in the XML document (like start tags).
 
 Read more at: http://expat.sourceforge.net

 This version of Expat library is the Object Pascal one and this Readme Note 
 is dealing with issues mainly around the Pascal version.
 
================================================================================
 Compilation matrix
================================================================================ 

 Expat can be currently compiled on the platforms with compilers 
 according to the following matrix:
 
 +----------------------+---------------------+---------------------+
 | Platform / Compiler  |  Free Pascal (FPC)  |   Delphi (Borland)  |
 +======================+=====================+=====================+
 | Windows Win32 API    | versions 2.0 and up | versions 2.0 and up |
 +----------------------+---------------------+---------------------+
 | Linux X11            | versions 2.0 and up | * no support *      |
 +----------------------+---------------------+---------------------+
 | Mac OS X Carbon      | versions 2.0 and up | * no support *      |
 +----------------------+---------------------+---------------------+

================================================================================
 Port comments
================================================================================

 The Object Pascal version (PasExpat) was created for the main reason of having
 a pascal version of the library when porting the SVG demo example in the AggPas
 porting project, which was dependant on Expat for C.

 This port is based on the C version 2.0.0, which was officialy released 
 on 11 January 2006.

 PasExpat port is the pure manual work. There were used no c_to_pascal conversion 
 utilities and the port is the native-one, which means it uses no external dll
 bindings - it's all just the native pascal code. 
 
 This Pascal library is an unfinished version marked as 0.83, which means
 it is approximately 83% of the functionality of the original C library.

 To use the library, just include the "expat" keyword into the uses clause.

 General Pascal port Issues
 ========================== 
 
 Source code comment marks:

 Here and there around the PasExpat source code, following comment marks
 can be found:
 
 {untested} - unit or procedure/function, which was ported but not tested
  
 {not_implemented} - procedure/function, which was not ported, because
                     related features were not implemented in port
 
 {hack} - marks some nonstandard solution
 
 {..} - part of the code, which is unfinished. 

 Compiling mode:

 The "expat_mode.inc" is the main compiler mode configuration file.
 The only one item, you would wish to change is the EXPAT_DEBUG conditional,
 which turns on/off the debug mode compilation settings. With the
 EXPAT_DEBUG not defined the range checking for example is turned off,
 which should lead to building the faster and smallest code.

================================================================================
 What's next
================================================================================
  
 As I said, this version is rather unfinished but sufficient for the AggPas
 SVG demo to work. In the near future, I am not gonna finish it, but if someone
 would need it with full functionality, I suggest to pick up this 0.83 release
 and finish the port.  
 
 
 All for now
 Yours Milano
 
================================================================================
 End of file
================================================================================
