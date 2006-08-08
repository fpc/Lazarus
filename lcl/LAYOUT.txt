Lazarus Component Library

It is organized according to the following structure:

lcl
 |
 +--- include
 |
 +--- interfaces
 |
 +--- templates
 |
 +--- units
 |
 +--- widgetset
 |
 +--- tests


lcl: 
  All definitions (= interfaces) of the LCL components 
  goes here.
  
lcl/include:
  The actual implementation of the components.
 
lcl/interfaces:
  The code to interface between the LCL and the graphic
  platform goes here.
  
lcl/templates:
  Some templates for interface, implementation 
  definitions
  
lcl/units:
  Contains the compiled units.

lcl/widgetset
  Contains the definition of the widgetset class 
  and the sceleton WSxxxComponent classes
  
lcl/tests
  Contains various tests for the LCL
