{ 
 /*************************************************************************** 
                         Interfaces.pp  -  determines what interface to use
                             ------------------- 

                   Initial Revision  : Thu July 1st CST 1999 


 ***************************************************************************/ 

/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
 }

Unit interfaces;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

Interface 

Uses
  InterfaceBase;

Var
  InterfaceObject: TInterfaceBase;

Implementation 

Uses
  Win32Int, Forms;

Initialization

InterfaceObject := TWin32Object.Create;

Finalization

Application.Free;
Application := Nil;

InterfaceObject.Free;

End.
