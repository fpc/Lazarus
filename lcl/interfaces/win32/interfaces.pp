{ 
 /*************************************************************************** 
                         Interfaces.pp  -  determines what interface to use
                             ------------------- 

                   Initial Revision  : Thu July 1st CST 1999 


 ***************************************************************************/ 

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
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
InterfaceObject := Nil;

End.
