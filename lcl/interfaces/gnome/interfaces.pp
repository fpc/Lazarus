{ 
 /*************************************************************************** 
              Interfaces.pp  -  determines what interface to use
              --------------------------------------------------
 
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
unit Interfaces;
 
{$mode objfpc}{$H+} 

interface

uses 
  InterfaceBase;

var
  InterfaceObject : TInterfaceBase;

implementation

uses 
  GnomeInt, Clipbrd, SysUtils,
  Forms;  // MG: GnomeInt uses forms, so the application object is destroyed
          //   in the forms finalization section AFTER this finalization
          //   section. But the lcl objects need the gtk to close cleanly.
          //   Therefore the application object is freed here before the
          //   InterfaceObject.
          //   Probably this is the case for all interfaces, so this should be
          //   moved to forms.pp.

initialization
  InterfaceObject := TGnomeObject.Create;

finalization
  Application.Free;
  Application:=nil;
  FreeAllClipBoards;
  InterfaceObject.Free;
  InterfaceObject:=nil;

end.
