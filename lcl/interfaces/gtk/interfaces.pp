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


unit interfaces;
 
{$mode objfpc}{$H+} 

interface

uses 
   InterfaceBase,Clipbrd,SysUtils;

var
   InterfaceObject : TInterfaceBase;

implementation

uses 
   GTKInt,
   Forms;  // MG: GTKInt uses forms, so the application object is destroyed
           //   in the forms finalization section AFTER this finalization
           //   section. But the lcl objects need the gtk to close clean.
           //   Therefore the application object is freed here before the
           //   InterfaceObject.
           //   Probably this is the case for all interfaces, so this should be
           //   moved to forms.pp.

initialization

  InterfaceObject := TgtkObject.Create;

finalization

  FreeandNil(Application);
  FreeAllClipBoards;
  FreeAndNil(InterfaceObject);

end.
