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
 
{$mode objfpc} 

interface
{ $DEFINE WIN}
{ $DEFINE QT}

{$DEFINE GTK}

{$ifdef WIN}

uses InterfaceBase, Win32Int;

type

   TInterfaceObject = class(TWin32Object);

{$endif}

{$ifdef GTK}

uses InterfaceBase, gtkint;

type

   TInterfaceObject = class(TgtkObject);

{$endif}

{$ifdef QT}

uses InterfaceBase, messages,qtint;

type
   TInterfaceObject = class(TQTObject);
   
{$endif}

var
   InterfaceObject : TInterfaceBase;


implementation


procedure Interface_Init;
begin
   InterfaceObject.Init;
end;

initialization

   InterfaceObject := TInterfaceObject.Create;

finalization

   InterfaceObject.Free;

end.
