{
 /***************************************************************************
                               AbstractEditor.pp
                             -------------------




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
unit AbstractEditor;

{$mode objfpc}

interface

uses
  classes, Controls, forms,buttons,sysutils, Graphics,Extctrls;

type

  TAbstractEditor = class
   public
     Function BufferModified : Boolean; virtual; abstract;
     Function LinesInBuffer : Longint; virtual; abstract;
     Function Filename : String; virtual; abstract;
  end;


implementation

end.
