{ /***************************************************************************
                   widgetstack.pp  -  Designer Widget Stack
                             -------------------
                 Implements a widget list created by TDesigner.


                 Initial Revision  : Sat May 10 23:15:32 CST 1999


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
unit designer;

{$mode objfpc}

interface

uses
  classes;

type

 TDesigner = class(TObject)
 public
 constructor Create;override;
 procedure CreateNew(FileName : string);
 procedure LoadFile(FileName: string);
 end;

 implementation

 constructor Create;override;
 begin
 end;

 procedure CreateNew(FileName : string);
 begin
 end;

 procedure LoadFile(FileName: string);
 begin
 end;

end.

