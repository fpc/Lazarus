{ /***************************************************************************
                   designerwidget.pp  -  Designer Widget Stack
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
unit designerwidget;

{$mode objfpc}

interface

uses
   classes, controls;

type

 TDesignerWidget = class(TObject)
 private
  FWidget: TControl;
 public
  procedure AddWidget(wcontrol: TControl);
 end;

implementation

procedure AddWidget(wcontrol: TControl);
begin
end;


end.

