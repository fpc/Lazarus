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
unit widgetstack;

{$mode objfpc}

interface

uses
   classes, designerwidget;

type

 TWidgetStack = class(TObject)
 public
 procedure AddControl(widget:TDesignerWidget);
 function GetControlByName(const wname: string):TDesignerWidget;
 function GetControlByIndex(const windex: integer):TDesignerWidget;
 function GetControlCount:integer;
 end;

 implementation

 procedure AddControl(widget:TDesignerWidget);
 begin
 end;

 function GetControlByName(const wname: string):TDesignerWidget;
 begin
 end;

 function GetControlByIndex(const windex: integer):TDesignerWidget;
 begin
 end;

 function GetControlCount:integer;
 begin
 end;


 end.

