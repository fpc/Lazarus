{
 /***************************************************************************
                               FormEditor.pp
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
{$H+}

unit FormEditor;

{$mode objfpc}

interface

uses
  classes, customformeditor,Controls, forms,buttons,sysutils,Graphics,Extctrls;

type

  TFormEditor = class(TCustomFormEditor)
  private

  public
    constructor Create; override;
    destructor destroy; override;
  end;


implementation
uses
  LCLLinux;

constructor TFormEditor.Create;
Begin
inherited;
end;

destructor TFormEditor.destroy;
Begin
inherited;
end;

end.
