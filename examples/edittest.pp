{ $Header$
 /***************************************************************************
                               EditTest.pp
                             -------------------
                           Test aplication for editors
                   Initial Revision  : Sun Dec 31 17:30:00:00 CET 2000




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
{
@author(Marc Weustink <marc@lazarus.dommelstein.net>)
@created(31-Dec-2000)

Detailed description of the Unit.
}
program edittest;

{$mode objfpc}

uses
  buttons, classes, forms, controls, sysutils, Graphics,  mwcustomedit, mwPasSyn;

type
  TEditTestForm = class(TForm)
  public
    FEdit: TmwCustomEdit;
    FHighlighter: TmwPasSyn;
    constructor Create(AOwner: TComponent); override;
  end;

var
  EditTestForm: TEditTestForm;

{------------------------------------------------------------------------------}
{  TEditTestorm                                          }
{------------------------------------------------------------------------------}
constructor TEditTestForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  Height := 200;
  Left := 200;
  Top := 200;
  Caption := 'Editor tester';

  if FHighlighter = nil
  then begin
    FHighlighter := TmwPasSyn.Create(Self);
    FHighlighter.CommentAttri.Foreground := clNavy;
    FHighlighter.NumberAttri.Foreground := clRed;
    FHighlighter.KeyAttri.Foreground := clGreen;
  end;

  FEdit := TmwCustomEdit.Create(Self);
  with FEdit
  do begin
    Parent := Self;
		Width := 300;
		Height := 200;
    {$IFDEF NEW_EDITOR}
    Gutter.Color := clBtnface;
    Gutter.ShowLineNumbers := True;
   {$ELSE}
    GutterColor := clBtnface;
    {$ENDIF}
    Color := clWindow;
    Visible := True;
    Font.Name := 'courier';
    Font.Size := 12;
    HighLighter := Self.FHighLighter;
  end;
end;

begin
   Application.Initialize;
   Application.CreateForm(TEditTestForm, EditTestForm);
   Application.Run;
end.

{
  $Log$
  Revision 1.1  2000/12/31 15:48:41  lazarus
  MWE:
    + Added Editor test app.

}

