{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit DlgForm;

{$mode objfpc}
{$H+}

interface

uses Interfaces, Classes, Forms, Buttons, Dialogs, Graphics;

type
  TSampleDialogs = class(TForm)
  private
  protected
  public
    closeButton : TButton;
    openButton : TButton;
    saveButton : TButton;
    colorButton : TButton;
    fontButton : TButton;
    constructor Create(AOwner: TComponent); override;
    procedure buttonClick(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  end;

var
   SampleDialogs : TSampleDialogs;

implementation

constructor TSampleDialogs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Common Dialogs';
  SetBounds(200, 200, 200, 200);
  Color := clTeal;

  OnDestroy := @FormDestroy;

  closeButton := TButton.Create(Self);
  with closeButton do
  begin
    Parent := Self;
    OnClick := @buttonClick;
    SetBounds(110, 145, 75, 32);
    caption := 'Close';
    Tag := 1;
    Show;
  end;
 
  openButton := TButton.Create(Self);
  with openButton do
  begin
    Parent := Self;
    OnClick := @buttonClick;
    SetBounds(10, 10, 75, 32);
    caption := 'Open';
    Tag := 2;
    Show;
  end;

  saveButton := TButton.Create(Self);
  with saveButton do
  begin
    Parent := Self;
    OnClick := @buttonClick;
    SetBounds(10, 52, 75, 32);
    caption := 'Save';
    Tag := 3;
    Show;
  end;

  fontButton := TButton.Create(Self);
  with fontButton do
  begin
    Parent := Self;
    OnClick := @buttonClick;
    SetBounds(10, 94, 75, 32);
    caption := 'Font';
    Tag := 4;
    Show;
  end; 
  
  colorButton := TButton.Create(Self);
  with colorButton do
  begin
    Parent := Self;
    OnClick := @buttonClick;
    SetBounds(10, 136, 75, 32);
    caption := 'Color';
    Tag := 5;
    Show;
  end;
end;

procedure TSampleDialogs.FormDestroy(Sender : TObject);
begin
   Application.Terminate;
end;

procedure TSampleDialogs.buttonClick(Sender : TObject);
begin

  case TButton(Sender).Tag of
    1 : Application.Terminate;
    2 : with TOpenDialog.Create(Self) do
        begin
          Filter := '*.pp';
          Execute;
          Free;
        end;
    3 : with TSaveDialog.Create(Self) do
        begin
          Filename := 'untitled.pp';
          Execute;
          Free;
        end;
    4 : with TFontDialog.Create(Self) do
        begin
          Execute;
          Free;
        end;
    5 : with TColorDialog.Create(Self) do
        begin
          if Execute then Self.Color := Color;
          Free;
        end;
  end;

end;

end.
