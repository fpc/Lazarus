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

uses Classes, Forms, Buttons, Dialogs, Graphics, StdCtrls;

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
    dirButton : TButton;
    dirLabel : TLabel;
    fileLabel : TLabel;
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
  SetBounds(200, 200, 400, 230);
  Color := clTeal;

  OnDestroy := @FormDestroy;

  dirLabel := TLabel.Create(Self);
  with dirLabel do
  begin
    Parent := Self;
    SetBounds(110, 40, 280, 35);
    Caption := 'Directory';
    Show;
  end;

  fileLabel := TLabel.Create(Self);
  with fileLabel do
  begin
    Parent := Self;
    SetBounds(110, 80, 280, 35);
    Caption := 'File';
    Show;
  end;

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

  dirButton := TButton.Create(Self);
  with dirButton do
  begin
    Parent := Self;
    OnClick := @buttonClick;
    SetBounds(10, 178, 75, 32);
    caption := 'Directory';
    Tag := 6;
    Show;
  end;
end;

procedure TSampleDialogs.FormDestroy(Sender : TObject);
begin

end;

procedure TSampleDialogs.buttonClick(Sender : TObject);
begin

  case TButton(Sender).Tag of
    1 : Close;
    2 : with TOpenDialog.Create(Self) do
        begin
          Filter := '*.pp';
          Options := Options + [ofAllowMultiSelect];
          if Execute then fileLabel.Caption := FileName;
          Free;
        end;
    3 : with TSaveDialog.Create(Self) do
        begin
          Filename := 'untitled.pp';
          if Execute then fileLabel.Caption := FileName;
          Free;
        end;
    4 : with TFontDialog.Create(Self) do
        begin
          Font.Assign(fontButton.Font);
          if Execute then fontButton.Font.Assign(Font);
          Free;
        end;
    5 : with TColorDialog.Create(Self) do
        begin
          Color := Self.Color;
          if Execute then Self.Color := Color;
          Free;
        end;
    6 : with TSelectDirectoryDialog.Create(Self) do
        begin
          if Execute then dirLabel.Caption := FileName;
          Free;
        end;
  end;

end;

end.
