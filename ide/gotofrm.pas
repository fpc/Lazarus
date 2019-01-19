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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit GotoFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Buttons, LazarusIDEStrConsts, LCLType, ButtonPanel;

type

  { TfrmGoto }

  TfrmGoto = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Edit1: TEdit;
    procedure Edit1Change(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: char);
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoShow; override;
  end;

implementation

{$R *.lfm}

{ TfrmGoto }

procedure TfrmGoto.Edit1KeyPress(Sender: TObject; var Key: char);
begin
  if not (UpCase(Key) in [#8,'0'..'9']) then
    Key:=#0;
end;

procedure TfrmGoto.Edit1Change(Sender: TObject);
var
  L: Integer;
begin
  ButtonPanel1.OKButton.Enabled := TryStrToInt(Edit1.Text,L);
end;

constructor TfrmGoto.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Caption := lisGotoLine;
  Label1.Caption := lisUEGotoLine;
  ButtonPanel1.OKButton.Caption:=lisMenuOk;
  ButtonPanel1.CancelButton.Caption:=lisCancel;
  Edit1.Caption := '';
  Edit1.MaxLength := 10;  //enough for MaxLongInt
end;

procedure TfrmGoto.DoShow;
begin
  Edit1.SelectAll;
  Edit1.SetFocus;
  Edit1Change(nil);
  inherited DoShow;
end;

end.

