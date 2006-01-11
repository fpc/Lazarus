{
/***************************************************************************
                             charactermapdlg.pas
                             -------------------

 ***************************************************************************/

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

  Author: Mattias Gaertner
  
  Abstract:
    Dialog for character map.
}
unit CharacterMapDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LCLType, GraphType, LazarusIDEStrConsts, EditorOptions,
  EnvironmentOpts, Grids;

type
  TOnInsertCharacterEvent = procedure (const C: TUTF8Char) of object;

  { TCharacterMapDialog }

  TCharacterMapDialog = class(TForm)
    CharactersGroupbox: TGroupbox;
    CharInfoLabel: TLabel;
    CloseButton: TButton;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FOnInsertCharacter: TOnInsertCharacterEvent;
    procedure FillCharMap;
  public
    property OnInsertCharacter: TOnInsertCharacterEvent read FOnInsertCharacter
                                                        write FOnInsertCharacter;
  end;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);

var
  CharacterMapDialog: TCharacterMapDialog;

implementation

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);
begin
  if CharacterMapDialog = nil then
    Application.CreateForm(TCharacterMapDialog, CharacterMapDialog);
    
  CharacterMapDialog.OnInsertCharacter := AOnInsertChar;
  CharacterMapDialog.Show;
end;

{ TCharacterMapDialog }

procedure TCharacterMapDialog.FormCreate(Sender: TObject);
begin
  Caption := lisCharacterMap;
  CharactersGroupbox.Caption := lisCharacterMap;
  CloseButton.Caption := lisClose;

  //EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
  FillCharMap;
end;

procedure TCharacterMapDialog.FormShow(Sender: TObject);
begin
  StringGrid1.Font.Name := EditorOpts.EditorFont;
  StringGrid1.Font.Size := 10;

  StringGrid1.AutoSizeColumns;
end;

procedure TCharacterMapDialog.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TCharacterMapDialog.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  if (Button = mbLeft) and (StringGrid1.MouseToGridZone(X, Y) = gzNormal) then
  begin
    StringGrid1.MouseToCell(X, Y, Col, Row);
    if Assigned(OnInsertCharacter) then
      OnInsertCharacter(StringGrid1.Cells[Col, Row][1]);
  end;
end;

procedure TCharacterMapDialog.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  CharOrd: Byte;
  Row, Col: Integer;
begin
  if StringGrid1.MouseToGridZone(X, Y) = gzNormal then
  begin
    StringGrid1.MouseToCell(X, Y, Col, Row);
    
    CharOrd := Ord(StringGrid1.Cells[Col, Row][1]);
    CharInfoLabel.Caption := 'Decimal = ' + IntToStr(CharOrd) +
                             ', Hex = $'  + HexStr(CharOrd, 2);
  end
  else
  begin
    CharInfoLabel.Caption := '-';
  end;
end;

procedure TCharacterMapDialog.FillCharMap;
var
  R, C: Integer;
begin
  for R := 0 to Pred(StringGrid1.RowCount) do
  begin
    if R <> 0 then  StringGrid1.Cells[0, R] := Format('%.3d +', [Succ(R) * 16]);
    for C := 1 to Pred(StringGrid1.ColCount) do
    begin
      if R = 0 then StringGrid1.Cells[C, R] := Format('%.2d', [Pred(C)])
      else
        StringGrid1.Cells[C, R] := Chr(Succ(R) * 16 + Pred(C));
    end;
  end;
end;


initialization
  {$I charactermapdlg.lrs}

end.

