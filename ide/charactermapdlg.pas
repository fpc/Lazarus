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
  SynEdit, StdCtrls, LCLType, GraphType, LazarusIDEStrConsts, EditorOptions,
  SynEditHighlighter, SynHighlighterPosition;

type
  TCharacterMapDialog = class(TForm)
    CharactersGroupbox: TGROUPBOX;
    CharInfoLabel: TLABEL;
    TextSynedit: TSYNEDIT;
    TextGroupbox: TGROUPBOX;
    OkButton: TBUTTON;
    CancelButton: TBUTTON;
    CharMapSynedit: TSYNEDIT;
    procedure CharMapSyneditMOUSEDOWN(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CharMapSyneditMOUSEMOVE(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CharacterMapDialogCREATE(Sender: TObject);
    procedure CharacterMapDialogDESTROY(Sender: TObject);
    procedure CharacterMapDialogKEYDOWN(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FPositionHighlighter: TSynPositionHighlighter;
    FCharacters: string;
    FHighlightAttr: TSynHighlighterAttributes;
    FHighlightAttrID: Integer;
    FTextAttrID: Integer;
    procedure FillCharMap;
    procedure SelectCharacter(Row, Col: integer);
    procedure HighlightCharacter(Row, Col: integer);
    function GetCharacterInfo(Row, Col: integer): string;
    procedure SetCharacters(const AValue: string);
    procedure SetSynEditSettings;
    function RowColValid(Row, Col: integer): boolean;
    function GetCharOrd(Row, Col: integer): integer;
    function RowColToSynEditRowCol(Row, Col: integer): TPoint;
    procedure SynEditXYToRowCol(X, Y: integer; var Row, Col: integer);
  public
    property Characters: string read FCharacters write SetCharacters;
  end;

function ShowCharacterMap(var s: string): boolean;

implementation

function ShowCharacterMap(var s: string): boolean;
var
  CharacterMapDialog: TCharacterMapDialog;
begin
  CharacterMapDialog:=TCharacterMapDialog.Create(nil);
  Result:=CharacterMapDialog.ShowModal=mrOk;
  if Result then
    s:=CharacterMapDialog.Characters;
  CharacterMapDialog.Free;
end;

{ TCharacterMapDialog }

procedure TCharacterMapDialog.CharacterMapDialogKEYDOWN(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then ModalResult:=mrCancel;
end;

procedure TCharacterMapDialog.CharMapSyneditMOUSEMOVE(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row: Integer;
  Col: Integer;
begin
  SynEditXYToRowCol(X,Y,Row,Col);
  HighlightCharacter(Row,Col);
end;

procedure TCharacterMapDialog.FillCharMap;
var
  y: Integer;
  x: Integer;
  CurLine: String;
  CharOrd: Integer;
  CurChar: string;
begin
  CharMapSynedit.Lines.Clear;
  for y:=0 to 13 do begin
    CurLine:='';
    for x:=0 to 15 do begin
      CharOrd:=(y+2)*16+x;
      CurChar:=chr(CharOrd)+' ';
      CurLine:=CurLine+CurChar;
    end;
    CharMapSynedit.Lines.Add(CurLine);
  end;
end;

procedure TCharacterMapDialog.SelectCharacter(Row, Col: integer);
begin

end;

procedure TCharacterMapDialog.HighlightCharacter(Row, Col: integer);
var
  s: String;
  HighlightPos: TPoint;
begin
  s:=GetCharacterInfo(Row,Col);
  //writeln('TCharacterMapDialog.HighlightCharacter Row=',Row,' Col=',Col,' ',s);
  CharInfoLabel.Caption:=s;
  FPositionHighlighter.ClearAllTokens;
  if RowColValid(Row,Col) then begin
    HighlightPos:=RowColToSynEditRowCol(Row,Col);
    FPositionHighlighter.AddToken(HighlightPos.Y-1,HighlightPos.X-1,
                                  FTextAttrID);
    FPositionHighlighter.AddToken(HighlightPos.Y-1,HighlightPos.X+1,
                                  FHighlightAttrID);
  end;
end;

function TCharacterMapDialog.GetCharacterInfo(Row, Col: integer): string;
var
  CharOrd: Integer;
begin
  if RowColValid(Row,Col) then begin
    CharOrd:=GetCharOrd(Row,Col);
    Result:='Dezimal='+IntToStr(CharOrd)+' Hex='+HexStr(CharOrd,2);
  end else begin
    Result:='-';
  end;
end;

procedure TCharacterMapDialog.SetCharacters(const AValue: string);
begin
  if FCharacters=AValue then exit;
  FCharacters:=AValue;
  TextSynedit.Lines.Text:=AValue;
end;

procedure TCharacterMapDialog.SetSynEditSettings;
begin
  EditorOpts.GetSynEditSettings(CharMapSynedit);
  EditorOpts.GetSynEditSettings(TextSynedit);
end;

function TCharacterMapDialog.RowColValid(Row, Col: integer): boolean;
begin
  Result:=(Row>=1) and (Row<=14) and (Col>=1) and (Col<=16);
end;

function TCharacterMapDialog.GetCharOrd(Row, Col: integer): integer;
begin
  Result:=(Row+1)*16+Col-1;
end;

function TCharacterMapDialog.RowColToSynEditRowCol(Row, Col: integer): TPoint;
begin
  Result.X:=Col*2-1;
  Result.Y:=Row;
end;

procedure TCharacterMapDialog.SynEditXYToRowCol(X, Y: integer; var Row,
  Col: integer);
var
  RowColumn: TPoint;
begin
  RowColumn:=CharMapSynedit.PixelsToRowColumn(Point(X,Y));
  Row:=RowColumn.Y;
  Col:=RowColumn.X div 2;
end;

procedure TCharacterMapDialog.CharacterMapDialogCREATE(Sender: TObject);
begin
  Caption:=lisCharacterMap;
  FHighlightAttr:=TSynHighlighterAttributes.Create('Highlight');
  with FHighlightAttr do begin
    Foreground:=clBlue;
    Style:=Style+[fsBold];
  end;
  FPositionHighlighter:=TSynPositionHighlighter.Create(Self);
  FHighlightAttrID:=FPositionHighlighter.GetCopiedTokenID(FHighlightAttr);
  FTextAttrID:=
    FPositionHighlighter.GetCopiedTokenID(FPositionHighlighter.TextAttri);
  CharMapSynedit.Highlighter:=FPositionHighlighter;
  SetSynEditSettings;
  FillCharMap;
  TextSynedit.Lines.Clear;
end;

procedure TCharacterMapDialog.CharMapSyneditMOUSEDOWN(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row: Integer;
  Col: Integer;
begin
  SynEditXYToRowCol(X,Y,Row,Col);
  if not RowColValid(Row,Col) then exit;
  Characters:=Characters+chr(GetCharOrd(Row,Col));
end;

procedure TCharacterMapDialog.CharacterMapDialogDESTROY(Sender: TObject);
begin
  FreeAndNil(FPositionHighlighter);
  FreeAndNil(FHighlightAttr);
end;

initialization
  {$I charactermapdlg.lrs}

end.

