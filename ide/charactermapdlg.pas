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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, LCLType, LCLUnicodeData, GraphType, Grids, ButtonPanel, ComCtrls,
  IDEHelpIntf, LazUTF8,
  {$ifdef WINDOWS}Windows,{$endif} lconvencoding,
  LazarusIDEStrConsts, EditorOptions, EnvironmentOpts;

type
  TOnInsertCharacterEvent = procedure (const C: TUTF8Char) of object;

  { TCharacterMapDialog }

  TCharacterMapDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    cbCodePage: TComboBox;
    CharInfoLabel: TLabel;
    cbUniRange: TComboBox;
    Label1: TLabel;
    RangeLabel: TLabel;
    UnicodeCharInfoLabel: TLabel;
    PageControl1: TPageControl;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    pgAnsi: TTabSheet;
    pgUnicode: TTabSheet;
    procedure cbCodePageSelect(Sender: TObject);
    procedure cbUniRangeSelect(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
    procedure StringGrid2SelectCell(Sender: TObject; aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
    procedure StringGridKeyPress(Sender: TObject; var Key: char);
    procedure StringGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid2MouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
  private
    FOnInsertCharacter: TOnInsertCharacterEvent;
    procedure DoStatusGrid1(ACol, ARow: integer);
    procedure DoStatusGrid2(ACol, ARow: integer);
    procedure FillCharMap;
    procedure SelectSystemCP;
  public
    property OnInsertCharacter: TOnInsertCharacterEvent read FOnInsertCharacter
                                                        write FOnInsertCharacter;
  end;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);

var
  CharacterMapDialog: TCharacterMapDialog;

implementation

{$R *.lfm}

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
  RangeLabel.Caption := lisRange;
  ButtonPanel.HelpButton.Caption:=lisMenuHelp;
  ButtonPanel.CloseButton.Caption:=lisBtnClose;

  //EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
  PageControl1.ActivePageIndex := 0;
  CharInfoLabel.Caption := '-';
  UnicodeCharInfoLabel.Caption := '-';
  SelectSystemCP;
  FillCharMap;
end;

procedure TCharacterMapDialog.SelectSystemCP;
{$ifdef Windows}
var
  i: Integer;
  cp: Word;
  cpStr: String;
{$endif}
begin
 {$ifdef Windows}
  // Find system code page on Windows...
  // see: msdn.microsoft.com/library/windows/desktop/dd317756%28v=vs.85%29.aspx
  cp := Windows.GetACP;
  case cp of  // add spaces to be sure of unique names found in the combobox
    437..1258: cpStr := 'cp' + IntToStr(cp) + ' ';
    10000    : cpStr := 'macintosh ';
    20866    : cpStr := 'koi8 ';
    28591    : cpStr := 'iso88591 ';
    28592    : cpStr := 'iso88592 ';
    28605    : cpStr := 'iso885915 ';
    else       cpStr := '';
  end;
  for i := 0 to cbCodePage.Items.Count-1 do
    if pos(cpStr, cbCodePage.Items[i]) = 1 then
    begin
      cbCodePage.ItemIndex := i;
      exit;
    end;
 {$endif}
  // ... if not found, or non-Windows, just pick the first item.
  cbCodePage.ItemIndex := 0;
end;

procedure TCharacterMapDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

function RoundUp(Value, Divi:integer):integer;
begin
  if Value mod Divi = 0 then
    Result:=Value div Divi
  else
    Result:=(Value div Divi)+1;
end;

procedure TCharacterMapDialog.cbCodePageSelect(Sender: TObject);
begin
  FillCharMap;
end;

procedure TCharacterMapDialog.cbUniRangeSelect(Sender: TObject);
var
  cnt, x, y: integer;
  S, E: integer;
begin
  S:=UnicodeBlocks[cbUniRange.ItemIndex].S;
  E:=UnicodeBlocks[cbUniRange.ItemIndex].E;
  StringGrid2.Clear;
  StringGrid2.ColCount:=16;
  StringGrid2.RowCount:=RoundUp(E-S,16);
  cnt:=0;
  for y:=0 to StringGrid2.RowCount-1 do
    for x:=0 to StringGrid2.ColCount-1 do
    begin
      if S+Cnt<=E then
        StringGrid2.Cells[x,y]:=UnicodeToUTF8(S+Cnt);
      inc(cnt);
    end;
  StringGrid2.AutoSizeColumns;
end;

procedure TCharacterMapDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
  begin
    Close;
    Key:= 0;
  end;
end;

procedure TCharacterMapDialog.FormShow(Sender: TObject);
var
  i:integer;
begin
  StringGrid1.Font.Name := EditorOpts.EditorFont;
  StringGrid2.Font.Name := EditorOpts.EditorFont;
  StringGrid1.Font.Size := 10;
  StringGrid2.Font.Size := 10;

  StringGrid1.AutoSizeColumns;

  cbUniRange.Items.Clear;
  for i:=0 to MaxUnicodeBlocks do
    cbUniRange.Items.Add(UnicodeBlocks[i].PG);
  cbUniRange.ItemIndex:=0;
  cbUniRangeSelect(nil);
end;

procedure TCharacterMapDialog.StringGrid1SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  DoStatusGrid1(aCol, aRow);
end;

procedure TCharacterMapDialog.StringGrid2SelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  DoStatusGrid2(aCol, aRow);
end;

procedure TCharacterMapDialog.StringGridKeyPress(Sender: TObject; var Key: char);
var
  sg: TStringGrid;
  s: string;
begin
  if Key = #13 then
  begin
    sg := Sender as TStringGrid;
    s := sg.Cells[sg.Col, sg.Row];
    if (s <> '') and (Assigned(OnInsertCharacter)) then
      OnInsertCharacter(s);
  end;
end;

procedure TCharacterMapDialog.StringGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
  sg: TStringGrid;
begin
  sg := Sender as TStringGrid;
  if (Button = mbLeft) and (sg.MouseToGridZone(X, Y) = gzNormal) then
  begin
    Col:=0; Row:=0;
    sg.MouseToCell(X, Y, Col, Row);
    if (sg.Cells[Col, Row] <> '') and (Assigned(OnInsertCharacter)) then
      OnInsertCharacter(sg.Cells[Col, Row]);
  end;
end;

procedure TCharacterMapDialog.DoStatusGrid1(ACol, ARow: integer);
var
  N: integer;
begin
  N := ACol-1 + (ARow-1)*16 + 32;
  CharInfoLabel.Caption := Format('Decimal: %s, Hex: $%s', [IntToStr(N), IntToHex(N, 2)]);
end;

procedure TCharacterMapDialog.StringGrid1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  if StringGrid1.MouseToGridZone(X, Y) = gzNormal then
  begin
    Col:=0; Row:=0;
    StringGrid1.MouseToCell(X, Y, Col, Row);
    DoStatusGrid1(Col, Row);
  end
  else
    CharInfoLabel.Caption := '-';
end;

procedure TCharacterMapDialog.DoStatusGrid2(ACol, ARow: integer);
var
  S: Cardinal;
  tmp, tmp2: String;
  i: Integer;
begin
  if cbUniRange.ItemIndex<0 then exit;
  S:=UnicodeBlocks[cbUniRange.ItemIndex].S+(ACol)+(ARow*16);
  tmp:=UnicodeToUTF8(S);
  tmp2:='';
  for i:=1 to Length(tmp) do
    tmp2:=tmp2+'$'+IntToHex(Ord(tmp[i]),2);
  UnicodeCharInfoLabel.Caption:='U+'+inttohex(S,4)+', UTF-8: '+tmp2;
end;

procedure TCharacterMapDialog.StringGrid2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  if StringGrid2.MouseToGridZone(X, Y) = gzNormal then
  begin
    Col:=0; Row:=0;
    StringGrid2.MouseToCell(X, Y, Col, Row);
    DoStatusGrid2(Col, Row);
  end
  else
    CharInfoLabel.Caption := '-';
end;

procedure TCharacterMapDialog.FillCharMap;
var
  R, C, p: Integer;
  cp: String;
begin
  cp := cbCodePage.Items[cbCodePage.ItemIndex];
  p := pos(' ', cp);
  if p > 0 then SetLength(cp, p-1);
  for R := 0 to Pred(StringGrid1.RowCount) do
  begin
    if R <> 0 then  StringGrid1.Cells[0, R] := Format('%.3d +', [Succ(R) * 16]);
    for C := 1 to Pred(StringGrid1.ColCount) do
    begin
      if R = 0 then StringGrid1.Cells[C, R] := Format('%.2d', [Pred(C)])
      else
        StringGrid1.Cells[C, R] := ConvertEncoding(Chr(Succ(R) * 16 + Pred(C)), cp, 'utf8');
    end;
  end;
end;

end.

