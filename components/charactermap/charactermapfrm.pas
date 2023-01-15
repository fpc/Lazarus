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

unit CharacterMapFrm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef WINDOWS}Windows,{$endif}
  Classes, SysUtils, Math,
  // LCL
  Controls, Graphics, Dialogs, Buttons, StdCtrls, Forms,
  LCLType, LCLUnicodeData, Grids, ButtonPanel, ComCtrls, Spin, ImgList,
  // LazUtils
  GraphType, LazUTF8, LConvEncoding;

type
  TOnInsertCharacterEvent = procedure (const C: TUTF8Char) of object;

  { TCharacterMapForm }

  TCharacterMapForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cbCodePage: TComboBox;
    AnsiCharInfoLabel: TLabel;
    cbUniRange: TComboBox;
    AnsiSizeLabel: TLabel;
    UniSizeLabel: TLabel;
    seUniSize: TSpinEdit;
    SortUniRangeListButton: TSpeedButton;
    CodePageLabel: TLabel;
    RangeLabel: TLabel;
    seAnsiSize: TSpinEdit;
    UnicodeCharInfoLabel: TLabel;
    PageControl1: TPageControl;
    AnsiGrid: TStringGrid;
    UnicodeGrid: TStringGrid;
    pgAnsi: TTabSheet;
    pgUnicode: TTabSheet;
    procedure GridPrepareCanvas(sender: TObject; {%H-}aCol, {%H-}aRow: Integer;
      {%H-}aState: TGridDrawState);
    procedure cbCodePageSelect(Sender: TObject);
    procedure cbUniRangeSelect(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seAnsiSizeChange(Sender: TObject);
    procedure seUniSizeChange(Sender: TObject);
    procedure SortUniRangeListButtonClick(Sender: TObject);
    procedure AnsiGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
    procedure UnicodeGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var {%H-}CanSelect: Boolean);
    procedure StringGridKeyPress(Sender: TObject; var Key: char);
    procedure StringGridMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure AnsiGridMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure UnicodeGridMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
  private
    FOnShowHelp: TNotifyEvent;
    FOnInsertCharacter: TOnInsertCharacterEvent;
    FUnicodeBlockIndex: Integer;
    procedure DoStatusAnsiGrid(ACol, ARow: integer);
    procedure DoStatusUnicodeGrid(ACol, ARow: integer);
    procedure FillAnsiGrid;
    procedure FillUnicodeGrid;
    procedure FillUniRangeList(ASorted: Boolean);
    function UnicodeBlockIndexByName(AName: String): Integer;
    function UnicodeBlockSelected: Boolean;
    procedure SelectSystemCP;
    procedure SetOnShowHelp(AValue: TNotifyEvent);
  public
    property OnInsertCharacter: TOnInsertCharacterEvent
      read FOnInsertCharacter write FOnInsertCharacter;
    property OnShowHelp: TNotifyEvent
      read FOnShowHelp write SetOnShowHelp;
  end;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);

var
  CharacterMapForm: TCharacterMapForm;

resourcestring
  lisCharacterMap = 'Character Map';
  lisRange = 'Range';
  lisSortUnicodeRangeListAlphabetically = 'Sort Unicode range list alphabetically';
  lisInsertCharacter = 'Insert from Character Map...';
  lisCharSize = 'Character Size';
  lisCodePage = 'Code Page';

implementation

{$R *.lfm}
{$R charactermap_images.res}

uses
  LCLStrConsts;

const
  NOT_SELECTED=Low(UnicodeBlocks)-1;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);
begin
  if CharacterMapForm = nil then
    Application.CreateForm(TCharacterMapForm, CharacterMapForm);
    
  CharacterMapForm.OnInsertCharacter := AOnInsertChar;
  CharacterMapForm.Show;
end;

{ TCharacterMapForm }

procedure TCharacterMapForm.FormCreate(Sender: TObject);
begin
  Caption := lisCharacterMap;
  RangeLabel.Caption := lisRange;
  UniSizeLabel.Caption := lisCharSize;
  CodePageLabel.Caption := lisCodePage;
  AnsiSizeLabel.Caption := lisCharSize;
  SortUniRangeListButton.Flat := True;
  SortUniRangeListButton.Hint := lisSortUnicodeRangeListAlphabetically;
  SortUniRangeListButton.Images := LCLGlyphs;
  SortUniRangeListButton.ImageIndex := LCLGlyphs.GetImageIndex('charmap_sortalphabetically');
  ButtonPanel.HelpButton.Caption := rsMbHelp;
  ButtonPanel.CloseButton.Caption := rsMbClose;
  ButtonPanel.ShowButtons := [pbClose];

  //EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
  PageControl1.ActivePageIndex := 0;
  AnsiCharInfoLabel.Caption := '-';
  UnicodeCharInfoLabel.Caption := '-';
  SelectSystemCP;
  FillAnsiGrid;
end;

procedure TCharacterMapForm.SelectSystemCP;
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
    20866    : cpStr := 'koi8r ';
    21866    : cpStr := 'koi8u ';
    28591    : cpStr := 'iso88591 ';
    28592    : cpStr := 'iso88592 ';
    28593    : cpStr := 'iso88593 ';
    28594    : cpStr := 'iso88594 ';
    28595    : cpStr := 'iso88595 ';
    28597    : cpStr := 'iso88597 ';
    28599    : cpStr := 'iso88599 ';
    28600    : cpStr := 'iso885910 ';
    28603    : cpStr := 'iso885913 ';
    28604    : cpStr := 'iso885914 ';
    28605    : cpStr := 'iso885915 ';
    28606    : cpStr := 'iso885916 ';
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

procedure TCharacterMapForm.HelpButtonClick(Sender: TObject);
begin
  //LazarusHelp.ShowHelpForIDEControl(Self);
end;

function RoundUp(Value, Divi:integer):integer;
begin
  if Value mod Divi = 0 then
    Result:=Value div Divi
  else
    Result:=(Value div Divi)+1;
end;

procedure TCharacterMapForm.cbCodePageSelect(Sender: TObject);
begin
  FillAnsiGrid;
end;

procedure TCharacterMapForm.cbUniRangeSelect(Sender: TObject);
begin
  FUnicodeBlockIndex:=UnicodeBlockIndexByName(cbUniRange.Text);
  FillUnicodeGrid;
end;

procedure TCharacterMapForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key=VK_ESCAPE then
  begin
    Close;
    Key:= 0;
  end;
end;

procedure TCharacterMapForm.FormShow(Sender: TObject);
var
  savedFontSize: Integer;
begin
  (*          wp
  AnsiGrid.Font.Name := EditorOpts.EditorFont;
  UnicodeGrid.Font.Name := EditorOpts.EditorFont;
  AnsiGrid.Font.Size := seAnsiSize.Value;
  UnicodeGrid.Font.Size := seUniSize.Value;
    *)

  // Auto-adjust the width of the AnsiGrid's fixed column. Note that
  // the font defined in PrepareCanvas is ignored by AutoSizeColumn.
  savedfontSize := AnsiGrid.Font.Size;
  AnsiGrid.Font.Size := 10;
  AnsiGrid.AutoSizeColumn(0);
  AnsiGrid.Font.Size := savedFontSize;
  // Now also auto-adjust the widths of the other AnsiGrid to fill the client area.
  AnsiGrid.AutoFillColumns := true;

  FUnicodeBlockIndex:=NOT_SELECTED;
  FillUniRangeList(SortUniRangeListButton.Down);
  FillUnicodeGrid;
  // wp
  //cbCodePage.DropDownCount := Math.max(EnvironmentOptions.DropDownCount, 25);
  //cbUniRange.DropDownCount := Math.max(EnvironmentOptions.DropDownCount, 25);
end;

procedure TCharacterMapForm.seAnsiSizeChange(Sender: TObject);
begin
  AnsiGrid.Font.Size := seAnsiSize.Value;
  seUniSize.Value := seAnsiSize.Value;
end;

procedure TCharacterMapForm.seUniSizeChange(Sender: TObject);
begin
  UnicodeGrid.Font.Size := seUniSize.Value;
  seAnsiSize.Value := seUniSize.Value;
end;

procedure TCharacterMapForm.SortUniRangeListButtonClick(Sender: TObject);
begin
  FillUniRangeList(SortUniRangeListButton.Down);
end;

procedure TCharacterMapForm.AnsiGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  DoStatusAnsiGrid(aCol, aRow);
end;

procedure TCharacterMapForm.UnicodeGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  DoStatusUnicodeGrid(aCol, aRow);
end;

procedure TCharacterMapForm.StringGridKeyPress(Sender: TObject; var Key: char);
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

procedure TCharacterMapForm.StringGridMouseDown(Sender: TObject;
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

procedure TCharacterMapForm.DoStatusAnsiGrid(ACol, ARow: integer);
var
  N: integer;
begin
  N := ACol-1 + (ARow-1)*16 + 32;
  AnsiCharInfoLabel.Caption := Format('Decimal: %s, Hex: $%s', [IntToStr(N), IntToHex(N, 2)]);
end;

procedure TCharacterMapForm.AnsiGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  if AnsiGrid.MouseToGridZone(X, Y) = gzNormal then
  begin
    Col:=0; Row:=0;
    AnsiGrid.MouseToCell(X, Y, Col, Row);
    DoStatusAnsiGrid(Col, Row);
  end
  else
    AnsiCharInfoLabel.Caption := '-';
end;

procedure TCharacterMapForm.GridPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  with (Sender as TStringGrid) do begin
    ts := Canvas.TextStyle;
    ts.Alignment := taCenter;
    Canvas.TextStyle := ts;
    if (Sender = AnsiGrid) and ((aCol = 0) or (aRow = 0)) then 
      Canvas.Font.Size := 10;
  end;
end;

procedure TCharacterMapForm.DoStatusUnicodeGrid(ACol, ARow: integer);
var
  S: Cardinal;
  tmp, tmp2: String;
  i: Integer;
begin
  if not UnicodeBlockSelected then Exit;
  S:=UnicodeBlocks[FUnicodeBlockIndex].S+(ACol)+(ARow*16);
  tmp:=UnicodeToUTF8(S);
  tmp2:='';
  for i:=1 to Length(tmp) do
    tmp2:=tmp2+'$'+IntToHex(Ord(tmp[i]),2);
  UnicodeCharInfoLabel.Caption:='U+'+inttohex(S,4)+', UTF-8: '+tmp2;
end;

procedure TCharacterMapForm.UnicodeGridMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  if UnicodeGrid.MouseToGridZone(X, Y) = gzNormal then
  begin
    Col:=0; Row:=0;
    UnicodeGrid.MouseToCell(X, Y, Col, Row);
    DoStatusUnicodeGrid(Col, Row);
  end
  else
    AnsiCharInfoLabel.Caption := '-';
end;

procedure TCharacterMapForm.FillAnsiGrid;
var
  R, C, p: Integer;
  cp: String;
begin
  cp := cbCodePage.Items[cbCodePage.ItemIndex];
  p := pos(' ', cp);
  if p > 0 then SetLength(cp, p-1);
  for R := 0 to Pred(AnsiGrid.RowCount) do
  begin
    if R <> 0 then  AnsiGrid.Cells[0, R] := Format('%.3d +', [Succ(R) * 16]);
    for C := 1 to Pred(AnsiGrid.ColCount) do
    begin
      if R = 0 then AnsiGrid.Cells[C, R] := Format('%.2d', [Pred(C)])
      else
        AnsiGrid.Cells[C, R] := ConvertEncoding(Chr(Succ(R) * 16 + Pred(C)), cp, 'utf8');
    end;
  end;
end;

procedure TCharacterMapForm.FillUnicodeGrid;
var
  cnt, x, y: integer;
  S, E: integer;
begin
  UnicodeGrid.Clear;
  if not UnicodeBlockSelected then
    Exit;
  S:=UnicodeBlocks[FUnicodeBlockIndex].S;
  E:=UnicodeBlocks[FUnicodeBlockIndex].E;
  UnicodeGrid.ColCount:=16;
  UnicodeGrid.RowCount:=RoundUp(E-S,16);
  cnt:=0;
  for y:=0 to UnicodeGrid.RowCount-1 do
    for x:=0 to UnicodeGrid.ColCount-1 do
    begin
      if S+Cnt<=E then
        UnicodeGrid.Cells[x,y]:=UnicodeToUTF8(S+Cnt);
      inc(cnt);
    end;
  UnicodeGrid.AutoSizeColumns;
end;

procedure TCharacterMapForm.FillUniRangeList(ASorted: Boolean);
var
  BlockIdx: Integer;
begin
  cbUniRange.Items.Clear;
  cbUniRange.Sorted:=ASorted;

  for BlockIdx:=Low(UnicodeBlocks) to High(UnicodeBlocks) do
    cbUniRange.Items.Append(UnicodeBlocks[BlockIdx].PG);

  if not UnicodeBlockSelected then
    FUnicodeBlockIndex:=Low(UnicodeBlocks);
  cbUniRange.Text:=UnicodeBlocks[FUnicodeBlockIndex].PG;
end;

function TCharacterMapForm.UnicodeBlockIndexByName(AName: String): Integer;
var
  BlockIdx: Integer;
begin
  for BlockIdx:=Low(UnicodeBlocks) to High(UnicodeBlocks) do
    if UnicodeBlocks[BlockIdx].PG=AName then
      Exit(BlockIdx);
  Result:=NOT_SELECTED;
end;

function TCharacterMapForm.UnicodeBlockSelected: Boolean;
begin
  Result:=(FUnicodeBlockIndex>=Low(UnicodeBlocks)) and (FUnicodeBlockIndex<=High(UnicodeBlocks));
end;

procedure TCharacterMapForm.SetOnShowHelp(AValue: TNotifyEvent);
begin
  FOnShowHelp := AValue;
  ButtonPanel.HelpButton.OnClick := FOnShowHelp;
  if FOnShowHelp <> nil then
    ButtonPanel.ShowButtons := ButtonPanel.ShowButtons + [pbHelp]
  else
    ButtonPanel.ShowButtons := ButtonPanel.ShowButtons - [pbHelp];
end;

end.

