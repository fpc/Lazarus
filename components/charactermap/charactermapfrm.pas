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
  TCharMapPage = (cmpUnicode, cmpAnsi);
  TOnInsertCharacterEvent = procedure (const C: TUTF8Char) of object;

  { TCharacterMapForm }

  TCharacterMapForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cbCodePage: TComboBox;
    AnsiCharInfoLabel: TLabel;
    cbUniRange: TComboBox;
    AnsiSizeLabel: TLabel;
    cbUniScripts: TComboBox;
    chbBMPOnly: TCheckBox;
    ScriptsLabel: TLabel;
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
    procedure cbUniScriptsSelect(Sender: TObject);
    procedure chbBMPOnlyChange(Sender: TObject);
    procedure GridPrepareCanvas(sender: TObject; {%H-}aCol, {%H-}aRow: Integer;
      {%H-}aState: TGridDrawState);
    procedure cbCodePageSelect(Sender: TObject);
    procedure cbUniRangeSelect(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; {%H-}Shift: TShiftState);
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
    FOnInsertCharacter: TOnInsertCharacterEvent;
    FUnicodeBlockIndex: Integer;
    procedure DoStatusAnsiGrid(ACol, ARow: integer);
    procedure DoStatusUnicodeGrid(ACol, ARow: integer);
    procedure FillAnsiGrid;
    procedure FillScripts;
    procedure FillUnicodeGrid;
    procedure FillUniRangeList;
    function GetActivePage: TCharMapPage;
    function GetAlphaSort: Boolean;
    function GetDropDownCount: Integer;
    function GetFontSize: Integer;
    procedure ScriptSelect;
    function UnicodeBlockIndexByName(AName: String): Integer;
    function UnicodeBlockSelected: Boolean;
    procedure SelectSystemCP;
    procedure SetActivePage(AValue: TCharMapPage);
    procedure SetAlphaSort(AValue: Boolean);
    procedure SetDropDownCount(AValue: Integer);
    procedure SetFontSize(AValue: Integer);
  public
    property ActivePage: TCharMapPage read GetActivePage write SetActivePage;
    property AlphaSort: Boolean read GetAlphaSort write SetAlphaSort;
    property DropDownCount: Integer read GetDropDownCount write SetDropDownCount;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property OnInsertCharacter: TOnInsertCharacterEvent
      read FOnInsertCharacter write FOnInsertCharacter;
  end;

procedure ShowCharacterMap(AOnInsertChar: TOnInsertCharacterEvent);

var
  CharacterMapForm: TCharacterMapForm;

resourcestring
  lisCharacterMap = 'Character Map';
  lisRange = 'Range';
  lisScript = 'Script';
  lisBMPOnly = 'BMP Only';
  lisBMPOnlyHint = 'Basic Multilingual Plane Only';
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
  ScriptsLabel.Caption:=lisScript;
  chbBMPOnly.Caption:=lisBMPOnly;
  chbBMPOnly.Hint:=lisBMPOnlyHint;
  UniSizeLabel.Caption := lisCharSize;
  CodePageLabel.Caption := lisCodePage;
  AnsiSizeLabel.Caption := lisCharSize;
  SortUniRangeListButton.Flat := True;
  SortUniRangeListButton.Hint := lisSortUnicodeRangeListAlphabetically;
  SortUniRangeListButton.Images := LCLGlyphs;
  SortUniRangeListButton.ImageIndex := LCLGlyphs.GetImageIndex('charmap_sortalphabetically');
  ButtonPanel.HelpButton.Caption := rsMbHelp;
  ButtonPanel.CloseButton.Caption := rsMbClose;
  ButtonPanel.ShowButtons := [pbClose, pbHelp];

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
  // Auto-adjust the width of the AnsiGrid's fixed column. Note that
  // the font defined in PrepareCanvas is ignored by AutoSizeColumn.
  savedfontSize := AnsiGrid.Font.Size;
  AnsiGrid.Font.Size := 10;
  AnsiGrid.AutoSizeColumn(0);
  AnsiGrid.Font.Size := savedFontSize;
  // Now also auto-adjust the widths of the other AnsiGrid to fill the client area.
  AnsiGrid.AutoFillColumns := true;

  FUnicodeBlockIndex:=NOT_SELECTED;
  FillScripts;
  FillUniRangeList;
  FillUnicodeGrid;
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
  FillUniRangeList;
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
  AnsiCharInfoLabel.Caption := Format('%s / $%s', [IntToStr(N), IntToHex(N, 2)]);
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

procedure TCharacterMapForm.ScriptSelect;
begin
  FillUniRangeList;
  FillUnicodeGrid;
end;

procedure TCharacterMapForm.chbBMPOnlyChange(Sender: TObject);
begin
  ScriptSelect;
end;

procedure TCharacterMapForm.cbUniScriptsSelect(Sender: TObject);
begin
  ScriptSelect;
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


procedure TCharacterMapForm.FillScripts;
var
  ScriptsIdx: Integer;
begin
  cbUniScripts.Items.Clear;
  cbUniScripts.Items.Add('All');
  for ScriptsIdx:=Low(Scripts) to High(Scripts)-4 do
    cbUniScripts.Items.Add(Scripts[ScriptsIdx]);

  cbUniScripts.ItemIndex:=0;
  cbUniScripts.Text:=cbUniScripts.Items[cbUniScripts.ItemIndex];
end;

procedure TCharacterMapForm.FillUniRangeList;
var
  UniBlock: TUnicodeBlock;
  BlockIdx, NewItemIndex: Integer;
  Txt: string;
begin
  Txt:=cbUniRange.Text;
  cbUniRange.Items.Clear;
  cbUniRange.Sorted:=SortUniRangeListButton.Down;
  NewItemIndex:=-1;
  for BlockIdx:=Low(UnicodeBlocks) to High(UnicodeBlocks) do
  begin
    UniBlock:=UnicodeBlocks[BlockIdx];
    if ((UniBlock.PL=0) or not chbBMPOnly.Checked)
    and ((UniBlock.SC=cbUniScripts.ItemIndex-1) or (cbUniScripts.ItemIndex=0))
    then begin
      cbUniRange.Items.Append(UniBlock.PG);
      if UniBlock.PG=Txt then
        NewItemIndex:=cbUniRange.Items.Count-1;
    end;
  end;
  if NewItemIndex>-1 then
    cbUniRange.ItemIndex:=NewItemIndex
  else
    cbUniRange.ItemIndex:=0;
  FUnicodeBlockIndex:=UnicodeBlockIndexByName(cbUniRange.Text);
end;

function TCharacterMapForm.GetActivePage: TCharMapPage;
begin
  Result := TCharMapPage(PageControl1.ActivePageIndex);
end;

function TCharacterMapForm.GetAlphaSort: Boolean;
begin
  Result := SortUniRangeListButton.Down;
end;

function TCharacterMapForm.GetDropDownCount: Integer;
begin
  Result := CbUniRange.DropDownCount;
end;

function TCharacterMapForm.GetFontSize: Integer;
begin
  Result := seUniSize.Value;
end;

procedure TCharacterMapForm.SetActivePage(AValue: TCharMapPage);
begin
  PageControl1.ActivePageIndex := ord(AValue);
end;

procedure TCharacterMapForm.SetAlphaSort(AValue: Boolean);
begin
  SortUniRangeListButton.Down := AValue;
end;

procedure TCharacterMapForm.SetDropDownCount(AValue: Integer);
begin
  CbUniRange.DropDownCount := AValue;
  CbCodePage.DropDownCount := AValue;
end;

procedure TCharacterMapForm.SetFontSize(AValue: Integer);
begin
  seUniSize.Value := AValue;
  seAnsiSize.Value := AValue;

  UnicodeGrid.Font.Size := AValue;
  AnsiGrid.Font.Size := AValue;
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

end.

