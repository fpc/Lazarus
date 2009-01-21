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
  StdCtrls, LCLType, LCLProc, GraphType, LazarusIDEStrConsts, EditorOptions,
  EnvironmentOpts, Grids, IDEContextHelpEdit, ButtonPanel, ComCtrls;

type
  TOnInsertCharacterEvent = procedure (const C: TUTF8Char) of object;

  { TCharacterMapDialog }

  TCharacterMapDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    CharInfoLabel: TLabel;
    cbUniRange: TComboBox;
    UnicodeCharInfoLabel: TLabel;
    PageControl1: TPageControl;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure cbUniRangeSelect(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StringGrid2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid2MouseMove(Sender: TObject; Shift: TShiftState; X,
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

type TUnicodeRange = record
    S: longint;
    E: longint;
    PG: string[50];
  end;

const
  MaxUnicodeRanges = 151;
  UnicodeRanges: array[0..MaxUnicodeRanges] of TUnicodeRange = (

    (S: $0020; E: $007F; PG: 'Basic Latin'),
    (S: $00A0; E: $00FF; PG: 'Latin-1 Supplement'),
    (S: $0100; E: $017F; PG: 'Latin Extended-A'),
    (S: $0180; E: $024F; PG: 'Latin Extended-B'),
    (S: $0250; E: $02AF; PG: 'IPA Extensions'),
    (S: $02B0; E: $02FF; PG: 'Spacing Modifier Letters'),
    (S: $0300; E: $036F; PG: 'Combining Diacritical Marks'),
    (S: $0370; E: $03FF; PG: 'Greek and Coptic'),
    (S: $0400; E: $04FF; PG: 'Cyrillic'),
    (S: $0500; E: $052F; PG: 'Cyrillic Supplement'),
    (S: $0530; E: $058F; PG: 'Armenian'),
    (S: $0590; E: $05FF; PG: 'Hebrew'),
    (S: $0600; E: $06FF; PG: 'Arabic'),
    (S: $0700; E: $074F; PG: 'Syriac'),
    (S: $0750; E: $077F; PG: 'Arabic Supplement'),
    (S: $0780; E: $07BF; PG: 'Thaana'),
    (S: $07C0; E: $07FF; PG: 'NKo'),
    (S: $0900; E: $097F; PG: 'Devanagari'),
    (S: $0980; E: $09FF; PG: 'Bengali'),
    (S: $0A00; E: $0A7F; PG: 'Gurmukhi'),
    (S: $0A80; E: $0AFF; PG: 'Gujarati'),
    (S: $0B00; E: $0B7F; PG: 'Oriya'),
    (S: $0B80; E: $0BFF; PG: 'Tamil'),
    (S: $0C00; E: $0C7F; PG: 'Telugu'),
    (S: $0C80; E: $0CFF; PG: 'Kannada'),
    (S: $0D00; E: $0D7F; PG: 'Malayalam'),
    (S: $0D80; E: $0DFF; PG: 'Sinhala'),
    (S: $0E00; E: $0E7F; PG: 'Thai'),
    (S: $0E80; E: $0EFF; PG: 'Lao'),
    (S: $0F00; E: $0FFF; PG: 'Tibetan'),
    (S: $1000; E: $109F; PG: 'Myanmar'),
    (S: $10A0; E: $10FF; PG: 'Georgian'),
    (S: $1100; E: $11FF; PG: 'Hangul Jamo'),
    (S: $1200; E: $137F; PG: 'Ethiopic'),
    (S: $1380; E: $139F; PG: 'Ethiopic Supplement'),
    (S: $13A0; E: $13FF; PG: 'Cherokee'),
    (S: $1400; E: $167F; PG: 'Unified Canadian Aboriginal Syllabics'),
    (S: $1680; E: $169F; PG: 'Ogham'),
    (S: $16A0; E: $16FF; PG: 'Runic'),
    (S: $1700; E: $171F; PG: 'Tagalog'),
    (S: $1720; E: $173F; PG: 'Hanunoo'),
    (S: $1740; E: $175F; PG: 'Buhid'),
    (S: $1760; E: $177F; PG: 'Tagbanwa'),
    (S: $1780; E: $17FF; PG: 'Khmer'),
    (S: $1800; E: $18AF; PG: 'Mongolian'),
    (S: $1900; E: $194F; PG: 'Limbu'),
    (S: $1950; E: $197F; PG: 'Tai Le'),
    (S: $1980; E: $19DF; PG: 'New Tai Lue'),
    (S: $19E0; E: $19FF; PG: 'Khmer Symbols'),
    (S: $1A00; E: $1A1F; PG: 'Buginese'),
    (S: $1B00; E: $1B7F; PG: 'Balinese'),
    (S: $1D00; E: $1D7F; PG: 'Phonetic Extensions'),
    (S: $1D80; E: $1DBF; PG: 'Phonetic Extensions Supplement'),
    (S: $1DC0; E: $1DFF; PG: 'Combining Diacritical Marks Supplement'),
    (S: $1E00; E: $1EFF; PG: 'Latin Extended Additional'),
    (S: $1F00; E: $1FFF; PG: 'Greek Extended'),
    (S: $2000; E: $206F; PG: 'General Punctuation'),
    (S: $2070; E: $209F; PG: 'Superscripts and Subscripts'),
    (S: $20A0; E: $20CF; PG: 'Currency Symbols'),
    (S: $20D0; E: $20FF; PG: 'Combining Diacritical Marks for Symbols'),
    (S: $2100; E: $214F; PG: 'Letterlike Symbols'),
    (S: $2150; E: $218F; PG: 'Number Forms'),
    (S: $2190; E: $21FF; PG: 'Arrows'),
    (S: $2200; E: $22FF; PG: 'Mathematical Operators'),
    (S: $2300; E: $23FF; PG: 'Miscellaneous Technical'),
    (S: $2400; E: $243F; PG: 'Control Pictures'),
    (S: $2440; E: $245F; PG: 'Optical Character Recognition'),
    (S: $2460; E: $24FF; PG: 'Enclosed Alphanumerics'),
    (S: $2500; E: $257F; PG: 'Box Drawing'),
    (S: $2580; E: $259F; PG: 'Block Elements'),
    (S: $25A0; E: $25FF; PG: 'Geometric Shapes'),
    (S: $2600; E: $26FF; PG: 'Miscellaneous Symbols'),
    (S: $2700; E: $27BF; PG: 'Dingbats'),
    (S: $27C0; E: $27EF; PG: 'Miscellaneous Mathematical Symbols-A'),
    (S: $27F0; E: $27FF; PG: 'Supplemental Arrows-A'),
    (S: $2800; E: $28FF; PG: 'Braille Patterns'),
    (S: $2900; E: $297F; PG: 'Supplemental Arrows-B'),
    (S: $2980; E: $29FF; PG: 'Miscellaneous Mathematical Symbols-B'),
    (S: $2A00; E: $2AFF; PG: 'Supplemental Mathematical Operators'),
    (S: $2B00; E: $2BFF; PG: 'Miscellaneous Symbols and Arrows'),
    (S: $2C00; E: $2C5F; PG: 'Glagolitic'),
    (S: $2C60; E: $2C7F; PG: 'Latin Extended-C'),
    (S: $2C80; E: $2CFF; PG: 'Coptic'),
    (S: $2D00; E: $2D2F; PG: 'Georgian Supplement'),
    (S: $2D30; E: $2D7F; PG: 'Tifinagh'),
    (S: $2D80; E: $2DDF; PG: 'Ethiopic Extended'),
    (S: $2E00; E: $2E7F; PG: 'Supplemental Punctuation'),
    (S: $2E80; E: $2EFF; PG: 'CJK Radicals Supplement'),
    (S: $2F00; E: $2FDF; PG: 'Kangxi Radicals'),
    (S: $2FF0; E: $2FFF; PG: 'Ideographic Description Characters'),
    (S: $3000; E: $303F; PG: 'CJK Symbols and Punctuation'),
    (S: $3040; E: $309F; PG: 'Hiragana'),
    (S: $30A0; E: $30FF; PG: 'Katakana'),
    (S: $3100; E: $312F; PG: 'Bopomofo'),
    (S: $3130; E: $318F; PG: 'Hangul Compatibility Jamo'),
    (S: $3190; E: $319F; PG: 'Kanbun'),
    (S: $31A0; E: $31BF; PG: 'Bopomofo Extended'),
    (S: $31C0; E: $31EF; PG: 'CJK Strokes'),
    (S: $31F0; E: $31FF; PG: 'Katakana Phonetic Extensions'),
    (S: $3200; E: $32FF; PG: 'Enclosed CJK Letters and Months'),
    (S: $3300; E: $33FF; PG: 'CJK Compatibility'),
    (S: $3400; E: $4DBF; PG: 'CJK Unified Ideographs Extension A'),
    (S: $4DC0; E: $4DFF; PG: 'Yijing Hexagram Symbols'),
    (S: $4E00; E: $9FFF; PG: 'CJK Unified Ideographs'),
    (S: $A000; E: $A48F; PG: 'Yi Syllables'),
    (S: $A490; E: $A4CF; PG: 'Yi Radicals'),
    (S: $A700; E: $A71F; PG: 'Modifier Tone Letters'),
    (S: $A720; E: $A7FF; PG: 'Latin Extended-D'),
    (S: $A800; E: $A82F; PG: 'Syloti Nagri'),
    (S: $A840; E: $A87F; PG: 'Phags-pa'),
    (S: $AC00; E: $D7AF; PG: 'Hangul Syllables'),
    (S: $D800; E: $DB7F; PG: 'High Surrogates'),
    (S: $DB80; E: $DBFF; PG: 'High Private Use Surrogates'),
    (S: $DC00; E: $DFFF; PG: 'Low Surrogates'),
    (S: $E000; E: $F8FF; PG: 'Private Use Area'),
    (S: $F900; E: $FAFF; PG: 'CJK Compatibility Ideographs'),
    (S: $FB00; E: $FB4F; PG: 'Alphabetic Presentation Forms'),
    (S: $FB50; E: $FDFF; PG: 'Arabic Presentation Forms-A'),
    (S: $FE00; E: $FE0F; PG: 'Variation Selectors'),
    (S: $FE10; E: $FE1F; PG: 'Vertical Forms'),
    (S: $FE20; E: $FE2F; PG: 'Combining Half Marks'),
    (S: $FE30; E: $FE4F; PG: 'CJK Compatibility Forms'),
    (S: $FE50; E: $FE6F; PG: 'Small Form Variants'),
    (S: $FE70; E: $FEFF; PG: 'Arabic Presentation Forms-B'),
    (S: $FF00; E: $FFEF; PG: 'Halfwidth and Fullwidth Forms'),
    (S: $FFF0; E: $FFFF; PG: 'Specials'),
    (S: $10000; E: $1007F; PG: 'Linear B Syllabary'),
    (S: $10080; E: $100FF; PG: 'Linear B Ideograms'),
    (S: $10100; E: $1013F; PG: 'Aegean Numbers'),
    (S: $10140; E: $1018F; PG: 'Ancient Greek Numbers'),
    (S: $10300; E: $1032F; PG: 'Old Italic'),
    (S: $10330; E: $1034F; PG: 'Gothic'),
    (S: $10380; E: $1039F; PG: 'Ugaritic'),
    (S: $103A0; E: $103DF; PG: 'Old Persian'),
    (S: $10400; E: $1044F; PG: 'Deseret'),
    (S: $10450; E: $1047F; PG: 'Shavian'),
    (S: $10480; E: $104AF; PG: 'Osmanya'),
    (S: $10800; E: $1083F; PG: 'Cypriot Syllabary'),
    (S: $10900; E: $1091F; PG: 'Phoenician'),
    (S: $10A00; E: $10A5F; PG: 'Kharoshthi'),
    (S: $12000; E: $123FF; PG: 'Cuneiform'),
    (S: $12400; E: $1247F; PG: 'Cuneiform Numbers and Punctuation'),
    (S: $1D000; E: $1D0FF; PG: 'Byzantine Musical Symbols'),
    (S: $1D100; E: $1D1FF; PG: 'Musical Symbols'),
    (S: $1D200; E: $1D24F; PG: 'Ancient Greek Musical Notation'),
    (S: $1D300; E: $1D35F; PG: 'Tai Xuan Jing Symbols'),
    (S: $1D360; E: $1D37F; PG: 'Counting Rod Numerals'),
    (S: $1D400; E: $1D7FF; PG: 'Mathematical Alphanumeric Symbols'),
    (S: $20000; E: $2A6DF; PG: 'CJK Unified Ideographs Extension B'),
    (S: $2F800; E: $2FA1F; PG: 'CJK Compatibility Ideographs Supplement'),
    (S: $E0000; E: $E007F; PG: 'Tags'),
    (S: $E0100; E: $E01EF; PG: 'Variation Selectors Supplement')
    //(S: $F0000; E: $FFFFF; PG: 'Supplementary Private Use Area-A'),
    //(S: $100000; E: $10FFFF; PG: 'Supplementary Private Use Area-B')
    );


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

  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;
  //EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
  CharInfoLabel.Caption := '-';
  UnicodeCharInfoLabel.Caption := '-';
  FillCharMap;
end;

procedure TCharacterMapDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

function RoundUp(Value, Divi:integer):integer;
begin
  if Value mod Divi=0 then
   Result:=Value div Divi else
   Result:=(Value div Divi)+1;
end;

procedure TCharacterMapDialog.cbUniRangeSelect(Sender: TObject);
var cnt, x, y :integer;
S,E:Integer;
begin
 S:=UnicodeRanges[cbUniRange.ItemIndex].S;
 E:=UnicodeRanges[cbUniRange.ItemIndex].E;
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

procedure TCharacterMapDialog.FormShow(Sender: TObject);
var i:integer;
begin
  StringGrid1.Font.Name := EditorOpts.EditorFont;
  StringGrid2.Font.Name := EditorOpts.EditorFont;
  StringGrid1.Font.Size := 10;
  StringGrid2.Font.Size := 10;

  StringGrid1.AutoSizeColumns;

  for i:=0 to MaxUnicodeRanges do cbUniRange.Items.Add(UnicodeRanges[i].PG);
  cbUniRange.ItemIndex:=0;
  cbUniRangeSelect(nil);
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
      OnInsertCharacter(StringGrid1.Cells[Col, Row]);
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
    
    CharOrd := Ord(UTF8ToAnsi(StringGrid1.Cells[Col, Row])[1]);
    CharInfoLabel.Caption := 'Decimal = ' + IntToStr(CharOrd) +
                             ', Hex = $'  + HexStr(CharOrd, 2);
  end
  else
  begin
    CharInfoLabel.Caption := '-';
  end;
end;

procedure TCharacterMapDialog.StringGrid2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Row, Col: Integer;
begin
  if (Button = mbLeft) and (StringGrid2.MouseToGridZone(X, Y) = gzNormal) then
  begin
    StringGrid2.MouseToCell(X, Y, Col, Row);
    if Assigned(OnInsertCharacter) then
      OnInsertCharacter(StringGrid2.Cells[Col, Row]);
  end;
end;

procedure TCharacterMapDialog.StringGrid2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var  Row, Col, i: Integer;
     S:Cardinal;
     tmp,tmp2:String;
begin
  if StringGrid2.MouseToGridZone(X, Y) = gzNormal then
  begin
    StringGrid2.MouseToCell(X, Y, Col, Row);
    S:=UnicodeRanges[cbUniRange.ItemIndex].S+(Col)+(Row*16);
    tmp:=UnicodeToUTF8(S);
    tmp2:='';
    for i:=1 to Length(tmp) do tmp2:=tmp2+'$'+IntToHex(Ord(tmp[i]),2);
    UnicodeCharInfoLabel.Caption:='U+'+inttohex(S,4)+', UTF-8 = '+tmp2;
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
        StringGrid1.Cells[C, R] := AnsiToUTF8(Chr(Succ(R) * 16 + Pred(C)));
    end;
  end;
end;


initialization
  {$I charactermapdlg.lrs}

end.

