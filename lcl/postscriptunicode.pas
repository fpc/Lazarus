{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Jesus Reyes Aguilar
}
unit postscriptunicode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Math,Maps;

type
  TUnicodeBlock = record
    Ini, Fin, PSCount: Integer;
  end;

  PGlyph = ^TGlyph;
  TGlyph = record
    Code: Word;
    Name: string[21];
  end;

// TODO: modify GlyphsArr sorted by Name
{$i glyphlist.inc}

type

  { TPsUnicode }

  TPsUnicode = class
  private
    FFontSize: Integer;
    FFontStyle: Integer;
    FGlyphs: TMap;
    FBlocks: array of TUnicodeBlock;
    FEncodings: array of Integer;
    FOutLst, FBaseFonts,FEncodedFonts,FUsedFonts: TStringList;
    FLastFontIndex: Integer;
    FFont: string;
    procedure CountPSChars;
    procedure CreateGlyphMap;
    procedure CreateUniCodeBlocks;
    function FindEncodingIndex(ABlock: Integer): Integer;
    function IndexOfFont(AFontName:string; AFontSize,AFontStyle,ABlock:Integer): Integer;
    function SelectFont(AFontName:string; AFontSize,AFontStyle,ABlock:Integer): string;
    procedure ReportBlockEncoding(i:Integer);
    procedure SetFont(const AValue: string);
    procedure SetFontSize(const AValue: Integer);
    procedure SetFontStyle(const AValue: Integer);
  public
    constructor create;
    destructor destroy; override;
    procedure OutputString(S:string);
    function BlockFor(var w: word):integer;
    procedure ResetLastFont;
    function UnicodeToGlyph(w: word): string;
    property Font: string read FFont write SetFont;
    property FontSize: Integer read FFontSize write SetFontSize;
    property FOntStyle: Integer read FFontStyle write SetFontStyle;
    property OutLst: TStringlist read FOutLst write FOutLst;
  end;

implementation

function Octal(c : byte) : string;
begin
  result := '\' +
    char( ord('0') + (c div 64) ) +
    char( ord('0') + (c mod 64) div 8 ) +
    Char( ord('0') + (c mod 8 ) );
end;

{ TPsUnicode }

procedure TPsUnicode.CreateGlyphMap;
var
  i: word;
begin

  if FGlyphs<>nil then
    exit;

  FGlyphs := TMap.Create(itu2, SizeOf(word));
  for i:=0 to GLYPHCOUNT-1 do
    FGlyphs.Add(GlyphsArr[i].Code, i);

  CountPSChars;
end;

procedure TPsUnicode.CreateUniCodeBlocks;
  procedure AddBlock(Ini,Fin:Integer);
  var
    i: Integer;
  begin
    i := Length(FBlocks);
    SetLength(FBlocks, i+1);
    FBlocks[i].Ini:=Ini;
    FBlocks[i].Fin:=Fin;
  end;
begin
  if Length(FBlocks)>0 then
    exit;
  //(^([A-Z0-9 \-]+)*).U\+([A-F0-9]+).U\+([A-F0-9]+)
  //  AddBlock\(\$$3,\$$4); // $1

  // Following two blocks are merged into one
  //AddBlock($0000,$007F); // Basic Latin	(128)
  //AddBlock($0080,$00FF); // Latin-1 Supplement	(128)
  AddBlock($0000,$00FF); // Basic Latin + Latin1 sup (256)

  AddBlock($0100,$017F); // Latin Extended-A	(128)
  AddBlock($0180,$024F); // Latin Extended-B	(208)
  AddBlock($0250,$02AF); // IPA Extensions	(96)
  AddBlock($02B0,$02FF); // Spacing Modifier Letters	(80)
  AddBlock($0300,$036F); // Combining Diacritical Marks	(112)
  AddBlock($0370,$03FF); // Greek and Coptic	(134)
  AddBlock($0400,$04FF); // Cyrillic	(256)
  AddBlock($0500,$052F); // Cyrillic Supplement	(36)
  AddBlock($0530,$058F); // Armenian	(86)
  AddBlock($0590,$05FF); // Hebrew	(87)
  AddBlock($0600,$06FF); // Arabic	(250)
  AddBlock($0700,$074F); // Syriac	(77)
  AddBlock($0750,$077F); // Arabic Supplement	(48)
  AddBlock($0780,$07BF); // Thaana	(50)
  AddBlock($07C0,$07FF); // NKo	(59)
  AddBlock($0900,$097F); // Devanagari	(112)
  AddBlock($0980,$09FF); // Bengali	(91)
  AddBlock($0A00,$0A7F); // Gurmukhi	(79)
  AddBlock($0A80,$0AFF); // Gujarati	(83)
  AddBlock($0B00,$0B7F); // Oriya	(84)
  AddBlock($0B80,$0BFF); // Tamil	(72)
  AddBlock($0C00,$0C7F); // Telugu	(93)
  AddBlock($0C80,$0CFF); // Kannada	(86)
  AddBlock($0D00,$0D7F); // Malayalam	(95)
  AddBlock($0D80,$0DFF); // Sinhala	(80)
  AddBlock($0E00,$0E7F); // Thai	(87)
  AddBlock($0E80,$0EFF); // Lao	(65)
  AddBlock($0F00,$0FFF); // Tibetan	(201)
  AddBlock($1000,$109F); // Myanmar	(156)
  AddBlock($10A0,$10FF); // Georgian	(83)
  AddBlock($1100,$11FF); // Hangul Jamo	(240)
  AddBlock($1200,$137F); // Ethiopic	(356)
  AddBlock($1380,$139F); // Ethiopic Supplement	(26)
  AddBlock($13A0,$13FF); // Cherokee	(85)
  AddBlock($1400,$167F); // Unified Canadian Aboriginal Syllabics	(630)
  AddBlock($1680,$169F); // Ogham	(29)
  AddBlock($16A0,$16FF); // Runic	(81)
  AddBlock($1700,$171F); // Tagalog	(20)
  AddBlock($1720,$173F); // Hanunoo	(23)
  AddBlock($1740,$175F); // Buhid	(20)
  AddBlock($1760,$177F); // Tagbanwa	(18)
  AddBlock($1780,$17FF); // Khmer	(114)
  AddBlock($1800,$18AF); // Mongolian	(156)
  AddBlock($1900,$194F); // Limbu	(66)
  AddBlock($1950,$197F); // Tai Le	(35)
  AddBlock($1980,$19DF); // New Tai Lue	(80)
  AddBlock($19E0,$19FF); // Khmer Symbols	(32)
  AddBlock($1A00,$1A1F); // Buginese	(30)
  AddBlock($1B00,$1B7F); // Balinese	(121)
  AddBlock($1B80,$1BBF); // Sundanese	(55)
  AddBlock($1C00,$1C4F); // Lepcha	(74)
  AddBlock($1C50,$1C7F); // Ol Chiki	(48)
  AddBlock($1D00,$1D7F); // Phonetic Extensions	(128)
  AddBlock($1D80,$1DBF); // Phonetic Extensions Supplement	(64)
  AddBlock($1DC0,$1DFF); // Combining Diacritical Marks Supplement	(41)
  AddBlock($1E00,$1EFF); // Latin Extended Additional	(256)
  AddBlock($1F00,$1FFF); // Greek Extended	(233)
  AddBlock($2000,$206F); // General Punctuation	(107)
  AddBlock($2070,$209F); // Superscripts and Subscripts	(34)
  AddBlock($20A0,$20CF); // Currency Symbols	(22)
  AddBlock($20D0,$20FF); // Combining Diacritical Marks for Symbols	(33)
  AddBlock($2100,$214F); // Letterlike Symbols	(80)
  AddBlock($2150,$218F); // Number Forms	(54)
  AddBlock($2190,$21FF); // Arrows	(112)
  AddBlock($2200,$22FF); // Mathematical Operators	(256)
  AddBlock($2300,$23FF); // Miscellaneous Technical	(232)
  AddBlock($2400,$243F); // Control Pictures	(39)
  AddBlock($2440,$245F); // Optical Character Recognition	(11)
  AddBlock($2460,$24FF); // Enclosed Alphanumerics	(160)
  AddBlock($2500,$257F); // Box Drawing	(128)
  AddBlock($2580,$259F); // Block Elements	(32)
  AddBlock($25A0,$25FF); // Geometric Shapes	(96)
  AddBlock($2600,$26FF); // Miscellaneous Symbols	(191)
  AddBlock($2700,$27BF); // Dingbats	(174)
  AddBlock($27C0,$27EF); // Miscellaneous Mathematical Symbols-A	(44)
  AddBlock($27F0,$27FF); // Supplemental Arrows-A	(16)
  AddBlock($2800,$28FF); // Braille Patterns	(256)
  AddBlock($2900,$297F); // Supplemental Arrows-B	(128)
  AddBlock($2980,$29FF); // Miscellaneous Mathematical Symbols-B	(128)
  AddBlock($2A00,$2AFF); // Supplemental Mathematical Operators	(256)
  AddBlock($2B00,$2BFF); // Miscellaneous Symbols and Arrows	(82)
  AddBlock($2C00,$2C5F); // Glagolitic	(94)
  AddBlock($2C60,$2C7F); // Latin Extended-C	(29)
  AddBlock($2C80,$2CFF); // Coptic	(114)
  AddBlock($2D00,$2D2F); // Georgian Supplement	(38)
  AddBlock($2D30,$2D7F); // Tifinagh	(55)
  AddBlock($2D80,$2DDF); // Ethiopic Extended	(79)
  AddBlock($2DE0,$2DFF); // Cyrillic Extended-A	(32)
  AddBlock($2E00,$2E7F); // Supplemental Punctuation	(49)
  AddBlock($2E80,$2EFF); // CJK Radicals Supplement	(115)
  AddBlock($2F00,$2FDF); // Kangxi Radicals	(214)
  AddBlock($2FF0,$2FFF); // Ideographic Description Characters	(12)
  AddBlock($3000,$303F); // CJK Symbols and Punctuation	(64)
  AddBlock($3040,$309F); // Hiragana	(93)
  AddBlock($30A0,$30FF); // Katakana	(96)
  AddBlock($3100,$312F); // Bopomofo	(41)
  AddBlock($3130,$318F); // Hangul Compatibility Jamo	(94)
  AddBlock($3190,$319F); // Kanbun	(16)
  AddBlock($31A0,$31BF); // Bopomofo Extended	(24)
  AddBlock($31C0,$31EF); // CJK Strokes	(36)
  AddBlock($31F0,$31FF); // Katakana Phonetic Extensions	(16)
  AddBlock($3200,$32FF); // Enclosed CJK Letters and Months	(242)
  AddBlock($3300,$33FF); // CJK Compatibility	(256)
  AddBlock($3400,$4DBF); // CJK Unified Ideographs Extension A	(2)
  AddBlock($4DC0,$4DFF); // Yijing Hexagram Symbols	(64)
  AddBlock($4E00,$9FFF); // CJK Unified Ideographs	(2)
  AddBlock($A000,$A48F); // Yi Syllables	(1165)
  AddBlock($A490,$A4CF); // Yi Radicals	(55)
  AddBlock($A500,$A63F); // Vai	(300)
  AddBlock($A640,$A69F); // Cyrillic Extended-B	(78)
  AddBlock($A700,$A71F); // Modifier Tone Letters	(32)
  AddBlock($A720,$A7FF); // Latin Extended-D	(114)
  AddBlock($A800,$A82F); // Syloti Nagri	(44)
  AddBlock($A840,$A87F); // Phags-pa	(56)
  AddBlock($A880,$A8DF); // Saurashtra	(81)
  AddBlock($A900,$A92F); // Kayah Li	(48)
  AddBlock($A930,$A95F); // Rejang	(37)
  AddBlock($AA00,$AA5F); // Cham	(83)
  AddBlock($AC00,$D7AF); // Hangul Syllables	(2)
  AddBlock($D800,$DB7F); // High Surrogates	(2)
  AddBlock($DB80,$DBFF); // High Private Use Surrogates	(2)
  AddBlock($DC00,$DFFF); // Low Surrogates	(2)
  AddBlock($E000,$F8FF); // Private Use Area	(2)
  AddBlock($F900,$FAFF); // CJK Compatibility Ideographs	(467)
  AddBlock($FB00,$FB4F); // Alphabetic Presentation Forms	(58)
  AddBlock($FB50,$FDFF); // Arabic Presentation Forms-A	(595)
  AddBlock($FE00,$FE0F); // Variation Selectors	(16)
  AddBlock($FE10,$FE1F); // Vertical Forms	(10)
  AddBlock($FE20,$FE2F); // Combining Half Marks	(7)
  AddBlock($FE30,$FE4F); // CJK Compatibility Forms	(32)
  AddBlock($FE50,$FE6F); // Small Form Variants	(26)
  AddBlock($FE70,$FEFF); // Arabic Presentation Forms-B	(141)
  AddBlock($FF00,$FFEF); // Halfwidth and Fullwidth Forms	(225)
  AddBlock($FFF0,$FFFF); // Specials	(5)

  // next blocks are outside BMP
  //AddBlock($10000,$1007F); // Linear B Syllabary	(88)
  //AddBlock($10080,$100FF); // Linear B Ideograms	(123)
  //AddBlock($10100,$1013F); // Aegean Numbers	(57)
  //AddBlock($10140,$1018F); // Ancient Greek Numbers	(75)
  //AddBlock($10190,$101CF); // Ancient Symbols	(12)
  //AddBlock($101D0,$101FF); // Phaistos Disc	(46)
  //AddBlock($10280,$1029F); // Lycian	(29)
  //AddBlock($102A0,$102DF); // Carian	(49)
  //AddBlock($10300,$1032F); // Old Italic	(35)
  //AddBlock($10330,$1034F); // Gothic	(27)
  //AddBlock($10380,$1039F); // Ugaritic	(31)
  //AddBlock($103A0,$103DF); // Old Persian	(50)
  //AddBlock($10400,$1044F); // Deseret	(80)
  //AddBlock($10450,$1047F); // Shavian	(48)
  //AddBlock($10480,$104AF); // Osmanya	(40)
  //AddBlock($10800,$1083F); // Cypriot Syllabary	(55)
  //AddBlock($10900,$1091F); // Phoenician	(27)
  //AddBlock($10920,$1093F); // Lydian	(27)
  //AddBlock($10A00,$10A5F); // Kharoshthi	(65)
  //AddBlock($12000,$123FF); // Cuneiform	(879)
  //AddBlock($12400,$1247F); // Cuneiform Numbers and Punctuation	(103)
  //AddBlock($1D000,$1D0FF); // Byzantine Musical Symbols	(246)
  //AddBlock($1D100,$1D1FF); // Musical Symbols	(220)
  //AddBlock($1D200,$1D24F); // Ancient Greek Musical Notation	(70)
  //AddBlock($1D300,$1D35F); // Tai Xuan Jing Symbols	(87)
  //AddBlock($1D360,$1D37F); // Counting Rod Numerals	(18)
  //AddBlock($1D400,$1D7FF); // Mathematical Alphanumeric Symbols	(996)
  //AddBlock($1F000,$1F02F); // Mahjong Tiles	(44)
  //AddBlock($1F030,$1F09F); // Domino Tiles	(100)
  //AddBlock($20000,$2A6DF); // CJK Unified Ideographs Extension B	(2)
  //AddBlock($2F800,$2FA1F); // CJK Compatibility Ideographs Supplement	(542)
  //AddBlock($E0000,$E007F); // Tags	(97)
  //AddBlock($E0100,$E01EF); // Variation Selectors Supplement	(240)
  //AddBlock($F0000,$FFFFF); // Supplementary Private Use Area-A	(2)
  //AddBlock($100000,$10FFFF); // Supplementary Private Use Area-B	(2)
end;

constructor TPsUnicode.create;
begin
  inherited create;
  FBaseFonts := TStringList.Create;
  FEncodedFonts := TStringList.Create;
  FUsedFonts := TStringList.Create;
  FLastFontIndex := -1;
  FFontSize := 12;
end;

destructor TPsUnicode.destroy;
begin
  FUsedFonts.Free;
  FEncodedFonts.Free;
  FBaseFonts.Free;
  FGlyphs.Free;
  inherited destroy;
end;

procedure TPsUnicode.OutputString(S: string);
var
{$IFDEF FPC_HAS_UNICODESTRING}
  UStr: UnicodeString;
{$ELSE}
  UStr: WideString;
{$ENDIF}
  w: word;
  i, b: Integer;
  c: char;
  SubStr,FontStr: string;
  FontIndex: Integer;

  procedure EmitSubStr;
  begin
    if SubStr<>'' then begin
      OutLst.Add(FontStr + '('+SubStr+') show');
    end;
    SubStr := '';
    FontStr := '';
  end;

begin

  CreateUnicodeBlocks;
  CreateGlyphMap;

  UStr := UTF8Decode(S);
  SubStr := '';
  for i:=1 to Length(UStr) do begin

    w := word(UStr[i]);
    b := BlockFor(w);

    FontIndex := IndexOfFont(Font, FontSize, FontStyle, b);
    if (FontIndex<0) or (FontIndex<>FLastFontIndex) then begin
      EmitSubStr;
      FontStr := SelectFont(Font, FontSize, FontStyle, b);
    end;

    c := Char(Byte(w-FBlocks[b].Ini));
    if c in  [#0..#31,'(',')','\'] then
      SubStr := SubStr + Octal(ord(c))
    else
      SubStr := SubStr + c;
  end;

  EmitSubStr;
end;

procedure TPsUnicode.CountPSChars;
var
  Id: Word;
  i,j: Integer;
begin
  for i:=0 to Length(FBlocks)-1 do begin
    FBlocks[i].PSCount:=0;
    for j:=FBlocks[i].Ini to FBlocks[i].Fin do begin
      Id := word(j);
      if FGlyphs.HasId(Id) then
        Inc(FBlocks[i].PSCount);
    end;
  end;
end;

function TPsUnicode.BlockFor(var w: word): integer;
var
  i: Integer;
begin
  CreateUnicodeBlocks;
  for i:=0 to Length(FBlocks)-1 do begin
    if FBlocks[i].PSCount=0 then
      continue;
    if w<FBlocks[i].Ini then
      break
    else
    if w<=FBlocks[i].Fin then begin
      result := i;
      exit;
    end;
  end;
  result := 0;
  w := 32;
end;

function TPsUnicode.FindEncodingIndex(ABlock: Integer): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=0 to Length(FEncodings)-1 do
    if FEncodings[i]=ABlock then begin
      result := i;
      break;
    end;
end;

function TPsUnicode.IndexOfFont(AFontName: string; AFontSize, AFontStyle,
  ABlock: Integer): Integer;
var
  BaseName: string;
begin
  Result := FindEncodingIndex(ABlock);
  if Result<0 then
    exit;

  Result := FBaseFonts.IndexOf(AFontName);
  if Result<0 then
    exit;

  BaseName := 'F'+IntToStr(Result)+IntToStr(ABlock);
  Result := FEncodedFonts.IndexOf(BaseName);
  if Result<0 then
      exit;

  Result := FUsedFonts.IndexOf(BaseName+IntToStr(AFontSize)+IntToStr(AFontStyle));
end;

function TPsUnicode.SelectFont(AFontName: string; AFontSize, AFontStyle,
  ABlock: Integer): string;
var
  i,EncIndx: Integer;
  EncFont,EncScaledFont: string;
begin

  EncIndx := FindEncodingIndex(ABlock);
  if EncIndx<0 then begin
    // add new encoding array
    EncIndx := Length(FEncodings);
    Setlength(FEncodings, EncIndx+1);
    FEncodings[EncIndx] := ABlock;

    // emit encoding array
    OutLst.Add(format('/Arr%d    %% %.4x-%.4x',[ABlock,FBLocks[ABlock].ini,FBLocks[ABlock].fin]));
    OutLst.Add('[');
    ReportBlockEncoding(ABlock);
    OutLst.Add('] def');
    OutLst.Add('');;
  end;

  // NewFontName [NewEncodingArray] /FontName RE
  i := FBaseFonts.IndexOf(AFontName);
  if i<0 then
    i := FBaseFonts.Add(AFontName);

  EncFont :='F'+IntToStr(i)+IntToStr(ABlock);
  i := FEncodedFonts.IndexOf(EncFont);
  if i<0 then begin
    i := FEncodedFonts.Add(EncFont);
    OutLst.Add('');;
    OutLst.Add( format('/%s Arr%d /%s RE',[EncFont,ABlock,AFontName]));
  end;

  // if it's first font use, scale and define it, else just invoke it
  EncScaledFont := format('%s%d%d',[EncFont,AFontSize,AFontStyle]);
  i := FUsedFonts.IndexOf(EncScaledFont);
  if i<0 then begin
    i := FUsedFonts.Add(EncScaledFont);
    OutLst.Add(format('/%s { /%s %d selectfont } bind def',
      [EncScaledFont,EncFont,AFontSize]));
    OutLst.Add('');
  end;
  if FLastFontIndex<>i then begin
    FLastFontIndex := i;
    Result := EncScaledFont + ' ';
  end else
    Result := '';
end;

procedure TPsUnicode.ReportBlockEncoding(i: Integer);
var
  b,g,j: Integer;
  Id,GlypIndx: Word;
  S: string;
  n: Integer;
begin
  CreateGlyphMap;
  n := FBlocks[i].Fin-FBlocks[i].Ini+1;
  g := FBlocks[i].Ini;
  S := '';
  for b:=1 to ceil(n/256) do begin
    for j:=0 to 255 do begin
      Id := Word(g);
      if (g<=FBlocks[i].Fin) and FGlyphs.HasID(Id) then begin
        FGlyphs.GetData(Id, GlypIndx);
        S := S + '/'+GlyphsArr[GlypIndx].Name+' ';
      end else
        S := S + '/uni'+IntToHex(Id,4)+' ';
      if (j+1) mod 8 = 0 then begin
        OutLst.Add(S);
        S := '';
      end;
      inc(g);
    end;
    break; // TODO: handle only first 256 chars on blocks with too many chars
  end;
  if s<>'' then
    OutLst.Add(S);
end;

procedure TPsUnicode.SetFont(const AValue: string);
begin
  if AValue='' then
    FFont := 'Times-Roman'
  else
    FFont := AValue;
end;

procedure TPsUnicode.SetFontSize(const AValue: Integer);
begin
  if FFontSize>0 then
    FFontSize := AValue;
end;

procedure TPsUnicode.SetFontStyle(const AValue: Integer);
begin
  if FFontStyle<0 then
    FFontStyle := 0
  else
    FFontStyle := AValue;
end;

procedure TPsUnicode.ResetLastFont;
begin
  FLastFontIndex:=-1;
end;

function TPsUnicode.UnicodeToGlyph(w: word): string;
var
  i: word;
begin
  CreateGlyphMap;

  if FGlyphs.GetData(w, i) then
    result := GlyphsArr[i].Name
  else
    result := '';
end;

end.

