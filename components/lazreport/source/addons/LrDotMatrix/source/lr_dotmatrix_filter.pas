{*
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 Grupo SC10
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *}
unit lr_dotmatrix_filter;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  // lazreport
  LR_Class,
  LR_Prntr;

const
  //LPI
  standartLPI = 5;

  // DPI
  horizontalDPI = (93 / 1.015); // Horizontal DPI, 91 DPI default (used by lazreport)
  verticalDPI = (93 / 1.022); // Vertical DPI, 91 DPI default (used by lazreport))

  // ESC command index
  cmdReset = 0;
  cmdCR = 1; // Carriage return(return to start of line)
  cmdLF = 2; // Line feed
  cmdFF = 3; // Form feed(new line)
  cmdSpace = 4;
  cmdBoldON = 5;
  cmdBoldOFF = 6;
  cmdItalicsON = 7;
  cmdItalicsOFF = 8;
  cmdUnderlineON = 9;
  cmdUnderlineOFF = 10;
  cmdExpandedON = 11;
  cmdExpandedOFF = 12;
  cmd10CPI = 13;
  cmd20CPI = 14;
  cmd6LPI = 15;
  cmd8LPI = 16;
  cmdCustomLPI = 17;
  cmdMax = 17; // command max

type
  TlrDMLineSpacing = (lsCustomLPI, ls6LPI, ls8LPI);

  { TlrDMConfig }

  TlrDMConfig = class(TPersistent)
  private
    fAutoNewPage: boolean;
    fAutoNewPageLines: integer;
    fLineSpacing: TlrDmLineSpacing;
    fLineSpacingCustomValue: integer;
  public
    ESCCode: array[0..cmdMax] of string;
    constructor Create;
    procedure AddingLineSpacingTo(aDestination: TStrings);
  published
    property AutoNewPage: boolean read fAutoNewPage write fAutoNewPage; // AutoNewPage false = inject
    property AutoNewPageLines: integer read fAutoNewPageLines write fAutoNewPageLines;
    property LineSpacing: TlrDmLineSpacing read fLineSpacing write fLineSpacing;
    property LineSpacingCustomValue: integer read fLineSpacingCustomValue write fLineSpacingCustomValue;
  end;

  { TlrDMFilter }

  TlrDMFilter = class(TfrExportFilter)
  private
    // Margins
    fMarginLeft: integer;
    fMarginTop: integer;
    fMarginRight: integer;
    fMarginBottom: integer;
    // Page client area
    fPageHeight: integer;
    fPageWidth: integer;
    // Lines
    fNumberOfLines: integer;
  protected
    procedure WriteStr(const aData: string); virtual; overload;
    procedure WriteStr(const aData: string; aAlignment: TAlignment; aNewLength: integer; aFillChar: char = ' '); virtual; overload;
    procedure WriteCmd(aData: integer; const aParameter: string = ''); virtual;
    function CheckView(aView: TfrView): boolean; override;
  public
    procedure OnBeginDoc; override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnEndDoc; override;
    procedure OnText(x, y: integer; const Text: string; View: TfrView); override;
  end;

var
  vlrDMConfig: TlrDMConfig;

implementation



function ConvertAccents(const aData: string): string;
var
  vChar, vCharPrevious: string;
begin
  Result := '';
  for vChar in aData do
  begin
    case vChar of
      #194:
      begin
        vCharPrevious := vChar;
        Continue;
      end;
      #195:
      begin
        vCharPrevious := vChar;
        Continue;
      end;
    end;
    case vCharPrevious of
      #194:
      begin
        case vChar of
          #186: Result += '*'; // º
          #170: Result += '*'; // ª
          else
            Result += vChar;
        end;
        vCharPrevious := #0;
      end;
      #195:
      begin
        case vChar of
          #160..#164: Result += 'a'; // áàãâä
          #128..#132: Result += 'A'; // ÁÀÃÂÄ
          #168..#171: Result += 'e'; // éèêë
          #136..#139: Result += 'E'; // ÉÈÊË
          #172..#175: Result += 'i'; // íìîï
          #140..#143: Result += 'I'; // ÍÌÎÏ
          #178..#182: Result += 'o'; // óòõôö
          #146..#150: Result += 'O'; // ÓÒÕÔÖ
          #185..#188: Result += 'u'; // úùûü
          #153..#156: Result += 'U'; // ÚÙÛÜ
          #167: Result += 'c'; // ç
          #135: Result += 'C'; // Ç
          else
            Result += vChar;
        end;
        vCharPrevious := #0;
      end;
      else
        Result += vChar;
    end;
  end;
end;

function PadR(const aData: string; aNewLength: integer; aFillChar: char): string;
var
  vLength: integer;
begin
  vLength := Length(aData);
  if (vLength < aNewLength) then
  begin
    Result := StringOfChar(aFillChar, aNewLength - vLength) + aData;
  end
  else
  begin
    Result := RightStr(aData, aNewLength);
  end;
end;

function PadL(const aData: string; aNewLength: integer; aFillChar: char): string;
var
  vLength: integer;
begin
  vLength := Length(aData);
  if (vLength < aNewLength) then
  begin
    Result := aData + StringOfChar(aFillChar, aNewLength - vLength);
  end
  else
  begin
    Result := LeftStr(aData, aNewLength);
  end;
end;

function PadC(const aData: string; aNewLength: integer; aFillChar: char): string;
var
  vLength: integer;
begin
  vLength := (aNewLength - Length(aData)) div 2;
  if (vLength > 0) then
  begin
    Result := StringOfChar(aFillChar, vLength) + aData + StringOfChar(aFillChar, vLength);
  end;
  // when length(Result) is an odd number
  Result := PadR(Result, aNewLength, aFillChar);
end;

{ TlrDMConfig }

constructor TlrDMConfig.Create;
begin
  //EPSON
  ESCCode[cmdReset] := #27#64;
  ESCCode[cmdCR] := #13;
  ESCCode[cmdLF] := #10;
  ESCCode[cmdFF] := #12;
  ESCCode[cmdSpace] := #32;
  ESCCode[cmdBoldON] := #27#69;
  ESCCode[cmdBoldOFF] := #27#70;
  ESCCode[cmdItalicsON] := #27#52;
  ESCCode[cmdItalicsOFF] := #27#53;
  ESCCode[cmdUnderlineON] := #27#45#1;
  ESCCode[cmdUnderlineOFF] := #27#45#0;
  ESCCode[cmdExpandedON] := #27#87#1;
  ESCCode[cmdExpandedOFF] := #27#87#0;
  ESCCode[cmd10CPI] := #27#80#18;
  ESCCode[cmd20CPI] := #27#77#15;
  ESCCode[cmd6LPI] := #27#48;
  ESCCode[cmd8LPI] := #27#50;
  ESCCode[cmdCustomLPI] := #27#65;
end;

procedure TlrDMConfig.AddingLineSpacingTo(aDestination: TStrings);
const
  vArray: array[TlrDMLineSpacing] of string = ('CustomLPI', '6LPI', '8LPI');
var
  vItem: string;
begin
  for vItem in vArray do
  begin
    aDestination.Add(vItem);
  end;
end;

{ TlrDMFilter }

procedure TlrDMFilter.WriteStr(const aData: string);
var
  Written: integer = 0;
begin
  Prn.Printer.Write(Pointer(aData)^, Length(aData), Written);
end;

procedure TlrDMFilter.WriteStr(const aData: string; aAlignment: TAlignment; aNewLength: integer; aFillChar: char);
begin
  case aAlignment of
    taCenter:
    begin
      WriteStr(PadC(ConvertAccents(aData), aNewLength, aFillChar));
    end;
    taLeftJustify:
    begin
      WriteStr(PadL(ConvertAccents(aData), aNewLength, aFillChar));
    end;
    taRightJustify:
    begin
      WriteStr(PadR(ConvertAccents(aData), aNewLength, aFillChar));
    end;
  end;
end;

procedure TlrDMFilter.WriteCmd(aData: integer; const aParameter: string);
begin
  WriteStr(vlrDMConfig.ESCCode[aData] + aParameter);
end;

function TlrDMFilter.CheckView(aView: TfrView): boolean;
begin
  { TODO -oheliosroots : gtLine }
  Result := aView.Typ in [gtMemo];
end;

procedure TlrDMFilter.OnBeginDoc;
begin
  Prn.Printer.RawMode := True;
  Prn.Printer.BeginDoc;
end;

procedure TlrDMFilter.OnBeginPage;
begin
  {%Region capturing metrics of the current page}
  // Margim
  fMarginBottom := CurPage.BottomMargin;
  fMarginLeft := CurPage.LeftMargin;
  fMarginRight := CurPage.RightMargin;
  fMarginTop := CurPage.TopMargin;

  // Page client area
  fPageHeight := (fMarginBottom - fMarginTop);
  fPageWidth := (fMarginRight - fMarginLeft);

  // Number of lines
  fNumberOfLines := round(fPageHeight / verticalDPI * standartLPI);
  {%EndRegion}

  // Clear buffer
  ClearLines;
  // Fill lines in buffer
  Lines.Count := fNumberOfLines;
end;

procedure TlrDMFilter.OnEndPage;

  function ConverFontSizeToCPI(aFontSize: integer): integer;
  begin
    if (aFontSize < 10) then
    begin
      Result := 20;
    end
    else
    if (aFontSize > 15) then
    begin
      Result := 5;
    end
    else
    begin
      Result := 10;
    end;
  end;

  procedure WriteHorizontalCompression(const aCPI: integer);
  begin
    if (aCPI = 5) then
    begin
      WriteCmd(cmdExpandedON);
      WriteCmd(cmd10CPI);
    end
    else
    if (aCPI = 20) then
    begin
      WriteCmd(cmdExpandedOFF);
      WriteCmd(cmd20CPI);
    end
    else
    begin
      WriteCmd(cmdExpandedOFF);
      WriteCmd(cmd10CPI);
    end;
  end;

  procedure WriteVerticalCompression(aLineSpacing: TlrDmLineSpacing; aLineSpacingCustomValue: integer);
  begin
    case aLineSpacing of
      ls6LPI:
      begin
        WriteCmd(cmd6LPI);
      end;
      ls8LPI:
      begin
        WriteCmd(cmd8LPI);
      end;
      lsCustomLPI:
      begin
        WriteCmd(cmdCustomLPI, Char(aLineSpacingCustomValue));
      end;
    end;
  end;

  procedure WriteCharacterDecoration(const aFontStyle: integer);
  begin
    // Italics
    if (aFontStyle and $1) <> 0 then
    begin
      WriteCmd(cmdItalicsON);
    end
    else
    begin
      WriteCmd(cmdItalicsOFF);
    end;
    // Bold
    if (aFontStyle and $2) <> 0 then
    begin
      WriteCmd(cmdBoldON);
    end
    else
    begin
      WriteCmd(cmdBoldOFF);
    end;
    // Underline
    if (aFontStyle and $4) <> 0 then
    begin
      WriteCmd(cmdUnderlineON);
    end
    else
    begin
      WriteCmd(cmdUnderlineOFF);
    end;
  end;

var
  vLoop, vCurrentCPI, vSpaceBetweenWords: integer;
  vTextRec: PfrTextRec = nil;
begin
  // Start(Reset) the printer
  WriteCmd(cmdReset);
  // Vertical compression
  WriteVerticalCompression(vlrDMConfig.LineSpacing, vlrDMConfig.LineSpacingCustomValue);
  //WriteCmd(cmdCustomLPI, char(16)); { TODO -oheliosroots : check line break }
  // Buffer
  for vLoop := 0 to (Lines.Count - 1) do
  begin
    vTextRec := PfrTextRec(Lines[vLoop]);
    vSpaceBetweenWords := 0;
    while vTextRec <> nil do // Words
    begin
      // remove left margim
      vTextRec^.X := vTextRec^.X - fMarginLeft;

      {%Region default}
      // Set default character decoration
      WriteCharacterDecoration(0);
      // Set default CPI
      vCurrentCPI := ConverFontSizeToCPI(10);
      // Set default horizonal compression
      WriteHorizontalCompression(vCurrentCPI);
      // Write space between words
      WriteStr(' ', taLeftJustify, Round((vTextRec^.X - vSpaceBetweenWords) / horizontalDPI * vCurrentCPI));
      // Recalculate space between words
      vSpaceBetweenWords := (vTextRec^.X + vTextRec^.W);
      {%EndRegion}

      {%Region}
      // Set character decoration
      WriteCharacterDecoration(vTextRec^.FontStyle);
      // Get current CPI
      vCurrentCPI := ConverFontSizeToCPI(vTextRec^.FontSize);
      // Set horizonal compression
      WriteHorizontalCompression(vCurrentCPI);
      // Write word
      WriteStr(vTextRec^.Text, vTextRec^.Alignment, round((vTextRec^.W / horizontalDPI) * vCurrentCPI));
      {%EndRegion}

      // Next word
      vTextRec := vTextRec^.Next;
    end;
    // Advances one line
    WriteCmd(cmdCR);
    WriteCmd(cmdLF);
  end;
  // Auto new page
  if (vlrDMConfig.AutoNewPage) then
  begin
    // Hop on the perforation
    for vLoop := 1 to vlrDMConfig.AutoNewPageLines do
    begin
      WriteCmd(cmdCR);
      WriteCmd(cmdLF);
    end;
  end
  else
  begin
    // Advances(Inject) one page
    WriteCmd(cmdFF);
  end;
end;

procedure TlrDMFilter.OnEndDoc;
begin
  Prn.Printer.EndDoc;
  Prn.Printer.RawMode := False;
end;

procedure TlrDMFilter.OnText(x, y: integer; const Text: string; View: TfrView);
var
  vTextRec: PfrTextRec = nil;
  vRow: integer;
begin
  // Area restrictions
  if (View = nil) or (y < fMarginTop) or (y > fMarginBottom) or
    (x < fMarginLeft) or (x + round(View.Width) > fMarginRight) then
  begin
    Exit;
  end;
  // Row position
  vRow := round(((y - fMarginTop) / verticalDPI) * standartLPI);
  // Row restrictions
  if (vRow < 0) or (vRow > fNumberOfLines) then
  begin
    Exit;
  end;
  // Add row in buffer
  NewRec(View, Text, vTextRec);
  if (View is TfrMemoView) then // Bug in procedure NewRec: ===>> {Alignment := Alignment}
  begin
    vTextRec^.Alignment := (View as TfrMemoView).Alignment;
  end;
  AddRec(vRow, vTextRec);
end;

initialization
  vlrDMConfig := TlrDMConfig.Create;
  vlrDMConfig.AutoNewPage := True;
  vlrDMConfig.AutoNewPageLines := 0;
  vlrDMConfig.LineSpacing := lsCustomLPI;
  vlrDMConfig.LineSpacingCustomValue := 14;

finalization
  vlrDMConfig.Free;

end.
