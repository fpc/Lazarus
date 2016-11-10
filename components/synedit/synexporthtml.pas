{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynExportHTML.pas, released 2000-04-16.

The Original Code is partly based on the mwHTMLExport.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
Portions created by James D. Jacobson are Copyright 1999 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynExportHTML;

{$I synedit.inc}

interface

uses
  Classes,
  LCLIntf, LCLType, Graphics, ClipBrd,
  SynEditHighlighter, SynEditExport, LCLProc, LazUtf8;

type
  THTMLFontSize = (fs01, fs02, fs03, fs04, fs05, fs06, fs07, fsDefault);        //eb 2000-10-12

  TExportHtmlOption = (
    heoFragmentOnly, //no surrounding <html><body>...</body><html> Note: will exclude heoDoctype, heoCharset
    heoDoctype,      //add doctype declaration
    heoCharset,      //add charset (UTF-8) information
    heoWinClipHeader //add Clipboard header (affects Windows only) Note: cannot be set if ExportAsText = True!
  );
  TExportHtmlOptions = set of TExportHtmlOption;


  { TSynExporterHTML }

  TSynExporterHTML = class(TSynCustomExporter)
  private
    fOptions: TExportHtmlOptions;
    fFontSize: THTMLFontSize;
    procedure SetExportHtmlOptions(Value: TExportHtmlOptions);
    function GetCreateHTMLFragment: Boolean;
    procedure SetCreateHTMLFragment(Value: Boolean);
    function MakeFontSpan(FG, BG: TColor): String;
    function StyleToHtml(AStyle: TFontStyles; IsSpace, DoSet: Boolean): String;
  protected
    procedure FormatAfterLastAttribute; override;
    procedure FormatAttributeDone(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
    procedure FormatAttributeInit(BackgroundChanged, ForegroundChanged: boolean;
      FontStylesChanged: TFontStyles); override;
{begin}                                                                         //mh 2000-10-10
    procedure FormatBeforeFirstAttribute(BackgroundChanged,
      ForegroundChanged: boolean; FontStylesChanged: TFontStyles); override;
{end}                                                                           //mh 2000-10-10
    procedure FormatBeforeFirstAttributeImmediate(BG, FG: TColor); override;
    procedure FormatAfterLastAttributeImmediate; override;
    procedure FormatAttributeInitImmediate(Attri: TSynHighlighterAttributes; IsSpace: Boolean); override;
    procedure FormatAttributeDoneImmediate(Attri: TSynHighlighterAttributes; IsSpace: Boolean); override;

    procedure FormatNewLine; override;
    function GetFooter: string; override;
    function GetFormatName: string; override;
    function GetHeader: string; override;
    procedure SetExportAsText(Value: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    function ColorToHTML(AColor: TColor): string;
  published
    property Color;
    property CreateHTMLFragment: boolean read GetCreateHTMLFragment
      write SetCreateHTMLFragment default FALSE; deprecated 'Use Options instead';
    property DefaultFilter;
    property Options: TExportHtmlOptions read fOptions write SetExportHtmlOptions default [heoDoctype, heoCharset];
    property Font;
    property Highlighter;
    property HTMLFontSize: THTMLFontSize read fFontSize write fFontSize;        //eb 2000-10-12
    property Title;
    property UseBackground;
  end;

implementation

uses
  SysUtils,
  SynEditStrConst;

const
  DocType = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"' + LineEnding +
            '"http://www.w3.org/TR/html4/loose.dtd">';  //cannot use strict, because we use <font> tag
  Generator = '<meta name="generator" content="Lazarus SynEdit Html Exporter">';
  CharSet = '<meta http-equiv="content-type" content="text/html; charset=utf-8">';
  DocumentStart = '<html>'+LineEnding+
                  '<head>'+LineEnding+
                  '%s'+LineEnding+
                  '</head>'+LineEnding+
                  '<body text=%s bgcolor=%s>';
  DocumentEnd = '</body>'+LineEnding+'</html>';
  CodeStart = '<pre><code>';
  CodeEnd = '</code></pre>';
  FontStart = '<font %s face="%s">';
  FontEnd = '</font>';
  WinClipHeaderFmt = 'Version:0.9' + LineEnding +
                     'StartHTML:%.8d' + LineEnding +
                     'EndHTML:%.8d' + LineEnding +
                     'StartFragment:%.8d' + LineEnding +
                     'EndFragment:%.8d' + LineEnding;

  StartFragmentComment = '<!--StartFragment-->';
  EndFragmentComment = '<!--EndFragment-->';

{ TSynExporterHTML }

constructor TSynExporterHTML.Create(AOwner: TComponent);
const
  HTML_Format = {$ifdef windows}'HTML Format'{$else}'text/html'{$endif};
begin
  inherited Create(AOwner);
  {**************}
  fClipboardFormat := RegisterClipboardFormat(HTML_Format);
  fFontSize := fs03;
  fOptions := [heoDocType, heoCharset];
  fDefaultFilter := SYNS_FilterHTML;
  // setup array of chars to be replaced
  fReplaceReserved['&'] := '&amp;';
  fReplaceReserved['<'] := '&lt;';
  fReplaceReserved['>'] := '&gt;';
  //fReplaceReserved['"'] := '&quot;';   //no need to replace this
  //fReplaceReserved[''''] := '&apos;';  //no need to replace this
{ The following characters are multi-byte in UTF-8:
  fReplaceReserved['™'] := '&trade;';
  fReplaceReserved['©'] := '&copy;';
  fReplaceReserved['®'] := '&reg;';
  fReplaceReserved['À'] := '&Agrave;';
  fReplaceReserved['Á'] := '&Aacute;';
  fReplaceReserved['Â'] := '&Acirc;';
  fReplaceReserved['Ã'] := '&Atilde;';
  fReplaceReserved['Ä'] := '&Auml;';
  fReplaceReserved['Å'] := '&Aring;';
  fReplaceReserved['Æ'] := '&AElig;';
  fReplaceReserved['Ç'] := '&Ccedil;';
  fReplaceReserved['È'] := '&Egrave;';
  fReplaceReserved['É'] := '&Eacute;';
  fReplaceReserved['Ê'] := '&Ecirc;';
  fReplaceReserved['Ë'] := '&Euml;';
  fReplaceReserved['Ì'] := '&Igrave;';
  fReplaceReserved['Í'] := '&Iacute;';
  fReplaceReserved['Î'] := '&Icirc;';
  fReplaceReserved['Ï'] := '&Iuml;';
  fReplaceReserved['Ð'] := '&ETH;';
  fReplaceReserved['Ñ'] := '&Ntilde;';
  fReplaceReserved['Ò'] := '&Ograve;';
  fReplaceReserved['Ó'] := '&Oacute;';
  fReplaceReserved['Ô'] := '&Ocirc;';
  fReplaceReserved['Õ'] := '&Otilde;';
  fReplaceReserved['Ö'] := '&Ouml;';
  fReplaceReserved['Ø'] := '&Oslash;';
  fReplaceReserved['Ù'] := '&Ugrave;';
  fReplaceReserved['Ú'] := '&Uacute;';
  fReplaceReserved['Û'] := '&Ucirc;';
  fReplaceReserved['Ü'] := '&Uuml;';
  fReplaceReserved['Ý'] := '&Yacute;';
  fReplaceReserved['Þ'] := '&THORN;';
  fReplaceReserved['ß'] := '&szlig;';
  fReplaceReserved['à'] := '&agrave;';
  fReplaceReserved['á'] := '&aacute;';
  fReplaceReserved['â'] := '&acirc;';
  fReplaceReserved['ã'] := '&atilde;';
  fReplaceReserved['ä'] := '&auml;';
  fReplaceReserved['å'] := '&aring;';
  fReplaceReserved['æ'] := '&aelig;';
  fReplaceReserved['ç'] := '&ccedil;';
  fReplaceReserved['è'] := '&egrave;';
  fReplaceReserved['é'] := '&eacute;';
  fReplaceReserved['ê'] := '&ecirc;';
  fReplaceReserved['ë'] := '&euml;';
  fReplaceReserved['ì'] := '&igrave;';
  fReplaceReserved['í'] := '&iacute;';
  fReplaceReserved['î'] := '&icirc;';
  fReplaceReserved['ï'] := '&iuml;';
  fReplaceReserved['ð'] := '&eth;';
  fReplaceReserved['ñ'] := '&ntilde;';
  fReplaceReserved['ò'] := '&ograve;';
  fReplaceReserved['ó'] := '&oacute;';
  fReplaceReserved['ô'] := '&ocirc;';
  fReplaceReserved['õ'] := '&otilde;';
  fReplaceReserved['ö'] := '&ouml;';
  fReplaceReserved['ø'] := '&oslash;';
  fReplaceReserved['ù'] := '&ugrave;';
  fReplaceReserved['ú'] := '&uacute;';
  fReplaceReserved['û'] := '&ucirc;';
  fReplaceReserved['ü'] := '&uuml;';
  fReplaceReserved['ý'] := '&yacute;';
  fReplaceReserved['þ'] := '&thorn;';
  fReplaceReserved['ÿ'] := '&yuml;';
  fReplaceReserved['¡'] := '&iexcl;';
  fReplaceReserved['¢'] := '&cent;';
  fReplaceReserved['£'] := '&pound;';
  fReplaceReserved['¤'] := '&curren;';
  fReplaceReserved['¥'] := '&yen;';
  fReplaceReserved['¦'] := '&brvbar;';
  fReplaceReserved['§'] := '&sect;';
  fReplaceReserved['¨'] := '&uml;';
  fReplaceReserved['ª'] := '&ordf;';
  fReplaceReserved['«'] := '&laquo;';
  fReplaceReserved['¬'] := '&shy;';
  fReplaceReserved['¯'] := '&macr;';
  fReplaceReserved['°'] := '&deg;';
  fReplaceReserved['±'] := '&plusmn;';
  fReplaceReserved['²'] := '&sup2;';
  fReplaceReserved['³'] := '&sup3;';
  fReplaceReserved['´'] := '&acute;';
  fReplaceReserved['µ'] := '&micro;';
  fReplaceReserved['·'] := '&middot;';
  fReplaceReserved['¸'] := '&cedil;';
  fReplaceReserved['¹'] := '&sup1;';
  fReplaceReserved['º'] := '&ordm;';
  fReplaceReserved['»'] := '&raquo;';
  fReplaceReserved['¼'] := '&frac14;';
  fReplaceReserved['½'] := '&frac12;';
  fReplaceReserved['¾'] := '&frac34;';
  fReplaceReserved['¿'] := '&iquest;';
  fReplaceReserved['×'] := '&times;';
  fReplaceReserved['÷'] := '&divide';
  fReplaceReserved['€'] := '&euro;';}
end;

function TSynExporterHTML.ColorToHTML(AColor: TColor): string;
var
  RGBColor: TColorRef;
  RGBValue: byte;
const
  Digits: array[0..15] of char = '0123456789ABCDEF';
begin
  Result := '';
  case AColor of
    clRed:     Result := 'red';
    clGreen:   Result := 'green';
    clBlue:    Result := 'blue';
    clPurple:  Result := 'purple';
    clYellow:  Result := 'yellow';
    clBlack:   Result := 'black';
    clWhite:   Result := 'white';
    clGray:    Result := 'gray';
    clMaroon:  Result := 'maroon';
    clFuchsia: Result := 'fuchsia';
    clLime:    Result := 'lime';
    clNavy:    Result := 'navy';
    clAqua:    Result := 'aqua';
    clTeal:    Result := 'teal';
    clSilver:  Result := 'silver';
  end;
  if (Result <> '') then
    Exit;
  RGBColor := ColorToRGB(AColor);
  Result := '"#000000"';
 {****************}
  RGBValue := GetRValue(RGBColor);
  if RGBValue > 0 then begin
    Result[3] := Digits[RGBValue shr  4];
    Result[4] := Digits[RGBValue and 15];
  end;
 {****************}
  RGBValue := GetGValue(RGBColor);
  if RGBValue > 0 then begin
    Result[5] := Digits[RGBValue shr  4];
    Result[6] := Digits[RGBValue and 15];
  end;
 {****************}
  RGBValue := GetBValue(RGBColor);
  if RGBValue > 0 then begin
    Result[7] := Digits[RGBValue shr  4];
    Result[8] := Digits[RGBValue and 15];
  end;
end;

procedure TSynExporterHTML.FormatAfterLastAttribute;
begin
  if fsStrikeout in fLastStyle then
    AddData('</strike>');
  if fsUnderline in fLastStyle then
    AddData('</u>');
  if fsItalic in fLastStyle then
    AddData('</i>');
  if fsBold in fLastStyle then
    AddData('</b>');
  if fLastFG <> fFont.Color then                                         
    AddData('</font>');
  if UseBackground and (fLastBG <> fBackgroundColor) then
    AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeDone(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
  if BackgroundChanged or ForegroundChanged or (FontStylesChanged <> []) then
  begin
    if fsStrikeout in fLastStyle then
      AddData('</strike>');
    if fsUnderline in fLastStyle then
      AddData('</u>');
    if fsItalic in fLastStyle then
      AddData('</i>');
    if fsBold in fLastStyle then
      AddData('</b>');
  end;
  if (BackgroundChanged or ForegroundChanged) and (fLastFG <> fFont.Color) then //mh 2000-10-10
    AddData('</font>');
  if BackgroundChanged then
    AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeInit(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
  if BackgroundChanged then
    AddData('<span style="background-color: ' +
      AnsiDequotedStr(ColorToHtml(fLastBG), '"') {Copy(ColorToHtml(fLastBG), 2, 9)} + '>');
  if (BackgroundChanged or ForegroundChanged) and (fLastFG <> fFont.Color) then
    AddData('<font color=' + ColorToHtml(fLastFG) + '>');
  if BackgroundChanged or ForegroundChanged or (FontStylesChanged <> []) then
  begin
    if fsBold in fLastStyle then
      AddData('<b>');
    if fsItalic in fLastStyle then
      AddData('<i>');
    if fsUnderline in fLastStyle then
      AddData('<u>');
    if fsStrikeout in fLastStyle then
      AddData('<strike>');
  end;
end;

{begin}                                                                         //mh 2000-10-10
procedure TSynExporterHTML.FormatBeforeFirstAttribute(BackgroundChanged,
  ForegroundChanged: boolean; FontStylesChanged: TFontStyles);
begin
  if BackgroundChanged then
    AddData('<span style="background-color: ' +
      AnsiDequotedStr(ColorToHtml(fLastBG), '"') {Copy(ColorToHtml(fLastBG), 2, 9)} + '>');
  AddData('<font color=' + ColorToHtml(fLastFG) + '>');
  if FontStylesChanged <> [] then begin
    if fsBold in fLastStyle then
      AddData('<b>');
    if fsItalic in fLastStyle then
      AddData('<i>');
    if fsUnderline in fLastStyle then
      AddData('<u>');
    if fsStrikeout in fLastStyle then
      AddData('<strike>');
  end;
end;

procedure TSynExporterHTML.FormatBeforeFirstAttributeImmediate(BG, FG: TColor);
var
  Span: String;
begin
  debugln(['TSynExporterHTML.FormatBeforeFirstAttributeImmediate']);
  // if not heoFragmentOnly this is handled in GetHeader
  if (heoFragmentOnly in Options) then
  begin
    Span := MakeFontSpan(FG, BG);
    AddData(Span);
  end;
end;

procedure TSynExporterHTML.FormatAfterLastAttributeImmediate;
begin
  debugln(['TSynExporterHTML.FormatAfterLastAttributeImmediate']);
  if (heoFragmentOnly in Options) then
    AddData('</span>');
end;

procedure TSynExporterHTML.FormatAttributeInitImmediate(
  Attri: TSynHighlighterAttributes; IsSpace: Boolean);
var
  Span, StyleStr: String;
  FG, BG: TColor;
begin
  debugln(['TSynExporterHTML.FormatAttributeInitImmediate']);
  FG := ValidatedColor(Attri.Foreground, fFont.Color);
  BG := ValidatedColor(Attri.Background, fBackgroundColor);
  if (not IsSpace and (FG <> fFont.Color)) or
     (UseBackGround and (BG <> fbackGroundColor)) then
  begin
    Span := MakeFontSpan(FG, BG);
    AddData(Span);
  end;
  if (Attri.Style <> []) then
  begin
    StyleStr := StyleToHtml(Attri.Style, IsSpace, True);
    AddData(StyleStr);
  end;
end;

procedure TSynExporterHTML.FormatAttributeDoneImmediate(
  Attri: TSynHighlighterAttributes; IsSpace: Boolean);
var
  FG, BG: TColor;
  StyleStr: String;
begin
  debugln(['TSynExporterHTML.FormatAttributeDoneImmediate']);
  //reversed order compared to FormatAttributeInitImmediate
  if (Attri.Style <> []) then
  begin
    StyleStr := StyleToHtml(Attri.Style, IsSpace, False);
    AddData(StyleStr);
  end;
  FG := ValidatedColor(Attri.Foreground, fFont.Color);
  BG := ValidatedColor(Attri.Background, fBackgroundColor);
  if (not IsSpace and (FG <> fFont.Color)) or
     (UseBackGround and (BG <> fbackGroundColor)) then
  begin
    AddData('</span>');
  end;

end;

{end}                                                                           //mh 2000-10-10

procedure TSynExporterHTML.FormatNewLine;
begin
  AddNewLine;
end;

function TSynExporterHTML.GetFooter: string;
begin
  Result := FontEnd + LineEnding + CodeEnd;
  if (heoWinClipHeader in Options) then
    Result := Result + EndFragmentComment;

  if not (heoFragmentOnly in Options) then
  begin
    if (Result <> '') then Result := Result + LineEnding;
    Result := Result + DocumentEnd;
  end;
end;

function TSynExporterHTML.GetFormatName: string;
begin
  Result := SYNS_ExporterFormatHTML;
end;

function TSynExporterHTML.GetHeader: string;
var
  sFontSize: string;                                                            //eb 2000-10-12
  DocHeader, HeadText, WinClipHeader, SFooter: String;
  WinClipHeaderSize, FooterLen: Integer;
begin
  Result := '';
  DocHeader := '';
  if not (heoFragmentOnly in Options) then
  begin
    if (heoDocType in fOptions) then
      DocHeader := DocHeader + DocType + LineEnding;
    HeadText := Generator;
    if (heoCharSet in fOptions) then
      HeadText := HeadText + LineEnding + CharSet;
    HeadText := HeadText + LineEnding + Format('<title>%s</title>',[Title]);
    DocHeader := DocHeader + Format(DocumentStart,[HeadText,ColorToHtml(fFont.Color),ColorToHTML(fBackgroundColor)]);
    if (heoWinClipHeader in fOptions) then
      DocHeader := DocHeader + LineEnding + StartFragmentComment;
    DocHeader := DocHeader + CodeStart; //Don't add LineEndings after this point, because of <pre> tag
  end  //not heoFragmentOnly
  else
  begin
    if (heoWinClipHeader in fOptions) then
      DocHeader := DocHeader + StartFragmentComment + CodeStart
    else
      DocHeader := DocHeader + CodeStart;
  end;
  if fFontSize <> fsDefault then
    sFontSize := Format(' size=%d', [1 + Ord(fFontSize)])
  else
    sFontSize := '';
  DocHeader := DocHeader + Format(FontStart,[sFontSize, fFont.Name]);

  if (heoWinClipHeader in fOptions) then
  begin
    WinClipHeaderSize := Length(Format(WinClipHeaderFmt,[0,0,0,0]));
    SFooter := GetFooter;
    FooterLen := Length(SFooter);

    //debugln(['TSynExporterHtml.GetHeader: WinClipHeaderSize=',WinClipHeadersize]);
    //debugln(['  Footer="',Sfooter,'"']);
    //debugln(['  FooterLen=',FooterLen]);
    //debugln(['  BufferSize=',getBufferSize]);
    //debugln(['  length(docHeader)=',length(docheader)]);

    // Described in http://msdn.microsoft.com/library/sdkdoc/htmlclip/htmlclipboard.htm
    WinClipHeader := Format(WinClipHeaderFmt,
      [WinClipHeaderSize,  //HtmlStart
       WinClipHeaderSize + Length(DocHeader) + FooterLen + GetBufferSize - 1, //HtmlEnd
       WinClipHeaderSize + Utf8Pos(StartFragmentComment, DocHeader) + Length(StartfragmentComment) - 1, //StartFragment
       WinClipHeaderSize + Length(DocHeader) + Utf8Pos(EndFragmentComment, SFooter) + GetBufferSize - 1  //EndFragment
      ]);
      DocHeader := WinClipHeader + DocHeader;
  end;

  Result := DocHeader;
end;

procedure TSynExporterHTML.SetExportAsText(Value: boolean);
begin
  if (Value <> ExportAsText) then
  begin
    inherited SetExportAsText(Value);
    if Value then
      fOptions := fOptions - [heoWinClipHeader];
  end;
end;

procedure TSynExporterHTML.SetExportHtmlOptions(Value: TExportHtmlOptions);
begin
  if (fOptions <> Value) then
  begin
    Clear;
    fOptions := Value;
    if ExportAsText then fOptions := fOptions - [heoWinClipHeader];
    if (heoFragmentOnly in Value) then
    begin
      fOptions := fOptions - [heoDoctype, heoCharSet];
    end;
  end;
end;

function TSynExporterHTML.GetCreateHTMLFragment: Boolean;
begin
  Result := (heoFragmentOnly in fOptions);
end;

procedure TSynExporterHTML.SetCreateHTMLFragment(Value: Boolean);
begin
  if (GetCreateHTMLFragment <> Value) then
  begin
    if Value then
      Options := Options + [heoFragmentOnly]
    else
      Options := Options - [heoFragmentOnly];
  end;
end;

function TSynExporterHTML.MakeFontSpan(FG, BG: TColor): String;
begin
  Result := '<span style="color: ';
  FG := ValidatedColor(FG, fFont.Color);
  Result := Result + AnsiDequotedStr(ColorToHtml(FG), '"');
  BG := ValidatedColor(BG, fBackgroundColor);
  if UseBackGround then
  begin
    Result := Result + '; background-color: ' + AnsiDequotedStr(ColorToHtml(BG), '"');
  end;
  Result := Result + ';">';
end;

function TSynExporterHTML.StyleToHtml(AStyle: TFontStyles; IsSpace, DoSet: Boolean): String;
begin
  Result := '';
  if not IsSpace then
  begin
    if (fsBold in AStyle) then
      if DoSet then Result := Result + '<b>' else Result := Result + '</b>';
    if (fsItalic in AStyle) then
      if DoSet then Result := Result + '<i>' else Result := Result + '</i>';
    if (fsUnderline in AStyle) then
      if DoSet then Result := Result + '<u>' else Result := Result + '</u>';
  end;
  //the only style that actually is applied to whitespace in HTML
  if (fsStrikeOut in AStyle) then
    if DoSet then Result := Result + '<strike>' else Result := Result + '</strike>';
end;

end.

