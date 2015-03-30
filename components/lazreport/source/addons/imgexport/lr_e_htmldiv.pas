{*****

  Copyright (c) 2012 Michał Gawrycki (michal.gawrycki(a.t.)gmsystems.pl
  License: modified LGPL (see 'COPYING.modifiedLGPL.txt' in Lazarus directory)

*****}

unit LR_e_htmldiv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, Graphics;

type
  TfrHtmlDivExport = class(TComponent)

  end;

  { TfrHtmlDivExportFilter }

  TfrHtmlDivExportFilter = class(TfrExportFilter)
  private
    FCurPage: integer;
    FImgCnt: Integer;
    FPageStyle: String;
    FExportImages: Boolean;
    FEmbeddedImages: Boolean;
    procedure WriteString(AValue: string);
  public
    constructor Create(AStream: TStream); override;
    procedure OnBeginDoc; override;
    procedure OnEndDoc; override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnData(x, y: integer; View: TfrView); override;
    procedure OnText(x, y: integer; const Text: string; View: TfrView); override;
    property PageStyle: String read FPageStyle write FPageStyle;
    property ExportImages: Boolean read FExportImages write FExportImages;
    property EmbeddedImages: Boolean read FEmbeddedImages write FEmbeddedImages;
  end;

implementation

uses
  base64, LR_BarC;

const
  HTML_REPORT_HEADER = '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">'
    + LineEnding + '<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">'
    + LineEnding + '<head>' + LineEnding +
    '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>' +
    LineEnding + '<title>%s</title>' + LineEnding + '<style type="text/css">' +
    LineEnding + '.page {position: relative;%s}' + LineEnding + '.fV {position: absolute;}'
    + LineEnding + '</style>' + LineEnding + '</head>' + LineEnding + '<body>' + LineEnding;
  HTML_REPORT_END = '</body></html>';
  HTML_PAGE_START =
    '<div class="page" style="width:%dpx;min-width:%0:dpx;height:%dpx;min-height:%1:dpx;">'
    + LineEnding;
  HTML_PAGE_END = '</div>' + LineEnding;
  HTML_BOX = '<div class="fV" style="%s"></div>' + LineEnding;
  HTML_IMG1 = '<img class="fV" style="%s" src="';
  HTML_IMG2 = ' " />' + LineEnding;
  HTML_TEXT = '<div class="fV" style="%s">%s</div>' + LineEnding;

type

  { TChunkStream }

  TChunkStream = class(TOwnerStream)
  private
    FPos: Integer;
    FChunkSize: Integer;
    FSeparator: String;
  public
    constructor Create(ASource: TStream);
    function Write(const Buffer; Count: Longint): Longint; override;
    property ChunkSize: Integer read FChunkSize write FChunkSize;
    property Separator: String read FSeparator write FSeparator;
  end;

{ TChunkStream }

constructor TChunkStream.Create(ASource: TStream);
begin
  inherited Create(ASource);
  FPos := 0;
  FSeparator := LineEnding;
  FChunkSize := 79;
end;

function TChunkStream.Write(const Buffer; Count: Longint): Longint;
var
  I: Integer;
  J,K: Integer;
begin
  Result := Count;
  I := 0;
  if FPos > 0 then
  begin
    I := FChunkSize - FPos;
    if I > Count then
    begin
      Source.Write(Buffer, Count);
      FPos := FPos + Count;
      Exit;
    end
    else
    begin
      Source.Write(Buffer, I);
      Source.Write(FSeparator[1], Length(FSeparator));
      FPos := 0;
    end;
  end;
  if ((Count - I) > FChunkSize) then
    J := (Count - I) div FChunkSize
  else
  begin
    Source.Write(PChar(@Buffer)[I], Count - I);
    FPos := (Count - I);
    Exit;
  end;
  for K := 0 to J - 1 do
  begin
    Source.Write(PChar(@Buffer)[(K * FChunkSize)], FChunkSize);
    Source.WriteBuffer(FSeparator[1], Length(FSeparator));
  end;
  J := (Count - I) mod FChunkSize;
  if J > 0 then
  begin
    Source.Write(PChar(@Buffer)[((K + 1) * FChunkSize)], J);
    FPos := J;
  end;
end;

{ TfrHtmlDivExportFilter }

function ColorToCSS(AColor: TColor): string;
begin
  Result := Format('#%.2x%.2x%.2x', [Red(AColor), Green(AColor), Blue(AColor)]);
end;

function SizeToCSS(X, Y, W, H: integer): string;
begin
  Result := Format(
    'width:%dpx;min-width:%0:dpx;height:%dpx;min-height:%1:dpx;left:%dpx;top:%dpx;',
    [W, H, X, Y]);
end;

procedure TfrHtmlDivExportFilter.WriteString(AValue: string);
begin
  Stream.Write(AValue[1], Length(AValue));
end;

constructor TfrHtmlDivExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FCurPage := 0;
  FImgCnt := 0;
  FPageStyle := '';
  FExportImages := True;
  FEmbeddedImages := True;
end;

procedure TfrHtmlDivExportFilter.OnBeginDoc;
begin
  WriteString(Format(HTML_REPORT_HEADER, [CurReport.Title, FPageStyle]));
end;

procedure TfrHtmlDivExportFilter.OnEndDoc;
begin
  WriteString(HTML_REPORT_END);
end;

procedure TfrHtmlDivExportFilter.OnBeginPage;
begin
  Inc(FCurPage);
  WriteString(Format(HTML_PAGE_START, [CurReport.EMFPages[FCurPage - 1]^.PrnInfo.Pgw,
    CurReport.EMFPages[FCurPage - 1]^.PrnInfo.Pgh]));
end;

procedure TfrHtmlDivExportFilter.OnEndPage;
begin
  WriteString(HTML_PAGE_END);
end;

procedure TfrHtmlDivExportFilter.OnData(x, y: integer; View: TfrView);

  function BorderStyleToCSS: string;
  begin
    case View.FrameStyle of
      frsSolid, frsDouble: Result := 'solid';
      frsDash, frsDashDot, frsDashDotDot: Result := 'dashed';
      frsDot: Result := 'dotted';
    end;
  end;

var
  W, H: integer;
  BrdW: integer;
  BLeft, BTop, BRight, BBottom: integer;
  St: string;
  B64: TBase64EncodingStream;
  Png: TPortableNetworkGraphic;
  BCBmp: TBitmap;
  CS: TChunkStream;
begin
  W := View.dx;
  H := View.dy;

  BrdW := Round(View.FrameWidth);

  if frbLeft in View.Frames then
    BLeft := BrdW
  else
    BLeft := 0;
  if frbTop in View.Frames then
    BTop := BrdW
  else
    BTop := 0;
  if frbRight in View.Frames then
    BRight := BrdW
  else
    BRight := 0;
  if frbBottom in View.Frames then
    BBottom := BrdW
  else
    BBottom := 0;

  if BLeft > 0 then
    Dec(W, BrdW);
  if BTop > 0 then
    Dec(H, BrdW);

  St := SizeToCSS(X, Y, W, H);

  St := St + Format('border-left:%0:dpx %4:s %5:s;border-top:%1:dpx %4:s %5:s;' +
    'border-right:%2:dpx %4:s %5:s;border-bottom:%3:dpx %4:s %5:s;',
    [BLeft, BTop, BRight, BBottom, BorderStyleToCSS, ColorToCSS(View.FrameColor)]);

  if View.FillColor <> clNone then
    St := St + 'background-color:' + ColorToCSS(View.FillColor) + ';';

  if ExportImages and ((View is TfrPictureView) or (View is TfrCustomBarCodeView)) then
  begin
    WriteString(Format(HTML_IMG1, [St]));
    Inc(FImgCnt);
    if EmbeddedImages then
      WriteString('data:image/png;base64,')
    else
      WriteString(ExtractFileName(TFileStream(Stream).FileName) + '_image_' + IntToStr(FImgCnt) + '.png');
    Png := TPortableNetworkGraphic.Create;
    if EmbeddedImages then
    begin
      CS := TChunkStream.Create(Stream);
      B64 := TBase64EncodingStream.Create(CS);
    end;
    if View is TfrCustomBarCodeView then
    begin
      BCBmp := TfrCustomBarCodeView(View).GenerateBitmap;
      Png.Assign(BCBmp);
      BCBmp.Free;
    end
    else
      if View is TfrPictureView then
      begin
        Png.SetSize(View.dx, View.dy);
        if TfrPictureView(View).Stretched then
          Png.Canvas.StretchDraw(Rect(0, 0, View.dx, View.dy), TfrPictureView(View).Picture.Graphic)
        else
          Png.Canvas.Draw(0, 0, TfrPictureView(View).Picture.Graphic);
      end;
    if EmbeddedImages then
    begin
      Png.SaveToStream(B64);
      B64.Flush;
      B64.Free;
      CS.Free;
    end
    else
      Png.SaveToFile(TFileStream(Stream).FileName + '_image_' + IntToStr(FImgCnt) + '.png');
    Png.Free;
    WriteString(HTML_IMG2);
  end
  else
    WriteString(Format(HTML_BOX, [St]));
end;

procedure TfrHtmlDivExportFilter.OnText(x, y: integer; const Text: string; View: TfrView);
var
  St: string;
begin
  if Trim(Text) = '' then
    Exit;
  St := SizeToCSS(X, Y, View.dx, View.dy);
  if View is TfrMemoView then
  begin
    St := St + 'font-family:''' + TfrMemoView(View).Font.Name +
      ''';font-size:' + IntToStr(TfrMemoView(View).Font.Size) +
      'pt;color:' + ColorToCSS(TfrMemoView(View).Font.Color) + ';';
    if fsBold in TfrMemoView(View).Font.Style then
      St := St + 'font-weight:bold;';
    if fsItalic in TfrMemoView(View).Font.Style then
      St := St + 'font-style:italic;';
    if fsUnderline in TfrMemoView(View).Font.Style then
      St := St + 'text-decoration:underline;';
    St := St + 'text-align:';
    case TfrMemoView(View).Alignment of
      taLeftJustify: St := St + 'left;';
      taCenter: St := St + 'center;';
      taRightJustify: St := St + 'right;';
    end;
  end;
  WriteString(Format(HTML_TEXT, [St, Text]));
end;

initialization
  frRegisterExportFilter(TfrHtmlDivExportFilter, 'HTML (div-based)  (*.html)', '*.html');

end.
