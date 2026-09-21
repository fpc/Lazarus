{
  Author: Mattias Gaertner

  Abstract:
    TSVGGraphic - a TGraphic that reads, keeps, renders and writes SVG,
    using the fcl-svg units.

    The parsed document is kept in the Document property, so the tree can be
    examined and edited, and written back out with SaveToStream.

    Width and Height are the intrinsic size the document asks for, in pixels:
    the width and height attributes of the root svg element resolved at DPI,
    the extent of its viewBox when those are absent, and 300x150 when there is
    neither. Setting them writes the attributes back into the root element.

    An unmodified document is written back byte for byte, so reading and
    writing a file leaves it untouched. Once something changed, the tree is
    serialized instead. The reader does not keep comments, processing
    instructions, namespace prefixes, CDATA sections or the DOCTYPE, so these
    are lost by the serializing path. Edits made through the Document property
    are invisible to the Modified flag, so call SVGChanged after them.
}
unit SVGImg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage, Graphics, GraphType, IntfGraphics, ClipBrd,
  fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.soft,
  fpsvg.fonts.provider;

const
  SVGMimeType = 'image/svg+xml';
  SVGFileExtensions = 'svg;svgz';
  SVGGraphicDescription = 'Scalable Vector Graphics';
  SVGDefaultDPI = 96;

type

  { TSVGGraphic }

  TSVGGraphic = class(TGraphic)
  private
    FBackend: TSVGSoftBackend;
    FCache: TBitmap;
    FCacheHeight: Integer;
    FCacheWidth: Integer;
    FDocument: TSVGDocument;
    FDPI: Double;
    FFontsSet: Boolean;
    FImages: TSVGFileImageResolver;
    FOptions: TSVGReadOptions;
    FRenderer: TSVGRenderer;
    FSource: String; // the bytes as read, for the verbatim round trip
    FSourceFile: String;
  protected
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetMimeType: string; override;
    function GetRootElement: TSVGElement;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    function NeedRenderer: TSVGRenderer;
    procedure Changed(Sender: TObject); override;
    procedure FreeRenderer;
    procedure RenderToCache(AWidth, AHeight: Integer);
    procedure SetDPI(AValue: Double);
    procedure SetHeight(AValue: Integer); override;
    procedure SetRootLength(const AName: TSVGString; AValue: Integer);
    procedure SetTransparent(AValue: Boolean); override;
    procedure SetWidth(AValue: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
    procedure Clear; override;
    procedure LoadFromFile(const AFilename: string); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToStream(AStream: TStream); override;
    class function GetFileExtensions: string; override;
    class function IsStreamFormatSupported(AStream: TStream): Boolean; override;
    // Serializes the tree, whatever Modified says.
    procedure WriteSVGToStream(AStream: TStream);
    function WriteSVGToString: String;
    // The size the document asks for, in pixels. False when there is nothing
    // to render.
    function DocumentSize(out AWidth, AHeight: Integer): Boolean;
    // The viewBox of the root element, empty when it has none.
    function ViewBox: TSVGRect;
    // Call after editing Document directly, so that SaveToStream writes the
    // tree instead of the bytes that were read.
    procedure SVGChanged;
    // The parsed document, owned by this graphic. Nil when nothing is loaded.
    property Document: TSVGDocument read FDocument;
    // The root svg element, nil when nothing is loaded.
    property RootElement: TSVGElement read GetRootElement;
    // The bytes that were read. They are gzip for an svgz.
    property SourceText: String read FSource;
    // Options the next load reads with.
    property Options: TSVGReadOptions read FOptions write FOptions;
    // Dots per inch that absolute units resolve against.
    property DPI: Double read FDPI write SetDPI;
  end;

// Writes an element and its children as SVG. AIsRoot adds the SVG namespace.
procedure WriteSVGElement(AStream: TStream; Element: TSVGElement;
  IsRoot: Boolean);
// Writes an XML declaration and the root element of a document.
procedure WriteSVGDocument(AStream: TStream; ADocument: TSVGDocument);
function SVGEscapeText(const AText: TSVGString): TSVGString;
function SVGEscapeAttribute(const AValue: TSVGString): TSVGString;

implementation

procedure SVGWriteStr(AStream: TStream; const AText: String);
begin
  if AText <> '' then
    AStream.WriteBuffer(AText[1], Length(AText));
end;

function SVGEscapeText(const AText: TSVGString): TSVGString;
begin
  Result := StringReplace(AText, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function SVGEscapeAttribute(const AValue: TSVGString): TSVGString;
begin
  Result := StringReplace(AValue, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;

procedure WriteSVGElement(AStream: TStream; Element: TSVGElement;
  IsRoot: Boolean);
var
  i: Integer;
  Namespace: TSVGString;
  AttrName: TSVGString;
  Node: TSVGNode;
begin
  if Element = nil then
    exit;
  SVGWriteStr(AStream, '<' + Element.TagName);
  // The reader drops the xmlns declarations, so they are put back here.
  Namespace := '';
  if Element is TSVGForeignElement then
    begin
    Namespace := TSVGForeignElement(Element).Namespace;
    if (Element.Parent is TSVGForeignElement)
        and (TSVGForeignElement(Element.Parent).Namespace = Namespace) then
      Namespace := '';
    end
  else if IsRoot then
    Namespace := SVGNamespace;
  if Namespace <> '' then
    SVGWriteStr(AStream, ' xmlns="' + SVGEscapeAttribute(Namespace) + '"');
  for i := 0 to Element.AttributeCount - 1 do
    begin
    AttrName := Element.AttributeNames[i];
    SVGWriteStr(AStream, ' ' + AttrName + '="'
      + SVGEscapeAttribute(Element.Attributes[AttrName]) + '"');
    end;
  if Element.ChildCount = 0 then
    begin
    SVGWriteStr(AStream, '/>');
    exit;
    end;
  SVGWriteStr(AStream, '>');
  for i := 0 to Element.ChildCount - 1 do
    begin
    Node := Element.Children[i];
    if Node is TSVGElement then
      WriteSVGElement(AStream, TSVGElement(Node), False)
    else if Node is TSVGTextNode then
      SVGWriteStr(AStream, SVGEscapeText(TSVGTextNode(Node).Text));
    end;
  SVGWriteStr(AStream, '</' + Element.TagName + '>');
end;

procedure WriteSVGDocument(AStream: TStream; ADocument: TSVGDocument);
begin
  SVGWriteStr(AStream, '<?xml version="1.0" encoding="UTF-8"?>' + LineEnding);
  if (ADocument = nil) or (ADocument.Root = nil) then
    exit;
  WriteSVGElement(AStream, ADocument.Root, True);
  SVGWriteStr(AStream, LineEnding);
end;

{ TSVGGraphic }

constructor TSVGGraphic.Create;
begin
  inherited Create;
  FDPI := SVGDefaultDPI;
  FOptions := [roKeepForeign, roPreserveSpace];
  FImages := TSVGFileImageResolver.Create('');
end;

destructor TSVGGraphic.Destroy;
begin
  OnChange := nil;
  Clear;
  FreeAndNil(FImages);
  inherited Destroy;
end;

procedure TSVGGraphic.Clear;
begin
  FreeRenderer;
  FreeAndNil(FBackend);
  FreeAndNil(FDocument);
  FSource := '';
  FSourceFile := '';
  Changed(Self);
end;

procedure TSVGGraphic.Changed(Sender: TObject);
begin
  FreeAndNil(FCache);
  FCacheWidth := 0;
  FCacheHeight := 0;
  inherited Changed(Sender);
end;

procedure TSVGGraphic.FreeRenderer;
begin
  if (FDocument <> nil) and (FRenderer <> nil) then
    FDocument.CSSResolver := nil;
  FreeAndNil(FRenderer);
  FFontsSet := False;
end;

function TSVGGraphic.NeedRenderer: TSVGRenderer;
begin
  if FRenderer = nil then
    begin
    FRenderer := TSVGRenderer.Create;
    FRenderer.DPI := FDPI;
    FRenderer.Images := FImages;
    end;
  Result := FRenderer;
end;

function TSVGGraphic.GetRootElement: TSVGElement;
begin
  if FDocument = nil then
    Result := nil
  else
    Result := FDocument.Root;
end;

function TSVGGraphic.GetEmpty: Boolean;
begin
  Result := (FDocument = nil) or (FDocument.Root = nil);
end;

function TSVGGraphic.GetTransparent: Boolean;
begin
  Result := True;
end;

procedure TSVGGraphic.SetTransparent(AValue: Boolean);
begin
  // A SVG is always drawn with alpha.
end;

function TSVGGraphic.GetMimeType: string;
begin
  Result := SVGMimeType;
end;

class function TSVGGraphic.GetFileExtensions: string;
begin
  Result := SVGFileExtensions;
end;

procedure TSVGGraphic.SetDPI(AValue: Double);
var
  OldModified: Boolean;
begin
  if FDPI = AValue then
    exit;
  FDPI := AValue;
  if FRenderer <> nil then
    FRenderer.DPI := FDPI;
  // The DPI changes the size and the rendering, but not the document.
  OldModified := Modified;
  Changed(Self);
  if not OldModified then
    Modified := False;
end;

function TSVGGraphic.DocumentSize(out AWidth, AHeight: Integer): Boolean;
begin
  AWidth := 0;
  AHeight := 0;
  Result := (FDocument <> nil)
        and NeedRenderer.DocumentSize(FDocument, AWidth, AHeight);
end;

function TSVGGraphic.GetWidth: Integer;
var
  h: Integer;
begin
  if not DocumentSize(Result, h) then
    Result := 0;
end;

function TSVGGraphic.GetHeight: Integer;
var
  w: Integer;
begin
  if not DocumentSize(w, Result) then
    Result := 0;
end;

procedure TSVGGraphic.SetRootLength(const AName: TSVGString; AValue: Integer);
var
  Root: TSVGElement;
begin
  Root := GetRootElement;
  if Root = nil then
    exit;
  // A bare number is CSS pixels, which is what the getters resolved to.
  Root.Attributes[AName] := IntToStr(AValue);
  Changed(Self);
end;

procedure TSVGGraphic.SetWidth(AValue: Integer);
begin
  if AValue = GetWidth then
    exit;
  SetRootLength('width', AValue);
end;

procedure TSVGGraphic.SetHeight(AValue: Integer);
begin
  if AValue = GetHeight then
    exit;
  SetRootLength('height', AValue);
end;

function TSVGGraphic.ViewBox: TSVGRect;
var
  Root: TSVGElement;
begin
  Result := TSVGRect.Empty;
  Root := GetRootElement;
  if Root = nil then
    exit;
  if not Result.ReadViewBoxAttribute(Root) then
    Result := TSVGRect.Empty;
end;

procedure TSVGGraphic.LoadFromStream(AStream: TStream);
var
  Src: String;
  Count: Int64;
  Reader: TSVGReader;
  NewDoc: TSVGDocument;
begin
  Count := AStream.Size - AStream.Position;
  SetLength(Src, Count);
  if Count > 0 then
    AStream.ReadBuffer(Src[1], Count);
  Reader := TSVGReader.Create;
  try
    Reader.Options := FOptions;
    // The reader inflates gzip itself, so an svgz needs nothing extra.
    NewDoc := Reader.ReadFromString(Src);
  finally
    Reader.Free;
  end;
  // Clear now, so that a failed parse leaves the old document alone.
  Clear;
  FDocument := NewDoc;
  FSource := Src;
  Changed(Self);
  Modified := False;
end;

procedure TSVGGraphic.LoadFromFile(const AFilename: string);
begin
  inherited LoadFromFile(AFilename);
  FSourceFile := AFilename;
  if FDocument <> nil then
    FDocument.BaseURI := AFilename;
  FImages.BasePath := ExtractFilePath(AFilename);
end;

procedure TSVGGraphic.SaveToStream(AStream: TStream);
begin
  if GetEmpty then
    exit;
  if (not Modified) and (FSource <> '') then
    AStream.WriteBuffer(FSource[1], Length(FSource))
  else
    WriteSVGToStream(AStream);
end;

procedure TSVGGraphic.WriteSVGToStream(AStream: TStream);
begin
  WriteSVGDocument(AStream, FDocument);
end;

function TSVGGraphic.WriteSVGToString: String;
var
  ms: TMemoryStream;
begin
  Result := '';
  ms := TMemoryStream.Create;
  try
    WriteSVGToStream(ms);
    SetLength(Result, ms.Size);
    if ms.Size > 0 then
      Move(ms.Memory^, Result[1], ms.Size);
  finally
    ms.Free;
  end;
end;

procedure TSVGGraphic.SVGChanged;
begin
  Changed(Self);
end;

procedure TSVGGraphic.Assign(ASource: TPersistent);
var
  Src: TSVGGraphic;
  ms: TMemoryStream;
begin
  if ASource is TSVGGraphic then
    begin
    Src := TSVGGraphic(ASource);
    FDPI := Src.DPI;
    FOptions := Src.Options;
    if Src.Empty then
      begin
      Clear;
      exit;
      end;
    // fcl-svg has no TSVGDocument.Clone, so copy it manually
    ms := TMemoryStream.Create;
    try
      Src.SaveToStream(ms);
      ms.Position := 0;
      LoadFromStream(ms);
    finally
      ms.Free;
    end;
    FSourceFile := Src.FSourceFile;
    if FDocument <> nil then
      FDocument.BaseURI := FSourceFile;
    FImages.BasePath := ExtractFilePath(FSourceFile);
    if Src.Modified then
      Changed(Self);
    end
  else
    inherited Assign(ASource);
end;

class function TSVGGraphic.IsStreamFormatSupported(AStream: TStream): Boolean;
var
  OldPos: Int64;
  Buf: array[0..511] of Byte;
  Count: Integer;
  s: String;
  p: Integer;
begin
  Result := False;
  OldPos := AStream.Position;
  try
    Count := AStream.Read(Buf, SizeOf(Buf));
    if Count < 4 then
      exit;
    if (Buf[0] = $1F) and (Buf[1] = $8B) then
      begin
      // gzip -> svgz
      Result := True;
      exit;
      end;
    SetLength(s, Count);
    Move(Buf, s[1], Count);
    p := 1;
    if Copy(s, 1, 3) = #$EF#$BB#$BF then
      inc(p, 3);
    while (p <= Length(s)) and (s[p] in [#9, #10, #13, ' ']) do
      inc(p);
    Result := CompareText(Copy(s, p, 4), '<svg') = 0;
    if Result then
      exit;
    Result := ((CompareText(Copy(s, p, 5), '<?xml') = 0)
            or (Copy(s, p, 4) = '<!--')
            or (CompareText(Copy(s, p, 9), '<!DOCTYPE') = 0))
          and (Pos('<svg', LowerCase(s)) > 0);
  finally
    AStream.Position := OldPos;
  end;
end;

procedure TSVGGraphic.RenderToCache(AWidth, AHeight: Integer);
var
  IntfImg: TLazIntfImage;
  Img: TFPCustomImage;
  x: Integer;
  y: Integer;
begin
  FreeAndNil(FCache);
  FCacheWidth := 0;
  FCacheHeight := 0;
  if GetEmpty or (AWidth <= 0) or (AHeight <= 0) then
    exit;
  NeedRenderer;
  if not FFontsSet then
    begin
    FRenderer.Fonts := SVGPlatformFontProvider;
    FFontsSet := True;
    end;
  if FBackend = nil then
    FBackend := TSVGSoftBackend.Create;
  FRenderer.RenderToSize(FDocument, FBackend, AWidth, AHeight);
  Img := FBackend.Image;
  if Img = nil then
    exit;
  IntfImg := TLazIntfImage.Create(AWidth, AHeight, [riqfRGB, riqfAlpha]);
  try
    for y := 0 to AHeight - 1 do
      for x := 0 to AWidth - 1 do
        IntfImg.Colors[x, y] := Img.Colors[x, y];
    FCache := TBitmap.Create;
    FCache.PixelFormat := pf32bit;
    FCache.SetSize(AWidth, AHeight);
    FCache.LoadFromIntfImage(IntfImg);
    FCacheWidth := AWidth;
    FCacheHeight := AHeight;
  finally
    IntfImg.Free;
  end;
end;

procedure TSVGGraphic.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  w: Integer;
  h: Integer;
begin
  if GetEmpty then
    exit;
  w := ARect.Right - ARect.Left;
  h := ARect.Bottom - ARect.Top;
  if (w <= 0) or (h <= 0) then
    exit;
  if (FCache = nil) or (FCacheWidth <> w) or (FCacheHeight <> h) then
    RenderToCache(w, h);
  if FCache <> nil then
    ACanvas.Draw(ARect.Left, ARect.Top, FCache);
end;

initialization
  TPicture.RegisterFileFormat(SVGFileExtensions, SVGGraphicDescription,
    TSVGGraphic);
  TPicture.RegisterClipboardFormat(RegisterClipboardFormat(SVGMimeType),
    TSVGGraphic);

finalization
  TPicture.UnregisterGraphicClass(TSVGGraphic);

end.
