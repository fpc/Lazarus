{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Software backend: rasterized fills composited into an fcl-image surface.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.soft;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Hash.Base64, System.Math,
     FpImage, FpImage.Reader.PNG, FpImage.Reader.JPEG, fpsvg.types,
     fpsvg.backend, fpsvg.geom, fpsvg.raster;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, base64, math, fpimage, fpreadpng, fpreadjpeg,
     fpsvg.types, fpsvg.backend, fpsvg.geom, fpsvg.raster;
{$ENDIF FPC_DOTTEDUNITS}

type
  ESVGSoft = class(ESVGBackend);

  { A memory image the backend draws into, which it can empty in one go.
    Its ancestor keeps four channels of a pixel and no palette, so a
    colour of it is reached without a test for one. }
  TSVGSoftSurface = class(TFPCompactImgRGBA16Bit)
  public
    // Sets every pixel to transparent black.
    procedure ClearToTransparent;
  end;

  { Wraps an fcl-image surface as a pixel source that a backend can read. }
  TSVGImageSource = class(TObject, ISVGImageSource)
  private
    FImage: TFPCustomImage;
    FOwnsImage: Boolean;
  public
    constructor Create(aImage: TFPCustomImage; aOwnsImage: Boolean);
    destructor Destroy; override;
    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetPixel(aX, aY: Integer): TSVGColor;
    function GetRow(aY, aX, aCount: Integer; aDest: PSVGColor): Boolean;
    // The wrapped surface. It is freed with this object only when this
    // object owns it.
    property Image: TFPCustomImage read FImage;
  end;

  { Reads image references from the file system and from data URIs. The
    formats it can read are the fcl-image readers the program has linked. }
  TSVGFileImageResolver = class(TObject, ISVGImageResolver)
  private
    FBasePath: String;
    function ReadDataURI(const aHRef: String): ISVGImageSource;
    function ReadFile(const aName: String): ISVGImageSource;
  public
    constructor Create(const aBasePath: String);
    function ResolveImage(const aHRef, aBaseURI: String): ISVGImageSource;
    // Directory a relative reference is resolved against, used when the
    // document has no location.
    property BasePath: String read FBasePath write FBasePath;
  end;

  TSVGSoftLayer = record
    Surface : TFPCustomImage;
    Opacity : Double;
    Bounds  : TSVGRect;
  end;
  TSVGSoftLayerArray = array of TSVGSoftLayer;
  TSVGSoftSurfaceArray = array of TFPCustomImage;

  { Clip coverage for each pixel of the rectangle it bounds. A pixel
    outside that rectangle is not clipped in, and reads as zero. }
  TSVGClipMask = record
    Left, Top, Right, Bottom : Integer;
    Data : TSVGCoverageArray;
    // Allocates a cleared mask over the given half-open pixel rectangle.
    procedure Allocate(aLeft, aTop, aRight, aBottom: Integer);
    // The coverage at a device pixel, zero outside the rectangle.
    function CoverageAt(aX, aY: Integer): Byte;
    // True when the rectangle holds no pixels.
    function IsEmpty: Boolean;
  end;
  TSVGClipMaskArray = array of TSVGClipMask;

  { Where in a pixel a paint server is read.
    psMiddle reads at the centre of the pixel. That is what SVG means, and
    where everything else here samples.
    psCorner reads at the top left corner, half a pixel away. That is how
    the reference images of the W3C suite were drawn. }
  TSVGPaintSample = (psMiddle, psCorner);

  { Draws into an fcl-image surface, using the scanline rasterizer. }
  TSVGSoftBackend = class(TSVGRenderBackend)
  private
    FImage: TFPCustomImage;
    FTarget: TFPCustomImage;
    FLayers: TSVGSoftLayerArray;
    FLayerCount: Integer;
    FFilterImages: TSVGSoftSurfaceArray;
    FClips: TSVGClipMaskArray;
    FClipCount: Integer;
    FMixing: TSVGColorInterpolation;
    FPaintSample: TSVGPaintSample;
    FOwnsImage: Boolean;
    FPoly: TSVGPolyPath;
    FSource: TSVGPolyPath;
    FDashed: TSVGPolyPath;
    FGlyphPath: TSVGPath;
    FOutline: TSVGPath;
    FRasterizer: TSVGRasterizer;
    FFlatness: Double;
    FInFrame: Boolean;
    FSpanColor: TSVGColor;
    FSpanOpacity: Double;
    FSpanGradient: TSVGGradient;
    FSpanIsGradient: Boolean;
    FSpanInverse: TSVGMatrix;
    function GetSubSamples: Integer;
    procedure SetSubSamples(aValue: Integer);
    function SampleOffset: Double;
    procedure BlendSpan(aY, aX, aCount: Integer; aCoverage: PByte);
    procedure ClipSpan(aY, aX, aCount: Integer; aCoverage: PByte);
    procedure BlendPixel(aX, aY: Integer; const aColor: TSVGColor;
      aAlpha: Integer);
    function ClipCoverage(aX, aY: Integer): Byte;
    procedure ApplyRasterClip;
    procedure ClipToPoly(aRule: TSVGFillRule; aDrawn: Boolean);
    procedure ClearSurface(aSurface: TFPCustomImage);
    procedure NeedFrame(const aOperation: String);
    function PrepareGradient(const aPaint: TSVGPaint;
      const aCTM: TSVGMatrix; const aBounds: TSVGRect): Boolean;
    procedure FillPolygon(const aPaint: TSVGPaint; aRule: TSVGFillRule;
      aOpacity: Double; const aCTM: TSVGMatrix; const aBounds: TSVGRect);
    procedure CompositeLayer(const aLayer: TSVGSoftLayer);
  public
    constructor Create; override;
    destructor Destroy; override;
    class function BackendName: String; override;
    class function Capabilities: TSVGBackendCapabilities; override;

    // Draws into an existing surface. The backend does not own it.
    procedure SetTarget(aImage: TFPCustomImage);

    procedure BeginFrame(aWidth, aHeight: Integer); override;
    procedure EndFrame; override;
    procedure FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aRule: TSVGFillRule; aOpacity: Double); override;
    procedure StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; const aPen: TSVGPen; aOpacity: Double); override;
    procedure PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      aRule: TSVGFillRule); override;
    procedure PushStrokeClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPen: TSVGPen); override;
    procedure PopClip; override;
    procedure SetColorInterpolation(
      aSpace: TSVGColorInterpolation); override;
    procedure PushLayer(const aBounds: TSVGRect; aOpacity: Double;
      aIsolate: Boolean); override;
    procedure PopLayer; override;
    procedure PopLayerAsMask(aMode: TSVGMaskMode); override;
    procedure PopLayerAsFilter(const aChain: TSVGFilterChain); override;
    procedure PopLayerAsFilterImage(aIndex: Integer); override;
    procedure ClearFilterImages;
    procedure DrawGlyphRun(aFont: TSVGFontHandle; const aGlyphs: TSVGGlyphArray;
      const aCTM: TSVGMatrix; const aPaint: TSVGPaint; aOpacity: Double); override;
    procedure DrawImage(aImage: ISVGImageSource; const aRect: TSVGRect;
      const aCTM: TSVGMatrix; aOpacity: Double); override;

    // The surface being drawn into. It is owned when BeginFrame created
    // it.
    property Image: TFPCustomImage read FImage;
    // How far a flattened segment may stray from the true curve.
    property Flatness: Double read FFlatness write FFlatness;
    // Where in a pixel a paint server is read. Leave it unchanged to read
    // at the centre of the pixel.
    property PaintSample: TSVGPaintSample read FPaintSample
      write FPaintSample;
    // Number of lines across a row of pixels that a shape is measured on.
    // The coverage of its edges comes from them, so more lines follow a
    // shape more closely and take longer.
    property SubSamples: Integer read GetSubSamples write SetSubSamples;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ELSE FPC_DOTTEDUNITS}
uses fpsvg.strings;
{$ENDIF FPC_DOTTEDUNITS}

{ TSVGSoftSurface }

procedure TSVGSoftSurface.ClearToTransparent;

begin
  if (FData <> nil) and (Width > 0) and (Height > 0) then
    FillChar(FData^, Width * Height * SizeOf(TFPColor), 0);
end;


{ TSVGImageSource }

constructor TSVGImageSource.Create(aImage: TFPCustomImage; aOwnsImage: Boolean);

begin
  inherited Create;
  if aImage = nil then
    raise ESVGSoft.Create(SErrNoImageToWrap);
  FImage := aImage;
  FOwnsImage := aOwnsImage;
end;


destructor TSVGImageSource.Destroy;

begin
  if FOwnsImage then
    FreeAndNil(FImage);
  inherited Destroy;
end;


function TSVGImageSource.GetWidth: Integer;

begin
  Result := FImage.Width;
end;


function TSVGImageSource.GetHeight: Integer;

begin
  Result := FImage.Height;
end;


function TSVGImageSource.GetPixel(aX, aY: Integer): TSVGColor;

begin
  Result := TSVGColor(FImage.Colors[aX, aY]);
end;


function TSVGImageSource.GetRow(aY, aX, aCount: Integer;
  aDest: PSVGColor): Boolean;

var
  I: Integer;

begin
  Result := True;
  for I := 0 to aCount - 1 do
    aDest[I] := TSVGColor(FImage.Colors[aX + I, aY]);
end;


{ TSVGClipMask }

procedure TSVGClipMask.Allocate(aLeft, aTop, aRight, aBottom: Integer);

begin
  Left := aLeft;
  Top := aTop;
  Right := Max(aLeft, aRight);
  Bottom := Max(aTop, aBottom);
  Data := nil;
  if not IsEmpty then
    SetLength(Data, (Right - Left) * (Bottom - Top));
end;


function TSVGClipMask.CoverageAt(aX, aY: Integer): Byte;

begin
  if (aX < Left) or (aX >= Right) or (aY < Top) or (aY >= Bottom) then
    Result := 0
  else
    Result := Data[(aY - Top) * (Right - Left) + (aX - Left)];
end;


function TSVGClipMask.IsEmpty: Boolean;

begin
  Result := (Right <= Left) or (Bottom <= Top);
end;


// True when a path is absolute, so it needs no base directory.
function IsAbsolutePath(const aName: String): Boolean;

begin
  Result := (aName <> '') and ((aName[1] = PathDelim) or (aName[1] = '/'));
  {$IFDEF WINDOWS}
  Result := Result or ((Length(aName) >= 2) and (aName[2] = ':'));
  {$ENDIF}
end;


{ TSVGFileImageResolver }

constructor TSVGFileImageResolver.Create(const aBasePath: String);

begin
  inherited Create;
  FBasePath := aBasePath;
end;


// The gamma a screen expects its samples in, the one sRGB defines.
const
  SVGScreenGamma = 0.45455;

// True when the stream opens with the eight bytes every PNG opens with.
// The position of the stream is left unchanged.
function IsPNGStream(aStream: TStream): Boolean;

const
  Signature: array[0..7] of Byte = (137, 80, 78, 71, 13, 10, 26, 10);

var
  lHead: array[0..7] of Byte;
  lWas: Int64;
  I: Integer;

begin
  Result := False;
  lWas := aStream.Position;
  try
    if aStream.Read(lHead, 8) <> 8 then
      Exit;
    for I := 0 to 7 do
      if lHead[I] <> Signature[I] then
        Exit;
    Result := True;
  finally
    aStream.Position := lWas;
  end;
end;


// The gamma a PNG declares in its gAMA chunk, or zero when it declares
// none. The chunks are walked here rather than read from the reader,
// which only some versions of fcl-image offer the value on.
function PNGGammaOf(aStream: TStream): Double;

var
  lWas: Int64;
  lLength, lValue: LongWord;
  lKind: array[0..3] of AnsiChar;

begin
  Result := 0;
  lWas := aStream.Position;
  try
    aStream.Position := lWas + 8;
    while aStream.Position + 8 <= aStream.Size do
      begin
      if aStream.Read(lLength, 4) <> 4 then
        Exit;
      lLength := BEtoN(lLength);
      if aStream.Read(lKind, 4) <> 4 then
        Exit;
      // The image data begins, and every chunk before it has been seen.
      if (lKind = 'IDAT') or (lKind = 'IEND') then
        Exit;
      if lKind = 'gAMA' then
        begin
        if (lLength <> 4) or (aStream.Read(lValue, 4) <> 4) then
          Exit;
        lValue := BEtoN(lValue);
        if lValue > 0 then
          Result := lValue / 100000;
        Exit;
        end;
      // Past the data of this chunk and the four bytes of its checksum.
      aStream.Position := aStream.Position + lLength + 4;
      end;
  finally
    aStream.Position := lWas;
  end;
end;


// Brings the samples of an image to the gamma a screen expects. The alpha
// is left unchanged: only the colour has a gamma.
procedure ApplyGamma(aImage: TFPMemoryImage; aExponent: Double);

var
  lTable: array of Word;
  X, Y, I: Integer;
  lColor: TFPColor;

begin
  SetLength(lTable, 65536);
  lTable[0] := 0;
  for I := 1 to 65535 do
    lTable[I] := Round(65535 * Power(I / 65535, aExponent));
  for Y := 0 to aImage.Height - 1 do
    for X := 0 to aImage.Width - 1 do
      begin
      lColor := aImage.Colors[X, Y];
      lColor.Red := lTable[lColor.Red];
      lColor.Green := lTable[lColor.Green];
      lColor.Blue := lTable[lColor.Blue];
      aImage.Colors[X, Y] := lColor;
      end;
end;


// Reads an image from a stream. When a PNG declares a gamma other than
// the one a screen expects, its samples are converted to that gamma: SVG
// draws the colours the file declares, not its raw numbers. Nil when the
// stream holds nothing that can be read.
function ReadImageStream(aStream: TStream): TFPMemoryImage;

var
  lReader: TFPReaderPNG;
  lGamma: Double;

begin
  Result := TFPMemoryImage.Create(0, 0);
  lReader := nil;
  lGamma := 0;
  try
    try
      if not IsPNGStream(aStream) then
        Result.LoadFromStream(aStream)
      else
        begin
        lReader := TFPReaderPNG.Create;
        lGamma := PNGGammaOf(aStream);
        Result.LoadFromStream(aStream, lReader);
        end;
    except
      FreeAndNil(Result);
    end;
  finally
    lReader.Free;
  end;
  // A file already at the gamma of the screen needs no conversion, and
  // the ones that declare 0.45 are close enough that converting them only
  // rounds the values.
  if (Result <> nil) and (lGamma > 0)
     and (Abs(lGamma - SVGScreenGamma) > 0.01) then
    ApplyGamma(Result, SVGScreenGamma / lGamma);
end;


function TSVGFileImageResolver.ReadDataURI(const aHRef: String): ISVGImageSource;

var
  lMarker: Integer;
  lText: RawByteString;
  lEncoded, lDecoded: TMemoryStream;
  lDecoder: TBase64DecodingStream;
  lImage: TFPMemoryImage;
  lBuffer: array[0..4095] of Byte;
  lRead: Integer;

begin
  Result := nil;
  lMarker := Pos(';base64,', aHRef);
  if lMarker = 0 then
    Exit;
  // The text of a URI is ASCII and the bytes behind it are not, so the
  // decoding runs over bytes from here on.
  lText := Copy(aHRef, lMarker + Length(';base64,'), Length(aHRef));
  lImage := nil;
  lDecoded := nil;
  lDecoder := nil;
  lEncoded := TMemoryStream.Create;
  try
    if lText <> '' then
      lEncoded.Write(lText[1], Length(lText));
    lEncoded.Position := 0;
    lDecoder := TBase64DecodingStream.Create(lEncoded);
    lDecoded := TMemoryStream.Create;
    lRead := lDecoder.Read(lBuffer, SizeOf(lBuffer));
    while lRead > 0 do
      begin
      lDecoded.WriteBuffer(lBuffer, lRead);
      lRead := lDecoder.Read(lBuffer, SizeOf(lBuffer));
      end;
    lDecoded.Position := 0;
    lImage := ReadImageStream(lDecoded);
    if lImage <> nil then
      Result := TSVGImageSource.Create(lImage, True);
  finally
    lDecoded.Free;
    lDecoder.Free;
    lEncoded.Free;
  end;
end;


function TSVGFileImageResolver.ReadFile(const aName: String): ISVGImageSource;

var
  lImage: TFPMemoryImage;
  lStream: TFileStream;

begin
  Result := nil;
  if not FileExists(aName) then
    Exit;
  lImage := nil;
  try
    lStream := TFileStream.Create(aName, fmOpenRead or fmShareDenyWrite);
    try
      lImage := ReadImageStream(lStream);
    finally
      lStream.Free;
    end;
  except
    FreeAndNil(lImage);
  end;
  if lImage <> nil then
    Result := TSVGImageSource.Create(lImage, True);
end;


function TSVGFileImageResolver.ResolveImage(
  const aHRef, aBaseURI: String): ISVGImageSource;

var
  lName, lBase: String;

begin
  Result := nil;
  lName := Trim(aHRef);
  if lName = '' then
    Exit;
  if Copy(lName, 1, 5) = 'data:' then
    Exit(ReadDataURI(lName));
  if Copy(lName, 1, 7) = 'file://' then
    lName := Copy(lName, 8, Length(lName));
  if not IsAbsolutePath(lName) then
    begin
    lBase := FBasePath;
    if aBaseURI <> '' then
      lBase := ExtractFilePath(aBaseURI);
    if lBase <> '' then
      lName := IncludeTrailingPathDelimiter(lBase) + lName;
    end;
  Result := ReadFile(lName);
end;


{ TSVGSoftBackend }

constructor TSVGSoftBackend.Create;

begin
  inherited Create;
  FPoly := TSVGPolyPath.Create;
  FSource := TSVGPolyPath.Create;
  FDashed := TSVGPolyPath.Create;
  FGlyphPath := TSVGPath.Create;
  FOutline := TSVGPath.Create;
  FRasterizer := TSVGRasterizer.Create;
  FRasterizer.OnSpan := @BlendSpan;
  FFlatness := SVGDefaultFlatness;
end;


destructor TSVGSoftBackend.Destroy;

var
  I: Integer;

begin
  for I := 0 to FLayerCount - 1 do
    FLayers[I].Surface.Free;
  FLayerCount := 0;
  ClearFilterImages;
  FreeAndNil(FRasterizer);
  FreeAndNil(FOutline);
  FreeAndNil(FGlyphPath);
  FreeAndNil(FDashed);
  FreeAndNil(FSource);
  FreeAndNil(FPoly);
  if FOwnsImage then
    FreeAndNil(FImage);
  inherited Destroy;
end;


class function TSVGSoftBackend.BackendName: String;

begin
  Result := 'software';
end;


class function TSVGSoftBackend.Capabilities: TSVGBackendCapabilities;

begin
  Result := [bcGroupOpacity, bcDashes, bcClipPath, bcMask, bcFilter];
end;


procedure TSVGSoftBackend.SetTarget(aImage: TFPCustomImage);

begin
  if FOwnsImage then
    FreeAndNil(FImage);
  FImage := aImage;
  FOwnsImage := False;
end;


procedure TSVGSoftBackend.NeedFrame(const aOperation: String);

begin
  if not FInFrame then
    raise ESVGSoft.CreateFmt(SErrOutsideFrame, [aOperation]);
end;


procedure TSVGSoftBackend.BeginFrame(aWidth, aHeight: Integer);

begin
  if FInFrame then
    raise ESVGSoft.Create(SErrBeginFrameInFrame);
  if (aWidth <= 0) or (aHeight <= 0) then
    raise ESVGSoft.CreateFmt(SErrFrameHasNoExtent,
      [aWidth, aHeight]);
  if FImage = nil then
    begin
    FImage := TSVGSoftSurface.Create(aWidth, aHeight);
    FOwnsImage := True;
    end
  else if (FImage.Width <> aWidth) or (FImage.Height <> aHeight) then
    FImage.SetSize(aWidth, aHeight);
  ClearSurface(FImage);
  FTarget := FImage;
  FLayerCount := 0;
  FClipCount := 0;
  FRasterizer.SetClip(0, 0, aWidth, aHeight);
  FInFrame := True;
end;


procedure TSVGSoftBackend.ClearSurface(aSurface: TFPCustomImage);

var
  X, Y: Integer;
  lTransparent: TFPColor;

begin
  if aSurface is TSVGSoftSurface then
    begin
    TSVGSoftSurface(aSurface).ClearToTransparent;
    Exit;
    end;
  lTransparent := TFPColor(TSVGColor.Transparent);
  for Y := 0 to aSurface.Height - 1 do
    for X := 0 to aSurface.Width - 1 do
      aSurface.Colors[X, Y] := lTransparent;
end;


procedure TSVGSoftBackend.EndFrame;

begin
  NeedFrame('EndFrame');
  if FLayerCount <> 0 then
    raise ESVGSoft.CreateFmt(SErrEndFrameLayersOpen, [FLayerCount]);
  if FClipCount <> 0 then
    raise ESVGSoft.CreateFmt(SErrEndFrameClipsOpen, [FClipCount]);
  FInFrame := False;
end;


procedure TSVGSoftBackend.BlendPixel(aX, aY: Integer;
  const aColor: TSVGColor; aAlpha: Integer);

var
  lTarget: TSVGColor;
  lSource, lKept, lOut: Int64;

begin
  if aAlpha <= 0 then
    Exit;
  if aAlpha > 65535 then
    aAlpha := 65535;
  lTarget := TSVGColor(FTarget.Colors[aX, aY]);
  if lTarget.Alpha = 0 then
    begin
    lTarget := aColor;
    lTarget.Alpha := aAlpha;
    FTarget.Colors[aX, aY] := TFPColor(lTarget);
    Exit;
    end;
  // The channels of the surface are not multiplied by its alpha. The part
  // the target keeps is weighted by its own alpha, and the sum is divided
  // by the resulting alpha.
  lSource := aAlpha;
  lKept := lTarget.Alpha * (65535 - aAlpha) div 65535;
  lOut := lSource + lKept;
  if lOut <= 0 then
    Exit;
  // Colours are mixed in the colour space of the property. Alpha is a
  // coverage and not a colour, so it mixes the same way in both.
  if FMixing = ciLinearRGB then
    begin
    lTarget.Red := SVGFromLinear((SVGToLinear(aColor.Red) * lSource
      + SVGToLinear(lTarget.Red) * lKept) / lOut);
    lTarget.Green := SVGFromLinear((SVGToLinear(aColor.Green) * lSource
      + SVGToLinear(lTarget.Green) * lKept) / lOut);
    lTarget.Blue := SVGFromLinear((SVGToLinear(aColor.Blue) * lSource
      + SVGToLinear(lTarget.Blue) * lKept) / lOut);
    end
  else
    begin
    lTarget.Red := (aColor.Red * lSource + lTarget.Red * lKept) div lOut;
    lTarget.Green := (aColor.Green * lSource + lTarget.Green * lKept) div lOut;
    lTarget.Blue := (aColor.Blue * lSource + lTarget.Blue * lKept) div lOut;
    end;
  lTarget.Alpha := lOut;
  FTarget.Colors[aX, aY] := TFPColor(lTarget);
end;


function TSVGSoftBackend.ClipCoverage(aX, aY: Integer): Byte;

begin
  if FClipCount = 0 then
    Result := 255
  else
    Result := FClips[FClipCount - 1].CoverageAt(aX, aY);
end;


function TSVGSoftBackend.GetSubSamples: Integer;

begin
  Result := FRasterizer.SubSamples;
end;


procedure TSVGSoftBackend.SetSubSamples(aValue: Integer);

begin
  FRasterizer.SubSamples := aValue;
end;


procedure TSVGSoftBackend.BlendSpan(aY, aX, aCount: Integer; aCoverage: PByte);

var
  I, lAlpha, lClip: Integer;
  lSource: TSVGColor;

begin
  for I := 0 to aCount - 1 do
    begin
    lAlpha := aCoverage[I];
    if lAlpha = 0 then
      Continue;
    if FClipCount > 0 then
      begin
      lClip := FClips[FClipCount - 1].CoverageAt(aX + I, aY);
      if lClip = 0 then
        Continue;
      lAlpha := (lAlpha * lClip) div 255;
      if lAlpha = 0 then
        Continue;
      end;
    if FSpanIsGradient then
      lSource := FSpanGradient.ColorAt(FSpanGradient.OffsetAt(
        FSpanInverse.Transform(TSVGPoint.Create(aX + I + SampleOffset,
          aY + SampleOffset))))
    else
      lSource := FSpanColor;
    BlendPixel(aX + I, aY,
      lSource, Round(lAlpha * 257 * FSpanOpacity * lSource.Alpha / 65535));
    end;
end;


function TSVGSoftBackend.SampleOffset: Double;

begin
  if FPaintSample = psCorner then
    Result := 0
  else
    Result := 0.5;
end;


procedure TSVGSoftBackend.ClipSpan(aY, aX, aCount: Integer; aCoverage: PByte);

var
  I, lIndex, lValue: Integer;

begin
  with FClips[FClipCount] do
    begin
    if (aY < Top) or (aY >= Bottom) then
      Exit;
    lIndex := (aY - Top) * (Right - Left) + (aX - Left);
    for I := 0 to aCount - 1 do
      begin
      if (aX + I < Left) or (aX + I >= Right) then
        Continue;
      lValue := aCoverage[I];
      if (lValue > 0) and (FClipCount > 0) then
        lValue := (lValue * FClips[FClipCount - 1].CoverageAt(aX + I, aY))
          div 255;
      Data[lIndex + I] := lValue;
      end;
    end;
end;


procedure TSVGSoftBackend.ApplyRasterClip;

begin
  if FClipCount = 0 then
    FRasterizer.SetClip(0, 0, FImage.Width, FImage.Height)
  else
    with FClips[FClipCount - 1] do
      FRasterizer.SetClip(Left, Top, Right, Bottom);
end;


function TSVGSoftBackend.PrepareGradient(const aPaint: TSVGPaint;
  const aCTM: TSVGMatrix; const aBounds: TSVGRect): Boolean;

var
  lForward: TSVGMatrix;

begin
  Result := False;
  if (aPaint.Server = nil)
     or not aPaint.Server.GetGradient(FSpanGradient)
     or not FSpanGradient.HasStops then
    Exit;
  // Gradient space maps to the device in three steps: the gradient
  // transform, then the bounding box when the units require it, then the
  // CTM.
  lForward := FSpanGradient.Transform;
  if FSpanGradient.Units = guObjectBoundingBox then
    begin
    if aBounds.IsEmpty then
      Exit;
    lForward := lForward.Compose(TSVGGradient.BoxTransform(aBounds));
    end;
  lForward := lForward.Compose(aCTM);
  Result := lForward.Invert(FSpanInverse);
end;


procedure TSVGSoftBackend.FillPolygon(const aPaint: TSVGPaint;
  aRule: TSVGFillRule; aOpacity: Double; const aCTM: TSVGMatrix;
  const aBounds: TSVGRect);

begin
  if aOpacity <= 0 then
    Exit;
  FSpanIsGradient := False;
  case aPaint.Kind of
    spColor: FSpanColor := aPaint.Color;
    spServer:
      begin
      if not PrepareGradient(aPaint, aCTM, aBounds) then
        Exit;
      FSpanIsGradient := True;
      end;
  else
    Exit;
  end;
  FSpanOpacity := SVGClamp(aOpacity, 0, 1);
  FRasterizer.Rasterize(FPoly, aRule);
end;


procedure TSVGSoftBackend.FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aRule: TSVGFillRule; aOpacity: Double);

var
  lScale: Double;

begin
  NeedFrame('FillPath');
  lScale := aCTM.MaxScale;
  if lScale <= 0 then
    Exit;
  // Flattening in user space keeps the bounding box that a bounding box
  // gradient needs. The tolerance is scaled so a magnified curve does not
  // become coarse.
  FSource.Flatten(aPath, TSVGMatrix.Identity, FFlatness / lScale);
  FPoly.Assign(FSource);
  FPoly.Transform(aCTM);
  FillPolygon(aPaint, aRule, aOpacity, aCTM, FSource.Bounds);
end;


procedure TSVGSoftBackend.StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; const aPen: TSVGPen; aOpacity: Double);

var
  lScale, lTolerance: Double;

begin
  NeedFrame('StrokePath');
  if aPen.Width <= 0 then
    Exit;
  // The pen is round in user space, so the outline is built there and then
  // mapped to the device. A CTM that scales unevenly makes it elliptical.
  lScale := aCTM.MaxScale;
  if lScale <= 0 then
    Exit;
  lTolerance := FFlatness / lScale;
  FSource.Flatten(aPath, TSVGMatrix.Identity, lTolerance);
  if aPen.IsDashed then
    begin
    FDashed.BuildDashes(FSource, aPen.Dashes, aPen.DashOffset);
    FPoly.BuildStroke(FDashed, aPen, lTolerance);
    end
  else
    FPoly.BuildStroke(FSource, aPen, lTolerance);
  FPoly.Transform(aCTM);
  FillPolygon(aPaint, frNonZero, aOpacity, aCTM, FSource.Bounds);
end;


// Intersects the clip with the polygon in FPoly, which is in device space
// by the time this is called. aDrawn is False when nothing was built, and
// the clip then closes to nothing.
procedure TSVGSoftBackend.ClipToPoly(aRule: TSVGFillRule; aDrawn: Boolean);

var
  lBounds: TSVGRect;
  lLeft, lTop, lRight, lBottom: Integer;

begin
  if FClipCount = Length(FClips) then
    SetLength(FClips, Max(4, FClipCount * 2));
  lLeft := 0;
  lTop := 0;
  lRight := 0;
  lBottom := 0;
  if aDrawn then
    begin
    lBounds := FPoly.Bounds;
    if not lBounds.IsEmpty then
      begin
      lLeft := Max(0, Floor(lBounds.Left));
      lTop := Max(0, Floor(lBounds.Top));
      lRight := Min(FImage.Width, Ceil(lBounds.Right));
      lBottom := Min(FImage.Height, Ceil(lBounds.Bottom));
      end;
    end;
  if FClipCount > 0 then
    with FClips[FClipCount - 1] do
      begin
      lLeft := Max(lLeft, Left);
      lTop := Max(lTop, Top);
      lRight := Min(lRight, Right);
      lBottom := Min(lBottom, Bottom);
      end;
  FClips[FClipCount].Allocate(lLeft, lTop, lRight, lBottom);
  if not FClips[FClipCount].IsEmpty then
    begin
    FRasterizer.SetClip(lLeft, lTop, lRight, lBottom);
    FRasterizer.OnSpan := @ClipSpan;
    try
      FRasterizer.Rasterize(FPoly, aRule);
    finally
      FRasterizer.OnSpan := @BlendSpan;
    end;
    end;
  Inc(FClipCount);
  ApplyRasterClip;
end;


procedure TSVGSoftBackend.PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
  aRule: TSVGFillRule);

var
  lScale: Double;

begin
  NeedFrame('PushClip');
  lScale := aCTM.MaxScale;
  if lScale > 0 then
    begin
    FSource.Flatten(aPath, TSVGMatrix.Identity, FFlatness / lScale);
    FPoly.Assign(FSource);
    FPoly.Transform(aCTM);
    end;
  ClipToPoly(aRule, lScale > 0);
end;


// The clip closes to the line the pen draws, which is the outline the
// path strokes to. It is built the way StrokePath builds it.
procedure TSVGSoftBackend.PushStrokeClip(aPath: TSVGPath;
  const aCTM: TSVGMatrix; const aPen: TSVGPen);

var
  lScale, lTolerance: Double;

begin
  NeedFrame('PushStrokeClip');
  lScale := aCTM.MaxScale;
  if (lScale > 0) and (aPen.Width > 0) then
    begin
    lTolerance := FFlatness / lScale;
    FSource.Flatten(aPath, TSVGMatrix.Identity, lTolerance);
    if aPen.IsDashed then
      begin
      FDashed.BuildDashes(FSource, aPen.Dashes, aPen.DashOffset);
      FPoly.BuildStroke(FDashed, aPen, lTolerance);
      end
    else
      FPoly.BuildStroke(FSource, aPen, lTolerance);
    FPoly.Transform(aCTM);
    end;
  ClipToPoly(frNonZero, (lScale > 0) and (aPen.Width > 0));
end;


procedure TSVGSoftBackend.SetColorInterpolation(
  aSpace: TSVGColorInterpolation);

begin
  FMixing := aSpace;
end;


procedure TSVGSoftBackend.PopClip;

begin
  if FClipCount = 0 then
    raise ESVGSoft.Create(SErrPopClipUnmatched);
  Dec(FClipCount);
  FClips[FClipCount].Data := nil;
  ApplyRasterClip;
end;


procedure TSVGSoftBackend.PushLayer(const aBounds: TSVGRect; aOpacity: Double;
  aIsolate: Boolean);

begin
  NeedFrame('PushLayer');
  if FLayerCount = Length(FLayers) then
    SetLength(FLayers, Max(4, FLayerCount * 2));
  FLayers[FLayerCount].Surface :=
    TSVGSoftSurface.Create(FImage.Width, FImage.Height);
  FLayers[FLayerCount].Opacity := SVGClamp(aOpacity, 0, 1);
  FLayers[FLayerCount].Bounds := aBounds;
  ClearSurface(FLayers[FLayerCount].Surface);
  FTarget := FLayers[FLayerCount].Surface;
  Inc(FLayerCount);
end;


procedure TSVGSoftBackend.CompositeLayer(const aLayer: TSVGSoftLayer);

var
  X, Y, lLeft, lTop, lRight, lBottom: Integer;
  lSource: TSVGColor;

begin
  lLeft := 0;
  lTop := 0;
  lRight := FImage.Width;
  lBottom := FImage.Height;
  if not aLayer.Bounds.IsEmpty then
    begin
    lLeft := Max(lLeft, Floor(aLayer.Bounds.Left));
    lTop := Max(lTop, Floor(aLayer.Bounds.Top));
    lRight := Min(lRight, Ceil(aLayer.Bounds.Right));
    lBottom := Min(lBottom, Ceil(aLayer.Bounds.Bottom));
    end;
  for Y := lTop to lBottom - 1 do
    for X := lLeft to lRight - 1 do
      begin
      lSource := TSVGColor(aLayer.Surface.Colors[X, Y]);
      if lSource.Alpha = 0 then
        Continue;
      BlendPixel(X, Y, lSource, Round(lSource.Alpha * aLayer.Opacity));
      end;
end;


procedure TSVGSoftBackend.PopLayer;

var
  lLayer: TSVGSoftLayer;

begin
  if FLayerCount = 0 then
    raise ESVGSoft.Create(SErrPopLayerUnmatched);
  Dec(FLayerCount);
  lLayer := FLayers[FLayerCount];
  if FLayerCount = 0 then
    FTarget := FImage
  else
    FTarget := FLayers[FLayerCount - 1].Surface;
  try
    CompositeLayer(lLayer);
  finally
    lLayer.Surface.Free;
    FLayers[FLayerCount].Surface := nil;
  end;
end;


// A number limited to the range of a channel.
function SVGClampWord(const aValue: Int64): Word;

begin
  if aValue < 0 then
    Result := 0
  else if aValue > 65535 then
    Result := 65535
  else
    Result := aValue;
end;


var
  { A channel converted to linear light and back again. Both directions
    map a channel value to a channel value, so each fits in a table and no
    power has to be computed per pixel. }
  SVGLinearOfChannel: array of Word;
  SVGSRGBOfChannel: array of Word;
  { A channel over its full value, the same division every time. }
  SVGUnitOfChannel: array of Double;


// Fills the channel maps, the first time any of them is wanted.
procedure SVGNeedChannelMaps;

var
  I: Integer;

begin
  if Length(SVGLinearOfChannel) = 65536 then
    Exit;
  SetLength(SVGLinearOfChannel, 65536);
  SetLength(SVGSRGBOfChannel, 65536);
  SetLength(SVGUnitOfChannel, 65536);
  for I := 0 to 65535 do
    begin
    SVGLinearOfChannel[I] := Round(SVGToLinear(I) * 65535);
    SVGSRGBOfChannel[I] := SVGFromLinear(I / 65535);
    SVGUnitOfChannel[I] := I / 65535;
    end;
end;


// Converts a whole surface to linear light, or back again. SVG applies a
// filter to linear light unless a document says otherwise. The alpha is
// left unchanged: it is not a colour.
procedure SVGTurnSurface(aSurface: TFPCustomImage; aToLinear: Boolean);

var
  X, Y: Integer;
  lColour: TSVGColor;

begin
  SVGNeedChannelMaps;
  for Y := 0 to aSurface.Height - 1 do
    for X := 0 to aSurface.Width - 1 do
      begin
      lColour := TSVGColor(aSurface.Colors[X, Y]);
      if lColour.Alpha = 0 then
        Continue;
      if aToLinear then
        begin
        lColour.Red := SVGLinearOfChannel[lColour.Red];
        lColour.Green := SVGLinearOfChannel[lColour.Green];
        lColour.Blue := SVGLinearOfChannel[lColour.Blue];
        end
      else
        begin
        lColour.Red := SVGSRGBOfChannel[lColour.Red];
        lColour.Green := SVGSRGBOfChannel[lColour.Green];
        lColour.Blue := SVGSRGBOfChannel[lColour.Blue];
        end;
      aSurface.Colors[X, Y] := TFPColor(lColour);
      end;
end;


// Fills the whole surface with one colour at one opacity, as a flood does
// to the region it writes.
procedure SVGFilterFlood(aOut: TFPCustomImage; const aColour: TSVGColor;
  aOpacity: Double);

var
  X, Y: Integer;
  lColour: TSVGColor;

begin
  lColour := aColour;
  lColour.Alpha := Round(SVGClamp(aColour.Alpha * aOpacity, 0, 65535));
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      aOut.Colors[X, Y] := TFPColor(lColour);
end;


// Moves a surface by whole pixels. An offset comes to this once its step
// has been mapped into the space that is drawn in.
procedure SVGFilterOffset(aIn, aOut: TFPCustomImage; aDX, aDY: Integer);

var
  X, Y, lX, lY: Integer;

begin
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      lX := X - aDX;
      lY := Y - aDY;
      if (lX < 0) or (lY < 0) or (lX >= aIn.Width) or (lY >= aIn.Height) then
        aOut.Colors[X, Y] := TFPColor(TSVGColor.Transparent)
      else
        aOut.Colors[X, Y] := aIn.Colors[lX, lY];
      end;
end;


// One pass of a box blur along a row or down a column, reading from
// aFrom to aTo either side of each pixel, on the premultiplied values so
// that a clear pixel lends no colour.
procedure SVGFilterBox(aIn, aOut: TFPCustomImage; aFrom, aTo: Integer;
  aDown: Boolean);

var
  I, J, K, lCount, lLength, lAcross: Integer;
  lRed, lGreen, lBlue, lAlpha, lMean: Int64;
  lColour: TSVGColor;

  // Brings the sample at one place along the line into the window or
  // takes it out again. A place outside the surface contributes nothing
  // but still counts towards the window, like a transparent sample.
  procedure Weigh(aAt, aSign: Integer);

  var
    lAt: TSVGColor;

  begin
    if (aAt < 0) or (aAt >= lLength) then
      Exit;
    if aDown then
      lAt := TSVGColor(aIn.Colors[I, aAt])
    else
      lAt := TSVGColor(aIn.Colors[aAt, I]);
    lRed := lRed + aSign * (Int64(lAt.Red) * lAt.Alpha div 65535);
    lGreen := lGreen + aSign * (Int64(lAt.Green) * lAt.Alpha div 65535);
    lBlue := lBlue + aSign * (Int64(lAt.Blue) * lAt.Alpha div 65535);
    lAlpha := lAlpha + aSign * Int64(lAt.Alpha);
  end;

begin
  if aDown then
    begin
    lLength := aIn.Height;
    lAcross := aIn.Width;
    end
  else
    begin
    lLength := aIn.Width;
    lAcross := aIn.Height;
    end;
  lCount := aTo - aFrom + 1;
  if lCount < 1 then
    lCount := 1;
  for I := 0 to lAcross - 1 do
    begin
    // The window keeps the sum of the places it covers, so sliding it
    // along the line only adds the sample it gains and subtracts the one
    // it drops.
    lRed := 0;
    lGreen := 0;
    lBlue := 0;
    lAlpha := 0;
    for K := aFrom to aTo do
      Weigh(K, 1);
    for J := 0 to lLength - 1 do
      begin
      lMean := lAlpha div lCount;
      lColour.Alpha := lMean;
      if lMean = 0 then
        begin
        lColour.Red := 0;
        lColour.Green := 0;
        lColour.Blue := 0;
        end
      else
        begin
        lColour.Red := SVGClampWord((lRed div lCount) * 65535 div lMean);
        lColour.Green := SVGClampWord((lGreen div lCount) * 65535 div lMean);
        lColour.Blue := SVGClampWord((lBlue div lCount) * 65535 div lMean);
        end;
      if aDown then
        aOut.Colors[I, J] := TFPColor(lColour)
      else
        aOut.Colors[J, I] := TFPColor(lColour);
      Weigh(J + aFrom, -1);
      Weigh(J + 1 + aTo, 1);
      end;
    end;
end;


// Lays one surface over another. A merge does this with each of its
// inputs, in the order they are given.
procedure SVGFilterOver(aOver, aUnder: TFPCustomImage);

var
  X, Y: Integer;
  lOver, lUnder: TSVGColor;
  lKept, lOut: Double;

begin
  for Y := 0 to aUnder.Height - 1 do
    for X := 0 to aUnder.Width - 1 do
      begin
      lOver := TSVGColor(aOver.Colors[X, Y]);
      if lOver.Alpha = 0 then
        Continue;
      lUnder := TSVGColor(aUnder.Colors[X, Y]);
      if lOver.Alpha = 65535 then
        begin
        aUnder.Colors[X, Y] := TFPColor(lOver);
        Continue;
        end;
      lKept := lUnder.Alpha / 65535 * (1 - lOver.Alpha / 65535);
      lOut := lOver.Alpha / 65535 + lKept;
      if lOut <= 0 then
        Continue;
      lUnder.Red := SVGClampWord(Round((lOver.Red * (lOver.Alpha / 65535)
        + lUnder.Red * lKept) / lOut));
      lUnder.Green := SVGClampWord(Round((lOver.Green * (lOver.Alpha / 65535)
        + lUnder.Green * lKept) / lOut));
      lUnder.Blue := SVGClampWord(Round((lOver.Blue * (lOver.Alpha / 65535)
        + lUnder.Blue * lKept) / lOut));
      lUnder.Alpha := SVGClampWord(Round(lOut * 65535));
      aUnder.Colors[X, Y] := TFPColor(lUnder);
      end;
end;


// The six ways SVG combines one surface with another. The first five are
// the Porter and Duff operators, and the sixth combines the two with four
// constants the document gives.
procedure SVGFilterComposite(aA, aB, aOut: TFPCustomImage;
  aOperator: Integer; const aK: TSVGDoubleArray);

var
  X, Y, I: Integer;
  lA, lB, lOut: TSVGColor;
  lFa, lFb, lAa, lAb, lValue: Double;
  lAin, lBin: array[0..3] of Double;

begin
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      lA := TSVGColor(aA.Colors[X, Y]);
      lB := TSVGColor(aB.Colors[X, Y]);
      lAa := lA.Alpha / 65535;
      lAb := lB.Alpha / 65535;
      case aOperator of
        1: begin lFa := lAb; lFb := 0; end;
        2: begin lFa := 1 - lAb; lFb := 0; end;
        3: begin lFa := lAb; lFb := 1 - lAa; end;
        4: begin lFa := 1 - lAb; lFb := 1 - lAa; end;
        5: begin lFa := 0; lFb := 0; end;
      else
        begin lFa := 1; lFb := 1 - lAa; end;
      end;
      // The values are weighted in premultiplied form, so a clear pixel
      // adds no colour to the pixel it is combined with.
      lAin[0] := lA.Red / 65535 * lAa;
      lAin[1] := lA.Green / 65535 * lAa;
      lAin[2] := lA.Blue / 65535 * lAa;
      lAin[3] := lAa;
      lBin[0] := lB.Red / 65535 * lAb;
      lBin[1] := lB.Green / 65535 * lAb;
      lBin[2] := lB.Blue / 65535 * lAb;
      lBin[3] := lAb;
      for I := 0 to 3 do
        begin
        if aOperator = 5 then
          lValue := aK[0] * lAin[I] * lBin[I] + aK[1] * lAin[I]
            + aK[2] * lBin[I] + aK[3]
        else
          lValue := lAin[I] * lFa + lBin[I] * lFb;
        lValue := SVGClamp(lValue, 0, 1);
        case I of
          0: lOut.Red := Round(lValue * 65535);
          1: lOut.Green := Round(lValue * 65535);
          2: lOut.Blue := Round(lValue * 65535);
          3: lOut.Alpha := Round(lValue * 65535);
        end;
        end;
      // Undo the premultiplying: the surfaces hold straight values.
      if lOut.Alpha > 0 then
        begin
        lOut.Red := SVGClampWord(Int64(lOut.Red) * 65535 div lOut.Alpha);
        lOut.Green := SVGClampWord(Int64(lOut.Green) * 65535 div lOut.Alpha);
        lOut.Blue := SVGClampWord(Int64(lOut.Blue) * 65535 div lOut.Alpha);
        end
      else
        begin
        lOut.Red := 0;
        lOut.Green := 0;
        lOut.Blue := 0;
        end;
      aOut.Colors[X, Y] := TFPColor(lOut);
      end;
end;


// The twenty numbers a colour matrix weights each channel by, built for
// the kind the document requested.
function SVGFilterMatrixOf(aKind: Integer;
  const aValues: TSVGDoubleArray): TSVGDoubleArray;

var
  I: Integer;
  lS, lAngle, lCos, lSin: Double;

begin
  SetLength(Result, 20);
  for I := 0 to 19 do
    Result[I] := 0;
  case aKind of
    1:
      begin
      lS := 1;
      if Length(aValues) > 0 then
        lS := aValues[0];
      Result[0] := 0.213 + 0.787 * lS;
      Result[1] := 0.715 - 0.715 * lS;
      Result[2] := 0.072 - 0.072 * lS;
      Result[5] := 0.213 - 0.213 * lS;
      Result[6] := 0.715 + 0.285 * lS;
      Result[7] := 0.072 - 0.072 * lS;
      Result[10] := 0.213 - 0.213 * lS;
      Result[11] := 0.715 - 0.715 * lS;
      Result[12] := 0.072 + 0.928 * lS;
      Result[18] := 1;
      end;
    2:
      begin
      lAngle := 0;
      if Length(aValues) > 0 then
        lAngle := aValues[0];
      lCos := Cos(DegToRad(lAngle));
      lSin := Sin(DegToRad(lAngle));
      Result[0] := 0.213 + lCos * 0.787 - lSin * 0.213;
      Result[1] := 0.715 - lCos * 0.715 - lSin * 0.715;
      Result[2] := 0.072 - lCos * 0.072 + lSin * 0.928;
      Result[5] := 0.213 - lCos * 0.213 + lSin * 0.143;
      Result[6] := 0.715 + lCos * 0.285 + lSin * 0.140;
      Result[7] := 0.072 - lCos * 0.072 - lSin * 0.283;
      Result[10] := 0.213 - lCos * 0.213 - lSin * 0.787;
      Result[11] := 0.715 - lCos * 0.715 + lSin * 0.715;
      Result[12] := 0.072 + lCos * 0.928 + lSin * 0.072;
      Result[18] := 1;
      end;
    3:
      begin
      Result[15] := 0.2125;
      Result[16] := 0.7154;
      Result[17] := 0.0721;
      end;
  else
    begin
    if Length(aValues) >= 20 then
      for I := 0 to 19 do
        Result[I] := aValues[I]
    else
      begin
      Result[0] := 1;
      Result[6] := 1;
      Result[12] := 1;
      Result[18] := 1;
      end;
    end;
  end;
end;


// Replaces every channel by a weighted sum of all four, in linear form.
procedure SVGFilterColorMatrix(aIn, aOut: TFPCustomImage;
  const aMatrix: TSVGDoubleArray);

var
  X, Y, I: Integer;
  lIn, lOut: TSVGColor;
  lChannel: array[0..3] of Double;
  lValue: Double;

begin
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      lIn := TSVGColor(aIn.Colors[X, Y]);
      lChannel[0] := lIn.Red / 65535;
      lChannel[1] := lIn.Green / 65535;
      lChannel[2] := lIn.Blue / 65535;
      lChannel[3] := lIn.Alpha / 65535;
      for I := 0 to 3 do
        begin
        lValue := aMatrix[I * 5] * lChannel[0]
          + aMatrix[I * 5 + 1] * lChannel[1]
          + aMatrix[I * 5 + 2] * lChannel[2]
          + aMatrix[I * 5 + 3] * lChannel[3] + aMatrix[I * 5 + 4];
        lValue := SVGClamp(lValue, 0, 1);
        case I of
          0: lOut.Red := Round(lValue * 65535);
          1: lOut.Green := Round(lValue * 65535);
          2: lOut.Blue := Round(lValue * 65535);
          3: lOut.Alpha := Round(lValue * 65535);
        end;
        end;
      aOut.Colors[X, Y] := TFPColor(lOut);
      end;
end;


// The five ways SVG puts one surface over another beyond a plain over.
procedure SVGFilterBlend(aA, aB, aOut: TFPCustomImage; aMode: Integer);

var
  X, Y, I: Integer;
  lA, lB, lOut: TSVGColor;
  lQa, lQb, lCa, lCb, lValue, lAlpha: Double;

begin
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      lA := TSVGColor(aA.Colors[X, Y]);
      lB := TSVGColor(aB.Colors[X, Y]);
      lQa := lA.Alpha / 65535;
      lQb := lB.Alpha / 65535;
      lAlpha := lQa + lQb - lQa * lQb;
      for I := 0 to 2 do
        begin
        case I of
          0: begin lCa := lA.Red / 65535 * lQa; lCb := lB.Red / 65535 * lQb; end;
          1: begin lCa := lA.Green / 65535 * lQa; lCb := lB.Green / 65535 * lQb; end;
        else
          begin lCa := lA.Blue / 65535 * lQa; lCb := lB.Blue / 65535 * lQb; end;
        end;
        case aMode of
          1: lValue := (1 - lQa) * lCb + (1 - lQb) * lCa + lCa * lCb;
          2: lValue := lCa + lCb - lCa * lCb;
          3: lValue := Min((1 - lQa) * lCb + lCa, (1 - lQb) * lCa + lCb);
          4: lValue := Max((1 - lQa) * lCb + lCa, (1 - lQb) * lCa + lCb);
        else
          lValue := lCa + lCb * (1 - lQa);
        end;
        lValue := SVGClamp(lValue, 0, 1);
        if lAlpha > 0 then
          lValue := lValue / lAlpha;
        lValue := SVGClamp(lValue, 0, 1);
        case I of
          0: lOut.Red := Round(lValue * 65535);
          1: lOut.Green := Round(lValue * 65535);
        else
          lOut.Blue := Round(lValue * 65535);
        end;
        end;
      lOut.Alpha := Round(SVGClamp(lAlpha, 0, 1) * 65535);
      aOut.Colors[X, Y] := TFPColor(lOut);
      end;
end;


// The smallest or the largest value of every channel over a box, which is
// how SVG thins or thickens a shape.
procedure SVGFilterMorphology(aIn, aOut: TFPCustomImage;
  aRadiusX, aRadiusY, aOperator: Integer);

var
  X, Y, I, J: Integer;
  lColour, lPick: TSVGColor;

  procedure Take(const aValue: TSVGColor);
  begin
    if aOperator = 1 then
      begin
      lPick.Red := Max(lPick.Red, aValue.Red);
      lPick.Green := Max(lPick.Green, aValue.Green);
      lPick.Blue := Max(lPick.Blue, aValue.Blue);
      lPick.Alpha := Max(lPick.Alpha, aValue.Alpha);
      end
    else
      begin
      lPick.Red := Min(lPick.Red, aValue.Red);
      lPick.Green := Min(lPick.Green, aValue.Green);
      lPick.Blue := Min(lPick.Blue, aValue.Blue);
      lPick.Alpha := Min(lPick.Alpha, aValue.Alpha);
      end;
  end;

begin
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      if aOperator = 1 then
        lPick := TSVGColor.FromBytes(0, 0, 0, 0)
      else
        lPick := TSVGColor.FromBytes(255, 255, 255, 255);
      for J := Y - aRadiusY to Y + aRadiusY do
        for I := X - aRadiusX to X + aRadiusX do
          begin
          if (I < 0) or (J < 0) or (I >= aIn.Width) or (J >= aIn.Height) then
            lColour := TSVGColor.Transparent
          else
            lColour := TSVGColor(aIn.Colors[I, J]);
          Take(lColour);
          end;
      aOut.Colors[X, Y] := TFPColor(lPick);
      end;
end;


// Applies the transfer function of one channel, in the five kinds SVG
// defines.
function SVGFilterTransferOf(const aTransfer: TSVGFilterTransfer;
  aValue: Double): Double;

var
  lCount, lAt: Integer;

begin
  Result := aValue;
  lCount := Length(aTransfer.Table);
  case aTransfer.Kind of
    1:
      begin
      if lCount = 0 then
        Exit;
      if lCount = 1 then
        Exit(aTransfer.Table[0]);
      lAt := Trunc(aValue * (lCount - 1));
      if lAt > lCount - 2 then
        lAt := lCount - 2;
      Result := aTransfer.Table[lAt] + (aValue * (lCount - 1) - lAt)
        * (aTransfer.Table[lAt + 1] - aTransfer.Table[lAt]);
      end;
    2:
      begin
      if lCount = 0 then
        Exit;
      lAt := Trunc(aValue * lCount);
      if lAt > lCount - 1 then
        lAt := lCount - 1;
      Result := aTransfer.Table[lAt];
      end;
    3: Result := aTransfer.Slope * aValue + aTransfer.Intercept;
    4: Result := aTransfer.Amplitude * Power(aValue, aTransfer.Exponent)
         + aTransfer.Offset;
  end;
  Result := SVGClamp(Result, 0, 1);
end;


// Puts every channel through the transfer function of that channel.
procedure SVGFilterComponents(aIn, aOut: TFPCustomImage;
  const aTransfer: TSVGFilterTransferArray);

var
  X, Y: Integer;
  lIn, lOut: TSVGColor;
  lAlpha: Double;

begin
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      lIn := TSVGColor(aIn.Colors[X, Y]);
      lAlpha := SVGFilterTransferOf(aTransfer[3], lIn.Alpha / 65535);
      lOut.Red := Round(SVGFilterTransferOf(aTransfer[0],
        lIn.Red / 65535) * 65535);
      lOut.Green := Round(SVGFilterTransferOf(aTransfer[1],
        lIn.Green / 65535) * 65535);
      lOut.Blue := Round(SVGFilterTransferOf(aTransfer[2],
        lIn.Blue / 65535) * 65535);
      lOut.Alpha := Round(lAlpha * 65535);
      aOut.Colors[X, Y] := TFPColor(lOut);
      end;
end;


{ The generator SVG writes out in its own appendix. The numbers it gives
  are the ones a reference was drawn with, so it is followed to the letter
  rather than replaced with any other noise. }
type
  TSVGTurbulence = class(TObject)
  private
    FLattice: array[0..2 * 256 + 1] of Integer;
    FGradient: array[0..3, 0..2 * 256 + 1, 0..1] of Double;
    function Noise2(aChannel: Integer; aX, aY: Double): Double;
  public
    constructor Create(aSeed: Integer);
    // The value of the noise at a point, over as many octaves as
    // requested.
    function Turbulence(aChannel: Integer; aX, aY, aBaseX, aBaseY: Double;
      aOctaves: Integer; aFractal: Boolean): Double;
  end;

const
  SVGTurbulenceRandM = 2147483647;
  SVGTurbulenceRandA = 16807;
  SVGTurbulenceRandQ = 127773;
  SVGTurbulenceRandR = 2836;
  SVGTurbulenceSize = 256;


// The next number of the generator from the SVG appendix. The lattice and
// the gradients are built from it.
function SVGTurbulenceNext(aSeed: Integer): Integer;

var
  lHigh, lLow: Integer;

begin
  lHigh := aSeed div SVGTurbulenceRandQ;
  lLow := aSeed mod SVGTurbulenceRandQ;
  Result := SVGTurbulenceRandA * lLow - SVGTurbulenceRandR * lHigh;
  if Result <= 0 then
    Result := Result + SVGTurbulenceRandM;
end;


constructor TSVGTurbulence.Create(aSeed: Integer);

var
  I, J, K, lSwap: Integer;
  lSeed: Integer;
  lLength: Double;

begin
  inherited Create;
  lSeed := aSeed;
  if lSeed <= 0 then
    lSeed := -(lSeed mod (SVGTurbulenceRandM - 1)) + 1;
  if lSeed > SVGTurbulenceRandM - 1 then
    lSeed := SVGTurbulenceRandM - 1;
  for K := 0 to 3 do
    for I := 0 to SVGTurbulenceSize - 1 do
      begin
      if K = 0 then
        FLattice[I] := I;
      for J := 0 to 1 do
        begin
        lSeed := SVGTurbulenceNext(lSeed);
        FGradient[K, I, J] := ((lSeed mod (SVGTurbulenceSize + SVGTurbulenceSize))
          - SVGTurbulenceSize) / SVGTurbulenceSize;
        end;
      lLength := Sqrt(FGradient[K, I, 0] * FGradient[K, I, 0]
        + FGradient[K, I, 1] * FGradient[K, I, 1]);
      if lLength > 0 then
        begin
        FGradient[K, I, 0] := FGradient[K, I, 0] / lLength;
        FGradient[K, I, 1] := FGradient[K, I, 1] / lLength;
        end;
      end;
  I := SVGTurbulenceSize - 1;
  while I > 0 do
    begin
    lSwap := FLattice[I];
    lSeed := SVGTurbulenceNext(lSeed);
    J := lSeed mod SVGTurbulenceSize;
    FLattice[I] := FLattice[J];
    FLattice[J] := lSwap;
    Dec(I);
    end;
  for I := 0 to SVGTurbulenceSize + 1 do
    begin
    FLattice[SVGTurbulenceSize + I] := FLattice[I];
    for K := 0 to 3 do
      for J := 0 to 1 do
        FGradient[K, SVGTurbulenceSize + I, J] := FGradient[K, I, J];
    end;
end;


function TSVGTurbulence.Noise2(aChannel: Integer; aX, aY: Double): Double;

var
  lBX0, lBX1, lBY0, lBY1, lI, lJ, lB00, lB10, lB01, lB11: Integer;
  lRX0, lRX1, lRY0, lRY1, lSX, lSY, lA, lB, lU, lV: Double;
  lT: Double;

  function SCurve(aValue: Double): Double;
  begin
    Result := aValue * aValue * (3 - 2 * aValue);
  end;

begin
  lT := aX + 4096;
  lBX0 := Trunc(lT) and (SVGTurbulenceSize - 1);
  lBX1 := (lBX0 + 1) and (SVGTurbulenceSize - 1);
  lRX0 := lT - Trunc(lT);
  lRX1 := lRX0 - 1;
  lT := aY + 4096;
  lBY0 := Trunc(lT) and (SVGTurbulenceSize - 1);
  lBY1 := (lBY0 + 1) and (SVGTurbulenceSize - 1);
  lRY0 := lT - Trunc(lT);
  lRY1 := lRY0 - 1;
  lI := FLattice[lBX0];
  lJ := FLattice[lBX1];
  lB00 := FLattice[lI + lBY0];
  lB10 := FLattice[lJ + lBY0];
  lB01 := FLattice[lI + lBY1];
  lB11 := FLattice[lJ + lBY1];
  lSX := SCurve(lRX0);
  lSY := SCurve(lRY0);
  lU := lRX0 * FGradient[aChannel, lB00, 0]
    + lRY0 * FGradient[aChannel, lB00, 1];
  lV := lRX1 * FGradient[aChannel, lB10, 0]
    + lRY0 * FGradient[aChannel, lB10, 1];
  lA := lU + lSX * (lV - lU);
  lU := lRX0 * FGradient[aChannel, lB01, 0]
    + lRY1 * FGradient[aChannel, lB01, 1];
  lV := lRX1 * FGradient[aChannel, lB11, 0]
    + lRY1 * FGradient[aChannel, lB11, 1];
  lB := lU + lSX * (lV - lU);
  Result := lA + lSY * (lB - lA);
end;


function TSVGTurbulence.Turbulence(aChannel: Integer; aX, aY,
  aBaseX, aBaseY: Double; aOctaves: Integer; aFractal: Boolean): Double;

var
  I: Integer;
  lRatio, lVX, lVY: Double;

begin
  Result := 0;
  lRatio := 1;
  lVX := aX * aBaseX;
  lVY := aY * aBaseY;
  for I := 1 to aOctaves do
    begin
    if aFractal then
      Result := Result + Noise2(aChannel, lVX, lVY) / lRatio
    else
      Result := Result + Abs(Noise2(aChannel, lVX, lVY)) / lRatio;
    lVX := lVX * 2;
    lVY := lVY * 2;
    lRatio := lRatio * 2;
    end;
end;


// A pixel of a surface, with the edge handled as the mode specifies: the
// nearest pixel repeated, the surface wrapped round, or nothing at all.
function SVGFilterAt(aIn: TFPCustomImage; aX, aY, aMode: Integer): TSVGColor;

begin
  case aMode of
    1:
      begin
      aX := ((aX mod aIn.Width) + aIn.Width) mod aIn.Width;
      aY := ((aY mod aIn.Height) + aIn.Height) mod aIn.Height;
      end;
    2:
      if (aX < 0) or (aY < 0) or (aX >= aIn.Width) or (aY >= aIn.Height) then
        Exit(TSVGColor.Transparent);
  else
    begin
    aX := Min(Max(aX, 0), aIn.Width - 1);
    aY := Min(Max(aY, 0), aIn.Height - 1);
    end;
  end;
  if (aX < 0) or (aY < 0) or (aX >= aIn.Width) or (aY >= aIn.Height) then
    Exit(TSVGColor.Transparent);
  Result := TSVGColor(aIn.Colors[aX, aY]);
end;


// Weights each pixel by a grid of numbers laid over it and the pixels
// around it. The grid is read backwards, as a convolution does.
procedure SVGFilterConvolve(aIn, aOut: TFPCustomImage;
  const aPrimitive: TSVGFilterPrimitive);

var
  X, Y, I, J, lWide, lTall, lAt: Integer;
  lLeft, lUp, lMode: Integer;
  lSum: array[0..3] of Double;
  lWeights: TSVGDoubleArray;
  lDivisor, lWeight, lValue, lAlpha, lBias: Double;
  lPreserve: Boolean;
  lColour, lOut: TSVGColor;

begin
  lWide := aPrimitive.Order[0];
  lTall := aPrimitive.Order[1];
  if Length(aPrimitive.Numbers) < lWide * lTall then
    begin
    aOut.Assign(aIn);
    Exit;
    end;
  lDivisor := aPrimitive.Divisor;
  if lDivisor = 0 then
    begin
    for I := 0 to lWide * lTall - 1 do
      lDivisor := lDivisor + aPrimitive.Numbers[I];
    if lDivisor = 0 then
      lDivisor := 1;
    end;
  // The grid, its position and its edge handling are the same for the
  // whole surface, and the grid is read from its end backwards.
  lWeights := aPrimitive.Numbers;
  lLeft := aPrimitive.Target[0];
  lUp := aPrimitive.Target[1];
  lMode := aPrimitive.Operation;
  lPreserve := aPrimitive.Preserve;
  lBias := aPrimitive.Bias;
  SVGNeedChannelMaps;
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      for I := 0 to 3 do
        lSum[I] := 0;
      lAt := lWide * lTall - 1;
      for J := 0 to lTall - 1 do
        for I := 0 to lWide - 1 do
          begin
          lWeight := lWeights[lAt];
          Dec(lAt);
          lColour := SVGFilterAt(aIn, X - lLeft + I, Y - lUp + J, lMode);
          if lPreserve then
            lAlpha := 1
          else
            lAlpha := SVGUnitOfChannel[lColour.Alpha];
          lSum[0] := lSum[0] + SVGUnitOfChannel[lColour.Red]
            * lAlpha * lWeight;
          lSum[1] := lSum[1] + SVGUnitOfChannel[lColour.Green]
            * lAlpha * lWeight;
          lSum[2] := lSum[2] + SVGUnitOfChannel[lColour.Blue]
            * lAlpha * lWeight;
          lSum[3] := lSum[3] + SVGUnitOfChannel[lColour.Alpha] * lWeight;
          end;
      // The bias is applied to the colour channels alone. SVG 1.1 adds
      // it to the alpha as well, which would make a biased filter
      // opaque.
      if lPreserve then
        lOut.Alpha := TSVGColor(aIn.Colors[X, Y]).Alpha
      else
        lOut.Alpha := Round(SVGClamp(lSum[3] / lDivisor, 0, 1) * 65535);
      lAlpha := lOut.Alpha / 65535;
      for I := 0 to 2 do
        begin
        lValue := lSum[I] / lDivisor + lBias * lAlpha;
        if lAlpha > 0 then
          lValue := lValue / lAlpha;
        lValue := SVGClamp(lValue, 0, 1);
        case I of
          0: lOut.Red := Round(lValue * 65535);
          1: lOut.Green := Round(lValue * 65535);
        else
          lOut.Blue := Round(lValue * 65535);
        end;
        end;
      aOut.Colors[X, Y] := TFPColor(lOut);
      end;
end;


// Moves each pixel by the values of two channels of another surface, as a
// displacement map does.
procedure SVGFilterDisplace(aIn, aMap, aOut: TFPCustomImage;
  aScale: Double; aXChannel, aYChannel: Integer);

  function ChannelOf(const aColour: TSVGColor; aWhich: Integer): Double;
  begin
    case aWhich of
      0: Result := aColour.Red / 65535;
      1: Result := aColour.Green / 65535;
      2: Result := aColour.Blue / 65535;
    else
      Result := aColour.Alpha / 65535;
    end;
  end;

var
  X, Y, lX, lY: Integer;
  lMap: TSVGColor;

begin
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      lMap := TSVGColor(aMap.Colors[X, Y]);
      lX := X + Round(aScale * (ChannelOf(lMap, aXChannel) - 0.5));
      lY := Y + Round(aScale * (ChannelOf(lMap, aYChannel) - 0.5));
      if (lX < 0) or (lY < 0) or (lX >= aIn.Width) or (lY >= aIn.Height) then
        aOut.Colors[X, Y] := TFPColor(TSVGColor.Transparent)
      else
        aOut.Colors[X, Y] := aIn.Colors[lX, lY];
      end;
end;


// Fills a surface with the noise the generator gives, one channel at a
// time. A fractal noise is centred on a half, a turbulence on zero.
procedure SVGFilterTurbulence(aOut: TFPCustomImage;
  const aPrimitive: TSVGFilterPrimitive; const aCTM: TSVGMatrix);

var
  X, Y, K, lOctaves: Integer;
  lNoise: TSVGTurbulence;
  lValue: Double;
  lColour: TSVGColor;
  lFractal: Boolean;
  lInverse: TSVGMatrix;
  lAt: TSVGPoint;

begin
  // The noise is generated in the space the document draws in, so a shape
  // with its own transform gets the same noise wherever it is placed on
  // the page.
  if not aCTM.Invert(lInverse) then
    Exit;
  lOctaves := Max(0, Round(aPrimitive.Numbers[2]));
  lFractal := aPrimitive.Operation = 1;
  // SVG passes the seed to its generator as a whole number, cutting off
  // the fraction instead of rounding it.
  lNoise := TSVGTurbulence.Create(Trunc(aPrimitive.Numbers[3]));
  try
    for Y := 0 to aOut.Height - 1 do
      for X := 0 to aOut.Width - 1 do
        begin
        lAt := lInverse.Transform(TSVGPoint.Create(X, Y));
        for K := 0 to 3 do
          begin
          lValue := lNoise.Turbulence(K, lAt.X, lAt.Y,
            aPrimitive.Numbers[0], aPrimitive.Numbers[1], lOctaves,
            lFractal);
          if lFractal then
            lValue := (lValue + 1) / 2;
          lValue := SVGClamp(lValue, 0, 1);
          case K of
            0: lColour.Red := Round(lValue * 65535);
            1: lColour.Green := Round(lValue * 65535);
            2: lColour.Blue := Round(lValue * 65535);
          else
            lColour.Alpha := Round(lValue * 65535);
          end;
          end;
        aOut.Colors[X, Y] := TFPColor(lColour);
        end;
  finally
    lNoise.Free;
  end;
end;


// Lights a surface, reading its alpha as a height, as the two lighting
// primitives do. Numbers three and up hold the kind of light and its
// position.
procedure SVGFilterLighting(aIn, aOut: TFPCustomImage;
  const aPrimitive: TSVGFilterPrimitive; aSpecular: Boolean);

var
  X, Y, I, lWide, lTall, lKind: Integer;
  lNX, lNY, lNZ, lLength: Double;
  lLX, lLY, lLZ, lHX, lHY, lHZ, lDot, lValue: Double;
  lSX, lSY, lSZ, lSpot, lSpan, lCone: Double;
  lRed, lGreen, lBlue: Double;
  lColour, lOut: TSVGColor;
  lChannel: array[0..2] of Double;
  // The height of every pixel, worked out once rather than thirteen
  // times over as the window walks across it.
  lHeights: array of Double;

  // The height of the surface at a pixel, which is its alpha scaled.
  function HeightAt(aX, aY: Integer): Double;
  begin
    if aX < 0 then
      aX := 0
    else if aX >= lWide then
      aX := lWide - 1;
    if aY < 0 then
      aY := 0
    else if aY >= lTall then
      aY := lTall - 1;
    Result := lHeights[aY * lWide + aX];
  end;

begin
  lWide := aIn.Width;
  lTall := aIn.Height;
  if (lWide <= 0) or (lTall <= 0) then
    Exit;
  SetLength(lHeights, lWide * lTall);
  for Y := 0 to lTall - 1 do
    for X := 0 to lWide - 1 do
      lHeights[Y * lWide + X] := TSVGColor(aIn.Colors[X, Y]).Alpha / 65535
        * aPrimitive.Numbers[0];
  lKind := Round(aPrimitive.Numbers[3]);
  lColour := aPrimitive.Colour;
  lRed := lColour.Red / 65535;
  lGreen := lColour.Green / 65535;
  lBlue := lColour.Blue / 65535;
  // The direction of a distant light is the same over the whole surface,
  // and so is the cone of a spot light.
  lLX := 0;
  lLY := 0;
  lLZ := 0;
  if not (lKind in [1, 2]) then
    begin
    lLX := Cos(DegToRad(aPrimitive.Numbers[4]))
      * Cos(DegToRad(aPrimitive.Numbers[5]));
    lLY := Sin(DegToRad(aPrimitive.Numbers[4]))
      * Cos(DegToRad(aPrimitive.Numbers[5]));
    lLZ := Sin(DegToRad(aPrimitive.Numbers[5]));
    end;
  lSX := 0;
  lSY := 0;
  lSZ := 0;
  lSpan := 1;
  lCone := 0;
  if lKind = 2 then
    begin
    lSX := aPrimitive.Numbers[7] - aPrimitive.Numbers[4];
    lSY := aPrimitive.Numbers[8] - aPrimitive.Numbers[5];
    lSZ := aPrimitive.Numbers[9] - aPrimitive.Numbers[6];
    lSpan := Sqrt(lSX * lSX + lSY * lSY + lSZ * lSZ);
    if lSpan = 0 then
      lSpan := 1;
    if aPrimitive.Numbers[11] >= 0 then
      lCone := Cos(DegToRad(aPrimitive.Numbers[11]));
    end;
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      // The slope of the height, which is the normal of the surface. SVG
      // gives the row and the column of the point double weight and takes
      // a quarter of the sum, so a slope of one over two pixels comes out
      // as one and not as a half.
      lNX := -(HeightAt(X + 1, Y - 1) + 2 * HeightAt(X + 1, Y)
        + HeightAt(X + 1, Y + 1) - HeightAt(X - 1, Y - 1)
        - 2 * HeightAt(X - 1, Y) - HeightAt(X - 1, Y + 1)) / 4;
      lNY := -(HeightAt(X - 1, Y + 1) + 2 * HeightAt(X, Y + 1)
        + HeightAt(X + 1, Y + 1) - HeightAt(X - 1, Y - 1)
        - 2 * HeightAt(X, Y - 1) - HeightAt(X + 1, Y - 1)) / 4;
      lNZ := 1;
      lLength := Sqrt(lNX * lNX + lNY * lNY + 1);
      lNX := lNX / lLength;
      lNY := lNY / lLength;
      lNZ := lNZ / lLength;
      if lKind in [1, 2] then
        begin
        lLX := aPrimitive.Numbers[4] - X;
        lLY := aPrimitive.Numbers[5] - Y;
        lLZ := aPrimitive.Numbers[6] - HeightAt(X, Y);
        lLength := Sqrt(lLX * lLX + lLY * lLY + lLZ * lLZ);
        if lLength = 0 then
          lLength := 1;
        lLX := lLX / lLength;
        lLY := lLY / lLength;
        lLZ := lLZ / lLength;
        end;
      // A spot lights the cone it points down and nothing outside it.
      lSpot := 1;
      if lKind = 2 then
        begin
        // The light points towards the surface, so the cone is measured
        // against the direction back to the light.
        lValue := (-lLX * lSX - lLY * lSY - lLZ * lSZ) / lSpan;
        if (lValue <= 0)
           or ((aPrimitive.Numbers[11] >= 0) and (lValue < lCone)) then
          lSpot := 0
        else
          lSpot := Power(lValue, aPrimitive.Numbers[10]);
        end;
      if aSpecular then
        begin
        lHX := lLX;
        lHY := lLY;
        lHZ := lLZ + 1;
        lLength := Sqrt(lHX * lHX + lHY * lHY + lHZ * lHZ);
        // A light shining straight away from the eye gives a halfway
        // vector of zero, which has no direction.
        if lLength < 1E-9 then
          lDot := 0
        else
          begin
          lDot := (lNX * lHX + lNY * lHY + lNZ * lHZ) / lLength;
          if lDot < 0 then
            lDot := 0;
          lDot := aPrimitive.Numbers[1] * Power(lDot, aPrimitive.Numbers[2]);
          end;
        end
      else
        begin
        lDot := lNX * lLX + lNY * lLY + lNZ * lLZ;
        if lDot < 0 then
          lDot := 0;
        lDot := aPrimitive.Numbers[1] * lDot;
        end;
      lChannel[0] := lDot * lSpot * lRed;
      lChannel[1] := lDot * lSpot * lGreen;
      lChannel[2] := lDot * lSpot * lBlue;
      for I := 0 to 2 do
        lChannel[I] := SVGClamp(lChannel[I], 0, 1);
      lOut.Red := Round(lChannel[0] * 65535);
      lOut.Green := Round(lChannel[1] * 65535);
      lOut.Blue := Round(lChannel[2] * 65535);
      if aSpecular then
        lOut.Alpha := Round(Max(lChannel[0], Max(lChannel[1],
          lChannel[2])) * 65535)
      else
        lOut.Alpha := 65535;
      aOut.Colors[X, Y] := TFPColor(lOut);
      end;
end;


// Lays an image over a box of a surface, taking the nearest pixel of the
// image for each pixel of the box. A filter image covers its region and no
// more.
procedure SVGFilterImage(aOut: TFPCustomImage; aImage: ISVGImageSource;
  const aBox: TSVGRect; const aRatio: TSVGPreserveAspectRatio);

var
  X, Y, U, V, lLeft, lTop, lRight, lBottom: Integer;
  lFit, lBack: TSVGMatrix;
  lAt: TSVGPoint;

begin
  if (aImage = nil) or (aImage.GetWidth <= 0) or (aImage.GetHeight <= 0) then
    Exit;
  lLeft := Max(0, Floor(aBox.Left));
  lTop := Max(0, Floor(aBox.Top));
  lRight := Min(aOut.Width, Ceil(aBox.Right));
  lBottom := Min(aOut.Height, Ceil(aBox.Bottom));
  if (lRight <= lLeft) or (lBottom <= lTop) then
    Exit;
  // The image is fitted into its box as the document specifies, which
  // keeps its shape instead of stretching it to the box.
  lFit := aRatio.ViewBoxTransform(TSVGRect.CreateSize(0, 0,
    aImage.GetWidth, aImage.GetHeight), aBox);
  if not lFit.Invert(lBack) then
    Exit;
  for Y := lTop to lBottom - 1 do
    for X := lLeft to lRight - 1 do
      begin
      lAt := lBack.Transform(TSVGPoint.Create(X + 0.5, Y + 0.5));
      U := Floor(lAt.X);
      V := Floor(lAt.Y);
      if (U < 0) or (V < 0) or (U >= aImage.GetWidth)
         or (V >= aImage.GetHeight) then
        Continue;
      aOut.Colors[X, Y] := TFPColor(aImage.GetPixel(U, V));
      end;
end;


// Lays the box a primitive wrote over the whole region, again and again.
procedure SVGFilterTile(aIn, aOut: TFPCustomImage; const aTile: TSVGRect);

var
  X, Y, lLeft, lTop, lWide, lTall, lX, lY: Integer;

begin
  lLeft := Floor(aTile.Left);
  lTop := Floor(aTile.Top);
  lWide := Ceil(aTile.Right) - lLeft;
  lTall := Ceil(aTile.Bottom) - lTop;
  if (lWide <= 0) or (lTall <= 0) then
    Exit;
  for Y := 0 to aOut.Height - 1 do
    for X := 0 to aOut.Width - 1 do
      begin
      lX := lLeft + ((X - lLeft) mod lWide + lWide) mod lWide;
      lY := lTop + ((Y - lTop) mod lTall + lTall) mod lTall;
      if (lX < 0) or (lY < 0) or (lX >= aIn.Width) or (lY >= aIn.Height) then
        Continue;
      aOut.Colors[X, Y] := aIn.Colors[lX, lY];
      end;
end;


// Clears everything a primitive wrote outside the box it was given.
procedure SVGFilterHoldTo(aSurface: TFPCustomImage; const aBox: TSVGRect);

var
  X, Y, lLeft, lTop, lRight, lBottom: Integer;

begin
  // A box with no readable edge clips nothing.
  if IsNan(aBox.Left) or IsNan(aBox.Top) or IsNan(aBox.Right)
     or IsNan(aBox.Bottom) or IsInfinite(aBox.Left) or IsInfinite(aBox.Top)
     or IsInfinite(aBox.Right) or IsInfinite(aBox.Bottom) then
    Exit;
  lLeft := Max(0, Floor(aBox.Left));
  lTop := Max(0, Floor(aBox.Top));
  lRight := Min(aSurface.Width, Ceil(aBox.Right));
  lBottom := Min(aSurface.Height, Ceil(aBox.Bottom));
  for Y := 0 to aSurface.Height - 1 do
    for X := 0 to aSurface.Width - 1 do
      if (X < lLeft) or (Y < lTop) or (X >= lRight) or (Y >= lBottom) then
        aSurface.Colors[X, Y] := TFPColor(TSVGColor.Transparent);
end;


// The width of the box blur that replaces a Gaussian of that deviation.
// SVG runs it three times, and the three together are close enough to a
// Gaussian that the specification prescribes this instead of the Gaussian
// itself.
function SVGFilterBoxWidth(aDeviation: Double): Integer;

begin
  Result := Floor(aDeviation * 3 * Sqrt(2 * Pi) / 4 + 0.5);
  if Result < 0 then
    Result := 0;
end;


// The window one pass of a box blur reads. SVG centres all three passes
// on the pixel for an odd width; for an even one the first two sit on the
// boundary either side of it and the third is a pixel wider.
procedure SVGFilterBoxWindow(aWidth, aPass: Integer;
  out aFrom, aTo: Integer);

begin
  aFrom := 0;
  aTo := 0;
  if aWidth <= 0 then
    Exit;
  if Odd(aWidth) then
    begin
    aFrom := -(aWidth div 2);
    aTo := aWidth div 2;
    Exit;
    end;
  case aPass of
    1:
      begin
      aFrom := -(aWidth div 2);
      aTo := aWidth div 2 - 1;
      end;
    2:
      begin
      aFrom := -(aWidth div 2) + 1;
      aTo := aWidth div 2;
      end;
  else
    begin
    aFrom := -(aWidth div 2);
    aTo := aWidth div 2;
    end;
  end;
end;


// Keeps the drawing of the layer as the result of one primitive. It is
// held until the chain that uses it has been applied.
procedure TSVGSoftBackend.PopLayerAsFilterImage(aIndex: Integer);

var
  lLayer: TSVGSoftLayer;
  I: Integer;

begin
  if FLayerCount = 0 then
    raise ESVGSoft.Create(SErrPopLayerAsFilterImageUnmatched);
  Dec(FLayerCount);
  lLayer := FLayers[FLayerCount];
  FLayers[FLayerCount].Surface := nil;
  if FLayerCount = 0 then
    FTarget := FImage
  else
    FTarget := FLayers[FLayerCount - 1].Surface;
  if aIndex < 0 then
    begin
    lLayer.Surface.Free;
    Exit;
    end;
  if aIndex > High(FFilterImages) then
    begin
    I := Length(FFilterImages);
    SetLength(FFilterImages, aIndex + 1);
    while I <= aIndex do
      begin
      FFilterImages[I] := nil;
      Inc(I);
      end;
    end;
  FFilterImages[aIndex].Free;
  FFilterImages[aIndex] := lLayer.Surface;
end;


// Lets go of the layers kept for the primitives of a chain.
procedure TSVGSoftBackend.ClearFilterImages;

var
  I: Integer;

begin
  for I := 0 to High(FFilterImages) do
    FFilterImages[I].Free;
  FFilterImages := nil;
end;


procedure TSVGSoftBackend.PopLayerAsFilter(const aChain: TSVGFilterChain);

var
  lLayer: TSVGSoftLayer;
  lResults: array of TFPCustomImage;
  lSource, lAlpha, lWork, lIn, lIn2, lOut: TFPCustomImage;
  lFill, lStroke, lBack, lBackAlpha: TFPCustomImage;
  I, J, lWide, lTall, lFrom, lTo: Integer;
  lScale: Double;
  lBox: TSVGRect;
  lColour: TSVGColor;
  lHeld: TSVGSoftLayer;

  // A surface the size of the frame, cleared.
  function NewSurface: TFPCustomImage;
  begin
    Result := TSVGSoftSurface.Create(FImage.Width, FImage.Height);
    ClearSurface(Result);
  end;

  // The alpha of a surface in black, which is what an alpha source
  // contains.
  function AlphaOf(aSurface: TFPCustomImage): TFPCustomImage;
  var
    X, Y: Integer;
    lPixel: TSVGColor;
  begin
    Result := NewSurface;
    for Y := 0 to aSurface.Height - 1 do
      for X := 0 to aSurface.Width - 1 do
        begin
        lPixel := TSVGColor.Black;
        lPixel.Alpha := TSVGColor(aSurface.Colors[X, Y]).Alpha;
        Result.Colors[X, Y] := TFPColor(lPixel);
        end;
  end;

  // A whole plane filled with one paint, as FillPaint and StrokePaint
  // are. A gradient is read against the box of the element and not of the
  // plane. A paint that is neither a colour nor a gradient gives no
  // plane.
  function PlaneOf(const aPaint: TSVGPaint;
    aOpacity: Double): TFPCustomImage;
  var
    X, Y: Integer;
    lColour: TSVGColor;
  begin
    Result := NewSurface;
    case aPaint.Kind of
      spColor:
        SVGFilterFlood(Result, aPaint.Color, aOpacity);
      spServer:
        begin
        if not PrepareGradient(aPaint, aChain.CTM, aChain.Bounds) then
          Exit;
        for Y := 0 to Result.Height - 1 do
          for X := 0 to Result.Width - 1 do
            begin
            lColour := FSpanGradient.ColorAt(FSpanGradient.OffsetAt(
              FSpanInverse.Transform(TSVGPoint.Create(X + SampleOffset,
                Y + SampleOffset))));
            lColour.Alpha := Round(SVGClamp(lColour.Alpha * aOpacity,
              0, 65535));
            Result.Colors[X, Y] := TFPColor(lColour);
            end;
        end;
    end;
    if aChain.Linear then
      SVGTurnSurface(Result, True);
  end;

  // The pixels the element was drawn over, which a background source
  // reads. The element drew into its own layer, so the pixels under it
  // are unchanged.
  function BackgroundSurface: TFPCustomImage;
  begin
    Result := NewSurface;
    if not aChain.Background then
      Exit;
    Result.Assign(FTarget);
    if aChain.Linear then
      SVGTurnSurface(Result, True);
  end;

  // A colour of a primitive converted to the light the chain works in.
  // The colour is written as it is displayed, and a chain working on
  // linear light multiplies by it before anything is converted back.
  function LitColour(const aColour: TSVGColor): TSVGColor;
  begin
    Result := aColour;
    if not aChain.Linear then
      Exit;
    Result.Red := Round(SVGToLinear(aColour.Red) * 65535);
    Result.Green := Round(SVGToLinear(aColour.Green) * 65535);
    Result.Blue := Round(SVGToLinear(aColour.Blue) * 65535);
  end;

  // A primitive with its colour converted to that light, and a
  // positioned light mapped into the space the frame is drawn in. A
  // distant light has a direction and no position, so nothing is mapped
  // for it.
  function LitBy(const aPrimitive: TSVGFilterPrimitive): TSVGFilterPrimitive;
  var
    lAt: TSVGPoint;
  begin
    Result := aPrimitive;
    Result.Colour := LitColour(aPrimitive.Colour);
    if Round(aPrimitive.Numbers[3]) = 0 then
      Exit;
    lAt := aChain.CTM.Transform(TSVGPoint.Create(aPrimitive.Numbers[4],
      aPrimitive.Numbers[5]));
    Result.Numbers[4] := lAt.X;
    Result.Numbers[5] := lAt.Y;
    Result.Numbers[6] := aPrimitive.Numbers[6] * lScale;
    if Round(aPrimitive.Numbers[3]) < 2 then
      Exit;
    lAt := aChain.CTM.Transform(TSVGPoint.Create(aPrimitive.Numbers[7],
      aPrimitive.Numbers[8]));
    Result.Numbers[7] := lAt.X;
    Result.Numbers[8] := lAt.Y;
    Result.Numbers[9] := aPrimitive.Numbers[9] * lScale;
  end;

  // The input of a primitive: one of the sources, or a result of the
  // chain.
  function SurfaceOf(aInput: Integer): TFPCustomImage;
  begin
    if aInput = SVGFilterSourceAlpha then
      Exit(lAlpha);
    if aInput = SVGFilterFillPaint then
      begin
      if lFill = nil then
        lFill := PlaneOf(aChain.FillPaint, aChain.FillOpacity);
      Exit(lFill);
      end;
    if aInput = SVGFilterStrokePaint then
      begin
      if lStroke = nil then
        lStroke := PlaneOf(aChain.StrokePaint, aChain.StrokeOpacity);
      Exit(lStroke);
      end;
    if aInput = SVGFilterBackgroundImage then
      begin
      if lBack = nil then
        lBack := BackgroundSurface;
      Exit(lBack);
      end;
    if aInput = SVGFilterBackgroundAlpha then
      begin
      if lBackAlpha = nil then
        begin
        if lBack = nil then
          lBack := BackgroundSurface;
        lBackAlpha := AlphaOf(lBack);
        end;
      Exit(lBackAlpha);
      end;
    if (aInput >= 0) and (aInput <= High(lResults))
       and (lResults[aInput] <> nil) then
      Exit(lResults[aInput]);
    Result := lSource;
  end;

begin
  if FLayerCount = 0 then
    raise ESVGSoft.Create(SErrPopLayerAsFilterUnmatched);
  Dec(FLayerCount);
  lLayer := FLayers[FLayerCount];
  FLayers[FLayerCount].Surface := nil;
  if FLayerCount = 0 then
    FTarget := FImage
  else
    FTarget := FLayers[FLayerCount - 1].Surface;
  lSource := nil;
  lAlpha := nil;
  lResults := nil;
  lFill := nil;
  lStroke := nil;
  lBack := nil;
  lBackAlpha := nil;
  try
    lSource := TSVGSoftSurface.Create(FImage.Width, FImage.Height);
    lSource.Assign(lLayer.Surface);
    if aChain.Linear then
      SVGTurnSurface(lSource, True);
    // The alpha of the drawing, which SVG offers as a separate source.
    lAlpha := AlphaOf(lSource);
    // A length of a primitive is given in the space the document draws
    // in and applied in the space the frame is drawn in.
    lScale := aChain.CTM.MaxScale;
    SetLength(lResults, Length(aChain.Primitives));
    for I := 0 to High(aChain.Primitives) do
      lResults[I] := nil;
    for I := 0 to High(aChain.Primitives) do
      begin
      lOut := NewSurface;
      lResults[I] := lOut;
      // The box of the primitive, or the whole region when it has none.
      if aChain.Primitives[I].HasRegion then
        lBox := aChain.Primitives[I].Region.Transform(aChain.CTM)
      else
        lBox := aChain.Region.Transform(aChain.CTM);
      lIn := lSource;
      if Length(aChain.Primitives[I].Inputs) > 0 then
        lIn := SurfaceOf(aChain.Primitives[I].Inputs[0]);
      case aChain.Primitives[I].Kind of
        fkFlood:
          SVGFilterFlood(lOut, LitColour(aChain.Primitives[I].Colour),
            aChain.Primitives[I].Opacity);
        fkOffset:
          SVGFilterOffset(lIn, lOut,
            Round(aChain.Primitives[I].Numbers[0] * lScale),
            Round(aChain.Primitives[I].Numbers[1] * lScale));
        fkGaussianBlur:
          begin
          lWide := SVGFilterBoxWidth(aChain.Primitives[I].Numbers[0] * lScale);
          lTall := SVGFilterBoxWidth(aChain.Primitives[I].Numbers[1] * lScale);
          lOut.Assign(lIn);
          if (lWide > 0) or (lTall > 0) then
            begin
            lWork := NewSurface;
            try
              for J := 1 to 3 do
                begin
                SVGFilterBoxWindow(lWide, J, lFrom, lTo);
                SVGFilterBox(lOut, lWork, lFrom, lTo, False);
                SVGFilterBoxWindow(lTall, J, lFrom, lTo);
                SVGFilterBox(lWork, lOut, lFrom, lTo, True);
                end;
            finally
              lWork.Free;
            end;
            end;
          end;
        fkMerge:
          for J := 0 to High(aChain.Primitives[I].Inputs) do
            SVGFilterOver(SurfaceOf(aChain.Primitives[I].Inputs[J]), lOut);
        fkComposite:
          begin
          lIn2 := lSource;
          if Length(aChain.Primitives[I].Inputs) > 1 then
            lIn2 := SurfaceOf(aChain.Primitives[I].Inputs[1]);
          SVGFilterComposite(lIn, lIn2, lOut,
            aChain.Primitives[I].Operation, aChain.Primitives[I].Numbers);
          end;
        fkColorMatrix:
          SVGFilterColorMatrix(lIn, lOut,
            SVGFilterMatrixOf(aChain.Primitives[I].Operation,
              aChain.Primitives[I].Numbers));
        fkBlend:
          begin
          lIn2 := lSource;
          if Length(aChain.Primitives[I].Inputs) > 1 then
            lIn2 := SurfaceOf(aChain.Primitives[I].Inputs[1]);
          SVGFilterBlend(lIn, lIn2, lOut, aChain.Primitives[I].Operation);
          end;
        fkMorphology:
          SVGFilterMorphology(lIn, lOut,
            Round(aChain.Primitives[I].Numbers[0] * lScale),
            Round(aChain.Primitives[I].Numbers[1] * lScale),
            aChain.Primitives[I].Operation);
        fkComponentTransfer:
          SVGFilterComponents(lIn, lOut, aChain.Primitives[I].Transfer);
        fkTile:
          begin
          // The tile is the box written by the primitive that is read,
          // or the whole region when that primitive has no box.
          if (Length(aChain.Primitives[I].Inputs) > 0)
             and (aChain.Primitives[I].Inputs[0] >= 0)
             and aChain.Primitives[aChain.Primitives[I].Inputs[0]].HasRegion then
            SVGFilterTile(lIn, lOut,
              aChain.Primitives[aChain.Primitives[I].Inputs[0]].Region
                .Transform(aChain.CTM))
          else
            SVGFilterTile(lIn, lOut, aChain.Region.Transform(aChain.CTM));
          end;
        fkConvolveMatrix:
          SVGFilterConvolve(lIn, lOut, aChain.Primitives[I]);
        fkDisplacementMap:
          begin
          lIn2 := lSource;
          if Length(aChain.Primitives[I].Inputs) > 1 then
            lIn2 := SurfaceOf(aChain.Primitives[I].Inputs[1]);
          SVGFilterDisplace(lIn, lIn2, lOut,
            aChain.Primitives[I].Numbers[0] * lScale,
            Round(aChain.Primitives[I].Numbers[1]),
            Round(aChain.Primitives[I].Numbers[2]));
          end;
        fkTurbulence:
          SVGFilterTurbulence(lOut, aChain.Primitives[I], aChain.CTM);
        fkDiffuseLighting:
          SVGFilterLighting(lIn, lOut, LitBy(aChain.Primitives[I]), False);
        fkSpecularLighting:
          SVGFilterLighting(lIn, lOut, LitBy(aChain.Primitives[I]), True);
        fkImage:
          // An element drawn into a layer is taken unchanged, being in the
          // space of the frame already. A file is laid over the region.
          // Either way the pixels arrive as they are displayed and are
          // converted to the linear light the chain works in.
          begin
          if (I <= High(FFilterImages)) and (FFilterImages[I] <> nil) then
            lOut.Assign(FFilterImages[I])
          else if aChain.Primitives[I].Image <> nil then
            SVGFilterImage(lOut, aChain.Primitives[I].Image, lBox,
              aChain.Primitives[I].Ratio);
          if aChain.Linear then
            SVGTurnSurface(lOut, True);
          end;
      else
        // A primitive that is not implemented here passes its input on, so
        // that a chain holding one still draws the rest of itself.
        lOut.Assign(lIn);
      end;
      SVGFilterHoldTo(lOut, lBox);
      end;
    if Length(lResults) > 0 then
      begin
      lOut := lResults[High(lResults)];
      if aChain.Linear then
        SVGTurnSurface(lOut, False);
      lHeld.Surface := lOut;
      lHeld.Opacity := lLayer.Opacity;
      lHeld.Bounds := lLayer.Bounds;
      CompositeLayer(lHeld);
      end;
  finally
    for I := 0 to High(lResults) do
      lResults[I].Free;
    lAlpha.Free;
    lFill.Free;
    lStroke.Free;
    lBack.Free;
    lBackAlpha.Free;
    lSource.Free;
    lLayer.Surface.Free;
    ClearFilterImages;
  end;
end;


procedure TSVGSoftBackend.PopLayerAsMask(aMode: TSVGMaskMode);

var
  lLayer: TSVGSoftLayer;
  X, Y, lValue: Integer;
  lSource, lTarget: TSVGColor;

begin
  if FLayerCount = 0 then
    raise ESVGSoft.Create(SErrPopLayerAsMaskUnmatched);
  Dec(FLayerCount);
  lLayer := FLayers[FLayerCount];
  if FLayerCount = 0 then
    FTarget := FImage
  else
    FTarget := FLayers[FLayerCount - 1].Surface;
  try
    // Every pixel of the target is visited. A pixel the mask does not
    // cover has to lose its alpha, not keep it.
    for Y := 0 to FTarget.Height - 1 do
      for X := 0 to FTarget.Width - 1 do
        begin
        lTarget := TSVGColor(FTarget.Colors[X, Y]);
        if lTarget.Alpha = 0 then
          Continue;
        lSource := TSVGColor(lLayer.Surface.Colors[X, Y]);
        if aMode <> mmLuminance then
          lValue := 65535
        // Alpha is a linear quantity, so the mask takes a luminance
        // weighted in linear light, and not one converted back to sRGB.
        else if FMixing = ciLinearRGB then
          lValue := Round(65535 * (0.2125 * SVGToLinear(lSource.Red)
            + 0.7154 * SVGToLinear(lSource.Green)
            + 0.0721 * SVGToLinear(lSource.Blue)))
        else
          lValue := (2125 * lSource.Red + 7154 * lSource.Green
            + 721 * lSource.Blue) div 10000;
        lValue := Round(lValue * (lSource.Alpha / 65535) * lLayer.Opacity);
        if lValue > 65535 then
          lValue := 65535
        else if lValue < 0 then
          lValue := 0;
        lTarget.Alpha := lTarget.Alpha * lValue div 65535;
        FTarget.Colors[X, Y] := TFPColor(lTarget);
        end;
  finally
    lLayer.Surface.Free;
    FLayers[FLayerCount].Surface := nil;
  end;
end;


procedure TSVGSoftBackend.DrawGlyphRun(aFont: TSVGFontHandle;
  const aGlyphs: TSVGGlyphArray; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aOpacity: Double);

var
  I: Integer;
  lScale: Double;

begin
  NeedFrame('DrawGlyphRun');
  if (aFont = nil) or (Length(aGlyphs) = 0) then
    Exit;
  lScale := aCTM.MaxScale;
  if lScale <= 0 then
    Exit;
  // The whole run becomes one path, so glyphs that touch are blended once
  // where they meet, and not twice.
  FGlyphPath.Clear;
  SVGAppendGlyphRun(aFont, aGlyphs, FGlyphPath);
  if FGlyphPath.IsEmpty then
    Exit;
  FSource.Flatten(FGlyphPath, TSVGMatrix.Identity, FFlatness / lScale);
  FPoly.Assign(FSource);
  FPoly.Transform(aCTM);
  FillPolygon(aPaint, frNonZero, aOpacity, aCTM, FSource.Bounds);
end;


// The colour of an image at a point of its pixel grid, taken from the
// four pixels around it. SVG prefers quality over speed when nothing says
// otherwise, and taking one pixel per pixel shows every step of a scale.
// The channels mix weighted by alpha, so a pixel that is clear lends its
// coverage and none of its colour. A tap off the edge takes the pixel at
// the edge.
function SampleImage(aImage: ISVGImageSource; aWidth, aHeight: Integer;
  aX, aY: Double): TSVGColor;

var
  U0, V0, I, J, lX, lY: Integer;
  lFU, lFV, lWU, lWV, lWeight: Double;
  lRed, lGreen, lBlue, lAlpha: Double;
  lPixel: TSVGColor;

begin
  // A pixel of the grid is sampled at its middle, so the four around a
  // point are the ones its half-pixel offset falls between.
  U0 := Floor(aX - 0.5);
  V0 := Floor(aY - 0.5);
  lFU := (aX - 0.5) - U0;
  lFV := (aY - 0.5) - V0;
  lRed := 0;
  lGreen := 0;
  lBlue := 0;
  lAlpha := 0;
  for J := 0 to 1 do
    for I := 0 to 1 do
      begin
      if I = 0 then
        lWU := 1 - lFU
      else
        lWU := lFU;
      if J = 0 then
        lWV := 1 - lFV
      else
        lWV := lFV;
      lWeight := lWU * lWV;
      if lWeight <= 0 then
        Continue;
      lX := Min(aWidth - 1, Max(0, U0 + I));
      lY := Min(aHeight - 1, Max(0, V0 + J));
      lPixel := aImage.GetPixel(lX, lY);
      lAlpha := lAlpha + lWeight * lPixel.Alpha;
      lRed := lRed + lWeight * lPixel.Red * lPixel.Alpha;
      lGreen := lGreen + lWeight * lPixel.Green * lPixel.Alpha;
      lBlue := lBlue + lWeight * lPixel.Blue * lPixel.Alpha;
      end;
  if lAlpha <= 0 then
    begin
    Result.Red := 0;
    Result.Green := 0;
    Result.Blue := 0;
    Result.Alpha := 0;
    Exit;
    end;
  Result.Red := Round(SVGClamp(lRed / lAlpha, 0, $FFFF));
  Result.Green := Round(SVGClamp(lGreen / lAlpha, 0, $FFFF));
  Result.Blue := Round(SVGClamp(lBlue / lAlpha, 0, $FFFF));
  Result.Alpha := Round(SVGClamp(lAlpha, 0, $FFFF));
end;


procedure TSVGSoftBackend.DrawImage(aImage: ISVGImageSource;
  const aRect: TSVGRect; const aCTM: TSVGMatrix; aOpacity: Double);

var
  lToDevice, lInverse: TSVGMatrix;
  lDevice: TSVGRect;
  X, Y, U, V, lLeft, lTop, lRight, lBottom, lClip: Integer;
  lWidth, lHeight: Integer;
  lPoint: TSVGPoint;
  lColor: TSVGColor;
  lOpacity: Double;

begin
  NeedFrame('DrawImage');
  if aImage = nil then
    Exit;
  lWidth := aImage.GetWidth;
  lHeight := aImage.GetHeight;
  lOpacity := SVGClamp(aOpacity, 0, 1);
  if (lWidth <= 0) or (lHeight <= 0) or (lOpacity <= 0)
     or (aRect.Width <= 0) or (aRect.Height <= 0) then
    Exit;
  // Image space is the pixel grid. It maps to the device through the
  // rectangle the image is drawn into, and then the CTM.
  lToDevice := TSVGMatrix.Scaling(aRect.Width / lWidth, aRect.Height / lHeight)
    .Compose(TSVGMatrix.Translation(aRect.Left, aRect.Top)).Compose(aCTM);
  if not lToDevice.Invert(lInverse) then
    Exit;
  lDevice := aRect.Transform(aCTM);
  lLeft := Max(0, Floor(lDevice.Left));
  lTop := Max(0, Floor(lDevice.Top));
  lRight := Min(FImage.Width, Ceil(lDevice.Right));
  lBottom := Min(FImage.Height, Ceil(lDevice.Bottom));
  for Y := lTop to lBottom - 1 do
    for X := lLeft to lRight - 1 do
      begin
      lClip := ClipCoverage(X, Y);
      if lClip = 0 then
        Continue;
      lPoint := lInverse.Transform(TSVGPoint.Create(X + 0.5, Y + 0.5));
      U := Floor(lPoint.X);
      V := Floor(lPoint.Y);
      if (U < 0) or (U >= lWidth) or (V < 0) or (V >= lHeight) then
        Continue;
      lColor := SampleImage(aImage, lWidth, lHeight, lPoint.X, lPoint.Y);
      if lColor.Alpha = 0 then
        Continue;
      BlendPixel(X, Y, lColor,
        Round(lColor.Alpha * lOpacity * lClip / 255));
      end;
end;


initialization
  SVGBackends.RegisterBackend(TSVGSoftBackend);
end.
