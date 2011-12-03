unit CocoaGDIObjects;
//todo: Remove MacOSAll unit to prevent Carbon framework linking.
//todo: Remove HIShape usage used in TCocoaRegion.

interface

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  SysUtils, MacOSAll, // for CGContextRef
  LCLtype, LCLProc,
  CocoaAll, CocoaUtils,
  Classes, Types;

type
  TCocoaBitmapAlignment = (
    cbaByte,  // each line starts at byte boundary.
    cbaWord,  // each line starts at word (16bit) boundary
    cbaDWord, // each line starts at double word (32bit) boundary
    cbaQWord, // each line starts at quad word (64bit) boundary
    cbaDQWord // each line starts at double quad word (128bit) boundary
  );

  TCocoaBitmapType = (
    cbtMono,  // mask or mono bitmap
    cbtGray,  // grayscale bitmap
    cbtRGB,   // color bitmap 8-8-8 R-G-B
    cbtARGB,  // color bitmap with alpha channel first 8-8-8-8 A-R-G-B
    cbtRGBA,  // color bitmap with alpha channel last 8-8-8-8 R-G-B-A
    cbtBGR,   // color bitmap 8-8-8 B-G-R (windows compatible)
    cbtBGRA   // color bitmap with alpha channel 8-8-8-8 B-G-R-A (windows compatible)
  );

const
  cbtMask = cbtMono;

type
  { TCocoaGDIObject }

  TCocoaGDIObject = class(TObject)
  public
    RefCount: Integer;
    procedure AddRef;
    procedure Release;
  end;

  TCocoaRegionType = (crt_Empty, crt_Rectangle, crt_Complex);
  TCocoaCombine = (cc_And, cc_Xor, cc_Or, cc_Diff, cc_Copy);

  { TCocoaRegion }

  //todo: Remove HIShape usage. HIShape is legacy
  TCocoaRegion = class(TCocoaGDIObject)
  private
    FShape: HIShapeRef;
  public
    constructor Create;
    constructor Create(const X1, Y1, X2, Y2: Integer);
    constructor Create(Points: PPoint; NumPts: Integer; isAlter: Boolean);
    destructor Destroy; override;

    procedure Apply(cg: CGContextRef);
    function GetBounds: TRect;
    function GetType: TCocoaRegionType;
    function ContainsPoint(const P: TPoint): Boolean;
    procedure SetShape(AShape: HIShapeRef);
    function CombineWith(ARegion: TCocoaRegion; CombineMode: TCocoaCombine): Boolean;
  public
    property Shape: HIShapeRef read FShape write SetShape;
  end;

  { TCocoaBrush }

  TCocoaBrush = class(TCocoaGDIObject)
    R,G,B : Single;
    procedure Apply(cg: CGContextRef);
  end;

  { TCocoaPen }

  TCocoaPen = class(TCocoaGDIObject)
  public
    Style : Integer;
    Width : Integer;
    R,G,B : Single;
    procedure Apply(cg: CGContextRef);
    constructor Create;
  end;

  { TCocoaFont }

  TCocoaFontStyle = set of (cfs_Bold, cfs_Italic, cfs_Underline, cfs_Strikeout);

  TCocoaFont = class(TCocoaGDIObject)
    Name  : AnsiString;
    Size  : Integer;
    Style : TCocoaFontStyle;
    Antialiased: Boolean;
    constructor CreateDefault;
  end;

  { TCocoaBitmap }

  TCocoaBitmap = class(TCocoaGDIObject)
  private
    FData: Pointer;
    FAlignment: TCocoaBitmapAlignment;
    FFreeData: Boolean;
    FDataSize: Integer;
    FBytesPerRow: Integer;
    FDepth: Byte;
    FBitsPerPixel: Byte;
    FWidth: Integer;
    FHeight: Integer;
    FType: TCocoaBitmapType;
    // Cocoa information
    FbitsPerSample: NSInteger;  // How many bits in each color component
    FsamplesPerPixel: NSInteger;// How many color components
  public
    image: NSImage;
    imagerep: NSBitmapImageRep;
    constructor Create(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
      AAlignment: TCocoaBitmapAlignment; AType: TCocoaBitmapType;
      AData: Pointer; ACopyData: Boolean = True);
    destructor Destroy; override;
    procedure SetInfo(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
      AAlignment: TCocoaBitmapAlignment; AType: TCocoaBitmapType);
  public
//    property BitsPerComponent: Integer read GetBitsPerComponent;
    property BitmapType: TCocoaBitmapType read FType;
//    property BytesPerRow: Integer read FBytesPerRow;
//    property CGImage: CGImageRef read FCGImage write SetCGImage;
//    property ColorSpace: CGColorSpaceRef read GetColorSpace;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize;
    property Depth: Byte read FDepth;
//    property Info: CGBitmapInfo read GetInfo;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  { TCocoaTextLayout }

  TCocoaTextLayout = class(TObject)
  public
    constructor Create; virtual; abstract;
    procedure SetFont(AFont: TCocoaFont); virtual; abstract;
    procedure SetText(UTF8Text: PChar; ByteSize: Integer); virtual; abstract;
    function GetSize: TSize; virtual; abstract;

    procedure Draw(cg: CGContextRef; X, Y: Integer; DX: PInteger); virtual; abstract;
  end;
  TCocoaTextLayoutClass = class of TCocoaTextLayout;

  { TASTUITextLayout }

  // legacy layout used for Mac OS X 10.4
  TASTUITextLayout = class(TCocoaTextLayout)
  private
    fBuffer     : WideString;
    fUTF8       : String;
    FDX         : PIntegerArray;

    FLayout     : ATSUTextLayout;
    FStyle      : ATSUStyle;

    FTextBefore : ATSUTextMeasurement;
    FTextAfter  : ATSUTextMeasurement;
    FAscent     : ATSUTextMeasurement;
    FDescent    : ATSUTextMeasurement;

    FValidSize  : Boolean;
    procedure RecountSize;
    procedure DoJustify(iLineRef: ATSULineRef; var Handled: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetFont(AFont: TCocoaFont); override;
    procedure SetText(UTF8Text: PChar; ByteSize: Integer); override;
    function GetSize: TSize; override;
    procedure Draw(cg: CGContextRef; X, Y: Integer; DX: PInteger); override;
  end;

  { TCoreTextLayout }

  //TCoreTextLayout = class(TCocoaTextLayout);

  { TCocoaContext }

  TCocoaContext = class(TObject)
  private
    fText    : TCocoaTextLayout;
    fBrush   : TCocoaBrush;
    fPen     : TCocoaPen;
    fFont    : TCocoaFont;
    fRegion  : TCocoaRegion;
    fBitmap  : TCocoaBitmap;
    procedure SetBitmap(const AValue: TCocoaBitmap);
    procedure SetBrush(const AValue: TCocoaBrush);
    procedure SetFont(const AValue: TCocoaFont);
    procedure SetPen(const AValue: TCocoaPen);
    procedure SetRegion(const AValue: TCocoaRegion);
  public
    ContextSize : TSize;
    ctx      : NSGraphicsContext;
    cgctx    : CGContextRef;
    PenPos   : TPoint;
    Stack    : Integer;
    TR,TG,TB : Single;
    constructor Create;
    destructor Destroy; override;
    function InitDraw(width, height: Integer): Boolean;
    procedure MoveTo(x,y: Integer);
    procedure LineTo(x,y: Integer);
    procedure Polygon(const Points: array of TPoint; NumPts: Integer; Winding: boolean);
    procedure Polyline(const Points: array of TPoint; NumPts: Integer);
    procedure Rectangle(X1, Y1, X2, Y2: Integer; FillRect: Boolean; UseBrush: TCocoaBrush);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure TextOut(X,Y: Integer; UTF8Chars: PChar; Count: Integer; CharsDelta: PInteger; BackgroundAlpha: Single);
    function GetTextExtentPoint(AStr: PChar; ACount: Integer; var Size: TSize): Boolean;
    function GetTextMetrics(var TM: TTextMetric): Boolean;
    procedure DrawBitmap(X,Y: Integer; ABitmap: TCocoaBitmap);
    procedure SetOrigin(X,Y: Integer);
    procedure GetOrigin(var X,Y: Integer);
    function CGContext: CGContextRef; virtual;
    property Brush: TCocoaBrush read fBrush write SetBrush;
    property Pen: TCocoaPen read fPen write SetPen;
    property Font: TCocoaFont read fFont write SetFont;
    property Region: TCocoaRegion read fRegion write SetRegion;
    property Bitmap: TCocoaBitmap read fBitmap write SetBitmap;
  end;

function CheckDC(dc: HDC): TCocoaContext;
function CheckDC(dc: HDC; Str: string): Boolean;
function CheckGDIOBJ(obj: HGDIOBJ): TCocoaGDIObject;
function CheckBitmap(ABitmap: HBITMAP; AStr: string): Boolean;

implementation

//todo: a better check!

function CheckDC(dc: HDC): TCocoaContext;
begin
  Result:=TCocoaContext(dc);
end;

function CheckDC(dc: HDC; Str: string): Boolean;
begin
  Result:=dc<>0;
end;

function CheckGDIOBJ(obj: HGDIOBJ): TCocoaGDIObject;
begin
  Result:=TCocoaGDIObject(obj);
end;

function CheckBitmap(ABitmap: HBITMAP; AStr: string): Boolean;
begin
  Result := ABitmap <> 0;
end;

constructor TCocoaFont.CreateDefault;
begin
  inherited Create({False});
end;

{ TCocoaBitmap }

type
  // The following dummy categories fix bugs in the Cocoa bindings available in FPC
  // Remove them when the FPC binding parser is fixed.
  // More details:
  // http://wiki.freepascal.org/FPC_PasCocoa/Differences#Sending_messages_to_id
  // http://wiki.lazarus.freepascal.org/FPC_PasCocoa#Category_declaration
  NSBitmapImageRepFix = objccategory external(NSBitmapImageRep)
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bytesPerRow_bitsPerPixel(planes: PPByte; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; rBytes: NSInteger; pBits: NSInteger): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:';
    function initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(planes: PPByte; width: NSInteger; height: NSInteger; bps: NSInteger; spp: NSInteger; alpha: Boolean; isPlanar_: Boolean; colorSpaceName_: NSString; bitmapFormat_: NSBitmapFormat; rBytes: NSInteger; pBits: NSInteger): id; message 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:';
  end;

{------------------------------------------------------------------------------
  Method:  TCocoaBitmap.Create
  Params:  AWidth        - Bitmap width
           AHeight       - Bitmap height
           ADepth        - Significant bits per pixel
           ABitsPerPixel - The number of allocated bits per pixel (can be larger than depth)
//           AAlignment    - Alignment of the data for each row
//           ABytesPerRow  - The number of bytes between rows
           ACopyData     - Copy supplied bitmap data (OPTIONAL)

  Creates Cocoa bitmap with the specified characteristics
 ------------------------------------------------------------------------------}
constructor TCocoaBitmap.Create(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
  AAlignment: TCocoaBitmapAlignment; AType: TCocoaBitmapType;
  AData: Pointer; ACopyData: Boolean);
var
  HasAlpha: Boolean;
  BitmapFormat: NSBitmapFormat;
  DataPointer: Pointer;
begin
  {$ifdef VerboseBitmaps}
  DebugLn(Format('[TCocoaBitmap.Create] AWidth=%d AHeight=%d ADepth=%d ABitsPerPixel=%d'
    + ' AAlignment=%d AType=%d AData=? ACopyData=%d',
    [AWidth, AHeight, ADepth, ABitsPerPixel, Integer(AAlignment), Integer(AType), Integer(ACopyData)]));
  {$endif}
  SetInfo(AWidth, AHeight, ADepth, ABitsPerPixel, AAlignment, AType);

  // Copy the image data, if necessary
  if ACopyData then
  begin
    System.GetMem(FData, FDataSize);
    FFreeData := True;
    if AData <> nil then
      System.Move(AData^, FData^, FDataSize) // copy data
    else
      FillDWord(FData^, FDataSize shr 2, 0); // clear bitmap
    DataPointer := @FData;
  end
  else if (AData = nil) then
  begin
    FData := AData;
    FFreeData := False;
    DataPointer := nil;
  end
  else
  begin
    FData := AData;
    FFreeData := False;
    DataPointer := @FData;
  end;

  HasAlpha := AType in [cbtARGB, cbtRGBA, cbtBGRA];
  BitmapFormat := NSAlphaNonpremultipliedBitmapFormat;
  if AType = cbtARGB then
    BitmapFormat := BitmapFormat or NSAlphaFirstBitmapFormat;

  {$ifdef VerboseBitmaps}
  DebugLn(Format('[TCocoaBitmap.Create] NSBitmapImageRep.alloc HasAlpha=%d',
    [Integer(HasAlpha)]));
  {$endif}
  // Create the associated NSImageRep
  imagerep := NSBitmapImageRep(NSBitmapImageRep.alloc.initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(
    DataPointer, // planes, BitmapDataPlanes
    FWidth, // width, pixelsWide
    FHeight,// height, PixelsHigh
    FbitsPerSample,// bitsPerSample, bps
    FsamplesPerPixel, // samplesPerPixel, sps
    HasAlpha, // hasAlpha
    False, // isPlanar
    NSCalibratedRGBColorSpace, // colorSpaceName
    BitmapFormat, // bitmapFormat
    FBytesPerRow, // bytesPerRow
    FBitsPerPixel //bitsPerPixel
    ));

  // Create the associated NSImage
  image := NSImage.alloc.initWithSize(NSMakeSize(AWidth, AHeight));
  image.addRepresentation(imagerep);
end;

destructor TCocoaBitmap.Destroy;
begin
  //CGImageRelease(FCGImage);
  if FFreeData then System.FreeMem(FData);

  inherited Destroy;
end;

procedure TCocoaBitmap.SetInfo(AWidth, AHeight, ADepth,
  ABitsPerPixel: Integer; AAlignment: TCocoaBitmapAlignment;
  AType: TCocoaBitmapType);
const
  ALIGNBITS: array[TCocoaBitmapAlignment] of Integer = (0, 1, 3, 7, $F);
var
  M: Integer;
begin
  if AWidth < 1 then AWidth := 1;
  if AHeight < 1 then AHeight := 1;
  FWidth := AWidth;
  FHeight := AHeight;
  FDepth := ADepth;
  FBitsPerPixel := ABitsPerPixel;
  FType := AType;
  FAlignment := AAlignment;

  if (FType in [cbtMono, cbtGray]) and (FDepth=0) then
    FDepth:=FBitsPerPixel;

  FBytesPerRow := ((AWidth * ABitsPerPixel) + 7) shr 3;
  M := FBytesPerRow and ALIGNBITS[AAlignment];
  if M <> 0 then Inc(FBytesPerRow, ALIGNBITS[AAlignment] + 1 - M);

  FDataSize := FBytesPerRow * FHeight;

  // Cocoa information
  case ABitsPerPixel of
    // Strangely, this might appear
    0:
    begin
      FbitsPerSample := 0;
      FsamplesPerPixel := 0;
    end;
    // Mono
    1:
    begin
      FbitsPerSample := 1;
      FsamplesPerPixel := 1;
    end;
    // Gray scale
    8:
    begin
      FbitsPerSample := 8;
      FsamplesPerPixel := 1;
    end;
    // ARGB
    32:
    begin
      FbitsPerSample := 8;
      FsamplesPerPixel := 4;
    end;
  else
    // Other RGB
    FbitsPerSample := ABitsPerPixel div 3;
    FsamplesPerPixel := 3;
  end;
end;

{ TASTUITextLayout }

function IntToFix(i: integer): Integer; inline;
begin
  Result:=i shl 16;
end;

function FixToInt(f: Integer): Integer; inline;
begin
  Result:=Round(Fix2X(F));
end;

procedure TASTUITextLayout.RecountSize;
begin
  ATSUGetUnjustifiedBounds(FLayout, kATSUFromTextBeginning, kATSUToTextEnd,
    FTextBefore, FTextAfter, FAscent, FDescent);
end;

constructor TASTUITextLayout.Create;
begin
  // create text layout
  ATSUCreateTextLayout(FLayout);
  SetText(#0, 1);
  ATSUSetTextLayoutRefCon(FLayout, URefCon(Self));

  ATSUCreateStyle(FStyle);

  // allow font substitution for exotic glyphs
  ATSUSetTransientFontMatching(FLayout, True);
end;

destructor TASTUITextLayout.Destroy;
begin
  ATSUDisposeTextLayout(FLayout);
  ATSUDisposeStyle(FStyle);
  inherited Destroy;
end;

const
  DefaultFont = 'Lucida Grande';
  DefaultSize = 13;

function FindATSUFontID(const FontName: String): ATSUFontID;
var
  fn  : String;
begin
  Result := 0;
  if SysUtils.CompareText(FontName, 'default')=0 then fn:=DefaultFont else fn:=FontName;
  if (fn <> '') then
    ATSUFindFontFromName(@fn[1], Length(fn),
        kFontFullName, kFontMacintoshPlatform, kFontRomanScript,
        kFontEnglishLanguage, Result);
end;

procedure TASTUITextLayout.SetFont(AFont:TCocoaFont);
var
  Attr: ATSUAttributeTag;
  M: ATSUTextMeasurement;
  O: ATSStyleRenderingOptions;
  B: Boolean;
  S: ByteCount;
  A: ATSUAttributeValuePtr;
  ID: ATSUFontID;
const
  ATSStyleRenderingOption: array [Boolean] of ATSStyleRenderingOptions =
    (kATSStyleNoAntiAliasing, kATSStyleApplyAntiAliasing);
begin
  if not Assigned(AFont) then Exit;

  ID := FindATSUFontID(AFont.Name);

  if ID <> 0 then
  begin
    Attr := kATSUFontTag;
    A := @ID;
    S := SizeOf(ID);
    ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);
  end;

  Attr := kATSUSizeTag;
  M := IntToFix(Abs(AFont.Size));
  A := @M;
  S := SizeOf(M);
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  S := SizeOf(B);
  Attr := kATSUQDBoldfaceTag;
  B := cfs_Bold in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUQDItalicTag;
  B := cfs_Italic in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUQDUnderlineTag;
  B := cfs_Underline in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUStyleStrikeThroughTag;
  B := cfs_Strikeout in AFont.Style;
  A := @B;
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  Attr := kATSUStyleRenderingOptionsTag;
  O := ATSStyleRenderingOption[AFont.Antialiased];
  A := @O;
  S := SizeOf(O);
  ATSUSetAttributes(FStyle, 1, @Attr, @S, @A);

  FValidSize:=False;
end;

procedure TASTUITextLayout.SetText(UTF8Text: PChar; ByteSize: Integer);
begin
  if (ByteSize=length(fUTF8)) and (fUTF8<>'') and
    (CompareChar(UTF8Text^, fUTF8[1], ByteSize)=0) then Exit; // same buffer, nothing to change!

  SetLength(fUTF8, ByteSize);
  if ByteSize>0 then
    System.Move(UTF8Text^, fUTF8[1], ByteSize)
  else
    fUTF8:='';

  fBuffer:=UTF8Decode(fUTF8);
  if fBuffer='' then fBuffer:=#0;
  ATSUSetTextPointerLocation(FLayout, @fBuffer[1], 0, length(fBuffer), length(fBuffer));
  ATSUSetRunStyle(FLayout, FStyle, kATSUFromTextBeginning, kATSUToTextEnd);

  FValidSize:=False;
end;

function TASTUITextLayout.GetSize:TSize;
begin
  if not FValidSize then RecountSize;
  Result.cx := FixToInt(FTextAfter - FTextBefore);
  Result.cy := FixToInt(FDescent + FAscent);
end;

var
  ATSUDirectUPP : ATSUDirectLayoutOperationOverrideUPP = nil; //NewATSUDirectLayoutOperationOverrideUPP(@ATSUCallback)

function ATSUCallback(iCurrentOperation: ATSULayoutOperationSelector; iLineRef: ATSULineRef; iRefCon: UInt32; iOperationCallbackParameterPtr: UnivPtr;
  var oCallbackStatus: ATSULayoutOperationCallbackStatus ): OSStatus; {$ifdef DARWIN}mwpascal;{$endif}
var
  Buffer  : TASTUITextLayout;
  Handled : Boolean;
begin
  Result := noErr;
  Buffer := TASTUITextLayout(iRefCon);
  oCallbackStatus:=kATSULayoutOperationCallbackStatusHandled;

  if Assigned(Buffer) then
    Buffer.DoJustify(iLineRef, Handled);
end;

procedure TASTUITextLayout.DoJustify(iLineRef: ATSULineRef; var Handled: Boolean);
type
	ATSLayoutRecord1 = packed record
		glyphID: ATSGlyphRef;
		flags: ATSGlyphInfoFlags;
		originalOffset: ByteCount;
		realPos: Fixed;
	end;

type
  TATSLayoutRecordArray = array [Word] of ATSLayoutRecord1;
  PATSLayoutRecordArray = ^TATSLayoutRecordArray;
var
  i, ofs  : Integer;
  Layouts   : PATSLayoutRecordArray;
  LayCount  : ItemCount;
begin
  if not Assigned(FDX) then Exit;
  Laycount:=0;
  ATSUDirectGetLayoutDataArrayPtrFromLineRef( iLineRef,
    kATSUDirectDataLayoutRecordATSLayoutRecordVersion1, true, @Layouts, Laycount);
  if Assigned(Layouts) and (Laycount>0) then
  begin
    ofs:=0;
    for i:=0 to LayCount-1 do
    begin
      Layouts^[i].realPos:=Long2Fix(ofs);
      inc(ofs, FDX^[i]);
    end;
  end;
  ATSUDirectReleaseLayoutDataArrayPtr(iLineRef, kATSUDirectDataLayoutRecordATSLayoutRecordCurrent, @Layouts );
  Handled:=True;
end;


procedure TASTUITextLayout.Draw(cg:CGContextRef;X,Y:Integer;DX:PInteger);
var
  MX, MY    : Integer;

  Tag       : ATSUAttributeTag;
  Size      : ByteCount;
  Value     : ATSUAttributeValuePtr;
  OverSpec  : ATSULayoutOperationOverrideSpecifier;
begin
  if not Assigned(cg) then Exit;
  if not FValidSize then RecountSize;

  MX:=0;
  MY:=0;
  Tag := kATSUCGContextTag;
  Size := sizeOf(CGContextRef);
  Value := @cg;
  ATSUSetLayoutControls(FLayout, 1, @Tag, @Size, @Value);

  Tag := kATSULayoutOperationOverrideTag;
  Size := sizeof (ATSULayoutOperationOverrideSpecifier);
  Value := @OverSpec;
  FillChar(OverSpec, sizeof(OverSpec), 0);
  if Assigned(Dx) then begin
    FDX := PIntegerArray(Dx);
    OverSpec.operationSelector := kATSULayoutOperationPostLayoutAdjustment;
    if not Assigned(ATSUDirectUPP) then ATSUDirectUPP:=NewATSUDirectLayoutOperationOverrideUPP(@ATSUCallback);
    OverSpec.overrideUPP := ATSUDirectUPP;
  end else
    FDX:=nil;
  ATSUSetLayoutControls (FLayout, 1, @Tag, @Size, @Value);

  ATSUDrawText(FLayout, kATSUFromTextBeginning, kATSUToTextEnd,
    IntToFix(X)- FTextBefore + MX, IntToFix(Y) - FAscent + MY);
end;

{ TCocoaContext }

function TCocoaContext.CGContext:CGContextRef;
begin
  if ctx = nil then Result := cgctx
  else Result:=CGContextRef(ctx.graphicsPort);
end;

procedure TCocoaContext.SetBitmap(const AValue: TCocoaBitmap);
begin
  fBitmap:=AValue;
end;

procedure TCocoaContext.SetBrush(const AValue: TCocoaBrush);
begin
  fBrush:=AValue;
  if Assigned(fBrush) then fBrush.Apply(CGContext);
end;

procedure TCocoaContext.SetFont(const AValue: TCocoaFont);
begin
  fFont:=AValue;
end;

procedure TCocoaContext.SetPen(const AValue: TCocoaPen);
begin
  fPen:=AValue;
  if Assigned(fPen) then fPen.Apply(CGContext);
end;

procedure TCocoaContext.SetRegion(const AValue: TCocoaRegion);
begin
  fRegion:=AValue;
end;

constructor TCocoaContext.Create;
begin
  FFont := TCocoaFont.CreateDefault;
  FFont.AddRef;
  FText := TASTUITextLayout.Create;
end;

destructor TCocoaContext.Destroy;
begin
  inherited Destroy;
end;

function TCocoaContext.InitDraw(width,height:Integer): Boolean;
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  Result:=Assigned(cg);
  if not Result then Exit;

  ContextSize.cx:=width;
  ContextSize.cy:=height;

  CGContextTranslateCTM(cg, 0, height);
  CGContextScaleCTM(cg, 1, -1);
  PenPos.x:=0;
  PenPos.y:=0;
end;

procedure TCocoaContext.MoveTo(x,y:Integer);
begin
  PenPos.x:=x;
  PenPos.y:=y;
end;

procedure TCocoaContext.LineTo(x,y:Integer);
var
  cg  : CGContextRef;
  p   : array [0..1] of CGPoint;
  deltaX, deltaY, absDeltaX, absDeltaY: Integer;
  clipDeltaX, clipDeltaY: Float32;
  tx,ty:Float32;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;

  deltaX := X - PenPos.x;
  deltaY := Y - PenPos.y;
  if (deltaX=0) and (deltaY=0) then Exit;

  absDeltaX := Abs(deltaX);
  absDeltaY := Abs(deltaY);
  if (absDeltaX<=1) and (absDeltaY<=1) then
  begin
    // special case for 1-pixel lines
    tx := PenPos.x + 0.55;
    ty := PenPos.y + 0.55;
  end
  else
  begin
    // exclude the last pixel from the line
    if absDeltaX > absDeltaY then
    begin
      if deltaX > 0 then clipDeltaX := -1.0 else clipDeltaX := 1.0;
      clipDeltaY := clipDeltaX * deltaY / deltaX;
    end
    else
    begin
      if deltaY > 0 then clipDeltaY := -1.0 else clipDeltaY := 1.0;
      clipDeltaX := clipDeltaY * deltaX / deltaY;
    end;
    tx := X + clipDeltaX + 0.5;
    ty := Y + clipDeltaY + 0.5;
  end;

  p[0].x:=PenPos.X+0.5;
  p[0].y:=PenPos.Y+0.5;
  p[1].x:=tx;
  p[1].y:=ty;

  CGContextBeginPath(cg);
  CGContextAddLines(cg, @p, 2);
  CGContextStrokePath(cg);

  PenPos.x := X;
  PenPos.y := Y;
end;

procedure CGContextAddLCLPoints(cg: CGContextRef; const Points: array of TPoint;NumPts:Integer);
var
  cp  : array of CGPoint;
  i   : Integer;
begin
  SetLength(cp, NumPts);
  for i:=0 to NumPts-1 do begin
    cp[i].x:=Points[i].X+0.5;
    cp[i].y:=Points[i].Y+0.5;
  end;
  CGContextAddLines(cg, @cp[0], NumPts);
end;

procedure CGContextAddLCLRect(cg: CGContextRef; x1, y1, x2, y2: Integer); overload;
var
  r  : CGRect;
begin
  r.origin.x:=x1+0.5;
  r.origin.y:=y1+0.5;
  r.size.width:=x2-x1-1;
  r.size.height:=y2-y1-1;
  CGContextAddRect(cg, r);
end;

procedure CGContextAddLCLRect(cg: CGContextRef; const R: TRect); overload;
begin
  CGContextAddLCLRect(cg, r.Left, r.Top, r.Right, r.Bottom);
end;

procedure TCocoaContext.Polygon(const Points:array of TPoint;NumPts:Integer;
  Winding:boolean);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) or (NumPts<=0) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLPoints(cg, Points, NumPts);
  CGContextClosePath(cg);

  if Winding then
    CGContextDrawPath(cg, kCGPathFillStroke)
  else
    CGContextDrawPath(cg, kCGPathEOFillStroke);
end;

procedure TCocoaContext.Polyline(const Points: array of TPoint; NumPts: Integer);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) or (NumPts<=0) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLPoints(cg, Points, NumPts);
  CGContextStrokePath(cg);
end;

procedure TCocoaContext.Rectangle(X1,Y1,X2,Y2:Integer;FillRect:Boolean; UseBrush: TCocoaBrush);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLRect(cg, X1,Y1,X2,Y2);
  if FillRect then begin
    //using the brush
    if Assigned(UseBrush) then UseBrush.Apply(cg);
    CGContextFillPath(cg);
    //restore the brush
    if Assigned(UseBrush) and Assigned(fBrush) then fBrush.Apply(cg);
  end else
    CGContextStrokePath(cg);
end;

procedure TCocoaContext.Ellipse(X1,Y1,X2,Y2:Integer);
var
  cg : CGContextRef;
  r  : CGRect;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;
  r.origin.x:=x1+0.5;
  r.origin.y:=y1+0.5;
  r.size.width:=x2-x1-1;
  r.size.height:=y2-y1-1;
  CGContextBeginPath(CGContext);
  CGContextAddEllipseInRect(CGContext, R);
  CGContextDrawPath(CGContext, kCGPathFillStroke);
end;

// for BackgroundAlpha 1 = opaque 0 = transparent
procedure TCocoaContext.TextOut(X,Y:Integer;UTF8Chars:PChar;Count:Integer;
  CharsDelta:PInteger; BackgroundAlpha: Single);
var
  cg: CGContextRef;
  ns: NSString;
  dic: NSDictionary;
begin
{  // Text rendering with Cocoa only
  ns:=NSStringUtf8(UTF8Chars);
//  dic := NSDictionary.dictionary();
  ns.drawAtPoint_withAttributes(GetNSPoint(10, 10), nil);
//  dic.release;
  ns.release;}

  // Text rendering with Carbon mixed (but it doesn't seam to work because cg returns nil)
  cg:=CGContext;
  if not Assigned(cg) then Exit;

  CGContextScaleCTM(cg, 1, -1);
  CGContextTranslateCTM(cg, 0, -ContextSize.cy);

  CGContextSetRGBFillColor(cg, TR, TG, TB, BackgroundAlpha);
  fText.SetText(UTF8Chars, Count);
  fText.Draw(cg, X, ContextSize.cy-Y, CharsDelta);

  if Assigned(fBrush) then fBrush.Apply(cg);

  CGContextTranslateCTM(cg, 0, ContextSize.cy);
  CGContextScaleCTM(cg, 1, -1);
end;

{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  Str   - Text string
           Count - Number of characters in string
           Size  - The record for the dimensions of the string
  Returns: If the function succeeds

  Computes the width and height of the specified string of text
 ------------------------------------------------------------------------------}
function TCocoaContext.GetTextExtentPoint(AStr: PChar; ACount: Integer;
  var Size: TSize): Boolean;
var
  LStr: String;
begin
  Result := False;
  Size.cx := 0;
  Size.cy := 0;

  if ACount = 0 then Exit(True);

  if ACount < 0 then LStr := AStr
  else LStr := Copy(AStr, 1, ACount);

  fText.SetText(PChar(LStr), Length(LStr));
  Size := fText.getSize();

  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaContext.GetTextMetrics
  Params:  TM - The Record for the text metrics
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font
 ------------------------------------------------------------------------------}
function TCocoaContext.GetTextMetrics(var TM: TTextMetric): Boolean;
{var
  TextStyle: ATSUStyle;
  M: ATSUTextMeasurement;
  B: Boolean;
  TextLayout: TCarbonTextLayout;
const
  SName = 'GetTextMetrics';
  SGetAttrName = 'ATSUGetAttribute';}
begin
  Result := False;

//  TextStyle := CurrentFont.Style;

  FillChar(TM, SizeOf(TM), 0);

{  // According to the MSDN library, TEXTMETRIC:
  // the average char width is generally defined as the width of the letter x
  if not BeginTextRender('x', 1, TextLayout) then Exit;
  try}

    TM.tmAscent := 5;//RoundFixed(TextLayout.Ascent);
    TM.tmDescent := 5;//RoundFixed(TextLayout.Descent);
    TM.tmHeight := 15;//RoundFixed(TextLayout.Ascent + TextLayout.Descent);

//    if OSError(ATSUGetAttribute(TextStyle, kATSULeadingTag, SizeOf(M), @M, nil),
//      Self, SName, SGetAttrName, 'kATSULeadingTag', kATSUNotSetErr) then Exit;
//    TM.tmInternalLeading := RoundFixed(M);
    TM.tmExternalLeading := 0;

    TM.tmAveCharWidth := 15;//RoundFixed(TextLayout.TextAfter - TextLayout.TextBefore);
//  finally
//    EndTextRender(TextLayout);
//  end;

  TM.tmMaxCharWidth := 15;//TM.tmAscent; // TODO: don't know how to determine this right
  TM.tmOverhang := 0;
  TM.tmDigitizedAspectX := 0;
  TM.tmDigitizedAspectY := 0;
  TM.tmFirstChar := 'a';
  TM.tmLastChar := 'z';
  TM.tmDefaultChar := 'x';
  TM.tmBreakChar := '?';

//  if OSError(ATSUGetAttribute(TextStyle, kATSUQDBoldfaceTag, SizeOf(B), @B, nil),
//    Self, SName, SGetAttrName, 'kATSUQDBoldfaceTag', kATSUNotSetErr) then Exit;
{  if B then} TM.tmWeight := FW_NORMAL;
//       else TM.tmWeight := FW_BOLD;

{  if OSError(ATSUGetAttribute(TextStyle, kATSUQDItalicTag, SizeOf(B), @B, nil),
    Self, SName, SGetAttrName, 'kATSUQDItalicTag', kATSUNotSetErr) then Exit;
  TM.tmItalic := Byte(B);}

{  if OSError(ATSUGetAttribute(TextStyle, kATSUQDUnderlineTag, SizeOf(B), @B, nil),
    Self, SName, SGetAttrName, 'kATSUQDUnderlineTag', kATSUNotSetErr) then Exit;
  TM.tmUnderlined := Byte(B);

  if OSError(ATSUGetAttribute(TextStyle, kATSUStyleStrikeThroughTag, SizeOf(B), @B, nil),
    Self, SName, SGetAttrName, 'kATSUStyleStrikeThroughTag', kATSUNotSetErr) then Exit;
  TM.tmStruckOut := Byte(B);}

  // TODO: get these from font
  TM.tmPitchAndFamily := FIXED_PITCH or TRUETYPE_FONTTYPE;
  TM.tmCharSet := DEFAULT_CHARSET;

  Result := True;
end;

procedure TCocoaContext.DrawBitmap(X,Y:Integer; ABitmap: TCocoaBitmap);
begin
  NSGraphicsContext.saveGraphicsState();
  NSGraphicsContext.setCurrentContext(ctx);
  ABitmap.imagerep.drawAtPoint(NSMakePoint(X, Y));
  NSGraphicsContext.restoreGraphicsState();
end;

procedure TCocoaContext.SetOrigin(X,Y:Integer);
var
  cg  : CGContextRef;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;
  if Assigned(cg) then CGContextTranslateCTM(cg, X, Y);
end;

procedure TCocoaContext.GetOrigin(var X,Y: Integer);
var
  cg  : CGContextRef;
  t   : CGAffineTransform;
begin
  cg:=CGContext;
  if not Assigned(cg) then Exit;
  t:=CGContextGetCTM(cg);
  X := Round(t.tx);
  Y := ContextSize.cy - Round(t.ty);
end;


{ TCocoaRegion }

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create

  Creates a new empty Cocoa region
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.Create;
begin
  inherited Create;

  FShape := HIShapeCreateEmpty;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create
  Params:  X1, Y1, X2, Y2 - Region bounding rectangle

  Creates a new rectangular Cocoa region
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.Create(const X1, Y1, X2, Y2: Integer);
begin
  inherited Create;
  FShape := HIShapeCreateWithRect(GetCGRect(X1, Y1, X2, Y2));
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create
  Params:  Points   - Pointer to array of polygon points
           NumPts   - Number of points passed
           FillMode - Filling mode

  Creates a new polygonal Cocoa region from the specified points
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.Create(Points: PPoint; NumPts: Integer; isAlter: Boolean);
var
  Bounds: TRect;
  Context: CGContextRef;
  W, H: Integer;
  Data: Pointer;
  PData: PByte;
  P: PPoint;
  I: Integer;
  X, Y, SX: Integer;
  LC, C: Byte;
  //Line: String;

  function GetPolygonBounds: TRect;
  var
    I: Integer;
  begin
    P := Points;
    Result := Classes.Rect(P^.X, P^.Y, P^.X, P^.Y);
    for I := 1 to NumPts - 1 do
    begin
      Inc(P);
      if P^.X < Result.Left then Result.Left := P^.X;
      if P^.X > Result.Right then Result.Right := P^.X;
      if P^.Y < Result.Top then Result.Top := P^.Y;
      if P^.Y > Result.Bottom then Result.Bottom := P^.Y;
    end;
  end;

  procedure AddPart(X1, X2, Y: Integer);
  var
    R: HIShapeRef;
  begin
    //DebugLn('AddPart:' + DbgS(X1) + ' - ' + DbgS(X2) + ', ' + DbgS(Y));

    R := HIShapeCreateWithRect(GetCGRect(X1, Y, X2, Y + 1));
    HIShapeUnion(FShape, R, FShape);
    CFRelease(R);
  end;

begin
  inherited Create;

(*
  The passed polygon is drawed into grayscale context, the region is constructed
  per rows from rectangles of drawed polygon parts.
  *)

  FShape := HIShapeCreateMutable;

  if (NumPts <= 2) or (Points = nil) then Exit;
  Bounds := GetPolygonBounds;
  W := Bounds.Right - Bounds.Left + 2;
  H := Bounds.Bottom - Bounds.Top + 2;

  if (W <= 0) or (H <= 0) then Exit;

  System.GetMem(Data, W * H);
  System.FillChar(Data^, W * H, 0); // clear bitmap context data to black
  try
    Context := CGBitmapContextCreate(Data, W, H, 8, W, CGColorSpaceCreateDeviceGray,
      kCGImageAlphaNone);
    try
      CGContextSetShouldAntialias(Context, 0); // disable anti-aliasing
      CGContextSetGrayFillColor(Context, 1.0, 1.0); // draw white polygon

      P := Points;
      CGContextBeginPath(Context);
      CGContextMoveToPoint(Context, P^.X, P^.Y);

      for I := 1 to NumPts - 1 do
      begin
        Inc(P);
        CGContextAddLineToPoint(Context, P^.X, P^.Y);
      end;

      CGContextClosePath(Context);

      if isAlter then
        CGContextEOFillPath(Context)
      else
        CGContextFillPath(Context);

      //SetLength(Line, W);

      PData := Data;
      for Y := 0 to Pred(H) do
      begin
        LC := 0; // edge is black
        for X := 0 to Pred(W) do
        begin
          C := PData^;
          //Line[X + 1] := Chr(Ord('0') + C div 255);

          if (C = $FF) and (LC = 0) then
            SX := X; // start of painted row part
          if (C = 0) and (LC = $FF) then
            // end of painted row part (SX, X)
            AddPart(SX, X,  Pred(H) - Y);

          LC := C;
          Inc(PData);
        end;
        //DebugLn(DbgS(Pred(H) - Y) + ':' + Line);
      end;

    finally
      CGContextRelease(Context);
    end;
  finally
    System.FreeMem(Data);
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Destroy

  Destroys Cocoa region
 ------------------------------------------------------------------------------}
destructor TCocoaRegion.Destroy;
begin
  CFRelease(FShape);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Apply
  Params:  ADC - Context to apply to

  Applies region to the specified context
  Note: Clipping region is only reducing
 ------------------------------------------------------------------------------}
procedure TCocoaRegion.Apply(cg: CGContextRef);
begin
  if not Assigned(cg) then Exit;
  if HIShapeIsEmpty(FShape) or (HIShapeReplacePathInCGContext(FShape, cg)<>noErr) then
    Exit;
  CGContextClip(cg);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.GetBounds
  Returns: The bounding box of Cocoa region
 ------------------------------------------------------------------------------}
function TCocoaRegion.GetBounds: TRect;
var
  R: HIRect;
begin
  if HIShapeGetBounds(FShape, R) = nil then begin
    System.FillChar(Result, sizeof(Result), 0);
    Exit;
  end;

  Result := CGRectToRect(R);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.GetType
  Returns: The type of Cocoa region
 ------------------------------------------------------------------------------}
function TCocoaRegion.GetType: TCocoaRegionType;
begin
  if not Assigned(FShape) or HIShapeIsEmpty(FShape) then
    Result := crt_Empty
  else if HIShapeIsRectangular(FShape) then
    Result := crt_Rectangle
  else
    Result := crt_Complex;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.ContainsPoint
  Params:  P - Point
  Returns: If the specified point lies in Cocoa region
 ------------------------------------------------------------------------------}
function TCocoaRegion.ContainsPoint(const P: TPoint): Boolean;
var
  cp : CGPoint;
begin
  cp.x:=P.x+0.5;
  cp.y:=P.y+0.5;
  Result := HIShapeContainsPoint(FShape, cp);
end;

procedure TCocoaRegion.SetShape(AShape: HIShapeRef);
begin
  if Assigned(FShape) then CFRelease(FShape);
  FShape := AShape;
end;

function TCocoaRegion.CombineWith(ARegion: TCocoaRegion; CombineMode: TCocoaCombine): Boolean;
var
  sh1, sh2: HIShapeRef;
const
  MinCoord=-35000;
  MaxSize=65000;
begin
  Result:=Assigned(ARegion);
  if not Assigned(ARegion) then Exit;

  if (CombineMode in [cc_AND, cc_OR, cc_XOR]) and HIShapeIsEmpty(FShape) then
    CombineMode := cc_COPY;

  case CombineMode of
    cc_AND: Shape:=HIShapeCreateIntersection(FShape, ARegion.Shape);
    cc_XOR:
    begin
      sh1 := HIShapeCreateUnion(FShape, ARegion.Shape);
      sh2 := HIShapeCreateIntersection(FShape, ARegion.Shape);
      Shape  := HIShapeCreateDifference(sh1, sh2);
      CFRelease(sh1); CFRelease(sh2);
    end;
    cc_OR:   Shape:=HIShapeCreateUnion(FShape, ARegion.Shape);
    cc_DIFF:
    begin
      if HIShapeIsEmpty(FShape) then
        {HIShapeCreateDifference doesn't work properly if original shape is empty}
        {to simulate "emptieness" very big shape is created }
        Shape:=HIShapeCreateWithRect(GetCGRect(MinCoord,MinCoord,MaxSize,MaxSize)); // create clip nothing.

      Shape:=HIShapeCreateDifference(FShape, ARegion.Shape);
    end;
    cc_COPY: Shape:=HIShapeCreateCopy(ARegion.Shape);
  else
    Result := false;
  end;
end;

{ TCocoaPen }

procedure TCocoaPen.Apply(cg:CGContextRef);
begin
  if not Assigned(cg) then Exit;
  CGContextSetRGBStrokeColor(cg, r, g, b, 1);
  CGContextSetLineWidth(cg, Width);
  //todo: style
end;

constructor TCocoaPen.Create;
begin
  inherited Create;
  Width:=1;
end;

{ TCocoaBrush }

procedure TCocoaBrush.Apply(cg:CGContextRef);
begin
  if cg = nil then Exit;
  CGContextSetRGBFillColor(cg, R,G,B, 1);
end;

{ TCocoaGDIObject }

procedure TCocoaGDIObject.AddRef;
begin
  if RefCount>=0 then inc(RefCount);
end;

procedure TCocoaGDIObject.Release;
begin
  if RefCount>0 then Dec(RefCount)
  else if RefCount=0 then Free;
end;

end.
