unit CocoaGDIObjects;
//todo: Remove MacOSAll unit to prevent Carbon framework linking.
//todo: Remove HIShape usage used in TCocoaRegion.

interface

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  MacOSAll, // for CGContextRef
  LCLtype, LCLProc, Graphics, Controls,
  CocoaAll, CocoaUtils,
  SysUtils, Classes, Contnrs, Types, Math;

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
  TCocoaBitmap = class;
  TCocoaContext = class;

  { TCocoaGDIObject }

  TCocoaGDIObject = class(TObject)
  strict private
    FRefCount: Integer;
    FGlobal: Boolean;
  public
    constructor Create(AGlobal: Boolean); virtual;

    procedure AddRef;
    procedure Release;
    property Global: Boolean read FGlobal write FGlobal;
    property RefCount: Integer read FRefCount;
  end;

  TCocoaRegionType = (
    crt_Error,
    crt_Empty,
    crt_Rectangle,
    crt_Complex);

  TCocoaCombine = (
    cc_And,
    cc_Xor,
    cc_Or,
    cc_Diff,
    cc_Copy);

  { TCocoaRegion }

  //todo: Remove HIShape usage. HIShape is legacy
  TCocoaRegion = class(TCocoaGDIObject)
  strict private
    FShape: HIShapeRef;
  public
    constructor CreateDefault;
    constructor Create(const X1, Y1, X2, Y2: Integer);
    constructor Create(Points: PPoint; NumPts: Integer; isAlter: Boolean);
    destructor Destroy; override;

    procedure Apply(ADC: TCocoaContext);
    function GetBounds: TRect;
    function GetType: TCocoaRegionType;
    function ContainsPoint(const P: TPoint): Boolean;
    procedure SetShape(AShape: HIShapeRef);
    procedure Clear;
    function CombineWith(ARegion: TCocoaRegion; CombineMode: TCocoaCombine): Boolean;
  public
    property Shape: HIShapeRef read FShape write SetShape;
  end;

  { TCocoaColorObject }

  TCocoaColorObject = class(TCocoaGDIObject)
  strict private
    FR, FG, FB: Byte;
    FA: Boolean; // alpha: True - solid, False - clear
    function GetColorRef: TColorRef;
  public
    constructor Create(const AColor: TColor; ASolid, AGlobal: Boolean); reintroduce;
    procedure SetColor(const AColor: TColor; ASolid: Boolean);
    procedure GetRGBA(AROP2: Integer; out AR, AG, AB, AA: Single);
    function CreateNSColor: NSColor;

    property Red: Byte read FR write FR;
    property Green: Byte read FG write FG;
    property Blue: Byte read FB write FB;
    property Solid: Boolean read FA write FA;
    property ColorRef: TColorRef read GetColorRef;
  end;

  { TCocoaBrush }

  TCocoaBrush = class(TCocoaColorObject)
  strict private
    FCGPattern: CGPatternRef;
    FColored: Boolean;
    FBitmap: TCocoaBitmap;
  private
    FImage: CGImageRef;
  strict protected
    procedure SetHatchStyle(AHatch: PtrInt);
    procedure SetBitmap(ABitmap: TCocoaBitmap);
    procedure SetImage(AImage: NSImage);
  public
    constructor CreateDefault(const AGlobal: Boolean = False);
    constructor Create(const ALogBrush: TLogBrush; const AGlobal: Boolean = False);
    constructor Create(const AColor: NSColor; const AGlobal: Boolean = False);
    destructor Destroy; override;
    procedure Apply(ADC: TCocoaContext; UseROP2: Boolean = True);
  end;

const
  // use the same pen shapes that are used for carbon
  CocoaDashStyle: Array [0..1] of Single = (3, 1);
  CocoaDotStyle: Array [0..1] of Single = (1, 1);
  CocoaDashDotStyle: Array [0..3] of Single = (3, 1, 1, 1);
  CocoaDashDotDotStyle: Array [0..5] of Single = (3, 1, 1, 1, 1, 1);

type
  TCocoaDashes = array of Float32;

  { TCocoaPen }

  TCocoaPen = class(TCocoaColorObject)
  strict private
    FWidth: Integer;
    FStyle: LongWord;
    FIsExtPen: Boolean;
    FIsGeometric: Boolean;
    FEndCap: CGLineCap;
    FJoinStyle: CGLineJoin;
   public
    Dashes: TCocoaDashes;
    constructor CreateDefault;
    constructor Create(const ALogPen: TLogPen; const AGlobal: Boolean = False);
    constructor Create(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord);
    constructor Create(const ABrush: TCocoaBrush; const AGlobal: Boolean = False);
    procedure Apply(ADC: TCocoaContext; UseROP2: Boolean = True);

    property Width: Integer read FWidth;
    property Style: LongWord read FStyle;
    property IsExtPen: Boolean read FIsExtPen;
    property IsGeometric: Boolean read FIsGeometric;
    property JoinStyle: CGLineJoin read FJoinStyle;
    property CapStyle: CGLineCap read FEndCap;
  end;

  { TCocoaFont }

  TCocoaFontStyle = set of (cfs_Bold, cfs_Italic, cfs_Underline, cfs_Strikeout);

  TCocoaFont = class(TCocoaGDIObject)
  strict private
    FFont: NSFont;
    FName: AnsiString;
    FSize: Integer;
    FStyle: TCocoaFontStyle;
    FAntialiased: Boolean;
  public
    constructor CreateDefault;
    constructor Create(const ALogFont: TLogFont; AFontName: String; AGlobal: Boolean = False);
    class function CocoaFontWeightToWin32FontWeight(const CocoaFontWeight: Integer): Integer; static;
    property Font: NSFont read FFont;
    property Name: String read FName;
    property Size: Integer read FSize;
    property Style: TCocoaFontStyle read FStyle;
  end;

  { TCocoaBitmap }

  TCocoaBitmap = class(TCocoaGDIObject)
  strict private
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
    FImage: NSImage;
    FImagerep: NSBitmapImageRep;
    function GetColorSpace: NSString;
  public
    constructor Create(ABitmap: TCocoaBitmap);
    constructor Create(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
      AAlignment: TCocoaBitmapAlignment; AType: TCocoaBitmapType;
      AData: Pointer; ACopyData: Boolean = True);
    destructor Destroy; override;
    procedure SetInfo(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
      AAlignment: TCocoaBitmapAlignment; AType: TCocoaBitmapType);

    function CreateSubImage(const ARect: TRect): CGImageRef;
  public
    property BitmapType: TCocoaBitmapType read FType;
    property BitsPerPixel: Byte read FBitsPerPixel;
    property BitsPerSample: NSInteger read FBitsPerSample;
    property BytesPerRow: Integer read FBytesPerRow;
    property Image: NSImage read FImage;
    property ImageRep: NSBitmapImageRep read FImageRep;
    property ColorSpace: NSString read GetColorSpace;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize;
    property Depth: Byte read FDepth;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  { TCocoaCursor }

  TCocoaCursor = class(TObject)
  strict private
    FStandard: Boolean;
    FBitmap: TCocoaBitmap;
    FCursor: NSCursor;
  public
    constructor CreateStandard(const ACursor: NSCursor);
    constructor CreateFromBitmap(const ABitmap: TCocoaBitmap; const hotSpot: NSPoint);
    destructor Destroy; override;
    function Install: TCocoaCursor;
    property Cursor: NSCursor read FCursor;
    property Standard: Boolean read FStandard;
  end;


  // device context data for SaveDC/RestoreDC
  TCocoaDCData = class
  public
    CurrentFont: TCocoaFont;
    CurrentBrush: TCocoaBrush;
    CurrentPen: TCocoaPen;
    CurrentRegion: TCocoaRegion;

    BkColor: TColor;
    BkMode: Integer;
    BkBrush: TCocoaBrush;

    TextColor: TColor;

    ROP2: Integer;
    PenPos: TPoint;
  end;

  { TCocoaTextLayout }

  TCocoaTextLayout = class
  strict private
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    FLayout: NSLayoutManager;
    FTextStorage: NSTextStorage;
    FTextContainer: NSTextContainer;
    FText: String;
    FFont: TCocoaFont;
    procedure SetBackgoundColor(AValue: TColor);
    procedure SetForegoundColor(AValue: TColor);
    procedure SetFont(AFont: TCocoaFont);
    procedure UpdateFont;
    procedure UpdateColor;
    function GetTextRange: NSRange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetText(UTF8Text: PChar; ByteSize: Integer);
    function GetSize: TSize;
    procedure Draw(ctx: NSGraphicsContext; X, Y: Integer; FillBackground: Boolean; DX: PInteger);

    property Font: TCocoaFont read FFont write SetFont;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgoundColor;
    property ForegroundColor: TColor read FForegroundColor write SetForegoundColor;
  end;

  { TCocoaContext }

  TCocoaContext = class(TObject)
  private
    FBkBrush: TCocoaBrush;
    FBkColor: TColor;
    FBkMode: Integer;
    FROP2: Integer;
    FText   : TCocoaTextLayout;
    FBrush  : TCocoaBrush;
    FPen    : TCocoaPen;
    FRegion : TCocoaRegion;
    FBitmap : TCocoaBitmap;
    FClipped: Boolean;
    FClipRegion: TCocoaRegion;
    FSavedDCList: TFPObjectList;
    FPenPos: TPoint;
    FSize: TSize;
    function GetFont: TCocoaFont;
    function GetTextColor: TColor;
    procedure SetBitmap(const AValue: TCocoaBitmap);
    procedure SetBkColor(AValue: TColor);
    procedure SetBkMode(AValue: Integer);
    procedure SetBrush(const AValue: TCocoaBrush);
    procedure SetFont(const AValue: TCocoaFont);
    procedure SetPen(const AValue: TCocoaPen);
    procedure SetRegion(const AValue: TCocoaRegion);
    procedure SetROP2(AValue: Integer);
    procedure SetTextColor(AValue: TColor);
  protected
    function SaveDCData: TCocoaDCData; virtual;
    procedure RestoreDCData(const AData: TCocoaDCData); virtual;
    procedure SetCGFillping(Ctx: CGContextRef; Width, Height: Integer);
    procedure RestoreCGFillping(Ctx: CGContextRef; Width, Height: Integer);
  public
    ctx: NSGraphicsContext;
    constructor Create;
    destructor Destroy; override;

    function SaveDC: Integer;
    function RestoreDC(ASavedDC: Integer): Boolean;

    function InitDraw(width, height: Integer): Boolean;

    // drawing functions
    procedure DrawBitmap(X,Y: Integer; ABitmap: TCocoaBitmap);
    procedure DrawFocusRect(ARect: TRect);
    procedure MoveTo(x,y: Integer);
    procedure LineTo(x,y: Integer);
    procedure Polygon(const Points: array of TPoint; NumPts: Integer; Winding: boolean);
    procedure Polyline(const Points: array of TPoint; NumPts: Integer);
    procedure Rectangle(X1, Y1, X2, Y2: Integer; FillRect: Boolean; UseBrush: TCocoaBrush);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure TextOut(X, Y: Integer; Options: Longint; Rect: PRect; UTF8Chars: PChar; Count: Integer; CharsDelta: PInteger);
    procedure Frame(const R: TRect);
    procedure Frame3d(var ARect: TRect; const FrameWidth: integer; const Style: TBevelCut);
    procedure FrameRect(const ARect: TRect; const ABrush: TCocoaBrush);
    function DrawCGImage(X, Y, Width, Height: Integer; CGImage: CGImageRef): Boolean;
    function StretchDraw(X, Y, Width, Height: Integer; SrcDC: TCocoaContext;
      XSrc, YSrc, SrcWidth, SrcHeight: Integer; Msk: TCocoaBitmap; XMsk,
      YMsk: Integer; Rop: DWORD): Boolean;

    function GetTextExtentPoint(AStr: PChar; ACount: Integer; var Size: TSize): Boolean;
    function GetTextMetrics(var TM: TTextMetric): Boolean;
    procedure SetOrigin(X, Y: Integer);
    procedure GetOrigin(var X,Y: Integer);

    function CGContext: CGContextRef; virtual;
    procedure SetAntialiasing(AValue: Boolean);

    function GetClipRect: TRect;
    function SetClipRegion(AClipRegion: TCocoaRegion; Mode: TCocoaCombine): TCocoaRegionType;
    function CopyClipRegion(ADstRegion: TCocoaRegion): TCocoaRegionType;

    property Clipped: Boolean read FClipped;
    property PenPos: TPoint read FPenPos write FPenPos;
    property ROP2: Integer read FROP2 write SetROP2;
    property Size: TSize read FSize;

    property BkColor: TColor read FBkColor write SetBkColor;
    property BkMode: Integer read FBkMode write SetBkMode;
    property BkBrush: TCocoaBrush read FBkBrush;

    property TextColor: TColor read GetTextColor write SetTextColor;

    // selected GDI objects
    property Brush: TCocoaBrush read FBrush write SetBrush;
    property Pen: TCocoaPen read FPen write SetPen;
    property Font: TCocoaFont read GetFont write SetFont;
    property Region: TCocoaRegion read FRegion write SetRegion;
    property Bitmap: TCocoaBitmap read FBitmap write SetBitmap;
  end;

var
  DefaultBrush: TCocoaBrush;
  DefaultPen: TCocoaPen;
  DefaultFont: TCocoaFont;

function CheckDC(dc: HDC): TCocoaContext;
function CheckDC(dc: HDC; Str: string): Boolean;
function CheckGDIOBJ(obj: HGDIOBJ): TCocoaGDIObject;
function CheckBitmap(ABitmap: HBITMAP; AStr: string): Boolean;

implementation

uses
  CocoaInt;

//todo: a better check!

function CheckDC(dc: HDC): TCocoaContext;
begin
  Result := TCocoaContext(dc);
end;

function CheckDC(dc: HDC; Str: string): Boolean;
begin
  Result := dc<>0;
end;

function CheckGDIOBJ(obj: HGDIOBJ): TCocoaGDIObject;
begin
  Result := TCocoaGDIObject(obj);
end;

function CheckBitmap(ABitmap: HBITMAP; AStr: string): Boolean;
begin
  Result := ABitmap <> 0;
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

  NSGraphicsContextFix = objccategory external(NSGraphicsContext)
    procedure setImageInterpolation(interpolation: NSImageInterpolation); message 'setImageInterpolation:';
    procedure setShouldAntialias(antialias: Boolean); message 'setShouldAntialias:';
  end;

{ TCocoaFont }

constructor TCocoaFont.CreateDefault;
begin
  inherited Create(False);
  FFont := NSFont.systemFontOfSize(0);
  FName := NSStringToString(FFont.familyName);
  FSize := Round(NSFont.systemFontSize);
  FStyle := [];
end;

constructor TCocoaFont.Create(const ALogFont: TLogFont; AFontName: String;
  AGlobal: Boolean);
var
  FontName: NSString;
  Descriptor: NSFontDescriptor;
  Attributes: NSDictionary;
  Pool: NSAutoreleasePool;
  Win32Weight, LoopCount: Integer;
  CocoaWeight: NSInteger;
begin
  inherited Create(AGlobal);

  Pool := NSAutoreleasePool.alloc.init;

  FName := AFontName;
  if FName = 'default' then
  begin
    FName := NSStringToString(NSFont.systemFontOfSize(0).familyName);
    FSize := Round(NSFont.systemFontSize);
  end
  else
    FSize := ALogFont.lfHeight;

  // create font attributes
  Win32Weight := ALogFont.lfWeight;
  FStyle := [];
  if ALogFont.lfItalic > 0 then
    include(FStyle, cfs_Italic);
  if Win32Weight > FW_NORMAL then
    include(FStyle, cfs_Bold);
  if ALogFont.lfUnderline > 0 then
    include(FStyle, cfs_Underline);
  if ALogFont.lfStrikeOut > 0 then
    include(FStyle, cfs_StrikeOut);


  Attributes := NSDictionary.dictionaryWithObjectsAndKeys(
        NSStringUTF8(FName), NSFontFamilyAttribute,
        NSNumber.numberWithFloat(ALogFont.lfHeight), NSFontSizeAttribute,
        nil);

  Descriptor := NSFontDescriptor.fontDescriptorWithFontAttributes(Attributes);
  FFont := NSFont.fontWithDescriptor_textTransform(Descriptor, nil);
  // we could use NSFontTraitsAttribute to request the desired font style (Bold/Italic)
  // but in this case we may get NIL as result. This way is safer.
  if cfs_Italic in Style then
    FFont := NSFontManager.sharedFontManager.convertFont_toHaveTrait(FFont, NSItalicFontMask);
  if cfs_Bold in Style then
    FFont := NSFontManager.sharedFontManager.convertFont_toHaveTrait(FFont, NSBoldFontMask);
  case ALogFont.lfPitchAndFamily and $F of
    FIXED_PITCH, MONO_FONT:
      FFont := NSFontManager.sharedFontManager.convertFont_toHaveTrait(FFont, NSFixedPitchFontMask);
    VARIABLE_PITCH:
      FFont := NSFontManager.sharedFontManager.convertFont_toNotHaveTrait(FFont, NSFixedPitchFontMask);
  end;
  if Win32Weight <> FW_DONTCARE then
  begin
    // currently if we request the desired waight by Attributes we may get a nil font
    // so we need to get font weight and to convert it to lighter/havier
    LoopCount := 0;
    repeat
      // protection from endless loop
      if LoopCount > 12 then
        Exit;
      CocoaWeight := CocoaFontWeightToWin32FontWeight(NSFontManager.sharedFontManager.weightOfFont(FFont));
      if CocoaWeight < Win32Weight then
        FFont := NSFontManager.sharedFontManager.convertWeight_ofFont(True, FFont)
      else
      if CocoaWeight > Win32Weight then
        FFont := NSFontManager.sharedFontManager.convertWeight_ofFont(False, FFont);
      inc(LoopCount);
    until CocoaWeight = Win32Weight;
  end;
  FFont.retain;
  Pool.release;
end;

class function TCocoaFont.CocoaFontWeightToWin32FontWeight(const CocoaFontWeight: Integer): Integer; static;
begin
  case CocoaFontWeight of
    0, 1: Result := FW_THIN;
    2: Result := FW_ULTRALIGHT;
    3: Result := FW_EXTRALIGHT;
    4: Result := FW_LIGHT;
    5: Result := FW_NORMAL;
    6: Result := FW_MEDIUM;
    7, 8: Result := FW_SEMIBOLD;
    9: Result := FW_BOLD;
    10: Result := FW_EXTRABOLD;
  else
    Result := FW_HEAVY;
  end;
end;

{ TCocoaColorObject }

function TCocoaColorObject.GetColorRef: TColorRef;
begin
  Result := TColorRef(RGBToColor(FR, FG, FB));
end;

constructor TCocoaColorObject.Create(const AColor: TColor; ASolid,
  AGlobal: Boolean);
begin
  inherited Create(AGlobal);

  SetColor(AColor, ASolid);
end;

procedure TCocoaColorObject.SetColor(const AColor: TColor; ASolid: Boolean);
begin
  RedGreenBlue(ColorToRGB(AColor), FR, FG, FB);
  FA := ASolid;
end;

procedure TCocoaColorObject.GetRGBA(AROP2: Integer; out AR, AG, AB, AA: Single
  );
begin
  case AROP2 of
    R2_BLACK:
    begin
      AR := 0;
      AG := 0;
      AB := 0;
      AA := Byte(FA);
    end;
    R2_WHITE:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := Byte(FA);
    end;
    R2_NOP:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := 0;
    end;
    R2_NOT:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := Byte(FA);
    end;
    R2_NOTCOPYPEN:
    begin
      AR := (255 - FR) / 255;
      AG := (255 - FG) / 255;
      AB := (255 - FB) / 255;
      AA := Byte(FA);
    end;
  else // copy
    begin
      AR := FR / 255;
      AG := FG / 255;
      AB := FB / 255;
      AA := Byte(FA);
    end;
  end;
end;

function TCocoaColorObject.CreateNSColor: NSColor;
begin
  Result := NSColor.colorWithCalibratedRed_green_blue_alpha(FR / 255, FG / 255, FB / 255, Byte(FA));
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
begin
  inherited Create(False);
  {$ifdef VerboseBitmaps}
  DebugLn(Format('[TCocoaBitmap.Create] AWidth=%d AHeight=%d ADepth=%d ABitsPerPixel=%d'
    + ' AAlignment=%d AType=%d AData=? ACopyData=%d',
    [AWidth, AHeight, ADepth, ABitsPerPixel, Integer(AAlignment), Integer(AType), Integer(ACopyData)]));
  {$endif}
  SetInfo(AWidth, AHeight, ADepth, ABitsPerPixel, AAlignment, AType);

  // Copy the image data, if necessary
  if (AData = nil) or ACopyData then
  begin
    System.GetMem(FData, FDataSize);
    FFreeData := True;
    if AData <> nil then
      System.Move(AData^, FData^, FDataSize) // copy data
    else
      FillDWord(FData^, FDataSize shr 2, 0); // clear bitmap
  end
  else
  begin
    FData := AData;
    FFreeData := False;
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
  FImagerep := NSBitmapImageRep(NSBitmapImageRep.alloc.initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(
    @FData, // planes, BitmapDataPlanes
    FWidth, // width, pixelsWide
    FHeight,// height, PixelsHigh
    FbitsPerSample,// bitsPerSample, bps
    FsamplesPerPixel, // samplesPerPixel, sps
    HasAlpha, // hasAlpha
    False, // isPlanar
    GetColorSpace, // colorSpaceName
    BitmapFormat, // bitmapFormat
    FBytesPerRow, // bytesPerRow
    FBitsPerPixel //bitsPerPixel
    ));

  // Create the associated NSImage
  FImage := NSImage.alloc.initWithSize(NSMakeSize(AWidth, AHeight));
  Image.addRepresentation(Imagerep);
end;

destructor TCocoaBitmap.Destroy;
begin
  image.release;
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

function TCocoaBitmap.CreateSubImage(const ARect: TRect): CGImageRef;
begin
  if ImageRep = nil then
    Result := nil
  else
    Result := CGImageCreateWithImageInRect(ImageRep.CGImage, RectToCGRect(ARect));
end;

function TCocoaBitmap.GetColorSpace: NSString;
begin
  if FType in [cbtMono, cbtGray] then
    Result := NSCalibratedWhiteColorSpace
  else
    Result := NSCalibratedRGBColorSpace;
end;

constructor TCocoaBitmap.Create(ABitmap: TCocoaBitmap);
begin
  Create(ABitmap.Width, ABitmap.Height, ABitmap.Depth, ABitmap.FBitsPerPixel,
    ABitmap.FAlignment, ABitmap.FType, ABitmap.Data);
end;

{ TCocoaCursor }
constructor TCocoaCursor.CreateStandard(const ACursor: NSCursor);
begin
  FBitmap := nil;
  FCursor := ACursor;
  FStandard := True;
end;

constructor TCocoaCursor.CreateFromBitmap(const ABitmap: TCocoaBitmap; const hotSpot: NSPoint);
begin
  FBitmap := ABitmap;
  FCursor := NSCursor.alloc.initWithImage_hotSpot(ABitmap.Image, hotSpot);
  FStandard := False;
end;

destructor TCocoaCursor.Destroy;
begin
  FBitmap.Free;
  if not Standard then
    FCursor.release;
  inherited;
end;

function TCocoaCursor.Install: TCocoaCursor;
begin
  FCursor.push;
  // also request form cursors invalidation
  CocoaWidgetSet.NSApp.keyWindow.resetCursorRects;
  Result := nil;
end;

{ TCocoaTextLayout }

procedure TCocoaTextLayout.UpdateFont;
const
  UnderlineStyle = NSUnderlineStyleSingle or NSUnderlinePatternSolid;
var
  Range: NSRange;
begin
  if Assigned(FFont) then
  begin
    Range := GetTextRange;
    // apply font itself
    FTextStorage.addAttribute_value_range(NSFontAttributeName, FFont.Font, Range);
    // aply font attributes which are not in NSFont
    if cfs_Underline in FFont.Style then
      FTextStorage.addAttribute_value_range(NSUnderlineStyleAttributeName, NSNumber.numberWithInteger(UnderlineStyle), Range);
    if cfs_Strikeout in FFont.Style then
      FTextStorage.addAttribute_value_range(NSStrikethroughStyleAttributeName, NSNumber.numberWithInteger(UnderlineStyle), Range);
  end;
end;

procedure TCocoaTextLayout.UpdateColor;
begin
  FTextStorage.addAttribute_value_range(NSForegroundColorAttributeName, ColorToNSColor(ForegroundColor), GetTextRange);
  FTextStorage.addAttribute_value_range(NSBackgroundColorAttributeName, ColorToNSColor(BackgroundColor), GetTextRange);
end;

function TCocoaTextLayout.GetTextRange: NSRange;
begin
  Result.location := 0;
  Result.length := FTextStorage.length;
end;

procedure TCocoaTextLayout.SetForegoundColor(AValue: TColor);
begin
  if FForegroundColor <> AValue then
  begin
    FForegroundColor := AValue;
    FTextStorage.beginEditing;
    UpdateColor;
    FTextStorage.endEditing;
  end;
end;

procedure TCocoaTextLayout.SetBackgoundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    FTextStorage.beginEditing;
    UpdateColor;
    FTextStorage.endEditing;
  end;
end;

constructor TCocoaTextLayout.Create;
var
  S: NSString;
begin
  FLayout := NSLayoutManager.alloc.init;
  FTextContainer := NSTextContainer.alloc.init;
  FLayout.addTextContainer(FTextContainer);
  FTextContainer.release;
  S := NSSTR('');
  FTextStorage := NSTextStorage.alloc.initWithString(S);
  S.release;
  FTextStorage.addLayoutManager(FLayout);
  FLayout.release;
  FFont := DefaultFont;
  FFont.AddRef;
  FText := '';
  FBackgroundColor := clWhite;
  FForegroundColor := clBlack;
end;

destructor TCocoaTextLayout.Destroy;
begin
  FTextStorage.release;
  FFont.release;
  inherited Destroy;
end;

procedure TCocoaTextLayout.SetFont(AFont: TCocoaFont);
begin
  if FFont <> AFont then
  begin
    FFont := AFont;
    FTextStorage.beginEditing;
    updateFont;
    FTextStorage.endEditing;
  end;
end;

procedure TCocoaTextLayout.SetText(UTF8Text: PChar; ByteSize: Integer);
var
  NewText: String;
  S: NSString;
begin
  if ByteSize >= 0 then
    System.SetString(NewText, UTF8Text, ByteSize)
  else
    NewText := StrPas(UTF8Text);
  if FText <> NewText then
  begin
    FText := NewText;
    S := NSStringUTF8(NewText);
    FTextStorage.beginEditing;
    FTextStorage.replaceCharactersInRange_withString(GetTextRange, S);
    updateFont;
    updateColor;
    FTextStorage.endEditing;
    S.release;
  end;
end;

function TCocoaTextLayout.GetSize: TSize;
var
  Range: NSRange;
begin
  Range := FLayout.glyphRangeForTextContainer(FTextContainer);
  with FLayout.boundingRectForGlyphRange_inTextContainer(Range, FTextContainer).size do
  begin
    Result.cx := Round(width);
    Result.cy := Round(height);
  end;
end;

procedure TCocoaTextLayout.Draw(ctx: NSGraphicsContext; X, Y: Integer; FillBackground: Boolean; DX: PInteger);
var
  Range: NSRange;
  Pt: NSPoint;
  Context: NSGraphicsContext;
begin
  if not ctx.isFlipped then
    Context := NSGraphicsContext.graphicsContextWithGraphicsPort_flipped(ctx.graphicsPort, True)
  else
    Context := ctx;

  ctx.setCurrentContext(Context);
  Range := FLayout.glyphRangeForTextContainer(FTextContainer);
  Pt.x := X;
  Pt.y := Y;
  if FillBackground then
    FLayout.drawBackgroundForGlyphRange_atPoint(Range, Pt);
  FLayout.drawGlyphsForGlyphRange_atPoint(Range, Pt);
end;

{ TCocoaContext }

function TCocoaContext.CGContext: CGContextRef;
begin
  Result := CGContextRef(ctx.graphicsPort);
end;

procedure TCocoaContext.SetAntialiasing(AValue: Boolean);
begin
  if not AValue then
    ctx.setImageInterpolation(NSImageInterpolationNone)
  else
    ctx.setImageInterpolation(NSImageInterpolationDefault);
  ctx.setShouldAntialias(AValue);
end;

function TCocoaContext.GetClipRect: TRect;
begin
  Result := CGRectToRect(CGContextGetClipBoundingBox(CGContext));
end;

function TCocoaContext.SetClipRegion(AClipRegion: TCocoaRegion; Mode: TCocoaCombine): TCocoaRegionType;
begin
  if FClipped then
  begin
    FClipped := False;
    ctx.restoreGraphicsState;
  end;

  if not Assigned(AClipRegion) then
    FClipRegion.Clear
  else
  begin
    ctx.saveGraphicsState;
    FClipRegion.CombineWith(AClipRegion, Mode);
    FClipRegion.Apply(Self);
    FClipped := True;
  end;
  Result := FClipRegion.GetType;
end;

function TCocoaContext.CopyClipRegion(ADstRegion: TCocoaRegion): TCocoaRegionType;
begin
  if Assigned(ADstRegion) and ADstRegion.CombineWith(FClipRegion, cc_Copy) then
    Result := ADstRegion.GetType
  else
    Result := crt_Error;
end;

procedure TCocoaContext.SetBitmap(const AValue: TCocoaBitmap);
begin
  if FBitmap <> AValue then
  begin
    FBitmap:=AValue;

  end;
end;

function TCocoaContext.GetTextColor: TColor;
begin
  Result := FText.ForegroundColor;
end;

function TCocoaContext.GetFont: TCocoaFont;
begin
  Result := FText.Font;
end;

procedure TCocoaContext.SetBkColor(AValue: TColor);
begin
  AValue := TColor(ColorToRGB(AValue));
  FBkColor := AValue;
  FBkBrush.SetColor(AValue, BkMode = OPAQUE);
end;

procedure TCocoaContext.SetBkMode(AValue: Integer);
begin
  if FBkMode <> AValue then
  begin
    FBkMode := AValue;
    FBkBrush.SetColor(FBkColor, FBkMode = OPAQUE);
  end;
end;

procedure TCocoaContext.SetBrush(const AValue: TCocoaBrush);
begin
  if FBrush <> AValue then
  begin
    FBrush := AValue;
    if Assigned(FBrush) then FBrush.Apply(Self);
  end;
end;

procedure TCocoaContext.SetFont(const AValue: TCocoaFont);
begin
  FText.Font := AValue;
end;

procedure TCocoaContext.SetPen(const AValue: TCocoaPen);
begin
  if FPen <> AValue then
  begin
    FPen := AValue;
    if Assigned(FPen) then FPen.Apply(Self);
  end;
end;

procedure TCocoaContext.SetRegion(const AValue: TCocoaRegion);
begin
  if FRegion <> AValue then
  begin
    FRegion := AValue;
    if Assigned(FRegion) then FRegion.Apply(Self);
  end;
end;

procedure TCocoaContext.SetROP2(AValue: Integer);
begin
  if FROP2 <> AValue then
  begin
    FROP2 := AValue;
    Pen.Apply(Self);
    Brush.Apply(Self);
  end;
end;

procedure TCocoaContext.SetTextColor(AValue: TColor);
begin
  FText.ForegroundColor := TColor(ColorToRGB(AValue));
end;

function TCocoaContext.SaveDCData: TCocoaDCData;
begin
  Result := TCocoaDCData.Create;

  Result.CurrentFont := Font;
  Result.CurrentBrush := FBrush;
  Result.CurrentPen := FPen;
  Result.CurrentRegion := FRegion;

  Result.BkColor := FBkColor;
  Result.BkMode := FBkMode;
  Result.BkBrush := FBkBrush;

  Result.TextColor := TextColor;

  Result.ROP2 := FROP2;
  Result.PenPos := FPenPos;
end;

procedure TCocoaContext.RestoreDCData(const AData: TCocoaDCData);
begin
  if (Font <> AData.CurrentFont) then
  begin
    if Assigned(Font) then
      Font.Release;
    if Assigned(AData.CurrentFont) then
      AData.CurrentFont.AddRef;
  end;
  Font := AData.CurrentFont;

  if (FBrush <> AData.CurrentBrush) then
  begin
    if Assigned(FBrush) then
      FBrush.Release;
    if Assigned(AData.CurrentBrush) then
      AData.CurrentBrush.AddRef;
  end;
  FBrush := AData.CurrentBrush;

  if (FPen <> AData.CurrentPen) then
  begin
    if Assigned(FPen) then
      FPen.Release;
    if Assigned(AData.CurrentPen) then
      AData.CurrentPen.AddRef;
  end;
  FPen := AData.CurrentPen;

  if (FRegion <> AData.CurrentRegion) then
  begin
    if Assigned(FRegion) then
      FRegion.Release;
    if Assigned(AData.CurrentRegion) then
      AData.CurrentRegion.AddRef;
  end;
  FRegion := AData.CurrentRegion;

  FBkColor := AData.BkColor;
  FBkMode := AData.BkMode;
  FBkBrush := AData.BkBrush;

  TextColor := AData.TextColor;

  FROP2 := AData.ROP2;
  FPenPos := AData.PenPos;
end;

constructor TCocoaContext.Create;
begin
  inherited Create;

  FBkBrush := TCocoaBrush.CreateDefault;

  FBrush := DefaultBrush;
  FBrush.AddRef;
  FPen := DefaultPen;
  FPen.AddRef;
  FRegion := TCocoaRegion.CreateDefault;
  FRegion.AddRef;
  FClipRegion := FRegion;
  FText := TCocoaTextLayout.Create;
  FClipped := False;
end;

destructor TCocoaContext.Destroy;
begin
  FBkBrush.Free;

  if Assigned(FBrush) then
    FBrush.Release;
  if Assigned(FPen) then
    FPen.Release;
  if Assigned(FRegion) then
    FRegion.Release;
  FClipRegion.Free;
  FSavedDCList.Free;
  FText.Free;
  inherited Destroy;
end;

function TCocoaContext.SaveDC: Integer;
begin
  if FClipped then
    ctx.restoreGraphicsState;

  Result := 0;

  if FSavedDCList = nil then
    FSavedDCList := TFPObjectList.Create(True);

  ctx.saveGraphicsState;
  Result := FSavedDCList.Add(SaveDCData) + 1;

  if FClipped then
  begin
    ctx.saveGraphicsState;
    FClipRegion.Apply(Self);
  end;
end;

function TCocoaContext.RestoreDC(ASavedDC: Integer): Boolean;
begin
  if FClipped then
    ctx.restoreGraphicsState;

  Result := False;
  if (FSavedDCList = nil) or (ASavedDC <= 0) or (ASavedDC > FSavedDCList.Count) then
    Exit;

  while FSavedDCList.Count > ASavedDC do
  begin
    ctx.restoreGraphicsState;
    FSavedDCList.Delete(FSavedDCList.Count - 1);
  end;

  ctx.restoreGraphicsState;
  RestoreDCData(TCocoaDCData(FSavedDCList[ASavedDC - 1]));
  FSavedDCList.Delete(ASavedDC - 1);
  Result := True;

  if FSavedDCList.Count = 0 then FreeAndNil(FSavedDCList);

  if FClipped then
  begin
    FClipped := False;
    FClipRegion.Shape := HIShapeCreateEmpty;
  end;

end;

function TCocoaContext.InitDraw(width, height:Integer): Boolean;
var
  cg: CGContextRef;
begin
  cg := CGContext;
  Result := Assigned(cg);
  if not Result then Exit;

  FSize.cx := width;
  FSize.cy := height;

  CGContextTranslateCTM(cg, 0, height);
  CGContextScaleCTM(cg, 1, -1);
  FPenPos.x := 0;
  FPenPos.y := 0;
end;

procedure TCocoaContext.MoveTo(x, y: Integer);
begin
  FPenPos.x := x;
  FPenPos.y := y;
end;

procedure TCocoaContext.LineTo(x, y: Integer);
var
  cg: CGContextRef;
  p: array [0..1] of CGPoint;
  deltaX, deltaY, absDeltaX, absDeltaY: Integer;
  clipDeltaX, clipDeltaY: Float32;
  tx,ty:Float32;
begin
  cg := CGContext;
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

  FPenPos.x := X;
  FPenPos.y := Y;
end;

procedure CGContextAddLCLPoints(cg: CGContextRef; const Points: array of TPoint;NumPts:Integer);
var
  cp: array of CGPoint;
  i: Integer;
begin
  SetLength(cp, NumPts);
  for i:=0 to NumPts-1 do
  begin
    cp[i].x:=Points[i].X+0.5;
    cp[i].y:=Points[i].Y+0.5;
  end;
  CGContextAddLines(cg, @cp[0], NumPts);
end;

procedure CGContextAddLCLRect(cg: CGContextRef; x1, y1, x2, y2: Integer); overload;
var
  r: CGRect;
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
  cg: CGContextRef;
begin
  cg := CGContext;
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
  cg: CGContextRef;
begin
  cg := CGContext;
  if not Assigned(cg) or (NumPts<=0) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLPoints(cg, Points, NumPts);
  CGContextStrokePath(cg);
end;

procedure TCocoaContext.Rectangle(X1, Y1, X2, Y2: Integer; FillRect: Boolean; UseBrush: TCocoaBrush);
var
  cg: CGContextRef;
begin
  cg := CGContext;
  if not Assigned(cg) then Exit;

  CGContextBeginPath(cg);
  CGContextAddLCLRect(cg, X1, Y1, X2, Y2);
  if FillRect then
  begin
    //using the brush
    if Assigned(UseBrush) then UseBrush.Apply(Self);
    CGContextFillPath(cg);
    //restore the brush
    if Assigned(UseBrush) and Assigned(FBrush) then FBrush.Apply(Self);
  end
  else
    CGContextStrokePath(cg);
end;

procedure TCocoaContext.Ellipse(X1, Y1, X2, Y2:Integer);
var
  cg: CGContextRef;
  r: CGRect;
begin
  cg := CGContext;
  if not Assigned(cg) then Exit;
  r.origin.x:=x1+0.5;
  r.origin.y:=y1+0.5;
  r.size.width:=x2-x1-1;
  r.size.height:=y2-y1-1;
  CGContextBeginPath(CGContext);
  CGContextAddEllipseInRect(CGContext, R);
  CGContextDrawPath(CGContext, kCGPathFillStroke);
end;

procedure TCocoaContext.TextOut(X, Y: Integer; Options: Longint; Rect: PRect; UTF8Chars: PChar; Count: Integer; CharsDelta: PInteger);
var
  BrushSolid, FillBg: Boolean;
begin
  ctx.saveGraphicsState;

  if Assigned(Rect) then
  begin
    // fill background
    if (Options and ETO_OPAQUE) <> 0 then
    begin
      BrushSolid := BkBrush.Solid;
      BkBrush.Solid := True;
      with Rect^ do
        Rectangle(Left, Top, Right, Bottom, True, BkBrush);
      BkBrush.Solid := BrushSolid;
    end;
  end;

  FillBg := not Assigned(Rect) and ((Options and ETO_OPAQUE) <> 0);
  if FillBg then
    FText.BackgroundColor := BkBrush.ColorRef;
  FText.SetText(UTF8Chars, Count);
  FText.Draw(ctx, X, Y, FillBg, CharsDelta);

  ctx.restoreGraphicsState;
end;

procedure TCocoaContext.Frame(const R: TRect);
begin
  Rectangle(R.Left, R.Top, R.Right + 1, R.Bottom + 1, False, nil);
end;

procedure TCocoaContext.Frame3d(var ARect: TRect; const FrameWidth: integer; const Style: TBevelCut);
var
  I, D: Integer;
  DrawInfo: HIThemeGroupBoxDrawInfo;
begin
  if Style = bvRaised then
  begin
    GetThemeMetric(kThemeMetricPrimaryGroupBoxContentInset, D);

    // draw frame as group box
    DrawInfo.version := 0;
    DrawInfo.state := kThemeStateActive;
    DrawInfo.kind := kHIThemeGroupBoxKindPrimary;

    for I := 1 to FrameWidth do
    begin
      HIThemeDrawGroupBox(RectToCGRect(ARect), DrawInfo, CGContext, kHIThemeOrientationNormal);
      InflateRect(ARect, -D, -D);
    end;
  end;
end;

procedure TCocoaContext.FrameRect(const ARect: TRect; const ABrush: TCocoaBrush);
begin
  if ABrush <> Brush then
    ABrush.Apply(Self);
  if not ctx.currentContextDrawingToScreen then
    ctx.setCurrentContext(ctx);
  NSFrameRect(RectToNSRect(ARect));
  if ABrush <> Brush then
    Brush.Apply(Self);
end;

procedure TCocoaContext.SetCGFillping(Ctx: CGContextRef; Width, Height: Integer);
begin
  if Width < 0 then
  begin
    CGContextTranslateCTM(Ctx, -Width, 0);
    CGContextScaleCTM(Ctx, -1, 1);
  end;

  if Height < 0 then
  begin
    CGContextTranslateCTM(Ctx, 0, -Height);
    CGContextScaleCTM(Ctx, 1, -1);
  end;
end;

procedure TCocoaContext.RestoreCGFillping(Ctx: CGContextRef; Width, Height: Integer);
begin
  if Height < 0 then
  begin
    CGContextTranslateCTM(Ctx, 0, Height);
    CGContextScaleCTM(Ctx, 1, -1);
  end;

  if Width < 0 then
  begin
    CGContextScaleCTM(Ctx, -1, 1);
    CGContextTranslateCTM(Ctx, Width, 0);
  end;
end;

function TCocoaContext.DrawCGImage(X, Y, Width, Height: Integer;
  CGImage: CGImageRef): Boolean;
begin
  Result := False;

  // save dest context
  ctx.saveGraphicsState;

  CGContextSetBlendMode(CGContext, kCGBlendModeNormal);
  try
    SetCGFillping(CGContext, Width, Height);
    CGContextDrawImage(CGContext, GetCGRectSorted(X, Y, X + Width, Y + Height), CGImage);
    RestoreCGFillping(CGContext, Width, Height);
  finally
    ctx.restoreGraphicsState;
  end;

  Result := True;
end;

function TCocoaContext.StretchDraw(X, Y, Width, Height: Integer;
  SrcDC: TCocoaContext; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Msk: TCocoaBitmap; XMsk, YMsk: Integer; Rop: DWORD): Boolean;
var
  Image, MskImage: CGImageRef;
  SubImage, SubMask: Boolean;
  Bmp: TCocoaBitmap;
  LayRect, DstRect: CGRect;
  ImgRect: CGRect;
  LayerContext: CGContextRef;
  Layer: CGLayerRef;
  UseLayer: Boolean;
begin
  Result := False;

  Bmp := SrcDC.Bitmap;
  if Assigned(Bmp) then
    Image := Bmp.ImageRep.CGImage
  else
    Image := nil;

  if Image = nil then Exit;

  DstRect := CGRectMake(X, Y, Abs(Width), Abs(Height));

  SubMask := (Msk <> nil)
         and (Msk.Image <> nil)
         and (  (XMsk <> 0)
             or (YMsk <> 0)
             or (Msk.Width <> SrcWidth)
             or (Msk.Height <> SrcHeight));

  SubImage := ((Msk <> nil) and (Msk.Image <> nil))
           or (XSrc <> 0)
           or (YSrc <> 0)
           or (SrcWidth <> Bmp.Width)
           or (SrcHeight <> Bmp.Height);


  if SubMask then
    MskImage := Msk.CreateSubImage(Bounds(XMsk, YMsk, SrcWidth, SrcHeight))
  else
    if Assigned(Msk) then
      MskImage := Msk.ImageRep.CGImage
    else
      MskImage := nil;

  if SubImage then
    Image := Bmp.CreateSubImage(Bounds(XSrc, YSrc, SrcWidth, SrcHeight));


  UseLayer:=Assigned(MskImage)
            or (CGImageGetWidth(Image)<>SrcWidth)
            or (CGImageGetHeight(Image)<>SrcHeight);

  try
    if not UseLayer then
    begin
      // Normal drawing
      Result := DrawCGImage(X, Y, Width, Height, Image);
    end
    else
    begin
      // use temp layer to mask source image
      // todo find a way to mask "hard" when stretching, now some soft remains are visible
      LayRect := CGRectMake(0, 0, SrcWidth, SrcHeight);
      Layer := CGLayerCreateWithContext(SrcDC.CGContext, LayRect.size, nil);

      // the sub-image is out of edges
      if (CGImageGetWidth(Image)<>SrcWidth) or (CGImageGetHeight(Image)<>SrcHeight) then
      begin
        with ImgRect do
          if XSrc<0 then origin.x:=SrcWidth-CGImageGetWidth(Image) else origin.x:=0;
        with ImgRect do
          if YSrc<0 then origin.y:=0 else origin.y:=SrcHeight-CGImageGetHeight(Image);

        ImgRect.size.width:=CGImageGetWidth(Image);
        ImgRect.size.height:=CGImageGetHeight(Image);
      end
      else
        ImgRect:=LayRect;

      try
        LayerContext := CGLayerGetContext(Layer);
        CGContextScaleCTM(LayerContext, 1, -1);
        CGContextTranslateCTM(LayerContext, 0, -SrcHeight);

        SetCGFillping(LayerContext, Width, Height);
        if Assigned(MskImage) then
          CGContextClipToMask(LayerContext, ImgRect, MskImage);
        CGContextDrawImage(LayerContext, ImgRect, Image);

        CGContextDrawLayerInRect(CGContext, DstRect, Layer);

        Result := True;
      finally
        CGLayerRelease(Layer);
      end;
    end;

  finally
    if SubImage then CGImageRelease(Image);
    if SubMask then CGImageRelease(MskImage);
  end;
end;

{------------------------------------------------------------------------------
  Method:  GetTextExtentPoint
  Params:  Str   - Text string
           Count - Number of characters in string
           Size  - The record for the dimensions of the string
  Returns: If the function succeeds

  Computes the width and height of the specified string of text
 ------------------------------------------------------------------------------}
function TCocoaContext.GetTextExtentPoint(AStr: PChar; ACount: Integer; var Size: TSize): Boolean;
begin
  FText.SetText(AStr, ACount);
  Size := FText.GetSize;
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

procedure TCocoaContext.DrawFocusRect(ARect: TRect);
var
  AOutSet: SInt32;
begin
  // LCL thinks that focus cannot be drawn outside focus rects, but carbon do that
  // => correct rect
  GetThemeMetric(kThemeMetricFocusRectOutset, AOutSet);
  InflateRect(ARect, -AOutSet, -AOutSet);
  HIThemeDrawFocusRect(RectToCGRect(ARect), True, CGContext, kHIThemeOrientationNormal);
end;

procedure TCocoaContext.SetOrigin(X, Y:Integer);
var
  cg: CGContextRef;
begin
  cg := CGContext;
  if Assigned(cg) then
    CGContextTranslateCTM(cg, X, Y);
end;

procedure TCocoaContext.GetOrigin(var X, Y: Integer);
var
  cg: CGContextRef;
  t: CGAffineTransform;
begin
  cg := CGContext;
  if Assigned(cg) then
  begin
    t := CGContextGetCTM(cg);
    X := Round(t.tx);
    Y := Size.cy - Round(t.ty);
  end;
end;


{ TCocoaRegion }

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create

  Creates a new empty Cocoa region
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.CreateDefault;
begin
  inherited Create(False);

  FShape := HIShapeCreateEmpty;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create
  Params:  X1, Y1, X2, Y2 - Region bounding rectangle

  Creates a new rectangular Cocoa region
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.Create(const X1, Y1, X2, Y2: Integer);
begin
  inherited Create(False);
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
  inherited Create(False);

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
procedure TCocoaRegion.Apply(ADC: TCocoaContext);
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;
  if HIShapeIsEmpty(FShape) or (HIShapeReplacePathInCGContext(FShape, ADC.CGContext) <> noErr) then
    Exit;
  CGContextClip(ADC.CGContext);
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

procedure TCocoaRegion.Clear;
begin
  HIShapeSetEmpty(FShape)
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

procedure TCocoaPen.Apply(ADC: TCocoaContext; UseROP2: Boolean = True);

  function GetDashes(Source: TCocoaDashes): TCocoaDashes;
  var
    i: Integer;
  begin
    Result := Source;
    for i := Low(Result) to High(Result) do
      Result[i] := Result[i] * FWidth;
  end;

var
  AR, AG, AB, AA: Single;
  AROP2: Integer;
  ADashes: TCocoaDashes;
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;

  if UseROP2 then
    AROP2 := ADC.ROP2
  else
    AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, AR, AG, AB, AA);

  if AROP2 <> R2_NOT then
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeNormal)
  else
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeDifference);

  CGContextSetRGBStrokeColor(ADC.CGContext, AR, AG, AB, AA);
  CGContextSetLineWidth(ADC.CGContext, FWidth);

  if IsExtPen then
  begin
    if IsGeometric then
    begin
      CGContextSetLineCap(ADC.CGContext, FEndCap);
      CGContextSetLineJoin(ADC.CGContext, FJoinStyle);
    end;
  end;

  case FStyle of
    PS_DASH:
      begin
        ADashes := GetDashes(CocoaDashStyle);
        CGContextSetLineDash(ADC.CGContext, 0, @ADashes[0], Length(ADashes));
      end;
    PS_DOT:
      begin
        ADashes := GetDashes(CocoaDotStyle);
        CGContextSetLineDash(ADC.CGContext, 0, @ADashes[0], Length(ADashes));
      end;
    PS_DASHDOT:
      begin
        ADashes := GetDashes(CocoaDashDotStyle);
        CGContextSetLineDash(ADC.CGContext, 0, @ADashes[0], Length(ADashes));
      end;
    PS_DASHDOTDOT:
      begin
        ADashes := GetDashes(CocoaDashDotDotStyle);
        CGContextSetLineDash(ADC.CGContext, 0, @ADashes[0], Length(ADashes));
      end;
    PS_USERSTYLE:
      CGContextSetLineDash(ADC.CGContext, 0, @Dashes[0], Length(Dashes));
  else
    CGContextSetLineDash(ADC.CGContext, 0, nil, 0);
  end;
end;

constructor TCocoaPen.CreateDefault;
begin
  inherited Create(clBlack, True, False);
  FStyle := PS_SOLID;
  FWidth := 1;
  FIsExtPen := False;
  Dashes := nil;
end;

constructor TCocoaPen.Create(const ALogPen: TLogPen; const AGlobal: Boolean = False);
begin
  case ALogPen.lopnStyle of
    PS_SOLID..PS_DASHDOTDOT,
    PS_INSIDEFRAME:
      begin
        inherited Create(ColorToRGB(TColor(ALogPen.lopnColor)), True, AGlobal);
        FWidth := Max(1, ALogPen.lopnWidth.x);
      end;
    else
    begin
      inherited Create(ColorToRGB(TColor(ALogPen.lopnColor)), False, AGlobal);
      FWidth := 1;
    end;
  end;

  FStyle := ALogPen.lopnStyle;
end;

constructor TCocoaPen.Create(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush;
  dwStyleCount: DWord; lpStyle: PDWord);
var
  i: integer;
begin
  case dwPenStyle and PS_STYLE_MASK of
    PS_SOLID..PS_DASHDOTDOT,
    PS_USERSTYLE:
      begin
        inherited Create(ColorToRGB(TColor(lplb.lbColor)), True, False);
      end;
    else
    begin
      inherited Create(ColorToRGB(TColor(lplb.lbColor)), False, False);
    end;
  end;

  FIsExtPen := True;
  FIsGeometric := (dwPenStyle and PS_TYPE_MASK) = PS_GEOMETRIC;

  if IsGeometric then
  begin
    case dwPenStyle and PS_JOIN_MASK of
      PS_JOIN_ROUND: FJoinStyle := kCGLineJoinRound;
      PS_JOIN_BEVEL: FJoinStyle := kCGLineJoinBevel;
      PS_JOIN_MITER: FJoinStyle := kCGLineJoinMiter;
    end;

    case dwPenStyle and PS_ENDCAP_MASK of
      PS_ENDCAP_ROUND: FEndCap := kCGLineCapRound;
      PS_ENDCAP_SQUARE: FEndCap := kCGLineCapSquare;
      PS_ENDCAP_FLAT: FEndCap := kCGLineCapButt;
    end;
    FWidth := Max(1, dwWidth);
  end
  else
    FWidth := 1;

  if (dwPenStyle and PS_STYLE_MASK) = PS_USERSTYLE then
  begin
    SetLength(Dashes, dwStyleCount);
    for i := 0 to dwStyleCount - 1 do
      Dashes[i] := lpStyle[i];
  end;

  FStyle := dwPenStyle and PS_STYLE_MASK;
end;

constructor TCocoaPen.Create(const ABrush: TCocoaBrush; const AGlobal: Boolean);
begin
  inherited Create(ABrush.ColorRef, True, AGlobal);
  FStyle := PS_SOLID;
  FWidth := 1;
  FIsExtPen := False;
  Dashes := nil;
end;

{ TCocoaBrush }

procedure DrawBitmapPattern(info: UnivPtr; c: CGContextRef); MWPascal;
var
  ABrush: TCocoaBrush absolute info;
  AImage: CGImageRef;
begin
  AImage := ABrush.FImage;
  CGContextDrawImage(c, GetCGRect(0, 0, CGImageGetWidth(AImage), CGImageGetHeight(AImage)),
    AImage);
end;

procedure TCocoaBrush.SetHatchStyle(AHatch: PtrInt);
const
  HATCH_DATA: array[HS_HORIZONTAL..HS_DIAGCROSS] of array[0..7] of Byte =
 (
 { HS_HORIZONTAL } ($FF, $FF, $FF, $00, $FF, $FF, $FF, $FF),
 { HS_VERTICAL   } ($F7, $F7, $F7, $F7, $F7, $F7, $F7, $F7),
 { HS_FDIAGONAL  } ($7F, $BF, $DF, $EF, $F7, $FB, $FD, $FE),
 { HS_BDIAGONAL  } ($FE, $FD, $FB, $F7, $EF, $DF, $BF, $7F),
 { HS_CROSS      } ($F7, $F7, $F7, $00, $F7, $F7, $F7, $F7),
 { HS_DIAGCROSS  } ($7E, $BD, $DB, $E7, $E7, $DB, $BD, $7E)
  );
var
  ACallBacks: CGPatternCallbacks;
begin
  if AHatch in [HS_HORIZONTAL..HS_DIAGCROSS] then
  begin
    FillChar(ACallBacks, SizeOf(ACallBacks), 0);
    ACallBacks.drawPattern := @DrawBitmapPattern;
    FBitmap := TCocoaBitmap.Create(8, 8, 1, 1, cbaByte, cbtMask, @HATCH_DATA[AHatch]);
    FImage := FBitmap.ImageRep.CGImageForProposedRect_context_hints(nil, nil, nil);
    FColored := False;
    FCGPattern := CGPatternCreate(Self, GetCGRect(0, 0, 8, 8),
      CGAffineTransformIdentity, 8, 8, kCGPatternTilingConstantSpacing,
      Ord(FColored), ACallBacks);
  end;
end;

procedure TCocoaBrush.SetBitmap(ABitmap: TCocoaBitmap);
var
  AWidth, AHeight: Integer;
  ACallBacks: CGPatternCallbacks;
begin
  AWidth := ABitmap.Width;
  AHeight := ABitmap.Height;
  FillChar(ACallBacks, SizeOf(ACallBacks), 0);
  ACallBacks.drawPattern := @DrawBitmapPattern;
  FBitmap := TCocoaBitmap.Create(ABitmap);
  FImage := FBitmap.imageRep.CGImageForProposedRect_context_hints(nil, nil, nil);
  FColored := True;
  FCGPattern := CGPatternCreate(Self, GetCGRect(0, 0, AWidth, AHeight),
    CGAffineTransformIdentity, AWidth, AHeight, kCGPatternTilingConstantSpacing,
    Ord(FColored), ACallBacks);
end;

procedure TCocoaBrush.SetImage(AImage: NSImage);
var
  AWidth, AHeight: Single;
  ACallBacks: CGPatternCallbacks;
  Rect: CGRect;
begin
  FillChar(ACallBacks, SizeOf(ACallBacks), 0);
  ACallBacks.drawPattern := @DrawBitmapPattern;
  FImage := AImage.CGImageForProposedRect_context_hints(nil, nil, nil);
  FColored := True;
  Rect.origin.x := 0;
  Rect.origin.y := 0;
  Rect.size := CGSize(AImage.size);
  FCGPattern := CGPatternCreate(Self, Rect,
    CGAffineTransformIdentity, AImage.size.width, AImage.size.height, kCGPatternTilingConstantSpacing,
    Ord(FColored), ACallBacks);
end;

constructor TCocoaBrush.CreateDefault(const AGlobal: Boolean = False);
begin
  inherited Create(clWhite, True, AGlobal);
  FBitmap := nil;
  FImage := nil;
  FCGPattern := nil;
end;

constructor TCocoaBrush.Create(const ALogBrush: TLogBrush; const AGlobal: Boolean = False);
begin
  FCGPattern := nil;
  FBitmap := nil;
  FImage := nil;
  case ALogBrush.lbStyle of
    BS_SOLID:
        inherited Create(ColorToRGB(TColor(ALogBrush.lbColor)), True, AGlobal);
    BS_HATCHED:        // Hatched brush.
      begin
        inherited Create(ColorToRGB(TColor(ALogBrush.lbColor)), True, AGlobal);
        SetHatchStyle(ALogBrush.lbHatch);
      end;
    BS_DIBPATTERN,
    BS_DIBPATTERN8X8,
    BS_DIBPATTERNPT,
    BS_PATTERN,
    BS_PATTERN8X8:
      begin
        inherited Create(ColorToRGB(TColor(ALogBrush.lbColor)), False, AGlobal);
        SetBitmap(TCocoaBitmap(ALogBrush.lbHatch));
      end
    else
      inherited Create(ColorToRGB(TColor(ALogBrush.lbColor)), False, AGlobal);
  end;
end;

constructor TCocoaBrush.Create(const AColor: NSColor; const AGlobal: Boolean);
var
  RGBColor, PatternColor: NSColor;
begin
  FCGPattern := nil;
  FBitmap := nil;
  FImage := nil;
  RGBColor := AColor.colorUsingColorSpaceName(NSCalibratedRGBColorSpace);
  if Assigned(RGBColor) then
    inherited Create(NSColorToRGB(RGBColor), True, AGlobal)
  else
  begin
    PatternColor := AColor.colorUsingColorSpaceName(NSPatternColorSpace);
    if Assigned(PatternColor) then
    begin
      inherited Create(0, False, AGlobal);
      SetImage(PatternColor.patternImage);
    end
    else
      inherited Create(0, True, AGlobal);
  end;
end;

destructor TCocoaBrush.Destroy;
begin
  if FCGPattern <> nil then
    CGPatternRelease(FCGPattern);
  FBitmap.Free;
  if FImage <> nil then
    CGImageRelease(FImage);
  inherited Destroy;
end;

procedure TCocoaBrush.Apply(ADC: TCocoaContext; UseROP2: Boolean = True);
var
  RGBA: array[0..3] of Single;
  AROP2: Integer;
  APatternSpace: CGColorSpaceRef;
  BaseSpace : CGColorSpaceRef;
begin
  if ADC = nil then Exit;

  if UseROP2 then
    AROP2 := ADC.ROP2
  else
    AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, RGBA[0], RGBA[1], RGBA[2], RGBA[3]);

  if AROP2 <> R2_NOT then
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeNormal)
  else
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeDifference);

  if FCGPattern <> nil then
  begin
    if not FColored then
      BaseSpace:=CGColorSpaceCreateDeviceRGB
    else
    begin
      BaseSpace:=nil;
      RGBA[0] := 1.0;
    end;
    APatternSpace := CGColorSpaceCreatePattern(BaseSpace);
    CGContextSetFillColorSpace(ADC.CGContext, APatternSpace);
    CGColorSpaceRelease(APatternSpace);
    if Assigned(BaseSpace) then CGColorSpaceRelease(BaseSpace);
    CGContextSetFillPattern(ADC.CGcontext, FCGPattern, @RGBA[0]);
  end
  else
    CGContextSetRGBFillColor(ADC.CGContext, RGBA[0], RGBA[1], RGBA[2], RGBA[3]);
end;

{ TCocoaGDIObject }

constructor TCocoaGDIObject.Create(AGlobal: Boolean);
begin
  FRefCount := 0;
  FGlobal := AGlobal;
end;

procedure TCocoaGDIObject.AddRef;
begin
  if FGlobal then Exit;
  if FRefCount >= 0 then inc(FRefCount);
end;

procedure TCocoaGDIObject.Release;
begin
  if FGlobal then Exit;
  if FRefCount > 0 then
    Dec(FRefCount)
  else
  begin
    //DebugLn('TCocoaGDIObject.Release Error - ', dbgsName(self), ' RefCount = ', dbgs(FRefCount));
  end;
end;

initialization
  DefaultBrush := TCocoaBrush.CreateDefault;
  DefaultPen := TCocoaPen.CreateDefault;
  DefaultFont := TCocoaFont.CreateDefault;

finalization
  DefaultBrush.Free;
  DefaultPen.Free;
  DefaultFont.Free;
end.
