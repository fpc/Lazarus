unit CocoaGDIObjects;
//todo: Remove MacOSAll unit to prevent Carbon framework linking.
//todo: Remove HIShape usage used in TCocoaRegion.

interface

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

uses
  MacOSAll, // for CGContextRef
  LCLtype, LCLProc, Graphics, Controls, fpcanvas, ImgList,
  CocoaAll, CocoaUtils,
  cocoa_extra,
  {$ifndef CocoaUseHITheme}
  customdrawndrawers, customdrawn_mac,
  {$endif}
  SysUtils, Classes, Contnrs, Types, Math, GraphMath;

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
    cbtABGR,  // color bitmap with alpha channel first 8-8-8-8 A-B-G-R
    cbtBGRA   // color bitmap with alpha channel last 8-8-8-8 B-G-R-A
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
    destructor Destroy; override;

    class function UpdateRefs(ATarget: TCocoaGDIObject; ASource: TCocoaGDIObject): Boolean; static;
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
    constructor CreateDefault(AGlobal: Boolean = False);
    constructor Create(const X1, Y1, X2, Y2: Integer);
    constructor Create(Points: PPoint; NumPts: Integer; isAlter: Boolean);
    destructor Destroy; override;

    procedure Apply(ADC: TCocoaContext);
    function GetBounds: TRect;
    function GetType: TCocoaRegionType;
    function ContainsPoint(const P: TPoint): Boolean;
    procedure SetShape(AShape: HIShapeRef);
    procedure Clear;
    function CombineWith(ARegion: TCocoaRegion; CombineMode: TCocoaCombine): TCocoaRegionType;
    procedure Offset(dx, dy: Integer);
    function GetShapeCopy: HIShapeRef;
    procedure MakeMutable;
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
    procedure GetRGBA(AROP2: Integer; out AR, AG, AB, AA: CGFloat);
    function ObtainNSColor: NSColor;

    property Red: Byte read FR write FR;
    property Green: Byte read FG write FG;
    property Blue: Byte read FB write FB;
    property Solid: Boolean read FA write FA;
    property ColorRef: TColorRef read GetColorRef;
  end;

  TCocoaPatternColorMode = (cpmBitmap, cpmBrushColor, cpmContextColor);

  { TCocoaBrush }

  TCocoaPatternInfo = record
    image: CGImageRef;
    bgColor: TColorRef;
    fgColor: TColorRef;
    colorMode:  TCocoaPatternColorMode;
  end;
  PCocoaPatternInfo = ^TCocoaPatternInfo;

  TCocoaBrush = class(TCocoaColorObject)
  strict private
    FCGPattern: CGPatternRef;
    FPatternColorMode: TCocoaPatternColorMode;
    FBitmap: TCocoaBitmap;
    FColor: NSColor;
    FFgColor: TColorRef;
  private
    FImage: CGImageRef;
  strict protected
    procedure Clear;
    procedure CreateCGPattern(ARect: CGRect; IsColored: ShortInt);
    procedure SetHatchStyle(AHatch: PtrInt);
    procedure SetBitmap(ABitmap: TCocoaBitmap);
    procedure SetImage(AImage: NSImage);
    procedure SetColor(AColor: NSColor); overload;
  public
    constructor CreateDefault(const AGlobal: Boolean = False);
    constructor Create(const ALogBrush: TLogBrush; const AGlobal: Boolean = False);
    constructor Create(const AColor: NSColor; const AGlobal: Boolean = False);
    constructor Create(const AColor: TColor; AStyle: TFPBrushStyle; APattern: TBrushPattern;
      AGlobal: Boolean = False);
    destructor Destroy; override;
    procedure Apply(ADC: TCocoaContext; UseROP2: Boolean = True);
    procedure ApplyAsPenColor(ADC: TCocoaContext; UseROP2: Boolean = True);

    // for brushes created by NCColor
    property Color: NSColor read FColor write SetColor;
  end;

type
  TCocoaStatDashes = record
    Len  : integer;
    Dash : array [0..5] of CGFloat;
  end;
  PCocoaStatDashes = ^TCocoaStatDashes;

const
  CocoaPenDash : array [Boolean] of
    array [PS_DASH..PS_DASHDOTDOT] of TCocoaStatDashes = (
    // cosmetic = false (geometry)
    (
      (len: 2; dash: (2,2,0,0,0,0)), // PS_DASH        = 1;      { ------- }
      (len: 2; dash: (0,2,0,0,0,0)), // PS_DOT         = 2;      { ....... }
      (len: 4; dash: (2,2,0,2,0,0)), // PS_DASHDOT     = 3;      { _._._._ }
      (len: 6; dash: (2,2,0,2,0,2))  // PS_DASHDOTDOT  = 4;      { _.._.._ }
    ),
    // cosmetic = true (windows like cosmetic)
    (
      (len: 2; dash: (18,6,0,0,0,0)), // PS_DASH        = 1;      { ------- }
      (len: 2; dash: (3,3,0,0,0,0)),  // PS_DOT         = 2;      { ....... }
      (len: 4; dash: (9,6,3,6,0,0)),  // PS_DASHDOT     = 3;      { _._._._ }
      (len: 6; dash: (9,3,3,3,3,3))   // PS_DASHDOTDOT  = 4;      { _.._.._ }
    )
  );

type
  TCocoaDashes = array of CGFloat;

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
    constructor CreateDefault(const AGlobal: Boolean = False);
    constructor Create(const ALogPen: TLogPen; const AGlobal: Boolean = False);
    constructor Create(dwPenStyle, dwWidth: DWord; const lplb: TLogBrush; dwStyleCount: DWord; lpStyle: PDWord);
    constructor Create(const ABrush: TCocoaBrush; const AGlobal: Boolean = False);
    constructor Create(const AColor: TColor; AGlobal: Boolean);
    constructor Create(const AColor: TColor; AStyle: TFPPenStyle; ACosmetic: Boolean;
      AWidth: Integer; AMode: TFPPenMode; AEndCap: TFPPenEndCap;
      AJoinStyle: TFPPenJoinStyle; AGlobal: Boolean = False);
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
    FIsSystemFont: Boolean;
    FRotationDeg: Single;
  public
    constructor CreateDefault(AGlobal: Boolean = False);
    constructor Create(const ALogFont: TLogFont; AFontName: String; AGlobal: Boolean = False); reintroduce; overload;
    constructor Create(const AFont: NSFont; AGlobal: Boolean = False); overload;
    constructor Create(const AFont: NSFont; const AExtraStyle: TCocoaFontStyle;  AGlobal: Boolean = False); overload;
    destructor Destroy; override;
    class function CocoaFontWeightToWin32FontWeight(const CocoaFontWeight: Integer): Integer; static;
    procedure SetHandle(ANewHandle: NSFont; const AExtraStyle: TCocoaFontStyle = []);
    property Antialiased: Boolean read FAntialiased;
    property Font: NSFont read FFont;
    property Name: String read FName;
    property Size: Integer read FSize;
    property Style: TCocoaFontStyle read FStyle;
    property RotationDeg: Single read FRotationDeg;
  end;

  { TCocoaBitmap }

  TCocoaBitmap = class(TCocoaGDIObject)
  strict private
    FData: Pointer;
    FOriginalData: PByte; // Exists and is set in case the data needed pre-multiplication
    FAlignment: TCocoaBitmapAlignment;
    FFreeData: Boolean;
    FModified_SinceLastRecreate: Boolean;
    FDataSize: Integer;
    FBytesPerRow: Integer;
    FDepth: Byte;
    FBitsPerPixel: Byte;
    FWidth: Integer;
    FHeight: Integer;
    FType: TCocoaBitmapType;
    // Cocoa information
    FBitsPerSample: NSInteger;  // How many bits in each color component
    FSamplesPerPixel: NSInteger;// How many color components
    FImage: NSImage;
    FImagerep: NSBitmapImageRep;
    function GetColorSpace: NSString;
    function DebugShowData(): string;
  public
    constructor Create(ABitmap: TCocoaBitmap);
    constructor Create(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
      AAlignment: TCocoaBitmapAlignment; AType: TCocoaBitmapType;
      AData: Pointer; ACopyData: Boolean = True);
    constructor CreateDefault;
    destructor Destroy; override;
    procedure SetInfo(AWidth, AHeight, ADepth, ABitsPerPixel: Integer;
      AAlignment: TCocoaBitmapAlignment; AType: TCocoaBitmapType);

    procedure CreateHandle();
    procedure FreeHandle();
    procedure ReCreateHandle();
    procedure ReCreateHandle_IfModified();
    procedure SetModified();
    function CreateSubImage(const ARect: TRect): CGImageRef;
    function CreateMaskImage(const ARect: TRect): CGImageRef;
    procedure PreMultiplyAlpha();
    function GetNonPreMultipliedData(): PByte;
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
    WindowOfs: TPoint;
    ViewportOfs: TPoint;

    isClipped: Boolean;
    ClipShape: HIShapeRef;

    destructor Destroy; override;
  end;

  { TCocoaContext }

  TCocoaBitmapContext = class;
  TCocoaContext = class(TObject)
  private
    FControl: TWinControl;
    FBkBrush: TCocoaBrush;
    FBkColor: TColor;
    FBkMode: Integer;
    FROP2: Integer;
    FBrush  : TCocoaBrush;
    FBackgroundColor: TColor;
    FForegroundColor: TColor;
    FFont: TCocoaFont;
    FPen    : TCocoaPen;
    FRegion : TCocoaRegion;
    // In Cocoa there is no way to enlarge a clip region :(
    // see http://stackoverflow.com/questions/18648608/how-can-i-reset-or-clear-the-clipping-mask-associated-with-a-cgcontext
    // So before every single clip operation we need to save the DC state
    // And before every single clip operator or savedc/restoredc
    // we need to restore the dc to clear the clipping region
    //
    // Also, because of bug 28015 FClipped cannot use ctx.Restore(Save)GraphicsState;
    // it will use CGContextRestore(Save)GState(CGContext()); to save/restore DC instead
    FClipped: Boolean;
    FFlipped: Boolean;
    FClipRegion: TCocoaRegion;
    FSavedDCList: TFPObjectList;
    FPenPos: TPoint;
    FSize: TSize;
    FViewPortOfs: TPoint;
    FWindowOfs: TPoint;
    boxview : NSBox; // the view is used to draw Frame3d
    function GetFont: TCocoaFont;
    function GetTextColor: TColor;
    procedure SetBkColor(AValue: TColor);
    procedure SetBkMode(AValue: Integer);
    procedure SetBrush(const AValue: TCocoaBrush);
    procedure SetFont(const AValue: TCocoaFont);
    procedure SetPen(const AValue: TCocoaPen);
    procedure SetRegion(const AValue: TCocoaRegion);
    procedure SetROP2(AValue: Integer);
    procedure SetTextColor(AValue: TColor);

    procedure UpdateContextOfs(const AWindowOfs, AViewOfs: TPoint);
    procedure SetViewPortOfs(AValue: TPoint);
    procedure SetWindowOfs(AValue: TPoint);
  protected
    function SaveDCData: TCocoaDCData; virtual;
    procedure RestoreDCData(const AData: TCocoaDCData); virtual;
    procedure ApplyTransform(Trans: CGAffineTransform);
    procedure ClearClipping;
    procedure AttachedBitmap_SetModified(); virtual;
    procedure DrawEdgeRect(const r: TRect; flags: Cardinal; LTColor, BRColor: TColor);
  public
    ctx: NSGraphicsContext;
    isControlDC: Boolean; // control DCs should never be freed by ReleaseDC as the control will free it by itself
    isDesignDC: Boolean;  // this is a special Designer Overlay DC
    constructor Create(AGraphicsContext: NSGraphicsContext); virtual;
    destructor Destroy; override;

    function SaveDC: Integer;
    function RestoreDC(ASavedDC: Integer): Boolean;

    function InitDraw(width, height: Integer): Boolean;

    // drawing functions
    procedure DrawFocusRect(ARect: TRect);
    procedure InvertRectangle(X1, Y1, X2, Y2: Integer);
    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);
    function GetPixel(X,Y:integer): TColor; virtual;
    procedure SetPixel(X,Y:integer; AColor:TColor); virtual;
    procedure Polygon(const Points: array of TPoint; NumPts: Integer; Winding: boolean);
    procedure Polyline(const Points: array of TPoint; NumPts: Integer);
    // draws a rectangle by given LCL coordinates.
    // always outlines rectangle
    // if FillRect is set to true, then fills with either Context brush
    // OR with "UseBrush" brush, if provided
    // if FillRect is set to false, draws outlines only.
    //   if "UseBrush" is not provided, uses the current pen
    //   if "useBrush" is provided, uses the color from the defined brush
    procedure Rectangle(X1, Y1, X2, Y2: Integer; FillRect: Boolean; UseBrush: TCocoaBrush);
    procedure BackgroundFill(dirtyRect:NSRect);
    procedure RoundRect(X1, Y1, X2, Y2, RX, RY: Integer);
    procedure Ellipse(X1, Y1, X2, Y2: Integer);
    procedure TextOut(X, Y: Integer; Options: Longint; Rect: PRect; UTF8Chars: PChar; Count: Integer; CharsDelta: PInteger);
    procedure TextOut(X, Y: CGFloat; Options: Longint; Rect: PRect; UTF8Chars: PChar; Count: Integer; CharsDelta: CGFloatPtr);
    procedure DrawEdge(var Rect: TRect; edge: Cardinal; grfFlags: Cardinal);
    procedure Frame(const R: TRect);
    procedure Frame3dClassic(var ARect: TRect; const FrameWidth: integer; const Style: TBevelCut);
    procedure Frame3dBox(var ARect: TRect; const FrameWidth: integer; const Style: TBevelCut);
    procedure FrameRect(const ARect: TRect; const ABrush: TCocoaBrush);
    procedure DrawBitmap(X, Y: Integer; ABitmap: TCocoaBitmap);
    function DrawImageRep(dstRect: NSRect; const srcRect: NSRect; ImageRep: NSBitmapImageRep): Boolean;
    function StretchDraw(X, Y, Width, Height: Integer; SrcDC: TCocoaBitmapContext;
      XSrc, YSrc, SrcWidth, SrcHeight: Integer; Msk: TCocoaBitmap; XMsk,
      YMsk: Integer; Rop: DWORD): Boolean;

    function GetTextExtentPoint(AStr: PChar; ACount: Integer; var Size: TSize): Boolean;
    function GetTextMetrics(var TM: TTextMetric): Boolean;

    function CGContext: CGContextRef; virtual;
    procedure SetAntialiasing(AValue: Boolean);

    function GetLogicalOffset: TPoint;
    function GetClipRect: TRect;
    function SetClipRegion(AClipRegion: TCocoaRegion; Mode: TCocoaCombine): TCocoaRegionType;
    function CopyClipRegion(ADstRegion: TCocoaRegion): TCocoaRegionType;

    property Control: TWinControl read FControl write FControl;

    property Clipped: Boolean read FClipped;
    property Flipped: Boolean read FFlipped;
    property PenPos: TPoint read FPenPos write FPenPos;
    property ROP2: Integer read FROP2 write SetROP2;
    property Size: TSize read FSize;
    property WindowOfs: TPoint read FWindowOfs write SetWindowOfs;
    property ViewPortOfs: TPoint read FViewPortOfs write SetViewPortOfs;

    property BkColor: TColor read FBkColor write SetBkColor;
    property BkMode: Integer read FBkMode write SetBkMode;
    property BkBrush: TCocoaBrush read FBkBrush;

    property TextColor: TColor read GetTextColor write SetTextColor;

    // selected GDI objects
    property Brush: TCocoaBrush read FBrush write SetBrush;
    property Pen: TCocoaPen read FPen write SetPen;
    property Font: TCocoaFont read GetFont write SetFont;
    property Region: TCocoaRegion read FRegion write SetRegion;
  end;

  { TCocoaBitmapContext }

  TCocoaBitmapContext = class(TCocoaContext)
  private
    FBitmap : TCocoaBitmap;
    procedure SetBitmap(const AValue: TCocoaBitmap);
  protected
    procedure AttachedBitmap_SetModified(); override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function GetPixel(X,Y:integer): TColor; override;
    property Bitmap: TCocoaBitmap read FBitmap write SetBitmap;
  end;

var
  DefaultBrush: TCocoaBrush;
  DefaultPen: TCocoaPen;
  DefaultFont: TCocoaFont;
  DefaultBitmap: TCocoaBitmap;
  DefaultContext: TCocoaBitmapContext;
  ScreenContext: TCocoaContext;

function CheckDC(dc: HDC): TCocoaContext;
function CheckDC(dc: HDC; Str: string): Boolean;
function CheckGDIOBJ(obj: HGDIOBJ): TCocoaGDIObject;
function CheckBitmap(ABitmap: HBITMAP; AStr: string): Boolean;

function AllocMultiResImageFromImageList(lst: TCustomImageList; width, imgIdx: Integer): NSImage;

type

  { LCLNSGraphicsContext }

  LCLNSGraphicsContext = objccategory (NSGraphicsContext)
    function lclCGContext: CGContextRef; message 'lclCGContext';
  end;

implementation

{ LCLNSGraphicsContext }

function LCLNSGraphicsContext.lclCGcontext: CGContextRef;
begin
  if NSAppKitVersionNumber >= NSAppKitVersionNumber10_10 then
    Result := CGContext
  else
    Result := CGContextRef(graphicsPort);
end;

//todo: a better check!

function CheckDC(dc: HDC): TCocoaContext;
begin
  //Result := TCocoaContext(dc);
  if TObject(dc) is TCocoaContext then
    Result := TCocoaContext(dc)
  else
    Result := nil;
end;

function CheckDC(dc: HDC; Str: string): Boolean;
begin
  //Result := dc<>0;
  Result := (dc <> 0) and (TObject(dc) is TCocoaContext);
end;

function CheckGDIOBJ(obj: HGDIOBJ): TCocoaGDIObject;
begin
  //Result := TObject(obj) as TCocoaGDIObject;
  if TObject(obj) is TCocoaGDIObject then
    Result := TCocoaGDIObject(obj)
  else
    Result := nil;
end;

function CheckBitmap(ABitmap: HBITMAP; AStr: string): Boolean;
begin
  Result := ABitmap <> 0;
end;

function AllocMultiResImageFromImageList(lst: TCustomImageList;
  width, imgIdx: Integer): NSImage;
var
  bmp : TBitmap;
  lstres: TCustomImageListResolution;
  w   : Integer;
  sz  : NSSize;
  x,y : integer;
  img : NSImage;
  rep : NSBitmapImageRep;
  cb  : TCocoaBitmap;
begin
  img := nil;
  w := lst.WidthForPPI[width, 96];
  sz.width := w;
  sz.height := lst.HeightForWidth[w];
  bmp := TBitmap.Create;
  try
    for lstres in lst.Resolutions do begin
      lstres.GetBitmap(imgIdx, bmp);

      if bmp.Handle = 0 then
        Continue;

      // Bitmap Handle should be nothing but TCocoaBitmap
      cb := TCocoaBitmap(bmp.Handle);

      // There's NSBitmapImageRep in TCocoaBitmap, but it depends on the availability
      // of memory buffer stored with TCocoaBitmap. As soon as TCocoaBitmap is freed
      // pixels are not available. For this reason, we're making a copy of the bitmapdata
      // allowing Cocoa to allocate its own buffer (by passing nil for planes parameter)
      rep := NSBitmapImageRep(NSBitmapImageRep.alloc).initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(
        nil, // planes, BitmapDataPlanes
        Round(cb.ImageRep.size.Width), // width, pixelsWide
        Round(cb.ImageRep.size.Height),// height, PixelsHigh
        cb.ImageRep.bitsPerSample,// bitsPerSample, bps
        cb.ImageRep.samplesPerPixel, // samplesPerPixel, spp
        cb.ImageRep.hasAlpha, // hasAlpha
        False, // isPlanar
        cb.ImageRep.colorSpaceName, // colorSpaceName
        cb.ImageRep.bitmapFormat, // bitmapFormat
        cb.ImageRep.bytesPerRow, // bytesPerRow
        cb.ImageRep.BitsPerPixel //bitsPerPixel
      );
      System.Move( cb.ImageRep.bitmapData^, rep.bitmapData^, cb.ImageRep.bytesPerRow * Round(cb.ImageRep.size.height));
      if img = nil then
        img := NSImage(NSImage.alloc).initWithSize( sz );
      img.addRepresentation(rep);
      rep.release;
    end;

  finally
    bmp.Free;
  end;
  Result := img;
end;

procedure GetWindowViewTranslate(const AWindowOfs, AViewOfs: TPoint; out dx, dy: Integer); inline;
begin
  dx := AViewOfs.x - AWindowOfs.x;
  dy := AViewOfs.y - AWindowOfs.y;
end;

function isSamePoint(const p1, p2: TPoint): Boolean; inline;
begin
  Result:=(p1.x=p2.x) and (p1.y=p2.y);
end;

{ TCocoaFont }

constructor TCocoaFont.CreateDefault(AGlobal: Boolean = False);
var Pool: NSAutoreleasePool;
begin
  Pool := NSAutoreleasePool.alloc.init;
  FIsSystemFont := True;
  Create(NSFont.systemFontOfSize(0), AGlobal);
  Pool.release;
end;

constructor TCocoaFont.Create(const ALogFont: TLogFont; AFontName: String; AGlobal: Boolean);
var
  FontName: NSString;
  Descriptor: NSFontDescriptor;
  Attributes: NSDictionary;
  Pool: NSAutoreleasePool;
  Win32Weight, LoopCount: Integer;
  CocoaWeight: NSInteger;
  FTmpFont: NSFont;
  IsDefault: Boolean;
begin
  inherited Create(AGlobal);

  Pool := NSAutoreleasePool.alloc.init;
  try
    FName := AFontName;

    // If we are using a "systemFont" font we need this complex shuffling,
    // because otherwise the result is wrong in Mac OS X 10.11, see bug 30300
    // Code used for 10.10 or inferior:
    // FName := NSStringToString(NSFont.systemFontOfSize(0).familyName);
    //
    // There's a differnet issue with not using systemFont.
    // NSComboBox, if assigned a manually created font have an odd ascending-offset
    // (easily seen in Xcode interface builder as well). systemFonts()
    // don't have such issue at all. see bug 33626
    // the fix below (detecting "default" font and use systemFont()) is a potential
    // regression for bug 30300.
    //
    // There might font properties (i.e. Transform Matrix) to adjust the position of
    // the font. But at this time, it's safer to use systemFont() method
    IsDefault := IsFontNameDefault(FName);
    {if IsDefault then
    begin
      FTmpFont := NSFont.fontWithName_size(NSFont.systemFontOfSize(0).fontDescriptor.postscriptName, 0);
      FName := NSStringToString(FTmpFont.familyName);
    end;}

    if ALogFont.lfHeight = 0 then
      FSize := Round(NSFont.systemFontSize)
    else
      FSize := Abs(ALogFont.lfHeight); // To-Do: emulate WinAPI difference between negative and absolute height values

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

    // If this is not a "systemFont" Create the font ourselves
    if IsDefault then
    begin
      if ALogFont.lfPitchAndFamily = FIXED_PITCH then
      begin
        if NSAppKitVersionNumber >= NSAppKitVersionNumber10_15 then
          FFont := NSFont.monospacedSystemFontOfSize_weight(FSize, NSFontWeightRegular)
        else
          FFont := NSFont.fontWithName_size(NSSTR('Menlo'), FSize);
        if cfs_Bold in Style then
          FFont := NSFontManager.sharedFontManager.convertFont_toHaveTrait(FFont, NSBoldFontMask);
      end
      else if cfs_Bold in Style then
        FFont := NSFont.boldSystemFontOfSize( FSize )
      else
        FFont := NSFont.systemFontOfSize( FSize );
    end else begin
      FontName := NSStringUTF8(FName);
      FFont := NSFont.fontWithName_size(FontName, FSize);
      FontName.release;
    end;

    if FFont = nil then
    begin
      // fallback to system font if not found (at least we can try to apply some of the other traits)
      FName := NSStringToString(NSFont.systemFontOfSize(0).familyName);
      FontName := NSStringUTF8(FName);
      Attributes := NSDictionary.dictionaryWithObjectsAndKeys(
                 FontName, NSFontFamilyAttribute,
                 NSNumber.numberWithFloat(FSize), NSFontSizeAttribute,
                 nil);
      FontName.release;
      Descriptor := NSFontDescriptor.fontDescriptorWithFontAttributes(Attributes);
      FFont := NSFont.fontWithDescriptor_textTransform(Descriptor, nil);
      if FFont = nil then
      begin
        exit;
      end;
    end;
    // we could use NSFontTraitsAttribute to request the desired font style (Bold/Italic)
    // but in this case we may get NIL as result. This way is safer.
    if cfs_Italic in Style then
      FFont := NSFontManager.sharedFontManager.convertFont_toHaveTrait(FFont, NSItalicFontMask);
    if not IsDefault then
    begin
      if cfs_Bold in Style then
        FFont := NSFontManager.sharedFontManager.convertFont_toHaveTrait(FFont, NSBoldFontMask);
      case ALogFont.lfPitchAndFamily and $F of
        FIXED_PITCH, MONO_FONT:
          FFont := NSFontManager.sharedFontManager.convertFont_toHaveTrait(FFont, NSFixedPitchFontMask);
      end;
    end;
    if (Win32Weight <> FW_DONTCARE) and (not IsDefault or (Win32Weight <> FW_BOLD)) then
    begin
      // currently if we request the desired weight by Attributes we may get a nil font
      // so we need to get font weight and to convert it to lighter/heavier
      LoopCount := 0;
      repeat
        // protection from endless loop
        if LoopCount > 12 then
          Break;
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
    FAntialiased := ALogFont.lfQuality <> NONANTIALIASED_QUALITY;

    FRotationDeg := ALogFont.lfEscapement / 10;
  finally
    Pool.release;
  end;
end;

constructor TCocoaFont.Create(const AFont: NSFont; AGlobal: Boolean = False);
begin
  Create(AFont, [], AGlobal);
end;

constructor TCocoaFont.Create(const AFont: NSFont; const AExtraStyle: TCocoaFontStyle;
  AGlobal: Boolean = False); overload;
begin
  inherited Create(AGlobal);
  SetHandle(AFont, AExtraStyle);
end;

destructor TCocoaFont.Destroy;
begin
  if Assigned(FFont) then
    FFont.release;
  inherited;
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

procedure TCocoaFont.SetHandle(ANewHandle: NSFont; const AExtraStyle: TCocoaFontStyle = []);
var
  pool: NSAutoreleasePool;
  lsymTraits: NSFontSymbolicTraits;
begin
  if FFont <> nil then
  begin
    FFont.release;
  end;
  Pool := NSAutoreleasePool.alloc.init;
  FFont := ANewHandle;
  FFont.retain;
  FName := NSStringToString(FFont.familyName);
  FSize := Round(FFont.pointSize);

  FStyle := [];
  lsymTraits := FFont.fontDescriptor.symbolicTraits;
  if (lsymTraits and NSFontBoldTrait) <> 0 then
    Include(FStyle, cfs_Bold);
  if (lsymTraits and NSFontItalicTrait) <> 0 then
    Include(FStyle, cfs_Italic);
  FStyle := FStyle + AExtraStyle;

  FAntialiased := True;
  Pool.release;
end;

{ TCocoaColorObject }

function TCocoaColorObject.GetColorRef: TColorRef;
begin
  Result := TColorRef(RGBToColor(FR, FG, FB));
end;

constructor TCocoaColorObject.Create(const AColor: TColor; ASolid, AGlobal: Boolean);
begin
  inherited Create(AGlobal);

  SetColor(AColor, ASolid);
end;

procedure TCocoaColorObject.SetColor(const AColor: TColor; ASolid: Boolean);
begin
  RedGreenBlue(ColorToRGB(AColor), FR, FG, FB);
  FA := ASolid;
end;

procedure TCocoaColorObject.GetRGBA(AROP2: Integer; out AR, AG, AB, AA: CGFloat);
var alpha:single;
begin
  if FA then
     alpha:=1
  else
     alpha:=0;

  case AROP2 of
    R2_BLACK:
    begin
      AR := 0;
      AG := 0;
      AB := 0;
      AA := alpha;
    end;
    R2_WHITE:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := alpha;
    end;
    R2_NOP:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := 0;
    end;
    R2_NOT, R2_NOTXORPEN:
    begin
      AR := 1;
      AG := 1;
      AB := 1;
      AA := alpha;
    end;
    R2_NOTCOPYPEN:
    begin
      AR := (255 - FR) / 255;
      AG := (255 - FG) / 255;
      AB := (255 - FB) / 255;
      AA := alpha;
    end;
  else // copy
    begin
      AR := FR / 255;
      AG := FG / 255;
      AB := FB / 255;
      AA := alpha;
    end;
  end;
end;

function TCocoaColorObject.ObtainNSColor: NSColor;
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

type
  TColorEntry = packed record
    C1, C2, C3, C4: Byte;
  end;
  PColorEntry = ^TColorEntry;

  TColorEntryArray = array[0..MaxInt div SizeOf(TColorEntry) - 1] of TColorEntry;
  PColorEntryArray = ^TColorEntryArray;


  procedure CopySwappedColorComponents(ASrcData, ADestData: PColorEntryArray; ADataSize: Integer; AType: TCocoaBitmapType);
  var
    I: Integer;
  begin
    //switch B and R components
    for I := 0 to ADataSize div SizeOf(TColorEntry) - 1 do
    begin
      case AType of
        cbtABGR:
        begin
          ADestData^[I].C1 := ASrcData^[I].C1;
          ADestData^[I].C2 := ASrcData^[I].C4;
          ADestData^[I].C3 := ASrcData^[I].C3;
          ADestData^[I].C4 := ASrcData^[I].C2;
        end;
        cbtBGRA:
        begin
          ADestData^[I].C1 := ASrcData^[I].C3;
          ADestData^[I].C2 := ASrcData^[I].C2;
          ADestData^[I].C3 := ASrcData^[I].C1;
          ADestData^[I].C4 := ASrcData^[I].C4;
        end;
      end;
    end;
  end;

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
    begin
      if AType in [cbtABGR, cbtBGRA] then
      begin
        Assert(AWidth * AHeight * SizeOf(TColorEntry) = FDataSize);
        CopySwappedColorComponents(AData, FData, FDataSize, AType);
      end
      else
        System.Move(AData^, FData^, FDataSize) // copy data
    end
    else
      FillDWord(FData^, FDataSize shr 2, 0); // clear bitmap
  end
  else
  begin
    FData := AData;
    FFreeData := False;
  end;

  CreateHandle();
end;

constructor TCocoaBitmap.CreateDefault;
begin
  Create(1, 1, 32, 32, cbaByte, cbtARGB, nil);
end;

destructor TCocoaBitmap.Destroy;
begin
  FreeHandle();
  if FFreeData then System.FreeMem(FData);
  if FOriginalData <> nil then
    System.FreeMem(FOriginalData);

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
  //WriteLn('[TCocoaBitmap.SetInfo] AWidth=', AWidth, ' AHeight=', AHeight,
  //  ' ADepth=', ADepth, ' ABitsPerPixel=', ABitsPerPixel);
  if AWidth < 1 then AWidth := 1;
  if AHeight < 1 then AHeight := 1;
  FWidth := AWidth;
  FHeight := AHeight;
  FDepth := ADepth;
  FBitsPerPixel := ABitsPerPixel;
  FType := AType;
  FAlignment := AAlignment;

  if (FType in [cbtMono, cbtGray]) and (FDepth=0) then
    FDepth := FBitsPerPixel;

  FBytesPerRow := ((AWidth * ABitsPerPixel) + 7) shr 3;
  M := FBytesPerRow and ALIGNBITS[AAlignment];
  if M <> 0 then Inc(FBytesPerRow, ALIGNBITS[AAlignment] + 1 - M);

  FDataSize := FBytesPerRow * FHeight;

  // Cocoa information
  case ABitsPerPixel of
    // Strangely, this might appear
    0:
    begin
      FBitsPerSample := 0;
      FSamplesPerPixel := 0;
    end;
    // Mono
    1:
    begin
      FBitsPerSample := 1;
      FSamplesPerPixel := 1;
    end;
    // Gray scale
    8:
    begin
      FBitsPerSample := 8;
      FSamplesPerPixel := 1;
    end;
    // ARGB
    32:
    begin
      FBitsPerSample := 8;
      if AType = cbtRGB then
        FSamplesPerPixel := 3
      else
        FSamplesPerPixel := 4;
    end;
  else
    // Other RGB
    FBitsPerSample := ABitsPerPixel div 3;
    FSamplesPerPixel := 3;
  end;
end;

procedure TCocoaBitmap.CreateHandle();
var
  HasAlpha: Boolean;
  BitmapFormat: NSBitmapFormat;
begin
  HasAlpha := FType in [cbtARGB, cbtRGBA, cbtABGR, cbtBGRA];
  // Non premultiplied bitmaps can't be used for bitmap context
  // So we need to pre-multiply ourselves, but only if we were allowed
  // to copy the data, otherwise we might corrupt the original
  if FFreeData then
    PreMultiplyAlpha();
  BitmapFormat := 0;
  if FType in [cbtARGB, cbtABGR, cbtRGB] then
    BitmapFormat := BitmapFormat or NSAlphaFirstBitmapFormat;

  //WriteLn('[TCocoaBitmap.Create] FSamplesPerPixel=', FSamplesPerPixel,
  //  ' FData=', DebugShowData());

  // Create the associated NSImageRep
  Assert(FImagerep = nil);
  FImagerep := NSBitmapImageRep(NSBitmapImageRep.alloc.initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(
    @FData, // planes, BitmapDataPlanes
    FWidth, // width, pixelsWide
    FHeight,// height, PixelsHigh
    FBitsPerSample,// bitsPerSample, bps
    FSamplesPerPixel, // samplesPerPixel, spp
    HasAlpha, // hasAlpha
    False, // isPlanar
    GetColorSpace, // colorSpaceName
    BitmapFormat, // bitmapFormat
    FBytesPerRow, // bytesPerRow
    FBitsPerPixel //bitsPerPixel
    ));

  // Create the associated NSImage
  Assert(FImage = nil);
  FImage := NSImage.alloc.initWithSize(NSMakeSize(FWidth, FHeight));
  //pool := NSAutoreleasePool.alloc.init;
  Image.addRepresentation(Imagerep);
  //pool.release;
end;

procedure TCocoaBitmap.FreeHandle;
begin
  if FImage <> nil then
  begin
    FImage.release;
    FImage := nil;
  end;
  if FImageRep <> nil then
  begin
    FImageRep.release;
    FImageRep := nil;
  end;
end;

procedure TCocoaBitmap.ReCreateHandle;
begin
  FreeHandle();
  if (FOriginalData <> nil) and (FData <> nil) then // fix bug 28692
    System.Move(FOriginalData^, FData^, FDataSize);
  CreateHandle();
end;

procedure TCocoaBitmap.ReCreateHandle_IfModified;
begin
  if FModified_SinceLastRecreate then
    ReCreateHandle();
  FModified_SinceLastRecreate := False;
end;

procedure TCocoaBitmap.SetModified;
begin
  if FOriginalData <> nil then
  begin
    // the original data no longer applies, as imageRep was modified
    System.FreeMem(FOriginalData);
    FOriginalData:=nil;
  end;
  FModified_SinceLastRecreate := True;
end;

function TCocoaBitmap.CreateSubImage(const ARect: TRect): CGImageRef;
begin
  if ImageRep = nil then
    Result := nil
  else
    Result := CGImageCreateWithImageInRect(MacOSAll.CGImageRef(ImageRep.CGImage), RectToCGRect(ARect));
end;


function TCocoaBitmap.CreateMaskImage(const ARect: TRect): CGImageRef;
var
  CGDataProvider: CGDataProviderRef;
  Mask: CGImageRef;
begin
  CGDataProvider := CGDataProviderCreateWithData(nil, FData, FDataSize, nil);
  try
    Mask := CGImageMaskCreate(FWidth, FHeight, FBitsPerPixel,
      FBitsPerPixel, FBytesPerRow, CGDataProvider, nil, 0);
    Result := CGImageCreateWithImageInRect(Mask, RectToCGRect(ARect));
  finally
    CGDataProviderRelease(CGDataProvider);
    CGImageRelease(Mask);
  end;
end;

function TCocoaBitmap.GetColorSpace: NSString;
begin
  if FType in [cbtMono, cbtGray] then
    Result := NSDeviceWhiteColorSpace
  else
    Result := NSDeviceRGBColorSpace;
end;

// Cocoa cannot create a context unless the image has alpha pre-multiplied
procedure TCocoaBitmap.PreMultiplyAlpha;
var
  lByteData: PByte;
  i: Integer;
  lAlpha, lRed, lGreen, lBlue: Byte;
begin
  if not (FType in [cbtARGB, cbtRGBA]) then Exit;
  if FData = nil then Exit;

  // Keep the original data in a copy, otherwise we cant get access to it
  // because pre-multiplying destroys the original value if we had alpha=0
  if FOriginalData <> nil then
    System.FreeMem(FOriginalData);
  System.GetMem(FOriginalData, FDataSize);
  System.Move(FData^, FOriginalData^, FDataSize); // copy data

  // Pre-Multiply
  lByteData := PByte(FData);
  i := 0;
  while i < FDataSize - 3 do
  begin
    if FType = cbtARGB then
    begin
      lAlpha := lByteData[i];
      lRed := lByteData[i+1];
      lGreen := lByteData[i+2];
      lBlue := lByteData[i+3];

      lByteData[i+1] := (lRed * lAlpha) div $FF;
      lByteData[i+2] := (lGreen * lAlpha) div $FF;
      lByteData[i+3] := (lBlue * lAlpha) div $FF;
    end
    else if FType = cbtRGBA then
    begin
      lAlpha := lByteData[i+3];
      lRed := lByteData[i];
      lGreen := lByteData[i+1];
      lBlue := lByteData[i+2];

      lByteData[i] := (lRed * lAlpha) div $FF;
      lByteData[i+1] := (lGreen * lAlpha) div $FF;
      lByteData[i+2] := (lBlue * lAlpha) div $FF;
    end;

    Inc(i, 4);
  end;
end;

// The Alpha pre-multiplication will prevent us from obtaining the original image
// raw data for the function RawImage_FromCocoaBitmap,
// so we need to store it
function TCocoaBitmap.GetNonPreMultipliedData(): PByte;
begin
  if FOriginalData <> nil then
    Result := FOriginalData
  else
    Result := PByte(FData);
end;

function TCocoaBitmap.DebugShowData: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FDataSize -1 do
  begin
    Result := Result + IntToHex(PByte(FData)[i], 2);
    if i mod 4 = 3 then
      Result := Result + ' - '
  end;
end;

constructor TCocoaBitmap.Create(ABitmap: TCocoaBitmap);
begin
  Create(ABitmap.Width, ABitmap.Height, ABitmap.Depth, ABitmap.FBitsPerPixel,
    ABitmap.FAlignment, ABitmap.FType, ABitmap.Data);
end;

{ TCocoaContext }

function TCocoaContext.CGContext: CGContextRef;
begin
  Result := CGContextRef(ctx.lclCGContext);
end;

procedure TCocoaContext.SetAntialiasing(AValue: Boolean);
begin
  if not AValue then
    ctx.setImageInterpolation(NSImageInterpolationNone)
  else
    ctx.setImageInterpolation(NSImageInterpolationDefault);
  ctx.setShouldAntialias(AValue);
end;

function TCocoaContext.GetLogicalOffset: TPoint;
begin
  GetWindowViewTranslate(WindowOfs, ViewportOfs, Result.X, Result.Y);
end;

function TCocoaContext.GetClipRect: TRect;
begin
  Result := CGRectToRect(CGContextGetClipBoundingBox(CGContext));
end;

function TCocoaContext.SetClipRegion(AClipRegion: TCocoaRegion; Mode: TCocoaCombine): TCocoaRegionType;
begin
  ClearClipping;
  FClipped := False;

  if not Assigned(AClipRegion) then
    FClipRegion.Clear
  else
  begin
    CGContextSaveGState(CGContext());
    FClipRegion.CombineWith(AClipRegion, Mode);
    FClipRegion.Apply(Self);
    FClipped := True;
  end;
  Result := FClipRegion.GetType;
end;

function TCocoaContext.CopyClipRegion(ADstRegion: TCocoaRegion): TCocoaRegionType;
begin
  if Assigned(ADstRegion) then
    Result := ADstRegion.CombineWith(FClipRegion, cc_Copy)
  else
    Result := crt_Error;
end;

function TCocoaContext.GetTextColor: TColor;
begin
  Result := FForegroundColor;
end;

function TCocoaContext.GetFont: TCocoaFont;
begin
  Result := FFont;
end;

procedure TCocoaContext.SetBkColor(AValue: TColor);
begin
  AValue := ColorToRGB(AValue);
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
  if TCocoaGDIObject.UpdateRefs(FBrush, AValue) then
  begin
    FBrush := AValue;
    if Assigned(FBrush) then FBrush.Apply(Self);
  end;
end;

procedure TCocoaContext.SetFont(const AValue: TCocoaFont);
begin
  if TCocoaGDIObject.UpdateRefs(FFont, AValue) then begin
    FFont := AValue;
  end;
end;

procedure TCocoaContext.SetPen(const AValue: TCocoaPen);
begin
  if TCocoaGDIObject.UpdateRefs(FPen, AValue) then
  begin
    FPen := AValue;
    if Assigned(FPen) then FPen.Apply(Self);
  end;
end;

procedure TCocoaContext.SetRegion(const AValue: TCocoaRegion);
begin
  if TCocoaGDIObject.UpdateRefs(FRegion, AValue) then
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
  FForegroundColor := AValue;
end;

procedure TCocoaContext.UpdateContextOfs(const AWindowOfs, AViewOfs: TPoint);
var
  dx, dy: Integer;
begin
  if isSamePoint(AWindowOfs, FWindowOfs) and isSamePoint(AViewOfs, FViewPortOfs) then Exit;
  GetWindowViewTranslate(FWindowOfs, FViewPortOfs, dx{%H-}, dy{%H-});
  CGContextTranslateCTM(CGContext, -dx, -dy);

  FWindowOfs := AWindowOfs;
  FViewPortOfs := AViewOfs;
  GetWindowViewTranslate(FWindowOfs, FViewPortOfs, dx, dy);
  CGContextTranslateCTM(CGContext, dx, dy);
end;

procedure TCocoaContext.SetViewPortOfs(AValue: TPoint);
begin
  UpdateContextOfs(WindowOfs, AValue);
end;

procedure TCocoaContext.SetWindowOfs(AValue: TPoint);
begin
  UpdateContextOfs(AValue, ViewPortOfs);
end;

function TCocoaContext.SaveDCData: TCocoaDCData;
begin
  Result := TCocoaDCData.Create;

  Result.CurrentFont := Font;
  Result.CurrentBrush := FBrush;
  Result.CurrentPen := FPen;
  Result.CurrentRegion := FRegion;

  // Add references for retained state
  if Assigned(Result.CurrentFont) then Result.CurrentFont.AddRef;
  if Assigned(Result.CurrentBrush) then Result.CurrentBrush.AddRef;
  if Assigned(Result.CurrentPen) then Result.CurrentPen.AddRef;
  if Assigned(Result.CurrentRegion) then Result.CurrentRegion.AddRef;

  Result.BkColor := FBkColor;
  Result.BkMode := FBkMode;
  Result.BkBrush := FBkBrush;

  Result.TextColor := TextColor;

  Result.ROP2 := FROP2;
  Result.PenPos := FPenPos;

  Result.WindowOfs := FWindowOfs;
  Result.ViewportOfs := FViewportOfs;

  Result.isClipped := FClipped;
  Result.ClipShape := FClipRegion.GetShapeCopy;
end;

destructor TCocoaDCData.Destroy;
begin
  // Remove references for retained state
  if Assigned(CurrentFont) then CurrentFont.Release;
  if Assigned(CurrentBrush) then CurrentBrush.Release;
  if Assigned(CurrentPen) then CurrentPen.Release;
  if Assigned(CurrentRegion) then CurrentRegion.Release;
end;

procedure TCocoaContext.RestoreDCData(const AData: TCocoaDCData);
begin
  Font := AData.CurrentFont;
  Brush := AData.CurrentBrush;
  Pen := AData.CurrentPen;
  Region := AData.CurrentRegion;

  FBkColor := AData.BkColor;
  FBkMode := AData.BkMode;
  FBkBrush := AData.BkBrush;

  TextColor := AData.TextColor;

  FROP2 := AData.ROP2;
  FPenPos := AData.PenPos;

  FWindowOfs := AData.WindowOfs;
  FViewportOfs := AData.ViewportOfs;

  FClipped := AData.isClipped;
  FClipRegion.Shape := AData.ClipShape;
end;

constructor TCocoaContext.Create(AGraphicsContext: NSGraphicsContext);
begin
  inherited Create;

  ctx := AGraphicsContext;
  if Assigned(ctx) then
    ctx.retain;

  FBkBrush := TCocoaBrush.CreateDefault;

  FBrush := DefaultBrush;
  FBrush.AddRef;
  FPen := DefaultPen;
  FPen.AddRef;
  FFont := DefaultFont;
  FRegion := TCocoaRegion.CreateDefault;
  FClipRegion := FRegion;
  FClipRegion.AddRef;

  FSavedDCList := nil;
  FClipped := False;
  FFlipped := False;
end;

destructor TCocoaContext.Destroy;
begin
  if Assigned(FBrush) then
    FBrush.Release;
  if Assigned(FPen) then
    FPen.Release;

  if Assigned(FRegion) then
    FRegion.Release;
  FClipRegion.Release;

  FSavedDCList.Free;

  FBkBrush.Free;

  if Assigned(ctx) then begin
    if Clipped then
      CGContextRestoreGState(CGContext());
    if Flipped then
      CGContextRestoreGState(CGContext());
    ctx.release;
  end;
  if Assigned(boxview) then boxview.release;
  inherited Destroy;
end;

function TCocoaContext.SaveDC: Integer;
begin
  ClearClipping;

  Result := 0;

  if FSavedDCList = nil then
    FSavedDCList := TFPObjectList.Create(True);

  NSGraphicsContext.classSaveGraphicsState;

  //ctx.saveGraphicsState;
  Result := FSavedDCList.Add(SaveDCData) + 1;

  if FClipped then
  begin
    CGContextSaveGState(CGContext());
    FClipRegion.Apply(Self);
  end;
end;

function TCocoaContext.RestoreDC(ASavedDC: Integer): Boolean;
begin
  ClearClipping;

  Result := False;
  if (FSavedDCList = nil) or (ASavedDC <= 0) or (ASavedDC > FSavedDCList.Count) then
    Exit;

  while FSavedDCList.Count > ASavedDC do
  begin
    NSGraphicsContext.classRestoreGraphicsState;
    RestoreDCData(TCocoaDCData(FSavedDCList.Count - 1));
    FSavedDCList.Delete(FSavedDCList.Count - 1);
  end;

  NSGraphicsContext.classRestoreGraphicsState;
  RestoreDCData(TCocoaDCData(FSavedDCList[ASavedDC - 1]));
  FSavedDCList.Delete(ASavedDC - 1);
  Result := True;

  if FSavedDCList.Count = 0 then FreeAndNil(FSavedDCList);

  if FClipped then
  begin
    CGContextSaveGState(CGContext());
    FClipRegion.Apply(Self);
  end;
end;

function TCocoaContext.InitDraw(width, height:Integer): Boolean;
var
  cg: CGContextRef;
begin
  cg := CGContext;
  Result := Assigned(cg);
  if not Result then Exit;

  CGContextSaveGState(cg);
  FFlipped := True;

  FSize.cx := width;
  FSize.cy := height;

  if NOT ctx.isFlipped then begin
    CGContextTranslateCTM(cg, 0, height);
    CGContextScaleCTM(cg, 1, -1);
  end;

  FPenPos.x := 0;
  FPenPos.y := 0;
end;

procedure TCocoaContext.InvertRectangle(X1, Y1, X2, Y2: Integer);
begin
  // save dest context
{$if FPC_FULLVERSION < 30300}
  ctx.instanceSaveGraphicsState;
{$else}
  ctx.saveGraphicsState;
{$endif}
  try
    DefaultBrush.Apply(Self, False);
    CGContextSetBlendMode(CGContext, kCGBlendModeDifference);

    CGContextFillRect(CGContext, GetCGRectSorted(X1, Y1, X2, Y2));
  finally
{$if FPC_FULLVERSION < 30300}
    ctx.instanceRestoreGraphicsState;
{$else}
    ctx.restoreGraphicsState;
{$endif}
    AttachedBitmap_SetModified();
  end;
end;

procedure TCocoaContext.MoveTo(X, Y: Integer);
begin
  FPenPos.x := X;
  FPenPos.y := Y;
end;

procedure TCocoaContext.LineTo(X, Y: Integer);
var
  cg: CGContextRef;
  deltaX, deltaY, absDeltaX, absDeltaY: Integer;
  clipDeltaX, clipDeltaY: Float32;
  tx, ty, bx, by: Float32;
begin
  cg := CGContext;
  if not Assigned(cg) then Exit;

  bx := FPenPos.x;
  by := FPenPos.y;
  deltaX := X-FPenPos.x;
  deltaY := Y-FPenPos.y;
  if (deltaX=0) and (deltaY=0) then Exit;

  absDeltaX := Abs(deltaX);
  absDeltaY := Abs(deltaY);

  if (absDeltaX<=1) and (absDeltaY<=1) then
  begin
    // special case for 1-pixel lines
    tx := bx + 0.5 * deltaX;
    ty := by + 0.5 * deltay;
  end
  else
  begin
    // exclude the last pixel from the line
    if absDeltaX > absDeltaY then
    begin
      if deltaX > 0 then clipDeltaX := -0.5 else clipDeltaX := 0.5;
      clipDeltaY := clipDeltaX * deltaY / deltaX;
    end
    else
    begin
      if deltaY > 0 then clipDeltaY := -0.5 else clipDeltaY := 0.5;
      clipDeltaX := clipDeltaY * deltaX / deltaY;
    end;
    bx := bx + clipDeltaX;
    by := by + clipDeltaY;
    tx := X + clipDeltaX;
    ty := Y + clipDeltaY;
  end;

  CGContextBeginPath(cg);
  CGContextMoveToPoint(cg, bx + 0.5, by + 0.5);
  CGContextAddLineToPoint(cg, tx + 0.5, ty + 0.5);
  CGContextStrokePath(cg);

  FPenPos.x := X;
  FPenPos.y := Y;

  AttachedBitmap_SetModified();
end;

function TCocoaContext.GetPixel(X,Y:integer): TColor;
begin
  Result := 0;
end;

procedure TCocoaContext.SetPixel(X,Y:integer; AColor:TColor);
var
  cg: CGContextRef;
  fillbrush: TCocoaBrush;
  r:CGRect;
begin
  cg := CGContext;
  if not Assigned(cg) then Exit;

  fillbrush:=TCocoaBrush.Create(ColorToNSColor(ColorRef(AColor)));
  fillbrush.Apply(self);

  r.origin.x:=x;
  r.origin.y:=y;
  r.size.height:=1;
  r.size.width:=1;

  CGContextFillRect(cg,r);

  fillbrush.Free;

    //restore the brush
  if Assigned(FBrush) then
     FBrush.Apply(Self);

  AttachedBitmap_SetModified();
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

procedure CGContextAddLCLRect(cg: CGContextRef; x1, y1, x2, y2: Integer; HalfPixel: boolean); overload;
var
  r: CGRect;
begin
  if HalfPixel then
  begin
    r.origin.x:=x1+0.5;
    r.origin.y:=y1+0.5;
    r.size.width:=x2-x1-1;
    r.size.height:=y2-y1-1;
  end else
  begin
    r.origin.x:=x1;
    r.origin.y:=y1;
    r.size.width:=x2-x1;
    r.size.height:=y2-y1;
  end;
  CGContextAddRect(cg, r);
end;

procedure CGContextAddLCLRect(cg: CGContextRef; const R: TRect; HalfPixel: boolean); overload;
begin
  CGContextAddLCLRect(cg, r.Left, r.Top, r.Right, r.Bottom, HalfPixel);
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

  AttachedBitmap_SetModified();
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

  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.Rectangle(X1, Y1, X2, Y2: Integer; FillRect: Boolean; UseBrush: TCocoaBrush);
var
  cg: CGContextRef;
  resetPen: Boolean;
begin
  if (X1=X2) or (Y1=Y2) then Exit;

  cg := CGContext;
  if not Assigned(cg) then Exit;

  resetPen := false;
  CGContextBeginPath(cg);

  if FillRect then
  begin
    CGContextAddLCLRect(cg, X1, Y1, X2, Y2, false);
    //using the brush
    if Assigned(UseBrush) then
       UseBrush.Apply(Self);
    CGContextFillPath(cg);
    //restore the brush
    if Assigned(UseBrush) and Assigned(FBrush) then
       FBrush.Apply(Self);
  end
  else
  begin
    CGContextAddLCLRect(cg, X1, Y1, X2, Y2, true);
    // this is a "special" case, when UseBrush is provided
    // but "FillRect" is set to false. Use for FrameRect() function
    // (it deserves a redesign)
    if Assigned(UseBrush) then
    begin
      UseBrush.Apply(Self);
      UseBrush.ApplyAsPenColor(Self);
      resetPen := true;
    end;
  end;

  CGContextStrokePath(cg);

  AttachedBitmap_SetModified();

  if resetPen and Assigned(fPen) then // pen was modified by brush. Setting it back
    fPen.Apply(Self);
end;

procedure TCocoaContext.BackgroundFill(dirtyRect:NSRect);
var
  cg: CGContextRef;

begin
  cg := CGContext;
  if not Assigned(cg) then Exit;

  FBkBrush.Apply(Self);

  CGContextFillRect(cg,CGRect(dirtyRect));

  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.RoundRect(X1, Y1, X2, Y2, RX, RY: Integer);
var
  cg: CGContextRef;
  rx2, ry2: CGFloat;

  procedure EnsureOrder(var X, Y: Integer);
  var
    tmp: Integer;
  begin
    if Y < X then
    begin
      tmp := X;
      X := Y;
      Y := tmp;
    end;
  end;

  procedure DrawArc(X, Y, Width, Height: Integer; Angle: Integer);
  const
    ARC_LENGTH = 90*16;
  var
    B: TBezier;
  begin
    Arc2Bezier(X, Y, Width, Height, Angle, ARC_LENGTH, 0.0, B);
    CGContextAddCurveToPoint(cg, B[1].X+0.5, B[1].Y+0.5, B[2].X+0.5, B[2].Y+0.5, B[3].X+0.5, B[3].Y+0.5);
  end;

begin
  cg := CGContext;
  if not Assigned(cg) then exit;

  EnsureOrder(X1, X2);
  EnsureOrder(Y1, Y2);

  if (X2 - X1 <= 0) or (Y2 - Y1 <= 0) then
    Exit;

  if (RX > 0) and (RY > 0) then
  begin
    Dec(X2);
    Dec(Y2);

    if X2 - X1 < RX then RX := X2 - X1;
    if Y2 - Y1 < RY then RY := Y2 - Y1;

    rx2 := RX * 0.5;    // ellipse radii  (RX, RY are diameters!)
    ry2 := RY * 0.5;

    CGContextBeginPath(cg);                                 // Begin path
    CGContextMoveToPoint(cg, X1 + rx2 + 0.5, Y2 + 0.5);     // Move to start
    CGContextAddLineToPoint(cg, X2 - rx2 + 0.5, Y2 + 0.5);  // Bottom horizontal line
    DrawArc(X2 - RX, Y2 - RY, RX, RY, 270*16);              // Bottom/right arc
    CGContextAddLineToPoint(cg, X2 + 0.5, Y1 + ry2 + 0.5);  // Right vertical line
    DrawArc(X2 - RX, Y1, RX, RY, 0);                        // Top/right arc
    CGContextAddLineToPoint(cg, X1 + rx2 + 0.5, Y1 + 0.5);  // Top horizontal line
    DrawArc(X1, Y1, RX, RY, 90*16);                         // Top/left arc
    CGContextAddLineToPoint(cg, X1 + 0.5, Y2 - ry2 + 0.5);  // Left vertical line
    DrawArc(X1, Y2 - RY, RX, RY, 180*16);                   // Bottom/left arc
    CGContextClosePath(cg);                                 // Close path

    CGContextDrawPath(cg, kCGPathFillStroke);
    AttachedBitmap_SetModified();
  end
  else
    Polygon([Point(X1,Y1), Point(X1,Y2-1), Point(X2-1,Y2-1), Point(X2-1,Y1)], 4, false);
//    Rectangle(X1, Y1, X2, Y2, Assigned(Brush), nil);
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

  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.TextOut(X, Y: Integer; Options: Longint; Rect: PRect; UTF8Chars: PChar; Count: Integer; CharsDelta: PInteger);
var
  CharsDeltaFloat: Array of CGFloat;
  i: integer;
begin
  if CharsDelta <> nil then begin
    SetLength(CharsDeltaFloat, Count);
    for i := 0 to Count - 1 do
      CharsDeltaFloat[i] := CharsDelta[i];
    TextOut(X,Y,Options, Rect, UTF8Chars, Count, CGFloatPtr(@CharsDeltaFloat[0]));
  end
  else
    TextOut(X,Y,Options, Rect, UTF8Chars, Count, CGFloatPtr(nil));
end;

procedure TCocoaContext.TextOut(X, Y: CGFloat; Options: Longint; Rect: PRect; UTF8Chars: PChar; Count: Integer; CharsDelta: CGFloatPtr);
const
  UnderlineStyle = NSUnderlineStyleSingle or NSUnderlinePatternSolid;
var
  cg: CGContextRef;
  BrushSolid, FillBg: Boolean;
  AttribStr: CFAttributedStringRef;
  Str: NSString;
  CoreLine: CTLineRef;
  Glyphcount, k, i, CurGlyphCount: integer;
  Locations: array of NSPoint;
  Runs: CFArrayRef;
  CurRun: CTRunRef;
  RunCount: Integer;
  Glyphs: array of CGGlyph;
  RunFont: CTFontRef;
  Dict: NSMutableDictionary;
  YPrime, StrikeH, StrikeW: CGFloat;
  lForegroundColor: NSColor;
begin
  cg := CGContext();
  if not Assigned(cg) then
    Exit;

  CGContextSaveGState(cg);
  CGContextSetTextMatrix(cg, CGAffineTransformIdentity);

  if Assigned(Rect) then
  begin
    // fill background
    //debugln(['TCocoaContext.TextOut ',UTF8Chars,' ',dbgs(Rect^)]);
    if (Options and ETO_OPAQUE) <> 0 then
    begin
      BrushSolid := BkBrush.Solid;
      BkBrush.Solid := True;
      with Rect^ do
        Rectangle(Left, Top, Right, Bottom, True, BkBrush);
      BkBrush.Solid := BrushSolid;
    end;

    if ((Options and ETO_CLIPPED) <> 0) and (Count > 0) then
    begin
      CGContextBeginPath(cg);
      CGContextAddRect(cg, RectToCGrect(Rect^));
      CGContextClip(cg);
    end;
  end;

  if (Count > 0) then
  begin
    FillBg := BkMode = OPAQUE;
    if FillBg then
      FBackgroundColor := BkBrush.ColorRef;

    Str := NSStringUTF8(UTF8Chars, Count);
    try

      lForegroundColor := SysColorToNSColor(SysColorToSysColorIndex(FForegroundColor));
      if lForegroundColor = nil then
        lForegroundColor := ColorToNSColor(ColorToRGB(FForegroundColor));

      Dict := NSMutableDictionary.dictionaryWithObjectsAndKeys(
            Font.Font, kCTFontAttributeName,
            lForegroundColor, NSForegroundColorAttributeName,
            nil);

      if FillBg then
        Dict.setObject_forKey(ColorToNSColor(FBackgroundColor), NSBackgroundColorAttributeName);

      if cfs_Underline in Font.Style then
        Dict.setObject_forKey(NSNumber.numberWithInteger(UnderlineStyle), NSUnderlineStyleAttributeName);

      AttribStr := CFAttributedStringCreate(kCFAllocatorDefault, CFStringRef(Str), CFDictionaryRef(Dict));
      try
        CoreLine := CTLineCreateWithAttributedString(CFAttributedStringRef(AttribStr));
        try

          ctx.setShouldAntialias(Font.Antialiased);
          if FFont.RotationDeg <> 0 then
          begin
            CGContextTranslateCTM(cg, X, Y);
            CGContextRotateCTM(cg, -FFont.RotationDeg*Pi/180);
            CGContextTranslateCTM(cg, -X, -Y);
          end;

          CGContextTranslateCTM(cg, 0, FSize.Height);
          CGContextScaleCTM(cg, 1, -1);
          YPrime := FSize.Height - y - Font.Font.ascender;
          CGContextSetTextPosition(cg, x, YPrime);

          if CharsDelta = nil then
          begin
            CTLineDraw(CoreLine, cg);
          end
          else // CharsDelta <> nil;
          begin
            CGContextSetFillColorWithColor(cg, lForegroundColor.CGColor);
            GlyphCount := CTLineGetGlyphCount(CoreLine);

            SetLength(Locations, GlyphCount);
            Locations[0].x := 0;
            Locations[0].y := 0;
            for k := 1 to GlyphCount - 1 do
            begin
              Locations[k] := Locations[k - 1];
              Locations[k].x := Locations[k].x + CharsDelta[k - 1]
            end;

            Runs := CTLineGetGlyphRuns(CoreLine);
            if Runs <> nil then
            begin
              GlyphCount := 0;
              RunCount := CFArrayGetCount(Runs);
              for i := 0 to RunCount - 1 do
              begin
                CurRun := CTRunRef(CFArrayGetValueAtIndex(Runs, i));
                CurGlyphCount := CTRunGetGlyphCount(CurRun);
                SetLength(Glyphs, CurGlyphCount);
                CTRunGetGlyphs(CurRun, CFRangeMake(0,0), @Glyphs[0]);
                RunFont := CFDictionaryGetValue(CTRunGetAttributes(CurRun), kCTFontAttributeName);
                CTFontDrawGlyphs(runFont, @Glyphs[0], @Locations[GlyphCount], CurGlyphCount, cg);
                GlyphCount := GlyphCount + CurGlyphCount;
              end;
            end
          end;

          if cfs_Strikeout in FFont.Style then begin
            CGContextSetStrokeColorWithColor(cg, lForegroundColor.CGColor);
            StrikeH := Font.Font.xHeight / 2;
            StrikeW := CTLineGetTypographicBounds(CoreLine, nil, nil, nil);
            CGContextMoveToPoint(cg, X, YPrime + StrikeH);
            CGContextAddLineToPoint(cg, X + StrikeW, YPrime + StrikeH);
            CGContextStrokePath(cg);
          end;
        finally
          CFRelease(CoreLine);
        end;
      finally
        CFRelease(AttribStr);
      end;
    finally
      Str.release;
    end;
  end;

  CGContextRestoreGState(cg);

  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.DrawEdgeRect(const r: TRect; flags: Cardinal;
  LTColor, BRColor: TColor);
var
  fixedRect: TRect;
begin
  // in LineTo(), 0.5 will be added to Right/Bottom,
  // it will result in exceeding the frame,
  // therefore, the frame needs to be shrunk first
  // see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40571
  fixedRect:= r;
  if fixedRect.Width>=1 then
    dec( fixedRect.Right );
  if fixedRect.Height>=1 then
    dec( fixedRect.Bottom );

  Pen.SetColor(LTColor, true);
  Pen.Apply(self);
  if flags and BF_LEFT > 0 then
  begin
    MoveTo(fixedRect.Left, fixedRect.Bottom);
    LineTo(fixedRect.Left, fixedRect.Top);
  end;
  if flags and BF_TOP > 0 then
  begin
    MoveTo(fixedRect.Left, fixedRect.Top);
    LineTo(fixedRect.Right, fixedRect.Top);
  end;

  Pen.SetColor(BRColor, true);
  Pen.Apply(self);
  if flags and BF_RIGHT > 0 then
  begin
    MoveTo(fixedRect.Right, fixedRect.Top);
    LineTo(fixedRect.Right, fixedRect.Bottom);
  end;
  if flags and BF_BOTTOM > 0 then
  begin
    MoveTo(fixedRect.Right, fixedRect.Bottom);
    // there's a missing pixel. Seems like it's accumulating an offset
    LineTo(fixedRect.Left-1, fixedRect.Bottom);
  end;
end;

procedure TCocoaContext.DrawEdge(var Rect: TRect; edge: Cardinal;
  grfFlags: Cardinal);
var
  r: TRect;
  keepPen   : TCocoaPen;
  edgePen   : TCocoaPen;
  keepBrush : TCocoaBrush;
  edgeBrush : TCocoaBrush;
const
  OutLT = cl3DLight;    // the next to hilight
  OutBR = cl3DDkShadow; // the darkest (almost black)
  InnLT = cl3DHiLight;  // the lightest (almost white)
  InnBR = cl3DShadow;   // darker than light, lighter than dark shadow
begin
  keepPen := Pen;
  keepBrush := Brush;
  try
    edgePen := TCocoaPen.Create($FFFFFF, psSolid, false, 1, pmCopy, pecRound, pjsRound);
    edgeBrush := TCocoaBrush.Create(NSColor.whiteColor, false);
    edgeBrush.Solid := false;
    Pen := edgePen;
    Brush := edgeBrush;

    r := Rect;
    if (edge and BDR_OUTER > 0) then
    begin
      if edge and BDR_RAISEDOUTER > 0 then
        DrawEdgeRect(r, grfFlags, OutLT, OutBR)
      else
        DrawEdgeRect(r, grfFlags, InnBR, InnLT);
      InflateRect(r, -1, -1);
    end;

    if (edge and BDR_INNER > 0) then
    begin
      if edge and BDR_RAISEDINNER > 0 then
        DrawEdgeRect(r, grfFlags, InnLT, InnBR)
      else
        DrawEdgeRect(r, grfFlags, OutBR, OutLT);
    end;

  finally
    Pen := keepPen;
    Brush := keepBrush;
    edgeBrush.Free;
    edgePen.Free;
  end;
end;

procedure TCocoaContext.Frame(const R: TRect);
begin
  Rectangle(R.Left, R.Top, R.Right + 1, R.Bottom + 1, False, nil);
  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.Frame3dClassic(var ARect: TRect; const FrameWidth: integer;
  const Style: TBevelCut);
const
  Edge: array[TBevelCut] of Integer =
  (
    {bvNone   } 0,
    {bvLowered} BDR_SUNKENOUTER,
    {bvRaised } BDR_RAISEDINNER,
    {bvSpace  } 0
  );
var
  I: Integer;
begin
  for I := 0 to FrameWidth - 1 do
  begin
    DrawEdge(aRect, Edge[Style], BF_RECT);
    InflateRect(aRect,-1,-1);
  end;
end;

procedure TCocoaContext.Frame3dBox(var ARect: TRect; const FrameWidth: integer; const Style: TBevelCut);
var
  dx,dy: integer;
  ns : NSRect;
  r  : TRect;
  yy : double;
begin
  if Style = bvNone then Exit;

  if (Style = bvRaised) or (Style = bvLowered) then
  begin
    if not Assigned(boxview) then
    begin
      boxview := NSBox.alloc.initWithFrame(NSMakeRect(0,0,0,0));
      boxview.setTitle(NSString.string_);
      boxview.setTitlePosition(NSNoTitle);
    end;

    dx:=3; // layout<->frame adjustement for the box
    dy:=3; // (should be aquired using 10.7 apis)
    if Style=bvRaised then
      boxview.setBoxType(NSBoxPrimary)
    else
      boxview.setBoxType(NSBoxSecondary);
    r:=ARect;
    InflateRect(r, dx, dy);
    ns := RectToNSRect(r);
    // used for size only, position is ignored
    boxview.setFrame(ns);
    yy := ns.size.height+ns.origin.y+1;
    CGContextTranslateCTM(ctx.lclCGContext, ns.origin.x, yy);
    CGContextScaleCTM(ctx.lclCGContext, 1, -1);

    boxview.displayRectIgnoringOpacity_inContext(
      NSMakeRect(0,0,ns.size.width, ns.size.height)
      , ctx);

    CGContextScaleCTM(ctx.lclCGContext, 1, -1);
    CGContextTranslateCTM(ctx.lclCGContext, -ns.origin.x,-yy);
  end;
  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.FrameRect(const ARect: TRect; const ABrush: TCocoaBrush);
begin
  Rectangle(Arect.Left,ARect.Top,Arect.Right,ARect.Bottom, False, ABrush);
  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.ApplyTransform(Trans: CGAffineTransform);
var
  T2: CGAffineTransform;
begin
  T2 := CGContextGetCTM(CGContext);
  // restore old CTM since CTM may changed after the clipping
  if CGAffineTransformEqualToTransform(Trans, T2) = 0 then
    CGContextTranslateCTM(CGContext, Trans.a * Trans.tx - T2.a * T2.tx,
       Trans.d * Trans.ty - T2.d * T2.ty);
end;

procedure TCocoaContext.ClearClipping;
var
  Trans: CGAffineTransform;
  cgc: CGContextRef;
begin
  if FClipped then
  begin
    cgc := CGContext();
    Trans := CGContextGetCTM(cgc);
    CGContextRestoreGState(cgc);
    ApplyTransform(Trans);
    if Assigned(FPen) then FPen.Apply(Self);
    if Assigned(FBrush) then FBrush.Apply(Self);
  end;
end;

procedure TCocoaContext.AttachedBitmap_SetModified;
begin

end;

function TCocoaContext.DrawImageRep(dstRect: NSRect; const srcRect: NSRect;
  ImageRep: NSBitmapImageRep): Boolean;
var
  Context: NSGraphicsContext;
begin
  NSGraphicsContext.classSaveGraphicsState;
  try
    // we flip the context on it initialization (see InitDraw) so to draw
    // a bitmap correctly we need to create a flipped context and to draw onto it

    if not ctx.isFlipped then
      Context := NSGraphicsContext.graphicsContextWithGraphicsPort_flipped(ctx.graphicsPort, True)
    else
      Context := ctx;
    NSGraphicsContext.setCurrentContext(Context);
    Result := ImageRep.drawInRect_fromRect_operation_fraction_respectFlipped_hints(
      dstRect, srcRect, NSCompositeSourceOver, 1.0, True, nil
      );
  finally
    NSGraphicsContext.classRestoreGraphicsState;
  end;
  AttachedBitmap_SetModified();
end;

function TCocoaContext.StretchDraw(X, Y, Width, Height: Integer;
  SrcDC: TCocoaBitmapContext; XSrc, YSrc, SrcWidth, SrcHeight: Integer;
  Msk: TCocoaBitmap; XMsk, YMsk: Integer; Rop: DWORD): Boolean;
var
  dcBitmap: TCocoaBitmap;
  maskImage: CGImageRef = nil;
  cgImage: CGImageRef;
  imageRep: NSBitmapImageRep;
begin
  dcBitmap := SrcDC.Bitmap;
  if not Assigned(dcBitmap) then
    Exit(False);

  // Make sure that bitmap is the most up-to-date
  dcBitmap.ReCreateHandle_IfModified(); // Fix for bug 28102

  // see https://bugs.freepascal.org/view.php?id=34197
  // Bitmap context windowsofs should be used when rendering a bitmap
  inc(XSrc, -SrcDC.WindowOfs.X);
  inc(YSrc, -SrcDC.WindowOfs.Y);

  if NOT SrcDC.ctx.isFlipped then begin
    YSrc := dcBitmap.Height - (SrcHeight + YSrc);
  end;

  imageRep:= dcBitmap.ImageRep;
  if (Msk <> nil) and (Msk.Image <> nil) then begin
    maskImage := Msk.CreateMaskImage(Bounds(XMsk, YMsk, SrcWidth, SrcHeight));
    cgImage:= CGImageCreateWithMask(imageRep.CGImage, maskImage);
    imageRep:= NSBitmapImageRep.alloc.initWithCGImage(cgImage);
  end;

  Result := DrawImageRep( GetNSRect(X, Y, Width, Height),
                          GetNSRect(XSrc, YSrc, SrcWidth, SrcHeight),
                          imageRep );

  if Assigned(maskImage) then begin
    imageRep.release;
    CGImageRelease(cgImage);
    CGImageRelease(maskImage);
  end;

  AttachedBitmap_SetModified();
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
var
  s : NSString;
  AttribStr : CFAttributedStringRef;
  CoreLine : CTLineRef;
  height, width, asc, dsc, lead: CGFloat;
begin
  S := NSStringUtf8(AStr, ACount);

  Result := Assigned(S);
  if not Result then Exit;

  AttribStr := CFAttributedStringCreate(kCFAllocatorDefault, CFStringRef(S),
     CFDictionaryRef(NSDictionary.dictionaryWithObject_forKey(
        Font.Font, id(kCTFontAttributeName))));

  Result := Assigned(AttribStr);
  if Result then
  begin
    CoreLine := CTLineCreateWithAttributedString(CFAttributedStringRef(AttribStr));
    try
      width := CTLineGetTypographicBounds(CoreLine, @asc, @dsc, @lead);
      height := asc + abs(dsc) + lead;
      Size.cx := Round(width);
      Size.cy := Round(height);
    finally
      CFRelease(CoreLine);
      CFRelease(AttribStr);
    end;
  end;
  S.release;
end;

{------------------------------------------------------------------------------
  Method:  TCocoaContext.GetTextMetrics
  Params:  TM - The Record for the text metrics
  Returns: If the function succeeds

  Fills the specified buffer with the metrics for the currently selected font
 ------------------------------------------------------------------------------}
function TCocoaContext.GetTextMetrics(var TM: TTextMetric): Boolean;
var
  lNSFont: NSFont;
  AttrStr: CFAttributedStringRef;
  CoreLine: CTLineRef;
begin
  result := False;
  if not Assigned(Font) then
    exit;

  FillChar(TM, SizeOf(TM), 0);

  lNSFont := Font.Font;
  TM.tmAscent := Round(lNSFont.ascender);
  TM.tmDescent := -Round(lNSFont.descender);
  TM.tmHeight := TM.tmAscent + TM.tmDescent;

  TM.tmInternalLeading := Round(lNSFont.leading);
  TM.tmExternalLeading := 0;

  TM.tmMaxCharWidth := Round(lNSFont.maximumAdvancement.width);

  AttrStr := CFAttributedStringCreate(kCFAllocatorDefault,
               CFSTR('WMTigq[_|^'),
               CFDictionaryRef(NSDictionary.dictionaryWithObject_forKey(
                 Font.Font, id(kCTFontAttributeName))));
  try
    CoreLine := CTLineCreateWithAttributedString(CFAttributedStringRef(AttrStr));
    try
      TM.tmAveCharWidth := Round(CTLineGetTypographicBounds(CoreLine, nil, nil, nil) /
                                 CTLineGetGlyphCount(CoreLine));
    finally
      CFRelease(CoreLine);
    end;
  finally
    CFRelease(AttrStr);
  end;

  TM.tmOverhang := 0;
  TM.tmDigitizedAspectX := 0;
  TM.tmDigitizedAspectY := 0;
  TM.tmFirstChar := 'a';
  TM.tmLastChar := 'z';
  TM.tmDefaultChar := 'x';
  TM.tmBreakChar := '?';

  TM.tmWeight := Font.CocoaFontWeightToWin32FontWeight(NSFontManager.sharedFontManager.weightOfFont(Font.Font));

  if cfs_Bold in Font.Style then
    TM.tmWeight := Min(FW_BOLD, TM.tmWeight);

  if cfs_Italic in Font.Style then
    TM.tmItalic := 1;

  if cfs_Underline in Font.Style then
    TM.tmUnderlined := 1;

  if cfs_StrikeOut in Font.Style then
    TM.tmStruckOut := 1;

  TM.tmPitchAndFamily := TRUETYPE_FONTTYPE;
  if Font.Font.isFixedPitch then
    TM.tmPitchAndFamily := TM.tmPitchAndFamily or FIXED_PITCH;

  // we can take charset from Font.Charset also but leave it to default for now
  TM.tmCharSet := DEFAULT_CHARSET;

  Result := True;
end;

procedure TCocoaContext.DrawBitmap(X, Y: Integer; ABitmap: TCocoaBitmap);
begin
  NSGraphicsContext.classSaveGraphicsState();
  NSGraphicsContext.setCurrentContext(ctx);
  ABitmap.imagerep.drawAtPoint(NSMakePoint(X, Y));
  NSGraphicsContext.classRestoreGraphicsState();
  AttachedBitmap_SetModified();
end;

procedure TCocoaContext.DrawFocusRect(ARect: TRect);
var
  {$ifdef CocoaUseHITheme}
  AOutSet: SInt32;
  {$else}
  lCanvas: TCanvas;
  lDrawer: TCDDrawer;
  {$endif}
begin
  {$ifdef CocoaUseHITheme}
  // LCL thinks that focus cannot be drawn outside focus rects, but carbon do that
  // => correct rect
  GetThemeMetric(kThemeMetricFocusRectOutset, AOutSet);
  InflateRect(ARect, -AOutSet, -AOutSet);
  HIThemeDrawFocusRect(RectToCGRect(ARect), True, CGContext, kHIThemeOrientationNormal);
  {$else}
  lCanvas := TCanvas.Create;
  try
    lDrawer := GetDrawer(dsMacOSX);
    lCanvas.Handle := HDC(Self);
    lDrawer.DrawFocusRect(lCanvas, Types.Point(ARect.Left, ARect.Top), Types.Size(ARect));
  finally
    lCanvas.Handle := 0;
    lCanvas.Free;
  end;
  {$endif}
  AttachedBitmap_SetModified();
end;

{ TCocoaBitmapContext }

procedure TCocoaBitmapContext.SetBitmap(const AValue: TCocoaBitmap);
var pool:NSAutoReleasePool;
begin
  if Assigned(ctx) then
  begin
    ctx.release;
    ctx := nil;
  end;

  // ToDo: Should we free the old FBitmap???
  FBitmap := AValue;
  if FBitmap <> nil then
  begin
    pool:=NSAutoreleasePool.alloc.init;
    ctx := NSGraphicsContext.graphicsContextWithBitmapImageRep(Bitmap.ImageRep);
    ctx.retain; // extend life beyond NSAutoreleasePool
    InitDraw(Bitmap.Width, Bitmap.Height);
    pool.release;
  end;
end;

procedure TCocoaBitmapContext.AttachedBitmap_SetModified;
begin
  if FBitmap = nil then Exit;
  FBitmap.SetModified();
end;

constructor TCocoaBitmapContext.Create;
begin
  inherited Create(nil);
  FBitmap := DefaultBitmap;
end;

destructor TCocoaBitmapContext.Destroy;
begin
  inherited Destroy;
end;

function TCocoaBitmapContext.GetPixel(X,Y:integer): TColor;
var
  cg: CGContextRef;
  color: NSColor;
  R,G, B: Byte;
begin
  Result := 0;
  cg := CGContext;
  if not Assigned(cg) then Exit;

  color := FBitmap.Imagerep.colorAtX_Y(X, Y);
  R := Round(color.redComponent * $FF);
  G := Round(color.greenComponent * $FF);
  B := Round(color.blueComponent * $FF);
  Result := Graphics.RGBToColor(R, G, B);
end;

{ TCocoaRegion }

{------------------------------------------------------------------------------
  Method:  TCocoaRegion.Create

  Creates a new empty Cocoa region
 ------------------------------------------------------------------------------}
constructor TCocoaRegion.CreateDefault(AGlobal: Boolean = False);
begin
  inherited Create(AGlobal);

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
var
  DeviceShape: HIShapeRef;
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;
  DeviceShape := HIShapeCreateMutableCopy(Shape);
  try
    with ADC.GetLogicalOffset do
      HIShapeOffset(DeviceShape, -X, -Y);
    if HIShapeIsEmpty(DeviceShape) or (HIShapeReplacePathInCGContext(DeviceShape, ADC.CGContext) <> noErr) then
      Exit;
    CGContextClip(ADC.CGContext);
  finally
    CFRelease(DeviceShape);
  end;
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

function TCocoaRegion.CombineWith(ARegion: TCocoaRegion; CombineMode: TCocoaCombine): TCocoaRegionType;
var
  sh1, sh2: HIShapeRef;
const
  MinCoord=-35000;
  MaxSize=65000;
begin
  if not Assigned(ARegion) then
    Result := crt_Error
  else
  begin
    if (CombineMode in [cc_AND, cc_OR, cc_XOR]) and HIShapeIsEmpty(FShape) then
      CombineMode := cc_Copy;

    case CombineMode of
      cc_AND:
        begin
          Shape := HIShapeCreateIntersection(FShape, ARegion.Shape);
          Result := GetType;
        end;
      cc_XOR:
      begin
        sh1 := HIShapeCreateUnion(FShape, ARegion.Shape);
        sh2 := HIShapeCreateIntersection(FShape, ARegion.Shape);
        Shape := HIShapeCreateDifference(sh1, sh2);
        CFRelease(sh1);
        CFRelease(sh2);
        Result := GetType;
      end;
      cc_OR:
        begin
          Shape := HIShapeCreateUnion(FShape, ARegion.Shape);
          Result := GetType;
        end;
      cc_DIFF:
      begin
        if HIShapeIsEmpty(FShape) then
          {HIShapeCreateDifference doesn't work properly if original shape is empty}
          {to simulate "emptieness" very big shape is created }
          Shape := HIShapeCreateWithRect(GetCGRect(MinCoord, MinCoord, MaxSize, MaxSize)); // create clip nothing.

        Shape := HIShapeCreateDifference(FShape, ARegion.Shape);
        Result := GetType;
      end;
      cc_COPY:
        begin
          Shape := HIShapeCreateCopy(ARegion.Shape);
          Result := GetType;
        end
    else
      Result := crt_Error;
    end;
  end;
end;

procedure TCocoaRegion.Offset(dx, dy: Integer);
begin
  MakeMutable;
  HIShapeOffset(FShape, dx, dy);
end;

function TCocoaRegion.GetShapeCopy: HIShapeRef;
begin
  Result := HIShapeCreateCopy(Shape);
end;

procedure TCocoaRegion.MakeMutable;
begin
  Shape := HIShapeCreateMutableCopy(Shape);
end;

{ TCocoaPen }

procedure CalcDashes(
  const Src: array of CGFloat;
  var Dst: array of CGFloat;
  out Len: Integer;
  mul: CGFloat;
  ofs: CGFloat = 0.0 // pixels are "half" offset in Cocoa drawing
  );
var
  i: Integer;
begin
  Len := Min(length(Src), length(Dst));
  for i := 0 to Len - 1 do
    Dst[i] := Src[i] * mul + ofs;
end;

procedure TCocoaPen.Apply(ADC: TCocoaContext; UseROP2: Boolean = True);
var
  AR, AG, AB, AA: CGFloat;
  AROP2: Integer;
  ADashes: array [0..15] of CGFloat;
  ADashLen: Integer;
  StatDash: PCocoaStatDashes;
  isCosm  : Boolean;
  WidthMul : array [Boolean] of CGFloat;
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;

  if UseROP2 then
    AROP2 := ADC.ROP2
  else
    AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, AR, AG, AB, AA);

  case AROP2 of
    R2_NOT, R2_NOTXORPEN:
      CGContextSetBlendMode(ADC.CGContext, kCGBlendModeDifference);
  else
    CGContextSetBlendMode(ADC.CGContext, kCGBlendModeNormal)
  end;

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
    PS_DASH..PS_DASHDOTDOT:
      begin
        isCosm := not IsGeometric;
        WidthMul[false]:=1.0;
        WidthMul[true]:=Width;
        StatDash := @CocoaPenDash[isCosm][FStyle];
        CalcDashes( Slice(StatDash^.dash, StatDash^.len), ADashes, ADashLen, WidthMul[IsGeometric]);
        CGContextSetLineDash(ADC.CGContext, 0, @ADashes[0], ADashLen);
      end;
    PS_USERSTYLE:
      if Length(Dashes) > 0 then
        CGContextSetLineDash(ADC.CGContext, 0, @Dashes[0], Length(Dashes))
      else
        CGContextSetLineDash(ADC.CGContext, 0, nil, 0)
  else
    CGContextSetLineDash(ADC.CGContext, 0, nil, 0);
  end;
end;

constructor TCocoaPen.CreateDefault(const AGlobal: Boolean = False);
begin
  inherited Create(clBlack, True, AGlobal);
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

constructor TCocoaPen.Create(const AColor: TColor; AGlobal: Boolean);
begin
  inherited Create(AColor, True, AGlobal);
  FStyle := PS_SOLID;
  FWidth := 1;
  FIsExtPen := False;
  Dashes := nil;
end;

constructor TCocoaPen.Create(const AColor: TColor; AStyle: TFPPenStyle;
  ACosmetic: Boolean; AWidth: Integer; AMode: TFPPenMode; AEndCap: TFPPenEndCap;
  AJoinStyle: TFPPenJoinStyle; AGlobal: Boolean);
begin
  inherited Create(AColor, True, AGlobal);

  case AStyle of
    psSolid:       FStyle := PS_SOLID;
    psDash:        FStyle := PS_DASH;
    psDot:         FStyle := PS_DOT;
    psDashDot:     FStyle := PS_DASHDOT;
    psDashDotDot:  FStyle := PS_DASHDOTDOT;
    psinsideFrame: FStyle := PS_INSIDEFRAME;
    psPattern:     FStyle := PS_USERSTYLE;
    psClear:       FStyle := PS_NULL;
  end;

  if ACosmetic then
  begin
    FWidth := 1;
    FIsGeometric := False;
  end
  else
  begin
    FIsGeometric := True;

    case AJoinStyle of
      pjsRound: FJoinStyle := kCGLineJoinRound;
      pjsBevel: FJoinStyle := kCGLineJoinBevel;
      pjsMiter: FJoinStyle := kCGLineJoinMiter;
    end;

    case AEndCap of
      pecRound: FEndCap := kCGLineCapRound;
      pecSquare: FEndCap := kCGLineCapSquare;
      pecFlat: FEndCap := kCGLineCapButt;
    end;
    FWidth := Max(1, AWidth);
  end;
  FIsExtPen := False;
  Dashes := nil;
end;

{ TCocoaBrush }

procedure DrawBitmapPattern(info: UnivPtr; c: CGContextRef); MWPascal;
var
  R: CGRect;
  sR, sG, sB: single;
  APatternInfoPtr: PCocoaPatternInfo;
begin
  APatternInfoPtr := PCocoaPatternInfo(Info);
  R:= CGRectMake(0, 0, CGImageGetWidth(APatternInfoPtr^.image),
      CGImageGetHeight(APatternInfoPtr^.image));
  if APatternInfoPtr^.colorMode = cpmContextColor then
  begin
    ColorToRGBFloat(APatternInfoPtr^.bgColor, sR, sG, sB);
    CGContextSetRGBFillColor(c, sR, sG, sB, 1);
    CGContextFillRect(c, R);
    ColorToRGBFloat(APatternInfoPtr^.fgColor, sR, sG, sB);
    CGContextSetRGBFillColor(c, sR, sG, sB, 1);
  end;
  CGContextDrawImage(c, R, APatternInfoPtr^.image);
end;

procedure PatternReleaseInfo(info: UnivPtr ); MWPascal;
begin
  CGImageRelease(PCocoaPatternInfo(info)^.image);
  Freemem(info);
end;

{ TCocoaBrush }

procedure TCocoaBrush.CreateCGPattern(ARect: CGRect; IsColored: ShortInt);
var
  APatternInfoPtr: PCocoaPatternInfo;
  Callbacks: CGPatternCallbacks;
begin
  if FCGPattern <> nil then CGPatternRelease(FCGPattern);

  APatternInfoPtr := GetMem(sizeof(TCocoapatternInfo));
  APatternInfoPtr^.image := FImage;
  CGImageRetain(APatternInfoPtr^.image);
  APatternInfoPtr^.bgColor := RGBToColorFloat(Red/255, Green/255, Blue/255);
  APatternInfoPtr^.fgColor := FFgColor;
  APatternInfoPtr^.colorMode := FPatternColorMode;

  FillChar(CallBacks, SizeOf(CallBacks), 0);
  Callbacks.drawPattern := @DrawBitmapPattern;
  Callbacks.releaseInfo := @PatternReleaseInfo;

  FCGPattern := CGPatternCreate(APatternInfoPtr, ARect, CGAffineTransformIdentity,
    ARect.size.width, ARect.size.height, kCGPatternTilingConstantSpacing,
    IsColored, Callbacks);
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
  CGDataProvider: CGDataProviderRef;
begin
  if AHatch in [HS_HORIZONTAL..HS_DIAGCROSS] then
  begin
    if (FBitmap <> nil) then FBitmap.Release;
    FBitmap := TCocoaBitmap.Create(8, 8, 1, 1, cbaByte, cbtMask, @HATCH_DATA[AHatch]);
    if FImage <> nil then CGImageRelease(FImage);
    CGDataProvider := CGDataProviderCreateWithData(nil, @HATCH_DATA[AHatch], 8, nil);
    FImage := CGImageMaskCreate(8, 8, 1, 1, 1, CGDataProvider, nil, 0);
    CGDataProviderRelease(CGDataProvider);
    FPatternColorMode := cpmBrushColor;
    CreateCGPattern(GetCGRect(0 , 0, 8, 8), 0);
  end;
end;

procedure TCocoaBrush.SetBitmap(ABitmap: TCocoaBitmap);
var
  AWidth, AHeight: Integer;
  CGDataProvider: CGDataProviderRef;
begin
  AWidth := ABitmap.Width;
  AHeight := ABitmap.Height;
  if (FBitmap <> nil) then FBitmap.Release;
  FBitmap := TCocoaBitmap.Create(ABitmap);
  if FImage <> nil then CGImageRelease(FImage);
  if FBitmap.BitmapType = cbtMono then
  begin
    with FBitmap do
    begin
      CGDataProvider := CGDataProviderCreateWithData(nil, Data, DataSize, nil);
      FImage := CGImageMaskCreate(Width, Height, BitsPerSample, BitsPerPixel, BytesPerRow, CGDataProvider, nil, 0);
      CGDataProviderRelease(CGDataProvider);
    end;
    FPatternColorMode := cpmContextColor;
  end
  else
  begin
    FImage := CGImageCreateCopy(MacOSAll.CGImageRef( FBitmap.imageRep.CGImageForProposedRect_context_hints(nil, nil, nil)));
    FPatternColorMode := cpmBitmap;
  end;
  CreateCGPattern(GetCGRect(0, 0, AWidth, AHeight), Ord(FPatternColorMode = cpmBitmap));
end;

procedure TCocoaBrush.SetImage(AImage: NSImage);
var
  Rect: CGRect;
begin
  if FImage <> nil then CGImageRelease(FImage);
  FImage := CGImageCreateCopy(MacOSAll.CGImageRef( AImage.CGImageForProposedRect_context_hints(nil, nil, nil)));
  FPatternColorMode := cpmBitmap;
  Rect.origin.x := 0;
  Rect.origin.y := 0;
  Rect.size := CGSize(AImage.size);
  CreateCGPattern(Rect, 1);
end;

procedure TCocoaBrush.SetColor(AColor: NSColor);
var
  RGBColor, PatternColor: NSColor;
begin
  Clear;

  FColor := AColor;
  FColor.retain;

  RGBColor := AColor.colorUsingColorSpaceName(NSDeviceRGBColorSpace);

  if Assigned(RGBColor) then
    SetColor(NSColorToRGB(RGBColor), True)
  else
  begin
    PatternColor := AColor.colorUsingColorSpaceName(NSPatternColorSpace);
    if Assigned(PatternColor) then
    begin
      SetColor(NSColorToColorRef(PatternColor.patternImage.backgroundColor), False);
      SetImage(PatternColor.patternImage);
    end
    else
      SetColor(0, True);
  end;
end;

constructor TCocoaBrush.CreateDefault(const AGlobal: Boolean = False);
begin
  inherited Create(clWhite, True, AGlobal);
  FBitmap := nil;
  FImage := nil;
  FCGPattern := nil;
  FColor := nil;
end;

constructor TCocoaBrush.Create(const ALogBrush: TLogBrush; const AGlobal: Boolean = False);
begin
  FCGPattern := nil;
  FBitmap := nil;
  FImage := nil;
  FColor := nil;
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
  FColor := AColor;
  FColor.retain;

  FCGPattern := nil;
  FBitmap := nil;
  FImage := nil;
  RGBColor := AColor.colorUsingColorSpaceName(NSDeviceRGBColorSpace);
  if Assigned(RGBColor) then
    inherited Create(NSColorToRGB(RGBColor), True, AGlobal)
  else
  begin
    PatternColor := AColor.colorUsingColorSpaceName(NSPatternColorSpace);
    if Assigned(PatternColor) then
    begin
      inherited Create(NSColorToColorRef(PatternColor.patternImage.backgroundColor), False, AGlobal);
      SetImage(PatternColor.patternImage);
    end
    else
      inherited Create(0, True, AGlobal);
  end;
end;

constructor TCocoaBrush.Create(const AColor: TColor; AStyle: TFPBrushStyle; APattern: TBrushPattern;
  AGlobal: Boolean);
begin
  case AStyle of
  bsSolid:
  begin
    inherited Create(AColor, True, AGlobal);
  end;
  // bsHorizontal, bsVertical, bsFDiagonal,
  // bsBDiagonal, bsCross, bsDiagCross,
  // bsImage, bsPattern
  else // bsClear
    inherited Create(AColor, False, AGlobal);
  end;
end;

procedure TCocoaBrush.Clear;
begin
  if FColor <> nil then
  begin
    FColor.release;
    FColor := nil;
  end;

  if FCGPattern <> nil then
  begin
    CGPatternRelease(FCGPattern);
    FCGPattern := nil;
  end;

  if FBitmap <> nil then
  begin
    FBitmap.Release;
    FBitmap := nil;
  end;

  if FImage <> nil then
  begin
    CGImageRelease(FImage);
    FImage := nil;
  end;
end;

destructor TCocoaBrush.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCocoaBrush.Apply(ADC: TCocoaContext; UseROP2: Boolean = True);
var
  RGBA: array[0..3] of CGFloat;
  AROP2: Integer;
  APatternSpace: CGColorSpaceRef;
  BaseSpace: CGColorSpaceRef;
  sR, sG, sB: single;
  sz: CGSize;
  offset: TPoint;
begin
  if ADC = nil then Exit;

  if ADC.CGContext = nil then
    Exit;

  if UseROP2 then
    AROP2 := ADC.ROP2
  else
    AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, RGBA[0], RGBA[1], RGBA[2], RGBA[3]);

  //if AROP2 <> R2_NOT then
    //CGContextSetBlendMode(ADC.CGContext, kCGBlendModeNormal)
  //else
    //CGContextSetBlendMode(ADC.CGContext, kCGBlendModeDifference);

  if Assigned(FCGPattern) then
  begin
    // Set proper pattern alignment
    offset:=ADC.GetLogicalOffset;
    with CGPointApplyAffineTransform(CGPointMake(0,0), CGContextGetCTM(ADC.CGContext)) do
    begin
      sz.width:=x - offset.X;
      sz.height:=y + offset.Y;
      sz.width:=Round(sz.width) mod CGImageGetWidth(FImage);
      sz.height:=Round(sz.height) mod CGImageGetHeight(FImage);
    end;
    CGContextSetPatternPhase(ADC.CGContext, sz);

    case FPatternColorMode of
      cpmBitmap:
        begin
          BaseSpace := nil;
          RGBA[0] := 1.0;
        end;
      cpmBrushColor:
        begin
          BaseSpace := CGColorSpaceCreateDeviceRGB;
        end;
      cpmContextColor:
        begin
          BaseSpace := CGColorSpaceCreateDeviceRGB;
          SetColor(ADC.BkColor, True);
          FFgColor:=ColorToRGB(ADC.TextColor);
          ColorToRGBFloat(FFgColor, sR, sG, sB);
          RGBA[0]:=sR;
          RGBA[1]:=sG;
          RGBA[2]:=sB;
          RGBA[3]:=1.0;
        end;
    end;
    APatternSpace := CGColorSpaceCreatePattern(BaseSpace);
    CGContextSetFillColorSpace(ADC.CGContext, APatternSpace);
    CGColorSpaceRelease(APatternSpace);
    if Assigned(BaseSpace) then CGColorSpaceRelease(BaseSpace);
    CGContextSetFillPattern(ADC.CGcontext, FCGPattern, @RGBA[0]);
  end
  else
  begin
    CGContextSetRGBFillColor(ADC.CGContext, RGBA[0], RGBA[1], RGBA[2], RGBA[3]);
  end;
end;

procedure TCocoaBrush.ApplyAsPenColor(ADC: TCocoaContext; UseROP2: Boolean);
var
  AR, AG, AB, AA: CGFloat;
  AROP2: Integer;
  ADashes: array [0..15] of CGFloat;
  ADashLen: Integer;
  StatDash: PCocoaStatDashes;
  isCosm  : Boolean;
  WidthMul : array [Boolean] of CGFloat;
begin
  if ADC = nil then Exit;
  if ADC.CGContext = nil then Exit;

  if UseROP2 then
    AROP2 := ADC.ROP2
  else
    AROP2 := R2_COPYPEN;

  GetRGBA(AROP2, AR, AG, AB, AA);

  CGContextSetRGBStrokeColor(ADC.CGContext, AR, AG, AB, AA);
end;

{ TCocoaGDIObject }

constructor TCocoaGDIObject.Create(AGlobal: Boolean);
begin
  FRefCount := 1;
  FGlobal := AGlobal;
end;

destructor TCocoaGDIObject.Destroy;
begin
  if not FGlobal then
  begin
    Dec(FRefCount);
    if FRefCount <> 0 then
    begin
      //DebugLn('TCocoaGDIObject.Destroy Error - ', dbgsName(self), ' RefCount = ', dbgs(FRefCount));
      FRefCount := FRefCount;
    end;
  end;
end;

class function TCocoaGDIObject.UpdateRefs(ATarget: TCocoaGDIObject; ASource: TCocoaGDIObject): Boolean; static;
begin
  result := ASource <> ATarget;
  if result then
  begin
    if Assigned(ASource) then
      ASource.AddRef;
    if Assigned(ATarget) then
      ATarget.Release;
  end;
end;

procedure TCocoaGDIObject.AddRef;
begin
  if FGlobal then Exit;
  inc(FRefCount);
end;

procedure TCocoaGDIObject.Release;
begin
  if FGlobal then Exit;
  if FRefCount <= 1 then
    self.Free                     // the last reference, so free it using the destructor
  else
    Dec(FRefCount);
end;

initialization


finalization


end.
