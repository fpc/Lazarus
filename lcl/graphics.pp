{  $Id$  }
{
 /***************************************************************************
                                graphics.pp
                                -----------
                             Graphic Controls
                   Initial Revision  : Mon Jul 26 0:02:58 1999

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Graphics;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  GraphType, SysUtils, Classes, vclGlobals, LMessages, LCLType, LCLLinux,
  LResources;


const
  // The follow colors match the predefined Delphi Colors
  clBlack =   TColor($000000);
  clMaroon =  TColor($000080);
  clGreen =   TColor($008000);
  clOlive =   TColor($008080);
  clNavy =    TColor($800000);
  clPurple =  TColor($800080);
  clTeal =    TColor($808000);
  clGray =    TColor($808080);
  clSilver =  TColor($C0C0C0);
  clRed =     TColor($0000FF);
  clLime =    TColor($00FF00);
  clYellow =  TColor($00FFFF);
  clBlue =    TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua =    TColor($FFFF00);
  clLtGray =  TColor($C0C0C0);
  clDkGray =  TColor($808080);
  clWhite  =  TColor($FFFFFF);
  clNone   =  TColor($1FFFFFFF);
  clDefault = TColor($20000000);
  
  //System colors
  // TODO : kick these out into platform specific units!
  clScrollBar               = TColor(SYS_COLOR_BASE or COLOR_SCROLLBAR);
  clBackground              = TColor(SYS_COLOR_BASE or COLOR_BACKGROUND);
  clActiveCaption           = TColor(SYS_COLOR_BASE or COLOR_ACTIVECAPTION);
  clInactiveCaption         = TColor(SYS_COLOR_BASE or COLOR_INACTIVECAPTION);
  clMenu                    = TColor(SYS_COLOR_BASE or COLOR_MENU);
  clWindow                  = TColor(SYS_COLOR_BASE or COLOR_WINDOW);
  clWindowFrame             = TColor(SYS_COLOR_BASE or COLOR_WINDOWFRAME);
  clMenuText                = TColor(SYS_COLOR_BASE or COLOR_MENUTEXT);
  clWindowText              = TColor(SYS_COLOR_BASE or COLOR_WINDOWTEXT);
  clCaptionText             = TColor(SYS_COLOR_BASE or COLOR_CAPTIONTEXT);
  clActiveBorder            = TColor(SYS_COLOR_BASE or COLOR_ACTIVEBORDER);
  clInactiveBorder          = TColor(SYS_COLOR_BASE or COLOR_INACTIVEBORDER);
  clAppWorkspace            = TColor(SYS_COLOR_BASE or COLOR_APPWORKSPACE);
  clHighlight               = TColor(SYS_COLOR_BASE or COLOR_HIGHLIGHT);
  clHighlightText           = TColor(SYS_COLOR_BASE or COLOR_HIGHLIGHTTEXT);
  clBtnFace                 = TColor(SYS_COLOR_BASE or COLOR_BTNFACE);
  clBtnShadow               = TColor(SYS_COLOR_BASE or COLOR_BTNSHADOW);
  clGrayText                = TColor(SYS_COLOR_BASE or COLOR_GRAYTEXT);
  clBtnText                 = TColor(SYS_COLOR_BASE or COLOR_BTNTEXT);
  clInactiveCaptionText     = TColor(SYS_COLOR_BASE or COLOR_INACTIVECAPTIONTEXT);
  clBtnHighlight            = TColor(SYS_COLOR_BASE or COLOR_BTNHIGHLIGHT);
  cl3DDkShadow              = TColor(SYS_COLOR_BASE or COLOR_3DDKSHADOW);
  cl3DLight                 = TColor(SYS_COLOR_BASE or COLOR_3DLIGHT);
  clInfoText                = TColor(SYS_COLOR_BASE or COLOR_INFOTEXT);
  clInfoBk                  = TColor(SYS_COLOR_BASE or COLOR_INFOBK);

  clHotLight                = TColor(SYS_COLOR_BASE or COLOR_HOTLIGHT);
  clGradientActiveCaption   = TColor(SYS_COLOR_BASE or COLOR_GRADIENTACTIVECAPTION);
  clGradientInactiveCaption = TColor(SYS_COLOR_BASE or COLOR_GRADIENTINACTIVECAPTION);
  clEndColors               = TColor(SYS_COLOR_BASE or COLOR_ENDCOLORS);
  clColorDesktop            = TColor(SYS_COLOR_BASE or COLOR_DESKTOP);
  cl3DFace                  = TColor(SYS_COLOR_BASE or COLOR_3DFACE);
  cl3DShadow                = TColor(SYS_COLOR_BASE or COLOR_3DSHADOW);
  cl3DHILight               = TColor(SYS_COLOR_BASE or COLOR_3DHIGHLIGHT);
  clBtnHILight              = TColor(SYS_COLOR_BASE or COLOR_BTNHILIGHT);

const
  cmBlackness = BLACKNESS;
  cmDstInvert = DSTINVERT;
  cmMergeCopy = MERGECOPY;
  cmMergePaint = MERGEPAINT;
  cmNotSrcCopy = NOTSRCCOPY;
  cmNotSrcErase = NOTSRCERASE;
  cmPatCopy = PATCOPY;
  cmPatInvert = PATINVERT;
  cmPatPaint = PATPAINT;
  cmSrcAnd = SRCAND;
  cmSrcCopy = SRCCOPY;
  cmSrcErase = SRCERASE;
  cmSrcInvert = SRCINVERT;
  cmSrcPaint = SRCPAINT;
  cmWhiteness = WHITENESS;


type
  TBitmap = class;
  TPixmap = class;
  TIcon = class;
  
  { TGraphicsObject }

  TGraphicsObject = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    Procedure DoChange(var msg); message LM_CHANGED;
  protected
    procedure Changed; dynamic;
    Procedure Lock;
    Procedure UnLock;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  { TFont }
  
  TFont = class(TGraphicsObject)
  private
    FColor : TColor;
    // Extra properties
    // TODO: implement them though GetTextMetrics, not here
    //FWidth : Integer;
    //FXBias : Integer;
    //FYBias : Integer;
    //---------
    FFontData: TFontData;
    FPixelsPerInch: Integer;
    FFontName: string;
    FUpdateCount: integer;
    FChanged: boolean;
    procedure FreeHandle;
    procedure GetData(var FontData: TFontData);
    procedure SetData(const FontData: TFontData);
  protected
    procedure Changed; override;
    function  GetCharSet: TFontCharSet;
    function  GetHandle: HFONT;
    function  GetHeight: Integer;
    function  GetName : TFontName;
    function  GetPitch: TFontPitch;
    function  GetSize : Integer;
    function  GetStyle: TFontStyles;
    procedure SetCharSet(const AValue: TFontCharSet);
    procedure SetColor(Value : TColor);
    procedure SetHandle(const Value: HFONT);
    procedure SetHeight(value : Integer);
    procedure SetName(const AValue : TFontName);
    procedure SetPitch(Value : TFontPitch);
    procedure SetSize(value : Integer);
    procedure SetStyle(Value: TFontStyles);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure Assign(const ALogFont: TLogFont);
    procedure BeginUpdate;
    procedure EndUpdate;
    function HandleAllocated: boolean;
    // Extra properties
    // TODO: implement them though GetTextMetrics, not here
    //Function GetWidth(Value : String) : Integer;
    // Extra properties
    // TODO: implement them though GetTextMetrics, not here
    //property Width : Integer read FWidth write FWidth;
    //property XBias : Integer read FXBias write FXBias;
    //property YBias : Integer read FYBias write FYBias;
    //-----------------
    property Handle : HFONT read GetHandle write SetHandle;
    property PixelsPerInch : Integer read FPixelsPerInch;
  published
    property CharSet: TFontCharSet read GetCharSet write SetCharSet;
    property Color : TColor read FColor write SetColor;
    property Height : Integer read GetHeight write SetHeight;
    property Name : TFontName read GetName write SetName;
    property Pitch: TFontPitch read GetPitch write SetPitch;
    property Size: Integer read GetSize write SetSize;
    property Style : TFontStyles read GetStyle write SetStyle;
  end;


  { TPen }

  TPen = class(TGraphicsObject)
  private
    FPenData : TPenData;
    FMode : TPenMode;
    procedure FreeHandle;
  protected
    function GetHandle: HPEN;
    procedure SetHandle(const Value: HPEN);
    procedure SetColor(Value : TColor);
    procedure SetMode(Value : TPenMode);
    procedure SetStyle(Value : TPenStyle);
    procedure Setwidth(value : Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Handle : HPEN read GetHandle write SetHandle;
  published
    property Color: TColor read FPenData.Color write SetColor;
    property Mode: TPenMode read FMode write SetMode;
    property Style: TPenStyle read FPenData.Style write SetStyle;
    property Width: Integer read FPenData.Width write SetWidth;
  end;


  { TBrush }

  TBrushData = record
    Handle : HBrush;
    Color : TColor;
    Bitmap : TBitmap;
    Style : TBrushStyle;
  end;

  TBrush = class(TgraphicsObject)
  private
    FBrushData : TBrushData;
//    Procedure Getdata(var BrushData: TBrushData);
//    Procedure SetData(const Brushdata: TBrushdata);
    procedure FreeHandle;
  protected
    function GetHandle: HBRUSH;
    Procedure SetBitmap(Value : TBitmap);
    Procedure SetColor(Value : TColor);
    procedure SetHandle(const Value: HBRUSH);
    Procedure SetStyle(value : TBrushStyle);
  public
    procedure Assign(Source : Tpersistent); override;
    constructor Create;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBrushData.Bitmap write SetBitmap;
    property Handle: HBRUSH read GetHandle write SetHandle;
  published
    property Color : TColor read FBrushData.Color write SetColor ;
    property Style: TBrushStyle read FBrushData.Style write SetStyle;
  end;

  TCanvas = class;

  { The TGraphic class is an abstract base class for dealing with graphic images
    such as bitmaps, pixmaps, icons, and other image formats.
      LoadFromFile - Read the graphic from the file system.  The old contents of
        the graphic are lost.  If the file is not of the right format, an
        exception will be generated.
      SaveToFile - Writes the graphic to disk in the file provided.
      LoadFromStream - Like LoadFromFile except source is a stream (e.g.
        TBlobStream).
      SaveToStream - stream analogue of SaveToFile.
      LoadFromClipboardFormat - Replaces the current image with the data
        provided.  If the TGraphic does not support that format it will generate
        an exception.
      SaveToClipboardFormats - Converts the image to a clipboard format.  If the
        image does not support being translated into a clipboard format it
        will generate an exception.
      Height - The native, unstretched, height of the graphic.
      Palette - Color palette of image.  Zero if graphic doesn't need/use palettes.
      Transparent - Image does not completely cover its rectangular area
      Width - The native, unstretched, width of the graphic.
      OnChange - Called whenever the graphic changes
      PaletteModified - Indicates in OnChange whether color palette has changed.
        Stays true until whoever's responsible for realizing this new palette
        (ex: TImage) sets it to False.
      OnProgress - Generic progress indicator event. Propagates out to TPicture
        and TImage OnProgress events.}
  TGraphic = class(TPersistent)
  private
    FModified: Boolean;
    FTransparent: Boolean;
    FOnChange: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FPaletteModified: Boolean;
    procedure SetModified(Value: Boolean);
  protected
    procedure Changed(Sender: TObject); virtual;
    function Equals(Graphic: TGraphic): Boolean; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
    function GetEmpty: Boolean; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetTransparent: Boolean; virtual;
    function GetWidth: Integer; virtual; abstract;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
      const Msg: string); dynamic;
    procedure ReadData(Stream: TStream); virtual;
    procedure SetHeight(Value: Integer); virtual; abstract;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual; abstract;
    procedure WriteData(Stream: TStream); virtual;
  public
    procedure LoadFromFile(const Filename: string); virtual;
    procedure SaveToFile(const Filename: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromLazarusResource(const ResName: String); virtual; abstract;
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat); virtual; abstract;
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat); virtual; abstract;
    constructor Create; virtual;
    property Empty: Boolean read GetEmpty;
    property Height: Integer read GetHeight write SetHeight;
    property Modified: Boolean read FModified write SetModified;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Width: Integer read GetWidth write SetWidth;
  end;

  TGraphicClass = class of TGraphic;

  { TPicture }
  { TPicture is a TGraphic container.  It is used in place of a TGraphic if the
    graphic can be of any TGraphic class.  LoadFromFile and SaveToFile are
    polymorphic. For example, if the TPicture is holding an Icon, you can
    LoadFromFile a bitmap file, where if the class is TIcon you could only read
    .ICO files.
    
      LoadFromFile - Reads a picture from disk. The TGraphic class created
        determined by the file extension of the file. If the file extension is
        not recognized an exception is generated.
      SaveToFile - Writes the picture to disk.
      LoadFromClipboardFormat - ToDo: Reads the picture from the handle provided in
        the given clipboard format.  If the format is not supported, an
        exception is generated.
      SaveToClipboardFormats - ToDo: Allocates a global handle and writes the picture
        in its native clipboard format (CF_BITMAP for bitmaps, CF_METAFILE
        for metafiles, etc.).  Formats will contain the formats written.
        Returns the number of clipboard items written to the array pointed to
        by Formats and Datas or would be written if either Formats or Datas are
        nil.
      SupportsClipboardFormat - Returns true if the given clipboard format
        is supported by LoadFromClipboardFormat.
      Assign - Copys the contents of the given TPicture.  Used most often in
        the implementation of TPicture properties.
      RegisterFileFormat - Register a new TGraphic class for use in
        LoadFromFile.
      RegisterClipboardFormat - Registers a new TGraphic class for use in
        LoadFromClipboardFormat.
      UnRegisterGraphicClass - Removes all references to the specified TGraphic
        class and all its descendents from the file format and clipboard format
        internal lists.
      Height - The native, unstretched, height of the picture.
      Width - The native, unstretched, width of the picture.
      Graphic - The TGraphic object contained by the TPicture
      Bitmap - Returns a bitmap.  If the contents is not already a bitmap, the
        contents are thrown away and a blank bitmap is returned.
      Icon - Returns an icon.  If the contents is not already an icon, the
        contents are thrown away and a blank icon is returned.
      Pixmap - Returns a pixmap.  If the contents is not already a pixmap, the
        contents are thrown away and a blank pixmap is returned.
      }

  TPicture = class(TPersistent)
  private
    FGraphic: TGraphic;
    FOnChange: TNotifyEvent;
    //FNotify: IChangeNotifier;
    FOnProgress: TProgressEvent;
    procedure ForceType(GraphicType: TGraphicClass);
    function GetBitmap: TBitmap;
    function GetPixmap: TPixmap;
    function GetIcon: TIcon;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure ReadData(Stream: TStream);
    procedure SetBitmap(Value: TBitmap);
    procedure SetPixmap(Value: TPixmap);
    procedure SetIcon(Value: TIcon);
    procedure SetGraphic(Value: TGraphic);
    procedure WriteData(Stream: TStream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed(Sender: TObject); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
                       PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
                       const Msg: string); dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat);
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat);
    class function SupportsClipboardFormat(FormatID: TClipboardFormat): Boolean;
    procedure Assign(Source: TPersistent); override;
    class procedure RegisterFileFormat(const AnExtension, ADescription: string;
      AGraphicClass: TGraphicClass);
    class procedure RegisterClipboardFormat(FormatID: TClipboardFormat;
      AGraphicClass: TGraphicClass);
    class procedure UnregisterGraphicClass(AClass: TGraphicClass);
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Pixmap: TPixmap read GetPixmap write SetPixmap;
    property Icon: TIcon read GetIcon write SetIcon;
    property Graphic: TGraphic read FGraphic write SetGraphic;
    //property PictureAdapter: IChangeNotifier read FNotify write FNotify;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;


  EInvalidGraphic = class(Exception);


  { TCanvas }

  TCanvas = class(TPersistent)
  private
    FAutoReDraw : Boolean;
    FState: TCanvasState;
    FFont : TFont;
    FSavedFontHandle: HFont;
    FPen: TPen;
    FSavedPenHandle: HPen;
    FBrush: TBrush;
    FSavedBrushHandle: HBrush;
    FPenPos : TPoint;
    FCopyMode : TCopyMode;
    FHandle : HDC;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FTextStyle : TTextStyle;
    FLock  :TCriticalSection;
    FLockCount: Integer;
    procedure BrushChanged(ABrush: TObject);
    procedure FontChanged(AFont: TObject);
    procedure CreateBrush;
    procedure CreateFont;
    Procedure CreatePen;
    procedure DeselectHandles;
    function GetCanvasClipRect: TRect;
    Function GetColor: TColor;
    function GetHandle : HDC;
    Function GetPenPos: TPoint;
    Function GetPixel(X,Y : Integer) : TColor;
    procedure PenChanged(APen: TObject);
    Procedure SetAutoReDraw(Value : Boolean);
    Procedure SetColor(c: TColor);
    Procedure SetBrush(value : TBrush);
    Procedure SetFont(value : TFont);
    procedure SetHandle(NewHandle: HDC);
    Procedure SetPen(value : TPen);
    Procedure SetPenPos(Value : TPoint);
    Procedure SetPixel(X,Y : Integer; Value : TColor);
  protected
    procedure CreateHandle; virtual;
    procedure RequiredState(ReqState: TCanvasState);
  public
    procedure Lock;
    procedure Unlock;

    procedure Arc(x,y,width,height,angle1,angle2 : Integer);
    procedure Arc(x,y,width,height,SX,SY,EX,EY : Integer);
    Procedure BrushCopy(Dest : TRect; InternalImages: TBitmap; Src : TRect;
                        TransparentColor :TColor);
    constructor Create;
    procedure Chord(x,y,width,height,angle1,angle2 : Integer);
    procedure Chord(x,y,width,height,SX,SY,EX,EY : Integer);
    Procedure CopyRect(const Dest : TRect; Canvas : TCanvas; const Source : TRect);
    destructor Destroy; override;
    Procedure Draw(X,Y: Integer; Graphic : TGraphic);
    procedure Ellipse(x1, y1, x2, y2: Integer);
    procedure Ellipse(const Rect: TRect);
    procedure Pie(x,y,width,height,angle1,angle2 : Integer);
    procedure Pie(x,y,width,height,SX,SY,EX,EY : Integer);
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
                         Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean{$IFDEF VER1_1} = False{$ENDIF};
                         Continuous: boolean{$IFDEF VER1_1} = False{$ENDIF});
    procedure PolyBezier(const Points: array of TPoint);
    procedure Polygon(const Points: array of TPoint;
                      Winding: Boolean{$IFDEF VER1_1} = False{$ENDIF};
                      StartIndex: Integer{$IFDEF VER1_1} = 0{$ENDIF};
                      NumPts: Integer {$IFDEF VER1_1} = -1{$ENDIF});
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean{$IFDEF VER1_1} = False{$ENDIF});
    Procedure Polygon(const Points: array of TPoint);
    procedure Polyline(const Points: array of TPoint;
                       StartIndex: Integer {$IFDEF VER1_1} = 0{$ENDIF};
                       NumPts: Integer {$IFDEF VER1_1} = -1{$ENDIF});
    procedure Polyline(Points: PPoint; NumPts: Integer);
    procedure Polyline(const Points: array of TPoint);
    Procedure FillRect(const Rect : TRect);
    procedure FloodFill(X, Y: Integer; FillColor: TColor; FillStyle: TFillStyle);
    procedure Frame3d(var Rect : TRect; const FrameWidth : integer;
                      const Style : TBevelCut);
    Procedure Rectangle(X1,Y1,X2,Y2 : Integer);
    Procedure Rectangle(const Rect: TRect); 
    Procedure Line(X1,Y1,X2,Y2 : Integer);
    Procedure MoveTo(X1,Y1 : Integer);
    Procedure LineTo(X1,Y1 : Integer);
    procedure TextOut(X,Y: Integer; const Text: String);
    procedure TextRect(Rect: TRect; X, Y: integer; const Text : string);
    procedure TextRect(Rect: TRect; X, Y: integer; const Text : string;
                       const Style : TTextStyle);
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    function TextWidth(const Text: string): Integer;
    function HandleAllocated: boolean;
    property ClipRect: TRect read GetCanvasClipRect;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property Pixels[X, Y: Integer]: TCOlor read GetPixel write SetPixel;
    property Handle: HDC read GetHandle write SetHandle;
    property TextStyle : TTextStyle read FTextStyle write FTextStyle;
    property LockCount:Integer read FLockCount;
  published
    property AutoRedraw : Boolean read FAutoReDraw write SetAutoReDraw;
    property Brush: TBrush read FBrush write SetBrush;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode default cmSrcCopy;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    // Extra
    property Color: TColor read GetColor write SetColor;
  end;


  { TBITMAP }

 TSharedImage = class
  private
    FRefCount: Integer;
  protected
    procedure Reference; // increase reference count
    procedure Release;   // decrease reference count
    procedure FreeHandle; virtual; abstract;
    property RefCount: Integer read FRefCount;
  end;

 TBitmapImage = class(TSharedImage)
  private
    FHandle: HBITMAP;
    FMaskHandle: HBITMAP;
    FPalette: HPALETTE;
    FDIBHandle: HBITMAP;
{    FDIB: TDIBSection;
    FOS2Format: Boolean;
    FHalftone: Boolean;
}
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

  TBitmap = class(TGraphic)
  private
    FCanvas: TCanvas;
    FImage : TBitmapImage;
    FMonochrome: Boolean;
    FPalette: HPALETTE;
    FPixelFormat: TPixelFormat;
    FTransparentColor: TColor;
    FHeight: integer;
    FWidth: integer;
    Procedure FreeContext;
    Procedure NewImage(NHandle: HBITMAP; NPallette: HPALETTE;
       const NDIB : TDIBSection; OS2Format : Boolean);
    procedure SetHandle(Value: HBITMAP);
    procedure SetMaskHandle(Value: HBITMAP);
    function GetHandle: HBITMAP; virtual;
    function GetMaskHandle: HBITMAP; virtual;
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure HandleNeeded;
    procedure MaskHandleNeeded;
    procedure PaletteNeeded;
    procedure ReadData(Stream: TStream); override;
    procedure ReadStream(Stream: TStream; Size: Longint); virtual;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure WriteData(Stream: TStream); override;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure FreeImage;
    function HandleAllocated: boolean;
    property Handle: HBITMAP read GetHandle write SetHandle;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromLazarusResource(const ResName: String); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); virtual;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); virtual;
    Procedure LoadFromXPMFile(Filename : String);
    procedure Mask(ATransparentColor: TColor);
    procedure SaveToStream(Stream: TStream); override;
    Function ReleaseHandle : HBITMAP;
    property Canvas : TCanvas read FCanvas write FCanvas;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property Monochrome: Boolean read FMonochrome write FMonochrome;
    // TODO: reflect real pixelformat of DC
    property PixelFormat: TPixelFormat read FPixelFormat write FPixelFormat;
    property TransparentColor: TColor read FTransparentColor write FTransparentColor;
  end;


  { TPixmap }
  {
    @abstract()
    Introduced by Marc Weustink <weus@quicknet.nl>
    Currently maintained by ?
  }
  TPixmap = class(TBitmap)
  protected
    procedure ReadStream(Stream: TStream; Size: Longint); override;
  public
    procedure LoadFromLazarusResource(const ResName: String); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); override;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); override;
  end;
  

  { TIcon }
  {
    @abstract()
    Introduced by Marc Weustink <weus@quicknet.nl>
    Currently maintained by ?
  }
  {
    TIcon reads and writes .ICO file format.
    ! Currently it is only a TPixmap, but eventually it will become a TBitmap
    descendent. !
  }
  TIcon = class(TPixmap)
  end;


  // Color / Identifier mapping
  TGetColorStringProc = procedure(const s:ansistring) of object;
  
function ColorToIdent(Color: Longint; var Ident: String): Boolean;
function IdentToColor(const Ident: string; var Color: Longint): Boolean;
function ColorToRGB(Color: TColor): Longint;
function ColorToString(Color: TColor): AnsiString;
function StringToColor(const S: shortstring): TColor;
procedure GetColorValues(Proc: TGetColorStringProc);

procedure GetCharsetValues(Proc: TGetStrProc);
function CharsetToIdent(Charset: Longint; var Ident: string): Boolean;
function IdentToCharset(const Ident: string; var Charset: Longint): Boolean;

function GetDefFontCharSet: TFontCharSet;
function IsFontNameXLogicalFontDesc(const LongFontName: string): boolean;
function XLFDNameToLogFont(const XLFDName: string): TLogFont;
function ExtractXLFDItem(const XLFDName: string; Index: integer): string;
function ExtractFamilyFromXLFDName(const XLFDName: string): string;
function ClearXLFDItem(const LongFontName: string; Index: integer): string;
function ClearXLFDHeight(const LongFontName: string): string;
function ClearXLFDPitch(const LongFontName: string): string;
function ClearXLFDStyle(const LongFontName: string): string;

function XPMToPPChar(const XPM: string): PPChar;
function LazResourceXPMToPPChar(const ResourceName: string): PPChar;
function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;
function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer
  ): boolean;

var 
  { Stores information about the current screen }
  ScreenInfo : TLMScreenInit;

const  // New TFont instances are initialized with the values in this structure:
  DefFontData: TFontData = (
    Handle: 0;
    Height: 0;
    Pitch: fpDefault;
    Style: [];
    Charset : DEFAULT_CHARSET;
    Name: 'default');


(***************************************************************************
 ***************************************************************************)
implementation


uses
  Controls, ClipBrd, TypInfo;

const
  GraphicsFinalized: boolean = false;

type
  TBitmapCanvas = class(TCanvas)
  private
    FBitmap : TBitmap;
    FOldBitmap : HBitmap;
    FOldPalette : HPALETTE;
    procedure FreeDC;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(ABitmap : TBitmap);
    destructor Destroy; override;
    // TODO: replace this by property BitmapHandle;
    // MWE: Not needed
    //property Bitmap: TBitmap read FBitmap;
  end;


{ Color mapping routines }

const
  Colors: array[0..41] of TIdentMapEntry = (
    (Value: clBlack; Name: 'clBlack'),
    (Value: clMaroon; Name: 'clMaroon'),
    (Value: clGreen; Name: 'clGreen'),
    (Value: clOlive; Name: 'clOlive'),
    (Value: clNavy; Name: 'clNavy'),
    (Value: clPurple; Name: 'clPurple'),
    (Value: clTeal; Name: 'clTeal'),
    (Value: clGray; Name: 'clGray'),
    (Value: clSilver; Name: 'clSilver'),
    (Value: clRed; Name: 'clRed'),
    (Value: clLime; Name: 'clLime'),
    (Value: clYellow; Name: 'clYellow'),
    (Value: clBlue; Name: 'clBlue'),
    (Value: clFuchsia; Name: 'clFuchsia'),
    (Value: clAqua; Name: 'clAqua'),
    (Value: clWhite; Name: 'clWhite'),
    (Value: clScrollBar; Name: 'clScrollBar'),
    (Value: clBackground; Name: 'clBackground'),
    (Value: clActiveCaption; Name: 'clActiveCaption'),
    (Value: clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: clMenu; Name: 'clMenu'),
    (Value: clWindow; Name: 'clWindow'),
    (Value: clWindowFrame; Name: 'clWindowFrame'),
    (Value: clMenuText; Name: 'clMenuText'),
    (Value: clWindowText; Name: 'clWindowText'),
    (Value: clCaptionText; Name: 'clCaptionText'),
    (Value: clActiveBorder; Name: 'clActiveBorder'),
    (Value: clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: clAppWorkSpace; Name: 'clAppWorkSpace'),
    (Value: clHighlight; Name: 'clHighlight'),
    (Value: clHighlightText; Name: 'clHighlightText'),
    (Value: clBtnFace; Name: 'clBtnFace'),
    (Value: clBtnShadow; Name: 'clBtnShadow'),
    (Value: clGrayText; Name: 'clGrayText'),
    (Value: clBtnText; Name: 'clBtnText'),
    (Value: clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: cl3DLight; Name: 'cl3DLight'),
    (Value: clInfoText; Name: 'clInfoText'),
    (Value: clInfoBk; Name: 'clInfoBk'),
    (Value: clNone; Name: 'clNone'));

function ColorToIdent(Color: Longint; var Ident: String): Boolean;
begin
  Result := IntToIdent(Color, Ident, Colors);
end;

function IdentToColor(const Ident: string; var Color: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Color, Colors);
end;

function ColorToRGB(Color: TColor): Longint;
begin
  if (Cardinal(Color) and Cardinal(SYS_COLOR_BASE)) <> 0
  then Result := GetSysColor(Color and $000000FF)
  else Result := Color;
  Result := Result and $FFFFFF;
end;

function ColorToString(Color: TColor): AnsiString;
begin
  if not ColorToIdent(Color, Result) then
    Result:='$'+HexStr(Color,8);
end;

function StringToColor(const S: shortstring): TColor;
begin
  if not IdentToColor(S, Longint(Result)) then
    Result := TColor(StrToInt(S));
end;

procedure GetColorValues(Proc: TGetColorStringProc);
var
  I: Integer;
begin
  for I := Low(Colors) to High(Colors) do Proc(Colors[I].Name);
end;



{$I graphicsobject.inc}
{$I graphic.inc}
{$I picture.inc}
{$I sharedimage.inc}
{$I bitmapimage.inc}
{$I bitmap.inc}
{$I bitmapcanvas.inc}
{$I pen.inc}
{$I brush.inc}
{$I font.inc}
{$I canvas.inc}
{$I pixmap.inc}


initialization
  RegisterIntegerConsts(TypeInfo(TColor), @IdentToColor, @ColorToIdent);

finalization
  GraphicsFinalized:=true;
  FreeAndNil(PicClipboardFormats);
  FreeAndNil(PicFileFormats);


end.

{ =============================================================================

  $Log$
  Revision 1.40  2002/09/02 08:13:16  lazarus
  MG: fixed GraphicClass.Create

  Revision 1.39  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.38  2002/08/15 13:37:56  lazarus
  MG: started menuitem icon, checked, radio and groupindex

  Revision 1.37  2002/08/13 07:08:24  lazarus
  MG: added gdkpixbuf.pp and changes from Andrew Johnson

  Revision 1.36  2002/08/08 18:05:46  lazarus
  MG: added graphics extensions from Andrew Johnson

  Revision 1.35  2002/08/06 09:32:48  lazarus
  MG: moved TColor definition to graphtype.pp and registered TColor names

  Revision 1.34  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.33  2002/06/05 12:33:57  lazarus
  MG: fixed fonts in XLFD format and styles

  Revision 1.32  2002/06/04 15:17:21  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.31  2002/06/01 08:41:28  lazarus
  MG: DrawFramControl now uses gtk style, transparent STrechBlt

  Revision 1.30  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.29  2002/03/14 23:25:51  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.28  2002/03/11 23:22:46  lazarus
  MG: added TPicture clipboard support

  Revision 1.27  2002/03/11 20:36:34  lazarus
  MG: fixed parser for multiple variant identifiers

  Revision 1.26  2002/03/09 12:03:41  lazarus
  MG: started real graphics

  Revision 1.25  2002/03/09 11:55:13  lazarus
  MG: fixed class method completion

  Revision 1.24  2002/03/08 16:16:55  lazarus
  MG: fixed parser of end blocks in initialization section added label sections

  Revision 1.23  2002/03/08 09:30:30  lazarus
  MG: nicer parameter names

  Revision 1.22  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.21  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.20  2002/01/02 12:10:01  lazarus
  MG: fixed typo

  Revision 1.19  2001/12/28 11:41:50  lazarus
  MG: added TCanvas.Ellipse, TCanvas.Pie

  Revision 1.18  2001/12/21 18:16:59  lazarus
  Added TImage class
  Shane

  Revision 1.17  2001/11/12 22:12:57  lazarus
  MG: fixed parser: multiple brackets, nil, string[]

  Revision 1.16  2001/11/09 19:14:23  lazarus
  HintWindow changes
  Shane

  Revision 1.15  2001/10/25 19:02:18  lazarus
  MG: fixed parsing constants with OR, AND, XOR, MOD, DIV, SHL, SHR

  Revision 1.14  2001/10/24 00:35:55  lazarus
  MG: fixes for fpc 1.1: range check errors

  Revision 1.13  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.12  2001/08/05 10:14:50  lazarus
  MG: removed double props in OI, small bugfixes

  Revision 1.11  2001/06/26 00:08:35  lazarus
  MG: added code for form icons from Rene E. Beszon

  Revision 1.10  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.9  2001/03/21 00:20:29  lazarus
  MG: fixed memory leaks

  Revision 1.7  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.6  2001/03/05 14:20:04  lazarus
  added streaming to tgraphic, added tpicture

  Revision 1.5  2001/02/04 19:23:26  lazarus
  Goto dialog added
  Shane

  Revision 1.4  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.3  2001/01/31 21:16:45  lazarus
  Changed to TCOmboBox focusing.
  Shane

  Revision 1.2  2000/08/10 18:56:23  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.46  2000/05/08 15:56:58  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.45  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.44  2000/03/21 23:47:33  lazarus
  MWE:
    + Added TBitmap.MaskHandle & TGraphic.Draw & TBitmap.Draw

  Revision 1.43  2000/03/16 23:58:46  lazarus
  MWE:
    Added TPixmap for XPM support

  Revision 1.42  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.41  2000/03/10 13:13:37  lazarus
  *** empty log message ***

  Revision 1.40  2000/03/09 23:44:03  lazarus
  MWE:
    * Fixed colorcache
    * Fixed black window in new editor
    ~ Did some cosmetic stuff

  From Peter Dyson <peter@skel.demon.co.uk>:
    + Added Rect api support functions
    + Added the start of ScrollWindowEx

  Revision 1.39  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.38  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.37  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.36  2000/01/17 20:36:25  lazarus
  Fixed Makefile again.
  Made implementation of TScreen and screen info saner.
  Began to implemented DeleteObject in GTKWinAPI.
  Fixed a bug in GDI allocation which in turn fixed A LOT of other bugs :-)

  Revision 1.35  1999/12/14 22:05:37  lazarus
  More changes for TToolbar
  Shane

  Revision 1.34  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !

  Revision 1.33  1999/11/29 00:46:47  lazarus
  MWE:
    Added TBrush as gdiobject
    commented out some more mwedit MWE_FPC ifdefs

  Revision 1.32  1999/11/25 23:45:08  lazarus
  MWE:
    Added font as GDIobject
    Added some API testcode to testform
    Commented out some more IFDEFs in mwCustomEdit

  Revision 1.31  1999/11/19 01:09:43  lazarus
  MWE:
    implemented TCanvas.CopyRect
    Added StretchBlt
    Enabled creation of TCustomControl.Canvas
    Added a temp hack in TWinControl.Repaint to get a LM_PAINT

  Revision 1.30  1999/11/18 00:13:08  lazarus
  MWE:
    Partly Implemented SelectObject
    Added  ExTextOut
    Added  GetTextExtentPoint
    Added  TCanvas.TextExtent/TextWidth/TextHeight
    Added  TSize and HPEN

  Revision 1.29  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.28  1999/11/09 17:19:54  lazarus
  added the property PITCH to TFONT.
  Shane

  Revision 1.26  1999/11/05 17:48:17  lazarus
  Added a mwedit1 component to lazarus (MAIN.PP)
  It crashes on create.
  Shane

  Revision 1.25  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.24  1999/10/28 17:17:42  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.23  1999/10/25 17:38:52  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.22  1999/10/22 21:01:51  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.21  1999/10/19 21:16:23  lazarus
  TColor added to graphics.pp

  Revision 1.20  1999/10/18 07:32:42  lazarus
  Added definitions for Load methods in the TBitmap class. The
  methods have not been implemented yet. They need to be implemented.   CAW

  Revision 1.19  1999/09/26 16:58:01  lazarus
  MWE: Added TBitMap.Mask method

  Revision 1.18  1999/08/26 23:36:02  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.17  1999/08/25 18:53:02  lazarus
  Added Canvas.pixel property which allows
  the user to get/set the pixel color.  This will be used in the editor
  to create the illusion of the cursor by XORing the pixel with black.

  Shane

  Revision 1.16  1999/08/20 15:44:37  lazarus
  TImageList changes added from Marc Weustink

  Revision 1.15  1999/08/17 16:46:25  lazarus
  Slight modification to Editor.pp
  Shane

  Revision 1.14  1999/08/16 20:48:03  lazarus
  Added a changed event for TFOnt and code to get the average size of the font.  Doesn't seem to work very well yet.
  The "average size" code is found in gtkobject.inc.

  Revision 1.13  1999/08/16 15:48:49  lazarus
  Changes by file:
       Control: TCOntrol-Function GetRect added
                         ClientRect property added
                TImageList - Added Count
                TWinControl- Function Focused added.
      Graphics: TCanvas - CopyRect added - nothing finished on it though
                          Draw added - nothing finiushed on it though
                clbtnhighlight and clbtnshadow added.  Actual color values not right.
               IMGLIST.PP and IMGLIST.INC files added.

   A few other minor changes for compatability added.

    Shane

  Revision 1.12  1999/08/13 19:55:47  lazarus
  TCanvas.MoveTo added for compatability.

  Revision 1.11  1999/08/13 19:51:07  lazarus
  Minor changes for compatability made.

  Revision 1.10  1999/08/11 20:41:33  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.9  1999/08/02 01:13:33  lazarus
  Added new colors and corrected BTNFACE
  Need the TSCrollbar class to go further with the editor.
  Mouse doesn't seem to be working correctly yet when I click on the editor window

  Revision 1.8  1999/08/01 21:46:26  lazarus
  Modified the GETWIDTH and GETHEIGHT of TFOnt so you can use it to calculate the length in Pixels of a string.  This is now used in the editor.

  Shane

  Revision 1.7  1999/07/31 06:39:26  lazarus

       Modified the IntCNSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
