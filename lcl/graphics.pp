{  $Id$  }
{
 /***************************************************************************
                                Graphics.pp
                             -------------------
                             Graphic Controls
                   Initial Revision  : Mon Jul 26 0:02:58 1999

  Contains :
    TGraphicsObject - TFont

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit Graphics;

{$mode objfpc}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
 SysUtils,classes,vclGlobals,LMessages, LCLLinux;


type

TColor = longint;  //Also defined in LMessages.pp
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
  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontName = string;
  TFontStyle = (fsBold, fsItalic, fsStrikeOut, fsUnderline);
  TFontCharSet = 0..255;
  TFontDataName = string[LF_FACESIZE -1];
  TFontStyles = set of TFontStyle;
  TFOntStylesbase = set of TFontStyle;

  TFontData = record
    Handle : HWND;
    Height : Integer;
    Pitch : TFontPitch;
    Style : TFontStylesBase;
    CharSet : TFontCharSet;
    Name : TFontDataName;
  end;

  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideframe);
  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot,
              pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,pmNotMerge, pmMask,
              pmNotMask, pmXor, pmNotXor
             );

  TPenData = record
    Handle : LongInt;
    Color : TColor;
    Width : Integer;
    Style : TPenStyle;
  end;

  TBitmap = class; //forward declaration

  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

  TBrushData = record
    Handle : LongInt;
    Color : TColor;
    Bitmap : TBitmap;
    Style : TBrushStyle;
  end;

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
    procedure FreeHandle;
  Protected
    function GetHandle: HFONT;
    procedure SetHandle(const Value: HFONT);
    Procedure SetName(const value : TFontName);
    Function  GetName : TFontName;
    Procedure SetSize(value : Integer);
    Procedure SetHeight(value : Integer);
    Function  GetSize : Integer;
    procedure SetStyle(Value: TFontStyles);
    Procedure SetPitch(Value : TFontPitch);
  public
    procedure Assign(Source : TPersistent); override;
    procedure SetColor(Value : TColor);
    // Extra properties
    // TODO: implement them though GetTextMetrics, not here
    //Function GetWidth(Value : String) : Integer;
    constructor Create;
    destructor Destroy; override;
    property Color : TColor read FColor write SetColor;
    property Height : Integer read FFontData.Height write SetHeight;
    property Name : TFontName read GetName write SetName;
    property Pitch: TFontPitch read FFontData.Pitch write SetPitch;
    property PixelsPerInch : Integer read FPixelsPerInch;
    property Size: Integer read GetSize write SetSize;
    property Style : TFontStyles read FFontData.Style write SetStyle;
    // Extra properties
    // TODO: implement them though GetTextMetrics, not here
    //property Width : Integer read FWidth write FWidth;
    //property XBias : Integer read FXBias write FXBias;
    //property YBias : Integer read FYBias write FYBias;
    //-----------------
    property Handle : HFONT read GetHandle write SetHandle;
  end;


  TPen = class(TgraphicsObject)
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
    property Color: TColor read FPenData.Color write SetColor;
    property Handle : HPEN read GetHandle write SetHandle;
    property Mode: TPenMode read FMode write SetMode;
    property Style: TPenStyle read FPenData.Style write SetStyle;
    property Width: Integer read FPenData.Width write SetWidth;
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

  TFillStyle = (fsSurface, fsBorder);
  TFillMode = (fmAlternate, fmWinding);

  TCopymode = longint;

  TCanvasStates = (csHandleValid, csFontValid, csPenvalid, csBrushValid);
  TCanvasState = set of TCanvasStates;
  TCanvasOrientation = (csLefttoRight, coRighttoLeft);
  
  TCanvas = class;

  TGraphic = class(TPersistent)
  private
    FWidth : Integer;
    FHeight : Integer;
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
  public
    constructor Create; virtual;
    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
  end;

  TCanvas = class(TPersistent)
  private
    FAutoReDraw : Boolean;
    FState: TCanvasState;
    FFont : TFont;
    FPen: TPen;
    FBrush: TBrush;
    FPenPos : TPoint;
    FCopyMode : TCopyMode;
    FHandle : HDC;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure BrushChanged(ABrush: TObject);
    procedure FontChanged(AFont: TObject);
    procedure CreateBrush;
    procedure CreateFont;
    Procedure CreatePen;
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
    Procedure SetPen(value : TPen);
    Procedure SetPenPos(Value : TPoint);
    Procedure SetPixel(X,Y : Integer; Value : TColor);
  protected
    procedure CreateHandle; virtual;
    procedure RequiredState(ReqState: TCanvasState);
  public

    procedure Arc(x,y,width,height,angle1,angle2 : Integer);
    Procedure BrushCopy(Dest : TRect; InternalImages: TBitmap; Src : TRect; TransparentColor :TColor);
    constructor Create;
    Procedure CopyRect(const Dest : TRect; Canvas : TCanvas; const Source : TRect);
    destructor Destroy; override;
    Procedure Draw(X,Y: Integer; Graphic : TGraphic);
    Procedure FillRect(const Rect : TRect);
    Procedure Rectangle(X1,Y1,X2,Y2 : Integer);
    Procedure Line(X1,Y1,X2,Y2 : Integer);
    Procedure MoveTo(X1,Y1 : Integer);
    Procedure LineTo(X1,Y1 : Integer);
    Procedure TextOut(X,Y: Integer; const Text: String);
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    function TextWidth(const Text: string): Integer;
    Procedure Polygon(const Points: array of TPoint);
    property ClipRect: TRect read GetCanvasClipRect;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property OnChange : TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property Pixels[X, Y: Integer]: TCOlor read GetPixel write SetPixel;
    property Handle: HDC read GetHandle write FHandle;
  published
    property AutoRedraw : Boolean read FAutoReDraw write SetAutoReDraw;
    property Brush: TBrush read FBrush write SetBrush;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode default cmSrcCopy;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    // Extra
    property Color: TColor read GetColor write SetColor;
  end;

  {TBITMAP}

 TSharedImage = class
  private
    FRefCount: Integer;
  protected
    procedure Reference;
    procedure Release;
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

  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);

  TBitmap = class(TGraphic)
  private
    FCanvas: TCanvas;
    FImage : TBitmapImage;
    FMonochrome: Boolean;
    FPalette: HPALETTE;
    FPixelFormat: TPixelFormat;
    FTransparentColor: TColor;
    Procedure FreeContext;
    Procedure NewImage(NHandle: HBITMAP; NPallette: HPALETTE; const NDIB : TDIBSection; OS2Format : Boolean);
    procedure SetHandle(Value: HBITMAP);
    procedure SetMaskHandle(Value: HBITMAP);
    function GetHandle: HBITMAP; virtual;
    function GetMaskHandle: HBITMAP; virtual;
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure HandleNeeded;
    procedure MaskHandleNeeded;
    procedure PaletteNeeded;
    procedure ReadStream(Stream: TStream; Size: Longint); virtual;
  public
    constructor Create; override;
    destructor Destroy ; Override;
    procedure Assign(Source: TPersistent); override;
    procedure FreeImage;
    property Handle: HBITMAP read GetHandle write SetHandle;
    procedure LoadFromStream(Stream: TStream); {override;  // Uncomment when method is implemented in TGraphic }
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); virtual;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); virtual;
    Procedure LoadFromXPMFile(Filename : String);
    procedure Mask(ATransparentColor: TColor);
    procedure SaveToStream(Stream: TStream); {override;   // Uncomment when method is implemented in TGraphic }
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
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); override;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); override;
  end;
  

  { TIcon }
  {
    @abstract()
    Introduced by Marc Weustink <weus@quicknet.nl>
    Currently maintained by ?
  }
  TIcon = class(TGraphic)
  // Introduced to get TImageList compiled
  end;

  
var 
  { Stores information about the current screen }
  ScreenInfo : TLMScreenInit;
  
function ColorToRGB(Color: TColor): Longint;
  

(***************************************************************************
 ***************************************************************************)
implementation

uses Controls;

type
  TBitmapCanvas = class(TCanvas)
  private
    FBitmap : TBitMap;
    FOldBitMap : HBitmap;
    FOldPalette : HPALETTE;
    procedure FreeDC;
  protected
    procedure CreateHandle; override;
  public
    constructor Create(ABitMap : TBitmap);
    destructor Destroy; override;
    // TODO: replace this by property BitmapHandle;
    // MWE: Not needed
    //property Bitmap: TBitmap read FBitmap;
  end;


function ColorToRGB(Color: TColor): Longint;
begin
  if (Color and SYS_COLOR_BASE) <> 0
  then Result := GetSysColor(Color and $000000FF)
  else Result := Color;
  Result := Result and $FFFFFF;
  //WriteLN(Format('[ColorToRGB] Color %8x --> RGB %8x', [Color, Result]));
end;

{$I graphicsobject.inc}
{$I graphic.inc}
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
  CNSendMessage(LM_SCREENINIT, nil, @ScreenInfo);

end.

{ =============================================================================

  $Log$
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
