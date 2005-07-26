{  $Id$  }
{
 /***************************************************************************
                                graphics.pp
                                -----------
                             Graphic Controls
                   Initial Revision : Mon Jul 26 0:02:58 1999

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


{$IFDEF VER1_0_10}
  {$DEFINE DisableFPImage}
{$ENDIF}
{$IFNDEF VER1_0}
  {$DEFINE UseFPCanvas}
{$ENDIF}


uses
  SysUtils, Classes, Contnrs, FPCAdds,
  {$IFNDEF DisableFPImage}
  FPImage, FPReadPNG, FPWritePNG, FPReadBMP, FPWriteBMP, FPReadPNM, FPWritePNM, IntfGraphics,
  {$IFDEF UseFPCanvas}
  FPCanvas,
  {$ENDIF}
  {$ENDIF}
  AvgLvlTree,
  LCLStrConsts, LCLType, LCLProc, LMessages, LCLIntf, LResources, LCLResCache,
  GraphType, GraphMath, InterfaceBase;

type
  PColor = ^TColor;
  TColor = TGraphicsColor;

  TFontPitch = (fpDefault, fpVariable, fpFixed);
  TFontName = string;
  TFontDataName = string[LF_FACESIZE -1];
  TFontStyle = (fsBold, fsItalic, fsStrikeOut, fsUnderline);
  TFontStyles = set of TFontStyle;
  TFontStylesbase = set of TFontStyle;
  TFontCharSet = 0..255;

  TFontData = record
    Handle: HFont;
    Height: Integer;
    Pitch: TFontPitch;
    Style: TFontStylesBase;
    CharSet: TFontCharSet;
    Name: TFontDataName;
  end;

const
  // New TFont instances are initialized with the values in this structure.
  // About font default values: The default font is chosen by the interfaces
  // depending on the context. For example, there can be a different default
  // font for a button and a groupbox.
  DefFontData: TFontData = (
    Handle: 0;
    Height: 0;
    Pitch: fpDefault;
    Style: [];
    Charset: DEFAULT_CHARSET;
    Name: 'default'
    );

type
  { Reflects text style when drawn in a rectangle }

  TTextLayout = (tlTop, tlCenter, tlBottom);
  TTextStyle = packed record
    Alignment : TAlignment;  // TextRect Only: horizontal alignment

    Layout    : TTextLayout; // TextRect Only: vertical alignment

    SingleLine: boolean;     // If WordBreak is false then process #13, #10 as
                              // standard chars and perform no Line breaking.

    Clipping  : boolean;     // TextRect Only: Clip Text to passed Rectangle

    ExpandTabs: boolean;     // currently ignored

    ShowPrefix: boolean;     // TextRect Only: Process first single '&' per
                              //    line as an underscore and draw '&&' as '&'

    Wordbreak : boolean;     // TextRect Only: If line of text is too long
                              //    too fit between left and right boundaries
                              //    try to break into multiple lines between
                              //    words

    Opaque    : boolean;     // TextRect: Fills background with current Brush
                              // TextOut : Fills background with current
                              //            foreground color

    SystemFont: Boolean;     // Use the system font instead of Canvas Font
  end;

  {$IFDEF UseFPCanvas}
type
  TPenStyle = TFPPenStyle;
  TPenMode = TFPPenMode;
  TBrushStyle = TFPBrushStyle;

const
  psSolid = FPCanvas.psSolid;
  psDash = FPCanvas.psDash;
  psDot = FPCanvas.psDot;
  psDashDot = FPCanvas.psDashDot;
  psDashDotDot = FPCanvas.psDashDotDot;
  psClear = FPCanvas.psClear;
  //psInsideframe = FPCanvas.psInsideframe;

  pmBlack = FPCanvas.pmBlack;
  pmWhite = FPCanvas.pmWhite;
  pmNop = FPCanvas.pmNop;
  pmNot = FPCanvas.pmNot;
  pmCopy = FPCanvas.pmCopy;
  pmNotCopy = FPCanvas.pmNotCopy;
  pmMergePenNot = FPCanvas.pmMergePenNot;
  pmMaskPenNot = FPCanvas.pmMaskPenNot;
  pmMergeNotPen = FPCanvas.pmMergeNotPen;
  pmMaskNotPen = FPCanvas.pmMaskNotPen;
  pmMerge = FPCanvas.pmMerge;
  pmNotMerge = FPCanvas.pmNotMerge;
  pmMask = FPCanvas.pmMask;
  pmNotMask = FPCanvas.pmNotMask;
  pmXor = FPCanvas.pmXor;
  pmNotXor = FPCanvas.pmNotXor;
  
  bsSolid = FPCanvas.bsSolid;
  bsClear = FPCanvas.bsClear;
  bsHorizontal = FPCanvas.bsHorizontal;
  bsVertical = FPCanvas.bsVertical;
  bsFDiagonal = FPCanvas.bsFDiagonal;
  bsBDiagonal = FPCanvas.bsBDiagonal;
  bsCross = FPCanvas.bsCross;
  bsDiagCross = FPCanvas.bsDiagCross;
  {$ELSE}
type
  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear,
               psInsideframe);
  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy, pmMergePenNot,
              pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,pmNotMerge,
              pmMask, pmNotMask, pmXor, pmNotXor
             );
  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal,
                 bsBDiagonal, bsCross, bsDiagCross);

  {$ENDIF}
type
  TFillStyle = TGraphicsFillStyle;
  TFillMode = (fmAlternate, fmWinding);

  TCopymode = longint;

  TCanvasStates = (csHandleValid,
                   csFontValid, // true if Font properties correspond to
                                // selected Font Handle in DC
                   csPenvalid, csBrushValid, csRegionValid);
  TCanvasState = set of TCanvasStates;
  TCanvasOrientation = (csLefttoRight, coRighttoLeft);

  { TProgressEvent is a generic progress notification event which may be
        used by TGraphic classes with computationally intensive (slow)
        operations, such as loading, storing, or transforming image data.
    Event params:
      Stage - Indicates whether this call to the OnProgress event is to
        prepare for, process, or clean up after a graphic operation.  If
        OnProgress is called at all, the first call for a graphic operation
        will be with Stage = psStarting, to allow the OnProgress event handler
        to allocate whatever resources it needs to process subsequent progress
        notifications.  After Stage = psStarting, you are guaranteed that
        OnProgress will be called again with Stage = psEnding to allow you
        to free those resources, even if the graphic operation is aborted by
        an exception.  Zero or more calls to OnProgress with Stage = psRunning
        may occur between the psStarting and psEnding calls.
      PercentDone - The ratio of work done to work remaining, on a scale of
        0 to 100.  Values may repeat or even regress (get smaller) in
        successive calls.  PercentDone is usually only a guess, and the
        guess may be dramatically altered as new information is discovered
        in decoding the image.
      RedrawNow - Indicates whether the graphic can be/should be redrawn
        immediately.  Useful for showing successive approximations of
        an image as data is available instead of waiting for all the data
        to arrive before drawing anything.  Since there is no message loop
        activity during graphic operations, you should call Update to force
        a control to be redrawn immediately in the OnProgress event handler.
        Redrawing a graphic when RedrawNow = False could corrupt the image
        and/or cause exceptions.
      Rect - Area of image that has changed and needs to be redrawn.
      Msg - Optional text describing in one or two words what the graphic
        class is currently working on.  Ex:  "Loading" "Storing"
        "Reducing colors".  The Msg string can also be empty.
        Msg strings should be resourced for translation,  should not
        contain trailing periods, and should be used only for
        display purposes.  (do not: if Msg = 'Loading' then...)
  }
  TProgressStage = (psStarting, psRunning, psEnding);
  TProgressEvent = procedure (Sender: TObject; Stage: TProgressStage;
                          PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                          const Msg: string; var DoContinue: Boolean) of object;

  { For Delphi compatibility }
  TPixelFormat = (
    pfDevice,
    pf1bit,
    pf4bit,
    pf8bit,
    pf15bit,
    pf16bit,
    pf24bit,
    pf32bit,
    pfCustom
    );

  TTransparentMode = (
    tmAuto,
    tmFixed
    );

const
  // The following colors match the predefined Delphi Colors
  clBlack   = TColor($000000);
  clMaroon  = TColor($000080);
  clGreen   = TColor($008000);
  clOlive   = TColor($008080);
  clNavy    = TColor($800000);
  clPurple  = TColor($800080);
  clTeal    = TColor($808000);
  clGray    = TColor($808080);
  clSilver  = TColor($C0C0C0);
  clRed     = TColor($0000FF);
  clLime    = TColor($00FF00);
  clYellow  = TColor($00FFFF);
  clBlue    = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua    = TColor($FFFF00);
  clLtGray  = TColor($C0C0C0);
  clDkGray  = TColor($808080);
  clWhite   = TColor($FFFFFF);
  clCream   = TColor($F0FBFF);
  clNone    = TColor($1FFFFFFF);
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
  clForm                    = TColor(SYS_COLOR_BASE or COLOR_FORM);

  clEndColors               = TColor(SYS_COLOR_BASE or COLOR_ENDCOLORS);
  clColorDesktop            = TColor(SYS_COLOR_BASE or COLOR_DESKTOP);
  cl3DFace                  = TColor(SYS_COLOR_BASE or COLOR_3DFACE);
  cl3DShadow                = TColor(SYS_COLOR_BASE or COLOR_3DSHADOW);
  cl3DHiLight               = TColor(SYS_COLOR_BASE or COLOR_3DHIGHLIGHT);
  clBtnHiLight              = TColor(SYS_COLOR_BASE or COLOR_BTNHILIGHT);

  clFirstSpecialColor = clBtnHiLight;

  clMask = clWhite;
  clDontMask = clBlack;

  // CLX base, mapped, pseudo, rgb values
  clForeground = TColor(-1);
  clButton = TColor(-2);
  clLight = TColor(-3);
  clMidlight = TColor(-4);
  clDark = TColor(-5);
  clMid = TColor(-6);
  clText = TColor(-7);
  clBrightText = TColor(-8);
  clButtonText = TColor(-9);
  clBase = TColor(-10);
  clxBackground = TColor(-11); // only used as base for the CLX colors
  clShadow = TColor(-12);
  clxHighlight = TColor(-13);  // only used as base for the CLX colors
  clHighlightedText = TColor(-14);

  // CLX mapped role offsets
  cloNormal = 32;
  cloDisabled = 64;
  cloActive = 96;

  // CLX normal, mapped, pseudo, rgb values
  clNormalForeground = TColor(clForeground - cloNormal);
  clNormalButton = TColor(clButton - cloNormal);
  clNormalLight = TColor(clLight - cloNormal);
  clNormalMidlight = TColor(clMidlight - cloNormal);
  clNormalDark = TColor(clDark - cloNormal);
  clNormalMid = TColor(clMid - cloNormal);
  clNormalText = TColor(clText - cloNormal);
  clNormalBrightText = TColor(clBrightText - cloNormal);
  clNormalButtonText = TColor(clButtonText - cloNormal);
  clNormalBase = TColor(clBase - cloNormal);
  clNormalBackground = TColor(clxBackground - cloNormal);
  clNormalShadow = TColor(clShadow - cloNormal);
  clNormalHighlight = TColor(clxHighlight - cloNormal);
  clNormalHighlightedText = TColor(clHighlightedText - cloNormal);

  // CLX disabled, mapped, pseudo, rgb values
  clDisabledForeground = TColor(clForeground - cloDisabled);
  clDisabledButton = TColor(clButton - cloDisabled);
  clDisabledLight = TColor(clLight - cloDisabled);
  clDisabledMidlight = TColor(clMidlight - cloDisabled);
  clDisabledDark = TColor(clDark - cloDisabled);
  clDisabledMid = TColor(clMid - cloDisabled);
  clDisabledText = TColor(clText - cloDisabled);
  clDisabledBrightText = TColor(clBrightText - cloDisabled);
  clDisabledButtonText = TColor(clButtonText - cloDisabled);
  clDisabledBase = TColor(clBase - cloDisabled);
  clDisabledBackground = TColor(clxBackground - cloDisabled);
  clDisabledShadow = TColor(clShadow - cloDisabled);
  clDisabledHighlight = TColor(clxHighlight - cloDisabled);
  clDisabledHighlightedText = TColor(clHighlightedText - cloDisabled);

  // CLX active, mapped, pseudo, rgb values
  clActiveForeground = TColor(clForeground - cloActive);
  clActiveButton = TColor(clButton - cloActive);
  clActiveLight = TColor(clLight - cloActive);
  clActiveMidlight = TColor(clMidlight - cloActive);
  clActiveDark = TColor(clDark - cloActive);
  clActiveMid = TColor(clMid - cloActive);
  clActiveText = TColor(clText - cloActive);
  clActiveBrightText = TColor(clBrightText - cloActive);
  clActiveButtonText = TColor(clButtonText - cloActive);
  clActiveBase = TColor(clBase - cloActive);
  clActiveBackground = TColor(clxBackground - cloActive);
  clActiveShadow = TColor(clShadow - cloActive);
  clActiveHighlight = TColor(clxHighlight - cloActive);
  clActiveHighlightedText = TColor(clHighlightedText - cloActive);

type
  TMappedColor = clActiveHighlightedText..clNormalForeground;

  TColorGroup = (cgInactive, cgDisabled, cgActive);
  TColorRole = (crForeground, crButton, crLight, crMidlight, crDark, crMid,
    crText, crBrightText, crButtonText, crBase, crBackground, crShadow,
    crHighlight, crHighlightText, crNoRole);

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
  TCanvas = class;

  // standard LCL graphic formats
  TBitmap = class;                  // bmp
  TPixmap = class;                  // xpm
  TIcon = class;                    // ico
  TPortableNetworkGraphic = class;  // png
  TPortableAnyMapGraphic = class;   // pnm formats: pbm, pgm and ppm
  {$IFDEF UseSimpleJpeg}
  {$error will be added to the LCL, when fpc 2.0 is released. Use the jpeg package in the components/jpeg directory instead. }
  // MG: will be added to the LCL, when fpc 2.0 is released
  //     but then with the advanced features of the existing package
  {$ENDIF}

  { TGraphicsObject
    In Delphi VCL this is the ancestor of TFon, TPen and TBrush.
    With FPC 2.0 the LCL uses TFPCanvasHelper. }

  TGraphicsObject = class(TPersistent)
  private
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    Procedure DoChange(var Msg); message LM_CHANGED;
  protected
    procedure Changing; dynamic;
    procedure Changed; dynamic;
    Procedure Lock;
    Procedure UnLock;
  public
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


  { TFontHandleCacheDescriptor }

  TFontHandleCacheDescriptor = class(TResourceCacheDescriptor)
  public
    LogFont: TLogFont;
    LongFontName: string;
  end;


  { TFontHandleCache }

  TFontHandleCache = class(TResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
    function CompareDescriptors(Tree: TAvgLvlTree; Desc1, Desc2: Pointer): integer; override;
    function FindFont(TheFont: HFONT): TResourceCacheItem;
    function FindFontDesc(const LogFont: TLogFont;
                          const LongFontName: string): TFontHandleCacheDescriptor;
    function Add(TheFont: HFONT; const LogFont: TLogFont;
                 const LongFontName: string): TFontHandleCacheDescriptor;
  end;


  { TFont }

  {$IFDEF UseFPCanvas}
  TFont = class(TFPCustomFont)
  {$ELSE}
  TFont = class(TGraphicsObject)
  {$ENDIF}
  private
    FCanUTF8: boolean;
    FHandle: HFont;
    FPitch: TFontPitch;
    FStyle: TFontStylesBase;
    FCharSet: TFontCharSet;
    FPixelsPerInch: Integer;
    FUpdateCount: integer;
    FChanged: boolean;
    FFontHandleCached: boolean;
    FColor: TColor;
    {$IFDEF UseFPCanvas}
    {$ELSE}
    FFontName: string;
    FSize: Integer;   // Important: because of rounding errors both Size and
                      // Height are stored. This way setting Height and reading
                      // it again will result in the same value
    {$ENDIF}
    FHeight: integer; // FHeight = -(FSize * FPixelsPerInch) div 72
    procedure FreeHandle;
    procedure GetData(var FontData: TFontData);
    function IsNameStored: boolean;
    procedure SetData(const FontData: TFontData);
  protected
    {$IFDEF UseFPCanvas}
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetFlags(Index: integer; AValue: boolean); override;
    procedure SetName(AValue: string); override;
    procedure SetSize(AValue: integer); override;
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
    {$ELSE}
    procedure SetName(const AValue: string);
    procedure SetSize(AValue: Integer);
    {$ENDIF}
    procedure Changed; override;
    function  GetCharSet: TFontCharSet;
    function  GetHandle: HFONT;
    function  GetHeight: Integer;
    function  GetName: string;
    function  GetPitch: TFontPitch;
    function  GetSize: Integer;
    function  GetStyle: TFontStyles;
    procedure SetCharSet(const AValue: TFontCharSet);
    procedure SetColor(Value: TColor);
    procedure SetHandle(const Value: HFONT);
    procedure SetHeight(value: Integer);
    procedure SetPitch(Value: TFontPitch);
    procedure SetStyle(Value: TFontStyles);
  public
    constructor Create; {$IFDEF UseFPCanvas}override;{$ENDIF}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Assign(const ALogFont: TLogFont);
    procedure BeginUpdate;
    procedure EndUpdate;
    function HandleAllocated: boolean;
    function IsDefault: boolean;
    // Extra properties
    // TODO: implement them through GetTextMetrics, not here
    //Function GetWidth(Value: String): Integer;
    //property Width: Integer read FWidth write FWidth;
    //property XBias: Integer read FXBias write FXBias;
    //property YBias: Integer read FYBias write FYBias;
    property Handle: HFONT read GetHandle write SetHandle;
    property PixelsPerInch: Integer read FPixelsPerInch;
    property CanUTF8: boolean read FCanUTF8;
  published
    property CharSet: TFontCharSet read GetCharSet write SetCharSet default DEFAULT_CHARSET;
    property Color: TColor read FColor write SetColor default clWindowText;
    property Height: Integer read GetHeight write SetHeight;
    property Name: string read GetName write SetName stored IsNameStored;
    property Pitch: TFontPitch read GetPitch write SetPitch default fpDefault;
    property Size: Integer read GetSize write SetSize stored false;
    property Style: TFontStyles read GetStyle write SetStyle;
  end;


  { TPen }
  
  TPenData = record
    Handle: HPen;
    Color: TColor;
    Width: Integer;
    Style: TPenStyle;
  end;
  
  TPenHandleCache = class(TBlockResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
  end;

  {$IFDEF UseFPCanvas}
  TPen = class(TFPCustomPen)
  {$ELSE}
  TPen = class(TGraphicsObject)
  {$ENDIF}
  private
    FHandle: HPen;
    FColor: TColor;
    FPenHandleCached: boolean;
    {$IFDEF UseFPCanvas}
    {$ELSE}
    FWidth: Integer;
    FStyle: TPenStyle;
    FMode: TPenMode;
    {$ENDIF}
    procedure FreeHandle;
  protected
    {$IFDEF UseFPCanvas}
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
    {$ENDIF}
    function GetHandle: HPEN;
    procedure SetHandle(const Value: HPEN);
    procedure SetColor(Value: TColor);
    procedure SetMode(Value: TPenMode); {$IFDEF UseFPCanvas}override;{$ENDIF}
    procedure SetStyle(Value: TPenStyle); {$IFDEF UseFPCanvas}override;{$ENDIF}
    procedure SetWidth(value: Integer); {$IFDEF UseFPCanvas}override;{$ENDIF}
  public
    constructor Create; {$IFDEF UseFPCanvas}override;{$ENDIF}
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Handle: HPEN read GetHandle write SetHandle;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    {$IFDEF UseFPCanvas}
    property Mode default pmCopy;
    property Style default psSolid;
    property Width default 1;
    {$ELSE}
    property Mode: TPenMode read FMode write SetMode default pmCopy;
    property Style: TPenStyle read FStyle write SetStyle default psSolid;
    property Width: Integer read FWidth write SetWidth default 1;
    {$ENDIF}
  end;


  { TBrush }

  TBrushData = record
    Handle: HBrush;
    Color: TColor;
    Bitmap: TBitmap;
    Style: TBrushStyle;
  end;

  TBrushHandleCache = class(TBlockResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
  end;

  {$IFDEF UseFPCanvas}
  TBrush = class(TFPCustomBrush)
  {$ELSE}
  TBrush = class(TGraphicsObject)
  {$ENDIF}
  private
    FHandle: HBrush;
    FBrushHandleCached: boolean;
    FColor: TColor;
    FBitmap: TBitmap;
    {$IFDEF UseFPCanvas}
    {$ELSE}
    FStyle: TBrushStyle;
    {$ENDIF}
    procedure FreeHandle;
    procedure DoChange(var Msg); message LM_CHANGED;
  protected
    {$IFDEF UseFPCanvas}
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
    {$ENDIF}
    function GetHandle: HBRUSH;
    procedure SetBitmap(Value: TBitmap);
    procedure SetColor(Value: TColor);
    procedure SetHandle(const Value: HBRUSH);
    Procedure SetStyle(Value: TBrushStyle); {$IFDEF UseFPCanvas}override;{$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; {$IFDEF UseFPCanvas}override;{$ENDIF}
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Handle: HBRUSH read GetHandle write SetHandle;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    {$IFDEF UseFPCanvas}
    property Style default bsSolid;
    {$ELSE}
    property Style: TBrushStyle read FStyle write SetStyle default bsSolid;
    {$ENDIF}
  end;


  { TRegion }

  TRegionData = record
    Handle: HRgn;
    Rect: TRect;
    {Polygon Region Info - not used yet}
    Polygon: PPoint;//Polygon Points
    NumPoints: Longint;//Number of Points
    Winding: Boolean;//Use Winding mode
  end;

  TRegion = class(TGraphicsObject)
  private
    FRegionData: TRegionData;
    procedure FreeHandle;
  protected
    function GetHandle: HRGN;
    procedure SetHandle(const Value: HRGN);
    procedure SetClipRect(value: TRect);
    Function GetClipRect: TRect;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Handle: HRGN read GetHandle write SetHandle;
    property ClipRect: TRect read GetClipRect write SetClipRect;
  end;


  { TGraphic }

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
      Transparent - Some parts of the image are not opaque. aka the background
        can be seen through.
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
  protected
    procedure Changed(Sender: TObject); virtual;
    function Equals(Graphic: TGraphic): Boolean; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
    function GetEmpty: Boolean; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetPalette: HPALETTE; virtual;
    function GetTransparent: Boolean; virtual;
    function GetWidth: Integer; virtual; abstract;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
      const Msg: string; var DoContinue: boolean); dynamic;
    procedure ReadData(Stream: TStream); virtual;
    procedure SetHeight(Value: Integer); virtual; abstract;
    procedure SetPalette(Value: HPALETTE); virtual;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual; abstract;
    procedure SetModified(Value: Boolean);
    procedure WriteData(Stream: TStream); virtual;
  public
    constructor Create; virtual;
    procedure LoadFromFile(const Filename: string); virtual;
    procedure SaveToFile(const Filename: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromMimeStream(Stream: TStream; const MimeType: string); virtual;
    procedure LoadFromLazarusResource(const ResName: String); virtual; abstract;
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat); virtual;
    procedure LoadFromClipboardFormatID(ClipboardType: TClipboardType;
      FormatID: TClipboardFormat); virtual;
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat); virtual;
    procedure SaveToClipboardFormatID(ClipboardType: TClipboardType;
      FormatID: TClipboardFormat); virtual;
    procedure GetSupportedSourceMimeTypes(List: TStrings); virtual;
    function GetDefaultMimeType: string; virtual;
    class function GetFileExtensions: string; virtual;
    {$IFNDEF DisableFPImage}
    class function GetFPReaderForFileExt(
      const FileExtension: string): TFPCustomImageReaderClass; virtual;
    class function GetFPWriterForFileExt(
      const FileExtension: string): TFPCustomImageWriterClass; virtual;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; virtual;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; virtual;
    {$ENDIF}
  public
    property Empty: Boolean read GetEmpty;
    property Height: Integer read GetHeight write SetHeight;
    property Modified: Boolean read FModified write SetModified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
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
      Pixmap - Returns a pixmap.  If the contents is not already a pixmap, the
        contents are thrown away and a blank pixmap is returned.
      PNG - Returns a png.  If the contents is not already a png, the
        contents are thrown away and a blank png (TPortableNetworkGraphic) is
        returned.
      PNM - Returns a pnm.  If the contents is not already a pnm, the
        contents are thrown away and a blank pnm (TPortableAnyMapGraphic) is
        returned.
      }

  TPicture = class(TPersistent)
  private
    FGraphic: TGraphic;
    FOnChange: TNotifyEvent;
    //FNotify: IChangeNotifier;
    FOnProgress: TProgressEvent;
    procedure ForceType(GraphicType: TGraphicClass);
    function GetBitmap: TBitmap;
    function GetPNG: TPortableNetworkGraphic;
    function GetPNM: TPortableAnyMapGraphic;
    function GetPixmap: TPixmap;
    function GetIcon: TIcon;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure ReadData(Stream: TStream);
    procedure SetBitmap(Value: TBitmap);
    procedure SetPNG(const AValue: TPortableNetworkGraphic);
    procedure SetPNM(const AValue: TPortableAnyMapGraphic);
    procedure SetPixmap(Value: TPixmap);
    procedure SetIcon(Value: TIcon);
    procedure SetGraphic(Value: TGraphic);
    procedure WriteData(Stream: TStream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed(Sender: TObject); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
                       PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                       const Msg: string; var DoContinue: boolean); dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromClipboardFormat(FormatID: TClipboardFormat);
    procedure LoadFromClipboardFormatID(ClipboardType: TClipboardType;
      FormatID: TClipboardFormat);
    procedure SaveToClipboardFormat(FormatID: TClipboardFormat);
    class function SupportsClipboardFormat(FormatID: TClipboardFormat): Boolean;
    procedure Assign(Source: TPersistent); override;
    class procedure RegisterFileFormat(const AnExtension, ADescription: string;
      AGraphicClass: TGraphicClass);
    class procedure RegisterClipboardFormat(FormatID: TClipboardFormat;
      AGraphicClass: TGraphicClass);
    class procedure UnregisterGraphicClass(AClass: TGraphicClass);
    procedure Clear; virtual;
  public
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Pixmap: TPixmap read GetPixmap write SetPixmap;
    property PNG: TPortableNetworkGraphic read GetPNG write SetPNG;
    property PNM: TPortableAnyMapGraphic read GetPNM write SetPNM;
    property Icon: TIcon read GetIcon write SetIcon;
    property Graphic: TGraphic read FGraphic write SetGraphic;
    //property PictureAdapter: IChangeNotifier read FNotify write FNotify;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;


  EInvalidGraphic = class(Exception);
  EInvalidGraphicOperation = class(Exception);


  { TCanvas }

  {$IFDEF UseFPCanvas}
  TCanvas = class(TFPCustomCanvas)
  {$ELSE}
  TCanvas = class(TPersistent)
  {$ENDIF}
  private
    FAutoRedraw: Boolean;
    FState: TCanvasState;
    FSavedFontHandle: HFont;
    FSavedPenHandle: HPen;
    FSavedBrushHandle: HBrush;
    FSavedRegionHandle: HRGN;
    FCopyMode: TCopyMode;
    FHandle: HDC;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FTextStyle: TTextStyle;
    FLock: TCriticalSection;// FLock is initialized on demand
    FRegion: TRegion;
    FPen: TPen;
    FFont: TFont;
    FBrush: TBrush;
    {$IFNDEF UseFPCanvas}
    FLockCount: Integer;
    FPenPos: TPoint;
    {$ENDIF}
    procedure BrushChanged(ABrush: TObject);
    procedure FontChanged(AFont: TObject);
    procedure PenChanged(APen: TObject);
    procedure RegionChanged(ARegion: TObject);
    function GetColor: TColor;
    function GetHandle: HDC;
    procedure SetAutoRedraw(Value: Boolean); virtual;
    procedure SetColor(c: TColor);
    procedure SetLazFont(value: TFont);
    procedure SetLazPen(value: TPen);
    procedure SetLazBrush(value: TBrush);
    {$IFNDEF UseFPCanvas}
    procedure SetPenPos(const AValue: TPoint);
    {$ENDIF}
    procedure SetRegion(Value: TRegion);
    {$IFDEF UseFPCanvas}
  protected
    function DoCreateDefaultFont: TFPCustomFont; override;
    function DoCreateDefaultPen: TFPCustomPen; override;
    function DoCreateDefaultBrush: TFPCustomBrush; override;
    procedure SetColor(x, y: integer; const Value: TFPColor); override;
    function  GetColor(x, y: integer): TFPColor; override;
    procedure SetHeight(AValue: integer); override;
    function  GetHeight: integer; override;
    procedure SetWidth(AValue: integer); override;
    function  GetWidth: integer; override;
    procedure SetPenPos(const AValue: TPoint); override;
    procedure DoLockCanvas; override;
    procedure DoUnlockCanvas; override;
    procedure DoTextOut(x, y: integer; Text: string); override;
    procedure DoGetTextSize(Text: string; var w,h:integer); override;
    function  DoGetTextHeight(Text: string): integer; override;
    function  DoGetTextWidth(Text: string): integer; override;
    procedure DoRectangle(const Bounds: TRect); override;
    procedure DoRectangleFill(const Bounds: TRect); override;
    procedure DoRectangleAndFill(const Bounds: TRect); override;
    procedure DoEllipse(const Bounds: TRect); override;
    procedure DoEllipseFill(const Bounds: TRect); override;
    procedure DoEllipseAndFill(const Bounds: TRect); override;
    procedure DoPolygon(const Points: array of TPoint); override;
    procedure DoPolygonFill(const Points: array of TPoint); override;
    procedure DoPolygonAndFill(const Points: array of TPoint); override;
    procedure DoPolyline(const Points: array of TPoint); override;
    procedure DoFloodFill(x, y: integer); override;
    procedure DoMoveTo(x, y: integer); override;
    procedure DoLineTo(x, y: integer); override;
    procedure DoLine(x1, y1, x2, y2: integer); override;
    procedure DoCopyRect(x, y: integer; SrcCanvas: TFPCustomCanvas;
                         const SourceRect: TRect); override;
    procedure DoDraw(x, y: integer; const Image: TFPCustomImage); override;
    procedure CheckHelper(AHelper: TFPCanvasHelper); override;
    {$ELSE}
    {$ENDIF}
  protected
    function GetClipRect: TRect; {$IFDEF UseFPCanvas}override;{$ELSE}virtual;{$ENDIF}
    Function GetPixel(X,Y: Integer): TColor; virtual;
    procedure CreateBrush; virtual;
    procedure CreateFont; virtual;
    procedure CreateHandle; virtual;
    Procedure CreatePen; virtual;
    Procedure CreateRegion; virtual;
    procedure DeselectHandles; virtual;
    procedure PenChanging(APen: TObject); virtual;
    procedure RealizeAutoRedraw; virtual;
    procedure RequiredState(ReqState: TCanvasState); virtual;
    procedure SetHandle(NewHandle: HDC); virtual;
    procedure SetInternalPenPos(const Value: TPoint); virtual;
    Procedure SetPixel(X,Y: Integer; Value: TColor); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    procedure Refresh; virtual;
    procedure Changing; virtual;
    procedure Changed; virtual;

    // extra drawing methods (there are more in the ancestor TFPCustomCanvas)
    procedure Arc(Left, Top, AWidth, AHeight, angle1, angle2: Integer); virtual;
    procedure Arc(Left, Top, AWidth, AHeight, SX, SY, EX, EY: Integer); virtual;
    Procedure BrushCopy(Dest: TRect; InternalImages: TBitmap; Src: TRect;
                        TransparentColor: TColor); virtual;
    procedure Chord(x, y, AWidth, AHeight,
                    StartAngle16Deg, EndAngle16Deg: Integer); virtual;
    procedure Chord(x, y, AWidth, AHeight, SX, SY, EX, EY: Integer); virtual;
    Procedure CopyRect(const Dest: TRect; SrcCanvas: TCanvas;
                       const Source: TRect); virtual;
    Procedure Draw(X,Y: Integer; SrcGraphic: TGraphic); virtual;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); virtual;
    procedure Ellipse(const ARect: TRect); // already in fpcanvas
    procedure Ellipse(x1, y1, x2, y2: Integer); virtual; // already in fpcanvas
    Procedure FillRect(const ARect: TRect); virtual;
    Procedure FillRect(X1,Y1,X2,Y2: Integer);
    procedure FloodFill(X, Y: Integer; FillColor: TColor;
                        FillStyle: TFillStyle); virtual;
    procedure Frame3d(var ARect: TRect; const FrameWidth: integer;
                      const Style: TGraphicsBevelCut); virtual;
    procedure Frame(const ARect: TRect); virtual; // border using pen
    procedure Frame(X1,Y1,X2,Y2: Integer);     // border using pen
    procedure FrameRect(const ARect: TRect); virtual; // border using brush
    procedure FrameRect(X1,Y1,X2,Y2: Integer); // border using brush
    Procedure Line(X1,Y1,X2,Y2: Integer); virtual; // short for MoveTo();LineTo(); // already in fpcanvas
    Procedure LineTo(X1,Y1: Integer); virtual; // already in fpcanvas
    Procedure MoveTo(X1,Y1: Integer); virtual; // already in fpcanvas
    procedure RadialPie(x,y,AWidth, AHeight,
                        StartAngle16Deg, EndAngle16Deg: Integer); virtual;
    procedure RadialPie(x, y, AWidth, AHeight, sx, sy, ex, ey: Integer); virtual;
    procedure Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2,
                  StartX,StartY,EndX,EndY: Integer); virtual;
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean{$IFNDEF VER1_0} = False{$ENDIF};
                         Continuous: boolean{$IFNDEF VER1_0} = False{$ENDIF}); virtual;
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean{$IFNDEF VER1_0} = False{$ENDIF};
                         Continuous: boolean{$IFNDEF VER1_0} = False{$ENDIF});
    {$ifdef VER1_0}
    procedure PolyBezier(const Points: array of TPoint);
    {$endif}
    procedure Polygon(const Points: array of TPoint;
                      Winding: Boolean;
                      StartIndex: Integer{$IFNDEF VER1_0} = 0{$ENDIF};
                      NumPts: Integer {$IFNDEF VER1_0} = -1{$ENDIF});
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean{$IFNDEF VER1_0} = False{$ENDIF}); virtual;
    Procedure Polygon(const Points: array of TPoint); // already in fpcanvas
    procedure Polyline(const Points: array of TPoint;
                       StartIndex: Integer;
                       NumPts: Integer {$IFNDEF VER1_0} = -1{$ENDIF});
    procedure Polyline(Points: PPoint; NumPts: Integer); virtual;
    procedure Polyline(const Points: array of TPoint); // already in fpcanvas
    Procedure Rectangle(X1,Y1,X2,Y2: Integer); virtual; // already in fpcanvas
    Procedure Rectangle(const ARect: TRect); // already in fpcanvas
    Procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY: Integer); virtual;
    Procedure RoundRect(const Rect: TRect; RX,RY: Integer);
    procedure TextOut(X,Y: Integer; const Text: String); virtual; // already in fpcanvas
    procedure TextRect(const ARect: TRect; X, Y: integer; const Text: string);
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
                       const Style: TTextStyle); virtual;
    function TextExtent(const Text: string): TSize; virtual;
    function TextHeight(const Text: string): Integer; virtual;
    function TextWidth(const Text: string): Integer; virtual;
    function HandleAllocated: boolean; virtual;
    function GetUpdatedHandle(ReqState: TCanvasState): HDC; virtual;
  public
    {$IFNDEF UseFPCanvas}
    property ClipRect: TRect read GetClipRect;
    property PenPos: TPoint read FPenPos write SetPenPos;
    {$ENDIF}
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property Handle: HDC read GetHandle write SetHandle;
    property TextStyle: TTextStyle read FTextStyle write FTextStyle;
  published
    property AutoRedraw: Boolean read FAutoRedraw write SetAutoRedraw;
    property Brush: TBrush read FBrush write SetLazBrush;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode default cmSrcCopy;
    property Font: TFont read FFont write SetLazFont;
    property Pen: TPen read FPen write SetLazPen;
    property Region: TRegion read FRegion write SetRegion;
    property Color: TColor read GetColor write SetColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;


  { TSharedImage -  base class for reference counted images }

  TSharedImage = class
  private
    FRefCount: Integer;
  protected
    procedure Reference; // increase reference count
    procedure Release;   // decrease reference count
    procedure FreeHandle; virtual; abstract;
    property RefCount: Integer read FRefCount;
  public
    function HandleAllocated: boolean; virtual; abstract;
  end;


  { TBitmapImage
  
    Descendent of TSharedImage for TBitmap. If a TBitmap is assigned to another
    TBitmap, only the reference count will be increased and both will share the
    same TBitmapImage }

  TBitmapNativeType = (
    bnNone,  // not a TBitmap native type
    bnWinBitmap,
    bnXPixmap,
    bnIcon
    );
  TBitmapNativeTypes = set of TBitmapNativeType;

  TBitmapHandleType = (bmDIB, bmDDB);

  { TBitmapImage }

  TBitmapImage = class(TSharedImage)
  private
    FHandle: HBITMAP;   // output device dependent handle
    FMaskHandle: HBITMAP;
    FPalette: HPALETTE;
    FDIBHandle: HBITMAP;// output device independent handle
    FSaveStream: TMemoryStream;
    FSaveStreamClass: TFPCustomImageWriterClass;
    FSaveStreamType: TBitmapNativeType;
  protected
    procedure FreeHandle; override;
    procedure FreeMaskHandle;
    function ReleaseHandle: HBITMAP;
    function IsEmpty: boolean;
    function GetPixelFormat: TPixelFormat;
  public
    FDIB: TDIBSection;
    destructor Destroy; override;
    function HandleAllocated: boolean; override;
    function GetHandleType: TBitmapHandleType;
    property SaveStream: TMemoryStream read FSaveStream write FSaveStream;
    property SaveStreamType: TBitmapNativeType read FSaveStreamType write FSaveStreamType;
    property SaveStreamClass: TFPCustomImageWriterClass read FSaveStreamClass write FSaveStreamClass;
  end;


  { TBitmap }

  { TBitmap is the data of an image. The image can be loaded from a file,
    stream or resource in .bmp (windows bitmap format) or .xpm (XPixMap format)
    The loading routine automatically recognizes the format, so it is also used
    to load the imagess from Delphi form streams (e.g. .dfm files).
    When the handle is created, it is up to the interface (gtk, win32, ...)
    to convert it automatically to the best internal format. That is why the
    Handle is interface dependent.
    To access the raw data, see TLazIntfImage in IntfGraphics.pas }

  TBitmapInternalStateFlag = (
    bmisCreatingCanvas
    );
  TBitmapInternalState = set of TBitmapInternalStateFlag;

  { TBitmap }

  TBitmap = class(TGraphic)
  private
    FCanvas: TCanvas;
    FImage: TBitmapImage;
    FPalette: HPALETTE;
    FPixelFormat: TPixelFormat;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    FInternalState: TBitmapInternalState;
    procedure FreeCanvasContext;
    function GetCanvas: TCanvas;
    procedure CreateCanvas;
    function GetMonochrome: Boolean;
    procedure SetHandle(Value: HBITMAP);
    procedure SetMaskHandle(NewMaskHandle: HBITMAP);
    function GetHandleType: TBitmapHandleType;
    procedure SetHandleType(Value: TBitmapHandleType); virtual;
    procedure SetMonochrome(const AValue: Boolean);
    procedure SetPixelFormat(const AValue: TPixelFormat);
    procedure UpdatePixelFormat;
  protected
    procedure Changed(Sender: TObject); override;
    procedure Changing(Sender: TObject); virtual;
    procedure Draw(DestCanvas: TCanvas; const DestRect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    function GetHandle: HBITMAP; virtual;
    function GetMaskHandle: HBITMAP; virtual;
    procedure HandleNeeded;
    procedure MaskHandleNeeded;
    procedure PaletteNeeded;
    procedure UnshareImage(CopyContent: boolean);
    procedure FreeSaveStream;
    procedure ReadData(Stream: TStream); override;
    procedure SetWidthHeight(NewWidth, NewHeight: integer); virtual;
    procedure SetHeight(NewHeight: Integer); override;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetTransparentMode(Value: TTransparentMode);
    procedure SetWidth(NewWidth: Integer); override;
    procedure WriteData(Stream: TStream); override;
    procedure StoreOriginalStream(Stream: TStream; Size: integer); virtual;
    procedure WriteStreamWithFPImage(Stream: TStream; WriteSize: boolean;
                               WriterClass: TFPCustomImageWriterClass); virtual;
    procedure InitFPImageReader(ImgReader: TFPCustomImageReader); virtual;
    procedure InitFPImageWriter(ImgWriter: TFPCustomImageWriter); virtual;
    procedure FinalizeFPImageReader(ImgReader: TFPCustomImageReader); virtual;
    procedure FinalizeFPImageWriter(ImgWriter: TFPCustomImageWriter); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure FreeImage;
    function HandleAllocated: boolean;
    function MaskHandleAllocated: boolean;
    function PaletteAllocated: boolean;
    procedure CreateFromBitmapHandles(SrcBitmap, SrcMaskBitmap: HBitmap;
                                      const SrcRect: TRect);
    procedure LoadFromDevice(DC: HDC); virtual;
    function LazarusResourceTypeValid(const ResourceType: string): boolean; virtual;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromLazarusResource(const ResName: String); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); virtual;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); virtual;
    procedure LoadFromMimeStream(Stream: TStream; const MimeType: string); override;
    procedure GetSupportedSourceMimeTypes(List: TStrings); override;
    function GetDefaultMimeType: string; override;
    class function GetFileExtensions: string; override;
    Procedure LoadFromXPMFile(const Filename: String);
    procedure Mask(ATransparentColor: TColor);
    procedure SaveToStream(Stream: TStream); override;
    procedure ReadStream(Stream: TStream; UseSize: boolean; Size: Longint); virtual;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean); virtual;
    Function ReleaseHandle: HBITMAP;
    function ReleasePalette: HPALETTE;
    class function GetFPReaderForFileExt(
      const FileExtension: string): TFPCustomImageReaderClass; override;
    class function GetFPWriterForFileExt(
      const FileExtension: string): TFPCustomImageWriterClass; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
    procedure ReadStreamWithFPImage(Stream: TStream; UseSize: boolean;
                               Size: Longint;
                               ReaderClass: TFPCustomImageReaderClass); virtual;
    procedure WriteNativeStream(Stream: TStream; WriteSize: Boolean;
      SaveStreamType: TBitmapNativeType); virtual;
    function CreateIntfImage: TLazIntfImage;
    function CanReadGraphicStreams(AClass: TFPCustomImageWriterClass): boolean; virtual;
  public
    property Canvas: TCanvas read GetCanvas write FCanvas;
    property Handle: HBITMAP read GetHandle write SetHandle;
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat default pfDevice;
    // property ScanLine[Row: Integer]: Pointer; // Use TLazIntfImage for such things
    property TransparentColor: TColor read FTransparentColor
                                      write FTransparentColor default clNone;
    property TransparentMode: TTransparentMode read FTransparentMode
                                        write SetTransparentMode default tmAuto;
  end;


  { TPixmap }

  TPixmap = class(TBitmap)
  public
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean); override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  end;


  { TFPImageBitmap }
  { Use this class to easily create a TBitmap descendent for FPImage
    reader and writer }
  
  TFPImageBitmap = class(TBitmap)
  public
    class function GetFileExtensions: string; override;
    class function IsFileExtensionSupported(const FileExtension: string): boolean;
    class function GetFPReaderForFileExt(
      const FileExtension: string): TFPCustomImageReaderClass; override;
    class function GetFPWriterForFileExt(
      const FileExtension: string): TFPCustomImageWriterClass; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
    procedure ReadStream(Stream: TStream; UseSize: boolean; Size: Longint); override;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean); override;
    function GetDefaultMimeType: string; override;
  end;


  { TPortableNetworkGraphic }

  TPortableNetworkGraphic = class(TFPImageBitmap)
  public
    class function GetFileExtensions: string; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  end;


  { TPortableAnyMapGraphic }

  TPortableAnyMapGraphic = class(TFPImageBitmap)
  public
    class function GetFileExtensions: string; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  end;

  
  { TIcon }
  {
    TIcon reads and writes .ICO file format.
    A .ico file typically contains several versions of the same image. When loading,
    the largest/most colourful image is loaded as the TBitmap and so can be handled
    as any other bitmap. Any other versions of the images are available via the
    Bitmaps property
    Writing is not (yet) implemented.
  }
  TIcon = class(TBitmap)
  private
    FBitmaps: TObjectList;
  protected
    procedure ReadData(Stream: TStream); override;
    procedure InitFPImageReader(ImgReader: TFPCustomImageReader); override;
  public
    class function GetFileExtensions: string; override;
    property Bitmaps: TObjectList read FBitmaps;
    destructor Destroy; override;
    procedure AddBitmap(Bitmap: TBitmap); { Note that Ownership passes to TIcon }
  end;



function GraphicFilter(GraphicClass: TGraphicClass): string;
function GraphicExtension(GraphicClass: TGraphicClass): string;
function GraphicFileMask(GraphicClass: TGraphicClass): string;
function GetGraphicClassForFileExtension(const FileExt: string): TGraphicClass;
function GetFPImageReaderForFileExtension(const FileExt: string
  ): TFPCustomImageReaderClass;
function GetFPImageWriterForFileExtension(const FileExt: string
  ): TFPCustomImageWriterClass;

type
  // Color / Identifier mapping
  TGetColorStringProc = procedure(const s:ansistring) of object;

function IdentEntry(Entry: Longint; var MapEntry: TIdentMapEntry): boolean;
function ColorToIdent(Color: Longint; var Ident: String): Boolean;
function IdentToColor(const Ident: string; var Color: Longint): Boolean;
function SysColorToSysColorIndex(Color: TColor): integer;
function ColorToRGB(Color: TColor): TColor;
function ColorToString(Color: TColor): AnsiString;
function StringToColor(const S: shortstring): TColor;
procedure GetColorValues(Proc: TGetColorStringProc);

Function Blue(rgb: TColor): BYTE;
Function Green(rgb: TColor): BYTE;
Function Red(rgb: TColor): BYTE;
procedure RedGreenBlue(rgb: TColor; var Red, Green, Blue: Byte);
function FPColorToTColor(const FPColor: TFPColor): TColor;
function TColorToFPColor(const c: TColor): TFPColor;

// fonts
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
function XLFDHeightIsSet(const LongFontName: string): boolean;

// graphics
type
  TOnLoadGraphicFromClipboardFormat =
    procedure(Dest: TGraphic; ClipboardType: TClipboardType;
              FormatID: TClipboardFormat);
  TOnSaveGraphicToClipboardFormat =
    procedure(Src: TGraphic; ClipboardType: TClipboardType;
              FormatID: TClipboardFormat);

var
  OnLoadGraphicFromClipboardFormat: TOnLoadGraphicFromClipboardFormat=nil;
  OnSaveGraphicToClipboardFormat: TOnSaveGraphicToClipboardFormat=nil;

function TestStreamBitmapNativeType(const AStream: TStream): TBitmapNativeType;
function TestStreamIsBMP(const AStream: TStream): boolean;
function TestStreamIsXPM(const AStream: TStream): boolean;
function TestStreamIsIcon(const AStream: TStream): boolean;

function XPMToPPChar(const XPM: string): PPChar;
function LazResourceXPMToPPChar(const ResourceName: string): PPChar;
function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;
function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer
                     ): boolean;

var
  { Stores information about the current screen
    - initialized on Interface startup }
  ScreenInfo: TScreenInfo=(PixelsPerInchX:72;PixelsPerInchY:72;
                           ColorDepth:24;Initialized:false;);
  
  FontResourceCache: TFontHandleCache;
  PenResourceCache: TPenHandleCache;
  BrushResourceCache: TBrushHandleCache;

const
  FontCharsets: array[0..18] of TIdentMapEntry = (
    (Value: ANSI_CHARSET;        Name: 'ANSI_CHARSET'),
    (Value: DEFAULT_CHARSET;     Name: 'DEFAULT_CHARSET'),
    (Value: SYMBOL_CHARSET;      Name: 'SYMBOL_CHARSET'),
    (Value: MAC_CHARSET;         Name: 'MAC_CHARSET'),
    (Value: SHIFTJIS_CHARSET;    Name: 'SHIFTJIS_CHARSET'),
    (Value: HANGEUL_CHARSET;     Name: 'HANGEUL_CHARSET'),
    (Value: JOHAB_CHARSET;       Name: 'JOHAB_CHARSET'),
    (Value: GB2312_CHARSET;      Name: 'GB2312_CHARSET'),
    (Value: CHINESEBIG5_CHARSET; Name: 'CHINESEBIG5_CHARSET'),
    (Value: GREEK_CHARSET;       Name: 'GREEK_CHARSET'),
    (Value: TURKISH_CHARSET;     Name: 'TURKISH_CHARSET'),
    (Value: VIETNAMESE_CHARSET;  Name: 'VIETNAMESE_CHARSET'),
    (Value: HEBREW_CHARSET;      Name: 'HEBREW_CHARSET'),
    (Value: ARABIC_CHARSET;      Name: 'ARABIC_CHARSET'),
    (Value: BALTIC_CHARSET;      Name: 'BALTIC_CHARSET'),
    (Value: RUSSIAN_CHARSET;     Name: 'RUSSIAN_CHARSET'),
    (Value: THAI_CHARSET;        Name: 'THAI_CHARSET'),
    (Value: EASTEUROPE_CHARSET;  Name: 'EASTEUROPE_CHARSET'),
    (Value: OEM_CHARSET;         Name: 'OEM_CHARSET'));


(***************************************************************************
 ***************************************************************************)

procedure Register;

implementation

procedure Register;
begin
  RegisterClasses([TBitmap,TPixmap,TPortableNetworkGraphic,TPortableAnyMapGraphic,TPicture,
                   TFont,TPen,TBrush,TRegion]);
end;

const
  GraphicsFinalized: boolean = false;

type
  TBitmapCanvas = class(TCanvas)
  private
    FBitmap: TBitmap;
    FOldBitmapValid: boolean;
    FOldBitmap: HBitmap;
    FOldPaletteValid: boolean;
    FOldPalette: HPALETTE;
    procedure FreeDC; // called by TBitmap.FreeCanvasContext
  protected
    procedure CreateHandle; override;
    procedure DeselectHandles; override;
  public
    constructor Create(ABitmap: TBitmap);
    destructor Destroy; override;
  end;


{ Color mapping routines }

const
  Colors: array[0..109] of TIdentMapEntry = (
    // The following colors match the predefined Delphi Colors
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
    (Value: clLtGray; Name: 'clLtGray'),
    (Value: clDkGray; Name: 'clDkGray'),
    (Value: clWhite; Name: 'clWhite'),
    (Value: clCream; Name: 'clCream'),
    (Value: clNone; Name: 'clNone'),
    (Value: clDefault; Name: 'clDefault'),

    //System colors
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
    (Value: clAppWorkspace; Name: 'clAppWorkspace'),
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

    (Value: clHotLight; Name: 'clHotLight'),
    (Value: clGradientActiveCaption; Name: 'clGradientActiveCaption'),
    (Value: clGradientInactiveCaption; Name: 'clGradientInactiveCaption'),
    (Value: clForm; Name: 'clForm'),

    (Value: clEndColors; Name: 'clEndColors'),
    (Value: clColorDesktop; Name: 'clColorDesktop'),
    (Value: cl3DFace; Name: 'cl3DFace'),
    (Value: cl3DShadow; Name: 'cl3DShadow'),
    (Value: cl3DHiLight; Name: 'cl3DHiLight'),
    (Value: clBtnHiLight; Name: 'clBtnHiLight'),

    // CLX base, mapped, pseudo, rgb values
    (Value: clForeground; Name: 'clForeground'),
    (Value: clButton; Name: 'clButton'),
    (Value: clLight; Name: 'clLight'),
    (Value: clMidlight; Name: 'clMidlight'),
    (Value: clDark; Name: 'clDark'),
    (Value: clMid; Name: 'clMid'),
    (Value: clText; Name: 'clText'),
    (Value: clBrightText; Name: 'clBrightText'),
    (Value: clButtonText; Name: 'clButtonText'),
    (Value: clBase; Name: 'clBase'),
    //clBackground
    (Value: clShadow; Name: 'clShadow'),
    //clHighlight
    (Value: clHighlightedText; Name: 'clHighlightedText'),

    // CLX normal, mapped, pseudo, rgb values
    (Value: clNormalForeground; Name: 'clNormalForeground'),
    (Value: clNormalButton; Name: 'clNormalButton'),
    (Value: clNormalLight; Name: 'clNormalLight'),
    (Value: clNormalMidlight; Name: 'clNormalMidlight'),
    (Value: clNormalDark; Name: 'clNormalDark'),
    (Value: clNormalMid; Name: 'clNormalMid'),
    (Value: clNormalText; Name: 'clNormalText'),
    (Value: clNormalBrightText; Name: 'clNormalBrightText'),
    (Value: clNormalButtonText; Name: 'clNormalButtonText'),
    (Value: clNormalBase; Name: 'clNormalBase'),
    (Value: clNormalBackground; Name: 'clNormalBackground'),
    (Value: clNormalShadow; Name: 'clNormalShadow'),
    (Value: clNormalHighlight; Name: 'clNormalHighlight'),
    (Value: clNormalHighlightedText; Name: 'clNormalHighlightedText'),

    // CLX disabled, mapped, pseudo, rgb values
    (Value: clDisabledForeground; Name: 'clDisabledForeground'),
    (Value: clDisabledButton; Name: 'clDisabledButton'),
    (Value: clDisabledLight; Name: 'clDisabledLight'),
    (Value: clDisabledMidlight; Name: 'clDisabledMidlight'),
    (Value: clDisabledDark; Name: 'clDisabledDark'),
    (Value: clDisabledMid; Name: 'clDisabledMid'),
    (Value: clDisabledText; Name: 'clDisabledText'),
    (Value: clDisabledBrightText; Name: 'clDisabledBrightText'),
    (Value: clDisabledButtonText; Name: 'clDisabledButtonText'),
    (Value: clDisabledBase; Name: 'clDisabledBase'),
    (Value: clDisabledBackground; Name: 'clDisabledBackground'),
    (Value: clDisabledShadow; Name: 'clDisabledShadow'),
    (Value: clDisabledHighlight; Name: 'clDisabledHighlight'),
    (Value: clDisabledHighlightedText; Name: 'clDisabledHighlightedText'),

    // CLX active, mapped, pseudo, rgb values
    (Value: clActiveForeground; Name: 'clActiveForeground'),
    (Value: clActiveButton; Name: 'clActiveButton'),
    (Value: clActiveLight; Name: 'clActiveLight'),
    (Value: clActiveMidlight; Name: 'clActiveMidlight'),
    (Value: clActiveDark; Name: 'clActiveDark'),
    (Value: clActiveMid; Name: 'clActiveMid'),
    (Value: clActiveText; Name: 'clActiveText'),
    (Value: clActiveBrightText; Name: 'clActiveBrightText'),
    (Value: clActiveButtonText; Name: 'clActiveButtonText'),
    (Value: clActiveBase; Name: 'clActiveBase'),
    (Value: clActiveBackground; Name: 'clActiveBackground'),
    (Value: clActiveShadow; Name: 'clActiveShadow'),
    (Value: clActiveHighlight; Name: 'clActiveHighlight'),
    (Value: clActiveHighlightedText; Name: 'clActiveHighlightedText')
    );

function IdentEntry(Entry: Longint; var MapEntry: TIdentMapEntry): boolean;
begin
  Result := False;
  if (Entry >= 0) and (Entry <= High(Colors)) then
  begin
    MapEntry := Colors[Entry];
    Result := True;
  end;
end;

function ColorToIdent(Color: Longint; var Ident: String): Boolean;
begin
  Result := IntToIdent(Color, Ident, Colors);
end;

function IdentToColor(const Ident: string; var Color: Longint): Boolean;
begin
  Result := IdentToInt(Ident, Color, Colors);
end;

function SysColorToSysColorIndex(Color: TColor): integer;
begin
  if (Cardinal(Color) and Cardinal(SYS_COLOR_BASE)) <> 0 then begin
    case Color of
    clHighlightedText..clForeground:
      Result:=clForeground+COLOR_clForeground-Color;
    clNormalHighlightedText..clNormalForeground:
      Result:=clNormalForeground+COLOR_clNormalForeground-Color;
    clDisabledHighlightedText..clDisabledForeground:
      Result:=clDisabledForeground+COLOR_clDisabledForeground-Color;
    clActiveHighlightedText..clActiveForeground:
      Result:=clActiveForeground+COLOR_clActiveForeground-Color;
    else
      Result:=Color and $FF;
    end;
  end else begin
    Result:=-1;
  end;
end;

function ColorToRGB(Color: TColor): TColor;
begin
  if (Cardinal(Color) and Cardinal(SYS_COLOR_BASE)) <> 0
  then Result := GetSysColor(SysColorToSysColorIndex(Color))
  else Result := Color;
  Result := Result and $FFFFFF;
end;

function ColorToString(Color: TColor): AnsiString;
begin
  Result := '';
  if not ColorToIdent(Color, Result) then
    Result:='$'+HexStr(Color,8);
end;

function StringToColor(const S: shortstring): TColor;
begin
  Result := clNone;
  if not IdentToColor(S, Longint(Result)) then
    Result := TColor(StrToInt(S));
end;

procedure GetColorValues(Proc: TGetColorStringProc);
var
  I: Integer;
begin
  for I := Low(Colors) to High(Colors) do Proc(Colors[I].Name);
end;

Function Blue(rgb: TColor): BYTE;
begin
  Result := (rgb shr 16) and $000000ff;
end;

Function Green(rgb: TColor): BYTE;
begin
  Result := (rgb shr 8) and $000000ff;
end;

Function Red(rgb: TColor): BYTE;
begin
  Result := rgb and $000000ff;
end;

procedure RedGreenBlue(rgb: TColor; var Red, Green, Blue: Byte);
begin
  Red := rgb and $000000ff;
  Green := (rgb shr 8) and $000000ff;
  Blue := (rgb shr 16) and $000000ff;
end;

{$IFNDEF DisableFPImage}
function FPColorToTColor(const FPColor: TFPColor): TColor;
begin
  Result:=((FPColor.Red shr 8) and $ff)
       or (FPColor.Green and $ff00)
       or ((FPColor.Blue shl 8) and $ff0000);
end;

function TColorToFPColor(const c: TColor): TFPColor;
begin
  Result.Red:=(c and $ff);
  Result.Red:=Result.Red+(Result.Red shl 8);
  Result.Green:=(c and $ff00);
  Result.Green:=Result.Green+(Result.Green shr 8);
  Result.Blue:=(c and $ff0000) shr 8;
  Result.Blue:=Result.Blue+(Result.Blue shr 8);
  Result.Alpha:=FPImage.alphaOpaque;
end;
{$ENDIF}

{$I graphicsobject.inc}
{$I graphic.inc}
{$I picture.inc}
{$I sharedimage.inc}
{$I bitmapimage.inc}
{$I bitmap.inc}
{$I bitmapcanvas.inc}
{$I pen.inc}
{$I brush.inc}
{$I region.inc}
{$I font.inc}
{$I canvas.inc}
{$I pixmap.inc}
{$I png.inc}
{$I pnm.inc}


{ TFPImageBitmap }

function TFPImageBitmap.GetFileExtensions: string;
begin
  Result:='';
end;

function TFPImageBitmap.IsFileExtensionSupported(
  const FileExtension: string): boolean;
var
  Extensions: String;
  StartPos: Integer;
  EndPos: Integer;
  i: Integer;
  Ext: String;
begin
  Result:=false;
  if FileExtension='' then exit;
  Extensions:=GetFileExtensions;
  if Extensions='' then exit;
  Ext:=FileExtension;
  if Ext[1]='.'  then begin
    Ext:=copy(Ext,2,length(Ext));
    if Ext='' then exit;
  end;
  StartPos:=1;
  while StartPos<=length(Extensions) do begin
    if not (Extensions[StartPos] in [';',' ']) then begin
      EndPos:=StartPos;
      while (EndPos<=length(Extensions)) and (Extensions[EndPos]<>';') do
        inc(EndPos);
      if EndPos-StartPos=length(Ext) then begin
        i:=1;
        while (i<=length(Ext))
        and (upcase(Extensions[StartPos+i-1])=upcase(Ext[i])) do
          inc(i);
        if i>length(Ext) then begin
          Result:=true;
          exit;
        end;
      end;
      StartPos:=EndPos;
    end else
      inc(StartPos);
  end;
end;

function TFPImageBitmap.GetFPReaderForFileExt(const FileExtension: string
  ): TFPCustomImageReaderClass;
begin
  if IsFileExtensionSupported(FileExtension) then
    Result:=GetDefaultFPReader
  else
    Result:=nil;
end;

function TFPImageBitmap.GetFPWriterForFileExt(const FileExtension: string
  ): TFPCustomImageWriterClass;
begin
  if IsFileExtensionSupported(FileExtension) then
    Result:=GetDefaultFPWriter
  else
    Result:=nil;
end;

function TFPImageBitmap.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=nil;
end;

function TFPImageBitmap.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result:=nil;
end;

function TFPImageBitmap.LazarusResourceTypeValid(const ResourceType: string
  ): boolean;
begin
  Result:=IsFileExtensionSupported(ResourceType);
end;

procedure TFPImageBitmap.ReadStream(Stream: TStream; UseSize: boolean;
  Size: Longint);
begin
  ReadStreamWithFPImage(Stream,UseSize,Size,GetDefaultFPReader);
end;

procedure TFPImageBitmap.WriteStream(Stream: TStream; WriteSize: Boolean);
begin
  WriteStreamWithFPImage(Stream,WriteSize,GetDefaultFPWriter);
end;

function TFPImageBitmap.GetDefaultMimeType: string;
var
  DefaultFileExt: String;
  i: Integer;
begin
  DefaultFileExt:=GetFileExtensions;
  i:=1;
  while (i<=length(DefaultFileExt)) and (DefaultFileExt[i]<>';') do
    inc(i);
  if i<=length(DefaultFileExt) then
    DefaultFileExt:=copy(DefaultFileExt,1,i);
  Result:='image/'+DefaultFileExt;
end;

{ TIcon }

const
  IconSignature: array [0..3] of char = #0#0#1#0;
  
function TestStreamIsIcon(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@IconSignature,4);
  AStream.Position:=OldPosition;
end;

{$IFNDEF DisableFPImage}
procedure TIcon.ReadData(Stream: TStream);
var
  Size: longint;
  Position: TStreamSeekType;
begin
  Position := Stream.Position;
  Stream.Read(Size, 4); // Beware BigEndian and LowEndian sytems
  {$IFDEF FPC_BIG_ENDIAN}
  Size := LEtoN(Size);
  {$ENDIF}
  if CompareMem(@Size,@IconSignature,4) then begin
    // Assume Icon - stream without explicit size
    Stream.Position := Position;
    ReadStream(Stream, false, Size);
  end else
    ReadStream(Stream, true, Size);
end;

procedure TIcon.InitFPImageReader(ImgReader: TFPCustomImageReader);
begin
  inherited InitFPImageReader(ImgReader);
  if ImgReader is TLazReaderIcon then
    TLazReaderIcon(ImgReader).Icon := self;
end;

function TIcon.GetFileExtensions: string;
begin
  Result:='ico';
end;

destructor TIcon.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBitmaps);
end;

procedure TIcon.AddBitmap(Bitmap: TBitmap);
begin
  if not Assigned(FBitmaps) then
    FBitmaps := TObjectList.create(True);
  FBitmaps.Add(Bitmap);
end;
{$ENDIF}

procedure InterfaceFinal;
begin
  //debugln('Graphics.InterfaceFinal');
  FreeAndNil(FontResourceCache);
  FreeAndNil(PenResourceCache);
  FreeAndNil(BrushResourceCache);
end;

initialization
  FontResourceCache:=TFontHandleCache.Create;
  PenResourceCache:=TPenHandleCache.Create;
  BrushResourceCache:=TBrushHandleCache.Create;
  RegisterIntegerConsts(TypeInfo(TColor), @IdentToColor, @ColorToIdent);
  RegisterIntegerConsts(TypeInfo(TFontCharset), @IdentToCharset, @CharsetToIdent);
  RegisterInterfaceFinalizationHandler(@InterfaceFinal);

finalization
  GraphicsFinalized:=true;
  OnLoadGraphicFromClipboardFormat:=nil;
  OnSaveGraphicToClipboardFormat:=nil;
  FreeAndNil(PicClipboardFormats);
  FreeAndNil(PicFileFormats);


end.

{ =============================================================================

  $Log$
  Revision 1.179  2005/07/26 08:45:15  vincents
  initialize variables at declaration instead in the unit initialization   from Florian Köberle

  Revision 1.178  2005/07/19 08:31:21  vincents
  added ColorBox (from Darius) to LCL

  Revision 1.177  2005/07/18 09:29:11  mattias
  Added TProtableAnyMapGraphic and fixed loading .ico on BIG_ENDIAN systems  from Colin Western

  Revision 1.176  2005/06/25 15:34:03  mattias
  fixed a few fpc over warnings  from Andrew Haines

  Revision 1.175  2005/06/22 09:45:59  mattias
  implemented saving alpha bmp and using transparency for IDE glyph editor

  Revision 1.174  2005/03/04 13:50:08  mattias
  fixed Arc and changed x,y to Left,Top to make meaning more clear

  Revision 1.173  2005/02/25 20:29:55  mattias
  FPCanvas is now used as default

  Revision 1.172  2005/01/15 11:56:13  mattias
  fixed locking TCanvas

  Revision 1.171  2005/01/10 18:44:44  mattias
  implemented the fpCanvas support for the LCL - Compile with -dUseFPCanvas

  Revision 1.170  2005/01/08 15:06:06  mattias
  fixed TabOrder dialog for new TabOrder

  Revision 1.169  2005/01/08 10:43:07  mattias
  added Lagunov Aleksey to Contributors.txt

  Revision 1.168  2005/01/07 21:02:59  mattias
  TFont, TBrush, TPen can now be used with fpCanvas

  Revision 1.167  2005/01/07 18:40:10  mattias
  clean up, added GetRGBValues

  Revision 1.166  2005/01/07 17:40:59  mattias
  fixed TTabSheet.SetPageControl

  Revision 1.165  2004/12/26 22:39:56  mattias
  updated finnish translation  from Seppo

  Revision 1.164  2004/12/23 22:38:18  mattias
  implemented TIElementName of link of RTTI controls for set elements

  Revision 1.163  2004/12/22 23:54:21  mattias
  started TControl.AnchorSide

  Revision 1.162  2004/12/22 19:56:44  mattias
  started TFont mirgration to fpCanvas font

  Revision 1.161  2004/11/20 11:20:06  mattias
  implemented creating classes at run time from any TComponent descendant

  Revision 1.160  2004/11/10 18:23:56  mattias
  impementing changing a TLabel.Font properties Size, Height, Name, Style - set only at Handle creation time

  Revision 1.159  2004/11/07 01:10:05  mattias
  fixed double calling destructor for resource cache items

  Revision 1.158  2004/10/01 13:25:51  mattias
  fixed 1.0.10 compilation

  Revision 1.157  2004/10/01 13:16:43  mattias
  fixed unselecting TCanvas objects

  Revision 1.156  2004/09/29 15:18:26  mattias
  fixed TBitmap.Canvas.Frame3d

  Revision 1.155  2004/09/27 10:01:18  mattias
  added TLazIntfImage.TColors property

  Revision 1.154  2004/09/24 21:34:14  micha
  convert LM_CREATE message to interface methods
  remove SendMsgToInterface, CNSendMessage and related methods
  remove TWidgetSet.IntSendMessage3; all LCL to interface messages have been converted

  Revision 1.153  2004/09/24 13:45:32  mattias
  fixed TCanvas.TextRect Delphi compatible Rect and added TBarChart from Michael VC

  Revision 1.152  2004/09/20 23:13:46  mattias
  moved remaining TCanvas handle dependent methods to protected

  Revision 1.151  2004/09/18 10:52:48  micha
  convert LM_SCREENINIT message to interface method (integrated with TWidgetSet.AppInit(var ScreenInfo)

  Revision 1.150  2004/09/14 18:02:44  mattias
  made TCanvas methods virtual

  Revision 1.149  2004/09/14 10:23:44  mattias
  implemented finding DefineProperties in registered TPersistent, implemented auto commenting of missing units for Delphi unit conversion

  Revision 1.148  2004/09/13 15:33:07  micha
  rename AutoReDraw to AutoRedraw, as "Re" is not a word

  Revision 1.147  2004/09/12 13:11:50  micha
  convert LM_GETPIXEL and LM_SETPIXEL to interface methods (of twidgetset, DCGetPixel and DCSetPixel)

  Revision 1.146  2004/09/11 10:19:07  mattias
  implemented TBitmap.LoadFromDevice

  Revision 1.145  2004/09/04 22:24:16  mattias
  added default values for compiler skip options and improved many parts of synedit for UTF8

  Revision 1.144  2004/08/18 13:12:05  mattias
  synedit UTF8 support started   by Mazen

  Revision 1.143  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.142  2004/08/11 22:05:07  mattias
  fixed brush handle cache size

  Revision 1.141  2004/08/11 21:10:30  mattias
  implemented TBrushHandleCache

  Revision 1.140  2004/08/11 20:57:09  mattias
  moved intfstrconsts.pp to lclstrconsts.pas, implemented TPenHandleCache

  Revision 1.139  2004/05/25 21:50:32  mattias
  TCustomNotebook now allows pageclass descendents

  Revision 1.138  2004/05/23 21:30:10  mattias
  added build date to About box  from vincent

  Revision 1.137  2004/05/07 08:07:57  mattias
  ifdefd UseSimpleJpeg

  Revision 1.136  2004/05/06 16:25:22  mazen
  + jpeg loading support

  Revision 1.135  2004/04/29 18:08:17  mattias
  fixed 1.0.10 compilation

  Revision 1.134  2004/04/12 22:36:29  mattias
  made TIcon more independent of TBitmap  from Colin

  Revision 1.133  2004/04/10 14:20:20  mattias
  fixed saving findtext  from vincent

  Revision 1.132  2004/04/10 00:11:16  mattias
  added basic TIcon reading  from Colin

  Revision 1.131  2004/04/03 16:47:46  mattias
  implemented converting gdkbitmap to RawImage mask

  Revision 1.130  2004/03/28 12:49:22  mattias
  implemented mask merge and extraction for raw images

  Revision 1.129  2004/03/22 19:10:04  mattias
  implemented icons for TPage in gtk, mask for TCustomImageList

  Revision 1.128  2004/03/06 17:12:18  mattias
  fixed CreateBrushIndirect

  Revision 1.127  2004/03/06 15:37:43  mattias
  fixed FreeDC

  Revision 1.126  2004/03/03 23:35:55  mattias
  accelerated TActionList editor  from Radek

  Revision 1.125  2004/03/02 22:37:36  mattias
  clean up for TBitmapImage sharing

  Revision 1.124  2004/03/01 18:02:00  mattias
  fixed IsFileExtensionSupported

  Revision 1.123  2004/02/29 22:51:54  mattias
  added jpeg example

  Revision 1.122  2004/02/29 10:34:00  mattias
  fixed bmp reader reader skipping header

  Revision 1.121  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.120  2004/02/24 19:40:17  mattias
  TBitmap can now read form streams without knowing the size

  Revision 1.119  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.117  2004/02/21 15:17:43  micha
  fixed: compiler doesn't know what function to call using ver 1.9.2

  Revision 1.116  2004/02/21 01:01:03  mattias
  added uninstall popupmenuitem to package graph explorer

  Revision 1.115  2004/02/19 05:07:16  mattias
  CreateBitmapFromRawImage now creates mask only if needed

  Revision 1.114  2004/02/10 00:38:43  mattias
  deactivated fpImage or fpc 1.0.10

  Revision 1.113  2004/02/10 00:21:04  mattias
  fixed TImage background for transparent images

  Revision 1.112  2004/02/07 20:25:37  mattias
  fixed saving custom TBitBtn kind

  Revision 1.111  2004/02/05 16:28:38  mattias
  fixed unsharing TBitmap

  Revision 1.110  2004/02/04 22:17:09  mattias
  removed workaround VirtualCreate

  Revision 1.109  2004/02/04 12:48:17  mattias
  added CLX colors

  Revision 1.108  2004/02/03 08:54:09  mattias
  Frame3D rect now var again

  Revision 1.107  2004/02/02 22:01:51  mattias
  fpImage is now used as default, deactivate it with -dDisableFPImage

  Revision 1.106  2004/02/02 19:13:31  mattias
  started reading TImageList in Delphi format

  Revision 1.105  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.104  2004/01/05 01:18:15  mattias
  implemented Double Buffering for synedit and deactivated multi buffering in TGTKObject.ExtTextOut

  Revision 1.103  2004/01/03 23:14:59  mattias
  default font can now change height and fixed gtk crash

  Revision 1.102  2003/12/26 09:55:08  mattias
  fixed range check type conversion

  Revision 1.101  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.100  2003/12/02 12:25:17  micha
  try: gdi memory leak fix for pen

  Revision 1.99  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.98  2003/11/25 08:59:01  mattias
  fixed a few more black colors

  Revision 1.97  2003/11/22 17:22:14  mattias
  moved TBevelCut to controls.pp

  Revision 1.96  2003/11/03 16:57:47  peter
    * change $ifdef ver1_1 to $ifndef ver1_0 so it works also with
      fpc 1.9.x

  Revision 1.95  2003/10/30 21:26:23  mattias
  removed some hints

  Revision 1.94  2003/10/26 17:34:41  micha
  new interface method to attach a menu to window

  Revision 1.93  2003/10/15 20:33:36  ajgenius
  add csForm, start fixing Style matching for syscolors and fonts

  Revision 1.92  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.91  2003/09/12 14:59:43  mattias
  added searching for fpImage reader/writer

  Revision 1.90  2003/09/10 19:15:15  mattias
  implemented copying graphics from/to clipboard

  Revision 1.89  2003/09/08 13:07:17  mattias
  TBitmap now uses fpImage for writing bitmaps

  Revision 1.88  2003/09/08 12:21:48  mattias
  added fpImage reader/writer hooks to TBitmap

  Revision 1.87  2003/09/05 21:27:28  mattias
  fixed types

  Revision 1.86  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.85  2003/09/02 16:08:19  mattias
  implemented TPortableNetworkGraphic reading

  Revision 1.84  2003/09/02 15:12:21  mattias
  TBitmap.Assign now shares image data

  Revision 1.83  2003/08/25 16:43:32  mattias
  moved many graphics types form graphtype.pp to graphics.pp

  Revision 1.82  2003/08/20 17:03:47  mattias
  implemented TPixmap and TPortableNetworkGraphic with fpImage

  Revision 1.81  2003/08/19 12:23:23  mattias
  moved types from graphtype.pp back to graphics.pp

  Revision 1.80  2003/08/18 19:24:18  mattias
  fixed TCanvas.Pie

  Revision 1.79  2003/07/20 06:27:19  mattias
  fixed GetWindowRelativePosition

  Revision 1.78  2003/07/04 22:06:49  mattias
  implemented interface graphics

  Revision 1.77  2003/07/04 08:54:53  mattias
  implemented 16bit rawimages for gtk

  Revision 1.76  2003/07/01 15:37:03  mattias
  fixed exception handling

  Revision 1.75  2003/07/01 09:29:51  mattias
  attaching menuitems topdown

  Revision 1.74  2003/06/30 17:25:26  mattias
  fixed parsing of with do try finally end

  Revision 1.73  2003/06/30 16:31:04  mattias
  fixed find declaration of with A,B do C; statements

  Revision 1.72  2003/06/30 14:58:29  mattias
  implemented multi file add to package editor

  Revision 1.71  2003/06/30 10:09:46  mattias
  fixed Get/SetPixel for DC without widget

  Revision 1.70  2003/06/25 10:38:28  mattias
  implemented saving original stream of TBitmap

  Revision 1.69  2002/08/18 04:57:01  mattias
  fixed csDashDot

  Revision 1.68  2003/06/13 21:08:53  mattias
  moved TColorButton to dialogs.pp

  Revision 1.67  2003/05/19 08:16:32  mattias
  fixed allocation of dc backcolor

  Revision 1.66  2003/04/02 13:23:23  mattias
  fixed default font

  Revision 1.65  2003/03/12 14:39:29  mattias
  fixed clipping origin in stretchblt

  Revision 1.64  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.63  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.62  2003/02/06 06:39:02  mattias
  implemented TCanvas.Refresh

  Revision 1.61  2003/01/28 17:04:34  mattias
  renamed one Rect

  Revision 1.60  2003/01/28 12:45:04  mattias
  fixed broken cvs

  Revision 1.59  2003/01/27 13:49:16  mattias
  reduced speedbutton invalidates, added TCanvas.Frame

  Revision 1.58  2002/12/28 17:43:43  mattias
  fixed FindControl and searching overloaded procs

  Revision 1.57  2002/12/16 12:12:50  mattias
  fixes for fpc 1.1

  Revision 1.56  2002/12/12 17:47:44  mattias
  new constants for compatibility

  Revision 1.55  2002/11/09 15:02:06  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.54  2002/10/27 11:51:34  lazarus
  MG: fixed memleaks

  Revision 1.53  2002/10/26 15:15:46  lazarus
  MG: broke LCL<->interface circles

  Revision 1.52  2002/10/25 10:42:08  lazarus
  MG: broke minor circles

  Revision 1.51  2002/10/24 10:05:51  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.50  2002/10/14 06:39:12  lazarus
  MG: fixed storing TFont.Size

  Revision 1.49  2002/10/08 16:15:43  lazarus
  MG: fixed small typos and accelerated TDynHashArray.Contains

  Revision 1.48  2002/10/01 18:00:03  lazarus
  AJ: Initial TUpDown, minor property additions to improve reading Delphi created forms.

  Revision 1.47  2002/09/27 20:52:21  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.46  2002/09/19 19:56:13  lazarus
  MG: accelerated designer drawings

  Revision 1.45  2002/09/18 17:07:24  lazarus
  MG: added patch from Andrew

  Revision 1.44  2002/09/12 05:56:15  lazarus
  MG: gradient fill, minor issues from Andrew

  Revision 1.43  2002/09/10 06:49:18  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.42  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.41  2002/09/03 08:07:18  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

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
