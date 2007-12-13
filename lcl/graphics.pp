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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  SysUtils, Math, Types, Classes, Contnrs, FPCAdds,
  FPImgCmn, FPImage, FPCanvas,
  FPReadPNG, FPWritePNG,   // png support
  FPReadBMP, FPWriteBMP,   // bmp support
  FPReadPNM, FPWritePNM,   // png support
  FPReadJpeg, FPWriteJpeg, // jpg support
  IntfGraphics,
  AvgLvlTree,
  LCLStrConsts, LCLType, LCLProc, LMessages, LCLIntf, LResources, LCLResCache,
  GraphType, GraphMath, InterfaceBase, WSReferences;

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
    
    RightToLeft: Boolean;    //For RightToLeft text reading (Text Direction)
  end;

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
  psInsideframe = FPCanvas.psInsideframe;

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
  TProgressStage = TFPImgProgressStage;
  TProgressEvent = TFPImgProgressEvent;

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

  clMoneyGreen = TColor($C0DCC0);
  clSkyBlue    = TColor($F0CAA6);
  clMedGray    = TColor($A4A0A0);

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
  TJpegImage = class;               // jpg

  { TGraphicsObject
    In Delphi VCL this is the ancestor of TFont, TPen and TBrush.
    Since FPC 2.0 the LCL uses TFPCanvasHelper as ancestor. }

  TGraphicsObject = class(TPersistent)
  private
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    procedure DoChange(var Msg); message LM_CHANGED;
  protected
    procedure Changing; dynamic;
    procedure Changed; dynamic;
    procedure Lock;
    procedure UnLock;
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

  TFont = class(TFPCustomFont)
  private
    FCanUTF8: boolean;
    FCanUTF8Valid: boolean;
    FIsMonoSpace: boolean;
    FIsMonoSpaceValid: boolean;
    FPitch: TFontPitch;
    FStyle: TFontStylesBase;
    FCharSet: TFontCharSet;
    FPixelsPerInch: Integer;
    FUpdateCount: integer;
    FChanged: boolean;
    FFontHandleCached: boolean;
    FColor: TColor;
    FHeight: integer; // FHeight = -(FSize * FPixelsPerInch) div 72
    FReference: TWSFontReference;
    procedure FreeReference;
    function GetCanUTF8: boolean;
    function  GetHandle: HFONT;
    procedure GetData(var FontData: TFontData);
    function GetIsMonoSpace: boolean;
    function GetReference: TWSFontReference;
    function IsNameStored: boolean;
    procedure SetData(const FontData: TFontData);
    procedure SetHandle(const Value: HFONT);
    procedure ReferenceNeeded;
  protected
    function  GetCharSet: TFontCharSet;
    function  GetHeight: Integer;
    function  GetName: string;
    function  GetPitch: TFontPitch;
    function  GetSize: Integer;
    function  GetStyle: TFontStyles;
    procedure Changed; override;
    procedure DoAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure DoDeAllocateResources; override;
    procedure SetCharSet(const AValue: TFontCharSet);
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetColor(Value: TColor);
    procedure SetFlags(Index: integer; AValue: boolean); override;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetHeight(value: Integer);
    procedure SetName(AValue: string); override;
    procedure SetPitch(Value: TFontPitch);
    procedure SetSize(AValue: integer); override;
    procedure SetStyle(Value: TFontStyles);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Assign(const ALogFont: TLogFont);
    procedure BeginUpdate;
    procedure EndUpdate;
    function HandleAllocated: boolean;
    property Handle: HFONT read GetHandle write SetHandle; deprecated;
    function IsDefault: boolean;
    function IsEqual(AFont: TFont): boolean; virtual;
    property IsMonoSpace: boolean read GetIsMonoSpace;
    procedure SetDefault;
    property CanUTF8: boolean read GetCanUTF8;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
    property Reference: TWSFontReference read GetReference;
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

  TPenHandleCache = class(TBlockResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
  end;

  TPen = class(TFPCustomPen)
  private
    FColor: TColor;
    FPenHandleCached: boolean;
    FReference: TWSPenReference;
    procedure FreeReference;
    function GetHandle: HPEN;
    function GetReference: TWSPenReference;
    procedure ReferenceNeeded;
    procedure SetHandle(const Value: HPEN);
  protected
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetColor(Value: TColor);
    procedure SetMode(Value: TPenMode); override;
    procedure SetStyle(Value: TPenStyle); override;
    procedure SetWidth(value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Handle: HPEN read GetHandle write SetHandle; deprecated;
    property Reference: TWSPenReference read GetReference;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property Mode default pmCopy;
    property Style default psSolid;
    property Width default 1;
  end;

  { TBrush }

  TBrushHandleCache = class(TBlockResourceCache)
  protected
    procedure RemoveItem(Item: TResourceCacheItem); override;
  public
    constructor Create;
  end;

  TBrush = class(TFPCustomBrush)
  private
    FBrushHandleCached: boolean;
    FColor: TColor;
    FBitmap: TBitmap;
    FReference: TWSBrushReference;
    procedure FreeReference;
    function GetHandle: HBRUSH;
    function GetReference: TWSBrushReference;
    procedure ReferenceNeeded;
    procedure SetHandle(const Value: HBRUSH);
    procedure DoChange(var Msg); message LM_CHANGED;
  protected
    procedure DoAllocateResources; override;
    procedure DoDeAllocateResources; override;
    procedure DoCopyProps(From: TFPCanvasHelper); override;
    procedure SetColor(const NewColor: TColor; const NewFPColor: TFPColor); virtual;
    procedure SetFPColor(const AValue: TFPColor); override;
    procedure SetBitmap(Value: TBitmap);
    procedure SetColor(Value: TColor);
    procedure SetStyle(Value: TBrushStyle); override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property Handle: HBRUSH read GetHandle write SetHandle; deprecated;
    property Reference: TWSBrushReference read GetReference;
  published
    property Color: TColor read FColor write SetColor default clWhite;
    property Style default bsSolid;
  end;


  { TRegion }

  TRegionData = record
    Reference: TWSRegionReference;
    Rect: TRect;
    {Polygon Region Info - not used yet}
    Polygon: PPoint;    //Polygon Points
    NumPoints: Longint; //Number of Points
    Winding: Boolean;   //Use Winding mode
  end;

  TRegion = class(TGraphicsObject)
  private
    FRegionData: TRegionData;
    procedure FreeReference;
    function GetReference: TWSRegionReference;
    function GetHandle: HRGN;
    procedure ReferenceNeeded;
    procedure SetHandle(const Value: HRGN);
  protected
    procedure SetClipRect(value: TRect);
    function GetClipRect: TRect;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property ClipRect: TRect read GetClipRect write SetClipRect;
    property Handle: HRGN read GetHandle write SetHandle; deprecated;
    property Reference: TWSRegionReference read GetReference;
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
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect;
      const Msg: string); dynamic;
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
    class function GetFPReaderForFileExt(
      const FileExtension: string): TFPCustomImageReaderClass; virtual;
    class function GetFPWriterForFileExt(
      const FileExtension: string): TFPCustomImageWriterClass; virtual;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; virtual;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; virtual;
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
      Jpeg - Returns a jpeg. If the contents is not already a jpeg, the
        contents are thrown away and a blank jpeg (TJPegImage) is
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
    function GetIcon: TIcon;
    function GetJpeg: TJpegImage;
    function GetPNG: TPortableNetworkGraphic;
    function GetPNM: TPortableAnyMapGraphic;
    function GetPixmap: TPixmap;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure ReadData(Stream: TStream);
    procedure SetBitmap(Value: TBitmap);
    procedure SetIcon(Value: TIcon);
    procedure SetJpeg(Value: TJpegImage);
    procedure SetPNG(const AValue: TPortableNetworkGraphic);
    procedure SetPNM(const AValue: TPortableAnyMapGraphic);
    procedure SetPixmap(Value: TPixmap);
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
    property Icon: TIcon read GetIcon write SetIcon;
    property Jpeg: TJpegImage read GetJpeg write SetJpeg;
    property Pixmap: TPixmap read GetPixmap write SetPixmap;
    property PNG: TPortableNetworkGraphic read GetPNG write SetPNG;
    property PNM: TPortableAnyMapGraphic read GetPNM write SetPNM;
    property Graphic: TGraphic read FGraphic write SetGraphic;
    //property PictureAdapter: IChangeNotifier read FNotify write FNotify;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;


  EInvalidGraphic = class(Exception);
  EInvalidGraphicOperation = class(Exception);

type
  TGradientDirection = (gdVertical,     // Fill vertical
                        gdHorizontal);  // Fill Horizontal

  { TCanvas }

  TCanvas = class(TFPCustomCanvas)
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
    FSavedHandleStates: TFPList;
    procedure BrushChanged(ABrush: TObject);
    procedure FontChanged(AFont: TObject);
    procedure PenChanged(APen: TObject);
    procedure RegionChanged(ARegion: TObject);
    function GetHandle: HDC;
    procedure SetAutoRedraw(Value: Boolean); virtual;
    procedure SetLazFont(value: TFont);
    procedure SetLazPen(value: TPen);
    procedure SetLazBrush(value: TBrush);
    procedure SetRegion(Value: TRegion);
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
  protected
    function GetClipRect: TRect; override;
    Function GetPixel(X,Y: Integer): TColor; virtual;
    procedure CreateBrush; virtual;
    procedure CreateFont; virtual;
    procedure CreateHandle; virtual;
    Procedure CreatePen; virtual;
    Procedure CreateRegion; virtual;
    procedure DeselectHandles; virtual;
    procedure PenChanging(APen: TObject); virtual;
    procedure FontChanging(APen: TObject); virtual;
    procedure BrushChanging(APen: TObject); virtual;
    procedure RegionChanging(APen: TObject); virtual;
    procedure RealizeAutoRedraw; virtual;
    procedure RequiredState(ReqState: TCanvasState); virtual;
    procedure SetHandle(NewHandle: HDC); virtual;
    procedure SetInternalPenPos(const Value: TPoint); virtual;
    Procedure SetPixel(X,Y: Integer; Value: TColor); virtual;
    procedure FreeHandle;virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    procedure Refresh; virtual;
    procedure Changing; virtual;
    procedure Changed; virtual;
    procedure SaveHandleState; virtual;
    procedure RestoreHandleState; virtual;

    // extra drawing methods (there are more in the ancestor TFPCustomCanvas)
    procedure Arc(ALeft, ATop, ARight, ABottom, angle1, angle2: Integer); virtual;
    procedure Arc(ALeft, ATop, ARight, ABottom, SX, SY, EX, EY: Integer); virtual;
    //Procedure BrushCopy(Dest: TRect; InternalImages: TBitmap; Src: TRect;
    //                    TransparentColor: TColor); virtual;
    procedure Chord(x1, y1, x2, y2,
                    StartAngle16Deg, EndAngle16Deg: Integer); virtual;
    procedure Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer); virtual;
    procedure CopyRect(const Dest: TRect; SrcCanvas: TCanvas;
                       const Source: TRect); virtual;
    procedure Draw(X,Y: Integer; SrcGraphic: TGraphic); virtual;
    procedure DrawFocusRect(const ARect: TRect); virtual;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); virtual;
    procedure Ellipse(const ARect: TRect); // already in fpcanvas
    procedure Ellipse(x1, y1, x2, y2: Integer); virtual; // already in fpcanvas
    procedure FillRect(const ARect: TRect); virtual;
    procedure FillRect(X1,Y1,X2,Y2: Integer);
    procedure FloodFill(X, Y: Integer; FillColor: TColor;
                        FillStyle: TFillStyle); virtual;
    procedure Frame3d(var ARect: TRect; const FrameWidth: integer;
                      const Style: TGraphicsBevelCut); virtual;
    procedure Frame(const ARect: TRect); virtual; // border using pen
    procedure Frame(X1,Y1,X2,Y2: Integer);     // border using pen
    procedure FrameRect(const ARect: TRect); virtual; // border using brush
    procedure FrameRect(X1,Y1,X2,Y2: Integer); // border using brush
    procedure GradientFill(ARect: TRect; AStart, AStop: TColor; ADirection: TGradientDirection);
    procedure Line(X1,Y1,X2,Y2: Integer); virtual; // short for MoveTo();LineTo(); // already in fpcanvas
    procedure Line(const p1,p2: TPoint);
    procedure Line(const Points: TRect);
    procedure LineTo(X1,Y1: Integer); virtual; // already in fpcanvas
    procedure MoveTo(X1,Y1: Integer); virtual; // already in fpcanvas
    procedure RadialPie(x1, y1, x2, y2,
                        StartAngle16Deg, EndAngle16Deg: Integer); virtual;
    procedure Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2,
                  StartX,StartY,EndX,EndY: Integer); virtual;
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False); virtual;
    procedure PolyBezier(const Points: array of TPoint;
                         Filled: boolean = False;
                         Continuous: boolean = False);
    procedure Polygon(const Points: array of TPoint;
                      Winding: Boolean;
                      StartIndex: Integer = 0;
                      NumPts: Integer = -1);
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean = False); virtual;
    procedure Polygon(const Points: array of TPoint); // already in fpcanvas
    procedure Polyline(const Points: array of TPoint;
                       StartIndex: Integer;
                       NumPts: Integer = -1);
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
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property Handle: HDC read GetHandle write SetHandle;
    property TextStyle: TTextStyle read FTextStyle write FTextStyle;
  published
    property AutoRedraw: Boolean read FAutoRedraw write SetAutoRedraw;
    property Brush: TBrush read FBrush write SetLazBrush;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode default cmSrcCopy;
    property Font: TFont read FFont write SetLazFont;
    property Height : integer read GetHeight;
    property Pen: TPen read FPen write SetLazPen;
    property Region: TRegion read FRegion write SetRegion;
    property Width : integer read GetWidth;
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
    bnIcon,
    bnCursor
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
    FBitmapCanvas: TCanvas; // current canvas selected into
    FSaveStream: TMemoryStream;
    FSaveStreamClass: TFPCustomImageWriterClass;
    FSaveStreamType: TBitmapNativeType;
  protected
    procedure FreeHandle; override;
    procedure FreeMaskHandle;
    procedure FreePalette;
    function ReleaseHandle: HBITMAP;
    function ReleaseMaskHandle: HBITMAP;
    function ReleasePalette: HPALETTE;
    function IsEmpty: boolean;
    function GetPixelFormat: TPixelFormat;
  public
    FDIB: TDIBSection;
    destructor Destroy; override;
    function HandleAllocated: boolean; override;
    function GetHandleType: TBitmapHandleType;
    property BitmapCanvas: TCanvas read FBitmapCanvas write FBitmapCanvas;
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
    FPixelFormat: TPixelFormat;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    FInternalState: TBitmapInternalState;
    procedure FreeCanvasContext;
    function GetCanvas: TCanvas;
    procedure CreateCanvas;
    procedure CreateMask(AColor: TColor = clDefault);
    function GetMonochrome: Boolean;
    procedure SetHandle(Value: HBITMAP);
    procedure SetMaskHandle(NewMaskHandle: HBITMAP);
    function GetHandleType: TBitmapHandleType;
    procedure SetHandleType(Value: TBitmapHandleType); virtual;
    procedure SetMonochrome(const AValue: Boolean);
    procedure SetPixelFormat(const AValue: TPixelFormat);
    procedure SetTransparentColor(const AValue: TColor);
    procedure UpdatePixelFormat;
  protected
    procedure Changed(Sender: TObject); override;
    procedure Changing(Sender: TObject); virtual;
    procedure ChangingAll(Sender: TObject);
    procedure Draw(DestCanvas: TCanvas; const DestRect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    function GetHandle: HBITMAP; virtual;
    function GetMaskHandle: HBITMAP; virtual;
    function GetTransparent: Boolean; override;
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
    procedure InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader); virtual;
    procedure InitFPImageWriter(IntfImg: TLazIntfImage; ImgWriter: TFPCustomImageWriter); virtual;
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
    procedure CreateFromBitmapHandles(ABitmap, AMask: HBitmap; const ARect: TRect);
    function LazarusResourceTypeValid(const ResourceType: string): boolean; virtual;
    procedure LoadFromDevice(DC: HDC); virtual;
    procedure LoadFromStream(Stream: TStream); override;
    procedure LoadFromLazarusResource(const ResName: String); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); virtual;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); virtual;
    procedure LoadFromMimeStream(Stream: TStream; const MimeType: string); override;
    procedure SaveToFile(const Filename: string); override;
    procedure GetSupportedSourceMimeTypes(List: TStrings); override;
    function GetDefaultMimeType: string; override;
    class function GetFileExtensions: string; override;
    procedure LoadFromXPMFile(const Filename: String);
    procedure LoadFromIntfImage(IntfImage: TLazIntfImage);
    procedure Mask(ATransparentColor: TColor);
    procedure SetHandles(ABitmap, AMask: HBITMAP);
    procedure SaveToStream(Stream: TStream); override;
    procedure ReadStream(Stream: TStream; UseSize: boolean; Size: Longint); virtual;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean); virtual;
    function ReleaseHandle: HBITMAP;
    function ReleaseMaskHandle: HBITMAP;
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
    procedure CreateIntfImage(var IntfImage: TLazIntfImage);
    function CreateIntfImage: TLazIntfImage;
    function CanReadGraphicStreams(AClass: TFPCustomImageWriterClass): boolean; virtual;
  public
    property Canvas: TCanvas read GetCanvas;
    property Handle: HBITMAP read GetHandle write SetHandle;
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
    property PixelFormat: TPixelFormat read FPixelFormat write SetPixelFormat default pfDevice;
    // property ScanLine[Row: Integer]: Pointer; -> Use TLazIntfImage for such things
    property TransparentColor: TColor read FTransparentColor
                                      write SetTransparentColor default clDefault;
    property TransparentMode: TTransparentMode read FTransparentMode
                                        write SetTransparentMode default tmAuto;
    {$IFDEF DebugBitmap}
    DebugEnabled: boolean;
    {$ENDIF}
  end;


  { TPixmap }

  TPixmap = class(TBitmap)
  public
    procedure SaveToFile(const Filename: string); override;
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
  protected
    procedure InitFPImageWriter(IntfImg: TLazIntfImage; ImgWriter: TFPCustomImageWriter); override;
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
    procedure InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader); override;
  public
    class function GetFileExtensions: string; override;
    property Bitmaps: TObjectList read FBitmaps;
    destructor Destroy; override;
    procedure AddBitmap(Bitmap: TBitmap); { Note that Ownership passes to TIcon }
  end;

  { TCursorImage }
  TCursorImage = class(TIcon)
  private
    FHotSpot: TPoint;
    FCursorHandle: HCURSOR;
    FOwnHandle: Boolean;
  protected
    function GetCursorHandle: HCURSOR;
    procedure CursorHandleNeeded;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetFileExtensions: string; override;
    function LazarusResourceTypeValid(const ResourceType: string): boolean; override;
    function ReleaseCursorHandle: HCURSOR;
    property HotSpot: TPoint read FHotSpot write FHotSpot;
    property CursorHandle: HCURSOR read GetCursorHandle;
  end;
  
  { TJpegImage }

  TJPEGQualityRange = TFPJPEGCompressionQuality;
  TJPEGPerformance = TJPEGReadPerformance;

  TJPEGImage = class(TFPImageBitmap)
  private
    FGrayScale: Boolean;
    FPerformance: TJPEGPerformance;
    FProgressiveEncoding: boolean;
    FQuality: TJPEGQualityRange;
  protected
    procedure InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader); override;
    procedure FinalizeFPImageReader(ImgReader: TFPCustomImageReader); override;
    procedure InitFPImageWriter(IntfImg: TLazIntfImage; ImgWriter: TFPCustomImageWriter); override;
  public
    constructor Create; override;
    class function GetFileExtensions: string; override;
    class function GetDefaultFPReader: TFPCustomImageReaderClass; override;
    class function GetDefaultFPWriter: TFPCustomImageWriterClass; override;
  public
    property CompressionQuality: TJPEGQualityRange read FQuality write FQuality;
    property GrayScale: Boolean read FGrayScale;
    property ProgressiveEncoding: boolean read FProgressiveEncoding;
    property Performance: TJPEGPerformance read FPerformance write FPerformance;
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
function InvertColor(AColor: TColor): TColor;
function DecColor(AColor: TColor; AQuantity: Byte): TColor;

Function Blue(rgb: TColor): BYTE;
Function Green(rgb: TColor): BYTE;
Function Red(rgb: TColor): BYTE;
function RGBToColor(R, G, B: Byte): TColor;
procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte);
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
procedure FontNameToPangoFontDescStr(const LongFontName: string;
  var aFamily,aStyle:String; var aSize: Integer);

// graphics
type
  TOnLoadGraphicFromClipboardFormat =
    procedure(Dest: TGraphic; ClipboardType: TClipboardType;
              FormatID: TClipboardFormat);
  TOnSaveGraphicToClipboardFormat =
    procedure(Src: TGraphic; ClipboardType: TClipboardType;
              FormatID: TClipboardFormat);

var
  OnLoadSaveClipBrdGraphicValid: boolean = false;
  OnLoadGraphicFromClipboardFormat: TOnLoadGraphicFromClipboardFormat=nil;
  OnSaveGraphicToClipboardFormat: TOnSaveGraphicToClipboardFormat=nil;

function TestStreamBitmapNativeType(const AStream: TStream): TBitmapNativeType;
function TestStreamIsBMP(const AStream: TStream): boolean;
function TestStreamIsXPM(const AStream: TStream): boolean;
function TestStreamIsIcon(const AStream: TStream): boolean;
function TestStreamIsCursor(const AStream: TStream): boolean;

function XPMToPPChar(const XPM: string): PPChar;
function LazResourceXPMToPPChar(const ResourceName: string): PPChar;
function ReadXPMFromStream(Stream: TStream; Size: integer): PPChar;
function ReadXPMSize(XPM: PPChar; var Width, Height, ColorCount: integer
                     ): boolean;
function LoadCursorFromLazarusResource(ACursorName: String): HCursor;
function LoadBitmapFromLazarusResource(ResourceName: String): TBitmap;
function LoadBitmapFromLazarusResourceHandle(Handle: TLResource): TBitmap;

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

function DbgS(const Style: TFontStyles): string; overload;


procedure Register;

implementation

function DbgS(const Style: TFontStyles): string;

  procedure Add(const s: string);
  begin
    if Result<>'' then Result:=Result+',';
    Result:=Result+s;
  end;

begin
  Result:='';
  if fsBold in Style then Add('fsBold');
  if fsItalic in Style then Add('fsItalic');
  if fsStrikeOut in Style then Add('fsStrikeOut');
  if fsUnderline in Style then Add('fsUnderline');
  Result:='['+Result+']';
end;

function LoadCursorFromLazarusResource(ACursorName: String): HCursor;
var
  CursorImage: TCursorImage;
begin
  CursorImage := TCursorImage.Create;
  CursorImage.LoadFromLazarusResource(ACursorName);
  Result := CursorImage.ReleaseCursorHandle;
  CursorImage.Free;
end;

function LoadBitmapFromLazarusResourceHandle(Handle: TLResource): TBitmap;
var
  Stream: TLazarusResourceStream;
  GraphicClass: TGraphicClass;
begin
  Result := nil;
  Stream := nil;
  try
    Stream := TLazarusResourceStream.CreateFromHandle(Handle);
    GraphicClass := GetGraphicClassForFileExtension(Stream.Res.ValueType);
    if (GraphicClass <> nil) and (GraphicClass.InheritsFrom(TBitmap)) then
    begin
      Result := TBitmap(GraphicClass.Create);
      Result.LoadFromStream(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

function LoadBitmapFromLazarusResource(ResourceName: String): TBitmap;
var
  Stream: TLazarusResourceStream;
  GraphicClass: TGraphicClass;
begin
  Result := nil;
  Stream := nil;
  try
    Stream := TLazarusResourceStream.Create(ResourceName, nil);
    if (Stream.Res <> nil) then
    begin
      GraphicClass := GetGraphicClassForFileExtension(Stream.Res.ValueType);
      if (GraphicClass <> nil) and (GraphicClass.InheritsFrom(TBitmap)) then
      begin
        Result := TBitmap(GraphicClass.Create);
        Result.LoadFromStream(Stream);
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure Register;
begin
  RegisterClasses([TBitmap,TPixmap,TPortableNetworkGraphic,
                   TPortableAnyMapGraphic,TJpegImage,TPicture,
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
  public
    constructor Create(ABitmap: TBitmap);
    destructor Destroy; override;
  end;


{ Color mapping routines }

const
  Colors: array[0..112] of TIdentMapEntry = (
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
    (Value: clActiveHighlightedText; Name: 'clActiveHighlightedText'),
    (Value: clMoneyGreen; Name: 'clMoneyGreen'),
    (Value: clSkyBlue; Name: 'clSkyBlue'),
    (Value: clMedGray; Name: 'clMedGray')
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

function InvertColor(AColor: TColor): TColor;
var
  R, G, B: Integer;
begin
  R := AColor and $ff;
  G := (AColor shr 8) and $ff;
  B := (AColor shr 16) and $ff;

  if Abs($80 - R) + Abs($80 - G) + Abs($80 - B) < $140 then
  begin
    if R<$80 then
      R:=Min($ff,R+$a0)
    else
      R:=Max(0,R-$a0);
    if G<$80 then
      G:=Min($ff,G+$a0)
    else
      G:=Max(0,G-$a0);
    if B<$80 then
      B:=Min($ff,B+$a0)
    else
      B:=Max(0,B-$a0);
  end
  else
  begin
    R := $ff - R;
    G := $ff - G;
    B := $ff - B;
  end;
  
  Result := ((B and $ff) shl 16) or ((G and $ff) shl 8) or (R and $ff);
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

function RGBToColor(R, G, B: Byte): TColor;
begin
  Result := (B shl 16) or (G shl 8) or R;
end;

procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte);
begin
  Red := rgb and $000000ff;
  Green := (rgb shr 8) and $000000ff;
  Blue := (rgb shr 16) and $000000ff;
end;

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

class function TFPImageBitmap.GetFileExtensions: string;
begin
  Result:='';
end;

class function TFPImageBitmap.IsFileExtensionSupported(
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

class function TFPImageBitmap.GetFPReaderForFileExt(const FileExtension: string
  ): TFPCustomImageReaderClass;
begin
  if IsFileExtensionSupported(FileExtension) then
    Result:=GetDefaultFPReader
  else
    Result:=nil;
end;

class function TFPImageBitmap.GetFPWriterForFileExt(const FileExtension: string
  ): TFPCustomImageWriterClass;
begin
  if IsFileExtensionSupported(FileExtension) then
    Result:=GetDefaultFPWriter
  else
    Result:=nil;
end;

class function TFPImageBitmap.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result:=nil;
end;

class function TFPImageBitmap.GetDefaultFPWriter: TFPCustomImageWriterClass;
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
  CursorSignature: array [0..3] of char = #0#0#2#0;

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

function TestStreamIsCursor(const AStream: TStream): boolean;
var
  Signature: array[0..3] of char;
  ReadSize: Integer;
  OldPosition: TStreamSeekType;
begin
  OldPosition:=AStream.Position;
  ReadSize:=AStream.Read(Signature, SizeOf(Signature));
  Result:=(ReadSize=SizeOf(Signature)) and CompareMem(@Signature,@CursorSignature,4);
  AStream.Position:=OldPosition;
end;

procedure TIcon.ReadData(Stream: TStream);
var
  Size: longint;
  Position: TStreamSeekType;
begin
  Position := Stream.Position;
  Stream.Read(Size, 4); // Beware BigEndian and LowEndian sytems
  if CompareMem(@Size,@IconSignature,4) then begin
    // Assume Icon - stream without explicit size
    Stream.Position := Position;
    ReadStream(Stream, false, Size);
  end else begin
    Size := LEtoN(Size);
    ReadStream(Stream, true, Size);
  end;
end;

procedure TIcon.InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader);
begin
  inherited InitFPImageReader(IntfImg, ImgReader);
  if ImgReader is TLazReaderIcon then
    TLazReaderIcon(ImgReader).Icon := self;
end;

class function TIcon.GetFileExtensions: string;
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

{ TCursorImage }

class function TCursorImage.GetFileExtensions: string;
begin
  Result := 'cur';
end;

function TCursorImage.LazarusResourceTypeValid(const ResourceType: string): boolean;
begin
  Result := inherited LazarusResourceTypeValid(ResourceType) or
            (AnsiCompareText(ResourceType,'CUR')=0);
end;

function TCursorImage.ReleaseCursorHandle: HCURSOR;
begin
  Result := CursorHandle;
  FCursorHandle := 0;
end;

function TCursorImage.GetCursorHandle: HCURSOR;
begin
  CursorHandleNeeded;
  Result := FCursorHandle;
end;

procedure TCursorImage.CursorHandleNeeded;
var
  IconInfo: TIconInfo;
begin
  if FCursorHandle = 0 then
  begin
    IconInfo.fIcon := False;
    IconInfo.xHotspot := HotSpot.X;
    IconInfo.yHotSpot := HotSpot.Y;
    IconInfo.hbmMask := MaskHandle;
    IconInfo.hbmColor := Handle;
    FCursorHandle := WidgetSet.CreateCursor(@IconInfo);
  end;
end;

constructor TCursorImage.Create;
begin
  inherited Create;
  FHotSpot := Point(0, 0);
  FCursorHandle := 0;
  FOwnHandle := True;
end;

destructor TCursorImage.Destroy;
begin
  if (FCursorHandle <> 0) then
    WidgetSet.DestroyCursor(FCursorHandle);
  inherited Destroy;
end;

// ------------------------------------------------------------------
// Decrease the component RGBs of a color of the quantity' passed
//
// Color    : Color to decrease
// Quantity : Decrease quantity
// ------------------------------------------------------------------
function DecColor(AColor: TColor; AQuantity: Byte) : TColor;
var
  R, G, B : Byte;
begin
  RedGreenBlue(ColorToRGB(AColor), R, G, B);
  R := Max(0, Integer(R) - AQuantity);
  G := Max(0, Integer(G) - AQuantity);
  B := Max(0, Integer(B) - AQuantity);
  Result := RGBToColor(R, G, B);
end;

procedure InterfaceInit;
begin
  //debugln('Graphics.InterfaceInit');
  FontResourceCache:=TFontHandleCache.Create;
  PenResourceCache:=TPenHandleCache.Create;
  BrushResourceCache:=TBrushHandleCache.Create;
end;

procedure InterfaceFinal;
begin
  //debugln('Graphics.InterfaceFinal');
  FreeAndNil(FontResourceCache);
  FreeAndNil(PenResourceCache);
  FreeAndNil(BrushResourceCache);
end;

{ TJpegImage }

{ TJPEGImage }

procedure TJPEGImage.InitFPImageReader(IntfImg: TLazIntfImage; ImgReader: TFPCustomImageReader);
var
  JPEGReader: TFPReaderJPEG;
begin
  if ImgReader is TFPReaderJPEG then
  begin
    JPEGReader := TFPReaderJPEG(ImgReader);
    JPEGReader.Performance := Performance;
    JPEGReader.OnProgress := @Progress;
  end;
  inherited InitFPImageReader(IntfImg, ImgReader);
end;

procedure TJPEGImage.FinalizeFPImageReader(ImgReader: TFPCustomImageReader);
var
  JPEGReader: TFPReaderJPEG;
begin
  if ImgReader is TFPReaderJPEG then
  begin
    JPEGReader := TFPReaderJPEG(ImgReader);
    FProgressiveEncoding := JPEGReader.ProgressiveEncoding;
    FGrayScale := JPEGReader.GrayScale;
  end;
  inherited FinalizeFPImageReader(ImgReader);
end;

procedure TJPEGImage.InitFPImageWriter(IntfImg: TLazIntfImage; ImgWriter: TFPCustomImageWriter);
var
  JPEGWriter: TFPWriterJPEG;
begin
  if ImgWriter is TFPWriterJPEG then
  begin
    JPEGWriter := TFPWriterJPEG(ImgWriter);
    if JPEGWriter <> nil then
    begin
      JPEGWriter.ProgressiveEncoding := ProgressiveEncoding;
      JPEGWriter.CompressionQuality := CompressionQuality;
      JPEGWriter.OnProgress := @Progress;
    end;
  end;
  inherited InitFPImageWriter(IntfImg, ImgWriter);
end;

class function TJPEGImage.GetDefaultFPReader: TFPCustomImageReaderClass;
begin
  Result := TFPReaderJPEG;
end;

class function TJPEGImage.GetDefaultFPWriter: TFPCustomImageWriterClass;
begin
  Result := TFPWriterJPEG;
end;

constructor TJPEGImage.Create;
begin
  inherited Create;
  FPerformance := jpBestQuality;
  FProgressiveEncoding := False;
  FGrayScale := False;
  FQuality := 75;
end;

class function TJPEGImage.GetFileExtensions: string;
begin
  Result := 'jpg;jpeg';
end;

initialization
  RegisterIntegerConsts(TypeInfo(TColor), @IdentToColor, @ColorToIdent);
  RegisterIntegerConsts(TypeInfo(TFontCharset), @IdentToCharset, @CharsetToIdent);
  RegisterInterfaceInitializationHandler(@InterfaceInit);
  RegisterInterfaceFinalizationHandler(@InterfaceFinal);

finalization
  GraphicsFinalized:=true;
  OnLoadSaveClipBrdGraphicValid:=false;
  FreeAndNil(PicClipboardFormats);
  FreeAndNil(PicFileFormats);


end.
