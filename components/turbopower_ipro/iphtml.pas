{******************************************************************}
{*     IPHTML.PAS - HTML Browser and associated classes           *}
{******************************************************************}

{ $Id$ }

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Turbo Power Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * 09/29/2007  DefaultTypeFace and FixedTypeFace are enabled
 *             FactBAParag: Incremental factor for space between lines
 *               default value is 1,
 *               proof it with values of 0.5 = {... margin-top: 0.5em; margin-bottom: 0.5em; }
 *             Delphi: adjustments
 * 10/01/2007  TextWidth of an anchor (<a name="XXXX">), before = TextWidth (' ') now is only 1
 *             Delphi: adjustments (crush when TIpHtmlPanelH was run-time created)
 * 10/03/2007  Delphi: supports jpg, png, etc
 *
 * Contributor(s):
 *
 * adem baba <adembaba@users.sourceforge.net>
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

{off $DEFINE IP_LAZARUS_DBG}

unit IpHtml;

interface

uses
  //MemCheck,
  Types, contnrs,
  LCLType, LCLProc, LCLIntf, LResources, LMessages, LCLMemManager,
  LazStringUtils, LConvEncoding, LazUTF8, AvgLvlTree,
  Messages, SysUtils, Classes, Graphics, TypInfo,
  {$IFDEF UseGifImageUnit} //TODO all of this units not exists
    GifImage,
  {$ELSE}
    IpAnim,
    {$IFDEF AndersGIFImage }
      IpAnAGif,
    {$ENDIF}
    {$IFDEF ImageLibGIFImage }
      IpAnImgL,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF UsePNGGraphic}
    IpPNGImg,
  {$ENDIF}
  Controls, StdCtrls, ExtCtrls, Buttons, Forms, ClipBrd, Dialogs,
  IpConst, IpUtils, IpHtmlTypes, IpHtmlClasses, IpHtmlProp, IpMsg, 
  IpCSS, IpHtmlUtils, IpHtmlTabList;

type
  {Note: Some of the code below relies on the fact that
   the end tag (when present) immediately follows the start tag.}

{$I iphtmlgenerated.inc}

type
  TIpEnumItemsMethod = TLCLEnumItemsMethod;
  TIpHtmlPoolManager = class(TLCLNonFreeMemManager)
  public
    constructor Create(TheItemSize, MaxItems : DWord);
    function NewItm : Pointer;
  end;

  TIpHtml = class;
  TIpAbstractHtmlDataProvider = class;
  TIpHtmlNode = class;
  TIpHtmlNodeCore = class;
  TIpHtmlNodeBlock = class;
  TIpHtmlNodeAlignInline = class;

  { TIpHtmlBaseLayouter }

  TIpHtmlNodeIterator = procedure (ANode: TIpHtmlNode; AProps: TIpHtmlProps;
    var Done: Boolean);

  // Abstract base class for the HTML Layout engine
  TIpHtmlBaseLayouter = class
  protected
    FOwner : TIpHtmlNodeCore;
    FElementQueue : TFPList;
    FCurProps : TIpHtmlProps;
    FBlockMin, FBlockMax : Integer;
    function GetProps: TIpHtmlProps;
    procedure ProcessDuplicateLFs;
    procedure RemoveLeadingLFs;
  public
    FPageRect : TRect;
    constructor Create(AOwner: TIpHtmlNodeCore); virtual;
    destructor Destroy; override;
    procedure ClearWordList;
    // Used by TIpHtmlNodeBlock descendants: Layout, CalcMinMaxPropWidth, Render
    procedure Layout(RenderProps: TIpHtmlProps; TargetRect: TRect); virtual; abstract;
    procedure CalcMinMaxPropWidth(RenderProps: TIpHtmlProps;
      var aMin, aMax: Integer); virtual; abstract;
    procedure Render(RenderProps: TIpHtmlProps); virtual; abstract;
    procedure IterateParents(AProc: TIpHtmlNodeIterator);
  public
    property Props : TIpHtmlProps read GetProps;
  end;

  TIpHtmlBaseLayouterClass = class of TIpHtmlBaseLayouter;

  { TIpHtmlBaseTableLayouter }

    // Abstract base class for layout methods of a HTML table
  TIpHtmlBaseTableLayouter = class(TIpHtmlBaseLayouter)
  protected
    FMin, FMax : Integer;
    FTableWidth: Integer;
    FCellSpacing: Integer;
    FCellPadding: Integer;
    FRowSp: TIntArr; // dynamic flag used for row spanning
  public
    constructor Create(AOwner: TIpHtmlNodeCore); override;
    destructor Destroy; override;
    // Used by TIpHtmlNodeTABLE
    procedure ResetSize;
    procedure CalcMinMaxColTableWidth(RenderProps: TIpHtmlProps;
      var Min, Max: Integer); virtual; abstract;
    procedure CalcSize(ParentWidth: Integer; RenderProps: TIpHtmlProps); virtual; abstract;
    function GetColCount: Integer; virtual; abstract;
  end;

  TIpHtmlBaseTableLayouterClass = class of TIpHtmlBaseTableLayouter;

  TIpHtmlElement = record
    ElementType : TElementType;
    AnsiWord: string;
    IsBlank : Integer;
    SizeProp: TIpHtmlPropA;
    Size: TSize;
    WordRect2 : TRect;
    Props : TIpHtmlProps;
    Owner : TIpHtmlNode;
    LFHeight : Integer;  // Height of LineFeed elements
    IsSelected: boolean;
  end;
  PIpHtmlElement = ^TIpHtmlElement;

  TRectMethod = procedure(const R : TRect) of object;
  TIpHtmlNodeEnumProc = procedure(Node: TIpHtmlNode; const UserData: Pointer) of object;
  TIpHtmlNodeClass = class of TIpHtmlNode;

  {abstract base node}
  TIpHtmlNode = class(TPersistent)
  protected
    FOwner : TIpHtml;
    FParentNode : TIpHtmlNode;
    procedure ScreenLine(StartPoint, EndPoint: TPoint; const Width: Integer; const Color: TColor);
    procedure ScreenRect(R : TRect; const Color : TColor);
    procedure ScreenFrame(R : TRect; Raised: boolean);
    procedure ScreenPolygon(Points : array of TPoint; const Color : TColor);
    function PagePtToScreen(const Pt: TPoint): TPoint;
    procedure Enqueue; virtual;
    procedure EnqueueElement(const Entry: PIpHtmlElement); virtual;
    function ElementQueueIsEmpty: Boolean; virtual;
    procedure ReportDrawRects(M : TRectMethod); virtual;
    procedure ReportCurDrawRects(Owner: TIpHtmlNode; M : TRectMethod); virtual;
    procedure ReportMapRects(M : TRectMethod); virtual;
    procedure Invalidate; virtual;
    procedure InvalidateSize; virtual;
    procedure SubmitRequest; virtual;
    procedure ResetRequest; virtual;
    function GetHint: string; virtual;
    procedure CreateControl(Parent : TWinControl); virtual;
    procedure MakeVisible; virtual;
    procedure UnmarkControl; virtual;
    procedure HideUnmarkedControl; virtual;
    procedure EnumChildren(EnumProc: TIpHtmlNodeEnumProc; UserData: Pointer); virtual;
    procedure AppendSelection(var S : string; var Completed: Boolean); virtual;
    function GetMargin(AMargin: TIpHtmlElemMargin; ADefault: Integer): Integer; virtual;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    function ExpParentWidth: Integer; virtual;
    procedure ImageChange(NewPicture : TPicture); virtual;
    function PageRectToScreen(const Rect : TRect; var ScreenRect: TRect): Boolean;
    procedure GetAttributes(Target: TStrings; IncludeValues, IncludeBlanks: Boolean);
    procedure SetAttributeValue(const AttrName, NewValue: string);
    procedure SetProps(const RenderProps: TIpHtmlProps); virtual;
  public
    property Owner : TIpHtml read FOwner;
    property ParentNode : TIpHtmlNode read FParentNode;
  end;

  TIpHtmlNodeNv = class(TIpHtmlNode)
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure Invalidate; override;
    procedure InvalidateSize; override;
    procedure Enqueue; override;
  public
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  end;

  TIpHtmlNodeMulti = class(TIpHtmlNode)
  private
    FProps: TIpHtmlProps;
    FChildren : TFPList;
    function GetChildNode(Index: Integer): TIpHtmlNode;
    function GetChildCount: Integer;
  protected
    procedure ReportDrawRects(M : TRectMethod); override;
    procedure ReportMapRects(M : TRectMethod); override;
    procedure AppendSelection(var S : string; var Completed: Boolean); override;
    procedure EnumChildren(EnumProc: TIpHtmlNodeEnumProc; UserData: Pointer); override;
    function GetMargin(AMargin: TIpHtmlElemMargin; ADefault: Integer): Integer; override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Enqueue; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  public
    property ChildCount : Integer read GetChildCount;
    property ChildNode[Index : Integer] : TIpHtmlNode read GetChildNode;
    property Props : TIpHtmlProps read FProps;
  end;

  { TIpHtmlNodeCore }

  TIpHtmlNodeCore = class(TIpHtmlNodeMulti)
  private
    FDir: TIpHtmlDirection;
    FHoverPropsLookupDone: Boolean;
    FElementName: String;
    FStyle: string;
    FClassId: string;
    FTitle: string;
    FId: string;
  protected
    FAreaList: TFPList;
    FCombinedCSSProps: TCSSProps; // props from all matching CSS selectors plus inline CSS combined
    FHoverPropsRef: TCSSProps;    // props for :hover (this is only a cached reference, we don't own it)
    FInlineCSSProps: TCSSProps;   // props from the style attribute
    procedure AddArea(const R: TRect);
    procedure BuildAreaList; virtual;
    procedure ClearAreaList; virtual;
    function SelectCSSFont(const aFont: string): string;
    procedure ApplyCSSProps(const ACSSProps: TCSSProps; const props: TIpHtmlProps);
    function GetAlign: TIpHtmlAlign; virtual;
    function GetFontSizeFromCSS(CurrentFontSize:Integer; aFontSize: string):Integer;
    procedure SetAlign(const Value: TIpHtmlAlign); virtual;
    procedure SetId(const Value: string); virtual;
    property ElementName: String read FElementName write FElementName;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure LoadAndApplyCSSProps; virtual;
    procedure MakeVisible; override;
    procedure ParseBaseProps(aOwner : TIpHtml);
    property InlineCSS: TCSSProps read FInlineCSSProps write FInlineCSSProps;
    property Align: TIpHtmlAlign read GetAlign write SetAlign;
    property ClassId : string read FClassId write FClassId;
    property Dir : TIpHtmlDirection read FDir write FDir;
    property Id : string read FId write SetId;
    property Style : string read FStyle write FStyle;
    property Title : string read FTitle write FTitle;
  end;

  TIpHtmlNodeInline = class(TIpHtmlNodeCore)
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
    procedure Invalidate; override;
  end;

  TIpHtmlNodeAlignInline = class(TIpHtmlNodeInline)
  private
    FAlignment: TIpHtmlImageAlign;
  protected
    Element : PIpHtmlElement;
    procedure SetRect(TargetRect: TRect); virtual;
    procedure SetAlignment(const Value: TIpHtmlImageAlign);
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Draw(Block: TIpHtmlNodeBlock); virtual; abstract;
    procedure Enqueue; override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); virtual; abstract;
    function GetDim(ParentWidth: Integer): TSize; virtual; abstract;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlImageAlign read FAlignment write SetAlignment;
  end;

  TIpHtmlNodeControl = class(TIpHtmlNodeAlignInline)
  protected
    FControl : TWinControl;
    Shown : Boolean;
    FAlt: string;
    FDisabled: Boolean;
    procedure HideUnmarkedControl; override;
    procedure UnmarkControl; override;
    function AdjustFromCss: boolean;
    procedure SetDisabled(const AValue: Boolean); virtual;
    property Disabled: Boolean read FDisabled write SetDisabled default false;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure AddValues(NameList, ValueList : TStringList); virtual; abstract;
    procedure Draw(Block: TIpHtmlNodeBlock); override;
    function GetDim(ParentWidth: Integer): TSize; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    procedure CalcMinMaxWidth(var Min, Max: Integer); override;
    procedure Reset; virtual; abstract;
    function Successful: Boolean; virtual; abstract;
  public
    property Control: TWinControl read FControl;
    property Alt : string read FAlt write FAlt;
  end;

  // Used by TIpHtmlNodeBlock

  TWordInfo = record
    BaseX     : Integer;
    BOff      : Integer;
    CurAsc    : Integer;
    Sz        : TSize;
    VA        : TIpHtmlVAlign3;
    Hs        : Integer;
  end;
  PWordInfo = ^TWordInfo;

  TWordList = array[0..Pred(MAXWORDS)] of TWordInfo;
  PWordList = ^TWordList;

  { TIpHtmlNodeBlock }

  TIpHtmlNodeBlock = class(TIpHtmlNodeCore)
  private
    function GetPageRect: TRect;
  protected
    FLayouter : TIpHtmlBaseLayouter;
    FLastW, FLastH : Integer;
    FBackground : string;
    FBgColor : TColor;
    FTextColor : TColor;
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
    procedure CalcMinMaxPropWidth(RenderProps: TIpHtmlProps; var aMin, aMax: Integer); virtual;
    procedure Invalidate; override;
    function GetHeight(const RenderProps: TIpHtmlProps; const Width: Integer): Integer;
    procedure InvalidateSize; override;
    procedure ReportCurDrawRects(aOwner: TIpHtmlNode; M : TRectMethod); override;
    procedure AppendSelection(var S : string; var Completed: Boolean); override;
    procedure SetBackground(const AValue: string);
    procedure SetBgColor(const AValue: TColor);
    procedure SetTextColor(const AValue: TColor);
  public
    constructor Create(ParentNode : TIpHtmlNode; LayouterClass: TIpHtmlBaseLayouterClass); overload;
    constructor Create(ParentNode : TIpHtmlNode); overload;
    destructor Destroy; override;
    procedure Layout(RenderProps: TIpHtmlProps; const TargetRect : TRect); virtual;
    procedure Render(RenderProps: TIpHtmlProps); virtual;
    function Level0: Boolean;
    procedure LoadAndApplyCSSProps; override;
  public
    property Layouter : TIpHtmlBaseLayouter read FLayouter;
    property PageRect : TRect read GetPageRect;
    property Background : string read FBackground write SetBackground;
    property BgColor : TColor read FBgColor write SetBgColor;
    property TextColor : TColor read FTextColor write SetTextColor;
  end;

  TIpHtmlNodeHEAD = class(TIpHtmlNodeMulti)
  private
    FProfile: string;
    FLang: string;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Lang : string read FLang write FLang;
    property Profile : string read FProfile write FProfile;
  end;

  TIpHtmlNodeSTYLE = class(TIpHtmlNodeMulti)
  private
    FMedia: string;
    FTitle: string;
    FType: string;
  protected
    procedure EnqueueElement(const Entry: PIpHtmlElement); override;
    function ElementQueueIsEmpty: Boolean; override;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Media : string read FMedia write FMedia;
    property Title : string read FTitle write FTitle;
    property Type_ : string read FType write FType;
  end;

  TIpHtmlNodeHeader = class(TIpHtmlNodeInline)
  private
    FAlign : TIpHtmlAlign;
    FSize : TIpHtmlHeaderSize;
  protected
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Enqueue; override;
    procedure LoadAndApplyCSSProps; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
    property ElementName;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
//    property Align : TIpHtmlAlign read FAlign write FAlign;
    property Size : TIpHtmlHeaderSize read FSize write FSize;
  end;

  { TIpHtmlNodeP }

  TIpHtmlNodeP = class(TIpHtmlNodeInline)
  private
    FAlign : TIpHtmlAlign;
  protected
    function GetAlign: TIpHtmlAlign; override;
    procedure SetAlign(const Value: TIpHtmlAlign); override;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure Enqueue; override;
    procedure LoadAndApplyCSSProps; override;
    procedure SetProps(const RenderProps: TIpHtmlProps); override;
  (*
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlAlign read GetAlign write SetAlign;
  *)
  end;

  TIpHtmlNodeHtml = class(TIpHtmlNodeMulti)
  private
    FLang: string;
    FVersion: string;
    FDir: TIpHtmlDirection;
  protected
    procedure CalcMinMaxHtmlWidth(const RenderProps: TIpHtmlProps; var Min, Max: Integer);
    function GetHeight(const RenderProps: TIpHtmlProps; const Width: Integer): Integer;
  public
    function HasBodyNode: Boolean;
    procedure Layout(const RenderProps: TIpHtmlProps; const TargetRect : TRect);
    procedure Render(RenderProps: TIpHtmlProps);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Dir : TIpHtmlDirection read FDir write FDir;
    property Lang : string read FLang write FLang;
    property Version : string read FVersion write FVersion;
  end;

  { cannot be moved to IpHtmlNodes, used by TIpHtml directly }
  TIpHtmlNodeTITLE = class(TIpHtmlNodeNv)  
  private
    FTitle: string;
  public
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Title : string read FTitle write FTitle;
  end;

  
  { TIpHtmlNodeBODY }

  TIpHtmlNodeBODY = class(TIpHtmlNodeBlock)
  private
    FLink : TColor;
    FVLink : TColor;
    FALink : TColor;
    procedure SetAlink(const Value: TColor);
    procedure SetLink(const Value: TColor);
    procedure SetVlink(const Value: TColor);
  protected
    FBGPicture: TPicture;
  public
    constructor Create(ParentNode : TIpHtmlNode);
    destructor Destroy; override;
    procedure ImageChange(NewPicture : TPicture); override;
    procedure LoadAndApplyCSSProps; override;
    procedure Render(RenderProps: TIpHtmlProps); override;
    property BgPicture: TPicture read FBgPicture;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property ALink : TColor read Falink write SetAlink;
    property Background;
    property BgColor;
    property Link : TColor read FLink write SetLink;
    property VLink : TColor read FVLink write SetVlink;
  end;

  TIpHtmlNodeFRAMESET = class(TIpHtmlNodeCore)
  private
    FCols: TIpHtmlMultiLengthList;
    FRows: TIpHtmlMultiLengthList;
  public
    destructor Destroy; override;
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Cols : TIpHtmlMultiLengthList read FCols write FCols;
    property Rows : TIpHtmlMultiLengthList read FRows write FRows;
    property ClassID;
    property ID;
    property Title;
  end;

  TIpHtmlFrame = class;

  TIpHtmlNodeIFRAME = class(TIpHtmlNodeControl)
  private
    FAlign: TIpHtmlAlign;
    FFrameBorder: Integer;
    FHeight: TIpHtmlLength;
    FLongDesc: string;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FName: string;
    FScrolling: TIpHtmlFrameScrolling;
    FSrc: string;
    FWidth: TIpHtmlLength;
    FFrame : TIpHtmlFrame;
    procedure SetFrameBorder(const Value: Integer);
    procedure SetMarginHeight(const Value: Integer);
    procedure SetMarginWidth(const Value: Integer);
    procedure SetScrolling(const Value: TIpHtmlFrameScrolling);
  protected
    procedure SetAlign(const Value: TIpHtmlAlign); override;
    procedure CreateControl(Parent : TWinControl); override;
  public
    destructor Destroy; override;
    procedure AddValues(NameList, ValueList : TStringList); override;
    procedure Reset; override;
    function Successful: Boolean; override;
    procedure WidthChanged(Sender: TObject);
  {$IFDEF HTML_RTTI}
  published
  {$ENDIF}
    property Align : TIpHtmlAlign read FAlign write SetAlign;
    property Frame: TIpHtmlFrame read FFrame;
    property FrameBorder : Integer read FFrameBorder write SetFrameBorder;
    property Height : TIpHtmlLength read FHeight write FHeight;
    property LongDesc : string read FLongDesc write FLongDesc;
    property MarginHeight : Integer read FMarginHeight write SetMarginHeight;
    property MarginWidth : Integer read FMarginWidth write SetMarginWidth;
    property Name : string read FName write FName;
    property Scrolling : TIpHtmlFrameScrolling read FScrolling write SetScrolling;
    property Src : string read FSrc write FSrc;
    property Width : TIpHtmlLength read FWidth write FWidth;
  end;

  TInvalidateEvent = procedure(Sender : TIpHtml; const Rect : TRect) of object;

  TIpHtmlBasicParser = class
  public
    function Execute: Boolean; virtual; abstract;
    function FindAttribute(const AttrNameSet: TIpHtmlAttributesSet): string; virtual; abstract;
  end;
  
  TWriteCharProvider = procedure(C : AnsiChar) of object;

  TIpHtmlDataGetImageEvent =
    procedure(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture) of object;

  TIpHtmlScrollEvent =
    procedure(Sender: TIpHtml; const R: TRect; ShowAtTop: Boolean) of object;

  TGetEvent =
    procedure(Sender: TIpHtml; const URL: string) of object;

  TPostEvent =
    procedure(Sender: TIpHtml; const URL: string; FormData: TIpFormDataEntity) of object;

  TIFrameCreateEvent =
    procedure(Sender: TIpHtml; Parent: TWinControl; Frame: TIpHtmlNodeIFRAME;
      var Control: TWinControl) of object;

  TURLCheckEvent =
    procedure(Sender: TIpHtml; const URL: string; var Visited: Boolean) of object;

  TReportURLEvent =
    procedure(Sender: TIpHtml; const URL: string) of object;

  TIpHtmlRectListEntry = record
    Rect : TRect;
    Element : PIpHtmlElement;
    Block : TIpHtmlNodeBlock;
  end;
  PIpHtmlRectListEntry = ^TIpHtmlRectListEntry;

  TControlEvent = procedure(Sender: TIpHtml; Node: TIpHtmlNodeControl)
    of object;

  TControlEvent2 = procedure(Sender: TIpHtml; Node: TIpHtmlNodeControl; var cancel: boolean)
    of object;

  TIpHtml = class
  private
    FHotNode : TIpHtmlNode;
    FCurElement : PIpHtmlElement;
    FHotPoint : TPoint;
    FMouseLastPoint : TPoint;
    FOnInvalidateRect : TInvalidateEvent;
    FTarget : TCanvas;
    FVLinkColor: TColor;
    FLinkColor: TColor;
    FALinkColor: TColor;
    FTextColor: TColor;
    FBgColor: TColor;
    FFontQuality: TFontQuality;
    FFactBAParag: Real;
    FHasFrames : Boolean;
    FLinksUnderlined: Boolean;
    FOnGetImageX : TIpHtmlDataGetImageEvent;
    FOnScroll : TIpHtmlScrollEvent;
    FOnInvalidateSize : TNotifyEvent;
    FOnGet: TGetEvent;
    FOnPost: TPostEvent;
    FOnIFrameCreate : TIFrameCreateEvent;
    FOnURLCheck: TURLCheckEvent;
    FOnReportURL: TReportURLEvent;
    FControlClick : TControlEvent;
    FControlClick2 : TControlEvent2;
    FControlOnEditingDone : TControlEvent;
    FControlOnChange : TControlEvent;
    FControlCreate : TControlEvent;
    FCurFrameSet : TIpHtmlNodeFRAMESET;
    FCanPaint : Boolean;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FRenderDev: TIpHtmlRenderDevice;
    FCSS: TCSSGlobalProps;
    FDocCharset: string;
    FTabList: TIpHtmlTabList;
    FNeedResize: Boolean;
    FParser: TIpHtmlBasicParser;
  protected
    CharStream : TStream;
    FHtml : TIpHtmlNodeHtml;
    FFlagErrors : Boolean;
    FPageRect : TRect;
    FClientRect : TRect;   {the coordinates of the paint rectangle}
    FPageViewRect : TRect; {the current section of the page}
    FPageViewBottom : Integer; {the lower end of the page, may be different from PageViewRect.Bottom }
    FPageViewTop: Integer; { the upper end of the page }
    DefaultProps : TIpHtmlProps;
    FBody : TIpHtmlNodeBODY;
    FTitleNode : TIpHtmlNodeTITLE;
    FDataProvider: TIpAbstractHtmlDataProvider;
    {$IFDEF UseGifImageUnit}
    GifImages : TFPList;
    {$ELSE}
    AnimationFrames : TFPList;
    {$ENDIF}
    FLIndent, FLOutdent : PIpHtmlElement;
    SoftLF,
    HardLF, HardLFClearLeft, SoftHyphen,
    HardLFClearRight, HardLFClearBoth : PIpHtmlElement;
    NameList : TStringList;
    IdList: TStringList;
    GifQueue : TFPList;
    MapList : TFPList;
    AreaList : TFPList;
    DefaultImage : TPicture;
    MapImgList : TFPList;
    PaintBufferBitmap : TBitmap;
    PaintBuffer : TCanvas;
    Destroying : Boolean;
    FAllSelected : Boolean;
    RectList : TFPList;
    FStartSel, FEndSel : TPoint;
    ElementPool : TIpHtmlPoolManager;
    AnchorList : TFPList;
    FControlList : TFPList;
    FCurURL : string;
    DoneLoading : Boolean;
    PropACache : TIpHtmlPropsAList;
    PropBCache : TIpHtmlPropsBList;
    RenderCanvas : TCanvas;
    FPageHeight : Integer;
    FFixedTypeface: string;
    FDefaultTypeFace: string;
    FDefaultFontSize: integer;
    FControlParent: TWinControl;
    procedure ResetCanvasData;
    procedure ResetWordLists;
    procedure ResetBlocks(Node: TIpHtmlNode);
    procedure ResetImages(Node: TIpHtmlNode);
    procedure ResetElementMetrics(P: Pointer);
    function CheckKnownURL(URL: string): boolean;
    procedure ReportReference(URL: string);
    procedure PaintSelection;
    function NewElement(EType : TElementType; Own: TIpHtmlNode) : PIpHtmlElement;
    function BuildStandardEntry(EType: TElementType): PIpHtmlElement;
    function BuildLinefeedEntry(EType: TElementType; AHeight: Integer): PIpHtmlElement;
    function FindAttribute(const AttrNameSet: TIpHtmlAttributesSet): string;
    procedure Parse;
    procedure InvalidateRect(R : TRect);
    procedure SetDefaultProps;
    function BuildPath(const Ext: string): string;
    procedure MakeVisible(const R: TRect; ShowAtTop: Boolean = True);
    procedure InvalidateSize;
    procedure AddGifQueue(Graphic: TGraphic; const R: TRect);
    procedure ClearGifQueue;
    procedure StartGifPaint(Target: TCanvas);
    procedure ClearAreaLists;
    procedure BuildAreaList;
    procedure ClearAreaList;
    procedure Get(const URL: string);
    procedure Post(const URL: string; FormData: TIpFormDataEntity);
    procedure ClearRectList;
    procedure CreateIFrame(Parent: TWinControl; Frame: TIpHtmlNodeIFRAME; var Control: TWinControl);
    procedure FinalizeRecs(P: Pointer);
    function LinkVisited(const URL: string): Boolean;
    procedure AddWord(Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
    procedure AddWordEntry(const Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
    function FindElement(const Name: string): TIpHtmlNode;
    function FindElementId(const Id: String): TIpHtmlNode;
    procedure Clear; {clear any contents}
    procedure Home;
    function GetPageRect(TargetCanvas: TCanvas; Width, Height : Integer): TRect; // computes the layout for this Canvas
    procedure MouseMove(Pt : TPoint);
    procedure DeselectAllItems(Item: Pointer);
    procedure SetSelection(StartPoint, EndPoint: TPoint);
    function HaveSelection: Boolean;
    procedure CopyToClipboard;
    procedure ReportReferences(Node: TIpHtmlNode);
    procedure RequestImageNodes(Node: TIpHtmlNode);
    procedure SelectAll;
    procedure DeselectAll;
    procedure ControlClick(Sender: TIpHtmlNodeControl);
    procedure ControlClick2(Sender: TIpHtmlNodeControl; var cancel: boolean);
    procedure ControlOnEditingDone(Sender: TIpHtmlNodeControl);
    procedure ControlOnChange(Sender: TIpHtmlNodeControl);
    procedure ControlCreate(Sender: TIpHtmlNodeControl);
    property HotNode: TIpHtmlNode read FHotNode;
    property CurElement: PIpHtmlElement read FCurElement write FCurElement;
    property HotPoint: TPoint read FHotPoint;
    property OnInvalidateRect: TInvalidateEvent read FOnInvalidateRect write FOnInvalidateRect;
    property TextColor: TColor read FTextColor write FTextColor;
    property LinkColor: TColor read FLinkColor write FLinkColor;
    property VLinkColor: TColor read FVLinkColor write FVLinkColor;
    property ALinkColor: TColor read FALinkColor write FALinkColor;
    property BgColor: TColor read FBgColor write FBgColor;
    property LinksUnderlined: Boolean read FLinksUnderlined write FLinksUnderlined;
    property HasFrames: Boolean read FHasFrames;
    property OnGetImageX: TIpHtmlDataGetImageEvent read FOnGetImageX write FOnGetImageX;
    property OnScroll: TIpHtmlScrollEvent read FOnScroll write FOnScroll;
    property OnInvalidateSize: TNotifyEvent read FOnInvalidateSize write FOnInvalidateSize;
    property OnGet: TGetEvent read FOnGet write FOnGet;
    property OnPost: TPostEvent read FOnPost write FOnPost;
    property OnIFrameCreate: TIFrameCreateEvent read FOnIFrameCreate write FOnIFrameCreate;
    property OnURLCheck: TURLCheckEvent read FOnURLCheck write FOnURLCheck;
    property OnReportURL: TReportURLEvent read FOnReportURL write FOnReportURL;
    property OnControlClick: TControlEvent read FControlClick write FControlClick;
    property OnControlClick2: TControlEvent2 read FControlClick2 write FControlClick2;
    property OnControlEditingDone: TControlEvent read FControlOnEditingDone write FControlOnEditingDone;
    property OnControlChange: TControlEvent read FControlOnChange write FControlOnChange;
    property OnControlCreate: TControlEvent read FControlCreate write FControlCreate;
    property CanPaint: Boolean read FCanPaint;
    property MarginWidth: Integer read FMarginWidth write FMarginWidth default 20;
    property MarginHeight: Integer read FMarginHeight write FMarginHeight default 20;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
    {$IFOPT C+}
    procedure CheckImage(Picture: TPicture);
    {$ENDIF}
    function GetSelectionBlocks(out StartSelIndex,EndSelIndex: Integer): boolean;
    function getControlCount:integer;
    function getControl(i:integer):TIpHtmlNode;
  public
    constructor Create;
    destructor Destroy; override;
    function PagePtToScreen(const Pt: TPoint): TPoint;
    function PageRectToScreen(const Rect: TRect; var ScreenRect: TRect): Boolean;
    procedure AddRect(const R: TRect; AElement: PIpHtmlElement; ABlock: TIpHtmlNodeBlock);
    procedure FixMissingBodyTag;
    procedure LoadFromStream(S : TStream);
    procedure Render(TargetCanvas: TCanvas; TargetPageRect : TRect;
      UsePaintBuffer: Boolean; const TopLeft: TPoint); overload;
    procedure Render(TargetCanvas: TCanvas; TargetPageRect: TRect;
      APageTop, APageBottom: Integer; UsePaintBuffer: Boolean;
      const TopLeft: TPoint); overload;
    {$IFDEF IP_LAZARUS_DBG}
    procedure DebugChild(Node: TIpHtmlNode; const UserData: Pointer);
    procedure DebugAll;
    {$ENDIF}
    property AllSelected : Boolean read FAllSelected;
    property Body: TIpHtmlNodeBODY read FBody;
    property CSS: TCSSGlobalProps read FCSS write FCSS;
    property DataProvider: TIpAbstractHtmlDataProvider read FDataProvider;
    property FlagErrors : Boolean read FFlagErrors write FFlagErrors;
    property FixedTypeface: string read FFixedTypeface write FFixedTypeface;
    property DefaultTypeFace: string read FDefaultTypeFace write FDefaultTypeFace;
    property DefaultFontSize: integer read FDefaultFontSize write FDefaultFontSize;
    property FontQuality: TFontQuality read FFontQuality write FFontQuality;
    property HtmlNode : TIpHtmlNodeHtml read FHtml;
    property CurUrl: string read FCurUrl;
    property TabList: TIpHtmlTabList read FTabList;
    property DocCharset: String read FDocCharset;  // Encoding of html text
    property Target: TCanvas read FTarget;
    property TitleNode : TIpHtmlNodeTITLE read FTitleNode;
    property PageHeight : Integer read FPageHeight;
    property PageViewRect : TRect read FPageViewRect;
    property PageViewBottom: Integer read FPageViewBottom;
    property PageViewTop: Integer read FPageViewTop;
    property ClientRect : TRect read FClientRect;
    property ControlsCount: integer read getControlCount;
    property Controls[i:integer]: TIpHtmlNode read getControl;
    property FrameSet : TIpHtmlNodeFRAMESET read FCurFrameSet;
    property FactBAParag: Real read FFactBAParag write FFactBAParag;
    property MouseLastPoint : TPoint read FMouseLastPoint;
    property RenderDevice: TIpHtmlRenderDevice read FRenderDev;
    property NeedResize: Boolean read FNeedResize write FNeedResize;
  end;

  TIpHtmlInternalPanel = class;

  TIpHtmlScrollBar = class
  private
    FKind: TScrollBarKind;
    FIncrement: TScrollBarInc;
    FPosition: Integer;
    FRange: Integer;
    FTracking: Boolean;
    FVisible: Boolean;
    procedure SetPosition(Value: Integer);
    procedure SetVisible(Value: Boolean);
  protected
    FControl: TIpHtmlInternalPanel;
    FPageIncrement: TScrollbarInc;
    FCalcRange: Integer;
    FUpdateNeeded: Boolean;
    procedure CalcAutoRange;
    function ControlSize(ControlSB, AssumeSB: Boolean): Integer;
    procedure DoSetRange(Value: Integer);
    function NeedsScrollBarVisible: Boolean;
    procedure ScrollMessage(var Msg: TLMScroll);
    procedure Update(ControlSB, AssumeSB: Boolean);
  public
    constructor Create(AControl: TIpHtmlInternalPanel; AKind: TScrollBarKind);
    property Kind: TScrollBarKind read FKind;
    property Increment: TScrollBarInc
                read FIncrement write FIncrement stored False default 8;
    property Position: Integer read FPosition write SetPosition default 0;
    property Range: Integer
                read FRange {write SetRange stored IsRangeStored default 0};
    property Tracking: Boolean read FTracking write FTracking default False;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TIpHtmlCustomPanel = class;

  { TIpHtmlInternalPanel }

  TIpHtmlInternalPanel = class(TCustomControl)
  private
    FHyper : TIpHtml;
    FPageRect : TRect;
    FPageRectValid: boolean;
    FAutoScroll: Boolean;
    FOnHotChange : TNotifyEvent;
    FOnCurElementChange : TNotifyEvent;
    FOnHotClick : TNotifyEvent;
    FOnClick : TNotifyEvent;
    function GetPageRect: TRect;
    procedure SetHtml(const Value: TIpHtml);
    procedure SetPageRect(const Value: TRect);
  protected
    FUpdatingScrollbars : Boolean;
    {$IFDEF Html_Print}
    InPrint: Integer;
    {$ENDIF}
    SettingPageRect : Boolean;
    FPaintingLock: Integer;
    MouseDownX, MouseDownY : Integer;
    HaveSelection,
    MouseIsDown,
    NewSelection : Boolean;
    SelStart, SelEnd : TPoint;
    HintWindow : THintWindow;
    CurHint : string;
    HintX, HintY : Integer;
    HintShownHere : Boolean;
    Printed: Boolean;
    procedure UpdateScrollBars;
    procedure ClearSelection;
    procedure SetSelection;
    procedure ScrollPtInView(P: TPoint);
    procedure ShowHintNow(const NewHint: string);
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure WMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
    procedure AsyncHotInvoke(data: ptrint);

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseLeave; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DoHotChange;
    procedure DoCurElementChange;
    procedure DoHotInvoke;
    procedure DoClick;
    procedure DoOnResize; override;
    procedure ScrollInView(R : TRect);
    procedure ScrollInViewRaw(R : TRect);
    function PagePtToScreen(const Pt : TPoint): TPoint;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure HideHint;
    function HtmlPanel: TIpHtmlCustomPanel;
    {$IFDEF Html_Print}
    procedure BeginPrint;
    procedure ResetPrint;
    procedure EndPrint;
    {$ENDIF}
  public
    ViewTop, ViewLeft : Integer;
    HScroll,
    VScroll : TIpHtmlScrollBar;
    {$IFDEF Html_Print}
    PrintPageRect : TRect;
    PrintWidth, PrintHeight: Integer;
    PrintTopLeft: TPoint;
    PageCount: Integer;
    function PreviewAntiAliasingMode: TAntiAliasingMode;
    {$ENDIF}
    procedure InvalidateSize;
    property Hyper : TIpHtml read FHyper write SetHtml;
    property PageRect : TRect read GetPageRect write SetPageRect;
    constructor Create(AOwner: TComponent); override;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
    property OnHotChange : TNotifyEvent read FOnHotChange write FOnHotChange;
    property OnCurElementChange: TNotifyEvent
                read FOnCurElementChange write FOnCurElementChange;
    property OnHotClick : TNotifyEvent read FOnHotClick write FOnHotClick;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
    destructor Destroy; override;
    procedure ScrollRequest(Sender: TIpHtml; const R: TRect; ShowAtTop: Boolean = True);
    {$IFDEF Html_Print}
    function GetPrintPageCount: Integer;
    procedure PrintPages(FromPage, ToPage: Integer);
    procedure PrintPreview;
    function SelectPrinterDlg: boolean;
    {$ENDIF}
    procedure EraseBackground(DC: HDC); override;
  end;

  { TIpAbstractHtmlDataProvider }

  TIpAbstractHtmlDataProvider = class(TIpBaseComponent)
  protected
    function DoGetHtmlStream(const URL: string;
      PostData: TIpFormDataEntity) : TStream; virtual; abstract;
    function DoCheckURL(const URL: string;
      var ContentType: string): Boolean; virtual; abstract;
    procedure DoLeave(Html: TIpHtml); virtual; abstract;
    procedure DoReference(const URL: string); virtual;  abstract;
    procedure DoGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture); virtual; abstract;
    function CanHandle(const URL: string): Boolean; virtual; abstract;
  public
    // The following methods were protected in the original code 
    // but had to be made public to cooperate with the TIpHtmlParser.
    function BuildURL(const OldURL, NewURL: string): string; virtual; abstract;
    { provider assumes ownership of returned TStream and will free it when
      done using it. }
    function DoGetStream(const URL: string): TStream; virtual; abstract;
  end;

  TIpHtmlEnumerator = procedure(Document: TIpHtml) of object;
  
  TIpHtmlFrame = class
  protected
    FCURURL : string;
    FCurAnchor : string;
    FViewer: TIpHtmlCustomPanel;
    FNoScroll: Boolean;
    FFramePanel : TPanel;
    Pnl : array[0..Pred(IPMAXFRAMES)] of TPanel;
    FMarginWidth, FMarginHeight : Integer;
    FFlagErrors : Boolean;
    PostData : TIpFormDataEntity;
    FHtml : TIpHtml;
    HyperPanel : TIpHtmlInternalPanel;
    FFrameCount : Integer;
    FFrames : array[0..Pred(IPMAXFRAMES)] of TIpHtmlFrame;
    FDataProvider : TIpAbstractHtmlDataProvider;
    FParent : TCustomPanel;
    FName : string;
    InOpen: Boolean;
    procedure InvalidateRect(Sender: TIpHtml; const R : TRect);
    procedure FramePanelResize(Sender: TObject);
    procedure AlignPanels;
    procedure InvalidateSize(Sender: TObject);
    procedure Get(Sender: TIpHtml; const URL: string);
    procedure Post(Sender: TIpHtml; const URL: string; FormData: TIpFormDataEntity);
    procedure IFrameCreate(Sender: TIpHtml; Parent: TWinControl;
      Frame: TIpHtmlNodeIFRAME; var Control: TWinControl);
    procedure InitHtml;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure ControlClick(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure ControlClick2(Sender: TIpHtml; Node: TIpHtmlNodeControl; var cancel: boolean);
    procedure ControlOnChange(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure ControlOnEditingDone(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure ControlCreate(Sender: TIpHtml; Node: TIpHtmlNodeControl);
    procedure OpenRelativeURL(const URL: string);
    procedure SelectAll;
    procedure DeselectAll;
    procedure CopyToClipboard;
    function HaveSelection: Boolean;
    function FindFrame(const FrameName: string): TIpHtmlFrame;
    procedure MakeAnchorVisible(const URL: string);
    function Scroll(Action: TIpScrollAction; ADistance: Integer = 100): Boolean;
    procedure Home;
    function IsExternal(const URL: string): Boolean;
    procedure SetHtml(NewHtml : TIpHtml);
    procedure Stop;
    function getFrame(i: integer): TIpHtmlFrame;
    procedure InternalFreeFrames;
    procedure InternalCreateFrames;
    procedure RemoveDataProvider;
  public
    constructor Create(Viewer: TIpHtmlCustomPanel; Parent: TCustomPanel;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors, NoScroll: Boolean;
      MarginWidth, MarginHeight: Integer);
    destructor Destroy; override;
    procedure OpenURL(const URL: string; Delayed: Boolean);
    property CurUrl: string read FCurUrl;
    property CurAnchor : string read FCurAnchor;
    property Html: TIpHtml read FHtml;
    property FramePanel : TPanel read FFramePanel;
    property Name: string read FName;
    property FrameCount: integer read FFrameCount;
    property Frames[i:integer] : TIpHtmlFrame read getFrame;
    property Viewer: TIpHtmlCustomPanel read FViewer;
  end;

  TIpHtmlCustomScanner = class;
  TIpHtmlNVFrame = class
  protected
    FCURURL : string;
    FCurAnchor : string;
    FScanner: TIpHtmlCustomScanner;
    FFlagErrors : Boolean;
    PostData : TIpFormDataEntity;
    FHtml : TIpHtml;
    FFrameCount : Integer;
    FFrames : array[0..Pred(IPMAXFRAMES)] of TIpHtmlNVFrame;
    FDataProvider : TIpAbstractHtmlDataProvider;
    FName : string;
    procedure InitHtml;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure OpenRelativeURL(const URL: string);
    procedure SelectAll;
    procedure CopyToClipboard;
    function HaveSelection: Boolean;
    function FindFrame(const FrameName: string): TIpHtmlNvFrame;
    procedure MakeAnchorVisible(const URL: string);
    procedure Home;
    procedure Stop;
    function getFrame(i: integer): TIpHtmlNVFrame;
  public
    constructor Create(Scanner: TIpHtmlCustomScanner;
      DataProvider : TIpAbstractHtmlDataProvider; FlagErrors: Boolean);
    destructor Destroy; override;
    procedure OpenURL(const URL: string);
    property CurUrl: string read FCurUrl;
    property CurAnchor : string read FCurAnchor;
    property Html: TIpHtml read FHtml;
    property Name: string read FName;
    property FrameCount: integer read FFrameCount;
    property Frames[i:integer] : TIpHtmlNVFrame read getFrame;
    property Scanner: TIpHtmlCustomScanner read FScanner;
  end;

  TIpHtmlControlEvent = procedure(Sender: TIpHtmlCustomPanel;
    Frame: TIpHtmlFrame; Html: TIpHtml; Node: TIpHtmlNodeControl) of object;

  TIpHtmlControlEvent2 = procedure(Sender: TIpHtmlCustomPanel;
    Frame: TIpHtmlFrame; Html: TIpHtml; Node: TIpHtmlNodeControl; var cancel: boolean) of object;

  { TIpHtmlCustomPanel }

  TIpHtmlHotURLEvent = procedure (Sender: TObject; const URL: String) of object;

  TIpHtmlCustomPanel = class(TCustomPanel)
  private
    FHotChange : TNotifyEvent;
    FHotClick : TNotifyEvent;
    FHotURLEvent: TIpHtmlHotURLEvent;
    FControlClick : TIpHtmlControlEvent;
    FControlClick2 : TIpHtmlControlEvent2;
    FControlOnEditingDone : TIpHtmlControlEvent;
    FControlOnChange : TIpHtmlControlEvent;
    FControlCreate : TIpHtmlControlEvent;
    FCurElementChange: TNotifyEvent;
    FDocumentOpen: TNotifyEvent;
    FAllowTextSelect: Boolean;
    FCurElement : PIpHtmlElement;
    FPrintSettings: TIpHtmlPrintSettings;
    FFactBAParag: Real;
    FFontQuality: TFontQuality;
    FWantTabs: Boolean;
    FScrollDist: Integer;
    FUsePaintBuffer: Boolean;
    procedure SetDataProvider(const AValue: TIpAbstractHtmlDataProvider);
    procedure SetFactBAParag(const Value: Real);
    function FactBAParagNotIs1: Boolean;
    function GetVScrollPos: Integer;
    procedure SetVScrollPos(const Value: Integer);
    procedure SetFontQuality(const AValue: TFontQuality);
  protected
    FFlagErrors: Boolean;
    FFixedTypeface: string;
    FDefaultTypeFace: string;
    FDefaultFontSize: integer;
    FHotURL: string;
    FDataProvider: TIpAbstractHtmlDataProvider;
    URLStack : TStringList;
    TargetStack : TStringList;
    Stp : Integer;
    VisitedList : TStringMap;
    FVLinkColor: TColor;
    FLinkColor: TColor;
    FALinkColor: TColor;
    FTextColor: TColor;
    FBgColor: TColor;
    FLinksUnderlined: Boolean;
    FShowHints: Boolean;
    FMarginHeight: Integer;
    FMarginWidth: Integer;
    FMasterFrame : TIpHtmlFrame;
    FHotNode : TIpHtmlNode;
    GetURL : string;
    PostURL : string;
    PostData : TIpFormDataEntity;
    procedure Push(const Target, URL: string);
    function GetTitle: string;
    procedure InternalOpenURL(const Target, HRef: string);
    procedure URLCheck(Sender: TIpHtml; const URL: string; var Visited: Boolean);
    procedure ReportURL(Sender: TIpHtml; const URL: string);
    procedure Paint; override;
    procedure HotChange(Sender: TObject);
    procedure CurElementChange(Sender: TObject);
    procedure HotClick(Sender: TObject);
    procedure ClientClick(Sender: TObject);
    procedure DoHotChange;
    procedure DoHotClick;
    procedure DoOnMouseWheel(Shift: TShiftState; Delta, XPos, YPos: SmallInt);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WMGetDlgCode(var Msg : TMessage); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure CMIpHttpGetRequest(var Message: TMessage); message CM_IpHttpGetRequest;
    procedure ControlClick(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    procedure ControlClick2(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl; var pCancel: boolean);
    procedure ControlOnChange(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    procedure ControlOnEditingdone(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    procedure ControlCreate(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
      pNode: TIpHtmlNodeControl);
    function GetVersion : string;
    function GetCurUrl: string;
    procedure SetVersion(const Value : string);
    procedure SetDefaultTypeFace(const Value: string);
    procedure SetDefaultFontSize(const Value: integer);
    procedure CalculatePreferredSize(var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    property UsePaintBuffer: Boolean read FUsePaintBuffer write FUsePaintBuffer default true;
  public
    {$IFDEF Html_Print}
    function GetPrintPageCount: Integer;
    {$ENDIF}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure EraseBackground(DC: HDC); override;

    procedure CopyToClipboard;
    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    procedure GoBack;
    function canGoBack : boolean;
    procedure GoForward;
    function canGoForward : boolean;
    function HaveSelection: Boolean;
    property MasterFrame : TIpHtmlFrame read FMasterFrame;
    property HotNode : TIpHtmlNode read FHotNode;
    function IsURLHtml(const URL: string): Boolean;
    procedure MakeAnchorVisible(const Name: string);
    procedure OpenURL(const URL: string);
    function Scroll(Action: TIpScrollAction; ADistance: Integer = 100): Boolean;
    procedure SelectAll;
    procedure DeselectAll;
    procedure SetHtml(NewHtml : TIpHtml);
    procedure SetHtmlFromFile(const AFileName: String);
    procedure SetHtmlFromStr(NewHtml : string);
    procedure SetHtmlFromStream(NewHtml : TStream);
    procedure Stop;
    {$IFDEF Html_Print}
    procedure Print(FromPg, ToPg: LongInt);
    procedure PrintPreview;
    {$ENDIF}
    function GetContentSize: TSize;

    property VScrollPos: Integer read GetVScrollPos write SetVScrollPos;
    property BgColor: TColor read FBgColor write FBgColor default clWhite;
    property ALinkColor: TColor read FALinkColor write FALinkColor default clRed;
    property AllowTextSelect: Boolean read FAllowTextSelect write FAllowTextSelect default True;
    property CurElement: PIpHtmlElement read FCurElement;
    property DataProvider: TIpAbstractHtmlDataProvider read FDataProvider write SetDataProvider;
    property FactBAParag: Real read FFactBAParag write SetFactBAParag stored FactBAParagNotIs1;
    property FlagErrors: Boolean read FFlagErrors write FFlagErrors;
    property FixedTypeface: string read FFixedTypeface write FFixedTypeface;
    property DefaultTypeFace: string read FDefaultTypeFace write SetDefaultTypeFace;
    property DefaultFontSize: integer read FDefaultFontSize write SetDefaultFontSize;
    property FontQuality: TFontQuality read FFontQuality write SetFontQuality default fqDefault;
    property HotURL: string read FHotURL;
    property LinkColor: TColor read FLinkColor write FLinkColor default clBlue;
    property LinksUnderlined: Boolean read FLinksUnderlined write FLinksUnderlined default DEFAULT_LINKS_UNDERLINED;
    property MarginHeight: Integer read FMarginHeight write FMarginHeight default 10;
    property MarginWidth: Integer read FMarginWidth write FMarginWidth default 10;
    property PrintSettings: TIpHtmlPrintSettings read FPrintSettings write FPrintSettings;
    property ScrollDist: Integer read FScrollDist write FScrollDist default 100;
    property ShowHints: Boolean read FShowHints write FShowHints default True;
    property TextColor: TColor read FTextColor write FTextColor default clBlack;
    property Title: string read GetTitle;
    property VLinkColor: TColor read FVLinkColor write FVLinkColor default clMaroon;

    property OnControlClick: TIpHtmlControlEvent read FControlClick write FControlClick;
    property OnControlClick2: TIpHtmlControlEvent2 read FControlClick2 write FControlClick2;
    property OnControlEditingDone: TIpHtmlControlEvent read FControlOnEditingDone
                                                      write FControlOnEditingDone;
    property OnControlChange: TIpHtmlControlEvent read FControlOnChange write FControlOnChange;
    property OnControlCreate: TIpHtmlControlEvent read FControlCreate write FControlCreate;
    property OnCurElementChange: TNotifyEvent read FCurElementChange write FCurElementChange;
    property OnDocumentOpen: TNotifyEvent read FDocumentOpen write FDocumentOpen;
    property OnHotChange: TNotifyEvent read FHotChange write FHotChange;
    property OnHotClick: TNotifyEvent read FHotClick write FHotClick;
    property OnHotURL: TIpHtmlHotURLEvent read FHotURLEvent write FHotURLEvent;
    property CurURL: string read GetCurUrl;
    property WantTabs: Boolean read FWantTabs write FWantTabs default True;
  published
    property Version: string read GetVersion write SetVersion stored False;
  end;

  TIpHtmlPanel = class(TIpHtmlCustomPanel)
  published
    property Align;
    property ALinkColor;
    property AllowTextSelect;
    property Anchors;
    property BgColor;
    property BorderSpacing;
    property BorderWidth;
    property BorderStyle;
    property Constraints;
    property DataProvider;
    property Enabled;
    property FixedTypeface;
    property FontQuality;
    property DefaultTypeFace;
    property DefaultFontSize;
    property FactBAParag;
    property FlagErrors;
    property LinkColor;
    property LinksUnderlined;
    property MarginHeight;
    property MarginWidth;
    property PopupMenu;
    property PrintSettings;
    property ScrollDist;
    property ShowHints;
    property TabOrder;
    property TabStop;
    property TextColor;
    property UsePaintBuffer;
    property Visible;
    property VLinkColor;
    property WantTabs;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnControlClick;
    property OnControlClick2;
    property OnControlChange;
    property OnControlEditingDone;
    property OnControlCreate;
    property OnCurElementChange;
    property OnDocumentOpen;
    property OnEnter;
    property OnExit;
    property OnHotChange;
    property OnHotClick;
    property OnHotURL;
  end;

  TIpHtmlCustomScanner = class(TComponent)
  private
    FDataProvider: TIpAbstractHtmlDataProvider;
    FFlagErrors: Boolean;
    function GetTitle: string;
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  protected
    URLStack : TStringList;
    TargetStack : TStringList;
    Stp : Integer;
    FCurURL : string;
    FMasterFrame : TIpHtmlNVFrame;
    procedure Push(const Target, URL: string);
    procedure InternalOpenURL(const Target, HRef: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnumDocuments(Enumerator: TIpHtmlEnumerator);
    function IsURLHtml(const URL: string): Boolean;
    procedure OpenURL(const URL: string);
    procedure Stop;

    property MasterFrame : TIpHtmlNVFrame read FMasterFrame;
    property DataProvider: TIpAbstractHtmlDataProvider read FDataProvider write FDataProvider;
    property FlagErrors : Boolean read FFlagErrors write FFlagErrors;
    property Title : string read GetTitle;
    property CurUrl: string read FCurUrl;
  published
    property Version : string read GetVersion write SetVersion stored False;
  end;

  TIpHtmlScanner = class(TIpHtmlCustomScanner)
  published
    property DataProvider;
    property FlagErrors;
  end;

  TIdFindNodeCriteria = function(ACurrNode: TIpHtmlNodeCore; const AParamStr: string): Boolean;

var
  // LayouterClass is initialized by the layout unit.
  BlockLayouterClass: TIpHtmlBaseLayouterClass;
  TableElemLayouterClass: TIpHtmlBaseLayouterClass;
  TableLayouterClass: TIpHtmlBaseTableLayouterClass;

function SizeRec(cx, cy: Integer): TSize;
function StdIndent: Integer;
procedure SetWordRect(Element: PIpHtmlElement; const Value: TRect);
function CalcMultiLength(const List: TIpHtmlMultiLengthList;
  Avail: Integer; var Sections: Integer): TIntArr;
//function GetAlignmentForStr(str: string; pDefault: TIpHtmlAlign = haDefault): TIpHtmlAlign;
procedure TrimFormatting(const S: string; Target: PAnsiChar; PreFormatted: Boolean = False);
function dbgs(et: TElementType): string; overload;

function GetNextSiblingNode(ANode: TIpHtmlNode): TIpHtmlNode;
function GetPrevSiblingNode(ANode: TIpHtmlNode): TIpHtmlNode;
function GetParentNodeOfClass(ANode: TIpHtmlNode; AClass: TIpHtmlNodeClass): TIpHtmlNode;
function FindNode(ANode: TIpHtmlNode; ACriteria: TIdFindNodeCriteria; const AParamStr: string): TIpHtmlNodeCore;
function FindNodeByElemId(ANode: TIpHtmlNode; const AElemId: string): TIpHtmlNodeCore;
function FindNodeByElemClass(ANode: TIpHtmlNode; const AElemClass: string): TIpHtmlNodeCore;

procedure Register;


implementation

uses
  // ipHtmlBlockLayout and ipHtmlTableLayout should not be needed here but
  // the initialization section is not called otherwise.
  {$IFDEF Html_Print}
  Printers, PrintersDlgs, IpHtmlPv,
  {$ENDIF}
  ipHtmlNodes, ipHtmlParser, ipHtmlBlockLayout, ipHtmlTableLayout; 

{$R *.res}

var
  FlatSB_GetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    var ScrollInfo: TScrollInfo): BOOL; stdcall;
  FlatSB_GetScrollPos: function(hWnd: HWND; nBar: Integer): Integer; stdcall;
  FlatSB_SetScrollPos: function(hWnd: HWND; nBar, nPos: Integer;
    bRedraw: BOOL): Integer; stdcall;
  FlatSB_SetScrollProp: function(p1: HWND; index: Integer; newValue: Integer;
    p4: Bool): Bool; stdcall;
  FlatSB_SetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;

const
  MaxElements = 1024*1024;
//  ShyChar = #1; {character used to represent soft-hyphen in strings}
//  NbspChar = #2; {character used to represent no-break space in strings}
//  NbspUtf8 = #194#160;  {utf8 code of no-break space character}
  WheelDelta = 8;

const
  WSB_PROP_CYVSCROLL      = $00000001;
  WSB_PROP_CXHSCROLL      = $00000002;
  WSB_PROP_CYHSCROLL      = $00000004;
  WSB_PROP_CXVSCROLL      = $00000008;
  WSB_PROP_CXHTHUMB       = $00000010;
  WSB_PROP_CYVTHUMB       = $00000020;
  WSB_PROP_VBKGCOLOR      = $00000040;
  WSB_PROP_HBKGCOLOR      = $00000080;
  WSB_PROP_VSTYLE         = $00000100;
  WSB_PROP_HSTYLE         = $00000200;
  WSB_PROP_WINSTYLE       = $00000400;
  WSB_PROP_PALETTE        = $00000800;
  WSB_PROP_MASK           = $00000FFF;
  FSB_FLAT_MODE               = 2;
  FSB_ENCARTA_MODE            = 1;
  FSB_REGULAR_MODE            = 0;

{$IFDEF IP_LAZARUS_DBG}
procedure DumpTIpHtmlProps(aProps: TIpHtmlProps);
var
   propA : TIpHtmlPropAFieldsRec;
   propB : TIpHtmlPropBFieldsRec;
begin
     if aProps = nil then
     begin
          writeln('TIpHtmlProps is nil');
          exit;
     end;
     writeln('>>> ', aProps.FOwner.ClassName, ': ', dbgs(@aProps));
     if aProps.PropA <> nil then
     begin
     propA := aProps.PropA.FPropRec;
     writeln('PropA >>>:');
     writeln('BaseFontSize :', propA.BaseFontSize);
     writeln('FontSize :', propA.FontSize);
     //writeln('FontStyle :', propA.FontStyle);
     writeln('FontName :', propA.FontName);
     end;

     if aProps.PropB <> nil then
     begin
     propB := aProps.PropB.FPropRec;
     writeln('PropB >>>:');
     writeln('FontBaseline :', propB.FontBaseline);
     writeln('Alignment :', Ord(propB.Alignment));
     writeln('FontColor :', propB.FontColor);
     writeln('VAlignment :', Ord(propB.VAlignment));
     writeln('LinkColor :', propB.LinkColor);
     writeln('VLinkColor :', propB.VLinkColor);
     writeln('ALinkColor :', propB.ALinkColor);
     writeln('BgColor :', propB.BgColor);
     writeln('NoBreak :', propB.NoBreak);
     end;
end;

procedure DebugBox(Canvas: TCanvas; R: TRect; cl:TColor; dbg:boolean=false);
var
  OldPenColor: TColor;
begin
  OldPenColor := Canvas.Pen.Color;
  Canvas.Pen.Color := cl;
  Canvas.Moveto(r.left+(r.right-r.left) div 2, r.top);
  Canvas.Lineto(r.left+(r.right-r.left) div 2, r.bottom);
  Canvas.MoveTo(r.Left, r.top+(r.bottom-r.top) div 2);
  Canvas.LineTo(r.right, r.top+(r.bottom-r.top) div 2);
  if Dbg then
    DebugLn('DebugBox:R=',dbgs(R));
  Canvas.Pen.Color := OldPenColor;
end;
{$ENDIF}

function dbgs(et: TElementType): string;
begin
  writestr(Result,et);
end;

function GetNextSiblingNode(ANode: TIpHtmlNode): TIpHtmlNode;
var
  node: TIpHtmlNode;
  parent: TIpHtmlNodeMulti;
  i: Integer;
begin
  Result := nil;
  if ANode = nil then
    exit;
  if (ANode.FParentNode = nil) or not (ANode.ParentNode is TIpHtmlNodeMulti) then
    exit;
  parent := TIpHtmlNodeMulti(ANode.FParentNode);
  if parent.ChildCount = 1 then
    exit;
  Result := parent.ChildNode[parent.ChildCount-1];
  for i := parent.ChildCount-2 downto 0 do
  begin
    node := parent.ChildNode[i];
    if node = ANode then
      exit;
    Result := node;
  end;
  Result := nil;
end;

function GetPrevSiblingNode(ANode: TIpHtmlNode): TIpHtmlNode;
var
  node: TIpHtmlNode;
  parent: TIpHtmlNodeMulti;
  i: Integer;
begin
  Result := nil;
  if ANode = nil then
    exit;
  if (ANode.FParentNode = nil) or not (ANode.ParentNode is TIpHtmlNodeMulti) then
    exit;
  parent := TIpHtmlNodeMulti(ANode.FParentNode);
  if parent.ChildCount = 1 then
    exit;
  Result := parent.ChildNode[0];
  for i:=1 to parent.ChildCount-1 do
  begin
    node := parent.ChildNode[i];
    if node = ANode then
      exit;
    Result := node;
  end;
  Result := nil;
end;

function GetParentNodeOfClass(ANode: TIpHtmlNode;
  AClass: TIpHtmlNodeClass): TIpHtmlNode;
begin
  Result := ANode;
  while Assigned(Result) and not (Result is AClass) do
    Result := Result.FParentNode;
end;

function FindNode(ANode: TIpHtmlNode; ACriteria: TIdFindNodeCriteria; const AParamStr: string): TIpHtmlNodeCore;
var
  I: Integer;
  VNode: TIpHtmlNodeMulti;
  VPrevNode, VNextNode: TIpHtmlNode;
begin
  if not Assigned(ANode) or not (ANode is TIpHtmlNodeMulti) then
    Exit(nil);
  VNode := ANode as TIpHtmlNodeMulti;
  if VNode.ChildCount < 1 then
    Exit(nil);
  for I := 0 to Pred(VNode.ChildCount) do
  begin
    VPrevNode := VNode.ChildNode[I];
    VNextNode := FindNode(VPrevNode, ACriteria, AParamStr);
    if not Assigned(VNextNode) then
      VNextNode := VPrevNode;
    if VNextNode is TIpHtmlNodeCore then
    begin
      Result := VNextNode as TIpHtmlNodeCore;
      if ACriteria(Result, AParamStr) then
        Exit;
    end;
  end;
  Result := nil;
end;

function Criteria_FindNodeByElemId(ACurrNode: TIpHtmlNodeCore; const AParamStr: string): Boolean;
begin
  if ACurrNode.Id = AParamStr then
    Exit(True);
  Result := False;
end;

function FindNodeByElemId(ANode: TIpHtmlNode; const AElemId: string): TIpHtmlNodeCore;
begin
  Result := FindNode(ANode, Criteria_FindNodeByElemId, AElemId);
end;

function Criteria_FindNodeByElemClass(ACurrNode: TIpHtmlNodeCore; const AParamStr: string): Boolean;
begin
  if ACurrNode.ClassId = AParamStr then
    Exit(True);
  Result := False;
end;

function FindNodeByElemClass(ANode: TIpHtmlNode; const AElemClass: string): TIpHtmlNodeCore;
begin
  Result := FindNode(ANode, Criteria_FindNodeByElemClass, AElemClass);
end;


(*
  procedure TurnSiblingsOff;
  var
    I: Integer;
    Sibling: TControl;
  begin
    if Parent <> nil then
      with Parent do
        for I := 0 to ControlCount - 1 do begin
          Sibling := Controls[I];
          if (Sibling <> Self)
          and (Sibling is THtmlRadioButton)
          and (Sibling.Tag = Self.Tag) then
            with THtmlRadioButton(Sibling) do
              SetChecked(False);
        end;
  end;

begin
  if FChecked <> Value then begin
    FChecked := Value;
    TabStop := Value;
    if HandleAllocated then
      SendMessage(Handle, BM_SETCHECK, Integer(FChecked), 0);
    if Value then begin
      TurnSiblingsOff;
      inherited Changed;
      if not ClicksDisabled then
        Click;
    end;
  end;
end;
*)

{$IFDEF Html_Print}
procedure GetRelativeAspect(PrinterDC : hDC);
var
  ScreenDC : hDC;
begin
  ScreenDC := GetDC(0);
  try
    Aspect := Printer.XDPI / GetDeviceCaps(ScreenDC, LOGPIXELSX);
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;
{$ENDIF}

constructor TIpHtmlPoolManager.Create(TheItemSize, MaxItems : DWord);
begin
  inherited Create(TheItemSize);
  ClearOnCreate:=true;
end;

function TIpHtmlPoolManager.NewItm : Pointer;
begin
  Result:=NewItem;
end;

(*

constructor TIpHtmlPoolManager.Create(ItemSize, MaxItems : DWord);
begin
  InitializeCriticalSection(Critical);
  EnterCriticalSection(Critical);
  try
    InternalSize := ItemSize;
    while 4096 mod InternalSize <> 0 do
      Inc(InternalSize);
    Root := VirtualAlloc(nil, InternalSize * MaxItems,
      MEM_RESERVE, PAGE_NOACCESS);
    NextPage := Root;
    Next := Root;
  finally
    LeaveCriticalSection(Critical);
  end;
  {Top := Pointer(DWord(Root) + InternalSize * MaxItems);}
end;

destructor TIpHtmlPoolManager.Destroy;
begin
  EnterCriticalSection(Critical);
  try
    if Root <> nil then
      VirtualFree(Root, 0, MEM_RELEASE);
    inherited Destroy;
  finally
    LeaveCriticalSection(Critical);
  end;
  DeleteCriticalSection(Critical);
end;

function TIpHtmlPoolManager.NewItm : Pointer;
begin
  EnterCriticalSection(Critical);
  if Next = NextPage then
    Grow;
  Result := Next;
  Inc(DWord(Next), InternalSize);
  LeaveCriticalSection(Critical);
end;

procedure TIpHtmlPoolManager.Grow;
var
  P: Pointer;
begin
  P := VirtualAlloc(NextPage, 4096, MEM_COMMIT, PAGE_READWRITE);
  if P = nil then
    raise Exception.Create('Out of memory');
  Inc(DWord(NextPage),4096);
end;

procedure TIpHtmlPoolManager.EnumerateItems(Method: TIpEnumItemsMethod);
var
  P : Pointer;
begin
  P := Root;
  while DWord(P) < DWord(Next) do begin
    Method(P);
    Inc(DWord(P), InternalSize);
  end;
end;
*)


procedure SetWordRect(Element: PIpHtmlElement; const Value: TRect);
begin
  Element.WordRect2 := Value;
  if Element.ElementType = etObject then begin
    if (Value.Left < Value.Right)
    and (Value.Bottom > Value.Top)
    and (Value.Left >= 0) and (Value.Top >= 0) then
      TIpHtmlNodeAlignInline(Element.Owner).SetRect(Value);
  end;
end;

function StdIndent: Integer;
begin
  if ScaleBitmaps and (Aspect > 0) then
    Result := round(16 * Aspect)
  else
    Result := 16;
end;

function SizeRec(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

{ TIpHtmlBaseLayouter }

constructor TIpHtmlBaseLayouter.Create(AOwner: TIpHtmlNodeCore);
begin
  inherited Create;
  FOwner := AOwner;
  FBlockMin := -1;
  FBlockMax := -1;
end;

destructor TIpHtmlBaseLayouter.Destroy;
begin
  inherited Destroy;
end;

procedure TIpHtmlBaseLayouter.ClearWordList;
begin
  if FElementQueue <> nil then
    FElementQueue.Clear;
end;

function TIpHtmlBaseLayouter.GetProps: TIpHtmlProps;
begin
  Result := FOwner.Props;
end;

procedure TIpHtmlBaseLayouter.IterateParents(AProc: TIpHtmlNodeIterator);
var
  p: TIpHtmlNode;
  done: Boolean;
begin
  p := FOwner; //.FParentNode;
  done := false;
  while Assigned(p) do
  begin
    AProc(p, Props, done);
    if done then
      break
    else
      p := p.FParentNode;
  end;
end;

procedure TIpHtmlBaseLayouter.ProcessDuplicateLFs;
var
  i: Integer;
  elem: PIpHtmlElement;
  prevelem: PIpHtmlElement;
begin
  i := pred(FElementQueue.Count);
  while i > 0 do begin
    elem := PIpHtmlElement(FElementQueue[i]);
    prevelem := PIpHtmlElement(FElementQueue[i-1]);
    case PIpHtmlElement(FElementQueue[i])^.ElementType of
      etSoftLF:
        if (prevelem.ElementType in [etHardLF, etSoftLF]) then begin
          prevelem.LFHeight := MaxI2(prevelem.LFHeight, elem.LFHeight);
          FElementQueue.Delete(i);
        end;
      etHardLF:
        if (prevelem.ElementType = etSoftLF) then begin
          prevelem.LFHeight := MaxI2(prevelem.LFHeight, elem.LFHeight);
          FElementQueue.Delete(i-1);
        end;
        // nothing to do for etHardLF
    end;
    dec(i);
  end;
end;

procedure TIpHtmlBaseLayouter.RemoveLeadingLFs;
begin
  while (FElementQueue.Count>0)
  and (PIpHtmlElement(FElementQueue[0])^.ElementType in [etSoftLF, etHardLF]) do
    FElementQueue.Delete(0);
end;


{ TIpHtmlBaseTableLayouter }

constructor TIpHtmlBaseTableLayouter.Create(AOwner: TIpHtmlNodeCore);
begin
  inherited Create(AOwner);
  ResetSize;
  FRowSp := TIntArr.Create;
end;

destructor TIpHtmlBaseTableLayouter.Destroy;
begin
  FRowSp.Free;
  inherited Destroy;
end;

procedure TIpHtmlBaseTableLayouter.ResetSize;
begin
  FMin := -1;
  FMax := -1;
end;

{ TIpHtmlNode }

function TIpHtmlNode.GetHint: string;
begin
  Result := '';
end;

constructor TIpHtmlNode.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create;
  if assigned(ParentNode) then
    if ParentNode is TIpHtmlNodeMulti then
      TIpHtmlNodeMulti(ParentNode).FChildren.Add(Self)
    else
      raise EIpHtmlException.Create(SHtmlNotContainer);
  FParentNode := ParentNode;
  if ParentNode <> nil then
    FOwner := ParentNode.Owner;
end;

destructor TIpHtmlNode.Destroy;
begin
  if ((Owner = nil) or not Owner.Destroying)
  and (FParentNode <> nil) then
    TIpHtmlNodeMulti(FParentNode).FChildren.Remove(Self);
end;

function TIpHtmlNode.PageRectToScreen(const Rect: TRect;
  var ScreenRect: TRect): Boolean;
{ -convert coordinates of rect passed in to screen coordinates and
  return false if entire rect is clipped}
var
  Tmp : TRect;
begin
  if (Rect.Left = 0) and (Rect.Right = 0) and
     (Rect.Top  = 0) and (Rect.Bottom = 0) then begin
    Result := False;
    Exit;
  end;
  if not IntersectRect(Tmp, Rect, Owner.FPageViewRect) then begin
    Result := False;
    Exit;
  end;
  ScreenRect := Rect;
  with Owner.FPageViewRect do
    OffsetRect(ScreenRect, -Left, -Top);
  with Owner.FClientRect do
    OffsetRect(ScreenRect, Left, Top);
  if not IntersectRect(Tmp, ScreenRect, Owner.FClientRect) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

procedure TIpHtmlNode.ScreenLine(StartPoint, EndPoint : TPoint;const Width : Integer;
  const Color : TColor);
var
  SaveWidth : Integer;
  aPen: TPen;
  aCanvas: TCanvas;
begin
  StartPoint := PagePtToScreen(StartPoint);
  EndPoint := PagePtToScreen(EndPoint);
  aCanvas := Owner.Target;
  aPen:= aCanvas.Pen;
  SaveWidth := aPen.Width;
  aPen.Width := Width;
  aPen.Color := Color;
  aCanvas.MoveTo(StartPoint.x, StartPoint.y);
  aCanvas.LineTo(EndPoint.x, EndPoint.y);
  aPen.Width := SaveWidth;
end;

procedure TIpHtmlNode.ScreenRect(R : TRect; const Color : TColor);
begin
  if PageRectToScreen(R, R) then begin
    with Owner.Target do begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FrameRect(R);
    end;
  end;
end;

procedure TIpHtmlNode.ScreenFrame(R : TRect; Raised: boolean);
var
  SaveWidth: Integer;
  procedure DoLine(X1,Y1,X2,Y2: Integer; Clr: TColor);
  begin
    with Owner.Target do begin
      Pen.Color := Clr;
      Line(X1,Y1,X2,Y2);
    end;
  end;
begin
  if PageRectToScreen(R, R) then
  with Owner.Target do begin
    Brush.Style := bsSolid;
    SaveWidth := Pen.Width;
    Pen.Width := 1;
    if Raised then begin
      DoLine(R.Left, R.Top, R.Right-1, R.Top, RGB(220,220,220)); // above
      DoLine(R.Right-1, R.Bottom-1, R.Left, R.Bottom-1, RGB(64,64,64)); // below
      DoLine(R.Left, R.Top, r.Left, R.Bottom-1, RGB(192,192,192)); // Left
      DoLine(R.Right-1, R.Bottom-1, R.Right-1, R.Top, RGB(128,128,128)); // Right
    end else begin
      DoLine(R.Left, R.Top, R.Right-1, R.Top, RGB(64,64,64)); // above
      DoLine(R.Right-1, R.Bottom-1, R.Left, R.Bottom-1,RGB(220,220,220) ); // below
      DoLine(R.Left, R.Top, r.Left, R.Bottom-1, RGB(128,128,128)); // Left
      DoLine(R.Right-1, R.Bottom-1, R.Right-1, R.Top, RGB(192,192,192)); // Right
    end;
    Pen.Width := SaveWidth;
  end;
end;

procedure TIpHtmlNode.ScreenPolygon(Points : array of TPoint; const Color : TColor);
var
  Pt : TPoint;
  i : Integer;
  SaveColor : TColor;
begin
  for i := 0 to High(Points) do begin
    Pt := PagePtToScreen(Points[i]);
    Points[i] := Pt;
  end;
  with Owner.Target do begin
    Pen.Color := Color;
    SaveColor := Brush.Color;
    Brush.Color := Color;
    Polygon(Points);
    Brush.Color := SaveColor;
  end;
end;

function TIpHtmlNode.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  with Owner.FPageViewRect do begin
    Dec(Result.x, Left);
    Dec(Result.y, Top);
  end;
  with Owner.FClientRect do begin
    Inc(Result.x, Left);
    Inc(Result.y, Top);
  end;
end;

procedure TIpHtmlNode.ReportDrawRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNode.ReportMapRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNode.InvalidateSize;
begin
  if FParentNode = nil then
    Owner.InvalidateSize
  else
    FParentNode.InvalidateSize;
end;

procedure TIpHtmlNode.EnumChildren(EnumProc: TIpHtmlNodeEnumProc;
  UserData: Pointer);
begin
  EnumProc(Self, UserData);
end;

procedure TIpHtmlNode.SubmitRequest;
begin
  if FParentNode <> nil then
    FParentNode.SubmitRequest;
end;

procedure TIpHtmlNode.ResetRequest;
begin
  if FParentNode <> nil then
    FParentNode.ResetRequest;
end;

procedure TIpHtmlNode.ReportCurDrawRects(Owner: TIpHtmlNode; M : TRectMethod);
begin
  if FParentNode <> nil then
    FParentNode.ReportCurDrawRects(Owner, M);
end;

procedure TIpHtmlNode.AppendSelection(var S: string; var Completed: Boolean);
begin
end;

procedure TIpHtmlNode.CreateControl(Parent: TWinControl);
begin
end;

procedure TIpHtmlNode.Enqueue;
begin

end;

procedure TIpHtmlNode.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

function TIpHtmlNode.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

procedure TIpHtmlNode.HideUnmarkedControl;
begin
end;

procedure TIpHtmlNode.ImageChange(NewPicture: TPicture);
begin
end;

procedure TIpHtmlNode.Invalidate;
begin
end;

procedure TIpHtmlNode.MakeVisible;
begin
end;

procedure TIpHtmlNode.SetProps(const RenderProps: TIpHtmlProps);
begin
end;

procedure TIpHtmlNode.UnmarkControl;
begin
end;

function TIpHtmlNode.GetMargin(AMargin: TIpHtmlElemMargin; ADefault: Integer): Integer;
begin
  Result := ADefault;
end;


{Attribute support code}

function GetPropertyValue(PI: PPropInfo; const AObject: TObject): string;

  function GetPropType : PTypeInfo;
  begin
    Result := PI.PropType;
  end;

  function GetIntegerProperty : string;
  begin
    Result := IntToStr(GetOrdProp(AObject, PI));
  end;

  function GetCharProperty : string;
  begin
    Result := Char(GetOrdProp(AObject, PI));
  end;

  function GetEnumProperty : string;
  begin
    Result := GetEnumName(GetPropType, GetOrdProp(AObject, PI));
  end;

  function GetFloatProperty : string;
  const
    Precisions : array[TFloatType] of Integer = (7, 15, 18, 18, 19);
  begin
    Result := FloatToStrF(GetFloatProp(AObject, PI), ffGeneral,
      Precisions[GetTypeData(GetPropType)^.FloatType], 0);
  end;

  function GetLStringProperty : string;
  begin
    Result := GetStrProp(AObject, PI);
  end;

  function GetWCharProperty : string;
  begin
    Result := Char(GetOrdProp(AObject, PI));
  end;

  function GetVariantProperty : string;
  begin
    Result := AnsiString(GetVariantProp(AObject, PI));
  end;

  function GetStringProperty : string;
  begin
    Result := GetStrProp(AObject, PI);
  end;

  type
    TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  function GetSetProperty : string;
  var
    TypeInfo : PTypeInfo;
    W        : Cardinal;
    I        : Integer;
  begin
    Result := '[';
    W := GetOrdProp(AObject, PI);
    TypeInfo := GetTypeData(GetPropType)^.CompType;
    for I := 0 to Pred(sizeof(Cardinal) * 8) do
      if I in TCardinalSet(W) then begin
        if Length(Result) <> 1 then
          Result := Result + ',';
        Result := Result + GetEnumName(TypeInfo, I);
      end;
    Result := Result + ']';
  end;

begin
  Result := '??';
  case PI.PropType^.Kind of
    tkInteger      : Result := GetIntegerProperty;
    tkChar         : Result := GetCharProperty;
    tkEnumeration  : Result := GetEnumProperty;
    tkFloat        : Result := GetFloatProperty;
    tkLString      : Result := GetLStringProperty;
    tkWChar        : Result := GetWCharProperty;
    tkVariant      : Result := GetVariantProperty;
    tkAString,
    tkString       : Result := GetStringProperty;
    tkSet          : Result := GetSetProperty;
    else             Result := 'unsupported';
  end;
end;

procedure SetPropertyValueLow(PI: PPropInfo;
  const AObject: TObject; const NewValue: string);

  function GetPropType : PTypeInfo;
  begin
    Result := PI.PropType;
  end;

  procedure SetIntegerProperty;
  begin
    SetOrdProp(AObject, PI, StrToInt(NewValue));
  end;

  procedure SetCharProperty;
  begin
    SetOrdProp(AObject, PI, ord(NewValue[1]));
  end;

  procedure SetEnumProperty;
  begin
    SetEnumProp(AObject, PI, NewValue);
  end;

  procedure SetFloatProperty;
  begin
    SetFloatProp(AObject, PI, StrToFloat(NewValue));
  end;

  procedure SetStringProperty;
  begin
    SetStrProp(AObject, PI, NewValue);
  end;

  type
    TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  procedure SetSetProperty;
  begin
    SetSetProp(AObject, PI, NewValue);
  end;

begin
  if not Assigned(PI.SetProc) then
    raise Exception.Create('Property is read-only');
  case PI.PropType^.Kind of
    tkInteger      : SetIntegerProperty;
    tkChar         : SetCharProperty;
    tkEnumeration  : SetEnumProperty;
    tkFloat        : SetFloatProperty;
    tkString,
    tkAString,
    tkLString      : SetStringProperty;
    tkSet          : SetSetProperty;
    else             raise Exception.Create('Unsupported attribute type');
  end;
end;

function GetPropertyList(C: TObject; IncludeValues, IncludeBlanks: Boolean): TStringList;
var
  LCount: Integer;
  LSize: Integer;
  PList : PPropList;
  I, J: Integer;
  S: string;
  SubList: TStringList;
  O: TObject;
begin
  Result := TStringList.Create;
  try
    if (C <> nil) and (C.ClassInfo <> nil) then begin
      LCount := GetPropList(C.ClassInfo, tkProperties, nil);
      LSize := LCount * SizeOf(Pointer);
      if LSize > 0 then begin
        GetMem(PList, LSize);
        try
          GetPropList(C.ClassInfo, tkProperties, PList);
          for I := 0 to LCount-1 do begin
            if PList^[I].PropType^.Kind = tkClass then begin
              SubList := nil;
              try
                O := TObject(GetOrdProp(C, PList^[I]));
                SubList := GetPropertyList(O, IncludeValues, IncludeBlanks);
                for j := 0 to Pred(SubList.Count) do
                  Result.Add(PList^[I]^.Name + '.' + SubList[j]);
              finally
                SubList.Free;
              end;
            end else begin
              if IncludeValues then begin
                S := GetPropertyValue(PList^[I], C);
                if IncludeBlanks or (S <> '') then
                  Result.Add(PList^[I]^.Name + '=' + S);
              end else
                Result.Add(PList^[I]^.Name);
            end;
          end;
        finally
          FreeMem(PList, LSize);
        end;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

procedure SetPropertyValue(C: TObject; PropPath: string; const NewValue: string);
var
  LCount: Integer;
  LSize: Integer;
  PList : PPropList;
  I, J: Integer;
  SubPropPath: string;
  O: TObject;
begin
  I := pos('=', PropPath);
  if I <> 0 then
    SetLength(PropPath, I - 1);
  PropPath := trim(PropPath);
  if PropPath = '' then
    Exit;
  if C.ClassInfo <> nil then begin
    LCount := GetPropList(C.ClassInfo, tkProperties, nil);
    LSize := LCount * SizeOf(Pointer);
    if LSize > 0 then begin
      GetMem(PList, LSize);
      try
        GetPropList(C.ClassInfo, tkProperties, PList);
        for I := 0 to LCount-1 do begin
          if PList^[I].PropType^.Kind = tkClass then begin
            J := pos('.', PropPath);
            if J <> 0 then begin
              SubPropPath := copy(PropPath, 1, J - 1);
              if CompareText(SubPropPath, PList^[I]^.Name) = 0 then begin
                O := TObject(GetOrdProp(C, PList^[I]));
                SetPropertyValue(O, copy(PropPath, J + 1, MAXINT), NewValue);
                Exit;
              end;
            end;
          end else begin
            if CompareText(PropPath, PList^[I]^.Name) = 0 then begin
              SetPropertyValueLow(PList^[I], C, NewValue);
              Exit;
            end;
          end;
        end;
      finally
        FreeMem(PList, LSize);
      end;
    end;
  end;
  raise Exception.Create('Unknown property:' + PropPath);
end;

procedure TIpHtmlNode.GetAttributes(Target: TStrings; IncludeValues, IncludeBlanks: Boolean);
var
  List : TStringList;
begin
  List := GetPropertyList(Self, IncludeValues, IncludeBlanks);
  try
    Target.Assign(List);
  finally
    List.Free;
  end;
end;

procedure TIpHtmlNode.SetAttributeValue(const AttrName, NewValue: string);
begin
  SetPropertyValue(Self, AttrName, NewValue);
end;

function TIpHtmlNode.ExpParentWidth: Integer;
begin
  if assigned(FParentNode) then
    Result := FParentNode.ExpParentWidth
  else
    Result := MAXINT;
end;


{ TIpHtmlNodeMulti }

constructor TIpHtmlNodeMulti.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FChildren := TFPList.Create;
  //Maybe this will create some unespected behavior (Owner=nil)
  if Owner <> nil then
    FProps := TIpHtmlProps.Create(FOwner.PropACache, FOwner.PropBCache);
end;

destructor TIpHtmlNodeMulti.Destroy;
var
  i : Integer;
begin
  if Owner.Destroying then begin
    for i := 0 to Pred(FChildren.Count) do
      TIpHtmlNode(FChildren[I]).Free;
  end else
    while FChildren.Count > 0 do begin
      TIpHtmlNode(FChildren[FChildren.Count - 1]).Free;
    end;
  FChildren.Free;
  if Assigned(FProps) then FreeAndNil(FProps);
  inherited Destroy;
end;

function TIpHtmlNodeMulti.GetChildNode(Index: Integer): TIpHtmlNode;
begin
  Result := TIpHtmlNode(FChildren[Index]);
end;

function TIpHtmlNodeMulti.GetChildCount: Integer;
begin
  Result := FChildren.Count;
end;

procedure TIpHtmlNodeMulti.Enqueue;
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).Enqueue;
end;

procedure TIpHtmlNodeMulti.SetProps(const RenderProps: TIpHtmlProps);
var
  i : Integer;
  savedColor, savedBgColor : TColor;
  IsMouseOver: boolean;
begin
//DebugLn(ClassName, ':', FParentNode.className, ':', IntToStr(RenderProps.BgColor));
  Props.Assign(RenderProps);
  if Self.InheritsFrom(TIpHtmlNodeCore)then
    TIpHtmlNodeCore(Self).LoadAndApplyCSSProps;

//DebugLn(ClassName, ':', FParentNode.className, ':', IntToStr(RenderProps.BgColor));

  IsMouseOver := Self = Owner.FHotNode;
  if IsMouseOver then
  begin
    //DebugLn('MouseOver: ', classname);
    Props.DelayCache:=True;
    if Props.HoverColor <> clNone then
    begin
      savedColor := Props.FontColor;
      Props.FontColor := Props.HoverColor;
    end;
    if Props.HoverBgColor <> clNone then
    begin
      savedBgColor := Props.BgColor;
      Props.BgColor := Props.HoverBgColor;
    end;
    Props.DelayCache:=False;
  end;
  for i := 0 to Pred(FChildren.Count) do
  begin
    TIpHtmlNode(FChildren[i]).SetProps(Props);
{  DebugLn(debugDashs , TIpHtmlNode(FChildren[i]).ClassName,
      ':', TIpHtmlNode(FChildren[i]).FParentNode.ClassName,
      ':', IntToStr(RenderProps.BgColor));
}
  end;
  if IsMouseOver then
  begin
    Props.DelayCache:=True;
    if Props.HoverColor <> clNone then Props.FontColor := savedColor;
    if Props.HoverBgColor <> clNone then Props.BgColor := savedBgColor;
    Props.DelayCache:=False;
  end;
end;

procedure TIpHtmlNodeMulti.ReportDrawRects(M: TRectMethod);
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).ReportDrawRects(M);
end;

procedure TIpHtmlNodeMulti.ReportMapRects(M: TRectMethod);
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).ReportMapRects(M);
end;

procedure TIpHtmlNodeMulti.EnumChildren(EnumProc: TIpHtmlNodeEnumProc;
  UserData: Pointer);
var
  i : Integer;
begin
  inherited;
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).EnumChildren(EnumProc, UserData);
end;

procedure TIpHtmlNodeMulti.AppendSelection(var S: string; var Completed: Boolean);
var
  i : Integer;
begin
  if Completed then
    exit;
  for i := 0 to Pred(FChildren.Count) do
  begin
    TIpHtmlNode(FChildren[i]).AppendSelection(S, Completed);
    if Completed then exit;
  end;
end;

function TIpHtmlNodeMulti.GetMargin(AMargin: TIpHtmlElemMargin;
  ADefault: Integer): Integer;
begin
  if AMargin.Style = hemsPx then
    Result := round(AMargin.Size)
  else
    Result := ADefault;
end;


{ TIpHtmlNodeBODY }

constructor TIpHtmlNodeBODY.Create(ParentNode : TIpHtmlNode);
begin
  inherited Create(ParentNode);
  FElementName := 'body';
  FLink := clNone;
  FVLink := clNone;
  FALink := clNone;
  Owner.FBody := Self;
end;

procedure TIpHtmlNodeBODY.Render(RenderProps: TIpHtmlProps);
var
  MaxX, MaxY: Integer;
  X, Y : Integer;
  P : TPoint;
begin
  if ScaleBitmaps then begin
    Owner.Target.Brush.Color := Owner.BgColor;
    Owner.Target.FillRect(Owner.ClientRect);
  end else begin
    // Fill page with background color
    if BGColor <> clNone then begin
      Owner.Target.Brush.Color := BGColor;
      Owner.Target.FillRect(Owner.ClientRect);
    end else begin
      Owner.Target.Brush.Color := Owner.BGColor;
      Owner.Target.FillRect(Owner.ClientRect);
    end;

    // Draw background image
    if Background <> '' then begin
      if BgPicture = nil then
        Owner.DoGetImage(Self, Owner.BuildPath(Background), FBgPicture);
      if (BgPicture <> nil) and (BgPicture.Height>0) and (BgPicture.Width>0) then begin
        MaxX := MaxI2(PageRect.Right, Owner.ClientRect.Right);
        MaxY := MaxI2(PageRect.Bottom, Owner.ClientRect.Bottom);
        Y := 0;
        while (Y <= MaxY{PageRect.Bottom}) do begin
          if (Y < Owner.PageViewRect.Top - BgPicture.Height)
          or (Y > Owner.PageViewRect.Bottom) then
          else begin
            X := 0;
            while (X <= MaxX{PageRect.Right}) do begin
              P := PagePtToScreen(Point(X, Y));
              Owner.Target.Draw(P.X, P.Y, BgPicture.Graphic);
              Inc(X, BgPicture.Width);
            end;
          end;
          Inc(Y, BgPicture.Height);
        end;
      end;
    end;
  end;

  inherited Render(RenderProps);

  // Restore style
  Owner.Target.Brush.Style:=bsSolid;
end;

procedure TIpHtmlNodeBODY.LoadAndApplyCSSProps;
var
  LinkProps: TCSSProps;
begin
  Props.DelayCache := True;
  inherited LoadAndApplyCSSProps;
  LinkProps := Owner.CSS.GetPropsObject('a:link', '');
  if (LinkProps <> nil) and (LinkProps.Color <> clNone) then
    Link := LinkProps.Color;
  LinkProps := Owner.CSS.GetPropsObject('a:visited', '');
  if (LinkProps <> nil) and (LinkProps.Color <> clNone) then
    VLink := LinkProps.Color;
  LinkProps := Owner.CSS.GetPropsObject('a:active', '');
  if (LinkProps <> nil) and (LinkProps.Color <> clNone) then
    ALink := LinkProps.Color;
  Props.DelayCache := True;
end;

destructor TIpHtmlNodeBODY.Destroy;
begin
  inherited;
  BgPicture.Free;
end;

procedure TIpHtmlNodeBODY.ImageChange(NewPicture: TPicture);
begin
  {$IFOPT C+}
  Owner.CheckImage(NewPicture);
  {$ENDIF}
  FBgPicture.Free;
  FBgPicture := NewPicture;
  Invalidate;
end;

procedure TIpHtmlNodeBODY.SetAlink(const Value: TColor);
begin
  if Value <> FAlink then begin
    Falink := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetLink(const Value: TColor);
begin
  if Value <> FLink then begin
    FLink := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBODY.SetVlink(const Value: TColor);
begin
  if Value <> FVLink then begin
    FVLink := Value;
    InvalidateSize;
  end;
end;

{ TIpHtml }

procedure TIpHtml.AddWordEntry(const Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
var
  Entry :  PIpHtmlElement;
  L : Integer;
begin
  Entry := NewElement(etWord, Owner);
  Entry.Props := Props;
  Entry.AnsiWord := Value;
  Entry.IsBlank := 0;
  L := length(Entry.AnsiWord);
  while Entry.IsBlank < L do
    if Entry.AnsiWord[Entry.IsBlank + 1] = ' ' then
      Inc(Entry.IsBlank)
    else
      break;
  if Entry.IsBlank < L  then
    Entry.IsBlank := 0;
  Owner.EnqueueElement(Entry);
end;

procedure TIpHtml.AddWord(Value: string; Props: TIpHtmlProps; Owner: TIpHtmlNode);
var
  P : Integer;
begin
  if FDocCharset<>'' then
    Value := ConvertEncoding(Value, FDocCharset, 'UTF-8');
  Value:= EscapeToAnsi(Value);
  P := CharPos(ShyChar, Value);
  if P = 0 then
    AddWordEntry(Value, Props, Owner)
  else begin
    while Value <> '' do begin
      AddWordEntry(copy(Value, 1, P - 1), Props, Owner);
      Delete(Value, 1, P);
      if Value <> '' then
        Owner.EnqueueElement(SoftHyphen);
      P := CharPos(ShyChar, Value);
      if P = 0 then
        P := length(Value) + 1;
    end;
  end;
end;

procedure TIpHtml.InvalidateRect(R: TRect);
begin
  if Assigned(FOnInvalidateRect) then
    FOnInvalidateRect(Self, R);
end;

procedure TIpHtml.Clear;
{- clear any contents}
var
  i : Integer;
begin
  {$IFDEF UseGifImageUnit}
  for i := 0 to Pred(GifImages.Count) do
    if TIpHtmlNodeIMG(GifImages[i]).FPicture <> nil then
      TGifImage(TIpHtmlNodeIMG(GifImages[i]).FPicture.Graphic).PaintStop;
  {$ELSE}
  for i := 0 to Pred(AnimationFrames.Count) do
    if TIpHtmlNodeIMG(AnimationFrames[i]).Picture <> nil then
      TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).Picture.Graphic).
        AggressiveDrawing := False;
  {$ENDIF}
  ClearGifQueue;
  FHotNode := nil;
  FHtml.Free;
  FHtml := TIpHtmlNodeHtml.Create(nil);
  FHtml.FOwner := Self;
end;

procedure TIpHtml.ReportReferences(Node : TIpHtmlNode);
var
  i : Integer;
  S : string;
begin
  if Node is TIpHtmlNodeA then
    S := Trim(TIpHtmlNodeA(Node).HRef)
  else
  if Node is TIpHtmlNodeAREA then
    S := Trim(TIpHtmlNodeAREA(Node).HRef);

  if (S <> '') then
    ReportReference(S);

  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ReportReferences(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.LoadFromStream(S: TStream);
begin
  DoneLoading := False;
  try
    FHasFrames := False;
    Clear;
    CharStream := S;
    Parse;
    ReportReferences(HtmlNode);
  finally
    DoneLoading := True;
    FCanPaint := True;
  end;                      
end;

procedure TrimFormatting(const S: string; Target: PAnsiChar; PreFormatted: Boolean = False);
var
  R, W : Integer;

  procedure CopyChar(ch: AnsiChar);
  begin
    Target[w] := ch;
    Inc(w);
  end;

begin
  r := 1;
  w := 0;
  while r <= length(S) do begin
    case S[r] of
    #0..#8, #11..#12, #14..#31 :
      ;
    #9 :
      if PreFormatted then
        CopyChar(' ');
    #13 :
      if PreFormatted then
        CopyChar(LF);
    #10 :
      if PreFormatted then begin
        if (w = 0) or (Target[w-1] <> LF) then
          CopyChar(LF);
      end
      else begin
        if w > 1 then
          CopyChar(' ');
      end;
    ' ' :
      if PreFormatted or (w = 0) or (Target[w-1] <> ' ') then
        CopyChar(' ');
    else
      CopyChar(S[r]);
    end;
    Inc(r);
  end;
  Target[w] := #0;
end;

function TIpHtml.FindAttribute(const AttrNameSet : TIpHtmlAttributesSet) : string;
begin
  if FParser <> nil then
    Result := FParser.FindAttribute(AttrNameSet)
  else
    Result := '';
end;

function CalcMultiLength(const List: TIpHtmlMultiLengthList;
  Avail: Integer; var Sections: Integer): TIntArr;
var
  OrgAvail, i, S : Integer;
begin
  Result := TIntArr.Create;
  if List.Entries = 0 then begin
    Sections := 1;
    Result[0] := Avail;
    Exit;
  end;
  OrgAvail := Avail;
  Sections := List.Entries;
  for i := 0 to Pred(List.Entries) do begin
    if List.Values[i].LengthType = hmlAbsolute then begin
      if Avail >= List.Values[i].LengthValue then begin
        Result[i] := List.Values[i].LengthValue;
        Dec(Avail, Result[i]);
      end else begin
        Result[i] := Avail;
        Avail := 0;
      end;
    end else
      Result[i] := 0;
  end;
  if Avail > 0 then begin
    for i := 0 to Pred(List.Entries) do
      if List.Values[i].LengthType = hmlPercent then
        Result[i] := round(List.Values[i].LengthValue * Avail / 100);
    for i := 0 to Pred(List.Entries) do
      if List.Values[i].LengthType = hmlPercent then
        Dec(Avail, Result[i]);
    if Avail > 0 then begin
      S := 0;
      for i := 0 to Pred(List.Entries) do
        if (List.Values[i].LengthType = hmlRelative) then
          Inc(S, List.Values[i].LengthValue);
      if S > 0 then
        for i := 0 to Pred(List.Entries) do
          if (List.Values[i].LengthType = hmlRelative) then begin
            Result[i] := round(List.Values[i].LengthValue * Avail / S);
            Dec(Avail, Result[i]);
          end;
      if Avail > 0 then
        for i := 0 to Pred(List.Entries) do
          if (List.Values[i].LengthType = hmlRelative)
          and (List.Values[i].LengthValue = 0) then begin
            Result[i] := Avail;
            break;
          end;
    end;
  end;
  repeat
    S := 0;
    for i := 0 to Pred(List.Entries) do
      Inc(S, Result[i]);
    S := OrgAvail - S;
    if S > 0 then
      for i := 0 to Pred(List.Entries) do begin
        Result[i] := Result[i] + 1;
        Dec(S);
        if S = 0 then break;
      end;
    if S < 0 then
      for i := 0 to Pred(List.Entries) do begin
        Result[i] := Result[i] - 1;
        Inc(S);
        if S = 0 then break;
      end;
  until S = 0;
end;
         
procedure TIpHtml.FixMissingBodyTag;
var
  i: Integer;
  node: TIpHtmlNode;
begin
  { Does the HTML include a body node? }
  if not FHtml.HasBodyNode then
    { No --> Create a body node under FHtml. }
    with FHtml do
    begin
      with TIpHtmlNodeBODY.Create(FHtml) do
        LoadAndApplyCSSProps;
      { Make each of FHtml's current children the children of the Body node. }
      for i := Pred(ChildCount) downto 0 do
      begin
        node := ChildNode[i];
        if node <> Body then
        begin
          FChildren.Remove(node);
          node.FParentNode := Body;
          Body.FChildren.Insert(0, node);
        end;
      end;
    end;
end;
      
procedure TIpHtml.Parse;
begin
  FParser := TIpHtmlParser.Create(Self, CharStream);
  try
    if FParser.Execute then begin
      FTitleNode := TIpHtmlParser(FParser).TitleNode;
      FCurFrameSet := TIpHtmlParser(FParser).FrameSet;
      FDocCharSet := TIpHtmlParser(FParser).DocCharSet;
      FHasFrames := TIpHtmlParser(FParser).HasFrames;
    end;
  finally
    FreeAndNil(FParser);
  end;
end;

constructor TIpHtml.Create;
var
  TmpBitmap: TGraphic;
begin
  inherited Create;
  PropACache := TIpHtmlPropsAList.Create;
  PropBCache := TIpHtmlPropsBList.Create;
  ElementPool := TIpHtmlPoolManager.Create(sizeof(TIpHtmlElement), MaxElements);
  SoftLF := BuildStandardEntry(etSoftLF);
  HardLF := BuildStandardEntry(etHardLF);
  HardLFClearLeft := BuildStandardEntry(etClearLeft);
  HardLFClearRight := BuildStandardEntry(etClearRight);
  HardLFClearBoth := BuildStandardEntry(etClearBoth);
  FLIndent := BuildStandardEntry(etIndent);
  FLOutdent := BuildStandardEntry(etOutdent);
  SoftHyphen := BuildStandardEntry(etSoftHyphen);
  DefaultProps := TIpHtmlProps.Create(PropACache, PropBCache);
  FHtml := TIpHtmlNodeHtml.Create(nil);
  FHtml.FOwner := Self;
  AnchorList := TFPList.Create;
  MapList := TFPList.Create;
  AreaList := TFPList.Create;
  MapImgList := TFPList.Create;
  RectList := TFPList.Create;
  FControlList := TFPList.Create;
  LinkColor := clBlue;
  VLinkColor := clPurple;
  ALinkColor := clRed;
  FLinksUnderlined := DEFAULT_LINKS_UNDERLINED;
  FCSS := TCSSGlobalProps.Create;
  FTabList := TIpHtmlTabList.Create;
  {$IFDEF UseGifImageUnit}
  GifImages := TFPList.Create;
  {$ELSE}
  AnimationFrames := TFPList.Create;
  {$ENDIF}
  NameList := TStringListUTF8Fast.Create;
  IdList := TStringListUTF8Fast.Create;
  DefaultImage := TPicture.Create;
  TmpBitmap := nil;
  try
    if LazarusResources.Find('DEFAULTIMAGE')<>nil then
      TmpBitmap := CreateBitmapFromLazarusResource('DEFAULTIMAGE')
    else
      TmpBitmap := CreateBitmapFromResourceName(HInstance, 'DEFAULTIMAGE');
    DefaultImage.Graphic := TmpBitmap;
  finally
    TmpBitmap.Free;
  end;
  GifQueue := TFPList.Create;
  FStartSel.x := -1;
  FEndSel.x := -1;
  //FixedTypeface := 'Courier New';
  FBgColor := clNone;
  FFactBAParag := 1;
  NeedResize := True;
end;

function TIpHtml.LinkVisited(const URL : string): Boolean;
begin
  if (length(URL) > 0) and (URL[1] = '#') then
    Result := True
  else
    Result := CheckKnownURL(URL);
end;

{$IFOPT C+}
procedure TIpHtml.CheckImage(Picture: TPicture);
begin
  if Picture <> nil then begin
    if not (Picture is TPicture) then
      raise EIpHtmlException.Create(SHTMLInvPicture);
    if Picture.Graphic = nil then
      raise EIpHtmlException.Create(SHTMLNoGraphic);
    if not (Picture.Graphic is TGraphic) then
      raise EIpHtmlException.Create(SHTMLInvGraphic);
  end;
end;
{$ENDIF}

procedure TIpHtml.DoGetImage(Sender: TIpHtmlNode; const URL: string; var Picture: TPicture);
begin
  if assigned(FOnGetImageX) then
    OnGetImageX(Sender, URL, Picture)
  else
    raise EIpHtmlException.Create(SHTMLNoGetImage);
  {$IFOPT C+}
  CheckImage(Picture);
  {$ENDIF}
end;

procedure TIpHtml.FinalizeRecs(P: Pointer);
begin
  with PIpHtmlElement(P)^ do begin
    //ElementType : TElementType;
    AnsiWord:='';
    //IsBlank : Integer;
    //SizeProp: TIpHtmlPropA;
    //Size: TSize;
    //WordRect2 : TRect;
    //Props : TIpHtmlProps;
    //Owner : TIpHtmlNode;
  end;
end;

destructor TIpHtml.Destroy;
var
  i : Integer;
begin
 FCSS.Free;
    {$IFDEF UseGifImageUnit}
    for i := 0 to Pred(GifImages.Count) do
      if TIpHtmlNodeIMG(GifImages[i]).Picture <> nil then
        TGifImage(TIpHtmlNodeIMG(GifImages[i]).Picture.Graphic).PaintStop;
    {$ELSE}
    for i := 0 to Pred(AnimationFrames.Count) do
      if TIpHtmlNodeIMG(AnimationFrames[i]).Picture <> nil then
        TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).Picture.Graphic).
          AggressiveDrawing := False;
    {$ENDIF}
  Destroying := True;
  PaintBufferBitmap.Free;
  ClearGifQueue;
  Clear;
  GifQueue.Free;
  DefaultImage.Free;
  NameList.Free;
  IdList.Free;
  FHtml.Free;
  AnchorList.Free;
  MapList.Free;
  AreaList.Free;
  ClearRectList;
  RectList.Free;
  MapImgList.Free;
  FControlList.Free;
  DefaultProps.Free;
  FTabList.Free;
  {$IFDEF UseGifImageUnit}
  GifImages.Free;
  {$ELSE}
  AnimationFrames.Free;
  {$ENDIF}
  ElementPool.EnumerateItems(FinalizeRecs);
  ElementPool.Free;
  PropACache.Free;
  PropBCache.Free;
  inherited;
end;
                     
procedure TIpHtml.SetDefaultProps;
begin
  if (FDefaultTypeFace='') or SameText(FDefaultTypeFace, 'default') then begin
    {$IFDEF MSWindows}
    Defaultprops.FontName := 'Times New Roman';
    {$ELSE}
    Defaultprops.FontName := Graphics.DefFontData.Name
    {$ENDIF}
  end else
    Defaultprops.FontName := FDefaultTypeface;
  Defaultprops.FontSize := FDefaultFontSize;
  DefaultProps.BaseFontSize := 3;
  Defaultprops.FontBaseline := 0;
  DefaultProps.VAlignment := hva3Baseline;
  Defaultprops.FontStyle := [];
  Defaultprops.Alignment := haLeft;
  DefaultProps.FontColor := TextColor;
  DefaultProps.LinkColor := LinkColor;
  DefaultProps.VLinkColor := VLinkColor;
  DefaultProps.ALinkColor := ALinkColor;
  DefaultProps.BgColor := BgColor;
  DefaultProps.Preformatted := False;
  DefaultProps.NoBreak := False;
  if Body <> nil then begin
    if Body.TextColor <> clNone then
      DefaultProps.FontColor := Body.TextColor;
    if Body.Link <> clNone then
      DefaultProps.LinkColor := Body.Link;
    if Body.VLink <> clNone then
      DefaultProps.VLinkColor := Body.VLink;
    if Body.ALink <> clNone then
      DefaultProps.ALinkColor := Body.ALink;
    if Body.BgColor <> clNone then
      DefaultProps.BgColor := Body.BgColor;
  end;
end;

function TIpHtml.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  with FPageViewRect do begin
    Dec(Result.x, Left);
    Dec(Result.y, Top);
  end;
  with FClientRect do begin
    Inc(Result.x, Left);
    Inc(Result.y, Top);
  end;
end;

function TIpHtml.PageRectToScreen(const Rect: TRect; var ScreenRect: TRect): Boolean;
{-convert coordinates of rect passed in to screen coordinates and
  return false if entire rect is clipped}
var
  Tmp : TRect;
begin
  if (Rect.Left = 0) and (Rect.Right = 0) and
     (Rect.Top  = 0) and (Rect.Bottom = 0) then begin
    Result := False;
    Exit;
  end;
  if not IntersectRect(Tmp, Rect, FPageViewRect) then begin
    Result := False;
    Exit;
  end;
  ScreenRect := Rect;
  with FPageViewRect do
    OffsetRect(ScreenRect, -Left, -Top);
  with FClientRect do
    OffsetRect(ScreenRect, Left, Top);
  if not IntersectRect(Tmp, ScreenRect, FClientRect) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TIpHtml.GetSelectionBlocks(out StartSelIndex,EndSelIndex: Integer): boolean;
var
  R : TRect;
  //CurBlock: TIpHtmlNodeBlock;
begin
  Result := false;

  if not FAllSelected
  and ((FStartSel.x < 0) or (FEndSel.x < 0)) then Exit;

  if not FAllSelected then begin
    //CurBlock := nil;
    // search blocks that intersect the selection
    // 1.- find first block that intersects upleft point of sel. (start from 0)
    StartSelIndex := 0;
    while StartSelIndex < RectList.Count do begin
      //CurBlock := PIpHtmlRectListEntry(RectList[StartSelIndex]).Block;
      {if FAllSelected and (CurBlock <> nil) then
        break;}
//      if PtInRect(CurBlock.PageRect, FStartSel) then begin
        R := PIpHtmlRectListEntry(RectList[StartSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          // block within selection (vertically)
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          // selection start or ends in this block
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
//      end;
      Inc(StartSelIndex);
    end;
    if StartSelIndex >= RectList.Count then Exit;
    // 2.- find first block that intersects downright point of sel. (start from count-1)
    EndSelIndex := Pred(RectList.Count);
    while EndSelIndex >= StartSelIndex do begin
 //     if PIpHtmlRectListEntry(RectList[EndSelIndex]).Block = CurBlock then begin
        {if FAllSelected then
          break;}
        R := PIpHtmlRectListEntry(RectList[EndSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
//      end;
      Dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := RectList.Count - 1;
  end;
  Result := True;
end;

function TIpHtml.getControlCount:integer;
begin
  result := FControlList.Count;
end;

function TIpHtml.getControl(i:integer):TIpHtmlNode;
begin
  result := FControlList[i];
end;

procedure TIpHtml.PaintSelection;
var
  StartSelIndex, EndSelIndex,
  i : Integer;
  R : TRect;
  CurBlock: TIpHtmlNodeBlock;
begin
  if not FAllSelected
  and ((FStartSel.x < 0) or (FEndSel.x < 0)) then Exit;
  if not FAllSelected then begin
    CurBlock := nil;
    StartSelIndex := 0;
    while StartSelIndex < RectList.Count do begin
      CurBlock := PIpHtmlRectListEntry(RectList[StartSelIndex]).Block;
      {if FAllSelected and (CurBlock <> nil) then
        break;}
      if PtInRect(CurBlock.PageRect, FStartSel) then begin
        R := PIpHtmlRectListEntry(RectList[StartSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      Inc(StartSelIndex);
    end;
    if StartSelIndex >= RectList.Count then Exit;
    EndSelIndex := Pred(RectList.Count);
    while EndSelIndex >= StartSelIndex do begin
      if PIpHtmlRectListEntry(RectList[EndSelIndex]).Block = CurBlock then begin
        {if FAllSelected then
          break;}
        R := PIpHtmlRectListEntry(RectList[EndSelIndex]).Rect;
        if R.Bottom = 0 then
        else
        if (R.Top > FStartSel.y) and (R.Bottom < FEndSel.y) then
          break
        else
        if PtInRect(R, FStartSel) or PtInRect(R, FEndSel) then
          break
        else
        if (R.Bottom < FStartSel.y) then
        else
        if (R.Top > FEndSel.Y) then
        else
          if (R.Left >= FStartSel.x) and (R.Right <= FEndSel.x) then
            break;
      end;
      Dec(EndSelIndex);
    end;
  end else begin
    StartSelIndex := 0;
    EndSelIndex := RectList.Count - 1;
  end;
  for i := StartSelIndex to EndSelIndex do begin
    R := PIpHtmlRectListEntry(RectList[i]).Rect;
    if PageRectToScreen(R, R) then begin
      DebugLn('TIpHtml.PaintSelection  PatBlt not implemented');
      (*
      PatBlt(PaintBuffer.Handle, R.Left, R.Top,
        R.Right - R.Left, R.Bottom - R.Top, DSTINVERT);
      *)
    end;
  end;
end;

procedure TIpHtml.RequestImageNodes(Node : TIpHtmlNode);
var
  i : Integer;
begin
  if Node is TIpHtmlNodeIMG then begin
    if TIpHtmlNodeIMG(Node).Picture = nil then
      TIpHtmlNodeIMG(Node).LoadImage;
  end;
  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do begin
      RequestImageNodes(TIpHtmlNodeMulti(Node).ChildNode[i]);
    end;
end;

{$IFDEF IP_LAZARUS_DBG}
var
  CCC: Integer;
  
procedure TIpHtml.DebugChild(Node: TIpHtmlNode; const UserData: Pointer);
var
  i: Integer;
begin
  if Node=UserData then
    Write('Parent: ');
  for i:=0 to CCC do Write(' ');
  Write('Node: ', Node.ClassName);
  if Node is TIpHtmlNodeText then
    Write(' ', TIpHtmlNodeText(NodE).ANSIText);
  WriteLn;
  if Node=UserData then
    Exit;
  Inc(CCC);
  Node.EnumChildren(DebugChild, Node);
  Dec(CCC);
end;

procedure TIpHtml.DebugAll;
//var
  //i: Integer;
  //item: PIpHtmlRectListEntry;
  //Node: TIpHtmlNode;
begin
  CCC := 0;
  Fhtml.EnumChildren(DebugChild, FHtml);
  {
  
  for i:=0 to RectList.Count-1 do begin
    WriteLn('RectList[',i,']:');
    Item := PIpHtmlRectListEntry(Rectlist[i]);
    if Item<>nil then begin
      WriteLn(' Node=', dbgs(Item.Node));
      WriteLn('   Owner=', dbgs(Item.Node^.Owner));
      WriteLn('    Text=', Item.Node^.AnsiWord);
      Node := Item.Node^.Owner;
      if Node<>nil then begin
      WriteLn('     ClassName:', Node.ClassName);
        if Node is TIpHtmlNodeText then
          WriteLn('       Text=', TIpHtmlNodeText(Node).ANSIText);
      end;
      WriteLn(' Block=', dbgs(Item.Block));
      WriteLn(' Rect=', dbgs(Item.Rect));
    end;
  end;
  }
end;
{$ENDIF}

procedure TIpHtml.Render(TargetCanvas: TCanvas; TargetPageRect: TRect;
  UsePaintBuffer: Boolean; const TopLeft: TPoint);
begin
  Render(TargetCanvas, TargetPageRect, TargetPageRect.Top, TargetPageRect.Bottom,
    UsePaintBuffer, TopLeft);
end;

procedure TIpHtml.Render(TargetCanvas: TCanvas; TargetPageRect: TRect;
  APageTop, APageBottom: Integer; UsePaintBuffer: Boolean; const TopLeft: TPoint);
var
  i : Integer;
begin
  FClientRect.TopLeft := TopLeft; {Point(0, 0);}
  FClientRect.Right := TargetPageRect.Right - TargetPageRect.Left;
  FClientRect.Bottom := TargetPageRect.Bottom - TargetPageRect.Top;
  if not DoneLoading then begin
    TargetCanvas.FillRect(FClientRect);
    Exit;
  end;
    {$IFDEF UseGifImageUnit}
    for i := 0 to Pred(GifImages.Count) do
      if TIpHtmlNodeIMG(GifImages[i]).Picture <> nil then
        with TGifImage(TIpHtmlNodeIMG(GifImages[i]).Picture.Graphic) do
          if Painters <> nil then
            PaintStop;
    {$ELSE}
    for i := 0 to Pred(AnimationFrames.Count) do
      if TIpHtmlNodeIMG(AnimationFrames[i]).Picture <> nil then
        with TIpAnimatedGraphic(TIpHtmlNodeIMG(AnimationFrames[i]).Picture.Graphic) do
          AggressiveDrawing := False;
    {$ENDIF}

  for i := 0 to Pred(FControlList.Count) do
    TIpHtmlNode(FControlList[i]).UnmarkControl;
  if NeedResize then
    SetDefaultProps;
  FPageViewRect := TargetPageRect;
  { Note: In Preview mode the page is tiled of "mini-pages" sized PageViewRect.
    The lower end of the "real" page is given by PageViewBottom. We set here
    its default. The value needed for the preview will be set there. }
  FPageViewBottom := APageBottom;
  FPageViewTop := APageTop;

  if UsePaintBuffer then begin
    if (PaintBuffer = nil)
    or (PaintBufferBitmap.Width <> FClientRect.Right)
    or (PaintBufferBitmap.Height <> FClientRect.Bottom) then begin
      PaintBufferBitmap.Free;
      PaintBufferBitmap := TBitmap.Create;
      PaintBufferBitmap.Width := FClientRect.Right;
      PaintBufferBitmap.Height := FClientRect.Bottom;
      PaintBuffer := PaintBufferBitmap.Canvas;
    end;
    FTarget := PaintBuffer;
  end else begin
    PaintBuffer := TargetCanvas;
    FTarget := TargetCanvas;
  end;
  ClearRectList;
  if FHtml <> nil then
    FHtml.Render(DefaultProps);

  for i := 0 to Pred(FControlList.Count) do
    TIpHtmlNode(FControlList[i]).HideUnmarkedControl;
  if UsePaintBuffer then
    TargetCanvas.CopyRect(FClientRect, PaintBuffer, FClientRect)
  else
    if PaintBufferBitmap <> nil then
      PaintBuffer := PaintBufferBitmap.Canvas
    else
      PaintBuffer := nil;
  StartGifPaint(TargetCanvas);
  {Request all non-visible images}
  RequestImageNodes(HtmlNode);
end;

procedure TIpHtml.ResetElementMetrics(P: Pointer);
begin
  with PIpHtmlElement(P)^ do begin
    Size.cx := 0;
    Size.cy := 0;
    WordRect2 := Rect(0, 0, 0, 0);
    SizeProp := nil;
  end;
end;

procedure TIpHtml.ResetWordLists;
begin
  ElementPool.EnumerateItems(ResetElementMetrics);
end;

procedure TIpHtml.ResetBlocks(Node: TIpHtmlNode);
var
  i : Integer;
begin
  if Node = nil then Exit;
  if Node is TIpHtmlNodeBlock then
    TIpHtmlNodeBlock(Node).InvalidateSize
  else
  if Node is TIpHtmlNodeTable then
    TIpHtmlNodeTable(Node).FLayouter.ResetSize;
  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ResetBlocks(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.ResetImages(Node: TIpHtmlNode);
var
  i : Integer;
begin
  if Node = nil then Exit;
  if Node is TIpHtmlNodeIMG then
    with TIpHtmlNodeIMG(Node) do begin
      {UnloadImage;}
      InvalidateSize;
    end
  else
  if Node is TIpHtmlNodeMulti then
    for i := 0 to Pred(TIpHtmlNodeMulti(Node).ChildCount) do
      ResetImages(TIpHtmlNodeMulti(Node).ChildNode[i]);
end;

procedure TIpHtml.ResetCanvasData;
begin
  PropACache.ResetCache;
  ResetWordLists;
  ResetBlocks(FHtml);
  ResetImages(FHtml);
end;

function TIpHtml.GetPageRect(TargetCanvas: TCanvas; Width, Height : Integer): TRect;
var
  DefPageRect : TRect;
  Min, Max, W, H : Integer;
begin
  //debugln(['TIpHtml.GetPageRect START DoneLoading=',DoneLoading,' FHtml=',FHtml<>nil]);
  if not DoneLoading then begin
    // always set Result
    SetRectEmpty(Result);
    Exit;
  end;
  DoneLoading := False;
  SetRectEmpty(FPageRect);
  if FHtml <> nil then begin
    if (TargetCanvas <> RenderCanvas)
    or (FPageHeight <> Height) then
      ResetCanvasData;
    FPageHeight := Height;
    SetDefaultProps;
    {PanelWidth := Width;}
    FTarget := TargetCanvas;
    FHtml.CalcMinMaxHtmlWidth(DefaultProps, Min, Max);
    //debugln(['TIpHtml.GetPageRect Min=',Min,' Max=',Max]);
    W := MaxI2(Min + 2 * MarginWidth, Width);
    H := FHtml.GetHeight(DefaultProps, W - 2 * MarginWidth) + 2 * MarginHeight;
    DefPageRect := Rect(
      MarginWidth,
      MarginHeight,
      W - MarginWidth,
      H - MarginHeight);
    ClearAreaLists;
    ClearAreaList;
    FHtml.Layout(DefaultProps, DefPageRect);
    FPageRect := DefPageRect;
    FPagerect.Bottom := FPageRect.Bottom + MarginHeight;
    FPageRect.Right := FPageRect.Right + MarginWidth;
    RenderCanvas := TargetCanvas;
  end;
  Result := FPageRect;
  DoneLoading := True;
end;

procedure TIpHtml.InvalidateSize;
begin
  if assigned(FOnInvalidateSize) then
    FOnInvalidateSize(Self);
end;

procedure TIpHtml.ClearAreaList;
var
  i : Integer;
begin
  for i := 0 to Pred(AreaList.Count) do
    TIpHtmlNodeArea(AreaList[i]).Reset;
  AreaList.Clear;
end;

function RectFromString(const S: string): TRect;
var
  i, j, x, err : Integer;

  procedure Next;
  begin
    j := i;
    while (j <= length(S)) and (S[j] <> ',') do
      Inc(j);
    val(copy(S, i, j - i), x, err);
  end;

begin
  SetRectEmpty(Result);
  i := 1;
  Next;
  if err <> 0 then Exit;
  Result.Left := x;
  i := j + 1;
  Next;
  if err <> 0 then Exit;
  Result.Top := x;
  i := j + 1;
  Next;
  if err <> 0 then Exit;
  Result.Right := x;
  i := j + 1;
  Next;
  if err <> 0 then Exit;
  Result.Bottom := x;
end;

function CircularRegion(const Coords: string; const Rect: TRect): HRgn;
var
  i, j, err, cx, cy, R : Integer;
begin
  Result := 0;
  i := 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') do
    Inc(j);
  val(copy(Coords, i, j - i), cx, err);
  if err <> 0 then Exit;
  i := j + 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') do
    Inc(j);
  val(copy(Coords, i, j - i), cy, err);
  if err <> 0 then Exit;
  i := j + 1;
  j := i;
  while (j <= length(Coords)) and (Coords[j] <> ',') and (Coords[j] <> '%') do
    Inc(j);
  val(copy(Coords, i, j - i), R, err);
  if err <> 0 then Exit;
  if (j <= length(Coords)) and (Coords[j] = '%') then
    R := round(R * MinI2(Rect.Right - Rect.Left, Rect.Bottom - Rect.Top) / 100);
  if R < 1 then Exit;
  Result := CreateEllipticRgn(
    Rect.Left + cx - R,
    Rect.Top + cy - R,
    Rect.Left + cx + R,
    Rect.Top + cy + R);
end;

function PolygonRegion(const Coords: string; const Rect: TRect): HRgn;
const
  MAXPOINTS = 4096;
var
  Points : array [0.. Pred(MAXPOINTS)] of TPoint;
  Count, i, j, x, y, err : Integer;
begin
  Result := 0;
  Count := 0;
  i := 1;
  while i < length(Coords) do begin
    j := i;
    while (j <= length(Coords)) and (Coords[j] <> ',') do
      Inc(j);
    val(copy(Coords, i, j - i), x, err);
    if err <> 0 then Exit;
    i := j + 1;
    j := i;
    while (j <= length(Coords)) and (Coords[j] <> ',') do
      Inc(j);
    val(copy(Coords, i, j - i), y, err);
    if err <> 0 then Exit;
    Points[Count].x := x + Rect.Left;
    Points[Count].y := y + Rect.Top;
    Inc(Count);
    i := j + 1;
  end;
  if Count < 3 then Exit;
  if (Points[0].x <> Points[Count - 1].x)
  or (Points[0].y <> Points[Count - 1].y) then begin
    Points[Count] := Points[0];
    Inc(Count);
  end;
  Result := CreatePolygonRgn(
    PPoint(@Points[0]),
    Count,
    ALTERNATE); {fill mode is irrelevant here}
end;

procedure TIpHtml.BuildAreaList;
var
  i, j, k : Integer;
  R, R2 : TRect;
begin
  ClearAreaList;
  for i := 0 to Pred(MapImgList.Count) do
    with TIpHtmlNodeIMG(MapImgList[i]) do begin
      R := GrossDrawRect;
      for j := 0 to Pred(MapList.Count) do
        with TIpHtmlNodeMap(MapList[j]) do begin
          for k := 0 to Pred(FChildren.Count) do
            if TIpHtmlNode(FChildren[k]) is TIpHtmlNodeArea then begin
              with TIpHtmlNodeArea(FChildren[k]) do begin
                if HRef <> '' then begin
                  case Shape of
                  hmsDefault :
                    Rect := R;
                  hmsRect :
                    begin
                      R2 := RectFromString(Coords);
                      OffsetRect(R2, R.Left, R.Top);
                      Rect := R2;
                    end;
                  hmsCircle :
                    Rgn := CircularRegion(Coords, R);
                  hmsPoly :
                    Rgn := PolygonRegion(Coords, R);
                  end;
                end;
              end;
              AreaList.Add(TIpHtmlNodeArea(FChildren[k]));
            end;
        end;
    end;
end;

procedure TIpHtml.MouseMove(Pt: TPoint);
var
  i : Integer;
begin
  FMouseLastPoint := Pt;
  FHotPoint := Point(-1, -1);
  if (MapList.Count > 0) and (AreaList.Count = 0) then
    BuildAreaList;
  for i := 0 to Pred(AnchorList.Count) do
    if TIpHtmlNodeA(AnchorList[i]).PtInRects(Pt) then begin
      if FHotNode <> TIpHtmlNodeA(AnchorList[i]) then begin
        if FHotNode <> nil then
          if FHotNode is TIpHtmlNodeA then
           TIpHtmlNodeA(FHotNode).Hot := False;
        FHotNode := TIpHtmlNode(AnchorList[i]);
        if FHotNode is TIpHtmlNodeA then
          TIpHtmlNodeA(FHotNode).Hot := True;
      end;
      if (FHotNode <> nil) then
        if FHotNode is TIpHtmlNodeA then
          FHotPoint := TIpHtmlNodeA(FHotNode).RelMapPoint(Pt);
      Exit;
    end;
  for i := 0 to Pred(AreaList.Count) do
    if TIpHtmlNodeAREA(AreaList[i]).PtInRects(Pt) then begin
      if FHotNode <> AreaList[i] then begin
        if FHotNode <> nil then
          if FHotNode is TIpHtmlNodeA then
            TIpHtmlNodeA(FHotNode).Hot := False;
        FHotNode := TIpHtmlNode(AreaList[i]);
      end;
      Exit;
    end;
  if FHotNode <> nil then
    if FHotNode is TIpHtmlNodeA then
      TIpHtmlNodeA(FHotNode).Hot := False;
  FHotNode := nil;
  FCurElement := nil;
  for i := 0 to Pred(RectList.Count) do
    if PtInRect(PIpHtmlRectListEntry(RectList[i]).Rect, Pt) then begin
      FCurElement := PIpHtmlRectListEntry(RectList[i]).Element;
      break;
    end;
end;

function TIpHtml.BuildPath(const Ext: string): string;
begin
  if FDataProvider <> nil then
    Result := FDataProvider.BuildURL(FCurURL,Ext)
  else
    Result :=  BuildURL(FCurURL, Ext);
end;

function TIpHtml.NewElement(EType : TElementType; Own: TIpHtmlNode) : PIpHtmlElement;
begin
  Result := ElementPool.NewItm;
  Result.ElementType := EType;
  Result.Owner := Own;
  Result.IsSelected := False;
end;

function TIpHtml.BuildStandardEntry(EType: TElementType): PIpHtmlElement;
begin
  Result := NewElement(EType, nil);
  Result.Props := nil;
  SetWordRect(Result, Rect(0, 0, 0, 0));
end;

function TIpHtml.BuildLineFeedEntry(EType: TElementType;
  AHeight: Integer): PIpHtmlElement;
begin
  if not (EType in [etHardLF, etSoftLF]) then
    raise Exception.Create('BuildLinefeedEntry can only be called with parameter etSoftLF or dtHardLF');
  Result := BuildStandardEntry(EType);
  Result.LFHeight := AHeight;
end;
procedure TIpHtml.MakeVisible(const R: TRect; ShowAtTop: Boolean = True);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, R, ShowAtTop);
end;

function TIpHtml.FindElement(const Name: string): TIpHtmlNode;
var
  i : Integer;
begin
  NameList.Sorted := True;
  i := NameList.IndexOf(Name);
  if i <> -1 then
    Result := TIpHtmlNode(NameList.Objects[i])
  else
    Result := nil;
end;

function TIpHtml.FindElementID(const Id: String): TIpHtmlNode;
var
  i: Integer;
begin
  IdList.Sorted := true;
  i := IdList.IndexOf(Id);
  if i <> -1 then
    Result := TIpHtmlNode(IdList.Objects[i])
  else
    Result := nil;
end;

type
  TIpHtmlGifQueueEntry = class
  protected
    FGraphic : TGraphic;
    FR : TRect;
  public
    constructor Create(AGraphic: TGraphic; ARect: TRect);
    property Graphic : TGraphic read FGraphic;
    property R : TRect read FR;
  end;

procedure TIpHtml.ClearAreaLists;
var
  i : Integer;
begin              
  for i := 0 to Pred(AnchorList.Count) do
    TIpHtmlNodeA(AnchorList[i]).ClearAreaList;
end;

procedure TIpHtml.Home;
begin
  MakeVisible(Rect(0, 0, 1, 1));
end;

procedure TIpHtml.Get(const URL: string);
begin
  if assigned(FOnGet) then
    FOnGet(Self, URL);
end;

procedure TIpHtml.Post(const URL: string; FormData: TIpFormDataEntity);
begin
  if assigned(FOnPost) then
    FOnPost(Self, URL, FormData);
end;

procedure TIpHtml.AddRect(const R: TRect; AElement: PIpHtmlElement;
  ABlock: TIpHtmlNodeBlock);
var
  NewEntry : PIpHtmlRectListEntry;
begin
  New(NewEntry);
  NewEntry.Rect := R;
  NewEntry.Element := AElement;
  NewEntry.Block := ABlock;
  RectList.Add(NewEntry);
end;

procedure TIpHtml.ClearRectList;
var
  i : Integer;
  p: PIpHtmlRectListEntry;
begin
  for i := Pred(RectList.Count) downto 0 do begin
    p:=PIpHtmlRectListEntry(RectList[i]);
    Freemem(p);
  end;
  RectList.Clear;
end;

procedure TIpHtml.DeselectAllItems(Item: Pointer);
begin
  PIpHtmlElement(item)^.IsSelected := False;
end;

procedure TIpHtml.SetSelection(StartPoint, EndPoint: TPoint);
var
  StartSelIndex,EndSelindex: Integer;
  i: Integer;
  r: TRect;
  Selected: boolean;
  DeselectAll: boolean;
  item: PIpHtmlRectListEntry;
begin
  if FAllSelected then
    InvalidateRect(Body.PageRect);
  FAllSelected := False;
  if EndPoint.y > StartPoint.y then begin
    FStartSel := StartPoint;
    FEndSel := EndPoint;
  end
  else
  if EndPoint.y = StartPoint.y then
    if EndPoint.x > StartPoint.x then begin
      FStartSel := StartPoint;
      FEndSel := EndPoint;
    end else begin
      FStartSel := EndPoint;
      FEndSel := StartPoint;
    end
  else begin
    FStartSel := EndPoint;
    FEndSel := StartPoint;
  end;
  if Body <> nil then begin
    // Invalidate only those blocks that need it
    DeselectAll := (EndPoint.x<0)and(EndPoint.y<0);
    GetSelectionBlocks(StartSelIndex,EndSelIndex);
    for i:= 0 to RectList.Count-1 do begin
      item := PIpHtmlRectListEntry(RectList[i]);
      // (de)select only text elements
      if Item.Element.ElementType<>etWord then
        Continue;
      if DeselectAll then
        Selected := false
      else
        Selected := (StartSelIndex<=i)and(i<=EndSelIndex);
      // Invalidate only changed elements
      if Item.Element.IsSelected<>Selected then begin
        Item.Element.IsSelected := Selected;
        if Body.PageRectToScreen(Item^.Rect, R) then
          InvalidateRect(R);
      end;
    end;
    // also deselect remaining elements
    if DeselectAll then
      ElementPool.EnumerateItems(DeselectAllItems);
  end;
end;

procedure TIpHtml.SelectAll;
begin
  FAllSelected := True;
end;

procedure TIpHtml.DeselectAll;
begin
  FAllSelected := False;
  FStartSel.x := -1;
  FEndSel.x := -1;
end;

procedure TIpHtml.CopyToClipboard;
var
  S : string;
  completed: Boolean;
begin
  if HaveSelection then begin
    S := '';
    if FHtml <> nil then begin
      completed := false;  // terminate recursion if selection-end-point is found
      FHtml.AppendSelection(S, completed);
    end;
    if S <> '' then begin
      Clipboard.Open;
      try
        Clipboard.Clear;
        Clipboard.AsText := S;
      finally
        Clipboard.Close;
      end;
    end;
  end;
end;

function TIpHtml.HaveSelection: Boolean;
begin
  Result := FAllSelected or ((FEndSel.x > 0) or (FEndSel.y > 0));
end;

procedure TIpHtml.CreateIFrame(Parent: TWinControl; Frame: TIpHtmlNodeIFRAME;
  var Control: TWinControl);
begin
  if assigned(FOnIFrameCreate) then
    FOnIFrameCreate(Self, Parent, Frame, Control);
end;

function TIpHtml.CheckKnownURL(URL: string): boolean;
var
  P : Integer;
begin
  if assigned(FOnURLCheck) then begin
    P := CharPos('#', URL);
    if P <> 0 then
      SetLength(URL, P - 1);
    Result:=true;
    FOnURLCheck(Self, URL, Result);
  end;
end;

procedure TIpHtml.ReportReference(URL: string);
var
  P : Integer;
begin
  if assigned(FOnReportURL) then begin
    P := CharPos('#', URL);
    if P <> 0 then
      if P = 1 then
        Exit
      else
        SetLength(URL, P - 1);
    FOnReportURL(Self, URL);
  end;
end;

procedure TIpHtml.ControlClick(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlClick) then
    FControlClick(Self, Sender);
end;

procedure TIpHtml.ControlClick2(Sender: TIpHtmlNodeControl; var cancel: boolean);
begin
  if assigned(FControlClick2) then
    FControlClick2(Self, Sender, cancel);
end;

procedure TIpHtml.ControlOnEditingDone(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlOnEditingDone) then
    FControlOnEditingDone(Self, Sender);
end;

procedure TIpHtml.ControlOnChange(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlOnChange) then
    FControlOnChange(Self, Sender);
end;

procedure TIpHtml.ControlCreate(Sender: TIpHtmlNodeControl);
begin
  if assigned(FControlCreate) then
    FControlCreate(Self, Sender);
end;

{ TIpHtmlGifQueueEntry }

constructor TIpHtmlGifQueueEntry.Create(AGraphic: TGraphic; ARect: TRect);
begin
  inherited Create;
  {$IFDEF IP_LAZARUS_DBG}
  DebugLn('TIpHtmlGifQueueEntry.Create ToDo NOT IMPLEMENTED YET');
  {$ELSE}
  FGraphic := AGraphic;
  {$ENDIF}
  FR := ARect;
end;

procedure TIpHtml.AddGifQueue(Graphic: TGraphic; const R: TRect);
begin
  GifQueue.Add(TIpHtmlGifQueueEntry.Create(Graphic, R));
end;

procedure TIpHtml.StartGifPaint(Target: TCanvas);
var
  i : Integer;
begin
  for i := 0 to Pred(GifQueue.Count) do
    with TIpHtmlGifQueueEntry(GifQueue[i]) do
      Target.StretchDraw(R, Graphic);
  ClearGifQueue;
end;

procedure TIpHtml.ClearGifQueue;
var
  i : Integer;
begin
  if Assigned(GifQueue) then
    for i := Pred(GifQueue.Count) downto 0 do begin
      TIpHtmlGifQueueEntry(GifQueue[i]).Free;
      GifQueue.Delete(i);
    end;
end;


{ TIpHtmlNodeBlock }

constructor TIpHtmlNodeBlock.Create(ParentNode: TIpHtmlNode;
  LayouterClass: TIpHtmlBaseLayouterClass);
begin
  inherited Create(ParentNode);
  FBgColor := clNone;
  FTextColor := clNone;
  FBackground := '';
  FLayouter := LayouterClass.Create(Self);
end;

constructor TIpHtmlNodeBlock.Create(ParentNode : TIpHtmlNode);
begin
  Create(ParentNode, BlockLayouterClass);  // The constructor above
end;

destructor TIpHtmlNodeBlock.Destroy;
begin
  FreeAndNil(FLayouter);
  inherited;
end;

procedure TIpHtmlNodeBlock.SetBackground(const AValue: string);
begin
  if AValue <> FBackground then begin
    FBackground := AValue;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBlock.SetBgColor(const AValue: TColor);
begin
  if AValue <> FBgColor then begin
    FBgColor := AValue;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBlock.SetTextColor(const AValue: TColor);
begin
  if AValue <> FTextColor then begin
    FTextColor := AValue;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeBlock.Render(RenderProps: TIpHtmlProps);
begin
  FLayouter.Render(RenderProps);
end;

procedure TIpHtmlNodeBlock.Layout(RenderProps: TIpHtmlProps; const TargetRect: TRect);
begin
  FLayouter.Layout(RenderProps, TargetRect);
end;

procedure TIpHtmlNodeBlock.CalcMinMaxPropWidth(RenderProps: TIpHtmlProps;
  var aMin, aMax: Integer);
begin
  FLayouter.CalcMinMaxPropWidth(RenderProps, aMin, aMax);
end;

procedure TIpHtmlNodeBlock.LoadAndApplyCSSProps;
begin
  inherited LoadAndApplyCSSProps;
  if FCombinedCSSProps <> nil then begin
    if FCombinedCSSProps.Color <> clNone then
      TextColor := FCombinedCSSProps.Color;
    if FCombinedCSSProps.BgColor <> clNone then
      BgColor := FCombinedCSSProps.BGColor;
  end;
end;

procedure TIpHtmlNodeBlock.EnqueueElement(const Entry: PIpHtmlElement);
begin
  FLayouter.FElementQueue.Add(Entry);
end;

procedure TIpHtmlNodeBlock.Invalidate;
var
  R : TRect;
begin
  if PageRectToScreen(PageRect, R) then
    Owner.InvalidateRect(R);
end;

function TIpHtmlNodeBlock.GetHeight(const RenderProps: TIpHtmlProps;
                                    const Width: Integer): Integer;
begin
  if FLastW = Width then begin
    Result := FLastH;
    Exit;
  end;
  Layout(RenderProps, Rect(0, 0, Width, MaxInt));
  Result := PageRect.Bottom;
  FLastH := Result;
  FLastW := Width;
end;

procedure TIpHtmlNodeBlock.InvalidateSize;
begin
  FLayouter.FBlockMin := -1;
  FLayouter.FBlockMax := -1;
  FLastW := 0;
  FLastH := 0;
  inherited;
end;

function TIpHtmlNodeBlock.Level0: Boolean;
var
  P : TIpHtmlNode;
begin
  Result := True;
  P := FParentNode;
  while P <> nil do begin
    if P is TIpHtmlNodeBlock then begin
      Result := False;
      break;
    end;
    P := P.FParentNode;
  end;
end;

procedure TIpHtmlNodeBlock.ReportCurDrawRects(aOwner: TIpHtmlNode; M : TRectMethod);
var
  i : Integer;
  CurElem : PIpHtmlElement;
begin
  for i := 0 to Pred(FLayouter.FElementQueue.Count) do begin
    CurElem := PIpHtmlElement(FLayouter.FElementQueue[i]);
    if CurElem.Owner = aOwner then
      M(CurElem.WordRect2);
  end;
end;

function TIpHtmlNodeBlock.GetPageRect: TRect;
begin
  Result := FLayouter.FPageRect;
end;

procedure TIpHtmlNodeBlock.AppendSelection(var S: string; var Completed: Boolean);

  // Avoid adding too many linefeeds - at most one blank line!
  procedure AddLF(var S: String);
  const
    DBL_LF = LineEnding + LineEnding;
  var
    endPart: String;
  begin
    if S <> '' then begin
      endpart := Copy(S, Length(S) - Length(DBL_LF) + 1, Length(DBL_LF));
      if endpart <> DBL_LF then
        S := S + LineEnding;
    end;
  end;

var
  LastY, StartSelIndex, EndSelIndex, i, istart, iend : Integer;
  LastNode: TIpHtmlNode;
  CurElem : PIpHtmlElement;
  R : TRect;
  LFDone : Boolean;
  EndPt: TPoint;
begin
  if Completed then
    exit;

  StartSelIndex := 0;
  EndSelIndex := pred(FLayouter.FElementQueue.Count);
  EndPt := Point(-1, -1);

  if not Owner.FAllSelected then
  begin
    // Find elements which contain the start-/end-selection-points
    // Note: they may not be in correct order because the y coords of the start/end
    // clicks may be reversed if in the same line of an etObject element!
    istart := -1;
    iend := -1;
    for i:=0 to pred(FLayouter.FElementQueue.Count) do
    begin
      CurElem := PIpHtmlElement(FLayouter.FElementQueue[i]);
      if PtInRect(CurElem^.WordRect2, Owner.FStartSel) then
        istart := i;
      if PtInRect(CurElem^.WordRect2, Owner.FEndSel) then
        iend := i;
      if (istart <> -1) and (iend <> -1) then
        break;
    end;

    // Start click could have been before first char of a line
    if (istart = -1) then
      for i:=0 to pred(FLayouter.FElementQueue.Count) do
      begin
        CurElem := PIpHtmlElement(FLayouter.FElementQueue[i]);
        R := CurElem^.WordRect2;
        if (Owner.FEndSel.Y >= R.Top) and (Owner.FEndSel.Y <= R.Bottom) and (Owner.FEndSel.X < R.Left) then
        begin
          istart := i;
          break;
        end;
      end;

    // End click could have been beyond line end
    if (iend = -1) then
      for i:=pred(FLayouter.FElementQueue.Count) downto 0 do
      begin
        CurElem := PIpHtmlElement(FLayouter.FElementQueue[i]);
        R := CurElem^.WordRect2;
        if (Owner.FEndSel.Y >= R.Top) and (Owner.FEndSel.Y <= R.Bottom) and (Owner.FEndSel.X > R.Right) then
        begin
          iend := i;
          EndPt := Point((R.Left + R.Right) div 2, (R.Top + R.Bottom) div 2);
          break;
        end;
      end;

    if (istart <> -1) and (iend <> -1) then
    begin
      if istart < iend then
      begin
        StartSelIndex := istart;
        EndSelIndex := iend;
        if (EndPt.X = -1) and (EndPt.Y = -1) then
          EndPt := Owner.FEndSel;
      end else
      begin
        StartSelIndex := iend;
        EndSelIndex := istart;
        if (EndPt.X = -1) and (EndPt.Y = -1) then
          EndPt := Owner.FStartSel;
      end;
    end else
    if (istart <> -1) and (iend = -1) then
      StartSelIndex := istart
    else
    if (istart = -1) and (iend <> -1) then
    begin
      EndSelIndex := iend;
      if (EndPt.X = -1) and (EndPt.Y = -1) then
        EndPt := Owner.FEndSel;
    end;
  end;

  LastNode := nil;
  LastY := -1;
  LFDone := True;
  for i := StartSelIndex to EndSelIndex do begin
    CurElem := PIpHtmlElement(FLayouter.FElementQueue[i]);
    R := CurElem.WordRect2;

    // Take care of inserting blank lines after headers etc., but don't insert
    // line breaks in long text elements.
    if not LFDone and (R.Top <> LastY) and (LastNode <> CurElem.Owner) then
      AddLF(S);

    case CurElem.ElementType of
    etWord :
      if CurElem.AnsiWord <> NAnchorChar then begin
        S := S + NoBreakToSpace(CurElem.AnsiWord);
        LFDone := False;
      end;
    etObject :
      begin
        TIpHtmlNodeAlignInline(CurElem.Owner).AppendSelection(S, Completed);
        LFDone := False;
      end;
    etSoftLF..etClearBoth :
      if not LFDone then begin
        AddLF(S);
        LFDone := True;
      end;
    end;
    LastY := R.Top;
    LastNode := CurElem.Owner;

    // Prevent running over selection end if there is an etObject element at
    // current level of recursion.
    if not Owner.FAllSelected then
      if PtInRect(R, EndPt) then begin
        Completed := true;
        exit;
      end;
  end;
end;

function TIpHtmlNodeBlock.ElementQueueIsEmpty: Boolean;
begin
  Result := FLayouter.FElementQueue.Count = 0;
end;

{ TIpHtmlNodeP }

constructor TIpHtmlNodeP.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  FElementName := 'p';
end;

destructor TIpHtmlNodeP.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeP.SetProps(const RenderProps: TIpHtmlProps);
var
  bgCol: TColor;
begin
  bgCol := Props.BgColor;
  Props.Assign(RenderProps);
  Props.Alignment := Align;
  if FParentNode = FOwner.Body then
    Props.BgColor := bgCol;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeP.Enqueue;
var
  elem: PIpHtmlElement;
  hf, h: Integer;
begin
  hf := Props.FontSize;
  if FChildren.Count > 0 then begin
    if not (FParentNode is TIpHtmlNodeLI) then
    begin
      if FParentNode is TIpHtmlNodeTD then h := 0 else h := hf; // div 2;
      // FIXME: above line is a workaround for LHelp to display the code tables
      // correctly
      h := GetMargin(Props.ElemMarginTop, h);
      elem := Owner.BuildLinefeedEntry(etSoftLF, h);
      EnqueueElement(elem);
    end;
  end;
  inherited Enqueue;
  if FChildren.Count > 0 then begin
    if not (FParentNode is TIpHtmlNodeLI) then
    begin
      if FParentNode is TIpHtmlNodeTD then h := 0 else h := hf; // div 2;
      // FIXME: above line is a workaround for LHelp to display the code tables
      // correctly
      h := GetMargin(Props.ElemMarginBottom, h);
      elem := Owner.BuildLinefeedEntry(etSoftLF, h);
      EnqueueElement(elem);
    end;
  end;
end;

function TIpHtmlNodeP.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeP.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
end;

procedure TIpHtmlNodeP.SetAlign(const Value: TIpHtmlAlign);
begin
  if Value <> FAlign then begin
    FAlign := Value;
    InvalidateSize;
  end;
end;


{ TIpHtmlNodeHeader }

constructor TIpHtmlNodeHeader.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
end;

destructor TIpHtmlNodeHeader.Destroy;
begin
  inherited;
end;

function TIpHtmlNodeHeader.GetAlign: TIpHtmlAlign;
begin
  Result := FAlign;
end;

procedure TIpHtmlNodeHeader.LoadAndApplyCSSProps;
begin
  inherited;
  if not (FCombinedCSSProps.Alignment in [haDefault, haUnknown]) then
    Align := FCombinedCSSProps.Alignment;
end;

procedure TIpHtmlNodeHeader.SetAlign(const Value: TIpHtmlAlign);
begin
  FAlign := Value;
end;

procedure TIpHtmlNodeHeader.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  Props.DelayCache:=True;
  Props.FontSize := FONTSIZESVALUESARRAY[abs(Size-6)];
  Props.FontStyle := [fsBold];
  Props.Alignment := Align;
  Props.DelayCache := False;
  inherited SetProps(Props);
end;

procedure TIpHtmlNodeHeader.Enqueue;
var
  elem: PIpHtmlElement;
  hf: Integer;
  h: Integer;
begin
  hf := Props.FontSize;
  // mimic layout/line spacing used in Chrome and Firefox
  if FChildren.Count > 0 then begin
    h := GetMargin(Props.ElemMarginTop, 3 * (Owner.DefaultFontSize div 2));
    elem := Owner.BuildLinefeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;
  inherited Enqueue;
  // mimic layout/line spacing used in Chrome and Firefox
  if FChildren.Count > 0 then begin
    h := GetMargin(Props.ElemMarginBottom, hf div 2);
    elem := Owner.BuildLinefeedEntry(etSoftLF, h);
    EnqueueElement(elem);
  end;
end;

type
  TFriendPanel = class(TCustomPanel) 
  end;

{ TIpHtmlNodeHtml }

procedure TIpHtmlNodeHtml.CalcMinMaxHtmlWidth(const RenderProps: TIpHtmlProps; var Min, Max: Integer);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).CalcMinMaxPropWidth(RenderProps, Min, Max);
end;

function TIpHtmlNodeHtml.GetHeight(const RenderProps: TIpHtmlProps; const Width: Integer): Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      Result := TIpHtmlNodeBody(FChildren[i]).GetHeight(RenderProps, Width);
end;

function TIpHtmlNodeHtml.HasBodyNode : Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to FChildren.Count - 1 do begin
    Result := (TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody);
    if Result then
      Break;
  end;
end;

procedure TIpHtmlNodeHtml.Layout(const RenderProps: TIpHtmlProps; const TargetRect: TRect);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).Layout(RenderProps, TargetRect);
end;

procedure TIpHtmlNodeHtml.Render(RenderProps: TIpHtmlProps);
var
  i : Integer;
begin
  for i := 0 to FChildren.Count - 1 do
    if TIpHtmlNode(FChildren[i]) is TIpHtmlNodeBody then
      TIpHtmlNodeBody(FChildren[i]).Render(RenderProps);
end;

{ TIpHtmlNodeCore }

procedure TIpHtmlNodeCore.AddArea(const R: TRect);
var
  RCopy : PRect;
  n : Integer;
begin
  n := FAreaList.Count;
  if n > 0 then begin
    RCopy := PRect(FAreaList[n-1]);
    if (R.Left = RCopy.Right) and (R.Top = RCopy.Top) and (R.Bottom = RCopy.Bottom)
    then begin
      RCopy.Right := R.Right;
      Exit;
    end;
  end;
  New(RCopy);
  RCopy^ := R;
  FAreaList.Add(RCopy);
end;

procedure TIpHtmlNodeCore.BuildAreaList;
var
  i : Integer;
begin
  for i := 0 to Pred(FChildren.Count) do
    TIpHtmlNode(FChildren[i]).ReportDrawRects(AddArea);
end;

procedure TIpHtmlNodeCore.ClearAreaList;
var
  a: Pointer;
begin
  while FAreaList.Count > 0 do begin
    a := FAreaList[0];
    FreeMem(a);
    FAreaList.Delete(0);
  end;
end;

procedure TIpHtmlNodeCore.ParseBaseProps(aOwner : TIpHtml);
var
  Commands: TStringList;
  s: String;
begin
  Id := aOwner.FindAttribute(htmlAttrID);
  ClassId := aOwner.FindAttribute(htmlAttrCLASS);
  Title := aOwner.FindAttribute(htmlAttrTITLE);
  Style := aOwner.FindAttribute(htmlAttrSTYLE);
  s := Uppercase(aOwner.FindAttribute(htmlAttrDIR));
  if (s = '') then 
  begin
    if (ParentNode is TIpHtmlNodeCore) then
      Dir := TIpHtmlNodeCore(ParentNode).Dir
    else 
    if (ParentNode is TIpHtmlNodeHtml) then
      Dir := TIpHtmlNodeHtml(ParentNode).Dir;
  end
  else
  if s = 'RTL' then 
    Dir := hdRTL 
  else 
  if s = 'LTR' then 
    Dir := hdLTR;

  if Style <> '' then
  begin
    if InlineCSS = nil then
      InlineCSS := TCSSProps.Create;
    Commands := SeparateCommands(Style);
    InlineCSS.ReadCommands(Commands);
    Commands.Free;
  end;
end;

(* look up the props for all CSS selectors that directly match this node, merge
   them all into one object (FCombinedCSSProps) and then apply them to Props.
   When FCombinedCSSProps already exists then the expensive lookup is skipped
   and the existing object is used. *)
procedure TIpHtmlNodeCore.LoadAndApplyCSSProps;
var
  TmpProps: TCSSProps;

begin
  if Owner.CSS = nil then
    exit;

  if FCombinedCSSProps = nil then
  begin
    FCombinedCSSProps := TCSSProps.Create;

    // first look for tag name only
    TmpProps := Owner.CSS.GetPropsObject(ElementName);
    if TmpProps <> nil then
      FCombinedCSSProps.MergeAdditionalProps(TmpProps);

    // look for .class if there is one
    if ClassID <> '' then
    begin
      TmpProps := Owner.CSS.GetPropsObject('', ClassId);
      if TmpProps <> nil then
        FCombinedCSSProps.MergeAdditionalProps(TmpProps);

      // then look for a tag.class selector if there is one
      TmpProps := Owner.CSS.GetPropsObject(ElementName, ClassId);
      if TmpProps <> nil then
        FCombinedCSSProps.MergeAdditionalProps(TmpProps);
    end;

    // lookup props for an id selector
    TmpProps := Owner.CSS.GetPropsObject(Id);
    if TmpProps <> nil then
      FCombinedCSSProps.MergeAdditionalProps(TmpProps);

    // inline css, not from the stylesheet
    if InlineCSS <> nil then
      FCombinedCSSProps.MergeAdditionalProps(InlineCSS);

  end;

  // look for :hover styles...
  if not FHoverPropsLookupDone then
  begin
    FHoverPropsRef := Owner.CSS.GetPropsObject(ElementName + ':hover');
    FHoverPropsLookupDone := True;
  end;
  // ...apply them if there are any.
  if FHoverPropsRef <> nil then
  begin
    Props.DelayCache:=True;
    if FHoverPropsRef.Color <> clNone then
      Props.HoverColor := FHoverPropsRef.Color;
    if FHoverPropsRef.BgColor <> clNone then
      Props.HoverBgColor := FHoverPropsRef.BgColor;
    Props.DelayCache:=False;
  end;

  Props.DelayCache:=True;
  ApplyCSSProps(FCombinedCSSProps, Props);
  Props.DelayCache:=False;
end;

procedure TIpHtmlNodeCore.MakeVisible;
var
  i : Integer;
  R : TRect;
begin
  if FAreaList.Count = 0 then
    BuildAreaList;
  SetRectEmpty(R);
  for i := 0 to Pred(FAreaList.Count) do
    UnionRect(R, R, PRect(FAreaList[i])^);

  Owner.MakeVisible(R, true);
  //Owner.MakeVisible(R, False);  // original
end;

function TIpHtmlNodeCore.SelectCSSFont(const aFont: string): string;
begin
  result := FindFontName(aFont);
end;

procedure TIpHtmlNodeCore.SetAlign(const Value: TIpHtmlAlign);
begin
  Props.Alignment := Value;
end;

procedure TIpHtmlNodeCore.SetId(const Value: String);
var
  idx: Integer;
begin
  if FId <> '' then
    with Owner.IdList do begin
      idx := IndexOf(Id);
      if idx > -1 then Delete(idx);
    end;
  FId:= Value;
  if FId <> '' then
    Owner.IdList.AddObject(FId, Self);
end;

procedure TIpHtmlNodeCore.ApplyCSSProps(const ACSSProps: TCSSProps;
  const props: TIpHtmlProps);

  function CssMarginToProps(CssMargin: TCSSMargin;
    out ElemMargin: TIpHtmlElemMargin): boolean;
  begin
    ElemMargin.Style:=hemsAuto;
    ElemMargin.Size:=0;
    if CssMargin.Style=cmsNone then exit(false);
    if CssMargin.Style=cmsAuto then exit(true);
    if CssMargin.Style=cmsPx then begin
      ElemMargin.Style:=hemsPx;
      ElemMargin.Size:=CssMargin.Size;
      exit(true);
    end;
    if CssMargin.Style=cmsEm then begin
      ElemMargin.Style:=hemsPx;
      ElemMargin.Size:=10*CssMargin.Size; // 1em = 1 current font size
      exit(true);
    end;
    debugln(['TIpHtmlNodeCore.ApplyCSSProps.CssMarginToProps note: margin style not supported ',ord(CssMargin.Style)]);
  end;

var
  ElemMargin: TIpHtmlElemMargin;
begin
  if (ACSSProps<>nil) and (props<>nil) then
  begin
    props.DelayCache:=True;
    {$WARNING Setting these font colors and name messes up the alignment for some reason}
    if ACSSProps.Color <> clNone then begin
      Props.FontColor := ACSSProps.Color;
    end;

    if ACSSProps.BGColor <> clNone then begin
      Props.BgColor := ACSSProps.BGColor;
    end;

    if ACSSProps.Alignment <> haUnknown then begin
      Props.Alignment := ACSSProps.Alignment;
    end;

    if ACSSProps.Font.Name <> '' then begin
      // put the code here, later refactore it
      Props.FontName := SelectCSSFont(ACSSProps.Font.Name);
    end;

     {$WARNING TODO Set Font size from CSS Value}
    // see http://xhtml.com/en/CSS/reference/font-size/
    if ACSSProps.Font.Size <> '' then begin
      // Props.FontSize :=  ACSSProps.Font.Size;
      props.FontSize:=GetFontSizeFromCSS(Props.FontSize, ACSSProps.Font.Size);
    end;

    if ACSSProps.Font.Style <> cfsNormal then begin
      case ACSSProps.Font.Style of
        cfsItalic,cfsOblique: Props.FontStyle := Props.FontStyle + [fsItalic];
        cfsInherit: ; // what to do?: search through parent nodes looking for a computed value
      end;
    end;

    if ACSSProps.Font.Weight <> cfwNormal then begin
      case ACSSProps.Font.Weight of
        cfwBold    : Props.FontStyle := Props.FontStyle + [fsBold];
        cfwBolder  : Props.FontStyle := Props.FontStyle + [fsBold];
        cfwLighter : Props.FontStyle := Props.FontStyle - [fsBold];
        cfw100     : ;
        cfw200     : ;
        cfw300     : ;
        cfw400     : ;
        cfw500     : ;
        cfw600     : ;
        cfw700     : ;
        cfw800     : ;
        cfw900     : ;
      end;
    end;

    if CssMarginToProps(ACSSProps.MarginTop,ElemMargin) then
      props.ElemMarginTop:=ElemMargin;
    if CssMarginToProps(ACSSProps.MarginRight,ElemMargin) then
      props.ElemMarginRight:=ElemMargin;
    if CssMarginToProps(ACSSProps.MarginBottom,ElemMargin) then
      props.ElemMarginBottom:=ElemMargin;
    if CssMarginToProps(ACSSProps.MarginLeft,ElemMargin) then
      props.ElemMarginLeft:=ElemMargin;

    props.DelayCache:=False;
  end;
end;

function TIpHtmlNodeCore.GetAlign: TIpHtmlAlign;
begin
  Result := Props.Alignment;
end;

function TIpHtmlNodeCore.GetFontSizeFromCSS(CurrentFontSize:Integer;
  aFontSize: string):Integer;

  function GetFSize(aUnits: string): double;
  var
    i: Integer;
  begin
    i := pos(aUnits, aFontSize);
    if i>0 then
      result := StrToFloatDef(copy(aFontSize,1,i-1), -1.0)
    else
      result := -1.0;
  end;
  
  function GetParentFontSize: integer;
  begin
    if (FParentNode is TIpHtmlNodeBlock) then
      result :=TIpHtmlNodeBlock(FParentNode).Props.FontSize
    else
    if (FParentNode is TIpHtmlNodeGenInline) then
      result := TIpHtmlNodeGenInline(FparentNode).Props.FontSize
    else
    if (FParentNode is TIpHtmlNodeHtml) or (FParentNode = nil) then
      Result := 14
    else
      result := CurrentFontSize;
  end;
  
var
  P: double;
  //ParentFSize: Integer;
begin
  result := CurrentFontSize;

  // check pt
  P:=GetFSize('pt');
  if P>0 then begin
    result := round(P);
    exit;
  end;
  
  // check px
  P:=GetFSize('px');
  if P>0 then begin
    // calculate points based on screen resolution :(
    // at 96dpi CSS21 recommneds 1px=0.26 mm
    // TODO: use screen resolution, check printing!
    Result := Round(P*0.7370241);
    exit;
  end;

  //todo: em, ex are supposed to be based on the computed pixel size of
  //      parent node, tpipro has no provision for this....

  // check %
  P:=GetFSize('%');
  if P>0 then begin
    result := round(GetParentFontSize * P/100);
    exit;
  end;
  
  // check em
  P:=GetFSize('em');
  if P>0 then begin
    result := round(GetParentFontSize * P);
  end;
end;

constructor TIpHtmlNodeCore.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  FAreaList := TFPList.Create;
end;

destructor TIpHtmlNodeCore.Destroy;
begin
  if Assigned(FInlineCSSProps) then
    FInlineCSSProps.Free;
  if Assigned(FCombinedCSSProps) then
    FCombinedCSSProps.Free;
  ClearAreaList;
  FAreaList.Free;
  inherited Destroy;
end;


{ TIpHtmlNodeSTYLE }

function TIpHtmlNodeSTYLE.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;

procedure TIpHtmlNodeSTYLE.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

{ TIpHtmlNodeIFRAME }

procedure TIpHtmlNodeIFRAME.CreateControl(Parent: TWinControl);
begin
  Owner.ControlCreate(Self);
  Owner.CreateIFrame(Parent, Self, FControl);
end;

procedure TIpHtmlNodeIFRAME.AddValues(NameList, ValueList: TStringList);
begin
end;

procedure TIpHtmlNodeIFRAME.Reset;
begin
end;

function TIpHtmlNodeIFRAME.Successful: Boolean;
begin
  Result := False;
end;

destructor TIpHtmlNodeIFRAME.Destroy;
begin
  inherited;
  FHeight.Free;
  FWidth.Free;
end;

procedure TIpHtmlNodeIFRAME.WidthChanged(Sender: TObject);
begin
  InvalidateSize;
end;

procedure TIpHtmlNodeIFRAME.SetAlign(const Value: TIpHtmlAlign);
begin
  if Value <> FAlign then begin
    FAlign := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetFrameBorder(const Value: Integer);
begin
  if Value <> FFrameBorder then begin
    FFrameBorder := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetMarginHeight(const Value: Integer);
begin
  if Value <> FMarginHeight then begin
    FMarginHeight := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetMarginWidth(const Value: Integer);
begin
  if Value <> FMarginWidth then begin
    FMarginWidth := Value;
    InvalidateSize;
  end;
end;

procedure TIpHtmlNodeIFRAME.SetScrolling(
  const Value: TIpHtmlFrameScrolling);
begin
  if Value <> FScrolling then begin
    FScrolling := Value;
    InvalidateSize;
  end;
end;


{ TIpHtmlNodeInline }

procedure TIpHtmlNodeInline.Invalidate;
begin
  FParentNode.Invalidate;
end;

procedure TIpHtmlNodeInline.EnqueueElement(const Entry: PIpHtmlElement);
begin
  FParentNode.EnqueueElement(Entry);
end;

function TIpHtmlNodeInline.ElementQueueIsEmpty: Boolean;
begin
  Result := FParentNode.ElementQueueIsEmpty;
end;

{ TIpHtmlNodeAlignInline }

constructor TIpHtmlNodeAlignInline.Create(ParentNode: TIpHtmlNode);
begin
  inherited;
  Element := Owner.NewElement(etObject, Self);
  Element.Props := Props;
end;

destructor TIpHtmlNodeAlignInline.Destroy;
begin
  inherited;
end;

procedure TIpHtmlNodeAlignInline.Enqueue;
begin
  EnqueueElement(Element);
end;

procedure TIpHtmlNodeAlignInline.SetAlignment(
  const Value: TIpHtmlImageAlign);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TIpHtmlNodeAlignInline.SetRect(TargetRect: TRect);
begin
end;

{ TIpHtmlNodeControl }

procedure TIpHtmlNodeControl.CalcMinMaxWidth(var Min, Max: Integer);
begin
  if FControl <> nil then
    Min := FControl.Width
  else
    Min := 0;
  Max := Min;
end;

constructor TIpHtmlNodeControl.Create(ParentNode: TIpHtmlNode);
begin
  inherited Create(ParentNode);
  Owner.FControlList.Add(Self);
  Align := hiaBottom;
end;

destructor TIpHtmlNodeControl.Destroy;
begin
  Owner.FControlList.Remove(Self);
  inherited;
end;

procedure TIpHtmlNodeControl.Draw;
var
  R : TRect;
  TopLeft : TPoint;
  Dim : TSize;
begin
  if FControl <> nil then begin
    TopLeft := Element.WordRect2.TopLeft;
    R.TopLeft := TopLeft;
    Dim := GetDim(0);
    R.Right := TopLeft.x + Dim.cx;
    R.Bottom := TopLeft.y + Dim.cy;
    if PageRectToScreen(R, R) then begin
      FControl.Left := R.Left;
      FCOntrol.Top := R.Top;
      FControl.Visible := True;
      Shown := not ScaleBitmaps{True}; {Keep controls hidden during printing}
    end else
      FControl.Visible := False;
  end;
end;

function TIpHtmlNodeControl.adjustFromCss: boolean;
begin
   result := false;
   LoadAndApplyCSSProps;
   if (props.FontSize <> -1) then
     FControl.Font.Size:= Props.FontSize;
     if Props.FontColor <> clNone then
       FControl.Font.Color:= Props.FontColor;
     if Props.BGColor <> clNone then
       FControl.Brush.Color:= Props.BGColor;
   result := True;
end;

procedure TIpHtmlNodeControl.SetDisabled(const AValue: Boolean);
begin
  if FDisabled = AValue then exit;
  FDisabled := AValue;
  FControl.Enabled := not FDisabled;
end;

procedure TIpHtmlNodeControl.SetProps(const RenderProps: TIpHtmlProps);
begin
  Props.Assign(RenderProps);
  LoadAndApplyCSSProps;
end;

function TIpHtmlNodeControl.GetDim(ParentWidth: Integer): TSize;
begin
  if FControl <> nil then
    Result := SizeRec(FControl.Width, FControl.Height)
  else
    Result := SizeRec(0, 0);
end;

procedure TIpHtmlNodeControl.HideUnmarkedControl;
begin
  if not Shown and (FControl <> nil) then
    FControl.Visible := False;
end;

procedure TIpHtmlNodeControl.UnmarkControl;
begin
  Shown := False;
end;

{ TIpHtmlNodeNv }

procedure TIpHtmlNodeNv.Invalidate;
begin
end;

procedure TIpHtmlNodeNv.InvalidateSize;
begin
end;

procedure TIpHtmlNodeNv.EnqueueElement(const Entry: PIpHtmlElement);
begin
end;

procedure TIpHtmlNodeNv.ReportDrawRects(M: TRectMethod);
begin
end;

procedure TIpHtmlNodeNv.SetProps(const RenderProps: TIpHtmlProps);
begin
end;

procedure TIpHtmlNodeNv.Enqueue;
begin
end;

function TIpHtmlNodeNv.ElementQueueIsEmpty: Boolean;
begin
  Result := True;
end;


{ TIpHtmlNodeFRAMESET }

destructor TIpHtmlNodeFRAMESET.Destroy;
begin
  inherited;
  FCols.Free;
  FRows.Free;
end;


{ TIpHtmlInternalPanel }

constructor TIpHtmlInternalPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse];
  DragMode := dmManual;
  HScroll := TIpHtmlScrollBar.Create(Self, sbHorizontal);
  HScroll.Tracking := True;
  VScroll := TIpHtmlScrollBar.Create(Self, sbVertical);
  VScroll.Tracking := True;
  HintWindow := THintWindow.Create(Self);
  HintWindow.Color := Application.HintColor;
end;

destructor TIpHtmlInternalPanel.Destroy;
begin
  HScroll.Free;
  VScroll.Free;
  HintWindow.Free;
  inherited;
end;

procedure TIpHtmlInternalPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or WS_HSCROLL or WS_VSCROLL;
end;

procedure TIpHtmlInternalPanel.DoHotChange;
begin
  if assigned(FOnHotChange) then
    FOnHotChange(Self);
end;

procedure TIpHtmlInternalPanel.DoCurElementChange;
begin
  if assigned(FOnCurElementChange) then
    FOnCurElementChange(Self);
end;

procedure TIpHtmlInternalPanel.DoHotInvoke;
begin
  if assigned(FOnHotClick) then
    FOnHotClick(Hyper);
end;

procedure TIpHtmlInternalPanel.DoClick;
begin
  if assigned(FOnClick) then
    FOnClick(Hyper);
end;

procedure TIpHtmlInternalPanel.ShowHintNow(const NewHint: string);
var
  Tw,Th : Integer;
  Sc : TPoint;
begin
  if HtmlPanel.ShowHints then begin
    if (NewHint<>'') then begin
      Tw := HintWindow.Canvas.TextWidth(NewHint);
      Th := HintWindow.Canvas.TextHeight(NewHint);
      Sc := ClientToScreen(Point(HintX,HintY));
      HintWindow.ActivateWithBounds(Rect(Sc.X + 6, Sc.Y + 16 - 6,
                                         Sc.X + Tw + 18, Sc.Y + Th + 16 + 6),
                                    NewHint);
      if Assigned(HtmlPanel.OnHotURL) then
        HtmlPanel.OnHotURL(HtmlPanel, NewHint);
    end else
      HideHint;
    CurHint := NewHint;
    HintShownHere := True;
  end;
end;

procedure TIpHtmlInternalPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldHot : TIpHtmlNode;
  OldCurElement : PIpHtmlElement;
  TmpOwnerNode: TIpHtmlNode;
begin
  if MouseIsDown and HaveSelection then begin
    SelEnd := Point(X + ViewLeft, Y + ViewTop);
    SetSelection;
    ScrollPtInView(Point(X + ViewLeft, Y + ViewTop));
  end;
  if Hyper <> nil then begin
    OldHot := Hyper.HotNode;
    OldCurElement := Hyper.CurElement;
    Hyper.MouseMove(Point(X + ViewLeft, Y + ViewTop));
    if (Hyper.HotNode <> OldHot) or (Hyper.HotPoint.x >= 0) then
      DoHotChange;
    if Hyper.HotNode <> nil then begin
      if Hyper.CurElement <> nil then begin
        Hyper.CurElement := nil;
        if OldCurElement <> Hyper.CurElement then
          DoCurElementChange;
      end;
    end else begin
      if HtmlPanel.AllowTextSelect then begin
        if Hyper.CurElement <> nil then begin
          if Hyper.CurElement.ElementType = etWord then
            Cursor := crIBeam
          else
            Cursor := crDefault;
        end else
          Cursor := crDefault;
      end;
      if OldCurElement <> Hyper.CurElement then
        DoCurElementChange;
    end;
  end;
  if (Hyper <> nil) and (Hyper.HotNode <> nil) then
    Hint := Hyper.HotNode.GetHint
  else
  if (Hyper <> nil) and (Hyper.CurElement <> nil)
  and (Hyper.CurElement.ElementType = etObject)
  and (Hyper.CurElement.Owner <> nil) then
    Hint := Hyper.CurElement.Owner.GetHint
  else
    Hint := '';
  inherited;

  // show hints for IpHtmlTagABBR and IpHtmlTagACRONYM
  if (Hyper <> nil) and (Hyper.CurElement <> nil) then begin

    TmpOwnerNode := Hyper.CurElement.Owner;
    while TmpOwnerNode <> nil do begin
      if TmpOwnerNode is TIpHtmlNodePhrase then begin
        if (TIpHtmlNodePhrase(TmpOwnerNode).Style = hpsABBR) or (TIpHtmlNodePhrase(TmpOwnerNode).Style = hpsACRONYM) then begin
          Hint := TIpHtmlNodePhrase(TmpOwnerNode).Title;
          Break;
        end else begin
          TmpOwnerNode := TmpOwnerNode.FParentNode;
        end;
      end else begin
        TmpOwnerNode := TmpOwnerNode.FParentNode;
      end;
    end;

  end;

  // "refresh" hint if it should have new value OR cursors position changes significantly (then we reposition the hint with the same text)
  if (Hint <> CurHint) or ((abs(HintX - X) > 4) or (abs(HintY - Y) > 4)) then begin
    HintShownHere := False;
    HintX := X;
    HintY := Y;
  end;
  if not HintShownHere then
    ShowHintNow(Hint);
end;

procedure TIpHtmlInternalPanel.HideHint;
begin
  HintWindow.Visible := False;
end;

procedure TIpHtmlInternalPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  MouseDownX := X;
  MouseDownY := Y;
  MouseIsDown := True;
  Self.SetFocus;
  if (Button=mbLeft) and HtmlPanel.AllowTextSelect then begin
    if Shift * [ssShift] = [] then begin
      ClearSelection;
      SelStart := Point(X + ViewLeft, Y + ViewTop);
      NewSelection := False;
      HaveSelection := True;
    end else
    if (Shift * [ssShift] = [ssShift]) and HaveSelection then begin
      SelEnd := Point(X + ViewLeft, Y + ViewTop);
      SetSelection;
      ScrollPtInView(SelEnd);
    end;
  end;
  inherited;
end;

procedure TIpHtmlInternalPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  MouseIsDown := False;
  if (abs(MouseDownX - X) < 4) and (abs(MouseDownY - Y) < 4) then
    if (Button = mbLeft) and (Shift = []) and (Hyper.HotNode <> nil) then
      // to avoid references to invalid objects do it asynchronously
      Application.QueueAsyncCall(AsyncHotInvoke, 0)
    else
      DoClick;
end;

procedure TIpHtmlInternalPanel.MouseLeave;
begin
  HideHint;
  inherited MouseLeave;
end;

procedure TIpHtmlInternalPanel.KeyDown(var Key: Word; Shift: TShiftState);
var
  TabList: TIpHtmlTabList;
begin
  if (key = VK_TAB) and TIpHtmlCustomPanel(Owner).WantTabs then
  begin
    TabList := FHyper.FTabList;

    if TabList.Index = -1 then
    begin
      // TODO find best place to start the index at...
      TabList.Index := 0;
    end;

    if (TabList.Count > 0) then
    begin
      if TIpHtmlNode(TabList[TabList.Index]) is TIpHtmlNodeA then
        TIpHtmlNodeA(TabList[TabList.Index]).DoOnBlur
      else if TObject(TabList[TabList.Index]).InheritsFrom(TIpHtmlNodeControl) then
        TIpHtmlNodeControl(TabList[TabList.Index]).FControl.Parent.SetFocus;

      if (ssShift in Shift) then
      begin
        if (TabList.Index > 0) then
        begin
          TabList.Index := TabList.Index -1;
          Key := 0;
        end
        else
          TabList.Index:=TabList.Count-1;
      end;

      if not(ssShift in Shift) then
      begin
        if TabList.Index < TabList.Count-1 then
        begin
          TabList.Index := TabList.Index + 1;
          Key := 0;
        end
        else
          TabList.Index := 0;
      end;

      if Key = 0 then
      begin
        if TIpHtmlNode(TabList[TabList.Index]) is TIpHtmlNodeA then
          TIpHtmlNodeA(TabList[TabList.Index]).DoOnFocus
        else if TObject(TabList[TabList.Index]).InheritsFrom(TIpHtmlNodeControl) then
          TIpHtmlNodeControl(TabList[TabList.Index]).FControl.SetFocus;
      end;
    end;
  end
  else if (key = VK_PRIOR) or ((key = VK_SPACE) and (ssShift in Shift)) then // page up
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaPgUp);
    Key := 0
  end
  else if (key = VK_NEXT) or ((key = VK_SPACE) and not(ssShift in Shift)) then // page down
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaPgDn);
    Key := 0
  end
  else if key = VK_UP then // up
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaUp, TIpHtmlCustomPanel(Owner).ScrollDist);
    Key := 0
  end
  else if key = VK_DOWN then // down
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaDown, TIpHtmlCustomPanel(Owner).ScrollDist);
    Key := 0
  end
  else if key = VK_LEFT then // left
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaLeft, TIpHtmlCustomPanel(Owner).ScrollDist);
    Key := 0
  end
  else if key = VK_RIGHT then // right
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaRight, TIpHtmlCustomPanel(Owner).ScrollDist);
    Key := 0
  end
  else if key = VK_HOME then // home
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaHome);
    Key := 0
  end
  else if key = VK_END then // end
  begin
    TIpHtmlCustomPanel(Owner).Scroll(hsaEnd);
    Key := 0
  end
  else if ((key = VK_C) or (key = VK_INSERT)) and (Shift = [ssCtrl]) then   // copy to clipboard
  begin
    HtmlPanel.CopyToClipboard;
//    FHyper.CopyToClipboard;
    Key := 0;
  end
  else if (key = VK_A) and (Shift = [ssCtrl]) then      // select all
  begin
    HtmlPanel.SelectAll;
//    FHyper.SelectAll;
//    Invalidate;
    Key := 0;
  end
  else if key = VK_RETURN then // return
  begin
    if (FHyper.FTabList.TabItem <> nil) and (FHyper.FTabList.TabItem is TIpHtmlNodeA) then
    begin
      TIpHtmlNodeA(FHyper.FTabList.TabItem).Hot:=True;
      FHyper.FHotNode := TIpHtmlNodeA(FHyper.FTabList.TabItem);

      DoHotChange;
      Application.QueueAsyncCall(AsyncHotInvoke, 0);
      Key := 0
    end;
  end
  else if ((key = VK_C) or (key = VK_INSERT)) and (ssCtrl in Shift) then
    FHyper.CopyToClipboard
  else
    inherited KeyDown(Key, Shift);
end;

function TIpHtmlInternalPanel.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
var
  i: Integer;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  for i := 0 to Mouse.WheelScrollLines-1 do
    if WheelDelta < 0 then
      Perform(LM_VSCROLL, MAKELONG(SB_LINEDOWN, 0), 0)
    else
      Perform(LM_VSCROLL, MAKELONG(SB_LINEUP, 0), 0);
end;

procedure TIpHtmlInternalPanel.Paint;
var
  CR: TRect;
begin
  if FPaintingLock > 0 then
    exit;
  inc(FPaintingLock);

  try
    if Assigned(HTMLPanel.OnPaint) then HTMLPanel.OnPaint(HTMLPanel);

    CR := GetClientRect;
    if not ScaleBitmaps {printing} and (Hyper <> nil) then
    begin
      // update layout
      GetPageRect;
      // render
      Hyper.Render(Canvas,
        Rect(
          ViewLeft, ViewTop,
          ViewLeft + (CR.Right - CR.Left),
          ViewTop + (CR.Bottom - CR.Top)
        ),
        ViewTop,
        ViewTop + (CR.Bottom - CR.Top),
        HTMLPanel.UsePaintBuffer,
        Point(0, 0)
      );
      FHyper.NeedResize := False;
    end
    else
      Canvas.FillRect(CR);
    //debugln(['TIpHtmlInternalPanel.Paint ',dbgs(CR)]);
    {$IFDEF IP_LAZARUS_DBG}
    DebugBox(Canvas, CR, clYellow);
    Debugbox(Canvas, Canvas.ClipRect, clLime, true);
    {$ENDIF}
  finally
    dec(FPaintingLock);
  end;
end;

{$IFDEF Html_Print}
function TIpHtmlInternalPanel.PreviewAntiAliasingMode: TAntiAliasingMode;
begin
  Result := HTMLPanel.PrintSettings.Preview.AntiAliasingMode;
end;

procedure TIpHtmlInternalPanel.BeginPrint;
begin
  if InPrint = 0 then begin
    Printed := False;
    ScaleBitmaps := True;
    ResetPrint;
  end;
  Inc(InPrint);
end;

procedure TIpHtmlInternalPanel.EndPrint;
begin
  Dec(InPrint);
  if InPrint = 0 then begin
    ScaleBitmaps := False;
    InvalidateSize;
  end;
end;

procedure TIpHtmlInternalPanel.ResetPrint;
var
  LogPixX, LMarginPix, RMarginPix,
  LogPixY, TMarginPix, BMarginPix,
  H: Integer;
  oldPrinterFileName: String;
begin
  // check ir BeginPrint was called
  if not Printed then begin
    SetRectEmpty(PrintPageRect);
    if Hyper.TitleNode <> nil then
      Printer.Title := Hyper.TitleNode.Title
    else
      Printer.Title := 'HTML Document';

    // Avoid showing the print file selection dialog appearing in case of some
    // PDF printers.
    oldPrinterFileName := Printer.FileName;
    Printer.FileName := 'test';

    Printer.BeginDoc;
    GetRelativeAspect(Printer.Canvas.Handle);
    {$IF NOT DEFINED(WINDOWS)}
    // this test looks weird, according to most references consulted, the number
    // of colors in a display is NColors = 1 shl (bitsPerPixel * Planes). A mono
    // printer should have 2 colors, somebody else needs to clarify.
    BWPrinter := false;
    {$ELSE}
    BWPrinter := GetDeviceCaps(Printer.Canvas.Handle, NUMCOLORS) = 2;
    {$ENDIF}
    LogPixX := Printer.XDPI;
    LMarginPix := round(HtmlPanel.PrintSettings.MarginLeft * LogPixX);
    RMarginPix := round(HtmlPanel.PrintSettings.MarginRight * LogPixX);
    PrintWidth := Printer.PageWidth - LMarginPix - RMarginPix;
    LogPixY := Printer.YDPI;
    TMarginPix := round(HtmlPanel.PrintSettings.MarginTop * LogPixY);
    BMarginPix := round(HtmlPanel.PrintSettings.MarginBottom * LogPixY);
    if Printer.Printers.Count = 0 then begin
      PrintHeight := 500;
    end else begin
      PrintHeight := Printer.PageHeight - TMarginPix - BMarginPix;
    end;
    PrintTopLeft := Point(LMarginPix, TMarginPix);
    {PrintBottomRight := Point(
      Printer.PageWidth - RMarginPix,
      Printer.PageHeight - BMarginPix);}
    PrintPageRect := Hyper.GetPageRect(Printer.Canvas, PrintWidth, PrintHeight);
    H := PrintPageRect.Bottom - PrintPageRect.Top;
    PageCount := H div PrintHeight;
    if H mod PrintHeight <> 0 then
      Inc(PageCount);
    Printer.Abort;
    Printer.FileName := oldPrinterFileName;
  end else
    raise Exception.Create('BeginPrint must be called before ResetPrint.');
end;

function TIpHtmlInternalPanel.SelectPrinterDlg: boolean;
var
  printDialog: TPrintDialog;
begin
  Result := False;
  printDialog := TPrintDialog.Create(nil);
  if printDialog.Execute then begin
    ResetPrint;
    Result := true;
  end;
end;

procedure TIpHtmlInternalPanel.PrintPages(FromPage, ToPage: Integer);
var
  CR : TRect;
  i : Integer;
  oldRD: TIpHtmlRenderDevice;
begin
  if (Hyper <> nil) then begin
    oldRD := Hyper.RenderDevice;
    Printer.Refresh;
    BeginPrint;
    Printer.BeginDoc;
    try
      CR := Rect(0, 0, PrintWidth, 0);
      for i := FromPage to ToPage do begin
        CR.Top := (i - 1) * PrintHeight;
        CR.Bottom := Cr.Top + PrintHeight;
        Hyper.FRenderDev := rdPrinter;
        Hyper.Render(Printer.Canvas, CR, False, PrintTopLeft);
        if i < ToPage then
          Printer.NewPage;
        Printed := True;
      end;
    finally
      if Printed then
        Printer.EndDoc
      else
        Printer.Abort;
      EndPrint;
      Hyper.FRenderDev := oldRD;
    end;
  end;
end;

procedure TIpHtmlInternalPanel.PrintPreview;
var
  preview: TIpHtmlPreview;
  p: TPosition;
  oldRD: TIpHtmlRenderDevice;
begin
  if (Hyper <> nil) then begin
    oldRD := Hyper.RenderDevice;
    BeginPrint;
    try
      preview := TIpHTMLPreview.Create(Application);
      with preview do
        try
          p := HTMLPanel.PrintSettings.Preview.Position;
          if not (p in [poDefault, poDefaultSizeOnly]) then begin
            Width := HTMLPanel.PrintSettings.Preview.Width;
            Height := HTMLPanel.PrintSettings.Preview.Height;
          end;
          if (p = poDesigned) or (p = poDefaultSizeOnly) then begin
            Left := HTMLPanel.PrintSettings.Preview.Left;
            Top := HTMLPanel.PrintSettings.Preview.Top;
          end;
          Position := p;
          if HTMLPanel.PrintSettings.Preview.Maximized then
            WindowState := wsMaximized else
            WindowState := wsNormal;
          lblMaxPage.Caption := IntToStr(PageCount);
          FCurPage := 1;
          HTML := Hyper;
          ScaleFonts := True;
          try
            OwnerPanel := Self;
            Zoom := HTMLPanel.PrintSettings.Preview.Zoom;
            Hyper.FRenderDev := rdPreview;
            ShowModal;
            HTMLPanel.PrintSettings.Preview.Maximized := (WindowState = wsMaximized);
            if (WindowState = wsNormal) then begin
              if (p = poDesigned) or (p = poDefaultSizeOnly) then begin
                HTMLPanel.PrintSettings.Preview.Left := Left;
                HTMLPanel.PrintSettings.Preview.Top := Top;
              end;
              if not (p in [poDefault, poDefaultSizeOnly]) then begin
                HTMLPanel.PrintSettings.Preview.Width := Width;
                HTMLPanel.PrintSettings.Preview.Height := Height;
              end;
            end;
          finally
            ScaleFonts := False;
          end;
        finally
          Free;
        end;

    finally
      EndPrint;
      Hyper.FRenderDev := oldRD;
    end;
  end;
end;
{$ENDIF}

procedure TIpHtmlInternalPanel.EraseBackground(DC: HDC);
begin
  //
end;

{$IFDEF Html_Print}
function TIpHtmlInternalPanel.GetPrintPageCount: Integer;
begin
  BeginPrint;
  try
    Result := PageCount;
  finally
    EndPrint;
  end;
end;
{$ENDIF}

procedure TIpHtmlInternalPanel.InvalidateSize;
begin
  FPageRectValid:=false;
  if FPaintingLock = 0 then
    Invalidate;
end;

procedure TIpHtmlInternalPanel.DoOnResize;
begin
  inherited;
  InvalidateSize;
  if Assigned(FHyper) then
    FHyper.NeedResize := True;
end;

function TIpHtmlInternalPanel.PagePtToScreen(const Pt : TPoint): TPoint;
{-convert coordinates of point passed in to screen coordinates}
begin
  Result := Pt;
  Dec(Result.x, ViewLeft);
  Dec(Result.y, ViewTop);
end;

procedure TIpHtmlInternalPanel.ScrollInViewRaw(R : TRect);
begin
  R.TopLeft := PagePtToScreen(R.TopLeft);
  R.BottomRight := PagePtToScreen(R.BottomRight);
  if R.Left < 0 then
    with HScroll do
     Position := Position + R.Left
  else if R.Right > ClientWidth then begin
    if R.Right - R.Left > ClientWidth then
      R.Right := R.Left + ClientWidth;
    with HScroll do
      Position := Position + R.Right - ClientWidth;
  end;
  if R.Top < 0 then
    with VScroll do
      Position := Position + R.Top
  else if R.Bottom > ClientHeight then begin
    if R.Bottom - R.Top > ClientHeight then
      R.Bottom := R.Top + ClientHeight;
    with VScroll do
      Position := Position + R.Bottom - ClientHeight;
  end;
end;

procedure TIpHtmlInternalPanel.ScrollInView(R : TRect);
begin
  R.Bottom := R.Top + (ClientHeight - (R.Bottom - R.Top) - 10);
  R.Right := R.Left + (ClientWidth - (R.Right - R.Left) - 10);
  ScrollInViewRaw(R);
end;

procedure TIpHtmlInternalPanel.ScrollPtInView(P : TPoint);
begin
  P := PagePtToScreen(P);
  if P.x < 0 then
    with HScroll do
     Position := Position + P.x
  else if P.x > ClientWidth then begin
    with HScroll do
      Position := Position + P.x - ClientWidth;
  end;
  if P.y < 0 then
    with VScroll do
      Position := Position + P.y
  else if P.y > ClientHeight then begin
    with VScroll do
      Position := Position + P.y - ClientHeight;
  end;
end;

procedure TIpHtmlInternalPanel.ScrollRequest(Sender: TIpHtml; const R: TRect; ShowAtTop: Boolean = True);
begin
  if not ShowAtTop then
    ScrollInViewRaw(R)
  else
  ScrollInView(R);
end;

procedure TIpHtmlInternalPanel.SetHtml(const Value: TIpHtml);
begin
  FHyper := Value;
  InvalidateSize;
end;

function TIpHtmlInternalPanel.GetPageRect: TRect;
begin
  if not FPageRectValid then begin
    if Hyper <> nil then
      PageRect := Hyper.GetPageRect(Canvas, ClientWidth, 0)
    else
      PageRect:=Rect(0,0,0,0);
    FPageRectValid:=true;
  end;
  Result:=FPageRect;
end;

procedure TIpHtmlInternalPanel.SetPageRect(const Value: TRect);
begin
  if not SettingPageRect then begin
    SettingPageRect := True;
    FPageRect := Value;
    HScroll.CalcAutoRange;
    VScroll.CalcAutoRange;
    SettingPageRect := False;
  end;
end;

procedure TIpHtmlInternalPanel.UpdateScrollBars;
begin
  if not FUpdatingScrollBars and HandleAllocated then
    try
      FUpdatingScrollBars := True;
      if VScroll.NeedsScrollBarVisible then
      begin
        HScroll.Update(False, True);
        VScroll.Update(True, False);
      end
      else if HScroll.NeedsScrollBarVisible then
      begin
        VScroll.Update(False, True);
        HScroll.Update(True, False);
      end
      else
      begin
        VScroll.Update(False, False);
        HScroll.Update(True, False);
      end;
      GetPageRect();
    finally
      FUpdatingScrollBars := False;
    end;
end;

procedure TIpHtmlInternalPanel.WMHScroll(var Message: TLMHScroll);
begin
  if HScroll.Visible then
    HScroll.ScrollMessage(Message);
end;

procedure TIpHtmlInternalPanel.WMVScroll(var Message: TLMVScroll);
begin
  if VScroll.Visible then
    VScroll.ScrollMessage(Message);
end;

procedure TIpHtmlInternalPanel.AsyncHotInvoke(data: ptrint);
begin
  DoHotInvoke;
end;

procedure TIpHtmlInternalPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TIpHtmlInternalPanel.ClearSelection;
begin
  Hyper.SetSelection(Point(-1, -1), Point(-1, -1));
  HaveSelection := False;
end;

procedure TIpHtmlInternalPanel.SetSelection;
begin
  if Hyper <> nil then
    Hyper.SetSelection(SelStart, SelEnd);
end;

function TIpHtmlInternalPanel.HtmlPanel: TIpHtmlCustomPanel;
begin
  Result := TIpHtmlPanel(Parent);
  while not (Result is TIpHtmlPanel) do
    Result := TIpHtmlPanel(Result.Parent);
end;

{ TIpHtmlScrollBar }

constructor TIpHtmlScrollBar.Create(AControl: TIpHtmlInternalPanel;
  AKind: TScrollBarKind);
begin
  inherited Create;
  FControl := AControl;
  FKind := AKind;
  FPageIncrement := 80;
  FIncrement := FPageIncrement div 10;
  FVisible := True;
  FUpdateNeeded := True;
end;

procedure TIpHtmlScrollBar.CalcAutoRange;
begin
  if Kind = sbHorizontal then
    DoSetRange(FControl.PageRect.Right)
  else
    DoSetRange(FControl.PageRect.Bottom);
end;

function TIpHtmlScrollBar.ControlSize(ControlSB, AssumeSB: Boolean): Integer;
var
  BorderAdjust: Integer;

  function ScrollBarVisible(Code: Word): Boolean;
  var
    Style: Longint;
  begin
    Style := WS_HSCROLL;
    if Code = SB_VERT then Style := WS_VSCROLL;
    Result := GetWindowLong(FControl.Handle, GWL_STYLE) and Style <> 0;
  end;

  function Adjustment(Code, Metric: Word): Integer;
  begin
    Result := 0;
    if not ControlSB then
      if AssumeSB and not ScrollBarVisible(Code) then
        Result := -(GetSystemMetrics(Metric) - BorderAdjust)
      else if not AssumeSB and ScrollBarVisible(Code) then
        Result := GetSystemMetrics(Metric) - BorderAdjust;
  end;

begin
  BorderAdjust := Integer(GetWindowLong(FControl.Handle, GWL_STYLE) and
    (WS_BORDER or WS_THICKFRAME) <> 0);
  if Kind = sbVertical then
    Result := FControl.ClientHeight + Adjustment(SB_HORZ, SM_CXHSCROLL) else
    Result := FControl.ClientWidth + Adjustment(SB_VERT, SM_CYVSCROLL);
end;

function TIpHtmlScrollBar.NeedsScrollBarVisible: Boolean;
begin
  Result := FRange > ControlSize(False, False);
end;

procedure TIpHtmlScrollBar.ScrollMessage(var Msg: TLMScroll);

  function GetRealScrollPosition: Integer;
  var
    SI: TScrollInfo;
    Code: Integer;
  begin
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_TRACKPOS;
    Code := SB_HORZ;
    if FKind = sbVertical then
      Code := SB_VERT;
    Result := Msg.Pos;
    if FlatSB_GetScrollInfo(FControl.Handle, Code, SI) then
      Result := SI.nTrackPos;
  end;

begin
  with Msg do
    case ScrollCode of
    SB_LINEUP:
      SetPosition(FPosition - FIncrement);
    SB_LINEDOWN:
      SetPosition(FPosition + FIncrement);
    SB_PAGEUP:
      SetPosition(FPosition - ControlSize(True, False));
    SB_PAGEDOWN:
      SetPosition(FPosition + ControlSize(True, False));
    SB_THUMBPOSITION:
      if FCalcRange > 32767 then
        SetPosition(GetRealScrollPosition)
      else
        SetPosition(Pos);
    SB_THUMBTRACK:
      if Tracking then
        if FCalcRange > 32767 then
          SetPosition(GetRealScrollPosition)
        else
          SetPosition(Pos);
    SB_TOP:
      SetPosition(0);
    SB_BOTTOM:
      SetPosition(FCalcRange);
    SB_ENDSCROLL:
      ;
    end;
end;

procedure TIpHtmlScrollBar.SetPosition(Value: Integer);
var
  Code: Word;
begin
  if csReading in FControl.ComponentState then
    FPosition := Value
  else begin
    if Value > FCalcRange then Value := FCalcRange
    else if Value < 0 then Value := 0;
    if Kind = sbHorizontal then
      Code := SB_HORZ else
      Code := SB_VERT;
    if Value <> FPosition then
    begin
      FPosition := Value;
      if Kind = sbHorizontal then
        FControl.ViewLeft := Value
      else
        FControl.ViewTop := Value;
      FControl.Invalidate;
    end;
    if FlatSB_GetScrollPos(FControl.Handle, Code) <> FPosition then
      FlatSB_SetScrollPos(FControl.Handle, Code, FPosition, True);
  end;
end;

procedure TIpHtmlScrollBar.DoSetRange(Value: Integer);
begin
  FRange := Value;
  if FRange < 0 then FRange := 0;
  FControl.UpdateScrollBars;
end;

procedure TIpHtmlScrollBar.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FControl.UpdateScrollBars;
end;

procedure TIpHtmlScrollBar.Update(ControlSB, AssumeSB: Boolean);
type
  TPropKind = (pkStyle, pkButtonSize, pkThumbSize, pkSize, pkBkColor);
const
  Props: array[TScrollBarKind, TPropKind] of Integer = (
    (WSB_PROP_HSTYLE, WSB_PROP_CXHSCROLL, WSB_PROP_CXHTHUMB, WSB_PROP_CYHSCROLL,
     WSB_PROP_HBKGCOLOR),
    (WSB_PROP_VSTYLE, WSB_PROP_CYVSCROLL, WSB_PROP_CYVTHUMB, WSB_PROP_CXVSCROLL,
     WSB_PROP_VBKGCOLOR));
var
  Code: Word;
  ScrollInfo: TScrollInfo;
  iPi: integer;

  procedure UpdateScrollProperties(Redraw: Boolean);
  begin
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkStyle], FSB_REGULAR_MODE, Redraw);
    FlatSB_SetScrollProp(FControl.Handle, Props[Kind, pkBkColor],
      integer(ColorToRGB(clBtnHighlight)), False);
  end;

begin
  FCalcRange := 0;
  Code := SB_HORZ;
  if Kind = sbVertical then
    Code := SB_VERT;
  if Visible then begin
    FCalcRange := Range - ControlSize(ControlSB, AssumeSB);
    if FCalcRange < 0 then
      FCalcRange := 0;
  end;
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  ScrollInfo.nMin := 0;
  if FCalcRange > 0 then
    ScrollInfo.nMax := Range
  else
    ScrollInfo.nMax := 0;
  iPi := ControlSize(ControlSB, AssumeSB) + 1;
  if iPi < 1 then iPi := 1;
  ScrollInfo.nPage := iPi;
  ScrollInfo.nPos := FPosition;
  ScrollInfo.nTrackPos := FPosition;
  UpdateScrollProperties(FUpdateNeeded);
  FUpdateNeeded := False;
  FlatSB_SetScrollInfo(FControl.Handle, Code, ScrollInfo, True);
  SetPosition(FPosition);
  iPi := (ControlSize(True, False) * 9) div 10;
  if iPi < low(TScrollbarInc) then iPi := low(TScrollbarInc)
  else if iPi > high(TScrollbarInc) then iPi := high(TScrollbarInc);
  FPageIncrement := iPi;
end;

{ TIpHtmlFrame }

procedure TIpHtmlFrame.InitHtml;
begin
  FHtml.FixedTypeface := Viewer.FixedTypeface;
  FHtml.DefaultTypeFace := Viewer.DefaultTypeFace;
  FHtml.DefaultFontSize := Viewer.DefaultFontSize;
  FHtml.TextColor := FViewer.TextColor;
  FHtml.LinkColor := FViewer.LinkColor;
  FHtml.ALinkColor := FViewer.ALinkColor;
  FHtml.VLinkColor := FViewer.VLinkColor;
  FHtml.BgColor := FViewer.BgColor;
  FHtml.LinksUnderlined := FViewer.LinksUnderlined;
  if FViewer.DataProvider <> nil then
    FHtml.OnGetImageX := FViewer.DataProvider.DoGetImage;
  FHtml.OnInvalidateRect := InvalidateRect;
  FHtml.OnInvalidateSize := InvalidateSize;
  FHtml.OnGet := Get;
  FHtml.OnPost := Post;
  FHtml.OnIFrameCreate := IFrameCreate;
  FHtml.OnURLCheck := FViewer.URLCheck;
  FHtml.OnReportURL := FViewer.ReportURL;
  FHtml.FlagErrors := FFlagErrors;
  FHtml.MarginWidth := FMarginWidth;
  FHtml.MarginHeight := FMarginHeight;
  if FDataProvider <> nil then
    FHtml.FDataProvider := FDataProvider;
  FHtml.FactBAParag := FViewer.FactBAParag;
end;

constructor TIpHtmlFrame.Create(Viewer: TIpHtmlCustomPanel; Parent: TCustomPanel;
  DataProvider : TIpAbstractHtmlDataProvider; FlagErrors, NoScroll: Boolean;
      MarginWidth, MarginHeight: Integer);
begin
  inherited Create;
  FNoScroll := NoScroll;
  FParent := Parent;
  FViewer := Viewer;
  FDataProvider := DataProvider;
  FHtml := TIpHtml.Create;
  FFlagErrors := FlagErrors;
  FMarginWidth := MarginWidth;
  FMarginheight := MarginHeight;
  InitHtml;
end;

destructor TIpHtmlFrame.Destroy;
var
  i : Integer;
begin
  if FFramePanel <> nil then
    FFramePanel.OnResize := nil;
  for i := 0 to Pred(FFrameCount) do
    FreeAndNil(FFrames[i]);
  if HyperPanel <> nil then begin
    HyperPanel.Hyper := nil;
    HyperPanel.Free;
    HyperPanel := nil;
  end;
  //debugln(['TIpHtmlFrame.Destroy ',DbgSName(Self),' ',dbgs(Pointer(FDataProvider))]);
  if (FDataProvider <> nil) and (not (csDestroying in FDataProvider.ComponentState)) then
    FDataProvider.DoLeave(FHtml);
  FreeAndNil(FHtml);
  inherited;
end;

procedure TIpHtmlFrame.InvalidateRect(Sender: TIpHtml; const R: TRect);
begin
  if HyperPanel <> nil then
    LCLIntf.InvalidateRect(HyperPanel.Handle, @R, False);
end;

procedure TIpHtmlFrame.InvalidateSize(Sender: TObject);
begin
  if HyperPanel <> nil then
    if not InOpen then
      HyperPanel.InvalidateSize;
end;

procedure TIpHtmlFrame.OpenURL(const URL: string; Delayed: Boolean);
begin
  if Delayed then begin
    FViewer.GetURL := URL;
    FViewer.PostURL := '';
    FViewer.PostData := nil;
    PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
  end else
    OpenRelativeURL(URL);
end;

procedure TIpHtmlFrame.AlignPanels;
var
  ColW : TIntArr;
  RowH : TIntArr;
  ColWCount, RowHCount : Integer;
  N, i, R, C, L, T : Integer;
begin
  if (FHtml = nil) or (FHtml.FrameSet = nil) then Exit;
  if FFramePanel = nil then Exit;
  ColW := CalcMultiLength(FHtml.FrameSet.Cols, FFramePanel.ClientWidth,
    ColWCount);
  try
    RowH := CalcMultiLength(FHtml.FrameSet.Rows, FFramePanel.ClientHeight,
      RowHCount);
    try
      R := 0;
      C := 0;
      L := 0;
      T := 0;
      N := 0;
      for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
        if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
          if Pnl[N] <> nil then
            Pnl[N].SetBounds(L, T, ColW[C], RowH[R]);
          Inc(L, ColW[C]);
          if C < ColWCount - 1 then
            Inc(C)
          else begin
            C := 0;
            L := 0;
            Inc(T, RowH[R]);
            Inc(R);
          end;
          Inc(N);
        end;
      end;
    finally
      RowH.Free;
    end;
  finally
    ColW.Free;
  end;
end;

function TIpHtmlFrame.IsExternal(const URL: string): Boolean;
var
  St, ResourceType : string;
begin
  if Assigned(FDataProvider) then
    St := FDataProvider.BuildURL(FCurURL, URL)
  else
    St := IpUtils.BuildURL(FCurURL, URL);
  if FDataProvider = nil then
    raise EIpHtmlException.Create(SHtmlNoDataProvider);
  if not FDataProvider.DoCheckURL(St, ResourceType) then
    raise EIpHtmlException.Create(SHtmlResUnavail + St);

  if (PosI('text/', ResourceType) <> 1) and (PosI('image/', ResourceType) <> 1) then begin
    FViewer.FHotURL := St;
    FViewer.DoHotClick;
    Result := True;
  end else
    Result := False;
end;

function BuildImagePage(const URL: string): TMemoryStream;
var
  S : string;
begin
  Result := TMemoryStream.Create;
  S := '<Html><BODY><IMG src=';
  Result.Write(S[1], length(S));
  Result.Write(URL[1], length(URL));
  S := '></BODY></Html>';
  Result.Write(S[1], length(S));
  Result.Seek(0, 0);
end;

procedure TIpHtmlFrame.InternalFreeFrames;
var
   i: integer;
begin
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].Free;
  FFramePanel.Free;
  FFramePanel := nil;
  FFrameCount := 0;
  if HyperPanel <> nil then begin
    FHtml.OnScroll := nil;
    HyperPanel.Hyper := nil;
    HyperPanel.Free;
    HyperPanel := nil;
  end;
  if FDataProvider <> nil then
    FDataProvider.DoLeave(FHtml);
  FHtml.Clear;
  FHtml.Free;
end;

procedure TIpHtmlFrame.InternalCreateFrames;
var
  MW, MH,
  i, R, C, L, T : Integer;
  ColW : TIntArr;
  RowH : TIntArr;
  ColWCount, RowHCount : Integer;
  Scroll : Boolean;
  CurFrameDef : TIpHtmlNodeFrame;
begin
  ColWCount := 0;
  RowHCount := 0;

  if FHtml.HasFrames then begin
    FFramePanel := TPanel.Create(FParent);
    FFramePanel.BevelOuter := bvNone;
    FFramePanel.Align := alClient;
    FFramePanel.Parent := FParent;
    FFramePanel.OnResize := FramePanelResize;
    FFramePanel.FullRepaint := False;
    ColW := CalcMultiLength(FHtml.FrameSet.Cols, FFramePanel.ClientWidth, ColWCount);
    try
      RowH := CalcMultiLength(FHtml.FrameSet.Rows, FFramePanel.ClientHeight, RowHCount);
      try
        R := 0;
        C := 0;
        L := 0;
        T := 0;
        FFrameCount := 0;
        for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
          if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
            CurFrameDef := TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]);
            Pnl[FFrameCount] := TPanel.Create(FFramePanel);
            Pnl[FFrameCount].BevelOuter := bvNone;
            Pnl[FFrameCount].SetBounds(L, T, ColW[C], RowH[R]);
            Pnl[FFrameCount].Parent := FFramePanel;
            Pnl[FFrameCount].FullRepaint := False;

            if CurFrameDef.FrameBorder <> 0 then begin
              Pnl[FFrameCount].BorderStyle := bsSingle;
              Pnl[FFrameCount].BorderWidth := CurFrameDef.FrameBorder;
            end;

            Inc(L, ColW[C]);

            case CurFrameDef.Scrolling of
            hfsAuto, hfsYes :
              Scroll := True;
            else //hfsNo :
              Scroll := False;
            end;

            if CurFrameDef.MarginWidth <> -1 then
              MW := CurFrameDef.MarginWidth
            else
              MW := FViewer.MarginWidth;
            if CurFrameDef.MarginHeight <> -1 then
              MH:= CurFramedef.MarginHeight
            else
              MH := FViewer.MarginHeight;

            FFrames[FFrameCount] :=
              TIpHtmlFrame.Create(FViewer, Pnl[FFrameCount], FDataProvider,
                FViewer.FlagErrors, not Scroll, MW, MH);
            FFrames[FFrameCount].FName := CurFrameDef.Name;
            if C < ColWCount - 1 then
              Inc(C)
            else begin
              C := 0;
              L := 0;
              Inc(T, RowH[R]);
              Inc(R);
            end;
            Inc(FFrameCount);
          end;
        end;
      finally
        RowH.Free;
      end;
    finally
      ColW.Free;
    end;
    Application.ProcessMessages;
    FFrameCount := 0;
    for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
      if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
        FFrames[FFrameCount].FCurURL := FCurURL;
        FFrames[FFrameCount].OpenRelativeURL(
          TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]).Src);
        Inc(FFrameCount);
      end;
    end;
  end else begin
    HyperPanel := TIpHtmlInternalPanel.Create(FParent);
    if FNoScroll then begin
      HyperPanel.HScroll.Visible := False;
      HyperPanel.VScroll.Visible := False;
    end;
    HyperPanel.Parent := FParent;
    HyperPanel.Align := alClient;
    HyperPanel.OnHotChange := FViewer.HotChange;
    HyperPanel.OnCurElementChange := FViewer.CurElementChange;
    HyperPanel.OnHotClick := FViewer.HotClick;
    HyperPanel.OnClick := FViewer.ClientClick;
    HyperPanel.TabStop := FViewer.WantTabs;
    FHtml.FControlParent := HyperPanel;
    FHtml.OnScroll := HyperPanel.ScrollRequest;
    FHtml.OnControlClick := ControlClick;
    FHtml.OnControlClick2 := ControlClick2;
    FHtml.OnControlChange := ControlOnChange;
    FHtml.OnControlEditingdone := ControlOnEditingDone;
    FHtml.OnControlCreate := ControlCreate;
    for i := 0 to Pred(FHtml.FControlList.Count) do
      TIpHtmlNode(FHtml.FControlList[i]).CreateControl(HyperPanel);
    HyperPanel.Hyper := FHtml;
  end;
end;

procedure TIpHtmlFrame.OpenRelativeURL(const URL: string);
var
  S : TStream;
  St, ResourceType : string;
  IsImage : Boolean;
begin
  InOpen := True;
  try
    if Assigned(FDataProvider) then
      St := FDataProvider.BuildURL(FCurURL, URL)
    else
      St := IpUtils.BuildURL(FCurURL, URL);

    if FDataProvider = nil then
      raise EIpHtmlException.Create(SHtmlNoDataProvider);
    if not FDataProvider.DoCheckURL(St, ResourceType) then
      raise EIpHtmlException.Create(SHtmlResUnavail + St);
    IsImage := False;
    S := nil;
    if PosI('image/', ResourceType) = 1 then begin
      IsImage := True;
      S := BuildImagePage(St);
    end else

    if PosI('text/', ResourceType) <> 1 then begin
      FViewer.FHotURL := St;
      FViewer.DoHotClick;
      Exit;
    end;
    FCurURL := St;
    FCurAnchor := '';
    InternalFreeFrames;
    //Memory comsumption is too high without free
    FHtml := TIpHtml.Create;
    InitHtml;
    //see above
    if FDataProvider <> nil then begin
      if not IsImage then
        S := FDataProvider.DoGetHtmlStream(FCurURL, PostData);
      if S <> nil then
        try
          FHtml.FCurURL := FCurURL;
          FHtml.LoadFromStream(S);
          InternalCreateFrames;
        finally
          S.Free;
        end;
    end;
  finally
    InOpen := False;
  end;
end;

procedure TIpHtmlFrame.FramePanelResize(Sender: TObject);
begin
  AlignPanels;
end;

procedure TIpHtmlFrame.MakeAnchorVisible(const URL: string);
var
  E : TIpHtmlNode;
  i : Integer;
begin
  E := FHtml.FindElement(URL);
  if E = nil then E := FHtml.FindElementID(URL);
  FCurAnchor := '';
  if E <> nil then begin
    HyperPanel.GetPageRect;  // Make sure that layout is valid
    E.MakeVisible;
    FCurAnchor := '#'+URL;
  end else
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].MakeAnchorVisible(URL);
end;

procedure TIpHtmlFrame.Home;
begin
  if FHtml <> nil then
    FHtml.Home;
end;

function TIpHtmlFrame.FindFrame(const FrameName: string): TIpHtmlFrame;
var
  i : Integer;
begin
  if AnsiCompareText(FrameName, FName) = 0 then
    Result := Self
  else begin
    Result := nil;
    for i := 0 to Pred(FFrameCount) do begin
      Result := FFrames[i].FindFrame(FrameName);
      if Result <> nil then
        Exit;
    end;
  end;
end;

procedure TIpHtmlFrame.Get(Sender: TIpHtml; const URL: string);
begin
  FViewer.GetURL := URL;
  FViewer.PostURL := '';
  FViewer.PostData := nil;
  PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
end;

procedure TIpHtmlFrame.Post(Sender: TIpHtml; const URL: string;
  FormData: TIpFormDataEntity);
begin
  FViewer.GetURL := '';
  FViewer.PostURL := URL;
  FViewer.PostData := FormData;
  PostMessage(FViewer.Handle, CM_IpHttpGetRequest, 0, PtrInt(Self));
end;

function TIpHtmlFrame.HaveSelection: Boolean;
var
  i : Integer;
begin
  if FHtml = nil then
    Result := False
  else
    if FHtml.HaveSelection then
      Result := True
    else begin
      Result := False;
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          Result := True;
          break;
        end;
    end;
end;

procedure TIpHtmlFrame.CopyToClipboard;
var
  i : Integer;
begin
  if FHtml <> nil then
    if FHtml.HaveSelection then
      FHtml.CopyToClipboard
    else begin
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          FFrames[i].CopyToClipboard;
          Exit;
        end;
    end;
end;

procedure TIpHtmlFrame.SelectAll;
var
  i : Integer;
begin
  if FHtml <> nil then begin
    FHtml.SelectAll;
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].SelectAll;
  end;
end;

procedure TIpHtmlFrame.DeselectAll;
var
  i : Integer;
begin
  if FHtml <> nil then begin
    FHtml.DeselectAll;
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].DeselectAll;
  end;
end;

procedure TIpHtmlFrame.IFrameCreate(Sender: TIpHtml; Parent: TWinControl;
  Frame: TIpHtmlNodeIFRAME; var Control: TWinControl);
var
  MW, MH, W, H : Integer;
  Scroll : Boolean;
  NewFrame : TIpHtmlFrame;
begin
  Control := TPanel.Create(Parent);
  Pnl[FFrameCount] := TPanel(Control);
  TPanel(Control).BevelOuter := bvNone;
  case Frame.Width.LengthType of
  hlAbsolute :
    W := Frame.Width.LengthValue;
  else
    begin
      if Frame.Width.LengthType = hlUndefined then
        W := Parent.ClientWidth
      else
        W := round(Frame.Width.LengthValue * Parent.ClientWidth / 100);
    end;
  end;
  case Frame.Height.LengthType of
  hlAbsolute :
    H := Frame.Height.LengthValue;
  else
    begin
      if Frame.Height.LengthType = hlUndefined then
        H := Parent.ClientHeight
      else
        H := round(Frame.Height.LengthValue * Parent.ClientHeight / 100);
    end;
  end;
  TPanel(Control).SetBounds(0, 0, W, H);
  TPanel(Control).Parent := Parent;
  TPanel(Control).FullRepaint := False;
  case Frame.Scrolling of
  hfsAuto, hfsYes :
    Scroll := True;
  else
    Scroll := False;
  end;
  if Frame.FrameBorder <> 0 then begin
    TPanel(Control).BorderStyle := bsSingle;
    TPanel(Control).BorderWidth := Frame.FrameBorder;
  end;

  if Frame.MarginWidth <> -1 then
    MW := Frame.MarginWidth
  else
    MW := FViewer.MarginWidth;
  if Frame.MarginHeight <> -1 then
    MH:= Frame.MarginHeight
  else
    MH := FViewer.MarginHeight;

  NewFrame := TIpHtmlFrame.Create(FViewer, TCustomPanel(Control), FDataProvider,
                                  FViewer.FlagErrors, not Scroll, MW, MH);
  FFrames[FFrameCount] := NewFrame;
  NewFrame.FName := Frame.FName;
  Application.ProcessMessages;
  NewFrame.FCurURL := FCurURL;
  NewFrame.OpenRelativeURL(Frame.Src);
  Inc(FFrameCount);
  Frame.FFrame := NewFrame;
end;

procedure TIpHtmlFrame.SetHtml(NewHtml: TIpHtml);
begin
  InternalFreeFrames;
  FHtml := NewHtml;
  InitHtml;
  FHtml.DoneLoading := True;
  InternalCreateFrames;
end;

procedure TIpHtmlFrame.EnumDocuments(Enumerator: TIpHtmlEnumerator);
var
  i : Integer;
begin
  if FHtml <> nil then
    Enumerator(FHtml);
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].EnumDocuments(Enumerator);
end;

procedure TIpHtmlFrame.ControlClick(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlClick(Self, Sender, Node);
end;

procedure TIpHtmlFrame.ControlClick2(Sender: TIpHtml; Node: TIpHtmlNodeControl;
  var cancel: boolean);
begin
  FViewer.ControlClick2(Self, Sender, Node, cancel);
end;

procedure TIpHtmlFrame.ControlOnChange(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlOnChange(Self, Sender, Node);
end;

procedure TIpHtmlFrame.ControlOnEditingDone(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlOnEditingdone(Self, Sender, Node);
end;

procedure TIpHtmlFrame.ControlCreate(Sender: TIpHtml; Node: TIpHtmlNodeControl);
begin
  FViewer.ControlCreate(Self, Sender, Node);
end;

{ Returns false if view rect was not changed }
function TIpHtmlFrame.Scroll(Action: TIpScrollAction;
  ADistance: Integer = 100): Boolean;
var
  R : TRect;
  H, W : Integer;
begin
  if FHtml = nil then Exit;
  if HyperPanel = nil then Exit;
  R := FHtml.FPageViewRect;
  H := R.Bottom - R.Top;
  W := R.Right - R.Left;
  case Action of
  hsaHome :
    begin
      R.Top := 0;
      R.Bottom := R.Top + H;
    end;
  hsaEnd :
    begin
      R.Bottom := FHtml.FPageRect.Bottom;
      R.Top := R.Bottom - H;
    end;
  hsaPgUp :
    begin
      OffsetRect(R, 0, -H);
      if R.Top < 0 then begin
        R.Top := 0;
        R.Bottom := R.Top + H;
      end;
    end;
  hsaPgDn :
    begin
      OffsetRect(R, 0, H);
      if R.Bottom > FHtml.FPageRect.Bottom then begin
        R.Bottom := FHtml.FPageRect.Bottom;
        R.Top := R.Bottom - H;
      end;
    end;
  hsaLeft :
    begin
      Result := FHtml.FPageViewRect.Left > 0;
      OffsetRect(R, -ADistance, 0);
      if R.Left < 0 then begin
        R.Left := 0;
        R.Right := R.Left + W;
      end;
    end;
  hsaRight :
    begin
      Result := FHtml.FPageViewRect.Right < FHtml.FPageRect.Right;
      OffsetRect(R, ADistance, 0);
      if R.Right > FHtml.FPageRect.Right then begin
        R.Bottom := FHtml.FPageRect.Right;
        R.Left := R.Right - W;
      end;
    end;
  hsaUp :
    begin
      Result := FHtml.FPageViewRect.Top > 0;
      OffsetRect(R, 0, -ADistance);
      if R.Top < 0 then begin
        R.Top := 0;
        R.Bottom := R.Top + H;
      end;
    end;
  hsaDown :
    begin
      Result := FHtml.FPageViewRect.Bottom < FHtml.FPageRect.Bottom;
      OffsetRect(R, 0, ADistance);
      if R.Bottom > FHtml.FPageRect.Bottom then begin
        R.Bottom := FHtml.FPageRect.Bottom;
        R.Top := R.Bottom - H;
      end;
    end;
  end;
  HyperPanel.ScrollInViewRaw(R);
end;

procedure TIpHtmlFrame.Stop;
begin
  if FDataProvider <> nil then
    FDataProvider.DoLeave(FHtml);
end;

function TIpHtmlFrame.getFrame(i: integer): TIpHtmlFrame;
begin
     result := FFrames[i];
end;

procedure TIpHtmlFrame.RemoveDataProvider;
var
  i: Integer;
begin
  FDataProvider := nil;
  for i:=0 to High(FFrames) do
    if FFrames[i] <> nil then FFrames[i].FDataProvider := nil;
end;


{ TIpHtmlNvFrame }

procedure TIpHtmlNvFrame.InitHtml;
begin
  if FScanner.DataProvider <> nil then
    FHtml.OnGetImageX := FScanner.DataProvider.DoGetImage;
  FHtml.FlagErrors := FFlagErrors;
end;

constructor TIpHtmlNvFrame.Create(Scanner: TIpHtmlCustomScanner;
  DataProvider : TIpAbstractHtmlDataProvider; FlagErrors: Boolean);
begin
  inherited Create;
  FScanner := Scanner;
  FDataProvider := DataProvider;
  FHtml := TIpHtml.Create;
  FFlagErrors := FlagErrors;
  InitHtml;
end;

destructor TIpHtmlNvFrame.Destroy;
var
  i : Integer;
begin
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].Free;
  FHtml.Free;
  inherited;
end;

procedure TIpHtmlNvFrame.OpenURL(const URL: string);
begin
  OpenRelativeURL(URL);
end;

procedure TIpHtmlNvFrame.OpenRelativeURL(const {Base, }URL: string);
var
  S : TStream;
  i, C : Integer;
  ColWCount : Integer;
  St, ResourceType : string;
  CurFrameDef : TIpHtmlNodeFrame;
begin
  if Assigned(FDataProvider) then
    St := FDataProvider.BuildURL(FCurURL, URL)
  else
    St := IpUtils.BuildURL(FCurURL, URL);

  if FDataProvider = nil then
    raise EIpHtmlException.Create(SHtmlNoDataProvider);
  if not FDataProvider.DoCheckURL(St, ResourceType) then
    raise EIpHtmlException.Create(SHtmlResUnavail + St);
  if CompareText(ResourceType, 'text/html') <> 0 then
    Exit;
  if CompareText(St, FCurURL) = 0 then Exit;
  FCurURL := St;
  FCurAnchor := '';
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].Free;
  FFrameCount := 0;
  FDataProvider.DoLeave(FHtml);
  FHtml.Clear;
  ColWCount := 0;
  if FDataProvider <> nil then begin
    S := FDataProvider.DoGetHtmlStream(FCurURL, PostData);
    if S <> nil then
      try
        FHtml.FCurURL := FCurURL;
        FHtml.LoadFromStream(S);
        if FHtml.HasFrames then begin
          C := 0;
          FFrameCount := 0;
          for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
            if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
              CurFrameDef := TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]);
              FFrames[FFrameCount] :=
                TIpHtmlNvFrame.Create(FScanner, FDataProvider,
                  FScanner.FlagErrors);
              FFrames[FFrameCount].FName := CurFrameDef.Name;
              if C < ColWCount - 1 then
                Inc(C)
              else begin
                C := 0;
              end;
              Inc(FFrameCount);
            end;
          end;
          Application.ProcessMessages;
          FFrameCount := 0;
          for i := 0 to Pred(FHtml.FrameSet.ChildCount) do begin
            if FHtml.FrameSet.ChildNode[i] is TIpHtmlNodeFrame then begin
              FFrames[FFrameCount].FCurURL := FCurURL;
              FFrames[FFrameCount].OpenRelativeURL({Base,}
                TIpHtmlNodeFrame(FHtml.FrameSet.ChildNode[i]).Src);
              Inc(FFrameCount);
            end;
          end;
        end;
      finally
        S.Free;
      end;
  end;
end;

procedure TIpHtmlNvFrame.MakeAnchorVisible(const URL: string);
var
  E : TIpHtmlNode;
  i : Integer;
begin
  E := FHtml.FindElement(URL);
  if E = nil then E := FHtml.FindElementID(URL);
  FCurAnchor := '';
  if E <> nil then begin
    E.MakeVisible;
    FCurAnchor := '#'+URL;
  end else
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].MakeAnchorVisible(URL);
end;

procedure TIpHtmlNvFrame.Home;
begin
  if FHtml <> nil then
    FHtml.Home;
end;

function TIpHtmlNvFrame.FindFrame(const FrameName: string): TIpHtmlNvFrame;
var
  i : Integer;
begin
  if AnsiCompareText(FrameName, FName) = 0 then
    Result := Self
  else begin
    Result := nil;
    for i := 0 to Pred(FFrameCount) do begin
      Result := FFrames[i].FindFrame(FrameName);
      if Result <> nil then
        Exit;
    end;
  end;
end;

function TIpHtmlNvFrame.HaveSelection: Boolean;
var
  i : Integer;
begin
  if FHtml = nil then
    Result := False
  else
    if FHtml.HaveSelection then
      Result := True
    else begin
      Result := False;
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          Result := True;
          break;
        end;
    end;
end;

procedure TIpHtmlNvFrame.CopyToClipboard;
var
  i : Integer;
begin
  if FHtml <> nil then
    if FHtml.HaveSelection then
      FHtml.CopyToClipboard
    else begin
      for i := 0 to Pred(FFrameCount) do
        if FFrames[i].HaveSelection then begin
          FFrames[i].CopyToClipboard;
          Exit;
        end;
    end;
end;

procedure TIpHtmlNvFrame.SelectAll;
var
  i : Integer;
begin
  if FHtml <> nil then begin
    FHtml.SelectAll;
    for i := 0 to Pred(FFrameCount) do
      FFrames[i].SelectAll;
  end;
end;

procedure TIpHtmlNvFrame.EnumDocuments(Enumerator: TIpHtmlEnumerator);
var
  i : Integer;
begin
  if FHtml <> nil then
    Enumerator(FHtml);
  for i := 0 to Pred(FFrameCount) do
    FFrames[i].EnumDocuments(Enumerator);
end;

procedure TIpHtmlNVFrame.Stop;
begin
  if FDataProvider <> nil then
    FDataProvider.DoLeave(FHtml);
end;

function TIpHtmlNVFrame.getFrame(i: integer): TIpHtmlNVFrame;
begin
  result := FFrames[i];
end;

{ TIpHtmlCustomPanel }

procedure TIpHtmlCustomPanel.DoHotChange;
begin
  if Assigned(FHotChange) then
    FHotChange(Self);
end;

procedure TIpHtmlCustomPanel.DoHotClick;
begin
  if Assigned(FHotClick) then
    FHotClick(Self);
end;

procedure TIpHtmlCustomPanel.DoOnMouseWheel(Shift: TShiftState; Delta, XPos, YPos: SmallInt);
var
  I: Integer;
begin
  if Delta < 0 then
  begin
    for I := 1 to WheelDelta do
      Scroll(hsaDown);
  end else
  if Delta > 0 then
  begin
    for I := 1 To WheelDelta do
      Scroll(hsaUp);
  end;
end;

procedure TIpHtmlCustomPanel.HotChange(Sender: TObject);
var
  P : TIpHtmlInternalPanel;
  vHtml : TIpHtml;
begin
  P := TIpHtmlInternalPanel(Sender);
  vHtml := P.Hyper;
  if vHtml.HotNode <> nil then begin
    if vHtml.HotPoint.x >= 0 then
      FHotURL := TIpHtmlNodeA(vHtml.HotNode).HRef+
        '?'+IntToStr(vHtml.HotPoint.x)+','+IntToStr(vHtml.HotPoint.y)
    else
      if vHtml.HotNode is TIpHtmlNodeA then
       FHotURL := TIpHtmlNodeA(vHtml.HotNode).HRef
      else
       FHotURL := TIpHtmlNodeAREA(vHtml.HotNode).HRef;
    FHotNode := vHtml.HotNode;
    P.Cursor := crHandPoint;
  end else begin
    FHotNode := nil;
    FHotURL := '';
    P.Cursor := crDefault;
  end;
  DoHotChange;
end;

procedure TIpHtmlCustomPanel.CurElementChange(Sender: TObject);
var
  P : TIpHtmlInternalPanel;
  vHtml : TIpHtml;
begin
  P := TIpHtmlInternalPanel(Sender);
  vHtml := P.Hyper;
  FCurElement := vHtml.CurElement;
  if assigned(FCurElementChange) then
    FCurElementChange(Self);
end;

function TIpHtmlCustomPanel.GetTitle: string;
begin
  if (FMasterFrame <> nil)
  and (FMasterFrame.FHtml <> nil)
  and (FMasterFrame.FHtml.TitleNode <> nil) then
    Result := FMasterFrame.FHtml.TitleNode.Title
  else
    Result := '';
end;

constructor TIpHtmlCustomPanel.Create(AOwner: TComponent);
begin
  inherited;
  BevelOuter := bvNone;
  Caption := '';
  ControlStyle := [csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  TargetStack := TStringList.Create;
  URLStack := TStringList.Create;
  VisitedList := TStringMap.Create(False);
  FLinksUnderlined := DEFAULT_LINKS_UNDERLINED;
  FTextColor := clBlack;
  FLinkColor := clBlue;
  FVLinkColor := clMaroon;
  FALinkColor := clRed;
  FBgColor := clWhite;
  FShowHints := True;
  FMarginWidth := 10;
  FMarginHeight := 10;
  FAllowTextSelect := True;
  {$IFDEF MSWINDOWS}
  FixedTypeface := 'Courier New';
  {$ELSE}
  FixedTypeFace := 'Courier';
  {$ENDIF}
  DefaultTypeFace := Graphics.DefFontData.Name;
  DefaultFontSize := 12;
  FFontQuality := fqDefault;
  FPrintSettings := TIpHtmlPrintSettings.Create;
  FFactBAParag := 1;
  FWantTabs := True;
  FScrollDist := 100;
  FUsePaintBuffer := true;
end;

destructor TIpHtmlCustomPanel.Destroy;
begin
  FPrintSettings.Free;
  TargetStack.Free;
  URLStack.Free;
  FMasterFrame.Free;
  FMasterFrame := nil;
  VisitedList.Free;
  inherited;
end;

procedure TIpHtmlCustomPanel.EraseBackground(DC: HDC);
begin
  //
end;

procedure TIpHtmlCustomPanel.OpenURL(const URL: string);
begin
  InternalOpenURL('', URL);
end;

procedure TIpHtmlCustomPanel.MakeAnchorVisible(const Name: string);
begin
  if FMasterFrame <> nil then
    FMasterFrame.MakeAnchorVisible(Name)
end;

procedure TIpHtmlCustomPanel.InternalOpenURL(const Target, HRef : string);
var
  URL, BaseURL, RelURL : string;
  P : Integer;
  TargetFrame : TIpHtmlFrame;
begin
  if HRef = '' then
    Exit;
  if HRef[1] = '#' then begin
    RelURL := copy(HRef, 2, length(HRef) - 1);
    BaseURL := '';
  end
  else begin
    if FMasterFrame <> nil then begin
      if Assigned(FDataProvider) then
        URL := FDataProvider.BuildURL(FMasterFrame.FHtml.FCurURL, HRef)
      else
        URL := IpUtils.BuildURL(FMasterFrame.FHtml.FCurURL, HRef);
    end
    else
      URL := HRef;
    P := CharPos('#', URL);
    if P = 0 then begin
      RelURL := '';
      BaseURL := URL;
    end else begin
      BaseURL := copy(URL, 1, P - 1);
      RelURL := copy(URL, P + 1, length(URL));
    end;
  end;
  if BaseURL = '' then begin
    if FMasterFrame <> nil then
      Push('', RelURL);
  end
  else  begin
    if not VisitedList.Contains(BaseURL) then
      VisitedList.Add(BaseURL);
    if (Target <> '') and (FMasterFrame <> nil) then
      TargetFrame := FMasterFrame.FindFrame(Target)
    else
      TargetFrame := nil;
    if TargetFrame = nil then begin
      if FMasterFrame <> nil then
        Push('', FMasterFrame.FCurURL + FMasterFrame.FCurAnchor);
      if DataProvider = nil then
        raise EIpHtmlException.Create(SHtmlNoDataProvider);
      if (FMasterFrame = nil)
      or ((FMasterFrame <> nil) and (not FMasterFrame.IsExternal(URL))) then begin
        if (FMasterFrame <> nil) and (FMasterFrame.FHtml <> nil) then
          FDataProvider.DoLeave(FMasterFrame.FHtml);
        FMasterFrame.Free;
        FMasterFrame := nil;
        Application.ProcessMessages;
        FMasterFrame := TIpHtmlFrame.Create(Self, Self, DataProvider, FlagErrors, False,
                                            MarginWidth, MarginHeight);
        FMasterFrame.OpenURL(URL, False);
      end;
    end else begin
      Push(Target, TargetFrame.FCurURL +  TargetFrame.FCurAnchor);
      TargetFrame.OpenURL(BaseURL, False);
    end;
  end;
  if RelURL <> '' then
    FMasterFrame.MakeAnchorVisible(RelURL)
  else
    if FMasterFrame <> nil then
      FMasterFrame.Home;
  if assigned(FDocumentOpen) then
    FDocumentOpen(Self);
end;

procedure TIpHtmlCustomPanel.HotClick(Sender: TObject);
var
  HRef : string;
  Target : string;
begin
  if TIpHtml(Sender).HotNode is TIpHtmlNodeA then begin
    HRef := TIpHtmlNodeA(TIpHtml(Sender).HotNode).HRef;
    Target := TIpHtmlNodeA(TIpHtml(Sender).HotNode).Target;
  end else begin
    HRef := TIpHtmlNodeAREA(TIpHtml(Sender).HotNode).HRef;
    Target := TIpHtmlNodeAREA(TIpHtml(Sender).HotNode).Target;
  end;
  if (FDataProvider <> nil)
  and FDataProvider.CanHandle(HRef) then
    InternalOpenURL(Target, HRef)
  else
    DoHotClick;
end;

procedure TIpHtmlCustomPanel.GoBack;
begin
  if (URLStack.Count > 0) then begin
    if URLStack.Count >= URLStack.count then Stp := URLStack.Count - 1;
    if URLStack.Count > 0 then begin
      InternalOpenURL(TargetStack[Stp], URLStack[Stp]);
      Dec(Stp);
    end;
  end;
end;

function TIpHtmlCustomPanel.canGoBack : boolean;
begin
  Result := (URLStack.Count > 0);
end;

procedure TIpHtmlCustomPanel.GoForward;
begin
  if Stp < URLStack.Count - 1 then begin
    InternalOpenURL(TargetStack[Stp + 1], URLStack[Stp + 1]);
    Inc(Stp);
  end;
end;

function TIpHtmlCustomPanel.canGoForward : boolean;
begin
  Result := (Stp < URLStack.Count - 1);
end;

procedure TIpHtmlCustomPanel.Push(const Target, URL: string);
begin
  if (Stp > 0)
  and (TargetStack[Stp] = Target)
  and (URLStack[Stp] = URL) then Exit;
  while STP < URLStack.Count - 1 do begin
    URLStack.Delete(Stp);
    TargetStack.Delete(Stp);
  end;
  URLStack.Add(URL);
  TargetStack.Add(Target);
  Stp := URLStack.Count - 1;
end;

procedure TIpHtmlCustomPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  //debugln(['TIpHtmlCustomPanel.Notification ',DbgSName(Self),' ',dbgs(Pointer(Self)),' AComponent=',DbgSName(AComponent),' ',dbgs(Pointer(AComponent))]);
  if (Operation = opRemove) then
    if (AComponent = DataProvider) then begin
      DataProvider := nil;
      if Assigned(FMasterFrame) then
        FMasterFrame.RemoveDataProvider;
    end;
  inherited Notification(AComponent, Operation);
end;

procedure TIpHtmlCustomPanel.Paint;
var
  Sz: TSize;
begin
  if csDesigning in ComponentState then begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Pen.Color := clWhite;
    Sz := Canvas.TextExtent('Html');
    Canvas.Polygon([
      Point(0,4),
      Point(0, Height - 5),
      Point(Width div 2 - Sz.cx div 2, Height div 2)]);
    Canvas.Polygon([
      Point(Width - 1,4),
      Point(Width - 1, Height - 5),
      Point(Width div 2 + Sz.cx div 2, Height div 2)]);
    Canvas.Polygon([
      Point(2, 4),
      Point(Width - 3, 4),
      Point(Width div 2, Height div 2 - Sz.cy div 2)]);
    Canvas.Polygon([
      Point(2, Height - 4),
      Point(Width - 3, Height - 4),
      Point(Width div 2, Height div 2 + Sz.cy div 2)]);
    Canvas.Brush.Color := clRed;
    Canvas.Pen.Color := clBlack;
    Canvas.Ellipse(
           Width div 2 - Sz.cx, Height div 2 - Sz.cy,
           Width div 2 + Sz.cx, Height div 2 + Sz.cy);
    Canvas.TextOut(Width div 2 - Sz.cx div 2, Height div 2 - Sz.cy div 2, 'Html');
    Canvas.Brush.Color := clWhite;
    Canvas.Pen.Color := clBlack;
  end;
end;

procedure TIpHtmlCustomPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  if (FMasterFrame = nil)
  or (FMasterFrame.FHtml = nil)
  or (not FMasterFrame.FHtml.CanPaint) then
    if not (csDesigning in ComponentState) then
      FillRect(Message.DC, ClientRect, Brush.Reference.Handle);
  Message.Result := 1;
end;

procedure TIpHtmlCustomPanel.CMIpHttpGetRequest(var Message: TMessage);
var
  FB : TIpHtmlFrame;
begin
  FB := TIpHtmlFrame(Message.lParam);
  if PostData <> nil then begin
    FB.PostData := PostData;
    FB.OpenRelativeURL(PostURL);
    {$IFNDEF HtmlWithoutHttp}
    PostData.Free;
    PostData := nil;
    {$ENDIF}
  end else
    FB.OpenRelativeURL(GetURL);
  if assigned(FDocumentOpen) then
    FDocumentOpen(Self);
end;

procedure TIpHtmlCustomPanel.ClientClick(Sender: TObject);
begin
  Click;
end;

function TIpHtmlCustomPanel.HaveSelection: Boolean;
begin
  Result := (FMasterFrame <> nil) and (FMasterFrame.HaveSelection);
end;

procedure TIpHtmlCustomPanel.SelectAll;
begin
  if FMasterFrame <> nil then begin
    FMasterFrame.SelectAll;
    Invalidate;
  end;
end;

procedure TIpHtmlCustomPanel.DeselectAll;
begin
  if FMasterFrame <> nil then begin
    FMasterFrame.DeselectAll;
    Invalidate;
  end;
end;

procedure TIpHtmlCustomPanel.CopyToClipboard;
begin
  if FMasterFrame <> nil then
    FMasterFrame.CopyToClipboard;
end;

procedure TIpHtmlCustomPanel.SetHtml(NewHtml: TIpHtml);
begin
  if (FMasterFrame <> nil)
  and (FMasterFrame.FHtml <> nil)
  and (FDataProvider <> nil) then
    FDataProvider.DoLeave(FMasterFrame.FHtml);
  FMasterFrame.Free;
  FMasterFrame := nil;
  FMasterFrame := TIpHtmlFrame.Create(Self, Self, DataProvider, FlagErrors, False,
    MarginWidth, MarginHeight);
  if NewHtml <> nil then begin
    NewHtml.FactBAParag := FactBAParag;
    NewHtml.BgColor := BgColor;
    NewHtml.FixedTypeface := FixedTypeface;
    NewHtml.DefaultTypeFace := DefaultTypeFace;
    NewHtml.DefaultFontSize := FDefaultFontSize;
    NewHtml.LinksUnderlined := FLinksUnderlined;
    FMasterFrame.SetHtml(NewHtml);
  end;
end;

procedure TIpHtmlCustomPanel.SetHtmlFromFile(const AFileName: String);
var
  strm: TFileStream;
begin
  strm := TFileStream.Create(AFileName, fmOpenRead + fmShareDenyNone);
  try
    SetHtmlFromStream(strm);
  finally
    strm.Free;
  end;
end;

procedure TIpHtmlCustomPanel.SetHtmlFromStr(NewHtml: string);
var
  strm: TStringStream;
begin
  strm:= TStringStream.Create(NewHtml);
  try
    SetHtmlFromStream(strm);
  finally
    strm.Free;
  end;
end;

procedure TIpHtmlCustomPanel.SetHtmlFromStream(NewHtml: TStream);
var
   iphtml: TIpHtml;
begin
     iphtml:= TIpHtml.Create;
     iphtml.LoadFromStream(NewHtml);
     SetHtml(iphtml);
end;

procedure TIpHtmlCustomPanel.URLCheck(Sender: TIpHtml; const URL: string;
  var Visited: Boolean);
begin
  Visited := VisitedList.Contains(URL);
end;

procedure TIpHtmlCustomPanel.ReportURL(Sender: TIpHtml; const URL: string);
begin
  if (FDataProvider <> nil) then
    FDataProvider.DoReference(URL);
end;

procedure TIpHtmlCustomPanel.EnumDocuments(Enumerator: TIpHtmlEnumerator);
begin
  if FMasterFrame <> nil then
    FMasterFrame.EnumDocuments(Enumerator);
end;

procedure TIpHtmlCustomPanel.ControlClick(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlClick) then
    FControlClick(Self, pFrame, pHtml, pNode);
end;

procedure TIpHtmlCustomPanel.ControlClick2(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl; var pCancel: boolean);
begin
  if assigned(FControlClick2) then
    FControlClick2(Self, pFrame, pHtml, pNode, pCancel);
end;

procedure TIpHtmlCustomPanel.ControlOnChange(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlOnChange) then
    FControlOnChange(Self, pFrame, pHtml, pNode);
end;

procedure TIpHtmlCustomPanel.ControlOnEditingDone(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlOnEditingDone) then
    FControlOnEditingDone(Self, pFrame, pHtml, pNode);
end;

procedure TIpHtmlCustomPanel.ControlCreate(pFrame: TIpHtmlFrame; pHtml: TIpHtml;
  pNode: TIpHtmlNodeControl);
begin
  if assigned(FControlCreate) then
    FControlCreate(Self, pFrame, pHtml, pNode);
end;

function TIpHtmlCustomPanel.IsURLHtml(const URL: string): Boolean;
var
  ResourceType: string;
begin
  Result := (FDataProvider <> nil)
  and FDataProvider.DoCheckURL(URL, ResourceType)
  and (CompareText(ResourceType, 'text/html') = 0);
end;

procedure TIpHtmlCustomPanel.Stop;
begin
  if assigned(FMasterFrame) then
    FMasterFrame.Stop;
end;

{$IFDEF Html_Print}
function TIpHtmlCustomPanel.GetPrintPageCount: Integer;
begin
  if Assigned(FMasterFrame) and Assigned(FMasterFrame.HyperPanel) then
    Result := FMasterFrame.HyperPanel.GetPrintPageCount
  else
    Result := 0;
end;

procedure TIpHtmlCustomPanel.Print(FromPg, ToPg: LongInt);
begin
  if Assigned(FMasterFrame) then
    FMasterFrame.HyperPanel.PrintPages(FromPg, ToPg);
end;

procedure TIpHtmlCustomPanel.PrintPreview;
begin
  if not assigned(printer) then begin
    raise exception.create(
      'Printer has not been assigned, checkout that package'#13+
      'Printer4lazarus.lpk has been installed and OSPrinters'#13+
      'or PrintDialog is in uses clause of main unit');
  end;
  if Assigned(FMasterFrame) then
    FMasterFrame.HyperPanel.PrintPreview;
end;
{$ENDIF}

function TIpHtmlCustomPanel.GetContentSize: TSize;
begin
  if FMasterFrame <> nil then
  begin
    with FMasterFrame.FHtml.FPageRect do
    begin
      Result.cx := Right - Left;
      Result.cy := Bottom - Top;
    end;
  end
  else
    Result := Size(0, 0);
end;

function TIpHtmlCustomPanel.Scroll(Action: TIpScrollAction;
  ADistance: Integer = 100): Boolean;
begin
  if FMasterFrame <> nil then
    Result := FMasterFrame.Scroll(Action, ADistance);
end;

procedure TIpHtmlCustomPanel.WMGetDlgCode(var Msg: TMessage);
begin
  { we want 'em all!  For Lazarus: Then use OnKeyDown! }
  Msg.Result := DLGC_WANTALLKEYS +
                DLGC_WANTARROWS +
                DLGC_WANTCHARS +
                DLGC_WANTTAB
end;

function TIpHtmlCustomPanel.GetVersion : string;
begin
  Result := IpShortVersion;
end;

function TIpHtmlCustomPanel.GetCurUrl: string;
begin
  Result := FMasterFrame.FCurURL;
end;

procedure TIpHtmlCustomPanel.SetVersion(const Value : string);
begin
  { Intentionally empty }
end;

procedure TIpHtmlCustomPanel.SetDefaultTypeFace(const Value: string);
begin
  if FDefaultTypeFace<>Value then begin
    FDefaultTypeFace := Value;
    if (FMasterFrame<>nil)and(FMasterFrame.FHtml<>nil) then begin
      FMasterFrame.FHtml.DefaultTypeFace := FDefaultTypeFace;
      Invalidate;
    end;
  end;
end;

procedure TIpHtmlCustomPanel.SetDefaultFontSize(const Value: integer);
begin
  if FDefaultFontSize<>Value then begin
    FDefaultFontSize := Value;
    if (FMasterFrame<>nil)and(FMasterFrame.FHtml<>nil) then begin
      FMasterFrame.FHtml.DefaultFontSize := FDefaultFontSize;
      Invalidate;
    end;
  end;
end;

procedure TIpHtmlCustomPanel.SetFontQuality(const AValue: TFontQuality);
begin
  if FFontQuality <> AValue then begin
    FFontQuality := AValue;
    if (FMasterFrame <> nil) and (FMasterFrame.FHtml <> nil) then
    begin
      FMasterFrame.FHtml.FontQuality := FFontQuality;
      Invalidate;
    end;
  end;
end;

procedure TIpHtmlCustomPanel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  r: TRect;
begin
  //debugln(['TIpHtmlCustomPanel.CalculatePreferredSize ',DbgSName(Self)]);
  r:=Rect(0,0,0,0);
  if (FMasterFrame<>nil) and (FMasterFrame.HyperPanel<>nil)
  and (FMasterFrame.HyperPanel.Hyper<>nil) then
    r:=FMasterFrame.HyperPanel.Hyper.GetPageRect(Canvas, 0, 0);
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  if PreferredWidth<r.Right-r.Left then
    PreferredWidth:=r.Right-r.Left;
  if PreferredHeight<r.Bottom-r.Top then
    PreferredHeight:=r.Bottom-r.Top;
end;

procedure TIpHtmlCustomPanel.SetFactBAParag(const Value: Real);
var
  V: Real;
begin
  V := Value;
  if V > 2 then
    V := 2
  else if  V < 0 then
    V := 0;
  FFactBAParag := V;
end;

procedure TIpHtmlCustomPanel.SetDataProvider(const AValue: TIpAbstractHtmlDataProvider);
begin
  if FDataProvider=AValue then exit;
  //debugln(['TIpHtmlCustomPanel.SetDataProvider Old=',DbgSName(FDataProvider),' ',dbgs(Pointer(FDataProvider)),' New=',DbgSName(AValue),' ',dbgs(Pointer(AValue))]);
  FDataProvider:=AValue;
  if FDataProvider<>nil then
    FDataProvider.FreeNotification(Self);
end;

function TIpHtmlCustomPanel.FactBAParagNotIs1: Boolean;
begin
  Result := FactBAParag <> 1;
end;

function TIpHtmlCustomPanel.GetVScrollPos: Integer;
begin
  if  FMasterFrame <> nil
  then  Result := FMasterFrame.HyperPanel.VScroll.Position
  else  Result := -1;
end;

procedure TIpHtmlCustomPanel.SetVScrollPos(const Value: Integer);
begin
  if  (FMasterFrame <> nil) and (Value >= 0)
  then  FMasterFrame.HyperPanel.VScroll.Position := Value;
end;


{ TIpHtmlCustomScanner }

function TIpHtmlCustomScanner.GetTitle: string;
begin
  if (FMasterFrame <> nil)
  and (FMasterFrame.FHtml <> nil)
  and (FMasterFrame.FHtml.TitleNode <> nil) then
    Result := FMasterFrame.FHtml.TitleNode.Title
  else
    Result := '';
end;

constructor TIpHtmlCustomScanner.Create(AOwner: TComponent);
begin
  inherited;
  TargetStack := TStringList.Create;
  URLStack := TStringList.Create;
end;

destructor TIpHtmlCustomScanner.Destroy;
begin
  TargetStack.Free;
  URLStack.Free;
  FMasterFrame.Free;
  FMasterFrame := nil;
  inherited;
end;

procedure TIpHtmlCustomScanner.OpenURL(const URL: string);
begin
  InternalOpenURL('', URL);
end;

procedure TIpHtmlCustomScanner.InternalOpenURL(const Target, HRef : string);
var
  URL, BaseURL, RelURL : string;
  P : Integer;
  TargetFrame : TIpHtmlNvFrame;
begin
  if HRef = '' then
    Exit;
  if HRef[1] = '#' then begin
    RelURL := copy(HRef, 2, length(HRef) - 1);
    BaseURL := '';
  end else begin
    if FMasterFrame <> nil then begin
      if Assigned(FDataProvider) then
        URL := FDataProvider.BuildURL(FMasterFrame.FHtml.FCurURL, HRef)
      else
        URL := IpUtils.BuildURL(FMasterFrame.FHtml.FCurURL, HRef);
    end
    else
      URL := HRef;
    P := CharPos('#', URL);
    if P = 0 then begin
      RelURL := '';
      BaseURL := URL;
    end else begin
      BaseURL := copy(URL, 1, P - 1);
      RelURL := copy(URL, P + 1, length(URL));
    end;
  end;
  if BaseURL <> '' then begin
    if (Target <> '') and (FMasterFrame <> nil) then
      TargetFrame := FMasterFrame.FindFrame(Target)
    else
      TargetFrame := nil;
    if TargetFrame = nil then begin
      if FMasterFrame <> nil then
        Push('', FMasterFrame.FCurURL + FMasterFrame.FCurAnchor);
      if DataProvider = nil then
        raise EIpHtmlException.Create(SHtmlNoDataProvider);
      if (FMasterFrame <> nil)
      and (FMasterFrame.FHtml <> nil) then
        FDataProvider.DoLeave(FMasterFrame.FHtml);
      FMasterFrame.Free;
      FMasterFrame := nil;
      Application.ProcessMessages;
      FMasterFrame := TIpHtmlNVFrame.Create(Self, DataProvider, FlagErrors);
      FMasterFrame.OpenURL(URL);
      FCurURL := URL;
    end else begin
      Push(Target, TargetFrame.FCurURL +  TargetFrame.FCurAnchor);
      TargetFrame.OpenURL(BaseURL);
    end;
  end;
  if RelURL <> '' then
    FMasterFrame.MakeAnchorVisible(RelURL)
  else
    FMasterFrame.Home;
end;

procedure TIpHtmlCustomScanner.Push(const Target, URL: string);
begin
  if (Stp > 0)
  and (TargetStack[Stp] = Target)
  and (URLStack[Stp] = URL) then Exit;
  while STP < URLStack.Count - 1 do begin
    URLStack.Delete(Stp);
    TargetStack.Delete(Stp);
  end;
  URLStack.Add(URL);
  TargetStack.Add(Target);
  Stp := URLStack.Count - 1;
end;

procedure TIpHtmlCustomScanner.EnumDocuments(Enumerator: TIpHtmlEnumerator);
begin
  if FMasterFrame <> nil then
    FMasterFrame.EnumDocuments(Enumerator);
end;

function TIpHtmlCustomScanner.IsURLHtml(const URL: string): Boolean;
var
  ResourceType: string;
begin
  Result := (FDataProvider <> nil)
  and FDataProvider.DoCheckURL(URL, ResourceType)
  and (CompareText(ResourceType, 'text/html') = 0);
end;

procedure TIpHtmlCustomScanner.Stop;
begin
  if assigned(FMasterFrame) then
    FMasterFrame.Stop;
end;

function TIpHtmlCustomScanner.GetVersion : string;
begin
  Result := IpShortVersion;
end;

procedure TIpHtmlCustomScanner.SetVersion(const Value : string);
begin
  { Intentionally empty }
end;

function LazFlatSB_GetScrollInfo(hWnd: HWND; BarFlag: Integer;
  var ScrollInfo: TScrollInfo): BOOL; stdcall;
begin
  Result:=LCLIntf.GetScrollInfo(HWnd,BarFlag,ScrollInfo);
end;
  
function LazFlatSB_GetScrollPos(hWnd: HWND; nBar: Integer): Integer; stdcall;
begin
  Result:=LCLIntf.GetScrollPos(HWnd,nBar);
end;

function LazFlatSB_SetScrollPos(hWnd: HWND; nBar, nPos: Integer;
  bRedraw: BOOL): Integer; stdcall;
begin
  Result:=LCLIntf.SetScrollPos(HWnd,nBar,nPos,bRedraw);
end;

function LazFlatSB_SetScrollProp(p1: HWND; index: Integer; newValue: Integer;
  p4: Bool): Bool; stdcall;
begin
  // ToDo
  Result:=true;
end;

function LazFlatSB_SetScrollInfo(hWnd: HWND; BarFlag: Integer;
  const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
begin
  Result:=LCLIntf.SetScrollInfo(HWnd,BarFlag,ScrollInfo,Redraw);
end;

procedure InitScrollProcs;
begin
  @FlatSB_GetScrollInfo := @LazFlatSB_GetScrollInfo;
  @FlatSB_GetScrollPos :=  @LazFlatSB_GetScrollPos;
  @FlatSB_SetScrollPos :=  @LazFlatSB_SetScrollPos;
  @FlatSB_SetScrollProp := @LazFlatSB_SetScrollProp;
  @FlatSB_SetScrollInfo := @LazFlatSB_SetScrollInfo;
end;

procedure Register;
begin
  RegisterComponents('IPro', [TIpHtmlPanel]);
end;


initialization
  InitScrollProcs;

end.

