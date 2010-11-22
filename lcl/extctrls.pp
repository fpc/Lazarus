{  $Id$  }
{
 /***************************************************************************
                               extctrls.pp
                               -----------
                             Component Library Extended Controls
                   Initial Revision  : Sat Jul 26 12:04:35 PDT 1999

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ExtCtrls;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  SysUtils, Types, Classes, LCLStrConsts, LCLType, LCLProc, LResources, Controls,
  Forms, StdCtrls, lMessages, GraphType, Graphics, LCLIntf, CustomTimer, Themes,
  LCLClasses, Menus, popupnotifier, ImgList;

type

  { TCustomPage }

  TPageFlag = (
    pfAdded,  // handle of page added to notebook handle
    pfAdding, // currently handle of page adding to notebook handle
    pfRemoving, // currently removing page handle from notebook handle
    pfInserting // currently inserting page into notebook
    );
  TPageFlags = set of TPageFlag;

  TCustomPage = class(TWinControl)
  private
    FTabVisible: Boolean;
    FFlags: TPageFlags;
    FImageIndex: TImageIndex;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetTabVisible: Boolean;
    procedure SetImageIndex(const AValue: TImageIndex);
    procedure SetTabVisible(const AValue: Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    procedure SetParent(AParent: TWinControl); override;
    property Flags: TPageFlags read FFlags write FFlags;
    procedure CMHitTest(var Message: TLMNCHITTEST); message CM_HITTEST;
    function GetPageIndex: integer;
    procedure SetPageIndex(AValue: Integer);
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure DoHide; virtual;
    procedure DoShow; virtual;
    procedure DestroyHandle; override;
    procedure RealSetText(const AValue: TCaption); override;
  public
    constructor Create(TheOwner: TComponent); override;
    function CanTab: boolean; override;
    function IsControlVisible: Boolean; override;
    function HandleObjectShouldBeVisible: boolean; override;
    function VisibleIndex: integer;
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property TabVisible: Boolean read GetTabVisible write SetTabVisible default True;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Left stored False;
    property Top stored False;
    property Width stored False;
    property Height stored False;
    property TabOrder stored False;
    property Visible stored false;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;

  TCustomPageClass = class of TCustomPage;


  { TNBPages }

  TCustomNotebook = class;

  TNBPages = class(TStrings)
  private
    FPageList: TListWithEvent;
    FNotebook: TCustomNotebook;
    procedure PageListChange(Ptr: Pointer; AnAction: TListNotification);
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(thePageList: TListWithEvent;
                       theNotebook: TCustomNotebook);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;


  { TCustomNotebook }

  TTabChangingEvent = procedure(Sender: TObject;
    var AllowChange: Boolean) of object;

  TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);

  TTabGetImageEvent = procedure(Sender: TObject; TabIndex: Integer;
    var ImageIndex: Integer) of object;

  TNoteBookOption = (nboShowCloseButtons, nboMultiLine, nboHidePageListPopup);
  TNoteBookOptions = set of TNoteBookOption;
  TNoteBookCapability = (nbcShowCloseButtons, nbcMultiLine, nbcPageListPopup);
  TNoteBookCapabilities = set of TNoteBookCapability;

  TCustomNotebook = class(TWinControl)
  private
    FAccess: TStrings; // TNBPages
    FAddingPages: boolean;
    FImages: TImageList;
    FImageListChangeLink: TChangeLink;
    FLoadedPageIndex: integer;
    FOnChanging: TTabChangingEvent;
    FOnCloseTabClicked: TNotifyEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    FOnPageChanged: TNotifyEvent;
    FOptions: TNoteBookOptions;
    FPageIndex: Integer;
    FPageIndexOnLastChange: integer;// needed for unique OnChange events
    FPageIndexOnLastShow: integer;
    FPageList: TList;  // TListWithEvent of TCustomPage
    FShowTabs: Boolean;
    FTabPosition: TTabPosition;
    procedure CNNotify(var Message: TLMNotify); message CN_NOTIFY;
    procedure DoSendPageIndex;
    procedure DoSendShowTabs;
    procedure DoSendTabPosition;
    procedure DoImageListChange(Sender: TObject);
    function GetActivePage: String;
    function GetActivePageComponent: TCustomPage;
    function GetMultiLine: Boolean;
    function GetPage(AIndex: Integer): TCustomPage;
    function GetPageCount : integer;
    function GetPageIndex: Integer;
    function FindVisiblePage(Index: Integer): Integer;
    procedure InsertPage(APage: TCustomPage; Index: Integer);
    function IsStoredActivePage: boolean;
    procedure AddRemovePageHandle(APage: TCustomPage);
    procedure MoveTab(Sender: TObject; NewIndex: Integer);
    procedure SetMultiLine(const AValue: Boolean);
    procedure WSMovePage(APage: TCustomPage; NewIndex: Integer);
    procedure PageRemoved(Index: Integer);
    procedure RemovePage(Index: Integer);
    procedure SetActivePage(const Value: String);
    procedure SetActivePageComponent(const AValue: TCustomPage);
    procedure SetImages(const AValue: TImageList);
    procedure SetOptions(const AValue: TNoteBookOptions);
    procedure SetPageIndex(AValue: Integer);
    procedure SetPages(AValue: TStrings);
    procedure SetShowTabs(AValue: Boolean);
    procedure SetTabPosition(tabPos: TTabPosition);
    procedure ShowCurrentPage;
    procedure UpdateAllDesignerFlags;
    procedure UpdateDesignerFlags(APageIndex: integer);
  protected
    PageClass: TCustomPageClass;
    class procedure WSRegisterClass; override;
    procedure CreateWnd; override;
    procedure DoCreateWnd; virtual;
    procedure DoChange; virtual;
    procedure Change; virtual;
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    function  DialogChar(var Message: TLMKey): boolean; override;
    procedure ShowControl(APage: TControl); override;
    procedure UpdateTabProperties; virtual;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property ActivePageComponent: TCustomPage read GetActivePageComponent
                                              write SetActivePageComponent;
    property ActivePage: String read GetActivePage write SetActivePage
                                                      stored IsStoredActivePage;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function TabIndexAtClientPos(ClientPos: TPoint): integer;
    function TabRect(AIndex: Integer): TRect;
    function GetImageIndex(ThePageIndex: Integer): Integer; virtual;
    function IndexOf(APage: TCustomPage): integer;
    function CustomPage(Index: integer): TCustomPage;
    function CanChangePageIndex: boolean; virtual;
    function GetMinimumTabWidth: integer; virtual;
    function GetMinimumTabHeight: integer; virtual;
    function GetCapabilities: TNoteBookCapabilities; virtual;
  public
    procedure DoCloseTabClicked(APage: TCustomPage); virtual;
    property Images: TImageList read FImages write SetImages;
    property MultiLine: Boolean read GetMultiLine write SetMultiLine default False;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnCloseTabClicked: TNotifyEvent read FOnCloseTabClicked
                                             write FOnCloseTabClicked;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex
                                                write FOnGetImageIndex;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property Options: TNoteBookOptions read FOptions write SetOptions default [];
    property Page[Index: Integer]: TCustomPage read GetPage;
    property PageCount: integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex write SetPageIndex default -1;
    property PageList: TList read FPageList;
    property Pages: TStrings read FAccess write SetPages;
    property ShowTabs: Boolean read FShowTabs write SetShowTabs default True;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition default tpTop;
  published
    property TabStop default true;
  end;


  { TPage }

  TPage = class(TCustomPage)
  published
    property Caption;
    property ChildSizing;
    property ClientWidth;
    property ClientHeight;
    property Font;
    property ImageIndex;
    property Left stored False;
    property Top stored False;
    property Width stored False;
    property Height stored False;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property PageIndex stored False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder stored False;
    property Visible stored False;
  end deprecated;


  { TNotebook }

  TNotebook = class(TCustomNotebook)
  private
    function GetActiveNotebookPageComponent: TPage;
    function GetNoteBookPage(Index: Integer): TPage;
    procedure SetActiveNotebookPageComponent(const AValue: TPage);
  public
    constructor Create(TheOwner: TComponent); override;
    property Page[Index: Integer]: TPage read GetNoteBookPage;
    property ActivePageComponent: TPage read GetActiveNotebookPageComponent
                                        write SetActiveNotebookPageComponent;
    property Pages;
  published
    property ActivePage;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property OnChangeBounds;
    property OnChanging;
    property OnCloseTabClicked;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPageChanged;
    property OnResize;
    property OnStartDrag;
    property Options;
    property PageIndex;
    property ParentFont;
    property PopupMenu;
    property ShowTabs;
    property TabOrder;
    property TabStop;
  end deprecated;

  TUNBPage = class;

  TBeforeShowPageEvent = procedure (ASender: TObject; ANewPage: TUNBPage; ANewIndex: Integer) of object;

  { TUNBPage }

  TUNBPage = class(TCustomControl)
  private
    FOnBeforeShow: TBeforeShowPageEvent;
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    // Lazarus-specific TPage events
    // OnBeforeShow occurs before a page is displayed, so that
    // preparations can be executed in it's user interface, for example
    property OnBeforeShow: TBeforeShowPageEvent read FOnBeforeShow write FOnBeforeShow;
    // Other events
    property ChildSizing;
    property ClientWidth;
    property ClientHeight;
    property Left stored False;
    property Top stored False;
    property Width stored False;
    property Height stored False;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder stored False;
    property Visible stored False;
  end;

  { TUNBPages }

  TUntabbedNotebook = class;

  TUNBPages = class(TStrings)
  private
    FPageList: TListWithEvent;
    FUNotebook: TUntabbedNotebook;
    procedure PageListChange(Ptr: Pointer; AnAction: TListNotification);
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(thePageList: TListWithEvent;
                       theUNotebook: TUntabbedNotebook);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
//    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

  { TUntabbedNotebook }

  TUntabbedNotebook = class(TCustomControl)
  private
    FPages: TStrings; // TUNBPages
    FPageIndex: Integer;
    FPageList: TListWithEvent;
{    function GetActivePage: String;
    function GetActivePageComponent: TCustomPage;}
    function GetPage(AIndex: Integer): TUNBPage;
//    function GetPageCount : integer;
    function GetPageIndex: Integer;
{    function FindVisiblePage(Index: Integer): Integer;}
    procedure InsertPage(APage: TUNBPage; Index: Integer);
{    procedure MovePage(APage: TCustomPage; NewIndex: Integer);
    procedure RemovePage(Index: Integer);
    procedure SetActivePage(const Value: String);}
    procedure SetPageIndex(AValue: Integer);
{    procedure ShowCurrentPage;}
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
{    function TabIndexAtClientPos(ClientPos: TPoint): integer;
    function TabRect(AIndex: Integer): TRect;
    function GetImageIndex(ThePageIndex: Integer): Integer; virtual;
    function IndexOf(APage: TCustomPage): integer;
    function CustomPage(Index: integer): TCustomPage;}
  public
    property Page[Index: Integer]: TUNBPage read GetPage;
//    property PageCount: integer read GetPageCount;
//    property PageList: TList read FPageList;
  published
    // TUntabbedNotebook specific properties
    property PageIndex: Integer read GetPageIndex write SetPageIndex default -1;
    property Pages: TStrings read FPages;
//    property ActivePage: String read GetActivePage write SetActivePage;
    // Generic properties
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property OnChangeBounds;
//    property OnChanging;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
//    property OnPageChanged;
    property OnResize;
    property OnStartDrag;
//    property Options;
//    property PageIndex;
    property PopupMenu;
    property TabOrder;
    property TabStop;
  end;

  { Timer }

  TTimer = class (TCustomTimer)
  published
    property Enabled;
    property Interval;
    property OnTimer;
    property OnStartTimer;
    property OnStopTimer;
  end;


  { TIdleTimer
    For example:
      Do something after 2 seconds after user input and idle.
      AutoEnabled:=true;
      AutoStartEvent:=[itaOnIdle];    // start the timer on first idle
      AutoEndEvent:=[itaOnUserInput]; // end on any user input

    If the OnTimer event works in several chunks, set FireOnIdle:=true.
    The OnTimer event will then be called on idle until FireOnIdle is false.
    FireOnIdle is set to false on any user input. }

  TIdleTimerAutoEvent = (
    itaOnIdle,
    itaOnIdleEnd,
    itaOnUserInput
    );
  TIdleTimerAutoEvents = set of TIdleTimerAutoEvent;

  { TCustomIdleTimer }

  TCustomIdleTimer = class(TCustomTimer)
  private
    FAutoEnabled: boolean;
    FAutoEndEvent: TIdleTimerAutoEvent;
    FAutoStartEvent: TIdleTimerAutoEvent;
    FFireOnIdle: boolean;
    FHandlersConnected: boolean;
    procedure UpdateHandlers;
  protected
    procedure SetAutoEnabled(const AValue: boolean); virtual;
    procedure DoOnIdle(Sender: TObject; var Done: Boolean); virtual;
    procedure DoOnIdleEnd(Sender: TObject); virtual;
    procedure DoOnUserInput(Sender: TObject; Msg: Cardinal); virtual;
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    property AutoEnabled: boolean read FAutoEnabled
                                  write SetAutoEnabled default False;
    property AutoStartEvent: TIdleTimerAutoEvent read FAutoStartEvent
                                    write FAutoStartEvent default itaOnIdle;
    property AutoEndEvent: TIdleTimerAutoEvent read FAutoEndEvent
                                 write FAutoEndEvent default itaOnUserInput;
    property FireOnIdle: boolean read FFireOnIdle write FFireOnIdle default false;
  end;

  TIdleTimer = class(TCustomIdleTimer)
  published
    property AutoEnabled;
    property AutoStartEvent;
    property AutoEndEvent;
    property Enabled;
    property Interval;
    property OnTimer;
    property OnStartTimer;
    property OnStopTimer;
  end;

  { TShape }

  TShapeType = (stRectangle, stSquare, stRoundRect, stRoundSquare,
    stEllipse, stCircle, stSquaredDiamond, stDiamond, stTriangle);

  TShape = class(TGraphicControl)
  private
    FPen: TPen;
    FBrush: TBrush;
    FShape: TShapeType;
    procedure SetBrush(Value: TBrush);
    procedure SetPen(Value: TPen);
    procedure SetShape(Value: TShapeType);
  protected
    class procedure WSRegisterClass; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure StyleChanged(Sender: TObject);
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Brush: TBrush read FBrush write SetBrush;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property Pen: TPen read FPen write SetPen;
    property OnChangeBounds;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property Shape: TShapeType read FShape write SetShape default stRectangle;
    property ShowHint;
    property Visible;
  end;


  { TCustomSplitter }

  TResizeStyle = (
    rsLine,     // draw a line, don't update splitter position during moving
    rsNone,     // draw nothing and don't update splitter position during moving
    rsPattern,  // draw a dot pattern, don't update splitter position during moving
    rsUpdate    // draw nothing, update splitter position during moving
  );

  TCanResizeEvent = procedure(Sender: TObject; var NewSize: Integer;
    var Accept: Boolean) of object;
  { TCustomSplitter is a control to interactively resize another control.
    It is a vertical or horizontal bar anchored to a side of a control.
    You can either set the Align property to alLeft (alRight,alTop,alBottom),
    then it will become a vertical bar, aligned to the left and when the user
    moves it with the mouse, the control to the left with the same Align=alLeft
    will be resized.
    The second more flexible possibility is to set the properties Align=alNone,
    AnchorSides and ResizeAnchor.
    }
  TCustomSplitter = class(TCustomControl)
  private
    FAutoSnap: boolean;
    FBeveled: boolean;
    FMinSize: integer;
    FMouseInControl: Boolean;
    FOnCanResize: TCanResizeEvent;
    FOnMoved: TNotifyEvent;
    FResizeAnchor: TAnchorKind;
    FResizeStyle: TResizeStyle;
    FSplitDragging: Boolean;
    FSplitterStartMouseXY: TPoint; // in screen coordinates
    FSplitterStartLeftTop: TPoint; // in screen coordinates
    FSplitterWindow: HWND;
    function GetResizeControl: TControl;
    procedure SetBeveled(const AValue: boolean);
    procedure SetMinSize(const AValue: integer);
  protected
    class procedure WSRegisterClass; override;
    procedure CheckAlignment;
    function CheckNewSize(var NewSize: integer): boolean; virtual;
    function FindAlignControl: TControl;
    function FindAlignOtherControl: TControl;

    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    
    procedure Paint; override;
    procedure SetAlign(Value: TAlign); override;
    procedure SetAnchors(const AValue: TAnchors); override;
    procedure SetResizeAnchor(const AValue: TAnchorKind); virtual;
    procedure SetResizeControl(const AValue: TControl); virtual;
    procedure StartSplitterMove(const MouseXY: TPoint);
    procedure StopSplitterMove(const MouseXY: TPoint);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AnchorSplitter(Kind: TAnchorKind; AControl: TControl);
    property ResizeControl: TControl read GetResizeControl write SetResizeControl;
    function GetOtherResizeControl: TControl;
    procedure MoveSplitter(Offset: integer);
    procedure SetSplitterPosition(NewPosition: integer);
    function GetSplitterPosition: integer;
  public
    property Align default alLeft;
    property AutoSnap: boolean read FAutoSnap write FAutoSnap default true;
    property Beveled: boolean read FBeveled write SetBeveled default false;
    property Cursor default crHSplit;
    property MinSize: integer read FMinSize write SetMinSize default 30;
    property OnCanResize: TCanResizeEvent read FOnCanResize write FOnCanResize;
    property OnMoved: TNotifyEvent read FOnMoved write FOnMoved;
    property ResizeAnchor: TAnchorKind read FResizeAnchor write SetResizeAnchor default akLeft;
    property ResizeStyle: TResizeStyle read FResizeStyle write FResizeStyle default rsUpdate;
  end;


  { TSplitter }

  TSplitter = class(TCustomSplitter)
  published
    property Align;
    property Anchors;
    property AutoSnap;
    property Beveled;
    property Color;
    property Constraints;
    property Cursor;
    property Height;
    property MinSize;
    property OnCanResize;
    property OnChangeBounds;
    property OnMoved;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ResizeAnchor;
    property ResizeStyle;
    property ShowHint;
    property Visible;
    property Width;
  end;


  { TPaintBox }

  TPaintBox = class(TGraphicControl)
  protected
    class procedure WSRegisterClass; override;
    procedure Paint; override;
    class function GetControlClassDefaultSize: TSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
//    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
//    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
//    property OnStartDock;
    property OnStartDrag;
  end;


  { TCustomImage }

  TCustomImage = class(TGraphicControl)
  private
    FOnPictureChanged: TNotifyEvent;
    FPicture: TPicture;
    FCenter: Boolean;
    FProportional: Boolean;
    FTransparent: Boolean;
    FStretch: Boolean;
    FUseAncestorCanvas: boolean;
    function  GetCanvas: TCanvas;
    procedure SetPicture(const AValue: TPicture);
    procedure SetCenter(const AValue : Boolean);
    procedure SetProportional(const AValue: Boolean);
    procedure SetStretch(const AValue : Boolean);
    procedure SetTransparent(const AValue : Boolean);
  protected
    class procedure WSRegisterClass; override;
    procedure PictureChanged(Sender : TObject); virtual;
    function DestRect: TRect; virtual;
    procedure CalculatePreferredSize(var PreferredWidth,
                                     PreferredHeight: integer;
                                     WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas: TCanvas read GetCanvas;
  public
    property Align;
    property AutoSize;
    property Center: Boolean read FCenter write SetCenter default False;
    property Constraints;
    property Picture: TPicture read FPicture write SetPicture;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Proportional: Boolean read FProportional write SetProportional default False;
    property OnPictureChanged: TNotifyEvent read FOnPictureChanged write FOnPictureChanged;
  end;


  { TImage }

  TImage = class(TCustomImage)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Center;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnPictureChanged;
    property OnResize;
    property OnStartDrag;
    property ParentShowHint;
    property Picture;
    property PopupMenu;
    property Proportional;
    property ShowHint;
    property Stretch;
    property Transparent;
    property Visible;
  end;


  { TBevel }

  TBevelStyle = (bsLowered, bsRaised);
  TBevelShape=(bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine, bsRightLine, bsSpacer);

  TBevel = class(TGraphicControl)
  private
    FStyle:TBevelStyle;
    FShape:TBevelShape;
    procedure SetStyle(AStyle: TBevelStyle);
    procedure SetShape(AShape: TBevelShape);
  protected
    class procedure WSRegisterClass; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property ParentShowHint;
    property Shape: TBevelShape read FShape write SetShape default bsBox;
    property ShowHint;
    property Style: TBevelStyle read FStyle write SetStyle default bsLowered;
    property Visible;
    property OnChangeBounds;
    property OnResize;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
  end;


  { TCustomRadioGroup }

  TColumnLayout = (
    clHorizontalThenVertical,
    clVerticalThenHorizontal
    );

  TCustomRadioGroup = class(TCustomGroupBox)
  private
    FAutoFill: Boolean;
    FButtonList: TList; // list of TRadioButton
    FColumnLayout: TColumnLayout;
    FColumns: integer;
    FCreatingWnd: boolean;
    FHiddenButton: TRadioButton;
    FIgnoreClicks: boolean;
    FItemIndex: integer;
    FItems: TStrings;
    FLastClickedItemIndex: integer;
    FOnClick: TNotifyEvent;
    FReading: boolean;
    FUpdatingItems: Boolean;
    procedure Changed(Sender: TObject);
    procedure Clicked(Sender: TObject);
    procedure ItemEnter(Sender: TObject);
    procedure ItemExit(Sender: TObject);
    procedure ItemKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ItemResize(Sender: TObject);
    procedure ItemsChanged(Sender: TObject);
    procedure SetAutoFill(const AValue: Boolean);
    procedure SetColumnLayout(const AValue: TColumnLayout);
    procedure UpdateControlsPerLine;
    procedure UpdateItems;
    procedure UpdateTabStops;
  protected
    class procedure WSRegisterClass; override;
    procedure Loaded; override;
    procedure InitializeWnd; override;
    procedure UpdateRadioButtonStates; virtual;
    procedure ReadState(Reader: TReader); override;
    procedure SetItems(Value: TStrings);
    procedure SetColumns(Value: integer);
    procedure SetItemIndex(Value: integer);
    function GetItemIndex: integer;
    procedure CheckItemIndexChanged; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function CanModify: boolean; virtual;
    function Rows: integer;
  public
    property AutoFill: Boolean read FAutoFill write SetAutoFill;
    property ItemIndex: integer read GetItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
    property Columns: integer read FColumns write SetColumns default 1;
    property ColumnLayout: TColumnLayout read FColumnLayout write SetColumnLayout default clHorizontalThenVertical;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;


  { TRadioGroup }

  TRadioGroup = class(TCustomRadioGroup)
  published
    property Align;
    property Anchors;
    property AutoFill;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;
    property Items;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;


  { TCustomCheckGroup }

  TCheckGroupClicked = procedure(Sender: TObject; Index: integer) of object;

  TCustomCheckGroup = class(TCustomGroupBox)
  private
    FAutoFill: boolean;
    FButtonList: TList; // list of TCheckBox
    FColumnLayout: TColumnLayout;
    FCreatingWnd: boolean;
    FItems: TStrings;
    FColumns: integer;
    FOnItemClick: TCheckGroupClicked;
    FUpdatingItems: Boolean;
    function GetChecked(Index: integer): boolean;
    function GetCheckEnabled(Index: integer): boolean;
    procedure Clicked(Sender: TObject);
    procedure DoClick(Index: integer);
    procedure ItemsChanged (Sender : TObject);
    procedure RaiseIndexOutOfBounds(Index: integer );
    procedure SetAutoFill(const AValue: boolean);
    procedure SetChecked(Index: integer; const AValue: boolean);
    procedure SetCheckEnabled(Index: integer; const AValue: boolean);
    procedure SetColumnLayout(const AValue: TColumnLayout);
    procedure UpdateItems;
    procedure UpdateControlsPerLine;
  protected
    class procedure WSRegisterClass; override;
    procedure SetItems(Value: TStrings);
    procedure SetColumns(Value: integer);
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure Loaded; override;
    procedure DoOnResize; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Rows: integer;
  public
    property AutoFill: boolean read FAutoFill write SetAutoFill;
    property Items: TStrings read FItems write SetItems;
    property Checked[Index: integer]: boolean read GetChecked write SetChecked;
    property CheckEnabled[Index: integer]: boolean read GetCheckEnabled write SetCheckEnabled;
    property Columns: integer read FColumns write SetColumns default 1;
    property ColumnLayout: TColumnLayout read FColumnLayout write SetColumnLayout default clHorizontalThenVertical;
    property OnItemClick: TCheckGroupClicked read FOnItemClick write FOnItemClick;
  end;


  { TCheckGroup }

  TCheckGroup = class(TCustomCheckGroup)
  published
    property Align;
    property Anchors;
    property AutoFill;
    property AutoSize;
    property BorderSpacing;
    property Caption;
    property ChildSizing;
    property Color;
    property ColumnLayout;
    property Columns;
    property Constraints;
    property Enabled;
    property Font;
    property Items;
    property OnChangeBounds;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnItemClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentFont;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property Visible;
  end;


  { TBoundLabel }

  TBoundLabel = class(TCustomLabel)
  public
    constructor Create(TheOwner: TComponent); override;
    property FocusControl;
  published
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Height;
    property Left;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Top;
    property Layout;
    property WordWrap;
    property Width;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //property OnEnter;
    //property OnExit;
    property OnStartDrag;
  end;


  { TCustomLabeledEdit }

  TLabelPosition = (lpAbove, lpBelow, lpLeft, lpRight);

  TCustomLabeledEdit = class(TCustomEdit)
  private
    FEditLabel: TBoundLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
  protected
    class procedure WSRegisterClass; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetName(const Value: TComponentName); override;
    procedure DoPositionLabel; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CreateInternalLabel; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    property EditLabel: TBoundLabel read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition
                                         write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing
                                                                      default 3;
  end;


  { TLabeledEdit }

  TLabeledEdit = class(TCustomLabeledEdit)
  published
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property EchoMode;
    property EditLabel;
    property Enabled;
    property Font;
    property LabelPosition;
    property LabelSpacing;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;


  { TCustomPanel }

  TPanelBevel = TBevelCut;
  TBevelWidth = 1..Maxint;

  TCustomPanel = class(TCustomControl)
  private
    FBevelInner, FBevelOuter : TPanelBevel;
    FBevelWidth : TBevelWidth;
    FAlignment : TAlignment;
    FFullRepaint: Boolean;
    procedure SetAlignment(const Value : TAlignment);
    procedure SetBevelInner(const Value: TPanelBevel);
    procedure SetBevelOuter(const Value: TPanelBevel);
    procedure SetBevelWidth(const Value: TBevelWidth);
  protected
    class procedure WSRegisterClass; override;
    procedure AdjustClientRect(var aRect: TRect); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    function GetDefaultDockCaption: String; override;
    procedure Loaded; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure Paint; override;
    procedure UpdateParentColorChange;
  public
    constructor Create(TheOwner: TComponent); override;
    property Align default alNone;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property Color default {$ifdef UseCLDefault}clDefault{$else}clBtnFace{$endif};
    property FullRepaint: Boolean read FFullRepaint write FFullRepaint default True;
    property ParentColor default true;
    property TabStop default False;
  end;


  { TPanel }

  TPanel = class(TCustomPanel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

  { TCustomTrayIcon }

  TBalloonFlags = (bfNone, bfInfo, bfWarning, bfError);

  TCustomTrayIcon = class(TLCLComponent)
  private
    FAnimate: Boolean;
    FAnimateTimer: TTimer;
    FCurAnimationStep: Integer;
    FBalloonFlags: TBalloonFlags;
    FBalloonHint: string;
    FBalloonTimeout: Integer;
    FBalloonTitle: string;
    FPopUpMenu: TPopupMenu;
    FIcon: TIcon;
    FIcons: TCustomImageList;
    FHint: string;
    FVisible, FShowIcon: Boolean;
    FNotifier: TPopupNotifier;
    FTimer: TTimer;
    FOnPaint, FOnClick, FOnDblClick: TNotifyEvent;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    function GetAnimateInterval: Cardinal;
    function  GetCanvas: TCanvas;
    procedure SetAnimate(const AValue: Boolean);
    procedure SetAnimateInterval(const AValue: Cardinal);
    procedure SetHint(const AValue: string);
    procedure SetIcon(const AValue: TIcon);
    procedure SetIcons(const AValue: TCustomImageList);
    procedure SetPopUpMenu(const AValue: TPopupMenu);
    procedure SetVisible(Value: Boolean);
    procedure HandleNotifierClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure HandleNotifierTimeout(Sender: TObject);
    procedure HandleOnAnimateTimer(Sender: TObject);
    procedure IconChanged(Sender: TObject);
  protected
    class procedure WSRegisterClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Handle: HWND;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Hide: Boolean;
    function Show: Boolean;
    procedure InternalUpdate;
    procedure ShowBalloonHint;
    function GetPosition: TPoint;
    { Properties }
    property Animate: Boolean read FAnimate write SetAnimate default False;
    property AnimateInterval: Cardinal read GetAnimateInterval write SetAnimateInterval default 1000;
    property BalloonFlags: TBalloonFlags read FBalloonFlags write FBalloonFlags default bfNone;
    property BalloonHint: string read FBalloonHint write FBalloonHint;
    property BalloonTimeout: Integer read FBalloonTimeout write FBalloonTimeout default 3000;
    property BalloonTitle: string read FBalloonTitle write FBalloonTitle;
    property Canvas: TCanvas read GetCanvas;
    property PopUpMenu: TPopupMenu read FPopUpMenu write SetPopUpMenu;
    property Icon: TIcon read FIcon write SetIcon;
    property Icons: TCustomImageList read FIcons write SetIcons;
    property Hint: string read FHint write SetHint;
    property ShowIcon: Boolean read FShowIcon write FShowIcon default True;
    property Visible: Boolean read FVisible write SetVisible default False;
    { Events }
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  { TTrayIcon }
  
  TTrayIcon = class(TCustomTrayIcon)
  published
    property BalloonFlags;
    property BalloonHint;
    property BalloonTimeout;
    property BalloonTitle;
    property PopUpMenu;
    property Icon;
    property Hint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnPaint;
  end;

const
  TCN_First = 0-550;
  TCN_SELCHANGE = TCN_FIRST - 1;
  TCN_SELCHANGING = TCN_FIRST - 2;

procedure Register;

implementation

// !!! Avoid unit circles. Only add units if really needed.
uses
  Math, WSExtCtrls;

{.$define INSTALL_TUNTABBEDNOTEBOOK}

procedure Register;
begin
  RegisterComponents('Standard',[TRadioGroup,TCheckGroup,TPanel]);
  RegisterComponents('Additional',[TImage,TShape,TBevel,TPaintBox,
    {$ifdef INSTALL_TUNTABBEDNOTEBOOK} TUntabbedNotebook, {$ENDIF}
    TLabeledEdit, TSplitter, TTrayIcon]);
  RegisterComponents('System',[TTimer,TIdleTimer]);
  RegisterNoIcon([TNotebook{%H-}, TPage{$ifdef INSTALL_TUNTABBEDNOTEBOOK}, TUNBPage{$ENDIF}]);
end;

{$I custompage.inc}
{$I page.inc}
{$I customnotebook.inc}
{$I notebook.inc}
{$I untabbednotebook.inc}
{$I timer.inc}
{$I idletimer.inc}
{$I shape.inc}
{$I customsplitter.inc}
{$I paintbox.inc}
{$I customcheckgroup.inc}
{$I boundlabel.inc}
{$I customlabelededit.inc}
{$I custompanel.inc}
{$I radiogroup.inc}
{$I bevel.inc}
{$I customimage.inc}
{$I customtrayicon.inc}

initialization
  DockSplitterClass := TSplitter;

end.
