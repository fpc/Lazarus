{
 /***************************************************************************
                               ComCtrls.pp
                             -------------------
                             Component Library Common Controls
                   Initial Revision  : Sat Apr 10 22:49:32 CST 1999


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
{
@abstract(Just a try to provide the same objects as the Delphi comctrls unit)
@author(TProgressBar - Stefan Hille <stoppok@osibisa.ms.sub.org>)
@author(TTrackBar - Stefan Hille <stoppok@osibisa.ms.sub.org>)
@created(1999)
@lastmod(1999)
}
unit ComCtrls;

{$mode objfpc}
{$H+}

interface

uses
  SysUtils, Classes, Controls,LclLinux, stdCtrls, vclGlobals, lMessages,
  Menus, ImgList, Graphics, Toolwin;


  const
  TBSTATE_CHECKED         = $01;
  TBSTATE_PRESSED         = $02;
  TBSTATE_ENABLED         = $04;
  TBSTATE_HIDDEN          = $08;
  TBSTATE_INDETERMINATE   = $10;
  TBSTATE_WRAP            = $20;
  TBSTATE_ELLIPSES        = $40;
  TBSTATE_MARKED          = $80;

  TBSTYLE_BUTTON          = $00;
  TBSTYLE_SEP             = $01;
  TBSTYLE_CHECK           = $02;
  TBSTYLE_GROUP           = $04;
  TBSTYLE_CHECKGROUP      = TBSTYLE_GROUP or TBSTYLE_CHECK;
  TBSTYLE_DROPDOWN        = $08;
  TBSTYLE_AUTOSIZE        = $0010;
  TBSTYLE_NOPREFIX        = $0020;

  TBSTYLE_TOOLTIPS        = $0100;
  TBSTYLE_WRAPABLE        = $0200;
  TBSTYLE_ALTDRAG         = $0400;
  TBSTYLE_FLAT            = $0800;
  TBSTYLE_LIST            = $1000;
  TBSTYLE_CUSTOMERASE     = $2000;
  TBSTYLE_REGISTERDROP    = $4000;
  TBSTYLE_TRANSPARENT     = $8000;
  TBSTYLE_EX_DRAWDDARROWS = $00000001;


  ToolBarClassName = 'ToolbarWindow32';

 // Toolbar custom draw result flags
{Not used yet, but soon}
  TBCDRF_NOEDGES              = $00010000;  // Don't draw the button edges
  TBCDRF_HILITEHOTTRACK       = $00020000;  // Use color of the button bk when hottracked
  TBCDRF_NOOFFSET             = $00040000;  // Don't offset the button if pressed 
  TBCDRF_NOMARK               = $00080000;  // Don't draw the default highlight of the image/text for TBSTATE_MARKED
  TBCDRF_NOETCHEDEFFECT       = $00100000;  // Don't draw the etched effect for disabled items

  TB_ENABLEBUTTON         = WM_USER + 1;
  TB_CHECKBUTTON          = WM_USER + 2;
  TB_PRESSBUTTON          = WM_USER + 3;
  TB_HIDEBUTTON           = WM_USER + 4;
  TB_INDETERMINATE        = WM_USER + 5;
  TB_MARKBUTTON           = WM_USER + 6;
  TB_ISBUTTONENABLED      = WM_USER + 9;
  TB_ISBUTTONCHECKED      = WM_USER + 10;
  TB_ISBUTTONPRESSED      = WM_USER + 11;
  TB_ISBUTTONHIDDEN       = WM_USER + 12;
  TB_ISBUTTONINDETERMINATE= WM_USER + 13;
  TB_ISBUTTONHIGHLIGHTED  = WM_USER + 14;
  TB_SETSTATE             = WM_USER + 17;
  TB_GETSTATE             = WM_USER + 18;
  TB_ADDBITMAP            = WM_USER + 19;
  TB_INSERTBUTTONA        = WM_USER + 21;
  TB_DELETEBUTTON         = WM_USER + 22;
  TB_GETBUTTON            = WM_USER + 23;
  TB_BUTTONCOUNT          = WM_USER + 24;

  TB_CUSTOMIZE            = WM_USER + 27;
  TB_ADDSTRINGA           = WM_USER + 28;
  TB_GETITEMRECT          = WM_USER + 29;
  TB_BUTTONSTRUCTSIZE     = WM_USER + 30;
  TB_SETBUTTONSIZE        = WM_USER + 31;
  TB_SETBITMAPSIZE        = WM_USER + 32;
  TB_AUTOSIZE             = WM_USER + 33;
  TB_GETTOOLTIPS          = WM_USER + 35;
  TB_SETTOOLTIPS          = WM_USER + 36;
  TB_SETPARENT            = WM_USER + 37;
  TB_SETROWS              = WM_USER + 39;
  TB_GETROWS              = WM_USER + 40;
  TB_SETCMDID             = WM_USER + 42;
  TB_CHANGEBITMAP         = WM_USER + 43;
  TB_GETBITMAP            = WM_USER + 44;
  TB_REPLACEBITMAP        = WM_USER + 46;
  TB_SETINDENT            = WM_USER + 47;
  TB_SETIMAGELIST         = WM_USER + 48;
  TB_GETIMAGELIST         = WM_USER + 49;
  TB_LOADIMAGES           = WM_USER + 50;
  TB_GETRECT              = WM_USER + 51; 
  TB_SETHOTIMAGELIST      = WM_USER + 52;
  TB_GETHOTIMAGELIST      = WM_USER + 53;
  TB_SETDISABLEDIMAGELIST = WM_USER + 54;
  TB_GETDISABLEDIMAGELIST = WM_USER + 55;
  TB_SETSTYLE             = WM_USER + 56;
  TB_GETSTYLE             = WM_USER + 57;
  TB_GETBUTTONSIZE        = WM_USER + 58;
  TB_SETBUTTONWIDTH       = WM_USER + 59;
  TB_SETMAXTEXTROWS       = WM_USER + 60;
  TB_GETTEXTROWS          = WM_USER + 61;
  TB_GETBUTTONINFOW       = WM_USER + 63;
  TB_SETBUTTONINFOW       = WM_USER + 64;
  TB_GETBUTTONINFOA       = WM_USER + 65;
  TB_SETBUTTONINFOA       = WM_USER + 66;

  TB_GETHOTITEM           = WM_USER + 71;
  TB_SETHOTITEM           = WM_USER + 72;

  TB_ADDSTRINGW           = WM_USER = 77;


  TB_INSERTBUTTON = TB_INSERTBUTTONA;
  TB_ADDSTRING = TB_ADDSTRINGA;
  TB_GETBUTTONINFO = TB_GETBUTTONINFOA;
  TB_SETBUTTONINFO = TB_SETBUTTONINFOA;

const
  TBN_First = 0-700;
  TBN_Last = 0-720;

  TBN_BEGINDRAG           = TBN_FIRST-1;
  TBN_ENDDRAG             = TBN_FIRST-2;
  TBN_BEGINADJUST         = TBN_FIRST-3;
  TBN_ENDADJUST           = TBN_FIRST-4;
  TBN_RESET               = TBN_FIRST-5;
  TBN_QUERYINSERT         = TBN_FIRST-6;
  TBN_QUERYDELETE         = TBN_FIRST-7;
  TBN_TOOLBARCHANGE       = TBN_FIRST-8;
  TBN_CUSTHELP            = TBN_FIRST-9;
  TBN_DROPDOWN            = TBN_FIRST-10;
  TBN_CLOSEUP             = TBN_FIRST-11;
  TBN_GETOBJECT           = TBN_FIRST-12;

  TBIF_IMAGE              = $00000001;
  TBIF_TEXT               = $00000002;
  TBIF_STATE              = $00000004;
  TBIF_STYLE              = $00000008;
  TBIF_LPARAM             = $00000010;
  TBIF_COMMAND            = $00000020;
  TBIF_SIZE               = $00000040;


type
   PTBButton = ^TTBButton;
  _TBBUTTON = packed record
    iBitmap: Integer;
    idCommand: Integer;
    fsState: Byte;
    fsStyle: Byte;
    bReserved: array[1..2] of Byte;
    dwData: Longint;
    iString: Integer;
  end;
  TTBButton = _TBBUTTON;

  TBBUTTONINFOA = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PAnsiChar;
    cchText: Integer;
  end;

  TBBUTTONINFOW = packed record
    cbSize: UINT;
    dwMask: DWORD;
    idCommand: Integer;
    iImage: Integer;
    fsState: Byte;
    fsStyle: Byte;
    cx: Word;
    lParam: DWORD;
    pszText: PWideChar;
    cchText: Integer;
  end;
  TBBUTTONINFO = TBBUTTONINFOA;

  PTBButtonInfoA = ^TTBButtonInfoA;
  PTBButtonInfoW = ^TTBButtonInfoW;

  PTBButtonInfo = PTBButtonInfoA;

  TTBButtonInfoA = TBBUTTONINFOA;
  TTBButtonInfoW = TBBUTTONINFOW;
  TTBButtonInfo = TTBButtonInfoA;

type
  PTBAddBitmap = ^TTBAddBitmap;
  tagTBADDBITMAP = packed record
    hInst: THandle;
    nID: UINT;
  end;
  TTBAddBitmap = tagTBADDBITMAP;

  TBADDBITMAP = tagTBADDBITMAP;

type
  TBREPLACEBITMAP = packed record
    hInstOld: THandle;
    nIDOld: Cardinal;
    hInstNew: THandle;
    nIDNew: Cardinal;
    nButtons: Integer;
  end;
  PTBReplaceBitmap = ^TTBReplaceBitmap;
  TTBReplaceBitmap = TBREPLACEBITMAP;

  tagNMTOOLBARA = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PAnsiChar;
  end;

  tagNMTOOLBARW = packed record
    hdr: TNMHdr;
    iItem: Integer;
    tbButton: TTBButton;
    cchText: Integer;
    pszText: PWideChar;
  end;

  tagNMTOOLBAR = tagNMTOOLBARA;
  PNMToolBarA = ^TNMToolBarA;
  PNMToolBarW = ^TNMToolBarW;
  PNMToolBar = PNMToolBarA;
  TNMToolBarA = tagNMTOOLBARA;
  TNMToolBarW = tagNMTOOLBARW;
  TNMToolBar = TNMToolBarA;

const

  CCS_TOP                 = $00000001;
  CCS_NOMOVEY             = $00000002;
  CCS_BOTTOM              = $00000003;
  CCS_NORESIZE            = $00000004;
  CCS_NOPARENTALIGN       = $00000008;
  CCS_ADJUSTABLE          = $00000020;
  CCS_NODIVIDER           = $00000040;
  CCS_VERT                = $00000080;
  CCS_LEFT                = (CCS_VERT or CCS_TOP);
  CCS_RIGHT               = (CCS_VERT or CCS_BOTTOM);
  CCS_NOMOVEX             = (CCS_VERT or CCS_NOMOVEY);

  ICC_LISTVIEW_CLASSES   = $00000001; 
  ICC_TREEVIEW_CLASSES   = $00000002; 
  ICC_BAR_CLASSES        = $00000004; 
  ICC_TAB_CLASSES        = $00000008; 
  ICC_UPDOWN_CLASS       = $00000010; 
  ICC_PROGRESS_CLASS     = $00000020; 
  ICC_HOTKEY_CLASS       = $00000040; 
  ICC_ANIMATE_CLASS      = $00000080; 
  ICC_WIN95_CLASSES      = $000000FF;
  ICC_DATE_CLASSES       = $00000100; 
  ICC_USEREX_CLASSES     = $00000200; 
  ICC_COOL_CLASSES       = $00000400; 
  ICC_INTERNET_CLASSES   = $00000800;
  ICC_PAGESCROLLER_CLASS = $00001000;
  ICC_NATIVEFNTCTL_CLASS = $00002000;

type
{ TAlignment = Class(TWinControl)
  public
   constructor Create(AOwner : TComponent); override;
   destructor Destroy; override;
  end;
 }
  TStatusPanelStyle = (psText, psOwnerDraw);
  TStatusPanelBevel = (pbNone, pbLowered, pbRaised);

  TStatusBar = class;  //forward declaration

  TStatusPanel = class(TCollectionItem)
  private
    FText: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FBevel: TStatusPanelBevel;
    FParentBiDiMode: Boolean;
    FStyle: TStatusPanelStyle;
    //FUpdateNeeded: Boolean;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: TStatusPanelBevel);
    procedure SetStyle(Value: TStatusPanelStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(aCollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Bevel: TStatusPanelBevel read FBevel write SetBevel default pbLowered;
    property Style: TStatusPanelStyle read FStyle write SetStyle default psText;
    property Text: string read FText write SetText;
    property Width: Integer read FWidth write SetWidth;
  end;

  TStatusPanels = class(TCollection)
  private
    FStatusBar: TStatusBar;
    function GetItem(Index: Integer): TStatusPanel;
    procedure SetItem(Index: Integer; Value: TStatusPanel);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(StatusBar: TStatusBar);
    function Add: TStatusPanel;
    property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
  end;

 TStatusBar = Class(TWinControl)
  private
    FCanvas : TCanvas;
    FPanels : TStatusPanels;
    FSimpleText : String;
    FSimplePanel : Boolean;
    //FContext : Integer;
    //FMessage : Integer;
    //FAlignmentWidget : TAlignment;
    procedure SetPanels(Value: TStatusPanels);
    procedure SetSimpleText(Value : String);
    procedure SetSimplePanel(Value : Boolean);
    Procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
    Procedure DrawDivider(X : Integer);
    Procedure DrawBevel(xLeft, PanelNum : Integer );
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    property Canvas : TCanvas read FCanvas;
  published
    property Panels: TStatusPanels read FPanels write SetPanels;
    property SimpleText : String read FSimpleText write SetSimpleText;
    property SimplePanel : Boolean read FSimplePanel write SetSimplePanel;
    property Visible;
 end;


 TListItems = class;  //forward declaration!
 TCustomListView = class;  //forward declaration!

 TListItem = class(TPersistent)
  private
    FOwner : TListItems;
    FSubItems: TStrings;
    //FIndex   : Integer;
    FCaption : String;
    Procedure SetCaption(const Value : String);
    Procedure SetSubItems(Value : TStrings);
    Function GetIndex : Integer;
  public
    constructor Create(AOwner : TListItems);
    destructor Destroy; override;
    procedure Delete;
    property Caption : String read FCaption write SetCaption;
    property Index : Integer read GetIndex;
    property Owner : TListItems read FOwner;
    property SubItems : TStrings read FSubItems write SetSubItems;
  end;

 TListItems = class(TPersistent)
  private
    FOwner : TCustomListView;
    Function GetCount : Integer;
  protected
    Function GetItem(Index : Integer): TListItem;
    procedure SetCount(Value : Integer);
    procedure SetITem(Index : Integer; Value : TListItem);
  public
    constructor Create(AOwner : TCustomListView);
    destructor Destroy; override;
    function Add:TListItem;
    Procedure Delete(Index : Integer);
    function Insert(Index : Integer) : TListItem;
    property Count : Integer read GetCount write SetCount;
    property Item[Index : Integer]: TListItem read GetItem write SetItem; default;
    property Owner : TCustomListView read FOwner;
  end;


 TCustomListView = class(TWinControl)
  private
    //FReadOnly : Boolean;
    FListItems : TListItems;
    procedure SetItems(Value : TListItems);
  protected
    ParentWindow : TScrolledWindow;
    procedure Delete(Item : TListItem);
    procedure InsertItem(Item : TListItem);
    property Items : TListItems read FListItems write SetItems;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
  end;

 TListView = class(TCustomListView)
  published
    property Items;
  end;

  TProgressBarOrientation = (pbHorizontal, pbVertical, pbRightToLeft, pbTopDown);
  
  { TProgressBar }
  {
    @abstract(Simple progressbar.)
    Introduced by Author Name <stoppok@osibisa.ms.sub.org>
    Currently maintained by Maintainer Name <stoppok@osibisa.ms.sub.org>
  }
  TProgressBar = class(TWinControl)
  private
    FMin              : Integer;
    FMax              : Integer;
    FStep             : Integer;
    FPosition         : Integer;
    FSmooth           : boolean;
    FBarShowText      : boolean;
    FBarTextFormat    : string;
    FOrientation      : TProgressBarOrientation;
    function GetMin: Integer;
    function GetMax: Integer;
    function GetPosition: Integer;
    procedure SetParams(AMin, AMax: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetStep(Value: Integer);
    procedure SetSmooth (Value : boolean);
    procedure SetBarShowText (Value : boolean);
    procedure SetOrientation (Value : TProgressBarOrientation);
  protected
    procedure ApplyChanges;
    procedure AttachSignals; override; 
    procedure InitializeWnd; override;
{    procedure SetBarTextFormat; }
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt;
    procedure StepBy(Delta: Integer);
  published
    property Min: Integer read GetMin write SetMin;
    property Max: Integer read GetMax write SetMax;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Step: Integer read FStep write SetStep default 10;
    property Smooth : boolean read FSmooth write SetSmooth default false;
    property Align;
    property Visible;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property Enabled;
    property OnEnter;
    property OnExit;
    property BorderWidth;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{ ... to be implemented for Delphi compatibility 
//    property Anchors;
//  property Constraints;
//  property OnStartDock;
//  property OnEndDock;
}
 published { additional functionality } 
    property BarShowText : boolean read FBarShowText write SetBarShowText;
{   property BarTextFormat : string read FBarTextFormat write SetBarTextFormat; }
 end;


  
{ TToolBar }

const
  CN_DROPDOWNCLOSED = LM_USER + $1000;

type
  HIMAGELIST = THANDLE;  //MOVE!!!!!
  TToolButtonStyle = (tbsButton, tbsCheck, tbsDropDown, tbsSeparator, tbsDivider);
  TToolButtonState = (tbsChecked, tbsPressed, tbsEnabled, tbsHidden,
                      tbsIndeterminate, tbsWrap, tbsEllipses, tbsMarked);
  TToolBar = class;
  TToolButton = class;


  TToolButton = class(TButtonControl)
  private
    FAllowAllUp: Boolean;
    FAutoSize: Boolean;
    FDown: Boolean;
    FGrouped: Boolean;
    FImageIndex: Integer;
    FIndeterminate: Boolean;
    FMarked: Boolean;
    FMenuItem: TMenuItem;
    FDropdownMenu: TPopupMenu;
    FWrap: Boolean;
    FStyle: TToolButtonStyle;
    FUpdateCount: Integer;
    function GetButtonState: Byte;
    function GetIndex: Integer;
    function IsCheckedStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetAutoSize(Value: Boolean);
    procedure SetButtonState(State: Byte);
    procedure SetDown(Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetGrouped(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetIndeterminate(Value: Boolean);
    procedure SetMarked(Value: Boolean);
    procedure SetMenuItem(Value: TMenuItem);
    procedure SetStyle(Value: TToolButtonStyle);
    procedure SetWrap(Value: Boolean);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure CMVisibleChanged(var Message: TLMessage); message CM_VISIBLECHANGED;
  protected
    FToolBar: TToolBar;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RefreshControl; virtual;
    procedure SetToolBar(AToolBar: TToolBar);
    procedure UpdateControl; virtual;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function CheckMenuDropdown: Boolean; dynamic;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Index: Integer read GetIndex;
  published
    property AllowAllUp: Boolean read FAllowAllUp write FAllowAllUp default False;
    property AutoSize: Boolean read FAutoSize write SetAutoSize default False;
    property Caption;
    property Down: Boolean read FDown write SetDown stored IsCheckedStored default False;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropdownMenu: TPopupMenu read FDropdownMenu write SetDropdownMenu;
    property Enabled;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property Height stored False;
    property ImageIndex: Integer read FImageIndex write SetImageIndex stored IsImageIndexStored default -1;
    property Indeterminate: Boolean read FIndeterminate write SetIndeterminate default False;
    property Marked: Boolean read FMarked write SetMarked default False;
    property MenuItem: TMenuItem read FMenuItem write SetMenuItem;
    property ParentShowHint;
    property PopupMenu;
    property Wrap: Boolean read FWrap write SetWrap default False;
    property ShowHint;
    property Style: TToolButtonStyle read FStyle write SetStyle default tbsButton;
    property Visible;
    property Width stored IsWidthStored;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;


  TToolBar = class(TToolWindow)
  private
    FButtonWidth: Integer;
    FButtonHeight: Integer;
    FButtons: TList;
    FCaption: string;
    FShowCaptions: Boolean;
    FList: Boolean;
    FFlat: Boolean;
    FTransparent: Boolean;
    FWrapable: Boolean;
    FImages: TCustomImageList;
    FDisabledImages: TCustomImageList;
    FHotImages: TCustomImageList;
    FIndent: Integer;
    FNewStyle: Boolean;
    FNullBitmap: TBitmap;
    FOldHandle: HBitmap;
    FUpdateCount: Integer;
    FHeightMargin: Integer;
    { Toolbar menu support }
    FCaptureChangeCancels: Boolean;
    FInMenuLoop: Boolean;
    FTempMenu: TPopupMenu;
    FButtonMenu: TMenuItem;
    FMenuButton: TToolButton;
    FMenuResult: Boolean;
    function ButtonIndex(OldIndex, ALeft, ATop: Integer): Integer;
    procedure LoadImages(AImages: TCustomImageList);
    function GetButton(Index: Integer): TToolButton;
    function GetButtonCount: Integer;
    procedure GetButtonSize(var AWidth, AHeight: Integer);
    function GetRowCount: Integer;
    procedure SetList(Value: Boolean);
    procedure SetShowCaptions(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetWrapable(Value: Boolean);
    procedure InsertButton(Control: TControl);
    procedure RemoveButton(Control: TControl);
    function RefreshButton(Index: Integer): Boolean;
    procedure UpdateButton(Index: Integer);
    procedure UpdateButtons;
    procedure UpdateButtonState(Index: Integer);
    procedure UpdateButtonStates;
    function UpdateItem(Message, FromIndex, ToIndex: Integer): Boolean;
    function UpdateItem2(Message, FromIndex, ToIndex: Integer): Boolean;
    procedure ClearTempMenu;
    procedure CreateButtons(NewWidth, NewHeight: Integer);
    procedure SetButtonWidth(Value: Integer);
    procedure SetButtonHeight(Value: Integer);
    procedure UpdateImages;
    procedure ImageListChange(Sender: TObject);
    procedure SetImageList(Value: HImageList);
    procedure SetImages(Value: TCustomImageList);
    procedure DisabledImageListChange(Sender: TObject);
    procedure SetDisabledImageList(Value: HImageList);
    procedure SetDisabledImages(Value: TCustomImageList);
    procedure HotImageListChange(Sender: TObject);
    procedure SetHotImageList(Value: HImageList);
    procedure SetHotImages(Value: TCustomImageList);
    procedure SetIndent(Value: Integer);
    procedure AdjustControl(Control: TControl);
    procedure RecreateButtons;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ResizeButtons;
    function InternalButtonCount: Integer;
    function ReorderButton(OldIndex, ALeft, ATop: Integer): Integer;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure WMGetDlgCode(var Message: TLMessage); message LM_GETDLGCODE;
    procedure WMGetText(var Message: TLMGetText); message LM_GETTEXT;
    procedure WMGetTextLength(var Message: TLMGetTextLength); message LM_GETTEXTLENGTH;
    procedure WMKeyDown(var Message: TLMKeyDown); message LM_KEYDOWN;
    procedure WMNotifyFormat(var Message: TLMessage); message LM_NOTIFYFORMAT;
    procedure WMSetText(var Message: TLMSetText); message LM_SETTEXT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    procedure WMSysChar(var Message: TLMSysChar); message LM_SYSCHAR;
    procedure WMSysCommand(var Message: TLMSysCommand); message LM_SYSCOMMAND;
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
    procedure WMWindowPosChanging(var Message: TLMWindowPosChanging); message LM_WINDOWPOSCHANGING;
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    procedure CMControlChange(var Message: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CNChar(var Message: TLMChar); message CN_CHAR;
    procedure CNSysKeyDown(var Message: TLMSysKeyDown); message CN_SYSKEYDOWN;
    procedure CMSysFontChanged(var Message: TLMessage); message CM_SYSFONTCHANGED;
    procedure CNDropDownClosed(var Message: TLMessage); message CN_DROPDOWNCLOSED;
    procedure CNNotify(var Message: TLMNotify); message CN_NOTIFY;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CancelMenu; dynamic;
    procedure ChangeScale(M, D: Integer); override;
    function CheckMenuDropdown(Button: TToolButton): Boolean; dynamic;
    procedure ClickButton(Button: TToolButton); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function FindButtonFromAccel(Accel: Word): TToolButton;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure InitMenu(Button: TToolButton); dynamic;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RepositionButton(Index: Integer);
    procedure RepositionButtons(Index: Integer);
    procedure WndProc(var Message: TLMessage); override;
    function WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function TrackMenu(Button: TToolButton): Boolean; dynamic;
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TToolButton read GetButton;
    property ButtonList : TList read FButtons;
    property RowCount: Integer read GetRowCount;
  published
    property Align default alTop;
    property AutoSize;
    property BorderWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 23;
    property Caption;
    property Color;
    property Ctl3D;
    property DisabledImages: TCustomImageList read FDisabledImages write SetDisabledImages;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EdgeBorders default [ebTop];
    property EdgeInner;
    property EdgeOuter;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property Height default 32;
    property HotImages: TCustomImageList read FHotImages write SetHotImages;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read FIndent write SetIndent default 0;
    property List: Boolean read FList write SetList default False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaptions: Boolean read FShowCaptions write SetShowCaptions default False;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Visible;
    property Wrapable: Boolean read FWrapable write SetWrapable default True;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;



  { TTrackBar }

  TTrackBarOrientation = (trHorizontal, trVertical);
  TTickMark = (tmBottomRight, tmTopLeft, tmBoth);
  TTickStyle = (tsNone, tsAuto, tsManual);
  TTrackBarScalePos = (trLeft, trRight, trTop, trBottom);

  {
    @abstract(Simple trackbar.)
    Introduced by Author Name <stoppok@osibisa.ms.sub.org>
    Currently maintained by Maintainer Name <stoppok@osibisa.ms.sub.org>
  }
  TTrackBar = class(TWinControl)
  private
    FOrientation: TTrackBarOrientation;
    FTickMarks: TTickMark;
    FTickStyle: TTickStyle;
    FLineSize: Integer;
    FPageSize: Integer;
    FMin: Integer;
    FMax: Integer;
    FFrequency: Integer;
    FPosition: Integer;
    FSelStart: Integer;
    FSelEnd: Integer;
    FShowScale : boolean;
    FScalePos : TTrackBarScalePos;
    FScaleDigits : integer;
    FOnChange: TNotifyEvent;
    procedure SetOrientation(Value: TTrackBarOrientation);
    procedure SetParams(APosition, AMin, AMax: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetFrequency(Value: Integer);
    procedure SetTickStyle(Value: TTickStyle);
    procedure SetTickMarks(Value: TTickMark);
    procedure SetLineSize(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetSelEnd(Value: Integer);
    procedure UpdateSelection;
  private { additional functionality }
    procedure SetShowScale(Value: boolean);
    procedure SetScalePos(Value: TTrackBarScalePos);
  protected
    procedure ApplyChanges;
    procedure DoChange(var msg); message LM_CHANGED;
    procedure AttachSignals; override; 
    procedure InitializeWnd; override;
{ ... what about these?
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DestroyWnd; override;
}    
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTick(Value: Integer);
  published
    property Ctl3D; 
    property LineSize: Integer read FLineSize write SetLineSize default 1;
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation;
    property PageSize: Integer read FPageSize write SetPageSize default 2;
    property Frequency: Integer read FFrequency write SetFrequency;
    property Position: Integer read FPosition write SetPosition;
    property SelEnd: Integer read FSelEnd write SetSelEnd;
    property SelStart: Integer read FSelStart write SetSelStart;
    property TickMarks: TTickMark read FTickMarks write SetTickMarks;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle;
    property Visible; 
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyUp;
    property Enabled;
    property OnKeyPress;
    property DragCursor;
    property ParentCtl3D;
    property ParentShowHint;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property PopupMenu;
    property ShowHint;
    property TabOrder; 
    property TabStop default True; 
    property OnStartDrag;
  published { additional functionality }
    property ShowScale : boolean read FShowScale write SetShowScale;
    property ScalePos : TTrackBarScalePos read FScalePos write SetScalePos; 
    property DragMode; 
  end;



function InitCommonControl(CC: Integer): Boolean;
procedure CheckCommonControl(CC: Integer);
Implementation

uses Forms,Interfaces;

const
  ButtonStates: array[TToolButtonState] of Word = (TBSTATE_CHECKED,
    TBSTATE_PRESSED, TBSTATE_ENABLED, TBSTATE_HIDDEN, TBSTATE_INDETERMINATE,
    TBSTATE_WRAP, TBSTATE_ELLIPSES, TBSTATE_MARKED);

  ButtonStyles: array[TToolButtonStyle] of Word = (TBSTYLE_BUTTON, TBSTYLE_CHECK,
    TBSTYLE_DROPDOWN, TBSTYLE_SEP, TBSTYLE_SEP);

{ Toolbar menu support }

var
  //ToolMenuKeyHook: HHOOK;
  //ToolMenuHook: HHOOK;
  //InitDone: Boolean;
  //MenuToolBar: TToolBar;
  //MenuToolBar2: TToolBar;
  //MenuButtonIndex: Integer;
  //LastMenuItem: TMenuItem;
  //LastMousePos: TPoint;
  StillModal: Boolean;


function InitCommonControl(CC: Integer): Boolean;
begin
Result := True;
end;

procedure CheckCommonControl(CC: Integer);
begin

end;

{$I statusbar.inc}
{$I statuspanel.inc}
{$I statuspanels.inc}
{ $I alignment.inc}
{$I listitem.inc}
{$I listitems.inc}
{$I customlistview.inc}
{$I progressbar.inc}
{$I toolbutton.inc}
{$I toolbar.inc}
{$I trackbar.inc}

end.

{ =============================================================================

  $Log$
  Revision 1.5  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.4  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.3  2001/01/30 18:15:02  lazarus
  Added code for TStatusBar
  I'm now capturing WMPainT and doing the drawing myself.
  Shane

  Revision 1.2  2000/12/29 18:33:54  lazarus
  TStatusBar's create and destroy were not set to override TWinControls so they were never called.
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.22  2000/03/10 12:55:58  lazarus
  *** empty log message ***

  Revision 1.21  2000/01/10 00:07:12  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries


}
