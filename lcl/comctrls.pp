{ $Id$}
{
 /***************************************************************************
                               ComCtrls.pp
                               -----------
                             Component Library Common Controls
                   Initial Revision  : Sat Apr 10 22:49:32 CST 1999


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
  SysUtils, Classes, Controls, Forms, LclLinux, LCLType, StdCtrls, ExtCtrls,
  vclGlobals, lMessages, Menus, ImgList, GraphType, Graphics, ToolWin, CommCtrl,
  Buttons;

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
    property SimpleText: String read FSimpleText write SetSimpleText;
    property SimplePanel: Boolean read FSimplePanel write SetSimplePanel default True;
    property Visible;
  end;


 { Custom draw }

  TCustomDrawTarget = (dtControl, dtItem, dtSubItem);
  TCustomDrawStage = (cdPrePaint, cdPostPaint, cdPreErase, cdPostErase);
  TCustomDrawStateFlag = (cdsSelected, cdsGrayed, cdsDisabled, cdsChecked,
    cdsFocused, cdsDefault, cdsHot, cdsMarked, cdsIndeterminate);
  TCustomDrawState = set of TCustomDrawStateFlag;


  { TListView }
  
  TListItems = class;  //forward declaration!
  TCustomListView = class;  //forward declaration!
  TSortType = (stNone, stData, stText, stBoth);

  TListItem = class(TPersistent)
  private
    FOwner: TListItems;
    FSubItems: TStrings;
    //FIndex   : Integer;
    FCaption: String;
    FData: Pointer;
    FImageIndex: Integer;
    FDestroying: Boolean;
    function GetState(const AIndex: Integer): Boolean;
    procedure SetState(const AIndex: Integer; const AState: Boolean);

    procedure SetData(const AValue: Pointer);
    procedure SetImageIndex(const AValue: Integer);
    procedure SetCaption(const AValue : String);
//    Procedure SetSubItems(Value : TStrings);
    function GetIndex : Integer;
  protected
    Procedure ItemChanged(sender : TObject);  //called by the onchange of the tstringlist in TListItem

  public
    constructor Create(AOwner : TListItems);
    destructor Destroy; override;
    procedure Delete;
    property Caption : String read FCaption write SetCaption;
    property Cut: Boolean index 0 read GetState write SetState;
    property Data: Pointer read FData write SetData;
    property DropTarget: Boolean index 1 read GetState write SetState;
    property Focused: Boolean index 2 read GetState write SetState;
    property Index : Integer read GetIndex;
    property Owner : TListItems read FOwner;
    property Selected: Boolean index 3 read GetState write SetState;
    property SubItems : TStrings read FSubItems write FSubItems;//SetSubItems;
    property ImageIndex : Integer read FImageIndex write SetImageIndex default -1;
  end;

  TListItems = class(TPersistent)
  private
    FOwner : TCustomListView;
    FItems : TList;
    function GetCount : Integer;
    procedure ItemDeleted(const AItem: TListItem); //called by TListItem when freed
  protected
    function GetItem(const AIndex: Integer): TListItem;
    procedure SetITem(const AIndex: Integer; const AValue: TListItem);
    procedure ItemChanged(sender : TObject);  //called by TListItem in response to SubItems changing
  public
    function Add: TListItem;
    constructor Create(AOwner : TCustomListView);
    destructor Destroy; override;
    procedure Delete(const AIndex : Integer);
    function FindData(const AData: Pointer): TListItem;
    function Insert(const AIndex: Integer) : TListItem;
    property Count: Integer read GetCount;
    property Item[const AIndex: Integer]: TListItem read GetItem write SetItem; default;
    property Owner : TCustomListView read FOwner;
  end;

  TWidth = 0..MaxInt;

  TListColumn = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FCaption: String;
    FMinWidth: TWidth;
    FMaxWidth: TWidth;
    FVisible: Boolean;
    FWidth: TWidth;
    FTag: Integer;
    function GetWidth: TWidth;
    procedure SetVisible(const AValue: Boolean);
    procedure SetAutoSize(const AValue: Boolean);
    procedure SetMinWidth(const AValue: TWidth);
    procedure SetMaxWidth(const AValue: TWidth);
    procedure SetWidth(const AValue: TWidth);
    procedure SetCaption(const AValue: String);
    procedure SetAlignment(const AValue: TAlignment);
//    procedure SetImageIndex(const AValue: TImageIndex);
  protected
//    procedure SetIndex(const AValue: Integer); override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Caption: string read FCaption write SetCaption;
//    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex;
    property MaxWidth: TWidth read FMaxWidth write SetMaxWidth;
    property MinWidth: TWidth read FMinWidth write SetMinWidth;
    property Tag: Integer read FTag write FTag;
    property Visible : Boolean read FVisible write SetVisible;
    property Width: TWidth read GetWidth write SetWidth;
  end;

  TListColumns = class(TCollection)
  private
    FOwner: TCustomListView;
    function GetItem(const AIndex: Integer): TListColumn;
    procedure SetItem(const AIndex: Integer; const AValue: TListColumn);
  protected
    procedure Update(AnItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TCustomListView);
    function Add: TListColumn;
    property Owner: TCustomListView read FOwner;
    property Items[const AIndex: Integer]: TListColumn read GetItem write SetItem; default;
  end;

  TItemChange = (ctText, ctImage, ctState);
  TViewStyle = (vsList,vsReport);

  TLVChangeEvent = procedure(Sender: TObject; Item: TListItem; Change: TItemChange) of object;
  TLVColumnClickEvent = procedure(Sender: TObject; Column: TListColumn) of object;
  TLVColumnRClickEvent = procedure(Sender: TObject; Column: TListColumn; Point: TPoint) of object;
  TLVDeletedEvent = procedure(Sender: TObject; Item: TListItem) of object;
  TLVSelectItemEvent = procedure(Sender: TObject; Item: TListItem; Selected: Boolean) of object;
  
  TListViewState = (lvMultiSelect, lvUpdateNeeded);
  TListViewStates = set of TListViewState;

  TCustomListView = class(TWinControl)
  private
    //FReadOnly : Boolean;
    FBorderStyle: TBorderStyle;
    FDefItemHeight: integer;
    FSmallImages : TCustomImageList;
    FListItems : TListItems;
    FColumns : TListColumns;
    FViewStyle : TViewStyle;
    FSortType: TSortType;
    FSortColumn : Integer;
    FImageChangeLink : TChangeLink;
    FScrollBars: TScrollStyle;
    FScrolledLeft: integer; // horizontal scrolled pixels (hidden pixels at top)
    FScrolledTop: integer;  // vertical scrolled pixels (hidden pixels at top)
    FSelected: TListItem;   // temp copy of the selected item
    FLastHorzScrollInfo: TScrollInfo;
    FLastVertScrollInfo: TScrollInfo;
    FUpdateCount: integer;
    FOnChange: TLVChangeEvent;
    FOnColumnClick: TLVColumnClickEvent;
    FOnDeletion: TLVDeletedEvent;
    FOnSelectItem: TLVSelectItemEvent;
    FStates: TListViewStates;
    function GetMultiSelect: Boolean;
    function GetSelection: TListItem;
    procedure SetColumns(const AValue: TListColumns);
    procedure SetDefaultItemHeight(AValue: integer);
    procedure SetItems(const AValue : TListItems);
    procedure SetMultiSelect(const AValue: Boolean);
    procedure SetSmallImages(const AValue: TCustomImageList);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetScrolledLeft(AValue: Integer);
    procedure SetScrolledTop(AValue: Integer);
    procedure SetSelection(const AValue: TListItem);
    procedure SetSortColumn(const AValue: Integer);
    procedure SetSortType(const AValue: TSortType);
    procedure SetViewStyle (const Avalue: TViewStyle);
    procedure UpdateScrollbars;
    procedure CNNotify(var AMessage: TLMNotify); message CN_NOTIFY;
    procedure DoUpdate;
  protected
    ParentWindow : TScrolledWindow;
    procedure InitializeWnd; override;
    procedure Loaded; override;
    procedure Change(AItem: TListItem; AChange: Integer); dynamic;
    procedure ColClick(AColumn: TListColumn); dynamic;
    procedure Delete(Item : TListItem);
    procedure DoDeletion(AItem: TListItem); dynamic;
    procedure DoSelectItem(AItem: TListItem; ASelected: Boolean); dynamic;
    procedure InsertItem(Item : TListItem);
    function GetMaxScrolledLeft : Integer;
    function GetMaxScrolledTop : Integer;
    procedure ColumnsChanged; //called by TListColumns
    procedure ItemChanged(Index : Integer);  //called by TListItems
    procedure ItemDeleted(Index : Integer);  //called by TListItems
    procedure ImageChanged(Sender : TObject);
    procedure ItemAdded;  //called by TListItems
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
//    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property Columns: TListColumns read FColumns write SetColumns;
//    property ColumnClick: Boolean read FColumnClick write SetColumnClick default True;
    property DefaultItemHeight: integer read FDefItemHeight write SetDefaultItemHeight;
//    property HideSelection: Boolean read FHideSelection write SetHideSelection default True;
    property Items: TListItems read FListItems write SetItems;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect default False;
//    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
//    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property ScrolledLeft: integer read FScrolledLeft write SetScrolledLeft;
    property ScrolledTop: integer read FScrolledTop write SetScrolledTop;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property SmallImages: TCustomImageList read FSmallImages write SetSmallImages;
    property SortType: TSortType read FSortType write SetSortType;
    property SortColumn: Integer read FSortColumn write SetSortColumn;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
    property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
    property OnDeletion: TLVDeletedEvent read FOnDeletion write FOnDeletion;
    property OnSelectItem: TLVSelectItemEvent read FOnSelectItem write FOnSelectItem;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Selected: TListItem read GetSelection write SetSelection;
  end;

 TListView = class(TCustomListView)
  published
    property Align;
    property Anchors;
//    property BorderStyle;
    property BorderWidth;
    property Color;
    property Columns;
//    property ColumnClick;
//    property Constraints;
    property Enabled;
    property Font;
//    property HideSelection;
    property Items;
    property MultiSelect;
    property PopupMenu;
//    property ReadOnly default False;
//    property RowSelect;
    property ScrollBars;
    property SmallImages;
    property SortColumn;
    property SortType;
    property Visible;
    property ViewStyle;
    property OnMouseMove;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMOuseUp;
    property OnKeyPress;
    property OnKeyUp;
    property OnKeyDown;
    property OnDeletion;
    property OnSelectItem;
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

{ TUpDown }
  TUDAlignButton = (udLeft, udRight);
  TUDOrientation = (udHorizontal, udVertical);
  TUDBtnType = (btNext, btPrev);
  TUDClickEvent = procedure (Sender: TObject; Button: TUDBtnType) of object;
  TUDChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;

  TCustomUpDown = class(TCustomControl)
  private
    MinBtn,
    MaxBtn : TControl;//TSpeedButton's
    BTimer : TTimer;
    BTimerProc : Procedure of Object;
    BTimerBounds : TRect;
    InheritedChangeBounds,
    FArrowKeys: Boolean;
    FAssociate: TWinControl;
    FMin: SmallInt;
    FMax: SmallInt;
    FIncrement: Integer;
    FPosition: SmallInt;
    FThousands: Boolean;
    FWrap: Boolean;
    FOnClick: TUDClickEvent;
    FAlignButton: TUDAlignButton;
    FOrientation: TUDOrientation;
    FOnChanging: TUDChangingEvent;
    procedure SetAssociate(Value: TWinControl);
    function GetPosition: SmallInt;
    procedure SetMin(Value: SmallInt);
    procedure SetMax(Value: SmallInt);
    procedure SetIncrement(Value: Integer);
    procedure SetPosition(Value: SmallInt);
    procedure SetAlignButton(Value: TUDAlignButton);
    procedure SetOrientation(Value: TUDOrientation);
    procedure SetArrowKeys(Value: Boolean);
    procedure SetThousands(Value: Boolean);
    procedure SetWrap(Value: Boolean);
    Procedure BTimerExec(Sender : TObject);
  protected
    OldKeyDown : TKeyEvent;
    Procedure AssociateKeyDown(Sender: TObject; var Key: Word; ShiftState : TShiftState);
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: Integer); Override;
    function CanChange: Boolean; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click(Button: TUDBtnType); dynamic;
    property AlignButton: TUDAlignButton read FAlignButton write SetAlignButton default udRight;
    property ArrowKeys: Boolean read FArrowKeys write SetArrowKeys default True;
    property Associate: TWinControl read FAssociate write SetAssociate;
    property Min: SmallInt read FMin write SetMin;
    property Max: SmallInt read FMax write SetMax default 100;
    property Increment: Integer read FIncrement write SetIncrement default 1;
    property Orientation: TUDOrientation read FOrientation write SetOrientation default udVertical;
    property Position: SmallInt read GetPosition write SetPosition;
    property Thousands: Boolean read FThousands write SetThousands default True;
    property Wrap: Boolean read FWrap write SetWrap;
    property OnChanging: TUDChangingEvent read FOnChanging write FOnChanging;
    property OnClick: TUDClickEvent read FOnClick write FOnClick;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
  end;

  TUpDown = class(TCustomUpDown)
  published
    property AlignButton;
    property Anchors;
    property Associate;
    property ArrowKeys;
    property Enabled;
    property Hint;
    property Min;
    property Max;
    property Increment;
    property Constraints;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Thousands;
    property Visible;
    property Wrap;
    property OnChanging;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
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
    procedure SetAutoSize(const Value: Boolean); Override;
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
    property AutoSize default False;
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


{ TTreeNode }

type
  TCustomTreeView = class;
  TTreeNodes = class;
  TTreeNode = class;

  TNodeState = (nsCut, nsDropHilited, nsFocused, nsSelected, nsMultiSelected,
                nsExpanded, nsHasChildren);
  TNodeStates = set of TNodeState;
  TNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);

  TAddMode = (taAddFirst, taAdd, taInsert);

  TTreeNodeArray = ^TTreeNode;

  ETreeNodeError = class(Exception);
  ETreeViewError = class(ETreeNodeError);

const
  NodeAttachModeNames: array[TNodeAttachMode] of string =
    ('naAdd', 'naAddFirst', 'naAddChild', 'naAddChildFirst', 'naInsert');
  AddModeNames: array[TAddMode] of string =
    ('taAddFirst', 'taAdd', 'taInsert');
  LCLStreamID = -7;

type
  TTVChangingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowChange: Boolean) of object;
  TTVChangedEvent = procedure(Sender: TObject; Node: TTreeNode) of object;
  TTVEditingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowEdit: Boolean) of object;
  TTVEditedEvent = procedure(Sender: TObject; Node: TTreeNode;
    var S: string) of object;
  TTVExpandingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowExpansion: Boolean) of object;
  TTVCollapsingEvent = procedure(Sender: TObject; Node: TTreeNode;
    var AllowCollapse: Boolean) of object;
  TTVExpandedEvent = procedure(Sender: TObject; Node: TTreeNode) of object;
  TTVCompareEvent = procedure(Sender: TObject; Node1, Node2: TTreeNode;
    var Compare: Integer) of object;
  TTVCustomDrawEvent = procedure(Sender: TCustomTreeView; const ARect: TRect;
    var DefaultDraw: Boolean) of object;
  TTVCustomDrawItemEvent = procedure(Sender: TCustomTreeView; Node: TTreeNode;
    State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TTVAdvancedCustomDrawEvent = procedure(Sender: TCustomTreeView;
    const ARect: TRect; Stage: TCustomDrawStage;
    var DefaultDraw: Boolean) of object;
  TTVAdvancedCustomDrawItemEvent = procedure(Sender: TCustomTreeView;
    Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
    var PaintImages, DefaultDraw: Boolean) of object;

  THitTest = (htAbove, htBelow, htNowhere, htOnItem, htOnButton, htOnIcon,
    htOnIndent, htOnLabel, htOnRight, htOnStateIcon, htToLeft, htToRight);
  THitTests = set of THitTest;

  TTreeNodeCompare = function(Node1, Node2: TTreeNode): integer of object;

  PTreeNodeInfo = ^TTreeNodeInfo;
  TTreeNodeInfo = packed record
    ImageIndex: Integer;
    SelectedIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    Data: Pointer;
    Count: Integer;
    Height: integer;
    Expanded: boolean;
    TextLen: integer;
    // here follows the text
  end;

  // this is the delphi node stream record
  PDelphiNodeInfo = ^TDelphiNodeInfo;
  TDelphiNodeInfo = packed record
    ImageIndex: Integer;
    SelectedIndex: Integer;
    StateIndex: Integer;
    OverlayIndex: Integer;
    Data: Pointer;
    Count: Integer;
    Text: string[255];
  end;

  TTreeNode = class(TPersistent)
  private
    FOwner: TTreeNodes;   // the object, which contains all nodes of the tree
    FCapacity: integer;   // size of FItems
    FCount: integer;      // # of first level childs in FItems
    FData: Pointer;       // custom data
    FDeleting: Boolean;
    FHeight: integer;     // height in pixels
    FInTree: Boolean;
    FImageIndex: integer;
    //FItemId: HTreeItem;
    FItems: TTreeNodeArray;  // first level child nodes
    FNextBrother: TTreeNode; // next sibling
    FNextMultiSelected: TTreeNode;
    FOverlayIndex: Integer;
    FParent: TTreeNode;
    FPrevBrother: TTreeNode; // previous sibling
    FPrevMultiSelected: TTreeNode;
    FSelectedIndex: Integer;
    FStateIndex: Integer;
    FStates: TNodeStates;
    FSubTreeCount: integer;// total of all child nodes and self
    FText: string;
    FTop: integer;        // top coordinate
    function AreParentsExpanded: Boolean;
    procedure BindToMultiSelected;
    function CompareCount(CompareMe: Integer): Boolean;
    function DoCanExpand(ExpandIt: Boolean): Boolean;
    procedure DoExpand(ExpandIt: Boolean);
    procedure ExpandItem(ExpandIt: Boolean; Recurse: Boolean);
    function GetAbsoluteIndex: Integer;
    function GetHasChildren: Boolean;
    function GetCount: Integer;
    function GetCut: boolean;
    function GetDropTarget: Boolean;
    function GetExpanded: Boolean;
    function GetFocused: Boolean;
    function GetHeight: integer;
    function GetIndex: Integer;
    function GetItems(AnIndex: Integer): TTreeNode;
    function GetLevel: Integer;
    function GetMultiSelected: Boolean;
    function GetSelected: Boolean;
    function GetState(NodeState: TNodeState): Boolean;
    function GetTreeNodes: TTreeNodes;
    function GetTreeView: TCustomTreeView;
    function GetTop: integer;
    procedure InternalMove(ANode: TTreeNode; AddMode: TAddMode);
    function IsEqual(Node: TTreeNode): Boolean;
    function IsNodeVisible: Boolean;
    procedure ReadData(Stream: TStream; StreamVersion: integer;
      Info: PTreeNodeInfo);
    procedure ReadDelphiData(Stream: TStream; Info: PDelphiNodeInfo);
    procedure SetCut(AValue: Boolean);
    procedure SetData(AValue: Pointer);
    procedure SetDropTarget(AValue: Boolean);
    procedure SetExpanded(AValue: Boolean);
    procedure SetFocused(AValue: Boolean);
    procedure SetHasChildren(AValue: Boolean);
    procedure SetHeight(AValue: integer);
    procedure SetImageIndex(AValue: integer);
    procedure SetItems(AnIndex: Integer; AValue: TTreeNode);
    procedure SetMultiSelected(const AValue: Boolean);
    procedure SetOverlayIndex(AValue: Integer);
    procedure SetSelected(AValue: Boolean);
    procedure SetSelectedIndex(AValue: Integer);
    procedure SetStateIndex(AValue: Integer);
    procedure SetText(const S: string);
    procedure Unbind;
    procedure UnbindFromMultiSelected;
    procedure WriteData(Stream: TStream; Info: PTreeNodeInfo);
    procedure WriteDelphiData(Stream: TStream; Info: PDelphiNodeInfo);
  public
    constructor Create(AnOwner: TTreeNodes);
    function AlphaSort: Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure Collapse(Recurse: Boolean);
    function CustomSort(SortProc: TTreeNodeCompare): Boolean;
    function DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer;
    procedure Delete;
    procedure DeleteChildren;
    destructor Destroy; override;
    function DisplayExpandSignLeft: integer;
    function DisplayExpandSignRect: TRect;
    function DisplayExpandSignRight: integer;
    function DisplayIconLeft: integer;
    function DisplayRect(TextOnly: Boolean): TRect;
    function DisplayStateIconLeft: integer;
    function DisplayTextLeft: integer;
    function DisplayTextRight: integer;
    function EditText: Boolean;
    procedure EndEdit(Cancel: Boolean);
    procedure Expand(Recurse: Boolean);
    procedure ExpandParents;
    function Bottom: integer;
    function BottomExpanded: integer;
    function GetFirstChild: TTreeNode;
    function GetHandle: THandle;
    function GetLastSibling: TTreeNode;
    function GetLastChild: TTreeNode;
    function GetLastSubChild: TTreeNode;
    function GetNext: TTreeNode;
    function GetNextChild(AValue: TTreeNode): TTreeNode;
    function GetNextMultiSelected: TTreeNode;
    function GetNextSibling: TTreeNode;
    function GetNextVisible: TTreeNode;
    function GetPrev: TTreeNode;
    function GetPrevChild(AValue: TTreeNode): TTreeNode;
    function GetPrevMultiSelected: TTreeNode;
    function GetPrevSibling: TTreeNode;
    function GetPrevVisible: TTreeNode;
    function HasAsParent(AValue: TTreeNode): Boolean;
    function IndexOf(AValue: TTreeNode): Integer;
    procedure MakeVisible;
    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); virtual;
    procedure MultiSelectGroup;
    procedure Update;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport(const Prefix: string; Recurse: boolean);
    property AbsoluteIndex: Integer read GetAbsoluteIndex;
    property Count: Integer read GetCount;
    property Cut: Boolean read GetCut write SetCut;
    property Data: Pointer read FData write SetData;
    property Deleting: Boolean read FDeleting;
    property Focused: Boolean read GetFocused write SetFocused;
    property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Handle: THandle read GetHandle;
    property HasChildren: Boolean read GetHasChildren write SetHasChildren;
    property Height: integer read GetHeight write SetHeight;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property Index: Integer read GetIndex;
    property IsVisible: Boolean read IsNodeVisible;
    property Items[ItemIndex: Integer]: TTreeNode read GetItems write SetItems; default;
    //property ItemId: HTreeItem read FItemId;
    property Level: Integer read GetLevel;
    property MultiSelected: Boolean read GetMultiSelected write SetMultiSelected;
    property OverlayIndex: Integer read FOverlayIndex write SetOverlayIndex;
    property Owner: TTreeNodes read FOwner;
    property Parent: TTreeNode read FParent;
    property Selected: Boolean read GetSelected write SetSelected;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property SubTreeCount: integer read FSubTreeCount;
    property StateIndex: Integer read FStateIndex write SetStateIndex;
    property Text: string read FText write SetText;
    property TreeNodes: TTreeNodes read GetTreeNodes;
    property TreeView: TCustomTreeView read GetTreeView;
    property Top: integer read GetTop;
  end;

{ TTreeNodes }

  PNodeCache = ^TNodeCache;
  TNodeCache = record
    CacheNode: TTreeNode;
    CacheIndex: Integer;
  end;

  TTreeNodes = class(TPersistent)
  private
    FCount: integer;
    FFirstMultiSelected: TTreeNode;
    FKeepCollapsedNodes: boolean;
    FNodeCache: TNodeCache;
    FOwner: TCustomTreeView;
    FTopLvlCapacity: integer;
    FTopLvlCount: integer;
    FTopLvlItems: TTreeNodeArray; // root and root siblings
    FUpdateCount: Integer;
    procedure AddedNode(AValue: TTreeNode);
    procedure ClearCache;
    function GetHandle: THandle;
    function GetNodeFromIndex(Index: Integer): TTreeNode;
    function GetTopLvlItems(Index: integer): TTreeNode;
    procedure GrowTopLvlItems;
    function IndexOfTopLvlItem(Node: TTreeNode): integer;
    procedure MoveTopLvlNode(TopLvlFromIndex, TopLvlToIndex: integer;
      Node: TTreeNode);
    procedure ReadData(Stream: TStream);
    procedure ReadExpandedState(Stream: TStream);
    procedure Repaint(ANode: TTreeNode);
    procedure ShrinkTopLvlItems;
    procedure SetTopLvlItems(Index: integer; AValue: TTreeNode);
    procedure WriteData(Stream: TStream);
    procedure WriteExpandedState(Stream: TStream);
  protected
    //function AddItem(Parent, Target: HTreeItem; const Item: TTVItem;
    //  AddMode: TAddMode): HTreeItem;
    function InternalAddObject(Node: TTreeNode; const S: string;
      Data: Pointer; AddMode: TAddMode): TTreeNode;
    procedure DefineProperties(Filer: TFiler); override;
    //function CreateItem(Node: TTreeNode): TTVItem;
    function GetCount: Integer;
    procedure SetItem(Index: Integer; AValue: TTreeNode);
    procedure SetUpdateState(Updating: Boolean);
  public
    constructor Create(AnOwner: TCustomTreeView);
    destructor Destroy; override;
    function Add(SiblingNode: TTreeNode; const S: string): TTreeNode;
    function AddChild(ParentNode: TTreeNode; const S: string): TTreeNode;
    function AddChildFirst(ParentNode: TTreeNode; const S: string): TTreeNode;
    function AddChildObject(ParentNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    function AddChildObjectFirst(ParentNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    function AddFirst(SiblingNode: TTreeNode; const S: string): TTreeNode;
    function AddObject(SiblingNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    function AddObjectFirst(SiblingNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear;
    procedure ClearMultiSelection;
    procedure Delete(Node: TTreeNode);
    procedure EndUpdate;
    function GetFirstNode: TTreeNode;
    //function GetNode(ItemId: HTreeItem): TTreeNode;
    function GetLastNode: TTreeNode; // last top level node
    function GetLastSubNode: TTreeNode; // absolute last node
    function GetLastExpandedSubNode: TTreeNode; // absolute last node
    function Insert(NextNode: TTreeNode; const S: string): TTreeNode;
    function InsertObject(NextNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    function InsertBehind(PrevNode: TTreeNode; const S: string): TTreeNode;
    function InsertObjectBehind(PrevNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    function ConsistencyCheck: integer;
    procedure WriteDebugReport(const Prefix: string; AllNodes: boolean);
    property Count: Integer read GetCount;
    //property Handle: HWND read GetHandle;
    property Items[Index: Integer]: TTreeNode read GetNodeFromIndex; default;
    property KeepCollapsedNodes: boolean
      read FKeepCollapsedNodes write FKeepCollapsedNodes;
    property Owner: TCustomTreeView read FOwner;
    property TopLvlCount: integer read FTopLvlCount;
    property TopLvlItems[Index: integer]: TTreeNode
      read GetTopLvlItems write SetTopLvlItems;
  end;

{ TCustomTreeView }

  TTreeViewState = (
    tvsScrollbarChanged,
    tvsMaxRightNeedsUpdate,
    tvsTopsNeedsUpdate,
    tvsMaxLvlNeedsUpdate,
    tvsTopItemNeedsUpdate,
    tvsBottomItemNeedsUpdate,
    tvsCanvasChanged,
    tvsDragged,
    tvsIsEditing,
    tvsStateChanging,
    tvsManualNotify,
    tvsUpdating,
    tvsMouseCapture,
    tvsWaitForDragging,
    tvsDblClicked,
    tvsTripleClicked,
    tvsQuadClicked
    );
  TTreeViewStates = set of TTreeViewState;

  TTreeViewOption = (
    tvoAutoExpand,
    tvoHideSelection,
    tvoHotTrack,
    tvoRightClickSelect,
    tvoReadOnly,
    tvoShowButtons,
    tvoShowRoot,
    tvoShowLines,
    tvoToolTips,
    tvoRowSelect,
    tvoKeepCollapsedNodes,
    tvoShowSeparators,
    tvoAllowMultiselect
    );
  TTreeViewOptions = set of TTreeViewOption;

  TTreeViewExpandSignType = (tvestPlusMinus, tvestArrow);

  TCustomTreeView = class(TCustomControl)
  private
    FBackgroundColor: TColor;
    FBorderStyle: TBorderStyle;
    FBottomItem: TTreeNode;
    FCanvas: TCanvas;
    FExpandSignType: TTreeViewExpandSignType;
    FExpandSignSize: integer;
    FDefEditProc: Pointer;
    FDefItemHeight: integer;
    FDragImage: TDragImageList;
    FDragNode: TTreeNode;
    FEditHandle: THandle;
    FIndent: integer;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FLastDropTarget: TTreeNode;
    FLastHorzScrollInfo: TScrollInfo;
    FLastVertScrollInfo: TScrollInfo;
    FMaxLvl: integer; // maximum level of all nodes
    FMaxRight: integer; // maximum text width of all nodes (needed for horizontal scrolling)
    //FMemStream: TMemoryStream;
    fMouseDownX: integer;
    fMouseDownY: integer;
    FOnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent;
    FOnChange: TTVChangedEvent;
    FOnChanging: TTVChangingEvent;
    FOnCollapsed: TTVExpandedEvent;
    FOnCollapsing: TTVCollapsingEvent;
    FOnCompare: TTVCompareEvent;
    FOnCustomDraw: TTVCustomDrawEvent;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
    FOnDeletion: TTVExpandedEvent;
    FOnEditing: TTVEditingEvent;
    FOnEdited: TTVEditedEvent;
    FOnExpanded: TTVExpandedEvent;
    FOnExpanding: TTVExpandingEvent;
    FOnGetImageIndex: TTVExpandedEvent;
    FOnGetSelectedIndex: TTVExpandedEvent;
    FOptions: TTreeViewOptions;
    FRClickNode: TTreeNode;
    //FSaveIndex: Integer;
    FSaveItems: TStringList;
    //FSaveTopIndex: Integer;
    FScrollBars: TScrollStyle;
    FScrolledLeft: integer; // horizontal scrolled pixels (hidden pixels at left)
    FScrolledTop: integer;  // vertical scrolled pixels (hidden pixels at top)
    FSelectedColor: TColor;
    FSelectedNode: TTreeNode;
    FSortType: TSortType;
    FStateChangeLink: TChangeLink;
    FStateImages: TCustomImageList;
    FStates: TTreeViewStates;
    FTopItem: TTreeNode;
    FTreeLineColor: TColor;
    FTreeNodes: TTreeNodes;
    FUpdateCount: integer;
    fSeparatorColor: TColor;
    //FWideText: WideString;
    procedure CanvasChanged(Sender: TObject);
    //procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    //procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    //procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMDrag(var AMessage: TCMDrag); message CM_DRAG;
    //procedure CNNotify(var Message: TWMNotify); message CN_NOTIFY;
    procedure EditWndProc(var Message: TLMessage);
    procedure DoDragOver(Source: TDragObject; X, Y: Integer; CanDrop: Boolean);
    function GetAutoExpand: boolean;
    function GetBottomItem: TTreeNode;
    function GetChangeDelay: Integer;
    function GetDropTarget: TTreeNode;
    function GetHideSelection: boolean;
    function GetHotTrack: boolean;
    function GetKeepCollapsedNodes: boolean;
    //function GetNodeFromItem(const Item: TTVItem): TTreeNode;
    function GetReadOnly: boolean;
    function GetRightClickSelect: boolean;
    function GetRowSelect: boolean;
    function GetSelection: TTreeNode;
    function GetShowButtons: boolean;
    function GetShowLines: boolean;
    function GetShowRoot: boolean;
    function GetShowSeparators: boolean;
    function GetToolTips: boolean;
    function GetTopItem: TTreeNode;
    procedure ImageListChange(Sender: TObject);
    procedure OnChangeTimer(Sender: TObject);
    procedure SetAutoExpand(Value: Boolean);
    procedure SetBackgroundColor(Value: TColor);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBottomItem(Value: TTreeNode);
    procedure SetChangeDelay(Value: Integer);
    procedure SetDefaultItemHeight(Value: integer);
    procedure SetExpandSignType(Value: TTreeViewExpandSignType);
    procedure SetDropTarget(Value: TTreeNode);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    //procedure SetImageList(Value: HImageList; Flags: Integer);
    procedure SetIndent(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetKeepCollapsedNodes(Value: Boolean);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRightClickSelect(Value: Boolean);
    procedure SetRowSelect(Value: Boolean);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetScrolledLeft(AValue: integer);
    procedure SetScrolledTop(AValue: integer);
    procedure SetSelectedColor(Value: TColor);
    procedure SetSelection(Value: TTreeNode);
    procedure SetSeparatorColor(const AValue: TColor);
    procedure SetShowButton(Value: Boolean);
    procedure SetShowLines(Value: Boolean);
    procedure SetShowRoot(Value: Boolean);
    procedure SetShowSeparators(Value: Boolean);
    procedure SetSortType(Value: TSortType);
    procedure SetStateImages(Value: TCustomImageList);
    procedure SetToolTips(Value: Boolean);
    procedure SetTreeLineColor(Value: TColor);
    procedure SetTreeNodes(Value: TTreeNodes);
    procedure SetTopItem(Value: TTreeNode);
    procedure UpdateAllTops;
    procedure UpdateBottomItem;
    procedure UpdateMaxLvl;
    procedure UpdateMaxRight;
    procedure UpdateTopItem;
    procedure UpdateScrollbars;
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure WMLButtonDown(var AMessage: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMNotify(var AMessage: TLMNotify); message LM_NOTIFY;
    procedure WMSize(var Msg: TLMSize); message LM_SIZE;
    //procedure WMContextMenu(var Message: TLMContextMenu); message LM_CONTEXTMENU;
    //procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
  protected
    FChangeTimer: TTimer;
    function CanEdit(Node: TTreeNode): Boolean; dynamic;
    function CanChange(Node: TTreeNode): Boolean; dynamic;
    function CanCollapse(Node: TTreeNode): Boolean; dynamic;
    function CanExpand(Node: TTreeNode): Boolean; dynamic;
    procedure Change(Node: TTreeNode); dynamic;
    procedure Collapse(Node: TTreeNode); dynamic;
    function CreateNode: TTreeNode; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CustomDraw(const ARect: TRect;
      Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; virtual;
    procedure Delete(Node: TTreeNode); dynamic;
    procedure DestroyWnd; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoPaint; virtual;
    procedure DoPaintNode(Node: TTreeNode); virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    //procedure Edit(const Item: TTVItem); dynamic;
    procedure EndEditing;
    procedure EnsureNodeIsVisible(ANode: TTreeNode);
    procedure Expand(Node: TTreeNode); dynamic;
    function GetDragImages: TDragImageList; //override;
    procedure GetImageIndex(Node: TTreeNode); virtual;
    function GetMaxLvl: integer;
    function GetMaxScrollLeft: integer;
    function GetMaxScrollTop: integer;
    function GetNodeAtInternalY(Y: Integer): TTreeNode;
    function GetNodeAtY(Y: Integer): TTreeNode;
    function GetNodeDrawAreaWidth: integer;
    function GetNodeDrawAreaHeight: integer;
    procedure GetSelectedIndex(Node: TTreeNode); virtual;
    function IsCustomDrawn(Target: TCustomDrawTarget;
      Stage: TCustomDrawStage): Boolean;
    function IsNodeVisible(ANode: TTreeNode): Boolean;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure SetDragMode(Value: TDragMode); override;
    procedure SetOptions(NewOptions: TTreeViewOptions);
    procedure WndProc(var Message: TLMessage); override;

    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand default False;
    property BorderStyle: TBorderStyle
      read FBorderStyle write SetBorderStyle default bsSingle;
    property ChangeDelay: Integer read GetChangeDelay write SetChangeDelay default 0;
    property HideSelection: Boolean
      read GetHideSelection write SetHideSelection default True;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read fIndent write SetIndent;
    property Items: TTreeNodes read FTreeNodes write SetTreeNodes;
    property OnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent
      read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent
      read FOnAdvancedCustomDrawItem write FOnAdvancedCustomDrawItem;
    property OnChange: TTVChangedEvent read FOnChange write FOnChange;
    property OnChanging: TTVChangingEvent read FOnChanging write FOnChanging;
    property OnCollapsed: TTVExpandedEvent read FOnCollapsed write FOnCollapsed;
    property OnCollapsing: TTVCollapsingEvent read FOnCollapsing write FOnCollapsing;
    property OnCompare: TTVCompareEvent read FOnCompare write FOnCompare;
    property OnCustomDraw: TTVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent
      read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnDeletion: TTVExpandedEvent read FOnDeletion write FOnDeletion;
    property OnEditing: TTVEditingEvent read FOnEditing write FOnEditing;
    property OnEdited: TTVEditedEvent read FOnEdited write FOnEdited;
    property OnExpanded: TTVExpandedEvent read FOnExpanded write FOnExpanded;
    property OnExpanding: TTVExpandingEvent read FOnExpanding write FOnExpanding;
    property OnGetImageIndex: TTVExpandedEvent
      read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent
      read FOnGetSelectedIndex write FOnGetSelectedIndex;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property RightClickSelect: Boolean
      read GetRightClickSelect write SetRightClickSelect default False;
    property RowSelect: Boolean read GetRowSelect write SetRowSelect default False;
    property ScrolledLeft: integer read FScrolledLeft write SetScrolledLeft;
    property ScrolledTop: integer read FScrolledTop write SetScrolledTop;
    property ShowButtons: Boolean read GetShowButtons write SetShowButton default True;
    property ShowLines: Boolean read GetShowLines write SetShowLines default True;
    property ShowRoot: Boolean read GetShowRoot write SetShowRoot default True;
    property ShowSeparators: Boolean read GetShowSeparators write SetShowSeparators default True;
    property SortType: TSortType read FSortType write SetSortType default stNone;
    property StateImages: TCustomImageList read FStateImages write SetStateImages;
    property ToolTips: Boolean read GetToolTips write SetToolTips default True;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    function AlphaSort: Boolean;
    procedure BeginUpdate;
    function CustomSort(SortProc: TTreeNodeCompare): Boolean;
    procedure EndUpdate;
    procedure FullCollapse;
    procedure FullExpand;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetNodeAt(X, Y: Integer): TTreeNode;
    function IsEditing: Boolean;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    function ConsistencyCheck: integer;
    procedure WriteDebugReport(const Prefix: string; AllNodes: boolean);
    property BackgroundColor: TColor
      read FBackgroundColor write SetBackgroundColor;
    property BorderWidth;
    property BottomItem: TTreeNode read GetBottomItem write SetBottomItem;
    property Canvas: TCanvas read FCanvas;
    property DropTarget: TTreeNode read GetDropTarget write SetDropTarget;
    property DefaultItemHeight: integer
      read FDefItemHeight write SetDefaultItemHeight;
    property ExpandSignType: TTreeViewExpandSignType
      read FExpandSignType write SetExpandSignType;
    property KeepCollapsedNodes: boolean
      read GetKeepCollapsedNodes write SetKeepCollapsedNodes;
    property Options: TTreeViewOptions read FOptions write SetOptions;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property Selected: TTreeNode read GetSelection write SetSelection;
    property SelectionColor: TColor read FSelectedColor write SetSelectedColor;
    property SeparatorColor: TColor read fSeparatorColor write SetSeparatorColor;
    property TopItem: TTreeNode read GetTopItem write SetTopItem;
    property TreeLineColor: TColor read FTreeLineColor write FTreeLineColor;
  end;

  TTreeView = class(TCustomTreeView)
  published
    property Align;
    property Anchors;
    property AutoExpand;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Ctl3D;
    property Constraints;
    property DefaultItemHeight;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExpandSignType;
    property Font;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCompare;
    //property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    //property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Options;
    //property OnStartDock;
    property OnStartDrag;
    property Items;
  end;

function InitCommonControl(CC: Integer): Boolean;
procedure CheckCommonControl(CC: Integer);


Implementation

const
  ButtonStates: array[TToolButtonState] of Word = (TBSTATE_CHECKED,
    TBSTATE_PRESSED, TBSTATE_ENABLED, TBSTATE_HIDDEN, TBSTATE_INDETERMINATE,
    TBSTATE_WRAP, TBSTATE_ELLIPSES, TBSTATE_MARKED);

  ButtonStyles: array[TToolButtonStyle] of Word = (TBSTYLE_BUTTON, TBSTYLE_CHECK,
    TBSTYLE_DROPDOWN, TBSTYLE_SEP, TBSTYLE_SEP);

  ScrollBarWidth=0;

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
{$I listcolumns.inc}
{$I listcolumn.inc}
{$I listitem.inc}
{$I listitems.inc}
{$I customlistview.inc}
{$I progressbar.inc}
{$I customupdown.inc}
{$I toolbutton.inc}
{$I toolbar.inc}
{$I trackbar.inc}
{$I treeview.inc}

end.

{ =============================================================================

  $Log$
  Revision 1.56  2002/10/26 11:20:29  lazarus
  MG: broke some interfaces.pp circles

  Revision 1.55  2002/10/25 10:42:07  lazarus
  MG: broke minor circles

  Revision 1.54  2002/10/21 14:40:51  lazarus
  MG: fixes for 1.1

  Revision 1.53  2002/10/20 21:54:02  lazarus
  MG: fixes for 1.1

  Revision 1.52  2002/10/20 19:03:56  lazarus
  AJ: minor fixes for FPC 1.1

  Revision 1.51  2002/10/14 14:29:50  lazarus
  AJ: Improvements to TUpDown; Added TStaticText & GNOME DrawText

  Revision 1.50  2002/10/09 11:46:04  lazarus
  MG: fixed loading TListView from stream

  Revision 1.49  2002/10/01 18:00:02  lazarus
  AJ: Initial TUpDown, minor property additions to improve reading Delphi created forms.

  Revision 1.48  2002/09/14 14:47:41  lazarus
  MG: fixed icons

  Revision 1.47  2002/09/14 10:39:40  lazarus
  MG: added expanding to unit dependencies

  Revision 1.46  2002/09/14 08:38:05  lazarus
  MG: added TListView notification from Vincent

  Revision 1.45  2002/09/13 16:07:20  lazarus
  Reverting statusbar changes.

  Revision 1.43  2002/09/10 10:00:27  lazarus
  MG: TListView now works handleless and SetSelection implemented

  Revision 1.42  2002/09/10 06:49:18  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.41  2002/09/09 19:04:01  lazarus
  MG: started TTreeView dragging

  Revision 1.40  2002/09/09 17:41:18  lazarus
  MG: added multiselection to TTreeView

  Revision 1.39  2002/09/05 13:33:10  lazarus
  MG: set default value for TStatusBar.SimplePanel

  Revision 1.38  2002/09/03 08:07:17  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.37  2002/08/17 15:45:31  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.36  2002/05/28 14:58:29  lazarus
  MG: added scrollbars for TListView

  Revision 1.35  2002/05/20 14:19:03  lazarus
  MG: activated the clientrect bugfixes

  Revision 1.34  2002/05/10 06:05:48  lazarus
  MG: changed license to LGPL

  Revision 1.33  2002/05/06 08:50:36  lazarus
  MG: replaced logo, increased version to 0.8.3a and some clientrectbugfix

  Revision 1.32  2002/04/17 09:15:51  lazarus
  MG: fixes, e.g. method jumping to changed overloaded methods

  Revision 1.31  2002/03/27 00:33:54  lazarus
  MWE:
    * Cleanup in lmessages
    * Added Listview selection and notification events
    + introduced commctrl

  Revision 1.30  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.29  2002/03/24 16:38:00  lazarus
  MWE:
    * Fixed bug on ListItems.Delete

  Revision 1.28  2002/03/23 15:51:17  lazarus
  MWE: Fixed more compatebility issues (Sort, SelectedItem)

  Revision 1.27  2002/03/12 23:55:36  lazarus
  MWE:
    * More delphi compatibility added/updated to TListView
    * Introduced TDebugger.locals
    * Moved breakpoints dialog to debugger dir
    * Changed breakpoints dialog to read from resource

  Revision 1.26  2002/03/11 23:21:14  lazarus
  *** empty log message ***

  Revision 1.25  2002/03/11 23:07:23  lazarus
  MWE:
    * Made TListview more Delphi compatible
    * Did some cleanup

  Revision 1.24  2002/03/08 09:30:30  lazarus
  MG: nicer parameter names

  Revision 1.23  2002/03/04 13:07:21  lazarus
  MG: fixed update bottomitem on wmsize

  Revision 1.22  2002/03/04 07:28:53  lazarus
  MG: find declaration: fixed function in with context

  Revision 1.21  2002/03/02 17:03:19  lazarus
  MG: fixed TTreeView resize update

  Revision 1.20  2002/03/02 13:22:27  lazarus
  MG: fixed find declaration and inheriting class visibility flags

  Revision 1.19  2002/02/28 12:09:10  lazarus
  MG: fixes, code creation policies, keymap categories, menu shortcuts

  Revision 1.18  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.17  2002/01/08 16:02:43  lazarus
  Minor changes to TListView.
  Added TImageList to the IDE
  Shane

  Revision 1.16  2002/01/04 21:25:05  lazarus
  MG: published background and selection color in TTreeView

  Revision 1.15  2002/01/04 21:07:49  lazarus
  MG: added TTreeView

  Revision 1.14  2002/01/04 20:29:04  lazarus
  Added images to TListView.
  Shane

  Revision 1.13  2002/01/03 21:17:08  lazarus
  added column visible and autosize settings.
  Shane

  Revision 1.12  2001/12/31 22:43:00  lazarus
  Added a TViewColumn editor to be used in the object inspector as TViewColumn's property editor.
  Shane

  Revision 1.11  2001/12/21 18:16:59  lazarus
  Added TImage class
  Shane

  Revision 1.10  2001/12/19 21:36:05  lazarus
  Added MultiSelect to TListView
  Shane

  Revision 1.9  2001/12/19 20:28:51  lazarus
  Enabled Alignment of columns in a TListView.
  Shane

  Revision 1.8  2001/12/18 21:10:01  lazarus
  MOre additions for breakpoints dialog
  Added a TSynEditPlugin in SourceEditor to get notified of lines inserted and deleted from the source.
  Shane

  Revision 1.7  2001/12/14 19:51:48  lazarus
  More changes to TListView
  Shane

  Revision 1.6  2001/12/14 18:38:55  lazarus
  Changed code for TListView
  Added a generic Breakpoints dialog
  Shane

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
