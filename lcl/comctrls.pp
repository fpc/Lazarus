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
@author(TCustomProgressBar - Stefan Hille <stoppok@osibisa.ms.sub.org>)
@author(TTrackBar - Stefan Hille <stoppok@osibisa.ms.sub.org>)
@created(1999)
@lastmod(1999)
}
unit ComCtrls;

{$mode objfpc}
{$H+}


interface

uses
  SysUtils, Classes, Math, FPCAdds, LCLStrConsts, LResources, LCLIntf, LCLType,
  LCLProc, AvgLvlTree, LMessages, ImgList, ActnList, GraphType, Graphics, Menus,
  Controls, Forms, StdCtrls, ExtCtrls, ToolWin, CommCtrl, Buttons;

type
  THitTest = (htAbove, htBelow, htNowhere, htOnItem, htOnButton, htOnIcon,
    htOnIndent, htOnLabel, htOnRight, htOnStateIcon, htToLeft, htToRight);
  THitTests = set of THitTest;

  TStatusPanelStyle = (psText, psOwnerDraw);
  TStatusPanelBevel = (pbNone, pbLowered, pbRaised);

  TStatusBar = class;  //forward declaration

  TPanelPart = (
    ppText,    // for text and text alignment
    ppBorder,  // for bevel and style
    ppWidth    // for width
    );
  TPanelParts = set of TPanelPart;

  { TStatusPanel }

  TStatusPanel = class(TCollectionItem)
  private
    FText: string;
    FWidth: Integer;
    FAlignment: TAlignment;
    FBevel: TStatusPanelBevel;
    FParentBiDiMode: Boolean;
    FStyle: TStatusPanelStyle;
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevel(Value: TStatusPanelBevel);
    procedure SetStyle(Value: TStatusPanelStyle);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
  protected
    function GetDisplayName: string; override;
    procedure PanelChanged(const Parts: TPanelParts);
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(aCollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function StatusBar: TStatusBar;
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
    constructor Create(TheStatusBar: TStatusBar);
    function Add: TStatusPanel;
    property Items[Index: Integer]: TStatusPanel read GetItem write SetItem; default;
    property StatusBar: TStatusBar read FStatusBar;
  end;


  { TStatusBar }

  TStatusBar = class(TWinControl)
  private
    FCanvas: TCanvas;
    FHandlePanelCount: integer; // realized panels in the Handle object
    FHandleObjectNeedsUpdate: boolean;
    FHandleUpdatePanelIndex: integer; // which panel in the handle object needs update
    FUpdateLock: integer; // set by BeginUpdate/EndUpdate
    FPanels : TStatusPanels;
    FSimpleText : String;
    FSimplePanel : Boolean;
    FOnHint: TNotifyEvent;
    procedure SetPanels(Value: TStatusPanels);
    procedure SetSimpleText(const Value : String);
    procedure SetSimplePanel(Value : Boolean);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure Loaded; override;
    procedure UpdateHandleObject(PanelIndex: integer); virtual;
    procedure CalculatePreferredSize(
                        var PreferredWidth, PreferredHeight: integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidatePanel(PanelIndex: integer; PanelParts: TPanelParts); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    function UpdatingStatusBar: boolean;
    property Canvas: TCanvas read FCanvas;
  published
    property Action;
    property AutoSize default true;
    property Panels: TStatusPanels read FPanels write SetPanels;
    property SimpleText: String read FSimpleText write SetSimpleText;
    property SimplePanel: Boolean read FSimplePanel write SetSimplePanel default True;
    property Visible default true;
    property Color default clBtnFace;
    property OnDblClick;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMouseDown;
  end;


  { TTabSheet }

  TPageControl = class;

  // TTabPosition is in extctrls.pas
  TTabStyle = (tsTabs, tsButtons, tsFlatButtons);

  { TTabSheet }

  TTabSheet = class(TCustomPage)
  private
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetPageControl: TPageControl;
    function GetTabIndex: Integer;
    procedure SetPageControl(APageControl: TPageControl);
    procedure SetTabIndex(const AValue: Integer);
  protected
    procedure DoHide; dynamic;
    procedure DoShow; dynamic;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property PageControl: TPageControl read GetPageControl write SetPageControl;
    property TabIndex: Integer read GetTabIndex write SetTabIndex;
  published
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Enabled;
    property Height stored False;
    property ImageIndex default 0;
    property Left stored False;
    property OnContextPopup;
    property OnEnter;
    property OnExit;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property PageIndex stored False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible default True;
    property Top stored False;
    property Visible stored False;
    property Width stored False;
  end;


  { TPageControl }

  TPageControl = class(TCustomNotebook)
  private
    //FOnChange: TNotifyEvent;  //changed to use inherited OnPageChanged
    FOnChanging: TTabChangingEvent;
    function GetActivePageIndex: Integer;
    function GetActiveTabSheet: TTabSheet;
    function GetTabIndex: Integer;
    function GetTabSheet(Index: Integer): TTabSheet;
    procedure SetActivePageIndex(const AValue: Integer);
    procedure SetActiveTabSheet(const AValue: TTabSheet);
    procedure SetTabIndex(const AValue: Integer);
  public
    constructor Create(TheOwner: TComponent); override;
    function FindNextPage(CurPage: TTabSheet;
                                GoForward, CheckTabVisible: Boolean): TTabSheet;
    procedure SelectNextPage(GoForward: Boolean);
    procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);
    property ActivePageIndex: Integer read GetActivePageIndex
                                      write SetActivePageIndex;
    property Pages[Index: Integer]: TTabSheet read GetTabSheet;
  published
    property ActivePage: TTabSheet read GetActiveTabSheet write SetActiveTabSheet;
    property Align;
    property Anchors;
    property BorderSpacing;
    //property BiDiMode;
    property Constraints;
    //property DockSite;
    //property DragCursor;
    //property DragKind;
    //property DragMode;
    property Enabled;
    property Font;
    //property HotTrack;
    property Images;
    //property MultiLine;
    //property OwnerDraw;
    //property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    //property RaggedRight;
    //property ScrollOpposite;
    property ShowHint;
    //property Style;
    //property TabHeight;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabOrder;
    property TabPosition;
    property TabStop;
    //property TabWidth;
    property Visible;
    property OnChange: TNotifyEvent read fOnPageChanged write fOnPageChanged;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnContextPopup;
    //property OnDockDrop;
    //property OnDockOver;
    //property OnDragDrop;
    //property OnDragOver;
    //property OnDrawTab;
    //property OnEndDock;
    //property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    //property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    //property OnStartDock;
    //property OnStartDrag;
    //property OnUnDock;
  end;


  TCustomTabControl = class;

  { TTabControlStrings }

  TTabControlStrings = class(TStrings)
  private
    FHotTrack: Boolean;
    FImages: TCustomImageList;
    FMultiLine: Boolean;
    FMultiSelect: Boolean;
    FOwnerDraw: Boolean;
    FRaggedRight: Boolean;
    FScrollOpposite: Boolean;
    FTabControl: TCustomTabControl;
    FTabHeight: Smallint;
    FTabWidth: Smallint;
    FUpdateCount: integer;
  protected
    function GetTabIndex: integer; virtual; abstract;
    procedure SetHotTrack(const AValue: Boolean); virtual;
    procedure SetImages(const AValue: TCustomImageList); virtual;
    procedure SetMultiLine(const AValue: Boolean); virtual;
    procedure SetMultiSelect(const AValue: Boolean); virtual;
    procedure SetOwnerDraw(const AValue: Boolean); virtual;
    procedure SetRaggedRight(const AValue: Boolean); virtual;
    procedure SetScrollOpposite(const AValue: Boolean); virtual;
    procedure SetTabHeight(const AValue: Smallint); virtual;
    procedure SetTabIndex(const AValue: integer); virtual; abstract;
    procedure SetTabWidth(const AValue: Smallint); virtual;
  public
    constructor Create(TheTabControl: TCustomTabControl); virtual;
    function GetHitTestInfoAt(X, Y: Integer): THitTests; virtual;
    function GetSize: integer; virtual; abstract;
    function IndexOfTabAt(X, Y: Integer): Integer; virtual;
    function RowCount: Integer; virtual;
    function TabRect(Index: Integer): TRect; virtual;
    procedure ImageListChange(Sender: TObject); virtual;
    procedure ScrollTabs(Delta: Integer); virtual;
    procedure TabControlBoundsChange; virtual;
    procedure UpdateTabImages; virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    function IsUpdating: boolean; virtual;
  public
    property TabControl: TCustomTabControl read FTabControl;
    property TabIndex: integer read GetTabIndex write SetTabIndex;
    property HotTrack: Boolean read FHotTrack write SetHotTrack;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read FMultiLine write SetMultiLine;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property OwnerDraw: Boolean read FOwnerDraw write SetOwnerDraw;
    property RaggedRight: Boolean read FRaggedRight write SetRaggedRight;
    property ScrollOpposite: Boolean read FScrollOpposite
                                     write SetScrollOpposite;
    property TabHeight: Smallint read FTabHeight write SetTabHeight;
    property TabWidth: Smallint read FTabWidth write SetTabWidth;
  end;
  
  
  { TTabControlNoteBookStrings }

  TTabControlNoteBookStrings = class(TTabControlStrings)
  private
    FNoteBook: TNoteBook;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    function GetTabIndex: integer; override;
    procedure NBChanging(Sender: TObject; var AllowChange: Boolean); virtual;
    procedure NBGetImageIndex(Sender: TObject; TheTabIndex: Integer;
                              var ImageIndex: Integer); virtual;
    procedure NBPageChanged(Sender: TObject); virtual;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetImages(const AValue: TCustomImageList); override;
    procedure SetTabIndex(const AValue: integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetTabHeight(const AValue: Smallint); override;
    procedure SetTabWidth(const AValue: Smallint); override;
  public
    constructor Create(TheTabControl: TCustomTabControl); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    function GetSize: integer; override;
    procedure TabControlBoundsChange; override;
    function IndexOfTabAt(X, Y: Integer): Integer; override;
  public
    property NoteBook: TNoteBook read FNoteBook;
  end;


  { TCustomTabControl }
  
  TDrawTabEvent = procedure(Control: TCustomTabControl; TabIndex: Integer;
    const Rect: TRect; Active: Boolean) of object;

  TCustomTabControl = class(TCustomControl)
  private
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FOnChange: TNotifyEvent;
    FOnChangeNeeded: Boolean;
    FOnChanging: TTabChangingEvent;
    FOnDrawTab: TDrawTabEvent;
    FOnGetImageIndex: TTabGetImageEvent;
    FStyle: TTabStyle;
    FTabControlCreating: Boolean;
    FTabPosition: TTabPosition;
    FTabs: TStrings;
    function GetDisplayRect: TRect;
    function GetHotTrack: Boolean;
    function GetMultiLine: Boolean;
    function GetMultiSelect: Boolean;
    function GetOwnerDraw: Boolean;
    function GetRaggedRight: Boolean;
    function GetScrollOpposite: Boolean;
    function GetTabHeight: Smallint;
    function GetTabIndex: Integer;
    function GetTabWidth: Smallint;
    procedure SetHotTrack(const AValue: Boolean);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetMultiLine(const AValue: Boolean);
    procedure SetMultiSelect(const AValue: Boolean);
    procedure SetOwnerDraw(const AValue: Boolean);
    procedure SetRaggedRight(const AValue: Boolean);
    procedure SetScrollOpposite(const AValue: Boolean);
    procedure SetStyle(const AValue: TTabStyle);
    procedure SetTabHeight(const AValue: Smallint);
    procedure SetTabPosition(const AValue: TTabPosition);
    procedure SetTabs(const AValue: TStrings);
    procedure SetTabWidth(const AValue: Smallint);
  protected
    function CanChange: Boolean; dynamic;
    function CanShowTab(ATabIndex: Integer): Boolean; virtual;
    procedure Change; dynamic;
    procedure DrawTab(ATabIndex: Integer; const Rect: TRect; Active: Boolean); virtual;
    function GetImageIndex(ATabIndex: Integer): Integer; virtual;
    procedure Loaded; override;
    procedure CreateWnd; override;
    procedure DestroyHandle; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetTabIndex(Value: Integer); virtual;
    procedure UpdateTabImages;
    procedure ImageListChange(Sender: TObject);
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure Paint; override;
    function GetDisplayRectWithBorder: TRect; virtual;
    procedure AdjustClientRect(var ARect: TRect); override;
  protected
    property DisplayRect: TRect read GetDisplayRect;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property MultiLine: Boolean read GetMultiLine write SetMultiLine default False;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TTabChangingEvent read FOnChanging write FOnChanging;
    property OnDrawTab: TDrawTabEvent read FOnDrawTab write FOnDrawTab;
    property OnGetImageIndex: TTabGetImageEvent read FOnGetImageIndex
                                                write FOnGetImageIndex;
    property OwnerDraw: Boolean read GetOwnerDraw write SetOwnerDraw default False;
    property RaggedRight: Boolean read GetRaggedRight write SetRaggedRight default False;
    property ScrollOpposite: Boolean read GetScrollOpposite
                                     write SetScrollOpposite default False;
    property Style: TTabStyle read FStyle write SetStyle default tsTabs;
    property TabHeight: Smallint read GetTabHeight write SetTabHeight default 0;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
                                       default tpTop;
    property Tabs: TStrings read FTabs write SetTabs;
    property TabWidth: Smallint read GetTabWidth write SetTabWidth default 0;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOfTabAt(X, Y: Integer): Integer;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function TabRect(Index: Integer): TRect;
    function RowCount: Integer;
    procedure ScrollTabs(Delta: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: boolean;
  public
    property TabStop default True;
  end;


  { TTabControl }

  TTabControl = class(TCustomTabControl)
  public
    property DisplayRect;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HotTrack;
    property Images;
    property MultiLine;
    property MultiSelect;
    property OnChange;
    property OnChangeBounds;
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OwnerDraw;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RaggedRight;
    property ScrollOpposite;
    property ShowHint;
    property Style;
    property TabHeight;
    property TabIndex;
    property TabOrder;
    property TabPosition;
    property Tabs;
    property TabStop;
    property TabWidth;
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
  
  TListItemState = (lisCut, lisDropTarget, lisFocused, lisSelected);
  TListItemStates = set of TListItemState;
  
  TListItemFlag = (lifDestroying, lifCreated);
  TListItemFlags = set of TListItemFlag;
  
  TListItem = class(TPersistent)
  private
    FOwner: TListItems;
    FFlags: TListItemFlags;
    FSubItems: TStrings;
    FCaption: String;
    FData: Pointer;
    FImageIndex: Integer;
    FStates: TListItemStates;
    function GetState(const ALisOrd: Integer): Boolean;
    function GetIndex: Integer;
    function GetSubItemImages(const AIndex: Integer): Integer;
    function GetSubItems: TStrings;

    function IntfUpdateAllowed: Boolean;
    procedure IntfUpdateText;
    procedure IntfUpdateImages;

    procedure SetState(const ALisOrd: Integer; const AIsSet: Boolean);
    procedure SetData(const AValue: Pointer);
    procedure SetImageIndex(const AValue: Integer);
    procedure SetCaption(const AValue : String);
    procedure SetSubItemImages(const AIndex, AValue: Integer);
    procedure SetSubItems(const AValue: TStrings);
  protected
    function IsEqual(const AItem: TListItem): Boolean;
  public
    procedure Assign(ASource: TPersistent); override;

    constructor Create(AOwner : TListItems);
    destructor Destroy; override;
    procedure Delete;
    procedure MakeVisible(PartialOK: Boolean);

    property Caption : String read FCaption write SetCaption;
    property Cut: Boolean index Ord(lisCut) read GetState write SetState;
    property Data: Pointer read FData write SetData;
    property DropTarget: Boolean index Ord(lisDropTarget) read GetState write SetState;
    property Focused: Boolean index Ord(lisFocused) read GetState write SetState;
    property Index: Integer read GetIndex;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Owner: TListItems read FOwner;
    property Selected: Boolean index Ord(lisSelected) read GetState write SetState;
    property SubItems: TStrings read GetSubItems write SetSubItems;
    property SubItemImages[const AIndex: Integer]: Integer
      read GetSubItemImages write SetSubItemImages;
  end;


  { TListItems }
  {
    Listitems have a build in cache of the last accessed item.
    This will speed up interface updates since Item.Index is 
    often used for the same item updating more properties.
    If FCacheIndex = -1 then the cache is not valid.
  }

  TListItems = class(TPersistent)
  private
    FOwner: TCustomListView;
    FItems: TList;        
    FCacheIndex: Integer;  // Caches the last used item 
    FCacheItem: TListItem; //
    procedure IntfCreateCacheItem;
    function IntfUpdateAllowed: Boolean;
    procedure ItemDestroying(const AItem: TListItem); //called by TListItem when freed
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount : Integer;
    function GetItem(const AIndex: Integer): TListItem;
    function IndexOf(const AItem: TListItem): Integer;
    procedure IntfCreateItems;
    procedure SetItem(const AIndex: Integer; const AValue: TListItem);
  public
    function Add: TListItem;
    procedure AddItem(AItem: TListItem);
    procedure Clear;
    constructor Create(AOwner : TCustomListView);
    destructor Destroy; override;
    procedure Delete(const AIndex : Integer);
    function FindCaption(StartIndex: Integer; Value: string;
                     Partial, Inclusive, Wrap: Boolean;
                     PartStart: Boolean{$IFNDEF 1_0} = True{$ENDIF}): TListItem;
    function FindData(const AData: Pointer): TListItem;
    function Insert(const AIndex: Integer) : TListItem;
    procedure InsertItem(AItem: TListItem; const AIndex: Integer);
    property Count: Integer read GetCount;
    property Item[const AIndex: Integer]: TListItem read GetItem write SetItem; default;
    property Owner : TCustomListView read FOwner;
  end;


  { TListColumn }

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
    FImageIndex: Integer;
    FTag: Integer;
    function GetWidth: TWidth;
    procedure IntfCreateColumn;
    function IntfUpdateAllowed: Boolean;
    procedure SetVisible(const AValue: Boolean);
    procedure SetAutoSize(const AValue: Boolean);
    procedure SetMinWidth(const AValue: TWidth);
    procedure SetMaxWidth(const AValue: TWidth);
    procedure SetWidth(const AValue: TWidth);
    procedure SetCaption(const AValue: String);
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetImageIndex(const AValue: TImageIndex);
  protected
    procedure SetIndex(AValue: Integer); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(ASource: TPersistent); override;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Caption: string read FCaption write SetCaption;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property MaxWidth: TWidth read FMaxWidth write SetMaxWidth default 0;
    property MinWidth: TWidth read FMinWidth write SetMinWidth default 0;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible default true;
    property Width: TWidth read GetWidth write SetWidth default 50;
  end;


  { TListColumns }

  TListColumns = class(TCollection)
  private
    FOwner: TCustomListView;
    FUpdateCount: integer;
    FItemNeedsUpdate: TCollectionItem;
    FNeedsUpdate: boolean;
    function GetItem(const AIndex: Integer): TListColumn;
    procedure IntfCreateColumns;
    procedure SetItem(const AIndex: Integer; const AValue: TListColumn);
  protected
  public
    constructor Create(TheOwner: TCustomListView);
    destructor Destroy; override;
    function Add: TListColumn;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    property Owner: TCustomListView read FOwner;
    property Items[const AIndex: Integer]: TListColumn
                                            read GetItem write SetItem; default;
    procedure Assign(Source: TPersistent); override;
  end;


  { TCustomListView }

  TItemChange = (ctText, ctImage, ctState);
  TViewStyle = (vsIcon, vsSmallIcon, vsList, vsReport);

  TLVChangeEvent = procedure(Sender: TObject; Item: TListItem;
                             Change: TItemChange) of object;
  TLVColumnClickEvent = procedure(Sender: TObject;
                                  Column: TListColumn) of object;
  TLVColumnRClickEvent = procedure(Sender: TObject; Column: TListColumn;
                                   Point: TPoint) of object;
  TLVCompareEvent = procedure(Sender: TObject; Item1, Item2: TListItem;
                               Data: Integer; var Compare: Integer) of object;
  TLVDeletedEvent = procedure(Sender: TObject; Item: TListItem) of object;
  TLVInsertEvent = TLVDeletedEvent;
  TLVSelectItemEvent = procedure(Sender: TObject; Item: TListItem;
                                 Selected: Boolean) of object;

  TListViewProperty = (
    lvpAutoArrange,
    lvpCheckboxes,
    lvpColumnClick,
    lvpFlatScrollBars,
    lvpFullDrag,
    lvpGridLines,
    lvpHideSelection,
    lvpHotTrack,
    lvpMultiSelect,
    lvpOwnerDraw,
    lvpReadOnly,
    lvpRowSelect,
    lvpShowColumnHeaders,
    lvpShowWorkAreas,
    lvpWrapText
  );
  TListViewProperties = set of TListViewProperty;

  TListViewImageList = (lvilSmall, lvilLarge, lvilState);
  
  TListHotTrackStyle = (htHandPoint, htUnderlineCold, htUnderlineHot);
  TListHotTrackStyles = set of TListHotTrackStyle;

  { TCustomListView }

  TCustomListView = class(TWinControl)
  private
    FAllocBy: Integer;
    FCanvas: TCanvas;
    FDefaultItemHeight: integer;
    FHotTrackStyles: TListHotTrackStyles;
//    FIconOptions: TIconOptions;
    FOwnerData: Boolean;
    FListItems: TListItems;
    FColumns: TListColumns;
    FImages: array[TListViewImageList] of TCustomImageList;

    FViewStyle: TViewStyle;
    FSortType: TSortType;
    FSortColumn: Integer;
    FImageChangeLink : TChangeLink;
    FScrollBars: TScrollStyle;
    FScrolledLeft: integer; // horizontal scrolled pixels (hidden pixels at top)
    FScrolledTop: integer;  // vertical scrolled pixels (hidden pixels at top)
    FSelected: TListItem;   // temp copy of the selected item
    FFocused: TListItem;    // temp copy of the focused item
    FHoverTime: Integer;    // temp copy of the hover time (the time a mouse must be over a item to auto select)
    FLastHorzScrollInfo: TScrollInfo;
    FLastVertScrollInfo: TScrollInfo;
    FUpdateCount: integer;
    FOnChange: TLVChangeEvent;
    FOnColumnClick: TLVColumnClickEvent;
    FOnCompare: TLVCompareEvent;
    FOnDeletion: TLVDeletedEvent;
    FOnInsert: TLVInsertEvent;
    FOnSelectItem: TLVSelectItemEvent;
    FProperties: TListViewProperties;
//    FWorkAreas: TWorkAreas;
    function GetBoundingRect: TRect;
    function GetColumnFromIndex(AIndex: Integer): TListColumn;
    function GetDropTarget: TListItem;
    function GetFocused: TListItem;
    function GetImageList(const ALvilOrd: Integer): TCustomImageList;
    function GetHoverTime: Integer;
    function GetProperty(const ALvpOrd: Integer): Boolean;
    function GetSelCount: Integer;
    function GetSelection: TListItem;
    function GetTopItem: TListItem;
    function GetViewOrigin: TPoint;
    function GetVisibleRowCount: Integer;

    procedure SetAllocBy(const AValue: Integer);
    procedure SetColumns(const AValue: TListColumns);
    procedure SetDefaultItemHeight(AValue: Integer);
    procedure SetDropTarget(const AValue: TListItem);
    procedure SetFocused(const AValue: TListItem);
    procedure SetHotTrackStyles(const AValue: TListHotTrackStyles);
    procedure SetHoverTime(const AValue: Integer);
//    procedure SetIconOptions(const AValue: TIconOptions);
    procedure SetImageList(const ALvilOrd: Integer; const AValue: TCustomImageList);
    procedure SetItems(const AValue : TListItems);
    procedure SetItemVisible(const AValue: TListItem; const APartialOK: Boolean);
    procedure SetOwnerData(const AValue: Boolean);
    procedure SetProperty(const ALvpOrd: Integer; const AIsSet: Boolean);
    procedure SetScrollBars(const AValue: TScrollStyle);
    procedure SetScrolledLeft(AValue: Integer);
    procedure SetScrolledTop(AValue: Integer);
    procedure SetSelection(const AValue: TListItem);
    procedure SetSortColumn(const AValue: Integer);
    procedure SetSortType(const AValue: TSortType);
    procedure SetViewStyle(const Avalue: TViewStyle);
    procedure Sort;
    procedure UpdateScrollbars;
    procedure CNNotify(var AMessage: TLMNotify); message CN_NOTIFY;
  protected
    //called by TListItems
    procedure ItemDeleted(const AItem: TListItem);
    procedure ItemInserted(const AItem: TListItem);

  protected
    procedure InitializeWnd; override;
    procedure Change(AItem: TListItem; AChange: Integer); dynamic;
    procedure ColClick(AColumn: TListColumn); dynamic;

    procedure Delete(Item : TListItem);
    procedure DoDeletion(AItem: TListItem); dynamic;
    procedure DoInsert(AItem: TListItem); dynamic;
    procedure DoSelectItem(AItem: TListItem; ASelected: Boolean); dynamic;
    procedure InsertItem(Item : TListItem);
    function GetMaxScrolledLeft : Integer;
    function GetMaxScrolledTop : Integer;
    procedure ImageChanged(Sender : TObject);
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
  protected
    property AllocBy: Integer read FAllocBy write SetAllocBy default 0;
    property BorderStyle default bsSingle;
    property Columns: TListColumns read FColumns write SetColumns;
    property ColumnClick: Boolean index Ord(lvpColumnClick) read GetProperty write SetProperty default True;
    property DefaultItemHeight: integer read FDefaultItemHeight write SetDefaultItemHeight;
    property HideSelection: Boolean index Ord(lvpHideSelection) read GetProperty write SetProperty default True;
    property HoverTime: Integer read GetHoverTime write SetHoverTime default -1;
//    property IconOptions: TIconOptions read FIconOptions write SetIconOptions;
    property Items: TListItems read FListItems write SetItems;
    property LargeImages: TCustomImageList index Ord(lvilLarge) read GetImageList write SetImageList;
    property MultiSelect: Boolean index Ord(lvpMultiselect) read GetProperty write SetProperty default False;
    property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
    property OwnerDraw: Boolean index Ord(lvpOwnerDraw) read GetProperty write SetProperty default False;
    property ReadOnly: Boolean index Ord(lvpReadOnly) read GetProperty write SetProperty default False;
    property ScrolledLeft: integer read FScrolledLeft write SetScrolledLeft;
    property ScrolledTop: integer read FScrolledTop write SetScrolledTop;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property ShowColumnHeaders: Boolean index Ord(lvpShowColumnHeaders) read GetProperty write SetProperty default True;
    property ShowWorkAreas: Boolean index Ord(lvpShowWorkAreas) read GetProperty write SetProperty default False;
    property SmallImages: TCustomImageList index Ord(lvilSmall) read GetImageList write SetImageList;
    property SortType: TSortType read FSortType write SetSortType;
    property SortColumn: Integer read FSortColumn write SetSortColumn;
    property StateImages: TCustomImageList index Ord(lvilState) read GetImageList write SetImageList;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
    property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
    property OnCompare: TLVCompareEvent read FOnCompare write FOnCompare;
    property OnDeletion: TLVDeletedEvent read FOnDeletion write FOnDeletion;
    property OnInsert: TLVInsertEvent read FOnInsert write FOnInsert;
    property OnSelectItem: TLVSelectItemEvent read FOnSelectItem write FOnSelectItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function FindCaption(StartIndex: Integer; Value: string; Partial, Inclusive, Wrap: Boolean; PartStart: Boolean = True): TListItem;
    property BoundingRect: TRect read GetBoundingRect;
    property Canvas: TCanvas read FCanvas;
    property Checkboxes: Boolean index Ord(lvpCheckboxes) read GetProperty write SetProperty default False;
    property Column[AIndex: Integer]: TListColumn read GetColumnFromIndex;
    property DropTarget: TListItem read GetDropTarget write SetDropTarget;
    property FlatScrollBars: Boolean index Ord(lvpFlatScrollBars) read GetProperty write SetProperty default False;
    property FullDrag: Boolean index Ord(lvpFullDrag) read GetProperty write SetProperty default False;
    property GridLines: Boolean index Ord(lvpGridLines) read GetProperty write SetProperty default False;
    property HotTrack: Boolean index Ord(lvpHotTrack) read GetProperty write SetProperty default False;
    property HotTrackStyles: TListHotTrackStyles read FHotTrackStyles write SetHotTrackStyles default [];
    property ItemFocused: TListItem read GetFocused write SetFocused;
    property RowSelect: Boolean index Ord(lvpRowSelect) read GetProperty write SetProperty default False;
    property SelCount: Integer read GetSelCount;
    property Selected: TListItem read GetSelection write SetSelection;
    property TabStop default true;
    property TopItem: TListItem read GetTopItem;
    property ViewOrigin: TPoint read GetViewOrigin;
    property VisibleRowCount: Integer read GetVisibleRowCount;
//    property WorkAreas: TWorkAreas read FWorkAreas;
  end;


  { TListView }

  TListView = class(TCustomListView)
  published
    property Align;
//    property AllocBy;
    property Anchors;
    property BorderSpacing;
//    property BorderStyle;
    property BorderWidth;
//    property Checkboxes;
    property Color default clWindow;
    property Columns;
    property ColumnClick;
    property Constraints;
//    property DefaultItemHeight;
//    property DropTarget;
    property Enabled;
//    property FlatScrollBars;
    property Font;
//    property FullDrag;
//    property GridLines;
//    property HideSelection;
//    property HotTrack;
//    property HotTrackStyles;
//    property HoverTime;
    property Items;
    property LargeImages;
    property MultiSelect;
//    property OwnerData;
//    property OwnerDraw;
    property PopupMenu;
//    property ReadOnly default False;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
//    property ShowWorkAreas;
    property SmallImages;
    property SortColumn;
    property SortType;
    property StateImages;
    property TabStop;
    property Visible;
    property ViewStyle;
    property OnMouseMove;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyPress;
    property OnKeyUp;
    property OnKeyDown;
    property OnDeletion;
    property OnSelectItem;
  end;

  TProgressBarOrientation = (pbHorizontal, pbVertical, pbRightToLeft, pbTopDown);

  { TCustomProgressBar }
  {
    @abstract(Simple progressbar.)
    Introduced by Author Name <stoppok@osibisa.ms.sub.org>
    Currently maintained by Maintainer Name <stoppok@osibisa.ms.sub.org>
  }
  TCustomProgressBar = class(TWinControl)
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
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure StepIt;
    procedure StepBy(Delta: Integer);
  public
    property Max: Integer read GetMax write SetMax;
    property Min: Integer read GetMin write SetMin;
    property Orientation: TProgressBarOrientation read FOrientation write SetOrientation default pbHorizontal;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Smooth : boolean read FSmooth write SetSmooth default false;
    property Step: Integer read FStep write SetStep default 10;
    property BarShowText : boolean read FBarShowText write SetBarShowText;
  end;
 
 
  { TProgressBar }
 
  TProgressBar = class(TCustomProgressBar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Hint;
    property Max;
    property Min;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property Orientation;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ShowHint;
    property Smooth;
    property Step;
    property TabOrder;
    property TabStop;
    property Visible;
    property BarShowText;
  end;
 

  { TUpDown }
  
  TUDAlignButton = (udLeft, udRight, udTop, udBottom);
  TUDOrientation = (udHorizontal, udVertical);
  TUDBtnType = (btNext, btPrev);
  TUDClickEvent = procedure (Sender: TObject; Button: TUDBtnType) of object;
  TUDChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;

  TCustomUpDown = class(TCustomControl)
  private
    MinBtn,
    MaxBtn : TControl;//TSpeedButton's
    BTimerProc : Procedure of Object;
    BTimerBounds : TRect;
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
    function GetPosition: SmallInt;
    Procedure BTimerExec(Sender : TObject);
    procedure SetAlignButton(Value: TUDAlignButton);
    procedure SetArrowKeys(Value: Boolean);
    procedure SetAssociate(Value: TWinControl);
    procedure SetIncrement(Value: Integer);
    procedure SetMax(Value: SmallInt);
    procedure SetMin(Value: SmallInt);
    procedure SetOrientation(Value: TUDOrientation);
    procedure SetPosition(Value: SmallInt);
    procedure SetThousands(Value: Boolean);
    procedure SetWrap(Value: Boolean);
    procedure UpdateAlignButtonPos;
    procedure UpdateOrientation;
    procedure UpdateUpDownPositionText;
  protected
    OldKeyDown : TKeyEvent;
    Procedure AssociateKeyDown(Sender: TObject; var Key: Word; ShiftState : TShiftState);
    procedure OnAssociateChangeBounds(Sender: TObject);
    procedure DoOnResize; override;
    function CanChange: Boolean; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click(Button: TUDBtnType); dynamic; overload;
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
    property BorderSpacing;
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


  { TToolButton }

const
  CN_DROPDOWNCLOSED = LM_USER + $1000;

type
  TToolButtonStyle = (
    tbsButton,    // button (can be clicked)
    tbsCheck,     // check item (click to toggle state, can be grouped)
    tbsDropDown,  // button with dropdown button to show a popup menu
    tbsSeparator, // space holder
    tbsDivider    // space holder with line
    );
    
  TToolButtonFlag = (
    tbfPressed // set while mouse is pressed on button
    );
  TToolButtonFlags = set of TToolButtonFlag;

  { TToolButtonActionLink }

  TToolButtonActionLink = class(TControlActionLink)
  protected
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsImageIndexLinked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  end;

  TToolButtonActionLinkClass = class of TToolButtonActionLink;
  
  TToolBar = class;

  TToolButton = class(TCustomControl)
  private
    FAllowAllUp: Boolean;
    FDown: Boolean;
    FDropdownMenu: TPopupMenu;
    FGrouped: Boolean;
    FImageIndex: Integer;
    FIndeterminate: Boolean;
    FLastButtonDrawFlags: Integer;
    FMarked: Boolean;
    FMenuItem: TMenuItem;
    FMouseInControl: boolean;
    FStyle: TToolButtonStyle;
    FToolButtonFlags: TToolButtonFlags;
    FUpdateCount: Integer;
    FWrap: Boolean;
    procedure GetGroupBounds(var StartIndex, EndIndex: integer);
    function GetIndex: Integer;
    function IsCheckedStored: Boolean;
    function IsImageIndexStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetAutoSize(const Value: Boolean); Override;
    procedure SetDown(Value: Boolean);
    procedure SetDropdownMenu(Value: TPopupMenu);
    procedure SetGrouped(Value: Boolean);
    procedure SetImageIndex(Value: Integer);
    procedure SetIndeterminate(Value: Boolean);
    procedure SetMarked(Value: Boolean);
    procedure SetMenuItem(Value: TMenuItem);
    procedure SetStyle(Value: TToolButtonStyle);
    procedure SetWrap(Value: Boolean);
    procedure SetMouseInControl(NewMouseInControl: Boolean);
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
  protected
    FToolBar: TToolBar;
    procedure CopyPropertiesFromMenuItem(const Value: TMenuItem);
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure RefreshControl; virtual;
    procedure SetToolBar(NewToolBar: TToolBar);
    procedure UpdateControl; virtual;
    function GetButtonDrawFlags: integer; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateVisibleToolbar;
    function GroupAllUpAllowed: boolean;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
  public
    constructor Create(TheOwner: TComponent); override;
    function CheckMenuDropdown: Boolean; dynamic;
    procedure GetCurrentIcon(var ImageList: TCustomImageList;
                             var TheIndex: integer); virtual;
    property Index: Integer read GetIndex;
  published
    property Action;
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
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    //property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    //property OnStartDock;
    property OnStartDrag;
  end;


  { TToolBar }
  
  TToolBarFlag = (
    tbfUpdateVisibleBarNeeded,
    tbfPlacingControls
    );
  
  TToolBarFlags = set of TToolBarFlag;
  
  TToolBar = class(TToolWindow)
  private
    FButtonHeight: Integer;
    FRealizedButtonHeight: integer;
    FButtons: TList;
    FButtonWidth: Integer;
    FDisabledImageChangeLink: TChangeLink;
    FDisabledImages: TCustomImageList;
    FDropDownWidth: integer;
    FDropDownButton: TToolButton;
    FFlat: Boolean;
    FHotImageChangeLink: TChangeLink;
    FHotImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FIndent: Integer;
    FList: Boolean;
    FNewStyle: Boolean;
    FRowCount: integer;
    FShowCaptions: Boolean;
    FCurrentMenu: TPopupMenu;
    FCurrentMenuAutoFree: boolean;
    FSrcMenu: TMenu;
    FSrcMenuItem: TMenuItem;
    FToolBarFlags: TToolBarFlags;
    FTransparent: Boolean;
    FUpdateCount: Integer;
    FWrapable: Boolean;
    function GetButton(Index: Integer): TToolButton;
    function GetButtonCount: Integer;
    procedure SetButtonHeight(const AValue: Integer);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetDisabledImages(const AValue: TCustomImageList);
    procedure SetFlat(const AValue: Boolean);
    procedure SetHotImages(const AValue: TCustomImageList);
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetIndent(const AValue: Integer);
    procedure SetList(const AValue: Boolean);
    procedure SetShowCaptions(const AValue: Boolean);
    procedure SetTransparent(const AValue: Boolean);
    procedure SetWrapable(const AValue: Boolean);
    procedure ToolButtonDown(AButton: TToolButton; NewDown: Boolean);
    procedure ImageListChange(Sender: TObject);
    procedure DisabledImageListChange(Sender: TObject);
    procedure HotImageListChange(Sender: TObject);
    procedure UpdateVisibleBar;
    procedure OnTemporaryPopupMenuClose(Sender: TObject);
    procedure MoveSubMenuItems(SrcMenuItem, DestMenuItem: TMenuItem);
    procedure AddButton(Button: TToolButton);
    procedure RemoveButton(Button: TToolButton);
  protected
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    function CheckMenuDropdown(Button: TToolButton): Boolean; dynamic;
    procedure ClickButton(Button: TToolButton); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure ControlsAligned; override;
    function FindButtonFromAccel(Accel: Word): TToolButton;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RepositionButton(Index: Integer);
    procedure RepositionButtons(Index: Integer);
    function WrapButtons(var NewWidth, NewHeight: Integer): Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: Boolean); override;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure Paint; override;
    procedure SetButtonSize(NewButtonWidth, NewButtonHeight: integer);
  public
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TToolButton read GetButton;
    property ButtonList: TList read FButtons;
    property RowCount: Integer read FRowCount;
  published
    property Align default alTop;
    property AutoSize;
    property BorderSpacing;
    property BorderWidth;
    property ButtonHeight: Integer read FButtonHeight write SetButtonHeight default 22;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth default 23;
    property Caption;
    property ChildSizing;
    property Constraints;
    property Color;
    property Ctl3D;
    //property Customizable: Boolean read FCustomizable write SetCustomizable default False;
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
    //property HideClippedButtons: Boolean read FHideClippedButtons write SetHideClippedButtons default False;
    property HotImages: TCustomImageList read FHotImages write SetHotImages;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read FIndent write SetIndent default 1;
    property List: Boolean read FList write SetList default False;
    //property Menu: TMainMenu read FMenu write SetMenu;
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
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;
    property OnChangeBounds;
    property OnStartDrag;
  end;
  

  { TCustomTrackBar }

  TTrackBarOrientation = (trHorizontal, trVertical);
  TTickMark = (tmBottomRight, tmTopLeft, tmBoth);
  TTickStyle = (tsNone, tsAuto, tsManual);
  TTrackBarScalePos = (trLeft, trRight, trTop, trBottom);

  TCustomTrackBar = class(TWinControl)
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
    FShowScale : boolean;
    FScalePos : TTrackBarScalePos;
    FScaleDigits : integer;
    FOnChange: TNotifyEvent;
    procedure SetFrequency(Value: Integer);
    procedure SetLineSize(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetOrientation(Value: TTrackBarOrientation);
    procedure SetPageSize(Value: Integer);
    procedure SetParams(APosition, AMin, AMax: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetScalePos(Value: TTrackBarScalePos);
    procedure SetShowScale(Value: boolean);
    procedure SetTickMarks(Value: TTickMark);
    procedure SetTickStyle(Value: TTickStyle);
    procedure UpdateSelection;
  protected
    procedure ApplyChanges;
    procedure DoChange(var msg); message LM_CHANGED;
    procedure InitializeWnd; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTick(Value: Integer);
  published
    property Frequency: Integer read FFrequency write SetFrequency;
    property LineSize: Integer read FLineSize write SetLineSize default 1;
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation;
    property PageSize: Integer read FPageSize write SetPageSize default 2;
    property Position: Integer read FPosition write SetPosition;
    property ScalePos: TTrackBarScalePos read FScalePos write SetScalePos;
    property ShowScale: boolean read FShowScale write SetShowScale;
    property TabStop default True;
    property TickMarks: TTickMark read FTickMarks write SetTickMarks default tmBottomRight;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle default tsAuto;
  end;
  
  
  { TTrackBar }
  
  TTrackBar = class(TCustomTrackBar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Frequency;
    property Hint;
    property LineSize;
    property Max;
    property Min;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
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
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnStartDrag;
    property Orientation;
    property PageSize;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property Position;
    property ScalePos;
    property ShowHint;
    property ShowScale;
    property TabOrder;
    property TabStop;
    property TickMarks;
    property TickStyle;
    property Visible;
  end;
  
  


{ TTreeNode }

type
  TCustomTreeView = class;
  TTreeNodes = class;
  TTreeNode = class;

  TNodeState = (nsCut, nsDropHilited, nsFocused, nsSelected, nsMultiSelected,
                nsExpanded, nsHasChildren);
  TNodeStates = set of TNodeState;
  TNodeAttachMode = (
    naAdd,           // add as last sibling of Destination
    naAddFirst,      // add as first sibling of Destnation
    naAddChild,      // add as last child of Destination
    naAddChildFirst, // add as first child of Destination
    naInsert,        // insert in front of Destination
    naInsertBehind   // insert behind Destination
    );

  TAddMode = (taAddFirst, taAdd, taInsert);

  TTreeNodeArray = ^TTreeNode;

  ETreeNodeError = class(Exception);
  ETreeViewError = class(ETreeNodeError);

const
  NodeAttachModeNames: array[TNodeAttachMode] of string =
    ('naAdd', 'naAddFirst', 'naAddChild', 'naAddChildFirst',
     'naInsert', 'naInsertBehind');
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
  TTVCustomCreateNodeEvent = procedure(Sender: TCustomTreeView; var ATreeNode: TTreenode) of object;

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

  { TTreeNode }

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
    function IsNodeHeightFullVisible: Boolean;
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
    function GetParentNodeOfAbsoluteLevel(TheAbsoluteLevel: integer): TTreeNode;
    function GetFirstChild: TTreeNode;
    function GetHandle: THandle;
    function GetLastSibling: TTreeNode;
    function GetLastChild: TTreeNode;
    function GetLastSubChild: TTreeNode;
    function GetNext: TTreeNode;
    function GetNextChild(AValue: TTreeNode): TTreeNode;
    function GetNextExpanded: TTreeNode;
    function GetNextMultiSelected: TTreeNode;
    function GetNextSibling: TTreeNode;
    function GetNextVisible: TTreeNode;
    function GetPrev: TTreeNode;
    function GetPrevChild(AValue: TTreeNode): TTreeNode;
    function GetPrevExpanded: TTreeNode;
    function GetPrevMultiSelected: TTreeNode;
    function GetPrevSibling: TTreeNode;
    function GetPrevVisible: TTreeNode;
    function HasAsParent(AValue: TTreeNode): Boolean;
    function IndexOf(AValue: TTreeNode): Integer;
    function IndexOfText(const NodeText: string): Integer;
    function FindNode(const NodeText: string): TTreeNode;
    procedure MakeVisible;
    procedure MoveTo(Destination: TTreeNode; Mode: TNodeAttachMode); virtual;
    procedure MultiSelectGroup;
    procedure Update;
    procedure ConsistencyCheck;
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
    property IsFullHeightVisible: Boolean read IsNodeHeightFullVisible;
    property Items[ItemIndex: Integer]: TTreeNode read GetItems write SetItems; default;
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

  { TTreeNodes }

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
    function InternalAddObject(Node: TTreeNode; const S: string;
      Data: Pointer; AddMode: TAddMode): TTreeNode;
    procedure DefineProperties(Filer: TFiler); override;
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
    function IsMultiSelection: boolean;
    procedure Delete(Node: TTreeNode);
    procedure EndUpdate;
    function GetFirstNode: TTreeNode;
    function GetLastNode: TTreeNode; // last top level node
    function GetLastSubNode: TTreeNode; // absolute last node
    function GetLastExpandedSubNode: TTreeNode; // absolute last node
    function FindTopLvlNode(const NodeText: string): TTreeNode;
    function Insert(NextNode: TTreeNode; const S: string): TTreeNode;
    function InsertObject(NextNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    function InsertBehind(PrevNode: TTreeNode; const S: string): TTreeNode;
    function InsertObjectBehind(PrevNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    procedure ConsistencyCheck;
    procedure WriteDebugReport(const Prefix: string; AllNodes: boolean);
    property Count: Integer read GetCount;
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
    tvsPainting,
    tvsMouseCapture,
    tvsWaitForDragging,
    tvsDblClicked,
    tvsTripleClicked,
    tvsQuadClicked,
    tvsSelectionChanged
    );
  TTreeViewStates = set of TTreeViewState;

  TTreeViewOption = (
    tvoAllowMultiselect,
    tvoAutoExpand,
    tvoAutoInsertMark,
    tvoAutoItemHeight,
    tvoHideSelection,
    tvoHotTrack,
    tvoKeepCollapsedNodes,
    tvoReadOnly,
    tvoRightClickSelect,
    tvoRowSelect,
    tvoShowButtons,
    tvoShowLines,
    tvoShowRoot,
    tvoShowSeparators,
    tvoToolTips
    );
  TTreeViewOptions = set of TTreeViewOption;

const
  DefaultTreeViewOptions = [tvoShowRoot, tvoShowLines, tvoShowButtons,
                            tvoHideSelection, tvoToolTips,
                            tvoKeepCollapsedNodes, tvoAutoItemHeight];

type
  TTreeViewExpandSignType = (tvestPlusMinus, tvestArrow);
  TTreeViewInsertMarkType = (
    tvimNone,
    tvimAsFirstChild, // or as root
    tvimAsNextSibling,
    tvimAsPrevSibling);

  TCustomTreeView = class(TCustomControl)
  private
    FBackgroundColor: TColor;
    FBottomItem: TTreeNode;
    FExpandSignType: TTreeViewExpandSignType;
    FExpandSignSize: integer;
    //FDefEditProc: Pointer;
    FDefItemHeight: integer;
    FDragImage: TDragImageList;
    FDragNode: TTreeNode;
    //FEditHandle: THandle;
    FIndent: integer;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FInsertMarkNode: TTreeNode;
    FInsertMarkType: TTreeViewInsertMarkType;
    FLastDropTarget: TTreeNode;
    FLastHorzScrollInfo: TScrollInfo;
    FLastVertScrollInfo: TScrollInfo;
    FMaxLvl: integer; // maximum level of all nodes
    FMaxRight: integer; // maximum text width of all nodes (needed for horizontal scrolling)
    fMouseDownX: integer;
    fMouseDownY: integer;
    FOnAdvancedCustomDraw: TTVAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawItem: TTVAdvancedCustomDrawItemEvent;
    FOnChange: TTVChangedEvent;
    FOnChanging: TTVChangingEvent;
    FOnCollapsed: TTVExpandedEvent;
    FOnCollapsing: TTVCollapsingEvent;
    FOnCompare: TTVCompareEvent;
    FOnCustomCreateItem: TTVCustomCreateNodeEvent;
    FOnCustomDraw: TTVCustomDrawEvent;
    FOnCustomDrawItem: TTVCustomDrawItemEvent;
    FOnDeletion: TTVExpandedEvent;
    FOnEditing: TTVEditingEvent;
    FOnEdited: TTVEditedEvent;
    FOnExpanded: TTVExpandedEvent;
    FOnExpanding: TTVExpandingEvent;
    FOnGetImageIndex: TTVExpandedEvent;
    FOnGetSelectedIndex: TTVExpandedEvent;
    FOnSelectionChanged: TNotifyEvent;
    FOptions: TTreeViewOptions;
    FRClickNode: TTreeNode;
    FSaveItems: TStringList;
    FScrollBars: TScrollStyle;
    FScrolledLeft: integer; // horizontal scrolled pixels (hidden pixels at left)
    FScrolledTop: integer;  // vertical scrolled pixels (hidden pixels at top)
    FSelectedColor: TColor;
    FSelectedNode: TTreeNode;
    fSelectionChangeEventLock: integer;
    fSeparatorColor: TColor;
    FSortType: TSortType;
    FStateChangeLink: TChangeLink;
    FStateImages: TCustomImageList;
    FStates: TTreeViewStates;
    FTopItem: TTreeNode;
    FTreeLineColor: TColor;
    FExpandSignColor : TColor;
    FTreeNodes: TTreeNodes;
    FUpdateCount: integer;
    procedure CanvasChanged(Sender: TObject);
    procedure CMDrag(var AMessage: TCMDrag); message CM_DRAG;
    //procedure EditWndProc(var Message: TLMessage);
    function GetAutoExpand: boolean;
    function GetBottomItem: TTreeNode;
    function GetDropTarget: TTreeNode;
    function GetHideSelection: boolean;
    function GetHotTrack: boolean;
    function GetKeepCollapsedNodes: boolean;
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
    procedure SetBottomItem(Value: TTreeNode);
    procedure SetDefaultItemHeight(Value: integer);
    procedure SetExpandSignType(Value: TTreeViewExpandSignType);
    procedure SetDropTarget(Value: TTreeNode);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHotTrack(Value: Boolean);
    procedure SetIndent(Value: Integer);
    procedure SetImages(Value: TCustomImageList);
    procedure SetInsertMarkNode(const AValue: TTreeNode);
    procedure SetInsertMarkType(const AValue: TTreeViewInsertMarkType);
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
    procedure InternalSelectionChanged;
  protected
    FChangeTimer: TTimer;
    function CanChange(Node: TTreeNode): Boolean; dynamic;
    function CanCollapse(Node: TTreeNode): Boolean; dynamic;
    function CanEdit(Node: TTreeNode): Boolean; dynamic;
    function CanExpand(Node: TTreeNode): Boolean; dynamic;
    function CreateNode: TTreeNode; virtual;
    function CustomDraw(const ARect: TRect;
      Stage: TCustomDrawStage): Boolean; virtual;
    function CustomDrawItem(Node: TTreeNode; State: TCustomDrawState;
      Stage: TCustomDrawStage; var PaintImages: Boolean): Boolean; virtual;
    function GetDragImages: TDragImageList; //override;
    function GetMaxLvl: integer;
    function GetMaxScrollLeft: integer;
    function GetMaxScrollTop: integer;
    function GetNodeAtInternalY(Y: Integer): TTreeNode;
    function GetNodeAtY(Y: Integer): TTreeNode;
    function GetNodeDrawAreaHeight: integer;
    function GetNodeDrawAreaWidth: integer;
    function IsCustomDrawn(Target: TCustomDrawTarget;
      Stage: TCustomDrawStage): Boolean;
    function IsNodeVisible(ANode: TTreeNode): Boolean;
    function IsNodeHeightFullVisible(ANode: TTreeNode): Boolean;
    function IsInsertMarkVisible: boolean; virtual;
    procedure Change(Node: TTreeNode); dynamic;
    procedure Collapse(Node: TTreeNode); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Delete(Node: TTreeNode); dynamic;
    procedure DestroyWnd; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DragOver(Source: TObject; X,Y: Integer; State: TDragState;
                       var Accept: Boolean); override;
    procedure DoPaint; virtual;
    procedure DoPaintNode(Node: TTreeNode); virtual;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure EndEditing;
    procedure EnsureNodeIsVisible(ANode: TTreeNode);
    procedure Expand(Node: TTreeNode); dynamic;
    procedure GetImageIndex(Node: TTreeNode); virtual;
    procedure GetSelectedIndex(Node: TTreeNode); virtual;
    procedure InitializeWnd; override;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
    procedure KeyDown(var Key : Word; Shift : TShiftState); override;
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
    procedure UpdateDefaultItemHeight; virtual;
    procedure WndProc(var Message: TLMessage); override;
    procedure UpdateInsertMark(X,Y: integer); virtual;
    procedure DoSelectionChanged; virtual;
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure WMLButtonDown(var AMessage: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMNotify(var AMessage: TLMNotify); message LM_NOTIFY;
    procedure Resize; override;
  protected
    property AutoExpand: Boolean read GetAutoExpand write SetAutoExpand default False;
    property BorderStyle default bsSingle;
    property HideSelection: Boolean
      read GetHideSelection write SetHideSelection default True;
    property HotTrack: Boolean read GetHotTrack write SetHotTrack default False;
    property Images: TCustomImageList read FImages write SetImages;
    property Indent: Integer read fIndent write SetIndent default 15;
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
    property OnCustomCreateItem: TTVCustomCreateNodeEvent read FOnCustomCreateItem write FOnCustomCreateItem;
    property OnCustomDraw: TTVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TTVCustomDrawItemEvent
      read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnDeletion: TTVExpandedEvent read FOnDeletion write FOnDeletion;
    property OnEdited: TTVEditedEvent read FOnEdited write FOnEdited;
    property OnEditing: TTVEditingEvent read FOnEditing write FOnEditing;
    property OnExpanded: TTVExpandedEvent read FOnExpanded write FOnExpanded;
    property OnExpanding: TTVExpandingEvent read FOnExpanding write FOnExpanding;
    property OnGetImageIndex: TTVExpandedEvent
      read FOnGetImageIndex write FOnGetImageIndex;
    property OnGetSelectedIndex: TTVExpandedEvent
      read FOnGetSelectedIndex write FOnGetSelectedIndex;
    property OnSelectionChanged: TNotifyEvent
      read FOnSelectionChanged write FOnSelectionChanged;
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
    procedure ConsistencyCheck;
    function CustomSort(SortProc: TTreeNodeCompare): Boolean;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function GetNodeAt(X, Y: Integer): TTreeNode;
    procedure GetInsertMarkAt(X, Y: Integer; var AnInsertMarkNode: TTreeNode;
                              var AnInsertMarkType: TTreeViewInsertMarkType);
    procedure SetInsertMark(AnInsertMarkNode: TTreeNode;
                            AnInsertMarkType: TTreeViewInsertMarkType);
    procedure SetInsertMarkAt(X,Y: integer); virtual;
    function IsEditing: Boolean;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure FullCollapse;
    procedure FullExpand;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure WriteDebugReport(const Prefix: string; AllNodes: boolean);
    procedure LockSelectionChangeEvent;
    procedure UnlockSelectionChangeEvent;
    function GetFirstMultiSelected: TTreeNode;
    function SelectionVisible: boolean;
    procedure MakeSelectionVisible;
  public
    property BackgroundColor: TColor
      read FBackgroundColor write SetBackgroundColor default clWhite;
    property BorderWidth default 0;
    property BottomItem: TTreeNode read GetBottomItem write SetBottomItem;
    property DefaultItemHeight: integer
      read FDefItemHeight write SetDefaultItemHeight default 20;
    property DropTarget: TTreeNode read GetDropTarget write SetDropTarget;
    property ExpandSignType: TTreeViewExpandSignType
      read FExpandSignType write SetExpandSignType default tvestPlusMinus;
    property InsertMarkNode: TTreeNode read FInsertMarkNode write SetInsertMarkNode;
    property InsertMarkType: TTreeViewInsertMarkType read FInsertMarkType write SetInsertMarkType;
    property KeepCollapsedNodes: boolean
      read GetKeepCollapsedNodes write SetKeepCollapsedNodes;
    property Options: TTreeViewOptions read FOptions write SetOptions default DefaultTreeViewOptions;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property Selected: TTreeNode read GetSelection write SetSelection;
    property SelectionColor: TColor read FSelectedColor write SetSelectedColor default clHighlight;
    property SeparatorColor: TColor read fSeparatorColor write SetSeparatorColor default clGray;
    property TopItem: TTreeNode read GetTopItem write SetTopItem;
    property TreeLineColor: TColor read FTreeLineColor write FTreeLineColor default cl3DLight;
    property ExpandSignColor: TColor read FExpandSignColor write FExpandSignColor default clWindowFrame;
  published
    property TabStop default true;
  end;


  { TTreeView }

  TTreeView = class(TCustomTreeView)
  published
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
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
    property Tag;
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
    property OnContextPopup;
    property OnCustomCreateItem;
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
    property OnSelectionChanged;
    property Options;
    //property OnStartDock;
    property OnStartDrag;
    property Items;
    property TreeLineColor;
    property ExpandSignColor;
  end;


  { TTreeNodeExpandedState }
  { class to store and restore the expanded state of a TTreeView
    The nodes are identified by their Text property.

    Usage example:
      // save old expanded state
      OldExpanded:=TTreeNodeExpandedState.Create(ATreeView);
      ... change a lot of nodes ...
      // restore old expanded state
      OldExpanded.Apply(ATreeView);
      OldExpanded.Free;
   }

  TTreeNodeExpandedState = class
    NodeText: string;
    Childs: TAvgLvlTree;
    constructor Create(FirstTreeNode: TTreeNode);
    constructor Create(TreeView: TCustomTreeView);
    destructor Destroy; override;
    procedure Clear;
    procedure CreateChildNodes(FirstTreeNode: TTreeNode);
    procedure Apply(FirstTreeNode: TTreeNode);
    procedure Apply(TreeView: TCustomTreeView);
  end;

function CompareExpandedNodes(Data1, Data2: Pointer): integer;
function CompareTextWithExpandedNode(Key, Data: Pointer): integer;

function InitCommonControl(CC: Integer): Boolean;
procedure CheckCommonControl(CC: Integer);

procedure Register;

implementation

// !!! Avoid unit circles. Only add units if really needed.
uses
  WSComCtrls;

const
  ScrollBarWidth=0;

{ Toolbar menu support }

function InitCommonControl(CC: Integer): Boolean;
begin
  Result := True;
end;

procedure CheckCommonControl(CC: Integer);
begin

end;

procedure Register;
begin
  RegisterComponents('Common Controls',[TTrackbar,TProgressBar,TTreeView,
    TListView,TStatusBar,TToolBar,TUpDown,TPageControl,TTabControl]);
  RegisterNoIcon([TToolButton,TTabSheet]);
end;

{$I statusbar.inc}
{$I statuspanel.inc}
{$I statuspanels.inc}
{$I tabsheet.inc}
{$I pagecontrol.inc}
{$I tabcontrol.inc}
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
  Revision 1.173  2005/06/22 23:04:44  mattias
  implemented default height for gtk TStatusBar and set TStatusBar.AutoSize to true

  Revision 1.172  2005/04/22 08:19:39  mattias
  added TListItems.FindCaption partstart parameter

  Revision 1.171  2005/04/21 16:15:52  mattias
  added TListView.FindCaption  from Matthijs Willemstein

  Revision 1.170  2005/03/07 00:52:51  mattias
  various Delphi compatibilities  from C Western

  Revision 1.169  2005/03/05 23:00:16  mattias
  adding panels to statusbar during designing now sets SimplePanel:=false

  Revision 1.168  2005/02/28 22:05:06  vincents
  listview fixes for fpc 1.0.x

  Revision 1.167  2005/02/28 18:12:36  mattias
  fixed TCollectionPropertyEditor deleting TStatuspanel

  Revision 1.166  2005/02/26 17:08:41  marc
  * Reworked listviews to match new interface

  Revision 1.165  2005/02/21 21:54:23  micha
  make Action property published for TStatusBar (fired when clicking the statusbar?)

  Revision 1.164  2005/02/21 01:08:19  mattias
  fixed changing Index of TStatusPanel

  Revision 1.163  2005/02/17 18:56:32  mattias
  line ends in debug output are now converted, added TTreeNode.FindNode

  Revision 1.162  2005/02/05 16:09:52  marc
  * first 64bit changes

  Revision 1.161  2005/01/28 16:45:43  mattias
  added TTreeView.OnCustomCreateItem  from Andre Haines

  Revision 1.160  2005/01/27 19:03:51  mattias
  added QuestionDlg - a MessageDlg with custom buttons

  Revision 1.159  2005/01/27 10:10:25  mattias
  added TTreeNode.GetParentNodeOfAbsoluteLevel  from Sergio

  Revision 1.158  2005/01/25 01:18:09  mattias
  added TTreeView.ExpandSignColor  from Sergio

  Revision 1.157  2005/01/07 17:40:59  mattias
  fixed TTabSheet.SetPageControl

  Revision 1.156  2005/01/03 11:54:37  micha
  add TStatusBar.Canvas

  Revision 1.155  2005/01/03 01:07:08  mattias
  fixed registering TProgressBar, disabled docking in TToolBar, return key for codeexplorer, updated finnish translation

  Revision 1.154  2005/01/01 18:56:47  mattias
  implemented TTIProgressBar

  Revision 1.153  2004/12/27 19:40:59  mattias
  published BorderSpacing for many controls

  Revision 1.152  2004/11/28 00:55:44  mattias
  deactivated sending SYSKey messages in gtk intf - they are not used anyway

  Revision 1.151  2004/09/13 14:34:53  micha
  convert LM_TB_BUTTONCOUNT to interface method

  Revision 1.150  2004/09/12 20:59:05  vincents
  fix fpc 1.0.x compilation

  Revision 1.149  2004/09/10 16:28:50  mattias
  implemented very rudimentary TTabControl

  Revision 1.148  2004/09/09 22:00:37  mattias
  started TTabControlNotebookStrings

  Revision 1.147  2004/09/09 09:35:44  mattias
  renamed customradiogroup.inc to radiogroup.inc

  Revision 1.146  2004/09/08 23:05:35  mattias
  improved TListView.SetItemVisible  from Andrew Haines

  Revision 1.145  2004/09/08 22:59:54  mattias
  started TTabControl

  Revision 1.144  2004/09/04 22:24:16  mattias
  added default values for compiler skip options and improved many parts of synedit for UTF8

  Revision 1.143  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.142  2004/08/15 17:00:58  mattias
  improved DefineProperties to read/write endian independent

  Revision 1.141  2004/08/04 09:35:38  mattias
  implemented setting TTabSheet.TabIndex

  Revision 1.140  2004/07/24 11:23:56  mattias
  Designer can now handle TPersistent selections, TCollectionPropertyEditor basically working

  Revision 1.139  2004/07/24 00:00:33  mattias
  started TCollectionPropertyEditor

  Revision 1.138  2004/07/23 16:44:27  mattias
  activated new TToolbar, old can be activated with -dOldToolBar

  Revision 1.137  2004/07/14 15:57:53  mattias
  fixed 1.0.10 compilation  from Vincent

  Revision 1.136  2004/07/11 17:20:47  marc
  * Implemented most of TListColoum/Item in the Ws for gtk and win32

  Revision 1.135  2004/07/11 13:03:53  mattias
  extended RolesForForm to manage multiple roles for on control

  Revision 1.134  2004/06/28 23:16:24  mattias
  added TListView.AddItems  from Andrew Haines

  Revision 1.133  2004/06/17 10:38:40  mattias
  fixed TToolButton.SetMenuItem while loading

  Revision 1.132  2004/06/15 17:21:01  mattias
  fixed TTreeNode.Delete and deleting in between node

  Revision 1.131  2004/06/05 10:29:15  mattias
  replaced FindMask by global function   from Vincent

  Revision 1.130  2004/06/01 09:58:34  mattias
  implemented setting TCustomPage.PageIndex  from Andrew Haines

  Revision 1.129  2004/05/21 18:34:44  mattias
  readded protected TWinControl.BorderStyle

  Revision 1.128  2004/05/21 18:12:17  mattias
  quick fixed crashing property overloading BorderStyle

  Revision 1.127  2004/05/21 09:03:54  micha
  implement new borderstyle
  - centralize to twincontrol (protected)
  - public expose at tcustomcontrol to let interface access it

  Revision 1.126  2004/05/20 21:28:54  marc
  * Fixed win32 listview

  Revision 1.125  2004/05/20 14:45:47  mattias
  implemented showing treenode completely on single selection

  Revision 1.124  2004/05/19 18:41:19  micha
  disable custom drawn border by default for treeview (already has borderstyle=bsSingle

  Revision 1.123  2004/05/18 23:10:41  marc
  * Started to move TListview to the WS interface

  Revision 1.122  2004/05/11 09:49:46  mattias
  started sending CN_KEYUP

  Revision 1.121  2004/04/04 17:10:05  marc
  Patch from Andrew Haines

  Revision 1.120  2004/03/18 22:35:52  mattias
  improved TCustomListView.ItemAdded with an Index param  from Andrew

  Revision 1.119  2004/03/06 18:44:06  mattias
  workaround for fpc bug 2859

  Revision 1.118  2004/02/28 10:16:02  mattias
  fixed 1.0.x compilation

  Revision 1.117  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.116  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.115  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.114  2004/02/22 16:22:53  mattias
  fixed old toolbar compilation

  Revision 1.113  2004/02/22 16:20:29  mattias
  fixed old toolbar compilation

  Revision 1.112  2004/02/22 15:39:43  mattias
  fixed error handling on saving lpi file

  Revision 1.111  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.110  2004/02/21 15:37:33  mattias
  moved compiler options to project menu, added -CX for smartlinking

  Revision 1.109  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.108  2004/02/12 18:09:10  mattias
  removed win32 specific TToolBar code in new TToolBar, implemented TWinControl.FlipChildren

  Revision 1.107  2004/02/11 11:34:15  mattias
  started new TToolBar

  Revision 1.106  2004/02/04 12:59:07  mattias
  added TToolButton.Action and published some props

  Revision 1.105  2004/02/02 20:00:45  mattias
  published TTreeView.Tab

  Revision 1.104  2004/02/02 19:48:01  mattias
  fixed removing TStatusBar panels in gtk

  Revision 1.103  2004/01/21 10:19:16  micha
  enable tabstops for controls; implement tabstops in win32 intf

  Revision 1.102  2004/01/12 13:43:12  mattias
  improved and activated new statusbar

  Revision 1.101  2004/01/12 08:36:33  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.100  2004/01/10 17:09:20  mattias
  deactivated EraseBackGound for TOIPropertyGrid and TTreeView

  Revision 1.99  2004/01/03 23:14:59  mattias
  default font can now change height and fixed gtk crash

  Revision 1.98  2004/01/03 21:06:05  micha
  - fix win32/checklistbox
  - implement proper lcl to interface move/size notify via setwindowpos
  - fix treeview to use inherited canvas from customcontrol
  - implement double buffering in win32

  Revision 1.97  2003/12/28 02:40:50  mattias
  set colors to default values

  Revision 1.96  2003/12/26 15:23:29  mattias
  started message editor and fixed some range checks

  Revision 1.95  2003/12/26 10:59:24  mattias
  fixed color coversion range check

  Revision 1.94  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.93  2003/12/21 16:01:58  mattias
  workaround for inherited bug in fpc 1.9

  Revision 1.92  2003/11/30 18:35:19  mattias
  fixed fpc 1.9.1 warns

  Revision 1.91  2003/11/07 15:57:45  mattias
  implemented auto selection visible for component tree

  Revision 1.90  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.89  2003/09/20 15:24:54  mattias
  implemented TPageControl and TTabSheet

  Revision 1.88  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.87  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.86  2003/09/13 10:06:53  mattias
  fixed ColorIsStored

  Revision 1.85  2003/08/30 18:53:07  mattias
  using default colors, when theme does not define them

  Revision 1.84  2003/08/22 18:10:39  mattias
  implemented selections in component tree

  Revision 1.83  2003/08/22 07:58:38  mattias
  started componenttree

  Revision 1.82  2003/08/21 13:04:10  mattias
  implemented insert marks for TTreeView

  Revision 1.81  2003/08/14 15:31:42  mattias
  started TTabSheet and TPageControl

  Revision 1.80  2003/06/25 21:02:19  mattias
  reduced TCustomProgressBar setproperties calls

  Revision 1.79  2003/06/19 16:36:35  mattias
  started codeexplorer

  Revision 1.78  2003/06/18 11:21:06  mattias
  fixed taborder=0, implemented TabOrder Editor

  Revision 1.77  2003/06/13 21:13:20  mattias
  fixed TCustomTrackBar initial size

  Revision 1.76  2003/06/13 12:53:51  mattias
  fixed TUpDown and added handler lists for TControl

  Revision 1.75  2003/06/13 11:58:46  mattias
  fixed readonly of properties

  Revision 1.74  2003/06/10 00:46:16  mattias
  fixed aligning controls

  Revision 1.73  2003/04/16 17:20:24  mattias
  implemented package check broken dependency on compile

  Revision 1.72  2003/04/14 18:03:48  mattias
  implemented inherited compiler options

  Revision 1.71  2003/04/13 13:45:04  mattias
  implemented broken dependencies dialog

  Revision 1.70  2003/04/08 16:56:55  mattias
  implemented saving package

  Revision 1.69  2003/04/04 16:35:24  mattias
  started package registration

  Revision 1.68  2003/04/02 13:23:23  mattias
  fixed default font

  Revision 1.67  2003/03/15 13:26:07  mattias
  fixes for fpc 1.1

  Revision 1.66  2003/03/11 07:46:43  mattias
  more localization for gtk- and win32-interface and lcl

  Revision 1.65  2003/03/06 17:15:39  ajgenius
  added Math to uses. new updown arrow code uses max

  Revision 1.64  2003/02/24 19:00:42  mattias
  added TlistView.SubItem improvements from Olivier Guilbaud

  Revision 1.63  2003/02/18 23:22:56  mattias
  added listview items property editor

  Revision 1.62  2002/12/28 11:29:47  mattias
  xmlcfg deletion, focus fixes

  Revision 1.61  2002/11/30 11:22:53  mattias
  statusbar now uses invalidaterect

  Revision 1.60  2002/11/25 11:37:18  mattias
  applied patch from Vasily

  Revision 1.59  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.58  2002/11/13 18:21:04  lazarus
  MG: added TListItems.Clear

  Revision 1.57  2002/10/30 13:20:10  lazarus
  MG: fixed example

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
