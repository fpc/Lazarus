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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  SysUtils, Types, Classes, Math, LCLStrConsts, LResources, LCLIntf, LCLType,
  LCLProc, AvgLvlTree, LMessages, ImgList, ActnList, GraphType, Graphics, Menus,
  Controls, Forms, StdCtrls, ExtCtrls, ToolWin, Buttons, Themes;

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

  //added.
  TStatusPanelClass = class of TStatusPanel;
  
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
                        var PreferredWidth, PreferredHeight: integer;
                        WithThemeSpace: Boolean); override;

    //added.
    function CreatePanel: TStatusPanel; virtual;
    function CreatePanels: TStatusPanels; virtual;
    function GetPanelClass: TStatusPanelClass; virtual;

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
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnHint: TNotifyEvent read FOnHint write FOnHint;
    property OnMouseDown;
    property OnStartDrag;
  end;


  { TTabSheet }

  TPageControl = class;

  // TTabPosition is in extctrls.pas
  TTabStyle = (tsTabs, tsButtons, tsFlatButtons);

  { TTabSheet }

  TTabSheet = class(TCustomPage)
  private
    function GetPageControl: TPageControl;
    function GetTabIndex: Integer;
    procedure SetPageControl(APageControl: TPageControl);
    procedure SetTabIndex(const AValue: Integer);
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
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow;
    property OnStartDrag;
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
    procedure DockDrop(DockObject: TDragDockObject; X, Y: Integer); override;
  published
    property ActivePage: TTabSheet read GetActiveTabSheet write SetActiveTabSheet;
    property Align;
    property Anchors;
    property BorderSpacing;
    //property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
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
    property OnChanging;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    //property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPageChanged;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
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
    FTabs: TStrings;// this is a TTabControlNoteBookStrings
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
    class function GetControlClassDefaultSize: TPoint; override;
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
    property TabPosition: TTabPosition read FTabPosition write SetTabPosition
                                       default tpTop;
    property TabWidth: Smallint read GetTabWidth write SetTabWidth default 0;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function IndexOfTabAt(X, Y: Integer): Integer;
    function GetHitTestInfoAt(X, Y: Integer): THitTests;
    function IndexOfTabWithCaption(const TabCaption: string): Integer;
    function TabRect(Index: Integer): TRect;
    function RowCount: Integer;
    procedure ScrollTabs(Delta: Integer);
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: boolean;
    property TabIndex: Integer read GetTabIndex write SetTabIndex default -1;
    property Tabs: TStrings read FTabs write SetTabs;
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

  TCustomDrawTarget = (
    dtControl,   // the whole control
    dtItem,      // one item (= line in report mode)
    dtSubItem    // one subitem, except for subitem 0, this one is drawn by dtItem
  );
  TCustomDrawStage = (
    cdPrePaint,
    cdPostPaint,
    cdPreErase,
    cdPostErase
  );
  TCustomDrawStateFlag = (
    cdsSelected,
    cdsGrayed,
    cdsDisabled,
    cdsChecked,
    cdsFocused,
    cdsDefault,
    cdsHot,
    cdsMarked,
    cdsIndeterminate
  );
  TCustomDrawState = set of TCustomDrawStateFlag;
  
  TCustomDrawResultFlag = (
    cdrSkipDefault,
    cdrNotifyPostpaint,
    cdrNotifyItemdraw,
    cdrNotifySubitemdraw,
    cdrNotifyPosterase,
    cdrNotifyItemerase
  );
  TCustomDrawResult = set of TCustomDrawResultFlag;


  { TListView }

  TListItems = class;  //forward declaration!
  TCustomListView = class;  //forward declaration!
  TSortType = (stNone, stData, stText, stBoth);
  
  TListItemState = (lisCut, lisDropTarget, lisFocused, lisSelected);
  TListItemStates = set of TListItemState;
  
  TListItemFlag = (lifDestroying, lifCreated);
  TListItemFlags = set of TListItemFlag;

  TDisplayCode = (drBounds, drIcon, drLabel, drSelectBounds);
  
  { TListItem }

  TListItem = class(TPersistent)
  private
    FOwner: TListItems;
    FFlags: TListItemFlags;
    FSubItems: TStrings;
    FCaption: String;
    FData: Pointer;
    FImageIndex: Integer;
    FStates: TListItemStates;
    FChecked: Boolean;
    function GetChecked: Boolean;
    function GetLeft: Integer;
    function GetListView: TCustomListView;
    function GetPosition: TPoint;
    function GetState(const ALisOrd: Integer): Boolean;
    function GetIndex: Integer;
    function GetSubItemImages(const AIndex: Integer): Integer;
    function GetSubItems: TStrings;
    function GetTop: Integer;

    function WSUpdateAllowed: Boolean;
    procedure WSUpdateText;
    procedure WSUpdateImages;
    procedure WSUpdateChecked;

    procedure SetChecked(AValue: Boolean);
    procedure SetState(const ALisOrd: Integer; const AIsSet: Boolean);
    procedure SetData(const AValue: Pointer);
    procedure SetImageIndex(const AValue: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetCaption(const AValue : String);
    procedure SetPosition(const AValue: TPoint);
    procedure SetSubItemImages(const AIndex, AValue: Integer);
    procedure SetSubItems(const AValue: TStrings);
    procedure SetTop(Value: Integer);
  protected
    function IsEqual(const AItem: TListItem): Boolean;
  public
    procedure Assign(ASource: TPersistent); override;

    constructor Create(AOwner : TListItems);
    destructor Destroy; override;
    procedure Delete;
    procedure MakeVisible(PartialOK: Boolean);
    function DisplayRect(Code: TDisplayCode): TRect;
    function DisplayRectSubItem(subItem: integer;Code: TDisplayCode): TRect;

    property Caption : String read FCaption write SetCaption;
    property Checked : Boolean read GetChecked write SetChecked;
    property Cut: Boolean index Ord(lisCut) read GetState write SetState;
    property Data: Pointer read FData write SetData;
    property DropTarget: Boolean index Ord(lisDropTarget) read GetState write SetState;
    property Focused: Boolean index Ord(lisFocused) read GetState write SetState;
    property Index: Integer read GetIndex;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Left: Integer read GetLeft write SetLeft;
    property ListView: TCustomListView read GetListView;
    property Owner: TListItems read FOwner;
    property Position: TPoint read GetPosition write SetPosition;
    property Selected: Boolean index Ord(lisSelected) read GetState write SetState;
    property SubItems: TStrings read GetSubItems write SetSubItems;
    property SubItemImages[const AIndex: Integer]: Integer read GetSubItemImages write SetSubItemImages;
    property Top: Integer read GetTop write SetTop;
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
    procedure WSCreateCacheItem;
    function WSUpdateAllowed: Boolean;
    procedure ItemDestroying(const AItem: TListItem); //called by TListItem when freed
    procedure ReadData(Stream: TStream); // read data in a Delphi compatible way
    procedure ReadLazData(Stream: TStream); // read data in a 64 bits safe way
    procedure WriteLazData(Stream: TStream); // write date in a 64 bits safe way
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetCount : Integer;
    function GetItem(const AIndex: Integer): TListItem;
    procedure WSCreateItems;
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
                     PartStart: Boolean = True): TListItem;
    function FindData(const AData: Pointer): TListItem;
    function IndexOf(const AItem: TListItem): Integer;
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
    FCaption: TTranslateString;
    FMinWidth: TWidth;
    FMaxWidth: TWidth;
    FVisible: Boolean;
    FWidth: TWidth;
    FImageIndex: Integer;
    FTag: Integer;
    function GetWidth: TWidth;
    procedure WSCreateColumn;
    procedure WSDestroyColumn;
    function  WSUpdateAllowed: Boolean;
    procedure SetVisible(const AValue: Boolean);
    procedure SetAutoSize(const AValue: Boolean);
    procedure SetMinWidth(const AValue: TWidth);
    procedure SetMaxWidth(const AValue: TWidth);
    procedure SetWidth(const AValue: TWidth);
    procedure SetCaption(const AValue: TTranslateString);
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
    property Caption: TTranslateString read FCaption write SetCaption;
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
    procedure WSCreateColumns;
    procedure SetItem(const AIndex: Integer; const AValue: TListColumn);
  protected
  public
    constructor Create(TheOwner: TCustomListView);
    destructor Destroy; override;
    function Add: TListColumn;
    procedure BeginUpdate; {$IFNDEF VER2_0}override{$ELSE}virtual{$ENDIF};
    procedure EndUpdate; {$IFNDEF VER2_0}override{$ELSE}virtual{$ENDIF};
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
  TLVCustomDrawEvent = procedure(Sender: TCustomListView; const ARect: TRect;
                                  var DefaultDraw: Boolean) of object;
  TLVCustomDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem; 
                                State: TCustomDrawState; var DefaultDraw: Boolean) of object;
  TLVCustomDrawSubItemEvent=procedure(Sender: TCustomListView; Item: TListItem; 
                                 SubItem: Integer; State: TCustomDrawState;
                                 var DefaultDraw: Boolean) of object;
  TLVAdvancedCustomDrawEvent = procedure(Sender: TCustomListView; const ARect: TRect;
                                Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;
  TLVAdvancedCustomDrawItemEvent = procedure(Sender: TCustomListView; Item: TListItem; 
                                State: TCustomDrawState; Stage: TCustomDrawStage; 
                                var DefaultDraw: Boolean) of object;
  TLVAdvancedCustomDrawSubItemEvent=procedure(Sender: TCustomListView; Item: TListItem; 
                                 SubItem: Integer; State: TCustomDrawState;
                                 Stage: TCustomDrawStage; var DefaultDraw: Boolean) of object;

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
    lvpWrapText,
    lvpToolTips
  );
  TListViewProperties = set of TListViewProperty;

  TListViewImageList = (lvilSmall, lvilLarge, lvilState);
  
  TListHotTrackStyle = (htHandPoint, htUnderlineCold, htUnderlineHot);
  TListHotTrackStyles = set of TListHotTrackStyle;
  
  TListViewFlag = (
    lffSelectedValid
    );
  TListViewFlags = set of TListViewFlag;

  { TCustomListView }

  TCustomListView = class(TWinControl)
  private
    FAllocBy: Integer;
    FCanvas: TCanvas;
    FDefaultItemHeight: integer;
    FHotTrackStyles: TListHotTrackStyles;
    FOwnerData: Boolean;
    FListItems: TListItems;
    FColumns: TListColumns;
    FImages: array[TListViewImageList] of TCustomImageList;
    FImageChangeLinks: array[TListViewImageList] of TChangeLink;
    FFlags: TListViewFlags;

    FViewStyle: TViewStyle;
    FSortType: TSortType;
    FSortColumn: Integer;
    FScrollBars: TScrollStyle;
    FViewOriginCache: TPoint; // scrolled originwhile handle is not created
    FSelected: TListItem;     // temp copy of the selected item
    FFocused: TListItem;      // temp copy of the focused item
    FHoverTime: Integer;      // temp copy of the hover time (the time a mouse must be over a item to auto select)
    // MWE: not used: see updateScrollbars
    // FLastHorzScrollInfo: TScrollInfo;
    // FLastVertScrollInfo: TScrollInfo;
    FUpdateCount: integer;
    FOnChange: TLVChangeEvent;
    FOnColumnClick: TLVColumnClickEvent;
    FOnCompare: TLVCompareEvent;
    FOnDeletion: TLVDeletedEvent;
    FOnInsert: TLVInsertEvent;
    FOnSelectItem: TLVSelectItemEvent;
    FOnCustomDraw: TLVCustomDrawEvent;
    FOnCustomDrawItem: TLVCustomDrawItemEvent;
    FOnCustomDrawSubItem: TLVCustomDrawSubItemEvent;
    FOnAdvancedCustomDraw: TLVAdvancedCustomDrawEvent;
    FOnAdvancedCustomDrawItem: TLVAdvancedCustomDrawItemEvent;
    FOnAdvancedCustomDrawSubItem: TLVAdvancedCustomDrawSubItemEvent;
    FProperties: TListViewProperties;
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
    procedure SetImageList(const ALvilOrd: Integer; const AValue: TCustomImageList);
    procedure SetItems(const AValue : TListItems);
    procedure SetItemVisible(const AValue: TListItem; const APartialOK: Boolean);
    procedure SetOwnerData(const AValue: Boolean);
    procedure SetProperty(const ALvpOrd: Integer; const AIsSet: Boolean);
    procedure SetScrollBars(const AValue: TScrollStyle);
    procedure SetSelection(const AValue: TListItem);
    procedure SetSortColumn(const AValue: Integer);
    procedure SetSortType(const AValue: TSortType);
    procedure SetViewOrigin(AValue: TPoint);
    procedure SetViewStyle(const Avalue: TViewStyle);
    procedure Sort;
    procedure UpdateScrollbars;
    procedure CNNotify(var AMessage: TLMNotify); message CN_NOTIFY;
    procedure InvalidateSelected;
  protected
    //called by TListItems
    procedure ItemDeleted(const AItem: TListItem);
    procedure ItemInserted(const AItem: TListItem);

  protected
    procedure InitializeWnd; override;
    procedure FinalizeWnd; override;

    procedure DestroyWnd; override;
    procedure Change(AItem: TListItem; AChange: Integer); dynamic;
    procedure ColClick(AColumn: TListColumn); dynamic;

    procedure Delete(Item : TListItem);
    procedure DoDeletion(AItem: TListItem); dynamic;
    procedure DoInsert(AItem: TListItem); dynamic;
    procedure DoSelectItem(AItem: TListItem; ASelected: Boolean); dynamic;
    procedure InsertItem(Item : TListItem);
    procedure ImageChanged(Sender : TObject);
    procedure Loaded; override;
    
    function IsCustomDrawn(ATarget: TCustomDrawTarget; AStage: TCustomDrawStage): Boolean; virtual;
    function CustomDraw(const ARect: TRect; AStage: TCustomDrawStage): Boolean; virtual;                                                   // Return True if default drawing should be done
    function CustomDrawItem(AItem: TListItem; AState: TCustomDrawState; AStage: TCustomDrawStage): Boolean; virtual;                       //
    function CustomDrawSubItem(AItem: TListItem; ASubItem: Integer; AState: TCustomDrawState; AStage: TCustomDrawStage): Boolean; virtual; //
    function IntfCustomDraw(ATarget: TCustomDrawTarget; AStage: TCustomDrawStage; AItem, ASubItem: Integer; AState: TCustomDrawState; const ARect: PRect): TCustomDrawResult;
  protected
    property AllocBy: Integer read FAllocBy write SetAllocBy default 0;
    property BorderStyle default bsSingle;
    property Columns: TListColumns read FColumns write SetColumns;
    property ColumnClick: Boolean index Ord(lvpColumnClick) read GetProperty write SetProperty default True;
    property DefaultItemHeight: integer read FDefaultItemHeight write SetDefaultItemHeight;
    property HideSelection: Boolean index Ord(lvpHideSelection) read GetProperty write SetProperty default True;
    property HoverTime: Integer read GetHoverTime write SetHoverTime default -1;
    property Items: TListItems read FListItems write SetItems;
    property LargeImages: TCustomImageList index Ord(lvilLarge) read GetImageList write SetImageList;
    property MultiSelect: Boolean index Ord(lvpMultiselect) read GetProperty write SetProperty default False;
    property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
    property OwnerDraw: Boolean index Ord(lvpOwnerDraw) read GetProperty write SetProperty default False;
    property ReadOnly: Boolean index Ord(lvpReadOnly) read GetProperty write SetProperty default False;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property ShowColumnHeaders: Boolean index Ord(lvpShowColumnHeaders) read GetProperty write SetProperty default True;
    property ShowWorkAreas: Boolean index Ord(lvpShowWorkAreas) read GetProperty write SetProperty default False;
    property SmallImages: TCustomImageList index Ord(lvilSmall) read GetImageList write SetImageList;
    property SortType: TSortType read FSortType write SetSortType;
    property SortColumn: Integer read FSortColumn write SetSortColumn;
    property StateImages: TCustomImageList index Ord(lvilState) read GetImageList write SetImageList;
    property ToolTips: Boolean index Ord(lvpToolTips) read GetProperty write SetProperty default True;
    property ViewStyle: TViewStyle read FViewStyle write SetViewStyle default vsList;
    property OnChange: TLVChangeEvent read FOnChange write FOnChange;
    property OnColumnClick: TLVColumnClickEvent read FOnColumnClick write FOnColumnClick;
    property OnCompare: TLVCompareEvent read FOnCompare write FOnCompare;
    property OnDeletion: TLVDeletedEvent read FOnDeletion write FOnDeletion;
    property OnInsert: TLVInsertEvent read FOnInsert write FOnInsert;
    property OnSelectItem: TLVSelectItemEvent read FOnSelectItem write FOnSelectItem;
    property OnCustomDraw: TLVCustomDrawEvent read FOnCustomDraw write FOnCustomDraw;
    property OnCustomDrawItem: TLVCustomDrawItemEvent read FOnCustomDrawItem write FOnCustomDrawItem;
    property OnCustomDrawSubItem: TLVCustomDrawSubItemEvent read FOnCustomDrawSubItem write FOnCustomDrawSubItem;
    property OnAdvancedCustomDraw: TLVAdvancedCustomDrawEvent read FOnAdvancedCustomDraw write FOnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem: TLVAdvancedCustomDrawItemEvent read FOnAdvancedCustomDrawItem write FOnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem: TLVAdvancedCustomDrawSubItemEvent read FOnAdvancedCustomDrawSubItem write FOnAdvancedCustomDrawSubItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure Clear;
    procedure EndUpdate;
    function FindCaption(StartIndex: Integer; Value: string; Partial, Inclusive, Wrap: Boolean; PartStart: Boolean = True): TListItem;
    function GetItemAt(x,y: integer): TListItem;
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
    property ViewOrigin: TPoint read GetViewOrigin write SetViewOrigin;
    property VisibleRowCount: Integer read GetVisibleRowCount;
  end;


  { TListView }

  TListView = class(TCustomListView)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
//    property BorderStyle;
    property BorderWidth;
    property Checkboxes;
    property Color default clWindow;
    property Columns;
    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragMode;
//    property DefaultItemHeight;
//    property DropTarget;
    property Enabled;
//    property FlatScrollBars;
    property Font;
//    property FullDrag;
//    property GridLines;
    property HideSelection;
//    property HotTrack;
//    property HotTrackStyles;
//    property HoverTime;
    property Items;
    property LargeImages;
    property MultiSelect;
//    property OwnerData;
//    property OwnerDraw;
    property PopupMenu;
    property ReadOnly;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
//    property ShowWorkAreas;
    property SmallImages;
    property SortColumn;
    property SortType;
    property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDrag;
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
    class function GetControlClassDefaultSize: TPoint; override;
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
 

  TUDAlignButton = (udLeft, udRight, udTop, udBottom);
  TUDOrientation = (udHorizontal, udVertical);
  TUDBtnType = (btNext, btPrev);
  TUDClickEvent = procedure (Sender: TObject; Button: TUDBtnType) of object;
  TUDChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;

  { TCustomUpDown }

  TCustomUpDown = class(TCustomControl)
  private
    MinBtn: TControl;// TSpeedButton
    MaxBtn: TControl;// TSpeedButton
    BTimerProc: Procedure of Object;
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
    class function GetControlClassDefaultSize: TPoint; override;
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


  { TUpDown }

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
    procedure CMEnabledChanged(var Message: TLMEssage); message CM_ENABLEDCHANGED;
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
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         WithThemeSpace: Boolean); override;
    class function GetControlClassDefaultSize: TPoint; override;
    procedure Loaded; override;
    procedure RefreshControl; virtual;
    procedure SetToolBar(NewToolBar: TToolBar);
    procedure UpdateControl; virtual;
    function GetButtonDrawDetail: TThemedElementDetails; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure UpdateVisibleToolbar;
    function GroupAllUpAllowed: boolean;
    function DialogChar(var Message: TLMKey): boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
    function CheckMenuDropdown: Boolean; dynamic;
    procedure Click; override;
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
    procedure ApplyFontForButtons;
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
    procedure AdjustClientRect(var ARect: TRect); override;
    class function GetControlClassDefaultSize: TPoint; override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure CalculatePreferredSize(var PreferredWidth,
                    PreferredHeight: integer; WithThemeSpace: Boolean); override;
    function CheckMenuDropdown(Button: TToolButton): Boolean; dynamic;
    procedure ClickButton(Button: TToolButton); dynamic;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure ControlsAligned; override;
    function FindButtonFromAccel(Accel: Word): TToolButton;
    procedure FontChanged(Sender: TObject); override;
    procedure ParentFontChanged; override;
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
    procedure SetTickMarks(Value: TTickMark);
    procedure SetTickStyle(Value: TTickStyle);
    procedure UpdateSelection;
  protected
    procedure ApplyChanges;
    procedure DoChange(var msg); message LM_CHANGED;
    class function GetControlClassDefaultSize: TPoint; override;
    procedure InitializeWnd; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTick(Value: Integer);
  published
    property Frequency: Integer read FFrequency write SetFrequency default 1;
    property LineSize: Integer read FLineSize write SetLineSize default 1;
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation;
    property PageSize: Integer read FPageSize write SetPageSize default 2;
    property Position: Integer read FPosition write SetPosition;
    property ScalePos: TTrackBarScalePos read FScalePos write SetScalePos;
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
                nsExpanded, nsHasChildren, nsInTree, nsDeleting);
  TNodeStates = set of TNodeState;
  TNodeAttachMode = (
    naAdd,           // add as last sibling of Destination
    naAddFirst,      // add as first sibling of Destination
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
    FHeight: integer;     // height in pixels
    FImageIndex: integer;
    FIndex: integer;      // index in parent
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
    function GetDeleting: Boolean;
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
    procedure SetIndex(const AValue: Integer);
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
    function GetTextPath: string;
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
    property Deleting: Boolean read GetDeleting;
    property Focused: Boolean read GetFocused write SetFocused;
    property DropTarget: Boolean read GetDropTarget write SetDropTarget;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property Handle: THandle read GetHandle;
    property HasChildren: Boolean read GetHasChildren write SetHasChildren;
    property Height: integer read GetHeight write SetHeight;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Index: Integer read GetIndex write SetIndex;
    property IsVisible: Boolean read IsNodeVisible;
    property IsFullHeightVisible: Boolean read IsNodeHeightFullVisible;
    property Items[ItemIndex: Integer]: TTreeNode read GetItems write SetItems; default;
    property Level: Integer read GetLevel;
    property MultiSelected: Boolean read GetMultiSelected write SetMultiSelected;
    property OverlayIndex: Integer read FOverlayIndex write SetOverlayIndex default -1;
    property Owner: TTreeNodes read FOwner;
    property Parent: TTreeNode read FParent;
    property Selected: Boolean read GetSelected write SetSelected;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex default -1;
    property SubTreeCount: integer read FSubTreeCount;
    property StateIndex: Integer read FStateIndex write SetStateIndex default -1;
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
    FLastMultiSelected: TTreeNode;
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
    procedure ClearMultiSelection(ClearSelected: boolean = false);
    procedure SelectOnlyThis(Node: TTreeNode);
    function IsMultiSelection: boolean;
    procedure Delete(Node: TTreeNode);
    procedure EndUpdate;
    function GetFirstNode: TTreeNode;
    function GetLastNode: TTreeNode; // last top level node
    function GetLastSubNode: TTreeNode; // absolute last node
    function GetLastExpandedSubNode: TTreeNode; // absolute last node
    function FindTopLvlNode(const NodeText: string): TTreeNode;
    function FindNodeWithText(const NodeText: string): TTreeNode;
    function FindNodeWithData(const NodeData: Pointer): TTreeNode;
    function Insert(NextNode: TTreeNode; const S: string): TTreeNode;
    function InsertObject(NextNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    function InsertBehind(PrevNode: TTreeNode; const S: string): TTreeNode;
    function InsertObjectBehind(PrevNode: TTreeNode; const S: string;
      Data: Pointer): TTreeNode;
    procedure SortTopLevelNodes(SortProc: TTreeNodeCompare);
    procedure ConsistencyCheck;
    procedure WriteDebugReport(const Prefix: string; AllNodes: boolean);
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TTreeNode read GetNodeFromIndex; default;
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
    tvoToolTips,
    tvoNoDoubleClickExpand
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
    function GetDragImages: TDragImageList; override;
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
    function DefaultTreeViewSort(Node1, Node2: TTreeNode): Integer;
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
    property OnShowHint;
    //property OnStartDock;
    property OnStartDrag;
    property Options;
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



  TCustomHeaderControl = class;


  { THeaderSection }
  
  THeaderSectionState =
  (
    hsNormal,
    hsHot,
    hsPressed
  );
  THeaderSection = class(TCollectionItem)
  private
    FAlignment: TAlignment;
    FText: string;
    FWidth: Integer;
    FImageIndex: TImageIndex;
    FState: THeaderSectionState;
    function GetLeft: Integer;
    function GetRight: Integer;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetState(const AValue: THeaderSectionState);
    procedure SetText(const Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetImageIndex(const Value: TImageIndex);
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property Left: Integer read GetLeft;
    property Right: Integer read GetRight;
    property State: THeaderSectionState read FState write SetState;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property Text: string read FText write SetText;
    property Width: Integer read FWidth write SetWidth;
  end;
  
  THeaderSectionClass = class of THeaderSection;


  { THeaderSections }
  
  THeaderSections = class(TCollection)
  private
    FHeaderControl: TCustomHeaderControl;
    function GetItem(Index: Integer): THeaderSection;
    procedure SetItem(Index: Integer; Value: THeaderSection);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(HeaderControl: TCustomHeaderControl);
    function Add: THeaderSection;
    function AddItem(Item: THeaderSection; Index: Integer): THeaderSection;
    function Insert(Index: Integer): THeaderSection;
    property Items[Index: Integer]: THeaderSection read GetItem write SetItem; default;
  end;

  TCustomSectionNotifyEvent = procedure(HeaderControl: TCustomHeaderControl;
    Section: THeaderSection) of object;
  TCustomHCCreateSectionClassEvent = procedure(Sender: TCustomHeaderControl;
    var SectionClass: THeaderSectionClass) of object;
    
    
  { TCustomHeaderControl }
  
  TCustomHeaderControl = class(TCustomControl)
  private
    FSections: THeaderSections;
    FImages: TCustomImageList;
    FPaintRect: TRect;
    FDown: Boolean;
    FDownPoint: TPoint;
    FMouseInControl: Boolean;
    
    FOnSectionClick: TCustomSectionNotifyEvent;
    FOnCreateSectionClass: TCustomHCCreateSectionClassEvent;
    procedure SetImages(const AValue: TCustomImageList);
    procedure SetSections(const AValue: THeaderSections);
    procedure UpdateSection(Index: Integer);
    procedure UpdateSections;
  protected
    function CreateSection: THeaderSection; virtual;
    function CreateSections: THeaderSections; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SectionClick(Section: THeaderSection); dynamic;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure UpdateState;
    class function GetControlClassDefaultSize: TPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure Click; override;
    function GetSectionAt(P: TPoint): Integer;
    procedure Paint; override;
    procedure PaintSection(Index: Integer); virtual;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property Sections: THeaderSections read FSections write SetSections;

    property OnSectionClick: TCustomSectionNotifyEvent read FOnSectionClick
      write FOnSectionClick;
    property OnCreateSectionClass: TCustomHCCreateSectionClassEvent read FOnCreateSectionClass
      write FOnCreateSectionClass;
  end;
  
  
  { THeaderControl }

  THeaderControl = class(TCustomHeaderControl)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderWidth;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Images;
    property Constraints;
    property Sections;
    property ShowHint;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    // events
    property OnContextPopup;
    property OnCreateSectionClass;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
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
    TListView,TStatusBar,TToolBar,TUpDown,TPageControl,TTabControl, THeaderControl]);
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
{$I headercontrol.inc}

end.
