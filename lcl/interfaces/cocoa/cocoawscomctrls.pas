unit CocoaWSComCtrls;

interface

{$mode delphi}
{$modeswitch objectivec1}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_TABCONTROL}
{.$DEFINE COCOA_DEBUG_LISTVIEW}

uses
  // RTL, FCL, LCL
  MacOSAll, CocoaAll,
  Classes, LCLType, SysUtils, LCLMessageGlue, LMessages,
  Controls, ComCtrls, Types, StdCtrls, LCLProc, Graphics, ImgList, Forms,
  Math,
  // WS
  WSComCtrls,
  // Cocoa WS
  CocoaPrivate, CocoaScrollers, CocoaWSScrollers, CocoaTabControls, CocoaUtils,
  CocoaWSCommon, CocoaTables, cocoa_extra, CocoaWSStdCtrls, CocoaGDIObjects, CocoaButtons;

type

  { TCocoaWSStatusBar }

  { TStatusBarCallback }

  TStatusBarCallback = class(TLCLCommonCallback, IStatusBarCallback, ICommonCallback)
    function GetBarsCount: Integer;
    function GetBarItem(idx: Integer; var txt: String; var width: Integer; var align: TAlignment): Boolean;
    procedure DrawPanel(idx: Integer; const r: TRect);
  end;

  TCocoaWSStatusBar = class(TWSStatusBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure PanelUpdate(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure SetPanelText(const AStatusBar: TStatusBar; PanelIndex: integer); override;
    class procedure Update(const AStatusBar: TStatusBar); override;
    //
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCocoaWSTabSheet }

  TCocoaWSTabSheet = class(TWSTabSheet)
  published
  end;

  { TLCLTabControlCallback }

  TLCLTabControlCallback = class(TLCLCommonCallback, ITabControlCallback)
    function shouldSelectTabViewItem(aTabIndex: Integer): Boolean;
    procedure willSelectTabViewItem(aTabIndex: Integer);
    procedure didSelectTabViewItem(aTabIndex: Integer);
  private
    procedure sengNotifyMsg(aTabIndex:Integer; aCode:Integer);
  end;

  { TCocoaWSCustomPage }

  TCocoaWSCustomPage = class(TWSCustomPage)
  public
    class function  GetCocoaTabPageFromHandle(AHandle: HWND): TCocoaTabPage;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetProperties(const ACustomPage: TCustomPage; ACocoaControl: NSTabViewItem);
    //
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: String); override;
    class function GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
  end;

  { TCocoaWSCustomTabControl }

  TCocoaWSCustomTabControl = class(TWSCustomTabControl)
  private
    class function LCLTabPosToNSTabStyle(AShowTabs: Boolean; ABorderWidth: Integer; ATabPos: TTabPosition): NSTabViewType;
  public
    class function  GetCocoaTabControlHandle(ATabControl: TCustomTabControl): TCocoaTabControl;
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;

    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;

    class procedure AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer); override;

    //class function GetNotebookMinTabHeight(const AWinControl: TWinControl): integer; override;
    //class function GetNotebookMinTabWidth(const AWinControl: TWinControl): integer; override;
    //class function GetPageRealIndex(const ATabControl: TCustomTabControl; AIndex: Integer): Integer; override;
    class function GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ATabControl: TCustomTabControl; const AIndex: Integer): TRect; override;
    class procedure SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer); override;
    class procedure SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean); override;

    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
      const AOldPos, ANewPos: Integer; const AChildren: TFPList); override;
  end;

  { TCocoaWSPageControl }

  TCocoaWSPageControl = class(TWSPageControl)
  published
  end;

  { TCocoaWSListViewHandler }

  TCocoaWSListViewHandler = class
  public
    // Column
    procedure ColumnDelete( const AIndex: Integer ); virtual; abstract;
    function  ColumnGetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; virtual; abstract;
    procedure ColumnInsert( const AIndex: Integer; const AColumn: TListColumn); virtual; abstract;
    procedure ColumnMove( const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); virtual; abstract;
    procedure ColumnSetAlignment( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); virtual; abstract;
    procedure ColumnSetAutoSize( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); virtual; abstract;
    procedure ColumnSetCaption( const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); virtual; abstract;
    procedure ColumnSetMaxWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); virtual; abstract;
    procedure ColumnSetMinWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); virtual; abstract;
    procedure ColumnSetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); virtual; abstract;
    procedure ColumnSetVisible( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); virtual; abstract;
    procedure ColumnSetSortIndicator( const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator); virtual; abstract;

    // Item
    procedure ItemDelete( const AIndex: Integer); virtual; abstract;
    function  ItemDisplayRect( const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; virtual; abstract;
    function  ItemGetChecked( const AIndex: Integer; const {%H-}AItem: TListItem): Boolean; virtual; abstract;
    function  ItemGetPosition( const AIndex: Integer): TPoint; virtual; abstract;
    function  ItemGetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; virtual; abstract; // returns True if supported
    procedure ItemInsert( const AIndex: Integer; const {%H-}AItem: TListItem); virtual; abstract;
    procedure ItemSetChecked( const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); virtual; abstract;
    procedure ItemSetImage( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); virtual; abstract;
    procedure ItemSetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); virtual; abstract;
    procedure ItemSetText( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); virtual; abstract;
    procedure ItemShow( const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); virtual; abstract;

    function GetFocused: Integer; virtual; abstract;
    function GetItemAt( x,y: integer): Integer; virtual; abstract;
    function GetSelCount: Integer; virtual; abstract;
    function GetSelection: Integer; virtual; abstract;
    function GetTopItem: Integer; virtual; abstract;
    function GetVisibleRowCount: Integer; virtual; abstract;

    procedure SelectAll( const AIsSet: Boolean); virtual; abstract;
    procedure SetDefaultItemHeight( const AValue: Integer); virtual; abstract;
    procedure SetImageList( const {%H-}AList: TListViewImageList; const {%H-}AValue: TCustomImageListResolution); virtual; abstract;
    procedure SetItemsCount( const Avalue: Integer); virtual; abstract;
    procedure SetOwnerData( const {%H-}AValue: Boolean); virtual; abstract;
    procedure SetProperty( const AProp: TListViewProperty; const AIsSet: Boolean); virtual; abstract;
    procedure SetScrollBars( const AValue: TScrollStyle); virtual; abstract;
    procedure SetSort( const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); virtual; abstract;
  end;

  { TLCLListViewCallback }

  TLCLListViewCallback = class(TLCLCommonCallback, IListViewCallback)
  public
    listView: TCustomListView;
    tempItemsCountDelta : Integer;

    isSetTextFromWS: Integer; // allows to suppress the notifation about text change
                              // when initiated by Cocoa itself.
    selectionIndexSet: NSMutableIndexSet;
    checkedIdx : NSMutableIndexSet;
    ownerData  : Boolean;

    constructor Create(AOwner: NSObject; ATarget: TWinControl; AHandleView: NSView); override;
    destructor Destroy; override;
    function ItemsCount: Integer;
    function GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean;
    function GetItemCheckedAt(ARow, ACol: Integer; var IsChecked: Integer): Boolean;
    function GetItemImageAt(ARow, ACol: Integer; var imgIdx: Integer): Boolean;
    function GetImageFromIndex(imgIdx: Integer): NSImage;
    procedure SetItemTextAt(ARow, ACol: Integer; const Text: String);
    procedure SetItemCheckedAt(ARow, ACol: Integer; IsChecked: Integer);
    function getItemStableSelection(ARow: Integer): Boolean;
    procedure selectionChanged(NewSel: Integer; Added, Removed: NSIndexSet);
    procedure selectOne(ARow: Integer; isSelected:Boolean );
    function shouldSelectionChange(NewSel: Integer): Boolean;
    procedure ColumnClicked(ACol: Integer);
    procedure DrawRow(rowidx: Integer; ctx: TCocoaContext; const r: TRect;
      state: TOwnerDrawState);
    procedure GetRowHeight(rowidx: Integer; var h: Integer);
  end;
  TLCLListViewCallBackClass = class of TLCLListViewCallback;

  { TCocoaListViewBackendControl }
  TCocoaListViewBackendControlProtocol = objcprotocol
    procedure backend_setCallback( cb: TLCLListViewCallback ); message 'backend_setCallback:';
    procedure backend_reloadData; message 'backend_reloadData';
    procedure backend_onInit( lclListView: TCustomListView );
      message 'backend_onInit:';
  end;

  { TCocoaListView }

  TCocoaListView = objcclass(NSView)
  private
    _viewStyle: TViewStyle;
    _scrollView: TCocoaScrollView;
    _backendControl: NSView; // NSTableView or NSCollectionView
    _WSHandler: TCocoaWSListViewHandler;
    _lclListView: TCustomListView;
    _needsCallLclInit: Boolean;
    _initializing: Boolean;
  private
    procedure createControls; message 'createControls';
    procedure releaseControls; message 'releaseControls';
    procedure initData; message 'initData';
  public
    callback: TLCLListViewCallback;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclContentView: NSView; override;
  public
    procedure setLclListView( lclListView: TCustomListView ); message 'setLclListView:';
    procedure setViewStyle( viewStyle: TViewStyle ); message 'setViewStyle:';
    function documentView: NSView; message 'documentView';
    function scrollView: TCocoaScrollView; message 'scrollView';
    function WSHandler: TCocoaWSListViewHandler; message 'WSHandler';
    procedure addSubview(aView: NSView); override;
    procedure setScrollView(aView: TCocoaScrollView); message 'setScrollView:';
    function initializing: Boolean; message 'isinitializing';
  end;

  { TCocoaWSCustomListView }

  TCocoaWSCustomListView = class(TWSCustomListView)
  private
    class function getWSHandler( const lclListView: TCustomListView ):
      TCocoaWSListViewHandler;
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    // Column
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; override;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); override;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); override;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); override;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); override;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); override;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); override;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); override;
    class procedure ColumnSetSortIndicator(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator); override;

    // Item
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); override;
    class function  ItemDisplayRect(const ALV: TCustomListView; const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    class function  ItemGetChecked(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem): Boolean; override;
    class function  ItemGetPosition(const ALV: TCustomListView; const AIndex: Integer): TPoint; override;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem); override;
    class procedure ItemSetChecked(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); override;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); override;
    //carbon//class function ItemSetPosition(const ALV: TCustomListView; const AIndex: Integer; const ANewPosition: TPoint): Boolean; override;*)
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); override;

    // LV
    //available in 10.7 only//class procedure BeginUpdate(const ALV: TCustomListView); override;
    //available in 10.7 only//class procedure EndUpdate(const ALV: TCustomListView); override;

    //class function GetBoundingRect(const ALV: TCustomListView): TRect; override;
    //carbon//class function GetDropTarget(const ALV: TCustomListView): Integer; override;
    class function GetFocused(const ALV: TCustomListView): Integer; override;
    //carbon//class function GetHoverTime(const ALV: TCustomListView): Integer; override;
    class function GetItemAt(const ALV: TCustomListView; x,y: integer): Integer; override;
    class function GetSelCount(const ALV: TCustomListView): Integer; override;
    class function GetSelection(const ALV: TCustomListView): Integer; override;
    class function GetTopItem(const ALV: TCustomListView): Integer; override;
    //class function GetViewOrigin(const ALV: TCustomListView): TPoint; override;
    class function GetVisibleRowCount(const ALV: TCustomListView): Integer; override;

    class procedure SelectAll(const ALV: TCustomListView; const AIsSet: Boolean); override;
    //carbon//class procedure SetAllocBy(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetDefaultItemHeight(const ALV: TCustomListView; const AValue: Integer); override;
    //carbon//class procedure SetHotTrackStyles(const ALV: TCustomListView; const AValue: TListHotTrackStyles); override;
    //carbon//class procedure SetHoverTime(const ALV: TCustomListView; const AValue: Integer); override;
    class procedure SetImageList(const ALV: TCustomListView; const {%H-}AList: TListViewImageList; const {%H-}AValue: TCustomImageListResolution); override;
    class procedure SetItemsCount(const ALV: TCustomListView; const Avalue: Integer); override;
    class procedure SetOwnerData(const ALV: TCustomListView; const {%H-}AValue: Boolean); override;
    class procedure SetProperty(const ALV: TCustomListView; const AProp: TListViewProperty; const AIsSet: Boolean); override;
    //class procedure SetProperties(const ALV: TCustomListView; const AProps: TListViewProperties); override;
    class procedure SetScrollBars(const ALV: TCustomListView; const AValue: TScrollStyle); override;
    class procedure SetSort(const ALV: TCustomListView; const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
    class function RestoreItemCheckedAfterSort(const ALV: TCustomListView): Boolean; override;
    (*class procedure SetViewOrigin(const ALV: TCustomListView; const AValue: TPoint); override;*)
    class procedure SetViewStyle(const ALV: TCustomListView; const AValue: TViewStyle); override;
  end;

  { TCocoaWSProgressBar }

  TCocoaWSProgressBar = class(TWSProgressBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure ApplyChanges(const AProgressBar: TCustomProgressBar); override;
    class procedure SetPosition(const AProgressBar: TCustomProgressBar; const NewPosition: integer); override;
    class procedure SetStyle(const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle); override;
  end;

  { TCocoaWSCustomUpDown }

  TCocoaWSCustomUpDown = class(TWSCustomUpDown)
  published
    class function CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetIncrement(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetMaxPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetMinPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetPosition(const AUpDown: TCustomUpDown; AValue: Double); override;
    class procedure SetWrap(const AUpDown: TCustomUpDown; ADoWrap: Boolean); override;
  end;

  { TCarbonWSUpDown }

  TCarbonWSUpDown = class(TWSUpDown)
  published
  end;

  { TCocoaWSToolButton }

  TCocoaWSToolButton = class(TWSToolButton)
  published
  end;

  { TCarbonWSToolBar }

  TCarbonWSToolBar = class(TWSToolBar)
  published
    //class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
  end;

  { TCocoaWSTrackBar }

  TCocoaWSTrackBar = class(TWSTrackBar)
  published
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle; override;
    class procedure ApplyChanges(const ATrackBar: TCustomTrackBar); override;
    class function  GetPosition(const ATrackBar: TCustomTrackBar): integer; override;
    class procedure SetPosition(const ATrackBar: TCustomTrackBar; const {%H-}NewPosition: integer); override;
    class procedure SetOrientation(const ATrackBar: TCustomTrackBar; const AOrientation: TTrackBarOrientation); override;
    class procedure SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer); override;
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCocoaWSCustomTreeView }

  TCocoaWSCustomTreeView = class(TWSCustomTreeView)
  published
  end;

  { TCocoaWSTreeView }

  TCocoaWSTreeView = class(TWSTreeView)
  published
  end;

implementation

uses
  CocoaCollectionView;

type
  TCustomListViewAccess = class(TCustomListView);

  { TCocoaWSListView_TableViewHandler }

  TCocoaWSListView_TableViewHandler = class(TCocoaWSListViewHandler)
  private
    _listView: TCocoaListView;
    _tableView: TCocoaTableListView;
  public
    constructor Create( listView: TCocoaListView );
    function getColumnFromIndex( const AIndex: Integer ): NSTableColumn;
    function getCallback: TLCLListViewCallback;
  public
    // Column
    procedure ColumnDelete( const AIndex: Integer ); override;
    function  ColumnGetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; override;
    procedure ColumnInsert( const AIndex: Integer; const AColumn: TListColumn); override;
    procedure ColumnMove( const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    procedure ColumnSetAlignment( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); override;
    procedure ColumnSetAutoSize( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); override;
    procedure ColumnSetCaption( const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); override;
    procedure ColumnSetMaxWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); override;
    procedure ColumnSetMinWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); override;
    procedure ColumnSetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); override;
    procedure ColumnSetVisible( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); override;
    procedure ColumnSetSortIndicator( const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator); override;

    // Item
    procedure ItemDelete( const AIndex: Integer); override;
    function  ItemDisplayRect( const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    function  ItemGetChecked( const AIndex: Integer; const {%H-}AItem: TListItem): Boolean; override;
    function  ItemGetPosition( const AIndex: Integer): TPoint; override;
    function  ItemGetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    procedure ItemInsert( const AIndex: Integer; const {%H-}AItem: TListItem); override;
    procedure ItemSetChecked( const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); override;
    procedure ItemSetImage( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); override;
    procedure ItemSetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    procedure ItemSetText( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    procedure ItemShow( const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); override;

    function GetFocused: Integer; override;
    function GetItemAt( x,y: integer): Integer; override;
    function GetSelCount: Integer; override;
    function GetSelection: Integer; override;
    function GetTopItem: Integer; override;
    function GetVisibleRowCount: Integer; override;

    procedure SelectAll( const AIsSet: Boolean); override;
    procedure SetDefaultItemHeight( const AValue: Integer); override;
    procedure SetImageList( const {%H-}AList: TListViewImageList; const {%H-}AValue: TCustomImageListResolution); override;
    procedure SetItemsCount( const Avalue: Integer); override;
    procedure SetOwnerData( const {%H-}AValue: Boolean); override;
    procedure SetProperty( const AProp: TListViewProperty; const AIsSet: Boolean); override;
    procedure SetScrollBars( const AValue: TScrollStyle); override;
    procedure SetSort( const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
  end;

  { TCocoaWSListView_CollectionViewHandler }

  TCocoaWSListView_CollectionViewHandler = class(TCocoaWSListViewHandler)
  private
    _listView: TCocoaListView;
    _collectionView: TCocoaCollectionView;
  private
    function getCallback: TLCLListViewCallback;
    procedure doReloadDataAfterDelete( AIndex: PtrInt);
  public
    constructor Create( listView: TCocoaListView );
  public
    // Column
    procedure ColumnDelete( const AIndex: Integer ); override;
    function  ColumnGetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn): Integer; override;
    procedure ColumnInsert( const AIndex: Integer; const AColumn: TListColumn); override;
    procedure ColumnMove( const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); override;
    procedure ColumnSetAlignment( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAlignment: TAlignment); override;
    procedure ColumnSetAutoSize( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AAutoSize: Boolean); override;
    procedure ColumnSetCaption( const AIndex: Integer; const {%H-}AColumn: TListColumn; const ACaption: String); override;
    procedure ColumnSetMaxWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMaxWidth: Integer); override;
    procedure ColumnSetMinWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AMinWidth: integer); override;
    procedure ColumnSetWidth( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AWidth: Integer); override;
    procedure ColumnSetVisible( const AIndex: Integer; const {%H-}AColumn: TListColumn; const AVisible: Boolean); override;
    procedure ColumnSetSortIndicator( const AIndex: Integer; const AColumn: TListColumn; const ASortIndicator: TSortIndicator); override;

    // Item
    procedure ItemDelete( const AIndex: Integer); override;
    function  ItemDisplayRect( const AIndex, ASubItem: Integer; ACode: TDisplayCode): TRect; override;
    function  ItemGetChecked( const AIndex: Integer; const {%H-}AItem: TListItem): Boolean; override;
    function  ItemGetPosition( const AIndex: Integer): TPoint; override;
    function  ItemGetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; out AIsSet: Boolean): Boolean; override; // returns True if supported
    procedure ItemInsert( const AIndex: Integer; const {%H-}AItem: TListItem); override;
    procedure ItemSetChecked( const AIndex: Integer; const {%H-}AItem: TListItem; const AChecked: Boolean); override;
    procedure ItemSetImage( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex, {%H-}AImageIndex: Integer); override;
    procedure ItemSetState( const AIndex: Integer; const {%H-}AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); override;
    procedure ItemSetText( const AIndex: Integer; const {%H-}AItem: TListItem; const {%H-}ASubIndex: Integer; const {%H-}AText: String); override;
    procedure ItemShow( const AIndex: Integer; const {%H-}AItem: TListItem; const PartialOK: Boolean); override;

    function GetFocused: Integer; override;
    function GetItemAt( x,y: integer): Integer; override;
    function GetSelCount: Integer; override;
    function GetSelection: Integer; override;
    function GetTopItem: Integer; override;
    function GetVisibleRowCount: Integer; override;

    procedure SelectAll( const AIsSet: Boolean); override;
    procedure SetDefaultItemHeight( const AValue: Integer); override;
    procedure SetImageList( const {%H-}AList: TListViewImageList; const {%H-}AValue: TCustomImageListResolution); override;
    procedure SetItemsCount( const Avalue: Integer); override;
    procedure SetOwnerData( const {%H-}AValue: Boolean); override;
    procedure SetProperty( const AProp: TListViewProperty; const AIsSet: Boolean); override;
    procedure SetScrollBars( const AValue: TScrollStyle); override;
    procedure SetSort( const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
  end;

type

  { TUpdownCommonCallback }

  TUpdownCommonCallback = class(TLCLCommonCallback, IStepperCallback)
    procedure BeforeChange(var Allowed: Boolean);
    procedure Change(NewValue: Double; isUpPressed: Boolean; var Allowed: Boolean);
    procedure UpdownClick(isUpPressed: Boolean);
  end;

type
  TAccessUpDown = class(TCustomUpDown);

{ TCocoaWSListView_TableViewHandler }

constructor TCocoaWSListView_TableViewHandler.Create(
   listView: TCocoaListView );
begin
  _listView:= listView;
  _tableView:= TCocoaTableListView(listView.documentView);
end;

function TCocoaWSListView_TableViewHandler.getColumnFromIndex(
  const AIndex: Integer): NSTableColumn;
begin
  Result:= nil;
  if (AIndex < 0) or (AIndex >= _tableView.tableColumns.count) then
    Exit;
  Result:= NSTableColumn( _tableView.tableColumns.objectAtIndex(AIndex) );
end;

function TCocoaWSListView_TableViewHandler.getCallback: TLCLListViewCallback;
begin
  Result:= TLCLListViewCallback( _tableView.lclGetCallback.GetCallbackObject );
end;

procedure TCocoaWSListView_TableViewHandler.ColumnDelete(
  const AIndex: Integer);
var
  cocoaColumn: NSTableColumn;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if Assigned(cocoaColumn) then
    _tableView.removeTableColumn( cocoaColumn );
end;

function TCocoaWSListView_TableViewHandler.ColumnGetWidth(
  const AIndex: Integer; const AColumn: TListColumn): Integer;
var
  cocoaColumn: NSTableColumn;
begin
  Result:= 0;
  cocoaColumn:= getColumnFromIndex( AIndex );
  if Assigned(cocoaColumn) then
    Result:= Round( cocoaColumn.width );
end;

procedure TCocoaWSListView_TableViewHandler.ColumnInsert(
  const AIndex: Integer; const AColumn: TListColumn);
var
  cocoaColumn: NSTableColumn;
  cocoaTitle: NSString;
begin
  if (AIndex < 0) or (AIndex > _tableView.tableColumns.count) then
    Exit;
  cocoaTitle := NSStringUTF8(AColumn.Caption);
  cocoaColumn := NSTableColumn.alloc.initWithIdentifier(cocoaTitle);
  cocoaColumn.headerCell.setStringValue(cocoaTitle);
  cocoaColumn.setResizingMask(NSTableColumnUserResizingMask);
  _tableView.addTableColumn(cocoaColumn);
  cocoaColumn.release;
  cocoaTitle.release;
end;

procedure TCocoaWSListView_TableViewHandler.ColumnMove(const AOldIndex,
  ANewIndex: Integer; const AColumn: TListColumn);
var
  columnCount: NSUInteger;
begin
  columnCount:= _tableView.tableColumns.count;
  if columnCount <= 1 then
    Exit;
  if (AOldIndex < 0) or (AOldIndex >= columnCount) then
    Exit;
  if (ANewIndex < 0) or (ANewIndex >= columnCount) then
    Exit;
  _tableView.moveColumn_toColumn(AOldIndex, ANewIndex);
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetAlignment(
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
var
  cocoaColumn: NSTableColumn;
const
  txtAlign : array[TAlignment] of NSTextAlignment = (
    NSLeftTextAlignment, NSRightTextAlignment, NSCenterTextAlignment
  );
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;
  _tableView.lclSetColumnAlign(cocoaColumn, txtAlign[AAlignment]);
  _tableView.setNeedsDisplayInRect(_tableView.rectOfColumn(AIndex));
  _tableView.headerView.setNeedsDisplayInRect( _tableView.headerView.headerRectOfColumn(AIndex) );
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetAutoSize(
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
var
  cocoaColumn: NSTableColumn;
  mask: NSUInteger;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;
  if AAutoSize then
    mask := NSTableColumnAutoresizingMask or NSTableColumnUserResizingMask
  else
    mask := NSTableColumnUserResizingMask;
  cocoaColumn.setResizingMask(mask);
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetCaption(
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
var
  cocoaColumn: NSTableColumn;
  cocoaTitle: NSString;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;

  cocoaTitle := NSStringUtf8(ACaption);
  if cocoaColumn.respondsToSelector(ObjCSelector('setTitle:')) then
    cocoaColumn.setTitle(cocoaTitle)
  else
    cocoaColumn.headerCell.setStringValue(cocoaTitle);

  {$ifdef BOOLFIX}
  _tableView.headerView.setNeedsDisplay__(Ord(true)); // forces the newly set Value (even for setTitle!)
  {$else}
  _tableView.headerView.setNeedsDisplay_(true); // forces the newly set Value (even for setTitle!)
  {$endif}
  cocoaTitle.release;
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetMaxWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
var
  cocoaColumn: NSTableColumn;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;

  if AMaxWidth <= 0 then
    cocoaColumn.setMaxWidth($FFFFFFFF)
  else
    cocoaColumn.setMaxWidth(AMaxWidth);
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetMinWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
var
  cocoaColumn: NSTableColumn;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;

  cocoaColumn.setMinWidth(AMinWidth);
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
var
  cocoaColumn: NSTableColumn;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;

  cocoaColumn.setWidth(AWidth);
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetVisible(
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
var
  cocoaColumn: NSTableColumn;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;

  {$ifdef BOOLFIX}
  cocoaColumn.setHidden_(Ord(not AVisible));
  {$else}
  cocoaColumn.setHidden(not AVisible);
  {$endif}
end;

procedure TCocoaWSListView_TableViewHandler.ColumnSetSortIndicator(
  const AIndex: Integer; const AColumn: TListColumn;
  const ASortIndicator: TSortIndicator);
var
  cocoaColumn: NSTableColumn;
begin
  cocoaColumn:= getColumnFromIndex( AIndex );
  if NOT Assigned(cocoaColumn) then
    Exit;

  case ASortIndicator of
    siNone:
      _tableView.setIndicatorImage_inTableColumn(nil, cocoaColumn);
    siAscending:
      _tableView.setIndicatorImage_inTableColumn(
        NSImage.imageNamed(NSSTR('NSAscendingSortIndicator')),
        cocoaColumn);
    siDescending:
      _tableView.setIndicatorImage_inTableColumn(
        NSImage.imageNamed(NSSTR('NSDescendingSortIndicator')),
        cocoaColumn);
  end;
end;

procedure TCocoaWSListView_TableViewHandler.ItemDelete(const AIndex: Integer
  );
var
  lclcb : TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.tempItemsCountDelta:= -1;
  lclcb.checkedIdx.shiftIndexesStartingAtIndex_by(AIndex, -1);
  _tableView.lclInsDelRow(AIndex, false);
  lclcb.tempItemsCountDelta := 0;
end;

function TCocoaWSListView_TableViewHandler.ItemDisplayRect(const AIndex,
  ASubItem: Integer; ACode: TDisplayCode): TRect;
begin
  LCLGetItemRect(_tableView, AIndex, ASubItem, Result);
  case ACode of
    drLabel: Result:= _tableView.lclGetLabelRect(AIndex, ASubItem, Result);
    drIcon:  Result:= _tableView.lclGetIconRect(AIndex, ASubItem, Result);
  end;
end;

function TCocoaWSListView_TableViewHandler.ItemGetChecked(
  const AIndex: Integer; const AItem: TListItem): Boolean;
var
  lclcb : TLCLListViewCallback;
begin
  Result:= False;
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  Result := lclcb.checkedIdx.containsIndex(AIndex);
end;

function TCocoaWSListView_TableViewHandler.ItemGetPosition(
  const AIndex: Integer): TPoint;
var
  rect: NSRect;
begin
  rect:= _tableView.rectOfRow(AIndex);
  Result.X := Round(rect.origin.X);
  Result.Y := Round(_listView.scrollView.frame.size.height - rect.origin.Y);
end;

function TCocoaWSListView_TableViewHandler.ItemGetState(
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
begin
  Result:= false;
  case AState of
    lisSelected: begin
      Result:= (AIndex>=0) and (AIndex <= _tableView.numberOfRows);
      AIsSet:= _tableView.isRowSelected(AIndex);
    end;
  end;
end;

procedure TCocoaWSListView_TableViewHandler.ItemInsert(
  const AIndex: Integer; const AItem: TListItem);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.checkedIdx.shiftIndexesStartingAtIndex_by(AIndex, 1);
  _tableView.lclInsDelRow(AIndex, true);
  _tableView.sizeToFit();
end;

procedure TCocoaWSListView_TableViewHandler.ItemSetChecked(
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  if AChecked and not lclcb.checkedIdx.containsIndex(AIndex) then begin
    lclcb.checkedIdx.addIndex(AIndex);
    _tableView.reloadDataForRow_column(AIndex, 0);
  end else if not AChecked and lclcb.checkedIdx.containsIndex(AIndex) then begin
    lclcb.checkedIdx.removeIndex(AIndex);
    _tableView.reloadDataForRow_column(AIndex, 0);
  end;
end;

procedure TCocoaWSListView_TableViewHandler.ItemSetImage(
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
begin
  _tableView.reloadDataForRow_column(AIndex, ASubIndex);
end;

procedure TCocoaWSListView_TableViewHandler.ItemSetState(
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
  row: Integer;
  isSel: Boolean;
begin
  row := AItem.Index;
  if (row < 0) or (row >= _tableView.numberOfRows) then Exit;

  case AState of
    lisSelected:
    begin
      isSel := _tableView.selectedRowIndexes.containsIndex(row);
      if AIsSet and not isSel then
        _tableView.selectRowIndexes_byExtendingSelection(NSIndexSet.indexSetWithIndex(row),false)
      else if not AIsSet and isSel then
        _tableView.deselectRow(row);
    end;
  end;
end;

procedure TCocoaWSListView_TableViewHandler.ItemSetText(
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
begin
  _tableView.reloadDataForRow_column(AIndex, ASubIndex);
end;

procedure TCocoaWSListView_TableViewHandler.ItemShow(const AIndex: Integer;
  const AItem: TListItem; const PartialOK: Boolean);
begin
  _tableView.scrollRowToVisible(AItem.Index);
end;

function TCocoaWSListView_TableViewHandler.GetFocused: Integer;
begin
  Result := _tableView.selectedRow;
end;

function TCocoaWSListView_TableViewHandler.GetItemAt(x, y: integer
  ): Integer;
begin
  Result:= LCLCoordToRow(_tableView, x,y);
end;

function TCocoaWSListView_TableViewHandler.GetSelCount: Integer;
begin
  Result:= _tableView.selectedRowIndexes.count;
end;

function TCocoaWSListView_TableViewHandler.GetSelection: Integer;
begin
  Result:= _tableView.selectedRow;
end;

function TCocoaWSListView_TableViewHandler.GetTopItem: Integer;
begin
  Result:= LCLGetTopRow( _tableView );
end;

function TCocoaWSListView_TableViewHandler.GetVisibleRowCount: Integer;
var
  rows: NSRange;
begin
  rows := _tableView.rowsInRect(_tableView.visibleRect());
  Result := rows.length;
end;

procedure TCocoaWSListView_TableViewHandler.SelectAll(const AIsSet: Boolean
  );
begin
  if AIsSet then
    _tableView.selectAll(_tableView)
  else
    _tableView.deselectAll(_tableView);
end;

procedure TCocoaWSListView_TableViewHandler.SetDefaultItemHeight(
  const AValue: Integer);
begin
  if AValue > 0 then
    _tableView.setRowHeight(AValue);
  // setRowSizeStyle could be used here but is available only in 10.7+
end;

procedure TCocoaWSListView_TableViewHandler.SetImageList(
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
var
  spacing: NSSize;
begin
  _tableView.lclSetImagesInCell(Assigned(AValue));

  if NOT Assigned(AValue) then
    Exit;
  if AValue.Height < _tableView.rowHeight-2 then
    Exit;
  spacing:= _tableView.intercellSpacing;
  spacing.height:= _tableView.rowHeight / 3 + 2;
  _tableView.setIntercellSpacing( spacing );
end;

procedure TCocoaWSListView_TableViewHandler.SetItemsCount(
  const Avalue: Integer);
begin
  _tableView.noteNumberOfRowsChanged();
end;

procedure TCocoaWSListView_TableViewHandler.SetOwnerData(
  const AValue: Boolean);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.ownerData := AValue;
  if lclcb.ownerData then
    lclcb.checkedIdx.removeAllIndexes; // releasing memory
end;

procedure TCocoaWSListView_TableViewHandler.SetProperty(
  const AProp: TListViewProperty; const AIsSet: Boolean);
const
  GridStyle : array [boolean] of NSUInteger = (
    NSTableViewGridNone,
    NSTableViewSolidHorizontalGridLineMask or NSTableViewSolidVerticalGridLineMask
  );
begin
  case AProp of
    {lvpAutoArrange,}
    lvpCheckboxes: _tableView.lclSetFirstColumCheckboxes(AIsSet);
   // lvpColumnClick: lTableLV.setAllowsColumnSelection(AIsSet);
  {  lvpFlatScrollBars,
    lvpFullDrag,}
    lvpGridLines: _tableView.setGridStyleMask(GridStyle[AIsSet]);
    {lvpHideSelection,
    lvpHotTrack,}
    lvpMultiSelect: _tableView.setAllowsMultipleSelection(AIsSet);
    {lvpOwnerDraw,}
    lvpReadOnly: _tableView.readOnly := AIsSet;
  {  lvpRowSelect,}
    lvpShowColumnHeaders:
      if (AIsSet <> Assigned(_tableView.headerView)) then
      begin
        if AIsSet then _tableView.setHeaderView ( NSTableHeaderView.alloc.init.autorelease )
        else _tableView.setHeaderView(nil);
      end;
  {  lvpShowWorkAreas,
    lvpWrapText,
    lvpToolTips}
  end;
end;

procedure TCocoaWSListView_TableViewHandler.SetScrollBars(
  const AValue: TScrollStyle);
begin
  ScrollViewSetScrollStyles(_listView.scrollView, AValue);

  {$ifdef BOOLFIX}
  _listView.setNeedsDisplay__(Ord(true));
  {$else}
  _listView.setNeedsDisplay_(true);
  {$endif}

  {$ifdef BOOLFIX}
  _tableView.setNeedsDisplay__(Ord(true));
  {$else}
  _tableView.setNeedsDisplay_(true);
  {$endif}
end;

procedure TCocoaWSListView_TableViewHandler.SetSort(const AType: TSortType;
  const AColumn: Integer; const ASortDirection: TSortDirection);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  if Assigned(lclcb.checkedIdx) then
    lclcb.checkedIdx.removeAllIndexes;
  _tableView.reloadData();
  { //todo:
    lNSColumn.setSortDescriptorPrototype(
    NSSortDescriptor.sortDescriptorWithKey_ascending_selector(
      NSSTR('none'),
      ASortDirection=sdAscending,
      objcselector('none:')
    )
  );}
end;

{ TCocoaWSListView_CollectionViewHandler }

constructor TCocoaWSListView_CollectionViewHandler.Create(
   listView: TCocoaListView );
begin
  _listView:= listView;
  _collectionView:= TCocoaCollectionView(listView.documentView);
end;

function TCocoaWSListView_CollectionViewHandler.getCallback: TLCLListViewCallback;
begin
  Result:= TLCLListViewCallback( _collectionView.lclGetCallback.GetCallbackObject );
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnDelete(
  const AIndex: Integer);
begin
end;

function TCocoaWSListView_CollectionViewHandler.ColumnGetWidth(
  const AIndex: Integer; const AColumn: TListColumn): Integer;
begin
  Result:= -1;
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnInsert(
  const AIndex: Integer; const AColumn: TListColumn);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnMove(const AOldIndex,
  ANewIndex: Integer; const AColumn: TListColumn);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetAlignment(
  const AIndex: Integer; const AColumn: TListColumn;
  const AAlignment: TAlignment);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetAutoSize(
  const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetCaption(
  const AIndex: Integer; const AColumn: TListColumn; const ACaption: String);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetMaxWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetMinWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetWidth(
  const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetVisible(
  const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ColumnSetSortIndicator(
  const AIndex: Integer; const AColumn: TListColumn;
  const ASortIndicator: TSortIndicator);
begin
end;

// when LCL call ItemDelete, the Item isn't Deleted at LCL
// delayed reload is necessary
procedure TCocoaWSListView_CollectionViewHandler.ItemDelete(
  const AIndex: Integer);
begin
  Application.QueueAsyncCall( doReloadDataAfterDelete, AIndex );
end;

procedure TCocoaWSListView_CollectionViewHandler.doReloadDataAfterDelete( AIndex: PtrInt);
var
  lclcb : TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex+1, -1 );
  _collectionView.reloadData;
end;

function TCocoaWSListView_CollectionViewHandler.ItemDisplayRect(const AIndex,
  ASubItem: Integer; ACode: TDisplayCode): TRect;
var
  item: NSCollectionViewItem;
  frame: NSRect;
begin
  Result:= Rect( 0, 0, 0, 0 );
  item:= _collectionView.itemAtIndex( AIndex );
  if NOT Assigned(item) then
    Exit;

  case ACode of
    drLabel:
      begin
        frame:= item.textField.frame;
        frame:= item.view.convertRect_toView( frame, _collectionView );
        frame.origin.x:= frame.origin.x + 1;
        frame.size.width:= frame.size.width - 2;
        frame.size.height:= frame.size.height + 8;
      end;
    drIcon:
      begin
        frame:= item.imageView.frame;
        frame:= item.view.convertRect_toView( frame, _collectionView );
      end
    else
      frame:= item.view.frame;
  end;

  Result:= NSRectToRect( frame );
end;

function TCocoaWSListView_CollectionViewHandler.ItemGetChecked(
  const AIndex: Integer; const AItem: TListItem): Boolean;
begin
  Result:= False;
end;

function TCocoaWSListView_CollectionViewHandler.ItemGetPosition(
  const AIndex: Integer): TPoint;
var
  rect: TRect;
begin
  rect:= self.ItemDisplayRect( AIndex, 0, drBounds );
  Result:= rect.TopLeft;
end;

function TCocoaWSListView_CollectionViewHandler.ItemGetState(
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
var
  lclcb : TLCLListViewCallback;
begin
  Result:= false;
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  case AState of
    lisSelected: begin
      Result:= (AIndex>=0) and (AIndex < _collectionView.numberOfItemsInSection(0));
      AIsSet:= lclcb.getItemStableSelection( AIndex );
    end;
  end;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemInsert(
  const AIndex: Integer; const AItem: TListItem);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= self.getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  if TCocoaListView(lclcb.Owner).initializing then
    Exit;

  lclcb.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetChecked(
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetImage(
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
begin
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetState(
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= self.getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  case AState of
    lisSelected: begin
      if lclcb.getItemStableSelection(AIndex) <> AIsSet then begin
        _collectionView.selectOneItemByIndex( AIndex, AIsSet );
        _collectionView.redrawVisibleItems;
      end;
    end;
  end;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemSetText(
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
begin
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.ItemShow(
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  indexPaths: NSSet;
begin
  indexPaths:= CocoaCollectionView.indexPathsWithOneIndex( _collectionView, AIndex );
  _collectionView.scrollToItemsAtIndexPaths_scrollPosition(
    indexPaths, NSCollectionViewScrollPositionTop );
end;

// what is the function?
// never be called ???
function TCocoaWSListView_CollectionViewHandler.GetFocused: Integer;
begin
  Result:= self.GetSelection;
end;

function TCocoaWSListView_CollectionViewHandler.GetItemAt(x, y: integer
  ): Integer;
var
  cocoaPoint: NSPoint;
  indexPath: NSIndexPath;
begin
  Result:= -1;
  cocoaPoint.x:= x;
  cocoaPoint.y:= y;
  indexPath:= _collectionView.indexPathForItemAtPoint( cocoaPoint );
  if Assigned(indexPath) then
    Result:= indexPath.item;
end;

function TCocoaWSListView_CollectionViewHandler.GetSelCount: Integer;
begin
  Result:= _collectionView.selectionIndexPaths.count;
end;

function TCocoaWSListView_CollectionViewHandler.GetSelection: Integer;
var
  lclListView: TCustomListView;
  lclItem: TListItem;
begin
  Result:= -1;
  lclListView:= TCustomListView(_collectionView.lclGetTarget);
  if Assigned(lclListView) then begin
    lclItem:= lclListView.LastSelected;
    if Assigned(lclItem) then
      Result:= lclItem.Index;
  end;
end;

function TCocoaWSListView_CollectionViewHandler.GetTopItem: Integer;
var
  items: NSArray;
  item: NSCollectionViewItem;
begin
  Result:= -1;
  items:= CocoaCollectionView.realVisibleItems( _collectionView );
  if items.count > 0 then begin
    item:= NSCollectionViewItem(items.firstObject);
    Result:= _collectionView.indexPathForItem(item).item;
  end;
end;

function TCocoaWSListView_CollectionViewHandler.GetVisibleRowCount: Integer;
begin
  Result:= CocoaCollectionView.realVisibleItems(_collectionView).count;
end;

procedure TCocoaWSListView_CollectionViewHandler.SelectAll(const AIsSet: Boolean
  );
begin
  if AIsSet then
    _collectionView.selectAll( _collectionView )
  else
    _collectionView.deselectAll( _collectionView );
end;

procedure TCocoaWSListView_CollectionViewHandler.SetDefaultItemHeight(
  const AValue: Integer);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.SetImageList(
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
var
  iconSize: NSSize;
begin
  if AList=lvilState then
    Exit;

  iconSize.Width:= AValue.Width;
  iconSize.Height:= AValue.Height;
  _collectionView.updateItemSize( iconSize );
end;

procedure TCocoaWSListView_CollectionViewHandler.SetItemsCount(
  const Avalue: Integer);
begin
  _collectionView.reloadData;
end;

procedure TCocoaWSListView_CollectionViewHandler.SetOwnerData(
  const AValue: Boolean);
begin

end;

procedure TCocoaWSListView_CollectionViewHandler.SetProperty(
  const AProp: TListViewProperty; const AIsSet: Boolean);
begin
  case AProp of
    {lvpHideSelection,
    lvpHotTrack,}
    lvpMultiSelect: _collectionView.setAllowsMultipleSelection(AIsSet);
    {lvpOwnerDraw,
    lvpReadOnly:
    lvpShowWorkAreas,
    lvpWrapText,
    lvpToolTips}
  end;
end;

// scrollBars auto handled by NSCollectionView
procedure TCocoaWSListView_CollectionViewHandler.SetScrollBars(
  const AValue: TScrollStyle);
begin
end;

procedure TCocoaWSListView_CollectionViewHandler.SetSort(
  const AType: TSortType; const AColumn: Integer;
  const ASortDirection: TSortDirection);
var
  lclcb : TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.selectionIndexSet.removeAllIndexes;
  _collectionView.reloadData();
end;

{ TUpdownCommonCallback }

procedure TUpdownCommonCallback.BeforeChange(var Allowed: Boolean);
begin
  if Assigned( TAccessUpDown(Target).OnChanging ) then
    TAccessUpDown(Target).OnChanging(Target, Allowed);
end;

procedure TUpdownCommonCallback.Change(NewValue: Double; isUpPressed: Boolean;
  var Allowed: Boolean);
const
  UpDownDir : array [Boolean] of TUpDownDirection = (updUp, updDown);
begin
  if Assigned( TAccessUpDown(Target).OnChangingEx ) then
    TAccessUpDown(Target).OnChangingEx(Target, Allowed,
      Round(NewValue), UpDownDir[isUpPressed]);
end;

procedure TUpdownCommonCallback.UpdownClick(isUpPressed: Boolean);
const
  UpDownBtn : array [Boolean] of TUDBtnType = (btPrev, btNext);
begin
  TAccessUpDown(Target).Position := NSStepper(Owner).intValue;
  if Assigned( TAccessUpDown(Target).OnClick ) then
    TAccessUpDown(Target).OnClick( Target, UpDownBtn[isUpPressed]);
end;

{ TCocoaWSCustomUpDown }

class function TCocoaWSCustomUpDown.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaStepper;
begin
  lResult := TCocoaStepper.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TUpdownCommonCallback.Create(lResult, AWinControl);
    //small constrol size looks like carbon
    //lResult.setControlSize(NSSmallControlSize);
    lResult.setTarget(lResult);
    lResult.setAction(objcselector('stepperAction:'));

  end;
  Result := TLCLHandle(lResult);
end;

class procedure TCocoaWSCustomUpDown.SetMinPosition(
  const AUpDown: TCustomUpDown; AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setMinValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetMaxPosition(
  const AUpDown: TCustomUpDown; AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setMaxValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetPosition(const AUpDown: TCustomUpDown;
  AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).lastValue := AValue;
  TCocoaStepper(AUpDown.Handle).setDoubleValue(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetIncrement(const AUpDown: TCustomUpDown;
  AValue: Double);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setIncrement(AValue);
end;

class procedure TCocoaWSCustomUpDown.SetWrap(const AUpDown: TCustomUpDown;
  ADoWrap: Boolean);
begin
  if not Assigned(AUpDown) or not AUpDown.HandleAllocated then Exit;
  TCocoaStepper(AUpDown.Handle).setValueWraps(ADoWrap);
end;

{ TStatusBarCallback }

function TStatusBarCallback.GetBarsCount: Integer;
begin
  if  TStatusBar(Target).SimplePanel
    then Result := 1
    else Result := TStatusBar(Target).Panels.Count;
end;

function TStatusBarCallback.GetBarItem(idx: Integer; var txt: String;
  var width: Integer; var align: TAlignment): Boolean;
var
  sb : TStatusBar;
begin
  sb := TStatusBar(Target);
  if sb.SimplePanel then begin
    Result := idx = 0;
    if not Result then Exit;
    txt := sb.SimpleText;
    width := sb.Width;
    align := taLeftJustify; // todo: RTL?
  end else begin
    Result := (idx >= 0) and (idx < sb.Panels.Count);
    if not Result then Exit;
    if sb.Panels[idx].Style = psOwnerDraw
      then txt := ''
      else txt := sb.Panels[idx].Text;
    width := sb.Panels[idx].Width;
    align := sb.Panels[idx].Alignment;
  end;
end;

procedure TStatusBarCallback.DrawPanel(idx: Integer; const r: TRect);
var
  sb  : TStatusBar;
  msg : TLMDrawItems;
  ctx : TCocoaContext;
  dr  : TDrawItemStruct;
  fr  : TRect;
  sv  : Integer;
begin
  sb := TStatusBar(Target);
  if sb.SimplePanel then Exit;
  if (idx<0) or (idx >= sb.Panels.Count) then Exit;
  if sb.Panels[idx].Style <> psOwnerDraw then Exit;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  sv := ctx.SaveDC;
  try
    FillChar(msg, sizeof(msg), 0);
    FillChar(dr, sizeof(dr), 0);
    msg.Ctl := Target.Handle;
    msg.Msg := LM_DRAWITEM;
    msg.DrawItemStruct := @dr;
    dr.itemID := idx;
    dr._hDC := HDC(ctx);
    dr.rcItem := r;
    fr := NSView(Owner).lclFrame;
    ctx.InitDraw(fr.Right-fr.Left, fr.Bottom-fr.Top);
    LCLMessageGlue.DeliverMessage(Target, msg);
  finally
    ctx.RestoreDC(sv);
    ctx.Free;
  end;
end;

{ TLCLTabControlCallback }

function TLCLTabControlCallback.shouldSelectTabViewItem(aTabIndex: Integer): Boolean;
begin
  Result:= NOT TTabControl(Target).Dragging;
end;

procedure TLCLTabControlCallback.sengNotifyMsg(aTabIndex:Integer; aCode:Integer);
var
  Msg: TLMNotify;
  Hdr: TNmHdr;
begin
  if aTabIndex<0 then exit;

  FillChar(Msg, SizeOf(Msg), 0);
  Msg.Msg := LM_NOTIFY;
  FillChar(Hdr, SizeOf(Hdr), 0);

  Hdr.hwndFrom := Target.Handle;
  Hdr.Code := aCode;
  Hdr.idFrom := TTabControl(Target).TabToPageIndex(ATabIndex);
  Msg.NMHdr := @Hdr;
  Msg.Result := 0;
  LCLMessageGlue.DeliverMessage(Target, Msg);
end;

procedure TLCLTabControlCallback.willSelectTabViewItem(aTabIndex: Integer);
begin
  sengNotifyMsg(aTabIndex, TCN_SELCHANGING);
end;

procedure TLCLTabControlCallback.didSelectTabViewItem(aTabIndex: Integer);
begin
  sengNotifyMsg(aTabIndex, TCN_SELCHANGE);
end;

{ TCocoaWSStatusBar }

class function TCocoaWSStatusBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaStatusBar;
  cell   : NSButtonCell;
  cb : TStatusBarCallback;
begin
  Result := 0;
  lResult := TCocoaStatusBar.alloc.lclInitWithCreateParams(AParams);
  if not Assigned(lResult) then Exit;
  Result := TLCLHandle(lResult);

  cb := TStatusBarCallback.Create(lResult, AWinControl);
  lResult.callback := cb;
  lResult.barcallback := cb;
  cb.BlockCocoaUpDown := true;
  //lResult.StatusBar := TStatusBar(AWinControl);

  //todo: get rid of Cells and replace them with views!
  cell:=NSButtonCell(NSButtonCell.alloc).initTextCell(nil);
  // NSSmallSquareBezelStyle aka "Gradient button", is the best looking
  // candidate for the status bar panel. Could be changed to any NSCell class
  // since CocoaStatusBar doesn't suspect any specific cell type.
  cell.setBezelStyle(NSSmallSquareBezelStyle);
  cell.setFont( NSFont.systemFontOfSize( NSFont.smallSystemFontSize ));

  cell.setLineBreakMode(NSLineBreakByClipping);
  //cell.setLineBreakMode(NSLineBreakByTruncatingTail);

  lResult.panelCell := cell;
end;

class procedure TCocoaWSStatusBar.PanelUpdate(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  // todo: can make more effecient
  Update(AStatusBar);
end;

class procedure TCocoaWSStatusBar.SetPanelText(const AStatusBar: TStatusBar;
  PanelIndex: integer);
begin
  Update(AStatusBar);
end;

class procedure TCocoaWSStatusBar.Update(const AStatusBar: TStatusBar);
begin
  if not Assigned(AStatusBar) or not (AStatusBar.HandleAllocated) then Exit;
  {$ifdef BOOLFIX}
  TCocoaStatusBar(AStatusBar.Handle).setNeedsDisplay__(Ord(true));
  {$else}
  TCocoaStatusBar(AStatusBar.Handle).setNeedsDisplay_(true);
  {$endif}
end;

class procedure TCocoaWSStatusBar.GetPreferredSize(const AWinControl: TWinControl;
  var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := STATUSBAR_DEFAULT_HEIGHT;
end;

{ TCocoaWSCustomPage }

class function  TCocoaWSCustomPage.GetCocoaTabPageFromHandle(AHandle: HWND): TCocoaTabPage;
var
  lHandle: TCocoaTabPageView;
begin
  lHandle := TCocoaTabPageView(AHandle);
  Result := lHandle.tabPage;
end;

class function TCocoaWSCustomPage.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  lControl: TCocoaTabPage;
  tv: TCocoaTabPageView;
  tabview: TCocoaTabControl;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomPage.CreateHandle]');
  {$ENDIF}
  lControl := TCocoaTabPage.alloc().init();
  Result := TLCLHandle(lControl);
  if Result <> 0 then
  begin
    //lControl.callback := TLCLCommonCallback.Create(lControl, AWinControl);
    SetProperties(TCustomPage(AWinControl), lControl);

    // Set a special view for the page
    // based on http://stackoverflow.com/questions/14892218/adding-a-nstextview-subview-to-nstabviewitem
    tabview := TCocoaTabControl(AWinControl.Parent.Handle);
    tabview.setAllowsTruncatedLabels(false);
    tv := TCocoaTabPageView.alloc.initWithFrame(NSZeroRect);
    tv.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
    {tv.setHasVerticalScroller(True);
    tv.setHasHorizontalScroller(True);
    tv.setAutohidesScrollers(True);
    tv.setBorderType(NSNoBorder);}
    tv.tabView := tabview;
    tv.tabPage := lControl;
    tv.callback := TLCLCommonCallback.Create(tv, AWinControl);
    TLCLCommonCallback(tv.callback.GetCallbackObject).BlockCocoaUpDown := true;
    lControl.callback := tv.callback;
    lControl.setView(tv);

    Result := TLCLHandle(tv);
  end;
end;

class procedure TCocoaWSCustomPage.DestroyHandle(const AWinControl: TWinControl);
var
  tv: TCocoaTabPageView;
  ndx: NSInteger;
begin
  tv := TCocoaTabPageView(AWinControl.Handle);
  ndx := tv.tabView.exttabIndexOfTabViewItem(tv.tabPage);
  if (ndx >= 0) and (ndx < tv.tabView.fulltabs.count) then
    tv.tabview.exttabRemoveTabViewItem(tv.tabPage);
  TCocoaWSWinControl.DestroyHandle(AWinControl);
end;

class procedure TCocoaWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  lTabPage: TCocoaTabPage;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.UpdateProperties] ACustomPage='+IntToStr(PtrInt(ACustomPage)));
  {$ENDIF}
  if not Assigned(ACustomPage) or not ACustomPage.HandleAllocated then Exit;
  lTabPage := GetCocoaTabPageFromHandle(ACustomPage.Handle);
  if Assigned(lTabPage) then SetProperties(ACustomPage, lTabPage);
end;

class procedure TCocoaWSCustomPage.SetProperties(
  const ACustomPage: TCustomPage; ACocoaControl: NSTabViewItem);
var
  lHintStr: string;
begin
  // title
  ACocoaControl.setLabel(ControlTitleToNSStr(ACustomPage.Caption));

  // hint
  if ACustomPage.ShowHint then lHintStr := ACustomPage.Hint
  else lHintStr := '';
  ACocoaControl.setToolTip(StrToNSString(lHintStr));
end;

class procedure TCocoaWSCustomPage.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Pages should be fixed into their PageControl owner,
  // allowing the TCocoaWSWinControl.SetBounds function to operate here
  // was causing bug 28489
end;

class procedure TCocoaWSCustomPage.SetText(const AWinControl: TWinControl;
  const AText: String);
var
  lTitle: String;
  page  : TCocoaTabPage;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;

  page := GetCocoaTabPageFromHandle(AWinControl.Handle);
  if not Assigned(page) then Exit;
  page.setLabel(ControlTitleToNSStr(AText));

  if (AWinControl.Parent <> nil)
    and (AWinControl.Parent is TCustomTabControl)
    and (AWinControl.HandleAllocated)
  then
    UpdateTabAndArrowVisibility( TCocoaTabControl(AWinControl.Parent.Handle) );
end;

class function TCocoaWSCustomPage.GetText(const AWinControl: TWinControl;
  var AText: String): Boolean;
var
  page  : TCocoaTabPage;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then
  begin
    Result := false;
    Exit;
  end;

  page := GetCocoaTabPageFromHandle(AWinControl.Handle);
  AText := NSStringToString( page.label_ );
  Result := true;
end;

{ TCocoaWSCustomTabControl }

class function TCocoaWSCustomTabControl.LCLTabPosToNSTabStyle(AShowTabs: Boolean; ABorderWidth: Integer; ATabPos: TTabPosition): NSTabViewType;
begin
  Result := NSTopTabsBezelBorder;
  if AShowTabs then
  begin
    case ATabPos of
    tpTop:    Result := NSTopTabsBezelBorder;
    tpBottom: Result := NSBottomTabsBezelBorder;
    tpLeft:   Result := NSLeftTabsBezelBorder;
    tpRight:  Result := NSRightTabsBezelBorder;
    end;
  end
  else
  begin
    if ABorderWidth = 0 then
      Result := NSNoTabsNoBorder
    else if ABorderWidth = 1 then
      Result := NSNoTabsLineBorder
    else
      Result := NSNoTabsBezelBorder;
  end;
end;

class function TCocoaWSCustomTabControl.GetCocoaTabControlHandle(ATabControl: TCustomTabControl): TCocoaTabControl;
begin
  Result := nil;
  if ATabControl = nil then Exit;
  if not ATabControl.HandleAllocated then Exit;
  Result := TCocoaTabControl(ATabControl.Handle);
end;

class function TCocoaWSCustomTabControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  lControl: TCocoaTabControl;
  lTabControl: TCustomTabControl = nil;
  lTabStyle: NSTabViewType = NSTopTabsBezelBorder;
begin
  lTabControl := TCustomTabControl(AWinControl);
  lControl := TCocoaTabControl.alloc.lclInitWithCreateParams(AParams);
  lTabStyle := LCLTabPosToNSTabStyle(lTabControl.ShowTabs, lTabControl.BorderWidth, lTabControl.TabPosition);
  lControl.setTabViewType(lTabStyle);
  lControl.lclEnabled := AWinControl.Enabled;
  Result := TLCLHandle(lControl);
  if Result <> 0 then
  begin
    lControl.callback := TLCLTabControlCallback.Create(lControl, AWinControl);
    lControl.setDelegate(lControl);
  end;
end;

class procedure TCocoaWSCustomTabControl.SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer);
begin
  // because TCocoaWSCustomPage.SetBounds() is disabled
  // all Pages should be invalidated in TCocoaWSCustomTabControl.SetBounds()
  // see also: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/40296
  TCocoaWSWinControl.SetBounds( AWinControl, ALeft, ATop, AWidth, AHeight );
  AWinControl.InvalidateClientRectCache(true);
end;

class procedure TCocoaWSCustomTabControl.AddPage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.AddPage] AChild='+IntToStr(PtrInt(AChild)));
  {$ENDIF}
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  AChild.HandleNeeded();
  if not Assigned(AChild) or not AChild.HandleAllocated then Exit;
  lTabPage := TCocoaWSCustomPage.GetCocoaTabPageFromHandle(AChild.Handle);

  lTabControl.exttabInsertTabViewItem_atIndex(lTabPage, AIndex);

  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.AddPage] END');
  {$ENDIF}
end;

class procedure TCocoaWSCustomTabControl.MovePage(const ATabControl: TCustomTabControl; const AChild: TCustomPage; const NewIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: TCocoaTabPage;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  AChild.HandleNeeded();
  if not Assigned(AChild) or not AChild.HandleAllocated then Exit;
  lTabPage := TCocoaWSCustomPage.GetCocoaTabPageFromHandle(AChild.Handle);

  lTabControl.exttabMoveTabViewItem_toIndex( lTabPage, NewIndex );
end;

class procedure TCocoaWSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  lTabPage := NSTabViewItem(lTabControl.fulltabs.objectAtIndex(AIndex));
  lTabControl.exttabremoveTabViewItem(lTabPage);
end;

class function TCocoaWSCustomTabControl.GetTabIndexAtPos(const ATabControl: TCustomTabControl; const AClientPos: TPoint): integer;
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
  lClientPos: NSPoint;
  pt : TPoint;
begin
  Result := -1;
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  pt.x := Round(AClientPos.x + lTabControl.contentRect.origin.x);
  pt.y := Round(AClientPos.y + lTabControl.contentRect.origin.y);

  if lTabControl.isFlipped then
  begin
    lClientPos.x := pt.X;
    lClientPos.y := pt.Y;
  end
  else
    lClientPos := LCLToNSPoint(pt, lTabControl.frame.size.height);

  lTabPage := lTabControl.tabViewItemAtPoint(lClientPos);
  if not Assigned(lTabPage) then
    Exit;
  Result := lTabControl.exttabIndexOfTabViewItem(lTabPage);
end;

class function TCocoaWSCustomTabControl.GetTabRect(
  const ATabControl: TCustomTabControl; const AIndex: Integer): TRect;
var
  lTabControl: TCocoaTabControl;
  lTabPage: NSTabViewItem;
  tb : TCocoaTabPageView;
  i   : integer;
  idx : NSUInteger;
  tr  : TRect;
  w   : array of Double;
  mw  : Double;
  ofs : Double; // aprx offset between label and the text (from both sides)
  x   : Double;
  vt  : NSTabViewType;
begin
  Result:=inherited GetTabRect(ATabControl, AIndex);
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);
  // unable to determine the rectangle view

  if (AIndex<0) or (AIndex>=ATabControl.PageCount) then Exit;
  tb := TCocoaTabPageView(ATabControl.Page[AIndex].Handle);
  if not Assigned(tb) then Exit;

  idx := lTabControl.tabViewItems.indexOfObject( tb.tabPage );
  if idx = NSNotFound then Exit;

  if not GetTabsRect(lTabControl, tr) then Exit;

  SetLength(w, lTabControl.tabViewItems.count);
  if (length(w) = 0) then Exit; // no tabs!

  vt := lTabControl.tabViewType;
  if (vt = NSTopTabsBezelBorder) or (vt = NSBottomTabsBezelBorder) then
  begin
    mw := 0;
    for i := 0 to Integer(lTabControl.tabViewItems.count)-1 do
    begin
      lTabPage := lTabControl.tabViewItemAtIndex(i);
      w[i] := lTabPage.sizeOfLabel(false).width;
      mw := mw + w[i];
    end;
    if (mw = 0) then Exit; // 0 for the total tabs width?

    ofs := (tr.Right - tr.Left - mw) / length(w);

    x := tr.Left;
    for i := 0 to Integer(idx)-1 do
      x := x+ofs+w[i];

    Result.Left := Round(x);
    Result.Right := Round(Result.Left + w[idx]);
    Result.Top := tr.Top;
    Result.Bottom := tr.Bottom;
  end
  else
  begin
    mw := 0;
    for i := 0 to Integer(lTabControl.tabViewItems.count)-1 do
    begin
      lTabPage := lTabControl.tabViewItemAtIndex(i);
      w[i] := lTabPage.sizeOfLabel(false).height;
      mw := mw + w[i];
    end;
    if (mw = 0) then Exit; // 0 for the total tabs width?

    ofs := (tr.Bottom - tr.Top - mw) / length(w);

    x := tr.Top;
    for i := 0 to Integer(idx)-1 do
      x := x+ofs+w[i];

    Result.Left := tr.Left;
    Result.Right := tr.Right;
    Result.Top := Round(x);
    Result.Bottom := Round(Result.Top + w[idx]);
  end;
end;

class procedure TCocoaWSCustomTabControl.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  i  : NSInteger;
  tb : TCocoaTabPageView;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn('[TCocoaWSCustomTabControl.SetPageIndex]');
  {$ENDIF}
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  if (AIndex<0) or (AIndex>=ATabControl.PageCount) then Exit;
  tb := TCocoaTabPageView(ATabControl.Page[AIndex].Handle);
  if not Assigned(tb) then Exit;

  i := TCocoaTabControl(ATabControl.Handle).exttabIndexOfTabViewItem(tb.tabPage);
  if i < 0 then
    Exit;

  TCocoaTabControl(ATabControl.Handle).extselectTabViewItemAtIndex(i);
end;

class procedure TCocoaWSCustomTabControl.SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
var
  lTabControl: TCocoaTabControl = nil;
  lTabStyle: NSTabViewType;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  lTabStyle := LCLTabPosToNSTabStyle(ATabControl.ShowTabs, ATabControl.BorderWidth, ATabPosition);
  lTabControl.setTabViewType(lTabStyle);
end;

class procedure TCocoaWSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
var
  lTabControl: TCocoaTabControl = nil;
  lOldTabStyle, lTabStyle: NSTabViewType;
var
  pr : TRect;
  ar : TRect;
  fr : NSRect;
  dx, dy : double;
  cb: ICommonCallback;
begin
  if not Assigned(ATabControl) or not ATabControl.HandleAllocated then Exit;
  lTabControl := TCocoaTabControl(ATabControl.Handle);

  lOldTabStyle := lTabControl.tabViewType();
  lTabStyle := LCLTabPosToNSTabStyle(AShowTabs, ATabControl.BorderWidth, ATabControl.TabPosition);
  pr := lTabControl.lclGetFrameToLayoutDelta;
  lTabControl.setTabViewType(lTabStyle);
  ar := lTabControl.lclGetFrameToLayoutDelta;
  // switching ShowTabs actually changes layout to frame
  // this needs to be compenstated
  if (ar.Top<>pr.Top) or (ar.Left<>pr.Left) then
  begin
    fr := lTabControl.frame;
    dx := pr.Left - ar.left;
    dy := pr.Top - ar.Top;
    fr.origin.x := fr.origin.x + dx;
    fr.origin.y := fr.origin.y + dy;
    fr.size.width := fr.size.width - dx - (ar.Right - pr.Right);
    fr.size.height := fr.size.height - dy - (ar.Bottom - pr.Bottom);
    lTabControl.setFrame(fr);
    cb := lTabControl.lclGetCallback;
    if Assigned(cb) then cb.frameDidChange(lTabControl);
  end;
end;

class procedure TCocoaWSCustomTabControl.SetChildZPosition(const AWinControl, AChild: TWinControl;
  const AOldPos, ANewPos: Integer; const AChildren: TFPList);
begin
  // subviews of NSTabView do not need to be resorted, Cocoa will take of it.
  // avoid unnecessary performance loss.
end;

{ TCocoaListView }

function TCocoaListView.documentView: NSView;
begin
  Result:= _backendControl;
end;

function TCocoaListView.scrollView: TCocoaScrollView;
begin
  Result:= _scrollView;
end;

function TCocoaListView.WSHandler: TCocoaWSListViewHandler;
begin
  Result:= _WSHandler;
end;

procedure TCocoaListView.addSubview(aView: NSView);
begin
  self.documentView.addSubview(aView);
end;

procedure TCocoaListView.setScrollView(aView: TCocoaScrollView);
begin
  _scrollView:= aView;
  Inherited addSubview(aView);
end;

function TCocoaListView.initializing: Boolean;
begin
  Result:= _initializing;
end;

procedure TCocoaListView.setViewStyle(viewStyle: TViewStyle);
begin
  if Assigned(_backendControl) and (_viewStyle=viewStyle) then
    Exit;

  _viewStyle:= viewStyle;
  releaseControls;
  createControls;
  initData;
end;

procedure TCocoaListView.createControls;
var
  controlFrame: NSRect;
  backendControlAccess: TCocoaListViewBackendControlProtocol;
begin
  if _viewStyle = vsReport then begin
    _backendControl:= AllocCocoaTableListView;
    _WSHandler:= TCocoaWSListView_TableViewHandler.Create( self );
  end else begin
    _backendControl:= AllocCocoaCollectionView;
    _WSHandler:= TCocoaWSListView_CollectionViewHandler.Create( self );
  end;

  controlFrame:= self.bounds;
  _backendControl.initWithFrame( controlFrame );
  _scrollView:= TCocoaScrollView.alloc.initWithFrame( controlFrame );
  _scrollView.setDocumentView( _backendControl );
  _scrollView.setAutoresizingMask( NSViewWidthSizable or NSViewHeightSizable );
  _scrollView.callback:= self.callback;
  self.setScrollView( _scrollView );
  ScrollViewSetBorderStyle( _scrollView, _lclListView.BorderStyle);

  backendControlAccess:= TCocoaListViewBackendControlProtocol(_backendControl);
  backendControlAccess.backend_onInit( _lclListView );
  backendControlAccess.backend_setCallback( self.callback );
end;

procedure TCocoaListView.releaseControls;
begin
  if not Assigned(_backendControl) then
    Exit;
  _scrollView.removeFromSuperview;
  _scrollView.setDocumentView( nil );
  _scrollView.release;
  _scrollView:= nil;
  _backendControl.release;
  _backendControl:= nil;
end;

procedure TCocoaListView.initData;
var
  needsInit: Boolean = False;
begin
  needsInit:= _needsCallLclInit;
  _needsCallLclInit:= False;
  if needsInit then begin
    _initializing:= True;
    TCustomListViewAccess(_lclListView).InitializeWnd;
    _initializing:= False;
    TCocoaListViewBackendControlProtocol(_backendControl).backend_reloadData;
  end;
  _needsCallLclInit:= True;
end;

function TCocoaListView.lclGetCallback: ICommonCallback;
begin
  Result:= callback;
end;

procedure TCocoaListView.lclClearCallback;
begin
  callback:= nil;
end;

function TCocoaListView.lclContentView: NSView;
begin
  Result:= documentView;
end;

procedure TCocoaListView.setLclListView(lclListView: TCustomListView);
begin
  _lclListView:= lclListView;
end;

{ TCocoaWSCustomListView }

class function TCocoaWSCustomListView.getWSHandler(
  const lclListView: TCustomListView): TCocoaWSListViewHandler;
var
  cocoaListView: TCocoaListView;
begin
  Result:= nil;
  if NOT Assigned(lclListView) or NOT lclListView.HandleAllocated then
    Exit;
  cocoaListView:= TCocoaListView( lclListView.Handle );
  if NOT Assigned(cocoaListView) then
    Exit;
  Result:= cocoaListView.WSHandler;
end;

class function TCocoaWSCustomListView.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLHandle;
var
  cocoaListView: TCocoaListView;
  lclListView: TCustomListViewAccess Absolute AWinControl;
  lclcb: TLCLListViewCallback;
begin
  cocoaListView:= TCocoaListView.alloc.lclInitWithCreateParams(AParams);
  cocoaListView.setLclListView( lclListView );
  cocoaListView.setAutoresizesSubviews( True );
  lclcb := TLCLListViewCallback.Create( cocoaListView, lclListView, cocoaListView );
  lclcb.listView := lclListView;
  cocoaListView.callback:= lclcb;
  Result:= TLCLHandle( cocoaListView );
end;

class procedure TCocoaWSCustomListView.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
var
  cocoaListView: TCocoaListView;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;
  cocoaListView:= TCocoaListView(AWinControl.Handle);
  ScrollViewSetBorderStyle(cocoaListView.scrollView, ABorderStyle);
  UpdateFocusRing(cocoaListView.documentView, ABorderStyle);
end;

class procedure TCocoaWSCustomListView.ColumnDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnDelete( AIndex );
end;

class function TCocoaWSCustomListView.ColumnGetWidth(
  const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn
  ): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ColumnGetWidth( AIndex, AColumn );
end;

class procedure TCocoaWSCustomListView.ColumnInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AColumn: TListColumn);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnInsert( AIndex, AColumn );
end;

class procedure TCocoaWSCustomListView.ColumnMove(const ALV: TCustomListView;
  const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnMove( AOldIndex, ANewIndex, AColumn );
end;

class procedure TCocoaWSCustomListView.ColumnSetAlignment(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AAlignment: TAlignment);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetAlignment( AIndex, AColumn, AAlignment );
end;

class procedure TCocoaWSCustomListView.ColumnSetAutoSize(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AAutoSize: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetAutoSize( AIndex, AColumn, AAutoSize );
end;

class procedure TCocoaWSCustomListView.ColumnSetCaption(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const ACaption: String);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetCaption( AIndex, AColumn, ACaption );
end;

class procedure TCocoaWSCustomListView.ColumnSetMaxWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AMaxWidth: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetMaxWidth( AIndex, AColumn, AMaxWidth );
end;

class procedure TCocoaWSCustomListView.ColumnSetMinWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AMinWidth: integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetMinWidth( AIndex, AColumn, AMinWidth );
end;

class procedure TCocoaWSCustomListView.ColumnSetWidth(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AWidth: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetWidth( AIndex, AColumn, AWidth );
end;

class procedure TCocoaWSCustomListView.ColumnSetVisible(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const AVisible: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetVisible( AIndex, AColumn, AVisible );
end;

class procedure TCocoaWSCustomListView.ColumnSetSortIndicator(
  const ALV: TCustomListView; const AIndex: Integer;
  const AColumn: TListColumn; const ASortIndicator: TSortIndicator);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ColumnSetSortIndicator( AIndex, AColumn, ASortIndicator );
end;

class procedure TCocoaWSCustomListView.ItemDelete(const ALV: TCustomListView;
  const AIndex: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemDelete( AIndex );
end;

class function TCocoaWSCustomListView.ItemDisplayRect(
  const ALV: TCustomListView; const AIndex, ASubItem: Integer;
  ACode: TDisplayCode): TRect;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= Bounds(0,0,0,0);
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ItemDisplayRect( AIndex, ASubItem, ACode );
end;

class function TCocoaWSCustomListView.ItemGetChecked(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem
  ): Boolean;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= False;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ItemGetChecked( AIndex, AItem );
end;

class function TCocoaWSCustomListView.ItemGetPosition(
  const ALV: TCustomListView; const AIndex: Integer): TPoint;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= Point( 0, 0 );
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ItemGetPosition( AIndex );
end;

class function TCocoaWSCustomListView.ItemGetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  out AIsSet: Boolean): Boolean;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= False;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.ItemGetState( AIndex, AItem, AState, AIsSet );
end;

class procedure TCocoaWSCustomListView.ItemInsert(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemInsert( AIndex, AItem );
end;

class procedure TCocoaWSCustomListView.ItemSetChecked(
  const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem;
  const AChecked: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemSetChecked( AIndex, AItem, AChecked );
end;

class procedure TCocoaWSCustomListView.ItemSetImage(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex,
  AImageIndex: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemSetImage( AIndex, AItem, ASubIndex, AImageIndex );
end;

class procedure TCocoaWSCustomListView.ItemSetState(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const AState: TListItemState;
  const AIsSet: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemSetState( AIndex, AItem, AState, AIsSet );
end;

class procedure TCocoaWSCustomListView.ItemSetText(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer;
  const AText: String);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemSetText( AIndex, AItem, ASubIndex, AText );
end;

class procedure TCocoaWSCustomListView.ItemShow(const ALV: TCustomListView;
  const AIndex: Integer; const AItem: TListItem; const PartialOK: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.ItemShow( AIndex, AItem, PartialOK );
end;

class function TCocoaWSCustomListView.GetFocused(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= -1;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetFocused();
end;

class function TCocoaWSCustomListView.GetItemAt(const ALV: TCustomListView; x,
  y: integer): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= -1;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetItemAt( x, y );
end;

class function TCocoaWSCustomListView.GetSelCount(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetSelCount();
end;

class function TCocoaWSCustomListView.GetSelection(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= -1;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetSelection();
end;

class function TCocoaWSCustomListView.GetTopItem(const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetTopItem();
end;

class function TCocoaWSCustomListView.GetVisibleRowCount(
  const ALV: TCustomListView): Integer;
var
  WSHandler: TCocoaWSListViewHandler;
begin
  Result:= 0;
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    Result:= WSHandler.GetVisibleRowCount();
end;

class procedure TCocoaWSCustomListView.SelectAll(const ALV: TCustomListView;
  const AIsSet: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SelectAll( AIsSet );
end;

class procedure TCocoaWSCustomListView.SetDefaultItemHeight(
  const ALV: TCustomListView; const AValue: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetDefaultItemHeight( AValue );
end;

class procedure TCocoaWSCustomListView.SetImageList(const ALV: TCustomListView;
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetImageList( AList, AValue );
end;

class procedure TCocoaWSCustomListView.SetItemsCount(
  const ALV: TCustomListView; const Avalue: Integer);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetItemsCount( AValue );
end;

class procedure TCocoaWSCustomListView.SetOwnerData(const ALV: TCustomListView;
  const AValue: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetOwnerData( AValue );
end;

class procedure TCocoaWSCustomListView.SetProperty(const ALV: TCustomListView;
  const AProp: TListViewProperty; const AIsSet: Boolean);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetProperty( AProp, AIsSet );
end;

class procedure TCocoaWSCustomListView.SetScrollBars(
  const ALV: TCustomListView; const AValue: TScrollStyle);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetScrollBars( AValue );
end;

class procedure TCocoaWSCustomListView.SetSort(const ALV: TCustomListView;
  const AType: TSortType; const AColumn: Integer;
  const ASortDirection: TSortDirection);
var
  WSHandler: TCocoaWSListViewHandler;
begin
  WSHandler:= getWSHandler( ALV );
  if Assigned(WSHandler) then
    WSHandler.SetSort( AType, AColumn, ASortDirection );
end;

class function TCocoaWSCustomListView.RestoreItemCheckedAfterSort(
  const ALV: TCustomListView): Boolean;
begin
  Result:= True;
end;

class procedure TCocoaWSCustomListView.SetViewStyle(const ALV: TCustomListView;
  const AValue: TViewStyle);
var
  cocoalistView: TCocoaListView;
begin
  cocoalistView:= TCocoaListView( ALV.Handle );
  cocoalistView.setViewStyle( AValue );
end;

{ TCocoaWSProgressBar }

class function TCocoaWSProgressBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaProgressIndicator;
begin
  lResult := TCocoaProgressIndicator.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TLCLCommonCallback.Create(lResult, AWinControl);
    lResult.startAnimation(nil);
    //small constrol size looks like carbon
    //lResult.setControlSize(NSSmallControlSize);
  end;
  Result := TLCLHandle(lResult);
end;

class procedure TCocoaWSProgressBar.ApplyChanges(
  const AProgressBar: TCustomProgressBar);
var
  ind : NSProgressIndicator;
begin
  if not Assigned(AProgressBar) or not AProgressBar.HandleAllocated then Exit;
  ind:=NSProgressIndicator(AProgressBAr.Handle);
  ind.setMaxValue(AProgressBar.Max);
  ind.setMinValue(AProgressBar.Min);
  ind.setDoubleValue(AProgressBar.Position);
  SetStyle(AProgressBar, AProgressBar.Style);
end;

class procedure TCocoaWSProgressBar.SetPosition(
  const AProgressBar: TCustomProgressBar; const NewPosition: integer);
begin
  if AProgressBar.HandleAllocated then
    NSProgressIndicator(AProgressBar.Handle).setDoubleValue(NewPosition);
end;

class procedure TCocoaWSProgressBar.SetStyle(
  const AProgressBar: TCustomProgressBar; const NewStyle: TProgressBarStyle);
begin
  if AProgressBar.HandleAllocated then
  begin
    NSProgressIndicator(AProgressBar.Handle).setIndeterminate(NewStyle = pbstMarquee);
    NSProgressIndicator(AProgressBar.Handle).startAnimation(nil);
  end;
end;

{ TCocoaTabPage }

(*function TCocoaTabPage.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaTabPage.lclClearCallback;
begin
  callback:=nil;
end;

{ TCocoaTabControl }

function TCocoaTabControl.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaTabControl.lclClearCallback;
begin
  callback:=nil;
end; *)

{ TLCLListViewCallback }

constructor TLCLListViewCallback.Create(AOwner: NSObject; ATarget: TWinControl; AHandleView: NSView);
begin
  inherited Create(AOwner, ATarget, AHandleView);
  selectionIndexSet:= NSMutableIndexSet.new;
  checkedIdx:= NSMutableIndexSet.new;
end;

destructor TLCLListViewCallback.Destroy;
begin
  selectionIndexSet.release;
  selectionIndexSet:= nil;
  checkedIdx.release;
  checkedIdx:= nil;
  inherited Destroy;
end;

function TLCLListViewCallback.ItemsCount: Integer;
begin
  Result:=listView.Items.Count + tempItemsCountDelta;
end;

function TLCLListViewCallback.GetItemTextAt(ARow, ACol: Integer;
  var Text: String): Boolean;
begin
  Result := (ACol>=0) and (ACol<listView.ColumnCount)
    and (ARow >= 0) and (ARow < listView.Items.Count);
  if not Result then Exit;
  if ACol = 0 then
    Text := listView.Items[ARow].Caption
  else
  begin
    Text := '';
    dec(ACol);
    if (ACol >=0) and (ACol < listView.Items[ARow].SubItems.Count) then
      Text := listView.Items[ARow].SubItems[ACol];
  end;
end;

function TLCLListViewCallback.GetItemCheckedAt(ARow, ACol: Integer;
  var IsChecked: Integer): Boolean;
var
  BoolState : array [Boolean] of Integer = (NSOffState, NSOnState);
begin
  if ownerData and Assigned(listView) and (ARow>=0) and (ARow < listView.Items.Count) then
    IsChecked := BoolState[listView.Items[ARow].Checked]
  else
    IsChecked := BoolState[checkedIdx.containsIndex(ARow)];
  Result := true;
end;

function TLCLListViewCallback.GetItemImageAt(ARow, ACol: Integer;
  var imgIdx: Integer): Boolean;
begin
  Result := (ACol >= 0) and (ACol < listView.ColumnCount)
    and (ARow >= 0) and (ARow < listView.Items.Count);

  if not Result then Exit;

  if ACol = 0 then
    imgIdx := listView.Items[ARow].ImageIndex
  else
  begin
    dec(ACol);
    if (ACol >=0) and (ACol < listView.Items[ARow].SubItems.Count) then
      imgIdx := listView.Items[ARow].SubItemImages[ACol];
  end;
end;

function TLCLListViewCallback.GetImageFromIndex(imgIdx: Integer): NSImage;
var
  bmp : TBitmap;
  lst : TCustomImageList;
  x,y : integer;
  img : NSImage;
  rep : NSBitmapImageRep;
  cb  : TCocoaBitmap;
begin
  lst := TCustomListViewAccess(listView).LargeImages;
  if NOT Assigned(lst) then
    lst := TCustomListViewAccess(listView).SmallImages;
  bmp := TBitmap.Create;
  try
    lst.GetBitmap(imgIdx, bmp);

    if bmp.Handle = 0 then begin
      Result := nil;
      Exit;
    end;

    // Bitmap Handle should be nothing but TCocoaBitmap
    cb := TCocoaBitmap(bmp.Handle);

    // There's NSBitmapImageRep in TCocoaBitmap, but it depends on the availability
    // of memory buffer stored with TCocoaBitmap. As soon as TCocoaBitmap is freed
    // pixels are not available. For this reason, we're making a copy of the bitmapdata
    // allowing Cocoa to allocate its own buffer (by passing nil for planes parameter)
    rep := NSBitmapImageRep(NSBitmapImageRep.alloc).initWithBitmapDataPlanes_pixelsWide_pixelsHigh__colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(
      nil, // planes, BitmapDataPlanes
      Round(cb.ImageRep.size.Width), // width, pixelsWide
      Round(cb.ImageRep.size.Height),// height, PixelsHigh
      cb.ImageRep.bitsPerSample,// bitsPerSample, bps
      cb.ImageRep.samplesPerPixel, // samplesPerPixel, spp
      cb.ImageRep.hasAlpha, // hasAlpha
      False, // isPlanar
      cb.ImageRep.colorSpaceName, // colorSpaceName
      cb.ImageRep.bitmapFormat, // bitmapFormat
      cb.ImageRep.bytesPerRow, // bytesPerRow
      cb.ImageRep.BitsPerPixel //bitsPerPixel
    );
    System.Move( cb.ImageRep.bitmapData^, rep.bitmapData^, cb.ImageRep.bytesPerRow * Round(cb.ImageRep.size.height));
    img := NSImage(NSImage.alloc).initWithSize( rep.size );
    img.addRepresentation(rep);
    Result := img;
	rep.release;

  finally
    bmp.Free;
  end;
end;

procedure TLCLListViewCallback.SetItemTextAt(ARow, ACol: Integer;
  const Text: String);
begin
  // there's no notifcaiton to be sent to the TCustomListView;
  if (ACol<>0) then Exit;

  inc(isSetTextFromWS);
  try
    if (ACol=0) then
      if (ARow>=0) and (ARow<listView.Items.Count) then
        TCustomListViewAccess(listView).DoEndEdit(listView.Items[ARow], Text);
  finally
    dec(isSetTextFromWS);
  end;

end;

procedure TLCLListViewCallback.SetItemCheckedAt(ARow, ACol: Integer;
  IsChecked: Integer);
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  if IsChecked = NSOnState
    then checkedIdx.addIndex(ARow)
    else checkedIdx.removeIndex(ARow);

  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  FillChar(NMLV{%H-}, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := ListView.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;
  NMLV.iItem := ARow;
  NMLV.iSubItem := 0;
  NMLV.uChanged := LVIF_STATE;
  Msg.NMHdr := @NMLV.hdr;

  LCLMessageGlue.DeliverMessage(ListView, Msg);
end;

function TLCLListViewCallback.getItemStableSelection(ARow: Integer): Boolean;
begin
  Result:= selectionIndexSet.containsIndex( ARow );
end;

procedure TLCLListViewCallback.selectionChanged(NewSel: Integer; Added, Removed: NSIndexSet);
var
  Msg: TLMNotify;
  NMLV: TNMListView;

  procedure RunIndex(idx: NSIndexSet);
  var
    buf : array [0..256-1] of NSUInteger;
    rng : NSRange;
    cnt : Integer;
    i   : Integer;
    itm : NSUInteger;
  begin
    rng.location := idx.firstIndex;
    repeat
      rng.length := idx.lastIndex - rng.location + 1;
      cnt := idx.getIndexes_maxCount_inIndexRange(@buf[0], length(buf), @rng);
      for i := 0 to cnt - 1 do begin
        NMLV.iItem := buf[i];
        LCLMessageGlue.DeliverMessage(ListView, Msg);
      end;
      if cnt < length(buf) then cnt := 0
      else rng.location := buf[cnt-1]+1;
    until cnt = 0;
  end;

begin
  {$IFDEF COCOA_DEBUG_LISTVIEW}
  WriteLn(Format('[TLCLListViewCallback.SelectionChanged] NewSel=%d', [NewSel]));
  {$ENDIF}

  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  FillChar(NMLV{%H-}, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := ListView.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;
  NMLV.iSubItem := 0;
  NMLV.uChanged := LVIF_STATE;
  Msg.NMHdr := @NMLV.hdr;

  if Removed.count>0 then
  begin
    NMLV.uNewState := 0;
    NMLV.uOldState := LVIS_SELECTED;
    RunIndex( Removed );
  end;
  if Added.count > 0 then begin
    NMLV.uNewState := LVIS_SELECTED;
    NMLV.uOldState := 0;
    RunIndex( Added );
  end;

  {if NewSel >= 0 then
  begin
    NMLV.iItem := NewSel;
    NMLV.uNewState := LVIS_SELECTED;
  end
  else
  begin
    NMLV.iItem := 0;
    NMLV.uNewState := 0;
    NMLV.uOldState := LVIS_SELECTED;
  end;

  LCLMessageGlue.DeliverMessage(ListView, Msg);}
end;

procedure TLCLListViewCallback.selectOne(ARow: Integer; isSelected: Boolean);
  procedure sendMsgToLCL;
  var
    Msg: TLMNotify;
    NMLV: TNMListView;
  begin
    Msg:= Default( TLMNotify );
    NMLV:= Default( TNMListView );

    Msg.Msg := CN_NOTIFY;

    NMLV.hdr.hwndfrom := ListView.Handle;
    NMLV.hdr.code := LVN_ITEMCHANGED;
    NMLV.iSubItem := 0;
    NMLV.uChanged := LVIF_STATE;
    Msg.NMHdr := @NMLV.hdr;

    if isSelected then begin
      NMLV.uNewState := LVIS_SELECTED;
      NMLV.uOldState := 0;
    end else begin
      NMLV.uNewState := 0;
      NMLV.uOldState := LVIS_SELECTED;
    end;

    NMLV.iItem := ARow;
    LCLMessageGlue.DeliverMessage(ListView, Msg);
  end;
begin
  if isSelected then
    self.selectionIndexSet.addIndex( ARow )
  else
    self.selectionIndexSet.removeIndex( ARow );

  sendMsgToLCL;
end;

function TLCLListViewCallback.shouldSelectionChange(NewSel: Integer
  ): Boolean;
var
  item: TListItem = nil;
begin
  if (NewSel>=0) and (NewSel<self.listView.Items.Count) then
    item:= self.listView.Items[NewSel];
  Result:= TCustomListViewAccess(self.listView).CanChange( item, LVIF_TEXT );
end;

procedure TLCLListViewCallback.ColumnClicked(ACol: Integer);
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  FillChar(NMLV{%H-}, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := ListView.Handle;
  NMLV.hdr.code := LVN_COLUMNCLICK;
  NMLV.iSubItem := ACol;
  NMLV.uChanged := 0;
  Msg.NMHdr := @NMLV.hdr;

  LCLMessageGlue.DeliverMessage(ListView, Msg);
end;

procedure TLCLListViewCallback.DrawRow(rowidx: Integer; ctx: TCocoaContext;
  const r: TRect; state: TOwnerDrawState);
var
  ALV: TCustomListViewAccess;
begin
  ALV:= TCustomListViewAccess(self.listView);
  ALV.Canvas.Handle:= HDC(ctx);
  ALV.IntfCustomDraw( dtItem, cdPrePaint, rowidx, 0, [], nil );
  ALV.Canvas.Handle:= 0;
end;

procedure TLCLListViewCallback.GetRowHeight(rowidx: Integer; var h: Integer);
begin

end;

{ TCocoaWSTrackBar }

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the control in Carbon interface

  Creates new track bar with the specified parameters
 ------------------------------------------------------------------------------}
class function TCocoaWSTrackBar.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  lResult: TCocoaSlider;
begin
  lResult := TCocoaSlider.alloc.lclInitWithCreateParams(AParams);
  if Assigned(lResult) then
  begin
    lResult.callback := TLCLCommonCallback.Create(lResult, AWinControl);
    lResult.setTarget(lResult);
    lResult.setAction(objcselector('sliderAction:'));
  end;
  Result := TLCLHandle(lResult);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.ApplyChanges
  Params:  ATrackBar - LCL custom track bar

  Sets the parameters (Min, Max, Position, Ticks) of slider
 ------------------------------------------------------------------------------}
class procedure TCocoaWSTrackBar.ApplyChanges(const ATrackBar: TCustomTrackBar);
var
  lSlider: TCocoaSlider;
  lTickCount, lTrackBarLength: Integer;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.setMaxValue(ATrackBar.Max);
  lSlider.setMinValue(ATrackBar.Min);
  lSlider.setIntValue(ATrackBar.Position);
  lSlider.intval := ATrackBar.Position;
  lSlider.setContinuous(true);
  lSlider.setAltIncrementValue(1); // forcing the slider to switch by 1 by the keyboard

  // Ticks
  if ATrackBar.TickStyle = tsAuto then
  begin
    // this should only apply to Auto
    // and for Manual it should drawn manually
    if ATrackBar.Frequency <> 0 then
      lTickCount := (ATrackBar.Max-ATrackBar.Min) div ATrackBar.Frequency + 1
    else
      lTickCount := (ATrackBar.Max-ATrackBar.Min);

    // Protection from too frequent ticks.
    // 1024 is a number of "too much" ticks, based on a common
    // screen resolution 1024 x 768
    // Protects ticks from "disappearing" when trackbar is resized
    // and is temporary too narrow to fit the trackbar
    if TickCount > 1024 then
    begin
      if ATrackBar.Orientation = trHorizontal then
        lTrackBarLength := ATrackBar.Width
      else
        lTrackBarLength := ATrackBar.Height;

      lTickCount := Min(lTickCount, lTrackBarLength);
    end;
  end else if ATrackBar.TickStyle = tsManual then
  begin
    lTickCount := 2;
  end else
    lTickCount := 0;

  lSlider.lclSetManTickDraw(ATrackBar.TickStyle = tsManual);

  lSlider.setNumberOfTickMarks(lTickCount);

  if ATrackBar.TickMarks = tmTopLeft then
    lSlider.setTickMarkPosition(NSTickMarkAbove)
  else
    lSlider.setTickMarkPosition(NSTickMarkBelow);
  lSlider.setNeedsDisplay_(true);
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.GetPosition
  Params:  ATrackBar - LCL custom track bar
  Returns: Position of slider
 ------------------------------------------------------------------------------}
class function TCocoaWSTrackBar.GetPosition(const ATrackBar: TCustomTrackBar
  ): integer;
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then
  begin
    Result := 0;
    Exit;
  end;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  Result := lSlider.intValue();
end;

{------------------------------------------------------------------------------
  Method:  TCocoaWSTrackBar.SetPosition
  Params:  ATrackBar - LCL custom track bar
           NewPosition  - New position

  Sets the position of slider
 ------------------------------------------------------------------------------}
class procedure TCocoaWSTrackBar.SetPosition(const ATrackBar: TCustomTrackBar;
  const NewPosition: integer);
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.setIntValue(ATrackBar.Position);
end;

// Cocoa auto-detects the orientation based on width/height and there seams
// to be no way to force it
class procedure TCocoaWSTrackBar.SetOrientation(const ATrackBar: TCustomTrackBar;
  const AOrientation: TTrackBarOrientation);
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  if (AOrientation = trHorizontal) and (ATrackBar.Height >= ATrackBar.Width) then
    ATrackBar.Width := ATrackBar.Height + 1
  else if (AOrientation = trVertical) and (ATrackBar.Width >= ATrackBar.Height) then
    ATrackBar.Height := ATrackBar.Width + 1;
end;

class procedure TCocoaWSTrackBar.SetTick(const ATrackBar: TCustomTrackBar; const ATick: integer);
var
  lSlider: TCocoaSlider;
begin
  if not Assigned(ATrackBar) or not ATrackBar.HandleAllocated then Exit;
  lSlider := TCocoaSlider(ATrackBar.Handle);
  lSlider.lclAddManTick(ATick);
end;

class procedure TCocoaWSTrackBar.GetPreferredSize(
  const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
var
  lSlider : TCocoaSlider;
  trk     : TCustomTrackBar;
  frm     : NSRect;
begin
  if not Assigned(AWinControl) or not AWinControl.HandleAllocated then Exit;
  trk := TCustomTrackBar(AWinControl);
  lSlider := TCocoaSlider(AWinControl.Handle);
  frm := lSlider.frame;
  try
    if trk.Orientation = trVertical then
      lSlider.setFrame(NSMakeRect(0,0,5,10))
    else
      lSlider.setFrame(NSMakeRect(0,0,10,5));

    TCocoaWSWinControl.GetPreferredSize(AWinControl,PreferredWidth, PreferredHeight, WithThemeSpace);
    if trk.Orientation = trVertical then
      PreferredHeight := 0
    else
      PreferredWidth := 0;
  finally
    lSlider.setFrame(frm);
  end;
end;

end.
