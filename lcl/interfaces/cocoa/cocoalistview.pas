unit CocoaListView;

{$mode delphi}
{$modeswitch objectivec2}
{$include cocoadefines.inc}

interface

uses
  // RTL, FCL, LCL
  MacOSAll, CocoaAll,
  Classes, LCLType, SysUtils, LCLMessageGlue, LMessages,
  Controls, ComCtrls, Types, StdCtrls, LCLProc, Graphics, ImgList, Forms,
  // Cocoa WS
  CocoaPrivate, CocoaCallback, CocoaListControl, CocoaWSCommon,
  CocoaScrollers, CocoaWSScrollers, CocoaTextEdits, CocoaGDIObjects, CocoaUtils,
  cocoa_extra;

type
  {
    1. Key Features
       1.1 currently ListView supports all ViewStyles of TListView
           (vsReport/vsIcon/vsSmallIcon/vsList)
       1.2 supports MultiSelection, supports checkBoxes
       1.3 supports keeping the selection and checkBox unchanged after
           inserting/deleting/sorting/switching ViewStyle
       1.4 supports OwnerData
       1.5 supports OwnerDraw(OnDrawItem)
       1.6 supports CustomDraw(OnCustomDraw/OnCustomDrawItem/OnCustomDrawSubItem)

    2. the Overall Structure of ListView
       since Cocoa does not have a single control corresponding to TListView,
       multiple controls need to be combined to implement TListView.
       at the same time, in order to avoid RecreateWnd when switching ViewStyle,
       a three-layers structure is used:
       2.1 Stability layer
           TCocoaListView is a simple NSView, corresponding to the Handle
           returned to LCL. it remains unchanged when switching ViewStyle.
       2.2 Scrolling Layer
           TCocoaScrollView is a simple NSScrollView that provides scrolling
           support for the underlying control. it will be recreated when
           switching ViewStyle.
       2.3 Implementation layer
           control that implement real functions. it will be recreated when
           switching ViewStyle.
         2.3.1 vsReport corresponds to TCocoaTableListView (NSTableView)
         2.3.2 other styles correspond to TCocoaCollectionView (NSCollectionView)

    3. TCocoaWSListViewHandler
       LCL interacts with TCocoaListView through TWSCustomListView. in order to
       isolate the codes of two different underlying controls,
       TCocoaWSListViewHandler and its subclasses are added:
       3.1 in TCocoaWSCustomListView (TWSCustomListView), forward to the
           corresponding subclass of TCocoaWSListViewHandler according to
           ViewStyle
       3.2 vsReport corresponds to TCocoaWSListView_TableViewHandler
           (implemented in CocoaTables unit)
       3.3 other styles correspond to TCocoaWSListView_CollectionViewHandler
           (implemented in CocoaCollectionView unit)

    4. TLCLListViewCallback
       TCocoaListView interacts with LCL through IListViewCallback (TLCLListViewCallback)
       4.1 since TListView is a whole control in LCL, TLCLListViewCallback
           does not need to be customized for different ViewStyles
       4.2 however, it should be noted that TCocoaTableListView is not only the
           underlying control of TListView, but also the corresponding control of
           TListBox/TCheckListBox.
           therefore, the callback contained in TCocoaTableListView is
           IListViewCallback. there are three implementation classes:
           TLCLListViewCallback/TLCLListBoxCallback/TLCLCheckboxListCallback

    5. TCocoaListView_CollectionView_StyleHandler
       TCocoaCollectionView supports three ViewStyles. in order to isolate the
       code of different ViewStyles, TCocoaListView_CollectionView_StyleHandler
       and its subclasses are added.
       5.1 vsIcon corresponds to TCocoaListView_CollectionView_LargeIconHandler
       5.2 vsSmallIcon corresponds to TCocoaListView_CollectionView_SmallIconHandler
       5.3 vsList corresponds to TCocoaListView_CollectionView_ListHandler
           (it uses a horizontal scroll bar)
  }

  { TLCLListViewCallback }

  TLCLListViewCallback = class(TLCLListControlCallback)
  public
    listView: TCustomListView;

    isSetTextFromWS: Integer; // allows to suppress the notifation about text change
                              // when initiated by Cocoa itself.
    ownerData: Boolean;

    function ItemsCount: Integer; override;
    function GetImageListType( out lvil: TListViewImageList ): Boolean; override;
    function GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean; override;
    function GetItemCheckedAt( row: Integer; var IsChecked: Integer): Boolean; override;
    function GetItemImageAt(ARow, ACol: Integer; var imgIdx: Integer): Boolean; override;
    function GetImageFromIndex(imgIdx: Integer): NSImage; override;
    procedure SetItemTextAt(ARow, ACol: Integer; const Text: String); override;
    procedure SetItemCheckedAt( row: Integer; IsChecked: Integer); override;
    function shouldSelectionChange(NewSel: Integer): Boolean; override;
    procedure ColumnClicked(ACol: Integer); override;
    function drawItem( row: Integer; ctx: TCocoaContext; const r: TRect;
      state: TOwnerDrawState): Boolean; override;
    function customDraw( row: Integer; col: Integer;
      ctx: TCocoaContext; state: TCustomDrawState ): Boolean; override;
    function isCustomDrawSupported: Boolean; override;
    procedure GetRowHeight(rowidx: Integer; var h: Integer); override;
    function GetBorderStyle: TBorderStyle; override;

    procedure selectOne(ARow: Integer; isSelected:Boolean );
    procedure callTargetInitializeWnd;
  end;
  TLCLListViewCallBackClass = class of TLCLListViewCallback;

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
    procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); virtual; abstract;
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
    procedure SetProperty( const AProp: TListViewProperty; const AIsSet: Boolean); virtual; abstract;
    procedure SetScrollBars( const AValue: TScrollStyle); virtual; abstract;
    procedure SetSort( const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); virtual; abstract;
  end;

  { TCocoaListViewBackendControl }
  TCocoaListViewBackendControlProtocol = objcprotocol
    procedure backend_setCallback( cb: TLCLListViewCallback ); message 'backend_setCallback:';
    procedure backend_reloadData; message 'backend_reloadData';
    procedure backend_onInit; message 'backend_onInit';
  end;

  CocoaListViewAllocFunc = procedure (const listView: NSView; const viewStyle: TViewStyle; out backendControl: NSView; out WSHandler: TCocoaWSListViewHandler );

  { TCocoaListView }

  TCocoaListView = objcclass(NSView)
  private
    _allocFunc: CocoaListViewAllocFunc;
    _viewStyle: TViewStyle;
    _scrollView: TCocoaScrollView;
    _backendControl: NSView; // NSTableView or NSCollectionView
    _WSHandler: TCocoaWSListViewHandler;
    _needsCallLclInit: Boolean;
    _initializing: Boolean;
    _captionEditor: TCocoaTextField;
    _captionFont: NSFont;
    _captionAlignment: NSTextAlignment;
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
    class function alloc: id; override;
    procedure dealloc; override;
  public
    procedure setAllocFunc( allocFunc: CocoaListViewAllocFunc ); message 'setAllocFunc:';
    procedure setViewStyle( viewStyle: TViewStyle ); message 'setViewStyle:';
    function documentView: NSView; message 'documentView';
    function scrollView: TCocoaScrollView; message 'scrollView';
    function WSHandler: TCocoaWSListViewHandler; message 'WSHandler';
    function initializing: Boolean; message 'isinitializing';

    function getLCLControlCanvas: TCanvas; message 'getLCLControlCanvas';
    procedure setCaptionEditor( captionEditor: TCocoaTextField ); message 'setCaptionEditor:';
    procedure setCaptionFont( captionFont: NSFont ); message 'setCaptionFont:';
    procedure setCaptionAlignment( alignment: NSTextAlignment ); message 'setCaptionAlignment:';
  end;

implementation

{ TCocoaListView }
type
  TCustomListViewAccess = class(TCustomListView);

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

function TCocoaListView.initializing: Boolean;
begin
  Result:= _initializing;
end;

function TCocoaListView.getLCLControlCanvas: TCanvas;
begin
  Result:= TCustomListView(self.callback.Target).Canvas;
end;

procedure TCocoaListView.setCaptionEditor(captionEditor: TCocoaTextField);
begin
  if Assigned(_captionEditor) then
    _captionEditor.release;

  _captionEditor:= captionEditor.retain;
  _captionEditor.setBezeled( False );
  _captionEditor.setFocusRingType( NSFocusRingTypeExterior );
  _captionEditor.setAlignment( _captionAlignment );
  _captionEditor.fixedInitSetting:= True;

  if Assigned(_captionFont) then
    _captionEditor.setFont( _captionFont );
end;

procedure TCocoaListView.setCaptionFont(captionFont: NSFont);
begin
  if Assigned(_captionFont) then
    _captionFont.release;
  _captionFont:= captionFont.retain;
  if Assigned(_captionEditor) then begin
    _captionEditor.removeFromSuperview;
    _backendControl.addSubview_positioned_relativeTo( _captionEditor, NSWindowAbove, nil );
    _captionEditor.setFont( _captionFont );
  end;
end;

procedure TCocoaListView.setCaptionAlignment( alignment: NSTextAlignment );
begin
  _captionAlignment:= alignment;
  _captionEditor.setAlignment( _captionAlignment );
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
  _allocFunc( self, _viewStyle, _backendControl, _WSHandler );

  controlFrame:= self.bounds;
  _backendControl.initWithFrame( controlFrame );
  _scrollView:= TCocoaScrollView.alloc.initWithFrame( controlFrame );
  _scrollView.setDocumentView( _backendControl );
  _scrollView.setAutoresizingMask( NSViewWidthSizable or NSViewHeightSizable );
  _scrollView.callback:= self.callback;
  self.addSubview_positioned_relativeTo( _scrollView, NSWindowBelow, nil );
  ScrollViewSetBorderStyle( _scrollView, callback.getBorderStyle );
  _scrollView.setFocusRingType( NSFocusRingTypeExterior );
  UpdateControlFocusRing( _backendControl, TWinControl(self.lclGetTarget) );

  backendControlAccess:= TCocoaListViewBackendControlProtocol(_backendControl);
  backendControlAccess.backend_setCallback( self.callback );
  backendControlAccess.backend_onInit;
end;

type
  TWinControlAccess = class(TWinControl);

procedure TCocoaListView.releaseControls;
begin
  if not Assigned(_backendControl) then
    Exit;
  FreeAndNil( _WSHandler );
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
    callback.callTargetInitializeWnd;
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
  _backendControl.lclClearCallback;
end;

function TCocoaListView.lclContentView: NSView;
begin
  Result:= documentView;
end;

class function TCocoaListView.alloc: id;
begin
  Result:=inherited alloc;
end;

procedure TCocoaListView.dealloc;
begin
  self.releaseControls;
  _captionEditor.release;
  _captionFont.release;
  inherited dealloc;
end;

procedure TCocoaListView.setAllocFunc( allocFunc: CocoaListViewAllocFunc );
begin
  _allocFunc:= allocFunc;
end;

{ TLCLListViewCallback }

function TLCLListViewCallback.ItemsCount: Integer;
begin
  Result:= listView.Items.Count;
end;

function TLCLListViewCallback.GetItemTextAt(ARow, ACol: Integer;
  var Text: String): Boolean;
begin
  Result := (ACol>=0) and ( (ACol<listView.ColumnCount) or (ACol=0) )
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

function TLCLListViewCallback.GetItemCheckedAt( row: Integer;
  var IsChecked: Integer): Boolean;
var
  BoolState : array [Boolean] of Integer = (NSOffState, NSOnState);
begin
  if ownerData and Assigned(listView) and (row>=0) and (row < listView.Items.Count) then
    IsChecked := BoolState[listView.Items[row].Checked]
  else
    Inherited GetItemCheckedAt( row, IsChecked );
  Result := true;
end;

function TLCLListViewCallback.GetItemImageAt(ARow, ACol: Integer;
  var imgIdx: Integer): Boolean;
begin
  imgIdx:= -1;
  Result := (ACol >= 0) and ( (ACol<listView.ColumnCount) or ( ACol=0) )
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
  lvil: TListViewImageList;
  lst : TCustomImageList;
  x,y : integer;
  img : NSImage;
  rep : NSBitmapImageRep;
  cb  : TCocoaBitmap;
begin
  Result:= nil;
  if imgIdx < 0 then
    Exit;

  if NOT self.GetImageListType( lvil ) then
    Exit;

  if lvil = lvilLarge then
    lst:= TCustomListViewAccess(listView).LargeImages
  else
    lst:= TCustomListViewAccess(listView).SmallImages;

  Result := AllocMultiResImageFromImageList(lst, lst.Width, imgIdx);
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

procedure TLCLListViewCallback.SetItemCheckedAt( row: Integer;
  IsChecked: Integer);
var
  Msg: TLMNotify;
  NMLV: TNMListView;
begin
  Inherited;

  FillChar(Msg{%H-}, SizeOf(Msg), #0);
  FillChar(NMLV{%H-}, SizeOf(NMLV), #0);

  Msg.Msg := CN_NOTIFY;

  NMLV.hdr.hwndfrom := ListView.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;
  NMLV.iItem := row;
  NMLV.iSubItem := 0;
  NMLV.uChanged := LVIF_STATE;
  Msg.NMHdr := @NMLV.hdr;

  LCLMessageGlue.DeliverMessage(ListView, Msg);
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
      NMLV.uNewState := LVIS_FOCUSED or LVIS_SELECTED;;
      NMLV.uOldState := 0;
    end else begin
      NMLV.uNewState := 0;
      NMLV.uOldState := LVIS_FOCUSED or LVIS_SELECTED;;
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

procedure TLCLListViewCallback.GetRowHeight(rowidx: Integer; var h: Integer);
begin

end;

function TLCLListViewCallback.GetBorderStyle: TBorderStyle;
begin
  Result:= TCustomListView(Target).BorderStyle;
end;

function TLCLListViewCallback.GetImageListType( out lvil: TListViewImageList ): Boolean;
const
  preferredImages: array [TViewStyle] of TListViewImageList = (
    lvilLarge, lvilSmall, lvilSmall, lvilSmall );
  alternativeImages: array [TViewStyle] of TListViewImageList = (
    lvilSmall, lvilLarge, lvilLarge, lvilLarge );
var
  viewStyle: TViewStyle;
  LVA: TCustomListViewAccess;
begin
  Result:= True;
  LVA:= TCustomListViewAccess(listView);
  viewStyle:= LVA.ViewStyle;

  lvil:= preferredImages[viewStyle];
  if (lvil=lvilLarge) and Assigned(LVA.LargeImages) then
    Exit;
  if (lvil=lvilSmall) and Assigned(LVA.SmallImages) then
    Exit;

  lvil:= alternativeImages[viewStyle];
  if (lvil=lvilLarge) and Assigned(LVA.LargeImages) then
    Exit;
  if (lvil=lvilSmall) and Assigned(LVA.SmallImages) then
    Exit;

  Result:= False;
end;

procedure TLCLListViewCallback.callTargetInitializeWnd;
begin
  TCustomListViewAccess(Target).InitializeWnd;
end;

function TLCLListViewCallback.drawItem( row: Integer; ctx: TCocoaContext;
  const r: TRect; state: TOwnerDrawState ): Boolean;
var
  Mess: TLMDrawListItem;
  DrawStruct: TDrawListItemStruct;
begin
  DrawStruct.ItemState := state;
  DrawStruct.Area := r;
  DrawStruct.DC := HDC(ctx);
  DrawStruct.ItemID := row;
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.Msg := CN_DRAWITEM;
  Mess.DrawListItemStruct := @DrawStruct;
  self.DeliverMessage( Mess );
  Result:= False;
end;

function TLCLListViewCallback.customDraw(row: Integer; col: Integer;
  ctx: TCocoaContext; state: TCustomDrawState ): Boolean;
var
  ALV: TCustomListViewAccess;
  drawTarget: TCustomDrawTarget;
  drawResult: TCustomDrawResult;
  rect: TRect;
begin
  ALV:= TCustomListViewAccess(self.listView);
  rect:= NSRectToRect( self.Owner.lclContentView.bounds );
  if col=0 then
    drawTarget:= dtItem
  else if col>0 then
    drawTarget:= dtSubItem
  else
    drawTarget:= dtControl;

  ALV.Canvas.Handle:= HDC(ctx);
  drawResult:= ALV.IntfCustomDraw( drawTarget, cdPrePaint, row, col, state, @rect );
  ALV.Canvas.Handle:= 0;
  Result:= cdrSkipDefault in drawResult;
end;

function TLCLListViewCallback.isCustomDrawSupported: Boolean;
begin
  Result:= True;
end;

end.

