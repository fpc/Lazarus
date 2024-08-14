{ $Id: $}
{                  --------------------------------------------
                  cocoatables.pas  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaTables;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_LISTVIEW}

interface

uses
  Classes, SysUtils,
  MacOSAll, CocoaAll,
  CocoaPrivate, Cocoa_Extra, CocoaCallback, CocoaConst, CocoaConfig,
  CocoaWSCommon, CocoaUtils, CocoaGDIObjects,
  CocoaListView, CocoaTextEdits,
  LCLType, LCLMessageGlue, LMessages, Controls, ComCtrls, StdCtrls, ImgList, Forms;

type

  { TCocoaStringList }

  TCocoaStringList = class(TStringList)
  protected
    procedure Changed; override;
  public
    Owner: NSTableView;
    // some notificaitons (i.e. selection change)
    // should not be passed to LCL while clearing
    isClearing: Boolean;
    constructor Create(AOwner: NSTableView);
    procedure Clear; override;
  end;

  { TCocoaTableListItem }

  TCocoaTableListItem = objcclass(NSTableCellView)
  private
    _tableView: NSTableView;
    _column: NSTableColumn;
    _checkBox: NSButton;
  private
    procedure createTextField; message 'createTextField';
    procedure createImageView; message 'createImageView';
    procedure createCheckBox; message 'createCheckBox';
  public
    procedure setColumn( column: NSTableColumn ); message 'setColumn:';
    function checkBox: NSButton; message 'checkBox';

    procedure drawRect(dirtyRect: NSRect); override;

    procedure loadView( row: Integer; col: Integer );
      message 'loadView:col:';
    procedure updateItemValue( row: NSInteger; col: NSInteger );
      message 'updateItemValue:col:';
    procedure updateItemLayout( row: NSInteger; col: NSInteger );
      message 'updateItemLayout:col:';

    procedure prepareForReuse; override;
    procedure dealloc; override;
  end;

  {
    1. TCocoaTableListView related need to support
       TListView/TListBox/TCheckListBox, etc.
    2. the differences between these controls can be considered to be
       implemented in the callback.
    3. however, after careful consideration, we tried to keep the original
       intention of the callback, and added TCocoaTableViewProcessor to
       isolate these differences.
  }
  { TCocoaTableViewProcessor }

  TCocoaTableViewProcessor = class
    function isInitializing( tv: NSTableView ): Boolean; virtual; abstract;
    procedure onReloadData( tv: NSTableView ); virtual; abstract;
    procedure onSelectOneItem( tv: NSTableView; selection: NSIndexSet ); virtual; abstract;
    procedure onSelectionChanged( tv: NSTableView ); virtual; abstract;
  end;

  { TCocoaTableListView }

  TCocoaTableListView = objcclass(
    NSTableView,
    NSTableViewDelegateProtocol,
    NSTableViewDataSourceProtocol,
    TCocoaListViewBackendControlProtocol )
  private
    _processor: TCocoaTableViewProcessor;
    _checkBoxes: Boolean;
  public
    iconSize: NSSize;
    callback: IListViewCallback;
    selectingByProgram: Boolean;
    readOnly: Boolean;
    isOwnerDraw : Boolean;
    isDynamicRowHeight: Boolean;
    CustomRowHeight: Integer;
    ScrollWidth: Integer;
  public
    procedure backend_setCallback( cb:TLCLListViewCallback );
    procedure backend_reloadData;
    procedure backend_onInit;
  public
    procedure addSubview(aView: NSView); override;
    procedure dealloc; override;

    procedure lclSetProcessor( processor: TCocoaTableViewProcessor ); message 'lclSetProcessor:';
    procedure lclSetCheckBoxes( checkBoxes: Boolean); message 'lclSetCheckBoxes:';
    function lclHasCheckBoxes: Boolean; message 'lclHasCheckBoxes';

    function tableView_viewForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
    function tableView_rowViewForRow(tableView: NSTableView; row: NSInteger): NSTableRowView;

    procedure checkboxAction(sender: NSButton); message 'checkboxAction:';
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;

    // Own methods, mostly convenience methods
    function getIndexOfColumn(ACol: NSTableColumn): Integer; message 'getIndexOfColumn:';

    procedure restoreFromStableSelection; message 'restoreFromStableSelection';
    procedure reloadData; override;
    procedure reloadDataForRow_column(ARow, ACol: NSInteger); message 'reloadDataForRow:column:';

    procedure selectOneItemByIndex( index: Integer; isSelected: Boolean );
      message 'selectOneItemByIndex:isSelected:';
    procedure selectRowIndexesByProgram( indexes: NSIndexSet );
      message 'selectRowIndexesByProgram:';

    function initWithFrame(frameRect: NSRect): id; override;
    function fittingSize: NSSize; override;

    procedure drawRect(dirtyRect: NSRect); override;
    function lclCallDrawItem( row: NSInteger; ctxSize: NSSize; clipRect: NSRect): Boolean;
      message 'lclCallDrawItem:ctxSize:clipRect:';
    function lclCallCustomDraw( row: Integer; col: Integer; ctxSize: NSSize; clipRect: NSRect): Boolean;
      message 'lclCallCustomDraw:col:ctxSize:clipRect:';

    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    // key
    procedure lclExpectedKeys(var wantTabs, wantKeys, wantReturn, wantAllKeys: Boolean); override;

    // NSTableViewDataSourceProtocol
    function numberOfRowsInTableView(tableView: NSTableView): NSInteger; message 'numberOfRowsInTableView:';
    //procedure tableView_sortDescriptorsDidChange(tableView: NSTableView; oldDescriptors: NSArray); message 'tableView:sortDescriptorsDidChange:';
    //function tableView_writeRowsWithIndexes_toPasteboard(tableView: NSTableView; rowIndexes: NSIndexSet; pboard: NSPasteboard): Boolean; message 'tableView:writeRowsWithIndexes:toPasteboard:';
    //function tableView_validateDrop_proposedRow_proposedDropOperation(tableView: NSTableView; info: NSDraggingInfoProtocol; row: NSInteger; dropOperation: NSTableViewDropOperation): NSDragOperation; message 'tableView:validateDrop:proposedRow:proposedDropOperation:';
    //function tableView_acceptDrop_row_dropOperation(tableView: NSTableView; info: NSDraggingInfoProtocol; row: NSInteger; dropOperation: NSTableViewDropOperation): Boolean; message 'tableView:acceptDrop:row:dropOperation:';
    //function tableView_namesOfPromisedFilesDroppedAtDestination_forDraggedRowsWithIndexes(tableView: NSTableView; dropDestination: NSURL; indexSet: NSIndexSet): NSArray; message 'tableView:namesOfPromisedFilesDroppedAtDestination:forDraggedRowsWithIndexes:';

    // NSTableViewDelegateProtocol
    //procedure tableView_willDisplayCell_forTableColumn_row(tableView: NSTableView; cell: id; tableColumn: NSTableColumn; row: NSInteger); message 'tableView:willDisplayCell:forTableColumn:row:';
    function tableView_shouldEditTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): Boolean; message 'tableView:shouldEditTableColumn:row:';
    function selectionShouldChangeInTableView(tableView: NSTableView): Boolean; message 'selectionShouldChangeInTableView:';
    function tableView_shouldSelectRow(tableView: NSTableView; row: NSInteger): Boolean; message 'tableView:shouldSelectRow:';
    {function tableView_selectionIndexesForProposedSelection(tableView: NSTableView; proposedSelectionIndexes: NSIndexSet): NSIndexSet; message 'tableView:selectionIndexesForProposedSelection:';
    function tableView_shouldSelectTableColumn(tableView: NSTableView; tableColumn: NSTableColumn): Boolean; message 'tableView:shouldSelectTableColumn:';
    procedure tableView_mouseDownInHeaderOfTableColumn(tableView: NSTableView; tableColumn: NSTableColumn); message 'tableView:mouseDownInHeaderOfTableColumn:';}
    procedure tableView_didClickTableColumn(tableView: NSTableView; tableColumn: NSTableColumn); message 'tableView:didClickTableColumn:';
    {procedure tableView_didDragTableColumn(tableView: NSTableView; tableColumn: NSTableColumn); message 'tableView:didDragTableColumn:';
    function tableView_toolTipForCell_rect_tableColumn_row_mouseLocation(tableView: NSTableView; cell: NSCell; rect: NSRectPointer; tableColumn: NSTableColumn; row: NSInteger; mouseLocation: NSPoint): NSString; message 'tableView:toolTipForCell:rect:tableColumn:row:mouseLocation:';}
    function tableView_heightOfRow(tableView: NSTableView; row: NSInteger): CGFloat; message 'tableView:heightOfRow:';
    {function tableView_typeSelectStringForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSString; message 'tableView:typeSelectStringForTableColumn:row:';
    function tableView_nextTypeSelectMatchFromRow_toRow_forString(tableView: NSTableView; startRow: NSInteger; endRow: NSInteger; searchString: NSString): NSInteger; message 'tableView:nextTypeSelectMatchFromRow:toRow:forString:';
    function tableView_shouldTypeSelectForEvent_withCurrentSearchString(tableView: NSTableView; event: NSEvent; searchString: NSString): Boolean; message 'tableView:shouldTypeSelectForEvent:withCurrentSearchString:';
    function tableView_shouldShowCellExpansionForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): Boolean; message 'tableView:shouldShowCellExpansionForTableColumn:row:';
    function tableView_shouldTrackCell_forTableColumn_row(tableView: NSTableView; cell: NSCell; tableColumn: NSTableColumn; row: NSInteger): Boolean; message 'tableView:shouldTrackCell:forTableColumn:row:';
    }
    {
    function tableView_isGroupRow(tableView: NSTableView; row: NSInteger): Boolean; message 'tableView:isGroupRow:';
    function tableView_sizeToFitWidthOfColumn(tableView: NSTableView; column: NSInteger): CGFloat; message 'tableView:sizeToFitWidthOfColumn:';
    function tableView_shouldReorderColumn_toColumn(tableView: NSTableView; columnIndex: NSInteger; newColumnIndex: NSInteger): Boolean; message 'tableView:shouldReorderColumn:toColumn:';}
    procedure tableViewSelectionDidChange(notification: NSNotification); message 'tableViewSelectionDidChange:';
    {procedure tableViewColumnDidMove(notification: NSNotification); message 'tableViewColumnDidMove:';}
    procedure tableViewColumnDidResize(notification: NSNotification); message 'tableViewColumnDidResize:';
    {procedure tableViewSelectionIsChanging(notification: NSNotification); message 'tableViewSelectionIsChanging:';}
  end;

  { TCocoaWSListView_TableViewHandler }

  TCocoaWSListView_TableViewHandler = class(TCocoaWSListViewHandler)
  private
    _listView: TCocoaListView;
    _tableView: TCocoaTableListView;
  private
    function getCallback: TLCLListViewCallback;
    procedure doReloadDataAfterDelete( AIndex: PtrInt );
  public
    constructor Create( listView: TCocoaListView );
    function getColumnFromIndex( const AIndex: Integer ): NSTableColumn;
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
    procedure SetProperty( const AProp: TListViewProperty; const AIsSet: Boolean); override;
    procedure SetScrollBars( const AValue: TScrollStyle); override;
    procedure SetSort( const {%H-}AType: TSortType; const {%H-}AColumn: Integer;
      const {%H-}ASortDirection: TSortDirection); override;
  end;

  { TCocoaTableListViewProcessor }

  TCocoaTableListViewProcessor = class( TCocoaTableViewProcessor )
  private
    function getCallback( tv: NSTableView ): TLCLListViewCallback;
  public
    function isInitializing( tv: NSTableView ): Boolean; override;
    procedure onReloadData( tv: NSTableView ); override;
    procedure onSelectOneItem( tv: NSTableView;  selection: NSIndexSet ); override;
    procedure onSelectionChanged( tv: NSTableView ); override;
  end;

function AllocCocoaTableListView: TCocoaTableListView;

function LCLCoordToRow(tbl: NSTableView; X,Y: Integer): Integer;
function LCLGetItemRect(tbl: NSTableView; row, col: Integer; var r: TRect): Boolean;
function LCLGetTopRow(tbl: NSTableView): Integer;

const
  DefaultRowHeight = 16; // per "rowHeight" property docs

implementation

type
  { TCocoaTableRowView }

  TCocoaTableRowView = objcclass(NSTableRowView)
  public
    tableView: TCocoaTableListView;
    row: Integer;
  public
    procedure drawRect(dirtyRect: NSRect); override;
  end;

function LCLCoordToRow(tbl: NSTableView; X,Y: Integer): Integer;
var
  pt : NSPoint;
  sc : NSScrollView;
  vr : NSRect;
begin
  if not Assigned(tbl) then
  begin
    Result := -1;
    Exit;
  end;

  sc := tbl.enclosingScrollView;
  if Assigned(sc) then
    vr := sc.documentVisibleRect
  else
    vr := tbl.visibleRect;
  pt.x := X;
  if tbl.isFlipped
    then pt.y := Y + vr.origin.y
    else pt.y := tbl.frame.size.height - Y - vr.origin.y;

  Result := tbl.rowAtPoint(pt);
end;

function LCLGetItemRect(tbl: NSTableView; row, col: Integer; var r: TRect): Boolean;
var
  nsr : NSRect;
begin
  if not Assigned(tbl) then begin
    Result := false;
    r := Bounds(0,0,0,0);
    Exit;
  end;
  nsr:=tbl.frameOfCellAtColumn_row(col,row);
  r:=NSRectToRect(nsr);
  Result := True;
end;

function LCLGetTopRow(tbl: NSTableView): Integer;
var
  visRange: NSRange;
begin
  if not Assigned(tbl) then
    Result := -1
  else
  begin
    visRange := tbl.rowsInRect(tbl.visibleRect());
    Result := visRange.location;
  end;
end;

function AllocCocoaTableListView: TCocoaTableListView;
begin
  // init will happen outside
  Result := TCocoaTableListView.alloc;
end;

procedure hideAllSubviews( parent: NSView );
var
  view: NSView;
begin
  for view in parent.subviews do
    view.setHidden( True );
end;

procedure TCocoaTableRowView.drawRect(dirtyRect: NSRect);
var
  done: Boolean;
begin
  done:= self.tableView.lclCallDrawItem( row , self.bounds.size, dirtyRect );

  if done then begin
    // the Cocoa default drawing cannot be skipped in NSTableView,
    // we can only hide the CellViews to get the same effect.
    // in the Lazarus IDE, there is a ListBox with OwnerDraw in Project-Forms,
    // it's a case where the default drawing must be skipped.
    hideAllSubviews( self );
  end else begin
    inherited drawRect( dirtyRect );
  end;
end;

{ TCocoaTableListView }

procedure TCocoaTableListView.lclExpectedKeys(var wantTabs, wantKeys, wantReturn,
  wantAllKeys: Boolean);
begin
  wantTabs := false;
  wantKeys := true;
  wantReturn := false; // todo: this should be "true" for editting purposes.
                       //       or false, to let LCL handle editting
  wantAllKeys := false;
end;

procedure TCocoaTableListView.lclSetCheckBoxes(checkBoxes: Boolean);
begin
  if _checkBoxes = checkBoxes then
    Exit;

  _checkBoxes:= checkBoxes;
  self.reloadData;
end;

function TCocoaTableListView.lclHasCheckBoxes: Boolean;
begin
  Result:= _checkBoxes;
end;

procedure TCocoaTableListView.backend_setCallback(cb: TLCLListViewCallback);
begin
  self.callback:= cb;
end;

procedure TCocoaTableListView.backend_reloadData;
var
  lclcb: TLCLListViewCallback;
begin
  self.reloadData;
  if Assigned(self.callback) then begin
    lclcb:= TLCLListViewCallback( self.callback.GetCallbackObject );
    self.selectRowIndexesByProgram( lclcb.selectionIndexSet );
  end;
end;

procedure TCocoaTableListView.backend_onInit;
var
  sz: NSSize;
begin
  self.setDataSource(self);
  self.setDelegate(self);
  self.setAllowsColumnReordering(False);
  self.setAllowsColumnSelection(False);

  UpdateFocusRing( self, self.callback.getBorderStyle );

  sz := self.intercellSpacing;
  // Windows compatibility. on Windows there's no extra space between columns
  sz.width := 0;
  self.setIntercellSpacing(sz);
end;

procedure TCocoaTableListView.addSubview(aView: NSView);
begin
  if NOT Assigned(self.callback) then
    Exit;
  if self.callback.onAddSubview(aView) then
    Exit;
  inherited addSubview(aView);
end;

procedure TCocoaTableListView.dealloc;
begin
  FreeAndNil( _processor );
end;

procedure TCocoaTableListView.lclSetProcessor( processor: TCocoaTableViewProcessor);
begin
  _processor:= processor;
end;

function TCocoaTableListView.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := NSViewCanFocus(Self);
end;

function TCocoaTableListView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTableListView.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaTableListView.fittingSize: NSSize;
begin
  Result:= NSZeroSize;
end;

function isFocused( tv: TCocoaTableListView ; row: NSInteger ): Boolean;
begin
  Result:= False;
  if Assigned(tv.window) and (tv.window.firstResponder = tv) then begin
    if row < 0 then
      Result:= True
    else if tv.isRowSelected(row) then
      Result:= True;
  end;
end;

function isChecked( tv: TCocoaTableListView ; row: NSInteger ): Boolean;
var
  checked: Integer;
begin
  Result:= False;
  if row < 0 then
    Exit;

  tv.callback.GetItemCheckedAt( row, checked );
  Result:= checked=NSOnState;
end;

function TCocoaTableListView.lclCallDrawItem(row: NSInteger;
  ctxSize: NSSize; clipRect: NSRect ): Boolean;
var
  ctx: TCocoaContext;
  ItemState: TOwnerDrawState;
begin
  Result:= False;
  if NOT self.isOwnerDraw then
    Exit;

  if not Assigned(callback) then Exit;
  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  ctx.InitDraw(Round(ctxSize.width), Round(ctxSize.height));
  try
    ItemState := [];
    if isRowSelected(row) then Include(ItemState, odSelected);
    if NOT lclIsEnabled then Include(ItemState, odDisabled);
    if isFocused(self,row) then
      Include(ItemState, odFocused);
    if isChecked(self,row) then
      Include(ItemState, odChecked);

    Result:= callback.drawItem(row, ctx, NSRectToRect(clipRect), ItemState);
  finally
    ctx.Free;
  end;
end;

function TCocoaTableListView.lclCallCustomDraw(row: Integer; col: Integer;
  ctxSize: NSSize; clipRect: NSRect): Boolean;
var
  ctx: TCocoaContext;
  state: TCustomDrawState;
begin
  Result:= False;
  if NOT Assigned(callback) then
    Exit;
  if NOT callback.isCustomDrawSupported then
    Exit;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  ctx.InitDraw(Round(ctxSize.width), Round(ctxSize.height));
  try
    state := [];
    if isRowSelected(row) then Include(state, cdsSelected);
    if NOT lclIsEnabled then Include(state, cdsDisabled);
    if isFocused(self,row) then
      Include(state, cdsFocused);
    if isChecked(self,row) then
      Include(state, cdsChecked);

    Result:= callback.customDraw(row, col, ctx, state);
  finally
    ctx.Free;
  end;
end;

procedure TCocoaTableListView.drawRect(dirtyRect: NSRect);
var
  done: Boolean;
begin
  if CheckMainThread and Assigned(callback) then
    callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);

  done:= self.lclCallCustomDraw( -1, -1, self.bounds.size, dirtyRect );

  if done then begin
    // the Cocoa default drawing cannot be skipped in NSTableView,
    // we can only hide the SubviewViews to get the same effect.
    hideAllSubviews( self );
  end else begin
    inherited drawRect( dirtyRect );
  end;
end;

function TCocoaTableListView.getIndexOfColumn(ACol: NSTableColumn): Integer;
var
  idx : NSUInteger;
begin
  idx := tableColumns.indexOfObject(ACol);
  if idx = NSNotFound then
    Result := -1
  else
    Result := Integer(idx);
end;

procedure TCocoaTableListView.restoreFromStableSelection;
var
  lclcb: TLCLListViewCallback;
begin
  if NOT Assigned(self.callback) then
    Exit;

  lclcb:= TLCLListViewCallback( self.callback.GetCallbackObject );
  self.selectRowIndexesByProgram( lclcb.selectionIndexSet );
end;

procedure TCocoaTableListView.reloadData;
begin
  if NOT Assigned(_processor) then
    Exit;
  if _processor.isInitializing(self) then
    Exit;

  inherited reloadData;
  _processor.onReloadData( self );
end;

procedure TCocoaTableListView.reloadDataForRow_column(ARow, ACol: NSInteger);
var
  lRowSet, lColSet: NSIndexSet;
begin
  lRowSet := NSIndexSet.indexSetWithIndex(ARow);
  lColSet := NSIndexSet.indexSetWithIndex(ACol);
  reloadDataForRowIndexes_columnIndexes(lRowSet, lColSet);
end;

procedure TCocoaTableListView.selectOneItemByIndex( index: Integer; isSelected: Boolean );
var
  selection: NSMutableIndexSet;
begin
  if (index < 0) or (index >= self.numberOfRows) then
    Exit;

  selection:= NSMutableIndexSet.alloc.initWithIndexSet( self.selectedRowIndexes );
  if isSelected then begin
    if NOT self.allowsMultipleSelection then
      selection.removeAllIndexes;
    selection.addIndex( index );
  end else begin
    selection.removeIndex( index );
  end;

  if NOT selection.isEqualToIndexSet(self.selectedRowIndexes) then begin
    if Assigned(_processor) then
      _processor.onSelectOneItem( self, selection );
    self.selectRowIndexesByProgram( selection );
  end;

  selection.release;
end;

procedure TCocoaTableListView.selectRowIndexesByProgram( indexes: NSIndexSet );
begin
  self.selectingByProgram:= True;
  self.selectRowIndexes_byExtendingSelection( indexes, False );
  self.selectingByProgram:= False;
end;

function TCocoaTableListView.initWithFrame(frameRect: NSRect): id;
begin
  Result:=inherited initWithFrame(frameRect);
  if NSAppkitVersionNumber >= NSAppKitVersionNumber11_0 then
    setStyle( CocoaConfig.CocoaTableViewStyle );
end;

procedure TCocoaTableListView.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    if Assigned(callback) then
      callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaTableListView.mouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaTableListView.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaTableListView.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaTableListView.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaTableListView.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaTableListView.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaTableListView.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaTableListView.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaTableListView.mouseMoved(event: NSEvent);
begin
  inherited mouseMoved(event);
end;

function TCocoaTableListView.numberOfRowsInTableView(tableView: NSTableView
  ): NSInteger;
begin
  if Assigned(callback) then
    Result := callback.ItemsCount
  else
    Result := 0;
end;

// TListView in LCL already supports editing, return False to avoid conflicts
function TCocoaTableListView.tableView_shouldEditTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): Boolean;
begin
  Result:= False;
end;

function TCocoaTableListView.selectionShouldChangeInTableView(
  tableView: NSTableView): Boolean;
begin
  Result:= true;
end;

function TCocoaTableListView.tableView_shouldSelectRow(tableView: NSTableView;
  row: NSInteger): Boolean;
begin
  Result:= callback.shouldSelectionChange( row );
end;

procedure TCocoaTableListView.tableView_didClickTableColumn(
  tableView: NSTableView; tableColumn: NSTableColumn);
begin
  if Assigned(callback) then
    callback.ColumnClicked(getIndexOfColumn(tableColumn));
end;

function TCocoaTableListView.tableView_heightOfRow(tableView: NSTableView;
  row: NSInteger): CGFloat;
var
  h : integer;
begin
  h := CustomRowHeight;
  if h = 0 then h := DefaultRowHeight;

  if isDynamicRowHeight and Assigned(callback) then
  begin
    callback.GetRowHeight(Integer(row), h);
    if h<=0 then h:=1; // must be positive (non-zero)
  end;
  Result := h;
end;

type
  TCompareData = record
    rmved : NSMutableIndexSet;
    added : NSMutableIndexSet;
    src   : NSIndexSet;
    dst   : NSIndexSet;
  end;

procedure DumpIndexes(src: NSIndexSet; dst: NSMutableIndexSet; rng: NSRange);
var
  buf : array [0..512 * 4-1] of NSUInteger;
  i   : NSUInteger;
  j   : Integer;
  trg : NSUInteger;
  r   : NSRange;
  gth : NSRange;
  k   : Integer;
  cmp : NSUInteger;
begin
  if src.containsIndexesInRange(rng) then begin
    // the range is packed
    dst.addIndexesInRange(rng);
    Exit;
  end;

  i := rng.location;
  trg := rng.location+rng.length;
  gth.location := NSNotFound;
  gth.length := 0;

  while i<=trg do begin
    r.location := i;
    r.length := trg - r.location;
    j := Integer(src.getIndexes_maxCount_inIndexRange(@buf[0], length(buf), @r));
    if j = 0 then i:=trg+1
    else
    begin
      i := buf[j-1]+1;
      if (gth.location = NSNotFound) then gth.location := buf[0];

      cmp := gth.length + gth.location;

      for k := 0 to j - 1 do begin
        if cmp = buf[k] then
          inc(cmp)
        else begin
          gth.length := cmp - gth.location + 1;
          dst.addIndexesInRange(gth);

          if k < j-1 then
            gth.location := buf[k]
          else
            gth.location := NSNotFound;
          gth.length := 0;

          cmp := gth.length + gth.location;
        end;
      end;
    end;
  end;

  if gth.location <> NSNotFound then begin
    gth.length := gth.length + 1;
    dst.addIndexesInRange(gth);
  end;
end;

procedure CompareIdxRange(const data: TCompareData; startIdx, endIdx: NSUInteger);
var
  isrc, idst: Boolean;
  rng : NSRange;
  m : Integer;
begin
  rng.location := startIdx;
  rng.length := endIdx - startIdx + 1;
  isrc := data.src.intersectsIndexesInRange(rng);
  idst := data.dst.intersectsIndexesInRange(rng);
  if (not isrc) and (not idst) then
    // there are no indexes in either selection
    // no need to dig deeper
    Exit;


  if (not isrc) then begin
    // there are only added indexes in this range
    DumpIndexes(data.dst, data.added, rng);
    Exit;
  end else if not (idst) then begin
    // there are only removed indexes in this range
    DumpIndexes(data.src, data.rmved, rng);
    Exit;
  end;

  isrc := data.src.containsIndexesInRange(rng);
  idst := data.dst.containsIndexesInRange(rng);
  if isrc and idst then begin
    // indexes match in both sets. No need for further investigation
    Exit;
  end;

  if startIdx < endIdx then
  begin
    m := (endidx - startidx) div 2 + startIdx;
    CompareIdxRange(data, startIdx, m);
    if m <= endIdx then
      CompareIdxRange(data, m+1, endIdx);
  end;
end;

procedure CompareIndexSets(src, dst: NSIndexSet; out removed, added: NSIndexSet);
var
  rm, ad : NSMutableIndexSet;
  srci : NSUInteger;
  dsti : NSUinteger;

  srcl : NSUInteger;
  dstl : NSUInteger;
  data : TCompareData;
begin
  rm := NSMutableIndexSet.alloc.init.autorelease;
  ad := NSMutableIndexSet.alloc.init.autorelease;
  removed := rm;
  added := ad;

  srci := src.firstIndex;
  dsti := dst.firstIndex;
  if (srci = NSNotFound) and (dsti <> NSNotFound) then begin
    // has not been previosly selected;
    ad.addIndexes(dst);
    Exit;
  end else if (dsti = NSNotFound) and (srci <> NSNotFound) then begin
    // cleared the selection
    rm.addIndexes(src);
    Exit;
  end;

  if srci < dsti then begin
    DumpIndexes(src, rm, NSMakeRange(srci, dsti-srci));
    srci := dsti;
  end else if dsti < srci  then begin
    DumpIndexes(dst, ad, NSMakeRange(dsti, srci-dsti));
    dsti := srci;
  end;

  srcl := src.lastIndex;
  dstl := dst.lastIndex;
  if srcl > dstl then begin
    DumpIndexes(src, rm, NSMakeRange(dstl+1, srcl - dstl));
    srcl := dstl;
  end else if dstl > srcl then begin
    DumpIndexes(dst, ad, NSMakeRange(srcl+1, dstl - srcl));
    dstl := srcl;
  end;

  if srci <= srcl then begin
    data.rmved := rm;
    data.added := ad;
    data.src := src;
    data.dst := dst;
    CompareIdxRange( data, srci, srcl);
  end;
end;

procedure sendSelectionChangedMsgToLCL(
  lclListView: TCustomListView;
  NewSel: Integer; Added, Removed: NSIndexSet);
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
        LCLMessageGlue.DeliverMessage(lclListView, Msg);
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

  NMLV.hdr.hwndfrom := lclListView.Handle;
  NMLV.hdr.code := LVN_ITEMCHANGED;
  NMLV.iSubItem := 0;
  NMLV.uChanged := LVIF_STATE;
  Msg.NMHdr := @NMLV.hdr;

  if Removed.count>0 then
  begin
    NMLV.uNewState := 0;
    NMLV.uOldState := LVIS_FOCUSED or LVIS_SELECTED;
    RunIndex( Removed );
  end;
  if Added.count > 0 then begin
    NMLV.uNewState := LVIS_FOCUSED or LVIS_SELECTED;;
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

  LCLMessageGlue.DeliverMessage(lclListView, Msg);}
end;

procedure TCocoaTableListView.tableViewSelectionDidChange(notification: NSNotification);
begin
  if Assigned(_processor) then
    _processor.onSelectionChanged( self );
end;

procedure TCocoaTableListView.tableViewColumnDidResize(
  notification: NSNotification);
begin
  self.reloadData;
end;

{ TCocoaStringList }

procedure TCocoaStringList.Changed;
begin
  inherited Changed;
  Owner.reloadData;
end;

constructor TCocoaStringList.Create(AOwner: NSTableView);
begin
  Owner:=AOwner;
  inherited Create;
end;

procedure TCocoaStringList.Clear;
begin
  isClearing := true;
  try
    inherited Clear;
  finally
    isClearing := false;
  end;
end;

{ TCocoaTableListItem }

function TCocoaTableListItem.checkBox: NSButton;
begin
  Result:= _checkBox;
end;

procedure TCocoaTableListItem.drawRect(dirtyRect: NSRect);
var
  row: Integer;
  col: Integer;
  done: Boolean;
begin
  row:= _tableView.rowForView( self );
  col:= _tableView.columnForView( self );
  done:= TCocoaTableListView(_tableView).lclCallCustomDraw(
            row, col, self.bounds.size, dirtyRect );

  if done then begin
    // the Cocoa default drawing cannot be skipped in NSTableView,
    // we can only hide the CellViews to get the same effect.
    // in the Lazarus IDE, there is a ListView with OnCustomDrawItem
    // in Perferences-Component Palette.
    hideAllSubviews( self );
  end else begin
    inherited drawRect(dirtyRect);
  end;
end;

procedure TCocoaTableListItem.createTextField;
var
  fieldControl: NSTextField;
begin
  if Assigned(self.textField) then
    Exit;

  fieldControl:= NSTextField.alloc.initWithFrame(NSZeroRect);
  fieldControl.setBordered( False );
  fieldControl.setDrawsBackground( False );
  fieldControl.setEditable( False );
  fieldControl.setLineBreakMode( NSLineBreakByTruncatingTail );
  self.setTextField( fieldControl );
  self.addSubview( fieldControl );
end;

procedure TCocoaTableListItem.createImageView;
var
  imageControl: NSImageView;
begin
  if Assigned(self.imageView) then
    Exit;

  imageControl:= NSImageView.alloc.initWithFrame( NSZeroRect );
  imageControl.setImageScaling( NSImageScaleProportionallyUpOrDown );
  self.setImageView( imageControl );
  self.addSubview( imageControl );
end;

procedure TCocoaTableListItem.createCheckBox;
begin
  if Assigned(_checkBox) then
    Exit;

  _checkBox:= NSButton.alloc.init;
  _checkBox.setButtonType( NSSwitchButton );
  _checkBox.setTitle( CocoaConst.NSSTR_EMPTY );
  _checkBox.setTarget( _tableView );
  _checkBox.setAction( ObjCSelector('checkboxAction:') );
  self.addSubview( _checkBox );
end;

procedure TCocoaTableListItem.setColumn(column: NSTableColumn);
begin
  _tableView:= column.tableView;
  _column:= column;
end;

procedure TCocoaTableListItem.loadView( row: Integer; col: Integer );
var
  tv: TCocoaTableListView;
  lclcb: IListViewCallback;
  lclImageIndex: Integer;
  lvil: TListViewImageList;
begin
  tv:= TCocoaTableListView( _tableView );
  lclcb:= tv.callback;

  self.createTextField;

  if col=0 then begin
    if lclcb.GetImageListType(lvil) then
      self.createImageView;
  end else begin
    lclcb.GetItemImageAt( row, col, lclImageIndex );
    if lclImageIndex >= 0 then
      self.createImageView;
  end;

  if (col=0) and tv.lclHasCheckBoxes then
    self.createCheckBox;
end;

procedure TCocoaTableListItem.updateItemValue(row: NSInteger; col: NSInteger );
var
  tv: TCocoaTableListView;
  lclcb: IListViewCallback;
  checkedValue: Integer;
  cocoaImage: NSImage;
  lclImageIndex: Integer;
  lclText: String;
begin
  tv:= TCocoaTableListView( _tableView );
  lclcb:= tv.callback;

  if Assigned(_checkBox) then begin
    lclcb.GetItemCheckedAt( row, checkedValue );
    _checkBox.setState( checkedValue );
  end;

  if Assigned(self.imageView) then begin
    lclcb.GetItemImageAt( row, col, lclImageIndex );
    if lclImageIndex >= 0 then begin
      cocoaImage:= lclcb.GetImageFromIndex( lclImageIndex );
      self.imageView.setImage( cocoaImage );
    end;
  end;

  lclcb.GetItemTextAt( row, col, lclText );
  self.textField.setStringValue( StrToNSString(lclText) );
end;

procedure TCocoaTableListItem.updateItemLayout(row: NSInteger; col: NSInteger );
var
  tv: TCocoaTableListView;
  aFrame: NSRect;
  rowHeight: CGFloat;
begin
  tv:= TCocoaTableListView( _tableView );

  aFrame:= NSZeroRect;
  rowHeight:= tv.tableView_heightOfRow( tv, row );

  if Assigned(_checkBox) then begin
    aFrame.size.width:= 18;
    aFrame.size.height:= 18;
    aFrame.origin.x:= 0;
    aFrame.origin.y:= (rowHeight - aFrame.size.height ) / 2;
    _checkBox.setFrame( aFrame );

    aFrame.origin.x:= 4;
  end;

  if Assigned(self.imageView) then begin
    aFrame.origin.x:= aFrame.origin.x + aFrame.size.width;
    aFrame.origin.y:= (rowHeight - tv.iconSize.Height) / 2;
    aFrame.size:= tv.iconSize;
    self.imageView.setFrame( aFrame );

    aFrame.origin.x:= aFrame.origin.x + 4;
  end;

  if Assigned(self.textField) then begin
    aFrame.size.height:= self.textField.frame.size.height;
    aFrame.origin.x:= aFrame.origin.x + aFrame.size.width;
    aFrame.origin.y:= (rowHeight - 15) / 2;
    aFrame.size.width:= _column.width - aFrame.origin.x;
    if aFrame.size.width < 16 then
      aFrame.size.width:= 16;
    aFrame.size.height:= 15;
    self.textField.setFrame( aFrame );
  end;
end;

procedure TCocoaTableListItem.prepareForReuse;
begin
  Inherited;

  if Assigned(self.imageView) then begin
    self.imageView.removeFromSuperview;
    self.imageView.release;
    self.setImageView(nil);
  end;

  if Assigned(_checkBox) then begin
    _checkBox.removeFromSuperview;
    _checkBox.release;
    _checkBox:= nil;
  end;

  self.removeFromSuperview;
end;

procedure TCocoaTableListItem.dealloc;
begin
  self.textField.removeFromSuperview;
  self.textField.release;

  if Assigned(self.imageView) then begin
    self.imageView.removeFromSuperview;
    self.imageView.release;
  end;

  if Assigned(_checkBox) then begin
    _checkBox.removeFromSuperview;
    _checkBox.release;
  end;

  inherited dealloc;
end;

function TCocoaTableListView.tableView_viewForTableColumn_row(tableView: NSTableView;
  tableColumn: NSTableColumn; row: NSInteger): NSView;
var
  col: NSInteger;
  frameRect: NSRect;
  item: TCocoaTableListItem;
  StrValue: NSString;
  chkst: Integer;
  txt: String;
begin
  Result:= nil;
  if row >= numberOfRowsInTableView(self) then
    Exit;

  frameRect.origin:= GetNSPoint(0,0);
  frameRect.size:= GetNSSize(tableColumn.width, rowHeight);

  item:= TCocoaTableListItem(makeViewWithIdentifier_owner(NSSTR('tblview'), self));
  if item = nil then begin
    item:= TCocoaTableListItem.alloc.initWithFrame(frameRect);
    item.setidentifier(NSSTR('tblview'));
  end;

  col:= tableColumns.indexOfObject (tableColumn );
  item.setColumn( tableColumn );
  item.loadView( row, col );
  item.updateItemValue( row, col );
  item.updateItemLayout( row, col );

  Result:= item;
end;

function TCocoaTableListView.tableView_rowViewForRow(
  tableView: NSTableView; row: NSInteger): NSTableRowView;
var
  rowView: TCocoaTableRowView Absolute Result;
begin
  Result:= TCocoaTableRowView.alloc.init;
  rowView.tableView:= self;
  rowView.row:= row;
end;

procedure TCocoaTableListView.checkboxAction(sender: NSButton);
var
  row: NSInteger;
begin
  if not Assigned(callback) then Exit;

  row := rowForView(sender.superview);
  callback.SetItemCheckedAt(row, sender.state);
  if sender.state = NSOnState then begin
    self.selectOneItemByIndex(row, True);
    self.window.makeFirstResponder( self );
  end;
end;

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

procedure TCocoaWSListView_TableViewHandler.doReloadDataAfterDelete( AIndex: PtrInt);
var
  lclcb : TLCLListViewCallback;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.checkedIndexSet.shiftIndexesStartingAtIndex_by( AIndex+1, -1);
  lclcb.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex+1, -1 );
  _tableView.selectRowIndexesByProgram( lclcb.selectionIndexSet );
  _tableView.reloadData;
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
begin
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
begin
  Application.QueueAsyncCall( @doReloadDataAfterDelete, AIndex );
end;

function TCocoaWSListView_TableViewHandler.ItemDisplayRect(const AIndex,
  ASubItem: Integer; ACode: TDisplayCode): TRect;
var
  item: TCocoaTableListItem;
  frame: NSRect;
  rect: TRect;
begin
  Result:= Bounds(0,0,0,0);
  item:= _tableView.viewAtColumn_row_makeIfNecessary( ASubItem, AIndex, True );
  if NOT Assigned(item) then
    Exit;

  frame:= item.frame;
  case ACode of
    drLabel:
      begin
        frame:= item.textField.frame;
        frame.origin.y:= frame.origin.y + 2;
        NSToLCLRect( frame, item.frame.size.height, rect );
        item.lclLocalToScreen( rect.left, rect.top );
        _listView.lclScreenToLocal( rect.left, rect.top );
        frame.origin.x:= rect.left;
        frame.origin.y:= rect.top;
      end;
    drIcon:
      begin
        if Assigned(item.imageView) then begin
          frame:= item.imageView.frame;
        end;
      end;
  end;

  Result:= NSRectToRect( frame );
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

  if TCocoaListView(lclcb.Owner).initializing then
    Exit;

  lclcb.checkedIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  lclcb.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  _tableView.selectRowIndexesByProgram( lclcb.selectionIndexSet );
  _tableView.reloadData;
  _tableView.sizeToFit();
end;

procedure TCocoaWSListView_TableViewHandler.ItemSetChecked(
  const AIndex: Integer; const AItem: TListItem; const AChecked: Boolean);
begin
  _tableView.reloadDataForRow_column(AIndex, 0);
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
  lclcb: TLCLListViewCallback;
begin
  lclcb:= self.getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  case AState of
    lisFocused,
    lisSelected: begin
      if lclcb.getItemStableSelection(AIndex) <> AIsSet then begin
        _tableView.selectOneItemByIndex( AIndex, AIsSet );
      end;
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
    _tableView.CustomRowHeight:= AValue;
  // setRowSizeStyle could be used here but is available only in 10.7+
end;

procedure TCocoaWSListView_TableViewHandler.SetImageList(
  const AList: TListViewImageList; const AValue: TCustomImageListResolution);
var
  lclcb: TLCLListViewCallback;
  lvil: TListViewImageList;
  spacing: NSSize;
begin
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  if NOT lclcb.GetImageListType(lvil) then
    Exit;

  if AList <> lvil then
    Exit;

  if NOT Assigned(AValue) then
    Exit;

  _tableView.iconSize.Width:= AValue.Width;
  _tableView.iconSize.Height:= AValue.Height;
  _tableView.CustomRowHeight:= AValue.Height + 8;

  _tableView.reloadData;
end;

procedure TCocoaWSListView_TableViewHandler.SetItemsCount(
  const Avalue: Integer);
begin
  _tableView.noteNumberOfRowsChanged();
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
    lvpCheckboxes: _tableView.lclSetCheckboxes(AIsSet);
   // lvpColumnClick: lTableLV.setAllowsColumnSelection(AIsSet);
  {  lvpFlatScrollBars,
    lvpFullDrag,}
    lvpGridLines: _tableView.setGridStyleMask(GridStyle[AIsSet]);
    {lvpHideSelection,
    lvpHotTrack,}
    lvpMultiSelect: _tableView.setAllowsMultipleSelection(AIsSet);
    lvpOwnerDraw: _tableView.isOwnerDraw:= AIsSet;
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
begin
  _tableView.deselectAll(nil);
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

{ TCocoaTableListViewProcessor }

function TCocoaTableListViewProcessor.getCallback( tv: NSTableView ): TLCLListViewCallback;
var
  cocoaTLV: TCocoaTableListView Absolute tv;
begin
  Result:= TLCLListViewCallback( cocoaTLV.callback.GetCallbackObject );
end;

function TCocoaTableListViewProcessor.isInitializing( tv: NSTableView ): Boolean;
var
  cocoaTLV: TCocoaTableListView Absolute tv;
begin
  Result:= False;
  if NOT Assigned(cocoaTLV.callback) then
    Exit;
  Result:= TCocoaListView( self.getCallback(tv).Owner).initializing;
end;

procedure TCocoaTableListViewProcessor.onReloadData( tv: NSTableView );
begin
  tv.cancelPreviousPerformRequestsWithTarget_selector_object(
    tv, ObjcSelector('restoreFromStableSelection'), nil );
  tv.performSelector_withObject_afterDelay(
    ObjcSelector('restoreFromStableSelection'), nil, 0 );
end;

procedure TCocoaTableListViewProcessor.onSelectOneItem(tv: NSTableView;
  selection: NSIndexSet);
var
  lclcb: TLCLListViewCallback;
begin
  lclcb:= self.getCallback(tv);
  if NOT Assigned(lclcb) then
    Exit;

  lclcb.selectionIndexSet.removeAllIndexes;
  lclcb.selectionIndexSet.addIndexes( selection );
end;

procedure TCocoaTableListViewProcessor.onSelectionChanged(tv: NSTableView);
var
  NewSel: Integer;
  rm : NSIndexSet;
  ad : NSIndexSet;
  selectionIndexSet: NSMutableIndexSet;

  lclListView: TCustomListView;
  cocoaTLV: TCocoaTableListView Absolute tv;
  lclcb: TLCLListViewCallback;
begin
  if NOT Assigned(cocoaTLV.callback) then
    Exit;

  lclcb:= self.getCallback( tv );
  lclListView:= TCustomListView( lclcb.Target );

  if self.isInitializing(tv) then
    Exit;

  selectionIndexSet:= lclcb.selectionIndexSet;
  CompareIndexSets(selectionIndexSet, cocoaTLV.selectedRowIndexes, rm, ad);

  NewSel := cocoaTLV.selectedRow();
  sendSelectionChangedMsgToLCL( lclListView, NewSel, ad, rm );

  if NOT cocoaTLV.selectingByProgram then begin
    selectionIndexSet.removeAllIndexes;
    selectionIndexSet.addIndexes( cocoaTLV.selectedRowIndexes );
  end;
end;

end.

