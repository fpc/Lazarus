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
  LCLType, LCLMessageGlue, LMessages, Controls, Graphics,
  ComCtrls, StdCtrls, ImgList, Forms,
  MacOSAll, CocoaAll,
  CocoaPrivate, Cocoa_Extra, CocoaCallback, CocoaListControl,
  CocoaConst, CocoaConfig, CocoaWSCommon, CocoaUtils, CocoaGDIObjects,
  CocoaListView, CocoaTextEdits;

type

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
    function fittingSize: NSSize; override;

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

  { TCocoaTableListView }

  TCocoaTableListView = objcclass(
    NSTableView,
    NSTableViewDelegateProtocol,
    NSTableViewDataSourceProtocol,
    TCocoaListViewBackendControlProtocol )
  private
    _coocaInitializing: Boolean;
  private
    _processor: TCocoaTableViewProcessor;
    _checkBoxes: Boolean;
    _checkBoxAllowsMixed: Boolean;
  public
    iconSize: NSSize;
    callback: TLCLListControlCallback;
    selectingByProgram: Boolean;
    dontSendOnChangeMessage: Boolean;
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

    function lclGetPorcessor: TCocoaTableViewProcessor; message 'lclGetPorcessor';
    procedure lclSetProcessor( processor: TCocoaTableViewProcessor ); message 'lclSetProcessor:';
    procedure lclSetCheckBoxes( checkBoxes: Boolean ); message 'lclSetCheckBoxes:';
    function lclHasCheckBoxes: Boolean; message 'lclHasCheckBoxes';
    procedure lclSetCheckBoxAllowsMixed( allowsMixed: Boolean ); message 'lclSetCheckBoxAllowsMixed:';
    function lclCheckBoxAllowsMixed: Boolean; message 'lclCheckBoxAllowsMixed';
    function lclGetCanvas: TCanvas; message 'lclGetCanvas';

    function tableView_viewForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView;
    function tableView_rowViewForRow(tableView: NSTableView; row: NSInteger): NSTableRowView;

    procedure lclInsertItem(const AIndex: Integer); message 'lclInsertItem:';
    procedure lclDeleteItem(const AIndex: Integer); message 'lclDeleteItem:';
    procedure lclExchangeItem(const AIndex1: Integer; const AIndex2: Integer); message 'lclExchangeItem:AIndex2:';
    procedure lclClearItem; message 'lclClearItem';
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
    procedure setNeedsDisplayInRect(invalidRect: NSRect); override;
    function lclCallDrawItem( row: NSInteger; canvasRect: NSRect ): Boolean;
      message 'lclCallDrawItem:canvasRect:';
    function lclCallCustomDraw( row: Integer; col: Integer; canvasRect: NSRect ): Boolean;
      message 'lclCallCustomDraw:col:canvasRect:';

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
    function tableView_isGroupRow(tableView: NSTableView; row: NSInteger): Boolean; message 'tableView:isGroupRow:';}
    function tableView_sizeToFitWidthOfColumn(tableView: NSTableView; column: NSInteger): CGFloat;
    {function tableView_shouldReorderColumn_toColumn(tableView: NSTableView; columnIndex: NSInteger; newColumnIndex: NSInteger): Boolean; message 'tableView:shouldReorderColumn:toColumn:';}
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
    procedure ItemExchange(const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer); override;
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

  TCocoaTableListViewProcessor = class( TCocoaTableListControlProcessor )
  public
    function isInitializing( tv: NSTableView ): Boolean; override;
    function getLCLControlCanvas( tv: NSTableView ): TCanvas; override;
    procedure onSelectionChanged( tv: NSTableView ); override;
  end;

function AllocCocoaTableListView: TCocoaTableListView;

function LCLCoordToRow(tbl: NSTableView; X,Y: Integer): Integer;
function LCLGetItemRect(tbl: NSTableView; row, col: Integer; var r: TRect): Boolean;
function LCLGetTopRow(tbl: NSTableView): Integer;

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

function updateNSTextFieldWithTFont( cocoaField: NSTextField; lclFont: TFont ):
  Boolean;
var
  saveFontColor: TColor;
  cocoaFont: NSFont;
  cocoaColor: NSColor;
begin
  Result:= False;
  saveFontColor:= lclFont.Color;

  lclFont.Color:= clDefault;
  if NOT lclFont.isDefault then begin
    cocoaFont:= TCocoaFont(lclFont.Reference.Handle).Font;
    cocoaField.setFont( cocoaFont );
    Result:= True;
  end;

  lclFont.Color:= saveFontColor;
  if lclFont.Color <> clDefault then begin
    cocoaColor:= ColorToNSColor(ColorToRGB(lclFont.Color));
    cocoaField.setTextColor( cocoaColor );
  end;
end;

procedure drawNSViewBackground( view: NSView; lclBrush: TBrush );
var
  ctx: TCocoaContext;
  cocoaBrush: TCocoaBrush;
  width: Integer;
  height: Integer;
begin
  if lclBrush.Color = clWhite then   // see also TBrush.create
    Exit;

  width:= Round( view.bounds.size.width );
  height:= Round( view.bounds.size.height );

  ctx := TCocoaContext.Create( NSGraphicsContext.currentContext );
  ctx.InitDraw( width, height );
  try
    cocoaBrush:= TCocoaBrush( lclBrush.Reference.Handle );
    ctx.Rectangle( 0, 0, width, height, True, cocoaBrush );
  finally
    ctx.Free;
  end;
end;

procedure TCocoaTableRowView.drawRect(dirtyRect: NSRect);
var
  done: Boolean;
begin
  if NOT self.tableView.isOwnerDraw then begin
    inherited drawRect( dirtyRect );
    Exit;
  end;

  done:= self.tableView.lclCallDrawItem( row, self.bounds );

  if done then begin
    // the Cocoa default drawing cannot be skipped in NSTableView,
    // we can only hide the CellViews to get the same effect.
    // in the Lazarus IDE, there is a ListBox with OwnerDraw in Project-Forms,
    // it's a case where the default drawing must be skipped.
    if Assigned(self.tableView.lclGetPorcessor) then
      self.tableView.lclGetPorcessor.onOwnerDrawItem( self );
  end else begin
    drawNSViewBackground( self, tableView.lclGetCanvas.Brush );
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

procedure TCocoaTableListView.lclSetCheckBoxAllowsMixed(allowsMixed: Boolean);
begin
  _checkBoxAllowsMixed:= allowsMixed;
end;

function TCocoaTableListView.lclCheckBoxAllowsMixed: Boolean;
begin
  Result:= _checkBoxAllowsMixed;
end;

procedure TCocoaTableListView.backend_setCallback(cb: TLCLListViewCallback);
begin
  self.callback:= cb;
end;

procedure TCocoaTableListView.backend_reloadData;
begin
  self.reloadData;
end;

procedure TCocoaTableListView.backend_onInit;
var
  sz: NSSize;
begin
  self.setDataSource(self);
  self.setDelegate(self);
  self.setAllowsColumnReordering(False);
  self.setAllowsColumnSelection(False);

  sz := self.intercellSpacing;
  // Windows compatibility. on Windows there's no extra space between columns
  sz.width := 0;
  self.setIntercellSpacing(sz);
end;

procedure TCocoaTableListView.addSubview(aView: NSView);
var
  textField: TCocoaTextField Absolute aView;
begin
  inherited;
  if NOT aView.isKindOfClass(TCocoaTextField) then
    Exit;
  if NOT Assigned(self.callback) then
    Exit;
  if NOT (self.callback.Owner.isKindOfClass(TCocoaListView)) then
    Exit;
  TCocoaListView(self.callback.Owner).setCaptionEditor( textField );
end;

function TCocoaTableListView.lclGetCanvas: TCanvas;
begin
  Result:= _processor.getLCLControlCanvas( self );
end;

procedure TCocoaTableListView.dealloc;
begin
  FreeAndNil( _processor );
end;

function TCocoaTableListView.lclGetPorcessor: TCocoaTableViewProcessor;
begin
  Result:= _processor;
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
  Result:= checked<>NSOffState;
end;

function TCocoaTableListView.lclCallDrawItem(row: NSInteger;
  canvasRect: NSRect ): Boolean;
var
  ctx: TCocoaContext;
  ItemState: TOwnerDrawState;
begin
  Result:= False;
  if NOT self.isOwnerDraw then
    Exit;
  if not Assigned(self.callback) then
    Exit;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  ctx.InitDraw(Round(canvasRect.size.width), Round(canvasRect.size.height));
  try
    ItemState := [];
    if isRowSelected(row) then Include(ItemState, odSelected);
    if NOT lclIsEnabled then Include(ItemState, odDisabled);
    if isFocused(self,row) then
      Include(ItemState, odFocused);
    if isChecked(self,row) then
      Include(ItemState, odChecked);

    Result:= self.callback.drawItem(row, ctx, NSRectToRect(canvasRect), ItemState);
  finally
    ctx.Free;
  end;
end;

function TCocoaTableListView.lclCallCustomDraw(row: Integer; col: Integer;
  canvasRect: NSRect ): Boolean;
var
  ctx: TCocoaContext;
  state: TCustomDrawState;
begin
  Result:= False;
  if NOT Assigned(self.callback) then
    Exit;
  if NOT self.callback.isCustomDrawSupported then
    Exit;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  ctx.InitDraw(Round(canvasRect.size.width), Round(canvasRect.size.height));
  try
    state := [];
    if isRowSelected(row) then Include(state, cdsSelected);
    if NOT lclIsEnabled then Include(state, cdsDisabled);
    if isFocused(self,row) then
      Include(state, cdsFocused);
    if isChecked(self,row) then
      Include(state, cdsChecked);

    Result:= self.callback.customDraw(row, col, ctx, state);
  finally
    ctx.Free;
  end;
end;

procedure TCocoaTableListView.drawRect(dirtyRect: NSRect);
var
  done: Boolean;
begin
  if NOT Assigned(self.callback) then
    Exit;

  if CheckMainThread then
    self.callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);

  if NOT self.callback.isCustomDrawSupported then begin
    inherited;
    Exit;
  end;

  done:= self.lclCallCustomDraw( -1, -1, self.bounds );

  if done then begin
    // the Cocoa default drawing cannot be skipped in NSTableView,
    // we can only hide the SubviewViews to get the same effect.
    hideAllSubviews( self );
  end else begin
    drawNSViewBackground( self, self.lclGetCanvas.Brush );
    inherited;
  end;
end;

procedure TCocoaTableListView.setNeedsDisplayInRect( invalidRect: NSRect );
var
  rowRange: NSRange;
  rowView: NSView;
  row: Integer;
  startIndex: Integer;
  endIndex: Integer;
begin
  inherited;

  // NSTableView.initWithFrame() will call setNeedsDisplayInRect()
  // at this time, calling rowsInRect() will crash on macOS 10.14.
  // it seems that this issue does not exist on other macOS versions.
  // FYI: https://gitlab.com/freepascal.org/lazarus/lazarus/-/issues/41133
  if  _coocaInitializing then
    Exit;

  rowRange := self.rowsInRect( invalidRect );
  if rowRange.length = 0 then
    Exit;;

  startIndex:= rowRange.location;
  endIndex:= rowRange.location + rowRange.length - 1;
  for row:= startIndex to endIndex do begin
    rowView := self.rowViewAtRow_makeIfNecessary( row, False );
    if Assigned(rowView) then
      rowView.setNeedsDisplay_( True );
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
  selection: NSIndexSet;
begin
  if NOT Assigned(self.callback) then
    Exit;

  self.dontSendOnChangeMessage:= True;
  selection:= self.callback.selectionIndexSet;
  self.selectRowIndexesByProgram( selection );
  self.dontSendOnChangeMessage:= False;
end;

procedure TCocoaTableListView.reloadData;
begin
  if NOT Assigned(_processor) then
    Exit;
  if _processor.isInitializing(self) then
    Exit;

  inherited reloadData;
  _processor.onReloadData( self );

  // 1. the first step is here
  //    synchronously restore the selection immediately after calling reloadData()
  // 2. the second step is elsewhere
  //    the selection will be restored asynchronously after reloadData actually takes effect.
  self.restoreFromStableSelection;
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
    self.selectRowIndexes_byExtendingSelection( selection, False );
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
  _coocaInitializing:= True;
  Result:=inherited initWithFrame(frameRect);
  _coocaInitializing:= False;
  if NSAppkitVersionNumber >= NSAppKitVersionNumber11_0 then
    setStyle( CocoaConfigListView.vsReport.tableViewStyle );
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
  if Assigned(self.callback) then
    Result := self.callback.ItemsCount
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
  Result:= self.callback.shouldSelectionChange( row );
end;

procedure TCocoaTableListView.tableView_didClickTableColumn(
  tableView: NSTableView; tableColumn: NSTableColumn);
begin
  if Assigned(self.callback) then
    self.callback.ColumnClicked(getIndexOfColumn(tableColumn));
end;

function TCocoaTableListView.tableView_heightOfRow(tableView: NSTableView;
  row: NSInteger): CGFloat;
var
  h: Integer;
begin
  h:= self.CustomRowHeight;
  if h = 0 then
    h:= CocoaConfigListView.vsReport.row.defaultHeight;

  if isDynamicRowHeight and Assigned(self.callback) then begin
    self.callback.GetRowHeight( row, h );
    if h<=0 then h:=1; // must be positive (non-zero)
  end;

  Result:= h;
end;

function TCocoaTableListView.tableView_sizeToFitWidthOfColumn(
  tableView: NSTableView; column: NSInteger): CGFloat;
var
  totalCount: Integer;
  startIndex: Integer;
  endIndex: Integer;

  row: Integer;
  item: TCocoaTableListItem;
  tableColumn: NSTableColumn;
  currentWidth: CGFloat;
begin
  Result:= CocoaConfigListView.vsReport.columnAutoFit.minWidth;
  tableColumn:= NSTableColumn( self.tableColumns.objectAtIndex(column) );
  tableColumn.sizeToFit;
  currentWidth:= tableColumn.width +
                 CocoaConfigListView.vsReport.columnAutoFit.headerAdditionalWidth;
  if currentWidth > Result then
    Result:= currentWidth;

  totalCount:= self.numberOfRows;
  if totalCount = 0 then
    Exit;

  if totalCount <= CocoaConfigListView.vsReport.columnAutoFit.maxCalcRows then begin
    startIndex:= 0;
    endIndex:= totalCount - 1;
  end else begin
    startIndex:= self.rowsInRect(self.visibleRect).location;
    endIndex:= startIndex + CocoaConfigListView.vsReport.columnAutoFit.maxCalcRows div 2;
    if endIndex > totalCount - 1 then
      endIndex:= totalCount - 1;
    startIndex:= endIndex - CocoaConfigListView.vsReport.columnAutoFit.maxCalcRows + 1;
    if startIndex < 0 then
      startIndex:= 0;
    endIndex:= startIndex + CocoaConfigListView.vsReport.columnAutoFit.maxCalcRows - 1;
  end;

  for row:=startIndex to endIndex do begin
    item:= TCocoaTableListItem( self.viewAtColumn_row_makeIfNecessary( column, row , True ) );
    if Assigned(item) then begin
      currentWidth:= item.fittingSize.width;
      if currentWidth > Result then
        Result:= currentWidth;
    end;
  end;
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
var
  selectionIndexSet: NSMutableIndexSet;
begin
  if NOT Assigned(self.callback) then
    Exit;

  if Assigned(_processor) then begin
    if NOT _processor.isInitializing(self) then
      _processor.onSelectionChanged( self );
  end;
end;

procedure TCocoaTableListView.tableViewColumnDidResize(
  notification: NSNotification);
begin
  self.reloadData;
end;

{ TCocoaTableListItem }

function TCocoaTableListItem.checkBox: NSButton;
begin
  Result:= _checkBox;
end;

function TCocoaTableListItem.fittingSize: NSSize;
var
  width: CGFloat;
begin
  width:= self.textField.fittingSize.width;
  if Assigned(_checkBox) then
    width:= width + _checkBox.frame.size.width +
            CocoaConfigListView.vsReport.column.controlSpacing;
  if Assigned(self.imageView) then
    width:= width + self.imageView.frame.size.width +
            CocoaConfigListView.vsReport.column.controlSpacing;
  Result.width:= width;
  Result.height:= self.frame.size.height;
end;

procedure TCocoaTableListItem.drawRect(dirtyRect: NSRect);
var
  row: Integer;
  col: Integer;
  done: Boolean;
  cocoaTLV: TCocoaTableListView;
begin
  cocoaTLV:= TCocoaTableListView( _tableView );

  if NOT Assigned(cocoaTLV.callback) then
    Exit;

  if NOT cocoaTLV.callback.isCustomDrawSupported then begin
    inherited;
    Exit;
  end;

  row:= _tableView.rowForView( self );
  col:= _tableView.columnForView( self );
  done:= cocoaTLV.lclCallCustomDraw( row, col, self.bounds );

  if done then begin
    // the Cocoa default drawing cannot be skipped in NSTableView,
    // we can only hide the CellViews to get the same effect.
    // in the Lazarus IDE, there is a ListView with OnCustomDrawItem
    // in Perferences-Component Palette.
    hideAllSubviews( self );
  end else begin
    if updateNSTextFieldWithTFont(self.textField, cocoaTLV.lclGetCanvas.Font) then
      updateItemLayout( row, col );
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
  fieldControl.setAllowsExpansionToolTips(True);
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
  _checkBox.setAllowsMixedState( TCocoaTableListView(_tableView).lclCheckBoxAllowsMixed );
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
    _checkBox.sizeToFit;
    aFrame.size:= _checkBox.frame.size;
    aFrame.origin.y:= round( (rowHeight - aFrame.size.height ) / 2 );
    _checkBox.setFrameOrigin( aFrame.origin );

    aFrame.origin.x:= CocoaConfigListView.vsReport.column.controlSpacing;
  end;

  if Assigned(self.imageView) then begin
    aFrame.origin.x:= aFrame.origin.x + aFrame.size.width;
    aFrame.origin.y:= round( (rowHeight - tv.iconSize.Height) / 2 );
    aFrame.size:= tv.iconSize;
    self.imageView.setFrame( aFrame );

    aFrame.origin.x:= aFrame.origin.x + CocoaConfigListView.vsReport.column.controlSpacing;
  end;

  if Assigned(self.textField) then begin
    self.textField.sizeToFit;
    aFrame.origin.x:= aFrame.origin.x + aFrame.size.width;
    aFrame.origin.y:= round( (rowHeight - self.textField.frame.size.height) / 2 );
    aFrame.size.width:= _column.width - aFrame.origin.x;
    aFrame.size.height:= self.textField.frame.size.height;
    if aFrame.size.width < CocoaConfigListView.vsReport.column.textFieldMinWidth then
      aFrame.size.width:= CocoaConfigListView.vsReport.column.textFieldMinWidth;
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

  col:= self.getIndexOfColumn( tableColumn );
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

procedure TCocoaTableListView.lclInsertItem(const AIndex: Integer);
begin
  if NOT Assigned(self.callback) then
    Exit;

  if _processor.isInitializing(self) then
    Exit;

  self.callback.checkedIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  self.callback.mixedCheckedIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  self.callback.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  self.reloadData;
  self.sizeToFit();
end;

procedure TCocoaTableListView.lclDeleteItem(const AIndex: Integer);
begin
  if NOT Assigned(self.callback) then
    Exit;

  self.callback.checkedIndexSet.shiftIndexesStartingAtIndex_by( AIndex+1, -1);
  self.callback.mixedCheckedIndexSet.shiftIndexesStartingAtIndex_by( AIndex+1, -1);
  self.callback.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex+1, -1 );
  self.reloadData;
end;

procedure ExchangeIndexSetItem( indexSet: NSMutableIndexSet;
  const AIndex1: Integer; const AIndex2: Integer );
var
  hasIndex1: Boolean;
  hasIndex2: Boolean;
begin
  hasIndex1:= indexSet.containsIndex(AIndex1);
  hasIndex2:= indexSet.containsIndex(AIndex2);
  if hasIndex1 = hasIndex2 then
    Exit;

  if hasIndex1 then begin
    indexSet.removeIndex( AIndex1 );
    indexSet.addIndex( AIndex2 );
  end;
  if hasIndex2 then begin
    indexSet.removeIndex( AIndex2 );
    indexSet.addIndex( AIndex1 );
  end;
end;

procedure TCocoaTableListView.lclExchangeItem(const AIndex1: Integer;
  const AIndex2: Integer);
begin
  if NOT Assigned(self.callback) then
    Exit;

  ExchangeIndexSetItem( self.callback.checkedIndexSet, AIndex1, AIndex2 );
  ExchangeIndexSetItem( self.callback.mixedCheckedIndexSet, AIndex1, AIndex2 );
  ExchangeIndexSetItem( self.callback.selectionIndexSet, AIndex1, AIndex2 );
  self.reloadData;
end;

procedure TCocoaTableListView.lclClearItem;
begin
  self.callback.checkedIndexSet.removeAllIndexes;
  self.callback.mixedCheckedIndexSet.removeAllIndexes;
  self.callback.selectionIndexSet.removeAllIndexes;
  self.reloadData;
end;

procedure TCocoaTableListView.checkboxAction(sender: NSButton);
var
  row: NSInteger;
begin
  if not Assigned(self.callback) then Exit;

  row := rowForView(sender.superview);
  self.callback.SetItemCheckedAt(row, sender.state);
  if sender.state <> NSOffState then begin
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
  _tableView.lclDeleteItem( AIndex );
end;

function TCocoaWSListView_TableViewHandler.ItemDisplayRect(const AIndex,
  ASubItem: Integer; ACode: TDisplayCode): TRect;
var
  item: TCocoaTableListItem;
  frame: NSRect;
begin
  Result:= Bounds(0,0,0,0);
  item:= _tableView.viewAtColumn_row_makeIfNecessary( ASubItem, AIndex, True );
  if NOT Assigned(item) then
    Exit;

  frame:= item.frame;
  case ACode of
    drLabel:
      begin
        _listView.setCaptionFont( item.textField.font );
        _listView.setCaptionAlignment( NSTextAlignmentLeft );
        // to do: completely restore TFont
        _listView.getLCLControlCanvas.Font.Height:= Round(item.textField.font.pointSize);
        frame:= item.textField.frame;
        frame:= item.convertRect_toView( frame, _tableView );
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

procedure TCocoaWSListView_TableViewHandler.ItemExchange(
  const ALV: TCustomListView; AItem: TListItem; const AIndex1, AIndex2: Integer);
begin
  _tableView.lclExchangeItem( AIndex1, AIndex2 );
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
var
  lclcb : TLCLListViewCallback;
begin
  Result:= false;
  lclcb:= getCallback;
  if NOT Assigned(lclcb) then
    Exit;

  case AState of
    lisSelected: begin
      Result:= (AIndex>=0) and (AIndex < _tableView.numberOfRows);
      AIsSet:= lclcb.getItemStableSelection( AIndex );
    end;
  end;
end;

procedure TCocoaWSListView_TableViewHandler.ItemInsert(
  const AIndex: Integer; const AItem: TListItem);
begin
  _tableView.lclInsertItem( AIndex );
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
  _tableView.CustomRowHeight:= AValue.Height +
     CocoaConfigListView.vsReport.row.imageLineSpacing;

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

function TCocoaTableListViewProcessor.isInitializing( tv: NSTableView ): Boolean;
var
  cocoaTLV: TCocoaTableListView Absolute tv;
begin
  Result:= False;
  if NOT Assigned(cocoaTLV.callback) then
    Exit;

  Result:= TCocoaListView( self.getCallback(tv).Owner ).initializing;
end;

function TCocoaTableListViewProcessor.getLCLControlCanvas(tv: NSTableView
  ): TCanvas;
begin
  Result:= TCustomListView(tv.lclGetTarget).Canvas;
end;

procedure TCocoaTableListViewProcessor.onSelectionChanged(tv: NSTableView);
var
  NewSel: Integer;
  rm : NSIndexSet;
  ad : NSIndexSet;
  selectionIndexSet: NSMutableIndexSet;

  lclListView: TCustomListView;
  cocoaTLV: TCocoaTableListView Absolute tv;
  lclcb: TLCLListControlCallback;
begin
  lclcb:= self.getCallback( tv );
  lclListView:= TCustomListView( lclcb.Target );

  selectionIndexSet:= lclcb.selectionIndexSet;
  CompareIndexSets(selectionIndexSet, cocoaTLV.selectedRowIndexes, rm, ad);

  if NOT cocoaTLV.selectingByProgram then begin
    selectionIndexSet.removeAllIndexes;
    selectionIndexSet.addIndexes( tv.selectedRowIndexes );
  end;

  if cocoaTLV.dontSendOnChangeMessage then
    Exit;

  NewSel := cocoaTLV.selectedRow();
  sendSelectionChangedMsgToLCL( lclListView, NewSel, ad, rm );
end;

end.

