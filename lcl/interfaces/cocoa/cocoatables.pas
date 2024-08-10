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
  CocoaListView,
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
    _checkBox: NSButton;
  private
    column: NSTableColumn;
    checkedSubView: NSButton;
    imageSubView: NSImageView;
    textSubView: NSTextField;
    idStr: NSString;
  private
    procedure createTextField; message 'createTextField';
    procedure createImageView; message 'createImageView';
    procedure createCheckBox; message 'createCheckBox';
  public
    procedure setTableView( tableView: NSTableView ); message 'setTableView:';
    function checkBox: NSButton; message 'checkBox';

    procedure loadView( row: Integer; col: Integer );
      message 'loadView:col:';
    procedure updateItemValue( row: NSInteger; col: NSInteger );
      message 'updateItemValue:col:';
    procedure updateItemLayout( row: NSInteger; col: NSInteger );
      message 'updateItemLayout:col:';
  public
    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc; override;
    procedure setColumn(AColumn: NSTableColumn); message 'setColumn:';
    procedure setImage(AImage: NSImage); message 'setImage:';
    procedure setCheckState(AState: NSInteger); message 'setCheckState:';
    procedure setStringValue(AString: NSString); message 'setStringValue:';
    procedure setEditable(flag: Boolean); message 'setEditable:';
    procedure setFont(AFont: NSFont); message 'setFont:';
    procedure setTarget(ATarget: id); message 'setTarget:';
    procedure setCheckAction(aSelector: SEL); message 'setCheckAction:';
    procedure setTextAction(aSelector: SEL); message 'setTextAction:';
    procedure resizeSubviewsWithOldSize(oldSize: NSSize); override;
    procedure setIdentifier(identifier_: NSString); message 'setIdentifier:'; override;
    function identifier: NSString; message 'identifier'; override;
    function textFrame: NSRect; message 'textFrame';
    procedure lclSetEnabled(AEnabled: Boolean); override;
  end;

  TCocoaTalbeListView_onSelectionChanged = procedure( tv: NSTableView );

  { TCocoaTableListView }

  TCocoaTableListView = objcclass(
    NSTableView,
    NSTableViewDelegateProtocol,
    NSTableViewDataSourceProtocol,
    TCocoaListViewBackendControlProtocol )
  public
    iconSize: NSSize;

    callback: IListViewCallback;

    onSelectionChanged: TCocoaTalbeListView_onSelectionChanged;
    selectingByProgram: Boolean;

    readOnly: Boolean;

    isImagesInCell: Boolean;
    isFirstColumnCheckboxes: Boolean;
    isOwnerDraw : Boolean;
    isDynamicRowHeight: Boolean;
    CustomRowHeight: Integer;
    ScrollWidth: Integer;

    smallimages : NSMutableDictionary;
  public
    procedure backend_setCallback( cb:TLCLListViewCallback );
    procedure backend_reloadData;
    procedure backend_onInit;

    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;

    // Own methods, mostly convenience methods
    function getIndexOfColumn(ACol: NSTableColumn): Integer; message 'getIndexOfColumn:';
    procedure reloadDataForRow_column(ARow, ACol: NSInteger); message 'reloadDataForRow:column:';
    procedure selectOneItemByIndex( index: Integer; isSelected: Boolean );
      message 'selectOneItemByIndex:isSelected:';
    procedure selectRowIndexesByProgram( indexes: NSIndexSet );
      message 'selectRowIndexesByProgram:';

    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc; override;
    function fittingSize: NSSize; override;

    procedure drawRow_clipRect(row: NSInteger; clipRect: NSRect); override;
    procedure drawRect(dirtyRect: NSRect); override;

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
    procedure lclSetFirstColumCheckboxes(acheckboxes: Boolean); message 'lclSetFirstColumCheckboxes:';
    procedure lclSetImagesInCell(aimagesInCell: Boolean); message 'lclSetImagesInCell:';

    procedure lclRegisterSmallImage(idx: Integer; img: NSImage); message 'lclRegisterSmallImage::';
    function lclGetSmallImage(idx: INteger): NSImage; message 'lclGetSmallImage:';
    function lclGetItemImageAt(ARow, ACol: Integer): NSImage; message 'lclGetItemImageAt::';

    // BoundsRect - is the rectangle of the cell, speciifed of aRow, acol.
    // so the function lclGetLabelRect, lclGetIconRect should only adjust "BoundsRect"
    // and return the adjusted rectangle
    function lclGetLabelRect(ARow, ACol: Integer; const BoundsRect: TRect): TRect; message 'lclGetLabelRect:::';
    function lclGetIconRect(ARow, ACol: Integer; const BoundsRect: TRect): TRect; message 'lclGetIconRect:::';

    procedure lclInsDelRow(Arow: Integer; inserted: Boolean); message 'lclInsDelRow::';
    procedure lclSetColumnAlign(acolumn: NSTableColumn; aalignment: NSTextAlignment); message 'lclSetColumn:Align:';

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


  // View based NSTableView

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

function AllocCocoaTableListView: TCocoaTableListView;
procedure TListView_onSelectionChanged( tv: NSTableView );

function LCLCoordToRow(tbl: NSTableView; X,Y: Integer): Integer;
function LCLGetItemRect(tbl: NSTableView; row, col: Integer; var r: TRect): Boolean;
function LCLGetTopRow(tbl: NSTableView): Integer;

const
  DefaultRowHeight = 16; // per "rowHeight" property docs

implementation

type
  { TCellCocoaTableListView }

  TCellCocoaTableListView = objcclass(
    TCocoaTableListView,
    NSTableViewDelegateProtocol,
    NSTableViewDataSourceProtocol )
  public
    procedure lclInsDelRow(Arow: Integer; inserted: Boolean); override;
    procedure lclSetColumnAlign(acolumn: NSTableColumn; aalignment: NSTextAlignment); override;
  end;

  { TViewCocoaTableListView }

  TViewCocoaTableListView = objcclass(TCocoaTableListView, NSTableViewDelegateProtocol, NSTableViewDataSourceProtocol)
    function tableView_viewForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView; message 'tableView:viewForTableColumn:row:';

    procedure textFieldAction(sender: NSTextField); message 'textFieldAction:';
    procedure checkboxAction(sender: NSButton); message 'checkboxAction:';

    function lclGetLabelRect(ARow, ACol: Integer; const BoundsRect: TRect): TRect; override;
    procedure lclInsDelRow(Arow: Integer; inserted: Boolean); override;
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

function AllocCocoaTableListView: TCocoaTableListView; // init will happen outside
begin
  Result := TViewCocoaTableListView.alloc;
  Result.setRowSizeStyle( NSTableViewRowSizeStyleCustom );
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

procedure TCocoaTableListView.lclSetFirstColumCheckboxes(acheckboxes: Boolean);
begin
  if isFirstColumnCheckboxes = acheckboxes then Exit;
  isFirstColumnCheckboxes := acheckboxes;
  reloadData();
end;

procedure TCocoaTableListView.lclSetImagesInCell(aimagesInCell: Boolean);
begin
  if isImagesInCell = aimagesInCell then Exit;
  isImagesInCell := aimagesInCell;
  reloadData();
end;

procedure TCocoaTableListView.lclRegisterSmallImage(idx: Integer; img: NSImage);
begin
  if not Assigned(smallimages) then
    smallimages := (NSMutableDictionary.alloc).init;

  if Assigned(img) then
    smallimages.setObject_forKey(img, NSNumber.numberWithInt(idx) )
  else
    smallimages.removeObjectForKey( NSNumber.numberWithInt(idx) );
end;

function TCocoaTableListView.lclGetSmallImage(idx: INteger): NSImage;
begin
  if not Assigned(smallimages) then Result := nil;
  Result := NSImage(smallimages.objectForKey( NSNumber.numberWithInt(idx) ) );
end;

function TCocoaTableListView.lclGetItemImageAt(ARow, ACol: Integer): NSImage;
var
  idx : Integer;
  img : NSimage;
begin
  if not Assigned(callback) then
  begin
    Result := nil;
    Exit;
  end;

  idx := -1;
  callback.GetItemImageAt(ARow, ACol, idx);
  if idx>=0 then
  begin
    img := lclGetSmallImage(idx);
    if not Assigned(img) then begin
      img := callback.GetImageFromIndex(idx);
      if Assigned(img) then lclRegisterSmallImage(idx, img);
    end;
  end else
    img := nil;
  Result := img;
end;

function TCocoaTableListView.lclGetLabelRect(ARow, ACol: Integer;
  const BoundsRect: TRect): TRect;
begin
  Result:= BoundsRect;
  Result.Top:= Result.Top - 2;
  Result.Height:= Result.Height + 4;
  if self.isImagesInCell then begin
    Result.Left:= Result.Left + BoundsRect.Height;
  end;
end;

function TCocoaTableListView.lclGetIconRect(ARow, ACol: Integer;
  const BoundsRect: TRect): TRect;
begin
  if self.isImagesInCell then begin
    Result:= BoundsRect;
    Result.Width:= Result.Height;
  end else begin
    Result:= TRect.Empty;
  end;
end;

procedure TCocoaTableListView.lclInsDelRow(Arow: Integer; inserted: Boolean);
begin
  // a row has been inserted or removed
  // the following rows needs to be invalidated
  // as well as number of total items in the table should be marked as modified
end;

procedure TCocoaTableListView.lclSetColumnAlign(acolumn: NSTableColumn;
  aalignment: NSTextAlignment);
begin

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

procedure TCocoaTableListView.dealloc;
begin
  //if Assigned(Items) then FreeAndNil(Items);
  if Assigned(smallimages) then smallimages.release; // all contents is released automatically
  inherited dealloc;
end;

function TCocoaTableListView.fittingSize: NSSize;
begin
  Result:= NSZeroSize;
end;

procedure TCocoaTableListView.drawRow_clipRect(row: NSInteger; clipRect: NSRect
  );
var
  ctx: TCocoaContext;
  ItemState: TOwnerDrawState;
begin
  inherited;
  if not Assigned(callback) then Exit;
  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  ctx.InitDraw(Round(bounds.size.width), Round(bounds.size.height));
  try
    ItemState := [];
    if isRowSelected(row) then Include(ItemState, odSelected);
    if lclIsEnabled then Include(ItemState, odDisabled);
    if Assigned(window) and (window.firstResponder = self) and (odSelected in ItemState) then
      Include(ItemState, odFocused);

    callback.DrawRow(row, ctx, NSRectToRect(rectOfRow(row)), ItemState);
  finally
    ctx.Free;
  end;
end;

procedure TCocoaTableListView.drawRect(dirtyRect: NSRect);
begin
  inherited drawRect(dirtyRect);
  if CheckMainThread and Assigned(callback) then
    callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
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
  lclcb: TLCLListViewCallback;
  selection: NSMutableIndexSet;
begin
  if (index < 0) or (index >= self.numberOfRows) then
    Exit;

  lclcb:= TLCLListViewCallback( self.callback.GetCallbackObject );
  selection:= NSMutableIndexSet.alloc.initWithIndexSet( self.selectedRowIndexes );
  if isSelected then begin
    if NOT self.allowsMultipleSelection then
      selection.removeAllIndexes;
    selection.addIndex( index );
  end else begin
    selection.removeIndex( index );
  end;

  if NOT selection.isEqualToIndexSet(self.selectedRowIndexes) then begin
    lclcb.selectionIndexSet.removeAllIndexes;
    lclcb.selectionIndexSet.addIndexes( selection );
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

procedure TListView_onSelectionChanged( tv: NSTableView );
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

  lclcb:= TLCLListViewCallback( cocoaTLV.callback.GetCallbackObject );
  lclListView:= TCustomListView( lclcb.Target );

  if TCocoaListView(lclcb.Owner).initializing then
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

procedure TCocoaTableListView.tableViewSelectionDidChange(notification: NSNotification);
begin
  if Assigned(onSelectionChanged) then
    onSelectionChanged( self );
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

{ TCellCocoaTableListView }

procedure TCellCocoaTableListView.lclInsDelRow(Arow: Integer; inserted: Boolean);
begin
  noteNumberOfRowsChanged;
end;

procedure TCellCocoaTableListView.lclSetColumnAlign(acolumn: NSTableColumn;
  aalignment: NSTextAlignment);
begin
  if not Assigned(acolumn) then Exit;
  NSCell(acolumn.headerCell).setAlignment( aalignment );
  NSCell(acolumn.dataCell).setAlignment( aalignment );
end;

{ TCocoaTableListItem }

function TCocoaTableListItem.checkBox: NSButton;
begin
  Result:= _checkBox;
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
  imageControl.cell.setImageScaling( NSImageScaleProportionallyUpOrDown );
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

procedure TCocoaTableListItem.setTableView(tableView: NSTableView);
begin
  _tableView:= tableView;
end;

procedure TCocoaTableListItem.loadView( row: Integer; col: Integer );
var
  tv: TCocoaTableListView;
  lclcb: TLCLListViewCallback;
  lclImageIndex: Integer;
  lvil: TListViewImageList;
begin
  tv:= TCocoaTableListView( _tableView );
  lclcb:= TLCLListViewCallback( tv.callback.GetCallbackObject );

  self.createTextField;

  if col=0 then begin
    if lclcb.GetImageListType(lvil) then
      self.createImageView;
  end else begin
    lclcb.GetItemImageAt( row, col, lclImageIndex );
    if lclImageIndex >= 0 then
      self.createImageView;
  end;

  if (col=0) and tv.isFirstColumnCheckboxes then
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
    lclcb.GetItemCheckedAt( row, 0, checkedValue );
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
  lclcb: TLCLListViewCallback;
  aFrame: NSRect;
  rowHeight: CGFloat;
begin
  tv:= TCocoaTableListView( _tableView );
  lclcb:= TLCLListViewCallback( tv.callback.GetCallbackObject );

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
  end;

  if Assigned(self.textField) then begin
    aFrame.size.height:= self.textField.frame.size.height;
    aFrame.origin.x:= aFrame.origin.x + aFrame.size.width + 4;
    aFrame.origin.y:= (rowHeight - 15) / 2;
    aFrame.size.width:= self.column.width - aFrame.origin.x;
    if aFrame.size.width < 16 then
      aFrame.size.width:= 16;
    aFrame.size.height:= 15;
    self.textField.setFrame( aFrame );
  end;
end;

function TCocoaTableListItem.initWithFrame(frameRect: NSRect): id;
var
  ImageIndex: NSInteger;
  //bmp: TBitmap;
  Img: NSImage;
begin
  Result := inherited initWithFrame(frameRect);

  /////
  EXIT;

  {$ifdef BOOLFIX}
  Result.setAutoresizesSubviews_(Ord(True));
  {$else}
  Result.setAutoresizesSubviews(True);
  {$endif}

  checkedSubview := NSButton.alloc.init;
  checkedSubview.setButtonType(NSSwitchButton);
  checkedSubview.setFrameOrigin(GetNSPoint(0,0));
  checkedSubview.setFrameSize(GetNSSize(frameRect.size.height, frameRect.size.height));
  Result.addSubview(checkedSubView);

  imageSubView := NSImageView.alloc.initWithFrame(checkedSubView.frame);
  Result.addSubview(imageSubView);

  textSubView := NSTextField.alloc.initWithFrame(frameRect);
  textSubView.setBordered(False);
  textSubView.setDrawsBackground(False);
  textSubView.cell.setSendsActionOnEndEditing(True);
  textSubView.cell.setLineBreakMode(NSLineBreakByTruncatingTail);
  textSubView.setEditable(False);
  textSubView.setAllowsEditingTextAttributes(False);
  Result.addSubview(textSubView);
end;

procedure TCocoaTableListItem.dealloc;
begin
  checkedSubView.release;
  imageSubView.release;
  textSubView.release;
  inherited dealloc;
end;

procedure TCocoaTableListItem.setColumn(AColumn: NSTableColumn);
begin
  column := AColumn;
  resizeSubviewsWithOldSize(GetNSSize(column.width, column.tableView.rowHeight));
end;

procedure TCocoaTableListItem.setImage(AImage: NSImage);
begin
  imageSubView.setImage(AImage);
  {$ifdef BOOLFIX}
  imageSubView.setHidden_(Ord(AImage = nil));
  {$else}
  imageSubView.setHidden(AImage = nil);
  {$endif}
  resizeSubviewsWithOldSize(GetNSSize(column.width, column.tableView.rowHeight));
end;

procedure TCocoaTableListItem.setCheckState(AState: NSInteger);
begin
  checkedSubView.setState(AState);
  {$ifdef BOOLFIX}
  checkedSubView.setHidden_(Ord(AState = -1));
  {$else}
  checkedSubView.setHidden(AState = -1);
  {$endif}
  resizeSubviewsWithOldSize(GetNSSize(column.width, column.tableView.rowHeight));
end;

procedure TCocoaTableListItem.setStringValue(AString: NSString);
begin
  if Assigned(textSubView) and Assigned(AString) then
    textSubView.setStringValue(AString);
end;

procedure TCocoaTableListItem.setEditable(flag: Boolean);
begin
  textSubView.setEditable(flag);
end;

procedure TCocoaTableListItem.setFont(AFont: NSFont);
begin
  textSubView.setFont(AFont);
  resizeSubviewsWithOldSize(GetNSSize(column.width, column.tableView.rowHeight));
end;

procedure TCocoaTableListItem.setTarget(ATarget: id);
begin
  checkedSubView.setTarget(ATarget);
  textSubView.setTarget(ATarget);
end;

procedure TCocoaTableListItem.setCheckAction(aSelector: SEL);
begin
  checkedSubView.setAction(aSelector);
end;

procedure TCocoaTableListItem.setTextAction(aSelector: SEL);
begin
  textSubView.setAction(aSelector);
end;

procedure TCocoaTableListItem.resizeSubviewsWithOldSize(oldSize: NSSize);
var
  origin: NSPoint;
  size: NSSize;
  height: CGFloat;
begin
  origin := bounds.origin;
  size := oldSize;
  if not checkedSubView.isHidden then
  begin
    checkedSubView.setFrameOrigin(origin);
    origin.x := origin.x + checkedSubView.frame.size.width;
    size.width := size.width - checkedSubView.frame.size.width;
  end;

  if not imageSubView.isHidden then
  begin
    imageSubView.setFrameOrigin(origin);
    origin.x := origin.x + imageSubView.frame.size.width;
    size.width := size.width - imageSubView.frame.size.width;
  end;

  // Vertically center text
  height := textSubView.font.boundingRectForFont.size.height;
  origin.y := ((size.height - height + 0.5) / 2.0);
  size.height := height;
  textSubView.setFrameOrigin(origin);
  textSubView.setFrameSize(size);
end;

procedure TCocoaTableListItem.setIdentifier(identifier_: NSString);
begin
  idStr := identifier_;
end;

function TCocoaTableListItem.identifier: NSString;
begin
  Result := idStr;
end;

function TCocoaTableListItem.textFrame: NSRect;
begin
  Result := textSubView.frame;
end;

procedure TCocoaTableListItem.lclSetEnabled(AEnabled: Boolean);
begin
  // If NSTextField editable is set False, text color won't change when disabled
  if AEnabled then
    textSubView.setTextColor(NSColor.controlTextColor)
  else
    textSubView.setTextColor(NSColor.disabledControlTextColor);
  inherited lclSetEnabled(AEnabled);
end;

{ TViewCocoaTableListView }

function TViewCocoaTableListView.tableView_viewForTableColumn_row(tableView: NSTableView;
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
  if row>=numberOfRowsInTableView(self) then
    Exit;

  frameRect.origin := GetNSPoint(0,0);
  frameRect.size := GetNSSize(tableColumn.width, rowHeight);

  item := TCocoaTableListItem(makeViewWithIdentifier_owner(NSSTR('tblview'), self));
  if item = nil then begin
    item := TCocoaTableListItem.alloc.initWithFrame(frameRect);
    item.setidentifier(NSSTR('tblview'));
  end;

  col := tableColumns.indexOfObject(tableColumn);

  item.setTableView( self );
  item.setColumn( tableColumn );
  item.loadView( row, col );
  item.updateItemValue( row, col );
  item.updateItemLayout( row, col );

  Result := item;
end;

procedure TViewCocoaTableListView.textFieldAction(sender: NSTextField);
var
  column, row: NSInteger;
begin
  if not Assigned(callback) then Exit;

  column := columnForView(sender);
  row := rowForView(sender);
  callback.SetItemTextAt(row, column, NSStringToString(sender.stringValue));
  reloadDataForRow_column(row, column);
end;

procedure TViewCocoaTableListView.checkboxAction(sender: NSButton);
var
  row: NSInteger;
begin
  if not Assigned(callback) then Exit;

  row := rowForView(sender.superview);
  callback.SetItemCheckedAt(row, 0, sender.state);
  if sender.state = NSOnState then begin
    self.selectOneItemByIndex(row, True);
  end;
  reloadDataForRow_column(row, 0);
end;

function TViewCocoaTableListView.lclGetLabelRect(ARow, ACol: Integer;
  const BoundsRect: TRect): TRect;
var
  lTableItemLV: TCocoaTableListItem;
begin
  Result := BoundsRect;
  lTableItemLV := TCocoaTableListItem(viewAtColumn_row_makeIfNecessary(ACol, ARow, False));
  Result.Left := Round(lTableItemLV.textFrame.origin.x - 1);
  Result.Width := Round(lTableItemLV.textFrame.size.width);
end;

procedure TViewCocoaTableListView.lclInsDelRow(Arow: Integer; inserted: Boolean);
var
  rows: NSIndexSet;
begin
  rows := NSIndexSet.indexSetWithIndexesInRange(NSMakeRange(Arow,1));
  if inserted then
    insertRowsAtIndexes_withAnimation(rows, 0)
  else
    removeRowsAtIndexes_withAnimation(rows, 0);
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
  _tableView.lclInsDelRow(AIndex, false);
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
begin
  Application.QueueAsyncCall( @doReloadDataAfterDelete, AIndex );
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

  lclcb.checkedIndexSet.shiftIndexesStartingAtIndex_by(AIndex, 1);
  lclcb.selectionIndexSet.shiftIndexesStartingAtIndex_by( AIndex, 1 );
  _tableView.lclInsDelRow(AIndex, true);
  _tableView.selectRowIndexesByProgram( lclcb.selectionIndexSet );
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

  _tableView.lclSetImagesInCell(Assigned(AValue));

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

end.

