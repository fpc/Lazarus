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
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_LISTVIEW}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects,
  cocoa_extra, CocoaPrivate, CocoaThemes,
  // LCL
  LCLType;

type

  { IListViewCallBack }

  IListViewCallBack = interface(ICommonCallback)
    function ItemsCount: Integer;
    function GetItemTextAt(ARow, ACol: Integer; var Text: String): Boolean;
    function GetItemCheckedAt(ARow, ACol: Integer; var CheckState: Integer): Boolean;
    function GetItemImageAt(ARow, ACol: Integer; var imgIdx: Integer): Boolean;
    function GetImageFromIndex(imgIdx: Integer): NSImage;
    procedure SetItemTextAt(ARow, ACol: Integer; const Text: String);
    procedure SetItemCheckedAt(ARow, ACol: Integer; CheckState: Integer);
    procedure tableSelectionChange(ARow: Integer; Added, Removed: NSIndexSet);
    procedure ColumnClicked(ACol: Integer);
    procedure DrawRow(rowidx: Integer; ctx: TCocoaContext; const r: TRect; state: TOwnerDrawState);
    procedure GetRowHeight(rowidx: Integer; var height: Integer);
  end;

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

  { TCocoaTableListView }

  TCocoaTableListView = objcclass(NSTableView, NSTableViewDelegateProtocol, NSTableViewDataSourceProtocol)
  public
    callback: IListViewCallback;

    readOnly: Boolean;

    beforeSel : NSIndexSet;

    isImagesInCell: Boolean;
    isFirstColumnCheckboxes: Boolean;
    isOwnerDraw : Boolean;
    isDynamicRowHeight: Boolean;
    CustomRowHeight: Integer;

    smallimages : NSMutableDictionary;

    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;

    // Own methods, mostly convenience methods
    function getIndexOfColumn(ACol: NSTableColumn): Integer; message 'getIndexOfColumn:';
    procedure reloadDataForRow_column(ARow, ACol: NSInteger); message 'reloadDataForRow:column:';

    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc; override;
    procedure resetCursorRects; override;

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
    {function tableView_shouldSelectRow(tableView: NSTableView; row: NSInteger): Boolean; message 'tableView:shouldSelectRow:';
    function tableView_selectionIndexesForProposedSelection(tableView: NSTableView; proposedSelectionIndexes: NSIndexSet): NSIndexSet; message 'tableView:selectionIndexesForProposedSelection:';
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
    {procedure tableViewColumnDidMove(notification: NSNotification); message 'tableViewColumnDidMove:';
    procedure tableViewColumnDidResize(notification: NSNotification); message 'tableViewColumnDidResize:';
    procedure tableViewSelectionIsChanging(notification: NSNotification); message 'tableViewSelectionIsChanging:';}
  end;


  { NSImageAndTextCell }

  NSImageAndTextCell = objcclass(NSTextFieldCell)
    drawImage : NSImage;
    procedure drawWithFrame_inView(cellFrame: NSRect; controlView_: NSView); override;
  end;

  { NSTableButtonCell }

  // NSButtonCell would handle a click on a checkbox caption as a click on
  // the checkbox itself.  (You can experience that by clicking on TCheckBox)
  // This is the expected behavior for a standalone checkbox control.
  // However, for a checkbox is in a table, it's not desired to have checkbox
  // triggered, by clicking on its caption.
  // (You can try it in macOS "System Preferences"->"Keyboard"->"Shortcuts")
  // Since a checkbox and the text are put in the same NSTableView column
  // using NSButtonCell, there's an override for hitTesting function.
  // IF a user hits caption, then hitTest returns "none" suppressing the hit.
  // Thus a checkbox in a table can only be checked if clicked directly into
  // the checkbox.
  //
  // todo: add support for images. (For TListView with checkboxes and images)
  //       It's rarely used (if ever), yet it's possible

  NSTableButtonCell = objcclass (NSButtonCell)
    _type: NSButtonType;
    function hitTestForEvent_inRect_ofView(event: NSEvent; cellFrame: NSRect; controlView_: NSView): NSUInteger; override;
    procedure setButtonType(aType: NSButtonType); override;
  end;

  { TCellCocoaTableListView }

  TCellCocoaTableListView = objcclass(TCocoaTableListView, NSTableViewDelegateProtocol, NSTableViewDataSourceProtocol)
  public
    function tableView_objectValueForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id; message 'tableView:objectValueForTableColumn:row:';
    procedure tableView_setObjectValue_forTableColumn_row(tableView: NSTableView; object_: id; tableColumn: NSTableColumn; row: NSInteger); message 'tableView:setObjectValue:forTableColumn:row:';
    function tableView_dataCellForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSCell; message 'tableView:dataCellForTableColumn:row:';
    procedure lclInsDelRow(Arow: Integer; inserted: Boolean); override;
    procedure lclSetColumnAlign(acolumn: NSTableColumn; aalignment: NSTextAlignment); override;
  end;

  TCellCocoaTableListView1013 = objcclass(TCellCocoaTableListView, NSTableViewDelegateProtocol, NSTableViewDataSourceProtocol)
    // overriding the highlight color for dark theme
    procedure highlightSelectionInClipRect(clipRect: NSRect); override;
  end;

  // View based NSTableView

  TCocoaTableListItem = objcclass(NSView)
  private
    column: NSTableColumn;
    checkedSubView: NSButton;
    imageSubView: NSImageView;
    textSubView: NSTextField;
    idStr: NSString;
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
    procedure setIdentifier(identifier_: NSString); message 'setIdentifier:'; {$if FPC_FULLVERSION >= 30200}override;{$endif}
    function identifier: NSString; message 'identifier'; {$if FPC_FULLVERSION >= 30200}override;{$endif}
    function textFrame: NSRect; message 'textFrame';
    procedure lclSetEnabled(AEnabled: Boolean); override;
  end;


  { TViewCocoaTableListView }

  TViewCocoaTableListView = objcclass(TCocoaTableListView, NSTableViewDelegateProtocol, NSTableViewDataSourceProtocol)
    // todo: this should be "override" for 10.7 and later
    //       on the other hand, it doesn't call "inherited" so there's no need
    //       to do an actual override
    function tableView_viewForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSView; message 'tableView:viewForTableColumn:row:';

    procedure textFieldAction(sender: NSTextField); message 'textFieldAction:';
    procedure checkboxAction(sender: NSButton); message 'checkboxAction:';

    function lclGetLabelRect(ARow, ACol: Integer; const BoundsRect: TRect): TRect; override;
    procedure lclInsDelRow(Arow: Integer; inserted: Boolean); override;
  end;

function AllocCocoaTableListView: TCocoaTableListView;

function LCLCoordToRow(tbl: NSTableView; X,Y: Integer): Integer;
function LCLGetItemRect(tbl: NSTableView; row, col: Integer; var r: TRect): Boolean;
function LCLGetTopRow(tbl: NSTableView): Integer;

const
  DefaultRowHeight = 16; // per "rowHeight" property docs

implementation

function LCLCoordToRow(tbl: NSTableView; X,Y: Integer): Integer;
var
  pt : NSPoint;
begin
  if not Assigned(tbl) then
  begin
    Result := -1;
    Exit;
  end;

  pt.x := X;
  if tbl.isFlipped
    then pt.y := Y + tbl.visibleRect.origin.y
    else pt.y := tbl.frame.size.height - Y - tbl.visibleRect.origin.y;

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

// not yet!
{.$DEFINE DYNAMIC_NSTABLEVIEW_BASE}

function AllocCocoaTableListView: TCocoaTableListView; // init will happen outside
begin
  {$IFDEF DYNAMIC_NSTABLEVIEW_BASE}
  if NSAppKitVersionNumber >= NSAppKitVersionNumber10_7 then
    Result := TViewCocoaTableListView.alloc
  else
    Result := TCellCocoaTableListView.alloc;
  {$ELSE}

  // this is required for "dark theme" support on 10.13
  if (NSAppkitVersionNumber >= NSAppKitVersionNumber10_13)
    and (NSAppkitVersionNumber < NSAppKitVersionNumber10_14)
  then
    Result := TCellCocoaTableListView1013.alloc
  else
    Result := TCellCocoaTableListView.alloc;
  {$ENDIF}
end;

{ NSTableButtonCell }

function NSTableButtonCell.hitTestForEvent_inRect_ofView(event: NSEvent;
  cellFrame: NSRect; controlView_: NSView): NSUInteger;
var
  r  : NSRect;
  pt : NSPoint;
begin
  Result := inherited hitTestForEvent_inRect_ofView(event, cellFrame, controlView_);
  if _type = NSSwitchButton then
  begin
    pt := event.locationInWindow;
    if Assigned(controlView_) then
      pt := controlView_.convertPoint_fromView(pt, nil);
    r := titleRectForBounds(cellFrame);

    // todo: pt.y seems to be off by some amount
    if ((pt.x >= r.origin.x) and (pt.x<=r.origin.x + r.size.width)) then
      Result := NSCellHitNone;
  end;
end;

procedure NSTableButtonCell.setButtonType(aType: NSButtonType);
begin
  _type := aType;
  inherited setButtonType(aType);
end;

{ NSImageAndTextCell }

procedure NSImageAndTextCell.drawWithFrame_inView(cellFrame: NSRect;
  controlView_: NSView);
var
  r : NSRect;
  srcsz : NSSize;
begin
  r:=cellFrame;
  cellFrame.origin.x:=cellFrame.origin.x+cellFrame.size.height;
  cellFrame.size.width:=cellFrame.size.width-cellFrame.size.height;
  inherited drawWithFrame_inView(cellFrame, controlView_);
  if Assigned(drawImage) then
  begin
    r.size.width:=r.size.height;
    srcsz := drawImage.size;
    drawImage.drawInRect_fromRect_operation_fraction_respectFlipped_hints(
      r, NSMakeRect(0,0, srcsz.width, srcsz.height), NSCompositeSourceOver, 1, true, nil
    );
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
  Result := BoundsRect;
end;

function TCocoaTableListView.lclGetIconRect(ARow, ACol: Integer;
  const BoundsRect: TRect): TRect;
begin
  Result := BoundsRect;
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

procedure TCocoaTableListView.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
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

function TCocoaTableListView.initWithFrame(frameRect: NSRect): id;
begin
  Result:=inherited initWithFrame(frameRect);
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

function TCocoaTableListView.tableView_shouldEditTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): Boolean;
begin
  Result := not readOnly;
end;

function TCocoaTableListView.selectionShouldChangeInTableView(
  tableView: NSTableView): Boolean;
begin
  if Assigned(beforeSel) then beforeSel.release;
  beforeSel := (NSIndexSet.alloc).initWithIndexSet(selectedRowIndexes);
  Result := true;
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
  rm := NSMutableIndexSet.alloc.init;
  ad := NSMutableIndexSet.alloc.init;
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

procedure TCocoaTableListView.tableViewSelectionDidChange(notification: NSNotification);
var
  NewSel: Integer;
  Unsel : NSIndexSet;
  rm : NSIndexSet;
  ad : NSIndexSet;
begin
  if Assigned(callback) then
  begin
    CompareIndexSets(beforeSel, selectedRowIndexes, rm, ad);

    NewSel := Self.selectedRow();
    callback.tableSelectionChange(NewSel, ad, rm);

    beforeSel.release;
    beforeSel := nil;
  end;
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

procedure TCellCocoaTableListView.tableView_setObjectValue_forTableColumn_row(
  tableView: NSTableView; object_: id; tableColumn: NSTableColumn;
  row: NSInteger);
var
  lColumnIndex: NSInteger;
  lNewValue: NSString;
  isSel: Integer;
begin
  if (NSObject(object_).isKindOfClass(NSNumber)) and isFirstColumnCheckboxes then begin
    lColumnIndex := getIndexOfColumn(tableColumn);
    if Assigned(callback) and (lColumnIndex = 0) then
      callback.SetItemCheckedAt(row, lColumnIndex, NSNumber(object_).integerValue);

    Exit;
  end;

  //WriteLn('[TCocoaTableListView.tableView_setObjectValue_forTableColumn_row]');
  if not NSObject(object_).isKindOfClass(NSString) then Exit;
  lNewValue := NSString(object_);
  //WriteLn('[TCocoaTableListView.tableView_setObjectValue_forTableColumn_row] A');}
  if ReadOnly then Exit;

  lColumnIndex := getIndexOfColumn(tableColumn);
  if Assigned(callback) then
  begin
    callback.SetItemTextAt(row, lColumnIndex, lNewValue.UTF8String);
    reloadDataForRow_column(lColumnIndex, row);
  end;
end;

function TCellCocoaTableListView.tableView_dataCellForTableColumn_row(
  tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSCell;
var
  chk : Boolean;
  txt : string;
  col : Integer;
  nstxt : NSString;
  idx  : Integer;
  img  : NSImage;
  btn  : NSTableButtonCell;
  colorTitle : NSMutableAttributedString;
begin
  Result:=nil;
  if not isFirstColumnCheckboxes and not isImagesInCell then Exit;

  col := getIndexOfColumn(tableColumn);
  if (col <> 0) then Exit;

  if not isFirstColumnCheckboxes and isImagesInCell then begin
    img := lclGetItemImageAt(row, col);

    Result := NSImageAndTextCell(NSImageAndTextCell.alloc).initTextCell(NSSTR(''));
    NSImageAndTextCell(Result).drawImage := img; // if "image" is assigned, text won't be drawn :(
    Exit;
  end;
  txt := '';
  chk := false;

  callback.GetItemTextAt(row, col, txt);

  if txt = '' then nstxt := NSString.string_
  else nstxt := NSString.stringWithUTF8String(@txt[1]);

  btn := NSTableButtonCell.alloc.init.autorelease;
  //Result.setAllowsMixedState(True);
  btn.setButtonType(NSSwitchButton);
  btn.setTitle(nstxt);

  // forced "controlTextColor" provides a better result on unfocused
  // nstablelist view with checkboxes
  colorTitle := NSMutableAttributedString.alloc.initWithAttributedString(btn.attributedTitle);
  colorTitle.addAttribute_value_range(NSForegroundColorAttributeName
   , NSColor.controlTextColor
   , NSMakeRange(0, colorTitle.length));
  btn.setAttributedTitle(colorTitle);
  colorTitle.release;

  if chk then begin
    btn.setIntValue(1);
    btn.setCellAttribute_to(NSCellState, NSOnState);
  end;
  Result := btn;
end;

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

function TCellCocoaTableListView.tableView_objectValueForTableColumn_row(
  tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id;
var
  lStringList: TStringList;
  col: NSInteger;
  StrResult: NSString;
  chk : Integer;
  txt : string;
begin
  {$IFDEF COCOA_DEBUG_TABCONTROL}
  WriteLn(Format('[TCocoaTableListView.tableView_objectValueForTableColumn_row] col=%d row=%d Items.Count=%d',
    [col, row, Items.Count]));
  {$ENDIF}

  Result := nil;
  if not Assigned(callback) then Exit;
  col := getIndexOfColumn(tableColumn);
  if (col = 0) and isFirstColumnCheckboxes then begin
    chk := 0;
    callback.GetItemCheckedAt(row, col, chk);
    Result := NSNumber.numberWithInt(chk);
    Exit;
  end;

  txt := '';
  if callback.GetItemTextAt(row, col, txt) then begin
    if txt = '' then Result := NSString.string_
    else Result := NSString.stringWithUTF8String(@txt[1])
  end;
  (*
  if row > Items.Count-1 then begin
    Result := nil;
    Exit;
  end;
  if col = 0 then
    StrResult := NSStringUTF8(Items.Strings[row])
  else
  begin
    lStringList := TStringList(Items.Objects[row]);
    StrResult := NSStringUTF8(lStringList.Strings[col-1]);
  end;
  Result := StrResult;
  *)
end;

{ TCocoaTableListItem }

function TCocoaTableListItem.initWithFrame(frameRect: NSRect): id;
var
  ImageIndex: NSInteger;
  //bmp: TBitmap;
  Img: NSImage;
begin
  Result := inherited initWithFrame(frameRect);
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
  frameRect.origin := GetNSPoint(0,0);
  frameRect.size := GetNSSize(tableColumn.width, rowHeight);

  item := TCocoaTableListItem(makeViewWithIdentifier_owner(NSSTR('tblview'), self));
  if item = nil then begin
    item := TCocoaTableListItem.alloc.initWithFrame(frameRect);
    //todo: should be system font :?
    //item.setfont(TCocoaFont(ListView.Font.Handle).Font);
    item.setTarget(self);
    item.setCheckAction(ObjCSelector('checkboxAction:'));
    item.setTextAction(ObjCSelector('textFieldAction:'));
    item.setidentifier(NSSTR('tblview'));
  end;

  item.setFrame(frameRect);
  item.setColumn(tableColumn);
  col := tableColumns.indexOfObject(tableColumn);
  if (col = 0) and isFirstColumnCheckboxes then begin
    callback.GetItemCheckedAt(row, col, chkst);
    item.setCheckState(chkst)
  end
  else
    item.setCheckState(-1);

  item.setImage(lclGetItemImageAt(row, col));

  txt := '';
  if not callback.GetItemTextAt(row, col, txt) then txt := '';

  item.setStringValue(NSStringUtf8(txt));
  item.lclSetEnabled(isEnabled);
  Result := item
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

  row := rowForView(sender);
  callback.SetItemCheckedAt(row, 0, sender.state);
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

{ TCellCocoaTableListView1013 }

procedure TCellCocoaTableListView1013.highlightSelectionInClipRect(clipRect: NSRect
  );
var
  r   : NSInteger;
  rows : NSRange;
  cnt  : integer;
  focused: Boolean;
  hicolor: NSColor;
begin
  if not IsPaintDark then begin
    inherited highlightSelectionInClipRect(clipRect);
    Exit;
  end;


  focused := Assigned(window) and (window.firstResponder = Self);
  if focused then
    hicolor := NSColor.alternateSelectedControlColor
  else
    // the default color is NSColor.secondarySelectedControlColor;
    hicolor := NSColor.darkGrayColor;

  rows := rowsInRect(clipRect);
  cnt := rows.length;
  r := rows.location;
  hicolor.setFill;
  while cnt>0 do
  begin
    if isRowSelected(r) then NSRectFill(rectOfRow(r));
    inc(r);
    dec(cnt);
  end;
end;

end.

