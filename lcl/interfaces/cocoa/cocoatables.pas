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

{.$DEFINE COCOA_DEBUG_LISTVIEW}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects,
  cocoa_extra, CocoaPrivate,
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
  end;

  { TCocoaStringList }

  TCocoaStringList = class(TStringList)
  protected
    procedure Changed; override;
  public
    Owner: NSTableView;
    constructor Create(AOwner: NSTableView);
  end;

  { TCocoaTableListView }

  TCocoaTableListView = objcclass(NSTableView, NSTableViewDelegateProtocol, NSTableViewDataSourceProtocol)
  public
    callback: IListViewCallback;

    readOnly: Boolean;

    beforeSel : NSIndexSet;

    isImagesInCell: Boolean;
    isFirstColumnCheckboxes: Boolean;
    isCustomDraw : Boolean;
    checkedIdx : NSMutableIndexSet;

    smallimages : NSMutableDictionary;

    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;

    // Own methods, mostly convenience methods
    function getIndexOfColumn(ACol: NSTableColumn): NSInteger; message 'getIndexOfColumn:';
    procedure reloadDataForRow_column(ARow, ACol: NSInteger); message 'reloadDataForRow:column:';

    function initWithFrame(frameRect: NSRect): id; override;
    procedure dealloc; override;
    procedure resetCursorRects; override;

    procedure drawRow_clipRect(row: NSInteger; clipRect: NSRect); override;

    // mouse
    procedure mouseDown(event: NSEvent); override;
    // procedure mouseUp(event: NSEvent); override;   This is eaten by NSTableView - worked around with NSTableViewDelegateProtocol
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    // key
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    function lclIsHandle: Boolean; override;
    procedure lclExpectedKeys(var wantTabs, wantKeys, wantAllKeys: Boolean); override;
    procedure lclSetFirstColumCheckboxes(acheckboxes: Boolean); message 'lclSetFirstColumCheckboxes:';
    procedure lclSetImagesInCell(aimagesInCell: Boolean); message 'lclSetImagesInCell:';

    procedure lclRegisterSmallImage(idx: Integer; img: NSImage); message 'lclRegisterSmallImage::';
    function lclGetSmallImage(idx: INteger): NSImage; message 'lclGetSmallImage:';

    // NSTableViewDataSourceProtocol
    function numberOfRowsInTableView(tableView: NSTableView): NSInteger; message 'numberOfRowsInTableView:';
    function tableView_objectValueForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id; message 'tableView:objectValueForTableColumn:row:';
    procedure tableView_setObjectValue_forTableColumn_row(tableView: NSTableView; object_: id; tableColumn: NSTableColumn; row: NSInteger); message 'tableView:setObjectValue:forTableColumn:row:';
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
    function tableView_toolTipForCell_rect_tableColumn_row_mouseLocation(tableView: NSTableView; cell: NSCell; rect: NSRectPointer; tableColumn: NSTableColumn; row: NSInteger; mouseLocation: NSPoint): NSString; message 'tableView:toolTipForCell:rect:tableColumn:row:mouseLocation:';
    function tableView_heightOfRow(tableView: NSTableView; row: NSInteger): CGFloat; message 'tableView:heightOfRow:';
    function tableView_typeSelectStringForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSString; message 'tableView:typeSelectStringForTableColumn:row:';
    function tableView_nextTypeSelectMatchFromRow_toRow_forString(tableView: NSTableView; startRow: NSInteger; endRow: NSInteger; searchString: NSString): NSInteger; message 'tableView:nextTypeSelectMatchFromRow:toRow:forString:';
    function tableView_shouldTypeSelectForEvent_withCurrentSearchString(tableView: NSTableView; event: NSEvent; searchString: NSString): Boolean; message 'tableView:shouldTypeSelectForEvent:withCurrentSearchString:';
    function tableView_shouldShowCellExpansionForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): Boolean; message 'tableView:shouldShowCellExpansionForTableColumn:row:';
    function tableView_shouldTrackCell_forTableColumn_row(tableView: NSTableView; cell: NSCell; tableColumn: NSTableColumn; row: NSInteger): Boolean; message 'tableView:shouldTrackCell:forTableColumn:row:';
    }
    function tableView_dataCellForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSCell; message 'tableView:dataCellForTableColumn:row:';
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

implementation

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

function TCocoaTableListView.lclIsHandle: Boolean;
begin
  Result:=true;
end;

procedure TCocoaTableListView.lclExpectedKeys(var wantTabs, wantKeys,
  wantAllKeys: Boolean);
begin
  wantTabs := false;
  wantKeys := true;
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

function TCocoaTableListView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaTableListView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaTableListView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
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
  if Assigned(checkedIdx) then checkedIdx.release;
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
  if not isCustomDraw then Exit;
  if not Assigned(callback) then Exit;
  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);

  ItemState := [];
  if isRowSelected(row) then Include(ItemState, odSelected);
  if lclIsEnabled then Include(ItemState, odDisabled);
  if Assigned(window) and (window.firstResponder = self) then Include(ItemState, odFocused);

  callback.DrawRow(row, ctx, NSRectToRect(rectOfRow(row)), ItemState);
end;

function TCocoaTableListView.getIndexOfColumn(ACol: NSTableColumn): NSInteger;
begin
  Result := tableColumns.indexOfObject(ACol);
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
  TCocoaTableListView(Result).checkedIdx := NSMutableIndexSet.alloc.init;
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

procedure TCocoaTableListView.keyDown(event: NSEvent);
var
  allow : Boolean;
begin
  if not Assigned(callback) then
    inherited keyDown(event)
  else
  begin
    callback.KeyEvPrepare(event);
    callback.KeyEvBefore(allow);
    if allow then inherited KeyDown(event);
    callback.KeyEvAfter;
  end;
end;

procedure TCocoaTableListView.keyUp(event: NSEvent);
var
  allow : Boolean;
begin
  if not Assigned(callback) then
    inherited KeyUp(event)
  else
  begin
    callback.KeyEvPrepare(event);
    callback.KeyEvBefore(allow);
    if allow then inherited KeyUp(event);
    callback.KeyEvAfter;
  end;
end;

function TCocoaTableListView.numberOfRowsInTableView(tableView: NSTableView
  ): NSInteger;
begin
  if Assigned(callback) then
    Result := callback.ItemsCount
  else
    Result := 0;
end;

function TCocoaTableListView.tableView_objectValueForTableColumn_row(
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

procedure TCocoaTableListView.tableView_setObjectValue_forTableColumn_row(
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
    begin
      isSel := NSNumber(object_).integerValue;
      if isSel = NSOffState
        then checkedIdx.addIndex(row)
        else checkedIdx.removeIndex(row);
      callback.SetItemCheckedAt(row, lColumnIndex, isSel);
      //reloadDataForRow_column(lColumnIndex, row);
    end;

    exit;
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

function TCocoaTableListView.tableView_dataCellForTableColumn_row(
  tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSCell;
var
  chk : Boolean;
  txt : string;
  col : Integer;
  nstxt : NSString;
  idx  : Integer;
  img  : NSImage;
begin
  Result:=nil;
  if not isFirstColumnCheckboxes and not isImagesInCell then Exit;

  col := getIndexOfColumn(tableColumn);
  if (col <> 0) then Exit;

  if not isFirstColumnCheckboxes and isImagesInCell then begin
    idx := -1;
    callback.GetItemImageAt(row, col, idx);
    if idx>=0 then
    begin
      img := lclGetSmallImage(idx);
      if not Assigned(img) then begin
        img := callback.GetImageFromIndex(idx);
        if Assigned(img) then lclRegisterSmallImage(idx, img);
      end;
    end else
      img := nil;

    Result := NSImageAndTextCell(NSImageAndTextCell.alloc).initTextCell(NSSTR(''));
    NSImageAndTextCell(Result).drawImage := img; // if "image" is assigned, text won't be drawn :(
    Exit;
  end;
  txt := '';
  chk := false;

  callback.GetItemTextAt(row, col, txt);

  if txt = '' then nstxt := NSString.string_
  else nstxt := NSString.stringWithUTF8String(@txt[1]);

  Result := NSButtonCell.alloc.init.autorelease;
  //Result.setAllowsMixedState(True);
  NSButtonCell(Result).setButtonType(NSSwitchButton);
  NSButtonCell(Result).setTitle(nstxt);
  if chk then begin
    NSButtonCell(Result).setIntValue(1);
    NSButtonCell(Result).setCellAttribute_to(NSCellState, NSOnState);
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

end.

