unit CocoaComboBox;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

interface

uses
  Types, Classes, SysUtils,
  LCLType, Graphics, Controls,
  MacOSAll, CocoaAll, CocoaConfig, CocoaUtils, CocoaGDIObjects,
  CocoaPrivate, CocoaCallback;

type
  TCocoaComboBox = objcclass;
  TCocoaReadOnlyComboBox = objcclass;

  { TCocoaComboBoxList }

  TCocoaComboBoxList = class(TStringList);

  TCocoaEditComboBoxList = class(TCocoaComboBoxList)
  protected
    FOwner: TCocoaComboBox;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
    procedure Changed; override;
  public
    // Pass only 1 owner and nil for the other ones
    constructor Create(AOwner: TCocoaComboBox);
  end;

  IComboboxCallBack = interface(ICommonCallBack)
    procedure ComboBoxWillPopUp;
    procedure ComboBoxWillDismiss;
    procedure ComboBoxSelectionDidChange;
    procedure ComboBoxSelectionIsChanging;

    procedure GetRowHeight(rowidx: integer; var h: Integer);
    procedure ComboBoxDrawItem(itemIndex: Integer; ctx: TCocoaContext;
      const r: TRect; isSelected: Boolean; backgroundPainted: Boolean );
  end;

  { TCocoaComboBoxItemCell }

  // represents an item in the combobox dropdown
  // it should be able to call "draw" callback

  TCocoaComboBoxItemCell = objcclass(NSTextFieldCell)
    procedure drawWithFrame_inView(cellFrame: NSRect; controlView_: NSView); override;
  end;

  { TCocoaComboBoxCell }

  // represents combobox itself. All functionality is implemented
  // in NSComboBoxCell. The cell is also acting as a delegate
  // for NSTextView, that's used in popup drop-down window.
  // Apple is deprecating "cells" so NSComboBox implementation
  // will change in future and it must be expected that NSComboBoxCell
  // would not be used in future.

  TCocoaComboBoxCell = objcclass(NSComboBoxCell)
    //function tableView_objectValueForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): id; message 'tableView:objectValueForTableColumn:row:';
    function tableView_dataCellForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSCell; message 'tableView:dataCellForTableColumn:row:';
    //function tableView_sizeToFitWidthOfColumn(tableView: NSTableView; column: NSInteger): CGFloat; message 'tableView:sizeToFitWidthOfColumn:';
    //procedure tableView_willDisplayCell_forTableColumn_row(tableView: NSTableView; cell: id; tableColumn: NSTableColumn; row: NSInteger); message 'tableView:willDisplayCell:forTableColumn:row:';
    //function tableView_heightOfRow(tableView: NSTableView; row: NSInteger): CGFloat; message 'tableView:heightOfRow:';
  end;

  { TCocoaComboBox }

  TCocoaComboBox = objcclass(NSComboBox, NSComboBoxDataSourceProtocol, NSComboBoxDelegateProtocol)
  private
    userSel: boolean;
  public
    callback: IComboboxCallBack;
    list: TCocoaComboBoxList;
    resultNS: NSString;  //use to return values to combo
    isDown: Boolean;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    procedure textDidChange(notification: NSNotification); override;
    // NSComboBoxDataSourceProtocol
    function comboBox_objectValueForItemAtIndex_(combo: TCocoaComboBox; row: NSInteger): id; message 'comboBox:objectValueForItemAtIndex:';
    function comboBox_indexOfItemWithStringValue(aComboBox: NSComboBox; string_: NSString): NSUInteger; message 'comboBox:indexOfItemWithStringValue:';
    function numberOfItemsInComboBox(combo: TCocoaComboBox): NSInteger; message 'numberOfItemsInComboBox:';
    //
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // NSComboBoxDelegateProtocol
    procedure comboBoxWillPopUp(notification: NSNotification); message 'comboBoxWillPopUp:';
    procedure comboBoxWillDismiss(notification: NSNotification); message 'comboBoxWillDismiss:';
    procedure comboBoxSelectionDidChange(notification: NSNotification); message 'comboBoxSelectionDidChange:';
    procedure comboBoxSelectionIsChanging(notification: NSNotification); message 'comboBoxSelectionIsChanging:';
    //
    procedure setStringValue(avalue: NSString); override;
    function lclGetFrameToLayoutDelta: TRect; override;
    // mouse
    function acceptsFirstMouse(event: NSEvent): LCLObjCBoolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

  { TCocoaReadOnlyView }

  TCocoaReadOnlyView = objcclass (NSView)
  private
    itemIndex: Integer;
    combobox: TCocoaReadOnlyComboBox;
  public
    procedure drawRect(dirtyRect: NSRect); override;
    procedure mouseUp(event: NSEvent); override;
  end;

  { TCocoaReadOnlyComboBoxList }

  TCocoaReadOnlyComboBoxList = class(TCocoaComboBoxList)
  private
    FId: Integer;
  protected
    FOwner: TCocoaReadOnlyComboBox;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
    procedure Put(Index: Integer; const S: string); override;
    procedure UpdateItemSize;
  public
    // Pass only 1 owner and nil for the other ones
    constructor Create(AOwner: TCocoaReadOnlyComboBox);
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
  end;

  { TCocoaReadOnlyComboBoxMenuDelegate }

  TCocoaReadOnlyComboBoxMenuDelegate = objcclass( NSObject, NSMenuDelegateProtocol )
  private
    _lastHightlightItem: NSMenuItem;
    _comboBox: NSPopUpButton;
    procedure menu_willHighlightItem (menu: NSMenu; item: NSMenuItem);
    procedure menuDidClose (menu: NSMenu);
  end;

  { TCocoaReadOnlyComboBox }

  TCocoaReadOnlyComboBox = objcclass(NSPopUpButton)
  private
    _menuDelegate: TCocoaReadOnlyComboBoxMenuDelegate;
    _textColorAttribs: NSDictionary;
    _defaultItemHeight: Integer;
  public
    //Owner: TCustomComboBox;
    callback: IComboboxCallBack;
    list: TCocoaComboBoxList;
    resultNS: NSString;  //use to return values to combo
    lastSelectedItemIndex: Integer; // -1 means invalid or none selected

    isOwnerDrawn: Boolean;
    isOwnerMeasure: Boolean;
    isComboBoxEx: Boolean;

    function initWithFrame(frameRect: NSRect): id; override;
    procedure setFrameSize(newSize: NSSize); override;
    procedure dealloc; override;

    function lclGetItemHeight( row: Integer ): Integer; message 'lclGetItemHeight:';
    function lclGetDefaultItemHeight: Integer; message 'lclGetDefaultItemHeight';
    procedure lclSetDefaultItemHeight(itemHeight: Integer); message 'lclSetItemHeight:';

    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclGetFrameToLayoutDelta: TRect; override;
    procedure comboboxAction(sender: id); message 'comboboxAction:';
    function stringValue: NSString; override;
    // drawing
    procedure drawRect(dirtyRect: NSRect); override;
    procedure setTextColor(newValue: NSColor); message 'setTextColor:';
    function colorTitle(ATitle: NSString): NSAttributedString; message 'colorTitle:';
    // mouse
    function acceptsFirstMouse(event: NSEvent): LCLObjCBoolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

implementation

uses CocoaWSComboBox;

{ TCocoaReadOnlyComboBoxList }

procedure TCocoaReadOnlyComboBoxList.InsertItem(Index: Integer;
  const S: string; O: TObject);
var
  astr     : NSString;
  mn       : NSMenuItem;
  menuItem : TCocoaReadOnlyView;
begin
  inherited InsertItem(Index, S, O);

  // Adding an item with its final name will cause it to be deleted,
  // so we need to first add all items with unique names, and then
  // rename all of them, see bug 30847
  astr := nsstr('@@@add');
  if Index >= FOwner.numberOfItems then
  begin
    FOwner.addItemWithTitle(astr);
    Index := FOwner.numberOfItems-1;
  end else
    FOwner.insertItemWithTitle_atIndex(astr, Index);

  // ItemIndex may be changed in addItemWithTitle() / insertItemWithTitle_atIndex()
  // keep compatibility with LCL by resetting ItemIndex
  FOwner.selectItemAtIndex(FOwner.lastSelectedItemIndex);

  mn := FOwner.itemAtIndex(Index);
  if not Assigned(mn) then Exit;

  // for TComboBoxEx, the parameter S passed in is always emtpy,
  // and NSPopUpButton always automatically sets the itemIndex according to
  // the title, so a unique title needs to be set.
  if not FOwner.isComboBoxEx then
    astr := NSStringUtf8(S)
  else
  begin
    astr := NSStringUtf8(FId.ToString);
    inc(FId);
  end;
  mn.setTitle(astr);
  mn.setAttributedTitle( FOwner.colorTitle(astr) );
  astr.release;

  if FOwner.isOwnerDrawn then
  begin
    menuItem := TCocoaReadOnlyView.alloc.initWithFrame(
      NSMakeRect(0,0, FOwner.frame.size.width, FOwner.lclGetItemHeight(index)) );
    menuItem.itemIndex := Index;
    menuItem.combobox := FOwner;
    mn.setView(menuItem);
    menuItem.release;
  end;
end;

procedure TCocoaReadOnlyComboBoxList.Put(Index: Integer; const S: string);
var
  selectedIndex: NSInteger;
begin
  inherited Put(Index, S);
  if ((index >= 0) and (Index < FOwner.numberOfItems)) then
  begin
    selectedIndex:= FOwner.indexOfSelectedItem;
    FOwner.removeItemAtIndex(Index);
    self.InsertItem(Index,S,nil);
    FOwner.selectItemAtIndex(selectedIndex);
  end;
end;

procedure TCocoaReadOnlyComboBoxList.UpdateItemSize;
var
  menuItem: NSMenuItem;
  itemView: TCocoaReadOnlyView;
  size: NSSize;
begin
  size.width:= FOwner.frame.size.width;
  for menuItem in FOwner.menu.itemArray do begin
    itemView:= TCocoaReadOnlyView( menuItem.view );
    size.height:= itemView.frame.size.height;
    itemView.setFrameSize( size );
  end;
end;

constructor TCocoaReadOnlyComboBoxList.Create(AOwner: TCocoaReadOnlyComboBox);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TCocoaReadOnlyComboBoxList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  if (Index>=0) and (Index < FOwner.numberOfItems) then
    FOwner.removeItemAtIndex(Index);
end;

procedure TCocoaReadOnlyComboBoxList.Clear;
begin
  inherited Clear;
  FOwner.removeAllItems;
end;

{ TCocoaReadOnlyView }

procedure TCocoaReadOnlyView.drawRect(dirtyRect: NSRect);
var
  ctx : TCocoaContext;
  ctxRect: TRect;
  isHighlighted: Boolean;
begin
  inherited drawRect(dirtyRect);

  if not Assigned(combobox) then Exit;

  isHighlighted:= self.combobox.itemAtIndex(itemIndex).isHighlighted;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  try
    ctxRect:= TCocoaTypeUtil.toRect( bounds );
    ctx.InitDraw( ctxRect.Width, ctxRect.Height );
    combobox.callback.ComboBoxDrawItem(itemIndex, ctx, ctxRect, isHighlighted, false);
  finally
    ctx.Free;
  end;
end;

procedure TCocoaReadOnlyView.mouseUp(event: NSEvent);
begin
  inherited mouseUp(event);
  if Assigned(combobox) then
  begin
    combobox.selectItemAtIndex(itemIndex);
    combobox.menu.cancelTracking;
  end;
end;

{ TCocoaComboBoxItemCell }

procedure TCocoaComboBoxItemCell.drawWithFrame_inView(cellFrame: NSRect; controlView_: NSView);
begin
  inherited drawWithFrame_inView(cellFrame, controlView_);
end;

function TCocoaComboBoxCell.tableView_dataCellForTableColumn_row(tableView: NSTableView; tableColumn: NSTableColumn; row: NSInteger): NSCell;
begin
  Result := TCocoaComboBoxItemCell.alloc.initTextCell(NSString.string_);
end;

{
procedure TCocoaComboBoxCell.tableView_willDisplayCell_forTableColumn_row(
  tableView: NSTableView; cell: id; tableColumn: NSTableColumn; row: NSInteger);
var
  sz : NSSize;
  pr : NSView;
  frm : NSRect;
begin
  writeln('will display ', row);
  if row = 0 then
  begin
    sz := tableView.frame.size;
    sz.width := 300;
    tableView.setFrameSize(sz);
    pr := tableView;
    while Assigned(pr) do begin
      writeln(pr.lclClassname);
      pr := pr.superview;
    end;
    writeln('at 10: ', tableView.window.lclClassName);
    writeln('max size = ', tableView.window.maxSize.width:0:0);
    writeln('min size = ', tableView.window.minSize.width:0:0);
    frm := tableView.window.frame;
    writeln('    size = ', frm.size.width:0:0);
    frm := NSView(tableView.window.contentView).frame;
    writeln('clt size = ', frm.size.width:0:0);
    frm.size.width := 96 * 2; //frm.size.width * 2;
    tableView.window.setContentSize(frm.size);
    writeln('clt size = ', frm.size.width:0:0);
  end;
end;
}

{function TCocoaComboBoxCell.tableView_heightOfRow(tableView: NSTableView;
  row: NSInteger): CGFloat;
begin
  writeln('height of row ', row);
  Result := 32;
end;}

{ TCocoaEditComboBoxList }

procedure TCocoaEditComboBoxList.InsertItem(Index: Integer; const S: string;
  O: TObject);
begin
  inherited InsertItem(Index, S, O);
  FOwner.noteNumberOfItemsChanged;
end;

procedure TCocoaEditComboBoxList.Changed;
begin
  inherited Changed;
  FOwner.reloadData;
end;

constructor TCocoaEditComboBoxList.Create(AOwner: TCocoaComboBox);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TCocoaComboBox }

procedure TCocoaComboBox.setStringValue(avalue: NSString);
var
  ch : Boolean;
  s  : NSString;
begin
  s := stringValue;
  ch := (Assigned(s)
        and Assigned(avalue)
        and (s.compare(avalue) <> NSOrderedSame));

  inherited setStringValue(avalue);

  if ch and userSel and Assigned(callback) then
    callback.SendOnChange;
end;

function TCocoaComboBox.lclGetFrameToLayoutDelta: TRect;
begin
  // todo: on 10.7 or later there's a special API for that!
    // The data is received from 10.6 Interface Builder
  case NSCell(Self.Cell).controlSize of
    NSSmallControlSize: begin
      Result.Left := 0;
      Result.Top := 1;
      Result.Right := -3;
      Result.Bottom := -4;
    end;
    NSMiniControlSize: begin
      Result.Left := 0;
      Result.Top := 1;
      Result.Right := -2;
      Result.Bottom := -4;
    end;
  else
    // NSRegularControlSize
    Result.Left := 0;
    Result.Top := 2;
    Result.Right := -3;
    Result.Bottom := -4;
  end;
end;

function TCocoaComboBox.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := TCocoaViewUtil.canLCLFocus(Self);
end;

procedure TCocoaComboBox.textDidChange(notification: NSNotification);
begin
  inherited textDidChange(notification);
  if Assigned(callback) then
    callback.SendOnChange;
end;

function TCocoaComboBox.comboBox_objectValueForItemAtIndex_(combo:TCocoaComboBox;
  row: NSInteger):id;
var
  ns : NSString;
begin
  if not Assigned(list) or (row<0) or (row>=list.Count) then
    Result:=nil
  else
  begin
    ns := NSStringUtf8(list[row]);
    Result := ns;
    ns.autorelease;
  end;
end;

function TCocoaComboBox.comboBox_indexOfItemWithStringValue(
  aComboBox: NSComboBox; string_: NSString): NSUInteger;
var
  idx : integer;
  lclString: String;
  lclCmb : TObject;
begin
  idx := indexOfSelectedItem;
  lclString := string_.UTF8String;
  lclCmb := lclGetTarget;
  if (idx>=0) and (idx<list.Count) and (list[idx]=lclString) then
    // this is used for the case of the same items in the combobox
    Result:=idx
  else
  begin
    idx := TCocoaWSCustomComboBox.GetObjectItemIndex(lclCmb);
    if idx<0 then
      Result := NSNotFound
    else
    begin
      // ComboBox.Text will be set to the List Item value after comboBox_indexOfItemWithStringValue()
      // so if cbactRetainPrefixCase set, ComboBox.Text should be reset Async
      TComboBoxAsyncHelper.ResetTextIfNecessary(self, lclString);
      Result := idx;
    end;
  end;
end;

function TCocoaComboBox.numberOfItemsInComboBox(combo:TCocoaComboBox):NSInteger;
begin
  if not Assigned(list) then Result:=0
  else Result:=list.Count;
end;

procedure TCocoaComboBox.dealloc;
begin
  FreeAndNil( list );
  if Assigned(resultNS) then resultNS.release;
  inherited dealloc;
end;

function TCocoaComboBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaComboBox.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaComboBox.comboBoxWillPopUp(notification: NSNotification);
begin
  self.setCompletes( TCocoaWSCustomComboBox.GetObjectAutoComplete(lclGetTarget) );
  callback.ComboBoxWillPopUp;
  isDown:=true;
end;

procedure TCocoaComboBox.comboBoxWillDismiss(notification: NSNotification);
begin
  self.setCompletes(false);
  callback.ComboBoxWillDismiss;
  isDown:=false;
end;

procedure TCocoaComboBox.comboBoxSelectionDidChange(notification: NSNotification);
var
  txt : NSString;
begin
  txt := comboBox_objectValueForItemAtIndex_(self, indexOfSelectedItem);
  if Assigned(txt) then setStringValue( txt );
  if userSel then
    callback.ComboBoxSelectionDidChange;
  userSel := false;
end;

procedure TCocoaComboBox.comboBoxSelectionIsChanging(notification: NSNotification);
begin
  userSel := true;
  callback.ComboBoxSelectionIsChanging;
end;

function TCocoaComboBox.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

procedure TCocoaComboBox.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaComboBox.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaComboBox.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaComboBox.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaComboBox.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaComboBox.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaComboBox.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaComboBox.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaComboBox.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaComboBox.mouseMoved(event: NSEvent);
begin
  // NSMouseMove event is being sent even after the selection is made.
  // In Cocoa world, the event is sent to the comboBox NSTextView edit section.
  // (even is the cursor is NOT over the NSTextView itself, but rather the popup window)
  //
  // The CocoaWS forwards the event to LCL. And LCL recognizes the event as a mouse action
  // beyond combobox boundries (it's NSMouseMove and not NSMouseDrag).
  // causing the issues with Object Inspector (where the mouse move with a left button down)
  // is recognized as a switch to a different row.
  //
  // WinAPI doesn't send MouseMove events when popup is dropped.
  // Enforcing the same approach for Cocoa. If combobox is showing popup
  // all mousemoves are suppressed
  if isDown then Exit;

  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaComboBox.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

{ TCocoaReadOnlyComboBox }

function TCocoaReadOnlyComboBox.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := TCocoaViewUtil.canLCLFocus(Self);
end;

function TCocoaReadOnlyComboBox.initWithFrame(frameRect: NSRect): id;
begin
  Result:=inherited initWithFrame(frameRect);
  _defaultItemHeight:= CocoaConfigComboBox.readOnly.item.defaultHeight;
  _menuDelegate:= TCocoaReadOnlyComboBoxMenuDelegate.new;
  _menuDelegate._comboBox:= self;
  self.menu.setDelegate( _menuDelegate );
end;

procedure TCocoaReadOnlyComboBox.setFrameSize(newSize: NSSize);
begin
  inherited setFrameSize(newSize);
  TCocoaReadOnlyComboBoxList(self.list).UpdateItemSize;
end;

procedure TCocoaReadOnlyComboBox.dealloc;
begin
  FreeAndNil( list );
  _menuDelegate.release;
  _textColorAttribs.release;
  if resultNS <> nil then resultNS.release;
  inherited dealloc;
end;

function TCocoaReadOnlyComboBox.lclGetDefaultItemHeight: Integer;
begin
  Result:= _defaultItemHeight;
end;

function TCocoaReadOnlyComboBox.lclGetItemHeight( row: Integer ): Integer;
begin
  if self.isOwnerMeasure and Assigned(self.callback) then
    self.callback.GetRowHeight( row, Result )
  else
    Result:= _defaultItemHeight;
end;

procedure TCocoaReadOnlyComboBox.lclSetDefaultItemHeight(itemHeight: Integer);
begin
  if itemHeight <= 0 then
    _defaultItemHeight:= CocoaConfigComboBox.readOnly.item.defaultHeight
  else
    _defaultItemHeight:= itemHeight;
end;

function TCocoaReadOnlyComboBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaReadOnlyComboBox.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaReadOnlyComboBox.lclGetFrameToLayoutDelta: TRect;
begin
  // todo: on 10.7 or later there's a special API for that!
    // The data is received from 10.6 Interface Builder
  case NSCell(Self.Cell).controlSize of
    NSSmallControlSize: begin
      Result.Left := 3;
      Result.Top := 1;
      Result.Right := -3;
      Result.Bottom := -4;
    end;
    NSMiniControlSize: begin
      Result.Left := 1;
      Result.Top := 0;
      Result.Right := -2;
      Result.Bottom := 0;
    end;
  else
    // NSRegularControlSize
    Result.Left := 3;
    Result.Top := 2;
    Result.Right := -3;
    Result.Bottom := -4;
  end;
end;

procedure TCocoaReadOnlyComboBox.comboboxAction(sender: id);
begin
  //setTitle(NSSTR(PChar(Format('%d=%d', [indexOfSelectedItem, lastSelectedItemIndex])))); // <= for debugging
  if Assigned(callback) then
    callback.SendOnChange;
  if (indexOfSelectedItem <> lastSelectedItemIndex) and (callback <> nil) then
    callback.ComboBoxSelectionDidChange;
  lastSelectedItemIndex := indexOfSelectedItem;
end;

function TCocoaReadOnlyComboBox.stringValue: NSString;
begin
  if Assigned(selectedItem) then
    Result:=selectedItem.title
  else
    Result:=NSString.string_;
end;

procedure TCocoaReadOnlyComboBox.drawRect(dirtyRect: NSRect);
var
  ctx : TCocoaContext;
  r   : NSRect;
  rr  : NSRect;
  dr  : TRect;
  t   : NSString;
begin
  if isOwnerDrawn then
  begin
    t := title;
    if Assigned(t) then t.retain;
    setTitle(NSString.string_);
  end else
    t := nil;

  inherited drawRect(dirtyRect);

  if Assigned(t) then
  begin
    setTitle(t);
    t.release;
  end;

  // if ownerDrawn style, then need to call "DrawItem" event
  if isOwnerDrawn and Assigned(callback)
    and (lastSelectedItemIndex>=0) and (lastSelectedItemIndex<list.Count)
  then
  begin
    ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
    try
      // todo: it's possible to query "cell" using titleRectForBounds method
      //       it actually returns somewhat desired offsets.
      //       (however, one should be careful and take layout offsets into account!)
      //       on the other hand, "cells" themselves are being deprecated...
      dr := lclFrame;

      // crop the drawing rectangle according to the
      // rounded corners and popup button of the ComboBox.
      // the APP can get better effect by drawing in the cropped rectangle.
      // the APP can also expand the rectangle if it knows what it is doing.
      Types.OffsetRect(dr, -dr.Left, -dr.Top);
      inc( dr.Left, CocoaConfigComboBox.readOnly.roundSize );
      inc( dr.Top, 2 );
      dec( dr.Right, CocoaConfigComboBox.readOnly.buttonWidth );
      inc( dr.Bottom, 1 );

      ctx.InitDraw(dr.Width, dr.Height);

      callback.ComboBoxDrawItem(lastSelectedItemIndex, ctx, dr, False, True);
    finally
      ctx.Free;
    end;
  end;
end;

procedure TCocoaReadOnlyComboBox.setTextColor(newValue: NSColor);
var
  item: NSMenuItem;
begin
  if Assigned(_textColorAttribs) then
    _textColorAttribs.release;
  _textColorAttribs:= NSMutableDictionary.alloc.initWithCapacity(1);
  _textColorAttribs.setValue_forKey( newValue, NSForegroundColorAttributeName );

  for item in self.itemArray do begin
    item.setAttributedTitle( self.colorTitle(item.title) );
  end;
end;

function TCocoaReadOnlyComboBox.colorTitle(ATitle: NSString
  ): NSAttributedString;
begin
  Result:= NSMutableAttributedString.alloc.initWithString_attributes(
             ATitle,
             _textColorAttribs );
  Result.autorelease;
end;

function TCocoaReadOnlyComboBox.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

procedure TCocoaReadOnlyComboBox.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    // a typical Apple "mouseDown" loop. The popup is shown on mouseDown event
    // The event only exists, whenever the popup is closed (for whatever reason)
    if Assigned(callback) then callback.ComboBoxWillPopUp;
    inherited mouseDown(event);
    if Assigned(callback) then callback.ComboBoxWillDismiss;
    callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaReadOnlyComboBox.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaReadOnlyComboBox.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaReadOnlyComboBox.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaReadOnlyComboBox.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaReadOnlyComboBox.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaReadOnlyComboBox.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaReadOnlyComboBox.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaReadOnlyComboBox.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaReadOnlyComboBox.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaReadOnlyComboBox.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

{ TCocoaReadOnlyComboBoxMenuDelegate }

// for OwnerDraw
procedure TCocoaReadOnlyComboBoxMenuDelegate.menu_willHighlightItem(
  menu: NSMenu; item: NSMenuItem);
begin
  if Assigned(_lastHightlightItem) then begin
    if menu.indexOfItem(_lastHightlightItem) >=0 then
      _lastHightlightItem.view.setNeedsDisplay_( True );
  end;
  _lastHightlightItem:= item;
end;

procedure TCocoaReadOnlyComboBoxMenuDelegate.menuDidClose(menu: NSMenu);
begin
  _lastHightlightItem:= nil;
  TCocoaReadOnlyComboBox(_comboBox).comboboxAction( nil );
end;

end.

