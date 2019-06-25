{ $Id: $}
{                  --------------------------------------------
                  cocoatextedits.pas  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaTextEdits;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$interfaces corba}
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_SETBOUNDS}
{.$DEFINE COCOA_SPIN_DEBUG}
{.$DEFINE COCOA_SPINEDIT_INSIDE_CONTAINER}

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

const
  SPINEDIT_DEFAULT_STEPPER_WIDTH = 15;
  SPINEDIT_EDIT_SPACING_FOR_SELECTION = 4;

  // From Interface Builder MacOSX 10.6
  // The heights are from layout rectangle.
  COMBOBOX_REG_HEIGHT   = 20;
  COMBOBOX_SMALL_HEIGHT = 17;
  COMBOBOX_MINI_HEIGHT  = 14;

  COMBOBOX_RO_REG_HEIGHT   = 20;
  COMBOBOX_RO_SMALL_HEIGHT = 17;
  COMBOBOX_RO_MINI_HEIGHT  = 15;


type

  TCocoaFieldEditor = objcclass;

  { TCocoaTextField }

  TCocoaTextField = objcclass(NSTextField)
    callback: ICommonCallback;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    // key
    procedure textDidChange(notification: NSNotification); override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
  end;

  { TCocoaSecureTextField }

  TCocoaSecureTextField = objcclass(NSSecureTextField)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    procedure resetCursorRects; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // key
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
  end;

  { TCocoaTextView }

  TCocoaTextView = objcclass(NSTextView, NSTextDelegateProtocol, NSTextViewDelegateProtocol)
  public
    callback: ICommonCallback;
    FEnabled: Boolean;
    FUndoManager: NSUndoManager;

    supressTextChangeEvent: Integer; // if above zero, then don't send text change event
    keyCaptured: Boolean;
    wantReturns: Boolean;

    preventInput: Boolean;

    procedure dealloc; override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function canBecomeKeyView: Boolean; override;
    function undoManager: NSUndoManager; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;

    procedure changeColor(sender: id); override;
    // keyboard
    procedure doCommandBySelector(aSelector: SEL); override;
    procedure insertNewline(sender: id); override;
    procedure keyDown(event: NSEvent); override;
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

    function lclIsEnabled: Boolean; override;
    procedure lclSetEnabled(AEnabled: Boolean); override;

    // delegate methods
    procedure textDidChange(notification: NSNotification); message 'textDidChange:';
    procedure lclExpectedKeys(var wantTabs, wantArrows, wantReturn, wantAll: Boolean); override;
  end;

  { TCococaFieldEditorExt }

  TCococaFieldEditorExt = objccategory(NSTextView)
    // this override should take care of any Cocoa editors possible
    // for example NSSecureTextView used with TCocoaSecureField aka NSSecureTextField
    function lclGetCallback: ICommonCallback; reintroduce;
  end;

  { TCocoaFieldEditor }

  TCocoaFieldEditor = objcclass(NSTextView)
  public
    // This flag is used to hack an infinite loop
    // when switching "editable" (readonly) mode of NSTextField
    // see TCocoaWSCustomEdit.SetReadOnly
    goingReadOnly: Boolean;
    keyCaptured: Boolean;
    function lclGetCallback: ICommonCallback; override;
    function becomeFirstResponder: LCLObjCBoolean; override;
    // keyboard
    procedure doCommandBySelector(aSelector: SEL); override;
    procedure insertNewline(sender: id); override;
    procedure keyDown(event: NSEvent); override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
  end;

const
  COMBOBOX_RO_MENUITEM_HEIGHT = 18;

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

    procedure ComboBoxDrawItem(itemIndex: Integer; ctx: TCocoaContext;
      const r: TRect; isSelected: Boolean);
  end;

  { TCocoaComboBox }

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
    procedure resetCursorRects; override;
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
    itemIndex: Integer;
    combobox: TCocoaReadOnlyComboBox;
    isMouseOver: Boolean;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseEntered(theEvent: NSEvent); override;
    procedure mouseExited(theEvent: NSEvent); override;
  end;

  { TCocoaReadOnlyComboBoxList }

  TCocoaReadOnlyComboBoxList = class(TCocoaComboBoxList)
  protected
    FOwner: TCocoaReadOnlyComboBox;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
    procedure Put(Index: Integer; const S: string); override;
  public
    // Pass only 1 owner and nil for the other ones
    constructor Create(AOwner: TCocoaReadOnlyComboBox);
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
  end;

  { TCocoaReadOnlyComboBox }

  TCocoaReadOnlyComboBox = objcclass(NSPopUpButton)
  public
    //Owner: TCustomComboBox;
    callback: IComboboxCallBack;
    list: TCocoaComboBoxList;
    resultNS: NSString;  //use to return values to combo
    lastSelectedItemIndex: Integer; // -1 means invalid or none selected

    isOwnerDrawn: Boolean;
    isOwnerMeasure: Boolean;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclGetFrameToLayoutDelta: TRect; override;
    procedure resetCursorRects; override;
    procedure comboboxAction(sender: id); message 'comboboxAction:';
    function stringValue: NSString; override;
    // drawing
    procedure drawRect(dirtyRect: NSRect); override;
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

  { TCocoaSpinEdit }
{$IFDEF COCOA_SPINEDIT_INSIDE_CONTAINER}
  TCocoaSpinEdit = objcclass(NSControl)
  public
    callback: ICommonCallback;
    Stepper: NSStepper;
    Edit: NSTextField;
    Spin: TCustomFloatSpinEdit;
    procedure dealloc; override;
    procedure UpdateControl(ASpinEdit: TCustomFloatSpinEdit); message 'UpdateControl:';
    procedure CreateSubcontrols(ASpinEdit: TCustomFloatSpinEdit; const AParams: TCreateParams); message 'CreateSubControls:AParams:';
    procedure PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer); message 'PositionSubcontrols:ATop:AWidth:AHeight:';
    procedure CalculateSubcontrolPos(const ASpinLCLBounds: TRect; out AEditBounds, AStepperBounds: TRect); message 'CalculateSubcontrolPos:AEditBounds:AStepperBounds:';
    procedure StepperChanged(sender: NSObject); message 'StepperChanged:';
    // lcl
    function acceptsFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // NSViewFix
    function fittingSize: NSSize; override;
  end;
{$ELSE}

  { TCocoaSpinEditStepper }

  TCocoaSpinEditStepper = objcclass(NSStepper)
    callback: ICommonCallback;
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

  TCocoaSpinEdit = objcclass(NSTextField, NSTextFieldDelegateProtocol)
    callback: ICommonCallback;
    Stepper: NSStepper;
    NumberFormatter: NSNumberFormatter;
    decimalPlaces: Integer;
    //Spin: TCustomFloatSpinEdit;
    procedure dealloc; override;
    function updateStepper: boolean; message 'updateStepper';
    procedure UpdateControl(min, max, inc, avalue: double; ADecimalPlaces: Integer); message 'UpdateControl:::::';
    procedure lclCreateSubcontrols(const AParams: TCreateParams); message 'lclCreateSubControls:';
    procedure lclReleaseSubcontrols; message 'lclReleaseSubcontrols';
    procedure PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer); message 'PositionSubcontrols:ATop:AWidth:AHeight:';
    procedure StepperChanged(sender: NSObject); message 'StepperChanged:';
    procedure textDidEndEditing(notification: NSNotification); message 'textDidEndEditing:'; override;
    // NSTextFieldDelegateProtocol
    procedure controlTextDidChange(obj: NSNotification); override;
    // lcl
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    procedure lclSetVisible(AVisible: Boolean); override;
    procedure lclSetFrame(const r: TRect); override;
    // NSViewFix
    function fittingSize: NSSize; override;
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
{$ENDIF}

// these constants are missing from CocoaAll for some reason
const
  NSTextAlignmentLeft      = 0;
  NSTextAlignmentRight     = 1; // it's 2 for iOS and family
  NSTextAlignmentCenter    = 2; // it's 1 for iOS and family
  NSTextAlignmentJustified = 3;
  NSTextAlignmentNatural   = 4;

function GetFieldEditor(afield: NSTextField): TCocoaFieldEditor;

implementation

function GetFieldEditor(afield: NSTextField): TCocoaFieldEditor;
var
  lFieldEditor: TCocoaFieldEditor;
  lText: NSText;
  window: NSWindow;
begin
  Result := nil;
  if not Assigned(afield) then Exit;
  window := afield.window;
  if window = nil then Exit;

  {$ifdef BOOLFIX}
  lText := window.fieldEditor_forObject_(Ord(True), afield);
  {$else}
  lText := window.fieldEditor_forObject(True, afield);
  {$endif}
  if (lText <> nil) and lText.isKindOfClass_(TCocoaFieldEditor) then
  begin
    Result := TCocoaFieldEditor(lText);
  end;
end;

{ TCocoaReadOnlyComboBoxList }

procedure TCocoaReadOnlyComboBoxList.InsertItem(Index: Integer;
  const S: string; O: TObject);
var
  astr     : NSString;
  mn       : NSMenuItem;
  menuItem : TCocoaReadOnlyView;
  track: NSTrackingArea;
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

  mn := FOwner.itemAtIndex(Index);
  if not Assigned(mn) then Exit;

  astr := NSStringUtf8(S);
  mn.setTitle(astr);
  astr.release;

  if FOwner.isOwnerDrawn then
  begin
    menuItem := TCocoaReadOnlyView.alloc.initWithFrame( NSMakeRect(0,0, FOwner.frame.size.width, COMBOBOX_RO_MENUITEM_HEIGHT) );
    menuItem.itemIndex := Index;
    menuItem.combobox := FOwner;

    track:=NSTrackingArea(NSTrackingArea.alloc).initWithRect_options_owner_userInfo(
      menuItem.bounds
      , NSTrackingMouseEnteredAndExited or NSTrackingActiveAlways
      , menuItem, nil);
    menuItem.addTrackingArea(track);
    track.release;
    mn.setView(menuItem);
  end;
end;

procedure TCocoaReadOnlyComboBoxList.Put(Index: Integer; const S: string);
var
  astr: NSString;
  mn  : NSMenuItem;
begin
  inherited Put(Index, S);
  if ((index >= 0) and (Index <= FOwner.numberOfItems)) then
  begin
    mn := FOwner.itemAtIndex(Index);
    astr := NSStringUtf8(S);
    mn.setTitle(astr);
    astr.release;
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

{ TCococaFieldEditorExt }

function TCococaFieldEditorExt.lclGetCallback: ICommonCallback;
begin
  if isFieldEditor and Assigned(delegate) then
  begin
    Result := NSObject(delegate).lclGetCallback;
  end
  else
    Result := inherited lclGetCallback;
end;

{ TCocoaSpinEditStepper }

function TCocoaSpinEditStepper.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

procedure TCocoaSpinEditStepper.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    if Assigned(Callback) then
      callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaSpinEditStepper.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSpinEditStepper.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSpinEditStepper.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSpinEditStepper.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaSpinEditStepper.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSpinEditStepper.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSpinEditStepper.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaSpinEditStepper.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSpinEditStepper.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaSpinEditStepper.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

{ TCocoaReadOnlyView }

procedure TCocoaReadOnlyView.drawRect(dirtyRect: NSRect);
var
  ctx : TCocoaContext;
begin
  inherited drawRect(dirtyRect);

  if not Assigned(combobox) then Exit;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  try
    ctx.InitDraw(Round(dirtyRect.size.width), Round(dirtyRect.size.height));
    combobox.callback.ComboBoxDrawItem(itemIndex, ctx, NSRectToRect(frame), isMouseOver);
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
    combobox.callback.ComboBoxSelectionDidChange;
    combobox.menu.performActionForItemAtIndex(itemIndex);
    combobox.menu.cancelTracking;
  end;
end;

procedure TCocoaReadOnlyView.mouseEntered(theEvent: NSEvent);
begin
  isMouseOver := true;
  inherited mouseEntered(theEvent);
  lclInvalidate;
end;

procedure TCocoaReadOnlyView.mouseExited(theEvent: NSEvent);
begin
  isMouseOver := false;
  inherited mouseExited(theEvent);
  lclInvalidate;
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

{ TCocoaFieldEditor }

function GetEditBox(src: TCocoaFieldEditor): NSView;
var
  v : NSObject;
begin
  Result := nil;
  if not Assigned(src) then Exit;
  v := NSObject(src.delegate);
  if Assigned(v) and (v.isKindOfClass(NSView)) then
    Result := NSView(v);
end;

function TCocoaFieldEditor.lclGetCallback: ICommonCallback;
begin
  if Assigned(delegate) then Result := NSObject(delegate).lclGetCallback
  else Result := nil;
end;

function TCocoaFieldEditor.becomeFirstResponder: LCLObjCBoolean;
begin
  if goingReadOnly then Result := false
  else Result:=inherited becomeFirstResponder;
end;

procedure TCocoaFieldEditor.doCommandBySelector(aSelector: SEL);
begin
  inherited doCommandBySelector(aSelector);
  keyCaptured := False;
end;

procedure TCocoaFieldEditor.insertNewline(sender: id);
begin
  // 10.6 cocoa handles the editors Return key as "insertNewLine" command (that makes sense)
  // which turns into textDidEndEditing done command (that also makes sense)
  // however, it ends up in an endless loop of "end-editing" calls.
  //
  // todo: find the reason for the endless loop and resolve it properly
end;

procedure TCocoaFieldEditor.keyDown(event: NSEvent);
begin
  // Input methods may capture Enter and Escape to accept/dismiss their popup
  // windows.  There isn't a way to detect the popup is open, so allow the
  // keys through.  If they make it to the default handlers let the LCL process
  // them further.  If they got swallowed prevent further processing.
  if Assigned(lclGetCallback) and (event.modifierFlags_ = 0) and
    ((NSEventRawKeyChar(event) = #13) or (NSEventRawKeyChar(event) = #27)) then
  begin
    keyCaptured := True;
    inherited keyDown(event);
    if keyCaptured then
      lclGetCallback.KeyEvHandled;
  end
  else
    inherited keyDown(event);
end;

procedure TCocoaFieldEditor.mouseDown(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
    begin
      inherited mouseDown(event);
      // NSTextView runs internal mouse-tracking loop in it's mouseDown implemenation.
      // Thus "inherited mouseDown" only returns after the mouse has been released.
      // why is TCocoaTextView not affected?
      if Assigned(v) and Assigned(v.lclGetCallback) then
        v.lclGetCallback.MouseUpDownEvent(event, true);
    end;
  end else
    inherited mouseDown(event);
end;

procedure TCocoaFieldEditor.mouseUp(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited mouseUp(event);
  end else
    inherited mouseUp(event);
end;

procedure TCocoaFieldEditor.rightMouseDown(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited rightMouseDown(event);
  end else
    inherited rightMouseDown(event);
end;

procedure TCocoaFieldEditor.rightMouseUp(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited rightMouseUp(event);
  end else
    inherited rightMouseUp(event);
end;

procedure TCocoaFieldEditor.otherMouseDown(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited otherMouseDown(event);
  end else
    inherited otherMouseDown(event);
end;

procedure TCocoaFieldEditor.otherMouseUp(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseUpDownEvent(event) then
      inherited otherMouseUp(event);
  end else
    inherited otherMouseUp(event);
end;

procedure TCocoaFieldEditor.mouseDragged(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseMove(event) then
      inherited mouseDragged(event);
  end else
    inherited mouseDragged(event);
end;

procedure TCocoaFieldEditor.mouseMoved(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.MouseMove(event) then
      inherited mouseMoved(event);
  end else
    inherited mouseMoved(event);
end;

{ TCocoaTextField }

function TCocoaTextField.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextField.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextField.resetCursorRects;
begin
  // this will not work well because
  // cocoa replaced TextField and TextView cursors in
  // mouseEntered, mouseMoved and CursorUpdate
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaTextField.textDidChange(notification: NSNotification);
begin
  if callback <> nil then
    callback.SendOnTextChanged;
end;

procedure TCocoaTextField.mouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    // the text selection is handled withing mouseDown
    if Assigned(callback) then
      callback.MouseUpDownEvent(event, True);
  end;
end;

procedure TCocoaTextField.mouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseUp(event);
  end;
end;

procedure TCocoaTextField.rightMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaTextField.rightMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaTextField.otherMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaTextField.otherMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaTextField.mouseDragged(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaTextField.mouseMoved(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

{ TCocoaTextView }

procedure TCocoaTextView.changeColor(sender: id);
begin
  //preventing text color from being changed
  //inherited changeColor(sender);
end;

procedure TCocoaTextView.dealloc;
begin
  if Assigned(FUndoManager) then
    FUndoManager.release;
  inherited dealloc;
end;

function TCocoaTextView.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := not preventInput;
end;

function TCocoaTextView.canBecomeKeyView: Boolean;
begin
  Result := not preventInput;
end;

function TCocoaTextView.undoManager: NSUndoManager;
begin
  if allowsUndo then
  begin
    if not Assigned(FUndoManager) then
      FUndoManager := NSUndoManager.alloc.init;
    Result := FUndoManager;
  end
  else
    Result := nil;
end;

function TCocoaTextView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextView.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextView.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaTextView.doCommandBySelector(aSelector: SEL);
begin
  inherited doCommandBySelector(aSelector);
  keyCaptured := False;
end;

procedure TCocoaTextView.insertNewline(sender: id);
begin
  if wantReturns then
    inherited insertNewline(sender);
end;

procedure TCocoaTextView.keyDown(event: NSEvent);
begin
  // See TCocoaFieldEditor.keyDown
  if Assigned(lclGetCallback) and (event.modifierFlags_ = 0) and
    ((NSEventRawKeyChar(event) = #13) or (NSEventRawKeyChar(event) = #27)) then
  begin
    keyCaptured := True;
    inherited keyDown(event);
    if keyCaptured then
      lclGetCallback.KeyEvHandled;
  end
  else
    inherited keyDown(event);
end;

procedure TCocoaTextView.mouseDown(event: NSEvent);
begin
  if Assigned(callback) then
  begin
    if not callback.MouseUpDownEvent(event) then
    begin
      inherited mouseDown(event);

      // Cocoa doesn't call mouseUp for NSTextView, so we have to emulate it here :(
      // See bug 29000
      if Assigned(callback) then
        callback.MouseUpDownEvent(event, True);
    end;
  end else
    inherited mouseDown(event);
end;

procedure TCocoaTextView.mouseUp(event: NSEvent);
begin
  if callback <> nil then
    callback.MouseUpDownEvent(event);
  inherited mouseUp(event);
end;

procedure TCocoaTextView.rightMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaTextView.rightMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaTextView.otherMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaTextView.otherMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaTextView.mouseDragged(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaTextView.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaTextView.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaTextView.mouseMoved(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

function TCocoaTextView.lclIsEnabled: Boolean;
begin
  Result := FEnabled;
end;

procedure TCocoaTextView.lclSetEnabled(AEnabled: Boolean);
begin
  FEnabled := AEnabled;
end;

procedure TCocoaTextView.textDidChange(notification: NSNotification);
begin
  if (callback <> nil) and (supressTextChangeEvent = 0) then
    callback.SendOnTextChanged;
end;

procedure TCocoaTextView.lclExpectedKeys(var wantTabs, wantArrows, wantReturn,
  wantAll: Boolean);
begin
  wantTabs := true;
  wantArrows := true;
  wantReturn := true;
  wantAll := true;
end;

{ TCocoaSecureTextField }

function TCocoaSecureTextField.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

procedure TCocoaSecureTextField.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

function TCocoaSecureTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSecureTextField.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaSecureTextField.mouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);

    if Assigned(callback) then
      callback.MouseUpDownEvent(event, True);
  end;
end;

procedure TCocoaSecureTextField.mouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSecureTextField.rightMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSecureTextField.rightMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSecureTextField.otherMouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSecureTextField.otherMouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSecureTextField.mouseDragged(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSecureTextField.mouseMoved(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

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
  Result := True;
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
begin
  idx := indexOfSelectedItem;
  if (idx>=0) and (idx<list.Count) and (list[idx]=string_.UTF8String) then
    // this is used for the case of the same items in the combobox
    Result:=idx
  else
  begin
    // todo: consider a faster search?
    idx := list.IndexOf(string_.UTF8String);
    if idx<0 then
      Result := NSNotFound
    else
      Result := idx;
  end;
end;

function TCocoaComboBox.numberOfItemsInComboBox(combo:TCocoaComboBox):NSInteger;
begin
  if not Assigned(list) then Result:=0
  else Result:=list.Count;
end;

procedure TCocoaComboBox.dealloc;
begin
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

procedure TCocoaComboBox.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaComboBox.comboBoxWillPopUp(notification: NSNotification);
begin
  callback.ComboBoxWillPopUp;
  isDown:=true;
end;

procedure TCocoaComboBox.comboBoxWillDismiss(notification: NSNotification);
begin
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
  Result := True;
end;

procedure TCocoaReadOnlyComboBox.dealloc;
begin
  if resultNS <> nil then resultNS.release;
  inherited dealloc;
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

procedure TCocoaReadOnlyComboBox.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
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
    Result:=inherited stringValue;
end;

procedure TCocoaReadOnlyComboBox.drawRect(dirtyRect: NSRect);
var
  ctx : TCocoaContext;
  r   : NSRect;
  rr  : NSRect;
  dr  : TRect;
begin
  inherited drawRect(dirtyRect);

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
      Types.OffsetRect(dr, -dr.Left, -dr.Top);
      SubLayoutFromFrame( lclGetFrameToLayoutDelta, dr);

      // magic offsets are based on the macOS 10.13.6 visual style
      // but hard-coding is never reliable
      inc(dr.Left, 11);
      inc(dr.Top, 5);
      dec(dr.Right,18);
      dec(dr.Bottom, 2);

      callback.ComboBoxDrawItem(lastSelectedItemIndex, ctx, dr, false);
    finally
      ctx.Free;
    end;
  end;
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

{ TCocoaSpinEdit }

{$IFDEF COCOA_SPINEDIT_INSIDE_CONTAINER}

procedure TCocoaSpinEdit.dealloc;
begin
  if Stepper <> nil then
    Stepper.release;
  if Edit <> nil then
    Edit.release;
  inherited dealloc;
end;

procedure TCocoaSpinEdit.UpdateControl(ASpinEdit: TCustomFloatSpinEdit);
begin
  Stepper.setMaxValue(ASpinEdit.MaxValue);
  Stepper.setMinValue(ASpinEdit.MinValue);
  Stepper.setIncrement(ASpinEdit.Increment);
  Stepper.setDoubleValue(ASpinEdit.Value);

  // update the UI too
  StepperChanged(Self);
end;

procedure TCocoaSpinEdit.CreateSubcontrols(ASpinEdit: TCustomFloatSpinEdit; const AParams: TCreateParams);
var
  lParams: TCreateParams;
  lEditRect, lStepperRect: TRect;
begin
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.CreateSubcontrols]');
  {$ENDIF}

  Spin := ASpinEdit;
  CalculateSubcontrolPos(Types.Bounds(AParams.X, AParams.Y, AParams.Width,
    AParams.Height), lEditRect, lStepperRect);

  // Now creates the subcontrols
  lParams := AParams;
  lParams.WndParent := HWND(Self);
  lParams.Style := AParams.Style or WS_VISIBLE;

  // Stepper
  lParams.X := lStepperRect.Left;
  lParams.Y := lStepperRect.Top;
  lParams.Width := lStepperRect.Right - lStepperRect.Left;
  lParams.Height := lStepperRect.Bottom - lStepperRect.Top;
  Stepper := NSStepper.alloc.lclInitWithCreateParams(lParams);
  Stepper.setValueWraps(False);

  // Edit
  lParams.X := lEditRect.Left;
  lParams.Y := lEditRect.Top;
  lParams.Width := lEditRect.Right - lEditRect.Left;
  lParams.Height := lEditRect.Bottom - lEditRect.Top;
  Edit := NSTextField.alloc.lclInitWithCreateParams(lParams);

  // Change event for the stepper
  Stepper.setTarget(Self);
  Stepper.setAction(objcselector('StepperChanged:'));
end;

procedure TCocoaSpinEdit.PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer);
var
  lNSStepperRect, lRect: NSRect;
  lStepperRect, lEditRect: TRect;
begin
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.PositionSubcontrols] AHeight=', AHeight);
  {$ENDIF}

  CalculateSubcontrolPos(Types.Bounds(ALeft, ATop, AWidth, AHeight), lEditRect, lStepperRect);

  // Stepper
  LCLToNSRect(lStepperRect, AHeight, lNSStepperRect);
  Stepper.setBounds(lNSStepperRect);

  // Edit
  LCLToNSRect(lEditRect, AHeight, lRect);
  Edit.setBounds(lRect);

  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn(':<[TCocoaSpinEdit.PositionSubcontrols] Edit=> X=', lRect.origin.x,
    ' Y=', lRect.origin.y, ' W=', lRect.size.width, ' H=', lRect.size.height,
    ' Stepper X=', lNSStepperRect.origin.x, ' Y=', lNSStepperRect.origin.y,
    ' W=', lNSStepperRect.size.width, ' H=', lNSStepperRect.size.height,
    ' frame.size.height=', frame.size.height);
  {$ENDIF}
end;

procedure TCocoaSpinEdit.CalculateSubcontrolPos(
  const ASpinLCLBounds: TRect; out AEditBounds, AStepperBounds: TRect);
var
  lWidth, lHeight: Integer;
begin
  lWidth := ASpinLCLBounds.Right - ASpinLCLBounds.Left;
  lHeight := ASpinLCLBounds.Bottom - ASpinLCLBounds.Top;

  // Stepper
  AStepperBounds.Left := lWidth - SPINEDIT_DEFAULT_STEPPER_WIDTH;
  AStepperBounds.Top := SPINEDIT_EDIT_SPACING_FOR_SELECTION;
  AStepperBounds.Right := lWidth;
  AStepperBounds.Bottom := lHeight - SPINEDIT_EDIT_SPACING_FOR_SELECTION;

  // Edit
  AEditBounds.Left := SPINEDIT_EDIT_SPACING_FOR_SELECTION;
  AEditBounds.Top := SPINEDIT_EDIT_SPACING_FOR_SELECTION;
  AEditBounds.Right := lWidth - SPINEDIT_DEFAULT_STEPPER_WIDTH;
  AEditBounds.Bottom := lHeight - SPINEDIT_EDIT_SPACING_FOR_SELECTION;

  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.CalculateSubcontrolPos] lWidth=', lWidth, ' lHeight=', lHeight,
    ' Stepper.Left=', AStepperBounds.Left, ' Stepper.Top=', AStepperBounds.Top,
    ' Stepper.Right=', AStepperBounds.Right, ' Stepper.Bottom=', AStepperBounds.Bottom,
    ' Edit.Left=', AEditBounds.Left, ' Edit.Top=', AEditBounds.Top,
    ' Edit.Right=', AEditBounds.Right, ' Edit.Bottom=', AEditBounds.Bottom
    );
  {$ENDIF}
end;

procedure TCocoaSpinEdit.StepperChanged(sender: NSObject);
var
  lNSStr: NSString;
  lStr: string;
begin
  lStr := Format('%.*f', [Spin.DecimalPlaces, Stepper.doubleValue()]);
  lNSStr := CocoaUtils.NSStringUtf8(lStr);
  Edit.setStringValue(lNSStr);
  lNSStr.release;
  // This implements OnChange for both user and code changes
  if callback <> nil then callback.SendOnTextChanged();
end;

function TCocoaSpinEdit.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaSpinEdit.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSpinEdit.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaSpinEdit.fittingSize: NSSize;
begin
  Result.width := -1;
  Edit.sizeToFit();
  Result.height := Edit.bounds.size.height + SPINEDIT_EDIT_SPACING_FOR_SELECTION * 2;
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.fittingSize] width=', Result.width,
    ' height=', Result.height);
  {$ENDIF}
end;

{$ELSE}

procedure TCocoaSpinEdit.dealloc;
begin
  lclReleaseSubControls;
  inherited dealloc;
end;

function TCocoaSpinEdit.updateStepper: boolean;
var
  lValid: Boolean = False;
  lValue: String;
  lFloat: Double;
begin
  lValue := CocoaUtils.NSStringToString(stringValue());
  lValid := SysUtils.TryStrToFloat(lValue, lFloat);
  if lValid then
  begin
    Stepper.setDoubleValue(lFloat);
    Result := true;
  end else
    Result := false;
end;

procedure TCocoaSpinEdit.UpdateControl(min, max, inc, avalue: double; ADecimalPlaces: Integer);
var
  notifyChange : Boolean;
  v : double;
begin
  Stepper.setIncrement(inc);

  v := avalue;
  if v < min then v := min
  else if v > max then v := max;

  notifyChange := (v <> Stepper.doubleValue) or (decimalPlaces <> ADecimalPlaces);

  // set min/max after checking for notify change
  // .doubleValue would be adjusted by setting min/max
  decimalPlaces := ADecimalPlaces;
  Stepper.setMinValue(min);
  Stepper.setMaxValue(max);

  if notifychange then
  begin
    Stepper.setDoubleValue(v);
    StepperChanged(Self);
  end;
end;

procedure TCocoaSpinEdit.lclCreateSubcontrols(const AParams: TCreateParams);
var
  lParams: TCreateParams;
begin
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.CreateSubcontrols]');
  {$ENDIF}

  // Now creates the subcontrols
  lParams := AParams;
  //lParams.Style := AParams.Style or WS_VISIBLE;

  // Stepper
  lParams.X := AParams.X + AParams.Width - SPINEDIT_DEFAULT_STEPPER_WIDTH;
  lParams.Width := SPINEDIT_DEFAULT_STEPPER_WIDTH;
  Stepper := TCocoaSpinEditStepper.alloc.lclInitWithCreateParams(lParams);
  TCocoaSpinEditStepper(Stepper).callback := callback;
  Stepper.setValueWraps(False);

  // Change event for the stepper
  Stepper.setTarget(Self);
  Stepper.setAction(objcselector('StepperChanged:'));

  // Accept numbers only
  setDelegate(Self);

  { The default way to do this in Cocoa is with NSNumberFormatter
    But it is a bit annoying, it just disallows losing focus from the control
    instead of the Windows like solution to just override with the last value
    If we ever want the Cocoa behavior, instead of implementing controlTextDidChange
    do this:
  var
  lNSStr: NSString;
  lStr: string;
  i: Integer;

  NumberFormatter := NSNumberFormatter.alloc.init;
  lStr := '##0';
  if ASpinEdit.DecimalPlaces > 0 then lStr := lStr + '.';
  for i := 0 to ASpinEdit.DecimalPlaces-1 do
    lStr := lStr + '0';
  lNSStr := CocoaUtils.NSStringUtf8(lStr);
  NumberFormatter.setFormat(lNSStr);
  lNSStr.release;
  NumberFormatter.setNumberStyle(NSNumberFormatterDecimalStyle);
  setFormatter(NumberFormatter);}
end;

procedure TCocoaSpinEdit.lclReleaseSubcontrols;
begin
  if Assigned(Stepper) then
  begin
    Stepper.removeFromSuperview;
    Stepper.release;
    Stepper := nil;
  end;
  if Assigned(NumberFormatter) then
  begin
    NumberFormatter.release;
    NumberFormatter := nil;
  end;
end;

procedure TCocoaSpinEdit.PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  lclSetFrame(Types.Bounds(ALeft, ATop, AWidth, AHeight));
end;

procedure TCocoaSpinEdit.StepperChanged(sender: NSObject);
var
  lNSStr: NSString;
  lStr: string;
begin
  // Stepper not might be assigend while creating or destroying handle
  if not Assigned(Stepper) then Exit;

  lStr := Format('%.*f', [DecimalPlaces, Stepper.doubleValue()]);
  lNSStr := CocoaUtils.NSStringUtf8(lStr);
  setStringValue(lNSStr);
  lNSStr.release;
  // This implements OnChange for both user and code changes
  if callback <> nil then callback.SendOnTextChanged();
end;

procedure TCocoaSpinEdit.textDidEndEditing(notification: NSNotification);
begin
  updateStepper;
  StepperChanged(nil); // and refresh self
  inherited textDidEndEditing(notification);
  //if Assigned(callback) then callback.SendOnTextChanged;
end;

procedure TCocoaSpinEdit.controlTextDidChange(obj: NSNotification);
begin
  updateStepper;
  if Assigned(callback) then callback.SendOnTextChanged;
end;

function TCocoaSpinEdit.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaSpinEdit.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSpinEdit.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaSpinEdit.resetCursorRects;
begin
  // this will not work well because
  // cocoa replaced TextField and TextView cursors in
  // mouseEntered, mouseMoved and CursorUpdate
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaSpinEdit.lclSetVisible(AVisible: Boolean);
begin
  inherited lclSetVisible(AVisible);
  {$ifdef BOOLFIX}
  Stepper.setHidden_(Ord(not AVisible));
  {$else}
  Stepper.setHidden(not AVisible);
  {$endif}
end;

procedure TCocoaSpinEdit.lclSetFrame(const r: TRect);
var
  ns, lStepperNS: NSRect;
  svHeight: CGFloat;
  lRect, lStepperRect: TRect;
begin
  lRect := r;
  lStepperRect := r;
  lRect.Right := lRect.Right - SPINEDIT_DEFAULT_STEPPER_WIDTH;
  lStepperRect.Left := lRect.Right;
  svHeight := GetNSViewSuperViewHeight(Self);
  if Assigned(superview)  then
  begin
    LCLToNSRect(lRect, svHeight, ns);
    LCLToNSRect(lStepperRect, svHeight, lStepperNS);
  end
  else
  begin
    ns := RectToNSRect(lRect);
    lStepperNS := RectToNSRect(lStepperRect);
  end;
  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  WriteLn(Format('LCLViewExtension.lclSetFrame: %s Bounds=%s height=%d ns_pos=%d %d ns_size=%d %d',
    [NSStringToString(Self.ClassName), dbgs(r), Round(svHeight),
     Round(ns.origin.x), Round(ns.origin.y), Round(ns.size.width), Round(ns.size.height)]));
  {$ENDIF}
  setFrame(ns);
  Stepper.setFrame(lStepperNS);
end;

function TCocoaSpinEdit.fittingSize: NSSize;
var
  fr : NSRect;
begin
  Result.width := -1;
  fr:=frame;
  sizeToFit();
  Result.height := bounds.size.height;
  if not NSEqualRects(frame, fr) then setFrame(fr); // prevent changes of frame after sizeToFit();
  {$IFDEF COCOA_SPIN_DEBUG}
  WriteLn('[TCocoaSpinEdit.fittingSize] width=', Result.width:0:0, ' height=', Result.height:0:0);
  {$ENDIF}
end;

function TCocoaSpinEdit.acceptsFirstMouse(event: NSEvent): LCLObjCBoolean;
begin
  Result:=true;
end;

procedure TCocoaSpinEdit.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    if Assigned(callback) then
      callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaSpinEdit.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSpinEdit.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSpinEdit.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSpinEdit.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaSpinEdit.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSpinEdit.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSpinEdit.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaSpinEdit.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSpinEdit.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaSpinEdit.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

{$ENDIF}

end.

