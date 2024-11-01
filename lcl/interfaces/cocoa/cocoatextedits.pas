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
  Types, Classes, SysUtils,
  Math, // needed for MinDouble, MaxDouble
  LCLType,
  MacOSAll, CocoaAll, CocoaConfig, CocoaUtils, CocoaGDIObjects,
  CocoaPrivate, CocoaCallback;

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

  COMBOBOX_RO_ROUND_SIZE = 7;
  COMBOBOX_RO_BUTTON_WIDTH = 18;

type

  TCocoaFieldEditor = objcclass;

  NSTextField_LCLExt = objcprotocol
    procedure lclSetMaxLength(amax: integer); message 'lclSetMaxLength:';
  end;

  { TCocoaTextField }

  TCocoaTextField = objcclass(NSTextField, NSTextField_LCLExt)
    callback: ICommonCallback;
    maxLength: Integer;
    fixedInitSetting: Boolean;

    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // key
    procedure textDidChange(notification: NSNotification); override;
    function textView_shouldChangeTextInRange_replacementString (textView: NSTextView; affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL; message 'textView:shouldChangeTextInRange:replacementString:';
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;

    procedure lclSetMaxLength(amax: integer);
  end;

  { TCocoaSecureTextField }

  TCocoaSecureTextField = objcclass(NSSecureTextField, NSTextField_LCLExt)
  public
    maxLength: Integer;
    callback: ICommonCallback;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
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
    procedure scrollWheel(event: NSEvent); override;

    procedure lclSetMaxLength(amax: integer);
  end;

  { TCocoaTextView }

  TCocoaTextView = objcclass(NSTextView, NSTextDelegateProtocol, NSTextViewDelegateProtocol)
  public
    callback: ICommonCallback;
    FEnabled: Boolean;
    FUndoManager: NSUndoManager;

    supressTextChangeEvent: Integer; // if above zero, then don't send text change event
    wantReturns: Boolean;

    procedure dealloc; override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;

    procedure changeColor(sender: id); override;
    // keyboard
    procedure cut(sender: id); override;
    procedure paste(sender: id); override;
    procedure insertNewline(sender: id); override;
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
    function undoManagerForTextView(view: NSTextView): NSUndoManager; message 'undoManagerForTextView:';
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
    function lclGetCallback: ICommonCallback; override;
    function becomeFirstResponder: LCLObjCBoolean; override;
    procedure setDelegate(adelegate: NSTextDelegateProtocol); override;
    procedure lclReviseCursorColor; message 'lclReviseCursorColor';
    // keyboard
    procedure cut(sender: id); override;
    procedure paste(sender: id); override;
    procedure insertNewline(sender: id); override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

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

  TCocoaSpinEdit = objcclass(TCocoaTextField)
    Stepper: NSStepper;
    NumberFormatter: NSNumberFormatter;
    decimalPlaces: Integer;

    avoidChangeEvent: Integer;
    anyChange: Boolean;

    //Spin: TCustomFloatSpinEdit;
    procedure dealloc; override;
    function updateStepper: boolean; message 'updateStepper';
    procedure UpdateControl(min, max, inc, avalue: double; ADecimalPlaces: Integer; checkMinMax: Boolean); message 'UpdateControl::::::';
    procedure lclCreateSubcontrols(const AParams: TCreateParams); message 'lclCreateSubControls:';
    procedure lclReleaseSubcontrols; message 'lclReleaseSubcontrols';
    procedure PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer); message 'PositionSubcontrols:ATop:AWidth:AHeight:';
    procedure StepperChanged(sender: NSObject); message 'StepperChanged:';
    procedure textDidChange(notification: NSNotification); override;
    procedure textDidEndEditing(notification: NSNotification); override;
    // lcl
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
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
  end;
{$ENDIF}

// these constants are missing from CocoaAll for some reason
const
  NSTextAlignmentLeft      = 0;
  NSTextAlignmentRight     = {$ifdef USE_IOS_VALUES}2{$else}1{$endif}; // it's 2 for iOS and family
  NSTextAlignmentCenter    = {$ifdef USE_IOS_VALUES}1{$else}2{$endif}; // it's 1 for iOS and family
  NSTextAlignmentJustified = 3;
  NSTextAlignmentNatural   = 4;

function GetFieldEditor(afield: NSTextField): TCocoaFieldEditor;

implementation

uses CocoaWSStdCtrls;

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
  Result := NSViewCanFocus(Self);
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
  ctxRect: TRect;
  isHighlighted: Boolean;
begin
  inherited drawRect(dirtyRect);

  if not Assigned(combobox) then Exit;

  isHighlighted:= self.combobox.itemAtIndex(itemIndex).isHighlighted;

  ctx := TCocoaContext.Create(NSGraphicsContext.currentContext);
  try
    ctxRect:= NSRectToRect( bounds );
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

function ReverseColor(clr: NSColor): NSColor;
var
  r,g,b: byte;
begin
  r := $FF xor byte(Round(clr.redComponent * 255));
  g := $FF xor byte(Round(clr.greenComponent * 255));
  b := $FF xor byte(Round(clr.blueComponent * 255));
  Result := NSColor.colorWithDeviceRed_green_blue_alpha(r / 255, g / 255, b / 255, 1);
end;

function TCocoaFieldEditor.becomeFirstResponder: LCLObjCBoolean;
begin
  if goingReadOnly then Result := false
  else Result:=inherited becomeFirstResponder;
end;

procedure TCocoaFieldEditor.setDelegate(adelegate: NSTextDelegateProtocol);
begin
  inherited setDelegate(adelegate);
  lclReviseCursorColor;
end;

procedure TCocoaFieldEditor.lclReviseCursorColor;
var
  clr :NSColor;
begin
  if not Assigned(delegate) then Exit;
  if not (NSObject(delegate).isKindOfClass(NSTextField)) then Exit;
  clr := NSTextField(delegate).backgroundColor.colorUsingColorSpace(NSColorSpace.deviceRGBColorSpace);

  if Assigned(clr) then
    setInsertionPointColor(ReverseColor(clr));
end;

procedure TCocoaFieldEditor.cut(sender: id);
var
  callback: ICommonCallback;
  accept: Boolean = False;
begin
  callback:= self.lclGetCallback;
  if Assigned(callback) then
    accept:= callback.SendOnEditCut;
  if accept then
    inherited cut(sender);
end;

procedure TCocoaFieldEditor.paste(sender: id);
var
  callback: ICommonCallback;
  accept: Boolean = False;
begin
  callback:= self.lclGetCallback;
  if Assigned(callback) then
    accept:= callback.SendOnEditPaste;
  if accept then
    inherited paste(sender);
end;

procedure TCocoaFieldEditor.insertNewline(sender: id);
begin
  // 10.6 cocoa handles the editors Return key as "insertNewLine" command (that makes sense)
  // which turns into textDidEndEditing done command (that also makes sense)
  // however, it ends up in an endless loop of "end-editing" calls.
  //
  // todo: find the reason for the endless loop and resolve it properly
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

procedure TCocoaFieldEditor.scrollWheel(event: NSEvent);
var
  v : NSView;
begin
  v := GetEditBox(Self);
  if Assigned(v) then
  begin
    if Assigned(v.lclGetCallback) and not v.lclGetCallback.scrollWheel(event) then
      inherited mouseMoved(event);
  end else
    inherited scrollWheel(event);
end;

{ TCocoaTextField }

function TCocoaTextField.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := NSViewCanFocus(Self);
end;

function TCocoaTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextField.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextField.textDidChange(notification: NSNotification);
begin
  if (maxLength>0) and Assigned(stringValue) and (stringValue.length > maxLength) then
    setStringValue(stringValue.substringWithRange(NSMakeRange(0,maxLength)));
  if callback <> nil then
    callback.SendOnTextChanged;
end;

// detect and remove line-break when entering text
// TextField (TEdit) should be single line
function TCocoaTextField.textView_shouldChangeTextInRange_replacementString (textView: NSTextView; affectedCharRange: NSRange; replacementString: NSString): ObjCBOOL;
var
  newString: NSString;   // need not release
begin
  Result:= true;
  newString:= NSStringRemoveLineBreak( replacementString );
  if newString.length <> replacementString.length then
  begin
    // only handled if there is line-break in replacementString
    // use insertText() for undo/redo support
    // textDidChange will be called in insertText()
    if newString.length>0 then currentEditor.insertText( newString );
    Result:= false;
  end;
end;

procedure TCocoaTextField.mouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);
    // the text selection is handled withing mouseDown
    if Assigned(callback) and isSelectable  then
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

procedure TCocoaTextField.scrollWheel(event: NSEvent);
begin
  if Assigned(callback) and not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaTextField.lclSetMaxLength(amax: integer);
begin
  maxLength := amax;
end;

{ TCocoaTextView }

procedure TCocoaTextView.changeColor(sender: id);
begin
  //preventing text color from being changed
  //inherited changeColor(sender);
end;

procedure TCocoaTextView.cut(sender: id);
var
  accept: Boolean = False;
begin
  if Assigned(callback) then
    accept:= callback.SendOnEditCut;
  if accept then
    inherited cut(sender);
end;

procedure TCocoaTextView.paste(sender: id);
var
  accept: Boolean = False;
begin
  if Assigned(callback) then
    accept:= callback.SendOnEditPaste;
  if accept then
    inherited paste(sender);
end;

procedure TCocoaTextView.dealloc;
begin
  if Assigned(FUndoManager) then
    FUndoManager.release;
  inherited dealloc;
end;

function TCocoaTextView.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := NSViewCanFocus(Self);
end;

function TCocoaTextView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextView.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaTextView.insertNewline(sender: id);
begin
  if wantReturns then
    inherited insertNewline(sender);
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

function TCocoaTextView.undoManagerForTextView(view: NSTextView): NSUndoManager;
begin
  if not Assigned(FUndoManager) then
    FUndoManager := NSUndoManager.alloc.init;
  Result := FUndoManager;
end;

{ TCocoaSecureTextField }

function TCocoaSecureTextField.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := NSViewCanFocus(Self);
end;

function TCocoaSecureTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSecureTextField.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaSecureTextField.textDidChange(notification: NSNotification);
begin
  inherited;
  if (maxLength>0) and Assigned(stringValue) and (stringValue.length > maxLength) then
    setStringValue(stringValue.substringWithRange(NSMakeRange(0,maxLength)));
  if callback <> nil then
    callback.SendOnTextChanged;
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

procedure TCocoaSecureTextField.scrollWheel(event: NSEvent);
begin
  if Assigned(callback) and not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaSecureTextField.lclSetMaxLength(amax: integer);
begin
  MaxLength := amax;
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
  Result := NSViewCanFocus(Self);
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
  Result := NSViewCanFocus(Self);
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
      inc( dr.Left, COMBOBOX_RO_ROUND_SIZE );
      inc( dr.Top, 2 );
      dec( dr.Right, COMBOBOX_RO_BUTTON_WIDTH );
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

procedure TCocoaReadOnlyComboBoxMenuDelegate.menu_willHighlightItem(
  menu: NSMenu; item: NSMenuItem);
begin
  if Assigned(_lastHightlightItem) then
    _lastHightlightItem.view.setNeedsDisplay_( True );
  _lastHightlightItem:= item;
end;

procedure TCocoaReadOnlyComboBoxMenuDelegate.menuDidClose(menu: NSMenu);
begin
  TCocoaReadOnlyComboBox(_comboBox).comboboxAction( nil );
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


// * value       - the current value to be selected. Note: it will be adjusted
//                 to "min" and "max", if "checkMinMax" is set to true
// * min, max    - minimum maximum values allowed. for NSStepper, there's no "unlimited"
//                 check. "Something" should be set as minimum and maximum
// * checkMinMax - indicates if the min and max values should be verified.
//                 in LCL is Min = Max then there's not boundries check applied
//                 Since "min" and "max" are doubles, it's safer to use a booleanFlag
//                 so the comparison is done by the caller, rather than using approximation
// * inc         - incremeantal value to be used when "up" or "down" are pressed
// * DecimalPlaces - the number of decimals to used when showing the value
procedure TCocoaSpinEdit.UpdateControl(min, max, inc, avalue: double; ADecimalPlaces: Integer; checkMinMax: Boolean);
var
  notifyChange : Boolean;
  v : double;
begin
  Stepper.setIncrement(inc);

  v := avalue;
  if checkMinMax then
  begin
    if v < min then v := min
    else if v > max then v := max;
  end;

  notifyChange := (v <> Stepper.doubleValue) or (decimalPlaces <> ADecimalPlaces);

  // set min/max after checking for notify change
  // .doubleValue would be adjusted by setting min/max
  decimalPlaces := ADecimalPlaces;
  if checkMinMax then begin
    Stepper.setMinValue(min);
    Stepper.setMaxValue(max);
  end else begin
    // "Math" unit declared MaxDouble and MinDouble constants
    // however, using setMinValue to MinDouble, seems to be ignored
    // and Cocoa falls back to the default "0". not sure why.
    // Instead using -MaxDouble.
    Stepper.setMinValue(-MaxDouble);
    Stepper.setMaxValue(+MaxDouble);
  end;

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
  if (callback <> nil) and (avoidChangeEvent=0) then
    callback.SendOnTextChanged();
end;

procedure TCocoaSpinEdit.textDidChange(notification: NSNotification);
var
  w : NSWindow;
begin
  w := Self.window;
  if Assigned(w)
      and (w.firstResponder.isKindOfClass(NSTextView))
      and (NSObject(NSTextView(w.firstResponder).delegate) = self) // is focused
  then
  begin
    anyChange:=true;
    updateStepper; // just update float value, keep text as is!
    if (callback <> nil) and (avoidChangeEvent=0) then
      callback.SendOnTextChanged();
  end
  else
  begin
    // not focused
    inc(avoidChangeEvent);
    try
      updateStepper;
      StepperChanged(nil); // and refresh self
      inherited textDidChange(notification);
    finally
      dec(avoidChangeEvent);
    end;
  end;
end;

procedure TCocoaSpinEdit.textDidEndEditing(notification: NSNotification);
begin
  if anyChange then
  begin
    updateStepper;
    StepperChanged(nil); // and refresh self
    anyChange := false;
  end;
  inherited textDidEndEditing(notification);
end;

function TCocoaSpinEdit.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := NSViewCanFocus(Self);
end;

function TCocoaSpinEdit.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSpinEdit.lclClearCallback;
begin
  callback := nil;
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
  if Assigned(superview) and NOT superview.isFlipped then
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

{$ENDIF}

end.

