{ $Id: $}
{                  --------------------------------------------
                  cocoaprivate.pp  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaPrivate;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$interfaces corba}

{.$DEFINE COCOA_DEBUG_SETBOUNDS}
{.$DEFINE COCOA_DEBUG_LISTVIEW}
{.$DEFINE COCOA_SPIN_DEBUG}
{.$DEFINE COCOA_SPINEDIT_INSIDE_CONTAINER}
{.$DEFINE COCOA_SUPERVIEW_HEIGHT}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects,
  cocoa_extra,
  // LCL
  LMessages, LCLMessageGlue, ExtCtrls, Graphics, Forms,
  LCLType, LCLProc, Controls, ComCtrls, StdCtrls;

const
  SPINEDIT_DEFAULT_STEPPER_WIDTH = 15;
  SPINEDIT_EDIT_SPACING_FOR_SELECTION = 4;
  STATUSBAR_DEFAULT_HEIGHT = 18;

type

  { ICommonCallback }

  ICommonCallback = interface
    // mouse events
    function MouseUpDownEvent(Event: NSEvent; AForceAsMouseUp: Boolean = False): Boolean;
    procedure MouseClick;
    function MouseMove(Event: NSEvent): Boolean;
    function KeyEvent(Event: NSEvent; AForceAsKeyDown: Boolean = False): Boolean;
    function scrollWheel(Event: NSEvent): Boolean;
    // size, pos events
    procedure frameDidChange(sender: id);
    procedure boundsDidChange(sender: id);
    // misc events
    procedure Draw(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    procedure DrawBackground(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    procedure DrawOverlay(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    function ResetCursorRects: Boolean;
    procedure BecomeFirstResponder;
    procedure ResignFirstResponder;
    procedure DidBecomeKeyNotification;
    procedure DidResignKeyNotification;
    procedure SendOnChange;
    procedure SendOnTextChanged;
    procedure scroll(isVert: Boolean; Pos: Integer);
    // non event methods
    function DeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
    function GetPropStorage: TStringList;
    function GetContext: TCocoaContext;
    function GetTarget: TObject;
    function GetHasCaret: Boolean;
    function GetCallbackObject: TObject;
    procedure SetHasCaret(AValue: Boolean);
    function GetIsOpaque: Boolean;
    procedure SetIsOpaque(AValue: Boolean);
    function GetShouldBeEnabled: Boolean;

    // properties
    property HasCaret: Boolean read GetHasCaret write SetHasCaret;
    property IsOpaque: Boolean read GetIsOpaque write SetIsOpaque;
  end;

  { LCLObjectExtension }

  LCLObjectExtension = objccategory(NSObject)
    function lclIsEnabled: Boolean; message 'lclIsEnabled';
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:';
    function lclIsVisible: Boolean; message 'lclIsVisible';
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:';
    function lclWindowState: Integer; message 'lclWindowState';

    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclUpdate; message 'lclUpdate';
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::';
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::';
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::';
    function lclParent: id; message 'lclParent';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';
    function lclClientFrame: TRect; message 'lclClientFrame';
    function lclGetCallback: ICommonCallback; message 'lclGetCallback';
    procedure lclClearCallback; message 'lclClearCallback';
    function lclGetPropStorage: TStringList; message 'lclGetPropStorage';
    function lclGetTarget: TObject; message 'lclGetTarget';
    function lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; message 'lclDeliverMessage:::';
    function lclIsHandle: Boolean; message 'lclIsHandle';
    function lclContentView: NSView; message 'lclContentView';
    procedure lclOffsetMousePos(var Point: NSPoint); message 'lclOffsetMousePos:';
  end;

  { LCLViewExtension }

  LCLViewExtension = objccategory(NSView)
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;

    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:'; reintroduce;
    function lclIsPainting: Boolean; message 'lclIsPainting';
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclUpdate; message 'lclUpdate'; reintroduce;
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::'; reintroduce;
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::'; reintroduce;
    function lclParent: id; message 'lclParent'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
    function lclContentView: NSView; message 'lclContentView'; reintroduce;
    procedure lclOffsetMousePos(var Point: NSPoint); message 'lclOffsetMousePos:'; reintroduce;
  end;

  NSViewFix = objccategory external (NSView)
    function fittingSize: NSSize; message 'fittingSize';
  end;

  { LCLControlExtension }

  LCLControlExtension = objccategory(NSControl)
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;
  end;

  TCocoaFieldEditor = objcclass;

  { TCocoaTextField }

  TCocoaTextField = objcclass(NSTextField)
    callback: ICommonCallback;
    procedure dealloc; override;
    function GetFieldEditor: TCocoaFieldEditor; message 'GetFieldEditor';
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function RealResignFirstResponder: Boolean; message 'RealResignFirstResponder';
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    // key
    //procedure keyDown(event: NSEvent); override; -> keyDown doesn't work in NSTextField
    procedure keyUp(event: NSEvent); override;
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
    procedure dealloc; override;
    function GetFieldEditor: TCocoaFieldEditor; message 'GetFieldEditor';
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function RealResignFirstResponder: Boolean; message 'RealResignFirstResponder';
    function resignFirstResponder: Boolean; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    // key
    //procedure keyDown(event: NSEvent); override; -> keyDown doesn't work in NSTextField
    procedure keyUp(event: NSEvent); override;
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

    supressTextChangeEvent: Integer; // if above zero, then don't send text change event

    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    // key
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    procedure flagsChanged(event: NSEvent); override;
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
  end;

  { TCocoaFieldEditor }

  TCocoaFieldEditor = objcclass(NSTextView)
  public
    lastEditBox: NSTextField;
    function resignFirstResponder: Boolean; override;
    // keyboard
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

  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
  private
    fstr : NSString;

    isdrawing   : integer;
    faileddraw  : Boolean;
  public
    callback: ICommonCallback;
    procedure dealloc; override;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // mouse
    function acceptsFirstMouse(event: NSEvent): Boolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
    // key
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    procedure flagsChanged(event: NSEvent); override;
    // nsview
    procedure setFrame(aframe: NSRect); override;
    // other
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    // value
    procedure setStringValue(avalue: NSString); override;
    function stringValue: NSString; override;
  end;

  { TCocoaScrollView }

  TCocoaScrollView = objcclass(NSScrollView)
  public
    callback: ICommonCallback;
    isCustomRange: Boolean;

    docrect    : NSRect;    // have to remember old
    holdscroll : Integer; // do not send scroll messages
    function initWithFrame(ns: NSRect): id; override;
    procedure dealloc; override;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    function lclClientFrame: TRect; override;
    function lclContentView: NSView; override;
    procedure setDocumentView(aView: NSView); override;
    procedure scrollContentViewBoundsChanged(notify: NSNotification); message 'scrollContentViewBoundsChanged:';
    procedure resetScrollRect; message 'resetScrollRect';
  end;

  { TCocoaManualScrollView }

  TCocoaManualScrollView = objcclass(NSView)
  private
    fdocumentView: NSView;
    fhscroll : NSScroller;
    fvscroll : NSScroller;
  public
    callback: ICommonCallback;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclIsHandle: Boolean; override;
    function lclContentView: NSView; override;
    function lclClientFrame: TRect; override;

    procedure setDocumentView(AView: NSView); message 'setDocumentView:';
    function documentView: NSView; message 'documentView';

    procedure setHasVerticalScroller(doshow: Boolean); message 'setHasVerticalScroller:';
    procedure setHasHorizontalScroller(doshow: Boolean); message 'setHasHorizontalScroller:';
    function hasVerticalScroller: Boolean; message 'hasVerticalScroller';
    function hasHorizontalScroller: Boolean; message 'hasHorizontalScroller';

    function horizontalScroller: NSScroller; message 'horizontalScroller';
    function verticalScroller: NSScroller; message 'verticalScroller';

    function allocHorizontalScroller(avisible: Boolean): NSScroller; message 'allocHorizontalScroller:';
    function allocVerticalScroller(avisible: Boolean): NSScroller; message 'allocVerticalScroller:';
  end;

  TStatusItemData = record
    Text  : NSString;
    Width : Integer;
    Align : TAlignment;
  end;

  TStatusItemDataArray = array of TStatusItemData;

  { TCocoaStatusBar }

  TCocoaStatusBar = objcclass(TCocoaCustomControl)
  public
    StatusBar : TStatusBar;
    panelCell : NSCell;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
  end;

  TCocoaComboBox = objcclass;
  TCocoaReadOnlyComboBox = objcclass;

  { TCocoaComboBoxList }

  TCocoaComboBoxList = class(TStringList)
  protected
    FOwner: TCocoaComboBox;
    FReadOnlyOwner: TCocoaReadOnlyComboBox;
    FPreChangeListCount: Integer;
    procedure Changed; override;
    procedure Changing; override;
  public
    // Pass only 1 owner and nil for the other ones
    constructor Create(AOwner: TCocoaComboBox; AReadOnlyOwner: TCocoaReadOnlyComboBox);
  end;

  IComboboxCallBack = interface(ICommonCallBack)
    procedure ComboBoxWillPopUp;
    procedure ComboBoxWillDismiss;
    procedure ComboBoxSelectionDidChange;
    procedure ComboBoxSelectionIsChanging;
  end;

  { TCocoaComboBox }

  TCocoaComboBox = objcclass(NSComboBox, NSComboBoxDataSourceProtocol, NSComboBoxDelegateProtocol)
  public
    callback: IComboboxCallBack;
    list: TCocoaComboBoxList;
    resultNS: NSString;  //use to return values to combo
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure textDidChange(notification: NSNotification); override;
    procedure textDidEndEditing(notification: NSNotification); override;
    // NSComboBoxDataSourceProtocol
    function comboBox_objectValueForItemAtIndex_(combo: TCocoaComboBox; row: NSInteger): id; message 'comboBox:objectValueForItemAtIndex:';
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
    function lclIsHandle: Boolean; override;
    procedure setStringValue(avalue: NSString); override;
  end;

  { TCocoaReadOnlyComboBox }

  TCocoaReadOnlyComboBox = objcclass(NSPopUpButton)
  public
    Owner: TCustomComboBox;
    callback: IComboboxCallBack;
    list: TCocoaComboBoxList;
    resultNS: NSString;  //use to return values to combo
    lastSelectedItemIndex: Integer; // -1 means invalid or none selected
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    procedure comboboxAction(sender: id); message 'comboboxAction:';
    function stringValue: NSString; override;
  end;

  { TCocoaScrollBar }

  TCocoaScrollBar = objcclass(NSScroller)
  public
    callback: ICommonCallback;
    // minInt,maxInt are used to calculate position for lclPos and lclSetPos
    minInt  : Integer;
    maxInt  : Integer;
    pageInt : Integer;
    procedure actionScrolling(sender: NSObject); message 'actionScrolling:';
    function IsHorizontal: Boolean; message 'IsHorizontal';
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    function lclPos: Integer; message 'lclPos';
    procedure lclSetPos(aPos: integer); message 'lclSetPos:';
  end;

  { TCocoaGroupBox }

  TCocoaGroupBox = objcclass(NSBox)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    function lclClientFrame: TRect; override;
    function lclContentView: NSView; override;
  end;

  { TCocoaProgressIndicator }

  TCocoaProgressIndicator = objcclass(NSProgressIndicator)
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
  end;

  { TCocoaSlider }

  TCocoaSlider = objcclass(NSSlider)
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    //
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    //
    procedure SnapToInteger(AExtraFactor: Integer = 0); message 'SnapToInteger:';
    procedure sliderAction(sender: id); message 'sliderAction:';
  end;

  TCocoaSliderCell = objcclass(NSSliderCell)
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
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclIsHandle: Boolean; override;
    // NSViewFix
    function fittingSize: NSSize; override;
  end;
{$ELSE}
  TCocoaSpinEdit = objcclass(NSTextField, NSTextFieldDelegateProtocol)
    callback: ICommonCallback;
    Stepper: NSStepper;
    NumberFormatter: NSNumberFormatter;
    decimalPlaces: Integer;
    //Spin: TCustomFloatSpinEdit;
    procedure dealloc; override;
    function updateStepper: boolean; message 'updateStepper';
    procedure UpdateControl(min, max, inc, avalue: double; ADecimalPlaces: Integer); message 'UpdateControl:::::';
    procedure CreateSubcontrols(const AParams: TCreateParams); message 'CreateSubControls:';
    procedure PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer); message 'PositionSubcontrols:ATop:AWidth:AHeight:';
    procedure StepperChanged(sender: NSObject); message 'StepperChanged:';
    function GetFieldEditor: TCocoaFieldEditor; message 'GetFieldEditor';
    procedure textDidEndEditing(notification: NSNotification); message 'textDidEndEditing:'; override;
    // NSTextFieldDelegateProtocol
    procedure controlTextDidChange(obj: NSNotification); override;
    // lcl
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function RealResignFirstResponder: Boolean; message 'RealResignFirstResponder';
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclIsHandle: Boolean; override;
    procedure lclSetVisible(AVisible: Boolean); override;
    procedure lclSetFrame(const r: TRect); override;
    // NSViewFix
    function fittingSize: NSSize; override;
  end;
{$ENDIF}

procedure SetViewDefaults(AView: NSView);
function CheckMainThread: Boolean;
function GetNSViewSuperViewHeight(view: NSView): CGFloat;

procedure SetNSControlSize(ctrl: NSControl; newHeight, miniHeight, smallHeight: Integer; AutoChangeFont: Boolean);

// these constants are missing from CocoaAll for some reason
const
  NSTextAlignmentLeft      = 0;
  NSTextAlignmentRight     = 1; // it's 2 for iOS and family
  NSTextAlignmentCenter    = 2; // it's 1 for iOS and family
  NSTextAlignmentJustified = 3;
  NSTextAlignmentNatural   = 4;

implementation

uses CocoaInt;

{$I mackeycodes.inc}

procedure SetViewDefaults(AView: NSView);
begin
  if not Assigned(AView) then Exit;
  AView.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;

function CheckMainThread: Boolean;
begin
  Result := NSThread.currentThread.isMainThread;
end;

function GetNSViewSuperViewHeight(view: NSView): CGFloat;
begin
  Result := -1;
  if not Assigned(view) then Exit;
  if not Assigned(view.superview) then Exit;
  //if view.superview.isKindOfClass_(TCocoaTabPageView) then
    //Result := TCocoaTabPageView(view.superview).tabview.contentRect.size.height
  //else
    Result := view.superview.frame.size.height;
  {$IFDEF COCOA_SUPERVIEW_HEIGHT}
  WriteLn(Format('GetNSViewSuperViewHeight Result=%f', [Result]));
  {$ENDIF}
end;

{ TCocoaManualScrollView }

function TCocoaManualScrollView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaManualScrollView.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaManualScrollView.lclIsHandle: Boolean;
begin
  Result := true;
end;

function TCocoaManualScrollView.lclContentView: NSView;
begin
  Result:=fdocumentView;
end;

function TCocoaManualScrollView.lclClientFrame: TRect;
begin
  if Assigned(fdocumentView) then
  begin
    Result:=fdocumentView.lclClientFrame;
  end
  else Result:=inherited lclClientFrame;
end;

procedure TCocoaManualScrollView.setDocumentView(AView: NSView);
var
  f  : NSrect;
begin
  if fdocumentView=AView then Exit;
  if Assigned(fdocumentView) then
    fdocumentView.removeFromSuperview;

  fdocumentView:=AView;
  if Assigned(fdocumentView) then
  begin
    addSubview(fdocumentView);
    f:=fdocumentView.frame;
    f.origin.x:=0;
    f.origin.y:=0;
    fdocumentView.setFrame(f);
    fdocumentView.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
  end;
end;

function TCocoaManualScrollView.documentView: NSView;
begin
  Result:=fdocumentView;
end;

procedure allocScroller(parent: TCocoaManualScrollView; var sc: NSScroller; dst: NSRect; aVisible: Boolean);
begin
  sc:=TCocoaScrollBar(TCocoaScrollBar.alloc).initWithFrame(dst);
  parent.addSubview(sc);
  sc.setEnabled(true);
  sc.setHidden(not AVisible);
  TCocoaScrollBar(sc).callback:=parent.callback;
  sc.setTarget(sc);
  sc.setAction(objcselector('actionScrolling:'));

end;

procedure updateDocSize(parent: NSView; doc: NSView; hrz, vrt: NSScroller);
var
  f  : NSRect;
  hr : NSRect;
  vr : NSRect;
  hw : CGFLoat;
  vw : CGFLoat;
begin
  if not Assigned(parent) or not Assigned(doc) then Exit;

  f := parent.frame;
  f.origin.x := 0;
  f.origin.y := 0;
  hr := f;
  vr := f;
  hw := NSScroller.scrollerWidth;
  vw := NSScroller.scrollerWidth;
  vr.size.width:=vw;
  vr.origin.x:=f.size.width-vr.size.width;
  hr.size.height:=hw;

  if Assigned(hrz) and (not hrz.isHidden) then
  begin
    f.size.height := f.size.height - hw;
    f.origin.y := hw;

    vr.origin.y := hw;
    vr.size.height := vr.size.height - hw;
    if Assigned(vrt) and (not vrt.isHidden) then
      hr.size.width:=hr.size.width-vw;

    hrz.setFrame(hr);
  end;

  if Assigned(vrt) and (not vrt.isHidden) then
  begin
    f.size.width := f.size.width-vw;
    vrt.setFrame(vr);
  end;


  if not NSEqualRects(doc.frame, f) then
  begin
    doc.setFrame(f);
    doc.setNeedsDisplay_(true);
  end;
end;

procedure TCocoaManualScrollView.setHasVerticalScroller(doshow: Boolean);
var
  ch : Boolean;
begin
  ch := false;
  if doshow then
  begin
    if not Assigned(fvscroll) then
    begin
      fvscroll := allocVerticalScroller(true);
      ch := true;
    end;

    if fvscroll.isHidden then
    begin
      fvscroll.setHidden(false);
      ch := true;
    end;
  end
  else if Assigned(fvscroll) and not fvscroll.isHidden then
  begin
    fvscroll.setHidden(true);
    ch := true;
  end;
  if ch then
    updateDocSize(self, fdocumentView, fhscroll, fvscroll);
end;

procedure TCocoaManualScrollView.setHasHorizontalScroller(doshow: Boolean);
var
  r : NSRect;
  f : NSRect;
  ch : Boolean;
begin
  f:=frame;
  ch:=false;
  if doshow then
  begin
    if not Assigned(fhscroll) then
    begin
      fhscroll := allocHorizontalScroller(true);
      ch := true;
    end;
    if fhscroll.isHidden then
    begin
      fhscroll.setHidden(false);
      ch := true;
    end;
  end
  else if Assigned(fhscroll) and (not fhscroll.isHidden) then
  begin
    fhscroll.setHidden(true);
    ch := true;
  end;

  if ch then
    updateDocSize(self, fdocumentView, fhscroll, fvscroll);
end;

function TCocoaManualScrollView.hasVerticalScroller: Boolean;
begin
  Result:=Assigned(fvscroll) and (not fvscroll.isHidden);
end;

function TCocoaManualScrollView.hasHorizontalScroller: Boolean;
begin
  Result:=Assigned(fhscroll) and (not fhscroll.isHidden);
end;

function TCocoaManualScrollView.horizontalScroller: NSScroller;
begin
  Result:=fhscroll;
end;

function TCocoaManualScrollView.verticalScroller: NSScroller;
begin
  Result:=fvscroll;
end;

function TCocoaManualScrollView.allocHorizontalScroller(avisible: Boolean): NSScroller;
var
  r : NSRect;
  f : NSRect;
  w : CGFloat;
begin
  if Assigned(fhscroll) then
    Result := fhscroll
  else
  begin
    f := frame;
    w := NSScroller.scrollerWidth;
    r := NSMakeRect(0, 0, f.size.width, NSScroller.scrollerWidth);
    allocScroller( self, fhscroll, r, avisible);
    fhscroll.setAutoresizingMask(NSViewWidthSizable);
    Result := fhscroll;
  end;
end;

function TCocoaManualScrollView.allocVerticalScroller(avisible: Boolean): NSScroller;
var
  r : NSRect;
  f : NSRect;
  w : CGFloat;
begin
  if Assigned(fvscroll) then
    Result := fvscroll
  else
  begin
    f := frame;
    w := NSScroller.scrollerWidth;
    r := NSMakeRect(f.size.width-w, 0, w, f.size.height);
    allocScroller( self, fvscroll, r, avisible);
    fvscroll.setAutoresizingMask(NSViewHeightSizable or NSViewMinXMargin);
    Result := fvscroll;
  end;
end;

{ TCocoaFieldEditor }

function TCocoaFieldEditor.resignFirstResponder: Boolean;
begin
  //DebugLn('[TCocoaFieldEditor.resignFirstResponder]');
  if (lastEditBox <> nil) then
  begin
    if lastEditBox.isKindOfClass_(TCocoaTextField) then
    begin
      TCocoaTextField(lastEditBox).RealResignFirstResponder();
    end
    else if lastEditBox.isKindOfClass_(TCocoaSecureTextField) then
    begin
      TCocoaSecureTextField(lastEditBox).RealResignFirstResponder();
    end;
    lastEditBox := nil;
  end;
  Result := inherited resignFirstResponder;
end;

procedure TCocoaFieldEditor.keyDown(event: NSEvent);
var
  cb : ICommonCallback;
  res : Boolean;
const
  NSKeyCodeTab  = 48;
begin
  if Assigned(lastEditBox) then
  begin
    cb := lastEditBox.lclGetCallback;
    if Assigned(cb) then
    begin
      res := cb.KeyEvent(event);
      // LCL has already handled tab (by switching focus)
      // do not let Cocoa to switch the focus again!
      if event.keyCode = NSKeyCodeTab then Exit;
    end else
      res := false;
    if not res then inherited keyDown(event);
  end
  else
    inherited keyDown(event);
end;

procedure TCocoaFieldEditor.mouseDown(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseUpDownEvent(event) then
    begin
      inherited mouseDown(event);
      // NSTextView runs internal mouse-tracking loop in it's mouseDown implemenation.
      // Thus "inherited mouseDown" only returns after the mouse has been released.
      // why is TCocoaTextView not affected?
      if Assigned(lastEditBox) and Assigned(lastEditBox.lclGetCallback) then
        lastEditBox.lclGetCallback.MouseUpDownEvent(event, true);
    end;
  end else
    inherited mouseDown(event);
end;

procedure TCocoaFieldEditor.mouseUp(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseUpDownEvent(event) then
      inherited mouseUp(event);
  end else
    inherited mouseUp(event);
end;

procedure TCocoaFieldEditor.rightMouseDown(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseUpDownEvent(event) then
      inherited rightMouseDown(event);
  end else
    inherited rightMouseDown(event);
end;

procedure TCocoaFieldEditor.rightMouseUp(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseUpDownEvent(event) then
      inherited rightMouseUp(event);
  end else
    inherited rightMouseUp(event);
end;

procedure TCocoaFieldEditor.otherMouseDown(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseUpDownEvent(event) then
      inherited otherMouseDown(event);
  end else
    inherited otherMouseDown(event);
end;

procedure TCocoaFieldEditor.otherMouseUp(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseUpDownEvent(event) then
      inherited otherMouseUp(event);
  end else
    inherited otherMouseUp(event);
end;

procedure TCocoaFieldEditor.mouseDragged(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseMove(event) then
      inherited mouseDragged(event);
  end else
    inherited mouseDragged(event);
end;

procedure TCocoaFieldEditor.mouseMoved(event: NSEvent);
begin
  if Assigned(lastEditBox) then
  begin
    if Assigned(lastEditBox.lclGetCallback) and not lastEditBox.lclGetCallback.MouseMove(event) then
      inherited mouseMoved(event);
  end else
    inherited mouseMoved(event);
end;

{ TCocoaScrollView }

function TCocoaScrollView.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaScrollView.lclClientFrame: TRect;
begin
  NSToLCLRect(contentView.frame, frame.size.height, Result);
end;

function TCocoaScrollView.lclContentView: NSView;
begin
  Result:=documentView;
end;

procedure TCocoaScrollView.setDocumentView(aView: NSView);
begin
  inherited setDocumentView(aView);
  resetScrollRect;
end;

procedure TCocoaScrollView.scrollContentViewBoundsChanged(notify: NSNotification
  );
var
  nw    : NSRect;
  dx,dy : CGFloat;
begin
  if not assigned(documentView) then Exit;
  nw:=documentVisibleRect;

  dx:=nw.origin.x-docrect.origin.x;
  dy:=docrect.origin.y-nw.origin.y; // cocoa flipped coordinates

  docrect:=nw;
  if (dx=0) and (dy=0) then Exit;

  if holdscroll>0 then Exit;
  inc(holdscroll);
  try
    if (dx<>0) and assigned(callback) then
      callback.scroll(false, round(nw.origin.x));

    if (dy<>0) and assigned(callback) then
      callback.scroll(true, round(self.documentView.frame.size.height - self.documentVisibleRect.origin.y - self.documentVisibleRect.size.height));
  finally
    dec(holdscroll);
  end;
end;

procedure TCocoaScrollView.resetScrollRect;
begin
  docrect:=documentVisibleRect;
end;

function TCocoaScrollView.initWithFrame(ns: NSRect): id;
var
  sc : TCocoaScrollView;
begin
  Result:=inherited initWithFrame(ns);
  sc:=TCocoaScrollView(Result);

  //sc.contentView.setPostsBoundsChangedNotifications(true);
  NSNotificationCenter.defaultCenter
    .addObserver_selector_name_object(sc, ObjCSelector('scrollContentViewBoundsChanged:')
      ,NSViewBoundsDidChangeNotification
      ,sc.contentView);
end;

procedure TCocoaScrollView.dealloc;
begin
  NSNotificationCenter.defaultCenter
    .removeObserver(self);
  inherited dealloc;
end;

function TCocoaScrollView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaScrollView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaScrollView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaScrollView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollView.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaScrollView.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaScrollBar }

procedure TCocoaScrollBar.actionScrolling(sender: NSObject);
begin
  if Assigned(callback) then
  begin
    callback.scroll( not IsHorizontal(), lclPos);
  end;
end;

function TCocoaScrollBar.IsHorizontal: Boolean;
begin
  Result := frame.size.width > frame.size.height;
end;

function TCocoaScrollBar.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaScrollBar.lclPos: Integer;
begin
  Result:=round( floatValue * (maxint-minInt)) + minInt;
end;

procedure TCocoaScrollBar.lclSetPos(aPos: integer);
var
  d : integer;
begin
  d := maxInt - minInt;
  if d = 0 then
    setDoubleValue(0)
  else
  begin
    if aPos < minInt then aPos:=minInt
    else if aPos > maxInt then aPos:=maxInt;
    setDoubleValue( (aPos - minInt) / d );
  end;
end;

function TCocoaScrollBar.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaScrollBar.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaScrollBar.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaScrollBar.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollBar.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaScrollBar.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaGroupBox }

function TCocoaGroupBox.lclIsHandle: Boolean;
begin
  Result := True;
end;

function TCocoaGroupBox.lclClientFrame: TRect;
var
  v : NSView;
begin
  v:=contentView;
  if not Assigned(v) then
    Result := inherited lclClientFrame
  else
    Result := NSRectToRect( v.frame );
end;

function TCocoaGroupBox.lclContentView: NSView;
begin
  Result := NSView(contentView);
end;

function TCocoaGroupBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaGroupBox.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaGroupBox.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaGroupBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaGroupBox.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaGroupBox.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaTextField }

function TCocoaTextField.lclIsHandle: Boolean;
begin
  Result := True;
end;

procedure TCocoaTextField.dealloc;
var
  lFieldEditor: TCocoaFieldEditor;
begin
  lFieldEditor := GetFieldEditor();
  if (lFieldEditor <> nil) and (lFieldEditor.lastEditBox = Self) then
  begin
    lFieldEditor.lastEditBox := nil;
  end;

  inherited dealloc;
end;

function TCocoaTextField.GetFieldEditor: TCocoaFieldEditor;
var
  lFieldEditor: TCocoaFieldEditor;
  lText: NSText;
begin
  Result := nil;
  if window = nil then Exit;
  lText := window.fieldEditor_forObject(True, Self);
  if (lText <> nil) and lText.isKindOfClass_(TCocoaFieldEditor) then
  begin
    Result := TCocoaFieldEditor(lText);
  end;
end;

function TCocoaTextField.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaTextField.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaTextField.RealResignFirstResponder: Boolean;
begin
  callback.ResignFirstResponder;
  Result := True;
end;

// Do not propagate this event to the LCL,
// because Cocoa NSTextField loses focus as soon as it receives it
// and the shared editor gets focus instead.
// see NSWindow.fieldEditor:forObject:
// See http://www.cocoabuilder.com/archive/cocoa/103607-resignfirstresponder-called-immediately.html
// See http://stackoverflow.com/questions/3192905/nstextfield-not-noticing-lost-focus-when-pressing-tab
function TCocoaTextField.resignFirstResponder: Boolean;
var
  lFieldEditor: TCocoaFieldEditor;
begin
  //DebugLn('[TCocoaTextField.resignFirstResponder]');
  Result := inherited resignFirstResponder;
  lFieldEditor := GetFieldEditor();
  if (lFieldEditor <> nil) then
  begin
    lFieldEditor.lastEditBox := Self;
  end;
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

procedure TCocoaTextField.keyUp(event: NSEvent);
begin
  if Assigned(callback) then
  begin
    // NSTextField doesn't provide keyDown, so emulate it here
    //callback.KeyEvent(event, True);
    // keyUp now
    // by this time the control might have been released and callback cleared
    callback.KeyEvent(event);
  end;
  inherited keyUp(event);
end;

procedure TCocoaTextField.textDidChange(notification: NSNotification);
begin
  if callback <> nil then
    callback.SendOnTextChanged;
end;

procedure TCocoaTextField.mouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaTextField.mouseUp(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
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

function TCocoaTextView.lclIsHandle: Boolean;
begin
  Result := True;
end;

procedure TCocoaTextView.keyDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    // don't skip inherited or else key input won't work
    inherited keyDown(event);
end;

procedure TCocoaTextView.keyUp(event: NSEvent);
begin
  if Assigned(callback) then callback.KeyEvent(event);
  // don't skip inherited or else key input won't work
  inherited keyUp(event);
end;

procedure TCocoaTextView.flagsChanged(event: NSEvent);
begin
  if Assigned(callback) then callback.KeyEvent(event);
  // don't skip inherited or else key input won't work
  inherited flagsChanged(event);
end;

function TCocoaTextView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaTextView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaTextView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
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

procedure TCocoaTextView.mouseDown(event: NSEvent);
begin
  if Assigned(callback) then
  begin
    if not callback.MouseUpDownEvent(event) then
      inherited mouseDown(event);

    // Cocoa doesn't call mouseUp for NSTextView, so we have to emulate it here :(
    // See bug 29000
    if Assigned(callback) then
      callback.MouseUpDownEvent(event, True);
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
  if Result and CocoaWidgetSet.IsControlDisabledDueToModal(Self) then Result := False;
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

//

{ TCocoaSecureTextField }

function TCocoaSecureTextField.lclIsHandle: Boolean;
begin
  Result := True;
end;

procedure TCocoaSecureTextField.dealloc;
var
  lFieldEditor: TCocoaFieldEditor;
begin
  lFieldEditor := GetFieldEditor();
  if (lFieldEditor <> nil) and (lFieldEditor.lastEditBox = Self) then
  begin
    lFieldEditor.lastEditBox := nil;
  end;

  inherited dealloc;
end;

function TCocoaSecureTextField.GetFieldEditor: TCocoaFieldEditor;
var
  lFieldEditor: TCocoaFieldEditor;
  lText: NSText;
begin
  Result := nil;
  if window = nil then Exit;
  lText := window.fieldEditor_forObject(True, Self);
  if (lText <> nil) and lText.isKindOfClass_(TCocoaFieldEditor) then
  begin
    Result := TCocoaFieldEditor(lText);
  end;
end;

function TCocoaSecureTextField.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaSecureTextField.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaSecureTextField.RealResignFirstResponder: Boolean;
begin
  callback.ResignFirstResponder;
  Result := True;
end;

function TCocoaSecureTextField.resignFirstResponder: Boolean;
var
  lFieldEditor: TCocoaFieldEditor;
begin
  //DebugLn('[TCocoaTextField.resignFirstResponder]');
  Result := inherited resignFirstResponder;
  lFieldEditor := GetFieldEditor();
  if (lFieldEditor <> nil) then
  begin
    lFieldEditor.lastEditBox := Self;
  end;
end;

procedure TCocoaSecureTextField.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaSecureTextField.keyUp(event: NSEvent);
begin
  if Assigned(callback) then
  begin
    // NSTextField doesn't provide keyDown, so emulate it here
    //callback.KeyEvent(event, True);
    // keyUp now
    callback.KeyEvent(event);
  end;
  inherited keyUp(event);
end;

procedure TCocoaSecureTextField.mouseDown(event: NSEvent);
begin
  if Assigned(callback) and not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
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

{ TCocoaCustomControl }

function TCocoaCustomControl.lclIsHandle: Boolean;
begin
  Result := True;
end;

procedure TCocoaCustomControl.setStringValue(avalue: NSString);
begin
  if Assigned(fstr) then fstr.release;
  if ASsigned(avalue) then
    fstr:=avalue.copyWithZone(nil)
  else
    fstr:=nil;
  inherited setStringValue(avalue);
end;

function TCocoaCustomControl.stringValue: NSString;
begin
  Result:=fstr;
end;

procedure TCocoaCustomControl.dealloc;
begin
  if Assigned(fstr) then fstr.release;
  inherited dealloc;
end;

function TCocoaCustomControl.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaCustomControl.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaCustomControl.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaCustomControl.acceptsFirstMouse(event: NSEvent): Boolean;
begin
  // By default, a mouse-down event in a window that isn’t the key window
  // simply brings the window forward and makes it key; the event isn’t sent
  // to the NSView object over which the mouse click occurs. The NSView can
  // claim an initial mouse-down event, however, by overriding acceptsFirstMouse: to return YES.
  // see bug #33034
  Result:=true;
end;

procedure TCocoaCustomControl.drawRect(dirtyRect: NSRect);
begin
  if isdrawing=0 then faileddraw:=false;
  inc(isdrawing);
  inherited drawRect(dirtyRect);

  // Implement Color property
  if Assigned(callback) then
    callback.DrawBackground(NSGraphicsContext.currentContext, bounds, dirtyRect);

  if CheckMainThread and Assigned(callback) then
    callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
  dec(isdrawing);

  if (isdrawing=0) and (faileddraw) then
  begin
    // Similar to Carbon. Cocoa doesn't welcome changing a framerects during paint event
    // If such thing happens, the results are pretty much inpredicatable. #32970
    // TreeView tries to updatedScrollBars during paint event. That sometimes is causing
    // the frame to be changed (i.e. scroll bar showed or hidden, resized the client rect)
    // as a result, the final image is shown up-side-down.
    //
    // Below is an attempt to prevent graphical artifacts and to redraw
    // the control again.
    inherited drawRect(dirtyRect);

    if Assigned(callback) then
      callback.DrawBackground(NSGraphicsContext.currentContext, bounds, dirtyRect);

    if CheckMainThread and Assigned(callback) then
      callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
  end;
end;

function TCocoaCustomControl.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaCustomControl.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaCustomControl.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaCustomControl.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaCustomControl.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaCustomControl.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaCustomControl.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaCustomControl.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaCustomControl.keyDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyDown(event);
end;

procedure TCocoaCustomControl.keyUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyUp(event);
end;

procedure TCocoaCustomControl.flagsChanged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited flagsChanged(event);
end;

procedure TCocoaCustomControl.setFrame(aframe: NSRect);
begin
  if NSEqualRects(aframe, frame) then Exit;
  if isdrawing>0 then
    faileddraw := true;

  inherited setFrame(aframe);
  // it actually should come from a notifcation
  if Assigned(callback) then callback.frameDidChange(self);
end;

procedure TCocoaCustomControl.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaCustomControl.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaCustomControl.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaCustomControl.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaCustomControl.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaCustomControl.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaCustomControl.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaCustomControl.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ LCLObjectExtension }

function LCLObjectExtension.lclIsEnabled: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetEnabled(AEnabled: Boolean);
begin
end;

function LCLObjectExtension.lclIsVisible: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetVisible(AVisible: Boolean);
begin
end;

function LCLObjectExtension.lclWindowState: Integer;
begin
  Result := SIZE_RESTORED;
end;

procedure LCLObjectExtension.lclInvalidateRect(const r: TRect);
begin
end;

procedure LCLObjectExtension.lclInvalidate;
begin
end;

procedure LCLObjectExtension.lclUpdate;
begin
end;

procedure LCLObjectExtension.lclRelativePos(var Left,Top: Integer);
begin
end;

procedure LCLObjectExtension.lclLocalToScreen(var X,Y: Integer);
begin
end;

procedure LCLObjectExtension.lclScreenToLocal(var X, Y: Integer);
begin
end;

function LCLObjectExtension.lclParent:id;
begin
  Result:=nil;
end;

function LCLObjectExtension.lclFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

procedure LCLObjectExtension.lclSetFrame(const r:TRect);
begin

end;

function LCLObjectExtension.lclClientFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

function LCLObjectExtension.lclGetCallback: ICommonCallback;
begin
  Result := nil;
end;

procedure LCLObjectExtension.lclClearCallback;
begin
end;

function LCLObjectExtension.lclGetPropStorage: TStringList;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetPropStorage
  else
    Result := nil;
end;

function LCLObjectExtension.lclGetTarget: TObject;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetTarget
  else
    Result := nil;
end;

function LCLObjectExtension.lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.DeliverMessage(Msg, WParam, LParam)
  else
    Result := 0;
end;

function LCLObjectExtension.lclIsHandle: Boolean;
begin
result:=false;
end;

function LCLObjectExtension.lclContentView: NSView;
begin
  Result := nil;
end;

procedure LCLObjectExtension.lclOffsetMousePos(var Point: NSPoint);
begin

end;

{ LCLControlExtension }

function RectToViewCoord(view: NSView; const r: TRect): NSRect;
var
  b: NSRect;
begin
  b := view.bounds;
  Result.origin.x := r.Left;
  Result.size.width := r.Right - r.Left;
  Result.size.height := r.Bottom - r.Top;
  if Assigned(view) and (view.isFlipped) then
    Result.origin.y := r.Top
  else
    Result.origin.y := b.size.height - r.Bottom;
end;

function LCLControlExtension.lclIsEnabled:Boolean;
begin
  Result := IsEnabled;
  if Result and CocoaWidgetSet.IsControlDisabledDueToModal(Self) then Result := False;
end;

procedure LCLControlExtension.lclSetEnabled(AEnabled:Boolean);
begin
  SetEnabled(AEnabled and ((not Assigned(superview)) or (superview.lclisEnabled)) );
  inherited lclSetEnabled(AEnabled);
end;

function LCLViewExtension.lclInitWithCreateParams(const AParams: TCreateParams): id;
var
  p: NSView;
  ns: NSRect;
  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  pstr: string;
  {$ENDIF}
begin
  p := nil;
  if (AParams.WndParent <> 0) then
    p := CocoaUtils.GetNSObjectView(NSObject(AParams.WndParent));

  if Assigned(p) then
    LCLToNSRect(Types.Bounds(AParams.X, AParams.Y, AParams.Width, AParams.Height),
      p.frame.size.height, ns)
  else
    ns := GetNSRect(AParams.X, AParams.Y, AParams.Width, AParams.Height);

  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  if Assigned(p) then
  begin
    pstr := NSStringToString(p.className);
    if NSStringToString(NSObject(AParams.WndParent).className) = 'TCocoaTabPage' then
      pstr := pstr + ' ' + NSStringToString(TCocoaTabPage(AParams.WndParent).label_);
  end
  else
    pstr := '';
  WriteLn(Format('[LCLViewExtension.lclInitWithCreateParams] Class=%s Caption=%s ParentClass=%s ParentClassView=%s rect=%d %d %d %d Visible=%d',
    [NSStringToString(Self.className), AParams.Caption,
     NSStringToString(NSObject(AParams.WndParent).className), pstr,
     Round(ns.Origin.x), Round(ns.Origin.y), Round(ns.size.width), Round(ns.size.height),
     AParams.Style and WS_VISIBLE]));
  {$ENDIF}

  Result := initWithFrame(ns);
  if not Assigned(Result) then
    Exit;

  setHidden(AParams.Style and WS_VISIBLE = 0);

  if Assigned(p) then
    p.lclContentView.addSubview(Result);
  SetViewDefaults(Result);
end;

function LCLViewExtension.lclIsEnabled: Boolean;
begin
  Result := true;
end;

procedure LCLViewExtension.lclSetEnabled(AEnabled: Boolean);
var
  cb : ICommonCallback;
  obj : NSObject;
begin
  for obj in subviews do begin
    cb := obj.lclGetCallback;
    obj.lclSetEnabled(AEnabled and ((not Assigned(cb)) or cb.GetShouldBeEnabled) );
  end;
end;

function LCLViewExtension.lclIsVisible: Boolean;
begin
  Result := not isHidden;
end;

procedure LCLViewExtension.lclSetVisible(AVisible: Boolean);
begin
  setHidden(not AVisible);
  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  WriteLn(Format('LCLViewExtension.lclSetVisible: %s AVisible=%d',
    [NSStringToString(Self.ClassName), Integer(AVisible)]));
  {$ENDIF}
end;

function LCLViewExtension.lclIsPainting: Boolean;
begin
  Result := Assigned(lclGetCallback) and Assigned(lclGetCallback.GetContext);
end;

procedure LCLViewExtension.lclInvalidateRect(const r:TRect);
var
  view : NSView;
begin
  view:=lclContentView;
  if Assigned(view) then
    view.setNeedsDisplayInRect(RectToViewCoord(view, r))
  else
    self.setNeedsDisplayInRect(RectToViewCoord(Self, r));
  //todo: it might be necessary to always invalidate self
  //      just need to get offset of the contentView relative for self
end;

procedure LCLViewExtension.lclInvalidate;
begin
  setNeedsDisplay_(True);
end;

procedure LCLViewExtension.lclUpdate;
begin
  display;
end;

procedure LCLViewExtension.lclRelativePos(var Left, Top: Integer);
begin
  Left := Round(frame.origin.x);
  Top := Round(frame.origin.y);
end;

procedure LCLViewExtension.lclLocalToScreen(var X, Y:Integer);
var
  P: NSPoint;

begin
  // 1. convert to window base
  P.x := X;
  if isFlipped then
    p.y := Y
  else
    P.y := frame.size.height-y;   // convert to Cocoa system

  P := convertPoint_ToView(P, nil);

  X := Round(P.X);
  Y := Round(window.frame.size.height-P.Y); // convert to LCL system

  // 2. convert window to screen
  window.lclLocalToScreen(X, Y);
end;

procedure LCLViewExtension.lclScreenToLocal(var X, Y: Integer);
var
  P: NSPoint;
begin
  // 1. convert from screen to window

  window.lclScreenToLocal(X, Y);
  P.x := X;
  P.y := Round(window.frame.size.height-Y); // convert to Cocoa system

  // 2. convert from window to local
  P := convertPoint_FromView(P, nil);
  X := Round(P.x);
  Y := Round(frame.size.height-P.y);   // convert to Cocoa system
end;

function LCLViewExtension.lclParent:id;
begin
  Result := superView;
end;

function LCLViewExtension.lclFrame: TRect;
var
  v: NSView;
begin
  v := superview;
  if Assigned(v) then
    NSToLCLRect(frame, v.frame.size.height, Result)
  else
    Result := NSRectToRect(frame);
end;

procedure LCLViewExtension.lclSetFrame(const r: TRect);
var
  ns: NSRect;
  svHeight: CGFloat;
begin
  svHeight := GetNSViewSuperViewHeight(Self);
  if Assigned(superview) and not superview.isFlipped then
  begin
    LCLToNSRect(r, svHeight, ns)
  end
  else
    ns := RectToNSRect(r);
  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  WriteLn(Format('LCLViewExtension.lclSetFrame: %s Bounds=%s height=%d ns_pos=%d %d ns_size=%d %d',
    [NSStringToString(Self.ClassName), dbgs(r), Round(svHeight),
     Round(ns.origin.x), Round(ns.origin.y), Round(ns.size.width), Round(ns.size.height)]));
  {$ENDIF}
  setFrame(ns);
end;

function LCLViewExtension.lclClientFrame: TRect;
var
  r: NSRect;
begin
  r := bounds;
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Round(r.size.width);
  Result.Bottom := Round(r.size.height);
end;

function LCLViewExtension.lclContentView: NSView;
begin
  Result := self;
end;

procedure LCLViewExtension.lclOffsetMousePos(var Point: NSPoint);
var
  es : NSScrollView;
  r  : NSRect;
begin
  Point := convertPoint_fromView(Point, nil);
  es := enclosingScrollView;
  if not isFlipped then
    Point.y := bounds.size.height - Point.y;

  if Assigned(es) then
  begin
    r := es.documentVisibleRect;
    if isFlipped then
      Point.y := Point.y - r.origin.y
    else
      Point.y := Point.y - (es.documentView.frame.size.height - r.size.height - r.origin.y);
    Point.X := Point.X - r.origin.x;
  end;
end;

{ TCocoaStatusBar }

procedure TCocoaStatusBar.drawRect(dirtyRect: NSRect);
var
  R    : TRect;
  i    : Integer;
  txt  : NSString;
  nr   : NSRect;
  x    : Integer;
const
  CocoaAlign: array [TAlignment] of Integer = (NSNaturalTextAlignment, NSRightTextAlignment, NSCenterTextAlignment);
begin
  //inherited NSControl.drawRect(dirtyRect);
  if callback = nil then Exit;

  if not Assigned(panelCell) then Exit;

  panelCell.setControlView(Self);
  FillChar(nr, sizeof(nr), 0);

  r := lclClientFrame();
  nr.size.height := StatusBar.Height;

  if StatusBar.SimplePanel then
  begin
    nr.size.width := r.Right-r.Left;
    txt := NSStringUtf8(StatusBar.SimpleText);
    panelCell.setAlignment( NSNaturalTextAlignment );
    panelCell.setTitle( txt );
    panelCell.drawWithFrame_inView(nr, Self);
    txt.release;
  end
  else
  begin
    x:=0;
    for i:=0 to StatusBar.Panels.Count-1 do
    begin
      if i=StatusBar.Panels.Count-1 then
        nr.size.width := r.Right-x+1
      else
        nr.size.width := StatusBar.Panels[i].Width+1;
      nr.origin.x := x;
      inc(x, StatusBar.Panels[i].Width);
      txt := NSStringUtf8(StatusBar.Panels[i].Text);
      panelCell.setTitle(txt);
      panelCell.setAlignment(CocoaAlign[StatusBar.Panels[i].Alignment]);
      panelCell.drawWithFrame_inView(nr, Self);
      txt.release;
    end;
  end;
end;

procedure TCocoaStatusBar.dealloc;
begin
  if Assigned(panelCell) then panelCell.release;
  inherited;
end;

{ TCocoaComboBoxList }

procedure TCocoaComboBoxList.Changed;
var
  i: Integer;
  nsstr: NSString;
  lItems: array of NSMenuItem;
begin
  if FOwner <> nil then
    fOwner.reloadData;
  if FReadOnlyOwner <> nil then
  begin
    // store the current item
    FReadOnlyOwner.lastSelectedItemIndex := FReadOnlyOwner.indexOfSelectedItem;

    FReadOnlyOwner.removeAllItems();
    // Adding an item with its final name will cause it to be deleted,
    // so we need to first add all items with unique names, and then
    // rename all of them, see bug 30847
    SetLength(lItems, Count);
    for i := 0 to Count-1 do
    begin
      nsstr := NSStringUtf8(Format('unique_item_%d', [i]));
      FReadOnlyOwner.addItemWithTitle(nsstr);
      lItems[i] := FReadOnlyOwner.lastItem;
      nsstr.release;
    end;
    for i := 0 to Count-1 do
    begin
      nsstr := NSStringUtf8(Strings[i]);
      lItems[i].setTitle(nsstr);
      nsstr.release;
    end;
    SetLength(lItems, 0);

    // reset the selected item
    FReadOnlyOwner.selectItemAtIndex(FReadOnlyOwner.lastSelectedItemIndex);
  end;
  inherited Changed;
end;

procedure TCocoaComboBoxList.Changing;
begin
  FPreChangeListCount := Count;
end;

constructor TCocoaComboBoxList.Create(AOwner: TCocoaComboBox; AReadOnlyOwner: TCocoaReadOnlyComboBox);
begin
  FOwner := AOwner;
  FReadOnlyOwner := AReadOnlyOwner;
end;

{ TCocoaComboBox }

function TCocoaComboBox.lclIsHandle: Boolean;
begin
  Result:=true;
end;

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

  if ch and Assigned(callback) then
    callback.SendOnChange;
end;

function TCocoaComboBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaComboBox.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaComboBox.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

procedure TCocoaComboBox.textDidChange(notification: NSNotification);
var
  TheEvent: NSEvent;
begin
  inherited textDidChange(notification);
  TheEvent := nsapp.currentevent;
  if assigned(callback) and (TheEvent.type_ = NSKeyDown) then
    callback.KeyEvent(TheEvent)
end;

procedure TCocoaComboBox.textDidEndEditing(notification: NSNotification);
var
  TheEvent: NSEvent;
begin
  inherited textDidEndEditing(notification);
  TheEvent := nsapp.currentevent;
  if assigned(callback) and (TheEvent.type_ = NSKeyDown) then
    callback.KeyEvent(TheEvent)
end;

function TCocoaComboBox.comboBox_objectValueForItemAtIndex_(combo:TCocoaComboBox;
  row: NSInteger):id;
begin
  if not Assigned(list) or (row<0) or (row>=list.Count)
    then Result:=nil
    else Result:=NSStringUtf8(list[row]);
end;

function TCocoaComboBox.numberOfItemsInComboBox(combo:TCocoaComboBox):NSInteger;
begin
  if not Assigned(list) then Result:=0
  else Result:=list.Count;
end;

procedure TCocoaComboBox.dealloc;
begin
  if Assigned(list) then
  begin
    list.Free;
    list:=nil;
  end;
  resultNS.release;
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
end;

procedure TCocoaComboBox.comboBoxWillDismiss(notification: NSNotification);
begin
  callback.ComboBoxWillDismiss;
end;

procedure TCocoaComboBox.comboBoxSelectionDidChange(notification: NSNotification);
var
  txt : NSString;
begin
  txt := comboBox_objectValueForItemAtIndex_(self, indexOfSelectedItem);
  if Assigned(txt) then setStringValue( txt );
  callback.ComboBoxSelectionDidChange;
end;

procedure TCocoaComboBox.comboBoxSelectionIsChanging(notification: NSNotification);
begin
  callback.ComboBoxSelectionIsChanging;
end;

{ TCocoaReadOnlyComboBox }

function TCocoaReadOnlyComboBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaReadOnlyComboBox.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaReadOnlyComboBox.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

procedure TCocoaReadOnlyComboBox.dealloc;
begin
  if Assigned(list) then
  begin
    list.Free;
    list:=nil;
  end;
  if resultNS <> nil then
    resultNS.release;
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

procedure TCocoaReadOnlyComboBox.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

function TCocoaReadOnlyComboBox.lclIsHandle: Boolean;
begin
  Result:=true;
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

{ TCocoaProgressIndicator }

function TCocoaProgressIndicator.acceptsFirstResponder: Boolean;
begin
  Result:=True;
end;

function TCocoaProgressIndicator.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaProgressIndicator.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaProgressIndicator.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaProgressIndicator.lclClearCallback;
begin
  callback:=nil;
end;

procedure TCocoaProgressIndicator.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaSlider }

function TCocoaSlider.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaSlider.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaSlider.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaSlider.lclGetCallback: ICommonCallback;
begin
  Result:=callback;
end;

procedure TCocoaSlider.lclClearCallback;
begin
  callback := nil;
end;

procedure TCocoaSlider.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaSlider.keyDown(event: NSEvent);
var
  KeyCode: word;
begin
  KeyCode := Event.keyCode;
  case KeyCode of
    MK_UP       : SnapToInteger(1);
    MK_DOWN     : SnapToInteger(-1);
    MK_LEFT     : SnapToInteger(-1);
    MK_RIGHT    : SnapToInteger(1);
  else
    // If this isn't done callback.KeyEvent will cause arrow left/right to change control
    if Assigned(callback) then callback.KeyEvent(event)
    else inherited keyDown(event);
  end;
end;

procedure TCocoaSlider.keyUp(event: NSEvent);
var
  KeyCode: word;
begin
  KeyCode := Event.keyCode;
  case KeyCode of
    MK_UP, MK_DOWN, MK_LEFT, MK_RIGHT: inherited keyUp(event);
  else
    // If this isn't done callback.KeyEvent will cause arrow left/right to change control
    if Assigned(callback) then callback.KeyEvent(event)
    else inherited keyUp(event);
  end;
end;

procedure TCocoaSlider.SnapToInteger(AExtraFactor: Integer);
begin
  setIntValue(Round(doubleValue() + AExtraFactor));
end;

procedure TCocoaSlider.sliderAction(sender: id);
begin
  SnapToInteger();
  // OnChange event
  if callback <> nil then
    callback.SendOnChange();
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

function TCocoaSpinEdit.becomeFirstResponder: Boolean;
begin
  Result := Edit.becomeFirstResponder;
  if Assigned(callback) then
    callback.BecomeFirstResponder;
end;

function TCocoaSpinEdit.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  if Assigned(callback) then
    callback.ResignFirstResponder;
end;

function TCocoaSpinEdit.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaSpinEdit.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaSpinEdit.lclIsHandle: Boolean;
begin
  Result := True;
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

function TCocoaTextField.lclIsHandle: Boolean;
begin
  Result := True;
end;

{$ELSE}

procedure TCocoaSpinEdit.dealloc;
var
  lFieldEditor: TCocoaFieldEditor;
begin
  lFieldEditor := GetFieldEditor();
  if (lFieldEditor <> nil) and (lFieldEditor.lastEditBox = Self) then
  begin
    lFieldEditor.lastEditBox := nil;
  end;

  if Stepper <> nil then
    Stepper.release;
  if NumberFormatter <> nil then
    NumberFormatter.release;

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
begin
  decimalPlaces := ADecimalPlaces;
  Stepper.setMinValue(min);
  Stepper.setMaxValue(max);
  Stepper.setIncrement(inc);
  Stepper.setDoubleValue(avalue);

  // update the UI too
  StepperChanged(Self);
end;

procedure TCocoaSpinEdit.CreateSubcontrols(const AParams: TCreateParams);
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
  Stepper := NSStepper.alloc.lclInitWithCreateParams(lParams);
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

procedure TCocoaSpinEdit.PositionSubcontrols(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  lclSetFrame(Types.Bounds(ALeft, ATop, AWidth, AHeight));
end;

procedure TCocoaSpinEdit.StepperChanged(sender: NSObject);
var
  lNSStr: NSString;
  lStr: string;
begin
  lStr := Format('%.*f', [DecimalPlaces, Stepper.doubleValue()]);
  lNSStr := CocoaUtils.NSStringUtf8(lStr);
  setStringValue(lNSStr);
  lNSStr.release;
  // This implements OnChange for both user and code changes
  if callback <> nil then callback.SendOnTextChanged();
end;

function TCocoaSpinEdit.GetFieldEditor: TCocoaFieldEditor;
var
  lFieldEditor: TCocoaFieldEditor;
  lText: NSText;
begin
  Result := nil;
  if window = nil then Exit;
  lText := window.fieldEditor_forObject(True, Self);
  if (lText <> nil) and lText.isKindOfClass_(TCocoaFieldEditor) then
  begin
    Result := TCocoaFieldEditor(lText);
  end;
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

function TCocoaSpinEdit.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaSpinEdit.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaSpinEdit.RealResignFirstResponder: Boolean;
begin
  callback.ResignFirstResponder;
  Result := True;
end;

// See TCocoaTextField.resignFirstResponder as to why this is done here
function TCocoaSpinEdit.resignFirstResponder: Boolean;
var
  lFieldEditor: TCocoaFieldEditor;
begin
  //DebugLn('[TCocoaTextField.resignFirstResponder]');
  Result := inherited resignFirstResponder;
  lFieldEditor := GetFieldEditor();
  if (lFieldEditor <> nil) then
  begin
    lFieldEditor.lastEditBox := Self;
  end;
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

function TCocoaSpinEdit.lclIsHandle: Boolean;
begin
  Result := True;
end;

procedure TCocoaSpinEdit.lclSetVisible(AVisible: Boolean);
begin
  inherited lclSetVisible(AVisible);
  Stepper.setHidden(not AVisible);
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

{$ENDIF}

procedure SetNSControlSize(ctrl: NSControl; newHeight, miniHeight, smallHeight: Integer; AutoChangeFont: Boolean);
var
  sz : NSControlSize;
begin
  if (miniHeight>0) and (newHeight<=miniHeight) then
    sz:=NSMiniControlSize
  else if (smallHeight>0) and (newHeight<=smallHeight) then
    sz:=NSSmallControlSize
  else
    sz:=NSRegularControlSize;

  //todo: "cell" property (function) has been deprecated since 10.10
  //      instead NSControl itself has controlSize method
  if NSCell(ctrl.cell).controlSize<>sz then
  begin
    NSCell(ctrl.cell).setControlSize(sz);
    if AutoChangeFont then
      ctrl.setFont(NSFont.systemFontOfSize(NSFont.systemFontSizeForControlSize(sz)));
  end;
end;


end.

