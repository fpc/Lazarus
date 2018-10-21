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
{$include cocoadefines.inc}

{.$DEFINE COCOA_DEBUG_SETBOUNDS}
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
  LCLType;

const
  SPINEDIT_DEFAULT_STEPPER_WIDTH = 15;
  SPINEDIT_EDIT_SPACING_FOR_SELECTION = 4;
  STATUSBAR_DEFAULT_HEIGHT = 18;

type

  { ICommonCallback }

  ICommonCallback = interface
    // mouse events
    function MouseUpDownEvent(Event: NSEvent; AForceAsMouseUp: Boolean = False; AOverrideBlock: Boolean = False): Boolean;
    procedure MouseClick;
    function MouseMove(Event: NSEvent): Boolean;
    function KeyEvent(Event: NSEvent; AForceAsKeyDown: Boolean = False): Boolean;

    // KeyEvXXX methods were introduced to allow a better control
    // over when Cocoa keys processing is being called.
    // (The initial KeyEvent() replicates Carbon implementation, and it's not
    // suitable for Cocoa, due to the use of OOP and the extual "inherited Key..."needs to be called
    // where for Carbon there's a special fucntion to call the "next event handler" present)
    //
    // The desired use is as following:
    // Call KeyEvPrepare and pass NSEvent object
    // after that call KeyEvBefore and pass a flag if AllowCocoaHandle
    //
    // The call would populate the flag. If it's "True" you should call "inherited" method (to let Cocoa handle the key).
    // If the flag returned "False", you should not call inherited.
    //
    // No matter what the flag value was you should call KeyEvAfter.
    procedure KeyEvPrepare(Event: NSEvent; AForceAsKeyDown: Boolean = False);
    procedure KeyEvBefore(out AllowCocoaHandle: boolean);
    procedure KeyEvAfter;
    procedure SetTabSuppress(ASuppress: Boolean);

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

    // returns rectangle describing deltas to get "Layout" rectangle from "Frame" rectangle
    //   left, top  - return offsets from top-left corner of the control (not reversed as in Cocoa coordinates)
    //                    (values are typically positive)
    //   right, bottom -  offsets for bottom-right corner
    //                    (typically negative)
    function lclGetFrameToLayoutDelta: TRect; message 'lclGetFrameToLayoutDelta';

    function lclClientFrame: TRect; message 'lclClientFrame';
    function lclGetCallback: ICommonCallback; message 'lclGetCallback';
    procedure lclClearCallback; message 'lclClearCallback';
    function lclGetPropStorage: TStringList; message 'lclGetPropStorage';
    function lclGetTarget: TObject; message 'lclGetTarget';
    function lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; message 'lclDeliverMessage:::';
    function lclContentView: NSView; message 'lclContentView';
    procedure lclOffsetMousePos(var Point: NSPoint); message 'lclOffsetMousePos:';
    procedure lclExpectedKeys(var wantTabs, wantArrows, wantReturn, wantAll: Boolean); message 'lclExpectedKeys::::';
    function lclIsMouseInAuxArea(Event: NSEvent): Boolean; message 'lclMouseInAuxArea:';
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

  { LCLControlExtension }

  LCLControlExtension = objccategory(NSControl)
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;
  end;

  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
  private
    fstr : NSString;

    isdrawing   : integer;
    faileddraw  : Boolean;
  public
    callback: ICommonCallback;
    auxMouseByParent: Boolean;
    procedure dealloc; override;
    function acceptsFirstResponder: Boolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclIsMouseInAuxArea(Event: NSevent): Boolean; override;
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
    procedure keyUp(event: NSEvent); override;
    procedure flagsChanged(event: NSEvent); override;
    // nsview
    procedure setFrame(aframe: NSRect); override;
    // other
    procedure resetCursorRects; override;
    // value
    procedure setStringValue(avalue: NSString); override;
    function stringValue: NSString; override;
    procedure addSubView(aview: NSView); override;
  end;

  TStatusItemData = record
    Text  : NSString;
    Width : Integer;
    Align : TAlignment;
  end;

  TStatusItemDataArray = array of TStatusItemData;

  { TCocoaStatusBar }

  IStatusBarCallback = interface {(ICommonCallback) // not needed to inherit from ICommonCallback}
    function GetBarsCount: Integer;
    //todo: consider the use Cocoa native types, instead of FPC TAlignment
    function GetBarItem(idx: Integer; var txt: String;
      var width: Integer; var align: TAlignment): Boolean;
    procedure DrawPanel(idx: Integer; const r: TRect);
  end;

  TCocoaStatusBar = objcclass(TCocoaCustomControl)
  public
    //StatusBar : TStatusBar;
    barcallback : IStatusBarCallback;
    panelCell   : NSCell;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
  end;

  { TCocoaGroupBox }

  TCocoaGroupBox = objcclass(NSBox)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclClientFrame: TRect; override;
    function lclContentView: NSView; override;
    function lclGetFrameToLayoutDelta: TRect; override;
  end;


const
  PROGRESS_REG_HEIGHT   = 16; // no longer applies on later macOS version
  PROGRESS_SMALL_HEIGHT = 10;

type
  { TCocoaProgressIndicator }

  TCocoaProgressIndicator = objcclass(NSProgressIndicator)
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    function lclGetFrameToLayoutDelta: TRect; override;
    procedure lclSetFrame(const r: TRect); override;
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
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
  end;

  { TManualTicks }

  TManualTicks = class(TObject)
    count : integer;
    //todo: keep sorted and do binary search
    ticks : array of Integer;
    draw  : Boolean;
    function AddTick(atick: integer): Boolean;
  end;

  { TCocoaSlider }

  TCocoaSlider = objcclass(NSSlider)
    callback  : ICommonCallback;
    intval    : Integer;
    man       : TManualTicks;

    procedure dealloc; override;
    procedure drawRect(dirtyRect: NSRect); override;

    function acceptsFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    procedure resetCursorRects; override;
    //
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    //
    procedure SnapToInteger(AExtraFactor: Integer = 0); message 'SnapToInteger:';
    procedure sliderAction(sender: id); message 'sliderAction:';
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
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;

    procedure lclAddManTick(atick : integer); message 'lclAddManTick:';
    procedure lclSetManTickDraw(adraw: Boolean); message 'lclSetManTickDraw:';
    procedure lclExpectedKeys(var wantTabs, wantArrows, wantReturn, wantAll: Boolean); override;
  end;

  TCocoaSliderCell = objcclass(NSSliderCell)
  end;

procedure SetViewDefaults(AView: NSView);
function CheckMainThread: Boolean;
function GetNSViewSuperViewHeight(view: NSView): CGFloat;

procedure SetNSControlSize(ctrl: NSView; newHeight, miniHeight, smallHeight: Integer; AutoChangeFont: Boolean);

// these constants are missing from CocoaAll for some reason
const
  NSTextAlignmentLeft      = 0;
  NSTextAlignmentRight     = 1; // it's 2 for iOS and family
  NSTextAlignmentCenter    = 2; // it's 1 for iOS and family
  NSTextAlignmentJustified = 3;
  NSTextAlignmentNatural   = 4;

var
  // todo: this should be a threadvar
  TrackedControl : NSObject = nil;

function isCallbackForSameObject(cb1, cb2: ICommonCallback): Boolean;

implementation

function isCallbackForSameObject(cb1, cb2: ICommonCallback): Boolean;
begin
  Result := Assigned(cb1) and Assigned(cb2);
  if Result then
    Result := (cb1 = cb2) or (cb1.GetTarget = cb2.GetTarget);
end;

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

{ TManualTicks }

function TManualTicks.AddTick(atick: integer): Boolean;
var
  i : integer;
begin
  //todo: must be a binary search
  for i:=0 to length(ticks)-1 do
    if ticks[i]=atick then begin
      Result:=false;
      Exit;
    end;

  // adding new tick
  if length(ticks)=count then begin
    if count=0 then SetLength(ticks, 8)
    else SetLength(ticks, count * 2);
  end;
  ticks[count]:=atick;
  inc(count);
  Result := true;
end;

{ TCocoaGroupBox }

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

function TCocoaGroupBox.lclGetFrameToLayoutDelta: TRect;
begin
  Result.Left := 3;
  Result.Right := -3;
  Result.Top := 0;
  Result.Bottom := -4;
end;

function TCocoaGroupBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
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

{ TCocoaCustomControl }

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

procedure TCocoaCustomControl.addSubView(aview: NSView);
begin
  inherited addSubView(aview);

  if Assigned(aview) then
  begin
    // forcing LCL compatible "auto-move" mode. Sticking to left/top corner
    if not autoresizesSubviews then setAutoresizesSubviews(true);
    aview.setAutoresizingMask(NSViewMaxXMargin or NSViewMinYMargin);
  end;
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

function TCocoaCustomControl.lclIsMouseInAuxArea(Event: NSevent): Boolean;
begin
  if auxMouseByParent and Assigned(superview) then
    Result := superview.lclIsMouseInAuxArea(Event)
  else
    Result := false;
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

function LCLObjectExtension.lclGetFrameToLayoutDelta: TRect;
begin
  Result.Top := 0;
  Result.Left := 0;
  Result.Right := 0;
  Result.Bottom := 0;
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

function LCLObjectExtension.lclContentView: NSView;
begin
  Result := nil;
end;

procedure LCLObjectExtension.lclOffsetMousePos(var Point: NSPoint);
begin

end;

procedure LCLObjectExtension.lclExpectedKeys(var wantTabs, wantArrows,
  wantReturn, wantAll: Boolean);
begin
  wantTabs := false;
  wantArrows := false;
  wantReturn := false;
  wantAll := false;
end;

{ The method should return TRUE, if mouse is located above an auxilary area
  of a (composited) control, and thus MOUSE MOVE event should not be propagated
  to LCL. For example, controls with Scrollbars should not report mouse events
  if mouse cursor is above ScrollBar and scroll bar is visible. (ScrollBar = Auxillary area)

  By default, the whole area is considered to be non-auxillary and must be
  reported to LCL.
  }
function LCLObjectExtension.lclIsMouseInAuxArea(Event: NSEvent): Boolean;
begin
  Result := false;
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
end;

procedure LCLControlExtension.lclSetEnabled(AEnabled:Boolean);
begin
  {$ifdef BOOLFIX}
  SetEnabled_( Ord(AEnabled and ((not Assigned(superview)) or (superview.lclisEnabled))) );
  {$else}
  SetEnabled( AEnabled and ((not Assigned(superview)) or (superview.lclisEnabled)) );
  {$endif}
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

  {$ifdef BOOLFIX}
  setHidden_(Ord(AParams.Style and WS_VISIBLE = 0));
  {$else}
  setHidden(AParams.Style and WS_VISIBLE = 0);
  {$endif}

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
  {$ifdef BOOLFIX}
  setHidden_(Ord(not AVisible));
  {$else}
  setHidden(not AVisible);
  {$endif}
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
  setNeedsDisplay_(true);
  //display;
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
  AddLayoutToFrame( lclGetFrameToLayoutDelta, Result);
end;

procedure LCLViewExtension.lclSetFrame(const r: TRect);
var
  ns: NSRect;
  svHeight: CGFloat;
  rr : TRect;
begin
  rr := r;
  SubLayoutFromFrame( lclGetFrameToLayoutDelta, rr);

  svHeight := GetNSViewSuperViewHeight(Self);
  if Assigned(superview) and not superview.isFlipped then
  begin
    LCLToNSRect(rr, svHeight, ns)
  end
  else
    ns := RectToNSRect(rr);

  {$IFDEF COCOA_DEBUG_SETBOUNDS}
  WriteLn(Format('LCLViewExtension.lclSetFrame: %s Bounds=%s height=%d ns_pos=%d %d ns_size=%d %d',
    [NSStringToString(Self.ClassName), dbgs(r), Round(svHeight),
     Round(ns.origin.x), Round(ns.origin.y), Round(ns.size.width), Round(ns.size.height)]));
  {$ENDIF}
  setFrame(ns);
end;

function LCLViewExtension.lclClientFrame: TRect;
begin
  Result := lclFrame;
  Types.OffsetRect(Result, -Result.Left, -Result.Top);
end;

function LCLViewExtension.lclContentView: NSView;
begin
  Result := self;
end;

procedure LCLViewExtension.lclOffsetMousePos(var Point: NSPoint);
var
  es : NSScrollView;
  r  : NSRect;
  dlt : TRect;
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

  dlt := lclGetFrameToLayoutDelta;
  Point.X := Point.X - dlt.Left;
  Point.Y := Point.Y - dlt.Top;
end;

{ TCocoaStatusBar }

procedure TCocoaStatusBar.drawRect(dirtyRect: NSRect);
var
  R    : TRect;
  i    : Integer;
  cs   : NSString;
  nr   : NSRect;
  al   : TAlignment;
  x    : Integer;
  txt  : string;
  cnt  : Integer;
  w    : Integer;
const
  CocoaAlign: array [TAlignment] of Integer = (NSNaturalTextAlignment, NSRightTextAlignment, NSCenterTextAlignment);
begin
  if not Assigned(barcallback) then Exit;

  if not Assigned(panelCell) then Exit;

  panelCell.setControlView(Self);

  r := lclClientFrame();
  nr.origin.y := 0;
  nr.size.height := self.lclFrame.Height;

  x:=0;
  cnt := barcallback.GetBarsCount;
  for i:=0 to cnt - 1 do begin

    txt := '';
    w := 0;
    al := taLeftJustify;

    if not barcallback.GetBarItem(i, txt, w, al) then Continue;


    if i = cnt - 1 then w := r.Right - x;
    nr.size.width := w;
    nr.origin.x := x;

    cs := NSStringUtf8(txt);
    panelCell.setTitle(cs);
    panelCell.setAlignment(CocoaAlign[al]);
    panelCell.drawWithFrame_inView(nr, Self);
    cs.release;
    barcallback.DrawPanel(i, NSRectToRect(nr));
    inc(x, w);
    if x > r.Right then break; // no place left
  end;
end;

procedure TCocoaStatusBar.dealloc;
begin
  if Assigned(panelCell) then panelCell.release;
  inherited;
end;

{ TCocoaProgressIndicator }

function TCocoaProgressIndicator.acceptsFirstResponder: Boolean;
begin
  Result:=True;
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

function TCocoaProgressIndicator.lclGetFrameToLayoutDelta: TRect;
begin
  case controlSize of
    NSSmallControlSize, NSMiniControlSize:
    begin
      Result.Left := 1;
      Result.Right := -1;
      Result.Top := 0;
      Result.Bottom := -2;
    end;
  else
    Result.Left := 2;
    Result.Right := -2;
    Result.Top := 0;
    Result.Bottom := -4;
  end;
end;

procedure TCocoaProgressIndicator.lclSetFrame(const r: TRect);
begin
  SetNSControlSize(self, r.Bottom - r.Top, 0, PROGRESS_SMALL_HEIGHT, true);
  inherited lclSetFrame(r);
end;

function TCocoaProgressIndicator.acceptsFirstMouse(event: NSEvent): Boolean;
begin
  Result:=true;
end;

procedure TCocoaProgressIndicator.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);

    callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaProgressIndicator.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaProgressIndicator.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaProgressIndicator.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaProgressIndicator.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaProgressIndicator.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaProgressIndicator.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaProgressIndicator.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaProgressIndicator.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaProgressIndicator.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaProgressIndicator.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;


{ TCocoaSlider }

function GetManTicks(slider: TCocoaSlider): TManualTicks;
begin
  if not Assigned(slider.man) then
    slider.man := TManualTicks.Create;
  Result := slider.man;
end;

procedure TCocoaSlider.dealloc;
begin
  man.Free;
  inherited dealloc;
end;

procedure TCocoaSlider.drawRect(dirtyRect: NSRect);
var
  i  : integer;
  nr : NSRect;
  xr : NSRect;
  dr : NSRect;
  nm : integer;
  ctx : NSGraphicsContext;
  pth : NSBezierPath;
begin
  if not Assigned(man) or (not man.draw) then begin
    inherited drawRect(dirtyRect);
    Exit;
  end;

  nm := round(maxValue - minValue);
  if nm = 0 then Exit;
  if numberOfTickMarks < 2 then Exit;

  nr := rectOfTickMarkAtIndex(0);
  xr := rectOfTickMarkAtIndex(1);

  ctx := NSGraphicsContext.currentContext;
  pth := NSBezierPath.bezierPath;
  NSColor.controlShadowColor.setFill;
  dr:=nr;
  dr.origin.y := dr.origin.y + 1;
  dr.size.height := dr.size.height - 1;
  for i := 0 to man.count - 1 do begin
    dr.origin.x := round(nr.origin.x + (xr.origin.x - nr.origin.x) * (man.ticks[i] - minValue) / nm);
    pth.fillRect(dr);
  end;
  inherited drawRect(dirtyRect);
end;

function TCocoaSlider.acceptsFirstResponder: Boolean;
begin
  Result := True;
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
    inherited keyDown(event);
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
var
  newval: Integer;
begin
  SnapToInteger();
  newval := intValue;
  if newval <> intval then begin
    intval := newval;
    // OnChange event
    if callback <> nil then
      callback.SendOnChange();
  end;
end;

function TCocoaSlider.acceptsFirstMouse(event: NSEvent): Boolean;
begin
  Result:=true;
end;

procedure TCocoaSlider.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    inherited mouseDown(event);

    callback.MouseUpDownEvent(event, true);
  end;
end;

procedure TCocoaSlider.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaSlider.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaSlider.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaSlider.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaSlider.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaSlider.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaSlider.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaSlider.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaSlider.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaSlider.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaSlider.lclAddManTick(atick: integer);
var
  mn : TManualTicks;
begin
  mn := GetManTicks(self);
  if mn.AddTick(atick) then
  begin
    if mn.draw then self.setNeedsDisplay;
  end;
end;

procedure TCocoaSlider.lclSetManTickDraw(adraw: Boolean);
var
  mn : TManualTicks;
begin
  mn := GetManTicks(self);
  if mn.draw=adraw then Exit;
  mn.draw:=adraw;
  self.setNeedsDisplay;
end;

procedure TCocoaSlider.lclExpectedKeys(var wantTabs, wantArrows, wantReturn,
  wantAll: Boolean);
begin
  wantTabs := false;
  wantArrows := true;
  wantReturn := false;
  wantAll := false;
end;

type
  NSViewControlSizeExt = objccategory external (NSView)
    function controlSize: Integer; message 'controlSize';
    procedure setControlSize(ASize: Integer); message 'setControlSize:';
    function cell: id; message 'cell';
    procedure setFont(afont: NSFont); message 'setFont:';
  end;

procedure SetNSControlSize(ctrl: NSView; newHeight, miniHeight, smallHeight: Integer; AutoChangeFont: Boolean);
var
  sz : NSControlSize;
begin
  if (miniHeight>0) and (newHeight<=miniHeight) then
    sz:=NSMiniControlSize
  else if (smallHeight>0) and (newHeight<=smallHeight) then
    sz:=NSSmallControlSize
  else
    sz:=NSRegularControlSize;

  if ctrl.respondsToSelector(ObjCSelector('setControlSize:')) then
    ctrl.setControlSize(sz)
  else if ctrl.respondsToSelector(ObjCSelector('cell')) then
  begin
    if NSCell(ctrl.cell).controlSize<>sz then
        NSCell(ctrl.cell).setControlSize(sz);
  end;
  if AutoChangeFont and (ctrl.respondsToSelector(ObjCSelector('setFont:'))) then
    ctrl.setFont(NSFont.systemFontOfSize(NSFont.systemFontSizeForControlSize(sz)));
end;


end.

