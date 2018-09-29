{ $Id: $}
{                  --------------------------------------------
                  cocoawindows.pas  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit CocoaWindows;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects,
  cocoa_extra, CocoaPrivate, CocoaTextEdits,
  // LCL
  //Forms,
  LCLType, LCLProc;

type

  { LCLWindowExtension }

  LCLWindowExtension = objccategory(NSWindow)
    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:'; reintroduce;
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;

    function lclWindowState: Integer; message 'lclWindowState'; reintroduce;
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclUpdate; message 'lclUpdate'; reintroduce;
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::'; reintroduce;
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
    function lclGetTopBarHeight:integer; message 'lclGetTopBarHeight'; reintroduce;
    procedure lclOffsetMousePos(var Point: NSPoint); message 'lclOffsetMousePos:'; reintroduce;
  end;

  { IWindowCallback }

  IWindowCallback = interface(ICommonCallBack)
    function CanActivate: Boolean;
    procedure Activate;
    procedure Deactivate;
    procedure CloseQuery(var CanClose: Boolean);
    procedure Close;
    procedure Resize;
    procedure Move;

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);

    function AcceptFilesDrag: Boolean;
    procedure DropFiles(const FileNames: array of string);

    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  { TCocoaPanel }

  TCocoaPanel = objcclass(NSPanel, NSWindowDelegateProtocol)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
  public
    callback: IWindowCallback;
    function acceptsFirstResponder: Boolean; override;
    function canBecomeKeyWindow: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // mouse
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
    procedure sendEvent(event: NSEvent); override;
  end;

  { TCocoaWindow }

  TCocoaWindowContent = objcclass;

  TCocoaWindow = objcclass(NSWindow, NSWindowDelegateProtocol)
  protected
    fieldEditor: TCocoaFieldEditor;
    firedMouseEvent: Boolean;
    isInFullScreen: Boolean;
    orderOutAfterFS : Boolean;
    fsview: TCocoaWindowContent;

    responderSwitch: Integer;
    respInitCb : ICommonCallback;

    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    function windowWillReturnFieldEditor_toObject(sender: NSWindow; client: id): id; message 'windowWillReturnFieldEditor:toObject:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
    // fullscreen notifications are only reported for 10.7 fullscreen
    procedure windowWillEnterFullScreen(notification: NSNotification); message 'windowWillEnterFullScreen:';
    procedure windowDidEnterFullScreen(notification: NSNotification); message 'windowDidEnterFullScreen:';
    procedure windowDidExitFullScreen(notification: NSNotification); message 'windowDidExitFullScreen:';
  public
    callback: IWindowCallback;
    //LCLForm: TCustomForm;
    procedure dealloc; override;
    function acceptsFirstResponder: Boolean; override;
    function canBecomeKeyWindow: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    // mouse
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
    procedure sendEvent(event: NSEvent); override;
    // key
    // in practice those key-handling methods should NOT be needed, because a window
    // always have TCocoaWindowContent view. However, on some instances
    // the focus is not switched to CocoaWindowContent, and the window itself
    // remains the firstResponder. (ie CodeCompletion window, see bug #34301)
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    procedure flagsChanged(event: NSEvent); override;
    // NSDraggingDestinationCategory
    function draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation; override;
    function performDragOperation(sender: NSDraggingInfoProtocol): Boolean; override;
    // windows
    function makeFirstResponder(r: NSResponder): Boolean; override;
    // menu support
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';

    procedure lclSwitchFullScreen(AEnabled: Boolean); message 'lclSwitchFullScreen:';
    function lclIsFullScreen: Boolean; message 'lclIsFullScreen';
  end;

  { TCocoaDesignOverlay }

  TCocoaDesignOverlay = objcclass(NSView)
    callback  : ICommonCallback;
    procedure drawRect(r: NSRect); override;
    function acceptsFirstResponder: Boolean; override;
    function hitTest(aPoint: NSPoint): NSView; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
  end;

  { TCocoaWindowContent }

  TCocoaWindowContent = objcclass(TCocoaCustomControl)
  protected
    procedure didBecomeKeyNotification(sender: NSNotification); message 'didBecomeKeyNotification:';
    procedure didResignKeyNotification(sender: NSNotification); message 'didResignKeyNotification:';
  public
    isembedded: Boolean; // true - if the content is inside of another control, false - if the content is in its own window;
    preventKeyOnShow: Boolean;
    ownwin: NSWindow;
    fswin: NSWindow; // window that was used as a content prior to switching to old-school fullscreen
    popup_parent: HWND; // if not 0, indicates that we should set the popup parent
    overlay: NSView;
    function performKeyEquivalent(event: NSEvent): Boolean; override;
    procedure resolvePopupParent(); message 'resolvePopupParent';
    function lclOwnWindow: NSWindow; message 'lclOwnWindow';
    procedure lclSetFrame(const r: TRect); override;
    function lclFrame: TRect; override;
    procedure viewDidMoveToSuperview; override;
    procedure viewDidMoveToWindow; override;
    procedure viewWillMoveToWindow(newWindow: CocoaAll.NSWindow); override;
    procedure dealloc; override;
    procedure setHidden(aisHidden: Boolean); override;
    procedure didAddSubview(aview: NSView); override;
  end;

procedure WindowPerformKeyDown(win: NSWindow; event: NSEvent; out processed: Boolean);

implementation

// Cocoa emulation routine.
//
// For whatever reason, the default keyDown: event processing, is triggerring
// some macOSX hot keys PRIOR to reaching keyDown: (Which is a little bit unpredictable)
// So the below Key-event-Path is a light version of what is described, in Cocoa
// documentation.
// first - run controls and menus, for performKeyEquivalent
// then pass keyDown through
//
// The order can be reverted and let Controls do the key processing first
// and menu to handle the event after.

procedure WindowPerformKeyDown(win: NSWindow; event: NSEvent; out processed: Boolean);
var
  r : NSResponder;
  fr : NSResponder;
  mn : NSMenu;
  cb : ICommonCallback;
  allowcocoa : Boolean;

begin
  fr := win.firstResponder;
  r := fr;
  processed := false;
  allowcocoa := true;

  if Assigned(fr) then
  begin
    cb := fr.lclGetCallback;
    if Assigned(cb) then
    begin
      cb.KeyEvPrepare(event);
      cb.KeyEvBefore(allowcocoa);
    end;
  end else
    cb := nil;

  // try..finally here is to handle "Exit"s
  // rather than excepting any exceptions to happen
  try
    if not allowcocoa then Exit;

    // let controls to performKeyEquivalent first
    while Assigned(r) and not processed do begin
      if r.respondsToSelector(objcselector('performKeyEquivalent:')) then
        processed := r.performKeyEquivalent(event);
      if not processed then r := r.nextResponder;
    end;
    if processed then Exit;

    // let menus do the hot key, if controls don't like it.
    if not processed then
    begin
      mn := NSApplication(NSApp).mainMenu;
      if Assigned(mn) then
        processed := mn.performKeyEquivalent(event);
    end;
    if processed then Exit;

    r := fr;
    while Assigned(r) and not processed do begin
      if r.respondsToSelector(objcselector('keyDown:')) then
      begin
        r.keyDown(event);
        processed := true;
      end;
      if not processed then r := r.nextResponder;
    end;

  finally
    if Assigned(cb) then
      cb.KeyEvAfter;
  end;

end;

{ TCocoaDesignOverlay }

procedure TCocoaDesignOverlay.drawRect(r: NSRect);
begin
  if Assigned(callback) then
    callback.DrawOverlay(NSGraphicsContext.currentContext, bounds, r);
  inherited drawRect(r);
end;

function TCocoaDesignOverlay.acceptsFirstResponder: Boolean;
begin
  Result:=false; // no focus
end;

function TCocoaDesignOverlay.hitTest(aPoint: NSPoint): NSView;
begin
  Result:=nil;  // no mouse
end;

function TCocoaDesignOverlay.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaDesignOverlay.lclClearCallback;
begin
  callback := nil;
end;

{ TCocoaWindowContent }

procedure TCocoaWindowContent.didAddSubview(aview: NSView);
begin
  if Assigned(aview) and Assigned(overlay) and (overlay<>aview) then
  begin
    overlay.retain;
    overlay.removeFromSuperview;
    addSubview_positioned_relativeTo(overlay, NSWindowAbove, nil);
  end;
  inherited didAddSubview(aview);
end;

procedure TCocoaWindowContent.didBecomeKeyNotification(sender: NSNotification);
begin
  if Assigned(callback) then
    callback.DidBecomeKeyNotification;
end;

procedure TCocoaWindowContent.didResignKeyNotification(sender: NSNotification);
begin
  if Assigned(callback) then
    callback.DidResignKeyNotification;
end;

function TCocoaWindowContent.performKeyEquivalent(event: NSEvent): Boolean;
var
  resp : NSResponder;
  wn   : NSWindow;
  view : NSTextView;
begin
  Result := false;
  // only respond to key, if focused

  wn := window;
  if not Assigned(wn) then Exit;
  resp := wn.firstResponder;
  if (not Assigned(resp)) or (not resp.isKindOfClass_(NSTextView)) then Exit;

  if (not resp.lclIsEnabled) then Exit;

  NSResponderHotKeys(self, event, Result, resp);
  if not Result then
    Result:=inherited performKeyEquivalent(event);
end;

procedure TCocoaWindowContent.resolvePopupParent();
var
  lWindow: NSWindow;
  isfront: Boolean;
begin
  lWindow := nil;
  if (popup_parent <> 0) then
  begin
    if (NSObject(popup_parent).isKindOfClass(TCocoaWindowContent)) then
    begin
      if (not TCocoaWindowContent(popup_parent).isembedded) then
        lWindow := NSWindow(TCocoaWindowContent(popup_parent).window);
    end
    else
    begin
      lWindow := NSWindow(popup_parent);
    end;
  end;
  if lWindow <> nil then
  begin
    isfront:=NSApplication(NSApp).mainWindow=self.window;

    lWindow.addChildWindow_ordered(Self.window, NSWindowAbove);

    // adding a window as a child, would bring the "child" form to the bottom
    // of Zorder. need to restore the order.
    if isfront then self.window.makeKeyAndOrderFront(nil);
  end;
  popup_parent := 0;
end;

function TCocoaWindowContent.lclOwnWindow: NSWindow;
begin
  if not isembedded then
    Result := NSWindow(window)
  else
    Result := nil;
end;

procedure TCocoaWindowContent.lclSetFrame(const r: TRect);
begin
  if isembedded then
    inherited lclSetFrame(r)
  else
    window.lclSetFrame(r);
end;

function TCocoaWindowContent.lclFrame: TRect;
var
  wfrm : TRect;
begin
  Result := inherited lclFrame;
  if not isembedded then
  begin
    //Window bounds should return "client rect" in screen coordinates
    if Assigned(window.screen) then
      NSToLCLRect(window.frame, window.screen.frame.size.height, wfrm)
    else
      wfrm := NSRectToRect(frame);
    OffsetRect(Result, -Result.Left+wfrm.Left, -Result.Top+wfrm.Top);
  end;
end;

procedure TCocoaWindowContent.viewDidMoveToSuperview;
begin
  inherited viewDidMoveToSuperview;
end;

procedure TCocoaWindowContent.viewDidMoveToWindow;
begin
  isembedded := window.contentView <> self;
  if isembedded then
  begin
    if Assigned(ownwin) then
      ownwin.close;
    ownwin := nil;
  end
  else
  begin
    ownwin := NSWindow(window);
    if Assigned(stringValue) then
      ownwin.setTitle(stringValue);
  end;
  inherited viewDidMoveToWindow;
end;

procedure TCocoaWindowContent.viewWillMoveToWindow(newWindow: CocoaAll.NSWindow);
begin
  if newWindow<>nil then
     newWindow.setAcceptsMouseMovedEvents(True);
  if not isembedded and (newWindow <> window) then
  begin
    if Assigned(window) then
    begin
      setStringValue(window.title);
      window.close;
    end;
    ownwin := nil;
    isembedded := false;
  end;
  inherited viewWillMoveToWindow(newWindow);
end;

procedure TCocoaWindowContent.dealloc;
begin
  inherited dealloc;
end;

procedure TCocoaWindowContent.setHidden(aisHidden: Boolean);
var
  cw : TCocoaWindow;
begin
  if isembedded then
  begin
    inherited setHidden(aisHidden);
  end
  else
  begin
    if aisHidden and window.isVisible then
    begin
      if (window.isKindOfClass(TCocoaWindow)) then
        cw := TCocoaWindow(window)
      else
        cw := nil;
      if cw.lclIsFullScreen then
      begin
        cw.orderOutAfterFS := true;
        cw.lclSwitchFullScreen(false);
      end else
        window.orderOut(nil);
    end
    else
    if not aisHidden and not window.isVisible then
    begin
      if preventKeyOnShow then // used for Hint-windows, so they would not steal the focus from the current window
        window.orderFrontRegardless
      else
        window.makeKeyAndOrderFront(nil);
    end;
  end;
end;

{ TCocoaPanel }

function TCocoaPanel.windowShouldClose(sender: id): LongBool;
var
  canClose: Boolean;
begin
  canClose := True;
  if Assigned(callback) then
    callback.CloseQuery(canClose);
  Result := canClose;
end;

procedure TCocoaPanel.windowWillClose(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Close;
end;

procedure TCocoaPanel.windowDidBecomeKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Activate;
end;

procedure TCocoaPanel.windowDidResignKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Deactivate;
end;

procedure TCocoaPanel.windowDidResize(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Resize;
end;

procedure TCocoaPanel.windowDidMove(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Move;
end;

function TCocoaPanel.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaPanel.canBecomeKeyWindow: Boolean;
begin
  Result := Assigned(callback) and callback.CanActivate;
end;

function TCocoaPanel.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
//  if Assigned(callback) then
//    callback.BecomeFirstResponder;
end;

function TCocoaPanel.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
//  if Assigned(callback) then
//    callback.ResignFirstResponder;
end;

function TCocoaPanel.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaPanel.lclClearCallback;
begin
  callback := nil;
  contentView.lclClearCallback;
end;

procedure TCocoaPanel.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaPanel.mouseUp(event: NSEvent);
begin
  if Assigned(callback) then callback.MouseUpDownEvent(event);
    inherited mouseUp(event);
end;

procedure TCocoaPanel.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaPanel.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaPanel.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaPanel.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaPanel.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaPanel.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaPanel.mouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaPanel.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaPanel.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaPanel.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaPanel.sendEvent(event: NSEvent);
var
  Message: NSMutableDictionary;
  Handle: HWND;
  Msg: Cardinal;
  WP: WParam;
  LP: LParam;
  ResultCode: NSNumber;
  Obj: NSObject;
begin
  if event.type_ = NSApplicationDefined then
  begin
    // event which we get through PostMessage or SendMessage
    if event.subtype = LCLEventSubTypeMessage then
    begin
      // extract message data
      Message := NSMutableDictionary(event.data1);
      Handle := NSNumber(Message.objectForKey(NSMessageWnd)).unsignedIntegerValue;
      Msg := NSNumber(Message.objectForKey(NSMessageMsg)).unsignedLongValue;
      WP := NSNumber(Message.objectForKey(NSMessageWParam)).integerValue;
      LP := NSNumber(Message.objectForKey(NSMessageLParam)).integerValue;
      Obj := NSObject(Handle);
      // deliver message and set result if response requested
      // todo: check that Obj is still a valid NSView/NSWindow
      ResultCode := NSNumber.numberWithInteger(Obj.lclDeliverMessage(Msg, WP, LP));
      if event.data2 <> 0 then
        Message.setObject_forKey(ResultCode, NSMessageResult)
      else
        Message.release;
      //ResultCode.release;                   // will be auto-released
     end;
  end
  else
    inherited sendEvent(event);
end;

{ TCocoaWindow }

function TCocoaWindow.windowShouldClose(sender: id): LongBool;
var
  canClose: Boolean;
begin
  canClose := True;
  if Assigned(callback) then
    callback.CloseQuery(canClose);
  Result := canClose;
end;

function TCocoaWindow.windowWillReturnFieldEditor_toObject(sender: NSWindow; client: id): id;
begin
  //DebugLn('[TCocoaWindow.windowWillReturnFieldEditor_toObject]');
  Result := nil;

  if (NSObject(client).isKindOfClass(NSTextField)) and Assigned(NSObject(client).lclGetCallBack) then
  begin
    if (fieldEditor = nil) then
    begin
      fieldEditor := TCocoaFieldEditor.alloc.init;
      fieldEditor.setFieldEditor(True);
    end;
    Result := fieldEditor;
  end;
end;

procedure TCocoaWindow.windowWillClose(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Close;
end;

procedure TCocoaWindow.windowDidBecomeKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Activate;
end;

procedure TCocoaWindow.windowDidResignKey(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Deactivate;
end;

procedure TCocoaWindow.windowDidResize(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Resize;
end;

procedure TCocoaWindow.windowDidMove(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.Move;
end;

procedure TCocoaWindow.windowWillEnterFullScreen(notification: NSNotification);
begin
  if not isInFullScreen then isInFullScreen := true;
  // setting fullscreen flag, prior to the "Fullscreen" has actually been enabled.
  // MacOS does 10.7 fullscreen switch with an animation (that's about 1 second long)
  // if during that animation there's another call toggleFullScreen() is made
  // then macOS produces an output "not in fullscreen state" and ignores the call.
end;

procedure TCocoaWindow.windowDidEnterFullScreen(notification: NSNotification);
begin
  if not isInFullScreen then isInFullScreen := true;
end;

procedure TCocoaWindow.windowDidExitFullScreen(notification: NSNotification);
begin
  if isInFullScreen then isInFullScreen := false;
  if orderOutAfterFS then begin
    self.orderOut(nil);
    orderOutAfterFS := false;
  end;
end;

procedure TCocoaWindow.dealloc;
begin
  if (fieldEditor <> nil) then
  begin
    fieldEditor.release;
    fieldEditor := nil;
  end;
  inherited dealloc;
end;

function TCocoaWindow.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaWindow.canBecomeKeyWindow: Boolean;
begin
  Result := Assigned(callback) and callback.CanActivate;
end;

function TCocoaWindow.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  // uncommenting the following lines starts an endless focus loop

//  if Assigned(callback) then
//    callback.BecomeFirstResponder;
end;

function TCocoaWindow.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
//  if Assigned(callback) then
//    callback.ResignFirstResponder;
end;

function TCocoaWindow.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaWindow.lclClearCallback;
begin
  callback := nil;
  contentView.lclClearCallback;
end;

procedure TCocoaWindow.mouseDown(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaWindow.mouseUp(event: NSEvent);
begin
  //firedMouseEvent:=true;
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaWindow.rightMouseDown(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaWindow.rightMouseUp(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaWindow.rightMouseDragged(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaWindow.otherMouseDown(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaWindow.otherMouseUp(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaWindow.otherMouseDragged(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaWindow.mouseDragged(event: NSEvent);
begin
  //if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaWindow.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaWindow.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaWindow.mouseMoved(event: NSEvent);
begin
  // no need to call for callback or anything, because WindowContent
  // will take care of it anyway
  inherited mouseMoved(event);
end;

procedure TCocoaWindow.scrollWheel(event: NSEvent);
begin
  if not Assigned(callback) or not callback.scrollWheel(event) then
    inherited scrollWheel(event);
end;

procedure TCocoaWindow.sendEvent(event: NSEvent);
var
  Message: NSMutableDictionary;
  Handle: HWND;
  Msg: Cardinal;
  WP: WParam;
  LP: LParam;
  ResultCode: NSNumber;
  Obj: NSObject;

  Epos: NSPoint;
  cr : NSRect;
  fr : NSRect;
  prc: Boolean;
begin
  if event.type_ = NSApplicationDefined then
  begin
    // event which we get through PostMessage or SendMessage
    if event.subtype = LCLEventSubTypeMessage then
    begin
      // extract message data
      Message := NSMutableDictionary(event.data1);
      Handle := NSNumber(Message.objectForKey(NSMessageWnd)).unsignedIntegerValue;
      Msg := NSNumber(Message.objectForKey(NSMessageMsg)).unsignedLongValue;
      WP := NSNumber(Message.objectForKey(NSMessageWParam)).integerValue;
      LP := NSNumber(Message.objectForKey(NSMessageLParam)).integerValue;
      // deliver message and set result if response requested
      Obj := NSObject(Handle);
      // todo: check that Obj is still a valid NSView/NSWindow
      ResultCode := NSNumber.numberWithInteger(Obj.lclDeliverMessage(Msg, WP, LP));
      if event.data2 <> 0 then
        Message.setObject_forKey(ResultCode, NSMessageResult)
      else
        Message.release;
      //ResultCode.release;               // will be auto-released
    end;
  end
  else
  if event.type_ = NSLeftMouseUp then
  // This code is introduced here for an odd cocoa feature.
  // mouseUp is not fired, if pressed on Window's title.
  // (even though mouseDown, mouseMove and mouseDragged are fired)
  // (there are some information in the internet, that mouseDown is not firing as well)
  // (however this is not true for macOS 10.12)
  // The logic below is as following. If mouseUp event arrived
  // and mouse position is on the title of the form.
  // then try to process the event. If event was not processed, call mouseUp()
  // specifically.
  begin
    Epos:=event.locationInWindow;
    fr := frame;
    fr.origin.x:=0;
    fr.origin.y:=0;
    cr := contentRectForFrameRect(fr);
    if NSPointInRect(Epos, fr) and not NSPointInRect(Epos, cr) then
    begin
      firedMouseEvent := false;
      inherited sendEvent(event);
      if not firedMouseEvent then mouseUp(event);
    end
    else
      inherited sendEvent(event);
  end
  else
  if event.type_ = NSKeyDown then
    WindowPerformKeyDown(self, event, prc)
  else
    inherited sendEvent(event);
end;

procedure TCocoaWindow.keyDown(event: NSEvent);
begin
  inherited keyDown(event);
end;

procedure TCocoaWindow.keyUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited keyUp(event);
end;

procedure TCocoaWindow.flagsChanged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.KeyEvent(event) then
    inherited flagsChanged(event);
end;

function TCocoaWindow.draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
begin
  Result := NSDragOperationNone;
  if (callback <> nil) and (callback.AcceptFilesDrag) then
    Result := sender.draggingSourceOperationMask();
end;

function TCocoaWindow.performDragOperation(sender: NSDraggingInfoProtocol): Boolean;
var
  draggedURLs{, lClasses}: NSArray;
  lFiles: array of string;
  i: Integer;
  pboard: NSPasteboard;
  lNSStr: NSString;
  //lClass: pobjc_class;
begin
  Result := False;
  pboard := sender.draggingPasteboard();

  // Multiple strings
  draggedURLs := pboard.propertyListForType(NSFilenamesPboardType);
  SetLength(lFiles, draggedURLs.count);
  for i := 0 to draggedURLs.count-1 do
  begin
    lNSStr := NSString(draggedURLs.objectAtIndex(i));
    lFiles[i] := NSStringToString(lNSStr);
  end;

  // Multiple URLs -> Results in strange URLs with file:// protocol
  {if pboard.types.containsObject(NSURLPboardType) then
  begin
    lClass := NSURL.classClass;
    lClasses := NSArray.arrayWithObjects_count(@lClass, 1);
    draggedURLs := pboard.readObjectsForClasses_options(lClasses, nil);
    SetLength(lFiles, draggedURLs.count);
    for i := 0 to draggedURLs.count-1 do
    begin
      lNSStr := NSURL(draggedURLs.objectAtIndex(i)).absoluteString;
      lFiles[i] := NSStringToString(lNSStr);
    end;
  end;}

  if (Length(lFiles) > 0) and (callback <> nil)  then
    callback.DropFiles(lFiles);
  Result := True;
end;

function TCocoaWindow.makeFirstResponder(r: NSResponder): Boolean;
var
  cbnew: ICommonCallback;
begin
  if (responderSwitch = 0) then
    respInitCb := firstResponder.lclGetCallback;

  // makeFirstResponder calls can be recursive!
  // the resulting NSResponder can be the same object  (i.e. fieldEditor)
  // yet, the callback should be the different anyway

  inc(responderSwitch);
  Result:=inherited makeFirstResponder(r);
  dec(responderSwitch);

  if (responderSwitch = 0) then
  begin
    cbnew := firstResponder.lclGetCallback;

    if not isCallbackForSameObject(respInitCb, cbnew) then
    begin
      if Assigned(respInitCb) then respInitCb.ResignFirstResponder;
      if Assigned(cbnew) then cbnew.BecomeFirstResponder;
    end;
  end;
end;

procedure TCocoaWindow.lclItemSelected(sender: id);
begin

end;

procedure TCocoaWindow.lclSwitchFullScreen(AEnabled: Boolean);
const
  fsmask =  NSWindowCollectionBehaviorFullScreenPrimary
            or
            NSWindowCollectionBehaviorFullScreenAuxiliary;
begin
  if isInFullScreen = AEnabled then Exit; // nothing to do

  //todo: there are two flavours of full-screen
  //      (soft) macOS 10.7+ toggleFullScreen()
  //      (hard) macOS 10.5+ enterFullScreenMode_withOptions()
  //      the function should be smart enough to figure out the available mode

  isInFullScreen := AEnabled;
  if NSAppKitVersionNumber >= NSAppKitVersionNumber10_7 then
  begin
    if Self.collectionBehavior and fsmask = 0 then
      Self.setCollectionBehavior(Self.collectionBehavior or NSWindowCollectionBehaviorFullScreenPrimary);
    Self.toggleFullScreen(nil);
  end
  else
  begin
    if AEnabled then
    begin
      fsview := TCocoaWindowContent(contentView);
      fsview.fswin := self;
      fsview.enterFullScreenMode_withOptions(self.screen, nil);
    end else begin
      fsview.exitFullScreenModeWithOptions(nil);
      self.setContentView(fsview);
      fsview := nil;
    end;
  end;
end;

function TCocoaWindow.lclIsFullScreen: Boolean;
begin
  Result := isInFullScreen;
end;

{ LCLWindowExtension }

function LCLWindowExtension.lclIsVisible: Boolean;
begin
  Result := isVisible;
end;

procedure LCLWindowExtension.lclSetVisible(AVisible: Boolean);
begin
  if AVisible then
    orderFrontRegardless
  else
    orderOut(nil);
end;

function LCLWindowExtension.lclIsEnabled: Boolean;
begin
  Result := contentView.lclIsEnabled;
end;

procedure LCLWindowExtension.lclSetEnabled(AEnabled: Boolean);
begin
  contentView.lclSetEnabled(AEnabled);
end;

function LCLWindowExtension.lclWindowState: Integer;
const
  NSFullScreenWindowMask = 1 shl 14;
begin
  if isMiniaturized then
    Result := SIZE_MINIMIZED
  else
  if (styleMask and NSFullScreenWindowMask) <> 0 then
    Result := SIZE_FULLSCREEN
  else
  if isZoomed then
    Result := SIZE_MAXIMIZED
  else
    Result := SIZE_RESTORED;
end;

procedure LCLWindowExtension.lclInvalidateRect(const r: TRect);
begin
  contentView.lclInvalidateRect(r);
end;

procedure LCLWindowExtension.lclInvalidate;
begin
  contentView.lclInvalidate;
end;

procedure LCLWindowExtension.lclUpdate;
begin
  contentView.lclUpdate;
end;

procedure LCLWindowExtension.lclRelativePos(var Left, Top: Integer);
var
   f: NSRect;
begin
  if Assigned(screen) then
  begin
    f:=frame;
    Left := Round(f.origin.x);
    Top := Round(screen.frame.size.height - f.size.height - f.origin.y);
    //debugln('Top:'+dbgs(Top));
  end;
end;

procedure LCLWindowExtension.lclLocalToScreen(var X, Y:Integer);
var
  f: NSRect;
begin
  if Assigned(screen) then
  begin
    f := frame;
    inc(X, Round(f.origin.x));
    inc(Y, Round(screen.frame.size.height - f.size.height - f.origin.y));
  end;
end;

procedure LCLWindowExtension.lclScreenToLocal(var X, Y: Integer);
var
  f: NSRect;
begin
  if Assigned(screen) then
  begin
    f := frame;
    dec(X, Round(f.origin.x));
    dec(Y, Round(screen.frame.size.height - f.size.height - f.origin.y));
  end;
end;

function LCLWindowExtension.lclFrame: TRect;
begin
  if Assigned(contentView) then
    Result:=contentView.lclFrame
  else
  begin
    if Assigned(screen) then
      NSToLCLRect(frame, screen.frame.size.height, Result)
    else
      Result := NSRectToRect(frame);
  end;
end;

function LCLWindowExtension.lclGetTopBarHeight:integer;
var nw,nf: NSRect;
begin
  nf:= NSMakeRect (0, 0, 100, 100);
  nw:=contentRectForFrameRect(nf);
  result:=round(nf.size.height-nw.size.height);
end;

procedure LCLWindowExtension.lclOffsetMousePos(var Point: NSPoint);
begin
  Point.y := contentView.bounds.size.height - Point.y;
end;

procedure LCLWindowExtension.lclSetFrame(const r: TRect);
var
  ns : NSRect;
  h  : integer;
  i  : integer;
  p  : NSPoint;
  sc : NSScreen;
  srect : NSRect;
  fnd: Boolean;
begin
  fnd := Assigned(screen);
  if fnd then
    srect := screen.frame
  else
  begin
    // the window doesn't have screen assigned.
    // figuring out the placement based of the Left/Top of the rect
    // and NSrects;
    fnd := false;
    srect := NSMakeRect(0,0,0,0); // making the compiler happy
    p.x:=r.Left;
    p.y:=r.Top;
    for sc in NSScreen.screens do begin
      srect := sc.frame;
      fnd := NSPointInRect(p, srect);
      if fnd then Break;
    end;
  end;

  if fnd then
    LCLToNSRect(r, srect.size.height, ns)
  else
    ns := RectToNSRect(r);

  // add topbar height
  h:=lclGetTopBarHeight;
  ns.size.height:=ns.size.height+h;
  ns.origin.y:=ns.origin.y-h;
  setFrame_display(ns, isVisible);
end;

function LCLWindowExtension.lclClientFrame: TRect;
var
  wFrame, cFrame: NSRect;
begin
  wFrame := frame;
  cFrame := contentRectForFrameRect(wFrame);
  Result.Left := Round(cFrame.origin.x - wFrame.origin.x);
  Result.Top := Round(wFrame.origin.y + wFrame.size.height - cFrame.origin.y - cFrame.size.height);
  Result.Right := Result.Left + Round(cFrame.size.width);
  Result.Bottom := Result.Top + Round(cFrame.size.height);
end;

end.

