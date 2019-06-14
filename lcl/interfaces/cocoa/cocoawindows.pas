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
{$include cocoadefines.inc}

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
    procedure WindowStateChanged;

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);

    function AcceptFilesDrag: Boolean;
    procedure DropFiles(const FileNames: array of string);

    function HasCancelControl: Boolean;
    function HasDefaultControl: Boolean;

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
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function canBecomeKeyWindow: LCLObjCBoolean; override;
    function becomeFirstResponder: LCLObjCBoolean; override;
    function resignFirstResponder: LCLObjCBoolean; override;
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

    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    function windowWillReturnFieldEditor_toObject(sender: NSWindow; client: id): id; message 'windowWillReturnFieldEditor:toObject:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
    procedure windowDidMiniaturize(notification: NSNotification); message 'windowDidMiniaturize:';
    procedure windowDidDeminiaturize(notification: NSNotification); message 'windowDidDeminiaturize:';
    // fullscreen notifications are only reported for 10.7 fullscreen
    procedure windowWillEnterFullScreen(notification: NSNotification); message 'windowWillEnterFullScreen:';
    procedure windowDidEnterFullScreen(notification: NSNotification); message 'windowDidEnterFullScreen:';
    procedure windowDidExitFullScreen(notification: NSNotification); message 'windowDidExitFullScreen:';
  public
    _keyEvCallback: ICommonCallback;
    _calledKeyEvAfter: Boolean;
    callback: IWindowCallback;
    keepWinLevel : NSInteger;
    //LCLForm: TCustomForm;
    procedure dealloc; override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function canBecomeKeyWindow: LCLObjCBoolean; override;
    function becomeFirstResponder: LCLObjCBoolean; override;
    function resignFirstResponder: LCLObjCBoolean; override;
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
    procedure keyDown(event: NSEvent); override;
    // menu support
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';

    procedure lclSwitchFullScreen(AEnabled: Boolean); message 'lclSwitchFullScreen:';
    function lclIsFullScreen: Boolean; message 'lclIsFullScreen';
  end;

  { TCocoaDesignOverlay }

  TCocoaDesignOverlay = objcclass(NSView)
    callback  : ICommonCallback;
    procedure drawRect(r: NSRect); override;
    function acceptsFirstResponder: LCLObjCBoolean; override;
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
    wincallback: IWindowCallback;
    isembedded: Boolean; // true - if the content is inside of another control, false - if the content is in its own window;
    preventKeyOnShow: Boolean;
    ownwin: NSWindow;
    fswin: NSWindow; // window that was used as a content prior to switching to old-school fullscreen
    popup_parent: HWND; // if not 0, indicates that we should set the popup parent
    overlay: NSView;
    function performKeyEquivalent(event: NSEvent): LCLObjCBoolean; override;
    procedure resolvePopupParent(); message 'resolvePopupParent';
    function lclOwnWindow: NSWindow; message 'lclOwnWindow';
    procedure lclSetFrame(const r: TRect); override;
    function lclFrame: TRect; override;
    function lclWindowState: Integer; override;
    procedure viewDidMoveToSuperview; override;
    procedure viewDidMoveToWindow; override;
    procedure viewWillMoveToWindow(newWindow: CocoaAll.NSWindow); override;
    procedure dealloc; override;
    procedure setHidden(aisHidden: LCLObjCBoolean); override;
    procedure didAddSubview(aview: NSView); override;
    // NSDraggingDestinationCategory
    function draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation; override;
    function performDragOperation(sender: NSDraggingInfoProtocol): LCLObjCBoolean; override;
    procedure setNeedsDisplay_(aflag: LCLObjCBoolean); override;
    procedure setNeedsDisplayInRect(arect: NSRect); override;
  end;

function NSEventRawKeyChar(ev: NSEvent): System.WideChar;
procedure NSScreenGetRect(sc: NSScreen; out r: TRect);
procedure NSScreenGetRect(sc: NSScreen; mainScreenHeight: double; out r: TRect);

implementation

function NSEventRawKeyChar(ev: NSEvent): System.WideChar;
var
  m : NSString;
begin
  m := ev.charactersIgnoringModifiers;
  if m.length <> 1 then
    Result := #0
  else
    Result := System.WideChar(m.characterAtIndex(0));
end;


{ TCocoaDesignOverlay }

procedure TCocoaDesignOverlay.drawRect(r: NSRect);
begin
  if Assigned(callback) then
    callback.DrawOverlay(NSGraphicsContext.currentContext, bounds, r);
  inherited drawRect(r);
end;

function TCocoaDesignOverlay.acceptsFirstResponder: LCLObjCBoolean;
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
const
  mustHaveSizing = (NSViewWidthSizable or NSViewHeightSizable);
begin
  if Assigned(aview) and Assigned(overlay) and (overlay<>aview) then
  begin
    overlay.retain;
    overlay.removeFromSuperview;
    addSubview_positioned_relativeTo(overlay, NSWindowAbove, nil);
    overlay.release;
    overlay.setFrame(frame);
    if (overlay.autoresizingMask and mustHaveSizing) <> mustHaveSizing then
      overlay.setAutoresizingMask(overlay.autoresizingMask or mustHaveSizing);
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

procedure NSResponderHotKeys(asender: NSResponder; event: NSEvent; var handled: LCLObjCBoolean; atarget: NSResponder);
var
  undoManager: NSUndoManager;
  ch : System.WideChar;
begin
  // todo: system keys could be overriden. thus need to review the current
  //       keyboard configuration first. See "Key Bindings" at
  //       https://developer.apple.com/library/content/documentation/Cocoa/Conceptual/EventOverview/TextDefaultsBindings/TextDefaultsBindings.html

  handled := false;
  if (event.type_ = NSKeyDown) then
  begin
    if ((event.modifierFlags and NSCommandKeyMask) = 0) then Exit;

    ch := NSEventRawKeyChar(event);
    case ch of
      'a': handled := NSApplication(NSApp).sendAction_to_from(objcselector('selectAll:'), atarget, asender);
      'c': handled := NSApplication(NSApp).sendAction_to_from(objcselector('copy:'), atarget, asender);
      'v': handled := NSApplication(NSApp).sendAction_to_from(objcselector('paste:'), atarget, asender);
      'x': handled := NSApplication(NSApp).sendAction_to_from(objcselector('cut:'), atarget, asender);
      'z':
      begin
        undoManager := atarget.undoManager;
        if Assigned(undoManager) and undoManager.canUndo then
        begin
          handled := true;
          undoManager.undo;
        end;
      end;
      'Z':
      begin
        undoManager := atarget.undoManager;
        if Assigned(undoManager) and undoManager.canRedo then
        begin
          handled := true;
          undoManager.redo;
        end;
      end;
    end;
  end;
end;

function TCocoaWindowContent.performKeyEquivalent(event: NSEvent): LCLObjCBoolean;
var
  resp : NSResponder;
  wn   : NSWindow;
  ch   : System.WideChar;
begin
  Result := false;

  // If the form has a default or cancel button, capture Return and Escape to
  // prevent further processing.  Actually clicking the buttons is handled in
  // the LCL in response to the keyUp
  if Assigned(wincallback) and (event.modifierFlags_ = 0) then
  begin
    ch := NSEventRawKeyChar(event);
    if (((ch = System.WideChar(NSCarriageReturnCharacter)) and wincallback.HasDefaultControl)
      or ((ch = #27{Escape}) and wincallback.HasCancelControl)) then
    begin
      Result := true;
      Exit;
    end;
  end;

  // Support Cut/Copy/Paste if the firstResponder is an NSTextView.
  // This could be done in TCocoaFieldEditor and TCocoaTextView's
  // performKeyEquivalent, but that wouldn't work for non-LCL edits.
  // Xcode Cocoa apps rely on the commands existing in the main menu
  wn := window;
  if Assigned(wn) then
  begin
    resp := wn.firstResponder;
    if Assigned(resp) and resp.isKindOfClass_(NSTextView) and
       resp.lclIsEnabled then
    begin
      NSResponderHotKeys(self, event, Result, resp);
      if Result then Exit;
    end;
  end;

  Result := inherited performKeyEquivalent(event);
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

function TCocoaWindowContent.lclWindowState: Integer;
begin
  if isembedded then
    Result := inherited lclWindowState
  else
    Result := window.lclWindowState;
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
      setStringValue(window.title);
    ownwin := nil;
    isembedded := false;
  end;
  inherited viewWillMoveToWindow(newWindow);
end;

procedure TCocoaWindowContent.dealloc;
begin
  inherited dealloc;
end;

procedure TCocoaWindowContent.setHidden(aisHidden: LCLObjCBoolean);
var
  cw : TCocoaWindow;
begin
  if isembedded then
  begin
    {$ifdef BOOLFIX}
    inherited setHidden_(Ord(aisHidden));
    {$else}
    inherited setHidden(aisHidden);
    {$endif}
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

function TCocoaPanel.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaPanel.canBecomeKeyWindow: LCLObjCBoolean;
begin
  Result := Assigned(callback) and callback.CanActivate;
end;

function TCocoaPanel.becomeFirstResponder: LCLObjCBoolean;
begin
  Result := inherited becomeFirstResponder;
//  if Assigned(callback) then
//    callback.BecomeFirstResponder;
end;

function TCocoaPanel.resignFirstResponder: LCLObjCBoolean;
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
  // forcing to keep the level as all other LCL windows
  // Modal windows tend to "restore" their elevated level
  // And that doesn't work for modal windows that are "Showing" other windows

  // Another approach is to set elevated levels for windows, shown during modal session
  // That requires to revoke the elevated level from windows on closing a window session
  // This might be the way to go, if FormStyle (such as fsStayOnTop) would come
  // in conflict with modality
  if level <> keepWinLevel then begin
    setLevel(keepWinLevel);
  end;

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

procedure TCocoaWindow.windowDidMiniaturize(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.WindowStateChanged;
end;

procedure TCocoaWindow.windowDidDeminiaturize(notification: NSNotification);
begin
  if Assigned(callback) then
    callback.WindowStateChanged;
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

function TCocoaWindow.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaWindow.canBecomeKeyWindow: LCLObjCBoolean;
begin
  Result := Assigned(callback) and callback.CanActivate;
end;

function TCocoaWindow.becomeFirstResponder: LCLObjCBoolean;
begin
  Result := inherited becomeFirstResponder;
  // uncommenting the following lines starts an endless focus loop

//  if Assigned(callback) then
//    callback.BecomeFirstResponder;
end;

function TCocoaWindow.resignFirstResponder: LCLObjCBoolean;
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
  Epos: NSPoint;
  cr : NSRect;
  fr : NSRect;
  prc: Boolean;
begin
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
    inherited sendEvent(event);
end;

procedure TCocoaWindow.keyDown(event: NSEvent);
var
  mn : NSMenu;
  allowcocoa : Boolean;
begin
  if performKeyEquivalent(event) then
    Exit;

  mn := NSApp.MainMenu;
  if Assigned(mn) and mn.performKeyEquivalent(event) then
    Exit;

  if Assigned(_keyEvCallback) then
  begin
    allowcocoa := True;
    _calledKeyEvAfter := True;
    _keyEvCallback.KeyEvAfterDown(allowcocoa);
    if not allowcocoa then
      Exit;
  end;

  inherited keyDown(event);
end;

function TCocoaWindowContent.draggingEntered(sender: NSDraggingInfoProtocol): NSDragOperation;
begin
  Result := NSDragOperationNone;
  if (wincallback <> nil) and (wincallback.AcceptFilesDrag) then
    Result := sender.draggingSourceOperationMask();
end;

function TCocoaWindowContent.performDragOperation(sender: NSDraggingInfoProtocol): LCLObjCBoolean;
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

  if (Length(lFiles) > 0) and (wincallback <> nil)  then
    wincallback.DropFiles(lFiles);
  Result := True;
end;

procedure TCocoaWindowContent.setNeedsDisplay_(aflag: LCLObjCBoolean);
begin
  inherited setNeedsDisplay;
  if Assigned(overlay) then overlay.setNeedsDisplay_(aflag);
end;

procedure TCocoaWindowContent.setNeedsDisplayInRect(arect: NSRect);
begin
  inherited setNeedsDisplayInRect(arect);
  if Assigned(overlay) then overlay.setNeedsDisplayInRect(arect);
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

procedure NSScreenGetRect(sc: NSScreen; mainScreenHeight: double; out r: TRect);
var
  fr : NSRect;
begin
  fr := sc.frame;
  r := Bounds(
    Round(fr.origin.x),
    Round(fr.origin.y - fr.size.height + mainScreenHeight),
    Round(fr.size.width), Round(fr.size.height)
  );
end;

procedure NSScreenGetRect(sc: NSScreen; out r: TRect);
begin
  NSScreenGetRect(sc, NSScreen.mainScreen.frame.size.height, r);
end;

function GetScreenForPoint(x,y: Integer): NSScreen;
var
  scarr : NSArray;
  sc    : NSScreen;
  r     : TRect;
  h     : double;
  p     : TPoint;
  i     : Integer;
begin
  p.x := x;
  p.y := y;
  scarr := NSScreen.screens;
  h := NSScreen.mainScreen.frame.size.height;
  sc := NSScreen(scarr.objectAtIndex(0));
  for i:=0 to scarr.count-1 do begin
    sc:=NSScreen(scarr.objectAtIndex(i));
    NSScreenGetRect(sc, h, r);
    if Types.PtInRect(r, p) then begin
      Result := sc;
      Exit;
    end;
  end;
  Result := NSScreen.mainScreen;
end;

procedure LCLWindowExtension.lclSetFrame(const r: TRect);
var
  ns : NSRect;
  h  : integer;
  sc : NSScreen;
  srect : NSRect;
begin
  sc := GetScreenForPoint(r.Left, r.Top);
  srect := sc.frame;

  LCLToNSRect(r, srect.size.height, ns);

  // add topbar height
  h:=lclGetTopBarHeight;
  ns.size.height:=ns.size.height+h;
  ns.origin.y:=ns.origin.y-h;
  {$ifdef BOOLFIX}
  setFrame_display_(ns, Ord(isVisible));
  {$else}
  setFrame_display(ns, isVisible);
  {$endif}
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

