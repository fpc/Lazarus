{$MainUnit customdrawnwsforms.pp}

{ TCDWSCustomForm }

{------------------------------------------------------------------------------
  Method:  TCDWSCustomForm.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Cocoa interface

  Creates new window in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}

class function TCDWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  win : TCocoaWindow;
  cnt : TCocoaCustomControl;
  ns  : NSString;
const
  WinMask = NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask;
begin
  win := TCocoaWindow(TCocoaWindow.alloc);

  if not Assigned(win) then begin
    Result:=0;
    Exit;
  end;

  win:=TCocoaWindow(win.initWithContentRect_styleMask_backing_defer(CreateParamsToNSRect(AParams), WinMask, NSBackingStoreBuffered, False));
  TCocoaWindow(win).callback:=TLCLCommonCallback.Create(win, AWinControl);
  TCocoaWindow(win).wincallback:=TLCLWindowCallback.Create(win, AWinControl);
  win.setDelegate(win);
  ns:=NSStringUtf8(AWinControl.Caption);
  win.setTitle(ns);
  ns.release;
  win.setAcceptsMouseMovedEvents(True);

  cnt:=TCocoaCustomControl.alloc.init;
  cnt.callback:=TCocoaWindow(win).callback;
  win.setContentView(cnt);

  Result := TLCLIntfHandle(win);
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  win : NSWindow;
begin
  win:=NSWindow(AWinControl.Handle);
  if not Assigned(win) then Exit;

  if AWinControl.Visible then
    win.orderFrontRegardless
  else
    win.orderOut(nil);
end;

class function TCDWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  win   : TCocoaWindow;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  Result:=Assigned(win);
  if not Result then Exit;
  AText:=NSStringToString(win.title);
  Result:=true;
end;

class function TCDWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  win   : TCocoaWindow;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  Result:=Assigned(win);
  if not Result then Exit;
  ALength:=win.title.length;
end;

class procedure TCDWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  win : TCocoaWindow;
  ns  : NSString;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  if not Assigned(win) then Exit;
  ns:=NSStringUtf8(AText);
  win.setTitle(ns);
  ns.release;
end;

class function TCDWSCustomForm.GetClientBounds(const AWinControl: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=AWinControl.Handle<>0;
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
end;

class function TCDWSCustomForm.GetClientRect(const AWinControl: TWinControl; var ARect: TRect): Boolean;
var
  x,y : Integer;
begin
  Result:=AWinControl.Handle<>0;
  if not Result then Exit;
  ARect:=NSObject(AWinControl.Handle).lclClientFrame;
  x:=0;y:=0;
  NSObject(AWinControl.Handle).lclLocalToScreen(x,y);
  MoveRect(ARect, x,y);
end;

class procedure TCDWSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if AWinControl.Handle=0 then Exit;
  {todo: setFrame_display(, true)? }
  //sf:=NSScreen.mainScreen.frame;
  NSObject(AWinControl.Handle).lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));

  //LCLToCocoaRect( GetNSRect(ALeft,ATop,AWidth,AHeight), sf, wf);
  //NSWindow(AWinControl.Handle).setFrame_display(wf, false);
  //NSWindow(AWinControl.Handle).setFrame_display( GetNSRect(ALeft,ATop, AWidth, AHeight), false);
  //NSWindow(AWinControl.Handle).setFrameTopLeftPoint( GetNSPoint(ALeft, ATop));
end;

