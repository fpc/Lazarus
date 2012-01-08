{$MainUnit customdrawnwsforms.pp}

{ TCDWSCustomForm }

class function TCDWSCustomForm.DoCreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  win : TCocoaForm;
  winhandle: TCocoaWindow;
  cnt : TCocoaCustomControl;
  ns  : NSString;
  lRect: NSRect;
const
  WinMask = NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask;
begin
  win := TCocoaForm(TCocoaForm.alloc);

  if not Assigned(win) then
  begin
    Result:=0;
    Exit;
  end;

  winhandle := TCocoaWindow.Create;
  winhandle.LCLForm := TCustomForm(AWinControl);

  if Application.ApplicationType = atMobileEmulator then lRect := GetNSRect(200, 200, 240, 320)
  // else ToDo: for atPDA make it fullscreen
  else lRect := CreateParamsToNSRect(AParams);
  win:=TCocoaForm(win.initWithContentRect_styleMask_backing_defer(lRect, WinMask, NSBackingStoreBuffered, False));
  win.WindowHandle := winhandle;
  win.setDelegate(win);
  ns:=NSStringUtf8(AWinControl.Caption);
  win.setTitle(ns);
  ns.release;
  win.setAcceptsMouseMovedEvents(True);

  cnt:=TCocoaCustomControl.alloc.init;
  cnt.WindowHandle := winhandle;
  win.setContentView(cnt);

  winhandle.CocoaForm := win;
  Result := TLCLIntfHandle(winhandle);
end;

class procedure TCDWSCustomForm.DoShowHide(const AWinControl: TWinControl);
var
  win: TCocoaWindow;
begin
  win := TCocoaWindow(AWinControl.Handle);
  if not Assigned(win) then Exit;

  if AWinControl.Visible then
    win.CocoaForm.orderFrontRegardless
  else
    win.CocoaForm.orderOut(nil);
end;

{------------------------------------------------------------------------------
  Method:  TCDWSCustomForm.CreateHandle
  Params:  AWinControl - LCL control
           AParams     - Creation parameters
  Returns: Handle to the window in Cocoa interface

  Creates new window in Cocoa interface with the specified parameters
 ------------------------------------------------------------------------------}
class function TCDWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
begin
  if LCLIntf.IsMobilePlatform() then
  begin
    Result := TLCLIntfhandle(AddNewForm(TCustomForm(AWinControl)));
    if AWinControl = Application.MainForm then
      CDWidgetset.MobileMainForm := DoCreateHandle(AWinControl, AParams);
  end
  else
    Result := DoCreateHandle(AWinControl, AParams);
end;

class procedure TCDWSCustomForm.DestroyHandle(const AWinControl: TWinControl);
begin
end;

class procedure TCDWSCustomForm.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
begin
  if AWinControl.Handle=0 then Exit;
  {todo: setFrame_display(, true)? }
  //sf:=NSScreen.mainScreen.frame;
  TCocoaWindow(AWinControl.Handle).CocoaForm.lclSetFrame(Bounds(ALeft, ATop, AWidth, AHeight));

  //LCLToCocoaRect( GetNSRect(ALeft,ATop,AWidth,AHeight), sf, wf);
  //NSWindow(AWinControl.Handle).setFrame_display(wf, false);
  //NSWindow(AWinControl.Handle).setFrame_display( GetNSRect(ALeft,ATop, AWidth, AHeight), false);
  //NSWindow(AWinControl.Handle).setFrameTopLeftPoint( GetNSPoint(ALeft, ATop));
end;

class procedure TCDWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
                               const ABorderIcons: TBorderIcons);
begin
end;

class procedure TCDWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
                         const AFormBorderStyle: TFormBorderStyle);
begin
end;

class procedure TCDWSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
begin
end;

class procedure TCDWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar);
begin
end;

class procedure TCDWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
end;

class procedure TCDWSCustomForm.ShowHide(const AWinControl: TWinControl);
begin
  DoShowHide(AWinControl);
end;

class function TCDWSCustomForm.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
var
  win   : TCocoaWindow;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  Result:=Assigned(win);
  if not Result then Exit;
  AText:=NSStringToString(win.CocoaForm.title);
  Result:=true;
end;

class function TCDWSCustomForm.GetTextLen(const AWinControl: TWinControl; var ALength: Integer): Boolean;
var
  win   : TCocoaWindow;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  Result:=Assigned(win);
  if not Result then Exit;
  ALength:=win.CocoaForm.title.length;
end;

class procedure TCDWSCustomForm.SetText(const AWinControl: TWinControl; const AText: String);
var
  win : TCocoaWindow;
  ns  : NSString;
begin
  win:=TCocoaWindow(AWinControl.Handle);
  if not Assigned(win) then Exit;
  ns:=NSStringUtf8(AText);
  win.CocoaForm.setTitle(ns);
  ns.release;
end;

class function TCDWSCustomForm.GetClientBounds(const AWinControl: TWinControl; var ARect: TRect): Boolean;
begin
  Result:=AWinControl.Handle<>0;
  if not Result then Exit;
  ARect:= TCocoaWindow(AWinControl.Handle).CocoaForm.lclClientFrame;
end;

class function TCDWSCustomForm.GetClientRect(const AWinControl: TWinControl; var ARect: TRect): Boolean;
var
  x,y : Integer;
begin
  Result:=AWinControl.Handle<>0;
  if not Result then Exit;
  ARect:= TCocoaWindow(AWinControl.Handle).CocoaForm.lclClientFrame;
  x:=0;y:=0;
  TCocoaWindow(AWinControl.Handle).CocoaForm.lclLocalToScreen(x,y);
  MoveRect(ARect, x,y);
end;

