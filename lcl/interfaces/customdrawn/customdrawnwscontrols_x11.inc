{$MainUnit customdrawnwscontrols.pp}

(*class procedure TCDWSWinControl.AddControl(const AControl: TControl);
{var
  ParentPanelHandle, ParentHandle, ChildHandle: HWND;}
begin
{  with TWinControl(AControl) do
  begin
    //DebugLn(Format('Trace:[TCDWSWinControl.AddControl] %S --> Calling Add Child: %S', [Parent.ClassName, ClassName]));
    ParentHandle := Parent.Handle;
    ChildHandle := Handle;
  end;

  //DebugLn('Trace:AddControl - Parent Window Handle is $' + IntToHex(LongInt(ParentHandle), 8));
  //DebugLn('Trace:AddControl - Child Window Handle is $' + IntToHex(LongInt(ChildHandle), 8));
  // handle groupbox exception
  ParentPanelHandle := GetWindowInfo(ChildHandle)^.ParentPanel;
  if ParentPanelHandle <> 0 then
    ChildHandle := ParentPanelHandle;
  SetParent(ChildHandle, ParentHandle);}
end; *)

class function  TCDWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  AText := '';
  Result := false;
end;

class procedure TCDWSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  RecreateWnd(AWinControl);
end;

class procedure TCDWSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
var
  AfterWnd: hWnd;
  n, StopPos: Integer;
  Child: TWinControl;
begin
{  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition')
  then Exit;
  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition (child)')
  then Exit;

  if ANewPos = 0 // bottom
  then AfterWnd := HWND_BOTTOM
  else if ANewPos >= AChildren.Count - 1
  then AfterWnd := HWND_TOP
  else begin
    // Search for the first child above us with a handle
    // the child list is reversed form the windows order.
    // So the first window is the top window and is the last child
    // if we don't find a allocated handle then we are effectively not moved
    AfterWnd := 0;
    if AOldPos > ANewPos
    then StopPos := AOldPos              // The child is moved to the bottom, oldpos is on top of it
    else StopPos := AChildren.Count - 1; // the child is moved to the top

    for n := ANewPos + 1 to StopPos do
    begin
      Child := TWinControl(AChildren[n]);
      if Child.HandleAllocated
      then begin
        AfterWnd := Child.Handle;
        Break;
      end;
    end;

    if AfterWnd = 0 then Exit; // nothing to do
  end;
  Windows.SetWindowPos(AChild.Handle, AfterWnd, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or
    SWP_NOSIZE or SWP_NOSENDCHANGING);        }
end;

{------------------------------------------------------------------------------
  Method:  SetBounds
  Params:  AWinControl                  - the object which invoked this function
           ALeft, ATop, AWidth, AHeight - new dimensions for the control
  Pre:     AWinControl.HandleAllocated
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
class procedure TCDWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  IntfLeft, IntfTop, IntfWidth, IntfHeight: integer;
  Handle: HWND;
begin
{  IntfLeft := ALeft;
  IntfTop := ATop;
  IntfWidth := AWidth;
  IntfHeight := AHeight;
  LCLBoundsToWin32Bounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight);
  {$IFDEF VerboseSizeMsg}
  DebugLn('TWin32WSWinControl.ResizeWindow A ', dbgsName(AWinControl),
    ' LCL=',Format('%d, %d, %d, %d', [ALeft,ATop,AWidth,AHeight]),
    ' Win32=',Format('%d, %d, %d, %d', [IntfLeft,IntfTop,IntfWidth,IntfHeight])
    );
  {$ENDIF}
  suppressMove := False;
  AdaptBounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight, suppressMove);
  if not suppressMove then
  begin
    Handle := AWinControl.Handle;
    WindowPlacement.length := SizeOf(WindowPlacement);
    if IsIconic(Handle) and GetWindowPlacement(Handle, @WindowPlacement) then
    begin
      WindowPlacement.rcNormalPosition := Bounds(IntfLeft, IntfTop, IntfWidth, IntfHeight);
      SetWindowPlacement(Handle, @WindowPlacement);
    end
    else
      Windows.SetWindowPos(Handle, 0, IntfLeft, IntfTop, IntfWidth, IntfHeight, SWP_NOZORDER or SWP_NOACTIVATE);
  end;
  LCLControlSizeNeedsUpdate(AWinControl, True);     }
end;

class procedure TCDWSWinControl.SetColor(const AWinControl: TWinControl);
begin

end;

class procedure TCDWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
//  Windows.SendMessage(AWinControl.Handle, WM_SETFONT, Windows.WParam(AFont.Reference.Handle), 1);
end;

class procedure TCDWSWinControl.SetText(const AWinControl: TWinControl; const AText: string);
begin
//  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;

//  Windows.SetWindowTextW(AWinControl.Handle, PWideChar(UTF8Decode(AText)));
end;

class procedure TCDWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin

end;

class function TCDWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
begin
end;

class procedure TCDWSWinControl.DestroyHandle(const AWinControl: TWinControl);
begin
end;

class procedure TCDWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  // lpRect = nil updates entire client area of window
//  InvalidateRect(AWinControl.Handle, nil, true);
end;

class procedure TCDWSWinControl.ShowHide(const AWinControl: TWinControl);
//const
//  VisibilityToFlag: array[Boolean] of UINT = (SWP_HIDEWINDOW, SWP_SHOWWINDOW);
begin
//  Windows.SetWindowPos(AWinControl.Handle, 0, 0, 0, 0, 0,
//    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or VisibilityToFlag[AWinControl.HandleObjectShouldBeVisible])
end;

