{%mainunit gtk2wsprivate.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TGtkPrivateWidget }

class procedure TGtkPrivateWidget.UpdateCursor(AInfo: PWidgetInfo);
var
  Widget, FixWidget: PGtkWidget;
  Window: PGdkWindow;
begin
  Widget := AInfo^.CoreWidget;
  FixWidget := GetFixedWidget(Widget);
  Window := GetControlWindow(FixWidget);
  if Window = nil then Exit;
  // always recurse windows which do not accept controls.
  // this way we will catch all widgets with double windows
  if not (csAcceptsControls in TControl(AInfo^.LCLObject).ControlStyle) then
    SetWindowCursor(Window, AInfo^.ControlCursor, False, True)
  else
    SetCursorForWindowsWithInfo(Window, AInfo, True);
end;

class procedure TGtkPrivateWidget.SetZPosition(const AWinControl: TWinControl; const APosition: TWSZPosition);
var
  Widget: PGtkWidget;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetZPosition')
  then Exit;

  Widget := GetWidgetWithWindow(AWincontrol.Handle);
  if Widget = nil then Exit;
  if Widget^.Window=nil then exit;

  case APosition of
    wszpBack:  begin
      gdk_window_lower(Widget^.Window);
    end;
    wszpFront: begin
      gdk_window_raise(Widget^.Window);
    end;
  end;
end;

{ TGtkPrivatePaned }

class procedure TGtkPrivatePaned.UpdateCursor(AInfo: PWidgetInfo);
var
  Widget: PGtkWidget;
  Window: PGdkWindow;
begin
  Widget := AInfo^.CoreWidget;
  Window := PGTkPaned(Widget)^.handle;
  if Window = nil then Exit;
  SetWindowCursor(Window, AInfo^.ControlCursor, False, True);
end;


{ TGtkPrivateEntry }

class procedure TGtk2PrivateButton.UpdateCursor(AInfo: PWidgetInfo);
var
  Widget: PGtkWidget;
  Window: PGdkWindow;
begin
  Widget := AInfo^.CoreWidget;
  if (Widget = nil) or not GTK_IS_BUTTON(Widget) then Exit;
  Window := PGtkButton(Widget)^.event_window;
  if Window = nil then Exit;
  SetWindowCursor(Window, AInfo^.ControlCursor, False, True);
end;

{ TGtk2PrivateMemo }

class procedure TGtk2PrivateMemo.UpdateCursor(AInfo: PWidgetInfo);
var
  Widget, FixWidget: PGtkWidget;
  Window: PGdkWindow;
begin
  Widget := AInfo^.CoreWidget;
  if (Widget = nil) or not GTK_IS_TEXT_VIEW(Widget) then Exit;
  FixWidget := GetFixedWidget(Widget);
  Window := GetControlWindow(FixWidget);
  if Window = nil then Exit;
  if TControl(AInfo^.LCLObject).Cursor = crDefault then
    SetWindowCursor(Window, 0, True, False)
  else
    SetWindowCursor(Window, AInfo^.ControlCursor, True, False);
end;

class procedure TGtk2PrivateNotebook.UpdateCursor(AInfo: PWidgetInfo);
var
  Widget: PGtkWidget;
  Window: PGdkWindow;

  procedure UpdateCursorInternal(AInfo: PWidgetInfo);
  var
    Widget, FixWidget: PGtkWidget;
    Window: PGdkWindow;
  begin
    Widget := AInfo^.CoreWidget;
    FixWidget := GetFixedWidget(Widget);
    Window := GetControlWindow(FixWidget);
    if Window = nil then Exit;
    // always recurse windows which do not accept controls.
    // this way we will catch all widgets with double windows
    if not (csAcceptsControls in TControl(AInfo^.LCLObject).ControlStyle) then
      SetWindowCursor(Window, AInfo^.ControlCursor, True, True)
    else
      SetCursorForWindowsWithInfo(Window, AInfo, True);
  end;

begin
  if IsTTabControl(AInfo^.CoreWidget) then
  begin
    UpdateCursorInternal(AInfo);
    exit;
  end;
  Widget := AInfo^.CoreWidget;
  Window := PGTkNotebook(Widget)^.event_window;
  if Window <> nil then
    SetWindowCursor(Window, AInfo^.ControlCursor, False, True);
  // do not know how to set cursor under tabs
end;

