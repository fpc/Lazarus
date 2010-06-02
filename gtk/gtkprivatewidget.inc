{%mainunit gtkwsprivate.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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
    SetWindowCursor(Window, AInfo^.ControlCursor, True)
  else
    SetCursorForWindowsWithInfo(Window, AInfo);
end;

class procedure TGtkPrivateWidget.SetDefaultCursor(AInfo: PWidgetInfo);
begin
  AInfo^.DefaultCursor := Screen.Cursors[crDefault];
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
  SetWindowCursor(Window, AInfo^.ControlCursor, False);
end;


{ TGtkPrivateEntry }

class procedure TGtkPrivateEntry.SetDefaultCursor(AInfo: PWidgetInfo);
begin
  AInfo^.DefaultCursor := Screen.Cursors[crIBeam];
end;

