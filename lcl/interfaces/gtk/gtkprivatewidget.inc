{%mainunit gtkprivate.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  SetWindowCursor(Window, AInfo^.ControlCursor, not (csAcceptsControls in TControl(AInfo^.LCLObject).ControlStyle));
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

