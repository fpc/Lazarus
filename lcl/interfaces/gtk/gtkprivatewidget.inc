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
begin
  //move code from gtkproc.inc here

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

