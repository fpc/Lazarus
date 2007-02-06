{%mainunit gtk2private.pp}

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


class procedure TGtk2PrivateButton.UpdateCursor(AInfo: PWidgetInfo);
var
  Widget: PGtkWidget;
  Window: PGdkWindow;
begin
  Widget := AInfo^.CoreWidget;
  Window := PGTkButton(Widget)^.event_window;
  if Window = nil then Exit;
  SetWindowCursor(Window, AInfo^.ControlCursor, False);
end;
