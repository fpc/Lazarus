{%mainunit gtk2wsprivate.pp}

{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  Widget := AInfo^.ClientWidget;
  if Widget = nil then Exit;
  Window := PGtkButton(Widget)^.event_window;
  if Window = nil then Exit;
  SetWindowCursor(Window, AInfo^.ControlCursor, False);
end;

class procedure TGtk2PrivateNotebook.UpdateCursor(AInfo: PWidgetInfo);
var
  Widget: PGtkWidget;
  Window: PGdkWindow;
begin
  Widget := AInfo^.CoreWidget;
  Window := PGTkNotebook(Widget)^.event_window;
  if Window <> nil then
    SetWindowCursor(Window, AInfo^.ControlCursor, False);
  // dont know how to set cursor under tabs
end;

