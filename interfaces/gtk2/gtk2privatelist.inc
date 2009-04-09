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


procedure Gtk2WS_ListBoxChange(Selection: PGtkTreeSelection; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess: TLMessage;
begin
  {$IFDEF EventTrace}
  EventTrace('Gtk2WS_ListBoxChange', WidgetInfo^.LCLObject);
  {$ENDIF}
  FillChar(Mess, SizeOf(Mess), 0);
  Mess.msg := LM_SELCHANGE;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

{ TGtk2PrivateList }

class procedure TGtk2PrivateList.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
var
  Selection: PGtkTreeSelection;
begin
  TGtkWSBaseScrollingWinControl.SetCallbacks(AGtkWidget, AWidgetInfo);

  Selection := gtk_tree_view_get_selection(PGtkTreeView(AWidgetInfo^.CoreWidget));
  SignalConnect(PGtkWidget(Selection), 'changed', @Gtk2WS_ListBoxChange, AWidgetInfo);
end;

