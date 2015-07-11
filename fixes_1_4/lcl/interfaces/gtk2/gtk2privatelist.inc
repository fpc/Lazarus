{%mainunit gtk2wsprivate.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


{ TGtkPrivateList }

class procedure TGtkPrivateList.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
end;

procedure Gtk2WS_ListBoxChange({%H-}Selection: PGtkTreeSelection; WidgetInfo: PWidgetInfo); cdecl;
var
  Mess: TLMessage;
begin
  {$IFDEF EventTrace}
  EventTrace('Gtk2WS_ListBoxChange', WidgetInfo^.LCLObject);
  {$ENDIF}
  FillChar(Mess{%H-}, SizeOf(Mess), 0);
  Mess.msg := LM_SELCHANGE;
  DeliverMessage(WidgetInfo^.LCLObject, Mess);
end;

{ TGtk2PrivateList }

class procedure TGtk2PrivateList.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
var
  Selection: PGtkTreeSelection;
begin
  TGtk2WSBaseScrollingWinControl.SetCallbacks(AGtkWidget, AWidgetInfo);

  Selection := gtk_tree_view_get_selection(PGtkTreeView(AWidgetInfo^.CoreWidget));
  SignalConnect(PGtkWidget(Selection), 'changed', @Gtk2WS_ListBoxChange, AWidgetInfo);
end;

