{%mainunit gtk1wsprivate.pp}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TGtk1PrivateList }

class procedure TGtk1PrivateList.SetCallbacks(const AGtkWidget: PGtkWidget; const AWidgetInfo: PWidgetInfo);
begin
  TGtkWSWinControl.SetCallbacks(PGtkObject(AGtkWidget), TComponent(AWidgetInfo^.LCLObject));
  GTKWidgetSet.SetCallback(LM_SELCHANGE, PGtkObject(AGtkWidget), AWidgetInfo^.LCLObject);
end;


