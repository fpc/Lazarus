{%MainUnit gtk1int.pp}

{******************************************************************************
                                   TGtkWidgetSet
 ******************************************************************************

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

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{------------------------------------------------------------------------------
  procedure SetWidgetFont
    AWidget : PGtkWidget; const AFont: TFont
 ------------------------------------------------------------------------------}
procedure TGtk1WidgetSet.SetWidgetFont(const AWidget: PGtkWidget; const AFont: TFont);
var
  WindowStyle: PGtkStyle;
  FontGdiObject: PGdiObject;

begin
  if GtkWidgetIsA(AWidget,GTKAPIWidget_GetType) then begin
    // the GTKAPIWidget is self drawn, so no use to change the widget style.
    exit;
  end;

  if (GTK_WIDGET_REALIZED(AWidget)) then begin
    WindowStyle := gtk_style_copy(gtk_widget_get_style (AWidget));
  end else begin
    WindowStyle := gtk_style_copy(gtk_rc_get_style (AWidget));
  end;
  if (Windowstyle = nil)  then begin
     Windowstyle := gtk_style_new ;
  end;

  FontGdiObject:=PGdiObject(AFont.Handle);
  windowstyle^.font:=pointer(FontGdiObject^.GdiFontObject);
  gtk_widget_set_style(aWidget,windowStyle);
end;
