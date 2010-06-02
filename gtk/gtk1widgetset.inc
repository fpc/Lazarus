{%MainUnit gtk1int.pp}

{******************************************************************************
                                   TGtkWidgetSet
 ******************************************************************************

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

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

function TGTK1WidgetSet.GetDeviceContextClass: TGtkDeviceContextClass;
begin
  Result := TGtk1DeviceContext;
end;

{------------------------------------------------------------------------------
  procedure SetWidgetFont
    AWidget : PGtkWidget; const AFont: TFont
 ------------------------------------------------------------------------------}
procedure TGtk1WidgetSet.SetWidgetFont(const AWidget: PGtkWidget; const AFont: TFont);
var
  WindowStyle: PGtkStyle;
  FontGdiObject: PGdiObject;

begin
  if GtkWidgetIsA(AWidget,GTKAPIWidget_GetType) then
    Exit; // the GTKAPIWidget is self drawn, so no use to change the widget style.

  if (GTK_WIDGET_REALIZED(AWidget)) then
    WindowStyle := gtk_style_copy(gtk_widget_get_style(AWidget))
  else
    WindowStyle := gtk_style_copy(gtk_rc_get_style(AWidget));
    
  if (Windowstyle = nil) then
    Windowstyle := gtk_style_new;

  FontGdiObject := PGdiObject(AFont.Reference.Handle);
  windowstyle^.font := Pointer(FontGdiObject^.GdiFontObject);
  gtk_widget_set_style(aWidget, windowStyle);
end;

procedure TGtk1WidgetSet.SetLabelCaption(const ALabel: PGtkLabel; const ACaption: String;
  const AComponent: TComponent = nil; const ASignalWidget: PGTKWidget = nil;
  const ASignal: PChar = nil);
var
  Caption, Pattern: String;
  CurText: PChar;
  AccelKey: Char;
begin
  Caption := ACaption;
  LabelFromAmpersands(Caption, Pattern, AccelKey);
  gtk_label_get(ALabel, @CurText);
  if Caption <> StrPas(CurText) then
  begin
    gtk_label_set_text(ALabel, PChar(Caption));
    gtk_label_set_pattern(ALabel, PChar(Pattern));
  end;

  if AComponent = nil then Exit;
  if ASignalWidget = nil then Exit;
  if ASignal = '' then Exit;

  // update the Accelerator
  if AccelKey = #0
  then Accelerate(AComponent, ASignalWidget, GDK_VOIDSYMBOL, 0, ASignal)
  else Accelerate(AComponent, ASignalWidget, Ord(AccelKey), 0, ASignal);
end;
