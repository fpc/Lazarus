{%MainUnit gtk2def.pp}

{******************************************************************************
                                   TGtk2DeviceContext
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

{ TGtk2DeviceContext }

function TGtk2DeviceContext.GetFunction: TGdkFunction;
begin
  Result := GCValues._function;
end;

constructor TGtk2DeviceContext.Create;
begin
  inherited Create;
{$ifdef gtk2_improve_TextOut_speed}
  FillChar(OldText, SizeOf(OldText), 0);
{$endif}
end;

procedure TGtk2DeviceContext.SetText(AFont: PPangoLayout; AText: PChar; ALength: LongInt);
begin
{$ifdef gtk2_improve_TextOut_speed}
  if (OldText.Font <> AFont) or (OldText.Len <> ALength) or (CompareChar(AText^, PChar(OldText.Text)^, ALength) <> 0) then
  begin
    pango_layout_set_text(AFont, AText, ALength);
    OldText.Font := AFont;
    OldText.Len := ALength;
    SetLength(OldText.Text, ALength);
    Move(AText^, PChar(OldText.Text)^, ALength);
  end;
{$else}
  pango_layout_set_text(AFont, AText, ALength);
{$endif}
end;

procedure TGtk2DeviceContext.DrawTextWithColors(AText: PChar; ALength: LongInt;
  X, Y: Integer; FGColor, BGColor: PGdkColor);
var
  WidgetCont: PPangoContext;
  NewMatrix: TPangoMatrix;
  OldMatrix: PPangoMatrix;
  renderer: PPangoRenderer;
  AFont: PGdiObject;
  
  procedure SetColors(AFGColor, ABGColor: PGdkColor); inline;
  begin
    gdk_pango_renderer_set_override_color(renderer, PANGO_RENDER_PART_FOREGROUND, AFGColor);
    gdk_pango_renderer_set_override_color(renderer, PANGO_RENDER_PART_UNDERLINE, AFGColor);
    gdk_pango_renderer_set_override_color(renderer, PANGO_RENDER_PART_STRIKETHROUGH, AFGColor);
    gdk_pango_renderer_set_override_color(renderer, PANGO_RENDER_PART_BACKGROUND, ABGColor);
  end;
  
begin
  AFont := GetFont;
  SetText(AFont^.GDIFontObject, AText, ALength);

  if AFont^.LogFont.lfEscapement <> 0 then
  begin
    renderer := gdk_pango_renderer_get_default(gtk_widget_get_screen(Widget));
    gdk_pango_renderer_set_drawable(renderer, drawable);
    gdk_pango_renderer_set_gc(renderer, GC);
    SetColors(FGColor, BGColor);

    WidgetCont := pango_layout_get_context(AFont^.GDIFontObject);
    OldMatrix := pango_context_get_matrix(WidgetCont);
    NewMatrix.xx := 1.0;
    NewMatrix.xy := 0.0;
    NewMatrix.yx := 0.0;
    NewMatrix.yy := 1.0;
    NewMatrix.x0 := 0.0;
    NewMatrix.y0 := 0.0;
    pango_matrix_translate(@NewMatrix, X, Y);
    pango_matrix_rotate(@NewMatrix, AFont^.LogFont.lfEscapement div 10);

    pango_context_set_matrix(WidgetCont, @NewMatrix);
    pango_layout_context_changed(AFont^.GDIFontObject);
    pango_renderer_draw_layout(renderer, AFont^.GDIFontObject, X, Y);

    //now reset
    pango_context_set_matrix(WidgetCont, OldMatrix);
    pango_layout_context_changed(AFont^.GDIFontObject);
    
    SetColors(nil, nil);
    gdk_pango_renderer_set_drawable(renderer, nil);
    gdk_pango_renderer_set_gc(renderer, nil);
  end
  else
    gdk_draw_layout_with_colors(drawable, GC, X, Y, AFont^.GDIFontObject, FGColor, BGColor);
end;


