// included by stdctrls.pp

{******************************************************************************
                               TCustomStaticText
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  TODO:

    - FocusControl
    - Enable Escaped '&' Shortcut to focus FocusControl
    - Compare/Match AutoSize to Delphi/Kylix's
    - ?? Check For Full Delphi/Kylix Compatibility

}
Procedure TCustomStaticText.DoAutoSize;
var
  R : TRect;
  DC : hDC;
begin
  If AutoSizing or not AutoSize then
    Exit;
  if (not HandleAllocated) or (csLoading in ComponentState) then exit;
  AutoSizing := True;
  DC := GetDC(Handle);
  Try
    R := Rect(0,0, Width, Height);
    If BorderStyle <> sbsNone then
      InflateRect(R, -2, -2);
    SelectObject(DC, Font.Handle);
    DrawText(DC, PChar(Caption), Length(Caption), R,
      DT_CalcRect or DT_NoPrefix or DT_WordBreak);
    If BorderStyle <> sbsNone then
      InflateRect(R, 2, 2);

    Width := R.Right - R.Left;
    Height := R.Bottom - R.Top;
  Finally
    ReleaseDC(Handle, DC);
    AutoSizing := False;
  end;
end;

Procedure TCustomStaticText.SetAlignment(Value : TAlignment);
begin
  If FAlignment <> Value then begin
    FAlignment := Value;
    Invalidate;
  end;
end;

Function TCustomStaticText.GetAlignment : TAlignment;
begin
  Result := FAlignment;
end;

Procedure TCustomStaticText.SetBorderStyle(Value : TStaticBorderStyle);
begin
  If FBorderStyle <> Value then begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

Function TCustomStaticText.GetBorderStyle : TStaticBorderStyle;
begin
  Result := FBorderStyle;
end;

Procedure TCustomStaticText.SetShowAccelChar(Value : Boolean);
begin
  If FShowAccelChar <> Value then begin
    FShowAccelChar := Value;
    Invalidate;
  end;
end;

procedure TCustomStaticText.CMTextChanged(var Message: TLMSetText);
begin
  Invalidate;
end;

Function TCustomStaticText.GetShowAccelChar : Boolean;
begin
  Result := FShowAccelChar;
end;

constructor TCustomStaticText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Font.OnChange := @FontChange;
  ControlStyle := [csSetCaption, csOpaque, csClickEvents, csDoubleClicks, csReplicatable];
  Width := 65;
  Height := 17;
end;

Procedure TCustomStaticText.Paint;
var
  TR : TTextStyle;
  R : TRect;
begin
  R := Rect(0,0,Width,Height);
  Canvas.Color := Self.Color;
  Canvas.Font := Self.Font;
  With Canvas do begin
    FillRect(R);
    If BorderStyle <> sbsNone then begin
      InflateRect(R,-2,-2);
      Pen.Style := psSolid;
      If BorderStyle = sbsSunken then
        Pen.Color := clBtnShadow
      else
        Pen.Color := clBtnHighlight;
      MoveTo(0, 0);
      LineTo(Width - 1,0);
      MoveTo(0, 0);
      LineTo(0,Height - 1);
      If BorderStyle = sbsSunken then
        Pen.Color := clBtnHighlight
      else
        Pen.Color := clBtnShadow;
      MoveTo(0,Height - 1);
      LineTo(Width - 1,Height - 1);
      MoveTo(Width - 1, 0);
      LineTo(Width - 1,Height);
    end;
    FillChar(TR,SizeOf(TR),0);
    With TR do begin
      Alignment := Self.Alignment;
      WordBreak := True;
      Clipping := True;
      ShowPrefix := ShowAccelChar;
    end;
    TextRect(R, 0, 0, Caption, TR);
  end;
end;

Procedure TCustomStaticText.FontChange(Sender : TObject);
begin
  If Caption > '' then
    Invalidate;
end;

 // included by stdctrls.pp


{
  $Log$
  Revision 1.1  2002/10/21 15:51:27  lazarus
  AJ: moved TCustomStaticText code to include/customstatictext.inc


}
