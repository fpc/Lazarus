{%MainUnit ../stdctrls.pp}
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

{$IFNDef NewCheckBox}


{$ENDIF}

{$IFDef NewCheckBox}
procedure TCheckbox.DoAutoSize;
var
  R : TRect;
  DC : hDC;
begin
  If AutoSizing or not AutoSize then
    Exit;
  if (not HandleAllocated) or ([csLoading,csDestroying]*ComponentState<>[]) then
    exit;
  AutoSizing := True;
  DC := GetDC(Handle);
  Try
    R := Rect(0,0, Width, Height);
    DrawText(DC, PChar(Caption), Length(Caption), R,
      DT_CalcRect or DT_NOPrefix);
    If R.Right > Width then
      Width := R.Right + 25;
    If R.Bottom > Height then
      Height := R.Bottom + 2;
  Finally
    ReleaseDC(Handle, DC);
    AutoSizing := False;
  end;
end;

function TCheckBox.GetChecked : Boolean;
begin
  Result := (State = cbChecked);
end;

procedure TCheckBox.SetChecked(Value : Boolean);
begin
  If Value then
    State := cbChecked
  else
    State := cbUnchecked
end;

procedure TCheckBox.SetCheckBoxStyle(Value : TCheckBoxStyle);
begin
  FCheckBoxStyle := Value;
  Invalidate;
end;

procedure TCheckBox.SetAttachTextToBox(Value : Boolean);
begin
  FAttachTextToBox := Value;
  Invalidate;
end;

procedure TCheckbox.SetAlignment(Value : TCBAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TCheckbox.SetState(Value : TCheckBoxState);
begin
  If Value = cbGrayed then begin
    If AllowGrayed then
      FState := Value
    else
      FState := cbUnchecked;
  end
  else
    FState := Value;
  Invalidate;
end;

procedure TCheckbox.CMMouseEnter(var Message: TLMMouse);
begin
  if not MouseInControl
  and Enabled and (GetCapture = 0)
  then begin
    FMouseInControl := True;
    Invalidate;
  end;
end;

procedure TCheckbox.CMMouseLeave(var Message: TLMMouse);
begin
  if MouseInControl
  and Enabled and (GetCapture = 0)
  and not MouseIsDragging
  then begin
    FMouseInControl := False;
    Invalidate;
  end;
end;

procedure TCheckbox.WMMouseDown(var Message : TLMMouseEvent);
begin
  if Enabled then
    If not MouseInControl then
      FMouseInControl := True;
  if MouseInControl and Enabled then begin
    FMouseIsDragging := True;
    Invalidate;
  end;
end;

procedure TCheckbox.WMMouseUp(var Message : TLMMouseEvent);
begin
  If MouseInControl and Enabled then begin
    FMouseIsDragging := False;
    Case State of
      cbUnchecked :
       begin
          If AllowGrayed then
            State := cbGrayed
          else
            State := cbChecked;
        end;
      cbGrayed :
        State := cbChecked;
      cbChecked :
        State := cbUnchecked;
    end;
    Click;
  end;
end;

procedure TCheckbox.WMKeyDown(var Message : TLMKeyDown);
begin
  Exclude(FControlState, csClicked);
  Case Message.CharCode of
    32:
      begin
        FMouseInControl := True;
        Invalidate;
      end;
    27:
      If MouseInControl then begin
        FMouseInControl := False;
        Invalidate;
      end;
  end;
  Message.Result := 1
end;

procedure TCheckbox.WMKeyUp(var Message : TLMKeyUp);
begin
  Case Message.CharCode of
    32:
      begin
        If MouseInControl then begin
          FMouseInControl := False;
          Case State of
            cbUnchecked :
              begin
                If AllowGrayed then
                  State := cbGrayed
                else
                  State := cbChecked;
              end;
            cbGrayed :
              State := cbChecked;
            cbChecked :
              State := cbUnchecked;
          end;
          Click;
        end;
      end;
  end;
  Message.Result := 1
end;

procedure TCheckBox.PaintCheck(var PaintRect: TRect);

  procedure DrawBorder(Highlight, Shadow : TColor; Rect : TRect; Down : Boolean);
  begin
    With Canvas, Rect do begin
      Pen.Style := psSolid;
      If Down then
        Pen.Color := shadow
      else
        Pen.Color := Highlight;
      MoveTo(Left, Top);
      LineTo(Right - 1,Top);
      MoveTo(Left, Top);
      LineTo(Left,Bottom - 1);
      If Down then
        Pen.Color := Highlight
      else
        Pen.Color := shadow;
      MoveTo(Left,Bottom - 1);
      LineTo(Right - 1,Bottom - 1);
      MoveTo(Right - 1, Top);
      LineTo(Right - 1,Bottom);
    end;
  end;

var
  FD1, FD2 : TPoint;
  BD1, BD2 : TPoint;
  APaintRect : TRect;
  DrawFlags : Longint;
begin
  If CheckBoxStyle <> cbsSystem then begin
    If (State = cbGrayed) or (not Enabled) then begin
      If (MouseInControl and MouseIsDragging) or (not Enabled) then
        Canvas.Brush.Color := clBtnFace
      else
        Canvas.Brush.Color := clBtnHighlight;
      Canvas.FillRect(CheckBoxRect);
      Canvas.Pen.Color := clBtnShadow;
    end
    else begin
      If MouseInControl and MouseIsDragging then
        Canvas.Brush.Color := clBtnFace
      else
        Canvas.Brush.Color := clWindow;
      Canvas.FillRect(CheckBoxRect);
      Canvas.Pen.Color := clWindowText;
    end;
    If State <> cbUnchecked then begin
      Case CheckBoxStyle of
        cbsCrissCross:
          begin
            Canvas.Pen.Width := 1;

            {Backward Diagonal}
              BD1 := Point(CheckBoxRect.Left + 3,CheckBoxRect.Top + 3);
              BD2 := Point(CheckBoxRect.Right - 3,CheckBoxRect.Bottom - 3);

              Canvas.MoveTo(BD1.X + 1, BD1.Y);
              Canvas.LineTo(BD2.X, BD2.Y - 1);{Top Line}
              Canvas.MoveTo(BD1.X, BD1.Y);
              Canvas.LineTo(BD2.X, BD2.Y);{Center Line}
              Canvas.MoveTo(BD1.X, BD1.Y + 1);
              Canvas.LineTo(BD2.X - 1, BD2.Y);{Bottom Line}

            {Forward Diagonal}
              FD1 := Point(CheckBoxRect.Left + 3,CheckBoxRect.Bottom - 4);
              FD2 := Point(CheckBoxRect.Right - 3,CheckBoxRect.Top + 2);

              Canvas.MoveTo(FD1.X, FD1.Y - 1);
              Canvas.LineTo(FD2.X - 1, FD2.Y);{Top Line}
              Canvas.MoveTO(FD1.X, FD1.Y);
              Canvas.LineTo(FD2.X, FD2.Y);{Center Line}
              Canvas.MoveTo(FD1.X + 1, FD1.Y);
              Canvas.LineTo(FD2.X, FD2.Y + 1);{Bottom Line}

            Canvas.Pen.Width := 0;
          end;
        cbsCheck:
          begin
            Canvas.Pen.Width := 1;

            {Short Diagonal}
              BD1 := Point(CheckBoxRect.Left + 4,CheckBoxRect.Bottom - 8);
              BD2 := Point(CheckBoxRect.Left + 4,CheckBoxRect.Bottom - 5);

              Canvas.MoveTO(BD1.X - 1, BD1.Y);
              Canvas.LineTo(BD2.X - 1, BD2.Y);{Left Line}
              Canvas.MoveTo(BD1.X, BD1.Y + 1);
              Canvas.LineTo(BD2.X, BD2.Y + 1);{Right Line}

            {Long Diagonal}
              FD1 := Point(CheckBoxRect.Left + 5,CheckBoxRect.Bottom - 6);
              FD2 := Point(CheckBoxRect.Right - 3,CheckBoxRect.Top + 2);

              Canvas.MoveTo(FD1.X,FD1.Y);
              Canvas.LineTo(FD2.X, FD2.Y);{Top Line}
              Canvas.MoveTo(FD1.X, FD1.Y + 1);
              Canvas.LineTo(FD2.X, FD2.Y + 1);{Center Line}
              Canvas.MoveTo(FD1.X, FD1.Y + 2);
              Canvas.LineTo(FD2.X, FD2.Y + 2);{Bottom Line}

            Canvas.Pen.Width := 0;
          end;
      end;
    end;
    DrawBorder(clBtnHighlight, clBtnShadow, CheckBoxRect, True);
    InflateRect(APaintRect, -1, -1);
    DrawBorder(clBtnFace, clBlack, APaintRect, True);
  end
  else begin
    DrawFlags:=DFCS_BUTTONPUSH + DFCS_FLAT;
    If MouseInControl and Enabled then
      Inc(DrawFlags,DFCS_CHECKED);
    DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);

    DrawFlags:=DFCS_BUTTONCHECK;
    if Checked or (State = cbGrayed) then inc(DrawFlags,DFCS_PUSHED);
    if not Enabled then inc(DrawFlags,DFCS_INACTIVE);
    If MouseInControl and Enabled then
      Inc(DrawFlags,DFCS_CHECKED);

    APaintRect := CheckBoxRect;
    DrawFrameControl(Canvas.Handle, APaintRect, DFC_BUTTON, DrawFlags);
  end;
end;

procedure TCheckBox.PaintText(var PaintRect: TRect);
var
  Sz : Integer;
  AR : TRect;
  dish, dis : TColor;

  procedure DoDrawText(theRect : TRect);
  var
    TextStyle : TTextStyle;
  begin
    With TextStyle do begin
      Layout     := tlCenter;
      SingleLine := False;
      Clipping   := True;
      ExpandTabs := False;
      ShowPrefix := False;
      Wordbreak  := Wordwrap;
      Opaque     := False;
      SystemFont := CheckBoxStyle = cbsSystem;
    end;

    Case Alignment of
      alLeftJustify:
        begin
          If not FAttachTextToBox then begin
            TextStyle.Alignment  := taLeftJustify;
          end
          else
            TextStyle.Alignment  := taRightJustify;
        end;
      alRightJustify:
        begin
          If not FAttachTextToBox then begin
            TextStyle.Alignment  := taRightJustify;
          end
          else
            TextStyle.Alignment  := taLeftJustify;
        end;
    end;
    Canvas.TextRect(theRect, ARect.Left, ARect.Top, Caption, TextStyle);
  end;

  procedure DoDisabledTextRect(Rect : TRect; Highlight, Shadow : TColor);
  var
    FC : TColor;
  begin
    FC := Canvas.Font.Color;
    Canvas.Font.Color := Highlight;
    OffsetRect(Rect, 1, 1);
    DoDrawText(Rect);
    Canvas.Font.Color := Shadow;
    OffsetRect(Rect, -1, -1);
    DoDrawText(Rect);
    Canvas.Font.Color := FC;
  end;

begin
  If Caption = '' then
    exit;
  Sz := CheckBoxRect.Right - CheckBoxRect.Left;
  AR.Top := PaintRect.Top;
  AR.Bottom := PaintRect.Bottom;
  If Alignment = alRightJustify then begin
    AR.Left := PaintRect.Left + Sz + 6;
    AR.Right := PaintRect.Right;
  end
  else begin
    AR.Left := PaintRect.Left;
    AR.Right := PaintRect.Right - Sz - 6;
  end;
  dish := clBtnHighlight;
  dis := clBtnShadow;
  Canvas.Font := Self.Font;
  If Enabled then begin
    If CheckBoxStyle = cbsSystem then
      Canvas.Font.Color := clBtnText;
    DoDrawText(AR)
  end
  else
    DoDisabledTextRect(AR,dish,dis);
end;

procedure TCheckbox.Paint;
var
  PaintRect: TRect;
begin
  PaintRect := Rect(0, 0, Width, Height);
  Canvas.Color := clBtnFace;

  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);
  PaintCheck(PaintRect);
  PaintText(PaintRect);
end;

Constructor TCheckbox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  controlstyle := controlstyle - [csAcceptsControls];
  Alignment := alRightJustify;
  FAttachTextToBox := True
end;

function TCheckBox.CheckBoxRect : TRect;
var
  Sz : Integer;
begin
  Sz := 13;
  Result.Top := (Height div 2) - (Sz div 2);
  Result.Bottom := Result.Top + Sz;
  If Alignment = alRightJustify then begin
    Result.Left := 2;
    Result.Right := Result.Left + Sz;
  end
  else begin
    Result.Right := Width - 2;
    Result.Left := Result.Right - Sz;
  end;
end;

procedure TCheckBox.Click;
begin
  If Assigned(OnClick) then
    OnClick(Self);
end;
{$EndIf NewCheckbox}

// included by stdctrls.pp

