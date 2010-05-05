{%MainUnit ../dialogs.pp}

{******************************************************************************
                                   TColorButton
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{ TColorButton }

constructor TColorButton.Create(AnOwner: TComponent);
begin
  Inherited Create(AnOwner);
  FButtonColorSize := 16;
  FBorderWidth := 2;
  FButtonColorAutoSize := True;
  FDisabledPattern := nil;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TColorButton.Destroy;
begin
  FreeAndNil(FDisabledPattern);
  inherited Destroy;
end;

procedure TColorButton.SetButtonColor(const AValue: TColor);
begin
  if AValue = FButtonColor then Exit;
  FButtonColor := AValue;
  if Assigned(FOnColorChanged) and (not (csLoading in ComponentState)) then
    FOnColorChanged(Self);
    
  Invalidate;
end;

procedure TColorButton.SetBorderWidth(const AValue: Integer);
begin
  if FBorderWidth = AValue then Exit;
  FBorderWidth := AValue;
  
  if FButtonColorAutoSize then Invalidate;
end;

function TColorButton.IsButtonColorAutoSizeStored: boolean;
begin
  Result := FButtonColorAutoSize = False;
end;

procedure TColorButton.SetButtonColorAutoSize(const AValue: Boolean);
begin
  if FButtonColorAutoSize = AValue then Exit;
  FButtonColorAutoSize := AValue;
  
  Invalidate;
end;

procedure TColorButton.ShowColorDialog;
var
  NewColor: TColor;
  FreeDialog: Boolean;
begin
  if not Enabled then Exit;
  
  FreeDialog := FColorDialog = nil;
  if FColorDialog = nil then FColorDialog := TColorDialog.Create(GetTopParent);
  try
    NewColor := ButtonColor;
    FColorDialog.Color := ButtonColor;
    if FColorDialog.Execute then
      NewColor := FColorDialog.Color;
  finally
    if FreeDialog then FreeAndNil(FColorDialog);
  end;
  ButtonColor := NewColor;
end;

function TColorButton.GetGlyphSize(PaintRect: TRect): TSize;
var
  T: TSize;
  S: Integer;
begin
  if ButtonColorAutoSize then
  begin
    T := GetTextSize(PaintRect);
    if Caption = '' then S := 0
    else S := Spacing;
    
    if Layout in [blGlyphLeft, blGlyphRight] then
    begin
      Result.CX := PaintRect.Right - PaintRect.Left - 2 * BorderWidth - S - T.CX;
      Result.CY := PaintRect.Bottom - PaintRect.Top - 2 * BorderWidth;
    end
    else
    begin
      Result.CX := PaintRect.Right - PaintRect.Left - 2 * BorderWidth;
      Result.CY := PaintRect.Bottom - PaintRect.Top - 2 * BorderWidth - S - T.CY;
    end;
  end
  else
  begin
    Result.CX := ButtonColorSize;
    Result.CY := ButtonColorSize;
  end;
end;

function TColorButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
  BiDiFlags: Longint): TRect;
var
  Size: TSize;
begin
  Canvas.Pen.Color := clBlack;
  if AState = bsDisabled then
  begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Bitmap := GetDisabledPattern;
  end
  else
  begin
    Canvas.Brush.Bitmap := nil;
    Canvas.Brush.Color := ButtonColor;
  end;
  Size := GetGlyphSize(AClient);
  
  Result := Bounds(AClient.Left + AOffset.X, AClient.Top + AOffset.Y,
    Size.CX - 1, Size.CY - 1);
  Canvas.Rectangle(Result);
end;

class function TColorButton.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 25;
end;

procedure TColorButton.SetButtonColorSize(const AValue: Integer);
begin
  if FButtonColorSize = AValue then Exit;
  FButtonColorSize := AValue;
  
  Invalidate;
end;

class procedure TColorButton.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterColorButton;
end;

function TColorButton.GetDisabledPattern: TBitmap;
const
  LineBitsDotted: array[0..7] of Word = ($55, $AA, $55, $AA, $55, $AA, $55, $AA);
begin
  if FDisabledPattern = nil then
  begin
    FDisabledPattern := TBitmap.Create;
    FDisabledPattern.SetHandles(CreateBitmap(8, 8, 1, 1, @LineBitsDotted), 0);
  end;
  Result := FDisabledPattern;
end;

procedure TColorButton.Click;
begin
  inherited Click;
  ShowColorDialog;
end;


// included by buttons.pp
