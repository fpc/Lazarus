{ TImage

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
}

constructor TImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCompStyle := csImage;
  ControlStyle:= [csCaptureMouse, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FStretch := False;
  FTransparent := True;
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
  Setbounds(0,0,100,100);
end;

destructor TImage.Destroy;
begin
  FPicture.OnChange := nil;
  FPicture.Graphic := nil;
  FPicture.Free;
  inherited;
end;

procedure TImage.SetPicture(const AValue: TPicture);
begin
  FPicture.Assign(AValue);  //the onchange of the picture gets called and notifies that something changed.
end;

procedure TImage.DoAutoSize;
var
  ModifyWidth,
  ModifyHeight : Boolean;
begin
  If AutoSize and not AutoSizing then begin
    AutoSizing := True;
    ModifyWidth := (Align = alleft) or (Align = alRight) or (Align = alNone);
    ModifyHeight := (Align = alTop) or (Align = alBottom) or (Align = alNone);
    If ModifyWidth and (Picture.Width > 0) then
      Width := Max(Picture.Width, CONSTRAINTS.MinWidth);
    If ModifyHeight and (Picture.Height > 0) then
      Height := Max(Picture.Height, CONSTRAINTS.MinHeight);
    PictureChanged(Self);
    AutoSizing := False;
  end;
end;

procedure TImage.SetStretch(Value : Boolean);
begin
  FStretch := Value;
  PictureChanged(Self);
end;

procedure TImage.SetTransparent(Value : Boolean);
begin
  FTransparent := Value;
  PictureChanged(Self);
end;

procedure TImage.SetCenter(Value : Boolean);
begin
  FCenter := Value;
  PictureChanged(Self);
end;

Procedure TImage.PictureChanged(Sender : TObject);
begin
  If AutoSize then begin
    SetAutoSize(False);
    SetAutoSize(True);
  end;
  Invalidate;
end;

Procedure TImage.Paint;

  Procedure DrawFrame;
  begin
    if csDesigning in ComponentState then begin
      With Canvas do begin
        Pen.Color := clBlack;
        Pen.Style := psDash;
        MoveTo(0, 0);
        LineTo(Width, 0);
        LineTo(Width, Height);
        LineTo(0, Height);
        LineTo(0, 0);
      end;
    end;
  end;

var
  RGN : longint;
  iRect : TRect;
begin
  With Canvas do begin
    RGN := CreateRectRGN(0, 0, Width + 1, Height + 1);
    SelectClipRGN(Handle, RGN);
    DeleteObject(RGN);
    DrawFrame;
    If Picture.Graphic = nil then
      exit;
    If Stretch then
      iRect := Rect(0, 0, Width + 1, Height + 1)
    else
      iRect := Rect(0,0, Picture.Width, Picture.Height);
    If Center then
      OffsetRect(iRect,
        (Width + 1) div 2 - (iRect.Right - iRect.Left) div 2,
        (Height + 1) div 2 - (iRect.Bottom -iRect.Top) div 2);
    If Picture.Graphic.Transparent and not Transparent then
    begin
      If Picture.Graphic is TBitmap then
        Brush.Color := TBitmap(Picture.Graphic).TransparentColor
      else
        Brush.Color := clWhite;
      FillRect(iRect);
    end;
    StretchDraw(iRect, Picture.Graphic);
  end;
end;
