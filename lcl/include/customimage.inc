// included by extctrls.pp

{ TCustomImage

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

constructor TCustomImage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle := csImage;
  ControlStyle:= [csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FStretch := False;
  FTransparent := True;
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
  SetInitialBounds(0,0,100,100);
end;

destructor TCustomImage.Destroy;
begin
  FPicture.OnChange := nil;
  FPicture.Graphic := nil;
  FPicture.Free;
  inherited Destroy;
end;

procedure TCustomImage.SetPicture(const AValue: TPicture);
begin
  if FPicture=AValue then exit;
  FPicture.Assign(AValue); //the OnChange of the picture gets called and
                           // notifies this TCustomImage that something changed.
end;

procedure TCustomImage.DoAutoSize;
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

procedure TCustomImage.SetStretch(Value : Boolean);
begin
  if FStretch=Value then exit;
  FStretch := Value;
  PictureChanged(Self);
end;

procedure TCustomImage.SetTransparent(Value : Boolean);
begin
  if FTransparent=Value then exit;
  FTransparent := Value;
  PictureChanged(Self);
end;

procedure TCustomImage.SetCenter(Value : Boolean);
begin
  if FCenter=Value then exit;
  FCenter := Value;
  PictureChanged(Self);
end;

procedure TCustomImage.SetProportional(const AValue: Boolean);
begin
  if FProportional=AValue then exit;
  FProportional:=AValue;
  PictureChanged(Self);
end;

Procedure TCustomImage.PictureChanged(Sender : TObject);
begin
  If AutoSize then begin
    SetAutoSize(False);
    SetAutoSize(True);
  end;
  Invalidate;
end;

function TCustomImage.DestRect: TRect;
var
  PicWidth: Integer;
  PicHeight: Integer;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: Integer;
  h: Integer;
begin
  PicWidth := Picture.Width;
  PicHeight := Picture.Height;
  ImgWidth := ClientWidth;
  ImgHeight := ClientHeight;
  if Stretch or (Proportional
  and ((PicWidth > ImgWidth) or (PicHeight > ImgHeight))) then begin
    if Proportional and (PicWidth > 0) and (PicHeight > 0) then begin
      w:=ImgWidth;
      h:=(PicHeight*w) div PicWidth;
      if h>ImgHeight then begin
        h:=ImgHeight;
        w:=(PicWidth*h) div PicHeight;
      end;
      PicWidth:=w;
      PicHeight:=h;
    end
    else begin
      PicWidth := ImgWidth;
      PicHeight := ImgHeight;
    end;
  end;

  Result:=Rect(0,0,PicWidth,PicHeight);

  if Center then
    OffsetRect(Result,(ImgWidth-PicWidth) div 2,(ImgHeight-PicHeight) div 2);
end;

Procedure TCustomImage.Paint;

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
    iRect:=DestRect;
    If Picture.Graphic.Transparent and not Transparent then begin
      If Picture.Graphic is TBitmap then
        Brush.Color := TBitmap(Picture.Graphic).TransparentColor
      else
        Brush.Color := clWhite;
      FillRect(iRect);
    end;
    Brush.Color:=clBlue;
    FillRect(iRect);
    StretchDraw(iRect, Picture.Graphic);
  end;

  inherited Paint;
end;

// included by extctrls.pp

