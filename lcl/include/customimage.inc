{%MainUnit ../extctrls.pp}

{ TCustomImage

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

constructor TCustomImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= [csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FProportional := False;
  FStretch := False;
  FTransparent := False;
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
  FUseAncestorCanvas := False;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomImage.Destroy;
begin
  FPicture.OnChange := nil;
  FPicture.Graphic := nil;
  FPicture.Free;
  inherited Destroy;
end;

function TCustomImage.GetCanvas: TCanvas;
var
  TempBitmap: TBitmap;
begin
  //debugln('TCustomImage.GetCanvas A ',DbgSName(Self),' ',DbgSName(FPicture.Graphic));
  if not FUseAncestorCanvas and (FPicture.Graphic = nil) then
  begin
    // make a new bitmap to draw on
    TempBitmap := TBitmap.Create;
    try
      TempBitmap.Width := Width;
      TempBitmap.Height := Height;
      FPicture.Graphic := TempBitmap;
    finally
      TempBitmap.Free;
    end;
  end;
  //debugln(['TCustomImage.GetCanvas B ',DbgSName(Self),' ',DbgSName(FPicture.Graphic),' FUseParentCanvas=',FUseAncestorCanvas]);
  // try draw on the bitmap, not on the form's canvas
  if not FUseAncestorCanvas and (FPicture.Graphic is TBitmap) then
    Result := TBitmap(FPicture.Graphic).Canvas
  else
    Result := inherited Canvas;
end;

procedure TCustomImage.SetPicture(const AValue: TPicture);
begin
  if FPicture=AValue then exit;
  //the OnChange of the picture gets called and
  // notifies this TCustomImage that something changed.
  FPicture.Assign(AValue);
end;

procedure TCustomImage.SetStretch(const AValue : Boolean);
begin
  if FStretch = AValue then exit;
  FStretch := AValue;
  PictureChanged(Self);
end;

procedure TCustomImage.SetTransparent(const AValue : Boolean);
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  if (FPicture.Graphic <> nil) and (FPicture.Graphic.Transparent <> FTransparent)
  then FPicture.Graphic.Transparent := FTransparent
  else PictureChanged(Self);
end;

class procedure TCustomImage.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomImage;
end;

procedure TCustomImage.SetCenter(const AValue : Boolean);
begin
  if FCenter = AValue then exit;
  FCenter := AValue;
  PictureChanged(Self);
end;

procedure TCustomImage.SetProportional(const AValue: Boolean);
begin
  if FProportional = AValue then exit;
  FProportional := AValue;
  PictureChanged(Self);
end;

procedure TCustomImage.PictureChanged(Sender : TObject);
begin
  if Picture.Graphic <> nil
  then begin
    if AutoSize
    then begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
    Picture.Graphic.Transparent := FTransparent;
  end;
  invalidate;
  if Assigned(OnPictureChanged) then
    OnPictureChanged(Self);
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

procedure TCustomImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := Picture.Width;
  PreferredHeight := Picture.Height;
end;

class function TCustomImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;

procedure TCustomImage.Paint;

  procedure DrawFrame;
  begin
    with inherited Canvas do
    begin
      Pen.Color := clBlack;
      Pen.Style := psDash;
      MoveTo(0, 0);
      LineTo(Self.Width-1, 0);
      LineTo(Self.Width-1, Self.Height-1);
      LineTo(0, Self.Height-1);
      LineTo(0, 0);
    end;
  end;

var
  R: TRect;
  C: TCanvas;
begin
  // detect loop
  if FUseAncestorCanvas then exit;

  if csDesigning in ComponentState
  then DrawFrame;
  
  if Picture.Graphic = nil
  then Exit;

  C := inherited Canvas;
  R := DestRect;
  C.StretchDraw(R, Picture.Graphic);

  FUseAncestorCanvas := True;
  try
    inherited Paint;
  finally
    FUseAncestorCanvas := False;
  end;
end;

// included by extctrls.pp
