{%MainUnit ../extctrls.pp}

{ TCustomImage

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

constructor TCustomImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:= [csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FKeepOriginXWhenClipped := False;
  FKeepOriginYWhenClipped := False;
  FProportional := False;
  FStretch := False;
  FStretchOutEnabled := True;
  FStretchInEnabled := True;
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
  if not FUseAncestorCanvas and (FPicture.Graphic is TCustomBitmap) then
    Result := TCustomBitmap(FPicture.Graphic).Canvas
  else
    Result := inherited Canvas;
end;

procedure TCustomImage.SetAntialiasingMode(AValue: TAntialiasingMode);
begin
  if FAntialiasingMode = AValue then Exit;
  FAntialiasingMode := AValue;
  PictureChanged(Self);
end;

procedure TCustomImage.SetKeepOriginX(AValue: Boolean);
begin
  if FKeepOriginXWhenClipped=AValue then Exit;
  FKeepOriginXWhenClipped:=AValue;
  PictureChanged(Self);
end;

procedure TCustomImage.SetKeepOriginY(AValue: Boolean);
begin
  if FKeepOriginYWhenClipped=AValue then Exit;
  FKeepOriginYWhenClipped:=AValue;
  PictureChanged(Self);
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

procedure TCustomImage.SetStretchInEnabled(AValue: Boolean);
begin
  if FStretchInEnabled = AValue then Exit;
  FStretchInEnabled := AValue;
  PictureChanged(Self);
end;

procedure TCustomImage.SetStretchOutEnabled(AValue: Boolean);
begin
  if FStretchOutEnabled = AValue then Exit;
  FStretchOutEnabled := AValue;
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

procedure TCustomImage.SetImageIndex(const AImageIndex: Integer);
begin
  if FImageIndex = AImageIndex then Exit;
  FImageIndex := AImageIndex;
  PictureChanged(Self);
end;

procedure TCustomImage.SetImages(const AImages: TCustomImageList);
begin
  if FImages = AImages then Exit;
  FImages := AImages;
  PictureChanged(Self);
end;

procedure TCustomImage.SetImageWidth(const AImageWidth: Integer);
begin
  if FImageWidth = AImageWidth then Exit;
  FImageWidth := AImageWidth;
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
  if HasGraphic then
  begin
    if AutoSize then
    begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
    if Assigned(Picture.Graphic) then
      Picture.Graphic.Transparent := FTransparent;
  end;
  Invalidate;
  if Assigned(OnPictureChanged) then
    OnPictureChanged(Self);
end;

function TCustomImage.DestRect: TRect;
var
  PicSize: TSize;
  PicWidth: Integer absolute PicSize.Width;
  PicHeight: Integer absolute PicSize.Height;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: Integer;
  h: Integer;
  ChangeX, ChangeY: Integer;
  PicInside, PicOutside, PicOutsidePartial: boolean;
begin
  if Assigned(Picture.Graphic) then
    PicSize := TSize.Create(Picture.Width, Picture.Height)
  else
  if Assigned(Images) then
    PicSize := Images.SizeForPPI[ImageWidth, Font.PixelsPerInch]
  else
    Exit(TRect.Create(0, 0, 0, 0));
  ImgWidth := ClientWidth;
  ImgHeight := ClientHeight;
  if (PicWidth=0) or (PicHeight=0) then Exit(Rect(0, 0, 0, 0));

  PicInside := (PicWidth<ImgWidth) and (PicHeight<ImgHeight);
  PicOutside := (PicWidth>ImgWidth) and (PicHeight>ImgHeight);
  PicOutsidePartial := (PicWidth>ImgWidth) or (PicHeight>ImgHeight);

  if Stretch or (Proportional and PicOutsidePartial) then
    if (FStretchOutEnabled or PicOutsidePartial) and
       (FStretchInEnabled or PicInside) then
      if Proportional then begin
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

  Result := Rect(0, 0, PicWidth, PicHeight);

  if Center then
  begin
    ChangeX := (ImgWidth-PicWidth) div 2;
    ChangeY := (ImgHeight-PicHeight) div 2;
    if FKeepOriginXWhenClipped and (ChangeX<0) then ChangeX := 0;
    if FKeepOriginYWhenClipped and (ChangeY<0) then ChangeY := 0;
    Types.OffsetRect(Result, ChangeX, ChangeY);
  end;
end;

procedure TCustomImage.Invalidate;
begin
  if FPainting then exit;
  inherited Invalidate;
end;

procedure TCustomImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  S: TSize;
begin
  if Assigned(Picture.Graphic) then
  begin
    PreferredWidth := Picture.Width;
    PreferredHeight := Picture.Height;
  end else
  if Assigned(Images) then
  begin
    S := Images.SizeForPPI[ImageWidth, Font.PixelsPerInch];
    PreferredWidth := S.Width;
    PreferredHeight := S.Height;
  end;
end;

class function TCustomImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;

function TCustomImage.GetHasGraphic: Boolean;
begin
  Result := Assigned(Picture.Graphic) or (Assigned(Images) and (ImageIndex>=0));
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
  
  if not HasGraphic then
    Exit;

  C := inherited Canvas;
  R := DestRect;
  C.AntialiasingMode := FAntialiasingMode;
  FPainting:=true;
  try
    if Assigned(FOnPaintBackground) then
      FOnPaintBackground(Self, C, R);

    if Assigned(Picture.Graphic) then
      C.StretchDraw(R, Picture.Graphic)
    else
    if Assigned(Images) and (ImageIndex>=0) then
      Images.StretchDraw(C, ImageIndex, R);
  finally
    FPainting:=false;
  end;

  FUseAncestorCanvas := True;
  try
    inherited Paint;
  finally
    FUseAncestorCanvas := False;
  end;
end;

// included by extctrls.pp
