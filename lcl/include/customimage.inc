{%MainUnit ../extctrls.pp}

{ TCustomDrawImage

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

constructor TCustomDrawImage.Create(AOwner: TComponent);
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
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

procedure TCustomDrawImage.SetKeepOriginX(AValue: Boolean);
begin
  if FKeepOriginXWhenClipped=AValue then Exit;
  FKeepOriginXWhenClipped:=AValue;
  PictureChanged(Self);
end;

procedure TCustomDrawImage.SetKeepOriginY(AValue: Boolean);
begin
  if FKeepOriginYWhenClipped=AValue then Exit;
  FKeepOriginYWhenClipped:=AValue;
  PictureChanged(Self);
end;

procedure TCustomDrawImage.SetStretch(const AValue : Boolean);
begin
  if FStretch = AValue then exit;
  FStretch := AValue;
  PictureChanged(Self);
end;

procedure TCustomDrawImage.SetStretchInEnabled(AValue: Boolean);
begin
  if FStretchInEnabled = AValue then Exit;
  FStretchInEnabled := AValue;
  PictureChanged(Self);
end;

procedure TCustomDrawImage.SetStretchOutEnabled(AValue: Boolean);
begin
  if FStretchOutEnabled = AValue then Exit;
  FStretchOutEnabled := AValue;
  PictureChanged(Self);
end;

procedure TCustomDrawImage.SetCenter(const AValue : Boolean);
begin
  if FCenter = AValue then exit;
  FCenter := AValue;
  PictureChanged(Self);
end;

procedure TCustomDrawImage.SetProportional(const AValue: Boolean);
begin
  if FProportional = AValue then exit;
  FProportional := AValue;
  PictureChanged(Self);
end;

procedure TCustomDrawImage.PictureChanged(Sender : TObject);
begin
  Invalidate;
  if Assigned(OnPictureChanged) then
    OnPictureChanged(Self);
end;

function TCustomDrawImage.DestRect: TRect;
var
  PicWidth: Integer;
  PicHeight: Integer;
  ImgWidth: Integer;
  ImgHeight: Integer;
  w: Integer;
  h: Integer;
  ChangeX, ChangeY: Integer;
  PicInside, PicOutside, PicOutsidePartial: boolean;
begin
  PicWidth := GetPictureSize.Width;
  PicHeight := GetPictureSize.Height;
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
    OffsetRect(Result, ChangeX, ChangeY);
  end;
end;

procedure TCustomDrawImage.Invalidate;
begin
  if FPainting then exit;
  inherited Invalidate;
end;

class function TCustomDrawImage.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 90;
  Result.CY := 90;
end;

procedure TCustomDrawImage.Paint;

  procedure DrawFrame;
  begin
    with Canvas do
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
begin
  if csDesigning in ComponentState
  then DrawFrame;
  
  R := DestRect;
  FPainting:=true;
  try
    if Assigned(FOnPaintBackground) then
      FOnPaintBackground(Self, Canvas, R);
    PaintImage(R);
  finally
    FPainting:=false;
  end;

  inherited Paint;
end;

{ TCustomImage }

constructor TCustomImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
end;

procedure TCustomImage.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := Picture.Width;
  PreferredHeight := Picture.Height;
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
  //debugln('TCustomDrawImage.GetCanvas A ',DbgSName(Self),' ',DbgSName(FPicture.Graphic));
  if (FPicture.Graphic = nil) then
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
  //debugln(['TCustomDrawImage.GetCanvas B ',DbgSName(Self),' ',DbgSName(FPicture.Graphic),' FUseParentCanvas=',FUseAncestorCanvas]);
  // try draw on the bitmap, not on the form's canvas
  if (FPicture.Graphic is TBitmap) then
    Result := TBitmap(FPicture.Graphic).Canvas
  else
    Result := inherited Canvas;
end;

function TCustomImage.GetPictureSize: TSize;
begin
  Result := TSize.Create(Picture.Width, Picture.Height);
end;

procedure TCustomImage.Paint;
begin
  inherited Canvas.AntialiasingMode := FAntialiasingMode;
  inherited Paint;
end;

procedure TCustomImage.PaintImage(const ARect: TRect);
begin
  if Assigned(Picture.Graphic) then
    inherited Canvas.StretchDraw(ARect, Picture.Graphic);
end;

procedure TCustomImage.PictureChanged(Sender: TObject);
begin
  if Picture.Graphic <> nil then
  begin
    if AutoSize then
    begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
    Picture.Graphic.Transparent := FTransparent;
  end;
  inherited;
end;

procedure TCustomImage.SetAntialiasingMode(AValue: TAntialiasingMode);
begin
  if FAntialiasingMode = AValue then Exit;
  FAntialiasingMode := AValue;
  PictureChanged(Self);
end;

procedure TCustomImage.SetPicture(const AValue: TPicture);
begin
  if FPicture=AValue then exit;
  //the OnChange of the picture gets called and
  // notifies this TCustomDrawImage that something changed.
  FPicture.Assign(AValue);
end;

procedure TCustomImage.SetTransparent(const AValue: Boolean);
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

{ TCustomImageListImage }

procedure TCustomImageListImage.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
end;

function TCustomImageListImage.GetPictureSize: TSize;
begin
  Result := Images.SizeForPPI[ImageWidth, Font.PixelsPerInch];
end;

procedure TCustomImageListImage.PaintImage(const ARect: TRect);
begin
  if Assigned(Images) and (ImageIndex>=0) then
    Images.StretchDraw(Canvas, ImageIndex, ARect);
end;

procedure TCustomImageListImage.SetImageIndex(const AImageIndex: Integer);
begin
  if FImageIndex = AImageIndex then Exit;
  FImageIndex := AImageIndex;
  PictureChanged(Self);
end;

procedure TCustomImageListImage.SetImages(const AImages: TCustomImageList);
begin
  if FImages = AImages then Exit;
  FImages := AImages;
  PictureChanged(Self);
end;

procedure TCustomImageListImage.SetImageWidth(const AImageWidth: Integer);
begin
  if FImageWidth = AImageWidth then Exit;
  FImageWidth := AImageWidth;
  PictureChanged(Self);
end;

// included by extctrls.pp
