{%MainUnit ../extctrls.pp}

{ TCustomImage

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

constructor TCustomImage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:= [csCaptureMouse, csClickEvents, csDoubleClicks];
  AutoSize := False;
  FCenter := False;
  FStretch := False;
  FTransparent := False;
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
  FUseParentCanvas := false;
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
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
  if not FUseParentCanvas and (FPicture.Graphic = nil) then
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
  //debugln('TCustomImage.GetCanvas B ',DbgSName(Self),' ',DbgSName(FPicture.Graphic));
  // try draw on the bitmap, not on the form's canvas
  if not FUseParentCanvas and (FPicture.Graphic is TBitmap) then
    Result := TBitmap(FPicture.Graphic).Canvas
  else
    Result := inherited Canvas;
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
  NewWidth: Integer;
  NewHeight: Integer;
  
  procedure OutOfBounds;
  begin
    DebugLn('TCustomImage.DoAutoSize NewWidth=',dbgs(NewWidth),
      ' NewHeight=',dbgs(NewHeight),
      ' ModifyWidth='+dbgs(ModifyWidth),
      ' Picture.Width='+dbgs(Picture.Width),
      ' ModifyHeight='+dbgs(ModifyHeight),
      ' Picture.Height='+dbgs(Picture.Height)+
      '');
    RaiseGDBException('');
  end;
  
begin
  if AutoSizing then Exit;    // we shouldn't come here in the first place
  
  BeginAutoSizing;
  try
    GetPreferredSize(NewWidth, NewHeight);
    ModifyWidth := [akLeft,akRight]*(Anchors+AnchorAlign[Align])<>[akLeft,akRight];
    ModifyHeight := [akTop,akBottom]*(Anchors+AnchorAlign[Align])<>[akTop,akBottom];

    if not ModifyWidth then NewWidth := Width;
    if not ModifyHeight then NewHeight := Height;
    
    if (NewWidth>100000) or (NewHeight>100000) then
      OutOfBounds;

    if (NewWidth<>Width) or (NewHeight<>Height)
    then begin
      SetBounds(Left, Top, NewWidth, NewHeight);
    end;
  finally
    EndAutoSizing;
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
  if  (FPicture.Graphic is TBitmap)
  and (FPicture.Graphic.Transparent <> FTransparent)
  then FPicture.Graphic.Transparent := FTransparent
  else PictureChanged(Self);
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

procedure TCustomImage.PictureChanged(Sender : TObject);
begin
  if Picture.Graphic <> nil
  then begin
    if AutoSize
    then begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
    if FTransparent
    then Picture.Graphic.Transparent := True;
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

procedure TCustomImage.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := Picture.Width;
  PreferredHeight := Picture.Height;
end;

class function TCustomImage.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=90;
  Result.Y:=90;
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
  BackgroundColor: Integer;
begin
  // detect loop
  if FUseParentCanvas then exit;

  if csDesigning in ComponentState
  then DrawFrame;
  
  if Picture.Graphic = nil
  then Exit;

  if Picture.Graphic.Transparent and not Transparent
  then begin
    if Picture.Graphic is TBitmap
    then BackgroundColor := TBitmap(Picture.Graphic).TransparentColor
    else BackgroundColor := clWhite;
  end
  else begin
    BackgroundColor := clNone;
  end;

  C := inherited Canvas;
  R := DestRect;
  if BackgroundColor <> clNone
  then begin
    C.Brush.Color := BackgroundColor;
    C.FillRect(R);
  end;
  C.StretchDraw(R, Picture.Graphic);

  try
    FUseParentCanvas := True;
    inherited Paint;
  finally
    FUseParentCanvas := False;
  end;
end;

// included by extctrls.pp
