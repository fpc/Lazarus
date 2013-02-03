{%MainUnit ../comctrls.pp}

{******************************************************************************
                                  TCoolBar
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
const
  GrabLeft = 2;
  GrabWidth = 9;
  ControlLeft = GrabLeft + GrabWidth + 6;

{ TCoolBand }

constructor TCoolBand.Create(aCollection: TCollection);
begin
  inherited Create(aCollection);
  Assert(aCollection is TCoolBands, 'TCoolBand.Create: aCollection is not TCoolBands');
  FCoolBar := TCoolBands(aCollection).FCoolBar;
  Width := 100;
  FBreak := True;
  FColor := clBtnFace;
  FFixedBackground := True;
  FImageIndex := -1;
  FMinHeight := 25;
  FParentColor := True;
  FParentBitmap := True;
  FBitmap := TBitmap.Create;
  FVisible := True;
end;

destructor TCoolBand.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TCoolBand.GetWidth: Integer;
begin
  Result := FCoolBar.Width;
end;

function TCoolBand.GetText: string;
begin
  if Assigned(FTextLabel) then
    Result := FTextLabel.Caption
  else
    Result := '';
end;

function TCoolBand.IsBitmapStored: Boolean;
begin
  Result := not ParentBitmap;
end;

function TCoolBand.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

function TCoolBand.GetHeight: Integer;
begin
  if Assigned(FControl) then
    Result := FControl.Height
  else
    Result := 20;
end;

function TCoolBand.GetVisible: Boolean;
begin
  Result := FVisible and not (FCoolBar.Vertical and FHorizontalOnly);
end;

procedure TCoolBand.ResetControlProps;
begin
  FControl.AnchorSide[akLeft].Control := Nil;
  FControl.BorderSpacing.Left := 0;
  FControl.Left := ControlLeft;
end;

procedure TCoolBand.SetBorderStyle(aValue: TBorderStyle);
begin
  if FBorderStyle = aValue then Exit;
  FBorderStyle := aValue;
  Changed(False);
end;

procedure TCoolBand.SetBreak(aValue: Boolean);
begin
  if FBreak = aValue then Exit;
  FBreak := aValue;
  Changed(False);
end;

procedure TCoolBand.SetFixedSize(aValue: Boolean);
begin
  if FFixedSize = aValue then Exit;
  FFixedSize := aValue;
  if FFixedSize then
    FBreak := False;
  Changed(FFixedSize);
end;

procedure TCoolBand.SetMinHeight(aValue: Integer);
begin
  if FMinHeight = aValue then Exit;
  FMinHeight := aValue;
  Changed(False);
end;

procedure TCoolBand.SetMinWidth(aValue: Integer);
begin
  // No operation currently. Client's width is used for band's width
end;

procedure TCoolBand.SetVisible(aValue: Boolean);
begin
  if FVisible = aValue then Exit;
  FVisible := aValue;
  Changed(True);
end;

procedure TCoolBand.SetHorizontalOnly(aValue: Boolean);
begin
  if FHorizontalOnly = aValue then Exit;
  FHorizontalOnly := aValue;
  Changed(FCoolBar.Vertical);
end;

procedure TCoolBand.SetImageIndex(aValue: TImageIndex);
begin
  if FImageIndex = aValue then Exit;
  FImageIndex := aValue;
  Changed(False);
end;

procedure TCoolBand.SetFixedBackground(aValue: Boolean);
begin
  if FFixedBackground = aValue then Exit;
  FFixedBackground := aValue;
  Changed(False);
end;

procedure TCoolBand.SetColor(aValue: TColor);
begin
  if FColor = aValue then Exit;
  FColor := aValue;
  FParentColor := False;
  Changed(False);
end;

procedure TCoolBand.SetControlWidth;
begin
  if not (FControl is TCustomCheckBox) then
    FControl.Width := FCoolBar.Width - FControl.Left - 6;
end;

procedure TCoolBand.UpdControl;
begin
  if FCoolBar = Nil then Exit;
  if Assigned(FTextLabel) then
  begin
    if Assigned(FControl) then
      FTextLabel.Top := FTop+4 // Adjust text position for the control (which is higher).
    else
      FTextLabel.Top := FTop+1;
    FTextLabel.Left := ControlLeft;
    FTextLabel.Visible := FCoolBar.ShowText;
  end;
  if Assigned(FControl) then
  begin
    //DebugLn('TCoolBand.UpdControl');
    // Calculate left positions and anchoring for text label and control
    FControl.Align := alNone;
    FControl.Parent := FCoolBar;
    FControl.FreeNotification(FCoolBar);
    FControl.Top := FTop;
    if Assigned(FTextLabel) and FCoolBar.ShowText then
    begin
      FControl.AnchorSide[akLeft].Control := FTextLabel;
      FControl.AnchorSide[akLeft].Side := asrRight;
      FControl.BorderSpacing.Left := 7;
    end
    else
      ResetControlProps;
    // Make sure other Anchors a Nil
    FControl.AnchorSide[akRight].Control := Nil;
    FControl.AnchorSide[akBottom].Control := Nil;
    FControl.AnchorSide[akTop].Control := Nil;
    SetControlWidth;
  end;
end;

procedure TCoolBand.SetControl(aValue: TControl);
var
  Band: TCoolBand;
begin
  if FControl = aValue then Exit;
  FCoolBar.BeginUpdate;
  if Assigned(aValue) then begin
    Band := TCoolBands(Collection).FindBand(aValue);
    if Assigned(Band) and (Band <> Self) then begin
      DebugLn(['TCoolBand.SetControl, removing old, FTop=', Band.FTop,
        ', FControl.Name=', Band.FControl.Name, ', FControl.ClassName=', Band.FControl.ClassName]);
      Band.ResetControlProps;
      Band.SetControl(nil);        // Remove old association
    end;
    DebugLn(['TCoolBand.SetControl, adding new, aValue.Name=', aValue.Name, ', aValue.ClassName=', aValue.ClassName]);
    aValue.Parent := Nil;
  end;
  FControl := aValue;
  FCoolBar.EndUpdate;
  Changed(True);
end;

procedure TCoolBand.SetParentColor(aValue: Boolean);
begin
  if FParentColor = aValue then Exit;
  FParentColor := aValue;
  Changed(False);
end;

procedure TCoolBand.SetParentBitmap(aValue: Boolean);
begin
  if FParentBitmap = aValue then Exit;
  FParentBitmap := aValue;
end;

procedure TCoolBand.SetBitmap(aValue: TBitmap);
begin
  FParentBitmap := False;
  FBitmap.Assign(aValue);
  Changed(True);
end;

procedure TCoolBand.SetText(const aValue: string);
begin
  if aValue <> '' then
  begin
    if FTextLabel = Nil then
    begin
      FTextLabel := TLabel.Create(FCoolBar);
      FTextLabel.Parent := FCoolBar;
      FTextLabel.Name := Format('TextLabel%d', [Index]);
      FTextLabel.AutoSize := True;
      FTextLabel.FreeNotification(FCoolBar);
    end
    else if FTextLabel.Caption = aValue then Exit;
    FTextLabel.Caption := aValue;
  end
  else begin
    if Assigned(FTextLabel) then
      FreeAndNil(FTextLabel);
  end;
  Changed(True);
end;

procedure TCoolBand.SetWidth(aValue: Integer);
begin
  // No operation currently
end;

function TCoolBand.GetDisplayName: string;
begin
  Result := Text;
  if Result = '' then
    Result := ClassName;
end;

procedure TCoolBand.SetIndex(aValue: Integer);
begin
  inherited SetIndex(aValue);
end;

procedure TCoolBand.Assign(aSource: TPersistent);
var
  src: TCoolBand;
  SrcCtrl: TWinControl;
begin
  if aSource is TCoolBand then
  begin
    src := TCoolBand(aSource);
    Bitmap          := src.Bitmap;
    Break           := src.Break;
    Color           := src.Color;
    FixedBackground := src.FixedBackground;
    FixedSize       := src.FixedSize;
    HorizontalOnly  := src.HorizontalOnly;
    ImageIndex      := src.ImageIndex;
    MinHeight       := src.MinHeight;
    MinWidth        := src.MinWidth;
    ParentBitmap    := src.ParentBitmap;
    ParentColor     := src.ParentColor;
    Text            := src.Text;
    Visible         := src.Visible;
//    Width           := src.Width;
    SrcCtrl := Nil;
    if Assigned(src.Control) then
      SrcCtrl := FCoolBar.Owner.FindComponent(src.Control.Name) as TWinControl;
    Control         := SrcCtrl;
  end
  else
    inherited Assign(aSource);
end;

{ TCoolBands }

constructor TCoolBands.Create(aCoolBar: TCustomCoolBar);
begin
  inherited Create(TCoolBand);
  FCoolBar := aCoolBar;
end;

function TCoolBands.GetItem(Index: Integer): TCoolBand;
begin
  Result := TCoolBand(inherited GetItem(Index));
end;

procedure TCoolBands.SetItem(Index: Integer; aValue: TCoolBand);
begin
  inherited SetItem(Index, aValue);
end;

function TCoolBands.GetOwner: TPersistent;
begin
  Result := FCoolBar;
end;

procedure TCoolBands.Update(aItem: TCollectionItem);
var
  i: Integer;
begin
  inherited Update(aItem);
  if FCoolBar = Nil then Exit;
  if csDestroying in FCoolBar.ComponentState then Exit;
  if FCoolBar.FUpdateCount = 0 then
    UpdControls;    // Calculate control positions
end;

procedure TCoolBands.Notify(aItem: TCollectionItem; aAction: TCollectionNotification);
begin
  inherited Notify(aItem, aAction);
  case aAction of
    cnAdded: begin
      DebugLn('TCoolBands.Notify: aAction = cnAdded');
    end;
    cnExtracting: begin
      DebugLn('TCoolBands.Notify: aAction = cnExtracting');
    end;
    cnDeleting: begin
      DebugLn('TCoolBands.Notify: aAction = cnDeleting');
    end;
  end;
end;

function TCoolBands.Add: TCoolBand;
begin
  Result := TCoolBand(inherited Add);
  DebugLn('TCoolBands.Add');
end;

function TCoolBands.FindBand(aControl: TControl): TCoolBand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if GetItem(i).FControl = AControl then
    begin
      Result := GetItem(i);
      Exit;
    end;
  end;
end;

procedure TCoolBands.UpdControls;
var
  i, hh, yy: Integer;
  Band: TCoolBand;
begin
  yy := 3;
  for i := 0 to Count-1 do
  begin
    Band := Items[i];
    hh := Band.Height;
    if FCoolBar.BandBorderStyle = bsSingle then
      Inc(hh, 2);
    Band.FTop := yy;
    Band.UpdControl;   // Set control's location
    Inc(yy, hh+3);
  end;
end;

{ TCustomCoolBar }

constructor TCustomCoolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csOpaque, csDoubleClicks];
  DragMode := dmAutomatic;
  Height := 75;
  Align := alTop;
  ParentColor := True;
  ParentFont := True;
  FBandBorderStyle := bsSingle;
  FBandMaximize := bmClick;
  FBands := TCoolBands.Create(Self);
  FBitmap := TBitmap.Create;
  FShowText := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
end;

destructor TCustomCoolBar.Destroy;
begin
  FImageChangeLink.Free;
  FBitmap.Free;
  FBands.Free;
  inherited Destroy;
end;

function TCustomCoolBar.GetAlign: TAlign;
begin
  Result := inherited Align;
end;

procedure TCustomCoolBar.SetAlign(aValue: TAlign);
var
  Old: TAlign;
begin
  Old := inherited Align;
  inherited Align := aValue;
  if (csReading in ComponentState) or (aValue = Old) then Exit;
  if aValue in [alLeft, alRight] then
    Vertical := True
  else if aValue in [alTop, alBottom] then
    Vertical := False;
end;

procedure TCustomCoolBar.SetBands(aValue: TCoolBands);
begin
  FBands.Assign(aValue);
end;

procedure TCustomCoolBar.SetBitmap(aValue: TBitmap);
begin
  FBitmap.Assign(aValue);
end;

procedure TCustomCoolBar.SetImages(aValue: TCustomImageList);
begin
  if Assigned(FImages) then
    FImages.UnRegisterChanges(FImageChangeLink);
  FImages := aValue;
  if Assigned(FImages) then
  begin
    FImages.RegisterChanges(FImageChangeLink);
    FImages.FreeNotification(Self);
  end;
  Invalidate;
end;

procedure TCustomCoolBar.SetShowText(aValue: Boolean);
begin
  if FShowText = aValue then Exit;
  FShowText := aValue;
  if not (csLoading in ComponentState) then
    FBands.Update(Nil);
end;

procedure TCustomCoolBar.SetVertical(aValue: Boolean);
begin
  if FVertical = aValue then Exit;
  Invalidate;
end;

procedure TCustomCoolBar.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TCustomCoolBar.AlignControls(aControl: TControl; var aRect: TRect);
begin
  //DebugLn('TCoolBar.AlignControls');
  if FUpdateCount = 0 then
  begin
    FBands.UpdControls;
    inherited AlignControls(aControl, aRect);
  end;
end;

procedure TCustomCoolBar.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if csDestroying in ComponentState then Exit;
  if Operation = opRemove then
  begin
    DebugLn('TCoolBar.Notification: Operation = opRemove');
    if AComponent = FImages then
      Images := nil;
  end;
end;

procedure TCustomCoolBar.InsertControl(AControl: TControl; Index: integer);
var
  Band: TCoolBand;
begin
  inherited InsertControl(AControl, Index);
  if (AControl is TWinControl) and not (csLoading in ComponentState) then
  begin
    Band := Bands.FindBand(AControl);
    if Band = Nil then
    begin
      DebugLn('TCoolBar.InsertControl: Adding band for Comp=' + AControl.Name + ', class=' + AControl.ClassName);
      Band := FBands.Add;
      Band.Control := AControl;
    end;
  end;
end;

procedure TCustomCoolBar.RemoveControl(AControl: TControl);
var
  Band: TCoolBand;
begin
  Band := Bands.FindBand(AControl);
  if Assigned(Band) then
    Band.FControl := nil;
  inherited RemoveControl(AControl);
end;

procedure TCustomCoolBar.Loaded;
begin
  inherited Loaded;
  DebugLn('TCoolBar.Loaded');
  FBands.Update(Nil);
end;

procedure TCustomCoolBar.Paint;

  procedure PaintGrabber(aRect: TRect);
  begin
    Canvas.Pen.Color := clBtnHighlight;
    Canvas.MoveTo(aRect.Left+2, aRect.Top);
    Canvas.LineTo(aRect.Left, aRect.Top);
    Canvas.LineTo(aRect.Left, aRect.Bottom+1);
    Canvas.Pen.Color := clBtnShadow;
    Canvas.MoveTo(aRect.Right, aRect.Top);
    Canvas.LineTo(aRect.Right, aRect.Bottom);
    Canvas.LineTo(aRect.Left, aRect.Bottom);
  end;

var
  i, BottomY: Integer;
begin
  inherited Paint;
  //DebugLn('TCoolBar.Paint');
  for i := 0 to FBands.Count-1 do
  begin
    BottomY := FBands[i].FTop+FBands[i].Height+2;
    // Paint a grabber
    PaintGrabber(Rect(GrabLeft, FBands[i].FTop, GrabLeft+GrabWidth, BottomY-1));
    // Paint a separator border below the band.
    if FBandBorderStyle = bsSingle then
    begin
      Canvas.Line(3, BottomY, Width-3, BottomY);
      Canvas.Pen.Color := clBtnHighlight;
      Canvas.Line(3, BottomY+1, Width-3, BottomY+1);
    end;
  end;
end;

procedure TCustomCoolBar.Resize;
var
  i: Integer;
begin
  inherited Resize;
  if [csLoading, csDestroying] * ComponentState <> [] then Exit;
  if not Assigned(FBands) then Exit;
  for i := 0 to FBands.Count-1 do
    if Assigned(FBands[i].FControl) then
      FBands[i].SetControlWidth;
end;

