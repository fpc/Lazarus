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
  ControlPosLeft = 5;

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
  FControl.Width := FCoolBar.Width - FControl.Left - 6;
end;

procedure TCoolBand.UpdControl;
begin
  if Assigned(FTextLabel) then
  begin
    if Assigned(FControl) then
      FTextLabel.Top := FTop+3 // Adjust text position for the control (which is higher).
    else
      FTextLabel.Top := FTop+1;
    FTextLabel.Left := ControlPosLeft;
    FTextLabel.Visible := FCoolBar.ShowText;
  end;
  if Assigned(FControl) then
  begin
    DebugLn('TCoolBand.UpdControl');
    // Calculate left positions and anchoring for text label and control
    FControl.Align:=alNone;
    FControl.Parent := FCoolBar;
    FControl.FreeNotification(FCoolBar);
    FControl.Top := FTop;
    if Assigned(FTextLabel) and FCoolBar.ShowText then
    begin
      FControl.AnchorSide[akLeft].Control := FTextLabel;
      FControl.AnchorSide[akLeft].Side := asrRight;
      FControl.BorderSpacing.Left := 7;
    end
    else begin
      FControl.AnchorSide[akLeft].Control := Nil;
      FControl.BorderSpacing.Left := 0;
      FControl.Left := ControlPosLeft;
    end;
    // Make sure other Anchors a Nil
    FControl.AnchorSide[akRight].Control := Nil;
    FControl.AnchorSide[akBottom].Control := Nil;
    FControl.AnchorSide[akTop].Control := Nil;
    SetControlWidth;
  end;
end;

procedure TCoolBand.CalcTop(var aTop: Integer);
// aTop is a cumulative value, set by the caller.
var
  hh: Integer;
begin
  hh := Height;
  Assert(Assigned(FCoolBar), 'TCoolBand.CalcTop: FCoolBar is not assigned');
  if FCoolBar.BandBorderStyle = bsSingle then
    Inc(hh, 2);
  FTop := aTop;
  Inc(aTop, hh+3);
end;

procedure TCoolBand.SetControl(aValue: TControl);
var
  Band: TCoolBand;
begin
  if FControl = aValue then Exit;
  DebugLn('TCoolBand.SetControl');
  if Assigned(aValue) then begin
    Band := TCoolBands(Collection).FindBand(aValue);
    if Assigned(Band) and (Band <> Self) then
      Band.SetControl(nil);
  end;
  FControl := aValue;
  Changed(True);
//  FCoolBar.Invalidate;
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
      FTextLabel.Free;
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
  i, yy: Integer;
begin
  inherited Update(aItem);
  if FCoolBar = Nil then Exit;
  if csDestroying in FCoolBar.ComponentState then Exit;
  // Calculate left positions and anchoring for text and control
  if Assigned(aItem) then begin          // For defined item only
    DebugLn(['TCoolBands.Update, Item Index', TCoolBand(aItem).Index]);
    TCoolBand(aItem).UpdControl;
  end
  else begin                             // For all items
    DebugLn('TCoolBands.Update, Item=Nil');
    for i := 0 to Count-1 do
      Items[i].UpdControl;
  end;
  // Top positions for controls must be a loop because y-position is cumulative.
  yy := 3;
  for i := 0 to Count-1 do
  begin
    Items[i].CalcTop(yy);
    if Assigned(aItem) and (Items[i] = aItem) then Break;
  end;
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
    if GetItem(i).FControl = AControl then begin
      Result := GetItem(i);
      Exit;
    end;
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

procedure TCustomCoolBar.Notification(AComponent: TComponent; Operation: TOperation);
var
  Band: TCoolBand;
begin
  inherited Notification(AComponent, Operation);
  case Operation of
    opInsert: begin
      DebugLn('TCoolBar.Notification: Operation = opInsert');
    end;
    opRemove: begin
      DebugLn('TCoolBar.Notification: Operation = opRemove');
      if not (csDestroying in ComponentState) then
      begin
        if AComponent is TWinControl then
        begin
          Band := Bands.FindBand(TControl(AComponent));
          if Band <> nil then
            Band.FControl := nil;
        end
        else if AComponent = FImages then
          Images := nil;
      end;
    end;
  end;
end;

procedure TCustomCoolBar.Loaded;
begin
  inherited Loaded;
  FBands.Update(Nil);
end;

procedure TCustomCoolBar.Paint;
var
  i, BottomY: Integer;
begin
  inherited Paint;
  for i := 0 to FBands.Count-1 do
  begin
    BottomY := FBands[i].FTop + FBands[i].Height + 1;
    if FBandBorderStyle = bsSingle then     // ToDo: paint the border properly
      Canvas.Line(3, BottomY, Width-3, BottomY);
  end;
end;

procedure TCustomCoolBar.Resize;
var
  i: Integer;
begin
  inherited Resize;
  if [csLoading, csDestroying] * ComponentState <> [] then Exit;
  if not Assigned(FBands) then Exit;
  DebugLn('TCoolBar.Resize');
  for i := 0 to FBands.Count-1 do
    if Assigned(FBands[i].FControl) then
      FBands[i].SetControlWidth;
end;

