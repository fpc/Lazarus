{%MainUnit ../extctrls.pp}

{******************************************************************************
                                  TControlBar
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

{ TCtrlBand }

function TCtrlBand.GetBandRect: TRect;
begin
  Result.Left := FLeft;
  Result.Top := FTop;
  Result.Right := Result.Left + FWidth;
  Result.Bottom := Result.Top + FHeight;
end;

function TCtrlBand.GetBottom: Integer;
begin
  Result := Top + Height;
end;

function TCtrlBand.GetRight: Integer;
begin
  Result := Left + Width;
end;

procedure TCtrlBand.SetBandRect(AValue: TRect);
begin
  Left := AValue.Left;
  Top := AValue.Top;
  Width := AValue.Right - AValue.Left;
  Height := AValue.Bottom - AValue.Top;
end;

procedure TCtrlBand.SetRight(AValue: Integer);
begin
  Left := AValue - Width;
end;

{ TCtrlBands }

function TCtrlBands.GetIndex(AControl: TControl): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].Control = AControl then begin
      Result := i;
      break;
    end;
end;

{ TCustomControlBar }

constructor TCustomControlBar.Create(AOwner: TComponent);
begin
  FBands := TCtrlBands.Create(True);
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csAutoSizeKeepChildLeft, csAutoSizeKeepChildTop,
                   csCaptureMouse, csClickEvents,
                   csDoubleClicks, csOpaque, csParentBackground];
  FAutoDrag := True;
  FAutoDock := True;
  BevelOuter:=bvLowered;
  BevelInner:=bvRaised;
  DockSite := True;
  GradientDirection := gdVertical;
  GradientStartColor := clDefault;
  GradientEndColor := clDefault;
  FPicture := TPicture.Create;
  FPicture.OnChange := @PictureChanged;
  FRowSize := 26;
  FRowSnap := True;
  SetInitialBounds(0, 0, 100, 50);
end;

destructor TCustomControlBar.Destroy;
begin
  FreeAndNil(FBands);
  FPicture.Free;
  inherited Destroy;
end;

procedure TCustomControlBar.AlignControlsToBands;
var bR2L: Boolean;
    aBand: TCtrlBand;
begin
  bR2L := IsRightToLeft;
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomControlBar.AlignControlsToBands'){$ENDIF};
  try
    for aBand in FVisiBands do
      AlignControlToBand(aBand, bR2L);
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomControlBar.AlignControlsToBands'){$ENDIF};
  end;
end;

procedure TCustomControlBar.AlignControlToBand(ABand: TCtrlBand; ARightToLeft: Boolean);
begin
  ABand.Control.Align:=alNone;
  ABand.Control.Top := ABand.Top + (ABand.Height - ABand.Control.Height) div 2;
  if not ARightToLeft then
    ABand.Control.Left := ABand.Left + cFullGrabber
  else
    ABand.Control.Left := ABand.Left + cBandBorderH;
  { store positions for compare in next Resize => efficient resizing }
  ABand.ControlLeft := ABand.Control.Left;
  ABand.ControlTop := ABand.Control.Top;
  ABand.ControlHeight := ABand.Control.Height;
  ABand.ControlWidth := ABand.Control.Width;
end;

procedure TCustomControlBar.BeginUpdate;
begin
  inc(FUpdateCount);
end;

function TCustomControlBar.CalcBandHeight(AControl: TControl): Integer;
begin
  if RowSnap then
    Result := CalcBandHeightSnapped(AControl)
  else
    Result := 2 * cBandBorderV + AControl.Height;
end;

function TCustomControlBar.CalcBandHeightSnapped(AControl: TControl): Integer;
begin
  Result := (1 + trunc((AControl.Height + 2 * cBandBorderV) div RowSize)) * RowSize;
end;

function TCustomControlBar.CalcInnerBevelWidth: Integer;
begin
  Result := 0;
  if BevelOuter <> bvNone then inc(Result, BevelWidth);
  if BevelInner <> bvNone then inc(Result, BevelWidth);
  inc(Result, BorderWidth);
end;

function TCustomControlBar.CalcLowestBandBottomPx: Integer;
var aBand: TCtrlBand;
begin
  Result := 0;
  for aBand in FVisiBands do
    Result := Math.max(Result, aBand.Bottom);
end;

procedure TCustomControlBar.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
  WithThemeSpace: Boolean);
begin
  //DebugLn('TCustomControlBar.CalculatePreferredSize');
  CheckBandsSizeAndVisibility;
  PreferredWidth := 0;
  PreferredHeight := CalcLowestBandBottomPx;
  if PreferredHeight > 0 then
    inc(PreferredHeight, CalcInnerBevelWidth);
end;

procedure TCustomControlBar.ChangeCursor(ACursor: TCursorDesign);
begin
  FCursorLock := True;
  case ACursor of
    cdDefault: Cursor := FDefCursor;
    cdGrabber: Cursor := crDrag;
    cdRestricted: Cursor := crNo;
  end;
  FCursorLock := False;
end;

procedure TCustomControlBar.CheckBandsSizeAndVisibility;
var aBand: TCtrlBand;
    aIndent: Integer;
    bCtrlVisible: Boolean;
begin
  if not (FBandMove = bmMoving) and not FLockResize then begin
    for aBand in FBands do begin
      bCtrlVisible := aBand.Control.Visible;
      if aBand.ControlVisible <> bCtrlVisible then begin
        if bCtrlVisible then begin
          InitializeBand(aBand, True);
        end else begin
          aBand.ControlVisible := bCtrlVisible;
          SortVisibleBands;
        end;
        break;
      end;
    end;
    SortVisibleBands;
    NormalizeRows;
    for aBand in FVisiBands do
      if (aBand.ControlLeft <> aBand.Control.Left) or
         (aBand.ControlTop <> aBand.Control.Top) or
         (aBand.ControlHeight <> aBand.Control.Height) or
         (aBand.ControlWidth <> aBand.Control.Width) then begin
        aBand.Width := cFullGrabber + aBand.Control.Width + cBandBorderH;
        aBand.Height := CalcBandHeightSnapped(aBand.Control);
        if not IsRightToLeft then
          aIndent := cFullGrabber
        else
          aIndent := cBandBorderH;
        InitializeMove(aBand);
        FInitDrag := Point(aBand.Left, aBand.Top);
        MoveBand(aBand, aBand.Control.Left - aIndent,
          aBand.Control.Top - (aBand.Height - aBand.Control.Height) div 2, False);
        break;
      end;
  end;
end;

procedure TCustomControlBar.CMBiDiModeChanged(var Message: TLMessage);
var i, aWidth: Integer;
begin
  inherited CMBiDiModeChanged(Message);
  aWidth := Width;
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomControlBar.CMBiDiModeChanged'){$ENDIF};
  try
    for i := 0 to FBands.Count - 1 do
      FBands[i].Left := abs(FBands[i].Left - aWidth) - FBands[i].Width;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TCustomControlBar.CMBiDiModeChanged'){$ENDIF};
  end;
end;

procedure TCustomControlBar.CMBorderChanged(var Message: TLMessage);
var i, aNewBevelWidth, aShift: Integer;
begin
  inherited CMBorderChanged(Message);
  if not (csLoading in ComponentState) then begin
    aNewBevelWidth := CalcInnerBevelWidth;
    if aNewBevelWidth <> FInnerBevelWidth then begin
      aShift := aNewBevelWidth - FInnerBevelWidth;
      for i := 0 to FBands.Count - 1 do
        FBands[i].Top := FBands[i].Top + aShift;
      if IsRightToLeft then aShift := - aShift;
      for i := 0 to FBands.Count - 1 do
        FBands[i].Left := FBands[i].Left + aShift;
      FInnerBevelWidth := aNewBevelWidth;
      AlignControlsToBands;
    end;
  end;
end;

procedure TCustomControlBar.CreateWnd;
begin
  //DebugLn('TCustomControlBar.CreateWnd');
  inherited CreateWnd;
  FDefCursor := Cursor;
  FPrevWidth := Width;
end;

procedure TCustomControlBar.DoBandMove(AControl: TControl; var ARect: TRect);
begin
  //DebugLn('TCustomControlBar.DoBandMove');
  if assigned(FOnBandMove) then
    FOnBandMove(self, AControl, ARect);
end;

procedure TCustomControlBar.DoBandPaint(AControl: TControl; ACanvas: TCanvas;
  var ARect: TRect; var AOptions: TBandPaintOptions);
begin
  if assigned(FOnBandPaint) then
    FOnBandPaint(self, AControl, ACanvas, ARect, AOptions);
end;

function TCustomControlBar.DragControl(AControl: TControl; X, Y: Integer;
  KeepCapture: Boolean): Boolean;
begin
  //DebugLn('TCustomControlBar.DragControl');
  Result := True;
  if assigned(AControl) and assigned(FOnBandDrag) then
    FOnBandDrag(self, AControl, Result);
  if Result then
    DragManager.DragStart(AControl, True, -1, True);
end;

procedure TCustomControlBar.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  Accept := Accept or (FBands.GetIndex(Source as TControl) < 0);
end;

procedure TCustomControlBar.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount = 0 then begin
    Invalidate;
  end;
end;

procedure TCustomControlBar.FlipChildren(AllLevels: Boolean);
begin
  // Do nothing
end;

procedure TCustomControlBar.GetControlInfo(AControl: TControl; var Insets: TRect;
  var PreferredSize, RowCount: Integer);
begin
  if assigned(FOnBandInfo) then
    FOnBandInfo(self, AControl, Insets, PreferredSize, RowCount);
end;

function TCustomControlBar.HitTest(X, Y: Integer): TControl;
var i: Integer;
    aPoint: TPoint;
    aRect: TRect;
begin
  Result := nil;
  aPoint := Point(X, Y);
  for i := 0 to length(FVisiBands) - 1 do begin
    aRect := FVisiBands[i].Control.ClientRect;
    if PtInRect(aRect, aPoint) then begin
      Result := FVisiBands[i].Control;
      break;
    end;
  end;
end;

class constructor TCustomControlBar.InitializeClass;
begin
  cFullGrabber := cGrabWidth + 2 * cBandBorderH;
end;

procedure TCustomControlBar.InitializeBand(ABand: TCtrlBand; AKeepPos: Boolean);
var i, j, k, aBevel, aCount, aLeft, aLimit, aRight, aTop: Integer;
    aRect: TRect;
    bR2L: Boolean;

  function GetOverlapBand: Integer;
  begin
    Result := 0;
    while Result < aCount do begin
      if IsBandOverlap(aRect, FVisiBands[Result].BandRect) then break;
      inc(Result);
    end;
  end;

begin
  //DebugLn('TCustomControlBar.InitializeBand');
  { control is not yet part of FVisiBands }
  aBevel := FInnerBevelWidth;
  aCount := length(FVisiBands);
  bR2L := IsRightToLeft;
  { calc. row for the new band }
  j := CalcLowestBandBottomPx;
  if j > 0 then j := (j - aBevel) div RowSize;
  { calc. initial geometry }
  ABand.Width := cFullGrabber + ABand.Control.Width + cBandBorderH;
  if not bR2L then begin
    aLeft := Math.max(ABand.Control.Left - cFullGrabber, aBevel);
    aLeft := Math.min(aLeft, ClientWidth - aBevel - cFullGrabber);
  end else begin
    aLeft := Math.min(ClientWidth - aBevel - ABand.Width, ABand.Control.Left - aBevel);
    aLeft := Math.max(aLeft, aBevel + cFullGrabber);
  end;
  aTop := aBevel + Math.max(Math.min(j, (ABand.Control.Top - aBevel) div RowSize), 0) * RowSize;
  ABand.Height := CalcBandHeight(ABand.Control);
  { check whether the new band overlap any other }
  aRect := Rect(aLeft, aTop, aLeft + ABand.Width, aTop + ABand.Height);
  i := GetOverlapBand;
  { try keep the ABand on its pos. and move others down }
  if (i < aCount) and AKeepPos then begin
    for k := 0 to aCount - 1 do
      FVisiBands[k].InitTop := FVisiBands[k].Top;
    j := ABand.Height;
    for i := 0 to aCount - 1 do begin
      if FVisiBands[i].Top >= aTop then
        FVisiBands[i].Top := FVisiBands[i].Top + j;
    end;
    i := GetOverlapBand;
    if i < aCount then
      for k := 0 to aCount - 1 do
        FVisiBands[k].Top := FVisiBands[k].InitTop;
  end;
  if i < aCount then begin
    { attempt to stick band by some existing band }
    k := 0;
    if not bR2L then begin
      aLimit := ClientWidth - aBevel - cFullGrabber;
      j := 0;
      if FVisiBands[i].Top <= aTop then j := i;
      while j < aCount do begin
        aRight := FVisiBands[j].Right;
        if (aLeft <= aRight) and (aRight <= aLimit) then begin
          aRect.Left := aRight;
          aRect.Right := aRect.Left + ABand.Width;
          aRect.Top := FVisiBands[j].Top;
          aRect.Bottom := aRect.Top + ABand.Height;
          k := GetOverlapBand;
          if k = aCount then break;
        end;
        inc(j);
      end;
    end else begin
      j := i;
      while (j < aCount) and (FVisiBands[j].Top = aTop) do begin
        aRect.Right := FVisiBands[j].Left;
        aRect.Left := aRect.Right - ABand.Width;
        k := GetOverlapBand;
        if k = aCount then break;
        inc(j);
      end;
    end;
    { attempt failed, place band below }
    if k < aCount then begin
      aRect.Left := aLeft;
      aRect.Right := aLeft + ABand.Width;
      aRect.Top := aTop + ABand.Height - RowSize;
      aRect.Bottom := aRect.Top + ABand.Height;
      while GetOverlapBand < aCount do begin
        inc(aRect.Top, RowSize);
        inc(aRect.Bottom, RowSize);
      end;
    end;
    DoBandMove(ABand.Control, aRect);
  end ;
  ABand.BandRect := aRect;
  ABand.ControlVisible := ABand.Control.Visible;
  AlignControlToBand(ABand, bR2L);
end;

procedure TCustomControlBar.InitializeMove(AMovingBand: TCtrlBand);
var i: Integer;
    aBand: TCtrlBand;
begin
  for aBand in FVisiBands do begin
    aBand.InitLeft := aBand.Left;
    aBand.InitTop := aBand.Top;
  end;
  { copy FVisiBands to FVisiBandsEx ommiting AMovingBand }
  SetLength(FVisiBandsEx, length(FVisiBands) - 1);
  i := 0;
  for aBand in FVisiBands do
    if aBand <> AMovingBand then begin
      FVisiBandsEx[i] := aBand;
      inc(i);
    end;
end;

procedure TCustomControlBar.InsertControl(AControl: TControl; Index: Integer);
var aBand: TCtrlBand;
    aRect: TRect;
    aWidth, aRows: Integer;
begin
  //DebugLn('TCustomControlBar.InsertControl');
  inherited InsertControl(AControl, Index);
  if not (csLoading in ComponentState) then
    { new control is not yet in FVisiBands }
    if AControl is TWinControl then begin
      aBand := TCtrlBand.Create;
      AControl.Align := alNone;
      aBand.Control := AControl;
      InitializeBand(aBand, False);
      aRect := AControl.BoundsRect;
      aWidth := aBand.Width;
      aRows := aBand.Height div RowSize;
      GetControlInfo(AControl, aRect, aWidth, aRows);
      FBands.Add(aBand);
    end;
end;

function TCustomControlBar.IsBandOverlap(ARect, BRect: TRect): Boolean;
begin
  Result := not ((ARect.Right <= BRect.Left) or (BRect.Right <= ARect.Left)
              or (ARect.Bottom <= BRect.Top) or (BRect.Bottom <= ARect.Top));
end;

procedure TCustomControlBar.Loaded;
var i, aIndent: Integer;
    aBand: TCtrlBand;
begin
  inherited Loaded;
  if not IsRightToLeft then
    aIndent := cFullGrabber
  else
    aIndent := cBandBorderH;
  for i := 0 to ControlCount - 1 do begin
    aBand := TCtrlBand.Create;
    aBand.Control := Controls[i];
    aBand.Height := CalcBandHeight(Controls[i]);
    aBand.Width := cFullGrabber + Controls[i].Width + cBandBorderH;
    aBand.Top := Controls[i].Top - (aBand.Height - Controls[i].Height) div 2;
    aBand.Left := Controls[i].Left - aIndent;
    FBands.Add(aBand);
  end;
end;

procedure TCustomControlBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //DebugLn('TCustomControlBar.MouseDown');
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and (FBandMove = bmReady) then begin
    FBandMove := bmMoving;
    InitializeMove(FHoveredBand);
    FInitDrag := Point(X, Y);
  end;
end;

procedure TCustomControlBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var aBand: TCtrlBand;
    aCursor: TCursorDesign;
    bDragging, bGrabber: Boolean;
begin
  inherited MouseMove(Shift, X, Y);
  if FBandMove <> bmMoving then begin
    aBand := MouseToBandPos(X, Y, bGrabber);
    if not bGrabber then
      aCursor := cdDefault
    else
      aCursor := cdGrabber;
    ChangeCursor(aCursor);
    if bGrabber then begin
      FBandMove := bmReady;
      FHoveredBand := aBand;
    end else begin
      FBandMove := bmNone;
      FHoveredBand := nil;
    end;
  end else begin
    if not DragManager.CanStartDragging(self,-1,X,Y) then begin
      MoveBand(FHoveredBand, X, Y, True);
    end else begin
      bDragging := False;
      if AutoDrag then
        bDragging := DragControl(FHoveredBand.Control, X, Y);
      if bDragging then
        FBandMove:=bmNone
      else
        MoveBand(FHoveredBand, X, Y, True);
    end;
  end;
end;

function TCustomControlBar.MouseToBandPos(X, Y: Integer; out AGrabber: Boolean): TCtrlBand;
var aBand: TCtrlBand;
    aLeft, aTop: Integer;
begin
  Result := nil;
  AGrabber := False;
  if length(FVisiBands) > 0 then begin
    if Y <= CalcLowestBandBottomPx then
      for aBand in FVisiBands do begin
        aLeft := aBand.Left;
        aTop := aBand.Top;
        if PtInRect(Rect(aLeft, aTop, aLeft + aBand.Width,
          aTop + aBand.Height), Point(X, Y)) then begin
          Result := aBand;
          if not IsRightToLeft then
            AGrabber := (X <= (aLeft + cFullGrabber))
          else
            AGrabber := (X >= (aLeft + aBand.Width - cFullGrabber));
          exit;  { Exit! }
        end;
      end;
  end;
end;

procedure TCustomControlBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //DebugLn('TCustomControlBar.MouseUp');
  inherited MouseUp(Button, Shift, X, Y);
  if FBandMove = bmMoving then begin
    FBandMove := bmNone;
    ChangeCursor(cdDefault);
    SortVisibleBands;
  end;
end;

procedure TCustomControlBar.MoveBand(AMoveBand: TCtrlBand; X, Y: Integer; ByMouse: Boolean);
var i, aBevel, aCount, aLimit, aNewBound, aNewRow: Integer;
    aRect: TRect;
    bR2L, bRestrictCursor: Boolean;

  function IsNewPositionFree: Boolean;
  var k: Integer;
  begin
    k := 0;
    while k < aCount do begin
      if IsBandOverlap(aRect, FVisiBandsEx[k].BandRect) then break;
      inc(k);
    end;
    Result := (k = aCount);
  end;

begin
  aBevel := FInnerBevelWidth;
  aCount := length(FVisiBandsEx);
  for i := 0 to aCount - 1 do begin
    FVisiBandsEx[i].Left := FVisiBandsEx[i].InitLeft;
    FVisiBandsEx[i].Top := FVisiBandsEx[i].InitTop;
  end;
  bR2L := IsRightToLeft;
  aNewRow := (Y - (FInitDrag.Y - AMoveBand.InitTop)) div RowSize;
  bRestrictCursor := (Y < aBevel) or (Y > (ClientHeight - aBevel));
  if not bR2L then begin
    aLimit := ClientWidth - aBevel - cFullGrabber;
    aNewBound := X - (FInitDrag.X - AMoveBand.InitLeft);
    if aNewBound > aLimit then begin
      aNewBound := aLimit;
      bRestrictCursor := True;
    end;
    if aNewBound < aBevel then begin
      aNewBound := aBevel;
      bRestrictCursor := True;
    end;
  end else begin
    aLimit := ClientWidth - aBevel - AMoveBand.Width;
    aNewBound := X - (FInitDrag.X - AMoveBand.InitLeft);
    if aNewBound > aLimit then begin
      aNewBound := aLimit;
      bRestrictCursor := True;
    end;
    aLimit := aBevel + cFullGrabber - AMoveBand.Width;
    if aNewBound < aLimit then begin
      aNewBound := aLimit;
      bRestrictCursor := True;
    end;
  end;
  aRect := Rect(aNewBound, aNewRow * RowSize + aBevel, aNewBound + AMoveBand.Width,
                aNewRow * RowSize + aBevel + AMoveBand.Height);
  i := 0;
  while i < aCount do begin
    if IsBandOverlap(aRect, FVisiBandsEx[i].BandRect) then begin
      { attempts to stick band }
      if FVisiBandsEx[i].Left < aRect.Left then begin
        { band dragged from the right }
        aRect.Left := FVisiBandsEx[i].InitLeft + FVisiBandsEx[i].Width;
        aRect.Right := aRect.Left + AMoveBand.Width;
        if (not bR2L and (aRect.Left < (ClientWidth - aBevel - cFullGrabber)))
          or (bR2L and (aRect.Right <= (ClientWidth - aBevel))) then
          if IsNewPositionFree then i := aCount;
        break;
      end else
        { band dragged from the left }
        if (aRect.Left + AMoveBand.Width) > FVisiBandsEx[i].InitLeft then begin
          aRect.Right := FVisiBandsEx[i].Left;
          aRect.Left := aRect.Right - AMoveBand.Width;
          if (not bR2L and (aRect.Left >= aBevel))
            or (bR2L and (aRect.Right > (aBevel + cFullGrabber))) then
            if IsNewPositionFree then i := aCount;
        break;
      end;
    end;
    inc(i);
  end;
  if i < aCount then begin  { new place is occupied, fallback to the last valid pos. }
    bRestrictCursor := True;
    aRect.Left := AMoveBand.Left;
    aRect.Right := aRect.Left + AMoveBand.Width;
    aRect.Top := AMoveBand.Top;
    aRect.Bottom := aRect.Top + AMoveBand.Height;
    { check if the fallback pos is occupied, when band was resized }
    if not IsNewPositionFree then begin
      aRect.Top := CalcLowestBandBottomPx;
      aRect.Bottom := aRect.Top + AMoveBand.Height;
    end;
  end;
  if ByMouse then
    if not bRestrictCursor then
      ChangeCursor(cdGrabber)
    else
      ChangeCursor(cdRestricted);
  DoBandMove(AMoveBand.Control, aRect);
  if not EqualRect(aRect, AMoveBand.BandRect) then begin
    AMoveBand.BandRect := aRect;
    NormalizeRows;
    AlignControlsToBands;
    Invalidate;
  end;
end;

procedure TCustomControlBar.NormalizeRows;
var i, j, aBevel, aCount, aMax: Integer;
    aRows: TBooleanDynArray;
begin
  //DebugLn('TCustomControlBar.NormalizeRows');
  { FVisiBands is not sorted here ! }
  aCount := length(FVisiBands);
  if aCount > 0 then begin
    aBevel := FInnerBevelWidth;
    { shift all rows so that the lowest begin at the top }
    j := high(Integer);
    aMax := 0;
    for i := 0 to aCount -1 do begin
      j := Math.min(j, FVisiBands[i].Top);
      aMax := Math.max(aMax, FVisiBands[i].Bottom);
    end;
    j := aBevel - j;
    if j <> 0 then begin
      for i := 0 to aCount - 1 do
        FVisiBands[i].Top := FVisiBands[i].Top + j;
      inc(aMax, j);
    end;
    { remove empty rows }
    aMax := (aMax - aBevel) div RowSize;
    SetLength(aRows, aMax);
    for i := 0 to aMax -1 do
      aRows[i] := False;
    for i := 0 to aCount - 1 do begin
      for j := ((FVisiBands[i].Top - aBevel) div RowSize) to
          ((FVisiBands[i].Bottom - aBevel) div RowSize) - 1 do
        aRows[j] := True;
    end;
    for i := aMax - 1 downto 0 do
      if not aRows[i] then begin
        for j := aCount - 1 downto 0 do
          if FVisiBands[j].Top > (i * RowSize + aBevel) then
            FVisiBands[j].Top := FVisiBands[j].Top - RowSize;
      end;
  end;
end;

procedure TCustomControlBar.Paint;
const cBandBevel = 1;
      cOptions = [bpoGrabber, bpoFrame, bpoGradient, bpoRoundRect];
var aBevel: Integer;
    i, j: Integer;
    aOptions: TBandPaintOptions;
    aRect: TRect;
    aStartColor, aEndColor: TColor;
begin
  inherited Paint;
  aBevel := CalcInnerBevelWidth;
  Canvas.Clipping := True;
  aRect := ClientRect;
  InflateRect(aRect, -aBevel, -aBevel);
  Canvas.ClipRect := aRect;
  if assigned(Picture) and (Picture.Width > 0) and (Picture.Height > 0) then begin
    for i := 0 to (ClientWidth - 2 * aBevel) div Picture.Width do
      for j := 0 to (ClientHeight - 2 * aBevel) div Picture.Height do
        Canvas.Draw(i * Picture.Width + aBevel, j * Picture.Height + aBevel, Picture.Bitmap);
  end;
  for i := 0 to length(FVisiBands) - 1 do
    begin
      aRect.Left := FVisiBands[i].Left;
      aRect.Top := FVisiBands[i].Top;
      aRect.Right := aRect.Left + FVisiBands[i].Width;
      aRect.Bottom := aRect.Top + FVisiBands[i].Height;
      aOptions := cOptions;
      DoBandPaint(FVisiBands[i].Control, Canvas, aRect, aOptions);
      if bpoFrame in aOptions then
        Canvas.Frame3d(aRect, cBandBevel, bvRaised);  { Frame3D inflates aRect }
      if (bpoGradient in aOptions) and (DrawingStyle = dsGradient) then begin
        aStartColor := GradientStartColor;
        if aStartColor = clDefault then aStartColor := clForm;
        aEndColor := GradientEndColor;
        if aEndColor = clDefault then aEndColor := clHighlight;
        Canvas.GradientFill(aRect, aStartColor, aEndColor, GradientDirection);
      end;
      if bpoGrabber in aOptions then begin
        if not IsRightToLeft then begin
          inc(aRect.Left, cBandBorderH - cBandBevel);
          aRect.Right := aRect.Left + cGrabWidth;
        end else begin
          dec(aRect.Right, cBandBorderH - cBandBevel);
          aRect.Left := aRect.Right - cGrabWidth;
        end;
        inc(aRect.Top, cBandBevel);
        dec(aRect.Bottom, cBandBevel);
        Canvas.Brush.Style := bsClear;
        Canvas.Frame3D(aRect, cBandBevel, bvRaised);
      end;
    end;
  Canvas.Clipping := False;
end;

procedure TCustomControlBar.PictureChanged(Sender: TObject);
begin
  if FUpdateCount = 0 then Invalidate;
end;

procedure TCustomControlBar.RemoveControl(AControl: TControl);
var aIndex: Integer;
begin
  //DebugLn('TCustomControlBar.RemoveControl', AControl.Name);
  aIndex := FBands.GetIndex(AControl);
  if aIndex >= 0 then FBands.Delete(aIndex);
  inherited RemoveControl(AControl);
  if not (csDestroying in ComponentState) then Invalidate;
end;

procedure TCustomControlBar.Resize;
begin
  //DebugLn('TCustomControlBar.Resize');
  inherited Resize;
  if not AutoSize then CheckBandsSizeAndVisibility;
  AlignControlsToBands;
  Invalidate;
end;

procedure TCustomControlBar.SetCursor(Value: TCursor);
begin
  inherited SetCursor(Value);
  if not FCursorLock then FDefCursor := Value;
end;

procedure TCustomControlBar.SetDrawingStyle(AValue: TBandDrawingStyle);
begin
  if FDrawingStyle = AValue then exit;
  FDrawingStyle := AValue;
  if FUpdateCount = 0 then Invalidate;
end;

procedure TCustomControlBar.SetGradientDirection(AValue: TGradientDirection);
begin
  if FGradientDirection = AValue then exit;
  FGradientDirection := AValue;
  if FUpdateCount = 0 then Invalidate;
end;

procedure TCustomControlBar.SetGradientEndColor(AValue: TColor);
begin
  if FGradientEndColor = AValue then exit;
  FGradientEndColor := AValue;
  if FUpdateCount = 0 then Invalidate;
end;

procedure TCustomControlBar.SetGradientStartColor(AValue: TColor);
begin
  if FGradientStartColor = AValue then exit;
  FGradientStartColor := AValue;
  if FUpdateCount = 0 then Invalidate;
end;

procedure TCustomControlBar.SetPicture(aValue: TPicture);
begin
  FPicture.Assign(aValue);
end;

procedure TCustomControlBar.SetRowSize(AValue: TRowSize);
var aBand: TCtrlBand;
    aBevel, aRow, aOldRowSize: Integer;
begin
  aOldRowSize := FRowSize;
  if aOldRowSize = AValue then exit;
  FRowSize := AValue;
  aBevel := FInnerBevelWidth;
  for aBand in FBands do begin
    aRow := (aBand.Top - aBevel) div aOldRowSize;
    aBand.Top := aRow * AValue + aBevel;
  end;
  if RowSnap then begin
    for aBand in FBands do
      aBand.Height := CalcBandHeightSnapped(aBand.Control);
  end;
  NormalizeRows;
  FLockResize := True;
  AlignControlsToBands;
  FLockResize := False;
  if AutoSize then InvalidatePreferredSize;
  if FUpdateCount = 0 then Invalidate;
end;

procedure TCustomControlBar.ShiftBands(AFrom, ATo, AShift, ALimit: Integer);
var i: Integer;
begin
  if not IsRightToLeft then begin
    for i := AFrom to ATo do
      if FVisiBands[i].Left >= ALimit then
        FVisiBands[i].Left := FVisiBands[i].Left + AShift
  end else begin
    for i := AFrom to ATo do
      if (FVisiBands[i].Left + FVisiBands[i].Width) <= ALimit then
        FVisiBands[i].Left := FVisiBands[i].Left - AShift;
  end;
end;

procedure TCustomControlBar.SortVisibleBands;
var i, j, aCount: Integer;
    b: Boolean;
    aBand: TCtrlBand;
begin
  //DebugLn('TCustomControlBar.SortVisiBands');
  { calculate number of visible controls and set visibility of bands }
  aCount := 0;
  for i := 0 to FBands.Count - 1 do begin
    b := FBands[i].Control.Visible;
    FBands[i].Visible := b;
    if b then inc(aCount);
  end;
  { set length of FVisiBands }
  SetLength(FVisiBands, aCount);
  { assign visible bands to FVisiBands }
  j := 0;
  for i := 0 to FBands.Count - 1 do
    if FBands[i].Visible then begin
      FVisiBands[j] := FBands[i];
      inc(j);
    end;
  { sort FVisiBands (when it makes sense) }
  if aCount > 1 then
    for j := 0 to aCount - 2 do
      for i := aCount - 1 downto j + 1 do
        if (FVisiBands[i].FTop < FVisiBands[i - 1].FTop) or
          ((FVisiBands[i].FTop = FVisiBands[i - 1].FTop) and
           (FVisiBands[i].FLeft < FVisiBands[i - 1].FLeft))
        then begin
          aBand := FVisiBands[i];
          FVisiBands[i] := FVisiBands[i - 1];
          FVisiBands[i - 1] := aBand;
        end;
end;

procedure TCustomControlBar.StickControls;
begin
  // ToDo
  AlignControlsToBands;
end;

procedure TCustomControlBar.WMSize(var Message: TLMSize);
var i, aBevel, aBound, aCount, aLeftMost, aMove, aRightMost, aShift: Integer;
    bR2L: Boolean;
begin
  //DebugLn('TCustomControlBar.WMSize');
  inherited WMSize(Message);
  bR2L := IsRightToLeft;
  aCount := length(FVisiBands);
  aShift := FPrevWidth - Message.Width;
  if bR2L then
    for i := 0 to aCount - 1 do
      FVisiBands[i].Left := FVisiBands[i].Left - aShift;
  if aShift > 0 then begin
    if aCount > 0 then begin
      aBevel := FInnerBevelWidth;
      aBound := Message.Width - aBevel;
      aLeftMost := high(Integer);
      aRightMost := low(Integer);
      for i := 0 to aCount - 1 do begin
        aLeftMost := Math.min(aLeftMost, FVisiBands[i].Left);
        aRightMost := Math.max(aRightMost, FVisiBands[i].Right);
      end;
    end
    else begin
      aBevel := 0;
      aBound := 0;
      aLeftMost := 0;
      aRightMost := 0;
    end;
    if not bR2L then begin
      if (aRightMost > aBound) and (aLeftMost > aBevel) then begin
        aMove := Math.min(aLeftMost - aBevel, aRightMost - aBound);
				aMove := Math.min(aMove, aShift);
        for i := 0 to aCount - 1 do
          FVisiBands[i].Left := FVisiBands[i].Left - aMove;
      end;
    end else begin
      if (aLeftMost < aBevel) and (aRightMost < aBound) then begin
        aMove := Math.min(aBevel - aLeftMost, aBound - aRightMost);
        aMove := Math.min(aMove, aShift);
        for i := 0 to aCount - 1 do
          FVisiBands[i].Left := FVisiBands[i].Left + aMove;
      end;
    end;
  end;
  FPrevWidth := Message.Width;
end;


