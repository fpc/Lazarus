{%MainUnit ../control.pp}

{******************************************************************************
                                     TDockZone
 ******************************************************************************

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

function TDockZone.GetHeight: Integer;
var
  Zone: TDockZone;
  R: TRect;
begin
  if (Self = FTree.FTopZone) or ((FParentZone = FTree.FTopZone) and
    (FChildControl <> nil) and (FTree.FTopZone.VisibleChildCount = 1)) then
  begin
    R := FTree.FDockSite.ClientRect;
    FTree.FDockSite.AdjustClientRect(R);
    Result := R.Bottom - R.Top;
  end
  else begin
    Zone := Self;
    while Zone.FParentZone<>nil do begin
      if Zone.FParentZone.FOrientation = doHorizontal then begin
        Result := Zone.ZoneLimit - Zone.LimitBegin;
        Exit;
      end else
        Zone := Zone.FParentZone;
    end;
    if FTree.FTopZone.FOrientation = doHorizontal then
      Result := FTree.FTopXYLimit
    else
      Result := FTree.FTopZone.ZoneLimit;
  end;
end;

function TDockZone.GetLeft: Integer;
var
  Zone: TDockZone;
  R: TRect;
begin
  Zone := Self;
  while Zone.FParentZone<>nil do begin
    if (Zone.FParentZone.FOrientation = doVertical)
    and (Zone.FPrevSibling <> nil) then begin
      Result := Zone.FPrevSibling.ZoneLimit;
      Exit;
    end else
      Zone := Zone.FParentZone;
  end;
  R := FTree.FDockSite.ClientRect;
  FTree.FDockSite.AdjustClientRect(R);
  Result:=R.Left;
end;

function TDockZone.GetLimitBegin: Integer;
// returns the zone limit.
var
  ZoneOrientation: TDockOrientation;
begin
  if FParentZone = nil then
    ZoneOrientation := Self.FOrientation
  else
    ZoneOrientation := FParentZone.FOrientation;
  if ZoneOrientation = doHorizontal then
    Result := Top
  else if ZoneOrientation = doVertical then
    Result := Left
  else
    raise Exception.Create('TDockZone.GetLimitBegin');
end;

function TDockZone.GetLimitSize: Integer;
var
  ZoneOrientation: TDockOrientation;
begin
  if FParentZone = nil then
    ZoneOrientation := Self.FOrientation
  else
    ZoneOrientation := FParentZone.FOrientation;
  if ZoneOrientation = doHorizontal then
    Result := Height
  else if ZoneOrientation = doVertical then
    Result := Width
  else
    raise Exception.Create('TDockZone.GetLimitSize');
end;

function TDockZone.GetTop: Integer;
var
  Zone: TDockZone;
  R: TRect;
begin
  Zone := Self;
  while Zone.FParentZone<>nil do begin
    if (Zone.FParentZone.FOrientation = doHorizontal)
    and (Zone.FPrevSibling <> nil) then begin
      Result := Zone.FPrevSibling.ZoneLimit;
      Exit;
    end else
      Zone := Zone.FParentZone;
  end;
  R := FTree.FDockSite.ClientRect;
  FTree.FDockSite.AdjustClientRect(R);
  Result:=R.Top;
end;

function TDockZone.GetVisible: Boolean;
// a zone is visible if it or one of its child zones contain a visible control
var
  Zone: TDockZone;
begin
  if Assigned(FChildControl) then
    Result := FChildControl.Visible
  else
  begin
    Result := True;
    Zone := FirstVisibleChild;
    while Assigned(Zone) do begin
      if Zone.Visible then Exit;
      Zone := Zone.FNextSibling;
    end;
    Result := False;
  end;
end;

function TDockZone.GetVisibleChildCount: Integer;
var
  Zone: TDockZone;
begin
  Result := 0;
  Zone := FirstVisibleChild;
  while Zone <> nil do begin
    Zone := Zone.NextVisible;
    Inc(Result);
  end;
end;

function TDockZone.GetWidth: Integer;
begin
  // ToDo
  Result:=0;
end;

function TDockZone.GetZoneLimit: Integer;
begin
  if (not Visible) and IsOrientationValid then
    // LimitSize will be zero and zone will take up no space
    Result := GetLimitBegin
  else
    Result := FZoneLimit;
end;

procedure TDockZone.SetZoneLimit(const AValue: Integer);
begin
  FZoneLimit := AValue;
end;

function TDockZone.IsOrientationValid: boolean;
begin
  Result := (Assigned(FParentZone) and (FParentZone.FOrientation <> doNoOrient))
            or ((FTree.FTopZone = Self) and (FOrientation <> doNoOrient));
end;

function TDockZone.GetNextVisibleZone: TDockZone;
begin
  Result := FNextSibling;
  while Assigned(Result) and not Result.Visible do
    Result := Result.FNextSibling;
end;

constructor TDockZone.Create(TheTree: TDockTree);
begin
  FTree:=TheTree;
end;

procedure TDockZone.ExpandZoneLimit(NewLimit: Integer);
begin
  // ToDo
end;

function TDockZone.FirstVisibleChild: TDockZone;
begin
  if FFirstChildZone<>nil then begin
    if FFirstChildZone.Visible then
      Result:=FFirstChildZone
    else
      Result:=FFirstChildZone.GetNextVisibleZone;
  end else begin
    Result:=nil;
  end;
end;

function TDockZone.NextVisible: TDockZone;
begin
  Result:=FNextSibling;
  while (Result<>nil) and (not Result.Visible) do Result:=Result.FNextSibling;
end;

function TDockZone.PrevVisible: TDockZone;
begin
  Result:=FPrevSibling;
  while (Result<>nil) and (not Result.Visible) do Result:=Result.FPrevSibling;
end;

procedure TDockZone.ResetChildren;
begin
  // ToDo
end;

procedure TDockZone.ResetZoneLimits;
begin
  // ToDo
end;

procedure TDockZone.Update;
begin
  // ToDo
end;

// included by control.pp

