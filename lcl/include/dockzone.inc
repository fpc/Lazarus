// included by control.pp

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
begin
  // ToDo
  Result:=0;
end;

function TDockZone.GetLeft: Integer;
begin
  // ToDo
  Result:=0;
end;

function TDockZone.GetLimitBegin: Integer;
// retunrs the zone limits. All childs are bounded to this value.
// Aka returns top for orientation doHorizontal and left for doVertical
var
  Zone: TDockZone;
begin
  if FTree.FTopZone = Self then
    Zone := Self
  else
    Zone := FParentZone;
  if Zone.FOrientation = doHorizontal then
    Result := Top
  else if Zone.FOrientation = doVertical then
    Result := Left
  else
    raise Exception.Create('TDockZone.GetLimitBegin');
end;

function TDockZone.GetLimitSize: Integer;
var
  Zone: TDockZone;
begin
  if FTree.FTopZone = Self then
    Zone := Self
  else
    Zone := FParentZone;
  if Zone.FOrientation = doHorizontal then
    Result := Height
  else if Zone.FOrientation = doVertical then
    Result := Width
  else
    raise Exception.Create('TDockZone.GetLimitSize');
end;

function TDockZone.GetTop: Integer;
begin
  // ToDo
  Result:=0;
end;

function TDockZone.GetVisible: Boolean;
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
  // ToDo
  Result:=nil;
end;

function TDockZone.NextVisible: TDockZone;
begin
  // ToDo
  Result:=nil;
end;

function TDockZone.PrevVisible: TDockZone;
begin
  // ToDo
  Result:=nil;
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

