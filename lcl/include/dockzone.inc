{%MainUnit ../control.pp}

{******************************************************************************
                                     TDockZone
 ******************************************************************************

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

function TDockZone.GetHeight: Integer;
//var
  //Zone: TDockZone;
  //R: TRect;
begin
  if not Visible then
    Result:=0
  else if (FChildControl<>nil) then
    Result:=FChildControl.Height
  else if FParentZone<>nil then
    Result:=FParentZone.Height
  else
    Result:=0;

  {if (Self = FTree.FTopZone)
  or ((FParentZone = FTree.FTopZone)
      and (FChildControl <> nil) and (FTree.FTopZone.VisibleChildCount = 1))
  then begin
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
  end;}
end;

function TDockZone.GetLeft: Integer;
//var
  //Zone: TDockZone;
  //R: TRect;
begin
  Result:=0;

  {Zone := Self;
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
  Result:=R.Left;}
end;

function TDockZone.GetLimitBegin: Integer;
// returns the zone limit.
begin
  if FOrientation = doHorizontal then
    Result := Top
  else if FOrientation = doVertical then
    Result := Left
  else
    raise Exception.Create('TDockZone.GetLimitBegin');
end;

function TDockZone.GetLimitSize: Integer;
// returns the zone size.
begin
  if FOrientation = doHorizontal then
    Result := Height
  else if FOrientation = doVertical then
    Result := Width
  else
    raise Exception.Create('TDockZone.GetLimitSize');
end;

function TDockZone.GetTop: Integer;
//var
  //Zone: TDockZone;
  //R: TRect;
begin
  Result:=0
  {Zone := Self;
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
  Result:=R.Top;}
end;

function TDockZone.GetVisible: Boolean;
// a zone is visible if one of its child zones contain a visible control
begin
  if Assigned(FChildControl) then
    Result := FChildControl.Visible
  else
    Result := FirstVisibleChild<>nil;
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
  if not Visible then
    Result:=0
  else if (FChildControl<>nil) then
    Result:=FChildControl.Width
  else if FParentZone<>nil then
    Result:=FParentZone.Width
  else
    Result:=0;
end;

function TDockZone.GetNextVisibleZone: TDockZone;
begin
  Result := FNextSibling;
  while Assigned(Result) and not Result.Visible do
    Result := Result.FNextSibling;
end;

constructor TDockZone.Create(TheTree: TDockTree; TheChildControl: TControl);
begin
  FTree:=TheTree;
  FChildControl:=TheChildControl;
end;

function TDockZone.FindZone(AControl: TControl): TDockZone;
begin
  Result := nil;
  if AControl = ChildControl then
  begin
    Result := Self;
    exit;
  end;
  if FFirstChildZone <> nil then
  begin
    Result := FFirstChildZone.FindZone(AControl);
    if Result <> nil then exit;
  end;
  if FNextSibling <> nil then
    Result := FNextSibling.FindZone(AControl);
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

procedure TDockZone.AddAsFirstChild(NewChildZone: TDockZone);
begin
  NewChildZone.FParentZone:=Self;
  NewChildZone.FNextSibling:=FFirstChildZone;
  if FFirstChildZone<>nil then
    FFirstChildZone.FPrevSibling:=NewChildZone;
  FFirstChildZone:=NewChildZone;
  inc(FChildCount);
end;

procedure TDockZone.AddAsLastChild(NewChildZone: TDockZone);
var
  LastChild: TDockZone;
begin
  NewChildZone.FParentZone:=Self;
  LastChild:=GetLastChild;
  NewChildZone.FPrevSibling:=LastChild;
  if LastChild<>nil then
    LastChild.FNextSibling:=NewChildZone
  else
    FFirstChildZone:=NewChildZone;
  inc(FChildCount);
end;

procedure TDockZone.ReplaceChild(OldChild, NewChild: TDockZone);
begin
  NewChild.FParentZone:=Self;
  NewChild.FNextSibling:=OldChild.FNextSibling;
  NewChild.FPrevSibling:=OldChild.FPrevSibling;
  if NewChild.FNextSibling<>nil then
    NewChild.FNextSibling.FPrevSibling:=NewChild;
  if NewChild.FPrevSibling<>nil then
    NewChild.FPrevSibling.FNextSibling:=NewChild;
  OldChild.FNextSibling:=nil;
  OldChild.FPrevSibling:=nil;
  OldChild.FParentZone:=nil;
end;

function TDockZone.GetLastChild: TDockZone;
begin
  Result:=FFirstChildZone;
  if Result=nil then exit;
  while (Result.FNextSibling<>nil) do Result:=Result.FNextSibling;
end;

function TDockZone.GetIndex: Integer;
var
  Zone: TDockZone;
begin
  Result:=0;
  Zone:=PrevSibling;
  while Zone<>nil do begin
    inc(Result);
    Zone:=Zone.PrevSibling;
  end;
end;

procedure TDockZone.Remove(ChildZone: TDockZone);
begin
  if ChildZone.Parent<>Self then
    raise Exception.Create('TDockZone.Remove');
  if ChildZone=FFirstChildZone then FFirstChildZone:=ChildZone.FNextSibling;
  if ChildZone.FNextSibling<>nil then
    ChildZone.FNextSibling.FPrevSibling:=ChildZone.FPrevSibling;
  if ChildZone.FPrevSibling<>nil then
    ChildZone.FPrevSibling.FNextSibling:=ChildZone.FNextSibling;
  ChildZone.FPrevSibling:=nil;
  ChildZone.FNextSibling:=nil;
  ChildZone.FParentZone:=nil;
  dec(FChildCount);
end;

// included by control.pp
