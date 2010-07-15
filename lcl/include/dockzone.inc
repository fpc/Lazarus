{%MainUnit ../control.pp}

{******************************************************************************
                                     TDockZone
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

procedure TDockZone.SetLimitBegin(const AValue: Integer);
// sets the left/top coord of zone.
begin
  case FOrientation of
    doHorizontal: Top := AValue;
    doVertical: Left := AValue;
  else
    raise Exception.Create('TDockZone.SetLimitBegin');
  end;
end;

procedure TDockZone.SetLimitSize(const AValue: Integer);
// sets the zone width/height.
begin
  case FOrientation of
    doHorizontal: Height := AValue;
    doVertical: Width := AValue;
  else
    raise Exception.Create('TDockZone.SetLimitSize');
  end;
end;

procedure TDockZone.SetHeight(const AValue: Integer);
begin
  if not Visible then
    Exit;

  if (FChildControl <> nil) then
    FChildControl.Height := AValue
  else
    FBounds.Bottom := AValue;
end;

procedure TDockZone.SetLeft(const AValue: Integer);
begin
  if not Visible then
    Exit;

  if (FChildControl <> nil) then
    FChildControl.Left := AValue
  else
    FBounds.Left := AValue;
end;

procedure TDockZone.SetTop(const AValue: Integer);
begin
  if not Visible then
    Exit;

  if (FChildControl <> nil) then
    FChildControl.Top := AValue
  else
    FBounds.Top := AValue;
end;

procedure TDockZone.SetWidth(const AValue: Integer);
begin
  if not Visible then
    Exit;

  if (FChildControl <> nil) then
    FChildControl.Width := AValue
  else
    FBounds.Right := AValue;
end;

function TDockZone.GetHeight: Integer;
begin
  if not Visible then
    Exit(0);

  if FTree.RootZone = Self then
    Result := FTree.FDockSite.ClientHeight
  else
  if (FChildControl <> nil) then
    Result := FChildControl.Height
  else
  begin
    if FParentZone.Orientation = doHorizontal then
      Result := FBounds.Bottom
    else
      Result := FParentZone.Height;
  end;
end;

function TDockZone.GetLeft: Integer;
begin
  if not Visible then
    Exit(0);

  if FTree.RootZone = Self then
    Result := 0
  else
  if (FChildControl <> nil) then
    Result := FChildControl.Left
  else
  begin
    if FParentZone.Orientation = doVertical then
      Result := FBounds.Left
    else
      Result := FParentZone.Left;
  end;
end;

function TDockZone.GetLimitBegin: Integer;
// returns the left/top coord of zone.
begin
  case FOrientation of
    doHorizontal: Result := Top;
    doVertical: Result := Left;
  else
    raise Exception.Create('TDockZone.GetLimitBegin');
  end;
end;

function TDockZone.GetLimitSize: Integer;
// returns the zone width/height.
begin
  case FOrientation of
    doHorizontal: Result := Height;
    doVertical: Result := Width;
  else
    raise Exception.Create('TDockZone.GetLimitSize');
  end;
end;

function TDockZone.GetTop: Integer;
begin
  if not Visible then
    Exit(0);

  if FTree.RootZone = Self then
    Result := 0
  else
  if (FChildControl <> nil) then
    Result := FChildControl.Top
  else
  begin
    if FParentZone.Orientation = doHorizontal then
      Result := FBounds.Top
    else
      Result := FParentZone.Top;
  end;
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
    Exit(0);

  if FTree.RootZone = Self then
    Result := FTree.FDockSite.ClientWidth
  else
  if (FChildControl <> nil) then
    Result := FChildControl.Width
  else
  begin
    if FParentZone.Orientation = doVertical then
      Result := FBounds.Right
    else
      Result := FParentZone.Width;
  end;
end;

function TDockZone.GetNextVisibleZone: TDockZone;
begin
  Result := FNextSibling;
  while Assigned(Result) and not Result.Visible do
    Result := Result.FNextSibling;
end;

constructor TDockZone.Create(TheTree: TDockTree; TheChildControl: TControl);
begin
  FTree := TheTree;
  FChildControl := TheChildControl;
  FBounds := Rect(0, 0, 0, 0);
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

procedure TDockZone.AddSibling(NewZone: TDockZone; InsertAt: TAlign);
var
  LinkAfter: TDockZone;
begin
  case InsertAt of
    alLeft, alTop:  LinkAfter := FPrevSibling;
    alRight, alBottom: LinkAfter := Self;
  else
    raise Exception.Create('TDockZone.AddSibling: unhandled insertion');
  end;
  if LinkAfter = nil then
    Parent.AddAsFirstChild(NewZone)
  else
  begin
    NewZone.FPrevSibling := LinkAfter;
    NewZone.FNextSibling := LinkAfter.NextSibling;
    NewZone.FParentZone := Parent;
    if LinkAfter.NextSibling <> nil then
      LinkAfter.NextSibling.FPrevSibling := NewZone;
    LinkAfter.FNextSibling := NewZone;
  end;
end;

procedure TDockZone.ReplaceChild(OldChild, NewChild: TDockZone);
begin
  NewChild.FParentZone := Self;
  NewChild.FNextSibling := OldChild.FNextSibling;
  NewChild.FPrevSibling := OldChild.FPrevSibling;
  if NewChild.FNextSibling <> nil then
    NewChild.FNextSibling.FPrevSibling := NewChild;
  if NewChild.FPrevSibling <> nil then
    NewChild.FPrevSibling.FNextSibling := NewChild;
  if FFirstChildZone = OldChild then
    FFirstChildZone := NewChild;
  OldChild.FNextSibling := nil;
  OldChild.FPrevSibling := nil;
  OldChild.FParentZone := nil;
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
  if ChildZone.Parent <> Self then
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
