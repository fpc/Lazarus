{%MainUnit ../extctrls.pp}
{******************************************************************************
                               TNotebook
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{******************************************************************************
                                   TUNBPages
 ******************************************************************************}

constructor TUNBPages.Create(theNotebook: TNotebook);
begin
  inherited Create;
  FPageList := TObjectList.Create(False);
  FNotebook := theNotebook;
end;

destructor TUNBPages.Destroy;
begin
  FPageList.Free;
  inherited Destroy;
end;

function TUNBPages.GetNotebookOwner: TComponent;
begin
  Result := FNotebook.Owner;
  if Result = nil then
    Result := FNotebook;
end;

function TUNBPages.Get(Index: Integer): String;
begin
  if (Index < 0) or (Index >= FPageList.Count) then
    RaiseGDBException('TUNBPages.Get Index out of bounds');
  Result := TPage(fPageList[Index]).Name;
end;

function TUNBPages.GetCount: Integer;
begin
  Result := FPageList.Count;
end;

function TUNBPages.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FPageList.Count) then
    RaiseGDBException('TUNBPages.GetObject Index out of bounds');
  Result := TPage(FPageList[Index]);
end;

procedure TUNBPages.Put(Index: Integer; const S: String);
begin
  if (Index < 0) or (Index >= FPageList.Count) then
    RaiseGDBException('TUNBPages.Put Index out of bounds');
  TPage(FPageList[Index]).Name := S;
end;

function TUNBPages.Add(const S: string): Integer;
begin
  Result := AddObject(S, TPage.Create(GetNotebookOwner));
end;

function TUNBPages.AddObject(const S: string; AObject: TObject): Integer;
var
  NewPage: TPage;
begin
  Result := FPageList.Add(AObject);
  NewPage := TPage(AObject);
  if IsValidIdent(S) then NewPage.Name := S;
  NewPage.Caption := S;
  NewPage.Parent := FNotebook;
  NewPage.Align := alClient;
  NewPage.Visible := False;
  NewPage.ControlStyle := NewPage.ControlStyle + [csNoDesignVisible];
  if FNotebook.PageIndex = -1 then FNotebook.SetPageIndex(Result);
end;

procedure TUNBPages.Clear;
begin
  while FPageList.Count > 0 do
    Delete(FPageList.Count - 1);
end;

procedure TUNBPages.Delete(Index: Integer);
var
  APage: TPage;
  NewPageIndex: Integer;
begin
  // Make sure Index is in the range of valid pages to delete
  if (Index < 0) or (Index >= FPageList.Count) then Exit;

  APage := TPage(FPageList[Index]);
  FPageList.Delete(Index);
  if APage.Parent = FNotebook then begin
    Application.ReleaseComponent(APage);
    Application.ProcessMessages;
  end;

  NewPageIndex := FNotebook.FPageIndex;
  if ((Index < NewPageIndex) and (NewPageIndex > 0)) or (NewPageIndex = FNotebook.PageCount) then
    Dec(NewPageIndex);
  FNotebook.FPageIndex := -1;
  FNotebook.SetPageIndex(NewPageIndex);
end;

function TUNBPages.IndexOfObject(AObject: TObject): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to FPageList.Count - 1 do
    if TPage(FPageList[Index]) = AObject then Exit(Index);
end;

procedure TUNBPages.Insert(Index: Integer; const S: string);
var
  NewPage: TPage;
begin
  if (Index < 0) or (Index >= FPageList.Count) then
    RaiseGDBException('TUNBPages.Insert Index out of bounds');
  NewPage := TPage.Create(GetNotebookOwner);
  FPageList.Insert(Index, NewPage);
  if IsValidIdent(S) then NewPage.Name := S;
  NewPage.Caption := S;
  NewPage.Parent := FNotebook;
  NewPage.Align := alClient;
  NewPage.Visible := False;
  NewPage.ControlStyle := NewPage.ControlStyle + [csNoDesignVisible];
  if FNotebook.PageIndex >= Index then
    FNotebook.PageIndex := FNotebook.PageIndex + 1;
end;

procedure TUNBPages.Move(CurIndex, NewIndex: Integer);
var
  CurPage: TPage;
begin
  // Get active page
  CurPage := FNotebook.Page[FNotebook.FPageIndex];
  FPageList.Move(CurIndex,NewIndex);
  // Restore active page
  FNotebook.SetPageIndex(IndexOfObject(CurPage));
end;

{******************************************************************************
                               TNotebook
 ******************************************************************************}
function TNotebook.GetActivePage: String;
begin
  Result := Page[GetPageIndex].Name;
end;

function TNotebook.GetActivePageComponent: TPage;
begin
  Result := Page[GetPageIndex];
end;

function TNotebook.GetPage(AIndex: Integer): TPage;
begin
  if (AIndex < 0) or (AIndex >= FPages.Count) then
    RaiseGDBException(Format('TNotebook.GetCustomPage Index out of bounds. AIndex=%d', [AIndex]));
  Result := TPage(FPages.Objects[AIndex]);
end;

function TNotebook.GetPageCount: integer;
begin
  Result := FPages.Count;
end;

function TNotebook.GetPageIndex: Integer;
begin
  Result := FPageIndex;
end;

function TNotebook.IndexOf(APage: TPage): integer;
begin
  Result := FPages.IndexOfObject(APage);
end;

procedure TNotebook.SetPageIndex(AValue: Integer);
var
  pg: TPage;
begin
  if (AValue < -1) or (AValue >= Pages.Count) then Exit;
  if FPageIndex = AValue then Exit;

  // Hide the previously shown page
  if (FPageIndex >= 0) and (FPageIndex < Pages.Count) then
  begin
    pg := Page[FPageIndex];
    pg.ControlStyle := pg.ControlStyle + [csNoDesignVisible];
    pg.Visible := False;
  end;

  // Update the property
  FPageIndex := AValue;
  if (FPageIndex = -1) then
    Exit;

  // And show the new one
  pg := Page[FPageIndex];
  if Assigned(pg.FOnBeforeShow) then
    pg.FOnBeforeShow(Self, pg, FPageIndex); // OnBeforeShow event
  pg.Visible := True;
  pg.ControlStyle := pg.ControlStyle - [csNoDesignVisible];
  pg.Align := alClient;
end;

procedure TNotebook.SetPages(Items: TStrings);
var
  Unchanged, Index, DummyIndex, ItemsCount: Integer;
  DummyPageList: TObjectList;
  APage: TPage;

  procedure DummyDelete(aName: String);
  var
    i, aIndex: Integer;
    DelPage: TPage;
  begin
    aIndex := -1;
    for i := 0  to DummyPageList.Count - 1 do
      if TPage(DummyPageList[i]).Name = aName then begin
        aIndex := i;
        Break;
      end;
    if aIndex = -1 then Exit;
    DelPage := TPage(DummyPageList[aIndex]);
    if Items.IndexOfObject(DelPage) >= 0 then Exit;
    DummyPageList.Delete(aIndex);
    DelPage.Parent := nil;
    Application.ReleaseComponent(DelPage);
    Application.ProcessMessages;
  end;

begin
  if csDesigning in ComponentState then Exit;

  Unchanged := 0;
  while (Unchanged < Items.Count) and (Unchanged < FPages.Count)
  and (Items.Objects[Unchanged] = FPages.Objects[Unchanged])
  and (Items[Unchanged] = TPage(FPages.Objects[Unchanged]).Name) do
    Inc(Unchanged);
  if (Unchanged = FPages.Count) and (Unchanged = Items.Count) then Exit;

  DummyPageList := TObjectList.Create(False);
  try
    // move all unused/changed pages to dummy
    for Index := FPages.Count - 1 downto Unchanged do
    begin
      APage := TPage(FPages.Objects[Index]);
      APage.SetParent(nil);
      DummyPageList.Add(APage);
    end;

    // add new or changed pages to notebook
    Index := 0;
    while Index < Items.Count do begin
      // if Items comes from a other notebook, Items.Count is changed by APage.SetParent
      ItemsCount := Items.Count;
      if Assigned(Items.Objects[Index]) then begin
        DummyDelete(Items[Index]);
        APage := TPage(Items.Objects[Index]);
        if IsValidIdent(Items[Index]) then APage.Name := Items[Index];
        APage.Caption := Items[Index];
        APage.SetParent(Self);
        DummyIndex := DummyPageList.IndexOf(APage);
        if DummyIndex >= 0 then
          DummyPageList.Delete(DummyIndex);
      end else begin
        DummyDelete(Items[Index]);
        FPages.Add(Items[Index]);
      end;
      Inc(Index, 1 - (ItemsCount - Items.Count));
    end;

    // delete all unused old pages
    for Index := DummyPageList.Count - 1 downto 0 do
    begin
      APage := TPage(DummyPageList[Index]);
      DummyPageList.Delete(Index);
      APage.Parent := nil;
      Application.ReleaseComponent(APage);
      Application.ProcessMessages;
    end;

  finally
    DummyPageList.Free;
  end;
end;

constructor TNotebook.Create(TheOwner: TComponent);
var
  lSize: TSize;
begin
  inherited Create(TheOwner);

  FPageIndex := -1;
  FPages := TUNBPages.Create(Self);

  ControlStyle := []; // do not add csAcceptsControls

  // Initial size
  lSize := GetControlClassDefaultSize();
  SetInitialBounds(0, 0, lSize.CX, lSize.CY);
end;

destructor TNotebook.Destroy;
begin
  FreeAndNil(FPages);

  inherited Destroy;
end;

procedure TNotebook.ShowControl(AControl: TControl);
var
  i: Integer;
begin
  if AControl = ActivePageComponent then exit;
  i := FPages.IndexOfObject(aControl);
  if i >= 0 then
    PageIndex := i;
  inherited ShowControl(AControl);
end;

