{%MainUnit ../extctrls.pp}
{******************************************************************************
                               TNotebook
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

{******************************************************************************
                                   TUNBPages
 ******************************************************************************}

{------------------------------------------------------------------------------
  TUNBPages Constructor
 ------------------------------------------------------------------------------}
constructor TUNBPages.Create(thePageList: TListWithEvent;
  theNotebook: TNotebook);
begin
  inherited Create;
  fPageList := thePageList;
  fPageList.OnChange:=@PageListChange;
  fNotebook := theNotebook;
end;

{------------------------------------------------------------------------------
  procedure TUNBPages.PageListChange(Ptr: Pointer; AnAction: TListNotification);
 ------------------------------------------------------------------------------}
procedure TUNBPages.PageListChange(Ptr: Pointer; AnAction: TListNotification);
{var
  APage: TUNBPage;}
begin
{  if (AnAction=lnAdded) then
  begin
    APage:=TObject(Ptr) as TUNBPage;
    if not (pfInserting in APage.FFlags) then
      APage.Parent:=fNotebook;
  end;}
end;

{------------------------------------------------------------------------------
  TUNBPages Get
 ------------------------------------------------------------------------------}
function TUNBPages.Get(Index: Integer): string;
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TUNBPages.Get Index out of bounds');
  Result := TPage(fPageList[Index]).Caption;
end;

{------------------------------------------------------------------------------
  TUNBPages GetCount
 ------------------------------------------------------------------------------}
function TUNBPages.GetCount: Integer;
begin
  Result := fPageList.Count;
end;

{------------------------------------------------------------------------------
  TUNBPages GetObject
 ------------------------------------------------------------------------------}
function TUNBPages.GetObject(Index: Integer): TObject;
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TUNBPages.GetObject Index out of bounds');
  Result := TPage(fPageList[Index]);
end;

{------------------------------------------------------------------------------
  TUNBPages Put
 ------------------------------------------------------------------------------}
procedure TUNBPages.Put(Index: Integer; const S: String);
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TUNBPages.Put Index out of bounds');
  //debugln(['TUNBPages.Put ',DbgSName(FNotebook),' ',Index,' S="',S,'"']);
  TPage(fPageList[Index]).Caption := S;
end;

{------------------------------------------------------------------------------
  TUNBPages Clear
 ------------------------------------------------------------------------------}
procedure TUNBPages.Clear;
begin
  while fPageList.Count>0 do
    Delete(fPageList.Count-1);
end;

{------------------------------------------------------------------------------
  TUNBPages Delete
 ------------------------------------------------------------------------------}
procedure TUNBPages.Delete(Index: Integer);
var
  APage: TPage;
begin
  // Make sure Index is in the range of valid pages to delete
  if (Index < 0) or (Index >= fPageList.Count) then Exit;

  APage := TPage(fPageList[Index]);
  // delete handle
  APage.Parent := nil;
  // free the page
  Application.ReleaseComponent(APage);
end;

{------------------------------------------------------------------------------
  TUNBPages Insert
 ------------------------------------------------------------------------------}
procedure TUNBPages.Insert(Index: Integer; const S: String);
var
  NewPage: TPage;
  NewOwner: TComponent;
begin
  NewOwner := FNotebook.Owner;
  if NewOwner = nil then
    NewOwner := FNotebook;
  NewPage := TPage.Create(NewOwner);
  NewPage.Caption := S;

  FNoteBook.InsertPage(NewPage,Index);
end;

{******************************************************************************
                               TNotebook
 ******************************************************************************}
function TNotebook.GetActivePage: String;
begin
  Result := Page[GetPageIndex].Caption;
end;

function TNotebook.GetActivePageComponent: TPage;
begin
  Result := Page[GetPageIndex];
end;

function TNotebook.GetPage(AIndex: Integer): TPage;
begin
  if (AIndex < 0) or (AIndex >= FPageList.Count) then
    RaiseGDBException(Format('TNotebook.GetCustomPage Index out of bounds. AIndex=%d', [AIndex]));
  Result := TPage(FPageList.Items[AIndex]);
end;

function TNotebook.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TNotebook.GetPageIndex: Integer;
begin
  Result := FPageIndex;
end;

procedure TNotebook.InsertPage(APage: TPage; Index: Integer);
begin
  if FPageList.IndexOf(APage) >= 0 then Exit;

  FPageList.Insert(Index, APage);

  APage.Parent := Self;
  APage.Align := alClient;
  APage.Visible := False;
  APage.ControlStyle := APage.ControlStyle + [csNoDesignVisible];

  if PageIndex = -1 then SetPageIndex(Index);
end;

procedure TNotebook.SetPageIndex(AValue: Integer);
begin
  if (AValue < -1) or (AValue >= Pages.Count) then Exit;
  if FPageIndex = AValue then exit;

  // Hide the previously shown page
  if (FPageIndex >= 0) and (FPageIndex < Pages.Count) then
  begin
    Page[FPageIndex].ControlStyle :=
      Page[FPageIndex].ControlStyle + [csNoDesignVisible];
    Page[FPageIndex].Visible := False;
  end;

  // Update the property
  FPageIndex := AValue;

  // And show the new one
  if Assigned(Page[FPageIndex].FOnBeforeShow) then Page[FPageIndex].FOnBeforeShow(Self, Page[FPageIndex], FPageIndex); // OnBeforeShow event
  Page[FPageIndex].Visible := True;
  Page[FPageIndex].ControlStyle :=
    Page[FPageIndex].ControlStyle - [csNoDesignVisible];
  Page[FPageIndex].Align := alClient;
end;

{------------------------------------------------------------------------------
  TNotebook Constructor
 ------------------------------------------------------------------------------}
constructor TNotebook.Create(TheOwner: TComponent);
var
  lSize: TSize;
begin
  inherited Create(TheOwner);

  FPageList := TListWithEvent.Create;
  FPageIndex := -1;
  FPages := TUNBPages.Create(FPageList, Self);

  ControlStyle := []; // do not add csAcceptsControls
  TabStop := true;

  // Initial size
  lSize := GetControlClassDefaultSize();
  SetInitialBounds(0, 0, lSize.CX, lSize.CY);
end;

destructor TNotebook.Destroy;
begin
  FreeAndNil(FPageList);
  FreeAndNil(FPages);

  inherited Destroy;
end;

