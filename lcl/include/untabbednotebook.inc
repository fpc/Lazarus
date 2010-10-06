{%MainUnit ../extctrls.pp}
{******************************************************************************
                               TUntabbedNotebook
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
  theUNotebook: TUntabbedNotebook);
begin
  inherited Create;
  fPageList := thePageList;
  fPageList.OnChange:=@PageListChange;
  fUNotebook := theUNotebook;
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
  Result := TUNBPage(fPageList[Index]).Caption;
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
  Result := TUNBPage(fPageList[Index]);
end;

{------------------------------------------------------------------------------
  TUNBPages Put
 ------------------------------------------------------------------------------}
procedure TUNBPages.Put(Index: Integer; const S: String);
begin
  if (Index<0) or (Index>=fPageList.Count) then
    RaiseGDBException('TUNBPages.Put Index out of bounds');
  //debugln(['TUNBPages.Put ',DbgSName(FNotebook),' ',Index,' S="',S,'"']);
  TUNBPage(fPageList[Index]).Caption := S;
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
  APage: TCustomPage;
begin
  // Make sure Index is in the range of valid pages to delete
  if (Index < 0) or (Index >= fPageList.Count) then Exit;

  APage := TCustomPage(fPageList[Index]);
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
  NewPage: TUNBPage;
  NewOwner: TComponent;
begin
  NewOwner := FUNotebook.Owner;
  if NewOwner = nil then
    NewOwner := FUNotebook;
  NewPage := TUNBPage.Create(NewOwner);
  NewPage.Caption := S;

  FUNoteBook.InsertPage(NewPage,Index);
end;

{******************************************************************************
                               TUntabbedNotebook
 ******************************************************************************}

function TUntabbedNotebook.GetPage(AIndex: Integer): TUNBPage;
begin
  if (AIndex < 0) or (AIndex >= FPageList.Count) then
    RaiseGDBException('TUntabbedNotebook.GetCustomPage Index out of bounds');
  Result := TUNBPage(FPageList.Items[AIndex]);
end;

function TUntabbedNotebook.GetPageIndex: Integer;
begin
  Result := FPageIndex;
end;

procedure TUntabbedNotebook.InsertPage(APage: TUNBPage; Index: Integer);
begin
  if FPageList.IndexOf(APage) >= 0 then Exit;

  FPageList.Insert(Index, APage);

  APage.Parent := Self;
  APage.Align := alClient;
  APage.Visible := False;
  APage.ControlStyle := APage.ControlStyle + [csNoDesignVisible];

  if PageIndex = -1 then SetPageIndex(Index);
end;

procedure TUntabbedNotebook.SetPageIndex(AValue: Integer);
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
  TUntabbedNotebook Constructor
 ------------------------------------------------------------------------------}
constructor TUntabbedNotebook.Create(TheOwner: TComponent);
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

destructor TUntabbedNotebook.Destroy;
begin
  FPageList.Free;

  inherited Destroy;
end;

