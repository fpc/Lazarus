{%MainUnit ../extctrls.pp}
{******************************************************************************
                                     TPage
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{ TPage }

procedure TPage.SetParent(AParent: TWinControl);
var
  OldParent: TWinControl;
  ParentNotebook: TNotebook;
  i: integer;
begin
  if (AParent = Parent) {or (pfInserting in FFlags)} then Exit;

  OldParent := Parent;
  //if old and new parent is a TNotebook then remove the page from the old one
  if (AParent <> nil) and (OldParent <> AParent) and (OldParent <> nil) and
     (OldParent is TNotebook)  then
  begin
    // remove from old pagelist
    ParentNotebook := TNotebook(OldParent);
    i := ParentNotebook.FPageList.IndexOf(Self);
    ParentNotebook.Pages.Delete(i);
  end;

  inherited SetParent(AParent);

  if (Parent <> nil) and (Parent is TNotebook) then
  begin
    // add to new pagelist
    ParentNotebook := TNotebook(Parent);
    i := ParentNotebook.FPageList.IndexOf(Self);
    if i < 0 then
      ParentNotebook.InsertPage(Self, ParentNotebook.Pages.Count);
  end;
end;

constructor TPage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  ControlStyle := ControlStyle +
    [csAcceptsControls, csDesignFixedBounds, csNoDesignVisible, csNoFocus];

  // height and width depends on parent, align to client rect
  Align := alClient;
  Caption := '';
  Visible := False;
end;

destructor TPage.Destroy;
begin
  {$ifdef DEBUG_NEW_NOTEBOOK}
  DebugLn('[TPage.Destroy]');
  {$endif}

  if (Parent <> nil) and (Parent is TNotebook) then
  begin
    {$ifdef DEBUG_NEW_NOTEBOOK}
    DebugLn('[TPage.Destroy] FPageList.Remove(Self)');
    {$endif}
    TNotebook(Parent).FPageList.Remove(Self);
  end;

  inherited Destroy;
end;

function TPage.GetPageIndex: Integer;
begin
  if Parent is TNotebook then
    Result := TNotebook(Parent).IndexOf(Self)
  else
    Result := -1;
end;


// included by extctrls.pp
