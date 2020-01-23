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
  inherited SetParent(AParent);

  if (Parent <> nil) and (Parent is TNotebook) then
  begin
    // add to new pagelist
    ParentNotebook := TNotebook(Parent);
    i := ParentNotebook.Pages.IndexOfObject(Self);
    if i < 0 then
      ParentNotebook.Pages.AddObject(Name, Self);
  end;

  //if old and new parent is a TNotebook then remove the page from the old one
  if (OldParent <> nil) and (OldParent is TNotebook) and
    not (csDestroying in OldParent.ComponentState) then
  begin
    // remove from old pagelist
    ParentNotebook := TNotebook(OldParent);
    i := ParentNotebook.FPages.IndexOfObject(Self);
    ParentNotebook.Pages.Delete(i);
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
var
  aIndex: Integer;
begin
  {$ifdef DEBUG_NEW_NOTEBOOK}
  DebugLn('[TPage.Destroy]');
  {$endif}

  if (Parent <> nil) and (Parent is TNotebook) and
    not (csDestroying in Parent.ComponentState) then
  begin
    {$ifdef DEBUG_NEW_NOTEBOOK}
    DebugLn('[TPage.Destroy] FPages.Remove(Self)');
    {$endif}
    aIndex := TNotebook(Parent).FPages.IndexOfObject(Self);
    TNotebook(Parent).FPages.Delete(aIndex);
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

class procedure TPage.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPropertyToSkip(TPage, 'ClientHeight', 'This property was published for a long time in Lazarus 0.9.31', '');
  RegisterPropertyToSkip(TPage, 'ClientWidth', 'This property was published for a long time in Lazarus 0.9.31', '');
end;
// included by extctrls.pp
