{%MainUnit ../comctrls.pp}

{ TStatusPanels

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

constructor TStatusPanels.Create(AStatusBar: TStatusBar);
begin
  inherited Create(AStatusBar.GetPanelClass);
  FStatusBar := AStatusBar;
end;

function TStatusPanels.Add: TStatusPanel;
begin
  Result := TStatusPanel(inherited Add);
  StatusBar.UpdateHandleObject(-1, AllPanelsParts);
end;

function TStatusPanels.GetItem(Index: Integer): TStatusPanel;
begin
  Result := TStatusPanel(inherited GetItem(Index));
end;

function TStatusPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;

procedure TStatusPanels.SetItem(Index: Integer; Value: TStatusPanel);
begin
  inherited SetItem(Index, Value);
end;

procedure TStatusPanels.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
// Update panels when a user changes them in Collection Property Editor.
// Add and Delete are needed, TStatusPanel.SetIndex takes care of moving up/down.
//var SOper: string;
begin
  //WriteStr(SOper, Operation);
  //debugln(['TStatusPanels.FPOObservedChanged: Operation=', SOper]);
  case Operation of
    ooChange: ;
    ooFree: ;
    ooAddItem: Add;
    ooDeleteItem: Delete(PtrInt(Data));
    ooCustom: ;
  end;
end;

procedure TStatusPanels.Update(Item: TCollectionItem);
begin
  // Don't invalidate the whole statusbar. There is an improved
  // TStatusBar.InvalidatePanel.
  //FStatusBar.Invalidate;
end;

// included by comctrls.pp
