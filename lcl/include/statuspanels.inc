{%MainUnit ../comctrls.pp}

{ TStatusPanels

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

procedure TStatusPanels.Update(Item: TCollectionItem);
begin
  // Don't invalidate the whole statusbar. There is an improved
  // TStatusBar.InvalidatePanel.
  //FStatusBar.Invalidate;
end;

// included by comctrls.pp
