{%MainUnit ../controls.pp}

{******************************************************************************
                                     TDockTree
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

const
  DefaultDockGrabberSize = 12;

procedure TDockTree.DeleteZone(Zone: TDockZone);

  procedure Traverse(Zone: TDockZone);
  begin
    if Zone <> nil then
    begin
      Traverse(Zone.FirstChild);
      Traverse(Zone.NextSibling);
      Zone.Free;
    end;
  end;
  
begin
  Traverse(Zone.FirstChild); // destroy all childs
  Zone.Free; // destroy zone itself
end;

procedure TDockTree.SetDockZoneClass(const AValue: TDockZoneClass);
begin
  FDockZoneClass := AValue;
end;

procedure TDockTree.AdjustDockRect(AControl: TControl; var ARect: TRect);
begin
end;

procedure TDockTree.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TDockTree.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount<0 then RaiseGDBException('TDockTree.EndUpdate');
  if FUpdateCount = 0 then begin
    if dtfUpdateAllNeeded in FFlags then
      UpdateAll;
  end;
end;

procedure TDockTree.GetControlBounds(AControl: TControl;
  out ControlBounds: TRect);
begin
  // ToDo
end;

function TDockTree.HitTest(const MousePos: TPoint; var HTFlag: Integer
  ): TControl;
begin
  // ToDo
  Result:=nil;
end;

procedure TDockTree.InsertControl(AControl: TControl; InsertAt: TAlign;
  DropControl: TControl);
begin
  // ToDo
end;

procedure TDockTree.LoadFromStream(SrcStream: TStream);
begin
  // ToDo
end;

procedure TDockTree.PaintDockFrame(ACanvas: TCanvas; AControl: TControl;
  const ARect: TRect);
begin
  // ToDo
end;

procedure TDockTree.PositionDockRect(AClient, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
begin
  // ToDo
end;

procedure TDockTree.RemoveControl(AControl: TControl);
begin
  // ToDo
end;

procedure TDockTree.SaveToStream(DestStream: TStream);
begin
  // ToDo
end;

procedure TDockTree.SetReplacingControl(AControl: TControl);
begin
  // ToDo
end;

procedure TDockTree.ResetBounds(Force: Boolean);
begin
  // ToDo
end;

procedure TDockTree.UpdateAll;
begin
  if FUpdateCount>0 then begin
    Include(FFlags,dtfUpdateAllNeeded);
    exit;
  end;
  Exclude(FFlags,dtfUpdateAllNeeded);
  // ToDo
end;

constructor TDockTree.Create(TheDockSite: TWinControl);
begin
  if FDockZoneClass = nil then
    FDockZoneClass := TDockZone;
  FDockSite := TheDockSite;
  inherited Create;
  FBorderWidth := 4;
  //FGrabberSize := DefaultDockGrabberSize;
  //FGrabbersOnTop:=(FDockSite.Align <> alTop) and (FDockSite.Align <> alBottom);
  FRootZone := FDockZoneClass.Create(Self, TheDockSite);
end;

destructor TDockTree.Destroy;
begin
  DeleteZone(FRootZone);
  inherited Destroy;
end;

procedure TDockTree.PaintSite(DC: HDC);
begin
  // TODO
end;

// included by controls.pp
