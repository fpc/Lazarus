{%MainUnit ../controls.pp}

{******************************************************************************
                                     TDockTree
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
  Traverse(Zone.FirstChild); // destroy all children
  Zone.Free; // destroy zone itself
end;

procedure TDockTree.SetDockSite(const AValue: TWinControl);
begin
  //DebugLn(['TDockTree.SetDockSite ',DbgSName(AValue)]);
  FDockSite:=AValue;
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
var
  Zone: TDockZone;
begin
  // return bounds of control dock zone and if not exits then (0,0,0,0)
  Zone := RootZone.FindZone(AControl);
  if Zone <> nil then
    ControlBounds := Rect(Zone.Left, Zone.Top, Zone.Width, Zone.Height)
  else
    ControlBounds := Rect(0, 0, 0, 0);
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

procedure TDockTree.MessageHandler(Sender: TControl; var Message: TLMessage);
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
var
  DropZone: TDockZone;
  R: TRect;
begin
  //determine drop zone
  if DropCtl = nil then
  begin
    DropZone := RootZone;
    R := DockSite.ClientRect;
  end 
  else
  begin
    DropZone := RootZone.FindZone(DropCtl);
    if DropZone = nil then
      DropZone := RootZone;
    //get the zone extent.
    with DropZone do
      R := Rect(Left, Top, Width, Height);
  end;

  // if there are no zones then use the whole rect
  // else split drop zone
  if RootZone.FirstChild <> nil then
  begin
    //shrink rect
    case DropAlign of
      alTop:
        R.Bottom := R.Bottom div 2;
      alBottom:
        begin
          R.Bottom := R.Bottom div 2;
          inc(R.Top, R.Bottom);
        end;
      alLeft:
        R.Right := R.Right div 2;
      alRight:
        begin
          R.Right := R.Right div 2;
          inc(R.Left, R.Right);
        end;
    end;
  end;

  //client to screen
  R.TopLeft := DockSite.ClientToScreen(R.TopLeft);

  //width/height to right/bottom
  inc(R.Bottom, R.Top);
  inc(R.Right, R.Left);

  //store result
  DockRect := r;
  debugln(['TDockTree.PositionDockRect ',dbgs(DockRect)]);
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
  DockSite := TheDockSite;
  inherited Create(TheDockSite);
  FBorderWidth := 4;
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

procedure TDockTree.DumpLayout(FileName: String);
begin

end;

// included by controls.pp
