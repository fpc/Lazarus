{%MainUnit ../controls.pp}

{******************************************************************************
                                     TDockTree
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
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
begin
  // ToDo
end;

procedure TDockTree.AdjustDockRect(AControl: TControl; var ARect: TRect);
begin
  // Allocate room for the caption on the left or top
  if FDockSite.Align in [alTop, alBottom] then
    Inc(ARect.Left,FGrabberSize)
  else
    Inc(ARect.Top,FGrabberSize);
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
  var ControlBounds: TRect);
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
var
  i: Integer;
begin
  FDockZoneClass:=TDockZone;
  inherited Create;
  FBorderWidth:=4;
  FDockSite:=TheDockSite;
  FGrabberSize:=DefaultDockGrabberSize;
  FGrabbersOnTop:=(FDockSite.Align <> alTop) and (FDockSite.Align <> alBottom);
  FTopZone:=FDockZoneClass.Create(Self);
  // insert existing controls into tree
  BeginUpdate;
  try
    for i:=0 to FDockSite.ControlCount-1 do
      InsertControl(FDockSite.Controls[i],alLeft,nil);
    FTopZone.ResetChildren;
  finally
    EndUpdate;
  end;
end;

destructor TDockTree.Destroy;
begin
  DeleteZone(FTopZone);
  inherited Destroy;
end;

procedure TDockTree.PaintSite(DC: HDC);
begin
  // TODO
end;

// included by controls.pp

