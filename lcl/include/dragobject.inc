{%MainUnit ../controls.pp}

{******************************************************************************
                                     TDragObject
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

{ TDragObject }

constructor TDragObject.Create(AControl : TControl);
begin
  FControl := AControl;
end;

procedure TDragObject.EndDrag(Target: TObject; X, Y: Integer);
begin
  if FControl <> nil then
    FControl.DoEndDrag(Target, X, Y);
end;

function TDragObject.GetDragImages: TDragImageList;
begin
  Result := nil;
end;

function TDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := crDrag
  else
    Result := crNoDrop;
end;

procedure TDragObject.HideDragImage;
begin
  if GetDragImages <> nil then
    GetDragImages.HideDragImage;
end;

procedure TDragObject.ShowDragImage;
begin
  if GetDragImages <> nil then
    GetDragImages.ShowDragImage;
end;

{ TDragControlObject }

function TDragControlObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := Control.DragCursor
  else
    Result := crNoDrop;
end;

function TDragControlObject.GetDragImages: TDragImageList;
begin
  Result := Control.GetDragImages;
end;

{ TDragDockObject }

procedure TDragDockObject.AdjustDockRect(ARect: TRect);

  function AbsMin(Value1, Value2: Integer): Integer;
  begin
    if Abs(Value1) < Abs(Value2) then Result := Value1
    else Result := Value2;
  end;

var
  DeltaX, DeltaY: Integer;
begin
  if (ARect.Left > FDragPos.x) or (ARect.Right < FDragPos.x) then
    DeltaX := AbsMin(ARect.Left - FDragPos.x, ARect.Right - FDragPos.x)
  else DeltaX := 0;
  if (ARect.Top > FDragPos.y) or (ARect.Bottom < FDragPos.y) then
    DeltaY := AbsMin(ARect.Top - FDragPos.y, ARect.Bottom - FDragPos.y)
  else DeltaY := 0;
  if (DeltaX <> 0) or (DeltaY <> 0) then
    OffsetRect(FDockRect, -DeltaX, -DeltaY);
end;

function TDragDockObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  Result := crDefault;
end;



// included by controls.pp
