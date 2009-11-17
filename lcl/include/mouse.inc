{%MainUnit ../controls.pp}

{******************************************************************************
                                     TMouse
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

function TMouse.GetCapture: HWND;
begin
  Result := FCapture;
end;

procedure TMouse.SetCapture(const Value: HWND);
begin
  {$IFDEF VerboseMouseCapture}
  DebugLn('TMouse.SetCapture ');
  {$ENDIF}
  FCapture := Value;
  if Value = 0 then
    ReleaseCapture
  else
    LCLIntf.SetCapture(Value);
  FCapture := GetCapture;
end;

function TMouse.GetCursorPos: TPoint;
begin
  if not WidgetSet.GetCursorPos(Result) then
    Result := Point(0,0);
end;

procedure TMouse.SetCursorPos(AValue: TPoint);
begin
  WidgetSet.SetCursorPos(AValue.X, AValue.Y);
end;

function TMouse.GetWheelScrollLines: Integer;
begin
  if FWheelScrollLines = 0 then
  begin
    if not WidgetSet.SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0) then
      FWheelScrollLines := 3;
  end;
  Result := FWheelScrollLines;
end;

function TMouse.GetDragImmediate: Boolean;
begin
  Result := DragManager.DragImmediate
end;

function TMouse.GetDragThreshold: Integer;
begin
  Result := DragManager.DragThreshold
end;

procedure TMouse.SetDragImmediate(const AValue: Boolean);
begin
  DragManager.DragImmediate := AValue;
end;

procedure TMouse.SetDragThreshold(const AValue: Integer);
begin
  DragManager.DragThreshold := AValue;
end;

function TMouse.GetIsDragging: Boolean;
begin
  Result := DragManager.IsDragging;
end;


// included by controls.pp
