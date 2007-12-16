{%MainUnit ../controls.pp}

{******************************************************************************
                                     TMouse
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

constructor TMouse.Create;
begin
  inherited Create;
  FDragImmediate := true;
  FDragThreshold := 5;
  FWheelScrollLines := -1;
end;

destructor TMouse.destroy;
begin
  inherited destroy;
end;


Function TMouse.GetCapture : HWND;
Begin
  Result := FCapture;
End;


Procedure TMouse.SetCapture(const Value : HWND);
Begin
  {$IFDEF VerboseMouseCapture}
  DebugLn('TMouse.SetCapture ');
  {$ENDIF}
  FCapture := Value;
  if Value = 0 then ReleaseCapture else LCLIntf.SetCapture(Value);
  FCapture := GetCapture;
end;

Function TMouse.GetCursorPos : TPoint;
var
  p : TPoint;
Begin
  WidgetSet.GetCursorPos(p);
  Result := P;
end;

procedure TMouse.SetCursorPos(AValue : TPoint);
Begin
  WidgetSet.SetCursorPos(AValue.X, AValue.Y);
end;

function TMouse.GetWheelScrollLines: Integer;
begin
  if FWheelScrollLines = -1 then
  begin
    WidgetSet.SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @FWheelScrollLines, 0);
  end;
  Result := FWheelScrollLines;
end;

function TMouse.GetIsDragging: Boolean;
begin
  Result := ActiveDrag <> dopNone;
end;

// included by controls.pp
