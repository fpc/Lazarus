// included by controls.pp

{******************************************************************************
                                     TMouse
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

constructor TMouse.Create;
begin
  inherited Create;
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
  writeln('TMouse.SetCapture ');
  {$ENDIF}
  FCapture := Value;
  if Value = 0 then ReleaseCapture else LCLLinux.SetCapture(Value);
end;

Procedure TMouse.SetCursorPos(Value : Tpoint);
Begin

end;

Function TMouse.GetCursorPos : TPoint;
var
  p : TPoint;
Begin
  GetCaretPos(p);
  Result := P;
end;

// included by controls.pp

