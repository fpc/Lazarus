(******************************************************************************
                                     TMouse
 ******************************************************************************)

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





