{%MainUnit ../actnlist.pas}

{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

function TShortCutList.Add(const S: String): Integer;
var
  ShortCut: TShortCut;
  p: Pointer;
begin
  Result := inherited Add(S);
  ShortCut := TextToShortCut(S);
  p := Pointer(PtrUInt(ShortCut));
  Objects[Result] := TObject(p);
end;

function TShortCutList.GetShortCuts(Index: Integer): TShortCut;
begin
  Result := TShortCut(PtrUInt(Objects[Index]));
end;

function TShortCutList.IndexOfShortCut(const Shortcut: TShortCut): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if TShortCut(PtrUInt(Objects[I])) = ShortCut then
    begin
      Result := I;
      break;
    end;
end;

// included by actnlist.pas
