unit TestWatchesUnitArray;

interface
uses sysutils, types;

procedure Test1;

implementation

var
  ArrayGlob_DynInt1: array of Integer;
  ArrayGlob_StatInt1: array [4..9] of Integer;
  ArrayGlob_StatInt2: array [-4..9] of Integer;

procedure Test1;
begin
  SetLength(ArrayGlob_DynInt1,20);
  ArrayGlob_DynInt1[0] := 5511;
  ArrayGlob_DynInt1[1] := 5512;
  ArrayGlob_DynInt1[2] := 5513;
  ArrayGlob_DynInt1[3] := 5514;
  ArrayGlob_DynInt1[4] := -5511;
  ArrayGlob_DynInt1[19] := 5500;

  ArrayGlob_StatInt1[4] := 6600;
  ArrayGlob_StatInt1[5] := 6601;
  ArrayGlob_StatInt1[6] := 6602;
  ArrayGlob_StatInt1[9] := 6699;

  ArrayGlob_StatInt2[-4] := 3300;
  ArrayGlob_StatInt2[-3] := 3301;
  ArrayGlob_StatInt2[-2] := 3302;
  ArrayGlob_StatInt2[-1] := 3303;
  ArrayGlob_StatInt2[0] := 3304;
  ArrayGlob_StatInt2[1] := 3305;


  ArrayGlob_DynInt1[9] := -5511; // BREAK

end;

end.

