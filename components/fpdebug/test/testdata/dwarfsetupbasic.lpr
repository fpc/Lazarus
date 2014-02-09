program DwarfSetupBasic;
{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}

// TODO: add integer, cardinal, ...
type
  TEnum0 = (e0a);
  TEnum1 = (e1a, e1b, e1c);
  TEnum2 = (e2a, e2b, e2c, e2d, e2e, e2f, e2g, e2h, e2i);
  TEnum3 = (e3a, e3b, e3c, e3d, e3e, e3f, e3g, e3h,
            e3i, e3j, e3k, e3l, e3m, e3n, e3o, e3p,
            e3q);
  TEnumX = (eXa, eXb := 10, eXc := 3);

  TSet0 = Set of TEnum0;
  TSet1 = Set of TEnum1;
  TSet2 = Set of TEnum2;
  TSet3 = Set of TEnum3;
  TSetX1 = Set of (s1a, s1b, s1c);

var // Globals
  VarEnum0: TEnum0;
  VarEnum1: TEnum1;
  VarEnum2: TEnum2;
  VarEnum3: TEnum3;
  VarEnumX: TEnumX;

  VarSet0: TSet0;
  VarSet1: TSet1;
  VarSet2: TSet2;
  VarSet3: TSet3;
  VarSetX1: TSetX1;


begin
  VarEnum0 := e0a;
  VarEnum1 := e1a;
  VarEnum2 := e2a;
  VarEnum3 := e3a;
  VarEnumX := eXa;

  VarSet0  := [];
  VarSet1  := [];
  VarSet2  := [];
  VarSet3  := [];
  VarSetX1 := [];
end.
