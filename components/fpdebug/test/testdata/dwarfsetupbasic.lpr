program DwarfSetupBasic;
{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}
{$A1}

// TODO: add integer, cardinal, ...
type
  TEnum0 = (e0a);
  TEnum1 = (e1a, e1b, e1c);
  TEnum2 = (e2a, e2b, e2c, e2d, e2e, e2f, e2g, e2h, e2i);
  TEnum3 = (e3a, e3b, e3c, e3d, e3e, e3f, e3g, e3h,
            e3i, e3j, e3k, e3l, e3m, e3n, e3o, e3p,
            e3q);
  TEnumX = (eXa, eXc := 3, eXb := 10);
  TEnumR3 = e3c..e3l;

  TSet0 = Set of TEnum0;
  TSet1 = Set of TEnum1;
  TSet2 = Set of TEnum2;
  TSet3 = Set of TEnum3;
  TSetX1 = Set of (s1a, s1b, s1c);
  TSetB1 = Set of Byte;
  TSetB2 = Set of 5..80;
  TSetC1 = Set of char;
  TSetR3 = Set of TEnumR3;

var // Globals
  VarEnum0: TEnum0;
  VarEnum1: TEnum1;
  VarEnum2: TEnum2;
  VarEnum3: TEnum3;
  VarEnumX: TEnumX;
  VarEnumR3: TEnumR3;

  VarSet0: TSet0;
  VarSet1: TSet1;
  VarSet2: TSet2;
  VarSet3: TSet3;
  VarSetX1: TSetX1;
  VarSetX2: set of (sxa, sxb, sxc, sxd);
  VarSetB1: TSetB1;
  VarSetB2: TSetB2;
  VarSetC1: TSetC1;
  VarSetC2: set of char;
  VarSetR3: TSetR3;

   VarByte: Byte;
   VarWord: word;
   VarLong: cardinal;

   VarRecEnum1: packed record  Enum1: TEnum1;  end;
   VarRecEnum2: packed record  Enum2: TEnum2;  end;
   VarRecSet1:  packed record  Set1:  TSet1;   end;
   VarRecSet2:  packed record  Set2:  TSet2;   end;
   VarRecSet3:  packed record  Set3:  TSet3;   end;
   VarRecSetB1: packed record  SetB1: TSetB1;  end;
   VarRecSetB2: packed record  SetB2: TSetB2;  end;


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
  VarSetX2 := [];
  VarSetB1 := [];
  VarSetB2 := [];
  VarSetC1 := [];

   VarByte := 0;
   VarRecEnum1.Enum1 := e1a;
end.
