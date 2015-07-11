program DwarfSetupArray;
{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}
{$A1}

type
  TDynIntArray =   Array of integer;
  TStatIntArray1 = Array [0..10] of integer;
  TStatIntArray2 = Array [5..10] of integer;

var // Globals
  VarDynIntArray: TDynIntArray;
  VarStatIntArray1: TStatIntArray1;
  VarStatIntArray2: TStatIntArray2;

begin
  VarStatIntArray1[1]:=0;
end.

