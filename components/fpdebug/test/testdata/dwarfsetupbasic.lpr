program DwarfSetupBasic;
{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}
{$A1}

// Basic types
// Records are for cast incompatiblity tests (they have no ordinal value)
type
  PByte = ^Byte;
  PWord = ^Word;
  PLongWord = ^LongWord;
  PQWord = ^QWord;

  PShortInt = ^ShortInt;
  PSmallInt = ^SmallInt;
  PInteger = ^Integer;
  PInt64 = ^Int64;

  TSub1 = 1..9;
  TSub2 = 1000..90000;
  TSub3 = byte(10)..byte(250);
  TSub4 = -1..9;
  TSub5 = -11..-2;

  PSub1 = ^TSub1;
  PSub2 = ^TSub2;
  PSub3 = ^TSub3;
  PSub4 = ^TSub4;
  PSub5 = ^TSub5;

  PBoolean = ^Boolean;

  TEnum0 = (e0a);
  TEnum1 = (e1a, e1b, e1c);
  TEnum2 = (e2a, e2b, e2c, e2d, e2e, e2f, e2g, e2h, e2i);
  TEnum3 = (e3a, e3b, e3c, e3d, e3e, e3f, e3g, e3h,
            e3i, e3j, e3k, e3l, e3m, e3n, e3o, e3p,
            e3q);
  TEnumX = (eXa, eXc := 3, eXb := 10);
  TEnumR3 = e3c..e3l;

  PEnum0 = ^TEnum0;
  PEnum1 = ^TEnum1;
  PEnum2 = ^TEnum2;
  PEnum3 = ^TEnum3;
  PEnumX = ^TEnumX;
  PEnumR3 = ^TEnumR3;

  TSet0 = Set of TEnum0;
  TSet1 = Set of TEnum1;
  TSet2 = Set of TEnum2;
  TSet3 = Set of TEnum3;
  TSetX1 = Set of (s1a, s1b, s1c);
  TSetB1 = Set of Byte;
  TSetB2 = Set of 5..80;
  TSetC1 = Set of char;
  TSetR3 = Set of TEnumR3;

  TSetP0 = packed Set of TEnum0;
  TSetP1 = packed Set of TEnum1;
  TSetP2 = packed Set of TEnum2;
  TSetP3 = packed Set of TEnum3;
  TSetPX1 = packed Set of (sp1a, sp1b, sp1c);
  TSetPB1 = packed Set of Byte;
  TSetPB2 = packed Set of 5..80;
  TSetPC1 = packed Set of char;
  TSetPR3 = packed Set of TEnumR3;

  PSet0 = ^TSet0;
  PSet1 = ^TSet1;
  PSet2 = ^TSet2;
  PSet3 = ^TSet3;
  PSetX1 = ^TSetX1;
  PSetB1 = ^TSetB1;
  PSetB2 = ^TSetB2;
  PSetC1 = ^TSetC1;
  PSetR3 = ^TSetR3;

  PSetP0 = ^TSetP0;
  PSetP1 = ^TSetP1;
  PSetP2 = ^TSetP2;
  PSetP3 = ^TSetP3;
  PSetPX1 = ^TSetPX1;
  PSetPB1 = ^TSetPB1;
  PSetPB2 = ^TSetPB2;
  PSetPC1 = ^TSetPC1;
  PSetPR3 = ^TSetPR3;



var // Globals
  VarByte:  Byte;
  VarWord:  Word;
  VarLong:  LongWord;
  VarQWord: QWord;

  VarInt8:  ShortInt;
  VarInt16: SmallInt;
  VarInt32: Integer;
  VarInt64: Int64;

  VarSub1: TSub1;
  VarSub2: TSub2;
  VarSub3: TSub3;
  VarSub4: TSub4;
  VarSub5: TSub5;

  VarPByte:  PByte;
  VarPWord:  PWord;
  VarPLong:  PLongWord;
  VarPQWord: PQWord;

  VarPInt8:  PShortInt;
  VarPInt16: PSmallInt;
  VarPInt32: PInteger;
  VarPInt64: PInt64;

  VarPSub1: PSub1;
  VarPSub2: PSub2;
  VarPSub3: PSub3;
  VarPSub4: PSub4;
  VarPSub5: PSub5;



  VarBoolean: Boolean;
  VarPBoolean: PBoolean;


  VarEnum0: TEnum0;
  VarEnum1: TEnum1;
  VarEnum2: TEnum2;
  VarEnum3: TEnum3;
  VarEnum4: (e4a,e4b,e4c,e4d);
  VarEnumX: TEnumX;
  VarEnumR3: TEnumR3;

  VarPEnum0: PEnum0;
  VarPEnum1: PEnum1;
  VarPEnum2: PEnum2;
  VarPEnum3: PEnum3;
  VarPEnumX: PEnumX;
  VarPEnumR3: PEnumR3;


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

  VarPSet0: PSet0;
  VarPSet1: PSet1;
  VarPSet2: PSet2;
  VarPSet3: PSet3;
  VarPSetX1: PSetX1;
  VarPSetB1: PSetB1;
  VarPSetB2: PSetB2;
  VarPSetC1: PSetC1;
  VarPSetR3: PSetR3;

  VarPSetP0: PSetP0;
  VarPSetP1: PSetP1;
  VarPSetP2: PSetP2;
  VarPSetP3: PSetP3;
  VarPSetPX1: PSetPX1;
  VarPSetPB1: PSetPB1;
  VarPSetPB2: PSetPB2;
  VarPSetPC1: PSetPC1;
  VarPSetPR3: PSetPR3;


  /////////////////
type
  TRecByte = packed record VarByte:  Byte; end;
  TRecWord = packed record VarWord:  Word; end;
  TRecLong = packed record VarLong:  LongWord; end;
  TRecQWord = packed record VarQWord: QWord; end;

  TRecInt8 = packed record VarInt8:  ShortInt; end;
  TRecInt16 = packed record VarInt16: SmallInt; end;
  TRecInt32 = packed record VarInt32: Integer; end;
  TRecInt64 = packed record VarInt64: Int64; end;

  TRecSub1 = packed record VarSub1: TSub1; end;
  TRecSub2 = packed record VarSub2: TSub2; end;
  TRecSub3 = packed record VarSub3: TSub3; end;
  TRecSub4 = packed record VarSub4: TSub4; end;
  TRecSub5 = packed record VarSub5: TSub5; end;

  TRecPByte = packed record VarPByte:  PByte; end;
  TRecPWord = packed record VarPWord:  PWord; end;
  TRecPLong = packed record VarPLong:  PLongWord; end;
  TRecPQWord = packed record VarPQWord: PQWord; end;

  TRecPInt8 = packed record VarPInt8:  PShortInt; end;
  TRecPInt16 = packed record VarPInt16: PSmallInt; end;
  TRecPInt32 = packed record VarPInt32: PInteger; end;
  TRecPInt64 = packed record VarPInt64: PInt64; end;

  TRecPSub1 = packed record VarPSub1: PSub1; end;
  TRecPSub2 = packed record VarPSub2: PSub2; end;
  TRecPSub3 = packed record VarPSub3: PSub3; end;
  TRecPSub4 = packed record VarPSub4: PSub4; end;
  TRecPSub5 = packed record VarPSub5: PSub5; end;


  TRecBoolean = packed record VarBoolean: Boolean; end;
  TRecPBoolean = packed record VarPBoolean: PBoolean; end;


  TRecEnum0 = packed record VarEnum0: TEnum0; end;
  TRecEnum1 = packed record VarEnum1: TEnum1; end;
  TRecEnum2 = packed record VarEnum2: TEnum2; end;
  TRecEnum3 = packed record VarEnum3: TEnum3; end;
  TRecEnumX = packed record VarEnumX: TEnumX; end;
  TRecEnumR3 = packed record VarEnumR3: TEnumR3; end;

  TRecPEnum0 = packed record VarPEnum0: PEnum0; end;
  TRecPEnum1 = packed record VarPEnum1: PEnum1; end;
  TRecPEnum2 = packed record VarPEnum2: PEnum2; end;
  TRecPEnum3 = packed record VarPEnum3: PEnum3; end;
  TRecPEnumX = packed record VarPEnumX: PEnumX; end;
  TRecPEnumR3 = packed record VarPEnumR3: PEnumR3; end;


  TRecSet0 = packed record VarSet0: TSet0; end;
  TRecSet1 = packed record VarSet1: TSet1; end;
  TRecSet2 = packed record VarSet2: TSet2; end;
  TRecSet3 = packed record VarSet3: TSet3; end;
  TRecSetX1 = packed record VarSetX1: TSetX1; end;
  TRecSetX2 = packed record VarSetX2: set of (trsxa, trsxb, trsxc, trsxd); end;
  TRecSetB1 = packed record VarSetB1: TSetB1; end;
  TRecSetB2 = packed record VarSetB2: TSetB2; end;
  TRecSetC1 = packed record VarSetC1: TSetC1; end;
  TRecSetC2 = packed record VarSetC2: set of char; end;
  TRecSetR3 = packed record VarSetR3: TSetR3; end;

  TRecPSet0 = packed record VarPSet0: PSet0; end;
  TRecPSet1 = packed record VarPSet1: PSet1; end;
  TRecPSet2 = packed record VarPSet2: PSet2; end;
  TRecPSet3 = packed record VarPSet3: PSet3; end;
  TRecPSetX1 = packed record VarPSetX1: PSetX1; end;
  TRecPSetB1 = packed record VarPSetB1: PSetB1; end;
  TRecPSetB2 = packed record VarPSetB2: PSetB2; end;
  TRecPSetC1 = packed record VarPSetC1: PSetC1; end;
  TRecPSetR3 = packed record VarPSetR3: PSetR3; end;

  TRecPSetP0 = packed record VarPSetP0: PSetP0; end;
  TRecPSetP1 = packed record VarPSetP1: PSetP1; end;
  TRecPSetP2 = packed record VarPSetP2: PSetP2; end;
  TRecPSetP3 = packed record VarPSetP3: PSetP3; end;
  TRecPSetPX1 = packed record VarPSetPX1: PSetPX1; end;
  TRecPSetPB1 = packed record VarPSetPB1: PSetPB1; end;
  TRecPSetPB2 = packed record VarPSetPB2: PSetPB2; end;
  TRecPSetPC1 = packed record VarPSetPC1: PSetPC1; end;
  TRecPSetPR3 = packed record VarPSetPR3: PSetPR3; end;

var
  VarRecByte: packed record VarByte:  Byte; end;
  VarRecWord: packed record VarWord:  Word; end;
  VarRecLong: packed record VarLong:  LongWord; end;
  VarRecQWord: packed record VarQWord: QWord; end;

  VarRecInt8: packed record VarInt8:  ShortInt; end;
  VarRecInt16: packed record VarInt16: SmallInt; end;
  VarRecInt32: packed record VarInt32: Integer; end;
  VarRecInt64: packed record VarInt64: Int64; end;

  VarRecSub1: packed record VarSub1: TSub1; end;
  VarRecSub2: packed record VarSub2: TSub2; end;
  VarRecSub3: packed record VarSub3: TSub3; end;
  VarRecSub4: packed record VarSub4: TSub4; end;
  VarRecSub5: packed record VarSub5: TSub5; end;

  VarRecPByte: packed record VarPByte:  PByte; end;
  VarRecPWord: packed record VarPWord:  PWord; end;
  VarRecPLong: packed record VarPLong:  PLongWord; end;
  VarRecPQWord: packed record VarPQWord: PQWord; end;

  VarRecPInt8: packed record VarPInt8:  PShortInt; end;
  VarRecPInt16: packed record VarPInt16: PSmallInt; end;
  VarRecPInt32: packed record VarPInt32: PInteger; end;
  VarRecPInt64: packed record VarPInt64: PInt64; end;

  VarRecPSub1: packed record VarPSub1: PSub1; end;
  VarRecPSub2: packed record VarPSub2: PSub2; end;
  VarRecPSub3: packed record VarPSub3: PSub3; end;
  VarRecPSub4: packed record VarPSub4: PSub4; end;
  VarRecPSub5: packed record VarPSub5: PSub5; end;


  VarRecBoolean: packed record VarBoolean: Boolean; end;
  VarRecPBoolean: packed record VarPBoolean: PBoolean; end;


  VarRecEnum0: packed record VarEnum0: TEnum0; end;
  VarRecEnum1: packed record VarEnum1: TEnum1; end;
  VarRecEnum2: packed record VarEnum2: TEnum2; end;
  VarRecEnum3: packed record VarEnum3: TEnum3; end;
  VarRecEnumX: packed record VarEnumX: TEnumX; end;
  VarRecEnumR3: packed record VarEnumR3: TEnumR3; end;

  VarRecPEnum0: packed record VarPEnum0: PEnum0; end;
  VarRecPEnum1: packed record VarPEnum1: PEnum1; end;
  VarRecPEnum2: packed record VarPEnum2: PEnum2; end;
  VarRecPEnum3: packed record VarPEnum3: PEnum3; end;
  VarRecPEnumX: packed record VarPEnumX: PEnumX; end;
  VarRecPEnumR3: packed record VarPEnumR3: PEnumR3; end;


  VarRecSet0: packed record VarSet0: TSet0; end;
  VarRecSet1: packed record VarSet1: TSet1; end;
  VarRecSet2: packed record VarSet2: TSet2; end;
  VarRecSet3: packed record VarSet3: TSet3; end;
  VarRecSetX1: packed record VarSetX1: TSetX1; end;
  VarRecSetX2: packed record VarSetX2: set of (rsxa, rsxb, rsxc, rsxd); end;
  VarRecSetB1: packed record VarSetB1: TSetB1; end;
  VarRecSetB2: packed record VarSetB2: TSetB2; end;
  VarRecSetC1: packed record VarSetC1: TSetC1; end;
  VarRecSetC2: packed record VarSetC2: set of char; end;
  VarRecSetR3: packed record VarSetR3: TSetR3; end;

  VarRecPSet0: packed record VarPSet0: PSet0; end;
  VarRecPSet1: packed record VarPSet1: PSet1; end;
  VarRecPSet2: packed record VarPSet2: PSet2; end;
  VarRecPSet3: packed record VarPSet3: PSet3; end;
  VarRecPSetX1: packed record VarPSetX1: PSetX1; end;
  VarRecPSetB1: packed record VarPSetB1: PSetB1; end;
  VarRecPSetB2: packed record VarPSetB2: PSetB2; end;
  VarRecPSetC1: packed record VarPSetC1: PSetC1; end;
  VarRecPSetR3: packed record VarPSetR3: PSetR3; end;

  VarRecPSetP0: packed record VarPSetP0: PSetP0; end;
  VarRecPSetP1: packed record VarPSetP1: PSetP1; end;
  VarRecPSetP2: packed record VarPSetP2: PSetP2; end;
  VarRecPSetP3: packed record VarPSetP3: PSetP3; end;
  VarRecPSetPX1: packed record VarPSetPX1: PSetPX1; end;
  VarRecPSetPB1: packed record VarPSetPB1: PSetPB1; end;
  VarRecPSetPB2: packed record VarPSetPB2: PSetPB2; end;
  VarRecPSetPC1: packed record VarPSetPC1: PSetPC1; end;
  VarRecPSetPR3: packed record VarPSetPR3: PSetPR3; end;



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
end.
