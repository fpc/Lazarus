program WatchesPrg;
{$H-}

uses sysutils;

type
  TFoo = class;

  { records }
  TRec = record
    ValInt: Integer;
    ValFoo: TFoo;
  end;

  PRec = ^TRec;
  PPRec = ^PRec;

  TNewRec = type TRec;

  { Classes }

  TFoo = class
  public
    ValueInt: Integer;
    ValueFoo: TFoo;
    ValueRec: TRec;
    property PropInt: Integer read ValueInt write ValueInt;
  end;

  TFooChild = class(TFoo) end;
  TFooKid = class(TFoo) end;

  PFoo = ^TFoo;
  PPFoo = ^PFoo;
  TSamePFoo = PFoo;
  TNewPFoo = {type} PFoo; // fpc crash

  TSameFoo = TFoo;
  TNewFoo = type TFoo;
  PNewFoo = ^TNewFoo;

  { ClassesTyps }
  TFooClass = Class of TFoo;
  PFooClass = ^TFooClass;
  PPFooClass = ^PFooClass;

  TNewFooClass = class of TNewFoo;
  PNewFooClass = ^TNewFooClass;

  { strings }
  TMyAnsiString = AnsiString;
  PMyAnsiString = ^TMyAnsiString;
  PPMyAnsiString = ^PMyAnsiString;

  TNewAnsiString = type AnsiString;
  PNewAnsiString = ^TNewAnsiString;


  TMyShortString = ShortString;
  PMyShortString = ^TMyShortString;
  PPMyShortString = ^PMyShortString;

  TNewhortString = type ShortString;
  PNewhortString = ^TNewhortString;

  TMyWideString = WideString;
  PMyWideString = ^TMyWideString;
  PPMyWideString = ^PMyWideString;

  TNewWideString = type WideString;
  PNewWideString = ^TNewWideString;

  TMyString10 = String[10];
  PMyString10 = ^TMyString10;
  PPMyString10 = ^PMyString10;


  { simple }
  { variants }
  { Array }





procedure FooFunc(
  (***  parameter and var-param  ***)
      { records }
      ArgTRec: TRec;                         var VArgTRec: TRec;
      ArgPRec: PRec;                         var VArgPRec: PRec;
      ArgPPRec: PPRec;                       var VArgPPRec: PPRec;
      ArgTNewRec: TNewRec;                   var VArgTNewRec: TNewRec;

      { Classes }
      ArgTFoo: TFoo;                        var VArgTFoo: TFoo;
      ArgPFoo: PFoo;                        var VArgPFoo: PFoo;
      ArgPPFoo: PPFoo;                      var VArgPPFoo: PPFoo;
      ArgTSamePFoo: TSamePFoo;              var VArgTSamePFoo: TSamePFoo;
      ArgTNewPFoo: TNewPFoo;                var VArgTNewPFoo: TNewPFoo;

      ArgTSameFoo: TSameFoo;                 var VArgTSameFoo: TSameFoo;
      ArgTNewFoo: TNewFoo;                   var VArgTNewFoo: TNewFoo;
      ArgPNewFoo: PNewFoo;                   var VArgPNewFoo: PNewFoo;

      { ClassesTyps }
      ArgTFooClass: TFooClass;               var VArgTFooClass: TFooClass;
      ArgPFooClass: PFooClass;               var VArgPFooClass: PFooClass;
      ArgPPFooClass: PPFooClass;             var VArgPPFooClass: PPFooClass;
      ArgTNewFooClass: TNewFooClass;         var VArgTNewFooClass: TNewFooClass;
      ArgPNewFooClass: PNewFooClass;         var VArgPNewFooClass: PNewFooClass;

      { strings }
      ArgTMyAnsiString: TMyAnsiString;           var VArgTMyAnsiString: TMyAnsiString;
      ArgPMyAnsiString: PMyAnsiString;           var VArgPMyAnsiString: PMyAnsiString;
      ArgPPMyAnsiString: PPMyAnsiString;         var VArgPPMyAnsiString: PPMyAnsiString;

      ArgTNewAnsiString: TNewAnsiString;         var VArgTNewAnsiString: TNewAnsiString;
      ArgPNewAnsiString: PNewAnsiString;         var VArgPNewAnsiString: PNewAnsiString;


      ArgTMyShortString: TMyShortString;         var VArgTMyShortString: TMyShortString;
      ArgPMyShortString: PMyShortString;         var VArgPMyShortString: PMyShortString;
      ArgPPMyShortString: PPMyShortString;       var VArgPPMyShortString: PPMyShortString;

      ArgTNewhortString: TNewhortString;         var VArgTNewhortString: TNewhortString;
      ArgPNewhortString: PNewhortString;         var VArgPNewhortString: PNewhortString;

      ArgTMyWideString: TMyWideString;           var VArgTMyWideString: TMyWideString;
      ArgPMyWideString: PMyWideString;           var VArgPMyWideString: PMyWideString;
      ArgPPMyWideString: PPMyWideString;         var VArgPPMyWideString: PPMyWideString;

      ArgTNewWideString: TNewWideString;         var VArgTNewWideString: TNewWideString;
      ArgPNewWideString: PNewWideString;         var VArgPNewWideString: PNewWideString;

      ArgTMyString10: TMyString10;               var VArgTMyString10: TMyString10;
      ArgPMyString10: PMyString10;               var VArgPMyString10: PMyString10;
      ArgPPMyString10: PPMyString10;             var VArgPPMyString10: PPMyString10;

      { simple }

      ArgByte: Byte;                       var VArgByte: Byte;
      ArgWord: Word;                       var VArgWord: Word;
      ArgLongWord: LongWord;               var VArgLongWord: LongWord;
      ArgQWord: QWord;                     var VArgQWord: QWord;

      ArgShortInt: ShortInt;               var VArgShortInt: ShortInt;
      ArgSmallInt: SmallInt;               var VArgSmallInt: SmallInt;
      ArgInt: Integer;                     var VArgInt: Integer;
      ArgInt64: Int64;                     var VArgInt64: Int64;

      ArgPByte: PByte;                     var VArgPByte: PByte;
      ArgPWord: PWord;                     var VArgPWord: PWord;
      ArgPLongWord: PLongWord;             var VArgPLongWord: PLongWord;
      ArgPQWord: PQWord;                   var VArgPQWord: PQWord;

      ArgPShortInt: PShortInt;             var VArgPShortInt: PShortInt;
      ArgPSmallInt: PSmallInt;             var VArgPSmallInt: PSmallInt;
      ArgPInt: PInteger;                   var VArgPInt: PInteger;
      ArgPInt64: PInt64;                   var VArgPInt64: PInt64;

      ArgPointer: Pointer;                 var VArgPointer: Pointer;
      ArgPPointer: PPointer;               var VArgPPointer: PPointer;

      ArgDouble: Double;                   var VArgDouble: Double;
      ArgExtended: Extended;               var VArgExtended: Extended;

      Dummy: Integer
);
var
  (***  local var  ***)
  { records }
  VarTRec: TRec;
  VarPRec: PRec;
  VarPPRec: PPRec;
  VarTNewRec: TNewRec;

  PVarTRec: ^TRec;
  PVarTNewRec: ^TNewRec;

  { Classes }
  VarTFoo: TFoo;
  VarPFoo: PFoo;
  VarPPFoo: PPFoo;
  VarTSamePFoo: TSamePFoo;
  VarTNewPFoo: TNewPFoo;

  VarTSameFoo: TSameFoo;
  VarTNewFoo: TNewFoo;
  VarPNewFoo: PNewFoo;

  PVarTFoo: ^TFoo;
  PVarPFoo: ^PFoo;
  PVarTSamePFoo: ^TSamePFoo;
  PVarTSameFoo: ^TSameFoo;

  { ClassesTyps }
  VarTFooClass: TFooClass;
  VarPFooClass: PFooClass;
  VarPPFooClass: PPFooClass;
  VarTNewFooClass: TNewFooClass;
  VarPNewFooClass: PNewFooClass;

  PVarTFooClass: ^TFooClass;

  { strings }
  VarTMyAnsiString: TMyAnsiString;
  VarPMyAnsiString: PMyAnsiString;
  VarPPMyAnsiString: PPMyAnsiString;

  VarTNewAnsiString: TNewAnsiString;
  VarPNewAnsiString: PNewAnsiString;

  VarTMyShortString: TMyShortString;
  VarPMyShortString: PMyShortString;
  VarPPMyShortString: PPMyShortString;

  VarTNewhortString: TNewhortString;
  VarPNewhortString: PNewhortString;

  VarTMyWideString: TMyWideString;
  VarPMyWideString: PMyWideString;
  VarPPMyWideString: PPMyWideString;

  VarTNewWideString: TNewWideString;
  VarPNewWideString: PNewWideString;

  VarTMyString10: TMyString10;
  VarPMyString10: PMyString10;
  VarPPMyString10: PPMyString10;

  PVarAnsiString: ^AnsiString;
  PVarShortString: ^ShortString;
  PVarWideString: ^WideString;

  VarString15: string[15];

  { simple }

  VarByte: Byte;
  VarWord: Word;
  VarLongWord: LongWord;
  VarQWord: QWord;

  VarShortInt: ShortInt;
  varSmallInt: SmallInt;
  VarInt: Integer;
  VarInt64: Int64;

  VarPByte: PByte;
  VarPWord: PWord;
  VarPLongWord: PLongWord;
  VarPQWord: PQWord;

  VarPShortInt: PShortInt;
  varPSmallInt: PSmallInt;
  VarPInt: PInteger;
  VarPInt64: PInt64;

  PVarByte: ^Byte;
  PVarWord: ^Word;
  PVarLongWord: ^LongWord;
  PVarQWord: ^QWord;

  PVarShortInt: ^ShortInt;
  PvarSmallInt: ^SmallInt;
  PVarInt: ^Integer;
  PVarInt64: ^Int64;

  VarPointer: Pointer;
  VarPPointer: PPointer;
  PVarPointer: ^Pointer;

  VarDouble: Double;
  VarExtended: Extended;
  PVarDouble: ^Double;
  PVarExtended: ^Extended;


  function SubFoo(var AVal1: Integer; AVal2: Integer) : Integer;
  begin
    writeln(1);
  end;

begin
  { records }
  VarTRec := ArgTRec;
  VarPRec := ArgPRec;
  VarPPRec := ArgPPRec;
  VarTNewRec := ArgTNewRec;

  PVarTRec := @ArgTRec;
  PVarTNewRec := @ArgTNewRec;

  { Classes }
  VarTFoo := ArgTFoo;
  VarPFoo := ArgPFoo;
  VarPPFoo := ArgPPFoo;
  VarTSamePFoo := ArgTSamePFoo;
  VarTNewPFoo := ArgTNewPFoo;

  VarTSameFoo := ArgTSameFoo;
  VarTNewFoo := ArgTNewFoo;
  VarPNewFoo := ArgPNewFoo;

  PVarTFoo := @ArgTFoo;
  PVarPFoo := @ArgPFoo;
  PVarTSamePFoo := @ArgTSamePFoo;
  PVarTSameFoo :=  @ArgTSameFoo;

  { ClassesTyps }
  VarTFooClass := ArgTFooClass;
  VarPFooClass := ArgPFooClass;
  VarPPFooClass := ArgPPFooClass;
  VarTNewFooClass := ArgTNewFooClass;
  VarPNewFooClass := ArgPNewFooClass;

  PVarTFooClass := @ArgTFooClass;

  { strings }
  VarTMyAnsiString := ArgTMyAnsiString + '-var';
  VarPMyAnsiString := ArgPMyAnsiString;
  VarPPMyAnsiString := ArgPPMyAnsiString;

  VarTNewAnsiString := ArgTNewAnsiString + '-var';
  VarPNewAnsiString := ArgPNewAnsiString;

  VarTMyShortString := ArgTMyShortString + '-var';
  VarPMyShortString := ArgPMyShortString;
  VarPPMyShortString := ArgPPMyShortString;

  VarTNewhortString := ArgTNewhortString + '-var';
  VarPNewhortString := ArgPNewhortString;

  VarTMyWideString := ArgTMyWideString + '-var';
  VarPMyWideString := ArgPMyWideString;
  VarPPMyWideString := ArgPPMyWideString;

  VarTNewWideString := ArgTNewWideString + '-var';
  VarPNewWideString := ArgPNewWideString;

  VarTMyString10 := ArgTMyString10 + '-var';
  VarPMyString10 := ArgPMyString10;
  VarPPMyString10 := ArgPPMyString10;

  PVarAnsiString := @ArgTMyAnsiString;
  PVarShortString := @ArgTMyShortString;
  PVarWideString := @ArgTMyWideString;

  VarString15 := 'T15' +#10#13 + 'L2' + #13 + 'L3' +#10 +'L4';

  { simple }

  VarByte := ArgByte + 100;
  VarWord := ArgWord + 100;
  VarLongWord := ArgLongWord + 100;
  VarQWord := ArgQWord + 100;

  VarShortInt := ArgShortInt + 100;
  VarSmallInt := ArgSmallInt + 100;
  VarInt := ArgInt + 100;
  VarInt64 := ArgInt64 + 100;

  VarPByte := ArgPByte;
  VarPWord := ArgPWord;
  VarPLongWord := ArgPLongWord;
  VarPQWord := ArgPQWord;

  VarPShortInt := ArgPShortInt;
  VarPSmallInt := ArgPSmallInt;
  VarPInt := ArgPInt;
  VarPInt64 := ArgPInt64;

  PVarByte := @ArgByte;
  PVarWord := @ArgWord;
  PVarLongWord := @ArgLongWord;
  PVarQWord := @ArgQWord;

  PVarShortInt := @ArgShortInt;
  PVarSmallInt := @ArgSmallInt;
  PVarInt := @ArgInt;
  PVarInt64 := @ArgInt64;

  VarPointer := ArgPointer;
  VarPPointer := ArgPPointer;
  PVarPointer := ArgPointer;

  VarDouble := ArgDouble;
  VarExtended := ArgExtended;
  PVarDouble := @ArgDouble;
  PVarExtended := @ArgExtended;


  SubFoo(VarInt, VarPInt^);
  // break on next line
  writeln(1);
end;


  (***  global var (to feed var-param)-***)
var
  { records }
  GlobTRec, GlobTRec1, GlobTRec2: TRec;
  GlobPRec: PRec;
  GlobPPRec: PPRec;
  GlobTNewRec: TNewRec;

  PGlobTRec: ^TRec;
  PGlobTNewRec: ^TNewRec;

  { Classes }
  GlobTFoo, GlobTFoo1, GlobTFoo2, GlobTFooNil: TFoo;
  GlobPFoo: PFoo;
  GlobPPFoo: PPFoo;
  GlobTSamePFoo: TSamePFoo;
  GlobTNewPFoo: TNewPFoo;

  GlobTSameFoo: TSameFoo;
  GlobTNewFoo: TNewFoo;
  GlobPNewFoo: PNewFoo;

  PGlobTFoo: ^TFoo;
  PGlobPFoo: ^PFoo;
  PGlobTSamePFoo: ^TSamePFoo;
  PGlobTSameFoo: ^TSameFoo;

  { ClassesTyps }
  GlobTFooClass: TFooClass;
  GlobPFooClass: PFooClass;
  GlobPPFooClass: PPFooClass;
  GlobTNewFooClass: TNewFooClass;
  GlobPNewFooClass: PNewFooClass;

  PGlobTFooClass: ^TFooClass;

  { strings }
  GlobTMyAnsiString: TMyAnsiString;
  GlobPMyAnsiString: PMyAnsiString;
  GlobPPMyAnsiString: PPMyAnsiString;

  GlobTNewAnsiString: TNewAnsiString;
  GlobPNewAnsiString: PNewAnsiString;

  GlobTMyShortString: TMyShortString;
  GlobPMyShortString: PMyShortString;
  GlobPPMyShortString: PPMyShortString;

  GlobTNewhortString: TNewhortString;
  GlobPNewhortString: PNewhortString;

  GlobTMyWideString: TMyWideString;
  GlobPMyWideString: PMyWideString;
  GlobPPMyWideString: PPMyWideString;

  GlobTNewWideString: TNewWideString;
  GlobPNewWideString: PNewWideString;

  GlobTMyString10: TMyString10;
  GlobPMyString10: PMyString10;
  GlobPPMyString10: PPMyString10;

  PGlobAnsiString: ^AnsiString;
  PGlobShortString: ^ShortString;
  PGlobWideString: ^WideString;

  GlobString15: string[15];

  { simple }

  GlobByte: Byte;
  GlobWord: Word;
  GlobLongWord: LongWord;
  GlobQWord: QWord;

  GlobShortInt: ShortInt;
  GlobSmallInt: SmallInt;
  GlobInt: Integer;
  GlobInt64: Int64;

  GlobPByte: PByte;
  GlobPWord: PWord;
  GlobPLongWord: PLongWord;
  GlobPQWord: PQWord;

  GlobPShortInt: PShortInt;
  GlobPSmallInt: PSmallInt;
  GlobPInt: PInteger;
  GlobPInt64: PInt64;

  PGlobByte: ^Byte;
  PGlobWord: ^Word;
  PGlobLongWord: ^LongWord;
  PGlobQWord: ^QWord;

  PGlobShortInt: ^ShortInt;
  PGlobSmallInt: ^SmallInt;
  PGlobInt: ^Integer;
  PGlobInt64: ^Int64;

  GlobPointer: Pointer;
  GlobPPointer: PPointer;
  PGlobPointer: ^Pointer;

  GlobDouble: Double;
  GlobExtended: Extended;
  PGlobDouble: ^Double;
  PGlobExtended: ^Extended;

begin
  { records }
  GlobTRec.ValInt := -1;
  GlobTRec.ValFoo := nil;
  GlobTRec1.ValInt := 1;
  GlobTRec1.ValFoo := TFoo.Create;
  GlobTRec1.ValFoo.ValueInt := 11;
  GlobTRec2.ValInt := 2;
  GlobTRec2.ValFoo := TFoo.Create;
  GlobTRec2.ValFoo.ValueInt := 22;

  GlobPRec := @GlobTRec1;
  GlobPPRec := @GlobPRec;
  GlobTNewRec.ValInt := 3;
  GlobTNewRec.ValFoo := nil;

  PGlobTRec := @GlobTNewRec;
  PGlobTNewRec := @GlobTNewRec;

  { Classes }
  GlobTFoo := TFoo.Create;
  GlobTFoo.ValueInt := -11;
  GlobTFoo1 := TFoo.Create;
  GlobTFoo1.ValueInt := 31;
  GlobTFoo2 := TFoo.Create;
  GlobTFoo2.ValueInt := 32;
  GlobTFooNil := nil;
  GlobPFoo := @GlobTFoo1;
  GlobPPFoo := @GlobPFoo;
  GlobTSamePFoo := @GlobTFoo2;
  GlobTNewPFoo := @GlobTFoo;

  GlobTSameFoo := TFoo.Create;
  GlobTSameFoo.ValueInt := 41;
  GlobTNewFoo := TNewFoo.Create;
  GlobTNewFoo.ValueInt := 42;
  GlobPNewFoo := @GlobTSameFoo;

  PGlobTFoo := @GlobTFoo;
  PGlobPFoo := @PGlobTFoo;
  PGlobTSamePFoo := @GlobTFoo;
  PGlobTSameFoo := @GlobTFoo;

  { ClassesTyps }
  GlobTFooClass := TFooKid;
  GlobPFooClass := @GlobTFooClass;
  GlobPPFooClass := @GlobPFooClass;
  GlobTNewFooClass := TNewFoo;
  GlobPNewFooClass := @GlobTNewFooClass;

  PGlobTFooClass := @GlobTNewFooClass;

  { strings }
  GlobTMyAnsiString := 'ansi';
  GlobPMyAnsiString := @GlobTMyAnsiString;
  GlobPPMyAnsiString := @GlobPMyAnsiString;

  GlobTNewAnsiString := 'newansi';
  GlobPNewAnsiString := @GlobTNewAnsiString;

  GlobTMyShortString := 'short';
  GlobPMyShortString := @GlobTMyShortString;
  GlobPPMyShortString := @GlobPMyShortString;

  GlobTNewhortString := 'newshort';
  GlobPNewhortString := @GlobTNewhortString;

  GlobTMyWideString := 'wide';
  GlobPMyWideString := @GlobTMyWideString;
  GlobPPMyWideString := @GlobPMyWideString;

  GlobTNewWideString := 'newwide';
  GlobPNewWideString := @GlobTNewWideString;

  GlobTMyString10 := 's10';
  GlobPMyString10 := @GlobTMyString10;
  GlobPPMyString10 := @GlobPMyString10;

  PGlobAnsiString := @GlobTMyAnsiString;
  PGlobShortString := @PGlobAnsiString;
  PGlobWideString := @PGlobShortString;

  GlobString15 := 'g15';

  { simple }

  GlobByte := 25;
  GlobWord := 26;
  GlobLongWord := 27;
  GlobQWord := 28;

  GlobShortInt := 35;
  GlobSmallInt := 36;
  GlobInt := 37;
  GlobInt64 := 38;

  GlobPByte := @GlobByte;
  GlobPWord := @GlobWord;
  GlobPLongWord := @GlobLongWord;
  GlobPQWord := @GlobQWord;

  GlobPShortInt := @GlobShortInt;
  GlobPSmallInt := @GlobSmallInt;
  GlobPInt := @GlobInt;
  GlobPInt64 := @GlobInt64;

  PGlobByte := @GlobByte;
  PGlobWord := @GlobWord;
  PGlobLongWord := @GlobLongWord;
  PGlobQWord := @GlobQWord;

  PGlobShortInt := @GlobShortInt;
  PGlobSmallInt := @GlobSmallInt;
  PGlobInt := @GlobInt;
  PGlobInt64 := @GlobInt64;

  GlobPointer := @GlobByte;
  GlobPPointer := @GlobPointer;
  PGlobPointer := @GlobPointer;

  GlobDouble := 1.123;
  GlobExtended := 2.345;
  PGlobDouble := @GlobDouble;
  PGlobExtended := @GlobExtended;




  FooFunc(
      { records }
      GlobTRec,                         GlobTRec,
      GlobPRec,                         GlobPRec,
      GlobPPRec,                       GlobPPRec,
      GlobTNewRec,                   GlobTNewRec,

      { Classes }
      GlobTFoo,                        GlobTFoo,
      GlobPFoo,                        GlobPFoo,
      GlobPPFoo,                      GlobPPFoo,
      GlobTSamePFoo,              GlobTSamePFoo,
      GlobTNewPFoo,                GlobTNewPFoo,

      GlobTSameFoo,                 GlobTSameFoo,
      GlobTNewFoo,                   GlobTNewFoo,
      GlobPNewFoo,                   GlobPNewFoo,

      { ClassesTyps }
      GlobTFooClass,               GlobTFooClass,
      GlobPFooClass,               GlobPFooClass,
      GlobPPFooClass,             GlobPPFooClass,
      GlobTNewFooClass,         GlobTNewFooClass,
      GlobPNewFooClass,         GlobPNewFooClass,

      { strings }
      GlobTMyAnsiString,           GlobTMyAnsiString,
      GlobPMyAnsiString,           GlobPMyAnsiString,
      GlobPPMyAnsiString,         GlobPPMyAnsiString,

      GlobTNewAnsiString,         GlobTNewAnsiString,
      GlobPNewAnsiString,         GlobPNewAnsiString,


      GlobTMyShortString,         GlobTMyShortString,
      GlobPMyShortString,         GlobPMyShortString,
      GlobPPMyShortString,       GlobPPMyShortString,

      GlobTNewhortString,         GlobTNewhortString,
      GlobPNewhortString,         GlobPNewhortString,

      GlobTMyWideString,           GlobTMyWideString,
      GlobPMyWideString,           GlobPMyWideString,
      GlobPPMyWideString,         GlobPPMyWideString,

      GlobTNewWideString,         GlobTNewWideString,
      GlobPNewWideString,         GlobPNewWideString,

      GlobTMyString10,               GlobTMyString10,
      GlobPMyString10,               GlobPMyString10,
      GlobPPMyString10,             GlobPPMyString10,

      { simple }

      GlobByte,                       GlobByte,
      GlobWord,                       GlobWord,
      GlobLongWord,               GlobLongWord,
      GlobQWord,                     GlobQWord,

      GlobShortInt,               GlobShortInt,
      GlobSmallInt,               GlobSmallInt,
      GlobInt,                     GlobInt,
      GlobInt64,                     GlobInt64,

      GlobPByte,                     GlobPByte,
      GlobPWord,                     GlobPWord,
      GlobPLongWord,             GlobPLongWord,
      GlobPQWord,                   GlobPQWord,

      GlobPShortInt,             GlobPShortInt,
      GlobPSmallInt,             GlobPSmallInt,
      GlobPInt,                   GlobPInt,
      GlobPInt64,                   GlobPInt64,

      GlobPointer,                 GlobPointer,
      GlobPPointer,               GlobPPointer,

      GlobDouble,                   GlobDouble,
      GlobExtended,               GlobExtended,

      0
  );

  // same with nil
  { records }
  //GlobTRec := nil;
  GlobPRec := nil;
  GlobPPRec := nil;
  //GlobTNewRec := nil;

  { Classes }
  GlobTFoo := nil;
  GlobPFoo := nil;
  GlobPPFoo := nil;
  GlobTSamePFoo := nil;
  GlobTNewPFoo := nil;

  GlobTSameFoo := nil;
  GlobTNewFoo := nil;
  GlobPNewFoo := nil;

  { ClassesTyps }
  GlobTFooClass := nil;
  GlobPFooClass := nil;
  GlobPPFooClass := nil;
  GlobTNewFooClass := nil;
  GlobPNewFooClass := nil;

  { strings }
  GlobTMyAnsiString := '';
  GlobPMyAnsiString := nil;
  GlobPPMyAnsiString := nil;

  GlobTNewAnsiString := '';
  GlobPNewAnsiString := nil;


  GlobTMyShortString := '';
  GlobPMyShortString := nil;
  GlobPPMyShortString := nil;

  GlobTNewhortString := '';
  GlobPNewhortString := nil;

  GlobTMyWideString := '';
  GlobPMyWideString := nil;
  GlobPPMyWideString := nil;

  GlobTNewWideString := '';
  GlobPNewWideString := nil;

  GlobTMyString10 := '';
  GlobPMyString10 := nil;
  GlobPPMyString10 := nil;

  { simple }

  GlobByte := 0;
  GlobWord := 0;
  GlobLongWord := 0;
  GlobQWord := 0;

  GlobShortInt := 0;
  GlobSmallInt := 0;
  GlobInt := 0;
  GlobInt64 := 0;

  GlobPByte := nil;
  GlobPWord := nil;
  GlobPLongWord := nil;
  GlobPQWord := nil;

  GlobPShortInt := nil;
  GlobPSmallInt := nil;
  GlobPInt := nil;
  GlobPInt64 := nil;

  GlobPointer := nil;
  GlobPPointer := nil;

  GlobDouble := 0;
  GlobExtended := 0;


  FooFunc(
      { records }
      GlobTRec,                         GlobTRec,
      GlobPRec,                         GlobPRec,
      GlobPPRec,                       GlobPPRec,
      GlobTNewRec,                   GlobTNewRec,

      { Classes }
      GlobTFoo,                        GlobTFoo,
      GlobPFoo,                        GlobPFoo,
      GlobPPFoo,                      GlobPPFoo,
      GlobTSamePFoo,              GlobTSamePFoo,
      GlobTNewPFoo,                GlobTNewPFoo,

      GlobTSameFoo,                 GlobTSameFoo,
      GlobTNewFoo,                   GlobTNewFoo,
      GlobPNewFoo,                   GlobPNewFoo,

      { ClassesTyps }
      GlobTFooClass,               GlobTFooClass,
      GlobPFooClass,               GlobPFooClass,
      GlobPPFooClass,             GlobPPFooClass,
      GlobTNewFooClass,         GlobTNewFooClass,
      GlobPNewFooClass,         GlobPNewFooClass,

      { strings }
      GlobTMyAnsiString,           GlobTMyAnsiString,
      GlobPMyAnsiString,           GlobPMyAnsiString,
      GlobPPMyAnsiString,         GlobPPMyAnsiString,

      GlobTNewAnsiString,         GlobTNewAnsiString,
      GlobPNewAnsiString,         GlobPNewAnsiString,


      GlobTMyShortString,         GlobTMyShortString,
      GlobPMyShortString,         GlobPMyShortString,
      GlobPPMyShortString,       GlobPPMyShortString,

      GlobTNewhortString,         GlobTNewhortString,
      GlobPNewhortString,         GlobPNewhortString,

      GlobTMyWideString,           GlobTMyWideString,
      GlobPMyWideString,           GlobPMyWideString,
      GlobPPMyWideString,         GlobPPMyWideString,

      GlobTNewWideString,         GlobTNewWideString,
      GlobPNewWideString,         GlobPNewWideString,

      GlobTMyString10,               GlobTMyString10,
      GlobPMyString10,               GlobPMyString10,
      GlobPPMyString10,             GlobPPMyString10,

      { simple }

      GlobByte,                       GlobByte,
      GlobWord,                       GlobWord,
      GlobLongWord,               GlobLongWord,
      GlobQWord,                     GlobQWord,

      GlobShortInt,               GlobShortInt,
      GlobSmallInt,               GlobSmallInt,
      GlobInt,                     GlobInt,
      GlobInt64,                     GlobInt64,

      GlobPByte,                     GlobPByte,
      GlobPWord,                     GlobPWord,
      GlobPLongWord,             GlobPLongWord,
      GlobPQWord,                   GlobPQWord,

      GlobPShortInt,             GlobPShortInt,
      GlobPSmallInt,             GlobPSmallInt,
      GlobPInt,                   GlobPInt,
      GlobPInt64,                   GlobPInt64,

      GlobPointer,                 GlobPointer,
      GlobPPointer,               GlobPPointer,

      GlobDouble,                   GlobDouble,
      GlobExtended,               GlobExtended,

      0
  );



  // no bother freeing mem
end.
