        ��  ��                  �  D   ��
 W A T C H E S S C O P E P R G . P A S       0	        // TEST_USES=WatchesScopeUnit1.pas,WatchesScopeUnit2.pas

(* Test Purpose
- Access to variables/tyes in current and all outer scopes
  (each scope has a variable with a different name)
- Choose variable of correct (most inner visible) scope
  (all scopes have a variable of the same name)
- Global vars according to the unit of the current selected stackframe
- Missing: Global vars of other units, according to order in "uses"
*)

(* Calling order / Do not change / Insertation allowed

// Nested functions

>> function FuncFoo: integer;
   >> function FuncFooNested: AnsiString;
      >> function FuncFooNestedTwice: AnsiString;
      * TEST_BREAKPOINT//=FuncFooNestedTwice
      <<

      >> function FuncFooNestedTwice2(Int_Hide_Foo: Integer): AnsiString;
      * TEST_BREAKPOINT//=FuncFooNestedTwice2
      <<

   * TEST_BREAKPOINT//=FuncFooNested
   << function FuncFooNested: AnsiString;

* TEST_BREAKPOINT//=FuncFoo
<< function FuncFoo: integer;

// Class vs Base Class + Nested Function in method (nested function access to self)
  TClassMainBaseBase = class()                // In Unit2 // private section should not be visible
  TClassMainBase = class(TClassMainBaseBase)  // In Unit1 // private section should not be visible
  TClassMain = class(TClassMainBase)
  TClassMainChild = class(TClassMain)


>> procedure TClassMainBaseBase.MethodMainBaseBase;
   >> procedure TClassMainBase.MethodMainBase;
      >> procedure TClassMain.MethodMain;

         >>>> class TClassMainChild
         >> procedure TClassMainChild.MethodMainChild;
            >> procedure MethodMainChildNested;
               >> procedure MethodMainChildNestedTwice;
               * TEST_BREAKPOINT//=MethodMainChildNestedTwice
               <<

            * TEST_BREAKPOINT//=MethodMainChildNested
            << procedure MethodMainChildNested;

         * TEST_BREAKPOINT//=MethodMainChild
         << procedure TClassMainChild.MethodMainChild;
         <<<< class TClassMainChild

      * TEST_BREAKPOINT//=MethodMain
      << procedure TClassMain.MethodMain;

   * TEST_BREAKPOINT//=MethodMainBase
   << procedure TClassMainBase.MethodMainBase;

* TEST_BREAKPOINT//=MethodMainBaseBase
<< procedure TClassMainBaseBase.MethodMainBaseBase;

*)

program WatchesScopePrg;
{$H-}

uses sysutils, Classes, WatchesScopeUnit1, WatchesScopeUnit2;

type

  { TClassMain }

  TClassMain = class(TClassMainBase)
  private
    Int_TClassMain_Priv: Integer;
  protected
    Int_TClassMain_Prot: Integer;
  public
    Int_TClassMain: Integer;
    procedure MethodMain; override;
    procedure MethodMainChild; virtual;
  public
  type
    TMainEnum = (mm1, mm2);
  end;

  { TClassMainChild }

  TClassMainChild = class(TClassMain)
  private
    Int_TClassMainChild_Priv: Integer;
  protected
    Int_TClassMainChild_Prot: Integer;
  public
    Int_TClassMainChild: Integer;

    Int_HideTest_Class: Integer; // Hide same name var in other scope
    procedure MethodMainChild; override;
  end;


type
  TMainGlobEnum = (mmG1, mmG2);
  THideMainEnum = (hmG1, hmG2);
var
  BreakDummy: Integer;
  Int_GlobalPrg: Integer;
  Int_HideTest_Class: Integer;
  Int_HideTest_Unit: Integer;

  TestClassMainChild: TClassMainChild;

  Int_Hide_Foo: Integer;
  e1: TMainGlobEnum;
  e2: THideMainEnum;

{ TClassMain }

procedure TClassMain.MethodMain;
var
  e1: TMainEnum;
begin
  e1 := mm1;

  MethodMainChild;  // call inherited class
  BreakDummy := 1; // TEST_BREAKPOINT=MethodMain
end;

procedure TClassMain.MethodMainChild;
begin
  //
end;

{ TClassMainChild }

procedure TClassMainChild.MethodMainChild;
type
  TMethodMainChildEnum = (mmC1, mmC2);
  THideMainEnum = (hmC1, hmC2);
var
  Int_MethodMainChild: Integer;

  procedure MethodMainChildNested;
  type
    TMethodMainChildNestedEnum = (mmCN1, mmCN2);
    THideMainEnum = (hmCN1, hmCN2);
  var
    Int_MethodMainChildNested: Integer;
    e1: TMethodMainChildNestedEnum;
    e2: THideMainEnum;

    procedure MethodMainChildNestedTwice;
    type
      TMethodMainChildNestedTwiceEnum = (mmCNT1, mmCNT2);
      THideMainEnum = (hmCNT1, hmCNT2);
    var
      Int_MethodMainChildNestedTwice: integer;
      e1: TMethodMainChildNestedTwiceEnum;
      e2: THideMainEnum;
    begin
      e1 := mmCNT1;
      e2 := hmCNT1;
      Int_MethodMainChildNestedTwice := 30;
      BreakDummy := 1; // TEST_BREAKPOINT=MethodMainChildNestedTwice
    end;

  begin
    e1 := mmCN1;
    e2 := hmCN1;
    Int_MethodMainChildNested := 40;
    MethodMainChildNestedTwice;
    BreakDummy := 1; // TEST_BREAKPOINT=MethodMainChildNested
  end;

var
  Int_MethodMainChild_Late: integer;
  e1: TMethodMainChildEnum;
  e2: THideMainEnum;
begin
  e1 := mmC1;
  e2 := hmC1;
  Int_MethodMainChild := 50;
  Int_MethodMainChild_Late := 52;
  Int_TClassMainChild      := 70;
  Int_TClassMainChild_Prot := 71;
  Int_TClassMainChild_Priv := 72;
  Int_TClassMain           := 80;
  Int_TClassMain_Prot := 81;
  Int_TClassMain_Priv := 82;

  Int_HideTest_Class := 3001;

  MethodMainChildNested;
  BreakDummy := 1; // TEST_BREAKPOINT=MethodMainChild
end;


function FuncFoo: integer;
  type
    TTestEnum = (te1, te2, te3);
  var
    Int_Hide_Foo: Integer;
    TestEnum: TTestEnum;


  function FuncFooNested: AnsiString;
  var
    Int_Hide_Foo: Integer;

    function FuncFooNestedTwice: AnsiString;
    var
      Int_Hide_Foo: Integer;
    begin
      Result := 'abc';
      Int_Hide_Foo := 4;
      BreakDummy := 1;  // TEST_BREAKPOINT=FuncFooNestedTwice
    end;

    function FuncFooNestedTwice2(Int_Hide_Foo: Integer): AnsiString;
    begin
      Result := 'abc2';
      Int_Hide_Foo := 5;
      BreakDummy := 1;  // TEST_BREAKPOINT=FuncFooNestedTwice2
    end;

  begin
    Result := 'bar';
    Int_Hide_Foo := 3;
    TestEnum := te3;
    FuncFooNestedTwice;
    FuncFooNestedTwice2(-1);
    BreakDummy := 1;  // TEST_BREAKPOINT=FuncFooNested
  end;

begin
  Result := 99;
  Int_Hide_Foo := 2;
  FuncFooNested;
  BreakDummy := 1; // TEST_BREAKPOINT=FuncFoo
end;

procedure TestFin;
var
  FinFoo1, FinFoo2, FinFoo3: integer;
  a: Integer;
begin
  try
    FinFoo1 := 123;
    FinFoo2 := 456;
    FinFoo3 := 789;
  finally
    a := FinFoo1;           // TEST_BREAKPOINT=FuncFin1
    a := a + FinFoo2 + 1;
    a := a + FinFoo3 + 2;   // TEST_BREAKPOINT=FuncFin2
    a := a + FinFoo1;
    FinFoo1 := a;           // TEST_BREAKPOINT=FuncFin3
  end;
end;

begin
  Unit1Init;
  Unit2Init;
  e1 := mmG1;
  e2 := hmG1;
  Int_GlobalUnit1 := 201;
  Int_GlobalUnit2 := 202;
  Int_GlobalPrg   := 101;

  Int_HideTest_Class := 3000;
  Int_HideTest_Unit := 3010;

  Int_Hide_Foo := 1;
  FuncFoo;

  TestClassMainChild := TClassMainChild.Create;

  // Call the deepest class first, and make the way up to each inherited class
  TestClassMainChild.MethodMainBaseBase();

  BreakDummy := 1; // TEST_BREAKPOINT=Prg

  TestFin;
end.
 �  H   ��
 W A T C H E S S C O P E U N I T 1 . P A S       0	        unit WatchesScopeUnit1;
{$H-}

interface

uses sysutils, Classes, WatchesScopeUnit2;

type

  { TClassMainBase }

  TClassMainBase = class(TClassMainBaseBase)
  private
    Int_TClassMainBase_Priv: Integer;
    Int_HideTest_Class: Integer;
  protected
    Int_TClassMainBase_Prot: Integer;
  public
    Int_TClassMainBase: Integer;
    procedure MethodMainBase; override;
    procedure MethodMain; virtual;
  private
  type
    TMainBaseEnum = (mmB1, mmB2);
    THideMainEnum = (hmB1, hmB2);
  end;

procedure Unit1Init;

var
  Int_GlobalUnit1: Integer;
  Int_HideTest_Class: Integer;
  Int_HideTest_Unit: Integer;
  BreakDummy1: Integer;

implementation

procedure Unit1Init;
begin
  Int_HideTest_Class := 1000;
  Int_HideTest_Unit := 1010;
end;

{ TClassMainBase }

procedure TClassMainBase.MethodMainBase;
var
  e1: TMainBaseEnum;
  e2: THideMainEnum;
begin
  e1 := mmB1;
  e2 := hmB1;

  Int_TClassMainBase      := 170;
  Int_TClassMainBase_Prot := 171;
  Int_TClassMainBase_Priv := 172;

  Int_HideTest_Class := 1001;

  MethodMain; // call inherited class
  BreakDummy1 := 1; // TEST_BREAKPOINT=MethodMainBase
end;

procedure TClassMainBase.MethodMain;
begin
  //
end;

end.

M  H   ��
 W A T C H E S S C O P E U N I T 2 . P A S       0	        unit WatchesScopeUnit2;
{$H-}

interface

uses sysutils, Classes;

type

  { TClassMainBaseBase }

  TClassMainBaseBase = class
  private
    Int_TClassMainBaseBase_Priv: Integer;
    Int_HideTest_Class: Integer;
  protected
    Int_TClassMainBaseBase_Prot: Integer;
  public
    Int_TClassMainBaseBase: Integer;
    procedure MethodMainBaseBase;
    procedure MethodMainBase; virtual;
  end;

procedure Unit2Init;

var
  Int_GlobalUnit2: Integer;
  Int_HideTest_Class: Integer;
  Int_HideTest_Unit: Integer;
  BreakDummy2: Integer;

implementation

procedure Unit2Init;
begin
  Int_HideTest_Class := 2000;
  Int_HideTest_Unit := 2010;
end;

{ TClassMainBaseBase }

procedure TClassMainBaseBase.MethodMainBaseBase;
begin
  Int_TClassMainBaseBase      := 270;
  Int_TClassMainBaseBase_Prot := 271;
  Int_TClassMainBaseBase_Priv := 272;

  Int_HideTest_Class := 2001;

  MethodMainBase; // call inherited class
  BreakDummy2 := 1; // TEST_BREAKPOINT=MethodMainBaseBase
end;

procedure TClassMainBaseBase.MethodMainBase;
begin
  //
end;

end.

   F�  D   ��
 W A T C H E S V A L U E P R G . P A S       0	        // TEST_USES=WatchesValuePrgIdent.inc

(*
  - Declare variables of every type available, including nested types, named/unnamed, ...
  - Declare in different locations: const, var, param, var param, field, ...

  Test that the debugger can read any type of value at any location
*)
program WatchesValuePrg;
{$mode objfpc}
{$LONGSTRINGS ON}
{$modeswitch advancedrecords}
{$hints off}
{$notes off}
{$warnings off}
{$inline off}
{$IfnDEF WIndows} {$Codepage utf8} {$ENDIF}

uses sysutils, Classes, variants;

function SomeFunc1(SomeValue, Foo: Integer; Bar: Word; X: Byte): Boolean;
begin result := SomeValue = 0; end;
procedure SomeProc1();
begin SomeFunc1(2,2,2,2); end;

var ValSomeFuncInt: integer;
function SomeFuncInt(): Integer;
begin
  result := ValSomeFuncInt;
  inc(ValSomeFuncInt);
end;
function SomeFuncIntRes(): Integer;
begin
  result := ValSomeFuncInt;
  ValSomeFuncInt := 0;
end;
function FuncIntAdd(a, b: Integer): Integer;
begin
  result := a+b;
end;
function FuncTooManyArg(a, b, c, d, e, f, g, h, i, j, k, l: Integer): Integer; // not enough registers to call in watch eval
begin result := 123; end;

const
  MaxListSize = high(longword) div 16;

type
{$ifdef CPU64}
  PtrUInt = type QWord;
{$endif CPU64}

{$ifdef CPU32}
  PtrUInt = type DWord;
{$endif CPU32}

  PPointerList = ^TPointerList;
  TPointerList = array[0..MaxListSize - 1] of Pointer;

  TMyStringItem = record
    FString: string;
    FObject: TObject;
  end;
  TMyStringItemListShort = array[0..10] of TMyStringItem;
  TMyStringItemList = array[0..MaxListSize - 1] of TMyStringItem;
  PMyStringItemList = ^TMyStringItemList;
  TMyStringList = class
  private
    FList: PMyStringItemList;
  end;

  TCastRecordB1 = packed record
    b: Byte;
  end;
  TCastRecordB2 = packed record
    b,b2: Byte;
  end;
  TCastRecordW1 = packed record
    w: Word;
  end;
  TCastRecordW2 = packed record
    w,w2: Word;
  end;
  TCastRecordL1 = packed record
    l: LongWord;
  end;
  TCastRecordL2 = packed record
    l,l2: LongWord;
  end;
  TCastRecordL4 = packed record
    l,l2,l3,l4: LongWord;
  end;
  TCastRecordQ2 = packed record
    q,q2: QWord;
  end;

var
  BreakDummy, BreakDummy2: PtrUInt;
  PByteDummy: PByte;
  p: Pointer;
  pw: PWord; // ensure we have the type
  InterfacedObject, InterfacedObject2: TInterfacedObject;

  variant1: variant;
  variant2: variant;
  v_rec: record
    variant1: variant;
    variant2: variant;
  end;
  v_array: array [3..4] of variant;

  SRef0, SRef1, SRef2, SRef3, SRef4: String;
  SConst: Ansistring;
  PCRef1: PChar;
  PtrRef1: Pointer;
  Short0: Shortstring;
  Short1: array [0..2] of String[10];
  ARef0, ARef1, ARef2, ARef3, ARef4: array of byte;

  VarCastRecb1: TCastRecordB1;
  VarCastRecb2: TCastRecordB2;
  VarCastRecw1: TCastRecordW1;
  VarCastRecw2: TCastRecordW2;
  VarCastRecl1: TCastRecordL1;
  VarCastRecl2: TCastRecordL2;
  VarCastRecl4: TCastRecordL4;
  VarCastRecq2: TCastRecordQ2;

type
  TClass1 = class;

  IntRange = -300..300;
  TSmallRange = 20..30;
  TTinyRange = 0..3;
  TTinyNegRange = -2..3;
  CardinalRange = 1..300;

  ShortStr1 = String[1];
  ShortStr10 = String[10];
  ShortStr255 = String[255];

  TStrA = AnsiString;
  TStrTA = type AnsiString;
  TPChr = ^Char;

  TWStrA = WideString;
  TWStrTA = type WideString;
  TPWChr = ^WideChar;

  TUStrA = UnicodeString;
  TUStrTA = type UnicodeString;
  //TPUChr = ^UnicodeChar;

  ShortRec = record // looks like shortstring
    length: byte;
    st: array [1..5] of char;
  end;

  TCharStatArray     = array [1..5] of char;
  TWCharStatArray    = array [1..5] of char;
  TIntStatArray      = array [1..5] of Integer;
  TAnsiStatArray     = array [1..5] of AnsiString;
  TShortStrStatArray = array [1..5] of ShortStr10;

  TCharDynArray      = array of char;
  TWCharDynArray     = array of widechar;
  TIntDynArray       = array of Integer;
  TAnsiDynArray      = array of AnsiString;
  TShortStrDynArray  = array of ShortStr10;

  TDynDynArrayInt    = array of array of integer;

  TRecordFive     =        record a:longint; b: byte end;
  TRecordFivePack = packed record a:longint; b: byte end;

  TRecord3Int64     =        record a,b,c: Int64; end;
  TRecord3QWord     =        record a,b,c: QWord; end;
  //PRecord3Int64     = ^TRecord3Int64;

  TObject3Int64     =        object a,b,c: Int64; end;
  TObject3Int64Ex   =        object(TObject3Int64) d: QWord; end;
  //PObject3Int64     = ^TObject3Int64;
  //PObject3Int64Ex   = ^TObject3Int64Ex;

  TObjectCreate3Int64     =        object a,b,c: Int64; public constructor Create; destructor Destroy; procedure Foo; virtual; end;
  TObjectCreate3Int64Ex   =        object(TObjectCreate3Int64) d: QWord; end;
  //PObjectCreate3Int64     = ^TObjectCreate3Int64;
  //PObjectCreate3Int64Ex   = ^TObjectCreate3Int64Ex;

  //PIUnknown = ^IUnknown;

  TFiveDynArray            =        array of          record a:longint; b: byte end;
  TFiveDynArrayPack        = packed array of          record a:longint; b: byte end;
  TFivePackDynArray        =        array of   packed record a:longint; b: byte end;
  TFivePackDynArrayPack    = packed array of   packed record a:longint; b: byte end;
  TRecFiveDynArray         =        array of   TRecordFive;
  TRecFiveDynPackArray     = packed array of   TRecordFive;
  TRecFivePackDynArray     =        array of   TRecordFivePack;
  TRecFivePackDynPackArray = packed array of   TRecordFivePack;

  TFiveStatArray            =        array [2..4] of          record a:longint; b: byte end;
  TFiveStatArrayPack        = packed array [2..4] of          record a:longint; b: byte end;
  TFivePackStatArray        =        array [2..4] of   packed record a:longint; b: byte end;
  TFivePackStatArrayPack    = packed array [2..4] of   packed record a:longint; b: byte end;
  TRecFiveStatArray         =        array [2..4] of   TRecordFive;
  TRecFiveStatPackArray     = packed array [2..4] of   TRecordFive;
  TRecFivePackStatArray     =        array [2..4] of   TRecordFivePack;
  TRecFivePackStatPackArray = packed array [2..4] of   TRecordFivePack;

  TRecordClass1     =        record Foo: TClass1; end;

  PClass1 = ^TClass1;
  TClass1a = class;
  TClass1 = class
  public
    FInt: integer;
    FDynInt: TIntDynArray;
    FAnsi: AnsiString;
    FThis: TClass1;
    FThat: TClass1a;
    FMe: PClass1;
  end;
  TClass1a = class(TClass1)
    FThisA: TClass1;
    FMeA: PClass1;
  end;

  TEnum  = (EnVal1, EnVal2, EnVal3, EnVal4);
  // TEnum16 more than 256 values => wordsized
  TEnum16  = (
    ExVal00, ExVal01, ExVal02, ExVal03, ExVal04, ExVal05, ExVal06, ExVal07, ExVal08, ExVal09, ExVal0A, ExVal0B, ExVal0C, ExVal0D, ExVal0E, ExVal0F,
    ExVal10, ExVal11, ExVal12, ExVal13, ExVal14, ExVal15, ExVal16, ExVal17, ExVal18, ExVal19, ExVal1A, ExVal1B, ExVal1C, ExVal1D, ExVal1E, ExVal1F,
    ExVal20, ExVal21, ExVal22, ExVal23, ExVal24, ExVal25, ExVal26, ExVal27, ExVal28, ExVal29, ExVal2A, ExVal2B, ExVal2C, ExVal2D, ExVal2E, ExVal2F,
    ExVal30, ExVal31, ExVal32, ExVal33, ExVal34, ExVal35, ExVal36, ExVal37, ExVal38, ExVal39, ExVal3A, ExVal3B, ExVal3C, ExVal3D, ExVal3E, ExVal3F,
    ExVal40, ExVal41, ExVal42, ExVal43, ExVal44, ExVal45, ExVal46, ExVal47, ExVal48, ExVal49, ExVal4A, ExVal4B, ExVal4C, ExVal4D, ExVal4E, ExVal4F,
    ExVal50, ExVal51, ExVal52, ExVal53, ExVal54, ExVal55, ExVal56, ExVal57, ExVal58, ExVal59, ExVal5A, ExVal5B, ExVal5C, ExVal5D, ExVal5E, ExVal5F,
    ExVal60, ExVal61, ExVal62, ExVal63, ExVal64, ExVal65, ExVal66, ExVal67, ExVal68, ExVal69, ExVal6A, ExVal6B, ExVal6C, ExVal6D, ExVal6E, ExVal6F,
    ExVal70, ExVal71, ExVal72, ExVal73, ExVal74, ExVal75, ExVal76, ExVal77, ExVal78, ExVal79, ExVal7A, ExVal7B, ExVal7C, ExVal7D, ExVal7E, ExVal7F,
    ExVal80, ExVal81, ExVal82, ExVal83, ExVal84, ExVal85, ExVal86, ExVal87, ExVal88, ExVal89, ExVal8A, ExVal8B, ExVal8C, ExVal8D, ExVal8E, ExVal8F,
    ExVal90, ExVal91, ExVal92, ExVal93, ExVal94, ExVal95, ExVal96, ExVal97, ExVal98, ExVal99, ExVal9A, ExVal9B, ExVal9C, ExVal9D, ExVal9E, ExVal9F,
    ExValA0, ExValA1, ExValA2, ExValA3, ExValA4, ExValA5, ExValA6, ExValA7, ExValA8, ExValA9, ExValAA, ExValAB, ExValAC, ExValAD, ExValAE, ExValAF,
    ExValB0, ExValB1, ExValB2, ExValB3, ExValB4, ExValB5, ExValB6, ExValB7, ExValB8, ExValB9, ExValBA, ExValBB, ExValBC, ExValBD, ExValBE, ExValBF,
    ExValC0, ExValC1, ExValC2, ExValC3, ExValC4, ExValC5, ExValC6, ExValC7, ExValC8, ExValC9, ExValCA, ExValCB, ExValCC, ExValCD, ExValCE, ExValCF,
    ExValD0, ExValD1, ExValD2, ExValD3, ExValD4, ExValD5, ExValD6, ExValD7, ExValD8, ExValD9, ExValDA, ExValDB, ExValDC, ExValDD, ExValDE, ExValDF,
    ExValE0, ExValE1, ExValE2, ExValE3, ExValE4, ExValE5, ExValE6, ExValE7, ExValE8, ExValE9, ExValEA, ExValEB, ExValEC, ExValED, ExValEE, ExValEF,
    ExValF0, ExValF1, ExValF2, ExValF3, ExValF4, ExValF5, ExValF6, ExValF7, ExValF8, ExValF9, ExValFA, ExValFB, ExValFC, ExValFD, ExValFE, ExValFF,
    ExValX0, ExValX1, ExValX2, ExValX3, ExValX4, ExValX5, ExValX6, ExValX7, ExValX8, ExValX9, ExValXA, ExValXB, ExValXC, ExValXD, ExValXE, ExValXF
  );
  TEnumSub =  EnVal1..EnVal2;
  TEnum2 = (EnVal21= 3, EnVal22=4, EnVal23=7, EnVal24=10, EnVal25=30);
  TEnum3  = (EnVal31, EnVal32);
  TEnum4  = ( // 12 values for 16 bit set  (leave some unused)
    E4Val00, E4Val01, E4Val02, E4Val03, E4Val04, E4Val05, E4Val06, E4Val07, E4Val08, E4Val09, E4Val0A, E4Val0B
  );
  TEnum5  = ( // 20 values for 24 bit set  (leave some unused)
    E5Val00, E5Val01, E5Val02, E5Val03, E5Val04, E5Val05, E5Val06, E5Val07, E5Val08, E5Val09, E5Val0A, E5Val0B, E5Val0C, E5Val0D, E5Val0E, E5Val0F,
    E5Val10, E5Val11, E5Val12, E5Val13
  );
  TEnum6  = ( // 28 values for 32 bit set  (leave some unused)
    E6Val00, E6Val01, E6Val02, E6Val03, E6Val04, E6Val05, E6Val06, E6Val07, E6Val08, E6Val09, E6Val0A, E6Val0B, E6Val0C, E6Val0D, E6Val0E, E6Val0F,
    E6Val10, E6Val11, E6Val12, E6Val13, E6Val14, E6Val15, E6Val16, E6Val17, E6Val18, E6Val19, E6Val1A, E6Val1B
  );
  TEnum7  = ( // 60 values for 8 byte set  (leave some unused)
    E7Val00, E7Val01, E7Val02, E7Val03, E7Val04, E7Val05, E7Val06, E7Val07, E7Val08, E7Val09, E7Val0A, E7Val0B, E7Val0C, E7Val0D, E7Val0E, E7Val0F,
    E7Val10, E7Val11, E7Val12, E7Val13, E7Val14, E7Val15, E7Val16, E7Val17, E7Val18, E7Val19, E7Val1A, E7Val1B, E7Val1C, E7Val1D, E7Val1E, E7Val1F,
    E7Val20, E7Val21, E7Val22, E7Val23, E7Val24, E7Val25, E7Val26, E7Val27, E7Val28, E7Val29, E7Val2A, E7Val2B, E7Val2C, E7Val2D, E7Val2E, E7Val2F,
    E7Val30, E7Val31, E7Val32, E7Val33, E7Val34, E7Val35, E7Val36, E7Val37, E7Val38, E7Val39, E7Val3A, E7Val3B
  );
  TEnum8  = ( // 92 values for 10 byte set  (leave some unused)
    E8Val00, E8Val01, E8Val02, E8Val03, E8Val04, E8Val05, E8Val06, E8Val07, E8Val08, E8Val09, E8Val0A, E8Val0B, E8Val0C, E8Val0D, E8Val0E, E8Val0F,
    E8Val10, E8Val11, E8Val12, E8Val13, E8Val14, E8Val15, E8Val16, E8Val17, E8Val18, E8Val19, E8Val1A, E8Val1B, E8Val1C, E8Val1D, E8Val1E, E8Val1F,
    E8Val20, E8Val21, E8Val22, E8Val23, E8Val24, E8Val25, E8Val26, E8Val27, E8Val28, E8Val29, E8Val2A, E8Val2B, E8Val2C, E8Val2D, E8Val2E, E8Val2F,
    E8Val30, E8Val31, E8Val32, E8Val33, E8Val34, E8Val35, E8Val36, E8Val37, E8Val38, E8Val39, E8Val3A, E8Val3B, E8Val3C, E8Val3D, E8Val3E, E8Val3F,
    E8Val40, E8Val41, E8Val42, E8Val43, E8Val44, E8Val45, E8Val46, E8Val47, E8Val48, E8Val49, E8Val4A, E8Val4B, E8Val4C, E8Val4D, E8Val4E, E8Val4F,
    E8Val50, E8Val51, E8Val52, E8Val53, E8Val54, E8Val55, E8Val56, E8Val57, E8Val58, E8Val59, E8Val5A, E8Val5B
  );

  TEnumX0 = (EnXVal01= -503, EnXVal02= 4, EnXVal03= 7, EnXVal04= 510);
  {$PackEnum 1}
  TEnumX1  = (EnXVal11=   -3, EnXVal12= 4, EnXVal13= 7, EnXVal14=  10);
  TEnumX1a = (EnXValA11=   1, EnXValA12= 4, EnXValA13= 7, EnXValA14=  190);
  {$PackEnum 2}
  TEnumX2 = (EnXVal21= -203, EnXVal22= 4, EnXVal23= 7, EnXVal24= 210);
  {$PackEnum default}


  TSet   = set of TEnum;
  TSet3  = set of TEnum3;
  TSmallRangeSet = set of TSmallRange;
  TSet4  = set of TEnum4; // 2 byte
  TSet5  = set of TEnum5; // 3 byte
  TSet6  = set of TEnum6; // 4 byte
  TSet7  = set of TEnum7; // 8 byte
  TSet8  = set of TEnum8; // 10 byte

  TArrayEnum = array [TEnum] of word;
  TArrayEnumSub = array [TEnumSub] of word;
  TArrayEnumElem = array [EnVal1..EnVal4] of word;
  TArrayEnumSubElem = array [EnVal1..EnVal2] of word;

  TBitPackBoolArray     = bitpacked array [0..3] of Boolean;
  TBitPackTinyArray     = bitpacked array [0..3] of TTinyRange;
  TBitPackTinyNegArray  = bitpacked array [0..3] of TTinyNegRange;
  TBitPackEnumArray     = bitpacked array [0..3] of TEnum;
  TBitPackEnum3Array    = bitpacked array [0..3] of TEnum3;
  TBitPackSetArray      = bitpacked array [0..3] of TSet;
  TBitPackSet3Array     = bitpacked array [0..3] of TSet3;

  TBitPackBoolArray2     = bitpacked array [0..1, 0..2] of Boolean;
  TBitPackTinyArray2     = bitpacked array [0..1, 0..2] of TTinyRange;
  TBitPackTinyNegArray2  = bitpacked array [0..1, 0..2] of TTinyNegRange;
  TBitPackEnumArray2     = bitpacked array [0..1, 0..2] of TEnum;
  TBitPackEnum3Array2    = bitpacked array [0..1, 0..2] of TEnum3;
  TBitPackSetArray2      = bitpacked array [0..1, 0..2] of TSet;
  TBitPackSet3Array2     = bitpacked array [0..1, 0..2] of TSet3;

  TBitPackBoolRecord     = bitpacked record a,b,c,d,e: Boolean; end;
  TBitPackTinyRecord     = bitpacked record a,b,c,d,e: TTinyRange; end;
  TBitPackTinyNegRecord  = bitpacked record a,b,c,d,e: TTinyNegRange; end;
  TBitPackEnumRecord     = bitpacked record a,b,c,d,e: TEnum; end;
  TBitPackEnum3Record    = bitpacked record a,b,c,d,e: TEnum3; end;
  TBitPackSetRecord      = bitpacked record a,b,c,d,e: TSet; end;
  TBitPackSet3Record     = bitpacked record a,b,c,d,e: TSet3; end;

  TBitPackBoolArrayRecord     = bitpacked record a,b: TBitPackBoolArray; end;
  TBitPackTinyArrayRecord     = bitpacked record a,b: TBitPackTinyArray; end;
  TBitPackTinyNegArrayRecord  = bitpacked record a,b: TBitPackTinyNegArray; end;
  TBitPackEnumArrayRecord     = bitpacked record a,b: TBitPackEnumArray; end;
  TBitPackEnum3ArrayRecord    = bitpacked record a,b: TBitPackEnum3Array; end;
  TBitPackSetArrayRecord      = bitpacked record a,b: TBitPackSetArray; end;
  TBitPackSet3ArrayRecord     = bitpacked record a,b: TBitPackSet3Array; end;

  TBitPackBoolRecordArray     = bitpacked array [0..3] of bitpacked record a,b,c: Boolean; end;
  TBitPackTinyRecordArray     = bitpacked array [0..3] of bitpacked record a,b,c: TTinyRange; end;
  TBitPackTinyNegRecordArray  = bitpacked array [0..3] of bitpacked record a,b,c: TTinyNegRange; end;
  TBitPackEnumRecordArray     = bitpacked array [0..3] of bitpacked record a,b,c: TEnum; end;
  TBitPackEnum3RecordArray    = bitpacked array [0..3] of bitpacked record a,b,c: TEnum3; end;
  TBitPackSetRecordArray      = bitpacked array [0..3] of bitpacked record a,b,c: TSet; end;
  TBitPackSet3RecordArray     = bitpacked array [0..3] of bitpacked record a,b,c: TSet3; end;

  TBitPackBoolTRecordArray     = bitpacked array [0..3] of TBitPackBoolRecord;
  TBitPackTinyTRecordArray     = bitpacked array [0..3] of TBitPackTinyRecord;
  TBitPackTinyNegTRecordArray  = bitpacked array [0..3] of TBitPackTinyNegRecord;
  TBitPackEnumTRecordArray     = bitpacked array [0..3] of TBitPackEnumRecord;
  TBitPackEnum3TRecordArray    = bitpacked array [0..3] of TBitPackEnum3Record;
  TBitPackSetTRecordArray      = bitpacked array [0..3] of TBitPackSetRecord;
  TBitPackSet3TRecordArray     = bitpacked array [0..3] of TBitPackSet3Record;

  TBitSize = -7..7;
  TFpDbgValueSize = bitpacked record
    Size: Int64;        // Also used for stried => can be negative
    BitSize: TBitSize;  // Must have the same sign as Size
  end;

  // recursive declaration
  TSize = record
    cx : Longint; cy : Longint;
   public
  {$if FPC_FULLVERSION >= 30000}
     constructor Create(asz :TSize);
  {$ENDIF}
  end;

  TFunc1 = function(SomeValue, Foo: Integer; Bar: Word; X: Byte): Boolean;
  TProc1 = procedure();
  TMeth1 = function(AVal: Integer): Boolean of object;
  {$if FPC_FULLVERSION >= 30000}
  PFuncSelfRef = ^TFuncSelfRef;
  TFuncSelfRef = function(SomeValue, Foo: PFuncSelfRef): PFuncSelfRef;
  {$ENDIF}


  // Recursive pointers
  TRecursePtrA1 = ^TRecursePtrA2;
  TRecursePtrA2 = ^TRecursePtrA1;

  TRecursePtrB1 = ^TRecursePtrB2;
  TRecursePtrB2 = ^TRecursePtrB3;
  TRecursePtrB3 = ^TRecursePtrB4;
  TRecursePtrB4 = ^TRecursePtrB1;

  TRecursePtrC1  = ^TRecursePtrC2;
  TRecursePtrC2  = ^TRecursePtrC3;
  TRecursePtrC3  = ^TRecursePtrC4;
  TRecursePtrC4  = ^TRecursePtrC5;
  TRecursePtrC5  = ^TRecursePtrC6;
  TRecursePtrC6  = ^TRecursePtrC7;
  TRecursePtrC7  = ^TRecursePtrC8;
  TRecursePtrC8  = ^TRecursePtrC9;
  TRecursePtrC9  = ^TRecursePtrC10;
  TRecursePtrC10 = ^TRecursePtrC11;
  TRecursePtrC11 = ^TRecursePtrC12;
  TRecursePtrC12 = ^TRecursePtrC13;
  TRecursePtrC13 = ^TRecursePtrC14;
  TRecursePtrC14 = ^TRecursePtrC15;
  TRecursePtrC15 = ^TRecursePtrC16;
  TRecursePtrC16 = ^TRecursePtrC17;
  TRecursePtrC17 = ^TRecursePtrC18;
  TRecursePtrC18 = ^TRecursePtrC1;

var
 RecursePtrA1:  TRecursePtrA1;
 RecursePtrA2:  TRecursePtrA2;

 RecursePtrB1:  TRecursePtrB1;
 RecursePtrB2:  TRecursePtrB2;
 RecursePtrB3:  TRecursePtrB3;
 RecursePtrB4:  TRecursePtrB4;

 RecursePtrC1:  TRecursePtrC1;
 RecursePtrC2:  TRecursePtrC2;
 RecursePtrC3:  TRecursePtrC3;
 RecursePtrC4:  TRecursePtrC4;
 RecursePtrC5:  TRecursePtrC5;
 RecursePtrC6:  TRecursePtrC6;
 RecursePtrC7:  TRecursePtrC7;
 RecursePtrC8:  TRecursePtrC8;
 RecursePtrC9:  TRecursePtrC9;
 RecursePtrC10: TRecursePtrC10;
 RecursePtrC11: TRecursePtrC11;
 RecursePtrC12: TRecursePtrC12;
 RecursePtrC13: TRecursePtrC13;
 RecursePtrC14: TRecursePtrC14;
 RecursePtrC15: TRecursePtrC15;
 RecursePtrC16: TRecursePtrC16;
 RecursePtrC17: TRecursePtrC17;
 RecursePtrC18: TRecursePtrC18;

type
  (* LOCATION: TYPE *)

  // T_a_Byte = array of Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=T_a_, "_OP_== array of ", (=;//, "_O2_== array of ", _EQ_=, _BLOCK_=TestVar )
  // PT_a_Byte = ^T_a_Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=PT_a_, "_OP_={", "_O2_={", _pre3_=^T_a_, "//@@=} = ")  // }}}}

  // T_sa_Byte = array [0..2] of Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=T_sa_, "_OP_== array [0..2] of ", (=;//, "_O2_== array [0..2] of ", _EQ_=, _BLOCK_=TestVar )
  // PT_sa_Byte = ^T_sa_Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=PT_sa_, "_OP_={", "_O2_={", _pre3_=^T_sa_, "//@@=} = ")  // }}}}

  // T_nsa_Byte = array [-1..2] of Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=T_nsa_, "_OP_== array [-1..2] of ", (=;//, "_O2_== array [-1..2] of ", _EQ_=, _BLOCK_=TestVar )
  // PT_nsa_Byte = ^T_nsa_Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=PT_nsa_, "_OP_={", "_O2_={", _pre3_=^T_nsa_, "//@@=} = ")  // }}}}


  // type TxByte: type Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=Tx, "_OP_== {$IFnDEF NO_TYPE}type{$ENDIF} ", (=;//, "_O2_= = {$IFnDEF NO_TYPE}type{$ENDIF}", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestType )
  // type PTxByte: ^TxByte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=PTx, _OP_={, _O2_={, _pre3_=^Tx, "//@@=} = ", _BLOCK_=TestVar, _BLOCK2_=TestType ) //}

  // type PxByte: ^Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=Px, "_OP_==^", "_O2_==^", "(=;//", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer ) //}

  (******** CLASS ***********)

  TMyBaseClass = class
  public const
    (* LOCATION: class const *)
    // cl_c_Byte = Byte( 1 + add );
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=cl_c_, ADD=1, CHR1='c', _OP_==, _O2_=:, _EQ_==,"(nil)=nil", _BLOCK_=TestConst)
  public class var
    (* LOCATION: class var *)
    // cl_v_Byte: Byte = (1 + add);
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=cl_v_, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )
  public
    function SomeMeth1(SomeValue: Integer): Boolean;
    procedure BaseMethFoo;
  public
    (* LOCATION: field in baseclass *)
    // mbcByte: Byte;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=mbc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )
  public class var
    ClassBaseVar1: integer;
  end;
  PMyBaseClass = ^TMyBaseClass;

  TMyClass = class(TMyBaseClass)
  protected
    FFunctInt, FFunctIntConst: Integer;
  public
    (* LOCATION: field in class *)
    // mcByte: Byte;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=mc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )
    FMyStringList: TMyStringList;

    function SomeFuncIntRes(): Integer;
    function SomeFuncIntResAdd(a: integer): Integer;
    procedure MethFoo;
  public class var
    ClassVar1: integer;
  end;
  PMyClass = ^TMyClass;


  (******** OLD OBJECT ***********)

  TMyBaseOldObject = object
    (* LOCATION: field in baseclass *)
    // obcByte: Byte;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=obc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

    procedure BaseObjMethFoo;
  end;
  PMyBaseOldObject = ^TMyBaseOldObject;

  TMyOldObject = object(TMyBaseOldObject)
    (* LOCATION: field in class *)
    // ocByte: Byte;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=oc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

    procedure ObjMethFoo;
  end;
  PMyOldObject = ^TMyOldObject;


  (******** RECORD ***********)

  TMyTestRec = record
    (* LOCATION: record var *)
    // rc_f_Byte: ADD=2, CHR1='r',
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=rc_f_, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

    MyEmbedClass: TMyClass;
  end;
  PMyTestRec = ^TMyTestRec;

var
  MyClass1, MyClassBadMem: TMyClass;
  MyNilClass1: TMyClass;
  MyClass2: TMyBaseClass; (* LOCATION: field, requires typecast of containing class *)
  MyPClass1: PMyClass;
  MyPClass2: PMyBaseClass;

  MyOldObjectBase: TMyBaseOldObject;
  MyOldObject:     TMyOldObject;
  MyPOldObjectBase: PMyBaseOldObject;
  MyPOldObject:     PMyOldObject;

  MyTestRec1: TMyTestRec;
  MyPTestRec1: PMyTestRec;

  MyStringItemList: TMyStringItemListShort;
  MyStringList: TMyStringList;

  U8Data1, U8Data2: Utf8String;

  {$if FPC_FULLVERSION >= 30000}
  dummy1: PFuncSelfRef;
  {$ENDIF}

const
(* LOCATION: global const *)
  // gcByte = Byte( 1 + add );
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gc, ADD=0, CHR1='A', _OP_==, _O2_=:, _EQ_==,"(nil)=nil", _BLOCK_=TestConst)

var
(* LOCATION: global var *)
  // gvByte: Byte = (1 + add);
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )
  // gv2_Byte: Byte = (1 + add);
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv2_, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )


(* LOCATION: global var  ARRAY OF <each type> *)
  // gvaByte: array of Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gva, "_OP_=: array of", (=;//, "_O2_=: array of", _EQ_=, _BLOCK_=TestVar )
  // gvp_a_Byte: PT_a_;  // ^array of byte
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_a_, "_OP_={", "_O2_={", "//@@=} :", _pre3_=PT_a_ ) // }

(* LOCATION: global var  ARRAY [0..2] OF <each type> *)
  // gv_sa_Byte: array [0..2] of Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_sa_, "_OP_=: array [0..2] of", (=;//, "_O2_=: array [0..2] of", _EQ_=, _BLOCK_=TestVar )
  // gvp_sa_Byte: PT_sa_;  // ^array [0..2] of byte
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_sa_, "_OP_={", "_O2_={", "//@@=} :", _pre3_=PT_sa_ ) // }

(* LOCATION: global var  ARRAY [-1..2] OF <each type> *)
  // gv_sa2_Byte: array [-1..2] of Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_nsa_, "_OP_=: array [-1..2] of", (=;//, "_O2_=: array [-1..2] of", _EQ_=, _BLOCK_=TestVar )
  // gvp_sa_Byte: PT_nsa_;  // ^array [-1..2] of byte
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_nsa_, "_OP_={", "_O2_={", "//@@=} :", _pre3_=PT_nsa_ ) // }


(* LOCATION: global var  pointer <each type> *)
  // gvp_Byte: ^Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  // gvp2_Byte: ^Byte; // gvp2_Byte := @gvaByte[1];
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp2_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )

(* LOCATION: global var  pointer <each type> *)  // unreadable mem
  // gvpX_Byte: $00000001;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvpX_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )

(* LOCATION: global var  TYPE alias // NO PRE-ASSIGNED VALUE *)
  // gvp_Byte: PxByte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvpt_, "_OP_={", "_O2_={", "//@@=} :", _pre3_=Px, _BLOCK_=TestVar, _BLOCK2_=TestPointer ) // }

(* LOCATION: global var  NAMED pointer <each type> // NO PRE-ASSIGNED VALUE *)
  // gvtt_Byte: TxByte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvtt_, "_OP_={", "_O2_={", "//@@=} :", _pre3_=Tx, _BLOCK_=TestVar, _BLOCK2_=TestType )  // }

(* LOCATION: global var  NAMED pointer <each TYPE ALIAS> // NO PRE-ASSIGNED VALUE *)
  // gvptt_Byte: PTxByte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvptt_, "_OP_={", "_O2_={", "//@@=} :", _pre3_=PTx, _BLOCK_=TestVar, _BLOCK2_=TestType )  // }


(* LOCATION: global var  untyped pointer // NO PRE-ASSIGNED VALUE *)
  // gv_ptr_Byte: pointer;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_ptr_, "_OP_=:pointer;//", "_O2_=:pointer;//" )
  // gv_ptr2_Byte: pointer;  //gv_ptr2_Byte := @gvaByte[1]
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_ptr2_, "_OP_=:pointer;//", "_O2_=:pointer;//" )
  // gv_aptr_Byte: array [0..2] of pointer;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_aptr_, "_OP_=:array [0..2] of pointer;//", "_O2_=:array [0..2] of pointer;//" )

(* LOCATION: global var  untyped PPointerList // NO PRE-ASSIGNED VALUE *)
  // gv_ptrlist_Byte: pointer;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_ptrlist_, "_OP_=:PPointerList;//", "_O2_=:PPointerList;//" )


{$if FPC_FULLVERSION >= 30000}
constructor TSize.Create(asz :TSize);
begin end;
{$endif}
constructor TObjectCreate3Int64.Create;
begin end;
destructor TObjectCreate3Int64.Destroy;
begin end;
procedure TObjectCreate3Int64.Foo;
begin end;

function TMyBaseClass.SomeMeth1(SomeValue: Integer): Boolean;
begin result := SomeValue = 0; end;

procedure TMyBaseClass.BaseMethFoo;
begin
  BreakDummy:= 112; // TEST_BREAKPOINT=BaseMethFoo
  BreakDummy2 := ClassBaseVar1;
end;

procedure TMyClass.MethFoo;
begin
  BreakDummy:= 113; // TEST_BREAKPOINT=MethFoo
  BreakDummy2 := ClassVar1;
end;

{$IFDEF SINGLE_BIG_FUNC}
  procedure Foo(
  (* LOCATION: param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=arg, _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    ArgMyClass1: TMyClass;      ArgMyClass2: TMyBaseClass;
    ArgMyTestRec1: TMyTestRec;  Dummy: Integer
  );
  var
  (* LOCATION: local var *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

  (* LOCATION: local var  pointer <each type>  FOR locals *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pl_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  (* LOCATION: local var  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pa_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )

  //TODO MyClass
  begin  // TEST_BREAKPOINT=FooBegin
    BreakDummy:= 1;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pl_, _OP_={, _O2_={, _pre3_=@fooloc, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pa_, _OP_={, _O2_={, _pre3_=@arg, "//@@=} :=", _BLOCK_=TestArg, _BLOCK2_=TestPointer) //}

    BreakDummy:= 1; // TEST_BREAKPOINT=Foo
  end;


  procedure FooVar(
  (* LOCATION: var param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=var argvar", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    ArgVarMyClass1: TMyClass;      ArgVarMyClass2: TMyBaseClass;
    ArgVarMyTestRec1: TMyTestRec;  Dummy: Integer
  );
  var
  (* LOCATION: var params  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pv_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  begin // TEST_BREAKPOINT=FooVarBegin
  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pv_, _OP_={, _O2_={, _pre3_=@argvar, "//@@=} :=", _BLOCK_=TestPointer, _BLOCK2_=TestArg) //}

    BreakDummy:= 1;
    BreakDummy:= 1; // TEST_BREAKPOINT=FooVar
  end;


  procedure FooConstRef(
  (* LOCATION: constref param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=constref argconstref", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    ArgConstRefMyClass1: TMyClass;      ArgConstRefMyClass2: TMyBaseClass;
    ArgConstRefMyTestRec1: TMyTestRec;  Dummy: Integer
  );
  var
    xxx, xx2: ansistring; // enforce a stackframe
  begin // TEST_BREAKPOINT=FooConstRefBegin
    BreakDummy:= 1;
    xxx := '1';
    BreakDummy:= 1; // TEST_BREAKPOINT=FooConstRef
  end;

{$ELSE} // SINGLE_BIG_FUNC
{$DEFINE PART1}
  procedure Foo1(
  (* LOCATION: param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=arg, _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    ArgMyClass1: TMyClass;      ArgMyClass2: TMyBaseClass;
    ArgMyTestRec1: TMyTestRec;  Dummy: Integer
  );
  var
  (* LOCATION: local var *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

  (* LOCATION: local var  pointer <each type>  FOR locals *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pl_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  (* LOCATION: local var  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pa_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )

  //TODO MyClass
  begin  // TEST_BREAKPOINT=FooBegin1
    BreakDummy:= 1;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pl_, _OP_={, _O2_={, _pre3_=@fooloc, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pa_, _OP_={, _O2_={, _pre3_=@arg, "//@@=} :=", _BLOCK_=TestArg, _BLOCK2_=TestPointer) //}

    BreakDummy:= 1; // TEST_BREAKPOINT=Foo1
  end;


  procedure FooVar1(
  (* LOCATION: var param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=var argvar", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    ArgVarMyClass1: TMyClass;      ArgVarMyClass2: TMyBaseClass;
    ArgVarMyTestRec1: TMyTestRec;  Dummy: Integer
  );
  var
  (* LOCATION: var params  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pv_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  begin // TEST_BREAKPOINT=FooVarBegin1
  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pv_, _OP_={, _O2_={, _pre3_=@argvar, "//@@=} :=", _BLOCK_=TestPointer, _BLOCK2_=TestArg) //}

    BreakDummy:= 1;
    BreakDummy:= 1; // TEST_BREAKPOINT=FooVar1
  end;


  procedure FooConstRef1(
  (* LOCATION: constref param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=constref argconstref", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    ArgConstRefMyClass1: TMyClass;      ArgConstRefMyClass2: TMyBaseClass;
    ArgConstRefMyTestRec1: TMyTestRec;  Dummy: Integer
  );
  var
    xxx, xx2: ansistring; // enforce a stackframe
  begin // TEST_BREAKPOINT=FooConstRefBegin1
    BreakDummy:= 1;
    xxx := '1';
    BreakDummy:= 1; // TEST_BREAKPOINT=FooConstRef1
  end;

{$UNDEF PART1}{$DEFINE PART2}
  procedure Foo2(
  (* LOCATION: param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=arg, _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    Dummy: Integer
  );
  var
  (* LOCATION: local var *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

  (* LOCATION: local var  pointer <each type>  FOR locals *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pl_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  (* LOCATION: local var  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pa_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )

  //TODO MyClass
  begin  // TEST_BREAKPOINT=FooBegin2
    BreakDummy:= 1;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pl_, _OP_={, _O2_={, _pre3_=@fooloc, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pa_, _OP_={, _O2_={, _pre3_=@arg, "//@@=} :=", _BLOCK_=TestArg, _BLOCK2_=TestPointer) //}

    BreakDummy:= 1; // TEST_BREAKPOINT=Foo2
  end;


  procedure FooVar2(
  (* LOCATION: var param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=var argvar", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    Dummy: Integer
  );
  var
  (* LOCATION: var params  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pv_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  begin // TEST_BREAKPOINT=FooVarBegin2
  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pv_, _OP_={, _O2_={, _pre3_=@argvar, "//@@=} :=", _BLOCK_=TestPointer, _BLOCK2_=TestArg) //}

    BreakDummy:= 1;
    BreakDummy:= 1; // TEST_BREAKPOINT=FooVar2
  end;


  procedure FooConstRef2(
  (* LOCATION: constref param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=constref argconstref", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    Dummy: Integer
  );
  var
    xxx, xx2: ansistring; // enforce a stackframe
  begin // TEST_BREAKPOINT=FooConstRefBegin2
    BreakDummy:= 1;
    xxx := '1';
    BreakDummy:= 1; // TEST_BREAKPOINT=FooConstRef2
  end;

{$UNDEF PART2}{$DEFINE PART3}
  procedure Foo3(
  (* LOCATION: param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=arg, _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    Dummy: Integer
  );
  var
  (* LOCATION: local var *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

  (* LOCATION: local var  pointer <each type>  FOR locals *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pl_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  (* LOCATION: local var  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pa_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )

  //TODO MyClass
  begin  // TEST_BREAKPOINT=FooBegin3
    BreakDummy:= 1;
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pl_, _OP_={, _O2_={, _pre3_=@fooloc, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pa_, _OP_={, _O2_={, _pre3_=@arg, "//@@=} :=", _BLOCK_=TestArg, _BLOCK2_=TestPointer) //}

    BreakDummy:= 1; // TEST_BREAKPOINT=Foo3
  end;


  procedure FooVar3(
  (* LOCATION: var param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=var argvar", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    Dummy: Integer
  );
  var
  (* LOCATION: var params  pointer <each type>  FOR args *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pv_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
  begin // TEST_BREAKPOINT=FooVarBegin3
  (* INIT: local var  pointer <each type> *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pv_, _OP_={, _O2_={, _pre3_=@argvar, "//@@=} :=", _BLOCK_=TestPointer, _BLOCK2_=TestArg) //}

    BreakDummy:= 1;
    BreakDummy:= 1; // TEST_BREAKPOINT=FooVar3
  end;


  procedure FooConstRef3(
  (* LOCATION: constref param *)
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=constref argconstref", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
    Dummy: Integer
  );
  var
    xxx, xx2: ansistring; // enforce a stackframe
  begin // TEST_BREAKPOINT=FooConstRefBegin3
    BreakDummy:= 1;
    xxx := '1';
    BreakDummy:= 1; // TEST_BREAKPOINT=FooConstRef3
  end;

{$UNDEF PART3}
{$ENDIF} // SINGLE_BIG_FUNC

function TMyClass.SomeFuncIntRes(): Integer;
begin
  result := FFunctInt + FFunctIntConst;
end;
function TMyClass.SomeFuncIntResAdd(a: integer): Integer;
begin
  result := 77 + a;
  FFunctInt := result;
end;

procedure TMyBaseOldObject.BaseObjMethFoo();
begin
  BreakDummy:= 1; // TEST_BREAKPOINT=BaseObjMethFoo
end;
procedure TMyOldObject.ObjMethFoo();
begin
  BreakDummy:= 1; // TEST_BREAKPOINT=ObjMethFoo
end;

var
  ModifyTestByte: record
    pre: qword; // padding, must not be changed
    val: byte;
    post: byte; // padding, must not be changed
  end;
  ModifyTestWord: record
    pre: qword; // padding, must not be changed
    val: word;
    post: byte; // padding, must not be changed
  end;
  ModifyTestCardinal: record
    pre: qword; // padding, must not be changed
    val: Cardinal;
    post: byte; // padding, must not be changed
  end;
  ModifyTestqword: record
    pre: qword; // padding, must not be changed
    val: qword;
    post: byte; // padding, must not be changed
  end;
  ModifyTestint: record
    pre: qword; // padding, must not be changed
    val: integer;
    post: byte; // padding, must not be changed
  end;
  ModifyTestInt64: record
    pre: qword; // padding, must not be changed
    val: int64;
    post: byte; // padding, must not be changed
  end;
  ModifyTestPointer: record
    pre: qword; // padding, must not be changed
    val: Pointer;
    post: byte; // padding, must not be changed
  end;
  ModifyTestPWord: record
    pre: qword; // padding, must not be changed
    val: PWord;
    post: byte; // padding, must not be changed
  end;
  ModifyTestBool: record
    pre: qword; // padding, must not be changed
    val: Boolean;
    post: byte; // padding, must not be changed
  end;
  ModifyTestByteBool: record
    pre: qword; // padding, must not be changed
    val: ByteBool;
    post: byte; // padding, must not be changed
  end;
  ModifyTestChar: record
    pre: qword; // padding, must not be changed
    val: Char;
    post: byte; // padding, must not be changed
  end;
  ModifyTestWideChar: record
    pre: qword; // padding, must not be changed
    val: WideChar;
    post: byte; // padding, must not be changed
  end;
  ModifyTestEnum: record
    pre: qword; // padding, must not be changed
    val: TEnum;
    post: byte; // padding, must not be changed
  end;
  ModifyTestEnum16: record
    pre: qword; // padding, must not be changed
    val: TEnum16;
    post: byte; // padding, must not be changed
  end;
  ModifyTestSet: record
    pre: qword; // padding, must not be changed
    val: TSet;
    post: byte; // padding, must not be changed
  end;
  ModifyTestSet4: record
    pre: qword; // padding, must not be changed
    val: TSet4;
    post: byte; // padding, must not be changed
  end;
  ModifyTestSet6: record
    pre: qword; // padding, must not be changed
    val: TSet6;
    post: byte; // padding, must not be changed
  end;
  ModifyTestSet7: record
    pre: qword; // padding, must not be changed
    val: TSet7;
    post: byte; // padding, must not be changed
  end;
  ModifyTestSet8: record
    pre: qword; // padding, must not be changed
    val: TSet8;
    post: byte; // padding, must not be changed
  end;
  ModifyTestSRangeSet: record
    pre: qword; // padding, must not be changed
    val: TSmallRangeSet;
    post: byte; // padding, must not be changed
  end;

  // 7 bytes aftr a qword align
  ModifyPackTestByte: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: byte;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestWord: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: word;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestCardinal: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: Cardinal;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestqword: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: qword;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestint: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: integer;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestInt64: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: int64;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestPointer: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: Pointer;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestPWord: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: PWord;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestBool: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: Boolean;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestByteBool: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: ByteBool;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestChar: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: Char;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestWideChar: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: WideChar;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestEnum: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TEnum;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestEnum16: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TEnum16;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestSet: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TSet;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestSet4: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TSet4;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestSet6: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TSet6;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestSet7: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TSet7;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestSet8: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TSet8;
    post: byte; // padding, must not be changed
  end;
  ModifyPackTestSRangeSet: packed record
    q1: QWord;
    p1, p2, p3, p4, p5, p6, pre: byte; // padding, must not be changed
    val: TSmallRangeSet;
    post: byte; // padding, must not be changed
  end;

// Intrinsic ..
type
  TRecPP = record
    p1, p2: pchar;
  end;
  PRecPP = ^TRecPP;
  TRecArray = array of TRecPP;
  PTRecArray = ^TRecArray;
  PRecArray = array of PRecPP;
var
  dotdotArray1a, dotdotArray1b: TRecArray;
  dotdotArray2a, dotdotArray2b: array of array of TRecPP;
  dotdotArrayP1a, dotdotArrayP1b: PRecArray;
  dotdotArrayP2a, dotdotArrayP2b: array of array of PRecPP;

  dotdotArrayPPa, dotdotArrayPPb: PTRecArray;
  i1,i2: integer;

begin
  U8Data1 := #$2267; //#$E2#$89#$A7;
  U8Data2 := #$2267'X';
  // access constant that are not passed as function arg
  // so every constant is accessed, and they can not be optimized away
  InterfacedObject:= TInterfacedObject.create;
  InterfacedObject2:= TInterfacedObject.create;
  BreakDummy := ord(gcCharStatArray[1]);
  BreakDummy := ord(gcWCharStatArray[1]);
  p := nil;
  PByteDummy := nil;
  pw := nil;
  SomeFunc1(1,1,1,1);
  SomeProc1();
  SomeFuncInt;
  SomeFuncIntRes;
  FuncIntAdd(1,1);
  FuncTooManyArg(1,1,1,1,1,1,1,1,1,1,1,1);
  variant1 := 102;
  variant2 := True;
  v_rec.variant1 := 103;
  v_rec.variant2 := False;
  v_array[3] := 104;
  v_array[4] := True;

  VarCastRecb1.b  := 1;
  VarCastRecb2.b  := 2;
  VarCastRecb2.b2 := 2;
  VarCastRecw1.w  := 4;
  VarCastRecw2.w  := 5;
  VarCastRecw2.w2 := 6;
  VarCastRecl1.l  := 7;
  VarCastRecl2.l  := 8;
  VarCastRecl2.l2 := 9;
  VarCastRecl4.l  := 10;
  VarCastRecl4.l2 := 11;
  VarCastRecl4.l3 := 12;
  VarCastRecl4.l4 := 13;
  VarCastRecq2.q  := $0010001100120013;
  VarCastRecq2.q2 := $0020002100220023;

  {$if FPC_FULLVERSION >= 30000}
  dummy1 := nil;
  {$ENDIF}

  with ModifyTestByte      do begin pre := qword($9696969696969696); post := $69; val := $01; end;
  with ModifyTestWord      do begin pre := qword($9696969696969696); post := $69; val := $0101; end;
  with ModifyTestCardinal  do begin pre := qword($9696969696969696); post := $69; val := $81020102; end;
  with ModifyTestQword     do begin pre := qword($9696969696969696); post := $69; val := qword($8102010201020102); end;
  with ModifyTestInt       do begin pre := qword($9696969696969696); post := $69; val := -$01030103; end;
  with ModifyTestInt64     do begin pre := qword($9696969696969696); post := $69; val := -$0103010301030103; end;
  with ModifyTestPointer   do begin pre := qword($9696969696969696); post := $69; val := pointer(30); end;
  with ModifyTestPWord     do begin pre := qword($9696969696969696); post := $69; val := pointer(40); end;
  with ModifyTestBool      do begin pre := qword($9696969696969696); post := $69; val := True; end;
  with ModifyTestByteBool  do begin pre := qword($9696969696969696); post := $69; val := False; end;
  with ModifyTestChar      do begin pre := qword($9696969696969696); post := $69; val := 'B'; end;
  with ModifyTestWideChar  do begin pre := qword($9696969696969696); post := $69; val := 'B'; end;
  with ModifyTestEnum      do begin pre := qword($9696969696969696); post := $69; val := EnVal2; end;
  with ModifyTestEnum16    do begin pre := qword($9696969696969696); post := $69; val := ExValX2; end;
  with ModifyTestSet       do begin pre := qword($9696969696969696); post := $69; val := [EnVal2, EnVal4]; end;
  with ModifyTestSet4      do begin pre := qword($9696969696969696); post := $69; val := [E4Val02, E4Val09]; end;
  with ModifyTestSet6      do begin pre := qword($9696969696969696); post := $69; val := [E6Val02, E6Val1A]; end;
  with ModifyTestSet7      do begin pre := qword($9696969696969696); post := $69; val := [E7Val02, E7Val3A]; end;
  with ModifyTestSet8      do begin pre := qword($9696969696969696); post := $69; val := [E8Val02, E8Val59]; end;
  with ModifyTestSRangeSet do begin pre := qword($9696969696969696); post := $69; val := [20,23,28]; end;

  with ModifyPackTestByte      do begin pre := $96; post := $69; val := $01; end;
  with ModifyPackTestWord      do begin pre := $96; post := $69; val := $0101; end;
  with ModifyPackTestCardinal  do begin pre := $96; post := $69; val := $81020102; end;
  with ModifyPackTestQword     do begin pre := $96; post := $69; val := qword($8102010201020102); end;
  with ModifyPackTestInt       do begin pre := $96; post := $69; val := -$01030103; end;
  with ModifyPackTestInt64     do begin pre := $96; post := $69; val := -$0103010301030103; end;
  with ModifyPackTestPointer   do begin pre := $96; post := $69; val := pointer(30); end;
  with ModifyPackTestPWord     do begin pre := $96; post := $69; val := pointer(40); end;
  with ModifyPackTestBool      do begin pre := $96; post := $69; val := True; end;
  with ModifyPackTestByteBool  do begin pre := $96; post := $69; val := False; end;
  with ModifyPackTestChar      do begin pre := $96; post := $69; val := 'B'; end;
  with ModifyPackTestWideChar  do begin pre := $96; post := $69; val := 'B'; end;
  with ModifyPackTestEnum      do begin pre := $96; post := $69; val := EnVal2; end;
  with ModifyPackTestEnum16    do begin pre := $96; post := $69; val := ExValX2; end;
  with ModifyPackTestSet       do begin pre := $96; post := $69; val := [EnVal2, EnVal4]; end;
  with ModifyPackTestSet4      do begin pre := $96; post := $69; val := [E4Val02, E4Val09]; end;
  with ModifyPackTestSet6      do begin pre := $96; post := $69; val := [E6Val02, E6Val1A]; end;
  with ModifyPackTestSet7      do begin pre := $96; post := $69; val := [E7Val02, E7Val3A]; end;
  with ModifyPackTestSet8      do begin pre := $96; post := $69; val := [E8Val02, E8Val59]; end;
  with ModifyPackTestSRangeSet do begin pre := $96; post := $69; val := [20,23,28]; end;

(* use global const / value in "gv" will be overriden... *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv, {e}={, "//@@=} :=", _pre3_=gc, _BLOCK_=TestAssignGC)

(* INIT: global var *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv,   ADD=1, CHR1='B', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv2_, ADD=3, CHR1='D', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

(* INIT: global var  TYPE alias // NO PRE-ASSIGNED VALUE *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gvtt_, ADD=7, CHR1='N', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign, _BLOCK2_=TestType)

(* INIT: global var  NAMED pointer <each type> // NO PRE-ASSIGNED VALUE *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvpt_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gv, _BLOCK_=TestVar, _BLOCK2_=TestPointer ) // }

(* INIT: global var  NAMED pointer <each TYPE ALIAS> // NO PRE-ASSIGNED VALUE *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvptt_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gvtt_, _BLOCK_=TestVar, _BLOCK2_=TestType )  // }


(* INIT: global var  untyped NAMED pointer // NO PRE-ASSIGNED VALUE *)
  // gv_ptr_Byte := @gvByte;            // ADD=1, CHR1='B'
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_ptr_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gv ) // }
  // gv_aptr_Byte[0] := @gvByte;        // ADD=1, CHR1='B'
  // gv_aptr_Byte[1] := @gv2_Byte;     // ADD=3, CHR1='D'
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_aptr_, "{e}=[0]", "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gv ) // }
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_aptr_, "{e}=[1]", "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gv2_ ) // }

(* INIT: global var  untyped PPointerList // NO PRE-ASSIGNED VALUE *)
  // gv_ptrlist_Byte := @gv_aptr_Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_ptrlist_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gv_aptr_ ) // }

(* INIT: class var *)
  // cl_v_Byte: Byte = (1 + add);
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=TMyClass.cl_v_,   ADD=1, CHR1='v', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)


(* INIT: field in class / baseclass *)
  MyClass1 := TMyClass.Create;
  MyClass1.FFunctIntConst := 999;
  MyClass1.SomeMeth1(1);
  MyClass1.SomeFuncIntRes();
  MyClass1.SomeFuncIntResAdd(1);
  MyPClass1 := @MyClass1;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyClass1.mbc, ADD=3, CHR1='D', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyClass1.mc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

  MyClassBadMem := TMyClass(Pointer(1));

  MyNilClass1 := nil;

(* INIT: field in class / baseclass // typecast *)
  MyClass2 := TMyClass.Create;
  MyPClass2 := @MyClass2;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,"pre__=TMyClass(MyClass2).mbc", ADD=5, CHR1='F', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,"pre__=TMyClass(MyClass2).mc", ADD=4, CHR1='E', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)



  MyPOldObjectBase := @MyOldObjectBase;
  MyPOldObject     := @MyOldObject;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyOldObjectBase.obc, ADD=3, CHR1='D', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyOldObject.obc, ADD=4, CHR1='E', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyOldObject.oc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)


(* INIT: record var *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyTestRec1.rc_f_, ADD=2, CHR1='r', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  MyPTestRec1 := @MyTestRec1;

  MyStringList := TMyStringList.Create;
  MyStringList.Flist := @MyStringItemList;
  MyClass1.FMyStringList := TMyStringList.Create;
  MyClass1.FMyStringList.Flist := @MyStringItemList;
  TMyClass(MyClass2).FMyStringList := TMyStringList.Create;
  TMyClass(MyClass2).FMyStringList.Flist := @MyStringItemList;
  MyStringItemList[0].FString := 'ABC1';
  MyStringItemList[1].FString := 'DEF2';
  MyStringItemList[2].FString := 'XYZ3';


(* INIT: global var  ARRAY OF <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,"pre__=SetLength(gva", "_OP_=,4);//", "_O2_=,4);//", _BLOCK_=TestSetLen)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gva, ADD=5, CHR1='K', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[0]", _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gva, ADD=6, CHR1='L', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[1]", _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gva, ADD=8, CHR1='N', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[2]", _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gva, ADD=9, CHR1='O', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[3]", _BLOCK_=TestAssign)
  // gvp_a_Byte := @gvaByte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_a_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gva ) // }

  // gv_ptr2_Byte := @gvaByte[1];
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv_ptr2_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gva ) //, {e3}=[1] }


(* INIT: global var  ARRAY [0..2] OF <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv_sa_, ADD=7, CHR1='O', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[0]", _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv_sa_, ADD=8, CHR1='P', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[1]", _BLOCK_=TestAssign)
  // gvp_sa_Byte := @gv_sa_Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_sa_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gv_sa_ ) // }

(* INIT: global var  ARRAY [-1..2] OF <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv_nsa_, ADD=9,  CHR1='Q', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[-1]", _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv_nsa_, ADD=10, CHR1='R', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[0]", _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv_nsa_, ADD=11, CHR1='S', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[1]", _BLOCK_=TestAssign)
  // gvp_nsa_Byte := @gv_nsa_Byte;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_nsa_, "_OP_= {", "_O2_={ ", "//@@=} :=", _pre3_=@gv_nsa_ ) // }

(* INIT: global var  pointer <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gvp_, _OP_={, _O2_={, _pre3_=@gv, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gvp2_, _OP_={, _O2_={, _pre3_=@gva, "//@@=} :=", {e3}=[1], _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}

  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gvpX_, _OP_={, _O2_={, "_pre3_=Pointer(1); //", "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}

  RecursePtrA1  := @RecursePtrA2;
  RecursePtrA2  := @RecursePtrA1;

  RecursePtrB1  := @RecursePtrB2;
  RecursePtrB2  := @RecursePtrB3;
  RecursePtrB3  := @RecursePtrB4;
  RecursePtrB4  := @RecursePtrB1;

  RecursePtrC1  := @RecursePtrC2;
  RecursePtrC2  := @RecursePtrC3;
  RecursePtrC3  := @RecursePtrC4;
  RecursePtrC4  := @RecursePtrC5;
  RecursePtrC5  := @RecursePtrC6;
  RecursePtrC6  := @RecursePtrC7;
  RecursePtrC7  := @RecursePtrC8;
  RecursePtrC8  := @RecursePtrC9;
  RecursePtrC9  := @RecursePtrC10;
  RecursePtrC10 := @RecursePtrC11;
  RecursePtrC11 := @RecursePtrC12;
  RecursePtrC12 := @RecursePtrC13;
  RecursePtrC13 := @RecursePtrC14;
  RecursePtrC14 := @RecursePtrC15;
  RecursePtrC15 := @RecursePtrC16;
  RecursePtrC16 := @RecursePtrC17;
  RecursePtrC17 := @RecursePtrC18;
  RecursePtrC18 := @RecursePtrC1;

  SRef0 := '';
  SRef1 := 'abcdef123456';
  SRef2 := inttostr(random(9))+SRef1;
  SRef3 := inttostr(random(9))+SRef1;
  SRef4 := SRef3;

  PCRef1 := @SRef1[1];
  PtrRef1 := PCRef1;

  Short0 := 'abcdef1234';
  Short1[0] := 'abcdef1234';
  Short1[1] := 'ABCDEF7890';
  Short1[2] := 'mnopqrstuv';

  ARef0 := nil;
  SetLength(ARef1, 10);
  SetLength(ARef2, 10);
  SetLength(ARef3, 10);
  ARef4 := ARef3;

  // .. intrinsic
  SConst := 'ABCDE';
  SetLength(dotdotArray1a, 5);
  SetLength(dotdotArray1b, 5);
  SetLength(dotdotArray2a, 6, 5);
  SetLength(dotdotArray2b, 6, 5);

  dotdotArray1a[0].p1 := @SRef1[1];
  pointer(dotdotArray1a[1].p1) := pointer(1);
  dotdotArray1a[2].p1 := @SConst[1];
  dotdotArray1a[3].p1 := nil;
  dotdotArray1a[4].p1 := @SRef1[2];

  pointer(dotdotArray1b[0].p1) := pointer(1);
  dotdotArray1b[1].p1 := @SRef1[1];
  dotdotArray1b[2].p1 := nil;
  dotdotArray1b[3].p1 := nil;
  dotdotArray1b[4].p1 := @SRef1[3];

  dotdotArray2a[0] := nil;
  dotdotArray2a[1][0].p1 := @SRef1[1];
  pointer(dotdotArray2a[1][1].p1) := pointer(1);
  dotdotArray2a[1][2].p1 := @SConst[1];
  dotdotArray2a[1][3].p1 := nil;
  dotdotArray2a[1][4].p1 := @SRef1[2];
  pointer(dotdotArray2a[2][0].p1) := pointer(1);
  dotdotArray2a[2][1].p1 := pointer(-1);
  dotdotArray2a[2][2].p1 := @SConst[1];
  dotdotArray2a[2][3].p1 := nil;
  dotdotArray2a[2][4].p1 := @SRef1[2];
  dotdotArray2a[3] := nil;
  dotdotArray2a[4][0].p1 := @SRef1[1];
  pointer(dotdotArray2a[4][1].p1) := pointer(1);
  dotdotArray2a[4][2].p1 := @SConst[1];
  dotdotArray2a[4][3].p1 := nil;
  dotdotArray2a[4][4].p1 := @SRef1[2];

  SetLength(dotdotArrayP1a, 5);
  SetLength(dotdotArrayP1b, 5);
  SetLength(dotdotArrayP2a, 6, 5);
  SetLength(dotdotArrayP2b, 6, 5);
  for i1 := 0 to 4 do begin
    dotdotArrayP1a[i1] := @dotdotArray1a[i1];
    dotdotArrayP1b[i1] := @dotdotArray1b[i1];
  end;
  for i1 := 0 to 5 do
  for i2 := 0 to 4 do begin
    if dotdotArray2a[i1] = nil then
      dotdotArrayP2a[i1] := nil
    else
      dotdotArrayP2a[i1][i2] := @dotdotArray2a[i1][i2];
    if dotdotArray2b[i1] = nil then
      dotdotArrayP2b[i1] := nil
    else
      dotdotArrayP2b[i1][i2] := @dotdotArray2b[i1][i2];
  end;

  dotdotArrayPPa := @dotdotArray1a;
  dotdotArrayPPb := @dotdotArray1b;



  BreakDummy:= 1; // TEST_BREAKPOINT=Prg

{$IFDEF SINGLE_BIG_FUNC}
  Foo(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    MyTestRec1,
    0
  );

  FooVar(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    MyTestRec1,
    0
  );

  FooConstRef(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    MyTestRec1,
    0
  );
{$ELSE} // SINGLE_BIG_FUNC
{$DEFINE PART1}
  Foo1(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    MyTestRec1,
    0
  );

  FooVar1(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    MyTestRec1,
    0
  );

  FooConstRef1(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    MyTestRec1,
    0
  );
{$UNDEF PART1}{$DEFINE PART2}
  Foo2(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    0
  );

  FooVar2(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    0
  );

  FooConstRef2(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    0
  );
{$UNDEF PART2}{$DEFINE PART3}
  Foo3(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    0
  );

  FooVar3(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    0
  );

  FooConstRef3(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    0
  );
{$UNDEF PART3}
{$ENDIF} // SINGLE_BIG_FUNC



  TMyClass.ClassBaseVar1 := 118;
  TMyClass.ClassVar1 := 119;

  MyClass1.BaseMethFoo();
  MyClass1.MethFoo();

  MyClass2.BaseMethFoo();
  variant1 := 102;
  variant2 := True;

  MyOldObjectBase.BaseObjMethFoo();
  MyOldObject.ObjMethFoo();


end.

  f�  P   ��
 W A T C H E S V A L U E P R G I D E N T . I N C         0	        {$DEFINE _BLOCK_}
{$DEFINE _BLOCK2_}

{$IFnDef Part2}{$IFnDef Part3} // Part 1
      // ******************************
      // **********  PART 1  **********
      // ******************************

// ADD max 15

  pre__Byte{e}          _OP_ Byte        (1                    + ADD);          //@@ _pre3_Byte{e3};
  pre__Word{e}          _OP_ Word        (100                  + ADD);          //@@ _pre3_Word{e3};
  pre__Longword{e}      _OP_ Longword    (1000                 + ADD);          //@@ _pre3_Longword{e3};
  pre__QWord{e}         _OP_ QWord       (10000                + ADD);          //@@ _pre3_QWord{e3};
  pre__Shortint{e}      _OP_ Shortint    (50                   + ADD);          //@@ _pre3_Shortint{e3};
  pre__Smallint{e}      _OP_ Smallint    (500                  + ADD);          //@@ _pre3_Smallint{e3};
  pre__Longint{e}       _OP_ Longint     (5000                 + ADD);          //@@ _pre3_Longint{e3};
  pre__Int64{e}         _OP_ Int64       (50000                + ADD);          //@@ _pre3_Int64{e3};
  pre__IntRange{e}      _OP_ IntRange   (-50                  + ADD);          //@@ _pre3_IntRange{e3};
  pre__CardinalRange{e} _OP_ CardinalRange(50                  + ADD);         //@@ _pre3_CardinalRange{e3};

  pre__Byte_2{e}        _OP_ Byte        (240                  + ADD);          //@@ _pre3_Byte_2{e3};
  pre__Word_2{e}        _OP_ Word        (65501                + ADD);          //@@ _pre3_Word_2{e3};
  pre__Longword_2{e}    _OP_ Longword    (4123456789           + ADD);          //@@ _pre3_Longword_2{e3};
  pre__QWord_2{e}       _OP_ QWord       (15446744073709551610 + ADD);          //@@ _pre3_QWord_2{e3};
  pre__Shortint_2{e}    _OP_ Shortint    (112                  + ADD);          //@@ _pre3_Shortint_2{e3};
  pre__Smallint_2{e}    _OP_ Smallint    (32012                + ADD);          //@@ _pre3_Smallint_2{e3};
  pre__Longint_2{e}     _OP_ Longint     (20123456             + ADD);          //@@ _pre3_Longint_2{e3};
  pre__Int64_2{e}       _OP_ Int64       (9123372036854775801  + ADD);          //@@ _pre3_Int64_2{e3};

  pre__Shortint_3{e}    _OP_ Shortint    (-112                 + ADD);          //@@ _pre3_Shortint_3{e3};
  pre__Smallint_3{e}    _OP_ Smallint    (-32012               + ADD);          //@@ _pre3_Smallint_3{e3};
  pre__Longint_3{e}     _OP_ Longint     (-20123456            + ADD);          //@@ _pre3_Longint_3{e3};
  pre__Int64_3{e}       _OP_ Int64       (-9123372036854775801 + ADD);          //@@ _pre3_Int64_3{e3};

  pre__Bool1{e}         _OP_ Boolean     (False);          //@@ _pre3_Bool1{e3};
  pre__Bool2{e}         _OP_ Boolean     (True);           //@@ _pre3_Bool2{e3};
  pre__WBool1{e}        _OP_ Boolean16   (False);          //@@ _pre3_WBool1{e3};
  pre__WBool2{e}        _OP_ Boolean16   (True);           //@@ _pre3_WBool2{e3};
  pre__LBool1{e}        _OP_ Boolean32   (False);          //@@ _pre3_LBool1{e3};
  pre__LBool2{e}        _OP_ Boolean32   (True);           //@@ _pre3_LBool2{e3};
  pre__QBool1{e}        _OP_ Boolean64   (False);          //@@ _pre3_QBool1{e3};
  pre__QBool2{e}        _OP_ Boolean64   (True);           //@@ _pre3_QBool2{e3};
  pre__ByteBool1{e}     _OP_ ByteBool    (False);          //@@ _pre3_ByteBool1{e3};
  pre__ByteBool2{e}     _OP_ ByteBool    (True);           //@@ _pre3_ByteBool2{e3};
  pre__WordBool1{e}     _OP_ WordBool    (False);          //@@ _pre3_WordBool1{e3};
  pre__WordBool2{e}     _OP_ WordBool    (True);           //@@ _pre3_WordBool2{e3};
  pre__LongBool1{e}     _OP_ LongBool    (False);          //@@ _pre3_LongBool1{e3};
  pre__LongBool2{e}     _OP_ LongBool    (True);           //@@ _pre3_LongBool2{e3};
  pre__QWordBool1{e}    _OP_ QWordBool   (False);          //@@ _pre3_QWordBool1{e3};
  pre__QWordBool2{e}    _OP_ QWordBool   (True);           //@@ _pre3_QWordBool2{e3};

  pre__Real{e}          _OP_ Real        (50.25                + ADD);          //@@ _pre3_Real{e3};
  pre__Single{e}        _OP_ Single      (100.125              + ADD);          //@@ _pre3_Single{e3};
  pre__Double{e}        _OP_ Double      (1000.125             + ADD);          //@@ _pre3_Double{e3};
  pre__Extended{e}      _OP_ Extended    (10000.175            + ADD);          //@@ _pre3_Extended{e3};
  //pre__Comp{e}        _OP_ Comp        (50.125               + ADD);          //@@ //_pre3_Comp{e3};
  {$IFDEF TestAssign}
  {$IFDEF TestType}
//  pre__Currency{e}      _OP_ TxCurrency    (125.123              + ADD);          //@@ _pre3_Currency{e3};
  {$ELSE}
  pre__Currency{e}      _OP_ Currency    (125.123              + ADD);          //@@ _pre3_Currency{e3};
  {$ENDIF}
  {$ELSE}
  pre__Currency{e}      _OP_ Currency    (125.123             + ADD);          //@@ _pre3_Currency{e3};
  {$ENDIF}

  pre__Real_2{e}        _OP_ Real        (-50.25               + ADD);          //@@ _pre3_Real_2{e3};
  pre__Single_2{e}      _OP_ Single      (-100.125             + ADD);          //@@ _pre3_Single_2{e3};
  pre__Double_2{e}      _OP_ Double      (-1000.125            + ADD);          //@@ _pre3_Double_2{e3};
  pre__Extended_2{e}    _OP_ Extended    (-10000.175           + ADD);          //@@ _pre3_Extended_2{e3};
  //pre__Comp_2{e}      _OP_ Comp        (-150.125             + ADD);          //@@ //_pre3_Comp_2{e3};
  {$IFDEF TestAssign}
  {$IFDEF TestType}
//  pre__Currency_2{e}    _OP_ TxCurrency_2    (-125.123             + ADD);          //@@ _pre3_Currency_2{e3};
  {$ELSE}
  pre__Currency_2{e}    _OP_ Currency    (-125.123             + ADD);          //@@ _pre3_Currency_2{e3};
  {$ENDIF}
  {$ELSE}
  pre__Currency_2{e}    _OP_ Currency    (-125.123             + ADD);          //@@ _pre3_Currency_2{e3};
  {$ENDIF}

  pre__Ptr1{e}          _OP_ Pointer     (0                         );          //@@ _pre3_Ptr1{e3};
  pre__Ptr2{e}          _OP_ Pointer     (1000                 + ADD);          //@@ _pre3_Ptr2{e3};

  // **** Char and String types ****

  pre__Char{e}          _OP_ char        (CHR1                    );            //@@ _pre3_Char{e3};
  pre__Char2{e}         _OP_ char        (#0                      );            //@@ _pre3_Char2{e3};
  pre__Char3{e}         _OP_ char        (' '                     );            //@@ _pre3_Char3{e3};

  pre__String1{e}       _OP_ ShortStr1   (CHR1+''                 );            //@@ _pre3_String1{e3};
  pre__String1e{e}      _OP_ ShortStr1   (     ''                 );            //@@ _pre3_String1e{e3};
  pre__String10{e}      _OP_ ShortStr10  (CHR1+'bc1'              );            //@@ _pre3_String10{e3};
  pre__String10e{e}     _OP_ ShortStr10  (     ''                 );            //@@ _pre3_String10e{e3};
  pre__String10x{e}     _OP_ ShortStr10  (CHR1+'S'#0'B'#9'b'#10#13);            //@@ _pre3_String10x{e3};
  pre__String255{e}     _OP_ ShortStr255 (CHR1+'bcd0123456789'    );            //@@ _pre3_String255{e3};
  pre__Ansi1{e}         _OP_ AnsiString  (succ(CHR1)              );            //@@ _pre3_Ansi1{e3};
  //pre__Ansi2{e}         _OP_ AnsiString  (CHR1+'abcd0123'         );            //@@ _pre3_Ansi2{e3};
  pre__Ansi2{e}         _OP_ AnsiString  (CHR1#$61'bcd0123'         );            //@@ _pre3_Ansi2{e3};
  pre__Ansi3{e}         _OP_ AnsiString  (     ''                 );            //@@ _pre3_Ansi3{e3};
  //pre__Ansi4{e}         _OP_ TStrA       (CHR1+'A'#0'B'#9'b'#10#13);            //@@ _pre3_Ansi4{e3};
  pre__Ansi4{e}         _OP_ TStrA       (CHR1#$41#0'B'#9'b'#10#13);            //@@ _pre3_Ansi4{e3};
  //pre__Ansi5{e}         _OP_ AnsiString  (CHR1+'bcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');          //@@ _pre3_Ansi5{e3};
  pre__Ansi5{e}         _OP_ AnsiString  (CHR1#$62'cdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');          //@@ _pre3_Ansi5{e3};

  pre__Ansi5_Int{e} _O2_ PtrUInt _EQ_ (0);    //@@ _pre3_Ansi5_Int{e3};
  {$IFDEF TestAssign}
  pre__Ansi5_Int{e} _OP_ PtrUInt(pre__Ansi5{e});    //@@ //}}
  {$ENDIF}

  pre__PChar{e}         _OP_ PChar       ( nil );                               //@@ _pre3_PChar{e3};
  pre__PChar2{e}        _OP_ TPChr       ( nil );                               //@@ _pre3_PChar2{e3};
  pre__PChar3{e}        _OP_ TPChr       ( nil );                               //@@ _pre3_PChar2{e3};
  {$IFDEF TestAssign}
  pre__PChar2{e} := @pre__Ansi2{e}[1];                                          //@@ _pre3_PChar2{e3}; // }
  pre__PChar3{e} := @pre__Ansi2{e}[4];                                          //@@ _pre3_PChar2{e3}; // }
  {$ENDIF}

  pre__WideChar{e}      _OP_ char        (CHR1                    );            //@@ _pre3_WideChar{e3};
  pre__WideChar2{e}     _OP_ char        (#0                      );            //@@ _pre3_WideChar2{e3};
  pre__WideChar3{e}     _OP_ char        (' '                     );            //@@ _pre3_WideChar3{e3};

  pre__WideString1{e}   _OP_ WideString  (succ(CHR1)              );            //@@ _pre3_WideString1{e3};
  pre__WideString2{e}   _OP_ WideString  (CHR1+'abcX0123'         );            //@@ _pre3_WideString2{e3};
  pre__WideString3{e}   _OP_ WideString  (     ''                 );            //@@ _pre3_WideString3{e3};
  pre__WideString4{e}   _OP_ TWStrA      (CHR1+'A'#0'X'#9'b'#10#13);            //@@ _pre3_WideString4{e3};
  pre__WideString5{e}   _OP_ TWStrTA     (CHR1+'XcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');          //@@ _pre3_WideString5{e3};

  pre__PWideChar{e}     _OP_ PWideChar   ( nil );                               //@@ _pre3_PWideChar{e3};
  pre__PWideChar2{e}    _OP_ TPWChr      ( nil );                               //@@ _pre3_PWideChar2{e3};
  {$IFDEF TestAssign}
  pre__PWideChar2{e} := @pre__WideString2{e}[1];                                //@@ _pre3_PWideChar2{e3}; // }
  {$ENDIF}

  pre__UnicodeString1{e}   _OP_ UnicodeString  (succ(CHR1)              );      //@@ _pre3_UnicodeString1{e3};
  pre__UnicodeString2{e}   _OP_ UnicodeString  (CHR1+'aBcX0123'         );      //@@ _pre3_UnicodeString2{e3};
  pre__UnicodeString3{e}   _OP_ UnicodeString  (     ''                 );      //@@ _pre3_UnicodeString3{e3};
  pre__UnicodeString4{e}   _OP_ TUStrA      (CHR1+'B'#0'X'#9'b'#10#13);         //@@ _pre3_UnicodeString4{e3};
  pre__UnicodeString5{e}   _OP_ TUStrTA     (CHR1+'YcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghijAbcdefghij');          //@@ _pre3_UnicodeString5{e3};

  {$IFnDEF TestType}
    {$IFnDEF TestConst}
    pre__Variant_1{e}       _O2_ variant         _EQ_      (71237);       //@@ _pre3_Variant_1{e3};
    pre__Variant_2{e}       _O2_ variant         _EQ_      (True);        //@@ _pre3_Variant_2{e3};
    {$ELSE}
    // not supported
    pre__Variant_1{e}      =   (71237); // }}}}
    pre__Variant_2{e}      =   (True); // }}}}
    {$ENDIF}
  {$ENDIF}

// wide string char...

  // types that may get confused with strings

  {$IFnDEF TestAssign}
  pre__ShortRec{e}       _O2_ ShortRec               _EQ_      (length: 5; st:  (CHR1, 'b',CHR1, 'b','c'));       //@@ _pre3_ShortRec{e3};
  {$ELSE}
  {$IFDEF TestType} // incomplete values
  pre__ShortRec{e}       := pre__ShortRec;   pre__ShortRec{e}.st[1] := CHR1; pre__ShortRec{e}.st[3] := CHR1;      //@@ _pre3_ShortRec{e3}; // }}
  {$ELSE}
  pre__ShortRec{e}       := _pre2_ShortRec;   pre__ShortRec{e}.st[1] := CHR1; pre__ShortRec{e}.st[3] := CHR1;      //@@ _pre3_ShortRec{e3}; // }}
  {$ENDIF}
  {$ENDIF}


{$ENDIF}{$ENDIF} // parts
{$IFnDef Part1}{$IFnDef Part3} // part 2

      // ******************************
      // **********  PART 2  **********
      // ******************************


  // **** ARRAY ****
  // **** Dyn ARRAY ****

  {$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  pre__CharDynArray{e}    _O2_ array of char          _EQ_      (nil);          //@@ _pre3_CharDynArray{e3};  // open array if used as function arg;
  pre__CharDynArray2{e}   _O2_ array of char          _EQ_      (nil);          //@@ _pre3_CharDynArray2{e3}; // len = 3 // open array if used as function arg;
  {$ENDIF} {$UNDEF NO_TYPE} // TestPointer
  pre__CharDynArray3{e}   _O2_ TCharDynArray          _EQ_      (nil);          //@@ _pre3_CharDynArray3{e3};
  pre__CharDynArray4{e}   _O2_ TCharDynArray          _EQ_      (nil);          //@@ _pre3_CharDynArray4{e3}; // len = 3;

  {$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  pre__WCharDynArray{e}   _O2_ array of widechar      _EQ_      (nil);          //@@ _pre3_WCharDynArray{e3};
  pre__WCharDynArray2{e}  _O2_ array of widechar      _EQ_      (nil);          //@@ _pre3_WCharDynArray2{e3}; // len = 3;
  {$ENDIF} {$UNDEF NO_TYPE} // TestPointer
  pre__WCharDynArray3{e}  _O2_ TWCharDynArray         _EQ_      (nil);          //@@ _pre3_WCharDynArray3{e3};
  pre__WCharDynArray4{e}  _O2_ TWCharDynArray         _EQ_      (nil);          //@@ _pre3_WCharDynArray4{e3}; // len = 3;

  {$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  pre__IntDynArray{e}    _O2_ array of Integer        _EQ_      (nil);          //@@ _pre3_IntDynArray{e3};    // open array if used as function arg;
  pre__IntDynArray2{e}   _O2_ array of Integer        _EQ_      (nil);          //@@ _pre3_IntDynArray2{e3};   // len = 3 // open array if used as function arg;
  {$ENDIF} {$UNDEF NO_TYPE} // TestPointer
  pre__IntDynArray3{e}   _O2_ TIntDynArray            _EQ_      (nil);          //@@ _pre3_IntDynArray3{e3};
  pre__IntDynArray4{e}   _O2_ TIntDynArray            _EQ_      (nil);          //@@ _pre3_IntDynArray4{e3};   // len = 3;

  pre__IntDynArray5{e}   _OP_ TIntDynArray ( nil );                             //@@ _pre3_IntDynArray5{e3};   // REAL CONST = nil

  pre__IntDynArray4_Int{e} _O2_ PtrUInt _EQ_ (0);    //@@ _pre3_IntDynArray4_Int{e3};

  {$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  pre__AnsiDynArray{e}    _O2_ array of AnsiString    _EQ_      (nil);          //@@ _pre3_AnsiDynArray{e3};    // open array if used as function arg;
  pre__AnsiDynArray2{e}   _O2_ array of AnsiString    _EQ_      (nil);          //@@ _pre3_AnsiDynArray2{e3};   // len = 3 // open array if used as function arg;
  {$ENDIF} {$UNDEF NO_TYPE} // TestPointer
  pre__AnsiDynArray3{e}   _O2_ TAnsiDynArray          _EQ_      (nil);          //@@ _pre3_AnsiDynArray3{e3};
  pre__AnsiDynArray4{e}   _O2_ TAnsiDynArray          _EQ_      (nil);          //@@ _pre3_AnsiDynArray4{e3};   // len = 3;

  {$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  pre__ShortStrDynArray{e}    _O2_ array of ShortStr10 _EQ_      (nil);         //@@ _pre3_ShortStrDynArray{e3};    // open array if used as function arg;
  pre__ShortStrDynArray2{e}   _O2_ array of ShortStr10 _EQ_      (nil);         //@@ _pre3_ShortStrDynArray2{e3};   // len = 3 // open array if used as function arg;
  {$ENDIF} {$UNDEF NO_TYPE} // TestPointer
  pre__ShortStrDynArray3{e}   _O2_ TShortStrDynArray   _EQ_      (nil);         //@@ _pre3_ShortStrDynArray3{e3};
  pre__ShortStrDynArray4{e}   _O2_ TShortStrDynArray   _EQ_      (nil);         //@@ _pre3_ShortStrDynArray4{e3};   // len = 3;

  pre__DynDynArrayInt{e}      _O2_ TDynDynArrayInt     _EQ_      (nil);         //@@ _pre3_DynDynArrayInt{e3};
  pre__DynDynArrayInt2{e}     _O2_ TDynDynArrayInt     _EQ_      (nil);         //@@ _pre3_DynDynArrayInt2{e3};

  pre__FiveDynArray{e}             _O2_ TFiveDynArray            _EQ_ (nil);    //@@ _pre3_FiveDynArray{e3};
  pre__FiveDynArrayPack{e}         _O2_ TFiveDynArrayPack        _EQ_ (nil);    //@@ _pre3_FiveDynArrayPack{e3};
  pre__FivePackDynArray{e}         _O2_ TFivePackDynArray        _EQ_ (nil);    //@@ _pre3_FivePackDynArray{e3};
  pre__FivePackDynArrayPack{e}     _O2_ TFivePackDynArrayPack    _EQ_ (nil);    //@@ _pre3_FivePackDynArrayPack{e3};
  pre__RecFiveDynArray{e}          _O2_ TRecFiveDynArray         _EQ_ (nil);    //@@ _pre3_RecFiveDynArray{e3};
  pre__RecFiveDynPackArray{e}      _O2_ TRecFiveDynPackArray     _EQ_ (nil);    //@@ _pre3_RecFiveDynPackArray{e3};
  pre__RecFivePackDynArray{e}      _O2_ TRecFivePackDynArray     _EQ_ (nil);    //@@ _pre3_RecFivePackDynArray{e3};
  pre__RecFivePackDynPackArray{e}  _O2_ TRecFivePackDynPackArray _EQ_ (nil);    //@@ _pre3_RecFivePackDynPackArray{e3};
{$IFnDEF TestAssignGC}
{$IFnDEF TestParam}
{$IFnDEF TestArg}
  {$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  pre__FiveDynArray2{e}            _O2_        array of          record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FiveDynArray2{e3};
  pre__FiveDynArrayPack2{e}        _O2_ packed array of          record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FiveDynArrayPack2{e3};
  pre__FivePackDynArray2{e}        _O2_        array of   packed record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FivePackDynArray2{e3};
  pre__FivePackDynArrayPack2{e}    _O2_ packed array of   packed record a:longint; b: byte end _EQ_ (nil);          //@@ _pre3_FivePackDynArrayPack2{e3};
  {$ENDIF} {$UNDEF NO_TYPE} // TestPointer
{$ENDIF}
{$ENDIF}
{$ENDIF}


  {$IFDEF TestAssign}
  SetLength(pre__CharDynArray2    {e}, 3);    pre__CharDynArray2{e}[0]     _OP_ 'N';          //@@ // }
                                              pre__CharDynArray2{e}[1]     _OP_ CHR1;         //@@
                                              pre__CharDynArray2{e}[2]     _OP_ 'M';          //@@
  SetLength(pre__CharDynArray4    {e}, 3);    pre__CharDynArray4{e}[0]     _OP_ 'J';          //@@ // }
                                              pre__CharDynArray4{e}[1]     _OP_ CHR1;         //@@
                                              pre__CharDynArray4{e}[2]     _OP_ 'M';          //@@

  SetLength(pre__WCharDynArray2   {e}, 3);    pre__WCharDynArray2{e}[0]    _OP_ 'W';          //@@ // }
                                              pre__WCharDynArray2{e}[1]    _OP_ CHR1;         //@@
                                              pre__WCharDynArray2{e}[2]    _OP_ 'M';          //@@
  SetLength(pre__WCharDynArray4   {e}, 3);    pre__WCharDynArray4{e}[0]    _OP_ 'K';          //@@ // }
                                              pre__WCharDynArray4{e}[1]    _OP_ CHR1;         //@@
                                              pre__WCharDynArray4{e}[2]    _OP_ 'M';          //@@

  SetLength(pre__IntDynArray2     {e}, 3);    pre__IntDynArray2{e}[0]      _OP_ 11;         //@@ // }
                                              pre__IntDynArray2{e}[1]      _OP_ 30+ADD;         //@@
                                              pre__IntDynArray2{e}[2]      _OP_ 60;         //@@
  SetLength(pre__IntDynArray4     {e}, 3);    pre__IntDynArray4{e}[0]      _OP_ 12;         //@@ // }
                                              pre__IntDynArray4{e}[1]      _OP_ 30+ADD;         //@@
                                              pre__IntDynArray4{e}[2]      _OP_ 60;         //@@

  pre__IntDynArray4_Int{e} _OP_ PtrUInt(pre__IntDynArray4{e});    //@@ //}}

  SetLength(pre__AnsiDynArray2    {e}, 3);    pre__AnsiDynArray2{e}[0]     _OP_ 'N123';         //@@ // }
                                              pre__AnsiDynArray2{e}[1]     _OP_ CHR1+'ab';         //@@
                                              pre__AnsiDynArray2{e}[2]     _OP_ 'M'#9;             //@@
  SetLength(pre__AnsiDynArray4    {e}, 3);    pre__AnsiDynArray4{e}[0]     _OP_ 'J123';            //@@ // }
                                              pre__AnsiDynArray4{e}[1]     _OP_ CHR1+'ab';         //@@
                                              pre__AnsiDynArray4{e}[2]     _OP_ 'M'#9;             //@@

  SetLength(pre__ShortStrDynArray2{e}, 3);    pre__ShortStrDynArray2{e}[0] _OP_ 'N123';            //@@ // }
                                              pre__ShortStrDynArray2{e}[1] _OP_ CHR1+'ac';         //@@
                                              pre__ShortStrDynArray2{e}[2] _OP_ 'M'#9;             //@@
  SetLength(pre__ShortStrDynArray4{e}, 3);    pre__ShortStrDynArray4{e}[0] _OP_ 'J123';            //@@ // }
                                              pre__ShortStrDynArray4{e}[1] _OP_ CHR1+'ac';         //@@
                                              pre__ShortStrDynArray4{e}[2] _OP_ 'M'#9;             //@@

  SetLength(pre__DynDynArrayInt{e}, 5); // }
  SetLength(pre__DynDynArrayInt{e}[0], 3);    pre__DynDynArrayInt{e}[0][0] := 11+ADD; // }}
                                              pre__DynDynArrayInt{e}[0][1] := 0; // }
                                              pre__DynDynArrayInt{e}[0][2] := -22; // }
  SetLength(pre__DynDynArrayInt{e}[1], 1);    pre__DynDynArrayInt{e}[1][0] := 110+ADD; // }}
            pre__DynDynArrayInt{e}[2]  := pre__DynDynArrayInt{e}[0]; // }}
  SetLength(pre__DynDynArrayInt{e}[3], 0); // }
  SetLength(pre__DynDynArrayInt{e}[4], 4);    pre__DynDynArrayInt{e}[4][0] := 11; // }}
                                              pre__DynDynArrayInt{e}[4][1] := 12; // }
                                              pre__DynDynArrayInt{e}[4][2] := 11; // }
                                              pre__DynDynArrayInt{e}[4][3] := 10; // }
  pre__DynDynArrayInt2 := pre__DynDynArrayInt;

  SetLength(pre__FiveDynArray           {e},3); pre__FiveDynArray           {e}[0].a _OP_ -1-ADD; pre__FiveDynArray           {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArray           {e}[1].a _OP_ -2-ADD; pre__FiveDynArray           {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArray           {e}[2].a _OP_ -3-ADD; pre__FiveDynArray           {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FiveDynArrayPack       {e},3); pre__FiveDynArrayPack       {e}[0].a _OP_ -1-ADD; pre__FiveDynArrayPack       {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArrayPack       {e}[1].a _OP_ -2-ADD; pre__FiveDynArrayPack       {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArrayPack       {e}[2].a _OP_ -3-ADD; pre__FiveDynArrayPack       {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArray       {e},3); pre__FivePackDynArray       {e}[0].a _OP_ -1-ADD; pre__FivePackDynArray       {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArray       {e}[1].a _OP_ -2-ADD; pre__FivePackDynArray       {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArray       {e}[2].a _OP_ -3-ADD; pre__FivePackDynArray       {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArrayPack   {e},3); pre__FivePackDynArrayPack   {e}[0].a _OP_ -1-ADD; pre__FivePackDynArrayPack   {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArrayPack   {e}[1].a _OP_ -2-ADD; pre__FivePackDynArrayPack   {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArrayPack   {e}[2].a _OP_ -3-ADD; pre__FivePackDynArrayPack   {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFiveDynArray        {e},3); pre__RecFiveDynArray        {e}[0].a _OP_ -1-ADD; pre__RecFiveDynArray        {e}[0].b _OP_ 11; // }}}
                                                pre__RecFiveDynArray        {e}[1].a _OP_ -2-ADD; pre__RecFiveDynArray        {e}[1].b _OP_ 22; // }}}
                                                pre__RecFiveDynArray        {e}[2].a _OP_ -3-ADD; pre__RecFiveDynArray        {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFiveDynPackArray    {e},3); pre__RecFiveDynPackArray    {e}[0].a _OP_ -1-ADD; pre__RecFiveDynPackArray    {e}[0].b _OP_ 11; // }}}
                                                pre__RecFiveDynPackArray    {e}[1].a _OP_ -2-ADD; pre__RecFiveDynPackArray    {e}[1].b _OP_ 22; // }}}
                                                pre__RecFiveDynPackArray    {e}[2].a _OP_ -3-ADD; pre__RecFiveDynPackArray    {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFivePackDynArray    {e},3); pre__RecFivePackDynArray    {e}[0].a _OP_ -1-ADD; pre__RecFivePackDynArray    {e}[0].b _OP_ 11; // }}}
                                                pre__RecFivePackDynArray    {e}[1].a _OP_ -2-ADD; pre__RecFivePackDynArray    {e}[1].b _OP_ 22; // }}}
                                                pre__RecFivePackDynArray    {e}[2].a _OP_ -3-ADD; pre__RecFivePackDynArray    {e}[2].b _OP_ 33; // }}}
  SetLength(pre__RecFivePackDynPackArray{e},3); pre__RecFivePackDynPackArray{e}[0].a _OP_ -1-ADD; pre__RecFivePackDynPackArray{e}[0].b _OP_ 11; // }}}
                                                pre__RecFivePackDynPackArray{e}[1].a _OP_ -2-ADD; pre__RecFivePackDynPackArray{e}[1].b _OP_ 22; // }}}
                                                pre__RecFivePackDynPackArray{e}[2].a _OP_ -3-ADD; pre__RecFivePackDynPackArray{e}[2].b _OP_ 33; // }}}
  SetLength(pre__FiveDynArray2          {e},3); pre__FiveDynArray2          {e}[0].a _OP_ -1-ADD; pre__FiveDynArray2          {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArray2          {e}[1].a _OP_ -2-ADD; pre__FiveDynArray2          {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArray2          {e}[2].a _OP_ -3-ADD; pre__FiveDynArray2          {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FiveDynArrayPack2      {e},3); pre__FiveDynArrayPack2      {e}[0].a _OP_ -1-ADD; pre__FiveDynArrayPack2      {e}[0].b _OP_ 11; // }}}
                                                pre__FiveDynArrayPack2      {e}[1].a _OP_ -2-ADD; pre__FiveDynArrayPack2      {e}[1].b _OP_ 22; // }}}
                                                pre__FiveDynArrayPack2      {e}[2].a _OP_ -3-ADD; pre__FiveDynArrayPack2      {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArray2      {e},3); pre__FivePackDynArray2      {e}[0].a _OP_ -1-ADD; pre__FivePackDynArray2      {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArray2      {e}[1].a _OP_ -2-ADD; pre__FivePackDynArray2      {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArray2      {e}[2].a _OP_ -3-ADD; pre__FivePackDynArray2      {e}[2].b _OP_ 33; // }}}
  SetLength(pre__FivePackDynArrayPack2  {e},3); pre__FivePackDynArrayPack2  {e}[0].a _OP_ -1-ADD; pre__FivePackDynArrayPack2  {e}[0].b _OP_ 11; // }}}
                                                pre__FivePackDynArrayPack2  {e}[1].a _OP_ -2-ADD; pre__FivePackDynArrayPack2  {e}[1].b _OP_ 22; // }}}
                                                pre__FivePackDynArrayPack2  {e}[2].a _OP_ -3-ADD; pre__FivePackDynArrayPack2  {e}[2].b _OP_ 33; // }}}
  {$ENDIF}

  // **** Static ARRAY ****
{$IFnDEF TestParam}
{$IFnDEF TestArg}
{$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  {$IFnDEF TestAssign}
  pre__CharStatArray{e}     _O2_ array[1..5] of char       _EQ_      (CHR1, 'b',CHR1, 'B','c');                //@@ _pre3_CharStatArray{e3};
  pre__WCharStatArray{e}    _O2_ array[1..5] of widechar   _EQ_      (CHR1, 'b',CHR1, 'B','d');                //@@ _pre3_WCharStatArray{e3};
  pre__IntStatArray{e}      _O2_ array[1..5] of Integer    _EQ_      (-1, 300+ADD, 2, 0, 1);                   //@@ _pre3_IntStatArray{e3};
  pre__AnsiStatArray{e}     _O2_ array[1..5] of AnsiString _EQ_      (CHR1, 'b123',CHR1+'ab', 'B','cdef'#9);   //@@ _pre3_AnsiStatArray{e3};
  pre__ShortStrStatArray{e} _O2_ array[1..5] of ShortStr10 _EQ_      (CHR1, 'b123',CHR1+'ab', 'C','cdef'#9);   //@@ _pre3_ShortStrStatArray{e3};

//  pre__FiveStatArray2{e}         _O2_        array [2..4] of        record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArray2{e3};
//  pre__FiveStatArrayPack2{e}     _O2_ packed array [2..4] of        record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArrayPack2{e3};
//  pre__FivePackStatArray2{e}     _O2_        array [2..4] of packed record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArray2{e3};
//  pre__FivePackStatArrayPack2{e} _O2_ packed array [2..4] of packed record a:longint; b: byte end _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArrayPack2{e3};

  pre__ArrayEnum1{e}      _O2_  array [TEnum]          of word  _EQ_ (500+ADD,701,702,703); //@@ _pre3_ArrayEnum1{e3};
  pre__ArrayEnumSub1{e}   _O2_  array [TEnumSub]       of word  _EQ_ (600+ADD,801);         //@@ _pre3_ArrayEnumSub1{e3};
  pre__ArrayEnum2{e}      _O2_  array [EnVal1..EnVal4] of word  _EQ_ (300+ADD,701,702,703); //@@ _pre3_ArrayEnum2{e3};
  pre__ArrayEnumSub2{e}   _O2_  array [EnVal1..EnVal2] of word  _EQ_ (400+ADD,801);         //@@ _pre3_ArrayEnumSub2{e3};
  {$ELSE}
  pre__CharStatArray{e}     := _pre2_CharStatArray;    pre__CharStatArray{e}[1]  := CHR1;    pre__CharStatArray{e}[3] := CHR1;          //@@ _pre3_CharStatArray{e3};   // }}}
  pre__WCharStatArray{e}    := _pre2_WCharStatArray;   pre__WCharStatArray{e}[1] := CHR1;   pre__WCharStatArray{e}[3] := CHR1;          //@@ _pre3_WCharStatArray{e3};  // }}}
  pre__IntStatArray{e}      := _pre2_IntStatArray;     pre__IntStatArray{e}[2]   := 300+ADD;                                            //@@ _pre3_IntStatArray{e3};    // }}}
  pre__AnsiStatArray{e}     := _pre2_AnsiStatArray;    pre__AnsiStatArray{e}[1]  := CHR1;    pre__AnsiStatArray{e}[3] := CHR1+'ab';     //@@ _pre3_AnsiStatArray{e3};   // }}}
  pre__ShortStrStatArray{e} := _pre2_ShortStrStatArray;pre__ShortStrStatArray{e}[1] := CHR1;pre__ShortStrStatArray{e}[3] := CHR1+'ab';  //@@ _pre3_ShortStrStatArray{e3}; // }}}

//  pre__FiveStatArray2{e}         := _pre2_FiveStatArray2;         pre__FiveStatArray2{e}        [3].a := -8-ADD;          //@@ _pre3_FiveStatArray2{e3};
//  pre__FiveStatArrayPack2{e}     := _pre2_FiveStatArrayPack2;     pre__FiveStatArrayPack2{e}    [3].a := -8-ADD;          //@@ _pre3_FiveStatArrayPack2{e3};
//  pre__FivePackStatArray2{e}     := _pre2_FivePackStatArray2;     pre__FivePackStatArray2{e}    [3].a := -8-ADD;          //@@ _pre3_FivePackStatArray2{e3};
//  pre__FivePackStatArrayPack2{e} := _pre2_FivePackStatArrayPack2; pre__FivePackStatArrayPack2{e}[3].a := -8-ADD;          //@@ _pre3_FivePackStatArrayPack2{e3};

  pre__ArrayEnum1{e}    := _pre2_ArrayEnum1;    pre__ArrayEnum1{e}[EnVal1]    := 500+ADD;   //@@ _pre3_ArrayEnum1{e3}; // }}
  pre__ArrayEnumSub1{e} := _pre2_ArrayEnumSub1; pre__ArrayEnumSub1{e}[EnVal1] := 600+ADD;   //@@ _pre3_ArrayEnumSub1{e3}; // }}
  pre__ArrayEnum2{e}    := _pre2_ArrayEnum2;    pre__ArrayEnum2{e}[EnVal1]    := 300+ADD;   //@@ _pre3_ArrayEnum2{e3}; // }}
  pre__ArrayEnumSub2{e} := _pre2_ArrayEnumSub2; pre__ArrayEnumSub2{e}[EnVal1] := 400+ADD;   //@@ _pre3_ArrayEnumSub2{e3}; // }}
  {$ENDIF}
{$ENDIF} {$UNDEF NO_TYPE} // TestPointer
{$ENDIF}
{$ENDIF}

  {$IFnDEF TestAssign}
  pre__CharStatArray2{e}     _O2_ TCharStatArray         _EQ_      (CHR1, 'c',CHR1, 'B','c');          //@@ _pre3_CharStatArray2{e3};
  pre__WCharStatArray2{e}    _O2_ TWCharStatArray        _EQ_      (CHR1, 'c',CHR1, 'B','d');          //@@ _pre3_WCharStatArray2{e3};
  pre__IntStatArray2{e}      _O2_ TIntStatArray          _EQ_      (-2, 200+ADD, 2, 0, 1);          //@@ _pre3_IntStatArray2{e3};
  pre__AnsiStatArray2{e}     _O2_ TAnsiStatArray         _EQ_      (CHR1, 'c123',CHR1+'ad', 'D','cxx'#9);          //@@ _pre3_AnsiStatArray2{e3};
  pre__ShortStrStatArray2{e} _O2_ TShortStrStatArray     _EQ_      (CHR1, 'c123',CHR1+'ad', 'C','cxx'#9);          //@@ _pre3_ShortStrStatArray2{e3};

  pre__FiveStatArray{e}             _O2_ TFiveStatArray            _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArray{e3};          // }}}
  pre__FiveStatArrayPack{e}         _O2_ TFiveStatArrayPack        _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FiveStatArrayPack{e3};      // }}}
  pre__FivePackStatArray{e}         _O2_ TFivePackStatArray        _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArray{e3};      // }}}
  pre__FivePackStatArrayPack{e}     _O2_ TFivePackStatArrayPack    _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_FivePackStatArrayPack{e3};  // }}}
  pre__RecFiveStatArray{e}          _O2_ TRecFiveStatArray         _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatArray{e3};       // }}}
  pre__RecFiveStatPackArray{e}      _O2_ TRecFiveStatPackArray     _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFiveStatPackArray{e3};   // }}}
  pre__RecFivePackStatArray{e}      _O2_ TRecFivePackStatArray     _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatArray{e3};   // }}}
  pre__RecFivePackStatPackArray{e}  _O2_ TRecFivePackStatPackArray _EQ_ ((a:-9;b:44), (a:-8-ADD;b:33), (a:-7;b:22));          //@@ _pre3_RecFivePackStatPackArray{e3};  // }}}

  pre__ArrayEnum3{e}      _O2_  TArrayEnum         _EQ_ (200+ADD,701,702,703); //@@ _pre3_ArrayEnum3{e3};
  pre__ArrayEnumSub3{e}   _O2_  TArrayEnumSub      _EQ_ (100+ADD,801);         //@@ _pre3_ArrayEnumSub3{e3};
  pre__ArrayEnum4{e}      _O2_  TArrayEnumElem     _EQ_ (800+ADD,701,702,703); //@@ _pre3_ArrayEnum4{e3};
  pre__ArrayEnumSub4{e}   _O2_  TArrayEnumSubElem  _EQ_ (700+ADD,801);         //@@ _pre3_ArrayEnumSub4{e3};
  {$ELSE}
  pre__CharStatArray2{e}     := _pre2_CharStatArray2;    pre__CharStatArray2{e}[1]  := CHR1;    pre__CharStatArray2{e}[3] := CHR1;          //@@ _pre3_CharStatArray2{e3};  // }}}
  pre__WCharStatArray2{e}    := _pre2_WCharStatArray2;   pre__WCharStatArray2{e}[1] := CHR1;   pre__WCharStatArray2{e}[3] := CHR1;          //@@ _pre3_WCharStatArray2{e3};  // }}}
  pre__IntStatArray2{e}      := _pre2_IntStatArray2;     pre__IntStatArray2{e}[2]   := 200+ADD;                                             //@@ _pre3_IntStatArray2{e3};    // }}}
  pre__AnsiStatArray2{e}     := _pre2_AnsiStatArray2;    pre__AnsiStatArray2{e}[1]  := CHR1;    pre__AnsiStatArray2{e}[3] := CHR1+'ad';     //@@ _pre3_AnsiStatArray2{e3};   // }}}
  pre__ShortStrStatArray2{e} := _pre2_ShortStrStatArray2;pre__ShortStrStatArray2{e}[1] := CHR1;pre__ShortStrStatArray2{e}[3] := CHR1+'ad';  //@@ _pre3_ShortStrStatArray2{e3};  // }}}

  pre__FiveStatArray{e}            := _pre2_FiveStatArray;            pre__FiveStatArray{e}           [3].a := -8-ADD;          //@@ _pre3_FiveStatArray{e3};  // }}}
  pre__FiveStatArrayPack{e}        := _pre2_FiveStatArrayPack;        pre__FiveStatArrayPack{e}       [3].a := -8-ADD;          //@@ _pre3_FiveStatArrayPack{e3};  // }}}
  pre__FivePackStatArray{e}        := _pre2_FivePackStatArray;        pre__FivePackStatArray{e}       [3].a := -8-ADD;          //@@ _pre3_FivePackStatArray{e3};  // }}}
  pre__FivePackStatArrayPack{e}    := _pre2_FivePackStatArrayPack;    pre__FivePackStatArrayPack{e}   [3].a := -8-ADD;          //@@ _pre3_FivePackStatArrayPack{e3};  // }}}
  pre__RecFiveStatArray{e}         := _pre2_RecFiveStatArray;         pre__RecFiveStatArray{e}        [3].a := -8-ADD;          //@@ _pre3_RecFiveStatArray{e3};  // }}}
  pre__RecFiveStatPackArray{e}     := _pre2_RecFiveStatPackArray;     pre__RecFiveStatPackArray{e}    [3].a := -8-ADD;          //@@ _pre3_RecFiveStatPackArray{e3};  // }}}
  pre__RecFivePackStatArray{e}     := _pre2_RecFivePackStatArray;     pre__RecFivePackStatArray{e}    [3].a := -8-ADD;          //@@ _pre3_RecFivePackStatArray{e3};  // }}}
  pre__RecFivePackStatPackArray{e} := _pre2_RecFivePackStatPackArray; pre__RecFivePackStatPackArray{e}[3].a := -8-ADD;          //@@ _pre3_RecFivePackStatPackArray{e3};  // }}}

  pre__ArrayEnum3{e}    := _pre2_ArrayEnum3;    pre__ArrayEnum3{e}[EnVal1]    := 200+ADD;   //@@ _pre3_ArrayEnum3{e3}; // }}
  pre__ArrayEnumSub3{e} := _pre2_ArrayEnumSub3; pre__ArrayEnumSub3{e}[EnVal1] := 100+ADD;   //@@ _pre3_ArrayEnumSub3{e3}; // }}
  pre__ArrayEnum4{e}    := _pre2_ArrayEnum4;    pre__ArrayEnum4{e}[EnVal1]    := 800+ADD;   //@@ _pre3_ArrayEnum4{e3}; // }}
  pre__ArrayEnumSub4{e} := _pre2_ArrayEnumSub4; pre__ArrayEnumSub4{e}[EnVal1] := 700+ADD;   //@@ _pre3_ArrayEnumSub4{e3}; // }}
  {$ENDIF}

  pre__Enum{e}  _OP_ TEnum(EnVal3);          //@@ _pre3_Enum{e3};
  pre__EnumA{e} _OP_ TEnum(EnVal1);          //@@ _pre3_EnumA{e3};
  pre__Enum1{e} _OP_ TEnumSub(EnVal2);         //@@ _pre3_Enum1{e3};   // subset;
  pre__Enum2{e} _OP_ TEnum2(EnVal21);          //@@ _pre3_Enum2{e3};
  pre__Enum3{e} _OP_ TEnum2(EnVal25);          //@@ _pre3_Enum3{e3};

  pre__Enum16{e}  _OP_ TEnum16(ExVal23);         //@@ _pre3_Enum16{e3};
  pre__Enum16A{e} _OP_ TEnum16(ExValX5);         //@@ _pre3_Enum16A{e3};

  pre__EnumX0a{e}  _OP_ TEnumX0(EnXVal01);          //@@ _pre3_EnumX0a{e3};
  pre__EnumX0b{e}  _OP_ TEnumX0(EnXVal04);          //@@ _pre3_EnumX0b{e3};
  pre__EnumX1a{e}  _OP_ TEnumX1(EnXVal11);          //@@ _pre3_EnumX1a{e3};
  pre__EnumX1b{e}  _OP_ TEnumX1(EnXVal14);          //@@ _pre3_EnumX1b{e3};
  pre__EnumX1Aa{e}  _OP_ TEnumX1a(EnXValA11);          //@@ _pre3_EnumX1Aa{e3};
  pre__EnumX1Ab{e}  _OP_ TEnumX1a(EnXValA14);          //@@ _pre3_EnumX1Ab{e3};
  pre__EnumX2a{e}  _OP_ TEnumX2(EnXVal21);          //@@ _pre3_EnumX2a{e3};
  pre__EnumX2b{e}  _OP_ TEnumX2(EnXVal24);          //@@ _pre3_EnumX2b{e3};

  pre__Set{e}      _OP_ TSet([EnVal2, EnVal4]);          //@@ _pre3_Set{e3};
  pre__SmallSet{e} _OP_ TSmallRangeSet([22, 24,25]);     //@@ _pre3_SmallSet{e3};

{$IFnDEF TestParam}
{$IFnDEF TestArg}
{$IFnDEF TestPointer} {$DEFINE NO_TYPE}
  pre__Set2{e}      _O2_ set of TEnum       _EQ_ ([EnVal1, EnVal4]);      //@@ _pre3_Set2{e3};
  pre__SmallSet2{e} _O2_ set of TSmallRange _EQ_ ([21, 24,25]);           //@@ _pre3_SmallSet2{e3};
{$ENDIF} {$UNDEF NO_TYPE}
{$ENDIF}
{$ENDIF}

  pre__Set4{e}      _OP_ TSet4([E4Val02, E4Val0A]);          //@@ _pre3_Set4{e3};
  pre__Set5{e}      _OP_ TSet5([E5Val02, E5Val12]);          //@@ _pre3_Set5{e3};
  pre__Set6{e}      _OP_ TSet6([E6Val02, E6Val1A]);          //@@ _pre3_Set6{e3};
  pre__Set7{e}      _OP_ TSet7([E7Val02, E7Val3A]);          //@@ _pre3_Set7{e3};
  pre__Set8{e}      _OP_ TSet8([E8Val02, E8Val5B]);          //@@ _pre3_Set8{e3};


{$ENDIF}{$ENDIF} // parts
{$IFnDef Part1}{$IFnDef Part2} // part 3

      // ******************************
      // **********  PART 3  **********
      // ******************************


  {$IFnDEF TestAssign}
  pre__BitPackBoolArray{e}      _O2_ TBitPackBoolArray    _EQ_ (True, False, True, True);          //@@ _pre3_BitPackBoolArray{e3}; // }}
  pre__BitPackTinyArray{e}      _O2_ TBitPackTinyArray    _EQ_ (1, 0, 3, 2);          //@@ _pre3_BitPackTinyArray{e3}; // }}
  pre__BitPackTinyNegArray{e}   _O2_ TBitPackTinyNegArray _EQ_ (2, 6{-2}, 0, 7{-1});          //@@ _pre3_BitPackTinyNegArray{e3}; // }}
  pre__BitPackEnumArray{e}      _O2_ TBitPackEnumArray    _EQ_ (EnVal3, EnVal1, EnVal2, EnVal3);          //@@ _pre3_BitPackEnumArray{e3}; // }}
  pre__BitPackEnum3Array{e}     _O2_ TBitPackEnum3Array   _EQ_ (EnVal32, EnVal32, EnVal31, EnVal32);          //@@ _pre3_BitPackEnum3Array{e3}; // }}
  pre__BitPackSetArray{e}       _O2_ TBitPackSetArray     _EQ_ ([EnVal3, EnVal1], [], [EnVal3], [EnVal1]);          //@@ _pre3_BitPackSetArray{e3}; // }}
  pre__BitPackSet3Array{e}      _O2_ TBitPackSet3Array    _EQ_ ([EnVal31, EnVal32], [], [EnVal31], [EnVal32]);          //@@ _pre3_BitPackSet3Array{e3}; // }}

  pre__BitPackBoolArray2{e}      _O2_ TBitPackBoolArray2    _EQ_ ((True, False, True), (False, True, True));          //@@ _pre3_BitPackBoolArray2{e3}; // }}
  pre__BitPackTinyArray2{e}      _O2_ TBitPackTinyArray2    _EQ_ ((1, 0, 3), (2,3,0));          //@@ _pre3_BitPackTinyArray2{e3}; // }}
  pre__BitPackTinyNegArray2{e}   _O2_ TBitPackTinyNegArray2 _EQ_ ((2, 6{-2}, 0), (1, 0, 7{-1}));          //@@ _pre3_BitPackTinyNegArray2{e3}; // }}
  pre__BitPackEnumArray2{e}      _O2_ TBitPackEnumArray2    _EQ_ ((EnVal3, EnVal1, EnVal2), (EnVal1, EnVal4, EnVal2));          //@@ _pre3_BitPackEnumArray2{e3}; // }}
  pre__BitPackEnum3Array2{e}     _O2_ TBitPackEnum3Array2   _EQ_ ((EnVal32, EnVal32, EnVal31), (EnVal31, EnVal31, EnVal32));          //@@ _pre3_BitPackEnum3Array2{e3}; // }}
//  pre__BitPackSetArray2{e}       _O2_ TBitPackSetArray2     _EQ_ (([EnVal3, EnVal1], [], [EnVal3]), ([],[EnVal1,EnVal2],[EnVal1]));          //@@ _pre3_BitPackSetArray2{e3}; // }}
//  pre__BitPackSet3Array2{e}      _O2_ TBitPackSet3Array2    _EQ_ (([EnVal31, EnVal32], [], [EnVal31]), ([EnVal31,EnVal32],[],[EnVal32]));          //@@ _pre3_BitPackSet3Array2{e3}; // }}

  pre__BitPackBoolRecord{e}      _O2_ TBitPackBoolRecord    _EQ_ (a:True; b:False; c:True; d: True; e: False);          //@@ _pre3_BitPackBoolRecord{e3}; // }}
  pre__BitPackTinyRecord{e}      _O2_ TBitPackTinyRecord    _EQ_ (a:1; b:1; c:0; d: 3; e: 0);          //@@ _pre3_BitPackTinyRecord{e3}; // }}
  pre__BitPackTinyNegRecord{e}   _O2_ TBitPackTinyNegRecord _EQ_ (a:3; b:6{-2}; c:7{-1}; d: 0; e: 1);          //@@ _pre3_BitPackTinyNegRecord{e3}; // }}
  pre__BitPackEnumRecord{e}      _O2_ TBitPackEnumRecord    _EQ_ (a:EnVal3; b:EnVal1; c:EnVal2; d: EnVal2; e: EnVal1);          //@@ _pre3_BitPackEnumRecord{e3}; // }}
  pre__BitPackEnum3Record{e}     _O2_ TBitPackEnum3Record   _EQ_ (a:EnVal31; b:EnVal32; c:EnVal31; d: EnVal31; e: EnVal32);          //@@ _pre3_BitPackEnum3Record{e3}; // }}
  pre__BitPackSetRecord{e}       _O2_ TBitPackSetRecord     _EQ_ (a:[EnVal3]; b:[]; c:[EnVal1,EnVal2]; d: [EnVal2]; e: [EnVal1,EnVal3]);          //@@ _pre3_BitPackSetRecord{e3}; // }}
  pre__BitPackSet3Record{e}      _O2_ TBitPackSet3Record    _EQ_ (a:[EnVal31]; b:[]; c:[EnVal31,EnVal32]; d: [EnVal32]; e: [EnVal31]);          //@@ _pre3_BitPackSet3Record{e3}; // }}

  pre__BitPackBoolArrayRecord{e}      _O2_ TBitPackBoolArrayRecord    _EQ_ (a:(True, False, True, True); b:(False, True, True, False));          //@@ _pre3_BitPackBoolArrayRecord{e3}; // }}
  pre__BitPackTinyArrayRecord{e}      _O2_ TBitPackTinyArrayRecord    _EQ_ (a:(1, 0, 3, 2); b:(2,3,0,1));          //@@ _pre3_BitPackTinyArrayRecord{e3}; // }}
  pre__BitPackTinyNegArrayRecord{e}   _O2_ TBitPackTinyNegArrayRecord _EQ_ (a:(2, 6{-2}, 0, 7{-1}); b:(1, 0, 7{-1},2));          //@@ _pre3_BitPackTinyNegArrayRecord{e3}; // }}
  pre__BitPackEnumArrayRecord{e}      _O2_ TBitPackEnumArrayRecord    _EQ_ (a:(EnVal3, EnVal1, EnVal2, EnVal4); b:(EnVal1, EnVal4, EnVal2, EnVal4));          //@@ _pre3_BitPackEnumArrayRecord{e3}; // }}
  pre__BitPackEnum3ArrayRecord{e}     _O2_ TBitPackEnum3ArrayRecord   _EQ_ (a:(EnVal32, EnVal32, EnVal31, EnVal31); b:(EnVal31, EnVal31, EnVal32, EnVal31));          //@@ _pre3_BitPackEnum3ArrayRecord{e3}; // }}
  pre__BitPackSetArrayRecord{e}       _O2_ TBitPackSetArrayRecord     _EQ_ (a:([EnVal3, EnVal1], [], [EnVal3], []); b:([],[EnVal1,EnVal2],[EnVal1],[EnVal4]));          //@@ _pre3_BitPackSetArrayRecord{e3}; // }}
  pre__BitPackSet3ArrayRecord{e}      _O2_ TBitPackSet3ArrayRecord    _EQ_ (a:([EnVal31, EnVal32], [], [EnVal31],[EnVal31,EnVal32]); b:([EnVal31,EnVal32],[],[EnVal32],[]));          //@@ _pre3_BitPackSet3ArrayRecord{e3}; // }}


  pre__FpDbgValueSize{e}   _O2_ TFpDbgValueSize    _EQ_ (Size: 0; BitSize: 2);          //@@ _pre3_FpDbgValueSize{e3}; // }}

  {$ELSE}
  pre__BitPackBoolArray{e}    := _pre2_BitPackBoolArray;         //@@ _pre3_BitPackBoolArray{e3}; // }}
  pre__BitPackTinyArray{e}    := _pre2_BitPackTinyArray;         //@@ _pre3_BitPackTinyArray{e3}; // }}
  pre__BitPackTinyNegArray{e} := _pre2_BitPackTinyNegArray;      //@@ _pre3_BitPackTinyNegArray{e3}; // }}
  pre__BitPackEnumArray{e}    := _pre2_BitPackEnumArray;   //@@ _pre3_BitPackEnumArray{e3}; // }}
  pre__BitPackEnum3Array{e}   := _pre2_BitPackEnum3Array;  //@@ _pre3_BitPackEnum3Array{e3}; // }}
  pre__BitPackSetArray{e}     := _pre2_BitPackSetArray;    //@@ _pre3_BitPackSetArray{e3}; // }}
  pre__BitPackSet3Array{e}    := _pre2_BitPackSet3Array;   //@@ _pre3_BitPackSet3Array{e3}; // }}

  pre__BitPackBoolArray2{e}    := _pre2_BitPackBoolArray2;         //@@ _pre3_BitPackBoolArray2{e3}; // }}
  pre__BitPackTinyArray2{e}    := _pre2_BitPackTinyArray2;         //@@ _pre3_BitPackTinyArray2{e3}; // }}
  pre__BitPackTinyNegArray2{e} := _pre2_BitPackTinyNegArray2;      //@@ _pre3_BitPackTinyNegArray2{e3}; // }}
  pre__BitPackEnumArray2{e}    := _pre2_BitPackEnumArray2;   //@@ _pre3_BitPackEnumArray2{e3}; // }}
  pre__BitPackEnum3Array2{e}   := _pre2_BitPackEnum3Array2;  //@@ _pre3_BitPackEnum3Array2{e3}; // }}
//  pre__BitPackSetArray2{e}     := _pre2_BitPackSetArray2;    //@@ _pre3_BitPackSetArray2{e3}; // }}
//  pre__BitPackSet3Array2{e}    := _pre2_BitPackSet3Array2;   //@@ _pre3_BitPackSet3Array2{e3}; // }}

  {$IFnDEF TestType}
  pre__BitPackBoolRecord{e}    := _pre2_BitPackBoolRecord;      //@@ _pre3_BitPackBoolRecord{e3}; // }}
  pre__BitPackTinyRecord{e}    := _pre2_BitPackTinyRecord;      //@@ _pre3_BitPackTinyRecord{e3}; // }}
  pre__BitPackTinyNegRecord{e} := _pre2_BitPackTinyNegRecord;   //@@ _pre3_BitPackTinyNegRecord{e3}; // }}
  pre__BitPackEnumRecord{e}    := _pre2_BitPackEnumRecord;      //@@ _pre3_BitPackEnumRecord{e3}; // }}
  pre__BitPackEnum3Record{e}   := _pre2_BitPackEnum3Record;     //@@ _pre3_BitPackEnum3Record{e3}; // }}
  pre__BitPackSetRecord{e}     := _pre2_BitPackSetRecord;       //@@ _pre3_BitPackSetRecord{e3}; // }}
  pre__BitPackSet3Record{e}    := _pre2_BitPackSet3Record;      //@@ _pre3_BitPackSet3Record{e3}; // }}

  pre__BitPackBoolArrayRecord{e}    := _pre2_BitPackBoolArrayRecord;         //@@ _pre3_BitPackBoolArrayRecord{e3}; // }}
  pre__BitPackTinyArrayRecord{e}    := _pre2_BitPackTinyArrayRecord;         //@@ _pre3_BitPackTinyArrayRecord{e3}; // }}
  pre__BitPackTinyNegArrayRecord{e} := _pre2_BitPackTinyNegArrayRecord;      //@@ _pre3_BitPackTinyNegArrayRecord{e3}; // }}
  pre__BitPackEnumArrayRecord{e}    := _pre2_BitPackEnumArrayRecord;   //@@ _pre3_BitPackEnumArrayRecord{e3}; // }}
  pre__BitPackEnum3ArrayRecord{e}   := _pre2_BitPackEnum3ArrayRecord;  //@@ _pre3_BitPackEnum3ArrayRecord{e3}; // }}
  pre__BitPackSetArrayRecord{e}     := _pre2_BitPackSetArrayRecord;    //@@ _pre3_BitPackSetArrayRecord{e3}; // }}
  pre__BitPackSet3ArrayRecord{e}    := _pre2_BitPackSet3ArrayRecord;   //@@ _pre3_BitPackSet3ArrayRecord{e3}; // }}

  pre__FpDbgValueSize{e}  :=  _pre2_FpDbgValueSize;  pre__FpDbgValueSize{e}.Size  :=  0;  //@@ _pre3_FpDbgValueSize{e3}; // }}
  {$ELSE}
  pre__BitPackBoolRecord{e}    := TxBitPackBoolRecord(_pre2_BitPackBoolRecord);      //@@ _pre3_BitPackBoolRecord{e3}; // }}
  pre__BitPackTinyRecord{e}    := TxBitPackTinyRecord(_pre2_BitPackTinyRecord);      //@@ _pre3_BitPackTinyRecord{e3}; // }}
  pre__BitPackTinyNegRecord{e} := TxBitPackTinyNegRecord(_pre2_BitPackTinyNegRecord);   //@@ _pre3_BitPackTinyNegRecord{e3}; // }}
  pre__BitPackEnumRecord{e}    := TxBitPackEnumRecord(_pre2_BitPackEnumRecord);      //@@ _pre3_BitPackEnumRecord{e3}; // }}
  pre__BitPackEnum3Record{e}   := TxBitPackEnum3Record(_pre2_BitPackEnum3Record);     //@@ _pre3_BitPackEnum3Record{e3}; // }}
  pre__BitPackSetRecord{e}     := TxBitPackSetRecord(_pre2_BitPackSetRecord);       //@@ _pre3_BitPackSetRecord{e3}; // }}
  pre__BitPackSet3Record{e}    := TxBitPackSet3Record(_pre2_BitPackSet3Record);      //@@ _pre3_BitPackSet3Record{e3}; // }}

  pre__BitPackBoolArrayRecord{e}    := TxBitPackBoolArrayRecord(_pre2_BitPackBoolArrayRecord);         //@@ _pre3_BitPackBoolArrayRecord{e3}; // }}
  pre__BitPackTinyArrayRecord{e}    := TxBitPackTinyArrayRecord(_pre2_BitPackTinyArrayRecord);         //@@ _pre3_BitPackTinyArrayRecord{e3}; // }}
  pre__BitPackTinyNegArrayRecord{e} := TxBitPackTinyNegArrayRecord(_pre2_BitPackTinyNegArrayRecord);      //@@ _pre3_BitPackTinyNegArrayRecord{e3}; // }}
  pre__BitPackEnumArrayRecord{e}    := TxBitPackEnumArrayRecord(_pre2_BitPackEnumArrayRecord);   //@@ _pre3_BitPackEnumArrayRecord{e3}; // }}
  pre__BitPackEnum3ArrayRecord{e}   := TxBitPackEnum3ArrayRecord(_pre2_BitPackEnum3ArrayRecord);  //@@ _pre3_BitPackEnum3ArrayRecord{e3}; // }}
  pre__BitPackSetArrayRecord{e}     := TxBitPackSetArrayRecord(_pre2_BitPackSetArrayRecord);    //@@ _pre3_BitPackSetArrayRecord{e3}; // }}
  pre__BitPackSet3ArrayRecord{e}    := TxBitPackSet3ArrayRecord(_pre2_BitPackSet3ArrayRecord);   //@@ _pre3_BitPackSet3ArrayRecord{e3}; // }}

  pre__FpDbgValueSize{e}  :=  TxFpDbgValueSize(_pre2_FpDbgValueSize);   //@@ _pre3_FpDbgValueSize{e3}; // }}
  {$ENDIF}

  {$ENDIF}


  {$IFnDEF TestAssign}
    pre__FiveRec{e}       _O2_ TRecordFive      _EQ_ (a:-22-ADD;b:44);          //@@ _pre3_FiveRec{e3};
    pre__Rec3S{e}         _O2_ TRecord3Int64    _EQ_ (a:-22;b:44;c:1000+ADD);          //@@ _pre3_Rec3S{e3};
    pre__Rec3U{e}         _O2_ TRecord3QWord    _EQ_ (a:111;b:44;c:1000+ADD);          //@@ _pre3_Rec3U{e3};

    pre__Obj3{e}          _O2_ TObject3Int64          _EQ_ (a:-22;b:44;c:4000+ADD);          //@@ _pre3_Obj3{e3};
    pre__Obj3Ex{e}        _O2_ TObject3Int64Ex        _EQ_ (a:-22;b:44;c:4100+ADD; d: 555);  //@@ _pre3_Obj3Ex{e3};
    {$IFnDEF TestConst}
    {$IFnDEF TestAssignGC}
    {$IFnDEF TestType} // will work in fpc 3.2.0 upwards
    pre__Obj3C{e}         _O2_ TObjectCreate3Int64   ;  //@@ _pre3_Obj3C{e3}; // }}}
    pre__Obj3ExC{e}       _O2_ TObjectCreate3Int64Ex ;  //@@ _pre3_Obj3ExC{e3}; // }}}
    {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  {$ELSE} // TestAssign
    {$IFDEF TestType} // incomplete values
    pre__FiveRec{e} := pre__FiveRec;   pre__FiveRec{e}.a := -22-ADD;          //@@ _pre3_FiveRec{e3};  // }}}
    pre__Rec3S{e}   := pre__Rec3S;     pre__Rec3S{e}.c := 1000+ADD;          //@@ _pre3_Rec3S{e3};  // }}}
    pre__Rec3U{e}   := pre__Rec3U;     pre__Rec3U{e}.c := 1000+ADD;          //@@ _pre3_Rec3U{e3};  // }}}

    pre__Obj3{e}    := pre__Obj3;      pre__Obj3{e}.c    := 4000+ADD;  //@@ _pre3_Obj3{e3};  // }}}
    pre__Obj3Ex{e}  := pre__Obj3Ex;    pre__Obj3Ex{e}.c  := 4100+ADD;  //@@ _pre3_Obj3Ex{e3};  // }}}
    {$ELSE}
    pre__FiveRec{e} := _pre2_FiveRec;   pre__FiveRec{e}.a := -22-ADD;          //@@ _pre3_FiveRec{e3};  // }}}
    pre__Rec3S{e}   := _pre2_Rec3S;     pre__Rec3S{e}.c := 1000+ADD;          //@@ _pre3_Rec3S{e3};  // }}}
    pre__Rec3U{e}   := _pre2_Rec3U;     pre__Rec3U{e}.c := 1000+ADD;          //@@ _pre3_Rec3U{e3};  // }}}

    pre__Obj3{e}    := _pre2_Obj3;      pre__Obj3{e}.c    := 4000+ADD;  //@@ _pre3_Obj3{e3};  // }}}
    pre__Obj3Ex{e}  := _pre2_Obj3Ex;    pre__Obj3Ex{e}.c  := 4100+ADD;  //@@ _pre3_Obj3Ex{e3};  // }}}
    {$ENDIF}

    {$IFnDEF TestConst}
    {$IFnDEF TestType} // will work in fpc 3.2.0 upwards
    pre__Obj3C{e}.Create;   pre__Obj3C{e}.a   := 22; pre__Obj3C{e}.b   := 44; pre__Obj3C{e}.c   := 4200+ADD;  //@@ _pre3_Obj3C{e3};  // }}}}}
    pre__Obj3ExC{e}.Create; pre__Obj3ExC{e}.a := 22; pre__Obj3ExC{e}.b := 44; pre__Obj3ExC{e}.c := 4300+ADD; pre__Obj3ExC{e}.d := 655; //@@ _pre3_Obj3ExC{e3};  // }}}}}
    {$ENDIF}
    {$ENDIF}
  {$ENDIF}


  pre__Instance0{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance0{e3};
  pre__Instance1{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance1{e3};
  pre__Instance1_Int{e} _O2_ PtrUInt _EQ_ (0);    //@@ _pre3_Instance1_Int{e3};
  {$IFDEF TestAssign}
  {$IFDEF TestType}
  pre__Instance1{e} := TxInstance1.Create;    //@@
  {$ELSE}
  pre__Instance1{e} := TClass1.Create;    //@@
  {$ENDIF}
  pre__Instance1{e}.FInt _OP_ 22+ADD;     //@@
  pre__Instance1{e}.FAnsi _OP_ CHR1+'T';  //@@
  pre__Instance1_Int{e} _OP_ PtrUInt(pre__Instance1{e});    //@@ //}}
  {$ENDIF}

  {$IFnDEF TestType}
  {$IFnDEF TestArg}{$IFnDEF TestParam}
  pre__Instance2{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance2{e3};
  pre__Instance2b{e} _O2_ TClass1 _EQ_ (nil);  //@@ _pre3_Instance2b{e3};
  {$IFDEF TestAssign}
  pre__Instance2{e} := TClass1.Create;    //@@
  pre__Instance2b{e} := pre__Instance2{e};    //@@  // }}
  {$ENDIF}
  {$ENDIF}{$ENDIF}
  {$ENDIF}

  {$IFnDEF TestType}
  {$IFnDEF TestAssign}
  pre__Class1Rec{e}     _O2_ TRecordClass1    _EQ_ (Foo: nil);    //@@ _pre3_Class1Rec{e3};
  {$ELSE}
  pre__Class1Rec{e}.Foo  := pre__Instance1{e};  //@@ _pre3_Class1Rec{e3};  // }}}
  {$ENDIF}
  {$ENDIF}


  pre__SomeFunc1Ref{e}    _O2_ TFunc1                 _EQ_      (nil); //@@ _pre3_SomeFunc1Ref{e3};
  pre__SomeProc1Ref{e}    _O2_ TProc1                 _EQ_      (nil); //@@ _pre3_SomeProc1Ref{e3};
  pre__SomeMeth1Ref{e}    _O2_ TMeth1                 _EQ_      (nil); //@@ _pre3_SomeMeth1Ref{e3};
  {$IFDEF TestAssign}
  pre__SomeFunc1Ref{e}    := @SomeFunc1; //@@
  pre__SomeProc1Ref{e}    := @SomeProc1; //@@
  pre__SomeMeth1Ref{e}    := @MyClass2.SomeMeth1; //@@
  //pre__SomeMeth1Ref{e}    := @TMyBaseClass(nil).SomeMeth1; //@@
  {$ENDIF}

// short bool ....
// interface / object vs class / record
// array dyn/stat / nested of record/class/char/num ... bitpacked
// array [ enum ]
// type = array [ enum ]
// class of
// class/record helper
// type
//   b = 1..5;  c = set of b; // set of subrange


// pointer / deref for all above

// self in instance / class

  pre__IntfUnknown{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown{e3};
  {$IFnDEF TestType}
  pre__IntfUnknown1{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown1{e3};
  {$IFDEF TestAssign}
  pre__IntfUnknown1{e}    := InterfacedObject as IUnknown; //@@
  {$ENDIF}
  {$IFnDEF TestArg}{$IFnDEF TestParam}  // reduce count of param...
  pre__IntfUnknown2{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown2{e3};
  pre__IntfUnknown2b{e} _OP_ IUnknown ( nil );          //@@ _pre3_IntfUnknown2b{e3};
  {$IFDEF TestAssign}
  pre__IntfUnknown2{e}    := InterfacedObject2 as IUnknown; //@@
  pre__IntfUnknown2b{e}   := InterfacedObject2 as IUnknown; //@@
  {$ENDIF}
  {$ENDIF}{$ENDIF}
  {$ENDIF}

{$ENDIF}{$ENDIF} // parts

{$UNDEF _BLOCK_}
{$UNDEF _BLOCK2_}

  