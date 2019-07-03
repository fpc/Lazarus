// TEST_USES=WatchesValuePrgIdent.inc

(*
  - Declare variables of every type available, including nested types, named/unnamed, ...
  - Declare in different locations: const, var, param, var param, field, ...

  Test that the debugger can read any type of value at any location
*)
program WatchesValuePrg;
{$LONGSTRINGS ON}

uses sysutils, Classes;

function SomeFunc1(SomeValue, Foo: Integer; Bar: Word; X: Byte): Boolean;
begin result := SomeValue = 0; end;
procedure SomeProc1();
begin SomeFunc1(2,2,2,2); end;

type
{$ifdef CPU64}
  PtrUInt = type QWord;
{$endif CPU64}

{$ifdef CPU32}
  PtrUInt = type DWord;
{$endif CPU32}

var
  BreakDummy: PtrUInt;
  p: Pointer;

type
  TIntRange = -300..300;
  TCardinalRange = 1..300;

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

  TShortRec = record // looks like shortstring
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

  TClass1 = class
  public
    FInt: integer;
    FDynInt: TIntDynArray;
    FAnsi: AnsiString;
  end;

  TEnum  = (EnVal1, EnVal2, EnVal3, EnVal4);
  TEnumSub =  EnVal1..EnVal2;
  TEnum2 = (EnVal21= 3, EnVal22=4, EnVal23=7, EnVal24=10, EnVal25=30);
  TSet   = set of TEnum;

  TArrayEnum = array [TEnum] of word;
  TArrayEnumSub = array [TEnumSub] of word;
  TArrayEnumElem = array [EnVal1..EnVal4] of word;
  TArrayEnumSubElem = array [EnVal1..EnVal2] of word;

  TFunc1 = function(SomeValue, Foo: Integer; Bar: Word; X: Byte): Boolean;
  TProc1 = procedure();
  TMeth1 = function(AVal: Integer): Boolean of object;

type
  (* LOCATION: TYPE *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=Tx, "_OP_== type ", (=;//, "_O2_= = type", _EQ_=, _BLOCK_=TestVar )
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=Px, _OP_={, _O2_={, _pre3_=^Tx, "//@@=} = ", _BLOCK_=TestVar) //}

  (* LOCATION: field in baseclass *)
  TMyBaseClass = class
  public
    function SomeMeth1(SomeValue: Integer): Boolean;
  public
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=mbc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )
  end;

  (* LOCATION: field in class *)
  TMyClass = class(TMyBaseClass)
  public
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=mc, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )
  end;

var
  MyClass1: TMyClass;
  MyClass2: TMyBaseClass; (* LOCATION: field, requires typecast of containing class *)

const
(* LOCATION: global const *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gc, ADD=0, CHR1='A', _OP_==, _O2_=:, _EQ_==,"(nil)=nil", _BLOCK_=TestConst)

var
(* LOCATION: global var *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, _OP_=:, (=;//, _O2_=:, _EQ_=, _BLOCK_=TestVar )

(* LOCATION: global var  ARRAY OF <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gva, "_OP_=: array of", (=;//, "_O2_=: array of", _EQ_=, _BLOCK_=TestVar )

(* LOCATION: global var  pointer <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gvp_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )

function TMyBaseClass.SomeMeth1(SomeValue: Integer): Boolean;
begin result := SomeValue = 0; end;


procedure Foo(
(* LOCATION: param *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=arg, _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
  ArgMyClass1: TMyClass;
  ArgMyClass2: TMyBaseClass;
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
begin  // TEST_BREAKPOINT=FooBegin
  BreakDummy:= 1;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

(* INIT: local var  pointer <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pl_, _OP_={, _O2_={, _pre3_=@fooloc, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pa_, _OP_={, _O2_={, _pre3_=@arg, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}

  BreakDummy:= 1; // TEST_BREAKPOINT=Foo
end;


procedure FooVar(
(* LOCATION: var param *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=var argvar", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
  ArgVarMyClass1: TMyClass;
  ArgVarMyClass2: TMyBaseClass;
  Dummy: Integer
);
var
(* LOCATION: var params  pointer <each type>  FOR args *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=fooloc_pv_, "_OP_=: ^", (=;//, "_O2_=: ^", _EQ_=, _BLOCK_=TestVar, _BLOCK2_=TestPointer )
begin // TEST_BREAKPOINT=FooVarBegin
(* INIT: local var  pointer <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=fooloc_pv_, _OP_={, _O2_={, _pre3_=@argvar, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}

  BreakDummy:= 1;
  BreakDummy:= 1; // TEST_BREAKPOINT=FooVar
end;


procedure FooConstRef(
(* LOCATION: constref param *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc, "pre__=constref argconstref", _OP_=:, (=;//, _O2_=:, _EQ_= , _BLOCK_=TestArg)
  ArgConstRefMyClass1: TMyClass;
  ArgConstRefMyClass2: TMyBaseClass;
  Dummy: Integer
);
var
  xxx, xx2: ansistring; // enforce a stackframe
begin // TEST_BREAKPOINT=FooConstRefBegin
  BreakDummy:= 1;
  xxx := '1';
  BreakDummy:= 1; // TEST_BREAKPOINT=FooConstRef
end;

begin
  // access constant that are not passed as function arg
  // so every constant is accessed, and they can not be optimized away
  BreakDummy := ord(gcCharStatArray[1]);
  BreakDummy := ord(gcWCharStatArray[1]);
  p := nil;
  SomeFunc1(1,1,1,1);
  SomeProc1();

(* use global const / value in "gv" will be overriden... *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv, {e}={, "//@@=} :=", _pre3_=gc, _BLOCK_=TestAssignGC)

(* INIT: global var *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gv, ADD=1, CHR1='B', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

(* INIT: field in class / baseclass *)
  MyClass1 := TMyClass.Create;
  MyClass1.SomeMeth1(1);
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyClass1.mbc, ADD=3, CHR1='D', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=MyClass1.mc, ADD=2, CHR1='C', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

(* INIT: field in class / baseclass // typecast *)
  MyClass2 := TMyClass.Create;
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,"pre__=TMyClass(MyClass2).mbc", ADD=5, CHR1='F', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,"pre__=TMyClass(MyClass2).mc", ADD=4, CHR1='E', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, _BLOCK_=TestAssign)

(* INIT: global var  ARRAY OF <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,"pre__=SetLength(gva", "_OP_=,2);//", "_O2_=,2);//", _BLOCK_=TestSetLen)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gva, ADD=5, CHR1='K', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[0]", _BLOCK_=TestAssign)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gva, ADD=6, CHR1='L', _OP_=:=, _O2_={, _EQ_=}:=, _pre2_=gc, "{e}=[1]", _BLOCK_=TestAssign)
(* INIT: global var  pointer <each type> *)
  TEST_PREPOCESS(WatchesValuePrgIdent.inc,pre__=gvp_, _OP_={, _O2_={, _pre3_=@gv, "//@@=} :=", _BLOCK_=TestVar, _BLOCK2_=TestPointer) //}


  BreakDummy:= 1; // TEST_BREAKPOINT=Prg

  Foo(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    0
  );

  FooVar(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    0
  );

  FooConstRef(
    TEST_PREPOCESS(WatchesValuePrgIdent.inc, pre__=gv, "_OP_=,//", "_O2_=,//", _BLOCK_=TestParam)
    MyClass1,
    MyClass2,
    0
  );

end.

