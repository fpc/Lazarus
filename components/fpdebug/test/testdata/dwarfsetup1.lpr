program ProgFoo;
{$mode objfpc}{$H+}
{$IF FPC_FULLVERSION>=20701}
  {$OPTIMIZATION NOREMOVEEMPTYPROCS}
  {$OPTIMIZATION NOORDERFIELDS}
{$ENDIF}
{$OPTIMIZATION OFF}
{$A2}

type

  TTestSetup1Class = class;

  { TTestSetup1Record }

  TTestSetup1Record = record
    FWord: Word;
    FBool: Boolean;
    FTest: TTestSetup1Class;
  end;
  PTestSetup1Record = ^TTestSetup1Record;

  TTestSetup1Record2 = record // same size
    FWord: Word;
    FBool: Boolean;
    FTest: TTestSetup1Class;
  end;
  PTestSetup1Record2 = ^TTestSetup1Record2;

  TTestSetup1Record3 = record // other size
    FWord: Word;
  end;
  PTestSetup1Record3 = ^TTestSetup1Record3;

  { TTestSetup1Class }

  TTestSetup1Class = class
  public
    FBool: Boolean;
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FTest: TTestSetup1Class;
    FByte: Byte;
    procedure ClassProc0(a:integer); virtual;
  end;
  PTestSetup1Class = ^TTestSetup1Class;

  TTestSetup1ClassChild = class(TTestSetup1Class)
    FInt64: Int64;
    FQWord: QWord;
  end;
  PTestSetup1ClassChild = ^TTestSetup1ClassChild;

  TTestSetup1Class2 = class
  public
    FInt: Integer;
    FWord: Word;
  end;
  PTestSetup1Class2 = ^TTestSetup1Class2;

  TTestSetup1ClassClass = class of TTestSetup1Class;
  TTestSetup1ClassChildClass = class of TTestSetup1ClassChild;

  { TTestSetup1Object }

  TTestSetup1Object = object
  public
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FBool: Boolean;
    FBool2: LongBool;
    FBool3: ByteBool;
    FTest: TTestSetup1Class;
    procedure ObjProc0(o1:integer); virtual;
    //only with a wirtual method, will there be a vptr entry
  end;
  PTestSetup1Object  = ^TTestSetup1Object;

  { TTestSetup1Object2 }

  TTestSetup1Object2 = object // same size
  public
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FBool: Boolean;
    FBool2: LongBool;
    FBool3: ByteBool;
    FTest: TTestSetup1Class;
    procedure ObjProc1(o2:integer); virtual;
  end;
  PTestSetup1Object2  = ^TTestSetup1Object2;

  { TTestSetup1Object3 }

  TTestSetup1Object3 = object // diff size
  public
    FWord: Word;
    procedure ObjProc1(o2:integer); virtual;
  end;
  PTestSetup1Object3  = ^TTestSetup1Object3;

  TTestSetup1Object4 = object // looks like a record....
  public
    FWord: Word;
  end;
  PTestSetup1Object4  = ^TTestSetup1Object4;

  Pint = ^ integer;
  PPInt = ^Pint;
  PPPInt = ^PPint;
  PQWord = ^QWord;

var // Globals
  GlobTestSetup1Record: TTestSetup1Record;
  GlobTestSetup1RecordP: PTestSetup1Record;
  GlobTestSetup1Record2: TTestSetup1Record2;
  GlobTestSetup1Record2P: PTestSetup1Record2;
  GlobTestSetup1Record3: TTestSetup1Record3;
  GlobTestSetup1Record3P: PTestSetup1Record3;

  GlobTestSetup1Class: TTestSetup1Class;
  GlobTestSetup1ClassP: PTestSetup1Class;
  GlobTestSetup1ClassChild: TTestSetup1ClassChild;
  GlobTestSetup1ClassChildP: PTestSetup1ClassChild;
  GlobTestSetup1Class2: TTestSetup1Class2;
  GlobTestSetup1Class2P: PTestSetup1Class2;
  GlobTestSetup1ClassClass: TTestSetup1ClassClass;
  GlobTestSetup1ClassChildClass: TTestSetup1ClassChildClass;

  GlobTestSetup1Object: TTestSetup1Object;
  GlobTestSetup1ObjectP: PTestSetup1Object;
  GlobTestSetup1Object2: TTestSetup1Object2;
  GlobTestSetup1Object2P: PTestSetup1Object2;
  GlobTestSetup1Object3: TTestSetup1Object3;
  GlobTestSetup1Object3P: PTestSetup1Object3;
  GlobTestSetup1Object4: TTestSetup1Object4;
  GlobTestSetup1Object4P: PTestSetup1Object4;

  GlobTestSetup1Pointer: Pointer;
  GlobTestSetup1QWord: QWord;

function TestSetup1Bar(
  ParamTestSetup1Record: TTestSetup1Record;
  ParamTestSetup1RecordP: PTestSetup1Record;

  ParamTestSetup1Class: TTestSetup1Class;
  ParamTestSetup1ClassP: PTestSetup1Class;
  ParamTestSetup1ClassChild: TTestSetup1ClassChild;
  ParamTestSetup1ClassChildP: PTestSetup1ClassChild;
  ParamTestSetup1ClassClass: TTestSetup1ClassClass;
  ParamTestSetup1ClassChildClass: TTestSetup1ClassChildClass;

  ParamTestSetup1Object: TTestSetup1Object;
  ParamTestSetup1ObjectP: PTestSetup1Object;

  var VParamTestSetup1Record: TTestSetup1Record;
  var VParamTestRecord: PTestSetup1Record;

  var VParamTestSetup1Class: TTestSetup1Class;
  var VParamTestSetup1ClassP: PTestSetup1Class;
  var VParamTestSetup1ClassChild: TTestSetup1ClassChild;
  var VParamTestSetup1ClassChildP: PTestSetup1ClassChild;
  var VParamTestSetup1ClassClass: TTestSetup1ClassClass;
  var VParamTestSetup1ClassChildClass: TTestSetup1ClassChildClass;

  var VParamTestSetup1Object: TTestSetup1Object;
  var VParamTestSetup1ObjectP: PTestSetup1Object
): Pointer;
var
  int1: Integer;
  pint1: ^Integer;
  bool1: Boolean;

  Obj1: TTestSetup1Class;
  PObj1: ^TTestSetup1Class;
  OldObj1: TTestSetup1Object;
  POldObj1: PTestSetup1Object;
  Rec1: TTestSetup1Record;
  PRec1: ^TTestSetup1Record;
  Rec2: record    FWord: Word;    FBool: Boolean;  end;

  pi: Pint;
  ppi: PPint;
  pppi: PPPint;

  subr: 1..9;
  subr2: -11..-9;
  subr3: #9..'m';

begin
  int1 := 1;
  pint1 := @Int1;
  bool1 := True;
  Obj1 := nil;
  PObj1 := @Obj1;
  POldObj1 := @OldObj1;
  PRec1 := @Rec1;
  Rec2.FBool := True;
  pi := @Int1;
  ppi := @pi;
  pppi := @ppi;
  subr := 1;
  subr2 := -11;
  subr3 := #9;
end;

{ TTestSetup1Object3 }

procedure TTestSetup1Object3.ObjProc1(o2: integer);
begin
  FWord := 0;//
end;

{ TTestSetup1Object2 }

procedure TTestSetup1Object2.ObjProc1(o2: integer);
begin
  FWord := 0;
end;


procedure TTestSetup1Class.ClassProc0(a: integer);
begin
  FWord := 0;
end;

{ TTestSetup1Object }

procedure TTestSetup1Object.ObjProc0(o1: integer);
begin
  FWord := 0;
end;

begin
  TestSetup1Bar(
  GlobTestSetup1Record,  GlobTestSetup1RecordP,
  GlobTestSetup1Class,  GlobTestSetup1ClassP,  GlobTestSetup1ClassChild,
  GlobTestSetup1ClassChildP,  GlobTestSetup1ClassClass,  GlobTestSetup1ClassChildClass,
  GlobTestSetup1Object,  GlobTestSetup1ObjectP,
  GlobTestSetup1Record,  GlobTestSetup1RecordP,
  GlobTestSetup1Class,  GlobTestSetup1ClassP,  GlobTestSetup1ClassChild,
  GlobTestSetup1ClassChildP,  GlobTestSetup1ClassClass,  GlobTestSetup1ClassChildClass,
  GlobTestSetup1Object,  GlobTestSetup1ObjectP
  );
end.
