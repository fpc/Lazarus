program ProgFoo;
{$mode objfpc}{$H+}

type

  TTestSetup1Class = class;

  { TTestSetup1Record }

  TTestSetup1Record = record
    FWord: Word;
    FBool: Boolean;
    FTest: TTestSetup1Class;
  end;
  PTestSetup1Record = ^TTestSetup1Record;

  { TTestSetup1Class }

  TTestSetup1Class = class
  public
    FWord: Word;
    FWordL: QWord;
    FInt: ShortInt;
    FIntL: Int64;
    FBool: Boolean;
    FTest: TTestSetup1Class;
    procedure f0(a:integer); virtual;
  end;
  PTestSetup1Class = ^TTestSetup1Class;

  TTestSetup1ClassChild = class(TTestSetup1Class)
    FInt64: Int64;
    FQWord: QWord;
  end;
  PTestSetup1ClassChild = ^TTestSetup1ClassChild;

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
    procedure f0(a:integer); virtual;
  end;
  PTestSetup1Object  = ^TTestSetup1Object;

  Pint = ^ integer;
  PPInt = ^Pint;
  PPPInt = ^PPint;
  PQWord = ^QWord;

var // Globals
  GlobTestSetup1Record: TTestSetup1Record;
  GlobTestSetup1RecordP: PTestSetup1Record;

  GlobTestSetup1Class: TTestSetup1Class;
  GlobTestSetup1ClassP: PTestSetup1Class;
  GlobTestSetup1ClassChild: TTestSetup1ClassChild;
  GlobTestSetup1ClassChildP: PTestSetup1ClassChild;
  GlobTestSetup1ClassClass: TTestSetup1ClassClass;
  GlobTestSetup1ClassChildClass: TTestSetup1ClassChildClass;

  GlobTestSetup1Object: TTestSetup1Object;
  GlobTestSetup1ObjectP: PTestSetup1Object;

  GlobTestSetup1Pointer: Pointer;
  GlobTestSetup1QWord: QWord;

function TestSetup1Bar(
  ParamTestSetup1Record: TTestSetup1Record;
  ParamTestRecord: PTestSetup1Record;

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


procedure TTestSetup1Class.f0(a: integer);
begin

end;

{ TTestSetup1Object }

procedure TTestSetup1Object.f0(a: integer);
begin

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
