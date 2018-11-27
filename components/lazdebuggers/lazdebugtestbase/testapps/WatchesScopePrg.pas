// TEST_USES=WatchesScopeUnit1.pas,WatchesScopeUnit2.pas
program WatchesPrg;
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


var
  BreakDummy: Integer;
  Int_GlobalPrg: Integer;
  Int_HideTest_Class: Integer;
  Int_HideTest_Unit: Integer;

  TestClassMainChild: TClassMainChild;

  Int_Hide_Foo: Integer;

{ TClassMain }

procedure TClassMain.MethodMain;

  procedure MethodMainNested;
  var
    IntMain: Integer;

    procedure MethodMainNestedTwice;
    var
      IntMain: integer;
    begin
    end;
  begin
  end;

begin
  MethodMainChild;  // call inherited class
  BreakDummy := 1; // TEST_BREAKPOINT=MethodMain
end;

procedure TClassMain.MethodMainChild;
begin
  //
end;


{ TClassMainChild }

procedure TClassMainChild.MethodMainChild;
var
  Int_MethodMainChild: Integer;

  procedure MethodMainChildNested;
  var
    Int_MethodMainChildNested: Integer;

    procedure MethodMainChildNestedTwice;
    var
      Int_MethodMainChildNestedTwice: integer;
    begin
      Int_MethodMainChildNestedTwice := 30;
      BreakDummy := 1; // TEST_BREAKPOINT=MethodMainChildNestedTwice
    end;

  begin
    Int_MethodMainChildNested := 40;
    MethodMainChildNestedTwice;
    BreakDummy := 1; // TEST_BREAKPOINT=MethodMainChildNested
  end;

var
  Int_MethodMainChild_Late: integer;
begin
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


begin
  Unit1Init;
  Unit2Init;
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
end.
