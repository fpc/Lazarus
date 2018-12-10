// TEST_USES=WatchesScopeUnit1.pas,WatchesScopeUnit2.pas

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
end.
