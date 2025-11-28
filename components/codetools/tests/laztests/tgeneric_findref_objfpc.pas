program tgeneric_findref_objfpc;
{$Mode objfpc}

type

  { TBase }

  TBase{#TBase} = class
    FBase: integer;
    class procedure ProcBase1{#TBase_ProcBase1};
    procedure       ProcBase2{#TBase_ProcBase2};
    function        ProcBase3{#TBase_ProcBase3}: TBase;
  end;

  { TTest1 }

  TTest1{#TTest1} = class(TBase)
    FTest1: integer;
    class procedure Foo {#TTest1_Foo};
    procedure       Foo2{#TTest1_Foo2};
  end;

  { TTest2 }

  generic TTest2{TODO: #TTest2}<T2{#TTest2_T2}> = class
    FTest2: integer;
    class procedure Foo {#TTest2_Foo};
    procedure       Foo2{#TTest2_Foo2}(aArg: T2);
    function        Foo3{#TTest2_Foo3}(aArg: T2): T2;
  end;

  { TTest2a }

  generic TTest2a{TODO: #TTest2a}<T2{#TTest2a_T2}> = class(TBase)
    FTest2a: integer;
    class procedure Foo {#TTest2a_Foo};
    procedure       Foo2{#TTest2a_Foo2}(aArg: T2);
    function        Foo3{#TTest2a_Foo3}(aArg: T2): T2;
  end;

  { TTest3 }

  generic TTest3{TODO: #TTest3}<T2{TODO: #TTest3_T2}: TObject> = class
    FTest3: integer;
    class procedure Foo {#TTest3_Foo};
    procedure       Foo2{#TTest3_Foo2}(aArg: T2);
    function        Foo3{#TTest3_Foo3}(aArg: T2): T2;
  end;

  { TWrap1 }

  TWrap1{#TWrap1} = class
  public
    FBar: integer;
  public type

    { TTest4 }

    generic TTest4{TODO: #TTest4}<T2{TODO: #TTest4_T2}: TObject> = class
      FTest4: integer;
      class procedure Foo {#TTest4_Foo};
      procedure       Foo2{#TTest4_Foo2}(aArg: T2);
      function        Foo3{#TTest4_Foo3}(aArg: T2): T2;
    end;

  public
    FOuter: integer;
    class procedure         NoGen{#TWrap1_NoGen};
    generic class procedure Foo  {#TWrap1_Foo}<P>;
    generic class procedure Foo1 {#TWrap1_Foo1}<P>(a:P);
    generic procedure       Foo2 {#TWrap1_Foo2}<P>(aArg: P);
  end;

  generic TTest5{TODO: #TTest5}<T2{TODO: #TTest5_T2}: TObject> = class(TBase)
  public
    FBar: integer;
  public type
    TInner{#TInner} = class
      FTest5: integer;
      class procedure Foo {#TInner_Foo};
      procedure       Foo2{#TInner_Foo2}(aArg: T2);
      function        Foo3{#TInner_Foo3}(aArg: T2): T2;
    end;
  end;

var
  Bar: integer;

  gBase:   TBase{@TBase};
  gTest1:  TTest1{@TTest1};
  gTest2:  specialize TTest2{@TTest2}<TObject>;
  gTest2a: specialize TTest2a{@TTest2a}<TObject>;
  gTest3:  specialize TTest3{@TTest3}<TObject>;
  gWrap1:  TWrap1{@TWrap1};
  gTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  gTest5:  specialize TTest5{@TTest5}<TObject>;
  gInner:  specialize TTest5{@TTest5}<TObject>.TInner;

type
  gTBase   = TBase{@TBase};
  gTTest1  = TTest1{@TTest1};
  gTTest2  = specialize TTest2{@TTest2}<TObject>;
  gTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  gTTest3  = specialize TTest3{@TTest3}<TObject>;
  gTWrap1  = TWrap1{@TWrap1};
  gTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  gTTest5  = specialize TTest5{@TTest5}<TObject>;
  gTInner  = specialize TTest5{@TTest5}<TObject>.TInner;

{ TBase }

class procedure TBase{@TBase}.ProcBase1{@TBase_ProcBase1};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

procedure TBase{@TBase}.ProcBase2{@TBase_ProcBase2};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTest4 .Foo  {@TTest4_Foo}           ;
  lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
  lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

function TBase{@TBase}.ProcBase3{@TBase_ProcBase3}: TBase;
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
end;

{ TTest1 }

class procedure TTest1{@TTest1}.Foo{@TTest1_Foo};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

procedure TTest1{@TTest1}.Foo2{@TTest1_Foo2};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTest4 .Foo  {@TTest4_Foo}           ;
  lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
  lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

{ TTest2 }

class procedure TTest2{@TTest2}.Foo{@TTest2_Foo};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest2_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

procedure TTest2{@TTest2}.Foo2{@TTest2_Foo2}(aArg: T2);
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest2_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTest4 .Foo  {@TTest4_Foo}           ;
  lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
  lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

function TTest2{@TTest2}.Foo3{@TTest2_Foo3}(aArg: T2): T2;
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
end;

{ TTest2a }

class procedure TTest2a{@TTest2a}.Foo{@TTest2a_Foo};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest2a_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

procedure TTest2a{@TTest2a}.Foo2{@TTest2a_Foo2}(aArg: T2);
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest2a_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTest4 .Foo  {@TTest4_Foo}           ;
  lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
  lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

function TTest2a{@TTest2a}.Foo3{@TTest2a_Foo3}(aArg: T2): T2;
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
end;

{ TTest3 }

class procedure TTest3{@TTest3}.Foo{@TTest3_Foo};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest3_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

procedure TTest3{@TTest3}.Foo2{@TTest3_Foo2}(aArg: T2);
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest3_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTest4 .Foo  {@TTest4_Foo}           ;
  lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
  lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

function TTest3{@TTest3}.Foo3{@TTest3_Foo3}(aArg: T2): T2;
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
end;

{ TWrap1.TTest4 }

class procedure TWrap1{@TWrap1}.TTest4{@TTest4}.Foo{@TTest4_Foo};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  //lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  //lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest4_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

procedure TWrap1{@TWrap1}.TTest4{@TTest4}.Foo2{@TTest4_Foo2}(aArg: T2);
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  //lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  //lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest4_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  //lTest4 .Foo  {@TTest4_Foo}           ;
  //lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  //lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
//  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
//  lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
//  lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

function TWrap1{@TWrap1}.TTest4{@TTest4}.Foo3{@TTest4_Foo3}(aArg: T2): T2;
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  //lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  //lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
end;

{ TWrap1 }

class procedure TWrap1{@TWrap1}.NoGen{@TWrap1_NoGen};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  //lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  //lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

generic class procedure TWrap1{@TWrap1}.Foo{@TWrap1_Foo}<P>;
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  //lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  //lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

generic class procedure TWrap1{@TWrap1}.Foo1{@TWrap1_Foo1}<P>(a: P);
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  //lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  //lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  //lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

generic procedure TWrap1{@TWrap1}.Foo2{@TWrap1_Foo2}<P>(aArg: P);
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  //lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  //lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  //lTest4 .Foo  {@TTest4_Foo}           ;
  //lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  //lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  //lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  //lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
  //lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

{ TTest5.TInner }

class procedure TTest5{@TTest5}.TInner{@TInner}.Foo{@TInner_Foo};
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest5_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTest4 .Foo  {@TTest4_Foo}           ;
  lInner .Foo  {@TInner_Foo}           ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

procedure TTest5{@TTest5}.TInner{@TInner}.Foo2{@TInner_Foo2}(aArg: T2);
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
  n: T2{@TTest5_T2};
begin
  lBase  .ProcBase1{@TBase_ProcBase1}  ;
  lBase  .ProcBase2{@TBase_ProcBase2}  ;
  lTest1 .Foo  {@TTest1_Foo}           ;
  lTest1 .Foo2 {@TTest1_Foo2}          ;
  lTest2 .Foo  {@TTest2_Foo}           ;
  lTest2 .Foo2 {@TTest2_Foo2} (nil)    ;
  lTest2 .Foo3 {@TTest2_Foo3} (nil)    ;
  lTest2a.Foo  {@TTest2a_Foo}          ;
  lTest2a.Foo2 {@TTest2a_Foo2}(nil)    ;
  lTest2a.Foo3 {@TTest2a_Foo3}(nil)    ;
  lTest3 .Foo  {@TTest3_Foo}           ;
  lTest3 .Foo2 {@TTest3_Foo2} (nil)    ;
  lTest3 .Foo3 {@TTest3_Foo3} (nil)    ;
  lWrap1 .NoGen{@TWrap1_NoGen}         ;
  lWrap1 .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lWrap1 .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lWrap1 .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTest4 .Foo  {@TTest4_Foo}           ;
  lTest4 .Foo2 {@TTest4_Foo2} (nil)    ;
  lTest4 .Foo3 {@TTest4_Foo3} (nil)    ;
  lInner .Foo  {@TInner_Foo}           ;
  lInner .Foo2 {@TInner_Foo2} (nil)    ;
  lInner .Foo3 {@TInner_Foo3} (nil)    ;

  lTBase(nil)  .ProcBase1{@TBase_ProcBase1}  ;
  lTBase(nil)  .ProcBase2{@TBase_ProcBase2}  ;
  lTTest1(nil) .Foo  {@TTest1_Foo}           ;
  lTTest1(nil) .Foo2 {@TTest1_Foo2}          ;
  lTTest2(nil) .Foo  {@TTest2_Foo}           ;
  lTTest2(nil) .Foo2 {@TTest2_Foo2} (nil)    ;
  lTTest2(nil) .Foo3 {@TTest2_Foo3} (nil)    ;
  lTTest2a(nil).Foo  {@TTest2a_Foo}          ;
  lTTest2a(nil).Foo2 {@TTest2a_Foo2}(nil)    ;
  lTTest2a(nil).Foo3 {@TTest2a_Foo3}(nil)    ;
  lTTest3(nil) .Foo  {@TTest3_Foo}           ;
  lTTest3(nil) .Foo2 {@TTest3_Foo2} (nil)    ;
  lTTest3(nil) .Foo3 {@TTest3_Foo3} (nil)    ;
  lTWrap1(nil) .NoGen{@TWrap1_NoGen}         ;
  lTWrap1(nil) .specialize Foo  {@TWrap1_Foo} <byte>()  ;
  lTWrap1(nil) .specialize Foo1 {@TWrap1_Foo1}<byte> (0);
  lTWrap1(nil) .specialize Foo2 {@TWrap1_Foo2}<byte> (0);
  lTTest4(nil) .Foo  {@TTest4_Foo}           ;
  lTTest4(nil) .Foo2 {@TTest4_Foo2} (nil)    ;
  lTTest4(nil) .Foo3 {@TTest4_Foo3} (nil)    ;
  lTInner(nil) .Foo  {@TInner_Foo}           ;
  lTInner(nil) .Foo2 {@TInner_Foo2} (nil)    ;
  lTInner(nil) .Foo3 {@TInner_Foo3} (nil)    ;

  specialize TTest2 {@TTest2} <TObject>.Foo{@TTest2_Foo};
  specialize TTest2a{@TTest2a}<TObject>.Foo{@TTest2a_Foo};
  specialize TTest3 {@TTest3} <TObject>.Foo{@TTest3_Foo};
  TWrap1            {@TWrap1}.specialize Foo{@TWrap1_Foo}<byte>;
  TWrap1            {@TWrap1}.specialize TTest4{@TTest4}<TObject>.Foo{@TTest4_Foo};
  specialize TTest5 {@TTest5} <TObject>.TInner.Foo{@TInner_Foo};
end;

function TTest5{@TTest5}.TInner{@TInner}.Foo3{@TInner_Foo3}(aArg: T2): T2;
type
  lTBase   = TBase{@TBase};
  lTTest1  = TTest1{@TTest1};
  lTTest2  = specialize TTest2{@TTest2}<TObject>;
  lTTest2a = specialize TTest2a{@TTest2a}<TObject>;
  lTTest3  = specialize TTest3{@TTest3}<TObject>;
  lTWrap1  = TWrap1{@TWrap1};
  lTTest4  = TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTTest5  = specialize TTest5{@TTest5}<TObject>;
  lTInner  = specialize TTest5{@TTest5}<TObject>.TInner;
var
  lBase:   TBase{@TBase};
  lTest1:  TTest1{@TTest1};
  lTest2:  specialize TTest2{@TTest2}<TObject>;
  lTest2a: specialize TTest2a{@TTest2a}<TObject>;
  lTest3:  specialize TTest3{@TTest3}<TObject>;
  lWrap1:  TWrap1{@TWrap1};
  lTest4:  TWrap1{@TWrap1}.specialize TTest4{@TTest4}<TObject>;
  lTest5:  specialize TTest5{@TTest5}<TObject>;
  lInner:  specialize TTest5{@TTest5}<TObject>.TInner;
begin
end;

begin
end.

