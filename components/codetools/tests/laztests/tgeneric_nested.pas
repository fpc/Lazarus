program tgeneric_nested;

{$mode objfpc}{$H+}{$notes off}

uses
  Classes;

type

  TRec0 = record r0a: integer; r0glob: integer; end;
  TRec1 = record r1a: integer; r1glob: integer; end;
  TRec2 = record r2a: integer; r2glob: integer; end;

  { TClass1 }

  TClass1 = class
  public type
    TRec2 = record r2a: integer; r2c1: integer; end;
    TRec3 = record r3a: integer; r3c1: integer; end; // Only in TBase
  end;

  { TClass2 }

  TClass2 = class(TClass1)
  public type
    TRec1 = record r1a: integer; r1c2: integer; end;
  end;

  { GEN_Field }

  generic GEN_Field<_F1> = class
  public type
    T_F1 = _F1;
  public
    FField1: _F1;
  end;

  (* Generics *)

  { GEN_1 }

  generic GEN_1<_REC> = class
  public type
    TFld0 = class(specialize GEN_Field<_REC>)
      fx: _REC {declaration:GEN_1._REC};
      fy: T_F1 {declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFld1 = class(specialize GEN_Field<TRec1>)
      fx: _REC { TODO,WRONG: declaration:GEN_1._REC};
      fy: T_F1 {declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFld2 = class(specialize GEN_Field<TRec2>)  // Global TRec2
      fx: _REC { TODO,WRONG: declaration:GEN_1._REC};
      fy: T_F1 {declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
  public class var
    fv: _REC {declaration:GEN_1._REC};
  public
    fx: _REC {declaration:GEN_1._REC};
    procedure Bar;
  end;

  (* Generics with Base *)

  { GENB_1 }

  generic GENB_1<_B1: TObject; _REC> = class(_B1)
  public type
    TFldB0 = class(specialize GEN_Field<_REC>)
      fx: _REC { TODO: declaration:GENB_1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldB1 = class(specialize GEN_Field<TRec1>)  // Global (can be specialized to TClass2.TRec1)
      fx: _REC { TODO: declaration:GENB_1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldB2 = class(specialize GEN_Field<TRec2>)  // Global TRec2 (can be specialized to TClass1.TRec2)
      fx: _REC { TODO: declaration:GENB_1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    //TFldB3 = class(specialize GEN_Field<TRec3>)  // not yet known
    //  procedure Foo;
    //end;
  public class var
    fv: _REC {declaration:GENB_1._REC};
  public
    fx: _REC {declaration:GENB_1._REC};
    procedure Bar;
  end;

  { GENB_2 }

  generic GENB_2<_B2: TClass1; _REC> = class(_B2)
  public type
    TFldB0 = class(specialize GEN_Field<_REC>)
      fx: _REC { TODO: declaration:GENB_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldB1 = class(specialize GEN_Field<TRec1>)  // Global (can be specialized to TClass2.TRec1)
      fx: _REC { TODO: declaration:GENB_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldB2 = class(specialize GEN_Field<TRec2>)  // TClass1.TRec2
      fx: _REC { TODO: declaration:GENB_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldB3 = class(specialize GEN_Field<TRec3>)  // TClass1.TRec3
      fx: _REC { TODO: declaration:GENB_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
  public class var
    fv: _REC {declaration:GENB_2._REC};
  public
    fx: _REC {declaration:GENB_2._REC};
    procedure Bar;
  end;

  (* Inherhited Generics *)

  { GENI_1 }

  generic GENI_1<_IB1: TObject; _REC> = class(specialize GENB_1<_IB1, _REC>)
  public type
    TFldI0 = class(specialize GEN_Field<_REC>)
      fx: _REC { TODO: declaration:GENI_1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI1 = class(specialize GEN_Field<TRec1>)  // Global (can be specialized to TClass2.TRec1)
      fx: _REC { TODO: declaration:GENI_1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI2 = class(specialize GEN_Field<TRec2>)  // Global TRec2
      fx: _REC { TODO: declaration:GENI_1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    //TFldX3 = class(specialize GEN_Field<TRec3>)  // not yet known
    //  procedure Foo;
    //end;
  public class var
    fIv: _REC {declaration:GENI_1._REC};
  public
    fIx: _REC {declaration:GENI_1._REC};
    procedure Bar;
  end;

  { GENI_2B1 }

  generic GENI_2B1<_IB2: TClass1; _REC> = class(specialize GENB_1<_IB2, _REC>)
  public type
    TFldI0 = class(specialize GEN_Field<_REC>)
      fx: _REC { TODO: declaration:GENI_2B1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI1 = class(specialize GEN_Field<TRec1>)  // Global (can be specialized to TClass2.TRec1)
      fx: _REC { TODO: declaration:GENI_2B1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI2 = class(specialize GEN_Field<TRec2>)  // TClass1.TRec2
      fx: _REC { TODO: declaration:GENI_2B1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI3 = class(specialize GEN_Field<TRec3>)  // TClass1.TRec3
      fx: _REC { TODO: declaration:GENI_2B1._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
  public class var
    fIv: _REC {declaration:GENI_2B1._REC};
  public
    fIx: _REC {declaration:GENI_2B1._REC};
    procedure Bar;
  end;

  { GENI_2 }

  generic GENI_2<_IB2: TClass1; _REC> = class(specialize GENB_2<_IB2, _REC>)
  public type
    TFldI0 = class(specialize GEN_Field<_REC>)
      fx: _REC { TODO: declaration:GENI_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI1 = class(specialize GEN_Field<TRec1>)  // Global (can be specialized to TClass2.TRec1)
      fx: _REC { TODO: declaration:GENI_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI2 = class(specialize GEN_Field<TRec2>)  // TClass1.TRec2
      fx: _REC { TODO: declaration:GENI_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
    TFldI3 = class(specialize GEN_Field<TRec3>)  // TClass1.TRec3
      fx: _REC { TODO: declaration:GENI_2._REC};
      fy: T_F1 { TODO: declaration:GEN_Field.T_F1};
      procedure Foo;
    end;
  public class var
    fIv: _REC {declaration:GENI_2._REC};
  public
    fIx: _REC {declaration:GENI_2._REC};
    procedure Bar;
  end;

  (* specialized classes *)

  (* Gen_1 *)

  TClassG1R0 = class(specialize Gen_1<TRec0>)
    procedure Abc;
  end;

  TClassG1R1 = class(specialize Gen_1<TRec1>)
    procedure Abc;
  end;

  TClassG1Rb1 = class(specialize Gen_1<TClass2.TRec1>)
    procedure Abc;
  end;

  (* GenB_1 *)

  TClassB1 = class(specialize GenB_1<TObject, TRec0>)
    procedure Abc;
  end;

  TClassB1C1 = class(specialize GenB_1<TClass1, TRec0>)
    procedure Abc;
  end;

  TClassB1C2 = class(specialize GenB_1<TClass2, TRec0>)
    procedure Abc;
  end;

  (* GenB_2 *)

  TClassB2C1 = class(specialize GenB_2<TClass1, TRec0>)
    procedure Abc;
  end;

  TClassB2C2 = class(specialize GenB_2<TClass2, TRec0>)
    procedure Abc;
  end;

  (* GenI_1 *)

  TClassI1 = class(specialize GenI_1<TObject, TRec0>)
    procedure Abc;
  end;

  TClassI1C1 = class(specialize GenI_1<TClass1, TRec0>)
    procedure Abc;
  end;

  TClassI1C2 = class(specialize GenI_1<TClass2, TRec0>)
    procedure Abc;
  end;

  (* GenI_2B1 *)

  TClassI2bC1 = class(specialize GenI_2B1<TClass1, TRec0>)
    procedure Abc;
  end;

  TClassI2bC2 = class(specialize GenI_2B1<TClass2, TRec0>)
    procedure Abc;
  end;

  (* GenI_2 *)

  TClassI2C1 = class(specialize GenI_2<TClass1, TRec0>)
    procedure Abc;
  end;

  TClassI2C2 = class(specialize GenI_2<TClass2, TRec0>)
    procedure Abc;
  end;

  (* directly specialized classes *)

  (* Gen_1 *)
  TXClassG1R0 = specialize Gen_1<TRec0>;
  TXClassG1R1 = specialize Gen_1<TRec1>;
  TXClassG1Rb1 = specialize Gen_1<TClass2.TRec1>;

  (* GenB_1 *)
  TXClassB1 = specialize GenB_1<TObject, TRec0>;
  TXClassB1C1 = specialize GenB_1<TClass1, TRec0>;
  TXClassB1C2 = specialize GenB_1<TClass2, TRec0>;

  (* GenB_2 *)
  TXClassB2C1 = specialize GenB_2<TClass1, TRec0>;
  TXClassB2C2 = specialize GenB_2<TClass2, TRec0>;

  (* GenI_1 *)
  TXClassI1 = specialize GenI_1<TObject, TRec0>;
  TXClassI1C1 = specialize GenI_1<TClass1, TRec0>;
  TXClassI1C2 = specialize GenI_1<TClass2, TRec0>;

  (* GenI_2B1 *)
  TXClassI2bC1 = specialize GenI_2B1<TClass1, TRec0>;
  TXClassI2bC2 = specialize GenI_2B1<TClass2, TRec0>;

  (* GenI_2 *)
  TXClassI2C1 = specialize GenI_2<TClass1, TRec0>;
  TXClassI2C2 = specialize GenI_2<TClass2, TRec0>;


  ////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////

{ GEN_1.TFld0 }

procedure GEN_1.TFld0.Foo;
var
  a: _REC {declaration:GEN_1._REC};
  a0: TFld0 {declaration:GEN_1.TFld0};
  a1: TFld1 {declaration:GEN_1.TFld1};
  a2: TFld2 {declaration:GEN_1.TFld2};
begin
  fv{declaration:GEN_1.fv} := default(_REC { TODO: declaration:GEN_1._REC} );

  fy{declaration:GEN_1.TFld0.fy} := default(_REC { TODO: declaration:GEN_1._REC} );
  FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GEN_1._REC} );

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GEN_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1} := default(TRec1 {declaration:TRec1} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GEN_1.TFld1 }

procedure GEN_1.TFld1.Foo;
var
  a: _REC { TODO,WRONG: declaration:GEN_1._REC}; // TODO: the test finds it and yet we can not jump there
  a0: TFld0 {declaration:GEN_1.TFld0};
  a1: TFld1 {declaration:GEN_1.TFld1};
  a2: TFld2 {declaration:GEN_1.TFld2};
begin
  fv{declaration:GEN_1.fv} := default(_REC { TODO: declaration:GEN_1._REC} );

  fy{declaration:GEN_1.TFld1.fy}.r1a{ TODO: declaration:TRec1.r1a} := 1;
  FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GEN_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GEN_1.TFld2 }

procedure GEN_1.TFld2.Foo;
var
  a: _REC { TODO,WRONG declaration:GEN_1._REC}; // TODO: the test finds something and yet we can not jump there
  a0: TFld0 {declaration:GEN_1.TFld0};
  a1: TFld1 {declaration:GEN_1.TFld1};
  a2: TFld2 {declaration:GEN_1.TFld2};
begin
  fv{declaration:GEN_1.fv} := default(_REC { TODO: declaration:GEN_1._REC} );

  fy{declaration:GEN_1.TFld2.fy}.r2a{ TODO: declaration:TRec2.r2a} := 1;
  FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GEN_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GEN_1 }

procedure GEN_1.Bar;
var
  a: _REC { TODO: declaration:GEN_1._REC}; // TODO: cache??
  a0: TFld0 {declaration:GEN_1.TFld0};
  a1: TFld1 {declaration:GEN_1.TFld1};
  a2: TFld2 {declaration:GEN_1.TFld2};
begin
  fv{declaration:GEN_1.fv} := default(_REC { TODO: declaration:GEN_1._REC} );
  fx{declaration:GEN_1.fx} := default(_REC { TODO: declaration:GEN_1._REC} );

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GEN_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENB_1.TFldB0 }

procedure GENB_1.TFldB0.Foo;
var
  a: _REC { TODO declaration:GENB_1._REC};
  a0: TFldB0 { TODO declaration:GENB_1.TFldB0};
  a1: TFldB1 { TODO declaration:GENB_1.TFldB1};
  a2: TFldB2 { TODO declaration:GENB_1.TFldB2};
begin
  fv{ TODO declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENB_1._REC} );

  fy{ TODO declaration:GENB_1.TFldB0.fy} := default(_REC { TODO: declaration:GENB_1._REC} );
  FField1{ TODO declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_1._REC} );

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENB_1.TFldB1 }

procedure GENB_1.TFldB1.Foo;
var
  a: _REC { TODO,WRONG: declaration:GENB_1._REC}; // TODO: the test finds it and yet we can not jump there
  a0: TFldB0 { TODO declaration:GENB_1.TFldB0};
  a1: TFldB1 { TODO declaration:GENB_1.TFldB1};
  a2: TFldB2 { TODO declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENB_1._REC} );

  fy{declaration:GENB_1.TFldB1.fy}.r1a{ TODO: declaration:TRec1.r1a} := 1;
  FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENB_1.TFldB2 }

procedure GENB_1.TFldB2.Foo;
var
  a: _REC { TODO,WRONG declaration:GENB_1._REC}; // TODO: the test finds something and yet we can not jump there
  a0: TFldB0 { TODO declaration:GENB_1.TFldB0};
  a1: TFldB1 { TODO declaration:GENB_1.TFldB1};
  a2: TFldB2 { TODO declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENB_1._REC} );

  fy{declaration:GENB_1.TFldB2.fy}.r2a{ TODO: declaration:TRec2.r2a} := 1;
  FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENB_1 }

procedure GENB_1.Bar;
var
  a: _REC {declaration:GENB_1._REC};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENB_1._REC} );
  fx{declaration:GENB_1.fx} := default(_REC { TODO: declaration:GENB_1._REC} );

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENB_2.TFldB0 }

procedure GENB_2.TFldB0.Foo;
var
  a: _REC { TODO declaration:GENB_2._REC};
  a0: TFldB0 { TODO declaration:GENB_2.TFldB0};
  a1: TFldB1 { TODO declaration:GENB_2.TFldB1};
  a2: TFldB2 { TODO declaration:GENB_2.TFldB2};
begin
  fv{ TODO declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENB_2._REC} );

  fy{ TODO declaration:GENB_2.TFldB0.fy} := default(_REC { TODO: declaration:GENB_1._REC} );
  FField1{ TODO declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_2._REC} );

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENB_2.TFldB1 }

procedure GENB_2.TFldB1.Foo;
var
  a: _REC { TODO,WRONG: declaration:GENB_2._REC}; // TODO: the test finds it and yet we can not jump there
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENB_2._REC} );

  fy{declaration:GENB_2.TFldB1.fy}.r1a{ TODO: declaration:TRec1.r1a} := 1;
  FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENB_2.TFldB2 }

procedure GENB_2.TFldB2.Foo;
var
  a: _REC { TODO,WRONG declaration:GENB_2._REC}; // TODO: the test finds something and yet we can not jump there
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENB_2._REC} );

  fy{declaration:GENB_2.TFldB2.fy}.r2a{ TODO: declaration:TClass1.TRec2.r2a} := 1;
  FField1{declaration:GEN_Field.FField1}.r2a{declaration:TClass1.TRec2.r2a} := 1;

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENB_2.TFldB3 }

procedure GENB_2.TFldB3.Foo;
var
  a: _REC { TODO,WRONG declaration:GENB_2._REC}; // TODO: the test finds something and yet we can not jump there
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENB_2._REC} );

  fy{declaration:GENB_2.TFldB3.fy}.r3a{ TODO: declaration:TClass1.TRec3.r3a} := 1;
  FField1{declaration:GEN_Field.FField1}.r3a{declaration:TClass1.TRec3.r3a} := 1;

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENB_2 }

procedure GENB_2.Bar;
var
  a: _REC {declaration:GENB_2._REC};
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENB_2._REC} );
  fx{declaration:GENB_2.fx} := default(_REC { TODO: declaration:GENB_2._REC} );

  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENB_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_1.TFldI0 }

procedure GENI_1.TFldI0.Foo;
var
  a: _REC { TODO declaration:GENI_1._REC};
  b0: TFldI0 { TODO declaration:GENI_1.TFldI0};
  b1: TFldI1 { TODO declaration:GENI_1.TFldI1};
  b2: TFldI2 { TODO declaration:GENI_1.TFldI2};
  a0: TFldB0 { TODO declaration:GENB_1.TFldB0};
  a1: TFldB1 { TODO declaration:GENB_1.TFldB1};
  a2: TFldB2 { TODO declaration:GENB_1.TFldB2};
begin
  fv{ TODO declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );
  fIv{ TODO declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );

  fy{ TODO declaration:GENB_1.TFldB0.fy} := default(_REC { TODO: declaration:GENI_1._REC} );
  FField1{ TODO declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENI_1.TFldI1 }

procedure GENI_1.TFldI1.Foo;
var
  a: _REC { TODO,WRONG: declaration:GENI_1._REC}; // TODO: the test finds it and yet we can not jump there
  b0: TFldI0 {declaration:GENI_1.TFldI0};
  b1: TFldI1 {declaration:GENI_1.TFldI1};
  b2: TFldI2 {declaration:GENI_1.TFldI2};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );
  fIv{ TODO: declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );

  fy{ TODO: declaration:GENB_1.TFldB1.fy}.r1a{ TODO: declaration:TRec1.r1a} := 1;
  FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENI_1.TFldI2 }

procedure GENI_1.TFldI2.Foo;
var
  a: _REC { TODO,WRONG declaration:GENI_1._REC}; // TODO: the test finds something and yet we can not jump there
  b0: TFldI0 {declaration:GENI_1.TFldI0};
  b1: TFldI1 {declaration:GENI_1.TFldI1};
  b2: TFldI2 {declaration:GENI_1.TFldI2};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );
  fIv{ TODO: declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );

  fy{ TODO declaration:GENB_1.TFldB2.fy}.r2a{ TODO: declaration:TRec2.r2a} := 1;
  FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENI_1 }

procedure GENI_1.Bar;
var
  a: _REC {declaration:GENI_1._REC};
  b0: TFldI0 {declaration:GENI_1.TFldI0};
  b1: TFldI1 {declaration:GENI_1.TFldI1};
  b2: TFldI2 {declaration:GENI_1.TFldI2};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );
  fIv{ TODO declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_1._REC} );
  fx{declaration:GENB_1.fx} := default(_REC { TODO: declaration:GENI_1._REC} );
  fIx{ TODO declaration:GENB_1.fx} := default(_REC { TODO: declaration:GENI_1._REC} );

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{declaration:TRec2.r2a} := 1;
end;

{ GENI_2B1.TFldI0 }

procedure GENI_2B1.TFldI0.Foo;
var
  a: _REC { TODO declaration:GENI_2B1._REC};
  b0: TFldI0 { TODO declaration:GENI_2B1.TFldI0};
  b1: TFldI1 { TODO declaration:GENI_2B1.TFldI1};
  b2: TFldI2 { TODO declaration:GENI_2B1.TFldI2};
  a0: TFldB0 { TODO declaration:GENB_2.TFldB0};
  a1: TFldB1 { TODO declaration:GENB_2.TFldB1};
  a2: TFldB2 { TODO declaration:GENB_2.TFldB2};
begin
  fv{ TODO declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2B1._REC} );

  fy{ TODO declaration:GENB_2.TFldB0.fy} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  FField1{ TODO declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO: declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO: declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2B1.TFldI1 }

procedure GENI_2B1.TFldI1.Foo;
var
  a: _REC { TODO,WRONG: declaration:GENI_2B1._REC}; // TODO: the test finds it and yet we can not jump there
  b0: TFldI0 {declaration:GENI_2B1.TFldI0};
  b1: TFldI1 {declaration:GENI_2B1.TFldI1};
  b2: TFldI2 {declaration:GENI_2B1.TFldI2};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  fIv{ TODO declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_2B1._REC} );

  fy{ TODO declaration:GENB_1.TFldB1.fy}.r1a{ TODO: declaration:TRec1.r1a} := 1;
  FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO: declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO: declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2B1.TFldI2 }

procedure GENI_2B1.TFldI2.Foo;
var
  a: _REC { TODO,WRONG declaration:GENI_2B1._REC}; // TODO: the test finds something and yet we can not jump there
  b0: TFldI0 {declaration:GENI_2B1.TFldI0};
  b1: TFldI1 {declaration:GENI_2B1.TFldI1};
  b2: TFldI2 {declaration:GENI_2B1.TFldI2};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_2B1._REC} );

  fy{ TODO declaration:GENB_1.TFldB2.fy}.r2a{ TODO: declaration:TClass1.TRec2.r2a} := 1;
  FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2B1.TFldI3 }

procedure GENI_2B1.TFldI3.Foo;
var
  a: _REC { TODO,WRONG declaration:GENI_2B1._REC}; // TODO: the test finds something and yet we can not jump there
  b0: TFldI0 {declaration:GENI_2B1.TFldI0};
  b1: TFldI1 {declaration:GENI_2B1.TFldI1};
  b2: TFldI2 {declaration:GENI_2B1.TFldI2};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_2B1._REC} );

  fy{ TODO declaration:GENB_1.TFldB3.fy}.r3a{ TODO: declaration:TClass1.TRec3.r3a} := 1;
  FField1{declaration:GEN_Field.FField1}.r3a{declaration:TClass1.TRec3.r3a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2B1 }

procedure GENI_2B1.Bar;
var
  a: _REC {declaration:GENI_2B1._REC};
  b0: TFldI0 {declaration:GENI_2B1.TFldI0};
  b1: TFldI1 {declaration:GENI_2B1.TFldI1};
  b2: TFldI2 {declaration:GENI_2B1.TFldI2};
  a0: TFldB0 {declaration:GENB_1.TFldB0};
  a1: TFldB1 {declaration:GENB_1.TFldB1};
  a2: TFldB2 {declaration:GENB_1.TFldB2};
begin
  fv{declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  fIv{ TODO: declaration:GENB_1.fv} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  fx{declaration:GENB_1.fx} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  fIx{ TODO: declaration:GENB_1.fx} := default(_REC { TODO: declaration:GENI_2B1._REC} );

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2B1._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2.TFldI0 }

procedure GENI_2.TFldI0.Foo;
var
  a: _REC { TODO declaration:GENI_2._REC};
  b0: TFldI0 { TODO declaration:GENI_2.TFldI0};
  b1: TFldI1 { TODO declaration:GENI_2.TFldI1};
  b2: TFldI2 { TODO declaration:GENI_2.TFldI2};
  a0: TFldB0 { TODO declaration:GENB_2.TFldB0};
  a1: TFldB1 { TODO declaration:GENB_2.TFldB1};
  a2: TFldB2 { TODO declaration:GENB_2.TFldB2};
begin
  fv{ TODO declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );
  fIv{ TODO declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );

  fy{ TODO declaration:GENB_2.TFldB0.fy} := default(_REC { TODO: declaration:GENI_2._REC} );
  FField1{ TODO declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2.TFldI1 }

procedure GENI_2.TFldI1.Foo;
var
  a: _REC { TODO,WRONG: declaration:GENI_2._REC}; // TODO: the test finds it and yet we can not jump there
  b0: TFldI0 {declaration:GENI_2.TFldI0};
  b1: TFldI1 {declaration:GENI_2.TFldI1};
  b2: TFldI2 {declaration:GENI_2.TFldI2};
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );
  fIv{ TODO declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );

  fy{ TODO declaration:GENB_2.TFldB1.fy}.r1a{ TODO: declaration:TRec1.r1a} := 1;
  FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2.TFldI2 }

procedure GENI_2.TFldI2.Foo;
var
  a: _REC { TODO,WRONG declaration:GENI_2._REC}; // TODO: the test finds something and yet we can not jump there
  b0: TFldI0 {declaration:GENI_2.TFldI0};
  b1: TFldI1 {declaration:GENI_2.TFldI1};
  b2: TFldI2 {declaration:GENI_2.TFldI2};
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );
  fIv{ TODO declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );

  fy{ TODO declaration:GENB_2.TFldB2.fy}.r2a{ TODO: declaration:TClass1.TRec2.r2a} := 1;
  FField1{declaration:GEN_Field.FField1}.r2a{declaration:TClass1.TRec2.r2a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2.TFldI3 }

procedure GENI_2.TFldI3.Foo;
var
  a: _REC { TODO,WRONG declaration:GENI_2._REC}; // TODO: the test finds something and yet we can not jump there
  b0: TFldI0 {declaration:GENI_2.TFldI0};
  b1: TFldI1 {declaration:GENI_2.TFldI1};
  b2: TFldI2 {declaration:GENI_2.TFldI2};
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );

  fy{ TODO declaration:GENB_2.TFldB3.fy}.r3a{ TODO: declaration:TClass1.TRec3.r3a} := 1;
  FField1{declaration:GEN_Field.FField1}.r3a{declaration:TClass1.TRec3.r3a} := 1;

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;

{ GENI_2 }

procedure GENI_2.Bar;
var
  a: _REC {declaration:GENI_2._REC};
  b0: TFldI0 {declaration:GENI_2.TFldI0};
  b1: TFldI1 {declaration:GENI_2.TFldI1};
  b2: TFldI2 {declaration:GENI_2.TFldI2};
  a0: TFldB0 {declaration:GENB_2.TFldB0};
  a1: TFldB1 {declaration:GENB_2.TFldB1};
  a2: TFldB2 {declaration:GENB_2.TFldB2};
begin
  fv{declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );
  fIv{ TODO declaration:GENB_2.fv} := default(_REC { TODO: declaration:GENI_2._REC} );
  fx{declaration:GENB_2.fx} := default(_REC { TODO: declaration:GENI_2._REC} );
  fIx{ TODO declaration:GENB_2.fx} := default(_REC { TODO: declaration:GENI_2._REC} );

  b0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  b1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  b2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
  a0.FField1{declaration:GEN_Field.FField1} := default(_REC { TODO: declaration:GENI_2._REC} );
  a1.FField1{declaration:GEN_Field.FField1}.r1a{declaration:TRec1.r1a} := 1;
  a2.FField1{declaration:GEN_Field.FField1}.r2a{ TODO declaration:TClass1.TRec2.r2a} := 1;
end;


{ TClassG1R0 }

procedure TClassG1R0.Abc;
begin

end;

{ TClassG1R1 }

procedure TClassG1R1.Abc;
begin

end;

{ TClassG1Rb1 }

procedure TClassG1Rb1.Abc;
begin

end;

{ TClassB1 }

procedure TClassB1.Abc;
begin

end;

{ TClassB1C1 }

procedure TClassB1C1.Abc;
begin

end;

{ TClassB1C2 }

procedure TClassB1C2.Abc;
begin

end;

{ TClassB2C1 }

procedure TClassB2C1.Abc;
begin

end;

{ TClassB2C2 }

procedure TClassB2C2.Abc;
begin

end;

{ TClassI1 }

procedure TClassI1.Abc;
begin

end;

{ TClassI1C1 }

procedure TClassI1C1.Abc;
begin

end;

{ TClassI1C2 }

procedure TClassI1C2.Abc;
begin

end;

{ TClassI2bC1 }

procedure TClassI2bC1.Abc;
begin

end;

{ TClassI2bC2 }

procedure TClassI2bC2.Abc;
begin

end;

{ TClassI2C1 }

procedure TClassI2C1.Abc;
begin

end;

{ TClassI2C2 }

procedure TClassI2C2.Abc;
begin

end;


begin

end.

