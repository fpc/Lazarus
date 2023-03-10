program tgeneric_base;

{$mode objfpc}{$H+}

uses
  Classes;

type

  (* ********************************
   *
   * Types to use a param to generic
   *
   * ********************************)

  { TMyClassBase }

  TMyClassBase = class
    FMyBase: Integer;
    procedure MyBaseOnlyProc;
    procedure MyBaseOnlyProc2;
    procedure MyBaseOnlyProc3;

    procedure MyBaseAndMyClassProc1;
    procedure MyBaseAndMyClassProc2;

    procedure MyBaseAndMySubProc1;
    procedure MyBaseAndMySubProc2;

    procedure MyEveryProc1;
    procedure MyEveryProc2;
  end;

  { TMyClass }

  TMyClass = class(TMyClassBase)
    FMyClass: Integer;
    procedure MyBaseAndMyClassProc1; reintroduce;
    procedure MyBaseAndMyClassProc2; reintroduce;


    procedure MyClassProc1;
    procedure MyClassProc2;

    procedure MyClassAndMySubProc;

    procedure MyEveryProc1; reintroduce;
    procedure MyEveryProc2; reintroduce;
  end;

  { TMyClassSub }

  TMyClassSub = class(TMyClass)
    FMySub: Integer;
    procedure MyBaseAndMySubProc1; reintroduce;
    procedure MyBaseAndMySubProc2; reintroduce;

    procedure MyClassAndMySubProc; reintroduce;

    procedure MyClassSubProc; reintroduce;

    procedure MyEveryProc1; reintroduce;
    procedure MyEveryProc2; reintroduce;
  end;

  TWrongClass = class
    FMyBase: Integer;
    FMyClass: Integer;
    FMySub: Integer;

    procedure MyBaseOnlyProc; virtual; abstract;

    procedure MyBaseAndMyClassProc1; virtual; abstract;
    procedure MyBaseAndMyClassProc2; virtual; abstract;

    procedure MyBaseAndMySubProc1; virtual; abstract;
    procedure MyBaseAndMySubProc2; virtual; abstract;

    procedure MyEveryProc1; virtual; abstract;
    procedure MyEveryProc2; reintroduce; virtual; abstract;

    procedure MyClassProc1; virtual; abstract;
    procedure MyClassProc2; virtual; abstract;

    procedure MyClassAndMySubProc; virtual; abstract;

    procedure MyClassSubProc; reintroduce; virtual; abstract;
  end;

  TMyRec1 = record
    FRecA: Integer;
    FRecB: Integer;
  end;


  (* ********************************
   *
   * Use Gen-Param as type for field
   *
   * ********************************)

  { GEN_Field1 }

  generic GEN_Field1<_F1> = class
  public
    FField1: _F1;
    procedure FieldGenProc;
  end;

  { GEN_TField1 - With TYPED generic-param}

  generic GEN_TField1<_TF1: TMyClass> = class
  public
    FField1: _TF1;
    procedure FieldGenProc;
  end;

  { GEN_Forw_Field1 }

  generic GEN_Forw_Field1<_F_F1> = class
  public
    FField1: specialize GEN_Field1<_F_F1>;
//    FField2: specialize GEN_TField1<TMyClass(_F_F1)>;  // TODO : codetool does not parse the type-cast / FPC does
  private type
    TGenPar = _F_F1;
  public
    FField3: specialize GEN_Field1<TGenPar>;
//    FField4: specialize GEN_TField1<TMyClass(TGenPar)>;
  private type
    TField5 = specialize GEN_Field1<_F_F1>;
//    TField6 = specialize GEN_Field1<TMyClass(_F_F1)>;
  public
    FField5: TField5;
//    FField6: TField6;
    procedure FieldGenProc;
  end;

  // Use the SAME gen-name as the forward (but as dummy)
  generic GEN_Forw_Field2<_F1, _F_F1> = class
  public
    FField1: specialize GEN_Field1<_F_F1>;
    //procedure FieldGenProc;
  end;

  // Use the SAME gen-name as the forward (but as dummy)
  generic GEN_Forw_Field3<_F1, _F_F1> = class(specialize GEN_Forw_Field1<_F_F1>)
  public
    //procedure FieldGenProc;
  end;

  { GEN_Forw_TField1 }

  generic GEN_Forw_TField1<_F_TF1: TMyClass> = class
  public
    FField1: specialize GEN_Field1<_F_TF1>;
    FField2: specialize GEN_TField1<_F_TF1>;
  private type
    TGenPar = _F_TF1;
  public
    FField3: specialize GEN_Field1<TGenPar>;
    FField4: specialize GEN_TField1<TGenPar>;
  private type
    TField5 = specialize GEN_Field1<_F_TF1>;
    TField6 = specialize GEN_Field1<_F_TF1>;
  public
    FField5: TField5;
    FField6: TField6;
    procedure FieldGenProc;
  end;

  { GEN_ForwBase_Field1 }

  generic GEN_ForwBase_Field1<_FB_F1> = class(specialize GEN_Field1<_FB_F1>)
  public
    procedure FieldGenProc;
  end;

  //{ GEN_ForwBaseT_Field1 }
  //
  //generic GEN_ForwBaseT_Field1<_FBT_F1> = class(specialize GEN_TField1<TMyClass(_FBT_F1)>)
  //public
  //  procedure FieldGenProc;
  //end;

  { GEN_ForwBase_TField1 }

  generic GEN_ForwBase_TField1<_FB_TF1: TMyClass> = class(specialize GEN_Field1<_FB_TF1>)
  public
    procedure FieldGenProc;
  end;


  // SPECIALIZE GEN_Field1

  TTest_Field1 = specialize GEN_Field1<TMyClassSub>;

  { TTestInherit_Field1 }

  TTestInherit_Field1 = class(TTest_Field1)
    procedure FieldGenProc;
  end;

  { TTestInline_Field1 }

  TTestInline_Field1 = class(specialize GEN_Field1<TMyClassSub>)
    procedure FieldGenProc;
  end;

  { TTestInline_Field1_Wrong }

  TTestInline_Field1_Wrong = class(specialize GEN_Field1<TWrongClass>)
    procedure FieldGenProc;
  end;

  TTestInline_Field1_Rec = class(specialize GEN_Field1<TMyRec1>)
//    procedure FieldGenProc;
    property  P1: Integer read FField1.FRecA{declaration:TMyRec1.FRecA};
  end;


  // SPECIALIZE GEN_TField1

  TTest_TField1 = specialize GEN_TField1<TMyClassSub>;

  TTestInherit_TField1 = class(TTest_TField1)
    procedure FieldGenProc;
  end;

  TTestInline_TField1 = class(specialize GEN_TField1<TMyClassSub>)
    procedure FieldGenProc;
  end;


  // SPECIALIZE GEN_Forw_Field1

  TTest_Forw_Field1 = specialize GEN_Forw_Field1<TMyClassSub>;

  TTestInherit_Forw_Field1 = class(TTest_Forw_Field1)
    procedure FieldGenProc;
  end;

  TTestInline_Forw_Field1 = class(specialize GEN_Forw_Field1<TMyClassSub>)
    procedure FieldGenProc;
  end;

  // SPECIALIZE GEN_Forw_Field2

  TTest_Forw_Field2 = specialize GEN_Forw_Field2<TWrongClass, TMyClassSub>;

  TTestInherit_Forw_Field2 = class(TTest_Forw_Field2)
    procedure FieldGenProc;
  end;

  TTestInline_Forw_Field2 = class(specialize GEN_Forw_Field2<TWrongClass, TMyClassSub>)
    procedure FieldGenProc;
  end;

  // SPECIALIZE GEN_Forw_Field3

  TTest_Forw_Field3 = specialize GEN_Forw_Field3<TWrongClass, TMyClassSub>;

  TTestInherit_Forw_Field3 = class(TTest_Forw_Field3)
    procedure FieldGenProc;
  end;

  TTestInline_Forw_Field3 = class(specialize GEN_Forw_Field3<TWrongClass, TMyClassSub>)
    procedure FieldGenProc;
  end;

  // SPECIALIZE GEN_Forw_TField1

  TTest_Forw_TField1 = specialize GEN_Forw_TField1<TMyClassSub>;

  TTestInherit_Forw_TField1 = class(TTest_Forw_TField1)
    procedure FieldGenProc;
  end;

  TTestInline_Forw_TField1 = class(specialize GEN_Forw_TField1<TMyClassSub>)
    procedure FieldGenProc;
  end;


  // SPECIALIZE GEN_ForwBase_Field1

  TTest_ForwBase_Field1 = specialize GEN_ForwBase_Field1<TMyClassSub>;

  TTestInherit_ForwBase_Field1 = class(TTest_ForwBase_Field1)
    procedure FieldGenProc;
  end;

  TTestInline_ForwBase_Field1 = class(specialize GEN_ForwBase_Field1<TMyClassSub>)
    procedure FieldGenProc;
  end;


  // SPECIALIZE GEN_ForwBase_TField1

  TTest_ForwBase_TField1 = specialize GEN_ForwBase_TField1<TMyClassSub>;

  TTestInherit_ForwBase_TField1 = class(TTest_ForwBase_TField1)
    procedure FieldGenProc;
  end;

  TTestInline_ForwBase_TField1 = class(specialize GEN_ForwBase_TField1<TMyClassSub>)
    procedure FieldGenProc;
  end;


var
  Var1Test_Field1                 : TTest_Field1;
  Var1TestInherit_Field1          : TTestInherit_Field1;
  Var1TestInline_Field1           : TTestInline_Field1;
  Var1TestInline_Field1_Rec       : TTestInline_Field1_Rec;
  Var1Test_TField1                : TTest_TField1;
  Var1TestInherit_TField1         : TTestInherit_TField1;
  Var1TestInline_TField1          : TTestInline_TField1;
  Var1Test_Forw_Field1           : TTest_Forw_Field1;
  Var1TestInherit_Forw_Field1    : TTestInherit_Forw_Field1;
  Var1TestInline_Forw_Field1     : TTestInline_Forw_Field1;
  Var1Test_Forw_Field2            : TTest_Forw_Field2;
  Var1TestInherit_Forw_Field2     : TTestInherit_Forw_Field2;
  Var1TestInline_Forw_Field2      : TTestInline_Forw_Field2;
  Var1Test_Forw_Field3            : TTest_Forw_Field3;
  Var1TestInherit_Forw_Field3     : TTestInherit_Forw_Field3;
  Var1TestInline_Forw_Field3      : TTestInline_Forw_Field3;
  Var1Test_Forw_TField1           : TTest_Forw_TField1;
  Var1TestInherit_Forw_TField1    : TTestInherit_Forw_TField1;
  Var1TestInline_Forw_TField1     : TTestInline_Forw_TField1;
  Var1Test_ForwBase_Field1        : TTest_ForwBase_Field1;
  Var1TestInherit_ForwBase_Field1 : TTestInherit_ForwBase_Field1;
  Var1TestInline_ForwBase_Field1  : TTestInline_ForwBase_Field1;
  Var1Test_ForwBase_TField1       : TTest_ForwBase_TField1;
  Var1TestInherit_ForwBase_TField1: TTestInherit_ForwBase_TField1;
  Var1TestInline_ForwBase_TField1 : TTestInline_ForwBase_TField1;


  Var2Test_Field1           : specialize GEN_Field1<TMyClassSub>;
  Var2Test_TField1          : specialize GEN_TField1<TMyClassSub>;
  Var2Test_Forw_Field1      : specialize GEN_Forw_Field1<TMyClassSub>;
  Var2Test_Forw_TField1     : specialize GEN_Forw_TField1<TMyClassSub>;
  Var2Test_ForwBase_Field1  : specialize GEN_ForwBase_Field1<TMyClassSub>;
  Var2Test_ForwBase_TField1 : specialize GEN_ForwBase_TField1<TMyClassSub>;

  Var2Test_Forw_Field2      : specialize GEN_Forw_Field2<TWrongClass, TMyClassSub>;
  Var2Test_Forw_Field3      : specialize GEN_Forw_Field3<TWrongClass, TMyClassSub>;

type
  (* ********************************
   *
   * Use Gen-Param as base class
   *
   * ********************************)

  { GEN_Base1 }

  generic GEN_Base1<_B1: class> = class(_B1)
    procedure MyBaseOnlyProc2; reintroduce;
  end;

  { GEN_TBase1 }

  generic GEN_TBase1<_TB1: TMyClass> = class(_TB1)
    procedure MyBaseOnlyProc2; reintroduce;
  end;

  generic GEN_ForwBase_Base1<_B1: class; _FB_TB1: class> = class(specialize GEN_Base1<_FB_TB1>)
    procedure MyBaseOnlyProc; reintroduce;
    procedure MyBaseOnlyProc2; reintroduce;
  end;


  TTest_Base1 = specialize GEN_Base1<TMyClassSub>;

  TTestInherhit_Base1 = class(TTest_Base1)
    procedure Foo;
    procedure MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3}; reintroduce; virtual; abstract;
    procedure MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2}; reintroduce; virtual; abstract;
    procedure MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2}; reintroduce; virtual; abstract;
    procedure MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2}; reintroduce; virtual; abstract;
  end;

  TTestInline_Base1 = class(specialize GEN_Base1<TMyClassSub>)
    procedure Foo;
    procedure MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3}; reintroduce; virtual; abstract;
    procedure MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2}; reintroduce; virtual; abstract;
    procedure MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2}; reintroduce; virtual; abstract;
    procedure MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2}; reintroduce; virtual; abstract;
  end;

  TTestInline_Base1Sub = class(TTestInline_Base1)
    procedure Foo;
    procedure MyEveryProc1       {declaration:TMyClassSub.MyEveryProc1}; reintroduce; virtual; abstract;
  end;


  TTest_TBase1 = specialize GEN_TBase1<TMyClassSub>;

  TTestInherhit_TBase1 = class(TTest_TBase1)
    procedure Foo;
    procedure MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3}; reintroduce; virtual; abstract;
    procedure MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2}; reintroduce; virtual; abstract;
    procedure MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2}; reintroduce; virtual; abstract;
    procedure MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2}; reintroduce; virtual; abstract;
  end;

  TTestInline_TBase1 = class(specialize GEN_TBase1<TMyClassSub>)
    procedure Foo;
    procedure MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3}; reintroduce; virtual; abstract;
    procedure MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2}; reintroduce; virtual; abstract;
    procedure MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2}; reintroduce; virtual; abstract;
    procedure MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2}; reintroduce; virtual; abstract;
  end;

  TTestInline_TBase1Sub = class(TTestInline_TBase1)
    procedure Foo;
    procedure MyEveryProc1       {declaration:TMyClassSub.MyEveryProc1}; reintroduce; virtual; abstract;
  end;


  TTest_ForwBase_Base1 = specialize GEN_ForwBase_Base1<TWrongClass, TMyClassSub>;

  { TTest_ForwBaseInherhit_Base1 }

  TTest_ForwBaseInherhit_Base1 = class(TTest_ForwBase_Base1)
    procedure Foo;
    procedure MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3}; reintroduce; virtual; abstract;
    procedure MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2}; reintroduce; virtual; abstract;
    procedure MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2}; reintroduce; virtual; abstract;
    procedure MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2}; reintroduce; virtual; abstract;
  end;

  { TTest_ForwBaseInline_Base1 }

  TTest_ForwBaseInline_Base1 = class(specialize GEN_ForwBase_Base1<TWrongClass, TMyClassSub>)
    procedure Foo;
    procedure MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3}; reintroduce; virtual; abstract;
    procedure MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2}; reintroduce; virtual; abstract;
    procedure MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2}; reintroduce; virtual; abstract;
    procedure MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2}; reintroduce; virtual; abstract;
  end;


  (* ********************************
   *
   * Types to use a param to generic
   *
   * ********************************)

{ TMyClassBase }

procedure TMyClassBase.MyBaseOnlyProc;
begin

end;

procedure TMyClassBase.MyBaseOnlyProc2;
begin

end;

procedure TMyClassBase.MyBaseOnlyProc3;
begin

end;

procedure TMyClassBase.MyBaseAndMyClassProc1;
begin
//
end;

procedure TMyClassBase.MyBaseAndMyClassProc2;
begin

end;

procedure TMyClassBase.MyBaseAndMySubProc1;
begin

end;

procedure TMyClassBase.MyBaseAndMySubProc2;
begin

end;

procedure TMyClassBase.MyEveryProc1;
begin
//
end;

procedure TMyClassBase.MyEveryProc2;
begin

end;

{ TMyClass }

procedure TMyClass.MyBaseAndMyClassProc1;
begin
//
end;

procedure TMyClass.MyBaseAndMyClassProc2;
begin

end;

procedure TMyClass.MyClassProc1;
begin

end;

procedure TMyClass.MyClassProc2;
begin

end;

procedure TMyClass.MyClassAndMySubProc;
begin

end;

procedure TMyClass.MyEveryProc1;
begin

end;

procedure TMyClass.MyEveryProc2;
begin

end;

{ TMyClassSub }

procedure TMyClassSub.MyBaseAndMySubProc1;
begin
//
end;

procedure TMyClassSub.MyBaseAndMySubProc2;
begin

end;

procedure TMyClassSub.MyClassAndMySubProc;
begin
//
end;

procedure TMyClassSub.MyClassSubProc;
begin

end;

procedure TMyClassSub.MyEveryProc1;
begin

end;

procedure TMyClassSub.MyEveryProc2;
begin

end;

  (* ********************************
   *
   * Use Gen-Param as type for field
   *
   * ********************************)

{ GEN_Field1 }

procedure GEN_Field1.FieldGenProc;
begin
  //
end;

{ GEN_TField1 }

procedure GEN_TField1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc{declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1{declaration:TMyClass.MyBaseAndMyClassProc1};
  {$IFDEF WRONG}
  FField1.MyClassSubProc{declaration:};
  {$ENDIF}
end;

{ GEN_Forw_Field1 }

procedure GEN_Forw_Field1.FieldGenProc;
begin
end;

{ GEN_Forw_TField1 }

procedure GEN_Forw_TField1.FieldGenProc;
begin
end;

{ GEN_ForwBase_Field1 }

procedure GEN_ForwBase_Field1.FieldGenProc;
begin
end;

//{ GEN_ForwBaseT_Field1 }
//
//procedure GEN_ForwBaseT_Field1.FieldGenProc;
//begin
//end;

{ GEN_ForwBase_TField1 }

procedure GEN_ForwBase_TField1.FieldGenProc;
begin
end;


{ TTestInherit_Field1 }

procedure TTestInherit_Field1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInline_Field1 }

procedure TTestInline_Field1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInline_Field1_Wrong }

procedure TTestInline_Field1_Wrong.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TWrongClass.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TWrongClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TWrongClass.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TWrongClass.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TWrongClass.FMySub} := 1;
end;

{ TTestInherit_TField1 }

procedure TTestInherit_TField1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInline_TField1 }

procedure TTestInline_TField1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInherit_Forw_Field1 }

procedure TTestInherit_Forw_Field1.FieldGenProc;
begin
  FField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField3.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField3.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField3.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField5.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField5.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField5.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

end;

{ TTestInline_Forw_Field1 }

procedure TTestInline_Forw_Field1.FieldGenProc;
begin
  FField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField3.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField3.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField3.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField5.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField5.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField5.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInherit_Forw_Field2 }

procedure TTestInherit_Forw_Field2.FieldGenProc;
begin
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

procedure TTestInline_Forw_Field2.FieldGenProc;
begin
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInherit_Forw_Field3 }

procedure TTestInherit_Forw_Field3.FieldGenProc;
begin
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField3.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField5.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

procedure TTestInline_Forw_Field3.FieldGenProc;
begin
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField3.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField5.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInherit_Forw_TField1 }

procedure TTestInherit_Forw_TField1.FieldGenProc;
begin
  FField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField2.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField2.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField2.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField3.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField3.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField3.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField4.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField4.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField4.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField5.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField5.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField5.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField6.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField6.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField6.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

end;

{ TTestInline_Forw_TField1 }

procedure TTestInline_Forw_TField1.FieldGenProc;
begin
  FField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField2.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField2.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField2.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField3.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField3.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField3.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField4.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField4.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField4.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField5.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField5.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField5.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  FField6.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField6.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField6.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

end;

{ TTestInherit_ForwBase_Field1 }

procedure TTestInherit_ForwBase_Field1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInline_ForwBase_Field1 }

procedure TTestInline_ForwBase_Field1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInherit_ForwBase_TField1 }

procedure TTestInherit_ForwBase_TField1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

{ TTestInline_ForwBase_TField1 }

procedure TTestInline_ForwBase_TField1.FieldGenProc;
begin
  FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;
end;

procedure TestGenFieldVariables;
begin
  Var1Test_Field1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var1Test_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Field1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var1Test_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Field1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var1TestInherit_Field1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var1TestInherit_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Field1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var1TestInherit_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Field1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var1TestInherit_Field1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var1TestInherit_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Field1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var1TestInherit_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Field1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var1TestInline_Field1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var1TestInline_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Field1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var1TestInline_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Field1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var1Test_Forw_Field1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_Field1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1Test_Forw_Field1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1Test_Forw_Field1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_Field1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_Field1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1Test_Forw_Field1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1Test_Forw_Field1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_Field1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_Field1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1Test_Forw_Field1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1Test_Forw_Field1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInherit_Forw_Field1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_Field1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1TestInherit_Forw_Field1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1TestInherit_Forw_Field1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_Field1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_Field1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1TestInherit_Forw_Field1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1TestInherit_Forw_Field1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_Field1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_Field1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1TestInherit_Forw_Field1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1TestInherit_Forw_Field1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInline_Forw_Field1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_Field1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1TestInline_Forw_Field1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1TestInline_Forw_Field1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_Field1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_Field1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1TestInline_Forw_Field1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1TestInline_Forw_Field1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_Field1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_Field1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var1TestInline_Forw_Field1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var1TestInline_Forw_Field1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1Test_Forw_Field2.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_Field2.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInherit_Forw_Field2.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_Field2.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInline_Forw_Field2.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_Field2.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1Test_Forw_Field3.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_Field3.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_Field3.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_Field3.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_Field3.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_Field3.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInherit_Forw_Field3.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_Field3.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_Field3.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_Field3.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_Field3.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_Field3.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInline_Forw_Field3.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_Field3.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_Field3.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_Field3.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_Field3.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_Field3.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1Test_Forw_TField1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_TField1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_TField1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_TField1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_TField1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_TField1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_TField1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_TField1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_TField1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_TField1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_Forw_TField1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_Forw_TField1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInherit_Forw_TField1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_TField1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_TField1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_TField1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_TField1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_TField1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_TField1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_TField1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_TField1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_TField1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_Forw_TField1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_Forw_TField1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInline_Forw_TField1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_TField1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_TField1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_TField1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_TField1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_TField1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_TField1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_TField1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_TField1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_TField1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_Forw_TField1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_Forw_TField1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1Test_ForwBase_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_ForwBase_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInherit_ForwBase_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_ForwBase_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInline_ForwBase_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_ForwBase_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1Test_ForwBase_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_ForwBase_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInherit_ForwBase_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_ForwBase_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var1TestInline_ForwBase_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_ForwBase_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

//  Var1TestInline_Field1_Rec.FField1.FRecA {declaration:TMyRec1.FRecA};

  Var1Test_TField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var1Test_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1Test_TField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var1Test_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1Test_TField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var1TestInherit_TField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var1TestInherit_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInherit_TField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var1TestInherit_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInherit_TField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var1TestInline_TField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var1TestInline_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var1TestInline_TField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var1TestInline_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var1TestInline_TField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var2Test_Field1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var2Test_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Field1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var2Test_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Field1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var2Test_TField1.FField1.MyBaseOnlyProc        {declaration:TMyClassBase.MyBaseOnlyProc};
  Var2Test_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_TField1.FField1.MyClassSubProc        {declaration:TMyClassSub.MyClassSubProc};
  Var2Test_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_TField1.FField1.FMySub                {declaration:TMyClassSub.FMySub} := 1;

  Var2Test_Forw_Field1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_Field1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var2Test_Forw_Field1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var2Test_Forw_Field1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_Field1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_Field1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var2Test_Forw_Field1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var2Test_Forw_Field1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_Field1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_Field1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  //Var2Test_Forw_Field1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  //Var2Test_Forw_Field1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var2Test_Forw_TField1.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_TField1.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_TField1.FField2.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_TField1.FField2.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_TField1.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_TField1.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_TField1.FField4.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_TField1.FField4.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_TField1.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_TField1.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_TField1.FField6.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_TField1.FField6.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var2Test_ForwBase_Field1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_ForwBase_Field1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var2Test_ForwBase_TField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_ForwBase_TField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var2Test_Forw_Field2.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_Field2.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

  Var2Test_Forw_Field3.FField1.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_Field3.FField1.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_Field3.FField3.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_Field3.FField3.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};
  Var2Test_Forw_Field3.FField5.FField1.MyBaseAndMyClassProc1 {declaration:TMyClass.MyBaseAndMyClassProc1};
  Var2Test_Forw_Field3.FField5.FField1.MyBaseAndMySubProc1   {declaration:TMyClassSub.MyBaseAndMySubProc1};

end;

  (* ********************************
   *
   * Use Gen-Param as base class
   *
   * ********************************)

{ GEN_Base1 }

procedure GEN_Base1.MyBaseOnlyProc2;
begin
  //
end;


{ GEN_TBase1 }

procedure GEN_TBase1.MyBaseOnlyProc2;
begin

end;

{ GEN_ForwBase_Base1 }

procedure GEN_ForwBase_Base1.MyBaseOnlyProc;
begin

end;

procedure GEN_ForwBase_Base1.MyBaseOnlyProc2;
begin

end;

{ TTestInherhit_Base1 }

procedure TTestInherhit_Base1.Foo;
begin
  MyEveryProc1          {declaration:TMyClassSub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};

  MyBaseOnlyProc3      {declaration:TTestInherhit_Base1.MyBaseOnlyProc3};
  inherited MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3};
  inherited MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2};
  inherited MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2};
  inherited MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

{ TTestInline_Base1 }

procedure TTestInline_Base1.Foo;
begin
  MyEveryProc1          {declaration:TMyClassSub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};

  MyBaseOnlyProc3      {declaration:TTestInline_Base1.MyBaseOnlyProc3};
  inherited MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3};
  inherited MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2};
  inherited MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2};
  inherited MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

{ TTestInline_Base1Sub }

procedure TTestInline_Base1Sub.Foo;
begin
  MyEveryProc1          {declaration:TTestInline_Base1Sub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

{ TTestInherhit_TBase1 }

procedure TTestInherhit_TBase1.Foo;
begin
  MyEveryProc1          {declaration:TMyClassSub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};

  MyBaseOnlyProc3      {declaration:TTestInherhit_TBase1.MyBaseOnlyProc3};
  inherited MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3};
  inherited MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2};
  inherited MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2};
  inherited MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

{ TTestInline_TBase1 }

procedure TTestInline_TBase1.Foo;
begin
  MyEveryProc1          {declaration:TMyClassSub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};

  MyBaseOnlyProc3      {declaration:TTestInline_TBase1.MyBaseOnlyProc3};
  inherited MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3};
  inherited MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2};
  inherited MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2};
  inherited MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

{ TTestInline_TBase1Sub }

procedure TTestInline_TBase1Sub.Foo;
begin
  MyEveryProc1          {declaration:TTestInline_TBase1Sub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

{ TTest_ForwBaseInherhit_Base1 }

procedure TTest_ForwBaseInherhit_Base1.Foo;
begin
  MyEveryProc1          {declaration:TMyClassSub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};

  MyBaseOnlyProc3      {declaration:TTest_ForwBaseInherhit_Base1.MyBaseOnlyProc3};
  inherited MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3};
  inherited MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2};
  inherited MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2};
  inherited MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

{ TTest_ForwBaseInline_Base1 }

procedure TTest_ForwBaseInline_Base1.Foo;
begin
  MyEveryProc1          {declaration:TMyClassSub.MyEveryProc1};
  inherited MyEveryProc1{declaration:TMyClassSub.MyEveryProc1};

  MyBaseOnlyProc3      {declaration:TTest_ForwBaseInline_Base1.MyBaseOnlyProc3};
  inherited MyBaseOnlyProc3      {declaration:TMyClassBase.MyBaseOnlyProc3};
  inherited MyBaseAndMyClassProc2{declaration:TMyClass.MyBaseAndMyClassProc2};
  inherited MyBaseAndMySubProc2  {declaration:TMyClassSub.MyBaseAndMySubProc2};
  inherited MyEveryProc2         {declaration:TMyClassSub.MyEveryProc2};
  {completion:MyBaseOnlyProc;MyBaseAndMyClassProc1;MyBaseAndMySubProc1;MyEveryProc1}
end;

begin
  a1{guesstype:TTest_Field1}                  := TTest_Field1.Create;
  a1{guesstype:TTestInherit_Field1}           := TTestInherit_Field1.Create;
  a1{guesstype:TTestInline_Field1}            := TTestInline_Field1.Create;
  a1{guesstype:TTestInline_Field1_Wrong}      := TTestInline_Field1_Wrong.Create;
  a1{guesstype:TTestInline_Field1_Rec}        := TTestInline_Field1_Rec.Create;
  a1{guesstype:TTest_TField1}                 := TTest_TField1.Create;
  a1{guesstype:TTestInherit_TField1}          := TTestInherit_TField1.Create;
  a1{guesstype:TTestInline_TField1}           := TTestInline_TField1.Create;
  a1{guesstype:TTest_Forw_Field1}             := TTest_Forw_Field1.Create;
  a1{guesstype:TTestInherit_Forw_Field1}      := TTestInherit_Forw_Field1.Create;
  a1{guesstype:TTestInline_Forw_Field1}       := TTestInline_Forw_Field1.Create;
  a1{guesstype:TTest_Forw_Field2}             := TTest_Forw_Field2.Create;
  a1{guesstype:TTestInherit_Forw_Field2}      := TTestInherit_Forw_Field2.Create;
  a1{guesstype:TTestInline_Forw_Field2}       := TTestInline_Forw_Field2.Create;
  a1{guesstype:TTest_Forw_Field3}             := TTest_Forw_Field3.Create;
  a1{guesstype:TTestInherit_Forw_Field3}      := TTestInherit_Forw_Field3.Create;
  a1{guesstype:TTestInline_Forw_Field3}       := TTestInline_Forw_Field3.Create;
  a1{guesstype:TTest_Forw_TField1}            := TTest_Forw_TField1.Create;
  a1{guesstype:TTestInherit_Forw_TField1}     := TTestInherit_Forw_TField1.Create;
  a1{guesstype:TTestInline_Forw_TField1}      := TTestInline_Forw_TField1.Create;
  a1{guesstype:TTest_ForwBase_Field1}         := TTest_ForwBase_Field1.Create;
  a1{guesstype:TTestInherit_ForwBase_Field1}  := TTestInherit_ForwBase_Field1.Create;
  a1{guesstype:TTestInline_ForwBase_Field1}   := TTestInline_ForwBase_Field1.Create;
  a1{guesstype:TTest_ForwBase_TField1}        := TTest_ForwBase_TField1.Create;
  a1{guesstype:TTestInherit_ForwBase_TField1} := TTestInherit_ForwBase_TField1.Create;
  a1{guesstype:TTestInline_ForwBase_TField1}  := TTestInline_ForwBase_TField1.Create;
  a1{guesstype:TTest_Base1}                   := TTest_Base1.Create;
  a1{guesstype:TTestInherhit_Base1}           := TTestInherhit_Base1.Create;
  a1{guesstype:TTestInline_Base1}             := TTestInline_Base1.Create;
  a1{guesstype:TTestInline_Base1Sub}          := TTestInline_Base1Sub.Create;
  a1{guesstype:TTest_TBase1}                  := TTest_TBase1.Create;
  a1{guesstype:TTestInherhit_TBase1}          := TTestInherhit_TBase1.Create;
  a1{guesstype:TTestInline_TBase1}            := TTestInline_TBase1.Create;
  a1{guesstype:TTestInline_TBase1Sub}         := TTestInline_TBase1Sub.Create;
  a1{guesstype:TTest_ForwBase_Base1}          := TTest_ForwBase_Base1.Create;
  a1{guesstype:TTest_ForwBaseInherhit_Base1}  := TTest_ForwBaseInherhit_Base1.Create;
  a1{guesstype:TTest_ForwBaseInline_Base1}    := TTest_ForwBaseInline_Base1.Create;
end.

