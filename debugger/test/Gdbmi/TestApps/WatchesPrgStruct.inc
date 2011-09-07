(* Struture
  program WatchesPrg;
  type
    {$DEFINE Global_Type}
    {$DEFINE Global_Implementation}

  procedure FooFunc(  {$DEFINE FooFunc_Param}  }
    type
      {$DEFINE FooFunc_LocalType}
    var
      {$DEFINE FooFunc_LocalVar}
    function SubFoo()():Integer; begin end;
  begin
    {$DEFINE FooFunc_Body}
  end;

  var
    {$DEFINE Global_Var}
  begin
    {$DEFINE Global_Body}
    FooFunc(   {$DEFINE Global_Call_FooFunc}   );
    {$DEFINE Global_Body_NIL}
    FooFunc(   {$DEFINE Global_Call_FooFunc}   );
  end;

*)

//TODO: globla/local const

{$IFDEF Global_Type}
//type
  TFoo = class;
{$ENDIF}


{* ******************** RECORD ******************** *}
{%region RECORD}

  {%region  TYPE}
    {$IFDEF Global_Type}
    //type
      PRec = ^TRec;
      PPRec = ^PRec;
      TRec = record
        ValInt: Integer;
        ValFoo: TFoo;
      end;

      PRec2 = ^TRec2;
      PPRec2 = ^PRec2;
      TRec2 = record
        ValInt:    Integer;
        ValPRec:   PRec;
        ValPPRec:  PPRec;
        ValPRec2:  PRec2;
        ValPPRec2: PPRec2;
      end;

      TNewRec = type TRec;
    {$ENDIF}

    {$IFDEF FooFunc_LocalType}
    //type
      PLocalRec = ^TLocalRec;
      TLocalRec = record
        ValInt:       Integer;
        ValBar:       TFoo;
        ValPRec:      PRec;
        ValPLocalRec: PLocalRec;
      end;
    {$ENDIF}

  {%endregion TYPE}

  {%region VARIABLES}
    {$IFDEF FooFunc_Param}
    //procedure FooFunc(
      ArgTRec: TRec;                         var VArgTRec: TRec;
      ArgPRec: PRec;                         var VArgPRec: PRec;
      ArgPPRec: PPRec;                       var VArgPPRec: PPRec;
      ArgTNewRec: TNewRec;                   var VArgTNewRec: TNewRec;
    {$ENDIF}
    {$IFDEF Global_Call_FooFunc}
    //FooFunc(
      { records }
      GlobTRec,                         GlobTRec,
      GlobPRec,                         GlobPRec,
      GlobPPRec,                       GlobPPRec,
      GlobTNewRec,                   GlobTNewRec,
    {$ENDIF}

    {$IFDEF FooFunc_LocalVar}
    //var
      { records }
      VarTRec: TRec;
      VarPRec: PRec;
      VarPPRec: PPRec;
      VarTNewRec: TNewRec;

      PVarTRec: ^TRec;
      PVarTNewRec: ^TNewRec;

      VarRecA: record val: Integer; end;
    {$ENDIF}

    {$IFDEF Global_Var}
    //var
      { records }
      GlobTRec, GlobTRec1, GlobTRec2: TRec;
      GlobPRec: PRec;
      GlobPPRec: PPRec;
      GlobTNewRec: TNewRec;

      PGlobTRec: ^TRec;
      PGlobTNewRec: ^TNewRec;
    {$ENDIF}

  {%endregion VARIABLES}

  {%region CODE (initilization)}
    {$IFDEF FooFunc_Body}
    //begin
      { records }
      VarTRec := ArgTRec;
      VarPRec := ArgPRec;
      VarPPRec := ArgPPRec;
      VarTNewRec := ArgTNewRec;

      PVarTRec := @ArgTRec;
      PVarTNewRec := @ArgTNewRec;

      VarRecA.val := 1;
    {$ENDIF}

    {$IFDEF Global_Body}
    //begin
      { records }
      with GlobTRec do begin
        ValInt := -1;
        ValFoo := nil;
      end;
      with GlobTRec1 do begin
        ValInt := 1;
        ValFoo := TFoo.Create;
        ValFoo.ValueInt := 11;
      end;
      with GlobTRec2 do begin
        ValInt := 2;
        ValFoo := TFoo.Create;
        ValFoo.ValueInt := 22;
      end;

      GlobPRec  := @GlobTRec1;
      GlobPPRec := @GlobPRec;

      with GlobTNewRec do begin
        ValInt := 3;
        ValFoo := nil;
      end;

      PGlobTRec    := @GlobTNewRec;
      PGlobTNewRec := @GlobTNewRec;
    {$ENDIF}

    {$IFDEF Global_Body_NIL}
    //begin
      { records }
      //GlobTRec := nil;
      GlobPRec := nil;
      GlobPPRec := nil;
      //GlobTNewRec := nil;
    {$ENDIF}

  {%endregion CODE (initilization)}

{%endregion RECORD}

{* ******************** CLASSES ******************** *}
{%region  CLASSES  TypeCast }
  {%region  TYPE}
    {$IFDEF Global_Type}
      //type
      TClassTCast = class                        public b: Integer;  end;
      TClassTCastObject = class(TObject)         public b: Integer;  end;
      TClassTCastComponent = class(TComponent)   public b: Integer;  end;

      {$IFDEF USE_W1}
      TClassTCastUW1 = class(TClassUW1Base)                     public b: Integer;  end;
      TClassTCastUW1Object = class(TClassUW1BaseObject)         public b: Integer;  end;
      TClassTCastUW1Component = class(TClassUW1BaseComponent)   public b: Integer;  end;
      {$ENDIF}

      TClassTCast2 = class(TClassTCast)          public c: Integer;  end;
      TClassTCast3 = type TClassTCast;
    {$ENDIF}

    {$IFDEF FooFunc_LocalVar}
      // VarO..., VarNO... : TObject; // cast to real class
      VarOTestTCast, VarOTestTCastObj, VarOTestTCastComp, VarOTestTCast2: TObject;
      VarNOTestTCast, VarNOTestTCastObj, VarNOTestTCastComp, VarNOTestTCast2: TObject; // nil
      {$IFDEF USE_W1}
      VarOTestTCastUW1, VarOTestTCastUW1Obj, VarOTestTCastUW1Comp: TObject;
      VarNOTestTCastUW1, VarNOTestTCastUW1Obj, VarNOTestTCastUW1Comp: TObject; // nil
      {$ENDIF}

      // VarC..., VarNC...: TComponent; // cast to real class OR object
      VarCTestTCastComp, VarNCTestTCastComp: TComponent;
      {$IFDEF USE_W1}
      VarCTestTCastUW1Comp, VarNCTestTCastUW1Comp: TComponent;
      {$ENDIF}

      // VarB.., VarBN...: TBase;
      VarBTestTCast2, VarBNTestTCast2: TClassTCast;

      // Var.., VarN...: TRealType; // cast to lower class
      VarTestTCast, VarNTestTCast: TClassTCast;
      VarTestTCastObj, VarNTestTCastObj: TClassTCastObject;
      VarTestTCastComp, VarNTestTCastComp: TClassTCastComponent;
      VarTestTCast2, VarNTestTCast2: TClassTCast2;
      VarTestTCast3, VarNTestTCast3: TClassTCast3;
      {$IFDEF USE_W1}
      VarUTestTCastUW1, VarNUTestTCastUW1: TClassUW1Base;
      VarUTestTCastUW1Obj, VarNUTestTCastUW1Obj: TClassUW1BaseObject;
      VarUTestTCastUW1Comp, VarNUTestTCastUW1Comp: TClassUW1BaseComponent;
      VarTestTCastUW1, VarNTestTCastUW1: TClassTCastUW1;
      VarTestTCastUW1Obj, VarNTestTCastUW1Obj: TClassTCastUW1Object;
      VarTestTCastUW1Comp, VarNTestTCastUW1Comp: TClassTCastUW1Component;
      {$ENDIF}
    {$ENDIF}

    {$IFDEF FooFunc_Body}
      VarOTestTCast := TClassTCast.Create;
      VarOTestTCastObj := TClassTCastObject.Create;
      VarOTestTCastComp := TClassTCastComponent.Create(nil);
      VarOTestTCast2 := TClassTCast.Create;
      VarNOTestTCast := nil;
      VarNOTestTCastObj := nil;
      VarNOTestTCastComp := nil;
      VarNOTestTCast2 := nil;
      {$IFDEF USE_W1}
      VarOTestTCastUW1 := TClassTCastUW1.Create;
      VarOTestTCastUW1Obj := TClassTCastUW1Object.Create;
      VarOTestTCastUW1Comp := TClassTCastUW1Component.Create(nil);
      VarNOTestTCastUW1 := nil;
      VarNOTestTCastUW1Obj := nil;
      VarNOTestTCastUW1Comp := nil;
      {$ENDIF}

      VarCTestTCastComp := TClassTCastComponent.Create(nil);
      VarNCTestTCastComp := nil;
      {$IFDEF USE_W1}
      VarCTestTCastUW1Comp := TClassTCastUW1Component.Create(nil);
      VarNCTestTCastUW1Comp := nil;
      {$ENDIF}

      VarBTestTCast2 := TClassTCast2.Create;
      VarBNTestTCast2 := nil;

      VarTestTCast := TClassTCast.Create;
      VarNTestTCast := nil;
      VarTestTCastObj := TClassTCastObject.Create;
      VarNTestTCastObj := nil;
      VarTestTCastComp := TClassTCastComponent.Create(nil);
      VarNTestTCastComp := nil;
      VarTestTCast2 := TClassTCast2.Create;
      VarNTestTCast2 := nil;
      VarTestTCast3 := TClassTCast3.Create;
      VarNTestTCast3 := nil;
      {$IFDEF USE_W1}
      VarUTestTCastUW1 := TClassTCastUW1.Create;
      VarNUTestTCastUW1 := nil;
      VarUTestTCastUW1Obj := TClassTCastUW1Object.Create;
      VarNUTestTCastUW1Obj := nil;
      VarUTestTCastUW1Comp := TClassTCastUW1Component.Create(nil);
      VarNUTestTCastUW1Comp := nil;
      VarTestTCastUW1 := TClassTCastUW1.Create;
      VarNTestTCastUW1 := nil;
      VarTestTCastUW1Obj := TClassTCastUW1Object.Create;
      VarNTestTCastUW1Obj := nil;
      VarTestTCastUW1Comp := TClassTCastUW1Component.Create(nil);
      VarNTestTCastUW1Comp := nil;
      {$ENDIF}
    {$ENDIF}
{%endregion  CLASSES  TypeCast}

{%region  CLASSES}

  {%region  TYPE}
    {$IFDEF Global_Type}
      TFoo = class
      private
        function GetValueInt: Integer;
        procedure SetValueInt(AValue: Integer);
      public
        ValueInt: Integer;
        ValueFoo: TFoo;
        ValueRec: TRec;
        FooText: string[20];
        FooString: String;
        FooChar: Char;
        property PropInt: Integer read ValueInt write ValueInt;
        property PropIntGS: Integer read GetValueInt write SetValueInt;
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
    {$ENDIF}

    {$IFDEF Global_Implementation}
      { TFoo }

      function TFoo.GetValueInt: Integer;
      begin
        Result := PropInt;
      end;

      procedure TFoo.SetValueInt(AValue: Integer);
      begin
        PropInt := AValue;
      end;
    {$ENDIF}

    {$IFDEF FooFunc_LocalType}
      //type
    {$ENDIF}
  {%endregion TYPE}

  {%region VARIABLES}
    {$IFDEF FooFunc_Param}
      { Classes }
      ArgTFoo: TFoo;                        var VArgTFoo: TFoo;
      ArgPFoo: PFoo;                        var VArgPFoo: PFoo;
      ArgPPFoo: PPFoo;                      var VArgPPFoo: PPFoo;
      ArgTSamePFoo: TSamePFoo;              var VArgTSamePFoo: TSamePFoo;
      ArgTNewPFoo: TNewPFoo;                var VArgTNewPFoo: TNewPFoo;

      ArgTSameFoo: TSameFoo;                 var VArgTSameFoo: TSameFoo;
      ArgTNewFoo: TNewFoo;                   var VArgTNewFoo: TNewFoo;
      ArgPNewFoo: PNewFoo;                   var VArgPNewFoo: PNewFoo;
    {$ENDIF}
    {$IFDEF Global_Call_FooFunc}
      //FooFunc(
      { Classes }
      GlobTFoo,                        GlobTFoo,
      GlobPFoo,                        GlobPFoo,
      GlobPPFoo,                      GlobPPFoo,
      GlobTSamePFoo,              GlobTSamePFoo,
      GlobTNewPFoo,                GlobTNewPFoo,

      GlobTSameFoo,                 GlobTSameFoo,
      GlobTNewFoo,                   GlobTNewFoo,
      GlobPNewFoo,                   GlobPNewFoo,
    {$ENDIF}

    {$IFDEF FooFunc_LocalVar}
      //var
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
    {$ENDIF}

    {$IFDEF Global_Var}
    //var
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
    {$ENDIF}
  {%endregion VARIABLES}

  {%region CODE (initilization)}
    {$IFDEF FooFunc_Body}
      //begin
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
    {$ENDIF}

    {$IFDEF Global_Body}
      //begin
      { Classes }
      GlobTFoo := TFoo.Create;
      GlobTFoo.ValueInt := -11;
      GlobTFoo.FooText := 'mem of TFoo '' "';
      GlobTFoo.FooString := 'a 1 \ " '' '#9'...';
      GlobTFoo.FooChar := '\';
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
    {$ENDIF}

    {$IFDEF Global_Body_NIL}
      //begin
      { Classes }
      GlobTFoo := nil;
      GlobPFoo := nil;
      GlobPPFoo := nil;
      GlobTSamePFoo := nil;
      GlobTNewPFoo := nil;

      GlobTSameFoo := nil;
      GlobTNewFoo := nil;
      GlobPNewFoo := nil;
    {$ENDIF}

  {%endregion CODE (initilization)}
{%endregion CLASSES}


{* ******************** CLASSTYPES ******************** *}

{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
    { ClassesTyps }
    ArgTFooClass: TFooClass;               var VArgTFooClass: TFooClass;
    ArgPFooClass: PFooClass;               var VArgPFooClass: PFooClass;
    ArgPPFooClass: PPFooClass;             var VArgPPFooClass: PPFooClass;
    ArgTNewFooClass: TNewFooClass;         var VArgTNewFooClass: TNewFooClass;
    ArgPNewFooClass: PNewFooClass;         var VArgPNewFooClass: PNewFooClass;
  {$ENDIF}

  {$IFDEF FooFunc_LocalVar}
  //var
    { ClassesTyps }
    VarTFooClass: TFooClass;
    VarPFooClass: PFooClass;
    VarPPFooClass: PPFooClass;
    VarTNewFooClass: TNewFooClass;
    VarPNewFooClass: PNewFooClass;

    PVarTFooClass: ^TFooClass;

    { OBJECT }
    VarOldObject: TOldObject;
  {$ENDIF}

  {$IFDEF FooFunc_Body}
  //begin
    { ClassesTypes }
    VarTFooClass := ArgTFooClass;
    VarPFooClass := ArgPFooClass;
    VarPPFooClass := ArgPPFooClass;
    VarTNewFooClass := ArgTNewFooClass;
    VarPNewFooClass := ArgPNewFooClass;

    PVarTFooClass := @ArgTFooClass;

    { OBJECT }
    VarOldObject.OldVal := 1;
  {$ENDIF}
{%endregion FooFunc}


{%region GLOBAL}
  {$IFDEF Global_Type}
    { ClassesTypes }
    TFooClass = Class of TFoo;
    PFooClass = ^TFooClass;
    PPFooClass = ^PFooClass;

    TNewFooClass = class of TNewFoo;
    PNewFooClass = ^TNewFooClass;

    { OBJECT }
    TOldObject = object
      OldVal: Integer;
    end;
  {$ENDIF}

  {$IFDEF Global_Var}
    { ClassesTyps }
    GlobTFooClass: TFooClass;
    GlobPFooClass: PFooClass;
    GlobPPFooClass: PPFooClass;
    GlobTNewFooClass: TNewFooClass;
    GlobPNewFooClass: PNewFooClass;

    PGlobTFooClass: ^TFooClass;
  {$ENDIF}

  {$IFDEF Global_Body}
  //begin
    { ClassesTyps }
    GlobTFooClass := TFooKid;
    GlobPFooClass := @GlobTFooClass;
    GlobPPFooClass := @GlobPFooClass;
    GlobTNewFooClass := TNewFoo;
    GlobPNewFooClass := @GlobTNewFooClass;

    PGlobTFooClass := @GlobTNewFooClass;
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
    { ClassesTyps }
    GlobTFooClass := nil;
    GlobPFooClass := nil;
    GlobPPFooClass := nil;
    GlobTNewFooClass := nil;
    GlobPNewFooClass := nil;
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
    { ClassesTyps }
    GlobTFooClass,               GlobTFooClass,
    GlobPFooClass,               GlobPFooClass,
    GlobPPFooClass,             GlobPPFooClass,
    GlobTNewFooClass,         GlobTNewFooClass,
    GlobPNewFooClass,         GlobPNewFooClass,
  {$ENDIF}
{%endregion GLOBAL}

