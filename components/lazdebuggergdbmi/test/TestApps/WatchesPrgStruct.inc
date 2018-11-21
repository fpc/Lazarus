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
      TNewRec = type TRec;

      PRecSelf = ^TRecSelf;
      PPRecSelf = ^PRecSelf;
      TRecSelf = record
        ValInt:    Integer;
        ValPRec:   PRec;
        ValPPRec:  PPRec;
        ValPRec2:  PRecSelf;
        ValPPRec2: PPRecSelf;
        ValRecSelfDArray: Array of PRecSelf;
        ValRecSelfS0Array: Array [0..2] of PRecSelf;
        ValRecSelfS1Array: Array [1..3] of PRecSelf;
        ValRecSelfS3Array: Array [3..6] of PRecSelf;
      end;

      TRecSelfDArray = Array of TRecSelf;
      TRecSelfS0Array = Array [0..2] of TRecSelf;
      TRecSelfS1Array = Array [1..3] of TRecSelf;
      TRecSelfS3Array = Array [3..6] of TRecSelf;

      TPRecSelfDArray = Array of PRecSelf;
      TPRecSelfS0Array = Array [0..2] of PRecSelf;
      TPRecSelfS1Array = Array [1..3] of PRecSelf;
      TPRecSelfS3Array = Array [3..6] of PRecSelf;
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

      ArgTRecSelf: TRecSelf;                 var VArgTRecSelf: TRecSelf;
      ArgPRecSelf: PRecSelf;                 var VArgPRecSelf: PRecSelf;
      ArgPPRecSelf: PPRecSelf;               var VArgPPRecSelf: PPRecSelf;

      ArgTRecSelfDArray: TRecSelfDArray;     var VArgTRecSelfDArray: TRecSelfDArray;
      ArgTRecSelfS0Array: TRecSelfS0Array;   var VArgTRecSelfS0Array: TRecSelfS0Array;
      ArgTRecSelfS1Array: TRecSelfS1Array;   var VArgTRecSelfS1Array: TRecSelfS1Array;
      ArgTRecSelfS3Array: TRecSelfS3Array;   var VArgTRecSelfS3Array: TRecSelfS3Array;

      ArgTPRecSelfDArray: TPRecSelfDArray;     var VArgTPRecSelfDArray: TPRecSelfDArray;
      ArgTPRecSelfS0Array: TPRecSelfS0Array;   var VArgTPRecSelfS0Array: TPRecSelfS0Array;
      ArgTPRecSelfS1Array: TPRecSelfS1Array;   var VArgTPRecSelfS1Array: TPRecSelfS1Array;
      ArgTPRecSelfS3Array: TPRecSelfS3Array;   var VArgTPRecSelfS3Array: TPRecSelfS3Array;
    {$ENDIF}
    {$IFDEF Global_Call_FooFunc}
    //FooFunc(
      { records }
      GlobTRec,                    GlobTRec,
      GlobPRec,                    GlobPRec,
      GlobPPRec,                   GlobPPRec,
      GlobTNewRec,                 GlobTNewRec,

      GlobTRecSelf,                GlobTRecSelf2,
      GlobPRecSelf,                GlobPRecSelf2,
      GlobPPRecSelf,               GlobPPRecSelf2,

      GlobTRecSelfDArray,          GlobTRecSelfDArray2,
      GlobTRecSelfS0Array,         GlobTRecSelfS0Array2,
      GlobTRecSelfS1Array,         GlobTRecSelfS1Array2,
      GlobTRecSelfS3Array,         GlobTRecSelfS3Array2,

      GlobTPRecSelfDArray,          GlobTPRecSelfDArray2,
      GlobTPRecSelfS0Array,         GlobTPRecSelfS0Array2,
      GlobTPRecSelfS1Array,         GlobTPRecSelfS1Array2,
      GlobTPRecSelfS3Array,         GlobTPRecSelfS3Array2,
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
      GlobPRec, GlobPRec2: PRec;
      GlobPPRec: PPRec;
      GlobTNewRec: TNewRec;

      PGlobTRec: ^TRec;
      PGlobTNewRec: ^TNewRec;

      GlobTRecSelf, GlobTRecSelf2: TRecSelf;
      GlobPRecSelf, GlobPRecSelf2: PRecSelf;
      GlobPPRecSelf, GlobPPRecSelf2: PPRecSelf;

      GlobTRecSelfDArray, GlobTRecSelfDArray2: TRecSelfDArray;
      GlobTRecSelfS0Array, GlobTRecSelfS0Array2: TRecSelfS0Array;
      GlobTRecSelfS1Array, GlobTRecSelfS1Array2: TRecSelfS1Array;
      GlobTRecSelfS3Array, GlobTRecSelfS3Array2: TRecSelfS3Array;

      GlobTPRecSelfDArray, GlobTPRecSelfDArray2: TPRecSelfDArray;
      GlobTPRecSelfS0Array, GlobTPRecSelfS0Array2: TPRecSelfS0Array;
      GlobTPRecSelfS1Array, GlobTPRecSelfS1Array2: TPRecSelfS1Array;
      GlobTPRecSelfS3Array, GlobTPRecSelfS3Array2: TPRecSelfS3Array;

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
      GlobTRec.ValInt := -1;
      GlobTRec.ValFoo := nil;

      GlobTRec1.ValInt := 1;
      GlobTRec1.ValFoo := TFoo.Create;
      GlobTRec1.ValFoo.ValueInt := 11;

      GlobTRec2.ValInt := 2;
      GlobTRec2.ValFoo := TFoo.Create;
      GlobTRec2.ValFoo.ValueInt := 22;

      GlobPRec  := @GlobTRec1;
      GlobPRec2 := @GlobTRec2;
      GlobPPRec := @GlobPRec2;

      GlobTNewRec.ValInt := 3;
      GlobTNewRec.ValFoo := nil;

      PGlobTRec    := @GlobTNewRec;
      PGlobTNewRec := @GlobTNewRec;

      //GlobTRecSelf, GlobTRecSelf2: TRecSelf;
      GlobTRecSelf.ValInt    := 100;
      GlobTRecSelf.ValPRec   := @GlobTRec1;    // Valint=1
      GlobTRecSelf.ValPPRec  := @GlobPRec2;    // Valint=2
      GlobTRecSelf.ValPRec2  := @GlobTRecSelf; // self
      GlobTRecSelf.ValPPRec2 := @GlobTRecSelf.ValPRec2;

      GlobTRecSelf2.ValInt    := 102;
      GlobTRecSelf2.ValPRec   := @GlobTRec1;   // Valint=1
      GlobTRecSelf2.ValPPRec  := @GlobPRec2;   // Valint=2
      GlobTRecSelf2.ValPRec2  := @GlobTRecSelf2;
      GlobTRecSelf2.ValPPRec2 := @GlobTRecSelf2.ValPRec2;

      SetLength(GlobTRecSelf.ValRecSelfDArray, 2);
      GlobTRecSelf.ValRecSelfDArray[0] := @GlobTRecSelf2;
      GlobTRecSelf.ValRecSelfDArray[1] := @GlobTRecSelf;
      GlobTRecSelf.ValRecSelfS0Array[0] := @GlobTRecSelf2;
      GlobTRecSelf.ValRecSelfS0Array[1] := @GlobTRecSelf;
      GlobTRecSelf.ValRecSelfS1Array[1] := @GlobTRecSelf2;
      GlobTRecSelf.ValRecSelfS1Array[2] := @GlobTRecSelf;
      GlobTRecSelf.ValRecSelfS3Array[3] := @GlobTRecSelf2;
      GlobTRecSelf.ValRecSelfS3Array[4] := @GlobTRecSelf;

      SetLength(GlobTRecSelf2.ValRecSelfDArray, 2);
      GlobTRecSelf2.ValRecSelfDArray[0] := @GlobTRecSelf;
      GlobTRecSelf2.ValRecSelfDArray[1] := @GlobTRecSelf2;
      GlobTRecSelf2.ValRecSelfS0Array[0] := @GlobTRecSelf;
      GlobTRecSelf2.ValRecSelfS0Array[1] := @GlobTRecSelf2;
      GlobTRecSelf2.ValRecSelfS1Array[1] := @GlobTRecSelf;
      GlobTRecSelf2.ValRecSelfS1Array[2] := @GlobTRecSelf2;
      GlobTRecSelf2.ValRecSelfS3Array[3] := @GlobTRecSelf;
      GlobTRecSelf2.ValRecSelfS3Array[4] := @GlobTRecSelf2;

      //GlobPRecSelf, GlobPRecSelf2: PRecSelf;
      GlobPRecSelf := @GlobTRecSelf;
      GlobPRecSelf2 := @GlobTRecSelf2;

      //GlobPPRecSelf, GlobPPRecSelf2: PPRecSelf;
      GlobPPRecSelf := @GlobPRecSelf;
      GlobPPRecSelf2 := @GlobPRecSelf2;

      //GlobTRecSelfDArray, GlobTRecSelfDArray2: TRecSelfDArray;
      SetLength(GlobTRecSelfDArray, 2);
      SetLength(GlobTRecSelfDArray2, 2);
      GlobTRecSelfDArray[0] := GlobTRecSelf;
      GlobTRecSelfDArray[1] := GlobTRecSelf2;
      GlobTRecSelfDArray2[0] := GlobTRecSelf;
      GlobTRecSelfDArray2[1] := GlobTRecSelf2;

      //GlobTRecSelfS0Array, GlobTRecSelfS0Array2: TRecSelfS0Array;
      GlobTRecSelfS0Array[0] := GlobTRecSelf;
      GlobTRecSelfS0Array[1] := GlobTRecSelf2;
      GlobTRecSelfS0Array2[0] := GlobTRecSelf;
      GlobTRecSelfS0Array2[1] := GlobTRecSelf2;
      //GlobTRecSelfS1Array, GlobTRecSelfS1Array2: TRecSelfS1Array;
      GlobTRecSelfS1Array[1] := GlobTRecSelf;
      GlobTRecSelfS1Array[2] := GlobTRecSelf2;
      GlobTRecSelfS1Array2[1] := GlobTRecSelf;
      GlobTRecSelfS1Array2[2] := GlobTRecSelf2;
      //GlobTRecSelfS3Array, GlobTRecSelfS3Array2: TRecSelfS3Array;
      GlobTRecSelfS3Array[3] := GlobTRecSelf;
      GlobTRecSelfS3Array[4] := GlobTRecSelf2;
      GlobTRecSelfS3Array2[3] := GlobTRecSelf;
      GlobTRecSelfS3Array2[4] := GlobTRecSelf2;

      //GlobTPRecSelfDArray, GlobTPRecSelfDArray2: TPRecSelfDArray;
      SetLength(GlobTPRecSelfDArray, 2);
      SetLength(GlobTPRecSelfDArray2, 2);
      GlobTPRecSelfDArray[0] :=  @GlobTRecSelfDArray[0];
      GlobTPRecSelfDArray[1] :=  @GlobTRecSelfDArray[1];
      GlobTPRecSelfDArray2[0] := @GlobTRecSelfDArray2[0];
      GlobTPRecSelfDArray2[1] := @GlobTRecSelfDArray2[1];


      //GlobTPRecSelfS0Array, GlobTPRecSelfS0Array2: TPRecSelfS0Array;
      //GlobTPRecSelfS1Array, GlobTPRecSelfS1Array2: TPRecSelfS1Array;
      //GlobTPRecSelfS3Array, GlobTPRecSelfS3Array2: TPRecSelfS3Array;
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
    {$IFDEF Global_Type}
      //type
      TClassTCast          = class               public b: Integer;  end;
      TClassTCastObject    = class(TObject)      public b: Integer; l: Array of Integer; end;
      TClassTCastComponent = class(TComponent)   public b: Integer;  end;

      {$IFDEF USE_W1}
      TClassTCastUW1          = class(TClassUW1Base)            public b: Integer;  end;
      TClassTCastUW1Object    = class(TClassUW1BaseObject)      public b: Integer;  end;
      TClassTCastUW1Component = class(TClassUW1BaseComponent)   public b: Integer;  end;
      {$ENDIF}

      TClassTCast2 = class(TClassTCast)
      private
        privMember1: Integer;
      protected
        protMember1: Integer;
      public
        publMember1: Integer;
        c: Integer;
        procedure ClassTCast2Method1; //procedure TClassTCast2.ClassTCast2Method1;
      end;
      TClassTCast3 = type TClassTCast;
    {$ENDIF}

    {$IFDEF FooFunc_LocalVar}
      // VarO..., VarNO... : TObject; // cast to real class
      VarOTestTCast, VarOTestTCastObj, VarOTestTCastComp, VarOTestTCast2:     TObject;
      VarNOTestTCast, VarNOTestTCastObj, VarNOTestTCastComp, VarNOTestTCast2: TObject; // nil
      {$IFDEF USE_W1}
      VarOTestTCastUW1, VarOTestTCastUW1Obj, VarOTestTCastUW1Comp:            TObject;
      VarNOTestTCastUW1, VarNOTestTCastUW1Obj, VarNOTestTCastUW1Comp:         TObject; // nil
      {$ENDIF}

      // VarC..., VarNC...: TComponent; // cast to real class OR object
      VarCTestTCastComp, VarNCTestTCastComp:         TComponent;
      {$IFDEF USE_W1}
      VarCTestTCastUW1Comp, VarNCTestTCastUW1Comp:   TComponent;
      {$ENDIF}

      // VarB.., VarBN...: TBase;
      VarBTestTCast2, VarBNTestTCast2:               TClassTCast;

      // Var.., VarN...: TRealType; // cast to lower class
      VarTestTCast, VarNTestTCast:                   TClassTCast;
      VarTestTCastObj, VarNTestTCastObj:             TClassTCastObject;
      VarTestTCastComp, VarNTestTCastComp:           TClassTCastComponent;
      VarTestTCast2, VarNTestTCast2:                 TClassTCast2;
      VarTestTCast3, VarNTestTCast3:                 TClassTCast3;
      {$IFDEF USE_W1}
      VarUTestTCastUW1, VarNUTestTCastUW1:           TClassUW1Base;
      VarUTestTCastUW1Obj, VarNUTestTCastUW1Obj:     TClassUW1BaseObject;
      VarUTestTCastUW1Comp, VarNUTestTCastUW1Comp:   TClassUW1BaseComponent;
      VarTestTCastUW1, VarNTestTCastUW1:             TClassTCastUW1;
      VarTestTCastUW1Obj, VarNTestTCastUW1Obj:       TClassTCastUW1Object;
      VarTestTCastUW1Comp, VarNTestTCastUW1Comp:     TClassTCastUW1Component;
      {$ENDIF}
    {$ENDIF}

    {$IFDEF FooFunc_Body}
      // Declared TObject
      VarOTestTCast               := TClassTCast.Create;
      VarOTestTCastObj            := TClassTCastObject.Create;
      SetLength(TClassTCastObject(VarOTestTCastObj).l, 3);
      TClassTCastObject(VarOTestTCastObj).l[1] := 1144;
      VarOTestTCastComp           := TClassTCastComponent.Create(nil);
      VarOTestTCast2              := TClassTCast.Create;
      VarNOTestTCast              := nil;
      VarNOTestTCastObj           := nil;
      VarNOTestTCastComp          := nil;
      VarNOTestTCast2             := nil;
      {$IFDEF USE_W1}
      VarOTestTCastUW1            := TClassTCastUW1.Create;
      VarOTestTCastUW1Obj         := TClassTCastUW1Object.Create;
      VarOTestTCastUW1Comp        := TClassTCastUW1Component.Create(nil);
      VarNOTestTCastUW1           := nil;
      VarNOTestTCastUW1Obj        := nil;
      VarNOTestTCastUW1Comp       := nil;
      {$ENDIF}

      // declared TComponent
      VarCTestTCastComp           := TClassTCastComponent.Create(nil);
      VarNCTestTCastComp          := nil;
      {$IFDEF USE_W1}
      VarCTestTCastUW1Comp        := TClassTCastUW1Component.Create(nil);
      VarNCTestTCastUW1Comp       := nil;
      {$ENDIF}

      // declared TClassTCast
      VarBTestTCast2              := TClassTCast2.Create;
      VarBNTestTCast2             := nil;

      VarTestTCast                := TClassTCast.Create;
      VarNTestTCast               := nil;
      // Declared TClassTCastObject;
      VarTestTCastObj             := TClassTCastObject.Create;
      VarNTestTCastObj            := nil;
      // declared TClassTCastComponent;
      VarTestTCastComp            := TClassTCastComponent.Create(nil);
      VarNTestTCastComp           := nil;
      // declared TClassTCast2
      VarTestTCast2               := TClassTCast2.Create;
      VarNTestTCast2              := nil;
      // declared TClassTCast3
      VarTestTCast3               := TClassTCast3.Create;
      VarNTestTCast3              := nil;
      {$IFDEF USE_W1}
      VarUTestTCastUW1            := TClassTCastUW1.Create;
      VarNUTestTCastUW1           := nil;
      VarUTestTCastUW1Obj         := TClassTCastUW1Object.Create;
      VarNUTestTCastUW1Obj        := nil;
      VarUTestTCastUW1Comp        := TClassTCastUW1Component.Create(nil);
      VarNUTestTCastUW1Comp       := nil;
      VarTestTCastUW1             := TClassTCastUW1.Create;
      VarNTestTCastUW1            := nil;
      VarTestTCastUW1Obj          := TClassTCastUW1Object.Create;
      VarNTestTCastUW1Obj         := nil;
      VarTestTCastUW1Comp         := TClassTCastUW1Component.Create(nil);
      VarNTestTCastUW1Comp        := nil;
      {$ENDIF}
    {$ENDIF}
{%endregion  CLASSES  TypeCast}

{%region  CLASSES}

  {%region  TYPE}
    {$IFDEF Global_Type}
      TFooStatArray = Array[3..7] of boolean;
      TFooDynArray = Array of boolean;
      TString25 = string[25];
      TFooTestRecord = record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;

      TFoo = class
      private
        a1: TFooStatArray;
        a2: TFooDynArray;
        a3: Array[3..7, 2..4] of boolean;
        a4: Array of Array of boolean;
        a5: array [1..2] of record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
        a6: array [1..2,3..4] of record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
        a7: array of record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
        a8: array of array of record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
        r1: record x1:boolean; x2:integer; xr: record x1:boolean; x2:integer; end; end;
        r2: TFooTestRecord;
        s1: string[25];
        s2: TString25;

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

      procedure TClassTCast2.ClassTCast2Method1;
      begin
        // break on next line
        Freemem(GetMem(1));
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

    GlobClassTCast2: TClassTCast2;
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

  {$IFDEF Global_Body_Extra}
  //begin
    GlobClassTCast2 := TClassTCast2.Create;
    GlobClassTCast2.privMember1 := 411;
    GlobClassTCast2.protMember1 := 412;
    GlobClassTCast2.publMember1 := 413;
    GlobClassTCast2.ClassTCast2Method1;
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


{%region  CLASSES  Auto-TypeCast }
    {$IFDEF Global_Type}
      //type
      TAutoCastClassBase = class
      public
        b: Integer;
      end;
      TAutoCastClass = class(TAutoCastClassBase)
      public
        C: Integer;
        N1: TAutoCastClass;
        N2, VarAutoCastClassBase1: TAutoCastClassBase;
      end;

    {$ENDIF}

    {$IFDEF FooFunc_LocalVar}
      // VarO..., VarNO... : TObject; // cast to real class
      VarAutoCastClassBase1: TAutoCastClassBase;
      VarAutoCastClassBaseArr: Array [1..4] of TAutoCastClassBase;
    {$ENDIF}

    {$IFDEF FooFunc_Body}
      VarAutoCastClassBase1 := TAutoCastClass.Create;
      TAutoCastClass(VarAutoCastClassBase1).b := 2;
      TAutoCastClass(VarAutoCastClassBase1).C := 3;
      TAutoCastClass(VarAutoCastClassBase1).N1 := TAutoCastClass.Create;
      TAutoCastClass(TAutoCastClass(VarAutoCastClassBase1).N1).b := 105;
      TAutoCastClass(TAutoCastClass(VarAutoCastClassBase1).N1).c := 1;
      TAutoCastClass(VarAutoCastClassBase1).N2 := TAutoCastClass.Create;
      TAutoCastClass(TAutoCastClass(VarAutoCastClassBase1).N2).b := 101;
      TAutoCastClass(TAutoCastClass(VarAutoCastClassBase1).N2).c := 1;
      TAutoCastClass(VarAutoCastClassBase1).VarAutoCastClassBase1 := TAutoCastClass.Create;
      TAutoCastClass(TAutoCastClass(VarAutoCastClassBase1).VarAutoCastClassBase1).b := 104;
      TAutoCastClass(TAutoCastClass(VarAutoCastClassBase1).VarAutoCastClassBase1).c := 4;

      VarAutoCastClassBaseArr[1] := TAutoCastClass.Create;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).b := 11;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).c := 111;

      VarAutoCastClassBaseArr[2] := TAutoCastClass.Create;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).b := 12;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).c := 112;

      VarAutoCastClassBaseArr[3] := TAutoCastClass.Create;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).b := 13;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).c := 113;

      VarAutoCastClassBaseArr[4] := TAutoCastClass.Create;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).b := 14;
      TAutoCastClass(VarAutoCastClassBaseArr[1]).c := 114;


    {$ENDIF}
{%endregion  CLASSES  Auto-TypeCast}


