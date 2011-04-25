
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
    { records }
    ArgTRec: TRec;                         var VArgTRec: TRec;
    ArgPRec: PRec;                         var VArgPRec: PRec;
    ArgPPRec: PPRec;                       var VArgPPRec: PPRec;
    ArgTNewRec: TNewRec;                   var VArgTNewRec: TNewRec;

    { Classes }
    ArgTFoo: TFoo;                        var VArgTFoo: TFoo;
    ArgPFoo: PFoo;                        var VArgPFoo: PFoo;
    ArgPPFoo: PPFoo;                      var VArgPPFoo: PPFoo;
    ArgTSamePFoo: TSamePFoo;              var VArgTSamePFoo: TSamePFoo;
    ArgTNewPFoo: TNewPFoo;                var VArgTNewPFoo: TNewPFoo;

    ArgTSameFoo: TSameFoo;                 var VArgTSameFoo: TSameFoo;
    ArgTNewFoo: TNewFoo;                   var VArgTNewFoo: TNewFoo;
    ArgPNewFoo: PNewFoo;                   var VArgPNewFoo: PNewFoo;

    { ClassesTyps }
    ArgTFooClass: TFooClass;               var VArgTFooClass: TFooClass;
    ArgPFooClass: PFooClass;               var VArgPFooClass: PFooClass;
    ArgPPFooClass: PPFooClass;             var VArgPPFooClass: PPFooClass;
    ArgTNewFooClass: TNewFooClass;         var VArgTNewFooClass: TNewFooClass;
    ArgPNewFooClass: PNewFooClass;         var VArgPNewFooClass: PNewFooClass;
  {$ENDIF}

  {$IFDEF FooFunc_Local}
  //var
    { records }
    VarTRec: TRec;
    VarPRec: PRec;
    VarPPRec: PPRec;
    VarTNewRec: TNewRec;

    PVarTRec: ^TRec;
    PVarTNewRec: ^TNewRec;

    VarRecA: record val: Integer; end;

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
    { records }
    VarTRec := ArgTRec;
    VarPRec := ArgPRec;
    VarPPRec := ArgPPRec;
    VarTNewRec := ArgTNewRec;

    PVarTRec := @ArgTRec;
    PVarTNewRec := @ArgTNewRec;

    VarRecA.val := 1;

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
  {$IFDEF Global_Types}
  //type
    TFoo = class;

    { records }
    TRec = record
      ValInt: Integer;
      ValFoo: TFoo;
    end;

    PRec = ^TRec;
    PPRec = ^PRec;

    TNewRec = type TRec;

    { Classes }

    TFoo = class
    public
      ValueInt: Integer;
      ValueFoo: TFoo;
      ValueRec: TRec;
      FooText: string[20];
      FooString: String;
      FooChar: Char;
      property PropInt: Integer read ValueInt write ValueInt;
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
  //var
    { records }
    GlobTRec, GlobTRec1, GlobTRec2: TRec;
    GlobPRec: PRec;
    GlobPPRec: PPRec;
    GlobTNewRec: TNewRec;

    PGlobTRec: ^TRec;
    PGlobTNewRec: ^TNewRec;

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
    { records }
    GlobTRec.ValInt := -1;
    GlobTRec.ValFoo := nil;
    GlobTRec1.ValInt := 1;
    GlobTRec1.ValFoo := TFoo.Create;
    GlobTRec1.ValFoo.ValueInt := 11;
    GlobTRec2.ValInt := 2;
    GlobTRec2.ValFoo := TFoo.Create;
    GlobTRec2.ValFoo.ValueInt := 22;

    GlobPRec := @GlobTRec1;
    GlobPPRec := @GlobPRec;
    GlobTNewRec.ValInt := 3;
    GlobTNewRec.ValFoo := nil;

    PGlobTRec := @GlobTNewRec;
    PGlobTNewRec := @GlobTNewRec;

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
    { records }
    //GlobTRec := nil;
    GlobPRec := nil;
    GlobPPRec := nil;
    //GlobTNewRec := nil;

    { Classes }
    GlobTFoo := nil;
    GlobPFoo := nil;
    GlobPPFoo := nil;
    GlobTSamePFoo := nil;
    GlobTNewPFoo := nil;

    GlobTSameFoo := nil;
    GlobTNewFoo := nil;
    GlobPNewFoo := nil;

    { ClassesTyps }
    GlobTFooClass := nil;
    GlobPFooClass := nil;
    GlobPPFooClass := nil;
    GlobTNewFooClass := nil;
    GlobPNewFooClass := nil;
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
    { records }
    GlobTRec,                         GlobTRec,
    GlobPRec,                         GlobPRec,
    GlobPPRec,                       GlobPPRec,
    GlobTNewRec,                   GlobTNewRec,

    { Classes }
    GlobTFoo,                        GlobTFoo,
    GlobPFoo,                        GlobPFoo,
    GlobPPFoo,                      GlobPPFoo,
    GlobTSamePFoo,              GlobTSamePFoo,
    GlobTNewPFoo,                GlobTNewPFoo,

    GlobTSameFoo,                 GlobTSameFoo,
    GlobTNewFoo,                   GlobTNewFoo,
    GlobPNewFoo,                   GlobPNewFoo,

    { ClassesTyps }
    GlobTFooClass,               GlobTFooClass,
    GlobPFooClass,               GlobPFooClass,
    GlobPPFooClass,             GlobPPFooClass,
    GlobTNewFooClass,         GlobTNewFooClass,
    GlobPNewFooClass,         GlobPNewFooClass,
  {$ENDIF}
{%endregion GLOBAL}

