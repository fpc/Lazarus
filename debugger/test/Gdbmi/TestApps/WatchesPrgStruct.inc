(* Struture
  program WatchesPrg;
  type
    {$DEFINE Global_Types}
    {$DEFINE Global_Implementation}

  procedure FooFunc(  {$DEFINE FooFunc_Param}  }
    type
      {$DEFINE FooFunc_LocalType}
    var
      {$DEFINE FooFunc_Local}
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

{$IFDEF Global_Types}
//type
  TFoo = class;
{$ENDIF}


{%region RECORD}

  {%region  TYPE}
    {$IFDEF Global_Types}
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



{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
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

    VarFooComp, VarFooComp1: TFooComp;

    { OBJECT }
    VarOldObject: TOldObject;
  {$ENDIF}

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

    { ClassesTypes }
    VarTFooClass := ArgTFooClass;
    VarPFooClass := ArgPFooClass;
    VarPPFooClass := ArgPPFooClass;
    VarTNewFooClass := ArgTNewFooClass;
    VarPNewFooClass := ArgPNewFooClass;

    PVarTFooClass := @ArgTFooClass;

    { OBJECT }
    VarOldObject.OldVal := 1;

    VarFooComp := TFooComp.Create(nil);
    VarFooComp := nil;
  {$ENDIF}
{%endregion FooFunc}

{%region GLOBAL}
  {$IFDEF Global_Types}
    { Classes }

    TFooComp = class(TComponent)
    public
      ValueInt: Integer;
    end;

    { TFoo }

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
    { TFoo }

    function TFoo.GetValueInt: Integer;
    begin
      Result := PropInt;
    end;

    procedure TFoo.SetValueInt(AValue: Integer);
    begin
      PropInt := AValue;
    end;

  var

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

