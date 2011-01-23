
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
    ArgEnum: TEnum;       var VArgEnum: TEnum;
    ArgEnumSet: TEnumSet; var VArgEnumSet: TEnumSet;
    ArgSet: TSet;         var VArgSet: TSet;
  {$ENDIF}

  {$IFDEF FooFunc_Local}
  //var
    VarEnumA: (e1,e2,e3);
    VarEnumSetA: set of TEnum;
    VarSetA: set of (s1,s2,s3);
  {$ENDIF}

  {$IFDEF FooFunc_Body}
  //begin
    VarEnumA := e3;
    VarEnumSetA := [Three];
    VarSetA := [s2];
  {$ENDIF}
{%endregion FooFunc}

{%region GLOBAL}
  {$IFDEF Global_Types}
  //type
    TEnum = (One, Two, Three);
    TEnumSet = set of TEnum;
    TSet = Set of (Alpha, Beta, Gamma);
  {$ENDIF}

  {$IFDEF Global_Var}
  //var
    GlobEnum: TEnum;
    GlobEnumSet: TEnumSet;
    GlobSet: TSet;
  {$ENDIF}

  {$IFDEF Global_Body}
  //begin
    GlobEnum := Two;
    GlobEnumSet := [Two, Three];
    GlobSet := [Alpha, Beta];
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
    GlobEnum := One;
    GlobEnumSet := [];
    GlobSet := [];
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
    GlobEnum, GlobEnum,
    GlobEnumSet, GlobEnumSet,
    GlobSet, GlobSet,
  {$ENDIF}
{%endregion GLOBAL}

