
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
    ArgTMyAnsiString: TMyAnsiString;           var VArgTMyAnsiString: TMyAnsiString;
    ArgPMyAnsiString: PMyAnsiString;           var VArgPMyAnsiString: PMyAnsiString;
    ArgPPMyAnsiString: PPMyAnsiString;         var VArgPPMyAnsiString: PPMyAnsiString;

    ArgTNewAnsiString: TNewAnsiString;         var VArgTNewAnsiString: TNewAnsiString;
    ArgPNewAnsiString: PNewAnsiString;         var VArgPNewAnsiString: PNewAnsiString;


    ArgTMyShortString: TMyShortString;         var VArgTMyShortString: TMyShortString;
    ArgPMyShortString: PMyShortString;         var VArgPMyShortString: PMyShortString;
    ArgPPMyShortString: PPMyShortString;       var VArgPPMyShortString: PPMyShortString;

    ArgTNewhortString: TNewhortString;         var VArgTNewhortString: TNewhortString;
    ArgPNewhortString: PNewhortString;         var VArgPNewhortString: PNewhortString;

    ArgTMyWideString: TMyWideString;           var VArgTMyWideString: TMyWideString;
    ArgPMyWideString: PMyWideString;           var VArgPMyWideString: PMyWideString;
    ArgPPMyWideString: PPMyWideString;         var VArgPPMyWideString: PPMyWideString;

    ArgTNewWideString: TNewWideString;         var VArgTNewWideString: TNewWideString;
    ArgPNewWideString: PNewWideString;         var VArgPNewWideString: PNewWideString;

    ArgTMyString10: TMyString10;               var VArgTMyString10: TMyString10;
    ArgPMyString10: PMyString10;               var VArgPMyString10: PMyString10;
    ArgPPMyString10: PPMyString10;             var VArgPPMyString10: PPMyString10;
  {$ENDIF}

  {$IFDEF FooFunc_LocalVar}
  //var
    VarAnsiString: AnsiString;
    VarTMyAnsiString: TMyAnsiString;
    VarPMyAnsiString: PMyAnsiString;
    VarPPMyAnsiString: PPMyAnsiString;

    VarTNewAnsiString: TNewAnsiString;
    VarPNewAnsiString: PNewAnsiString;

    VarTMyShortString: TMyShortString;
    VarPMyShortString: PMyShortString;
    VarPPMyShortString: PPMyShortString;

    VarTNewhortString: TNewhortString;
    VarPNewhortString: PNewhortString;

    VarTMyWideString: TMyWideString;
    VarPMyWideString: PMyWideString;
    VarPPMyWideString: PPMyWideString;

    VarTNewWideString: TNewWideString;
    VarPNewWideString: PNewWideString;

    VarTMyString10: TMyString10;
    VarPMyString10: PMyString10;
    VarPPMyString10: PPMyString10;

    PVarAnsiString: ^AnsiString;
    PVarShortString: ^ShortString;
    PVarWideString: ^WideString;

    VarString15: string[15];
  {$ENDIF}

  {$IFDEF FooFunc_Body}
  //begin
    VarAnsiString := 'abcd';
    VarTMyAnsiString := ArgTMyAnsiString + '-var';
    VarPMyAnsiString := ArgPMyAnsiString;
    VarPPMyAnsiString := ArgPPMyAnsiString;

    VarTNewAnsiString := ArgTNewAnsiString + '-var';
    VarPNewAnsiString := ArgPNewAnsiString;

    VarTMyShortString := ArgTMyShortString + '-var';
    VarPMyShortString := ArgPMyShortString;
    VarPPMyShortString := ArgPPMyShortString;

    VarTNewhortString := ArgTNewhortString + '-var';
    VarPNewhortString := ArgPNewhortString;

    VarTMyWideString := ArgTMyWideString + '-var';
    VarPMyWideString := ArgPMyWideString;
    VarPPMyWideString := ArgPPMyWideString;

    VarTNewWideString := ArgTNewWideString + '-var';
    VarPNewWideString := ArgPNewWideString;

    VarTMyString10 := ArgTMyString10 + '-var';
    VarPMyString10 := ArgPMyString10;
    VarPPMyString10 := ArgPPMyString10;

    PVarAnsiString := @ArgTMyAnsiString;
    PVarShortString := @ArgTMyShortString;
    PVarWideString := @ArgTMyWideString;

    VarString15 := 'T15' +#10#13 + 'L2' + #13 + 'L3' +#10 +'L4';
  {$ENDIF}
{%endregion FooFunc}

{%region GLOBAL}
  {$IFDEF Global_Type}
  //type
    TMyAnsiString = AnsiString;
    PMyAnsiString = ^TMyAnsiString;
    PPMyAnsiString = ^PMyAnsiString;

    TNewAnsiString = type AnsiString;
    PNewAnsiString = ^TNewAnsiString;


    TMyShortString = ShortString;
    PMyShortString = ^TMyShortString;
    PPMyShortString = ^PMyShortString;

    TNewhortString = type ShortString;
    PNewhortString = ^TNewhortString;

    TMyWideString = WideString;
    PMyWideString = ^TMyWideString;
    PPMyWideString = ^PMyWideString;

    TNewWideString = type WideString;
    PNewWideString = ^TNewWideString;

    TMyString10 = String[10];
    PMyString10 = ^TMyString10;
    PPMyString10 = ^PMyString10;
  {$ENDIF}

  {$IFDEF Global_Var}
  //var
    GlobTMyAnsiString: TMyAnsiString;
    GlobPMyAnsiString: PMyAnsiString;
    GlobPPMyAnsiString: PPMyAnsiString;

    GlobTNewAnsiString: TNewAnsiString;
    GlobPNewAnsiString: PNewAnsiString;

    GlobTMyShortString: TMyShortString;
    GlobPMyShortString: PMyShortString;
    GlobPPMyShortString: PPMyShortString;

    GlobTNewhortString: TNewhortString;
    GlobPNewhortString: PNewhortString;

    GlobTMyWideString: TMyWideString;
    GlobPMyWideString: PMyWideString;
    GlobPPMyWideString: PPMyWideString;

    GlobTNewWideString: TNewWideString;
    GlobPNewWideString: PNewWideString;

    GlobTMyString10: TMyString10;
    GlobPMyString10: PMyString10;
    GlobPPMyString10: PPMyString10;

    PGlobAnsiString: ^AnsiString;
    PGlobShortString: ^ShortString;
    PGlobWideString: ^WideString;

    GlobString15: string[15];
  {$ENDIF}

  {$IFDEF Global_Body}
  //begin
    GlobTMyAnsiString := 'ansi';
    GlobPMyAnsiString := @GlobTMyAnsiString;
    GlobPPMyAnsiString := @GlobPMyAnsiString;

    GlobTNewAnsiString := 'newansi';
    GlobPNewAnsiString := @GlobTNewAnsiString;

    GlobTMyShortString := 'short';
    GlobPMyShortString := @GlobTMyShortString;
    GlobPPMyShortString := @GlobPMyShortString;

    GlobTNewhortString := 'newshort';
    GlobPNewhortString := @GlobTNewhortString;

    GlobTMyWideString := 'wide';
    GlobPMyWideString := @GlobTMyWideString;
    GlobPPMyWideString := @GlobPMyWideString;

    GlobTNewWideString := 'newwide';
    GlobPNewWideString := @GlobTNewWideString;

    GlobTMyString10 := 's10';
    GlobPMyString10 := @GlobTMyString10;
    GlobPPMyString10 := @GlobPMyString10;

    PGlobAnsiString := @GlobTMyAnsiString;
    PGlobShortString := @PGlobAnsiString;
    PGlobWideString := @PGlobShortString;

    GlobString15 := 'g15';
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
    GlobTMyAnsiString := '';
    GlobPMyAnsiString := nil;
    GlobPPMyAnsiString := nil;

    GlobTNewAnsiString := '';
    GlobPNewAnsiString := nil;


    GlobTMyShortString := '';
    GlobPMyShortString := nil;
    GlobPPMyShortString := nil;

    GlobTNewhortString := '';
    GlobPNewhortString := nil;

    GlobTMyWideString := '';
    GlobPMyWideString := nil;
    GlobPPMyWideString := nil;

    GlobTNewWideString := '';
    GlobPNewWideString := nil;

    GlobTMyString10 := '';
    GlobPMyString10 := nil;
    GlobPPMyString10 := nil;
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
    GlobTMyAnsiString,           GlobTMyAnsiString,
    GlobPMyAnsiString,           GlobPMyAnsiString,
    GlobPPMyAnsiString,         GlobPPMyAnsiString,

    GlobTNewAnsiString,         GlobTNewAnsiString,
    GlobPNewAnsiString,         GlobPNewAnsiString,


    GlobTMyShortString,         GlobTMyShortString,
    GlobPMyShortString,         GlobPMyShortString,
    GlobPPMyShortString,       GlobPPMyShortString,

    GlobTNewhortString,         GlobTNewhortString,
    GlobPNewhortString,         GlobPNewhortString,

    GlobTMyWideString,           GlobTMyWideString,
    GlobPMyWideString,           GlobPMyWideString,
    GlobPPMyWideString,         GlobPPMyWideString,

    GlobTNewWideString,         GlobTNewWideString,
    GlobPNewWideString,         GlobPNewWideString,

    GlobTMyString10,               GlobTMyString10,
    GlobPMyString10,               GlobPMyString10,
    GlobPPMyString10,             GlobPPMyString10,
  {$ENDIF}
{%endregion GLOBAL}

