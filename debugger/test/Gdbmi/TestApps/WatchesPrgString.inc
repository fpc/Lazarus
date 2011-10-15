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

    function SubFoo()():Integer;
      type
        {$DEFINE Sub_FooFunc_LocalType}
      var
        {$DEFINE Sub_FooFunc_LocalVar}
      begin
        {$DEFINE Sub_FooFunc_Body}
      end;

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

  {%region  TYPE}

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

      // Array of char types
      TCharArray = Array of Char;
      TCharArray10B0 = Array [0..9] of Char;
      TCharArray10B1 = Array [1..10] of Char;

      PCharArray = ^TCharArray;
      PCharArray10B0 = ^TCharArray10B0;
      PCharArray10B1 = ^TCharArray10B1;

      PPCharArray = ^PCharArray;
      PPCharArray10B0 = ^PCharArray10B0;
      PPCharArray10B1 = ^PCharArray10B1;

      // TODO: strings in Array
      TMyAnsiStringDArray = Array of AnsiString;
      TMyAnsiStringSArray = Array [3..5] of AnsiString;

// TODO array of ShortString

      // strings in object
      TStringHolderObj = class
      public
        FTMyAnsiString: AnsiString;
        FPMyAnsiString: PMyAnsiString;
        FPPMyAnsiString: PPMyAnsiString;
        FRMyAnsiString: ^TMyAnsiString;

        FTNewAnsiString: TNewAnsiString;
        FPNewAnsiString: PNewAnsiString;
        FRNewAnsiString: ^TNewAnsiString;


        FTMyShortString: ShortString;
        FPMyShortString: PMyShortString;
        FPPMyShortString: PPMyShortString;
        FRMyShortString: ^TMyShortString;

        FTNewhortString: TNewhortString;
        FPNewhortString: PNewhortString;
        FRNewhortString: ^TNewhortString;

        FTMyWideString: WideString;
        FPMyWideString: ^TMyWideString;
        FPPMyWideString: ^PMyWideString;

        FTNewWideString: TNewWideString;
        FPNewWideString: PNewWideString;
        FRNewWideString: ^TNewWideString;

        FTMyString10: TMyString10;
        FPMyString10: PMyString10;
        FPPMyString10: PPMyString10;
        FRMyString10: ^TMyString10;
        FAMyString10: String[10];

        // Array of char types
        FTCharArray: TCharArray;
        FTCharArray10B0: TCharArray10B0;
        FTCharArray10B1: TCharArray10B1;
        FACharArray: Array of Char;
        FACharArray10B0: Array [0..9] of Char;
        FACharArray10B1: Array [1..10] of Char;

        FPCharArray: PCharArray;
        FPCharArray10B0: PCharArray10B0;
        FPCharArray10B1: PCharArray10B1;
        FRCharArray: ^TCharArray;
        FRCharArray10B0: ^TCharArray10B0;
        FRCharArray10B1: ^TCharArray10B1;

        FPPCharArray: PPCharArray;
        FPPCharArray10B0: PPCharArray10B0;
        FPPCharArray10B1: PPCharArray10B1;
        FRPCharArray: ^PCharArray;
        FRPCharArray10B0: ^PCharArray10B0;
        FRPCharArray10B1: ^PCharArray10B1;

        FTMyAnsiStringDArray: TMyAnsiStringDArray;
        FTMyAnsiStringSArray: TMyAnsiStringSArray;
        FAMyAnsiStringDArray: Array of AnsiString;
        FAMyAnsiStringSArray: Array [3..5] of AnsiString;
      end;

      // strings in record
      TStringHolderRec = record
        FTMyAnsiString: AnsiString;
        FPMyAnsiString: PMyAnsiString;
        FPPMyAnsiString: PPMyAnsiString;
        FRMyAnsiString: ^TMyAnsiString;

        FTNewAnsiString: TNewAnsiString;
        FPNewAnsiString: PNewAnsiString;
        FRNewAnsiString: ^TNewAnsiString;


        FTMyShortString: ShortString;
        FPMyShortString: PMyShortString;
        FPPMyShortString: PPMyShortString;
        FRMyShortString: ^TMyShortString;

        FTNewhortString: TNewhortString;
        FPNewhortString: PNewhortString;
        FRNewhortString: ^TNewhortString;

        FTMyWideString: WideString;
        FPMyWideString: ^TMyWideString;
        FPPMyWideString: ^PMyWideString;

        FTNewWideString: TNewWideString;
        FPNewWideString: PNewWideString;
        FRNewWideString: ^TNewWideString;

        FTMyString10: TMyString10;
        FPMyString10: PMyString10;
        FPPMyString10: PPMyString10;
        FRMyString10: ^TMyString10;
        FAMyString10: String[10];

        // Array of char types
        FTCharArray: TCharArray;
        FTCharArray10B0: TCharArray10B0;
        FTCharArray10B1: TCharArray10B1;
        FACharArray: Array of Char;
        FACharArray10B0: Array [0..9] of Char;
        FACharArray10B1: Array [1..10] of Char;

        FPCharArray: PCharArray;
        FPCharArray10B0: PCharArray10B0;
        FPCharArray10B1: PCharArray10B1;
        FRCharArray: ^TCharArray;
        FRCharArray10B0: ^TCharArray10B0;
        FRCharArray10B1: ^TCharArray10B1;

        FPPCharArray: PPCharArray;
        FPPCharArray10B0: PPCharArray10B0;
        FPPCharArray10B1: PPCharArray10B1;
        FRPCharArray: ^PCharArray;
        FRPCharArray10B0: ^PCharArray10B0;
        FRPCharArray10B1: ^PCharArray10B1;

        FTMyAnsiStringDArray: TMyAnsiStringDArray;
        FTMyAnsiStringSArray: TMyAnsiStringSArray;
        FAMyAnsiStringDArray: Array of AnsiString;
        FAMyAnsiStringSArray: Array [3..5] of AnsiString;
      end;

    {$ENDIF}

    {$IFDEF Sub_FooFunc_LocalType}
      AnsiString = Array of Char; // 0 based
    {$ENDIF}

  {%endregion TYPE}

  {%region VARIABLES}

    {$IFDEF FooFunc_Param}
    //procedure FooFunc(
      ArgAnsiString: AnsiString;                 var VArgAnsiString: AnsiString;

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

      ArgPChar: PChar;                           var VarArgPChar: PChar;
      ArgPPChar: PPChar;                         var VarArgPPChar: PPChar;

      // Array of char types
      ArgTCharArray: TCharArray;                 var VArgTCharArray:TCharArray;
      ArgTCharArray10B0: TCharArray10B0;         var VArgTCharArray10B0:TCharArray10B0;
      ArgTCharArray10B1: TCharArray10B1;         var VArgTCharArray10B1:TCharArray10B1;

      ArgPCharArray: PCharArray;                 var VArgPCharArray:PCharArray;
      ArgPCharArray10B0: PCharArray10B0;         var VArgPCharArray10B0:PCharArray10B0;
      ArgPCharArray10B1: PCharArray10B1;         var VArgPCharArray10B1:PCharArray10B1;

      ArgPPCharArray: PPCharArray;               var VArgPPCharArray:PPCharArray;
      ArgPPCharArray10B0: PPCharArray10B0;       var VArgPPCharArray10B0:PPCharArray10B0;
      ArgPPCharArray10B1: PPCharArray10B1;       var VArgPPCharArray10B1:PPCharArray10B1;

      ArgTMyAnsiStringDArray: TMyAnsiStringDArray; var VArgTMyAnsiStringDArray: TMyAnsiStringDArray;
      ArgTMyAnsiStringSArray: TMyAnsiStringSArray; var VArgTMyAnsiStringSArray: TMyAnsiStringSArray;
      ArgTStringHolderObj: TStringHolderObj;       var VArgTStringHolderObj: TStringHolderObj;
      ArgTStringHolderRec: TStringHolderRec;       var VArgTStringHolderRec: TStringHolderRec;

    {$ENDIF}
    {$IFDEF Global_Call_FooFunc}
    //FooFunc(
      GlobAnsiString,             GlobAnsiString2, // AnsiString

      GlobTMyAnsiString,          GlobTMyAnsiString2,
      GlobPMyAnsiString,          GlobPMyAnsiString2,
      GlobPPMyAnsiString,         GlobPPMyAnsiString2,

      GlobTNewAnsiString,         GlobTNewAnsiString2,
      GlobPNewAnsiString,         GlobPNewAnsiString2,


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

      GlobPChar,                  GlobPChar,
      GlobPPChar,                 GlobPPChar,

      // Array of char types
      GlobTCharArray,             GlobTCharArray,
      GlobTCharArray10B0,         GlobTCharArray10B0,
      GlobTCharArray10B1,         GlobTCharArray10B1,

      GlobPCharArray,             GlobPCharArray,
      GlobPCharArray10B0,         GlobPCharArray10B0,
      GlobPCharArray10B1,         GlobPCharArray10B1,

      GlobPPCharArray,            GlobPPCharArray,
      GlobPPCharArray10B0,        GlobPPCharArray10B0,
      GlobPPCharArray10B1,        GlobPPCharArray10B1,

      GlobTMyAnsiStringDArray,    GlobTMyAnsiStringDArray2,
      GlobTMyAnsiStringSArray,    GlobTMyAnsiStringSArray2,
      GlobTStringHolderObj,       GlobTStringHolderObj2,
      GlobTStringHolderRec,       GlobTStringHolderRec2,
    {$ENDIF}

    {$IFDEF FooFunc_LocalVar}
    //var
      VarAnsiString: AnsiString;
      VarTMyAnsiString: TMyAnsiString;
      VarPMyAnsiString: PMyAnsiString;
      VarPPMyAnsiString: PPMyAnsiString;
      VarRAnsiString: ^AnsiString;

      VarTNewAnsiString: TNewAnsiString;
      VarPNewAnsiString: PNewAnsiString;

      VarTMyShortString: TMyShortString;
      VarPMyShortString: PMyShortString;
      VarPPMyShortString: PPMyShortString;
      VarRShortString: ^ShortString;

      VarTNewhortString: TNewhortString;
      VarPNewhortString: PNewhortString;

      VarTMyWideString: TMyWideString;
      VarPMyWideString: PMyWideString;
      VarPPMyWideString: PPMyWideString;
      VarRWideString: ^WideString;

      VarTNewWideString: TNewWideString;
      VarPNewWideString: PNewWideString;

      VarTMyString10: TMyString10;
      VarPMyString10: PMyString10;
      VarPPMyString10: PPMyString10;

      VarString15: string[15];

      VarPChar: PChar;
      VarPPChar: PPChar;
      VarRChar: ^Char;
      VarRPChar: ^PChar;

      // Array of char types
      VarTCharArray: TCharArray;
      VarTCharArray10B0: TCharArray10B0;
      VarTCharArray10B1: TCharArray10B1;
      VarACharArray: Array of Char;
      VarACharArray10B0: Array [0..9] of Char;
      VarACharArray10B1: Array [1..10] of Char;

      VarPCharArray: PCharArray;
      VarPCharArray10B0: PCharArray10B0;
      VarPCharArray10B1: PCharArray10B1;
      VarRCharArray: ^TCharArray;
      VarRCharArray10B0: ^TCharArray10B0;
      VarRCharArray10B1: ^TCharArray10B1;

      VarPPCharArray: PPCharArray;
      VarPPCharArray10B0: PPCharArray10B0;
      VarPPCharArray10B1: PPCharArray10B1;

      VarTMyAnsiStringDArray: TMyAnsiStringDArray;
      VarTMyAnsiStringSArray: TMyAnsiStringSArray;
      VarAMyAnsiStringDArray: Array of AnsiString;
      VarAMyAnsiStringSArray: Array [3..5] of AnsiString;

      VarTStringHolderObj: TStringHolderObj;
      VarTStringHolderRec: TStringHolderRec;

    {$ENDIF}

    {$IFDEF Global_Var}
    //var
      GlobAnsiString,  GlobAnsiString2: AnsiString;
      GlobTMyAnsiString,  GlobTMyAnsiString2: TMyAnsiString;
      GlobTMyAnsiString_P,  GlobTMyAnsiString_P2: TMyAnsiString;
      GlobPMyAnsiString,  GlobPMyAnsiString2: PMyAnsiString;
      GlobPPMyAnsiString,  GlobPPMyAnsiString2: PPMyAnsiString;
      GlobRAnsiString,  GlobRAnsiString2: ^AnsiString;

      GlobTNewAnsiString,  GlobTNewAnsiString2: TNewAnsiString;
      GlobTNewAnsiString_P,  GlobTNewAnsiString_P2: TNewAnsiString;
      GlobPNewAnsiString,  GlobPNewAnsiString2: PNewAnsiString;

      GlobTMyShortString,  GlobTMyShortString2: TMyShortString;
      GlobPMyShortString,  GlobPMyShortString2: PMyShortString;
      GlobPPMyShortString,  GlobPPMyShortString2: PPMyShortString;
      GlobRShortString,  GlobRShortString2: ^ShortString;

      GlobTNewhortString,  GlobTNewhortString2: TNewhortString;
      GlobPNewhortString,  GlobPNewhortString2: PNewhortString;

      GlobTMyWideString,  GlobTMyWideString2: TMyWideString;
      GlobPMyWideString,  GlobPMyWideString2: PMyWideString;
      GlobPPMyWideString,  GlobPPMyWideString2: PPMyWideString;
      GlobRWideString,  GlobRWideString2: ^WideString;

      GlobTNewWideString,  GlobTNewWideString2: TNewWideString;
      GlobPNewWideString,  GlobPNewWideString2: PNewWideString;

      GlobTMyString10,  GlobTMyString102: TMyString10;
      GlobPMyString10,  GlobPMyString102: PMyString10;
      GlobPPMyString10,  GlobPPMyString102: PPMyString10;

      GlobString15,  GlobString152: string[15];

      GlobPChar,  GlobPChar2: PChar;
      GlobPPChar,  GlobPPChar2: PPChar;
      GlobRChar,  GlobRChar2: ^Char;
      GlobRPChar,  GlobRPChar2: ^PChar;

      // Array of char types
      GlobTCharArray,  GlobTCharArray2: TCharArray;
      GlobTCharArray10B0,  GlobTCharArray10B02: TCharArray10B0;
      GlobTCharArray10B1,  GlobTCharArray10B12: TCharArray10B1;
      GlobACharArray,  GlobACharArray2: Array of Char;
      GlobACharArray10B0,  GlobACharArray10B02: Array [0..9] of Char;
      GlobACharArray10B1,  GlobACharArray10B12: Array [1..10] of Char;

      GlobPCharArray,  GlobPCharArray2: PCharArray;
      GlobPCharArray10B0,  GlobPCharArray10B02: PCharArray10B0;
      GlobPCharArray10B1,  GlobPCharArray10B12: PCharArray10B1;
      GlobRCharArray,  GlobRCharArray2: ^TCharArray;
      GlobRCharArray10B0,  GlobRCharArray10B02: ^TCharArray10B0;
      GlobRCharArray10B1,  GlobRCharArray10B12: ^TCharArray10B1;

      GlobPPCharArray,  GlobPPCharArray2: PPCharArray;
      GlobPPCharArray10B0,  GlobPPCharArray10B02: PPCharArray10B0;
      GlobPPCharArray10B1,  GlobPPCharArray10B12: PPCharArray10B1;

      GlobTMyAnsiStringDArray,  GlobTMyAnsiStringDArray2: TMyAnsiStringDArray;
      GlobTMyAnsiStringSArray,  GlobTMyAnsiStringSArray2: TMyAnsiStringSArray;
      GlobAMyAnsiStringDArray,  GlobAMyAnsiStringDArray2: Array of AnsiString;
      GlobAMyAnsiStringSArray,  GlobAMyAnsiStringSArray2: Array [3..5] of AnsiString;

      GlobTStringHolderObj,  GlobTStringHolderObj2: TStringHolderObj;
      GlobTStringHolderRec,  GlobTStringHolderRec2: TStringHolderRec;

    {$ENDIF}

  {%endregion VARIABLES}

  {%region CODE (initilization)}

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

      // In place defined types
      VarRAnsiString := @ArgTMyAnsiString;
      VarRShortString := @ArgTMyShortString;
      VarRWideString := @ArgTMyWideString;

      VarString15 := 'T15' +#10#13 + 'L2' + #13 + 'L3' +#10 +'L4';

/////
      setlength(VarACharArray,2);
      VarACharArray10B0[1] := 'a';
      VarACharArray10B1[1] := 'b';

    {$ENDIF}

    {$IFDEF Global_Body}
    //begin

      GlobAnsiString  := 'Ansi';
      GlobAnsiString2 := 'Ansi 2';

      GlobTMyAnsiString  := 'MyAnsi';
      GlobTMyAnsiString2 := 'MyAnsi 2';

      GlobTMyAnsiString_P  := 'MyAnsi P';
      GlobTMyAnsiString_P2 := 'MyAnsi P2';
      GlobPMyAnsiString := @GlobTMyAnsiString_P;
      GlobPMyAnsiString2 := @GlobTMyAnsiString_P2;
      GlobPPMyAnsiString := @GlobPMyAnsiString;
      GlobPPMyAnsiString2 := @GlobPMyAnsiString2;
      //GlobRAnsiString,  GlobRAnsiString2: ^AnsiString;

      GlobTNewAnsiString := 'NewAnsi';
      GlobTNewAnsiString2 := 'NewAnsi 2';
      GlobTNewAnsiString_P := 'NewAnsi P';
      GlobTNewAnsiString_P2 := 'NewAnsi P2';
      GlobPNewAnsiString := @GlobTNewAnsiString_P;
      GlobPNewAnsiString2 := @GlobTNewAnsiString_P2;

      //GlobTMyShortString,  GlobTMyShortString2: TMyShortString;
      //GlobPMyShortString,  GlobPMyShortString2: PMyShortString;
      //GlobPPMyShortString,  GlobPPMyShortString2: PPMyShortString;
      //GlobRShortString,  GlobRShortString2: ^ShortString;
      //
      //GlobTNewhortString,  GlobTNewhortString2: TNewhortString;
      //GlobPNewhortString,  GlobPNewhortString2: PNewhortString;
      //
      //GlobTMyWideString,  GlobTMyWideString2: TMyWideString;
      //GlobPMyWideString,  GlobPMyWideString2: PMyWideString;
      //GlobPPMyWideString,  GlobPPMyWideString2: PPMyWideString;
      //GlobRWideString,  GlobRWideString2: ^WideString;
      //
      //GlobTNewWideString,  GlobTNewWideString2: TNewWideString;
      //GlobPNewWideString,  GlobPNewWideString2: PNewWideString;
      //
      //GlobTMyString10,  GlobTMyString102: TMyString10;
      //GlobPMyString10,  GlobPMyString102: PMyString10;
      //GlobPPMyString10,  GlobPPMyString102: PPMyString10;
      //
      //GlobString15,  GlobString152: string[15];
      //
      //GlobPChar,  GlobPChar2: PChar;
      //GlobPPChar,  GlobPPChar2: PPChar;
      //GlobRChar,  GlobRChar2: ^Char;
      //GlobRPChar,  GlobRPChar2: ^PChar;
      //
      //// Array of char types
      //GlobTCharArray,  GlobTCharArray2: TCharArray;
      //GlobTCharArray10B0,  GlobTCharArray10B02: TCharArray10B0;
      //GlobTCharArray10B1,  GlobTCharArray10B12: TCharArray10B1;
      //GlobACharArray,  GlobACharArray2: Array of Char;
      //GlobACharArray10B0,  GlobACharArray10B02: Array [0..9] of Char;
      //GlobACharArray10B1,  GlobACharArray10B12: Array [1..10] of Char;
      //
      //GlobPCharArray,  GlobPCharArray2: PCharArray;
      //GlobPCharArray10B0,  GlobPCharArray10B02: PCharArray10B0;
      //GlobPCharArray10B1,  GlobPCharArray10B12: PCharArray10B1;
      //GlobRCharArray,  GlobRCharArray2: ^TCharArray;
      //GlobRCharArray10B0,  GlobRCharArray10B02: ^TCharArray10B0;
      //GlobRCharArray10B1,  GlobRCharArray10B12: ^TCharArray10B1;
      //

      //GlobPPCharArray,  GlobPPCharArray2: PPCharArray;
      //GlobPPCharArray10B0,  GlobPPCharArray10B02: PPCharArray10B0;
      //GlobPPCharArray10B1,  GlobPPCharArray10B12: PPCharArray10B1;
      //


      SetLength(GlobTMyAnsiStringDArray, 3);
      SetLength(GlobTMyAnsiStringDArray2, 3);
      GlobTMyAnsiStringDArray[0] := 'DArray1 Str0';
      GlobTMyAnsiStringDArray[1] := 'DArray1 Str1';
      GlobTMyAnsiStringDArray2[0] := 'DArray2 Str0';
      GlobTMyAnsiStringDArray2[0] := 'DArray2 Str1';

      GlobTMyAnsiStringSArray[3] := 'SArray1 Str3';
      GlobTMyAnsiStringSArray[4] := 'SArray1 Str4';
      GlobTMyAnsiStringSArray2[3] := 'SArray2 Str3';
      GlobTMyAnsiStringSArray2[4] := 'SArray2 Str4';

      //GlobAMyAnsiStringDArray,  GlobAMyAnsiStringDArray2: Array of AnsiString;
      //GlobAMyAnsiStringSArray,  GlobAMyAnsiStringSArray2: Array [3..5] of AnsiString;
      //

      GlobTStringHolderObj := TStringHolderObj.Create;
      GlobTStringHolderObj.FTMyAnsiString := 'Obj1 MyAnsi';
      GlobTStringHolderObj.FTMyShortString := 'Obj1 Short';

      GlobTStringHolderObj2 := TStringHolderObj.Create;
      GlobTStringHolderObj2.FTMyAnsiString := 'Obj2 MyAnsi';
      GlobTStringHolderObj2.FTMyShortString := 'Obj2 Short';

      GlobTStringHolderRec.FTMyAnsiString := 'Rec1 MyAnsi';
      GlobTStringHolderRec.FTMyShortString := 'Rec1 Short';
      GlobTStringHolderRec2.FTMyAnsiString := 'Rec2 MyAnsi';
      GlobTStringHolderRec2.FTMyShortString := 'Rec2 Short';





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

      GlobPChar := @GlobTMyAnsiString[2];
      GlobPPChar := @GlobPChar;

      // Array of char types
      SetLength(GlobTCharArray, 3);
      GlobTCharArray[0] := 'A'; GlobTCharArray[1] := 'B'; GlobTCharArray[2] := 'C';
      for i := 0 to 9 do begin
        GlobTCharArray10B0[i]   := chr(ord('D')+i);
        GlobTCharArray10B1[i+1] := chr(ord('d')+i);
      end;

      // In place defined types
      GlobRAnsiString := @GlobTMyAnsiString;
      GlobRShortString := @GlobRAnsiString; // XXXXXXXXXXXXXXX
      GlobRWideString := @GlobRShortString; // XXXXXXXXXXXXXXX

      GlobString15 := 'g15';
    {$ENDIF}

    {$IFDEF Global_Body_NIL}
    //begin
      GlobAnsiString := '';
      GlobAnsiString2 := '';

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

      GlobPChar := nil;
      GlobPPChar := nil;
    {$ENDIF}

  {%endregion CODE (initilization)}



