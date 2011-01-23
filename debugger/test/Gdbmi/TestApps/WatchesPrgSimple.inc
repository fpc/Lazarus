
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
    ArgByte: Byte;                       var VArgByte: Byte;
    ArgWord: Word;                       var VArgWord: Word;
    ArgLongWord: LongWord;               var VArgLongWord: LongWord;
    ArgQWord: QWord;                     var VArgQWord: QWord;

    ArgShortInt: ShortInt;               var VArgShortInt: ShortInt;
    ArgSmallInt: SmallInt;               var VArgSmallInt: SmallInt;
    ArgInt: Integer;                     var VArgInt: Integer;
    ArgInt64: Int64;                     var VArgInt64: Int64;

    ArgPByte: PByte;                     var VArgPByte: PByte;
    ArgPWord: PWord;                     var VArgPWord: PWord;
    ArgPLongWord: PLongWord;             var VArgPLongWord: PLongWord;
    ArgPQWord: PQWord;                   var VArgPQWord: PQWord;

    ArgPShortInt: PShortInt;             var VArgPShortInt: PShortInt;
    ArgPSmallInt: PSmallInt;             var VArgPSmallInt: PSmallInt;
    ArgPInt: PInteger;                   var VArgPInt: PInteger;
    ArgPInt64: PInt64;                   var VArgPInt64: PInt64;

    ArgPointer: Pointer;                 var VArgPointer: Pointer;
    ArgPPointer: PPointer;               var VArgPPointer: PPointer;

    ArgDouble: Double;                   var VArgDouble: Double;
    ArgExtended: Extended;               var VArgExtended: Extended;
  {$ENDIF}

  {$IFDEF FooFunc_Local}
  //var
    VarByte: Byte;
    VarWord: Word;
    VarLongWord: LongWord;
    VarQWord: QWord;

    VarShortInt: ShortInt;
    varSmallInt: SmallInt;
    VarInt: Integer;
    VarInt64: Int64;

    VarPByte: PByte;
    VarPWord: PWord;
    VarPLongWord: PLongWord;
    VarPQWord: PQWord;

    VarPShortInt: PShortInt;
    varPSmallInt: PSmallInt;
    VarPInt: PInteger;
    VarPInt64: PInt64;

    PVarByte: ^Byte;
    PVarWord: ^Word;
    PVarLongWord: ^LongWord;
    PVarQWord: ^QWord;

    PVarShortInt: ^ShortInt;
    PvarSmallInt: ^SmallInt;
    PVarInt: ^Integer;
    PVarInt64: ^Int64;

    VarPointer: Pointer;
    VarPPointer: PPointer;
    PVarPointer: ^Pointer;

    VarDouble: Double;
    VarExtended: Extended;
    PVarDouble: ^Double;
    PVarExtended: ^Extended;
  {$ENDIF}

  {$IFDEF FooFunc_Body}
  //begin
    VarByte := ArgByte + 100;
    VarWord := ArgWord + 100;
    VarLongWord := ArgLongWord + 100;
    VarQWord := ArgQWord + 100;

    VarShortInt := ArgShortInt + 100;
    VarSmallInt := ArgSmallInt + 100;
    VarInt := ArgInt + 100;
    VarInt64 := ArgInt64 + 100;

    VarPByte := ArgPByte;
    VarPWord := ArgPWord;
    VarPLongWord := ArgPLongWord;
    VarPQWord := ArgPQWord;

    VarPShortInt := ArgPShortInt;
    VarPSmallInt := ArgPSmallInt;
    VarPInt := ArgPInt;
    VarPInt64 := ArgPInt64;

    PVarByte := @ArgByte;
    PVarWord := @ArgWord;
    PVarLongWord := @ArgLongWord;
    PVarQWord := @ArgQWord;

    PVarShortInt := @ArgShortInt;
    PVarSmallInt := @ArgSmallInt;
    PVarInt := @ArgInt;
    PVarInt64 := @ArgInt64;

    VarPointer := ArgPointer;
    VarPPointer := ArgPPointer;
    PVarPointer := ArgPointer;

    VarDouble := ArgDouble;
    VarExtended := ArgExtended;
    PVarDouble := @ArgDouble;
    PVarExtended := @ArgExtended;
  {$ENDIF}
{%endregion FooFunc}

{%region GLOBAL}
  {$IFDEF Global_Var}
  //var
    GlobByte: Byte;
    GlobWord: Word;
    GlobLongWord: LongWord;
    GlobQWord: QWord;

    GlobShortInt: ShortInt;
    GlobSmallInt: SmallInt;
    GlobInt: Integer;
    GlobInt64: Int64;

    GlobPByte: PByte;
    GlobPWord: PWord;
    GlobPLongWord: PLongWord;
    GlobPQWord: PQWord;

    GlobPShortInt: PShortInt;
    GlobPSmallInt: PSmallInt;
    GlobPInt: PInteger;
    GlobPInt64: PInt64;

    PGlobByte: ^Byte;
    PGlobWord: ^Word;
    PGlobLongWord: ^LongWord;
    PGlobQWord: ^QWord;

    PGlobShortInt: ^ShortInt;
    PGlobSmallInt: ^SmallInt;
    PGlobInt: ^Integer;
    PGlobInt64: ^Int64;

    GlobPointer: Pointer;
    GlobPPointer: PPointer;
    PGlobPointer: ^Pointer;

    GlobDouble: Double;
    GlobExtended: Extended;
    PGlobDouble: ^Double;
    PGlobExtended: ^Extended;
  {$ENDIF}

  {$IFDEF Global_Body}
  //begin
    GlobByte := 25;
    GlobWord := 26;
    GlobLongWord := 27;
    GlobQWord := 28;

    GlobShortInt := 35;
    GlobSmallInt := 36;
    GlobInt := 37;
    GlobInt64 := 38;

    GlobPByte := @GlobByte;
    GlobPWord := @GlobWord;
    GlobPLongWord := @GlobLongWord;
    GlobPQWord := @GlobQWord;

    GlobPShortInt := @GlobShortInt;
    GlobPSmallInt := @GlobSmallInt;
    GlobPInt := @GlobInt;
    GlobPInt64 := @GlobInt64;

    PGlobByte := @GlobByte;
    PGlobWord := @GlobWord;
    PGlobLongWord := @GlobLongWord;
    PGlobQWord := @GlobQWord;

    PGlobShortInt := @GlobShortInt;
    PGlobSmallInt := @GlobSmallInt;
    PGlobInt := @GlobInt;
    PGlobInt64 := @GlobInt64;

    GlobPointer := @GlobByte;
    GlobPPointer := @GlobPointer;
    PGlobPointer := @GlobPointer;

    GlobDouble := 1.123;
    GlobExtended := 2.345;
    PGlobDouble := @GlobDouble;
    PGlobExtended := @GlobExtended;
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
    GlobByte := 0;
    GlobWord := 0;
    GlobLongWord := 0;
    GlobQWord := 0;

    GlobShortInt := 0;
    GlobSmallInt := 0;
    GlobInt := 0;
    GlobInt64 := 0;

    GlobPByte := nil;
    GlobPWord := nil;
    GlobPLongWord := nil;
    GlobPQWord := nil;

    GlobPShortInt := nil;
    GlobPSmallInt := nil;
    GlobPInt := nil;
    GlobPInt64 := nil;

    GlobPointer := nil;
    GlobPPointer := nil;

    GlobDouble := 0;
    GlobExtended := 0;
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
      GlobByte,                       GlobByte,
      GlobWord,                       GlobWord,
      GlobLongWord,               GlobLongWord,
      GlobQWord,                     GlobQWord,

      GlobShortInt,               GlobShortInt,
      GlobSmallInt,               GlobSmallInt,
      GlobInt,                     GlobInt,
      GlobInt64,                     GlobInt64,

      GlobPByte,                     GlobPByte,
      GlobPWord,                     GlobPWord,
      GlobPLongWord,             GlobPLongWord,
      GlobPQWord,                   GlobPQWord,

      GlobPShortInt,             GlobPShortInt,
      GlobPSmallInt,             GlobPSmallInt,
      GlobPInt,                   GlobPInt,
      GlobPInt64,                   GlobPInt64,

      GlobPointer,                 GlobPointer,
      GlobPPointer,               GlobPPointer,

      GlobDouble,                   GlobDouble,
      GlobExtended,               GlobExtended,
  {$ENDIF}
{%endregion GLOBAL}

