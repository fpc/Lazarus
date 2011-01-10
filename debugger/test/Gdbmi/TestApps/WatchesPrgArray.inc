
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
  {$ENDIF}

  {$IFDEF FooFunc_Local}
  //var
    VarDynIntArray: TDynIntArray;
    VarStatIntArray: TStatIntArray;
    VarDynIntArrayA: Array of Integer;
    VarStatIntArrayA: Array [5..9] of Integer;
  {$ENDIF}

  {$IFDEF FooFunc_Body}
  //begin
    SetLength(VarDynIntArray,2);
    SetLength(VarDynIntArrayA,2);
    VarStatIntArray[5] := 1;
    VarStatIntArrayA[5] := 1;
  {$ENDIF}
{%endregion FooFunc}

{%region GLOBAL}
  {$IFDEF Global_Types}
  //type
    TDynIntArray = Array of Integer;
    TStatIntArray = Array [5..9] of Integer;
  {$ENDIF}

  {$IFDEF Global_Var}
  //var
  {$ENDIF}

  {$IFDEF Global_Body}
  //begin
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
  {$ENDIF}
{%endregion GLOBAL}

