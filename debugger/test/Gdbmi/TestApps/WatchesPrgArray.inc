
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
  {$ENDIF}

  {$IFDEF FooFunc_Local}
  //var
  {$ENDIF}

  {$IFDEF FooFunc_Body}
  //begin
  {$ENDIF}
{%endregion FooFunc}

{%region GLOBAL}
  {$IFDEF Global_Types}
  //type
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

