
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
    ArgVariantInt: Variant;     var VArgVariantInt: Variant;
    ArgVariantString: Variant;  var VArgVariantString: Variant;
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
    GlobVariantInt, GlobVariantString: Variant;
  {$ENDIF}

  {$IFDEF Global_Body}
  //begin
    GlobVariantInt := 5;
    GlobVariantString := 'v';
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
    GlobVariantInt := 0;
    GlobVariantString := '';
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
    GlobVariantInt,    GlobVariantInt,
    GlobVariantString, GlobVariantString,
  {$ENDIF}
{%endregion GLOBAL}

