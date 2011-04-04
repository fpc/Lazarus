program ExceptPrg;
{$IFDEF TEST_WITH_HPLUS}
  {$H+}
{$ELSE}
  {$H-}
{$ENDIF}

//{$DEFINE  TEST_NO_EXCEPTION_TYPE}{$DEFINE TEST_NO_POINTER_VAR}{$DEFINE TEST_NO_EXCEPTION_TYPE}{$DEFINE TEST_NO_EXCEPTION_VAR}
uses sysutils;

{$IFnDEF TEST_NO_EXCEPTION_TYPE}
type
  MyESome = class(Exception) end;
  MyEOther = class(Exception) end;
{$ENDIF}

var
  i: integer;
  {$IFnDEF TEST_NO_POINTER_VAR}
  p: pointer; // ensure pointer is in symbol info
  {$ENDIF}
  {$IFnDEF TEST_NO_STRING_VAR}
  s: string[100];
  {$ENDIF}
  {$IFnDEF TEST_NO_EXCEPTION_VAR}
  x: Exception;
  {$ENDIF}

  {$IFnDEF TEST_NO_EXCEPTION_TYPE}
  procedure foo;
    var a: string;
  begin
    a:= 'abc';
    raise MyESome.create(a);
  end;
  {$ENDIF}

begin
  {$IFnDEF TEST_NO_POINTER_VAR}
  p := nil;
  {$ENDIF}
  //foo;

  try
    {$IFnDEF TEST_NO_EXCEPTION_VAR}
    x := Exception.Create('foo');
    raise x;
    {$ELSE}
    raise Exception.Create('foo');
    {$ENDIF}
  except
    on e: Exception do begin
      {$IFnDEF TEST_NO_STRING_VAR}
      s := IntToStr(PtrInt(Pointer(e)));
      writeln(e.Message + s);
      {$ELSE}
      writeln(e.Message);
      {$ENDIF}
    end;
  end;
  writeln(1);
  {$IFnDEF TEST_NO_EXCEPTION_TYPE}
  foo;
  {$ENDIF}
  writeln(2);
end.
