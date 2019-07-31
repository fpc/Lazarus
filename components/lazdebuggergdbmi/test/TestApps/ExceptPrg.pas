program ExceptPrg;  {$INLINE OFF}
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
    a:= 'abc üü {[''[{ \n\t'#13#9'#';
    raise MyESome.create(a);
  end;
  {$ENDIF}

  {$IFDEF TEST_EXCEPTION_AT}
  procedure Bar;
  begin
    raise Exception.create('at1') at
    get_caller_addr(get_caller_frame(get_frame)),
    get_caller_frame(get_caller_frame(get_frame));
  end;

  procedure Bar1;
  begin
    Bar();
  end;

  procedure Bar2;
  begin
    Bar1();
  end;

  procedure BarBar;
  begin
    raise Exception.create('at2') at
    get_caller_addr(get_frame),
    get_caller_frame(get_frame);
  end;

  procedure BarBar1;
  begin
    BarBar();
  end;

  procedure BarBar2;
  begin
    BarBar1();
  end;
  {$ENDIF}

  {$IFDEF TEST_RUNERR}
  {$R+}
  procedure Run;
  var a: array of integer;
  begin
    SetLength(a, 2);
    a[0] := -2;
    a[1] := a[length(a)-a[0]];
  end;

  procedure Run1;
  begin
    Run();
  end;
  {$ENDIF}

  {$IFDEF TEST_ASSERT}
  {$C+}
  procedure check;
  begin
    Assert(false, 'denied');
  end;

  procedure check1;
  begin
    check();
  end;
  {$ENDIF}
begin
  IsConsole := true; // dont show unhandled exceptions
  ExceptProc := NIL;
  {$IFnDEF TEST_NO_POINTER_VAR}
  p := nil;
  {$ENDIF}
  //foo;

  {$IFnDEF TEST_SKIP_EXCEPTION_1}
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
      Freemem(GetMem(1)); //writeln(e.Message + s);
      {$ELSE}
      Freemem(GetMem(1)); //writeln(e.Message);
      {$ENDIF}
    end;
  end;
  Freemem(GetMem(1));
  {$ENDIF}

  {$IFDEF TEST_EXCEPTION_AT}
  try
  Bar2();
  except end;
  try
  BarBar2();
  except end;
  {$ENDIF}

  {$IFDEF TEST_RUNERR}
  Run1();
  {$ENDIF}

  {$IFDEF TEST_ASSERT}
  check1();
  {$ENDIF}

  {$IFnDEF TEST_NO_EXCEPTION_TYPE}
  try
  foo;
  except end;
  {$ENDIF}

  Freemem(GetMem(1));
end.
