unit EMSSelfTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, SynEditTypes, SynEditKeyCmds, LazLoggerBase,
  IDECommands, EMScriptClasses, EMScriptMacro, Clipbrd, Dialogs, Controls,
  uPSCompiler, uPSRuntime, uPSUtils, uPSDebugger, uPSR_std, uPSC_std;

type

  TEMScriptSelfTestException = Exception;

  { TEMSTPSTestExec }

  TEMSTPSTestExec = class(TEMSTPSExec)
  protected
    procedure AddFuncToExec; override;
  public
  end;

  { TEMSPSTestPascalCompiler }

  TEMSPSTestPascalCompiler = class(TEMSPSPascalCompiler)
  private
    FNextOnUses: TPSOnUses;
  public
    constructor Create;
  end;

  { TEMSelfTestEditorMacro }

  TEMSelfTestEditorMacro = class(TEMSEditorMacro)
  public
    constructor Create(aOwner: TComponent); override;
  end;


  function DoSelfTest: Boolean;

implementation

{%region RegisterSelfTests}

var
  TestResultA: integer;
  TestResultInt1, TestResultInt2: integer;
  TestInputInt1, TestInputInt2: integer;
  TestResultBool1, TestResultBool2: boolean;
  TestInputBool1, TestInputBool2: boolean;
  TestResultStr1, TestResultStr2: String;
  TestInputStr1, TestInputStr2: String;

function test_ord_mt(AType: TMsgDlgType): Integer;
begin
  Result := ord(AType);
  TestResultA := Result;
end;

function test_ord_mb(ABtn: TMsgDlgBtn): Integer;
begin
  Result := ord(ABtn);
  TestResultA := Result;
end;

procedure test_int1(AValue: Integer);
begin
  TestResultInt1 := AValue;
end;

procedure test_int2(AValue: Integer);
begin
  TestResultInt2 := AValue;
end;

function test_getint1: Integer;
begin
  Result := TestInputInt1;
end;

function test_getint2: Integer;
begin
  Result := TestInputInt2;
end;

procedure test_bool1(AValue: boolean);
begin
  TestResultBool1 := AValue;
end;

procedure test_bool2(AValue: boolean);
begin
  TestResultBool2 := AValue;
end;

function test_getbool1: Boolean;
begin
  Result := TestInputBool1;
end;

function test_getbool2: Boolean;
begin
  Result := TestInputBool2;
end;

procedure test_point(AValue: TPoint);
begin
  TestResultInt1 := AValue.X;
  TestResultInt2 := AValue.Y;
end;

function test_getpoint: TPoint;
begin
  Result.X := TestInputInt1;
  Result.Y := TestInputInt2;
end;

procedure test_str1(AValue: String);
begin
  TestResultStr1 := AValue;
end;

procedure test_str2(AValue: String);
begin
  TestResultStr2 := AValue;
end;

function test_getstr1: String;
begin
  Result := TestInputStr1;
end;

function test_getstr2: String;
begin
  Result := TestInputStr2;
end;

const
  Decltest_ord_mt   = 'function test_ord_mt(AType: TMsgDlgType): Integer;';
  Decltest_ord_mb   = 'function test_ord_mb(ABtn: TMsgDlgBtn): Integer;';
  Decltest_int1     = 'procedure test_int1(AValue: Integer);';
  Decltest_int2     = 'procedure test_int2(AValue: Integer);';
  Decltest_getint1  = 'function test_getint1: Integer;';
  Decltest_getint2  = 'function test_getint2: Integer;';
  Decltest_bool1    = 'procedure test_bool1(AValue: Boolean);';
  Decltest_bool2    = 'procedure test_bool2(AValue: Boolean);';
  Decltest_getbool1 = 'function test_getbool1: Boolean;';
  Decltest_getbool2 = 'function test_getbool2: Boolean;';
  Decltest_point    = 'procedure test_point(AValue: TPoint);';
  Decltest_getpoint = 'function test_getpoint: TPoint;';
  Decltest_str1     = 'procedure test_str1(AValue: String);';
  Decltest_str2     = 'procedure test_str2(AValue: String);';
  Decltest_getstr1  = 'function test_getstr1: String;';
  Decltest_getstr2  = 'function test_getstr2: String;';
  Functest_ord_mt:    function(AType: TMsgDlgType): Integer = @test_ord_mt;
  Functest_ord_mb:    function(ABtn: TMsgDlgBtn): Integer = @test_ord_mb;
  Proctest_int1:      procedure (AValue: Integer) = @test_int1;
  Proctest_int2:      procedure (AValue: Integer) = @test_int2;
  Proctest_getint1:   function: Integer = @test_getint1;
  Proctest_getint2:   function: Integer = @test_getint2;
  Proctest_bool1:     procedure (AValue: Boolean) = @test_bool1;
  Proctest_bool2:     procedure (AValue: Boolean) = @test_bool2;
  Proctest_getbool1:  function: Boolean = @test_getbool1;
  Proctest_getbool2:  function: Boolean = @test_getbool2;
  Proctest_point:     procedure (AValue: TPoint)  = @test_point;
  Proctest_getpoint:  function: TPoint = @test_getpoint;
  Proctest_str1:      procedure (AValue: String)  = @test_str1;
  Proctest_str2:      procedure (AValue: String)  = @test_str2;
  Proctest_getstr1:   function: String = @test_getstr1;
  Proctest_getstr2:   function: String = @test_getstr2;

{$IFDEF PasMacroNoNativeCalls}
const
  Id_test_ord_mb    = 901;
  Id_test_ord_mt    = 902;
  Id_test_int1      = 903;
  Id_test_int2      = 904;
  Id_test_getint1   = 905;
  Id_test_getint2   = 906;
  Id_test_bool1     = 907;
  Id_test_bool2     = 908;
  Id_test_getbool1  = 909;
  Id_test_getbool2  = 910;
  Id_test_point     = 911;
  Id_test_getpoint  = 912;
  Id_test_str1      = 913;
  Id_test_str2      = 914;
  Id_test_getstr1   = 915;
  Id_test_getstr2   = 916;

function ExecTestHandler({%H-}Caller: TPSExec; p: TPSExternalProcRec;
  {%H-}Global, Stack: TPSStack): Boolean;
var
  data: PPoint;
begin
  Result := True;
  case Longint(p.Ext1) of
    Id_test_ord_mb: begin // test_ord_mb(ABtn: TMsgDlgBtn): Integer;
        if Stack.Count < 2 then raise TEMScriptBadParamException.Create('Invalid param count for "test_ord_mb"');
        Stack.SetInt(-1, test_ord_mb(TMsgDlgBtn(Stack.GetUInt(-2))) );
      end;
    Id_test_ord_mt: begin // test_ord_mt(AType: TMsgDlgType): Integer;
        if Stack.Count < 2 then raise TEMScriptBadParamException.Create('Invalid param count for "test_ord_mt"');
        //  Stack[Stack.Count-2]^.FType.ExportName = 'TMSGDLGTYPE'
        Stack.SetInt(-1, test_ord_mt(TMsgDlgType(Stack.GetUInt(-2))) );
      end;
    Id_test_int1: begin // test_int1(AValue: Integer);
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_int1"');
        test_int1(Stack.GetUInt(-1));
      end;
    Id_test_int2: begin // test_int2(AValue: Integer);
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_int2"');
        test_int2(Stack.GetUInt(-1));
      end;
    Id_test_getint1: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getint1"');
        Stack.SetInt(-1, test_getint1());
      end;
    Id_test_getint2: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getint2"');
        Stack.SetInt(-1, test_getint2());
      end;
    Id_test_bool1: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_bool1()"');
        test_bool1(Stack.GetBool(-1));
      end;
    Id_test_bool2: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_bool2()"');
        test_bool2(Stack.GetBool(-1));
      end;
    Id_test_getbool1: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getbool1"');
        Stack.SetBool(-1, test_getbool1());
      end;
    Id_test_getbool2: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getbool2"');
        Stack.SetBool(-1, test_getbool2());
      end;
    Id_test_point: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_point()"');
        test_point(GetPointFromStack(Stack, -1));
      end;
    Id_test_getpoint: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getpoint"');
        data := GetVarPointFromStack(Stack, -1);
        TPoint(data^) := test_getpoint;
      end;
    Id_test_str1: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_str1()"');
        test_str1(Stack.GetAnsiString(-1));
      end;
    Id_test_str2: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_str2()"');
        test_str2(Stack.GetAnsiString(-1));
      end;
    Id_test_getstr1: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getstr1"');
        Stack.SetAnsiString(-1, test_getstr1());
      end;
    Id_test_getstr2: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getstr2"');
        Stack.SetAnsiString(-1, test_getstr2());
      end;
    else
      Result := False;
  end;
end;
{$ENDIF}

procedure CompRegisterSelfTests(AComp: TPSPascalCompiler);
begin
  // for tests
  AComp.AddDelphiFunction(Decltest_ord_mb);
  AComp.AddDelphiFunction(Decltest_ord_mt);
  AComp.AddDelphiFunction(Decltest_int1);
  AComp.AddDelphiFunction(Decltest_int2);
  AComp.AddDelphiFunction(Decltest_getint1);
  AComp.AddDelphiFunction(Decltest_getint2);
  AComp.AddDelphiFunction(Decltest_bool1);
  AComp.AddDelphiFunction(Decltest_bool2);
  AComp.AddDelphiFunction(Decltest_getbool1);
  AComp.AddDelphiFunction(Decltest_getbool2);
  AComp.AddDelphiFunction(Decltest_point);
  AComp.AddDelphiFunction(Decltest_getpoint);
  AComp.AddDelphiFunction(Decltest_str1);
  AComp.AddDelphiFunction(Decltest_str2);
  AComp.AddDelphiFunction(Decltest_getstr1);
  AComp.AddDelphiFunction(Decltest_getstr2);
end;

procedure ExecRegisterSelfTests(AExec: TEMSTPSExec);
begin
  // for tests
  {$IFnDEF PasMacroNoNativeCalls}
  AExec.RegisterDelphiFunction(Functest_ord_mb,    'test_ord_mb',   cdRegister);
  AExec.RegisterDelphiFunction(Functest_ord_mt,    'test_ord_mt',   cdRegister);
  AExec.RegisterDelphiFunction(Proctest_int1,      'test_int1',     cdRegister);
  AExec.RegisterDelphiFunction(Proctest_int2,      'test_int2',     cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getint1,   'test_getint1',  cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getint2,   'test_getint2',  cdRegister);
  AExec.RegisterDelphiFunction(Proctest_bool1,     'test_bool1',    cdRegister);
  AExec.RegisterDelphiFunction(Proctest_bool2,     'test_bool2',    cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getbool1,  'test_getbool1', cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getbool2,  'test_getbool2', cdRegister);
  AExec.RegisterDelphiFunction(Proctest_point,     'test_point',    cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getpoint,  'test_getpoint', cdRegister);
  AExec.RegisterDelphiFunction(Proctest_str1,      'test_str1',     cdRegister);
  AExec.RegisterDelphiFunction(Proctest_str2,      'test_str2',     cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getstr1,   'test_getstr1',  cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getstr2,   'test_getstr2',  cdRegister);
  {$ELSE}
  AExec.RegisterFunctionName('test_ord_mb',       @ExecTestHandler, Pointer(Id_test_ord_mb), nil);
  AExec.RegisterFunctionName('test_ord_mt',       @ExecTestHandler, Pointer(Id_test_ord_mt), nil);
  AExec.RegisterFunctionName('test_int1',         @ExecTestHandler, Pointer(Id_test_int1), nil);
  AExec.RegisterFunctionName('test_int2',         @ExecTestHandler, Pointer(Id_test_int2), nil);
  AExec.RegisterFunctionName('test_getint1',      @ExecTestHandler, Pointer(Id_test_getint1), nil);
  AExec.RegisterFunctionName('test_getint2',      @ExecTestHandler, Pointer(Id_test_getint2), nil);
  AExec.RegisterFunctionName('test_bool1',        @ExecTestHandler, Pointer(Id_test_bool1), nil);
  AExec.RegisterFunctionName('test_bool2',        @ExecTestHandler, Pointer(Id_test_bool2), nil);
  AExec.RegisterFunctionName('test_getbool1',     @ExecTestHandler, Pointer(Id_test_getbool1), nil);
  AExec.RegisterFunctionName('test_getbool2',     @ExecTestHandler, Pointer(Id_test_getbool2), nil);
  AExec.RegisterFunctionName('test_point',        @ExecTestHandler, Pointer(Id_test_point), nil);
  AExec.RegisterFunctionName('test_getpoint',     @ExecTestHandler, Pointer(Id_test_getpoint), nil);
  AExec.RegisterFunctionName('test_str1',         @ExecTestHandler, Pointer(Id_test_str1), nil);
  AExec.RegisterFunctionName('test_str2',         @ExecTestHandler, Pointer(Id_test_str2), nil);
  AExec.RegisterFunctionName('test_getstr1',      @ExecTestHandler, Pointer(Id_test_getstr1), nil);
  AExec.RegisterFunctionName('test_getstr2',      @ExecTestHandler, Pointer(Id_test_getstr2), nil);
  {$ENDIF}
end;

{%endregion RegisterSelfTests}


{ TEMSPSTestPascalCompiler }

function CompilerOnUses(Sender: TPSPascalCompiler; const Name: TbtString): Boolean;
var
  S: TEMSPSTestPascalCompiler;
begin
  S :=  (Sender as TEMSPSTestPascalCompiler);
  Result := assigned(S.FNextOnUses) and S.FNextOnUses(Sender, Name);

  if Result and (Name = 'SYSTEM') then
  begin
    CompRegisterSelfTests(S);

    Result := True;
  end;
end;

constructor TEMSPSTestPascalCompiler.Create;
begin
  inherited Create;
  FNextOnUses := OnUses;
  OnUses := @CompilerOnUses;

end;

{ TEMSTPSTestExec }

procedure TEMSTPSTestExec.AddFuncToExec;
begin
  inherited AddFuncToExec;
  ExecRegisterSelfTests(Self);
end;

{ TEMSelfTestEditorMacro }

constructor TEMSelfTestEditorMacro.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Compiler := TEMSPSTestPascalCompiler.Create;
  Exec := TEMSTPSTestExec.Create;
end;

function DoSelfTest: Boolean;
var
  m: TEMSEditorMacro;

  procedure RunMacro(AText: String);
  begin
    m.SetFromSource(AText);
    m.PlaybackMacro(nil);
  end;

  procedure AssertEQ(Msg: String; Exp, Got: String); overload;
  begin
    if not(Got = Exp) then
      raise TEMScriptSelfTestException.Create(Format('%s [Exp: "%s" / Got: "%s"]', [Msg, Exp, Got]));
  end;
  procedure AssertEQ(Msg: String; Exp, Got: Integer); overload;
  begin
    if not(Got = Exp) then
      raise TEMScriptSelfTestException.Create(Format('%s [Exp: %d / Got: %d]', [Msg, Exp, Got]));
  end;
  procedure AssertEQ(Msg: String; Exp, Got: Boolean); overload;
  begin
    if not(Got = Exp) then
      raise TEMScriptSelfTestException.Create(Format('%s [Exp: %s / Got: %s]', [Msg, dbgs(Exp), dbgs(Got)]));
  end;

begin
  Result := True;
  try
    try
    m := TEMSelfTestEditorMacro.Create(nil);


    // test_int1
    TestResultInt1 := 99;
    RunMacro('begin' +
             '  test_int1(42);' +
             'end.');
    AssertEQ('Failed int param (42)', 42, TestResultInt1);

    RunMacro('begin' +
             '  test_int1(-3);' +
             'end.');
    AssertEQ('Failed int param (-3)', -3, TestResultInt1);

    TestInputInt1 := 1001;
    TestInputInt2 := 2002;
    RunMacro('var i: Integer;' +
             'begin' +
             '  i := test_getint1' +
             '  test_int1(i);' +
             '  test_int2(test_getint2);' +
             'end.');
    AssertEQ('Failed getint1', 1001, TestResultInt1);
    AssertEQ('Failed getint2', 2002, TestResultInt2);


    // test_bool
    TestResultBool1 := False;
    TestResultBool2 := False;
    RunMacro('begin' +
             '  test_bool1(False);' +
             '  test_bool2(False);' +
             'end.');
    AssertEQ('Failed bool param 1 (f,f => f,f)', False, TestResultBool1);
    AssertEQ('Failed bool param 2 (f,f => f,f)', False, TestResultBool2);

    TestResultBool1 := False;
    TestResultBool2 := False;
    RunMacro('begin' +
             '  test_bool1(True);' +
             '  test_bool2(True);' +
             'end.');
    AssertEQ('Failed bool param 1 (f,f => t,t)', True, TestResultBool1);
    AssertEQ('Failed bool param 2 (f,f => t,t)', True, TestResultBool2);

    TestResultBool1 := True;
    TestResultBool2 := True;
    RunMacro('begin' +
             '  test_bool1(False);' +
             '  test_bool2(False);' +
             'end.');
    AssertEQ('Failed bool param 1 (t,t => f,f)', False, TestResultBool1);
    AssertEQ('Failed bool param 2 (t,t => f,f)', False, TestResultBool2);

    TestResultBool1 := True;
    TestResultBool2 := True;
    RunMacro('begin' +
             '  test_bool1(True);' +
             '  test_bool2(True);' +
             'end.');
    AssertEQ('Failed bool param 1 (t,t => t,t)', True, TestResultBool1);
    AssertEQ('Failed bool param 2 (t,t => t,t)', True, TestResultBool2);

    TestResultBool1 := True;
    TestResultBool2 := False;
    RunMacro('begin' +
             '  test_bool1(False);' +
             '  test_bool2(True);' +
             'end.');
    AssertEQ('Failed bool param 1 (t,f => f,t)', False, TestResultBool1);
    AssertEQ('Failed bool param 2 (t,f => f,t)', True, TestResultBool2);

    TestInputBool1 := True;
    TestInputBool2 := False;
    RunMacro('var i: Boolean;' +
             'begin' +
             '  i := test_getbool1' +
             '  test_bool1(i);' +
             '  test_bool2(test_getbool2);' +
             'end.');
    AssertEQ('Failed getbool1', True, TestResultBool1);
    AssertEQ('Failed getbool2', False, TestResultBool2);


    // size_of(point)
    TestResultInt1 := -1;
    RunMacro('var p: TPoint;' +
             'begin' +
             '  test_int1(SizeOf(p));' +
             'end.');
    AssertEQ('Failed int param (SizeOf(TPoint))',
             SizeOf({$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF}), TestResultInt1);

    // TPoint
    TestResultInt1 := -1;
    TestResultInt2 := -1;
    RunMacro('var p: TPoint;' +
             'begin' +
             '  p := point(1001, 2002);' +
             '  test_point(p);' +
             'end.');
    AssertEQ('Failed point param X', 1001, TestResultInt1);
    AssertEQ('Failed point param Y', 2002, TestResultInt2);


    TestInputInt1 := 3001;
    TestInputInt2 := 4002;
    RunMacro('var p: TPoint;' +
             'begin' +
             '  p := test_getpoint' +
             '  test_point(p);' +
             'end.');
    AssertEQ('Failed getint1', 3001, TestResultInt1);
    AssertEQ('Failed getint2', 4002, TestResultInt2);

    TestInputInt1 := 5001;
    TestInputInt2 := 6002;
    RunMacro('begin' +
             '  test_point(test_getpoint);' +
             'end.');
    AssertEQ('Failed getint1', 5001, TestResultInt1);
    AssertEQ('Failed getint2', 6002, TestResultInt2);

    // string
    TestResultStr1 := 'no no';
    TestResultStr2 := TestResultStr1;
    RunMacro('var s: String;' +
             'begin' +
             '  s := ''abc'';' +
             '  test_str1(''123'');' +
             '  test_str2(s);' +
             'end.');
    AssertEQ('Failed str1 param', '123', TestResultStr1);
    AssertEQ('Failed str1 param', 'abc', TestResultStr2);

    TestInputStr1 := '123';
    TestInputStr2 := '456';
    RunMacro('var s: String;' +
             'begin' +
             '  s := test_getstr1' +
             '  test_str1(s);' +
             '  test_str2(test_getstr2);' +
             'end.');
    AssertEQ('Failed getstr1', '123', TestResultStr1);
    AssertEQ('Failed getstr2', '456', TestResultStr2);


    finally
      FreeAndNil(m);
    end;
  except
    Result := False;
  end;
end;

end.

