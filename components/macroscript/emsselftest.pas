unit EMSSelfTest;

{$mode objfpc}{$H+}

interface

{$IFDEF darwin}
  {$DEFINE NeedTPointFix }
{$ENDIF}

uses
  Classes, SysUtils, SynEdit, LazLoggerBase,
  IDECommands, EMScriptClasses, EMScriptMacro, Clipbrd, Dialogs, Controls,
  uPSCompiler, uPSRuntime, uPSUtils;

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

var
  SelfTestErrorMsg: String;

implementation

{$IFDEF NeedTPointFix}
type TPoint2 = record x,y,a,b,c: Longint; end;
{$ENDIF}

{%region RegisterSelfTests}

var
  {%H-}TestResultA: integer;
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

procedure test_varint1(var AValue: Integer);
begin
  TestResultInt1 := AValue;
  AValue := TestInputInt1;
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

function test_getpoint: {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF};
begin
  Result.X := TestInputInt1;
  Result.Y := TestInputInt2;
end;

procedure test_varpoint(var AValue: TPoint);
begin
  TestResultInt1 := AValue.X;
  TestResultInt2 := AValue.Y;
  AValue.X := TestInputInt1;
  AValue.Y := TestInputInt2;
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

procedure test_varstr1(var AValue: String);
begin
  TestResultStr1 := AValue;
  AValue := TestInputStr1;
end;

const
  Decltest_ord_mt   = 'function test_ord_mt(AType: TMsgDlgType): Integer;';
  Decltest_ord_mb   = 'function test_ord_mb(ABtn: TMsgDlgBtn): Integer;';
  Decltest_int1     = 'procedure test_int1(AValue: Integer);';
  Decltest_int2     = 'procedure test_int2(AValue: Integer);';
  Decltest_getint1  = 'function test_getint1: Integer;';
  Decltest_getint2  = 'function test_getint2: Integer;';
  Decltest_varint1  = 'procedure test_varint1(var AValue: Integer);';
  Decltest_bool1    = 'procedure test_bool1(AValue: Boolean);';
  Decltest_bool2    = 'procedure test_bool2(AValue: Boolean);';
  Decltest_getbool1 = 'function test_getbool1: Boolean;';
  Decltest_getbool2 = 'function test_getbool2: Boolean;';
  Decltest_point    = 'procedure test_point(AValue: TPoint);';
  Decltest_getpoint = 'function test_getpoint: TPoint;';
  Decltest_varpoint = 'procedure test_varpoint(var AValue: TPoint);';
  Decltest_str1     = 'procedure test_str1(AValue: String);';
  Decltest_str2     = 'procedure test_str2(AValue: String);';
  Decltest_getstr1  = 'function test_getstr1: String;';
  Decltest_getstr2  = 'function test_getstr2: String;';
  Decltest_varstr1  = 'procedure test_varstr1(var AValue: String);';
  Functest_ord_mt:    function(AType: TMsgDlgType): Integer = @test_ord_mt;
  Functest_ord_mb:    function(ABtn: TMsgDlgBtn): Integer = @test_ord_mb;
  Proctest_int1:      procedure (AValue: Integer) = @test_int1;
  Proctest_int2:      procedure (AValue: Integer) = @test_int2;
  Proctest_getint1:   function: Integer = @test_getint1;
  Proctest_getint2:   function: Integer = @test_getint2;
  Proctest_varint1:   procedure (var AValue: Integer) = @test_varint1;
  Proctest_bool1:     procedure (AValue: Boolean) = @test_bool1;
  Proctest_bool2:     procedure (AValue: Boolean) = @test_bool2;
  Proctest_getbool1:  function: Boolean = @test_getbool1;
  Proctest_getbool2:  function: Boolean = @test_getbool2;
  Proctest_point:     procedure (AValue: TPoint)  = @test_point;
  Proctest_getpoint:  function: {$IFDEF NeedTPointFix}TPoint2{$ELSE}TPoint{$ENDIF} = @test_getpoint;
  Proctest_varpoint:  procedure (var AValue: TPoint)  = @test_varpoint;
  Proctest_str1:      procedure (AValue: String)  = @test_str1;
  Proctest_str2:      procedure (AValue: String)  = @test_str2;
  Proctest_getstr1:   function: String = @test_getstr1;
  Proctest_getstr2:   function: String = @test_getstr2;
  Proctest_varstr1:   procedure (var AValue: String)  = @test_varstr1;

{$IFDEF PasMacroNoNativeCalls}
const
  Id_test_ord_mb    = 901;
  Id_test_ord_mt    = 902;
  Id_test_int1      = 910;
  Id_test_int2      = 911;
  Id_test_getint1   = 912;
  Id_test_getint2   = 913;
  Id_test_varint1   = 914;
  Id_test_bool1     = 920;
  Id_test_bool2     = 921;
  Id_test_getbool1  = 922;
  Id_test_getbool2  = 923;
  Id_test_point     = 930;
  Id_test_getpoint  = 931;
  Id_test_varpoint  = 932;
  Id_test_str1      = 940;
  Id_test_str2      = 941;
  Id_test_getstr1   = 942;
  Id_test_getstr2   = 943;
  Id_test_varstr1   = 944;

function ExecTestHandler({%H-}Caller: TPSExec; p: TPSExternalProcRec;
  {%H-}Global, Stack: TPSStack): Boolean;
var
  data: PPoint;
  i: integer;
  s: TbtString;
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
    Id_test_varint1: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_int1"');
        i := Stack.GetInt(-1);
        test_varint1(i);
        Stack.SetInt(-1, i);
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
    Id_test_varpoint: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_getpoint"');
        data := GetVarPointFromStack(Stack, -1);
        test_varpoint(TPoint(data^));
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
    Id_test_varstr1: begin
        if Stack.Count < 1 then raise TEMScriptBadParamException.Create('Invalid param count for "test_str1()"');
        s := Stack.GetAnsiString(-1);
        test_varstr1(s);
        Stack.SetAnsiString(-1, s);
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
  AComp.AddDelphiFunction(Decltest_varint1);
  AComp.AddDelphiFunction(Decltest_bool1);
  AComp.AddDelphiFunction(Decltest_bool2);
  AComp.AddDelphiFunction(Decltest_getbool1);
  AComp.AddDelphiFunction(Decltest_getbool2);
  AComp.AddDelphiFunction(Decltest_point);
  AComp.AddDelphiFunction(Decltest_getpoint);
  AComp.AddDelphiFunction(Decltest_varpoint);
  AComp.AddDelphiFunction(Decltest_str1);
  AComp.AddDelphiFunction(Decltest_str2);
  AComp.AddDelphiFunction(Decltest_getstr1);
  AComp.AddDelphiFunction(Decltest_getstr2);
  AComp.AddDelphiFunction(Decltest_varstr1);
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
  AExec.RegisterDelphiFunction(Proctest_varint1,   'test_varint1',  cdRegister);
  AExec.RegisterDelphiFunction(Proctest_bool1,     'test_bool1',    cdRegister);
  AExec.RegisterDelphiFunction(Proctest_bool2,     'test_bool2',    cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getbool1,  'test_getbool1', cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getbool2,  'test_getbool2', cdRegister);
  AExec.RegisterDelphiFunction(Proctest_point,     'test_point',    cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getpoint,  'test_getpoint', cdRegister);
  AExec.RegisterDelphiFunction(Proctest_varpoint,  'test_varpoint', cdRegister);
  AExec.RegisterDelphiFunction(Proctest_str1,      'test_str1',     cdRegister);
  AExec.RegisterDelphiFunction(Proctest_str2,      'test_str2',     cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getstr1,   'test_getstr1',  cdRegister);
  AExec.RegisterDelphiFunction(Proctest_getstr2,   'test_getstr2',  cdRegister);
  AExec.RegisterDelphiFunction(Proctest_varstr1,   'test_varstr1',  cdRegister);
  {$ELSE}
  AExec.RegisterFunctionName('test_ord_mb',       @ExecTestHandler, Pointer(Id_test_ord_mb), nil);
  AExec.RegisterFunctionName('test_ord_mt',       @ExecTestHandler, Pointer(Id_test_ord_mt), nil);
  AExec.RegisterFunctionName('test_int1',         @ExecTestHandler, Pointer(Id_test_int1), nil);
  AExec.RegisterFunctionName('test_int2',         @ExecTestHandler, Pointer(Id_test_int2), nil);
  AExec.RegisterFunctionName('test_getint1',      @ExecTestHandler, Pointer(Id_test_getint1), nil);
  AExec.RegisterFunctionName('test_getint2',      @ExecTestHandler, Pointer(Id_test_getint2), nil);
  AExec.RegisterFunctionName('test_varint1',      @ExecTestHandler, Pointer(Id_test_varint1), nil);
  AExec.RegisterFunctionName('test_bool1',        @ExecTestHandler, Pointer(Id_test_bool1), nil);
  AExec.RegisterFunctionName('test_bool2',        @ExecTestHandler, Pointer(Id_test_bool2), nil);
  AExec.RegisterFunctionName('test_getbool1',     @ExecTestHandler, Pointer(Id_test_getbool1), nil);
  AExec.RegisterFunctionName('test_getbool2',     @ExecTestHandler, Pointer(Id_test_getbool2), nil);
  AExec.RegisterFunctionName('test_point',        @ExecTestHandler, Pointer(Id_test_point), nil);
  AExec.RegisterFunctionName('test_getpoint',     @ExecTestHandler, Pointer(Id_test_getpoint), nil);
  AExec.RegisterFunctionName('test_varpoint',     @ExecTestHandler, Pointer(Id_test_varpoint), nil);
  AExec.RegisterFunctionName('test_str1',         @ExecTestHandler, Pointer(Id_test_str1), nil);
  AExec.RegisterFunctionName('test_str2',         @ExecTestHandler, Pointer(Id_test_str2), nil);
  AExec.RegisterFunctionName('test_getstr1',      @ExecTestHandler, Pointer(Id_test_getstr1), nil);
  AExec.RegisterFunctionName('test_getstr2',      @ExecTestHandler, Pointer(Id_test_getstr2), nil);
  AExec.RegisterFunctionName('test_varstr1',      @ExecTestHandler, Pointer(Id_test_varstr1), nil);
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

type THackTEMSEditorMacro = class(TEMSEditorMacro) end;
function DoSelfTest: Boolean;
var
  m: TEMSEditorMacro;
  syn: TSynEdit;

  procedure RunMacro(AText: String);
  begin
    m.SetFromSource(AText);
    m.PlaybackMacro(syn);
  end;

  procedure AssertEQ(Msg: String; Exp, Got: String); overload;
  begin
    if not(Got = Exp) then
      raise TEMScriptSelfTestException.Create(Format('%s [Exp: "%s" / Got: "%s" / Info: %s / SynTxt: %s]', [Msg, Exp, Got, dbgs(m.IsInvalid) + ' ' + THackTEMSEditorMacro(m).GetErrorMsg, syn.Text]));
  end;
  procedure AssertEQ(Msg: String; Exp, Got: Integer); overload;
  begin
    if not(Got = Exp) then
      raise TEMScriptSelfTestException.Create(Format('%s [Exp: %d / Got: %d / Info: %s / SynTxt: %s]', [Msg, Exp, Got, dbgs(m.IsInvalid) + ' ' + THackTEMSEditorMacro(m).GetErrorMsg, syn.Text]));
  end;
  procedure AssertEQ(Msg: String; Exp, Got: Boolean); overload;
  begin
    if not(Got = Exp) then
      raise TEMScriptSelfTestException.Create(Format('%s [Exp: %s / Got: %s / Info: %s / SynTxt: %s]', [Msg, dbgs(Exp), dbgs(Got), dbgs(m.IsInvalid) + ' ' + THackTEMSEditorMacro(m).GetErrorMsg, syn.Text]));
  end;

  procedure TestInt(Msg, AText: String; Exp: Integer);
  begin
    TestResultInt1 := 0;
    RunMacro(AText);
    AssertEQ(Msg + '(init: 0)', Exp, TestResultInt1);

    TestResultInt1 := -1;
    RunMacro(AText);
    AssertEQ(Msg + '(init: -1)', Exp, TestResultInt1);

    TestResultInt1 := 99919;
    RunMacro(AText);
    AssertEQ(Msg + '(init: 99919)', Exp, TestResultInt1);
  end;

  procedure TestBool(Msg, AText: String; Exp: Boolean);
  begin
    TestResultBool1 := False;
    RunMacro(AText);
    AssertEQ(Msg + '(init: F)', Exp, TestResultBool1);

    TestResultBool1 := True;
    RunMacro(AText);
    AssertEQ(Msg + '(init: T)', Exp, TestResultBool1);
  end;

  procedure TestBool(Msg, AText: String; Exp, Exp2: Boolean);
  begin
    TestResultBool1 := False;
    TestResultBool2 := False;
    RunMacro(AText);
    AssertEQ(Msg + '(init: F,F)', Exp, TestResultBool1);
    AssertEQ(Msg + '(init: F,F)', Exp2, TestResultBool2);

    TestResultBool1 := True;
    TestResultBool2 := True;
    RunMacro(AText);
    AssertEQ(Msg + '(init: T,T)', Exp, TestResultBool1);
    AssertEQ(Msg + '(init: T,T)', Exp2, TestResultBool2);

    TestResultBool1 := True;
    TestResultBool2 := False;
    RunMacro(AText);
    AssertEQ(Msg + '(init: T,F)', Exp, TestResultBool1);
    AssertEQ(Msg + '(init: T,F)', Exp2, TestResultBool2);

    TestResultBool1 := False;
    TestResultBool2 := True;
    RunMacro(AText);
    AssertEQ(Msg + '(init: F,T)', Exp, TestResultBool1);
    AssertEQ(Msg + '(init: F,T)', Exp2, TestResultBool2);
  end;

  procedure TestSyn(Msg, AText: String; Exp: String);
  begin
    syn.ClearAll;
    RunMacro(AText);
    AssertEQ(Msg , True, pos(Exp, syn.Text) > 0);
  end;

  procedure TestSyn(Msg, AInit, AText: String; Exp: String);
  begin
    syn.ClearAll;
    syn.Text := AInit;
    RunMacro(AText);
    AssertEQ(Msg , True, pos(Exp, syn.Text) > 0);
  end;

begin
  Result := False;
  SelfTestErrorMsg := '';
  try
    try
    m := TEMSelfTestEditorMacro.Create(nil);
    syn := TSynEdit.Create(nil);

    {%region calling convention}

    // test_int1
    TestResultInt1 := 99;
    TestInt('test_int1(42)',
            'begin' +
            '  test_int1(42);' +
            'end.',
            42);
    TestInt('test_int1(-3)',
            'begin' +
            '  test_int1(-3);' +
            'end.',
            -3);

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

    TestInputInt1 := 2001;
    TestResultInt1 := -1;
    TestResultInt2 := -1;
    RunMacro('var i: Integer;' +
             'begin' +
             '  i := 1002' +
             '  test_varint1(i);' +
             '  test_int2(i);' +
             'end.');
    AssertEQ('Failed varint a', 1002, TestResultInt1);
    AssertEQ('Failed varint b', 2001, TestResultInt2);


    // test_bool
    TestBool('test_bool(F,F)',
             'begin' +
             '  test_bool1(False);' +
             '  test_bool2(False);' +
             'end.',
             False, False);

    TestBool('test_bool(T,T)',
             'begin' +
             '  test_bool1(True);' +
             '  test_bool2(True);' +
             'end.',
             True, True);

    TestBool('test_bool(T,F)',
             'begin' +
             '  test_bool1(True);' +
             '  test_bool2(False);' +
             'end.',
             True, False);

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
    AssertEQ('Failed int param (SizeOf(TPoint))', SizeOf(TPoint), TestResultInt1);

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
    AssertEQ('Failed getpoint param X', 3001, TestResultInt1);
    AssertEQ('Failed getpoint param Y', 4002, TestResultInt2);

    TestInputInt1 := 5001;
    TestInputInt2 := 6002;
    RunMacro('begin' +
             '  test_point(test_getpoint);' +
             'end.');
    AssertEQ('Failed getpoint(2) param X', 5001, TestResultInt1);
    AssertEQ('Failed getpoint(2) param Y', 6002, TestResultInt2);

    TestResultInt1 := -1;
    TestResultInt2 := -1;
    TestResultBool1 := False;
    TestInputInt1 := 1005;
    TestInputInt2 := 1006;
    RunMacro('var p: TPoint;' +
             'begin' +
             '  p := point(990, 991);' +
             '  test_varpoint(p);' +
             '  test_bool1((p.x =  1005) and (p.y = 1006));' +
             'end.');
    AssertEQ('Failed varpoint x', 990, TestResultInt1);
    AssertEQ('Failed varpoint y', 991, TestResultInt2);
    AssertEQ('Failed varpoint new', True, TestResultBool1);

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

    TestInputStr1 := '123';
    TestResultStr1 := '';
    TestResultStr2 := '';
    TestResultBool1 := False;
    RunMacro('var s: String;' +
             'begin' +
             '  s := ''aaa''' +
             '  test_varstr1(s);' +
             '  test_bool1(s = ''123'');' +
             '  test_str2(s);' +
             'end.');
    AssertEQ('Failed varstr1', 'aaa', TestResultStr1);
    AssertEQ('Failed varstr2', '123', TestResultStr2);
    AssertEQ('Failed varstr3',True, TestResultBool1);

    {%endregion calling convention}

    {%region }

    TestBool('mrNone',
             'begin' +
             '  test_bool1(mrNone = ' +IntToStr(mrNone) + ');' +
             'end.',
             True
             );

    TestBool('mrOk',
             'begin' +
             '  test_bool1(mrOk = ' +IntToStr(mrOk) + ');' +
             'end.',
             True
             );
    TestBool('mtWarning',
             'begin' +
             '  test_bool1(test_ord_mt(mtWarning) = ' +IntToStr(ord(mtWarning)) + ');' +
             'end.',
             True
             );
    TestBool('mtConfirmation',
             'begin' +
             '  test_bool1(test_ord_mt(mtConfirmation) = ' +IntToStr(ord(mtConfirmation)) + ');' +
             'end.',
             True
             );
    TestBool('mbYes',
             'begin' +
             '  test_bool1(test_ord_mb(mbYes) = ' +IntToStr(ord(mbYes)) + ');' +
             'end.',
             True
             );
    TestBool('mbCancel',
             'begin' +
             '  test_bool1(test_ord_mb(mbCancel) = ' +IntToStr(ord(mbCancel)) + ');' +
             'end.',
             True
             );

    {%endregion }

    TestSyn('ecChar',
            'begin ecChar(''C''); end.',
            'C'
            );

    TestSyn('InsertTextAtCaret',
            'begin Caller.InsertTextAtCaret(''Foo'', scamEnd); end.',
            'Foo');

    TestSyn('TextBetweenPoints',
            '123456',
            'begin Caller.TextBetweenPoints[Point(3,1), point(5,1)] :=  ''ng''; end.',
            '12ng56');

    TestSyn('Replace All',   'Test abc abcde 123',
            'begin Caller.SearchReplace(''abc'', ''XYZ'', [ssoReplaceAll]); end.',
            'Test XYZ XYZde 123'
            );

      Result := True;
    finally
      FreeAndNil(m);
      FreeAndNil(syn);
    end;
  except
    on E: Exception do begin
        SelfTestErrorMsg := E.Message;
        Result := False;
      end;
  end;
end;

end.

