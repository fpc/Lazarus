unit TestLazLoggerCase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, LazLoggerBase, LazLogger, LazClasses;

type
  TStringArray = array of string;

  { TTestLazLogger }

  TTestLazLogger = class(TTestCase)
  protected
    FTheLogger: TLazLoggerFile;
    FOnDbgOutCount, FOnDebugLnCount: Integer;
    FOnDbgOutText, FOnDebugLnText: string;
    FArgcMem, FArgVSaved: PPChar;
    FArgCSaved: Integer;
    FArgs: TStringArray;

    procedure TestOnDbgOut(Sender: TObject; S: string; var Handled: Boolean);
    procedure TestOnDebugln(Sender: TObject; S: string; var Handled: Boolean);
    procedure InitLogger;
    procedure AssertDbgOut(Name: string; ExpCount: integer; ExpText: String);
    procedure AssertDebugLn(Name: string; ExpCount: integer; ExpText: String);
    procedure SaveArgs;
    procedure RestoreArgs;
    procedure SetArgv(List: TStringArray);
    procedure SetArgs(List: array of string);
  published
    procedure TestEvent;
    procedure TestFilter;
    procedure TestReCreate;
  end;

  TLazLoggerForTest = class(TLazLogger)
  end;

implementation

procedure TTestLazLogger.TestOnDbgOut(Sender: TObject; S: string; var Handled: Boolean);
begin
  inc(FOnDbgOutCount);
  FOnDbgOutText := FOnDbgOutText + s;
  Handled := True;
end;

procedure TTestLazLogger.TestOnDebugln(Sender: TObject; S: string; var Handled: Boolean);
begin
  inc(FOnDebugLnCount);
  FOnDebugLnText := FOnDebugLnText + s + LineEnding;
  Handled := True;
end;

procedure TTestLazLogger.InitLogger;
begin
  FreeAndNil(FTheLogger);
  FTheLogger := TLazLoggerFile.Create;
  FTheLogger.OnDebugLn  := @TestOnDebugln;
  FTheLogger.OnDbgOut  := @TestOnDbgOut;
  FOnDebugLnCount := 0;
  FOnDebugLnText := '';
end;

procedure TTestLazLogger.AssertDbgOut(Name: string; ExpCount: integer; ExpText: String);
begin
  AssertEquals(Name + ' DbgOut call count', ExpCount, FOnDbgOutCount);
  AssertEquals(Name + ' DbgOut text', ExpText, FOnDbgOutText);
  FOnDbgOutCount := 0;
  FOnDbgOutText := '';
end;

procedure TTestLazLogger.AssertDebugLn(Name: string; ExpCount: integer; ExpText: String);
begin
  AssertEquals(Name + ' DebugLn call count', ExpCount, FOnDebugLnCount);
  AssertEquals(Name + ' DebugLn text', ExpText, FOnDebugLnText);
  FOnDebugLnCount := 0;
  FOnDebugLnText := '';
end;

procedure TTestLazLogger.SaveArgs;
begin
  FArgVSaved := argv;
  FArgCSaved := argc;
end;

procedure TTestLazLogger.RestoreArgs;
begin
  argv := FArgVSaved;
  argc := FArgCSaved;
end;

procedure TTestLazLogger.SetArgv(List: TStringArray);
var
  i: Integer;
begin
  if List = nil then begin
    argc := 0;
    argv := nil;
    ReAllocMem(FArgcMem, 0);
    exit;
  end;
  argc := Length(List);
  ReAllocMem(FArgcMem, argc * SizeOf(PChar));
  argv := FArgcMem;
  for i := 0 to argc - 1 do
    FArgcMem[i] := PChar(List[i]);
end;

procedure TTestLazLogger.SetArgs(List: array of string);
var
  i: Integer;
begin
  if Length(List) = 0 then begin
    SetArgv(nil);
    Exit;
  end;
  SetLength(FArgs, Length(List));
  for i := 0 to Length(List) - 1 do
    FArgs[i] := List[i];
  SetArgv(FArgs);
end;

procedure TTestLazLogger.TestEvent;
begin
  InitLogger;

  FTheLogger.DebugLn('a');
  AssertDebugLn('debugln a', 1, 'a'+LineEnding);
  AssertDbgOut('debugln a', 0, '');

  FTheLogger.DebugLn('b', 'c');
  AssertDebugLn('debugln b,c', 1, 'bc'+LineEnding);
  AssertDbgOut('debugln b,c', 0, '');

  FTheLogger.DebugLn(['d', 1]);
  AssertDebugLn('debugln d,1', 1, 'd1'+LineEnding);
  AssertDbgOut('debugln d,1', 0, '');

  FTheLogger.DebugLn('e %d', [1]);
  AssertDebugLn('debugln e,1', 1, 'e 1'+LineEnding);
  AssertDbgOut('debugln e,1', 0, '');


  FTheLogger.DbgOut('a');
  AssertDbgOut('DbgOut a', 1, 'a');
  AssertDebugLn('DbgOut a', 0, '');

  FTheLogger.DbgOut('b', 'c');
  AssertDbgOut('DbgOut b,c', 1, 'bc');
  AssertDebugLn('DbgOut b,c', 0, '');

  FTheLogger.DbgOut(['d', 1]);
  AssertDbgOut('DbgOut d,1', 1, 'd1');
  AssertDebugLn('DbgOut d,1', 0, '');

  FTheLogger.DbgOut('e %d', [1]);
  AssertDbgOut('DbgOut e,1', 1, 'e 1');
  AssertDebugLn('DbgOut e,1', 0, '');

  FTheLogger.DebugLnEnter('a');
  AssertDebugLn('DebugLnEnter() a', 1, 'a'+LineEnding);
  AssertDbgOut('DebugLnEnter() a', 0, '');

  FTheLogger.DebugLn('in enter');
  AssertDebugLn('debugln in enter', 1, '  in enter'+LineEnding);
  AssertDbgOut('debugln in enter', 0, '');

  FTheLogger.DebugLnExit('b');
  AssertDebugLn('DebugLnExit() b', 1, 'b'+LineEnding);
  AssertDbgOut('DebugLnExit() b', 0, '');

  FTheLogger.DebugLn('after exit');
  AssertDebugLn('debugln after exit', 1, 'after exit'+LineEnding);
  AssertDbgOut('debugln after exit', 0, '');


  FreeAndNil(FTheLogger);
end;

procedure TTestLazLogger.TestFilter;
  procedure FilterInit(List: array of string);
  begin
    SetArgs(List);
    InitLogger;
    FTheLogger.ParamForEnabledLogGroups := '--dbe=';
  end;

  procedure FilterInitNoParam(List: array of string);
  begin
    SetArgs(List);
    InitLogger;
  end;

  Procedure TestNoGroup(Name: String);
  begin
    FTheLogger.DebugLn('NoGroup'+Name);
    AssertDebugLn(Name + 'debugln NoGroup'+Name, 1, 'NoGroup'+Name+LineEnding);

    FTheLogger.DebugLn('NoGroupNil'+Name);
    AssertDebugLn(Name + 'debugln [nil] NoGroupNil'+Name, 1, 'NoGroupNil'+Name+LineEnding);
  end;

  Procedure TestGroup(Name: String; g: PLazLoggerLogGroup; ExpLog: Boolean);
  begin
    FTheLogger.DebugLn(g, 'Group'+Name);
    if ExpLog then
      AssertDebugLn(Name + 'debugln ['+g^.ConfigName+'] Group'+Name, 1, 'Group'+Name+LineEnding)
    else
      AssertDebugLn(Name + 'debugln ['+g^.ConfigName+'] Group'+Name, 0, '');

    FTheLogger.DebugLn(g, ['Group2',Name]);
    if ExpLog then
      AssertDebugLn(Name + 'debugln ['+g^.ConfigName+'] [Group2,'+Name+']', 1, 'Group2'+Name+LineEnding)
    else
      AssertDebugLn(Name + 'debugln ['+g^.ConfigName+'] [Group2,'+Name+']', 0, '');
  end;

var
  g1: PLazLoggerLogGroup;
  s, a: String;
begin
  SaveArgs;
  try
    {%region g1 not default enabled}
      s := 'g1(false)';
      a := '';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);
      g1^.Enabled := True;
      TestGroup  (Format('%s -- %s', [s+'->enabled', a]), g1, True);

      a := '--dbe=';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);

      a := '--dbe=g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);
      g1^.Enabled := False;
      TestGroup  (Format('%s -- %s', [s+'->disabled', a]), g1, False);

      a := '--dbe=+g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=-g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);

      a := '--dbe=g2';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);

      a := '--dbe=g2,g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=g1,g2';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=-g2,g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=-';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);

      a := '--dbe=- --dbe=+g1';
      FilterInit(['exe', '--debug-log=a', '--dbe=-', '--dbe=+g1']);
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);
    {%endregion g1 not default enabled}

    {%region g1 default enabled}
      s := 'g1(true)';
      a := '';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1', True);
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1', True);
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=+g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1', True);
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=-g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1', True);
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);

      a := '--dbe=-g2';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1', True);
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, True);

      a := '--dbe=g2,-g1';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1', True);
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);

      a := '--dbe=-';
      FilterInit(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1', True);
      TestNoGroup(Format('%s -- %s', [s, a]));
      TestGroup  (Format('%s -- %s', [s, a]), g1, False);
    {%endregion g1 default enabled}


    {%region g1 not default enabled / param after}
      s := 'g1(false)';
      a := '';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, False);

      a := '--dbe=';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, False);

      a := '--dbe=g1';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      g1 := FTheLogger.RegisterLogGroup('g1');
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, True);

      a := '--dbe=+g1';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, True);

      a := '--dbe=-g1';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, False);

      a := '--dbe=g2';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, False);

      a := '--dbe=g2,g1';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, True);

      a := '--dbe=g1,g2';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, True);

      a := '--dbe=-g2,g1';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, True);

      a := '--dbe=-';
      FilterInitNoParam(['exe', '--debug-log=a', a]);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, False);

      a := '--dbe=- --dbe=+g1';
      FilterInitNoParam(['exe', '--debug-log=a', '--dbe=-', '--dbe=+g1']);
      g1 := FTheLogger.RegisterLogGroup('g1');
      FTheLogger.ParamForEnabledLogGroups := '--dbe=';
      TestNoGroup(Format('Param late: %s -- %s', [s, a]));
      TestGroup  (Format('Param late: %s -- %s', [s, a]), g1, True);
    {%endregion g1 not default enabled}
  finally
    SetArgv(nil);
    RestoreArgs;
    FreeAndNil(FTheLogger);
  end;
end;

function CreateDebugLogger: TRefCountedObject;
begin
  Result := TLazLoggerForTest.Create;
  TLazLoggerFile(Result).Assign(GetExistingDebugLogger);
end;

procedure TTestLazLogger.TestReCreate;
begin
  AssertTrue('DebugLogger is TLazLoggerFile', DebugLogger is TLazLoggerFile);

  DebugLogger.MaxNestPrefixLen := 122;
  LazDebugLoggerCreator := @CreateDebugLogger;

  AssertTrue('still DebugLogger is TLazLoggerFile', DebugLogger is TLazLoggerFile);

  RecreateDebugLogger;
  AssertTrue('DebugLogger is TLazLoggerForTest', LazLoggerBase.DebugLogger is TLazLoggerForTest);
  AssertEquals('MaxNestPrefixLen = 122', 122, TLazLoggerForTest(LazLoggerBase.DebugLogger).MaxNestPrefixLen);

end;


initialization

  RegisterTest(TTestLazLogger);
end.

