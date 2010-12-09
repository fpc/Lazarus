unit TestWatches;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  TestBase, Debugger, GDBMIDebugger, LCLProc;

type

  { TTestWatch }

  TTestWatch = class(TBaseWatch)
  private
    FHasMultiValue: Boolean;
    FHasValue: Boolean;
    FMaster: TDBGWatch;
    FValue: String;
  protected
    procedure DoChanged; override;
  public
    constructor Create(AOwner: TBaseWatches; AMaster: TDBGWatch);
    property Master: TDBGWatch read FMaster;
    property HasMultiValue: Boolean read FHasMultiValue;
    property HasValue: Boolean read FHasValue;
    property Value: String read FValue;
  end;

  { TTestWatches }

  TTestWatches = class(TGDBTestCase)
  private
    FWatches: TBaseWatches;

    FAVal1Watch: TTestWatch;
    FAVal2Watch: TTestWatch;

    FTestIntWatch: TTestWatch;
    FTestShortStringWatch: TTestWatch;
    FTestAnsiStringWatch: TTestWatch;
    FTestPCharWatch: TTestWatch;

    FArgAnsisString1: TTestWatch;
    FArgAnsisString2: TTestWatch;
    FArgAnsisString3: TTestWatch;
    FArgChar1: TTestWatch;
    FArgChar2: TTestWatch;
    FArgChar3: TTestWatch;
  public
    procedure DebugInteract(dbg: TGDBMIDebugger);

  published
    procedure TestWatches;
  end;


implementation

{ TTestWatch }

procedure TTestWatch.DoChanged;
begin
  if FMaster.Valid = vsValid then begin
    if FHasValue and (FValue <> FMaster.Value) then
      FHasMultiValue := True;
    FHasValue := True;
    FValue := FMaster.Value;
  end;
end;

constructor TTestWatch.Create(AOwner: TBaseWatches; AMaster: TDBGWatch);
begin
  inherited Create(AOwner);
  FMaster := AMaster;
  FMaster.Slave := Self;
  FMaster.Enabled := True;
end;

{ TTestWatches }

procedure TTestWatches.DebugInteract(dbg: TGDBMIDebugger);
var s: string;
begin
  readln(s);
  while s <> '' do begin
    dbg.TestCmd(s);
    readln(s);
  end;
end;

procedure TTestWatches.TestWatches;
var FailText: String;
  procedure TestWatch(Name: String; AWatch: TTestWatch; Exp: String; StripQuotes: Boolean = False);
  var
    s: String;
  begin
    try
      AWatch.Master.Value; // trigger read
      AssertTrue  (Name+ ' (HasValue)',   AWatch.HasValue);
      AssertFalse (Name+ ' (One Value)',  AWatch.HasMultiValue);
      s := AWatch.Value;
      if StripQuotes and (length(s) > 1) and
        (s[1] = '''') and (s[length(s)] = '''')
      then
        s := copy(s, 2, length(s)-2);
      AssertEquals(Name+ ' (Value)', Exp, s);
    except
      on e: Exception do
        FailText := FailText + e.Message + LineEnding;
    end;
  end;

var
  TestExeName: string;
  dbg: TGDBMIDebugger;
begin
  TestCompile(AppDir + 'WatchesPrg.pas', TestExeName);
  FTestIntWatch := nil;

  try
    FWatches := TBaseWatches.Create(TBaseWatch);
    dbg := TGDBMIDebugger.Create(DebuggerInfo.ExeName);
    //dbg.OnBreakPointHit  := @DebuggerBreakPointHit;
    with dbg.BreakPoints.Add('WatchesPrg.pas', 20) do begin
      InitialEnabled := True;
      Enabled := True;
    end;
    with dbg.BreakPoints.Add('WatchesPrg.pas', 30) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    FAVal1Watch := TTestWatch.Create(FWatches, dbg.Watches.Add('AVal1'));
    FAVal2Watch := TTestWatch.Create(FWatches, dbg.Watches.Add('AVal2'));

    FTestIntWatch         := TTestWatch.Create(FWatches, dbg.Watches.Add('TestInt'));
    FTestShortStringWatch := TTestWatch.Create(FWatches, dbg.Watches.Add('TestShortString'));
    FTestAnsiStringWatch  := TTestWatch.Create(FWatches, dbg.Watches.Add('TestAnsiString'));
    FTestPCharWatch       := TTestWatch.Create(FWatches, dbg.Watches.Add('TestPChar'));

    FArgAnsisString1 := TTestWatch.Create(FWatches, dbg.Watches.Add('ArgAnsiString1'));
    FArgAnsisString2 := TTestWatch.Create(FWatches, dbg.Watches.Add('ArgAnsiString2'));
    FArgAnsisString3 := TTestWatch.Create(FWatches, dbg.Watches.Add('ArgAnsiString3'));
    FArgChar1       := TTestWatch.Create(FWatches, dbg.Watches.Add('ArgChar1'));
    FArgChar2       := TTestWatch.Create(FWatches, dbg.Watches.Add('ArgChar2'));
    FArgChar3       := TTestWatch.Create(FWatches, dbg.Watches.Add('ArgChar3'));

    dbg.Init;
    if dbg.State = dsError then
      Fail(' Failed Init');

    dbg.WorkingDir := AppDir;
    dbg.FileName   := TestExeName;
    dbg.Arguments := '';
    dbg.ShowConsole := True;

    dbg.Run;
    // hit breakpoint
    //TestWatch('AVal1', FAVal1Watch, '10');
    TestWatch('AVal2', FAVal2Watch, '5');

    TestWatch('TestInt',         FTestIntWatch, '10');
    //TestWatch('TestShortString', FTestShortStringWatch, '3:',     True);
    TestWatch('TestAnsiString',  FTestAnsiStringWatch,  '3: Foo', True);
    TestWatch('TestPChar',       FTestPCharWatch,       ': Foo',  True);

    dbg.Run;
    // 2nd breakpoint
    TestWatch('ArgAnsiString1',  FArgAnsisString1, 'abc', True);
    //TestWatch('ArgAnsiString2',  FArgAnsisString2, 'def', True);
    TestWatch('ArgAnsiString3',  FArgAnsisString3, 'ghi', True);

    TestWatch('ArgChar1',  FArgChar1, '88 ''X''');
    //TestWatch('ArgChar2',  FArgChar2, '89 ''Y''');
    TestWatch('ArgChar3',  FArgChar3, '90 ''Z''');

	//DebugInteract(dbg);


    dbg.Stop;
  finally
    dbg.Free;
    FreeAndNil(FWatches);

    //debugln(FailText)
    if FailText <> '' then fail(FailText);
  end;
end;



initialization

  RegisterDbgTest(TTestWatches);
end.

