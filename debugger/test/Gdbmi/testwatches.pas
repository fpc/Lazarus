unit Testwatches;

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
    FTestIntWatch: TTestWatch;
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

procedure TTestWatches.TestWatches;
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
    with dbg.BreakPoints.Add('WatchesPrg.pas', 16) do begin
      InitialEnabled := True;
      Enabled := True;
    end;

    FTestIntWatch := TTestWatch.Create(FWatches, dbg.Watches.Add('TestInt'));

    dbg.Init;
    if dbg.State = dsError then
      Fail(' Failed Init');

    dbg.WorkingDir := AppDir;
    dbg.FileName   := TestExeName;
    dbg.Arguments := '';
    dbg.ShowConsole := True;

    dbg.Run;
    // hit breakpoint
    FTestIntWatch.Master.Value; // trigger read
    AssertTrue  ('TestInt (HasValue)', FTestIntWatch.HasValue);
    AssertFalse ('TestInt (One Value)', FTestIntWatch.HasMultiValue);
    AssertEquals('TestInt (Value)', FTestIntWatch.Value, '10');


    dbg.Stop;
  finally
    dbg.Free;
    //FreeAndNil(FTestIntWatch);
    FreeAndNil(FWatches);
  end;

end;



initialization

  RegisterDbgTest(TTestWatches);
end.

