program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, Interfaces,
  FPCUnit, TestReport, TestRegistry, PlainTestReport, UtilsTest;

type

  { TAChartTests }

  TAChartTests = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TAChartTests }

procedure TAChartTests.DoRun;
var
  testResult: TTestResult = nil;
  writer: TPlainResultsWriter = nil;
begin
  testResult := TTestResult.Create;
  writer := TPlainResultsWriter.Create(nil);
  try
    testResult.AddListener(writer);
    GetTestRegistry.Run(testResult);
    writer.WriteResult(testResult);
  finally
    testResult.Free;
    writer.Free;
  end;
  Terminate;
end;

constructor TAChartTests.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TAChartTests.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TAChartTests;

{$R *.res}

begin
  Application:=TAChartTests.Create(nil);
  Application.Title:='TAChart tests';
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

