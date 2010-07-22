{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Authors: Alexander Klenin

}

program test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  FPCUnit, TestReport, TestRegistry, PlainTestReport, UtilsTest, SourcesTest;

// This is deliberately a console application to also test a proper
// separation of logic and presentation in TAChart units.

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
  Application.Title := 'TAChart tests';
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

