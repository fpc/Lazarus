program clienttest;

{$mode objfpc}{$H+}

uses
  Classes, jsonparser, consoletestrunner, tcTests, fpcunittestinsight, testinsightclient,
  testinsightprotocol;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  if IsTestinsightListening() then
    RunRegisteredTests('','')
  else
    begin
    Application := TMyTestRunner.Create(nil);
    Application.Initialize;
    Application.Title := 'FPCUnit Console test runner';
    Application.Run;
    Application.Free;
    end;
end.
