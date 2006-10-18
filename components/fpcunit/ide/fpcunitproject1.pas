program FPCUnitProject1;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner;

type

  { TLazTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  App: TMyTestRunner;

begin
  App := TMyTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FPCUnit Console test runner';
  App.Run;
  App.Free;
end.
