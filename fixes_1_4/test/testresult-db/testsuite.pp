{$mode objfpc}
{$h+}
program testsuite;

uses utests;

Var
  App : TTestSuite;

begin
  App:=TTestSuite.Create(nil);
  Try
    App.Title:='Lazarus Test Suite Results';
    App.Initialize;
    App.Run;
  Finally
    App.Free;
  end;
end.
