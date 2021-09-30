program LazDebFpTest;

{$mode objfpc}{$H+}
{off $DEFINE NOGUI}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  TestDbgControlForm,
  Interfaces, Forms,
  {$ifdef NOGUI} consoletestrunner, {$ELSE}   GuiTestRunner, {$ENDIF}
  LazClasses, TestVarious,
  TestAsm, TestWatches, TestBase, TestBreakPoint, TestStepping;

{$R *.res}

{$ifdef NOGUI}
var
  Application: TTestRunner;
  LclApp: TApplication;
{$ENDIF}

begin
  {$ifdef NOGUI}
  LclApp := Forms.Application.Create(nil);
  LclApp.Initialize;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := '';
  Application.Run;
  Application.Free;
  LclApp.Free;
  {$ELSE}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
  {$ENDIF}
end.


