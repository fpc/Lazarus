program FpTest;

{$mode objfpc}{$H+}
{off $DEFINE TESTGUI}

uses
  Interfaces, Forms,
  {$ifdef TESTGUI} GuiTestRunner, {$ELSE}  consoletestrunner, {$ENDIF}
  TestHelperClasses,
  TestDwarfVarious, TestMemManager, TestPascalParser,
  TestErrorHandler, TestLineMap;

{$R *.res}

{$ifNdef TESTGUI}
var
  Application: TTestRunner;
{$ENDIF}

begin
  {$ifdef TESTGUI}
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
  {$ELSE}
  DefaultRunAllTests:=True;
  DefaultFormat:=fXML;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
  {$ENDIF}
end.

