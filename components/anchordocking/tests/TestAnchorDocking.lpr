program TestAnchorDocking;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestADRestore, anchordockpkg;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

