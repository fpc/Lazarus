program checksnapshotsgui;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  SnapshotsUptodate, ftplistertest, TestMonitorCfg;

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

