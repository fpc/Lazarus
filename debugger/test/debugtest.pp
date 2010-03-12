program debugtest;

{$mode objfpc}
{$H+}


uses
  Classes, Forms, DebugTestForm, BreakpointsDlg, LocalsDlg,
  Interfaces, Unit1;

{$R debugtest.res}

begin
   Application.Initialize;
   Application.CreateForm(TDebugTestForm, DebugTestFrm);
   Application.Run;
end.
