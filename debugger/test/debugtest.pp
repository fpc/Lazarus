program debugtest;

{$mode objfpc}
{$H+}


uses
  Classes, Forms, DebugTestForm, BreakpointsDlg, LocalsDlg,
  Interfaces, Unit1;

begin
   Application.Initialize;
   Application.CreateForm(TDebugTestForm, DebugTestFrm);
   Application.Run;
end.
