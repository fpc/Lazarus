program debugtest;

{$mode objfpc}
{$H+}


uses
  Classes, Forms, DebugTestForm, BreakpointsDlg, LocalsDlg;

begin
   Application.Initialize; 
   Application.CreateForm(TDebugTestForm, DebugTestForm1);
   Application.Run;
end.
