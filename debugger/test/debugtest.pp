program debugtest;

{$mode objfpc}
{$H+}


uses
  Classes, Forms, DebugTestForm, BreakpointsDlg;

begin
   Application.Initialize; 
   Application.CreateForm(TDebugTestForm, DebugTestForm1);
   Application.Run;
end.
