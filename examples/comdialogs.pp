program comdialogs;

{$mode objfpc}

uses forms,
     dlgform;

begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   Application.CreateForm(TSampleDialogs, SampleDialogs);
   Application.Run;
end.
