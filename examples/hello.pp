program hello_world;

{$mode objfpc}

uses forms,
     helloform;

begin
   Application.Initialize; { calls InitProcedure which starts up GTK }
   Application.CreateForm(THello, Hello);
   Application.Run;
end.
