program test_obj_inspector;

{$MODE OBJFPC}

uses
  classes, forms, sysutils, test_unit;

begin
  Application.Initialize; { calls InitProcedure which starts up GTK }
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

