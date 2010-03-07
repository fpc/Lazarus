program ResExploer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, reMainUnit, reAboutUnit, reConstsUnit
  { you can add units after this };

{$R resexplorer.res}

begin
  Application.Initialize;
  Application.CreateForm(TreMainForm, reMainForm);
  if Paramcount>0 then
    reMainForm.OpenFile(ParamStr(1));
  Application.Run;
end.

