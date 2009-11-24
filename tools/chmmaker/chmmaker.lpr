program chmmaker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, unit1, CHMSiteMapEditor, lhelpcontrolpkg;

begin
  Application.Initialize;
  Application.CreateForm(TCHMForm, CHMForm);
  Application.CreateForm(TSitemapEditForm, SitemapEditForm);
  if ParamCount = 1 then CHMForm.OpenProject(ParamStr(1));
  Application.Run;
end.

