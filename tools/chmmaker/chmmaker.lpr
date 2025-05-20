program chmmaker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  chmAbout, Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils,
  CHMMain, CHMSiteMapEditor;

var
  i: Integer;
  Filename: String;

{$R *.res}

begin
  Application.Scaled := True;
  Application.Title := '';
  Application.Initialize;
  Application.CreateForm(TCHMForm, CHMForm);
  Application.CreateForm(TSitemapEditForm, SitemapEditForm);
  for i:=1 to Application.ParamCount do
  begin
    Filename:=ParamStr(i);
    if (Filename='') or (Filename[1]='-') then continue;
    CHMForm.OpenProject(CleanAndExpandFilename(Filename));
    break;
  end;
  Application.Run;
end.

