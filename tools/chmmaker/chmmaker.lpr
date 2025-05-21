program chmmaker;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  chmAbout, Interfaces, // this includes the LCL widgetset
  Forms, LazFileUtils, LCLTranslator,
  CHMMain, CHMSiteMapEditor, chmStrConsts;

{$R *.res}

begin
  Application.Scaled := True;
  Application.Title := '';
  Application.Initialize;
  Application.CreateForm(TCHMForm, CHMForm);
  Application.CreateForm(TSitemapEditForm, SitemapEditForm);
  Application.Run;
end.

