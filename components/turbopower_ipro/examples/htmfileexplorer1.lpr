program HtmFileExplorer1;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Interfaces,
  Forms, HtmFileExp1;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='HtmFileExplorer1';
  Application.Initialize;
  Application.CreateForm(TFHtmFileExp1, FHtmFileExp1);
  Application.Run;
end.

