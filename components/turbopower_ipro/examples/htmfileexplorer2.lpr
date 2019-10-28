program HtmFileExplorer2;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Interfaces,
  Forms, HtmFileExp2;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='HtmFileExplorer2';
  Application.Initialize;
  Application.CreateForm(TFHtmFileExp2, FHtmFileExp2);
  Application.Run;
end.

