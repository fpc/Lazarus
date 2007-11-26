program HtmFileExplorer2;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Interfaces,
  Forms, HtmFileExp2, JPEGForLazarus;

begin
  Application.Title:='HtmFileExplorer2';
  Application.Initialize;
  Application.CreateForm(TFHtmFileExp2, FHtmFileExp2);
  Application.Run;
end.

