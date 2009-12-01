program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, EasyDockMgr, fEditorSite, fEditBook, fClientForm;

begin
  Application.Initialize;
  Application.CreateForm(TEditorSite, EditorSite);
  Application.Run;
end.

