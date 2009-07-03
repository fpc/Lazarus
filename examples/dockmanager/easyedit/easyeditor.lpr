program easyeditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, EditMain, EasyDockMgr, fEditForm, fEditBook;

{$IFDEF WINDOWS}{$R easyeditor.rc}{$ENDIF}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

