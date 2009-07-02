program easyeditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, EditMain, LResources, EasyDockMgr, fEditForm;

{$IFDEF WINDOWS}{$R easyeditor.rc}{$ENDIF}

begin
  {$I easyeditor.lrs}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

