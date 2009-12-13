program MiniIDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LResources, fminiide, EasyDockMgr, fclientform, fEditBook
  { you can add units after this };

{$IFDEF WINDOWS}{$R MiniIDE.rc}{$ENDIF}

begin
  {$I MiniIDE.lrs}
  Application.Initialize;
  Application.CreateForm(TMainBar, MainBar);
  Application.Run;
end.

