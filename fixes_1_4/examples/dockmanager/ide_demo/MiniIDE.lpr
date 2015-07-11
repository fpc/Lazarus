program MiniIDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fminiide, EasyDockMgr, fclientform, fEditBook
  { you can add units after this };

{$IFDEF WINDOWS}{.$R MiniIDE.rc}{$ENDIF}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainBar, MainBar);
  Application.Run;
end.

