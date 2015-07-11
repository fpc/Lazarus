program MiniIDE2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, fMiniIde2, EasyDockMgr, fClientForm2, fEditBook2
  { you can add units after this };

{$IFDEF WINDOWS}{.$R MiniIDE.rc}{$ENDIF}
{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainBar, MainBar);
  Application.Run;
end.

