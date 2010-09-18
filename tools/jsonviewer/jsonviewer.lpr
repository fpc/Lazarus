program jsonviewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, frmNewBoolean, frmnewinteger, frmnewstring, msgjsonviewer
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='JSON Data viewer';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TNewBooleanForm, NewBooleanForm);
  Application.CreateForm(TNewNumberForm, NewNumberForm);
  Application.CreateForm(TNewStringForm, NewStringForm);
  Application.Run;
end.

