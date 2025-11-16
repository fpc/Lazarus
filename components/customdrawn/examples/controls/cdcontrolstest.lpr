program cdcontrolstest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainForm, ToolBarTest, ButtonTest
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormCDControlsTest, FormCDControlsTest);
  Application.CreateForm(TFormToolBar, FormToolBar);
  Application.CreateForm(TFormButtons, FormButtons);
  Application.Run;
end.

