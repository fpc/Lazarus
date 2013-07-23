program lazde;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  FrmMain,
  frmOptions,
  frmNewNode,
  frmmakeskel,
  frmLink,
  frmTable,
  frmabout,
  freditor,
  fpdeutil,
  frmexample,
  frmbuild,
  frmsource,
  lazdemsg,
  lazdeopts, 
  frpeditor, 
  frmlists;

{$R lazde.res}

begin
  Application.Title:='Lazarus documentation editor.';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


