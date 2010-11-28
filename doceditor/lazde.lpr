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
  pkeditor,
  eleditor,
  fpdeutil,
  frmexample,
  frmbuild,
  frmsource,
  lazdemsg,
  lazdeopts,
  pgeditor;

{$R lazde.res}

begin
  Application.Title:='Lazarus documentation editor.';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.


