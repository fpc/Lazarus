program lazdatadesktop;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, dicteditor, DBFLaz, frmimportdd,
  frmgeneratesql, RunTimeTypeInfoControls, frmSQLConnect,
  ddfiles, frmselectconnectiontype,
  lazdatadeskstr, fraquery, fradata, fraconnection,
  reglddfeatures;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Title:='Lazarus Data Desktop';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

