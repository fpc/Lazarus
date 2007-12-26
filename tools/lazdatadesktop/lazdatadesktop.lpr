program lazdatadesktop;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, frmmain, dicteditor, DBFLaz,
  frmimportdd, RunTimeTypeInfoControls, frmgeneratesql, SQLDBLaz,
  frmSQLConnect, ddfiles, conneditor, datapanel, querypanel, lazdbexport,
  lazdatadict;

begin
  Application.Title:='Lazarus Data Desktop';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

