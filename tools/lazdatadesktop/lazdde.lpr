program lazdde;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, frmmain, dicteditor, fpdatadict, fpdddbf, DBFLaz,
  frmimportdd, RunTimeTypeInfoControls, frmgeneratesql, fpddsqldb, SQLDBLaz,
  frmSQLConnect, ddfiles, conneditor, datapanel, querypanel;

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TImportDDform, ImportDDform);
  Application.CreateForm(TGenerateSQLForm, GenerateSQLForm);
  Application.CreateForm(TSQLConnectionForm, SQLConnectionForm);
  Application.Run;
end.

