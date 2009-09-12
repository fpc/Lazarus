program lazdatadesktop;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, dicteditor, DBFLaz, frmimportdd,
  frmgeneratesql, SQLDBLaz, lazdatadict, RunTimeTypeInfoControls, frmSQLConnect,
  ddfiles, conneditor, datapanel, querypanel, frmselectconnectiontype,
  lazdatadeskstr, lazdbexport;

{$IFDEF WINDOWS}{$R lazdatadesktop.rc}{$ENDIF}

begin
  Application.Title:='Lazarus Data Desktop';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

