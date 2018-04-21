program reportdesign;

{$mode objfpc}{$H+}

// Connections to be included

{$DEFINE HASIBCONNECTION}
{$DEFINE HASMYSQL50CONNECTION}
{$DEFINE HASMYSQL55CONNECTION}
{$DEFINE HASMYSQL4CONNECTION}
{$DEFINE HASPQCONNECTION}
{$DEFINE HASSQLITE3CONNECTION}

{$IF (FPC_FULLVERSION>30302) or not defined(win64)}
 {$DEFINE HASORACLECONNECTION}
{$ENDIF}

{$IF FPC_FULLVERSION >= 20601}
  // MS SQL Server and Sybase ASE connectors were introduced in the FPC 2.7 development branch,
  //  and backported to 2.6.1. Operating systems should match FPC packages\fcl-db\fpmake.pp
  {$IF DEFINED(BEOS) OR DEFINED(HAIKU) OR DEFINED(LINUX) OR DEFINED(FREEBSD) OR DEFINED (NETBSD) OR DEFINED(OPENBSD) OR DEFINED(WIN32) OR DEFINED(WIN64)}
    {$DEFINE HASMSSQLCONNECTION}
    {$DEFINE HASSYBASECONNECTION}
  {$ENDIF}
{$ENDIF}

{$IF FPC_FULLVERSION >= 20603}
  {$DEFINE HASMYSQL56CONNECTION}
{$ENDIF}
{$IF FPC_FULLVERSION >= 20701}
  {$DEFINE HASMYSQL57CONNECTION}
{$ENDIF}

uses


  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  {$IFDEF HASIBCONNECTION}
    ibconnection,
  {$ENDIF}
  {$IFDEF HASMSSQLCONNECTION}
    // mssqlconn provide both MS SQL Server and Sybase ASE connectors.
    mssqlconn,
  {$ENDIF}
  odbcconn,
  {$IFDEF HASPQCONNECTION}
    pqconnection,
    {$IFDEF HASPQEVENT}
    pqteventmonitor,
    {$ENDIF}
  {$ENDIF}
  {$IFDEF HASORACLECONNECTION}
    oracleconnection,
  {$ENDIF}
  {$IFDEF HASMYSQL4CONNECTION}
    mysql40conn, mysql41conn,
  {$ENDIF}
  {$IFDEF HASMYSQL50CONNECTION}
    mysql50conn,
    mysql51conn,
  {$ENDIF}
  {$IFDEF HASMYSQL55CONNECTION}
    mysql55conn,
  {$ENDIF}
  {$IFDEF HASMYSQL56CONNECTION}
    mysql56conn,
  {$ENDIF}
  {$IFDEF HASMYSQL57CONNECTION}
    mysql57conn,
  {$ENDIF}
  {$IFDEF HASSQLITE3CONNECTION}
    sqlite3conn,
  {$ENDIF}
  fpreport,
  fpreportdb,
  Interfaces, // this includes the LCL widgetset
  Forms,
  runtimetypeinfocontrols,
  // These configure various designer factories
  regfpdesigner,
  frmfpreportdesignermain,
  fpreportdatadbf,
  fpreportdatasqldb,
  fpreportdatajson,
  frafpreportcsvdata,
  frafpreportdbfdata,
  frafpreportjsondata,
  frafpreportsqldbdata,
  frmideselectreportdata,
  // Various forms
  frmfprdresizeelements,
  frmfpreportalignelements,
  frmfpreportdataconnectioneditor,
  frmconfigreportdata,
  frmfpreportvariables,
  frmfpreportproperties,
  frmfpreportpreviewdata,
  cfgfpreportpdfexport,
  cfgfpreportimageexport,
  cfgfpreporthtmlexport,
  //
  sqldb,
  fpreporthtmlexport,
  fpreportpreview,
  fpreportformexport;

{$R *.res}

TYpe

  { Tlogger }

  Tlogger = Class(TObject)
    Constructor Create;
  private
    procedure DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
  end;

var
  FPReportDesignerForm: TFPReportDesignerForm;

{ Tlogger }

constructor Tlogger.Create;
begin
  GlobalDBLogHook:=@DoLog;
end;

procedure Tlogger.DoLog(Sender: TSQLConnection; EventType: TDBEventType; const Msg: String);
begin
  Writeln(Eventtype:10,': ',Msg);
end;

begin
//  Tlogger.Create;
  RequireDerivedFormResource:=True;
  RegisterFPReportPropEditors;
  Application.Initialize;
  Application.CreateForm(TFPReportDesignerForm,FPReportDesignerForm);
  Application.Scaled:=False;
  if (ParamCount>0) then
    FPReportDesignerForm.InitialFileName:=ParamStr(1);
  // Improve this to check for options ?
  Application.Run;
end.

