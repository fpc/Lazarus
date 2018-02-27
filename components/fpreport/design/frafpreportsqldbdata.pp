{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Frame to configure a SQL report data loop.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frafpreportsqldbdata;

{$mode objfpc}{$H+}
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

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, EditBtn, StdCtrls, Buttons, ActnList, SynEdit, SynHighlighterSQL,
  fpreportdesignreportdata, fpjson,  db, sqldb, reportdesigndatasql, dialogs;

type
  TFrame = TReportDataConfigFrame;

  { TSQLReportDataConfigFrame }

  TSQLReportDataConfigFrame = class(TFrame)
    ATest: TAction;
    ALJSON: TActionList;
    AConstruct: TAction;
    EConnection: TEditButton;
    ILJSON: TImageList;
    Label1: TLabel;
    LSQl: TLabel;
    SpeedButton1: TSpeedButton;
    SESQL: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure ATestExecute(Sender: TObject);
    procedure EConnectionButtonClick(Sender: TObject);
  private
    FConnectionData: TJSONObject;

  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure GetConfig(aConfig : TJSONObject); override;
    Procedure SetConfig(aConfig : TJSONObject); override;
    Function SaveNotOKMessage : string; override;
    Property ConnectionData : TJSONObject Read FConnectionData;
  end;

  { TSQLDBReportDataHandler }

  TSQLDBReportDataHandler = Class(TDesignReportDataHandler)
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; override;
    Function CreateConfigFrame(AOwner : TComponent) : TReportDataConfigFrame; override;
    Class Function CheckConfig(AConfig: TJSONObject): String; override;
    Class Function DataType : String; override;
    Class Function DataTypeDescription : String; override;
  end;

implementation

uses
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
  frmfpreportdataconnectioneditor;


resourcestring
  SErrNoConnectionData = 'No connection data available';
  SErrNoSQL = 'No SQL statement set';

{$R *.lfm}

{ TSQLReportDataConfigFrame }

procedure TSQLReportDataConfigFrame.EConnectionButtonClick(Sender: TObject);

Var
  F : TReportConnectionEditorForm;

begin
  F:=TReportConnectionEditorForm.Create(Self);
  try
    F.Params:=FConnectionData;
    if F.ShowModal=mrOK then
      begin
      FreeAndNil(FConnectionData);
      FConnectionData:=F.Params.Clone as TJSONObject;
      EConnection.Text:=FConnectionData.AsJSON;
      end;
  finally
    F.Free;
  end;
end;


procedure TSQLReportDataConfigFrame.ATestExecute(Sender: TObject);

Var
  S : String;

begin
  S:=TFPReportConnector.TestConnection(FConnectionData);
  if (S<>'') then
    MessageDlg(SErrConnectionNotOK,S,mtError,[mbOK],0)
  else
    MessageDlg(SSuccess,SConnectionSuccesful,mtInfo,[mbOK],0)
end;

constructor TSQLReportDataConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionData:=TJSONObject.Create;
end;

destructor TSQLReportDataConfigFrame.Destroy;
begin
  FreeAndNil(FConnectionData);
  inherited Destroy;
end;

procedure TSQLReportDataConfigFrame.GetConfig(aConfig: TJSONObject);

begin
  aConfig.Objects[keyConnection]:=FConnectionData.CLone as TJSONObject;
  aConfig.strings[keySQL]:=SESQL.Text;
end;

procedure TSQLReportDataConfigFrame.SetConfig(aConfig: TJSONObject);

Var
  O : TJSONObject;
begin
  O:=aConfig.Get(keyConnection,TJSONObject(Nil));
  if Assigned(O) then
    begin
    FreeAndNil(FConnectionData);
    FConnectionData:=O.Clone as TJSONObject;
    EConnection.Text:=FConnectionData.asJSON;
    end;
  SESQL.Text:=aConfig.get(keySQL,'');
end;

function TSQLReportDataConfigFrame.SaveNotOKMessage: string;
begin
  if (FConnectionData.Count=0) then
    Result:=SErrNoConnectionData
  else if Trim(SESQL.Text)='' then
    Result:=SErrNoSQL
  else
    Result:=TFPReportConnector.TestConnection(FConnectionData);
end;

{ TSQLDBReportDataHandler }

function TSQLDBReportDataHandler.CreateDataset(AOwner: TComponent; AConfig: TJSONObject): TDataset;
begin
  Result:=TFPReportConnector.CreateDataset(aOwner,aConfig);
end;

function TSQLDBReportDataHandler.CreateConfigFrame(AOwner: TComponent): TReportDataConfigFrame;
begin
  Result:=TSQLReportDataConfigFrame.Create(AOwner);
end;

class function TSQLDBReportDataHandler.CheckConfig(AConfig: TJSONObject): String;

Var
  O : TJSONObject;

begin
  O:=aConfig.Get(keyConnection,TJSONObject(Nil));
  if (O=Nil) or (O.Count=0) then
    Result:=SErrNoConnectionData
  else if Trim(aConfig.Get(keySQL,''))='' then
    Result:=SErrNoSQL
end;

class function TSQLDBReportDataHandler.DataType: String;
begin
  Result:='SQLDB';
end;

class function TSQLDBReportDataHandler.DataTypeDescription: String;
begin
  Result:='SQL Database server';
end;

initialization
  TSQLDBReportDataHandler.RegisterHandler;
end.

