unit dmRestBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldbrestbridge, sqldbrestschema, pqconnection, ibconnection, sqldbrestauth,db,
  // Register formats
  sqldbrestcsv ,sqldbrestxml, sqldbrestcds, sqldbrestado;

type

  { TRestDataModule }

  TRestDataModule = class(TDataModule)
    AuthBasic: TRestBasicAuthenticator;
    Dispatcher: TSQLDBRestDispatcher;
    ExpensesSchema: TSQLDBRestSchema;
    BPProjects: TSQLDBRestBusinessProcessor;
    procedure DataModuleCreate(Sender: TObject);
    procedure DispatcherLog(Sender: TObject; aType: TRestDispatcherLogOption;
      const aMessage: UTF8String);
    procedure DoAllowedOperations(aSender: TObject; aContext: TBaseRestContext; var aOperations: TRestOperations);
    procedure DoAllowedRecord(aSender: TObject; aContext: TBaseRestContext; aDataSet: TDataset; var allowRecord: Boolean);
    procedure DoAllowResource(aSender: TObject; aContext: TBaseRestContext; var allowResource: Boolean);
    procedure DoCheckParams(aSender: TObject; aContext: TBaseRestContext; aOperation: TRestOperation; Params: TParams);
    procedure DoGetDataset(aSender: TObject; aContext: TBaseRestContext;  aFieldList: TRestFieldPairArray; aOrderBy: TRestFieldOrderPairArray;    aLimit, aOffset: Int64; var aDataset: TDataset);
  private
  public

  end;

var
  RestDataModule: TRestDataModule;

implementation

{$R *.lfm}

uses sqldbrestini, custapp;

{ TRestDataModule }

procedure TRestDataModule.DataModuleCreate(Sender: TObject);

Var
  D,Cfg : String;

begin
  D:=ExtractFilePath(ParamStr(0));
  if CustomApplication.Hasoption('c','connection') then
    Cfg:=CustomApplication.GetoptionValue('c','connection')
  else
    Cfg:=D+'connection.ini';
  if FileExists(Cfg) then
    Dispatcher.Connections[0].LoadFromFile('connection.ini');
  if CustomApplication.Hasoption('c','connection') then
    Cfg:=CustomApplication.GetoptionValue('i','ini')
  else
    Cfg:=D+'server.ini';
  if FileExists(Cfg) then
    Dispatcher.LoadFromFile('server.ini',[]);
  // Manual config
  if CustomApplication.Hasoption('b','basedir') then
    Dispatcher.BasePath:=CustomApplication.GetoptionValue('b','basedir');
  if CustomApplication.HasOption('q','quiet') then
    begin
    Dispatcher.OnLog:=Nil;
    Dispatcher.LogOptions:=[];
    end
  else if CustomApplication.HasOption('v','verbose') then
    Dispatcher.LogOptions:=Dispatcher.LogOptions+[rloSQL];
  // Activate
  Dispatcher.Active:=True;
end;

procedure TRestDataModule.DispatcherLog(Sender: TObject;
  aType: TRestDispatcherLogOption; const aMessage: UTF8String);
begin
  if isConsole then
    Writeln('['+LogNames[aType]+'] '+aMessage)
  else if Assigned(CustomApplication) then
    CustomApplication.Log(etInfo,'['+LogNames[aType]+'] '+aMessage);
end;

procedure TRestDataModule.DoAllowedOperations(aSender: TObject;
  aContext: TBaseRestContext; var aOperations: TRestOperations);
begin
  if IsConsole then
    Writeln('[Debug] AllowedOperations for ',aContext.UserID);
end;

procedure TRestDataModule.DoAllowedRecord(aSender: TObject;
  aContext: TBaseRestContext; aDataSet: TDataset; var allowRecord: Boolean);
begin
  if IsConsole then
    Writeln('[Debug] AllowedRecord for ',aContext.UserID);
  AllowRecord:=True;
end;

procedure TRestDataModule.DoAllowResource(aSender: TObject;
  aContext: TBaseRestContext; var allowResource: Boolean);
begin
  if IsConsole then
    Writeln('[Debug]  AllowedResource for ',aContext.UserID);
end;

procedure TRestDataModule.DoCheckParams(aSender: TObject;
  aContext: TBaseRestContext; aOperation: TRestOperation; Params: TParams);

Var
  P : TParam;
begin
  if IsConsole then
    begin
    Writeln('CheckParams for ',aContext.UserID,', aOperation : ',aOperation);
    For P in Params do
      Writeln('Param ',P.Name,' : ',P.AsString);
    end;
end;

procedure TRestDataModule.DoGetDataset(aSender: TObject;
  aContext: TBaseRestContext; aFieldList: TRestFieldPairArray;
  aOrderBy: TRestFieldOrderPairArray; aLimit, aOffset: Int64;
  var aDataset: TDataset);
begin
  Writeln('DoGetDataset for ',aContext.UserID);
end;

end.

