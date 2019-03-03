unit dmRestBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldbrestbridge, sqldbrestschema, pqconnection, sqldbrestauth,db,
  // Register formats
  sqldbrestcsv ,sqldbrestxml, sqldbrestcds;

type

  { TRestDataModule }

  TRestDataModule = class(TDataModule)
    AuthBasic: TRestBasicAuthenticator;
    Dispatcher: TSQLDBRestDispatcher;
    ExpensesSchema: TSQLDBRestSchema;
    BPProjects: TSQLDBRestBusinessProcessor;
    procedure DataModuleCreate(Sender: TObject);
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

uses sqldbrestini;

{ TRestDataModule }

procedure TRestDataModule.DataModuleCreate(Sender: TObject);
begin
  if FileExists('connection.ini') then
    Dispatcher.Connections[0].LoadFromFile('connection.ini');
end;

procedure TRestDataModule.DoAllowedOperations(aSender: TObject;
  aContext: TBaseRestContext; var aOperations: TRestOperations);
begin
  if IsConsole then
    Writeln('AllowedOperations for ',aContext.UserID);
end;

procedure TRestDataModule.DoAllowedRecord(aSender: TObject;
  aContext: TBaseRestContext; aDataSet: TDataset; var allowRecord: Boolean);
begin
  if IsConsole then
    Writeln('AllowedRecord for ',aContext.UserID);
end;

procedure TRestDataModule.DoAllowResource(aSender: TObject;
  aContext: TBaseRestContext; var allowResource: Boolean);
begin
  if IsConsole then
    Writeln('AllowedResource for ',aContext.UserID);
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

