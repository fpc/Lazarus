unit sqldbschemaedittools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldbrestschema, sqldbrestbridge, sqldb, controls, forms;

Const
   // Index in imagelist.
  idxConnection        = 0;
  idxConnectionAdd     = 1;
  idxConnectionDelete  = 2;
  idxConnectionEdit    = 3;
  idxConnectionExpose  = 4;
  idxConnectionRefresh = 5;
  idxTable             = 6;
  idxTableAdd          = 7;
  idxTableDelete       = 8;
  idxTableEdit         = 9;
  idxField             = 10;
  idxFields            = 11;
  idxFieldAdd          = 12;
  idxFieldDelete       = 13;
  idxFieldEdit         = 14;
  idxKeyField          = 15;
  idxConnectionsHide   = 16;
  idxConnectionsShow   = 17;
  idxTableInfo         = 18;


Type
  TOnGetSQLConnection = Procedure (Sender : TObject; aConnName : String; Out aConn : TSQLConnection) of object;

  { TBaseEditFrame }

  TBaseEditFrame = Class(TFrame)
  private
    FConnections: TSQLDBRestConnectionList;
    FFrameData: TObject;
    FMinFieldOptions: TRestFieldOptions;
    Procedure DoSetFrameData(aData : TObject);
  Protected
    Procedure SetFrameData(aData : TObject); virtual; abstract;
    Function CanGetSQLConnection : Boolean;
    Function GetSQLConnection(Const aName : String) : TSQLConnection;
    Function ExecuteSelect(const aConnection : String; aSQL : String) : TSQLQuery;
    procedure SetConnections(AValue: TSQLDBRestConnectionList); virtual;
  Public
    Function Modified : Boolean; virtual; abstract;
    Procedure SaveData; virtual; abstract;
    Function FrameCaption : String; virtual; abstract;
    // Must be set !
    Property FrameData : TObject Read FFrameData Write DoSetFrameData;
    Property Connections : TSQLDBRestConnectionList Read FConnections Write SetConnections;
    Property MinFieldOptions : TRestFieldOptions Read FMinFieldOptions Write FMinFieldOptions;
  end;

  { TStringsDragObject }

  TStringsDragObject = Class(TDragObjectEx)
  private
    FItems: Tstrings; // to keep the items in.
    procedure SetItems(const AValue: Tstrings);
  Public
    Constructor Create(AControl : TControl); override;
    Destructor Destroy; override;
    // Objects contain TMySQLDBRestConnection instance
    Property Items : TStrings Read FItems Write SetItems;
  end;

  TMySQLDBRestResource = class(TSQLDBRestResource)
  end;

  { TMySQLDBRestSchema }

  TMySQLDBRestSchema = Class(TSQLDBRestSchema)
  Protected
    Function CreateResourceList: TSQLDBRestResourceList; override;
  end;
  { TMySQLDBRestConnection }

  TMySQLDBRestConnection = Class(TSQLDBRestConnection)
  private
    FMyConnection: TSQLConnection;
    FMyTransaction : TSQLTransaction;
    function getMyConnection: TSQLConnection;
  Public
    Destructor Destroy; override;
    Procedure CreateConnection;
    Property MyConnection : TSQLConnection read getMyConnection Write FMyConnection;
  end;

{$r *.lfm}

Resourcestring
  SSchema = 'Schema';
  SFields = 'Fields';
  SField = 'Field';
  SResource = 'Resource';
  SEdit = 'Edit';
  SPropTableName  = 'Table name: %s';
  SPropConnection = 'Connection: %s';
  SErrNoConnection = 'Cannot execute SQL: no connection available';
  SSQLValidatesOK = 'SQL Statement validates OK!';
  SNameForResource =  'Give a name for the new resource';
  SNewResource = 'New resource';
  SNameForField =  'Give a name for the new field for resource %s';
  SNewField = 'New field';
  SErrDuplicateResource = 'Duplicate resource name: %s';
  SErrDuplicateField = 'Duplicate field name: %s';
  SDeleteResourceCaption = 'Delete resource';
  SDeleteResourceMsg = 'Delete resource %s ?%sThis action cannot be undone';
  SDeleteFieldCaption = 'Delete Field';
  SDeleteFieldMsg = 'Delete field %s from resource %s ?%sThis action cannot be undone';
  SYesDelete = 'Yes, delete';
  SNoDoNotDelete = 'No, do not delete';
  SUnknownObject = 'Unknown';
  SEditObject = '%s %s';
  SSelectResource = 'Select a resource';
  SResetFields = 'Reset fields';
  SResetFieldsPrompt = 'There are already fields defined for this resource. %sThis action will remove the existing fields. %sAre '
    +'you sure you want to reset the field list ?';
  SYesResetFields = 'Reset fields';
  SDoNotResetFields = 'Do not reset fields';
  STableNameChanged = 'The table name changed, and a default SQL statement is used.%sDo you want to regenerate the field list '
    +'based on the current table name ?';
  SEditObjectFields = 'Fields of resource %s';
  SErrConnectingTo = 'Error connecting to connection %s : %s';
  SErrShowingTablesConnectingTo = 'Error connecting to connection %s trying to show the table list: %s';

implementation

{ TBaseEditFrame }

procedure TBaseEditFrame.SetConnections(AValue: TSQLDBRestConnectionList);
begin
  if FConnections=AValue then Exit;
  FConnections:=AValue;
end;

procedure TBaseEditFrame.DoSetFrameData(aData: TObject);
begin
  FFrameData:=aData;
  SetFrameData(FFrameData);
end;

function TBaseEditFrame.CanGetSQLConnection: Boolean;
begin
  Result:=Assigned(FConnections)
end;

function TBaseEditFrame.GetSQLConnection(const aName: String): TSQLConnection;

Var
  C : TSQLDBRestConnection;

begin
  Result:=nil;
  if Not CanGetSQLConnection then
    exit;
  if (aName<>'') then
    C:=Connections.FindConnection(aName)
  else if (Connections.Count=1) then
    C:=Connections[0];
  if C<>Nil then
    begin
    Result:=C.SingleConnection;
    if Result=Nil then
      begin
      Result:=TSQLConnector.Create(Self.Owner);
      Result.Transaction:=TSQLTransaction.Create(Self.Owner);
      Result.Transaction.DataBase:=Result;
      C.ConfigConnection(Result);
      C.SingleConnection:=Result;
      end;
    end;
end;

function TBaseEditFrame.ExecuteSelect(const aConnection: String; aSQL: String): TSQLQuery;

Var
  C : TSQLConnection;

begin
  C:=GetSQLConnection(aConnection);
  if C=Nil then
    Raise ESQLDatabaseError.Create(SErrNoConnection);
  Result:=TSQLQuery.Create(Self);
  try
    Result.SQLConnection:=C;
    if Result.Transaction=Nil then
      begin
      Result.Transaction:=TSQLTransaction.Create(C);
      Result.Transaction.DataBase:=C;
      end;
    Result.SQL.Text:=aSQL;
    Result.PacketRecords:=1;
    Result.ParseSQL:=True;
    Result.UniDirectional:=True;
    Result.Open;
  except
    Result.Free;
    Raise;
  end;
end;

{ TStringsDragObject }

procedure TStringsDragObject.SetItems(const AValue: Tstrings);
begin
  if FItems=AValue then exit;
  FItems.Assign(AValue);
end;

constructor TStringsDragObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
  FItems:=TStringList.Create;
end;

destructor TStringsDragObject.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TMySQLDBRestSchema }

function TMySQLDBRestSchema.CreateResourceList: TSQLDBRestResourceList;
begin
  Result:=TSQLDBRestResourceList.Create(Self,TMySQLDBRestResource);
end;

{ TMySQLDBRestConnection }

function TMySQLDBRestConnection.getMyConnection: TSQLConnection;

begin
  Result:=FMyConnection;
  if (FMyConnection=Nil) then
    Result:=SingleConnection;
end;

destructor TMySQLDBRestConnection.Destroy;
begin
  FreeAndNil(FMyTransaction);
  FreeAndNil(FMyConnection);
  inherited Destroy;
end;

procedure TMySQLDBRestConnection.CreateConnection;


begin
  FreeAndNil(FMyConnection);
  FMyConnection:=TSQLConnector.Create(Nil);
  FMyTransaction:=TSQLTransaction.Create(nil);
  FMyConnection.Transaction:=FMyTransaction;
  ConfigConnection(FMyConnection);
end;


end.

