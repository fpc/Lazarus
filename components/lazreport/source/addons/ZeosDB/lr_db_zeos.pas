unit LR_DB_Zeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Graphics, DB, LR_Class, ZDataset, LR_DBComponent,
  LR_Intrp, ZConnection;

type
  TlrZeosData = class(TComponent)

  end;

type

  { TLRZQuery }

  TLRZQuery = class(TLRDataSetControl)
  private
    FDatabase: string;
    FParams: TfrVariables;
    procedure SetDatabase(AValue: string);
    procedure ZQueryBeforeOpen(ADataSet: TDataSet);
    procedure DoMakeParams;
    procedure DoEditParams;
  protected
    function GetSQL: TStringList;virtual;
    procedure SetSQL(AValue: TStringList);virtual;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property SQL:TStringList read GetSQL write SetSQL;
    property Database:string read FDatabase write SetDatabase;
    property Params:TfrVariables read FParams write FParams;
  end;

  { TLRZConnection }

  TLRZConnection = class(TfrNonVisualControl)
  private
    FConnected:boolean;
    FZConnection: TZConnection;
    function GetConnected: Boolean;
    function GetDatabase: string;
    function GetHostName: string;
    function GetLoginPrompt: Boolean;
    function GetPassword: string;
    function GetPort: Integer;
    function GetProtocol: string;
    function GetUser: string;
    procedure SetConnected(AValue: Boolean);
    procedure SetDatabase(AValue: string);
    procedure SetHostName(AValue: string);
    procedure SetLoginPrompt(AValue: Boolean);
    procedure SetPassword(AValue: string);
    procedure SetPort(AValue: Integer);
    procedure SetProtocol(AValue: string);
    procedure SetUser(AValue: string);
  protected
    procedure SetName(const AValue: string); override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Connected: Boolean read GetConnected write SetConnected;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property Protocol: string read GetProtocol write SetProtocol;
    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
  end;

procedure Register;
implementation
uses LR_Utils, DBPropEdits, PropEdits, LazarusPackageIntf, ZDbcIntfs, types,
  LR_EditVariables, Forms, Controls, variants;

var
  lrBMP_ZQuery:TBitmap = nil;
  lrBMP_ZConnection:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_ZQuery) then
  begin
    lrBMP_ZQuery := TbitMap.Create;
    lrBMP_ZQuery.LoadFromLazarusResource('TLRZQuery');
    frRegisterObject(TLRZQuery, lrBMP_ZQuery, 'TLRZQuery', nil, otlUIControl, nil);
  end;
  if not assigned(lrBMP_ZConnection) then
  begin
    lrBMP_ZConnection := TbitMap.Create;
    lrBMP_ZConnection.LoadFromLazarusResource('TLRZConnection');
    frRegisterObject(TLRZConnection, lrBMP_ZConnection, 'TLRZConnection', nil, otlUIControl, nil);
  end;
end;

{ TLRZConnection }

procedure TLRZConnection.SetDatabase(AValue: string);
begin
  FZConnection.Database:=AValue;
end;

function TLRZConnection.GetDatabase: string;
begin
  Result:=FZConnection.Database;
end;

function TLRZConnection.GetConnected: Boolean;
begin
  Result:=FZConnection.Connected;
end;

function TLRZConnection.GetHostName: string;
begin
  Result:=FZConnection.HostName;
end;

function TLRZConnection.GetLoginPrompt: Boolean;
begin
  Result:=FZConnection.LoginPrompt;
end;

function TLRZConnection.GetPassword: string;
begin
  Result:=FZConnection.Password;
end;

function TLRZConnection.GetPort: Integer;
begin
  Result:=FZConnection.Port;
end;

function TLRZConnection.GetProtocol: string;
begin
  Result:=FZConnection.Protocol;
end;

function TLRZConnection.GetUser: string;
begin
  Result:=FZConnection.User;
end;

procedure TLRZConnection.SetConnected(AValue: Boolean);
begin
  FZConnection.Connected:=AValue;
end;

procedure TLRZConnection.SetHostName(AValue: string);
begin
  FZConnection.HostName:=AValue;
end;

procedure TLRZConnection.SetLoginPrompt(AValue: Boolean);
begin
  FZConnection.LoginPrompt:=AValue;
end;

procedure TLRZConnection.SetPassword(AValue: string);
begin
  FZConnection.Password:=AValue;
end;

procedure TLRZConnection.SetPort(AValue: Integer);
begin
  FZConnection.Port:=AValue;
end;

procedure TLRZConnection.SetProtocol(AValue: string);
begin
  FZConnection.Protocol:=AValue;
end;

procedure TLRZConnection.SetUser(AValue: string);
begin
  FZConnection.User:=AValue;
end;

procedure TLRZConnection.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FZConnection.Name:=Name;
end;

procedure TLRZConnection.AfterLoad;
begin
  inherited AfterLoad;
  FZConnection.Connected:=FConnected;
end;

constructor TLRZConnection.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'LRZConnection';
  FZConnection:=TZConnection.Create(OwnerForm);
end;

destructor TLRZConnection.Destroy;
begin
  if not (Assigned(OwnerPage) and (OwnerPage is TfrPageDialog)) then
  begin
    FreeAndNil(FZConnection);
  end;
  inherited Destroy;
end;

procedure TLRZConnection.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FZConnection.Port:=XML.GetValue(Path + 'Connected/Value'{%H-}, 0);
  FZConnection.LoginPrompt:=XML.GetValue(Path + 'LoginPrompt/Value'{%H-}, false);
  FZConnection.Protocol:=XML.GetValue(Path + 'Protocol/Value'{%H-}, '');

  FZConnection.HostName:=XML.GetValue(Path + 'HostName/Value'{%H-}, '');
  FZConnection.Database:=XML.GetValue(Path + 'Database/Value'{%H-}, '');
  FZConnection.User:=XML.GetValue(Path + 'User/Value'{%H-}, '');
  FZConnection.Password:=XML.GetValue(Path + 'Password/Value'{%H-}, '');
  FConnected:=XML.GetValue(Path + 'Connected/Value'{%H-}, false);
end;

procedure TLRZConnection.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path + 'Connected/Value'{%H-}, FZConnection.Port);
  XML.SetValue(Path + 'LoginPrompt/Value'{%H-}, FZConnection.LoginPrompt);
  XML.SetValue(Path + 'Protocol/Value'{%H-}, FZConnection.Protocol);

  XML.SetValue(Path + 'HostName/Value'{%H-}, FZConnection.HostName);
  XML.SetValue(Path + 'Database/Value'{%H-}, FZConnection.Database);
  XML.SetValue(Path + 'User/Value'{%H-}, FZConnection.User);
  XML.SetValue(Path + 'Password/Value'{%H-}, FZConnection.Password);
  XML.SetValue(Path + 'Connected/Value'{%H-}, FZConnection.Connected);
end;

{ TLRZQuery }

procedure TLRZQuery.SetSQL(AValue: TStringList);
begin
  if TZQuery(DataSet).SQL = AValue then Exit;
  TZQuery(DataSet).SQL.Assign(AValue);
  DoMakeParams;
end;

procedure TLRZQuery.AfterLoad;
var
  D:TComponent;
begin
  D:=frFindComponent(DataSet.Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
  begin
    TZQuery(DataSet).Connection:=TZConnection(D);
    DataSet.Active:=FActive;
  end;
end;

procedure TLRZQuery.SetDatabase(AValue: string);
var
  D:TComponent;
begin
  if FDatabase=AValue then Exit;
  FDatabase:=AValue;

  DataSet.Active:=false;
  D:=frFindComponent(TZQuery(DataSet).Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
    TZQuery(DataSet).Connection:=TZConnection(D);
end;

procedure TLRZQuery.ZQueryBeforeOpen(ADataSet: TDataSet);
var
  i: Integer;
  s: String;
  SaveView: TfrView;
  SavePage: TfrPage;
  SaveBand: TfrBand;
  Q:TZQuery;
begin
  Q:=TZQuery(DataSet);
  SaveView := CurView;
  SavePage := CurPage;
  SaveBand := CurBand;

  CurView := nil;
  CurPage := OwnerPage;
  CurBand := nil;

  for i := 0 to Q.Params.Count - 1 do
  begin
    s := Trim(VarToStr(FParams[Q.Params[i].Name]));
    if (s <> '') and (DocMode = dmPrinting) then
      Q.Params[i].AsString := frParser.Calc(s);
  end;

  if Assigned(Q.Connection) then
    if not Q.Connection.Connected then Q.Connection.Connect;

  CurView := SaveView;
  CurPage := SavePage;
  CurBand := SaveBand;
end;

procedure TLRZQuery.DoMakeParams;
var
  Q:TZQuery;
  i:integer;
begin
  Q:=TZQuery(DataSet);
  if Q.Params.Count > 0 then
  begin
    //Add new params...
    for i:=0 to Q.Params.Count-1 do
      if FParams.IndexOf(Q.Params[i].Name)<0 then
        FParams[Q.Params[i].Name]:='';

    //Delete not exists params
    for i:=FParams.Count-1 downto 0 do
      if not Assigned(Q.Params.FindParam(FParams.Name[i])) then
        FParams.Delete(i);
  end
  else
    FParams.Clear;
end;

procedure TLRZQuery.DoEditParams;
begin
  lrEditVariablesForm:=TlrEditVariablesForm.Create(Application);
  lrEditVariablesForm.LoadParamList(FParams);
  if lrEditVariablesForm.ShowModal = mrOk then
  begin
    lrEditVariablesForm.SaveParamList(FParams);
    frDesigner.Modified:=true;
  end;
  lrEditVariablesForm.Free;
end;

function TLRZQuery.GetSQL: TStringList;
begin
  Result:=TZQuery(DataSet).SQL as TStringList;
end;

constructor TLRZQuery.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrZQuery';
  FParams:=TfrVariables.Create;

  DataSet:=TZQuery.Create(OwnerForm);
  DataSet.BeforeOpen:=@ZQueryBeforeOpen;
end;

destructor TLRZQuery.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TLRZQuery.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  C, I:integer;
begin
  inherited LoadFromXML(XML, Path);
  SQL.Text  := XML.GetValue(Path+'SQL/Value'{%H-}, '');
  FDatabase:= XML.GetValue(Path+'Database/Value'{%H-}, '');


  C:=XML.GetValue(Path+'Params/Count/Value', 0);
  for i:=0 to C-1 do
    FParams[XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Name', '')] := XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Value', '');
end;

procedure TLRZQuery.SaveToXML(XML: TLrXMLConfig; const Path: String);
var
  i:integer;
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'SQL/Value', SQL.Text);
  XML.SetValue(Path+'Database/Value', FDatabase);

  XML.SetValue(Path+'Params/Count/Value', FParams.Count);
  for i:=0 to FParams.Count-1 do
  begin
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Name', FParams.Name[i]);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Value', String(FParams.Value[i]));
  end;
end;


type

  { TLRZQueryParamsProperty }

  TLRZQueryParamsProperty = class(TPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

  { TLRZQueryDataBaseProperty }

  TLRZQueryDataBaseProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TLRZConnectionProtocolProperty }

  TLRZConnectionProtocolProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TLRZQueryParamsProperty }

function TLRZQueryParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TLRZQueryParamsProperty.GetValue: ansistring;
begin
  Result:='(Params)';
end;

procedure TLRZQueryParamsProperty.Edit;
begin
  if (GetComponent(0) is TLRZQuery) then
    TLRZQuery(GetComponent(0)).DoEditParams;
end;

{ TLRZConnectionProtocolProperty }

procedure TLRZConnectionProtocolProperty.FillValues(const Values: TStringList);
var
  i, j:integer;
  A:TStringDynArray;
begin
  if (GetComponent(0) is TLRZConnection) and Assigned(DriverManager) then
  begin
    for i:=0 to DriverManager.GetDrivers.GetCount-1 do
    begin
      A:=(DriverManager.GetDrivers.Items[i] as IZDriver).GetSupportedProtocols;
      for j:=Low(A) to High(A) do
        Values.Add(A[j]);
    end;
  end;
end;

{ TLRZQueryDataBaseProperty }
procedure TLRZQueryDataBaseProperty.FillValues(const Values: TStringList);
begin
  if (GetComponent(0) is TLRZQuery) then
    frGetComponents(nil, TZConnection, Values, nil);
end;


procedure RegisterLR_DB_Zeos;
begin
  RegisterComponents('LazReport',[TlrZeosData]);
end;

procedure Register;
begin
  RegisterUnit('LR_DB_Zeos', @RegisterLR_DB_Zeos);
end;


initialization
  {$I lr_zeos_img.inc}
  InitLRComp;

  RegisterPropertyEditor(TypeInfo(string), TLRZQuery, 'Database', TLRZQueryDataBaseProperty);
  RegisterPropertyEditor(TypeInfo(TfrVariables), TLRZQuery, 'Params', TLRZQueryParamsProperty);

  RegisterPropertyEditor(TypeInfo(string), TLRZConnection, 'Protocol', TLRZConnectionProtocolProperty);

finalization
  if Assigned(lrBMP_ZQuery) then
    FreeAndNil(lrBMP_ZQuery);
  if Assigned(lrBMP_ZConnection) then
    FreeAndNil(lrBMP_ZConnection);
end.

unit LR_DB_Zeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Graphics, DB, LR_Class, ZDataset, LR_DBComponent,
  LR_Intrp, ZConnection;

type
  TlrZeosData = class(TComponent)

  end;

type

  { TLRZQuery }

  TLRZQuery = class(TLRDataSetControl)
  private
    FDatabase: string;
    FParams: TfrVariables;
    procedure SetDatabase(AValue: string);
    procedure ZQueryBeforeOpen(ADataSet: TDataSet);
    procedure DoMakeParams;
    procedure DoEditParams;
  protected
    function GetSQL: TStringList;virtual;
    procedure SetSQL(AValue: TStringList);virtual;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property SQL:TStringList read GetSQL write SetSQL;
    property Database:string read FDatabase write SetDatabase;
    property Params:TfrVariables read FParams write FParams;
  end;

  { TLRZConnection }

  TLRZConnection = class(TfrNonVisualControl)
  private
    FConnected:boolean;
    FZConnection: TZConnection;
    function GetConnected: Boolean;
    function GetDatabase: string;
    function GetHostName: string;
    function GetLoginPrompt: Boolean;
    function GetPassword: string;
    function GetPort: Integer;
    function GetProtocol: string;
    function GetUser: string;
    procedure SetConnected(AValue: Boolean);
    procedure SetDatabase(AValue: string);
    procedure SetHostName(AValue: string);
    procedure SetLoginPrompt(AValue: Boolean);
    procedure SetPassword(AValue: string);
    procedure SetPort(AValue: Integer);
    procedure SetProtocol(AValue: string);
    procedure SetUser(AValue: string);
  protected
    procedure SetName(const AValue: string); override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Connected: Boolean read GetConnected write SetConnected;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property Protocol: string read GetProtocol write SetProtocol;
    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
  end;

procedure Register;
implementation
uses LR_Utils, DBPropEdits, PropEdits, LazarusPackageIntf, ZDbcIntfs, types,
  LR_EditVariables, Forms, Controls, variants;

var
  lrBMP_ZQuery:TBitmap = nil;
  lrBMP_ZConnection:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_ZQuery) then
  begin
    lrBMP_ZQuery := TbitMap.Create;
    lrBMP_ZQuery.LoadFromLazarusResource('TLRZQuery');
    frRegisterObject(TLRZQuery, lrBMP_ZQuery, 'TLRZQuery', nil, otlUIControl, nil);
  end;
  if not assigned(lrBMP_ZConnection) then
  begin
    lrBMP_ZConnection := TbitMap.Create;
    lrBMP_ZConnection.LoadFromLazarusResource('TLRZConnection');
    frRegisterObject(TLRZConnection, lrBMP_ZConnection, 'TLRZConnection', nil, otlUIControl, nil);
  end;
end;

{ TLRZConnection }

procedure TLRZConnection.SetDatabase(AValue: string);
begin
  FZConnection.Database:=AValue;
end;

function TLRZConnection.GetDatabase: string;
begin
  Result:=FZConnection.Database;
end;

function TLRZConnection.GetConnected: Boolean;
begin
  Result:=FZConnection.Connected;
end;

function TLRZConnection.GetHostName: string;
begin
  Result:=FZConnection.HostName;
end;

function TLRZConnection.GetLoginPrompt: Boolean;
begin
  Result:=FZConnection.LoginPrompt;
end;

function TLRZConnection.GetPassword: string;
begin
  Result:=FZConnection.Password;
end;

function TLRZConnection.GetPort: Integer;
begin
  Result:=FZConnection.Port;
end;

function TLRZConnection.GetProtocol: string;
begin
  Result:=FZConnection.Protocol;
end;

function TLRZConnection.GetUser: string;
begin
  Result:=FZConnection.User;
end;

procedure TLRZConnection.SetConnected(AValue: Boolean);
begin
  FZConnection.Connected:=AValue;
end;

procedure TLRZConnection.SetHostName(AValue: string);
begin
  FZConnection.HostName:=AValue;
end;

procedure TLRZConnection.SetLoginPrompt(AValue: Boolean);
begin
  FZConnection.LoginPrompt:=AValue;
end;

procedure TLRZConnection.SetPassword(AValue: string);
begin
  FZConnection.Password:=AValue;
end;

procedure TLRZConnection.SetPort(AValue: Integer);
begin
  FZConnection.Port:=AValue;
end;

procedure TLRZConnection.SetProtocol(AValue: string);
begin
  FZConnection.Protocol:=AValue;
end;

procedure TLRZConnection.SetUser(AValue: string);
begin
  FZConnection.User:=AValue;
end;

procedure TLRZConnection.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FZConnection.Name:=Name;
end;

procedure TLRZConnection.AfterLoad;
begin
  inherited AfterLoad;
  FZConnection.Connected:=FConnected;
end;

constructor TLRZConnection.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'LRZConnection';
  FZConnection:=TZConnection.Create(OwnerForm);
end;

destructor TLRZConnection.Destroy;
begin
  if not (Assigned(OwnerPage) and (OwnerPage is TfrPageDialog)) then
  begin
    FreeAndNil(FZConnection);
  end;
  inherited Destroy;
end;

procedure TLRZConnection.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FZConnection.Port:=XML.GetValue(Path + 'Connected/Value'{%H-}, 0);
  FZConnection.LoginPrompt:=XML.GetValue(Path + 'LoginPrompt/Value'{%H-}, false);
  FZConnection.Protocol:=XML.GetValue(Path + 'Protocol/Value'{%H-}, '');

  FZConnection.HostName:=XML.GetValue(Path + 'HostName/Value'{%H-}, '');
  FZConnection.Database:=XML.GetValue(Path + 'Database/Value'{%H-}, '');
  FZConnection.User:=XML.GetValue(Path + 'User/Value'{%H-}, '');
  FZConnection.Password:=XML.GetValue(Path + 'Password/Value'{%H-}, '');
  FConnected:=XML.GetValue(Path + 'Connected/Value'{%H-}, false);
end;

procedure TLRZConnection.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path + 'Connected/Value'{%H-}, FZConnection.Port);
  XML.SetValue(Path + 'LoginPrompt/Value'{%H-}, FZConnection.LoginPrompt);
  XML.SetValue(Path + 'Protocol/Value'{%H-}, FZConnection.Protocol);

  XML.SetValue(Path + 'HostName/Value'{%H-}, FZConnection.HostName);
  XML.SetValue(Path + 'Database/Value'{%H-}, FZConnection.Database);
  XML.SetValue(Path + 'User/Value'{%H-}, FZConnection.User);
  XML.SetValue(Path + 'Password/Value'{%H-}, FZConnection.Password);
  XML.SetValue(Path + 'Connected/Value'{%H-}, FZConnection.Connected);
end;

{ TLRZQuery }

procedure TLRZQuery.SetSQL(AValue: TStringList);
begin
  if TZQuery(DataSet).SQL = AValue then Exit;
  TZQuery(DataSet).SQL.Assign(AValue);
  DoMakeParams;
end;

procedure TLRZQuery.AfterLoad;
var
  D:TComponent;
begin
  D:=frFindComponent(DataSet.Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
  begin
    TZQuery(DataSet).Connection:=TZConnection(D);
    DataSet.Active:=FActive;
  end;
end;

procedure TLRZQuery.SetDatabase(AValue: string);
var
  D:TComponent;
begin
  if FDatabase=AValue then Exit;
  FDatabase:=AValue;

  DataSet.Active:=false;
  D:=frFindComponent(TZQuery(DataSet).Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
    TZQuery(DataSet).Connection:=TZConnection(D);
end;

procedure TLRZQuery.ZQueryBeforeOpen(ADataSet: TDataSet);
var
  i: Integer;
  s: String;
  SaveView: TfrView;
  SavePage: TfrPage;
  SaveBand: TfrBand;
  Q:TZQuery;
begin
  Q:=TZQuery(DataSet);
  SaveView := CurView;
  SavePage := CurPage;
  SaveBand := CurBand;

  CurView := nil;
  CurPage := OwnerPage;
  CurBand := nil;

  for i := 0 to Q.Params.Count - 1 do
  begin
    s := Trim(VarToStr(FParams[Q.Params[i].Name]));
    if (s <> '') and (DocMode = dmPrinting) then
      Q.Params[i].AsString := frParser.Calc(s);
  end;

  if Assigned(Q.Connection) then
    if not Q.Connection.Connected then Q.Connection.Connect;

  CurView := SaveView;
  CurPage := SavePage;
  CurBand := SaveBand;
end;

procedure TLRZQuery.DoMakeParams;
var
  Q:TZQuery;
  i:integer;
begin
  Q:=TZQuery(DataSet);
  if Q.Params.Count > 0 then
  begin
    //Add new params...
    for i:=0 to Q.Params.Count-1 do
      if FParams.IndexOf(Q.Params[i].Name)<0 then
        FParams[Q.Params[i].Name]:='';

    //Delete not exists params
    for i:=FParams.Count-1 downto 0 do
      if not Assigned(Q.Params.FindParam(FParams.Name[i])) then
        FParams.Delete(i);
  end
  else
    FParams.Clear;
end;

procedure TLRZQuery.DoEditParams;
begin
  lrEditVariablesForm:=TlrEditVariablesForm.Create(Application);
  lrEditVariablesForm.LoadParamList(FParams);
  if lrEditVariablesForm.ShowModal = mrOk then
  begin
    lrEditVariablesForm.SaveParamList(FParams);
    frDesigner.Modified:=true;
  end;
  lrEditVariablesForm.Free;
end;

function TLRZQuery.GetSQL: TStringList;
begin
  Result:=TZQuery(DataSet).SQL as TStringList;
end;

constructor TLRZQuery.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrZQuery';
  FParams:=TfrVariables.Create;

  DataSet:=TZQuery.Create(OwnerForm);
  DataSet.BeforeOpen:=@ZQueryBeforeOpen;
end;

destructor TLRZQuery.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TLRZQuery.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  C, I:integer;
begin
  inherited LoadFromXML(XML, Path);
  SQL.Text  := XML.GetValue(Path+'SQL/Value'{%H-}, '');
  FDatabase:= XML.GetValue(Path+'Database/Value'{%H-}, '');


  C:=XML.GetValue(Path+'Params/Count/Value', 0);
  for i:=0 to C-1 do
    FParams[XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Name', '')] := XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Value', '');
end;

procedure TLRZQuery.SaveToXML(XML: TLrXMLConfig; const Path: String);
var
  i:integer;
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'SQL/Value', SQL.Text);
  XML.SetValue(Path+'Database/Value', FDatabase);

  XML.SetValue(Path+'Params/Count/Value', FParams.Count);
  for i:=0 to FParams.Count-1 do
  begin
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Name', FParams.Name[i]);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Value', String(FParams.Value[i]));
  end;
end;


type

  { TLRZQueryParamsProperty }

  TLRZQueryParamsProperty = class(TPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

  { TLRZQueryDataBaseProperty }

  TLRZQueryDataBaseProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TLRZConnectionProtocolProperty }

  TLRZConnectionProtocolProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TLRZQueryParamsProperty }

function TLRZQueryParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TLRZQueryParamsProperty.GetValue: ansistring;
begin
  Result:='(Params)';
end;

procedure TLRZQueryParamsProperty.Edit;
begin
  if (GetComponent(0) is TLRZQuery) then
    TLRZQuery(GetComponent(0)).DoEditParams;
end;

{ TLRZConnectionProtocolProperty }

procedure TLRZConnectionProtocolProperty.FillValues(const Values: TStringList);
var
  i, j:integer;
  A:TStringDynArray;
begin
  if (GetComponent(0) is TLRZConnection) and Assigned(DriverManager) then
  begin
    for i:=0 to DriverManager.GetDrivers.GetCount-1 do
    begin
      A:=(DriverManager.GetDrivers.Items[i] as IZDriver).GetSupportedProtocols;
      for j:=Low(A) to High(A) do
        Values.Add(A[j]);
    end;
  end;
end;

{ TLRZQueryDataBaseProperty }
procedure TLRZQueryDataBaseProperty.FillValues(const Values: TStringList);
begin
  if (GetComponent(0) is TLRZQuery) then
    frGetComponents(nil, TZConnection, Values, nil);
end;


procedure RegisterLR_DB_Zeos;
begin
  RegisterComponents('LazReport',[TlrZeosData]);
end;

procedure Register;
begin
  RegisterUnit('LR_DB_Zeos', @RegisterLR_DB_Zeos);
end;


initialization
  {$I lr_zeos_img.inc}
  InitLRComp;

  RegisterPropertyEditor(TypeInfo(string), TLRZQuery, 'Database', TLRZQueryDataBaseProperty);
  RegisterPropertyEditor(TypeInfo(TfrVariables), TLRZQuery, 'Params', TLRZQueryParamsProperty);

  RegisterPropertyEditor(TypeInfo(string), TLRZConnection, 'Protocol', TLRZConnectionProtocolProperty);

finalization
  if Assigned(lrBMP_ZQuery) then
    FreeAndNil(lrBMP_ZQuery);
  if Assigned(lrBMP_ZConnection) then
    FreeAndNil(lrBMP_ZConnection);
end.

unit LR_DB_Zeos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Graphics, DB, LR_Class, ZDataset, LR_DBComponent,
  LR_Intrp, ZConnection;

type
  TlrZeosData = class(TComponent)

  end;

type

  { TLRZQuery }

  TLRZQuery = class(TLRDataSetControl)
  private
    FDatabase: string;
    FParams: TfrVariables;
    procedure SetDatabase(AValue: string);
    procedure ZQueryBeforeOpen(ADataSet: TDataSet);
    procedure DoMakeParams;
    procedure DoEditParams;
  protected
    function GetSQL: TStringList;virtual;
    procedure SetSQL(AValue: TStringList);virtual;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property SQL:TStringList read GetSQL write SetSQL;
    property Database:string read FDatabase write SetDatabase;
    property Params:TfrVariables read FParams write FParams;
  end;

  { TLRZConnection }

  TLRZConnection = class(TfrNonVisualControl)
  private
    FConnected:boolean;
    FZConnection: TZConnection;
    function GetConnected: Boolean;
    function GetDatabase: string;
    function GetHostName: string;
    function GetLoginPrompt: Boolean;
    function GetPassword: string;
    function GetPort: Integer;
    function GetProtocol: string;
    function GetUser: string;
    procedure SetConnected(AValue: Boolean);
    procedure SetDatabase(AValue: string);
    procedure SetHostName(AValue: string);
    procedure SetLoginPrompt(AValue: Boolean);
    procedure SetPassword(AValue: string);
    procedure SetPort(AValue: Integer);
    procedure SetProtocol(AValue: string);
    procedure SetUser(AValue: string);
  protected
    procedure SetName(const AValue: string); override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Connected: Boolean read GetConnected write SetConnected;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property Protocol: string read GetProtocol write SetProtocol;
    property HostName: string read GetHostName write SetHostName;
    property Port: Integer read GetPort write SetPort;
    property Database: string read GetDatabase write SetDatabase;
    property User: string read GetUser write SetUser;
    property Password: string read GetPassword write SetPassword;
  end;

procedure Register;
implementation
uses LR_Utils, DBPropEdits, PropEdits, LazarusPackageIntf, ZDbcIntfs, types,
  LR_EditVariables, Forms, Controls, variants;

var
  lrBMP_ZQuery:TBitmap = nil;
  lrBMP_ZConnection:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_ZQuery) then
  begin
    lrBMP_ZQuery := TbitMap.Create;
    lrBMP_ZQuery.LoadFromLazarusResource('TLRZQuery');
    frRegisterObject(TLRZQuery, lrBMP_ZQuery, 'TLRZQuery', nil, otlUIControl, nil);
  end;
  if not assigned(lrBMP_ZConnection) then
  begin
    lrBMP_ZConnection := TbitMap.Create;
    lrBMP_ZConnection.LoadFromLazarusResource('TLRZConnection');
    frRegisterObject(TLRZConnection, lrBMP_ZConnection, 'TLRZConnection', nil, otlUIControl, nil);
  end;
end;

{ TLRZConnection }

procedure TLRZConnection.SetDatabase(AValue: string);
begin
  FZConnection.Database:=AValue;
end;

function TLRZConnection.GetDatabase: string;
begin
  Result:=FZConnection.Database;
end;

function TLRZConnection.GetConnected: Boolean;
begin
  Result:=FZConnection.Connected;
end;

function TLRZConnection.GetHostName: string;
begin
  Result:=FZConnection.HostName;
end;

function TLRZConnection.GetLoginPrompt: Boolean;
begin
  Result:=FZConnection.LoginPrompt;
end;

function TLRZConnection.GetPassword: string;
begin
  Result:=FZConnection.Password;
end;

function TLRZConnection.GetPort: Integer;
begin
  Result:=FZConnection.Port;
end;

function TLRZConnection.GetProtocol: string;
begin
  Result:=FZConnection.Protocol;
end;

function TLRZConnection.GetUser: string;
begin
  Result:=FZConnection.User;
end;

procedure TLRZConnection.SetConnected(AValue: Boolean);
begin
  FZConnection.Connected:=AValue;
end;

procedure TLRZConnection.SetHostName(AValue: string);
begin
  FZConnection.HostName:=AValue;
end;

procedure TLRZConnection.SetLoginPrompt(AValue: Boolean);
begin
  FZConnection.LoginPrompt:=AValue;
end;

procedure TLRZConnection.SetPassword(AValue: string);
begin
  FZConnection.Password:=AValue;
end;

procedure TLRZConnection.SetPort(AValue: Integer);
begin
  FZConnection.Port:=AValue;
end;

procedure TLRZConnection.SetProtocol(AValue: string);
begin
  FZConnection.Protocol:=AValue;
end;

procedure TLRZConnection.SetUser(AValue: string);
begin
  FZConnection.User:=AValue;
end;

procedure TLRZConnection.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FZConnection.Name:=Name;
end;

procedure TLRZConnection.AfterLoad;
begin
  inherited AfterLoad;
  FZConnection.Connected:=FConnected;
end;

constructor TLRZConnection.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'LRZConnection';
  FZConnection:=TZConnection.Create(OwnerForm);
end;

destructor TLRZConnection.Destroy;
begin
  if not (Assigned(OwnerPage) and (OwnerPage is TfrPageDialog)) then
  begin
    FreeAndNil(FZConnection);
  end;
  inherited Destroy;
end;

procedure TLRZConnection.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FZConnection.Port:=XML.GetValue(Path + 'Connected/Value'{%H-}, 0);
  FZConnection.LoginPrompt:=XML.GetValue(Path + 'LoginPrompt/Value'{%H-}, false);
  FZConnection.Protocol:=XML.GetValue(Path + 'Protocol/Value'{%H-}, '');

  FZConnection.HostName:=XML.GetValue(Path + 'HostName/Value'{%H-}, '');
  FZConnection.Database:=XML.GetValue(Path + 'Database/Value'{%H-}, '');
  FZConnection.User:=XML.GetValue(Path + 'User/Value'{%H-}, '');
  FZConnection.Password:=XML.GetValue(Path + 'Password/Value'{%H-}, '');
  FConnected:=XML.GetValue(Path + 'Connected/Value'{%H-}, false);
end;

procedure TLRZConnection.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path + 'Connected/Value'{%H-}, FZConnection.Port);
  XML.SetValue(Path + 'LoginPrompt/Value'{%H-}, FZConnection.LoginPrompt);
  XML.SetValue(Path + 'Protocol/Value'{%H-}, FZConnection.Protocol);

  XML.SetValue(Path + 'HostName/Value'{%H-}, FZConnection.HostName);
  XML.SetValue(Path + 'Database/Value'{%H-}, FZConnection.Database);
  XML.SetValue(Path + 'User/Value'{%H-}, FZConnection.User);
  XML.SetValue(Path + 'Password/Value'{%H-}, FZConnection.Password);
  XML.SetValue(Path + 'Connected/Value'{%H-}, FZConnection.Connected);
end;

{ TLRZQuery }

procedure TLRZQuery.SetSQL(AValue: TStringList);
begin
  if TZQuery(DataSet).SQL = AValue then Exit;
  TZQuery(DataSet).SQL.Assign(AValue);
  DoMakeParams;
end;

procedure TLRZQuery.AfterLoad;
var
  D:TComponent;
begin
  D:=frFindComponent(DataSet.Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
  begin
    TZQuery(DataSet).Connection:=TZConnection(D);
    DataSet.Active:=FActive;
  end;
end;

procedure TLRZQuery.SetDatabase(AValue: string);
var
  D:TComponent;
begin
  if FDatabase=AValue then Exit;
  FDatabase:=AValue;

  DataSet.Active:=false;
  D:=frFindComponent(TZQuery(DataSet).Owner, FDatabase);
  if Assigned(D) and (D is TZConnection)then
    TZQuery(DataSet).Connection:=TZConnection(D);
end;

procedure TLRZQuery.ZQueryBeforeOpen(ADataSet: TDataSet);
var
  i: Integer;
  s: String;
  SaveView: TfrView;
  SavePage: TfrPage;
  SaveBand: TfrBand;
  Q:TZQuery;
begin
  Q:=TZQuery(DataSet);
  SaveView := CurView;
  SavePage := CurPage;
  SaveBand := CurBand;

  CurView := nil;
  CurPage := OwnerPage;
  CurBand := nil;

  for i := 0 to Q.Params.Count - 1 do
  begin
    s := Trim(VarToStr(FParams[Q.Params[i].Name]));
    if (s <> '') and (DocMode = dmPrinting) then
      Q.Params[i].AsString := frParser.Calc(s);
  end;

  if Assigned(Q.Connection) then
    if not Q.Connection.Connected then Q.Connection.Connect;

  CurView := SaveView;
  CurPage := SavePage;
  CurBand := SaveBand;
end;

procedure TLRZQuery.DoMakeParams;
var
  Q:TZQuery;
  i:integer;
begin
  Q:=TZQuery(DataSet);
  if Q.Params.Count > 0 then
  begin
    //Add new params...
    for i:=0 to Q.Params.Count-1 do
      if FParams.IndexOf(Q.Params[i].Name)<0 then
        FParams[Q.Params[i].Name]:='';

    //Delete not exists params
    for i:=FParams.Count-1 downto 0 do
      if not Assigned(Q.Params.FindParam(FParams.Name[i])) then
        FParams.Delete(i);
  end
  else
    FParams.Clear;
end;

procedure TLRZQuery.DoEditParams;
begin
  lrEditVariablesForm:=TlrEditVariablesForm.Create(Application);
  lrEditVariablesForm.LoadParamList(FParams);
  if lrEditVariablesForm.ShowModal = mrOk then
  begin
    lrEditVariablesForm.SaveParamList(FParams);
    frDesigner.Modified:=true;
  end;
  lrEditVariablesForm.Free;
end;

function TLRZQuery.GetSQL: TStringList;
begin
  Result:=TZQuery(DataSet).SQL as TStringList;
end;

constructor TLRZQuery.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrZQuery';
  FParams:=TfrVariables.Create;

  DataSet:=TZQuery.Create(OwnerForm);
  DataSet.BeforeOpen:=@ZQueryBeforeOpen;
end;

destructor TLRZQuery.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TLRZQuery.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  C, I:integer;
begin
  inherited LoadFromXML(XML, Path);
  SQL.Text  := XML.GetValue(Path+'SQL/Value'{%H-}, '');
  FDatabase:= XML.GetValue(Path+'Database/Value'{%H-}, '');


  C:=XML.GetValue(Path+'Params/Count/Value', 0);
  for i:=0 to C-1 do
    FParams[XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Name', '')] := XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Value', '');
end;

procedure TLRZQuery.SaveToXML(XML: TLrXMLConfig; const Path: String);
var
  i:integer;
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'SQL/Value', SQL.Text);
  XML.SetValue(Path+'Database/Value', FDatabase);

  XML.SetValue(Path+'Params/Count/Value', FParams.Count);
  for i:=0 to FParams.Count-1 do
  begin
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Name', FParams.Name[i]);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Value', String(FParams.Value[i]));
  end;
end;


type

  { TLRZQueryParamsProperty }

  TLRZQueryParamsProperty = class(TPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

  { TLRZQueryDataBaseProperty }

  TLRZQueryDataBaseProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

  { TLRZConnectionProtocolProperty }

  TLRZConnectionProtocolProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;

{ TLRZQueryParamsProperty }

function TLRZQueryParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TLRZQueryParamsProperty.GetValue: ansistring;
begin
  Result:='(Params)';
end;

procedure TLRZQueryParamsProperty.Edit;
begin
  if (GetComponent(0) is TLRZQuery) then
    TLRZQuery(GetComponent(0)).DoEditParams;
end;

{ TLRZConnectionProtocolProperty }

procedure TLRZConnectionProtocolProperty.FillValues(const Values: TStringList);
var
  i, j:integer;
  A:TStringDynArray;
begin
  if (GetComponent(0) is TLRZConnection) and Assigned(DriverManager) then
  begin
    for i:=0 to DriverManager.GetDrivers.GetCount-1 do
    begin
      A:=(DriverManager.GetDrivers.Items[i] as IZDriver).GetSupportedProtocols;
      for j:=Low(A) to High(A) do
        Values.Add(A[j]);
    end;
  end;
end;

{ TLRZQueryDataBaseProperty }
procedure TLRZQueryDataBaseProperty.FillValues(const Values: TStringList);
begin
  if (GetComponent(0) is TLRZQuery) then
    frGetComponents(nil, TZConnection, Values, nil);
end;


procedure RegisterLR_DB_Zeos;
begin
  RegisterComponents('LazReport',[TlrZeosData]);
end;

procedure Register;
begin
  RegisterUnit('LR_DB_Zeos', @RegisterLR_DB_Zeos);
end;


initialization
  {$I lr_zeos_img.inc}
  InitLRComp;

  RegisterPropertyEditor(TypeInfo(string), TLRZQuery, 'Database', TLRZQueryDataBaseProperty);
  RegisterPropertyEditor(TypeInfo(TfrVariables), TLRZQuery, 'Params', TLRZQueryParamsProperty);

  RegisterPropertyEditor(TypeInfo(string), TLRZConnection, 'Protocol', TLRZConnectionProtocolProperty);

finalization
  if Assigned(lrBMP_ZQuery) then
    FreeAndNil(lrBMP_ZQuery);
  if Assigned(lrBMP_ZConnection) then
    FreeAndNil(lrBMP_ZConnection);
end.

