unit lr_SQLQuery;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LR_Class, LR_DBComponent, sqldb, DB, contnrs;

type

  TQueryParam = class
    ParamType:TFieldType;
    ParamName:string;
    ParamValue:string;
  end;

  { TQueryParamList }

  TQueryParamList = class(TFPObjectList)
    function ParamByName(AParamName:string):TQueryParam;
    function Add(AParamType:TFieldType; const AParamName, AParamValue:string):TQueryParam;
  end;

  { TLRSQLQuery }

  TLRSQLQuery = class(TLRDataSetControl)
  private
    FDatabase: string;
    FParams: TQueryParamList;
    procedure SetDatabase(AValue: string);
    procedure DoMakeParams;
    procedure DoEditParams;
    procedure SQLQueryBeforeOpen(ADataSet: TDataSet);
  protected
    function GetSQL: string;
    procedure SetSQL(AValue:string);
    procedure SetDataSource(AValue: string); override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property SQL:string read GetSQL write SetSQL;
    property Database:string read FDatabase write SetDatabase;
    property Params:TQueryParamList read FParams write FParams;
  end;

  { TLRSQLConnection }

  TLRSQLConnection = class(TfrNonVisualControl)
  private
    FConnected:boolean;
    function GetCharSet: string;
    function GetConnected: Boolean;
    function GetDatabase: string;
    function GetHostName: string;
    function GetLoginPrompt: Boolean;
    function GetPassword: string;
    function GetUserName: string;
    procedure SetCharSet(AValue: string);
    procedure SetConnected(AValue: Boolean);
    procedure SetDatabase(AValue: string);
    procedure SetHostName(AValue: string);
    procedure SetLoginPrompt(AValue: Boolean);
    procedure SetPassword(AValue: string);
    procedure SetUserName(AValue: string);
  protected
    FConnection: TSQLConnection;
    FSQLTransaction:TSQLTransaction;
    procedure SetName(const AValue: string); override;
    procedure AfterLoad;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property CharSet: string read GetCharSet write SetCharSet;
    property Connected: Boolean read GetConnected write SetConnected;
    property LoginPrompt: Boolean read GetLoginPrompt write SetLoginPrompt;
    property HostName: string read GetHostName write SetHostName;
    property DatabaseName: string read GetDatabase write SetDatabase;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
  end;


implementation

{$R lrsqldb_img.res}

uses LR_Utils, DBPropEdits, PropEdits, Controls, Forms,
  lr_EditSQLDBParamsUnit;

var
  lrBMP_SQLQuery:TBitmap = nil;

procedure InitLRComp;
begin
  if not assigned(lrBMP_SQLQuery) then
  begin
    lrBMP_SQLQuery := TbitMap.Create;
    lrBMP_SQLQuery.LoadFromResourceName(HInstance, 'TLRSQLQuery');
    frRegisterObject(TLRSQLQuery, lrBMP_SQLQuery, 'TLRSQLQuery', nil, otlUIControl, nil);
  end;
end;

{ TQueryParamList }

function TQueryParamList.ParamByName(AParamName: string): TQueryParam;
var
  i:integer;
begin
  Result:=nil;
  AParamName:=UpperCase(AParamName);
  for i:=0 to Count - 1 do
  begin
    if UpperCase(TQueryParam(Items[i]).ParamName) = AParamName then
    begin
      Result:=TQueryParam(Items[i]);
      exit;
    end;
  end;
end;

function TQueryParamList.Add(AParamType: TFieldType; const AParamName,
  AParamValue: string): TQueryParam;
begin
  Result:=TQueryParam.Create;
  inherited Add(Result);
  Result.ParamType:=AParamType;
  Result.ParamName:=AParamName;
  Result.ParamValue:=AParamValue;
end;

{ TLRSQLConnection }

function TLRSQLConnection.GetCharSet: string;
begin
  Result:=FConnection.CharSet;
end;

function TLRSQLConnection.GetConnected: Boolean;
begin
  Result:=FConnection.Connected;
end;

function TLRSQLConnection.GetDatabase: string;
begin
  Result:=FConnection.DatabaseName;
end;

function TLRSQLConnection.GetHostName: string;
begin
  Result:=FConnection.HostName;
end;

function TLRSQLConnection.GetLoginPrompt: Boolean;
begin
  Result:=FConnection.LoginPrompt;
end;

function TLRSQLConnection.GetPassword: string;
begin
  Result:=FConnection.Password;
end;

function TLRSQLConnection.GetUserName: string;
begin
  Result:=FConnection.UserName;
end;

procedure TLRSQLConnection.SetCharSet(AValue: string);
begin
  FConnection.CharSet:=AValue;
end;

procedure TLRSQLConnection.SetConnected(AValue: Boolean);
begin
  FConnection.Connected:=AValue;
end;

procedure TLRSQLConnection.SetDatabase(AValue: string);
begin
  FConnection.DatabaseName:=AValue;
end;

procedure TLRSQLConnection.SetHostName(AValue: string);
begin
  FConnection.HostName:=AValue;
end;

procedure TLRSQLConnection.SetLoginPrompt(AValue: Boolean);
begin
  FConnection.LoginPrompt:=AValue;
end;

procedure TLRSQLConnection.SetPassword(AValue: string);
begin
  FConnection.Password:=AValue;
end;

procedure TLRSQLConnection.SetUserName(AValue: string);
begin
  FConnection.UserName:=AValue;
end;

procedure TLRSQLConnection.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FConnection.Name:=Name;
end;

procedure TLRSQLConnection.AfterLoad;
begin
  inherited AfterLoad;
  FConnection.Connected:=FConnected;
end;

constructor TLRSQLConnection.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FSQLTransaction:=TSQLTransaction.Create(OwnerForm);
end;

destructor TLRSQLConnection.Destroy;
begin
  if not (Assigned(OwnerPage) and (OwnerPage is TfrPageDialog)) then
  begin
    FreeAndNil(FSQLTransaction);
    FreeAndNil(FConnection);
  end;
  inherited Destroy;
end;

procedure TLRSQLConnection.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FConnection.LoginPrompt:=XML.GetValue(Path + 'LoginPrompt/Value'{%H-}, false);
  FConnection.CharSet:=XML.GetValue(Path + 'CharSet/Value'{%H-}, '');

  FConnection.HostName:=XML.GetValue(Path + 'HostName/Value'{%H-}, '');
  FConnection.DatabaseName:=XML.GetValue(Path + 'Database/Value'{%H-}, '');
  FConnection.UserName:=XML.GetValue(Path + 'User/Value'{%H-}, '');
  FConnection.Password:=XML.GetValue(Path + 'Password/Value'{%H-}, '');

  FConnected:=XML.GetValue(Path + 'Connected/Value'{%H-}, false);
end;

procedure TLRSQLConnection.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path + 'LoginPrompt/Value'{%H-}, FConnection.LoginPrompt);
  XML.SetValue(Path + 'CharSet/Value'{%H-}, FConnection.CharSet);

  XML.SetValue(Path + 'HostName/Value'{%H-}, FConnection.HostName);
  XML.SetValue(Path + 'Database/Value'{%H-}, FConnection.DatabaseName);
  XML.SetValue(Path + 'User/Value'{%H-}, FConnection.UserName);
  XML.SetValue(Path + 'Password/Value'{%H-}, FConnection.Password);
  XML.SetValue(Path + 'Connected/Value'{%H-}, FConnection.Connected);
end;

{ TLRSQLQuery }

procedure TLRSQLQuery.SetDatabase(AValue: string);
var
  D:TComponent;
begin
  if FDatabase=AValue then Exit;
  FDatabase:=AValue;

  DataSet.Active:=false;
  D:=frFindComponent(TSQLQuery(DataSet).Owner, FDatabase);
  if Assigned(D) and (D is TSQLConnection)then
    TSQLQuery(DataSet).DataBase:=TSQLConnection(D);
end;

procedure TLRSQLQuery.DoMakeParams;
var
  Q:TSQLQuery;
  i:integer;
begin
  Q:=TSQLQuery(DataSet);
  if Q.Params.Count > 0 then
  begin
    //Add new params...
    for i:=0 to Q.Params.Count-1 do
    begin
      if not Assigned(FParams.ParamByName(Q.Params[i].Name)) then
        FParams.Add(ftUnknown, Q.Params[i].Name, '');
    end;

    //Delete not exists params
    for i:=FParams.Count-1 downto 0 do
    begin
      if not Assigned(Q.Params.FindParam(TQueryParam(FParams[i]).ParamName)) then
        FParams.Delete(i);
    end;
  end
  else
    FParams.Clear;
end;

procedure TLRSQLQuery.DoEditParams;
var
  lrEditParamsForm: Tlr_EditSQLDBParamsForm;
begin
  lrEditParamsForm:=Tlr_EditSQLDBParamsForm.Create(Application);
  lrEditParamsForm.LoadParamList(FParams);
  if lrEditParamsForm.ShowModal = mrOk then
  begin
    lrEditParamsForm.SaveParamList(FParams);
    if Assigned(frDesigner) then
      frDesigner.Modified:=true;
  end;
  lrEditParamsForm.Free;
end;

procedure TLRSQLQuery.SQLQueryBeforeOpen(ADataSet: TDataSet);
var
  i: Integer;
  s: String;
  SaveView: TfrView;
  SavePage: TfrPage;
  SaveBand: TfrBand;
  Q:TSQLQuery;
  P:TQueryParam;
begin
  Q:=TSQLQuery(DataSet);
  SaveView := CurView;
  SavePage := CurPage;
  SaveBand := CurBand;

  CurView := Self;
  CurPage := OwnerPage;
  CurBand := nil;

  for i := 0 to Q.Params.Count - 1 do
  begin
    S:=Q.Params[i].Name;
    P:=FParams.ParamByName(S);
    if Assigned(P) and (P.ParamValue <> '') and (DocMode = dmPrinting) then
    begin
      case P.ParamType of
        ftDate,
        ftDateTime:Q.Params[i].AsDateTime := frParser.Calc(P.ParamValue);
        ftInteger:Q.Params[i].AsInteger := frParser.Calc(P.ParamValue);
        ftFloat:Q.Params[i].AsFloat := frParser.Calc(P.ParamValue);
        ftString:Q.Params[i].AsString := frParser.Calc(P.ParamValue);
      else
        Q.Params[i].Value := frParser.Calc(P.ParamValue);
      end;
    end;
  end;

  if Assigned(Q.DataBase) then
    if not Q.DataBase.Connected then Q.DataBase.Connected:=true;

  CurView := SaveView;
  CurPage := SavePage;
  CurBand := SaveBand;
end;

function TLRSQLQuery.GetSQL: string;
begin
  Result:=TSQLQuery(DataSet).SQL.Text;
end;

procedure TLRSQLQuery.SetSQL(AValue: string);
begin
  DataSet.Active:=false;
  TSQLQuery(DataSet).SQL.Text:=AValue;
  DoMakeParams;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TLRSQLQuery.SetDataSource(AValue: string);
var
  D:TComponent;
begin
  inherited SetDataSource(AValue);
  D:=frFindComponent(OwnerForm, AValue);
  if Assigned(D) and (D is TDataSource)then
    TSQLQuery(DataSet).DataSource:=TDataSource(D);
end;

procedure TLRSQLQuery.AfterLoad;
var
  D:TComponent;
begin
  D:=frFindComponent(OwnerForm, DataSource);
  if Assigned(D) and (D is TDataSource)then
    TSQLQuery(DataSet).DataSource:=TDataSource(D);

  D:=frFindComponent(DataSet.Owner, FDatabase);
  if Assigned(D) and (D is TSQLConnection)then
  begin
    TSQLQuery(DataSet).DataBase:=TSQLConnection(D);
    DataSet.Active:=FActive;
  end;
end;

constructor TLRSQLQuery.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrSQLQuery';
  DataSet:=TSQLQuery.Create(OwnerForm);
  DataSet.BeforeOpen:=@SQLQueryBeforeOpen;
  FParams:=TQueryParamList.Create;
end;

destructor TLRSQLQuery.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function StrToFieldType(AStrTypeName:string):TFieldType;
var
  i:TFieldType;
begin
  Result:=ftUnknown;
  AStrTypeName:=UpperCase(AStrTypeName);
  for i in TFieldType do
  begin
    if UpperCase(Fieldtypenames[i]) = AStrTypeName then
    begin
      Result:=i;
      exit;
    end;
  end;
end;

procedure TLRSQLQuery.LoadFromXML(XML: TLrXMLConfig; const Path: String);
var
  C: Integer;
  i: Integer;
begin
  inherited LoadFromXML(XML, Path);
  TSQLQuery(DataSet).SQL.Text  := XML.GetValue(Path+'SQL/Value'{%H-}, '');
  FDatabase:= XML.GetValue(Path+'Database/Value'{%H-}, '');

  C:=XML.GetValue(Path+'Params/Count/Value', 0);
  for i:=0 to C-1 do
    FParams.Add(
        StrToFieldType(XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/ParamType', '')),
        XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Name', ''),
        XML.GetValue(Path+'Params/Item'+IntToStr(i)+'/Value', '')
        );
end;

procedure TLRSQLQuery.SaveToXML(XML: TLrXMLConfig; const Path: String);
var
  i: Integer;
  P: TQueryParam;
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'SQL/Value', TSQLQuery(DataSet).SQL.Text);
  XML.SetValue(Path+'Database/Value', FDatabase);

  XML.SetValue(Path+'Params/Count/Value', FParams.Count);
  for i:=0 to FParams.Count-1 do
  begin
    P:=TQueryParam(FParams[i]);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Name', P.ParamName);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/Value', P.ParamValue);
    XML.SetValue(Path+'Params/Item'+IntToStr(i)+'/ParamType', Fieldtypenames[P.ParamType]);
  end;
end;

type

  { TLRSQLQueryParamsProperty }

  TLRSQLQueryParamsProperty = class(TPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;


  TLRSQLConnectionProtocolProperty = class(TFieldProperty)
  public
    procedure FillValues(const Values: TStringList); override;
  end;


  { TLRSQLQuerySQLProperty }

  TLRSQLQuerySQLProperty = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

{ TLRSQLQueryParamsProperty }

function TLRSQLQueryParamsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TLRSQLQueryParamsProperty.GetValue: ansistring;
begin
  Result:='(Params)';
end;

procedure TLRSQLQueryParamsProperty.Edit;
begin
  if (GetComponent(0) is TLRSQLQuery) then
    TLRSQLQuery(GetComponent(0)).DoEditParams;
end;

{ TLRSQLQuerySQLProperty }

function TLRSQLQuerySQLProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paReadOnly];
end;

function TLRSQLQuerySQLProperty.GetValue: ansistring;
begin
  Result:='(SQL)';
end;

procedure TLRSQLQuerySQLProperty.Edit;
var
  TheDialog : TStringsPropEditorDlg;
  AString : string;
begin
  AString := GetStrValue;
  TheDialog := TStringsPropEditorDlg.Create(nil);
  try
    TheDialog.Editor := Self;
    TheDialog.Memo.Text := AString;
    TheDialog.MemoChange(nil);
    if (TheDialog.ShowModal = mrOK) then
    begin
      AString := TheDialog.Memo.Text;
      //erase the last lineending if any
      if Copy(AString, length(AString) - length(LineEnding) + 1, length(LineEnding)) = LineEnding then
        Delete(AString, length(AString) - length(LineEnding) + 1, length(LineEnding));
      SetStrValue(AString);
    end;
  finally
    TheDialog.Free;
  end;
end;

{ TLRZConnectionProtocolProperty }

procedure TLRSQLConnectionProtocolProperty.FillValues(const Values: TStringList);
begin
  if (GetComponent(0) is TLRSQLQuery) then
    frGetComponents(nil, TSQLConnection, Values, nil);
end;

initialization
  InitLRComp;

  RegisterPropertyEditor(TypeInfo(string), TLRSQLQuery, 'Database', TLRSQLConnectionProtocolProperty);
  RegisterPropertyEditor(TypeInfo(string), TLRSQLQuery, 'SQL', TLRSQLQuerySQLProperty);

  RegisterPropertyEditor(TypeInfo(TQueryParamList), TLRSQLQuery, 'Params', TLRSQLQueryParamsProperty);
finalization
  if Assigned(lrBMP_SQLQuery) then
    FreeAndNil(lrBMP_SQLQuery);
end.

