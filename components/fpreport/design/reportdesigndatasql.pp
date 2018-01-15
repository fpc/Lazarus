{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Report Designer Data connector for SQLDB based data.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit reportdesigndatasql;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils, sqldb, db, fpjson;

Const
  keyConnection   = 'connection';
  keySQL          = 'sql';
  keyType         = 'dbtype';
  keyHostName     = 'host';
  keyDatabaseName = 'database';
  keyUserName     = 'user';
  keyPassword     = 'pwd';
  keyRole         = 'role';
  keyParams       = 'params';
  KeyCharSet      = 'charset';
  keyHash         = 'FPCRulez';

Resourcestring
  SErrNoConnectionData = 'No connection data available';

Type

  { TFPReportConnector }

  TFPReportConnector = Class(TSQLConnector)
  Private
    FRefCount: Integer;
    Class procedure init;
    class procedure done;
    Class var
      FPool : TStringList;
  Public
    Procedure LoadFromConfig(aConfig : TJSONObject);
    class function CreateConnection(aConfig: TJSONObject): TFPReportConnector;
    Class Function TestConnection (aConfig : TJSONObject) : string;
    class function CreateDataset(aOwner: TComponent; aConfig: TJSONObject): TSQLQuery;
    class function CreateConfigHash(aConfig: TJSONObject): String;
    Class procedure CheckDBRelease;
    Property RefCount : Integer Read FRefCount;
  end;

  { TFPReportQuery }

  TFPReportQuery = class(TSQLQuery)
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  end;

implementation

{ TFPReportQuery }

constructor TFPReportQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly:=True;
end;

destructor TFPReportQuery.Destroy;

begin
  If Database is TFPReportConnector then
    Dec(TFPReportConnector(Database).FRefCount);
  inherited Destroy;
  TFPReportConnector.CheckDBRelease;
end;

{ TFPReportConnector }

class procedure TFPReportConnector.init;
begin
  FPool:=TStringList.Create;
  FPool.OwnsObjects:=True;
  FPool.Sorted:=True;
  FPool.Duplicates:=dupError;
end;

class procedure TFPReportConnector.done;
begin
  FreeAndNil(FPool);
end;

Class Function TFPReportConnector.CreateConfigHash(aConfig : TJSONObject) : String;

  Procedure AH(N,V : String);

  begin
    if (V<>'') then
      Result:=Result+';'+N+'='+V;
  end;

  Procedure AH(N : String);


  begin
    AH(N,aConfig.get(N,''));
  end;

Var
  A : TJSONArray;
  I : Integer;

begin
  AH(keyType);
  AH(keyHostName);
  AH(keyDatabaseName);
  AH(keyUserName);
  AH(keyPassword);
  AH(keyRole);
  A:=aConfig.get(keyParams,TJSONArray(Nil));
  If Assigned(A) then
    For I:=0 to A.Count-1 do
      AH(IntToStr(I),A.Strings[i]);
end;

class procedure TFPReportConnector.CheckDBRelease;

Var
  I : Integer;

begin
  For I:=FPool.Count-1 downto 0 do
    begin
    Writeln('Connection count for ',FPool[i], ' : ',TFPReportConnector(FPool.Objects[i]).FRefCount);
    if TFPReportConnector(FPool.Objects[i]).FRefCount=0 then
      FPool.Delete(I);
    end;
end;

procedure TFPReportConnector.LoadFromConfig(aConfig: TJSONObject);

Var
  S : String;
  A : TJSONArray;
  I : Integer;

begin
  ConnectorType:=aConfig.get(keyType,'');
  HostName:=aConfig.get(keyHostName,'');
  DatabaseName:=aConfig.get(keyDatabaseName,'');
  UserName:=aConfig.get(keyUserName,'');
  S:=aConfig.get(keyPassword,'');
  if (S<>'') then
    Password:=XORDecode(keyHash,S);
  Role:=aConfig.get(keyRole,'');
  Params.Clear;
  A:=aConfig.get(keyParams,TJSONArray(Nil));
  If Assigned(A) then
    For I:=0 to A.Count-1 do
      Params.Add(A.Strings[i]);
end;

class function TFPReportConnector.CreateConnection(aConfig: TJSONObject): TFPReportConnector;

begin
  Result:=Self.Create(Nil);
  Result.LoadFromConfig(aConfig);
  Result.Transaction:=TSQLtransaction.Create(Result);
end;

class function TFPReportConnector.TestConnection(aConfig: TJSONObject): string;

Var
  C : TFPReportConnector;

begin
  Result:='';
  C:=CreateConnection(aConfig);
  try
    C.Connected:=True;
  except
    On E : Exception do
      Result:=E.Message;
  end;
  C.free;
end;

class function TFPReportConnector.CreateDataset(aOwner: TComponent; aConfig: TJSONObject): TSQLQuery;

Var
  S : String;
  C : TFPReportConnector;
  I : integer;
  O : TJSONObject;

begin
  O:=aConfig.Get(keyConnection,TJSONObject(Nil));
  if O=Nil then
    Raise EDatabaseError.Create(SErrNoConnectionData);
  S:=CreateConfigHash(o);
  i:=FPool.IndexOf(S);
  if (I<>-1) then
    C:=FPool.Objects[i] as TFPReportConnector
  else
    begin
    C:=CreateConnection(o);
    FPool.AddObject(S,C);
    end;
  Result:=TFPReportQuery.Create(aOwner);
  Result.Database:=C;
  Result.SQL.Text:=aConfig.get(keySQL,'');
  Inc(C.FRefCount);
end;


initialization
  TFPReportConnector.Init;
Finalization
  TFPReportConnector.Done;
end.

