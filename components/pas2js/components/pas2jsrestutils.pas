unit pas2jsrestutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, stub.restdataset, bufdataset, fphttpclient;

Type

  { TPas2JSRestUtils }
  TLogEvent = Procedure(Sender: TObject; Const Msg : String) of object;
  TPas2JSRestUtils = class(TComponent)
  private
    FLogEnabled: Boolean;
    FOnLog: TLogEvent;
    FShowRaw: Boolean;
    function GetFieldType(aType: String): TFieldType;
  Protected
    Procedure DoLog(Const Msg : String); overload;
    Procedure DoLog(Const Fmt : String; Const Args : Array of const); overload;
    function LoadDataset(aDataset : TBufDataset; const aURL : String; const aUserName,aPassword : string) : Boolean;
    function LoadDataset(aDataset: TBufDataset; aConnection: TSQLDBRestConnection; aResource: string) : Boolean;
  Public
    Procedure GetResourceList(aConnection : TSQLDBRestConnection; aList : TStrings; {%H-}aScheme : String = '');
    Procedure GetConnectionList(aConnection : TSQLDBRestConnection; aList : TStrings; {%H-}aScheme : String = '');
    function UpdateFieldDefs(aConnection: TSQLDBRestConnection; aResource: String;
      aFieldDefs: TFieldDefs): Integer;
    Procedure GetDatasetData(aDataset : TSQLDBRestDataset; aBuf : TBufDataset);
    Function GetFullResourceName(aDataset : TSQLDBRestDataset) : String;
    Property OnLog : TLogEvent read FOnLog write FOnLog;
    Property LogEnabled : Boolean Read FLogEnabled Write FLogEnabled;
    Property ShowRaw : Boolean Read FShowRaw Write FShowRaw;
  end;

Type

  { TLocalBufDataset }

  TLocalBufDataset = Class(TBufDataset)
    procedure LoadBlobIntoBuffer(FieldDef: TFieldDef;ABlobBuf: PBufBlobField); override;
  end;

Var
  IDERestUtils : TPas2JSRestUtils;

implementation

uses XMLDatapacketReader;

{ TPas2JSRestUtils }


{ TLocalBufDataset }

procedure TLocalBufDataset.LoadBlobIntoBuffer(FieldDef: TFieldDef;
  ABlobBuf: PBufBlobField);
begin
  If Assigned(FieldDef) and Assigned(ABlobBuf) then ;
end;


procedure TPas2JSRestUtils.DoLog(const Msg: String);
begin
  if FLogEnabled and Assigned(FOnLog) then
    FOnLog(Self,Msg);
end;

procedure TPas2JSRestUtils.DoLog(const Fmt: String; const Args: array of const);
begin
  DoLog(Format(Fmt,Args));
end;

function TPas2JSRestUtils.LoadDataset(aDataset: TBufDataset;
  const aURL: String; const aUserName, aPassword: string) : Boolean;

Var
  C : TFPHTTPClient;
  S : TStringStream;
  U : String;

begin
  Result:=False;
  DoLog('Loading data from URL %s (user: %s, password: %s)',[aURL,aUserName,StringOfChar('*',Length(aPAssword))]);
  U:=aURL;
  S:=Nil;
  C:=TFPHTTPClient.Create(Self);
  try
    C.UserName:=aUserName;
    C.Password:=aPassword;
    S:=TStringStream.Create('');
    if Pos('?',U)=0 then
      U:=U+'?'
    else
      U:=U+'&';
    U:=U+'fmt=buf';
    try
      C.Get(U,S);
    except
      On E : Exception do
        begin
        DoLog('Exception %s when getting data from url "%s": %s',[E.ClassName,U,E.Message]);
        Exit;
        end;
    end;
    if FShowRaw then
      begin
      DoLog('Raw request data:');
      DoLog('---');
      DoLog(S.Datastring);
      DoLog('---');
      end;
    S.Position:=0;
    aDataset.LoadFromStream(S,dfXML);
    Result:=True;
  finally
    S.Free;
    C.Free;
  end;
end;

function TPas2JSRestUtils.LoadDataset(aDataset : TBufDataset; aConnection: TSQLDBRestConnection; aResource : string) : Boolean;

Var
  aURL : String;

begin
  aURL:=aConnection.BaseURL;
  aURL:=aURL+aResource;
  Result:=LoadDataset(aDataset,aURL,aConnection.UserName,aConnection.Password);
end;

procedure TPas2JSRestUtils.GetResourceList(aConnection: TSQLDBRestConnection;
  aList: TStrings; aScheme: String);

Var
  Buf : TBufDataset;

begin
  Buf:=TLocalBufDataset.Create(Self);
  try
    if not LoadDataset(Buf,AConnection,aConnection.MetaDataResourceName) then
      exit;
    if (aConnection.CustomViewResourceName<>'') then
      aList.Add(aConnection.CustomViewResourceName);
    if (aConnection.MetaDataResourceName<>'') then
      aList.Add(aConnection.MetaDataResourceName);
    Buf.Open;
    While not Buf.EOF do
      begin
      aList.Add(Buf.FieldByName('name').AsString);
      Buf.Next;
      end;
  finally
    Buf.Free;
  end;
end;

procedure TPas2JSRestUtils.GetConnectionList(aConnection: TSQLDBRestConnection;
  aList: TStrings; aScheme: String);
Var
  Buf : TBufDataset;

begin
  Buf:=TLocalBufDataset.Create(Self);
  try
    LoadDataset(Buf,AConnection,aConnection.ConnectionsResourceName);
    Buf.Open;
    While not Buf.EOF do
      begin
      aList.Add(Buf.FieldByName('name').AsString);
      Buf.Next;
      end;
  finally
    Buf.Free;
  end;
end;

function TPas2JSRestUtils.GetFieldType(aType: String): TFieldType;

begin
  Case lowerCase(aType) of
    'text' : Result:=ftString;
    'datetime' : result:=ftDateTime;
    'date' : Result:=ftDate;
    'time' : Result:=ftTime;
    'float' : Result:=ftFloat;
    'bigint' : Result:=ftLargeInt;
    'int' : Result:=ftInteger;
    'blob' : Result:=ftBlob;
    'bool' : Result:=ftBoolean;
  else
    Result:=ftString;
  end;
end;

Function TPas2JSRestUtils.UpdateFieldDefs(aConnection: TSQLDBRestConnection;
  aResource: String; aFieldDefs: TFieldDefs) : Integer;
Var
  Buf : TBufDataset;
  aName : TField;
  aMaxLen : TField;
  aType : TField;
  aRequired : TField;

begin
  Result:=0;
  Buf:=TLocalBufDataset.Create(Self);
  try
    if not LoadDataset(Buf,AConnection,aConnection.MetaDataResourceName+'/'+aResource) then
       Exit(-1);
    Buf.Open;
    aName:=Buf.FieldByName('name');
    aType:=Buf.FieldByName('type');
    aMaxLen:=Buf.FieldByName('maxlen');
    aRequired:=Buf.FieldByName('required');
    aFieldDefs.BeginUpdate;
    While not Buf.EOF do
      begin
      if aFieldDefs.Find(aName.AsString)=nil then
        begin
        aFieldDefs.Add(aName.AsString,GetFieldType(aType.AsString),aMaxLen.asInteger,aRequired.AsBoolean);
        Inc(Result);
        end;
      Buf.Next;
      end;
  finally
    aFieldDefs.EndUpdate;
    Buf.Free;
  end;
end;

procedure TPas2JSRestUtils.GetDatasetData(aDataset: TSQLDBRestDataset;
  aBuf: TBufDataset);

begin
  LoadDataset(aBuf,aDataset.Connection,GetFullResourceName(aDataset));
end;

function TPas2JSRestUtils.GetFullResourceName(aDataset: TSQLDBRestDataset
  ): String;
begin
  Result:=aDataset.ResourceName;
  if (aDataset.DatabaseConnection<>'') then
    Result:=aDataset.DatabaseConnection+'/'+Result;
end;

initialization
  IDERestUtils:=TPas2JSRestUtils.Create(Nil);

Finalization
  FreeAndNil(IDERestUtils);

end.

