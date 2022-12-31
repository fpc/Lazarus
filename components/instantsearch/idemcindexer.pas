unit idemcindexer;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Types, DB, SQLDB, MySQL80Conn, fpJSON;

Const
  DefaultMCMySQLPort   = 9306;
  DefaultMCHTTPPort    = 9308;
  DefaultMCBinaryPort  = 9312;
  DefaultMCIndexName   = 'sources';
  DefaultMCMinInfixLen = 3;

Type
  EManticoreSearch = Class(Exception)
    ManticoreCommand : String;
  end;

  TMCIndexOption = (ioRecurse,ioStoreRelativeNames,ioAllFiles);
  TMCIndexOptions = set of TMCIndexOption;

  TMCTransport = (mctNone,mctMysql,mctHttp);
  TMCMySQLClientVersion = (mcvNone,mcv57,mcv80);

  TMCLogkind = (mlkError,mlkInfo,mlkDebug,mlkProgress);
  TMCLogEvent = Procedure(Sender : TObject; aKind : TMCLogKind; const aMessage : String) of object;
  TIndexProgressEvent = Procedure(Sender : TObject; const aFileName : String; var aContinue : Boolean) of object;

  { TMCSearchResult }

  TMCSearchResult = record
    ID : Int64;
    Tree : string;
    FileName : string;
    LineNo : Integer;
    Content : String;
    Weight : Integer;
    Function Description(aFull: Boolean = False) : string;
  end;
  PMCSearchResult = ^TMCSearchResult;
  TMCSearchResultArray = Array of TMCSearchResult;
  TMCSearchResultCallBack = Procedure (Sender : TObject; const aResult : TMCSearchResult; aData : Pointer; var aContinue : Boolean) of object;

  { TMCToArrayConverter }

  TMCToArrayConverter = class
    Results : TMCSearchResultArray;
    Count : Cardinal;
    FGrowDelta : Cardinal;
    constructor Create(aGrowDelta : Cardinal);
    Procedure DoAddToResult(Sender : TObject; const aResult : TMCSearchResult; aData : Pointer; var aContinue : Boolean);
  end;


  { TManticoreSearchSources }

  TManticoreSearchSources = class(TComponent)
  private
    FConnected : Boolean;
    FExtensions: TStringDynArray;
    FHostName: String;
    FIndexName: String;
    FLimit: Cardinal;
    FMinInfixLen: Integer;
    FMySQLVersion: TMCMySQLClientVersion;
    FOnLog: TMCLogEvent;
    FPort: Word;
    FProtocol: TMCTransport;
    FMySQLConn : TSQLConnector;
    FTrans : TSQLTransaction;
    procedure SetHostName(AValue: String);
    procedure SetIndexName(AValue: String);
    procedure SetMySQLVersion(AValue: TMCMySQLClientVersion);
    procedure SetPort(AValue: Word);
    procedure SetProtocol(AValue: TMCTransport);
  Protected
    // Check if we are disconnected. If not, raise an exception
    Procedure CheckDisconnected;
    // Check if we are connected. If not, raise an exception..
    Procedure CheckConnected;
    // Do logging
    Procedure DoLog(aKind : TMCLogKind; Const aMessage : string);
    Procedure DoLog(aKind : TMCLogKind; Const aFmt : String; Const aArgs : Array of const);
    // Get correct port for transport
    Function GetTransportPort(aTransport : TMCTransport) : Word;
    // Transform an exception to a EManticoreSearch exception. Log the exception
    Function TransformError(E: Exception; const aCommand: String): EManticoreSearch;
    // On muysq, check if transaction is active
    function IsTransActionActive : Boolean;
    // On mysql, start a transaction
    procedure StartTransaction; virtual;
    // On mysql, rollback a transaction. NOOP on http.
    procedure RollbackTransaction; virtual;
    // On mysql, commit a transaction NOOP on http.
    procedure CommitTransaction; virtual;
    // Connection/deconnection NOOP on http.
    Procedure DoMysqlConnect; virtual;
    Procedure DoMySQLDisconnect; virtual;
    //
    // Low-level command execution.
    //
    // MySQL-based commands
    Procedure DoMySQLSingleColCommand(const aCmd: String; aList : TStrings);
    function DoResultCommand(const aCmd: String; aOnResult: TMCSearchResultCallBack; aData: Pointer) : Integer;
    procedure ExecuteMySQLCommand(const aCommand: String); virtual;
    // For search
    function DoMySQLResultCommand(const aCmd: String;  aOnResult: TMCSearchResultCallBack; aData: Pointer) : Integer;
    // HTTP-based commands
    function CreateHTTPCmdURL(aCmd: String): string;
    function ExecuteHTTPCommand(const aCommand: String): String; virtual;
    function ExecuteHTTPCommandResult(const aCmd: String): TJSONArray;
    Procedure DoHTTPSingleColCommand(const aCmd: String; aList : TStrings);
    // For search
    function DoHTTPResultCommand(const aCmd: String;  aOnResult: TMCSearchResultCallBack; aData: Pointer) : Integer;
    // Commands with result
    // Get declaration of the index table, used in CreateIndex.
    function GetIndexSQL: string;
    // Used During indexing:
    // Actual call to do the indexing
    function DoIndexSources(const aTree, aDir, aBaseDir: String;  aOptions: TMCIndexOptions; aOnProgress : TIndexProgressEvent = Nil): Integer; virtual;

  Public
    // escape a string so it can be used in a Match() operation
    class function Escape(aString: String): String;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Create a copy with the same connection parameters as this instance
    function Clone(aOwner: TComponent): TManticoreSearchSources; virtual;
    // Connect to manticore
    Procedure Connect;
    // Disconnect from manticore
    Procedure Disconnect;
    // Create the index with given name. If left empty, the IndexName property is used.
    Procedure CreateIndex(const aIndexName : string = '');
    // Truncate the index.
    Procedure TruncateIndex;
    // Delete the current index.
    Procedure DeleteIndex;
    // Delete the indicated index.
    Procedure DeleteIndex(const aIndexName : String);
    // List Indexes
    procedure ListIndexes(aList : TStrings);
    Function ListIndexes : TStringDynArray;
    // List trees for IndexName
    procedure ListTrees(aList : TStrings);
    Function ListTrees : TStringDynArray;
    // Delete the source tree in the index.
    Procedure DeleteTree(const aTree : String);
    // Execute an arbitrary ManticoreSearch command
    procedure ExecuteCommand(const aCommand: String);
    // See if extension is in list of extensions
    function AllowExtension(aExtension: String): Boolean;
    // Index a source file aFile in source tree aTree.
    procedure IndexSourceFile(const aTree, aStoredFileName, aActualFileName: String);
    // Index files using aTree, starting in directory aDir, using given options
    Function IndexSources(const aTree, aDir: String; aOptions: TMCIndexOptions; aOnProgress : TIndexProgressEvent = nil) : Integer;
    // Search in all source trees
    Function Search(const aMatchTerm : String) : TMCSearchResultArray;
    // Search in source tree
    Function Search(const aMatchTerm : String; Const aTree : String) : TMCSearchResultArray;
    // Search in source trees
    Function Search(const aMatchTerm : String; Const aTrees : Array of String) : TMCSearchResultArray;
    // Call aOnResult with aData for every result. Return number of results
    Function Search(const aMatchTerm : String; Const aTrees : Array of String; aOnResult : TMCSearchResultCallBack; aData : Pointer) : Integer;
    // Create your own command
    function ResultCommand(const aCmd: String; aOnResult: TMCSearchResultCallBack; aData: Pointer) : Integer;
    // Run a command that returns a single column. Values are stored in aList
    Procedure SingleColCommand(const aCmd: String; aList : TStrings);
    // Run a command that returns a single value. If no results are returned then the result is the empty string;
    Function SingleValueCommand(const aCmd: String) : String;
    // Are we currently connected ?
    Property Connected : Boolean Read FConnected;
    // Extensions to use when searching files
    Property Extensions : TStringDynArray Read FExtensions Write FExtensions;
  Published
    // Client library version to use when connecting to manticore
    Property MySQLVersion : TMCMySQLClientVersion Read FMySQLVersion Write SetMySQLVersion;
    // Transport to use.
    Property Transport : TMCTransport Read FProtocol Write SetProtocol;
    // Hostname
    Property HostName : String Read FHostName Write SetHostName;
    // Port may differ, depending on transport. If left empty, the default for the current protocol will be used.
    Property Port : Word Read FPort Write SetPort;
    // IndexName to use
    Property IndexName : String Read FIndexName Write SetIndexName;
    // Limit number of results. If 0, the manticoresearch default is used.
    Property Limit : Cardinal Read FLimit Write FLimit;
    // Minimum infix len when creating index. Must be 2 or higher to enable partial matches
    Property MinInfixLen : Integer Read FMinInfixLen Write FMinInfixLen Default DefaultMCMinInfixLen;
    // Log event
    Property OnLog : TMCLogEvent Read FOnLog Write FOnLog;
  end;
  TManticoreSearchSourcesClass = Class of TManticoreSearchSources;

Const
  MySQLConnNames : Array[TMCMySQLClientVersion] of string = ('','MySQL 5.7','MySQL 8.0');
  MCTransportNames : Array[TMCTransport] of string = ('','MySQL','HTTP');

implementation

uses fphttpclient;

{ TMCToArrayConverter }

constructor TMCToArrayConverter.Create(aGrowDelta: Cardinal);
begin
  FGrowDelta:=aGrowDelta;
  if FGrowDelta=0 then
    FGrowDelta:=20;
end;

procedure TMCToArrayConverter.DoAddToResult(Sender: TObject;
  const aResult: TMCSearchResult; aData: Pointer; var aContinue: Boolean);

Var
  Len : Integer;

begin
  Len:=Length(Results);
  if Count>=Len then
    SetLength(Results,Len+FGrowDelta);
  Results[Count]:=aResult;
  Inc(Count);
  aContinue:=True;
end;


{ TMCSearchResult }

function TMCSearchResult.Description(aFull: Boolean): string;
begin
  Result:=Format('%s(%d): %s',[FileName,LineNo,Content]);
  if aFull then
    Result:='['+Tree+']'+Result
end;


{ TManticoreSearchSources }

procedure TManticoreSearchSources.SetHostName(AValue: String);
begin
  if FHostName=AValue then Exit;
  CheckDisconnected;
  FHostName:=AValue;
end;

procedure TManticoreSearchSources.SetIndexName(AValue: String);
begin
  if FIndexName=AValue then Exit;
  CheckDisconnected;
  FIndexName:=AValue;
end;

procedure TManticoreSearchSources.SetMySQLVersion(
  AValue: TMCMySQLClientVersion);
begin
  if FMySQLVersion=AValue then Exit;
  CheckDisconnected;
  FMySQLVersion:=AValue;
end;

procedure TManticoreSearchSources.SetPort(AValue: Word);
begin
  if FPort=AValue then Exit;
  CheckDisconnected;
  FPort:=AValue;
end;

procedure TManticoreSearchSources.SetProtocol(AValue: TMCTransport);
begin
  if FProtocol=AValue then Exit;
  CheckDisconnected;
  FProtocol:=AValue;
end;



procedure TManticoreSearchSources.DoLog(aKind: TMCLogKind;
  const aMessage: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self,aKind,aMessage);
end;

procedure TManticoreSearchSources.DoLog(aKind: TMCLogKind;
  const aFmt: String; const aArgs: array of const);
begin
  DoLog(aKind,Format(aFmt,aArgs));
end;


function TManticoreSearchSources.GetTransportPort(aTransport: TMCTransport
  ): Word;
begin
  if Fport<>0 then
    Result:=FPort
  else
    case aTransport of
      mctHttp : Result:=DefaultMCHTTPPort;
      mctMysql : Result:=DefaultMCMySQLPort;
    end;
end;

procedure TManticoreSearchSources.CheckDisconnected;
begin
  if FConnected then
    Raise EManticoreSearch.Create('Cannot perform this operation when connected');
end;

procedure TManticoreSearchSources.CheckConnected;
begin
  if not FConnected then
    Raise EManticoreSearch.Create('Cannot perform this operation when disconnected');
end;

procedure TManticoreSearchSources.DoMysqlConnect;


Var
  EM : EManticoreSearch;

begin
  If (Transport<>mctMySQL) then
    Raise EManticoreSearch.Create('Internal error: attempting MySQL connection when transport is not mysql');
  If (MySQLVersion=mcvNone) then
    Raise EManticoreSearch.Create('Attempting MySQL connection without MySQLVersion set');

  EM:=Nil;
  FTrans:=Nil;
  FMySQLConn:=TSQLConnector.Create(Self);
  FMySQLConn.HostName:=Self.HostName;
  if FMySQLConn.HostName='' then
    FMySQLConn.HostName:='0';
  FMySQLConn.Params.Values['Port']:=IntToStr(GetTransportPort(mctMysql));
  FMySQLConn.ConnectorType:=MySQLConnNames[MySQLVersion];
  // Keep SQLDB happy, these make no sense for manticore.
  FMySQLConn.DatabaseName:='Dummy';
//  FMySQLConn.UserName:='Dummy';
//  FMySQLConn.Password:='Dummy';
  FTrans:=TSQLTransaction.Create(Self);
  try
    FMySQLConn.Transaction:=FTrans;
    FMySQLConn.Connected:=true;
    FConnected:=true
  except
    on E : Exception do
      begin
      FreeAndNil(FTrans);
      FreeAndNil(FMySQLConn);
      EM:=TransformError(E,'Connect');
      end;
  end;
  if Assigned(EM) then
    Raise EM;
end;

procedure TManticoreSearchSources.DoMySQLDisconnect;
begin
  FConnected:=False;
  FreeAndNil(FTrans);
  FreeAndNil(FMySQLConn);
end;

constructor TManticoreSearchSources.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinInfixLen:=DefaultMCMinInfixLen;
  FIndexName:=DefaultMCIndexName;
end;

destructor TManticoreSearchSources.Destroy;
begin
  try
    Disconnect;
  except
    // MySQL connection sometimes burps after long inactivity
  end;
  inherited Destroy;
end;

function TManticoreSearchSources.Clone(aOwner : TComponent): TManticoreSearchSources;
begin
  Result:=TManticoreSearchSourcesClass(Self.ClassType).Create(aOwner);
  Result.Extensions:=Self.Extensions;
  Result.HostName:=Self.HostName;
  Result.IndexName:=Self.IndexName;
  Result.Limit:=Self.Limit;
  Result.MinInfixLen:=Self.MinInfixLen;
  Result.MySQLVersion:=Self.MySQLVersion;
  Result.OnLog:=Self.OnLog;
  Result.Port:=Self.Port;
  Result.Transport:=Self.Transport;
end;

procedure TManticoreSearchSources.Connect;
begin
  if Connected then
    exit;
  If (Transport=mctNone) then
    Raise EManticoreSearch.Create('No transport selected');
  If (Transport=mctHttp) then
    FConnected:=True
  else
    DoMySQLConnect;
end;

procedure TManticoreSearchSources.Disconnect;
begin
  if not Connected then
    exit;
  try
    If (Transport=mctHttp) then
      FConnected:=False
    else
      DoMySQLDisConnect;
  except
    On E : Exception do
      DoLog(mlkError,Format('Error %s while disconnecting: %s',[E.ClassName,E.Message]));
  end;
end;

function TManticoreSearchSources.GetIndexSQL : string;

begin
  if MinInfixLen=1 then
    MinInfixLen:=2;
  Result:=Format('(tree string, filename string, lineno int, line text) min_infix_len = ''%d'' ',[MinInfixLen]);
end;

function TManticoreSearchSources.TransformError(E: Exception;
  const aCommand: String): EManticoreSearch;

begin
  DoLog(mlkError,Format('Error %s with message "%s" executing command "%s"',[E.ClassName,E.Message,aCommand]));
  Result:=EManticoreSearch.Create(E.Message);
  Result.ManticoreCommand:=aCommand;
end;

function TManticoreSearchSources.IsTransActionActive: Boolean;
begin
  if Transport=mctMysql then
    Result:=FTrans.Active
  else
    Result:=True;
end;


procedure TManticoreSearchSources.ExecuteMySQLCommand(
  const aCommand: String);

Var
  SQL : TSQLStatement;
  EM : EManticoreSearch;
  doTrans : Boolean;

begin
  EM:=nil;
  SQL:=TSQLStatement.Create(Self);
  try
    SQL.Database:=FMySQLConn;
    SQL.Transaction:=FTrans;
    doTrans:=Not FTrans.Active;
    if doTrans then
      StartTransaction;
    // Avoid parsing
    SQL.SQL.Clear;
    DoLog(mlkDebug,'Executing command: '+aCommand);
    SQL.SQL.Add(aCommand);
    SQL.Execute;
    if doTrans then
      CommitTransaction;
  except
    On E : exception do
      begin
      if doTrans then
        RollBackTransaction;
      EM:=TransFormError(E,aCommand);
      end;
  end;
  SQL.Free;
  if Assigned(EM) then
    Raise EM;
end;

procedure TManticoreSearchSources.DoMySQLSingleColCommand(const aCmd: String; aList: TStrings);
Var
  Q : TSQLQuery;

begin
  Q:=TSQLQuery.Create(Self);
  try
    Q.DataBase:=FMySQLConn;
    Q.Transaction:=FTrans;
    Q.SQL.Clear;
    Q.SQL.Add(aCmd);
    // Disable fetching indexes !
    Q.UsePrimaryKeyAsKey:=False;
    Q.UniDirectional:=True;
    Q.Open;
    While not Q.EOF do
      begin
      aList.Add(Q.Fields[0].AsString);
      Q.Next;
      end;
  finally
    Q.Free;
  end;
end;

function TManticoreSearchSources.ExecuteHTTPCommandResult(const aCmd: String) : TJSONArray;

var
  lJSON : String;
  aData : TJSONData;

begin
  aData:=Nil;
  lJSON:=ExecuteHTTPCommand(aCmd);
  try
    aData:=GetJSON(lJSON);
    if aData is TJSONArray then
      begin
      Result:=TJSONArray(aData);
      aData:=nil;
      end
    else
      Raise EJSON.Create('Command result is not an array: '+lJSON);
  except
    aData.Free;
    Raise;
  end;
end;
procedure TManticoreSearchSources.DoHTTPSingleColCommand(const aCmd: String; aList: TStrings);

Var
  aJSON : TJSONData;
  aSet,aResult : TJSONEnum;
  aResultRecord : TJSONObject;
  aResultPart : TJSONArray;


begin
  aJSON:=ExecuteHTTPCommandResult(aCmd);
  for aSet in aJSON do
    if aSet.Value is TJSONObject then
      begin
      aResultPart:=TJSONObject(aSet.Value).Get('data',TJSONArray(Nil));
      if Assigned(aResultPart) then
        For aResult in aResultPart do
          if aResult.Value is TJSONObject then
            begin
            aResultRecord:=TJSONObject(aResult.Value);
            if (aResultRecord.Count>0) and
                not (aResultRecord.Items[0].JSONType in StructuredJSONTypes) then
              aList.Add(aResultRecord.Items[0].AsString);
            end;
      end;
end;

function TManticoreSearchSources.CreateHTTPCmdURL(aCmd : String) : string;


  Function EscapeHTML(aCmd : string) : String;

  begin
    Result:=StringReplace(aCmd,' ','%20',[rfReplaceAll]);
  end;


Const
  BaseURL = 'http://%s:%d/cli?%s';

Var
  lCmd,lHostName : String;

begin
  lHostName:=HostName;
  if lHostName='' then
    lHostName:='127.0.0.1';
  lCmd:=EscapeHTML(aCmd);
  Result:=Format(BaseURL,[lHostName,GetTransportPort(mctHttp),lCmd]);
end;

function TManticoreSearchSources.ExecuteHTTPCommand(
  const aCommand: String) : String;

Var
  HTTP : TFPHTTPClient;
  EM : EManticoreSearch;
  aURL : String;

begin
  EM:=nil;
  HTTP:=TFPHTTPClient.Create(Self);
  try
    aURL:=CreateHTTPCmdURL(aCommand);
    DoLog(mlkDebug,'Getting URL: '+aURL);
    Result:=HTTP.Get(aURL);
  except
    On E : exception do
      EM:=TransFormError(E,aCommand);
  end;
  HTTP.Free;
  if Assigned(EM) then
    Raise EM;
end;

procedure TManticoreSearchSources.ExecuteCommand(const aCommand: String);

begin
  DoLog(mlkDebug,'Executing command '+aCommand);
  if FProtocol=mctMysql then
    ExecuteMySQLCommand(aCommand)
  else
    ExecuteHTTPCommand(aCommand);
end;


class function TManticoreSearchSources.Escape(aString: String): String;

var
  a : Char;

begin
  Result:=StringReplace(aString,'\','\\',[rfReplaceAll]);
  Result:=StringReplace(Result,'''','\''',[rfReplaceAll]);
  For a in ['(',')','|','-','!','@','~','"','&','^','$','=','<'] do
    Result:=StringReplace(Result,a,'\'+a,[rfReplaceAll]);
end;


procedure TManticoreSearchSources.StartTransaction;

begin
  If Transport=mctMysql then
    FTrans.StartTransaction;
end;

procedure TManticoreSearchSources.CommitTransaction;

begin
  If Transport=mctMysql then
    FTrans.Commit;
end;

procedure TManticoreSearchSources.RollbackTransaction;

begin
  If Transport=mctMysql then
    FTrans.Rollback;
end;

procedure TManticoreSearchSources.IndexSourceFile(const aTree, aStoredFileName, aActualFileName: String);


Const
  SQL = 'INSERT INTO %s (id,tree,filename,lineno,line) values (0,''%s'',''%s'',%d,''%s'');';

Var
  F : Text;
  aLineNo : Integer;
  aLine : String;
  lSQL,lTree,lFile : String;
  doTrans : Boolean;
begin
  CheckConnected;
  doTrans:=Not IsTransActionActive;
  if DoTrans then
    StartTransaction;
  try
    DoLog(mlkInfo,'Indexing file "%s" using stored filename "%s" ',[aActualFileName,aStoredFileName]);
    lTree:=Escape(aTree);
    lFile:=Escape(aStoredFileName);
    AssignFile(F,aActualFileName);
    Reset(F);
    aLineNo:=0;
    While not EOF(F) do
      begin
      Inc(aLineNo);
      ReadLn(F,aLine);
      lSQL:=Format(SQL,[IndexName,lTree,lFile,aLineNo,Escape(aLine)]);
      ExecuteCommand(lsql);
      end;
    CloseFile(F);
    if DoTrans then
      CommitTransaction;
  except
    if DoTrans then
      RollBackTransaction;
    Raise;
  end;
end;


function TManticoreSearchSources.AllowExtension(aExtension: String): Boolean;

Var
  S : String;

begin
  if (aExtension<>'') and (aExtension[1]='.') then
    Delete(aExtension,1,1);
  Result:=Length(FExtensions)=0;
  if Not Result then
    for S in FExtensions do
      if SameText(S,aExtension) then
          Exit(True);
end;

function TManticoreSearchSources.DoIndexSources(const aTree, aDir,
  aBaseDir: String; aOptions: TMCIndexOptions; aOnProgress : TIndexProgressEvent): Integer;

Var
  Info : TSearchRec;
  DoContinue,StoreRelative,allFiles : Boolean;
  lActual,lStored : String;

begin
  Result:=0;
  allFiles:=ioAllFiles in aOptions;
  StoreRelative:=ioStoreRelativeNames in aOptions;
  DoContinue:=True;
  If FindFirst(aDir+'*.*',0,Info)=0 then
    try
      Repeat
        if (Info.Attr and faDirectory)=0 then
          if  AllFiles or AllowExtension(ExtractFileExt(Info.Name)) then
            begin
            lActual:=aDir+Info.Name;
            if StoreRelative then
              lStored:=ExtractRelativePath(aBaseDir,lActual)
            else
              lActual:=lStored;
            IndexSourceFile(aTree,lStored,lActual);
            Inc(Result);
            if Assigned(aOnProgress) then
              aOnProgress(Self,lActual,DoContinue);
            end;
      Until (FindNext(Info)<>0) or not DoContinue;
    finally
      FindClose(Info)
    end;
  if (ioRecurse in aOptions) and DoContinue then
    If FindFirst(aDir+AllFilesMask,faDirectory,Info)=0 then
      try
        Repeat
          if ((Info.Attr and faDirectory)=faDirectory) and (Info.Name<>'.') and (Info.Name<>'..') then
            begin
            Result:=Result+DoIndexSources(aTree,aDir+Info.Name+PathDelim,aBaseDir,aOptions,aOnProgress);
            if Assigned(aOnProgress) then
              aOnProgress(Self,lActual,DoContinue);
            end;
        Until (FindNext(Info)<>0) or Not DoContinue;
      finally
        FindClose(Info)
      end;
end;

function TManticoreSearchSources.IndexSources(const aTree, aDir: String;
  aOptions: TMCIndexOptions; aOnProgress : TIndexProgressEvent = nil): Integer;

Var
  lDir : String;

begin
  lDir:=IncludeTrailingPathDelimiter(aDir);
  Result:=DoIndexSources(aTree,lDir,lDir,aOptions,aOnProgress);
end;

function TManticoreSearchSources.Search(const aMatchTerm: String
  ): TMCSearchResultArray;
begin
  Result:=Search(aMatchTerm,[]);
end;

function TManticoreSearchSources.Search(const aMatchTerm: String;
  const aTree: String): TMCSearchResultArray;
begin
  if aTree<>'' then
    Result:=Search(aMatchTerm,[aTree])
  else
    Result:=Search(aMatchTerm,[])
end;

function TManticoreSearchSources.Search(const aMatchTerm: String;
  const aTrees: array of String): TMCSearchResultArray;

Var
  aCollector : TMCToArrayConverter;

begin
  aCollector:=TMCToArrayConverter.Create(Limit);
  try
    Search(aMatchTerm,aTrees,@aCollector.DoAddToResult,Self);
    Result:=aCollector.Results;
    SetLength(Result,aCollector.Count);
  finally
    aCollector.Free;
  end;
end;

function TManticoreSearchSources.DoMySQLResultCommand(const aCmd : String; aOnResult: TMCSearchResultCallBack; aData: Pointer) : Integer;

Var
  Q : TSQLQuery;
  Res : TMCSearchResult;
  FID,FWeight,FLineNo,FFileName,FTree,FContent : TField;
  aContinue : Boolean;

begin
  CheckConnected;
  Result:=0;
  aContinue:=True;
  Q:=TSQLQuery.Create(Self);
  try
    Q.DataBase:=FMySQLConn;
    Q.Transaction:=FTrans;
    Q.SQL.Clear;
    Q.SQL.Add(aCmd);
    // Disable fetching indexes !
    Q.UsePrimaryKeyAsKey:=False;
    Q.UniDirectional:=True;
    Q.Open;
    if Q.IsEmpty then
      exit; // Manticore search does not return field definitions if the result is empty
    FWeight:=Q.FieldByName('theweight');
    FLineNo:=Q.FieldByName('lineno');
    FFileName:=Q.FieldByName('filename');
    FTree:=Q.FieldByName('tree');
    FContent:=Q.FieldByName('line');
    FID:=Q.FieldByName('id');
    While not Q.EOF do
      begin
      Inc(Result);
      Res.FileName:=FFileName.AsString;
      Res.LineNo:=FLineNo.AsInteger;
      Res.Content:=FContent.AsString;
      Res.Weight:=FWeight.Asinteger;
      Res.Tree:=FTree.AsString;
      Res.ID:=FID.AsLargeInt;
      aOnResult(Self,Res,aData,aContinue);
      if not aContinue then
        break;
      Q.Next;

      end;
  finally
    Q.Free;
  end;
end;

function TManticoreSearchSources.DoHTTPResultCommand(const aCmd : String; aOnResult: TMCSearchResultCallBack; aData: Pointer) : Integer;

Var
  aJSON : TJSONArray;
  aSet,aResult : TJSONEnum;
  aResultRecord : TJSONObject;
  aResultPart : TJSONArray;
  Res : TMCSearchResult;
  Continue : Boolean;

begin
  Result:=0;
  Continue:=True;
  aJSON:=ExecuteHTTPCommandResult(aCmd);
  for aSet in aJSON do
    if aSet.Value is TJSONObject then
      begin
      aResultPart:=TJSONObject(aSet.Value).Get('data',TJSONArray(Nil));
      if Assigned(aResultPart) then
        For aResult in aResultPart do
          if aResult.Value is TJSONObject then
            begin
            aResultRecord:=TJSONObject(aResult.Value);
            Res.Weight:=aResultRecord.Get('theweight',Integer(0));
            Res.LineNo:=aResultRecord.Get('lineno',0);
            Res.FileName:=aResultRecord.Get('filename','');
            Res.Tree:=aResultRecord.Get('tree','');
            Res.Content:=aResultRecord.Get('line','');
            Res.ID:=aResultRecord.Get('id',Int64(0));
            inc(Result);
            aOnResult(Self,Res,aData,Continue);
            if not Continue then
              Break;
            end;
      if not Continue then
        Break;
      end;

end;

function TManticoreSearchSources.DoResultCommand(const aCmd: String;
  aOnResult: TMCSearchResultCallBack; aData: Pointer): Integer;

Var
  EM : EManticoreSearch;

begin
  EM:=Nil;
  try
    if Transport=mctMysql then
      Result:=DoMySQLResultCommand(aCmd,aOnResult,aData)
    else
      Result:=DoHTTPResultCommand(aCmd,aOnResult,aData)
  except
    On E : Exception do
      EM:=TransformError(E,aCmd);
  end;
  if Assigned(EM) then
    Raise EM;
end;

function TManticoreSearchSources.Search(const aMatchTerm: String;
  const aTrees: array of String; aOnResult: TMCSearchResultCallBack;
  aData: Pointer): Integer;

Const
  BaseSQL ='select weight() as theweight, * from %s where MATCH(''%s'')';

Var
  aInTrees,aCmd,aTree : String;

begin

  aCmd:=Format(BaseSQL,[IndexName,aMatchTerm]);
  aInTrees:='';
  for aTree in aTrees do
    begin
    if aInTrees<>'' then
      aInTrees:=aInTrees+' OR ';
    aInTrees:=aInTrees+Format('(tree=''%s'')',[Escape(aTree)]);
    end;
  if aInTrees<>'' then
    aCmd:=aCmd+' AND ('+aInTrees+')';
  if FLimit>0 then
    aCmd:=aCmd+Format(' LIMIT %d',[FLimit]);
//  aCmd:='select weight() as theweight, * from sources where MATCH(''*load*'') and (tree<>''base'');';
//  aCmd:='SELECT weight() as theweight, id, tree, line, filename, lineno FROM sources where MATCH(''*load*'') and (tree<>''base'');';
//  aCmd:='SELECT * FROM sources where MATCH(''*load*'')';
  Result:=DoResultCommand(aCmd+';',aOnResult,aData);
end;

function TManticoreSearchSources.ResultCommand(const aCmd: String;
  aOnResult: TMCSearchResultCallBack; aData: Pointer): Integer;
begin
  Result:=DoResultCommand(aCmd,aOnResult,aData);
end;

procedure TManticoreSearchSources.SingleColCommand(const aCmd: String; aList: TStrings);

Var
  EM : EManticoreSearch;

begin
  CheckConnected;
  EM:=nil;
  try
    if Transport=mctMysql then
      DoMySQLSingleColCommand(aCmd,aList)
    else
      DoHTTPSingleColCommand(aCmd,aList)
  except
    On E : Exception do
      EM:=TransformError(E,aCmd);
  end;
  if Assigned(Em) then
    Raise Em;
end;

function TManticoreSearchSources.SingleValueCommand(const aCmd: String): String;

Var
  L : TStringList;

begin
  Result:='';
  L:=TStringList.Create;
  try
    SingleColCommand(aCmd,L);
    if L.Count>1 then
      Raise EManticoreSearch.Create('Multiple results returned for command : '+aCmd);
    if L.Count=1 then
      Result:=L[0];
  finally
    L.Free;
  end;
end;


procedure TManticoreSearchSources.CreateIndex(const aIndexName: string);

Var
  lIndex : String;

begin
  lIndex:=aIndexName;
  if lIndex='' then
    lIndex:=Self.IndexName;
  ExecuteCommand('CREATE TABLE '+lIndex+' '+GetIndexSQL);
end;

procedure TManticoreSearchSources.TruncateIndex;
begin
  ExecuteCommand('TRUNCATE TABLE '+IndexName);
end;

procedure TManticoreSearchSources.DeleteIndex;
begin
  DeleteIndex(Self.IndexName);
end;

procedure TManticoreSearchSources.DeleteIndex(const aIndexName : string);
begin
  ExecuteCommand('DROP TABLE '+aIndexName);
end;


procedure TManticoreSearchSources.ListIndexes(aList: TStrings);
begin
  SingleColCommand('SHOW TABLES',aList);
end;

function TManticoreSearchSources.ListIndexes: TStringDynArray;

Var
  L : TStringList;

begin
  L:=TStringList.Create;
  try
    ListIndexes(L);
    Result:=L.ToStringArray;
  finally
    L.Free;
  end;
end;

procedure TManticoreSearchSources.ListTrees(aList: TStrings);
begin
  SingleColCommand(Format('select tree from %s group by tree',[IndexName]),aList);
end;

function TManticoreSearchSources.ListTrees: TStringDynArray;

Var
  L : TStringList;

begin
  L:=TStringList.Create;
  try
    ListTrees(L);
    Result:=L.ToStringArray;
  finally
    L.Free;
  end;
end;

procedure TManticoreSearchSources.DeleteTree(const aTree: String);
begin
  ExecuteCommand('DELETE FROM '+IndexName+' where (tree='''+Escape(aTree)+''');');
end;

end.

