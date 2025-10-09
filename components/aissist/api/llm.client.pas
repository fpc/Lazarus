{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt

    AI server communication implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit LLM.Client;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$IFNDEF VER3_2}
{$modeswitch functionreferences}
{$ENDIF}

interface

uses
  Types, Classes, SysUtils, fpjson, fpwebclient;

Type
  ELLMServer = Class(Exception);
  { TLLMServerSettings }

  TLLMServerSettings = class(TPersistent)
  private
    FAuthorizationKey: String;
    FBaseURL: String;
    FDefaultMaxLength: Integer;
    FDefaultModel: String;
    FOnProtocolChange: TNotifyEvent;
    FProtocol: String;
    procedure SetBaseURL(AValue: String);
    procedure SetProtocol(AValue: String);
  protected
    Property OnProtocolChange : TNotifyEvent Read FOnProtocolChange Write FOnProtocolChange;
  public
    constructor create;
    procedure Assign(Source: TPersistent); override;
  Published
    Property BaseURL : String Read FBaseURL Write SetBaseURL;
    Property DefaultModel : String Read FDefaultModel Write FDefaultModel;
    Property DefaultMaxLength : Integer Read FDefaultMaxLength Write FDefaultMaxLength;
    Property AuthorizationKey : String Read FAuthorizationKey Write FAuthorizationKey;
    Property Protocol : String Read FProtocol Write SetProtocol;
  end;

  TModelData = Record
    // Just ID and Human-readable name for the time being
    ID : String; // To be used as model identifier in prompts.
    Name : string;
  end;
  TModelDataArray = Array of TModelData;

  TPrompt = record
    Role : string;
    Text : string;
  end;
  TPromptArray  = Array of TPrompt;

  // For the time being
  TPromptResponse = TPrompt;
  TPromptResponseArray  = Array of TPromptResponse;

  { TLLMResult }
  TLLMRestStatusInfo = record
    Status : Integer;
    StatusCode : string;
    ErrorContent : string;
  end;

  generic TLLMResult<T> = record
  private
    FValue : T;
  public
    StatusInfo : TLLMRestStatusInfo;
    MetaData : TStringDynArray;
    function Success : Boolean;
    procedure SetOKValue(aValue : T);
    property Value : T Read FValue;
  end;

  TGetModelsResult = specialize TLLMResult<TModelDataArray>;
  TSendPromptResult = specialize TLLMResult<TPromptResponseArray>;

  TModelsResultEvent = Procedure (Sender : TObject; aResult: TGetModelsResult) of object;
  TPromptResultEvent = Procedure (Sender : TObject; aResult : TSendPromptResult) of object;

  TModelsResultHandler = {$IFNDEF VER3_2}Reference to{$ENDIF} Procedure (aResult: TGetModelsResult);
  TPromptResultHandler = {$IFNDEF VER3_2}Reference to{$ENDIF} Procedure (aResult : TSendPromptResult);

//  TLLMRequestErrorHandler = procedure (Sender : TObject; aErrorData : TLLMRequestErrorData) of object;

  TCustomLLMClient = Class;
  TLLMProtocol = Class;

  TLLMUrl = (auListModels,auPrompt);
  TLLMUrls = Set of TLLMUrl;

  { TLLMProtocol }

  TLLMProtocol = Class(TObject)
  private
    FClient: TCustomLLMClient;
  protected
    // Get webclient from client
    function ResolveWebClient: TAbstractWebClient;
    // Get key from client
    function ResolveAuthorizationKey : String;
    // get BaseURL from client
    function ResolveBaseURL : String;
    // Get selected model from client
    function ResolveModel : string;
    // Convert an exception to status info
    function ExceptionToStatusInfo(aException : Exception) : TLLMRestStatusInfo;
  public
    constructor Create(aClient : TCustomLLMClient); virtual;
    // Get models. Exceptions are handled by caller.
    Function GetModels: TGetModelsResult; virtual; abstract;
    // Send completion prompt. Exceptions are handled by caller.
    Function SendPrompt(aPrompt : TPromptArray; aMaxLen: Integer) : TSendPromptResult; virtual; abstract;
    // Register the protocol
    Class procedure Register;
    // Sets the environment variable from which the authentication token must be retrieved. Used only when authenticationtoken is empty.
    Class function APIKeyVariable : String; virtual;
    // Protocol name
    class function ProtocolName : string; virtual;
    // Default base URL to specify to the user.
    class function DefaultURL : String; virtual;
    // Default environment variable name to get API key from.
    class function DefaultAPIKeyVariable : String; virtual;
    // AI client that is controlling this protocol instance.
    property Client : TCustomLLMClient Read FClient;
  end;
  TLLMProtocolClass = Class of TLLMProtocol;

  { TCustomLLMClient }

  TCustomLLMClient = class(TComponent)
  Private
  type
    TModelResponseHandlers = record
      Event : TModelsResultEvent;
      Callback : TModelsResultHandler;
    end;
    TPromptResponseHandlers = record
      Event : TPromptResultEvent;
      Callback : TPromptResultHandler;
    end;
    class var
      _protocols : Array of TLLMProtocolClass;
      _protocolcount : integer;
    class function IndexOfProtocol(const aName: string): Integer;
  private
    FCallsInProgress : Integer;
    FOnModelsResult: TModelsResultEvent;
    FOnPromptResult: TPromptResultEvent;
    FProtocol: TLLMProtocol;
    FSettings: TLLMServerSettings;
    FSynchronizeCallBacks: Boolean;
    FUseThreads: Boolean;
    FWebClient : TAbstractWebClient;
    FModel : string;
    procedure PrepareCall(const aCall: string);
    procedure ProtocolChange(Sender: TObject);
    procedure SetSettings(AValue: TLLMServerSettings);
  Protected
    procedure CheckProtocol;
    procedure CheckServerURL;
    function CreateSettings : TLLMServerSettings; virtual;
    Function WebClient : TAbstractWebClient;
    Property Protocol : TLLMProtocol Read FProtocol;
    // Actual calls
    procedure HandleThreadTerminate(Sender: TObject); virtual;
    function GetAuthenticationToken : String; virtual;
    function GetselectedModel : string; virtual;
    procedure DoGetModels(aCallbacks: TModelResponseHandlers); virtual;
    procedure DoSendPrompt(aPrompt : TPromptArray; aMaxLen : integer; aCallBacks: TPromptResponseHandlers);virtual;
  Public
    // Protocol management
    class Procedure RegisterLLMProtocol(aClass : TLLMProtocolClass);
    class Procedure UnRegisterLLMProtocol(const aProtocol : String);
    class function GetProtocolClass(const aName : string) : TLLMProtocolClass;
    class function FindProtocolClass(const aName: string): TLLMProtocolClass;
    class function GetProtocolList(aList : TStrings) : Integer;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;
    // Get a list of available model descriptions
    procedure GetModels;
    procedure GetModels(aEvent : TModelsResultEvent);
    procedure GetModels(aCallBack : TModelsResultHandler);
    // Send a prompt to the AI.
    Procedure SetModel(aModel : String);
    // Single prompt
    procedure SendPrompt(const aPrompt: string; aMaxLength: Cardinal = 0);
    procedure SendPrompt(const aPrompt: string; aEvent: TPromptResultEvent; aMaxLength: Cardinal = 0);
    procedure SendPrompt(const aPrompt: string; aCallback: TPromptResultHandler; aMaxLength: Cardinal = 0);
    // Prompt history
    procedure SendPrompt(const aPrompt: TPromptArray; aMaxLength: Cardinal = 0);
    procedure SendPrompt(const aPrompt: TPromptArray; aEvent: TPromptResultEvent; aMaxLength: Cardinal = 0);
    procedure SendPrompt(const aPrompt: TPromptArray; aCallback: TPromptResultHandler; aMaxLength: Cardinal = 0);
    // AI server settings
    property Settings : TLLMServerSettings Read FSettings Write SetSettings;
    // Run calls in thread ?
    Property UseThreads : Boolean Read FUseThreads Write FUseThreads;
    // When using thread, should callbacks be executed in the main thread ?
    property SynchronizeCallBacks : Boolean Read FSynchronizeCallBacks Write FSynchronizeCallBacks;
    // Called when a prompt result is received.
    Property OnPromptResult : TPromptResultEvent Read FOnPromptResult Write FOnPromptResult;
    // Called when a list of models is requested
    Property OnGetModelsResult : TModelsResultEvent Read FOnModelsResult Write FOnModelsResult;
  end;

  TLLMClient = Class(TCustomLLMClient)
  Published
    property Settings;
    property SynchronizeCallBacks;
    Property OnPromptResult;
    Property OnGetModelsResult;
  end;

Procedure RegisterLLMProtocol(aClass : TLLMProtocolClass);

implementation

uses httpprotocol, fphttpwebclient;

procedure RegisterLLMProtocol(aClass: TLLMProtocolClass);
begin
  TCustomLLMClient.RegisterLLMProtocol(aClass);
end;

Type
  // We use these call handlers so we can use threads at a later time
  { TCallHandler }

  TCallHandler = Class(TObject)
  Private
    FSender : TObject;
    FProtocol : TLLMProtocol;
    FSynchronizeCallBack: Boolean;
  Protected
    constructor Create(aProtocol : TLLMProtocol; aSender : TObject);
    procedure Execute; virtual; abstract;
    procedure Callback; virtual; abstract;
    procedure HandleCallBack;
    property Sender : TObject read FSender;
    Property SynchronizeCallBack : Boolean read FSynchronizeCallBack Write FSynchronizeCallBack;
    property Protocol : TLLMProtocol read FProtocol;
  end;

  { TModelsHandler }

  TModelsHandler = Class(TCallHandler)
  Private
    FCallBacks : TLLMClient.TModelResponseHandlers;
    FResult : TGetModelsResult;
  Public
    constructor Create(aProtocol : TLLMProtocol; aSender : TObject; aCallBack : TLLMClient.TModelResponseHandlers); reintroduce;
    procedure Execute; override;
    procedure Callback; override;
  end;

  { TPromptHandler }
  TPromptHandler = class(TCallHandler)
  Private
    FCallBacks : TLLMClient.TPromptResponseHandlers;
    FPrompt : TPromptArray;
    FResult : TSendPromptResult;
    FMaxLen : Integer;
  Public
    constructor Create(aProtocol: TLLMProtocol; aPrompt: TPromptArray; aMaxlen: integer; aSender: TObject; aCallBack: TLLMClient.TPromptResponseHandlers);
    procedure Execute; override;
    procedure Callback; override;
  end;

  { THandlerThread }

  THandlerThread = class(TThread)
  Private
    FHandler : TCallhandler;
  Public
    constructor Create(aHandler : TCallhandler; aOnTerminate : TNotifyEvent);
    procedure Execute; override;
  end;

{ THandlerThread }

constructor THandlerThread.Create(aHandler: TCallhandler; aOnTerminate: TNotifyEvent);
begin
  OnTerminate:=aOnTerminate;
  FreeOnTerminate:=True;
  FHandler:=aHandler;
  Inherited Create(False);
end;

procedure THandlerThread.Execute;
begin
  try
    FHandler.Execute;
  finally
    FHandler.Free;
  end;
end;


{ TCallHandler }

constructor TCallHandler.Create(aProtocol: TLLMProtocol; aSender: TObject);
begin
  FProtocol:=aProtocol;
  FSender:=aSender;
end;

procedure TCallHandler.HandleCallBack;
begin
  if FSynchronizeCallBack then
    TThread.Synchronize(TThread.CurrentThread,@Callback)
  else
    CallBack;
end;

{ TPromptHandler }

constructor TPromptHandler.Create(aProtocol: TLLMProtocol; aPrompt: TPromptArray; aMaxlen: integer; aSender: TObject; aCallBack: TLLMClient.TPromptResponseHandlers);

begin
  inherited create(aProtocol,aSender);
  FCallBacks:=aCallBack;
  FPrompt:=aPrompt;
  FMaxlen:=aMaxlen;
end;

procedure TPromptHandler.Execute;

begin
  FResult:=Default(TSendPromptResult);
  try
    FResult:=Protocol.SendPrompt(FPrompt,FMaxLen);
  except
    On E : Exception do
      FResult.StatusInfo:=Protocol.ExceptionToStatusInfo(E);
  end;
  HandleCallback;
end;

procedure TPromptHandler.Callback;

begin
  if assigned(FCallBacks.Event) then
    FCallBacks.Event(FSender,FResult)
  else if assigned(FCallBacks.Callback) then
    FCallBacks.Callback(FResult)
end;

{ TModelsHandler }

constructor TModelsHandler.Create(aProtocol: TLLMProtocol; aSender: TObject; aCallBack: TLLMClient.TModelResponseHandlers);
begin
  Inherited Create(aProtocol,aSender);
  FCallBacks:=aCallBack;
  FSender:=aSender;
end;

procedure TModelsHandler.Execute;

begin
  FResult:=Default(TGetModelsResult);
  try
    FResult:=Protocol.GetModels;
  except
    On E : Exception do
      FResult.StatusInfo:=Protocol.ExceptionToStatusInfo(E);
  end;
  HandleCallBack;
end;

procedure TModelsHandler.Callback;

begin
  if assigned(FCallbacks.Event) then
    FCallBacks.event(FSender,FResult)
  else
    FCallBacks.Callback(FResult)
end;

{ TLLMServerSettings }

procedure TLLMServerSettings.SetBaseURL(AValue: String);
begin
  if FBaseURL=AValue then Exit;
  FBaseURL:=AValue;
  if FBaseURL<>'' then
    FBaseURL:=IncludeHTTPPathDelimiter(FBaseURL);
end;

procedure TLLMServerSettings.SetProtocol(AValue: String);
begin
  if FProtocol=AValue then Exit;
  FProtocol:=AValue;
  If Assigned(FOnProtocolChange) then
    FOnProtocolChange(Self);
end;

constructor TLLMServerSettings.create;
begin
  FDefaultMaxLength:=2048;
end;

procedure TLLMServerSettings.Assign(Source: TPersistent);
var
  aSource: TLLMServerSettings absolute Source;
begin
  if Source is TLLMServerSettings then
  begin
    FDefaultModel:=aSource.FDefaultModel;
    FDefaultMaxLength:=aSource.FDefaultMaxLength;
    FBaseURL:=aSource.BaseURL;
    Protocol:=aSource.FProtocol; // trigger onchange
    AuthorizationKey:=aSource.AuthorizationKey;
  end else
    inherited Assign(Source);
end;

{ TLLMResult }

function TLLMResult.Success: Boolean;
begin
  Result:=(StatusInfo.Status Div 100)=2;
end;

procedure TLLMResult.SetOKValue(aValue: T);
begin
  FValue:=aValue;
  StatusInfo.Status:=200;
  StatusInfo.StatusCode:='OK';
  StatusInfo.ErrorContent:='';
end;

{ TLLMProtocol }

function TLLMProtocol.ResolveWebClient: TAbstractWebClient;
begin
  Result:=FClient.WebClient;
end;

function TLLMProtocol.ResolveAuthorizationKey: String;
begin
  Result:=Client.GetAuthenticationToken;
end;

function TLLMProtocol.ResolveBaseURL: String;
begin
  Result:=Client.Settings.BaseURL;
  if Result='' then
    Result:=DefaultURL;
end;

function TLLMProtocol.ResolveModel: string;
begin
  Result:=Client.GetselectedModel;
end;

function TLLMProtocol.ExceptionToStatusInfo(aException: Exception): TLLMRestStatusInfo;
var
  lMsg : string;
begin
  Result:=Default(TLLMRestStatusInfo);
  Result.Status:=-1;
  Result.StatusCode:='FAILED';
  lMsg:=StringReplace(aException.Message,'"','\"',[rfReplaceAll]);
  Result.ErrorContent:=Format('{ "exception":"%s", "message":"%s" }',[aException.ClassName,lMsg]);
end;

constructor TLLMProtocol.Create(aClient: TCustomLLMClient);
begin
  FClient:=aClient;
end;

class procedure TLLMProtocol.Register;
begin
  TCustomLLMClient.RegisterLLMProtocol(Self);
end;

class function TLLMProtocol.APIKeyVariable: String;
begin
  Result:='';
end;

class function TLLMProtocol.ProtocolName: string;
begin
  Result:=ClassName;
end;

class function TLLMProtocol.DefaultURL: String;
begin
  Result:='';
end;

class function TLLMProtocol.DefaultAPIKeyVariable: String;
begin
  Result:='';
end;

{ TCustomLLMClient }

procedure TCustomLLMClient.SetSettings(AValue: TLLMServerSettings);
begin
  if FSettings=AValue then Exit;
  FSettings.Assign(AValue);
end;

function TCustomLLMClient.CreateSettings: TLLMServerSettings;
begin
  Result:=TLLMServerSettings.Create;
end;

function TCustomLLMClient.WebClient: TAbstractWebClient;
begin
  if FWebClient=Nil then
    FWebClient:=DefaultWebClientClass.Create(Self);
  Result:=FWebClient;
end;

function TCustomLLMClient.GetAuthenticationToken: String;
begin
  Result:=Settings.AuthorizationKey;
  if (Result='') and (Protocol.APIKeyVariable<>'') then
    Result:=GetEnvironmentVariable(Protocol.APIKeyVariable);
end;

function TCustomLLMClient.GetselectedModel: string;
begin
  Result:=FModel;
  if Result='' then
    Result:=Settings.DefaultModel;
end;

constructor TCustomLLMClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSettings:=CreateSettings;
  FSettings.OnProtocolChange:=@ProtocolChange;
end;

destructor TCustomLLMClient.Destroy;
begin
  FreeAndNil(FWebClient);
  FreeAndNil(FSettings);
  inherited Destroy;
end;

procedure TCustomLLMClient.GetModels;
begin
  GetModels(OnGetModelsResult);
end;


procedure TCustomLLMClient.ProtocolChange(Sender: TObject);

var
  aClass : TLLMProtocolClass;

begin
  if FCallsInProgress>0 then
    Raise ELLMServer.Create('Cannot change protocol while calls are in progress');
  aClass:=GetProtocolClass(FSettings.Protocol);
  FreeAndNil(FProtocol);
  FProtocol:=aClass.Create(Self);
end;

procedure TCustomLLMClient.PrepareCall(const aCall : string);

begin
  CheckProtocol;
  CheckServerURL;
end;

procedure TCustomLLMClient.GetModels(aCallBack : TModelsResultHandler);

var
  lHandlers : TModelResponseHandlers;
begin
  PrepareCall('GetModels');
  lHandlers:=Default(TModelResponseHandlers);
  lHandlers.Callback:=aCallback;
  DoGetModels(lHandlers);
end;

procedure TCustomLLMClient.GetModels(aEvent: TModelsResultEvent);

var
  lHandlers : TModelResponseHandlers;
begin
  PrepareCall('GetModels');
  lHandlers:=Default(TModelResponseHandlers);
  lHandlers.Event:=aEvent;
  DoGetModels(lHandlers);
end;

procedure TCustomLLMClient.SetModel(aModel: String);
begin
  FModel:=aModel;
end;

procedure TCustomLLMClient.SendPrompt(const aPrompt: string; aMaxLength: Cardinal);
begin
  SendPrompt(aPrompt,OnPromptResult,aMaxLength);
end;



procedure TCustomLLMClient.DoSendPrompt(aPrompt: TPromptArray; aMaxLen: integer; aCallBacks: TPromptResponseHandlers);
var
  lHandler : TPromptHandler;
begin
  lHandler:=TPromptHandler.Create(FProtocol,aPrompt,aMaxLen,Self,aCallbacks);
  if UseThreads then
    begin
    lHandler.SynchronizeCallBack:=SynchronizeCallBacks;
    InterlockedIncrement(FCallsInProgress);
    THandlerThread.Create(lHandler,@HandleThreadTerminate); // Will free the handler
    end
  else
    try
      lHandler.Execute;
    finally
      lHandler.Free;
    end;
end;

procedure TCustomLLMClient.DoGetModels(aCallbacks: TModelResponseHandlers);
var
  lHandler : TModelsHandler;
begin
  lHandler:=TModelsHandler.Create(FProtocol,Self,aCallbacks);
  if UseThreads then
    begin
    lHandler.SynchronizeCallBack:=SynchronizeCallBacks;
    InterlockedIncrement(FCallsInProgress);
    THandlerThread.Create(lHandler,@HandleThreadTerminate)
    end
  else
    try
      lHandler.Execute;
    finally
      lHandler.Free;
    end;
end;

class procedure TCustomLLMClient.RegisterLLMProtocol(aClass: TLLMProtocolClass);

var
  Len : Integer;

begin
  Len:=Length(_protocols);
  If _protocolcount=len then
    SetLength(_protocols,len+10);
  _protocols[_protocolcount]:=aClass;
  inc(_protocolcount);
end;

class procedure TCustomLLMClient.UnRegisterLLMProtocol(const aProtocol: String);

var
  Idx : Integer;

begin
  Idx:=IndexOfProtocol(aProtocol);
  if Idx=_protocolcount-1 then
    _protocols[idx]:=nil
  else
    begin
    _Protocols[Idx]:=_Protocols[_protocolCount-1];
    _Protocols[_protocolCount]:=nil;
    end;
  Dec(_protocolcount);
end;

class function TCustomLLMClient.IndexOfProtocol(const aName: string): Integer;

begin
  Result:=_protocolcount-1;
  While (Result>=0) and Not SameText(_protocols[Result].protocolname,aName) do
    Dec(Result);
end;

procedure TCustomLLMClient.HandleThreadTerminate(Sender: TObject);
begin
  InterlockedDecrement(FCallsInProgress);
end;

procedure TCustomLLMClient.CheckProtocol;
begin
  If FProtocol=Nil then
    Raise ELLMServer.Create('No protocol assigned');
end;

procedure TCustomLLMClient.CheckServerURL;
var
  lURL : String;
begin
  lURL:=Settings.BaseURL;
  if (lURL='') and Assigned(FProtocol) then
    lURL:=FProtocol.DefaultURL;
  if lURL='' then
    Raise ELLMServer.Create('Server URL is not set');
end;

class function TCustomLLMClient.FindProtocolClass(const aName: string): TLLMProtocolClass;

var
  Idx : integer;
begin
  Idx:=IndexOfProtocol(aName);
  if Idx=-1 then
    Result:=Nil
  else
    Result:=_protocols[Idx]
end;

class function TCustomLLMClient.GetProtocolList(aList: TStrings): Integer;

var
  i : Integer;

begin
  For I:=0 to _protocolcount-1 do
    aList.Add(_protocols[i].protocolname);
  Result:=_protocolcount
end;

class function TCustomLLMClient.GetProtocolClass(const aName: string): TLLMProtocolClass;
begin
  Result:=FindProtocolClass(aName);
  if (Result=Nil) then
    Raise ELLMServer.CreateFmt('Unknown AI protocol: "%s"',[aName]);
end;

procedure TCustomLLMClient.SendPrompt(const aPrompt: string; aEvent: TPromptResultEvent; aMaxLength: Cardinal);
var
  lArr : TPromptArray;
begin
  lArr:=[];
  SetLength(lArr,1);
  lArr[0].Role:='user';
  lArr[0].Text:=aPrompt;
  SendPrompt(lArr,aEvent,aMaxLength);
end;

procedure TCustomLLMClient.SendPrompt(const aPrompt: string; aCallback: TPromptResultHandler; aMaxLength: Cardinal);
var
  lArr : TPromptArray;
begin
  lArr:=[];
  SetLength(lArr,1);
  lArr[0].Role:='user';
  lArr[0].Text:=aPrompt;
  SendPrompt(lArr,aCallBack,aMaxLength);
end;


procedure TCustomLLMClient.SendPrompt(const aPrompt: TPromptArray; aCallback: TPromptResultHandler; aMaxLength: Cardinal);

var
  lMaxLen : integer;
  lHandlers : TPromptResponseHandlers;
begin
  lHandlers:=Default(TPromptResponseHandlers);
  lHandlers.Callback:=aCallBack;
  PrepareCall('SendPrompt');
  lMaxLen:=aMaxLength;
  if lMaxLen=0 then
    lMaxLen:=Settings.DefaultMaxLength;
  DoSendPrompt(aPrompt,lMaxLen,lHandlers);
end;


procedure TCustomLLMClient.SendPrompt(const aPrompt: TPromptArray; aEvent: TPromptResultEvent; aMaxLength: Cardinal);

var
  lMaxLen : integer;
  lHandlers : TPromptResponseHandlers;
begin
  lHandlers:=Default(TPromptResponseHandlers);
  lHandlers.Event:=aEvent;
  PrepareCall('SendPrompt');
  lMaxLen:=aMaxLength;
  if lMaxLen=0 then
    lMaxLen:=Settings.DefaultMaxLength;
  DoSendPrompt(aPrompt,lMaxLen,lHandlers);
end;

procedure TCustomLLMClient.SendPrompt(const aPrompt: TPromptArray; aMaxLength: Cardinal);
begin
  SendPrompt(aPrompt,OnPromptResult,aMaxLength);
end;

initialization
  if DefaultWebClientClass=Nil then
    DefaultWebClientClass:=TFPHTTPWebClient;
end.

