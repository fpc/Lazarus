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
unit AIClient;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, fpjson, fpwebclient, fpasyncwebclient;

Type
  EAIServer = Class(Exception);
  { TAIServerSettings }

  TAIServerSettings = class(TPersistent)
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

  TPromptResponse = record
    Role : string;
    Response : string;
  end;
  TPromptResponseArray  = Array of TPromptResponse;

  TAIRequestErrorData = record
    Error : String;
    ErrorClass : String;
    Method : String;
    URL : String;
    RequestBody : String;
  end;

  TModelsResponseCallBack = Procedure (Sender : TObject; aModels : TModelDataArray) of object;
  TPromptResponseCallBack = Procedure (Sender : TObject; aResponses : TPromptResponseArray) of object;
  TAIRequestErrorHandler = procedure (Sender : TObject; aErrorData : TAIRequestErrorData) of object;

  TCustomAIClient = Class;
  TAIProtocol = Class;

  TAiUrl = (auListModels,auPrompt);
  TAiUrls = Set of TAiUrl;

  { TAIProtocol }

  TAIProtocol = Class(TObject)
  private
    FBaseURL: String;
    FClient: TCustomAIClient;
    FModel : String;
  protected
    procedure SetBaseURL(const aValue: String); virtual;
    procedure SetModel(const aModel : String); virtual;
  public
    constructor Create(aClient : TCustomAIClient); virtual;
    // Convert responses to user data
    // Response to model
    Function ResponseToModels(aResponse : TJSONData; out Models: TModelDataArray) : boolean; virtual; abstract;
    // Response to prompt response(s)
    Function ResponseToPromptResponses(aResponse : TJSONData; out Responses: TPromptResponseArray) : boolean; virtual; abstract;
    // Create a prompt request
    function CreatePromptRequest(const aPrompt : string; aMaxResponseLength : Cardinal) : TJSONData; virtual; abstract;
    // Returned URL must be complete URL starting at BaseURL
    function GetAIURL(aURL : TAiUrl) : String; virtual; abstract;
    // set the authorization key as required by the AI server API.
    procedure AuthenticateRequest(const aRequest: TWebClientRequest; const aKey : String); virtual;
    // User selected model
    property Model : String Read FModel write SetModel;
    // Sets the base URL as specified by user
    property BaseURL : String Read FBaseURL Write SetBaseURL;
    // Protocol name
    class function ProtocolName : string; virtual;
    // Default base URL to specify to the user.
    class function DefaultURL : String; virtual;
    // AI client that is controlling this protocol instance.
    property Client : TCustomAIClient Read FClient;
  end;
  TAIProtocolClass = Class of TAIProtocol;

  { TCustomAIClient }

  TCustomAIClient = class(TComponent)
  Private type
    THTTPRequestResponse = Record
      Response : TWebClientResponse;
      UserCallbackMethod : TMethod;
    end;
    THTTPResultHandler = procedure (const aResponse : THTTPRequestResponse) of object;
    class var
      _protocols : Array of TAIProtocolClass;
      _protocolcount : integer;
    class function IndexOfProtocol(const aName: string): Integer;
  private
    FOnError: TAIRequestErrorHandler;
    FProtocol: TAIProtocol;
    FSettings: TAIServerSettings;
    FSynchronizeCallBacks: Boolean;
    FWebClient : TAbstractWebClient;
    procedure ProtocolChange(Sender: TObject);
    procedure SetSettings(AValue: TAIServerSettings);
  Protected
    procedure CheckProtocol;
    procedure CheckServerURL;
    function CreateSettings : TAIServerSettings; virtual;
    // Async response handling
    procedure HandleModelsResponse(const aResponse: THTTPRequestResponse);
    procedure HandlePromptResponse(const aResponse: THTTPRequestResponse);
    Property Protocol : TAIProtocol Read FProtocol;
    // HTTP Request handling
    procedure ErrorHandler(aException: Exception; aContext : TAsyncRequestContext);
    procedure ServerRequest(const aMethod, aURL: String; aResultHandler: THTTPResultHandler; aUserCallback: TMethod);
    procedure ServerDataRequest(const aMethod, aURL: String; aJSON: TJSONData; aResultHandler: THTTPResultHandler;  aUserCallback: TMethod);
  // Protocol management
  Public
    class Procedure RegisterAIProtocol(aClass : TAIProtocolClass);
    class Procedure UnRegisterAIProtocol(const aProtocol : String);
    class function GetProtocolClass(const aName : string) : TAIProtocolClass;
    class function FindProtocolClass(const aName: string): TAIProtocolClass;
    class function GetProtocolList(aList : TStrings) : Integer;
  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;

    // Get a list of available model descriptions
    procedure GetModels(aCallBack : TModelsResponseCallBack);
    // Send a prompt to the AI.
    procedure SendPrompt(aCallBack: TPromptResponseCallBack; const aPrompt: string; aModel : String = ''; aMaxLength: Cardinal = 0);
    // AI server settings
    property Settings : TAIServerSettings Read FSettings Write SetSettings;
    // Should callbacks be executed in the main thread ?
    property SynchronizeCallBacks : Boolean Read FSynchronizeCallBacks Write FSynchronizeCallBacks;
    // Error handler
    property OnError : TAIRequestErrorHandler Read FOnError Write FOnError;
  end;

  TAIClient = Class(TCustomAIClient)
    property Settings;
    property OnError;
  end;

Procedure RegisterAIProtocol(aClass : TAIProtocolClass);

implementation

uses httpprotocol, fphttpwebclient;

Type

  { TAIRequestData }

  TAIRequestData = Class(TAsyncRequestData)
    UserCallbackMethod : TMethod;
    ResultHandler : TCustomAIClient.THTTPResultHandler;
    procedure HandleWebResponse(aResponse: TWebClientResponse; aUserData: TObject);
  end;

procedure RegisterAIProtocol(aClass: TAIProtocolClass);
begin
  TCustomAIClient.RegisterAIProtocol(aClass);
end;

{ TAIServerSettings }

procedure TAIServerSettings.SetBaseURL(AValue: String);
begin
  if FBaseURL=AValue then Exit;
  FBaseURL:=AValue;
  if FBaseURL<>'' then
    FBaseURL:=IncludeHTTPPathDelimiter(FBaseURL);
end;

procedure TAIServerSettings.SetProtocol(AValue: String);
begin
  if FProtocol=AValue then Exit;
  FProtocol:=AValue;
  If Assigned(FOnProtocolChange) then
    FOnProtocolChange(Self);
end;

procedure TAIServerSettings.Assign(Source: TPersistent);
var
  aSource: TAIServerSettings absolute Source;
begin
  if Source is TAIServerSettings then
  begin
    FDefaultModel:=aSource.FDefaultModel;
    FDefaultMaxLength:=aSource.FDefaultMaxLength;
    FBaseURL:=aSource.BaseURL;
    Protocol:=aSource.FProtocol; // trigger onchange
    AuthorizationKey:=aSource.AuthorizationKey;
  end else
    inherited Assign(Source);
end;

{ TAIProtocol }

constructor TAIProtocol.Create(aClient: TCustomAIClient);
begin
  FClient:=aClient;
end;

procedure TAIProtocol.SetBaseURL(const aValue: String);
begin
  if FBaseURL=aValue then Exit;
  FBaseURL:=aValue;
end;

procedure TAIProtocol.SetModel(const aModel: String);
begin
  FModel:=aModel;
end;

procedure TAIProtocol.AuthenticateRequest(const aRequest: TWebClientRequest; const aKey: String);
begin
  // Do nothing
end;

class function TAIProtocol.ProtocolName: string;
begin
  Result:=ClassName;
end;

class function TAIProtocol.DefaultURL: String;
begin
  Result:='';
end;

{ TCustomAIClient }

procedure TCustomAIClient.SetSettings(AValue: TAIServerSettings);
begin
  if FSettings=AValue then Exit;
  FSettings.Assign(AValue);
end;

function TCustomAIClient.CreateSettings: TAIServerSettings;
begin
  Result:=TAIServerSettings.Create;
end;

constructor TCustomAIClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FSettings:=CreateSettings;
  FSettings.OnProtocolChange:=@ProtocolChange;
  FWebClient:=DefaultWebClientClass.Create(Self);
end;

destructor TCustomAIClient.Destroy;
begin
  FreeAndNil(FWebClient);
  FreeAndNil(FSettings);
  inherited Destroy;
end;

Type

  { TModelsResponseCallbackHandler }

  TModelsResponseCallbackHandler = Class(TObject)
  Private
    FCallBack : TModelsResponseCallBack;
    FList : TModelDataArray;
    FSender : TObject;
  Public
    constructor Create(aSender : TObject; aCallBack : TModelsResponseCallback; aList :TModelDataArray);
    procedure Execute;
  end;

{ TModelsResponseCallbackHandler }

constructor TModelsResponseCallbackHandler.Create(aSender : TObject; aCallBack: TModelsResponseCallback; aList: TModelDataArray);
begin
  FCallBack:=aCallBack;
  FList:=aList;
  FSender:=aSender;
end;

procedure TModelsResponseCallbackHandler.Execute;
begin
  try
    FCallBack(Fsender,FList);
  finally
    Free;
  end;
end;

procedure TCustomAIClient.HandleModelsResponse(const aResponse: THTTPRequestResponse);

var
  CallBack : TModelsResponseCallback;
  handler : TModelsResponseCallbackHandler;
  aList : TModelDataArray;
  JSON : TJSONData;

begin
  CheckProtocol;
  json:=Nil;
  try
    Writeln('Response : ',aResponse.Response.GetContentAsString);
    JSON:=GetJSON(aResponse.Response.GetContentAsString);
    if Protocol.ResponseToModels(JSON,aList) then
      begin
      Callback:=TModelsResponseCallback(aResponse.UserCallbackMethod);
      handler:=TModelsResponseCallbackHandler.Create(Self,CallBack,aList);
      if SynchronizeCallBacks then
        TThread.Synchronize(TThread.CurrentThread,@Handler.Execute)
      else
        Handler.Execute;
      end;
  finally
    JSON.Free;
  end;
end;

procedure TAIRequestData.HandleWebResponse(aResponse : TWebClientResponse; aUserData : TObject);

var
  Res : TCustomAIClient.THTTPRequestResponse;

begin
  Res.Response:=aResponse;
  Res.UserCallbackMethod:=Self.UserCallbackMethod;
  if Assigned(Self.ResultHandler) then
    ResultHandler(Res);
end;

procedure TCustomAIClient.ServerRequest(const aMethod,aURL : String; aResultHandler :  THTTPResultHandler; aUserCallback : TMethod);

var
  Context : TAsyncRequestContext;
  Data : TAIRequestData;

begin
  Context.Client:=FWebClient;
  Context.Request:=FWebClient.CreateRequest;
  Context.Method:=aMethod;
  Context.URL:=aURL;
  Protocol.AuthenticateRequest(Context.Request,Settings.AuthorizationKey);
  Data:=TAIRequestData.Create(Context);
  // AI specific
  Data.UserCallbackMethod:=aUserCallBack;
  Data.OnResponse:=@Data.HandleWebResponse;
  Data.ResultHandler:=aResultHandler;
  THTTPRequestThread.create(Data);
end;

Type

  { TErrorCallBackHandler }

  TErrorCallBackHandler = class(TObject)
    FServer : TCustomAIClient;
    FData : TAIRequestErrorData;
    constructor create(aServer : TCustomAIClient; aData :TAIRequestErrorData);
    procedure Execute;
  end;

{ TErrorCallBackHandler }

constructor TErrorCallBackHandler.create(aServer: TCustomAIClient; aData: TAIRequestErrorData);
begin
  FServer:=aServer;
  FData:=aData;
end;

procedure TErrorCallBackHandler.Execute;
begin
  try
    if Assigned(FServer.FOnError) then
      FServer.FOnError(FServer,FData);
  finally
    Free;
  end;
end;

procedure TCustomAIClient.ErrorHandler(aException: Exception; aContext : TAsyncRequestContext);

var
  AIContext: TAIRequestErrorData;
  Handler : TErrorCallBackHandler;
begin
  if Assigned(FOnError) then
    begin
    AIContext.Error:=aException.Message;
    AIContext.ErrorClass:=aException.ClassName;
    AIContext.URL:=aContext.URL;
    AIContext.Method:=aContext.Method;
    AIContext.RequestBody:=aContext.Request.GetContentAsString;
    Handler:=TErrorCallBackHandler.Create(Self,AICOntext);
    if SynchronizeCallBacks then
      TThread.Synchronize(TThread.CurrentThread,@Handler.Execute)
    else
      Handler.Execute;
    end;
end;

procedure TCustomAIClient.ProtocolChange(Sender: TObject);

var
  aClass : TAIProtocolClass;

begin
  aClass:=GetProtocolClass(FSettings.Protocol);
  FreeAndNil(FProtocol);
  FProtocol:=aClass.Create(Self);
end;

procedure TCustomAIClient.ServerDataRequest(const aMethod,aURL : String; aJSON : TJSONData; aResultHandler :  THTTPResultHandler; aUserCallback : TMethod);

var
  Data : TAIRequestData;
  Context : TAsyncRequestContext;

begin
  Context.Client:=FWebClient;
  Context.Request:=FWebClient.CreateRequest;
  Context.Request.SetContentFromString(aJSON.AsJSON);
  Protocol.AuthenticateRequest(Context.Request,Settings.AuthorizationKey);
//  Writeln('Request: ',Context.Request.GetContentAsString);

  Context.Request.Headers.Values['Content-Type']:='application/json';
  Context.Method:=aMethod;
  Context.URL:=aURL;
  Data:=TAIRequestData.Create(Context);
  // AI specific
  Data.UserCallbackMethod:=aUserCallBack;
  Data.OnResponse:=@Data.HandleWebResponse;
  Data.ResultHandler:=aResultHandler;
  Data.OnError:=@ErrorHandler;
  THTTPRequestThread.create(Data);
end;

procedure TCustomAIClient.GetModels(aCallBack: TModelsResponseCallBack);

var
  RequestURL : String;

begin
  CheckProtocol;
  CheckServerURL;
  RequestURL:=Settings.BaseURL+Protocol.GetAIURL(auListModels);
  ServerRequest('GET',RequestURL,@HandleModelsResponse,TMethod(aCallBack));
end;

Type

  { TPromptResponseHandler }

  TPromptResponseHandler = class(TObject)
  Private
    FSender : TObject;
    FCallBack :TPromptResponseCallback;
    FResponses : TPromptResponseArray;
  Public
    Constructor Create(aSender : TObject; aCallBack :TPromptResponseCallback; aResponses : TPromptResponseArray);
    procedure Execute;
  end;

{ TPromptResponseHandler }

constructor TPromptResponseHandler.Create(aSender: TObject; aCallBack: TPromptResponseCallback; aResponses: TPromptResponseArray);
begin
  FSender:=aSender;
  FCallBack:=aCallBack;
  FResponses:=aResponses;
end;

procedure TPromptResponseHandler.Execute;
begin
  try
    FCallBack(FSender,FResponses);
  finally
    Free;
  end;
end;

procedure TCustomAIClient.HandlePromptResponse(const aResponse: THTTPRequestResponse);

var
  CallBack : TPromptResponseCallback;
  lResponses : TPromptResponseArray;
  Handler : TPromptResponseHandler;
  JSON : TJSONData;

begin
  CheckProtocol;
  CheckServerURL;
  json:=Nil;
  try
    //Writeln('Response: ',aResponse.Response.GetContentAsString);
    JSON:=GetJSON(aResponse.Response.GetContentAsString);
    if Protocol.ResponseToPromptResponses(JSON,lResponses) then
      begin
      Callback:=TPromptResponseCallback(aResponse.UserCallbackMethod);
      Handler:=TPromptResponseHandler.Create(Self,CallBack,lResponses);
      if SynchronizeCallBacks then
        TThread.Synchronize(TThread.CurrentThread,@Handler.Execute)
      else
        Handler.Execute;
      end;
  finally
    JSON.Free;
  end;
end;

class procedure TCustomAIClient.RegisterAIProtocol(aClass: TAIProtocolClass);

var
  Len : Integer;

begin
  Len:=Length(_protocols);
  If _protocolcount=len then
    SetLength(_protocols,len+10);
  _protocols[_protocolcount]:=aClass;
  inc(_protocolcount);
end;

class procedure TCustomAIClient.UnRegisterAIProtocol(const aProtocol: String);

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

class function TCustomAIClient.IndexOfProtocol(const aName: string): Integer;

begin
  Result:=_protocolcount-1;
  While (Result>=0) and Not SameText(_protocols[Result].protocolname,aName) do
    Dec(Result);
end;

procedure TCustomAIClient.CheckProtocol;
begin
  If FProtocol=Nil then
    Raise EAIServer.Create('No protocol assigned');
end;

procedure TCustomAIClient.CheckServerURL;
begin
  if Settings.BaseURL='' then
    Raise EAIServer.Create('Server URL is not set');
end;

class function TCustomAIClient.FindProtocolClass(const aName: string): TAIProtocolClass;

var
  Idx : integer;
begin
  Idx:=IndexOfProtocol(aName);
  if Idx=-1 then
    Result:=Nil
  else
  Result:=_protocols[Idx]
end;

class function TCustomAIClient.GetProtocolList(aList: TStrings): Integer;

var
  i : Integer;

begin
  For I:=0 to _protocolcount-1 do
    aList.Add(_protocols[i].protocolname);
  Result:=_protocolcount
end;

class function TCustomAIClient.GetProtocolClass(const aName: string): TAIProtocolClass;
var
  Idx : integer;
begin
  Result:=FindProtocolClass(aName);
  if (Result=Nil) then
    Raise EAIServer.CreateFmt('Unknown AI protocol: "%s"',[aName]);
end;

procedure TCustomAIClient.SendPrompt(aCallBack: TPromptResponseCallBack; const aPrompt: string; aModel: String; aMaxLength: Cardinal
  );

var
  JSON : TJSONData;
  lModel,RequestURL : String;
  lMaxLen : Cardinal;

begin
  CheckProtocol;
  RequestURL:=Settings.BaseURL+Protocol.GetAIURL(auPrompt);
  lMaxLen:=aMaxLength;
  if lMaxLen=0 then
    lMaxLen:=Settings.DefaultMaxLength;
  lModel:=aModel;
  if lModel='' then
    lModel:=Settings.DefaultModel;
  Protocol.SetModel(lModel);
  JSON:=Protocol.CreatePromptRequest(aPrompt,lMaxLen);
  try
    ServerDataRequest('POST',RequestURL,JSON,@HandlePromptResponse,TMethod(aCallBack));
  finally
    JSON.Free;
  end;
end;

initialization
  if DefaultWebClientClass=Nil then
    DefaultWebClientClass:=TFPHTTPWebClient;
end.

