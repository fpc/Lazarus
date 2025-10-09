{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    Open API service - web server module

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpopenapimodule;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface


{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Contnrs, FpJson.Data, FpWeb.Http.Protocol, FpWeb.Http.Defs, FpWeb.Route;
{$ELSE}
uses sysutils, classes, contnrs, fpjson, httpprotocol, httpdefs, httproute;
{$ENDIF}

Type
  TFPOpenAPIModule = Class;
  TFPOpenAPIModuleClass = Class of TFPOpenAPIModule;

  TOpenAPIBeforeRequestHandler = Procedure(Sender : TObject; aRequest : TRequest; aResponse: TResponse; var aContinue: Boolean) of object;
  TOpenAPIAfterRequestHandler = Procedure(Sender : TObject; aRequest : TRequest; aResponse: TResponse) of object;
  { TFPOpenAPIModule }
  TArgumentLocation = (alQuery,alPath,alHeader,alCookie);
  TArgumentLocations = set of TArgumentLocation;

  TFPOpenAPIModule = class(TDataModule)
  Private
    FAfterRequest: TOpenAPIAfterRequestHandler;
    FBeforeRequest: TOpenAPIBeforeRequestHandler;
    Type
      TOpenAPIRoute = Class(TObject)
        APIMethod : Pointer;
        APIClass : TFPOpenAPIModuleClass;
        Streaming : Boolean;
        constructor create(aMethod : TMethod; aStreaming : Boolean);
      end;
    class var
      _Routes : TFPObjectList;
  protected
    class constructor Init;
    class destructor Done;
    class procedure RegisterOpenAPIRoute(aBaseURL, aEndPoint: String; aMethodPointer: TRouteEvent; aStreaming: Boolean);
    class function GetClassInstance(aClass: TFPOpenAPIModuleClass; aOwner: TComponent=nil; aStreaming: Boolean=False): TFPOpenAPIModule; virtual;
    class procedure ReleaseClassInstance(aInstance: TFPOpenAPIModule); virtual;
    class procedure HandleOpenAPIRequest(aData : Pointer; ARequest: TRequest; AResponse: TResponse); static;
    function DoPrepareRequest(aRequest: TRequest; aResponse : TResponse) : Boolean;
    procedure DoProcessResponse(aRequest: TRequest; aResponse : TResponse);
    function PrepareRequest(aRequest: TRequest; aResponse : TResponse) : Boolean;
    Procedure ProcessResponse(aRequest: TRequest; aResponse : TResponse);
    procedure handleRequestError(aError : Exception; aRequest : TRequest; aResponse : TResponse); virtual;
    function ExtractRequestArgument(aRequest : TRequest; aLocation : TArgumentLocation; aName : String; aDefault : String) : String;
    function ExtractRequestArgument(aRequest : TRequest; aLocation : TArgumentLocation; aName : String; aDefault : Integer) : Integer;
    function ExtractRequestArgument(aRequest : TRequest; aLocation : TArgumentLocation; aName : String; aDefault : Int64) : Int64;
    function ExtractRequestArgument(aRequest : TRequest; aLocation : TArgumentLocation; aName : String; aDefault : TDateTime) : TDateTime;
    function ExtractRequestArgument(aRequest : TRequest; aLocation : TArgumentLocation; aName : String; aDefault : Double) : Double;
  Public
    class var APIModuleOwner : TComponent;
    class Procedure RegisterAPIRoutes(aBaseURL : String; aUseStreaming : Boolean = False); virtual; abstract;
  published
    Property BeforeRequest : TOpenAPIBeforeRequestHandler Read FBeforeRequest Write FBeforeRequest;
    Property AfterRequest : TOpenAPIAfterRequestHandler Read FAfterRequest Write FAfterRequest;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.DateUtils;
{$ELSE}
uses dateutils;
{$ENDIF}

{ TFPOpenAPIModule.TOpenAPIRoute }

constructor TFPOpenAPIModule.TOpenAPIRoute.create(aMethod: TMethod; aStreaming : Boolean);
begin
  APIMethod:=aMethod.Code;
  // As a class method, Data contains the class pointer...
  APIClass:=TFPOpenAPIModuleClass(aMethod.Data);
  Streaming:=aStreaming;
end;

{ TFPOpenAPIModule }

class constructor TFPOpenAPIModule.Init;
begin
  _Routes:=TFPObjectList.Create(True);
end;

class destructor TFPOpenAPIModule.Done;
begin
  FreeAndNil(_Routes);
end;

class procedure TFPOpenAPIModule.RegisterOpenAPIRoute(aBaseURL, aEndPoint: String; aMethodPointer: TRouteEvent; aStreaming : Boolean);

var
  lRoute : TOpenAPIRoute;
  lURL,lEnd : string;

begin
  lRoute:=TOpenAPIRoute.create(TMethod(aMethodPointer),aStreaming);
  _Routes.Add(lRoute);
  lURL:=IncludeHTTPPathDelimiter(aBaseURL);
  lEnd:=aEndPoint;
  While (lEnd<>'') and (lEnd[1]='/') do
    Delete(lEnd,1,1);
  lURL:=lURL+lEnd;
  HTTPRouter.RegisterRoute(lURL,lRoute,@HandleOpenAPIRequest,False);
end;

class function TFPOpenAPIModule.GetClassInstance(aClass: TFPOpenAPIModuleClass; aOwner : TComponent = Nil; aStreaming : Boolean = False): TFPOpenAPIModule;
begin
  if aStreaming then
    Result:=TFPOpenAPIModuleClass(aClass).Create(aOwner)
  else
    Result:=TFPOpenAPIModuleClass(aClass).CreateNew(aOwner,0);
end;

class procedure TFPOpenAPIModule.ReleaseClassInstance(aInstance: TFPOpenAPIModule);
begin
  aInstance.Free;
end;

class procedure TFPOpenAPIModule.HandleOpenAPIRequest(aData: Pointer; ARequest: TRequest; AResponse: TResponse);

var
  aRoute : TOpenAPIRoute absolute aData;

var
  lModule : TFPOpenAPIModule;
  lMethod : TMethod;

begin
  lModule:=GetClassInstance(aRoute.APIClass,APIModuleOwner,aRoute.Streaming);
  try
    lMethod.Data:=lModule;
    lMethod.Code:=aRoute.APIMethod;
    TRouteEvent(lMethod)(aRequest,aResponse);
  finally
    ReleaseClassInstance(lModule);
  end;
end;

function TFPOpenAPIModule.DoPrepareRequest(aRequest: TRequest; aResponse: TResponse): Boolean;
begin
  Result:=True;
  if Assigned(BeforeRequest) then
    BeforeRequest(Self,aRequest,aResponse,Result);
end;

procedure TFPOpenAPIModule.DoProcessResponse(aRequest: TRequest; aResponse: TResponse);
begin
  if Assigned(AfterRequest) then
    AfterRequest(Self,aRequest,aResponse);
end;

function TFPOpenAPIModule.PrepareRequest(aRequest: TRequest; aResponse: TResponse): Boolean;
begin
  aResponse.ContentType:='application/json';
  Result:=DoPrepareRequest(aRequest,aResponse);
end;

procedure TFPOpenAPIModule.ProcessResponse(aRequest: TRequest; aResponse: TResponse);
begin
  if (aResponse.ContentLength=0) then
    aResponse.ContentLength:=Length(aResponse.Content);
  DoProcessResponse(aRequest,aResponse);
  if not aResponse.ContentSent then
    aResponse.SendContent;
end;

procedure TFPOpenAPIModule.handleRequestError(aError: Exception; aRequest: TRequest; aResponse: TResponse);

var
  lError : TJSONObject;

begin
  lError:=TJSONObject.Create(['classname',aError.ClassName,'message',aError.message]);
  aResponse.Code:=500;
  aResponse.CodeText:='INTERNAL SERVER ERROR';
  aResponse.Content:=lError.AsJSON;
  aResponse.ContentLength:=Length(aResponse.Content);
  aResponse.ContentType:='application/json';
  ProcessResponse(aRequest,aResponse);
end;

function TFPOpenAPIModule.ExtractRequestArgument(aRequest: TRequest;aLocation : TArgumentLocation; aName: String; aDefault: String): String;
begin
  case aLocation of
    alPath: Result:=aRequest.RouteParams[aName];
    alQuery : Result:=aRequest.QueryFields.Values[aName];
    alHeader : Result:=aRequest.GetCustomHeader(aName);
    alCookie : Result:=aRequest.CookieFields.Values[aName];
  end;
  if (Result='') then
    Result:=aDefault;
end;

function TFPOpenAPIModule.ExtractRequestArgument(aRequest: TRequest; aLocation : TArgumentLocation; aName: String; aDefault: Integer): Integer;

var
  S : String;

begin
  S:=ExtractRequestArgument(aRequest,aLocation,aName,'');
  Result:=StrToIntDef(S,aDefault);
end;

function TFPOpenAPIModule.ExtractRequestArgument(aRequest: TRequest; aLocation : TArgumentLocation; aName: String; aDefault: Int64): Int64;
var
  S : String;

begin
  S:=ExtractRequestArgument(aRequest,aLocation,aName,'');
  Result:=StrToInt64Def(S,aDefault);
end;

function TFPOpenAPIModule.ExtractRequestArgument(aRequest: TRequest; aLocation : TArgumentLocation; aName: String; aDefault: TDateTime): TDateTime;

var
  S : String;

begin
  S:=ExtractRequestArgument(aRequest,aLocation,aName,'');
  if S='' then
    Result:=aDefault
  else if not TryISOStrToDateTime(S,Result) then
    Result:=aDefault;
end;

function TFPOpenAPIModule.ExtractRequestArgument(aRequest: TRequest; aLocation : TArgumentLocation; aName: String; aDefault: Double): Double;
var
  S : String;
  aCode : Integer;

begin
  S:=ExtractRequestArgument(aRequest,aLocation,aName,'');
  if S='' then
    Result:=aDefault
  else
    begin
    Val(S,Result,aCode);
    if aCode<>0 then
      Result:=aDefault;
    end;
end;

end.

