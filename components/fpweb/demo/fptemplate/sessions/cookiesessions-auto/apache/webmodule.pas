unit webmodule; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, HTTPDefs, websession, fpHTTP, fpWeb; 

type

  { TFPWebModule1 }

  TFPWebModule1 = class(TFPWebModule)
    procedure DataModuleAfterResponse(Sender: TObject; AResponse: TResponse);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModuleNewSession(Sender: TObject);
    procedure DataModuleSessionExpired(Sender: TObject);
    procedure gotonextpageRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    NewSessionCreated : Boolean;
    ASessionExpired : Boolean;
    procedure AutoSessionTemplateReplaceTag(Sender: TObject; const TagString:String;
      TagParams: TStringList; Out ReplaceText: String);
  public
    { public declarations }
  end; 

var
  FPWebModule1: TFPWebModule1; 

implementation

{ TFPWebModule1 }

procedure TFPWebModule1.DataModuleAfterResponse(Sender: TObject;
  AResponse: TResponse);
begin
  //reset global variables for apache modules for the next incoming request
  NewSessionCreated := false;
  ASessionExpired := false;
  //
end;

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
var
 S:TFPWebSession;
begin
  NewSessionCreated := false;
  ASessionExpired := false;
  Template.AllowTagParams := true;
  Template.StartDelimiter := '{+';        //The default is { and } which is usually not good if we use Javascript in our templates
  Template.EndDelimiter := '+}';

  CreateSession := true;                  //Turn on automatic session handling for this web module
  S := TFPWebSession.Create(Nil);
  S.SessionCookie := 'AFPCustomCookieName'; {Use this to set the cookie name that will be used for the session management. Default is 'FPWebSession'}
  S.SessionDir := GlobalSessionDir;//Default temp directory
  //or
  S.SessionDir := IncludeTrailingPathDelimiter(GetTempDir(True));{dir to store the sessionID files}
  S.TimeoutMinutes := 2;//with a session timeout of 2 minutes (default is 15)
  Session := S;
end;

procedure TFPWebModule1.DataModuleDestroy(Sender: TObject);
begin
  if Assigned(Session) then
  begin                  //free the session object created at the web module creation
    Session.Free;
    Session := nil;
  end;
end;

procedure TFPWebModule1.DataModuleNewSession(Sender: TObject);
begin
  NewSessionCreated := true;
end;

procedure TFPWebModule1.DataModuleSessionExpired(Sender: TObject);
begin
  ASessionExpired := true;
end;

procedure TFPWebModule1.gotonextpageRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin     //Template:TFPTemplate is a property of the web module
  Template.FileName := 'autosession-template.html';
  Template.OnReplaceTag := @AutoSessionTemplateReplaceTag;

  AResponse.Content := Template.GetContent;

  Handled := true;
end;

procedure TFPWebModule1.AutoSessionTemplateReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
begin
  if AnsiCompareText(TagString, 'DATETIME') = 0 then
  begin
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now);
  end else

  if AnsiCompareText(TagString, 'SESSIONID') = 0 then
  begin
    if Assigned(Session) then
      ReplaceText := Session.SessionID;
  end else

  if AnsiCompareText(TagString, 'TIMEOUTMINUTES') = 0 then
  begin
    if Assigned(Session) then
      ReplaceText := IntToStr(Session.TimeOutMinutes);
  end else

  if AnsiCompareText(TagString, 'SESSIONFILE') = 0 then
  begin
    if Assigned(Session) then
      ReplaceText := IncludeTrailingPathDelimiter(TFPWebSession(Session).SessionDir) + Session.SessionID;
  end else

  if AnsiCompareText(TagString, 'EXPIREDMESSAGE') = 0 then
  begin
    if Assigned(Session) and ASessionExpired then
      ReplaceText := TagParams.Values['MESSAGE'];
  end else

    if AnsiCompareText(TagString, 'NEWSESSIONMESSAGE') = 0 then
  begin
    if Assigned(Session) and NewSessionCreated then
      ReplaceText := TagParams.Values['MESSAGE'];
  end else


  begin
    //Not found value for tag -> TagString
    ReplaceText := '[Template tag {+' + TagString + '+} is not implemented yet.]';
  end;
end;

initialization
  {$I webmodule.lrs}

  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.
