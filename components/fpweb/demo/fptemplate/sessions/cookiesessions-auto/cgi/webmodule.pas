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
    procedure DataModuleNewSession(Sender: TObject);
    procedure DataModuleSessionExpired(Sender: TObject);
    procedure gotonextpageRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    NewSessionCreated : Boolean;
    ASessionExpired : Boolean;
    procedure GetSessionEvent(Var ASession : TCustomSession);
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

procedure TFPWebModule1.GetSessionEvent(Var ASession : TCustomSession);
var
 S:TFPWebSession;
begin
  S := TFPWebSession.Create(Nil);
  S.SessionCookie := 'ACustomCookieName'; {Use this to set the cookie name that will be used for the session management. Default is 'FPWebSession'}
//  S.SessionDir := '/Path/To/A/Directory/';{Use this if you don't want the automatic Temp dir to store the sessionID files}
  S.TimeoutMinutes := 2;//with a session timeout of 2 minutes (default is 15)
  ASession := S;
end;

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  NewSessionCreated := false;
  ASessionExpired := false;
  Template.AllowTagParams := true;
  Template.StartDelimiter := '{+';        //The default is { and } which is usually not good if we use Javascript in our templates
  Template.EndDelimiter := '+}';
  OnGetDefaultSession := @GetSessionEvent;
  CreateSession := true;                  //Turn on automatic session handling for this web module
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
      ReplaceText := IncludeTrailingPathDelimiter(GetTempDir(True)) + Session.SessionID;
{NOTE: GetTempDir
used by the session manager returns the OS temporary directory if possible, or from the
environment variable TEMP . For CGI programs you need to pass global environment
variables, it is not automatic. For example in the Apache httpd.conf with a
"PassEnv TEMP" or "SetEnv TEMP /pathtotmpdir" line so the web server passes this
global environment variable to the CGI programs' local environment variables.
}
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
