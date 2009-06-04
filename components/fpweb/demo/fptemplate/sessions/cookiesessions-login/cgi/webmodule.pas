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
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure logoutRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure someactionRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    NewSessionCreated : Boolean;
    ASessionExpired : Boolean;
    LoggedInLoginName : String;
    SessionDBFile : String;
    UserDBFile : String;
    function NotLoggedIn:Boolean;
    function CommonTemplateTagReplaces(const TagString:String;
      TagParams: TStringList; Out ReplaceText: String):Boolean;
    procedure GetSessionEvent(Var ASession : TCustomSession);

    procedure loginReplaceTag(Sender: TObject; const TagString:String;
      TagParams: TStringList; Out ReplaceText: String);
    procedure logoutReplaceTag(Sender: TObject; const TagString:String;
      TagParams: TStringList; Out ReplaceText: String);
    procedure welcomeReplaceTag(Sender: TObject; const TagString:String;
      TagParams: TStringList; Out ReplaceText: String);
    procedure someactionReplaceTag(Sender: TObject; const TagString:String;
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
  LoggedInLoginName := '';
  //
end;

procedure TFPWebModule1.GetSessionEvent(Var ASession : TCustomSession);
var
 S:TFPWebSession;
begin
  S := TFPWebSession.Create(Nil);
  S.SessionCookie := 'CustomCookieName';  {Use this to set the cookie name that will be used for the session management. Default is 'FPWebSession'}
//  S.SessionDir := '/Path/To/A/Directory/';{Use this if you don't want the automatic Temp dir to store the sessionID files}
  S.TimeoutMinutes := 2;//with a session timeout of 2 minutes (default is 15)
  ASession := S;
end;

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  Template.AllowTagParams := true;
  Template.StartDelimiter := '{+';         //The default is { and } which is usually not good if we use Javascript in our templates
  Template.EndDelimiter := '+}';
  NewSessionCreated := false;
  ASessionExpired := false;
  OnGetDefaultSession := @GetSessionEvent;
  CreateSession := true;                   //Turn on automatic session handling for this web module
  sessiondbfile := 'sessiondb.txt';        //This will contain the name=sessionID pairs
  userdbfile := 'userdb.txt';              //This simulates a user database with passwords
end;

function FindNameInList(const SL:TStrings; const N:String):String;
var
    i : Integer;
begin
  Result := '';
  for i := 0 to SL.Count - 1 do
    if SL.Names[i] = N then
    begin
      Result := SL.Values[SL.Names[i]];//return with the sessionID
      break;
    end;
end;

function FindValueInList(const SL:TStrings; const Sess:String):String;
var
  s : String;
  i : Integer;
begin
  Result := '';
  if SL.Count <= 0 then Exit;
  s := '=' + Sess;
  i := 0;
  repeat
    if pos(s, SL[i]) > 0 then
    begin
      Result := SL.Names[i];
      break;
    end;
    inc(i);
  until i >= SL.Count;
end;

procedure RemoveValueIfExists(SL:TStrings; const S_ID:String);
var
  s : String;
  i : Integer;
begin
  if SL.Count <= 0 then Exit;
  s := '=' + S_ID;
  i := 0;
  repeat
    if pos(s, SL[i]) > 0 then
      SL.Delete(i)
    else
      inc(i);
  until i >= SL.Count;
end;

procedure RemoveNameIfExists(SL:TStrings; const N:String);
var
  i: Integer;
begin
  if SL.Count <= 0 then Exit;
  i := 0;
  repeat
    if SL.Names[i] = N then
      SL.Delete(i)
    else
      inc(i);
  until i >= SL.Count;
end;

function TFPWebModule1.NotLoggedIn:Boolean;
var
  sessiondatabase : TStringlist;
begin
  Result := false;

  //check if the current sessionID is in the sessionDB
  sessiondatabase := TStringList.Create;
  if FileExists(sessiondbfile) then
    sessiondatabase.LoadFromFile(sessiondbfile);
  LoggedInLoginName := FindValueInList(sessiondatabase, session.sessionID);
  sessiondatabase.Free;
  //

  if LoggedInLoginName = '' then
  begin
    Result := true;   //not found -> not logged in or previous session has expired

    //show the login screen again with the expired session message
    Template.FileName := 'testlogin.html';
    Template.OnReplaceTag := @loginReplaceTag;
    Request.QueryFields.Add('MSG=SESSIONEXPIRED');
    Response.Content := Template.GetContent;
  end;
end;

procedure TFPWebModule1.DataModuleNewSession(Sender: TObject);
begin
  NewSessionCreated := true;
end;

procedure TFPWebModule1.DataModuleSessionExpired(Sender: TObject);
var
  sessiondatabase : TStringlist;
begin
  ASessionExpired := true;
  //delete the expired session ID from the session database
  sessiondatabase := TStringList.Create;
  if FileExists(sessiondbfile) then
    sessiondatabase.LoadFromFile(sessiondbfile);
  if sessiondatabase.Count > 0 then
  begin
    RemoveValueIfExists(sessiondatabase, session.sessionid);
    sessiondatabase.SaveToFile(sessiondbfile);
  end;
  sessiondatabase.Free;
end;

procedure TFPWebModule1.loginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  loginname, pwd, pwd1 : String;
  userdatabase, sessiondatabase : TStringlist;
  i: Integer;
begin
  Handled := true;
  Template.FileName := 'testlogin.html';
  Template.OnReplaceTag := @loginReplaceTag;
  AResponse.CustomHeaders.Add('Pragma=no-cache');//do not cache the response in the web browser

  if FindNameInList(ARequest.ContentFields, 'LoginName') = '' then
  begin//called the login action without parameters -> display the login page
    ARequest.QueryFields.Add('MSG=NORMAL');
    AResponse.Content := Template.GetContent;
    Exit;
  end;

  loginname := Trim(ARequest.ContentFields.Values['LoginName']);
  pwd := Trim(ARequest.ContentFields.Values['Password']);
  if (pwd = '') or (loginname = '') then
  begin//empty login name or password -> return to the login screen
    ARequest.QueryFields.Add('MSG=MISSING');
    AResponse.Content := Template.GetContent;
    Exit;
  end;

  //simulate a user database loaded into a stringlist
  userdatabase := TStringlist.Create;
  userdatabase.LoadFromFile(userdbfile);
  //

  pwd1 := userdatabase.values[LoginName];
  userdatabase.free;
  if pwd <> pwd1 then
  begin//either the password or the login name was invalid
    ARequest.QueryFields.Add('MSG=INVLOGIN');
    AResponse.Content := Template.GetContent;
    Exit;
  end;

  //succesful login
  LoggedInLoginName := loginname;

  //session starting, need to store it somewhere next to the name of the logged in person
  sessiondatabase := TStringList.Create;
  if FileExists(sessiondbfile) then
    sessiondatabase.LoadFromFile(sessiondbfile);
  if sessiondatabase.Count > 0 then
    RemoveValueIfExists(sessiondatabase, Session.SessionID);  //New login, kill all sessions with this session ID (same computer, same browser, multiple persons)
  if FindNameInList(sessiondatabase, LoginName) <> '' then
    sessiondatabase.Values[LoginName] := Session.SessionID    //overwrite with the new session ID
  else
    sessiondatabase.Add(LoginName + '=' + Session.SessionID); //create a new entry for this person
  sessiondatabase.SaveToFile(sessiondbfile);
  sessiondatabase.Free;

  //generate the Welcome page content
  Template.FileName := 'testwelcome.html';
  Template.OnReplaceTag := @welcomeReplaceTag;
  AResponse.Content := Template.GetContent;
end;

procedure TFPWebModule1.loginReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
begin
  {Handle tags used in multiple templates}
  if CommonTemplateTagReplaces(TagString, TagParams, ReplaceText) then
    Exit;

  {Handle tags specific to this template if there are any}
  if AnsiCompareText(TagString, 'MESSAGE') = 0 then
  begin
    ReplaceText := TagParams.Values[Request.QueryFields.Values['MSG']];
  end else

  {Message for tags not handled}
  begin
    ReplaceText := '[Template tag "' + TagString + '" is not implemented yet.]';
  end;
end;

procedure TFPWebModule1.welcomeReplaceTag(Sender: TObject; const TagString:String;
      TagParams: TStringList; Out ReplaceText: String);
begin
  {Handle tags used in multiple templates}
  if CommonTemplateTagReplaces(TagString, TagParams, ReplaceText) then
    Exit;

  {Handle tags specific to this template if there are any}


  {Message for tags not handled}
  begin
    ReplaceText := '[Template tag "' + TagString + '" is not implemented yet.]';
  end;
end;

procedure TFPWebModule1.logoutRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  sessiondatabase : TStringList;
begin
  Handled := true;

  if NotLoggedIn then Exit;

  //delete the sessionID and all occurences of the login name assigned to it from the sessiondb
  sessiondatabase := TStringList.Create;
  if FileExists(sessiondbfile) then
    sessiondatabase.LoadFromFile(sessiondbfile);
  if sessiondatabase.Count > 0 then
  begin
    RemoveValueIfExists(sessiondatabase, session.SessionID);
    RemoveNameIfExists(sessiondatabase, LoggedInLoginName);
    sessiondatabase.SaveToFile(sessiondbfile);
  end;
   sessiondatabase.Free;
  //

  Template.FileName := 'testlogout.html';
  Template.OnReplaceTag := @logoutReplaceTag;
  AResponse.Content := Template.GetContent;//generate the Logout page content.
end;

procedure TFPWebModule1.logoutReplaceTag(Sender: TObject; const TagString:String;
  TagParams: TStringList; Out ReplaceText: String);
begin
  {Handle tags used in multiple templates}
  if CommonTemplateTagReplaces(TagString, TagParams, ReplaceText) then
    Exit;

  {Handle tags specific to this template if there are any}


  {Message for tags not handled}
  begin
    ReplaceText := '[Template tag "' + TagString + '" is not implemented yet.]';
  end;
end;

procedure TFPWebModule1.someactionRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled := true;

  if NotLoggedIn then Exit;

  Template.FileName := 'testsomepage.html';
  Template.OnReplaceTag := @someactionReplaceTag;
  AResponse.Content := Template.GetContent;
end;

procedure TFPWebModule1.someactionReplaceTag(Sender: TObject; const TagString:
  String; TagParams: TStringList; Out ReplaceText: String);
begin
  {Handle tags used in multiple templates}
  if CommonTemplateTagReplaces(TagString, TagParams, ReplaceText) then
    Exit;

  {Handle tags specific to this template if there are any}


  {Message for tags not handled}
  begin
    ReplaceText := '[Template tag {+' + TagString + '+} is not implemented yet.]';
  end;
end;

function TFPWebModule1.CommonTemplateTagReplaces(const TagString:String;
  TagParams: TStringList; out ReplaceText: String):Boolean;
begin
  Result := true;

  if AnsiCompareText(TagString, 'DATETIME') = 0 then
  begin
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now);
  end else

  if AnsiCompareText(TagString, 'SESSIONID') = 0 then
  begin
    if Assigned(Session) then
      ReplaceText := Session.SessionID;
  end else

  if AnsiCompareText(TagString, 'MINUTESLEFT') = 0 then
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

  if AnsiCompareText(TagString, 'LOGINNAME') = 0 then
  begin
      ReplaceText := LoggedInLoginName;
  end else

  Result := false;
end;

initialization
  {$I webmodule.lrs}

  RegisterHTTPModule('TFPWebModule1', TFPWebModule1); 
end.
