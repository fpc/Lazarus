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
    procedure loginRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure logoutRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure someactionRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
  private
    { private declarations }
    LoggedInLoginName : String;
    SessionID: String;
    SessionDBFile : String;
    UserDBFile : String;
    SessionVariable: String;
    TimeoutMinutes: Integer;
    function RemoveExpiredSessions(SL:TStringList; const SIDToDelete:String):Boolean;
    function NotLoggedIn:Boolean;
    function CommonTemplateTagReplaces(const TagString:String;
      TagParams: TStringList; Out ReplaceText: String):Boolean;

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
var
  sessiondatabase:TStringList;
  SIDLastRefresh:String;
begin
  //update the session DB for the current session
  if (SessionID <> '')and(LoggedinLoginName <> '') then
  begin//for many concurrent request websites this part needs to be modified to have some kind of locking while writing into the file/relational database
    SIDLastRefresh := '';
    sessiondatabase := TStringList.Create;
    if FileExists(sessiondbfile) then
      sessiondatabase.LoadFromFile(sessiondbfile);
    SIDLastRefresh := sessiondatabase.Values[SessionID];
    if SIDLastRefresh <> '' then
    begin
      sessiondatabase.Values[SessionID] := DateTimeToStr(Now) + LoggedinLoginName;//update the Last refresh time
      sessiondatabase.SaveToFile(sessiondbfile);
    end;
    sessiondatabase.Free;
  end;

  //reset global variables for apache modules for the next incoming request
  LoggedInLoginName := '';
  SessionID := '';
  //
end;

procedure TFPWebModule1.DataModuleCreate(Sender: TObject);
begin
  Template.AllowTagParams := true;
  Template.StartDelimiter := '{+'; //The default is { and } which is usually not good if we use Javascript in our templates
  Template.EndDelimiter := '+}';
  sessiondbfile := 'session-db.txt';//This will contain the sessionID=expiration pairs
  userdbfile := 'userdb.txt';      //This simulates a user database with passwords
  TimeOutMinutes := 2;             //With a session timeout of 2 minutes
  SessionVariable := 'sid';        //Query parameter name for the session ID, for all links in the templates
  LongTimeFormat := 'hh:mm:ss';    //to save on date time conversion code
  ShortDateFormat := 'YYYY/MM/DD'; //to save on date time conversion code
end;

function FindNameInList(const SL:TStrings; const N:String):String;
var
    i : Integer;
begin
  Result := '';
  for i := 0 to SL.Count - 1 do
    if SL.Names[i] = N then
    begin
      Result := SL.Values[SL.Names[i]];
      break;
    end;
end;

function TFPWebModule1.RemoveExpiredSessions(SL:TStringList; const SIDToDelete:String):Boolean;
var
  DT:TDateTime;
  i, j: Integer;
  s, SIDLastRefresh: String;
begin
  Result := false;

  if SL.Count <= 0 then Exit;
  i := 0;
  repeat
    s := SL[i];
    j := pos('=', s);
    if j > 0 then
    begin
      if copy(s, 1, j - 1) = SIDToDelete then
      begin
        SL.Delete(i);
        dec(i);
      end else begin
        SIDLastRefresh := copy(s, j + 1, 19);{YYYY/MM/DD hh:mm:ss}
        DT := StrToDateTime(SIDLastRefresh);
        if ((Now - DT) > (TimeOutMinutes/1440)) then
        begin
          Result := true;
          SL.Delete(i);
          dec(i);
        end;
      end;
    end;
    inc(i);
  until i >= SL.Count;
end;

function TFPWebModule1.NotLoggedIn:Boolean;
var
  sessiondatabase:TStringlist;
  SIDLastRefresh:String;
begin
  Result := false;

  //check if the current sessionID is valid
  SessionID := UpperCase(Request.QueryFields.Values[SessionVariable]);
  if SessionID <> '' then
  begin
    sessiondatabase := TStringList.Create;
    if FileExists(sessiondbfile) then
      sessiondatabase.LoadFromFile(sessiondbfile);
//    if RemoveExpiredSessions(sessiondatabase, '') then  //Remove all expired sessions
//      sessiondatabase.SaveToFile(sessiondbfile);   {enough to purge only at logout events}
    RemoveExpiredSessions(sessiondatabase, '');      {                                     }
    SIDLastRefresh := sessiondatabase.Values[SessionID];
    sessiondatabase.Free;

    if SIDLastRefresh <> '' then
    begin
      LoggedinLoginName := copy(SIDLastRefresh, 20, 1024);
      Exit;//OK
    end;
  end;

  //show the login screen again with the expired session message
  Template.FileName := 'testurllogin.html';
  Template.OnReplaceTag := @loginReplaceTag;
  Request.QueryFields.Add('MSG=SESSIONEXPIRED');
  Response.Content := Template.GetContent;
  Result := true;
end;

procedure TFPWebModule1.loginRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  loginname, pwd, pwd1 : String;
  userdatabase, sessiondatabase : TStringlist;
  G : TGUID;
begin
  Handled := true;
  Template.FileName := 'testurllogin.html';
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

  if userdatabase.Count <= 0 then
  begin//cannot find user DB or it is empty
    ARequest.QueryFields.Add('MSG=NODB');
    AResponse.Content := Template.GetContent;
    Exit;
  end;
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
  CreateGUID(G);
  SessionID:=UpperCase(GuiDToString(G));
  sessiondatabase.Add(SessionID + '=' + DateTimeToStr(Now) + LoggedinLoginName);//create a new entry for this session
  sessiondatabase.SaveToFile(sessiondbfile);//for many concurrent request websites this part needs to be modified to have some kind of locking while writing into the file/relational database
  sessiondatabase.Free;

  //generate the Welcome page content
  Template.FileName := 'testurlwelcome.html';
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
    ReplaceText := '[Template tag {+' + TagString + '+} is not implemented yet.]';
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
    ReplaceText := '[Template tag {+' + TagString + '+} is not implemented yet.]';
  end;
end;

procedure TFPWebModule1.logoutRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
var
  sessiondatabase : TStringList;
begin
  Handled := true;

  if NotLoggedIn then Exit;

  //delete the sessionID from the sessiondb with all expired sessions
  sessiondatabase := TStringList.Create;
  if FileExists(sessiondbfile) then
    sessiondatabase.LoadFromFile(sessiondbfile);
  RemoveExpiredSessions(sessiondatabase, SessionID);
  sessiondatabase.SaveToFile(sessiondbfile);//for many concurrent request websites this part needs to be modified to have some kind of locking while writing into the file/relational database
  sessiondatabase.Free;
  //

  Template.FileName := 'testurllogout.html';
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
    ReplaceText := '[Template tag {+' + TagString + '+} is not implemented yet.]';
  end;
end;

procedure TFPWebModule1.someactionRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  Handled := true;

  if NotLoggedIn then Exit;

  Template.FileName := 'testurlsomepage.html';
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

  if AnsiCompareText(TagString, 'SESSION-VARIABLE') = 0 then
  begin
    ReplaceText := SessionVariable + '=' + SessionID;
  end else

  if AnsiCompareText(TagString, 'DATETIME') = 0 then
  begin
    ReplaceText := FormatDateTime(TagParams.Values['FORMAT'], Now);
  end else

  if AnsiCompareText(TagString, 'SESSIONID') = 0 then
  begin
    ReplaceText := SessionID;
  end else

  if AnsiCompareText(TagString, 'MINUTESLEFT') = 0 then
  begin
    ReplaceText := IntToStr(TimeOutMinutes);
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
