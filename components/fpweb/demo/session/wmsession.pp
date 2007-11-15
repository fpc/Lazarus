unit wmsession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, HTTPDefs, websession, fpHTTP,
    fpWeb; 

type

  { TSessionModule }

  TSessionModule = class(TFPWebModule)
    procedure EndSessionRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure InSessionRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure NewSessionRequest(Sender: TObject; ARequest: TRequest;
      AResponse: TResponse; var Handled: Boolean);
    procedure SessionModuleNewSession(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SessionModule: TSessionModule;

implementation

{ TSessionModule }

{
  The default action is the 'InSession' action. When a new session
  is started, the newsession event handler is called, and
  we set the 'NewSession' action as default.
}

procedure TSessionModule.SessionModuleNewSession(Sender: TObject);
begin
  Actions.ActionByName('NewSession').Default:=True;
  Actions.ActionByName('InSession').Default:=False;
end;

{
  When a new session is detected

  - either because there was no session,in which case NewSession is the default

  - The URL contained the newsession action in the 'DemoSession' action variable,
    something like:

  http://localhost/cgi-bin/sessiondemo.cgi?DemoSession=NewSession
  
  in either case, the NewSession action is called, and this event is triggered:
}

procedure TSessionModule.NewSessionRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
  
Var
  C : TCookie;
  
begin
  With AResponse.Contents do
    begin
    Add('<HTML><TITLE>Demo session was started</TITLE><BODY>');
    Add('<H1>New session started</H1>');
    Add('A new session was started<P>.');
    If Session is TFPWebSession then
      begin
      C:=AResponse.Cookies.FindCookie((Session as TFPWebSession).SessionCookie);
      If Assigned(C) then
        begin
        Add('The issued session cookie is called <B>'+C.Name+'</B><BR> ');
        Add('The issued session cookie has value <B>'+C.Value+'</B><BR>.');
        end
      else
        Add('No session cookie was found.');
      end;
    Add('</BODY></HTML>');
    end;
  Handled:=True; // Content will be sent.
end;

{
  The default action is the 'InSession' action.
  We display the session cookie, and the value (name 'Var')
  that is currently stored in the session object.

  If the user supplied a new value for 'var', we store it in the session.
  to supply the value, append
  ?var=value
  to the URL.
}

procedure TSessionModule.InSessionRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);

Var
  V : string;
  C : TCookie;

begin
  With AResponse.Contents do
    begin
    Add('<HTML><TITLE>Demo session active</TITLE><BODY>');
    Add('<H1>Demo session active</H1>');
    Add('The demo session is still active<P>');
    If Session is TFPWebSession then
      begin
      C:=AResponse.Cookies.FindCookie((Session as TFPWebSession).SessionCookie);
      If Assigned(C) then
        begin
        Add('Current session Cookie is called <B>'+C.Name+'</B><BR>');
        Add('and has value <B>'+C.Value+'</B>.');
        end;
      V:=Session.Variables['Var'];
      If (V<>'') then
        Add('<P>Stored session value: <B>'+V+'</B>.')
      else
        Add('<P>No values stored in session.');
      V:=ARequest.QueryFields.Values['Var'];
      If V<>'' then
        begin
        Add('<P>Storing new session value: <B>'+V+'</B>.');
        Session.Variables['Var']:=V;
        end;
      end;
    Add('</BODY></HTML>');
    AResponse.SendContent; // Handles the response.
    end;
end;


{
  When the 'EndSession' action is called, the session is ended. The
  endsession action can be called by providing the 'EndSession' value for
  the 'DemoSession' action variable, something like:

  http://localhost/cgi-bin/sessiondemo.cgi?DemoSession=EndSession
}
procedure TSessionModule.EndSessionRequest(Sender: TObject; ARequest: TRequest;
  AResponse: TResponse; var Handled: Boolean);
begin
  // Stop the session
  Session.Terminate;
  With AResponse.Contents do
    begin
    Add('<HTML><TITLE>Demo Session Is Terminated</TITLE><BODY>');
    Add('<H1>Demo session Terminated</H1>');
    Add('The session was terminated, the cookie is cleared and the');
    Add('stored value is lost');
    Add('</BODY></HTML>');
    end;
  AResponse.SendContent;
end;

initialization
  {$I wmsession.lrs}

  RegisterHTTPModule('session', TSessionModule);
end.

