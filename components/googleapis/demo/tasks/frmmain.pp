unit frmmain;

{$mode objfpc}{$H+}
// Define USESYNAPSE if you want to force use of synapse
{ $DEFINE USESYNAPSE}

// For version 2.6.4, synapse is the only option.
{$IFDEF VER2_6}
{$DEFINE USESYNAPSE}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  synautil, IniFiles, googlebase, googleservice, googleclient, googletasks;

type

  { TMainForm }
  TAccessTokenState = (acsWaiting,acsOK,acsCancel);

  TMainForm = class(TForm)
    BCancel: TButton;
    BSetAccess: TButton;
    BFetchTaskLists: TButton;
    BFetchTasks: TButton;
    EAccessCode: TEdit;
    GBAccess: TGroupBox;
    LTasks: TLabel;
    LEAccess: TLabel;
    LBTaskLists: TListBox;
    LBTasks: TListBox;
    procedure BCancelClick(Sender: TObject);
    procedure BFetchTasksClick(Sender: TObject);
    procedure BSetAccessClick(Sender: TObject);
    procedure BFetchTaskListsClick(Sender: TObject);
    Procedure DoUserConsent(Const AURL : String; Out AAuthCode : String) ;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBTaskListsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FAccessState : TAccessTokenState;
    FClient : TGoogleClient;
    FTasksAPI: TTasksAPI;
    FTaskLists: TTaskLists;
    FCurrentList: TTaskList;
    FTasks : TTasks;
    procedure LoadAuthConfig;
    procedure SaveRefreshToken;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation


uses {$ifdef windows}windows,{$endif}
  jsonparser, // needed
  fpoauth2,
{$IFDEF USESYNAPSE}
  ssl_openssl,
  synapse,webclient
{$ELSE}
  fphttpwebclient,
{$ENDIF}
  lclintf;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Register Tasks resources.
  TTasksAPI.RegisterAPIResources;
  // Set up google client.
  FClient:=TGoogleClient.Create(Self);
  {$IFDEF USESYNAPSE}
    FClient.WebClient:=TSynapseWebClient.Create(Self);
  {$ELSE}
    FClient.WebClient:=TFPHTTPWebClient.Create(Self);
  {$ENDIF}
  FClient.WebClient.RequestSigner:=FClient.AuthHandler;
  FClient.WebClient.LogFile:='requests.log';
  FClient.AuthHandler.WebClient:=FClient.WebClient;
  FClient.AuthHandler.Config.AccessType:=atOffLine;
  // We want to enter a code.
  FClient.OnUserConsent:=@DoUserConsent;
  // Create a Tasks API and connect it to the client.
  FTasksAPI:=TTasksAPI.Create(Self);
  FTasksAPI.GoogleClient:=FClient;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTaskLists);
  FreeAndNil(FTasks);
end;

procedure TMainForm.LBTaskListsSelectionChange(Sender: TObject; User: boolean);
begin
  BFetchTasks.Enabled:=User and (LBTaskLists.ItemIndex<>-1);
  if BFetchTasks.Enabled then
    begin
    FCurrentList:=LBTaskLists.Items.Objects[LBTaskLists.ItemIndex] as TTaskList;
    LTasks.Caption:='Tasks for list : '+FCurrentList.Title;
    end
  else
    begin
    LTasks.Caption:='Tasks for list : <select as tasklist>';
    LBTasks.Items.Clear;
    FCurrentList:=Nil;
    end;
end;

procedure TMainForm.LoadAuthConfig;

Var
  ini:TIniFile;

begin
  ini:=TIniFile.Create('google.ini');
  try
    // Registered application needs tasks scope
    FClient.AuthHandler.Config.ClientID:=ini.ReadString('Credentials','ClientID','');;
    FClient.AuthHandler.Config.ClientSecret:=ini.ReadString('Credentials','ClientSecret','');
    FClient.AuthHandler.Config.AuthScope:=ini.ReadString('Credentials','Scope','https://www.googleapis.com/auth/tasks');
    // We are offline.
    FClient.AuthHandler.Config.RedirectUri:='urn:ietf:wg:oauth:2.0:oob';
    // Session data
    FClient.AuthHandler.Session.RefreshToken:=ini.ReadString('Session','RefreshToken','');
    FClient.AuthHandler.Session.AccessToken:=ini.ReadString('Session','AccesToken','');
    FClient.AuthHandler.Session.AuthTokenType:=ini.ReadString('Session','TokenType','');
    FClient.AuthHandler.Session.AuthExpires:=ini.ReadDateTime('Session','AuthExpires',0);
    FClient.AuthHandler.Session.AuthExpiryPeriod:=Ini.ReadInteger('Session','AuthPeriod',0);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.SaveRefreshToken;

Var
  ini:TIniFile;

begin
  // We save the refresh token for later use.
  if FClient.AuthHandler.Session.RefreshToken<>'' then
    begin
    ini:=TIniFile.Create('google.ini');
    try
      ini.WriteString('Session','RefreshToken',FClient.AuthHandler.Session.RefreshToken);
      ini.WriteString('Session','AccessToken',FClient.AuthHandler.Session.AccessToken);
      ini.WriteString('Session','TokenType',FClient.AuthHandler.Session.AuthTokenType);
      ini.WriteDateTime('Session','AuthExpires',FClient.AuthHandler.Session.AuthExpires);
      ini.WriteInteger('Session','AuthPeriod',FClient.AuthHandler.Session.AuthExpiryPeriod);
    finally
      Ini.Free;
    end;
    end;
end;

procedure TMainForm.BFetchTaskListsClick(Sender: TObject);

var
  Entry: TTaskList;
  Resource : TTaskListsResource;
  EN : String;

  i:integer;
begin
  LBTaskLists.Items.Clear;
  FreeAndNil(FTaskLists);
  Resource:=Nil;
  try
    LoadAuthConfig;
    Resource:=FTasksAPI.CreateTaskListsResource;
    FTaskLists:=Resource.list('');
    SaveRefreshToken;
    if assigned(FTaskLists) then
      for i:= 0 to Length(FTaskLists.items)-1 do
        begin
        Entry:=FTaskLists.items[i];
        EN:=Entry.title;
        LBTaskLists.Items.AddObject(IntToStr(i)+': '+EN,Entry);
        end;
     BFetchTasks.Enabled:=LBTaskLists.Items.Count>0;
  finally
    FreeAndNil(Resource);
  end;
end;

procedure TMainForm.BSetAccessClick(Sender: TObject);
begin
  FAccessState:=acsOK;
  GBAccess.Visible:=False;
end;

procedure TMainForm.BCancelClick(Sender: TObject);
begin
  FAccessState:=acsCancel;
  GBAccess.Visible:=False;
end;

procedure TMainForm.BFetchTasksClick(Sender: TObject);
var
  Entry: TTask;
  EN : String;
  i:integer;

begin
  if LBTaskLists.ItemIndex<0 then
    Exit;
  LBTasks.Items.Clear;
  FreeAndNil(FTasks);
  FTasks:=FTasksAPI.TasksResource.list(FCurrentList.id,'');
  SaveRefreshToken;
  if assigned(FTasks) then
    for i:= 0 to Length(FTasks.items)-1 do
      begin
      Entry:=FTasks.items[i];
      EN:=Entry.title;
      if EN='' then
        EN:=Entry.id+' ('+Entry.Status+')';
      if Entry.Completed<>0 then
        EN:=EN+' (Completed :'+DateToStr(Entry.Completed)+')';
      LBTasks.Items.AddObject(IntToStr(i)+': '+EN,Entry);
      end;
end;

Procedure TMainForm.DoUserConsent(Const AURL: String; Out AAuthCode: String);

begin
  GBAccess.Visible:=True;
  EAccessCode.Text:='<enter code here>';
  FAccessState:=acsWaiting;
  OpenUrl(AURL);
  While (FAccessState=acsWaiting) do
    Application.ProcessMessages;
  if FAccessState=acsOK then
    AAuthCode:=EAccessCode.Text;
  GBAccess.Visible:=False;
end;

end.

