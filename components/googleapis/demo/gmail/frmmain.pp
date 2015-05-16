unit frmmain;
// Define USESYNAPSE if you want to force use of synapse
{ $DEFINE USESYNAPSE}

// For version 2.6.4, synapse is the only option.
{$IFDEF VER2_6}
{$DEFINE USESYNAPSE}
{$ENDIF}

{$mode objfpc}{$H+}

interface

uses
  googlegmail, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, synautil, IniFiles, googlebase, googleservice, googleclient;

Const
  ILeaf      = 0;
  ILeafEmpty = 1;

type

  { TMainForm }
  TAccessTokenState = (acsWaiting,acsOK,acsCancel);

  TMainForm = class(TForm)
    BCancel: TButton;
    BSetAccess: TButton;
    BRefreshFolders: TButton;
    BRefreshFiles: TButton;
    EAccessCode: TEdit;
    GBAccess: TGroupBox;
    LLabels: TLabel;
    LVMessages: TListView;
    LMails: TLabel;
    LEAccess: TLabel;
    SDDownload: TSaveDialog;
    TVLabels: TTreeView;
    procedure BCancelClick(Sender: TObject);
    procedure BRefreshFilesClick(Sender: TObject);
    procedure BSetAccessClick(Sender: TObject);
    procedure BRefreshFoldersClick(Sender: TObject);
    Procedure DoUserConsent(Const AURL : String; Out AAuthCode : String) ;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVMessagesDblClick(Sender: TObject);
    procedure TVLabelsSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FAccessState : TAccessTokenState;
    FClient : TGoogleClient;
    FGmailAPI: TGmailAPI;
    procedure AddLabels;
    procedure ClearMailListView;
    procedure ClearTreeView;
    function CreateNodeWithTextPath(TextPath: string): TTreeNode;
    procedure LoadAuthConfig;
    procedure SaveRefreshToken;
    procedure ShowLabel(ALabelID: String);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation


uses
  strutils,
  ssl_openssl,
  jsonparser, // needed
  fpjson,
  fpoauth2,
  lclintf,
  fpwebclient,
  {$IFDEF USESYNAPSE}
    ssl_openssl,
    synapsewebclient
  {$ELSE}
    fphttpwebclient
  {$ENDIF}
  ;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Register Tasks resources.
  TGmailAPI.RegisterAPIResources;
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
  FGmailAPI:=TGmailAPI.Create(Self);
  FGmailAPI.GoogleClient:=FClient;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.LVMessagesDblClick(Sender: TObject);

Var
  Entry : TMessage;
  Request : TWebClientRequest;
  Response: TWebClientResponse;
  S,URL,LFN: String;
  D : TJSONEnum;
begin
  If Not (Assigned(LVMessages.Selected) and Assigned(LVMessages.Selected.Data)) then
    Exit;
  Entry:=TMessage(LVMessages.Selected.Data);
  // Add some code here to show the message
end;

procedure TMainForm.TVLabelsSelectionChanged(Sender: TObject);
begin
  BRefreshFilesClick(Sender)
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
    FClient.AuthHandler.Config.AuthScope:=ini.ReadString('Credentials','Scope','https://www.googleapis.com/auth/drive');
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

procedure TMainForm.ClearTreeView;

Var
  I : Integer;

begin
  With TVLabels.Items do
    begin
    BeginUpdate;
    try
      For I:=0 to Count-1 do
        TObject(Item[i].Data).Free;
      Clear;
    finally
      EndUpdate;
    end;
    end;
end;

function TMainForm.CreateNodeWithTextPath(TextPath: string): TTreeNode;

var
  p: SizeInt;
  CurText: String;
  AParent : TTreeNode;

begin
  Result:=nil;
  AParent:=Nil;
  repeat
    p:=System.Pos('/',TextPath);
    if p>0 then
      begin
      CurText:=LeftStr(TextPath,p-1);
      System.Delete(TextPath,1,p);
      end
    else
      begin
      CurText:=TextPath;
      TextPath:='';
      end;
    //debugln(['TTreeNodes.FindNodeWithTextPath CurText=',CurText,' Rest=',TextPath]);
    if AParent=nil then
      Result:=TVLabels.Items.FindTopLvlNode(CurText)
    else
      Result:=AParent.FindNode(CurText);
    if (Result=Nil) Then
      Result:=TVLabels.Items.AddChild(AParent,CurText);
    AParent:=Result;
  until (Result=nil) or (TextPath='');
end;

procedure TMainForm.AddLabels;

var
  EF,Entry: googlegmail.TLabel;
  Resource : TUsersLabelsResource;
  EN : String;
  List : TListLabelsResponse;
  i : integer;
  PN,N : TTreeNode;
  ShowThisLabel : boolean;

begin
  Resource:=Nil;
  List:=Nil;
  Entry:=Nil;
  try
    Resource:=FGmailAPI.CreateUsersLabelsResource(Self);
    // Search for folders of indicated folder only.
    List:=Resource.list('me');
    SaveRefreshToken;
    With TVLabels.Items do
      begin
      BeginUpdate;
      try
        I:=0;
        if Assigned(List) then
          for Entry in List.Labels do
            begin
            List.Labels[i]:=Nil;
            Inc(I);
            ShowThisLabel:=False;
            Case lowercase(Entry.labelListVisibility) of
              'labelhide' : ShowThisLabel:=False;
              'labelshow' : ShowThisLabel:=True;
              'labelshowifunread' : ShowThisLabel:=Entry.messagesUnread>0;
            end;
            if not ShowThisLabel then
              begin
              EF:=Entry;
              FreeAndNil(EF);
              end
            else
              begin
              N:=CreateNodeWithTextPath(Entry.Name);
              N.Data:=Entry;
              end;
            end;
      finally
        EndUpdate;
      end;
      end;
    Application.ProcessMessages;
  finally
    FreeAndNil(List);
    FreeAndNil(Resource);
  end;
end;

procedure TMainForm.BRefreshFoldersClick(Sender: TObject);


begin
  LoadAuthConfig;
  ClearTreeView;
  AddLabels;
  TVLabels.SortType:=stText;
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

procedure TMainForm.ClearMailListView;

Var
  I : Integer;

begin
  With LVMessages.Items do
    begin
    BeginUpdate;
    try
      For I:=0 to Count-1 do
        TObject(Item[i].Data).Free;
      Clear;
    finally
      EndUpdate;
    end;
    end;
end;

procedure TMainForm.BRefreshFilesClick(Sender: TObject);

begin
  if (TVLabels.Selected=Nil) or (TVLabels.Selected.Data=Nil) then
    ShowLabel('root')
  else
    ShowLabel(googlegmail.TLabel(TVLabels.Selected.Data).id);
end;

procedure TMainForm.ShowLabel(ALabelID: String);

Type
  TMailDescription = Record
    Subject : String;
    Sender : String;
    From : String;
    Recipient : String;
    Received : String;
    Snippet : String;
  end;

  Procedure CreateDesc(E : TMessage; var Desc : TMailDescription);

  Var
    H : TMessagePartHeader;

  begin
    Desc.Subject:='';
    Desc.Sender:='';
    Desc.Received:='';
    Desc.from:='';
    Desc.Recipient:='';
    Desc.Snippet:=E.snippet;
    If Assigned(E.payload) then
      For H in E.payload.headers do
        Case LowerCase(h.name) of
          'subject' : Desc.Subject:=H.value;
          'sender' : Desc.Sender:=H.value;
          'received' : Desc.Received:=H.Value;
          'date' : Desc.Received:=H.Value;
          'from' : Desc.from:=H.Value;
          'to' : Desc.Recipient:=H.Value;
        end;
  end;

var
  Msg,Entry: Tmessage;
  EN : String;
  i:integer;
  Q : TUsersMessagesListOptions;
  Resource : TUsersMessagesResource;
  List : TListMessagesResponse;
  LI : TListItem;
  Desc : TMailDescription;

begin
  ClearMailListView;
  Resource:=Nil;
  try
    Resource:=FGmailAPI.CreateusersMessagesResource(Self);
    // Search for files of indicated folder only.
    Q.labelIds:=ALabelID;
    List:=Resource.list('me',Q);
    SaveRefreshToken;
    With LVMessages.Items do
      begin
      BeginUpdate;
      try
        Clear;
        if Assigned(List) then
          for Msg in List.messages do
            begin
            Entry:=Resource.Get(Msg.id,'me','format=full');
            LI:=Add;
            CreateDesc(Entry,Desc);
            LI.Caption:=Desc.Subject;
            With LI.SubItems do
              begin
              Add(Desc.From);
              Add(Desc.Received);
              Add(Desc.Recipient);
              Add(Desc.Sender);
              Add(Desc.Snippet);
              end;
            Li.Data:=Entry;
            Application.ProcessMessages;
            end;
      finally
        EndUpdate;
      end;
      end;
  Finally
    Resource.Free;
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

