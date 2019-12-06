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
  ComCtrls, synautil, IniFiles, googlebase, googleservice, googleclient,
  googledrive;

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
    Label1: TLabel;
    LVFiles: TListView;
    LTasks: TLabel;
    LEAccess: TLabel;
    SDDownload: TSaveDialog;
    TVFolders: TTreeView;
    procedure BCancelClick(Sender: TObject);
    procedure BRefreshFilesClick(Sender: TObject);
    procedure BSetAccessClick(Sender: TObject);
    procedure BRefreshFoldersClick(Sender: TObject);
    Procedure DoUserConsent(Const AURL : String; Out AAuthCode : String) ;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVFilesDblClick(Sender: TObject);
    procedure TVFoldersSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FAccessState : TAccessTokenState;
    FClient : TGoogleClient;
    FDriveAPI: TDriveAPI;
    procedure AddFolders(AParent: TTreeNode; AFolderID: String);
    procedure ClearFileListView;
    procedure ClearTreeView;
    procedure LoadAuthConfig;
    procedure SaveRefreshToken;
    procedure ShowFolder(AFolderID: String);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation


uses {$ifdef windows}windows,{$endif}
  ssl_openssl,
  jsonparser, // needed
  fpjson,
  fpoauth2,
  lclintf,
  fpwebclient,
  frmselectdownload,
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
  TDriveAPI.RegisterAPIResources;
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
  FDriveAPI:=TDriveAPI.Create(Self);
  FDriveAPI.GoogleClient:=FClient;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
end;

procedure TMainForm.LVFilesDblClick(Sender: TObject);

Var
  Entry : TFile;
  Request : TWebClientRequest;
  Response: TWebClientResponse;
  S,URL,LFN: String;
  D : TJSONEnum;
begin
  If Not (Assigned(LVFiles.Selected) and Assigned(LVFiles.Selected.Data)) then
    Exit;
  Entry:=TFile(LVFiles.Selected.Data);
  if (Entry.WebViewLink='') then
    Exit;
  if Entry.WebViewLink<>'' then
    URL:=TDriveAPI.APIBaseURL+'files/'+Entry.ID+'?alt=media';
  SDDownload.FileName:=Application.Location+Entry.name+'.'+Entry.fileExtension;
  If Not SDDownload.Execute then
    Exit;
  Response:=Nil;
  Request:=FClient.WebClient.CreateRequest;
  try
    Response:=FClient.WebClient.ExecuteSignedRequest('GET',URL,Request);
    With TFileStream.Create(SDDownLoad.FileName,fmCreate) do
      try
        CopyFrom(Response.Content,0);
      finally
        Free;
      end;
  finally
    Response.Free;
    Request.Free;
  end;
end;

procedure TMainForm.TVFoldersSelectionChanged(Sender: TObject);
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
  With TVFolders.Items do
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

procedure TMainForm.AddFolders(AParent : TTreeNode; AFolderID : String);

var
  Entry: TFile;
  Resource : TFilesResource;
  EN : String;
  Q : TFilesListOptions;
  List : TFileList;
  i : integer;
  N : TTreeNode;

begin
  Resource:=Nil;
  try
    Resource:=FDriveAPI.CreateFilesResource(Self);
    // Search for folders of indicated folder only.
    Q.q:='mimeType = ''application/vnd.google-apps.folder'' and '''+AFolderId+''' in parents';
    Q.corpus:='';
    q.pageSize:=12;
    Q.pageToken:='';
    List:=Resource.list(Q);
    SaveRefreshToken;
    With TVFolders.Items do
      begin
      BeginUpdate;
      try
        if Assigned(List) then
          for i:= 0 to Length(List.files)-1 do
            begin
            Entry:=List.files[i];
            List.files[i]:=Nil;
            N:=AddChild(AParent,Entry.Name);
            N.Data:=Entry;
            end;
      finally
        EndUpdate;
      end;
      end;
    Application.ProcessMessages;
    if Assigned(AParent) then
      for I:=AParent.Count-1 downto 0 do
        AddFolders(AParent.Items[i],TFile(AParent.Items[i].Data).id)
    else if (TVFolders.Items.Count>0) then
      for I:=TVFolders.Items.Count-1 downto 0  do
        AddFolders(TVFolders.Items[i],TFile(TVFolders.Items[i].Data).id)
  finally
    FreeAndNil(Resource);
  end;
end;

procedure TMainForm.BRefreshFoldersClick(Sender: TObject);


begin
  LoadAuthConfig;
  ClearTreeView;
  AddFolders(Nil,'root');
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

procedure TMainForm.ClearFileListView;

Var
  I : Integer;

begin
  With LVFiles.Items do
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
  if (TVFolders.Selected=Nil) or (TVFolders.Selected.Data=Nil) then
    ShowFolder('root')
  else
    ShowFolder(TFile(TVFolders.Selected.Data).ID);
end;

procedure TMainForm.ShowFolder(AFolderID : String);

var
  Entry: TFile;
  EN : String;
  i:integer;
  Q : TFilesListOptions;
  List : TFileList;
  Resource : TFilesResource;
  LI : TListItem;

begin
  ClearFileListView;
  Resource:=Nil;
  try
    Resource:=FDriveAPI.CreateFilesResource(Self);
    // Search for files of indicated folder only.
    Q.q:='mimeType != ''application/vnd.google-apps.folder'' and '''+AFolderId+''' in parents';
    List:=Resource.list(Q);
    SaveRefreshToken;
    With LVFiles.Items do
      begin
      BeginUpdate;
      try
        Clear;
        if Assigned(List) then
          for i:= 0 to Length(List.files)-1 do
            begin
            Entry:=List.files[i];
            List.files[i]:=Nil;
            LI:=Add;
            LI.Caption:=Entry.Name;
            With LI.SubItems do
              begin
              Add(DateTimeToStr(Entry.createdTime));
              Add(Entry.Description);
              Add(BoolToStr(Entry.capabilities.canEdit,'Yes','No'));
              Add(Entry.Size);
              Add(Entry.lastModifyingUser.displayName);
              Add(Entry.webContentLink);
              Add(Entry.version);
              Add(Entry.mimeType);
              end;
            Li.Data:=Entry;
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

