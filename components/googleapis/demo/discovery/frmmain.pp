unit frmmain;

{$mode objfpc}{$H+}

// Define this if you want to use synapse.
{ $DEFINE USESYNAPSE}

// For 2.6.4, synapse is currently the only option.
// You will need to add lazsynapsewebclient to the requires list.
{$IFDEF VER2_6}
{$DEFINE USESYNAPSE}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, Menus, ListViewFilterEdit, restbase, googleclient,
  googlediscovery, frmgenoptions, frmview;

type

  { TMainForm }

  TMainForm = class(TForm)
    APreViewRest: TAction;
    AGenCode: TAction;
    AViewHelp: TAction;
    ASaveREST: TAction;
    APreferredOnly: TAction;
    AQuit: TAction;
    AFetch: TAction;
    ActionList1: TActionList;
    EFilter: TEdit;
    ILDiscovery: TImageList;
    Label1: TLabel;
    LVServices: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MAPI: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MIQuit: TMenuItem;
    MIPreferredOnly: TMenuItem;
    MServices: TMenuItem;
    SDJSON: TSaveDialog;
    SBDiscovery: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure APreViewRestExecute(Sender: TObject);
    procedure APreViewRestUpdate(Sender: TObject);
    procedure ASaveRESTExecute(Sender: TObject);
    procedure ASaveRESTUpdate(Sender: TObject);
    procedure AGenCodeExecute(Sender: TObject);
    procedure AGenCodeUpdate(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure AViewHelpExecute(Sender: TObject);
    procedure AViewHelpUpdate(Sender: TObject);
    procedure BFetchClick(Sender: TObject);
    procedure CBPreferredOnlyChange(Sender: TObject);
    procedure EFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FClient : TGoogleClient;
    FDiscoveryAPI : TDiscoveryAPI;
    FDirectory : TDirectoryList;
    Function  CurrentAPI : TDirectoryListTypeitemsItem;
    procedure DoFetch;
    procedure DownLoadRestAPI(Const AName,AURL: String);
    procedure GenerateCode(const AName, AURL: String);
    function HttpGetBinary(AURL: String; S: TStream): Boolean;
    procedure ShowDiscovery(PreferredOnly: Boolean; FilterOn: String);
    procedure UpdateCaption;
    procedure ViewFile(AFileName: String);
    procedure ViewFile(AStream: TStream; ASyntax: TSyntax; ACaption: String);
    procedure ViewRestAPI(const AName, AURL: String);
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}
uses
  ssl_openssl,
  jsonparser, // needed
  fpoauth2, lclintf,
{$IFDEF USESYNAPSE}
  synapsewebclient,
  httpsend,
{$ELSE}
  fphttpclient,
  fphttpwebclient,
{$ENDIF}
  googlediscoverytopas;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // set up communication.
  FClient:=TGoogleClient.Create(Self);
{$IFDEF USESYNAPSE}
  FClient.WebClient:=TSynapseWebClient.Create(Self);
{$ELSE}
  FClient.WebClient:=TFPHTTPWebClient.Create(Self);
{$ENDIF}
  // Register all classes so they can be streamed.
  TDiscoveryAPI.RegisterAPIResources;
  // create the API and hook it up to the google client.
  FDiscoveryAPI:=TDiscoveryAPI.Create(Self);
  FDiscoveryAPI.GoogleClient:=FClient;
  // The code generator uses it's own objects.
  TDiscoveryJSONToPas.RegisterAllObjects;
  UpdateCaption;
end;

procedure TMainForm.BFetchClick(Sender: TObject);

begin
  DoFetch;
end;

procedure TMainForm.AQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AViewHelpExecute(Sender: TObject);
begin
  OpenURL(CurrentAPI.DocumentationLink);
end;

procedure TMainForm.AViewHelpUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentAPI) and (CurrentAPI.documentationLink<>'');
end;

procedure TMainForm.ASaveRESTUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentAPI) and (CurrentAPI.discoveryRestUrl<>'');
end;

procedure TMainForm.AGenCodeExecute(Sender: TObject);

Var
  DLI : TDirectoryListTypeitemsItem;

begin
  DLI:=CurrentAPI;
  GenerateCode(DLI.Name,DLI.DiscoveryRestUrl);
end;

procedure TMainForm.AGenCodeUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentAPI) and (CurrentAPI.discoveryRestUrl<>'')
end;

procedure TMainForm.ViewFile(AFileName : String);

begin
  With TViewForm.Create(Nil) do
    begin
    Caption:='Viewing file: '+AFileName;
    FileName:=AFileName;
    Show;
    end;
end;

procedure TMainForm.ViewFile(AStream :  TStream; ASyntax : TSyntax; ACaption : String);

begin
  With TViewForm.Create(Nil) do
    begin
    AStream.POsition:=0;
    Caption:='Viewing: '+ACaption;
    Stream:=AStream;
    Syntax:=ASyntax;
    FreeStream:=True;
    Show;
    end;
end;

Function TMainForm.HttpGetBinary(AURL : String; S : TStream) : Boolean;

begin
{$IFDEF USESYNAPSE}
   Result:=httpsend.HttpGetBinary(AURL,S);
{$ELSE}
  try
    TFPHTTPClient.SimpleGet(AURL,S);
    S.Position:=0;
    Result:=True;
  except
    Result:=False;
  end;
{$ENDIF}
end;

procedure TMainForm.GenerateCode(const AName, AURL: String);

Var
  S : TMemoryStream;
  FO : TGenCodeFormOptions;
  DP : TDiscoveryJSONToPas;

begin
  FO:=Nil;
  DP:=Nil;
  S:=TMemoryStream.Create;
  try
    if HttpGetBinary(AURL,S) then
      begin
      S.Position:=0;
      FO:=TGenCodeFormOptions.Create(Self);
      FO.UnitName:=AName;
      If FO.ShowModal=mrOK then
        begin
        DP:=TDiscoveryJSONToPas.Create(Self);
        DP.LoadFromStream(S);
        DP.BaseClassName:=FO.BaseClass;
        DP.OutputUnitName:=FO.UnitName;
        DP.ExtraUnits:=FO.ExtraUnits;
        DP.ClassPrefix:=FO.Prefix;
        DP.SaveToFile(FO.FileName);
        if FO.DoPreview then
          ViewFile(FO.FileName);
        end;
      end;
  Finally
    FO.Free;
    DP.Free;
    S.Free;
  end;
end;

procedure TMainForm.DownLoadRestAPI(const AName, AURL: String);

Var
  S : TMemoryStream;

begin
  S:=TMemoryStream.Create;
  try
    if HttpGetBinary(AURL,S) then
      begin
      SDJSON.FileName:='google'+aname+'.json';
      If SDJSON.Execute then
        With TFileStream.Create(SDJSON.FileName,fmCreate) do
          try
            CopyFrom(S,0);
          finally
            Free;
          end;
      end;
  finally
    S.Free;
  end;
end;

procedure TMainForm.ViewRestAPI(const AName, AURL: String);
Var
  S : TMemoryStream;

begin
  S:=TMemoryStream.Create;
  try
    if HttpGetBinary(AURL,S) then
      begin
      ViewFile(S,sJSON,'REST discovery for '+AName);
      S:=Nil;
      end;
  finally
    S.Free;
  end;
end;

procedure TMainForm.ASaveRESTExecute(Sender: TObject);

Var
  DLI : TDirectoryListTypeitemsItem;
begin
  DLI:=CurrentAPI;
  DownLoadRestAPI(DLI.Name,DLI.DiscoveryRestUrl);
end;

procedure TMainForm.APreViewRestUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentAPI) and (CurrentAPI.discoveryRestUrl<>'');
end;

procedure TMainForm.APreViewRestExecute(Sender: TObject);

Var
  DLI : TDirectoryListTypeitemsItem;

begin
  DLI:=CurrentAPI;
  ViewRestAPI(DLI.Name,DLI.DiscoveryRestUrl);
end;

procedure TMainForm.CBPreferredOnlyChange(Sender: TObject);
begin
  if (LVServices.Items.Count>0) then
    ShowDiscovery(MIPreferredOnly.Checked,EFilter.Text);
end;

procedure TMainForm.EFilterChange(Sender: TObject);
begin
  if (LVServices.Items.Count>0) then
    ShowDiscovery(MIPreferredOnly.Checked,EFilter.Text);
end;

procedure TMainForm.UpdateCaption;

Var
  C : Integer;

begin
  C:=LVServices.Items.Count;
  if (C=0) then
    Caption:='Google Discovery Service Demo'
  else
    Caption:=Format('Google Discovery Service Demo (%d services)',[C]);
  SBDiscovery.Panels[0].Text:=Format('%d items',[C]);
end;

function TMainForm.CurrentAPI: TDirectoryListTypeitemsItem;
begin
  If Assigned(LVServices.Selected) and Assigned(LVServices.Selected.Data) then
    Result:=TDirectoryListTypeitemsItem(LVServices.Selected.Data)
  else
    Result:=Nil;
end;

procedure TMainForm.ShowDiscovery(PreferredOnly : Boolean; FilterOn : String);

  Function DoComma(S : TStringArray) : String;
  Var
    I : String;
  begin
    Result:='';
    For I in S do
      begin
      if Result<>'' then
        Result:=Result+',';
      Result:=Result+I;
      end;
  end;

  Function Contains(S: String) : Boolean; inline;
  begin
    Result:=Pos(FilterOn,LowerCase(S))<>0;
  end;

  Function ShowItem (DLI : TDirectoryListTypeitemsItem) : Boolean;

  begin
    Result:=DLI.Preferred or (Not PreferredOnly);
    if Result and (FilterOn<>'') then
      begin
      Result:=Contains(DLI.Name)
              or Contains(DLI.Title)
              or Contains(DLI.kind)
              or Contains(DLI.description)
              or Contains(DoComma(DLI.Labels));
      end;
  end;
Var
  DLI : TDirectoryListTypeitemsItem;
  LI : TListItem;

begin
  FilterOn:=LowerCase(Filteron);
  LVServices.Items.BeginUpdate;
  try
    LVServices.Items.Clear;
    LVServices.Column[1].Visible:=Not PreferredOnly;
    For DLI in FDirectory.Items do
      if ShowItem(DLI) then
        begin
        LI:=LVServices.Items.Add;
        LI.Caption:=DLI.name;
        LI.Data:=DLI;
        With LI.SubItems,DLI do
          begin
          Add(BoolToStr(preferred,'True','False'));
          Add(id);
          Add(title);
          Add(version);
          Add(description);
          Add(discoveryLink);
          Add(discoveryRestUrl);
          Add(documentationLink);
          Add(icons.x16);
          Add(icons.x32);
          Add(DoComma(labels));
          end;
        end;
    UpdateCaption;
  finally
    LVServices.Items.EndUpdate;
  end;
end;

procedure TMainForm.DoFetch;

begin
  // Free any previous list.
  FreeAndNil(FDirectory);
  // Get the new list using a default ApisResource.
  FDirectory:=FDiscoveryAPI.ApisResource.List();
  ShowDiscovery(MIPreferredOnly.Checked,EFilter.Text);
end;

end.

