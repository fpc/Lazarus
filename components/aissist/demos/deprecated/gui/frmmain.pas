unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, IniPropStorage, chatcontrol, aiclient,
  janaiprotocol;

type
  TChatState = (csDisconnected,csConnected,csWaiting,csAIThinking);
  { TMainChatForm }

  TMainChatForm = class(TForm)
    Button1: TButton;
    btnConnect: TButton;
    btnPrompt: TButton;
    cbModels: TComboBox;
    edtURL: TEdit;
    GBChat: TGroupBox;
    psAI: TIniPropStorage;
    Label1: TLabel;
    lblURL: TLabel;
    lblModel: TLabel;
    mPrompt: TMemo;
    Panel1: TPanel;
    pnlPrompt: TPanel;
    Splitter1: TSplitter;
    procedure cbModelsChange(Sender: TObject);
    procedure edtURLChange(Sender: TObject);
    procedure handleConnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure handlePrompt(Sender: TObject);
    procedure psAIRestoreProperties(Sender: TObject);
    procedure psAISaveProperties(Sender: TObject);
  private
    FChat : TChatControl;
    FAIClient : TAIClient;
    FChatState : TChatState;
    procedure HandleAIAnswer(Sender: TObject; aResponses: TPromptResponseArray);
    procedure HandleRequestError(Sender: TObject; aErrorData: TAIRequestErrorData);
    procedure SetState(aState : TChatState);
    Procedure CheckState;
    function ExtractModelID(S: String): string;
    procedure HandleGetModels(Sender: TObject; aModels: TModelDataArray);
    property State : TChatState Read FChatState Write SetState;
  public

  end;

var
  MainChatForm: TMainChatForm;

implementation

{$R *.lfm}

{ TMainChatForm }

procedure TMainChatForm.FormCreate(Sender: TObject);
begin
  FChat:=TChatControl.Create(Self);
  FChat.Parent:=GBChat;
  FChat.Align:=alClient;
  FChat.Width:=ClientWidth div 2;
  FAIClient:=TAIClient.Create(Self);
  FAIClient.Settings.Protocol:=TJanAIServerProtocol.protocolname;
  FAIClient.OnError:=@HandleRequestError;
  FAIClient.SynchronizeCallBacks:=True;
  FAIClient.Settings.DefaultMaxLength:=2048;
  psAI.IniFileName:=GetAppConfigFile(False);
  psAI.Active:=True;
  mPrompt.Text:=FAIClient.Settings.DefaultModel;
  SetState(csDisconnected);
end;

procedure TMainChatForm.handlePrompt(Sender: TObject);

var
  S : String;

begin
  if State<>csWaiting then exit;
  S:=mPrompt.Text;
  FChat.AddText(S,tsRight);
  FChat.LeftTyping:=True;
  FAIClient.SendPrompt(@HandleAIAnswer,mPrompt.Text);
  State:=csAIThinking;
end;

procedure TMainChatForm.psAIRestoreProperties(Sender: TObject);
begin
  FAIClient.Settings.BaseURL:=psAI.ReadString('URL','');
  FAIClient.Settings.DefaultModel:=psAI.ReadString('model','');
  edtURL.Text:=FAIClient.Settings.BaseURL;
end;

procedure TMainChatForm.psAISaveProperties(Sender: TObject);
begin
  psAI.WriteString('URL',FAIClient.Settings.BaseURL);
  psAI.WriteString('model',FAIClient.Settings.DefaultModel);
end;

procedure TMainChatForm.SetState(aState: TChatState);
begin
  if aState=FChatState then exit;
  FChatState:=aState;
  CheckState;
end;

procedure TMainChatForm.HandleRequestError(Sender: TObject; aErrorData: TAIRequestErrorData);

var
  Msg : TStrings;

begin
  Msg:=TStringList.Create;
  try
    Msg.Add('Drat!');
    Msg.Add('An error occurred while talking to the AI!');
    Msg.Add('Here is the error we got: '+aErrorData.Error);
    Msg.Add('This is what we were trying to do: '+aErrorData.Method+' '+aErrorData.URL);
    if aErrorData.RequestBody<>'' then
      begin
      Msg.Add('And this is what we were saying:');
      Msg.Add(aErrorData.RequestBody);
      end;
    FChat.AddText(Msg.Text,tsLeft);
    FChat.LeftTyping:=False;
    if State=csAIThinking then
      State:=csWaiting
    else
      State:=csDisconnected;
  finally
    Msg.Free;
  end;
end;

procedure TMainChatForm.HandleAIAnswer(Sender: TObject; aResponses: TPromptResponseArray);
begin
  FChat.LeftTyping:=False;
  State:=csWaiting;
  if Length(aResponses)=0 then
    FChat.AddText('No answer from AI, try refining your prompt',tsLeft)
  else
    FChat.AddText(aResponses[0].Response,tsLeft);
end;

procedure TMainChatForm.CheckState;
begin
  pnlPrompt.Enabled:=(State=csWaiting);
  if State<csConnected then
    begin
    cbModels.Clear;
    cbModels.Items.Clear;
    end;
end;

procedure TMainChatForm.HandleGetModels(Sender: TObject; aModels: TModelDataArray);

var
  aModel : TModelData;
  Idx,I : Integer;
begin
  Idx:=-1;
  With cbModels.Items do
    begin
    BeginUpdate;
    For aModel in aModels do
      begin
      I:=Add('['+aModel.ID+'] '+aModel.Name);
      if aModel.ID=FAIClient.Settings.DefaultModel then
        Idx:=I;
      end;
    EndUpdate;
    end;
  cbModels.ItemIndex:=Idx;
  FChat.LeftTyping:=False;
  if Idx>=0 then
    State:=csWaiting
  else
    State:=csConnected;
end;

procedure TMainChatForm.handleConnect(Sender: TObject);
begin
  FAIClient.Settings.BaseURL:=edtURL.Text;
  FAIClient.GetModels(@HandleGetModels);
  FChat.LeftTyping:=True;
end;

procedure TMainChatForm.edtURLChange(Sender: TObject);
begin
  State:=csDisconnected;
end;

function TMainChatForm.ExtractModelID(S : String) : string;

var
  P1,P2 : Integer;

begin
  P1:=Pos('[',S);
  P2:=Pos(']',S);
  if (P1>0) and (P2>P1) then
    Result:=Copy(S,P1+1,P2-P1-1)
  else
    Result:=S;
end;

procedure TMainChatForm.cbModelsChange(Sender: TObject);

var
  mID : String;

begin
  mID:=ExtractModelID(cbModels.Text);
  FAIClient.Settings.DefaultModel:=mID;
  if mID<>'' then
    State:=csWaiting;
end;

end.

