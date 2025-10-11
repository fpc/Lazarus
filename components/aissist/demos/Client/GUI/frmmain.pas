unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LLM.Client,
  // Add all units so we compile in support for all supported backends
  llm.claude, llm.chatgpt, llm.gemini, llm.perplexity, llm.ollama, 
  opensslsockets, jsonparser, fpwebclient, fphttpwebclient ;

type

  { TMainForm }

  TMainForm = class(TForm)
    btnGetModels: TButton;
    btnPrompt: TButton;
    cbModels: TComboBox;
    Label1: TLabel;
    lblModels: TLabel;
    lblPrompt: TLabel;
    llm: TLLMClient;
    mPrompt: TMemo;
    mAnswer: TMemo;
    procedure btnGetModelsClick(Sender: TObject);
    procedure btnPromptClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure llmGetModelsResult(Sender: TObject; aResult: TGetModelsResult);
    procedure llmPromptResult(Sender: TObject; aResult: TSendPromptResult);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnPromptClick(Sender: TObject);
begin
  if cbModels.Text='' then
    begin
    ShowMessage('Please select a model');
    Exit;
    end;
  if Trim(mPrompt.Text)='' then
    begin
    ShowMessage('Please enter a prompt');
    Exit;
    end;
  LLM.SetModel(cbModels.Text);
  mAnswer.Clear;
  // In a real-world program you would do this in a thread
  LLM.SendPrompt(mPrompt.Text);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  mPrompt.Text:='';
  mAnswer.Text:='';
  LLM.UseThreads:=True;
  llm.SynchronizeCallBacks:=True;
end;

procedure TMainForm.llmGetModelsResult(Sender: TObject; aResult: TGetModelsResult);
var
  lMOdel : TModelData;
begin
  if not aResult.Success then
    begin
    With aResult.StatusInfo do
      ShowMessage(Format('Failed to get models : %d %s',[Status,StatusCode]));
    exit;
    end;
  cbModels.Clear;
  cbModels.Items.Clear;
  for lModel in aResult.Value do
    cbModels.Items.Add(lModel.ID);
  if cbModels.Items.Count>0 then
    cbModels.ItemIndex:=0;
end;

procedure TMainForm.llmPromptResult(Sender: TObject; aResult: TSendPromptResult);
begin
  if not aResult.Success then
    begin
    With aResult.StatusInfo do
      ShowMessage(Format('Failed to send prompt : %d %s',[Status,StatusCode]));
    exit;
    end;
  if Length(aResult.Value)=0 then
    begin
    ShowMessage('Got no reply from model');
    exit;
    end;
  mAnswer.Text:=aResult.Value[0].Text;
end;

procedure TMainForm.btnGetModelsClick(Sender: TObject);
begin
  llm.GetModels;
end;

initialization
  DefaultWebClientClass:=TFPHTTPWebClient;
end.

