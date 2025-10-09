program client_demo;

uses classes, custapp, fphttpwebclient, opensslsockets, jsonparser, llm.ollama, llm.client;

type

  { TLLMClientApp }

  TLLMClientApp = class(TCustomApplication)
  private
    FClient: TLLMClient;
    FModel : string;
    procedure HandleModels(Sender: TObject; aResult: TGetModelsResult);
    procedure HandlePrompt(Sender: TObject; aResult: TSendPromptResult);
    procedure WriteError(aMsg: String; aInfo: TLLMRestStatusInfo);
  Public
    Constructor create(aOwner : TComponent); override;
    procedure ShowModels;
    procedure DoPrompt;
    procedure DoRun; override;
  end;

{ TLLMClientApp }

procedure TLLMClientApp.WriteError(aMsg : String; aInfo : TLLMRestStatusInfo);

begin
  Writeln(StdErr,aMsg,' (',aInfo.Status,' ',aInfo.StatusCode,'): ',aInfo.ErrorContent);
end;

procedure TLLMClientApp.HandleModels(Sender: TObject; aResult: TGetModelsResult);
var
  lModel: TModelData;
  i : integer;
begin
  if not aResult.Success then
    WriteError('Failed to get models',aResult.StatusInfo)
  else
    begin
    i:=0;
    For lModel in aResult.Value do
      begin
      Inc(i);
      if FModel='' then
        FModel:=lModel.ID;
      Writeln('Model ',i:2,': ',lModel.ID,' [',lModel.Name,']');
      end;
    DoPrompt;
    end;
end;

procedure TLLMClientApp.HandlePrompt(Sender: TObject; aResult: TSendPromptResult);
var
  lResponse: TPromptResponse;
  i : integer;
begin
  if not aResult.Success then
    WriteError('Failed to get prompt result',aResult.StatusInfo)
  else
    begin
    i:=0;
    if Length(aResult.Value)=1 then
      Writeln('Response: ',aResult.Value[0].Text)
    else
    For lResponse in aResult.Value do
      begin
      Inc(i);
      Writeln('Response ',i:2,': ',lResponse.Text);
      end;
    end;
end;

constructor TLLMClientApp.create(aOwner: TComponent);
begin
  Inherited;
  FClient:=TLLMClient.Create(Self);
  FClient.OnGetModelsResult:=@HandleModels;
  FClient.OnPromptResult:=@HandlePrompt;
end;

procedure TLLMClientApp.ShowModels;
begin
  FClient.GetModels();
end;

procedure TLLMClientApp.DoPrompt;
begin
  FClient.SetModel(FModel);
  FClient.SendPrompt('What is the capital of France ?',100);
end;

procedure TLLMClientApp.DoRun;
var
  lProtocol : string;
begin
  Terminate;
  FModel:=GetOptionValue('m','model');
  lProtocol:=GetOptionValue('p','protocol');
  if lProtocol='' then
    lProtocol:='ollama';
  FClient.Settings.Protocol:=lProtocol;
  ShowModels;
end;

var
  Application : TLLMClientApp;

begin
  Application:=TLLMClientApp.Create(Nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

