program testaissist;

uses cthreads, sysutils, classes, custapp, aiclient, fpwebclient, jsonparser, janaiprotocol;

Type

  { TApp }

  { TMyApp }

  TMyApp = Class(TCustomApplication)
    FClient : TAIClient;
    constructor create(aOwner : TComponent); override;
    Procedure Run;
  private
    procedure DoError(Sender: TObject; aErrorData : TAIRequestErrorData);
    procedure HandleModels(Sender: TObject; aModels: TModelDataArray);
    procedure HandlePrompt(Sender: TObject; aResponses: TPromptResponseArray);
  end;

{ TMyApp }

constructor TMyApp.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FClient:=TAIClient.Create(Self);
  FClient.Settings.BaseURL:='http://localhost:1337/v1';
  FClient.Settings.DefaultModel:='mistral-ins-7b-q4';
  FClient.Settings.DefaultMaxLength:=2048;
  FClient.Settings.Protocol:=TJANAIServerProtocol.protocolname;
  FClient.OnError:=@DoError;
end;

procedure TMyApp.Run;

begin
  FClient.GetModels(@HandleModels);
  While not Terminated do
    begin
    CheckSynchronize;
    Sleep(100);
    end;
end;

procedure TMyApp.DoError(Sender: TObject; aErrorData : TAIRequestErrorData);
begin
  With aErrorData do
    begin
    Writeln('Got error ',ErrorClass,' during AI request : ',Error);
    Writeln('Request details: ',METHOD,' ',URL);
    if RequestBody<>'' then
      begin
      Writeln('Request body:');
      Writeln(RequestBody);
      end;
    end;
  Terminate;
end;

procedure TMyApp.HandleModels(Sender: TObject; aModels: TModelDataArray);

var
  Model : TModelData;

begin
  Writeln('Received model list (',Length(aModels),' entries) : ');
  For Model in aModels do
    Writeln(Model.id,' : ',Model.Name);
  Writeln('Asking for hello world program...');
  FClient.SendPrompt(@HandlePrompt,'Please create a "hello, world!" program in pascal');
  Writeln('The AI is thinking. Waiting for reply...');
end;

procedure TMyApp.HandlePrompt(Sender: TObject; aResponses: TPromptResponseArray);

var
  Resp : TPromptResponse;

begin
  Writeln('Got ',Length(aResponses),' replies: ');
  For Resp in aResponses do
    Writeln(Resp.Response);
  Terminate;
end;

begin
  With TMyApp.Create(nil) do
    try
      Run;
    finally
      Free;
    end;
end.

