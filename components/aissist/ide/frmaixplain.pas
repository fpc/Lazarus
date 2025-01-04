unit frmaixplain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ButtonPanel, ComCtrls, Buttons,
  typingindicator, SrcEditorIntf, AIClient;

type

  { TAIxplainForm }

  TAIxplainForm = class(TForm)
    bpExplain: TButtonPanel;
    mExplain: TMemo;
    mPrompt: TMemo;
    nbExplain: TNotebook;
    PPrompt: TPage;
    PReply: TPage;
    pnlThinking: TPanel;
    SBRefresh: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure pnlThinkingClick(Sender: TObject);
    procedure SBRefreshClick(Sender: TObject);
  private
    FTyping : TTypingIndicator;
    FEditor: TSourceEditorInterface;
    FAIClient: TAIClient;
    FBusy : Boolean;
    procedure ActivateResponse;
    procedure CreatePrompt;
    function GetPrompt: string;
    procedure HandleAIError(Sender: TObject; aErrorData: TAIRequestErrorData);
    procedure HandleAIResponse(Sender: TObject; aResponses: TPromptResponseArray);
    procedure SendPrompt;
  public
    procedure Explain(aEditor: TSourceEditorInterface; aAIClient: TAIClient);
  end;

var
  AIxplainForm: TAIxplainForm;

implementation

uses StrAIssist;

{$R *.lfm}

const
  piPrompt = 0;
  piReply  = 1;

{ TAIxplainForm }

procedure TAIxplainForm.FormCreate(Sender: TObject);
begin
  FTyping:=TTypingIndicator.Create(Self);
  FTyping.Parent:=PReply;
  FTyping.Visible:=False;
  FTyping.Width:=80;
  FTyping.Height:=40;
  FTyping.Top:=mExplain.Top+24;
  FTyping.Left:=mExplain.left+24;
  pnlThinking.Visible:=False;
  nbExplain.PageIndex:=piReply;
  sbRefresh.Caption:=EditPromptCaption;
end;

procedure TAIxplainForm.pnlThinkingClick(Sender: TObject);
begin

end;

procedure TAIxplainForm.SBRefreshClick(Sender: TObject);
begin
  if nbExplain.PageIndex=piReply then
    begin
    nbExplain.PageIndex:=piPrompt;
    sbRefresh.Caption:=SendPromptCaption;
    end
  else if nbExplain.PageIndex=piPrompt then
    begin
    nbExplain.PageIndex:=piReply;
    SendPrompt;
    sbRefresh.Caption:=EditPromptCaption;
    end;
end;

procedure TAIxplainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TAIxplainForm.ActivateResponse;

begin
  FTyping.Visible:=False;
  nbExplain.PageIndex:=piReply;
  mExplain.Clear;
  mExplain.Visible:=True;
  SBRefresh.Visible:=True;
  pnlThinking.Visible:=False;
  sbRefresh.Caption:=EditPromptCaption;
end;

procedure TAIxplainForm.HandleAIResponse(Sender: TObject; aResponses: TPromptResponseArray);

var
  S : TStrings;

begin
  FBusy:=False;
  ActivateResponse;
  if (Length(AResponses)=0) then
    begin
    mExplain.Lines.Add(SNoExplanation);
    end
  else
    begin
    mExplain.Lines.Add(SAIExplanation);
    S:=TStringList.Create;
    try
      S.Text:=aResponses[0].Response;
      mExplain.Lines.AddStrings(S);
    finally
      S.Free;
    end;
    end;
end;

procedure TAIxplainForm.HandleAIError(Sender: TObject; aErrorData: TAIRequestErrorData);
begin
  ActivateResponse;
  FBusy:=False;
  mExplain.Lines.Add(SErrorTitle);
  mExplain.Lines.Add(SErrorIntro);
  mExplain.Lines.Add(SErrorInfo,[aErrorData.Error]);
  mExplain.Lines.Add(SErrorContext,[aErrorData.Method,aErrorData.URL]);
  // Body ?
end;

procedure TAIxplainForm.Explain(aEditor: TSourceEditorInterface; aAIClient: TAIClient);
begin
  FEditor:=aEditor;
  FAIClient:=aAIClient;
  FAIClient.OnError:=@HandleAIError;
  FAIClient.SynchronizeCallBacks:=True;
  CreatePrompt;
  SendPrompt;
end;

function TAIxplainForm.GetPrompt : string;

begin
  Result:=mPrompt.Text;
end;

procedure TAIxplainForm.SendPrompt;

begin
  if FBusy then
    exit;
  FBusy:=True;
  FAIClient.SendPrompt(@HandleAIResponse,GetPrompt);
  mExplain.Clear;
  mExplain.Visible:=False;
  SBRefresh.Visible:=False;
  nbExplain.PageIndex:=piReply;
  pnlThinking.Visible:=True;
  FTyping.Visible:=True;
end;

procedure TAIxplainForm.CreatePrompt;

var
  S : String;
  Src : TStrings;

begin
  Src:=TStringList.Create;
  try
    S:=Feditor.GetText(True);
    if S='' then
      S:=Feditor.GetText(False);
    Src.Text:=S;
    MPrompt.Lines.Add(SExplainPrompt);
    MPrompt.Lines.Add('');
    MPrompt.Lines.AddStrings(Src);
  finally
    Src.Free;
  end;
end;

end.

