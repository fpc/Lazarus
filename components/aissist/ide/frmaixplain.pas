unit FrmAixplain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ButtonPanel,
  ComCtrls, Buttons,
  typingindicator, SrcEditorIntf, LLM.Client;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); virtual;
    procedure FormCreate(Sender: TObject); virtual;
    procedure pnlThinkingClick(Sender: TObject); virtual;
    procedure SBRefreshClick(Sender: TObject); virtual;
  protected
    FLLMClient: TLLMClient;
    FBusy : Boolean;
    FEditor: TSourceEditorInterface;
    FTyping : TTypingIndicator;
    procedure ActivateResponse; virtual;
    procedure CreatePrompt; virtual;
    function GetPrompt: string; virtual;
    procedure HandlePromptResult(Sender: TObject; aResult: TSendPromptResult);
    procedure SendPrompt; virtual;
  public
    procedure Explain(aEditor: TSourceEditorInterface; aLLMClient: TLLMClient); virtual;
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

procedure TAIxplainForm.HandlePromptResult(Sender: TObject; aResult: TSendPromptResult);

var
  S : TStrings;

begin
  FBusy:=False;
  ActivateResponse;
  if (not aResult.Success) or (Length(aResult.Value)=0) then
    begin
    mExplain.Lines.Add(SNoExplanation);
    end
  else
    begin
    mExplain.Lines.Add(SAIExplanation);
    S:=TStringList.Create;
    try
      S.Text:=aResult.Value[0].Text;
      mExplain.Lines.AddStrings(S);
    finally
      S.Free;
    end;
    end;
end;

procedure TAIxplainForm.Explain(aEditor: TSourceEditorInterface; aLLMClient: TLLMClient);
begin
  FEditor:=aEditor;
  FLLMClient:=aLLMClient;
  FLLMClient.OnPromptResult:=@HandlePromptResult;
  FLLMClient.UseThreads:=True;
  FLLMClient.SynchronizeCallBacks:=True;
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
  FLLMClient.SendPrompt(GetPrompt,@HandlePromptResult);
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
    S:=FEditor.GetText(True);
    if S='' then
      S:=FEditor.GetText(False);
    Src.Text:=S;
    MPrompt.Lines.Add(SExplainPrompt);
    MPrompt.Lines.Add('');
    MPrompt.Lines.AddStrings(Src);
  finally
    Src.Free;
  end;
end;

end.

