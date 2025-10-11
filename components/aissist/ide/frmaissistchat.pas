{ Copyright (C) 2024

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt

  Abstract: AI Assistant conversation window
}
unit FrmAIssistChat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ChatControl,
  LCLType, StdCtrls, ExtCtrls, Menus, LLM.Client;

type

  { TAIssistChatForm }

  TChatState = (csUnconfigured,csWaiting,csAIThinking);

  TAIssistChatForm = class(TForm)
    btnPrompt: TButton;
    btnConfigure: TButton;
    GBChat: TGroupBox;
    lblPrompt: TLabel;
    MICopy: TMenuItem;
    mPrompt: TMemo;
    pnlPrompt: TPanel;
    pmChat: TPopupMenu;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure HandleConfigureClick(Sender: TObject);
    procedure HandlePrompt(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure pmChatPopup(Sender: TObject);
  private
    FChat : TChatControl;
    FLLMClient : TLLMClient;
    FChatState : TChatState;
    FOnConfigure: TNotifyEvent;
    procedure CheckState;
    procedure ConfigureServer;
    procedure CreateServer;
    procedure HandlePromptResult(Sender: TObject; aResult: TSendPromptResult);
    procedure HandleRequestError(aErrorData: TLLMRestStatusInfo);
    procedure SetState(AValue: TChatState);
  public
    property State : TChatState Read FChatState Write SetState;
    property OnConfigure : TNotifyEvent Read FOnConfigure Write FOnConfigure;
  end;

implementation

uses
  ClipBrd, StrAIssist, AIssistController;

{$R *.lfm}

{ TAIssistChatForm }

procedure TAIssistChatForm.HandlePromptResult(Sender: TObject; aResult: TSendPromptResult);
begin
  FChat.LeftTyping:=False;
  State:=csWaiting;
  if not Aresult.Success then
    HandleRequestError(aResult.StatusInfo)
  else
    if Length(aResult.Value)=0 then
      FChat.AddText(SErrNoAnswer,tsLeft)
    else
      FChat.AddText(aResult.Value[0].text,tsLeft);
end;

procedure TAIssistChatForm.HandleRequestError(aErrorData: TLLMRestStatusInfo);

var
  Msg : TStrings;

begin
  Msg:=TStringList.Create;
  try
    Msg.Add(SErrorTitle);
    Msg.Add(SErrorIntro);
    Msg.Add(SErrorInfo,[aErrorData.StatusCode]);
    if aErrorData.ErrorContent<>'' then
      begin
      Msg.Add(SErrorBody);
      Msg.Add(SErrorContext,[aErrorData.ErrorContent]);
      end;
    FChat.AddText(Msg.Text,tsLeft);
    FChat.LeftTyping:=False;
    if State=csAIThinking then
      State:=csWaiting
    else
      State:=csUnconfigured;
  finally
    Msg.Free;
  end;
end;

procedure TAIssistChatForm.SetState(AValue: TChatState);
begin
  if FChatState=AValue then Exit;
  FChatState:=AValue;
  CheckState;
end;

procedure TAIssistChatForm.FormCreate(Sender: TObject);
begin
  FChat:=TChatControl.Create(Self);
  FChat.Parent:=GBChat;
  FChat.Align:=alClient;
  FChat.Width:=ClientWidth div 2;
  FChat.PopupMenu:=pmChat;
  CreateServer;
end;

procedure TAIssistChatForm.CreateServer;

begin
  FLLMClient:=AIController.CreateLLMClient;
  if Assigned(FLLMClient) then
    ConfigureServer
  else
    FChat.AddText(SErrPleaseConfigure,tsLeft);
end;

procedure TAIssistChatForm.HandleConfigureClick(Sender: TObject);
begin
  if Assigned(FOnConfigure) then
    FOnConfigure(Self);
  FreeAndNil(FLLMClient);
  State:=csUnconfigured;
  CreateServer;
end;

procedure TAIssistChatForm.CheckState;

begin
  pnlPrompt.Enabled:=(State=csWaiting);
end;

procedure TAIssistChatForm.ConfigureServer;

begin
  FLLMClient.UseThreads:=True;
  FLLMClient.SynchronizeCallBacks:=True;
  State:=csWaiting;
end;

procedure TAIssistChatForm.HandlePrompt(Sender: TObject);
var
  S : String;

begin
  Case State of
    csUnconfigured : FChat.AddText(SErrPleaseConfigure,tsLeft);
    csAIThinking : FChat.AddText(SErrAIWaiting,tsLeft);
  end;
  if State<>csWaiting then
    exit;
  S:=mPrompt.Text;
  if S='' then
    begin
    FChat.AddText(SErrPleaseEnterPrompt,tsLeft);
    exit;
    end;
  FChat.AddText(S,tsRight);
  FChat.LeftTyping:=True;
  FLLMClient.SendPrompt(S,@HandlePromptResult);
  State:=csAIThinking;
end;

procedure TAIssistChatForm.MICopyClick(Sender: TObject);

var
  lPt : TPoint;
  Item : TChatItem;

begin
  lPt:=pmChat.PopupPoint;
  lPt:=FChat.ScreenToClient(lPt);
  Item:=FChat.GetItemAt(lPt.X,lPt.Y);
  if Item<>Nil then
    Clipboard.AsText:=Item.Text;
end;

procedure TAIssistChatForm.pmChatPopup(Sender: TObject);
var
  lPt : TPoint;
  Item : TChatItem;
  HaveItem : Boolean;

begin
  lPt:=pmChat.PopupPoint;
  lPt:=FChat.ScreenToClient(lPt);
  Item:=FChat.GetItemAt(lPt.X,lPt.Y);
  HaveItem:=Item<>Nil;
  MICopy.Enabled:=HaveItem;
end;

end.

