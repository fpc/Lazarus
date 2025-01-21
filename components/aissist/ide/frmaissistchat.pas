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
  LCLType, StdCtrls, ExtCtrls, Menus, AIClient;

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
    FAIClient : TAIClient;
    FChatState : TChatState;
    FOnConfigure: TNotifyEvent;
    procedure CheckState;
    procedure ConfigureServer;
    procedure CreateServer;
    procedure HandleAIAnswer(Sender: TObject; aResponses: TPromptResponseArray);
    procedure HandleRequestError(Sender: TObject; aErrorData: TAIRequestErrorData);
    procedure SetState(AValue: TChatState);
  public
    { public declarations }
    property State : TChatState Read FChatState Write SetState;
    property OnConfigure : TNotifyEvent Read FOnConfigure Write FOnConfigure;
  end;



implementation

uses ClipBrd,StrAIssist, AIssistController;

{$R *.lfm}

{ TAIssistChatForm }

procedure TAIssistChatForm.HandleAIAnswer(Sender: TObject; aResponses: TPromptResponseArray);
begin
  FChat.LeftTyping:=False;
  State:=csWaiting;
  if Length(aResponses)=0 then
    FChat.AddText(SErrNoAnswer,tsLeft)
  else
    FChat.AddText(aResponses[0].Response,tsLeft);
end;

procedure TAIssistChatForm.HandleRequestError(Sender: TObject; aErrorData: TAIRequestErrorData);

var
  Msg : TStrings;

begin
  Msg:=TStringList.Create;
  try
    Msg.Add(SErrorTitle);
    Msg.Add(SErrorIntro);
    Msg.Add(SErrorInfo,[aErrorData.Error]);
    Msg.Add(SErrorContext,[aErrorData.Method,aErrorData.URL]);
    if aErrorData.RequestBody<>'' then
      begin
      Msg.Add(SErrorBody);
      Msg.Add(aErrorData.RequestBody);
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
  FAIClient:=AIController.CreateAIClient;
  if Assigned(FAIClient) then
    ConfigureServer
  else
    FChat.AddText(SErrPleaseConfigure,tsLeft);
end;

procedure TAIssistChatForm.HandleConfigureClick(Sender: TObject);
begin
  if Assigned(FOnConfigure) then
    FOnConfigure(Self);
  FreeAndNil(FAIClient);
  State:=csUnconfigured;
  CreateServer;
end;

procedure TAIssistChatForm.CheckState;

begin
  pnlPrompt.Enabled:=(State=csWaiting);
end;

procedure TAIssistChatForm.ConfigureServer;

begin
  FAIClient.OnError:=@HandleRequestError;
  FAIClient.SynchronizeCallBacks:=True;
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
  FAIClient.SendPrompt(@HandleAIAnswer,S);
  State:=csAIThinking;
end;

procedure TAIssistChatForm.MICopyClick(Sender: TObject);

var
  lPt : TPoint;
  Item : TChatItem;

begin
  lPt:=pmChat.PopupPoint;
  lpt:=FChat.ScreenToClient(lpt);
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
  lpt:=FChat.ScreenToClient(lpt);
  Item:=FChat.GetItemAt(lPt.X,lPt.Y);
  HaveItem:=Item<>Nil;
  MICopy.Enabled:=HaveItem;
end;

end.

