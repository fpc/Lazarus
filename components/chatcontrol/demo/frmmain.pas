unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, IniPropStorage, Menus, chatcontrol, typingindicator;

type
  { TMainChatForm }

  TMainChatForm = class(TForm)
    Button1: TButton;
    btnAdd: TButton;
    Button2: TButton;
    cbLeft: TCheckBox;
    cbCtrlSelects: TCheckBox;
    ccMain: TChatControl;
    GBChat: TGroupBox;
    MIDelete: TMenuItem;
    MICopy: TMenuItem;
    pmChat: TPopupMenu;
    psChat: TIniPropStorage;
    lblText: TLabel;
    mPrompt: TMemo;
    pnlPrompt: TPanel;
    Splitter1: TSplitter;
    tmrTyping: TTimer;
    procedure Button2Click(Sender: TObject);
    procedure cbCtrlSelectsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure handlePrompt(Sender: TObject);
    procedure MICopyClick(Sender: TObject);
    procedure MIDeleteClick(Sender: TObject);
    procedure mPromptChange(Sender: TObject);
    procedure pmChatPopup(Sender: TObject);
    procedure psChatRestoreProperties(Sender: TObject);
    procedure psChatSaveProperties(Sender: TObject);
    procedure tmrTypingTimer(Sender: TObject);
    procedure ChatMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoOnItemClick(Sender: TObject; aItem: TChatItem);
  private

  end;

var
  MainChatForm: TMainChatForm;

implementation

uses clipbrd;

{$R *.lfm}

{ TMainChatForm }

procedure TMainChatForm.FormCreate(Sender: TObject);
begin
  ccMain.Width:=ClientWidth div 2;
  ccMain.PopupMenu:=pmChat;
  ccMain.LeftTypingIndicator.DotRadius:=4;
  ccMain.RightTypingIndicator.DotRadius:=4;
  psChat.IniFileName:=GetAppConfigFile(False);
  psChat.Active:=True;
end;

procedure TMainChatForm.ChatMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  mPrompt.Text:=Format('Click at (%d,%d)',[X,Y]);
end;

procedure TMainChatForm.DoOnItemClick(Sender: TObject; aItem: TChatItem);
begin
  ShowMessage('You clicked on text:'+sLineBreak+aItem.Text);
end;

procedure TMainChatForm.Button2Click(Sender: TObject);
begin
  With ccMain do
    begin
    AddText('Hello, how are you?',tsRight);
    AddText('Fine, thanks for asking. How are you?'+sLineBreak
             +sLineBreak
             +'* Bullet **1**'+sLineBreak
             +'* Bullet **2**'+sLineBreak
             +'* Bullet **3**',
             tsLeft,True);
    AddText('Also fine. Can I ask you a question?',tsRight);
    AddText('Sure, go right ahead...',tsLeft);
    AddText('How does this program actually work?',tsRight);
    AddText('Or is that *too difficult* to explain?',tsRight,True);
    AddText('No, it is actually **quite easy** to make using lazarus:',tsLeft,True);
    AddText('There is a control that makes this a breeze...',tsLeft);
    end;
end;

procedure TMainChatForm.cbCtrlSelectsChange(Sender: TObject);
begin
  ccMain.CtrlSelects:=cbCtrlSelects.Checked;
end;

procedure TMainChatForm.handlePrompt(Sender: TObject);

var
  S : String;

begin
  S:=mPrompt.Text;
  if CBLeft.Checked then
    begin
    ccMain.AddText(S,tsLeft);
    ccMain.LeftTyping:=False;
    end
  else
    begin
    ccMain.AddText(S,tsRight);
    ccMain.RightTyping:=False;
    end;
end;

procedure TMainChatForm.MICopyClick(Sender: TObject);
var
  lPt : TPoint;
  Item : TChatItem;
  HaveItem : Boolean;

begin
  lPt:=pmChat.PopupPoint;
  lpt:=ccMain.ScreenToClient(lpt);
  Item:=ccMain.GetItemAt(lPt.X,lPt.Y);
  HaveItem:=Item<>Nil;
  if HaveItem then
    Clipboard.AsText:=Item.Text;
end;

procedure TMainChatForm.MIDeleteClick(Sender: TObject);
var
  lPt : TPoint;
  Item : TChatItem;
begin
  lPt:=pmChat.PopupPoint;
  lpt:=ccMain.ScreenToClient(lpt);
  Item:=ccMain.GetItemAt(lPt.X,lPt.Y);
  if Item<>Nil then
    ccMain.DeleteItem(Item);
end;

procedure TMainChatForm.mPromptChange(Sender: TObject);
begin
  if cbLeft.Checked then
    ccMain.LeftTyping:=True
  else
    ccMain.RightTyping:=True;
  tmrTyping.Enabled:=False;
  tmrTyping.Enabled:=True;
end;

procedure TMainChatForm.pmChatPopup(Sender: TObject);

var
  lPt : TPoint;
  Item : TChatItem;
  HaveItem : Boolean;

begin
  lPt:=pmChat.PopupPoint;
  lpt:=ccMain.ScreenToClient(lpt);
  Item:=ccMain.GetItemAt(lPt.X,lPt.Y);
  HaveItem:=Item<>Nil;
  MICopy.Enabled:=HaveItem;
  MIDelete.Enabled:=HaveItem;
end;

procedure TMainChatForm.psChatRestoreProperties(Sender: TObject);
begin
end;

procedure TMainChatForm.psChatSaveProperties(Sender: TObject);
begin
end;

procedure TMainChatForm.tmrTypingTimer(Sender: TObject);
begin
  ccMain.IsTyping[tsLeft]:=False;
  ccMain.IsTyping[tsRight]:=False;
end;



end.

