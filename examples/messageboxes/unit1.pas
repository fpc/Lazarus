unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type
  TIntArray = array of Longint;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonAskUser: TButton;
    ButtonPromptUser: TButton;
    ButtonAppMsgbox: TButton;
    ButtonLCLIntfMsgbox: TButton;
    ButtonMsgDlg: TButton;
    CheckFlag_AbortRetryIgnore: TRadioButton;
    CheckFlag_Ok: TRadioButton;
    CheckFlag_OkCancel: TRadioButton;
    CheckFlag_RetryCancel: TRadioButton;
    CheckFlag_YesNo: TRadioButton;
    CheckFlag_YesNoCancel: TRadioButton;
    EditCaption: TEdit;
    EditMessage: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ButtonClose: TButton;
    CheckFlag_Info: TRadioButton;
    CheckFlag_Warn: TRadioButton;
    CheckFlag_Error: TRadioButton;
    CheckFlag_Ask: TRadioButton;
    CheckFlag_NoneIcon: TRadioButton;
    CheckFlag_B1: TRadioButton;
    CheckFlag_B2: TRadioButton;
    CheckFlag_B3: TRadioButton;
    CheckFlag_B4: TRadioButton;
    procedure ButtonAppMsgboxClick(Sender: TObject);
    procedure ButtonAskUserClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonLCLIntfMsgboxClick(Sender: TObject);
    procedure ButtonMsgDlgClick(Sender: TObject);
    procedure ButtonPromptUserClick(Sender: TObject);
  private
    function GetMsgFlags: integer;
    function GetDlgType: TMsgDlgType;
    function GetDlgButtons: TMsgDlgButtons;
    function GetDlgTypeInt: integer;
    function GetBtnArray: TIntArray;
    function ButtonIdToString(N: integer): string;
    function ButtonAltIdToString(N: integer): string;

  public

  end;

var
  Form1: TForm1;

implementation

uses
  LCLType, LCLProc, LCLIntf, InterfaceBase, System.UITypes;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

function TForm1.GetMsgFlags: integer;
begin
  Result:= 0;

  if CheckFlag_Ok.Checked then
    Result:= MB_OK
  else
  if CheckFlag_OkCancel.Checked then
    Result:= MB_OKCANCEL
  else
  if CheckFlag_YesNo.Checked then
    Result:= MB_YESNO
  else
  if CheckFlag_YesNoCancel.Checked then
    Result:= MB_YESNOCANCEL
  else
  if CheckFlag_RetryCancel.Checked then
    Result:= MB_RETRYCANCEL
  else
  if CheckFlag_AbortRetryIgnore.Checked then
    Result:= MB_ABORTRETRYIGNORE;

  if CheckFlag_Info.Checked then
    Inc(Result, MB_ICONINFORMATION)
  else
  if CheckFlag_Warn.Checked then
    Inc(Result, MB_ICONWARNING)
  else
  if CheckFlag_Error.Checked then
    Inc(Result, MB_ICONERROR)
  else
  if CheckFlag_Ask.Checked then
    Inc(Result, MB_ICONQUESTION);

  if CheckFlag_B1.Checked then
    Inc(Result, MB_DEFBUTTON1)
  else
  if CheckFlag_B2.Checked then
    Inc(Result, MB_DEFBUTTON2)
  else
  if CheckFlag_B3.Checked then
    Inc(Result, MB_DEFBUTTON3)
  else
  if CheckFlag_B4.Checked then
    Inc(Result, MB_DEFBUTTON4);
end;

function TForm1.GetDlgType: TMsgDlgType;
begin
  if CheckFlag_Info.Checked then
    Result:= mtInformation
  else
  if CheckFlag_Warn.Checked then
    Result:= mtWarning
  else
  if CheckFlag_Error.Checked then
    Result:= mtError
  else
  if CheckFlag_Ask.Checked then
    Result:= mtConfirmation
  else
    Result:= mtCustom;
end;

function TForm1.GetDlgButtons: TMsgDlgButtons;
begin
  if CheckFlag_Ok.Checked then
    Result:= [mbOk]
  else
  if CheckFlag_OkCancel.Checked then
    Result:= mbOKCancel
  else
  if CheckFlag_YesNo.Checked then
    Result:= mbYesNo
  else
  if CheckFlag_YesNoCancel.Checked then
    Result:= mbYesNoCancel
  else
  if CheckFlag_RetryCancel.Checked then
    Result:= [mbRetry, mbCancel]
  else
  if CheckFlag_AbortRetryIgnore.Checked then
    Result:= mbAbortRetryIgnore
  else
    Result:= [mbOk];
end;

function TForm1.GetDlgTypeInt: integer;
begin
  if CheckFlag_Info.Checked then
    Result:= idDialogInfo
  else
  if CheckFlag_Warn.Checked then
    Result:= idDialogWarning
  else
  if CheckFlag_Error.Checked then
    Result:= idDialogError
  else
  if CheckFlag_Ask.Checked then
    Result:= idDialogConfirm
  else
    Result:= 0;
end;

function TForm1.GetBtnArray: TIntArray;
begin
  SetLength(Result, 0);
  if CheckFlag_Ok.Checked then
  begin
    SetLength(Result, 1);
    Result[0]:= idButtonOk;
  end
  else
  if CheckFlag_OkCancel.Checked then
  begin
    SetLength(Result, 2);
    Result[0]:= idButtonOk;
    Result[1]:= idButtonCancel;
  end
  else
  if CheckFlag_YesNo.Checked then
  begin
    SetLength(Result, 2);
    Result[0]:= idButtonYes;
    Result[1]:= idButtonNo;
  end
  else
  if CheckFlag_YesNoCancel.Checked then
  begin
    SetLength(Result, 3);
    Result[0]:= idButtonYes;
    Result[1]:= idButtonNo;
    Result[2]:= idButtonCancel;
  end
  else
  if CheckFlag_RetryCancel.Checked then
  begin
    SetLength(Result, 2);
    Result[0]:= idButtonRetry;
    Result[1]:= idButtonCancel;
  end
  else
  if CheckFlag_AbortRetryIgnore.Checked then
  begin
    SetLength(Result, 3);
    Result[0]:= idButtonAbort;
    Result[1]:= idButtonRetry;
    Result[2]:= idButtonIgnore;
  end;
end;

function TForm1.ButtonIdToString(N: integer): string;
begin
  case N of
    ID_OK: Result:= 'ID_OK';
    ID_CANCEL: Result:= 'ID_CANCEL';
    ID_YES: Result:= 'ID_YES';
    ID_NO: Result:= 'ID_NO';
    ID_RETRY: Result:= 'ID_RETRY';
    ID_IGNORE: Result:= 'ID_IGNORE';
    ID_ABORT: Result:= 'ID_ABORT';
    ID_CLOSE: Result:= 'ID_CLOSE';
    ID_HELP: Result:= 'ID_HELP';
    else Result:= IntToStr(N);
  end;
end;

function TForm1.ButtonAltIdToString(N: integer): string;
begin
  case N of
    idButtonOk: Result:= 'idButtonOk';
    idButtonCancel: Result:= 'idButtonCancel';
    idButtonYes: Result:= 'idButtonYes';
    idButtonNo: Result:= 'idButtonNo';
    idButtonRetry: Result:= 'idButtonRetry';
    idButtonIgnore: Result:= 'idButtonIgnore';
    idButtonAbort: Result:= 'idButtonAbort';
    idButtonClose: Result:= 'idButtonClose';
    idButtonHelp: Result:= 'idButtonHelp';
    else Result:= IntToStr(N);
  end;
end;

procedure TForm1.ButtonAppMsgboxClick(Sender: TObject);
var
  N: integer;
begin
  N:= Application.MessageBox(
    PChar(EditMessage.Text),
    PChar(EditCaption.Text),
    GetMsgFlags);
  Caption:= 'Application.MessageBox() returned '+ButtonIdToString(N);
end;

procedure TForm1.ButtonAskUserClick(Sender: TObject);
var
  Buttons: TDialogButtons;
  N: integer;
  NDef: integer;
begin
  NDef:= -1;
  if CheckFlag_B1.Checked then
    NDef:= 0
  else
  if CheckFlag_B2.Checked then
    NDef:= 1
  else
  if CheckFlag_B3.Checked then
    NDef:= 2
  else
  if CheckFlag_B4.Checked then
    NDef:= 3;

  Buttons:= TDialogButtons.Create(TDialogButton);
  with Buttons.Add do
  begin
    Caption:= '<ok>';
    ModalResult:= mrOk;
  end;
  with Buttons.Add do
  begin
    Caption:= '<cancel>';
    ModalResult:= mrCancel;
  end;
  with Buttons.Add do
  begin
    Caption:= '<yes>';
    ModalResult:= mrYes;
  end;
  with Buttons.Add do
  begin
    Caption:= '<no>';
    ModalResult:= mrNo;
  end;
  with Buttons.Add do
  begin
    Caption:= '<ignore>';
    ModalResult:= mrIgnore;
  end;

  if (NDef>=0) and (NDef<Buttons.Count) then
    Buttons.DefaultButton:= Buttons[NDef];

  N:= AskUser(
    EditCaption.Text,
    EditMessage.Text,
    GetDlgTypeInt,
    Buttons,
    0);
  FreeAndNil(Buttons);

  Caption:= 'AskUser() returned '+ButtonIdToString(N);
end;

procedure TForm1.ButtonLCLIntfMsgboxClick(Sender: TObject);
var
  N: integer;
begin
  N:= LCLIntf.MessageBox(Handle,
    PChar(EditMessage.Text),
    PChar(EditCaption.Text),
    GetMsgFlags
    );
  Caption:= 'LCLIntf.MessageBox() returned '+ButtonIdToString(N);
end;

procedure TForm1.ButtonMsgDlgClick(Sender: TObject);
var
  Res: TModalResult;
begin
  Res:= MessageDlg(
    EditCaption.Text,
    EditMessage.Text,
    GetDlgType,
    GetDlgButtons,
    0);
  Caption:= 'MessageDlg() returned '+ModalResultStr[Res];
end;

procedure TForm1.ButtonPromptUserClick(Sender: TObject);
var
  Btn: array of Longint;
  N: integer;
begin
  Btn:= GetBtnArray;
  if Length(Btn)=0 then exit;

  N:= PromptUser(
    EditCaption.Text,
    EditMessage.Text,
    GetDlgTypeInt,
    @Btn[0],
    Length(Btn),
    0,
    0);
  Caption:= 'PromptUser() returned '+ButtonAltIdToString(N);
end;

end.

