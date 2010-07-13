
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Progress dialog             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_progr;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources, LMessages,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,LCLIntf,
  
  LCLProc,

  LR_Const, LR_Class, ExtCtrls;

const
  CM_BeforeModal = WM_USER + 1;

type

  { TfrProgressForm }

  TfrProgressForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    fDoc: TfrReport;
    fOnBeforeModal: TNotifyEvent;
    procedure DoBeforeModal(Data: ptrint);
  public
    { Public declarations }
    FirstCaption: String;
    function Show_Modal(Doc: TfrReport): Word;
    procedure ModalDone(AModalResult: TModalResult);

    property OnBeforeModal: TNotifyEvent read FOnBeforeModal write FOnBeforeModal;
  end;

var
  frProgressForm: TfrProgressForm;

implementation

{$R *.lfm}

function TfrProgressForm.Show_Modal(Doc: TfrReport): Word;
begin
  FDoc := Doc;
  Application.QueueAsyncCall(@DoBeforeModal, 0);
  //PostMessage(Handle, CM_BeforeModal, 0, 0);
  Visible:=False;
  Enabled:=True;
  ModalResult:=mrNone;
  InitializeWnd;
  Result:=ShowModal;
end;

procedure TfrProgressForm.ModalDone(AModalResult: TModalResult);
begin
  ModalResult := AModalResult;
  Timer1.Enabled:=true;
end;

procedure TfrProgressForm.Button1Click(Sender: TObject);
begin
  fDoc.Terminated := True;
  ModalResult := mrCancel;
end;

procedure TfrProgressForm.DoBeforeModal(Data: Ptrint);
begin
  if Assigned(fOnBeforeModal) then
    fOnBeforeModal(Self);
end;

procedure TfrProgressForm.FormCreate(Sender: TObject);
begin
  Button1.Caption:=sCancel;
end;

procedure TfrProgressForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
end;

end.

