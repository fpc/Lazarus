
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
  Classes, SysUtils, LResources,LMessages,Messages,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,LCLIntf,
  
  LCLProc,

  LR_Class, LR_Const;

const
  CM_BeforeModal = WM_USER + 1;

type

  { TfrProgressForm }

  TfrProgressForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fDoc: TfrReport;
    fOnBeforeModal: TNotifyEvent;

    procedure DoBeforeModal(Data: ptrint);
  public
    { Public declarations }
    FirstCaption: String;
    property OnBeforeModal: TNotifyEvent read FOnBeforeModal write FOnBeforeModal;

    function Show_Modal(Doc: TfrReport): Word;
  end;

var
  frProgressForm: TfrProgressForm;

implementation


function TfrProgressForm.Show_Modal(Doc: TfrReport): Word;
begin
  FDoc := Doc;
  Application.QueueAsyncCall(@DoBeforeModal, 0);
  //PostMessage(Handle, CM_BeforeModal, 0, 0);
  Visible:=False;
  Enabled:=True;
  ModalResult:=mrNone;
  {$IFDEF DebugLR}
  DebugLn('A1');
  {$ENDIF}
  InitializeWnd;
  
  Result:=ShowModal;
  {$IFDEF DebugLR}
  DebugLn('A2');
  {$ENDIF}
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

initialization
  {$I lr_progr.lrs}

end.

