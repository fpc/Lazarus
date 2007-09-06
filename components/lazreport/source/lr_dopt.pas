
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Document options             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Dopt;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,
  LR_Const;

type

  { TfrDocOptForm }

  TfrDocOptForm = class(TForm)
    edTitle: TEdit;
    edSubject: TEdit;
    edKeyWords: TEdit;
    GroupBox1: TGroupBox;
    ComB1: TComboBox;
    CB1: TCheckBox;
    GroupBox2: TGroupBox;
    CB2: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    labTitle: TLabel;
    labSubject: TLabel;
    labKeyWords: TLabel;
    labComments: TLabel;
    edComments: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frDocOptForm: TfrDocOptForm;

implementation

uses LR_Prntr;

procedure TfrDocOptForm.FormActivate(Sender: TObject);
begin
  ComB1.Items.Assign(Prn.Printers);
  ComB1.ItemIndex := Prn.PrinterIndex;
end;

procedure TfrDocOptForm.FormCreate(Sender: TObject);
begin
  Caption := sDocOptFormOpt;
  GroupBox1.Caption := sDocOptFormPrinter;
  CB1.Caption := sDocOptFormSelect;
  GroupBox2.Caption := sDocOptFormOther;
  CB2.Caption := sDocOptForm2Pass;
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
  labTitle.Caption:=sDocOptFormTitle;
  labSubject.Caption:=sDocOptFormSubject;
  labKeywords.Caption:=sDocOptFormKeyWords;
  labComments.Caption:=sDocOptFormComments;
end;

initialization
  {$I lr_dopt.lrs}

end.

