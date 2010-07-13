
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Search dialog              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Srch;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,Forms, Controls, Graphics, Dialogs,
  StdCtrls,Buttons, LR_Const;

type
  TfrPreviewSearchForm = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    CB1: TCheckBox;
    GroupBox2: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frPreviewSearchForm: TfrPreviewSearchForm;

implementation

{$R *.lfm}

procedure TfrPreviewSearchForm.FormActivate(Sender: TObject);
begin
  Edit1.SetFocus;
  Edit1.SelectAll;
end;

procedure TfrPreviewSearchForm.FormCreate(Sender: TObject);
begin
  Caption := sFindTextCaption;
  Label1.Caption := sFindTextText;
  GroupBox1.Caption := sFindTextOptions;
  CB1.Caption := sFindTextCase;
  GroupBox2.Caption := sFindTextOrg;
  RB1.Caption := sFindTextFirstPg;
  RB2.Caption := sFindTextCurrentPg;
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
end;

end.

