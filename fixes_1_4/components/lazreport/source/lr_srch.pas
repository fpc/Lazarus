
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
  StdCtrls,Buttons, ButtonPanel, ExtCtrls, LR_Const;

type

  { TfrPreviewSearchForm }

  TfrPreviewSearchForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBox1: TCheckGroup;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    GroupBox2: TRadioGroup;
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
  GroupBox1.Items.Clear;
  GroupBox1.Items.Add(sFindTextCase);
  //CB1.Caption := sFindTextCase;

  GroupBox2.Caption := sFindTextOrg;
  GroupBox2.Items.Clear;
  GroupBox2.Items.Add(sFindTextFirstPg);
  GroupBox2.Items.Add(sFindTextCurrentPg);
  //RB1.Caption := sFindTextFirstPg;
  //RB2.Caption := sFindTextCurrentPg;
  //Button1.Caption := sOk;
  //Button2.Caption := sCancel;
end;

end.

