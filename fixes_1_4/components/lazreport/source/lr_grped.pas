
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Group band editor            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_GrpEd;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,ExtCtrls,

  LR_Class,LR_Const, ButtonPanel, EditBtn;

type

  { TfrGroupEditorForm }

  TfrGroupEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Edit1: TEditButton;
    GB1: TGroupBox;
    procedure frSpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView);
  end;

var
  frGroupEditorForm: TfrGroupEditorForm;

implementation

{$R *.lfm}

uses LR_Flds;

procedure TfrGroupEditorForm.ShowEditor(t: TfrView);
begin
  Edit1.Text := (t as TfrBandView).GroupCondition;
  if ShowModal = mrOk then
  begin
    frDesigner.BeforeChange;
    (t as TfrBandView).GroupCondition := Edit1.Text;
  end;
end;

procedure TfrGroupEditorForm.frSpeedButton1Click(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(nil);
  with frFieldsForm do
  if ShowModal = mrOk then
    Edit1.Text := DBField;
  frFieldsForm.Free;
end;

procedure TfrGroupEditorForm.FormCreate(Sender: TObject);
begin
  Caption := sGroupEditorFormCapt;
  GB1.Caption := sGroupEditorFormCond;
  Edit1.ButtonHint := sGroupEditorFormAddDbField;
end;

end.

