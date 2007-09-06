
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

  LR_Class,LR_Const;

type
  TfrGroupEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GB1: TGroupBox;
    Edit1: TEdit;
    Panel1: TPanel;
    frSpeedButton1: TSpeedButton;
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
  frSpeedButton1.Hint := sGroupEditorFormAddDbField;
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
end;

INITIALIZATION
  {$I lr_grped.lrs}

end.

