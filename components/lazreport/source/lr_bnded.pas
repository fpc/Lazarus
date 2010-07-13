
{*****************************************}
{                                         }
{             FastReport v2.3             }
{     Select Band datasource dialog       }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_BndEd;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ButtonPanel,

  LR_Class;

type

  { TfrBandEditorForm }

  TfrBandEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    GB1: TGroupBox;
    Label2: TLabel;
    CB1: TComboBox;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure CB1Click(Sender: TObject);
  private
    { Private declarations }
    procedure FillCombo;
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView);
  end;

var
  frBandEditorForm: TfrBandEditorForm;

implementation

{$R *.lfm}

uses LR_DSet, LR_Const, LR_Utils;

procedure TfrBandEditorForm.ShowEditor(t: TfrView);
var
  i: Integer;
  s: String;
begin
  FillCombo;
  s := (t as TfrBandView).DataSet;

  if (s <> '') and (s[1] in ['1'..'9']) then
  begin
    i := 1;
    Edit1.Text := s;
  end
  else
  begin
    i := CB1.Items.IndexOf(s);
    if i = -1 then
      i := CB1.Items.IndexOf(sNotAssigned);
  end;
  CB1.ItemIndex := i;
  CB1Click(nil);
  if ShowModal = mrOk then
  begin
    frDesigner.BeforeChange;
    if CB1.ItemIndex = 1 then
      (t as TfrBandView).DataSet := Edit1.Text
    else
      (t as TfrBandView).DataSet := CB1.Items[CB1.ItemIndex];
  end;
end;

procedure TfrBandEditorForm.FillCombo;
begin
  frGetComponents(CurReport.Owner, TfrDataset, CB1.Items, nil);
  TStringList(CB1.Items).Sort;

  TStringList(CB1.Items).Sorted := False;
  CB1.Items.Insert(0, sVirtualDataset);
  CB1.Items.Insert(0, sNotAssigned);
end;

procedure TfrBandEditorForm.FormCreate(Sender: TObject);
begin
  Caption := sBandEditorFormCapt;
  GB1.Caption := sBandEditorFormDataSrc;
  Label2.Caption := sBandEditorFormRecCount;
end;

procedure TfrBandEditorForm.CB1Click(Sender: TObject);
begin
  frEnableControls([Label2, Edit1], CB1.ItemIndex = 1);
end;

end.

