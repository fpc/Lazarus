
{*****************************************}
{                                         }
{             FastReport v2.3             }
{          Insert fields dialog           }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_IFlds;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ButtonPanel,

  LR_DBRel;

type

  { TfrInsertFieldsForm }

  TfrInsertFieldsForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    FieldsL: TListBox;
    DatasetCB: TComboBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    HorzRB: TRadioButton;
    VertRB: TRadioButton;
    GroupBox2: TGroupBox;
    HeaderCB: TCheckBox;
    BandCB: TCheckBox;
    procedure DatasetCBChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure GetFields;
  public
    { Public declarations }
    DataSet: TfrTDataSet;
  end;

var
  frInsertFieldsForm: TfrInsertFieldsForm;

implementation

{$R *.lfm}

uses LR_Class, LR_Const, LR_Utils, DB;

procedure TfrInsertFieldsForm.FormShow(Sender: TObject);
begin
  DataSet := nil;
  if Curreport.DataType = dtDataSet then
    frGetComponents(CurReport.Owner, TDataSet, DatasetCB.Items, nil)
  else
    frGetComponents(CurReport.Owner, TDataSource, DatasetCB.Items, nil);
  if DatasetCB.Items.Count > 0 then
    DatasetCB.ItemIndex := 0;
  GetFields;
end;

procedure TfrInsertFieldsForm.DatasetCBChange(Sender: TObject);
begin
  GetFields;
end;

procedure TfrInsertFieldsForm.GetFields;
begin
  FieldsL.Items.Clear;
  if DatasetCB.ItemIndex<>-1 then
  begin
    DataSet := frGetDataSet(DatasetCB.Items[DatasetCB.ItemIndex]);
    if DataSet <> nil then
      frGetFieldNames(DataSet, FieldsL.Items);
  end;
end;

procedure TfrInsertFieldsForm.FormCreate(Sender: TObject);
begin
  Caption := sInsertFieldsFormCapt;
  Label1.Caption := sInsertFieldsFormAviableDSet;
  GroupBox1.Caption := sInsertFieldsFormPlace;
  HorzRB.Caption := sInsertFieldsFormHorz;
  VertRB.Caption := sInsertFieldsFormVert;
  HeaderCB.Caption := sInsertFieldsFormHeader;
  BandCB.Caption := sInsertFieldsFormBand;
end;

end.

