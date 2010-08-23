
{*****************************************}
{                                         }
{             FastReport v2.3             }
{               Fields list               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Flds;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  StdCtrls,LCLType;

type

  { TfrFieldsForm }

  TfrFieldsForm = class(TForm)
    ValCombo: TComboBox;
    ValList: TListBox;
    Label1: TLabel;
    procedure ValComboClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure ValListDblClick(Sender: TObject);
    procedure ValListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ValListSelectionChange(Sender: TObject; User: boolean);
  private
    { Private declarations }
    procedure FillValCombo;
    procedure UpdateDBField;
  public
    { Public declarations }
    DBField: String;
  end;

var
  frFieldsForm: TfrFieldsForm;
implementation

{$R *.lfm}

uses LR_Class, LR_Const, LR_Utils, LR_DBRel, DB;

var
  LastDB: String;


procedure TfrFieldsForm.ValListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
  begin
    UpdateDBField;
    ModalResult := mrOk;
  end;
end;

procedure TfrFieldsForm.FillValCombo;
var
  Lst : TStringList;
begin
  Lst := TStringList.Create;
  try
    if Curreport.DataType = dtDataSet then
      frGetComponents(CurReport.Owner, TDataSet, Lst, nil)
    else
      frGetComponents(CurReport.Owner, TDataSource, Lst, nil);
    Lst.Sort;
    ValCombo.Items.Assign(Lst);
    ValCombo.Enabled:=(Lst.Count>0);
  finally
    Lst.Free;
  end;
end;

procedure TfrFieldsForm.UpdateDBField;
begin
  if ValCombo.Items.Count>0 then
  begin
    LastDB := ValCombo.Items[ValCombo.ItemIndex];
    if ValList.ItemIndex <> -1 then
       DBField:=LastDB + '."' + ValList.Items[ValList.ItemIndex] + '"';
  end
  else DBField:='';
end;

procedure TfrFieldsForm.ValComboClick(Sender: TObject);
var
  DataSet: TfrTDataSet;
begin
  ValList.Items.Clear;
  if ValCombo.Items.Count>0 then
  begin
    DataSet := nil;
    DataSet := frGetDataSet(ValCombo.Items[ValCombo.ItemIndex]);
    if Assigned(DataSet) then
    begin
      try
        frGetFieldNames(DataSet, ValList.Items);
      except
      end;
    end;
  end;
end;

procedure TfrFieldsForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Escape then
    ModalResult := mrCancel;
end;

procedure TfrFieldsForm.ValListSelectionChange(Sender: TObject; User: boolean);
begin
{  if User then
  begin
    UpdateDbField;
    if DBField<>'' then
      ModalResult := mrOk;
  end;}
end;

procedure TfrFieldsForm.FormCreate(Sender: TObject);
begin
  Caption := sFieldsFormInsert;
  Label1.Caption := sFieldsFormAviableDB;
end;

procedure TfrFieldsForm.FormActivate(Sender: TObject);
begin
  FillValCombo;
  if ValCombo.Items.Count>0 then
  begin
    if ValCombo.Items.IndexOf(LastDB) <> -1 then
      ValCombo.ItemIndex := ValCombo.Items.IndexOf(LastDB)
    else
      ValCombo.ItemIndex := 0;
    ValComboClick(nil);
  end;
end;

procedure TfrFieldsForm.FormDeactivate(Sender: TObject);
begin
  //UpdateDBField;
end;

procedure TfrFieldsForm.ValListDblClick(Sender: TObject);
begin
  UpdateDbField;
  if DBField<>'' then
    ModalResult := mrOk;
end;

end.
