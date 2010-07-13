
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Variables form              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Var;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  StdCtrls,

  LCLType,LCLIntf,

  LR_Const;

type

  TfrVarForm = class(TForm)
    ValList: TListBox;
    ValCombo: TComboBox;
    Label1: TLabel;
    procedure ValListDblClick(Sender: TObject);
    procedure ValListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ValComboClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    function CurVal: String;
    function CurDataSet: String;
    procedure GetVariables;
    procedure GetSpecValues;
    procedure GetFRVariables;
    procedure FillValCombo;
  public
    { Public declarations }
    SelectedItem: String;
  end;

var
  frVarForm: TfrVarForm;

implementation

{$R *.lfm}

uses LR_Class;

var
  LastCategory: String;

function TfrVarForm.CurVal: String;
begin
  Result := '';
  if ValList.ItemIndex <> -1 then
    Result := ValList.Items[ValList.ItemIndex];
end;

function TfrVarForm.CurDataSet: String;
begin
  Result := '';
  if ValCombo.ItemIndex <> -1 then
    Result := ValCombo.Items[ValCombo.ItemIndex];
end;

procedure TfrVarForm.FillValCombo;
var
  s: TStringList;
begin
  s := TStringList.Create;
  CurReport.GetCategoryList(s);
  s.Add(sSpecVal);
  s.Add(sFRVariables);
  ValCombo.Items.Assign(s);
  s.Free;
end;

procedure TfrVarForm.ValComboClick(Sender: TObject);
begin
  if CurDataSet = sFRVariables then
    GetFRVariables
  else
    if CurDataSet = sSpecVal then
      GetSpecValues
    else
      GetVariables;
end;

procedure TfrVarForm.GetVariables;
begin
  CurReport.GetVarList(ValCombo.ItemIndex, ValList.Items);
end;

procedure TfrVarForm.GetSpecValues;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    for i := 0 to frSpecCount-1 do
      if i <> 1 then
        Add(frSpecArr[i]);
  end;
end;

procedure TfrVarForm.GetFRVariables;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    for i := 0 to frVariables.Count - 1 do
      Add(frVariables.Name[i]);
  end;
end;

procedure TfrVarForm.ValListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Return then
  begin
    if CurDataSet <> sSpecVal then
      SelectedItem := CurVal
    else
      if ValList.items.Count>0 then
      begin
        if ValList.ItemIndex > 0 then
          SelectedItem := frSpecFuncs[ValList.ItemIndex + 1]
        else
          SelectedItem := frSpecFuncs[0];
      end;

      ModalResult := mrOk;
  end;
end;

procedure TfrVarForm.ValListDblClick(Sender: TObject);
begin
  if CurDataSet <> sSpecVal then
    SelectedItem := CurVal
  else
    if ValList.items.Count>0 then
    begin
      if ValList.ItemIndex > 0 then
        SelectedItem := frSpecFuncs[ValList.ItemIndex + 1]
      else
        SelectedItem := frSpecFuncs[0];
    end;
    
   ModalResult:=mrOk;
end;

procedure TfrVarForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = vk_Escape then
    ModalResult := mrCancel;
end;

procedure TfrVarForm.FormCreate(Sender: TObject);
begin
  Caption := sVarFormCapt;
  Label1.Caption := sVarFormCat;
end;

procedure TfrVarForm.FormActivate(Sender: TObject);
begin
  FillValCombo;
  if ValCombo.Items.IndexOf(LastCategory) <> -1 then
    ValCombo.ItemIndex := ValCombo.Items.IndexOf(LastCategory)
  else
    ValCombo.ItemIndex := 0;
  ValComboClick(nil);
end;

procedure TfrVarForm.FormDeactivate(Sender: TObject);
begin
  if ModalResult = mrOk then
    if CurDataSet <> sSpecVal then
      SelectedItem := CurVal
    else
      if ValList.ItemIndex > 0 then
        SelectedItem := frSpecFuncs[ValList.ItemIndex + 1]
      else
        SelectedItem := frSpecFuncs[0];
  LastCategory := ValCombo.Items[ValCombo.ItemIndex];
end;

end.

