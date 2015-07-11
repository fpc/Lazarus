unit lr_EditSQLDBParamsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Buttons, ExtCtrls, lr_SQLQuery, DB;

type

  { Tlr_EditSQLDBParamsForm }

  Tlr_EditSQLDBParamsForm = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    Memo1: TMemo;
    Splitter1: TSplitter;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    FParams:TQueryParamList;
    EditItem:integer;
  public
    procedure LoadParamList(AParams:TQueryParamList);
    procedure SaveParamList(AParams:TQueryParamList);
  end;

implementation
uses lr_expres;

{$R *.lfm}

{ Tlr_EditSQLDBParamsForm }

procedure Tlr_EditSQLDBParamsForm.ListBox1Click(Sender: TObject);
var
  P:TQueryParam;
begin
  if (ListBox1.Items.Count>0) and (ListBox1.ItemIndex > -1) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
  begin
    if EditItem>-1 then
    begin
      P:=TQueryParam(FParams[EditItem]);
      case ComboBox1.ItemIndex of
        0:P.ParamType:=ftString; //String
        1:P.ParamType:=ftInteger; //Integer
        2:P.ParamType:=ftFloat; //Float
        3:P.ParamType:=ftDateTime; //DateTime
      else
        P.ParamType:=ftUnknown;
      end;
      P.ParamValue:=Memo1.Text;
    end;
    EditItem:=ListBox1.ItemIndex;
    P:=TQueryParam(FParams[EditItem]);
    case P.ParamType of
      ftString:ComboBox1.ItemIndex:=0; //String
      ftInteger:ComboBox1.ItemIndex:=1; //Integer
      ftFloat:ComboBox1.ItemIndex:=2; //Float
      ftDateTime:ComboBox1.ItemIndex:=3; //DateTime
    else
      ComboBox1.ItemIndex:=-1;
    end;
    Memo1.Text:=P.ParamValue;
  end;
end;

procedure Tlr_EditSQLDBParamsForm.BitBtn1Click(Sender: TObject);
var
  EF:TlrExpresionEditorForm;
begin
  EF:=TlrExpresionEditorForm.Create(Application);
  if EF.ShowModal = mrOk then
    Memo1.Text:=EF.ResultExpresion;
  EF.Free;
end;

procedure Tlr_EditSQLDBParamsForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  if ModalResult = mrOk then
    ListBox1Click(nil);
end;

procedure Tlr_EditSQLDBParamsForm.FormCreate(Sender: TObject);
begin
{  Caption:=slrEditParamsForm_Caption;
  GroupBox1.Caption:=slrEditParamsForm_ParamsList;
  GroupBox2.Caption:=slrEditParamsForm_ParamValue;
  Label1.Caption:=slrEditParamsForm_ParamType;
  Label2.Caption:=slrEditParamsForm_ParamValue;
  BitBtn1.Caption:=slrEditParamsForm_SelectExpresion;}
  //
  FParams:=TQueryParamList.Create;
end;

procedure Tlr_EditSQLDBParamsForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FParams);
end;

procedure Tlr_EditSQLDBParamsForm.LoadParamList(AParams: TQueryParamList);
var
  i:integer;
  P:TQueryParam;
begin
  FParams.Clear;
  ListBox1.Items.Clear;
  for i:=0 to AParams.Count - 1 do
  begin
    P:=TQueryParam(AParams[i]);
    FParams.Add(P.ParamType, P.ParamName, P.ParamValue);
    ListBox1.Items.Add(P.ParamName);
  end;
  EditItem:=-1;
  if ListBox1.Items.Count > 0 then
  begin
    ListBox1.ItemIndex:=0;
    ListBox1Click(nil);
  end;
end;

procedure Tlr_EditSQLDBParamsForm.SaveParamList(AParams: TQueryParamList);
var
  i:integer;
  P, P1:TQueryParam;
begin
  for i:=0 to FParams.Count - 1 do
  begin
    P:=TQueryParam(FParams[i]);
    P1:=TQueryParam(AParams[i]);
    P1.ParamType:=P.ParamType;
    P1.ParamName:=P.ParamName;
    P1.ParamValue:=P.ParamValue;
  end;
end;

end.

