unit frmconfprojdatadict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  EditBtn, StdCtrls, idedatadict, ldd_consts;

type

  { TConfigureProjectDDForm }

  TConfigureProjectDDForm = class(TForm)
    BOK: TButton;
    BCancel: TButton;
    CBUseDataDict: TCheckBox;
    CBDD: TComboBox;
    FEDD: TFileNameEdit;
    OpenDialog1: TOpenDialog;
    RBUseKnownDD: TRadioButton;
    RBUseFile: TRadioButton;
    procedure BOKClick(Sender: TObject);
    procedure CBDDChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBUseFileClick(Sender: TObject);
  private
    { private declarations }
    FDD : TIDEDataDictionary;
    procedure CheckData;
    procedure CheckRB;
    procedure DataDictToForm;
    procedure FormToDataDict;
    procedure SetKnownFileName;
  public
    { public declarations }
  end; 

var
  ConfigureProjectDDForm: TConfigureProjectDDForm;

Function ShowConfigProjectDDDialog : Boolean;

implementation

Function ShowConfigProjectDDDialog : Boolean;

begin
  With TConfigureProjectDDForm.Create(Application) do
    try
      Result:=(ShowModal=MROK)
    finally
      Free;
    end;
end;


{ TConfigureProjectDDForm }

procedure TConfigureProjectDDForm.CBDDChange(Sender: TObject);
begin
  SetKnownFileName;
end;

procedure TConfigureProjectDDForm.BOKClick(Sender: TObject);
begin
  CheckData;
  FormToDataDict;
end;

procedure TConfigureProjectDDForm.CheckData;

begin
  If CBUseDataDict.Checked then
    If RBUseKnownDD.Checked then
      begin
      if (CBDD.Text='') then
        Raise Exception.Create(SErrSelectDD)
      end
    else
      begin
      If (FEDD.FileName='') or not (FileExistsUTF8(FEDD.FileName)) then
        Raise Exception.Create(SErrSelectFile)
      end;
end;


procedure TConfigureProjectDDForm.FormCreate(Sender: TObject);
begin
  //
  Caption     := ldd_SetProjectDataDictionary;
  FEDD.Filter := ldd_Filenameedit;
  CBUseDataDict.Caption := ldd_UseDatadictionary;
  RBUseKnownDD.Caption  := ldd_Knowndatadictionary;
  RBUseFile.Caption     := ldd_Datadictionary;
  OpenDialog1.Title     := ldd_Openexistingfile;
  BOK.Caption           := ldd_Ok;
  BCancel.Caption       := ldd_Cancel;
  FDD:=IDEDataDictionary;
  FDD.Update;
  DataDictToForm;
end;

procedure TConfigureProjectDDForm.RBUseFileClick(Sender: TObject);
begin
  CheckRB;
end;

procedure TConfigureProjectDDForm.FormToDataDict;

begin
  If Not CBUSeDataDict.Checked then
    FDD.Active:=False;
  if RBUseKnownDD.Checked then
    FDD.DictionaryName:=CBDD.Text
  else
    FDD.FileName:=FEDD.FileName;
  If CBUSeDataDict.Checked then
    FDD.Active:=True;
  FDD.Save;
end;


procedure TConfigureProjectDDForm.DataDictToForm;

begin
  FDD.GetKnownDictionaries(CBDD.Items);
  CBUseDataDict.Checked:=FDD.Active;
  RBUseKnownDD.Checked:=(FDD.DictionaryName<>'') or (FDD.FileName='');
  CheckRB;
  FEDD.InitialDir:=DefaultDictionaryDir;
  If RBUSeKnownDD.Checked and (FDD.DictionaryName<>'') then
    begin
    With CBDD do
      ItemIndex:=Items.IndexOf(FDD.DictionaryName);
    SetKnownFileName;
    end
  else
    FEDD.FileName:=FDD.FileName;
end;

procedure TConfigureProjectDDForm.CheckRB;

begin
  CBDD.Enabled:=RBUseKnownDD.Checked;
  FEDD.Enabled:=Not CBDD.Enabled;
end;


procedure TConfigureProjectDDForm.SetKnownFileName;

Var
  FN : String;

begin
  FN:=FDD.GetDictionaryFileName(CBDD.Text);
  FEDD.FileName:=FN;
end;

initialization
  {$I frmconfprojdatadict.lrs}

end.

