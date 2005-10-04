{ Copyright (C) 2005

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Lagunov Aleksey

  Abstract:
    Property Editors for TMaskEdit.EditMask of FCL and LCL.
}

unit MaskPropEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, FileUtil, MaskEdit, LazIDEIntf, PropEdits,
  ObjInspStrConsts;

type

  { TMaskEditorForm }

  TMaskEditorForm = class(TForm)
    OkButton: TButton;
    CancelButton: TButton;
    HelpButton: TButton;
    LoadSampleMasksButton: TButton;
    SaveLiteralCheckBox: TCheckBox;
    InputMaskEdit: TEdit;
    CharactersForBlanksEdit: TEdit;
    InputMaskLabel: TLabel;
    SampleMasksLabel: TLabel;
    CharactersForBlanksLabel: TLabel;
    TestInputLabel: TLabel;
    SampleMasksListBox: TListBox;
    TestMaskEdit: TMaskEdit;
    OpenDialog1: TOpenDialog;
    TestInputPanel: TPanel;
    procedure LoadSampleMasksButtonClick(Sender: TObject);
    procedure SaveLiteralCheckBoxClick(Sender: TObject);
    procedure InputMaskEditChange(Sender: TObject);
    procedure CharactersForBlankEditChange(Sender: TObject);
    procedure SampleMasksListBoxClick(Sender: TObject);
    procedure MaskEditorFormCreate(Sender: TObject);
  private
    function GetEditMask: string;
    procedure LoadDEMFile(AFileName:string);
    procedure SetEditMask(AValue: string);
    procedure UpdateTestEditor;
  public
    property EditMask:string read GetEditMask write SetEditMask;
  end; 

  { TEditMaskProperty }

  TEditMaskProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

uses StrUtils;

{ TMaskEditorForm }

procedure TMaskEditorForm.LoadSampleMasksButtonClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFileDir(ParamStr(0));
  if OpenDialog1.Execute then
    LoadDEMFile(OpenDialog1.FileName);
end;

procedure TMaskEditorForm.SaveLiteralCheckBoxClick(Sender: TObject);
var
  I:integer;
  S1:string;
begin
  S1:=InputMaskEdit.Text;
  I:=Pos(';', S1);
  if (I>0) and (I<Length(S1)) then
  begin
    S1[i+1]:=IntToStr(Ord(SaveLiteralCheckBox.Checked))[1];
    InputMaskEdit.Text:=S1;
  end;
end;

procedure TMaskEditorForm.InputMaskEditChange(Sender: TObject);
begin
  UpdateTestEditor;
end;

procedure TMaskEditorForm.CharactersForBlankEditChange(Sender: TObject);
var
  I:integer;
  S1:string;
begin
  S1:=InputMaskEdit.Text;
  I:=NPos(';', S1, 2);
  if (I>0) and (I<Length(S1)) and (CharactersForBlanksEdit.Text<>'') then
  begin
    S1[i+1]:=CharactersForBlanksEdit.Text[1];
    InputMaskEdit.Text:=S1;
  end;
end;

procedure TMaskEditorForm.SampleMasksListBoxClick(Sender: TObject);
var
  S1, S2:string;
begin
  if (SampleMasksListBox.Items.Count>0) then
  begin
    TestMaskEdit.Text:='';
    S1:=SampleMasksListBox.Items[SampleMasksListBox.ItemIndex];
    Delete(S1, 1, Pos('|', S1));
    S2:=Copy(S1, 1, Pos('|', S1)-1);
    Delete(S1, 1, Pos('|', S1));
    EditMask:=S2;
  end;
end;

procedure TMaskEditorForm.MaskEditorFormCreate(Sender: TObject);
var
  aDemFile:string;
begin
  OkButton.Caption:=oisOk2;
  CancelButton.Caption:=oiStdActDataSetCancel1Hint;
  HelpButton.Caption:=cActionListEditorHelpCategory;
  LoadSampleMasksButton.Caption:=oisMasks;
  SaveLiteralCheckBox.Caption:=oisSaveLiteralCharacters;
  InputMaskLabel.Caption:=oisInputMask;
  SampleMasksLabel.Caption:=oisSampleMasks;
  CharactersForBlanksLabel.Caption:=oisCharactersForBlanks;
  TestInputLabel.Caption:=oisTestInput;

  if LazarusIDE<>nil then
    aDemFile:=LazarusIDE.GetPrimaryConfigPath
  else
    aDemFile:=ExtractFileDir(ParamStr(0));
  aDemFile:=CleanAndExpandDirectory(aDemFile)+'maskeditmasks.txt';
  if FileExists(aDemFile) then
    LoadDEMFile(aDemFile);
end;

function TMaskEditorForm.GetEditMask: string;
begin
  Result:=InputMaskEdit.Text;
end;

procedure TMaskEditorForm.LoadDEMFile(AFileName: string);
begin
  SampleMasksListBox.Items.Clear;
  SampleMasksListBox.Items.LoadFromFile(AFileName);
end;

procedure TMaskEditorForm.SetEditMask(AValue: string);
begin
  InputMaskEdit.Text:=AValue;
  Delete(AValue, 1, Pos(';', AValue));
  if AValue<>'' then
    SaveLiteralCheckBox.Checked:=AValue[1]='1';
  Delete(AValue, 1, Pos(';', AValue));
  CharactersForBlanksEdit.Text:=AValue;
  UpdateTestEditor;
end;

procedure TMaskEditorForm.UpdateTestEditor;
begin
  TestMaskEdit.EditMask:=InputMaskEdit.Text;
end;

{ TEditMaskProperty }

function TEditMaskProperty.GetAttributes: TPropertyAttributes;
begin
  Result:= [paDialog];
end;

procedure TEditMaskProperty.Edit;
var
  MaskEditorForm: TMaskEditorForm;
begin
  MaskEditorForm:=TMaskEditorForm.Create(Application);
  try
    MaskEditorForm.EditMask:=GetValue;
    if MaskEditorForm.ShowModal = mrOk then
      SetValue(MaskEditorForm.EditMask);
  finally
    MaskEditorForm.Free;
  end;
end;


initialization
  {$I maskpropedit.lrs}
  RegisterPropertyEditor(TypeInfo(string), TCustomMaskEdit, 'EditMask',
                         TEditMaskProperty);

end.

