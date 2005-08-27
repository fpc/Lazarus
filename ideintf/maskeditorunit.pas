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

unit MaskEditorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, MaskEdit, PropEdits, Componenteditors, TypInfo;

type

  { TMaskEditorForm }

  TMaskEditorForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    BtnLoadDem: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    TestMaskEdit: TMaskEdit;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure BtnLoadDemClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure MaskEditorFormCreate(Sender: TObject);
  private
    function GetEditMask: string;
    procedure LoadDEMFile(AFileName:string);
    procedure SetEditMask(AValue: string);
    procedure UpdateTestEditor;
  public
    property EditMask:string read GetEditMask write SetEditMask;
  end; 

type
  { TEditMaskProperty }
  TEditMaskProperty = class(TStringPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation
uses StrUtils;

{ TMaskEditorForm }

procedure TMaskEditorForm.BtnLoadDemClick(Sender: TObject);
begin
  OpenDialog1.InitialDir:=ExtractFileDir(ParamStr(0));
  if OpenDialog1.Execute then
    LoadDEMFile(OpenDialog1.FileName);
end;

procedure TMaskEditorForm.CheckBox1Click(Sender: TObject);
var
  I:integer;
  S1:string;
begin
  S1:=Edit1.Text;
  I:=Pos(';', S1);
  if (I>0) and (I<Length(S1)) then
  begin
    S1[i+1]:=IntToStr(Ord(CheckBox1.Checked))[1];
    Edit1.Text:=S1;
  end;
end;

procedure TMaskEditorForm.Edit1Change(Sender: TObject);
begin
  UpdateTestEditor;
end;

procedure TMaskEditorForm.Edit2Change(Sender: TObject);
var
  I:integer;
  S1:string;
begin
  S1:=Edit1.Text;
  I:=NPos(';', S1, 2);
  if (I>0) and (I<Length(S1)) and (Edit2.Text<>'') then
  begin
    S1[i+1]:=Edit2.Text[1];
    Edit1.Text:=S1;
  end;
end;

procedure TMaskEditorForm.ListBox1Click(Sender: TObject);
var
  S1, S2:string;
begin
  TestMaskEdit.Text:='';
  S1:=ListBox1.Items[ListBox1.ItemIndex];
  Delete(S1, 1, Pos('|', S1));
  S2:=Copy(S1, 1, Pos('|', S1)-1);
  Delete(S1, 1, Pos('|', S1));
  EditMask:=S1;
end;

procedure TMaskEditorForm.MaskEditorFormCreate(Sender: TObject);
var
  aDemFile:string;
begin
  aDemFile:=ExtractFileDir(ParamStr(0))+DirectorySeparator+'us.dem';
  if FileExists(aDemFile) then
    LoadDEMFile(aDemFile);
end;

function TMaskEditorForm.GetEditMask: string;
begin
  Result:=Edit1.Text;
end;

procedure TMaskEditorForm.LoadDEMFile(AFileName: string);
begin
  ListBox1.Items.Clear;
  ListBox1.Items.LoadFromFile(AFileName);
end;

procedure TMaskEditorForm.SetEditMask(AValue: string);
begin
  Edit1.Text:=AValue;
  Delete(AValue, 1, Pos(';', AValue));
  if AValue<>'' then
    CheckBox1.Checked:=AValue[1]='1';
  Delete(AValue, 1, Pos(';', AValue));
  Edit2.Text:=AValue;
  UpdateTestEditor;
end;

procedure TMaskEditorForm.UpdateTestEditor;
begin
  TestMaskEdit.EditMask:=Edit1.Text;
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
  {$I maskeditorunit.lrs}
  RegisterPropertyEditor(TypeInfo(string), TCustomMaskEdit, 'EditMask', TEditMaskProperty);

end.

