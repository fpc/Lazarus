{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    A dialog to choose a TCustomTextConverterToolClass.
}
unit IDETextConvListAdd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, IDETextConverter;

type

  { TIDETextConvListAddDlg }

  TIDETextConvListAddDlg = class(TForm)
    ClassLabel: TLabel;
    ClassComboBox: TComboBox;
    AddButton: TButton;
    CancelButton: TButton;
    procedure AddButtonClick(Sender: TObject);
    procedure ClassComboBoxEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FToolClass: TCustomTextConverterToolClass;
    procedure SetToolClass(const AValue: TCustomTextConverterToolClass);
    procedure FillClassComboBox;
  public
    property ToolClass: TCustomTextConverterToolClass read FToolClass write SetToolClass;
  end;


function ShowIDETextConvListAddDlg(out ToolClass: TCustomTextConverterToolClass
  ): TModalResult;

implementation

function ShowIDETextConvListAddDlg(out ToolClass: TCustomTextConverterToolClass
  ): TModalResult;
var
  IDETextConvListAddDlg: TIDETextConvListAddDlg;
begin
  IDETextConvListAddDlg:=TIDETextConvListAddDlg.Create(nil);
  Result:=IDETextConvListAddDlg.ShowModal;
  ToolClass:=IDETextConvListAddDlg.ToolClass;
  IDETextConvListAddDlg.Free;
end;

{ TIDETextConvListAddDlg }

procedure TIDETextConvListAddDlg.FormCreate(Sender: TObject);
begin
  ClassLabel.Caption:='&Select a class';
  AddButton.Caption:='&Add';
  CancelButton.Caption:='&Cancel';
  
  FillClassComboBox;
  ToolClass:=TextConverterToolClasses[0];
end;

procedure TIDETextConvListAddDlg.ClassComboBoxEditingDone(Sender: TObject);
begin
  FToolClass:=TextConverterToolClasses.FindByFirstLineOfClassDescription(
                                                            ClassComboBox.Text);
end;

procedure TIDETextConvListAddDlg.AddButtonClick(Sender: TObject);
begin
  if FToolClass=nil then begin
    MessageDlg('Invalid class',
      'Invalid class',mtError,[mbCancel],0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TIDETextConvListAddDlg.SetToolClass(
  const AValue: TCustomTextConverterToolClass);
begin
  if FToolClass=AValue then exit;
  FToolClass:=AValue;
  if FToolClass<>nil then
    ClassComboBox.Text:=FToolClass.FirstLineOfClassDescription;
end;

procedure TIDETextConvListAddDlg.FillClassComboBox;
var
  i: Integer;
begin
  ClassComboBox.Items.BeginUpdate;
  ClassComboBox.Items.Clear;
  for i:=0 to TextConverterToolClasses.Count-1 do begin
    ClassComboBox.Items.Add(
                       TextConverterToolClasses[i].FirstLineOfClassDescription);
  end;
  ClassComboBox.Items.EndUpdate;
end;

initialization
  {$I idetextconvlistadd.lrs}

end.

