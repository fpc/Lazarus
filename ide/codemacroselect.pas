{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Dialog to select a macro.
}
unit CodeMacroSelect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, SrcEditorIntf, IDEWindowIntf,
  InputHistory , LazarusIDEStrConsts, ButtonPanel;

type

  { TCodeMacroSelectDlg }

  TCodeMacroSelectDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    ParameterEdit: TEdit;
    ParameterGroupBox: TGroupBox;
    MacrosListBox: TListBox;
    DescriptionMemo: TMemo;
    MacrosGroupBox: TGroupBox;
    DescriptionGroupBox: TGroupBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MacrosListBoxSelectionChange(Sender: TObject; User: boolean);
  private
    FSelected: TIDECodeMacro;
    procedure FillMacrosListbox;
  public
    property Selected: TIDECodeMacro read FSelected;
  end;


function ShowCodeMacroSelectDialog(var Parameter: string): TIDECodeMacro;

implementation

{$R *.lfm}

function ShowCodeMacroSelectDialog(var Parameter: string): TIDECodeMacro;
var
  CodeMacroSelectDlg: TCodeMacroSelectDlg;
begin
  CodeMacroSelectDlg:=TCodeMacroSelectDlg.Create(nil);
  if CodeMacroSelectDlg.ShowModal=mrOk then begin
    Result:=CodeMacroSelectDlg.Selected;
    Parameter:=CodeMacroSelectDlg.ParameterEdit.Text;
  end else begin
    Result:=nil;
    Parameter:='';
  end;
  CodeMacroSelectDlg.Free;
end;

{ TCodeMacroSelectDlg }

procedure TCodeMacroSelectDlg.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self,550,250);

  Caption:=lisCTSelectCodeMacro;
  MacrosGroupBox.Caption:=lisEdtExtToolMacros;
  DescriptionGroupBox.Caption:=lisCodeHelpDescrTag;
  ButtonPanel1.OkButton.Caption:=lisInsertMacro;
  ParameterGroupBox.Caption:=lisCMParameter;
  ParameterEdit.Text:='';
  
  FillMacrosListbox;

  MacrosListBoxSelectionChange(self, False);
end;

procedure TCodeMacroSelectDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TCodeMacroSelectDlg.MacrosListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  i: LongInt;
  MacroName: string;
begin
  i:=MacrosListBox.ItemIndex;
  if (i>=0) then begin
    MacroName:=MacrosListBox.Items[i];
    FSelected:=IDECodeMacros.FindByName(MacroName);
  end else begin
    FSelected:=nil;
  end;
  if FSelected<>nil then begin
    DescriptionMemo.Text:=FSelected.LongDescription;
  end else begin
    DescriptionMemo.Text:=lisCTPleaseSelectAMacro;
  end;
end;

procedure TCodeMacroSelectDlg.FillMacrosListbox;
var
  i: Integer;
begin
  with MacrosListBox.Items do begin
    BeginUpdate;
    Clear;
    for i:=0 to IDECodeMacros.Count-1 do
      Add(IDECodeMacros[i].Name);
    EndUpdate;
  end;
end;

end.

