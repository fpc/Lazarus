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
    Dialog to edit System-Variables-User-Overrides.
    Used by the run parameter dialog
}
unit SysVarUserOverrideDlg;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Controls, Forms, Buttons, StdCtrls, ComCtrls, Dialogs,
  LazarusIDEStrConsts, ButtonPanel;

type
  { TSysVarUserOverrideDialog }
  TSysVarUserOverrideDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    VariableLabel: TLabel;
    VariableEdit: TEdit;
    ValueLabel: TLabel;
    ValueEdit: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
  end;

function ShowSysVarUserOverrideDialog(var Variable, Value: string): TModalResult;

implementation

{$R *.lfm}

function ShowSysVarUserOverrideDialog(var Variable, Value: string): TModalResult;
var SysVarUserOverrideDialog: TSysVarUserOverrideDialog;
begin
  SysVarUserOverrideDialog:=TSysVarUserOverrideDialog.Create(nil);
  with SysVarUserOverrideDialog do begin
    VariableEdit.Text:=Variable;
    ValueEdit.Text:=Value;
    if Variable=''
      then ActiveControl := VariableEdit;
    Result:=ShowModal;
    if (Result=mrOk) then begin
      Variable:=Trim(VariableEdit.Text);
      Value:=ValueEdit.Text;
    end;
    Free;
  end;
end;

{ TSysVarUserOverrideDialog }

procedure TSysVarUserOverrideDialog.OkButtonClick(Sender: TObject);
var v: string;
begin
  v:=Trim(VariableEdit.Text);
  if not IsValidIdent(v) then begin
    if MessageDlg(lisSVUOInvalidVariableName,
      Format(lisSVUOisNotAValidIdentifier, ['"', v, '"']),
      mtWarning,[mbCancel,mbIgnore],0)=mrCancel
    then ModalResult := mrNone; //cancel close
  end;
end;

procedure TSysVarUserOverrideDialog.FormCreate(Sender: TObject);
begin
  //XXX: ButtonPanel's button event can't be assigned from OI
  ButtonPanel.OKButton.OnClick:=@OKButtonClick;
end;

constructor TSysVarUserOverrideDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption:=lisSVUOOverrideSystemVariable;

  VariableLabel.Caption:=lisCodeToolsDefsVariable;
  ValueLabel.Caption:=lisValue;
end;

end.

