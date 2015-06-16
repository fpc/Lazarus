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
  Classes, SysUtils, Controls, Forms, Buttons, StdCtrls, Dialogs,
  LazarusIDEStrConsts, ButtonPanel, IDEDialogs;

type
  { TSysVarUserOverrideDialog }
  TSysVarUserOverrideDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    VariableLabel: TLabel;
    VariableEdit: TEdit;
    ValueLabel: TLabel;
    ValueEdit: TEdit;
    procedure OkButtonClick(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
  end;

function ShowSysVarUserOverrideDialog(var AName, AValue: string): TModalResult;

implementation

{$R *.lfm}

function ShowSysVarUserOverrideDialog(var AName, AValue: string): TModalResult;
begin
  with TSysVarUserOverrideDialog.Create(nil) do
  try
    VariableEdit.Text:=AName;
    ValueEdit.Text:=AValue;
    //if AName=''
    //  then ActiveControl := VariableEdit;
    Result:=ShowModal;
    if (Result=mrOk) then begin
      AName:=Trim(VariableEdit.Text);
      AValue:=ValueEdit.Text;
    end;
  finally
    Free;
  end;
end;

{ TSysVarUserOverrideDialog }

procedure TSysVarUserOverrideDialog.OkButtonClick(Sender: TObject);
var v: string;
begin
  v:=Trim(VariableEdit.Text);
  if not IsValidIdent(v) then begin
    if IDEMessageDialog(lisSVUOInvalidVariableName,
      Format(lisSVUOisNotAValidIdentifier, [v]),
      mtWarning,[mbCancel,mbIgnore])=mrCancel
    then ModalResult := mrNone; //cancel close
  end;
end;

constructor TSysVarUserOverrideDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Caption:=lisSVUOOverrideSystemVariable;

  VariableLabel.Caption:=lisVariable;
  ValueLabel.Caption:=lisValue;
end;

end.

