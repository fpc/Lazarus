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
  LazarusIDEStrConsts, LResources;

type
  TSysVarUserOverrideDialog = class(TForm)
    VariableLabel: TLabel;
    VariableEdit: TEdit;
    ValueLabel: TLabel;
    ValueEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure SysVarUserOverrideDialogResize(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
  end;

function ShowSysVarUserOverrideDialog(var Variable, Value: string): TModalResult;


implementation


function ShowSysVarUserOverrideDialog(var Variable, Value: string): TModalResult;
var SysVarUserOverrideDialog: TSysVarUserOverrideDialog;
begin
  SysVarUserOverrideDialog:=TSysVarUserOverrideDialog.Create(Application);
  with SysVarUserOverrideDialog do begin
    VariableEdit.Text:=Variable;
    ValueEdit.Text:=Value;
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
    then exit;
  end;
  ModalResult:=mrOk;
end;

procedure TSysVarUserOverrideDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TSysVarUserOverrideDialog.SysVarUserOverrideDialogResize(
  Sender: TObject);
begin
  with VariableLabel do begin
    SetBounds(10,10,150,Height);
  end;

  with VariableEdit do begin
    SetBounds(VariableLabel.Left,VariableLabel.Top+VariableLabel.Height+2,
      Self.ClientWidth-2*VariableLabel.Left,Height);
  end;

  with ValueLabel do begin
    SetBounds(VariableEdit.Left,VariableEdit.Top+VariableEdit.Height+10,
      150,Height);
  end;

  with ValueEdit do begin
    SetBounds(ValueLabel.Left,ValueLabel.Top+ValueLabel.Height+2,
      Self.ClientWidth-2*ValueLabel.Left,Height);
  end;

  with OkButton do begin
    SetBounds(Self.ClientWidth-220,Self.ClientHeight-40,100,25);
  end;

  with CancelButton do begin
    SetBounds(OkButton.Left+OkButton.Width+10,OkButton.Top,100,25);
  end;
end;

constructor TSysVarUserOverrideDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin

    Caption:=lisSVUOOverrideSystemVariable;
    Width:=400;
    Height:=170;
    Position:=poScreenCenter;
    OnResize:=@SysVarUserOverrideDialogResize;

    VariableLabel:=TLabel.Create(Self);
    with VariableLabel do begin
      Name:='VariableLabel';
      Parent:=Self;
      SetBounds(10,10,150,Height);
      Caption:=lisCodeToolsDefsVariable;
      Visible:=true;
    end;
    
    VariableEdit:=TEdit.Create(Self);
    with VariableEdit do begin
      Name:='VariableEdit';
      Parent:=Self;
      SetBounds(VariableLabel.Left,VariableLabel.Top+VariableLabel.Height+2,
        Self.ClientWidth-2*VariableLabel.Left,Height);
      Visible:=true;
    end;

    ValueLabel:=TLabel.Create(Self);
    with ValueLabel do begin
      Name:='ValueLabel';
      Parent:=Self;
      SetBounds(VariableEdit.Left,VariableEdit.Top+VariableEdit.Height+10,
        150,Height);
      Caption:='Value:';
      Visible:=true;
    end;

    ValueEdit:=TEdit.Create(Self);
    with ValueEdit do begin
      Name:='ValueEdit';
      Parent:=Self;
      SetBounds(ValueLabel.Left,ValueLabel.Top+ValueLabel.Height+2,
        Self.ClientWidth-2*ValueLabel.Left,Height);
      Visible:=true;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-220,Self.ClientHeight-40,100,25);
      Caption:=lisSVUOOk;
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      SetBounds(OkButton.Left+OkButton.Width+10,OkButton.Top,100,25);
      Caption:=dlgCancel;
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;
  end;
  SysVarUserOverrideDialogResize(nil);
end;

end.

