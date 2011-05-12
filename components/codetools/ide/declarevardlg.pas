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
    LCL controls for Cody.
}
unit DeclareVarDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls,
  FileProcs, CodeToolManager, CodeCache, FindDeclarationTool, CodeCompletionTool;

type

  { TCodyDeclareVarDialog }

  TCodyDeclareVarDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    NameEdit: TEdit;
    NameLabel: TLabel;
    Panel1: TPanel;
    TypeEdit: TEdit;
    TypeLabel: TLabel;
    WhereComboBox: TComboBox;
    WhereLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
  public
  end;


procedure ShowDeclareVariableDialog(Sender: TObject);

implementation

procedure ShowDeclareVariableDialog(Sender: TObject);
var
  CodyDeclareVarDialog: TCodyDeclareVarDialog;
begin
  ShowMessage('Not implemented yet');
  CodyDeclareVarDialog:=TCodyDeclareVarDialog.Create(nil);
  try
    CodyDeclareVarDialog.ShowModal;
  finally
    CodyDeclareVarDialog.Free;
  end;
end;

{$R *.lfm}

{ TCodyDeclareVarDialog }

procedure TCodyDeclareVarDialog.FormCreate(Sender: TObject);
begin
  Caption:='Declare a new variable';
  WhereLabel.Caption:='Where';
  NameLabel.Caption:='Name';
  TypeEdit.Caption:='Type';
end;

procedure TCodyDeclareVarDialog.FormDestroy(Sender: TObject);
begin

end;

end.

