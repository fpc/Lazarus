{***************************************************************************
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
}
unit CompOptCondPropsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls,
  ProjectIntf,
  LazarusIDEStrConsts;

type

  { TCompOptCondPropsDialog }

  TCompOptCondPropsDialog = class(TForm)
    ValueButton: TButton;
    ButtonPanel1: TButtonPanel;
    NodeTypeComboBox: TComboBox;
    ValueTypeComboBox: TComboBox;
    ValueEdit: TEdit;
    NodeTypeLabel: TLabel;
    ValueTypeLabel: TLabel;
    ValueLabel: TLabel;
    PropsGroupBox: TGroupBox;
    procedure FormCreate(Sender: TObject);
    procedure NodeTypeComboBoxEditingDone(Sender: TObject);
    procedure ValueEditEditingDone(Sender: TObject);
    procedure ValueTypeComboBoxEditingDone(Sender: TObject);
  private
    FNode: TCompOptCondNode;
    procedure SetNode(const AValue: TCompOptCondNode);
    procedure NodeChanged;
    procedure NodeTypeChanged;
  public
    property Node: TCompOptCondNode read FNode write SetNode;
  end; 

function EditCompOptCondProperties(Node: TCompOptCondNode): TModalResult;

implementation

function EditCompOptCondProperties(Node: TCompOptCondNode): TModalResult;
var
  CompOptCondPropsDialog: TCompOptCondPropsDialog;
begin
  CompOptCondPropsDialog:=TCompOptCondPropsDialog.Create(nil);
  try
    CompOptCondPropsDialog.Node:=Node;
    Result:=CompOptCondPropsDialog.ShowModal;
  finally
    CompOptCondPropsDialog.Free;
  end;
end;

{ TCompOptCondPropsDialog }

procedure TCompOptCondPropsDialog.FormCreate(Sender: TObject);
var
  nt: TCOCNodeType;
  vt: TCOCValueType;
begin
  Caption:='Properties of conditional compiler option';

  NodeTypeLabel.Caption:='Type:';
  ValueTypeLabel.Caption:='Action:';
  ValueLabel.Caption:='Value:';
  ValueTypeComboBox.Items.Clear;
  NodeTypeComboBox.Items.Clear;
  for nt:=Low(TCOCNodeType) to High(TCOCNodeType) do
    NodeTypeComboBox.Items.Add(COCNodeTypeLocalizedName(nt));
  for vt:=Low(TCOCValueType) to High(TCOCValueType) do
    ValueTypeComboBox.Items.Add(COCValueTypeLocalizedName(vt));
end;

procedure TCompOptCondPropsDialog.NodeTypeComboBoxEditingDone(Sender: TObject);
begin
  NodeTypeChanged;
end;

procedure TCompOptCondPropsDialog.ValueEditEditingDone(Sender: TObject);
begin

end;

procedure TCompOptCondPropsDialog.ValueTypeComboBoxEditingDone(Sender: TObject);
begin

end;

procedure TCompOptCondPropsDialog.SetNode(const AValue: TCompOptCondNode);
begin
  if FNode=AValue then exit;
  FNode:=AValue;
  NodeChanged;
end;

procedure TCompOptCondPropsDialog.NodeChanged;
var
  NodeTypeStr: String;
begin
  if Node=nil then begin
    NodeTypeLabel.Enabled:=false;
    NodeTypeComboBox.Enabled:=false;
    ValueTypeLabel.Enabled:=false;
    ValueTypeComboBox.Enabled:=false;
    ValueLabel.Enabled:=false;
    ValueEdit.Enabled:=false;
    ValueButton.Enabled:=false;
  end else begin
    NodeTypeStr:=COCNodeTypeLocalizedName(Node.NodeType);
    NodeTypeLabel.Enabled:=true;
    NodeTypeComboBox.ItemIndex:=NodeTypeComboBox.Items.IndexOf(NodeTypeStr);
    NodeTypeComboBox.Text:=NodeTypeStr;
    NodeTypeChanged;
    ValueEdit.Text:=Node.Value;
  end;
end;

procedure TCompOptCondPropsDialog.NodeTypeChanged;
var
  s: String;
begin
  if Node=nil then exit;
  ValueTypeLabel.Enabled:=Node.NodeType in [cocntAddValue];
  ValueTypeComboBox.Enabled:=ValueTypeLabel.Enabled;
  s:=COCValueTypeLocalizedName(Node.ValueType);
  ValueTypeComboBox.ItemIndex:=ValueTypeComboBox.Items.IndexOf(s);
  ValueTypeComboBox.Text:=s;
  ValueLabel.Enabled:=Node.NodeType in [cocntNone,cocntIf,
    cocntIfdef,cocntIfNdef,cocntElseIf,cocntAddValue];
  ValueEdit.Enabled:=ValueLabel.Enabled;
  ValueButton.Enabled:=ValueLabel.Enabled;
  case Node.NodeType of
  cocntNone: ValueLabel.Caption:=lisResult;
  cocntIf, cocntElseIf: ValueLabel.Caption:=lisExpression;
  cocntIfdef, cocntIfNdef: ValueLabel.Caption:=lisCodeToolsDefsVariable;
  cocntAddValue: ValueLabel.Caption:=lisAddValue;
  end;
end;

initialization
  {$I compoptcondpropsdlg.lrs}

end.

