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
    procedure ButtonPanel1OkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NodeTypeComboBoxEditingDone(Sender: TObject);
    procedure ValueEditEditingDone(Sender: TObject);
    procedure ValueTypeComboBoxEditingDone(Sender: TObject);
  private
    FAllowedValueTypes: TCOCValueTypes;
    FOriginalNode: TCompOptCondNode;
    FNodeType: TCOCNodeType;
    FValueType: TCOCValueType;
    FValue: string;
    function GetNodeType: TCOCNodeType;
    function GetValue: string;
    function GetValueType: TCOCValueType;
    procedure SetAllowedValueTypes(const AValue: TCOCValueTypes);
    procedure SetNodeType(const AValue: TCOCNodeType);
    procedure SetOriginalNode(const AValue: TCompOptCondNode);
    procedure SetValue(const AValue: string);
    procedure SetValueType(const AValue: TCOCValueType);
    procedure UpdateNodeTypeControls;
    procedure UpdateValueControls;
  public
    property OriginalNode: TCompOptCondNode read FOriginalNode write SetOriginalNode;
    property NodeType: TCOCNodeType read GetNodeType write SetNodeType;
    property ValueType: TCOCValueType read GetValueType write SetValueType;
    property Value: string read GetValue write SetValue;
    property AllowedValueTypes: TCOCValueTypes read FAllowedValueTypes write SetAllowedValueTypes;
  end;

function EditCompOptCondProperties(Node: TCompOptCondNode;
  const AllowedValueTypes: TCOCValueTypes): TModalResult;

implementation

function EditCompOptCondProperties(Node: TCompOptCondNode;
  const AllowedValueTypes: TCOCValueTypes): TModalResult;
var
  CompOptCondPropsDialog: TCompOptCondPropsDialog;
begin
  CompOptCondPropsDialog:=TCompOptCondPropsDialog.Create(nil);
  try
    CompOptCondPropsDialog.OriginalNode:=Node;
    CompOptCondPropsDialog.AllowedValueTypes:=AllowedValueTypes;
    Result:=CompOptCondPropsDialog.ShowModal;
  finally
    CompOptCondPropsDialog.Free;
  end;
end;

{ TCompOptCondPropsDialog }

procedure TCompOptCondPropsDialog.FormCreate(Sender: TObject);
var
  nt: TCOCNodeType;
begin
  Caption:=lisPropertiesOfConditionalCompilerOption;
  PropsGroupBox.Caption:=lisHlpOptsProperties;

  NodeTypeLabel.Caption:=lisUIDType;
  ValueTypeLabel.Caption:=lisAction;
  ValueLabel.Caption:=lisValue;
  NodeTypeComboBox.Items.Clear;
  for nt:=Low(TCOCNodeType) to High(TCOCNodeType) do
    NodeTypeComboBox.Items.Add(COCNodeTypeLocalizedName(nt));
  AllowedValueTypes:=[cocvtNone];
  ButtonPanel1.OKButton.OnClick:=@ButtonPanel1OkClick;
end;

procedure TCompOptCondPropsDialog.ButtonPanel1OkClick(Sender: TObject);
begin
  if FOriginalNode<>nil then begin
    FOriginalNode.NodeType:=NodeType;
    FOriginalNode.ValueType:=ValueType;
    FOriginalNode.Value:=Value;
  end;
  ModalResult:=mrOk;
end;

procedure TCompOptCondPropsDialog.NodeTypeComboBoxEditingDone(Sender: TObject);
begin
  GetNodeType;
  UpdateValueControls;
end;

procedure TCompOptCondPropsDialog.ValueEditEditingDone(Sender: TObject);
begin
  GetValue;
  UpdateValueControls;
end;

procedure TCompOptCondPropsDialog.ValueTypeComboBoxEditingDone(Sender: TObject);
begin
  GetValueType;
  UpdateValueControls;
end;

procedure TCompOptCondPropsDialog.SetOriginalNode(const AValue: TCompOptCondNode);
begin
  if FOriginalNode=AValue then exit;
  FOriginalNode:=AValue;
  if FOriginalNode<>nil then begin
    FNodeType:=FOriginalNode.NodeType;
    FValueType:=FOriginalNode.ValueType;
    FValue:=FOriginalNode.Value;
  end else begin
    FNodeType:=cocntNone;
    FValueType:=cocvtNone;
    FValue:='';
  end;
  UpdateNodeTypeControls;
end;

function TCompOptCondPropsDialog.GetNodeType: TCOCNodeType;
var
  i: LongInt;
begin
  i:=NodeTypeComboBox.Items.IndexOf(NodeTypeComboBox.Text);
  if i<0 then
    FNodeType:=cocntNone
  else
    FNodeType:=TCOCNodeType(i);
  Result:=FNodeType;
end;

function TCompOptCondPropsDialog.GetValue: string;
begin
  FValue:=ValueEdit.Text;
  Result:=FValue;
end;

function TCompOptCondPropsDialog.GetValueType: TCOCValueType;
var
  i: LongInt;
begin
  i:=ValueTypeComboBox.Items.IndexOf(ValueTypeComboBox.Text);
  if i<0 then
    FValueType:=cocvtNone
  else
    FValueType:=TCOCValueType(i);
  Result:=FValueType;
end;

procedure TCompOptCondPropsDialog.SetAllowedValueTypes(
  const AValue: TCOCValueTypes);
var
  vt: TCOCValueType;
begin
  if FAllowedValueTypes=AValue then exit;
  FAllowedValueTypes:=AValue;
  ValueTypeComboBox.Items.BeginUpdate;
  ValueTypeComboBox.Items.Clear;
  for vt:=Low(TCOCValueType) to High(TCOCValueType) do
    if vt in AllowedValueTypes then
      ValueTypeComboBox.Items.Add(COCValueTypeLocalizedName(vt));
  ValueTypeComboBox.Items.EndUpdate;
end;

procedure TCompOptCondPropsDialog.SetNodeType(const AValue: TCOCNodeType);
begin
  if FNodeType=AValue then exit;
  FNodeType:=AValue;
  UpdateNodeTypeControls;
end;

procedure TCompOptCondPropsDialog.UpdateNodeTypeControls;
var
  NodeTypeStr: String;
begin
  if OriginalNode=nil then begin
    NodeTypeLabel.Enabled:=false;
    NodeTypeComboBox.Enabled:=false;
    ValueTypeLabel.Enabled:=false;
    ValueTypeComboBox.Enabled:=false;
    ValueLabel.Enabled:=false;
    ValueEdit.Enabled:=false;
    ValueButton.Enabled:=false;
  end else begin
    NodeTypeStr:=COCNodeTypeLocalizedName(FNodeType);
    NodeTypeLabel.Enabled:=true;
    NodeTypeComboBox.ItemIndex:=NodeTypeComboBox.Items.IndexOf(NodeTypeStr);
    NodeTypeComboBox.Text:=NodeTypeStr;
    UpdateValueControls;
  end;
end;

procedure TCompOptCondPropsDialog.UpdateValueControls;
var
  s: String;
begin
  ValueTypeLabel.Enabled:=FNodeType in [cocntAddValue];
  ValueTypeComboBox.Enabled:=ValueTypeLabel.Enabled;
  s:=COCValueTypeLocalizedName(fValueType);
  ValueTypeComboBox.ItemIndex:=ValueTypeComboBox.Items.IndexOf(s);
  ValueTypeComboBox.Text:=s;
  ValueLabel.Enabled:=fNodeType in [cocntNone,cocntIf,
                cocntIfdef,cocntIfNdef,cocntElseIf,cocntAddValue,cocntSetValue];
  ValueEdit.Enabled:=ValueLabel.Enabled;
  ValueButton.Enabled:=ValueLabel.Enabled;
  case fNodeType of
  cocntNone: ValueLabel.Caption:=lisResult;
  cocntIf, cocntElseIf: ValueLabel.Caption:=lisExpression;
  cocntIfdef, cocntIfNdef: ValueLabel.Caption:=lisCodeToolsDefsVariable;
  cocntAddValue: ValueLabel.Caption:=lisAddValue;
  cocntSetValue: ValueLabel.Caption:=lisSetValue;
  else ValueLabel.Caption:='?';
  end;
  ValueEdit.Text:=FValue;
end;

procedure TCompOptCondPropsDialog.SetValue(const AValue: string);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
  ValueEdit.Text:=FValue;
end;

procedure TCompOptCondPropsDialog.SetValueType(const AValue: TCOCValueType);
begin
  if FValueType=AValue then exit;
  FValueType:=AValue;
  UpdateValueControls;
end;

initialization
  {$I compoptcondpropsdlg.lrs}

end.

