{
 /***************************************************************************
                             taborderdlg.pas
                             ---------------

 ***************************************************************************/

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
}

unit TabOrderDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs, Buttons,
  ComCtrls, StdCtrls, Arrow, LazarusIDEStrConsts, ButtonPanel, PropEdits;

type

  { TTabOrderDialog }

  TTabOrderDialog = class(TForm)
    ArrowDown: TSpeedButton;
    ArrowUp: TSpeedButton;
    ItemTreeview: TTreeView;
    procedure TabOrderDialogCREATE(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpSpeedbuttonCLICK(Sender: TObject);
    procedure DownSpeedbuttonCLICK(Sender: TObject);
  private
    FUpdating: Boolean;
    function SwapNodes(ANode1, ANode2: TTreeNode): boolean;
    procedure FillTree;
    procedure CreateNodes(ParentControl: TWinControl; ParentNode: TTreeNode);
    procedure Refresh(Force: boolean);
    procedure OnRefreshPropertyValues;
  end;

  { TTabOrderPropEditor }

  TTabOrderPropEditor = class(TIntegerPropertyEditor)
  public
    function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure SetValue(const NewValue: ansistring);  override;
    procedure Edit; override;
  end;

var
  TabOrderDialog: TTabOrderDialog;
  ShowTabOrderEditor: TNotifyEvent;

implementation

{$R *.lfm}

{ TTabOrderDialog }

procedure TTabOrderDialog.TabOrderDialogCREATE(Sender: TObject);
begin
  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
  ArrowDown.LoadGlyphFromLazarusResource('arrow_down');
  ArrowUp.LoadGlyphFromLazarusResource('arrow_up');
end;

procedure TTabOrderDialog.FormShow(Sender: TObject);
begin
  Refresh(true);
end;

procedure TTabOrderDialog.UpSpeedbuttonCLICK(Sender: TObject);
var
  CurItem, NewItem: TTreeNode;
begin
  CurItem:=ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetPrevSibling=nil) then exit;
  NewItem := CurItem.GetPrevSibling;
  SwapNodes(NewItem, CurItem);
  ItemTreeview.Selected:=CurItem;
end;

procedure TTabOrderDialog.DownSpeedbuttonCLICK(Sender: TObject);
var
  CurItem, NewItem: TTreeNode;
begin
  CurItem:=ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetNextSibling=nil) then exit;
  NewItem := CurItem.GetNextSibling;
  SwapNodes(CurItem, NewItem);
  ItemTreeview.Selected:=CurItem;
end;

function TTabOrderDialog.SwapNodes(ANode1, ANode2: TTreeNode): boolean;
var
  Ctrl1, Ctrl2: TWinControl;
  TabOrd: TTabOrder;
begin
  ANode2.MoveTo(ANode1,naInsert);          // Move Node2 in front of Node1.
  Ctrl1 := TWinControl(ANode1.Data);
  Ctrl2 := TWinControl(ANode2.Data);
  TabOrd := Ctrl1.TabOrder;                // Swap TabOrder values.
  Ctrl1.TabOrder := Ctrl2.TabOrder;
  Ctrl2.TabOrder := TabOrd;
  ANode1.Text := Ctrl1.Name + '   (' + IntToStr(Ctrl1.TabOrder) + ')';
  ANode2.Text := Ctrl2.Name + '   (' + IntToStr(Ctrl2.TabOrder) + ')';
end;

procedure TTabOrderDialog.FillTree;
var
  LookupRoot: TPersistent;
begin
  ItemTreeview.BeginUpdate;
  try
    ItemTreeview.Items.Clear;
    LookupRoot := GlobalDesignHook.LookupRoot;
    if Assigned(LookupRoot) and (LookupRoot is TWinControl) then begin
      CreateNodes(TWinControl(LookupRoot), nil);
      Caption:=lisTabOrderOf + ' ' + TWinControl(LookupRoot).Name;
    end;
  finally
    ItemTreeview.EndUpdate;
  end;
end;

procedure TTabOrderDialog.CreateNodes(ParentControl: TWinControl; ParentNode: TTreeNode);
var
  AControl: TControl;
  i, CurTab: integer;
  FirstSibling: TTreeNode;
  NodeBehind: TTreeNode;
  NewNode: TTreeNode;
  NodeText: string;
  AWinControl: TWinControl;
begin
  ItemTreeview.BeginUpdate;
  if ParentNode = nil then
    FirstSibling := nil
  else
    FirstSibling := ParentNode.GetFirstChild;
  for i := 0 to ParentControl.ControlCount - 1 do
  begin
    AControl := ParentControl.Controls[i];
    // skip non TWinControls and ivisible for designer controls
    if not (AControl is TWinControl) or (csNoDesignVisible in AControl.ControlStyle) then
      continue;
    AWinControl := TWinControl(AControl);
    CurTab      := AWinControl.TabOrder;
    NodeBehind  := FirstSibling;
    while (NodeBehind <> nil) and (TWinControl(NodeBehind.Data).TabOrder <= CurTab) do
      NodeBehind := NodeBehind.GetNextSibling;
    NodeText := AWinControl.Name + '   (' + IntToStr(AWinControl.TabOrder) + ')';
    if NodeBehind <> nil then
      NewNode := ItemTreeview.Items.InsertObject(NodeBehind, NodeText, AControl)
    else
      NewNode := ItemTreeview.Items.AddChildObject(ParentNode, NodeText, AControl);
    if (FirstSibling = nil) or (NewNode.GetPrevSibling = nil) then
      FirstSibling := NewNode;
    CreateNodes(AWinControl, NewNode);
    NewNode.Expanded := True;
  end;
  ItemTreeview.EndUpdate;
end;

procedure TTabOrderDialog.Refresh(Force: boolean);
begin
  if Force or IsVisible and not FUpdating then
  begin
    FUpdating:=true;
    try
      FillTree;
    finally
      FUpdating:=false;
    end;
  end;
end;

procedure TTabOrderDialog.OnRefreshPropertyValues;
begin
  Refresh(false);
end;

{ TTabOrderPropEditor }

function TTabOrderPropEditor.OrdValueToVisualValue(OrdValue: longint): string;
begin
  Result:=inherited OrdValueToVisualValue(OrdValue);
end;

procedure TTabOrderPropEditor.SetValue(const NewValue: ansistring);
begin
  inherited SetValue(NewValue);
end;

procedure TTabOrderPropEditor.Edit;
begin
  ShowTabOrderEditor(Self);
end;

initialization
  RegisterPropertyEditor(TypeInfo(Integer), TControl, 'TabOrder', TTabOrderPropEditor);

end.
