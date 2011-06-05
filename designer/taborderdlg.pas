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
  ComCtrls, StdCtrls, Arrow, LazarusIDEStrConsts, ButtonPanel,
  PropEdits, IDEWindowIntf;

type

  { TTabOrderDialog }

  TTabOrderDialog = class(TForm)
    ArrowDown: TSpeedButton;
    ArrowUp: TSpeedButton;
    ItemTreeview: TTreeView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ItemTreeviewClick(Sender: TObject);
    procedure TabOrderDialogCREATE(Sender: TObject);
    procedure UpSpeedbuttonCLICK(Sender: TObject);
    procedure DownSpeedbuttonCLICK(Sender: TObject);
  private
    FUpdating: Boolean;
    procedure SwapNodes(ANode1, ANode2, NewSelected: TTreeNode);
    procedure CheckButtonsEnabled;
    procedure CreateNodes(ParentControl: TWinControl; ParentNode: TTreeNode);
    procedure RefreshTree;
    procedure OnSomethingChanged;
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnDeletePersistent(var APersistent: TPersistent);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
  end;

  { TTabOrderPropEditor }

  TTabOrderPropEditor = class(TIntegerPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
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
  GlobalDesignHook.AddHandlerChangeLookupRoot(@OnSomethingChanged);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnSomethingChanged);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerDeletePersistent(@OnDeletePersistent);
  GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);

  ArrowDown.LoadGlyphFromLazarusResource('arrow_down');
  ArrowUp.LoadGlyphFromLazarusResource('arrow_up');

  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TTabOrderDialog.FormShow(Sender: TObject);
var
  Sel: TPersistentSelectionList;
begin
  RefreshTree;
  Sel := TPersistentSelectionList.Create;
  try
    GlobalDesignHook.GetSelection(Sel);
    OnSetSelection(Sel);
  finally
    Sel.Free;
  end;
  CheckButtonsEnabled;
end;

procedure TTabOrderDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TTabOrderDialog.ItemTreeviewClick(Sender: TObject);
begin
  CheckButtonsEnabled;
end;

procedure TTabOrderDialog.UpSpeedbuttonCLICK(Sender: TObject);
var
  CurItem, NewItem: TTreeNode;
begin
  CurItem := ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetPrevSibling=nil) then exit;
  NewItem := CurItem.GetPrevSibling;
  SwapNodes(NewItem, CurItem, CurItem);
end;

procedure TTabOrderDialog.DownSpeedbuttonCLICK(Sender: TObject);
var
  CurItem, NewItem: TTreeNode;
begin
  CurItem:=ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetNextSibling=nil) then exit;
  NewItem := CurItem.GetNextSibling;
  SwapNodes(CurItem, NewItem, CurItem);
end;

procedure TTabOrderDialog.SwapNodes(ANode1, ANode2, NewSelected: TTreeNode);
var
  Ctrl1, Ctrl2: TWinControl;
  TabOrd: TTabOrder;
begin
  if IsVisible and not FUpdating then
  begin
    FUpdating := true;
    ItemTreeview.BeginUpdate;
    try
      ANode2.MoveTo(ANode1,naInsert);          // Move Node2 in front of Node1.
      Ctrl1 := TWinControl(ANode1.Data);
      Ctrl2 := TWinControl(ANode2.Data);
      TabOrd := Ctrl1.TabOrder;                // Swap TabOrder values.
      Ctrl1.TabOrder := Ctrl2.TabOrder;
      Ctrl2.TabOrder := TabOrd;
      ANode1.Text := Ctrl1.Name + '   (' + IntToStr(Ctrl1.TabOrder) + ')';
      ANode2.Text := Ctrl2.Name + '   (' + IntToStr(Ctrl2.TabOrder) + ')';
      ItemTreeview.Selected := NewSelected;
      GlobalDesignHook.Modified(Self);
      GlobalDesignHook.RefreshPropertyValues;
      CheckButtonsEnabled;
    finally
      ItemTreeview.EndUpdate;
      FUpdating := false;
    end;
  end;
end;

procedure TTabOrderDialog.CheckButtonsEnabled;
var
  CurItem: TTreeNode;
begin
  CurItem := ItemTreeview.Selected;
  ArrowUp.Enabled   := Assigned(CurItem) and Assigned(CurItem.GetPrevSibling);
  ArrowDown.Enabled := Assigned(CurItem) and Assigned(CurItem.GetNextSibling);
end;

procedure TTabOrderDialog.CreateNodes(ParentControl: TWinControl; ParentNode: TTreeNode);
// Add all controls in Designer to ItemTreeview.
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
    // skip non TWinControls and invisible form designer controls
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

procedure TTabOrderDialog.RefreshTree;
var
  LookupRoot: TPersistent;
begin
  if IsVisible and not FUpdating then
  begin
    FUpdating := true;
    ItemTreeview.BeginUpdate;
    try
      ItemTreeview.Items.Clear;
      LookupRoot := GlobalDesignHook.LookupRoot;
      if Assigned(LookupRoot) and (LookupRoot is TWinControl) then begin
        CreateNodes(TWinControl(LookupRoot), nil);
        Caption := Format(lisTabOrderOf, [TWinControl(LookupRoot).Name]);
      end;
    finally
      ItemTreeview.EndUpdate;
      FUpdating := false;
    end;
  end;
end;

procedure TTabOrderDialog.OnSomethingChanged;
begin
  RefreshTree;
  CheckButtonsEnabled;
end;

procedure TTabOrderDialog.OnPersistentAdded(APersistent: TPersistent; Select: boolean);
begin
  OnSomethingChanged;
end;

procedure TTabOrderDialog.OnPersistentDeleting(APersistent: TPersistent);
begin
  OnSomethingChanged;
end;

procedure TTabOrderDialog.OnDeletePersistent(var APersistent: TPersistent);
begin
  ShowMessage('TTabOrderDialog.OnDeletePersistent is never called for some reason!');
  OnSomethingChanged;
end;

procedure TTabOrderDialog.OnSetSelection(const ASelection: TPersistentSelectionList);
// Select item also in TreeView when selection in Designer changes.
var
  Ctrl: TPersistent;
  Node: TTreeNode;
begin
  // ToDo: support also multiply selections.
  ItemTreeview.BeginUpdate;
  Node := ItemTreeview.Items.GetFirstNode;
  while Assigned(Node) do begin
    if Assigned(Node.Data) then begin
      Ctrl := TPersistent(Node.Data);
      Assert(Ctrl is TWinControl);
      if ASelection.IndexOf(Ctrl) >= 0 then begin
        ItemTreeview.Selected := Node;
        Break;
      end;
    end;
    Node:=Node.GetNext;
  end;
  ItemTreeview.EndUpdate;
  CheckButtonsEnabled;
end;

{ TTabOrderPropEditor }

function TTabOrderPropEditor.GetAttributes: TPropertyAttributes;
begin
  Result:=(inherited GetAttributes)+[paDialog];
end;

procedure TTabOrderPropEditor.Edit;
begin
  ShowTabOrderEditor(Self);
end;

initialization
  RegisterPropertyEditor(TypeInfo(TTabOrder), TControl, 'TabOrder', TTabOrderPropEditor);

end.

