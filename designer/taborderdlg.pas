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
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  Buttons, ComCtrls, StdCtrls, Arrow, LazarusIDEStrConsts, ButtonPanel;

type

  { TTabOrderDialog }

  TTabOrderDialog = class(TForm)
    ArrowDown: TSpeedButton;
    ArrowUp: TSpeedButton;
    BtnPanel: TButtonPanel;
    OkButton:     TBitBtn;
    ShowOldValuesCheckbox: TCheckBox;
    ItemTreeview: TTreeView;
    procedure DownSpeedbuttonCLICK(Sender: TObject);
    procedure OkButtonCLICK(Sender: TObject);
    procedure ShowOldValuesCheckboxCLICK(Sender: TObject);
    procedure TabOrderDialogCLOSE(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure TabOrderDialogCREATE(Sender: TObject);
    procedure UpSpeedbuttonCLICK(Sender: TObject);
  private
    FLookupRoot: TComponent;
    procedure SetLookupRoot(const AValue: TComponent);
    procedure CommitNodes(ANode: TTreeNode; var TabChanged: boolean);
    procedure CreateNodes(ParentControl: TWinControl; ParentNode: TTreeNode);
  public
    procedure FillTree;
    procedure ClearTree;
  public
    property LookupRoot: TComponent Read FLookupRoot Write SetLookupRoot;
  end;

function ShowTabOrderDialog(LookupRoot: TComponent): TModalresult;

implementation

{$R *.lfm}

function ShowTabOrderDialog(LookupRoot: TComponent): TModalresult;
var
  TabOrderDialog: TTabOrderDialog;
begin
  TabOrderDialog := TTabOrderDialog.Create(nil);
  TabOrderDialog.LookupRoot := LookupRoot;
  Result := TabOrderDialog.ShowModal;
  TabOrderDialog.Free;
end;

{ TTabOrderDialog }

procedure TTabOrderDialog.TabOrderDialogCREATE(Sender: TObject);
begin
  ShowOldValuesCheckbox.Caption := lisShowOldTabOrder;
  BtnPanel.OKButton.OnClick := @OkButtonCLICK;

  ArrowDown.LoadGlyphFromLazarusResource('arrow_down');
  ArrowUp.LoadGlyphFromLazarusResource('arrow_up');
end;

procedure TTabOrderDialog.UpSpeedbuttonCLICK(Sender: TObject);
var
  CurItem: TTreeNode;
begin
  CurItem:=ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetPrevSibling=nil) then exit;
  CurItem.MoveTo(CurItem.GetPrevSibling,naInsert);
  ItemTreeview.Selected:=CurItem;
end;

procedure TTabOrderDialog.TabOrderDialogCLOSE(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  FLookupRoot:=nil;
end;

procedure TTabOrderDialog.ShowOldValuesCheckboxCLICK(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to Pred(ItemTreeView.Items.Count) do
    if Assigned(ItemTreeView.Items[i].Data) then
      if ShowOldValuesCheckbox.Checked then
        ItemTreeView.Items[i].Text :=
          TWinControl(ItemTreeView.Items[i].Data).Name +
          '   (' + IntToStr(TWinControl(ItemTreeView.Items[i].Data).TabOrder) + ')'
      else
        ItemTreeView.Items[i].Text := TWinControl(ItemTreeView.Items[i].Data).Name;
end;

procedure TTabOrderDialog.DownSpeedbuttonCLICK(Sender: TObject);
var
  CurItem: TTreeNode;
begin
  CurItem:=ItemTreeview.Selected;
  if (CurItem=nil) or (CurItem.GetNextSibling=nil) then exit;
  CurItem.MoveTo(CurItem.GetNextSibling,naInsertBehind);
  ItemTreeview.Selected:=CurItem;
end;

procedure TTabOrderDialog.OkButtonCLICK(Sender: TObject);
var
  TabChanged: Boolean;
begin
  TabChanged:=false;
  CommitNodes(ItemTreeview.Items.GetFirstNode,TabChanged);
  if TabChanged then
    ModalResult:=mrOk
  else
    ModalResult:=mrCancel;
end;

procedure TTabOrderDialog.SetLookupRoot(const AValue: TComponent);
begin
  if FLookupRoot=AValue then exit;
  FLookupRoot:=AValue;
  if FLookupRoot<>nil then begin
    Caption:=lisTabOrderOf + ' ' + FLookupRoot.Name;
  end;
  FillTree;
end;

procedure TTabOrderDialog.CommitNodes(ANode: TTreeNode;
  var TabChanged: boolean);
var
  AControl: TWinControl;
  CurTabOrder: Integer;
begin
  CurTabOrder := 0;
  while ANode <> nil do
  begin
    AControl := TWinControl(ANode.Data);
    if AControl.TabOrder <> CurTabOrder then
      TabChanged := True;
    AControl.TabOrder := TTabOrder(CurTabOrder);
    //DebugLn('TTabOrderDialog.CommitNodes A ',AControl.Name,' ',
    //  IntToStr(AControl.TabOrder),' ',IntToStr(CurTabOrder));
    inc(CurTabOrder);
    CommitNodes(ANode.GetFirstChild, TabChanged);
    ANode := ANode.GetNextSibling;
  end;
end;

procedure TTabOrderDialog.FillTree;
var
  AControl: TWinControl;
begin
  ItemTreeview.BeginUpdate;
  try
    ClearTree;
    if (FLookupRoot = nil) or (not (FLookupRoot is TWinControl)) then
      exit;
    AControl := TWinControl(FLookupRoot);
    CreateNodes(AControl, nil);
  finally
    ItemTreeview.EndUpdate;
  end;
end;

procedure TTabOrderDialog.ClearTree;
begin
  ItemTreeview.Items.Clear;
end;

procedure TTabOrderDialog.CreateNodes(ParentControl: TWinControl;
  ParentNode: TTreeNode);
var
  i:      integer;
  AControl: TControl;
  CurTab: integer;
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
    NodeText := AWinControl.Name;
    if ShowOldValuesCheckbox.Checked then
      NodeText := NodeText + '   (' + IntToStr(AWinControl.TabOrder) + ')';
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

end.
