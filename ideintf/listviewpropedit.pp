{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Property editor for TListView objects

 Author: Olivier Guilbaud  (golivier@free.fr)
         Tomas Gregorovic
 
 History
   01/28/2003 OG - Create
   18/02/2003 OG - First release
   19/02/2003 OG - Add ObjInspStrConsts unit
   24/02/2003 OG - Replace TListBox with TTreeView
                   Include suItems property
   22/01/2006 TG - Dialog converted to lfm.
                   
   ToDo :
     Select the first item on show editor ... do not work :o(
}
unit ListViewPropEdit;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, Menus, PropEdits, ComponentEditors, LCLProc,
  ObjInspStrConsts;

type
  { TListViewItemsEditorForm }

  TListViewItemsEditorForm = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnApply: TBitBtn;
    BtnHelp: TBitBtn;
    BtnNewItem: TButton;
    BtnNewSubItem: TButton;
    BtnDelete: TButton;
    edtText: TEdit;
    edtIndexImg: TEdit;
    edtIndexState: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    LabelCaption: TLabel;
    LabelImageIndex: TLabel;
    LabelStateIndex: TLabel;
    BtnPanel: TPanel;
    TreeView1: TTreeView;
    procedure BtnNewItemClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TreeView1SelectionChanged(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure edtIndexStateEditingDone(Sender: TObject);
  private
    FListView: TListView;
    FModified: Boolean;
  public
    procedure LoadFromList(AListView: TListView);
    procedure SaveToList;
  end;

  { TListViewComponentEditor }

  TListViewComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {TListViewItemsPropertyEditor
   Property editor for the Items properties of TListView object.
   Brings up the dialog for editing items}
  TListViewItemsPropertyEditor = Class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;
  
implementation

{$R *.lfm}

function EditListView(AListView: TListView): Boolean;
var
  ListViewEditorDlg: TListViewItemsEditorForm;
begin
  ListViewEditorDlg := TListViewItemsEditorForm.Create(Application);
  try
    ListViewEditorDlg.LoadFromList(AListView);

    if ListViewEditorDlg.ShowModal = mrOk then
      ListViewEditorDlg.SaveToList;
      
    Result := ListViewEditorDlg.FModified;
  finally
    ListViewEditorDlg.Free;
  end;
end;

{ TListViewItemsEditorForm }

procedure TListViewItemsEditorForm.BtnNewItemClick(Sender: TObject);
var
  S: String;
begin
  S := sccsLvEdtItem + IntToStr(TreeView1.Items.Count);
  if (Sender as TComponent).Tag = 1 then
    TreeView1.Selected := TreeView1.Items.Add(nil, S)
  else
  begin
    if (TreeView1.Selected=nil) or (TreeView1.Selected.Level = 0) then
      TreeView1.Selected := TreeView1.Items.AddChild(TreeView1.Selected, S)
    else
      TreeView1.Selected := TreeView1.Items.Add(TreeView1.Selected, S);
  end;

  GroupBox2.Enabled := TreeView1.Items.Count > 0;

  edtText.SetFocus;
  edtText.SelectAll;
end;

procedure TListViewItemsEditorForm.Edit1Change(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
    TreeView1.Selected.Text := edtText.Text;
end;

procedure TListViewItemsEditorForm.FormCreate(Sender: TObject);
begin
  Caption  := sccsLvEdtCaption;
  
  GroupBox1.Caption := sccsLvEdtGrpLCaption;
  GroupBox2.Caption := sccsLvEdtGrpRCaption;

  BtnNewItem.Caption := sccsLvEdtNewItem;
  BtnNewSubItem.Caption := sccsLvEdtNewSubItem;
  BtnDelete.Caption := sccsLvEdtDelete;
  BtnApply.Caption := sccsLvEdtApply;
  
  LabelCaption.Caption := sccsLvEdtLabelCaption;
  LabelImageIndex.Caption := sccsLvEdtLabelImageIndex;
  LabelStateIndex.Caption := sccsLvEdtLabelStateIndex;
end;

procedure TListViewItemsEditorForm.TreeView1SelectionChanged(
  Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
  begin
    edtText.Text := TreeView1.Selected.Text;
    edtIndexImg.Text := IntToStr(TreeView1.Selected.ImageIndex);
    edtIndexState.Text := IntToStr(TreeView1.Selected.StateIndex);
  end;
end;

procedure TListViewItemsEditorForm.btnApplyClick(Sender: TObject);
begin
  SaveToList;
end;

procedure TListViewItemsEditorForm.btnDeleteClick(Sender: TObject);
var
  TempNode: TTreeNode;
begin
  if Assigned(TreeView1.Selected) then
  begin
    TempNode := TreeView1.Selected.GetNextSibling;
    if TempNode = nil then
      TempNode := TreeView1.Selected.GetPrevSibling;
    if TempNode = nil then
      TempNode := TreeView1.Selected.Parent;

    TreeView1.Items.Delete(TreeView1.Selected);

    if TempNode <> nil then
      TreeView1.Selected := TempNode;

    GroupBox2.Enabled := TreeView1.Items.Count > 0;
    TreeView1.SetFocus;
  end;
end;

procedure TListViewItemsEditorForm.edtIndexStateEditingDone(
  Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
  begin
    TreeView1.Selected.ImageIndex := StrToIntDef(edtIndexImg.Text, -1);
    TreeView1.Selected.StateIndex := StrToIntDef(edtIndexState.Text, -1);
    TreeView1.Selected.SelectedIndex := TreeView1.Selected.ImageIndex;

    edtIndexImg.Text := IntToStr(TreeView1.Selected.ImageIndex);
    edtIndexState.Text := IntToStr(TreeView1.Selected.StateIndex);
  end;
end;

procedure TListViewItemsEditorForm.LoadFromList(AListView: TListView);
var
  I, J: Integer;
  Node: TTreeNode;
begin
  FListView := AListView;
  if Assigned(AListView) then
  begin
    TreeView1.Images := AListView.SmallImages;
    TreeView1.StateImages := AListView.StateImages;
    
    TreeView1.Items.BeginUpdate;
    try
      TreeView1.Items.Clear;
      
      for I := 0 to AListView.Items.Count - 1 do
      begin
        Node := TreeView1.Items.Add(nil, AListView.Items[I].Caption);
        with Node do
        begin
          ImageIndex := AListView.Items[I].ImageIndex;
          StateIndex := AListView.Items[I].StateIndex;
          SelectedIndex := ImageIndex;
        end;

        //SubItems
        for J := 0 to AListView.Items[I].SubItems.Count - 1 do
        begin
          with TreeView1.Items.AddChild(Node, AListView.Items[I].SubItems[J]) do
          begin
            ImageIndex := AListView.Items[I].SubItemImages[J];
            SelectedIndex := ImageIndex;
          end;
        end;
      end;
    finally
      TreeView1.Items.EndUpdate;
    end;
  end;

  GroupBox2.Enabled := TreeView1.Items.Count > 0;
end;

procedure TListViewItemsEditorForm.SaveToList;
var
  I, J: Integer;
  Node: TTreeNode;
  Item: TListItem;
begin
  if Assigned(FListView) then
  begin
    FListView.BeginUpdate;
    try
      FListView.Items.Clear;

      //Recreate new items or modify
      for I := 0 to TreeView1.Items.Count - 1 do
      begin
        Node := TreeView1.Items[I];
        if Node.Level = 0 then
        begin
          Item := FListView.Items.Add;
          Item.Caption := Node.Text;
          Item.ImageIndex := Node.ImageIndex;
          Item.StateIndex := Node.StateIndex;

          //SubItems
          for J := 0 to Node.Count - 1 do
          begin
            Item.SubItems.Add(Node.Items[J].Text);
            Item.SubItemImages[J] := Node.Items[J].ImageIndex;
          end;
        end;
      end;
    finally
      FListView.EndUpdate;
    end;
        
    FModified := True;
  end;
end;

{ TListViewItemsPropertyEditor }

procedure TListViewItemsPropertyEditor.Edit;
begin
  if EditListView(GetComponent(0) as TListView) then Modified;
end;

function TListViewItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly, paRevertable];
end;

{ TListViewComponentEditor }

procedure TListViewComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
  AListView: TListView;
begin
  AListView := GetComponent as TListView;
  case Index of
    0: 
    begin
      GetHook(Hook);
      if EditListView(AListView) then
        if Assigned(Hook) then Hook.Modified(Self);
    end;
    1:
    begin
      GetHook(Hook);
      EditCollection(AListView, AListView.Columns, 'Columns');
      if Assigned(Hook) then Hook.Modified(Self);
    end;
  end;
end;

function TListViewComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := '';
  case Index of
    0: Result := sccsLvEdt;
    1: Result := sccsLvColEdt;
    else
      Result := '';
  end;
end;

function TListViewComponentEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

initialization
  //Register TListViewItemsPropertyEditor
  RegisterPropertyEditor(ClassTypeInfo(TListItems), TListView, 'Items',
    TListViewItemsPropertyEditor);

  //Register a component editor for TListView
  RegisterComponentEditor(TListView, TListViewComponentEditor);
  
end.
