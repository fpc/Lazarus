{ Copyright (C) 2005

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Lagunov Aleksey

  Abstract:
    Property Editors for TTreeView.
}

unit TreeViewPropEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  PropEdits, Componenteditors, StdCtrls, ComCtrls;

type

  { TTreeViewItemsEditorForm }

  TTreeViewItemsEditorForm = class(TForm)
    btnSave: TButton;
    Button1: TButton;
    Button2: TButton;
    btnApply: TButton;
    Button4: TButton;
    BtnNewItem: TButton;
    Button6: TButton;
    btnDelete: TButton;
    btnLoad: TButton;
    edtText: TEdit;
    edtIndexImg: TEdit;
    edtIndexSel: TEdit;
    edtIndexState: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    TreeView1: TTreeView;
    procedure BtnNewItemClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FTreeView:TTreeView;
    FModified:boolean;
    procedure UpdateState;
    procedure LoadFromTree(ATreeView:TTreeView);
    procedure SaveToTree;
  public
    { public declarations }
  end; 


type
  TTreeViewItemsProperty = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function  GetAttributes: TPropertyAttributes; override;
  end;

  TTreeViewComponentEditor = class(TDefaultComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

function EditTreeView(ATreeView: TTreeView):boolean;
var
  TreeViewItemsEditorForm: TTreeViewItemsEditorForm;
begin
  TreeViewItemsEditorForm:=TTreeViewItemsEditorForm.Create(Application);
  try
    TreeViewItemsEditorForm.LoadFromTree(ATreeView);
    if TreeViewItemsEditorForm.ShowModal = mrOk then
      TreeViewItemsEditorForm.SaveToTree;
    Result:=TreeViewItemsEditorForm.FModified;
  finally
    TreeViewItemsEditorForm.Free;
  end;
end;

{ TTreeViewItemsEditorForm }

procedure TTreeViewItemsEditorForm.BtnNewItemClick(Sender: TObject);
var
  S:string;
begin
  S:='Item_'+IntToStr(TreeView1.Items.Count);
  if (Sender as TComponent).Tag = 1 then
    TreeView1.Items.Add(TreeView1.Selected, S)
  else
  TreeView1.Items.AddChild(TreeView1.Selected, S);
end;

procedure TTreeViewItemsEditorForm.Edit1Change(Sender: TObject);
begin
  UpdateState;
end;

procedure TTreeViewItemsEditorForm.TreeView1Click(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
  begin
    edtText.Text:=TreeView1.Selected.Text;
    edtIndexImg.Text:=IntToStr(TreeView1.Selected.ImageIndex);
    edtIndexSel.Text:=IntToStr(TreeView1.Selected.SelectedIndex);
    edtIndexState.Text:=IntToStr(TreeView1.Selected.StateIndex);
  end;
end;

procedure TTreeViewItemsEditorForm.btnApplyClick(Sender: TObject);
begin
  SaveToTree;
end;

procedure TTreeViewItemsEditorForm.btnDeleteClick(Sender: TObject);
begin
  if Assigned(TreeView1.Selected) then
    TreeView1.Items.Delete(TreeView1.Selected);
end;

procedure TTreeViewItemsEditorForm.btnLoadClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    TreeView1.LoadFromFile(OpenDialog1.FileName);
end;

procedure TTreeViewItemsEditorForm.btnSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    TreeView1.SaveToFile(SaveDialog1.FileName);
end;

procedure TTreeViewItemsEditorForm.UpdateState;
begin
  if Assigned(TreeView1.Selected) then
  begin
    TreeView1.Selected.Text:=edtText.Text;
    TreeView1.Selected.ImageIndex:=StrToIntDef(edtIndexImg.Text, -1);
    TreeView1.Selected.SelectedIndex:=StrToIntDef(edtIndexSel.Text, -1);
    TreeView1.Selected.StateIndex:=StrToIntDef(edtIndexState.Text, -1);
  end;
end;

procedure TTreeViewItemsEditorForm.LoadFromTree(ATreeView: TTreeView);
var
  S:TMemoryStream;
begin
  FTreeView:=ATreeView;
  if Assigned(ATreeView) then
  begin
    S:=TMemoryStream.Create;
    try
      TreeView1.Images:=ATreeView.Images;
      TreeView1.StateImages:=ATreeView.StateImages;
      ATreeView.SaveToStream(S);
      S.Seek(0, soFromBeginning);
      TreeView1.LoadFromStream(S);
    finally
      S.Free;
    end;
  end;
end;

procedure TTreeViewItemsEditorForm.SaveToTree;
var
  S:TMemoryStream;
begin
  if Assigned(FTreeView) then
  begin
    S:=TMemoryStream.Create;
    try
      TreeView1.SaveToStream(S);
      S.Seek(0, soFromBeginning);
      FTreeView.LoadFromStream(S);
      FModified:=true;
    finally
      S.Free;
    end;
  end
end;


{ TTreeViewItemsProperty }

procedure TTreeViewItemsProperty.Edit;
begin
  if EditTreeView(GetComponent(0) as TTreeView) then
    Modified;
end;

function TTreeViewItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog,paReadOnly,paRevertable];
end;


{ TTreeViewComponentEditor }
procedure TTreeViewComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook    : TPropertyEditorHook;
begin
  If Index=0 then
  begin
    GetHook(Hook);
    if EditTreeView(GetComponent as TTreeView) then
      if Assigned(Hook) then
        Hook.Modified(Self);
  end;
end;

function TTreeViewComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:='';
  If Index=0 then
    Result:='Edit TreeView';
end;

function TTreeViewComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

initialization
  {$I treeviewpropedit.lrs}

  RegisterPropertyEditor(ClassTypeInfo(TTreeNodes), TTreeView, 'Items', TTreeViewItemsProperty);
  RegisterComponentEditor(TTreeView,TTreeViewComponentEditor);
end.

