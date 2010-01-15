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
    This unit defines a dialog for the lazarus options.
}
unit IdeOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, ComCtrls, LCLProc, LCLType,
  Buttons, ButtonPanel,
  EnvironmentOpts, LazarusIDEStrConsts, IDEWindowIntf, IDEOptionsIntf,
  EditorOptions, IDECommands;

type
  { TIDEOptionsDialog }

  TIDEOptionsDialog = class(TAbstractOptionsEditorDialog)
    ButtonPanel: TButtonPanel;
    CategoryTree: TTreeView;

    procedure CategoryTreeChange(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnLoadOptions: TOnLoadIDEOptions;
    FOnSaveOptions: TOnSaveIDEOptions;
    FOptionsFilter: TAbstractIDEOptionsClass;
    PrevEditor: TAbstractIDEOptionsEditor;
    FEditorToOpen: TAbstractIDEOptionsEditorClass;
    FEditorsCreated: Boolean;

    function CheckValues: boolean;
    procedure DoOpenEditor;
    procedure LoadIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure CreateEditors;
    function SearchEditorNode(AEditor: TAbstractIDEOptionsEditorClass): TTreeNode;
    function PassesFilter(ARec: PIDEOptionsGroupRec): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function ShowModal: Integer; override;

    function AddButton: TBitBtn; override;
    function AddControl(AControlClass: TControlClass): TControl; override;
    procedure OpenEditor(AEditor: TAbstractIDEOptionsEditorClass); override;
    function FindEditor(AEditor: TAbstractIDEOptionsEditorClass): TAbstractIDEOptionsEditor; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions);
    procedure WriteSettings(AOptions: TAbstractIDEOptions);
    procedure ReadAll;
    procedure WriteAll;

    property OptionsFilter: TAbstractIDEOptionsClass read FOptionsFilter write FOptionsFilter;
    property OnLoadIDEOptions: TOnLoadIDEOptions read FOnLoadOptions write FOnLoadOptions;
    property OnSaveIDEOptions: TOnSaveIDEOptions read FOnSaveOptions write FOnSaveOptions;
  end;

implementation

{$R *.lfm}

uses
  IDEContextHelpEdit;

{ TIDEOptionsDialog }

constructor TIDEOptionsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PrevEditor := nil;
  FEditorToOpen := nil;
  FEditorsCreated := False;

  IDEDialogLayoutList.ApplyLayout(Self, Width, Height);
  Caption := dlgIDEOptions;
  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.OKButton.ModalResult := mrNone;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;
end;

procedure TIDEOptionsDialog.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

procedure TIDEOptionsDialog.CategoryTreeChange(Sender: TObject;
  Node: TTreeNode);
var
  AEditor: TAbstractIDEOptionsEditor;
begin
  while Node <> nil do
  begin
    if Node.Data <> nil then
      break;
    Node := Node.GetFirstChild;
  end;

  AEditor := TAbstractIDEOptionsEditor(Node.Data);
  if (AEditor <> nil) and (AEditor <> PrevEditor) then
  begin
    if PrevEditor <> nil then
      PrevEditor.Visible := False;
      //PrevEditor.Parent := nil;

    AEditor.Anchors := [akLeft, akTop, akRight, akBottom];
    AEditor.AnchorSideLeft.Side := asrBottom;
    AEditor.AnchorSideLeft.Control := CategoryTree;
    AEditor.AnchorSideTop.Control := Self;
    AEditor.AnchorSideRight.Side := asrBottom;
    AEditor.AnchorSideRight.Control := Self;
    AEditor.AnchorSideBottom.Side := asrTop;
    AEditor.AnchorSideBottom.Control := CategoryTree.AnchorSide[akBottom].Control;
    AEditor.BorderSpacing.Around := 6;
    //AEditor.Parent := Self;
    AEditor.Visible := True;

    PrevEditor := AEditor;
  end;
end;

procedure TIDEOptionsDialog.CategoryTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Command: Word;
begin
  Command := EditorOpts.KeyMap.TranslateKey(Key,Shift,nil);
  if (Command=ecContextHelp) and (PrevEditor <> nil) then begin
    Key:=VK_UNKNOWN;
    ShowContextHelpForIDE(PrevEditor);
  end;
end;

procedure TIDEOptionsDialog.FormShow(Sender: TObject);
begin
  // make the category visible in the treeview
  if (CategoryTree.Selected<>nil) and (CategoryTree.Selected.Parent<>nil) then
    CategoryTree.TopItem:=CategoryTree.Selected.Parent;
end;

procedure TIDEOptionsDialog.OkButtonClick(Sender: TObject);
begin
  if not CheckValues then
    Exit;
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOk;
end;

procedure TIDEOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TIDEOptionsDialog.ReadSettings(AOptions: TAbstractIDEOptions);
var
  ClassTypeForCompare: TClass;

  procedure Traverse(Node: TTreeNode);
  begin
    if Node <> nil then
    begin
      if Node.Data <> nil then
        with TAbstractIDEOptionsEditor(Node.Data) do
          if ((ClassTypeForCompare = nil) and (SupportedOptionsClass = nil)) or
             ClassTypeForCompare.InheritsFrom(SupportedOptionsClass) then
            ReadSettings(AOptions);
      Traverse(Node.GetFirstChild);
      Traverse(Node.GetNextSibling);
    end;
  end;

begin
  CreateEditors;
  if AOptions <> nil then
    ClassTypeForCompare := AOptions.ClassType
  else
    ClassTypeForCompare := nil;

  Traverse(CategoryTree.Items.GetFirstNode);
end;

procedure TIDEOptionsDialog.WriteSettings(AOptions: TAbstractIDEOptions);
var
  ClassTypeForCompare: TClass;

  procedure Traverse(Node: TTreeNode);
  begin
    if Node <> nil then
    begin
      if Node.Data <> nil then
        with TAbstractIDEOptionsEditor(Node.Data) do
          if ((ClassTypeForCompare = nil) and (SupportedOptionsClass = nil)) or
             ClassTypeForCompare.InheritsFrom(SupportedOptionsClass) then
            WriteSettings(AOptions);
      Traverse(Node.GetFirstChild);
      Traverse(Node.GetNextSibling);
    end;
  end;

begin
  if AOptions <> nil then
    ClassTypeForCompare := AOptions.ClassType
  else
    ClassTypeForCompare := nil;

  Traverse(CategoryTree.Items.GetFirstNode);
end;

procedure TIDEOptionsDialog.ReadAll;
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
begin
  for i := 0 to IDEEditorGroups.Count - 1 do
  begin
    Rec := IDEEditorGroups[i];
    if not PassesFilter(Rec) then
      Continue;
    if Rec^.Items <> nil then
    begin
      if Rec^.GroupClass <> nil then
      begin
        Instance := Rec^.GroupClass.GetInstance;
        if Instance <> nil then
        begin
          Instance.DoBeforeRead;
          ReadSettings(Instance);
          Instance.DoAfterRead;
        end;
      end;
    end;
  end;
  // load settings that does not belong to any group
  ReadSettings(nil);
end;

procedure TIDEOptionsDialog.WriteAll;
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
begin
  for i := 0 to IDEEditorGroups.Count - 1 do
  begin
    Rec := IDEEditorGroups[i];
    if not PassesFilter(Rec) then
      Continue;
    if Rec^.Items <> nil then
    begin
      if Rec^.GroupClass <> nil then
      begin
        Instance := Rec^.GroupClass.GetInstance;
        if Instance <> nil then
        begin
          Instance.DoBeforeWrite;
          WriteSettings(Instance);
          Instance.DoAfterWrite;
        end;
      end;
    end;
  end;
  // save settings that does not belong to any group
  WriteSettings(nil);
end;

function TIDEOptionsDialog.CheckValues: boolean;

  function Traverse(Node: TTreeNode): Boolean;
  begin
    if Node <> nil then
    begin
      if Node.Data <> nil then
        Result := TAbstractIDEOptionsEditor(Node.Data).Check
      else
        Result := True;

      Result := Result and
                Traverse(Node.GetFirstChild) and
                Traverse(Node.GetNextSibling);
    end
    else
      Result := True;
  end;

begin
  Result := Traverse(CategoryTree.Items.GetFirstNode);
end;

procedure TIDEOptionsDialog.LoadIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
begin
  if Assigned(OnLoadIDEOptions) then
    OnLoadIDEOptions(Self, AOptions);
  ReadSettings(AOptions);
end;

procedure TIDEOptionsDialog.SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
begin
  WriteSettings(AOptions);
  if Assigned(OnSaveIDEOptions) then
    OnSaveIDEOptions(Self, AOptions);
end;

procedure TIDEOptionsDialog.CreateEditors;

  function SearchNode(Node: TTreeNode; Index: Integer): TTreeNode;
  begin
    Result := nil;
    if Node =  nil then
      Exit;

    if (Node.Data <> nil) and (TAbstractIDEOptionsEditor(Node.Data).Tag = Index) then
      Result := Node;

    if Result <> nil then
      Exit;

    Result := SearchNode(Node.GetFirstChild, Index);
    if Result <> nil then
      Exit;
    Result := SearchNode(Node.GetNextSibling, Index);
  end;

var
  Instance: TAbstractIDEOptionsEditor;
  GroupNode, ItemNode, ItemParent: TTreeNode;
  i, j: integer;
  Rec: PIDEOptionsGroupRec;
  ACaption: string;
begin
  if FEditorsCreated then
    Exit;
  FEditorsCreated := True;
  IDEEditorGroups.Resort;

  for i := 0 to IDEEditorGroups.Count - 1 do
  begin
    Rec := IDEEditorGroups[i];
    //DebugLn(['TIDEOptionsDialog.CreateEditors ',Rec^.GroupClass.ClassName]);
    if not PassesFilter(Rec) then
      Continue;
    if Rec^.Items <> nil then
    begin
      if Rec^.GroupClass<>nil then
        ACaption := Rec^.GroupClass.GetGroupCaption
      else
        ACaption := format('Group<%d>',[i]);
      GroupNode := CategoryTree.Items.AddChild(nil, ACaption);
      for j := 0 to Rec^.Items.Count - 1 do
      begin
        Instance := Rec^.Items[j]^.EditorClass.Create(Self);
        Instance.OnLoadIDEOptions := @LoadIDEOptions;
        Instance.OnSaveIDEOptions := @SaveIDEOptions;
        Instance.Setup(Self);
        Instance.Tag := Rec^.Items[j]^.Index;
        Instance.Visible := False;
        Instance.Parent := Self;

        if Rec^.Items[j]^.Parent = NoParent then
          ItemParent := GroupNode
        else
        begin
          ItemParent := SearchNode(GroupNode.GetFirstChild, Rec^.Items[j]^.Parent);
          if ItemParent = nil then
            ItemParent := GroupNode;
        end;

        ItemNode := CategoryTree.Items.AddChild(ItemParent, Instance.GetTitle);
        ItemNode.Data := Instance;

        ItemParent.Expanded := True;
      end;
      GroupNode.Expanded := True;
    end;
  end;
end;

function TIDEOptionsDialog.SearchEditorNode(AEditor: TAbstractIDEOptionsEditorClass): TTreeNode;

  function Traverse(ANode: TTreeNode): TTreeNode;
  begin
    Result := nil;
    if ANode <> nil then
    begin
      if (ANode.Data <> nil) and (TObject(ANode.Data).ClassType = AEditor) then
        Result := ANode;
      if Result = nil then
        Result := Traverse(ANode.GetFirstChild);
      if Result = nil then
        Result := Traverse(ANode.GetNextSibling);
    end;
  end;

begin
  Result := Traverse(CategoryTree.Items.GetFirstNode);
end;

function TIDEOptionsDialog.PassesFilter(ARec: PIDEOptionsGroupRec): Boolean;
begin
  if (ARec^.GroupClass = nil) and (OptionsFilter <> nil) then
    Exit(False);
  if (OptionsFilter<>nil) and (ARec^.GroupClass <> nil)
  and (not ARec^.GroupClass.InheritsFrom(OptionsFilter)) then
    Exit(False);
  Result := True;
end;

procedure TIDEOptionsDialog.DoOpenEditor;
var
  Node: TTreeNode;
begin
  if FEditorToOpen = nil then
    Node := CategoryTree.Items.GetFirstNode
  else
    Node := SearchEditorNode(FEditorToOpen);
  if Node <> nil then
    CategoryTree.Selected := Node;
end;

function TIDEOptionsDialog.ShowModal: Integer;
begin
  CreateEditors;
  DoOpenEditor;
  Result := inherited ShowModal;
end;

function TIDEOptionsDialog.AddButton: TBitBtn;
begin
  Result := TBitBtn.Create(Self);
  Result.Align := alCustom;
  Result.Parent := ButtonPanel;
end;

function TIDEOptionsDialog.AddControl(AControlClass: TControlClass): TControl;
var
  Control: TControl;
begin
  Result := AControlClass.Create(Self);
  Result.Parent := Self;
  Result.Anchors := [akLeft, akBottom];
  Result.BorderSpacing.Around := 6;
  Control := CategoryTree;
  while Control.AnchorSide[akBottom].Control <> ButtonPanel do
    Control := Control.AnchorSide[akBottom].Control;
  Result.AnchorSide[akBottom].Control := ButtonPanel;
  Result.AnchorSide[akLeft].Control := Self;
  Control.AnchorSide[akBottom].Control := Result;
end;

procedure TIDEOptionsDialog.OpenEditor(AEditor: TAbstractIDEOptionsEditorClass);
begin
  FEditorToOpen := AEditor;
  if Visible then
    DoOpenEditor;
end;

function TIDEOptionsDialog.FindEditor(AEditor: TAbstractIDEOptionsEditorClass): TAbstractIDEOptionsEditor;
var
  Node: TTreeNode;
begin
  Node := SearchEditorNode(AEditor);
  if Node <> nil then
    Result := TAbstractIDEOptionsEditor(Node.Data)
  else
    Result := nil;
end;

end.

