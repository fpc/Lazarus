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
  Buttons, ButtonPanel, ExtCtrls,
  EnvironmentOpts, LazarusIDEStrConsts, IDEWindowIntf, IDEOptionsIntf,
  EditorOptions, IDECommands;

type
  TIDEOptsDlgAction = (
    iodaRead,
    iodaWrite,
    iodaRestore
    );

  { TIDEOptionsDialog }

  TIDEOptionsDialog = class(TAbstractOptionsEditorDialog)
    ButtonPanel: TButtonPanel;
    CategoryTree: TTreeView;
    CatTVSplitter: TSplitter;
    procedure CategoryTreeChange(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeCollapsed(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeExpanded(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure IDEOptionsDialogKeyPress(Sender: TObject; var Key: char);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnLoadOptions: TOnLoadIDEOptions;
    FOnSaveOptions: TOnSaveIDEOptions;
    FOptionsFilter: TAbstractIDEOptionsClass;
    PrevEditor: TAbstractIDEOptionsEditor;
    FEditorsCreated: Boolean;
    SelectNode: TTreeNode;
    NewLastSelected: PIDEOptionsEditorRec;

    function CheckValues: boolean;
    procedure DoOpenEditor(EditorToOpen: TAbstractIDEOptionsEditorClass);
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
    procedure OpenEditor(GroupIndex, AIndex: integer); override;
    function FindEditor(AEditor: TAbstractIDEOptionsEditorClass): TAbstractIDEOptionsEditor; override;
    function FindEditor(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditor; override;
    function FindEditorClass(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditorClass; override;
    procedure TraverseSettings(AOptions: TAbstractIDEOptions; anAction: TIDEOptsDlgAction);
    procedure ReadAll;
    procedure WriteAll(Restore: boolean);

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
  FEditorsCreated := False;

  IDEDialogLayoutList.ApplyLayout(Self, Width, Height);
  Caption := dlgIDEOptions;
  ButtonPanel.OKButton.Caption := lisOk;
  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.OKButton.ModalResult := mrNone;
  ButtonPanel.CancelButton.Caption := dlgCancel;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.Caption:= lisMenuHelp;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  OnKeyPress:=@IDEOptionsDialogKeyPress;
end;

procedure TIDEOptionsDialog.HelpButtonClick(Sender: TObject);
begin
  if PrevEditor<>nil then
    ShowContextHelpForIDE(PrevEditor)
  else
    ShowContextHelpForIDE(Self);
end;

procedure TIDEOptionsDialog.IDEOptionsDialogKeyPress(Sender: TObject;
  var Key: char);
begin
  debugln(['TIDEOptionsDialog.IDEOptionsDialogKeyPress ',ord(Key)]);
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
  NewLastSelected := AEditor.Rec;
  if (AEditor <> nil) and (AEditor <> PrevEditor) then
  begin
    if PrevEditor <> nil then
      PrevEditor.Visible := False;
      //PrevEditor.Parent := nil;

    AEditor.Align := alClient;
    AEditor.BorderSpacing.Around := 6;
    //AEditor.Parent := Self;
    AEditor.Visible := True;

    PrevEditor := AEditor;
  end;
end;

procedure TIDEOptionsDialog.CategoryTreeCollapsed(Sender: TObject; Node: TTreeNode);
begin
  if node.Deleting then exit;
  if (Node.Data <> nil) then
    TAbstractIDEOptionsEditor(Node.Data).Rec^.Collapsed := True
  else
  if (Node.GetFirstChild <> nil) and (Node.GetFirstChild.Data <> nil) then
    TAbstractIDEOptionsEditor(Node.GetFirstChild.Data).GroupRec^.Collapsed := True;
end;

procedure TIDEOptionsDialog.CategoryTreeExpanded(Sender: TObject; Node: TTreeNode);
begin
  if node.Deleting then exit;
  if (Node.Data <> nil) then
    TAbstractIDEOptionsEditor(Node.Data).Rec^.Collapsed := False
  else
  if (Node.GetFirstChild <> nil) and (Node.GetFirstChild.Data <> nil) then
    TAbstractIDEOptionsEditor(Node.GetFirstChild.Data).GroupRec^.Collapsed := False;
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
  IDEEditorGroups.LastSelected := NewLastSelected;
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

procedure TIDEOptionsDialog.TraverseSettings(AOptions: TAbstractIDEOptions;
  anAction: TIDEOptsDlgAction);
var
  ClassTypeForCompare: TClass;

  procedure Traverse(Node: TTreeNode);
  begin
    if Node <> nil then
    begin
      if Node.Data <> nil then
        with TAbstractIDEOptionsEditor(Node.Data) do
          if ((ClassTypeForCompare = nil) and (SupportedOptionsClass = nil)) or
             ((SupportedOptionsClass <> nil) and ClassTypeForCompare.InheritsFrom(SupportedOptionsClass)) then
          begin
            case anAction of
            iodaRead: ReadSettings(AOptions);
            iodaWrite: WriteSettings(AOptions);
            iodaRestore: RestoreSettings(AOptions);
            end;
          end;
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

procedure TIDEOptionsDialog.ReadAll;
type
  TStage = (
    sBefore,
    sRead,
    sAfter
    );
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
  InstanceList: TFPList;
  s: TStage;
begin
  for s:=low(TStage) to High(TStage) do
  begin
    InstanceList:=TFPList.Create;
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
          if (InstanceList.IndexOf(Instance)<0) and (Instance <> nil) then
          begin
            InstanceList.Add(Instance);
            case s of
            sBefore:
              Instance.DoBeforeRead;
            sRead:
              TraverseSettings(Instance,iodaRead);
            sAfter:
              Instance.DoAfterRead;
            end;
          end;
        end;
      end;
    end;
    if s=sRead then
      TraverseSettings(nil,iodaRead); // load settings that does not belong to any group
    InstanceList.Free;
  end;
end;

procedure TIDEOptionsDialog.WriteAll(Restore: boolean);
type
  TStage = (
    sBefore,
    sWrite,
    sAfter
    );
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
  s: TStage;
begin
  for s:=low(TStage) to High(TStage) do
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
            case s of
            sBefore:
              Instance.DoBeforeWrite(Restore);
            sWrite:
              if Restore then
                TraverseSettings(Instance,iodaRestore)
              else
                TraverseSettings(Instance,iodaWrite);
            sAfter:
              Instance.DoAfterWrite(Restore);
            end;
          end;
        end;
      end;
    end;

    if s=sWrite then
      TraverseSettings(nil,iodaWrite); // save settings that does not belong to any group
  end;
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
  TraverseSettings(AOptions,iodaRead);
end;

procedure TIDEOptionsDialog.SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
begin
  TraverseSettings(AOptions,iodaWrite);
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
  SelectNode := nil;

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
        instance.Rec := Rec^.Items[j];

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

        if ItemParent.Data <> nil then begin
          Instance := TAbstractIDEOptionsEditor(ItemParent.Data);
          ItemParent.Expanded := not Instance.Rec^.Collapsed;
        end;
        if IDEEditorGroups.LastSelected = Rec^.Items[j] then
          SelectNode := ItemNode;
      end;
      if (GroupNode.GetFirstChild <> nil) and (GroupNode.GetFirstChild.Data <> nil) then
        TAbstractIDEOptionsEditor(GroupNode.GetFirstChild.Data).GroupRec := Rec;
      GroupNode.Expanded := not Rec^.Collapsed;
      //GroupNode.Data := Rec;
    end;
  end;
  if SelectNode <> nil then
    SelectNode.Selected := True;
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
  //DebugLn(['TIDEOptionsDialog.PassesFilter ',DbgSName(ARec^.GroupClass),' ',DbgSName(OptionsFilter)]);
  if (ARec^.GroupClass = nil) and (OptionsFilter <> nil) then
    Exit(False);
  if (OptionsFilter<>nil) and (ARec^.GroupClass <> nil)
  and (not ARec^.GroupClass.InheritsFrom(OptionsFilter)) then
    Exit(False);
  Result := True;
end;

procedure TIDEOptionsDialog.DoOpenEditor(EditorToOpen: TAbstractIDEOptionsEditorClass);
var
  Node: TTreeNode;
begin
  if EditorToOpen = nil then begin
    if SelectNode <> nil then
      Node := SelectNode
    else
      Node := CategoryTree.Items.GetFirstNode
  end
  else
    Node := SearchEditorNode(EditorToOpen);
  if Node <> nil then
    CategoryTree.Selected := Node;
  SelectNode := nil;
end;

function TIDEOptionsDialog.ShowModal: Integer;
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TIDEOptionsDialog.ShowModal'){$ENDIF};
  try
    CreateEditors;
    DoOpenEditor(nil);
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TIDEOptionsDialog.ShowModal'){$ENDIF};
  end;
  Result := inherited ShowModal;
end;

function TIDEOptionsDialog.AddButton: TBitBtn;
begin
  Result := TBitBtn.Create(Self);
  Result.Align := alCustom;
  Result.Default := false;
  Result.Constraints.MinWidth:=25;
  Result.AutoSize := true;
  Result.Parent := ButtonPanel;
end;

function TIDEOptionsDialog.AddControl(AControlClass: TControlClass): TControl;
begin
  Result := AControlClass.Create(Self);
  Result.Parent := Self;
  Result.Align:=alBottom;
  Result.BorderSpacing.Around := 6;
end;

procedure TIDEOptionsDialog.OpenEditor(AEditor: TAbstractIDEOptionsEditorClass);
begin
  if IsVisible then
    DoOpenEditor(AEditor);
end;

procedure TIDEOptionsDialog.OpenEditor(GroupIndex, AIndex: integer);
begin
  DoOpenEditor(FindEditorClass(GroupIndex,AIndex));
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

function TIDEOptionsDialog.FindEditor(GroupIndex, AIndex: integer
  ): TAbstractIDEOptionsEditor;
var
  EditorClass: TAbstractIDEOptionsEditorClass;
begin
  EditorClass:=FindEditorClass(GroupIndex,AIndex);
  if EditorClass<>nil then
    Result:=FindEditor(EditorClass)
  else
    Result:=nil;
end;

function TIDEOptionsDialog.FindEditorClass(GroupIndex, AIndex: integer
  ): TAbstractIDEOptionsEditorClass;
var
  Grp: PIDEOptionsGroupRec;
  i: Integer;
begin
  Result:=nil;
  Grp:=IDEEditorGroups.GetByIndex(GroupIndex);
  if (Grp=nil) or (Grp^.Items=nil) then exit;
  for i:=0 to Grp^.Items.Count-1 do
  begin
    if Grp^.Items[i]^.Index=AIndex then
    begin
      Result:=Grp^.Items[i]^.EditorClass;
      exit;
    end;
  end;
end;

end.

