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
  Buttons, ButtonPanel, ExtCtrls, EditBtn, StdCtrls, Dialogs,
  TreeFilterEdit, IDEWindowIntf, IDEOptionsIntf, IDECommands, IDEHelpIntf,
  EnvironmentOpts, LazarusIDEStrConsts, CompOptsIntf, EditorOptions,
  {$IFDEF NewBuildModeWindow}
  BuildModesManager; //project_save_options;
  {$ELSE}
  BuildModesEditor;
  {$ENDIF}

type
  TIDEOptsDlgAction = (
    iodaRead,
    iodaWrite,
    iodaRestore
    );

  TIDEOptionsEditorFilter = array of TAbstractIDEOptionsClass;

  { TIDEOptionsDialog }

  TIDEOptionsDialog = class(TAbstractOptionsEditorDialog)
    BuildModeComboBox: TComboBox;
    BuildModeManageButton: TButton;
    BuildModeLabel: TLabel;
    ButtonPanel: TButtonPanel;
    CategoryTree: TTreeView;
    CatTVSplitter: TSplitter;
    CategoryPanel: TPanel;
    EditorsPanel: TScrollBox;
    FilterEdit: TTreeFilterEdit;
    BuildModeSelectPanel: TPanel;
    SettingsPanel: TPanel;
    procedure BuildModeComboBoxClick(Sender: TObject);
    procedure BuildModeComboBoxSelect(Sender: TObject);
    procedure BuildModeManageButtonClick(Sender: TObject);
    procedure CategoryTreeChange(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeCollapsed(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeExpanded(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function FilterEditFilterItem(Item: TObject; out Done: Boolean): Boolean;
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure IDEOptionsDialogKeyPress(Sender: TObject; var Key: char);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FOnLoadOptionsHook: TOnLoadIDEOptions;
    FOnSaveOptionsHook: TOnSaveIDEOptions;
    FOptionsFilter: TIDEOptionsEditorFilter;
    FEditorToOpen: TAbstractIDEOptionsEditorClass;
    FSettings: TIDEOptionsEditorSettings;
    PrevEditor: TAbstractIDEOptionsEditor;
    FEditorsCreated: Boolean;
    SelectNode: TTreeNode;
    NewLastSelected: PIDEOptionsEditorRec;
    PrevComboIndex: integer;

    function FindGroupClass(Node: TTreeNode): TAbstractIDEOptionsClass;
    procedure TraverseSettings(AOptions: TAbstractIDEOptions; anAction: TIDEOptsDlgAction);
    function CheckValues: boolean;
    procedure DoOpenEditor(EditorToOpen: TAbstractIDEOptionsEditorClass);
    procedure LoadIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure CreateEditors;
    function SearchEditorNode(AEditor: TAbstractIDEOptionsEditorClass): TTreeNode;
    function PassesFilter(ARec: PIDEOptionsGroupRec): Boolean;
    procedure SetSettings(const AValue: TIDEOptionsEditorSettings);
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
    procedure ReadAll;
    procedure WriteAll(Restore: boolean);
  public
    property OptionsFilter: TIDEOptionsEditorFilter read FOptionsFilter write FOptionsFilter;
    property Settings: TIDEOptionsEditorSettings read FSettings write SetSettings;
    property OnLoadIDEOptionsHook: TOnLoadIDEOptions read FOnLoadOptionsHook write FOnLoadOptionsHook;
    property OnSaveIDEOptionsHook: TOnSaveIDEOptions read FOnSaveOptionsHook write FOnSaveOptionsHook;
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
  FEditorToOpen := nil;
  SettingsPanel.Constraints.MinHeight:=0;
  BuildModeSelectPanel.Height:=0;

  IDEDialogLayoutList.ApplyLayout(Self, Width, Height);
  Caption := dlgIDEOptions;
  ButtonPanel.OKButton.Caption := lisMenuOk;
  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.OKButton.ModalResult := mrNone;
  ButtonPanel.CancelButton.Caption := lisCancel;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.Caption:= lisMenuHelp;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  OnKeyPress:=@IDEOptionsDialogKeyPress;
end;

procedure TIDEOptionsDialog.FormShow(Sender: TObject);
begin
  // make the category visible in the treeview
  if (CategoryTree.Selected<>nil) and (CategoryTree.Selected.Parent<>nil) then
    CategoryTree.TopItem:=CategoryTree.Selected.Parent;
  {$IFDEF NewBuildModeWindow}
  BuildModesManager.OnLoadIDEOptionsHook := @LoadIDEOptions;
  BuildModesManager.OnSaveIDEOptionsHook := @SaveIDEOptions;
  UpdateBuildModeCombo(BuildModeComboBox);
  {$ENDIF}
end;

procedure TIDEOptionsDialog.HelpButtonClick(Sender: TObject);
begin
  if PrevEditor<>nil then
    LazarusHelp.ShowHelpForIDEControl(PrevEditor)
  else
    LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TIDEOptionsDialog.IDEOptionsDialogKeyPress(Sender: TObject; var Key: char);
begin
  debugln(['TIDEOptionsDialog.IDEOptionsDialogKeyPress ',ord(Key)]);
end;

procedure TIDEOptionsDialog.CategoryTreeChange(Sender: TObject; Node: TTreeNode);
var
  {$IFDEF NewBuildModeWindow}
  GroupClass: TAbstractIDEOptionsClass;
  {$ENDIF}
  AEditor: TAbstractIDEOptionsEditor;
begin
  if Assigned(Node) then begin
    // The GUI filter can hide nodes. Get a visible node.
    if not Node.Visible then
      Node := Node.GetPrevVisible;
    // Group category node Has Data=nil. Get the first sub-item.
    while Assigned(Node) and not Assigned(Node.Data) do
      Node := Node.GetFirstVisibleChild;
  end;
  AEditor := nil;
  {$IFDEF NewBuildModeWindow}
  GroupClass := nil;
  {$ENDIF}
  if Assigned(Node) and Assigned(Node.Data) then begin
    Assert(TObject(Node.Data) is TAbstractIDEOptionsEditor,
      'TIDEOptionsDialog.CategoryTreeChange: Node.Data is not TAbstractIDEOptionsEditor');
    if CategoryTree.Selected = nil then
      Node.Selected := True;
    AEditor := TAbstractIDEOptionsEditor(Node.Data);
    {$IFDEF NewBuildModeWindow}
    GroupClass := FindGroupClass(Node);
    {$ENDIF}
  end;
  // Show the Build Mode panel for Compiler Options
  {$IFDEF NewBuildModeWindow}
  if (GroupClass <> nil) and (GroupClass.InheritsFrom(TLazCompilerOptions)) then
    BuildModeSelectPanel.Height:=40
  else
  {$ENDIF}
    BuildModeSelectPanel.Height:=0;
  // Hide the old and show the new editor frame
  if Assigned(AEditor) then
    NewLastSelected := AEditor.Rec;
  if (AEditor <> PrevEditor) then begin
    if Assigned(PrevEditor) then
      PrevEditor.Visible := False;
    if Assigned(AEditor) then begin
      AEditor.Align := alClient;
      AEditor.BorderSpacing.Around := 6;
      AEditor.Visible := True;
    end;
    PrevEditor := AEditor;
  end;
end;

procedure TIDEOptionsDialog.BuildModeComboBoxClick(Sender: TObject);
begin
  PrevComboIndex := BuildModeComboBox.ItemIndex;
end;

procedure TIDEOptionsDialog.BuildModeComboBoxSelect(Sender: TObject);
begin
  {$IFDEF NewBuildModeWindow}
  if BuildModeComboBox.Text = lisAllBuildModes then begin
    ShowMessage('This will allow changing all build modes at once. Not implemented yet.');
    BuildModeComboBox.ItemIndex := PrevComboIndex;
  end
  else
    SwitchBuildMode(BuildModeComboBox.Text);
  {$ENDIF}
end;

procedure TIDEOptionsDialog.BuildModeManageButtonClick(Sender: TObject);
begin
  {$IFDEF NewBuildModeWindow}
  if ShowBuildModesDlg = mrOK then
    UpdateBuildModeCombo(BuildModeComboBox);
  {$ENDIF}
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
    LazarusHelp.ShowHelpForIDEControl(PrevEditor);
  end;
end;

function TIDEOptionsDialog.FilterEditFilterItem(Item: TObject; out Done: Boolean): Boolean;
var
  OptEditor: TAbstractIDEOptionsEditor;
begin
  Result:=False;
  Done:=False;                        // Filter will use also the node caption.
  if Item=nil then Exit;
  OptEditor:=TAbstractIDEOptionsEditor(Item);
  OptEditor.RememberDefaultStyles;
  Result:=OptEditor.ContainsTextInCaption(FilterEdit.Filter);
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

function TIDEOptionsDialog.FindGroupClass(Node: TTreeNode): TAbstractIDEOptionsClass;
// Find the group category class where this node belongs to.
begin
  while Assigned(Node) do begin
    if Assigned(Node.Parent) then
      Node := Node.Parent
    else
      Break;
  end;
  // GroupRec is stored in the first child editor
  Result := nil;
  if Assigned(Node) then
    Result := TAbstractIDEOptionsEditor(Node.GetFirstChild.Data).GroupRec^.GroupClass;
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
          if ((ClassTypeForCompare = nil) and (SupportedOptionsClass = nil))
          or ((SupportedOptionsClass <> nil)
          and ClassTypeForCompare.InheritsFrom(SupportedOptionsClass)) then
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
  TStage = (sBefore, sRead, sAfter);
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
  InstanceList: TFPList;
  stag: TStage;
begin
  for stag:=low(TStage) to High(TStage) do
  begin
    InstanceList:=TFPList.Create;
    for i := 0 to IDEEditorGroups.Count - 1 do
    begin
      Rec := IDEEditorGroups[i];
      if not PassesFilter(Rec) then
        Continue;
      if Assigned(Rec^.Items) and Assigned(Rec^.GroupClass) then
      begin
        Instance := Rec^.GroupClass.GetInstance;
        if (InstanceList.IndexOf(Instance)<0) and Assigned(Instance) then
        begin
          InstanceList.Add(Instance);
          case stag of
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
    if stag=sRead then
      TraverseSettings(nil,iodaRead); // load settings that does not belong to any group
    InstanceList.Free;
  end;
end;

procedure TIDEOptionsDialog.WriteAll(Restore: boolean);
type
  TStage = (sBefore, sWrite, sAfter);
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
  stag: TStage;
begin
  for stag:=low(TStage) to High(TStage) do
  begin
    for i := 0 to IDEEditorGroups.Count - 1 do
    begin
      Rec := IDEEditorGroups[i];
      if not PassesFilter(Rec) then
        Continue;
      if Assigned(Rec^.Items) and Assigned(Rec^.GroupClass) then
      begin
        Instance := Rec^.GroupClass.GetInstance;
        if Assigned(Instance) then
        begin
          case stag of
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

    if stag=sWrite then
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
  if Assigned(OnLoadIDEOptionsHook) then
    OnLoadIDEOptionsHook(Self, AOptions);
  TraverseSettings(AOptions,iodaRead);
end;

procedure TIDEOptionsDialog.SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
begin
  TraverseSettings(AOptions,iodaWrite);
  if Assigned(OnSaveIDEOptionsHook) then
    OnSaveIDEOptionsHook(Self, AOptions);
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
    if PassesFilter(Rec) and (Rec^.Items <> nil) then
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
        Instance.Parent := EditorsPanel;
        instance.Rec := Rec^.Items[j];

        ItemParent := GroupNode;
        if Rec^.Items[j]^.Parent <> NoParent then begin
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
var
  i: Integer;
begin
  if (ARec^.GroupClass = nil) then
    Exit(Length(OptionsFilter) = 0);

  for i := 0 to Length(OptionsFilter) - 1 do
    if ARec^.GroupClass.InheritsFrom(OptionsFilter[i]) then
      Exit(True);

  Result := False;
end;

procedure TIDEOptionsDialog.SetSettings(const AValue: TIDEOptionsEditorSettings);
begin
  if FSettings <> AValue then
  begin
    FSettings := AValue;
    if ioesReadOnly in Settings then
      ButtonPanel.ShowButtons := ButtonPanel.ShowButtons - [pbOK, pbCancel] + [pbClose]
    else
      ButtonPanel.ShowButtons := ButtonPanel.ShowButtons + [pbOK, pbCancel] - [pbClose];
  end;
end;

procedure TIDEOptionsDialog.DoOpenEditor(EditorToOpen: TAbstractIDEOptionsEditorClass);
var
  Node: TTreeNode;
begin
  if EditorToOpen = nil then
  begin
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
    DoOpenEditor(FEditorToOpen);
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
  Result.Parent := SettingsPanel;
  Result.Align := alBottom;
  Result.BorderSpacing.Around := 6;
end;

procedure TIDEOptionsDialog.OpenEditor(AEditor: TAbstractIDEOptionsEditorClass);
begin
  if IsVisible then
    DoOpenEditor(AEditor)
  else
    FEditorToOpen := AEditor;
end;

procedure TIDEOptionsDialog.OpenEditor(GroupIndex, AIndex: integer);
begin
  if IsVisible then
    DoOpenEditor(FindEditorClass(GroupIndex,AIndex))
  else
    FEditorToOpen := FindEditorClass(GroupIndex,AIndex);
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

function TIDEOptionsDialog.FindEditor(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditor;
var
  EditorClass: TAbstractIDEOptionsEditorClass;
begin
  EditorClass := FindEditorClass(GroupIndex, AIndex);
  if EditorClass <> nil then
    Result := FindEditor(EditorClass)
  else
    Result := nil;
end;

function TIDEOptionsDialog.FindEditorClass(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditorClass;
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

