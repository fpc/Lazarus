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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    This unit defines a dialog for the lazarus options.
}
unit IDEOptionsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Controls, Forms, ComCtrls, Buttons, ButtonPanel, ExtCtrls, StdCtrls,
  Dialogs, Graphics,
  // LazControls
  TreeFilterEdit, DividerBevel,
  // LazUtils
  LazLoggerBase,
  // IdeIntf
  IDEWindowIntf, IDEOptionsIntf, IDEOptEditorIntf, IDECommands, IDEHelpIntf,
  IdeIntfStrConsts, IDEImagesIntf, ProjectIntf,
  // IdeConfig
  EnvironmentOpts,
  // IDE
  EditorOptions, EnvGuiOptions, Compiler_ModeMatrix, Project,
  BuildModesManager, BuildManager, LazarusIDEStrConsts,
  // Packager
  PackageDefs;

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
    BuildModeManageButton: TSpeedButton;
    BuildModeSelectPanel: TPanel;
    ButtonPanel: TButtonPanel;
    CategoryPanel: TPanel;
    CategoryTree: TTreeView;
    Splitter: TSplitter;
    EditorsPanel: TScrollBox;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    BuildModeDividerBevel: TDividerBevel;
    FilterEdit: TTreeFilterEdit;
    BuildModesLabel: TLabel;
    SettingsPanel: TPanel;
    btnApply: TBitBtn;
    procedure ApplyButtonClick(Sender: TObject);
    procedure BuildModeComboBoxSelect(Sender: TObject);
    procedure BuildModeManageButtonClick(Sender: TObject);
    procedure CategoryTreeChange(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeCollapsed(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeCustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; {%H-}State: TCustomDrawState; var {%H-}DefaultDraw: Boolean);
    procedure CategoryTreeExpanded(Sender: TObject; Node: TTreeNode);
    procedure CategoryTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    function FilterEditFilterItem(ItemData: Pointer; out Done: Boolean): Boolean;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FTitle: string;
    FIsProjectOptionsDialog: boolean;
    FEditorsCreated: Boolean;
    FEditorToOpen: TAbstractIDEOptionsEditorClass;
    FNewLastSelected: PIDEOptionsEditorRec;
    FOnLoadOptionsHook: TOnLoadIDEOptions;
    FOnSaveOptionsHook: TOnSaveIDEOptions;
    FOptionsFilter: TIDEOptionsEditorFilter;
    FPrevEditor: TAbstractIDEOptionsEditor;
    FSelectNode: TTreeNode;
    FSettings: TIDEOptionsEditorSettings;
    function Apply: Boolean;
    function FindGroupClass(Node: TTreeNode): TAbstractIDEOptionsClass;
    procedure TraverseSettings(AOptions: TAbstractIDEOptions; anAction: TIDEOptsDlgAction);
    function CheckValues: boolean;
    procedure DoOpenEditor(EditorToOpen: TAbstractIDEOptionsEditorClass);
    procedure LoadIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure SaveIDEOptions(Sender: TObject; AOptions: TAbstractIDEOptions);
    procedure CreateEditors;
    function SearchEditorNode(AEditor: TAbstractIDEOptionsEditorClass): TTreeNode;
    function SearchEditorNode(const IDEOptionsEditorClassName: string): TTreeNode;
    function PassesFilter(ARec: PIDEOptionsGroupRec): Boolean;
    procedure SetSettings(const AValue: TIDEOptionsEditorSettings);
    function AllBuildModes: boolean;
    procedure UpdateBuildModeButtons;
    procedure UpdateDialogCaption;
    procedure SetTitle(ATitle: string);
  public
    constructor Create(AOwner: TComponent); override;
    function ShowModal: Integer; override;
    function AddButton: TBitBtn; override;
    procedure AddButtonSeparator; override;
    function AddControl(AControlClass: TControlClass): TControl; override;
    procedure OpenEditor(AEditor: TAbstractIDEOptionsEditorClass); override;
    procedure OpenEditor(GroupIndex, AIndex: integer); override;
    function FindEditor(AEditor: TAbstractIDEOptionsEditorClass): TAbstractIDEOptionsEditor; override;
    function FindEditor(const IDEOptionsEditorClassName: string): TAbstractIDEOptionsEditor; override;
    function FindEditor(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditor; override;
    function FindEditorClass(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditorClass; override;
    function ResetFilter: Boolean; override;
    procedure UpdateBuildModeGUI; override;
    procedure ReadAll;
    procedure WriteAll(Restore: boolean);
  public
    property Title: string read FTitle write SetTitle;
    property OptionsFilter: TIDEOptionsEditorFilter read FOptionsFilter write FOptionsFilter;
    property Settings: TIDEOptionsEditorSettings read FSettings write SetSettings;
    property OnLoadIDEOptionsHook: TOnLoadIDEOptions read FOnLoadOptionsHook write FOnLoadOptionsHook;
    property OnSaveIDEOptionsHook: TOnSaveIDEOptions read FOnSaveOptionsHook write FOnSaveOptionsHook;
  end;

implementation

{$R *.lfm}

procedure SetDropDownCount(AWinControl: TWinControl);
// Set global DropDownCount to all TCustomComboBox descendants under AWinControl.
var
  i: integer;
begin
  for i := 0 to AWinControl.ControlCount - 1 do
  begin
    if AWinControl.Controls[i] is TCustomComboBox then
      TCustomComboBox(AWinControl.Controls[i]).DropDownCount :=
                            EnvironmentOptions.DropDownCount;
    // recursive call
    if AWinControl.Controls[i] is TWinControl then
      SetDropDownCount(TWinControl(AWinControl.Controls[i]));
  end;
end;

{ TIDEOptionsDialog }

constructor TIDEOptionsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrevEditor := nil;
  FEditorsCreated := False;
  FEditorToOpen := nil;
  BuildModeSelectPanel.Visible := False;

  btnApply := AddButton;
  btnApply.LoadGlyphFromResource(idButtonRetry);
  btnApply.OnClick := @ApplyButtonClick;
  ButtonPanel.OKButton.OnClick := @OKButtonClick;
  ButtonPanel.OKButton.ModalResult := mrNone;
  ButtonPanel.CancelButton.OnClick := @CancelButtonClick;
  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  // caption
  Caption := dlgIDEOptions;
  FTitle := Caption;
  BuildModesLabel.Caption := lisBuildModes;
  ButtonPanel.OKButton.Caption := lisBtnOk;
  ButtonPanel.CancelButton.Caption := lisCancel;
  ButtonPanel.HelpButton.Caption:= lisMenuHelp;
  btnApply.Caption := lisApply;

  // hint
  ButtonPanel.ShowHint := true;
  btnApply.Hint := '[Shift+Enter]';
  BuildModeManageButton.Hint := lisEditBuildModes + ' [Ctrl+B]';
  BuildModeComboBox.Hint := lisSelectBuildMode + ' [Ctrl+Shift+B]';
  FilterEdit.TextHint := lisFindOption + ' [Ctrl+F]';

  // images
  IDEImages.AssignImage(BuildModeManageButton, 'menu_compiler_options');

  BuildModeComboBox.DropDownCount := EnvironmentOptions.DropDownCount;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TIDEOptionsDialog.FormShow(Sender: TObject);
begin
  // make sure the Selected Item is Visible,
  // and try to set the Top Category to TopItem.
  // 1. make the Top Category the TopItem and visible in the treeview
  // 2. make sure the Selected Item is Visible in the treeview
  if CategoryTree.Selected<>nil then begin
    CategoryTree.TopItem:=CategoryTree.Selected.GetParentNodeOfAbsoluteLevel(0);
    CategoryTree.Selected.MakeVisible;
  end;

  // select first not empty category
  if (CategoryTree.Selected <> nil) and
     (CategoryTree.Selected.Data = nil) and // is group category
     (CategoryTree.Selected.GetFirstVisibleChild <> nil)
  then
    CategoryTree.Selected := CategoryTree.Selected.GetFirstVisibleChild;

  BuildModesManager.OnLoadIDEOptionsHook := @LoadIDEOptions;
  BuildModesManager.OnSaveIDEOptionsHook := @SaveIDEOptions;
  UpdateBuildModeGUI;
end;

procedure TIDEOptionsDialog.HelpButtonClick(Sender: TObject);
begin
  if FPrevEditor<>nil then
    LazarusHelp.ShowHelpForIDEControl(FPrevEditor)
  else
    LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TIDEOptionsDialog.CategoryTreeChange(Sender: TObject; Node: TTreeNode);
var
  GroupClass: TAbstractIDEOptionsClass;
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
  GroupClass := nil;
  if Assigned(Node) and Assigned(Node.Data) then begin
    Assert(TObject(Node.Data) is TAbstractIDEOptionsEditor,
      'TIDEOptionsDialog.CategoryTreeChange: Node.Data is not TAbstractIDEOptionsEditor');
    if CategoryTree.Selected = nil then
      Node.Selected := True;
    AEditor := TAbstractIDEOptionsEditor(Node.Data);
    GroupClass := FindGroupClass(Node);
  end;
  FIsProjectOptionsDialog := Assigned(GroupClass)
    and (GroupClass.InheritsFrom(TAbstractIDEProjectOptions) or
         GroupClass.InheritsFrom(TProjectCompilerOptions));
  UpdateDialogCaption;
  // Show the Build Mode panel for project compiler options
  BuildModeSelectPanel.Visible := Assigned(GroupClass)
    and GroupClass.InheritsFrom(TProjectCompilerOptions);
  // Show the Apply button only for global options (not project or package options).
  btnApply.Visible := Assigned(GroupClass)
    and GroupClass.InheritsFrom(TAbstractIDEEnvironmentOptions);
  // Hide the old and show the new editor frame
  if Assigned(AEditor) then
    FNewLastSelected := AEditor.Rec;
  if (AEditor <> FPrevEditor) then begin
    if Assigned(FPrevEditor) then
      FPrevEditor.Visible := False;
    if Assigned(AEditor) then begin
      AEditor.Align := alClient;
      SetDropDownCount(AEditor);
      AEditor.Visible := True;
    end;
    FPrevEditor := AEditor;
  end;
end;

procedure TIDEOptionsDialog.BuildModeComboBoxSelect(Sender: TObject);
begin
  if not FIsProjectOptionsDialog then exit;
  
  if AllBuildModes then
    ShowMessage(lisThisWillAllowChangingAllBuildModesAtOnceNotImpleme)
  else begin
    SwitchBuildMode(BuildModeComboBox.Text);
    UpdateDialogCaption;    
  end;
end;

procedure TIDEOptionsDialog.BuildModeManageButtonClick(Sender: TObject);
begin
  if not FIsProjectOptionsDialog then exit;
  
  if ShowBuildModesDlg(Project1.SessionStorage in pssHasSeparateSession) = mrOK then
  begin
    UpdateBuildModeCombo(BuildModeComboBox);
    UpdateDialogCaption;
  end;
end;

procedure TIDEOptionsDialog.CategoryTreeCollapsed(Sender: TObject; Node: TTreeNode);
begin
  if Node.Deleting then exit;
  if Assigned(Node.Data) then
    TAbstractIDEOptionsEditor(Node.Data).Rec^.Collapsed := True
  else if Assigned(Node.GetFirstChild) and Assigned(Node.GetFirstChild.Data) then
    TAbstractIDEOptionsEditor(Node.GetFirstChild.Data).GroupRec^.Collapsed := True;
end;

procedure TIDEOptionsDialog.CategoryTreeCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  // make group categories bold
  if Node.Data = nil then // is group category
    Node.TreeView.Font.Style := [fsBold]
  else
    Node.TreeView.Font.Style := [];
end;

procedure TIDEOptionsDialog.CategoryTreeExpanded(Sender: TObject; Node: TTreeNode);
begin
  if node.Deleting then exit;
  if Assigned(Node.Data) then
    TAbstractIDEOptionsEditor(Node.Data).Rec^.Collapsed := False
  else if Assigned(Node.GetFirstChild) and Assigned(Node.GetFirstChild.Data) then
    TAbstractIDEOptionsEditor(Node.GetFirstChild.Data).GroupRec^.Collapsed := False;
end;

procedure TIDEOptionsDialog.CategoryTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Command: Word;
begin
  Command := EditorOpts.KeyMap.TranslateKey(Key,Shift,nil);
  if (Command=ecContextHelp) and (FPrevEditor <> nil) then begin
    Key:=VK_UNKNOWN;
    LazarusHelp.ShowHelpForIDEControl(FPrevEditor);
  end;
end;

function TIDEOptionsDialog.FilterEditFilterItem(ItemData: Pointer; out Done: Boolean): Boolean;
var
  OptEditor: TAbstractIDEOptionsEditor;
begin
  Result:=False;
  Done:=False;                        // Filter will use also the node caption.
  if ItemData=nil then Exit;
  OptEditor:=TAbstractIDEOptionsEditor(ItemData);
  OptEditor.RememberDefaultStyles;
  Result:=OptEditor.ContainsTextInCaption(FilterEdit.Filter);
end;

procedure TIDEOptionsDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // dialog
  if (Key = VK_ESCAPE) and (Shift = []) then
  begin
    CancelButtonClick(Sender);
    Key := 0;
  end
  else if (Key = VK_RETURN) and (Shift = [ssShift]) then
  begin
    ApplyButtonClick(Sender);
    Key := 0;
  end

  // filter
  else if (Key = VK_F) and (Shift = [ssCtrl]) then
  begin
    if FilterEdit.CanSetFocus then
      FilterEdit.SetFocus;
    Key := 0;
  end

  // build modes
  else if (Key = VK_B) and (Shift = [ssCtrl]) then
  begin
    BuildModeManageButtonClick(Sender);
    Key := 0;
  end
  else if (Key = VK_B) and (Shift = [ssCtrl, ssShift]) then
  begin
    with BuildModeComboBox do
      if FIsProjectOptionsDialog and (Items.Count > 0) then
      begin
        // next mode (in a circle)
        ItemIndex := (ItemIndex + 1) mod Items.Count;
        BuildModeComboBoxSelect(Sender);
      end;
    Key := 0;
  end;
end;

function TIDEOptionsDialog.Apply: Boolean;
begin
  Result := CheckValues;
  if not Result then
  begin
    DebugLn(['TIDEOptionsDialog.Apply: CheckValues failed!']);
    Exit;
  end;
  IDEEditorGroups.LastSelected := FNewLastSelected;
  WriteAll(false);  // write new values
end;

procedure TIDEOptionsDialog.ApplyButtonClick(Sender: TObject);
// This is called only for global options, ApplyButton is hidden otherwise.
begin
  if not Apply then
    Exit;
  // update TaskBarBehavior immediately
  if EnvironmentGuiOpts.Desktop.SingleTaskBarButton
    then Application.TaskBarBehavior := tbSingleButton
    else Application.TaskBarBehavior := tbDefault;
  // update DropDownCount property immediately
  SetDropDownCount(EditorsPanel);
end;

procedure TIDEOptionsDialog.OkButtonClick(Sender: TObject);
begin
  if not Apply then
    Exit;
  // close
  if WindowState <> wsMaximized then
    IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOk;
end;

procedure TIDEOptionsDialog.CancelButtonClick(Sender: TObject);
begin
  // cancel
  WriteAll(true); // restore old values
  // ToDo: Set build target only for project options.
  MainBuildBoss.SetBuildTargetProject1;
  // close
  if WindowState <> wsMaximized then
    IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TIDEOptionsDialog.SetTitle(ATitle: string);
begin
  FTitle := ATitle;
  UpdateDialogCaption;
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
  if Assigned(Node) then
    Result := TAbstractIDEOptionsEditor(Node.GetFirstChild.Data).GroupRec^.GroupClass
  else
    Result := nil;
end;

procedure TIDEOptionsDialog.TraverseSettings(AOptions: TAbstractIDEOptions;
  anAction: TIDEOptsDlgAction);
var
  ClassTypeForCompare: TClass;

  procedure Traverse(Node: TTreeNode);
  begin
    if Node = nil then exit;
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

    // save settings that do not belong to any group
    if stag=sWrite then
      if Restore then
        TraverseSettings(nil,iodaRestore)
      else
        TraverseSettings(nil,iodaWrite);
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
  if AOptions is TProjectCompilerOptions then
    UpdateBuildModeButtons;
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
  FSelectNode := nil;

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
        Instance.Rec := Rec^.Items[j];

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
          FSelectNode := ItemNode;
      end;
      if (GroupNode.GetFirstChild <> nil) and (GroupNode.GetFirstChild.Data <> nil) then
        TAbstractIDEOptionsEditor(GroupNode.GetFirstChild.Data).GroupRec := Rec;
      GroupNode.Expanded := not Rec^.Collapsed;
    end;
  end;
  if FSelectNode <> nil then
    FSelectNode.Selected := True;
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

function TIDEOptionsDialog.SearchEditorNode(
  const IDEOptionsEditorClassName: string): TTreeNode;

  function Traverse(ANode: TTreeNode): TTreeNode;
  begin
    Result := nil;
    if ANode <> nil then
    begin
      if (ANode.Data <> nil)
      and (CompareText(TObject(ANode.Data).ClassName,IDEOptionsEditorClassName)=0) then
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

function TIDEOptionsDialog.AllBuildModes: boolean;
begin
  Result:=BuildModeComboBox.Text = lisAllBuildModes;
end;

procedure TIDEOptionsDialog.UpdateBuildModeButtons;
var
  ModeMatrix: TCompOptModeMatrixFrame;
begin
  ModeMatrix:=TCompOptModeMatrixFrame(FindEditor(TCompOptModeMatrixFrame));
  if Assigned(ModeMatrix) then
    ModeMatrix.UpdateModes;
end;

procedure TIDEOptionsDialog.UpdateDialogCaption;
begin
  // Show current build mode in Caption for Project options dialog
  if FIsProjectOptionsDialog then
    Caption := Format('%s [%s]', [FTitle, BuildModeComboBox.Text])
  else
    Caption := FTitle;
end;

procedure TIDEOptionsDialog.DoOpenEditor(EditorToOpen: TAbstractIDEOptionsEditorClass);
var
  Node: TTreeNode;
begin
  if EditorToOpen = nil then
  begin
    if FSelectNode <> nil then
      Node := FSelectNode
    else
      Node := CategoryTree.Items.GetFirstNode
  end
  else
    Node := SearchEditorNode(EditorToOpen);
  if Node <> nil then
    CategoryTree.Selected := Node;
  FSelectNode := nil;
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
  Result.Constraints.MinWidth := 75;
  Result.AutoSize := true;
  Result.Parent := ButtonPanel;
end;

procedure TIDEOptionsDialog.AddButtonSeparator;
var
  pnl: TPanel;
begin
  pnl := TPanel.Create(Self);
  pnl.Align := alCustom;
  pnl.BevelOuter := bvNone;
  pnl.Width := 6;
  pnl.Parent := ButtonPanel;
end;

function TIDEOptionsDialog.AddControl(AControlClass: TControlClass): TControl;
begin
  Result := AControlClass.Create(Self);
  Result.Parent := SettingsPanel;
  Result.BorderSpacing.Top := 6;
  Result.BorderSpacing.Bottom := 6;
  Result.Align := alBottom;
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

function TIDEOptionsDialog.FindEditor(const IDEOptionsEditorClassName: string
  ): TAbstractIDEOptionsEditor;
var
  Node: TTreeNode;
begin
  Node := SearchEditorNode(IDEOptionsEditorClassName);
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
  if Assigned(Grp) and Assigned(Grp^.Items) then
    for i:=0 to Grp^.Items.Count-1 do
      if Grp^.Items[i]^.Index=AIndex then
        exit(Grp^.Items[i]^.EditorClass);
end;

function TIDEOptionsDialog.ResetFilter: Boolean;
begin
  Result := FilterEdit.Filter <> '';
  FilterEdit.ResetFilter;
end;

procedure TIDEOptionsDialog.UpdateBuildModeGUI;
begin
  UpdateBuildModeCombo(BuildModeComboBox);
  UpdateBuildModeButtons;
end;

end.

