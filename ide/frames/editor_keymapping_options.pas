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
}
unit editor_keymapping_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, ComCtrls, Controls, Dialogs, LCLType, LazUTF8, Menus, Buttons,
  Clipbrd, EditBtn, ExtCtrls,
  // LazControls
  TreeFilterEdit,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, SrcEditorIntf,
  // IDE
  EditorOptions, LazarusIDEStrConsts, editor_general_options,
  KeymapSchemeDlg, KeyMapping, IDECommands, KeyMapShortCutDlg;

type

  { TEditorKeymappingOptionsFrame }

  TEditorKeymappingOptionsFrame = class(TAbstractIDEOptionsEditor)
    BtnPanel: TPanel;
    ChooseSchemeButton: TBitBtn;
    ClearButton: TBitBtn;
    EditButton: TBitBtn;
    FilterEdit: TTreeFilterEdit;
    FindKeyButton: TBitBtn;
    CommandLabel: TLabel;
    ConflictsTreeView: TTreeView;
    KeyMapSplitter: TSplitter;
    KeyMapTreePanel: TPanel;
    SchemeLabel: TLabel;
    ResetKeyFilterBtn: TSpeedButton;
    TreeView: TTreeView;
    EditMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ClearMenuItemClick(Sender: TObject);
    procedure ConflictsTreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EditButtonClick(Sender: TObject);
    procedure EditMenuItemClick(Sender: TObject);
    procedure ChooseSchemeButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure FilterEditAfterFilter(Sender: TObject);
    function FilterEditFilterItem(ItemData: Pointer; out Done: Boolean): Boolean;
    procedure FilterEditKeyPress(Sender: TObject; var {%H-}Key: char);
    procedure FindKeyButtonClick(Sender: TObject);
    procedure KeyMapSplitterMoved(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure ResetKeyFilterBtnClick(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure TreeViewKeyPress(Sender: TObject; var Key: char);
    procedure TreeViewSelectionChanged(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    FEditingKeyMap: TKeyCommandRelationList;
    FIdleConnected: boolean;
    KeyMapKeyFilter: TIDEShortCut;
    fModified: Boolean;
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    procedure FillKeyMappingTreeView;
    procedure EditCommandMapping(ANode: TTreeNode);
    procedure EditConflict(ANode: TTreeNode);
    procedure EditCommandRelation(ARelation: TKeyCommandRelation);
    procedure ClearCommandMapping(ANode: TTreeNode);
    procedure ClearConflict(ANode: TTreeNode);
    procedure ClearCommandRelation(ARelation: TKeyCommandRelation);
    function KeyMappingRelationToCaption(Index: Integer): String;
    function KeyMappingRelationToCaption(KeyRelation: TKeyCommandRelation): String;
    function KeyShortCutToCaption(const aKey: TKeyCommandRelation): string;
    function CaptionToKeyMappingRelation(aCaption: string): TKeyCommandRelation;
    procedure SetIdleConnected(AValue: boolean);
    procedure UpdateKeyFilterButton;
    procedure UpdateSchemeLabel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure SelectByIdeCommand(ACmd: word);
    procedure UpdateTree;
    procedure UpdateConflictTree;
    property EditingKeyMap: TKeyCommandRelationList read FEditingKeyMap;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

implementation

{$R *.lfm}

var
  imgKeyCategory, imgKeyItem: Integer;

type

  { TKeyMapErrorsForm }

  TKeyMapErrorsForm = class(TForm)
    ListBox: TListBox;
    BackButton: TButton;
    ErrorsPopupMenu: TPopupMenu;
    CopyMenuItem: TMenuItem;
    procedure BackButtonClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
  public
    constructor Create(AnOwner: TComponent); override;
  end;

constructor TKeyMapErrorsForm.Create(AnOwner: TComponent);
begin
  inherited CreateNew(AnOwner);
  SetBounds((Screen.Width - 410) div 2, (Screen.Height - 260) div 2, 400, 250);
  Caption := dlgKeyMappingErrors;

  ListBox := TListBox.Create(Self);
  with ListBox do
  begin
    Name := 'ListBox';
    Align := alTop;
    Parent := Self;
  end;

  BackButton := TButton.Create(Self);
  with BackButton do
  begin
    Name := 'BackButton';
    AutoSize := true;
    Anchors := [akLeft,akBottom];
    Parent := Self;
    AnchorParallel(akBottom,6,Self);
    AnchorParallel(akLeft,6,Self);
    Caption := dlgEdBack;
    OnClick := @BackButtonClick;
  end;

  ErrorsPopupMenu := TPopupMenu.Create(Self);
  ErrorsPopupMenu.Name := 'ErrorsPopupMenu';
  CopyMenuItem := TMenuItem.Create(Self);
  CopyMenuItem.Caption := lisCopyAllItemsToClipboard;
  CopyMenuItem.OnClick := @CopyMenuItemClick;
  CopyMenuItem.ImageIndex := IDEImages.LoadImage('laz_copy');
  ErrorsPopupMenu.Items.Add(CopyMenuItem);

  ListBox.AnchorToNeighbour(akBottom,6,BackButton);
  ListBox.PopupMenu := ErrorsPopupMenu;
end;

procedure TKeyMapErrorsForm.BackButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TKeyMapErrorsForm.CopyMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := ListBox.Items.Text;
end;

{ TEditorKeymappingOptionsFrame }

constructor TEditorKeymappingOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditingKeyMap := TKeyCommandRelationList.Create;
  EditButton.Enabled:=false;
  ClearButton.Enabled:=false;
  fModified:=False;
end;

destructor TEditorKeymappingOptionsFrame.Destroy;
begin
  IdleConnected:=false;
  FEditingKeyMap.Free;
  inherited Destroy;
end;

procedure TEditorKeymappingOptionsFrame.ChooseSchemeButtonClick(Sender: TObject);
var
  NewScheme: String;
begin
  NewScheme := EditorOpts.KeyMappingScheme;
  if ShowChooseKeySchemeDialog(NewScheme) = mrOk then begin
    EditorOpts.KeyMappingScheme := NewScheme;
    FEditingKeyMap.LoadScheme(NewScheme);
    FillKeyMappingTreeView;
    fModified:=False;
    UpdateSchemeLabel;
    UpdateConflictTree;
  end;
end;

procedure TEditorKeymappingOptionsFrame.EditButtonClick(Sender: TObject);
begin
  EditCommandMapping(TreeView.Selected)
end;

procedure TEditorKeymappingOptionsFrame.ClearButtonClick(Sender: TObject);
begin
  ClearCommandMapping(TreeView.Selected)
end;

function TEditorKeymappingOptionsFrame.FilterEditFilterItem(ItemData: Pointer;
  out Done: Boolean): Boolean;
var
  KeyRel: TKeyCommandRelation;
begin
  Done:=True;
  Result:=False;
  if TObject(ItemData) is TKeyCommandRelation then
  begin
    KeyRel:=TKeyCommandRelation(ItemData);      // Tree item is actual key command.
    Done:=False;
    Result:=KeyMapKeyFilter.Key1<>VK_UNKNOWN;
    if Result then begin                        // Key filter is defined.
      Done:=True;
      Result:=(CompareIDEShortCutKey1s(@KeyMapKeyFilter,@KeyRel.ShortcutA)=0)
           or (CompareIDEShortCutKey1s(@KeyMapKeyFilter,@KeyRel.ShortcutB)=0);
    end;
  end;
end;

procedure TEditorKeymappingOptionsFrame.FilterEditKeyPress(Sender: TObject; var Key: char);
begin
  ResetKeyFilterBtnClick(Nil);
end;

procedure TEditorKeymappingOptionsFrame.FindKeyButtonClick(Sender: TObject);
var
  ShortCutDialog: TShortCutDialog;
begin
  ShortCutDialog := TShortCutDialog.Create(nil);
  try
    ShortCutDialog.ShowSecondary:=False;
    ShortCutDialog.ShowSequence:=False;
    ShortCutDialog.Caption:=lisChooseAKey;
    ShortCutDialog.PrimaryShortCut := KeyMapKeyFilter;
    if ShortCutDialog.ShowModal = mrOK then begin
      KeyMapKeyFilter := ShortCutDialog.PrimaryShortCut;
      UpdateKeyFilterButton;
      FilterEdit.Filter:='';       // Allow only one of the filters to be active.
      FilterEdit.InvalidateFilter;
    end;
  finally
    ShortCutDialog.Free;
  end;
end;

procedure TEditorKeymappingOptionsFrame.KeyMapSplitterMoved(Sender: TObject);
begin
  TreeView.Update;
  ConflictsTreeView.Update;
end;

procedure TEditorKeymappingOptionsFrame.OnIdle(Sender: TObject;
  var Done: Boolean);
begin
  IdleConnected:=false;
  UpdateConflictTree;
end;

procedure TEditorKeymappingOptionsFrame.ResetKeyFilterBtnClick(Sender: TObject);
begin
  KeyMapKeyFilter.Key1 := VK_UNKNOWN;
  KeyMapKeyFilter.Key2 := VK_UNKNOWN;
  UpdateKeyFilterButton;           // Allow only one of the filters to be active.
  FilterEdit.InvalidateFilter;
end;

procedure TEditorKeymappingOptionsFrame.TreeViewDblClick(Sender: TObject);
var
  P: TPoint;
  ANode: TTreeNode;
begin
  P := TreeView.ScreenToClient(Mouse.CursorPos);
  ANode := TreeView.GetNodeAt(P.X, P.Y);
  if (ANode<>nil) and (ANode.Data<>nil) and (TObject(ANode.Data) is TKeyCommandRelation) then
    EditCommandMapping(ANode);
end;

procedure TEditorKeymappingOptionsFrame.TreeViewKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = char(VK_RETURN)) and (TreeView.Selected<>nil) then
    EditCommandMapping(TreeView.Selected);
end;

procedure TEditorKeymappingOptionsFrame.TreeViewSelectionChanged(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := TreeView.Selected;
  EditButton.Enabled:=
    (ANode<>nil) and (ANode.Data<>nil) and (TObject(ANode.Data) is TKeyCommandRelation);
  ClearButton.Enabled:=EditButton.Enabled;
end;

procedure TEditorKeymappingOptionsFrame.PopupMenu1Popup(Sender: TObject);
var
  ANode: TTreeNode;
  pop: TPopupMenu;
begin
  pop := Sender as TPopupMenu;
  if pop.PopupComponent = TreeView then begin
    ANode := TreeView.Selected;
    EditMenuItem.Enabled:=
      (ANode<>nil) and (ANode.Data<>nil) and (TObject(ANode.Data) is TKeyCommandRelation);
  end else if pop.PopupComponent=ConflictsTreeView then begin
    ANode:=ConflictsTreeView.Selected;
    EditMenuItem.Enabled := (ANode<>nil) and (CaptionToKeyMappingRelation(ANode.Text)<>nil);
  end else
    EditMenuItem.Enabled := False;
  ClearMenuItem.Enabled := EditMenuItem.Enabled;
end;

procedure TEditorKeymappingOptionsFrame.EditMenuItemClick(Sender: TObject);
begin
  if PopupMenu1.PopupComponent=TreeView then
    EditCommandMapping(TreeView.Selected)
  else
    EditConflict(ConflictsTreeView.Selected);
end;

procedure TEditorKeymappingOptionsFrame.ClearMenuItemClick(Sender: TObject);
begin
  if PopupMenu1.PopupComponent=TreeView then
    ClearCommandMapping(TreeView.Selected)
  else
    ClearConflict(ConflictsTreeView.Selected);
end;

procedure TEditorKeymappingOptionsFrame.ConflictsTreeViewMouseDown(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Node: TTreeNode;
begin
  if (Button=mbLeft) and (ssDouble in Shift) then begin
    Node:=ConflictsTreeView.GetNodeAt(X,Y);
    EditConflict(Node);
  end;
end;

function TEditorKeymappingOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame; inline;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

function TEditorKeymappingOptionsFrame.GetTitle: String;
begin
  Result := dlgKeyMapping;
end;

procedure TEditorKeymappingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;
  ChooseSchemeButton.Caption := lisEdOptsLoadAScheme;
  FindKeyButton.Caption := lisFindKeyCombination;
  CommandLabel.Caption := lisSelectedCommandsMapping;
  EditButton.Caption := lisEdit;
  ClearButton.Caption := lisClear;
  EditMenuItem.Caption := lisEdit;
  ClearMenuItem.Caption := lisClear;

  TreeView.Images := IDEImages.Images_16;
  ConflictsTreeView.Images := IDEImages.Images_16;
  imgKeyCategory := IDEImages.LoadImage('item_keyboard');
  imgKeyItem := IDEImages.LoadImage('item_character');
  IDEImages.AssignImage(ChooseSchemeButton, 'item_keyboard'); // keymapcategory
  IDEImages.AssignImage(FindKeyButton, 'menu_search_find');
  IDEImages.AssignImage(EditButton, 'laz_edit');
  IDEImages.AssignImage(ClearButton, 'menu_clean');
  PopupMenu1.Images := IDEImages.Images_16;
  EditMenuItem.ImageIndex := IDEImages.LoadImage('laz_edit');
  ClearMenuItem.ImageIndex := IDEImages.LoadImage('menu_clean');

  IDEImages.AssignImage(ResetKeyFilterBtn, ResBtnListFilter);
  ResetKeyFilterBtn.Enabled := not IDEShortCutEmpty(KeyMapKeyFilter);

//  FillKeyMappingTreeView;    ... Done in ReadSettings.
//  UpdateSchemeLabel;
end;

procedure TEditorKeymappingOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
begin
  with AOptions as TEditorOptions do
    FEditingKeyMap.Assign(KeyMap);
  FillKeyMappingTreeView;
  UpdateSchemeLabel;
  with GeneralPage do
    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        FEditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
  IdleConnected:=true;
end;

procedure TEditorKeymappingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
    KeyMap.Assign(FEditingKeyMap);
end;

class function TEditorKeymappingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

function GetIDECommandImageIndex(AKeyRelation: TKeyCommandRelation): Integer;
var
  IDECommand: TIDECommand;
begin
  IDECommand:=IDECommandList.FindIDECommand(AKeyRelation.Command);
  if IDECommand.UserCount=0 then
    Exit(imgKeyItem);
  Result:=IDECommand.Users[0].ImageIndex;
  if Result=-1 then
    Result:=imgKeyItem;
end;

procedure TEditorKeymappingOptionsFrame.FillKeyMappingTreeView;
var
  i, j: Integer;
  NewCategoryNode, NewKeyNode: TTreeNode;
  CurCategory: TIDECommandCategory;
  CurKeyRelation: TKeyCommandRelation;
  ChildNodeIndex: Integer;
  CategoryNodeIndex: Integer;
  ItemCaption: String;
begin
  with TreeView do
  begin
    BeginUpdate;
    CategoryNodeIndex:=0;
    for i := 0 to FEditingKeyMap.CategoryCount - 1 do
    begin
      CurCategory := FEditingKeyMap.Categories[i];
      if Items.TopLvlCount > CategoryNodeIndex then
      begin
        NewCategoryNode := Items.TopLvlItems[CategoryNodeIndex];
        NewCategoryNode.Text := CurCategory.Description;
        NewCategoryNode.Data := CurCategory;
      end
      else
        NewCategoryNode := Items.AddObject(Nil, CurCategory.Description, CurCategory);
      NewCategoryNode.ImageIndex := imgKeyCategory;
      NewCategoryNode.SelectedIndex := NewCategoryNode.ImageIndex;
      ChildNodeIndex:=0;
      for j := 0 to CurCategory.Count - 1 do
      begin
        CurKeyRelation := TKeyCommandRelation(CurCategory[j]);
        ItemCaption:=KeyMappingRelationToCaption(CurKeyRelation);
        if NewCategoryNode.Count > ChildNodeIndex then
        begin
          NewKeyNode := NewCategoryNode.Items[ChildNodeIndex];
          NewKeyNode.Text := ItemCaption;
          NewKeyNode.Data := CurKeyRelation;
        end
        else
          NewKeyNode := Items.AddChildObject(NewCategoryNode,ItemCaption, CurKeyRelation);
        NewKeyNode.ImageIndex := GetIDECommandImageIndex(CurKeyRelation);
        NewKeyNode.SelectedIndex := NewKeyNode.ImageIndex;
        inc(ChildNodeIndex);
      end;
      // delete unneeded ones
      while NewCategoryNode.Count > ChildNodeIndex do
        NewCategoryNode[NewCategoryNode.Count - 1].Delete;
      if NewCategoryNode.Count>0 then
        inc(CategoryNodeIndex);
    end;
    while Items.TopLvlCount > CategoryNodeIndex do
      Items.TopLvlItems[Items.TopLvlCount - 1].Delete;
    EndUpdate;
  end;
end;

procedure TEditorKeymappingOptionsFrame.FilterEditAfterFilter(Sender: TObject);
begin
  TreeView.MakeSelectionVisible;
end;

function TEditorKeymappingOptionsFrame.KeyMappingRelationToCaption(Index: Integer): String;
begin
  Result := KeyMappingRelationToCaption(FEditingKeyMap.Relations[Index]);
end;

function TEditorKeymappingOptionsFrame.KeyMappingRelationToCaption(
  KeyRelation: TKeyCommandRelation): String;
const
  MaxLength = 60;
begin
  with KeyRelation do
  begin
    Result := LocalizedName;
    if UTF8Length(Result)>MaxLength then
      Result := UTF8Copy(LocalizedName, 1, MaxLength)+'...';
    if Result <> '' then
      Result := Result + '  ';
    Result := Result + KeyValuesToCaptionStr(ShortcutA, ShortcutB, '[');
  end;
end;

function TEditorKeymappingOptionsFrame.KeyShortCutToCaption(
  const aKey: TKeyCommandRelation): string;
begin
  Result:=aKey.Category.Description+'/'
        +KeyMappingRelationToCaption(aKey);
end;

function TEditorKeymappingOptionsFrame.CaptionToKeyMappingRelation(
  aCaption: string): TKeyCommandRelation;
var
  c: Integer;
  aCategory: TIDECommandCategory;
  CatStr: String;
  i: Integer;
  aKey: TKeyCommandRelation;
  s: String;
begin
  for c:=0 to FEditingKeyMap.CategoryCount-1 do begin
    aCategory:=FEditingKeyMap.Categories[c];
    CatStr:=aCategory.Description+'/';
    if LeftStr(aCaption,length(CatStr))<>CatStr then continue;
    for i:=0 to aCategory.Count-1 do begin
      aKey:=TObject(aCategory[i]) as TKeyCommandRelation;
      s:=CatStr+EditorCommandToDescriptionString(aKey.Command);
      if LeftStr(aCaption,length(s))<>s then continue;
      Result:=aKey;
      exit;
    end;
  end;
  Result:=nil;
end;

procedure TEditorKeymappingOptionsFrame.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TEditorKeymappingOptionsFrame.UpdateKeyFilterButton;
begin
  if IDEShortCutEmpty(KeyMapKeyFilter) then
    FindKeyButton.Caption := lisFindKeyCombination
  else
    FindKeyButton.Caption:=
      Format(lisFilter3, [KeyAndShiftStateToEditorKeyString(KeyMapKeyFilter)]);
  ResetKeyFilterBtn.Enabled := not IDEShortCutEmpty(KeyMapKeyFilter);
end;

procedure TEditorKeymappingOptionsFrame.UpdateSchemeLabel;
var
  s: String;
begin
  s:=lisNowLoadedScheme+EditorOpts.KeyMappingScheme;
  if fModified then
    s:=s+' (*)';
  SchemeLabel.Caption:=s;
end;

procedure TEditorKeymappingOptionsFrame.SelectByIdeCommand(ACmd: word);
var
  Node: TTreeNode;
begin
  Node := TreeView.Items.GetFirstNode;
  while node <> nil do begin
    if (node.Data <> nil) and (TObject(Node.Data) is TKeyCommandRelation) and
       (TKeyCommandRelation(Node.Data).Command = ACmd)
    then
      break;
    node := Node.GetNext;
  end;
  if node <> nil then begin
    Node.MakeVisible;
    Node.Selected := True;
  end;
end;

procedure TEditorKeymappingOptionsFrame.UpdateTree;
begin
  FillKeyMappingTreeView;
end;

procedure TEditorKeymappingOptionsFrame.UpdateConflictTree;
var
  ConflictCount: integer;
  Key1: TKeyCommandRelation;
  Key2: TKeyCommandRelation;

  procedure Check(const ShortCut1, ShortCut2: TIDEShortCut);
  // check if ShortCut1 hides ShortCut2
  var
    ConflictNode: TTreeNode;
    KeyNode: TTreeNode;
  begin
    if (ShortCut1.Key1=VK_UNKNOWN)
    or (ShortCut1.Key1<>ShortCut2.Key1)
    or (ShortCut1.Shift1<>ShortCut2.Shift1) then
      exit;    // first key differ

    if (ShortCut1.Key2=VK_UNKNOWN) or (ShortCut2.Key2=VK_UNKNOWN)
    or ((ShortCut1.Key2=ShortCut2.Key2) and (ShortCut1.Shift2=ShortCut2.Shift2))
    then begin
      // conflict found, add node with a sub node for each key
      inc(ConflictCount);
      ConflictNode:=ConflictsTreeView.Items.Add(nil,srkmConflic+IntToStr(ConflictCount));
      ConflictNode.ImageIndex:=imgKeyItem;
      ConflictNode.StateIndex:=imgKeyItem;
      KeyNode:=ConflictsTreeView.Items.AddChild(ConflictNode,
                                          KeyShortCutToCaption(Key1));
      KeyNode.ImageIndex := imgKeyItem;
      KeyNode.SelectedIndex := imgKeyItem;
      KeyNode:=ConflictsTreeView.Items.AddChild(ConflictNode,
                                          KeyShortCutToCaption(Key2));
      KeyNode.ImageIndex := imgKeyItem;
      KeyNode.SelectedIndex := imgKeyItem;
      ConflictNode.Expanded:=true;
    end;
  end;

var
  i: Integer;
  j: Integer;
begin
  ConflictsTreeView.BeginUpdate;
  ConflictsTreeView.Items.Clear;

  ConflictCount:=0;
  for i:=0 to FEditingKeyMap.Count-1 do begin
    Key1:=FEditingKeyMap[i];
    for j:=i+1 to FEditingKeyMap.Count-1 do begin
      Key2:=FEditingKeyMap[j];
      if (not Key1.Category.ScopeIntersects(Key2.Category.Scope)) then
        continue;
      Check(Key1.ShortcutA,Key2.ShortcutA);
      Check(Key1.ShortcutA,Key2.ShortcutB);
      Check(Key1.ShortcutB,Key2.ShortcutA);
      Check(Key1.ShortcutB,Key2.ShortcutB);
    end;
  end;

  if ConflictsTreeView.Items.Count=0 then
    ConflictsTreeView.Items.Add(nil, lisThereAreNoConflictingKeys);

  ConflictsTreeView.EndUpdate;
end;

procedure TEditorKeymappingOptionsFrame.EditCommandMapping(ANode: TTreeNode);
begin
  if ANode=nil then exit;
  EditCommandRelation(TKeyCommandRelation(ANode.Data));
end;

procedure TEditorKeymappingOptionsFrame.EditConflict(ANode: TTreeNode);
var
  ARelation: TKeyCommandRelation;
begin
  if ANode=nil then exit;
  ARelation:=CaptionToKeyMappingRelation(ANode.Text);
  EditCommandRelation(ARelation);
end;

procedure TEditorKeymappingOptionsFrame.EditCommandRelation(
  ARelation: TKeyCommandRelation);
var
  i: Integer;
begin
  if ARelation=nil then exit;
  i := FEditingKeyMap.IndexOf(ARelation);
  if (i < 0) or (ShowKeyMappingEditForm(i, FEditingKeyMap) <> mrOk) then exit;
  FillKeyMappingTreeView;
  fModified:=True;
  UpdateSchemeLabel;
  with GeneralPage do
    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        FEditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
  UpdateConflictTree;
end;

procedure TEditorKeymappingOptionsFrame.ClearCommandMapping(ANode: TTreeNode);
begin
  if ANode=nil then exit;
  ClearCommandRelation(TKeyCommandRelation(ANode.Data));
end;

procedure TEditorKeymappingOptionsFrame.ClearConflict(ANode: TTreeNode);
var
  ARelation: TKeyCommandRelation;
begin
  if ANode=nil then exit;
  ARelation:=CaptionToKeyMappingRelation(ANode.Text);
  ClearCommandRelation(ARelation);
end;

procedure TEditorKeymappingOptionsFrame.ClearCommandRelation(
  ARelation: TKeyCommandRelation);
var
  i: Integer;
begin
  if ARelation=nil then exit;
  i := FEditingKeyMap.IndexOf(ARelation);
  if (i < 0) then exit;
  ARelation.ShortcutA := IDEShortCut(VK_UNKNOWN, []);
  ARelation.ShortcutB := IDEShortCut(VK_UNKNOWN, []);
  FillKeyMappingTreeView;
  fModified:=True;
  UpdateSchemeLabel;
  with GeneralPage do
    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        FEditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
  UpdateConflictTree;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorKeymappingOptionsFrame, EdtOptionsKeys);
end.

