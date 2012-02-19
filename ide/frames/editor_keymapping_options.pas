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
}
unit editor_keymapping_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TreeFilterEdit, Forms, StdCtrls, ComCtrls,
  Controls, Dialogs, LCLType, LCLProc, Menus, Buttons, EditorOptions,
  LazarusIDEStrConsts, IDEOptionsIntf, IDEImagesIntf, editor_general_options,
  KeymapSchemeDlg, KeyMapping, IDECommands, KeyMapShortCutDlg, SrcEditorIntf;

type

  { TEditorKeymappingOptionsFrame }

  TEditorKeymappingOptionsFrame = class(TAbstractIDEOptionsEditor)
    ChooseSchemeButton: TBitBtn;
    ClearButton: TBitBtn;
    ConsistencyCheckButton: TBitBtn;
    FilterEdit: TTreeFilterEdit;
    FindKeyButton: TBitBtn;
    HelpLabel: TLabel;
    SchemeLabel: TLabel;
    TreeView: TTreeView;
    EditMenuItem: TMenuItem;
    ClearMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ClearMenuItemClick(Sender: TObject);
    procedure EditMenuItemClick(Sender: TObject);
    procedure ChooseSchemeButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ConsistencyCheckButtonClick(Sender: TObject);
    procedure FindKeyButtonClick(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure TreeViewKeyPress(Sender: TObject; var Key: char);
    procedure TreeViewSelectionChanged(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    EditingKeyMap: TKeyCommandRelationList;
    KeyMapKeyFilter: TIDEShortCut;
    fModified: Boolean;
    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    procedure FillKeyMappingTreeView;
    procedure EditCommandMapping(ANode: TTreeNode);
    procedure ClearCommandMapping(ANode: TTreeNode);
    function KeyMappingRelationToString(Index: Integer): String;
    function KeyMappingRelationToString(KeyRelation: TKeyCommandRelation): String;
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
  end;

implementation

{$R *.lfm}

var
  imgKeyCategory, imgKeyItem: Integer;

type
  TKeyMapErrorsForm = class(TForm)
    ListBox: TListBox;
    BackButton: TButton;
    procedure BackButtonClick(Sender: TObject);
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
    Align:=alTop;
    Parent := Self;
  end;

  BackButton := TButton.Create(Self);
  with BackButton do
  begin
    Name := 'BackButton';
    AutoSize:=true;
    Anchors:=[akLeft,akBottom];
    Parent := Self;
    AnchorParallel(akBottom,6,Self);
    AnchorParallel(akLeft,6,Self);
    Caption := dlgEdBack;
    OnClick := @BackButtonClick;
  end;
  ListBox.AnchorToNeighbour(akBottom,6,BackButton);
end;

procedure TKeyMapErrorsForm.BackButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

{ TEditorKeymappingOptionsFrame }

constructor TEditorKeymappingOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EditingKeyMap := TKeyCommandRelationList.Create;
  ClearButton.Enabled:=false;
  fModified:=False;
end;

destructor TEditorKeymappingOptionsFrame.Destroy;
begin
  EditingKeyMap.Free;
  inherited Destroy;
end;

procedure TEditorKeymappingOptionsFrame.ChooseSchemeButtonClick(Sender: TObject);
var
  NewScheme: String;
begin
  NewScheme := EditorOpts.KeyMappingScheme;
  if ShowChooseKeySchemeDialog(NewScheme) <> mrOk then
    Exit;
  EditorOpts.KeyMappingScheme := NewScheme;
  EditingKeyMap.LoadScheme(NewScheme);
  FillKeyMappingTreeView;
  fModified:=False;
  UpdateSchemeLabel;
end;

procedure TEditorKeymappingOptionsFrame.ClearButtonClick(Sender: TObject);
begin
  ClearCommandMapping(TreeView.Selected);
end;

procedure TEditorKeymappingOptionsFrame.ConsistencyCheckButtonClick(
  Sender: TObject);
var
  Protocol: TStringList;
  ErrorCount, Index1, Index2: Integer;
  ACaption, AText: String;
  KeyMapErrorsForm: TKeyMapErrorsForm;
begin
  Protocol := TStringList.Create;
  try
    ErrorCount := FindKeymapConflicts(EditingKeyMap, Protocol, Index1, Index2);
    if ErrorCount > 0 then
    begin
      KeyMapErrorsForm := TKeyMapErrorsForm.Create(nil);
      try
        KeyMapErrorsForm.ListBox.Items.Assign(Protocol);
        KeyMapErrorsForm.ShowModal;
      finally
        KeyMapErrorsForm.Free;
      end;
    end
    else
    begin
      ACaption := dlgReport;
      AText    := dlgEdNoErr;
      MessageDlg(ACaption, AText, mtInformation, [mbOk], 0);
    end;
  finally
    Protocol.Free;
  end;
end;

procedure TEditorKeymappingOptionsFrame.FindKeyButtonClick(Sender: TObject);
var
  KeyFilter: TIDEShortCut;
begin
  if ShowKeyMappingGrabForm(KeyFilter) <> mrOK then
    Exit;
  KeyMapKeyFilter := KeyFilter;
  UpdateKeyFilterButton;
  FillKeyMappingTreeView;
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
  if Key = char(VK_RETURN) then
    EditCommandMapping(TreeView.Selected);
end;

procedure TEditorKeymappingOptionsFrame.TreeViewSelectionChanged(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := TreeView.Selected;
  ClearButton.Enabled:=
    (ANode<>nil) and (ANode.Data<>nil) and (TObject(ANode.Data) is TKeyCommandRelation);
end;

procedure TEditorKeymappingOptionsFrame.PopupMenu1Popup(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := TreeView.Selected;
  EditMenuItem.Enabled:=
    (ANode<>nil) and (ANode.Data<>nil) and (TObject(ANode.Data) is TKeyCommandRelation);
  ClearMenuItem.Enabled := EditMenuItem.Enabled;
end;

procedure TEditorKeymappingOptionsFrame.EditMenuItemClick(Sender: TObject);
begin
  EditCommandMapping(TreeView.Selected);
end;

procedure TEditorKeymappingOptionsFrame.ClearMenuItemClick(Sender: TObject);
begin
  ClearCommandMapping(TreeView.Selected);
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
  HelpLabel.Caption := lisCommandEditHint;
  ChooseSchemeButton.Caption := lisEdOptsLoadAScheme;
  ConsistencyCheckButton.Caption := dlgCheckConsistency;
  FindKeyButton.Caption := lisFindKeyCombination;
  ClearButton.Caption := lisClearKeyMapping;
  ClearMenuItem.Caption := lisClearKeyMapping;
  EditMenuItem.Caption := lisEditKeyMapping;

  TreeView.Images := IDEImages.Images_16;
  imgKeyCategory := IDEImages.LoadImage(16, 'item_keyboard');
  imgKeyItem := IDEImages.LoadImage(16, 'item_character');
  ChooseSchemeButton.LoadGlyphFromLazarusResource('item_keyboard'); // keymapcategory
  ConsistencyCheckButton.LoadGlyphFromLazarusResource('menu_tool_check_lfm');
  FindKeyButton.LoadGlyphFromLazarusResource('menu_search_find');
  ClearButton.LoadGlyphFromLazarusResource('menu_clean');
  PopupMenu1.Images := IDEImages.Images_16;
  ClearMenuItem.ImageIndex := IDEImages.LoadImage(16, 'menu_clean');
  EditMenuItem.ImageIndex := IDEImages.LoadImage(16, 'laz_edit');

  FillKeyMappingTreeView;
  UpdateSchemeLabel;
end;

procedure TEditorKeymappingOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
begin
  with AOptions as TEditorOptions do
    EditingKeyMap.Assign(KeyMap);
  FillKeyMappingTreeView;

  with GeneralPage do
    for i := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[i] <> nil then
        EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
end;

procedure TEditorKeymappingOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
    KeyMap.Assign(EditingKeyMap);
end;

class function TEditorKeymappingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
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
    for i := 0 to EditingKeyMap.CategoryCount - 1 do
    begin
      CurCategory := EditingKeyMap.Categories[i];
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
        ItemCaption:=KeyMappingRelationToString(CurKeyRelation);
        if (KeyMapKeyFilter.Key1<>VK_UNKNOWN)
        and (CompareIDEShortCutKey1s(@KeyMapKeyFilter,@CurKeyRelation.ShortcutA)<>0)
        and (CompareIDEShortCutKey1s(@KeyMapKeyFilter,@CurKeyRelation.ShortcutB)<>0)
        then
          continue;
        if NewCategoryNode.Count > ChildNodeIndex then
        begin
          NewKeyNode := NewCategoryNode.Items[ChildNodeIndex];
          NewKeyNode.Text := ItemCaption;
          NewKeyNode.Data := CurKeyRelation;
        end
        else
          NewKeyNode := Items.AddChildObject(NewCategoryNode,ItemCaption, CurKeyRelation);
        NewKeyNode.ImageIndex := imgKeyItem;
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

function TEditorKeymappingOptionsFrame.KeyMappingRelationToString(Index: Integer): String;
begin
  Result := KeyMappingRelationToString(EditingKeyMap.Relations[Index]);
end;

function TEditorKeymappingOptionsFrame.KeyMappingRelationToString(
  KeyRelation: TKeyCommandRelation): String;
const
  MaxLength = 60;

  function AddBrakets(S: String): String;
  begin
    Result := '[' + S + ']';
  end;

begin
  with KeyRelation do
  begin
    Result := LocalizedName;
    if UTF8Length(Result)>MaxLength then
      Result := UTF8Copy(LocalizedName, 1, MaxLength)+'...';
    if Result <> '' then
      Result := Result + '  ';
    if (ShortcutA.Key1 = VK_UNKNOWN) and (ShortcutB.Key1 = VK_UNKNOWN) then
      Result := Result{ + lisNone2 }
    else
    if (ShortcutA.Key1 = VK_UNKNOWN) then
      Result := Result + AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutB))
    else
    if (ShortcutB.Key1 = VK_UNKNOWN) then
      Result := Result + AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutA))
    else
      Result := Result + AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutA))
                       + '  '+lisOr+'  ' +
                         AddBrakets(KeyAndShiftStateToEditorKeyString(ShortcutB));
  end;
end;

procedure TEditorKeymappingOptionsFrame.UpdateKeyFilterButton;
begin
  if IDEShortCutEmpty(KeyMapKeyFilter) then
    FindKeyButton.Caption:=lisFindKeyCombination
  else
    FindKeyButton.Caption:=
      Format(lisFilter3, [KeyAndShiftStateToEditorKeyString(KeyMapKeyFilter)]);
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

procedure TEditorKeymappingOptionsFrame.EditCommandMapping(ANode: TTreeNode);
var
  i: integer;
  ARelation: TKeyCommandRelation;
begin
  ARelation := TKeyCommandRelation(ANode.Data);
  i := EditingKeyMap.IndexOf(ARelation);
  if (i >= 0) and (ShowKeyMappingEditForm(i, EditingKeyMap) = mrOk) then
  begin
    FillKeyMappingTreeView;
    fModified:=True;
    UpdateSchemeLabel;
    with GeneralPage do
      for i := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[i] <> nil then
          EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
  end;
end;

procedure TEditorKeymappingOptionsFrame.ClearCommandMapping(ANode: TTreeNode);
var
  i: integer;
  ARelation: TKeyCommandRelation;
begin
  ARelation := TKeyCommandRelation(ANode.Data);
  i := EditingKeyMap.IndexOf(ARelation);
  if (i >= 0) {and (ShowKeyMappingEditForm(i, EditingKeyMap) = mrOk)} then
  begin
    ARelation.ShortcutA := IDEShortCut(VK_UNKNOWN, []);
    ARelation.ShortcutB := IDEShortCut(VK_UNKNOWN, []);
    FillKeyMappingTreeView;
    fModified:=True;
    UpdateSchemeLabel;
    with GeneralPage do
      for i := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[i] <> nil then
          EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
  end;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorKeymappingOptionsFrame, EdtOptionsKeys);
end.

