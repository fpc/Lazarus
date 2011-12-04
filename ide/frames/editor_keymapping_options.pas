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
  Classes, SysUtils, FileUtil, Forms, StdCtrls, ComCtrls, Controls,
  Dialogs, LCLType, LCLProc,
  EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf, IDEImagesIntf,
  editor_general_options,
  KeymapSchemeDlg, KeyMapping, IDECommands, KeyMapShortCutDlg, SrcEditorIntf;

type

  { TEditorKeymappingOptionsFrame }

  TEditorKeymappingOptionsFrame = class(TAbstractIDEOptionsEditor)
    KeyMappingClearButton: TButton;
    KeyMappingChooseSchemeButton: TButton;
    KeyMappingConsistencyCheckButton: TButton;
    KeyMappingFilterEdit: TEdit;
    KeyMappingFindKeyButton: TButton;
    KeyMappingHelpLabel: TLabel;
    KeyMappingTreeView: TTreeView;
    procedure KeyMappingChooseSchemeButtonClick(Sender: TObject);
    procedure KeyMappingClearButtonClick(Sender: TObject);
    procedure KeyMappingConsistencyCheckButtonClick(Sender: TObject);
    procedure KeyMappingFilterEditChange(Sender: TObject);
    procedure KeyMappingFilterEditEnter(Sender: TObject);
    procedure KeyMappingFilterEditExit(Sender: TObject);
    procedure KeyMappingFindKeyButtonClick(Sender: TObject);
    procedure KeyMappingTreeViewDblClick(Sender: TObject);
    procedure KeyMappingTreeViewSelectionChanged(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    KeyMapNameFilter: string;
    EditingKeyMap: TKeyCommandRelationList;
    KeyMapKeyFilter: TIDEShortCut;

    function GeneralPage: TEditorGeneralOptionsFrame; inline;
    procedure FillKeyMappingTreeView;
    function KeyMappingRelationToString(Index: Integer): String;
    function KeyMappingRelationToString(KeyRelation: TKeyCommandRelation): String;
    procedure UpdateKeyFilterButton;
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

procedure TEditorKeymappingOptionsFrame.KeyMappingFilterEditChange(Sender: TObject);
var
  Filter: String;
begin
  if [csLoading, csDestroying] * ComponentState <> [] then
    Exit;
  Filter := KeyMappingFilterEdit.Text;
  if (Filter = lisFilter2) or (Filter = KeyMappingFilterEdit.Name) then
    Filter := '';
  KeyMapNameFilter := Filter;
  FillKeyMappingTreeView;
end;

procedure TEditorKeymappingOptionsFrame.KeyMappingChooseSchemeButtonClick(
  Sender: TObject);
var
  NewScheme: String;
begin
  NewScheme := EditorOpts.KeyMappingScheme;
  if ShowChooseKeySchemeDialog(NewScheme) <> mrOk then
    Exit;
  EditorOpts.KeyMappingScheme := NewScheme;
  EditingKeyMap.LoadScheme(NewScheme);
  FillKeyMappingTreeView;
end;

procedure TEditorKeymappingOptionsFrame.KeyMappingClearButtonClick(
  Sender: TObject);
var
  i: integer;
  ARelation: TKeyCommandRelation;
  ANode: TTreeNode;
begin
  ANode := KeyMappingTreeView.Selected;
  if (ANode <> nil) and (ANode.Data <> nil) and
     (TObject(ANode.Data) is TKeyCommandRelation) then
  begin
    ARelation := TKeyCommandRelation(ANode.Data);
    i := EditingKeyMap.IndexOf(ARelation);
    if (i >= 0) {and (ShowKeyMappingEditForm(i, EditingKeyMap) = mrOk)} then
    begin
      ARelation.ShortcutA := IDEShortCut(VK_UNKNOWN, []);
      ARelation.ShortcutB := IDEShortCut(VK_UNKNOWN, []);
      FillKeyMappingTreeView;
      with GeneralPage do
        for i := Low(PreviewEdits) to High(PreviewEdits) do
          if PreviewEdits[i] <> nil then
            EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
    end;
  end;
end;

procedure TEditorKeymappingOptionsFrame.KeyMappingConsistencyCheckButtonClick(
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

procedure TEditorKeymappingOptionsFrame.KeyMappingFilterEditEnter(Sender: TObject);
begin
  if KeyMappingFilterEdit.Text = lisFilter2 then
    KeyMappingFilterEdit.Text := '';
end;

procedure TEditorKeymappingOptionsFrame.KeyMappingFilterEditExit(Sender: TObject);
begin
  if KeyMappingFilterEdit.Text = '' then
    KeyMappingFilterEdit.Text := lisFilter2;
end;

procedure TEditorKeymappingOptionsFrame.KeyMappingFindKeyButtonClick(Sender: TObject);
var
  KeyFilter: TIDEShortCut;
begin
  if ShowKeyMappingGrabForm(KeyFilter) <> mrOK then
    Exit;
  //debugln(['TEditorOptionsForm.KeyMappingFindKeyButtonClick ',KeyAndShiftStateToEditorKeyString(KeyFilter)]);
  KeyMapKeyFilter := KeyFilter;
  UpdateKeyFilterButton;
  FillKeyMappingTreeView;
end;

procedure TEditorKeymappingOptionsFrame.KeyMappingTreeViewDblClick(
  Sender: TObject);
var
  P: TPoint;
  i: integer;
  ARelation: TKeyCommandRelation;
  ANode: TTreeNode;
begin
  P := KeyMappingTreeView.ScreenToClient(Mouse.CursorPos);
  ANode := KeyMappingTreeView.GetNodeAt(P.X, P.Y);
  if (ANode <> nil) and (ANode.Data <> nil) and
     (TObject(ANode.Data) is TKeyCommandRelation) then
  begin
    ARelation := TKeyCommandRelation(ANode.Data);
    i := EditingKeyMap.IndexOf(ARelation);
    if (i >= 0) and (ShowKeyMappingEditForm(i, EditingKeyMap) = mrOk) then
    begin
      FillKeyMappingTreeView;
      with GeneralPage do
        for i := Low(PreviewEdits) to High(PreviewEdits) do
          if PreviewEdits[i] <> nil then
            EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes, TSourceEditorWindowInterface);
    end;
  end;
end;

procedure TEditorKeymappingOptionsFrame.KeyMappingTreeViewSelectionChanged(
  Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := KeyMappingTreeView.Selected;
  KeyMappingClearButton.Enabled:=(ANode <> nil) and (ANode.Data <> nil)
                               and (TObject(ANode.Data) is TKeyCommandRelation);
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
  KeyMappingChooseSchemeButton.Caption := lisEdOptsLoadAScheme;
  KeyMappingConsistencyCheckButton.Caption := dlgCheckConsistency;
  KeyMappingHelpLabel.Caption := lisHintDoubleClickOnTheCommandYouWantToEdit;
  KeyMappingFilterEdit.Text := lisFilter2;
  KeyMappingFindKeyButton.Caption := lisFindKeyCombination;
  KeyMappingTreeView.Images := IDEImages.Images_16;
  KeyMappingClearButton.Caption := lisClearKeyMapping;
  imgKeyCategory := IDEImages.LoadImage(16, 'item_keyboard');
  imgKeyItem := IDEImages.LoadImage(16, 'item_character');
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
  HasFilter: Boolean;
  ItemCaption: String;
  NameFilterUp: String;
begin
  HasFilter:=(KeyMapNameFilter<>'');
  NameFilterUp:=uppercase(KeyMapNameFilter);
  with KeyMappingTreeView do
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
        if (NameFilterUp<>'')
        and (System.Pos(NameFilterUp,UpperCase(ItemCaption))=0) then
          continue;
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
          NewKeyNode := Items.AddChildObject(NewCategoryNode,
            ItemCaption, CurKeyRelation);
        NewKeyNode.ImageIndex := imgKeyItem;
        NewKeyNode.SelectedIndex := NewKeyNode.ImageIndex;
        inc(ChildNodeIndex);
      end;
      // delete unneeded ones
      while NewCategoryNode.Count > ChildNodeIndex do
        NewCategoryNode[NewCategoryNode.Count - 1].Delete;
      if NewCategoryNode.Count>0 then begin
        if HasFilter then
          NewCategoryNode.Expanded:=true;
        inc(CategoryNodeIndex);
      end;
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
    KeyMappingFindKeyButton.Caption:=lisFindKeyCombination
  else
    KeyMappingFindKeyButton.Caption:=
      Format(lisFilter3, [KeyAndShiftStateToEditorKeyString(KeyMapKeyFilter)]);
end;

constructor TEditorKeymappingOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EditingKeyMap := TKeyCommandRelationList.Create;
  KeyMappingClearButton.Enabled:=false;
end;

destructor TEditorKeymappingOptionsFrame.Destroy;
begin
  EditingKeyMap.Free;
  inherited Destroy;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorKeymappingOptionsFrame, EdtOptionsKeys);
end.

