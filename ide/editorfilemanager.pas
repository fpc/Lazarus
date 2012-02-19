unit EditorFileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, ListFilterEdit, Forms, Controls, CheckLst, ButtonPanel, StdCtrls,
  Buttons, ExtCtrls, Menus, LCLProc, LCLType, IDEImagesIntf, LazIDEIntf,
  SourceEditor, LazarusIDEStrConsts;

type

  { TEditorFileManagerForm }

  TEditorFileManagerForm = class(TForm)
    ActivateMenuItem: TMenuItem;
    MoveDownBtn: TSpeedButton;
    MoveUpBtn: TSpeedButton;
    FilterPanel: TPanel;
    OpenButton: TSpeedButton;
    SaveCheckedButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CloseCheckedButton: TBitBtn;
    CloseMenuItem: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SelectAllCheckBox: TCheckBox;
    CheckListBox1: TCheckListBox;
    FilterEdit: TListFilterEdit;
    SortAlphabeticallyButton: TSpeedButton;
    procedure ActivateMenuItemClick(Sender: TObject);
    procedure CheckListBox1DblClick(Sender: TObject);
    procedure CheckListBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckListBox1KeyPress(Sender: TObject; var Key: char);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
    procedure CloseCheckedButtonClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SaveCheckedButtonClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActivateButtonClick(Sender: TObject);
    procedure SelectAllCheckBoxClick(Sender: TObject);
    procedure SortAlphabeticallyButtonClick(Sender: TObject);
  private
    FSortAlphabetically: boolean;
    procedure CloseListItem(ListIndex: integer);
    procedure SetSortAlphabetically(AValue: boolean);
    procedure UpdateButtons;
    procedure UpdateMoveButtons(ListIndex: integer);
  public
    property SortAlphabetically: boolean read FSortAlphabetically write SetSortAlphabetically;
  end;

  function ShowEditorFileManagerForm: TModalResult;

implementation

{$R *.lfm}

function ShowEditorFileManagerForm: TModalResult;
begin
  with TEditorFileManagerForm.Create(Nil) do
  try
    Result:=ShowModal;
  finally
    Free;
  end;
end;

{ TEditorFileManagerForm }

procedure TEditorFileManagerForm.FormCreate(Sender: TObject);
var
  i: Integer;
  s: String;
  SrcEdit: TSourceEditor;
begin
  // Populate the list
  with SourceEditorManager do
    for i:=0 to SourceEditorCount-1 do begin
      s:=SourceEditors[i].FileName;
      SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(s);
      FilterEdit.Data.AddObject(s, SrcEdit);
    end;
  FilterEdit.InvalidateFilter;
  // Captions
  Caption:=lisEditorWindowManager;
  ActivateMenuItem.Caption:=lisActivate;
  CloseMenuItem.Caption:=lisClose;
  SelectAllCheckBox.Caption:=lisCheckAll;
  SaveCheckedButton.Caption:=lisSaveAllChecked;
  CloseCheckedButton.Caption:=lisCloseAllChecked;
  MoveUpBtn.Hint:=lisMoveSelectedUp;
  MoveDownBtn.Hint:=lisMoveSelectedDown;
  // Icons
  PopupMenu1.Images:=IDEImages.Images_16;
  ActivateMenuItem.ImageIndex:=IDEImages.LoadImage(16, 'laz_open');
  CloseMenuItem.ImageIndex:=IDEImages.LoadImage(16, 'menu_close');
  CloseCheckedButton.LoadGlyphFromLazarusResource('menu_close_all');
  SaveCheckedButton.LoadGlyphFromLazarusResource('menu_save_all');
  MoveUpBtn.LoadGlyphFromLazarusResource('arrow_up');
  MoveDownBtn.LoadGlyphFromLazarusResource('arrow_down');
  // Buttons on FilterPanel
  OpenButton.LoadGlyphFromLazarusResource('laz_open');
  OpenButton.Hint:=lisActivateSelected;
  SortAlphabeticallyButton.Hint:=lisPESortFilesAlphabetically;
  SortAlphabeticallyButton.LoadGlyphFromLazarusResource('pkg_sortalphabetically');
end;

procedure TEditorFileManagerForm.CheckListBox1Click(Sender: TObject);
var
  clb: TCheckListBox;
begin
  clb:=Sender as TCheckListBox;
  // Enable Activate when there is a selected item.
  OpenButton.Enabled:=clb.SelCount>0;
  UpdateMoveButtons(clb.ItemIndex);
end;

procedure TEditorFileManagerForm.CheckListBox1ItemClick(Sender: TObject; Index: integer);
var
  clb: TCheckListBox;
  i: Integer;
  HasChecked: Boolean;
begin
  clb:=Sender as TCheckListBox;
  // Notify the filter edit that item's checked state has changed.
  if Index>-1 then
    FilterEdit.ItemWasClicked(clb.Items[Index], clb.Checked[Index]);
  // Enable save and close buttons when there are checked items.
  HasChecked:=False;
  for i:=clb.Count-1 downto 0 do
    if clb.Checked[i] then begin
      HasChecked:=True;
      Break;
    end;
  SaveCheckedButton.Enabled:=HasChecked;
  CloseCheckedButton.Enabled:=HasChecked;
  CloseMenuItem.Enabled:=HasChecked;
  CheckListBox1Click(CheckListBox1); // Call also OnClick handler for other controls.
end;

procedure TEditorFileManagerForm.SelectAllCheckBoxClick(Sender: TObject);
var
  cb: TCheckBox;
  i: Integer;
begin
  cb:=Sender as TCheckBox;
  // Caption text : check all / uncheck all
  if cb.Checked then
    cb.Caption:=lisUncheckAll
  else
    cb.Caption:=lisCheckAll;
  // Set / reset all CheckListBox1 items.
  for i:=0 to CheckListBox1.Count-1 do begin
    if CheckListBox1.Checked[i]<>cb.Checked then begin
      CheckListBox1.Checked[i]:=cb.Checked;
      FilterEdit.ItemWasClicked(CheckListBox1.Items[i], cb.Checked); // Notify the filter
    end;
  end;
  CheckListBox1ItemClick(CheckListBox1, 0);
end;

procedure TEditorFileManagerForm.SortAlphabeticallyButtonClick(Sender: TObject);
begin
  SortAlphabetically:=SortAlphabeticallyButton.Down;
end;

procedure TEditorFileManagerForm.SaveCheckedButtonClick(Sender: TObject);
var
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  for i:=CheckListBox1.Count-1 downto 0 do
    if CheckListBox1.Checked[i] then begin
      SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(CheckListBox1.Items[i]);
      if (not SrcEdit.CodeBuffer.IsVirtual) and (LazarusIDE.DoSaveEditorFile(SrcEdit, []) <> mrOk) then
        DebugLn(['TSourceNotebook.EncodingClicked LazarusIDE.DoSaveEditorFile failed']);
    end;
end;

procedure TEditorFileManagerForm.CloseCheckedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=CheckListBox1.Count-1 downto 0 do
    if CheckListBox1.Checked[i] then
      CloseListItem(i);
  UpdateButtons;
end;

procedure TEditorFileManagerForm.PopupMenu1Popup(Sender: TObject);
var
  HasSelected: Boolean;
begin
  HasSelected:=CheckListBox1.SelCount>0;
  ActivateMenuItem.Enabled:=HasSelected;
  CloseMenuItem.Enabled:=HasSelected;
end;

procedure TEditorFileManagerForm.CloseMenuItemClick(Sender: TObject);
begin
  CloseListItem(CheckListBox1.ItemIndex);
  UpdateButtons;
end;

procedure TEditorFileManagerForm.ActivateMenuItemClick(Sender: TObject);
begin
  ActivateButtonClick(nil);
end;

procedure TEditorFileManagerForm.CheckListBox1DblClick(Sender: TObject);
begin
  ActivateButtonClick(nil);
end;

procedure TEditorFileManagerForm.CheckListBox1KeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in shift ) and ((Key = VK_UP) or (Key = VK_DOWN)) then begin
    if Key = VK_UP then
      MoveUpBtnClick(nil)
    else
      MoveDownBtnClick(nil);
    Key:=VK_UNKNOWN;
  end;
end;

procedure TEditorFileManagerForm.CheckListBox1KeyPress(Sender: TObject; var Key: char);
begin
  if Key = char(VK_RETURN) then
    ActivateButtonClick(nil);
end;

procedure TEditorFileManagerForm.MoveDownBtnClick(Sender: TObject);
var
  SrcEdit: TSourceEditor;
  i: Integer;
begin
  i:=CheckListBox1.ItemIndex;
  if (i>-1) and (i<CheckListBox1.Items.Count-1)
  and (FilterEdit.Filter='') and not SortAlphabetically then begin
    SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(CheckListBox1.Items[i]);
    if SrcEdit.PageIndex < SrcEdit.SourceNotebook.PageCount-1 then begin
      // First move the source editor tab
      SrcEdit.SourceNotebook.MoveEditor(SrcEdit.PageIndex, SrcEdit.PageIndex+1);
      // Then switch the list items
      FilterEdit.Data.Exchange(i, i+1); //  CheckListBox1.Items.Exchange(i, i+1);
      FilterEdit.InvalidateFilter;
      UpdateMoveButtons(i+1);
    end;
  end;
end;

procedure TEditorFileManagerForm.MoveUpBtnClick(Sender: TObject);
var
  SrcEdit: TSourceEditor;
  i: Integer;
begin
  i := CheckListBox1.ItemIndex;
  if (i > 0) and (FilterEdit.Filter='') and not SortAlphabetically then begin
    SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(CheckListBox1.Items[i]);
    if SrcEdit.PageIndex > 0 then begin
      // First move the source editor tab
      SrcEdit.SourceNotebook.MoveEditor(SrcEdit.PageIndex, SrcEdit.PageIndex-1);
      // Then switch the list items
      FilterEdit.Data.Exchange(i, i-1);
      FilterEdit.InvalidateFilter;
      UpdateMoveButtons(i-1);
    end;
  end;
end;

procedure TEditorFileManagerForm.ActivateButtonClick(Sender: TObject);
var
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  for i:=0 to CheckListBox1.Count-1 do
    if CheckListBox1.Selected[i] then begin       // Find first selected.
      SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(
                                    CheckListBox1.Items[CheckListBox1.ItemIndex]);
      SrcEdit.Activate;
      Break;
    end;
end;

// Private methods

procedure TEditorFileManagerForm.CloseListItem(ListIndex: integer);
var
  SrcEdit: TSourceEditor;
  s: String;
begin
  s:=CheckListBox1.Items[ListIndex];
  SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(s);
  if Assigned(SrcEdit) then begin
    SrcEdit.SourceNotebook.CloseFile(SrcEdit.PageIndex);
    CheckListBox1.Items.Delete(ListIndex);
    FilterEdit.RemoveItem(s);
  end;
end;

procedure TEditorFileManagerForm.SetSortAlphabetically(AValue: boolean);
begin
  if FSortAlphabetically=AValue then exit;
  FSortAlphabetically:=AValue;
  SortAlphabeticallyButton.Down:=FSortAlphabetically;
  FilterEdit.SortData:=FSortAlphabetically;
  FilterEdit.InvalidateFilter;
end;

procedure TEditorFileManagerForm.UpdateButtons;
// Update the filter and buttons. Reuse event handlers for it.
begin
  FilterEdit.InvalidateFilter;
  CheckListBox1ItemClick(CheckListBox1, 0);
end;

procedure TEditorFileManagerForm.UpdateMoveButtons(ListIndex: integer);
var
  SrcEdit: TSourceEditor;
  UpEnabled, DownEnabled: Boolean;
begin
  UpEnabled:=False;
  DownEnabled:=False;
  if (ListIndex>-1) and (ListIndex<CheckListBox1.Items.Count)
  and (FilterEdit.Filter='') and not SortAlphabetically then begin
    SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(CheckListBox1.Items[ListIndex]);
    DownEnabled:=(ListIndex<CheckListBox1.Items.Count-1)
             and (SrcEdit.PageIndex<SrcEdit.SourceNotebook.PageCount-1);
    UpEnabled:=(ListIndex>0) and (SrcEdit.PageIndex>0);
  end;
  MoveUpBtn.Enabled:=UpEnabled;
  MoveDownBtn.Enabled:=DownEnabled;
end;

end.

