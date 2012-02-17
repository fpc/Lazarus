unit EditorFileManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics,
  Dialogs, CheckLst, ButtonPanel, StdCtrls, Buttons, ExtCtrls, Menus,
  IDEImagesIntf, SourceEditor, LazarusIDEStrConsts;

type

  { TEditorFileManagerForm }

  TEditorFileManagerForm = class(TForm)
    ActivateMenuItem: TMenuItem;
    ActivateButton: TBitBtn;
    SaveCheckedButton: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CloseCheckedButton: TBitBtn;
    CloseMenuItem: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    SelectAllCheckBox: TCheckBox;
    CheckListBox1: TCheckListBox;
    FilterEdit: TListFilterEdit;
    procedure ActivateMenuItemClick(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
    procedure CloseCheckedButtonClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SaveCheckedButtonClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActivateButtonClick(Sender: TObject);
    procedure SelectAllCheckBoxClick(Sender: TObject);
  private
    procedure CloseListItem(ListIndex: integer);
    procedure UpdateButtons;
  public

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
  Caption:=lisEditorFileManager;
  ActivateMenuItem.Caption:=lisActivate;
  CloseMenuItem.Caption:=lisMenuClose;
  SelectAllCheckBox.Caption:=lisCheckAll;
  ActivateButton.Caption:=lisActivateSelected;
  SaveCheckedButton.Caption:=lisSaveAllChecked;
  CloseCheckedButton.Caption:=lisCloseAllChecked;
  // Icons
  PopupMenu1.Images:=IDEImages.Images_16;
  ActivateMenuItem.ImageIndex:=IDEImages.LoadImage(16, 'laz_open');
  CloseMenuItem.ImageIndex:=IDEImages.LoadImage(16, 'menu_close');
  ActivateButton.LoadGlyphFromLazarusResource('laz_open');
  CloseCheckedButton.LoadGlyphFromLazarusResource('menu_close_all');
  SaveCheckedButton.LoadGlyphFromLazarusResource('menu_save_all');
end;

procedure TEditorFileManagerForm.CheckListBox1Click(Sender: TObject);
begin
  // Enable ActivateButton when there is a selected item.
  ActivateButton.Enabled:=(Sender as TCheckListBox).SelCount>0;
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

procedure TEditorFileManagerForm.SaveCheckedButtonClick(Sender: TObject);
var
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  ShowMessage('Under construction ...'); Exit;
  for i:=CheckListBox1.Count-1 downto 0 do
    if CheckListBox1.Checked[i] then begin
      SrcEdit:=SourceEditorManager.SourceEditorIntfWithFilename(CheckListBox1.Items[i]);
      // ToDo: save it somehow
    end;
end;

procedure TEditorFileManagerForm.CloseCheckedButtonClick(Sender: TObject);
var
  i: Integer;
  SrcEdit: TSourceEditor;
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

procedure TEditorFileManagerForm.UpdateButtons;
// Update the filter and buttons. Reuse event handlers for it.
begin
  FilterEdit.InvalidateFilter;
  CheckListBox1ItemClick(CheckListBox1, 0);
end;

end.

