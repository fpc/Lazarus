unit CollectionPropEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, StdCtrls;

type
  { TCollectionPropertyEditor }

  TCollectionPropertyEditorForm = class(TForm)
    CollectionListBox: TListBox;
    ToolBar1: TToolBar;
    AddButton: TToolButton;
    DeleteButton: TToolButton;
    ToolButton3: TToolButton;
    MoveUpButton: TToolButton;
    MoveDownButton: TToolButton;
    procedure AddButtonClick(Sender: TObject);
    procedure CollectionListBoxClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
  private
    FCollection: TCollection;
    FOwnerPersistent: TPersistent;
    FPropertyName: String;
  protected
    procedure UpdateCaption;
    procedure UpdateButtons;
    procedure ComponentRenamed(AComponent: TComponent);
    procedure PersistentDeleting(APersistent: TPersistent);
    procedure RefreshPropertyValues;
  public
    procedure FillCollectionListBox;
    procedure SelectInObjectInspector(UnselectAll: Boolean);
    procedure SetCollection(NewCollection: TCollection;
                    NewOwnerPersistent: TPersistent; const NewPropName: String);
    procedure Modified;
  public
    property Collection: TCollection read FCollection;
    property OwnerPersistent: TPersistent read FOwnerPersistent;
    property PropertyName: String read FPropertyName;
  end;

implementation

{$R *.lfm}

uses
  Controls, Dialogs, IDEImagesIntf, ObjInspStrConsts, PropEdits, PropEditUtils;

procedure TCollectionPropertyEditorForm.FormCreate(Sender: TObject);
begin
  ToolBar1.Images := IDEImages.Images_16;
  AddButton.Caption := oiColEditAdd;
  DeleteButton.Caption := oiColEditDelete;
  MoveUpButton.Caption := oiColEditUp;
  MoveDownButton.Caption := oiColEditDown;
  AddButton.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  DeleteButton.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  MoveUpButton.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
  MoveDownButton.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');
end;

procedure TCollectionPropertyEditorForm.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TCollectionPropertyEditorForm.MoveDownButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I >= Collection.Count - 1 then Exit;

  Collection.Items[I].Index := I + 1;
  CollectionListBox.ItemIndex := I + 1;

  FillCollectionListBox;
  SelectInObjectInspector(False);
  Modified;
end;

procedure TCollectionPropertyEditorForm.MoveUpButtonClick(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I < 0 then Exit;

  Collection.Items[I].Index := I - 1;
  CollectionListBox.ItemIndex := I - 1;

  FillCollectionListBox;
  SelectInObjectInspector(False);
  Modified;
end;

procedure TCollectionPropertyEditorForm.AddButtonClick(Sender: TObject);
begin
  if Collection = nil then Exit;
  Collection.Add;

  FillCollectionListBox;
  if CollectionListBox.Items.Count > 0 then
    CollectionListBox.ItemIndex := CollectionListBox.Items.Count - 1;
  SelectInObjectInspector(False);
  UpdateButtons;
  UpdateCaption;
  Modified;
end;

procedure TCollectionPropertyEditorForm.CollectionListBoxClick(Sender: TObject);
begin
  UpdateButtons;
  UpdateCaption;
  SelectInObjectInspector(False);
end;

procedure TCollectionPropertyEditorForm.DeleteButtonClick(Sender: TObject);
var
  I : Integer;
  NewItemIndex: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if (I >= 0) and (I < Collection.Count) then
  begin
    if MessageDlg(oisConfirmDelete,
      Format(oisDeleteItem, ['"', Collection.Items[I].DisplayName, '"']),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // select other item, or unselect
      NewItemIndex := I + 1;
      while (NewItemIndex < CollectionListBox.Items.Count)
      and (CollectionListBox.Selected[NewItemIndex]) do Inc(NewItemIndex);

      if NewItemIndex = CollectionListBox.Items.Count then
      begin
        NewItemIndex := 0;
        while (NewItemIndex < Pred(I))
        and not (CollectionListBox.Selected[NewItemIndex]) do Inc(NewItemIndex);

        if NewItemIndex = I then NewItemIndex := -1;
      end;

      CollectionListBox.ItemIndex := -1;

      if NewItemIndex > I then Dec(NewItemIndex);
      //debugln('TCollectionPropertyEditorForm.DeleteClick A NewItemIndex=',dbgs(NewItemIndex),' ItemIndex=',dbgs(CollectionListBox.ItemIndex),' CollectionListBox.Items.Count=',dbgs(CollectionListBox.Items.Count),' Collection.Count=',dbgs(Collection.Count));
      // unselect all items in OI (collections can act strange on delete)
      SelectInObjectInspector(True);
      // now delete
      Collection.Items[I].Free;
      // update listbox after whatever happened
      FillCollectionListBox;
      // set NewItemIndex
      if NewItemIndex < CollectionListBox.Items.Count then
      begin
        CollectionListBox.ItemIndex := NewItemIndex;
        SelectInObjectInspector(False);
      end;
      //debugln('TCollectionPropertyEditorForm.DeleteClick B');
      Modified;
    end;
  end;
  UpdateButtons;
  UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.UpdateCaption;
var
  NewCaption: String;
begin
  //I think to match Delphi this should be formatted like
  //"Editing ComponentName.PropertyName[Index]"
  if OwnerPersistent is TComponent then
    NewCaption := TComponent(OwnerPersistent).Name
  else
    if OwnerPersistent <> nil then
      NewCaption := OwnerPersistent.GetNamePath
    else
      NewCaption := '';

  if NewCaption <> '' then NewCaption := NewCaption + '.';
  NewCaption := oiColEditEditing + ' ' + NewCaption + PropertyName;

  if CollectionListBox.ItemIndex > -1 then
    NewCaption := NewCaption + '[' + IntToStr(CollectionListBox.ItemIndex) + ']';
  Caption := NewCaption;
end;

procedure TCollectionPropertyEditorForm.UpdateButtons;
var
  I: Integer;
begin
  I := CollectionListBox.ItemIndex;
  AddButton.Enabled := Collection <> nil;
  DeleteButton.Enabled := I > -1;
  MoveUpButton.Enabled := I > 0;
  MoveDownButton.Enabled := (I >= 0) and (I < Collection.Count - 1);
end;

procedure TCollectionPropertyEditorForm.ComponentRenamed(AComponent: TComponent);
begin
  if AComponent = OwnerPersistent then UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.PersistentDeleting(APersistent: TPersistent);
var
  OldCollection: TCollection;
begin
  //debugln('TCollectionPropertyEditorForm.PersistentDeleting A APersistent=',dbgsName(APersistent),' OwnerPersistent=',dbgsName(OwnerPersistent));
  if APersistent = OwnerPersistent then
  begin
    OldCollection := Collection;
    SetCollection(nil, nil, '');
    GlobalDesignHook.Unselect(OldCollection);
    if GlobalDesignHook.LookupRoot = OldCollection then
      GlobalDesignHook.LookupRoot := nil;

    Hide;
  end;
end;

procedure TCollectionPropertyEditorForm.RefreshPropertyValues;
begin
  FillCollectionListBox;
end;

procedure TCollectionPropertyEditorForm.FillCollectionListBox;
var
  I: Integer;
  CurItem: String;
  Cnt: Integer;
begin
  CollectionListBox.Items.BeginUpdate;
  try
    if Collection <> nil then Cnt := Collection.Count
    else Cnt := 0;

    // add or replace list items
    for I := 0 to Cnt - 1 do
    begin
      CurItem := IntToStr(I) + ' - ' + Collection.Items[I].DisplayName;
      if I >= CollectionListBox.Items.Count then
        CollectionListBox.Items.Add(CurItem)
      else
        CollectionListBox.Items[I] := CurItem;
    end;

    // delete unneeded list items
    if Cnt > 0 then
    begin
      while CollectionListBox.Items.Count > Cnt do
      begin
        CollectionListBox.Items.Delete(CollectionListBox.Items.Count - 1);
      end;
    end
    else
    begin
      CollectionListBox.Items.Clear;
    end;
  finally
    CollectionListBox.Items.EndUpdate;
    UpdateButtons;
    UpdateCaption;
  end;
end;

procedure TCollectionPropertyEditorForm.SelectInObjectInspector(UnselectAll: Boolean);
var
  I: Integer;
  NewSelection: TPersistentSelectionList;
begin
  if Collection = nil then Exit;
  // select in OI
  NewSelection := TPersistentSelectionList.Create;
  try
    if not UnselectAll then
    begin
      for I := 0 to CollectionListBox.Items.Count - 1 do
        if CollectionListBox.Selected[I] then
          NewSelection.Add(Collection.Items[I]);
    end;
    GlobalDesignHook.SetSelection(NewSelection);
    GlobalDesignHook.LookupRoot := GetLookupRootForComponent(OwnerPersistent);
  finally
    NewSelection.Free;
  end;
end;

procedure TCollectionPropertyEditorForm.SetCollection(NewCollection: TCollection;
  NewOwnerPersistent: TPersistent; const NewPropName: String);
begin
  if (FCollection = NewCollection) and (FOwnerPersistent = NewOwnerPersistent)
    and (FPropertyName = NewPropName) then Exit;

  FCollection := NewCollection;
  FOwnerPersistent := NewOwnerPersistent;
  FPropertyName := NewPropName;
  //debugln('TCollectionPropertyEditorForm.SetCollection A Collection=',dbgsName(FCollection),' OwnerPersistent=',dbgsName(OwnerPersistent),' PropName=',PropertyName);
  if GlobalDesignHook <> nil then
  begin
    if FOwnerPersistent <> nil then
    begin
      GlobalDesignHook.AddHandlerComponentRenamed(@ComponentRenamed);
      GlobalDesignHook.AddHandlerPersistentDeleting(@PersistentDeleting);
      GlobalDesignHook.AddHandlerRefreshPropertyValues(@RefreshPropertyValues);
    end
    else
    begin
      GlobalDesignHook.RemoveAllHandlersForObject(Self);
    end;
  end;

  FillCollectionListBox;
  UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.Modified;
begin
  GlobalDesignHook.Modified(Self);
end;

end.

