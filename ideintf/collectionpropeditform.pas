unit CollectionPropEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, StdCtrls, ActnList, LCLType;

type
  { TCollectionPropertyEditorForm }

  TCollectionPropertyEditorForm = class(TForm)
    actAdd: TAction;
    actDel: TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    ActionList1: TActionList;
    CollectionListBox: TListBox;
    ToolBar1: TToolBar;
    AddButton: TToolButton;
    DeleteButton: TToolButton;
    ToolButton3: TToolButton;
    MoveUpButton: TToolButton;
    MoveDownButton: TToolButton;
    procedure actAddExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure CollectionListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    procedure SelectInObjectInspector(ForceUpdate, UnselectAll: Boolean);
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
  actAdd.Caption := oiColEditAdd;
  actDel.Caption := oiColEditDelete;
  actMoveUp.Caption := oiColEditUp;
  actMoveDown.Caption := oiColEditDown;
  actAdd.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  actDel.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  actMoveUp.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
  actMoveDown.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');
  actMoveUp.ShortCut := scCtrl or VK_UP;
  actMoveDown.ShortCut := scCtrl or VK_DOWN;

  actAdd.Hint := oiColEditAdd;
  actDel.Hint := oiColEditDelete;
  actMoveUp.Hint := oiColEditUp;
  actMoveDown.Hint := oiColEditDown;
end;

procedure TCollectionPropertyEditorForm.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TCollectionPropertyEditorForm.CollectionListBoxClick(Sender: TObject);
begin
  UpdateButtons;
  UpdateCaption;
  SelectInObjectInspector(False, False);
end;

procedure TCollectionPropertyEditorForm.actAddExecute(Sender: TObject);
begin
  if Collection = nil then Exit;
  Collection.Add;

  FillCollectionListBox;
  if CollectionListBox.Items.Count > 0 then
    CollectionListBox.ItemIndex := CollectionListBox.Items.Count - 1;
  SelectInObjectInspector(True, False);
  UpdateButtons;
  UpdateCaption;
  Modified;
end;

procedure TCollectionPropertyEditorForm.actDelExecute(Sender: TObject);
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
      SelectInObjectInspector(True, True);
      // now delete
      Collection.Items[I].Free;
      // update listbox after whatever happened
      FillCollectionListBox;
      // set NewItemIndex
      if NewItemIndex < CollectionListBox.Items.Count then
      begin
        CollectionListBox.ItemIndex := NewItemIndex;
        SelectInObjectInspector(False, False);
      end;
      //debugln('TCollectionPropertyEditorForm.DeleteClick B');
      Modified;
    end;
  end;
  UpdateButtons;
  UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.actMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I >= Collection.Count - 1 then Exit;

  Collection.Items[I].Index := I + 1;
  CollectionListBox.ItemIndex := I + 1;

  FillCollectionListBox;
  SelectInObjectInspector(True, False);
  Modified;
end;

procedure TCollectionPropertyEditorForm.actMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I < 0 then Exit;

  Collection.Items[I].Index := I - 1;
  CollectionListBox.ItemIndex := I - 1;

  FillCollectionListBox;
  SelectInObjectInspector(True, False);
  Modified;
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
  actAdd.Enabled := Collection <> nil;
  actDel.Enabled := I > -1;
  actMoveUp.Enabled := I > 0;
  actMoveDown.Enabled := (I >= 0) and (I < Collection.Count - 1);
end;

procedure TCollectionPropertyEditorForm.ComponentRenamed(AComponent: TComponent);
begin
  if AComponent = OwnerPersistent then UpdateCaption;
end;

procedure TCollectionPropertyEditorForm.PersistentDeleting(APersistent: TPersistent);
var
  OldCollection: TCollection;
  AIndex, I: Integer;
begin
  if APersistent = OwnerPersistent then
  begin
    OldCollection := Collection;
    SetCollection(nil, nil, '');
    GlobalDesignHook.Unselect(OldCollection);
    if GlobalDesignHook.LookupRoot = OldCollection then
      GlobalDesignHook.LookupRoot := nil;

    Hide;
  end
  else
  if Assigned(Collection) and (APersistent is TCollectionItem) and
    (TCollectionItem(APersistent).Collection = Collection) then
  begin
    // persistent is still alive
    AIndex := CollectionListBox.ItemIndex;
    CollectionListBox.Items.BeginUpdate;
    CollectionListBox.Items.Delete(TCollectionItem(APersistent).Index);
    for I := TCollectionItem(APersistent).Index to CollectionListBox.Items.Count - 1 do
      CollectionListBox.Items[I] := IntToStr(I) + ' - ' + Collection.Items[I + 1].DisplayName;
    CollectionListBox.Items.EndUpdate;
    if AIndex < CollectionListBox.Items.Count then
      CollectionListBox.ItemIndex := AIndex
    else
      CollectionListBox.ItemIndex := CollectionListBox.Items.Count - 1;
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

procedure TCollectionPropertyEditorForm.SelectInObjectInspector(ForceUpdate, UnselectAll: Boolean);
var
  I: Integer;
  NewSelection: TPersistentSelectionList;
begin
  if Collection = nil then Exit;
  // select in OI
  NewSelection := TPersistentSelectionList.Create;
  NewSelection.ForceUpdate := ForceUpdate;
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

