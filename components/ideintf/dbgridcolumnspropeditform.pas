unit DBGridColumnsPropEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, db,
  // LCL
  Controls, Dialogs, LCLProc, Forms, ComCtrls, StdCtrls, ActnList, LCLType, DBGrids,
  // IdeIntf
  IDEImagesIntf, ObjInspStrConsts, PropEdits, PropEditUtils;

type
  { TDBGridColumnsPropertyEditorForm }

  TDBGridColumnsPropertyEditorForm = class(TForm)
    actAdd: TAction;
    actDel: TAction;
    actAddFields: TAction;
    actDeleteAll: TAction;
    actFetchLabels: TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    ActionList1: TActionList;
    CollectionListBox: TListBox;
    DividerToolButton1: TToolButton;
    DividerToolButton2: TToolButton;
    DividerToolButton3: TToolButton;
    ToolBar1: TToolBar;
    AddButton: TToolButton;
    DeleteButton: TToolButton;
    DividerToolButton: TToolButton;
    MoveUpButton: TToolButton;
    MoveDownButton: TToolButton;
    btAddFlds: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure actAddExecute(Sender: TObject);
    procedure actAddFieldsExecute(Sender: TObject);
    procedure actDeleteAllExecute(Sender: TObject);
    procedure actDelExecute(Sender: TObject);
    procedure actFetchLabelsExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure CollectionListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCollection: TCollection;
    FOwnerComponent: TPersistent;
    FOwnerPersistent: TPersistent;
    FPropertyName: String;
    procedure FillCollectionListBox;
    procedure SelectInObjectInspector;
    procedure UnSelectInObjectInspector;
    procedure UpdDesignHook(aSelection: TPersistentSelectionList);
  protected
    procedure UpdateCaption;
    procedure UpdateButtons;
    procedure PersistentAdded({%H-}APersistent: TPersistent; {%H-}Select: boolean);
    procedure ComponentRenamed(AComponent: TComponent);
    procedure PersistentDeleting(APersistent: TPersistent);
    procedure RefreshPropertyValues;
  public
    procedure SetCollection(NewCollection: TCollection;
                    NewOwnerPersistent: TPersistent; const NewPropName: String);
    procedure Modified;
  public
    property Collection: TCollection read FCollection;
    property OwnerComponent: TPersistent read FOwnerComponent;
    property OwnerPersistent: TPersistent read FOwnerPersistent;
    property PropertyName: String read FPropertyName;
  end;

implementation

{$R *.lfm}

type
  TPersistentAccess = class(TPersistent);

procedure TDBGridColumnsPropertyEditorForm.FormCreate(Sender: TObject);
begin
  ToolBar1.Images := IDEImages.Images_16;
  actAdd.Caption := oiColEditAdd;
  actAddFields.Caption := dceAddFields;
  actAddFields.ImageIndex := IDEImages.LoadImage('laz_add');
  actFetchLabels.Caption := dceFetchLabels;
  actFetchLabels.ImageIndex := IDEImages.LoadImage('laz_add');
  actDel.Caption := oiColEditDelete;
  actMoveUp.Caption := oiColEditUp;
  actMoveDown.Caption := oiColEditDown;
  actAdd.ImageIndex := IDEImages.LoadImage('laz_add');
  actDel.ImageIndex := IDEImages.LoadImage('laz_delete');
  actDeleteAll.Caption := dceDeleteAll;
  actDeleteAll.ImageIndex := IDEImages.LoadImage('laz_delete');
  actMoveUp.ImageIndex := IDEImages.LoadImage('arrow_up');
  actMoveDown.ImageIndex := IDEImages.LoadImage('arrow_down');
  actMoveUp.ShortCut := scCtrl or VK_UP;
  actMoveDown.ShortCut := scCtrl or VK_DOWN;

  actAdd.Hint := oiColEditAdd;
  actDel.Hint := oiColEditDelete;
  actMoveUp.Hint := oiColEditUp;
  actMoveDown.Hint := oiColEditDown;
end;

procedure TDBGridColumnsPropertyEditorForm.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TDBGridColumnsPropertyEditorForm.CollectionListBoxClick(Sender: TObject);
begin
  UpdateButtons;
  UpdateCaption;
  SelectInObjectInspector;
end;

procedure TDBGridColumnsPropertyEditorForm.actAddExecute(Sender: TObject);
begin
  if Collection = nil then Exit;
  Collection.Add;

  FillCollectionListBox;
  if CollectionListBox.Items.Count > 0 then
    CollectionListBox.ItemIndex := CollectionListBox.Items.Count - 1;
  SelectInObjectInspector;
  UpdateButtons;
  UpdateCaption;
  Modified;
end;

procedure TDBGridColumnsPropertyEditorForm.actAddFieldsExecute(Sender: TObject);
var It : TCollectionItem;
  flChanged : Boolean;
  flEmpty : Boolean;
  i : Integer;
begin
  if Collection = nil then Exit;
  if not ( Collection is TDBGridColumns ) then Exit;

  flChanged := False;
  flEmpty := False;
  if Collection.Count > 0 then
    It := Collection.Items[ 0 ]
  else begin
    It := Collection.Add;
    flEmpty:=True;
  end;

  if flEmpty or (MessageDlg(dceColumnEditor, dceOkToDelete, mtConfirmation,
                            [mbYes, mbNo], 0) = mrYes) then
  begin

    try
      if (It is TColumn) and ( Assigned( TDBGrid( TColumn( It ).Grid).DataSource ) ) and
        Assigned ( TDBGrid( TColumn( It ).Grid).DataSource.DataSet ) then begin
        flChanged:=True;
        BeginFormUpdate;
        Collection.Clear;
        It := Collection.Add;
        //TDBGrid( TColumn( It ).Grid).DataSource.DataSet.Open;
        if TDBGrid( TColumn( It ).Grid).DataSource.DataSet.Fields.Count > 0 then begin
          for i := 0 to TDBGrid( TColumn( It ).Grid).DataSource.DataSet.Fields.Count - 1 do begin
            if i > 0 then
              It := Collection.Add;
            TColumn( It ).Field:=TDBGrid( TColumn( It ).Grid).DataSource.DataSet.Fields[ i ];
            TColumn( It ).Title.Caption:=TDBGrid( TColumn( It ).Grid).DataSource.DataSet.Fields[ i ].DisplayLabel;
            end;
          end;
        end;

    finally
      if flChanged then begin
        RefreshPropertyValues;
        UpdateButtons;
        UpdateCaption;
        Modified;
        EndFormUpdate;
        end
      else
        if flEmpty then begin
          Collection.Clear;
          RefreshPropertyValues;
          UpdateButtons;
          UpdateCaption;
          Modified;
          EndFormUpdate;
          end;
    end;
  end;
end;

procedure TDBGridColumnsPropertyEditorForm.actDeleteAllExecute(Sender: TObject);
begin
  if Collection = nil then
    Exit;
  if (MessageDlg(dceColumnEditor, dceOkToDelete, mtConfirmation,
                 [mbYes, mbNo], 0) = mrYes) then
  begin
    try
      BeginFormUpdate;
      UnSelectInObjectInspector;
      Collection.Clear;
    finally
      RefreshPropertyValues;
      UpdateButtons;
      UpdateCaption;
      Modified;
      EndFormUpdate;
    end;
  end;
end;

procedure TDBGridColumnsPropertyEditorForm.actDelExecute(Sender: TObject);
var
  I : Integer;
  NewItemIndex: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if (I >= 0) and (I < Collection.Count) then
  begin
    if MessageDlg(oisConfirmDelete,
      Format(oisDeleteItem, [Collection.Items[I].DisplayName]),
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
      //debugln('TDBGridColumnsPropertyEditorForm.DeleteClick A NewItemIndex=',dbgs(NewItemIndex),' ItemIndex=',dbgs(CollectionListBox.ItemIndex),' CollectionListBox.Items.Count=',dbgs(CollectionListBox.Items.Count),' Collection.Count=',dbgs(Collection.Count));
      // unselect all items in OI (collections can act strange on delete)
      UnSelectInObjectInspector;
      // now delete
      Collection.Items[I].Free;
      // update listbox after whatever happened
      FillCollectionListBox;
      // set NewItemIndex
      if NewItemIndex < CollectionListBox.Items.Count then
      begin
        CollectionListBox.ItemIndex := NewItemIndex;
        SelectInObjectInspector;
      end;
      //debugln('TDBGridColumnsPropertyEditorForm.DeleteClick B');
      Modified;
    end;
  end;
  UpdateButtons;
  UpdateCaption;
end;

procedure TDBGridColumnsPropertyEditorForm.actFetchLabelsExecute(Sender: TObject);
var
  Column: TColumn;
  DataSet: TDataSet;
  Field: TField;
  i: Integer;
begin
  if (FOwnerPersistent as TDBGrid).DataSource=nil then Exit;
  DataSet:=TDBGrid(FOwnerPersistent).DataSource.DataSet;
  if DataSet=nil then Exit;

  if MessageDlg(dceColumnEditor, dceWillReplaceContinue, mtConfirmation,
    [mbYes, mbNo], 0)<>mrYes then Exit;

  for i:=0 to FCollection.Count-1 do
  begin
    Column:=FCollection.Items[i] as TColumn;
    Field:= DataSet.FindField(Column.FieldName);
    if Field<>nil then
      Column.Title.Caption:=Field.DisplayLabel;
  end;
end;

procedure TDBGridColumnsPropertyEditorForm.actMoveDownExecute(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I >= Collection.Count - 1 then Exit;

  Collection.Items[I].Index := I + 1;
  CollectionListBox.ItemIndex := I + 1;

  FillCollectionListBox;
  SelectInObjectInspector;
  Modified;
end;

procedure TDBGridColumnsPropertyEditorForm.actMoveUpExecute(Sender: TObject);
var
  I: Integer;
begin
  if Collection = nil then Exit;

  I := CollectionListBox.ItemIndex;
  if I < 0 then Exit;

  Collection.Items[I].Index := I - 1;
  CollectionListBox.ItemIndex := I - 1;

  FillCollectionListBox;
  SelectInObjectInspector;
  Modified;
end;

procedure TDBGridColumnsPropertyEditorForm.UpdateCaption;
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

procedure TDBGridColumnsPropertyEditorForm.UpdateButtons;
var
  I: Integer;
begin
  I := CollectionListBox.ItemIndex;
  actAdd.Enabled := Collection <> nil;
  actFetchLabels.Enabled:=actAdd.Enabled and (CollectionListBox.Items.Count > 0);
  actDel.Enabled := I > -1;
  actMoveUp.Enabled := I > 0;
  actMoveDown.Enabled := (I >= 0) and (I < CollectionListBox.Items.Count - 1);
end;

procedure TDBGridColumnsPropertyEditorForm.PersistentAdded(APersistent: TPersistent; Select: boolean);
begin
  //DebugLn('*** TDBGridColumnsPropertyEditorForm.PersistentAdded called ***');
  FillCollectionListBox;
end;

procedure TDBGridColumnsPropertyEditorForm.ComponentRenamed(AComponent: TComponent);
begin
  //DebugLn('*** TDBGridColumnsPropertyEditorForm.ComponentRenamed called ***');
  if AComponent = OwnerPersistent then
    UpdateCaption;
end;

procedure TDBGridColumnsPropertyEditorForm.PersistentDeleting(APersistent: TPersistent);
begin
  // For some reason this is called only when the whole collection is deleted,
  // for example when changing to another project. Thus clear the whole collection.
  DebugLn(['TDBGridColumnsPropertyEditorForm.PersistentDeleting: APersistent=', APersistent,
           ', OwnerPersistent=', OwnerPersistent, ', OwnerComponent=', OwnerComponent]);
  SetCollection(nil, nil, '');
  Hide;
  UpdateButtons;
  UpdateCaption;
end;

procedure TDBGridColumnsPropertyEditorForm.RefreshPropertyValues;
begin
  FillCollectionListBox;
  //DebugLn('*** TDBGridColumnsPropertyEditorForm.RefreshPropertyValues called ***');
end;

procedure TDBGridColumnsPropertyEditorForm.FillCollectionListBox;
var
  I: Integer;
  CurItem: String;
  Cnt: Integer;
begin
  CollectionListBox.Items.BeginUpdate;
  try
    if Collection <> nil then
      Cnt := Collection.Count
    else
      Cnt := 0;

    // add or replace list items
    for I := 0 to Cnt - 1 do
    begin
      CurItem := IntToStr(I) + ' - ' + Collection.Items[I].DisplayName;// +' '+ TDBGrid(TColumn(Collection.Items[I]).Grid).DataSource.DataSet.FieldByName( TColumn(Collection.Items[I]).FieldName ).DisplayLabel ;// + ' - '+Collection.Items[I].ClassName +' - ' + Collection.ClassName;

      DividerToolButton1.Visible := (Collection is TDBGridColumns);
      btAddFlds.Visible := DividerToolButton1.Visible;
      actAddFields.Enabled := DividerToolButton1.Visible;
      if I >= CollectionListBox.Items.Count then
        CollectionListBox.Items.Add(CurItem)
      else
        CollectionListBox.Items[I] := CurItem;
    end;

    // delete unneeded list items
    if Cnt > 0 then
      while CollectionListBox.Items.Count > Cnt do
        CollectionListBox.Items.Delete(CollectionListBox.Items.Count - 1)
    else
      CollectionListBox.Items.Clear;
  finally
    CollectionListBox.Items.EndUpdate;
    UpdateButtons;
    UpdateCaption;
  end;
end;

procedure TDBGridColumnsPropertyEditorForm.SelectInObjectInspector;
var
  I: Integer;
  NewSelection: TPersistentSelectionList;
begin
  Assert(Assigned(Collection), 'SelectInObjectInspector: Collection=Nil.');
  // select in OI
  NewSelection := TPersistentSelectionList.Create;
  try
    for I := 0 to CollectionListBox.Items.Count - 1 do
      if CollectionListBox.Selected[I] then
        NewSelection.Add(Collection.Items[I]);
    UpdDesignHook(NewSelection);
  finally
    NewSelection.Free;
  end;
end;

procedure TDBGridColumnsPropertyEditorForm.UnSelectInObjectInspector;
var
  EmptySelection: TPersistentSelectionList;
begin
  EmptySelection := TPersistentSelectionList.Create;
  try
    UpdDesignHook(EmptySelection);
  finally
    EmptySelection.Free;
  end;
end;

procedure TDBGridColumnsPropertyEditorForm.UpdDesignHook(aSelection: TPersistentSelectionList);
begin
  if GlobalDesignHook = nil then Exit;
  GlobalDesignHook.SetSelection(aSelection);
  GlobalDesignHook.LookupRoot := GetLookupRootForComponent(OwnerPersistent);
end;

procedure TDBGridColumnsPropertyEditorForm.SetCollection(NewCollection: TCollection;
  NewOwnerPersistent: TPersistent; const NewPropName: String);
begin
  if (FCollection = NewCollection) and (FOwnerPersistent = NewOwnerPersistent)
    and (FPropertyName = NewPropName) then Exit;

  FCollection := NewCollection;
  FOwnerPersistent := NewOwnerPersistent;
  FPropertyName := NewPropName;
  //find the component that owns the collection
  FOwnerComponent := NewOwnerPersistent;
  while FOwnerComponent <> nil do
  begin
    if FOwnerComponent is TComponent then
      break;
    FOwnerComponent := TPersistentAccess(FOwnerComponent).GetOwner;
  end;
  //debugln('TDBGridColumnsPropertyEditorForm.SetCollection A Collection=',dbgsName(FCollection),' OwnerPersistent=',dbgsName(OwnerPersistent),' PropName=',PropertyName);
  if GlobalDesignHook <> nil then
  begin
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
    if FOwnerPersistent <> nil then
    begin
      GlobalDesignHook.AddHandlerPersistentAdded(@PersistentAdded);
      GlobalDesignHook.AddHandlerComponentRenamed(@ComponentRenamed);
      GlobalDesignHook.AddHandlerPersistentDeleting(@PersistentDeleting);
      GlobalDesignHook.AddHandlerRefreshPropertyValues(@RefreshPropertyValues);
    end;
  end;

  FillCollectionListBox;
  UpdateCaption;
end;

procedure TDBGridColumnsPropertyEditorForm.Modified;
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.Modified(Self);
end;

end.

