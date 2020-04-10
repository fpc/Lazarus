{ Copyright (C) 2005 Alexandru Alexandrov
  Date: 11.06.2005

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Modified Date: 20.10.2010
 By: Marcelo Borges de Paula
}
unit FieldsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db,
  // LCL
  LCLType, Forms, Controls, Menus, Dialogs, ComCtrls, ActnList, StdCtrls,
  // IdeIntf
  ObjInspStrConsts, ComponentEditors, PropEdits, PropEditUtils,
  NewField, FieldsList, IDEImagesIntf, IDEWindowIntf;

type

  TFieldsComponentEditor = class;

  { TDSFieldsEditorFrm }

  TDSFieldsEditorFrm = class(TForm)
    Fields: TImageList;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    tbCommands: TToolBar;
    tbAddFld: TToolButton;
    tbUnselect: TToolButton;
    tbDeleteFld: TToolButton;
    tbNewFld: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    tbMoveUp: TToolButton;
    tbMoveDown: TToolButton;
    ToolButton8: TToolButton;
    tbSelect: TToolButton;
    UnselectAllActn: TAction;
    SelectAllActn: TAction;
    FieldsListBox: TListBox;
    MoveDownActn: TAction;
    MoveUpActn: TAction;
    NewActn: TAction;
    DeleteFieldsActn: TAction;
    AddFieldsActn: TAction;
    ActionList1: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    PopupMenu1: TPopupMenu;
    procedure ActionList1Update({%H-}AAction: TBasicAction; var {%H-}Handled: Boolean);
    procedure AddFieldsActnExecute(Sender: TObject);
    procedure DeleteFieldsActnExecute(Sender: TObject);
    procedure FieldsEditorFrmClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure FieldsEditorFrmDestroy(Sender: TObject);
    procedure FieldsListBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
      ARect: TRect; {%H-}State: TOwnerDrawState);
    procedure FieldsListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure NewActnExecute(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure MoveDownActnExecute(Sender: TObject);
    procedure MoveUpActnExecute(Sender: TObject);
    procedure SelectAllActnExecute(Sender: TObject);
    procedure UnselectAllActnExecute(Sender: TObject);
  protected
    procedure DoSelected(All: boolean);
    procedure SelectionChanged(AOrderChanged: Boolean = false);
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
  private
    LinkDataset: TDataset;
    FDesigner: TComponentEditorDesigner;
    FComponentEditor: TFieldsComponentEditor;
    FUpdateSelectionCount: Integer;
    procedure BeginUpdateSelection;
    procedure EndUpdateSelection;
    procedure ExchangeItems(const fFirst, fSecond: integer);
    procedure RefreshFieldsListBox(SelectAllNew: boolean);
    function FindChild(ACandidate: TPersistent; out AIndex: Integer): Boolean;
  public
    constructor Create(AOwner: TComponent; ADataset: TDataset;
      ADesigner: TComponentEditorDesigner); reintroduce;
    destructor Destroy; override;
    property Designer: TComponentEditorDesigner read FDesigner write FDesigner;
    property ComponentEditor: TFieldsComponentEditor write FComponentEditor;
  end;

  { TFieldsComponentEditor }

  TFieldsComponentEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

{$R *.lfm}

{ TDSFieldsEditorFrm }

constructor TDSFieldsEditorFrm.Create(AOwner: TComponent; ADataset: TDataset;
    ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AOwner);

  tbCommands.Images := IDEImages.Images_16;
  tbAddFld.ImageIndex := IDEImages.LoadImage('laz_add');
  tbDeleteFld.ImageIndex := IDEImages.LoadImage('laz_delete');
  tbNewFld.ImageIndex := IDEImages.LoadImage('menu_new');
  tbMoveDown.ImageIndex := IDEImages.LoadImage('arrow_down');
  tbMoveUp.ImageIndex := IDEImages.LoadImage('arrow_up');
  tbSelect.ImageIndex := IDEImages.LoadImage('menu_select_all');
  tbUnselect.ImageIndex := IDEImages.LoadImage('menu_close_all');

  LinkDataset := ADataset;
  FDesigner := ADesigner;
  Caption := fesFeTitle + ' - ' + LinkDataset.Name;
  AddFieldsActn.Caption := oisAddFields;
  AddFieldsActn.Hint := oisAddFieldsFromFieldDefs;
  DeleteFieldsActn.Caption:=oisDeleteComponents;
  DeleteFieldsActn.Hint:=oisDeleteSelectedFieldS;
  NewActn.Caption:=oisNew;
  NewActn.Hint:=oisCreateNewFieldAndAddItAtCurrentPosition;
  MoveUpActn.Caption:=oisMoveUp;
  MoveUpActn.Hint:=oisMoveUpHint;
  MoveDownActn.Caption:=oisMoveDown;
  MoveDownActn.Hint:=oisMoveDownHint;
  SelectAllActn.Caption:=oisSelectAll;
  SelectAllActn.Hint:=oisSelectAllHint;
  UnselectAllActn.Caption:=oisUnselectAll;
  UnselectAllActn.Hint:=oisUnselectAllHint;

  FieldsListBox.Clear;
  RefreshFieldsListBox(False);

  if Assigned(GlobalDesignHook) then
  begin
    GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
    GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
    GlobalDesignHook.AddHandlerGetSelection(@OnGetSelection);
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
    GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
  end;

  SelectionChanged;
end;

destructor TDSFieldsEditorFrm.Destroy;
begin
  UnregisterEditorForm(Self);
  inherited Destroy;
end;

procedure TDSFieldsEditorFrm.FormCreate(Sender: TObject);
begin
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TDSFieldsEditorFrm.FieldsEditorFrmClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  CloseAction := caFree;
end;

procedure TDSFieldsEditorFrm.AddFieldsActnExecute(Sender: TObject);
var FieldsList: TFieldsListFrm;
begin
  try
    FieldsList :=  TFieldsListFrm.Create(Self, LinkDataset, Designer);
  except
    on E:Exception do begin
      ShowMessage(fesNoFields+^M+fesCheckDSet+^M^M+E.Message);
      exit;
    end;
  end;
  try
    FieldsList.ShowModal;
  finally
    FieldsList.Free;
  end;
  SelectionChanged;
end;

procedure TDSFieldsEditorFrm.DeleteFieldsActnExecute(Sender: TObject);
var
  PreActive: boolean;
begin
  PreActive := LinkDataSet.Active;
  LinkDataSet.Active := False;
  if FieldsListBox.SelCount = 0 then
    exit;
  BeginUpdateSelection;
  FDesigner.DeleteSelection;
  EndUpdateSelection;
  if PreActive then
    LinkDataSet.Active := True;
end;

procedure TDSFieldsEditorFrm.FieldsEditorFrmDestroy(Sender: TObject);
begin
  if GlobalDesignHook = Nil then
    Exit;
  if Assigned(FComponentEditor) and Assigned(LinkDataset)
  and not (csDestroying in LinkDataset.ComponentState)
  and (FieldsListBox.SelCount > 0) then
    GlobalDesignHook.SelectOnlyThis(LinkDataset);
  GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TDSFieldsEditorFrm.FieldsListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  fld: TField;
begin
  if Index < 0 then Exit;
  if not Assigned(FieldsListBox.Items.Objects[Index]) then Exit;
  //
  FieldsListBox.Canvas.FillRect(ARect);
  fld := TField(FieldsListBox.Items.Objects[Index]);
  //
  if pfinKey in fld.ProviderFlags then
    Fields.Draw(FieldsListBox.Canvas,1,ARect.Top,0)
  else
    case fld.FieldKind of
      fkData         : Fields.Draw(FieldsListBox.Canvas,1,ARect.Top,1);
      fkCalculated   : Fields.Draw(FieldsListBox.Canvas,1,ARect.Top,2);
      fkLookup       : Fields.Draw(FieldsListBox.Canvas,1,ARect.Top,3);
      fkInternalCalc : Fields.Draw(FieldsListBox.Canvas,1,ARect.Top,4);
    end;
  //
  FieldsListBox.Canvas.TextRect(ARect, ARect.Left + 20,ARect.Top,
                                FieldsListBox.Items[Index]);
end;

procedure TDSFieldsEditorFrm.FieldsListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      VK_UP: begin
        MoveUpActn.Execute;
        Key := 0;
      end;
      VK_DOWN: begin
        MoveDownActn.Execute;
        Key := 0;
      end;
    end;
end;

procedure TDSFieldsEditorFrm.ExchangeItems(const fFirst, fSecond: integer);
var SelFirst,
    SelSecond: boolean;
begin
  with FieldsListBox do begin
//  save selected
    SelFirst := Selected[fFirst];
    SelSecond := Selected[fSecond];
//  exchange items
    FieldsListBox.Items.Exchange(fFirst,fSecond);
//  restore selected
    Selected[fFirst] := SelSecond;
    Selected[fSecond] := SelFirst;

    TField(Items.Objects[fFirst]).Index := fFirst;
  end;
end;

procedure TDSFieldsEditorFrm.RefreshFieldsListBox(SelectAllNew: boolean);
var i, j: integer;
    fld: TField;
    PreActive: boolean;
begin
  PreActive := LinkDataSet.Active;
  if PreActive And LinkDataset.DefaultFields then
    LinkDataset.Close;
  //Deselect & refresh all existing
  FieldsListBox.ClearSelection;
  //Add new fields
  for i := 0 to LinkDataset.Fields.Count - 1 do begin
    fld := LinkDataset.Fields[i];
    if FieldsListBox.Items.IndexOfObject(fld) < 0 then begin
      j := FieldsListBox.Items.AddObject(fld.FieldName, fld);
      FieldsListBox.Selected[j] := SelectAllNew;
    end;
  end;
  if PreActive and not LinkDataset.Active then
    LinkDataset.Active:=true;
end;

function TDSFieldsEditorFrm.FindChild(ACandidate: TPersistent; out
  AIndex: Integer): Boolean;
begin
  if ACandidate is TField then
    AIndex := FieldsListBox.Items.IndexOfObject(ACandidate)
  else
    AIndex := -1;
  Result := AIndex >= 0;
end;

procedure TDSFieldsEditorFrm.NewActnExecute(Sender: TObject);
var nf: TNewFieldFrm;
begin
  nf := TNewFieldFrm.Create(Self, LinkDataset, Designer);
  try
    nf.ShowModal;
  finally
    nf.Free;
  end;
  SelectionChanged;
end;

procedure TDSFieldsEditorFrm.ListBox1Click(Sender: TObject);
begin
  SelectionChanged;
end;

procedure TDSFieldsEditorFrm.MoveDownActnExecute(Sender: TObject);
var i: integer;
    bModified: boolean;
begin
  if FieldsListBox.Selected[FieldsListBox.Items.Count - 1] then exit;
  bModified := False;
  for i := FieldsListBox.Items.Count - 2 downto 0 do
    if FieldsListBox.Selected[i] then begin
      ExchangeItems(i, i + 1);
      bModified := True;
    end;
  SelectionChanged(True);
  if bModified then fDesigner.Modified;
end;

procedure TDSFieldsEditorFrm.MoveUpActnExecute(Sender: TObject);
var i: integer;
    bModified: boolean;
begin
  if FieldsListBox.Selected[0] then exit;
  bModified := False;
  for i := 1 to FieldsListBox.Items.Count - 1 do
    if FieldsListBox.Selected[i] then begin
      ExchangeItems(i - 1, i);
      bModified := True;
    end;
  SelectionChanged(True);
  if bModified then fDesigner.Modified;
end;

procedure TDSFieldsEditorFrm.ActionList1Update(AAction: TBasicAction;
  var Handled: Boolean);
var
  b: boolean;
  i, SelectedCount: integer;
begin
  b := FieldsListBox.Count > 0;
  SelectedCount := 0;
  for i:= 0 to FieldsListBox.Count-1 do
    if FieldsListBox.Selected[i] then
      Inc(SelectedCount);

  DeleteFieldsActn.Enabled := b and (SelectedCount > 0);
  MoveDownActn.Enabled := b and (SelectedCount > 0)
    and (Not FieldsListBox.Selected[FieldsListBox.Items.Count - 1]);
  MoveUpActn.Enabled := b and (SelectedCount > 0)
    and (Not FieldsListBox.Selected[0]);
  SelectAllActn.Enabled := b and (FieldsListBox.Count <> SelectedCount);
  UnselectAllActn.Enabled := b and (SelectedCount > 0);
end;

procedure TDSFieldsEditorFrm.SelectAllActnExecute(Sender: TObject);
begin
  DoSelected(True);
  SelectionChanged;
end;

procedure TDSFieldsEditorFrm.UnselectAllActnExecute(Sender: TObject);
begin
  DoSelected(False);
  SelectionChanged;
end;

procedure TDSFieldsEditorFrm.DoSelected(All: boolean);
var i: integer;
begin
  for i := 0 to FieldsListBox.Items.Count - 1 do begin
    FieldsListBox.Items[i] := (FieldsListBox.Items.Objects[i] as TField).FieldName;
    FieldsListBox.Selected[i] := All;
  end;
end;

procedure TDSFieldsEditorFrm.SelectionChanged(AOrderChanged: Boolean = false);
var SelList: TPersistentSelectionList;
begin
  if (FUpdateSelectionCount>0) or (GlobalDesignHook=nil) then
    exit;
  GlobalDesignHook.RemoveHandlerSetSelection(@OnSetSelection);
  try
    SelList := TPersistentSelectionList.Create;
    SelList.ForceUpdate := AOrderChanged;
    try
      OnGetSelection(SelList);
      FDesigner.PropertyEditorHook.SetSelection(SelList) ;
    finally
      SelList.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
  ActionList1.UpdateAction(nil);
end;

procedure TDSFieldsEditorFrm.OnComponentRenamed(AComponent: TComponent);
var Field: TField;
    i: integer;
begin
  if AComponent is TField then begin
    Field := TField(AComponent);
    if not Assigned( Field ) then Exit;
    i := FieldsListBox.Items.IndexOfObject(Field);
    if i >= 0 then
      FieldsListBox.Items[i] := Field.FieldName;
  end else
  if (AComponent is TDataset) And (AComponent = LinkDataset) then
    Caption := fesFeTitle + ' - ' + LinkDataset.Name;
end;

procedure TDSFieldsEditorFrm.OnPersistentDeleting(APersistent: TPersistent);
var i: integer;
begin
  if FindChild(APersistent, i) then
    FieldsListBox.Items.Delete(i);
end;

procedure TDSFieldsEditorFrm.OnGetSelection(
  const ASelection: TPersistentSelectionList);
var i: integer;
begin
  if Not Assigned(ASelection) then exit;
  if ASelection.Count > 0 then ASelection.Clear;
  for i := 0 to FieldsListBox.Items.Count - 1 do
    if FieldsListBox.Selected[i] then
      ASelection.Add(TPersistent(FieldsListBox.Items.Objects[i]));
end;

procedure TDSFieldsEditorFrm.OnSetSelection(
  const ASelection: TPersistentSelectionList);
var i, j: integer;
begin
  if Assigned(ASelection) then begin
    //Unselect all
    FieldsListBox.ClearSelection;
    //select from list
    for i := 0 to ASelection.Count - 1 do
      if FindChild(ASelection.Items[i], j) then
        FieldsListBox.Selected[j] := true;
  end;
end;

procedure TDSFieldsEditorFrm.OnPersistentAdded(APersistent: TPersistent;
  Select: boolean);
var fld: TField;
begin
  if Assigned(APersistent) And
     (APersistent is TField) And
     ((APersistent as TField).DataSet = LinkDataset) then begin
       fld := APersistent as TField;
       with FieldsListBox do
         Selected[Items.AddObject(fld.FieldName, fld)] := Select;
  end;
end;

procedure TDSFieldsEditorFrm.BeginUpdateSelection;
begin
  Inc(FUpdateSelectionCount);
end;

procedure TDSFieldsEditorFrm.EndUpdateSelection;
begin
  dec(FUpdateSelectionCount);
  if FUpdateSelectionCount=0 then
    SelectionChanged;
end;

{ TFieldsComponentEditor }

function TFieldsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TFieldsComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := fesFeTitle;
    else Result := '';
  end;
end;

procedure TFieldsComponentEditor.ExecuteVerb(Index: Integer);
var
  ADataset: TDataset;
  AEditor: TObject;
begin
  case index of
    0:
      begin
        ADataset := GetComponent as TDataset;
        if ADataset=nil then
           raise Exception.Create('TFieldsComponentEditor.Edit LinkDataset=nil');

        AEditor := FindEditorForm(ADataset);
        if AEditor=nil then begin
          AEditor := TDSFieldsEditorFrm.Create(Application, ADataset, Designer);
          RegisterEditorForm(AEditor, ADataset);
        end;

        if AEditor<>nil then
          with TDsFieldsEditorFrm(AEditor) do begin
            ComponentEditor := Self;
            ShowOnTop;
          end;
      end;
  end;
end;

initialization
  RegisterComponentEditor(TDataset, TFieldsComponentEditor);
end.
