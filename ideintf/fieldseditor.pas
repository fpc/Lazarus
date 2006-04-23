{ Copyright (C) 2005 Alexandru Alexandrov
  Date: 11.06.2005

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit fieldseditor;

{$mode objfpc}{$H+}

interface

{$IFNDEF VER2_0}
uses
  Classes, SysUtils, LResources, TypInfo, LCLProc, Forms,
  Controls, Menus, Graphics, Dialogs, ComCtrls,
  db, ActnList, StdCtrls, ComponentEditors, PropEdits, LCLType,
  NewField, FieldsList, ComponentReg;

type

  TFieldsComponentEditor = class;

  { TDSFieldsEditorFrm }

  TDSFieldsEditorFrm = class(TForm)
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
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
    procedure AddFieldsActnExecute(Sender: TObject);
    procedure DeleteFieldsActnExecute(Sender: TObject);
    procedure FieldsEditorFrmClose(Sender: TObject;
      var CloseAction: TCloseAction);
    procedure FieldsEditorFrmDestroy(Sender: TObject);
    procedure FieldsListBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure NewActnExecute(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure MoveDownActnExecute(Sender: TObject);
    procedure MoveUpActnExecute(Sender: TObject);
    procedure SelectAllActnExecute(Sender: TObject);
    procedure UnselectAllActnExecute(Sender: TObject);
  protected
    { protected declarations }
    procedure DoSelected(All: boolean);
    procedure SelectionChanged;
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
  private
    { private declarations }
    LinkDataset: TDataset;
    FDesigner: TComponentEditorDesigner;
    FComponentEditor: TFieldsComponentEditor;
    procedure ExchangeItems(const fFirst, fSecond: integer);
    procedure RefreshFieldsListBox(SelectAllNew: boolean);
  public
    { public declarations }
    constructor Create(AOwner: TComponent; ADataset: TDataset;
      ADesigner: TComponentEditorDesigner); reintroduce;
    property Designer: TComponentEditorDesigner read FDesigner write FDesigner;
    property ComponentEditor: TFieldsComponentEditor write FComponentEditor;
  end;

  { TActionListComponentEditor }

  TFieldsComponentEditor = class(TComponentEditor)
  private
    FDataSet: TDataset;
    FFieldsEditorForm: TDSFieldsEditorFrm;
    fWindowClosed: Boolean;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure EditorWindowClose;
    property LinkDataset: TDataset read FDataSet write FDataSet;
  end;

implementation

resourcestring
  rsTitle = 'Edit fields';

{ TDSFieldsEditorFrm }

procedure TDSFieldsEditorFrm.AddFieldsActnExecute(Sender: TObject);
var FieldsList: TFieldsListFrm;
begin
  FieldsList :=  TFieldsListFrm.Create(Self, LinkDataset, Designer);
  try
    FieldsList.ShowModal;
  finally
    FieldsList.Free;
  end;
  SelectionChanged;
end;

constructor TDSFieldsEditorFrm.Create(AOwner: TComponent; ADataset: TDataset;
    ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AOwner);

  LinkDataset := ADataset;
  FDesigner := ADesigner;
  Caption := rsTitle + ' - ' + LinkDataset.Name;
  FieldsListBox.Clear;
  RefreshFieldsListBox(False);

  GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerGetSelection(@OnGetSelection);
  GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);

  SelectionChanged;
end;

procedure TDSFieldsEditorFrm.DeleteFieldsActnExecute(Sender: TObject);
var i: integer;
    sActive: boolean;
    bModified: boolean;
    fld: TField;
begin
  sActive := LinkDataSet.Active;
  LinkDataSet.Active := False;
  bModified := False;
  for i := FieldsListBox.Items.Count - 1 downto 0 do
    if FieldsListBox.Selected[i] then begin
      fld := TField(FieldsListBox.Items.Objects[i]);
      FieldsListBox.Items.Delete(i);
      FDesigner.PropertyEditorHook.PersistentDeleting(fld);
      fld.Free;
      bModified := True;
    end;
  if bModified then fDesigner.Modified;
  if LinkDataset.Fields.Count > 0 then LinkDataSet.Active := sActive;
  SelectionChanged;
end;

procedure TDSFieldsEditorFrm.FieldsEditorFrmClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDSFieldsEditorFrm.FieldsEditorFrmDestroy(Sender: TObject);
begin
  if Assigned(FComponentEditor) then begin
    if Assigned(LinkDataset) And (Not (csDestroying in LinkDataset.ComponentState)) And (FieldsListBox.SelCount > 0) then
         GlobalDesignHook.SelectOnlyThis(LinkDataset);
    FComponentEditor.EditorWindowClose;
  end;
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
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
begin
  if LinkDataset.Active And LinkDataset.DefaultFields then LinkDataset.Close;
  //Deselect & refresh all existing
  DoSelected(False);
  //Add new fields
  for i := 0 to LinkDataset.Fields.Count - 1 do begin
    fld := LinkDataset.Fields[i];
    if FieldsListBox.Items.IndexOfObject(fld) < 0 then begin
      j := FieldsListBox.Items.AddObject(fld.FieldName, fld);
      FieldsListBox.Selected[j] := SelectAllNew;
    end;
  end;
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
  SelectionChanged;
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
  SelectionChanged;
  if bModified then fDesigner.Modified;
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

procedure TDSFieldsEditorFrm.SelectionChanged;
var SelList: TPersistentSelectionList;
begin
  GlobalDesignHook.RemoveHandlerSetSelection(@OnSetSelection);
  try
    SelList := TPersistentSelectionList.Create;
    try
      OnGetSelection(SelList);
      FDesigner.PropertyEditorHook.SetSelection(SelList) ;
    finally
      SelList.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
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
  if AComponent is TDataset And (AComponent = LinkDataset) then
    Caption := rsTitle + ' - ' + LinkDataset.Name;
end;

procedure TDSFieldsEditorFrm.OnPersistentDeleting(APersistent: TPersistent);
var i: integer;
begin
  if APersistent = LinkDataset then begin
//    removing all fields here ?
  end else begin
    i := FieldsListBox.Items.IndexOfObject(APersistent as TObject);
    if i >= 0 then FieldsListBox.Items.Delete( i );
  end;
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
    DoSelected(False);
    //select from list
    for i := 0 to ASelection.Count - 1 do
      if ASelection.Items[i] is TField then begin
        j := FieldsListBox.Items.IndexOfObject(ASelection.Items[i]);
        if j >= 0 then FieldsListBox.Selected[j] := True;
      end;
  end;
end;

procedure TDSFieldsEditorFrm.OnPersistentAdded(APersistent: TPersistent;
  Select: boolean);
var i: integer;
begin
  if Assigned(APersistent) And
     (APersistent is TField) And
     ((APersistent as TField).DataSet = LinkDataset) then begin
    i := FieldsListBox.Items.AddObject( TField(APersistent).FieldName, APersistent );
    FieldsListBox.Selected[i] := Select;
    TField(APersistent).Index := i;
  end;
end;

{ TFieldsComponentEditor }

constructor TFieldsComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  fWindowClosed := True;
end;

destructor TFieldsComponentEditor.Destroy;
begin
  if not fWindowClosed
    then FreeThenNil(FFieldsEditorForm);
  inherited Destroy;
end;

function TFieldsComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TFieldsComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := rsTitle;
  end;
end;

procedure TFieldsComponentEditor.ExecuteVerb(Index: Integer);
var ADataset: TDataset;
begin
  case index of
    0: begin
      ADataset := GetComponent as TDataset;
      if ADataset = nil
      then raise Exception.Create('TFieldsComponentEditor.Edit LinkDataset=nil');
      if fWindowClosed then begin
        FFieldsEditorForm := TDSFieldsEditorFrm.Create(Application, ADataset, Designer);
        fWindowClosed := False;
      end;
      with FFieldsEditorForm do begin
        ComponentEditor := Self;
        ShowOnTop;
      end;
    end;
  end;
end;

procedure TFieldsComponentEditor.EditorWindowClose;
begin
  fWindowClosed := True;
end;


initialization
  {$I fieldseditor.lrs}
  RegisterComponentEditor(TDataset, TFieldsComponentEditor);

{$ELSE The FCL of FPC 2.0 does not support this}
implementation
{$ENDIF}
end.
