{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

Author: Alexander Klenin

}
unit TASubcomponentsEditor;

{$H+}

interface

uses
  Classes, ComCtrls, ComponentEditors, Forms, Menus, PropEdits, StdCtrls;

type

  { TSubComponentListEditor }

  TSubComponentListEditor = class(TComponentEditor)
  protected
    function MakeEditorForm: TForm; virtual; abstract;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
  end;

  { TComponentListPropertyEditor }

  TComponentListPropertyEditor = class(TPropertyEditor)
  protected
    function GetChildrenCount: Integer; virtual; abstract;
    function MakeEditorForm: TForm; virtual; abstract;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

  { TComponentListEditorForm }

  TComponentListEditorForm = class(TForm)
    ChildrenListBox: TListBox;
    menuAddItem: TPopupMenu;
    tbAdd: TToolButton;
    tbCommands: TToolBar;
    tbDelete: TToolButton;
    tbMoveDown: TToolButton;
    tbMoveUp: TToolButton;
    procedure ChildrenListBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbMoveDownClick(Sender: TObject);
    procedure tbMoveUpClick(Sender: TObject);
  private
    FComponentEditor: TSubComponentListEditor;
    FDesigner: TComponentEditorDesigner;
    FParent: TComponent;
    FPropertyEditor: TComponentListPropertyEditor;
    function FindChild(ACandidate: TPersistent; out AIndex: Integer): Boolean;
    procedure MoveSelection(AStart, ADir: Integer);
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; ASelect: Boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure RefreshList;
    procedure SelectionChanged(AOrderChanged: Boolean = false);
  protected
    procedure AddSubcomponent(AParent, AChild: TComponent); virtual; abstract;
    procedure AddSubcomponentClass(const ACaption: String; ATag: Integer);
    procedure BuildCaption; virtual; abstract;
    function ChildClass: TComponentClass; virtual; abstract;
    procedure EnumerateSubcomponentClasses; virtual; abstract;
    function GetChildrenList: TFPList; virtual; abstract;
    function MakeSubcomponent(
      AOwner: TComponent; ATag: Integer): TComponent; virtual; abstract;
    property Parent: TComponent read FParent;
  public
    constructor Create(
      AOwner, AParent: TComponent; AComponentEditor: TSubComponentListEditor;
      APropertyEditor: TComponentListPropertyEditor); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  IDEImagesIntf, Math, SysUtils, TAChartUtils;

{$R *.lfm}

{ TComponentListPropertyEditor }

procedure TComponentListPropertyEditor.Edit;
var
  propValue: TPersistent;
  editorForm: TForm;
begin
  propValue := GetComponent(0);
  if propValue = nil then
    raise Exception.Create('TComponentListPropertyEditor.Component=nil');
  editorForm := FindEditorForm(propValue) as TForm;
  if editorForm = nil then begin
    editorForm := MakeEditorForm;
    RegisterEditorForm(editorForm, propValue);
  end;
  editorForm.EnsureVisible;
end;

function TComponentListPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TComponentListPropertyEditor.GetValue: ansistring;
var
  c: Integer;
begin
  c := GetChildrenCount;
  if c = 1 then
    Result := '1 item'
  else
    Result := IntToStr(c) + ' items';
end;

{ TSubComponentListEditor }

procedure TSubComponentListEditor.ExecuteVerb(Index: Integer);
var
  propValue: TPersistent;
  editorForm: TForm;
begin
  if Index <> 0 then exit;
  propValue := GetComponent;
  if propValue = nil then
    raise Exception.Create('TSubComponentListEditor.Component=nil');
  editorForm := FindEditorForm(propValue) as TForm;
  if editorForm = nil then begin
    editorForm := MakeEditorForm;
    RegisterEditorForm(editorForm, propValue);
  end;
  editorForm.ShowOnTop;
end;

function TSubComponentListEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TComponentListEditorForm }

procedure TComponentListEditorForm.AddSubcomponentClass(
  const ACaption: String; ATag: Integer);
var
  mi: TMenuItem;
begin
  if ACaption = '' then exit; // Empty names denote deprecated components.
  mi := TMenuItem.Create(Self);
  mi.OnClick := @miAddClick;
  mi.Caption := ACaption;
  mi.Tag := ATag;
  menuAddItem.Items.Add(mi);
end;

procedure TComponentListEditorForm.ChildrenListBoxClick(Sender: TObject);
begin
  SelectionChanged;
end;

constructor TComponentListEditorForm.Create(
  AOwner, AParent: TComponent; AComponentEditor: TSubComponentListEditor;
  APropertyEditor: TComponentListPropertyEditor);
begin
  inherited Create(AOwner);
  FParent := AParent;
  FComponentEditor := AComponentEditor;
  FPropertyEditor := APropertyEditor;
  if FComponentEditor <> nil then
    FDesigner := FComponentEditor.Designer
  else
    FDesigner := FindRootDesigner(FParent) as TComponentEditorDesigner;
  BuildCaption;
  EnumerateSubcomponentClasses;

  RefreshList;

  GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerGetSelection(@OnGetSelection);
  GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);

  SelectionChanged;
end;

destructor TComponentListEditorForm.Destroy;
begin
  UnregisterEditorForm(Self);
  inherited Destroy;
end;

function TComponentListEditorForm.FindChild(
  ACandidate: TPersistent; out AIndex: Integer): Boolean;
begin
  if ACandidate is ChildClass then
    AIndex := ChildrenListBox.Items.IndexOfObject(ACandidate)
  else
    AIndex := -1;
  Result := AIndex >= 0;
end;

procedure TComponentListEditorForm.FormClose(
  Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TComponentListEditorForm.FormCreate(Sender: TObject);
begin
  tbCommands.Images := IDEImages.Images_16;
  tbAdd.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  tbDelete.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  tbMoveDown.ImageIndex := IDEImages.LoadImage(16, 'arrow_down');
  tbMoveUp.ImageIndex := IDEImages.LoadImage(16, 'arrow_up');
end;

procedure TComponentListEditorForm.FormDestroy(Sender: TObject);
begin
  if
    (FComponentEditor <> nil) and (FParent <> nil) and
    (not (csDestroying in FParent.ComponentState)) and
    (ChildrenListBox.SelCount > 0)
  then
    GlobalDesignHook.SelectOnlyThis(FParent);
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TComponentListEditorForm.miAddClick(Sender: TObject);
var
  s: TComponent;
  n: String;
begin
  s := MakeSubcomponent(FParent.Owner, (Sender as TMenuItem).Tag);
  try
    n := Copy(s.ClassName, 2, Length(s.ClassName) - 1);
    s.Name := FDesigner.CreateUniqueComponentName(FParent.Name + n);
    AddSubcomponent(FParent, s);
    FDesigner.PropertyEditorHook.PersistentAdded(s, true);
    FDesigner.Modified;
    RefreshList;
  except
    s.Free;
    raise;
  end;
end;

procedure TComponentListEditorForm.MoveSelection(AStart, ADir: Integer);
var
  i: Integer;
begin
  if not ChildrenListBox.SelCount = 0 then exit;
  i := AStart - ADir;
  with ChildrenListBox do
    while InRange(i, 0, Count - 1) and InRange(i + ADir, 0, Count - 1) do begin
      if Selected[i] and not Selected[i + ADir] then begin
        with TIndexedComponent(Items.Objects[i]) do
          Index := Index + ADir;
        Items.Move(i, i + ADir);
        Selected[i + ADir] := true;
        Selected[i] := false;
      end;
      i -= ADir;
    end;
  FDesigner.Modified;
  SelectionChanged(true);
end;

procedure TComponentListEditorForm.OnComponentRenamed(AComponent: TComponent);
var
  i: Integer;
begin
  if AComponent = nil then exit;
  if FindChild(AComponent, i) then
    ChildrenListBox.Items[i] := AComponent.Name
  else if AComponent = FParent then
    BuildCaption;
end;

procedure TComponentListEditorForm.OnGetSelection(
  const ASelection: TPersistentSelectionList);
var
  i: Integer;
begin
  if ASelection = nil then exit;
  ASelection.Clear;
  with ChildrenListBox do
    for i := 0 to Items.Count - 1 do
      if Selected[i] then
        ASelection.Add(TPersistent(Items.Objects[i]));
end;

procedure TComponentListEditorForm.OnPersistentAdded(
  APersistent: TPersistent; ASelect: Boolean);
var
  s: TComponent;
begin
  if (APersistent = nil) or not (APersistent is ChildClass) then exit;
  s := APersistent as TComponent;
  if s.GetParentComponent <> FParent then exit;
  with ChildrenListBox do
    Selected[Items.AddObject(s.Name, s)] := ASelect;
end;

procedure TComponentListEditorForm.OnPersistentDeleting(
  APersistent: TPersistent);
var
  i: Integer;
begin
  if FindChild(APersistent, i) then
    ChildrenListBox.Items.Delete(i);
end;

procedure TComponentListEditorForm.OnSetSelection(
  const ASelection: TPersistentSelectionList);
var
  i, j: Integer;
begin
  if ASelection = nil then exit;
  ChildrenListBox.ClearSelection;
  for i := 0 to ASelection.Count - 1 do
    if FindChild(ASelection.Items[i], j) then
      ChildrenListBox.Selected[j] := true;
end;

procedure TComponentListEditorForm.RefreshList;
var
  ci: TStrings;
  i: Integer;
begin
  ci := ChildrenListBox.Items;
  try
    ci.BeginUpdate;
    ci.Clear;
    with GetChildrenList do
      for i := 0 to Count - 1 do
        ci.AddObject(TComponent(Items[i]).Name, TObject(Items[i]));
  finally
    ci.EndUpdate;
  end;
end;

procedure TComponentListEditorForm.SelectionChanged(AOrderChanged: Boolean);
var
  sel: TPersistentSelectionList;
begin
  GlobalDesignHook.RemoveHandlerSetSelection(@OnSetSelection);
  try
    sel := TPersistentSelectionList.Create;
    sel.ForceUpdate := AOrderChanged;
    try
      OnGetSelection(sel);
      FDesigner.PropertyEditorHook.SetSelection(sel);
    finally
      sel.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
end;

procedure TComponentListEditorForm.tbDeleteClick(Sender: TObject);
var
  i: Integer;
  s: TComponent;
begin
  if ChildrenListBox.SelCount = 0 then exit;
  for i := ChildrenListBox.Items.Count - 1 downto 0 do
    if ChildrenListBox.Selected[i] then begin
      s := TComponent(ChildrenListBox.Items.Objects[i]);
      ChildrenListBox.Items.Delete(i);
      FDesigner.PropertyEditorHook.PersistentDeleting(s);
      s.Free;
    end;
  FDesigner.Modified;
  SelectionChanged;
end;

procedure TComponentListEditorForm.tbMoveDownClick(Sender: TObject);
begin
  MoveSelection(ChildrenListBox.Count - 1, 1);
end;

procedure TComponentListEditorForm.tbMoveUpClick(Sender: TObject);
begin
  MoveSelection(0, -1);
end;

end.

