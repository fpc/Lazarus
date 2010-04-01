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
  Classes, ComponentEditors, Forms, Menus, PropEdits, StdCtrls;

type

  { TSubComponentListEditor }

  TSubComponentListEditor = class(TComponentEditor)
  private
    FEditorForm: TForm;
  protected
    function MakeEditorForm: TForm; virtual; abstract;
  public
    destructor Destroy; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerbCount: Integer; override;
  end;

  { TComponentListPropertyEditor }

  TComponentListPropertyEditor = class(TPropertyEditor)
  private
    FEditorForm: TForm;
  protected
    function GetChildrenCount: Integer; virtual; abstract;
    function MakeEditorForm: TForm; virtual; abstract;
  public
    destructor Destroy; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
  end;

  { TComponentListEditorForm }

  TComponentListEditorForm = class(TForm)
    ChildrenListBox: TListBox;
    MainMenu1: TMainMenu;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    procedure ChildrenListBoxClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
  private
    FComponentEditor: TSubComponentListEditor;
    FDesigner: TComponentEditorDesigner;
    FParent: TComponent;
    FPropertyEditor: TComponentListPropertyEditor;
    function FindChild(ACandidate: TPersistent; out AIndex: Integer): Boolean;
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnGetSelection(const ASelection: TPersistentSelectionList);
    procedure OnPersistentAdded(APersistent: TPersistent; ASelect: Boolean);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnSetSelection(const ASelection: TPersistentSelectionList);
    procedure SelectionChanged;
  protected
    procedure AddSubcomponent(AParent, AChild: TComponent); virtual; abstract;
    procedure AddSubcomponentClass(const ACaption: String; ATag: Integer);
    procedure BuildCaption; virtual; abstract;
    function ChildClass: TComponentClass; virtual; abstract;
    procedure EnumerateSubcomponentClasses; virtual; abstract;
    function MakeSubcomponent(
      AOwner: TComponent; ATag: Integer): TComponent; virtual; abstract;
    procedure RefreshList; virtual; abstract;
    property Parent: TComponent read FParent;
  public
    constructor Create(
      AOwner, AParent: TComponent; AComponentEditor: TSubComponentListEditor;
      APropertyEditor: TComponentListPropertyEditor); reintroduce;
  end;

implementation

uses
  SysUtils;

{$R *.lfm}

{ TComponentListPropertyEditor }

destructor TComponentListPropertyEditor.Destroy;
begin
  FreeAndNil(FEditorForm);
  inherited;
end;

procedure TComponentListPropertyEditor.Edit;
begin
  if GetComponent(0) = nil then
    raise Exception.Create('TComponentListPropertyEditor.Component=nil');
  if FEditorForm = nil then
    FEditorForm := MakeEditorForm;
  FEditorForm.EnsureVisible;
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

destructor TSubComponentListEditor.Destroy;
begin
  FreeAndNil(FEditorForm);
  inherited;
end;

procedure TSubComponentListEditor.ExecuteVerb(Index: Integer);
begin
  if Index <> 0 then exit;
  if GetComponent = nil then
    raise Exception.Create('TSubComponentListEditor.Component=nil');
  if FEditorForm = nil then
    FEditorForm := MakeEditorForm;
  FEditorForm.ShowOnTop;
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
  mi := TMenuItem.Create(Self);
  mi.OnClick := @miAddClick;
  mi.Caption := ACaption;
  mi.Tag := ATag;
  miAdd.Add(mi);
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

procedure TComponentListEditorForm.FormDestroy(Sender: TObject);
begin
  if FComponentEditor <> nil then begin
    FComponentEditor.FEditorForm := nil;
    if
      (FParent <> nil) and (not (csDestroying in FParent.ComponentState)) and
      (ChildrenListBox.SelCount > 0)
    then
      GlobalDesignHook.SelectOnlyThis(FParent);
  end;
  if FPropertyEditor <> nil then
    FPropertyEditor.FEditorForm := nil;
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

procedure TComponentListEditorForm.miDeleteClick(Sender: TObject);
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

procedure TComponentListEditorForm.SelectionChanged;
var
  sel: TPersistentSelectionList;
begin
  GlobalDesignHook.RemoveHandlerSetSelection(@OnSetSelection);
  try
    sel := TPersistentSelectionList.Create;
    try
      OnGetSelection(sel);
      FDesigner.PropertyEditorHook.SetSelection(sel) ;
    finally
      sel.Free;
    end;
  finally
    GlobalDesignHook.AddHandlerSetSelection(@OnSetSelection);
  end;
end;

end.

