{
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

  Author: Mattias Gaertner

  Abstract:
    This units defines the component editors used by the designer.
    A Component Editor is a plugin used by the designer to add special
    functions for component classes.
    For more information see the big comment part below.
}
unit ComponentEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, TypInfo, LCLProc, Forms, Controls, Menus,
  ExtCtrls, StdCtrls, Graphics, Grids, CheckLst, Buttons, ComCtrls, Dialogs,
  LazStringGridEdit, CheckListboxEditorDlg, GraphType, PropEdits, ObjInspStrConsts;

type
  { TComponentEditorDesigner }
  
  TComponentPasteSelectionFlag = (
    cpsfReplace,
    cpsfFindUniquePositions
    );
  TComponentPasteSelectionFlags = set of TComponentPasteSelectionFlag;


  TComponentEditorDesigner = class(TIDesigner)
  protected
    FForm: TCustomForm;
    function GetPropertyEditorHook: TPropertyEditorHook; virtual; abstract;
  public
    function CopySelection: boolean; virtual; abstract;
    function CutSelection: boolean; virtual; abstract;
    function CanPaste: boolean; virtual; abstract;
    function PasteSelection(Flags: TComponentPasteSelectionFlags): boolean; virtual; abstract;
    function DeleteSelection: boolean; virtual; abstract;
    function CopySelectionToStream(s: TStream): boolean; virtual; abstract;
    function InsertFromStream(s: TStream; Parent: TWinControl;
                              Flags: TComponentPasteSelectionFlags
                              ): Boolean; virtual; abstract;
    function InvokeComponentEditor(AComponent: TComponent;
                                   MenuIndex: integer): boolean; virtual; abstract;

    procedure DrawDesignerItems(OnlyIfNeeded: boolean); virtual; abstract;
    function CreateUniqueComponentName(const AClassName: string
                                       ): string; virtual; abstract;
    property PropertyEditorHook: TPropertyEditorHook read GetPropertyEditorHook;
    property Form: TCustomForm read FForm;
  end;


{ Component Editor Types }

type

{ TComponentEditor
  A component editor is created for each component that is selected in the
  form designer based on the component's type (see GetComponentEditor and
  RegisterComponentEditor). When the component is double-clicked the Edit
  method is called. When the context menu for the component is invoked the
  GetVerbCount and GetVerb methods are called to build the menu. If one
  of the verbs are selected, ExecuteVerb is called. Paste is called whenever
  the component is pasted to the clipboard. You only need to create a
  component editor if you wish to add verbs to the context menu, change
  the default double-click behavior, or paste an additional clipboard format.
  The default component editor (TDefaultEditor) implements Edit to searches the
  properties of the component and generates (or navigates to) the OnCreate,
  OnChanged, or OnClick event (whichever it finds first). Whenever the
  component editor modifies the component, it *must* call Designer.Modified to
  inform the designer that the form has been modified. (Or else the user can not
  save the changes).

    Edit
      Called when the user double-clicks the component. The component editor can
      bring up a dialog in response to this method, for example, or some kind
      of design expert. If GetVerbCount is greater than zero, edit will execute
      the first verb in the list (ExecuteVerb(0)).

    ExecuteVerb(Index)
      The Index'ed verb was selected by the use off the context menu. The
      meaning of this is determined by component editor.

    GetVerb
      The component editor should return a string that will be displayed in the
      context menu. It is the responsibility of the component editor to place
      the & character and the '...' characters as appropriate.

    GetVerbCount
      The number of valid indices to GetVerb and Execute verb. The index is
      assumed to be zero based (i.e. 0..GetVerbCount - 1).

    PrepareItem
      While constructing the context menu PrepareItem will be called for
      each verb. It will be passed the menu item that will be used to represent
      the verb. The component editor can customize the menu item as it sees fit,
      including adding subitems. If you don't want that particular menu item
      to be shown, don't free it, simply set its Visible property to False.

    Copy
      Called when the component is being copied to the clipboard. The
      component's filed image is already on the clipboard. This gives the
      component editor a chance to paste a different type of format which is
      ignored by the designer but might be recognized by another application.

    IsInInlined
      Determines whether Component is in the Designer which owns it.
      Essentially, Components should not be able to be added to a Frame
      instance (collections are fine though) so this function checks to
      determine whether the currently selected component is within a Frame
      instance or not.

    GetComponent
      Returns the edited component.

    GetDesigner
      Returns the current Designer for the form owning the component.
    }

{ TComponentEditor
  All component editors are assumed derived from TBaseComponentEditor.

    Create(AComponent, ADesigner)
      Called to create the component editor. AComponent is the component to
      be edited by the editor. ADesigner is an interface to the designer to
      find controls and create methods (this is not used often). If a component
      editor modifies the component in any way it *must* call
      ADesigner.Modified. }

  TBaseComponentEditor = class
  protected
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner); virtual;
    procedure Edit; virtual; abstract;
    procedure ExecuteVerb(Index: Integer); virtual; abstract;
    function GetVerb(Index: Integer): string; virtual; abstract;
    function GetVerbCount: Integer; virtual; abstract;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); virtual; abstract;
    procedure Copy; virtual; abstract;
    function IsInInlined: Boolean; virtual; abstract;
    function GetComponent: TComponent; virtual; abstract;
    function GetDesigner: TComponentEditorDesigner; virtual; abstract;
    function GetHook(out Hook: TPropertyEditorHook): boolean; virtual; abstract;
    procedure Modified; virtual; abstract;
  end;

  TComponentEditorClass = class of TBaseComponentEditor;


{ TComponentEditor
  This class provides a default implementation for the IComponentEditor
  interface. There is no assumption by the designer that you use this class
  only that your class derive from TBaseComponentEditor and implement
  IComponentEditor. This class is provided to help you implement a class
  that meets those requirements. }
  TComponentEditor = class(TBaseComponentEditor)
  private
    FComponent: TComponent;
    FDesigner: TComponentEditorDesigner;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetComponent: TComponent; override;
    function GetDesigner: TComponentEditorDesigner; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function IsInInlined: Boolean; override;
    procedure Copy; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    property Component: TComponent read FComponent;
    property Designer: TComponentEditorDesigner read GetDesigner;
    function GetHook(out Hook: TPropertyEditorHook): boolean; override;
    function HasHook: boolean;
    procedure Modified; override;
  end;


{ TDefaultComponentEditor
  An editor that provides default behavior for the double-click that will
  iterate through the properties looking for the most appropriate method
  property to edit }
  TDefaultComponentEditor = class(TComponentEditor)
  private
    FBestEditEvent: string;
    FFirst: TPropertyEditor;
    FBest: TPropertyEditor;
    FContinue: Boolean;
    FPropEditCandidates: TList; // list of TPropertyEditor
    procedure CheckEdit(Prop: TPropertyEditor);
  protected
    procedure EditProperty(const Prop: TPropertyEditor;
      var Continue: Boolean); virtual;
    procedure ClearPropEditorCandidates;
  public
    constructor Create(AComponent: TComponent;
      ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    property BestEditEvent: string read FBestEditEvent write FBestEditEvent;
  end;
  
  
{ TNotebookComponentEditor
  The default component editor for TCustomNotebook. }
  TNotebookComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure AddNewPageToDesigner(Index: integer); virtual;
    procedure DoAddPage; virtual;
    procedure DoInsertPage; virtual;
    procedure DoDeletePage; virtual;
    procedure DoMoveActivePageLeft; virtual;
    procedure DoMoveActivePageRight; virtual;
    procedure DoMovePage(CurIndex, NewIndex: Integer); virtual;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function Notebook: TCustomNotebook; virtual;
  end;
  
  
{ TPageComponentEditor
  The default component editor for TCustomPage. }
  TPageComponentEditor = class(TNotebookComponentEditor)
  protected
  public
    function Notebook: TCustomNotebook; override;
    function Page: TCustomPage; virtual;
  end;


{ TTabControlComponentEditor
  The default component editor for TCustomTabControl. }
  TTabControlComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure DoAddTab; virtual;
    procedure DoInsertTab; virtual;
    procedure DoDeleteTab; virtual;
    procedure DoMoveActiveTabLeft; virtual;
    procedure DoMoveActiveTabRight; virtual;
    procedure DoMoveTab(CurIndex, NewIndex: Integer); virtual;
    procedure AddMenuItemsForTabs(ParentMenuItem: TMenuItem); virtual;
    procedure ShowTabMenuItemClick(Sender: TObject);
    function CreateNewTabCaption: string;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function TabControl: TCustomTabControl; virtual;
  end;


{ TStringGridComponentEditor
  The default componenteditor for TStringGrid }

  TStringGridComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TCheckListBoxComponentEditor
  The default componenteditor for TCheckListBox }

  TCheckListBoxComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


{ TCheckGroupComponentEditor
  The default componenteditor for TCheckGroup }

  TCheckGroupComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure DoShowEditor;
    procedure AssignCheck(dstCheck, srcCheck: TCheckGroup);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;


{ TToolBarComponentEditor
  The default componenteditor for TToolBar }

  TToolBarComponentEditor = class(TDefaultComponentEditor)
  protected
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function ToolBar: TToolBar; virtual;
  end;


{ TFileDialogComponentEditor
  The default componenteditor for TFileDialog }

  TCommonDialogComponentEditor = class(TComponentEditor)
  private
    procedure TestDialog;
  public
    function GetVerbCount:integer;override;
    function GetVerb(Index:integer):string;override;
    procedure ExecuteVerb(Index:integer);override;
  end;
  

{ Register a component editor }
type
  TRegisterComponentEditorProc =
    procedure (ComponentClass: TComponentClass;
               ComponentEditor: TComponentEditorClass);

var
  RegisterComponentEditorProc: TRegisterComponentEditorProc;


procedure RegisterComponentEditor(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);
function GetComponentEditor(Component: TComponent;
  const Designer: TComponentEditorDesigner): TBaseComponentEditor;

type
  TPropertyEditorFilterFunc =
    function(const ATestEditor: TPropertyEditor): Boolean of object;


implementation

{ RegisterComponentEditor }
type
  PComponentClassRec = ^TComponentClassRec;
  TComponentClassRec = record
    Group: Integer;
    ComponentClass: TComponentClass;
    EditorClass: TComponentEditorClass;
  end;

const
  ComponentClassList: TList = nil;

procedure DefaultRegisterComponentEditorProc(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);
var
  P: PComponentClassRec;
begin
  if ComponentClassList = nil then
    ComponentClassList := TList.Create;
  New(P);
  P^.Group := -1;//CurrentGroup;
  P^.ComponentClass := ComponentClass;
  P^.EditorClass := ComponentEditor;
  ComponentClassList.Insert(0, P);
end;

procedure RegisterComponentEditor(ComponentClass: TComponentClass;
  ComponentEditor: TComponentEditorClass);
begin
  if Assigned(RegisterComponentEditorProc) then
    RegisterComponentEditorProc(ComponentClass, ComponentEditor);
end;

function GetComponentEditor(Component: TComponent;
  const Designer: TComponentEditorDesigner): TBaseComponentEditor;
var
  P: PComponentClassRec;
  I: Integer;
  ComponentClass: TComponentClass;
  EditorClass: TComponentEditorClass;
begin
  ComponentClass := TComponentClass(TPersistent);
  EditorClass := TDefaultComponentEditor;
  if ComponentClassList <> nil then
    for I := 0 to ComponentClassList.Count-1 do
    begin
      P := PComponentClassRec(ComponentClassList[I]);
      //DebugLn('GetComponentEditor Component=',dbgsName(Component),' ',dbgsName(P^.ComponentClass),' ',dbgsName(P^.EditorClass));
      if (Component is P^.ComponentClass) and
        (P^.ComponentClass <> ComponentClass) and
        (P^.ComponentClass.InheritsFrom(ComponentClass)) then
      begin
        EditorClass := P^.EditorClass;
        ComponentClass := P^.ComponentClass;
      end;
    end;
  Result := EditorClass.Create(Component, Designer);
end;

{ Component Editors -----------------------------------------------------------}


{ TBaseComponentEditor }

constructor TBaseComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create;
end;

{ TComponentEditor }

constructor TComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FComponent := AComponent;
  FDesigner := ADesigner;
end;

procedure TComponentEditor.Edit;
begin
  if GetVerbCount > 0 then ExecuteVerb(0);
end;

function TComponentEditor.GetComponent: TComponent;
begin
  Result := FComponent;
end;

function TComponentEditor.GetDesigner: TComponentEditorDesigner;
begin
  Result := FDesigner;
end;

function TComponentEditor.GetVerbCount: Integer;
begin
  // Intended for descendents to implement
  Result := 0;
end;

function TComponentEditor.GetVerb(Index: Integer): string;
begin
  // Intended for descendents to implement
  Result:=ClassName+IntToStr(Index);
end;

procedure TComponentEditor.ExecuteVerb(Index: Integer);
begin
  // Intended for descendents to implement
  DebugLn(Classname+'.ExecuteVerb: ',IntToStr(Index));
end;

procedure TComponentEditor.Copy;
begin
  // Intended for descendents to implement
end;

function TComponentEditor.IsInInlined: Boolean;
begin
  Result := csInline in Component.Owner.ComponentState;
end;

procedure TComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  // Intended for descendents to implement
end;

function TComponentEditor.GetHook(out Hook: TPropertyEditorHook): boolean;
begin
  Result:=false;
  Hook:=nil;
  if GetDesigner=nil then exit;
  Hook:=GetDesigner.PropertyEditorHook;
  Result:=Hook<>nil;
end;

function TComponentEditor.HasHook: boolean;
var
  Hook: TPropertyEditorHook;
begin
  Result:=GetHook(Hook) and (Hook<>nil);
end;

procedure TComponentEditor.Modified;
begin
  GetDesigner.Modified;
end;

{ TDefaultComponentEditor }

procedure TDefaultComponentEditor.CheckEdit(Prop: TPropertyEditor);
begin
  if FContinue then
    EditProperty(Prop, FContinue);
  if FPropEditCandidates=nil then
    FPropEditCandidates:=TList.Create;
  FPropEditCandidates.Add(Prop);
end;

procedure TDefaultComponentEditor.EditProperty(const Prop: TPropertyEditor;
  var Continue: Boolean);
var
  PropName: string;
  BestName: string;

  procedure ReplaceBest;
  begin
    FBest := Prop;
    if FFirst = FBest then FFirst := nil;
  end;

begin
  if not Assigned(FFirst) and (Prop is TMethodPropertyEditor) then
    FFirst := Prop;
  PropName := Prop.GetName;
  BestName := '';
  if Assigned(FBest) then BestName := FBest.GetName;
  if CompareText(PropName, FBestEditEvent) = 0 then
    ReplaceBest
  else if CompareText(BestName, FBestEditEvent) <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
      ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;

procedure TDefaultComponentEditor.ClearPropEditorCandidates;
var
  i: Integer;
begin
  if FPropEditCandidates=nil then exit;
  for i:=0 to FPropEditCandidates.Count-1 do
    TObject(FPropEditCandidates[i]).Free;
  FPropEditCandidates.Free;
  FPropEditCandidates:=nil;
end;

constructor TDefaultComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FBestEditEvent:='OnCreate';
end;

destructor TDefaultComponentEditor.Destroy;
begin
  ClearPropEditorCandidates;
  inherited Destroy;
end;

procedure TDefaultComponentEditor.Edit;
var
  PropertyEditorHook: TPropertyEditorHook;
  NewLookupRoot: TPersistent;
begin
  PropertyEditorHook:=nil;
  if not GetHook(PropertyEditorHook) then exit;
  NewLookupRoot:=GetLookupRootForComponent(Component);
  if not (NewLookupRoot is TComponent) then exit;
  if NewLookupRoot<>PropertyEditorHook.LookupRoot then
    GetDesigner.SelectOnlyThisComponent(Component);
  FContinue := True;
  FFirst := nil;
  FBest := nil;
  try
    GetPersistentProperties(Component,tkAny,PropertyEditorHook,@CheckEdit,nil);
    if FContinue
    then begin
      if Assigned(FBest) then
        FBest.Edit
      else if Assigned(FFirst) then
        FFirst.Edit;
    end;
  finally
    FFirst := nil;
    FBest := nil;
    ClearPropEditorCandidates;
  end;
end;

function TDefaultComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TDefaultComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:=oisCreateDefaultEvent;
end;

procedure TDefaultComponentEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;


{ TNotebookComponentEditor }

const
  nbvAddPage       = 0;
  nbvInsertPage    = 1;
  nbvDeletePage    = 2;
  nbvMovePageLeft  = 3;
  nbvMovePageRight = 4;
  nbvShowPage      = 5;

procedure TNotebookComponentEditor.ShowPageMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewPageIndex: integer;
begin
  AMenuItem:=TMenuItem(Sender);
  if (AMenuItem=nil) or (not (AMenuItem is TMenuItem)) then exit;
  NewPageIndex:=AMenuItem.MenuIndex;
  if (NewPageIndex<0) or (NewPageIndex>=Notebook.PageCount) then exit;
  NoteBook.PageIndex:=NewPageIndex;
  GetDesigner.SelectOnlyThisComponent(NoteBook.CustomPage(NoteBook.PageIndex));
end;

procedure TNotebookComponentEditor.AddNewPageToDesigner(Index: integer);
var
  Hook: TPropertyEditorHook;
  NewPage: TCustomPage;
  NewName: string;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;
  NewPage:=NoteBook.CustomPage(Index);
  NewName:=GetDesigner.CreateUniqueComponentName(NewPage.ClassName);
  NewPage.Caption:=NewName;
  NewPage.Name:=NewName;
  NoteBook.PageIndex:=Index;
  Hook.PersistentAdded(NewPage,true);
  Modified;
end;

procedure TNotebookComponentEditor.DoAddPage;
begin
  if not HasHook then exit;
  NoteBook.Pages.Add('');
  AddNewPageToDesigner(NoteBook.PageCount-1);
end;

procedure TNotebookComponentEditor.DoInsertPage;
var
  NewIndex: integer;
begin
  if not HasHook then exit;
  NewIndex:=Notebook.PageIndex;
  if NewIndex<0 then NewIndex:=0;
  Notebook.Pages.Insert(NewIndex,'');
  AddNewPageToDesigner(NewIndex);
end;

procedure TNotebookComponentEditor.DoDeletePage;
var
  Hook: TPropertyEditorHook;
  OldIndex: integer;
  PageComponent: TComponent;
begin
  OldIndex:=Notebook.PageIndex;
  if (OldIndex>=0) and (OldIndex<Notebook.PageCount) then begin
    if not GetHook(Hook) then exit;
    PageComponent:=TComponent(NoteBook.Pages.Objects[OldIndex]);
    Hook.DeletePersistent(PageComponent);
  end;
end;

procedure TNotebookComponentEditor.DoMoveActivePageLeft;
var
  Index: integer;
begin
  Index:=NoteBook.PageIndex;
  if (Index<0) then exit;
  DoMovePage(Index,Index-1);
end;

procedure TNotebookComponentEditor.DoMoveActivePageRight;
var
  Index: integer;
begin
  Index:=NoteBook.PageIndex;
  if (Index>=0)
  and (Index>=NoteBook.PageCount-1) then exit;
  DoMovePage(Index,Index+1);
end;

procedure TNotebookComponentEditor.DoMovePage(
  CurIndex, NewIndex: Integer);
begin
  NoteBook.Pages.Move(CurIndex,NewIndex);
  Modified;
end;

procedure TNotebookComponentEditor.AddMenuItemsForPages(
  ParentMenuItem: TMenuItem);
var
  i: integer;
  NewMenuItem: TMenuItem;
begin
  ParentMenuItem.Enabled:=NoteBook.PageCount>0;
  for i:=0 to NoteBook.PageCount-1 do begin
    NewMenuItem:=TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name:='ShowPage'+IntToStr(i);
    NewMenuItem.Caption:=Notebook.CustomPage(i).Name+' "'+Notebook.Pages[i]+'"';
    NewMenuItem.OnClick:=@ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

procedure TNotebookComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    nbvAddPage:       DoAddPage;
    nbvInsertPage:    DoInsertPage;
    nbvDeletePage:    DoDeletePage; // beware: this can free the editor itself
    nbvMovePageLeft:  DoMoveActivePageLeft;
    nbvMovePageRight: DoMoveActivePageRight;
  end;
end;

function TNotebookComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    nbvAddPage:       Result:=nbcesAddPage;
    nbvInsertPage:    Result:=nbcesInsertPage;
    nbvDeletePage:    Result:=nbcesDeletePage;
    nbvMovePageLeft:  Result:=nbcesMovePageLeft;
    nbvMovePageRight: Result:=nbcesMovePageRight;
    nbvShowPage:      Result:=nbcesShowPage;
  else
    Result:='';
  end;
end;

function TNotebookComponentEditor.GetVerbCount: Integer;
begin
  Result:=6;
end;

procedure TNotebookComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    nbvAddPage:       ;
    nbvInsertPage:    AnItem.Enabled:=Notebook.PageIndex>=0;
    nbvDeletePage:    AnItem.Enabled:=Notebook.PageIndex>=0;
    nbvMovePageLeft:  AnItem.Enabled:=Notebook.PageIndex>0;
    nbvMovePageRight: AnItem.Enabled:=Notebook.PageIndex<Notebook.PageCount-1;
    nbvShowPage:      AddMenuItemsForPages(AnItem);
  end;
end;

function TNotebookComponentEditor.Notebook: TCustomNotebook;
begin
  Result:=TCustomNotebook(GetComponent);
end;

{ TPageComponentEditor }

function TPageComponentEditor.Notebook: TCustomNotebook;
var
  APage: TCustomPage;
begin
  APage:=Page;
  if (APage.Parent<>nil) and (APage.Parent is TCustomNoteBook) then
    Result:=TCustomNoteBook(APage.Parent);
end;

function TPageComponentEditor.Page: TCustomPage;
begin
  Result:=TCustomPage(GetComponent);
end;


function EditStringGrid(AStringGrid: TStringGrid): Boolean;
var
  StringGridEditorDlg: TStringGridEditorDlg;
begin
  StringGridEditorDlg := TStringGridEditorDlg.Create(Application);
  try
    StringGridEditorDlg.LoadFromGrid(AStringGrid);
    if StringGridEditorDlg.ShowModal = mrOk then
    begin
      StringGridEditorDlg.SaveToGrid;
    end;
    Result := StringGridEditorDlg.Modified;
  finally
    StringGridEditorDlg.Free;
  end;
end;

{ TStringGridComponentEditor }

procedure TStringGridComponentEditor.ExecuteVerb(Index: Integer);
var
  Hook: TPropertyEditorHook;
begin
  if Index = 0 then
  begin
    GetHook(Hook);
    if EditStringGrid(GetComponent as TStringGrid) then
      if Assigned(Hook) then
        Hook.Modified(Self);
  end;
end;

function TStringGridComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result := sccsSGEdt
  else Result := '';
end;

function TStringGridComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TCheckListBoxComponentEditor }

procedure TCheckListBoxComponentEditor.DoShowEditor;
var Dlg: TCheckListBoxEditorDlg;
begin
  Dlg:=TCheckListBoxEditorDlg.Create(nil);
  with Dlg do begin
    Caption:=clbCheckListBoxEditor;
    BtnOK.Caption:=oisOk;
    BtnCancel.Caption:=oisCancel;
    BtnHelp.Caption:=cActionListEditorHelpCategory;
    BtnAdd.Caption:=oiscAdd;
    BtnDelete.Caption:=oiscDelete;
    BtnUp.Caption:=clbUp;
    BtnDown.Caption:=clbDown;
    BtnModify.Caption:='...';
    BtnModify.ShowHint:=true;
    BtnModify.Hint:=clbModify;
    Modified:=false;
  end;

  try
    if GetComponent is TCheckListBox then begin
      Dlg.aCheck:=TCheckListBox(GetComponent);
      if not HasHook then exit;

      AssignCheck(Dlg.FCheck, Dlg.aCheck);

      //ShowEditor
      if Dlg.ShowModal=mrOK then begin
        AssignCheck(Dlg.aCheck, Dlg.FCheck);
        Modified;
      end;
      if Dlg.Modified then
        Modified;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TCheckListBoxComponentEditor.ExecuteVerb(Index: Integer);
begin
  DoShowEditor;
end;

function TCheckListBoxComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:=clbCheckListBoxEditor+' ...';
end;

function TCheckListBoxComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

{ TCheckGroupEditorDlg }

type

  TCheckGroupEditorDlg = class(TForm)
    FBtnAdd: TButton;
    FBtnCancel: TBitBtn;
    FBtnDelete: TButton;
    FBtnDown: TButton;
    FBtnModify: TButton;
    FBtnOK: TBitBtn;
    FBtnUp: TButton;
    FCheck: TCheckGroup;
    FPanelButtons: TPanel;
    FPanelOKCancel: TPanel;
    FPopupMenu: TPopupMenu;
    LabelDisable: TLabel;
    procedure AddItem(Sender: TObject);
    procedure CreateItems(Sender: TObject);
    procedure DeleteItem(Sender: TObject);
    procedure ItemClick(Sender: TObject; Index: integer);
    procedure ModifyItem(Sender: TObject);
    procedure MoveDownItem(Sender: TObject);
    procedure MoveUpItem(Sender: TObject);
    procedure EnableDisable(Sender:TObject);
  private
    { private declarations }
    ItemIndex:integer;
  public
    { public declarations }
  end;

procedure TCheckGroupEditorDlg.AddItem(Sender:TObject);
var strItem:string;
begin
  if InputQuery(clbCheckGroupEditor, clbAdd, strItem) then
    FCheck.Items.Add(strItem);
end;

procedure TCheckGroupEditorDlg.DeleteItem(Sender:TObject);
begin
  if ItemIndex=-1 then exit;
  if MessageDlg(clbCheckGroupEditor,Format(clbDelete,[ItemIndex, FCheck.Items[ItemIndex]]),
    mtConfirmation, mbYesNo, 0)=mrYes then begin
    FCheck.Items.Delete(ItemIndex);
    if ItemIndex>FCheck.Items.Count-1 then
      ItemIndex:=FCheck.Items.Count-1;
  end;
end;

procedure TCheckGroupEditorDlg.MoveUpItem(Sender:TObject);
var itemtmp:string;
    checkedtmp:boolean;
begin
  if (FCheck.Items.Count<=1)or(ItemIndex<1) then exit;
   //swap the caption and the checked states
  itemtmp:=FCheck.Items[ItemIndex-1];
  checkedtmp:=FCheck.Checked[ItemIndex-1];
  FCheck.Items[ItemIndex-1]:=FCheck.Items[ItemIndex];
  FCheck.Checked[ItemIndex-1]:=FCheck.Checked[ItemIndex];
  FCheck.Items[ItemIndex]:=itemtmp;
  FCheck.Checked[ItemIndex]:=checkedtmp;
  //swap the states enabled
  checkedtmp:=FCheck.CheckEnabled[ItemIndex-1];
  FCheck.CheckEnabled[ItemIndex-1]:=FCheck.CheckEnabled[ItemIndex];
  FCheck.CheckEnabled[ItemIndex]:=checkedtmp;

  ItemIndex:=ItemIndex-1
end;

procedure TCheckGroupEditorDlg.MoveDownItem(Sender:TObject);
var itemtmp:string;
    checkedtmp:boolean;
begin
  if (FCheck.Items.Count<=1)or(ItemIndex=FCheck.Items.Count-1)or(ItemIndex=-1) then exit;
   //swap the caption and the checked states
  itemtmp:=FCheck.Items[ItemIndex+1];
  checkedtmp:=FCheck.Checked[ItemIndex+1];
  FCheck.Items[ItemIndex+1]:=FCheck.Items[ItemIndex];
  FCheck.Checked[ItemIndex+1]:=FCheck.Checked[ItemIndex];
  FCheck.Items[ItemIndex]:=itemtmp;
  FCheck.Checked[ItemIndex]:=checkedtmp;
  //swap the states enabled
  checkedtmp:=FCheck.CheckEnabled[ItemIndex+1];
  FCheck.CheckEnabled[ItemIndex+1]:=FCheck.CheckEnabled[ItemIndex];
  FCheck.CheckEnabled[ItemIndex]:=checkedtmp;

  ItemIndex:=ItemIndex+1
end;

procedure TCheckGroupEditorDlg.ModifyItem(Sender:TObject);
begin
  if ItemIndex=-1 then exit;
  FCheck.Items[ItemIndex]:=InputBox(clbCheckGroupEditor,clbModify,FCheck.Items[ItemIndex]);
end;

procedure TCheckGroupEditorDlg.ItemClick(Sender: TObject; Index: integer);
begin
  ItemIndex:=Index;
end;

procedure TCheckGroupEditorDlg.EnableDisable(Sender:TObject);
var i:integer;
begin
  for i:=0 to FCheck.Items.Count-1 do begin
    if (Sender=FPopupMenu.Items[i]) then
      FCheck.CheckEnabled[i]:=not FCheck.CheckEnabled[i]
  end;
end;

procedure TCheckGroupEditorDlg.CreateItems(Sender:TObject);
var i:integer;
begin
  FPopupMenu.Items.Clear;
  for i:=0 to FCheck.Items.Count-1 do begin
    FPopupMenu.Items.Add(TMenuItem.Create(self));
    FPopupMenu.Items[i].Caption:=FCheck.Items[i];
    FPopupMenu.Items[i].Checked:=FCheck.CheckEnabled[i];
    FPopupMenu.Items[i].OnClick:=@EnableDisable;
  end;;
end;

procedure TCheckGroupComponentEditor.DoShowEditor;
var Dlg : TCheckGroupEditorDlg;
    aCheck: TCheckGroup;
begin
  Dlg:=TCheckGroupEditorDlg.Create(nil);
  with Dlg do begin
    Caption:=clbCheckGroupEditor;
    ItemIndex:=-1;
    FBtnAdd.Caption:=oiscAdd;
    FBtnDelete.Caption:=oiscDelete;
    FBtnUp.Caption:=clbUp;
    FBtnDown.Caption:=clbDown;
    FBtnModify.ShowHint:=true;
    FBtnModify.Hint:=clbModify;
    FBtnModify.Caption:='...';
    LabelDisable.Caption:=clbDisable
  end;

  try
    if GetComponent is TCheckGroup then begin
      aCheck:=TCheckGroup(GetComponent);
      if not HasHook then exit;

      AssignCheck(Dlg.FCheck, aCheck);

      //ShowEditor
      if Dlg.ShowModal=mrOK then begin
        //Apply the modifications
        AssignCheck(aCheck, Dlg.FCheck);
        Modified;
      end;
    end;
  finally
    Dlg.Free;
  end;
end;

procedure TCheckGroupComponentEditor.AssignCheck(dstCheck, srcCheck: TCheckGroup);
var i: integer;
begin
  DstCheck.Items.Clear;
  DstCheck.Items:=srcCheck.Items;
  DstCheck.Caption:=srcCheck.Caption;
  for i:=0 to srcCheck.Items.Count-1 do begin
    dstCheck.Checked[i]:=srcCheck.Checked[i];
    dstCheck.CheckEnabled[i]:=srcCheck.CheckEnabled[i]
  end;
end;

procedure TCheckGroupComponentEditor.ExecuteVerb(Index: Integer);
begin
  DoShowEditor;
end;

function TCheckGroupComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:=clbCheckGroupEditor+' ...';
end;

function TCheckGroupComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

{ TToolBarComponentEditor }

procedure TToolBarComponentEditor.ExecuteVerb(Index: Integer);
var
  NewStyle: TToolButtonStyle;
  Hook: TPropertyEditorHook;
  NewToolButton: TToolButton;
  NewName: string;
  CurToolBar: TToolBar;
  SiblingButton: TToolButton;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;
  case Index of
  0: NewStyle:=tbsButton;
  1: NewStyle:=tbsCheck;
  2: NewStyle:=tbsSeparator;
  else exit;
  end;
  CurToolBar:=ToolBar;
  NewToolButton:=TToolButton.Create(CurToolBar.Owner);
  NewName:=GetDesigner.CreateUniqueComponentName(NewToolButton.ClassName);
  NewToolButton.Caption:=NewName;
  NewToolButton.Name:=NewName;
  NewToolButton.Style:=NewStyle;
  // position the button next to the last button
  if CurToolBar.ButtonCount>0 then begin
    SiblingButton := CurToolBar.Buttons[CurToolBar.ButtonCount-1];
    NewToolButton.SetBounds(SiblingButton.Left + SiblingButton.Width,
      SiblingButton.Top, NewToolButton.Width, NewToolButton.Height);
  end;
  NewToolButton.Parent:=CurToolBar;
  Hook.PersistentAdded(NewToolButton,true);
  Modified;
end;

function TToolBarComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result:='New Button';
  1: Result:='New Checkbutton';
  2: Result:='New Separator';
  else Result:='';
  end;
end;

function TToolBarComponentEditor.GetVerbCount: Integer;
begin
  Result:=3;
end;

function TToolBarComponentEditor.ToolBar: TToolBar;
begin
  Result:=TToolBar(GetComponent);
end;

{ TCommonDialogComponentEditor }

procedure TCommonDialogComponentEditor.TestDialog;
begin
  with Component as TCommonDialog do Execute;
end;

function TCommonDialogComponentEditor.GetVerbCount: integer;
begin
  Result:=1;
end;

function TCommonDialogComponentEditor.GetVerb(Index: integer): string;
begin
  case Index of
    0:Result:=oisTestDialog;
  else
    Result:=inherited GetVerb(Index);
  end;
end;

procedure TCommonDialogComponentEditor.ExecuteVerb(Index: integer);
begin
  case Index of
    0:TestDialog;
  else
    inherited ExecuteVerb(Index);
  end;
end;

//------------------------------------------------------------------------------

procedure InternalFinal;
var
  p: PComponentClassRec;
  i: integer;
begin
  if ComponentClassList<>nil then begin
    for i:=0 to ComponentClassList.Count-1 do begin
      p:=PComponentClassRec(ComponentClassList[i]);
      Dispose(p);
    end;
    ComponentClassList.Free;
  end;
end;

{ TTabControlComponentEditor }

const
  tcvAddTab       = 0;
  tcvInsertTab    = 1;
  tcvDeleteTab    = 2;
  tcvMoveTabLeft  = 3;
  tcvMoveTabRight = 4;

procedure TTabControlComponentEditor.DoAddTab;
begin
  TabControl.Tabs.Add(CreateNewTabCaption);
  Modified;
end;

procedure TTabControlComponentEditor.DoInsertTab;
begin
  TabControl.Tabs.Insert(TabControl.TabIndex,CreateNewTabCaption);
  Modified;
end;

procedure TTabControlComponentEditor.DoDeleteTab;
begin
  if (TabControl.Tabs.Count=0) then exit;
  TabControl.Tabs.Delete(TabControl.TabIndex);
  Modified;
end;

procedure TTabControlComponentEditor.DoMoveActiveTabLeft;
var
  Index: integer;
begin
  Index:=TabControl.TabIndex;
  if (Index<0) then exit;
  DoMoveTab(Index,Index-1);
end;

procedure TTabControlComponentEditor.DoMoveActiveTabRight;
var
  Index: integer;
begin
  Index:=TabControl.TabIndex;
  if (Index>=TabControl.Tabs.Count-1) then exit;
  DoMoveTab(Index,Index+1);
end;

procedure TTabControlComponentEditor.DoMoveTab(CurIndex, NewIndex: Integer);
begin
  TabControl.Tabs.Move(CurIndex,NewIndex);
  Modified;
end;

procedure TTabControlComponentEditor.AddMenuItemsForTabs(
  ParentMenuItem: TMenuItem);
var
  i: integer;
  NewMenuItem: TMenuItem;
begin
  ParentMenuItem.Enabled:=TabControl.Tabs.Count>0;
  for i:=0 to TabControl.Tabs.Count-1 do begin
    NewMenuItem:=TMenuItem.Create(ParentMenuItem);
    NewMenuItem.Name:='ShowTab'+IntToStr(i);
    NewMenuItem.Caption:='"'+TabControl.Tabs[i]+'"';
    NewMenuItem.OnClick:=@ShowTabMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

procedure TTabControlComponentEditor.ShowTabMenuItemClick(Sender: TObject);
var
  AMenuItem: TMenuItem;
  NewTabIndex: LongInt;
begin
  AMenuItem:=TMenuItem(Sender);
  if (AMenuItem=nil) or (not (AMenuItem is TMenuItem)) then exit;
  NewTabIndex:=AMenuItem.MenuIndex;
  if (NewTabIndex<0) or (NewTabIndex>=TabControl.Tabs.Count) then exit;
  TabControl.TabIndex:=NewTabIndex;
  Modified;
end;

function TTabControlComponentEditor.CreateNewTabCaption: string;
begin
  Result:='New Tab';
  while TabControl.IndexOfTabWithCaption(Result)>=0 do
    Result:=CreateNextIdentifier(Result);
end;

procedure TTabControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    tcvAddTab:       DoAddTab;
    tcvInsertTab:    DoInsertTab;
    tcvDeleteTab:    DoDeleteTab; // beware: this can free the editor itself
    tcvMoveTabLeft:  DoMoveActiveTabLeft;
    tcvMoveTabRight: DoMoveActiveTabRight;
  end;
end;

function TTabControlComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    tcvAddTab:       Result:=tccesAddTab;
    tcvInsertTab:    Result:=tccesInsertTab;
    tcvDeleteTab:    Result:=tccesDeleteTab;
    tcvMoveTabLeft:  Result:=tccesMoveTabLeft;
    tcvMoveTabRight: Result:=tccesMoveTabRight;
  else
    Result:='';
  end;
end;

function TTabControlComponentEditor.GetVerbCount: Integer;
begin
  Result:=5;
end;

procedure TTabControlComponentEditor.PrepareItem(Index: Integer;
  const AnItem: TMenuItem);
begin
  inherited PrepareItem(Index, AnItem);
  case Index of
    tcvAddTab:       ;
    tcvInsertTab:    AnItem.Enabled:=TabControl.TabIndex>=0;
    tcvDeleteTab:    AnItem.Enabled:=TabControl.TabIndex>=0;
    tcvMoveTabLeft:  AnItem.Enabled:=TabControl.TabIndex>0;
    tcvMoveTabRight: AnItem.Enabled:=TabControl.TabIndex<TabControl.Tabs.Count-1;
  end;
end;

function TTabControlComponentEditor.TabControl: TCustomTabControl;
begin
  Result:=TCustomTabControl(GetComponent);
end;

initialization
  {$I checkgroupeditordlg.lrs}

  RegisterComponentEditorProc:=@DefaultRegisterComponentEditorProc;
  RegisterComponentEditor(TCustomNotebook,TNotebookComponentEditor);
  RegisterComponentEditor(TCustomPage,TPageComponentEditor);
  RegisterComponentEditor(TCustomTabControl,TTabControlComponentEditor);
  RegisterComponentEditor(TStringGrid,TStringGridComponentEditor);
  RegisterComponentEditor(TCheckListBox,TCheckListBoxComponentEditor);
  RegisterComponentEditor(TCheckGroup,TCheckGroupComponentEditor);
  RegisterComponentEditor(TToolBar,TToolBarComponentEditor);
  RegisterComponentEditor(TCommonDialog, TCommonDialogComponentEditor);

finalization
  InternalFinal;

end.

