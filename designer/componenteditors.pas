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

  ToDo:
}
unit ComponentEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Forms, Controls, PropEdits, Menus, ExtCtrls;


{ TComponentEditorDesigner }

type
  TComponentEditorDesigner = class(TIDesigner)
  protected
    function GetPropertyEditorHook: TPropertyEditorHook; virtual; abstract;
  public
    function CreateUniqueComponentName(const AClassName: string): string; virtual; abstract;
    property PropertyEditorHook: TPropertyEditorHook read GetPropertyEditorHook;
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
    function GetHook(var Hook: TPropertyEditorHook): boolean; virtual; abstract;
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
    procedure Copy; virtual; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    property Component: TComponent read FComponent;
    property Designer: TComponentEditorDesigner read GetDesigner;
    function GetHook(var Hook: TPropertyEditorHook): boolean; override;
  end;


{ TDefaultComponentEditor
  An editor that provides default behavior for the double-click that will
  iterate through the properties looking for the most appropriate method
  property to edit }
  TDefaultComponentEditor = class(TComponentEditor)
  private
    FFirst: TPropertyEditor;
    FBest: TPropertyEditor;
    FContinue: Boolean;
    procedure CheckEdit(Prop: TPropertyEditor);
  protected
    procedure EditProperty(const Prop: TPropertyEditor;
      var Continue: Boolean); virtual;
  public
    procedure Edit; override;
  end;
  
{ TNotebookComponentEditor
  The default component editor for TNotebook. }
  TNotebookComponentEditor = class(TDefaultComponentEditor)
  protected
    procedure AddNewPageToDesigner(Index: integer); virtual;
    procedure DoAddPage; virtual;
    procedure DoInsertPage; virtual;
    procedure DoDeletePage; virtual;
    procedure DoMoveActivePageLeft; virtual;
    procedure DoMoveActivePageRight; virtual;
    procedure DoMoveActivePage(CurIndex, NewIndex: Integer); virtual;
    procedure AddMenuItemsForPages(ParentMenuItem: TMenuItem); virtual;
    procedure ShowPageMenuItemClick(Sender: TObject);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure PrepareItem(Index: Integer; const AnItem: TMenuItem); override;
    function Notebook: TNotebook; virtual;
  end;
  
{ TPageComponentEditor
  The default component editor for TPage. }
  TPageComponentEditor = class(TNotebookComponentEditor)
  protected
  public
    function Notebook: TNotebook; override;
    function Page: TPage; virtual;
  end;


{ Register a component editor to be created when a component derived from
  ComponentClass is the only selection in the designer }
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
  writeln(Classname+'.ExecuteVerb: ',Index);
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

function TComponentEditor.GetHook(var Hook: TPropertyEditorHook): boolean;
begin
  Result:=false;
  if GetDesigner=nil then exit;
  Hook:=GetDesigner.PropertyEditorHook;
  Result:=Hook<>nil;
end;

{ TDefaultComponentEditor }

procedure TDefaultComponentEditor.CheckEdit(Prop: TPropertyEditor);
begin
  if FContinue then
    EditProperty(Prop, FContinue);
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
  if not Assigned(FFirst)
  and (Prop is TMethodPropertyEditor) then
    FFirst := Prop;
  PropName := Prop.GetName;
  BestName := '';
  if Assigned(FBest) then BestName := FBest.GetName;
  if CompareText(PropName, 'ONCREATE') = 0 then
    ReplaceBest
  else if CompareText(BestName, 'ONCREATE') <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
      ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;

procedure TDefaultComponentEditor.Edit;
var
  Components: TComponentSelectionList;
  PropertyEditorHook: TPropertyEditorHook;
begin
  PropertyEditorHook:=nil;
  if not GetHook(PropertyEditorHook) then exit;
  Components := TComponentSelectionList.Create;
  FContinue := True;
  Components.Add(Component);
  FFirst := nil;
  FBest := nil;
  try
    GetComponentProperties(Components, tkAny, PropertyEditorHook,
                           @CheckEdit, nil);
    if FContinue then
      if Assigned(FBest) then
        FBest.Edit
      else if Assigned(FFirst) then
        FFirst.Edit;
  finally
    FFirst := nil;
    FBest := nil;
    Components.Free;
  end;
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
  GetDesigner.SelectOnlyThisComponent(
    TPage(NoteBook.PageList[NoteBook.PageIndex]));
end;

procedure TNotebookComponentEditor.AddNewPageToDesigner(Index: integer);
var
  Hook: TPropertyEditorHook;
  NewPage: TPage;
  NewName: string;
begin
  Hook:=nil;
  if not GetHook(Hook) then exit;
  NewPage:=NoteBook.Page[Index];
  NewName:=GetDesigner.CreateUniqueComponentName(NewPage.ClassName);
  NewPage.Caption:=NewName;
  NewPage.Name:=NewName;
  NoteBook.PageIndex:=Index;
  Hook.ComponentAdded(NewPage,true);
  GetDesigner.Modified;
end;

procedure TNotebookComponentEditor.DoAddPage;
var
  Hook: TPropertyEditorHook;
begin
  if not GetHook(Hook) then exit;
  NoteBook.Pages.Add('');
  AddNewPageToDesigner(NoteBook.PageCount-1);
end;

procedure TNotebookComponentEditor.DoInsertPage;
var
  Hook: TPropertyEditorHook;
  NewIndex: integer;
begin
  if not GetHook(Hook) then exit;
  NewIndex:=Notebook.PageIndex;
  if NewIndex<0 then NewIndex:=0;
  Notebook.Pages.Insert(NewIndex,'');
  AddNewPageToDesigner(NewIndex);
end;

procedure TNotebookComponentEditor.DoDeletePage;
var
  Hook: TPropertyEditorHook;
  OldIndex: integer;
begin
  OldIndex:=Notebook.PageIndex;
  if (OldIndex>=0) and (OldIndex<Notebook.PageCount) then begin
    if not GetHook(Hook) then exit;
    Hook.DeleteComponent(TComponent(NoteBook.PageList[OldIndex]));
    GetDesigner.Modified;
  end;
end;

procedure TNotebookComponentEditor.DoMoveActivePageLeft;
var
  Index: integer;
begin
  Index:=NoteBook.PageIndex;
  if (Index<0) then exit;
  DoMoveActivePage(Index,Index-1);
end;

procedure TNotebookComponentEditor.DoMoveActivePageRight;
var
  Index: integer;
begin
  Index:=NoteBook.PageIndex;
  if (Index>=0)
  and (Index>=NoteBook.PageCount-1) then exit;
  DoMoveActivePage(Index,Index+1);
end;

procedure TNotebookComponentEditor.DoMoveActivePage(
  CurIndex, NewIndex: Integer);
begin
  NoteBook.Pages.Move(CurIndex,NewIndex);
  GetDesigner.Modified;
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
    NewMenuItem.Caption:=
      TPage(Notebook.PageList[i]).Name+' "'+Notebook.Pages[i]+'"';
    NewMenuItem.OnClick:=@ShowPageMenuItemClick;
    ParentMenuItem.Add(NewMenuItem);
  end;
end;

procedure TNotebookComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    nbvAddPage:       DoAddPage;
    nbvInsertPage:    DoInsertPage;
    nbvDeletePage:    DoDeletePage;
    nbvMovePageLeft:  DoMoveActivePageLeft;
    nbvMovePageRight: DoMoveActivePageRight;
  end;
end;

function TNotebookComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    nbvAddPage:       Result:='Add page';
    nbvInsertPage:    Result:='Insert page';
    nbvDeletePage:    Result:='Delete page';
    nbvMovePageLeft:  Result:='Move page left';
    nbvMovePageRight: Result:='Move page right';
    nbvShowPage:      Result:='Show page ...';
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

function TNotebookComponentEditor.Notebook: TNotebook;
begin
  Result:=TNotebook(GetComponent);
end;

{ TPageComponentEditor }

function TPageComponentEditor.Notebook: TNotebook;
var
  APage: TPage;
begin
  APage:=Page;
  if (APage.Parent<>nil) and (APage.Parent is TNoteBook) then
    Result:=TNoteBook(APage.Parent);
end;

function TPageComponentEditor.Page: TPage;
begin
  Result:=TPage(GetComponent);
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

initialization
  RegisterComponentEditorProc:=@DefaultRegisterComponentEditorProc;
  RegisterComponentEditor(TNotebook,TNotebookComponentEditor);
  RegisterComponentEditor(TPage,TPageComponentEditor);
  
finalization
  InternalFinal;

end.

