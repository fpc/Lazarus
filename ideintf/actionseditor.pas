{ Copyright (C) 2004

  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU General Public License along with
  this program; if not, write to the Free Software Foundation, Inc., 59 Temple
  Place - Suite 330, Boston, MA 02111-1307, USA.
  
  
  implementing ActionList Editor
  
  author:
     Radek Cervinka, radek.cervinka@centrum.cz
     Mattias Gaertner
  
  contributors:
     Mattias Gaertner
  
  version:
    0.1 - 26-27.2.2004 - write all from scratch
    0.2 -  3.3.2004 - speed up filling listboxes
                      some ergonomic fixes (like stay in category after ADD)
                      fixed possible language problems
    0.3 - 27.3.2004 - rename action > actualise editor
    0.4 - 29.3.2004 - dblclick generate xxx.OnExecute code to editor
                      
  TODO:- after changing action category in Object Inspector
         need sort category to listbox
       - sometimes click in listbox causes selecting last item
         (it's an strange gtk error. The LCL and the gtk intf do not send any
          change to the gtk. Either it is a bug in the gtk1 or we are doing
          something wrong in the handlers.)
}


unit ActionsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, StdCtrls, Buttons, ActnList, ExtCtrls,
  Controls, Dialogs, ObjInspStrConsts, ComponentEditors, PropEdits, DBActns,
  StdActns;

type
  { TActionListEditor }

  TActionListEditor = class(TForm)
    btnAdd: TButton;
    btnDelete: TButton;
    lblCategory: TLabel;
    lblName: TLabel;
    lstActionName: TListBox;
    lstCategory: TListBox;
    Panel: TPanel;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure lstActionNameClick(Sender: TObject);
    procedure lstActionNameDblClick(Sender: TObject);
    procedure lstCategoryClick(Sender: TObject);
  private
    FActionList: TActionList;
    FDesigner: TComponentEditorDesigner;
  protected
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure CreateActionListEditor; // create form
    function GetSelectedAction: TContainedAction;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetActionList(AActionList: TActionList);
    procedure FillCategories;
    procedure FillActionByCategory(iIndex: Integer);
    property Designer:TComponentEditorDesigner read FDesigner write FDesigner;
  end;


  { TActionListComponentEditor }

  TActionListComponentEditor = class(TComponentEditor)
  private
    FActionList: TActionList;
    FDesigner: TComponentEditorDesigner;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    property ActionList: TActionList read FActionList write FActionList;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
  

  { Action Registration }
  
  TRegisteredAction = class
  private
    FActionClass: TBasicActionClass;
    FGroupId: Integer;
  public
    constructor Create(TheActionClass: TBasicActionClass; TheGroupID: integer);
    property ActionClass: TBasicActionClass read FActionClass;
    property GroupId: Integer read FGroupId;
  end;
  PRegisteredAction = ^TRegisteredAction;

  TRegisteredActionCategory = class
  private
    FCount: integer;
    FName: string;
    FItems: PRegisteredAction;
    FResource: TComponentClass;
    function GetItems(Index: integer): TRegisteredAction;
  public
    constructor Create(const CategoryName: string; AResource: TComponentClass);
    procedure Add(const AClasses: array of TBasicActionClass);
    destructor Destroy; override;
    function IndexOfClass(AClass: TBasicActionClass): integer;
    procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
    property Count: integer read FCount;
    property Name: string read FName;
    property Items[Index: integer]: TRegisteredAction read GetItems;
    property Resource: TComponentClass read FResource;
  end;
  
  TRegisteredActionCategories = class
  private
    FItems: TList;
    function GetItems(Index: integer): TRegisteredActionCategory;
  public
    procedure Add(const CategoryName: string;
                  const AClasses: array of TBasicActionClass;
                  AResource: TComponentClass);
    destructor Destroy; override;
    function IndexOfCategory(const CategoryName: string): integer;
    procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
    function FindResource(AClass: TBasicActionClass): TComponentClass;
    function Count: integer;
    property Items[Index: integer]: TRegisteredActionCategory read GetItems;
  end;

var
  RegisteredActions: TRegisteredActionCategories;

type
  TNotifyActionListChange = procedure;

var
  NotifyActionListChange: TNotifyActionListChange;

procedure RegisterActions(const ACategory: string;
                          const AClasses: array of TBasicActionClass;
                          AResource: TComponentClass);
procedure UnRegisterActions(const Classes: array of TBasicActionClass);
procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
function CreateAction(TheOwner: TComponent;
                      ActionClass: TBasicActionClass): TBasicAction;


  { ActionListEditorForm }

var
  ActionListEditorForm: TActionListEditor; // global since there is normally
                                           // only one needed

procedure ShowActionListEditor(AActionList: TActionList;
  ADesigner: TComponentEditorDesigner);

implementation


procedure RegisterActions(const ACategory: string;
  const AClasses: array of TBasicActionClass; AResource: TComponentClass);
begin
  RegisteredActions.Add(ACategory,AClasses,AResource);
end;

procedure UnRegisterActions(const Classes: array of TBasicActionClass);
begin

end;

procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
begin
  RegisteredActions.EnumActions(Proc,Info);
end;

function CreateAction(TheOwner: TComponent;
  ActionClass: TBasicActionClass): TBasicAction;
var
  ResourceClass: TComponentClass;
  ResInstance: TComponent;
  i: Integer;
  Component: TComponent;
  Action: TBasicAction;
  Src: TCustomAction;
  Dest: TCustomAction;
begin
  Result := ActionClass.Create(TheOwner);
  // find a Resource component registered for this ActionClass
  ResourceClass:=RegisteredActions.FindResource(ActionClass);
  if ResourceClass=nil then exit;
  ResInstance:=ResourceClass.Create(nil);
  try
    // find an action owned by the Resource component
    Action:=nil;
    for i:=0 to ResInstance.ComponentCount-1 do begin
      Component:=ResInstance.Components[i];
      if (CompareText(Component.ClassName,ActionClass.ClassName)=0)
      and (Component is TBasicAction) then begin
        Action:=TBasicAction(Component);
        break;
      end;
    end;
    if Action=nil then exit;
    
    // copy TCustomAction properties
    if (Action is TCustomAction) and (Result is TCustomAction) then begin
      Src:=TCustomAction(Action);
      Dest:=TCustomAction(Result);
      Dest.Caption:=Src.Caption;
      Dest.Checked:=Src.Checked;
      Dest.Enabled:=Src.Enabled;
      Dest.HelpContext:=Src.HelpContext;
      Dest.Hint:=Src.Hint;
      Dest.ImageIndex:=Src.ImageIndex;
      Dest.ShortCut:=Src.ShortCut;
      Dest.Visible:=Src.Visible;
      if (Dest is TContainedAction) and (Dest.ImageIndex>=0)
      and (Src is TContainedAction) then begin
        // ToDo: copy image
        
      end;
    end;
  finally
    ResInstance.Free;
  end;
end;

procedure ShowActionListEditor(AActionList: TActionList;
  ADesigner:TComponentEditorDesigner);
begin
  if AActionList=nil then
    Raise Exception.Create('ShowActionListEditor AActionList=nil');
  if ActionListEditorForm=nil then
    ActionListEditorForm:=TActionListEditor.Create(Application);
  ActionListEditorForm.Designer:=ADesigner;
  ActionListEditorForm.SetActionList(AActionList);
  ActionListEditorForm.ShowOnTop;
end;

procedure TActionListEditor.btnAddClick(Sender: TObject);
var
  NewAction:TContainedAction;

begin
  NewAction:=TAction.Create(FActionList.Owner);
{  writeln('Add entry');
  writeln(NewAction.ClassName);
  writeln(FDesigner.CreateUniqueComponentName(NewAction.ClassName));}
  NewAction.Name:=FDesigner.CreateUniqueComponentName(NewAction.ClassName);
  writeln(NewAction.Name);
  
  if lstCategory.ItemIndex>1 then // ignore first two items (virtual categories)
    NewAction.Category:=lstCategory.Items[lstCategory.ItemIndex]
  else
    NewAction.Category:='';
//  writeln('Category',NewAction.Category);

  NewAction.ActionList:=FActionList;

  FDesigner.PropertyEditorHook.PersistentAdded(NewAction,true);
  FDesigner.Modified;
//  writeln('Add done');
end;

procedure TActionListEditor.btnDeleteClick(Sender: TObject);
var
  iNameIndex:Integer;
  OldName: string;
  OldAction: TContainedAction;
begin
//  writeln('Delete Enter');
  iNameIndex:=lstActionName.ItemIndex;
  if iNameIndex<0 then Exit;
  OldName:=lstActionName.Items[iNameIndex];
  writeln('',OldName);
  lstActionName.Items.Delete(iNameIndex);
  
  OldAction:=FActionList.ActionByName(OldName);

  // be gone
  if assigned(OldAction) then
  begin
    try
      FDesigner.PropertyEditorHook.PersistentDeleting(OldAction);
      OldAction.Free;
    except
      on E: Exception do begin
        MessageDlg('Error deleting action',
          'Error while deleting action:'#13
          +E.Message,mtError,[mbOk],0);
      end;
    end;
  end;
  
  if lstActionName.Items.Count=0 then // last act in category > rebuild
    FillCategories
  else
  begin
    if iNameIndex>=lstActionName.Items.Count then
      lstActionName.ItemIndex:=lstActionName.Items.Count -1
    else
      lstActionName.ItemIndex:=iNameIndex;
      
    FDesigner.SelectOnlyThisComponent(
       FActionList.ActionByName(lstActionName.Items[lstActionName.ItemIndex]));
  end;
end;

procedure TActionListEditor.lstActionNameDblClick(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  CurAction:=GetSelectedAction;
  if CurAction=nil then exit;
  // Add OnExecute for this action
  CreateComponentEvent(CurAction,'OnExecute');
end;

procedure TActionListEditor.lstCategoryClick(Sender: TObject);
begin
  if lstCategory.ItemIndex<0 then Exit;
  FillActionByCategory(lstCategory.ItemIndex);
end;

procedure TActionListEditor.lstActionNameClick(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  if lstActionName.ItemIndex<0 then Exit;
  CurAction:=GetSelectedAction;
  if CurAction=nil then exit;

  FDesigner.SelectOnlyThisComponent(CurAction);
end;

procedure TActionListEditor.OnPersistentDeleting(APersistent: TPersistent);
var
  xIndex:Integer;
begin
  if (APersistent is TAction) then
  begin
    xIndex:=lstActionName.Items.IndexOf(TAction(APersistent).Name);
    if xIndex<0 then Exit; // action not showed in listbox (other category)
    lstActionName.Items.Delete(xIndex);
    if lstActionName.Items.Count=0 then
      FillCategories //last action in category is deleted, rebuild category list
    else
      if xIndex>=lstActionName.Items.Count then
        lstActionName.ItemIndex:=lstActionName.Items.Count-1
      else
        lstActionName.ItemIndex:=xIndex;
  end;
end;

procedure TActionListEditor.OnPersistentAdded(APersistent: TPersistent;
  Select: boolean);
begin
  if (APersistent is TAction) then
    // ToDo: only set update flag and do not rebuild everything on every change
    FillCategories;
end;

procedure TActionListEditor.OnComponentRenamed(AComponent: TComponent);
var
  iIndex:Integer;
begin
  if not (AComponent is TAction) then Exit;
  FillActionByCategory(lstCategory.ItemIndex);
  iIndex:= lstActionName.Items.IndexOf(AComponent.Name);// is new action showed?
  if iIndex<0 then Exit;
  lstActionName.ItemIndex:=iIndex; // yes, select is
end;

constructor TActionListEditor.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  CreateActionListEditor;
end;

destructor TActionListEditor.Destroy;
begin
  if GlobalDesignHook<>nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TActionListEditor.CreateActionListEditor;
begin
  Caption:=oisActionListEditor;
  BorderStyle:=bsDialog;
  SetInitialBounds(0,0,400,300);
  Position :=poScreenCenter;
  Panel:=TPanel.Create(Self);
  with Panel do
  begin
    Parent:=Self;
    Align:=alTop;
    Height:=42;
  end;
  btnAdd:=TButton.Create(Panel);
  with btnAdd do
  begin
    Parent:=Panel;
    OnClick:=@btnAddClick;
    Caption:=oisAdd;
    Top:=8;
    Left:=2;
    Width:=75;
  end;
  btnDelete:=TButton.Create(Panel);
  with btnDelete do
  begin
    Parent:=Panel;
    OnClick:=@btnDeleteClick;
    Caption:=sccsLvEdtBtnDel;
    Top:=8;
    Width:=75;
    Left:=128;
  end;
  lstCategory:=TListBox.Create(Self);
  with lstCategory do
  begin
    Parent:=Self;
    SetBounds(8,72,112, 224);
    OnClick:=@lstCategoryClick;
  end;

  lstActionName:=TListBox.Create(Self);
  with lstActionName do
  begin
    Parent:=Self;
    SetBounds(130,72, 160 ,224);
    OnClick:=@lstActionNameClick;
    OnDblClick:=@lstActionNameDblClick;
  end;
  lblCategory:=TLabel.Create(Self);
  with lblCategory do
  begin
    Parent:=Self;
    Caption:=oisCategory;
    SetBounds(8,48, 65 ,17);
  end;

  lblName:=TLabel.Create(Self);
  with lblName do
  begin
    Parent:=Self;
    Caption:=oisAction;
    SetBounds(130,48, 65 ,17);
  end;

  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
  GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
end;

function TActionListEditor.GetSelectedAction: TContainedAction;
begin
  if lstActionName.ItemIndex>=0 then
    Result:=
         FActionList.ActionByName(lstActionName.Items[lstActionName.ItemIndex])
  else
    Result:=nil;
end;

procedure TActionListEditor.SetActionList(AActionList: TActionList);
begin
  FActionList:=AActionList;
  FillCategories;
end;

procedure TActionListEditor.FillCategories;
var
  i:Integer;
  sCategory:String;
  xIndex:Integer;
  sOldCategory:String;
begin
  // try remember old category
  sOldCategory:='';
  if (lstCategory.Items.Count>0) and (lstCategory.ItemIndex>-1) then
    sOldCategory:=lstCategory.Items[lstCategory.ItemIndex];

  lstCategory.Items.BeginUpdate;
  try
    lstCategory.Clear;
    lstCategory.Items.Add(cActionListEditorUnknownCategory);
    lstCategory.Items.Add(cActionListEditorAllCategory);

    for i:=0 to FActionList.ActionCount-1 do
    begin
      sCategory:=FActionList.Actions[i].Category;
      if Trim(sCategory)='' then
        Continue;
      xIndex:=lstCategory.Items.IndexOf(sCategory);
      if xIndex<0 then
        lstCategory.Items.Add(sCategory);
    end;
  finally
    lstCategory.Items.EndUpdate;
  end;
  xIndex:=lstCategory.Items.IndexOf(sOldCategory);
  if xIndex<0 then
    xIndex:=0;
  lstCategory.ItemIndex:=xIndex;

  FillActionByCategory(xIndex);
end;

procedure TActionListEditor.FillActionByCategory(iIndex:Integer);
var
  i:Integer;
  sCategory:String;
begin

  lstActionName.Items.BeginUpdate;
  if iIndex<0 then iIndex:=1;// all
  try
    lstActionName.Clear;
    // handle all
    if iIndex = 1 then
    begin
      for i:=0 to FActionList.ActionCount-1 do
        lstActionName.Items.Add(FActionList.Actions[i].Name);
      Exit; //throught finally
    end;

    // handle unknown
    if iIndex = 0 then
    begin
      for i:=0 to FActionList.ActionCount-1 do
      begin
        if Trim(FActionList.Actions[i].Category)='' then
          lstActionName.Items.Add(FActionList.Actions[i].Name);
      end;
      Exit; //throught finally
    end;

    // else sort to categories
    sCategory:=lstCategory.Items[iIndex];
    for i:=0 to FActionList.ActionCount-1 do
    begin
      if FActionList.Actions[i].Category = sCategory then
        lstActionName.Items.Add(FActionList.Actions[i].Name);
    end;
  finally
    lstActionName.Items.EndUpdate;
  end;
end;

{ TActionListComponentEditor }

constructor TActionListComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner:=ADesigner;
end;

destructor TActionListComponentEditor.Destroy;
begin
  if assigned(ActionListEditorForm)
  and (ActionListEditorForm.FActionList=GetComponent) then
    FreeThenNil(ActionListEditorForm);
  inherited Destroy;
end;

procedure TActionListComponentEditor.Edit;
begin
  writeln('TActionListComponentEditor.Edit ',GetComponent.Name);
  ShowActionListEditor(GetComponent as TActionList, FDesigner);
end;

function TActionListComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TActionListComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:=oisEditActionList;
end;

procedure TActionListComponentEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;

//------------------------------------------------------------------------------

{ TRegisteredActionCategory }

function TRegisteredActionCategory.GetItems(Index: integer): TRegisteredAction;
begin
  Result:=FItems[Index];
end;

constructor TRegisteredActionCategory.Create(const CategoryName: string;
  AResource: TComponentClass);
begin
  FName:=CategoryName;
  FResource:=AResource;
end;

procedure TRegisteredActionCategory.Add(
  const AClasses: array of TBasicActionClass);
var
  i: integer;
  CurCount: Integer;
  IsDouble: Boolean;
  j: Integer;
  AClass: TBasicActionClass;
  l: Integer;
begin
  l:=High(AClasses)-Low(AClasses)+1;
  if l=0 then exit;
  CurCount:=FCount;
  inc(FCount,l);
  // add all classes (ignoring doubles)
  ReAllocMem(FItems,SizeOf(TBasicActionClass)*FCount);
  for i:=Low(AClasses) to High(AClasses) do begin
    AClass:=AClasses[i];
    // check if already exists
    IsDouble:=false;
    for j:=0 to CurCount-1 do begin
      if FItems[j].ActionClass=AClass then begin
        IsDouble:=true;
        break;
      end;
    end;
    // add
    if not IsDouble then begin
      // TODO use current designer group instead of -1
      FItems[CurCount]:=TRegisteredAction.Create(AClass,-1);
      inc(CurCount);
      RegisterNoIcon([AClass]);
      Classes.RegisterClass(AClass);
    end;
  end;
  // resize FItems
  if CurCount<FCount then begin
    FCount:=CurCount;
    ReAllocMem(FItems,SizeOf(TBasicActionClass)*FCount);
  end;
end;

destructor TRegisteredActionCategory.Destroy;
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do Items[i].Free;
  ReAllocMem(FItems,0);
  inherited Destroy;
end;

function TRegisteredActionCategory.IndexOfClass(AClass: TBasicActionClass
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (FItems[Result].ActionClass<>AClass) do dec(Result);
end;

procedure TRegisteredActionCategory.EnumActions(Proc: TEnumActionProc;
  Info: Pointer);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Proc(Name,FItems[i].ActionClass,Info);
end;

{ TRegisteredAction }

constructor TRegisteredAction.Create(TheActionClass: TBasicActionClass;
  TheGroupID: integer);
begin
  FActionClass:=TheActionClass;
  FGroupId:=TheGroupID;
end;

{ TRegisteredActionCategories }

function TRegisteredActionCategories.GetItems(Index: integer
  ): TRegisteredActionCategory;
begin
  Result:=TRegisteredActionCategory(FItems[Index]);
end;

procedure TRegisteredActionCategories.Add(const CategoryName: string;
  const AClasses: array of TBasicActionClass; AResource: TComponentClass);
var
  i: LongInt;
  Category: TRegisteredActionCategory;
begin
  i:=IndexOfCategory(CategoryName);
  if i>=0 then begin
    Category:=Items[i];
    if Category.Resource<>AResource then
      raise Exception.Create('TRegisteredActionCategories.Add Resource<>OldResource');
  end else begin
    Category:=TRegisteredActionCategory.Create(CategoryName,AResource);
    if FItems=nil then FItems:=TList.Create;
    FItems.Add(Category);
  end;
  Category.Add(AClasses);
  if Assigned(NotifyActionListChange) then
    NotifyActionListChange;
end;

destructor TRegisteredActionCategories.Destroy;
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do Items[i].Free;
  FItems.Free;
  inherited Destroy;
end;

function TRegisteredActionCategories.IndexOfCategory(const CategoryName: string
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (CompareText(Items[Result].Name,CategoryName)<>0) do
    dec(Result);
end;

procedure TRegisteredActionCategories.EnumActions(Proc: TEnumActionProc;
  Info: Pointer);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].EnumActions(Proc,Info);
end;

function TRegisteredActionCategories.FindResource(AClass: TBasicActionClass
  ): TComponentClass;
var
  Category: TRegisteredActionCategory;
  i: Integer;
begin
  for i:=0 to Count-1 do begin
    Category:=Items[i];
    if Category.IndexOfClass(AClass)>=0 then begin
      Result:=Category.Resource;
      exit;
    end;
  end;
  Result:=nil;
end;

function TRegisteredActionCategories.Count: integer;
begin
  if FItems=nil then
    Result:=0
  else
    Result:=FItems.Count;
end;

procedure RegisterStandardActions;
begin
  // TODO
  //  - default images for actions
  
  // register edit actions
  RegisterActions('Edit',[TEditCut,TEditCopy,TEditPaste,TEditSelectAll,
   TEditUndo,TEditDelete],nil);
  // register help actions
  RegisterActions('Help',[THelpAction,THelpContents,THelpTopicSearch,
    THelpOnHelp,THelpContextAction],nil);
  // register file actions
  RegisterActions('File',[TFileOpen,TFileOpenWith,TFileSaveAs,TFileExit],nil);
  // register dialog actions
  RegisterActions('Dialog',[TFontEdit,TColorSelect],nil);
  // register database actions
  RegisterActions('Database',[TDataSetFirst,TDataSetLast,TDataSetNext,
    TDataSetPrior,TDataSetRefresh,TDataSetCancel,TDataSetDelete,TDataSetEdit,
    TDataSetInsert,TDataSetPost],nil);
end;

initialization
  NotifyActionListChange:=nil;
  ActionListEditorForm:=nil;
  RegisteredActions:=TRegisteredActionCategories.Create;
  RegisterActionsProc := @RegisterActions;
  UnRegisterActionsProc := @UnregisterActions;
  EnumRegisteredActionsProc := @EnumActions;
  CreateActionProc := @CreateAction;
  RegisterComponentEditor(TActionList,TActionListComponentEditor);
  RegisterStandardActions;
  
finalization
  RegisteredActions.Free;
  RegisteredActions:=nil;
end.


