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
  
  contributors:
  
  
  version:
    0.1 - 26-27.2.2004 - write all from scratch
}


unit ActionsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, StdCtrls, Buttons, ActnList, ExtCtrls,
  Controls, ObjInspStrConsts, ComponentEditors, PropEdits;

type
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
    procedure lstCategoryClick(Sender: TObject);
    procedure lstActionNameClick(Sender: TObject);
  private
    FActionList:TActionList;
    FDesigner: TComponentEditorDesigner;
  protected
    procedure OnComponentDeleting(AComponent: TComponent);
    procedure OnComponentAdded(AComponent: TComponent; Select: boolean);
    procedure CreateActionListEditor; // create form
    function GetSelectedAction: TContainedAction;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetActionList(AActionList:TActionList);
    procedure FillCategories;
    procedure FillActionByCategory(const sCategory:String);
    property Designer:TComponentEditorDesigner read FDesigner write FDesigner;
  end;

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

var
  ActionListEditorForm: TActionListEditor;


procedure ShowActionListEditor(AActionList: TActionList;
  ADesigner: TComponentEditorDesigner);

implementation

procedure ShowActionListEditor(AActionList: TActionList;
  ADesigner:TComponentEditorDesigner);
begin
  if AActionList=nil then
    Raise Exception.Create('ShowActionListEditor AActionList=nil');
  if ActionListEditorForm=nil then
  begin
    ActionListEditorForm:=TActionListEditor.Create(Application);
  end;
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
  
  if lstCategory.ItemIndex>-1 then
    NewAction.Category:=lstCategory.Items[lstCategory.ItemIndex]
  else
    NewAction.Category:='';
//  writeln('Category',NewAction.Category);

  NewAction.ActionList:=FActionList;

  FDesigner.PropertyEditorHook.ComponentAdded(NewAction,true);
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
  if OldAction=nil then begin
    // item already deleted -> only update list
    exit;
  end;

  // be gone
  try
    OldAction.Free;
  except
    // rebuild
    FillActionByCategory(lstCategory.Items[lstCategory.ItemIndex]);
  end;
end;

procedure TActionListEditor.lstCategoryClick(Sender: TObject);
begin
  if lstCategory.ItemIndex<0 then Exit;
  FillActionByCategory(lstCategory.Items[lstCategory.ItemIndex]);
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

procedure TActionListEditor.OnComponentDeleting(AComponent: TComponent);
begin
  if (AComponent is TAction) then
    // ToDo: only set update flag and do not rebuild everything on every change
    FillCategories;
end;

procedure TActionListEditor.OnComponentAdded(AComponent: TComponent;
  Select: boolean);
begin
  if (AComponent is TAction) then
    // ToDo: only set update flag and do not rebuild everything on every change
    FillCategories;
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

  GlobalDesignHook.AddHandlerComponentDeleting(@OnComponentDeleting);
  GlobalDesignHook.AddHandlerComponentAdded(@OnComponentAdded);
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
  lstCategory.Clear;
  lstCategory.Items.Add(cActionListEditorUnknownCategory);
  lstCategory.Items.Add(cActionListEditorAllCategory);
  if lstCategory.ItemIndex<0 then
     lstCategory.ItemIndex:=0;
     
  sOldCategory:=lstCategory.Items[lstCategory.ItemIndex];

  for i:=0 to FActionList.ActionCount-1 do
  begin
    sCategory:=FActionList.Actions[i].Category;
    if Trim(sCategory)='' then
      Continue;
    xIndex:=lstCategory.Items.IndexOf(sCategory);
    if xIndex<0 then
      lstCategory.Items.Add(sCategory);
  end;
  
  xIndex:=lstCategory.Items.IndexOf(sOldCategory);
  if xIndex<0 then
    xIndex:=0;
  lstCategory.ItemIndex:=xIndex;

  FillActionByCategory(lstCategory.Items[xIndex]);
end;

procedure TActionListEditor.FillActionByCategory(const sCategory: String);
var
  i:Integer;
begin
  lstActionName.Clear;
  
  // handle all (ToDo: fix changing languages)
  if sCategory = cActionListEditorAllCategory then
  begin
    for i:=0 to FActionList.ActionCount-1 do
      lstActionName.Items.Add(FActionList.Actions[i].Name);
    Exit;
  end;

  // handle unknown (ToDo: fix changing languages)
  if sCategory = cActionListEditorUnknownCategory then
  begin
    for i:=0 to FActionList.ActionCount-1 do
    begin
      if trim(FActionList.Actions[i].Category)='' then
        lstActionName.Items.Add(FActionList.Actions[i].Name);
    end;
    Exit;
  end;

  // else sort to categories
  for i:=0 to FActionList.ActionCount-1 do
  begin
    if FActionList.Actions[i].Category = sCategory then
      lstActionName.Items.Add(FActionList.Actions[i].Name);
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
initialization
  RegisterComponentEditor(TActionList,TActionListComponentEditor);
end.

