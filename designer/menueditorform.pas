{  $Id$  }
{
 /***************************************************************************
                            menueditorform.pas
                            ------------------


 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Martin Patik

}
unit MenuEditorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, LCLProc, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DesignerMenu, Menus, GraphType,
  ComponentEditors, LazarusIDEStrConsts, PropEdits;

type

  { TMainMenuEditorForm }

  TMainMenuEditorForm = class(TForm)
    List_menus: TListBox;
    Label_menus: TLabel;
    MenuScrollBox: TScrollBox;
    Panel: TPanel;
    Panel_MenuList:TPanel;
    Splitter_BoxPanel:TSplitter;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure List_menusClick(Sender: TObject);
  private
    FDesignerMainMenu: TDesignerMainMenu;
    FMenu: TMenu;
    FDesigner: TComponentEditorDesigner;
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure CreateDesignerMenu;
    procedure UpdateListOfMenus;
  public
    procedure SetMenu(NewMenu: TMenu);
    property DesignerMainMenu: TDesignerMainMenu read FDesignerMainMenu
                                                 write FDesignerMainMenu;
  end;

{ TMenuComponentEditor
  The default component editor for TMenu. }
  TMainMenuComponentEditor = class(TComponentEditor)
  private
    FDesigner: TComponentEditorDesigner;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;


{ TMenuItemsPropertyEditor
  PropertyEditor editor for the TMenu.Items properties.
  Brings up the menu editor. }

  TMenuItemsPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


var
  MainMenuEditorForm: TMainMenuEditorForm;

procedure ShowMenuEditor(AMenu: TMenu);


implementation

{$R *.lfm}

procedure ShowMenuEditor(AMenu: TMenu);
begin
  if AMenu=nil then RaiseGDBException('ShowMenuEditor AMenu=nil');
  if MainMenuEditorForm=nil then
    MainMenuEditorForm:=TMainMenuEditorForm.Create(Application);
  MainMenuEditorForm.SetMenu(AMenu);
  MainMenuEditorForm.ShowOnTop;
end;

{ TMainMenuEditorForm }

procedure TMainMenuEditorForm.FormCreate(Sender: TObject);
begin
  Caption:=lisMenuEditorMenuEditor;
  Panel.Height:=Panel.Parent.Height;
  Label_menus.Caption:=lisMenuEditorSelectMenu;

  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
end;

procedure TMainMenuEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SetMenu(nil);
end;

procedure TMainMenuEditorForm.FormDestroy(Sender: TObject);
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
end;

procedure TMainMenuEditorForm.List_menusClick(Sender: TObject);
var
  i, j: Integer;
  CurComponent: TComponent;
begin
  for i := 0 to List_menus.Items.Count - 1 do
  begin
    if List_menus.Selected[i] then
    begin
      for j := 0 to FDesigner.Form.ComponentCount - 1 do
      begin
        CurComponent:=FDesigner.Form.Components[j];
        if (List_menus.Items[i] = CurComponent.Name) and (CurComponent is TMenu) then 
        begin
          SetMenu(TMenu(CurComponent));
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TMainMenuEditorForm.OnPersistentDeleting(APersistent: TPersistent);
var
  i: Integer;
  AComponent: TComponent;
begin
  if APersistent is TComponent then 
  begin
    AComponent := TComponent(APersistent);
    if FindRootDesigner(AComponent) <> FDesigner then Exit;
    i := List_menus.Items.IndexOf(AComponent.Name);
    if i >= 0 then List_menus.Items.Delete(i);
    
    if AComponent = FMenu then SetMenu(nil);
  end;
end;

procedure TMainMenuEditorForm.OnPersistentAdded(APersistent: TPersistent;
  Select: boolean);
begin
  //debugln('TMainMenuEditorForm.OnPersistentAdded ',dbgsName(APersistent));
  if APersistent is TMenu then
    UpdateListOfMenus;
end;

procedure TMainMenuEditorForm.CreateDesignerMenu;
begin
  DesignerMainMenu := TDesignerMainMenu.CreateWithMenu(Self, FMenu);
  with DesignerMainMenu do
  begin
    Parent := Self;
    ParentCanvas := Canvas;
    LoadMainMenu;
    SetCoordinates(10, 10, 0, DesignerMainMenu.Root);
  end;
  DesignerMainMenu.Panel := Panel;
end;

procedure TMainMenuEditorForm.UpdateListOfMenus;
var
  i: Integer;
  CurComponent: TComponent;
begin
  List_menus.Items.BeginUpdate;
  List_menus.Items.Clear;
  if FDesigner <> nil then
  begin
    for i := 0 to FDesigner.Form.ComponentCount - 1 do
    begin
      CurComponent:=FDesigner.Form.Components[i];
      //debugln('TMainMenuEditorForm.UpdateListOfMenus A ',dbgsName(CurComponent));
      if (CurComponent is TMainMenu) or (CurComponent is TPopupMenu) then
      begin
        List_menus.Items.Add(CurComponent.Name);
      end;
    end;
  end;
  List_menus.Items.EndUpdate;

  if FMenu <> nil then 
  begin
    for i := 0 to List_menus.Items.Count - 1 do
      begin
        if (FMenu.Name = List_menus.Items[i]) then
        begin
          List_menus.Selected[i] := True;
        end;
      end;
  end;
end;

procedure TMainMenuEditorForm.SetMenu(NewMenu: TMenu);
begin
  if NewMenu <> FMenu then
  begin
    DesignerMainMenu.Free;
    DesignerMainMenu := nil;
    FMenu := NewMenu;
    FDesigner := FindRootDesigner(FMenu) as TComponentEditorDesigner;
    UpdateListOfMenus;
    if FMenu <> nil then
    begin
      CreateDesignerMenu;
      DesignerMainMenu.RealignDesigner;
    end;
  end;
end;

{ TMainMenuComponentEditor}

constructor TMainMenuComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner := ADesigner;
end;

procedure TMainMenuComponentEditor.Edit;
begin
  ShowMenuEditor(Component as TMenu);
end;

function TMainMenuComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TMainMenuComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := lisMenuEditor;
  end;
end;

procedure TMainMenuComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

{ TMenuItemsPropertyEditor }

procedure TMenuItemsPropertyEditor.Edit;
var
  Menu: TMenu;
  MenuItem: TMenuItem;
begin
  MenuItem := TMenuItem(GetObjectValue(TMenuItem));
  if MenuItem = nil then exit;
  Menu := MenuItem.GetParentMenu;
  if Menu = nil then exit;
  ShowMenuEditor(Menu);
end;

function TMenuItemsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paReadOnly];
end;

//=============================================================================

procedure InitMenuEditorGlobals;
begin
  RegisterComponentEditor(TMenu,TMainMenuComponentEditor);

  RegisterPropertyEditor(GetPropInfo(TMenu,'Items')^.PropType,
    TMenu,'',TMenuItemsPropertyEditor);
end;

initialization
  InitMenuEditorGlobals;

end.
