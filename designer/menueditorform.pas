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
  LResources, StdCtrls, Buttons, ExtCtrls, DesignerMenu, Menus, GraphType,
  ComponentEditors, LazarusIDEStrConsts, PropEdits;

type
  TMainMenuEditorForm = class(TForm)
  private
    fDesignerMainMenu: TDesignerMainMenu;
    fPanel: TPanel;
    fMenu: TMenu;
    fDesigner: TComponentEditorDesigner;
    List_menus: TListBox;
    Label_menus: TLabel;
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);
    procedure CreateDesignerMenu;
    procedure UpdateListOfMenus;
  public
    constructor CreateWithMenu(aOwner: TComponent; aMenu: TMenu);
    destructor Destroy; override;
    procedure Paint; override;
    procedure SelectMenuClick(Sender: TObject);
    procedure SetMenu(NewMenu: TMenu);
    property DesignerMainMenu: TDesignerMainMenu read fDesignerMainMenu
                                                 write fDesignerMainMenu;
    property Panel: TPanel read FPanel write FPanel;
  end;

{ TMenuComponentEditor
  The default component editor for TMenu. }
  TMainMenuComponentEditor = class(TComponentEditor)
  private
    fMenu: TMainMenu;
    fDesigner: TComponentEditorDesigner;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    property Menu: TMainMenu read fMenu write fMenu;
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

procedure ShowMenuEditor(AMenu: TMenu);
begin
  if AMenu=nil then RaiseGDBException('ShowMenuEditor AMenu=nil');
  if MainMenuEditorForm=nil then begin
    MainMenuEditorForm:=TMainMenuEditorForm.CreateWithMenu(Application,AMenu);
  end;
  MainMenuEditorForm.SetMenu(AMenu);
  MainMenuEditorForm.ShowOnTop;
end;

{ TMainMenuEditorForm }

procedure TMainMenuEditorForm.OnPersistentDeleting(APersistent: TPersistent);
var
  i: Integer;
  AComponent: TComponent;
begin
  if APersistent is TComponent then begin
    AComponent:=TComponent(APersistent);
    if FindRootDesigner(AComponent)<>fDesigner then exit;
    i:=List_menus.Items.IndexOf(AComponent.Name);
    if i>=0 then List_menus.Items.Delete(i);
  end;
end;

procedure TMainMenuEditorForm.OnPersistentAdded(APersistent: TPersistent;
  Select: boolean);
begin
  debugln('TMainMenuEditorForm.OnPersistentAdded ',dbgsName(APersistent));
  if APersistent is TMenu then
    UpdateListOfMenus;
end;

procedure TMainMenuEditorForm.CreateDesignerMenu;
begin
  DesignerMainMenu:=TDesignerMainMenu.CreateWithMenu(Self, fMenu);
  with DesignerMainMenu do
  begin
    Parent:=Self;
    ParentCanvas:=Canvas;
    LoadMainMenu;
    SetCoordinates(10,10,0,DesignerMainMenu.Root);
  end;

  Invalidate;
end;

procedure TMainMenuEditorForm.UpdateListOfMenus;
var
  i: Integer;
  CurComponent: TComponent;
begin
  List_menus.Items.BeginUpdate;
  List_menus.Items.Clear;
  for i:=0 to fDesigner.Form.ComponentCount - 1 do
  begin
    CurComponent:=fDesigner.Form.Components[i];
    debugln('TMainMenuEditorForm.UpdateListOfMenus A ',dbgsName(CurComponent));
    if (CurComponent is TMainMenu) or (CurComponent is TPopupMenu) then
    begin
      List_menus.Items.Add(CurComponent.Name);
    end;
  end;
  List_menus.Items.EndUpdate;

  if fMenu<>nil then begin
    for i:=0 to List_menus.Items.Count - 1 do
      begin
        if (fMenu.Name = List_menus.Items[i]) then
        begin
          List_menus.Selected[i]:=true;
        end;
      end;
  end;
end;

constructor TMainMenuEditorForm.CreateWithMenu(aOwner: TComponent;
  aMenu: TMenu);
var
  Cmp: TPanel;
  Cmp2: TScrollBox;
begin
  inherited Create(AOwner);
  
  Caption:=lisMenuEditorMenuEditor;
  width:=600;
  height:=220;
  position:=poScreenCenter;
  
  Cmp2:=TScrollBox.Create(self);
  with Cmp2 do
  begin
    Parent:=self;
    Left:=0;
    Top:=0;
    Width:=400;
    Height:=Parent.Height;
    Autoscroll:=true;
    Anchors := [aktop, akleft, akright,akbottom];
  end;
  
  Cmp:=TPanel.Create(self);
  with Cmp do
  begin
   Parent:=Cmp2;
   Left:=0;
   Top:=0;
   Width:=200;
   Height:=Parent.Height;
   Bevelouter:=bvnone;
  end;
  
  Label_menus:=TLabel.Create(self);
  with Label_menus do
  begin
    Parent:=self;
    Left:=410;
    Top:=10;
    Width:=180;
    Height:=20;
    Text:=lisMenuEditorSelectMenu;
    Anchors := [aktop, akright];
  end;
  
  List_menus:=TListBox.Create(self);
  with List_menus do
  begin
    Parent:=self;
    Left:=410;
    Top:=30;
    Width:=180;
    Height:=180;
    OnClick:=@SelectMenuClick;
    Anchors := [akright, aktop, akbottom];
  end;
  
  Panel:=Cmp;

  SetMenu(aMenu);

  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
end;

destructor TMainMenuEditorForm.Destroy;
begin
  if GlobalDesignHook<>nil then
    GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TMainMenuEditorForm.SelectMenuClick(Sender: TObject);
var
  i,j: Integer;
  NewMenu: TMenu;
  CurComponent: TComponent;
begin
  for i:=0 to List_menus.Items.Count - 1 do
  begin
    if (List_menus.Selected[i] = true) then
    begin
      for j:=0 to fDesigner.Form.ComponentCount -1 do
      begin
        CurComponent:=fDesigner.Form.Components[j];
        if (List_menus.Items[i] = CurComponent.Name) and (CurComponent is TMenu)
        then begin
          NewMenu:=TMenu(CurComponent);
          SetMenu(NewMenu);
          exit;
        end;
      end;
    end;
  end;
end;

procedure TMainMenuEditorForm.SetMenu(NewMenu: TMenu);
begin
  if NewMenu <> fMenu then
  begin
    DesignerMainMenu.Free;
    DesignerMainMenu := nil;
    fMenu := NewMenu;
    fDesigner := FindRootDesigner(fMenu) as TComponentEditorDesigner;
    UpdateListOfMenus;
    CreateDesignerMenu;
  end;
end;

procedure TMainMenuEditorForm.Paint;
var
  temp_coord: TRect;
begin
  inherited Paint;
  temp_coord:=DesignerMainMenu.GetMaxCoordinates(DesignerMainMenu.Root, 0, 0);
  Panel.Width:=temp_coord.Right + 10;
  Panel.Height:=temp_coord.Bottom + 10;
  //writeln('Panel Width: ', Panel.width, ' Panel Height: ', Panel.Height);
  DesignerMainMenu.Draw(DesignerMainMenu.Root, Panel, Panel);
end;

{ TMainMenuComponentEditor}

constructor TMainMenuComponentEditor.Create(AComponent: TComponent;
  aDesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent,ADesigner);
  fDesigner:=aDesigner;
end;

destructor TMainMenuComponentEditor.Destroy;
begin
  if (MainMenuEditorForm <> nil) and (MainMenuEditorForm.DesignerMainMenu <> nil)
      and (MainMenuEditorForm.DesignerMainMenu.Menu=Component) then 
    FreeThenNil(MainMenuEditorForm);

  inherited Destroy;
end;

procedure TMainMenuComponentEditor.Edit;
begin
  ShowMenuEditor(Component as TMenu);
end;

function TMainMenuComponentEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

function TMainMenuComponentEditor.GetVerb(Index: Integer): string;
begin
  Result:='Edit';
end;

procedure TMainMenuComponentEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
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
