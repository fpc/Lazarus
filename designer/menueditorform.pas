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
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs, LResources,
  StdCtrls, Buttons, ExtCtrls, LMessages, DesignerMenu, Menus, GraphType,
  ComponentEditors, Designer, LazarusIDEStrConsts, PropEdits;

type
  TMainMenuEditorForm = class(TForm)
  private
    fDesignerMainMenu: TDesignerMainMenu;
    fPanel: TPanel;
    fMenu: TMenu;
    fDesigner: TDesigner;
    fEditor: TComponentEditor;
    List_menus: TListBox;
    Label_menus: TLabel;
    //DesignerPopupMenu: TPopupMenu;
  //  fEditor: TComponentEditor;
    procedure OnComponentDeleting(AComponent: TComponent);
  public
    constructor CreateWithMenu(aOwner: TComponent; aMenu: TMenu; aEditor: TComponentEditor; aDesigner: TDesigner);
    destructor Destroy; override;
    procedure Paint; override;
    procedure SelectMenuClick(Sender: TObject);
    property DesignerMainMenu: TDesignerMainMenu read fDesignerMainMenu write fDesignerMainMenu;
    property Panel: TPanel read FPanel write FPanel;
//    property Editor: TComponentEditor read fEditor write fEditor;
  end;

{ TMenuComponentEditor
  The default component editor for TMenu. }
  TMainMenuComponentEditor = class(TComponentEditor)
  private
    fMenu: TMainMenu;
    fDesigner: TDesigner;
  protected
  public
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    property Menu: TMainMenu read fMenu write fMenu;
  end;

var
  MainMenuEditorForm: TMainMenuEditorForm;

implementation

{ TMainMenuEditorForm }

procedure TMainMenuEditorForm.OnComponentDeleting(AComponent: TComponent);
var
  i: Integer;
begin
  if FindRootDesigner(AComponent)<>fDesigner then exit;
  i:=List_menus.Items.IndexOf(AComponent.Name);
  if i>=0 then List_menus.Items.Delete(i);
end;

constructor TMainMenuEditorForm.CreateWithMenu(aOwner: TComponent; aMenu: TMenu; aEditor: TComponentEditor; aDesigner: TDesigner);
var
  Cmp: TPanel;
  Cmp2: TScrollBox;
  i: Integer;
  CurComponent: TComponent;
begin
  inherited Create(AOwner);
  
  Caption:=lisMenuEditorMenuEditor;
  width:=600;
  height:=220;
  position:=poDesktopCenter;
  
  fMenu:=aMenu;
  fDesigner:=aDesigner;
  fEditor:=aEditor;

  
  DesignerMainMenu:=TDesignerMainMenu.CreateWithMenu(Self, fMenu, fEditor);
  PopupMenu:=DesignerMainMenu.MainPopupMenu;
  
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
  
  for i:=0 to aDesigner.Form.ComponentCount - 1 do
  begin
    CurComponent:=aDesigner.Form.Components[i];
    if (CurComponent is TMainMenu) or (CurComponent is TPopupMenu) then
    begin
      List_menus.Items.Add(CurComponent.Name);
    end;
  end;
  for i:=0 to List_menus.Items.Count - 1 do
    begin
      if (aMenu.Name = List_menus.Items[i]) then
      begin
        List_menus.Selected[i]:=true;
      end;
    end;
  
  Panel:=Cmp;
  
  with DesignerMainMenu do
  begin    
    Parent:=Self;
    ParentCanvas:=Canvas;
    LoadMainMenu;
    SetCoordinates(10,10,0,DesignerMainMenu.Root);
  end;

  GlobalDesignHook.AddHandlerComponentDeleting(@OnComponentDeleting);
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
begin
  DesignerMainMenu.Free;
  for i:=0 to List_menus.Items.Count - 1 do
  begin
    if (List_menus.Selected[i] = true) then
    begin
      for j:=0 to fDesigner.Form.ComponentCount -1 do
      begin
        if (List_menus.Items[i] = fDesigner.Form.Components[j].Name) then
        begin
          writeln(fDesigner.Form.Components[j].Name);
          fMenu:=TMenu(fDesigner.Form.Components[j]);
        end;
      end;
      DesignerMainMenu:=TDesignerMainMenu.CreateWithMenu(Self, fMenu, fEditor);
      with DesignerMainMenu do
      begin
        Parent:=Self;
        ParentCanvas:=Canvas;
        LoadMainMenu;
        SetCoordinates(10,10,0,DesignerMainMenu.Root);
      end;
    end;
  end;
  Invalidate;
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

constructor TMainMenuComponentEditor.Create(AComponent: TComponent; aDesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent,ADesigner);
  fDesigner:=TDesigner(aDesigner);
end;

destructor TMainMenuComponentEditor.Destroy;
begin
  if MainMenuEditorForm.DesignerMainMenu.Editor=Self then begin
    FreeThenNil(MainMenuEditorForm);
  end;
  inherited Destroy;
end;

procedure TMainMenuComponentEditor.Edit;
begin
  //if Menu=nil then RaiseGDBException('TMainMenuComponentEditor.Edit Menu=nil');
  if MainMenuEditorForm=nil then
    MainMenuEditorForm:=TMainMenuEditorForm.CreateWithMenu(Application,
                                          TMenu(GetComponent), Self, fDesigner);
  MainMenuEditorForm.Show;
  //MainMenuEditorForm.Free;
end;

{ //TMainMenuComponentEditor}

initialization
  RegisterComponentEditor(TMainMenu,TMainMenuComponentEditor);
  RegisterComponentEditor(TPopupMenu,TMainMenuComponentEditor);
end.
