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
  ComponentEditors;

type

  TMainMenuEditorForm = class(TForm)
  private
    FDesignerMainMenu: TDesignerMainMenu;
  public
    constructor CreateWithMenu(TheOwner: TComponent; AMenu: TMainMenu);
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseDownClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X,Y: Integer);
    property DesignerMainMenu: TDesignerMainMenu read FDesignerMainMenu
                                                 write FDesignerMainMenu;
  end;

{ TMenuComponentEditor
  The default component editor for TMenu. }
  TMainMenuComponentEditor = class(TDefaultComponentEditor)
  private
    FMenu: TMainMenu;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    property Menu: TMainMenu read FMenu write FMenu;
  end;

implementation

{ TMainMenuEditorForm }

constructor TMainMenuEditorForm.CreateWithMenu(TheOwner: TComponent;
  AMenu: TMainMenu);
begin
  inherited Create(TheOwner);
  width:=800;
  height:=600;
  position:=poDesktopCenter;
  OnMouseDown:=@MouseDownClick;
  
  DesignerMainMenu:=TDesignerMainMenu.CreateWithMenu(Self,AMenu);
  with DesignerMainMenu do
  begin    
    Parent:=Self;
    LoadMainMenu;
    SetCoordinates(1,1,DesignerMainMenu.Root);
  end;
end;

destructor TMainMenuEditorForm.Destroy;
begin
  inherited Destroy;
end;

procedure TMainMenuEditorForm.Paint;
begin
  inherited Paint;
  DesignerMainMenu.Erase(DesignerMainmenu.Root,Canvas);
  DesignerMainMenu.Draw(DesignerMainMenu.Root,Canvas);
end;

procedure TMainMenuEditorForm.MouseDownClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X,Y: Integer);
begin
  DesignerMainMenu.MouseDownClick(DesignerMainMenu.Root,X,Y);
  Paint;
end;

{ TMainMenuComponentEditor}

constructor TMainMenuComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
var
  m1: TMenuItem;
  m2: TMenuItem;
  m3: TMenuItem;
  m4: TMenuItem;
  m5: TMenuItem;
  m6: TMenuItem;
  m7: TMenuItem;
  m8: TMenuItem;
begin
  inherited Create(AComponent,ADesigner);
  Menu:=TMainMenu.Create(AComponent);
  
  m1:=TMenuItem.Create(AComponent);
  m1.Caption:='File';
  Menu.Items.Add(m1);
  
  m2:=TMenuItem.Create(AComponent);
  m2.Caption:='Power';
  Menu.Items.Add(m2);
  
  m3:=TMenuItem.Create(AComponent);
  m3.Caption:='Settings';
  Menu.Items.Add(m3);
  
  m4:=TMenuItem.Create(AComponent);
  m4.Caption:='New';
  m1.Add(m4);
  
  m5:=TMenuItem.Create(AComponent);
  m5.Caption:='Wizard';
  m1.Add(m5);
  
  m6:=TMenuItem.Create(AComponent);
  m6.Caption:='Project';
  m5.Add(m6);
  
  m7:=TMenuItem.Create(AComponent);
  m7.Caption:='Power On';
  m2.Add(m7);
  
  m8:=TMenuItem.Create(AComponent);
  m8.Caption:='Another Caption';
  m6.Add(m8);
end;

procedure TMainMenuComponentEditor.Edit;
var
  MainMenuEditorForm: TMainMenuEditorForm;
begin
  if Menu=nil then RaiseGDBException('TMainMenuComponentEditor.Edit Menu=nil');
  MainMenuEditorForm:=TMainMenuEditorForm.CreateWithMenu(Application,Menu);
  MainMenuEditorForm.ShowModal;
  MainMenuEditorForm.Free;
end;
{ //TMainMenuComponentEditor}

initialization
  RegisterComponentEditor(TMainMenu,TMainMenuComponentEditor);

end.
