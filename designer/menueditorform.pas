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
    fDesignerMainMenu: TDesignerMainMenu;
    fPanel: TPanel;
  //  fEditor: TComponentEditor;
  public
    constructor CreateWithMenu(aOwner: TComponent; aMenu: TMenu; aEditor: TComponentEditor);
    destructor Destroy; override;
    procedure Paint; override;
    
    property DesignerMainMenu: TDesignerMainMenu read fDesignerMainMenu write fDesignerMainMenu;
    property Panel: TPanel read FPanel write FPanel;
//    property Editor: TComponentEditor read fEditor write fEditor;
  end;

{ TMenuComponentEditor
  The default component editor for TMenu. }
  TMainMenuComponentEditor = class(TComponentEditor)
  private
    fMenu: TMainMenu;
  protected
  public
    constructor Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    property Menu: TMainMenu read fMenu write fMenu;
    
//    function Menu: TMenu;
  end;

implementation

{ TMainMenuEditorForm }

constructor TMainMenuEditorForm.CreateWithMenu(aOwner: TComponent; aMenu: TMenu; aEditor: TComponentEditor);
var
  Cmp: TPanel;
begin
  inherited Create(AOwner);
  
  width:=400;
  height:=200;
  position:=poDesktopCenter;
  
  Cmp:=TPanel.Create(self);
  with Cmp do
  begin
   Parent:=self;
   Align:=alClient;
   Bevelouter:=bvnone;
   Bevelwidth:=0;
  end;
  
  Panel:=Cmp;

  DesignerMainMenu:=TDesignerMainMenu.CreateWithMenu(Self,aMenu,aEditor);
  with DesignerMainMenu do
  begin    
    Parent:=Self;
    ParentCanvas:=Canvas;
    LoadMainMenu;
    SetCoordinates(1,1,0,DesignerMainMenu.Root);
  end;
end;

destructor TMainMenuEditorForm.Destroy;
begin
  inherited Destroy;
end;

procedure TMainMenuEditorForm.Paint;
begin
  inherited Paint;
  DesignerMainMenu.Draw(DesignerMainMenu.Root, Panel, Panel);
end;

{ TMainMenuComponentEditor}

constructor TMainMenuComponentEditor.Create(AComponent: TComponent; ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent,ADesigner);
end;

procedure TMainMenuComponentEditor.Edit;
var
  MainMenuEditorForm: TMainMenuEditorForm;
begin
  //if Menu=nil then RaiseGDBException('TMainMenuComponentEditor.Edit Menu=nil');
  MainMenuEditorForm:=TMainMenuEditorForm.CreateWithMenu(Application,TMenu(GetComponent),Self);
  MainMenuEditorForm.Show;
  //MainMenuEditorForm.Free;
end;

{ //TMainMenuComponentEditor}

initialization
  RegisterComponentEditor(TMainMenu,TMainMenuComponentEditor);
  RegisterComponentEditor(TPopupMenu,TMainMenuComponentEditor);
end.
