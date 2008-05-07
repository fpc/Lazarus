{
 *****************************************************************************
 *                             FpGUIObjects.pas                              *
 *                              --------------                               *
 *      Place for wrapper classes which aren't widgets                       *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit fpguiobjects;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL, LCL
  Classes, SysUtils,
  Graphics, Menus,
  // Widgetset
  fpguiwsprivate,
  // interface
  fpgfx, gui_menu;

type

  { TFpGuiDeviceContext }

  TFpGuiDeviceContext = class(TObject)
  public
    fpgCanvas: TfpgCanvas;
  public
    constructor Create(AfpgCanvas: TfpgCanvas);
  end;

  { TFPGUIPrivateMenuItem }

  TFPGUIPrivateMenuItem = class(TObject)
  private
  protected
  public
    MenuItem: TfpgMenuItem;
    LCLMenuItem: TMenuItem;
  public
    constructor Create(const AMenuItem: TMenuItem); virtual;
    destructor  Destroy; override;
  end;

implementation

{ TFpGuiDeviceContext }

constructor TFpGuiDeviceContext.Create(AfpgCanvas: TfpgCanvas);
begin
  fpgCanvas := AfpgCanvas;
end;

{ TFPGUIPrivateMenuItem }

constructor TFPGUIPrivateMenuItem.Create(const AMenuItem: TMenuItem);
var
  AMenuName, hotkeydef: string;
  { Possible parents }
{  ParentPrivateItem: TFPGUIPrivateMenuItem;
  ParentPrivateMenu: TFPGUIPrivateMenuBar;
  ParentPrivatePopUp: TFPGUIPrivatePopUpMenu;}
begin
  LCLMenuItem := AMenuItem;
  
  { Tryes to identify the parent and do an adequate creation }
{  if Assigned(LCLMenuItem.Parent) then
  begin
    if (LCLMenuItem.Parent is TMenuItem) then
    begin
      ParentPrivateItem := TFPGUIPrivateMenuItem(LCLMenuItem.Parent.Handle);

      MenuItem := TfpgMenuItem.Create(nil);
    end
    else if LCLMenuItem.Owner is TMenu then
    begin
      ParentPrivateMenu := TFPGUIPrivateMenuBar(LCLMenuItem.Parent.Handle);

      MenuItem := ParentPrivateMenu.MenuBar.AddMenuItem(AMenuName, nil);
    end
    else if LCLMenuItem.Owner is TPopUpMenu then
    begin
      ParentPrivatePopUp := TFPGUIPrivatePopUpMenu(LCLMenuItem.Parent.Handle);

      MenuItem := ParentPrivatePopUp.PopUpMenu.AddMenuItem(AMenuName, hotkeydef, nil);
    end
    else
      raise Exception.Create('Unable to detect the class of the menu parent');
  end
  else
  begin
    MenuItem := TfpgMenuItem.Create(nil);
  end;}
end;

destructor TFPGUIPrivateMenuItem.Destroy;
begin
//  MenuItem.Free;

  inherited Destroy;
end;

end.

