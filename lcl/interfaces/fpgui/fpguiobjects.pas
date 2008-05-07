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
  // RTL, FCL
  Classes, SysUtils,
  // LCL
  Graphics, Menus,
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
begin
  MenuItem := TfpgMenuItem.Create(nil)
end;

destructor TFPGUIPrivateMenuItem.Destroy;
begin
  MenuItem.Free;

  inherited Destroy;
end;

end.

