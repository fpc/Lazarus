{
 *****************************************************************************
 *                            CustomDrawnWSMenus.pp                          *
 *                               ------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    * 
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CustomDrawnWSMenus;

{$mode objfpc}{$H+}

interface

//{$I qtdefines.inc}

uses
  // Bindings
  // LCL
  SysUtils, Classes, Types, LCLType, LCLProc, Graphics, Controls, Forms, Menus,
  // Widgetset
  WSMenus, WSLCLClasses;

type

  { TCDWSMenuItem }

  TCDWSMenuItem = class(TWSMenuItem)
  published
//    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
{    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); override;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); override;}
  end;

  { TCDWSMenu }

  TCDWSMenu = class(TWSMenu)
  published
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
{    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); override;}
  end;

  { TCDWSMainMenu }

  TCDWSMainMenu = class(TWSMainMenu)
  published
  end;

  { TCDWSPopupMenu }

  TCDWSPopupMenu = class(TWSPopupMenu)
  published
//    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

{ TCDWSMenuItem }

class function TCDWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  // Fill a dummy value to get a positive result for HandleAllocated
  Result := $FFFFFF;
end;

class procedure TCDWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin

end;

{ TCDWSMenu }

class function TCDWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  // Fill a dummy value to get a positive result for HandleAllocated
  Result := $FFFFFF;
end;

end.
