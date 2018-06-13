{ $Id$}
{
 *****************************************************************************
 *                                WSMenus.pp                                 * 
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSMenus;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Menus, Graphics,
////////////////////////////////////////////////////
  WSLCLClasses, LCLType, WSFactory,
  LazUtilities, LazLogger;

type
  { TWSMenuItem }

  TWSMenuItem = class(TWSLCLComponent)
  published
    class function  OpenCommand: LongInt; virtual;
    class procedure CloseCommand(ACommand: LongInt); virtual;
    class procedure AttachMenu(const AMenuItem: TMenuItem); virtual;
    class function  CreateHandle(const AMenuItem: TMenuItem): HMENU; virtual;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); virtual;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); virtual;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); virtual;
    class procedure SetVisible(const AMenuItem: TMenuItem; const Visible: boolean); virtual;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; virtual;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; virtual;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; virtual;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; virtual;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap); virtual;
  end;
  TWSMenuItemClass = class of TWSMenuItem;

  { TWSMenu }

  TWSMenuClass = class of TWSMenu;
  TWSMenu = class(TWSLCLComponent)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; virtual;
    
    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); virtual;
  end;

  { TWSMainMenu }

  TWSMainMenu = class(TWSMenu)
  published
  end;

  { TWSPopupMenu }

  TWSPopupMenu = class(TWSMenu)
  published
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); virtual;
  end;
  TWSPopupMenuClass = class of TWSPopupMenu;

function WSCheckMenuItem(const AMenuItem: TMenuItem;
  const AProcName: String): Boolean;

  { WidgetSetRegistration }

  procedure RegisterMenuItem;
  procedure RegisterMenu;
  procedure RegisterMainMenu;
  procedure RegisterPopupMenu;

implementation

{ Menu command management }

var
  CommandPool: TBits = nil;

function UniqueCommand: LongInt;
begin
  if CommandPool = nil then
    CommandPool := TBits.Create(16);
  Result := CommandPool.OpenBit;
  CommandPool[Result] := True;
end;

{ TWSMenuItem }

class function TWSMenuItem.OpenCommand: LongInt;
begin
  Result := UniqueCommand;
end;

class procedure TWSMenuItem.CloseCommand(ACommand: LongInt);
begin
  CommandPool[ACommand] := False;
end;

class procedure TWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
end;

class function  TWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := 0;
end;

class procedure TWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
end;

class procedure TWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
end;

class procedure TWSMenuItem.SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut);
begin
end;

class procedure TWSMenuItem.SetVisible(const AMenuItem: TMenuItem; const Visible: boolean);
begin
end;

class function TWSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
begin
  Result := false;
end;

class function TWSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
begin
  Result := false;
end;

class function TWSMenuItem.SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean;
begin
  Result := false;
end;

class function TWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  Result := false;
end;

class procedure TWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: TBitmap);
begin
  // emulate old behaviour
  AMenuItem.RecreateHandle;
end;


          
{ TWSMenu }

class function  TWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := 0;
end;

class procedure TWSMenu.SetBiDiMode(const AMenu : TMenu; UseRightToLeftAlign,
  UseRightToLeftReading : Boolean);
begin
end;


{ TWSPopupMenu }

class procedure TWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
begin
end;

function WSCheckMenuItem(const AMenuItem: TMenuItem;
  const AProcName: String): Boolean;

  procedure Warn;
  begin
    DebugLn('[WARNING] %s called without handle for %s(%s)', [AProcName, AMenuItem.Name, AMenuItem.ClassName]);
  end;
begin
  Result := AMenuItem.HandleAllocated;
  if Result then Exit;
  Warn;
end;

{ WidgetSetRegistration }

procedure RegisterMenuItem;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMenuItem;
//  if not WSRegisterMenuItem then
//    RegisterWSComponent(TMenuItem, TWSMenuItem);
  Done := True;
end;

procedure RegisterMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMenu;
//  if not WSRegisterMenu then
//    RegisterWSComponent(TMenu, TWSMenu);
  Done := True;
end;

procedure RegisterMainMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterMainMenu;
//  if not WSRegisterMainMenu then
//    RegisterWSComponent(TMainMenu, TWSMainMenu);
  Done := True;
end;

procedure RegisterPopupMenu;
const
  Done: Boolean = False;
begin
  if Done then exit;
  WSRegisterPopupMenu;
//  if not WSRegisterPopupMenu then
//    RegisterWSComponent(TPopupMenu, TWSPopupMenu);
  Done := True;
end;

finalization
  FreeThenNil(CommandPool);
end.
