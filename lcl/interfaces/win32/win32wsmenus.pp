{ $Id$}
{
 *****************************************************************************
 *                              Win32WSMenus.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Win32WSMenus;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Menus, Forms,
////////////////////////////////////////////////////
  WSMenus, WSLCLClasses,
  Windows, Controls, Classes, SysUtils, Win32Int, Win32Proc, InterfaceBase, LCLProc;

type

  { TWin32WSMenuItem }

  TWin32WSMenuItem = class(TWSMenuItem)
  private
  protected
  public
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function  CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRadioItem(const AMenuItem: TMenuItem; const RadioItem: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override; 
  end;

  { TWin32WSMenu }

  TWin32WSMenu = class(TWSMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TWin32WSMainMenu }

  TWin32WSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TWin32WSPopupMenu }

  TWin32WSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;


implementation

{ helper routines }

procedure TriggerFormUpdate(const AMenuItem: TMenuItem);
var
  lMenu: TMenu;
begin
  lMenu := AMenuItem.GetParentMenu;
  if (lMenu<>nil) and (lMenu.Parent<>nil)
  and (lMenu.Parent is TCustomForm)
  and TCustomForm(lMenu.Parent).HandleAllocated 
  and not (csDestroying in lMenu.Parent.ComponentState) then
    AddToChangedMenus(TCustomForm(lMenu.Parent).Handle);
end;

function ChangeMenuFlag(const AMenuItem: TMenuItem; Flag: Integer; Value: boolean): boolean;
var
  MenuInfo: MENUITEMINFO;
begin
  MenuInfo.cbSize := sizeof(MenuInfo);
  MenuInfo.fMask := MIIM_TYPE;
  MenuInfo.dwTypeData := nil;  // don't retrieve caption
  GetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  if Value then
    MenuInfo.fType := MenuInfo.fType or Flag
  else
    MenuInfo.fType := MenuInfo.fType and (not Flag);
  MenuInfo.dwTypeData := LPSTR(AMenuItem.Caption);
  Result := SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  TriggerFormUpdate(AMenuItem);
end;

{ TWin32WSMenuItem }

procedure UpdateCaption(const AMenuItem: TMenuItem; ACaption: String);
var 
  MenuInfo: MENUITEMINFO;
begin
  with MenuInfo do
  begin
    cbsize:=sizeof(MENUITEMINFO);
    if ACaption <> '-' then
    begin
      fType := MFT_STRING;
      {In Win32 Menu items that are created without a initial caption default to disabled,
       the next three lines are to counter that.}
      fMask:=MIIM_STATE;
      GetMenuItemInfo(AMenuItem.Parent.Handle,
                      AMenuItem.Command, false, @MenuInfo);
      if AMenuItem.Enabled then
        fState := fState and DWORD(not (MFS_DISABLED or MFS_GRAYED));

      fMask:=MIIM_TYPE or MIIM_STATE;
      dwTypeData:=LPSTR(ACaption);
      cch := StrLen(dwTypeData);
    end
    else fType := MFT_SEPARATOR;
  end;
  SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  TriggerFormUpdate(AMenuItem);
end;

procedure TWin32WSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var 
  MenuInfo: MENUITEMINFO;
  ParentMenuHandle: HMenu;
  ParentOfParent: HMenu;

  function GetCheckBitmap(checked: boolean): HBitmap;
  {TODO: create "checked" icon}
  var 
    hbmpCheck, hbmpTrans, hbmpMask: HBITMAP;
    rectBitmap: Windows.RECT;
    hbrTrans: HBRUSH;
    OldCheckMark, OldOrigBitmap, OldTransBitmap: HBITMAP;
    hdcNewBitmap, hdcOrigBitmap, hdcTransBitmap: HDC;
    hdcScreen: HDC;
    maxWidth, newWidth, bmpWidth: integer;
    maxHeight, newHeight, bmpHeight: integer;
  begin
    maxWidth:=GetSystemMetrics(SM_CXMENUCHECK);
    maxHeight:=GetSystemMetrics(SM_CYMENUCHECK);
    if (maxWidth>=AMenuItem.Bitmap.Width) and (maxHeight>=AMenuItem.Bitmap.Height) then Result:=AMenuItem.Bitmap.Handle
    else
    begin
      bmpWidth := AMenuItem.Bitmap.Width;
      bmpHeight := AMenuItem.Bitmap.Height;
      newWidth := min(maxWidth, bmpWidth);
      newHeight := min(maxHeight, bmpHeight);
      hdcScreen := GetDC(GetDesktopWindow);
      hdcOrigBitmap  := CreateCompatibleDC(hdcScreen);
      hdcNewBitmap   := CreateCompatibleDC(hdcScreen);
      hdcTransBitmap := CreateCompatibleDC(hdcScreen);
      hbmpCheck := CreateCompatibleBitmap(hdcScreen, newWidth, newHeight);
      hbmpTrans := CreateCompatibleBitmap(hdcScreen, bmpWidth, bmpHeight);
      hbmpMask  := AMenuItem.Bitmap.MaskHandle;
      ReleaseDC(GetDesktopWindow, hdcScreen);
      hbrTrans := CreateSolidBrush(GetSysColor(COLOR_MENU));
      OldOrigBitmap  := SelectObject(hdcOrigBitmap, AMenuItem.Bitmap.Handle);
      OldCheckmark   := SelectObject(hdcNewBitmap, hbmpCheck);
      OldTransBitmap := SelectObject(hdcTransBitmap, hbmpTrans);
      // fill transparent-bitmap with transparent color
      rectBitmap := RECT(0, 0, bmpWidth, bmpHeight);
      FillRect(hdcTransBitmap, rectBitmap, hbrTrans);
      // blit menu icon transparently
      TWin32WidgetSet(WidgetSet).MaskBlt(hdcTransBitmap, 0, 0, bmpWidth, 
        bmpHeight, hdcOrigBitmap, 0, 0, hbmpMask, 0, 0);
      // scale to correct size
      StretchBlt(hdcNewBitmap, 0, 0, newWidth, newHeight, hdcTransBitmap, 0, 0, bmpWidth, bmpHeight, SRCCOPY);
      // free mem
      SelectObject(hdcOrigBitmap, OldOrigBitmap);
      SelectObject(hdcTransBitmap, OldTransBitmap);
      SelectObject(hdcNewBitmap, OldCheckmark);
      DeleteDC(hdcOrigBitmap);
      DeleteDC(hdcTransBitmap);
      DeleteDC(hdcNewBitmap);
      DeleteObject(hbmpTrans);
      DeleteObject(hbrTrans);
      {TODO: Add hbmpCheck into a list of object they must be deleted}
      Result := hbmpCheck;
    end;
  end;

var
  newCaption: string;
begin
  ParentMenuHandle := AMenuItem.Parent.Handle;

  {Following part fixes the case when an item is added in runtime
  but the parent item has not defined the submenu flag (hSubmenu=0) }
  if AMenuItem.Parent.Parent<>nil then
  begin
    ParentOfParent := AMenuItem.Parent.Parent.Handle;
    with MenuInfo do begin
      cbSize:=sizeof(MENUITEMINFO);
      fMask:=MIIM_SUBMENU;
    end;
    GetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command,
                    false, @MenuInfo);
    if MenuInfo.hSubmenu=0 then // the parent menu item is not yet defined with submenu flag
    begin
      MenuInfo.hSubmenu:=ParentMenuHandle;
      SetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command,
                      false, MenuInfo);
    end;
  end;

  with MenuInfo do begin
    cbsize:=sizeof(MENUITEMINFO);
    if AMenuItem.Enabled then fState:=MFS_ENABLED else fstate:=MFS_GRAYED;
    if AMenuItem.Checked then fState:=fState or MFS_CHECKED;
    fMask:=MIIM_ID or MIIM_DATA or MIIM_STATE or MIIM_TYPE;
    wID:=AMenuItem.Command; {value may only be 16 bit wide!}
    dwItemData:=PtrInt(AMenuItem);
    // Note: can't use "and MFT_STRING", because MFT_STRING is zero :-)
    if (AMenuItem.Count > 0) then 
    begin
      fMask := fMask or MIIM_SUBMENU;
      hSubMenu := AMenuItem.Handle;
    end else
      hSubMenu := 0;
    if AMenuItem.Caption <> '-' then
    begin
      fType:=MFT_STRING;
      newCaption:=AMenuItem.Caption;
      if AMenuItem.ShortCut <> scNone then
        newCaption:=newCaption+#9+ShortCutToText(AMenuItem.ShortCut);
      dwTypeData:=LPSTR(newCaption);
      cch:=Length(newCaption);
    end else begin
      fType:=MFT_SEPARATOR;
      dwTypeData:=nil;
      cch:=0;
    end;
    if AMenuItem.RadioItem then fType := fType or MFT_RADIOCHECK;
    if AMenuItem.RightJustify then fType := fType or MFT_RIGHTJUSTIFY;
    if AmenuItem.HasIcon then {adds the menuitem icon}
    begin
      fMask:=fMask or MIIM_CHECKMARKS;
      hbmpUnchecked:=GetCheckBitmap(false);
      hbmpChecked:=0;
      {TODO: add support for getting icon from SubmenuImages as it will be
       implemented in LCL}
    end;
  end;
  if dword(InsertMenuItem(ParentMenuHandle,
       AMenuItem.Parent.VisibleIndexOf(AMenuItem), true, @MenuInfo)) = 0 then
    DebugLn('InsertMenuItem failed with error: ', IntToStr(Windows.GetLastError));
  TriggerFormUpdate(AMenuItem);
end;

function  TWin32WSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := CreatePopupMenu;
end;

procedure TWin32WSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  { not assigned when this the menuitem of a TMenu; handle is destroyed above }
  if Assigned(AMenuItem.Parent) then
    DeleteMenu(AMenuItem.Parent.Handle, AMenuItem.Command, MF_BYCOMMAND);
  TriggerFormUpdate(AMenuItem);
end;

procedure TWin32WSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
var newCaption: string;
begin
  newCaption := ACaption;  
  if AMenuItem.ShortCut <> scNone then
    newCaption := newCaption+#9+ShortCutToText(AMenuItem.ShortCut);
  UpdateCaption(AMenuItem, newCaption);
end;
  
procedure TWin32WSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
var
  NewCaption: string;
begin
  NewCaption := AMenuItem.Caption;
  if NewShortCut <> scNone then
    NewCaption := NewCaption + #9 + ShortCutToText(NewShortCut);
  UpdateCaption(AMenuItem, NewCaption);
end;

function TWin32WSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;

  function doCheckMenuItem(aMI: TMenuItem; CF: Integer): boolean;
  begin
    Result := Windows.CheckMenuItem(aMI.Parent.Handle, aMI.Command, CF) <> DWORD($FFFFFFFF);
  end;

  procedure InterfaceTurnSiblingsOff(AMenuItem: TMenuItem);
  var
    aParent, aSibling: TMenuItem;
    i: integer;
  begin
    // Just check all siblings that are in the same group
    // TMenuItem.TurnSiblingsOff should have modified internal flags
    aParent := AMenuItem.Parent;
    if aParent <> nil then
      for i := 0 to aParent.Count-1 do
      begin
        aSibling := aParent.Items[i];
        if (aSibling <> AMenuItem) and aSibling.RadioItem and (aSibling.GroupIndex=AMenuItem.GroupIndex) then
          doCheckMenuItem(aParent[i], MF_UNCHECKED or MF_BYCOMMAND);
      end;
  end;
var
  CheckFlag: Integer;
begin
  if Checked then CheckFlag := MF_CHECKED
  else CheckFlag := MF_UNCHECKED;
  CheckFlag := CheckFlag or MF_BYCOMMAND;
  if (CheckFlag and MF_CHECKED <> 0) and
    (AMenuItem.GroupIndex <> 0) and AMenuItem.RadioItem
  then
    InterfaceTurnSiblingsOff(aMenuItem);
  Result := doCheckMenuItem(aMenuItem, CheckFlag);
end;

function TWin32WSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
var
  EnableFlag: Integer;
begin
  if Enabled then EnableFlag := MF_ENABLED
  else EnableFlag := MF_GRAYED;
  EnableFlag := EnableFlag or MF_BYCOMMAND;
  Result := Boolean(Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, EnableFlag));
  TriggerFormUpdate(AMenuItem);
end;

function TWin32WSMenuItem.SetRadioItem(const AMenuItem: TMenuItem;
  const RadioItem: boolean): boolean;
var
  AParent, ASibling: TMenuItem;
  i: integer;
begin
  // Change all siblings that are in the same group
  AParent := AMenuItem.Parent;
  if AParent <> nil then begin
    Result := True;
    for i := 0 to AParent.Count-1 do
    begin
      ASibling := AParent.Items[i];
      if (AMenuItem.GroupIndex<>0) and
         (ASibling.GroupIndex=AMenuItem.GroupIndex) then begin
        Result := Result and ChangeMenuFlag(ASibling, MFT_RADIOCHECK, RadioItem);
        // make sure siblings have same state as the LCL has set them
        Result := Result and SetCheck(ASibling, ASibling.Checked);
      end;
    end;
  end
  else Result := False;
end;

function TWin32WSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  Result := ChangeMenuFlag(AMenuItem, MFT_RIGHTJUSTIFY, Justified);
end;


{ TWin32WSMenu }

function  TWin32WSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreateMenu;
end;

{ TWin32WSPopupMenu }

function  TWin32WSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreatePopupMenu;
end;

procedure TWin32WSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
var
  MenuHandle, AppHandle: HWND;
begin
  MenuHandle := APopupMenu.Handle;
  AppHandle := TWin32WidgetSet(WidgetSet).AppHandle;
  GetWindowInfo(AppHandle)^.PopupMenu := MenuHandle;
  TrackPopupMenuEx(MenuHandle, TPM_LEFTALIGN or TPM_LEFTBUTTON or TPM_RIGHTBUTTON,
    X, Y, AppHandle, Nil);
end;
  
initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TWin32WSMenuItem);
  RegisterWSComponent(TMenu, TWin32WSMenu);
//  RegisterWSComponent(TMainMenu, TWin32WSMainMenu);
  RegisterWSComponent(TPopupMenu, TWin32WSPopupMenu);
////////////////////////////////////////////////////
end.
