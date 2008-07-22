{
 *****************************************************************************
 *                              WinCEWSMenus.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit WinCEWSMenus;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Graphics, GraphType, ImgList, Menus, Forms, LCLIntf, {keep before Windows }
  Controls,  InterfaceBase, LCLProc,
  // RTL, FCL
  Windows, Classes, SysUtils, commctrl,
  {$ifndef win32}aygshell,{$endif}
  // widgetset
  WinceInt, WinceProc, WinCEWSImgList,
  WSMenus, WSLCLClasses;

type

  { TWinCEWSMenuItem }

  TWinCEWSMenuItem = class(TWSMenuItem)
  private
  protected
  public
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const OldShortCut, NewShortCut: TShortCut); override;
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
    class function SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean; override;
    class procedure UpdateMenuIcon(const AMenuItem: TMenuItem; const HasIcon: Boolean; const AIcon: Graphics.TBitmap); override;
  end;

  { TWinCEWSMenu }

  TWinCEWSMenu = class(TWSMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
  end;

  { TWinCEWSMainMenu }

  TWinCEWSMainMenu = class(TWSMainMenu)
  private
  protected
  public
  end;

  { TWinCEWSPopupMenu }

  TWinCEWSPopupMenu = class(TWSPopupMenu)
  private
  protected
  public
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;

const
  //having left or right submenus [true,false] means right have submenu,left doesnt have
  MenuBarIDS : array[Boolean, Boolean] of integer =
  (
    (101,105),
    (106,107)
  );
  MenuBarID_L = 40052;
  MenuBarID_R = 40053;

var
  MenuItemsList : TStringList;

function FindMenuItemAccelerator(const ACharCode: char; const AMenuHandle: HMENU): integer;
procedure CeSetMenu(Wnd: HWND; Menu: HMENU);

implementation

uses strutils;

{$R wincemenures.rc}

{ helper routines }

const
  SpaceBetweenIcons = 5;

var
  menuiteminfosize : DWORD = 0;

type
  TCaptionFlags = (cfBold, cfUnderline);
  TCaptionFlagsSet = set of TCaptionFlags;

//menus

const
  SPI_GETPLATFORMTYPE = 257;//roozbeh : should be moved to windows unit

function WStrCmp( W1, W2: PWideChar ): Integer;
var
  counter: Integer;
begin
  counter := 0;
  while W1[counter] = W2[counter] do
  begin
    if (W2[counter] = #0) or (W1[counter] = #0) then
       break;
    Inc(counter);
  end;
  Result := ord(W1[counter]) - ord(W2[counter]);
end;

function IsSmartPhone: Boolean;
var
  buf: array[0..255] of WideChar;
begin
  Result := false;
  if SystemParametersInfo(SPI_GETPLATFORMTYPE,sizeof(buf),@buf,0) then
  begin
    if WStrCmp(@buf,PWideChar('SmartPhone')) = 0  then
      Result := true
    else
      Result := false;//roozbeh : either it is 'PocketPC' or something else :)
  end
  else if GetLastError=ERROR_ACCESS_DENIED then
    Result := true;
end;

//both menus are popup menus or submenus
procedure CeMakeMenuesSame(SrcMenu,dstMenu : HMENU);
var
  i: integer;
  mi: MENUITEMINFO;
  buf: array[0..255] of WideChar;
  fState:integer;
  uIDNewItem  : integer;
begin
  while RemoveMenu(dstMenu,0,MF_BYPOSITION)  do ;

  i:=0;
  mi.cbSize:=SizeOf(mi);
  mi.fMask:=MIIM_SUBMENU or MIIM_TYPE or MIIM_ID or MIIM_STATE;
  mi.dwTypeData:=@buf;

  while GetMenuItemInfo(srcMenu, i, True, mi) do
  begin
    buf[mi.cch]:=#0;
    fState:=MF_STRING;
    if mi.fState and MFS_DISABLED <> 0 then
      fState:=fState or MF_GRAYED;
    if mi.fState and MFS_CHECKED <> 0 then
      fState:=fState or MF_CHECKED;
    uIDNewItem := mi.wID;
    if mi.hSubMenu <>  0 then
    begin
      uIDNewItem  := mi.hSubMenu;
      fstate := fstate or MF_POPUP;
    end;
    AppendMenu(dstMenu,fState,uIDNewItem,@buf);
    inc(i);
  end;
end;

procedure CeSetMenu(Wnd: HWND; Menu: HMENU);
{$ifndef Win32}
var
  mbi: SHMENUBARINFO;
  mi: MENUITEMINFO;
  tb: TBButton;
  tbbi : TBBUTTONINFO;
  i: integer;
  buf: array[0..255] of WideChar;
  R, BR: TRect;
  hasLMenu,hasRMenu : boolean;
  MenuBarRLID : integer;
{$endif}
begin
{$ifndef Win32}
  hasLMenu := false;
  hasRMenu := false;
  FillChar(mi, SizeOf(mi), 0);
  with mi do begin
    cbSize:=SizeOf(mi);
    fMask:=MIIM_SUBMENU or MIIM_TYPE or MIIM_ID or MIIM_STATE;
    dwTypeData:=@buf;
  end;

  if Menu <> 0 then begin
    if GetMenuItemInfo(Menu, 0, True, mi) then//does it have left menu?
      hasLMenu := True;
    if GetMenuItemInfo(Menu, 1, True, mi) then//does it have right menu?
      hasRMenu := True;
  end;

  GetWindowRect(Wnd, BR);
  mbi.hwndMB:=SHFindMenuBar(Wnd);
  FillChar(mbi, SizeOf(mbi), 0);
  with mbi do begin
    cbSize:=SizeOf(mbi);
    hwndParent:=Wnd;
    dwFlags:=SHCMBF_HMENU;
    nToolBarId:=MenuBarIDS[hasLMenu,hasRMenu];
    hInstRes:=HINSTANCE;
  end;

  {if found a menubar check if it matches number of buttons of previous menubar...}
  if (mbi.hwndMB = 0) or (
     (not ((boolean(SendMessage (mbi.hwndMB, TB_COMMANDTOINDEX, MenuBarID_L, 0) + 1)) xor (hasLMenu))) and
     (not ((boolean(SendMessage (mbi.hwndMB, TB_COMMANDTOINDEX, MenuBarID_R, 0) + 1)) xor (hasRMenu))))
    then
  begin
    if not SHCreateMenuBar(@mbi) then
    begin
      //MsgBox('not ok',0);
      exit;
    end;
  end;

  while SendMessage(mbi.hwndMB, TB_DELETEBUTTON, 0, 0) <> 0 do ;

  with mi do
  begin
    cbSize:=SizeOf(mi);
    fMask:=MIIM_SUBMENU or MIIM_TYPE or MIIM_ID or MIIM_STATE;
    dwTypeData:=@buf;
  end;

  if Menu <> 0 then
  begin
    i:=0;
    while True do
    begin
      mi.cch:=SizeOf(buf);
      if not GetMenuItemInfo(Menu, i, True, @mi) then
        break;
      buf[mi.cch]:=#0;
      FillChar(tb, SizeOf(tb), 0);
      tb.iBitmap:=I_IMAGENONE;
      tb.idCommand:=mi.wID;
      tb.iString:=SendMessage(mbi.hwndMB, TB_ADDSTRING, 0, LPARAM(@buf));
      if mi.fState and MFS_DISABLED = 0 then
        tb.fsState:=TBSTATE_ENABLED;
      if mi.fState and MFS_CHECKED <> 0 then
        tb.fsState:=tb.fsState or TBSTATE_CHECKED;
      if mi.hSubMenu <> 0 then
        tb.fsStyle:=TBSTYLE_DROPDOWN or $0080 or TBSTYLE_AUTOSIZE
      else
        tb.fsStyle:=TBSTYLE_BUTTON or TBSTYLE_AUTOSIZE;
      tb.dwData:=mi.hSubMenu;
      {roozbeh : this wont work on 2002/2003...should i uncomment it or not?works this way anyway}
      SendMessage(mbi.hwndMB, TB_INSERTBUTTON, i, LPARAM(@tb));
      //MsgBox('i = ' + int2str(i),0);

      if (IsSmartphone) and (i < 2) then{Smartphones can have only 2 buttons!}
      begin
        case i of
          0:  MenuBarRLID := MenuBarID_L;
          1 : MenuBarRLID := MenuBarID_R;
        end;
        tbbi.cbSize := sizeof(tbbi);
        tbbi.pszText := @buf;
        tbbi.dwMask := TBIF_TEXT;
        SendMessage(mbi.hwndMB,TB_SETBUTTONINFO,MenuBarRLID,LPARAM(@tbbi));
        tbbi.dwMask := TBIF_LPARAM;
        SendMessage (mbi.hwndMB, TB_GETBUTTONINFO, MenuBarRLID, LPARAM(@tbbi));
        CeMakeMenuesSame(mi.hSubMenu,HMENU(tbbi.lParam));
      end;

      Inc(i);
    end;
  end;

 GetWindowRect(mbi.hwndMB, R);
//  if BR.Bottom > R.Top then
//    SetWindowPos(wnd, 0, 0, 0, BR.Right - BR.Left, R.Top - BR.Top, SWP_NOZORDER or SWP_NOREPOSITION or SWP_NOMOVE);

//DrawMenuBar(wnd);
{$endif}
end;



(* Returns index of the character in the menu item caption that is displayed
   as underlined and is therefore the hot key of the menu item.
   If the caption does not contain any underlined character, 0 is returned.
   If there are more "underscored" characters in the caption, the last one is returned.
   Does some Windows API function exists which can do the same?
   AnUnderlinedChar - character which tells that tne following character should be underlined
   ACaption - menu item caption which is parsed *)
function SearchMenuItemHotKeyIndex(const AnUnderlinedChar: char; ACaption: string): integer;
var position: integer;
begin
  position := pos(AnUnderlinedChar, ACaption);
  Result := 0;
  // if aChar is on the last position then there is nothing to underscore, ignore this character
  while (position > 0) and (position < length(ACaption)) do
  begin
    // two 'AnUnderlinedChar' characters together are not valid hot key, they are replaced by one
    if ACaption[position + 1] <> AnUnderlinedChar then
      Result := position + 1;
    position := posEx(AnUnderlinedChar, ACaption, position + 2);
  end;
end;

function FindMenuItemAccelerator(const ACharCode: char; const AMenuHandle: HMENU): integer;
var
  MenuItemIndex: integer;
  ItemInfo: MENUITEMINFO;
  FirstMenuItem: TMenuItem;
  SiblingMenuItem: TmenuItem;
  HotKeyIndex: integer;
  i: integer;
begin
  Result := MakeLResult(0, 0);
  MenuItemIndex := -1;
  ItemInfo.cbSize := menuiteminfosize;
  ItemInfo.fMask := MIIM_DATA;
  if not GetMenuItemInfo(AMenuHandle, 0, true, @ItemInfo) then Exit;
  FirstMenuItem := TMenuItem(ItemInfo.dwItemData);
  if FirstMenuItem = nil then exit;
  i := 0;
  while (i < FirstMenuItem.Parent.Count) and (MenuItemIndex < 0) do
  begin
    SiblingMenuItem := FirstMenuItem.Parent.Items[i];
    HotKeyIndex := SearchMenuItemHotKeyIndex('&', SiblingMenuItem.Caption);
    if (HotKeyIndex > 0) and
      (Upcase(ACharCode) = Upcase(SiblingMenuItem.Caption[HotKeyIndex])) then
        MenuItemIndex := i;
    inc(i);
  end;
  if MenuItemIndex > -1 then Result := MakeLResult(MenuItemIndex, 2)
  else Result := MakeLResult(0, 0);
end;

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
  wCaption : WideString;
begin
  MenuInfo.cbSize := menuiteminfosize;
  MenuInfo.fMask := MIIM_TYPE;
  MenuInfo.dwTypeData := nil;  // don't retrieve caption
  GetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  if Value then
    MenuInfo.fType := MenuInfo.fType or Flag
  else
    MenuInfo.fType := MenuInfo.fType and (not Flag);
  wCaption := UTF8Decode(AMenuItem.Caption);
  {$ifdef win32}
  MenuInfo.dwTypeData := PChar(PWideChar(wCaption));
  {$else}
  MenuInfo.dwTypeData := PWideChar(wCaption);
  {$endif}
  Result := SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  TriggerFormUpdate(AMenuItem);
end;

{ TWinCEWSMenuItem }

procedure UpdateCaption(const AMenuItem: TMenuItem; ACaption: String);
var
  MenuInfo: MENUITEMINFO;
  wCaption: WideString;
begin
  wCaption := UTF8Decode(ACaption);
  with MenuInfo do
  begin
    cbsize := menuiteminfosize;
    if ACaption <> '-' then
    begin
      fType := MFT_STRING;
      fMask := MIIM_TYPE;
      {$ifdef win32}
      dwTypeData:=PChar(PWideChar(wCaption));
      {$else}
      dwTypeData:=PWideChar(wCaption);
      {$endif}
      cch := Length(aCaption);
    end
    else
      fType := MFT_SEPARATOR;
  end;
  if not SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo) then
    DebugLn('SetMenuItemInfo failed: ', GetLastErrorText(GetLastError));
  TriggerFormUpdate(AMenuItem);
end;

class procedure TWinCEWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  MenuInfo: MENUITEMINFO;
  ParentMenuHandle: HMenu;
  ParentOfParent: HMenu;
  wCaption: WideString;
  Index, fstate, cmd: integer;
begin
  ParentMenuHandle := AMenuItem.Parent.Handle;
  
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);

  {Following part fixes the case when an item is added in runtime
  but the parent item has not defined the submenu flag (hSubmenu=0) }
  if AMenuItem.Parent.Parent <> nil then
  begin
    ParentOfParent := AMenuItem.Parent.Parent.Handle;
    with MenuInfo do
    begin
      cbSize := menuiteminfosize;
      fMask := MIIM_SUBMENU;
    end;
    GetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command,
                    False, @MenuInfo);
    if MenuInfo.hSubmenu = 0 then // the parent menu item is not yet defined with submenu flag
    begin
      //roozbeh: wont work on smartphones...i guess i have to remove and add new one with submenu flag
      //not yet found time to do....not so hard
      MenuInfo.hSubmenu := ParentMenuHandle;
      SetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command,
                      False, @MenuInfo);
    end;
  end;

  fState := MF_STRING or MF_BYPOSITION;
  if AMenuItem.Enabled then
    fState := fState or MF_ENABLED
  else
    fstate := fState or MF_GRAYED;
  if AMenuItem.Checked then
    fState := fState or MF_CHECKED;

  cmd := AMenuItem.Command; {value may only be 16 bit wide!}
  if (AMenuItem.Count > 0) then
  begin
    fState := fState or MF_POPUP;
    cmd := AMenuItem.Handle;
  end
  else
  begin
    if AMenuItem.IsLine then
      fState := (fState xor MF_STRING) or MF_SEPARATOR;
  end;

  wCaption := UTF8Decode(AmenuItem.Caption);
  Index := AMenuItem.Parent.VisibleIndexOf(AMenuItem);

  if not InsertMenuW(ParentMenuHandle, Index, fState, cmd, PWideChar(wCaption)) then
    DebugLn('InsertMenuW failed for ', dbgsName(AMenuItem), ' : ', GetLastErrorText(GetLastError));

  MenuInfo.cbSize := SizeOf(MenuInfo);
  MenuInfo.fMask := MIIM_DATA;
  //GetMenuItemInfo(ParentMenuHandle, Index, True, @MenuInfo);
  MenuInfo.dwItemData := PtrInt(AMenuItem);
  //MenuInfo.wID := AMenuItem.Command;
  
  if not SetMenuItemInfoW(ParentMenuHandle, Index, True, @MenuInfo) then
    DebugLn(['SetMenuItemInfoW failed for ', dbgsName(AMenuItem), ' : ', GetLastErrorText(GetLastError)]);

  MenuItemsList.AddObject(IntToStr(AMenuItem.Command), AMenuItem);
  TriggerFormUpdate(AMenuItem);
end;

class function TWinCEWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := CreatePopupMenu;
end;

class procedure TWinCEWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  if Assigned(AMenuItem.Parent) then
    DeleteMenu(AMenuItem.Parent.Handle, AMenuItem.Command, MF_BYCOMMAND);
  DestroyMenu(AMenuItem.Handle);
  TriggerFormUpdate(AMenuItem);
end;

class procedure TWinCEWSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
  UpdateCaption(AMenuItem, aCaption);
end;

class function TWinCEWSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
var
  uCheck: UINT;
begin
  if Checked then
    uCheck := MF_CHECKED
  else
    uCheck := MF_UNCHECKED;
  Result := Boolean(Windows.CheckMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, uCheck));
end;

class procedure TWinCEWSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin
  UpdateCaption(AMenuItem, aMenuItem.Caption);
end;

class function TWinCEWSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
var
  EnableFlag: Integer;
begin
  if Enabled then
    EnableFlag := MF_ENABLED
  else
    EnableFlag := MF_GRAYED;
  EnableFlag := EnableFlag or MF_BYCOMMAND;
  Result := Boolean(Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, EnableFlag));
  TriggerFormUpdate(AMenuItem);
end;

class function TWinCEWSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  Result := ChangeMenuFlag(AMenuItem, MFT_RIGHTJUSTIFY, Justified);
end;

class procedure TWinCEWSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: Graphics.TBitmap);
begin
  // not implemented
end;

{ TWinCEWSMenu }

class function TWinCEWSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreateMenu;
end;

{ TWinCEWSPopupMenu }

class function TWinCEWSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreatePopupMenu;
end;

class procedure TWinCEWSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
var
  MenuHandle: HMENU;
  AppHandle: HWND;
const
  lAlign: array[Boolean] of Word = (TPM_LEFTALIGN, TPM_RIGHTALIGN);
begin
  MenuHandle := APopupMenu.Handle;
  AppHandle := TWinCEWidgetSet(WidgetSet).AppHandle;
  GetWindowInfo(AppHandle)^.PopupMenu := APopupMenu;
  TrackPopupMenuEx(MenuHandle, lAlign[APopupMenu.IsRightToLeft] or TPM_LEFTBUTTON or TPM_RIGHTBUTTON,
    X, Y, AppHandle, nil);
end;

initialization
  menuiteminfosize := SizeOf(TMenuItemInfo);
  MenuItemsList := TStringList.Create;

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TMenuItem, TWinCEWSMenuItem);
  RegisterWSComponent(TMenu, TWinCEWSMenu);
//  RegisterWSComponent(TMainMenu, TWinCEWSMainMenu);
  RegisterWSComponent(TPopupMenu, TWinCEWSPopupMenu);
////////////////////////////////////////////////////

finalization
  MenuItemsList.Free;

end.
