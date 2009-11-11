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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  public
    class procedure AttachMenuEx(const AMenuItem: TMenuItem; const AParentHandle: HMENU);
    class procedure CopyMenuToHandle(const AMenuItem: TMenuItem; const ADest: HMENU);
  published
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
  published
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
  published
    class function  CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;

const
  // IDs corresponding to the file winceres.rc
  MenuBarID_Items  = 20000;
  MenuBarID_PopUp_Item = 20001;
  MenuBarID_Item_Popup = 20002;
  MenuBarID_Popups = 20003;
  MenuBarID_L = 1;
  MenuBarID_R = 2;
  StartMenuItem = 200;
var
  MenuItemsList: TStringList;
  MenuHandleList, MenuLCLObjectList: TFPList;

function FindMenuItemAccelerator(const ACharCode: char; const AMenuHandle: HMENU): integer;
{$ifndef Win32}
procedure CeSetMenu(Wnd: HWND; Menu: HMENU; LCLMenu: TMenu);
{$endif}

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

{$ifndef Win32}
//both menus are popup menus or submenus
procedure CeMakeMenuesSame(SrcMenu,dstMenu : HMENU);
var
  i: Integer;
  mi: MENUITEMINFO;
  buf: array[0..255] of WideChar;
  fState:integer;
  uIDNewItem : Integer;
begin
//  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
//   'CeMakeMenusSame Src: ' + IntToStr(SrcMenu) + ' Dst: ' + IntToStr(DstMenu));

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
    uIDNewItem := mi.wID + StartMenuItem;
    if mi.hSubMenu <>  0 then
    begin
      uIDNewItem  := mi.hSubMenu;
      fstate := fstate or MF_POPUP;
    end;
    AppendMenu(dstMenu,fState,uIDNewItem,@buf);
    inc(i);
  end;
end;

{
  The main menu setting routine, it is called by LCLIntf.SetMenu, which
  associates a menu with a window.
}
procedure CeSetMenu(Wnd: HWND; Menu: HMENU; LCLMenu: TMenu);
var
  mbi: SHMENUBARINFO;
  mi: MENUITEMINFO;
  tb: TBButton;
  tbbi: TBBUTTONINFO;
  i, j, k: integer;
  buf: array[0..255] of WideChar;
  R, BR, WR: TRect;
  LeftMenuCount: Integer = -1;
  RightMenuCount: Integer = -1;
  MenuBarRLID: integer;
begin
  GetWindowRect(Wnd, BR);
  mbi.hwndMB:=SHFindMenuBar(Wnd);
//  if (mbi.hwndMB <> 0) and (CePlatform = cpSmartphone) then begin
//    DestroyWindow(mbi.hwndMB);
//    mbi.hwndMB:=0;
//  end;

  // If no menu is currently associated in the application
  // so we create a new one
  GetWindowRect(Wnd, BR);
  mbi.hwndMB:=SHFindMenuBar(Wnd);

  if mbi.hwndMB = 0 then
  begin
    FillChar(mbi, SizeOf(mbi), 0);
    mbi.cbSize := SizeOf(mbi);
    mbi.hwndParent := Wnd;
    //mbi.dwFlags := SHCMBF_HMENU;// This options ruins smartphone menu setting
    mbi.hInstRes := HINSTANCE;

//    if (Application.ApplicationType = atKeyPadDevice)
//      and (LCLMenu <> nil) then

    FillChar(mi, SizeOf(mi), 0);
    mi.cbSize:=SizeOf(mi);
    mi.fMask:=MIIM_SUBMENU or MIIM_TYPE or MIIM_ID or MIIM_STATE;
    mi.dwTypeData:=@buf;

    // Verifies the menu to find the best match for it's layout in the .rc file
    if LCLMenu <> nil then
    begin
      for j:=0 to LCLMenu.Items.Count - 1 do
      begin
        if LCLMenu.Items.Items[j].Visible then
        begin
          if LeftMenuCount = -1 then
            LeftMenuCount := LCLMenu.Items.Items[j].Count
          else if RightMenuCount = -1 then
            RightMenuCount := LCLMenu.Items.Items[j].Count
          else Break;
        end;
      end;
    end;

    if (LeftMenuCount >= 1) and (RightMenuCount >= 1) then
      mbi.nToolBarId := MenuBarID_Popups
    else if (LeftMenuCount >= 1) then
      mbi.nToolBarId := MenuBarID_PopUp_Item
    else if (RightMenuCount >= 1) then
      mbi.nToolBarId := MenuBarID_Item_Popup
    else
      mbi.nToolBarId := MenuBarID_Items;

    if not SHCreateMenuBar(@mbi) then Exit;
  end;

//  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
//    'menu bar window = ' + IntToStr(mbi.hwndMB) +
//    ' mbi.nToolBarId = ' + IntToStr(mbi.nToolBarId)
//    );

  // Clear any previously set menu items
  while SendMessage(mbi.hwndMB, TB_DELETEBUTTON, 0, 0) <> 0 do ;

  // Now we will add the buttons in the menu
  //
  // Note that there are two versions of this part of the code
  // First an approach like KOL-CE does, which works better for smartphones
  // and later the original code from lcl-wince, which already works for PDAs
  FillChar(mi, SizeOf(mi), 0);
  mi.cbSize:=SizeOf(mi);
  mi.fMask:=MIIM_SUBMENU or MIIM_TYPE or MIIM_ID or MIIM_STATE;
  mi.dwTypeData:=@buf;

  if (Application.ApplicationType = atKeyPadDevice) then
  begin
    if (Menu <> 0) and (LCLMenu <> nil) then
    begin
      i:=0;
      for j:=0 to LCLMenu.Items.Count - 1 do
        if LCLMenu.Items.Items[j].Visible then
        begin
          if LCLMenu.Items.Items[j].Enabled then
            tbbi.fsState:=TBSTATE_ENABLED
          else
            tbbi.fsState:=0;
          if LCLMenu.Items.Items[j].Checked then
            tbbi.fsState:=tbbi.fsState or TBSTATE_CHECKED;

          if (Application.ApplicationType = atKeyPadDevice) then
          begin
            // Adds a top-level item (We cant really add it, so we find
            // and modify the existing top-level item)
            if i = 2 then Break; // smartphones have maximum 2 top level menu items.
            tbbi.cbSize := sizeof(tbbi);
            tbbi.pszText := PWideChar(UTF8Decode(LCLMenu.Items.Items[j].Caption));
//            tbbi.idCommand := FID;
            tbbi.dwMask := TBIF_TEXT {or TBIF_COMMAND} or TBIF_STATE;
            SendMessage(mbi.hwndMB, TB_SETBUTTONINFO, i + 1, LPARAM(@tbbi));

            // Adds subitems to a top-level item
            tbbi.dwMask := TBIF_LPARAM;
            SendMessage(mbi.hwndMB, TB_GETBUTTONINFO, i + 1 {FID}, LPARAM(@tbbi));

            // Remove any present buttons, for example the one from the .rc file
            // Careful that using TB_DELETEBUTTON doesnt work here
            while RemoveMenu(HMENU(tbbi.lParam), 0, MF_BYPOSITION) do ;

            for k := 0 to LCLMenu.Items.Items[j].Count - 1 do
              TWinCEWSMenuItem.AttachMenuEx(
                LCLMenu.Items.Items[j].Items[k], HMENU(tbbi.lParam));
          end
          else
          begin
{            FillChar(tb, SizeOf(tb), 0);
            tb.iBitmap:=I_IMAGENONE;
            tb.idCommand:=fID;
            tb.iString:=longint(PKOLChar(Caption));
            tb.fsState:=st;
            if SubMenu <> 0 then
              tb.fsStyle:=TBSTYLE_DROPDOWN or $0080 or TBSTYLE_AUTOSIZE
            else
              tb.fsStyle:=TBSTYLE_BUTTON or TBSTYLE_AUTOSIZE;
            tb.dwData:=SubMenu;
            SendMessage(mbi.hwndMB, TB_INSERTBUTTON, i, LPARAM(@tb));}
          end;
          Inc(i);
        end;

      if (Application.ApplicationType = atKeyPadDevice) and (i = 1) then
      begin
        tbbi.dwMask := TBIF_STATE;
        tbbi.fsState:=0;
        SendMessage(mbi.hwndMB, TB_SETBUTTONINFO, 2, LPARAM(@tbbi));
      end;
    end;
  end
  else
  begin
    // Now we will add the buttons in the menu
  //  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
  //    'Menu: ' + IntToStr(Menu) + ' LCLMenu: ' + IntToStr(PtrInt(LCLMenu)));
    if (Menu <> 0) then
    begin
  //    DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log', 'if (Menu <> 0) and (LCLMenu <> nil) then');
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

        if (Application.ApplicationType = atKeyPadDevice) and (i < 2) then{KeyPadDevices can have only 2 buttons!}
        begin
          case i of
            0: MenuBarRLID := MenuBarID_L;
            1: MenuBarRLID := MenuBarID_R;
          end;
          tbbi.cbSize := sizeof(tbbi);
          tbbi.pszText := @buf;
          tbbi.dwMask := TBIF_TEXT;
          SendMessage(mbi.hwndMB, TB_SETBUTTONINFO, MenuBarRLID, LPARAM(@tbbi));
          tbbi.dwMask := TBIF_LPARAM;
          SendMessage(mbi.hwndMB, TB_GETBUTTONINFO, MenuBarRLID, LPARAM(@tbbi));
          CeMakeMenuesSame(mi.hSubMenu, HMENU(tbbi.lParam));
        end;

        Inc(i);
      end;
    end;
  end;

  // Correction for the position of the window
  // Avoids overlapping the menu, when it doesn't belong to the work area
  if (Application.ApplicationType in [atPDA, atDefault]) and
     (GetWindowLong(Wnd, GWL_STYLE) and WS_POPUP = 0) then // BorderStyle is neither bsDialog nor bsNone
  begin
    GetWindowRect(mbi.hwndMB, R);
    Windows.SystemParametersInfo(SPI_GETWORKAREA, 0, @WR, 0);

    if WR.Bottom > R.Top then
      SetWindowPos(wnd, 0, 0, 0, WR.Right - WR.Left, R.Top - WR.Top, SWP_NOZORDER or SWP_NOREPOSITION or SWP_NOMOVE);
  end;

//DrawMenuBar(wnd);
end;
{$endif}

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
  GetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command + StartMenuItem, false, @MenuInfo);
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
  Result := SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command + StartMenuItem, false, @MenuInfo);
  TriggerFormUpdate(AMenuItem);
end;

{ TWinCEWSMenuItem }

procedure UpdateCaption(const AMenuItem: TMenuItem; ACaption: String);
var
  MenuInfo: MENUITEMINFO;
  wCaption: WideString;
begin
  wCaption := UTF8Decode(ACaption);
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  with MenuInfo do
  begin
    cbsize := menuiteminfosize;
    fMask := MIIM_TYPE or MIIM_STATE;
    if ACaption <> cLineCaption then
    begin
      fType := MFT_STRING;
      if AMenuItem.Enabled then fState := MF_ENABLED
      else fState := MF_GRAYED;
      {$ifdef win32}
      dwTypeData:=PChar(PWideChar(wCaption));
      {$else}
      dwTypeData:=PWideChar(wCaption);
      {$endif}
      cch := Length(aCaption);
    end
    else
    begin
      fType := MFT_SEPARATOR;
      fState := MFS_DISABLED;
   end;
  end;
  if not SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command + StartMenuItem, false, @MenuInfo) then
    DebugLn('SetMenuItemInfo failed: ', GetLastErrorText(GetLastError));
  TriggerFormUpdate(AMenuItem);
end;

class procedure TWinCEWSMenuItem.AttachMenuEx(const AMenuItem: TMenuItem;
  const AParentHandle: HMENU);
var
  MenuInfo: MENUITEMINFO;
  ParentOfParent: HMenu;
  wCaption: WideString;
  Index, fstate, cmd: integer;
begin
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
    GetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command + StartMenuItem,
                    False, @MenuInfo);
    if MenuInfo.hSubmenu = 0 then // the parent menu item is not yet defined with submenu flag
    begin
      //roozbeh: wont work on smartphones...i guess i have to remove and add new one with submenu flag
      //not yet found time to do....not so hard
      MenuInfo.hSubmenu := AParentHandle;
      SetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command,
                      False, @MenuInfo);
    end;
  end;

  fState := MF_STRING or MF_BYPOSITION;
  if AMenuItem.Enabled then fState := fState or MF_ENABLED
  else fState := fState or MF_GRAYED;
  if AMenuItem.Checked then
    fState := fState or MF_CHECKED;

  cmd := AMenuItem.Command + StartMenuItem; {value may only be 16 bit wide!}
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

  if not InsertMenuW(AParentHandle, Index, fState, cmd, PWideChar(wCaption)) then
    DebugLn('InsertMenuW failed for ', dbgsName(AMenuItem), ' : ', GetLastErrorText(GetLastError));

  MenuInfo.cbSize := SizeOf(MenuInfo);
  MenuInfo.fMask := MIIM_DATA;
  //GetMenuItemInfo(ParentMenuHandle, Index, True, @MenuInfo);
  MenuInfo.dwItemData := PtrInt(AMenuItem);
  //MenuInfo.wID := AMenuItem.Command;
  if not SetMenuItemInfoW(AParentHandle, Index, True, @MenuInfo) then
    DebugLn(['SetMenuItemInfoW failed for ', dbgsName(AMenuItem), ' : ', GetLastErrorText(GetLastError)]);

  MenuItemsList.AddObject(IntToStr(AMenuItem.Command + StartMenuItem), AMenuItem);
  TriggerFormUpdate(AMenuItem);

//  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
//    'MenuItemsList.AddObject: ' + IntToStr(AMenuItem.Command + StartMenuItem) +
//    ' Object: ' + IntToStr(PtrInt(AMenuItem))
//    );
end;

class procedure TWinCEWSMenuItem.CopyMenuToHandle(const AMenuItem: TMenuItem;
  const ADest: HMENU);
var
  i: integer;
  mi: MENUITEMINFO;
  buf: array[0..255] of WideChar;
  fState:integer;
  uIDNewItem  : integer;
begin
//  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
//   'CeMakeMenusSame Src: ' + IntToStr(SrcMenu) + ' Dst: ' + IntToStr(DstMenu));

  while RemoveMenu(ADest, 0, MF_BYPOSITION)  do ;

  i:=0;
  mi.cbSize:=SizeOf(mi);
  mi.fMask:=MIIM_SUBMENU or MIIM_TYPE or MIIM_ID or MIIM_STATE;
  mi.dwTypeData:=@buf;

  while GetMenuItemInfo(AMenuItem.Handle, i, True, mi) do
  begin
    buf[mi.cch]:=#0;
    fState:=MF_STRING;
    if mi.fState and MFS_DISABLED <> 0 then
      fState:=fState or MF_GRAYED;
    if mi.fState and MFS_CHECKED <> 0 then
      fState:=fState or MF_CHECKED;
    uIDNewItem := mi.wID + StartMenuItem;
    if mi.hSubMenu <>  0 then
    begin
      uIDNewItem  := mi.hSubMenu;
      fstate := fstate or MF_POPUP;
    end;
    Windows.AppendMenu(ADest, fState, uIDNewItem, @buf);
    inc(i);
  end;
end;

class procedure TWinCEWSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
begin
  AttachMenuEx(AMenuItem, AMenuItem.Parent.Handle);
end;

class function TWinCEWSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := CreatePopupMenu;
end;

class procedure TWinCEWSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  if Assigned(AMenuItem.Parent) then
    DeleteMenu(AMenuItem.Parent.Handle, AMenuItem.Command + StartMenuItem, MF_BYCOMMAND);
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
  Result := Boolean(Windows.CheckMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command + StartMenuItem, uCheck));
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
  EnableFlag := MF_BYCOMMAND;
  if AMenuItem.Enabled then EnableFlag := EnableFlag or MF_ENABLED
  else EnableFlag := EnableFlag or MF_GRAYED;
  Result := Boolean(Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command + StartMenuItem, EnableFlag));
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
  // A pointer to the LCL item is saved to be used latter by CeSetMenu
  // LCLIntf.SetProp and SetWindowLongW were also tryed but didn't work
  MenuHandleList.Add(Pointer(Result));
  MenuLCLObjectList.Add(Pointer(AMenu));
//  DbgAppendToFile(ExtractFilePath(ParamStr(0)) + '1.log',
//   'TWinCEWSMenu.CreateHandle: ' + IntToStr(Result) + ' AMenu: ' + IntToStr(PtrInt(AMenu)));
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

  MenuHandleList := TFPList.Create;
  MenuLCLObjectList := TFPList.Create;

finalization

  MenuItemsList.Free;

  MenuHandleList.Free;
  MenuLCLObjectList.Free;

end.
