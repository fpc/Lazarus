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
  Windows, Classes, SysUtils,
  {$ifndef ver2_2_0}commctrl,
  {$ifndef win32}aygshell,{$endif}{$endif}
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
  MenuBarID_1_Item = 20004;
  MenuBarID_1_Popup = 20005;
  MenuBarID_Empty = 20006;
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

{$ifndef ver2_2_0}
{$R wincemenures.rc}
{$else}
{$R wincemenures.res}
{$endif}

{ helper routines }

const
  SpaceBetweenIcons = 5;

var
  menuiteminfosize : DWORD = 0;

type
  TCaptionFlags = (cfBold, cfUnderline);
  TCaptionFlagsSet = set of TCaptionFlags;
  TMenuItemAccess = class(TMenuItem);

//menus

{$ifndef Win32}
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
  VisibleTopLevelCount: Integer = 0;
begin
  {$ifdef VerboseWinCEMenu}
  DebugLn('[CeSetMenu]');
  {$endif}

  GetWindowRect(Wnd, BR);
  mbi.hwndMB := SHFindMenuBar(Wnd);

  {$ifdef VerboseWinCEMenu}
  DebugLn('[CeSetMenu] p1 menu bar window = ' + IntToStr(mbi.hwndMB));
  {$endif}

  // It is necessary to always create a new menu bar for atKeyPadDevice?
{  if (Application.ApplicationType = atKeyPadDevice) then
  begin
    if (mbi.hwndMB <> 0) then
      DestroyWindow(mbi.hwndMB);

    mbi.hwndMB := 0;
  end;}

  GetWindowRect(Wnd, BR);

  // If no menu is currently associated in the application
  // so we create a new one
  if mbi.hwndMB = 0 then
  begin
    FillChar(mbi, SizeOf(mbi), 0);
    mbi.cbSize := SizeOf(mbi);
    mbi.hwndParent := Wnd;
    //mbi.dwFlags := SHCMBF_HMENU;// This options ruins smartphone menu setting
    mbi.hInstRes := HINSTANCE;

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
          Inc(VisibleTopLevelCount);

          if LeftMenuCount = -1 then
            LeftMenuCount := LCLMenu.Items.Items[j].Count
          else if RightMenuCount = -1 then
            RightMenuCount := LCLMenu.Items.Items[j].Count
          else Break;
        end;
      end;
    end;

    // Chooses the best style
    if VisibleTopLevelCount = 0 then
      mbi.nToolBarId := MenuBarID_Empty
    else if VisibleTopLevelCount = 1 then
    begin
      if (LeftMenuCount >= 1) then
        mbi.nToolBarId := MenuBarID_1_Popup
      else mbi.nToolBarId := MenuBarID_1_Item;
    end
    else
    begin
      if (LeftMenuCount >= 1) and (RightMenuCount >= 1) then
        mbi.nToolBarId := MenuBarID_Popups
      else if (LeftMenuCount >= 1) then
        mbi.nToolBarId := MenuBarID_PopUp_Item
      else if (RightMenuCount >= 1) then
        mbi.nToolBarId := MenuBarID_Item_Popup
      else
        mbi.nToolBarId := MenuBarID_Items;
    end;

    if not SHCreateMenuBar(@mbi) then
    begin
      {$ifdef VerboseWinCEMenu}
      DebugLn('[CeSetMenu] SHCreateMenuBar failed');
      {$endif}
      Exit;
    end;
  end;

  {$ifdef VerboseWinCEMenu}
  DebugLn('[CeSetMenu] menu bar window = ' + IntToStr(mbi.hwndMB) +
    ' mbi.nToolBarId = ' + IntToStr(mbi.nToolBarId));
  {$endif}

  // Clear any previously set menu items
  while SendMessage(mbi.hwndMB, TB_DELETEBUTTON, 0, 0) <> 0 do
  {$ifdef VerboseWinCEMenu}
  DebugLn('[CeSetMenu] TB_DELETEBUTTON')
  {$endif}
  ;

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
      i:=0; // j = counts all top-level menu items;
            // i = counts only visible ones;
      for j:=0 to LCLMenu.Items.Count - 1 do
      begin
        if LCLMenu.Items.Items[j].Visible then
        begin
          if LCLMenu.Items.Items[j].Enabled then
            tbbi.fsState:=TBSTATE_ENABLED
          else
            tbbi.fsState:=0;
          if LCLMenu.Items.Items[j].Checked then
            tbbi.fsState:=tbbi.fsState or TBSTATE_CHECKED;

          // Adds a top-level item (We cant really add it, so we find
          // and modify the existing top-level item)
          if i = 2 then Break; // smartphones have maximum 2 top level menu items.

          if i = 0 then MenuBarRLID := MenuBarID_L
          else MenuBarRLID := MenuBarID_R;

          tbbi.cbSize := sizeof(tbbi);
          tbbi.pszText := PWideChar(UTF8Decode(LCLMenu.Items.Items[j].Caption));
          tbbi.dwMask := TBIF_TEXT or TBIF_COMMAND or TBIF_STATE;

          // Without setting idCommand the top-level items don't respond to clicks
          case mbi.nToolBarId of
          MenuBarID_Popups:     tbbi.idCommand := MenuBarRLID;
          MenuBarID_PopUp_Item: tbbi.idCommand := MenuBarRLID;
          MenuBarID_Item_Popup: tbbi.idCommand := MenuBarRLID;
          MenuBarID_Items:      tbbi.idCommand := StartMenuItem + MenuBarRLID;
          MenuBarID_1_Popup:    tbbi.idCommand := MenuBarRLID;
          MenuBarID_1_Item:     tbbi.idCommand := StartMenuItem + MenuBarRLID;
          end;
          // Update the MenuItem Command to use latter
          TMenuItemAccess(LCLMenu.Items.Items[j]).FCommand := MenuBarRLID;

          {$ifdef VerboseWinCEMenu}
          DebugLn('[CeSetMenu] atKeyPadDevice Set FCommand from ', LCLMenu.Items.Items[j].Name, ' to: ',
            dbgs(TMenuItemAccess(LCLMenu.Items.Items[j]).FCommand));
          DebugLn('[CeSetMenu] atKeyPadDevice Message TB_SETBUTTONINFO with ButtonID: MenuBarRLID = ' + IntToStr(MenuBarRLID));
          {$endif}

          if SendMessage(mbi.hwndMB, TB_SETBUTTONINFO, MenuBarRLID, LPARAM(@tbbi)) = 0 then
            DebugLn('[CeSetMenu] TB_SETBUTTONINFO failed');

          // Adds subitems to a top-level item
          {$ifdef VerboseWinCEMenu}
          DebugLn('[CeSetMenu] atKeyPadDevice Message TB_GETBUTTONINFO with ButtonID: MenuBarRLID = ' + IntToStr(MenuBarRLID));
          {$endif}
          tbbi.dwMask := TBIF_LPARAM;
          if SendMessage(mbi.hwndMB, TB_GETBUTTONINFO, MenuBarRLID, LPARAM(@tbbi)) = - 1 then
            DebugLn('[CeSetMenu] TB_GETBUTTONINFO failed');

          // Remove any present buttons, for example the one from the .rc file
          // Careful that using TB_DELETEBUTTON doesnt work here
          while RemoveMenu(HMENU(tbbi.lParam), 0, MF_BYPOSITION) do DebugLn('[CeSetMenu] RemoveMenu');

          for k := 0 to LCLMenu.Items.Items[j].Count - 1 do
            TWinCEWSMenuItem.AttachMenuEx(
              LCLMenu.Items.Items[j].Items[k], HMENU(tbbi.lParam));
          Inc(i);
        end;
      end;

      if i = 1 then
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
  //  DebugLn('Menu: ' + IntToStr(Menu) + ' LCLMenu: ' + IntToStr(PtrInt(LCLMenu)));
    if (Menu <> 0) and (LCLMenu <> nil) then
    begin
      i:=0;
      while True do
      begin
        mi.cch:=SizeOf(buf);

        // Find the winapi menu item
        if not GetMenuItemInfo(Menu, i, True, @mi) then
        begin
          DebugLn('GetMenuItemInfo i=', dbgs(i), ' failed, breaking');
          Break;
        end;

        // Find the associated LCL Menu item
        k:=0; // j = counts all top-level menu items
              // k = counts only visible ones;
        for j:=0 to LCLMenu.Items.Count - 1 do
        begin
          if LCLMenu.Items.Items[j].Visible then
          begin
            if k = i then Break;
            Inc(k);
          end;
        end;
        // Update the MenuItem Command to use latter
        TMenuItemAccess(LCLMenu.Items.Items[j]).FCommand := mi.wID;

        buf[mi.cch]:=#0;
        FillChar(tb, SizeOf(tb), 0);
        tb.iBitmap:=I_IMAGENONE;
        tb.idCommand := mi.wID;
        {$ifdef VerboseWinCEMenu}
        DebugLn('[CeSetMenu] p3 atPDA menu ' + LCLMenu.Items.Items[j].Name + ' Set FCommand = mi.wID = ' + IntToStr(tb.idCommand));
        {$endif}

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
        {$ifdef VerboseWinCEMenu}
        DebugLn('[CeSetMenu] atPDA Message TB_INSERTBUTTON with ButtonID: i = ' + IntToStr(i));
        {$endif}
        if SendMessage(mbi.hwndMB, TB_INSERTBUTTON, i, LPARAM(@tb)) = 0 then
          DebugLn('TB_INSERTBUTTON failed');

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
  {$ifdef VerboseWinCEMenu}
  DebugLn('[UpdateCaption] SetMenuItemInfo for ' + AMenuItem.Name +
    ' with ButtonID = AMenuItem.Command + StartMenuItem = ' + IntToStr(AMenuItem.Command + StartMenuItem));
  {$endif}
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
  {$ifdef VerboseWinCEMenu}
  DebugLn('[TWinCEWSMenuItem.AttachMenuEx] Start');
  {$endif}

  FillChar(MenuInfo, SizeOf(MenuInfo), 0);

  {Following part fixes the case when an item is added in runtime
  but the parent item has not defined the submenu flag (hSubmenu=0) }
  if (AMenuItem.Parent.Parent <> nil) and
    (Application.ApplicationType <> atKeyPadDevice) then
  begin
    ParentOfParent := AMenuItem.Parent.Parent.Handle;
    with MenuInfo do
    begin
      cbSize := menuiteminfosize;
      fMask := MIIM_SUBMENU;
    end;
    {$ifdef VerboseWinCEMenu}
    DebugLn('[TWinCEWSMenuItem.AttachMenuEx] GetMenuItemInfo for '
      + AMenuItem.Parent.Name + ' with ButtonID = AMenuItem.Parent.Command + StartMenuItem = ' + IntToStr(AMenuItem.Parent.Command + StartMenuItem));
    {$endif}
    if not GetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command + StartMenuItem, False, @MenuInfo) then
      DebugLn('[TWinCEWSMenuItem.AttachMenuEx] GetMenuItemInfo failed');
    if MenuInfo.hSubmenu = 0 then // the parent menu item is not yet defined with submenu flag
    begin
      //roozbeh: wont work on smartphones...i guess i have to remove and add new one with submenu flag
      //not yet found time to do....not so hard
      MenuInfo.hSubmenu := AParentHandle;
      {$ifdef VerboseWinCEMenu}
      DebugLn('[TWinCEWSMenuItem.AttachMenuEx] SetMenuItemInfo for ' +
        AMenuItem.Parent.Name + ' with ButtonID = AMenuItem.Parent.Command = ' + IntToStr(AMenuItem.Parent.Command));
      {$endif}
      if not SetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command, False, @MenuInfo) then
        DebugLn('[TWinCEWSMenuItem.AttachMenuEx] SetMenuItemInfo failed');
    end;
  end;
{  else if (AMenuItem.Parent.Parent = nil) and
    (Application.ApplicationType = atKeyPadDevice) then
  begin
    {$ifdef VerboseWinCEMenu}
    DebugLn('[TWinCEWSMenuItem.AttachMenuEx] Exiting from initial AttachMenuEx');
    {$endif}
    Exit;
  end;}

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

  // Never allow the use of the value 201 and 202 under atKeyPadDevice
  // Because they may colide with the ids of the fixed menus
{  if Application.ApplicationType = atKeyPadDevice then
  begin
    if (cmd = 201) then
    begin
      TMenuItemAccess(AMenuItem).FCommand := 2001;
      cmd := 2201;
    end;
    if (cmd = 202) then
    begin
      TMenuItemAccess(AMenuItem).FCommand := 2002;
      cmd := 2202;
    end;
  end;}

  wCaption := UTF8Decode(AmenuItem.Caption);
  Index := AMenuItem.Parent.VisibleIndexOf(AMenuItem);

  {$ifdef VerboseWinCEMenu}
  DebugLn('[TWinCEWSMenuItem.AttachMenuEx] InsertMenuW item = ', AMenuItem.Name, ' cmd = ', IntToStr(cmd));
  {$endif}
  if not InsertMenuW(AParentHandle, Index, fState, cmd, PWideChar(wCaption)) then
    DebugLn('[TWinCEWSMenuItem.AttachMenuEx] InsertMenuW failed for ', dbgsName(AMenuItem), ' : ', GetLastErrorText(GetLastError));

  MenuInfo.cbSize := SizeOf(MenuInfo);
  MenuInfo.fMask := MIIM_DATA;
  //GetMenuItemInfo(ParentMenuHandle, Index, True, @MenuInfo);
  MenuInfo.dwItemData := PtrInt(AMenuItem);
  //MenuInfo.wID := AMenuItem.Command;
  {$ifdef VerboseWinCEMenu}
  DebugLn('[TWinCEWSMenuItem.AttachMenuEx] SetMenuItemInfoW Index = ' + IntToStr(Index));
  {$endif}
  if not SetMenuItemInfoW(AParentHandle, Index, True, @MenuInfo) then
    DebugLn('[TWinCEWSMenuItem.AttachMenuEx] SetMenuItemInfoW failed for ', dbgsName(AMenuItem), ' : ', GetLastErrorText(GetLastError));

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
//  DebugLn(Format('[TWinCEWSMenuItem.CreateHandle] Name:%s Parent:%d Parent:%s Items:%d',
//    [AMenuItem.Name, Integer(AMenuItem.Parent), AMenuItem.Parent.Name,
//    Integer(AMenuItem.GetParentMenu.Items)]));
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
{$ifndef Win32}
var
  bi: TBBUTTONINFO;
  w: WideString;
  h: THandle;
  i, j, MenuBarRLID: Integer;
  FormFound: Boolean;
  AMenu: TMenu;
{$endif}
begin
  // The code to set top-level menus is different then ordinary items under WinCE
  {$ifndef Win32}
  AMenu := AMenuItem.GetParentMenu;
//  DebugLn(Format('[TWinCEWSMenuItem.SetCaption] A AItem.Menu:%d', [PtrInt(AMenu)]));

  // Top-Level menu items for PDA systems
  if (Application.ApplicationType in [atPDA, atKeyPadDevice]) and
    (AMenu <> nil) and (AMenu is TMainMenu) and
    (AMenuItem.Parent = AMenu.Items) then
  begin
//    DebugLn('[TWinCEWSMenuItem.SetCaption] B');

    // Iterate through all forms to find the parent
    FormFound := False;
    for i := 0 to Screen.FormCount - 1 do
      if Screen.Forms[i].Menu = AMenu then
      begin
        h := SHFindMenuBar(Screen.Forms[i].Handle);
        FormFound := True;
        Break;
      end;

//    DebugLn('[TWinCEWSMenuItem.SetCaption] C');
    if not FormFound then Exit;
//    DebugLn('[TWinCEWSMenuItem.SetCaption] D');

    FillChar(bi, SizeOf(TBBUTTONINFO), 0);
    bi.cbSize := SizeOf(TBBUTTONINFO);
    bi.dwMask := TBIF_TEXT;
    w := UTF8Decode(ACaption);
    bi.pszText := PWideChar(w);

    // Under Windows the Command numbering is different
    // Here we need to find the position of the button
    // in the list of visible buttons and use MenuBarID_L or _R
    if (Application.ApplicationType = atKeyPadDevice) then
    begin
      i := 0; // Counts only really visible menus
      for j := 0 to AMenu.Items.Count - 1 do
      begin
        if AMenuItem = AMenu.Items.Items[j] then Break;
        if AMenu.Items.Items[j].Visible then Inc(i);
      end;

      if i = 0 then MenuBarRLID := StartMenuItem + MenuBarID_L
      else MenuBarRLID := StartMenuItem + MenuBarID_R;

      {$ifdef VerboseWinCEMenu}
      DebugLn('[TWinCEWSMenuItem.SetCaption] TB_SETBUTTONINFO with ButtonID: ' + IntToStr(MenuBarRLID));
      {$endif}
      SendMessageW(h, TB_SETBUTTONINFO, MenuBarRLID, LPARAM(@bi));
    end
    else
    begin
      {$ifdef VerboseWinCEMenu}
      DebugLn('[TWinCEWSMenuItem.SetCaption] TB_SETBUTTONINFO with ButtonID: ' + IntToStr(AMenuItem.Command));
      {$endif}
      SendMessageW(h, TB_SETBUTTONINFO, AMenuItem.Command, LPARAM(@bi));
    end;
  end
  else
  {$endif}
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
  TWinCEWSMenuItem.SetCaption(AMenuItem, aMenuItem.Caption);
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
  lAlignment: array[TPopupAlignment, Boolean] of DWORD = (
              { left-to-rght } { right-to-left }
 { paLeft   } (TPM_LEFTALIGN,   TPM_RIGHTALIGN),
 { paRight  } (TPM_RIGHTALIGN,  TPM_LEFTALIGN),
 { paCenter } (TPM_CENTERALIGN, TPM_CENTERALIGN)
  );
  lTrackButtons: array[TTrackButton] of DWORD = (
 { tbRightButton } TPM_RIGHTBUTTON,
 { tbLeftButton  } TPM_LEFTBUTTON
  );
begin
  MenuHandle := APopupMenu.Handle;
  AppHandle := TWinCEWidgetSet(WidgetSet).AppHandle;
  GetWindowInfo(AppHandle)^.PopupMenu := APopupMenu;
  TrackPopupMenuEx(MenuHandle,
    lAlignment[APopupMenu.Alignment, APopupMenu.IsRightToLeft] or lTrackButtons[APopupMenu.TrackButton],
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
