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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Graphics, GraphType, ImgList, Menus, Forms,
////////////////////////////////////////////////////
  WSMenus, WSLCLClasses, WSProc,
  Windows, Controls, Classes, SysUtils, Win32Int, Win32Proc, Win32WSImgList,
  InterfaceBase, LCLProc, Themes, Win32UxTheme, TmSchema, Win32Themes, Win32Extra,
  FileUtil;

type

  { TWin32WSMenuItem }

  TWin32WSMenuItem = class(TWSMenuItem)
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

  { TWin32WSMenu }

  TWin32WSMenu = class(TWSMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure SetBiDiMode(const AMenu: TMenu; UseRightToLeftAlign, UseRightToLeftReading : Boolean); override;
  end;

  { TWin32WSMainMenu }

  TWin32WSMainMenu = class(TWSMainMenu)
  published
  end;

  { TWin32WSPopupMenu }

  TWin32WSPopupMenu = class(TWSPopupMenu)
  published
    class function CreateHandle(const AMenu: TMenu): HMENU; override;
    class procedure Popup(const APopupMenu: TPopupMenu; const X, Y: integer); override;
  end;

  function MenuItemSize(AMenuItem: TMenuItem; AHDC: HDC): TSize;
  procedure DrawMenuItem(const AMenuItem: TMenuItem; const AHDC: HDC; const ARect: Windows.RECT; const ItemAction, ItemState: UINT);
  function FindMenuItemAccelerator(const ACharCode: char; const AMenuHandle: HMENU): integer;
  procedure DrawMenuItemIcon(const AMenuItem: TMenuItem; const AHDC: HDC;
    const ImageRect: TRect; const ASelected: Boolean);

implementation

uses strutils;

{ helper routines }

const
  SpaceBetweenIcons = 5;

  // define the size of the MENUITEMINFO structure used by older Windows
  // versions (95, NT4) to keep the compatibility with them
  // Since W98 the size is 48 (hbmpItem was added)
  W95_MENUITEMINFO_SIZE = 44;

  EnabledToStateFlag: array[Boolean] of DWord =
  (
    MF_GRAYED,
    MF_ENABLED
  );

  PopupItemStates: array[{ Enabled } Boolean, { Selected } Boolean] of TThemedMenu =
  (
    (tmPopupItemDisabled, tmPopupItemDisabledHot),
    (tmPopupItemNormal, tmPopupItemHot)
  );

  PopupCheckBgStates: array[{ Enabled } Boolean] of TThemedMenu =
  (
    tmPopupCheckBackgroundDisabled,
    tmPopupCheckBackgroundNormal
  );

  PopupCheckStates: array[{ Enabled } Boolean, { RadioItem } Boolean] of TThemedMenu =
  (
    (tmPopupCheckMarkDisabled, tmPopupBulletDisabled),
    (tmPopupCheckMarkNormal,  tmPopupBulletNormal)
  );

  PopupSubmenuStates: array[{ Enabled } Boolean] of TThemedMenu =
  (
    tmPopupSubmenuDisabled,
    tmPopupSubmenuNormal
  );

var
  menuiteminfosize : DWORD = 0;

type
  TCaptionFlags = (cfBold, cfUnderline);
  TCaptionFlagsSet = set of TCaptionFlags;

  // metrics for vista drawing
  TVistaPopupMenuMetrics = record
    ItemMargins: TMargins;
    CheckSize: TSize;
    CheckMargins: TMargins;
    CheckBgMargins: TMargins;
    GutterSize: TSize;
    SubMenuSize: TSize;
    SubMenuMargins: TMargins;
    TextSize: TSize;
    TextMargins: TMargins;
    ShortCustSize: TSize;
    SeparatorSize: TSize;
  end;

  TVistaBarMenuMetrics = record
    ItemMargins: TMargins;
    TextSize: TSize;
  end;

function GetLastErrorReport: AnsiString;
begin
  Result := IntToStr(GetLastError) + ' : ' + UTF8ToConsole(AnsiToUtf8(GetLastErrorText(GetLastError)));
end;

(* Returns index of the character in the menu item caption that is displayed
   as underlined and is therefore the hot key of the menu item.
   If the caption does not contain any underlined character, 0 is returned.
   If there are more "underscored" characters in the caption, the last one is returned.
   Does some Windows API function exists which can do the same?
   AnUnderlinedChar - character which tells that tne following character should be underlined
   ACaption - menu item caption which is parsed *)
function SearchMenuItemHotKeyIndex(const AnUnderlinedChar: char; ACaption: string): integer;
var
  position: integer;
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
  SiblingMenuItem: TMenuItem;
  HotKeyIndex: integer;
  i: integer;
begin
  Result := MakeLResult(0, MNC_IGNORE);
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
    if (HotKeyIndex > 0) and (Upcase(ACharCode) = Upcase(SiblingMenuItem.Caption[HotKeyIndex])) then
      MenuItemIndex := SiblingMenuItem.MenuVisibleIndex;
    inc(i);
  end;
  if MenuItemIndex > -1 then
    Result := MakeLResult(MenuItemIndex, MNC_EXECUTE);
end;

function GetMenuItemFont(const AFlags: TCaptionFlagsSet): HFONT;
var 
  lf: LOGFONT;
  ncm: NONCLIENTMETRICS;
begin
  ncm.cbSize := sizeof(ncm);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(ncm), @ncm, 0) then
    lf := ncm.lfMenuFont
  else
    GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(LOGFONT), @lf);
  if cfUnderline in AFlags then
    lf.lfUnderline := 1
  else
    lf.lfUnderline := 0;
  if cfBold in AFlags then
  begin
    if lf.lfWeight <= 400 then
      lf.lfWeight := lf.lfWeight + 300
    else
      lf.lfWeight := lf.lfWeight + 100;
  end;
  Result := CreateFontIndirect(@lf);
end;

(* Get the menu item caption including shortcut *)
function CompleteMenuItemCaption(const AMenuItem: TMenuItem; Spacing: String): string;
begin
  Result := AMenuItem.Caption;
  if AMenuItem.ShortCut <> scNone then
    Result := Result + Spacing + ShortCutToText(AMenuItem.ShortCut);
end;

(* Get the maximum length of the given string in pixels *)
function StringSize(const aCaption: String; const aHDC: HDC; const aDecoration:TCaptionFlagsSet): TSize;
var
  oldFont: HFONT;
  newFont: HFONT;
  tmpRect: Windows.RECT;
{$ifdef WindowsUnicodeSupport}
  AnsiBuffer: ansistring;
  WideBuffer: widestring;
{$endif WindowsUnicodeSupport}
begin
  FillChar(tmpRect, SizeOf(tmpRect), 0);
  newFont := GetMenuItemFont(aDecoration);
  oldFont := SelectObject(aHDC, newFont);
{$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
  begin
    WideBuffer := UTF8ToUTF16(aCaption);
    DrawTextW(aHDC, PWideChar(WideBuffer), length(WideBuffer), @TmpRect, DT_CALCRECT);
  end
  else
  begin
    AnsiBuffer := Utf8ToAnsi(aCaption);
    DrawText(aHDC, pChar(AnsiBuffer), length(AnsiBuffer), @TmpRect, DT_CALCRECT);
  end;
{$else}
  DrawText(aHDC, pChar(aCaption), length(aCaption), @TmpRect, DT_CALCRECT);
{$endif}
  SelectObject(aHDC, oldFont);
  DeleteObject(newFont);
  Result.cx := TmpRect.right - TmpRect.left;
  Result.cy := TmpRect.Bottom - TmpRect.Top;
end;
  
function CheckSpace(AMenuItem: TMenuItem): integer;
var
  i: integer;
begin
  Result := 0;
  if AMenuItem.IsInMenuBar then
  begin
    if AMenuItem.Checked then
      Result := GetSystemMetrics(SM_CXMENUCHECK);
  end
  else
  begin
    for i := 0 to AMenuItem.Parent.Count - 1 do
    begin
      if AMenuItem.Parent.Items[i].Checked then
      begin
        Result := GetSystemMetrics(SM_CXMENUCHECK);
        break;
      end;
    end;
  end;
end;

function MenuIconWidth(const AMenuItem: TMenuItem): integer;
var
  SiblingMenuItem : TMenuItem;
  i, RequiredWidth: integer;
begin
  Result := 0;

  if AMenuItem.IsInMenuBar then
  begin
    Result := AMenuItem.GetIconSize.x;
  end
  else
  begin
    for i := 0 to AMenuItem.Parent.Count - 1 do
    begin
      SiblingMenuItem := AMenuItem.Parent.Items[i];
      if SiblingMenuItem.HasIcon then
      begin
        RequiredWidth := SiblingMenuItem.GetIconSize.x;
        if RequiredWidth > Result then
          Result := RequiredWidth;
      end;
    end;
  end;
end;

function LeftCaptionPosition(const AMenuItem: TMenuItem): integer;
var
  ImageWidth: Integer;
begin
  // If we have Check and Icon then we use only width of Icon
  // we draw our MenuItem so: space Image space Caption
  ImageWidth := MenuIconWidth(AMenuItem);
  if ImageWidth = 0 then
    ImageWidth := CheckSpace(aMenuItem);

  Result := SpaceBetweenIcons;

  inc(Result, ImageWidth);

  if not aMenuItem.IsInMenuBar or (ImageWidth <> 0) then
    inc(Result, SpaceBetweenIcons);
end;

function TopPosition(const aMenuItemHeight: integer; const anElementHeight: integer): integer;
begin
  Result := (aMenuItemHeight - anElementHeight) div 2;
end;

function IsVistaMenu: Boolean; inline;
begin
  Result := ThemeServices.ThemesAvailable and (WindowsVersion >= wvVista) and
     (TWin32ThemeServices(ThemeServices).Theme[teMenu] <> 0);
end;

function GetVistaPopupMenuMetrics(const AMenuItem: TMenuItem; DC: HDC): TVistaPopupMenuMetrics;
var
  Theme: HTHEME;
  TextRect: TRect;
  W: WideString;
  AFont, OldFont: HFONT;
begin
  Theme := TWin32ThemeServices(ThemeServices).Theme[teMenu];
  GetThemeMargins(Theme, DC, MENU_POPUPITEM, 0, TMT_CONTENTMARGINS, nil, Result.ItemMargins);
  GetThemePartSize(Theme, DC, MENU_POPUPCHECK, 0, nil, TS_TRUE, Result.CheckSize);
  GetThemeMargins(Theme, DC, MENU_POPUPCHECK, 0, TMT_CONTENTMARGINS, nil, Result.CheckMargins);
  GetThemeMargins(Theme, DC, MENU_POPUPCHECKBACKGROUND, 0, TMT_CONTENTMARGINS, nil, Result.CheckBgMargins);
  GetThemePartSize(Theme, DC, MENU_POPUPGUTTER, 0, nil, TS_TRUE, Result.GutterSize);
  GetThemePartSize(Theme, DC, MENU_POPUPSUBMENU, 0, nil, TS_TRUE, Result.SubMenuSize);
  GetThemeMargins(Theme, DC, MENU_POPUPSUBMENU, 0, TMT_CONTENTMARGINS, nil, Result.SubMenuMargins);

  if AMenuItem.IsLine then
  begin
    GetThemePartSize(Theme, DC, MENU_POPUPSEPARATOR, 0, nil, TS_TRUE, Result.SeparatorSize);
    FillChar(Result.TextMargins, SizeOf(Result.TextMargins), 0);
    FillChar(Result.TextSize, SizeOf(Result.TextSize), 0);
  end
  else
  begin
    Result.TextMargins := Result.ItemMargins;
    GetThemeInt(Theme, MENU_POPUPITEM, 0, TMT_BORDERSIZE, Result.TextMargins.cxRightWidth);
    GetThemeInt(Theme, MENU_POPUPBACKGROUND, 0, TMT_BORDERSIZE, Result.TextMargins.cxLeftWidth);

    if AMenuItem.Default then
      AFont := GetMenuItemFont([cfBold])
    else
      AFont := GetMenuItemFont([]);
    OldFont := SelectObject(DC, AFont);

    W := UTF8ToUTF16(CompleteMenuItemCaption(AMenuItem, #9));
    GetThemeTextExtent(Theme, DC, MENU_POPUPITEM, 0, PWideChar(W), Length(W),
      DT_SINGLELINE or DT_LEFT or DT_EXPANDTABS, nil, TextRect);
    Result.TextSize.cx := TextRect.Right - TextRect.Left;
    Result.TextSize.cy := TextRect.Bottom - TextRect.Top;

    if AMenuItem.ShortCut <> scNone then
    begin;
      W := UTF8ToUTF16(ShortCutToText(AMenuItem.ShortCut));
      GetThemeTextExtent(Theme, DC, MENU_POPUPITEM, 0, PWideChar(W), Length(W),
        DT_SINGLELINE or DT_LEFT, nil, TextRect);
      Result.ShortCustSize.cx := TextRect.Right - TextRect.Left;
      Result.ShortCustSize.cy := TextRect.Bottom - TextRect.Top;
    end;
    if OldFont <> 0 then
      DeleteObject(SelectObject(DC, OldFont));
  end;
end;

function GetVistaBarMenuMetrics(const AMenuItem: TMenuItem; DC: HDC): TVistaBarMenuMetrics;
var
  Theme: HTHEME;
  TextRect: TRect;
  W: WideString;
  AFont, OldFont: HFONT;
begin
  Theme := TWin32ThemeServices(ThemeServices).Theme[teMenu];
  GetThemeMargins(Theme, 0, MENU_BARITEM, 0, TMT_CONTENTMARGINS, nil, Result.ItemMargins);

  if AMenuItem.Default then
    AFont := GetMenuItemFont([cfBold])
  else
    AFont := GetMenuItemFont([]);

  OldFont := SelectObject(DC, AFont);

  W := UTF8ToUTF16(AMenuItem.Caption);
  GetThemeTextExtent(Theme, DC, MENU_BARITEM, 0, PWideChar(W), Length(W),
    DT_SINGLELINE or DT_LEFT or DT_EXPANDTABS, nil, TextRect);
  Result.TextSize.cx := TextRect.Right - TextRect.Left;
  Result.TextSize.cy := TextRect.Bottom - TextRect.Top;
  if OldFont <> 0 then
    DeleteObject(SelectObject(DC, OldFont));
end;

function VistaBarMenuItemSize(AMenuItem: TMenuItem; ADC: HDC): TSize;
var
  Metrics: TVistaBarMenuMetrics;
  IconSize: TPoint;
begin
  Metrics := GetVistaBarMenuMetrics(AMenuItem, ADC);
  // item margins. Seems windows adds that margins itself to our return values
  Result.cx := 0; //Metrics.ItemMargins.cxLeftWidth + Metrics.ItemMargins.cxRightWidth;
  Result.cy := 0; //Metrics.ItemMargins.cyTopHeight + Metrics.ItemMargins.cyBottomHeight;
  // + text size / icon size
  IconSize := AMenuItem.GetIconSize;
  Result.cx := Result.cx + Metrics.TextSize.cx + IconSize.x;
  if IconSize.x > 0 then
    inc(Result.cx, Metrics.ItemMargins.cxLeftWidth);
  Result.cy := Result.cy + Max(Metrics.TextSize.cy, IconSize.y);
end;

function VistaPopupMenuItemSize(AMenuItem: TMenuItem; ADC: HDC): TSize;
var
  Metrics: TVistaPopupMenuMetrics;
begin
  Metrics := GetVistaPopupMenuMetrics(AMenuItem, ADC);
  // count check
  Result.cx := Metrics.CheckSize.cx + Metrics.CheckMargins.cxRightWidth + Metrics.CheckMargins.cxLeftWidth;
  if AMenuItem.IsLine then
  begin
    Result.cx := Result.cx + Metrics.SeparatorSize.cx + Metrics.ItemMargins.cxLeftWidth + Metrics.ItemMargins.cxRightWidth;
    Result.cy := Metrics.SeparatorSize.cy + Metrics.ItemMargins.cyTopHeight + Metrics.ItemMargins.cyBottomHeight;
  end
  else
  begin
    Result.cy := Metrics.CheckSize.cy + ScaleY(Metrics.CheckMargins.cyTopHeight + Metrics.CheckMargins.cyBottomHeight, 96);
    if AMenuItem.HasIcon then
    begin
      Result.cy := Max(Result.cy, AMenuItem.GetIconSize.y);
      Result.cx := Max(Result.cx, AMenuItem.GetIconSize.x);
    end;
  end;
  // count gutter
  Result.cx := Result.cx + (Metrics.CheckBgMargins.cxRightWidth - Metrics.CheckMargins.cxRightWidth) +
               Metrics.GutterSize.cx;
  // count text
  Result.cx := Result.cx + Metrics.TextSize.cx;
  Result.cx := Result.cx + Metrics.TextMargins.cxLeftWidth + Metrics.TextMargins.cxRightWidth;
end;

procedure ThemeDrawElement(DC: HDC; Details: TThemedElementDetails; const R: TRect; ClipRect: PRect); inline;
begin
  with Details do
    DrawThemeBackground(TWin32ThemeServices(ThemeServices).Theme[Element], DC, Part, State, R, ClipRect);
end;

procedure ThemeDrawText(DC: HDC; Details: TThemedElementDetails;
  const S: String; R: TRect; Flags, Flags2: Cardinal);
var
  w: widestring;
begin
  with Details do
  begin
    w := UTF8ToUTF16(S);
    DrawThemeText(TWin32ThemeServices(ThemeServices).Theme[Element], DC, Part, State, PWideChar(w), Length(w), Flags, Flags2, R);
  end;
end;

procedure DrawVistaMenuBar(const AMenuItem: TMenuItem; const AHDC: HDC; const ARect: TRect; const ASelected, ANoAccel: Boolean; const ItemAction, ItemState: UINT);
const
  BarState: array[Boolean] of TThemedMenu =
  (
    tmBarBackgroundInactive,
    tmBarBackgroundActive
  );
  OBJID_MENU = LONG($FFFFFFFD);

  function IsLast: Boolean;
  var
    Index, i: Integer;
  begin
    Index := AMenuItem.Parent.IndexOf(AMenuItem);
    for i := Index + 1 to AMenuItem.Parent.Count - 1 do
      if AMenuItem.Parent.Items[i].Visible then
        Exit(False);
    Result := True;
  end;
var
  MenuState: TThemedMenu;
  Metrics: TVistaBarMenuMetrics;
  Details, Tmp: TThemedElementDetails;
  BGRect, BGClip, WndRect, TextRect, ImageRect: TRect;
  IconSize: TPoint;
  TextFlags: DWord;
  AFont, OldFont: HFONT;
  IsRightToLeft: Boolean;
  Info: tagMENUBARINFO;
  AWnd: HWND;
begin
  if (ItemState and ODS_SELECTED) <> 0 then
    MenuState := tmBarItemPushed
  else
  if (ItemState and ODS_HOTLIGHT) <> 0 then
    MenuState := tmBarItemHot
  else
    MenuState := tmBarItemNormal;

  if (ItemState and (ODS_DISABLED or ODS_INACTIVE)) <> 0 then
    inc(MenuState, 3);

  IsRightToLeft := AMenuItem.GetIsRightToLeft;
  Metrics := GetVistaBarMenuMetrics(AMenuItem, AHDC);

  // draw backgound
  // This is a hackish way to draw. Seems windows itself draws this in WM_PAINT or another paint handler?
  AWnd := TCustomForm(AMenuItem.GetParentMenu.Parent).Handle;
  if (AMenuItem.Parent.VisibleIndexOf(AMenuItem) = 0) then
  begin
    /// if we are painting the first item then request full repaint to draw the bg correctly
    if (GetProp(AWnd, 'LCL_MENUREDRAW') = 0) then
    begin
      SetProp(AWnd, 'LCL_MENUREDRAW', 1);
      DrawMenuBar(AWnd);
      Exit;
    end
    else
      SetProp(AWnd, 'LCL_MENUREDRAW', 0);
    // repainting menu bar bg
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);
    GetMenuBarInfo(AWnd, OBJID_MENU, 0, @Info);
    GetWindowRect(AWnd, @WndRect);
    OffsetRect(Info.rcBar, -WndRect.Left, -WndRect.Top);
    Tmp := ThemeServices.GetElementDetails(BarState[(ItemState and ODS_INACTIVE) = 0]);
    ThemeDrawElement(AHDC, Tmp, Info.rcBar, nil);
  end;

  BGRect := ARect;
  BGClip := ARect;
  if IsRightToLeft <> AMenuItem.RightJustify then
  begin
    inc(BGRect.Right, 2);
    dec(BGRect.Left, 2);
  end
  else
  begin
    inc(BGRect.Right, 2);
    dec(BGRect.Left, 2);
  end;
  Tmp := ThemeServices.GetElementDetails(BarState[(ItemState and ODS_INACTIVE) = 0]);
  ThemeDrawElement(AHDC, Tmp, BGRect, @BGClip);

  Details := ThemeServices.GetElementDetails(MenuState);
  // draw menu item
  ThemeDrawElement(AHDC, Details, ARect, nil);

  TextRect := ARect;
  inc(TextRect.Left, Metrics.ItemMargins.cxLeftWidth);
  dec(TextRect.Right, Metrics.ItemMargins.cxRightWidth);
  inc(TextRect.Top, Metrics.ItemMargins.cyTopHeight);
  dec(TextRect.Bottom, Metrics.ItemMargins.cyBottomHeight);
  // draw check/image
  if AMenuItem.HasIcon then
  begin
    IconSize := AMenuItem.GetIconSize;
    if IsRightToLeft then
      ImageRect.Left := TextRect.Right - IconSize.x
    else
      ImageRect.Left := TextRect.Left;
    ImageRect.Top := (TextRect.Top + TextRect.Bottom - IconSize.y) div 2;
    ImageRect.Right := 0;
    ImageRect.Bottom := 0;
    DrawMenuItemIcon(AMenuItem, AHDC, ImageRect, ASelected);
    if IsRightToLeft then
      dec(TextRect.Right, IconSize.x + Metrics.ItemMargins.cxLeftWidth)
    else
      inc(TextRect.Left, IconSize.x + Metrics.ItemMargins.cxLeftWidth);
  end;

  // draw text
  TextRect.Top := (TextRect.Top + TextRect.Bottom - Metrics.TextSize.cy) div 2;
  TextRect.Bottom := TextRect.Top + Metrics.TextSize.cy;
  TextFlags := DT_SINGLELINE or DT_EXPANDTABS;
  if IsRightToLeft then
    TextFlags := TextFlags or DT_RTLREADING;
  if ANoAccel then
    TextFlags := TextFlags or DT_HIDEPREFIX;
  if AMenuItem.Default then
    AFont := GetMenuItemFont([cfBold])
  else
    AFont := GetMenuItemFont([]);
  OldFont := SelectObject(AHDC, AFont);
  ThemeDrawText(AHDC, Details, AMenuItem.Caption, TextRect, TextFlags, 0);
  if OldFont <> 0 then
    DeleteObject(SelectObject(AHDC, OldFont));
end;

procedure DrawVistaPopupMenu(const AMenuItem: TMenuItem; const AHDC: HDC; const ARect: TRect; const ASelected, ANoAccel: boolean);
var
  Details, Tmp: TThemedElementDetails;
  Metrics: TVistaPopupMenuMetrics;
  CheckRect, GutterRect, TextRect, SeparatorRect, ImageRect, SubMenuRect: TRect;
  IconSize: TPoint;
  TextFlags: DWord;
  AFont, OldFont: HFONT;
  IsRightToLeft: Boolean;
begin
  Metrics := GetVistaPopupMenuMetrics(AMenuItem, AHDC);
  // draw backgound
  Details := ThemeServices.GetElementDetails(PopupItemStates[AMenuItem.Enabled, ASelected]);
  if ThemeServices.HasTransparentParts(Details) then
  begin
    Tmp := ThemeServices.GetElementDetails(tmPopupBackground);
    ThemeDrawElement(AHDC, Tmp, ARect, nil);
  end;
  IsRightToLeft := AMenuItem.GetIsRightToLeft;
  if IsRightToLeft then
    SetLayout(AHDC, LAYOUT_RTL);
  // calc check/image rect
  CheckRect := ARect;
  CheckRect.Right := CheckRect.Left + Metrics.CheckSize.cx + Metrics.CheckMargins.cxRightWidth + Metrics.CheckMargins.cxLeftWidth;
  CheckRect.Bottom := CheckRect.Top + Metrics.CheckSize.cy + ScaleY(Metrics.CheckMargins.cyTopHeight + Metrics.CheckMargins.cyBottomHeight, 96);
  // draw gutter
  GutterRect := CheckRect;
  GutterRect.Left := GutterRect.Right + Metrics.CheckBgMargins.cxRightWidth - Metrics.CheckMargins.cxRightWidth;
  GutterRect.Right := GutterRect.Left + Metrics.GutterSize.cx;
  Tmp := ThemeServices.GetElementDetails(tmPopupGutter);
  ThemeDrawElement(AHDC, Tmp, GutterRect, nil);

  if AMenuItem.IsLine then
  begin
    // draw separator
    SeparatorRect.Left := GutterRect.Right + Metrics.ItemMargins.cxLeftWidth;
    SeparatorRect.Right := ARect.Right - Metrics.ItemMargins.cxRightWidth;
    SeparatorRect.Top := ARect.Top + Metrics.ItemMargins.cyTopHeight;
    SeparatorRect.Bottom := ARect.Bottom - Metrics.ItemMargins.cyBottomHeight;
    Tmp := ThemeServices.GetElementDetails(tmPopupSeparator);
    ThemeDrawElement(AHDC, Tmp, SeparatorRect, nil);
  end
  else
  begin
    // draw menu item
    ThemeDrawElement(AHDC, Details, ARect, nil);
    // draw submenu
    if AMenuItem.Count > 0 then
    begin
      SubMenuRect := ARect;
      SubMenuRect.Right := SubMenuRect.Right - Metrics.SubMenuMargins.cxRightWidth + Metrics.SubMenuMargins.cxLeftWidth;
      SubMenuRect.Left := SubMenuRect.Right - Metrics.SubMenuSize.cx;
      SubMenuRect.Top := SubMenuRect.Top + Metrics.ItemMargins.cyTopHeight;
      SubMenuRect.Bottom := SubMenuRect.Bottom - Metrics.ItemMargins.cyBottomHeight;
      Tmp := ThemeServices.GetElementDetails(PopupSubmenuStates[AMenuItem.Enabled]);
      Tmp.State := Tmp.State + 2;
      ThemeDrawElement(AHDC, Tmp, SubMenuRect, nil);
    end;
    // draw check/image
    if AMenuItem.HasIcon then
    begin
      ImageRect := CheckRect;
      IconSize := AMenuItem.GetIconSize;
      ImageRect.Left := (ImageRect.Left + ImageRect.Right - IconSize.x) div 2;
      ImageRect.Top := (ImageRect.Top + ImageRect.Bottom - IconSize.y) div 2;
      if IsRightToLeft then
      begin
        // we can't use RTL layout here since our imagelist does not support
        // coordinates mirroring
        SetLayout(AHDC, 0);
        ImageRect.Left := ARect.Right - ImageRect.Left - IconSize.x;
      end;
      ImageRect.Right := IconSize.x;
      ImageRect.Bottom := IconSize.y;
      DrawMenuItemIcon(AMenuItem, AHDC, ImageRect, ASelected);
      if IsRightToLeft then
        SetLayout(AHDC, LAYOUT_RTL);
    end
    else
    if AMenuItem.Checked then
    begin
      Tmp := ThemeServices.GetElementDetails(PopupCheckBgStates[AMenuItem.Enabled]);
      ThemeDrawElement(AHDC, Tmp, CheckRect, nil);
      Tmp := ThemeServices.GetElementDetails(PopupCheckStates[AMenuItem.Enabled, AMenuItem.RadioItem]);
      ThemeDrawElement(AHDC, Tmp, CheckRect, nil);
    end;
    // draw text
    TextFlags := DT_SINGLELINE or DT_EXPANDTABS;
    // todo: distinct UseRightToLeftAlignment and UseRightToLeftReading
    if IsRightToLeft then
    begin
      // restore layout before the text drawing since windows has bug with
      // DT_RTLREADING support
      SetLayout(AHDC, 0);
      TextFlags := TextFlags or DT_RIGHT or DT_RTLREADING;
      TextRect.Right := ARect.Right - GutterRect.Right - Metrics.TextMargins.cxLeftWidth;
      TextRect.Left := ARect.Left + Metrics.TextMargins.cxRightWidth;
      TextRect.Top := (GutterRect.Top + GutterRect.Bottom - Metrics.TextSize.cy) div 2;
      TextRect.Bottom := TextRect.Top + Metrics.TextSize.cy;
    end
    else
    begin
      TextFlags := TextFlags or DT_LEFT;
      TextRect := GutterRect;
      TextRect.Left := TextRect.Right + Metrics.TextMargins.cxLeftWidth;
      TextRect.Right := ARect.Right - Metrics.TextMargins.cxRightWidth;
      TextRect.Top := (TextRect.Top + TextRect.Bottom - Metrics.TextSize.cy) div 2;
      TextRect.Bottom := TextRect.Top + Metrics.TextSize.cy;
    end;

    if ANoAccel then
      TextFlags := TextFlags or DT_HIDEPREFIX;
    if AMenuItem.Default then
      AFont := GetMenuItemFont([cfBold])
    else
      AFont := GetMenuItemFont([]);
    OldFont := SelectObject(AHDC, AFont);

    ThemeDrawText(AHDC, Details, AMenuItem.Caption, TextRect, TextFlags, 0);
    if AMenuItem.ShortCut <> scNone then
    begin
      if IsRightToLeft then
      begin
        TextRect.Right := TextRect.Left + Metrics.ShortCustSize.cx;
        TextFlags := TextFlags xor DT_RIGHT or DT_LEFT;
      end
      else
      begin
        TextRect.Left := TextRect.Right - Metrics.ShortCustSize.cx;
        TextFlags := TextFlags xor DT_LEFT or DT_RIGHT;
      end;

      ThemeDrawText(AHDC, Details, ShortCutToText(AMenuItem.ShortCut), TextRect, TextFlags, 0);
    end;
    // exlude menu item rectangle to prevent drawing by windows after us
    if AMenuItem.Count > 0 then
      ExcludeClipRect(AHDC, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    if OldFont <> 0 then
      DeleteObject(SelectObject(AHDC, OldFont));
  end;
end;

function MenuItemSize(AMenuItem: TMenuItem; AHDC: HDC): TSize;
var
  decoration: TCaptionFlagsSet;
  minimumHeight: Integer;
begin
  if IsVistaMenu then
  begin
    if AMenuItem.IsInMenuBar then
      Result := VistaBarMenuItemSize(AMenuItem, AHDC)
    else
      Result := VistaPopupMenuItemSize(AMenuItem, AHDC);
    Exit;
  end;

  if AMenuItem.Default then
    decoration := [cfBold]
  else
    decoration := [];
    
  Result := StringSize(CompleteMenuItemCaption(AMenuItem, '  '), AHDC, decoration);
  inc(Result.cx, LeftCaptionPosition(AMenuItem));

  if not AMenuItem.IsInMenuBar then
    inc(Result.cx, SpaceBetweenIcons)
  else
    dec(Result.cx, SpaceBetweenIcons);

  if (AMenuItem.ShortCut <> scNone) then
    Inc(Result.cx, SpaceBetweenIcons);

  minimumHeight := GetSystemMetrics(SM_CYMENU);
  if not AMenuItem.IsInMenuBar then
    Dec(minimumHeight, 2);
  if AMenuItem.IsLine then
    Result.cy := 10 // it is a separator
  else
  begin
    if AMenuItem.hasIcon then
      Result.cy := Max(Result.cy, aMenuItem.GetIconSize.y);
    Inc(Result.cy, 2);
    if Result.cy < minimumHeight then
      Result.cy := minimumHeight;
  end;
end;

function IsFlatMenus: Boolean; inline;
var
  IsFlatMenu: Windows.BOOL;
begin
  Result := (WindowsVersion >= wvXP) and ((SystemParametersInfo(SPI_GETFLATMENU, 0, @IsFlatMenu, 0)) and IsFlatMenu);
end;

function BackgroundColorMenu(const ItemState: UINT; const aIsInMenuBar: boolean): COLORREF;
begin
  if IsFlatMenus then
  begin
    if (ItemState and (ODS_HOTLIGHT or ODS_SELECTED)) <> 0 then
     Result := GetSysColor(COLOR_MENUHILIGHT)
    else
   if aIsInMenuBar then
     Result := GetSysColor(COLOR_MENUBAR)
    else
      Result := GetSysColor(COLOR_MENU);
  end
  else
  begin
    // 3d menu bar always have standard color
    if aIsInMenuBar then
      Result := GetSysColor(COLOR_MENU)
    else
    if (ItemState and ODS_SELECTED) <> 0 then
      Result := GetSysColor(COLOR_HIGHLIGHT)
    else
      Result := GetSysColor(COLOR_MENU);
  end;
end;

function TextColorMenu(const ItemState: UINT; const aIsInMenuBar: boolean; const anEnabled: boolean): COLORREF;
begin
  if anEnabled then
  begin
    if IsFlatMenus then
    begin
      if (ItemState and (ODS_HOTLIGHT or ODS_SELECTED)) <> 0 then
        Result := GetSysColor(COLOR_HIGHLIGHTTEXT)
      else
        Result := GetSysColor(COLOR_MENUTEXT);
    end
    else
    begin
      if ((ItemState and ODS_SELECTED) <> 0) and not aIsInMenuBar then
        Result := GetSysColor(COLOR_HIGHLIGHTTEXT)
      else
        Result := GetSysColor(COLOR_MENUTEXT);
    end;
  end
  else
    Result := GetSysColor(COLOR_GRAYTEXT);
end;

procedure DrawSeparator(const AHDC: HDC; const ARect: Windows.RECT);
var
  separatorRect: Windows.RECT;
begin
  separatorRect.left := ARect.left;
  separatorRect.right := ARect.right;
  separatorRect.top := (ARect.top + ARect.bottom ) div 2 - 1;
  separatorRect.bottom := separatorRect.top + 2;
  DrawEdge(aHDC, separatorRect, BDR_SUNKENOUTER, BF_RECT);
end;

procedure DrawMenuItemCheckMark(const aMenuItem: TMenuItem; const aHDC: HDC; const aRect: Windows.RECT; const aSelected: boolean);
var
  checkMarkWidth: integer;
  checkMarkHeight: integer;
  hdcMem: HDC;
  monoBitmap: HBITMAP;
  oldBitmap: HBITMAP;
  checkMarkShape: integer;
  checkMarkRect: Windows.RECT;
  x:Integer;
begin
  hdcMem := CreateCompatibleDC(aHDC);
  checkMarkWidth := GetSystemMetrics(SM_CXMENUCHECK);
  checkMarkHeight := GetSystemMetrics(SM_CYMENUCHECK);
  monoBitmap := CreateBitmap(checkMarkWidth, checkMarkHeight, 1, 1, nil);
  oldBitmap := SelectObject(hdcMem, monoBitmap);
  checkMarkRect.left := 0;
  checkMarkRect.top := 0;
  checkMarkRect.right := checkMarkWidth;
  checkMarkRect.bottom := checkMarkHeight;
  if aMenuItem.RadioItem then
    checkMarkShape := DFCS_MENUBULLET
  else
    checkMarkShape := DFCS_MENUCHECK;
  DrawFrameControl(hdcMem, @checkMarkRect, DFC_MENU, checkMarkShape);
  if aMenuItem.GetIsRightToLeft then
    x := aRect.Right - checkMarkWidth - spaceBetweenIcons
  else
    x := aRect.left + spaceBetweenIcons;
  BitBlt(aHDC, x, aRect.top + topPosition(aRect.bottom - aRect.top, checkMarkRect.bottom - checkMarkRect.top), checkMarkWidth, checkMarkHeight, hdcMem, 0, 0, SRCCOPY);
  SelectObject(hdcMem, oldBitmap);
  DeleteObject(monoBitmap);
  DeleteDC(hdcMem);
end;

procedure DrawMenuItemText(const AMenuItem: TMenuItem; const AHDC: HDC;
  ARect: TRect; const ASelected, ANoAccel: boolean; ItemState: UINT);
var
  crText: COLORREF;
  crBkgnd: COLORREF;
  TmpHeight: integer;
  oldFont: HFONT;
  newFont: HFONT;
  decoration: TCaptionFlagsSet;
  shortCutText: string;
  WorkRect: Windows.RECT;
  IsRightToLeft: Boolean;
  etoFlags: Cardinal;
  dtFlags: DWord;
{$ifdef WindowsUnicodeSupport}
  AnsiBuffer: ansistring;
  WideBuffer: widestring;
{$endif WindowsUnicodeSupport}
begin
  crText := TextColorMenu(ItemState, AMenuItem.IsInMenuBar, AMenuItem.Enabled);
  crBkgnd := BackgroundColorMenu(ItemState, AMenuItem.IsInMenuBar);
  SetTextColor(AHDC, crText);
  SetBkColor(AHDC, crBkgnd);

  if AMenuItem.Default then
    decoration := [cfBold]
  else
    decoration := [];
    
  newFont := GetMenuItemFont(decoration);
  oldFont := SelectObject(AHDC, newFont);
  IsRightToLeft := AMenuItem.GetIsRightToLeft;

  etoFlags := ETO_OPAQUE;
  dtFlags := DT_EXPANDTABS;
  if ANoAccel then
    dtFlags := dtFlags or DT_HIDEPREFIX;
  if IsRightToLeft then
  begin
    etoFlags := etoFlags or ETO_RTLREADING;
    dtFlags := dtFlags or DT_RIGHT or DT_RTLREADING;
  end;

  // fill the menu item background
  ExtTextOut(AHDC, 0, 0, etoFlags, @ARect, PChar(''), 0, nil);

  if AMenuItem.IsInMenuBar and not IsFlatMenus then
  begin
    if (ItemState and ODS_SELECTED) <> 0 then
      DrawEdge(AHDC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST)
    else
    if (ItemState and ODS_HOTLIGHT) <> 0 then
      DrawEdge(AHDC, ARect, BDR_RAISEDINNER, BF_RECT);
  end;

  TmpHeight := ARect.Bottom - ARect.Top;

{$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
  begin
    WideBuffer := UTF8ToUTF16(AMenuItem.Caption);
    DrawTextW(AHDC, PWideChar(WideBuffer), length(WideBuffer), @WorkRect, DT_CALCRECT);
  end
  else
  begin
    AnsiBuffer := Utf8ToAnsi(AMenuItem.Caption);
    DrawText(AHDC, PChar(AnsiBuffer), length(AnsiBuffer), @WorkRect, DT_CALCRECT);
  end;
{$else}
  DrawText(AHDC, PChar(AMenuItem.Caption), length(AMenuItem.Caption), @WorkRect, DT_CALCRECT);
{$endif}

  if IsRightToLeft then
    Dec(ARect.Right, LeftCaptionPosition(AMenuItem))
  else
    Inc(ARect.Left, LeftCaptionPosition(AMenuItem));
  Inc(ARect.Top, TopPosition(TmpHeight, WorkRect.Bottom - WorkRect.Top));

{$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then
    DrawTextW(AHDC, PWideChar(WideBuffer), Length(WideBuffer), @ARect, dtFlags)
  else
    DrawText(AHDC, PChar(AnsiBuffer), Length(AnsiBuffer), @ARect, dtFlags);
{$else}
  DrawText(AHDC, PChar(AMenuItem.Caption), Length(AMenuItem.Caption), @ARect, dtFlags);
{$endif}

  if AMenuItem.ShortCut <> scNone then
  begin
    shortCutText := ShortCutToText(AMenuItem.ShortCut);
    if IsRightToLeft then
    begin
      Inc(ARect.Left, GetSystemMetrics(SM_CXMENUCHECK));
      dtFlags := DT_LEFT;
    end
    else
    begin
      Dec(ARect.Right, GetSystemMetrics(SM_CXMENUCHECK));
      dtFlags := DT_RIGHT;
    end;

    {$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        WideBuffer := UTF8ToUTF16(shortCutText);
        DrawTextW(AHDC, PWideChar(WideBuffer), Length(WideBuffer), @ARect, dtFlags);
      end
      else
      begin
        AnsiBuffer := Utf8ToAnsi(shortCutText);
        DrawText(AHDC, PChar(AnsiBuffer), Length(AnsiBuffer), @ARect, dtFlags);
      end;
    {$else}
      DrawText(AHDC, PChar(shortCutText), Length(shortCutText), @ARect, dtFlags);
    {$endif}
  end;
  SelectObject(AHDC, oldFont);
  DeleteObject(newFont);
end;

procedure DrawMenuItemIcon(const AMenuItem: TMenuItem; const AHDC: HDC;
  const ImageRect: TRect; const ASelected: Boolean);
var
  AEffect: TGraphicsDrawEffect;
  AImageList: TCustomImageList;
  FreeImageList: Boolean;
  AImageIndex: Integer;
begin
  AImageList := AMenuItem.GetImageList;
  if AImageList = nil then
  begin
    AImageList := TImageList.Create(nil);
    AImageList.Width := AMenuItem.Bitmap.Width; // maybe height to prevent too wide bitmaps?
    AImageList.Height := AMenuItem.Bitmap.Height;
    AImageIndex := AImageList.Add(AMenuItem.Bitmap, nil);
    FreeImageList := True;
  end
  else
  begin
    FreeImageList := False;
    AImageIndex := AMenuItem.ImageIndex;
  end;

  if not AMenuItem.Enabled then
    AEffect := gdeDisabled
  else
  if ASelected then
    AEffect := gdeHighlighted
  else
    AEffect := gdeNormal;

  if AImageIndex < AImageList.Count then
    TWin32WSCustomImageList.DrawToDC(AImageList, AImageIndex, AHDC,
      ImageRect, AImageList.BkColor, AImageList.BlendColor,
      AEffect, AImageList.DrawingStyle, AImageList.ImageType);
  if FreeImageList then
    AImageList.Free;
end;

procedure DrawClassicMenuItemIcon(const AMenuItem: TMenuItem; const AHDC: HDC;
  const ARect: TRect; const ASelected, AChecked: boolean);
var
  x: Integer;
  ImageRect: TRect;
  IconSize: TPoint;
begin
  IconSize := AMenuItem.GetIconSize;
  if AMenuItem.GetIsRightToLeft then
    x := ARect.Right - IconSize.x - spaceBetweenIcons
  else
    x := ARect.Left + spaceBetweenIcons;

  ImageRect := Rect(x, ARect.top + TopPosition(ARect.Bottom - ARect.Top, IconSize.y),
                    IconSize.x, IconSize.y);
      
  if AChecked then // draw rectangle around
  begin
    FrameRect(aHDC,
      Rect(ImageRect.Left - 1, ImageRect.Top - 1, ImageRect.Left + ImageRect.Right + 1, ImageRect.Top + ImageRect.Bottom + 1),
      GetSysColorBrush(COLOR_HIGHLIGHT));
  end;

  DrawMenuItemIcon(AMenuItem, AHDC, ImageRect, ASelected);
end;

procedure DrawMenuItem(const AMenuItem: TMenuItem; const AHDC: HDC; const ARect: Windows.RECT; const ItemAction, ItemState: UINT);
var
  ASelected, ANoAccel: Boolean;
  B: Bool;
begin
  ASelected := (ItemState and ODS_SELECTED) <> 0;
  ANoAccel := (ItemState and ODS_NOACCEL) <> 0;
  if ANoAccel and (WindowsVersion >= wv2000) then
    if SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @B, 0) then
      ANoAccel := not B
    else
  else
    ANoAccel := False;
  if IsVistaMenu then
  begin
    if AMenuItem.IsInMenuBar then
      DrawVistaMenuBar(AMenuItem, AHDC, ARect, ASelected, ANoAccel, ItemAction, ItemState)
    else
      DrawVistaPopupMenu(AMenuItem, AHDC, ARect, ASelected, ANoAccel);
    Exit;
  end;

  if aMenuItem.IsLine then
    DrawSeparator(AHDC, ARect)
  else
  begin
    DrawMenuItemText(AMenuItem, AHDC, ARect, ASelected, ANoAccel, ItemState);
    if aMenuItem.HasIcon then
      DrawClassicMenuItemIcon(AMenuItem, AHDC, ARect, ASelected, AMenuItem.Checked)
    else
    if AMenuItem.Checked then
      DrawMenuItemCheckMark(AMenuItem, AHDC, ARect, ASelected);
  end;
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

function ChangeMenuFlag(const AMenuItem: TMenuItem; Flag: Cardinal; Value: boolean): boolean;
var
  MenuInfo: MENUITEMINFO;
begin
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  MenuInfo.cbSize := menuiteminfosize;
  MenuInfo.fMask := MIIM_TYPE;
  GetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  if Value then
    MenuInfo.fType := MenuInfo.fType or Flag
  else
    MenuInfo.fType := MenuInfo.fType and (not Flag);
  MenuInfo.dwTypeData := LPSTR(AMenuItem.Caption);
  Result := SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  TriggerFormUpdate(AMenuItem);
end;

{------------------------------------------------------------------------------
  Method: SetMenuFlag
  Returns: Nothing

  Change the menu flags for handle of TMenuItem or TMenu,
  added for BidiMode Menus
 ------------------------------------------------------------------------------}
procedure SetMenuFlag(const Menu: HMenu; Flag: Cardinal; Value: boolean);
var
  MenuInfo: MENUITEMINFO;
begin
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  MenuInfo.cbSize := menuiteminfosize;
  MenuInfo.fMask := MIIM_TYPE;
  GetMenuItemInfo(Menu, 0, True, @MenuInfo);
  if Value then
    MenuInfo.fType := MenuInfo.fType or Flag
  else
    MenuInfo.fType := MenuInfo.fType and not Flag;
  SetMenuItemInfo(Menu, 0, True, @MenuInfo);
end;

{ TWin32WSMenuItem }

procedure UpdateCaption(const AMenuItem: TMenuItem; ACaption: String);
var
  MenuInfo: MENUITEMINFO;
begin
  if (AMenuItem.Parent = nil) or not AMenuItem.Parent.HandleAllocated then
    Exit;

  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  with MenuInfo do
  begin
    cbSize := menuiteminfosize;
    fMask := MIIM_TYPE or MIIM_STATE;
    dwTypeData := nil;  // don't retrieve caption
  end;
  GetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  with MenuInfo do
  begin
    // change enabled too since we can change from '-' to normal caption and vice versa
    if ACaption <> cLineCaption then
    begin
      fType := (fType or MFT_STRING) and not (MFT_SEPARATOR or MFT_OWNERDRAW);
      fState := EnabledToStateFlag[AMenuItem.Enabled];
      dwTypeData := LPSTR(ACaption);
      cch := StrLen(dwTypeData);
    end
    else
    begin
      fType := (fType or MFT_SEPARATOR) and not (MFT_STRING or MFT_OWNERDRAW);
      fState := MFS_DISABLED;
    end;
  end;
  SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);

  // MIIM_BITMAP is needed to request new measure item call
  with MenuInfo do
  begin
    fMask := MIIM_BITMAP;
    dwTypeData := nil;
  end;
  SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);

  // set owner drawn
  with MenuInfo do
  begin
    fMask := MIIM_TYPE;
    fType := (fType or MFT_OWNERDRAW) and not (MFT_STRING or MFT_SEPARATOR);
    dwTypeData := LPSTR(ACaption);
    cch := StrLen(dwTypeData);
  end;
  SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);

  TriggerFormUpdate(AMenuItem);
end;

class procedure TWin32WSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  MenuInfo: MENUITEMINFO;
  ParentMenuHandle: HMenu;
  ParentOfParent: HMenu;
begin
  ParentMenuHandle := AMenuItem.Parent.Handle;
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  MenuInfo.cbSize := menuiteminfosize;

  // Following part fixes the case when an item is added in runtime
  // but the parent item has not defined the submenu flag (hSubmenu=0)
  if AMenuItem.Parent.Parent <> nil then
  begin
    ParentOfParent := AMenuItem.Parent.Parent.Handle;
    MenuInfo.fMask := MIIM_SUBMENU;

    if GetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command, False, @MenuInfo) then
    begin
      // the parent menu item is not defined with submenu flag
      // convert it to submenu
      if MenuInfo.hSubmenu = 0 then
      begin
        MenuInfo.hSubmenu := ParentMenuHandle;
        if not SetMenuItemInfo(ParentOfParent, AMenuItem.Parent.Command, False, @MenuInfo) then
          DebugLn(['SetMenuItemInfo failed: ', GetLastErrorReport]);
      end;
    end;
  end;

  with MenuInfo do
  begin
    if AMenuItem.Enabled then
      fState := MFS_ENABLED
    else
      fstate := MFS_GRAYED;
    if AMenuItem.Checked then
      fState := fState or MFS_CHECKED;
    fMask := MIIM_ID or MIIM_DATA or MIIM_STATE or MIIM_TYPE;
    wID := AMenuItem.Command; {value may only be 16 bit wide!}
    dwItemData := PtrInt(AMenuItem);
    if (AMenuItem.Count > 0) then
    begin
      fMask := fMask or MIIM_SUBMENU;
      hSubMenu := AMenuItem.Handle;
    end else
      hSubMenu := 0;
    fType := MFT_OWNERDRAW;
    if AMenuItem.IsLine then
    begin
      fType := fType or MFT_SEPARATOR;
      fState := fState or MFS_DISABLED;
    end;
    dwTypeData := PChar(AMenuItem);
    if AMenuItem.RadioItem then
      fType := fType or MFT_RADIOCHECK;
    if (AMenuItem.GetIsRightToLeft) then
    begin
      fType := fType or MFT_RIGHTORDER;
      //Reverse the RIGHTJUSTIFY to be left
      if not AMenuItem.RightJustify then
        fType := fType or MFT_RIGHTJUSTIFY;
    end
    else
    if AMenuItem.RightJustify then
      fType := fType or MFT_RIGHTJUSTIFY;
  end;
  if not InsertMenuItem(ParentMenuHandle, AMenuItem.Parent.VisibleIndexOf(AMenuItem), True, @MenuInfo) then
    DebugLn(['InsertMenuItem failed with error: ', GetLastErrorReport]);
  TriggerFormUpdate(AMenuItem);
end;

class function TWin32WSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := CreatePopupMenu;
end;

class procedure TWin32WSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
var
  ParentOfParentHandle, ParentHandle: HMENU;
  MenuInfo: MENUITEMINFO;
begin
  if Assigned(AMenuItem.Parent) then
  begin
    ParentHandle := AMenuItem.Parent.Handle;
    RemoveMenu(ParentHandle, AMenuItem.Command, MF_BYCOMMAND);
    // convert submenu to a simple menu item if needed
    if (GetMenuItemCount(ParentHandle) = 0) and Assigned(AMenuItem.Parent.Parent) and
       AMenuItem.Parent.Parent.HandleAllocated then
    begin
      ParentOfParentHandle := AMenuItem.Parent.Parent.Handle;
      FillChar(MenuInfo, SizeOf(MenuInfo), 0);
      with MenuInfo do
      begin
        cbSize := menuiteminfosize;
        fMask := MIIM_SUBMENU;
      end;
      GetMenuItemInfo(ParentOfParentHandle, AMenuItem.Parent.Command, False, @MenuInfo);
      // the parent menu item is defined with submenu flag then reset it
      if MenuInfo.hSubmenu <> 0 then
      begin
        MenuInfo.hSubmenu := 0;
        if not SetMenuItemInfo(ParentOfParentHandle, AMenuItem.Parent.Command, False, @MenuInfo) then
          DebugLn(['SetMenuItemInfo failed: ', GetLastErrorReport]);
        // Set menu item info destroys/corrupts our internal popup menu for the
        // unknown reason. We need to recreate it.
        if not IsMenu(ParentHandle) then
        begin
          ParentHandle := CreatePopupMenu;
          AMenuItem.Parent.Handle := ParentHandle;
        end;
      end;
    end;
  end;
  DestroyMenu(AMenuItem.Handle);
  TriggerFormUpdate(AMenuItem);
end;

class procedure TWin32WSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
  UpdateCaption(AMenuItem, aCaption);
end;

class function TWin32WSMenuItem.SetCheck(const AMenuItem: TMenuItem;
  const Checked: boolean): boolean;
begin
  UpdateCaption(AMenuItem, aMenuItem.Caption);
  Result := Checked;
end;

class procedure TWin32WSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin
  UpdateCaption(AMenuItem, aMenuItem.Caption);
end;

class function TWin32WSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
var
  EnableFlag: DWord;
begin
  EnableFlag := MF_BYCOMMAND or EnabledToStateFlag[Enabled];
  Result := Boolean(Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, EnableFlag));
  TriggerFormUpdate(AMenuItem);
end;

class function TWin32WSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  Result := ChangeMenuFlag(AMenuItem, MFT_RIGHTJUSTIFY, Justified);
end;

class procedure TWin32WSMenuItem.UpdateMenuIcon(const AMenuItem: TMenuItem;
  const HasIcon: Boolean; const AIcon: Graphics.TBitmap);
begin
  UpdateCaption(AMenuItem, aMenuItem.Caption);
end;

{ TWin32WSMenu }

class function TWin32WSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreateMenu;
end;

class procedure TWin32WSMenu.SetBiDiMode(const AMenu : TMenu;
  UseRightToLeftAlign, UseRightToLeftReading: Boolean);
begin
  if not WSCheckHandleAllocated(AMenu, 'SetBiDiMode')
  then Exit;

  SetMenuFlag(AMenu.Handle, MFT_RIGHTORDER or MFT_RIGHTJUSTIFY, AMenu.IsRightToLeft);

  //TriggerFormUpdate not take TMenu, we repeate the code
  if not (AMenu.Parent is TCustomForm) then Exit;
  if not TCustomForm(AMenu.Parent).HandleAllocated then Exit;
  if csDestroying in AMenu.Parent.ComponentState then Exit;

  AddToChangedMenus(TCustomForm(AMenu.Parent).Handle);
end;


{ TWin32WSPopupMenu }

class function TWin32WSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreatePopupMenu;
end;

class procedure TWin32WSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
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
  AppHandle := TWin32WidgetSet(WidgetSet).AppHandle;
  GetWin32WindowInfo(AppHandle)^.PopupMenu := APopupMenu;
  TrackPopupMenuEx(MenuHandle,
    lAlignment[APopupMenu.Alignment, APopupMenu.IsRightToLeft] or lTrackButtons[APopupMenu.TrackButton],
    X, Y, AppHandle, nil);
end;

initialization
  if (Win32MajorVersion = 4) and (Win32MinorVersion = 0) then
    menuiteminfosize := W95_MENUITEMINFO_SIZE
  else
    menuiteminfosize := sizeof(TMenuItemInfo);
end.
