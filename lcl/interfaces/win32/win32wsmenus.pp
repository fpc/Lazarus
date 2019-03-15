{ $Id$}
{
 *****************************************************************************
 *                              Win32WSMenus.pp                              *
 *                              ---------------                              *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  LCLType, Graphics, GraphType, ImgList, Menus, Forms,
////////////////////////////////////////////////////
  WSMenus, WSLCLClasses, WSProc,
  Windows, Controls, Classes, SysUtils, Win32Int, Win32Proc, Win32WSImgList,
  LCLProc, Themes, UxTheme, Win32Themes, Win32Extra,
  FileUtil, LazUTF8;

type

  { TWin32WSMenuItem }

  TWin32WSMenuItem = class(TWSMenuItem)
  published
    class procedure AttachMenu(const AMenuItem: TMenuItem); override;
    class function CreateHandle(const AMenuItem: TMenuItem): HMENU; override;
    class procedure DestroyHandle(const AMenuItem: TMenuItem); override;
    class procedure SetCaption(const AMenuItem: TMenuItem; const ACaption: string); override;
    class function SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean; override;
    class procedure SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut); override;
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
  function FindMenuItemAccelerator(const ACharCode: word; const AMenuHandle: HMENU): integer;
  procedure DrawMenuItemIcon(const AMenuItem: TMenuItem; const AHDC: HDC;
    const ImageRect: TRect; const ASelected: Boolean);
  function ItemStateToDrawState(const ItemState: UINT): LCLType.TOwnerDrawState;

implementation

uses strutils;

type
  TMenuItemHelper = class helper for TMenuItem
  public
    function MeasureItem(ACanvas: TCanvas; var AWidth, AHeight: Integer): Boolean;
    function DrawItem(ACanvas: TCanvas; ARect: TRect; AState: LCLType.TOwnerDrawState): Boolean;
  end;

{ TMenuItemHelper }

function TMenuItemHelper.DrawItem(ACanvas: TCanvas; ARect: TRect;
  AState: LCLType.TOwnerDrawState): Boolean;
begin
  Result := DoDrawItem(ACanvas, ARect, AState);
end;

function TMenuItemHelper.MeasureItem(ACanvas: TCanvas; var AWidth,
  AHeight: Integer): Boolean;
begin
  Result := DoMeasureItem(ACanvas, AWidth, AHeight);
end;

{ helper routines }

const
  SpaceNextToCheckMark = 2; // Used by Windows for check bitmap
  SpaceNextToIcon      = 5; // Our custom spacing for bitmaps bigger than check mark

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

function FindMenuItemAccelerator(const ACharCode: word; const AMenuHandle: HMENU): integer;
var
  MenuItemIndex: integer;
  ItemInfo: MENUITEMINFO;     // TMenuItemInfoA and TMenuItemInfoW have same size and same structure type
  FirstMenuItem: TMenuItem;
  SiblingMenuItem: TMenuItem;
  i: integer;
begin
  Result := MakeLResult(0, MNC_IGNORE);
  MenuItemIndex := -1;
  ItemInfo.cbSize := sizeof(TMenuItemInfo);
  ItemInfo.fMask := MIIM_DATA;
  if not GetMenuItemInfoW(AMenuHandle, 0, true, @ItemInfo) then Exit;

  FirstMenuItem := TMenuItem(ItemInfo.dwItemData);
  if FirstMenuItem = nil then exit;
  i := 0;
  while (i < FirstMenuItem.Parent.Count) and (MenuItemIndex < 0) do
  begin
    SiblingMenuItem := FirstMenuItem.Parent.Items[i];
    if IsAccel(ACharCode, SiblingMenuItem.Caption) then
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

(* Get the menu item shortcut text *)
function MenuItemShortCut(const AMenuItem: TMenuItem): string;
begin
  Result := ShortCutToText(AMenuItem.ShortCut);
  if AMenuItem.ShortCutKey2 <> scNone then
    Result := Result + ', ' + ShortCutToText(AMenuItem.ShortCutKey2);
end;

(* Get the menu item caption including shortcut *)
function CompleteMenuItemCaption(const AMenuItem: TMenuItem; Spacing: String): string;
begin
  Result := AMenuItem.Caption;
  if AMenuItem.ShortCut <> scNone then
    Result := Result + Spacing + MenuItemShortCut(AMenuItem);
end;

(* Item with external string caption *)
function CompleteMenuItemStringCaption(const AMenuItem: TMenuItem; ACaption: String; Spacing: String): string;
begin
  Result := ACaption;
  if AMenuItem.ShortCut <> scNone then begin
    Result := Result + Spacing;
    Result := Result + MenuItemShortCut(AMenuItem);
  end;
end;

(* Get the maximum length of the given string in pixels *)
function StringSize(const aCaption: String; const aHDC: HDC): TSize;
var
  tmpRect: Windows.RECT;
  WideBuffer: widestring;
begin
  FillChar(tmpRect, SizeOf(tmpRect), 0);
  WideBuffer := UTF8ToUTF16(aCaption);
  DrawTextW(aHDC, PWideChar(WideBuffer), length(WideBuffer), @TmpRect, DT_CALCRECT);

  Result.cx := TmpRect.right - TmpRect.left;
  Result.cy := TmpRect.Bottom - TmpRect.Top;
end;

function GetAverageCharSize(AHDC: HDC): TSize;
const
  alph: AnsiString = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
var
  sz: SIZE;
  tm: TEXTMETRIC;
begin
  if GetTextMetrics(AHDC, @tm) = False then
    Result.cy := 0
  else
    Result.cy := WORD(tm.tmHeight);

  if GetTextExtentPoint(AHDC, @alph[1], 52, @sz) = False then
    Result.cx := 0
  else
    Result.cx := (sz.cx div 26 + 1) div 2;
end;

function MenuIconWidth(const AMenuItem: TMenuItem; DC: HDC): integer;
var
  SiblingMenuItem : TMenuItem;
  i, RequiredWidth: integer;
begin
  Result := 0;

  if AMenuItem.IsInMenuBar then
  begin
    Result := AMenuItem.GetIconSize(DC).x;
  end
  else
  begin
    for i := 0 to AMenuItem.Parent.Count - 1 do
    begin
      SiblingMenuItem := AMenuItem.Parent.Items[i];
      if SiblingMenuItem.HasIcon then
      begin
        RequiredWidth := SiblingMenuItem.GetIconSize(DC).x;
        if RequiredWidth > Result then
          Result := RequiredWidth;
      end;
    end;
  end;
end;

procedure GetNonTextSpace(const AMenuItem: TMenuItem; DC: HDC;
                          AvgCharWidth: Integer;
                          out LeftSpace, RightSpace: Integer);
var
  Space: Integer = SpaceNextToCheckMark;
  CheckMarkWidth: Integer;
begin
  // If we have Check and Icon then we use only width of Icon.
  // We draw our MenuItem so: space Image space Caption.
  // Items not in menu bar always have enough space for a check mark.

  CheckMarkWidth := GetSystemMetrics(SM_CXMENUCHECK);
  LeftSpace := MenuIconWidth(AMenuItem, DC);

  if LeftSpace > 0 then
  begin
    if not AMenuItem.IsInMenuBar then
    begin
      if LeftSpace < CheckMarkWidth then
        LeftSpace := CheckMarkWidth
      else
      if LeftSpace > CheckMarkWidth then
        Space := SpaceNextToIcon;
    end;
  end
  else
  begin
    if not AMenuItem.IsInMenuBar or AMenuItem.Checked then
      LeftSpace := CheckMarkWidth;
  end;

  if LeftSpace > 0 then
  begin
    // Space to the left of the icon or check.
    if not AMenuItem.IsInMenuBar then
      Inc(LeftSpace, Space);
    // Space between icon or check and caption.
    if AMenuItem.Caption <> '' then
      Inc(LeftSpace, Space);
  end;

  if AMenuItem.IsInMenuBar then
    RightSpace := 0
  else
    RightSpace := CheckMarkWidth + AvgCharWidth;

  if AMenuItem.Caption <> '' then
  begin
    if AMenuItem.IsInMenuBar then
    begin
      Inc(LeftSpace, AvgCharWidth);
      Inc(RightSpace, AvgCharWidth);
    end
    else
    begin
      // Space on the right side of the text.
      Inc(RightSpace, SpaceNextToCheckMark);
    end;
  end;
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
      W := UTF8ToUTF16(MenuItemShortCut(AMenuItem));
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
  IconSize := AMenuItem.GetIconSize(ADC);
  Result.cx := Result.cx + Metrics.TextSize.cx + IconSize.x;
  if IconSize.x > 0 then
    inc(Result.cx, Metrics.ItemMargins.cxLeftWidth);
  Result.cy := Result.cy + Max(Metrics.TextSize.cy, IconSize.y);
end;

function VistaPopupMenuItemSize(AMenuItem: TMenuItem; ADC: HDC): TSize;
var
  Metrics: TVistaPopupMenuMetrics;
  IconSize: TPoint;
  IconWidth: Integer;
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
    Result.cy := Max(Metrics.TextSize.cy + 1, Metrics.CheckSize.cy + Metrics.CheckMargins.cyTopHeight + Metrics.CheckMargins.cyBottomHeight);
    if AMenuItem.HasIcon then
    begin
      IconSize := AMenuItem.GetIconSize(ADC);
      Result.cy := Max(Result.cy, IconSize.y);
      Result.cx := Max(Result.cx, IconSize.x);
    end;
    IconWidth := MenuIconWidth(AMenuItem, ADC);
    Result.cx := Max(Result.cx, IconWidth);
    Result.cy := Max(Result.cy, IconWidth);
  end;
  // count gutter
  Result.cx := Result.cx + (Metrics.CheckBgMargins.cxRightWidth - Metrics.CheckMargins.cxRightWidth) +
               Metrics.GutterSize.cx;
  // count text
  Result.cx := Result.cx + Metrics.TextSize.cx;
  Result.cx := Result.cx + Metrics.TextMargins.cxLeftWidth + Metrics.TextMargins.cxRightWidth;
end;

function ClassicMenuItemSize(AMenuItem: TMenuItem; ADC: HDC): TSize;
var
  LeftSpace, RightSpace: Integer;
  oldFont: HFONT;
  newFont: HFONT;
  AvgCharSize: TSize;
begin
  if AMenuItem.Default then
    newFont := GetMenuItemFont([cfBold])
  else
    newFont := GetMenuItemFont([]);
  oldFont := SelectObject(ADC, newFont);
  AvgCharSize := GetAverageCharSize(ADC);

  Result := StringSize(CompleteMenuItemCaption(AMenuItem, EmptyStr), ADC);

  // Space between text and shortcut.
  if AMenuItem.ShortCut <> scNone then
    inc(Result.cx, AvgCharSize.cx);

  GetNonTextSpace(AMenuItem, ADC, AvgCharSize.cx, LeftSpace, RightSpace);
  inc(Result.cx, LeftSpace + RightSpace);

  // Windows adds additional space to value returned from WM_MEASUREITEM
  // for owner drawn menus. This is to negate that.
  Dec(Result.cx, AvgCharSize.cx * 2);

  // As for height of items in menu bar, regardless of what is set here,
  // Windows seems to always use SM_CYMENUSIZE (space for a border is included).

  if AMenuItem.IsLine then
    Result.cy := GetSystemMetrics(SM_CYMENUSIZE) div 2 // it is a separator
  else
  begin
    if AMenuItem.IsInMenuBar then
    begin
      Result.cy := Max(Result.cy, GetSystemMetrics(SM_CYMENUSIZE));
      if AMenuItem.hasIcon then
        Result.cy := Max(Result.cy, aMenuItem.GetIconSize(ADC).y);
    end
    else
    begin
      Result.cy := Max(Result.cy + 2, AvgCharSize.cy + 4);
      if AMenuItem.hasIcon then
        Result.cy := Max(Result.cy, aMenuItem.GetIconSize(ADC).y + 2);
    end;
  end;

  SelectObject(ADC, oldFont);
  DeleteObject(newFont);
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
  CalculatedSize: TSIZE;
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
  //center the menu item
  CalculatedSize := VistaBarMenuItemSize(AMenuItem, AHDC);
  TextRect.Left := (TextRect.Right+TextRect.Left-CalculatedSize.cx) div 2;
  TextRect.Right := TextRect.Left + CalculatedSize.cx;
  TextRect.Top := (TextRect.Bottom+TextRect.Top-CalculatedSize.cy) div 2;
  TextRect.Bottom := TextRect.Top + CalculatedSize.cy;

  // draw check/image
  if AMenuItem.HasIcon then
  begin
    IconSize := AMenuItem.GetIconSize(AHDC);
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
  CheckRect, CheckRect2, GutterRect, TextRect, SeparatorRect, ImageRect, SubMenuRect: TRect;
  IconSize: TPoint;
  TextFlags: DWord;
  AFont, OldFont: HFONT;
  IsRightToLeft: Boolean;
  IconWidth: Integer;
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
  CheckRect.Bottom := CheckRect.Top + Metrics.CheckSize.cy + Metrics.CheckMargins.cyTopHeight + Metrics.CheckMargins.cyBottomHeight;
  if AMenuItem.HasIcon then
  begin
    IconSize := AMenuItem.GetIconSize(AHDC);
    CheckRect.Bottom := Max(CheckRect.Bottom, CheckRect.Top+IconSize.y);
  end;
  IconWidth := MenuIconWidth(AMenuItem, AHDC);
  CheckRect.Right := Max(CheckRect.Right, CheckRect.Left+IconWidth);
  CheckRect.Bottom := Max(CheckRect.Bottom, CheckRect.Top+IconWidth);
  OffsetRect(CheckRect, 0, (ARect.Bottom-ARect.Top-CheckRect.Bottom+CheckRect.Top) div 2);
  // draw gutter
  GutterRect := Rect(0, ARect.Top, CheckRect.Right, ARect.Bottom);
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
      SubMenuRect.Top := (SubMenuRect.Top + SubMenuRect.Bottom - Metrics.SubMenuSize.cy) div 2;
      SubMenuRect.Bottom := SubMenuRect.Top + Metrics.SubMenuSize.cy;
      SubMenuRect.Right := SubMenuRect.Right - Metrics.SubMenuMargins.cxRightWidth + Metrics.SubMenuMargins.cxLeftWidth;
      SubMenuRect.Left := SubMenuRect.Right - Metrics.SubMenuSize.cx;
      Tmp := ThemeServices.GetElementDetails(PopupSubmenuStates[AMenuItem.Enabled]);
      Tmp.State := Tmp.State + 2;
      ThemeDrawElement(AHDC, Tmp, SubMenuRect, nil);
    end;
    // draw check/image
    if AMenuItem.HasIcon then
    begin
      ImageRect := CheckRect;
      if AMenuItem.Checked then // draw checked rectangle around
      begin
        Tmp := ThemeServices.GetElementDetails(PopupCheckBgStates[AMenuItem.Enabled]);
        ThemeDrawElement(AHDC, Tmp, CheckRect, nil);
      end;
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
      CheckRect2.Left := CheckRect.Left + (CheckRect.Right-CheckRect.Left-Metrics.CheckSize.cx) div 2;
      CheckRect2.Top := CheckRect.Top + (CheckRect.Bottom-CheckRect.Top-Metrics.CheckSize.cy) div 2;
      CheckRect2.Right := CheckRect2.Left + Metrics.CheckSize.cx;
      CheckRect2.Bottom := CheckRect2.Top + Metrics.CheckSize.cy;
      Tmp := ThemeServices.GetElementDetails(PopupCheckStates[AMenuItem.Enabled, AMenuItem.RadioItem]);
      ThemeDrawElement(AHDC, Tmp, CheckRect2, nil);
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
      ThemeDrawText(AHDC, Details, MenuItemShortCut(AMenuItem), TextRect, TextFlags, 0);
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
  CC: TControlCanvas;
  ParentMenu: TMenu;
begin
  CC := TControlCanvas.Create;
  try
    CC.Handle := AHDC;
    Result.cx := 0;
    Result.cy := 0;

    if not AMenuItem.MeasureItem(CC, Result.cx, Result.cy) then
    begin
      if IsVistaMenu then
      begin
        if AMenuItem.IsInMenuBar then
          Result := VistaBarMenuItemSize(AMenuItem, AHDC)
        else
          Result := VistaPopupMenuItemSize(AMenuItem, AHDC);
      end
      else
        Result := ClassicMenuItemSize(AMenuItem, AHDC);
    end;
  finally
    CC.Free;
  end;
end;

function IsFlatMenus: Boolean; inline;
var
  IsFlatMenu: Windows.BOOL;
begin
  Result := (WindowsVersion >= wvXP) and
      (SystemParametersInfo(SPI_GETFLATMENU, 0, @IsFlatMenu, 0) and IsFlatMenu);
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
  space: Integer;
begin
  if IsFlatMenus then
    space := 3
  else
    space := 1;

  separatorRect.Left  := ARect.Left  + space;
  separatorRect.Right := ARect.Right - space;
  separatorRect.Top   := ARect.Top + GetSystemMetrics(SM_CYMENUSIZE) div 4 - 1;
  DrawEdge(AHDC, separatorRect, EDGE_ETCHED, BF_TOP);
end;

procedure DrawMenuItemCheckMark(const aMenuItem: TMenuItem; const aHDC: HDC;
  const aRect: Windows.RECT; const aSelected: boolean; AvgCharWidth: Integer);
var
  checkMarkWidth: integer;
  checkMarkHeight: integer;
  hdcMem: HDC;
  monoBitmap: HBITMAP;
  oldBitmap: HBITMAP;
  checkMarkShape: integer;
  checkMarkRect: Windows.RECT;
  x:Integer;
  space: Integer;
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
  if aMenuItem.IsInMenuBar then
    space := AvgCharWidth
  else
    space := SpaceNextToCheckMark;
  if aMenuItem.GetIsRightToLeft then
    x := aRect.Right - checkMarkWidth - space
  else
    x := aRect.left + space;
  BitBlt(aHDC, x, aRect.top + topPosition(aRect.bottom - aRect.top, checkMarkRect.bottom - checkMarkRect.top), checkMarkWidth, checkMarkHeight, hdcMem, 0, 0, SRCCOPY);
  SelectObject(hdcMem, oldBitmap);
  DeleteObject(monoBitmap);
  DeleteDC(hdcMem);
end;

procedure DrawMenuItemText(const AMenuItem: TMenuItem; const AHDC: HDC;
  ARect: TRect; const ASelected, ANoAccel: boolean; ItemState: UINT;
  AvgCharWidth: Integer);
var
  crText: COLORREF;
  crBkgnd: COLORREF;
  oldBkMode: Longint;
  shortCutText: string;
  IsRightToLeft: Boolean;
  etoFlags: Cardinal;
  dtFlags: DWord;
  WideBuffer: widestring;
  LeftSpace, RightSpace: Integer;
begin
  crText := TextColorMenu(ItemState, AMenuItem.IsInMenuBar, AMenuItem.Enabled);
  crBkgnd := BackgroundColorMenu(ItemState, AMenuItem.IsInMenuBar);
  SetTextColor(AHDC, crText);
  SetBkColor(AHDC, crBkgnd);

  IsRightToLeft := AMenuItem.GetIsRightToLeft;
  etoFlags := ETO_OPAQUE;
  // DT_LEFT is default because its value is 0
  dtFlags := DT_EXPANDTABS or DT_VCENTER or DT_SINGLELINE;
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
    begin
      DrawEdge(AHDC, ARect, BDR_SUNKENOUTER, BF_RECT);

      // Adjust caption position when menu is open.
      OffsetRect(ARect, 1, 1);
    end
    else
    if (ItemState and ODS_HOTLIGHT) <> 0 then
      DrawEdge(AHDC, ARect, BDR_RAISEDINNER, BF_RECT);
  end;

  GetNonTextSpace(AMenuItem, AHDC, AvgCharWidth, LeftSpace, RightSpace);

  if IsRightToLeft then
  begin
    Dec(ARect.Right, LeftSpace);
    Inc(ARect.Left, RightSpace);
  end
  else
  begin
    Inc(ARect.Left, LeftSpace);
    Dec(ARect.Right, RightSpace);
  end;

  // Move text up by 1 pixel otherwise it is too low.
  Dec(ARect.Top, 1);
  Dec(ARect.Bottom, 1);

  oldBkMode := SetBkMode(AHDC, TRANSPARENT);

  WideBuffer := UTF8ToUTF16(AMenuItem.Caption);
  DrawTextW(AHDC, PWideChar(WideBuffer), Length(WideBuffer), @ARect, dtFlags);


  if AMenuItem.ShortCut <> scNone then
  begin
    dtFlags := DT_VCENTER or DT_SINGLELINE;
    shortCutText := MenuItemShortCut(AMenuItem);
    if IsRightToLeft then
      dtFlags := dtFlags or DT_LEFT
    else
      dtFlags := dtFlags or DT_RIGHT;

    WideBuffer := UTF8ToUTF16(shortCutText);
    DrawTextW(AHDC, PWideChar(WideBuffer), Length(WideBuffer), @ARect, dtFlags);

  end;

  SetBkMode(AHDC, oldBkMode);
end;

procedure DrawMenuItemIcon(const AMenuItem: TMenuItem; const AHDC: HDC;
  const ImageRect: TRect; const ASelected: Boolean);
var
  AEffect: TGraphicsDrawEffect;
  AImageList: TCustomImageList;
  FreeImageList: Boolean;
  AImageIndex, AImagesWidth: Integer;
  APPI: longint;
begin
  AMenuItem.GetImageList(AImageList, AImagesWidth);
  if (AImageList = nil) or (AMenuItem.ImageIndex < 0) then // using icon from Bitmap
  begin
    AImageList := TImageList.Create(nil);
    AImageList.Width := AMenuItem.Bitmap.Width; // maybe height to prevent too wide bitmaps?
    AImageList.Height := AMenuItem.Bitmap.Height;
    if not AMenuItem.Bitmap.Transparent then
      AImageIndex := AImageList.AddMasked(AMenuItem.Bitmap, AMenuItem.Bitmap.Canvas.Pixels[0, AImageList.Height-1])
    else
      AImageIndex := AImageList.Add(AMenuItem.Bitmap, nil);
    FreeImageList := True;
  end
  else  // using icon from ImageList
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
  begin
    APPI := GetDeviceCaps(AHDC, LOGPIXELSX);
    TWin32WSCustomImageListResolution.DrawToDC(AImageList.ResolutionForPPI[AImagesWidth, APPI, 1].Resolution,
      AImageIndex, AHDC, ImageRect,
      AImageList.BkColor, AImageList.BlendColor,
      AEffect, AImageList.DrawingStyle, AImageList.ImageType);
  end;
  if FreeImageList then
    AImageList.Free;
end;

function ItemStateToDrawState(const ItemState: UINT): LCLType.TOwnerDrawState;
begin
  Result := [];
  if ItemState and ODS_SELECTED <> 0 then
    Include(Result, LCLType.odSelected);
  if ItemState and ODS_GRAYED <> 0 then
    Include(Result, LCLType.odGrayed);
  if ItemState and ODS_DISABLED <> 0 then
    Include(Result, LCLType.odDisabled);
  if ItemState and ODS_CHECKED <> 0 then
    Include(Result, LCLType.odChecked);
  if ItemState and ODS_FOCUS <> 0 then
    Include(Result, LCLType.odFocused);
  if ItemState and ODS_DEFAULT <> 0 then
    Include(Result, LCLType.odDefault);
  if ItemState and ODS_HOTLIGHT <> 0 then
    Include(Result, LCLType.odHotLight);
  if ItemState and ODS_INACTIVE <> 0 then
    Include(Result, LCLType.odInactive);
  if ItemState and ODS_NOACCEL <> 0 then
    Include(Result, LCLType.odNoAccel);
  if ItemState and ODS_NOFOCUSRECT <> 0 then
    Include(Result, LCLType.odNoFocusRect);
  if ItemState and ODS_COMBOBOXEDIT <> 0 then
    Include(Result, LCLType.odComboBoxEdit);
end;

procedure DrawClassicMenuItemIcon(const AMenuItem: TMenuItem; const AHDC: HDC;
  const ARect: TRect; const ASelected, AChecked: boolean);
var
  x: Integer;
  Space: Integer = SpaceNextToCheckMark;
  ImageRect: TRect;
  IconSize: TPoint;
  checkMarkWidth: integer;
begin
  IconSize := AMenuItem.GetIconSize(AHDC);
  checkMarkWidth := GetSystemMetrics(SM_CXMENUCHECK);
  if not AMenuItem.IsInMenuBar then
  begin
    if IconSize.x < checkMarkWidth then
    begin
      // Center the icon horizontally inside check mark space.
      Inc(Space, TopPosition(checkMarkWidth, IconSize.x));
    end
    else
    if IconSize.x > checkMarkWidth then
    begin
      Space := SpaceNextToIcon;
    end;
  end;

  if AMenuItem.GetIsRightToLeft then
    x := ARect.Right - IconSize.x - Space
  else
    x := ARect.Left + Space;

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

procedure DrawClassicMenuItem(const AMenuItem: TMenuItem; const AHDC: HDC;
  const ARect: Windows.RECT; const ASelected, ANoAccel: boolean; ItemState: UINT);
var
  oldFont: HFONT;
  newFont: HFONT;
  AvgCharWidth: Integer;
begin
  if AMenuItem.IsLine then
    DrawSeparator(AHDC, ARect)
  else
  begin
    if AMenuItem.Default then
      newFont := GetMenuItemFont([cfBold])
    else
      newFont := GetMenuItemFont([]);
    oldFont := SelectObject(AHDC, newFont);
    AvgCharWidth := GetAverageCharSize(AHDC).cx;

    DrawMenuItemText(AMenuItem, AHDC, ARect, ASelected, ANoAccel, ItemState, AvgCharWidth);
    if aMenuItem.HasIcon then
      DrawClassicMenuItemIcon(AMenuItem, AHDC, ARect, ASelected, AMenuItem.Checked)
    else
    if AMenuItem.Checked then
      DrawMenuItemCheckMark(AMenuItem, AHDC, ARect, ASelected, AvgCharWidth);

    SelectObject(AHDC, oldFont);
    DeleteObject(newFont);
  end;
end;

procedure DrawMenuItem(const AMenuItem: TMenuItem; const AHDC: HDC; const ARect: Windows.RECT; const ItemAction, ItemState: UINT);
var
  ASelected, ANoAccel: Boolean;
  B: Bool;
  CC: TControlCanvas;
  ItemDrawState: LCLType.TOwnerDrawState;
begin
  ASelected := (ItemState and ODS_SELECTED) <> 0;
  ANoAccel := (ItemState and ODS_NOACCEL) <> 0;
  if ANoAccel and (WindowsVersion >= wv2000) then
    if SystemParametersInfo(SPI_GETKEYBOARDCUES, 0, @B, 0) then
      ANoAccel := not B
    else
  else
    ANoAccel := False;

  CC := TControlCanvas.Create;
  try
    CC.Handle := AHDC;
    ItemDrawState := ItemStateToDrawState(ItemState);
    if not AMenuItem.DrawItem(CC, ARect, ItemDrawState) then
    begin
      if IsVistaMenu then
      begin
        if AMenuItem.IsInMenuBar then
          DrawVistaMenuBar(AMenuItem, AHDC, ARect, ASelected, ANoAccel, ItemAction, ItemState)
        else
          DrawVistaPopupMenu(AMenuItem, AHDC, ARect, ASelected, ANoAccel);
      end
      else
        DrawClassicMenuItem(AMenuItem, AHDC, ARect, ASelected, ANoAccel, ItemState);
    end;
  finally
    CC.Free;
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
  MenuInfo: MENUITEMINFO;     // TMenuItemInfoA and TMenuItemInfoW have same size and same structure type
begin
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  MenuInfo.cbSize := sizeof(TMenuItemInfo);
  MenuInfo.fMask := MIIM_FTYPE;         // don't retrieve caption (MIIM_STRING not included)
  GetMenuItemInfoW(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  if Value then
    MenuInfo.fType := MenuInfo.fType or Flag
  else
    MenuInfo.fType := MenuInfo.fType and (not Flag);
  Result := SetMenuItemInfoW(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
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
  MenuInfo: MENUITEMINFO;     // TMenuItemInfoA and TMenuItemInfoW have same size and same structure type
begin
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  MenuInfo.cbSize := sizeof(TMenuItemInfo);
  MenuInfo.fMask := MIIM_TYPE;  //MIIM_FTYPE not work here please use only MIIM_TYPE, caption not retrieved (dwTypeData = nil)
  GetMenuItemInfoW(Menu, 0, True, @MenuInfo);
  if Value then
    MenuInfo.fType := MenuInfo.fType or Flag
  else
    MenuInfo.fType := MenuInfo.fType and not Flag;
  SetMenuItemInfoW(Menu, 0, True, @MenuInfo);
end;

{ TWin32WSMenuItem }

procedure UpdateCaption(const AMenuItem: TMenuItem; ACaption: String);
var
  MenuInfo: MENUITEMINFO;     // TMenuItemInfoA and TMenuItemInfoW have same size and same structure type
  WideBuffer: widestring;
begin
  if (AMenuItem.Parent = nil) or not AMenuItem.Parent.HandleAllocated then
    Exit;

  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  with MenuInfo do
  begin
    cbSize := sizeof(TMenuItemInfo);
    fMask := MIIM_FTYPE or MIIM_STATE;  // don't retrieve current caption
  end;
  GetMenuItemInfoW(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  with MenuInfo do
  begin
    // change enabled too since we can change from '-' to normal caption and vice versa
    if ACaption <> cLineCaption then
    begin
      fType := fType or MIIM_STRING;
      fType := fType and not (MFT_SEPARATOR or MFT_OWNERDRAW);
      fState := EnabledToStateFlag[AMenuItem.Enabled];
      if AMenuItem.Checked then
        fState := fState or MFS_CHECKED;
//      AMenuItem.Caption := ACaption;          // Already set
        WideBuffer := UTF8ToUTF16(CompleteMenuItemStringCaption(AMenuItem, ACaption, #9));
        dwTypeData := PChar(WideBuffer);      // PWideChar forced to PChar
        cch := length(WideBuffer);

      fMask := fMask or MIIM_STRING;      // caption updated too
    end
    else
    begin
      fType := fType and not (MIIM_STRING);
      fType := (fType or MFT_SEPARATOR) and not (MFT_OWNERDRAW);
      fState := MFS_DISABLED;
    end;
  end;
  SetMenuItemInfoW(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);

  // MIIM_BITMAP is needed to request new measure item call
  with MenuInfo do
  begin
    fMask := MIIM_BITMAP;
    dwTypeData := nil;
  end;
  SetMenuItemInfoW(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);

  // set owner drawn
  with MenuInfo do
  begin
    fMask := MIIM_FTYPE;      // don't set caption
    fType := (fType or MFT_OWNERDRAW) and not (MIIM_STRING or MFT_SEPARATOR);
  end;
  SetMenuItemInfoW(AMenuItem.Parent.Handle, AMenuItem.Command, False, @MenuInfo);
  TriggerFormUpdate(AMenuItem);
end;

class procedure TWin32WSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var
  MenuInfo: MENUITEMINFO;     // TMenuItemInfoA and TMenuItemInfoW have same size and same structure type
  ParentMenuHandle: HMenu;
  ParentOfParent: HMenu;
  CallMenuRes: Boolean;
  WideBuffer: widestring;
begin
  ParentMenuHandle := AMenuItem.Parent.Handle;
  FillChar(MenuInfo, SizeOf(MenuInfo), 0);
  MenuInfo.cbSize := sizeof(TMenuItemInfo);

  // Following part fixes the case when an item is added in runtime
  // but the parent item has not defined the submenu flag (hSubmenu=0)
  if AMenuItem.Parent.Parent <> nil then
  begin
    ParentOfParent := AMenuItem.Parent.Parent.Handle;
    MenuInfo.fMask := MIIM_SUBMENU;
    CallMenuRes := GetMenuItemInfoW(ParentOfParent, AMenuItem.Parent.Command, False, @MenuInfo);
    if CallMenuRes then
    begin
      // the parent menu item is not defined with submenu flag
      // convert it to submenu
      if MenuInfo.hSubmenu = 0 then
      begin
        MenuInfo.hSubmenu := ParentMenuHandle;
        CallMenuRes := SetMenuItemInfoW(ParentOfParent, AMenuItem.Parent.Command, False, @MenuInfo);
        if not CallMenuRes then
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
    fMask := MIIM_ID or MIIM_DATA or MIIM_STATE or MIIM_FTYPE or MIIM_STRING;
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
    WideBuffer := UTF8ToUTF16(CompleteMenuItemCaption(AMenuItem, #9));
    dwTypeData := PChar(WideBuffer);        // PWideChar forced to PChar
    cch := length(WideBuffer);

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
    if AMenuItem.Default then
      fState := fState or MFS_DEFAULT;
  end;
  CallMenuRes := InsertMenuItemW(ParentMenuHandle, AMenuItem.Parent.VisibleIndexOf(AMenuItem), True, @MenuInfo);
  if not CallMenuRes then
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
  MenuInfo: MENUITEMINFO;     // TMenuItemInfoA and TMenuItemInfoW have same size and same structure type
  CallMenuRes: Boolean;
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
        cbSize := sizeof(TMenuItemInfo);
        fMask := MIIM_SUBMENU;
      end;
      GetMenuItemInfoW(ParentOfParentHandle, AMenuItem.Parent.Command, False, @MenuInfo);
      // the parent menu item is defined with submenu flag then reset it
      if MenuInfo.hSubmenu <> 0 then
      begin
        MenuInfo.hSubmenu := 0;
        CallMenuRes := SetMenuItemInfoW(ParentOfParentHandle, AMenuItem.Parent.Command, False, @MenuInfo);
        if not CallMenuRes then
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

class function TWin32WSMenuItem.SetCheck(const AMenuItem: TMenuItem; const Checked: boolean): boolean;
begin
  UpdateCaption(AMenuItem, aMenuItem.Caption);
  Result := Checked;
end;

class procedure TWin32WSMenuItem.SetShortCut(const AMenuItem: TMenuItem; const ShortCutK1, ShortCutK2: TShortCut);
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

  AddToChangedMenus((AMenu.Parent as TCustomForm).Handle);
end;


{ TWin32WSPopupMenu }

class function TWin32WSPopupMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreatePopupMenu;
end;

class procedure TWin32WSPopupMenu.Popup(const APopupMenu: TPopupMenu; const X, Y: integer);
var
  MenuHandle: HMENU;
  WinHandle: HWND;
const
  lAlignment: array[TPopupAlignment, Boolean] of DWORD = (
              { left-to-rght } { right-to-left }
 { paLeft   } (TPM_LEFTALIGN,   TPM_RIGHTALIGN  or TPM_LAYOUTRTL),
 { paRight  } (TPM_RIGHTALIGN,  TPM_LEFTALIGN   or TPM_LAYOUTRTL),
 { paCenter } (TPM_CENTERALIGN, TPM_CENTERALIGN or TPM_LAYOUTRTL)
  );
  lTrackButtons: array[TTrackButton] of DWORD = (
 { tbRightButton } TPM_RIGHTBUTTON,
 { tbLeftButton  } TPM_LEFTBUTTON
  );
begin
  MenuHandle := APopupMenu.Handle;
  WinHandle:=Win32WidgetSet.AppHandle;
  if (WinHandle=0) and (Screen.ActiveCustomForm<>nil) and Screen.ActiveCustomForm.HandleAllocated then
    WinHandle:=Screen.ActiveCustomForm.Handle;
  GetWin32WindowInfo(WinHandle)^.PopupMenu := APopupMenu;
  TrackPopupMenuEx(MenuHandle,
    lAlignment[APopupMenu.Alignment, APopupMenu.IsRightToLeft] or lTrackButtons[APopupMenu.TrackButton],
    X, Y, WinHandle, nil);
end;

end.
