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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
    class function SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean; override;
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

  
  function MenuItemLength(const aMenuItem: TMenuItem; const aHDC: HDC): integer;
  function MenuItemHeight(const aMenuItem: TMenuItem; const aHDC: HDC): integer;
  procedure DrawMenuItem(const aMenuItem: TMenuItem; const aHDC: HDC; const aRect: Windows.RECT; const aSelected: boolean);


implementation

{ helper routines }

const SpaceBetweenIcons = 5;

type
  TCaptionFlags = (cfBold, cfUnderline);
  TCaptionFlagsSet = set of TCaptionFlags;
  
  TMenuItemCaptionToken = class(TObject)
    token: string;
    fontDecoration: TCaptionFlagsSet;
    constructor Create;
    destructor Destroy; override;
  end;

(* TMenuItemCaptionToken *)
constructor TMenuItemCaptionToken.Create;
begin
  inherited Create;
  token := '';
  fontDecoration := [];
end;
    
destructor TMenuItemCaptionToken.Destroy;
begin
  inherited Destroy;
end;
(* End of TMenuItemCaptionToken *)

procedure RemoveUnderline(const aList: Tlist);
var i: integer;
    token: TMenuItemCaptionToken;
begin
  for i := 0 to aList.Count - 1 do
  begin
    token := TMenuItemCaptionToken(aList[i]);
    token.fontDecoration := token.fontDecoration - [cfUnderline];
  end;
end;

procedure MergeTokens(const aList: TList);
var i: integer;
    token1, token2: TMenuItemCaptionToken;
begin
  i := aList.Count - 1;
  while i > 0 do
  begin
    token1 := TMenuItemCaptionToken(aList[i-1]);
    token2 := TMenuItemCaptionToken(aList[i]);
    if (token1.fontDecoration = token2.fontDecoration) then
    begin
      token1.token := token1.token + token2.token;
      token2.Free;
      aList.Delete(i);
    end;
    dec(i);
  end;
end;

function ParseMenuItemCaption(const aUnderlinedChar: char; aCaption: string; const aDecoration: TCaptionFlagsSet): TList;
var position: integer;
    token: TMenuItemCaptionToken;
    subcaption: string;
begin
  subcaption := '';
  Result := TList.Create;
  position := pos(aUnderlinedChar, aCaption);
  // if aChar is on the last position then there is nothing to underscore, ignore this character
  //while (position > 0) and (position < length(aCaption)) do
  while (position > 0) do
  begin
    if aCaption[position+1] = aUnderlinedChar then
    begin
      // two 'aChar' characters together are replaced by one
      subCaption := subcaption + copy(aCaption, 1, position);
    end else begin
      // before adding new underlined character, all previous underlined characters must be changed to not underlined - it is Delphi like behavior
      removeUnderline(Result);
      if (position > 1) or (subcaption <> '') then
      begin
        token := TMenuItemCaptionToken.Create;
        token.token := subcaption + copy(aCaption, 1, position - 1);
        token.fontDecoration := aDecoration;
        Result.add(token);
      end;
      // next character (if any) must be underlined
      if position < length(aCaption) then begin
        token := TMenuItemCaptionToken.Create;
        token.token := copy(aCaption, position + 1, 1);
        token.fontDecoration := aDecoration + [cfUnderline];
        Result.add(token);
        subcaption := '';
      end;
    end;
    aCaption := copy(aCaption, position + 2, length(aCaption) - position - 1);
    position := pos(aUnderlinedChar, aCaption);
  end;
  token := TMenuItemCaptionToken.Create;
  token.token := subcaption + aCaption;
  token.fontDecoration := aDecoration;
  Result.add(token);
  mergeTokens(Result);
end;

procedure DestroyCaptionTokens(aList: TList);
var i: integer;
    token: TMenuItemCaptionToken;
begin
  for i := 0 to aList.Count - 1 do
  begin
    token := TMenuItemCaptionToken(aList[i]);
    token.Free;
  end;
  aList.Free;
end;
    

function MenuItemCaptionFromList(const aList: TList): string;
var i: integer;
begin
  Result := '';
  for i := 0 to aList.Count - 1 do
    Result := Result + (TMenuItemCaptionToken(aList[i]).token);
end;

  
function GetMenuItemFont(const aFlags: TCaptionFlagsSet): HFONT;
var lf: LOGFONT;
    ncm: NONCLIENTMETRICS;
begin
  ncm.cbSize:= sizeof(ncm);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(ncm), @ncm, 0) then
    lf:= ncm.lfMenuFont
  else
    GetObject(GetStockObject(DEFAULT_GUI_FONT), sizeof(LOGFONT), @lf);
  if cfUnderline in aFlags then lf.lfUnderline := 1
  else lf.lfUnderline := 0;
  if cfBold in aFlags then
  begin
    if lf.lfWeight<=400 then
      lf.lfWeight:= lf.lfWeight + 300
    else
      lf.lfWeight:= lf.lfWeight + 100;
  end;
  Result := CreateFont(lf.lfHeight, lf.lfWidth, 
    lf.lfEscapement, lf.lfOrientation, lf.lfWeight, 
    lf.lfItalic, lf.lfUnderline, lf.lfStrikeOut, lf.lfCharSet, 
    lf.lfOutPrecision, lf.lfClipPrecision, lf.lfQuality, 
    lf.lfPitchAndFamily, lf.lfFaceName);
end;
  
(* Get the menu item caption including shortcut *)
function CompleteMenuItemCaption(const aMenuItem: TMenuItem): string;
begin
  Result := aMenuItem.Caption;
  if aMenuItem.shortCut <> scNone then
    Result := Result  + ShortCutToText(aMenuItem.shortCut);
end;
  
(* Get the maximum length of the given string in pixels *)
function StringLength(const aCaption: String; const aHDC: HDC; const aDecoration:TCaptionFlagsSet): integer;
var captionSize: SIZE;
    oldFont: HFONT;
    newFont: HFONT;
begin
  newFont := getMenuItemFont(aDecoration);
  oldFont := SelectObject(aHDC, newFont);
  GetTextExtentPoint32(aHDC, PChar(aCaption), length(aCaption), @captionSize);
  SelectObject(aHDC, oldFont);
  DeleteObject(newFont);
  Result := captionSize.cx;
end;
  
function MenuCaptionLength(const aList: Tlist; const aHDC: HDC): integer;
var i: integer;
    token: TMenuItemCaptionToken;
begin
  Result := 0;
  for i := 0 to aList.Count - 1 do
  begin
    token := TMenuItemCaptionToken(aList[i]);
    Result := Result + StringLength(token.token, aHDC, token.fontDecoration);
  end;
end;
  
(* Get the maximum height of the given string in pixels *)
function StringHeight(const aCaption: String; const aHDC: HDC; const aDecoration: TCaptionFlagsSet): integer;
var captionSize: SIZE;
    oldFont: HFONT;
    newFont: HFONT;
begin
  newFont := getMenuItemFont(aDecoration);
  oldFont := SelectObject(aHDC, newFont);
  GetTextExtentPoint32(aHDC, PChar(aCaption), length(aCaption), @captionSize);
  SelectObject(aHDC, oldFont);
  DeleteObject(newFont);
  Result := captionSize.cy;
end;
  
function MenuCaptionHeight(const aList: TList; const aHDC: HDC): integer;
var i: integer;
    token: TMenuItemCaptionToken;
    height: integer;
begin
  Result := 0;
  for i := 0 to aList.Count - 1 do
  begin
    token := TMenuItemCaptionToken(aList[i]);
    height := StringHeight(token.token, aHDC, token.fontDecoration);
    if height > Result then Result := height;
  end;
end;

function LeftIconPosition: integer;
begin
  Result := GetSystemMetrics(SM_CXMENUCHECK);
end;

function MenuIconWidth(const AMenuItem: TMenuItem): integer;
var
  SiblingMenuItem : TMenuItem;
  i : integer;
  RequiredWidth: integer;
begin
  Result := 0;
  for i:= 0 to AMenuItem.Parent.Count -1 do begin
    SiblingMenuItem := AMenuItem.Parent.Items[i];
    if SiblingMenuItem.HasIcon then begin
      RequiredWidth := SiblingMenuItem.Bitmap.Width;
      if RequiredWidth > Result then
        Result := RequiredWidth;
    end;
  end;
  Result := Result + LeftIconPosition;
end;

function MenuItemLength(const aMenuItem: TMenuItem; const aHDC: HDC): integer;
var captionTokens: TList;
    decoration: TCaptionFlagsSet;
begin
  if aMenuItem.Default then decoration := [cfBold]
  else decoration := [];
  captionTokens := parseMenuItemCaption('&', CompleteMenuItemCaption(aMenuItem), decoration);
  if aMenuItem.IsInMenuBar then Result := MenuCaptionLength(captionTokens, aHDC)
  else Result := MenuIconWidth(aMenuItem) + spaceBetweenIcons + MenuCaptionLength(captionTokens, aHDC) + spaceBetweenIcons;
  if aMenuItem.ShortCut <> scNone then
    Result := Result + spaceBetweenIcons;
  destroyCaptionTokens(captionTokens);
end;

function MenuItemHeight(const AMenuItem: TMenuItem; const aHDC: HDC): integer;
var captionTokens: TList;
    decoration: TCaptionFlagsSet;
    minimumHeight: integer;
begin
  minimumHeight := GetSystemMetrics(SM_CYMENU);
  if not aMenuItem.IsInMenuBar then minimumHeight := minimumHeight - 2;
  if aMenuItem.IsLine then Result := 10 // it is a separator
  else begin
    if aMenuItem.Default then decoration := [cfBold]
    else decoration := [];
    captionTokens := parseMenuItemCaption('&', aMenuItem.Caption, decoration);
    Result := MenuCaptionHeight(captionTokens, aHDC);
    if aMenuItem.hasIcon and (aMenuItem.bitmap.height > Result) then
      Result := aMenuItem.bitmap.height;
    Result := Result + 2;
    if Result < minimumHeight then Result := minimumHeight;
    DestroyCaptionTokens(captionTokens);
  end;
end;

function LeftCaptionPosition(const aMenuItemLength: integer; const anElementLength: integer; const AMenuItem: TMenuItem): integer;
begin
  if AMenuItem.IsInMenuBar then Result := (aMenuItemLength - anElementLength) div 2
  else Result := MenuIconWidth(AMenuItem) + SpaceBetweenIcons;
end;

function TopPosition(const aMenuItemHeight: integer; const anElementHeight: integer): integer;
begin
  Result := (aMenuItemHeight - anElementHeight) div 2;
end;

function BackgroundColorMenu(const aSelected: boolean; const aInMainMenu: boolean): COLORREF;
var IsFlatMenu: Windows.BOOL;
begin
  if aSelected then
    Result := GetSysColor(COLOR_HIGHLIGHT)
  // SPI_GETFLATMENU = 0x1022, it is not yet defined in the FPC
  else if aInMainMenu and (SystemParametersInfo($1022, 0, @IsFlatMenu, 0)) and IsFlatMenu then // COLOR_MENUBAR is not supported on Windows version < XP
    Result := GetSysColor(COLOR_MENUBAR)
  else
    Result := GetSysColor(COLOR_MENU);
end;

function TextColorMenu(const aSelected: boolean; const anEnabled: boolean): COLORREF;
begin
  if anEnabled then
  begin
    if aSelected then
      Result := GetSysColor(COLOR_HIGHLIGHTTEXT)
    else
      Result := GetSysColor(COLOR_MENUTEXT);
  end else
    Result := GetSysColor(COLOR_GRAYTEXT);
end;

procedure DrawSeparator(const aHDC: HDC; const aRect: Windows.RECT);
var separatorRect: Windows.RECT;
begin
  separatorRect.left := aRect.left;
  separatorRect.right := aRect.right;
  separatorRect.top := aRect.top + (aRect.bottom - aRect.top) div 2 - 1;
  separatorRect.bottom := separatorRect.top + 2;
  DrawEdge(aHDC, separatorRect, BDR_SUNKENOUTER, BF_RECT);
end;

procedure DrawList(const aList: TList; const aHDC: HDC; const aRect: Windows.RECT);
var i: integer;
    token: TMenuItemCaptionToken;
    oldFont: HFONT;
    newFont: HFONT;
begin
  for i := 0 to aList.Count - 1 do
  begin
    token := TMenuItemCaptionToken(aList[i]);
    newFont := getMenuItemFont(token.fontDecoration);
    oldFont := SelectObject(aHDC, newFont);
    ExtTextOut(aHDC, 0, 0, ETO_OPAQUE, nil, PChar(token.token), length(token.token), nil);
    SelectObject(aHDC, oldFont);
    DeleteObject(newFont);
  end;
end;

procedure DrawMenuItemCheckMark(const aMenuItem: TMenuItem; const aHDC: HDC; const aRect: Windows.RECT; const aSelected: boolean);
var checkMarkWidth: integer;
    checkMarkHeight: integer;
    hdcMem: HDC;
    monoBitmap: HBITMAP;
    oldBitmap: HBITMAP;
    checkMarkShape: integer;
    checkMarkRect: Windows.RECT;
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
  if aMenuItem.RadioItem then checkMarkShape := DFCS_MENUBULLET
  else checkMarkShape := DFCS_MENUCHECK;
  DrawFrameControl(hdcMem, @checkMarkRect, DFC_MENU, checkMarkShape);
  BitBlt(aHDC, aRect.left, aRect.top + topPosition(aRect.bottom - aRect.top, checkMarkRect.bottom - checkMarkRect.top), checkMarkWidth, checkMarkHeight, hdcMem, 0, 0, SRCCOPY);
  SelectObject(hdcMem, oldBitmap);
  DeleteObject(monoBitmap);
  DeleteDC(hdcMem);
end;

procedure DrawMenuItemCaption(const aMenuItem: TMenuItem; const aHDC: HDC; const aRect: Windows.RECT; const aSelected: boolean);
var crText: COLORREF;
    crBkgnd: COLORREF;
    captionTokens: TList;
    decoration: TCaptionFlagsSet;
begin
  if aMenuItem.Default then decoration := [cfBold]
  else decoration := [];
  captionTokens := parseMenuItemCaption('&', aMenuItem.Caption, decoration);
  crText := TextColorMenu(aSelected, aMenuItem.Enabled);
  crBkgnd := BackgroundColorMenu(aSelected, aMenuItem.IsInMenuBar);
  SetTextColor(aHDC, crText);
  SetBkColor(aHDC, crBkgnd);
  SetTextAlign(aHDC, TA_UPDATECP);
  ExtTextOut(aHDC, 0, 0, ETO_OPAQUE, @aRect, PChar(''), 0, nil);
  MoveToEx(aHDC, aRect.left + leftCaptionPosition(aRect.right - aRect.left, menuCaptionLength(captionTokens, aHDC), aMenuItem), aRect.top + topPosition(aRect.bottom - aRect.top, menuCaptionHeight(captionTokens, aHDC)), nil);
  DrawList(captionTokens, aHDC, aRect);
  destroyCaptionTokens(captionTokens);
end;

procedure DrawMenuItemShortCut(const aMenuItem: TMenuItem; const aHDC: HDC; const aRect: Windows.RECT; const aSelected: boolean);
var crText: COLORREF;
    crBkgnd: COLORREF;
    shortCutText: String;
    captionTokens: TList;
    decoration: TCaptionFlagsSet;
begin
  shortCutText := ShortCutToText(aMenuItem.ShortCut);
  if aMenuItem.Default then decoration := [cfBold]
  else decoration := [];
  captionTokens := parseMenuItemCaption('&', shortCutText, decoration);
  crText := TextColorMenu(aSelected, aMenuItem.Enabled);
  crBkgnd := BackgroundColorMenu(aSelected, aMenuItem.IsInMenuBar);
  SetTextColor(aHDC, crText);
  SetBkColor(aHDC, crBkgnd);
  MoveToEx(aHDC, aRect.right - menuCaptionLength(captionTokens, aHDC) - GetSystemMetrics(SM_CXMENUCHECK), aRect.top + topPosition(aRect.bottom - aRect.top, menuCaptionHeight(captionTokens, aHDC)), nil);
  DrawList(captionTokens, aHDC, aRect);
  destroyCaptionTokens(captionTokens);
end;

procedure DrawMenuItemIcon(const aMenuItem: TMenuItem; const aHDC: HDC; const aRect: Windows.RECT; const aSelected: boolean);
var hdcMem: HDC;
    hbmpOld: HBITMAP;
begin
  hdcMem := CreateCompatibleDC(aHDC);
  hbmpOld := SelectObject(hdcMem, aMenuItem.Bitmap.Handle);
  TWin32WidgetSet(WidgetSet).MaskBlt(aHDC, aRect.left + LeftIconPosition, aRect.top + TopPosition(aRect.bottom - aRect.top, aMenuItem.Bitmap.Height), aMenuItem.Bitmap.Width, aMenuItem.Bitmap.Height, hdcMem, 0, 0, aMenuItem.Bitmap.MaskHandle, 0, 0);
  SelectObject(hdcMem, hbmpOld);
  DeleteDC(hdcMem);
end;

procedure DrawMenuItem(const aMenuItem: TMenuItem; const aHDC: HDC; const aRect: Windows.RECT; const aSelected: boolean);
begin
  if aMenuItem.IsLine then
    DrawSeparator(aHDC, aRect)
  else begin
    DrawMenuItemCaption(aMenuItem, aHDC, aRect, aSelected);
    if aMenuItem.ShortCut <> scNone then
      DrawMenuItemShortCut(aMenuItem, aHDC, aRect, aSelected);
    if aMenuItem.Checked then
      DrawMenuItemCheckMark(aMenuItem, aHDC, aRect, aSelected);
    if aMenuItem.hasIcon then
      DrawMenuItemIcon(aMenuItem, aHDC, aRect, aSelected);
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
      fMask:=MIIM_TYPE;
      dwTypeData:=LPSTR(ACaption);
      cch := StrLen(dwTypeData);
    end
    else fType := MFT_SEPARATOR;
  end;
  SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  with MenuInfo do
  begin
    cbsize:=sizeof(MENUITEMINFO);
    fMask := MIIM_TYPE;
    fType := MFT_OWNERDRAW;
    dwTypeData:=LPSTR(ACaption);
    cch := StrLen(dwTypeData);
  end;
  SetMenuItemInfo(AMenuItem.Parent.Handle, AMenuItem.Command, false, @MenuInfo);
  TriggerFormUpdate(AMenuItem);
end;

class procedure TWin32WSMenuItem.AttachMenu(const AMenuItem: TMenuItem);
var 
  MenuInfo: MENUITEMINFO;
  ParentMenuHandle: HMenu;
  ParentOfParent: HMenu;
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
                      false, @MenuInfo);
    end;
  end;

  with MenuInfo do begin
    cbsize:=sizeof(MENUITEMINFO);
    if AMenuItem.Enabled then fState:=MFS_ENABLED else fstate:=MFS_GRAYED;
    if AMenuItem.Checked then fState:=fState or MFS_CHECKED;
    fMask:=MIIM_ID or MIIM_DATA or MIIM_STATE or MIIM_TYPE;
    wID:=AMenuItem.Command; {value may only be 16 bit wide!}
    dwItemData:=PtrInt(AMenuItem);
    if (AMenuItem.Count > 0) then 
    begin
      fMask := fMask or MIIM_SUBMENU;
      hSubMenu := AMenuItem.Handle;
    end else
      hSubMenu := 0;
    if not AMenuItem.IsLine then
    begin
      fType:=MFT_OWNERDRAW;
    end else begin
      fType:=MFT_OWNERDRAW or MFT_SEPARATOR;
      fState:=fState or MFS_DISABLED;
    end;
    dwTypeData := PChar(AMenuItem);
    if AMenuItem.RadioItem then fType := fType or MFT_RADIOCHECK;
    if AMenuItem.RightJustify then fType := fType or MFT_RIGHTJUSTIFY;
  end;
  if dword(InsertMenuItem(ParentMenuHandle,
       AMenuItem.Parent.VisibleIndexOf(AMenuItem), true, @MenuInfo)) = 0 then
    DebugLn('InsertMenuItem failed with error: ', IntToStr(Windows.GetLastError));
  TriggerFormUpdate(AMenuItem);
end;

class function TWin32WSMenuItem.CreateHandle(const AMenuItem: TMenuItem): HMENU;
begin
  Result := CreatePopupMenu;
end;

class procedure TWin32WSMenuItem.DestroyHandle(const AMenuItem: TMenuItem);
begin
  if Assigned(AMenuItem.Parent) then
    DeleteMenu(AMenuItem.Parent.Handle, AMenuItem.Command, MF_BYCOMMAND);
  DestroyMenu(AMenuItem.Handle);
  TriggerFormUpdate(AMenuItem);
end;

class procedure TWin32WSMenuItem.SetCaption(const AMenuItem: TMenuItem; const ACaption: string);
begin
  UpdateCaption(AMenuItem, aCaption);
end;
  
class procedure TWin32WSMenuItem.SetShortCut(const AMenuItem: TMenuItem;
  const OldShortCut, NewShortCut: TShortCut);
begin
  UpdateCaption(AMenuItem, aMenuItem.Caption);
end;

class function TWin32WSMenuItem.SetEnable(const AMenuItem: TMenuItem; const Enabled: boolean): boolean;
var
  EnableFlag: Integer;
begin
  if Enabled then EnableFlag := MF_ENABLED
  else EnableFlag := MF_GRAYED;
  EnableFlag := EnableFlag or MF_BYCOMMAND;
  Result := Boolean(Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, EnableFlag));
  TriggerFormUpdate(AMenuItem);
end;

class function TWin32WSMenuItem.SetRightJustify(const AMenuItem: TMenuItem; const Justified: boolean): boolean;
begin
  Result := ChangeMenuFlag(AMenuItem, MFT_RIGHTJUSTIFY, Justified);
end;


{ TWin32WSMenu }

class function TWin32WSMenu.CreateHandle(const AMenu: TMenu): HMENU;
begin
  Result := CreateMenu;
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
begin
  MenuHandle := APopupMenu.Handle;
  AppHandle := TWin32WidgetSet(WidgetSet).AppHandle;
  GetWindowInfo(AppHandle)^.PopupMenu := APopupMenu;
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
