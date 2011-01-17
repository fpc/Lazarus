{
 *****************************************************************************
 *                            Win32WSExtCtrls.pp                             *
 *                            ------------------                             *
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
unit Win32WSExtCtrls;

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
// rtl
  Windows, CommCtrl, SysUtils, Classes,
// lcl
  ExtCtrls, Controls, ImgList, LCLType, LCLIntf, LCLProc, Themes, LCLMessageGlue,
// ws
  WSControls, WSExtCtrls, WSLCLClasses, WSProc, Win32Extra, Win32Int, Win32Proc,
  InterfaceBase, Win32WSControls;

type

  { TWin32WSCustomPage }

  TWin32WSCustomPage = class(TWSCustomPage)
  public
    class procedure ThemeChange(Wnd: HWND);
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWin32WSCustomNotebook }

  TWin32WSCustomNotebook = class(TWSCustomNotebook)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure AddAllNBPages(const ANotebook: TCustomNotebook);
    class procedure AdjustSizeNotebookPages(const ANotebook: TCustomNotebook);
    class procedure AddPage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook;
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemoveAllNBPages(const ANotebook: TCustomNotebook);
    class procedure RemovePage(const ANotebook: TCustomNotebook;
      const AIndex: integer); override;

    class function GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer; override;
    class function GetTabIndexAtPos(const ANotebook: TCustomNotebook; const AClientPos: TPoint): integer; override;
    class function GetTabRect(const ANotebook: TCustomNotebook; const AIndex: Integer): TRect; override;
    class function GetCapabilities: TNoteBookCapabilities;override;
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class procedure SetImageList(const ANotebook: TCustomNotebook; const AImageList: TCustomImageList); override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
    class procedure UpdateProperties(const ANotebook: TCustomNotebook); override;
  end;

  { TWin32WSPage }

  TWin32WSPage = class(TWSPage)
  published
  end;

  { TWin32WSNotebook }

  TWin32WSNotebook = class(TWSNotebook)
  published
  end;

  { TWin32WSShape }

  TWin32WSShape = class(TWSShape)
  published
  end;

  { TWin32WSCustomSplitter }

  TWin32WSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TWin32WSSplitter }

  TWin32WSSplitter = class(TWSSplitter)
  published
  end;

  { TWin32WSPaintBox }

  TWin32WSPaintBox = class(TWSPaintBox)
  published
  end;

  { TWin32WSCustomImage }

  TWin32WSCustomImage = class(TWSCustomImage)
  published
  end;

  { TWin32WSImage }

  TWin32WSImage = class(TWSImage)
  published
  end;

  { TWin32WSBevel }

  TWin32WSBevel = class(TWSBevel)
  published
  end;

  { TWin32WSCustomRadioGroup }

  TWin32WSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TWin32WSRadioGroup }

  TWin32WSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TWin32WSCustomCheckGroup }

  TWin32WSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TWin32WSCheckGroup }

  TWin32WSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TWin32WSCustomLabeledEdit }

  TWin32WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TWin32WSLabeledEdit }

  TWin32WSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TWin32WSCustomPanel }

  TWin32WSCustomPanel = class(TWSCustomPanel)
  published
  end;

  { TWin32WSPanel }

  TWin32WSPanel = class(TWSPanel)
  published
  end;

  { TWin32WSCustomTrayIcon }

  TWin32WSCustomTrayIcon = class(TWSCustomTrayIcon)
  protected
    class function AddIcon(ATrayIcon: TCustomTrayIcon): Boolean;
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

procedure NotebookFocusNewControl(const ANotebook: TCustomNotebook; NewIndex: integer);
function NotebookPageRealToLCLIndex(const ANotebook: TCustomNotebook; AIndex: integer): integer;

implementation

uses
  Forms, LMessages, ShellAPI;

type
  TCustomPageAccess = class(TCustomPage)
  end;

function IsNotebookGroupFocused(const ANotebook: TCustomNotebook): boolean;
var
  lNotebookHandle, lWindow: HWND;
begin
  result := false;
  if not ANotebook.HandleAllocated then exit;
  lNotebookHandle := ANotebook.Handle;
  lWindow := Windows.GetFocus;
  while (lWindow <> 0) and (lWindow <> lNotebookHandle) do
    lWindow := Windows.GetParent(lWindow);
  if lWindow = 0 then exit;
  result := true;
end;

{ sets focus to a control on the newly focused tab page }
procedure NotebookFocusNewControl(const ANotebook: TCustomNotebook; NewIndex: integer);
var
  Page: TCustomPage;
  AWinControl: TWinControl;
  ParentForm: TCustomForm;
begin
  { see if currently focused control is within notebook }
  if not IsNotebookGroupFocused(ANotebook) then exit;

  { focus was/is within notebook, pick a new control to focus }
  Page := ANotebook.CustomPage(NewIndex);
  ParentForm := GetParentForm(ANotebook);
  if ParentForm <> nil then
  begin
    if ANotebook.ContainsControl(ParentForm.ActiveControl) and (ParentForm.ActiveControl <> ANotebook) then
    begin
      AWinControl := nil;
      if Page.CanFocus then
        AWinControl := TCustomPageAccess(Page).FindNextControl(nil, True, True, False);
      // if nothing to focus then focus notebook then we can traverse pages by keys
      if AWinControl = nil then
        AWinControl := ANotebook;
      AWinControl.SetFocus;
    end;
  end;
end;

function NotebookPageRealToLCLIndex(const ANotebook: TCustomNotebook; AIndex: integer): integer;
var
  I: Integer;
begin
  Result := AIndex;
  if csDesigning in ANotebook.ComponentState then exit;
  I := 0;
  while (I < ANotebook.PageCount) and (I <= Result) do 
  begin
    if not ANotebook.Page[I].TabVisible then Inc(Result);
    Inc(I);
  end;
end;

function PageWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
begin
  if Msg = WM_THEMECHANGED then
  begin
    ThemeServices.UpdateThemes;
    TWin32WSCustomPage.ThemeChange(Window);
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

{ TWin32WSCustomPage }

class function TWin32WSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName[0];
    SubClassWndProc := @PageWindowProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // return window handle
  Result := Params.Window;
  ThemeChange(Result);
end;

class procedure TWin32WSCustomPage.DestroyHandle(const AWinControl: TWinControl);
var
  PageIndex, RealIndex: integer;
  PageControlHandle: HWND;
begin
  // remove tab from pagecontrol only if not pfRemoving is set
  // if pfRemoving is set then Tab has been deleted by RemovePage
  if (AWinControl.Parent <> nil) and (AWinControl.Parent.HandleAllocated) and
     not (pfRemoving in TCustomPageAccess(AWinControl).Flags) then
  begin
    PageControlHandle := AWinControl.Parent.Handle;
    PageIndex := TCustomPage(AWinControl).PageIndex;
    RealIndex := TWin32WSCustomNotebook.GetPageRealIndex(TCustomNotebook(AWinControl.Parent), PageIndex);
    if RealIndex <> -1 then
    begin
      Windows.SendMessage(PageControlHandle, TCM_DELETEITEM, Windows.WPARAM(RealIndex), 0);
      AWinControl.Parent.InvalidateClientRectCache(False);
    end;
  end;
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class procedure TWin32WSCustomPage.ThemeChange(Wnd: HWnd);
var
  WindowInfo: PWin32WindowInfo;
begin
  WindowInfo := GetWin32WindowInfo(Wnd);
  if WindowInfo <> nil then
  begin
    with WindowInfo^ do
    begin
      needParentPaint := ThemeServices.ThemesEnabled;
      isTabPage := ThemeServices.ThemesEnabled;
    end;
  end;
end;

class procedure TWin32WSCustomPage.SetText(const AWinControl: TWinControl; const AText: string);
var
  TCI: TC_ITEM;
  PageIndex, RealIndex: integer;
  NotebookHandle: HWND;
begin
  PageIndex := TCustomPage(AWinControl).PageIndex;
  RealIndex := TWin32WSCustomNotebook.GetPageRealIndex(TCustomNotebook(AWinControl.Parent), PageIndex);
  NotebookHandle := AWinControl.Parent.Handle;
  // We can't set label of a page not yet added,
  // Check for valid page index
  if (RealIndex >= 0) and (RealIndex < Windows.SendMessage(NotebookHandle, TCM_GETITEMCOUNT, 0, 0)) then
  begin
    // retrieve page handle from tab as extra check (in case page isn't added yet).
    TCI.mask := TCIF_PARAM;
    Windows.SendMessage(NotebookHandle, TCM_GETITEM, RealIndex, LPARAM(@TCI));
    if PtrUInt(TCI.lParam) = PtrUInt(AWinControl) then
    begin
      Assert(False, Format('Trace:TWin32WSCustomPage.SetText --> %S', [AText]));
      TCI.mask := TCIF_TEXT;
{$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        TCI.pszText := PChar(PWideChar(UTF8ToUTF16(AText)));
        Windows.SendMessage(NotebookHandle, TCM_SETITEMW, RealIndex, LPARAM(@TCI));
      end
      else
      begin
        TCI.pszText := PChar(UTF8ToAnsi(AText));
        Windows.SendMessage(NotebookHandle, TCM_SETITEM, RealIndex, LPARAM(@TCI));
      end;
{$else}
      TCI.pszText := PChar(AText);
      Windows.SendMessage(NotebookHandle, TCM_SETITEM, RealIndex, LPARAM(@TCI));
{$endif}
    end;
  end;
end;

class procedure TWin32WSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  TCI: TC_ITEM;
  PageIndex, RealIndex: integer;
  NotebookHandle: HWND;
begin
  PageIndex := ACustomPage.PageIndex;
  RealIndex := TWin32WSCustomNotebook.GetPageRealIndex(TCustomNotebook(ACustomPage.Parent), PageIndex);
  NotebookHandle := ACustomPage.Parent.Handle;
  // Check for valid page index
  if (RealIndex >= 0) and (RealIndex < Windows.SendMessage(NotebookHandle, TCM_GETITEMCOUNT,0,0)) then
  begin
    // retrieve page handle from tab as extra check (in case page isn't added yet).
    TCI.mask := TCIF_PARAM;
    Windows.SendMessage(NotebookHandle, TCM_GETITEM, RealIndex, LPARAM(@TCI));
    if PtrUInt(TCI.lParam) = PtrUInt(ACustomPage) then
    begin
      TCI.mask := TCIF_IMAGE;
      TCI.iImage := TCustomNotebook(ACustomPage.Parent).GetImageIndex(PageIndex);

      Windows.SendMessage(NotebookHandle, TCM_SETITEM, RealIndex, LPARAM(@TCI));
    end;
  end;
end;

{ TWin32WSCustomNotebook }

class function TWin32WSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
const
  TabPositionFlags: array[TTabPosition] of DWord = (
 { tpTop    } 0,
 { tpBottom } TCS_BOTTOM,
 { tpLeft   } TCS_VERTICAL or TCS_MULTILINE,
 { tpRight  } TCS_VERTICAL or TCS_RIGHT or TCS_MULTILINE
  );
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    Flags := Flags or TabPositionFlags[TCustomNoteBook(AWinControl).TabPosition];
    if nboMultiLine in TCustomNotebook(AWinControl).Options then
      Flags := Flags or TCS_MULTILINE;
    pClassName := WC_TABCONTROL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

  if TCustomNoteBook(AWinControl).Images <> nil then
    SendMessage(Result, TCM_SETIMAGELIST, 0, TCustomNoteBook(AWinControl).Images.Reference._Handle);

  // although we may be child of tabpage, cut the paint chain
  // to improve speed and possible paint anomalities
  Params.WindowInfo^.needParentPaint := false;
end;

class procedure TWin32WSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AIndex: integer);
var
  TCI: TC_ITEM;
begin
  with ANotebook do
  begin
    // other widgetsets allocates handles because they really need this
    // but on windows page handle is differ from tab and thus allocation can be
    // postponed, but this cause problems with event handling like bug #0012434
    // so to overcome such problems we need to allocate this handle
    AChild.HandleNeeded;
    if ShowTabs then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
      // store object as extra, so we can verify we got the right page later
      TCI.lParam := PtrUInt(AChild);
      TCI.iImage := ANotebook.GetImageIndex(NotebookPageRealToLCLIndex(ANotebook, AIndex));
  {$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        TCI.pszText := PChar(PWideChar(UTF8ToUTF16(AChild.Caption)));
        Windows.SendMessage(Handle, TCM_INSERTITEMW, AIndex, LPARAM(@TCI));
      end
      else
      begin
        TCI.pszText := PChar(Utf8ToAnsi(AChild.Caption));
        Windows.SendMessage(Handle, TCM_INSERTITEM, AIndex, LPARAM(@TCI));
      end;
  {$else}
      TCI.pszText := PChar(AChild.Caption);
      Windows.SendMessage(Handle, TCM_INSERTITEM, AIndex, LPARAM(@TCI));
  {$endif}
    end;
    // clientrect possible changed, adding first tab, or deleting last
    // windows should send a WM_SIZE message because of this, but it doesn't
    // send it ourselves
    if LCLControlSizeNeedsUpdate(ANotebook, True) then
      AdjustSizeNotebookPages(ANotebook);
  end;
end;

class procedure TWin32WSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
begin
  RemovePage(ANotebook, AChild.PageIndex);
  AddPage(ANotebook,AChild,NewIndex);
end;

class procedure TWin32WSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook;
  const AIndex: integer);
begin
  Windows.SendMessage(ANotebook.Handle, TCM_DELETEITEM, Windows.WPARAM(AIndex), 0);
  if LCLControlSizeNeedsUpdate(ANotebook, True) then
    AdjustSizeNotebookPages(ANotebook);
end;

{ -----------------------------------------------------------------------------
  Method: AddAllNBPages
  Params: Notebook - A notebook control
  Returns: Nothing

  Adds all pages to notebook (showtabs becomes true)
 ------------------------------------------------------------------------------}
class procedure TWin32WSCustomNotebook.AddAllNBPages(const ANotebook: TCustomNotebook);
var
  TCI: TC_ITEM;
  I, Res, RealIndex: Integer;
  APage: TCustomPage;
  WinHandle: HWND;
begin
  WinHandle := ANotebook.Handle;
  RealIndex := 0;
  for I := 0 to ANotebook.PageCount - 1 do
  begin
    APage := ANotebook.Page[I];
    if not APage.TabVisible and not (csDesigning in APage.ComponentState) then
      continue;
    // check if already shown
    TCI.Mask := TCIF_PARAM;
    Res := Windows.SendMessage(ANotebook.Handle, TCM_GETITEM, RealIndex, LPARAM(@TCI));
    if (Res = 0) or (PtrUInt(TCI.lParam) <> PtrUInt(APage)) then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
      TCI.lParam := PtrUInt(APage);
      TCI.iImage := ANotebook.GetImageIndex(I);
{$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        TCI.pszText := PChar(PWideChar(UTF8ToUTF16(APage.Caption)));
        Windows.SendMessage(WinHandle, TCM_INSERTITEMW, RealIndex, LPARAM(@TCI));
      end
      else
      begin
        TCI.pszText := PChar(Utf8ToAnsi(APage.Caption));
        Windows.SendMessage(WinHandle, TCM_INSERTITEM, RealIndex, LPARAM(@TCI));
      end;
{$else}
      TCI.pszText := PChar(APage.Caption);
      Windows.SendMessage(WinHandle, TCM_INSERTITEM, RealIndex, LPARAM(@TCI));
{$endif}
    end;
    Inc(RealIndex);
  end;
  AdjustSizeNotebookPages(ANotebook);
end;

class procedure TWin32WSCustomNotebook.AdjustSizeNotebookPages(const ANotebook: TCustomNotebook);
var
  I: Integer;
  R: TRect;
  WinHandle: HWND;
  lPage: TCustomPage;
begin
  WinHandle := ANotebook.Handle;
  // Adjust page size to fit in tabcontrol, need bounds of notebook in client of parent
  TWin32WidgetSet(WidgetSet).GetClientRect(WinHandle, R);
  for I := 0 to ANotebook.PageCount - 1 do
  begin
    lPage := ANotebook.Page[I];
    // we don't need to resize non-existing pages yet, they will be sized when created
    if lPage.HandleAllocated then
      SetBounds(lPage, R.Left, R.Top, R.Right, R.Bottom);
  end;
end;

{------------------------------------------------------------------------------
  Method: RemoveAllNBPages
  Params: Notebook - The notebook control
  Returns: Nothing

  Removes all pages from a notebook control (showtabs becomes false)
 ------------------------------------------------------------------------------}
class procedure TWin32WSCustomNotebook.RemoveAllNBPages(const ANotebook: TCustomNotebook);
var
  I: Integer;
  WinHandle: HWND;
begin
  WinHandle := ANotebook.Handle;
  for I := ANotebook.PageCount - 1 downto 0 do
    Windows.SendMessage(WinHandle, TCM_DELETEITEM, Windows.WPARAM(I), 0);
  AdjustSizeNotebookPages(ANotebook);
end;

class function TWin32WSCustomNotebook.GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer;
var
  X: Integer;
begin
  Result := AIndex;
  if csDesigning in ANotebook.ComponentState then exit;
  for X := 0 to AIndex - 1 do
    if ANotebook.Page[X].TabVisible = False then Dec(Result);
end;

procedure SendSelChangeMessage(const ANotebook: TCustomNotebook; const AHandle: HWND;
  const APageIndex: integer);
var
  Mess: TLMNotify;
  NMHdr: tagNMHDR;
begin
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg := LM_NOTIFY;
  FillChar(NMHdr,SizeOf(NMHdr),0);
  NMHdr.code := TCN_SELCHANGE;
  NMHdr.hwndfrom := AHandle;
  NMHdr.idfrom := APageIndex;  //use this to set pageindex to the correct page.
  Mess.NMHdr := @NMHdr;
  DeliverMessage(ANotebook, TLMessage(Mess));
end;

class function TWin32WSCustomNotebook.GetTabIndexAtPos(const ANotebook: TCustomNotebook;
  const AClientPos: TPoint): integer;
var
  hittestInfo: TC_HITTESTINFO;
  Orect: TRect;
begin
  GetLCLClientBoundsOffset(ANotebook, ORect);
  hittestInfo.pt.x := AClientPos.x + ORect.Left;
  hittestInfo.pt.y := AClientPos.y + ORect.Top;
  Result := Windows.SendMessage(ANotebook.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
end;

class function TWin32WSCustomNotebook.GetTabRect(const ANotebook: TCustomNotebook;
  const AIndex: Integer): TRect;
var
  Orect: TRect;
begin
  GetLCLClientBoundsOffset(ANotebook, ORect);
  if Windows.SendMessage(ANotebook.Handle, TCM_GETITEMRECT, WPARAM(AIndex), LPARAM(@Result)) <> 0
  then begin
    Result.Top := Result.Top - Orect.Top;
    Result.Bottom := Result.Bottom - Orect.Top;
    Result.Left := Result.Left - Orect.Left;
    Result.Right := Result.Right - Orect.Left;
  end
  else
    Result := inherited GetTabRect(ANotebook, AIndex);
end;

class function TWin32WSCustomNotebook.GetCapabilities: TNoteBookCapabilities;
begin
  Result:=[nbcMultiLine];
end;

class function TWin32WSCustomNotebook.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
var
  hittestInfo: TC_HITTESTINFO;
  AIndex, ACurIndex: Integer;
begin
  hittestInfo.pt.x := AClientPos.x;
  hittestInfo.pt.y := AClientPos.y;
  AIndex := Windows.SendMessage(AWinControl.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
  ACurIndex := SendMessage(AWinControl.Handle, TCM_GETCURSEL, 0, 0);
  Result := (AIndex <> -1) and (AIndex <> ACurIndex);
end;

class procedure TWin32WSCustomNotebook.SetImageList(
  const ANotebook: TCustomNotebook; const AImageList: TCustomImageList);
begin
  if not WSCheckHandleAllocated(ANotebook, 'SetImageList') then
    Exit;

  if AImageList <> nil then
    SendMessage(ANoteBook.Handle, TCM_SETIMAGELIST, 0, AImageList.Reference._Handle)
  else
    SendMessage(ANoteBook.Handle, TCM_SETIMAGELIST, 0, 0);
  // if you set big images like 32x32 then tabs will be big too => you need to
  // readjust the size of pages
  AdjustSizeNotebookPages(ANotebook);
end;

class procedure TWin32WSCustomNotebook.SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer);
var
  NotebookHandle, OldPageHandle, NewPageHandle: HWND;
  NewRealIndex: Integer;
begin
  NotebookHandle := ANotebook.Handle;
  // get the current top window
  OldPageHandle := GetTopWindow(NotebookHandle);
  NewPageHandle := 0;
  NewRealIndex := GetPageRealIndex(ANotebook, AIndex);

  SendMessage(NotebookHandle, TCM_SETCURSEL, Windows.WParam(NewRealIndex), 0);
  if not (csDestroying in ANotebook.ComponentState) then
  begin
    // create handle if not already done, need to show!
    if (AIndex >= 0) and (AIndex < ANotebook.PageCount) then
    begin
      NewPageHandle := ANotebook.Page[AIndex].Handle;
      Windows.SetWindowPos(NewPageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW or SWP_NOACTIVATE);
      SendSelChangeMessage(ANotebook, NotebookHandle, AIndex);
      NotebookFocusNewControl(ANotebook, AIndex);
    end;
    // traverse children and hide them if needed
    while OldPageHandle <> 0 do
    begin
      // don't touch non-lcl windows
      if (OldPageHandle <> NewPageHandle) and IsWindowVisible(OldPageHandle) and Assigned(GetProp(OldPageHandle, 'WinControl')) then
        Windows.SetWindowPos(OldPageHandle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_HIDEWINDOW or SWP_NOACTIVATE);
      OldPageHandle := GetNextWindow(OldPageHandle, GW_HWNDNEXT);
    end;
  end;
end;

class procedure TWin32WSCustomNotebook.SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
begin
  if ANoteBook.HandleAllocated then
    RecreateWnd(ANoteBook);
end;

class procedure TWin32WSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean);
begin
  if AShowTabs then
    AddAllNBPages(ANotebook)
  else
    RemoveAllNBPages(ANotebook);
end;

class procedure TWin32WSCustomNotebook.UpdateProperties(const ANotebook: TCustomNotebook);
var
  CurrentStyle, NewStyle: cardinal;
begin
  CurrentStyle := GetWindowLong(ANotebook.Handle, GWL_STYLE);
  if (nboMultiLine in ANotebook.Options) or (ANotebook.TabPosition in [tpLeft, tpRight]) then
    NewStyle := CurrentStyle or TCS_MULTILINE
  else
    NewStyle := CurrentStyle and not TCS_MULTILINE;
  if NewStyle <> CurrentStyle then
  begin
    SetWindowLong(ANotebook.Handle, GWL_STYLE, NewStyle);
    SetWindowPos(ANoteBook.Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_DRAWFRAME);
    if LCLControlSizeNeedsUpdate(ANotebook, True) then
      AdjustSizeNotebookPages(ANotebook);
  end;
end;

{$include win32trayicon.inc}

end.
