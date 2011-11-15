{%MainUnit win32wscomctrls.pp}
{
 *****************************************************************************
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

type
  TCustomPageAccess = class(TCustomPage)
  end;

function IsNotebookGroupFocused(const ATabControl: TCustomTabControl): boolean;
var
  lNotebookHandle, lWindow: HWND;
begin
  result := false;
  if not ATabControl.HandleAllocated then exit;
  lNotebookHandle := ATabControl.Handle;
  lWindow := Windows.GetFocus;
  while (lWindow <> 0) and (lWindow <> lNotebookHandle) do
    lWindow := Windows.GetParent(lWindow);
  if lWindow = 0 then exit;
  result := true;
end;

{ sets focus to a control on the newly focused tab page }
procedure NotebookFocusNewControl(const ATabControl: TCustomTabControl; NewIndex: integer);
var
  Page: TCustomPage;
  AWinControl: TWinControl;
  ParentForm: TCustomForm;
begin
  { see if currently focused control is within notebook }
  if not IsNotebookGroupFocused(ATabControl) then exit;

  { focus was/is within notebook, pick a new control to focus }
  Page := ATabControl.CustomPage(NewIndex);
  ParentForm := GetParentForm(ATabControl);
  if ParentForm <> nil then
  begin
    if ATabControl.ContainsControl(ParentForm.ActiveControl) and (ParentForm.ActiveControl <> ATabControl) then
    begin
      AWinControl := nil;
      if Page.CanFocus then
        AWinControl := TCustomPageAccess(Page).FindNextControl(nil, True, True, False);
      // if nothing to focus then focus notebook then we can traverse pages by keys
      if AWinControl = nil then
        AWinControl := ATabControl;
      AWinControl.SetFocus;
    end;
  end;
end;

function NotebookPageRealToLCLIndex(const ATabControl: TCustomTabControl; AIndex: integer): integer;
begin
  Result := ATabControl.TabToPageIndex(AIndex);
end;

function ShowHideTabPage(NotebookHandle: HWnd; Showing: boolean): integer;
const
  ShowFlags: array[Boolean] of DWord = (SWP_HIDEWINDOW or SWP_NOZORDER, SWP_SHOWWINDOW);
var
  NoteBook: TCustomTabControl;
  PageIndex: Integer;
  PageHandle: HWND;
begin
  Notebook := GetWin32WindowInfo(NotebookHandle)^.WinControl as TCustomTabControl;
  PageIndex := Windows.SendMessage(NotebookHandle, TCM_GETCURSEL, 0, 0);
  PageIndex := NotebookPageRealToLCLIndex(Notebook, PageIndex);

  if NoteBook.IsUnpaged then
    exit(PageIndex);

  if PageIndex = -1 then
    exit(PageIndex); //DONE: must return something!

  PageHandle := Notebook.CustomPage(PageIndex).Handle;
  Windows.SetWindowPos(PageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or ShowFlags[Showing]);
  Windows.RedrawWindow(PageHandle, nil, 0, RDW_INVALIDATE or RDW_ALLCHILDREN or RDW_ERASE);
  Result := PageIndex;
end;

function PageWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Info: PWin32WindowInfo;
begin
  case Msg of
    WM_THEMECHANGED:
      begin
        ThemeServices.UpdateThemes;
        TWin32WSCustomPage.ThemeChange(Window);
      end;
    WM_SIZE:
      begin
        Info := GetWin32WindowInfo(Window);
        if (Info^.WinControl.Parent is TCustomTabControl) then
        begin
          // the TCustomPage size is the ClientRect size of the parent
          // => invalidate the Parent.ClientRect
          Info^.WinControl.Parent.InvalidateClientRectCache(false);
        end;
      end;
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
    RealIndex := TCustomTabControl(AWinControl.Parent).PageToTabIndex(PageIndex);
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
  RealIndex := TCustomTabControl(AWinControl.Parent).PageToTabIndex(PageIndex);
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
  RealIndex := TCustomTabControl(ACustomPage.Parent).PageToTabIndex(PageIndex);
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
      TCI.iImage := TCustomTabControl(ACustomPage.Parent).GetImageIndex(PageIndex);

      Windows.SendMessage(NotebookHandle, TCM_SETITEM, RealIndex, LPARAM(@TCI));
    end;
  end;
end;

{ TWin32WSCustomNotebook }

function NotebookParentMsgHandler(const AWinControl: TWinControl; Window: HWnd;
      Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam;
      var MsgResult: Windows.LResult; var WinProcess: Boolean): Boolean;
var
  NMHdr: PNMHDR;
  LMNotify: TLMNotify;
begin
  Result := False;
  if Msg = WM_NOTIFY then
  begin
    NMHdr := PNMHDR(LParam);
    with NMHdr^ do
      case code of
        TCN_SELCHANGE:
          begin
            Result := True;
            idFrom := ShowHideTabPage(HWndFrom, True);
            with LMNotify Do
            begin
              Msg := LM_NOTIFY;
              IDCtrl := WParam;
              NMHdr := PNMHDR(LParam);
              Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
            end;
            DeliverMessage(AWinControl, LMNotify);
            NotebookFocusNewControl(AWinControl as TCustomTabControl, idFrom);
            MsgResult := LMNotify.Result;
          end;
        TCN_SELCHANGING:
          begin
            Result := True;
            with LMNotify Do
            begin
              Msg := LM_NOTIFY;
              IDCtrl := WParam;
              NMHdr := PNMHDR(LParam);
              Result := CallDefaultWindowProc(Window, Msg, WParam, LParam);
            end;
            DeliverMessage(AWinControl, LMNotify);
            if LMNotify.Result = 0 then
              ShowHideTabPage(HWndFrom, False);
            MsgResult := LMNotify.Result;
          end;
      end;
  end;
end;

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
    Flags := Flags or TabPositionFlags[TCustomTabControl(AWinControl).TabPosition];
    if nboMultiLine in TCustomTabControl(AWinControl).Options then
      Flags := Flags or TCS_MULTILINE;
    pClassName := WC_TABCONTROL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

  if TCustomTabControl(AWinControl).Images <> nil then
    SendMessage(Result, TCM_SETIMAGELIST, 0, TCustomTabControl(AWinControl).Images.Reference._Handle);

  // although we may be child of tabpage, cut the paint chain
  // to improve speed and possible paint anomalities
  Params.WindowInfo^.ParentMsgHandler := @NotebookParentMsgHandler;
  Params.WindowInfo^.needParentPaint := false;
end;

class procedure TWin32WSCustomNotebook.AddPage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AIndex: integer);
var
  TCI: TC_ITEM;
begin
  with ATabControl do
  begin
    // other widgetsets allocates handles because they really need this
    // but on windows page handle is differ from tab and thus allocation can be
    // postponed, but this cause problems with event handling like bug #0012434
    // so to overcome such problems we need to allocate this handle
    if not ATabControl.IsUnpaged then
      AChild.HandleNeeded;
    if ShowTabs then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
      // store object as extra, so we can verify we got the right page later
      TCI.lParam := PtrUInt(AChild);
      TCI.iImage := ATabControl.GetImageIndex(NotebookPageRealToLCLIndex(ATabControl, AIndex));
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
    if LCLControlSizeNeedsUpdate(ATabControl, True) then
      AdjustSizeNotebookPages(ATabControl);
  end;
end;

class procedure TWin32WSCustomNotebook.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
begin
  RemovePage(ATabControl, AChild.PageIndex);
  AddPage(ATabControl,AChild,NewIndex);
end;

class procedure TWin32WSCustomNotebook.RemovePage(const ATabControl: TCustomTabControl;
  const AIndex: integer);
begin
  Windows.SendMessage(ATabControl.Handle, TCM_DELETEITEM, Windows.WPARAM(AIndex), 0);
  if LCLControlSizeNeedsUpdate(ATabControl, True) then
    AdjustSizeNotebookPages(ATabControl);
end;

{ -----------------------------------------------------------------------------
  Method: AddAllNBPages
  Params: Notebook - A notebook control
  Returns: Nothing

  Adds all pages to notebook (showtabs becomes true)
 ------------------------------------------------------------------------------}
class procedure TWin32WSCustomNotebook.AddAllNBPages(const ATabControl: TCustomTabControl);
var
  TCI: TC_ITEM;
  I, Res, RealIndex: Integer;
  APage: TCustomPage;
  WinHandle: HWND;
begin
  WinHandle := ATabControl.Handle;
  RealIndex := 0;
  for I := 0 to ATabControl.PageCount - 1 do
  begin
    APage := ATabControl.Page[I];
    if not APage.TabVisible and not (csDesigning in APage.ComponentState) then
      continue;
    // check if already shown
    TCI.Mask := TCIF_PARAM;
    Res := Windows.SendMessage(ATabControl.Handle, TCM_GETITEM, RealIndex, LPARAM(@TCI));
    if (Res = 0) or (PtrUInt(TCI.lParam) <> PtrUInt(APage)) then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
      TCI.lParam := PtrUInt(APage);
      TCI.iImage := ATabControl.GetImageIndex(I);
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
  AdjustSizeNotebookPages(ATabControl);
end;

class procedure TWin32WSCustomNotebook.AdjustSizeNotebookPages(const ATabControl: TCustomTabControl);
var
  I: Integer;
  R: TRect;
  WinHandle: HWND;
  lPage: TCustomPage;
begin
  WinHandle := ATabControl.Handle;
  // Adjust page size to fit in tabcontrol, need bounds of notebook in client of parent
  TWin32WidgetSet(WidgetSet).GetClientRect(WinHandle, R);
  for I := 0 to ATabControl.PageCount - 1 do
  begin
    lPage := ATabControl.Page[I];
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
class procedure TWin32WSCustomNotebook.RemoveAllNBPages(const ATabControl: TCustomTabControl);
var
  I: Integer;
  WinHandle: HWND;
begin
  WinHandle := ATabControl.Handle;
  for I := ATabControl.PageCount - 1 downto 0 do
    Windows.SendMessage(WinHandle, TCM_DELETEITEM, Windows.WPARAM(I), 0);
  AdjustSizeNotebookPages(ATabControl);
end;

procedure SendSelChangeMessage(const ATabControl: TCustomTabControl; const AHandle: HWND;
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
  DeliverMessage(ATabControl, TLMessage(Mess));
end;

class function TWin32WSCustomNotebook.GetTabIndexAtPos(const ATabControl: TCustomTabControl;
  const AClientPos: TPoint): integer;
var
  hittestInfo: TC_HITTESTINFO;
  Orect: TRect;
begin
  GetLCLClientBoundsOffset(ATabControl, ORect);
  hittestInfo.pt.x := AClientPos.x + ORect.Left;
  hittestInfo.pt.y := AClientPos.y + ORect.Top;
  Result := Windows.SendMessage(ATabControl.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
end;

class function TWin32WSCustomNotebook.GetTabRect(const ATabControl: TCustomTabControl;
  const AIndex: Integer): TRect;
var
  Orect: TRect;
begin
  GetLCLClientBoundsOffset(ATabControl, ORect);
  if Windows.SendMessage(ATabControl.Handle, TCM_GETITEMRECT, WPARAM(AIndex), LPARAM(@Result)) <> 0
  then begin
    Result.Top := Result.Top - Orect.Top;
    Result.Bottom := Result.Bottom - Orect.Top;
    Result.Left := Result.Left - Orect.Left;
    Result.Right := Result.Right - Orect.Left;
  end
  else
    Result := inherited GetTabRect(ATabControl, AIndex);
end;

class function TWin32WSCustomNotebook.GetCapabilities: TCTabControlCapabilities;
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
  const ATabControl: TCustomTabControl; const AImageList: TCustomImageList);
begin
  if not WSCheckHandleAllocated(ATabControl, 'SetImageList') then
    Exit;

  if AImageList <> nil then
    SendMessage(ATabControl.Handle, TCM_SETIMAGELIST, 0, AImageList.Reference._Handle)
  else
    SendMessage(ATabControl.Handle, TCM_SETIMAGELIST, 0, 0);
  // if you set big images like 32x32 then tabs will be big too => you need to
  // readjust the size of pages
  AdjustSizeNotebookPages(ATabControl);
end;

class procedure TWin32WSCustomNotebook.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  NotebookHandle, OldPageHandle, NewPageHandle: HWND;
  NewRealIndex: Integer;
begin
  NotebookHandle := ATabControl.Handle;
  // get the current top window
  OldPageHandle := GetTopWindow(NotebookHandle);
  NewPageHandle := 0;
  NewRealIndex := ATabControl.PageToTabIndex(AIndex);

  SendMessage(NotebookHandle, TCM_SETCURSEL, Windows.WParam(NewRealIndex), 0);

  if ATabControl.IsUnpaged then
    exit; //all done

  if not (csDestroying in ATabControl.ComponentState) then
  begin
    // create handle if not already done, need to show!
    if (AIndex >= 0) and (AIndex < ATabControl.PageCount) then
    begin
      NewPageHandle := ATabControl.Page[AIndex].Handle;
      Windows.SetWindowPos(NewPageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW or SWP_NOACTIVATE);
      SendSelChangeMessage(ATabControl, NotebookHandle, AIndex);
      NotebookFocusNewControl(ATabControl, AIndex);
    end;
    // traverse children and hide them if needed
    while OldPageHandle <> 0 do
    begin
      // don't touch non-lcl windows
      if (OldPageHandle <> NewPageHandle) and IsWindowVisible(OldPageHandle) and Assigned(LCLIntf.GetProp(OldPageHandle, 'WinControl')) then
        Windows.SetWindowPos(OldPageHandle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER or SWP_HIDEWINDOW or SWP_NOACTIVATE);
      OldPageHandle := GetNextWindow(OldPageHandle, GW_HWNDNEXT);
    end;
  end;
end;

class procedure TWin32WSCustomNotebook.SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
begin
  if ATabControl.HandleAllocated then
    RecreateWnd(ATabControl);
end;

class procedure TWin32WSCustomNotebook.ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
begin
  if AShowTabs then
    AddAllNBPages(ATabControl)
  else
    RemoveAllNBPages(ATabControl);
end;

class procedure TWin32WSCustomNotebook.UpdateProperties(const ATabControl: TCustomTabControl);
var
  CurrentStyle, NewStyle: cardinal;
begin
  CurrentStyle := GetWindowLong(ATabControl.Handle, GWL_STYLE);
  if (nboMultiLine in ATabControl.Options) or (ATabControl.TabPosition in [tpLeft, tpRight]) then
    NewStyle := CurrentStyle or TCS_MULTILINE
  else
    NewStyle := CurrentStyle and not TCS_MULTILINE;
  if NewStyle <> CurrentStyle then
  begin
    SetWindowLong(ATabControl.Handle, GWL_STYLE, NewStyle);
    SetWindowPos(ATabControl.Handle, 0, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_DRAWFRAME);
    if LCLControlSizeNeedsUpdate(ATabControl, True) then
      AdjustSizeNotebookPages(ATabControl);
  end;
end;

