{%MainUnit win32wscomctrls.pp}
{
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  TCustomPageAccess = class(TCustomPage)
  end;

function IsTabControlGroupFocused(const ATabControl: TCustomTabControl): boolean;
var
  Handle, FocusHandle: HWND;
begin
  Result := False;
  if not ATabControl.HandleAllocated then exit;
  Handle := ATabControl.Handle;
  FocusHandle := Windows.GetFocus;
  while (FocusHandle <> 0) and (FocusHandle <> Handle) do
    FocusHandle := Windows.GetParent(FocusHandle);
  if FocusHandle = 0 then exit;
  Result := True;
end;

{ sets focus to a control on the newly focused tab page }
procedure TabControlFocusNewControl(const ATabControl: TCustomTabControl; NewIndex: integer);
var
  Page: TCustomPage;
  AWinControl: TWinControl;
  ParentForm: TCustomForm;
begin
  { see if currently focused control is within tab control }
  if not IsTabControlGroupFocused(ATabControl) then exit;

  { focus was/is within tab control, pick a new control to focus }
  Page := ATabControl.CustomPage(NewIndex);
  ParentForm := GetParentForm(ATabControl);
  if ParentForm <> nil then
  begin
    if ATabControl.ContainsControl(ParentForm.ActiveControl) and (ParentForm.ActiveControl <> ATabControl) then
    begin
      // if ActiveControl is already on active page
      if Page.IsParentOf(ParentForm.ActiveControl) then Exit;
      AWinControl := nil;
      if Page.CanFocus then
        AWinControl := TCustomPageAccess(Page).FindNextControl(nil, True, True, False);
      // if nothing to focus then focus tab control then we can traverse pages by keys
      if AWinControl = nil then
        AWinControl := ATabControl;
      AWinControl.SetFocus;
    end;
  end;
end;

function ShowHideTabPage(TabControlHandle: HWnd; Showing: boolean): integer;
const
  ShowFlags: array[Boolean] of DWord = (SWP_HIDEWINDOW or SWP_NOZORDER, SWP_SHOWWINDOW);
var
  TabControl: TCustomTabControl;
  PageIndex: Integer;
  PageHandle: HWND;
begin
  TabControl := GetWin32WindowInfo(TabControlHandle)^.WinControl as TCustomTabControl;
  PageIndex := Windows.SendMessage(TabControlHandle, TCM_GETCURSEL, 0, 0);
  PageIndex := TabControl.TabToPageIndex(PageIndex);

  if (TabControl is TTabControl) then
    exit(PageIndex);

  if PageIndex = -1 then
    exit(PageIndex); //DONE: must return something!

  PageHandle := TabControl.CustomPage(PageIndex).Handle;
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
begin
  // remove tab from pagecontrol only if not pfRemoving is set
  // if pfRemoving is set then Tab has been deleted by RemovePage
  if (AWinControl.Parent <> nil) and (AWinControl.Parent.HandleAllocated) and
     not (pfRemoving in TCustomPageAccess(AWinControl).Flags) then
  begin
    PageIndex := TCustomPage(AWinControl).PageIndex;
    RealIndex := TCustomTabControl(AWinControl.Parent).PageToTabIndex(PageIndex);
    if RealIndex <> -1 then
    begin
      TWin32WSCustomTabControl.DeletePage(TCustomTabControl(AWinControl.Parent), RealIndex);
      AWinControl.Parent.InvalidateClientRectCache(False);
    end;
  end;
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class procedure TWin32WSCustomPage.ThemeChange(Wnd: HWND);
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
  TabControl: TCustomTabControl;
  PageIndex, RealIndex: integer;
  TabControlHandle: HWND;
begin
  TabControl := TCustomTabControl(AWinControl.Parent);
  PageIndex := TCustomPage(AWinControl).PageIndex;
  RealIndex := TabControl.PageToTabIndex(PageIndex);
  TabControlHandle := TabControl.Handle;
  // We can't set label of a page not yet added,
  // Check for valid page index
  if (RealIndex >= 0) and (RealIndex < Windows.SendMessage(TabControlHandle, TCM_GETITEMCOUNT, 0, 0)) then
  begin
    // retrieve page handle from tab as extra check (in case page isn't added yet).
    TCI.mask := TCIF_PARAM;
    Windows.SendMessage(TabControlHandle, TCM_GETITEM, RealIndex, LPARAM(@TCI));
    if PtrUInt(TCI.lParam) = PtrUInt(AWinControl) then
    begin
      TCI.mask := TCIF_TEXT;
      TCI.pszText := PChar(PWideChar(UTF8ToUTF16(AText)));
      Windows.SendMessage(TabControlHandle, TCM_SETITEMW, RealIndex, LPARAM(@TCI));
      LCLControlSizeNeedsUpdate(TabControl, True);
    end;
  end;
end;

class procedure TWin32WSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  TCI: TC_ITEM;
  PageIndex, RealIndex: integer;
  TabControlHandle: HWND;
begin
  PageIndex := ACustomPage.PageIndex;
  RealIndex := TCustomTabControl(ACustomPage.Parent).PageToTabIndex(PageIndex);
  TabControlHandle := ACustomPage.Parent.Handle;
  // Check for valid page index
  if (RealIndex >= 0) and (RealIndex < Windows.SendMessage(TabControlHandle, TCM_GETITEMCOUNT,0,0)) then
  begin
    // retrieve page handle from tab as extra check (in case page isn't added yet).
    TCI.mask := TCIF_PARAM;
    Windows.SendMessage(TabControlHandle, TCM_GETITEM, RealIndex, LPARAM(@TCI));
    if PtrUInt(TCI.lParam) = PtrUInt(ACustomPage) then
    begin
      TCI.mask := TCIF_IMAGE;
      TCI.iImage := TCustomTabControl(ACustomPage.Parent).GetImageIndex(PageIndex);

      Windows.SendMessage(TabControlHandle, TCM_SETITEM, RealIndex, LPARAM(@TCI));
    end;
  end;
end;

{ TWin32WSCustomTabControl }

function TabControlParentMsgHandler(const AWinControl: TWinControl; Window: HWnd;
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
            TabControlFocusNewControl(AWinControl as TCustomTabControl, idFrom);
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

procedure TabControlClientOffset(const AWinControl: TWinControl;
  var ORect: Rect);
var
  ARect: TRect;
begin
  // Can't use complete client rect in win32 interface, top part contains the tabs
  Windows.GetClientRect(AWinControl.Handle, @ARect);
  ORect := ARect;
  Windows.SendMessage(AWinControl.Handle, TCM_AdjustRect, 0, LPARAM(@ORect));
  Dec(ORect.Right, ARect.Right);
  Dec(ORect.Bottom, ARect.Bottom);
end;

class procedure TWin32WSCustomTabControl.DeletePage(
  const ATabControl: TCustomTabControl; const AIndex: integer);

var
  Wnd: HWND;

  function TabsScrollingNeeded: Boolean;
  var
    HitTestInfo: TC_HITTESTINFO;
    ARect: TRect;
    TabCount, FirstShowedIndex: Integer;
  begin
    if AIndex <= 0 then Exit(False);

    TabCount := Windows.SendMessage(Wnd, TCM_GETITEMCOUNT, 0, 0);
    if AIndex < TabCount - 1 then Exit(False);

    // we have to look, if the first shown tab is the tab that is to be deleted
    Windows.GetClientRect(Wnd, @ARect);
    Windows.SendMessage(Wnd, TCM_AdjustRect, 0, LPARAM(@ARect));

    HitTestInfo.pt.x := ARect.Left;
    HitTestInfo.pt.y := ARect.Top div 2;
    FirstShowedIndex := Windows.SendMessage(Wnd, TCM_HITTEST, 0, LPARAM(@HitTestInfo));

    Result := (FirstShowedIndex > 0) and (FirstShowedIndex = AIndex);
  end;

begin
// There is a bug in Windows. When only one tab is left in a scrolled Tab Control
// and this is deleted, Windows doesn't scroll it automatically. So we have to
// do it manually. See Mantis #19278
  Wnd := ATabControl.Handle;
  if TabsScrollingNeeded then
    Windows.SendMessage(Wnd, TCM_SETCURSEL, Windows.WPARAM(AIndex - 1), 0);
  Windows.SendMessage(Wnd, TCM_DELETEITEM, Windows.WPARAM(AIndex), 0);
end;

function CustomTabControlWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; stdcall;
var
  Control: TWinControl;
  LMessage: TLMessage;
begin
  case Msg of
    WM_ERASEBKGND:
      begin
        if not ThemeServices.ThemesEnabled then
        begin
          // prevent flickering
          Control := GetWin32WindowInfo(Window)^.WinControl;
          LMessage.msg := Msg;
          LMessage.wParam := WParam;
          LMessage.lParam := LParam;
          LMessage.Result := 0;
          Result := DeliverMessage(Control, LMessage);
          Exit;
        end;
      end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

class function TWin32WSCustomTabControl.CreateHandle(const AWinControl: TWinControl;
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
  T: TCustomTabControl;
begin
  T := TCustomTabControl(AWinControl);
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    SubClassWndProc := @CustomTabControlWndProc;
    if AWinControl is TTabControl then begin
      // TTabControl is not really a TabControl, it is a container, and has a child that does the tabs
      pClassName := @ClsName[0];
    end
    else begin
      Flags := Flags or TabPositionFlags[T.TabPosition];
      if nboMultiLine in T.Options then
        Flags := Flags or TCS_MULTILINE;
      if T.TabWidth > 0 then
        Flags := Flags or TCS_FIXEDWIDTH;
      pClassName := WC_TABCONTROL;
    end;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;

  if not (AWinControl is TTabControl) then begin
    if T.Images <> nil then
      SendMessage(Result, TCM_SETIMAGELIST, 0, T.Images.ReferenceForPPI[T.ImagesWidth, T.Font.PixelsPerInch]._Handle);

    // although we may be child of tabpage, cut the paint chain
    // to improve speed and possible paint anomalities
    Params.WindowInfo^.ParentMsgHandler := @TabControlParentMsgHandler;
    Params.WindowInfo^.needParentPaint := false;
    Params.WindowInfo^.ClientOffsetProc := @TabControlClientOffset;

    SendMessage(Result, TCM_SETITEMSIZE, 0, MakeLParam(
      T.TabWidth,
      T.TabHeight));
  end;
end;

class procedure TWin32WSCustomTabControl.AddPage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AIndex: integer);
var
  TCI: TC_ITEM;
begin
  if ATabControl is TTabControl then
    exit;

  with ATabControl do
  begin
    // other widgetsets allocates handles because they really need this
    // but on windows page handle is differ from tab and thus allocation can be
    // postponed, but this cause problems with event handling like bug #0012434
    // so to overcome such problems we need to allocate this handle
    if not (ATabControl is TTabControl) then
      AChild.HandleNeeded;
    if ShowTabs then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
      // store object as extra, so we can verify we got the right page later
      TCI.lParam := PtrInt(AChild);
      TCI.iImage := ATabControl.GetImageIndex(ATabControl.TabToPageIndex(AIndex));
      TCI.pszText := PChar(PWideChar(UTF8ToUTF16(AChild.Caption)));
      Windows.SendMessage(Handle, TCM_INSERTITEMW, AIndex, LPARAM(@TCI));
    end;
    // clientrect possible changed, adding first tab, or deleting last
    // windows should send a WM_SIZE message because of this, but it doesn't
    // send it ourselves
    if LCLControlSizeNeedsUpdate(ATabControl, True) then
      AdjustSizeTabControlPages(ATabControl);
  end;
end;

class procedure TWin32WSCustomTabControl.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
var
  Index: Integer;
  TCI: TC_ITEM;
begin
  if ATabControl is TTabControl then
    exit;

  if not ATabControl.ShowTabs then
    Exit;

  Index := AChild.VisibleIndex;
  TCI.Mask := TCIF_IMAGE or TCIF_PARAM;
  Windows.SendMessage(ATabControl.Handle, TCM_GETITEMW, Windows.WPARAM(Index), LParam(@TCI));
  Windows.SendMessage(ATabControl.Handle, TCM_DELETEITEM, Windows.WPARAM(Index), 0);
  TCI.Mask := TCI.Mask or TCIF_TEXT;
  TCI.pszText := PChar(PWideChar(UTF8ToUTF16(AChild.Caption)));
  Windows.SendMessage(ATabControl.Handle, TCM_INSERTITEMW, NewIndex, LPARAM(@TCI));
  if LCLControlSizeNeedsUpdate(ATabControl, True) then
    AdjustSizeTabControlPages(ATabControl);
end;

class procedure TWin32WSCustomTabControl.RemovePage(const ATabControl: TCustomTabControl;
  const AIndex: integer);
begin
  if ATabControl is TTabControl then
    exit;

  DeletePage(ATabControl, AIndex);
  if LCLControlSizeNeedsUpdate(ATabControl, True) then
    AdjustSizeTabControlPages(ATabControl);
end;

class function TWin32WSCustomTabControl.GetNotebookMinTabHeight(const AWinControl: TWinControl): integer;
var
  R: TRect;
begin
  if AWinControl is TTabControl then begin
    Result := 0;;
    exit;
  end;

  if (not GetLCLClientBoundsOffset(AWinControl, R)) then begin
    Result := inherited GetNotebookMinTabHeight(AWinControl);
    exit;
  end;
  // The bigger offset is the height of the tab
  Result := Max(R.Top, -R.Bottom)
  // but includes spacing for the child
          - 1;
end;

class function TWin32WSCustomTabControl.GetNotebookMinTabWidth(const AWinControl: TWinControl): integer;
var
  R: TRect;
begin
  if AWinControl is TTabControl then begin
    Result := 0;;
    exit;
  end;

  if (not GetLCLClientBoundsOffset(AWinControl, R)) then begin
    Result := inherited GetNotebookMinTabHeight(AWinControl);
    exit;
  end;
  Result := Max(R.Left, -R.Right)
          - 1;
end;

{ -----------------------------------------------------------------------------
  Method: AddAllNBPages
  Adds all pages to tab control (showtabs becomes true)
 ------------------------------------------------------------------------------}
class procedure TWin32WSCustomTabControl.AddAllNBPages(const ATabControl: TCustomTabControl);
var
  TCI: TC_ITEM;
  I, Res, RealIndex: Integer;
  APage: TCustomPage;
  WinHandle: HWND;
begin
  if ATabControl is TTabControl then
    exit;

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
      TCI.pszText := PChar(PWideChar(UTF8ToUTF16(APage.Caption)));
      Windows.SendMessage(WinHandle, TCM_INSERTITEMW, RealIndex, LPARAM(@TCI));
    end;
    Inc(RealIndex);
  end;
  SetPageIndex(ATabControl, ATabControl.PageIndex); // index may not have been updated while pages where hidden
  AdjustSizeTabControlPages(ATabControl);
end;

class procedure TWin32WSCustomTabControl.AdjustSizeTabControlPages(const ATabControl: TCustomTabControl);
var
  I: Integer;
  R: TRect;
  WinHandle: HWND;
  lPage: TCustomPage;
begin
  if ATabControl is TTabControl then
    exit;

  WinHandle := ATabControl.Handle;
  // Adjust page size to fit in tabcontrol, need bounds of tab control in client of parent
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

  Removes all pages from a tab control (showtabs becomes false)
 ------------------------------------------------------------------------------}
class procedure TWin32WSCustomTabControl.RemoveAllNBPages(const ATabControl: TCustomTabControl);
var
  I: Integer;
  WinHandle: HWND;
begin
  if ATabControl is TTabControl then
    exit;

  WinHandle := ATabControl.Handle;
  for I := ATabControl.PageCount - 1 downto 0 do
    Windows.SendMessage(WinHandle, TCM_DELETEITEM, Windows.WPARAM(I), 0);
  AdjustSizeTabControlPages(ATabControl);
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

class function TWin32WSCustomTabControl.GetTabIndexAtPos(const ATabControl: TCustomTabControl;
  const AClientPos: TPoint): integer;
var
  hittestInfo: TC_HITTESTINFO;
  Orect: TRect;
begin
  if ATabControl is TTabControl then begin
    Result := 0;;
    exit;
  end;

  GetLCLClientBoundsOffset(ATabControl, ORect);
  hittestInfo.pt.x := AClientPos.x + ORect.Left;
  hittestInfo.pt.y := AClientPos.y + ORect.Top;
  Result := Windows.SendMessage(ATabControl.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
end;

class function TWin32WSCustomTabControl.GetTabRect(const ATabControl: TCustomTabControl;
  const AIndex: Integer): TRect;
var
  Orect: TRect;
begin
  if ATabControl is TTabControl then begin
    Result.Top := 0;;
    Result.Left := 0;;
    Result.Bottom := 0;;
    Result.Right := 0;;
    exit;
  end;

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

class function TWin32WSCustomTabControl.GetCapabilities: TCTabControlCapabilities;
begin
  Result:=[nbcMultiLine, nbcTabsSizeable];
end;

class function TWin32WSCustomTabControl.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
var
  hittestInfo: TC_HITTESTINFO;
  AIndex, ACurIndex: Integer;
begin
  if AWinControl is TTabControl then begin
    Result := inherited GetDesignInteractive(AWinControl, AClientPos);
    exit;
  end;

  hittestInfo.pt.x := AClientPos.x;
  hittestInfo.pt.y := AClientPos.y;
  AIndex := Windows.SendMessage(AWinControl.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
  ACurIndex := SendMessage(AWinControl.Handle, TCM_GETCURSEL, 0, 0);
  Result := (AIndex <> -1) and (AIndex <> ACurIndex);
end;

class procedure TWin32WSCustomTabControl.SetTabSize(
  const ATabControl: TCustomTabControl;
  const ATabWidth, ATabHeight: integer);
begin
  if ATabControl is TTabControl then
    exit;

  Windows.SendMessage(ATabControl.Handle, TCM_SETITEMSIZE,
    0, MakeLParam(ATabWidth, ATabHeight));
end;

class procedure TWin32WSCustomTabControl.SetImageList(
  const ATabControl: TCustomTabControl; const AImageList: TCustomImageListResolution);
begin
  if ATabControl is TTabControl then
    exit;

  if not WSCheckHandleAllocated(ATabControl, 'SetImageList') then
    Exit;

  if AImageList <> nil then
    SendMessage(ATabControl.Handle, TCM_SETIMAGELIST, 0, AImageList.Reference._Handle)
  else
    SendMessage(ATabControl.Handle, TCM_SETIMAGELIST, 0, 0);
  // if you set big images like 32x32 then tabs will be big too => you need to
  // readjust the size of pages
  AdjustSizeTabControlPages(ATabControl);
end;

class procedure TWin32WSCustomTabControl.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  TabControlHandle, OldPageHandle, NewPageHandle: HWND;
  NewRealIndex: Integer;
begin
  if ATabControl is TTabControl then
    exit;

  TabControlHandle := ATabControl.Handle;
  // get the current top window
  OldPageHandle := GetTopWindow(TabControlHandle);
  NewPageHandle := 0;
  NewRealIndex := ATabControl.PageToTabIndex(AIndex);

  SendMessage(TabControlHandle, TCM_SETCURSEL, Windows.WParam(NewRealIndex), 0);

  if (ATabControl is TTabControl) then
    exit; //all done

  if not (csDestroying in ATabControl.ComponentState) then
  begin
    // create handle if not already done, need to show!
    if (AIndex >= 0) and (AIndex < ATabControl.PageCount) then
    begin
      NewPageHandle := ATabControl.Page[AIndex].Handle;
      Windows.SetWindowPos(NewPageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW or SWP_NOACTIVATE);
      SendSelChangeMessage(ATabControl, TabControlHandle, AIndex);
      TabControlFocusNewControl(ATabControl, AIndex);
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

class procedure TWin32WSCustomTabControl.SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
begin
  if ATabControl is TTabControl then
    exit;

  if ATabControl.HandleAllocated then
    RecreateWnd(ATabControl);
end;

class procedure TWin32WSCustomTabControl.ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
begin
  if ATabControl is TTabControl then
    exit;

  if AShowTabs then
    AddAllNBPages(ATabControl)
  else
    RemoveAllNBPages(ATabControl);
end;

class procedure TWin32WSCustomTabControl.UpdateProperties(const ATabControl: TCustomTabControl);
var
  CurrentStyle, NewStyle: cardinal;
begin
  if ATabControl is TTabControl then
    exit;

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
      AdjustSizeTabControlPages(ATabControl);
  end;
end;

