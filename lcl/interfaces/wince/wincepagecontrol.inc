{%MainUnit wincewscomctrls.pp}
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
var
  I: Integer;
begin
  Result := AIndex;
  if csDesigning in ATabControl.ComponentState then exit;
  I := 0;
  while (I < ATabControl.PageCount) and (I <= Result) do
  begin
    if not ATabControl.Page[I].TabVisible then Inc(Result);
    Inc(I);
  end;
end;

{ TWinCEWSCustomPage }

function PageWindowProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam;
    LParam: Windows.LParam): LResult; {$ifdef win32}stdcall{$else}cdecl{$endif};
begin
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

class function TWinCEWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params, PanelParams: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
  lPanel: TPanel;
  WindowInfo: PWindowInfo;
begin
//  DebugLn('Creating CustomPage A');
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_TAB_CLASSES;
  InitCommonControlsEx(@init);

  // First create the TabSheet

  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName;
    SubClassWndProc := @PageWindowProc;
    Flags := Flags and not WS_VISIBLE;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);

  // return window handle
  Result := Params.Window;
  Params.WindowInfo^.ParentPanel := Params.Window;

{  // The standard control created to show a tabsheet is unable to
  // show non-windowed controls inside it, no matter what is done.
  // The found solution was to add a panel to it and place all child
  // controls inside the panel instead.
  lPanel := TPanel.Create(nil);
  lPanel.BevelOuter := bvNone;
  lPanel.Left := 0;
  lPanel.Top := 0;
  lPanel.Width := Params.Width;
  lPanel.Height := Params.Height;
  lPanel.Align := alClient;
  lPanel.ParentWindow := Params.Window;
  lPanel.HandleNeeded;
  WindowInfo := GetWindowInfo(lPanel.Handle);

  // return window handle
  Result := lPanel.Handle;
  WindowInfo^.ParentPanel := Params.Window;}

//  DebugLn(Format('Creating CustomPage B Panel: %s Page: %s',
//    [IntToHex(PanelParams.Window, 8), IntToHex(Params.Window, 8)]));
end;

class procedure TWinCEWSCustomPage.DestroyHandle(const AWinControl: TWinControl);
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
      Windows.SendMessage(PageControlHandle, TCM_DELETEITEM,
        Windows.WPARAM(RealIndex), 0);
  end;
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class procedure TWinCEWSCustomPage.SetText(const AWinControl: TWinControl; const AText: string);
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
    Windows.SendMessageW(NotebookHandle, TCM_GETITEMW, RealIndex, LPARAM(@TCI));
    if PtrUInt(TCI.lParam)=PtrUInt(AWinControl) then
    begin
      //DebugLn(Format('Trace:TWinCEWSCustomPage.SetText --> %S', [AText]));
      TCI.mask := TCIF_TEXT;
      {$ifdef Win32}
      TCI.pszText := PChar(PWideChar(UTF8Decode(AText)));
      {$else}
      TCI.pszText := PWideChar(UTF8Decode(AText));
      {$endif}
      Windows.SendMessageW(NotebookHandle, TCM_SETITEMW, RealIndex, LPARAM(@TCI));
      FreeMem(TCI.pszText);
    end;
  end;
end;

class procedure TWinCEWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
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

{ TWinCEWSCustomNotebook }

class function TWinCEWSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
// The prefered style for the non-supported ones is bottom, as per MS guidelines,
// so that the user won't cover the screen with the hand while changing tabs
// Left and Right styles aren't supported because vertical text isn't supported
// See: http://wiki.lazarus.freepascal.org/Windows_CE_Development_Notes#Tab_Controls_.28TPageControl.29
const
  TabPositionFlags: array[TTabPosition] of DWord = (
 { tpTop    } 0,
 { tpBottom } TCS_BOTTOM,
 { tpLeft   } TCS_BOTTOM, //TCS_VERTICAL or TCS_MULTILINE,
 { tpRight  } TCS_BOTTOM //TCS_VERTICAL or TCS_RIGHT or TCS_MULTILINE
  );
var
  Params: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
begin
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_TAB_CLASSES;
  InitCommonControlsEx(@init);
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
    SendMessage(Result, TCM_SETIMAGELIST, 0, TCustomImageListResolution(TCustomTabControl(AWinControl).Images).Reference._Handle);

  // although we may be child of tabpage, cut the paint chain
  // to improve speed and possible paint anomalities
  Params.WindowInfo^.needParentPaint := False;

  // The Windows CE tab controls are backwards compatible with older versions
  // so we need to specify if we desire the more modern flat style manually
  //SendMessage(Params.Window, CCM_SETVERSION, COMCTL32_VERSION, 0);
end;

class procedure TWinCEWSCustomNotebook.AddPage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const AIndex: integer);
var
  TCI: TC_ITEM;
  WideStr: widestring;
begin
  with ATabControl do
  begin
    AChild.HandleNeeded;
    if ShowTabs then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
      // store object as extra, so we can verify we got the right page later
      TCI.lParam := PtrUInt(AChild);
      TCI.iImage := ATabControl.GetImageIndex(NotebookPageRealToLCLIndex(ATabControl, AIndex));
      WideStr := UTF8Decode(AChild.Caption);
      {$ifdef Win32}
      TCI.pszText := PChar(PWideChar(WideStr));
      {$else}
      TCI.pszText := PWideChar(WideStr);
      {$endif}
      Windows.SendMessageW(Handle, TCM_INSERTITEMW, AIndex, LPARAM(@TCI));
    end;
    // clientrect possible changed, adding first tab, or deleting last
    // windows should send a WM_SIZE message because of this, but it doesn't
    // send it ourselves
    LCLControlSizeNeedsUpdate(ATabControl, True);
  end;
end;

class procedure TWinCEWSCustomNotebook.MovePage(const ATabControl: TCustomTabControl;
  const AChild: TCustomPage; const NewIndex: integer);
begin
  RemovePage(ATabControl, AChild.PageIndex);
  AddPage(ATabControl, AChild, NewIndex);
end;

class procedure TWinCEWSCustomNotebook.RemovePage(const ATabControl: TCustomTabControl;
  const AIndex: integer);
begin
  Windows.SendMessage(ATabControl.Handle, TCM_DELETEITEM, Windows.WPARAM(AIndex), 0);
end;

{ -----------------------------------------------------------------------------
  Method: AddAllNBPages
  Params: Notebook - A notebook control
  Returns: Nothing

  Adds all pages to notebook (showtabs becomes true)
 ------------------------------------------------------------------------------}
class procedure TWinCEWSCustomNotebook.AddAllNBPages(const ATabControl: TCustomTabControl);
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
      {$ifdef Win32}
      TCI.pszText := PChar(PWideChar(UTF8Decode(APage.Caption)));
      {$else}
      TCI.pszText := PWideChar(UTF8Decode(APage.Caption));
      {$endif}
      Windows.SendMessageW(WinHandle, TCM_INSERTITEMW, RealIndex, LPARAM(@TCI));
    end;
    Inc(RealIndex);
  end;
  AdjustSizeNotebookPages(ATabControl);
end;

class procedure TWinCEWSCustomNotebook.AdjustSizeNotebookPages(const ATabControl: TCustomTabControl);
var
  I: Integer;
  R: TRect;
  WinHandle: HWND;
  lPage: TCustomPage;
begin
  WinHandle := ATabControl.Handle;

  // Adjust page size to fit in tabcontrol, need bounds of notebook in client of parent
  LCLIntf.GetClientRect(WinHandle, R);

  for I := 0 to ATabControl.PageCount - 1 do
  begin
    lPage := ATabControl.Page[I];
    // we don't need to resize non-existing pages yet, they will be sized when created
    if lPage.HandleAllocated then
      // The Windows CE notebook as some alignment problems which we need to workaround
      // by adding an extra change to the position it gives us for the sheet position
      SetBounds(lPage, R.Left - 3, R.Top, R.Right - R.Left + 3, R.Bottom - R.Top);
  end;
end;

{------------------------------------------------------------------------------
  Method: RemoveAllNBPages
  Params: Notebook - The notebook control
  Returns: Nothing

  Removes all pages from a notebook control (showtabs becomes false)
 ------------------------------------------------------------------------------}
class procedure TWinCEWSCustomNotebook.RemoveAllNBPages(const ATabControl: TCustomTabControl);
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
  DeliverMessage(ATabControl, Mess);
end;

class function TWinCEWSCustomNotebook.GetTabIndexAtPos(const ATabControl: TCustomTabControl;
  const AClientPos: TPoint): integer;
var
  hittestInfo: TC_HITTESTINFO;
begin
  hittestInfo.pt.X := AClientPos.X;
  hittestInfo.pt.Y := AClientPos.Y;
  Result := Windows.SendMessage(ATabControl.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
end;

class function TWinCEWSCustomNotebook.GetTabRect(
  const ATabControl: TCustomTabControl; const AIndex: Integer): TRect;
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

class function TWinCEWSCustomNotebook.GetCapabilities: TCTabControlCapabilities;
begin
  Result:=[];
end;

class function TWinCEWSCustomNotebook.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
begin
  Result:=inherited GetDesignInteractive(AWinControl, AClientPos);
end;

class procedure TWinCEWSCustomNotebook.SetImageList(
  const ATabControl: TCustomTabControl; const AImageList: TCustomImageList);
begin
  if not WSCheckHandleAllocated(ATabControl, 'SetImageList') then
    Exit;

  if AImageList <> nil then
    SendMessage(ATabControl.Handle, TCM_SETIMAGELIST, 0, AImageList.Reference[ATabControl.ImagesWidth]._Handle)
  else
    SendMessage(ATabControl.Handle, TCM_SETIMAGELIST, 0, 0);
end;

class procedure TWinCEWSCustomNotebook.SetPageIndex(const ATabControl: TCustomTabControl; const AIndex: integer);
var
  Handle: HWND;
  PageHandle: HWND;
  OldIndex, OldRealIndex, NewRealIndex: Integer;
  WindowInfo: PWindowInfo;
begin
  Handle := ATabControl.Handle;
  OldRealIndex := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
  OldIndex := NotebookPageRealToLCLIndex(ATabControl, OldRealIndex);
  NewRealIndex := ATabControl.PageToTabIndex(AIndex);
  SendMessage(Handle, TCM_SETCURSEL, Windows.WParam(NewRealIndex), 0);
  if not (csDestroying in ATabControl.ComponentState) then
  begin
    // create handle if not already done, need to show!
    if (AIndex >= 0) and (AIndex < ATabControl.PageCount) then
    begin
//      PageHandle := ATabControl.CustomPage(AIndex).Handle;
      WindowInfo := GetWindowInfo(ATabControl.CustomPage(AIndex).Handle);
      PageHandle := WindowInfo^.ParentPanel;

      SetWindowPos(PageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW);
      SendSelChangeMessage(ATabControl, Handle, AIndex);
      NotebookFocusNewControl(ATabControl, AIndex);
    end;
    if (OldIndex >= 0) and (OldIndex <> AIndex) and
       (OldIndex < ATabControl.PageCount) and
       (ATabControl.CustomPage(OldIndex).HandleAllocated) then
      ShowWindow(ATabControl.CustomPage(OldIndex).Handle, SW_HIDE);
  end;
end;

{ Nothing can be done here because WinCE only supports tabs on the bottom }
class procedure TWinCEWSCustomNotebook.SetTabPosition(const ATabControl: TCustomTabControl; const ATabPosition: TTabPosition);
begin

end;

class procedure TWinCEWSCustomNotebook.ShowTabs(const ATabControl: TCustomTabControl; AShowTabs: boolean);
begin
  if AShowTabs then
    AddAllNBPages(ATabControl)
  else
    RemoveAllNBPages(ATabControl);
end;

class procedure TWinCEWSCustomNotebook.UpdateProperties(
  const ATabControl: TCustomTabControl);
begin
  inherited UpdateProperties(ATabControl);
end;

