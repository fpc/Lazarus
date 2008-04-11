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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  ExtCtrls, Controls, ImgList, LCLType, LCLIntf, Themes,
// ws
  WSControls, WSExtCtrls, WSLCLClasses, WSProc, Win32Extra, Win32Int, Win32Proc,
  InterfaceBase, Win32WSControls;

type

  { TWin32WSCustomPage }

  TWin32WSCustomPage = class(TWSCustomPage)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWin32WSCustomNotebook }

  TWin32WSCustomNotebook = class(TWSCustomNotebook)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
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
    class function GetDesignInteractive(const AWinControl: TWinControl; AClientPos: TPoint): Boolean; override;
    class procedure SetImageList(const ANotebook: TCustomNotebook; const AImageList: TCustomImageList); override;
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
  end;

  { TWin32WSPage }

  TWin32WSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TWin32WSNotebook }

  TWin32WSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TWin32WSShape }

  TWin32WSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TWin32WSCustomSplitter }

  TWin32WSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TWin32WSSplitter }

  TWin32WSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TWin32WSPaintBox }

  TWin32WSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TWin32WSCustomImage }

  TWin32WSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TWin32WSImage }

  TWin32WSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TWin32WSBevel }

  TWin32WSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TWin32WSCustomRadioGroup }

  TWin32WSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TWin32WSRadioGroup }

  TWin32WSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TWin32WSCustomCheckGroup }

  TWin32WSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TWin32WSCheckGroup }

  TWin32WSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TWin32WSCustomLabeledEdit }

  TWin32WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TWin32WSLabeledEdit }

  TWin32WSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TWin32WSCustomPanel }

  TWin32WSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWin32WSPanel }

  TWin32WSPanel = class(TWSPanel)
  private
  protected
  public
  end;

  { TWin32WSCustomTrayIcon }

  TWin32WSCustomTrayIcon = class(TWSCustomTrayIcon)
  public
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

procedure NotebookFocusNewControl(const ANotebook: TCustomNotebook; NewIndex: integer);
function  NotebookPageRealToLCLIndex(const ANotebook: TCustomNotebook; AIndex: integer): integer;

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
  ControlList: TFPList;
  lWinControl: TWinControl;
  I: integer;
begin
  { see if currently focused control is within notebook }
  if not IsNotebookGroupFocused(ANotebook) then exit;

  { focus was/is within notebook, pick a new control to focus }
  Page := ANotebook.CustomPage(NewIndex);
  ControlList := TFPList.Create;
  try
    Page.GetTabOrderList(ControlList);
    I := 0;
    while I < ControlList.Count do
    begin
      lWinControl := TWinControl(ControlList[I]);
      if lWinControl.TabStop and lWinControl.Enabled and lWinControl.CanFocus then
      begin
        lWinControl.SetFocus;
        break;
      end;
      Inc(I);
    end;
  finally
    ControlList.Free;
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

{ TWin32WSCustomPage }

class function TWin32WSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName[0];
    Flags := Flags and not WS_VISIBLE;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // return window handle
  Result := Params.Window;
  if ThemeServices.ThemesEnabled then
    with Params.WindowInfo^ do
    begin
      needParentPaint := true;
      isTabPage := true;
    end;
end;

class procedure TWin32WSCustomPage.DestroyHandle(const AWinControl: TWinControl);
var
  PageIndex: integer;
  PageControlHandle: HWND;
begin
  // remove tab from pagecontrol only if not pfRemoving is set
  // if pfRemoving is set then Tab has been deleted by RemovePage
  if (AWinControl.Parent <> nil) and (AWinControl.Parent.HandleAllocated) and
     not (pfRemoving in TCustomPageAccess(AWinControl).Flags) then
  begin
    PageControlHandle := AWinControl.Parent.Handle;
    PageIndex := TCustomPage(AWinControl).PageIndex;
    if PageIndex <> -1 then
      Windows.SendMessage(PageControlHandle, TCM_DELETEITEM,
        Windows.WPARAM(PageIndex), 0);
  end;
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class procedure TWin32WSCustomPage.SetText(const AWinControl: TWinControl; const AText: string);
var
  TCI: TC_ITEM;
  PageIndex: integer;
  NotebookHandle: HWND;
begin
  PageIndex := TCustomPage(AWinControl).PageIndex;
  NotebookHandle := AWinControl.Parent.Handle;
  // We can't set label of a page not yet added,
  // Check for valid page index
  if (PageIndex>=0) and
    (PageIndex < Windows.SendMessage(NotebookHandle, TCM_GETITEMCOUNT,0,0)) then
  begin
    // retrieve page handle from tab as extra check (in case page isn't added yet).
    TCI.mask := TCIF_PARAM;
    Windows.SendMessage(NotebookHandle, TCM_GETITEM, PageIndex, LPARAM(@TCI));
    if PtrUInt(TCI.lParam)=PtrUInt(AWinControl) then
    begin
      Assert(False, Format('Trace:TWin32WSCustomPage.SetText --> %S', [AText]));
      TCI.mask := TCIF_TEXT;
{$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        TCI.pszText := PChar(PWideChar(Utf8Decode(AText)));
        Windows.SendMessage(NotebookHandle, TCM_SETITEMW, PageIndex, LPARAM(@TCI));
      end
      else
      begin
        TCI.pszText := PChar(UTF8ToAnsi(AText));
        Windows.SendMessage(NotebookHandle, TCM_SETITEM, PageIndex, LPARAM(@TCI));
      end;
{$else}
      TCI.pszText := PChar(AText);
      Windows.SendMessage(NotebookHandle, TCM_SETITEM, PageIndex, LPARAM(@TCI));
{$endif}
    end;
  end;
end;

class procedure TWin32WSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
var
  TCI: TC_ITEM;
  PageIndex: integer;
  NotebookHandle: HWND;
begin
  PageIndex := ACustomPage.PageIndex;
  NotebookHandle := ACustomPage.Parent.Handle;
  // Check for valid page index
  if (PageIndex>=0) and
    (PageIndex < Windows.SendMessage(NotebookHandle, TCM_GETITEMCOUNT,0,0)) then
  begin
    // retrieve page handle from tab as extra check (in case page isn't added yet).
    TCI.mask := TCIF_PARAM;
    Windows.SendMessage(NotebookHandle, TCM_GETITEM, PageIndex, LPARAM(@TCI));
    if PtrUInt(TCI.lParam)=PtrUInt(ACustomPage) then
    begin
      TCI.mask := TCIF_IMAGE;
      TCI.iImage := TCustomNotebook(ACustomPage.Parent).GetImageIndex(PageIndex);

      Windows.SendMessage(NotebookHandle, TCM_SETITEM, PageIndex, LPARAM(@TCI));
    end;
  end;
end;

{ TWin32WSCustomNotebook }

class function TWin32WSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    case TCustomNoteBook(AWinControl).TabPosition of
      tpTop:
        Flags := Flags and not(TCS_VERTICAL or TCS_MULTILINE or TCS_BOTTOM);
      tpBottom:
        Flags := (Flags or TCS_BOTTOM) and not (TCS_VERTICAL or TCS_MULTILINE);
      tpLeft:
        Flags := (Flags or TCS_VERTICAL or TCS_MULTILINE) and not TCS_RIGHT;
      tpRight:
        Flags := Flags or (TCS_VERTICAL or TCS_RIGHT or TCS_MULTILINE);
    end;
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
    TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
    // store object as extra, so we can verify we got the right page later
    TCI.lParam := PtrUInt(AChild);
    TCI.iImage := ANotebook.GetImageIndex(AIndex);
{$ifdef WindowsUnicodeSupport}
    if UnicodeEnabledOS then
    begin
      TCI.pszText := PChar(PWideChar(Utf8Decode(AChild.Caption)));
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
    // clientrect possible changed, adding first tab, or deleting last
    // windows should send a WM_SIZE message because of this, but it doesn't
    // send it ourselves
    LCLControlSizeNeedsUpdate(ANotebook, true);
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
  lPage: TCustomPage;
  WinHandle: HWND;
begin
  WinHandle := ANotebook.Handle;
  RealIndex := 0;
  for I := 0 to ANotebook.PageCount - 1 do
  begin
    lPage := ANotebook.Page[I];
    if not lPage.TabVisible and not (csDesigning in lPage.ComponentState) then 
      continue;
    // check if already shown
    TCI.Mask := TCIF_PARAM;
    Res := Windows.SendMessage(ANotebook.Handle, TCM_GETITEM, RealIndex, LPARAM(@TCI));
    if (Res = 0) or (PtrUInt(TCI.lParam) <> PtrUInt(lPage)) then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM or TCIF_IMAGE;
      TCI.lParam := PtrUInt(lPage);
      TCI.iImage := ANotebook.GetImageIndex(RealIndex);
{$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
      begin
        TCI.pszText := PChar(PWideChar(Utf8Decode(lPage.Caption)));
        Windows.SendMessage(WinHandle, TCM_INSERTITEMW, RealIndex, LPARAM(@TCI));
      end
      else
      begin
        TCI.pszText := PChar(Utf8ToAnsi(lPage.Caption));
        Windows.SendMessage(WinHandle, TCM_INSERTITEM, RealIndex, LPARAM(@TCI));
      end;
{$else}
      TCI.pszText := PChar(lPage.Caption);
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
  R.Right := R.Right - R.Left;
  R.Bottom := R.Bottom - R.Top;
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
  for X := 0 to AIndex-1 do
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
begin
  hittestInfo.pt := AClientPos;
  Result := Windows.SendMessage(ANotebook.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
end;

class function TWin32WSCustomNotebook.GetDesignInteractive(
  const AWinControl: TWinControl; AClientPos: TPoint): Boolean;
var
  AIndex, ACurIndex: Integer;
begin
  AIndex := GetTabIndexAtPos(TCustomNoteBook(AWinControl), AClientPos);
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
end;

class procedure TWin32WSCustomNotebook.SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer);
var
  Handle: HWND;
  PageHandle: HWND;
  OldIndex, OldRealIndex, NewRealIndex: Integer;
begin
  Handle := ANotebook.Handle;
  OldRealIndex := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
  OldIndex := NotebookPageRealToLCLIndex(ANotebook, OldRealIndex);
  NewRealIndex := GetPageRealIndex(ANotebook, AIndex);
  SendMessage(Handle, TCM_SETCURSEL, Windows.WParam(NewRealIndex), 0);
  if not (csDestroying in ANotebook.ComponentState) then
  begin
    // create handle if not already done, need to show!
    if (AIndex >= 0) and (AIndex < ANotebook.PageCount) then
    begin
      PageHandle := ANotebook.CustomPage(AIndex).Handle;
      SetWindowPos(PageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW);
      SendSelChangeMessage(ANotebook, Handle, AIndex);
      NotebookFocusNewControl(ANotebook, AIndex);
    end;
    if (OldIndex >= 0) and (OldIndex <> AIndex)
        and (OldIndex < ANotebook.PageCount)
        and (ANotebook.CustomPage(OldIndex).HandleAllocated) then
      ShowWindow(ANotebook.CustomPage(OldIndex).Handle, SW_HIDE);
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
  begin
    AddAllNBPages(ANotebook);
  end else begin
    RemoveAllNBPages(ANotebook);
  end;
end;

{ TWin32WSCustomPanel }

class function TWin32WSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName[0];
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;


{$include win32trayicon.inc}

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomPage, TWin32WSCustomPage);
  RegisterWSComponent(TCustomNotebook, TWin32WSCustomNotebook);
//  RegisterWSComponent(TPage, TWin32WSPage);
//  RegisterWSComponent(TNotebook, TWin32WSNotebook);
//  RegisterWSComponent(TShape, TWin32WSShape);
//  RegisterWSComponent(TCustomSplitter, TWin32WSCustomSplitter);
//  RegisterWSComponent(TSplitter, TWin32WSSplitter);
//  RegisterWSComponent(TPaintBox, TWin32WSPaintBox);
//  RegisterWSComponent(TCustomImage, TWin32WSCustomImage);
//  RegisterWSComponent(TImage, TWin32WSImage);
//  RegisterWSComponent(TBevel, TWin32WSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TWin32WSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TWin32WSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TWin32WSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TWin32WSCheckGroup);
//  RegisterWSComponent(TCustomLabeledEdit, TWin32WSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TWin32WSLabeledEdit);
  RegisterWSComponent(TCustomPanel, TWin32WSCustomPanel);
//  RegisterWSComponent(TPanel, TWin32WSPanel);
  RegisterWSComponent(TCustomTrayIcon, TWin32WSCustomTrayIcon);
////////////////////////////////////////////////////
end.
