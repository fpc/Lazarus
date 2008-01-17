{
 *****************************************************************************
 *                            WinCEWSExtCtrls.pp                             *
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
unit WinCEWSExtCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Windows, SysUtils, ExtCtrls, Classes, Controls, LCLType, LCLIntf,
////////////////////////////////////////////////////
  WSControls, WSExtCtrls, WSLCLClasses, WinCEInt, WinCEProc, InterfaceBase,
  WinCEWSControls;

type

  { TWinCEWSCustomPage }

  TWinCEWSCustomPage = class(TWSCustomPage)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure UpdateProperties(const ACustomPage: TCustomPage); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
  end;

  { TWinCEWSCustomNotebook }

  TWinCEWSCustomNotebook = class(TWSCustomNotebook)
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
    class procedure SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer); override;
    class procedure SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition); override;
    class procedure ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean); override;
  end;

  { TWinCEWSPage }

  TWinCEWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TWinCEWSNotebook }

  TWinCEWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TWinCEWSShape }

  TWinCEWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TWinCEWSCustomSplitter }

  TWinCEWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TWinCEWSSplitter }

  TWinCEWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TWinCEWSPaintBox }

  TWinCEWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TWinCEWSCustomImage }

  TWinCEWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TWinCEWSImage }

  TWinCEWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TWinCEWSBevel }

  TWinCEWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TWinCEWSCustomRadioGroup }

  TWinCEWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TWinCEWSRadioGroup }

  TWinCEWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TWinCEWSCustomCheckGroup }

  TWinCEWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TWinCEWSCheckGroup }

  TWinCEWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TWinCEWSCustomLabeledEdit }

  TWinCEWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TWinCEWSLabeledEdit }

  TWinCEWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TWinCEWSCustomPanel }

  TWinCEWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWinCEWSPanel }

  TWinCEWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

procedure NotebookFocusNewControl(const ANotebook: TCustomNotebook; NewIndex: integer);

implementation

uses
  LMessages;

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
    if I = ControlList.Count then
      Windows.SetFocus(Page.Handle);
  finally
    ControlList.Free;
  end;
end;


{ TWinCEWSCustomPage }

class function TWinCEWSCustomPage.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
begin
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_TAB_CLASSES;
  InitCommonControlsEx(@init);
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName;
    Flags := Flags and not WS_VISIBLE;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // return window handle
  Result := Params.Window;
end;

class procedure TWinCEWSCustomPage.DestroyHandle(const AWinControl: TWinControl);
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

class procedure TWinCEWSCustomPage.SetText(const AWinControl: TWinControl; const AText: string);
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
      Assert(False, Format('Trace:TWinCEWSCustomPage.SetText --> %S', [AText]));
      TCI.mask := TCIF_TEXT;
      TCI.pszText := PWideChar(Utf8Decode(AText));
      Windows.SendMessage(NotebookHandle, TCM_SETITEMW, PageIndex, LPARAM(@TCI));
    end;
  end;
end;

class procedure TWinCEWSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
  // TODO: implement me!
end;

{ TWinCEWSCustomNotebook }

class function TWinCEWSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  init : TINITCOMMONCONTROLSEX;
begin
  init.dwSize := Sizeof(TINITCOMMONCONTROLSEX);
  init.dwICC := ICC_TAB_CLASSES;
  InitCommonControlsEx(@init);
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    // Hard-coded bottom style as per MS guidelines
    // so that the user won't cover the screen with the hand while changing tabs
    // Without doing this the text on the Tab becomes invisible
    Flags := (Flags or TCS_BOTTOM) and not (TCS_VERTICAL or TCS_MULTILINE);
    pClassName := WC_TABCONTROL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  
  // although we may be child of tabpage, cut the paint chain
  // to improve speed and possible paint anomalities
  Params.WindowInfo^.needParentPaint := false;

  // The Windows CE tab controls are backwards compatible with older versions
  // so we need to specify if we desire the more modern flat style manually
  //SendMessage(Params.Window, CCM_SETVERSION, COMCTL32_VERSION, 0);
end;

class procedure TWinCEWSCustomNotebook.AddPage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const AIndex: integer);
var
  TCI: TC_ITEM;
begin
  with ANotebook do
  begin
    TCI.Mask := TCIF_TEXT or TCIF_PARAM;
    TCI.pszText := PWideChar(Utf8Decode(AChild.Caption));
    // store object as extra, so we can verify we got the right page later
    TCI.lParam := PtrUInt(AChild);
    Windows.SendMessage(Handle, TCM_INSERTITEM, AIndex, LPARAM(@TCI));
    // clientrect possible changed, adding first tab, or deleting last
    // windows should send a WM_SIZE message because of this, but it doesn't
    // send it ourselves
    LCLControlSizeNeedsUpdate(ANotebook, true);
  end;
end;

class procedure TWinCEWSCustomNotebook.MovePage(const ANotebook: TCustomNotebook;
  const AChild: TCustomPage; const NewIndex: integer);
begin
  RemovePage(ANotebook, AChild.PageIndex);
  AddPage(ANotebook, AChild, NewIndex);
end;

class procedure TWinCEWSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook;
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
class procedure TWinCEWSCustomNotebook.AddAllNBPages(const ANotebook: TCustomNotebook);
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
      TCI.Mask := TCIF_TEXT or TCIF_PARAM;
      TCI.lParam := PtrUInt(lPage);
      TCI.pszText := PWideChar(Utf8Decode(lPage.Caption));
      Windows.SendMessage(WinHandle, TCM_INSERTITEM, RealIndex, LPARAM(@TCI));
    end;
    Inc(RealIndex);
  end;
  AdjustSizeNotebookPages(ANotebook);
end;

class procedure TWinCEWSCustomNotebook.AdjustSizeNotebookPages(const ANotebook: TCustomNotebook);
var
  I: Integer;
  R: TRect;
  WinHandle: HWND;
  lPage: TCustomPage;
begin
  WinHandle := ANotebook.Handle;

  // Adjust page size to fit in tabcontrol, need bounds of notebook in client of parent
  LCLIntf.GetClientRect(WinHandle, R);
  
  for I := 0 to ANotebook.PageCount - 1 do
  begin
    lPage := ANotebook.Page[I];
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
class procedure TWinCEWSCustomNotebook.RemoveAllNBPages(const ANotebook: TCustomNotebook);
var
  I: Integer;
  WinHandle: HWND;
begin
  WinHandle := ANotebook.Handle;
  for I := ANotebook.PageCount - 1 downto 0 do
    Windows.SendMessage(WinHandle, TCM_DELETEITEM, Windows.WPARAM(I), 0);
  AdjustSizeNotebookPages(ANotebook);
end;

class function TWinCEWSCustomNotebook.GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer;
var
  X: Integer;
begin
  Result := AIndex;
  for X := 0 to AIndex-1 do begin
    if ANotebook.Page[X].TabVisible = False then Dec(Result);
  end;
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
  DeliverMessage(ANotebook, Mess);
end;

class function TWinCEWSCustomNotebook.GetTabIndexAtPos(const ANotebook: TCustomNotebook;
  const AClientPos: TPoint): integer;
var
  hittestInfo: TC_HITTESTINFO;
begin
  hittestInfo.pt.X := AClientPos.X;
  hittestInfo.pt.Y := AClientPos.Y;
  Result := Windows.SendMessage(ANotebook.Handle, TCM_HITTEST, 0, LPARAM(@hittestInfo));
end;

class procedure TWinCEWSCustomNotebook.SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer);
var
  Handle: HWND;
  PageHandle: HWND;
  OldRealIndex, NewRealIndex: Integer;
begin
  Handle := ANotebook.Handle;
  OldRealIndex := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
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
    if (OldRealIndex >= 0) and (OldRealIndex <> NewRealIndex)
        and (OldRealIndex < ANotebook.PageCount)
        and (ANotebook.CustomPage(OldRealIndex).HandleAllocated) then 
      ShowWindow(ANotebook.CustomPage(OldRealIndex).Handle, SW_HIDE);
  end;
end;

{ Nothing can be done here because WinCE only supports tabs on the bottom }
class procedure TWinCEWSCustomNotebook.SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
begin

end;

class procedure TWinCEWSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean);
begin
  if AShowTabs then AddAllNBPages(ANotebook)
  else RemoveAllNBPages(ANotebook);
end;

{ TWinCEWSCustomPanel }

class function TWinCEWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;


initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TCustomPage, TWinCEWSCustomPage);
  RegisterWSComponent(TCustomNotebook, TWinCEWSCustomNotebook);
//  RegisterWSComponent(TPage, TWinCEWSPage);
//  RegisterWSComponent(TNotebook, TWinCEWSNotebook);
//  RegisterWSComponent(TShape, TWinCEWSShape);
//  RegisterWSComponent(TCustomSplitter, TWinCEWSCustomSplitter);
//  RegisterWSComponent(TSplitter, TWinCEWSSplitter);
//  RegisterWSComponent(TPaintBox, TWinCEWSPaintBox);
//  RegisterWSComponent(TCustomImage, TWinCEWSCustomImage);
//  RegisterWSComponent(TImage, TWinCEWSImage);
//  RegisterWSComponent(TBevel, TWinCEWSBevel);
//  RegisterWSComponent(TCustomRadioGroup, TWinCEWSCustomRadioGroup);
//  RegisterWSComponent(TRadioGroup, TWinCEWSRadioGroup);
//  RegisterWSComponent(TCustomCheckGroup, TWinCEWSCustomCheckGroup);
//  RegisterWSComponent(TCheckGroup, TWinCEWSCheckGroup);
//  RegisterWSComponent(TCustomLabeledEdit, TWinCEWSCustomLabeledEdit);
//  RegisterWSComponent(TLabeledEdit, TWinCEWSLabeledEdit);
  RegisterWSComponent(TCustomPanel, TWinCEWSCustomPanel);
//  RegisterWSComponent(TPanel, TWinCEWSPanel);
//  RegisterWSComponent(TCustomTrayIcon, TWinCEWSCustomTrayIcon);
////////////////////////////////////////////////////
end.
