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
 *  See the file COPYING.LCL, included in this distribution,                 *
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

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Windows, ExtCtrls, Classes, Controls, LCLType, SysUtils,
////////////////////////////////////////////////////
  WSExtCtrls, WSLCLClasses, WinExt, Win32Int, Win32Proc, InterfaceBase, 
  Win32WSControls;

type

  { TWin32WSCustomPage }

  TWin32WSCustomPage = class(TWSCustomPage)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
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
    class procedure AddPage(const ANotebook: TCustomNotebook; 
      const AChild: TCustomPage; const AIndex: integer); override;
    class procedure MovePage(const ANotebook: TCustomNotebook; 
      const AChild: TCustomPage; const NewIndex: integer); override;
    class procedure RemovePage(const ANotebook: TCustomNotebook; 
      const AIndex: integer); override;

    class function GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer; override;
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

procedure NotebookTabChanged(Notebook: TCustomNotebook; NewIndex: integer);

implementation

{ -----------------------------------------------------------------------------
  Method: AddAllNBPages
  Params: Notebook - A notebook control
  Returns: Nothing

  Adds all pages to notebook (showtabs becomes true)
 ------------------------------------------------------------------------------}
procedure AddAllNBPages(Notebook: TCustomNotebook);
var
  TCI: TC_ITEM;
  I, Res: Integer;
  lPage: TCustomPage;
begin
  for I := 0 to Notebook.PageCount - 1 do
  begin
    lPage := Notebook.Page[I];
    // check if already shown
    TCI.Mask := TCIF_PARAM;
    Res := Windows.SendMessage(Notebook.Handle, TCM_GETITEM, I, LPARAM(@TCI));
    if (Res = 0) or (dword(TCI.lParam) <> dword(lPage)) then
    begin
      TCI.Mask := TCIF_TEXT or TCIF_PARAM;
      TCI.pszText := PChar(lPage.Caption);
      TCI.lParam := dword(lPage);
      Windows.SendMessage(Notebook.Handle, TCM_INSERTITEM, I, LPARAM(@TCI));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: RemoveAllNBPages
  Params: Notebook - The notebook control
  Returns: Nothing

  Removes all pages from a notebook control (showtabs becomes false)
 ------------------------------------------------------------------------------}
procedure RemoveAllNBPages(Notebook: TCustomNotebook);
var
  I: Integer;
  R: TRect;
begin
  for I := Notebook.PageCount - 1 downto 0 do
    Windows.SendMessage(Notebook.Handle, TCM_DELETEITEM, Windows.WPARAM(I), 0);
  // Adjust page size to fit in tabcontrol, need bounds of notebook in client of parent
  TWin32WidgetSet(InterfaceObject).GetClientRect(Notebook.Handle, R);
  R.Right := R.Right - R.Left;
  R.Bottom := R.Bottom - R.Top;
  for I := 0 to Notebook.PageCount - 1 do
    TWin32WidgetSet(InterfaceObject).ResizeChild(Notebook.Page[I], R.Left, R.Top, R.Right, R.Bottom);
end;

{ sets focus to a control on the newly focused tab page }
procedure NotebookTabChanged(Notebook: TCustomNotebook; NewIndex: integer);
var
  Page: TCustomPage;
  ControlList: TList;
  lWinControl: TWinControl;
  I: integer;
begin
  Page := Notebook.CustomPage(NewIndex);
  ControlList := TList.Create;
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


{ TWin32WSCustomPage }

function TWin32WSCustomPage.CreateHandle(const AWinControl: TWinControl;
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
    Flags := Flags and not WS_VISIBLE;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // return window handle
  Result := Params.Window;
  if TWin32WidgetSet(InterfaceObject).ThemesActive then
  with Params.WindowInfo^ do
  begin
    hasTabParent := true;
    isTabPage := true;
  end;
end;

procedure TWin32WSCustomPage.SetText(const AWinControl: TWinControl; const AText: string);
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
    if dword(TCI.lParam)=dword(AWinControl) then
    begin
      Assert(False, Format('Trace:TWin32WSCustomPage.SetText --> %S', [AText]));
      TCI.mask := TCIF_TEXT;
      TCI.pszText := PChar(AText);
      Windows.SendMessage(NotebookHandle, TCM_SETITEM, PageIndex, LPARAM(@TCI));
    end;
  end;
end;

procedure TWin32WSCustomPage.UpdateProperties(const ACustomPage: TCustomPage);
begin
  // TODO: implement me!
end;

{ TWin32WSCustomNotebook }

function TWin32WSCustomNotebook.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    pClassName := WC_TABCONTROL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  // although we may be child of tabpage, cut the paint chain
  // to improve speed and possible paint anomalities
  Params.WindowInfo^.hasTabParent := false;
end;

procedure TWin32WSCustomNotebook.AddPage(const ANotebook: TCustomNotebook; 
  const AChild: TCustomPage; const AIndex: integer);
var
  TCI: TC_ITEM;
begin
  with ANotebook do
  begin
    TCI.Mask := TCIF_TEXT or TCIF_PARAM;
    TCI.pszText := PChar(AChild.Caption);
    // store object as extra, so we can verify we got the right page later
    TCI.lParam := dword(AChild);
    Windows.SendMessage(Handle, TCM_INSERTITEM, AIndex, LPARAM(@TCI));
    // clientrect possible changed, adding first tab, or deleting last
    // windows should send a WM_SIZE message because of this, but it doesn't
    // send it ourselves
    LCLControlSizeNeedsUpdate(ANotebook, true);
  end;
end;

procedure TWin32WSCustomNotebook.MovePage(const ANotebook: TCustomNotebook; 
  const AChild: TCustomPage; const NewIndex: integer);
begin
  RemovePage(ANotebook, AChild.PageIndex);
  AddPage(ANotebook,AChild,NewIndex);
end;

procedure TWin32WSCustomNotebook.RemovePage(const ANotebook: TCustomNotebook; 
  const AIndex: integer);
begin
  Windows.SendMessage(ANotebook.Handle, TCM_DELETEITEM, Windows.WPARAM(AIndex), 0);
end;

function TWin32WSCustomNotebook.GetPageRealIndex(const ANotebook: TCustomNotebook; AIndex: Integer): Integer;
var
X: Integer;
begin
  Result := AIndex;
  for X := 0 to AIndex-1 do begin
    if ANotebook.Page[X].TabVisible = False then Dec(Result);
  end;
end;

procedure TWin32WSCustomNotebook.SetPageIndex(const ANotebook: TCustomNotebook; const AIndex: integer);
var
  OldPageIndex: Integer;
  PageHandle: HWND;
  Handle: HWND;
  RealIndex: Integer;
begin
  Handle := ANotebook.Handle;
  OldPageIndex := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
  RealIndex := GetPageRealIndex(ANotebook,AIndex);
  SendMessage(Handle, TCM_SETCURSEL, Windows.WParam(RealIndex), 0);
  if not (csDestroying in ANotebook.ComponentState) then
  begin
    // create handle if not already done, need to show!
    if (AIndex >= 0) and (AIndex < ANotebook.PageCount) then
    begin
      PageHandle := ANotebook.CustomPage(AIndex).Handle;
      SetWindowPos(PageHandle, HWND_TOP, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_SHOWWINDOW);
      NotebookTabChanged(ANotebook, AIndex);
    end;
    if (OldPageIndex >= 0) and (OldPageIndex<>AIndex)
    and (OldPageIndex < ANotebook.PageCount)
    and (ANotebook.CustomPage(OldPageIndex).HandleAllocated)
      then ShowWindow(ANotebook.CustomPage(OldPageIndex).Handle, SW_HIDE);
  end;
end;

procedure TWin32WSCustomNotebook.SetTabPosition(const ANotebook: TCustomNotebook; const ATabPosition: TTabPosition);
var
  NotebookHandle: HWND;
  WindowStyle: dword;
begin
  // VS: not tested
  NotebookHandle := ANotebook.Handle;
  WindowStyle := Windows.GetWindowLong(NotebookHandle, GWL_STYLE);
  case ATabPosition of
    tpTop:
      WindowStyle := WindowStyle and not(TCS_VERTICAL or TCS_MULTILINE or TCS_BOTTOM);
    tpBottom:
      WindowStyle := (WindowStyle or TCS_BOTTOM) and not (TCS_VERTICAL or TCS_MULTILINE);
    tpLeft:
      WindowStyle := (WindowStyle or TCS_VERTICAL or TCS_MULTILINE) and not TCS_RIGHT;
    tpRight:
      WindowStyle := WindowStyle or (TCS_VERTICAL or TCS_RIGHT or TCS_MULTILINE);
  end;
  Windows.SetWindowLong(NotebookHandle, GWL_STYLE, WindowStyle);
end;

procedure TWin32WSCustomNotebook.ShowTabs(const ANotebook: TCustomNotebook; AShowTabs: boolean);
begin
  if AShowTabs then
  begin
    AddAllNBPages(ANotebook);
  end else begin
    RemoveAllNBPages(ANotebook);
  end;
end;

{ TWin32WSCustomPanel }

function TWin32WSCustomPanel.CreateHandle(const AWinControl: TWinControl;
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
////////////////////////////////////////////////////
end.
