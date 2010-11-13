{ $Id$}
{
 *****************************************************************************
 *                              Win32WSForms.pp                              * 
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
unit Win32WSForms;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  Forms, Controls, LCLType, Classes,
////////////////////////////////////////////////////
  WSForms, WSProc, WSLCLClasses, Windows, SysUtils, Win32Extra,
  InterfaceBase, Win32Int, Win32Proc, Win32WSControls;

type

  { TWin32WSScrollingWinControl }

  TWin32WSScrollingWinControl = class(TWSScrollingWinControl)
  published
    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
      const DeltaX, DeltaY: integer); override;
  end;

  { TWin32WSScrollBox }

  TWin32WSScrollBox = class(TWSScrollBox)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWin32WSCustomFrame }

  TWin32WSCustomFrame = class(TWSCustomFrame)
  published
  end;

  { TWin32WSFrame }

  TWin32WSFrame = class(TWSFrame)
  published
  end;

  { TWin32WSCustomForm }

  TWin32WSCustomForm = class(TWSCustomForm)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetAlphaBlend(const ACustomForm: TCustomForm; const AlphaBlend: Boolean;
      const Alpha: Byte); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
          const ABorderIcons: TBorderIcons); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop,
          AWidth, AHeight: Integer); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetFormStyle(const AForm: TCustomform; const AFormStyle, AOldFormStyle: TFormStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const Small, Big: HICON); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
    class procedure SetPopupParent(const ACustomForm: TCustomForm;
       const APopupMode: TPopupMode; const APopupParent: TCustomForm); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TWin32WSForm }

  TWin32WSForm = class(TWSForm)
  published
  end;

  { TWin32WSHintWindow }

  TWin32WSHintWindow = class(TWSHintWindow)
  published
    class function CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TWin32WSScreen }

  TWin32WSScreen = class(TWSScreen)
  published
  end;

  { TWin32WSApplicationProperties }

  TWin32WSApplicationProperties = class(TWSApplicationProperties)
  published
  end;


implementation

type
  TWinControlAccess = class(TWinControl)
  end;

{ TWin32WSScrollBox }

class function TWin32WSScrollBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;

  {$IFDEF NewScrollingLayer}
  procedure CreateScrollingLayer(ParentH: HWND);
  var
    Params: TCreateWindowExParams;
  begin
    // general initialization of Params
    with Params do
    begin
      Flags := WS_CHILD or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
      FlagsEx := 0;
      Window := HWND(nil);
      Buddy := HWND(nil);
      Parent := ParentH;
      SubClassWndProc := @WindowProc;
      WindowTitle := nil;
      StrCaption := 'TWin32WSScrollBox.CreateHandle ScrollLayer';
      Height := 50;
      Left := 0;
      //Parent := AWinControl.Parent;
      Top := 0;
      Width := 50;
      Flags := Flags or WS_VISIBLE;
      FlagsEx := FlagsEx or WS_EX_CONTROLPARENT;
    end;
    // customization of Params
    with Params do
    begin
      pClassName := @ClsName[0];
      SubClassWndProc := nil;
    end;
    // create window
    with Params do
    begin
      MenuHandle := HMENU(nil);

      Window := CreateWindowEx(FlagsEx, pClassName, WindowTitle, Flags,
          Left, Top, Width, Height, Parent, MenuHandle, HInstance, Nil);

      if Window = 0 then
      begin
        raise exception.create('failed to create win32 sub control, error: '+IntToStr(GetLastError()));
      end;
    end;
    with Params do
    begin
      if Window <> HWND(Nil) then
      begin
        // some controls (combobox) immediately send a message upon setting font
        {WindowInfo := AllocWindowInfo(Window);
        if GetWindowInfo(Parent)^.needParentPaint then
          WindowInfo^.needParentPaint := true;
        WindowInfo^.WinControl := AWinControl;
        if SubClassWndProc <> nil then
          WindowInfo^.DefWndProc := Windows.WNDPROC(SetWindowLong(
            Window, GWL_WNDPROC, PtrInt(SubClassWndProc)));
        lhFont := FDefaultFont;
        Windows.SendMessage(Window, WM_SETFONT, WPARAM(lhFont), 0);}
      end;
    end;
    Result := Params.Window;
  end;
  {$ENDIF}
  
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    //TODO: Make control respond to user scroll request
    if TScrollBox(AWinControl).BorderStyle = bsSingle then
      FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    pClassName := @ClsName[0];
    Flags := Flags or WS_HSCROLL or WS_VSCROLL;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
  
  {$IFDEF NewScrollingLayer}
  CreateScrollingLayer(Result);
  {$ENDIF}
end;

{ TWin32WSScrollingWinControl }

function ScrollWindowPtr(hWnd:HWND; XAmount:longint; YAmount:longint; lpRect: pointer; lpClipRect: pointer):WINBOOL; stdcall; external 'user32' name 'ScrollWindow';

class procedure TWin32WSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl;
  const DeltaX, DeltaY: integer);
var
  lVisible: boolean;
begin
  lVisible := AWinControl.HandleAllocated and Windows.IsWindowVisible(AWinControl.Handle);
  if lVisible then
    ScrollWindowPtr(AWinControl.Handle, -DeltaX, -DeltaY, nil, nil);
end;

{ TWin32WSCustomForm }

function CalcBorderIconsFlags(const AForm: TCustomForm): DWORD;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if (biSystemMenu in BorderIcons) or (csDesigning in AForm.ComponentState) then
    Result := Result or WS_SYSMENU;
  if GetDesigningBorderStyle(AForm) in [bsNone, bsSingle, bsSizeable] then
  begin
    if biMinimize in BorderIcons then
      Result := Result or WS_MINIMIZEBOX;
    if biMaximize in BorderIcons then
      Result := Result or WS_MAXIMIZEBOX;
  end;
end;

function CalcBorderIconsFlagsEx(const AForm: TCustomForm): DWORD;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if GetDesigningBorderStyle(AForm) in [bsSingle, bsSizeable, bsDialog] then
  begin
    if biHelp in BorderIcons then
      Result := Result or WS_EX_CONTEXTHELP;
  end;
end;

procedure CalcFormWindowFlags(const AForm: TCustomForm; var Flags, FlagsEx: DWORD);
var
  BorderStyle: TFormBorderStyle;
begin
  BorderStyle := GetDesigningBorderStyle(AForm);
  Flags := BorderStyleToWin32Flags(BorderStyle);
  if AForm.Parent <> nil then
    Flags := (Flags or WS_CHILD) and not WS_POPUP;
  // clear border style flags
  FlagsEx := FlagsEx and not (WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE or WS_EX_TOOLWINDOW);
  // set border style flags
  FlagsEx := FlagsEx or BorderStyleToWin32FlagsEx(BorderStyle);
  if (AForm.FormStyle in fsAllStayOnTop) and 
      not (csDesigning in AForm.ComponentState) then
    FlagsEx := FlagsEx or WS_EX_TOPMOST;
  Flags := Flags or CalcBorderIconsFlags(AForm);
  FlagsEx := FlagsEx or CalcBorderIconsFlagsEx(AForm);
end;

procedure AdjustFormBounds(const AForm: TCustomForm; var SizeRect: TRect);
var
  BorderStyle: TFormBorderStyle;
begin
  // the LCL defines the size of a form without border, win32 with.
  // -> adjust size according to BorderStyle
  SizeRect := AForm.BoundsRect;
  BorderStyle := GetDesigningBorderStyle(AForm);
  Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWin32Flags(
      BorderStyle), false, BorderStyleToWin32FlagsEx(BorderStyle));
end;

function CustomFormWndProc(Window: HWnd; Msg: UInt; WParam: Windows.WParam; LParam: Windows.LParam): LResult; stdcall;

  procedure LCLFormSizeToWin32Size(Form: TCustomForm; var AWidth, AHeight: Integer);
  var
    SizeRect: Windows.RECT;
    BorderStyle: TFormBorderStyle;
  begin
    with SizeRect do
    begin
      Left := 0;
      Top := 0;
      Right := AWidth;
      Bottom := AHeight;
    end;
    BorderStyle := GetDesigningBorderStyle(Form);
    Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWin32Flags(
        BorderStyle), false, BorderStyleToWin32FlagsEx(BorderStyle));
    AWidth := SizeRect.Right - SizeRect.Left;
    AHeight := SizeRect.Bottom - SizeRect.Top;
  end;

  procedure SetMinMaxInfo(WinControl: TWinControl; var MinMaxInfo: TMINMAXINFO);
    procedure SetWin32SizePoint(AWidth, AHeight: integer; var pt: TPoint);
    var
      IntfWidth, IntfHeight: integer;
    begin
      // 0 means no constraint
      if (AWidth=0) and (AHeight=0) then exit;

      IntfWidth := AWidth;
      IntfHeight := AHeight;
      LCLFormSizeToWin32Size(TCustomForm(WinControl), IntfWidth, IntfHeight);

      if AWidth>0 then
        pt.X:= IntfWidth;
      if AHeight>0 then
        pt.Y := IntfHeight;
    end;
  begin
    with WinControl.Constraints do
    begin
      SetWin32SizePoint(MinWidth, MinHeight, MinMaxInfo.ptMinTrackSize);
      SetWin32SizePoint(MaxWidth, MaxHeight, MinMaxInfo.ptMaxTrackSize);
    end;
  end;

var
  Info: PWin32WindowInfo;
  WinControl: TWinControl;
begin
  Info := GetWin32WindowInfo(Window);
  WinControl := Info^.WinControl;
  case Msg of
    WM_GETMINMAXINFO:
      begin
        SetMinMaxInfo(WinControl, PMINMAXINFO(LParam)^);
        Exit(CallDefaultWindowProc(Window, Msg, WParam, LParam));
      end;
  end;
  Result := WindowProc(Window, Msg, WParam, LParam);
end;

class function TWin32WSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  lForm: TCustomForm absolute AWinControl;
  Bounds: TRect;
  SystemMenu: HMenu;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    CalcFormWindowFlags(lForm, Flags, FlagsEx);
    pClassName := @ClsName[0];
    WindowTitle := StrCaption;
    AdjustFormBounds(lForm, Bounds);
    if (lForm.Position in [poDefault, poDefaultPosOnly]) and not (csDesigning in lForm.ComponentState) then
    begin
      Left := CW_USEDEFAULT;
      Top := CW_USEDEFAULT;
    end
    else
    begin
      Left := Bounds.Left;
      Top := Bounds.Top;
    end;
    if (lForm.Position in [poDefault, poDefaultSizeOnly]) and not (csDesigning in lForm.ComponentState) then
    begin
      Width := CW_USEDEFAULT;
      Height := CW_USEDEFAULT;
    end
    else
    begin
      Width := Bounds.Right - Bounds.Left;
      Height := Bounds.Bottom - Bounds.Top;
    end;
    SubClassWndProc := @CustomFormWndProc;
    if not (csDesigning in lForm.ComponentState) and lForm.AlphaBlend then
      FlagsEx := FlagsEx or WS_EX_LAYERED;
  end;
  SetStdBiDiModeParams(AWinControl, Params);
  // create window
  FinishCreateWindow(AWinControl, Params, False);

  Result := Params.Window;

  // remove system menu items for bsDialog
  if (lForm.BorderStyle = bsDialog) and not (csDesigning in lForm.ComponentState) then
  begin
    SystemMenu := GetSystemMenu(Result, False);
    DeleteMenu(SystemMenu, SC_RESTORE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_SIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_MINIMIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, SC_MAXIMIZE, MF_BYCOMMAND);
    DeleteMenu(SystemMenu, 1, MF_BYPOSITION); // remove the separator between move and close
  end;

  // Beginning with Windows 2000 the UI in an application may hide focus
  // rectangles and accelerator key indication. According to msdn we need to
  // initialize all root windows with this message
  if WindowsVersion >= wv2000 then
    Windows.SendMessage(Result, WM_CHANGEUISTATE,
      MakeWParam(UIS_INITIALIZE, UISF_HIDEFOCUS or UISF_HIDEACCEL), 0)
end;

class procedure TWin32WSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
  DragAcceptFiles(AForm.Handle, AValue);
end;

class procedure TWin32WSCustomForm.SetBorderIcons(const AForm: TCustomForm;
          const ABorderIcons: TBorderIcons);
var
  ExStyle, NewStyle: DWORD;
begin
  UpdateWindowStyle(AForm.Handle, CalcBorderIconsFlags(AForm), 
    WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
  ExStyle := GetWindowLong(AForm.Handle, GWL_EXSTYLE);
  NewStyle := (ExStyle and not WS_EX_CONTEXTHELP) or CalcBorderIconsFlagsEx(AForm);
  if ExStyle <> NewStyle then
  begin
    SetWindowLong(AForm.Handle, GWL_EXSTYLE, NewStyle);
    Windows.RedrawWindow(AForm.Handle, nil, 0, RDW_FRAME or RDW_ERASE or RDW_INVALIDATE or RDW_NOCHILDREN);
  end;
end;

class procedure TWin32WSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
          const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
end;

function EnumStayOnTopProc(Handle: HWND; Param: LPARAM): WINBOOL; stdcall;
var
  list: TList absolute Param;
  lWindowInfo: PWin32WindowInfo;
  lWinControl: TWinControl;
begin
  Result := True;
  lWindowInfo := GetWin32WindowInfo(Handle);
  if (lWindowInfo <> nil) then
  begin
    lWinControl := lWindowInfo^.WinControl;
    if (lWinControl <> nil) and (lWinControl is TCustomForm)
      and (TCustomForm(lWinControl).FormStyle in fsAllStayOnTop)
      and not (csDesigning in TCustomForm(lWinControl).ComponentState) then
      list.Add(Pointer(Handle));
  end;
end;

procedure EnumStayOnTop(window: THandle; dstlist: TList);
begin
  EnumThreadWindows(GetWindowThreadProcessId(Window, nil),
    @EnumStayOnTopProc, LPARAM(dstlist));
end;

class procedure TWin32WSCustomForm.SetFormStyle(const AForm: TCustomform;
  const AFormStyle, AOldFormStyle: TFormStyle);
var
  toplist : TList;
  i       : Integer;
begin
  // Some changes don't require RecreateWnd

  // From normal to StayOnTop
  if (AOldFormStyle = fsNormal) and (AFormStyle in fsAllStayOnTop) then begin
    if not (csDesigning in AForm.ComponentState) then
      SetWindowPos(AForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE)
  // From StayOnTop to normal
  end else if (AOldFormStyle in fsAllStayOnTop) and (AFormStyle = fsNormal) then begin

    // NOTE:
    // see bug report #16573
    // if a window changes from HWND_TOPMOST to HWND_NOTOPMOST
    // other TOP most windows also change their state to Non-topmost!

    // the page http://msdn.microsoft.com/en-us/library/ms633545(VS.85).aspx, says:
    // "When a topmost window is made non-topmost, its owners and its owned windows are also made non-topmost windows"
    // Is it possible, that Application window, makes all other forms, non-top most?
    // It's also possible to make a list of "topmost forms" and re-enable their state
    // after changing the style of the window (so recreation can be avoided)

    // Possible solution, using window re-creation
    //if not (csDesigning in AForm.ComponentState) then
    //  RecreateWnd(AForm);


    if not (csDesigning in AForm.ComponentState) then begin
      toplist:=TList.Create;
      try
        EnumStayOnTop(AForm.Handle, toplist);
        SetWindowPos(AForm.Handle, HWND_NOTOPMOST,  0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        for i:=0 to toplist.Count-1 do begin
          if HWND(toplist[i])<>AForm.Handle then
            SetWindowPos(HWND(toplist[i]), HWND_TOPMOST,  0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
        end;
      finally
        toplist.Free;
      end;
    end;

    // original code:
    //  if not (csDesigning in AForm.ComponentState) then
    //    SetWindowPos(AForm.Handle, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE)
  end else begin
    RecreateWnd(AForm);
  end;
end;
                            
class procedure TWin32WSCustomForm.SetBounds(const AWinControl: TWinControl;
    const ALeft, ATop, AWidth, AHeight: Integer);
var
  AForm: TCustomForm absolute AWinControl;
  CurRect, SizeRect: Windows.RECT;
  BorderStyle: TFormBorderStyle;
  L, T, W, H: Integer;
begin
  // the LCL defines the size of a form without border, win32 with.
  // -> adjust size according to BorderStyle
  with SizeRect do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft + AWidth;
    Bottom := ATop + AHeight;
  end;
  BorderStyle := GetDesigningBorderStyle(TCustomForm(AWinControl));
  Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWin32Flags(
      BorderStyle), false, BorderStyleToWin32FlagsEx(BorderStyle));

  L := ALeft;
  T := ATop;
  W := SizeRect.Right - SizeRect.Left;
  H := SizeRect.Bottom - SizeRect.Top;
  
  // we are calling setbounds in TWinControl.Initialize
  // if position is default it will be changed to designed. We do not want this.
  if wcfInitializing in TWinControlAccess(AWinControl).FWinControlFlags then
  begin
    if GetWindowRect(AForm.Handle, CurRect) then
    begin
      if AForm.Position in [poDefault, poDefaultPosOnly] then
      begin
        L := CurRect.Left;
        T := CurRect.Top;
      end;

      if AForm.Position in [poDefault, poDefaultSizeOnly] then
      begin
        W := CurRect.Right - CurRect.Left;
        H := CurRect.Bottom - CurRect.Top;
      end;
    end;
  end;
      
  // rect adjusted, pass to inherited to do real work
  TWin32WSWinControl.SetBounds(AWinControl, L, T, W, H);
end;

class procedure TWin32WSCustomForm.SetIcon(const AForm: TCustomForm; const Small, Big: HICON);
var
  Wnd: HWND;
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon') then
    Exit;
  Wnd := AForm.Handle;
  SendMessage(Wnd, WM_SETICON, ICON_SMALL, LPARAM(Small));
  SetClassLong(Wnd, GCL_HICONSM, LONG(Small));

  SendMessage(Wnd, WM_SETICON, ICON_BIG, LPARAM(Big));
  SetClassLong(Wnd, GCL_HICON, LONG(Big));
  // for some reason sometimes frame does not invalidate itself. lets ask it to invalidate always
  Windows.RedrawWindow(Wnd, nil, 0,
    RDW_INVALIDATE or RDW_FRAME or RDW_NOCHILDREN or RDW_ERASE);
end;

class procedure TWin32WSCustomForm.SetPopupParent(const ACustomForm: TCustomForm;
  const APopupMode: TPopupMode; const APopupParent: TCustomForm);
begin
  // changing parent is not possible without handle recreation
  RecreateWnd(ACustomForm);
end;

class procedure TWin32WSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
var
  OldStyle, NewStyle: DWord;
  Visible, Active: Boolean;
begin
  if not WSCheckHandleAllocated(AForm, 'SetShowInTaskbar') then
    Exit;
  if Assigned(Application) and (AForm = Application.MainForm) then
    Exit;

  OldStyle := GetWindowLong(AForm.Handle, GWL_EXSTYLE);
  NewStyle := OldStyle;
  if AValue = stAlways then
    NewStyle := NewStyle or WS_EX_APPWINDOW
  else
    NewStyle := NewStyle and not WS_EX_APPWINDOW;
  if OldStyle = NewStyle then exit;

  // to apply this changes we need either to hide window or recreate it. Hide is
  // less difficult
  Visible := IsWindowVisible(AForm.Handle);
  Active := GetForegroundWindow = AForm.Handle;
  if Visible then
    ShowWindow(AForm.Handle, SW_HIDE);

  SetWindowLong(AForm.Handle, GWL_EXSTYLE, NewStyle);

  // now we need to restore window visibility with saving focus
  if Visible then
    if Active then
      ShowWindow(AForm.Handle, SW_SHOW)
    else
      ShowWindow(AForm.Handle, SW_SHOWNA);
end;

class procedure TWin32WSCustomForm.ShowHide(const AWinControl: TWinControl);
var
  Flags: dword;
begin
  if AWinControl.HandleObjectShouldBeVisible then
  begin
    case TCustomForm(AWinControl).WindowState of
      wsMaximized: Flags := SW_SHOWMAXIMIZED;
      wsMinimized: Flags := SW_SHOWMINIMIZED;
    else
      Flags := SW_SHOW;
    end;
    Windows.ShowWindow(AWinControl.Handle, Flags);
    { ShowWindow does not send WM_SHOWWINDOW when creating overlapped maximized window }
    { TODO: multiple WM_SHOWWINDOW when maximizing after initial show? }
    if Flags = SW_SHOWMAXIMIZED then
      Windows.SendMessage(AWinControl.Handle, WM_SHOWWINDOW, 1, 0);
  end
  else
    ShowWindow(AWinControl.Handle, SW_HIDE);
end;

class procedure TWin32WSCustomForm.ShowModal(const ACustomForm: TCustomForm);
var
  Parent: HWND;
begin
  Parent := GetParent(ACustomForm.Handle);
  if (Parent <> 0) and (GetWindowLong(Parent, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0) then
    SetWindowPos(ACustomForm.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE)
  else
    BringWindowToTop(ACustomForm.Handle);
end;

class procedure TWin32WSCustomForm.SetAlphaBlend(const ACustomForm: TCustomForm;
  const AlphaBlend: Boolean; const Alpha: Byte);
var
  Style: DWord;
begin
  if not WSCheckHandleAllocated(ACustomForm, 'SetAlphaBlend') then
    Exit;

  Style := GetWindowLong(ACustomForm.Handle, GWL_EXSTYLE);

  if AlphaBlend then
  begin
    if (Style and WS_EX_LAYERED) = 0 then
      SetWindowLong(ACustomForm.Handle, GWL_EXSTYLE, Style or WS_EX_LAYERED);
    Win32Extra.SetLayeredWindowAttributes(ACustomForm.Handle, 0, Alpha, LWA_ALPHA);
  end
  else
  begin
    if (Style and WS_EX_LAYERED) <> 0 then
      SetWindowLong(ACustomForm.Handle, GWL_EXSTYLE, Style and not WS_EX_LAYERED);
    RedrawWindow(ACustomForm.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
  end;
end;

{ TWin32WSHintWindow }

class function TWin32WSHintWindow.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsHintName[0];
    WindowTitle := StrCaption;
    Flags := WS_POPUP;
    FlagsEx := FlagsEx or WS_EX_TOOLWINDOW;
    Left := LongInt(CW_USEDEFAULT);
    Top := LongInt(CW_USEDEFAULT);
    Width := LongInt(CW_USEDEFAULT);
    Height := LongInt(CW_USEDEFAULT);
    SubClassWndProc := @CustomFormWndProc;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWin32WSHintWindow.ShowHide(const AWinControl: TWinControl);
begin
  if AWinControl.HandleObjectShouldBeVisible then
    Windows.SetWindowPos(AWinControl.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_NOOWNERZORDER)
  else
    Windows.ShowWindow(AWinControl.Handle, SW_HIDE);
end;

end.
