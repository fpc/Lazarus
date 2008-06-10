{ $Id: wsforms.pp 7361 2005-07-16 00:08:26Z marc $}
{
 *****************************************************************************
 *                                WSForms.pp                                 * 
 *                                ----------                                 * 
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
unit WinCEWSForms;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL, LCL
  Windows,
  SysUtils, Controls, LCLType, Forms, InterfaceBase,
  // Widgetset
  winceproc, wincewscontrols,
  WSForms, WSLCLClasses;

type

  { TWinCEWSScrollingWinControl }

  TWinCEWSScrollingWinControl = class(TWSScrollingWinControl)
  private
  protected
  public
    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
      const DeltaX, DeltaY: integer); override;
  end;

  { TWinCEWSScrollBox }

  TWinCEWSScrollBox = class(TWSScrollBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWinCEWSCustomFrame }

  TWinCEWSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TWinCEWSFrame }

  TWinCEWSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TWinCEWSCustomForm }

  TWinCEWSCustomForm = class(TWSCustomForm)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): HWND; override;

    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetBounds(const AWinControl: TWinControl;
      const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
                                   const ABorderIcons: TBorderIcons); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
  end;

  { TWinCEWSForm }

  TWinCEWSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TWinCEWSHintWindow }

  TWinCEWSHintWindow = class(TWSHintWindow)
  private
  protected
  public
  end;

  { TWinCEWSScreen }

  TWinCEWSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TWinCEWSApplicationProperties }

  TWinCEWSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
  end;


implementation

uses Winceint;

{ TWinCEWSScrollBox }

class function TWinCEWSScrollBox.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    //TODO: Make control respond to user scroll request
    FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    pClassName := @ClsName;
    Flags := Flags or WS_HSCROLL or WS_VSCROLL;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

{ TWinCEWSScrollingWinControl }

{$ifdef win32}
function ScrollWindowPtr(hWnd:HWND; XAmount:longint; YAmount:longint; lpRect: pointer; lpClipRect: pointer):WINBOOL; stdcall; external 'user32' name 'ScrollWindow';
{$else}
function ScrollWindowPtr(hWnd:HWND; dx:longint; dy:longint; prcScroll: lpRECT; prcClip: lpRECT;
  hrgnUpdate: HRGN; prcUpdate: LPRECT; flags:UINT):longint; external KernelDll name 'ScrollWindowEx';
{$endif}

class procedure TWinCEWSScrollingWinControl.ScrollBy(const AWinControl: TScrollingWinControl;
  const DeltaX, DeltaY: integer);
var
  lVisible: boolean;
  rgn : HRGN;
  rect : trect;
begin
 lVisible := AWinControl.HandleAllocated and Windows.IsWindowVisible(AWinControl.Handle);
 rgn := 0; //roozbeh : seems to be ok?
 // GetClipRgn(AWinControl.Handle,rgn);
 // roozbeh:which flags really are required?!
 if lVisible then
  {$ifdef win32}
  ScrollWindowPtr(AWinControl.Handle, DeltaX, DeltaY, nil, nil);
  {$else}
  ScrollWindowPtr(AWinControl.Handle, DeltaX, DeltaY, nil, nil,
    rgn, nil, SW_INVALIDATE or SW_ERASE or SW_SCROLLCHILDREN);
  {$endif}
end;

{ TWinCEWSCustomForm }

function CalcBorderIconsFlags(const AForm: TCustomForm): dword;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if biSystemMenu in BorderIcons then
    Result := Result or WS_SYSMENU;
  if AForm.BorderStyle in [bsNone, bsSingle, bsSizeable] then
  begin
    if biMinimize in BorderIcons then
      Result := Result or WS_MINIMIZE;
    if biMaximize in BorderIcons then
      Result := Result or WS_MAXIMIZE;
  end;
end;

procedure CalcFormWindowFlags(const AForm: TCustomForm; var Flags, FlagsEx: dword);
var
  BorderStyle: TFormBorderStyle;
begin
  BorderStyle := AForm.BorderStyle;
  Flags := BorderStyleToWin32Flags(BorderStyle);
  if AForm.Parent <> nil then
    Flags := (Flags or WS_CHILD) and not WS_POPUP;
  FlagsEx := BorderStyleToWin32FlagsEx(BorderStyle);
  if (AForm.FormStyle in fsAllStayOnTop) then
    FlagsEx := FlagsEx or WS_EX_TOPMOST;
  Flags := Flags or CalcBorderIconsFlags(AForm);
end;

procedure AdjustFormBounds(const AForm: TCustomForm; var SizeRect: TRect);
begin
  // the LCL defines the size of a form without border, win32 with.
  // -> adjust size according to BorderStyle
  SizeRect := AForm.BoundsRect;
  Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWin32Flags(
    AForm.BorderStyle), false, BorderStyleToWin32FlagsEx(AForm.BorderStyle));
end;

procedure CalculateDialogPosition(var Params: TCreateWindowExParams;
 Bounds: TRect; lForm: TCustomForm);
begin
  if lForm.Position in [poDefault, poDefaultPosOnly] then
  begin
    Params.Left := CW_USEDEFAULT;
    Params.Top := CW_USEDEFAULT;
  end
  else
  begin
    Params.Left := Bounds.Left;
    Params.Top := Bounds.Top;
  end;
  if lForm.Position in [poDefault, poDefaultSizeOnly] then
  begin
    Params.Width := CW_USEDEFAULT;
    Params.Height := CW_USEDEFAULT;
  end
  else
  begin
    Params.Width := Bounds.Right - Bounds.Left;
    Params.Height := Bounds.Bottom - Bounds.Top;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinCEWSCustomForm.CreateHandle
  Params:  None
  Returns: Nothing

  Creates a Windows CE Form, initializes it according to it´s properties and shows it
 ------------------------------------------------------------------------------}
class function TWinCEWSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  LForm : TCustomForm;
  BorderStyle: TFormBorderStyle;
  WR: Windows.RECT;
  Bounds: TRect;
begin
  {$ifdef VerboseWinCE}
  WriteLn('TWinCEWSCustomForm.CreateHandle');
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  
  // customization of Params
  with Params do
  begin
    //TODO: Make control respond to user scroll request
    pClassName := @ClsName;
    FlagsEx := 0;
    Flags := WS_OVERLAPPEDWINDOW;
    WindowTitle := StrCaption;
    lForm := TCustomForm(AWinControl);
    CalcFormWindowFlags(lForm, Flags, FlagsEx);
    SubClassWndProc := nil;
    Parent := 0;
    BorderStyle := TCustomForm(AWinControl).BorderStyle;
    AdjustFormBounds(lForm, Bounds);

    Windows.SystemParametersInfo(SPI_GETWORKAREA, 0, @WR, 0);

    if Application.ApplicationType in [atPDA, atSmartphone, atDefault] then
    begin
      { The position and size of common windows is ignored on PDA mode,
        and a position and size that covers the whole workarea excluding
        the menu is used. The Workarea size automatically excludes the
        Taskbar.

        Simply using CM_USEDEFAULT produces a too large Height, which
        covers the menus. So the workarea size is detected (which ignores
        the Taskbar).

        In some devices subtracting the menu size seams to work better, but
        others, if no menu is present, it's a big problem.
      }
      if (BorderStyle <> bsDialog) and (BorderStyle <> bsNone) then
      begin
        Left := WR.Left;
        Top := WR.Top;
        Height := WR.Bottom - WR.Top;// - GetSystemMetrics(SM_CYMENU)
         //- GetSystemMetrics(SM_CXBORDER) * 2;
        Width := WR.Right - WR.Left;
      end
      else if (BorderStyle = bsDialog) then
      { On normal dialogs we need to take into consideration the size of
        the window decoration.

        For the Top and Left coordinates, using CM_USEDEFAULT produces
        a wrong and bad result. Using the Workarea rectagle works fine.
      }
      begin
        Top := WR.Top;
        Left := WR.Left;
        Height := Bounds.Bottom - Bounds.Top;
        Width := Bounds.Right - Bounds.Left;
      end
      else { BorderStyle = bsNone }
      { On borderless Windows we allow the user full control of the
        window position
      }
      begin
        CalculateDialogPosition(Params, Bounds, lForm);
      end;
    end
    else
    { On Desktop mode we need to take into consideration the size of
      the window decoration }
      CalculateDialogPosition(Params, Bounds, lForm);
  end;
  
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  // TODO: proper icon, for now set default icon
  SetIcon(TCustomForm(AWinControl), 0);
  Result := Params.Window;

  {$ifdef VerboseWinCE}
  WriteLn('Window Handle = ' + IntToStr(Result));
  {$endif}
end;

class procedure TWinCEWSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  EnableApplicationWindows(ACustomForm.Handle);
end;

class procedure TWinCEWSCustomForm.SetBorderIcons(const AForm: TCustomForm;
 const ABorderIcons: TBorderIcons);
begin
  UpdateWindowStyle(AForm.Handle, CalcBorderIconsFlags(AForm),
    WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
  SetIcon(AForm, 0);
end;

class procedure TWinCEWSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
          const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
end;
                            
class procedure TWinCEWSCustomForm.SetBounds(const AWinControl: TWinControl;
    const ALeft, ATop, AWidth, AHeight: Integer);
var
  SizeRect: Windows.RECT;
  BorderStyle: TFormBorderStyle;
  WR: Windows.RECT;
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
  BorderStyle := TCustomForm(AWinControl).BorderStyle;
  Windows.AdjustWindowRectEx(@SizeRect, BorderStyleToWin32Flags(
      BorderStyle), false, BorderStyleToWin32FlagsEx(BorderStyle));

  if (Application.ApplicationType in [atPDA, atSmartphone, atDefault]) then
  begin
    { We should never move forms which are in full-screen mode }
    if (BorderStyle <> bsDialog) and (BorderStyle <> bsNone) then Exit;

    { On normal dialogs we need to take into consideration the size of
      the window decoration. }
    if (BorderStyle = bsDialog) then
    begin
      Windows.SystemParametersInfo(SPI_GETWORKAREA, 0, @WR, 0);
      SizeRect.Top := WR.Top;
      SizeRect.Left := WR.Left;
    end;
    { On borderless Windows we allow the user full control of the window position }
  end;

  // rect adjusted, pass to inherited to do real work
  TWinCEWSWinControl.SetBounds(AWinControl, SizeRect.Left, SizeRect.Top,
    SizeRect.Right - SizeRect.Left, SizeRect.Bottom - SizeRect.Top);
end;

class procedure TWinCEWSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
var
  winHandle: HWND;
  iconHandle: HICON;
begin
  winHandle := AForm.Handle;
  if AForm.BorderStyle = bsDialog then
    iconHandle := 0
{ TODO: fix icon handling
  else
  if AIcon <> 0 then
    iconHandle := AIcon
}
  else
    iconHandle := Windows.LoadIcon(MainInstance, 'MAINICON');
  SendMessage(winHandle, WM_SETICON, ICON_BIG, iconHandle);
end;

class procedure TWinCEWSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
  if not AForm.HandleAllocated then exit;
  if (Application <> nil) and (AForm = Application.MainForm) then
    exit;

  RecreateWnd(AForm);
end;

class procedure TWinCEWSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  DisableApplicationWindows(ACustomForm.Handle);
  ShowWindow(ACustomForm.Handle, SW_SHOW);
  BringWindowToTop(ACustomForm.Handle);
end;


initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TScrollingWinControl, TWinCEWSScrollingWinControl);
  RegisterWSComponent(TScrollBox, TWinCEWSScrollBox);
//  RegisterWSComponent(TCustomFrame, TWinCEWSCustomFrame);
//  RegisterWSComponent(TFrame, TWinCEWSFrame);
  RegisterWSComponent(TCustomForm, TWinCEWSCustomForm);
//  RegisterWSComponent(TForm, TWinCEWSForm);
//  RegisterWSComponent(THintWindow, TWinCEWSHintWindow);
//  RegisterWSComponent(TScreen, TWinCEWSScreen);
//  RegisterWSComponent(TApplicationProperties, TWinCEWSApplicationProperties);
////////////////////////////////////////////////////
end.
