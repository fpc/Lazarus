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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  private
  protected
  public
    class procedure ScrollBy(const AWinControl: TScrollingWinControl;
      const DeltaX, DeltaY: integer); override;
  end;

  { TWin32WSScrollBox }

  TWin32WSScrollBox = class(TWSScrollBox)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWin32WSCustomFrame }

  TWin32WSCustomFrame = class(TWSCustomFrame)
  private
  protected
  public
  end;

  { TWin32WSFrame }

  TWin32WSFrame = class(TWSFrame)
  private
  protected
  public
  end;

  { TWin32WSCustomForm }

  TWin32WSCustomForm = class(TWSCustomForm)
  private
  protected
  public
    class procedure CloseModal(const ACustomForm: TCustomForm); override;
    class procedure SetAllowDropFiles(const AForm: TCustomForm; AValue: Boolean); override;
    class procedure SetBorderIcons(const AForm: TCustomForm;
          const ABorderIcons: TBorderIcons); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, 
          AWidth, AHeight: Integer); override;
    class procedure SetFormBorderStyle(const AForm: TCustomForm;
                             const AFormBorderStyle: TFormBorderStyle); override;
    class procedure SetIcon(const AForm: TCustomForm; const AIcon: HICON); override;
    class procedure SetShowInTaskbar(const AForm: TCustomForm; const AValue: TShowInTaskbar); override;
    class procedure ShowModal(const ACustomForm: TCustomForm); override;
  end;

  { TWin32WSForm }

  TWin32WSForm = class(TWSForm)
  private
  protected
  public
  end;

  { TWin32WSHintWindow }

  TWin32WSHintWindow = class(TWSHintWindow)
  private
  protected
  public
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWin32WSScreen }

  TWin32WSScreen = class(TWSScreen)
  private
  protected
  public
  end;

  { TWin32WSApplicationProperties }

  TWin32WSApplicationProperties = class(TWSApplicationProperties)
  private
  protected
  public
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
        lhFont := GetStockObject(DEFAULT_GUI_FONT)
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
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    //TODO: Make control respond to user scroll request
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

function CalcBorderIconsFlags(const AForm: TCustomForm): dword;
var
  BorderIcons: TBorderIcons;
begin
  Result := 0;
  BorderIcons := AForm.BorderIcons;
  if biSystemMenu in BorderIcons then
    Result := Result or WS_SYSMENU;
  if GetDesigningBorderStyle(AForm) in [bsNone, bsSingle, bsSizeable] then
  begin
    if biMinimize in BorderIcons then
      Result := Result or WS_MINIMIZEBOX;
    if biMaximize in BorderIcons then
      Result := Result or WS_MAXIMIZEBOX;
  end;
end;

procedure CalcFormWindowFlags(const AForm: TCustomForm; var Flags, FlagsEx: dword);
var
  BorderStyle: TFormBorderStyle;
begin
  BorderStyle := GetDesigningBorderStyle(AForm);
  Flags := BorderStyleToWin32Flags(BorderStyle);
  if AForm.Parent <> nil then
    Flags := (Flags or WS_CHILD) and not WS_POPUP;
  FlagsEx := BorderStyleToWin32FlagsEx(BorderStyle);
  if (AForm.FormStyle in fsAllStayOnTop) and 
      not (csDesigning in AForm.ComponentState) then
    FlagsEx := FlagsEx or WS_EX_TOPMOST;
  Flags := Flags or CalcBorderIconsFlags(AForm);
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

class function TWin32WSCustomForm.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
  lForm: TCustomForm;
  Bounds: TRect;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, Params);
  // customization of Params
  with Params do
  begin
    lForm := TCustomForm(AWinControl);
    CalcFormWindowFlags(lForm, Flags, FlagsEx);
    pClassName := @ClsName[0];
    WindowTitle := StrCaption;
    AdjustFormBounds(lForm, Bounds);
    if lForm.Position in [poDefault, poDefaultPosOnly] then
    begin
      Left := CW_USEDEFAULT;
      Top := CW_USEDEFAULT;
    end
    else
    begin
      Left := Bounds.Left;
      Top := Bounds.Top;
    end;
    if lForm.Position in [poDefault, poDefaultSizeOnly] then
    begin
      Width := CW_USEDEFAULT;
      Height := CW_USEDEFAULT;
    end
    else
    begin
      Width := Bounds.Right - Bounds.Left;
      Height := Bounds.Bottom - Bounds.Top;
    end;
    SubClassWndProc := nil;
    if ((Application = nil) or (Application.MainForm <> lForm))  and
       ( not (csDesigning in lForm.ComponentState) and
        (lForm.ShowInTaskBar = stAlways)) then
      Parent := 0;
  end;
  SetStdBiDiModeParams(AWinControl, Params);
  // create window
  FinishCreateWindow(AWinControl, Params, false);

  Result := Params.Window;
end;

class procedure TWin32WSCustomForm.CloseModal(const ACustomForm: TCustomForm);
begin
  EnableApplicationWindows(ACustomForm.Handle);
end;

class procedure TWin32WSCustomForm.SetAllowDropFiles(const AForm: TCustomForm;
  AValue: Boolean);
begin
  DragAcceptFiles(AForm.Handle, AValue);
end;

class procedure TWin32WSCustomForm.SetBorderIcons(const AForm: TCustomForm;
          const ABorderIcons: TBorderIcons);
begin
  UpdateWindowStyle(AForm.Handle, CalcBorderIconsFlags(AForm), 
    WS_SYSMENU or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);
  SetIcon(AForm, 0);
end;

class procedure TWin32WSCustomForm.SetFormBorderStyle(const AForm: TCustomForm;
          const AFormBorderStyle: TFormBorderStyle);
begin
  RecreateWnd(AForm);
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
  // if position is default it will be changed to designed. We dont want this.
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

class procedure TWin32WSCustomForm.SetIcon(const AForm: TCustomForm; const AIcon: HICON);
begin
  if not WSCheckHandleAllocated(AForm, 'SetIcon') then
    Exit;
  SendMessage(AForm.Handle, WM_SETICON, ICON_BIG, LPARAM(AIcon));
end;

class procedure TWin32WSCustomForm.SetShowInTaskbar(const AForm: TCustomForm;
  const AValue: TShowInTaskbar);
begin
  if not WSCheckHandleAllocated(AForm, 'SetShowInTaskbar') then
    Exit;
  if (Application <> nil) and (AForm = Application.MainForm) then
    Exit;

  RecreateWnd(AForm);
end;

class procedure TWin32WSCustomForm.ShowModal(const ACustomForm: TCustomForm);
begin
  DisableApplicationWindows(ACustomForm.Handle);
  ShowWindow(ACustomForm.Handle, SW_SHOW);
  BringWindowToTop(ACustomForm.Handle);
end;

{ TWin32WSHintWindow }

class function TWin32WSHintWindow.CreateHandle(const AWinControl: TWinControl;
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
    WindowTitle := StrCaption;
    Flags := dword(WS_POPUP);
    FlagsEx := FlagsEx or WS_EX_TOOLWINDOW;
    Left := LongInt(CW_USEDEFAULT);
    Top := LongInt(CW_USEDEFAULT);
    Width := LongInt(CW_USEDEFAULT);
    Height := LongInt(CW_USEDEFAULT);
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
  RegisterWSComponent(TScrollingWinControl, TWin32WSScrollingWinControl);
  RegisterWSComponent(TScrollBox, TWin32WSScrollBox);
//  RegisterWSComponent(TCustomFrame, TWin32WSCustomFrame);
//  RegisterWSComponent(TFrame, TWin32WSFrame);
  RegisterWSComponent(TCustomForm, TWin32WSCustomForm);
//  RegisterWSComponent(TForm, TWin32WSForm);
  RegisterWSComponent(THintWindow, TWin32WSHintWindow);
//  RegisterWSComponent(TScreen, TWin32WSScreen);
//  RegisterWSComponent(TApplicationProperties, TWin32WSApplicationProperties);
////////////////////////////////////////////////////
end.
