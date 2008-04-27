{ $Id$}
{
 *****************************************************************************
 *                            Win32WSControls.pp                             *
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
unit Win32WSControls;

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
  CommCtrl, Windows, Classes, Controls, Graphics,
////////////////////////////////////////////////////
  WSControls, WSLCLClasses, SysUtils, Win32Proc, Win32Extra, WSProc,
  { TODO: needs to move }
  Forms, ComCtrls, Buttons, StdCtrls, ExtCtrls, GraphMath, GraphType,
  InterfaceBase, LCLIntf, LCLType, LCLProc;

type
  { TWin32WSDragImageList }

  TWin32WSDragImageList = class(TWSDragImageList)
  private
  protected
  public
    class function BeginDrag(const ADragImageList: TDragImageList; Window: HWND;
      AIndex, X, Y: Integer): Boolean; override;
    class function DragMove(const ADragImageList: TDragImageList; X, Y: Integer): Boolean; override;
    class procedure EndDrag(const ADragImageList: TDragImageList); override;
    class function HideDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; override;
    class function ShowDragImage(const ADragImageList: TDragImageList;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; override;
  end;

  { TWin32WSControl }

  TWin32WSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TWin32WSWinControl }

  TWin32WSWinControl = class(TWSWinControl)
  private
  protected
  public
    class procedure AddControl(const AControl: TControl); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetBiDiMode(const AWinControl: TWinControl; UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean); override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;
    class procedure SetCursor(const AWinControl: TWinControl; const ACursor: HCursor); override;
    class procedure SetShape(const AWinControl: TWinControl; const AShape: HBITMAP); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure PaintTo(const AWinControl: TWinControl; ADC: HDC; X, Y: Integer); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
  end;

  { TWin32WSGraphicControl }

  TWin32WSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWin32WSCustomControl }

  TWin32WSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWin32WSImageList }

  TWin32WSImageList = class(TWSImageList)
  private
  protected
  public
  end;


type
  TCreateWindowExParams = record
    Buddy, Parent, Window: HWND;
    Left, Top, Height, Width: integer;
    WindowInfo, BuddyWindowInfo: PWindowInfo;
    MenuHandle: HMENU;
    Flags, FlagsEx: dword;
    SubClassWndProc: pointer;
    WindowTitle, StrCaption: PChar;
    pClassName: PChar;
  end;


// TODO: better names?

procedure PrepareCreateWindow(const AWinControl: TWinControl; var Params: TCreateWindowExParams);
procedure FinishCreateWindow(const AWinControl: TWinControl; var Params: TCreateWindowExParams;
  const AlternateCreateWindow: boolean);
procedure WindowCreateInitBuddy(const AWinControl: TWinControl;
  var Params: TCreateWindowExParams);
  
// Must be in win32proc but TCreateWindowExParams declared here
procedure SetStdBiDiModeParams(const AWinControl: TWinControl; var Params:TCreateWindowExParams);

implementation

uses
  Win32Int, Win32WSButtons;

{ Global helper routines }

procedure PrepareCreateWindow(const AWinControl: TWinControl; var Params: TCreateWindowExParams);
begin
  with Params do
  begin
    Flags := WS_CHILD or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
    FlagsEx := 0;
    Assert(False, 'Trace:Setting flags');
    Window := HWND(nil);
    Buddy := HWND(nil);
    Assert(False, 'Trace:Setting window');

    if AWinControl.Parent <> nil then
      Parent := AWinControl.Parent.Handle
    else
      Parent := TWin32WidgetSet(WidgetSet).AppHandle;

    SubClassWndProc := @WindowProc;
    StrCaption := PChar(AWinControl.Caption);
    WindowTitle := nil;
    Height := AWinControl.Height;
    Left := AWinControl.Left;
    //Parent := AWinControl.Parent;
    Top := AWinControl.Top;
    Width := AWinControl.Width;
    if AWinControl.Visible then
      Flags := Flags or WS_VISIBLE;
    if csAcceptsControls in AWinControl.ControlStyle then
      FlagsEx := FlagsEx or WS_EX_CONTROLPARENT;
    if AWinControl.TabStop then
      Flags := Flags or WS_TABSTOP;
    Assert(False, 'Trace:Setting dimentions');
    LCLBoundsToWin32Bounds(AWinControl, Left, Top, Width, Height);
    if AWinControl is TCustomControl then
      if TCustomControl(AWinControl).BorderStyle = bsSingle then
        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;
    SetStdBiDiModeParams(AWinControl, Params);
    {$IFDEF VerboseSizeMsg}
    DebugLn('PrepareCreateWindow ' + dbgsName(AWinControl) + ' ' +
      Format('%d, %d, %d, %d', [Left, Top, Width, Height]));
    {$ENDIF}

    Assert(False, Format('Trace:PrepareCreateWindow - Creating component %S with the caption of %S', [AWinControl.ClassName, AWinControl.Caption]));
    Assert(False, Format('Trace:PrepareCreateWindow - Left: %D, Top: %D, Width: %D, Height: %D, Parent handle: 0x%X, instance handle: 0x%X', [Left, Top, Width, Height, Parent, HInstance]));
  end;
end;

procedure FinishCreateWindow(const AWinControl: TWinControl; var Params: TCreateWindowExParams;
  const AlternateCreateWindow: boolean);
var
  lhFont: HFONT;
begin
  if not AlternateCreateWindow then
  begin
    with Params do
    begin
      if (Flags and WS_CHILD) <> 0 then
      begin
        // menu handle is also for specifying a control id if this is a child
        MenuHandle := HMENU(AWinControl);
      end else begin
        MenuHandle := HMENU(nil);
      end;

      {$ifdef WindowsUnicodeSupport}
      if UnicodeEnabledOS then
        Window := CreateWindowExW(FlagsEx, PWideChar(WideString(pClassName)),
          PWideChar(Utf8Decode(WindowTitle)), Flags,
          Left, Top, Width, Height, Parent, MenuHandle, HInstance, Nil)
      else
        Window := CreateWindowEx(FlagsEx, pClassName, PChar(Utf8ToAnsi(WindowTitle)), Flags,
          Left, Top, Width, Height, Parent, MenuHandle, HInstance, Nil);
      {$else}
        Window := CreateWindowEx(FlagsEx, pClassName, WindowTitle, Flags,
          Left, Top, Width, Height, Parent, MenuHandle, HInstance, Nil);
      {$endif}

      if Window = 0 then
      begin
        raise exception.create('failed to create win32 control, error: '+IntToStr(GetLastError()));
      end;
    end;
    { after creating a child window the following happens:
      1) the previously bottom window is thrown to the top
      2) the created window is added at the bottom
      undo this by throwing them both to the bottom again }
    { not needed anymore, tab order is handled entirely by LCL now
    Windows.SetWindowPos(Windows.GetTopWindow(Parent), HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    Windows.SetWindowPos(Window, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    }
  end;

  with Params do
  begin
    if Window <> HWND(Nil) then
    begin
      // some controls (combobox) immediately send a message upon setting font
      WindowInfo := AllocWindowInfo(Window);
      if GetWindowInfo(Parent)^.needParentPaint then
        WindowInfo^.needParentPaint := true;
      WindowInfo^.WinControl := AWinControl;
      AWinControl.Handle := Window;
      if SubClassWndProc <> nil then
        WindowInfo^.DefWndProc := Windows.WNDPROC(SetWindowLong(
          Window, GWL_WNDPROC, PtrInt(SubClassWndProc)));
      if AWinControl.Font.IsDefault then
        lhFont := GetStockObject(DEFAULT_GUI_FONT)
      else
        lhFont := AWinControl.Font.Reference.Handle;
      Windows.SendMessage(Window, WM_SETFONT, WPARAM(lhFont), 0)
    end;
  end;
end;

procedure WindowCreateInitBuddy(const AWinControl: TWinControl;
  var Params: TCreateWindowExParams);
var
  lhFont: HFONT;
begin
  with Params do
    if Buddy <> HWND(Nil) then
    begin
      BuddyWindowInfo := AllocWindowInfo(Buddy);
      BuddyWindowInfo^.AWinControl := AWinControl;
      BuddyWindowInfo^.DefWndProc := Windows.WNDPROC(SetWindowLong(
        Buddy, GWL_WNDPROC, PtrInt(SubClassWndProc)));
      if AWinControl.Font.IsDefault then
        lhFont := GetStockObject(DEFAULT_GUI_FONT)
      else
        lhFont := AWinControl.Font.Reference.Handle;
      Windows.SendMessage(Buddy, WM_SETFONT, lhFont, 0);
    end
    else
      BuddyWindowInfo := nil;
end;

procedure SetStdBiDiModeParams(const AWinControl: TWinControl; var Params:TCreateWindowExParams);
begin
  with Params do
  begin
    //remove old bidimode ExFlags
    FlagsEx := FlagsEx and not(WS_EX_RTLREADING or WS_EX_RIGHT or WS_EX_LEFTSCROLLBAR);

    if AWinControl.UseRightToLeftAlignment then
      FlagsEx := FlagsEx or WS_EX_RIGHT;
    if AWinControl.UseRightToLeftScrollBar then
      FlagsEx := FlagsEx or WS_EX_LEFTSCROLLBAR;
    if AWinControl.UseRightToLeftReading then
      FlagsEx := FlagsEx or WS_EX_RTLREADING;
  end;
end;

{ TWin32WSWinControl }

class function TWin32WSWinControl.CreateHandle(const AWinControl: TWinControl;
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
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWin32WSWinControl.AddControl(const AControl: TControl);
var
  ParentPanelHandle, ParentHandle, ChildHandle: HWND;
begin
  {$ifdef OldToolbar}
  if (AControl.Parent is TToolbar) then
    exit;
  {$endif}

  with TWinControl(AControl) do
  begin
    Assert(False, Format('Trace:[TWin32WSWinControl.AddControl] %S --> Calling Add Child: %S', [Parent.ClassName, ClassName]));
    ParentHandle := Parent.Handle;
    ChildHandle := Handle;
  end;

  Assert(False, 'Trace:AddControl - Parent Window Handle is $' + IntToHex(LongInt(ParentHandle), 8));
  Assert(False, 'Trace:AddControl - Child Window Handle is $' + IntToHex(LongInt(ChildHandle), 8));
  // handle groupbox exception
  ParentPanelHandle := GetWindowInfo(ChildHandle)^.ParentPanel;
  if ParentPanelHandle <> 0 then
    ChildHandle := ParentPanelHandle;
  SetParent(ChildHandle, ParentHandle);
end;

class function  TWin32WSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  AText := '';
  Result := false;
end;

class procedure TWin32WSWinControl.SetBiDiMode(const AWinControl : TWinControl;
  UseRightToLeftAlign, UseRightToLeftReading, UseRightToLeftScrollBar : Boolean
  );
var
  FlagsEx: dword;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetBiDiMode') then
    Exit;

  FlagsEx := GetWindowLong(AWinControl.Handle, GWL_EXSTYLE);
  FlagsEx := FlagsEx and not (WS_EX_RTLREADING or WS_EX_RIGHT or WS_EX_LEFTSCROLLBAR);
  if UseRightToLeftAlign then
    FlagsEx := FlagsEx or WS_EX_RIGHT;
  if UseRightToLeftReading then
    FlagsEx := FlagsEx or WS_EX_RTLREADING ;
  if UseRightToLeftScrollBar then
    FlagsEx := FlagsEx or WS_EX_LEFTSCROLLBAR;
  SetWindowLong(AWinControl.Handle, GWL_EXSTYLE, FlagsEx);
end;

class procedure TWin32WSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  RecreateWnd(AWinControl);
end;

class procedure TWin32WSWinControl.SetChildZPosition(
  const AWinControl, AChild: TWinControl; const AOldPos, ANewPos: Integer;
  const AChildren: TFPList);
var
  AfterWnd: hWnd;
  n, StopPos: Integer;
  Child: TWinControl;
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetChildZPosition')
  then Exit;
  if not WSCheckHandleAllocated(AChild, 'SetChildZPosition (child)')
  then Exit;

  if ANewPos = 0 // bottom
  then AfterWnd := HWND_BOTTOM
  else if ANewPos >= AChildren.Count - 1
  then AfterWnd := HWND_TOP
  else begin
    // Search for the first child above us with a handle
    // the child list is reversed form the windows order.
    // So the first window is the top window and is the last child
    // if we don't find a allocated handle then we are effectively not moved
    AfterWnd := 0;
    if AOldPos > ANewPos
    then StopPos := AOldPos              // The child is moved to the bottom, oldpos is on top of it
    else StopPos := AChildren.Count - 1; // the child is moved to the top

    for n := ANewPos + 1 to StopPos do
    begin
      Child := TWinControl(AChildren[n]);
      if Child.HandleAllocated
      then begin
        AfterWnd := Child.Handle;
        Break;
      end;
    end;

    if AfterWnd = 0 then Exit; // nothing to do
  end;

  Windows.SetWindowPos(AChild.Handle, AfterWnd, 0, 0, 0, 0,
    SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOOWNERZORDER or
    SWP_NOSIZE or SWP_NOSENDCHANGING or SWP_DEFERERASE);
end;

{------------------------------------------------------------------------------
  Method:  SetBounds
  Params:  AWinControl                  - the object which invoked this function
           ALeft, ATop, AWidth, AHeight - new dimensions for the control
  Pre:     AWinControl.HandleAllocated
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
class procedure TWin32WSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  IntfLeft, IntfTop, IntfWidth, IntfHeight: integer;
  suppressMove: boolean;
begin
  IntfLeft := ALeft; IntfTop := ATop;
  IntfWidth := AWidth; IntfHeight := AHeight;
  LCLBoundsToWin32Bounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight);
  {$IFDEF VerboseSizeMsg}
  DebugLn('TWin32WSWinControl.ResizeWindow A ', dbgsName(AWinControl),
    ' LCL=',Format('%d, %d, %d, %d', [ALeft,ATop,AWidth,AHeight]),
    ' Win32=',Format('%d, %d, %d, %d', [IntfLeft,IntfTop,IntfWidth,IntfHeight])
    );
  {$ENDIF}
  suppressMove := false;
  AdaptBounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight, suppressMove);
  if not suppressMove then
    MoveWindow(AWinControl.Handle, IntfLeft, IntfTop, IntfWidth, IntfHeight, true);
  LCLControlSizeNeedsUpdate(AWinControl, false);
end;

class procedure TWin32WSWinControl.SetColor(const AWinControl: TWinControl);
begin
  // TODO: to be implemented, had no implementation in LM_SETCOLOR message
end;

class procedure TWin32WSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetFont')
  then Exit;
  Windows.SendMessage(AWinControl.Handle, WM_SETFONT, Windows.WParam(AFont.Reference.Handle), 1);
end;

class procedure TWin32WSWinControl.SetText(const AWinControl: TWinControl; const AText: string);
Begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;

{$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS 
  then Windows.SetWindowTextW(AWinControl.Handle, PWideChar(Utf8Decode(AText)))
  else Windows.SetWindowText(AWinControl.Handle, PChar(Utf8ToAnsi(AText)));
{$else}
  Windows.SetWindowText(AWinControl.Handle, PChar(AText));
{$endif}
End;

class procedure TWin32WSWinControl.SetCursor(const AWinControl: TWinControl;
  const ACursor: HCursor);
begin
  // in win32 controls have no cursor property. they can change their cursor
  // by listening WM_SETCURSOR and adjusting global cursor
  if csDesigning in AWinControl.ComponentState then
    Windows.SetCursor(ACursor);
end;

class procedure TWin32WSWinControl.SetShape(const AWinControl: TWinControl;
  const AShape: HBITMAP);
var
  Rgn: HRGN;
begin
  if not WSCheckHandleAllocated(AWinControl, 'SetShape') then
    Exit;

  if AShape <> 0 then
    Rgn := BitmapToRegion(AShape)
  else
    Rgn := 0;
  SetWindowRgn(AWinControl.Handle, Rgn, True);
  if Rgn <> 0 then
    DeleteObject(Rgn);
end;

class procedure TWin32WSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin
  // TODO: implement me!
end;

class procedure TWin32WSWinControl.DestroyHandle(const AWinControl: TWinControl);
var
  Handle: HWND;
  AccelTable: HACCEL;
begin
  Handle := AWinControl.Handle;
  AccelTable := GetWindowInfo(Handle)^.Accel;
  if AccelTable <> 0 then
    DestroyAcceleratorTable(AccelTable);
  DestroyWindow(Handle);
end;

class procedure TWin32WSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  // lpRect = nil updates entire client area of window
  InvalidateRect(AWinControl.Handle, nil, true);
end;

class procedure TWin32WSWinControl.PaintTo(const AWinControl: TWinControl;
  ADC: HDC; X, Y: Integer);
var
  SavedDC: Integer;
begin
  SavedDC := SaveDC(ADC);
  MoveWindowOrgEx(ADC, X, Y);
  SendMessage(AWinControl.Handle, WM_PRINT, ADC,
    PRF_CHECKVISIBLE or PRF_CHILDREN or PRF_CLIENT or PRF_NONCLIENT or PRF_OWNED);
  RestoreDC(ADC, SavedDC);
end;

class procedure TWin32WSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  // other methods also use ShowHide, can't move code
  TWin32WidgetSet(WidgetSet).ShowHide(AWinControl);
end;

{ TWin32WSDragImageList }

class function TWin32WSDragImageList.BeginDrag(
  const ADragImageList: TDragImageList; Window: HWND; AIndex, X, Y: Integer): Boolean;
begin
  // No check to Handle should be done, because if there is no handle (no needed)
  // we must create it here. This is normal for imagelist (we can never need handle)
  Result := ImageList_BeginDrag(ADragImageList.Reference.Handle, AIndex, X, Y);
end;

class function TWin32WSDragImageList.DragMove(const ADragImageList: TDragImageList;
  X, Y: Integer): Boolean;
begin
  Result := ImageList_DragMove(X, Y);
end;

class procedure TWin32WSDragImageList.EndDrag(const ADragImageList: TDragImageList);
begin
  ImageList_EndDrag;
end;

class function TWin32WSDragImageList.HideDragImage(const ADragImageList: TDragImageList;
  ALockedWindow: HWND; DoUnLock: Boolean): Boolean;
begin
  if DoUnLock then
    Result := ImageList_DragLeave(ALockedWindow)
  else
    Result := ImageList_DragShowNolock(False);
end;

class function TWin32WSDragImageList.ShowDragImage(const ADragImageList: TDragImageList;
  ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean;
begin
  if DoLock then
    Result := ImageList_DragEnter(ALockedWindow, X, Y)
  else
    Result := ImageList_DragShowNolock(True);
end;

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
  RegisterWSComponent(TDragImageList, TWin32WSDragImageList); // Uncomment with native image list
  RegisterWSComponent(TControl, TWin32WSControl);
  RegisterWSComponent(TWinControl, TWin32WSWinControl);
//  RegisterWSComponent(TGraphicControl, TWin32WSGraphicControl);
//  RegisterWSComponent(TCustomControl, TWin32WSCustomControl);
//  RegisterWSComponent(TImageList, TWin32WSImageList);
////////////////////////////////////////////////////
end.
