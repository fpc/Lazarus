{ $Id: wscontrols.pp 7963 2005-10-12 22:22:02Z marc $}
{
 *****************************************************************************
 *                               WSControls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WinCEWSControls;

{$mode objfpc}{$H+}

interface

uses
  // Libs
  Windows,
  commctrl,
  // RTL, LCL
  Classes, LCLProc, LazUTF8, LCLType, Controls, Forms, Graphics,
  // Widgetset
  winceproc, WinCEInt, Sysutils, WSProc,
  WSLCLClasses, WSImgList, WSControls,
  InterfaceBase;

type
  { TWinCEWSDragImageListResolution }

  TWinCEWSDragImageListResolution = class(TWSDragImageListResolution)
  published
    class function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND;
      AIndex, X, Y: Integer): Boolean; override;
    class function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; override;
    class procedure EndDrag(const ADragImageList: TDragImageListResolution); override;
    class function HideDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; override;
    class function ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; override;
  end;

  { TWinCEWSControl }

  TWinCEWSControl = class(TWSControl)
  private
  protected
  public
  end;

  { TWinCEWSWinControlPrivate }

  TWinCEWSWinControlPrivate = class(TWSPrivate)
  private
  protected
  public
  end;
  TWinCEWSWinControlPrivateClass = class of TWinCEWSWinControlPrivate;


  { TWinCEWSWinControl }

  TWinCEWSWinControl = class(TWSWinControl)
  published
    class procedure AddControl(const AControl: TControl); override;

    class function  GetText(const AWinControl: TWinControl; var AText: String): Boolean; override;
    class procedure SetBounds(const AWinControl: TWinControl; const ALeft, ATop, AWidth, AHeight: Integer); override;
    class procedure SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle); override;
    class procedure SetChildZPosition(const AWinControl, AChild: TWinControl;
                                      const AOldPos, ANewPos: Integer;
                                      const AChildren: TFPList); override;
    class procedure SetColor(const AWinControl: TWinControl); override;
    class procedure SetFont(const AWinControl: TWinControl; const AFont: TFont); override;
    class procedure SetText(const AWinControl: TWinControl; const AText: string); override;

    class procedure ConstraintsChange(const AWinControl: TWinControl); override;
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class procedure Invalidate(const AWinControl: TWinControl); override;
    class procedure ShowHide(const AWinControl: TWinControl); override;
    class procedure ScrollBy(const AWinControl: TWinControl; DeltaX, DeltaY: integer); override;
  end;

  { TWinCEWSGraphicControl }

  TWinCEWSGraphicControl = class(TWSGraphicControl)
  private
  protected
  public
  end;

  { TWinCEWSCustomControl }

  TWinCEWSCustomControl = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWinCEWSImageList }

  TWinCEWSImageList = class(TWSImageList)
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
    WindowTitle, StrCaption: String;
    pClassName: PWideChar;
  end;

 // TODO: better names?

procedure PrepareCreateWindow(const AWinControl: TWinControl;
  const CreateParams: TCreateParams; out Params: TCreateWindowExParams);
procedure FinishCreateWindow(const AWinControl: TWinControl; var Params: TCreateWindowExParams;
  const AlternateCreateWindow: boolean);
procedure WindowCreateInitBuddy(const AWinControl: TWinControl;
  var Params: TCreateWindowExParams);


implementation

{ Global helper routines }

procedure PrepareCreateWindow(const AWinControl: TWinControl;
  const CreateParams: TCreateParams; out Params: TCreateWindowExParams);
begin
  Fillchar(Params,Sizeof(Params),0);
  with Params do
  begin
    Window := HWND(nil);
    Buddy := HWND(nil);
    WindowTitle := '';
    SubClassWndProc := @WindowProc;

    Flags := CreateParams.Style;
    FlagsEx := CreateParams.ExStyle;

    // Never set the parent of a window to AppHandle,
    // otherwise wince will really try to make it a child
    Parent := CreateParams.WndParent;

    StrCaption := CreateParams.Caption;

    Left := CreateParams.X;
    Top := CreateParams.Y;
    Width := CreateParams.Width;
    Height := CreateParams.Height;

    LCLBoundsToWin32Bounds(AWinControl, Left, Top);
    if AWinControl is TCustomControl then
      if TCustomControl(AWinControl).BorderStyle = bsSingle then
        FlagsEx := FlagsEx or WS_EX_CLIENTEDGE;

    {$IFDEF VerboseSizeMsg}
    Debugln('PrepareCreateWindow ',AWinControl.Name,':',AWinControl.ClassName,
      ' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height));
    {$ENDIF}
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

      Window := CreateWindowExW(
       FlagsEx,            // Extra Flags
       pClassName,         // Name of the registered class
       PWideChar(UTF8Decode(WindowTitle)),// Title of the window
       Flags,              // Style of the window
       Left,               // x-position (at beginning)
       Top,                // y-position (at beginning)
       Width,              // window width
       Height,             // window height
       Parent,             // handle to parent or owner window
       MenuHandle,         // handle to menu
       System.HInstance,   // handle to application instance
       nil);               // pointer to window-creation data

      if Window = 0 then
      begin
        raise exception.create('failed to create win32 control, error: '+IntToStr(GetLastError())
         + ' WindowTitle: ' + WindowTitle);
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
        WindowInfo^.DefWndProc := Windows.WNDPROC(Windows.SetWindowLongW(
          Window, GWL_WNDPROC, LongInt(SubClassWndProc)));
      {$ifdef MSG_DEBUG}
        DebugLn('Trace:FinishCreateWindow - ', AWinControl.Name,
          ' SubClassWndProc: ', IntToHex(PtrInt(SubClassWndProc), 8),
          ' DefWndProc: ', IntToHex(PtrInt(WindowInfo^.DefWndProc), 8));
      {$endif}
      if AWinControl.Font.IsDefault then
        lhFont := WinCEWidgetset.MessageFont
      else
        lhFont := AWinControl.Font.Reference.Handle;
      Windows.SendMessage(Window, WM_SETFONT, lhFont, 0)
    end;
  end;
end;

procedure WindowCreateInitBuddy(const AWinControl: TWinControl;
  var Params: TCreateWindowExParams);
var
  lhFont: HFONT;
begin
  if Params.Buddy <> HWND(Nil) then
  begin
    Params.BuddyWindowInfo := AllocWindowInfo(Params.Buddy);
    Params.BuddyWindowInfo^.AWinControl := AWinControl;
    Params.BuddyWindowInfo^.DefWndProc := Windows.WNDPROC(Windows.SetWindowLong(
      Params.Buddy, GWL_WNDPROC, LongInt(Params.SubClassWndProc)));
    if AWinControl.Font.IsDefault then
      lhFont := WinCEWidgetset.MessageFont
    else
      lhFont := AWinControl.Font.Reference.Handle;
    Windows.SendMessage(Params.Buddy, WM_SETFONT, lhFont, 0);
  end
  else
    Params.BuddyWindowInfo := nil;
end;


{ TWinCEWSWinControl }

class function TWinCEWSWinControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  {$ifdef VerboseWinCE}
  DebugLn(' TWinCEWSWinControl.CreateHandle ');
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName;
    WindowTitle := StrCaption;
    SubClassWndProc := nil;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

class procedure TWinCEWSWinControl.AddControl(const AControl: TControl);
var
  ParentPanelHandle, ParentHandle, ChildHandle: HWND;
begin
  {$ifdef OldToolbar}
  if (AControl.Parent is TToolbar) then
    exit;
  {$endif}

  with TWinControl(AControl) do
  begin
    //DebugLn(Format('Trace:[TWinCEWSWinControl.AddControl] %S --> Calling Add Child: %S', [Parent.ClassName, ClassName]));
    ParentHandle := Parent.Handle;
    ChildHandle := Handle;
  end;

  //DebugLn('Trace:AddControl - Parent Window Handle is $' + IntToHex(LongInt(ParentHandle), 8));
  //DebugLn('Trace:AddControl - Child Window Handle is $' + IntToHex(LongInt(ChildHandle), 8));
  // handle groupbox exception
  ParentPanelHandle := GetWindowInfo(ChildHandle)^.ParentPanel;
  if ParentPanelHandle <> 0 then
    ChildHandle := ParentPanelHandle;
  SetParent(ChildHandle, ParentHandle);
end;

class function  TWinCEWSWinControl.GetText(const AWinControl: TWinControl; var AText: String): Boolean;
begin
  AText := '';
  Result := false;
end;

class procedure TWinCEWSWinControl.SetBorderStyle(const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  RecreateWnd(AWinControl);
end;

class procedure TWinCEWSWinControl.SetChildZPosition(
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
    SWP_NOSIZE or SWP_NOSENDCHANGING);
end;

{------------------------------------------------------------------------------
  Method:  SetBounds
  Params:  AWinControl                  - the object which invoked this function
           ALeft, ATop, AWidth, AHeight - new dimensions for the control
  Pre:     AWinControl.HandleAllocated
  Returns: Nothing

  Resize a window
 ------------------------------------------------------------------------------}
class procedure TWinCEWSWinControl.SetBounds(const AWinControl: TWinControl;
  const ALeft, ATop, AWidth, AHeight: Integer);
var
  IntfLeft, IntfTop, IntfWidth, IntfHeight: integer;
  suppressMove: boolean;
begin
  IntfLeft := ALeft; IntfTop := ATop;
  IntfWidth := AWidth; IntfHeight := AHeight;
  LCLBoundsToWin32Bounds(AWinControl, IntfLeft, IntfTop);
  {$IFDEF VerboseSizeMsg}
  Debugln('TWinCEWSWinControl.ResizeWindow A ',AWinControl.Name,':',AWinControl.ClassName,
    ' LCL=',dbgs(ALeft),',',dbgs(ATop),',',dbgs(AWidth)+','+dbgs(AHeight),
    ' Win32=',dbgs(IntfLeft)+','+dbgs(IntfTop)+','+dbgs(IntfWidth),',',dbgs(IntfHeight),
    '');
  {$ENDIF}
  suppressMove := false;
  AdaptBounds(AWinControl, IntfLeft, IntfTop, IntfWidth, IntfHeight, suppressMove);
  // Some controls, like spins, may set suppressMove in AdaptBounds
  if not suppressMove then
    MoveWindow(AWinControl.Handle, IntfLeft, IntfTop, IntfWidth, IntfHeight, true);

  LCLControlSizeNeedsUpdate(AWinControl, false);
end;

class procedure TWinCEWSWinControl.SetColor(const AWinControl: TWinControl);
begin
  // TODO: to be implemented, had no implementation in LM_SETCOLOR message
end;

class procedure TWinCEWSWinControl.SetFont(const AWinControl: TWinControl; const AFont: TFont);
begin
  Windows.SendMessage(AWinControl.Handle, WM_SETFONT, Windows.WParam(AFont.Reference.Handle), 1);
end;

class procedure TWinCEWSWinControl.SetText(const AWinControl: TWinControl; const AText: string);
begin
  if not WSCheckHandleAllocated(AWincontrol, 'SetText') then Exit;

  Windows.SetWindowTextW(AWinControl.Handle, PWideChar(UTF8Decode(AText)));
end;

class procedure TWinCEWSWinControl.ConstraintsChange(const AWinControl: TWinControl);
begin
  // TODO: implement me!
end;

class procedure TWinCEWSWinControl.DestroyHandle(const AWinControl: TWinControl);
var
  Handle: HWND;
begin
  Handle := AWinControl.Handle;
  DestroyWindow(Handle);
end;

class procedure TWinCEWSWinControl.Invalidate(const AWinControl: TWinControl);
begin
  // lpRect = nil updates entire client area of window
  InvalidateRect(AWinControl.Handle, nil, true);
end;

class procedure TWinCEWSWinControl.ShowHide(const AWinControl: TWinControl);
begin
  // other methods also use ShowHide, can't move code
  TWinCEWidgetSet(WidgetSet).ShowHide(AWinControl);
end;

class procedure TWinCEWSWinControl.ScrollBy(const AWinControl: TWinControl;
  DeltaX, DeltaY: integer);
begin
  if AWinControl.HandleAllocated then
    ScrollWindowEx(AWinControl.Handle, DeltaX, DeltaY, nil, nil, 0, nil,
      SW_INVALIDATE or SW_ERASE or SW_SCROLLCHILDREN);
end;

{ TWinCEWSDragImageListResolution }

class function TWinCEWSDragImageListResolution.BeginDrag(
  const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean;
begin
  // No check to Handle should be done, because if there is no handle (no needed)
  // we must create it here. This is normal for imagelist (we can never need handle)
  Result := ImageList_BeginDrag(ADragImageList.Reference.Handle, AIndex, X, Y);
end;

class function TWinCEWSDragImageListResolution.DragMove(const ADragImageList: TDragImageListResolution;
  X, Y: Integer): Boolean;
begin
  Result := ImageList_DragMove(X, Y);
end;

class procedure TWinCEWSDragImageListResolution.EndDrag(const ADragImageList: TDragImageListResolution);
begin
  ImageList_EndDrag;
end;

class function TWinCEWSDragImageListResolution.HideDragImage(const ADragImageList: TDragImageListResolution;
  ALockedWindow: HWND; DoUnLock: Boolean): Boolean;
begin
  if DoUnLock then
    Result := ImageList_DragLeave(ALockedWindow)
  else
    Result := ImageList_DragShowNolock(False);
end;

class function TWinCEWSDragImageListResolution.ShowDragImage(const ADragImageList: TDragImageListResolution;
  ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean;
begin
  if DoLock then
    Result := ImageList_DragEnter(ALockedWindow, X, Y)
  else
    Result := ImageList_DragShowNolock(True);
end;

end.
