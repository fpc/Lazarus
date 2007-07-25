{%MainUnit win32int.pp}
{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove

function TWin32WidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
var
  listlen: dword;
  lListIndex: pdword;
begin
  listlen := Length(FWaitHandles);
  if FWaitHandleCount = listlen then
  begin
    inc(listlen, 16);
    SetLength(FWaitHandles, listlen);
    SetLength(FWaitHandlers, listlen);
  end;
  New(lListIndex);
  FWaitHandles[FWaitHandleCount] := AHandle;
  FWaitHandlers[FWaitHandleCount].ListIndex := lListIndex;
  FWaitHandlers[FWaitHandleCount].UserData := AData;
  FWaitHandlers[FWaitHandleCount].OnEvent := AEventHandler;
  lListIndex^ := FWaitHandleCount;
  Inc(FWaitHandleCount);
{$ifdef DEBUG_ASYNCEVENTS}  
  DebugLn('Waiting for handle: ', IntToHex(AHandle, 8));
{$endif}
  Result := lListIndex;
end;

procedure TWin32WidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
var
  lListIndex: pdword absolute AHandler;
  I: dword;
begin
  if AHandler = nil then exit;
{$ifdef DEBUG_ASYNCEVENTS}
  DebugLn('Removing handle: ', IntToHex(FWaitHandles[lListIndex^], 8));
  if Length(FWaitHandles) > 0 then
    DebugLn(' WaitHandleCount=', IntToStr(FWaitHandleCount), ', WaitHandle[0]=', IntToHex(FWaitHandles[0], 8));
{$endif}    
  // swap with last one
  if FWaitHandleCount >= 2 then
  begin
    I := lListIndex^;
    FWaitHandles[I] := FWaitHandles[FWaitHandleCount-1];
    FWaitHandlers[I] := FWaitHandlers[FWaitHandleCount-1];
    FWaitHandlers[I].ListIndex^ := I;
  end;
  Dec(FWaitHandleCount);
  Dispose(lListIndex);
  AHandler := nil;
end;

function TWin32WidgetSet.AddPipeEventHandler(AHandle: THandle; 
  AEventHandler: TPipeEvent; AData: PtrInt): PPipeEventHandler;
var
  lHandler: PPipeEventInfo;
begin
  if AEventHandler = nil then exit;
  New(lHandler);
  lHandler^.Handle := AHandle;
  lHandler^.UserData := AData;
  lHandler^.OnEvent := AEventHandler;
  lHandler^.Prev := nil;
  lHandler^.Next := FWaitPipeHandlers;
  if FWaitPipeHandlers <> nil then
    FWaitPipeHandlers^.Prev := lHandler;
  FWaitPipeHandlers := lHandler;
  Result := lHandler;
end;

procedure TWin32WidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
var
  lHandler: PPipeEventInfo absolute AHandler;
begin
  if AHandler = nil then exit;
  if lHandler^.Prev <> nil then
    lHandler^.Prev^.Next := lHandler^.Next
  else
    FWaitPipeHandlers := lHandler^.Next;
  if lHandler^.Next <> nil then
    lHandler^.Next^.Prev := lHandler^.Prev;
  Dispose(lHandler);
  AHandler := nil;
end;

function TWin32WidgetSet.AddProcessEventHandler(AHandle: THandle;
  AEventHandler: TChildExitEvent; AData: PtrInt): PProcessEventHandler;
var
  lProcessEvent: PProcessEvent;
begin
  if AEventHandler = nil then exit;
  New(lProcessEvent);
  lProcessEvent^.Handle := AHandle;
  lProcessEvent^.UserData := AData;
  lProcessEvent^.OnEvent := AEventHandler;
  lProcessEvent^.Handler := AddEventHandler(AHandle, 0, 
    @HandleProcessEvent, PtrInt(lProcessEvent));
  Result := lProcessEvent;
end;

{------------------------------------------------------------------------------
  Method:  ExtUTF8Out

  As ExtTextOut except that Str is treated as UTF8
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint; Rect: PRect;
  Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result := ExtTextOut(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

{------------------------------------------------------------------------------
  function FontCanUTF8(Font: HFont): boolean;

  True if font recognizes Unicode UTF8 encoding.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  {$ifdef WindowsUnicodeSupport}
    Result := True;
  {$else}
    Result := False;
  {$endif}
end;

procedure TWin32WidgetSet.HandleProcessEvent(AData: PtrInt; AFlags: dword);
var
  lProcessEvent: PProcessEvent absolute AData;
  exitcode: dword;
begin
  if not Windows.GetExitCodeProcess(lProcessEvent^.Handle, exitcode) then
    exitcode := 0;
  lProcessEvent^.OnEvent(lProcessEvent^.UserData, cerExit, exitcode);
end;

procedure TWin32WidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
var
  lProcessEvent: PProcessEvent absolute AHandler;
begin
  if AHandler = nil then exit;
  RemoveEventHandler(lProcessEvent^.Handler);
  Dispose(lProcessEvent);
  AHandler := nil;
end;

function TWin32WidgetSet.AppHandle: THandle;
begin
  Result:= FAppHandle;
end;

{------------------------------------------------------------------------------
  Function:
  Params:

  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  case ACursor of
    crSqlWait..crDrag,
    crHandPoint, crNone: begin
      // TODO: load custom cursors here not in the LCL
    end;
  else
    Result := Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]);
  end;
end;

{------------------------------------------------------------------------------
  Method: CallbackAllocateHWnd
  Params:   None
  Returns:  Nothing

  Callback for the AllocateHWnd function
 ------------------------------------------------------------------------------}
procedure CallbackAllocateHWnd(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam); stdcall;
var
  Msg: TLMessage;
  PMethod: ^TLCLWndMethod;
begin
  FillChar(Msg, SizeOf(Msg), #0);
  
  Msg.msg := uMsg;
  Msg.wParam := wParam;
  Msg.lParam := lParam;

  {------------------------------------------------------------------------------
    Here we get the callback WndMethod associated with this window
   ------------------------------------------------------------------------------}
  PMethod := Pointer(Widgetset.GetWindowLong(ahwnd, GWL_USERDATA));

  if Assigned(PMethod) then PMethod^(Msg);
   
  Windows.DefWindowProc(ahwnd, uMsg, wParam, lParam);
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.AllocateHWnd
  Params:   Method  - The callback method for the window. Can be nil
  Returns:  A window handle

  Allocates a non-visible window that can be utilized to receive and send message
  
  On Windows, you must call Windows.DefWindowProc(MyHandle, Msg.msg, Msg.wParam, msg.lParam);
  in your callback function, if you provide one at all, of course.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.AllocateHWnd(Method: TLCLWndMethod): HWND;
var
  PMethod: ^TLCLWndMethod;
begin
  Result := Windows.CreateWindow(@ClsName[0],
   '', WS_OVERLAPPED, 0, 0, 0, 0, 0, 0, MainInstance, nil);

  {------------------------------------------------------------------------------
    SetWindowLong has only space for 1 pointer on each slot, but a method is
   referenced as a structure with 2 pointers, so here we allocate memory for
   the structure before it can be used to transport data between the callback
   and this function
   ------------------------------------------------------------------------------}
  if Assigned(Method) then
  begin
    Getmem(PMethod, SizeOf(TMethod));
    PMethod^ := Method;

    Self.SetWindowLong(Result, GWL_USERDATA, PtrInt(PMethod));
  end;
  
  Self.SetWindowLong(Result, GWL_WNDPROC, PtrInt(@CallbackAllocateHWnd))
end;

{------------------------------------------------------------------------------
  Method: TWin32WidgetSet.DeallocateHWnd
  Params:   Wnd   - A Window handle, that was created with AllocateHWnd
  Returns:  Nothing
 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DeallocateHWnd(Wnd: HWND);
var
  PMethod: ^TLCLWndMethod;
begin
  PMethod := Pointer(Self.GetWindowLong(Wnd, GWL_USERDATA));

  if Wnd <> 0 then Windows.DestroyWindow(Wnd);

  {------------------------------------------------------------------------------
    This must be done after DestroyWindow, otherwise a Access Violation will
   happen when WM_CLOSE message is sent to the callback

    This memory is for the TMethod structure allocated on AllocateHWnd
   ------------------------------------------------------------------------------}
  if Assigned(PMethod) then Freemem(PMethod);
end;

{------------------------------------------------------------------------------
  Procedure:
  Params:

  Returns:

 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
const
    { up, down, left, right }
  ArrowTypeToState: array[TArrowType] of dword = (DFCS_SCROLLUP, DFCS_SCROLLDOWN,
    DFCS_SCROLLLEFT, DFCS_SCROLLRIGHT);
var
  drawRect: Windows.RECT;
  canvasHandle: HDC;
begin
  drawRect := TControl(Arrow).ClientRect;
  canvasHandle := TCanvas(Canvas).Handle;
  Windows.FillRect(canvasHandle, drawRect, GetSysColorBrush(COLOR_BTNFACE));
  dec(drawRect.Left, 2);
  dec(drawRect.Top, 2);
  inc(drawRect.Right, 2);
  inc(drawRect.Bottom, 2);
  Windows.DrawFrameControl(TCanvas(Canvas).Handle, drawRect,
	DFC_SCROLL, ArrowTypeToState[TArrow(Arrow).ArrowType]);
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  //TODO: Implement
  Result := '';
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints;
  SizeRect: TRect;
  Height, Width: Integer;
  FixedHeight, FixedWidth: boolean;
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    SizeConstraints:=TSizeConstraints(Constraints);

    if (SizeConstraints.Control=nil) then exit;

    FixedHeight := false;
    FixedWidth := false;
    if SizeConstraints.Control is TCustomCalendar then
    begin
      FixedHeight := true;
      FixedWidth := true;
    end;
    
    if (FixedHeight or FixedWidth)
      and TWinControl(SizeConstraints.Control).HandleAllocated then 
    begin
      Windows.GetWindowRect(TWinControl(SizeConstraints.Control).Handle, @SizeRect);
      if FixedHeight then
        Height := SizeRect.Bottom - SizeRect.Top
      else
        Height := 0;
      if FixedWidth then
        Width := SizeRect.Right - SizeRect.Left
      else
        Width := 0;
      SizeConstraints.SetInterfaceConstraints(Width, Height, Width, Height);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Function: GetListBoxIndexAtY
  Params: ListBox:
          y:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := -1;
  if ListBox is TCustomListBox then begin
    Result := Windows.SendMessage(TCustomListBox(ListBox).Handle, LB_ITEMFROMPOINT, 0, MakeLParam(0,y));
    if hi(Result)=0 then
      Result := lo(Result)
    else Result := -1;
  end;
end;

function TWin32WidgetSet.GetListBoxItemRect(ListBox: TComponent;
  Index: integer; var ARect: TRect): boolean;
begin
  Result := false;
  if ListBox is TCustomListBox then
    Result := Windows.SendMessage(TCustomListBox(ListBox).Handle,
                LB_GETITEMRECT, Index, LPARAM(@ARect)) <> LB_ERR;
end;

function TWin32WidgetSet.LoadStockPixmap(StockID: longint; var Mask: HBitmap): HBitmap;
{$ifdef NoMaskProblem}
var
  AIconHandle: HIcon;
  AIconInfo: TIconInfo;
{$endif}
begin
{$ifdef NoMaskProblem}
  Result := 0;
  case StockID of
    idDialogWarning: AIconHandle := LoadIcon(0, IDI_EXCLAMATION);
    idDialogError  : AIconHandle := LoadIcon(0, IDI_HAND);
    idDialogInfo   : AIconHandle := LoadIcon(0, IDI_ASTERISK);
    idDialogConfirm: AIconHandle := LoadIcon(0, IDI_QUESTION);
  else
    AIconHandle := 0;
  end;
  if AIconHandle <> 0 then
  begin
    if GetIconInfo(AIconHandle, @AIconInfo) then
    begin
      Result := AIconInfo.hbmColor;
      Mask := AIconInfo.hbmMask;
    end
  end;
  if Result = 0 then
{$endif}
    Result := inherited LoadStockPixmap(StockID, Mask);
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
