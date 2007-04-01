{%MainUnit ../lclintf.pp}
{ $Id: lclintf.inc 8032 2005-11-02 12:44:29Z vincents $
******************************************************************************
  All interface communication related stuff goes here.
  This file is used by LCLIntf.pas
  If a procedure is platform dependent then it should call:
    WidgetSet.MyDependentProc

  If a procedure insn't platform dependent, it is no part of InterfaseBase has
  to be implementerd here

  !! Keep this alphabetical !!

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
 ******************************************************************************

******************************************************************************
  These functions redirect to the platform specific interface object.

  Note:
    the section for not referring WidgetSet is at the end
 ******************************************************************************}
//##apiwiz##sps##   // Do not remove

function TWinCEWidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword;
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

procedure TWinCEWidgetSet.RemoveEventHandler(var AHandler: PEventHandler);
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

function TWinCEWidgetSet.AddPipeEventHandler(AHandle: THandle;
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

procedure TWinCEWidgetSet.RemovePipeEventHandler(var AHandler: PPipeEventHandler);
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

function TWinCEWidgetSet.AddProcessEventHandler(AHandle: THandle;
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

procedure TWinCEWidgetSet.HandleProcessEvent(AData: PtrInt; AFlags: dword);
var
  lProcessEvent: PProcessEvent absolute AData;
  exitcode: dword;
begin
  if not Windows.GetExitCodeProcess(lProcessEvent^.Handle, exitcode) then
    exitcode := 0;
  lProcessEvent^.OnEvent(lProcessEvent^.UserData, cerExit, exitcode);
end;

procedure TWinCEWidgetSet.RemoveProcessEventHandler(var AHandler: PProcessEventHandler);
var
  lProcessEvent: PProcessEvent absolute AHandler;
begin
  if AHandler = nil then exit;
  RemoveEventHandler(lProcessEvent^.Handler);
  Dispose(lProcessEvent);
  AHandler := nil;
end;

{------------------------------------------------------------------------------
  Function:
  Params:

  Returns:

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
begin
  Result := 0;
  if ACursor < crLow then Exit;
  if ACursor > crHigh then Exit;

  case ACursor of
    crSqlWait..crDrag,
    crHandPoint: 
      begin
        // will be created later by CreateCursor
      end;
  else
    Result := Windows.LoadCursor(0, LclCursorToWin32CursorMap[ACursor]);
  end;
end;

procedure TWinCEWidgetSet.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
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
  Windows.DrawFrameControl(TCanvas(Canvas).Handle, @drawRect,
	DFC_SCROLL, ArrowTypeToState[TArrow(Arrow).ArrowType]);
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TWinCEWidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
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
function TWinCEWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
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
    {if SizeConstraints.Control is TCustomCalendar then
    begin
      FixedHeight := true;
      FixedWidth := true;
    end;}
    
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
function TWinCEWidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := -1;
  if ListBox is TCustomListBox then begin
    Result := Windows.SendMessage(TCustomListBox(ListBox).Handle, LB_ITEMFROMPOINT, 0, MakeLParam(0,y));
    if hi(Result)=0 then
      Result := lo(Result)
    else Result := -1;
  end;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line
