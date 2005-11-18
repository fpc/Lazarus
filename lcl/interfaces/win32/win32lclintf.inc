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
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

procedure TWin32WidgetSet.AddEventHandler(AHandle: THandle; AFlags: dword; 
  AEventHandler: TWaitHandleEvent; AData: PtrInt);
var
  listlen: dword;
begin
  listlen := Length(FWaitHandles);
  if FWaitHandleCount = listlen then
  begin
    inc(listlen, 16);
    SetLength(FWaitHandles, listlen);
    SetLength(FWaitHandlers, listlen);
  end;
  FWaitHandles[FWaitHandleCount] := AHandle;
  FWaitHandlers[FWaitHandleCount].UserData := AData;
  FWaitHandlers[FWaitHandleCount].OnEvent := AEventHandler;
  Inc(FWaitHandleCount);
end;

procedure TWin32WidgetSet.RemoveEventHandler(AHandle: THandle);
var
  I: integer;
begin
  for I := 0 to FWaitHandleCount - 1 do
    if FWaitHandles[I] = AHandle then
    begin
      // swap with last one
      if FWaitHandleCount >= 2 then
      begin
        FWaitHandles[I] := FWaitHandles[FWaitHandleCount-1];
        FWaitHandlers[I] := FWaitHandlers[FWaitHandleCount-1];
      end;
      Dec(FWaitHandleCount);
      exit;
    end;
end;

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
    if SizeConstraints.Control is TCustomComboBox then
      FixedHeight := true;
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

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

