(******************************************************************************
                                     TCANVAS
 ******************************************************************************)

const
  csAllValid = [csHandleValid..csBrushValid];

{-----------------------------------------------}
{--  TCanvas.BrushCopy --}
{-----------------------------------------------}
Procedure TCanvas.BrushCopy(Dest : TRect; InternalImages: TBitmap; Src : TRect;
  TransparentColor :TColor);
Begin
//TODO:TCANVAS.BRUSHCOPY
end;

{-----------------------------------------------}
{--  TCanvas.Draw --}
{-----------------------------------------------}
Procedure TCanvas.Draw(X,Y : Integer; Graphic : TGraphic);
begin

end;

{-----------------------------------------------}
{--  TCanvas.GetCanvasClipRect --}
{-----------------------------------------------}
function TCanvas.GetCanvasClipRect: TRect;
begin
// quick cheat to get code working
     Result.Left := 0;
     Result.Top := 0;
     Result.Right := 1000;
     Result.Bottom := 1000;
end;


{-----------------------------------------------}
{--  TCanvas.CopyRect --}
{-----------------------------------------------}
Procedure TCanvas.CopyRect(const Dest : TRect; Canvas : TCanvas; const Source : TRect);
var
  SH, SW, DH, DW: Integer;
Begin
//this SHOULD stretch the image to the new canvas, but it doesn't yet.....
  Assert(False, Format('Trace:==> [TCanvas.CopyRect] ', []));
  if Canvas <> nil
  then begin
    Canvas.RequiredState([csHandleValid, csBrushValid]);
    RequiredState([csHandleValid, csFontValid, csBrushValid]);

    SH := Source.Bottom - Source.Top;
    SW := Source.Right - Source.Left;
    DH := Dest.Bottom - Dest.Top;
    DW := Dest.Right - Dest.Left;
    StretchBlt(FHandle, Dest.Left, Dest.Top, DW, DH,
      Canvas.FHandle, Source.Left, Source.Top, SW, SH, CopyMode);
  end;

  Assert(False, Format('Trace:<== [TCanvas.CopyRect] ', []));
end;
{-----------------------------------------------}
{--  TCanvas.GetPixel --}
{-----------------------------------------------}
Function TCanvas.GetPixel(X,Y : Integer) : TColor;
var
Msg : TLMSetGetPixel;
{TLMSetGetPixel = record
  X,Y : Integer;
  PixColor : TColor;
end;
}
Begin
  msg.X := x;
  msg.Y := Y;
  CNSendMessage(LM_GetPixel, Self, @msg);
  Result := msg.PixColor;
end;

{-----------------------------------------------}
{--  TCanvas.SetPixel --}
{-----------------------------------------------}
Procedure TCanvas.SetPixel(X,Y: Integer; Value : TColor);
var
  Msg : TLMSetGetPixel;
Begin
  Msg.X := X;
  msg.Y := Y;
  MSg.PixColor := Value;
  CNSendMessage(LM_SetPixel, Self, @msg);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateBrush
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateBrush;
var OldHandle: HBRUSH;
begin
//writeln('[TCanvas.CreateBrush] ',Classname,'  Self=',HexStr(Cardinal(Pointer(Self)),8)
// ,'  Brush=',HexStr(Cardinal(Pointer(Brush)),8));
  OldHandle:=SelectObject(FHandle, Brush.Handle);
  if (OldHandle<>Brush.Handle) and (FSavedBrushHandle=0) then
    FSavedBrushHandle:=OldHandle;
  Include(FState, csBrushValid);
//  SetBkColor(FHandle, not ColorToRGB(Brush.Color));
//  SetBkMode(FHandle, TRANSPARENT);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreatePen
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreatePen;
var OldHandle: HPEN;
begin
//writeln('[TCanvas.CreatePen] ',Classname,'  Self=',HexStr(Cardinal(Pointer(Self)),8)
// ,'  Pen=',HexStr(Cardinal(Pointer(Pen)),8));
  OldHandle:=SelectObject(FHandle, Pen.Handle);
  if (OldHandle<>Pen.Handle) and (FSavedPenHandle=0) then
    FSavedPenHandle:=OldHandle;
  Include(FState, csPenValid);
//  SetROP2(FHandle, PenModes[Pen.Mode]);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.CreateFont
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.CreateFont;
var OldHandle: HPEN;
begin
  OldHandle:=SelectObject(FHandle, Font.Handle);
  if (OldHandle<>Font.Handle) and (FSavedFontHandle=0) then
    FSavedFontHandle:=OldHandle;
  Include(FState, csFontValid);
  SetTextColor(FHandle, ColorToRGB(Font.Color));
end;

{------------------------------------------------------------------------------
  Function: TCanvas.GetPenPos
  Params:   None
  Returns:  PenPos

 ------------------------------------------------------------------------------}
function TCanvas.GetPenPos: TPoint;
begin
  Result := FPenPos;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.SetAutoReDraw
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetAutoReDraw(Value : Boolean);
begin
  FAutoRedraw := Value;
  If FAutoReDraw then
    CNSendMessage(LM_ReDraw, Self, nil);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetPenPos
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetPenPos(Value : TPoint);
begin
  MoveTo(Value.X, Value.Y);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetBrush
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetBrush(Value : TBrush);
begin
  FBrush.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetFont
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetFont(Value : TFont);
begin
  FFont.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.SetPen
  Params:   Value
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetPen(Value : TPen);
begin
  FPen.Assign(Value);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Arc
  Params:   x,y,width,height,angle1,angle2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Arc(x,y,width,height,angle1,angle2 : Integer);
begin

end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FillRect
  Params:   Rect
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.FillRect(const Rect : TRect);
begin
writeln('[TCanvas.FillRect] 1');
  RequiredState([csHandleValid, csBrushValid]);
writeln('[TCanvas.FillRect] 2');
  LCLLinux.FillRect(FHandle, Rect, Brush.Handle);
writeln('[TCanvas.FillRect] 3');
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.Rectangle
  Params:   X1,Y1,X2,Y2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Rectangle(X1,Y1,X2,Y2 : Integer);
begin
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  LCLLinux.Rectangle(FHandle, X1, Y1, X2, Y2);
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.TextOut
  Params:   X,Y,Text
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.TextOut(X,Y: Integer; const Text: String);
var
  pStr: PChar;
begin
  RequiredState([csHandleValid, csFontValid, csBrushValid]);

  pStr := StrAlloc(Length(Text)+1);
  try
    StrPcopy(pStr, Text);
    ExtTextOut(FHandle, X, Y, 0 { <-- TODO: FTextFlags}, nil, pStr, Length(Text), nil);
    MoveTo(X + TextWidth(Text), Y);
  finally
    StrDispose(PStr);
  end;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.Polygon
  Params:   Points
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Polygon(const Points: array of TPoint);
begin

end;

{------------------------------------------------------------------------------
  Method:   TCanvas.MoveTo
  Params:   X1,Y1
  Returns:  Nothing

 ------------------------------------------------------------------------------}
Procedure TCanvas.MoveTo(X1, Y1 : Integer);
begin
  RequiredState([csHandleValid]);
  if LCLLinux.MoveToEx(FHandle, X1, Y1, nil) then FPenPos:= Point(X1, Y1);
End;

{------------------------------------------------------------------------------
  Method:   TCanvas.LineTo
  Params:   X1,Y1
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.LineTo(X1, Y1 : Integer);
begin
  RequiredState([csHandleValid, csPenValid]);
  if LCLLinux.LineTo(FHandle, X1, Y1) then FPenPos:= Point(X1, Y1);
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.Line
  Params:   X1,Y1,X2,Y2
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.Line(X1,Y1,X2,Y2 : Integer);
begin
  //?? Additional function ??
  MoveTo(X1, Y1);
  LineTo(X2, Y2);
end;


{------------------------------------------------------------------------------
  Function: TCanvas.GetColor
  Params:   None
  Returns:

 ------------------------------------------------------------------------------}
function TCanvas.GetColor:TColor;
begin
  Result:=Brush.Color;
end;


{------------------------------------------------------------------------------
  Method:   TCanvas.SetColor
  Params:   None
  Returns:  Nothing

 ------------------------------------------------------------------------------}
procedure TCanvas.SetColor(c:TColor);
begin
  Brush.Color:=c;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.BrushChanged
  Params:   ABrush: The changed brush
  Returns:  Nothing

  Notify proc for a brush change
 ------------------------------------------------------------------------------}
procedure TCanvas.BrushChanged(ABrush: TObject);
begin
  if csBrushValid in FState
  then begin
    Exclude(FState, csBrushValid);
    //TODO: Select stock object;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.FontChanged
  Params:   AFont: the changed font
  Returns:  Nothing

  Notify proc for a font change
 ------------------------------------------------------------------------------}
procedure TCanvas.FontChanged(AFont: TObject);
begin
  if csFontValid in FState
  then begin
    Exclude(FState, csFontValid);
    //TODO: Select stock object;
  end;
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.PenChanged
  Params:   APen: The changed pen
  Returns:  Nothing

  Notify proc for a pen change
 ------------------------------------------------------------------------------}
procedure TCanvas.PenChanged(APen: TObject);
begin
  if csPenValid in FState
  then begin
    Exclude(FState, csPenValid);
    //TODO: Select stock object;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.Create
  Params:  none
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TCanvas.Create;
begin
  FHandle := 0;
  inherited Create;
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FSavedFontHandle := 0;
  FPen := TPen.Create;
  FPen.OnChange := @PenChanged;
  FSavedPenHandle := 0;
  FBrush := TBrush.Create;
  FBrush.OnChange := @BrushChanged;
  FSavedBrushHandle := 0;
  FCopyMode := cmSrcCopy;
  FPenPos := Point(0, 0);
end;

{------------------------------------------------------------------------------
  Method: TCanvas.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TCanvas.Destroy;
begin
//writeln('[TCanvas.Destroy] ',ClassName,'  Self=',HexStr(Cardinal(Pointer(Self)),8));
  Handle := 0;
  FFont.Free;
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.GetHandle
  Params:   None
  Returns:  A handle to the GUI object

  Checks if a handle is allocated, otherwise create it
 ------------------------------------------------------------------------------}
function TCanvas.GetHandle : HDC;
begin
  RequiredState(csAllValid);
  Result := FHandle;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.SetHandle
  Params:  NewHandle - the new device context
  Returns: nothing

  Deselect sub handles and sets the Handle
 ------------------------------------------------------------------------------}
procedure TCanvas.SetHandle(NewHandle: HDC);
begin
  if FHandle<>NewHandle then begin
//writeln('[TCanvas.SetHandle] Old=',HexStr(FHandle,8),' New=',HexStr(NewHandle,8));
    if FHandle <> 0 then
    begin
      DeselectHandles;
      FPenPos := GetPenPos;
      FHandle := 0;
      Exclude(FState, csHandleValid);
    end;
    if NewHandle <> 0 then
    begin
      Include(FState, csHandleValid);
      FHandle := NewHandle;
      SetPenPos(FPenPos);
    end;
//writeln('[TCanvas.SetHandle] END Handle=',HexStr(FHandle,8));
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.DeselectHandles
  Params:  none
  Returns: nothing

  Deselect all subhandles in the current device context
 ------------------------------------------------------------------------------}
procedure TCanvas.DeselectHandles;
begin
  if (FHandle<>0) 
  and (FState - [csPenValid, csBrushValid, csFontValid] <> FState) then begin
    // select default sub handles in the device context without deleting owns
    if FSavedBrushHandle<>0 then begin
      SelectObject(FHandle,FSavedBrushHandle);
      FSavedBrushHandle:=0;
    end;
    if FSavedPenHandle<>0 then begin
      SelectObject(FHandle,FSavedPenHandle);
      FSavedPenHandle:=0;
    end;
    if FSavedFontHandle<>0 then begin
      SelectObject(FHandle,FSavedFontHandle);
      FSavedFontHandle:=0;
    end;
    FState := FState - [csPenValid, csBrushValid, csFontValid];
  end;
end;

{------------------------------------------------------------------------------
  Method:  TCanvas.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object).
 ------------------------------------------------------------------------------}
procedure TCanvas.CreateHandle;
begin
// Plain canvas does nothing
end;

{------------------------------------------------------------------------------
  Method:   TCanvas.RequiredState
  Params:   ReqState: The required state
  Returns:  Nothing

  Ensures that all handles needed are valid;
 ------------------------------------------------------------------------------}
procedure TCanvas.RequiredState(ReqState: TCanvasState);
var
  Needed: TCanvasState;
begin
  Needed := ReqState - FState;
  if Needed <> [] then
  begin
writeln('[TCanvas.RequiredState] 1');
    if csHandleValid in Needed then
    begin
      CreateHandle;
      if FHandle = 0
      then raise EInvalidOperation.Create('Canvas does not allow drawing');
      Include(FState, csHandleValid);
    end;
writeln('[TCanvas.RequiredState] 2');
    if csFontValid in Needed then CreateFont;
writeln('[TCanvas.RequiredState] 3');
    if csPenValid in Needed then
    begin
      CreatePen;
      if Pen.Style in [psDash, psDot, psDashDot, psDashDotDot]
      then Include(Needed, csBrushValid);
    end;
writeln('[TCanvas.RequiredState] 4');
    if csBrushValid in Needed then CreateBrush;
  end;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextExtent
  Params:   Text: The text to measure
  Returns:  The size

  Gets the width and height of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextExtent(const Text: string): TSize;
var
  pStr: PChar;
begin
  Result.cX := 0;
  Result.cY := 0;
  RequiredState([csHandleValid, csFontValid]);

  pStr := StrAlloc(Length(Text)+1);
  try
    StrPcopy(pStr, Text);
    GetTextExtentPoint(FHandle, pStr, Length(Text), Result);
  finally
    StrDispose(PStr);
  end;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextWidth
  Params:   Text: The text to measure
  Returns:  The width

  Gets the width of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cX;
end;

{------------------------------------------------------------------------------
  Function: TCanvas.TextHeight
  Params:   Text: The text to measure
  Returns:  A handle to the GUI object

  Gets the height of a text
 ------------------------------------------------------------------------------}
function TCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cY;
end;

{ =============================================================================

  $Log$
  Revision 1.6  2001/03/22 22:39:02  lazarus
  MG: fixed unreleased handle bug

  Revision 1.4  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.3  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.2  2000/08/10 18:56:24  lazarus
  Added some winapi calls.
  Most don't have code yet.
  SetTextCharacterExtra
  CharLowerBuff
  IsCharAlphaNumeric
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.6  2000/07/09 20:18:56  lazarus
  MWE:
    + added new controlselection
    + some fixes
    ~ some cleanup

  Revision 1.5  2000/05/10 22:52:57  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.4  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.3  2000/05/08 15:56:58  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.2  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.27  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.26  2000/03/21 18:53:28  lazarus
  Added code for TBitBtn. Not finished but looks like mostly working.
  Shane

  Revision 1.25  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.24  2000/01/26 19:16:24  lazarus
  Implemented TPen.Style properly for GTK. Done SelectObject for pen objects.
  Misc bug fixes.
  Corrected GDK declaration for gdk_gc_set_slashes.

  Revision 1.23  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.22  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.21  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.20  1999/12/06 16:56:30  lazarus
  Modifications made to help me debug the error during SETTEXT.
  Shane

  Revision 1.19  1999/12/02 19:00:59  lazarus
  MWE:
    Added (GDI)Pen
    Changed (GDI)Brush
    Changed (GDI)Font (color)
    Changed Canvas to use/create pen/brush/font
    Hacked mwedit to allow setting the number of chars (till it get a WM/LM_SIZE event)
    The editor shows a line !


}
