{******************************************************************************
                                  TToolWindow
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

{ TToolWindow }

constructor TToolWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  FEdgeInner := esRaised;
  FEdgeOuter := esLowered;
end;

procedure TToolWindow.SetEdgeBorders(Value: TEdgeBorders);
begin
  if FEdgeBorders <> Value then
  begin
    FEdgeBorders := Value;
    RecreateWnd;
  end;
end;

procedure TToolWindow.SetEdgeInner(Value: TEdgeStyle);
begin
  if FEdgeInner <> Value then
  begin
    FEdgeInner := Value;
    RecreateWnd;
  end;
end;

procedure TToolWindow.SetEdgeOuter(Value: TEdgeStyle);
begin
  if FEdgeOuter <> Value then
  begin
    FEdgeOuter := Value;
    RecreateWnd;
  end;
end;

procedure TToolWindow.LMNCCalcSize(var Message: TLMNCCalcSize);
var
  EdgeSize: Integer;
begin
  with Message.CalcSize_Params^ do
  begin
    InflateRect(rgrc[0], -BorderWidth, -BorderWidth);
    EdgeSize := 0;
    if EdgeInner <> esNone then Inc(EdgeSize, 1);
    if EdgeOuter <> esNone then Inc(EdgeSize, 1);
    with rgrc[0] do
    begin
      if ebLeft in FEdgeBorders then Inc(Left, EdgeSize);
      if ebTop in FEdgeBorders then Inc(Top, EdgeSize);
      if ebRight in FEdgeBorders then Dec(Right, EdgeSize);
      if ebBottom in FEdgeBorders then Dec(Bottom, EdgeSize);
    end;
  end;
end;

procedure TToolWindow.LMNCPaint(var Message: TLMessage);
const
  InnerStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
  //Ctl3DStyles: array[Boolean] of Integer = (BF_MONO, 0);
var
  DC: HDC;
  RW: TRect;
  FEdgeBorderType : Cardinal;
begin
Assert(False, 'Trace:********************');
Assert(False, 'Trace:********************');
Assert(False, 'Trace:********************');

 DC := GetDC(Handle);
  try
//    GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
//    MapWindowPoints(0, Handle, RW, 2);
//    OffsetRect(RC, -RW.Left, -RW.Top);
//    ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
    { Draw borders in non-client area }
//    OffsetRect(RW, -RW.Left, -RW.Top);
     FEdgeBorderType := 0;
    if (ebTOP in FEdgeBorders)  then
    FEdgeBorderType := FEdgeBorderType or longint(ebTOP);
    if (ebBottom in FEdgeBorders)  then
    FEdgeBorderType := FEdgeBorderType or longint(ebBottom);
    if (ebLeft in FEdgeBorders)  then
    FEdgeBorderType := FEdgeBorderType or longint(ebLeft);
    if (ebRight in FEdgeBorders)  then
    FEdgeBorderType := FEdgeBorderType or longint(ebRight);
    DrawEdge(DC,RW,InnerStyles[FEdgeInner] or OuterStyles[FEdgeOuter],FEdgeBorderType);
//    FillRect(DC, RW, Brush.Handle);
  finally
    ReleaseDC(Handle, DC);
  end;

end;

procedure TToolWindow.CMBorderChanged(var Message: TLMessage);
begin
//TODO: FiNISH CMBORDERCHANGED in toolwindow.inc
end;

procedure TToolWindow.CMCtl3DChanged(var Message: TLMessage);
begin
//TODO: FiNISH CMCTL3DCHANGED in toolwindow.inc
end;

