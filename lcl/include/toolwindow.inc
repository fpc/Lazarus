{%MainUnit ../toolwin.pp}

{******************************************************************************
                                  TToolWindow
 ******************************************************************************

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

{ TToolWindow }

constructor TToolWindow.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FEdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
  FEdgeInner := esRaised;
  FEdgeOuter := esLowered;
end;

procedure TToolWindow.AdjustClientRect(var ARect: TRect);
var
  EdgeWidth: Integer;
begin
  EdgeWidth:=0;
  if FEdgeOuter<>esNone then inc(EdgeWidth);
  if FEdgeInner<>esNone then inc(EdgeWidth);
  if ebLeft in FEdgeBorders then
    inc(ARect.Left,EdgeWidth);
  if ebTop in FEdgeBorders then
    inc(ARect.Top,EdgeWidth);
  if ebRight in FEdgeBorders then
    dec(ARect.Right,EdgeWidth);
  if ebBottom in FEdgeBorders then
    dec(ARect.Bottom,EdgeWidth);
end;

procedure TToolWindow.SetEdgeBorders(Value: TEdgeBorders);
begin
  if FEdgeBorders <> Value then begin
    FEdgeBorders := Value;
    Invalidate;
  end;
end;

procedure TToolWindow.SetEdgeInner(Value: TEdgeStyle);
begin
  if FEdgeInner <> Value then begin
    FEdgeInner := Value;
    Invalidate;
  end;
end;

procedure TToolWindow.SetEdgeOuter(Value: TEdgeStyle);
begin
  if FEdgeOuter <> Value then begin
    FEdgeOuter := Value;
    Invalidate;
  end;
end;

procedure TToolWindow.Paint;
const
  InnerStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDINNER, BDR_SUNKENINNER);
  OuterStyles: array[TEdgeStyle] of Integer = (0, BDR_RAISEDOUTER, BDR_SUNKENOUTER);
var
  FEdgeBorderType : Cardinal;
  ARect: TRect;
begin
  FEdgeBorderType := 0;
  if (ebTop in FEdgeBorders)  then
  FEdgeBorderType := FEdgeBorderType or longint(BF_TOP);
  if (ebBottom in FEdgeBorders)  then
  FEdgeBorderType := FEdgeBorderType or longint(BF_BOTTOM);
  if (ebLeft in FEdgeBorders)  then
  FEdgeBorderType := FEdgeBorderType or longint(BF_LEFT);
  if (ebRight in FEdgeBorders)  then
  FEdgeBorderType := FEdgeBorderType or longint(BF_RIGHT);
  ARect:=ClientRect;
  DrawEdge(Canvas.Handle,ARect,
           InnerStyles[FEdgeInner] or OuterStyles[FEdgeOuter],FEdgeBorderType);

  inherited Paint;
end;

// included by toolwin.pp
