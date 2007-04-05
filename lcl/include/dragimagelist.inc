{%MainUnit ../controls.pp}

{******************************************************************************
                                  TDragImageList
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

{ TDragImageList }

{
   TDragImageList.SetDragCursor
   sets drag cursor that is associated with drag image list
}
procedure TDragImageList.SetDragCursor(const AValue: TCursor);
begin
  if FDragCursor = AValue then exit;
  FDragCursor := AValue;
  if Dragging then
    Screen.Cursor := FDragCursor;
end;

{
  TDragImageList.Initialize
  set default values for properties
}
procedure TDragImageList.Initialize;
begin
  inherited Initialize;
  FDragging := False;
  FDragCursor := crNone;
  FDragHotspot := Point(0, 0);
  FOldCursor := crNone;
  FImageIndex := -1;
end;

{
  TDragImageList.BeginDrag
  Start dragging of drag image list
}
function TDragImageList.BeginDrag(Window: HWND; X, Y: Integer): Boolean;
begin
  {$note implement}
   Result := TWsDragImageListClass(WidgetSetClass).BeginDrag(Self, Window, X, Y);
   FDragging := Result;
   if Result then
   begin
     FOldCursor := Screen.Cursor;
     Screen.Cursor := FDragCursor;
   end;
end;

{
  TDragImageList.DragLock
}
function TDragImageList.DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
begin
  if Dragging then
  begin
    {$note implement}
  end else
    Result := False;
end;

{
  TDragImageList.DragMove
  Move dragging image to position X, Y
}
function TDragImageList.DragMove(X, Y: Integer): Boolean;
begin
  if not Dragging then
    BeginDrag(0, X, Y);
  Result := Dragging;
  if Result then
    Result := TWsDragImageListClass(WidgetSetClass).DragMove(Self, X, Y);
  {$note implement}
end;

{
  TDragImageList.DragUnlock
}
procedure TDragImageList.DragUnlock;
begin
  if Dragging then
  begin
    {$note implement}
  end;
end;

{
  Finish dragging of drag image list
}
function TDragImageList.EndDrag: Boolean;
begin
  if Dragging then
  begin
    FDragging := False;
    FDragCursor := crNone;
    TWsDragImageListClass(WidgetSetClass).EndDrag(Self);
    {$note implement}
  end else
    Result := False;
end;

{
  TDragImageList.GetHotSpot
  Returns HotSpot
}
function TDragImageList.GetHotSpot: TPoint;
begin
  Result := inherited GetHotSpot;
end;

{
  TDragImageList.HideDragImage
  Hide dragging image
}
procedure TDragImageList.HideDragImage;
begin
  if Dragging then
  begin
    TWsDragImageListClass(WidgetSetClass).HideDragImage(Self);
    {$note implement}
  end;
end;

{
  TDragImageList.SetDragImage
  Set index of dragging image and hotspot
}
function TDragImageList.SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean;
begin
  FImageIndex := Index;
  FDragHotSpot := Point(HotSpotX, HotSpotY);
  {$note implement}
end;

{
  TDragImageList.ShowDragImage
  Show dragging image
}
procedure TDragImageList.ShowDragImage;
begin
  if Dragging then
  begin
    TWsDragImageListClass(WidgetSetClass).ShowDragImage(Self);
    {$note implement}
  end;
end;

// included by controls.pp
