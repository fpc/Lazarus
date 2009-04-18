{%MainUnit ../controls.pp}

{******************************************************************************
                                  TDragImageList
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

const
  // bad window handle. 0 is reserverd by DesktopWindow
  NoLockedWindow: HWND = High(PtrInt);
  
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
    WidgetSet.SetCursor(Screen.Cursors[DragCursor]);
end;

class procedure TDragImageList.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterDragImageList;
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
  FLastDragPos := Point(0, 0);
  FOldCursor := crNone;
  FLockedWindow := NoLockedWindow;
  FImageIndex := 0;
end;

{
  TDragImageList.BeginDrag
  Start dragging of drag image list
}
function TDragImageList.BeginDrag(Window: HWND; X, Y: Integer): Boolean;
begin
  Result := TWsDragImageListClass(WidgetSetClass).BeginDrag(Self, Window,
    FImageIndex, FDragHotspot.X, FDragHotspot.Y);
  FDragging := Result;
  if Result then
  begin
    DragLock(Window, X, Y);
    FOldCursor := Screen.Cursor;
    WidgetSet.SetCursor(Screen.Cursors[DragCursor])
  end;
end;

{
  TDragImageList.DragLock
  Show drag image and locks updates of Window during drag operation
}
function TDragImageList.DragLock(Window: HWND; XPos, YPos: Integer): Boolean;
begin
  Result := Dragging;
  if not Result then
  begin
    Result := BeginDrag(Window, XPos, YPos);
    Exit;
  end;

  if Window <> FLockedWindow then
  begin
    if FLockedWindow <> NoLockedWindow then
      DragUnlock;
    FLockedWindow := Window;
    Result := TWsDragImageListClass(WidgetSetClass).ShowDragImage(Self, Window,
      XPos, YPos, True);
    if Result then
      FLastDragPos := Point(XPos, YPos);
  end;
end;

{
  TDragImageList.DragMove
  Move dragging image to position X, Y
}
function TDragImageList.DragMove(X, Y: Integer): Boolean;
begin
  Result := Dragging and TWsDragImageListClass(WidgetSetClass).DragMove(Self, X, Y);
  if Result then
    FLastDragPos := Point(X, Y);
end;

{
  TDragImageList.DragUnlock
  Hide drag image and stop lock updates of Window during drag operation
}
procedure TDragImageList.DragUnlock;
begin
  if Dragging then
  begin
    TWsDragImageListClass(WidgetSetClass).HideDragImage(Self, FLockedWindow, True);
    FLockedWindow := NoLockedWindow;
    FLastDragPos := Point(0, 0);
  end;
end;

{
  Finish dragging of drag image list
}
function TDragImageList.EndDrag: Boolean;
begin
  Result := Dragging;
  if not Result then
    Exit;

  DragUnlock;
  TWsDragImageListClass(WidgetSetClass).EndDrag(Self);
  FDragging := False;
  FDragCursor := crNone;
  WidgetSet.SetCursor(Screen.Cursors[FOldCursor])
end;

{
  TDragImageList.GetHotSpot
  Returns HotSpot
}
function TDragImageList.GetHotSpot: TPoint;
begin
  Result := FDragHotSpot;
end;

{
  TDragImageList.HideDragImage
  Hide dragging image without unlocking of window
}
procedure TDragImageList.HideDragImage;
begin
  if Dragging then
    TWsDragImageListClass(WidgetSetClass).HideDragImage(Self, 0, False);
end;

{
  TDragImageList.SetDragImage
  Set index of dragging image and hotspot
}
function TDragImageList.SetDragImage(Index, HotSpotX, HotSpotY: Integer): Boolean;
var
  CurLockedWindow: HWND;
  CurDragPos: TPoint;
begin
  Result := True;
  if (FImageIndex <> Index) or (FDragHotSpot.X <> HotSpotX) or
     (FDragHotSpot.Y <> HotSpotY) then
  begin
    FImageIndex := Index;
    FDragHotSpot := Point(HotSpotX, HotSpotY);
    if Dragging then
    begin
      // restart dragging with new params
      CurLockedWindow := FLockedWindow;
      CurDragPos := FLastDragPos;
      EndDrag;
      BeginDrag(CurLockedWindow, CurDragPos.X, CurDragPos.Y);
    end;
  end;
end;

{
  TDragImageList.ShowDragImage
  Show dragging image
}
procedure TDragImageList.ShowDragImage;
begin
  if Dragging then
    TWsDragImageListClass(WidgetSetClass).ShowDragImage(Self, 0, 0, 0, False);
end;

// included by controls.pp
