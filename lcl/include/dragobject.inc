{%MainUnit ../controls.pp}

{******************************************************************************
                                     TDragObject
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

{ TDragObject }

procedure TDragObject.Assign(Source: TDragObject);
begin
  FDragTarget := Source.FDragTarget;
  FDragHandle := Source.FDragHandle;
  FDragPos := Source.FDragPos;
  FDragTargetPos := Source.FDragTargetPos;
  FMouseDeltaX := Source.FMouseDeltaX;
  FMouseDeltaY := Source.FMouseDeltaY;
  FAlwaysShowDragImages := Source.FAlwaysShowDragImages;
end;

function TDragObject.Capture: HWND;
begin
  Result:=0;
  //SetCapture(Result);
end;

procedure TDragObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
  
end;

function TDragObject.GetName: string;
begin
  Result := ClassName;
end;

function TDragObject.GetDragImages: TDragImageList;
begin
  Result := nil;
end;

procedure TDragObject.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P:=Point(X,Y);
  DragTo(P);
end;

procedure TDragObject.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin

end;

procedure TDragObject.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  DragDone(True);
end;

procedure TDragObject.CaptureChanged(OldCaptureControl: TControl);
var
  i: integer;
  IsDragging: Boolean;
begin
  // if this is TWinControl, and it have controls (not TWinControls)
  // then we should check Dragging in those controls
  IsDragging := OldCaptureControl.Dragging;
  if (not IsDragging) and (OldCaptureControl is TWinControl) then
  begin
    for i := 0 to TWinControl(OldCaptureControl).ControlCount - 1 do
    begin
      IsDragging := IsDragging or TWinControl(OldCaptureControl).Controls[i].Dragging;
      if IsDragging then
        break;
    end;
  end;
  DragDone(IsDragging);
end;

procedure TDragObject.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of

  VK_CONTROL:
    DragTo(DragObject.DragPos);

  VK_ESCAPE:
    begin
      Key:=VK_UNKNOWN; // Consume keystroke and cancel drag operation
      DragDone(False);
    end;

  end;
end;

procedure TDragObject.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_CONTROL then DragTo(DragObject.DragPos);
end;

destructor TDragObject.Destroy;
begin
  {$IFDEF VerboseDrag}
  DebugLn('TDragObject.Destroy ',ClassName,' Self=',DbgS(Self));
  {$ENDIF}
  inherited Destroy;
end;

function TDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := crDrag
  else
    Result := crNoDrop;
end;

procedure TDragObject.HideDragImage;
begin
  if GetDragImages <> nil then
    GetDragImages.HideDragImage;
end;

function TDragObject.Instance: THandle;
begin
  // exist only for compatibility
  Result:=0;
end;

procedure TDragObject.ShowDragImage;
begin
  if GetDragImages <> nil then
    GetDragImages.ShowDragImage;
end;

{ TDragControlObject }

function TDragControlObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then
    Result := Control.DragCursor
  else
    Result := crNoDrop;
end;

function TDragControlObject.GetDragImages: TDragImageList;
begin
  Result := Control.GetDragImages;
end;

{ TDragDockObject }

procedure TDragDockObject.SetBrush(Value: TBrush);
begin
  if FBrush=nil then FBrush:=TBrush.Create;
  FBrush.Assign(Value);
end;

procedure TDragDockObject.AdjustDockRect(ARect: TRect);

  function AbsMin(Value1, Value2: Integer): Integer;
  begin
    if Abs(Value1) < Abs(Value2) then Result := Value1
    else Result := Value2;
  end;

var
  DeltaX, DeltaY: Integer;
begin
  if (FDragPos.x<ARect.Left) or (FDragPos.x>ARect.Right) then
    DeltaX := AbsMin(ARect.Left-FDragPos.x,ARect.Right-FDragPos.x)
  else
    DeltaX := 0;
  if (FDragPos.y<ARect.Top) or (FDragPos.y>ARect.Bottom) then
    DeltaY := AbsMin(ARect.Top-FDragPos.y,ARect.Bottom-FDragPos.y)
  else
    DeltaY := 0;
  if (DeltaX<>0) or (DeltaY<>0) then
    OffsetRect(FDockRect, -DeltaX, -DeltaY);
end;

procedure TDragDockObject.DrawDragDockImage;
begin
  FControl.DrawDragDockImage(Self);
end;

procedure TDragDockObject.EndDrag(Target: TObject; X, Y: Integer);
begin
  FControl.DoEndDock(Target, X, Y);
end;

procedure TDragDockObject.EraseDragDockImage;
begin
  FControl.EraseDragDockImage(Self);
end;

function TDragDockObject.GetDragCursor(Accepted: Boolean; X, Y: Integer
  ): TCursor;
begin
  Result := crDefault;
end;

function TDragDockObject.GetFrameWidth: Integer;
begin
  Result:=4;
end;

constructor TDragDockObject.Create(AControl: TControl);
begin
  inherited Create(AControl);
end;

destructor TDragDockObject.Destroy;
begin
  FreeThenNil(FBrush);
  inherited Destroy;
end;

procedure TDragDockObject.Assign(Source: TDragObject);
begin
  inherited Assign(Source);
end;


// included by controls.pp
