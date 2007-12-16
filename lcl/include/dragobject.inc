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

procedure TDragObject.WndProc(var Msg: TLMessage);
//Some drag&dock handling
var P: TPoint;
begin
  try
    case Msg.Msg of
      LM_MOUSEMOVE: begin
        P := SmallPointToPoint(TLMMouseMove(Msg).Pos);
        ClientToScreen(DragCapture, P);
        DragManager.DragTo(P);
      end;
      LM_CAPTURECHANGED: DragManager.DragDone(false);
      LM_LBUTTONUP, LM_RBUTTONUP: DragManager.DragDone(True);
      { Forms.IsKeyMsg sends WM_KEYxxx messages here (+CN_BASE) when a
        TPUtilWindow has the mouse capture. }
      CN_KEYUP:
        //TODO: This is never called! Missing code?
        if Msg.WParam = VK_CONTROL
        then DragManager.DragTo(DragPos);
      CN_KEYDOWN:begin
        //TODO: This is never called! Missing code?
        case Msg.WParam of
          VK_CONTROL: DragManager.DragTo(DragPos);
          VK_ESCAPE:begin
            { Consume keystroke and cancel drag operation }
            Msg.Result := 1;
            DragManager.DragDone(false);
          end;
        end;
      end;
    end;
  except
    DragManager.DragDone(false);
    Application.HandleException(Self);
  end;
end;

procedure TDragObject.MainWndProc(var Message: TLMessage);
begin
  try
    WndProc(Message);
  except
    Application.HandleException(Self);
  end;
end;

procedure TDragObject.Capture;
begin
  if DragCapture=0 then begin
    DragCapture := WidgetSet.AllocateHWnd(@MainWndProc);
    WidgetSet.SetCapture(DragCapture);
  end;
end;

procedure TDragObject.ReleaseCapture;
begin
  if DragCapture<>0 then begin
    WidgetSet.ReleaseCapture;
    WidgetSet.DeallocateHWnd(DragCapture);
    DragCapture := 0;
  end;
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
    Result := Control.DragCursor else
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
  FBrush:=TBrush.Create;
  FBrush.Color:=clGray;
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
