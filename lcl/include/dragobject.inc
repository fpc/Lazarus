(******************************************************************************
                                     TDragObject
 ******************************************************************************)

procedure TDragObject.Assign(Source: TDragObject);
begin
  FDragTarget := Source.FDragTarget;
  FDragHandle := Source.FDragHandle;
  FDragPos := Source.FDragPos;
  FDragTargetPos := Source.FDragTargetPos;
  FMouseDeltaX := Source.FMouseDeltaX;
  FMouseDeltaY := Source.FMouseDeltaY;
end;

function TDragObject.Capture: HWND;
begin
  // ToDo
  Result:=0;
end;

procedure TDragObject.Finished(Target: TObject; X, Y: Integer; Accepted: Boolean);
begin
  
end;

function TDragObject.GetName: string;
begin
  Result := ClassName;
end;

procedure TDragObject.ReleaseCapture(Handle: HWND);
begin
end;

procedure TDragObject.MouseMsg(var Msg: TLMessage);
var
  P: TPoint;
begin
Assert(False, 'Trace:******************8');
Assert(False, 'Trace:DRAGOBJECT.MOUSEMSG');
Assert(False, 'Trace:******************8');
  try
    case Msg.Msg of
      LM_MOUSEMOVE:
        begin
          P := SmallPointToPoint(TLMMouse(Msg).Pos);
          ClientToScreen(DragCapture, P);
          DragTo(P);
        end;
      LM_LBUTTONUP, LM_RBUTTONUP: begin
Assert(False, 'Trace:******************');
Assert(False, 'Trace:******************');
          DragDone(True);
Assert(False, 'Trace:******************');
Assert(False, 'Trace:******************');
               end;
      CN_KEYUP: Begin
        if Msg.WParam = VK_CONTROL then DragTo(DragObject.DragPos);
                end;
      CN_KEYDOWN:
        begin
          case Msg.WParam of
            VK_CONTROL: begin
              DragTo(DragObject.DragPos);
                        end;
            VK_ESCAPE:
              begin
                Msg.Result := 1;
                DragDone(False);
              end;
          end;
        end;
    end;
  except
    if DragControl <> nil then DragDone(False);
    raise;
  end;
end;

function TDragObject.GetDragImages: TDragImageList;
begin
  Result := nil;
end;

function TDragObject.GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor;
begin
  if Accepted then Result := crDrag
  else Result := crNoDrop;
end;

procedure TDragObject.HideDragImage;
begin
  // do nothing
end;

function TDragObject.Instance: THandle;
begin
  // ToDo
  Result:=0;
end;

procedure TDragObject.ShowDragImage;
begin
  // do nothing
end;
