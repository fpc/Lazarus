(******************************************************************************
                                     TControl
 ******************************************************************************)

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{ $DEFINE CHECK_POSITION}

{------------------------------------------------------------------------------}
{       TControl.AdjustSize
}
{------------------------------------------------------------------------------}
procedure TControl.Adjustsize;
begin
  if not (csLoading in ComponentState) then SetBounds(Left, Top, Width, Height);
end;

{------------------------------------------------------------------------------
  Method: TControl.BeginDrag
  Params: Immediate: Drag behaviour
          Threshold: default -1, distance to move before dragging starts
  Returns: Nothing

  Starts the dragging of a control. If the Immidiate flag is set, draggin
  starts immediate.
 ------------------------------------------------------------------------------}
procedure TControl.BeginDrag(Immediate: Boolean; Threshold: Integer);
var
  P : TPoint;
begin
  if (DragControl = nil) or (pointer(dragcontrol) = pointer($FFFFFFFF)) then
     Begin
     Assert(False, 'Trace:Dragcontrol = nil or fff');
     DragControl := nil;
     if csLButtonDown in ControlState then
         Begin
          GetCursorPos(p);
          Assert(False, Format('Trace:p.x p.y = %d,%d',[P.x,P.y]));
          P := ScreentoClient(p);
          Assert(False, Format('Trace:p.x p.y = %d,%d',[P.x,P.y]));
          Perform(LM_LBUTTONUP,0,Longint(PointtoSmallPoint(p)));
        end;
        if Threshold < 0 then
           Threshold := MOuse.DragThreshold;
        if pointer(DragControl) <> pointer($FFFFFFFF) then
           DragInitControl(Self,Immediate,Threshold);
     end;
end;

procedure TControl.BeginDrag(Immediate: Boolean);
begin
  BeginDrag(Immediate, -1);
end;


{------------------------------------------------------------------------------}
{       TControl.BeginAutoDrag
}
{------------------------------------------------------------------------------}
Procedure TControl.BeginAutoDrag;
begin
  BeginDrag(Mouse.DragImmediate,MOuse.DragThreshold);
end;

{------------------------------------------------------------------------------}
{       TControl.BoundsChanged                                                 
}
{------------------------------------------------------------------------------}
procedure TControl.BoundsChanged;
begin
  { Notifications can be performed here }
end; 

{------------------------------------------------------------------------------}
{       TControl.Bringtofront
}
{------------------------------------------------------------------------------}
Procedure TControl.BringToFront;
begin
  SetZOrder(true);
end;

{------------------------------------------------------------------------------}
{       TControl.Change
}
{------------------------------------------------------------------------------}
Procedure TControl.Changed;
Begin
  Perform(CM_CHANGED,0,Longint(self));
End;

{------------------------------------------------------------------------------}
{       TControl.ChangeBounds
}
{------------------------------------------------------------------------------}
procedure TControl.ChangeBounds(ALeft, ATop, AWidth, AHeight : integer);
var SizeChanged, PosChanged : boolean;
    SizeMsg : TLMSize;
    MoveMsg : TLMMove;
begin
  DoConstrainedResize(AWidth, AHeight);
  SizeChanged:= (FWidth <> AWidth) or (FHeight <> AHeight);
  PosChanged:= (FLeft <> ALeft) or (FTop <> ATop);
  if SizeChanged or PosChanged then begin
    FLeft:= ALeft;
    FTop:= ATop;
    FWidth:= AWidth;
    FHeight:= AHeight;
    if SizeChanged then Invalidate;
//    UpdateAnchorRules;
    BoundsChanged;
    RequestAlign;
    if (not (csLoading in ComponentState)) then begin
      Resize;
      Perform(LM_WindowposChanged, 0, 0);

      { TODO : Replace this with LCL functions eventually.
          Right now we have to call these messages as some
          descendents incorrectly depend on it, so force it
          down their throat }

      if SizeChanged then begin
        with SizeMsg do begin
          Msg:= LM_SIZE;
          SizeType:= 6; // force realign
          Width:= FWidth;
          Height:= FHeight;
{$IFDEF CHECK_POSITION}
writeln(' [TControl.ChangeBounds] ',Name,':',ClassName,' SizeMsg Width=',Width,' Height=',Height);
{$ENDIF}
        end;
        WindowProc(TLMessage(SizeMsg));
      end;

      if PosChanged then begin
        with MoveMsg do begin
          Msg:= LM_MOVE;
          MoveType:= 1;
          XPos:= FLeft;
          YPos:= FTop;
{$IFDEF CHECK_POSITION}
writeln(' [TControl.ChangeBounds] ',Name,':',ClassName,' MoveMsg XPos=',XPos,' YPos=',YPos);
{$ENDIF}
        end;
        WindowProc(TLMessage(MoveMsg));
      end;
    end;
  end;    
end;

{------------------------------------------------------------------------------}
{       TControl.ChangeScale
}
{------------------------------------------------------------------------------}
Procedure TControl.ChangeScale(M,D : Integer);
Begin
//TODO: TCONTROL.CHANGESCALE
  Assert(False, 'Trace:TODO: [TControl.ChangeScale]');
end;


{------------------------------------------------------------------------------}
{       TControl.CheckMenuPopup
}
{------------------------------------------------------------------------------}
Procedure TControl.CheckMenuPopup(const P : TSmallPoint);
var
  Control: TControl;
  TempPopupMenu: TPopupMenu;
  P2 : Tpoint;
begin
  if csDesigning in ComponentState then Exit;
  Control := Self;
  while Control <> nil do
  begin
    TempPopupMenu := Control.GetPopupMenu;
    if (TempPopupMenu <> nil) then
    begin
      if not TempPopupMenu.AutoPopup then Exit;
//      SendCancelMode(nil);
      TempPopupMenu.PopupComponent := Control;
      P2 := SmallPointtoPoint(P);
      P2 := ClientToScreen(P2);
        TempPopupMenu.Popup(P2.X, P2.Y);
      Exit;
    end;
    Control := Control.Parent;
  end;
end;


Procedure TControl.LMCaptureChanged(Var Message: TLMessage);
Begin
  //Writeln('[LMCaptureChanged for '+classname+']');
End;


{------------------------------------------------------------------------------}
{       TControl.CMENABLEDCHANGED
}
{------------------------------------------------------------------------------}
procedure TControl.CMEnabledChanged(var Message: TLMEssage);
begin
  invalidate;
end;

{------------------------------------------------------------------------------}
{       TControl.CMHITTEST
}
{------------------------------------------------------------------------------}
procedure TControl.CMHITTEST(var MEssage : TCMHitTest);
begin
  message.Result := 1;
end;

{------------------------------------------------------------------------------}
{       TControl.CMMouseEnter
}
{------------------------------------------------------------------------------}
Procedure TControl.CMMouseEnter(var Message :TLMessage);
Begin
if FParent <> nil then
   FParent.Perform(CM_MOUSEENTER,0,longint(Self));
end;

{------------------------------------------------------------------------------}
{       TControl.CMMouseLEave
}
{------------------------------------------------------------------------------}
Procedure TControl.CMMouseLeave(var Message :TLMessage);
Begin
if FParent <> nil then
   FParent.Perform(CM_MOUSELEAVE,0,longint(Self));

end;

{------------------------------------------------------------------------------}
{       TControl.CMVisibleChanged
}
{------------------------------------------------------------------------------}
procedure TControl.CMVisibleChanged(var MEssage : TLMessage);
begin
  if not (csDesigning in ComponentState) or (csNoDesignVisible in ControlStyle) then begin
    InvalidateControl(true, FVisible and (csOpaque in ControlStyle));
  end;
end;

{------------------------------------------------------------------------------}
{       TControl.ConstrainedResize
}
{------------------------------------------------------------------------------}
procedure TControl.ConstrainedResize(var MinWidth, MinHeight, MaxWidth, MaxHeight : TConstraintSize);
begin
  if Assigned(FOnConstrainedResize) then
    FOnConstrainedResize(Self, MinWidth, MinHeight, MaxWidth, MaxHeight);
end;

{------------------------------------------------------------------------------}
{       TControl.DoConstrainedResize
}
{------------------------------------------------------------------------------}
procedure TControl.DoConstrainedResize(var NewWidth, NewHeight : integer);
var MinWidth, MinHeight, MaxWidth, MaxHeight : TConstraintSize;
begin
  if Constraints.MinWidth > 0 then MinWidth:= Constraints.MinWidth
  else MinWidth:= 0;
  if Constraints.MinHeight > 0 then MinHeight:= Constraints.MinHeight
  else MinHeight:= 0;
  if Constraints.MaxWidth > 0 then MaxWidth:= Constraints.MaxWidth
  else MaxWidth:= 0;
  if Constraints.MaxHeight > 0 then MaxHeight:= Constraints.MaxHeight
  else MaxHeight:= 0;

  ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);
  
  if (MinWidth > 0) and (NewWidth < MinWidth) then NewWidth:= MinWidth
  else if (MaxWidth > 0) and (NewWidth > MaxWidth) then NewWidth:= MaxWidth;
  
  if (MinHeight > 0) and (NewHeight < MinHeight) then NewHeight:= MinHeight
  else if (MaxHeight > 0) and (NewHeight > MaxHeight) then NewHeight:= MaxHeight;
end;

{------------------------------------------------------------------------------}
{       TControl.DoConstraintsChange					       }
{------------------------------------------------------------------------------}
procedure TControl.DoConstraintsChange(Sender : TObject);
begin
  AdjustSize;
end;

{------------------------------------------------------------------------------}
{       TControl.DragCanceled
}
{------------------------------------------------------------------------------}
procedure TControl.DragCanceled;
begin
end;


{------------------------------------------------------------------------------}
{       TControl.DoStartDrag
}
{------------------------------------------------------------------------------}
procedure TControl.DoStartDrag(var DragObject: TDragObject);
begin
  if Assigned(FOnStartDrag) then FOnStartDrag(Self, DragObject);
end;

{------------------------------------------------------------------------------}
{       TControl.DoStartDrag
}
{------------------------------------------------------------------------------}
Procedure TControl.DoEndDrag(Target: TObject; X,Y : Integer);
Begin

end;



{------------------------------------------------------------------------------}
{       TControl.Perform
}
{------------------------------------------------------------------------------}
Function TControl.Perform(Msg:Cardinal; WParam , LParam : LongInt): LongInt;
var
  Message : TLMessage;
begin
  Message.Msg := Msg;
  Message.WParam := WParam;
  Message.LParam := LParam;
  Message.Result := 0;
  If Self <> nil then WindowProc(Message);
    Result := Message.Result;
end;

{------------------------------------------------------------------------------}
{       TControl.GetClientOrigin
}
{------------------------------------------------------------------------------}
function TControl.GetClientOrigin: TPoint;
Begin
  Assert(False, Format('Trace:[TControl.GetClientOrigin] %s',  [Classname]));
  if Parent = nil then Exit;
  Result := Parent.ClientOrigin;
  Inc(Result.X, FLeft);
  Inc(Result.Y, FTop);
  Assert(False, Format('Trace:[TControl.GetClientOrigin] %s --> (%d, %d)',  [Classname, Result.X, Result.Y]));
end;

{------------------------------------------------------------------------------}
{       TControl.ScreenToClient
}
{------------------------------------------------------------------------------}
Function TControl.ScreenToCLient(const Point : TPoint) : TPoint;
var
  P : TPoint;
begin
  P := ClientOrigin;
  Result.X := Point.X - P.X;
  Result.Y := Point.Y - P.Y;
  Assert(False, Format('Trace:[TControl.ScreenToCLient] %s: Point(%d, %d) Org(%d, %d) --> (%d, %d)',  [Classname, Point.x,  point.y, p.x, p.y, result.x, result.y]));
end;

{------------------------------------------------------------------------------}
{       TControl.ClienttoScreen
}
{------------------------------------------------------------------------------}
Function TControl.ClientToScreen(const Point : TPoint) : TPoint;
var
  P : TPoint;
begin
  P := ClientOrigin;
  Result.X := Point.X + P.X;
  Result.Y := Point.Y + P.Y;
end;

{------------------------------------------------------------------------------}
{       TControl.SendDockNotification
}
{------------------------------------------------------------------------------}
Procedure TControl.SendDockNotification(Msg: Cardinal; WPAram, LParam : Integer);
begin

end;


{------------------------------------------------------------------------------}
{       TControl.DblClick
}
{------------------------------------------------------------------------------}
procedure TControl.DblClick;
begin
If Assigned(FOnDblClick) then FOnDblCLick(Self);
end;



{------------------------------------------------------------------------------}
{       TControl.DoDragMsg
}
{------------------------------------------------------------------------------}
Procedure TControl.DoDragMsg(var Dragmsg : TCMDrag);
var
Accepts : Boolean;
S : TObject;
Begin
with DragMSg, Dragrec^ do
   Begin
    S := Source;
    with ScreentoClient(pos) do
    begin
    case DragMessage of
        dmDragEnter, dmDragLeave, dmDragMove:
           begin
            Accepts := True;
            case DragMessage of
                dmDragEnter : DragOver(S,X,Y,dsDragEnter,Accepts);
                dmDragLeave : DragOver(S,X,Y,dsDragLeave,Accepts);
                dmDragMove : DragOver(S,X,Y,dsDragMove,Accepts);
              end;
            Result := ord(Accepts);
           end;
         end; //case
    end;//with
   end;  //with
end;

{------------------------------------------------------------------------------}
{       TControl.DragOver
}
{------------------------------------------------------------------------------}
Procedure TControl.DragOver(Source: TObject; X,Y : Integer; State : TDragState; var Accept:Boolean);
begin
  Accept := False;
  if Assigned(FOnDragOver) 
  then begin
    Accept := True;
  //Do something else yet....
  end;

end;

{------------------------------------------------------------------------------}
{       TControl.DragDrop
}
{------------------------------------------------------------------------------}
Procedure TControl.DragDrop(Source: TObject; X,Y : Integer);
begin
If Assigned(FOnDragDrop) then FOnDragDrop(Self, Source,X,Y);
end;

{------------------------------------------------------------------------------}
{       TControl Method SetColor  "Sets the default color and tells the widget set"                  }
{------------------------------------------------------------------------------}
procedure TControl.SetColor(value : TColor);
begin
   FColor := Value;
   CNSendMessage(LM_SETCOLOR, Self, nil);
end;

{------------------------------------------------------------------------------}
{       TControl CanAutoSize                                 }
{------------------------------------------------------------------------------}
Function TControl.CanAutoSize(Var NewWidth, NewHeight : Integer): Boolean;
Begin
Result := True;
end;

{------------------------------------------------------------------------------}
{       TControl Dragging                                 }
{------------------------------------------------------------------------------}
Function TControl.Dragging: Boolean;
Begin
Result := (DragControl = self);
end;

{------------------------------------------------------------------------------}
{       TControl GetBoundsRect                                 }
{------------------------------------------------------------------------------}
Function TControl.GetBoundsRect: TRect;
Begin
Result.left := FLEft;
Result.Top := FTop;
Result.Right := FLeft+FWidth;
Result.Bottom := FTop+FHeight;
end;

{------------------------------------------------------------------------------}
{       TControl GetEnabled                                 }
{------------------------------------------------------------------------------}
function TControl.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

{------------------------------------------------------------------------------}
{       TControl GetMouseCapture                                 }
{------------------------------------------------------------------------------}
Function TControl.GetMouseCapture : Boolean;
Begin
Result := GetCaptureControl = Self;
end;

{------------------------------------------------------------------------------}
{       TControl GetPopupMenu                                 }
{------------------------------------------------------------------------------}
function TControl.GetPopupMenu: TPopupMenu;
begin
  Result := FPopupMenu;
end;

{------------------------------------------------------------------------------}
{       TControl GetClientRect                                 }
{------------------------------------------------------------------------------}
function TControl.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

{------------------------------------------------------------------------------}
{       TControl WndPRoc                                 }
{------------------------------------------------------------------------------}
procedure TControl.WndPRoc(var Message : TLMessage);
Var
  Form : TCustomForm;
begin
  if (csDesigning in ComponentState) then
  begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.Designer <> nil) and
          Form.Designer.IsDesignMsg(Self,MEssage) then Exit;
  end
  else
  begin
    if (Message.Msg >= LM_KeyFirst) and (Message.Msg <= LM_KeyLast) then
    begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.WantChildKey(Self,Message)) then exit;
    end
    else
      begin
      if (Message.Msg >= LM_MOUSEFIRST) and (Message.msg <= LM_MOUSELAST) then
        begin
        if not (csDoubleClicks in ControlStyle) then
          begin
          case Message.Msg of
            LM_LButtonDBLCLK,
            LM_RBUTTONDBLCLK,
            LM_MBUTTONDBLCLK:
              Dec(Message.Msg, LM_LBUTTONDBLCLK - LM_LBUTTONDOWN);
          end;
          end;
        case Message.Msg of
          LM_MOUSEMOVE:
            begin
            Application.HintMouseMessage(Self, Message);
            if Dragging then DragObject.MouseMsg(Message);
            end;
          LM_LBUTTONDOWN,
          LM_LBUTTONDBLCLK: begin
            if FDragMode = dmAutomatic 
            then begin
              Assert(False, 'Trace:Begin AutoDrag called');
              BeginAutoDrag;
              Exit;
            end;
            Include(FControlState,csLButtonDown);
          end;
          LM_LBUTTONUP:
             Begin
               Exclude(FControlState, csLButtonDown);
               if Dragging then DragObject.MouseMsg(Message);
             end;
        end;
      end
      else begin
        if Message.MSg = CM_VISIBLECHANGED
        then with Message do SendDockNotification(Msg,WParam,LParam);
      end;
    end;
  end;
{debug purposes}
//Assert(False, 'Trace:TCONTROL.WNDPROC');
//Assert(False, Format('Trace:Control = %s     -->Message = %d',[CLASSNAME,Message.msg]));
  Dispatch(Message);
end;

{------------------------------------------------------------------------------}
{       TControl SendDockNotification                                 }
{------------------------------------------------------------------------------}
Procedure SendDockNotification(Msg: Cardinal; WParam, LParam : Integer);
Begin
//TODO: SendDockNotification
end;

{------------------------------------------------------------------------------}
{       TControl Invalidate                                 }
{------------------------------------------------------------------------------}
procedure TControl.Invalidate;
Begin
  InvalidateControl(Visible, csOpaque in ControlStyle);
end;


{------------------------------------------------------------------------------}
{       TControl DoMouseDown  "Event Handler"                                 }
{------------------------------------------------------------------------------}
procedure TControl.DoMouseDown(var Message: TLMMouse; Button: TMOuseButton; Shift:TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
  Begin
    with Message do MouseDown(Button, KeysToShiftState(Keys) + Shift, XPos, YPos);
  end;
end;

{------------------------------------------------------------------------------}
{       TControl DoMouseUp  "Event Handler"                                     }
{------------------------------------------------------------------------------}
procedure TControl.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle)
  then with Message do MouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonDown
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonDown(var Message: TLMLButtonDown);
begin
  if csCaptureMouse in ControlStyle then MouseCapture := True;
  if csClickEvents in ControlStyle then Include(FControlState, csClicked);
  DoMouseDown(Message, mbLeft, []);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonDown
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonDown(var Message: TLMRButtonDown);
begin
  DoMouseDown(Message, mbRight, []);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMMButtonDown
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMMButtonDown(var Message: TLMMButtonDown);
begin
  DoMouseDown(Message, mbMiddle, []);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonDblClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonDblClk(var Message: TLMLButtonDblClk);
begin
//TODO: SendCancelMode(self);
if csCaptureMouse in ControlStyle then MouseCapture := True;
if csClickEvents in ControlStyle then DblClick;
DoMouseDown(Message, mbleft ,[ssDouble]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonDblClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonDblClk(var Message: TLMRButtonDblClk);
begin
DoMouseDown(Message, mbRight ,[ssDouble]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMMButtonDblClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMMButtonDblClk(var Message: TLMMButtonDblClk);
begin
DoMouseDown(Message, mbMiddle ,[ssDouble]);
end;


{------------------------------------------------------------------------------
  Method: TControl.WMLButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonUp(var Message: TLMLButtonUp);
begin
  if csCaptureMouse in ControlStyle then
     MouseCapture := False;

  if csClicked in ControlState then
  begin
    Exclude(FControlState, csClicked);
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos))
    then begin
      Click;
    end;
  end;
  DoMouseUp(Message, mbLeft);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonUp(var Message: TLMRButtonUp);
begin
//  inherited;
  DoMouseUp(Message, mbRight);
  if Message.Result = 0 then CheckMenuPopup(Message.pos);

end;

{------------------------------------------------------------------------------
  Method: TControl.WMMButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMMButtonUp(var Message: TLMMButtonUp);
begin
  DoMouseUp(Message, mbMiddle);
end;

{------------------------------------------------------------------------------}
{       TControl Click                                                         }
{------------------------------------------------------------------------------}
Procedure TControl.Click;
Begin
   if Assigned (FOnClick) then FOnClick(Self);
end;

{------------------------------------------------------------------------------}
{  TControl AddControl                                                         }
{------------------------------------------------------------------------------}
procedure TControl.AddControl;
begin
   CNSendMessage(LM_AddChild, Self, nil);
end;

{------------------------------------------------------------------------------}
{       TControl SetAutoSize                                                   }
{------------------------------------------------------------------------------}
Procedure TControl.SetAutoSize(value : Boolean);
Begin
  if FAutoSize <> value then
  FAutosize := Value;
//TODO: Finish this by calling gtk and telling it to resize...?
end;

{------------------------------------------------------------------------------}
{       TControl SetBoundsRect                                                 }
{------------------------------------------------------------------------------}
Procedure TControl.SetBoundsRect(const Rect : TRect);
Begin
{$IFDEF CHECK_POSITION}
writeln('[TControl.SetBoundsRect] ',Name,':',ClassName);
{$ENDIF}
  with Rect do
    SetBounds(Left,Top,Right - Left, Bottom - Top);
end;

{------------------------------------------------------------------------------}
{  TControl SetCursor                                                          }
{------------------------------------------------------------------------------}
procedure TControl.SetCursor(Value: TCursor);
begin
  if FCursor <> Value 
  then begin
    FCursor := Value;
    // This should not be called if it is already set to VALUE but if
    // it's not created when it's set, and you set it again it skips this,
    // so for now I do it this way.
    // later, I'll create the cursor in the CreateComponent
    // (or something like that)
    CNSendMessage(LM_SetCursor,Self,Nil);
  end;

end;

{------------------------------------------------------------------------------}
{  TControl SetEnabled                                                         }
{------------------------------------------------------------------------------}
procedure TControl.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value 
  then begin
    FEnabled := Value;
    Perform(CM_ENABLEDCHANGED, 0, 0);
  end;

end;

{------------------------------------------------------------------------------}
{  TControl SetMouseCapture                                                    }
{------------------------------------------------------------------------------}
procedure TControl.SetMouseCapture(Value : Boolean);
begin
  if MouseCapture <> Value
  then begin
    if Value 
    then SetCaptureControl(Self) 
    else SetCaptureControl(nil);
  end
end;

{------------------------------------------------------------------------------
   Method:  TControl.SetHint
   Params:  Value: the text of the hint to be set
   Returns: Nothing
 
   Sets the hint text of a control
 ------------------------------------------------------------------------------}
procedure TControl.SetHint(const Value: String);
begin
  if FHint <> Value then FHint := Value;
end;

{------------------------------------------------------------------------------}
{  TControl SetName                                                            }
{------------------------------------------------------------------------------}
procedure TControl.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  ChangeText := (csSetCaption in ControlStyle) and
    not (csLoading in ComponentState) and (Name = Text) and
    ((Owner = nil) or not (Owner is TControl) or
     not (csLoading in TControl(Owner).ComponentState));

  inherited SetName(Value);
  if ChangeText then Text := Value;
end;

{------------------------------------------------------------------------------}
{  TControl Show                                                               }
{------------------------------------------------------------------------------}
procedure TControl.Show;
begin
  if Parent <> nil then Parent.ShowControl(Self);
  if not(csDesigning in ComponentState) or (csNoDesignVisible in ControlStyle) then
    Visible:= true;
end;

{------------------------------------------------------------------------------}
{  TControl Notification                                                       }
{------------------------------------------------------------------------------}
procedure TControl.Notification( AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = PopupMenu then PopupMenu := nil;
end;


{------------------------------------------------------------------------------}
{  TControl GetText                                                            }
{------------------------------------------------------------------------------}
function TControl.GetText: TCaption;
begin
  Assert(False, 'Trace:[TControl.GetText]');

  if (Self is TWinControl)
  and TWinControl(Self).HandleAllocated
  and InterfaceObject.GetText(Self, Result)
  then Assert(False, Format('Trace:[TControl.GetText] %s got: "%s"', [ClassName, Result]))
  else Result := FCaption;

  Assert(False, 'Trace:<TControl.GetText> End');
end;

{------------------------------------------------------------------------------}
{  TControl IsCaptionStored                                                    }
{------------------------------------------------------------------------------}
Function TControl.IsCaptionStored : Boolean;
Begin
result := true;
end;

{------------------------------------------------------------------------------}
{  TControl InvalidateControl                                                            }
{------------------------------------------------------------------------------}
procedure TControl.InvalidateControl(IsVisible, IsOpaque : Boolean);
//var
//  Rect : TRect;
begin
  if (IsVisible or (csDesigning in ComponentState)
  and not (csNoDesignVisible in ControlStyle))
  and (Parent <> nil) and (Parent.HandleAllocated) then
    Begin
     CNSendMessage(LM_REDRAW,Self,Nil);
//    Rect := BoundsRect;
//    InvalidateRect(parent.handle,@Rect, True);
    end;
end;

{------------------------------------------------------------------------------}
{  TControl Refresh                                                            }
{------------------------------------------------------------------------------}
procedure TControl.Refresh;
begin
  Repaint;
end;

{------------------------------------------------------------------------------}
{  TControl Repaint                                                            }
{------------------------------------------------------------------------------}
procedure TControl.Repaint;
var
 DC: HDC;
begin
  if (Visible or (csDesigning in ComponentState) and
    not (csNoDesignVisible in ControlStyle)) and (Parent <> nil) and
    Parent.HandleAllocated then
    if csOpaque in ControlStyle then
    begin
      DC := GetDC(Parent.Handle);
      try
        IntersectClipRect(DC, Left, Top, Left + Width, Top + Height);
        Parent.PaintControls(DC, Self);
      finally
        ReleaseDC(Parent.Handle, DC);
      end;
    end else
    begin
      Invalidate;
      Update;
    end;
end;

{------------------------------------------------------------------------------}
{  TControl Resize                                                            }
{------------------------------------------------------------------------------}
procedure TControl.Resize;
begin
//writeln('[TControl.Resize] ',ClassName);
  if (csLoading in ComponentState) then exit;
  if (FLastResizeWidth<>Width) or (FLastResizeHeight<>Height) then begin
    FLastResizeWidth:=Width;
    FLastResizeHeight:=Height;
    if Assigned(FOnResize) then FOnResize(Self);
  end;
end;

{------------------------------------------------------------------------------}
{  TControl SetBounds                                                          }
{------------------------------------------------------------------------------}
procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight : integer);
begin
  ChangeBounds(ALeft, ATop, AWidth, AHeight);
end;

{------------------------------------------------------------------------------}
{  TControl SetConstraints                                                     }
{------------------------------------------------------------------------------}
procedure TControl.SetConstraints(const Value : TSizeConstraints);
begin
  FConstraints.Assign(Value);
end;

{------------------------------------------------------------------------------}
{  TControl SetAlign                                                           }
{------------------------------------------------------------------------------}
procedure TControl.SetAlign(Value: TAlign);
begin
  if FAlign <> Value then begin
    FAlign := Value;
    RequestAlign;
  end;
end;

{------------------------------------------------------------------------------}
{  TControl RequestAlign                                                       }
{------------------------------------------------------------------------------}
procedure TControl.RequestAlign;
begin
  if (Parent <> nil) then begin
    Parent.AlignControl(Self);
  end;
end;

{------------------------------------------------------------------------------}
{  TControl SetDragmode                                                            }
{------------------------------------------------------------------------------}
procedure TControl.SetDragMode(Value: TDragMode);
begin
  FDragMode := Value;
  CNSendMessage(LM_DRAGINFOCHANGED,Self,Nil);
end;

{------------------------------------------------------------------------------}
{  TControl SetLeft                                                            }
{------------------------------------------------------------------------------}
procedure TControl.SetLeft(Value: Integer);
begin
{$IFDEF CHECK_POSITION}
writeln('[TControl.SetLeft] ',Name,':',ClassName,' ',Value);
{$ENDIF}
  SetBounds(Value, FTop, FWidth, FHeight);
end;

{------------------------------------------------------------------------------}
{  TControl SetTop                                                             }
{------------------------------------------------------------------------------}
procedure TControl.SetTop(Value: Integer);
begin
{$IFDEF CHECK_POSITION}
writeln('[TControl.SetTop] ',Name,':',ClassName,' ',Value);
{$ENDIF}
  SetBounds(FLeft, Value, FWidth, FHeight);
end;

{------------------------------------------------------------------------------}
{  TControl SetWidth                                                           }
{------------------------------------------------------------------------------}
procedure TControl.SetWidth(Value: Integer);
begin
{$IFDEF CHECK_POSITION}
writeln('[TControl.SetWidth] ',Name,':',ClassName,' ',Value);
{$ENDIF}
  SetBounds(FLeft, FTop, Value, FHeight);
end;

{------------------------------------------------------------------------------}
{  TControl SetHeight                                                          }
{------------------------------------------------------------------------------}
procedure TControl.SetHeight(Value: Integer);
begin
{$IFDEF CHECK_POSITION}
writeln('[TControl.SetHeight] ',Name,':',ClassName,' ',Value);
{$ENDIF}
  SetBounds(FLeft, FTop, FWidth, Value);
end;

{------------------------------------------------------------------------------}
{  TControl SetParent                                                                 }
{------------------------------------------------------------------------------}
Procedure TControl.SetParent(AParent : TWinControl);
begin
{  if AParent = nil
  then Assert(False, Format('Trace:[TControl.SetParent] %s --> Parent: nil', [ClassName]))
  else Assert(False, Format('Trace:[TControl.SetParent] %s --> Parent: %s', [ClassName, AParent.ClassName]));
}
  if FParent <> AParent
  then begin
    if AParent = Self
    then begin
      Assert(False, 'Trace:[TControl.SetParent] EInvalidOperation --> FParent = Self');
      raise EInvalidOperation.Create('A control can''t have itself as parent');
    end;

    if FParent <> nil then FParent.RemoveControl(Self);
    if AParent <> nil then AParent.InsertControl(Self);
  end;


end;


{------------------------------------------------------------------------------}
{  TControl SetParentComponent                                                 }
{------------------------------------------------------------------------------}
Procedure TControl.SetParentComponent(Value : TComponent);
Begin
if (Value is TWinControl) then Setparent(TWinControl(Value));
end;

{------------------------------------------------------------------------------}
{  TControl SetParentShowHint                                                                 }
{------------------------------------------------------------------------------}
Procedure TControl.SetParentShowHint(Value : Boolean);
Begin
  if FParentShowHint <> Value 
  then begin
    FParentShowHint := Value;
    //Sendmessage to stop/start hints for parent
  end;
end;

{------------------------------------------------------------------------------}
{  TControl SetPopupMenu                                                                 }
{------------------------------------------------------------------------------}
procedure TControl.SetPopupMenu(Value : TPopupMenu);
begin
  FPopupMenu :=  Value;
{    If Value <> nil then
  begin

  end;
}
end;

{------------------------------------------------------------------------------}
{  TControl WMDragStart
}
{------------------------------------------------------------------------------}
Procedure TControl.WMDragStart(Var Message: TLMessage);
Begin
//do this here?
BeginDrag(true);
end;


{------------------------------------------------------------------------------}
{  TControl WMMouseMove
}
{------------------------------------------------------------------------------}
Procedure TControl.WMMouseMove(Var Message: TLMMouseMove);
Begin
  if not (csNoStdEvents in COntrolStyle)
  then with Message do
       MouseMove(KeystoShiftState(Keys), XPos, YPos);
End;

{------------------------------------------------------------------------------}
{  TControl MouseDown
}
{------------------------------------------------------------------------------}

Procedure TControl.MouseDown(Button: TMouseButton; Shift:TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMOuseDown) then FOnMOuseDOwn(Self, Button, Shift, X,Y);
end;

{------------------------------------------------------------------------------}
{  TControl MouseMove
}
{------------------------------------------------------------------------------}

Procedure TControl.MouseMove(Shift:TShiftState; X, Y: Integer);
begin
if Assigned(FOnMOuseMove) then FOnMOuseMove(Self, Shift, X,Y);
end;

{------------------------------------------------------------------------------}
{  TControl MouseUp
}
{------------------------------------------------------------------------------}

Procedure TControl.MouseUp(Button: TMouseButton; Shift:TShiftState; X, Y: Integer);
begin
if Assigned(FOnMOuseUp) then FOnMOuseUp(Self, Button, Shift, X,Y);
end;

{------------------------------------------------------------------------------}
{  TControl SetShowHint
}
{------------------------------------------------------------------------------}
procedure TControl.SetShowHint(Value : Boolean);
begin
   if FShowHint <> Value then
   begin
      FShowHint := Value;
      FParentShowHint := False;
      Perform(CM_SHOWHINTCHANGED, 0, 0);
   end;
end;

{------------------------------------------------------------------------------}
{  TControl SetVisible
}
{------------------------------------------------------------------------------}
procedure TControl.SetVisible(Value : Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Perform(CM_VISIBLECHANGED, Ord(Value), 0);
    RequestAlign;
  end;
end;

{------------------------------------------------------------------------------}
{       TControl.SetZOrder
}
{------------------------------------------------------------------------------}
Procedure TControl.SetZOrder(Topmost : Boolean);
var AParent : TWinControl;
    AControl : TControl;
begin
//if FParent <> nil then
//     if Topmost then SetZOrderPosition(FParent.FControls.Count-1)
//   else
//   SetZOrderPosition(0);
  if Parent <> nil then begin
    AParent:= Parent;
    { Just reinsert the control on top. Don't if it already is }
    if Topmost then begin
      if (AParent.Controls[AParent.ControlCount - 1] <> Self) then begin
        AParent.RemoveControl(Self);
        AParent.InsertControl(Self);
      end;
    end else begin
      { Move all other controls over this one }
      if (AParent.Controls[0] <> Self) then begin
        AParent.RemoveControl(Self);
        AParent.InsertControl(Self);
        while AParent.Controls[0] <> Self do begin
          AControl:= AParent.Controls[0];
  	  AParent.RemoveControl(AControl);
	  AParent.InsertControl(AControl);
	end;  
      end;
    end;
  end;    
end;

{------------------------------------------------------------------------------}
{       TControl.SetZOrderPosition
}
{------------------------------------------------------------------------------}
Procedure TControl.SetZOrderPosition(Position : Integer);
Var
  I : Integer;
  Count : Integer;
begin
if FParent <> nil then
   Begin
    I := FParent.FControls.Indexof(self);
    if I >= 0 then
       begin
         Count := FParent.FControls.Count;
         if Position < 0 then Position := 0;
         if Position >= Count then Position := Count-1;
         if Position <> I then
            begin
            FParent.FControls.Delete(i);
            FParent.FControls.Insert(Position,Self);
            InvalidateControl(Visible,True);
            end;
        end;
   end;
end;

{------------------------------------------------------------------------------
  Method: TControl.GetTextBuf
  Params:  None
  Returns: Nothing

 ------------------------------------------------------------------------------}
function TControl.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
var
  S: string;
begin
  S := GetText;
  if BufSize<=0 then exit;
  if length(S) >= BufSize
  then begin
    StrPCopy(Buffer, copy(S, 1, BufSize-1));
    Result := BufSize - 1;
  end else begin
    StrPCopy(Buffer, S);
    Result := length(S);
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.SetTextBuf
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
Procedure TControl.SetTextBuf(Buffer : PChar);
Begin
  CNSendMessage(LM_SetLabel, Self, Buffer);
  Perform(CM_TEXTCHANGED,0,0);
end;

{------------------------------------------------------------------------------}
{  TControl Update                                                           }
{------------------------------------------------------------------------------}
procedure TControl.Update;    //pbd
begin
     // Todo
end;

{------------------------------------------------------------------------------}
{  TControl SetText                                                            }
{------------------------------------------------------------------------------}
procedure TControl.SetText(const Value: TCaption);
begin
  if GetText <> value
  then begin
    //  Need to set FCaption otherwise those components that simply
    //  check FCaption will always be wrong.
    FCaption := Value;

    if Self is TWinControl then
      SetTextBuf(PChar(FCaption));
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TControl.Destroy;
begin
//writeln('[TControl.Destroy] A ',Name,':',ClassName);
  Application.ControlDestroyed(Self);
  SetParent(nil);
  FFont.Free;
//writeln('[TControl.Destroy] B ',Name,':',ClassName);
  inherited Destroy;
//writeln('[TControl.Destroy] END ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method: TControl.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TControl.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
  FConstraints:= TSizeConstraints.Create(Self);
  FConstraints.OnChange:= @DoConstraintsChange;
  
  FAnchors := [akLeft,akTop];
  FAlign := alNone;
  // initial control isn't visible (It isn't created yet)
  FVisible := False;
//  FOwner := AOwner;
  FParentShowHint := True;
  FWindowProc := @WndProc;
  FCursor := crDefault;
  FFont := TFont.Create;
  //FFont.OnChange := @FontChanged;
  FIsControl := False;
  FEnabled := True; {Default}
end;

{------------------------------------------------------------------------------}
{  TControl Create Component                                                   }
{------------------------------------------------------------------------------}
procedure TControl.CreateComponent(AOwner : TComponent);
begin
  CNSendMessage(LM_CREATE, Self, nil);
end;

{------------------------------------------------------------------------------}
{  TControl DoEvents                                                           }
{------------------------------------------------------------------------------}
procedure TControl.DoEvents;
begin
  // used to make sure that events are handled while in long loops.
  InterfaceObject.DoEvents;
end;

{------------------------------------------------------------------------------}
{  TControl Destroy Component                                                  }
{------------------------------------------------------------------------------}
procedure TControl.DestroyComponent;
begin
  CNSendMessage(LM_DESTROY, Self, nil);
end;

{------------------------------------------------------------------------------}
{  TControl SetCallback                                                        }
{------------------------------------------------------------------------------}
procedure TControl.SetCallback(Msg : LongInt);
begin
   InterfaceObject.SetCallback(Msg, Self);
end;

{------------------------------------------------------------------------------}
{  TControl RemoveCallbacks                                                                 }
{------------------------------------------------------------------------------}
procedure TControl.RemoveCallbacks;
begin
  InterfaceObject.RemoveCallbacks(Self);
end;

{------------------------------------------------------------------------------
  Method:  TControl.GetDeviceContext
  Params:  WindowHandle: the windowhandle of this control
  Returns: a Devicecontext

  Get the devicecontext of the parent Wincontrol for this Control.
 ------------------------------------------------------------------------------}
function TControl.GetDeviceContext(var WindowHandle: HWnd): HDC;
begin
  if Parent = nil
  then raise EInvalidOperation.CreateFmt('Control ''%s'' has no parent window', [Name]);

  Result := Parent.GetDeviceContext(WindowHandle);
end;

{------------------------------------------------------------------------------
  Method:  TControl.SendToBack
  Params:  None
  Returns: Nothing

  Puts a control back in Z-order behind all other controls
 ------------------------------------------------------------------------------}
procedure TControl.SendToBack;
begin
  SetZOrder(false);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMWindowPosChanged
  Params:   Msg: The message
  Returns:  nothing

  event handler.
  
 ------------------------------------------------------------------------------}
procedure TControl.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  Assert(False, Format('Trace:[TControl.WMWindowPosChanged] %s', [ClassName]));
  // TODO : Docksites and constraints
  
  { Do not handle this message and leave it to WMSize and WMMove }
  Message.Result:= 0;
  
  //if Message.WindowPos <> nil then with Message.WindowPos^ do begin
  //   SetBounds(X, Y, cX, cY);
  //   Message.Result:= 1;
  //end;
end;

{------------------------------------------------------------------------------
  Method: TControl.WMSize
  Params:   Msg: The message
  Returns:  nothing

  event handler.
  
  Message.SizeType=Size_Restored is the default. All other values will force a
  ReAlign.
 ------------------------------------------------------------------------------}
procedure TControl.WMSize(Var Message : TLMSize);
begin
  if (Message.SizeType = Size_Restored) 
    and (FWidth = Message.Width) and (FHeight = Message.Height) then exit;

{$IFDEF CHECK_POSITION}
writeln('[TControl.WMSize] Name=',Name,':',ClassName,' Message.Width=',Message.Width,' Message.Height=',Message.Height,' Width=',Width,' Height=',Height);
{$ENDIF}
  Assert(False, Format('Trace:[TWinControl.WMSize] %s', [ClassName]));

  SetBounds(Left, Top, Message.Width, Message.Height);
  if not (csLoading in ComponentState) then Resize;
end;

{------------------------------------------------------------------------------
  Method: TControl.WMMove
  Params:   Msg: The message
  Returns:  nothing

  event handler.
  
  Message.MoveType=0 is the default, all other values will force a RequestAlign.
 ------------------------------------------------------------------------------}
procedure TControl.WMMove(var Message: TLMMove);
begin
  if (Message.MoveType = 0)
  and (FLeft = Message.XPos) and (FTop = Message.YPos) then
    Exit;
{$IFDEF CHECK_POSITION}
writeln('[TControl.WMMove] Name=',Name,':',ClassName,' Message.XPos=',Message.XPos,' Message.YPos=',Message.YPos,' OldLeft=',Left,' OldTop=',Top);
{$ENDIF}
  { Just sync the coordinates }
  SetBounds(Message.XPos, Message.YPos, Width, Height);
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

{ =============================================================================
  $Log$
  Revision 1.38  2002/03/16 21:40:54  lazarus
  MG: reduced size+move messages between lcl and interface

  Revision 1.37  2002/03/14 23:25:52  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.36  2002/03/14 18:12:46  lazarus
  Mouse events fixes.

  Revision 1.35  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)


  Revision 1.34  2002/03/09 02:03:59  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.33  2002/03/08 11:37:42  lazarus
  MG: outputfilter can now find include files

  Revision 1.32  2002/01/01 18:38:36  lazarus
  MG: more wmsize messages :(

  Revision 1.31  2002/01/01 15:50:14  lazarus
  MG: fixed initial component aligning

  Revision 1.30  2001/12/08 08:54:45  lazarus
  MG: added TControl.Refresh

  Revision 1.29  2001/11/10 10:48:00  lazarus
  MG: fixed set formicon on invisible forms

  Revision 1.28  2001/10/31 16:29:21  lazarus
  Fixed the gtk mousemove bug where the control gets the coord's based on it's parent instead of itself.
  Shane

  Revision 1.27  2001/10/16 20:01:28  lazarus
  MG: removed splashform fix, because of the unpredictable side effects

  Revision 1.26  2001/10/16 14:19:13  lazarus
  MG: added nvidia opengl support and a new opengl example from satan

  Revision 1.25  2001/10/07 07:28:33  lazarus
  MG: fixed setpixel and TCustomForm.OnResize event

  Revision 1.24  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.23  2001/08/07 11:05:51  lazarus
  MG: small bugfixes

  Revision 1.22  2001/06/28 18:15:03  lazarus
  MG: bugfixes for destroying controls

  Revision 1.21  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.20  2001/05/13 22:07:08  lazarus
  Implemented BringToFront / SendToBack.

  Revision 1.19  2001/04/02 14:45:26  lazarus
  MG: bugfixes for TBevel

  Revision 1.18  2001/03/27 21:12:53  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.17  2001/03/21 23:48:29  lazarus
  MG: fixed window positions

  Revision 1.16  2001/03/19 14:00:50  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.15  2001/02/20 16:53:27  lazarus
  Changes for wordcompletion and many other things from Mattias.
  Shane

  Revision 1.14  2001/02/06 20:59:16  lazarus
  Trying to get the last control of the last form focused when a dialog closes.
  Still working on it.
  Shane

  Revision 1.11  2001/02/04 04:18:12  lazarus
  Code cleanup and JITFOrms bug fix.
  Shane

  Revision 1.10  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.9  2001/01/09 18:23:20  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.8  2001/01/05 18:56:23  lazarus
  Minor changes

  Revision 1.7  2000/12/29 18:33:54  lazarus
  TStatusBar's create and destroy were not set to override TWinControls so they were never called.
  Shane

  Revision 1.6  2000/12/29 13:14:05  lazarus
  Using the lresources.pp and registering components.
  This is a major change but will create much more flexibility for the IDE.
  Shane

  Revision 1.5  2000/12/22 19:55:37  lazarus
  Added the Popupmenu code to the LCL.
  Now you can right click on the editor and a PopupMenu appears.
  Shane

  Revision 1.4  2000/11/30 21:43:38  lazarus
  Changed TDesigner.  It's now notified when a control is added to it's CustomForm.
  It's created in main.pp when New Form is selected.

  Shane

  Revision 1.3  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.2  2000/07/30 21:48:32  lazarus
  MWE:
    = Moved ObjectToGTKObject to GTKProc unit
    * Fixed array checking in LoadPixmap
    = Moved LM_SETENABLED to API func EnableWindow and EnableMenuItem
    ~ Some cleanup

  Revision 1.1  2000/07/13 10:28:25  michael
  + Initial import

  Revision 1.20  2000/06/28 13:11:37  lazarus
  Fixed TNotebook so it gets page change events.  Shane

  Revision 1.19  2000/06/19 18:21:21  lazarus
  Spinedit was never getting created
  Shane

  Revision 1.18  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.17  2000/06/14 16:10:36  lazarus
  Took out some unneeded code in control.inc

  Revision 1.16  2000/06/14 16:09:09  lazarus
  Added the start for the ability to move controls.
  Shane

  Revision 1.15  2000/05/27 22:20:55  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.14  2000/05/17 22:34:07  lazarus
  MWE:
    * Fixed Sizing & events

  Revision 1.13  2000/05/14 21:56:11  lazarus
  MWE:
    + added local messageloop
    + added PostMessage
    * fixed Peekmessage
    * fixed ClientToScreen
    * fixed Flat style of Speedutton (TODO: Draw)
    + Added TApplicatio.OnIdle

  Revision 1.12  2000/05/10 22:52:57  lazarus
  MWE:
    = Moved some global api stuf to gtkobject

  Revision 1.11  2000/05/09 12:52:03  lazarus
  *** empty log message ***

  Revision 1.10  2000/05/09 02:07:40  lazarus
  Replaced writelns with Asserts.                          CAW

  Revision 1.9  2000/05/08 16:07:32  lazarus
  fixed screentoclient and clienttoscreen
  Shane

  Revision 1.8  2000/05/08 15:56:58  lazarus
  MWE:
    + Added support for mwedit92 in Makefiles
    * Fixed bug # and #5 (Fillrect)
    * Fixed labelsize in ApiWizz
    + Added a call to the resize event in WMWindowPosChanged

  Revision 1.7  2000/04/18 21:03:14  lazarus
  Added
  TControl.bringtofront
  Shane

  Revision 1.6  2000/04/18 14:02:32  lazarus
  Added Double Clicks.  Changed the callback in gtkcallback for the buttonpress event to check the event type.
  Shane

  Revision 1.5  2000/04/17 19:50:06  lazarus
  Added some compiler stuff built into Lazarus.
  This depends on the path to your compiler being correct in the compileroptions
  dialog.
  Shane

  Revision 1.4  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.3  2000/04/10 15:05:30  lazarus
  Modified the way the MOuseCapture works.
  Shane

  Revision 1.2  2000/04/07 16:59:54  lazarus
  Implemented GETCAPTURE and SETCAPTURE along with RELEASECAPTURE.
  Shane

  Revision 1.1  2000/04/02 20:49:55  lazarus
  MWE:
    Moved lazarus/lcl/*.inc files to lazarus/lcl/include

  Revision 1.79  2000/03/30 18:07:53  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.78  2000/03/23 22:48:56  lazarus
  MWE & Hans-Joachim Ott <hjott@compuserve.com>:
    + added replacement for LM_GetText

  Revision 1.77  2000/03/23 20:40:03  lazarus
  Added some drag code
  Shane

  Revision 1.76  2000/03/22 20:40:43  lazarus
  Added dragobject shell

  Revision 1.75  2000/03/21 18:53:28  lazarus
  Added code for TBitBtn. Not finished but looks like mostly working.
  Shane

  Revision 1.74  2000/03/20 21:12:00  lazarus
  *** empty log message ***

  Revision 1.73  2000/03/15 20:15:31  lazarus
  MOdified TBitmap but couldn't get it to work
  Shane

  Revision 1.72  2000/03/15 00:51:57  lazarus
  MWE:
    + Added LM_Paint on expose
    + Added forced creation of gdkwindow if needed
    ~ Modified DrawFrameControl
    + Added BF_ADJUST support on DrawEdge
    - Commented out LM_IMAGECHANGED in TgtkObject.IntSendMessage3
       (It did not compile)

  Revision 1.71  2000/03/14 19:49:04  lazarus
  Modified the painting process for TWincontrol.  Now it runs throug it's FCONTROLS list and paints all them
  Shane

  Revision 1.70  2000/03/10 18:31:09  lazarus
  Added TSpeedbutton code
  Shane

  Revision 1.69  2000/03/08 23:57:38  lazarus
  MWE:
    Added SetSysColors
    Fixed TEdit text bug (thanks to hans-joachim ott <hjott@compuserve.com>)
    Finished GetKeyState
    Added changes from Peter Dyson <peter@skel.demon.co.uk>
    - a new GetSysColor
    - some improvements on ExTextOut

  Revision 1.68  2000/03/06 00:05:05  lazarus
  MWE: Added changes from Peter Dyson <peter@skel.demon.co.uk> for a new
    release of mwEdit (0.92)

  Revision 1.67  2000/03/01 00:41:02  lazarus
  MWE:
    Fixed updateshowing problem
    Added some debug code to display the name of messages
    Did a bit of cleanup in main.pp to get the code a bit more readable
      (my editor does funny things with tabs if the indent differs)

  Revision 1.66  2000/02/28 00:15:54  lazarus
  MWE:
    Fixed creation of visible componets at runtime. (when a new editor
      was created it didn't show up)
    Made the hiding/showing of controls more delphi compatible

  Revision 1.65  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.64  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.63  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.62  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.61  2000/02/21 21:08:29  lazarus
  Bug fix in GetCaption.  Added the line to check if a handle is allocated for a csEdit.   Otherwise when creating it, it check's it's caption.  It then sends a LM_GETTEXT and the edit isn't created, so it calls LM_CREATE which in turn checks the caption again, etc.
  Shane

  Revision 1.60  2000/02/20 20:13:47  lazarus
  On my way to make alignments and stuff work :-)

  Revision 1.59  2000/02/19 18:11:58  lazarus
  More work on moving, resizing, forms' border style etc.

  Revision 1.58  2000/02/18 19:38:52  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.57  2000/01/31 20:00:21  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.56  2000/01/18 21:47:00  lazarus
  Added OffSetRec

  Revision 1.55  2000/01/17 23:33:06  lazarus
  MWE:
    fixed: nil pointer reference in DeleteObject
    fixed: some trace info didn't start with 'trace:'

  Revision 1.54  2000/01/14 15:01:15  lazarus
  Changed SETCURSOR so the cursor's were created in the gtkObject.Init and destroyed in GTkObject.AppTerminate
  Shane

  Revision 1.53  2000/01/11 20:50:32  lazarus
  Added some code for SETCURSOR.  Doesn't work perfect yet but getting there.
  Shane

  Revision 1.52  2000/01/07 21:14:13  lazarus
  Added code for getwindowlong and setwindowlong.
  Shane

  Revision 1.51  2000/01/04 21:00:34  lazarus
  *** empty log message ***

  Revision 1.50  2000/01/03 00:19:20  lazarus
  MWE:
    Added keyup and buttonup events
    Added LM_MOUSEMOVE callback
    Started with scrollbars in editor

  Revision 1.49  1999/12/31 14:58:00  lazarus
  MWE:
    Set unkown VK_ codesto 0
    Added pfDevice support for bitmaps

  Revision 1.48  1999/12/23 21:48:13  lazarus
  *** empty log message ***

  Revision 1.46  1999/12/21 00:07:06  lazarus
  MWE:
    Some fixes
    Completed a bit of DraWEdge

  Revision 1.45  1999/12/20 21:01:13  lazarus
  Added a few things for compatability with Delphi and TToolbar
  Shane

  Revision 1.44  1999/12/18 18:27:31  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.43  1999/12/14 21:16:26  lazarus
  Added Autosize to TControl
  Shane

  Revision 1.42  1999/12/14 21:07:12  lazarus
  Added more stuff for TToolbar
  Shane

  Revision 1.41  1999/12/14 16:41:55  lazarus
  Minor changes because of conflicts
  Shane

  Revision 1.40  1999/12/14 00:16:43  lazarus
  MWE:
    Renamed LM... message handlers to WM... to be compatible and to
      get more edit parts to compile
    Started to implement GetSystemMetrics
    Removed some Lazarus specific parts from mwEdit

  Revision 1.39  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.38  1999/12/08 21:42:36  lazarus
  Moved more messages over to wndproc.
  Shane

  Revision 1.37  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.36  1999/12/07 01:19:25  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.35  1999/11/30 21:30:06  lazarus
  Minor Issues
  Shane

  Revision 1.34  1999/11/23 22:06:27  lazarus
  Minor changes to get it running again with the latest compiler.  There is something wrong with the compiler that is preventing certain things from working.
  Shane

  Revision 1.33  1999/11/17 01:16:39  lazarus
  MWE:
    Added some more API stuff
    Added an initial TBitmapCanvas
    Added some DC stuff
    Changed and commented out, original gtk linedraw/rectangle code. This
      is now called through the winapi wrapper.

  Revision 1.32  1999/11/04 21:52:08  lazarus
  wndproc being used a little
  Shane

  Revision 1.31  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.30  1999/10/30 16:33:28  lazarus
  MWE: Added check when setiing Parent := self

  Revision 1.29  1999/10/28 23:48:57  lazarus
  MWE: Added new menu classes and started to use handleneeded

  Revision 1.28  1999/10/28 19:25:09  lazarus
  Added a ton of messaging stuff
  Shane

  Revision 1.27  1999/10/28 17:17:41  lazarus
  Removed references to FCOmponent.
  Shane

  Revision 1.26  1999/10/27 17:27:07  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.25  1999/10/27 13:11:51  lazarus
  Added some LM_??? stuff to LMEssages.
  Shane

  Revision 1.24  1999/10/26 19:50:56  lazarus
  Added TControl.wndProc
  Shane

  Revision 1.23  1999/10/25 21:07:49  lazarus
  Many changes for compatability made again..

  Shane

  Revision 1.22  1999/10/25 15:33:54  lazarus
  Added a few more procedures for compatability.
  Shane

  Revision 1.21  1999/10/22 21:08:59  lazarus
  Moved TEXTMETRICS to WINDOWS.PP
  Shane

  Revision 1.20  1999/10/22 18:52:42  lazarus
  Added OnDragDrop and OnDragOver stuff.

  Revision 1.19  1999/10/22 18:39:43  lazarus
  Added kEYUP- KeyPress - Keydown, etc.

  Shane

  Revision 1.18  1999/10/21 21:33:29  lazarus
  Made many changes to the Messages and LMessages units
  Shane

  Revision 1.15  1999/09/25 17:10:21  lazarus
  Modified TEDIT to give the correct text when you use Edit1.Text
  Thanks to Ned Boddie for noticing the error and sending the fix.

  Revision 1.14  1999/09/22 20:07:14  lazarus
  *** empty log message ***

  Revision 1.13  1999/09/21 23:46:53  lazarus
  *** empty log message ***

  Revision 1.12  1999/08/26 23:36:01  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.11  1999/08/17 13:20:34  lazarus
  Added a dynamic procedure called CLICK in TCOntrol

  Revision 1.10  1999/08/16 15:48:47  lazarus
  Changes by file:
       Control: TCOntrol-Function GetRect added
                         ClientRect property added
                TImageList - Added Count
                TWinControl- Function Focused added.
      Graphics: TCanvas - CopyRect added - nothing finished on it though
                          Draw added - nothing finiushed on it though
                clbtnhighlight and clbtnshadow added.  Actual color values not right.
               IMGLIST.PP and IMGLIST.INC files added.

   A few other minor changes for compatability added.

    Shane

  Revision 1.9  1999/08/12 18:36:53  lazarus
  Added a bunch of "stuff" for compatablility.  Not sure if it'll all compile yet, will look at that shortly.

  Revision 1.8  1999/08/11 20:41:29  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.7  1999/08/07 17:59:11  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

  Revision 1.6  1999/08/01 00:06:14  lazarus
  Alignement Changes CEB

  Revision 1.5  1999/07/31 06:39:17  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
