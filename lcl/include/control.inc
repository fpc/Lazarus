{******************************************************************************
                                     TControl
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

  Starts the dragging of a control. If the Immidiate flag is set, dragging
  starts immediate.
 ------------------------------------------------------------------------------}
procedure TControl.BeginDrag(Immediate: Boolean; Threshold: Integer);
var
  P : TPoint;
begin
  if (DragControl = nil) or (Pointer(DragControl) = Pointer($FFFFFFFF)) then
  Begin
    DragControl := nil;
    if csLButtonDown in ControlState then
    Begin
      GetCursorPos(p);
      P := ScreenToClient(p);
      Perform(LM_LBUTTONUP,0,Longint(PointToSmallPoint(p)));
    end;
    if Threshold < 0 then
      Threshold := Mouse.DragThreshold;
    if Pointer(DragControl) <> Pointer($FFFFFFFF) then
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
  BeginDrag(Mouse.DragImmediate,Mouse.DragThreshold);
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
{       TControl.CanTab
}
{------------------------------------------------------------------------------}
Function TControl.CanTab: Boolean;
begin
  Result := False;
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
var
  SizeChanged, PosChanged : boolean;
begin
  DoConstrainedResize(AWidth, AHeight);
  SizeChanged:= (FWidth <> AWidth) or (FHeight <> AHeight);
  PosChanged:= (FLeft <> ALeft) or (FTop <> ATop);
  if SizeChanged or PosChanged then begin

    {If AutoSize and not AutoSizing then begin
      If SizeChanged then begin
        DoAutoSize;
        AutoSizing := True;
        If PosChanged then
          ChangeBounds(ALeft, ATop, Width, Height);
        AutoSizing := False;
        exit;
      end;
    end;}

    FLastResize.X:= AWidth - FWidth;
    FLastResize.Y:= AHeight - FHeight;

    //writeln('TControl.ChangeBounds A ',Name,':',ClassName);
    InvalidateControl(Visible, False, true);
    //writeln('TControl.ChangeBounds B ',Name,':',ClassName);
    DoSetBounds(ALeft,ATop,AWidth,AHeight);

    if SizeChanged then
      Invalidate;
//    UpdateAnchorRules;
    BoundsChanged;
    RequestAlign;
    if (not (csLoading in ComponentState)) then begin
      Resize;
      SendMoveSizeMessages(SizeChanged,PosChanged);
    end;
    If Parent <> nil then
      Parent.DoAutoSize;
  end;
end;

{-------------------------------------------------------------------------------
  TControl.DoSetBounds
  Params: ALeft, ATop, AWidth, AHeight : integer
  
  store bounds in private variables
-------------------------------------------------------------------------------}
procedure TControl.DoSetBounds(ALeft, ATop, AWidth, AHeight : integer);
begin
  FLeft:= ALeft;
  FTop:= ATop;
  FWidth:= AWidth;
  FHeight:= AHeight;
end;

{------------------------------------------------------------------------------}
{       TControl.ChangeScale
}
{------------------------------------------------------------------------------}
Procedure TControl.ChangeScale(M,D : Integer);
Begin
  // TODO: TCONTROL.CHANGESCALE
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
      P2 := SmallPointToPoint(P);
      P2 := ClientToScreen(P2);
      TempPopupMenu.Popup(P2.X, P2.Y);
      Exit;
    end;
    Control := Control.Parent;
  end;
end;

{------------------------------------------------------------------------------}
{       TControl.Focused                                                       }
{------------------------------------------------------------------------------}
Function TControl.Focused : Boolean;
Begin
  Result := False;
end;

{------------------------------------------------------------------------------}
{       TControl.SetFocus                                                      }
{------------------------------------------------------------------------------}
procedure TControl.SetFocus;
begin
  //Implemented by TWinControl, or other descendent
end;

procedure TControl.SetTabStop(Value : Boolean);
begin
  If FTabStop = Value then
    exit;
    
  FTabStop := Value;
end;

{------------------------------------------------------------------------------}
{       TControl.GetClientHeight                                               }
{------------------------------------------------------------------------------}
function TControl.GetClientHeight: Integer;
begin
  Result := ClientRect.Bottom;
end;

{------------------------------------------------------------------------------}
{       TControl.GetClientWidth                                                }
{------------------------------------------------------------------------------}
function TControl.GetClientWidth: Integer;
begin
  Result := ClientRect.Right;
end;

function TControl.IsHelpContextStored: boolean;
begin
  Result:=false;
end;

{------------------------------------------------------------------------------}
{       TControl GetTabOrder                                                   }
{------------------------------------------------------------------------------}
Function TControl.GetTabOrder : TTabOrder;
Begin
  If Parent <> nil then
    Result := ListIndexOf(Parent.FTabList, Self)
  else
    Result := -1;
end;

{------------------------------------------------------------------------------}
{       TControl SetTabOrder                                                   }
{------------------------------------------------------------------------------}
Procedure TControl.SetTabOrder(Value : TTabOrder);
Begin
  UpdateTabOrder(Value);
end;

{------------------------------------------------------------------------------}
{  TControl UpdateTabOrder                                                     }
{------------------------------------------------------------------------------}
Procedure TControl.UpdateTabOrder(Value : TTabOrder);
var
  CurentOrder,
  OrderCount    : Integer;
begin
  If (Parent = nil) or not CanTab then
    exit;
  CurentOrder := GetTabOrder;
  If CurentOrder >= 0 then begin
    OrderCount := ListCount(Parent.FTabList);
    If (Value < 0) or (Value >= OrderCount) then
      Value := OrderCount - 1;
    If Value <> CurentOrder then begin
      ListRemove(Parent.FTabList, Self);
      ListInsert(Parent.FTabList, Value,Self);
    end;
  end
  else
    ListAdd(Parent.FTabList, Self);
end;

{------------------------------------------------------------------------------}
{       TControl.LMCaptureChanged                                              }
{------------------------------------------------------------------------------}
Procedure TControl.LMCaptureChanged(Var Message: TLMessage);
Begin
  //Writeln('[LMCaptureChanged for '+Name+':'+Classname+']');
End;

{------------------------------------------------------------------------------}
{       TControl.CMENABLEDCHANGED                                              }
{------------------------------------------------------------------------------}
procedure TControl.CMEnabledChanged(var Message: TLMEssage);
begin
  Invalidate;
end;

{------------------------------------------------------------------------------}
{       TControl.CMHITTEST                                                     }
{------------------------------------------------------------------------------}
procedure TControl.CMHITTEST(var Message : TCMHitTest);
begin
  Message.Result := 1;
end;

{------------------------------------------------------------------------------
       TControl.CMMouseEnter
------------------------------------------------------------------------------}
Procedure TControl.CMMouseEnter(var Message :TLMessage);
Begin
  // this is a LCL based mouse message, so don't call DoBeforeMouseMessage
  if not FMouseEntered then begin
    FMouseEntered:=true;
    if Assigned(OnMouseEnter) then OnMouseEnter(Self);
    if FParent <> nil then
      FParent.Perform(CM_MOUSEENTER,0,longint(Self));
  end;
end;

{------------------------------------------------------------------------------
       TControl.CMMouseLeave
------------------------------------------------------------------------------}
Procedure TControl.CMMouseLeave(var Message :TLMessage);
Begin
  // this is a LCL based mouse message, so don't call DoBeforeMouseMessage
  if FMouseEntered then begin
    FMouseEntered:=false;
    if Assigned(OnMouseLeave) then OnMouseLeave(Self);
    if FParent <> nil then
      FParent.Perform(CM_MOUSELEAVE,0,longint(Self));
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.CMHintShow(var Message: TLMessage);
------------------------------------------------------------------------------}
procedure TControl.CMHintShow(var Message: TLMessage);
begin
  DoOnShowHint(TCMHintShow(Message).HintInfo);
  if (ActionLink <> nil)
  and not ActionLink.DoShowHint(TCMHintShow(Message).HintInfo^.HintStr)
  then
    Message.Result := 1;
end;

{------------------------------------------------------------------------------
       TControl.CMVisibleChanged
------------------------------------------------------------------------------}
procedure TControl.CMVisibleChanged(var Message : TLMessage);
begin
  if (not (csDesigning in ComponentState)
    or (csNoDesignVisible in ControlStyle))
  and (not (csLoading in ComponentState)) then begin
    InvalidateControl(true, FVisible and (csOpaque in ControlStyle),true);
  end;
end;

{------------------------------------------------------------------------------}
{       TControl.ConstrainedResize                                             }
{------------------------------------------------------------------------------}
procedure TControl.ConstrainedResize(var MinWidth, MinHeight,
  MaxWidth, MaxHeight : TConstraintSize);
begin
  if Assigned(FOnConstrainedResize) then
    FOnConstrainedResize(Self, MinWidth, MinHeight, MaxWidth, MaxHeight);
end;

{------------------------------------------------------------------------------
  function TControl.GetPalette: HPalette;
------------------------------------------------------------------------------}
function TControl.GetPalette: HPalette;
begin
  Result:=0;
end;

{------------------------------------------------------------------------------
  procedure TControl.DoBeforeMouseMessage;
------------------------------------------------------------------------------}
procedure TControl.DoBeforeMouseMessage;
begin
  if Application<>nil then
    Application.DoBeforeMouseMessage(Self);
end;

{------------------------------------------------------------------------------
       TControl.DoConstrainedResize
------------------------------------------------------------------------------}
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
  
  if (MinWidth > 0) and (NewWidth < MinWidth) then
    NewWidth:= MinWidth
  else if (MaxWidth > 0) and (NewWidth > MaxWidth) then
    NewWidth:= MaxWidth;
  
  if (MinHeight > 0) and (NewHeight < MinHeight) then
    NewHeight:= MinHeight
  else if (MaxHeight > 0) and (NewHeight > MaxHeight) then
    NewHeight:= MaxHeight;
end;

{------------------------------------------------------------------------------}
{       TControl.DoConstraintsChange					       }
{------------------------------------------------------------------------------}
procedure TControl.DoConstraintsChange(Sender : TObject);
begin
  AdjustSize;
end;

{------------------------------------------------------------------------------
  procedure TControl.SendMoveSizeMessages(SizeChanged, PosChanged: boolean);
------------------------------------------------------------------------------}
procedure TControl.SendMoveSizeMessages(SizeChanged, PosChanged: boolean);
begin

end;

{------------------------------------------------------------------------------
       TControl.DragCanceled
------------------------------------------------------------------------------}
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

Function TControl.PerformTab : Boolean;

  Function TopLevelAncestor(TopControl : TControl) : TWinControl;
  begin
    Result := nil;

    If TopControl = nil then
      exit;

    If TopControl is TForm then
      Result := TForm(TopControl)
    else
      Result := TopLevelAncestor(TopControl.Parent);
  end;

var
  I : Integer;
  List : TList;
  FirstFocus, OFocus, NFocus : TControl;
  TopLevel : TWinControl;
begin
  NFocus := nil;
  OFocus := nil;
  TopLevel := TopLevelAncestor(Self);
  If TopLevel = nil then
    exit;
  try
    List := TList.Create;
    TopLevel.GetTabOrderList(List);
    FirstFocus := nil;
    For I := 0 to List.Count - 1 do
      If List[I] <> nil then begin
        If I = 0 then
          FirstFocus := TControl(List[I]);
        If TControl(List[I]).Focused then begin
          OFocus := TControl(List[I]);
          Break;
        end;
      end;
  Finally
    List.Free;
  end;

  NFocus := TopLevel.FindNextControl(OFocus,True,True,False);
  If NFocus = OFocus then begin
    Result := True;
    exit;
  end;
  If (NFocus <> nil) then begin
    NFocus.SetFocus;
    Result := NFocus.Focused;
  end
  else
    If FirstFocus <> nil then begin
      FirstFocus.SetFocus;
      Result := FirstFocus.Focused;
    end;
end;

{------------------------------------------------------------------------------}
{       TControl.GetClientOrigin
}
{------------------------------------------------------------------------------}
function TControl.GetClientOrigin: TPoint;
Begin
  Assert(False, Format('Trace:[TControl.GetClientOrigin] %s',  [Classname]));
  if Parent = nil then
    raise Exception.Create('TControl.GetClientOrigin: Parent=nil for '
                           +Name+':'+ClassName);
    //raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
  Result := Parent.ClientOrigin;
  Inc(Result.X, FLeft);
  Inc(Result.Y, FTop);
  Assert(False, Format('Trace:[TControl.GetClientOrigin] %s --> (%d, %d)',  [Classname, Result.X, Result.Y]));
end;

{------------------------------------------------------------------------------}
{       TControl.ScreenToClient
}
{------------------------------------------------------------------------------}
Function TControl.ScreenToClient(const Point : TPoint) : TPoint;
var
  P : TPoint;
begin
  P := ClientOrigin;
  Result.X := Point.X - P.X;
  Result.Y := Point.Y - P.Y;
  Assert(False, Format('Trace:[TControl.ScreenToCLient] %s: Point(%d, %d) Org(%d, %d) --> (%d, %d)',  [Classname, Point.x,  point.y, p.x, p.y, result.x, result.y]));
end;

{------------------------------------------------------------------------------
  Function TControl.ClientToScreen(const Point : TPoint) : TPoint;
  
------------------------------------------------------------------------------}
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
  If Assigned(FOnDblClick) then FOnDblClick(Self);
end;

{------------------------------------------------------------------------------}
{       TControl.TripleClick
}
{------------------------------------------------------------------------------}
procedure TControl.TripleClick;
begin
  If Assigned(FOnTripleClick) then FOnTripleClick(Self);
end;

{------------------------------------------------------------------------------}
{       TControl.QuadClick
}
{------------------------------------------------------------------------------}
procedure TControl.QuadClick;
begin
  If Assigned(FOnQuadClick) then FOnQuadClick(Self);
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
  if FColor <> Value then
  begin
    FColor := Value;
    FParentColor := False;
    CNSendMessage(LM_SETCOLOR, Self, nil);
  end;
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
  Result.Left := FLeft;
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

{------------------------------------------------------------------------------
  procedure TControl.DoOnShowHint(var HintInfo: THintInfo);
------------------------------------------------------------------------------}
procedure TControl.DoOnShowHint(HintInfo: Pointer);
begin
  if Assigned(OnShowHint) then
    OnShowHint(Self,HintInfo);
end;

{------------------------------------------------------------------------------
       TControl GetClientRect
------------------------------------------------------------------------------}
function TControl.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

{------------------------------------------------------------------------------
  function TControl.GetScrolledClientRect: TRect;

------------------------------------------------------------------------------}
function TControl.GetScrolledClientRect: TRect;
var
  ScrolledOffset: TPoint;
begin
  Result:=GetClientRect;
  ScrolledOffset:=GetClientScrollOffset;
  inc(Result.Left,ScrolledOffset.X);
  inc(Result.Top,ScrolledOffset.Y);
  inc(Result.Right,ScrolledOffset.X);
  inc(Result.Bottom,ScrolledOffset.Y);
end;

{------------------------------------------------------------------------------
  function TControl.GetChildsRect(Scrolled: boolean): TRect;
  
  Returns the Client rectangle relative to the controls left, top.
  If Scrolled is true, the rectangle is moved by the current scrolling values
  (for an example see TScrollingWincontrol).
------------------------------------------------------------------------------}
function TControl.GetChildsRect(Scrolled: boolean): TRect;
var
  ScrolledOffset: TPoint;
begin
  Result:=ClientRect;
  if Scrolled then begin
    ScrolledOffset:=GetClientScrollOffset;
    inc(Result.Left,ScrolledOffset.X);
    inc(Result.Top,ScrolledOffset.Y);
    inc(Result.Right,ScrolledOffset.X);
    inc(Result.Bottom,ScrolledOffset.Y);
  end;
end;

{------------------------------------------------------------------------------
  function TControl.GetClientScrollOffset: TPoint;

  Returns the scrolling offset of the client area.
------------------------------------------------------------------------------}
function TControl.GetClientScrollOffset: TPoint;
begin
  Result:=Point(0,0);
end;

{------------------------------------------------------------------------------
       TControl WndPRoc
------------------------------------------------------------------------------}
procedure TControl.WndProc(var TheMessage : TLMessage);
Var
  Form : TCustomForm;
begin
  //writeln('CCC TControl.WndPRoc ',Name,':',ClassName);
  if (csDesigning in ComponentState) then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil)
    and Form.Designer.IsDesignMsg(Self,TheMessage) then begin
      Exit;
    end;
  end
  else
  begin
    if (TheMessage.Msg >= LM_KeyFirst) and (TheMessage.Msg <= LM_KeyLast) then
    begin
      Form := GetParentForm(Self);
      if (Form <> nil) and (Form.WantChildKey(Self,TheMessage)) then exit;
    end
    else
    begin
      if ((TheMessage.Msg>=LM_MOUSEFIRST) and (TheMessage.Msg<=LM_MOUSELAST))
      or ((TheMessage.Msg>=LM_MOUSEFIRST2) and (TheMessage.Msg<=LM_MOUSELAST2))
      then
      begin
        // map double clicks for controls, that do not want doubleclicks
        if not (csDoubleClicks in ControlStyle) then
        begin
          case TheMessage.Msg of
            LM_LButtonDBLCLK,
            LM_RButtonDBLCLK,
            LM_MButtonDBLCLK:
              Dec(TheMessage.Msg, LM_LBUTTONDBLCLK - LM_LBUTTONDOWN);
          end;
        end;
        // map triple clicks for controls, that do not want tripleclicks
        if not (csTripleClicks in ControlStyle) then
        begin
          case TheMessage.Msg of
            LM_LBUTTONTRIPLECLK: TheMessage.Msg:=LM_LBUTTONDOWN;
            LM_MBUTTONTRIPLECLK: TheMessage.Msg:=LM_MBUTTONDOWN;
            LM_RBUTTONTRIPLECLK: TheMessage.Msg:=LM_RBUTTONDOWN;
          end;
        end;
        // map quad clicks for controls, that do not want quadclicks
        if not (csQuadClicks in ControlStyle) then
        begin
          case TheMessage.Msg of
            LM_LBUTTONQUADCLK: TheMessage.Msg:=LM_LBUTTONDOWN;
            LM_MBUTTONQUADCLK: TheMessage.Msg:=LM_MBUTTONDOWN;
            LM_RBUTTONQUADCLK: TheMessage.Msg:=LM_RBUTTONDOWN;
          end;
        end;

        case TheMessage.Msg of
        
          LM_MOUSEMOVE:
            begin
              Application.HintMouseMessage(Self, TheMessage);
              if Dragging then DragObject.MouseMsg(TheMessage);
            end;
            
          LM_LBUTTONDOWN,
          LM_LBUTTONDBLCLK:
            begin
              if FDragMode = dmAutomatic
              then begin
                Assert(False, 'Trace:Begin AutoDrag called');
                BeginAutoDrag;
                Exit;
              end;
              Include(FControlState,csLButtonDown);
            end;
            
          LM_LBUTTONUP:
            begin
              Exclude(FControlState, csLButtonDown);
              if Dragging then DragObject.MouseMsg(TheMessage);
            end;
        end;
      end
      else begin
        if TheMessage.Msg = CM_VISIBLECHANGED
        then begin
          with TheMessage do SendDockNotification(Msg,WParam,LParam);
	end;    
      end;
    end;
  end;
{debug purposes}
//Assert(False, 'Trace:TCONTROL.WNDPROC');
//Assert(False, Format('Trace:Control = %s     -->Message = %d',[CLASSNAME,Message.msg]));
  Dispatch(TheMessage);
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
procedure TControl.DoMouseDown(var Message: TLMMouse; Button: TMOuseButton;
  Shift:TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
  Begin
    with Message do
      MouseDown(Button, KeysToShiftState(Keys) + Shift, XPos, YPos);
  end;
end;

{------------------------------------------------------------------------------}
{       TControl DoMouseUp  "Event Handler"                                     }
{------------------------------------------------------------------------------}
procedure TControl.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle)
  then with Message do
    MouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonDown
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonDown(var Message: TLMLButtonDown);
begin
  {$IFDEF VerboseMouseBugfix}
  Writeln('TCONTROL WMLBUTTONDOWN A ',Name,':',ClassName);
  {$ENDIF}
  DoBeforeMouseMessage;
  if csCaptureMouse in ControlStyle then MouseCapture := True;
  if csClickEvents in ControlStyle then Include(FControlState, csClicked);
  DoMouseDown(Message, mbLeft, []);
  //Writeln('TCONTROL WMLBUTTONDOWN B ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonDown
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonDown(var Message: TLMRButtonDown);
begin
  DoBeforeMouseMessage;
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
  DoBeforeMouseMessage;
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
  DoBeforeMouseMessage;
  //TODO: SendCancelMode(self);
  if csCaptureMouse in ControlStyle then MouseCapture := True;
  if csClickEvents in ControlStyle then DblClick;
  DoMouseDown(Message, mbLeft ,[ssDouble]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonDblClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonDblClk(var Message: TLMRButtonDblClk);
begin
  DoBeforeMouseMessage;
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
  DoBeforeMouseMessage;
  DoMouseDown(Message, mbMiddle ,[ssDouble]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonTripleClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonTripleClk(var Message: TLMLButtonTripleClk);
begin
  DoBeforeMouseMessage;
  //TODO: SendCancelMode(self);
  if csCaptureMouse in ControlStyle then MouseCapture := True;
  if csClickEvents in ControlStyle then TripleClick;
  DoMouseDown(Message, mbLeft ,[ssTriple]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonTripleClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonTripleClk(var Message: TLMRButtonTripleClk);
begin
  DoBeforeMouseMessage;
  DoMouseDown(Message, mbRight ,[ssTriple]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMMButtonTripleClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMMButtonTripleClk(var Message: TLMMButtonTripleClk);
begin
  DoBeforeMouseMessage;
  DoMouseDown(Message, mbMiddle ,[ssTriple]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonQuadClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonQuadClk(var Message: TLMLButtonQuadClk);
begin
  DoBeforeMouseMessage;
  //TODO: SendCancelMode(self);
  if csCaptureMouse in ControlStyle then MouseCapture := True;
  if csClickEvents in ControlStyle then QuadClick;
  DoMouseDown(Message, mbLeft ,[ssQuad]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonQuadClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonQuadClk(var Message: TLMRButtonQuadClk);
begin
  DoBeforeMouseMessage;
  DoMouseDown(Message, mbRight ,[ssQuad]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMMButtonQuadClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMMButtonQuadClk(var Message: TLMMButtonQuadClk);
begin
  DoBeforeMouseMessage;
  DoMouseDown(Message, mbMiddle ,[ssQuad]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonUp(var Message: TLMLButtonUp);
begin
  DoBeforeMouseMessage;
  //Writeln('TControl.WMLButtonUp A ',Name,':',ClassName,' csCaptureMouse=',csCaptureMouse in ControlStyle,' csClicked=',csClicked in ControlState);
  if csCaptureMouse in ControlStyle then
    MouseCapture := False;

  if csClicked in ControlState then
  begin
    Exclude(FControlState, csClicked);
    //writeln('TControl.WMLButtonUp B ',ClientRect.Left,',',ClientRect.Top,',',ClientRect.Right,',',ClientRect.Bottom,' ',Message.Pos.X,',',Message.Pos.Y);
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos))
    then begin
      //writeln('TControl.WMLButtonUp C');
      Click;
    end;
  end;
  DoMouseUp(Message, mbLeft);
  //Writeln('TControl.WMLButtonUp END');
end;

{------------------------------------------------------------------------------
  Method: TControl.WMRButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMRButtonUp(var Message: TLMRButtonUp);
begin
  DoBeforeMouseMessage;
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
  DoBeforeMouseMessage;
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

{------------------------------------------------------------------------------
       TControl SetAutoSize
------------------------------------------------------------------------------}
Procedure TControl.SetAutoSize(const value : Boolean);
Begin
  If AutoSize <> Value then begin
    FAutoSize := Value;
    DoAutoSize;
  end;
end;

{------------------------------------------------------------------------------
       TControl DoAutoSize
------------------------------------------------------------------------------}
Procedure TControl.DoAutoSize;
Begin
  //Handled by TWinControl, or other descendants
end;

{------------------------------------------------------------------------------
       TControl SetBoundsRect
------------------------------------------------------------------------------}
Procedure TControl.SetBoundsRect(const Rect : TRect);
Begin
  {$IFDEF CHECK_POSITION}
  writeln('[TControl.SetBoundsRect] ',Name,':',ClassName);
  {$ENDIF}
  with Rect do
    SetBounds(Left,Top,Right - Left, Bottom - Top);
end;

{------------------------------------------------------------------------------
  TControl SetClientHeight
------------------------------------------------------------------------------}
procedure TControl.SetClientHeight(Value: Integer);
begin
  SetClientSize(Point(ClientWidth, Value));
end;

{------------------------------------------------------------------------------
  TControl SetClientSize
------------------------------------------------------------------------------}
procedure TControl.SetClientSize(Value: TPoint);
var
  Client: TRect;
begin
  Client := GetClientRect;
  SetBounds(FLeft, FTop, Width - Client.Right + Value.X, Height -
    Client.Bottom + Value.Y);
end;

{------------------------------------------------------------------------------}
{  TControl SetClientWidth                                                     }
{------------------------------------------------------------------------------}
procedure TControl.SetClientWidth(Value: Integer);
begin
  SetClientSize(Point(Value, ClientHeight));
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
    if not(csDesigning in ComponentState) then CNSendMessage(LM_SETCURSOR, Self, nil);
  end;
end;

procedure TControl.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
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
    {$IFDEF VerboseMouseCapture}
    writeln('TControl.SetMouseCapture ',Name,':',ClassName,' NewValue=',Value);
    {$ENDIF}
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
  if not (csDesigning in ComponentState)
  or (csNoDesignVisible in ControlStyle) then begin
    Visible := true;
  end;
end;

{------------------------------------------------------------------------------
  TControl Notification
------------------------------------------------------------------------------}
procedure TControl.Notification( AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = PopupMenu then PopupMenu := nil;
end;


{------------------------------------------------------------------------------
  TControl GetText
------------------------------------------------------------------------------}
function TControl.GetText: TCaption;
begin
  Assert(False, 'Trace:[TControl.GetText]');

  if (Self is TWinControl)
  and TWinControl(Self).HandleAllocated
  and (not (csLoading in ComponentState))
  and boolean(CNSendMessage(LM_GETTEXT, Self, @Result))
  then Assert(False, Format('Trace:[TControl.GetText] %s got: "%s"', [ClassName, Result]))
  else Result := FCaption;

  Assert(False, 'Trace:<TControl.GetText> End');
end;

{------------------------------------------------------------------------------
  TControl IsCaptionStored
------------------------------------------------------------------------------}
Function TControl.IsCaptionStored : Boolean;
Begin
  Result := true;
end;

{------------------------------------------------------------------------------
  TControl InvalidateControl
------------------------------------------------------------------------------}
procedure TControl.InvalidateControl(IsVisible, IsOpaque : Boolean);
var
  Rect: TRect;

  function BackgroundClipped: Boolean;
  var
    R: TRect;
    List: TList;
    I: Integer;
    C: TControl;
  begin
    Result := True;
    List := FParent.FControls;
    if List<>nil then begin
      I := List.IndexOf(Self);
      while I > 0 do
      begin
        Dec(I);
        C := TControl(List[I]);
        with C do
          if C.Visible and (csOpaque in ControlStyle) then
          begin
            IntersectRect(R, Rect, BoundsRect);
            if EqualRect(R, Rect) then Exit;
          end;
      end;
    end;
    Result := False;
  end;

begin
  if (IsVisible or (csDesigning in ComponentState)
  and not (csNoDesignVisible in ControlStyle))
  and (Parent <> nil) and Parent.HandleAllocated
  and (not (csLoading in Parent.ComponentState)) then
  begin
    Rect := BoundsRect;
    InvalidateRect(Parent.Handle, @Rect, not (IsOpaque or
      (csOpaque in Parent.ControlStyle) or BackgroundClipped));
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.InvalidateControl(IsVisible, IsOpaque,
    IgnoreWinControls: Boolean);
------------------------------------------------------------------------------}
procedure TControl.InvalidateControl(IsVisible, IsOpaque,
  IgnoreWinControls: Boolean);
begin
  if IgnoreWinControls and (Self is TWinControl) then exit;
  InvalidateControl(IsVisible,IsOpaque);
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
  if (Visible or (csDesigning in ComponentState)
  and not (csNoDesignVisible in ControlStyle))
  and (Parent <> nil)
  and Parent.HandleAllocated
  then
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

{------------------------------------------------------------------------------
  TControl Resize
  
  Calls OnResize
-------------------------------------------------------------------------------}
procedure TControl.Resize;
begin
  if (csLoading in ComponentState) then exit;
  if (FLastResizeWidth<>Width) or (FLastResizeHeight<>Height)
  or (FLastResizeClientWidth<>ClientWidth)
  or (FLastResizeClientHeight<>ClientHeight) then begin
    {writeln('[TControl.Resize] ',Name,':',ClassName,
    ' Last=',FLastResizeWidth,',',FLastResizeHeight,
    ' LastClient=',FLastResizeClientWidth,',',FLastResizeClientHeight,
    ' New=',Width,',',Height,
    ' NewClient=',ClientWidth,',',ClientHeight);}
    FLastResizeWidth:=Width;
    FLastResizeHeight:=Height;
    FLastResizeClientWidth:=ClientWidth;
    FLastResizeClientHeight:=ClientHeight;
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

procedure SetFont(Value: TFont);
begin

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

procedure TControl.SetHelpContext(const AValue: THelpContext);
begin
  if FHelpContext=AValue then exit;
  FHelpContext:=AValue;
end;

procedure TControl.SetHelpKeyword(const AValue: String);
begin
  if FHelpKeyword=AValue then exit;
  FHelpKeyword:=AValue;
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
  if (Value is TWinControl) then SetParent(TWinControl(Value));
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
  {$IFDEF VerboseMouseBugfix}
  writeln('[TControl.WMMouseMove] ',Name,':',ClassName);
  {$ENDIF}
  DoBeforeMouseMessage;
  if not (csNoStdEvents in ControlStyle)
  then with Message do
    MouseMove(KeystoShiftState(Keys), XPos, YPos);
End;

{------------------------------------------------------------------------------}
{  TControl MouseDown
}
{------------------------------------------------------------------------------}

Procedure TControl.MouseDown(Button: TMouseButton; Shift:TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X,Y);
end;

{------------------------------------------------------------------------------}
{  TControl MouseMove
}
{------------------------------------------------------------------------------}

Procedure TControl.MouseMove(Shift:TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X,Y);
end;

{------------------------------------------------------------------------------}
{  TControl MouseUp
}
{------------------------------------------------------------------------------}

Procedure TControl.MouseUp(Button: TMouseButton; Shift:TShiftState;
  X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X,Y);
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
  if (csLoading in ComponentState) then
    ControlState:=ControlState+[csVisibleSetInLoading];
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
Procedure TControl.SetZOrderPosition(Position: Integer);
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
        InvalidateControl(Visible,True,True);
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
  if GetText = Value then exit;
  //  Need to set FCaption otherwise those components that simply
  //  check FCaption will always be wrong.
  FCaption := Value;

  if Self is TWinControl then
    SetTextBuf(PChar(FCaption));
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
  FreeThenNil(FConstraints);
  FreeThenNil(FFont);
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
  //writeln('TControl.Create START ',Name,':',ClassName);
  inherited Create(AOwner);

  FControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks,
                    csOpaque];
  FConstraints:= TSizeConstraints.Create(Self);
  FConstraints.OnChange:= @DoConstraintsChange;
  
  FAnchors := [akLeft,akTop];
  FAlign := alNone;
  FColor := clWindow;
  FVisible := true;
  FParentShowHint := True;
  FWindowProc := @WndProc;
  FCursor := crDefault;
  FFont := TFont.Create;
  //FFont.OnChange := @FontChanged;
  FIsControl := False;
  FEnabled := True;
  TabStop := False;
  //writeln('TControl.Create END ',Name,':',ClassName);
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
  Method:  TControl.HasParent
  Params:  
  Returns: True - the item has a parent responsible for streaming

  This function will be called during streaming to decide if a component has
  to be streamed by it's owner or parent.
 ------------------------------------------------------------------------------}
function TControl.HasParent : Boolean;
begin
  Result := (FParent <> nil);
end;

{------------------------------------------------------------------------------
  function TControl.IsParentOf(AControl: TControl): boolean;

 ------------------------------------------------------------------------------}
function TControl.IsParentOf(AControl: TControl): boolean;
begin
  Result:=false;
  while AControl<>nil do begin
    AControl:=AControl.Parent;
    if Self=AControl then begin
      Result:=true;
      exit;
    end;
  end;
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
  Params:   Message : TLMSize
  Returns:  nothing

  event handler.
  
  Message.SizeType=Size_Restored is the default. All other values will result in
  a ReAlign.
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
  Revision 1.110  2002/12/27 17:12:37  mattias
  added more Delphi win32 compatibility functions

  Revision 1.109  2002/12/25 10:21:05  mattias
  made Form.Close more Delphish, added some windows compatibility functions

  Revision 1.108  2002/02/09 01:48:23  mattias
  renamed TinterfaceObject.Init to AppInit and TWinControls can now contain childs in gtk

  Revision 1.107  2002/12/04 20:39:14  mattias
  patch from Vincent: clean ups and fixed crash on destroying window

  Revision 1.106  2002/11/29 15:14:47  mattias
  replaced many invalidates by invalidaterect

  Revision 1.105  2002/11/27 14:37:37  mattias
  added form editor options for rubberband and colors

  Revision 1.104  2002/11/21 18:49:53  mattias
  started OnMouseEnter and OnMouseLeave

  Revision 1.103  2002/11/18 13:38:44  mattias
  fixed buffer overrun and added several checks

  Revision 1.102  2002/11/16 14:38:48  mattias
  fixed TControl.Show and Visible of designer forms

  Revision 1.101  2002/11/12 16:18:45  lazarus
  MG fixed hidden component page

  Revision 1.100  2002/11/09 15:02:06  lazarus
  MG: fixed LM_LVChangedItem, OnShowHint, small bugs

  Revision 1.99  2002/11/06 15:59:24  lazarus
  MG: fixed codetools abort

  Revision 1.98  2002/11/04 19:49:36  lazarus
  MG: added persistent hints for main ide bar

  Revision 1.97  2002/11/03 22:40:28  lazarus
  MG: fixed ControlAtPos

  Revision 1.96  2002/11/01 14:40:31  lazarus
  MG: fixed mouse coords on scrolling wincontrols

  Revision 1.95  2002/10/30 13:20:10  lazarus
  MG: fixed example

  Revision 1.94  2002/10/22 12:12:08  lazarus
  MG: accelerators are now shared between non modal forms

  Revision 1.93  2002/10/21 14:40:52  lazarus
  MG: fixes for 1.1

  Revision 1.92  2002/10/20 21:49:09  lazarus
  MG: fixes for fpc1.1

  Revision 1.91  2002/10/11 07:28:03  lazarus
  MG: gtk interface now sends keyboard events via DeliverMessage

  Revision 1.90  2002/10/09 10:22:54  lazarus
  MG: fixed client origin coordinates

  Revision 1.89  2002/10/08 22:32:26  lazarus
  MG: fixed cool little bug (menu double attaching bug)

  Revision 1.88  2002/09/29 15:08:38  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>
    Patch includes:
      -fixes Problems with hiding modal forms
      -temporarily fixes TCustomForm.BorderStyle in bsNone
      -temporarily fixes problems with improper tabbing in TSynEdit

  Revision 1.87  2002/09/27 20:52:23  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.86  2002/09/16 15:56:01  lazarus
  Resize cursors in designer.

  Revision 1.85  2002/09/10 06:49:19  lazarus
  MG: scrollingwincontrol from Andrew

  Revision 1.84  2002/09/09 19:04:01  lazarus
  MG: started TTreeView dragging

  Revision 1.83  2002/09/08 10:01:59  lazarus
  MG: fixed streaming visible=false

  Revision 1.82  2002/09/07 19:35:42  lazarus
  Visible property is by default true.

  Revision 1.81  2002/09/06 22:32:21  lazarus
  Enabled cursor property + property editor.

  Revision 1.80  2002/09/06 13:58:13  lazarus
  MG: added try for invalidate control

  Revision 1.79  2002/09/06 11:33:36  lazarus
  MG: added jitform error messagedlg

  Revision 1.78  2002/09/05 13:46:19  lazarus
  MG: activated InvalidateControl for TWinControls

  Revision 1.77  2002/09/05 12:11:43  lazarus
  MG: TNotebook is now streamable

  Revision 1.76  2002/09/03 20:02:01  lazarus
  Intermediate UI patch to show a bug.

  Revision 1.75  2002/09/03 08:40:53  lazarus
  MG: lazarus now requires the stable 1.0.6 fpc with ssTriple

  Revision 1.74  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.73  2002/09/02 19:10:28  lazarus
  MG: TNoteBook now starts with no Page and TPage has no auto names

  Revision 1.72  2002/09/01 16:11:21  lazarus
  MG: double, triple and quad clicks now works

  Revision 1.71  2002/08/31 11:37:09  lazarus
  MG: fixed destroying combobox

  Revision 1.70  2002/08/30 12:32:20  lazarus
  MG: MoveWindowOrgEx, Splitted FWinControls/FControls, TControl drawing, Better DesignerDrawing, ...

  Revision 1.69  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.68  2002/08/29 00:07:01  lazarus
  MG: fixed TComboBox and InvalidateControl

  Revision 1.67  2002/08/28 11:41:53  lazarus
  MG: activated environment opts in debugger

  Revision 1.66  2002/08/26 17:28:20  lazarus
  MG: fixed speedbutton in designmode

  Revision 1.65  2002/08/24 13:41:29  lazarus
  MG: fixed TSpeedButton.SetDown and Invalidate

  Revision 1.64  2002/08/24 12:57:32  lazarus
  MG: reduced output

  Revision 1.63  2002/08/24 12:54:59  lazarus
  MG: fixed mouse capturing, OI edit focus

  Revision 1.62  2002/08/22 16:22:39  lazarus
  MG: started debugging of mouse capturing

  Revision 1.61  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.60  2002/08/17 07:57:05  lazarus
  MG: added TPopupMenu.OnPopup and SourceEditor PopupMenu checks

  Revision 1.59  2002/08/05 08:56:56  lazarus
  MG: TMenuItems can now be enabled and disabled

  Revision 1.58  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.57  2002/07/09 17:18:22  lazarus
  MG: fixed parser for external vars

  Revision 1.56  2002/06/19 19:46:08  lazarus
  MG: Form Editing: snapping, guidelines, modified on move/resize, creating components in csDesigning, ...

  Revision 1.55  2002/06/04 15:17:22  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.54  2002/05/30 21:17:27  lazarus
  lcl/controls.pp

  Revision 1.53  2002/05/29 21:44:38  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.52  2002/05/24 07:16:31  lazarus
  MG: started mouse bugfix and completed Makefile.fpc

  Revision 1.51  2002/05/20 11:25:29  lazarus
  MG: readded ssTriple/ssQuad compiler directives

  Revision 1.50  2002/05/20 07:02:26  lazarus
  MG: removed 1_0_6 directives

  Revision 1.49  2002/05/13 15:26:13  lazarus
  MG: fixed form positioning when show, hide, show

  Revision 1.48  2002/05/10 06:05:51  lazarus
  MG: changed license to LGPL

  Revision 1.47  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.46  2002/04/24 16:11:17  lazarus
  MG: started new client rectangle

  Revision 1.45  2002/04/24 09:29:07  lazarus
  MG: fixed typos

  Revision 1.44  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.43  2002/04/21 06:53:55  lazarus
  MG: fixed save lrs to test dir

  Revision 1.42  2002/04/03 11:26:34  lazarus
  MG: fixed mem leaks

  Revision 1.41  2002/03/29 17:12:52  lazarus
  MG: added Triple and Quad mouse clicks to lcl and synedit

  Revision 1.40  2002/03/27 08:57:16  lazarus
  MG: reduced compiler warnings

  Revision 1.39  2002/03/25 17:59:20  lazarus
  GTK Cleanup
  Shane

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
