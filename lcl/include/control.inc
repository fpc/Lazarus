{%MainUnit ../controls.pp}

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

{------------------------------------------------------------------------------
  TControl.AdjustSize

  Calls DoAutoSize. This method tries to reduce this calls during loading and
  handle creation.
------------------------------------------------------------------------------}
procedure TControl.Adjustsize;
begin
  if not (csLoading in ComponentState) then DoAutoSize;
end;

{------------------------------------------------------------------------------
  Method: TControl.BeginDrag
  Params: Immediate: Drag behaviour
          Threshold: distance to move before dragging starts
                     -1 uses the default value of Mouse.DragThreshold
  Returns: Nothing

  Starts the dragging of a control. If the Immediate flag is set, dragging
  starts immediately.
 ------------------------------------------------------------------------------}
procedure TControl.BeginDrag(Immediate: Boolean; Threshold: Integer);
var
  P : TPoint;
begin
  // start a drag operation, if not already running
  if (DragControl = nil) then begin

    // if the last mouse down was not followed by a mouse up, simulate a
    // mouse up. This way applications need only to react to mouse up to
    // clean up.
    if csLButtonDown in ControlState then begin
      GetCursorPos(p);
      P := ScreenToClient(p);
      Perform(LM_LBUTTONUP, 0, LParam(PointToSmallPoint(p)));
    end;

    if Threshold < 0 then
      Threshold := Mouse.DragThreshold;
    DragInitControl(Self,Immediate,Threshold);
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.BeginDrag(Immediate: Boolean);
------------------------------------------------------------------------------}
procedure TControl.BeginDrag(Immediate: Boolean);
begin
  BeginDrag(Immediate, -1);
end;

{------------------------------------------------------------------------------
       TControl.BeginAutoDrag
------------------------------------------------------------------------------}
Procedure TControl.BeginAutoDrag;
begin
  BeginDrag(Mouse.DragImmediate,Mouse.DragThreshold);
end;

{------------------------------------------------------------------------------
  procedure TControl.DoEndDock(Target: TObject; X, Y: Integer);
------------------------------------------------------------------------------}
procedure TControl.DoEndDock(Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnEndDock) then
    FOnEndDock(Self,Target,X,Y);
end;

{------------------------------------------------------------------------------
  procedure TControl.DoDock(NewDockSite: TWinControl; var ARect: TRect);
------------------------------------------------------------------------------}
procedure TControl.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  if (NewDockSite = nil) then Parent := nil;
  if NewDockSite<>nil then begin
    DebugLn('TControl.DoDock BEFORE Adjusting ',Name,' ',dbgs(ARect));
    // adjust new bounds, so that they at least fit into the client area of
    // its parent
    LCLProc.MoveRectToFit(ARect,NewDockSite.ClientRect);
    // consider Align to increase chance the width/height is kept
    case Align of
    alLeft: OffsetRect(ARect,-ARect.Left,0);
    alTop: OffsetRect(ARect,0,-ARect.Top);
    alRight: OffsetRect(ARect,NewDockSite.ClientWidth-ARect.Right,0);
    alBottom: OffsetRect(ARect,0,NewDockSite.ClientHeight-ARect.Bottom);
    end;
    DebugLn('TControl.DoDock AFTER Adjusting ',Name,' ',dbgs(ARect),' Align=',AlignNames[Align],' NewDockSite.ClientRect=',dbgs(NewDockSite.ClientRect));
  end;
  //debugln('TControl.DoDock BEFORE MOVE ',Name,' BoundsRect=',dbgs(BoundsRect),' NewRect=',dbgs(ARect));
  if Parent<>NewDockSite then
    BoundsRectForNewParent := ARect
  else
    BoundsRect := ARect;
  debugln('TControl.DoDock AFTER MOVE ',Name,' BoundsRect=',dbgs(BoundsRect),' TriedRect=',dbgs(ARect));
end;

{------------------------------------------------------------------------------
  procedure TControl.DoStartDock(var DragObject: TDragObject);
------------------------------------------------------------------------------}
procedure TControl.DoStartDock(var DragObject: TDragObject);
begin
  if Assigned(FOnStartDock) then
    FOnStartDock(Self,TDragDockObject(DragObject));
end;

{------------------------------------------------------------------------------
  function TControl.GetDockEdge(const MousePos: TPoint): TAlign;

  Calculate the dock side depending on current MousePos.
  
  Important: MousePos is relative to this control's Left, Top.
------------------------------------------------------------------------------}
function TControl.GetDockEdge(const MousePos: TPoint): TAlign;
var
  BestDistance: Integer;

  procedure FindMinDistance(CurAlign: TAlign; CurDistance: integer);
  begin
    if CurDistance>=BestDistance then exit;
    Result:=CurAlign;
    BestDistance:=CurDistance;
  end;

begin
  // check if MousePos outside the control
  if MousePos.X<=0 then
    Result:=alLeft
  else if MousePos.Y<=0 then
    Result:=alTop
  else if MousePos.X>=Width then
    Result:=alRight
  else if MousePos.Y>=Height then
    Result:=alBottom
  else begin
    // MousePos is inside the control -> find nearest edge
    BestDistance:=MousePos.X;
    Result:=alLeft;
    FindMinDistance(alRight,Width-MousePos.X);
    FindMinDistance(alTop,MousePos.Y);
    FindMinDistance(alBottom,Height-MousePos.Y);
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.PositionDockRect(DragDockObject: TDragDockObject);
  
  
------------------------------------------------------------------------------}
procedure TControl.PositionDockRect(DragDockObject: TDragDockObject);
var
  NewWidth: LongInt;
  NewHeight: LongInt;
  NewLeft: LongInt;
  NewTop: LongInt;
  WinDragTarget: TWinControl;
begin
  with DragDockObject do begin
    if (DragTarget is TWinControl) and TWinControl(DragTarget).UseDockManager
    then begin
      WinDragTarget:=TWinControl(DragTarget);
      GetWindowRect(WinDragTarget.Handle, FDockRect);
      if (WinDragTarget.DockManager<>nil) then
        WinDragTarget.DockManager.PositionDockRect(Control,DropOnControl,
                                                   DropAlign,FDockRect);
    end else begin
      NewWidth := Control.UndockWidth;
      NewHeight := Control.UndockHeight;
      NewLeft := DragPos.X;
      NewTop := DragPos.Y;
      with FDockRect do
      begin
        Left := NewLeft;
        Top := NewTop;
        Right := Left + NewWidth;
        Bottom := Top + NewHeight;
      end;
      // let user adjust dock rect
      AdjustDockRect(FDockRect);
    end;
  end;
end;

{------------------------------------------------------------------------------
       TControl.BoundsChanged

------------------------------------------------------------------------------}
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

{------------------------------------------------------------------------------
       TControl.Change
------------------------------------------------------------------------------}
Procedure TControl.Changed;
Begin
  Perform(CM_CHANGED, 0, LParam(self));
End;

{------------------------------------------------------------------------------
  TControl.EditingDone
  
  Called when user has finished editing. This procedure can be used by data
  links to commit the changes.
  For example:
  - When focus switches to another control (default)
  - When user selected another item
  It's totally up to the control, what events will commit.
------------------------------------------------------------------------------}
procedure TControl.EditingDone;
begin
  if Assigned(OnEditingDone) then OnEditingDone(Self);
end;

procedure TControl.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TControl.SetAction(Value: TBasicAction);
begin
  if (Value=Action) then exit;
  //debugln('TControl.SetAction A ',Name,':',ClassName,' Old=',HexStr(Cardinal(Action),8),' New=',HexStr(Cardinal(Value),8));
  if Value = nil then begin
    ActionLink.Free;
    ActionLink:=nil;
    Exclude(FControlStyle, csActionClient);
  end
  else
  begin
    Include(FControlStyle, csActionClient);
    if ActionLink = nil then
      ActionLink := GetActionLinkClass.Create(Self);
    ActionLink.Action := Value;
    ActionLink.OnChange := @DoActionChange;
    ActionChange(Value, csLoading in Value.ComponentState);
    Value.FreeNotification(Self);
  end;
end;

{------------------------------------------------------------------------------
       TControl.ChangeBounds
------------------------------------------------------------------------------}
procedure TControl.ChangeBounds(ALeft, ATop, AWidth, AHeight : integer);
var
  SizeChanged, PosChanged : boolean;
  OldLeft: Integer;
  OldTop: Integer;
  OldWidth: Integer;
  OldHeight: Integer;
  CurBounds: TRect;
  NewBounds: TRect;

  function UpdatePosSizeChanged: boolean;
  begin
    SizeChanged:= (FWidth <> OldWidth) or (FHeight <> OldHeight);
    PosChanged:= (FLeft <> OldLeft) or (FTop <> OldTop);
    Result:=(not SizeChanged) and (not PosChanged);
  end;

begin
  {$IFDEF VerboseSizeMsg}
  DebugLn('TControl.ChangeBounds A ',Name,':',ClassName,
    ' Old='+dbgs(Left)+','+dbgs(Top)+','+dbgs(Width),',',dbgs(Height),
    ' New='+dbgs(ALeft)+','+dbgs(ATop)+','+dbgs(AWidth),',',dbgs(AHeight));
  {$ENDIF}
  // constraint the size
  DoConstrainedResize(AWidth, AHeight);

  // check, if we are already processing this bound change
  NewBounds:=Bounds(ALeft,ATop,AWidth,AHeight);
  if CompareRect(@FLastChangebounds,@NewBounds) then exit;
  FLastChangebounds:=NewBounds;

  OldLeft:=FLeft;
  OldTop:=FTop;
  OldWidth:=FWidth;
  OldHeight:=FHeight;

  // check if something would change
  SizeChanged:= (FWidth <> AWidth) or (FHeight <> AHeight);
  PosChanged:= (FLeft <> ALeft) or (FTop <> ATop);
  if (not SizeChanged) and (not PosChanged) then exit;

  //DebugLn('TControl.ChangeBounds A ',Name,':',ClassName);
  if (not (csLoading in ComponentState))
  and (not (Self is TWinControl)) then
    InvalidateControl(Visible, False, true);
  //DebugLn('TControl.ChangeBounds B ',Name,':',ClassName);
  DoSetBounds(ALeft,ATop,AWidth,AHeight);

  // change base bounds
  // (base bounds are the base for the automatic resizing)
  UpdateAnchorRules;
  // lock the base bounds while automatic resizing
  LockBaseBounds;
  // lock size messages
  inc(FSizeLock);
  try
    // lock the autosizing of the child controls
    if Self is TWinControl then
      TWinControl(Self).DisableAlign;
    try
      // resize parents client area
      If Parent <> nil then
        Parent.AdjustSize;
      if UpdatePosSizeChanged then exit;
      // notify before autosizing
      BoundsChanged;
      if UpdatePosSizeChanged then exit;
      //if csDesigning in ComponentState then
      //  DebugLn('TControl.ChangeBounds ',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);
      
      // autosize this control and its brothers
      RequestAlign;
      if UpdatePosSizeChanged then exit;
      // autosize childs
      if SizeChanged and (Self is TWinControl) then
        TWinControl(Self).ReAlign;
    finally
      // unlock the autosizing of the child controls
      // (this will autosize the childs)
      if Self is TWinControl then
        TWinControl(Self).EnableAlign;
    end;
  finally
    dec(FSizeLock);
    UnlockBaseBounds;
  end;
  if UpdatePosSizeChanged then exit;

  // send messages, if this is the top level call
  if FSizeLock>0 then exit;

  // invalidate
  if SizeChanged
  and (not (csLoading in ComponentState))
  and (not (Self is TWinControl)) then
    Invalidate;
  // notify user about resize
  if (not (csLoading in ComponentState)) then begin
    Resize;
    CurBounds:=BoundsRect;
    if not CompareRect(@FLastDoChangeBounds,@CurBounds) then begin
      FLastDoChangeBounds:=CurBounds;
      DoOnChangeBounds;
    end;
    // for delphi compatibility send size/move messages
    SendMoveSizeMessages(SizeChanged,PosChanged);
  end;
end;

{-------------------------------------------------------------------------------
  TControl.DoSetBounds
  Params: ALeft, ATop, AWidth, AHeight : integer

  store bounds in private variables
-------------------------------------------------------------------------------}
procedure TControl.DoSetBounds(ALeft, ATop, AWidth, AHeight : integer);

  procedure BoundsOutOfBounds;
  begin
    DebugLn('TControl.DoSetBounds ',Name,':',ClassName,
      ' Old=',dbgs(Left,Top,Width,Height),
      ' New=',dbgs(aLeft,aTop,aWidth,aHeight),
      '');
    RaiseGDBException('TControl.DoSetBounds '+Name+':'+ClassName+' Invalid bounds');
  end;

begin
  if (AWidth>100000) or (AHeight>100000) then
    BoundsOutOfBounds;
  {$IFDEF CHECK_POSITION}
  if csDesigning in ComponentState then
    DebugLn('TControl.DoSetBounds ',Name,':',ClassName,
      ' Old=',Left,',',Top,',',Width,',',Height,
      ' New=',aLeft,',',aTop,',',aWidth,',',aHeight,
      '');
  {$ENDIF}
  FLeft:= ALeft;
  FTop:= ATop;
  FWidth:= AWidth;
  FHeight:= AHeight;
  if Parent<>nil then Parent.InvalidatePreferredSize;
end;

{------------------------------------------------------------------------------
       TControl.ChangeScale

  Scale contorl by factor Multiplier/Divider
------------------------------------------------------------------------------}
Procedure TControl.ChangeScale(Multiplier, Divider: Integer);
Begin
  // TODO: TCONTROL.CHANGESCALE
  //Assert(False, 'Trace:TODO: [TControl.ChangeScale]');
end;

{------------------------------------------------------------------------------
       TControl.CheckMenuPopup
------------------------------------------------------------------------------}
Procedure TControl.CheckMenuPopup(const P: TSmallPoint);
var
  Control: TControl;
  TempPopupMenu: TPopupMenu;
  P2: TPoint;
  Handled: Boolean;
begin
  if csDesigning in ComponentState then Exit;

  P2 := SmallPointToPoint(P);
  Handled:=false;
  DoContextPopup(P2,Handled);
  if Handled then exit;

  Control := Self;
  while Control <> nil do
  begin
    TempPopupMenu := Control.GetPopupMenu;
    if (TempPopupMenu <> nil) then
    begin
      if not TempPopupMenu.AutoPopup then Exit;
//      SendCancelMode(nil);
      TempPopupMenu.PopupComponent := Control;
      P2 := ClientToScreen(P2);
      TempPopupMenu.Popup(P2.X, P2.Y);
      Exit;
    end;
    Control := Control.Parent;
  end;
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

{------------------------------------------------------------------------------
  procedure TControl.CalculateDockSizes;

  Compute docking width, height based on docking properties.
------------------------------------------------------------------------------}
procedure TControl.CalculateDockSizes;
begin
  if Floating then begin
    // the control is floating. Save Width and Height for undocking
    UndockHeight:=Height;
    UndockWidth:=Width;
  end
  else if HostDockSite<>nil then begin
    // the control is docked into a HostSite. That means some of it bounds
    // were maximized to fit into the HostSite.
    if (DockOrientation=doHorizontal)
    or (HostDockSite.Align in [alLeft,alRight]) then
      // the control is aligned left/right, that means its width is not
      // maximized. Save Width for docking.
      LRDockWidth:=Width
    else if (DockOrientation=doVertical)
    or (HostDockSite.Align in [alTop,alBottom]) then
      // the control is aligned top/bottom, that means its height is not
      // maximized. Save Height for docking.
      TBDockHeight:=Height;
  end;
end;

{------------------------------------------------------------------------------
  function TControl.CreateFloatingDockSite(const Bounds: TRect): TWinControl;
------------------------------------------------------------------------------}
function TControl.CreateFloatingDockSite(const Bounds: TRect): TWinControl;
var
  FloatingClass: TWinControlClass;
  NewWidth: Integer;
  NewHeight: Integer;
  NewClientWidth: Integer;
  NewClientHeight: Integer;
begin
  Result := nil;
  FloatingClass:=FloatingDockSiteClass;
  if (FloatingClass<>nil) and (FloatingClass<>TWinControlClass(ClassType)) then
  begin
    Result := FloatingClass.Create(Application);
    // resize with minimal resizes
    NewClientWidth:=Bounds.Right-Bounds.Left;
    NewClientHeight:=Bounds.Bottom-Bounds.Top;
    NewWidth:=Result.Width-Result.ClientWidth+NewClientWidth;
    NewHeight:=Result.Height-Result.ClientHeight+NewClientHeight;
    Result.SetBounds(Bounds.Top,Bounds.Left,NewWidth,NewHeight);
    SetClientSize(Point(NewClientWidth,NewClientHeight));
  end;
end;

procedure TControl.ExecuteDefaultAction;
begin
end;

procedure TControl.ExecuteCancelAction;
begin
end;

{------------------------------------------------------------------------------
  function TControl.GetFloating: Boolean;
------------------------------------------------------------------------------}
function TControl.GetFloating: Boolean;
var
  CurHostDockSite: TWinControl;
begin
  CurHostDockSite:=HostDockSite;
  Result := (CurHostDockSite <> nil)
            and (CurHostDockSite is FloatingDockSiteClass);
end;

{------------------------------------------------------------------------------
  function TControl.GetFloatingDockSiteClass: TWinControlClass;
------------------------------------------------------------------------------}
function TControl.GetFloatingDockSiteClass: TWinControlClass;
begin
  Result := FFloatingDockSiteClass;
end;

{------------------------------------------------------------------------------
  function TControl.GetLRDockWidth: Integer;
------------------------------------------------------------------------------}
function TControl.GetLRDockWidth: Integer;
begin
  if FLRDockWidth>0 then
    Result := FLRDockWidth
  else
    Result := UndockWidth;
end;

{------------------------------------------------------------------------------
  function TControl.IsHelpContextStored: boolean;
------------------------------------------------------------------------------}
function TControl.IsHelpContextStored: boolean;
begin
  if ActionLink=nil then
    Result := HelpContext<>0
  else
    Result := not ActionLink.IsHelpContextLinked;
end;

{------------------------------------------------------------------------------
  function TControl.IsHelpKeyWordStored: boolean;
------------------------------------------------------------------------------}
function TControl.IsHelpKeyWordStored: boolean;
begin
  if ActionLink=nil then
    Result := HelpKeyword<>''
  else
    Result := not ActionLink.IsHelpContextLinked;
end;

function TControl.IsOnClickStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsOnExecuteLinked;
end;

function TControl.IsShowHintStored: Boolean;
begin
  Result := not ParentShowHint;
end;

function TControl.IsVisibleStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsVisibleLinked;
end;

function TControl.GetUndockHeight: Integer;
begin
  if FUndockHeight>0 then
    Result := FUndockHeight
  else
    Result := Height;
end;

function TControl.GetUndockWidth: Integer;
begin
  if FUndockWidth>0 then
    Result := FUndockWidth
  else
    Result := Width;
end;

function TControl.IsAnchorsStored: boolean;
begin
  Result:=(Anchors<>AnchorAlign[Align]);
end;

function TControl.IsVisible: Boolean;
begin
  Result := FVisible and ((Parent = nil) or (Parent.IsVisible));
end;

{------------------------------------------------------------------------------}
{       TControl.LMCaptureChanged                                              }
{------------------------------------------------------------------------------}
Procedure TControl.LMCaptureChanged(Var Message: TLMessage);
Begin
  //DebugLn('[LMCaptureChanged for '+Name+':'+Classname+']');
  CaptureChanged;
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
Procedure TControl.CMMouseEnter(var Message: TLMessage);
Begin
  // this is a LCL based mouse message, so don't call DoBeforeMouseMessage
  //DebugLn('TControl.CMMouseEnter ',Name,':',ClassName,' ',FMouseEntered,' ',Message.LParam);
  if (Message.LParam=0) and (not FMouseEntered) then begin
    FMouseEntered:=true;
    MouseEnter;
    if FParent <> nil then
      FParent.Perform(CM_MOUSEENTER, 0, LParam(Self));
  end;
end;

{------------------------------------------------------------------------------
       TControl.CMMouseLeave
------------------------------------------------------------------------------}
Procedure TControl.CMMouseLeave(var Message: TLMessage);
Begin
  // this is a LCL based mouse message, so don't call DoBeforeMouseMessage
  //DebugLn('TControl.CMMouseLeave ',Name,':',ClassName,' ',FMouseEntered,' ',Message.LParam);
  if (Message.LParam=0) and FMouseEntered then begin
    FMouseEntered:=false;
    MouseLeave;
    if FParent <> nil then
      FParent.Perform(CM_MOUSELEAVE, 0, LParam(Self));
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

{------------------------------------------------------------------------------
       TControl.CMParentColorChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}
procedure TControl.CMParentColorChanged(var Message: TLMessage);
begin
  if FParentColor then
  begin
    Color := FParent.Color;
    FParentColor := true;
  end;
end;

{------------------------------------------------------------------------------
       TControl.CMShowHintChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}
procedure TControl.CMParentShowHintChanged(var Message: TLMessage);
begin
  if FParentShowHint then
  begin
    ShowHint := FParent.ShowHint;
    FParentShowHint := true;
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
  procedure TControl.CalculatePreferredSize(var PreferredWidth,
    PreferredHeight: integer);

  Calculates the default/preferred width and height for a control, which is used
  by the LCL autosizing algorithms as default size. Only positive values are
  valid. Negative or 0 are treated as undefined and the LCL uses other sizes
  instead.
  TWinControl overrides this and asks the interface for theme dependent values.
  See TWinControl.GetPreferredSize for more information.
 ------------------------------------------------------------------------------}
procedure TControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;

{------------------------------------------------------------------------------
  function TControl.GetPalette: HPalette;
------------------------------------------------------------------------------}
function TControl.GetPalette: HPalette;
begin
  Result:=0;
end;

function TControl.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=false;
end;

{------------------------------------------------------------------------------
  procedure TControl.DoOnResize;

  Call events
------------------------------------------------------------------------------}
procedure TControl.DoOnResize;
var
  i: Integer;
begin
  if Assigned(FOnResize) then FOnResize(Self);
  i:=FControlHandlers[chtOnResize].Count;
  while FControlHandlers[chtOnResize].NextDownIndex(i) do
    TNotifyEvent(FControlHandlers[chtOnResize][i])(Self);
end;

{------------------------------------------------------------------------------
  procedure TControl.DoOnChangeBounds;

  Call events
------------------------------------------------------------------------------}
procedure TControl.DoOnChangeBounds;
var
  i: Integer;
begin
  if Assigned(FOnChangeBounds) then FOnChangeBounds(Self);
  i:=FControlHandlers[chtOnChangeBounds].Count;
  while FControlHandlers[chtOnChangeBounds].NextDownIndex(i) do
    TNotifyEvent(FControlHandlers[chtOnChangeBounds][i])(Self);
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
  function TControl.ColorIsStored: boolean;
------------------------------------------------------------------------------}
function TControl.ColorIsStored: boolean;
begin
  Result:=(Color<>clWindow);
  if Result and ParentColor and (Parent<>nil) then
    Result:=false;
end;

{------------------------------------------------------------------------------
       TControl.DoConstrainedResize
------------------------------------------------------------------------------}
procedure TControl.DoConstrainedResize(var NewWidth, NewHeight : integer);
var MinWidth, MinHeight, MaxWidth, MaxHeight : TConstraintSize;
begin
  MinWidth:= Constraints.EffectiveMinWidth;
  MinHeight:= Constraints.EffectiveMinHeight;
  MaxWidth:= Constraints.EffectiveMaxWidth;
  MaxHeight:= Constraints.EffectiveMaxHeight;

  ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);

  if (MinWidth > 0) and (NewWidth < MinWidth) then
    NewWidth:= MinWidth
  else if (MaxWidth > 0) and (NewWidth > MaxWidth) then
    NewWidth:= MaxWidth;

  if (MinHeight > 0) and (NewHeight < MinHeight) then
    NewHeight:= MinHeight
  else if (MaxHeight > 0) and (NewHeight > MaxHeight) then
    NewHeight:= MaxHeight;
  //debugln('TControl.DoConstrainedResize ',DbgSName(Self),' ',dbgs(NewWidth),',',dbgs(NewHeight));
end;

{------------------------------------------------------------------------------
       TControl.DoConstraintsChange
------------------------------------------------------------------------------}
procedure TControl.DoConstraintsChange(Sender : TObject);
begin
  AdjustSize;
end;

procedure TControl.DoBorderSpacingChange(Sender: TObject);
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
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DragCanceled');
  {$ENDIF}
end;

{------------------------------------------------------------------------------
       TControl.DoStartDrag

------------------------------------------------------------------------------}
procedure TControl.DoStartDrag(var DragObject: TDragObject);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DoStartDrag ',Name,':',ClassName);
  {$ENDIF}
  if Assigned(FOnStartDrag) then FOnStartDrag(Self, DragObject);
end;

{------------------------------------------------------------------------------
       TControl.DoEndDrag
------------------------------------------------------------------------------}
Procedure TControl.DoEndDrag(Target: TObject; X,Y: Integer);
Begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DoEndDrag ',Name,':',ClassName,' XY=',X,',',Y);
  {$ENDIF}
  if Assigned(FOnEndDrag) then FOnEndDrag(Self,Target,X,Y);
end;

{------------------------------------------------------------------------------
       TControl.Perform

------------------------------------------------------------------------------}
Function TControl.Perform(Msg:Cardinal; WParam: WParam; LParam: LParam): LongInt;
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

{------------------------------------------------------------------------------
       TControl.GetClientOrigin
------------------------------------------------------------------------------}
function TControl.GetClientOrigin: TPoint;

  procedure RaiseParentNil;
  begin
    raise Exception.Create('TControl.GetClientOrigin: Parent=nil for '
                           +Name+':'+ClassName);
  end;

Begin
  //Assert(False, Format('Trace:[TControl.GetClientOrigin] %s',  [Classname]));
  if Parent = nil then
    RaiseParentNil;
    //raise EInvalidOperation.CreateFmt(SParentRequired, [Name]);
  Result := Parent.ClientOrigin;
  Inc(Result.X, FLeft);
  Inc(Result.Y, FTop);
  //Assert(False, Format('Trace:[TControl.GetClientOrigin] %s --> (%d, %d)',  [Classname, Result.X, Result.Y]));
end;

{------------------------------------------------------------------------------
       TControl.ScreenToClient
------------------------------------------------------------------------------}
Function TControl.ScreenToClient(const APoint: TPoint): TPoint;
var
  P : TPoint;
begin
  P := ClientOrigin;
  Result.X := APoint.X - P.X;
  Result.Y := APoint.Y - P.Y;
end;

{------------------------------------------------------------------------------
  Function TControl.ClientToScreen(const APoint: TPoint): TPoint;
------------------------------------------------------------------------------}
Function TControl.ClientToScreen(const APoint: TPoint): TPoint;
var
  P : TPoint;
begin
  P := ClientOrigin;
  Result.X := APoint.X + P.X;
  Result.Y := APoint.Y + P.Y;
end;

{------------------------------------------------------------------------------
  function TControl.ScreenToControl(const APoint: TPoint): TPoint;
------------------------------------------------------------------------------}
function TControl.ScreenToControl(const APoint: TPoint): TPoint;
var
  P : TPoint;
begin
  P := ControlOrigin;
  Result.X := APoint.X - P.X;
  Result.Y := APoint.Y - P.Y;
end;

{------------------------------------------------------------------------------
  function TControl.ControlToScreen(const APoint: TPoint): TPoint;
------------------------------------------------------------------------------}
function TControl.ControlToScreen(const APoint: TPoint): TPoint;
var
  P : TPoint;
begin
  P := ControlOrigin;
  Result.X := APoint.X + P.X;
  Result.Y := APoint.Y + P.Y;
end;

{------------------------------------------------------------------------------
       TControl.DblClick
------------------------------------------------------------------------------}
procedure TControl.DblClick;
begin
  If Assigned(FOnDblClick) then FOnDblClick(Self);
end;

{------------------------------------------------------------------------------
       TControl.TripleClick
------------------------------------------------------------------------------}
procedure TControl.TripleClick;
begin
  If Assigned(FOnTripleClick) then FOnTripleClick(Self);
end;

{------------------------------------------------------------------------------
       TControl.QuadClick
------------------------------------------------------------------------------}
procedure TControl.QuadClick;
begin
  If Assigned(FOnQuadClick) then FOnQuadClick(Self);
end;

{------------------------------------------------------------------------------
       TControl.DoDragMsg
------------------------------------------------------------------------------}
Procedure TControl.DoDragMsg(var DragMsg: TCMDrag);
var
  Accepts: Boolean;
  Src: TObject;
  P: TPoint;
Begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DoDragMsg ',Name,':',ClassName,' DragMsg.DragMessage=',ord(DragMsg.DragMessage));
  {$ENDIF}
  Src := DragMsg.Dragrec^.Source;
  P:=ScreenToClient(DragMsg.Dragrec^.Pos);
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DoDragMsg ',Name,':',ClassName,' DragMsg.Dragrec^.Pos=',DragMsg.Dragrec^.Pos.X,',',DragMsg.Dragrec^.Pos.Y,' -> P=',P.X,P.Y);
  if P.X<0 then RaiseGDBException('');
  {$ENDIF}
  case DragMsg.DragMessage of

    dmFindTarget:
      DragMsg.Result := longint(Self);

    dmDragEnter, dmDragLeave, dmDragMove:
      begin
        Accepts := True;
        case DragMsg.DragMessage of
          dmDragEnter: DragOver(Src,P.X,P.Y,dsDragEnter,Accepts);
          dmDragLeave: DragOver(Src,P.X,P.Y,dsDragLeave,Accepts);
          dmDragMove : DragOver(Src,P.X,P.Y,dsDragMove,Accepts);
        end;
        if Accepts then
          DragMsg.Result := 1
        else
          DragMsg.Result := 0;
      end;

    dmDragDrop:
      DragDrop(Src, P.X, P.Y);

  end; //case
end;

{------------------------------------------------------------------------------
  TControl.DragOver
------------------------------------------------------------------------------}
Procedure TControl.DragOver(Source: TObject; X,Y : Integer; State: TDragState;
  var Accept:Boolean);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DragOver ',Name,':',ClassName,' XY=',X,',',Y);
  {$ENDIF}
  Accept := False;
  if Assigned(FOnDragOver) then begin
    Accept := True;
    FOnDragOver(Self,Source,X,Y,State,Accept);
  end;
end;

{------------------------------------------------------------------------------
  TControl.DragDrop
------------------------------------------------------------------------------}
Procedure TControl.DragDrop(Source: TObject; X,Y : Integer);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DragDrop ',Name,':',ClassName,' XY=',X,',',Y);
  {$ENDIF}
  If Assigned(FOnDragDrop) then FOnDragDrop(Self, Source,X,Y);
end;

{------------------------------------------------------------------------------
  TControl Method SetColor  "Sets the default color and tells the widget set"
------------------------------------------------------------------------------}
procedure TControl.SetColor(value : TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FParentColor := False;
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
       TControl CanAutoSize
------------------------------------------------------------------------------}
Function TControl.CanAutoSize(Var NewWidth, NewHeight : Integer): Boolean;
Begin
  Result := True;
end;

{------------------------------------------------------------------------------
       TControl Dragging
------------------------------------------------------------------------------}
Function TControl.Dragging: Boolean;
Begin
  Result := (DragControl = self);
end;

{------------------------------------------------------------------------------
       TControl GetBoundsRect
------------------------------------------------------------------------------}
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

function TControl.GetTBDockHeight: Integer;
begin
  if FTBDockHeight>0 then
    Result := FTBDockHeight
  else
    Result := UndockHeight;
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

procedure TControl.SetAlignedBounds(aLeft, aTop, aWidth, aHeight: integer);
{ try to set the automatic changed bounds
  If the interface does not like our bounds, it sends a message with the real
  bounds, which invokes the automatic realigning of the control, .. a circle.
  To break the circle, only bounds that are different from the last try will
  be sent.
}
var
  NewBounds: TRect;
begin
  NewBounds:=Bounds(aLeft, aTop, aWidth, aHeight);
  if (cfLastAlignedBoundsValid in FControlFlags)
  and CompareRect(@NewBounds,@fLastAlignedBounds) then
    exit;
  fLastAlignedBounds:=NewBounds;
  Include(FControlFlags,cfLastAlignedBoundsValid);

  //if AnsiCompareText(ClassName,'TSCROLLBAR')=0 then
  //  DebugLn('TControl.SetAlignedBounds A ',Name,':',ClassName,' ',aLeft,',',aTop,',',aWidth,',',aHeight);

  SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight, true);
end;

{------------------------------------------------------------------------------
  procedure TControl.VisibleChanging;
------------------------------------------------------------------------------}
procedure TControl.VisibleChanging;
begin

end;

procedure TControl.AddHandler(HandlerType: TControlHandlerType;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FControlHandlers[HandlerType]=nil then
    FControlHandlers[HandlerType]:=TMethodList.Create;
  FControlHandlers[HandlerType].Add(AMethod);
end;

procedure TControl.RemoveHandler(HandlerType: TControlHandlerType;
  const AMethod: TMethod);
begin
  FControlHandlers[HandlerType].Remove(AMethod);
end;

{------------------------------------------------------------------------------
  procedure TControl.DoContextPopup(const MousePos: TPoint;
    var Handled: Boolean);
------------------------------------------------------------------------------}
procedure TControl.DoContextPopup(const MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FOnContextPopup) then
    FOnContextPopup(Self, MousePos, Handled);
end;

procedure TControl.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var
  NewAction: TCustomAction;
begin
  if Sender is TCustomAction then begin
    NewAction:=TCustomAction(Sender);
    if (not CheckDefaults)
    or (Caption = '') or (Caption = Name) then
      Caption := NewAction.Caption;
    if not CheckDefaults or Enabled then
      Enabled := NewAction.Enabled;
    if not CheckDefaults or (Hint = '') then
      Hint := NewAction.Hint;
    if not CheckDefaults or Visible then
      Visible := NewAction.Visible;
    if not CheckDefaults or not Assigned(OnClick) then
      OnClick := NewAction.OnExecute;
  end;
end;

procedure TControl.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

function TControl.GetAnchorSide(Kind: TAnchorKind): TAnchorSide;
begin
  Result:=FAnchorSides[Kind];
end;

function TControl.GetAnchorSideIndex(Index: integer): TAnchorSide;
begin
  case Index of
  0: Result:=FAnchorSides[akLeft];
  1: Result:=FAnchorSides[akTop];
  2: Result:=FAnchorSides[akRight];
  3: Result:=FAnchorSides[akBottom];
  else
  Result:=nil;
  end;
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
  function TControl.GetClientScrollOffset: TPoint;

  Returns the screen coordinate of the topleft pixel of the control.
------------------------------------------------------------------------------}
function TControl.GetControlOrigin: TPoint;
var
  ParentsClientOrigin: TPoint;
begin
  Result:=Point(Left,Top);
  if Parent<>nil then begin
    ParentsClientOrigin:=Parent.ClientOrigin;
    inc(Result.X,ParentsClientOrigin.X);
    inc(Result.Y,ParentsClientOrigin.Y);
  end;
end;

{------------------------------------------------------------------------------
       TControl WndPRoc
------------------------------------------------------------------------------}
procedure TControl.WndProc(var TheMessage : TLMessage);
Var
  Form : TCustomForm;
begin
  //DebugLn('CCC TControl.WndPRoc ',Name,':',ClassName);
  if (csDesigning in ComponentState) then
  begin
    // redirect messages to designer
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.Designer <> nil)
    and Form.Designer.IsDesignMsg(Self,TheMessage) then begin
      Exit;
    end;
  end
  else if (TheMessage.Msg >= LM_KeyFirst) and (TheMessage.Msg <= LM_KeyLast)
  then begin
    // keyboard messages
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.WantChildKey(Self,TheMessage)) then exit;
  end
  else if ((TheMessage.Msg>=LM_MOUSEFIRST) and (TheMessage.Msg<=LM_MOUSELAST))
  or ((TheMessage.Msg>=LM_MOUSEFIRST2) and (TheMessage.Msg<=LM_MOUSELAST2))
  then begin
    // mouse messages
    
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
        end;

      LM_LBUTTONDOWN,
      LM_LBUTTONDBLCLK:
        begin
          if FDragMode = dmAutomatic
          then begin
            BeginAutoDrag;
            { The VCL holds up the mouse down for dmAutomatic
              and sends it, when it decides, if it is a drag operation or
              not.
              This decision requires full control of focus and mouse, which
              do not all LCL interfaces provide. Therefore the mouse down event
              is sent immediately.
            }
            // VCL: exit;
          end;
          Include(FControlState,csLButtonDown);
        end;

      LM_LBUTTONUP:
        begin
          Exclude(FControlState, csLButtonDown);
        end;
    end;
  end;
  Dispatch(TheMessage);
end;

{------------------------------------------------------------------------------
  procedure TControl.ParentFormHandleInitialized;
  
  called by ChildHandlesCreated of parent form
------------------------------------------------------------------------------}
procedure TControl.ParentFormHandleInitialized;
begin

end;

{------------------------------------------------------------------------------
       TControl Invalidate
------------------------------------------------------------------------------}
procedure TControl.Invalidate;
Begin
  InvalidateControl(Visible, csOpaque in ControlStyle);
end;

{------------------------------------------------------------------------------
       TControl DoMouseDown  "Event Handler"
------------------------------------------------------------------------------}
procedure TControl.DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
  Shift: TShiftState);
begin
  if not (csNoStdEvents in ControlStyle) then
  Begin
    with Message do
      MouseDown(Button, KeysToShiftState(Keys) + Shift, XPos, YPos);
  end;
end;

{------------------------------------------------------------------------------
       TControl DoMouseUp  "Event Handler"
------------------------------------------------------------------------------}
procedure TControl.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle)
  then with Message do
    MouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TControl.SetAnchorSideIndex(Index: integer; const AValue: TAnchorSide
  );
begin
  GetAnchorSideIndex(Index).Assign(AValue);
end;

procedure TControl.SetBorderSpacing(const AValue: TControlBorderSpacing);
begin
  if FBorderSpacing=AValue then exit;
  FBorderSpacing.Assign(AValue);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonDown
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonDown(var Message: TLMLButtonDown);
begin
  DoBeforeMouseMessage;
  if csCaptureMouse in ControlStyle then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMLButtonDown ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  if csClickEvents in ControlStyle then Include(FControlState, csClicked);
  DoMouseDown(Message, mbLeft, []);
  //DebugLn('TCONTROL WMLBUTTONDOWN B ',Name,':',ClassName);
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
  if csCaptureMouse in ControlStyle then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMLButtonDblClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  // first send a mouse down
  DoMouseDown(Message, mbLeft ,[ssDouble]);
  // then send the double click
  if csClickEvents in ControlStyle then DblClick;
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
  if csCaptureMouse in ControlStyle then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMLButtonTripleClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if csCaptureMouse in ControlStyle then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMLButtonQuadClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  //DebugLn('TControl.WMLButtonUp A ',Name,':',ClassName,' csCaptureMouse=',csCaptureMouse in ControlStyle,' csClicked=',csClicked in ControlState);
  if csCaptureMouse in ControlStyle then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMLButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;

  if csClicked in ControlState then
  begin
    Exclude(FControlState, csClicked);
    //DebugLn('TControl.WMLButtonUp B ',ClientRect.Left,',',ClientRect.Top,',',ClientRect.Right,',',ClientRect.Bottom,' ',Message.Pos.X,',',Message.Pos.Y);
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos))
    then begin
      //DebugLn('TControl.WMLButtonUp C');
      Click;
    end;
  end;
  DoMouseUp(Message, mbLeft);
  //DebugLn('TControl.WMLButtonUp END');
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
  if (not (csDesigning in ComponentState)) and (ActionLink<>nil)
  and ((Action=nil) or (@FOnClick<>@Action.OnExecute) or Assigned(FOnClick))
  then
    ActionLink.Execute(Self)
  else if Assigned(FOnClick) then
    FOnClick(Self);
end;

{------------------------------------------------------------------------------
  TControl AddControl
  
  Add Handle object to parents Handle object.
------------------------------------------------------------------------------}
procedure TControl.AddControl;
begin
  TWSControlClass(WidgetSetClass).AddControl(Self);
end;

{------------------------------------------------------------------------------
  function TControl.CheckChildClassAllowed(ChildClass: TClass;
    ExceptionOnInvalid: boolean): boolean;
    
  Checks if this control can be the parent of a control of class ChildClass.
------------------------------------------------------------------------------}
function TControl.CheckChildClassAllowed(ChildClass: TClass;
  ExceptionOnInvalid: boolean): boolean;
  
  procedure RaiseInvalidChild;
  begin
    raise Exception.Create(ClassName+' can not have '+ChildClass.ClassName+' as child');
  end;
  
begin
  Result:=ChildClassAllowed(ChildClass);
  if (not Result) and ExceptionOnInvalid then
    RaiseInvalidChild;
end;

{------------------------------------------------------------------------------
  procedure TControl.CheckNewParent(AParent: TWinControl);
  
  Checks if this control can be the child of AParent.
  This check is executed in SetParent.
------------------------------------------------------------------------------}
procedure TControl.CheckNewParent(AParent: TWinControl);
begin
  if (AParent<>nil) then AParent.CheckChildClassAllowed(ClassType,true);
  if AParent = Self then begin
    raise EInvalidOperation.Create('A control can not have itself as parent');
  end;
end;

{------------------------------------------------------------------------------
       TControl SetAutoSize
------------------------------------------------------------------------------}
Procedure TControl.SetAutoSize(const value : Boolean);
Begin
  If AutoSize <> Value then begin
    FAutoSize := Value;
    //debugln('TControl.SetAutoSize ',DbgSName(Self));
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

procedure TControl.AnchorSideChanged(TheAnchorSide: TAnchorSide);
begin
  RequestAlign;
end;

{------------------------------------------------------------------------------
  function TControl.AutoSizeCanStart: boolean;
  
  Returns true if DoAutoSize can start.
  It returns false if
  - AutoSize=false
  - or the control is currently autosizing
  - or the control is not visible
  - or the control is destroying
------------------------------------------------------------------------------}
function TControl.AutoSizeCanStart: boolean;
begin
  Result:=false;
  if not AutoSize then exit;
  if AutoSizing then exit;
  if (csDestroying in ComponentState) then exit;
  if (not (Visible or ((csDesigning in ComponentState)
                       and (csNoDesignVisible in ControlStyle)))) then exit;
  Result:=true;
end;

{------------------------------------------------------------------------------
  function TControl.AutoSizeDelayed: boolean;
  
  Returns true, if the DoAutoSize should skip now, because not all parameters
  needed to calculate the AutoSize bounds are loaded or initialized.
------------------------------------------------------------------------------}
function TControl.AutoSizeDelayed: boolean;
begin
  Result:=(csLoading in ComponentState)
          or ((Parent<>nil) and Parent.AutoSizeDelayed);
end;

{------------------------------------------------------------------------------
       TControl SetBoundsRect
------------------------------------------------------------------------------}
Procedure TControl.SetBoundsRect(const ARect : TRect);
Begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TControl.SetBoundsRect] ',Name,':',ClassName);
  {$ENDIF}
  with ARect do
    SetBounds(Left,Top,Right - Left, Bottom - Top);
end;

procedure TControl.SetBoundsRectForNewParent(const AValue: TRect);
begin
  Include(FControlFlags,cfBoundsRectForNewParentValid);
  FBoundsRectForNewParent:=AValue;
end;

{------------------------------------------------------------------------------
  TControl SetClientHeight
------------------------------------------------------------------------------}
procedure TControl.SetClientHeight(Value: Integer);
begin
  if csLoading in ComponentState then begin
    FLoadedClientSize.Y:=Value;
    Include(FControlFlags,cfClientHeightLoaded);
  end;
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
  if csLoading in ComponentState then begin
    FLoadedClientSize.X:=Value;
    Include(FControlFlags,cfClientWidthLoaded);
  end;
  SetClientSize(Point(Value, ClientHeight));
end;

{------------------------------------------------------------------------------}
{  TControl SetTempCursor                                                      }
{------------------------------------------------------------------------------}
procedure TControl.SetTempCursor(Value: TCursor);
begin
  TWSControlClass(WidgetSetClass).SetCursor(Self, Value);
end;

procedure TControl.UpdateRolesForForm;
begin
  // called by the form when the "role" controls DefaultControl or CancelControl
  // has changed
end;

{------------------------------------------------------------------------------}
{  TControl SetCursor                                                          }
{------------------------------------------------------------------------------}
procedure TControl.SetCursor(Value: TCursor);
begin
  if FCursor <> Value
  then begin
    FCursor := Value;
    SetTempCursor(Value);
  end;
end;

procedure TControl.SetDragCursor(const AValue: TCursor);
begin
  if FDragCursor=AValue then exit;
  FDragCursor:=AValue;
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
    DebugLn('TControl.SetMouseCapture ',Name,':',ClassName,' NewValue=',Value);
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
procedure TControl.SetHint(const Value: TTranslateString);
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
  // do not switch the visible flag in design mode
  if not (csDesigning in ComponentState) or
    (csNoDesignVisible in ControlStyle) then Visible := True;
end;

{------------------------------------------------------------------------------
  TControl Notification
------------------------------------------------------------------------------}
procedure TControl.Notification( AComponent : TComponent; Operation : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = PopupMenu then PopupMenu := nil
    else if AComponent = Action then Action := nil;
end;

{------------------------------------------------------------------------------
  TControl GetText
------------------------------------------------------------------------------}
function TControl.GetText: TCaption;
var
  len: Integer;
begin
  // Check if GetTextBuf is overridden, otherwise
  // we can call RealGetText directly
{$IFDEF VER1_0}
  if Pointer(@Self.GetTextBuf) = Pointer(@TControl.GetTextBuf)
{$ELSE}
  if TMethod(@Self.GetTextBuf).Code = Pointer(@TControl.GetTextBuf)
{$ENDIF}
  then begin
    Result := RealGetText;
  end
  else begin
    // Bummer, we have to do it the compatible way.
    DebugLn('Note: GetTextBuf is overridden for: ', Classname);
    
    len := GetTextLen;
    if len = 0 
    then begin
      Result := '';
    end
    else begin
      SetLength(Result, len+1); // make sure there is room for the extra #0
      FillChar(Result[1], len, #0);
      len := GetTextBuf(@Result[1], len+1); 
      SetLength(Result, len);
    end;
  end;
end;

{------------------------------------------------------------------------------
  TControl RealGetText
------------------------------------------------------------------------------}
function TControl.RealGetText: TCaption;
begin
  Result := FCaption;
end;

function TControl.GetTextLen: Integer; 
begin
  Result := Length(FCaption);
end;

function TControl.GetAction: TBasicAction;
begin
  if ActionLink <> nil then
    Result := ActionLink.Action
  else
    Result := nil;
end;

function TControl.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TControlActionLink;
end;

{------------------------------------------------------------------------------
  TControl IsCaptionStored
------------------------------------------------------------------------------}
Function TControl.IsCaptionStored : Boolean;
Begin
  Result := (ActionLink = nil) or not ActionLink.IsCaptionLinked;
end;

function TControl.IsColorStored: Boolean;
begin
  Result := not ParentColor;
end;

function TControl.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsEnabledLinked;
end;

function TControl.IsFontStored: Boolean;
begin
  Result := not ParentFont {and not DesktopFont};
end;

function TControl.IsHintStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsHintLinked;
end;

{------------------------------------------------------------------------------
  TControl InvalidateControl
------------------------------------------------------------------------------}
procedure TControl.InvalidateControl(CtrlIsVisible, CtrlIsOpaque: Boolean);
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
  if (Parent=nil) or (not Parent.HandleAllocated)
  or ([csLoading,csDestroying]*Parent.ComponentState<>[])
  or ([csLoading,csDestroying]*ComponentState<>[])
  then exit;

  if (CtrlIsVisible or ((csDesigning in ComponentState)
  and not (csNoDesignVisible in ControlStyle)))
  then begin
    Rect := BoundsRect;
    InvalidateRect(Parent.Handle, @Rect, not (CtrlIsOpaque or
      (csOpaque in Parent.ControlStyle) or BackgroundClipped));
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.InvalidateControl(CtrlIsVisible, CtrlIsOpaque,
    IgnoreWinControls: Boolean);
------------------------------------------------------------------------------}
procedure TControl.InvalidateControl(CtrlIsVisible, CtrlIsOpaque,
  IgnoreWinControls: Boolean);
begin
  if IgnoreWinControls and (Self is TWinControl) then exit;
  InvalidateControl(CtrlIsVisible,CtrlIsOpaque);
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
  if (Parent=nil) or (not Parent.HandleAllocated)
  or (csDestroying in ComponentState) then exit;

  if (Visible or (csDesigning in ComponentState)
  and not (csNoDesignVisible in ControlStyle))
  then
    if csOpaque in ControlStyle then
    begin
      {$IFDEF VerboseDsgnPaintMsg}
      if csDesigning in ComponentState then
        DebugLn('TControl.Repaint A ',Name,':',ClassName);
      {$ENDIF}
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
  if ([csLoading,csDestroying]*ComponentState<>[]) then exit;
  if (FLastResizeWidth<>Width) or (FLastResizeHeight<>Height)
  or (FLastResizeClientWidth<>ClientWidth)
  or (FLastResizeClientHeight<>ClientHeight) then begin
    //if AnsiCompareText('NOTEBOOK',Name)=0 then
    {DebugLn('[TControl.Resize] ',Name,':',ClassName,
    ' Last=',FLastResizeWidth,',',FLastResizeHeight,
    ' LastClient=',FLastResizeClientWidth,',',FLastResizeClientHeight,
    ' New=',Width,',',Height,
    ' NewClient=',ClientWidth,',',ClientHeight);}
    FLastResizeWidth:=Width;
    FLastResizeHeight:=Height;
    FLastResizeClientWidth:=ClientWidth;
    FLastResizeClientHeight:=ClientHeight;
    DoOnResize;
  end;
end;

procedure TControl.Loaded;
begin
  inherited Loaded;
  {DebugLn('TControl.Loaded A ',Name,':',ClassName,
    ' CW=',cfClientWidthLoaded in FControlFlags,'=',FLoadedClientSize.X,
    ' CH=',cfClientHeightLoaded in FControlFlags,'=',FLoadedClientSize.Y,
    '');}
  UpdateBaseBounds(true,true,true);

  // align this control and the brothers
  if cfRequestAlignNeeded in FControlFlags then
    RequestAlign;
  if Action <> nil then ActionChange(Action, True);
end;

{------------------------------------------------------------------------------
  procedure TControl.DefineProperties(Filer: TFiler);
------------------------------------------------------------------------------}
procedure TControl.DefineProperties(Filer: TFiler);
begin
  // Optimiziation:
  // do not call inherited: TComponent only defines 'Left' and 'Top' and
  // TControl has them as regular properties.
end;

{------------------------------------------------------------------------------
  procedure TControl.AssignTo(Dest: TPersistent);
------------------------------------------------------------------------------}
procedure TControl.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomAction then
    with TCustomAction(Dest) do begin
      Enabled := Self.Enabled;
      Hint := Self.Hint;
      Caption := Self.Caption;
      Visible := Self.Visible;
      OnExecute := Self.OnClick;
    end
  else inherited AssignTo(Dest);
end;

{------------------------------------------------------------------------------
  TControl SetBounds
------------------------------------------------------------------------------}
procedure TControl.SetBounds(ALeft, ATop, AWidth, AHeight : integer);
begin
  ChangeBounds(ALeft, ATop, AWidth, AHeight);
end;

{------------------------------------------------------------------------------
  TControl SetConstraints
------------------------------------------------------------------------------}
procedure TControl.SetConstraints(const Value : TSizeConstraints);
begin
  FConstraints.Assign(Value);
end;

{------------------------------------------------------------------------------
  TControl SetAlign
------------------------------------------------------------------------------}
procedure TControl.SetAlign(Value: TAlign);
var
  OldAlign: TAlign;
begin
  if FAlign = Value then exit;
  //DebugLn('TControl.SetAlign ',Name,':',ClassName,' Old=',AlignNames[FAlign],' New=',AlignNames[Value]);
  OldAlign:=FAlign;
  FAlign := Value;
  // if anchors were on default then change them to new default
  // This is done for Delphi compatibility.
  if Anchors=AnchorAlign[OldAlign] then
    Anchors:=AnchorAlign[FAlign];
  RequestAlign;
end;

{------------------------------------------------------------------------------
  TControl SetAnchors
------------------------------------------------------------------------------}
procedure TControl.SetAnchors(const AValue: TAnchors);
begin
  if Anchors=AValue then exit;
  FAnchors:=AValue;
  RequestAlign;
end;

{------------------------------------------------------------------------------
  TControl RequestAlign

  Requests the parent to realign all brothers
------------------------------------------------------------------------------}
procedure TControl.RequestAlign;
begin
  if (Parent = nil) then exit;
  if (csLoading in ComponentState) or (not Parent.HandleAllocated) then begin
    Include(FControlFlags,cfRequestAlignNeeded);
    exit;
  end;
  Parent.AlignControl(Self);
  Exclude(FControlFlags,cfRequestAlignNeeded);
end;

procedure TControl.UpdateBaseBounds(StoreBounds,
  StoreParentClientSize, UseLoadedValues: boolean);
var
  NewBaseBounds: TRect;
  NewBaseParentClientSize: TPoint;
begin
  if (csLoading in ComponentState) or (fBaseBoundsLock>0) then exit;
  if StoreBounds then
    NewBaseBounds:=BoundsRect
  else
    NewBaseBounds:=FBaseBounds;
  if StoreParentClientSize then begin
    if Parent<>nil then begin
      NewBaseParentClientSize:=Point(Parent.ClientWidth,Parent.ClientHeight);
      if UseLoadedValues then begin
        if cfClientWidthLoaded in Parent.FControlFlags then
          NewBaseParentClientSize.X:=Parent.FLoadedClientSize.X;
        if cfClientHeightLoaded in Parent.FControlFlags then
          NewBaseParentClientSize.Y:=Parent.FLoadedClientSize.Y;
      end;
    end else
      NewBaseParentClientSize:=Point(0,0);
  end else
    NewBaseParentClientSize:=FBaseParentClientSize;
  if CompareRect(@NewBaseBounds,@FBaseBounds)
  and (NewBaseParentClientSize.X=FBaseParentClientSize.X)
  and (NewBaseParentClientSize.Y=FBaseParentClientSize.Y)
  then exit;
  {if csDesigning in ComponentState then
  DebugLn('TControl.UpdateBaseBounds ',Name,':',ClassName,
    ' OldBounds=',FBaseBounds.Left,',',FBaseBounds.Top,',',FBaseBounds.Right-FBaseBounds.Left,',',FBaseBounds.Bottom-FBaseBounds.Top,
    ' OldClientSize=',FBaseParentClientSize.X,',',FBaseParentClientSize.Y,
    ' NewBounds=',NewBaseBounds.Left,',',NewBaseBounds.Top,',',NewBaseBounds.Right-NewBaseBounds.Left,',',NewBaseBounds.Bottom-NewBaseBounds.Top,
    ' NewClientSize=',NewBaseParentClientSize.X,',',NewBaseParentClientSize.Y,
    '');}
  FBaseBounds:=NewBaseBounds;
  FBaseParentClientSize:=NewBaseParentClientSize;
  fLastAlignedBounds:=Rect(0,0,0,0);
end;

procedure TControl.LockBaseBounds;
begin
  inc(fBaseBoundsLock);
end;

procedure TControl.UnlockBaseBounds;
begin
  dec(fBaseBoundsLock);
  if fBaseBoundsLock<0 then RaiseGDBException('TControl.UnlockBaseBounds');
end;

procedure TControl.UpdateAnchorRules;
begin
  UpdateBaseBounds(true,true,false);
end;

{------------------------------------------------------------------------------
  TControl SetDragmode
------------------------------------------------------------------------------}
procedure TControl.SetDragMode(Value: TDragMode);
begin
  if FDragMode = Value then exit;
  FDragMode := Value;
end;

procedure TControl.DrawDragDockImage(DragDockObject: TDragDockObject);
begin
  DefaultDockImage(DragDockObject, False);
end;

procedure TControl.EraseDragDockImage(DragDockObject: TDragDockObject);
begin
  DefaultDockImage(DragDockObject, True);
end;

{------------------------------------------------------------------------------
  TControl DefaultDockImage
------------------------------------------------------------------------------}
procedure TControl.DefaultDockImage(DragDockObject: TDragDockObject;
  Erase: Boolean);
begin
  // ToDo Dock: draw or erase dock image
end;

{------------------------------------------------------------------------------
  TControl DockTrackNoTarget
------------------------------------------------------------------------------}
procedure TControl.DockTrackNoTarget(Source: TDragDockObject; X, Y: Integer);
begin
  PositionDockRect(Source);
end;

{------------------------------------------------------------------------------
  TControl SetLeft
------------------------------------------------------------------------------}
procedure TControl.SetLeft(Value: Integer);
begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TControl.SetLeft] ',Name,':',ClassName,' ',Value);
  {$ENDIF}
  SetBounds(Value, FTop, FWidth, FHeight);
end;

{------------------------------------------------------------------------------
  TControl SetTop
------------------------------------------------------------------------------}
procedure TControl.SetTop(Value: Integer);
begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TControl.SetTop] ',Name,':',ClassName,' ',Value);
  {$ENDIF}
  SetBounds(FLeft, Value, FWidth, FHeight);
end;

{------------------------------------------------------------------------------
  TControl SetWidth
------------------------------------------------------------------------------}
procedure TControl.SetWidth(Value: Integer);

  procedure CheckDesignBounds;
  begin
    // the user changed the width
    if Value<0 then
      raise Exception.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Negative width '
          +dbgs(Value)+' not allowed.');
    if Value>=10000 then
      raise Exception.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Width '
          +dbgs(Value)+' not allowed.');
  end;

begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TControl.SetWidth] ',Name,':',ClassName,' ',Value);
  {$ENDIF}
  if [csDesigning,csDestroying,csLoading]*ComponentState=[csDesigning] then
    CheckDesignBounds;
  SetBounds(FLeft, FTop, Max(0,Value), FHeight);
end;

{------------------------------------------------------------------------------
  TControl SetHeight
------------------------------------------------------------------------------}
procedure TControl.SetHeight(Value: Integer);

  procedure CheckDesignBounds;
  begin
    // the user changed the height
    if Value<0 then
      raise Exception.Create(
        'TWinControl.SetHeight ('+DbgSName(Self)+'): Negative height '
          +dbgs(Value)+' not allowed.');
    if Value>=10000 then
      raise Exception.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Height '
          +dbgs(Value)+' not allowed.');
  end;

begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TControl.SetHeight] ',Name,':',ClassName,' ',Value);
  {$ENDIF}
  if [csDesigning,csDestroying,csLoading]*ComponentState=[csDesigning] then
    CheckDesignBounds;
  SetBounds(FLeft, FTop, FWidth, Max(0,Value));
end;

{------------------------------------------------------------------------------
  procedure TControl.SetHelpContext(const AValue: THelpContext);
------------------------------------------------------------------------------}
procedure TControl.SetHelpContext(const AValue: THelpContext);
begin
  if FHelpContext=AValue then exit;
  FHelpContext:=AValue;
end;

{------------------------------------------------------------------------------
  procedure TControl.SetHelpKeyword(const AValue: String);
------------------------------------------------------------------------------}
procedure TControl.SetHelpKeyword(const AValue: String);
begin
  if FHelpKeyword=AValue then exit;
  FHelpKeyword:=AValue;
end;

procedure TControl.SetHostDockSite(const AValue: TWinControl);
begin
  if AValue=FHostDockSite then exit;
  Dock(AValue, BoundsRect);
end;

{------------------------------------------------------------------------------
  Procedure TControl.SetParent(NewParent : TWinControl);
------------------------------------------------------------------------------}
Procedure TControl.SetParent(NewParent: TWinControl);
begin
  if FParent = NewParent then exit;
  CheckNewParent(NewParent);
  if FParent <> nil then FParent.RemoveControl(Self);
  if cfBoundsRectForNewParentValid in FControlFlags then begin
    Exclude(FControlFlags,cfBoundsRectForNewParentValid);
    BoundsRect:=BoundsRectForNewParent;
  end;
  if NewParent <> nil then NewParent.InsertControl(Self);
end;

{------------------------------------------------------------------------------
  TControl SetParentComponent
------------------------------------------------------------------------------}
Procedure TControl.SetParentComponent(NewParentComponent: TComponent);
Begin
  if (NewParentComponent is TWinControl) then
    SetParent(TWinControl(NewParentComponent));
end;

{------------------------------------------------------------------------------
  procedure TControl.SetParentColor(Value : Boolean);
------------------------------------------------------------------------------}
procedure TControl.SetParentColor(Value : Boolean);
begin
  if FParentColor <> Value then
  begin
    FParentColor := Value;
    if Assigned(FParent) and not (csReading in ComponentState) then
      Perform(CM_PARENTCOLORCHANGED, 0, 0);
  end;
end;

{------------------------------------------------------------------------------
  TControl SetParentShowHint
------------------------------------------------------------------------------}
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
procedure TControl.SetPopupMenu(Value: TPopupMenu);
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
  DebugLn('[TControl.WMMouseMove] ',Name,':',ClassName,' ',Message.XPos,',',Message.YPos);
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

Procedure TControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if (Button in [mbLeft,mbRight]) and Dragging and (DragObject<>nil) then begin
    P:=ClientToScreen(Point(X,Y));
    DragObject.MouseDown(Button,Shift,P.X,P.Y);
  end;
  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X,Y);
end;

{------------------------------------------------------------------------------
  TControl MouseMove

------------------------------------------------------------------------------}
Procedure TControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if Dragging and (DragObject<>nil) then begin
    P:=ClientToScreen(Point(X,Y));
    DragObject.MouseMove(Shift,P.X,P.Y);
  end;
  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X,Y);
end;

{------------------------------------------------------------------------------
  TControl MouseUp
------------------------------------------------------------------------------}
Procedure TControl.MouseUp(Button: TMouseButton; Shift:TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if (Button in [mbLeft,mbRight]) and Dragging and (DragObject<>nil) then begin
    P:=ClientToScreen(Point(X,Y));
    DragObject.MouseUp(Button,Shift,P.X,P.Y);
  end;
  if Assigned(FOnMouseUp) then FOnMouseUp(Self, Button, Shift, X,Y);
end;

procedure TControl.MouseEnter;
begin
  //DebugLn('TControl.MouseEnter ',Name,':',ClassName,' ',Assigned(FOnMouseEnter));
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TControl.MouseLeave;
begin
  //DebugLn('TControl.MouseLeave ',Name,':',ClassName,' ',Assigned(FOnMouseLeave));
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

{------------------------------------------------------------------------------
  procedure TControl.CaptureChanged;

------------------------------------------------------------------------------}
procedure TControl.CaptureChanged;
begin
  if Dragging and (DragObject<>nil) then DragObject.CaptureChanged(Self);
end;

{------------------------------------------------------------------------------
  TControl SetShowHint

------------------------------------------------------------------------------}
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
    VisibleChanging;
    FVisible := Value;
    Perform(CM_VISIBLECHANGED, WParam(Ord(Value)), 0);
    RequestAlign;
  end;
  if (csLoading in ComponentState) then
    ControlState:=ControlState+[csVisibleSetInLoading];
end;

{------------------------------------------------------------------------------}
{       TControl.SetZOrder
}
{------------------------------------------------------------------------------}
Procedure TControl.SetZOrder(Topmost: Boolean);
begin
  if FParent = nil then exit;
  if Topmost then
    SetZOrderPosition(Parent.ControlCount-1)
  else
    SetZOrderPosition(0);
  {if Parent <> nil then begin
    AParent:= Parent;
     Just reinsert the control on top. Don't if it already is
    if Topmost then begin
      if (AParent.Controls[AParent.ControlCount - 1] <> Self) then begin
        AParent.RemoveControl(Self);
        AParent.InsertControl(Self);
      end;
    end else begin
      // Move all other controls over this one
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
  end;  }
end;

{------------------------------------------------------------------------------
       TControl.SetZOrderPosition
------------------------------------------------------------------------------}
function TControl.HandleObjectShouldBeVisible: boolean;
begin
  Result:=(Visible
          or ((csDesigning in ComponentState)
              and not (csNoDesignVisible in ControlStyle)))
          and not (csReadingState in ControlState);
end;

{------------------------------------------------------------------------------
  function TControl.ParentHandlesAllocated: boolean;
------------------------------------------------------------------------------}
function TControl.ParentHandlesAllocated: boolean;
begin
  Result:=(Parent<>nil) and (Parent.ParentHandlesAllocated);
end;

{------------------------------------------------------------------------------
  procedure TControl.InitiateAction;
------------------------------------------------------------------------------}
procedure TControl.InitiateAction;
begin
  if ActionLink <> nil then ActionLink.Update;
end;

{------------------------------------------------------------------------------
  procedure TControl.Dock(NewDockSite: TWinControl; ARect: TRect);
  
  
------------------------------------------------------------------------------}
procedure TControl.Dock(NewDockSite: TWinControl; ARect: TRect);

  procedure RaiseAlreadyDocking;
  begin
    RaiseGDBException('TControl.Dock '+Name+':'+ClassName+' csDocking in FControlState');
  end;

var
  OldHostDockSite: TWinControl;
begin
  if (csDocking in FControlState) then
    RaiseAlreadyDocking;
    
  // dock
  Include(FControlState, csDocking);
  try
    OldHostDockSite:=HostDockSite;

    if OldHostDockSite<>NewDockSite then begin
      // HostDockSite will change -> prepare
      if (OldHostDockSite<>nil) and (OldHostDockSite.FDockClients<>nil) then
        OldHostDockSite.FDockClients.Remove(Self);
      if (NewDockSite<>nil) and (NewDockSite.FDockClients<>nil) then
        NewDockSite.FDockClients.Add(Self);
    end;

    DoDock(NewDockSite,ARect);
    
    if FHostDockSite<>NewDockSite then
    begin
      // HostDockSite has changed -> commit
      OldHostDockSite := FHostDockSite;
      FHostDockSite := NewDockSite;
      if NewDockSite<>nil then NewDockSite.DoAddDockClient(Self,ARect);
      if OldHostDockSite<>nil then OldHostDockSite.DoRemoveDockClient(Self);
    end;
  finally
    Exclude(FControlState, csDocking);
  end;
end;

{------------------------------------------------------------------------------
  function TControl.ManualDock(NewDockSite: TWinControl; DropControl: TControl;
    ControlSide: TAlign): Boolean;


------------------------------------------------------------------------------}
function TControl.ManualDock(NewDockSite: TWinControl; DropControl: TControl;
  ControlSide: TAlign): Boolean;
var
  NewBounds: TRect;
  DockObject: TDragDockObject;
  NewPosition: TPoint;
begin
  if (NewDockSite=nil) then begin
    // undock / float this control
    // float the control at the same screen position
    if HostDockSiteManagerAvailable(HostDockSite) then begin
      HostDockSite.DockManager.GetControlBounds(Self,NewBounds);
      NewBounds.TopLeft:=HostDockSite.ClientToScreen(NewBounds.TopLeft);
    end else begin
      NewBounds.TopLeft:=ControlOrigin;
    end;
    NewBounds := Bounds(NewBounds.Left,NewBounds.Top,UndockWidth,UndockHeight);
    DebugLn('TControl.ManualDock ',Name,' NewDockSite=nil HostDockSiteManagerAvailable=',dbgs(HostDockSiteManagerAvailable(HostDockSite)),' NewBounds=',dbgs(NewBounds));
    Result := ManualFloat(NewBounds);
  end
  else
  begin
    // dock / unfloat this control
    CalculateDockSizes;
    
    Result := (HostDockSite=nil);
    if not Result then begin
      // undock from old HostSite
      DebugLn('TControl.ManualDock UNDOCKING ',Name);
      Result:=HostDockSite.DoUndock(NewDockSite,Self);
    end;
    
    if Result then begin
      DebugLn('TControl.ManualDock DOCKING ',Name);
      // create TDragDockObject for docking parameters
      DockObject := TDragDockObject.Create(Self);
      try
        // get current screen coordinates
        if HostDockSite<>nil then
          NewPosition:=HostDockSite.ClientToScreen(Point(Left,Top))
        else
          NewPosition:=ControlOrigin;
        // initialize DockObject
        with DockObject do begin
          FDragTarget := NewDockSite;
          FDropAlign := ControlSide;
          FDropOnControl := DropControl;
          DockRect := Bounds(NewPosition.X,NewPosition.Y,Width,Height);
        end;
        // map from screen coordinates to new HostSite coordinates
        NewPosition:=NewDockSite.ScreenToClient(NewPosition);
        // DockDrop
        DebugLn('TControl.ManualDock DOCKDROP ',Name,' DockRect=',dbgs(DockObject.DockRect),' NewPos=',dbgs(NewPosition));
        NewDockSite.DockDrop(DockObject,NewPosition.X,NewPosition.Y);
      finally
        DockObject.Free;
      end;
    end;
  end;
end;

{------------------------------------------------------------------------------
  function TControl.ManualFloat(TheScreenRect: TRect): Boolean;

  Undock and float.
  Float means here: create the floating dock site and dock this control into it.
  Exception: Forms do not need float dock sites and float on their own.
------------------------------------------------------------------------------}
function TControl.ManualFloat(TheScreenRect: TRect): Boolean;
var
  FloatHost: TWinControl;
begin
  // undock from old floating host dock site
  Result := (HostDockSite=nil) or HostDockSite.DoUndock(nil,Self);
  // create new float dock site and dock this control into it.
  if Result then begin
    FloatHost := CreateFloatingDockSite(TheScreenRect);
    debugln('TControl.ManualFloat A '+Name,':',ClassName,' ',dbgs(TheScreenRect),' FloatHost=',dbgs(FloatHost<>nil));
    if FloatHost<>nil then
      Dock(FloatHost,Rect(0,0,FloatHost.ClientWidth,FloatHost.ClientHeight))
    else
      Dock(FloatHost,TheScreenRect);
  end;
end;

{------------------------------------------------------------------------------
  function TControl.ReplaceDockedControl(Control: TControl;
    NewDockSite: TWinControl; DropControl: TControl; ControlSide: TAlign
    ): Boolean;

------------------------------------------------------------------------------}
function TControl.ReplaceDockedControl(Control: TControl;
  NewDockSite: TWinControl; DropControl: TControl; ControlSide: TAlign
  ): Boolean;
var
  OldDockSite: TWinControl;
begin
  Result := False;

  OldDockSite := Control.HostDockSite;
  if (OldDockSite<>nil) and (not HostDockSiteManagerAvailable(OldDockSite)) then
    exit;

  if OldDockSite <> nil then
    OldDockSite.DockManager.SetReplacingControl(Control);
  try
    ManualDock(OldDockSite,nil,alTop);
  finally
    if OldDockSite <> nil then
      OldDockSite.DockManager.SetReplacingControl(nil);
  end;
  Result:=Control.ManualDock(NewDockSite,DropControl,ControlSide);
end;

procedure TControl.AddHandlerOnResize(OnResizeEvent: TNotifyEvent;
  AsLast: boolean);
begin
  AddHandler(chtOnResize,TMethod(OnResizeEvent),AsLast);
end;

procedure TControl.RemoveHandlerOnResize(OnResizeEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnResize,TMethod(OnResizeEvent));
end;

procedure TControl.AddHandlerOnChangeBounds(OnChangeBoundsEvent: TNotifyEvent;
  AsLast: boolean);
begin
  AddHandler(chtOnChangeBounds,TMethod(OnChangeBoundsEvent),AsLast);
end;

procedure TControl.RemoveHandlerOnChangeBounds(OnChangeBoundsEvent: TNotifyEvent
  );
begin
  RemoveHandler(chtOnChangeBounds,TMethod(OnChangeBoundsEvent));
end;

procedure TControl.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TControlHandlerType;
begin
  inherited RemoveAllHandlersOfObject(AnObject);
  for HandlerType:=Low(TControlHandlerType) to High(TControlHandlerType) do
    FControlHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

{------------------------------------------------------------------------------
  TControl.SetZOrderPosition
  
  Set the position of the child control in the Controls list of its parent.
  TWinControl overrides this and will position itself in the FWinControls
  while this function position itself in the FControls list.

  Notes:
    The FControls are always below the FWinControls.
    TWinControl overrides this and will position itself in the FWinControls
    list.
------------------------------------------------------------------------------}
Procedure TControl.SetZOrderPosition(NewPosition: Integer);
Var
  OldPosition: Integer;
  Count: Integer;
begin
  if Parent = nil then exit;
  OldPosition := FParent.FControls.IndexOf(self);
  if (OldPosition >= 0) then
  begin
    Count := FParent.FControls.Count;
    if NewPosition < 0 then NewPosition := 0;
    if NewPosition >= Count then NewPosition := Count-1;
    if NewPosition <> OldPosition then begin
      FParent.FControls.Move(OldPosition,NewPosition);
      InvalidateControl(Visible,True,True);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.GetTextBuf
  Params:  None
  Returns: Nothing

  Copies max bufsize-1 chars to buffer
 ------------------------------------------------------------------------------}
function TControl.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
var
  S: string;
begin
  if BufSize <= 0 then Exit;

  S := RealGetText;
  if Length(S) >= BufSize
  then begin
    StrPLCopy(Buffer, S, BufSize - 1);
    Result := BufSize - 1;
  end 
  else begin
    StrPCopy(Buffer, S);
    Result := length(S);
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.SetTextBuf
  Params:  None
  Returns: Nothing
  
 ------------------------------------------------------------------------------}
procedure TControl.SetTextBuf(Buffer: PChar);
begin
  RealSetText(Buffer);
end;

{------------------------------------------------------------------------------}
{  TControl RealSetText                                                            }
{------------------------------------------------------------------------------}
procedure TControl.RealSetText(const Value: TCaption);
begin
  if FCaption = Value then Exit;
  FCaption := Value;
  Perform(CM_TEXTCHANGED, 0, 0);
end;

{------------------------------------------------------------------------------
  TControl SetText
------------------------------------------------------------------------------}
procedure TControl.SetText(const Value: TCaption);
begin
  //if CompareText(ClassName,'TMEMO')=0 then
  //  debugln('TControl.SetText A ',DbgSName(Self),' GetText="',GetText,'" Value="',Value,'" FCaption="',FCaption,'"');
  if GetText = Value then Exit;
  
  // Check if SetTextBuf is overridden, otherwise
  // we can call RealSetText directly
{$IFDEF VER1_0}
  if Pointer(@Self.GetTextBuf) = Pointer(@TControl.GetTextBuf)
{$ELSE}
  if TMethod(@Self.SetTextBuf).Code = Pointer(@TControl.SetTextBuf)
{$ENDIF}
  then begin
    RealSetText(Value);
  end
  else begin
    // Bummer, we have to do it the compatible way.
    DebugLn('Note: SetTextBuf is overridden for: ', Classname);
    SetTextBuf(PChar(Value));
  end;
  //if CompareText(ClassName,'TMEMO')=0 then
  //  debugln('TControl.SetText END ',DbgSName(Self),' FCaption="',FCaption,'"');
end;

{------------------------------------------------------------------------------
  TControl Update
------------------------------------------------------------------------------}
procedure TControl.Update;
begin
  if Parent<>nil then Parent.Update;
end;

{------------------------------------------------------------------------------
  Method: TControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TControl.Destroy;
var
  HandlerType: TControlHandlerType;
  Side: TAnchorKind;
begin
  //DebugLn('[TControl.Destroy] A ',Name,':',ClassName);
  Application.ControlDestroyed(Self);
  SetParent(nil);
  FreeThenNil(FActionLink);
  for Side:=Low(FAnchorSides) to High(FAnchorSides) do
    FreeThenNil(FAnchorSides[Side]);
  FreeThenNil(FBorderSpacing);
  FreeThenNil(FConstraints);
  FreeThenNil(FFont);
  //DebugLn('[TControl.Destroy] B ',Name,':',ClassName);
  inherited Destroy;
  //DebugLn('[TControl.Destroy] END ',Name,':',ClassName);
  for HandlerType:=Low(TControlHandlerType) to High(TControlHandlerType) do
    FreeThenNil(FControlHandlers[HandlerType]);
end;

{------------------------------------------------------------------------------
  Method: TControl.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TControl.Create(TheOwner: TComponent);
var
  Side: TAnchorKind;
begin
  //if AnsiCompareText(ClassName,'TSpeedButton')=0 then
  //  DebugLn('TControl.Create START ',Name,':',ClassName);
  inherited Create(TheOwner);

  FControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks,
                    csOpaque];
  FConstraints:= TSizeConstraints.Create(Self);
  FBorderSpacing:=TControlBorderSpacing.Create(Self);
  FBorderSpacing.OnChange:= @DoBorderSpacingChange;
  for Side:=Low(FAnchorSides) to High(FAnchorSides) do
    FAnchorSides[Side]:=TAnchorSide.Create(Self,Side);

  FAnchors := [akLeft,akTop];
  FAlign := alNone;
  FColor := clWindow;
  FVisible := true;
  FParentShowHint := True;
  FParentColor := True;
  FWindowProc := @WndProc;
  FCursor := crDefault;
  FFont := TFont.Create;
  FFont.OnChange := @FontChanged;
  FIsControl := False;
  FEnabled := True;
  FHelpType := htContext;
  FDragCursor := crDrag;
  FFloatingDockSiteClass := TCustomDockForm;
  //DebugLn('TControl.Create END ',Name,':',ClassName);
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
  MoveWindowOrgEx(Result, Left, Top);
  IntersectClipRect(Result, 0, 0, Width, Height);
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
  procedure TControl.AnchorToNeighbour(Side: TAnchorKind; Space: integer;
    Sibling: TControl);

  Setup AnchorSide to anchor one side to the side of a neighbour sibling.
  For example Right side to Left side, or Top side to Bottom.
 ------------------------------------------------------------------------------}
procedure TControl.AnchorToNeighbour(Side: TAnchorKind; Space: integer;
  Sibling: TControl);
begin
  case Side of
  akLeft: BorderSpacing.Left:=Space;
  akTop: BorderSpacing.Top:=Space;
  akRight: BorderSpacing.Right:=Space;
  akBottom: BorderSpacing.Bottom:=Space;
  end;
  AnchorSide[Side].Side:=DefaultSideForAnchorKind[Side];
  AnchorSide[Side].Control:=Sibling;
  Anchors:=Anchors+[Side];
end;

{------------------------------------------------------------------------------
  procedure TControl.AnchorHorizontalCenterTo(Sibling: TControl);

  Setup AnchorSide to center the control horizontally relative to a sibling.
 ------------------------------------------------------------------------------}
procedure TControl.AnchorHorizontalCenterTo(Sibling: TControl);
begin
  AnchorSide[akLeft].Side:=asrCenter;
  AnchorSide[akLeft].Control:=Sibling;
  Anchors:=Anchors+[akLeft]-[akRight];
end;

{------------------------------------------------------------------------------
  procedure TControl.AnchorVerticalCenterTo(Sibling: TControl);

  Setup AnchorSide to center the control vertically relative to a sibling.
 ------------------------------------------------------------------------------}
procedure TControl.AnchorVerticalCenterTo(Sibling: TControl);
begin
  AnchorSide[akTop].Side:=asrCenter;
  AnchorSide[akTop].Control:=Sibling;
  Anchors:=Anchors+[akTop]-[akBottom];
end;

procedure TControl.SetInitialBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  //DebugLn('TControl.SetInitialBounds A ',Name,':',ClassName,' ',aLeft,',',aTop,',',aWidth,',',aHeight);
  if (csLoading in ComponentState)
  or ((Owner<>nil) and (csLoading in Owner.ComponentState)) then
    exit;
  //DebugLn('TControl.SetInitialBounds B ',Name,':',ClassName,' ',aLeft,',',aTop,',',aWidth,',',aHeight);
  SetBounds(aLeft,aTop,aWidth,aHeight);
end;

procedure TControl.SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer;
  Lock: boolean);
begin
  if Lock then LockBaseBounds;
  try
    SetBounds(aLeft, aTop, aWidth, aHeight);
  finally
    if Lock then UnlockBaseBounds;
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.GetPreferredSize(
    var PreferredWidth, PreferredHeight: integer; Raw: boolean);
    
  Returns the default/preferred width and height for a control, which is used
  by the LCL autosizing algorithms as default size. Only positive values are
  valid. Negative or 0 are treated as undefined and the LCL uses other sizes
  instead.
  If not Raw then then values be be adjusted by the constraints and undefined
  values will be replaced by the current width and height.
  TWinControl overrides this and asks the interface for theme dependent values.
  See TWinControl.GetPreferredSize for more information.
 ------------------------------------------------------------------------------}
procedure TControl.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: boolean);
begin
  if not (cfPreferredSizeValid in FControlFlags) then begin
    CalculatePreferredSize(FPreferredWidth,FPreferredHeight);
    Include(FControlFlags,cfPreferredSizeValid);
  end;
  PreferredWidth:=FPreferredWidth;
  PreferredHeight:=FPreferredHeight;

  if not Raw then begin
    // use Width and Height for undefined preferred size
    if PreferredWidth<=0 then PreferredWidth:=Width;
    if PreferredHeight<=0 then PreferredHeight:=Height;
    
    // if this control is aligned adjust PreferredWidth and or PreferredHeight
    if Parent<>nil then begin
      if AnchorAlign[Align]*[akLeft,akRight]=[akLeft,akRight] then begin
        // the control will be expanded to maximum width
        // -> use the current width, which is or will be eventually set by the
        //    aligning code
        PreferredWidth:=Width;
      end;
      if AnchorAlign[Align]*[akTop,akBottom]=[akTop,akBottom] then begin
        // the control will be expanded to maximum height
        // -> use the current height, which is or will be eventually set by the
        //    aligning code
        PreferredHeight:=Height;
      end;
    end;
    
    // apply constraints
    PreferredWidth:=Constraints.MinMaxWidth(PreferredWidth);
    PreferredHeight:=Constraints.MinMaxHeight(PreferredHeight);
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.InvalidatePreferredSize;
  
  Invalidate the cache of the preferred size of this and all parent controls.
 ------------------------------------------------------------------------------}
procedure TControl.InvalidatePreferredSize;
var
  AControl: TControl;
begin
  AControl:=Self;
  while AControl<>nil do begin
    Exclude(AControl.FControlFlags,cfPreferredSizeValid);
    AControl:=AControl.Parent;
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.WMWindowPosChanged
  Params:   Msg: The message
  Returns:  nothing

  event handler.

 ------------------------------------------------------------------------------}
procedure TControl.WMWindowPosChanged(var Message: TLMWindowPosChanged);
begin
  // Do not handle this message and leave it to WMSize and WMMove
  Message.Result:= 0;
end;

{------------------------------------------------------------------------------
  Method: TControl.WMSize
  Params:   Message : TLMSize
  Returns:  nothing

  Event handler for LMSize messages.
  Overriden by TWinControl.WMSize.
 ------------------------------------------------------------------------------}
procedure TControl.WMSize(Var Message : TLMSize);
begin
  {$IFDEF CHECK_POSITION}
  DebugLn('[TControl.WMSize] Name=',Name,':',ClassName,' Message.Width=',Message.Width,' Message.Height=',Message.Height,' Width=',Width,' Height=',Height);
  {$ENDIF}
  //Assert(False, Format('Trace:[TWinControl.WMSize] %s', [ClassName]));

  SetBoundsKeepBase(Left,Top,Message.Width,Message.Height,Parent<>nil);
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
  {$IFDEF CHECK_POSITION}
  DebugLn('[TControl.WMMove] Name=',Name,':',ClassName,' Message.XPos=',Message.XPos,' Message.YPos=',Message.YPos,' OldLeft=',Left,' OldTop=',Top);
  {$ENDIF}
  { Just sync the coordinates }
  SetBoundsKeepBase(Message.XPos, Message.YPos, Width, Height,Parent<>nil);
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

// included by controls.pp

{ =============================================================================
  $Log$
  Revision 1.239  2005/01/21 11:52:01  micha
  cleanup focus; fix tabbing

  Revision 1.238  2005/01/21 10:34:56  mattias
  implemented streaming of anchorsides

  Revision 1.237  2005/01/17 17:49:27  mattias
  fixed constraints for forms under gtk

  Revision 1.236  2005/01/17 11:53:39  mattias
  added showing all four sides to AnchorEditor

  Revision 1.235  2005/01/13 20:21:41  mattias
  added desgntime check for TControl.Width/Height for high values

  Revision 1.234  2005/01/13 19:52:50  mattias
  added desgntime check for TControl.Width/Height for negative values

  Revision 1.233  2005/01/12 23:18:07  mattias
  limited widget sizes to 10000x10000

  Revision 1.232  2005/01/08 14:23:56  micha
  move taborder and tabstop to twincontrol

  Revision 1.231  2005/01/07 01:31:44  mattias
  implemented TCheckBox.State=cbGrayed for gtk intf without visual representation

  Revision 1.230  2005/01/03 22:44:31  mattias
  implemented TControl.AnchorSide

  Revision 1.229  2005/01/03 01:07:08  mattias
  fixed registering TProgressBar, disabled docking in TToolBar, return key for codeexplorer, updated finnish translation

  Revision 1.228  2005/01/01 14:38:36  mattias
  fixed loading TMemo.Lines - During Loading

  Revision 1.227  2004/12/22 23:54:21  mattias
  started TControl.AnchorSide

  Revision 1.226  2004/12/21 19:01:00  mattias
  fixed saving default value of TControl.Anchors

  Revision 1.225  2004/12/20 00:11:24  mattias
  changed TControl.Anchors default value to AnchorAlign[Align]

  Revision 1.224  2004/12/07 06:28:09  vincents
  fixed error because of conflicted merge

  Revision 1.223  2004/12/07 03:15:32  vincents
  fixed fpc 1.0.x compilation

  Revision 1.222  2004/12/06 22:41:45  vincents
  fixed type cast of method pointer

  Revision 1.221  2004/11/05 22:08:53  mattias
  implemented auto sizing: child to parent sizing

  Revision 1.220  2004/11/03 14:18:35  mattias
  implemented preferred size for controls for theme depending AutoSizing

  Revision 1.219  2004/10/28 09:30:49  mattias
  implemented borderspacing TWinControl.ChildSizing.Left/Top

  Revision 1.218  2004/10/13 09:59:24  vincents
  change parameter type in implementation to TTranslateString too

  Revision 1.217  2004/09/14 10:23:44  mattias
  implemented finding DefineProperties in registered TPersistent, implemented auto commenting of missing units for Delphi unit conversion

  Revision 1.216  2004/09/12 13:21:37  micha
  remove obsolete message LM_DRAGINFOCHANGED

  Revision 1.215  2004/09/11 13:06:48  micha
  convert LM_ADDCHILD message to interface method

  Revision 1.214  2004/09/08 22:59:54  mattias
  started TTabControl

  Revision 1.213  2004/09/04 22:24:16  mattias
  added default values for compiler skip options and improved many parts of synedit for UTF8

  Revision 1.212  2004/08/26 19:09:34  mattias
  moved navigation key handling to TApplication and added options for custom navigation

  Revision 1.211  2004/08/18 22:56:11  mattias
  implemented basic manual docking

  Revision 1.210  2004/08/18 20:49:02  mattias
  simple forms can now be child controls

  Revision 1.209  2004/08/18 09:08:34  mattias
  fixed deleting of collection item in prop editor

  Revision 1.208  2004/08/17 19:01:36  mattias
  gtk intf now ignores size notifications of unrealized widgets

  Revision 1.207  2004/08/05 21:20:47  mattias
  moved designer/abstractformeditor.pp to ideintf/formeditingintf.pas

  Revision 1.206  2004/07/25 01:04:45  mattias
  TXMLPropStorage basically working

  Revision 1.205  2004/07/17 15:08:36  mattias
  fixed tab for TPanel and TPage

  Revision 1.204  2004/07/11 13:03:54  mattias
  extended RolesForForm to manage multiple roles for on control

  Revision 1.203  2004/07/07 22:26:58  mattias
  fixed showing grabers for boundless components

  Revision 1.202  2004/07/04 20:07:08  micha
  form notifies control of new role

  Revision 1.201  2004/07/03 14:59:42  mattias
  fixed keydown geting all keys

  Revision 1.200  2004/07/01 20:42:11  micha
  implement better ExecuteXXAction design; break dependency on TButton class in TCustomForm

  Revision 1.199  2004/06/28 23:46:40  marc
  * Fixed compilation on 1.0.10
  * Fixed check for override of GetTextBuf and SetTextBuf

  Revision 1.198  2004/06/28 09:48:46  mattias
  added valgrind flag to compiler options

  Revision 1.196  2004/06/24 17:59:18  mattias
  fixed compilation for fpc 1.0.10

  Revision 1.195  2004/06/20 21:21:49  micha
  fix GetVisible to return this control's visibility, instead introduce IsVisible to check for recursive visibility

  Revision 1.194  2004/06/20 20:25:47  micha
  fix tabbing to next control to skip invisible notebook pages

  Revision 1.193  2004/06/17 20:52:18  mattias
  fixed setting ImageIndex when TMenuItem.ActionChange

  Revision 1.192  2004/06/17 10:38:40  mattias
  fixed TToolButton.SetMenuItem while loading

  Revision 1.191  2004/06/15 17:21:01  mattias
  fixed TTreeNode.Delete and deleting in between node

  Revision 1.190  2004/06/14 12:54:02  micha
  fix designer cursor to not set Form.Cursor directly

  Revision 1.189  2004/06/01 22:49:50  mattias
  added workaround for buggy typinfo GetMethodProp function

  Revision 1.188  2004/06/01 09:58:35  mattias
  implemented setting TCustomPage.PageIndex  from Andrew Haines

  Revision 1.187  2004/05/30 14:02:30  mattias
  implemented OnChange for TRadioButton, TCheckBox, TToggleBox and some more docking stuff

  Revision 1.186  2004/05/15 20:17:09  mattias
  replaced WMSize by DoSetBounds

  Revision 1.185  2004/05/11 11:42:27  mattias
  replaced writeln by debugln

  Revision 1.184  2004/05/11 10:53:59  mattias
  replaced writeln by debugln

  Revision 1.183  2004/04/18 23:55:39  marc
  * Applied patch from Ladislav Michl
  * Changed the way TControl.Text is resolved
  * Added setting of text to TWSWinControl

  Revision 1.182  2004/04/11 10:19:28  micha
  cursor management updated:
  - lcl notifies interface via WSControl.SetCursor of changes
  - fix win32 interface to respond to wm_setcursor callback and set correct cursor

  Revision 1.181  2004/04/10 17:58:56  mattias
  implemented mainunit hints for include files

  Revision 1.180  2004/04/02 19:39:46  mattias
  fixed checking empty mask raw image

  Revision 1.179  2004/04/01 18:09:50  mattias
  removed unneeded SendDockNotification

  Revision 1.178  2004/03/24 01:21:41  marc
  * Simplified signals for gtkwsbutton

  Revision 1.177  2004/03/08 22:36:01  mattias
  added TWinControl.ParentFormInitializeWnd

  Revision 1.176  2004/03/07 09:37:20  mattias
  added workaround for AutoSize in TCustomLabel

  Revision 1.175  2004/02/28 00:34:35  mattias
  fixed CreateComponent for buttons, implemented basic Drag And Drop

  Revision 1.174  2004/02/23 18:24:38  mattias
  completed new TToolBar

  Revision 1.173  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.171  2004/02/22 10:43:20  mattias
  added child-parent checks

  Revision 1.170  2004/02/21 15:37:33  mattias
  moved compiler options to project menu, added -CX for smartlinking

  Revision 1.169  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.168  2004/02/13 15:49:54  mattias
  started advanced LCL auto sizing

  Revision 1.167  2004/02/04 23:30:18  mattias
  completed TControl actions

  Revision 1.166  2004/02/02 16:59:28  mattias
  more Actions  TAction, TBasicAction, ...

  Revision 1.165  2004/01/27 21:32:11  mattias
  improved changing style of controls

  Revision 1.164  2004/01/10 18:00:42  mattias
  fixed GetWindowOrgEx, added GetDCOriginRelativeToWindow

  Revision 1.163  2004/01/06 17:58:06  mattias
  fixed setting TRadioButton.Caption for gtk

  Revision 1.162  2004/01/03 18:16:25  mattias
  set DragCursor props to default

  Revision 1.161  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.160  2003/12/28 02:40:50  mattias
  set colors to default values

  Revision 1.159  2003/12/25 14:17:07  mattias
  fixed many range check warnings

  Revision 1.158  2003/12/14 19:18:04  micha
  hint fixes: parentfont, font itself, showing/hiding + more

  Revision 1.157  2003/09/23 17:52:04  mattias
  added SetAnchors

  Revision 1.156  2003/09/20 13:27:49  mattias
  varois improvements for ParentColor from Micha

  Revision 1.155  2003/09/13 15:51:21  mattias
  implemented parent color from Micha

  Revision 1.154  2003/09/13 10:04:35  mattias
  fixed ColorIsStored

  Revision 1.153  2003/09/13 10:02:18  mattias
  set default color to clWindow

  Revision 1.152  2003/08/27 11:01:10  mattias
  started TDockTree

  Revision 1.151  2003/08/26 20:30:39  mattias
  fixed updating component tree on delete component

  Revision 1.150  2003/08/26 14:33:40  mattias
  implemented component tree for OI

  Revision 1.149  2003/08/23 11:30:50  mattias
  fixed SetComboHeight in win32 intf and finddeclaration of overloaded proc definition

  Revision 1.148  2003/08/22 07:58:38  mattias
  started componenttree

  Revision 1.147  2003/08/21 13:04:10  mattias
  implemented insert marks for TTreeView

  Revision 1.146  2003/08/14 15:31:42  mattias
  started TTabSheet and TPageControl

  Revision 1.145  2003/08/04 08:43:20  mattias
  fixed breaking circle in ChangeBounds

  Revision 1.144  2003/07/31 19:56:50  mattias
  fixed double messages SETLabel

  Revision 1.143  2003/07/24 06:54:32  mattias
  fixed anti circle mechnism for aligned controls

  Revision 1.142  2003/07/07 07:59:34  mattias
  made Size_SourceIsInterface a flag

  Revision 1.141  2003/07/06 20:40:34  mattias
  TWinControl.WmSize/Move now updates interface messages smarter

  Revision 1.140  2003/07/06 17:53:34  mattias
  updated polish localization

  Revision 1.139  2003/06/27 23:42:38  mattias
  fixed TScrollBar resizing

  Revision 1.138  2003/06/25 18:12:32  mattias
  added docking properties

  Revision 1.137  2003/06/23 09:42:09  mattias
  fixes for debugging lazarus

  Revision 1.136  2002/08/17 23:41:34  mattias
  many clipping fixes

  Revision 1.135  2003/06/20 12:56:53  mattias
  reduced paint messages on destroy

  Revision 1.134  2003/06/13 14:38:01  mattias
  fixed using streamed clientwith/height for child anchors

  Revision 1.133  2003/06/13 12:53:52  mattias
  fixed TUpDown and added handler lists for TControl

  Revision 1.132  2003/06/12 18:55:44  mattias
  improved designer to recognize auto child moves

  Revision 1.131  2003/06/11 22:29:42  mattias
  fixed realizing bounds after loading form

  Revision 1.130  2003/06/10 17:23:34  mattias
  implemented tabstop

  Revision 1.129  2003/06/10 15:58:39  mattias
  started TLabeledEdit

  Revision 1.128  2003/06/10 12:28:23  mattias
  fixed anchoring controls

  Revision 1.127  2003/06/10 00:46:16  mattias
  fixed aligning controls

  Revision 1.126  2003/06/07 17:14:12  mattias
  small changes for fpc 1.1

  Revision 1.125  2003/05/28 08:46:24  mattias
  break;points dialog now gets the items without debugger

  Revision 1.124  2003/05/24 08:51:41  mattias
  implemented designer close query

  Revision 1.123  2003/05/03 09:53:33  mattias
  fixed popupmenu for component palette

  Revision 1.122  2003/04/11 08:09:26  mattias
  published TControl help properties

  Revision 1.121  2003/04/10 09:22:42  mattias
  implemented changing dependency version

  Revision 1.120  2003/03/25 10:45:40  mattias
  reduced focus handling and improved focus setting

  Revision 1.119  2003/03/17 23:39:30  mattias
  added TCheckGroup

  Revision 1.118  2003/03/13 10:11:41  mattias
  fixed TControl.Show in design mode

  Revision 1.117  2003/03/11 23:14:19  mattias
  added TControl.HandleObjectShouldBeVisible

  Revision 1.116  2003/03/11 22:56:41  mattias
  added visiblechanging

  Revision 1.115  2003/01/18 21:31:43  mattias
  fixed scrolling offset of TScrollingWinControl

  Revision 1.114  2003/01/01 13:01:01  mattias
  fixed setcolor for streamed components

  Revision 1.113  2002/12/29 18:13:38  mattias
  identifier completion: basically working, still hidden

  Revision 1.112  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.111  2002/12/27 17:46:04  mattias
  fixed SetColor

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
