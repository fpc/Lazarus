{%MainUnit ../controls.pp}
{  $Id$  }

{******************************************************************************
                                     TControl
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

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{ $DEFINE CHECK_POSITION}

{------------------------------------------------------------------------------
  TControl.AdjustSize

  Calls DoAutoSize smart.
  During loading and handle creation the calls are delayed.

  This method does the same as TWinControl.DoAutoSize at the beginning.
  But since DoAutoSize is commonly overriden by existing Delphi components,
  they do not all tests, which can result in too much overhead. To reduce this
  the LCL calls AdjustSize instead.
------------------------------------------------------------------------------}
procedure TControl.Adjustsize;
begin
  //debugln(['TControl.AdjustSize START ',DbgSName(Self),' AutoSizeCanStart=',AutoSizeCanStart,' AutoSizeDelayed=',AutoSizeDelayed]);
  if (not AutoSizeCanStart) or AutoSizeDelayed then begin
    //debugln('TControl.AdjustSize AutoSizeDelayed ',DbgSName(Self));
    Include(FControlFlags,cfAutoSizeNeeded);
    exit;
  end;
  //debugln(['TControl.AdjustSize DoAutoSize ',DbgSName(Self)]);
  DoAutoSize;
  Exclude(FControlFlags,cfAutoSizeNeeded);
end;

{------------------------------------------------------------------------------
  Method: TControl.BeginDrag
  Params: Immediate: Drag behaviour
          Threshold: distance to move before dragging starts
                     -1 uses the default value of DragManager.DragThreshold
  Returns: Nothing

  Starts the dragging of a control. If the Immediate flag is set, dragging
  starts immediately.
 ------------------------------------------------------------------------------}
procedure TControl.BeginDrag(Immediate: Boolean; Threshold: Integer);
begin
  DragManager.DragStart(Self, Immediate, Threshold);
end;

{------------------------------------------------------------------------------
       TControl.BeginAutoDrag
------------------------------------------------------------------------------}
procedure TControl.BeginAutoDrag;
begin
  BeginDrag(DragManager.DragImmediate, DragManager.DragThreshold);
end;

{------------------------------------------------------------------------------
       TControl.BeginAutoSizing
------------------------------------------------------------------------------}
procedure TControl.BeginAutoSizing;
  procedure Error;
  begin
    RaiseGDBException('TControl.BeginAutoSizing');
  end;
begin
  if FAutoSizing then Error;
  FAutoSizing := True;
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
    LCLProc.MoveRectToFit(ARect, NewDockSite.GetLogicalClientRect);
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
  function TControl.GetDragImages: TDragImageList;

  Returns Drag image list that will be used while drag opetations
------------------------------------------------------------------------------}
function TControl.GetDragImages: TDragImageList;
begin
  Result := nil;
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

{------------------------------------------------------------------------------
       TControl.Bringtofront
------------------------------------------------------------------------------}
procedure TControl.BringToFront;
begin
  SetZOrder(true);
end;

{------------------------------------------------------------------------------
       TControl.CanTab
------------------------------------------------------------------------------}
function TControl.CanTab: Boolean;
begin
  Result := False;
end;

{------------------------------------------------------------------------------
       TControl.Change
------------------------------------------------------------------------------}
procedure TControl.Changed;
begin
  Perform(CM_CHANGED, 0, LParam(self));
end;

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
  ParentFont := False;
  Invalidate;
end;

procedure TControl.ParentFontChanged;
begin
  if csLoading in ComponentState then exit;

  if FParentFont then
  begin
    Font := FParent.Font;
    FParentFont := true;
  end;
end;

procedure TControl.SetAction(Value: TBasicAction);
begin
  if (Value=Action) then exit;
  //debugln('TControl.SetAction A ',Name,':',ClassName,' Old=',DbgS(Action),' New=',DbgS(Value));
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
  NewBounds: TRect;

  function PosSizeKept: boolean;
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
  //if (Parent=nil) and (Left>0) and (ALeft=0) then DumpStack; // This can happen if the interface has not yet moved the window and for some reason something applies the interface coords back to the LCL
  {$ENDIF}

  // constraint the size
  DoConstrainedResize(ALeft, ATop, AWidth, AHeight);

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
    InvalidateControl(IsControlVisible, False, true);
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
      if Parent <> nil then
        Parent.AdjustSize;
      if PosSizeKept then exit;
      // notify before autosizing
      BoundsChanged;
      if PosSizeKept then exit;
      //if csDesigning in ComponentState then
      //  DebugLn('TControl.ChangeBounds ',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);

      // autosize this control and its brothers
      RequestAlign;
      if PosSizeKept then exit;
      // autosize childs
      if SizeChanged and (Self is TWinControl) then
        TWinControl(Self).ReAlign;
    finally
      // unlock the autosizing of the child controls
      // (this will autosize the childs)
      if Self is TWinControl then
        TWinControl(Self).EnableAlign;
      // autosize self
      AdjustSize;
    end;
  finally
    dec(FSizeLock);
    UnlockBaseBounds;
  end;
  if PosSizeKept then exit;

  // send messages, if this is the top level call
  if FSizeLock>0 then exit;

  // invalidate
  if (not (csLoading in ComponentState))
  and (not (Self is TWinControl)) then
    Invalidate;
  // notify user about resize
  if (not (csLoading in ComponentState)) then begin
    Resize;
    CheckOnChangeBounds;
    // for delphi compatibility send size/move messages
    PosSizeKept;
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
  if CheckPosition(Self) then
  if csDesigning in ComponentState then
    DebugLn('TControl.DoSetBounds ',Name,':',ClassName,
      ' Old=',DbgS(Left,Top,Width,Height),
      ' New=',DbgS(aLeft,aTop,aWidth,aHeight),
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
    Result.SetBounds(Bounds.Left,Bounds.Top,NewWidth,NewHeight);
    Result.SetClientSize(Point(NewClientWidth,NewClientHeight));
    debugln('TControl.CreateFloatingDockSite A ',DbgSName(Self),' ',DbgSName(Result),' ',dbgs(Result.BoundsRect));
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
  Result := (ActionLink = nil) or not ActionLink.IsHelpLinked;
end;

{------------------------------------------------------------------------------
  function TControl.IsHelpKeyWordStored: boolean;
------------------------------------------------------------------------------}
// Using IsHelpContextLinked() for controlling HelpKeyword
// is not correct. Therefore, use IsHelpLinked which means that all 3 Help* properties
// must be equal. Also, this function becomes exactly the same as one just above.
function TControl.IsHelpKeyWordStored: boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsHelpLinked;
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
  Result := IsControlVisible and ((Parent = nil) or (Parent.IsVisible));
end;

function TControl.IsControlVisible: Boolean;
begin
  Result := (FVisible
             or ((csDesigning in ComponentState)
                  and (not (csNoDesignVisible in ControlStyle))));
end;

function TControl.FormIsUpdating: boolean;
begin
  Result:=(Parent<>nil) and Parent.FormIsUpdating;
end;

{------------------------------------------------------------------------------
       TControl.LMCaptureChanged
------------------------------------------------------------------------------}
Procedure TControl.LMCaptureChanged(Var Message: TLMessage);
Begin
  //DebugLn('[LMCaptureChanged for '+Name+':'+Classname+']');
  CaptureChanged;
End;

{------------------------------------------------------------------------------
       TControl.CMENABLEDCHANGED
------------------------------------------------------------------------------}
procedure TControl.CMEnabledChanged(var Message: TLMEssage);
begin
  Invalidate;
end;

{------------------------------------------------------------------------------
       TControl.CMHITTEST
------------------------------------------------------------------------------}
procedure TControl.CMHITTEST(var Message : TCMHitTest);
begin
  Message.Result := 1;
end;

{------------------------------------------------------------------------------
       TControl.CMMouseEnter
------------------------------------------------------------------------------}
procedure TControl.CMMouseEnter(var Message: TLMessage);
begin
  // this is a LCL based mouse message, so don't call DoBeforeMouseMessage
  //DebugLn('TControl.CMMouseEnter ',Name,':',ClassName,' ',FMouseEntered,' ',Message.LParam);
  if (Message.LParam=0) and (not FMouseEntered) then
  begin
    FMouseEntered := True;
    MouseEnter;
    if FParent <> nil then
      FParent.Perform(CM_MOUSEENTER, 0, LParam(Self));
  end;
end;

{------------------------------------------------------------------------------
       TControl.CMMouseLeave
------------------------------------------------------------------------------}
procedure TControl.CMMouseLeave(var Message: TLMessage);
begin
  // this is a LCL based mouse message, so don't call DoBeforeMouseMessage
  //DebugLn('TControl.CMMouseLeave ',Name,':',ClassName,' ',FMouseEntered,' ',Message.LParam);
  if (Message.LParam = 0) and FMouseEntered then
  begin
    FMouseEntered := False;
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

procedure TControl.CMTextChanged(var Message: TLMessage);
begin
  TextChanged;
end;

{------------------------------------------------------------------------------
       TControl.CMParentColorChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}
procedure TControl.CMParentColorChanged(var Message: TLMessage);
begin
  if csLoading in ComponentState then exit;
  
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
    PreferredHeight: integer; WithThemeSpace: Boolean);

  Calculates the default/preferred width and height for a control, which is used
  by the LCL autosizing algorithms as default size. Only positive values are
  valid. Negative or 0 are treated as undefined and the LCL uses other sizes
  instead.
  TWinControl overrides this and asks the interface for theme dependent values.
  See TWinControl.GetPreferredSize for more information.

  WithThemeSpace: If true, adds space for stacking. For example: TRadioButton
  has a minimum size. But for staking multiple TRadioButtons there should be
  some space around. This space is theme dependent, so it passed parameter to
  the widgetset.
 ------------------------------------------------------------------------------}
procedure TControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
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
begin
  Exclude(FControlFlags,cfOnResizeNeeded);
  if Assigned(FOnResize) then FOnResize(Self);
  DoCallNotifyHandler(chtOnResize);
end;

{------------------------------------------------------------------------------
  procedure TControl.DoOnChangeBounds;

  Call events
------------------------------------------------------------------------------}
procedure TControl.DoOnChangeBounds;
begin
  Exclude(FControlFlags,cfOnChangeBoundsNeeded);
  if Assigned(FOnChangeBounds) then FOnChangeBounds(Self);
  DoCallNotifyHandler(chtOnChangeBounds);
end;

procedure TControl.CheckOnChangeBounds;
var
  CurBounds: TRect;
  CurClientSize: TPoint;
begin
  if [csLoading,csDestroying]*ComponentState<>[] then exit;
  CurBounds:=BoundsRect;
  CurClientSize:=Point(ClientWidth,ClientHeight);
  if (not CompareRect(@FLastDoChangeBounds,@CurBounds))
  or (ComparePoints(CurClientSize,FLastDoChangeClientSize)<>0) then begin
    if FormIsUpdating then begin
      Include(FControlFlags,cfOnChangeBoundsNeeded);
      exit;
    end;
    FLastDoChangeBounds:=CurBounds;
    FLastDoChangeClientSize:=CurClientSize;
    DoOnChangeBounds;
  end;
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
procedure TControl.DoConstrainedResize(var NewLeft, NewTop,
  NewWidth, NewHeight: integer);
var MinWidth, MinHeight, MaxWidth, MaxHeight : TConstraintSize;
begin
  MinWidth:= Constraints.EffectiveMinWidth;
  MinHeight:= Constraints.EffectiveMinHeight;
  MaxWidth:= Constraints.EffectiveMaxWidth;
  MaxHeight:= Constraints.EffectiveMaxHeight;

  ConstrainedResize(MinWidth, MinHeight, MaxWidth, MaxHeight);

  if (MinWidth > 0) and (NewWidth < MinWidth) then
  begin
    // right kept position ? interpret as resizing left border
    if (NewLeft+NewWidth) = (Left+Width) then
    begin
      Dec(NewLeft, MinWidth - NewWidth);
      if NewLeft < Left then
        NewLeft := Left;
    end;
    NewWidth:= MinWidth
  end else if (MaxWidth > 0) and (NewWidth > MaxWidth) then
  begin
    if (NewLeft+NewWidth) = (Left+Width) then
    begin
      Inc(NewLeft, NewWidth - MaxWidth);
      if NewLeft > Left then
        NewLeft := Left;
    end;
    NewWidth:= MaxWidth;
  end;

  if (MinHeight > 0) and (NewHeight < MinHeight) then
  begin
    // bottom kept position ? interpret as resizing bottom border
    if (NewTop+NewHeight) = (Top+Height) then
    begin
      Dec(NewTop, MinHeight - NewHeight);
      if NewTop < Top then
        NewTop := Top;
    end;
    NewHeight:= MinHeight
  end else if (MaxHeight > 0) and (NewHeight > MaxHeight) then
  begin
    if (NewTop+NewHeight) = (Top+Height) then
    begin
      Inc(NewTop, NewHeight - MaxHeight);
      if NewTop > Top then
        NewTop := Top;
    end;
    NewHeight:= MaxHeight;
  end;
  //debugln('TControl.DoConstrainedResize ',DbgSName(Self),' ',dbgs(NewWidth),',',dbgs(NewHeight));
end;

{------------------------------------------------------------------------------
       TControl.DoConstraintsChange
------------------------------------------------------------------------------}
procedure TControl.DoConstraintsChange(Sender : TObject);
begin
  AdjustSize;
end;

procedure TControl.DoBorderSpacingChange(Sender: TObject;
  InnerSpaceChanged: Boolean);
begin
  if InnerSpaceChanged then
    AdjustSize
  else
    RequestAlign;
end;

function TControl.IsBorderSpacingInnerBorderStored: Boolean;
begin
  Result:=BorderSpacing.InnerBorder<>0;
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
  DebugLn('TControl.DoEndDrag ',Name,':',ClassName,' XY=',IntToStr(X),',',IntToStr(Y));
  {$ENDIF}
  if Assigned(FOnEndDrag) then FOnEndDrag(Self,Target,X,Y);
end;

{------------------------------------------------------------------------------
       TControl.Perform

------------------------------------------------------------------------------}
function TControl.Perform(Msg:Cardinal; WParam: WParam; LParam: LParam): LRESULT;
var
  Message : TLMessage;
begin
  Message.Msg := Msg;
  Message.WParam := WParam;
  Message.LParam := LParam;
  Message.Result := 0;
  if Self <> nil then WindowProc(Message);
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
function TControl.ScreenToClient(const APoint: TPoint): TPoint;
var
  P : TPoint;
begin
  P := ClientOrigin;
  Result.X := APoint.X - P.X;
  Result.Y := APoint.Y - P.Y;
end;

{------------------------------------------------------------------------------
  function TControl.ClientToScreen(const APoint: TPoint): TPoint;
------------------------------------------------------------------------------}
function TControl.ClientToScreen(const APoint: TPoint): TPoint;
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
function TControl.DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean):LRESULT;
var
  AWinTarget: TWinControl;
  Accepts: Boolean;
  P: TPoint;
Begin
  Result := 0;
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DoDragMsg ',Name,':',ClassName,' DragMsg.DragMessage=',IntToStr(ord(ADragMessage)));
  {$ENDIF}

  case ADragMessage of

    dmFindTarget:
      Result := PtrInt(Self);

    dmDragEnter, dmDragLeave, dmDragMove:
      begin
        Accepts := True;
        P := ScreenToClient(APosition);
        if ADragObject is TDragDockObject then
        begin
          AWinTarget:= TWinControl(ADragObject.DragTarget);
          AWinTarget.DockOver(TDragDockObject(ADragObject), P.X, P.Y, TDragState(ADragMessage), Accepts);
        end
        else
          DragOver(ADragObject, P.X, P.Y, TDragState(ADragMessage), Accepts);
        Result := Ord(Accepts);
      end;

    dmDragDrop:
      begin
        P := ScreenToClient(APosition);
        if ADragObject is TDragDockObject then
        begin
          AWinTarget:= TWinControl(ADragObject.DragTarget);
          AWinTarget.DockDrop(TDragDockObject(ADragObject), P.X, P.Y);
        end
        else
          DragDrop(ADragObject, P.X, P.Y);
      end;
  end;
end;

{------------------------------------------------------------------------------
  TControl.DragOver
------------------------------------------------------------------------------}
Procedure TControl.DragOver(Source: TObject; X,Y : Integer; State: TDragState;
  var Accept:Boolean);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DragOver ',Name,':',ClassName,' XY=',IntToStr(X),',',IntToStr(Y));
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
  DebugLn('TControl.DragDrop ',Name,':',ClassName,' XY=',IntToStr(X),',',IntToStr(Y));
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
function TControl.CanAutoSize(Var NewWidth, NewHeight : Integer): Boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
       TControl Dragging
------------------------------------------------------------------------------}
function TControl.Dragging: Boolean;
begin
  Result := DragManager.Dragging(Self);
end;

{------------------------------------------------------------------------------
       TControl GetBoundsRect
------------------------------------------------------------------------------}
function TControl.GetBoundsRect: TRect;
begin
  Result.Left := FLeft;
  Result.Top := FTop;
  Result.Right := FLeft+FWidth;
  Result.Bottom := FTop+FHeight;
end;

function TControl.GetClientHeight: Integer;
begin
  Result:=ClientRect.Bottom;
end;

function TControl.GetClientWidth: Integer;
begin
  Result:=ClientRect.Right;
end;

{------------------------------------------------------------------------------
       TControl GetEnabled
------------------------------------------------------------------------------}
function TControl.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

{------------------------------------------------------------------------------
       TControl GetMouseCapture
------------------------------------------------------------------------------}
function TControl.GetMouseCapture : Boolean;
begin
  Result := GetCaptureControl = Self;
end;

function TControl.GetTBDockHeight: Integer;
begin
  if FTBDockHeight>0 then
    Result := FTBDockHeight
  else
    Result := UndockHeight;
end;

{------------------------------------------------------------------------------
       TControl GetPopupMenu
------------------------------------------------------------------------------}
function TControl.GetPopupMenu: TPopupMenu;
begin
  Result := FPopupMenu;
end;

{------------------------------------------------------------------------------
  procedure TControl.DoOnShowHint(HintInfo: Pointer);
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
  if (fLastAlignedBoundsTried>0)
  and CompareRect(@NewBounds,@fLastAlignedBounds) then begin
    inc(fLastAlignedBoundsTried);
    if fLastAlignedBoundsTried>4 then begin
      DebugLn(['TControl.SetAlignedBounds ',DbgSName(Self),' last try ',dbgs(NewBounds),' try=',fLastAlignedBoundsTried]);
      exit;
    end;
  end;
  fLastAlignedBounds:=NewBounds;
  fLastAlignedBoundsTried:=1;

  //if AnsiCompareText(ClassName,'TSCROLLBAR')=0 then
  //  DebugLn('TControl.SetAlignedBounds A ',Name,':',ClassName,' ',aLeft,',',aTop,',',aWidth,',',aHeight);

  SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight, true);
end;

function TControl.IsAParentAligning: boolean;
var
  p: TWinControl;
begin
  p:=Parent;
  while (p<>nil) do begin
    if (wcfAligningControls in p.FWinControlFlags)
    or (p.FAlignLevel>0) then
      exit(true);
    p:=p.Parent;
  end;
  Result:=false;
end;

{------------------------------------------------------------------------------
  procedure TControl.VisibleChanging;
------------------------------------------------------------------------------}
procedure TControl.VisibleChanging;
begin
  DoCallNotifyHandler(chtOnVisibleChanging);
end;

procedure TControl.VisibleChanged;
begin
  DoCallNotifyHandler(chtOnVisibleChanged);
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

procedure TControl.DoCallNotifyHandler(HandlerType: TControlHandlerType);
begin
  FControlHandlers[HandlerType].CallNotifyEvents(Self);
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
    if not CheckDefaults or (Self.HelpContext = 0) then
      Self.HelpContext := HelpContext;
    if not CheckDefaults or (Self.HelpKeyword = '') then
      Self.HelpKeyword := HelpKeyword;
    // HelpType is set implicitly when assigning HelpContext or HelpKeyword
  end;
end;

procedure TControl.DoActionChange(Sender: TObject);
begin
  if Sender = Action then ActionChange(Sender, False);
end;

function TControl.CaptureMouseButtonsIsStored: boolean;
begin
  Result := FCaptureMouseButtons <> [mbLeft];
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

function TControl.GetAnchoredControls(Index: integer): TControl;
begin
  Result:=TControl(fAnchoredControls[Index]);
end;

{------------------------------------------------------------------------------
  TControl GetClientRect
  
  Returns the size of visual client area.
  For example the inner size of a TGroupBox.
  For a TScrollBox it is the visual size, not the logical size.
------------------------------------------------------------------------------}
function TControl.GetClientRect: TRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

{------------------------------------------------------------------------------
  TControl GetLogicalClientRect

  Returns the size of complete client area. It can be bigger or smaller than
  the visual size, but normally it is the same. For example a TScrollBox can
  have different sizes.
------------------------------------------------------------------------------}
function TControl.GetLogicalClientRect: TRect;
begin
  Result:=ClientRect;
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
  function TControl.GetControlOrigin: TPoint;

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

              Further Note:
                Under winapi a  LM_LBUTTONDOWN ends the drag immediate.
              For example: If we exit here, then mouse down on TTreeView does
                not work any longer under gtk.
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
  // The form is really connection to the target screen. For example, the gtk
  // under X gathers some screen information not before form creation.
  // But this information is needed to create DeviceContexts, which
  // are needed to calculate Text Size and such stuff needed for AutoSizing.
  // That's why AdjustSize delays AutoSizing till this moment. Now do the
  // AutoSize.
  AdjustSize;
end;

{------------------------------------------------------------------------------
       TControl Invalidate
------------------------------------------------------------------------------}
procedure TControl.Invalidate;
begin
  //DebugLn(['TControl.Invalidate ',DbgSName(Self)]);
  InvalidateControl(IsVisible, csOpaque in ControlStyle);
end;

{------------------------------------------------------------------------------
       TControl DoMouseDown  "Event Handler"
------------------------------------------------------------------------------}
procedure TControl.DoMouseDown(var Message: TLMMouse; Button: TMouseButton;
  Shift: TShiftState);
begin
  //DebugLn('TControl.DoMouseDown ',DbgSName(Self),' ');
  if not (csNoStdEvents in ControlStyle) then begin
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
  Method: TControl.WMContextMenu
  Params: Message
  Returns: Nothing

  ContextMenu event handler
 ------------------------------------------------------------------------------}

procedure TControl.WMContextMenu(var Message: TLMMouse);
var
  Control: TControl;
  TempPopupMenu: TPopupMenu;
  P2: TPoint;
  Handled: Boolean;
begin
  if (csDesigning in ComponentState) or (Message.Result <> 0) then Exit;
  P2 := SmallPointToPoint(Message.Pos);
  Handled:=False;
  DoContextPopup(P2,Handled);
  if Handled then
  begin
    Message.Result:=1;
    Exit;
  end;

  Control := Self;
  while Control <> nil do
  begin
    TempPopupMenu := Control.GetPopupMenu;
    if (TempPopupMenu <> nil) then
    begin
      if not TempPopupMenu.AutoPopup then Exit;
//      SendCancelMode(nil);
      TempPopupMenu.PopupComponent := Control;
      //DebugLn(['TControl.WMContextMenu ',DbgSName(Self),' Message.pos=',dbgs(P2)]);
      P2 := ClientToScreen(P2);
      //DebugLn(['TControl.WMContextMenu ',DbgSName(Self),' P2=',dbgs(P2)]);
      TempPopupMenu.Popup(P2.X, P2.Y);
      Message.Result:= 1;
      Exit;
    end;
    Control := Control.Parent;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
  begin
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
  if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMRButtonDown ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonDown ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
  begin
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
  if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMRButtonDblClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonDblClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
  begin
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
  if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMRButtonTripleClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonTripleClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
  begin
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
  if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMRButtonQuadClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonQuadClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
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
  //DebugLn('TControl.WMLButtonUp A ',Name,':',ClassName,' csCaptureMouse=',DbgS(csCaptureMouse in ControlStyle),' csClicked=',DbgS(csClicked in ControlState));
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMLButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;

  if csClicked in ControlState then
  begin
    Exclude(FControlState, csClicked);
    //DebugLn('TControl.WMLButtonUp B ',dbgs(ClientRect.Left),',',dbgs(ClientRect.Top),',',dbgs(ClientRect.Right),',',dbgs(ClientRect.Bottom),' ',dbgs(Message.Pos.X),',',dbgs(Message.Pos.Y));
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
  if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMRButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;
  
  Message.Result := Perform(LM_CONTEXTMENU,TLMessage(Message).WParam,
     TLMessage(Message).LParam);
  //MouseUp event is independent of return values of contextmenu
  DoMouseUp(Message, mbRight);
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
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;
  
  DoMouseUp(Message, mbMiddle);
end;

{------------------------------------------------------------------------------
       TControl Click
------------------------------------------------------------------------------}
procedure TControl.Click;
begin
  //DebugLn(['TControl.Click ',DbgSName(Self)]);
  if (not (csDesigning in ComponentState)) and (ActionLink<>nil)
  and ((Action=nil) or (@FOnClick<>@Action.OnExecute) or Assigned(FOnClick))
  then
    ActionLink.Execute(Self)
  else if Assigned(FOnClick) then
    FOnClick(Self);
end;

{------------------------------------------------------------------------------
  TControl DialogChar
  
  Do something useful with accelerators etc.
------------------------------------------------------------------------------}
function TControl.DialogChar(var Message: TLMKey): boolean;
begin
  Result := false;
end;

procedure TControl.UpdateMouseCursor(X, Y: integer);
begin
  //DebugLn(['TControl.UpdateMouseCursor ',DbgSName(Self)]);
  if csDesigning in ComponentState then Exit;
  if Screen.Cursor <> crDefault then Exit;
  SetTempCursor(Cursor);
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
    if FAutoSize then
      AdjustSize;
  end;
end;

{------------------------------------------------------------------------------
  TControl DoAutoSize

  IMPORTANT: Many Delphi controls override this method and many call this method
  directly after setting some properties.
  During handle creation not all interfaces can create complete Device Contexts
  which are needed to calculate things like text size.
  That's why you should always call AdjustSize instead of DoAutoSize.
------------------------------------------------------------------------------}
procedure TControl.DoAutoSize;
begin
  //Handled by TWinControl, or other descendants
end;

procedure TControl.AnchorSideChanged(TheAnchorSide: TAnchorSide);
begin
  //debugln('TControl.AnchorSideChanged ',DbgSName(Self));
  RequestAlign;
end;

procedure TControl.ForeignAnchorSideChanged(TheAnchorSide: TAnchorSide;
  Operation: TAnchorSideChangeOperation);
var
  Side: TAnchorKind;
  AControl: TControl;
begin
  AControl:=TheAnchorSide.Owner;
  //debugln('TControl.ForeignAnchorSideChanged A Self=',DbgSName(Self),' TheAnchorSide.Owner=',DbgSName(TheAnchorSide.Owner),' Operation=',dbgs(ord(Operation)),' Anchor=',dbgs(TheAnchorSide.Kind));
  if TheAnchorSide.Control=Self then begin
    if fAnchoredControls=nil then
      fAnchoredControls:=TFPList.Create;
    if fAnchoredControls.IndexOf(AControl)<0 then
      fAnchoredControls.Add(AControl);
  end else if fAnchoredControls<>nil then begin
    if TheAnchorSide.Owner<>nil then begin
      for Side:=Low(TAnchorKind) to High(TAnchorKind) do begin
        if (AControl.FAnchorSides[Side]<>nil)
        and (AControl.FAnchorSides[Side].Control=Self) then begin
          // still anchored
          exit;
        end;
      end;
    end;
    fAnchoredControls.Remove(AControl);
  end;
end;

{------------------------------------------------------------------------------
  function TControl.AutoSizeCanStart: boolean;
  
  Returns true if DoAutoSize can start. That means, it tests the minimum
  requirements to start. Some controls need even more.

  It returns false if
  - AutoSize=false
  - or the control is currently autosizing
  - or the control is not visible
  - or the control is destroying
------------------------------------------------------------------------------}
function TControl.AutoSizeCanStart: boolean;
begin
  Result:=AutoSize
          and (not AutoSizing)
          and (not (csDestroying in ComponentState))
          and IsControlVisible;
  {$IFDEF VerboseCanAutoSize}
  if (not Result) and AutoSize then begin
    DbgOut('TControl.AutoSizeCanStart Self='+DbgSName(Self)+' ');
    if not AutoSize then DebugLn('not AutoSize')
    else if AutoSizing then DebugLn('AutoSizing')
    else if csDestroying in ComponentState then DebugLn('csDestroying in ComponentState')
    else if not IsControlVisible then
      DebugLn('Visible=',dbgs(Visible),
              ' csDesigning=',dbgs(csDesigning in ComponentState),
              ' csNoDesignVisible=',dbgs(csNoDesignVisible in ControlStyle))
    else DebugLn('?');
  end;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  function TControl.AutoSizeDelayed: boolean;
  
  Returns true, if the DoAutoSize should skip now, because not all parameters
  needed to calculate the AutoSize bounds are loaded or initialized.
------------------------------------------------------------------------------}
function TControl.AutoSizeDelayed: boolean;
begin
  Result:=(FAutoSizingLockCount>0)
          // no autosize during loading or destruction
          or ([csLoading,csDestroying]*ComponentState<>[])
          // no autosize for invisible controls
          or (not IsControlVisible)
          // if there is no parent, then this control is not visible
          //  (TCustomForm will override this)
          or (NeedParentForAutoSize and (Parent=nil))
          // if there is a parent, ask it
          or ((Parent<>nil) and Parent.AutoSizeDelayed);
  {$IFDEF VerboseCanAutoSize}
  if Result {and AutoSize} then begin
    DbgOut('TControl.AutoSizeDelayed Self='+DbgSName(Self)+' ');
    if FAutoSizingLockCount>0 then debugln('FAutoSizingLockCount=',dbgs(FAutoSizingLockCount))
    else if csLoading in ComponentState then debugln('csLoading')
    else if csDestroying in ComponentState then debugln('csDestroying')
    else if not Visible then debugln('Visible')
    else if NeedParentForAutoSize and (Parent=nil) then debugln('NeedParentForAutoSize and (Parent=nil)')
    else if ((Parent<>nil) and Parent.AutoSizeDelayed) then debugln('Parent.AutoSizeDelayed')
    else debugln('?');
  end;
  {$ENDIF}
end;

function TControl.NeedParentForAutoSize: Boolean;
begin
  Result:=true;
end;

{------------------------------------------------------------------------------
       TControl SetBoundsRect
------------------------------------------------------------------------------}
Procedure TControl.SetBoundsRect(const ARect : TRect);
Begin
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
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
  end else begin
    // during loading the ClientHeight is not used to set the Height of the
    // control, but only to restore autosizing. For example Anchors=[akBottom]
    // needs ClientHeight.
    SetClientSize(Point(ClientWidth, Value));
  end;
end;

{------------------------------------------------------------------------------
  TControl SetClientSize
------------------------------------------------------------------------------}
procedure TControl.SetClientSize(const Value: TPoint);
var
  Client: TRect;
begin
  Client := GetClientRect;
  SetBounds(FLeft, FTop,
            Width - Client.Right + Value.X, Height - Client.Bottom + Value.Y);
end;

{------------------------------------------------------------------------------
  TControl SetClientWidth
------------------------------------------------------------------------------}
procedure TControl.SetClientWidth(Value: Integer);
begin
  if csLoading in ComponentState then begin
    FLoadedClientSize.X:=Value;
    Include(FControlFlags,cfClientWidthLoaded);
  end else begin
    // during loading the ClientWidth is not used to set the Width of the
    // control, but only to restore autosizing. For example Anchors=[akRight]
    // needs ClientWidth.
    SetClientSize(Point(Value, ClientHeight));
  end;
end;

{------------------------------------------------------------------------------
  TControl SetTempCursor
------------------------------------------------------------------------------}
procedure TControl.SetTempCursor(Value: TCursor);
begin
  if Parent<>nil then
    Parent.SetTempCursor(Value);
end;

procedure TControl.ActiveDefaultControlChanged(NewControl: TControl);
begin
end;

procedure TControl.UpdateRolesForForm;
begin
  // called by the form when the "role" controls DefaultControl or CancelControl
  // has changed
end;

{------------------------------------------------------------------------------
  TControl SetCursor
------------------------------------------------------------------------------}
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
  if FFont.IsEqual(Value) then exit;
  FFont.Assign(Value);
  Invalidate;
end;

{------------------------------------------------------------------------------
  TControl SetEnabled
------------------------------------------------------------------------------}
procedure TControl.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value
  then begin
    FEnabled := Value;
    Perform(CM_ENABLEDCHANGED, 0, 0);
  end;
end;

{------------------------------------------------------------------------------
  TControl SetMouseCapture
------------------------------------------------------------------------------}
procedure TControl.SetMouseCapture(Value : Boolean);
begin
  if (MouseCapture <> Value) or (not Value and (CaptureControl=Self))
  then begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.SetMouseCapture ',DbgSName(Self),' NewValue=',DbgS(Value));
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
procedure TControl.Notification(AComponent: TComponent; Operation: TOperation);
var
  Kind: TAnchorKind;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = PopupMenu then
      PopupMenu := nil
    else
    if AComponent = Action then
      Action := nil;
    //debugln('TControl.Notification A ',DbgSName(Self),' ',DbgSName(AComponent));
    for Kind := Low(TAnchorKind) to High(TAnchorKind) do
    begin
      if (FAnchorSides[Kind] <> nil) and (FAnchorSides[Kind].Control = AComponent) then
        FAnchorSides[Kind].FControl := nil;
    end;
  end;
end;

procedure TControl.DoFloatMsg(ADockSource: TDragDockObject);
var
  P: TPoint;
  FloatHost: TWinControl;
begin
  if Floating and (Parent <> nil)then
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
    with ADockSource.DockRect do begin
      Parent.BoundsRect := Bounds(Left + Parent.Left - P.X, Top + Parent.Top - P.Y,
        Right - Left + Parent.Width - Width,  Bottom - Top + Parent.Height - Height);
    end;
  end else begin
    FloatHost := CreateFloatingDockSite(ADockSource.DockRect);
    if FloatHost <> nil then begin
      ADockSource.DragTarget := FloatHost;
    end;
  end;
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
  if TMethod(@Self.GetTextBuf).Code = Pointer(@TControl.GetTextBuf)
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
function TControl.IsCaptionStored : Boolean;
Begin
  Result := (ActionLink = nil) or not ActionLink.IsCaptionLinked;
end;

function TControl.IsClientHeightStored: boolean;
begin
  Result:=false;
end;

function TControl.IsClientWidthStored: boolean;
begin
  Result:=false;
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
    List: TFPList;
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
          if C.IsControlVisible and (csOpaque in ControlStyle) then
          begin
            IntersectRect(R, Rect, BoundsRect);
            if EqualRect(R, Rect) then Exit;
          end;
      end;
    end;
    Result := False;
  end;

begin
  //DebugLn(['TControl.InvalidateControl ',DbgSName(Self)]);
  if (Parent=nil) or (not Parent.HandleAllocated)
  or ([csLoading,csDestroying]*Parent.ComponentState<>[])
  then exit;
  // Note: it should invalidate, when this control is loaded/destroyed, but parent not

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
  //DebugLn(['TControl.InvalidateControl ',DbgSName(Self)]);
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

{------------------------------------------------------------------------------
  TControl Repaint
------------------------------------------------------------------------------}
procedure TControl.Repaint;
var
  DC: HDC;
begin
  if (Parent=nil) or (not Parent.HandleAllocated)
  or (csDestroying in ComponentState) then exit;

  if IsVisible then
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
    if FormIsUpdating then begin
      Include(FControlFlags,cfOnResizeNeeded);
      exit;
    end;
    //if AnsiCompareText('NOTEBOOK',Name)=0 then
    {DebugLn(['[TControl.Resize] ',Name,':',ClassName,
    ' Last=',FLastResizeWidth,',',FLastResizeHeight,
    ' LastClient=',FLastResizeClientWidth,',',FLastResizeClientHeight,
    ' New=',Width,',',Height,
    ' NewClient=',ClientWidth,',',ClientHeight]);}
    FLastResizeWidth:=Width;
    FLastResizeHeight:=Height;
    FLastResizeClientWidth:=ClientWidth;
    FLastResizeClientHeight:=ClientHeight;
    DoOnResize;
  end;
end;

procedure TControl.Loaded;
var
  UseClientWidthForWidth: boolean;
  UseClientHeightForHeight: boolean;
  NewWidth: LongInt;
  NewHeight: LongInt;
begin
  inherited Loaded;
  
  {DebugLn(['TControl.Loaded A ',DbgSName(Self),
    ' LoadedClientWidth=',cfClientWidthLoaded in FControlFlags,'=',FLoadedClientSize.X,
    ' LoadedClientHeight=',cfClientHeightLoaded in FControlFlags,'=',FLoadedClientSize.Y,
    ' LoadedBounds=',DbgS(FReadBounds),
    '']);}
  UseClientWidthForWidth:=(not (cfWidthLoaded in FControlFlags))
                 and (cfClientWidthLoaded in FControlFlags);
  UseClientHeightForHeight:=(not (cfHeightLoaded in FControlFlags))
                 and (cfClientHeightLoaded in FControlFlags);
  if UseClientWidthForWidth or UseClientHeightForHeight then begin
    //DebugLn(['TControl.Loaded ',DbgSName(Self),' Note: Width and/or Height were not set during loading, using ClientWidth/ClientHeight']);
    NewWidth:=Width;
    if UseClientWidthForWidth then
      NewWidth:=FLoadedClientSize.X;
    NewHeight:=Height;
    if UseClientHeightForHeight then
      NewHeight:=FLoadedClientSize.Y;
    SetBoundsKeepBase(Left,Top,NewWidth,NewHeight);
  end;
    
  if Assigned(Parent) then begin
    if ParentColor then begin
      Color := Parent.Color;
      ParentColor := true;
    end;

    if ParentFont then begin
      Font := Parent.Font;
      ParentFont := true;
    end;

    if ParentBidiMode then begin
      BiDiMode := Parent.BiDiMode;
      ParentBidiMode := true;
    end;
  end;

  UpdateBaseBounds(true,true,true);

  // align this control and the brothers
  if cfRequestAlignNeeded in FControlFlags then
    RequestAlign;
  // autosize this control
  if cfAutoSizeNeeded in FControlFlags then
    AdjustSize;
  if Action <> nil then ActionChange(Action, True);
  
  CheckOnChangeBounds;
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
      HelpContext := Self.HelpContext;
      HelpKeyword := Self.HelpKeyword;
      HelpType := Self.HelpType;
    end
  else inherited AssignTo(Dest);
end;

procedure TControl.FormEndUpdated;
// called when control is on a form and EndFormUpdate reached 0
// it is called recursively
begin
  if cfOnResizeNeeded in FControlFlags then
    Resize;
  if cfOnChangeBoundsNeeded in FControlFlags then
    CheckOnChangeBounds;
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
  //DebugLn(['TControl.SetAlign ',DbgSName(Self),' Old=',AlignNames[FAlign],' New=',AlignNames[Value],' ',Anchors<>AnchorAlign[FAlign]]);
  OldAlign:=FAlign;
  FAlign := Value;
  fLastAlignedBoundsTried:=0;
  // if anchors were on default then change them to new default
  // This is done for Delphi compatibility.
  if (Anchors=AnchorAlign[OldAlign]) and (Anchors<>AnchorAlign[FAlign]) then
    Anchors:=AnchorAlign[FAlign]
  else
    RequestAlign;
end;

{------------------------------------------------------------------------------
  TControl SetAnchors
------------------------------------------------------------------------------}
procedure TControl.SetAnchors(const AValue: TAnchors);
begin
  if Anchors=AValue then exit;
  FAnchors:=AValue;
  fLastAlignedBoundsTried:=0;
  RequestAlign;
end;

{------------------------------------------------------------------------------
  TControl RequestAlign

  Requests the parent to realign all brothers
------------------------------------------------------------------------------}
procedure TControl.RequestAlign;
begin
  if (Parent = nil) or (csDestroying in ComponentState) then exit;
  if (csLoading in ComponentState) or (not Parent.HandleAllocated) then begin
    //debugln('TControl.RequestAlign csLoading or not HandleAllocated ',DbgSName(Self));
    Include(FControlFlags,cfRequestAlignNeeded);
    exit;
  end;
  //debugln('TControl.RequestAlign AlignControl ',DbgSName(Self));
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
  //if csDesigning in ComponentState then
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
    DebugLn(['TControl.UpdateBaseBounds '+DbgSName(Self),
    ' OldBounds='+dbgs(FBaseBounds),
    ' OldClientSize='+dbgs(FBaseParentClientSize),
    ' NewBounds='+dbgs(NewBaseBounds),
    ' NewClientSize='+dbgs(NewBaseParentClientSize),
    '']);
  {$ENDIF}
  FBaseBounds:=NewBaseBounds;
  Include(FControlFlags,cfBaseBoundsValid);
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

procedure TControl.WriteLayoutDebugReport(const Prefix: string);
var
  a: TAnchorKind;
  NeedSeparator: Boolean;
begin
  DbgOut(Prefix,'TControl.WriteLayoutDebugReport ');
  DbgOut(DbgSName(Self),' Bounds=',dbgs(BoundsRect));
  if Align<>alNone then
    DbgOut(' Align=',AlignNames[Align]);
  DbgOut(' Anchors=[');
  NeedSeparator:=false;
  for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
    if a in Anchors then begin
      if NeedSeparator then DbgOut(',');
      DbgOut(dbgs(a));
      if AnchorSide[a].Control<>nil then begin
        DbgOut('(',DbgSName(AnchorSide[a].Control),')');
      end;
      NeedSeparator:=true;
    end;
  end;
  DbgOut(']');
  DebugLn;
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

function TControl.GetDefaultDockCaption: String;
begin
  Result := '';
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
  if CheckPosition(Self) then
  DebugLn('[TControl.SetLeft] ',Name,':',ClassName,' ',DbgS(Value));
  {$ENDIF}
  if csLoading in ComponentState then begin
    inc(FReadBounds.Right,Value-FReadBounds.Left);
    FReadBounds.Left:=Value;
    Include(FControlFlags,cfLeftLoaded);
  end;
  SetBounds(Value, FTop, FWidth, FHeight);
end;

{------------------------------------------------------------------------------
  TControl SetTop
------------------------------------------------------------------------------}
procedure TControl.SetTop(Value: Integer);
begin
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
  DebugLn('[TControl.SetTop] ',Name,':',ClassName,' ',Dbgs(Value));
  {$ENDIF}
  if csLoading in ComponentState then begin
    inc(FReadBounds.Bottom,Value-FReadBounds.Top);
    FReadBounds.Top:=Value;
    Include(FControlFlags,cfTopLoaded);
  end;
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
  if CheckPosition(Self) then
  DebugLn('[TControl.SetWidth] ',Name,':',ClassName,' ',dbgs(Value));
  {$ENDIF}
  if csLoading in ComponentState then begin
    FReadBounds.Right:=FReadBounds.Left+Value;
    Include(FControlFlags,cfWidthLoaded);
  end;
  if [csDesigning,csDestroying,csLoading]*ComponentState=[csDesigning] then
    CheckDesignBounds;
  SetBounds(FLeft, FTop, Max(0,Value), FHeight);
end;

function TControl.GetCursor: TCursor;
begin
  Result := FCursor;
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
  if CheckPosition(Self) then
  DebugLn('[TControl.SetHeight] ',Name,':',ClassName,' ',dbgs(Value));
  {$ENDIF}
  if csLoading in ComponentState then begin
    FReadBounds.Bottom:=FReadBounds.Top+Value;
    Include(FControlFlags,cfHeightLoaded);
  end;
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
  if not (csLoading in ComponentState) then
    FHelpType := htContext;
  FHelpContext:=AValue;
end;

{------------------------------------------------------------------------------
  procedure TControl.SetHelpKeyword(const AValue: String);
------------------------------------------------------------------------------}
procedure TControl.SetHelpKeyword(const AValue: String);
begin
  if FHelpKeyword=AValue then exit;
  if not (csLoading in ComponentState) then
    FHelpType := htKeyword;
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
procedure TControl.SetParent(NewParent: TWinControl);
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

procedure TControl.SetParentFont(Value: Boolean);
begin
  if FParentFont <> Value then
  begin
    FParentFont := Value;
    if Assigned(FParent) and not (csReading in ComponentState) then
      ParentFontChanged;
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

{------------------------------------------------------------------------------
  TControl SetPopupMenu
------------------------------------------------------------------------------}
procedure TControl.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu :=  Value;
end;

{------------------------------------------------------------------------------
  TControl WMMouseMove
------------------------------------------------------------------------------}
procedure TControl.WMMouseMove(Var Message: TLMMouseMove);
begin
  {$IFDEF VerboseMouseBugfix}
  DebugLn(['[TControl.WMMouseMove] ',Name,':',ClassName,' ',Message.XPos,',',Message.YPos]);
  {$ENDIF}
  DoBeforeMouseMessage;
  UpdateMouseCursor(Message.XPos,Message.YPos);
  if not (csNoStdEvents in ControlStyle)
  then with Message do
    MouseMove(KeystoShiftState(Keys), XPos, YPos);
end;

{------------------------------------------------------------------------------
  TControl MouseDown
------------------------------------------------------------------------------}
procedure TControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TPoint;
  Form: TCustomForm;
begin
  if (not (Self is TWinControl)) or (not TWinControl(Self).CanFocus) then
  begin
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.ActiveControl <> nil) then
      Form.ActiveControl.EditingDone;
  end;

  if (Button in [mbLeft, mbRight]) and DragManager.IsDragging then
  begin
    P := ClientToScreen(Point(X,Y));
    DragManager.MouseDown(Button, Shift, P.X, P.Y);
    exit;
  end;

  if Assigned(FOnMouseDown) then FOnMouseDown(Self, Button, Shift, X,Y);
end;

{------------------------------------------------------------------------------
  TControl MouseMove
------------------------------------------------------------------------------}
procedure TControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if DragManager.IsDragging then
  begin
    P := ClientToScreen(Point(X, Y));
    DragManager.MouseMove(Shift, P.X, P.Y);
    exit;
  end;

  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X,Y);
end;

{------------------------------------------------------------------------------
  TControl MouseUp
------------------------------------------------------------------------------}
procedure TControl.MouseUp(Button: TMouseButton; Shift:TShiftState;
  X, Y: Integer);
var
  P: TPoint;
begin
  if (Button in [mbLeft, mbRight]) and DragManager.IsDragging then
  begin
    P := ClientToScreen(Point(X, Y));
    DragManager.MouseUp(Button, Shift, P.X, P.Y);
    exit;
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
  if DragManager.IsDragging then
    DragManager.CaptureChanged(Self);
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

{------------------------------------------------------------------------------
  TControl SetVisible

------------------------------------------------------------------------------}
procedure TControl.SetVisible(Value : Boolean);
var
  AsWincontrol: TWinControl;
begin
  if FVisible <> Value then begin
    //DebugLn(['TControl.SetVisible ',DbgSName(Self),' NewVisible=',Value]);
    VisibleChanging;
    FVisible := Value;
    try
      Perform(CM_VISIBLECHANGED, WParam(Ord(Value)), 0);
      Include(FControlFlags,cfRequestAlignNeeded);
      if (Self is TWinControl) then
        AsWincontrol:=TWinControl(Self)
      else
        AsWincontrol:=nil;
      if FVisible then begin
        if AsWincontrol<>nil then
          Include(AsWincontrol.FWinControlFlags,wcfReAlignNeeded);
        AdjustSize;
        if (AsWincontrol<>nil) and (wcfReAlignNeeded in AsWincontrol.FWinControlFlags) then
          AsWincontrol.ReAlign;
      end;
      if cfRequestAlignNeeded in FControlFlags then
        RequestAlign;
      if AsWincontrol<>nil then
        AsWincontrol.ResizeDelayedAutoSizeChildren;
    finally
      VisibleChanged;
    end;
  end;
  if (csLoading in ComponentState) then
    ControlState:=ControlState+[csVisibleSetInLoading];
end;

{------------------------------------------------------------------------------
       TControl.SetZOrder

------------------------------------------------------------------------------}
procedure TControl.SetZOrder(Topmost: Boolean);
const
  POSITION: array[Boolean] of Integer = (0, MaxInt);
begin
  if FParent = nil then exit;
  FParent.SetChildZPosition(Self, POSITION[TopMost]);
end;


{------------------------------------------------------------------------------
  function TControl.HandleObjectShouldBeVisible
------------------------------------------------------------------------------}
function TControl.HandleObjectShouldBeVisible: boolean;
begin
  Result:=(Visible
          or ((csDesigning in ComponentState)
              and not (csNoDesignVisible in ControlStyle)));
end;

{------------------------------------------------------------------------------
  procedure TControl Hide
------------------------------------------------------------------------------}
procedure TControl.Hide;
begin
  Visible := False;
end;

{------------------------------------------------------------------------------
  function TControl.ParentDestroyingHandle: boolean;
  
  Returns whether any parent is destroying it's handle (and its children's)
 ------------------------------------------------------------------------------}
function TControl.ParentDestroyingHandle: boolean;
var
  CurControl: TControl;
begin
  Result:=true;
  CurControl:=Self;
  while CurControl<>nil do begin
    if csDestroyingHandle in CurControl.ControlState then
      exit;
    CurControl:=CurControl.Parent;
  end;
  Result:=false;
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

procedure TControl.ShowHelp;
begin
  if HelpType=htContext then begin
    if HelpContext<>0 then begin
      Application.HelpContext(Self,ClientToScreen(Point(0,0)),HelpContext);
      exit;
    end;
  end else begin
    if HelpKeyword<>'' then begin
      Application.HelpKeyword(Self,ClientToScreen(Point(0,0)),HelpKeyword);
      exit;
    end;
  end;
  if Parent<>nil then Parent.ShowHelp;
end;

function TControl.HasHelp: Boolean;
begin
  if HelpType=htContext then
    Result:=HelpContext<>0
  else
    Result:=HelpKeyword<>'';
end;

{------------------------------------------------------------------------------
  procedure TControl.Dock(NewDockSite: TWinControl; ARect: TRect);
  
  Docks this control into NewDockSite at ARect.
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

    //debugln('TControl.Dock A ',DbgSName(Self));

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

  Docks this control to DropControl or on NewDockSite.
  If DropControl is not nil, ControlSide defines on which side of DropControl
  this control is docked. (alNone,alClient for stacked in pages). DropControl
  will become part of a TDockManager.
  If DropControl is nil, then DropControl becomes a normal child of NewDockSite
  and ControlSide is ignored.
------------------------------------------------------------------------------}
function TControl.ManualDock(NewDockSite: TWinControl; DropControl: TControl;
  ControlSide: TAlign; KeepDockSiteSize: Boolean): Boolean;
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
      // - this only undocks from the DockManager
      // - this control still uses the DockSite as parent control
      DebugLn('TControl.ManualDock UNDOCKING ',Name);
      Result:=HostDockSite.DoUndock(NewDockSite,Self);
    end;
    
    if Result then begin
      DebugLn('TControl.ManualDock DOCKING ',Name);
      // create TDragDockObject for docking parameters
      DockObject := TDragDockObject.Create(Self);
      try
        // get current screen coordinates
        NewPosition:=ControlOrigin;
        // initialize DockObject
        with DockObject do begin
          FDragTarget := NewDockSite;
          FDropAlign := ControlSide;
          FDropOnControl := DropControl;
          FIncreaseDockArea := not KeepDockSiteSize;
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
  function TControl.ManualFloat(TheScreenRect: TRect;
    KeepDockSiteSize: Boolean = true): Boolean;

  Undock and float.
  Float means here: create the floating dock site and dock this control into it.
  Exception: Forms do not need float dock sites and float on their own.
------------------------------------------------------------------------------}
function TControl.ManualFloat(TheScreenRect: TRect;
  KeepDockSiteSize: Boolean): Boolean;
var
  FloatHost: TWinControl;
begin
  DebugLn(['TControl.ManualFloat ',DbgSName(Self)]);
  // undock from old host dock site
  if HostDockSite=nil then
  begin
    Result := true;
    if Parent<>nil then
      Parent.DoUndockClientMsg(nil,Self);
  end else begin
    Result := HostDockSite.DoUndock(nil,Self,KeepDockSiteSize);
  end;
  // create new float dock site and dock this control into it.
  if Result then begin
    FloatHost := CreateFloatingDockSite(TheScreenRect);
    //debugln('TControl.ManualFloat A '+Name,':',ClassName,' ',dbgs(TheScreenRect),' FloatHost=',dbgs(FloatHost<>nil));
    if FloatHost<>nil then  begin
      // => dock this control into it.
      FloatHost.Visible := true;
      Dock(FloatHost,Rect(0,0,FloatHost.ClientWidth,FloatHost.ClientHeight))
    end else
      Dock(nil,TheScreenRect);
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

procedure TControl.AddHandlerOnResize(const OnResizeEvent: TNotifyEvent;
  AsLast: boolean);
begin
  AddHandler(chtOnResize,TMethod(OnResizeEvent),AsLast);
end;

procedure TControl.RemoveHandlerOnResize(const OnResizeEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnResize,TMethod(OnResizeEvent));
end;

procedure TControl.AddHandlerOnChangeBounds(
  const OnChangeBoundsEvent: TNotifyEvent; AsLast: boolean);
begin
  AddHandler(chtOnChangeBounds,TMethod(OnChangeBoundsEvent),AsLast);
end;

procedure TControl.RemoveHandlerOnChangeBounds(
  const OnChangeBoundsEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnChangeBounds,TMethod(OnChangeBoundsEvent));
end;

procedure TControl.AddHandlerOnVisibleChanging(
  const OnVisibleChangingEvent: TNotifyEvent; AsLast: boolean);
begin
  AddHandler(chtOnVisibleChanging,TMethod(OnVisibleChangingEvent));
end;

procedure TControl.RemoveHandlerOnVisibleChanging(
  const OnVisibleChangingEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnVisibleChanging,TMethod(OnVisibleChangingEvent));
end;

procedure TControl.AddHandlerOnVisibleChanged(
  const OnVisibleChangedEvent: TNotifyEvent; AsLast: boolean);
begin
  AddHandler(chtOnVisibleChanged,TMethod(OnVisibleChangedEvent));
end;

procedure TControl.RemoveHandlerOnVisibleChanged(
  const OnVisibleChangedEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnVisibleChanged,TMethod(OnVisibleChangedEvent));
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

{------------------------------------------------------------------------------
  TControl RealSetText
------------------------------------------------------------------------------}
procedure TControl.RealSetText(const Value: TCaption);
begin
  if FCaption = Value then Exit;
  FCaption := Value;
  TextChanged;
end;

procedure TControl.TextChanged;
begin
end;

function TControl.GetCachedText(var CachedText: TCaption): boolean;
begin
  CachedText := FCaption;
  Result:= true;
end;

{------------------------------------------------------------------------------
  TControl SetText
------------------------------------------------------------------------------}
procedure TControl.SetText(const Value: TCaption);
begin
  //if CompareText(Name,'TextToFindComboBox')=0 then
  //  debugln('TControl.SetText A ',DbgSName(Self),' GetText="',GetText,'" Value="',Value,'" FCaption="',FCaption,'"');
  if GetText = Value then Exit;
  
  // Check if SetTextBuf is overridden, otherwise
  // we can call RealSetText directly
  if TMethod(@Self.SetTextBuf).Code = Pointer(@TControl.SetTextBuf)
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
  i: Integer;
  CurAnchorSide: TAnchorSide;
begin
  //DebugLn('[TControl.Destroy] A ',Name,':',ClassName);
  // make sure the capture is released
  MouseCapture := false;
  Application.ControlDestroyed(Self);
  if (FHostDockSite <> nil) and not (csDestroying in FHostDockSite.ComponentState) then
  begin
    FHostDockSite.DoUndockClientMsg(nil, Self);
    SetParent(nil);
    Dock(nil, BoundsRect);
    FHostDockSite := nil;
  end else
    SetParent(nil);
  FreeThenNil(FActionLink);
  for Side:=Low(FAnchorSides) to High(FAnchorSides) do
    FreeThenNil(FAnchorSides[Side]);
  FreeThenNil(FBorderSpacing);
  FreeThenNil(FConstraints);
  if fAnchoredControls<>nil then begin
    for i:=0 to fAnchoredControls.Count-1 do
      for Side:=Low(TAnchorKind) to High(TAnchorKind) do begin
        CurAnchorSide:=AnchoredControls[i].AnchorSide[Side];
        if CurAnchorSide.FControl=Self then
          CurAnchorSide.FControl:=nil;
      end;
    FreeThenNil(fAnchoredControls);
  end;
  FreeThenNil(FFont);
  //DebugLn('[TControl.Destroy] B ',DbgSName(Self));
  inherited Destroy;
  //DebugLn('[TControl.Destroy] END ',DbgSName(Self));
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

  // no csOpaque: delphi compatible, win32 themes notebook depend on it
  // csOpaque means entire client area will be drawn 
  // (most controls are semi-transparent)
  FControlStyle := FControlStyle
                 +[csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
  FConstraints:= TSizeConstraints.Create(Self);
  FBorderSpacing:=TControlBorderSpacing.Create(Self);
  for Side:=Low(FAnchorSides) to High(FAnchorSides) do
    FAnchorSides[Side]:=TAnchorSide.Create(Self,Side);

  FAnchors := [akLeft,akTop];
  FAlign := alNone;
  FCaptureMouseButtons := [mbLeft];
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
  FParentBidiMode := True;
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

function TControl.GetTopParent: TControl;
begin
  Result:=Self;
  while Result.Parent<>nil do
    Result:=Result.Parent;
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
  if Parent<>nil then Parent.DisableAlign;
  try
    case Side of
    akLeft: BorderSpacing.Left:=Space;
    akTop: BorderSpacing.Top:=Space;
    akRight: BorderSpacing.Right:=Space;
    akBottom: BorderSpacing.Bottom:=Space;
    end;
    AnchorSide[Side].Side:=DefaultSideForAnchorKind[Side];
    AnchorSide[Side].Control:=Sibling;
    Anchors:=Anchors+[Side];
  finally
    if Parent<>nil then Parent.EnableAlign;
  end;
end;

procedure TControl.AnchorParallel(Side: TAnchorKind; Space: integer;
  Sibling: TControl);
begin
  if Parent<>nil then Parent.DisableAlign;
  try
    case Side of
    akLeft: BorderSpacing.Left:=Space;
    akTop: BorderSpacing.Top:=Space;
    akRight: BorderSpacing.Right:=Space;
    akBottom: BorderSpacing.Bottom:=Space;
    end;
    case Side of
    akLeft: AnchorSide[Side].Side:=asrLeft;
    akTop: AnchorSide[Side].Side:=asrTop;
    akRight: AnchorSide[Side].Side:=asrRight;
    akBottom: AnchorSide[Side].Side:=asrBottom;
    end;
    AnchorSide[Side].Control:=Sibling;
    Anchors:=Anchors+[Side];
  finally
    if Parent<>nil then Parent.EnableAlign;
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.AnchorHorizontalCenterTo(Sibling: TControl);

  Setup AnchorSide to center the control horizontally relative to a sibling.
 ------------------------------------------------------------------------------}
procedure TControl.AnchorHorizontalCenterTo(Sibling: TControl);
begin
  if Parent<>nil then Parent.DisableAlign;
  try
    AnchorSide[akLeft].Side:=asrCenter;
    AnchorSide[akLeft].Control:=Sibling;
    Anchors:=Anchors+[akLeft]-[akRight];
  finally
    if Parent<>nil then Parent.EnableAlign;
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.AnchorVerticalCenterTo(Sibling: TControl);

  Setup AnchorSide to center the control vertically relative to a sibling.
 ------------------------------------------------------------------------------}
procedure TControl.AnchorVerticalCenterTo(Sibling: TControl);
begin
  if Parent<>nil then Parent.DisableAlign;
  try
    AnchorSide[akTop].Side:=asrCenter;
    AnchorSide[akTop].Control:=Sibling;
    Anchors:=Anchors+[akTop]-[akBottom];
  finally
    if Parent<>nil then Parent.EnableAlign;
  end;
end;

procedure TControl.AnchorToCompanion(Side: TAnchorKind; Space: integer;
  Sibling: TControl; FreeCompositeSide: boolean);

  procedure AnchorCompanionSides(
    ResizeSide,// the side of this control, where Sibling is touched and moved
    OppositeResizeSide, // opposite of ResizeSide
    FixedSide1,// the first non moving side
    FixedSide2:// the second non moving side
      TAnchorKind);
  begin
    if not (OppositeAnchor[Side] in Anchors) then
      AnchorSide[OppositeResizeSide].Control:=nil;
    AnchorToNeighbour(ResizeSide,Space,Sibling);
    AnchorParallel(FixedSide1,0,Sibling);
    AnchorParallel(FixedSide2,0,Sibling);
  end;

var
  NewAnchors: TAnchors;
begin
  if Parent<>nil then Parent.DisableAlign;
  try
    // anchor all. Except for the opposite side.
    NewAnchors:=[akLeft,akTop,akRight,akBottom];
    if FreeCompositeSide or (not (OppositeAnchor[Side] in Anchors)) then
      Exclude(NewAnchors,OppositeAnchor[Side]);
    Anchors:=NewAnchors;

    case Side of
    akLeft:   AnchorCompanionSides(akLeft,akRight,akTop,akBottom);
    akRight:  AnchorCompanionSides(akRight,akLeft,akTop,akBottom);
    akTop:    AnchorCompanionSides(akTop,akBottom,akLeft,akRight);
    akBottom: AnchorCompanionSides(akBottom,akTop,akLeft,akRight);
    end;
  finally
    if Parent<>nil then Parent.EnableAlign;
  end;
end;

procedure TControl.AnchorSame(Side: TAnchorKind; Sibling: TControl);
begin
  if Parent<>nil then Parent.DisableAlign;
  try
    if Side in Sibling.Anchors then
      Anchors:=Anchors+[Side]
    else
      Anchors:=Anchors-[Side];
    AnchorSide[Side].Assign(Sibling.AnchorSide[Side]);
  finally
    if Parent<>nil then Parent.EnableAlign;
  end;
end;

procedure TControl.AnchorAsAlign(TheAlign: TAlign; Space: Integer);
begin
  Parent.DisableAlign;
  try
    if akLeft in AnchorAlign[TheAlign] then begin
      BorderSpacing.Left:=Space;
      AnchorSide[akLeft].Side:=asrLeft;
      AnchorSide[akLeft].Control:=Parent;
    end;
    if akTop in AnchorAlign[TheAlign] then begin
      BorderSpacing.Top:=Space;
      AnchorSide[akTop].Side:=asrTop;
      AnchorSide[akTop].Control:=Parent;
    end;
    if akRight in AnchorAlign[TheAlign] then begin
      BorderSpacing.Right:=Space;
      AnchorSide[akRight].Side:=asrRight;
      AnchorSide[akRight].Control:=Parent;
    end;
    if akBottom in AnchorAlign[TheAlign] then begin
      BorderSpacing.Bottom:=Space;
      AnchorSide[akBottom].Side:=asrBottom;
      AnchorSide[akBottom].Control:=Parent;
    end;
    Anchors:=Anchors+AnchorAlign[TheAlign];
  finally
    Parent.EnableAlign;
  end;
end;

procedure TControl.AnchorClient(Space: Integer);
begin
  AnchorAsAlign(alClient,Space);
end;

function TControl.AnchoredControlCount: integer;
begin
  if fAnchoredControls=nil then
    Result:=0
  else
    Result:=fAnchoredControls.Count;
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
    var PreferredWidth, PreferredHeight: integer; Raw: boolean;
    WithThemeSpace: Boolean);
    
  Returns the default/preferred width and height for a control, which is used
  by the LCL autosizing algorithms as default size. Only positive values are
  valid. Negative or 0 are treated as undefined and the LCL uses other sizes
  instead.

  Raw: If not Raw then the values will be adjusted by the constraints and
  undefined values will be replaced by GetDefaultWidth/GetDefaultHeight.

  WithThemeSpace: If true, adds space for stacking. For example: TRadioButton
  has a minimum size. But for stacking multiple TRadioButtons there should be
  some space around. This space is theme dependent, so it passed parameter to
  the widgetset.

  TWinControl overrides this and asks the interface for theme dependent values.
  See TWinControl.GetPreferredSize for more information.
 ------------------------------------------------------------------------------}
procedure TControl.GetPreferredSize(var PreferredWidth,
  PreferredHeight: integer; Raw: boolean; WithThemeSpace: boolean);
begin
  if WithThemeSpace then begin
    if not (cfPreferredSizeValid in FControlFlags) then begin
      CalculatePreferredSize(FPreferredWidth,FPreferredHeight,true);
      Include(FControlFlags,cfPreferredSizeValid);
    end;
    PreferredWidth:=FPreferredWidth;
    PreferredHeight:=FPreferredHeight;
  end else begin
    if not (cfPreferredMinSizeValid in FControlFlags) then begin
      CalculatePreferredSize(FPreferredMinWidth,FPreferredMinHeight,false);
      Include(FControlFlags,cfPreferredMinSizeValid);
    end;
    PreferredWidth:=FPreferredMinWidth;
    PreferredHeight:=FPreferredMinHeight;
  end;

  if not Raw then begin
    // use defaults for undefined preferred size
    if AutoSize then begin
      if PreferredWidth<=0 then PreferredWidth:=GetDefaultWidth;
      if PreferredHeight<=0 then PreferredHeight:=GetDefaultHeight;
    end else begin
      if PreferredWidth<=0 then PreferredWidth:=Width;
      if PreferredHeight<=0 then PreferredHeight:=Height;
    end;
    
    // if this control is aligned adjust PreferredWidth and/or PreferredHeight
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
  function TControl.GetDefaultWidth: integer;

  The default width for this control independent of any calculated values
  like Width and GetPreferredSize.
 ------------------------------------------------------------------------------}
function TControl.GetDefaultWidth: integer;
begin
  if cfBaseBoundsValid in FControlFlags then
    Result:=FBaseBounds.Right-FBaseBounds.Left
  else if cfWidthLoaded in FControlFlags then
    Result:=FReadBounds.Right-FReadBounds.Left
  else
    Result:=GetControlClassDefaultSize.X;
end;

{------------------------------------------------------------------------------
  function TControl.GetDefaultHeight: integer;

  The default height for this control independent of any calculated values
  like Height and GetPreferredSize.
 ------------------------------------------------------------------------------}
function TControl.GetDefaultHeight: integer;
begin
  if cfBaseBoundsValid in FControlFlags then
    Result:=BaseBounds.Bottom-BaseBounds.Top
  else if cfHeightLoaded in FControlFlags then
    Result:=FReadBounds.Bottom-FReadBounds.Top
  else
    Result:=GetControlClassDefaultSize.Y;
end;

{------------------------------------------------------------------------------
  class function TControl.GetControlClassDefaultSize: TPoint;

  The default size of this type of controls.
  Used by GetDefaultWidth and GetDefaultHeight.
 ------------------------------------------------------------------------------}
class function TControl.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=75;
  Result.Y:=50;
end;

{------------------------------------------------------------------------------
  procedure TControl.CNPreferredSizeChanged;

  Utility function to retrieve Left,Top,Right and Bottom.
 ------------------------------------------------------------------------------}
function TControl.GetSidePosition(Side: TAnchorKind): integer;
begin
  case Side of
  akLeft: Result:=Left;
  akTop: Result:=Top;
  akRight: Result:=Left+Width;
  akBottom: Result:=Top+Height;
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.CNPreferredSizeChanged;

  Called by the LCL interface, when something changed that effects the result
  of the interface values for GetPreferredSize.
 ------------------------------------------------------------------------------}
procedure TControl.CNPreferredSizeChanged;
begin
  InvalidatePreferredSize;
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
    Exclude(AControl.FControlFlags,cfPreferredMinSizeValid);
    AControl:=AControl.Parent;
  end;
end;

function TControl.GetAnchorsDependingOnParent(WithNormalAnchors: Boolean
  ): TAnchors;
var
  a: TAnchorKind;
begin
  Result:=[];
  if Parent=nil then exit;

  if (Anchors*[akLeft,akRight]=[]) then begin
    // center horizontally
    Result:=Result+[akLeft,akRight];
  end;
  if (Anchors*[akTop,akBottom]=[]) then begin
    // center vertically
    Result:=Result+[akTop,akBottom];
  end;

  for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
    if (a in (Anchors+AnchorAlign[Align])) then begin
      if WithNormalAnchors
      or (AnchorSide[a].Control=Parent)
      or ((AnchorSide[a].Control=nil) and (a in [akRight,akBottom])) then begin
        // side anchored
        Include(Result,a);
      end;
    end;
  end;
end;

procedure TControl.DisableAutoSizing;
begin
  inc(FAutoSizingLockCount);
end;

procedure TControl.EnableAutoSizing;

  procedure AdjustSizeRecursive(AControl: TControl);
  var
    AWinControl: TWinControl;
    i: Integer;
  begin
    if cfAutoSizeNeeded in AControl.FControlFlags then
      AControl.AdjustSize;
    if AControl is TWinControl then begin
      AWinControl:=TWinControl(AControl);
      if AWincontrol.FWinControls<>nil then
        for i:=0 to AWincontrol.FWinControls.Count-1 do
          AdjustSizeRecursive(TControl(AWincontrol.FWinControls[i]));
    end;
  end;

begin
  if FAutoSizingLockCount<=0 then RaiseGDBException('TControl.EnableAutoSizing');
  dec(FAutoSizingLockCount);
  if FAutoSizingLockCount=0 then
    AdjustSizeRecursive(Self);
end;

procedure TControl.EndAutoSizing;
  procedure Error;
  begin
    RaiseGDBException('TControl.EndAutoSizing');
  end;
begin
  if not FAutoSizing then Error;
  FAutoSizing := False;
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
  if CheckPosition(Self) then
  DebugLn('[TControl.WMSize] Name=',Name,':',ClassName,' Message.Width=',DbgS(Message.Width),' Message.Height=',DbgS(Message.Height),' Width=',DbgS(Width),' Height=',DbgS(Height));
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
  if CheckPosition(Self) then
  DebugLn('[TControl.WMMove] Name=',Name,':',ClassName,' Message.XPos=',DbgS(Message.XPos),' Message.YPos=',DbgS(Message.YPos),' OldLeft=',DbgS(Left),' OldTop=',DbgS(Top));
  {$ENDIF}
  { Just sync the coordinates }
  SetBoundsKeepBase(Message.XPos, Message.YPos, Width, Height,Parent<>nil);
end;

{------------------------------------------------------------------------------
   Method:  TControl.SetBiDiMode
 ------------------------------------------------------------------------------}

procedure TControl.SetBiDiMode(const AValue: TBiDiMode);
begin
  if FBiDiMode=AValue then exit;
  FBiDiMode:=AValue;
  FParentBiDiMode := False;
  Perform(CM_BIDIMODECHANGED, 0, 0);
end;

{------------------------------------------------------------------------------
   Method:  TControl.SetParentBiDiMode
 ------------------------------------------------------------------------------}

procedure TControl.SetParentBiDiMode(const AValue: Boolean);
begin
  if FParentBiDiMode=AValue then exit;
  FParentBiDiMode:=AValue;
  if (FParent <> nil) and not (csReading in ComponentState) then
    Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
end;

{------------------------------------------------------------------------------
   Method:  TControl.CMBiDiModeChanged
 ------------------------------------------------------------------------------}

procedure TControl.CMBiDiModeChanged(var Message: TLMessage);
begin
  if (Message.wParam = 0) then
    Invalidate;
end;

{------------------------------------------------------------------------------
       TControl.CMParentBidiModeChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}

procedure TControl.CMParentBidiModeChanged(var Message: TLMessage);
begin
  if csLoading in ComponentState then exit;

  if FParentBidiMode then
  begin
    if FParent <> nil then
      BidiMode := FParent.BidiMode;
    FParentBidiMode := true;
  end;
end;

{------------------------------------------------------------------------------
       TControl.IsBiDiModeStored
------------------------------------------------------------------------------}
function TControl.IsBiDiModeStored: boolean;
begin
  Result := not FParentBidiMode;
end;


{------------------------------------------------------------------------------
       TControl.IsRightToLeft
------------------------------------------------------------------------------}

function TControl.IsRightToLeft: Boolean;
begin
  Result := UseRightToLeftReading;
end;

{------------------------------------------------------------------------------
       TControl.UseRightToLeftAlignment
------------------------------------------------------------------------------}

function TControl.UseRightToLeftAlignment: Boolean;
begin
  Result := (BiDiMode = bdRightToLeft);
end;

{------------------------------------------------------------------------------
       TControl.UseRightToLeftReading
------------------------------------------------------------------------------}

function TControl.UseRightToLeftReading: Boolean;
begin
  Result := (BiDiMode <> bdLeftToRight);
end;

{------------------------------------------------------------------------------
       TControl.UseRightToLeftScrollBar
------------------------------------------------------------------------------}

function TControl.UseRightToLeftScrollBar: Boolean;
begin
  Result := (BiDiMode in [bdRightToLeft, bdRightToLeftNoAlign]);
end;

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}

// included by controls.pp
