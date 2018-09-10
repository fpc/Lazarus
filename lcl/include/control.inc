{%MainUnit ../controls.pp}

{******************************************************************************
                                     TControl
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{ $DEFINE CHECK_POSITION}

{ TLazAccessibleObjectEnumerator }

function TLazAccessibleObjectEnumerator.GetCurrent: TLazAccessibleObject;
begin
  Result:=TLazAccessibleObject(FCurrent.Data);
end;

{ TLazAccessibleObject }

function TLazAccessibleObject.GetHandle: PtrInt;
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  if (WidgetsetClass <> nil) and (FHandle = 0) then
  begin
    FHandle := WidgetsetClass.CreateHandle(Self);
    InitializeHandle();
  end;
  Result := FHandle;
end;

function TLazAccessibleObject.GetAccessibleValue: TCaption;
begin
  Result := FAccessibleValue;
end;

function TLazAccessibleObject.GetPosition: TPoint;
begin
  if (OwnerControl <> nil) and (OwnerControl.GetAccessibleObject() = Self) then
  begin
    Result := Point(OwnerControl.Left, OwnerControl.Top);
    Exit;
  end;
  Result := FPosition;
end;

function TLazAccessibleObject.GetSize: TSize;
begin
  if (OwnerControl <> nil) and (OwnerControl.GetAccessibleObject() = Self) then
  begin
    Result := Types.Size(OwnerControl.Width, OwnerControl.Height);
    Exit;
  end;
  Result := FSize;
end;

procedure TLazAccessibleObject.SetHandle(AValue: PtrInt);
begin
  if AValue = FHandle then Exit;
  FHandle := AValue;
  InitializeHandle();
end;

procedure TLazAccessibleObject.SetPosition(AValue: TPoint);
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  if (FPosition.X=AValue.X) and (FPosition.Y=AValue.Y) then Exit;
  FPosition := AValue;
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  WidgetsetClass.SetPosition(Self, AValue);
end;

procedure TLazAccessibleObject.SetSize(AValue: TSize);
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  if (FSize.CX=AValue.CX) and (FSize.CY=AValue.CY) then Exit;
  FSize := AValue;
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  WidgetsetClass.SetSize(Self, AValue);
end;

class procedure TLazAccessibleObject.WSRegisterClass;
begin
//  inherited WSRegisterClass;
  RegisterLazAccessibleObject;
end;

constructor TLazAccessibleObject.Create(AOwner: TControl);
begin
  inherited Create;//(AOwner);
  OwnerControl := AOwner;
  FChildrenSortedForDataObject := TAvlTree.Create(@CompareDataObjectWithLazAccessibleObject);
  WSRegisterClass();
end;

destructor TLazAccessibleObject.Destroy;
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  ClearChildAccessibleObjects();
  if (WidgetsetClass <> nil) and (FHandle <> 0) then
    WidgetsetClass.DestroyHandle(Self);
  FreeAndNil(FChildrenSortedForDataObject);
  inherited Destroy;
end;

function TLazAccessibleObject.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TLazAccessibleObject.InitializeHandle;
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  WidgetsetClass.SetAccessibleDescription(Self, FAccessibleDescription);
  WidgetsetClass.SetAccessibleValue(Self, FAccessibleValue);
  WidgetsetClass.SetAccessibleRole(Self, FAccessibleRole);
end;

procedure TLazAccessibleObject.SetAccessibleDescription(const ADescription: TCaption);
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  if FAccessibleDescription=ADescription then Exit;
  FAccessibleDescription := ADescription;
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  WidgetsetClass.SetAccessibleDescription(Self, ADescription);
end;

procedure TLazAccessibleObject.SetAccessibleValue(const AValue: TCaption);
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  if FAccessibleValue=AValue then Exit;
  FAccessibleValue := AValue;
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  WidgetsetClass.SetAccessibleValue(Self, AValue);
end;

procedure TLazAccessibleObject.SetAccessibleRole(const ARole: TLazAccessibilityRole);
var
  WidgetsetClass: TWSLazAccessibleObjectClass;
begin
  if FAccessibleRole=ARole then Exit;
  FAccessibleRole := ARole;
  WidgetsetClass := TWSLazAccessibleObjectClass(GetWSLazAccessibleObject());
  WidgetsetClass.SetAccessibleRole(Self, ARole);
end;

function TLazAccessibleObject.FindOwnerWinControl: TWinControl;
begin
  Result := nil;
  if (OwnerControl <> nil) and (OwnerControl is TWinControl) then Exit(OwnerControl as TWinControl);
  if Self.Parent = nil then Exit;
  Result := Self.Parent.FindOwnerWinControl();
end;

function TLazAccessibleObject.AddChildAccessibleObject: TLazAccessibleObject;
begin
  Result := nil;
  if FChildrenSortedForDataObject = nil then Exit;
  Result := TLazAccessibleObject.Create(OwnerControl);
  Result.Parent := Self;
  FChildrenSortedForDataObject.Add(Result);
  //DebugLn('[TControl.AddChildAccessibleObject] Name=%s', [Name]);
end;

procedure TLazAccessibleObject.InsertChildAccessibleObject(
  AObject: TLazAccessibleObject);
begin
  if FChildrenSortedForDataObject = nil then Exit;
  FChildrenSortedForDataObject.Add(AObject);
end;

procedure TLazAccessibleObject.ClearChildAccessibleObjects;
var
  lXObject: TLazAccessibleObject;
  AVLNode: TAvlTreeNode;
begin
  if FChildrenSortedForDataObject = nil then Exit;
  //DebugLn(Format('[TControl.ClearChildAccessibleObjects] Name=%s Count=%d', [Name, FAccessibleChildren.Count]));
  // Free only the non-control children
  AVLNode:=FChildrenSortedForDataObject.FindLowest;
  while AVLNode<>nil do begin
    lXObject := TLazAccessibleObject(AVLNode.Data);
    if lXObject.OwnerControl = OwnerControl then
      lXObject.Free;
    AVLNode:=FChildrenSortedForDataObject.FindSuccessor(AVLNode);
  end;
  FChildrenSortedForDataObject.Clear;
end;

procedure TLazAccessibleObject.RemoveChildAccessibleObject(
  AObject: TLazAccessibleObject; AFreeObject: Boolean = True);
var
  Node: TAvlTreeNode;
begin
  if FChildrenSortedForDataObject = nil then Exit;
  Node:=FChildrenSortedForDataObject.Find(AObject);
  if Node=nil then exit;
  FChildrenSortedForDataObject.Delete(Node);
  if AFreeObject then
    AObject.Free;
end;

function TLazAccessibleObject.GetChildAccessibleObjectWithDataObject(
  ADataObject: TObject): TLazAccessibleObject;
var
  Node: TAvlTreeNode;
begin
  Result := nil;
  if FChildrenSortedForDataObject = nil then Exit;
  Node:=FChildrenSortedForDataObject.FindKey(ADataObject,@CompareDataObjectWithLazAccessibleObject);
  if Node<>nil then
    Result:=TLazAccessibleObject(Node.Data);
end;

function TLazAccessibleObject.GetChildAccessibleObjectsCount: Integer;
begin
  Result := 0;
  if FChildrenSortedForDataObject <> nil then
    Result := FChildrenSortedForDataObject.Count;
end;

function TLazAccessibleObject.GetChildAccessibleObject(AIndex: Integer): TLazAccessibleObject;
var
  lNode: TAvlTreeNode = nil;
begin
  Result := nil;
  if AIndex = 0 then lNode := FChildrenSortedForDataObject.FindLowest()
  else if AIndex = GetChildAccessibleObjectsCount()-1 then
    lNode := FChildrenSortedForDataObject.FindHighest()
  else if AIndex = FLastSearchIndex then lNode := FLastSearchNode
  else if AIndex = FLastSearchIndex+1 then
    lNode := FChildrenSortedForDataObject.FindSuccessor(FLastSearchNode)
  else if AIndex = FLastSearchIndex-1 then
    lNode := FChildrenSortedForDataObject.FindPrecessor(FLastSearchNode);

  if lNode = nil then Exit;

  Result := TLazAccessibleObject(lNode.Data);
end;

function TLazAccessibleObject.GetFirstChildAccessibleObject: TLazAccessibleObject;
begin
  Result := nil;
  FLastSearchInSubcontrols := False;
  if GetChildAccessibleObjectsCount() > 0 then
    Result := GetChildAccessibleObject(0)
  else if (OwnerControl <> nil) and (OwnerControl is TWinControl) then
  begin
    FLastSearchIndex := 1;
    FLastSearchInSubcontrols := True;
    if (TWinControl(OwnerControl).ControlCount > 0) then
      Result := TWinControl(OwnerControl).Controls[0].GetAccessibleObject();
  end;
end;

function TLazAccessibleObject.GetNextChildAccessibleObject: TLazAccessibleObject;
begin
  Result := nil;
  if not FLastSearchInSubcontrols then
  begin
    if GetChildAccessibleObjectsCount() < FLastSearchIndex then
      Result := GetChildAccessibleObject(FLastSearchIndex)
    else if (OwnerControl <> nil) and (OwnerControl is TWinControl) then
    begin
      FLastSearchIndex := 1;
      FLastSearchInSubcontrols := True;
      Result := TWinControl(OwnerControl).Controls[0].GetAccessibleObject();
    end;
  end
  else
  begin
    if TWinControl(OwnerControl).ControlCount > FLastSearchIndex then
    begin
      Result := TWinControl(OwnerControl).Controls[FLastSearchIndex].GetAccessibleObject();
      Inc(FLastSearchIndex);
    end;
  end;
end;

function TLazAccessibleObject.GetSelectedChildAccessibleObject: TLazAccessibleObject;
begin
  Result := nil;
  if OwnerControl = nil then Exit;
  Result := OwnerControl.GetSelectedChildAccessibleObject();
end;

function TLazAccessibleObject.GetChildAccessibleObjectAtPos(APos: TPoint): TLazAccessibleObject;
begin
  Result := nil;
  if OwnerControl = nil then Exit;
  Result := OwnerControl.GetChildAccessibleObjectAtPos(APos);
end;

function TLazAccessibleObject.GetEnumerator: TLazAccessibleObjectEnumerator;
begin
  Result:=TLazAccessibleObjectEnumerator.Create(FChildrenSortedForDataObject);
end;

{------------------------------------------------------------------------------
  TControl.AdjustSize

  Calls DoAutoSize smart.
  During loading and handle creation the calls are delayed.

  This method does the same as TWinControl.DoAutoSize at the beginning.
  But since DoAutoSize is commonly overriden by existing Delphi components,
  they do not all tests, which can result in too much overhead. To reduce this
  the LCL calls AdjustSize instead.
------------------------------------------------------------------------------}
procedure TControl.AdjustSize;

  procedure RaiseLoop;
  begin
    raise Exception.Create('TControl.AdjustSize loop detected '+DbgSName(Self)+' Bounds='+dbgs(BoundsRect));
  end;

begin
  {$IFDEF VerboseAdjustSize}
  if (not (cfAutoSizeNeeded in FControlFlags))
    and (Parent=nil)
    and (Self is TCustomForm)
  then begin
    DebugLn(['TControl.AdjustSize ',DbgSName(Self)]);
  end;
  {$ENDIF}
  Include(FControlFlags, cfAutoSizeNeeded);
  if IsControlVisible then
  begin
    if Parent <> nil then
      Parent.AdjustSize
    else begin
      if cfKillAdjustSize in FControlFlags then
        RaiseLoop;
      if not AutoSizeDelayed then
        DoAllAutoSize;
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.BeginDrag
  Params: Immediate: Drag behaviour
          Threshold: distance to move before dragging starts
                     -1 uses the default value of DragManager.DragThreshold
  Returns: Nothing

  Starts the dragging of a control. If the Immediate flag is set, dragging
  starts immediately. A drag-dock should not normally start immediately!
 ------------------------------------------------------------------------------}
procedure TControl.BeginDrag(Immediate: Boolean; Threshold: Integer);
begin
  DragManager.DragStart(Self, Immediate, Threshold);
end;

procedure TControl.EndDrag(Drop: Boolean);
begin
  if Dragging then
    DragManager.DragStop(Drop);
end;

{------------------------------------------------------------------------------
       TControl.BeginAutoDrag
------------------------------------------------------------------------------}
procedure TControl.BeginAutoDrag;
begin
  {$IFDEF VerboseDrag}
  debugln(['TControl.BeginAutoDrag ',DbgSName(Self)]);
  {$ENDIF}
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
  if FAutoSizingSelf then Error;
  FAutoSizingSelf := True;
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
    //DebugLn('TControl.DoDock BEFORE Adjusting ',DbgSName(Self),' ',dbgs(ARect));
    // adjust new bounds, so that they at least fit into the client area of
    // its parent
    if NewDockSite.AutoSize then begin
      case align of
        alLeft,
        alRight : ARect:=Rect(0,0,Width,NewDockSite.ClientHeight);
        alTop,
        alBottom : ARect:=Rect(0,0,NewDockSite.ClientWidth,Height);
      else
        ARect:=Rect(0,0,Width,Height);
      end;
    end else begin
      LCLProc.MoveRectToFit(ARect, NewDockSite.GetLogicalClientRect);
      // consider Align to increase chance the width/height is kept
      case Align of
        alLeft: OffsetRect(ARect,-ARect.Left,0);
        alTop: OffsetRect(ARect,0,-ARect.Top);
        alRight: OffsetRect(ARect,NewDockSite.ClientWidth-ARect.Right,0);
        alBottom: OffsetRect(ARect,0,NewDockSite.ClientHeight-ARect.Bottom);
      end;
    end;
    //DebugLn('TControl.DoDock AFTER Adjusting ',DbgSName(Self),' ',dbgs(ARect),' Align=',DbgS(Align),' NewDockSite.ClientRect=',dbgs(NewDockSite.ClientRect));
  end;
  //debugln('TControl.DoDock BEFORE MOVE ',Name,' BoundsRect=',dbgs(BoundsRect),' NewRect=',dbgs(ARect));
  if Parent<>NewDockSite then
    BoundsRectForNewParent := ARect
  else
    BoundsRect := ARect;
  //debugln('TControl.DoDock AFTER MOVE ',DbgSName(Self),' BoundsRect=',dbgs(BoundsRect),' TriedRect=',dbgs(ARect));
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
    if CurDistance<0 then
      CurDistance:=-CurDistance;
    if CurDistance>=BestDistance then exit;
    Result:=CurAlign;
    BestDistance:=CurDistance;
  end;

begin
  Result:=alNone;
  BestDistance:=High(Integer);
  FindMinDistance(alLeft,MousePos.X);
  FindMinDistance(alRight,Width-MousePos.X);
  FindMinDistance(alTop,MousePos.Y);
  FindMinDistance(alBottom,Height-MousePos.Y);
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
  WinDragTarget: TWinControl;
begin
  with DragDockObject do
  begin
    if (DragTarget is TWinControl) and TWinControl(DragTarget).UseDockManager then
    begin
      WinDragTarget := TWinControl(DragTarget);
      GetWindowRect(WinDragTarget.Handle, FDockRect);
      if (WinDragTarget.DockManager <> nil) then
        WinDragTarget.DockManager.PositionDockRect(DragDockObject);
    end else
    begin
      with FDockRect do
      begin
        Left := DragPos.X;
        Top := DragPos.Y;
        Right := Left + Control.UndockWidth;
        Bottom := Top + Control.UndockHeight;
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
  FParentFont := False;
  FDesktopFont := False;
  Invalidate;
  Perform(CM_FONTCHANGED, 0, 0);
  if AutoSize then
  begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
end;

procedure TControl.ParentFontChanged;
begin
  //kept for compatibility. The real work is done in CMParentFontChanged
end;

procedure TControl.SetAction(Value: TBasicAction);
begin
  //debugln('TControl.SetAction A ',Name,':',ClassName,' Old=',DbgS(Action),' New=',DbgS(Value));
  if Value = nil then
  begin
    ActionLink.Free;
    ActionLink := nil;
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
procedure TControl.ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
  KeepBase: boolean);
var
  SizeChanged, PosChanged : boolean;
  OldLeft, OldTop, OldWidth, OldHeight: Integer;

  function PosSizeChanged: boolean;
  begin
    SizeChanged:= (FWidth <> OldWidth) or (FHeight <> OldHeight);
    PosChanged:= (FLeft <> OldLeft) or (FTop <> OldTop);
    Result:= SizeChanged or PosChanged;
  end;

  procedure DebugInvalidPos(N: integer);
  begin
    if (FLeft < Low(Smallint)) or (FLeft > High(Smallint))
    or (FTop  < Low(Smallint)) or (FTop  > High(Smallint)) then
      DebugLn(['TControl.ChangeBounds test(',N,')',DbgSName(Self),
        ' Old=',OldLeft,',',OldTop,',',OldWidth,',',OldHeight,
        ' New=',ALeft,',',ATop,',',AWidth,',',AHeight,
        ' Real=',FLeft,',',FTop,',',FWidth,',',FHeight]);
  end;

begin
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TControl.ChangeBounds A ',DbgSName(Self),
    ' Old=',Left,',',Top,',',Width,',',Height,
    ' New=',ALeft,',',ATop,',',AWidth,',',AHeight,
    ' KeepBase=',KeepBase]);
  //if (Parent=nil) and (Left>0) and (ALeft=0) then DumpStack; // This can happen if the interface has not yet moved the window and for some reason something applies the interface coords back to the LCL
  {$ENDIF}
  if Assigned(Parent) and not KeepBase then
    Parent.UpdateAlignIndex(Self);

  // constraint the size
  DoConstrainedResize(ALeft, ATop, AWidth, AHeight);

  // check if something would change
  SizeChanged := (FWidth <> AWidth) or (FHeight <> AHeight);
  PosChanged := (FLeft <> ALeft) or (FTop <> ATop);
  if not (SizeChanged or PosChanged) then Exit;

  // check for loop.
  if (not KeepBase) and (cfKillChangeBounds in GetTopParent.FControlFlags) then
    raise Exception.Create('TControl.ChangeBounds loop detected '+DbgSName(Self)
      +' Left='+dbgs(Left)+',Top='+dbgs(Top)+',Width='+dbgs(Width)+',Height='+dbgs(Height)
      +' NewLeft='+dbgs(aLeft)+',NewTop='+dbgs(aTop)+',NewWidth='+dbgs(aWidth)+',NewHeight='+dbgs(aHeight)
      );
  OldLeft := FLeft;
  OldTop := FTop;
  OldWidth := FWidth;
  OldHeight := FHeight;

  //DebugLn('TControl.ChangeBounds A ',DbgSName(Self),' Old=',dbgs(BoundsRect),' New=',dbgs(Bounds(ALeft,ATop,AWidth,AHeight)));
  if not ((csLoading in ComponentState) or (Self is TWinControl)) then
    InvalidateControl(IsControlVisible, False, true);
  //DebugLn('TControl.ChangeBounds B ',Name,':',ClassName);
  DoSetBounds(ALeft, ATop, AWidth, AHeight);
  DebugInvalidPos(1);

  // change base bounds
  // (base bounds are the base for the automatic resizing)
  if not KeepBase then
    UpdateAnchorRules;
  DebugInvalidPos(2);

  // lock size messages
  inc(FSizeLock);
  try
    // notify before autosizing
    BoundsChanged;
    if not PosSizeChanged then exit;
    if (Parent<>nil) or SizeChanged then
      AdjustSize;
  finally
    dec(FSizeLock);
  end;
  if not PosSizeChanged then exit;
  DebugInvalidPos(3);

  // send messages, if this is the top level call
  if FSizeLock > 0 then exit;

  // invalidate
  if (csDesigning in ComponentState) and (Parent <> nil) then
    Parent.Invalidate
  else
  if (not (csLoading in ComponentState)) and (not (Self is TWinControl)) then
    Invalidate;
  DebugInvalidPos(4);
  // notify user about resize
  if (not (csLoading in ComponentState)) then
  begin
    Resize;
    DebugInvalidPos(5);
    CheckOnChangeBounds;
    DebugInvalidPos(6);
    // for delphi compatibility send size/move messages
    if PosSizeChanged then
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
    DebugLn(['TControl.DoSetBounds ',DbgSName(Self),
      ' Old=',Left,',',Top,',',Width,'x',Height,
      ' New=',aLeft,',',aTop,',',aWidth,'x',aHeight]);
  {$ENDIF}
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
  if Parent <> nil then Parent.InvalidatePreferredSize;
end;

procedure TControl.ScaleConstraints(Multiplier, Divider: Integer);
begin
  with Constraints do
  begin
    if MinWidth > 0 then
      MinWidth := MulDiv(MinWidth, Multiplier, Divider);
    if MaxWidth > 0 then
      MaxWidth := MulDiv(MaxWidth, Multiplier, Divider);
    if MinHeight > 0 then
      MinHeight := MulDiv(MinHeight, Multiplier, Divider);
    if MaxHeight > 0 then
      MaxHeight := MulDiv(MaxHeight, Multiplier, Divider);
  end;
end;

function TControl.ScaleDesignToForm(const ASize: Integer): Integer;
var
  ParentForm: TCustomDesignControl;
begin
  ParentForm := NeedParentDesignControl(Self);
  Result := MulDiv(ASize, ParentForm.PixelsPerInch, ParentForm.DesignTimePPI);
end;

function TControl.Scale96ToForm(const ASize: Integer): Integer;
var
  ParentForm: TCustomDesignControl;
begin
  ParentForm := NeedParentDesignControl(Self);
  Result := MulDiv(ASize, ParentForm.PixelsPerInch, 96);
end;

function TControl.Scale96ToScreen(const ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, Screen.PixelsPerInch, 96);
end;

function TControl.ScaleFormTo96(const ASize: Integer): Integer;
var
  ParentForm: TCustomDesignControl;
begin
  ParentForm := NeedParentDesignControl(Self);
  Result := MulDiv(ASize, 96, ParentForm.PixelsPerInch);
end;

function TControl.ScaleFormToDesign(const ASize: Integer): Integer;
var
  ParentForm: TCustomDesignControl;
begin
  ParentForm := NeedParentDesignControl(Self);
  Result := MulDiv(ASize, ParentForm.DesignTimePPI, ParentForm.PixelsPerInch);
end;

function TControl.ScaleScreenTo96(const ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, 96, Screen.PixelsPerInch);
end;

function TControl.Scale96ToFont(const ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, Font.PixelsPerInch, 96);
end;

function TControl.ScaleFontTo96(const ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, 96, Font.PixelsPerInch);
end;

function TControl.ScaleScreenToFont(const ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, Font.PixelsPerInch, Screen.PixelsPerInch);
end;

function TControl.ScaleFontToScreen(const ASize: Integer): Integer;
begin
  Result := MulDiv(ASize, Screen.PixelsPerInch, Font.PixelsPerInch);
end;

procedure TControl.ScaleFontsPPI(const AToPPI: Integer;
  const AProportion: Double);
begin
  // Problem: all fonts have to be scaled.
  // Override this function - list all custom fonts in the overriden procedure

  DoScaleFontPPI(Font, AToPPI, AProportion);
end;

{------------------------------------------------------------------------------
       TControl.ChangeScale

  Scale contorl by factor Multiplier/Divider
------------------------------------------------------------------------------}
procedure TControl.ChangeScale(Multiplier, Divider: Integer);
var
  R: TRect;
begin
  if Multiplier <> Divider then
  begin
    ScaleConstraints(Multiplier, Divider);
    if not ParentFont then
      Font.Height := MulDiv(GetFontData(Font.Reference.Handle).Height, Multiplier, Divider);
    R := BaseBounds;
    if (Self is TCustomForm) and (GetParentForm(Self, True) = Self) then
    begin
      //Dont change Left,Top if this is the topmost form
      R.Right := R.Left + MulDiv(R.Right-R.Left, Multiplier, Divider);
      R.Bottom := R.Top + MulDiv(R.Bottom-R.Top, Multiplier, Divider);
    end
    else
    begin
      R.Left := MulDiv(R.Left, Multiplier, Divider);
      R.Top := MulDiv(R.Top, Multiplier, Divider);
      R.Right := MulDiv(R.Right, Multiplier, Divider);
      R.Bottom := MulDiv(R.Bottom, Multiplier, Divider);
    end;
    BoundsRect := R;
  end;
end;

{------------------------------------------------------------------------------
  procedure TControl.CalculateDockSizes;

  Compute docking width, height based on docking properties.
------------------------------------------------------------------------------}
procedure TControl.CalculateDockSizes;
begin
  if Floating then
  begin
    // if control is floating then save it size for further undocking
    UndockHeight := Height;
    UndockWidth := Width;
  end
  else
  if HostDockSite <> nil then
  begin
    // the control is docked into a HostSite. That means some of it bounds
    // were maximized to fit into the HostSite.
    if (DockOrientation = doHorizontal) or
       (HostDockSite.Align in [alLeft,alRight]) then
      // the control is aligned left/right, that means its width is not
      // maximized. Save Width for docking.
      LRDockWidth := Width
    else
    if (DockOrientation = doVertical) or
       (HostDockSite.Align in [alTop,alBottom]) then
      // the control is aligned top/bottom, that means its height is not
      // maximized. Save Height for docking.
      TBDockHeight := Height;
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
    Result := TWinControl(FloatingClass.NewInstance);
    Result.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.CreateFloatingDockSite'){$ENDIF};
    Result.Create(Self);
    // resize with minimal resizes
    NewClientWidth:=Bounds.Right-Bounds.Left;
    NewClientHeight:=Bounds.Bottom-Bounds.Top;
    NewWidth:=Result.Width-Result.ClientWidth+NewClientWidth;
    NewHeight:=Result.Height-Result.ClientHeight+NewClientHeight;
    Result.SetBounds(Bounds.Left,Bounds.Top,NewWidth,NewHeight);
    Result.SetClientSize(Point(NewClientWidth,NewClientHeight));
    {$IFDEF DebugDisableAutoSizing}
    debugln('TControl.CreateFloatingDockSite A ',DbgSName(Self),' ',DbgSName(Result),' ',dbgs(Result.BoundsRect));
    {$ENDIF}
    Result.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.CreateFloatingDockSite'){$ENDIF};
  end;
end;

procedure TControl.ExecuteDefaultAction;
begin
end;

procedure TControl.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
  // Problem: Font.PixelsPerInch isn't saved in the LFM, therefore the
  // design-time font PPI is different from the one that is loaded on target
  // machine, which results in different font scaling.
  // DoFixDesignFont restores the corrent design-time font PPI so that it can
  // be used for LCL HighDPI scaling.
  // Override this function - list all custom fonts in the overriden procedure
  // To-Do: maybe save Font.PixelsPerInch in the LFM and remove this?

  DoFixDesignFontPPI(Font, ADesignTimePPI);
end;

procedure TControl.ExecuteCancelAction;
begin
end;

{------------------------------------------------------------------------------
  function TControl.GetFloating: Boolean;
------------------------------------------------------------------------------}
function TControl.GetFloating: Boolean;
begin
  // a non-windowed control can never float for itself
  Result := (HostDockSite <> nil) and (HostDockSite is FloatingDockSiteClass)
      and (HostDockSite.DockClientCount<=1);
end;

{------------------------------------------------------------------------------
  function TControl.GetFloatingDockSiteClass: TWinControlClass;
------------------------------------------------------------------------------}
function TControl.GetFloatingDockSiteClass: TWinControlClass;
begin
  Result := FFloatingDockSiteClass;
end;

procedure TControl.BeforeDragStart;
begin
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
function TControl.IsHelpContextStored: Boolean;
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
  if FUndockHeight > 0 then
    Result := FUndockHeight
  else
    Result := Height;
end;

function TControl.GetUndockWidth: Integer;
begin
  if FUndockWidth > 0 then
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

{------------------------------------------------------------------------------
  Method: TControl.IsEnabled
  Params:   none
  Returns:  Boolean

  Returns True only if both TControl and it's parent hierarchy are enabled.
  Used internally by TGraphicControls for painting and various states during
  runtime.
 ------------------------------------------------------------------------------}
function TControl.IsEnabled: Boolean;
var
  TheControl: TControl;
begin
  TheControl := Self;
  repeat
    Result := TheControl.Enabled;
    TheControl := TheControl.Parent;
  until (TheControl = nil) or (not Result);
end;

{------------------------------------------------------------------------------
  Method: TControl.IsParentColor
  Params:   none
  Returns:  Boolean

  Used at places where we need to check ParentColor property from TControl.
  Property is protected, so this function avoids hacking to get
  protected property value.
 ------------------------------------------------------------------------------}
function TControl.IsParentColor: Boolean;
begin
  Result := FParentColor;
end;

{------------------------------------------------------------------------------
  Method: TControl.IsParentFont
  Params:   none
  Returns:  Boolean

  Used at places where we need to check ParentFont property from TControl.
  Property is protected, so this function avoids hacking to get
  protected property value.
 ------------------------------------------------------------------------------}
function TControl.IsParentFont: Boolean;
begin
  Result := FParentFont;
end;

function TControl.FormIsUpdating: boolean;
begin
  Result := Assigned(Parent) and Parent.FormIsUpdating;
end;

function TControl.IsProcessingPaintMsg: boolean;
begin
  Result:=cfProcessingWMPaint in FControlFlags;
end;

{------------------------------------------------------------------------------
       TControl.LMCaptureChanged
------------------------------------------------------------------------------}
procedure TControl.LMCaptureChanged(var Message: TLMessage);
begin
  //DebugLn('[LMCaptureChanged for '+Name+':'+Classname+']');
  CaptureChanged;
end;

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
procedure TControl.CMHitTest(var Message: TCMHittest);
begin
  Message.Result := 1;
end;

{------------------------------------------------------------------------------
       TControl.CMMouseEnter
------------------------------------------------------------------------------}
procedure TControl.CMMouseEnter(var Message: TLMessage);
begin
  if FMouseInClient then
    Exit;

  FMouseInClient := True;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSEENTER, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseEnter;
end;

{------------------------------------------------------------------------------
       TControl.CMMouseLeave
------------------------------------------------------------------------------}
procedure TControl.CMMouseLeave(var Message: TLMessage);
begin
  if not FMouseInClient then
    Exit;

  FMouseInClient := False;

  // broadcast to parents first
  if Assigned(Parent) then
    Parent.Perform(CM_MOUSELEAVE, 0, LParam(Self));

  // if it is not a child message then perform an event
  if (Message.LParam = 0) then
    MouseLeave;
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
  if (not (csDesigning in ComponentState) or (csNoDesignVisible in ControlStyle)) and
     (not (csLoading in ComponentState)) then
    InvalidateControl(True, FVisible and (csOpaque in ControlStyle), True);
end;

procedure TControl.CMTextChanged(var Message: TLMessage);
begin
  TextChanged;
end;

procedure TControl.CMCursorChanged(var Message: TLMessage);
begin
  if not (csDesigning in ComponentState) then
    SetTempCursor(Cursor);
end;

{------------------------------------------------------------------------------
       TControl.CMParentColorChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}
procedure TControl.CMParentColorChanged(var Message: TLMessage);
begin
  if csLoading in ComponentState then Exit;
  
  if FParentColor then
  begin
    Color := FParent.Color;
    FParentColor := True;
  end;
end;

{------------------------------------------------------------------------------
       TControl.CMParentFontChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}
procedure TControl.CMParentFontChanged(var Message: TLMessage);
begin
  if csLoading in ComponentState then exit;

  if FParentFont then
  begin
    if Assigned(FParent) then
    begin
      Font.BeginUpdate;
      try
        Font.PixelsPerInch := FParent.Font.PixelsPerInch; // PixelsPerInch isn't assigned
        Font := FParent.Font;
      finally
        Font.EndUpdate;
      end;
    end;
    FParentFont := True;
  end;
  //call here for compatibility with older LCL code
  ParentFontChanged;
end;

{------------------------------------------------------------------------------
       TControl.CMParentShowHintChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}
procedure TControl.CMParentShowHintChanged(var Message: TLMessage);
begin
  if csLoading in ComponentState then Exit;

  if FParentShowHint then
  begin
    ShowHint := FParent.ShowHint;
    FParentShowHint := True;
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
var
  NewMouseControl: TControl;
begin
  if Assigned(Application) then
  begin
    NewMouseControl := GetCaptureControl;
    if NewMouseControl = nil then
      NewMouseControl := Application.GetControlAtMouse;
    Application.DoBeforeMouseMessage(NewMouseControl);
  end;
end;

{------------------------------------------------------------------------------
  function TControl.ColorIsStored: boolean;
------------------------------------------------------------------------------}
function TControl.ColorIsStored: boolean;
begin
  Result := not ParentColor;
end;

function TControl.GetDefaultColor(const DefaultColorType: TDefaultColorType): TColor;
const
  DefColors: array[TDefaultColorType] of TColor = (
  { dctBrush } clWindow,
  { dctFont  } clWindowText
  );
begin
  Result := TWSControlClass(WidgetSetClass).GetDefaultColor(Self, DefaultColorType);
  if (Result = clDefault) then
    if ParentColor and Assigned(Parent) then
      Result := Parent.GetDefaultColor(DefaultColorType)
    else
      Result := DefColors[DefaultColorType];
end;

function TControl.GetColorResolvingParent: TColor;
begin
  if Color = clDefault then
    Result := GetDefaultColor(dctBrush) // GetDefaultColor resolves the parent
  else
    Result := Color;
end;

function TControl.GetRGBColorResolvingParent: TColor;
begin
  Result := ColorToRGB(GetColorResolvingParent());
end;

{------------------------------------------------------------------------------
       TControl.DoConstrainedResize
------------------------------------------------------------------------------}
procedure TControl.DoConstrainedResize(var NewLeft, NewTop,
  NewWidth, NewHeight: integer);
var
  MinWidth, MinHeight, MaxWidth, MaxHeight : TConstraintSize;
begin
  if NewWidth<0 then NewWidth:=0;
  if NewHeight<0 then NewHeight:=0;
  MinWidth := Constraints.EffectiveMinWidth;
  MinHeight := Constraints.EffectiveMinHeight;
  MaxWidth := Constraints.EffectiveMaxWidth;
  MaxHeight := Constraints.EffectiveMaxHeight;

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
    NewWidth := MinWidth
  end else if (MaxWidth > 0) and (NewWidth > MaxWidth) then
  begin
    if (NewLeft+NewWidth) = (Left+Width) then
    begin
      Inc(NewLeft, NewWidth - MaxWidth);
      if NewLeft > Left then
        NewLeft := Left;
    end;
    NewWidth := MaxWidth;
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
    NewHeight := MinHeight
  end else if (MaxHeight > 0) and (NewHeight > MaxHeight) then
  begin
    if (NewTop+NewHeight) = (Top+Height) then
    begin
      Inc(NewTop, NewHeight - MaxHeight);
      if NewTop > Top then
        NewTop := Top;
    end;
    NewHeight := MaxHeight;
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
  if Parent <> nil then Parent.InvalidatePreferredSize;
  AdjustSize;
end;

function TControl.IsBorderSpacingInnerBorderStored: Boolean;
begin
  Result:=BorderSpacing.InnerBorder<>0;
end;

{------------------------------------------------------------------------------
  TControl IsCaptionStored
------------------------------------------------------------------------------}
function TControl.IsCaptionStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsCaptionLinked;
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
procedure TControl.DoEndDrag(Target: TObject; X,Y: Integer);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DoEndDrag ',Name,':',ClassName,' XY=',IntToStr(X),',',IntToStr(Y));
  {$ENDIF}
  if Assigned(FOnEndDrag) then FOnEndDrag(Self,Target,X,Y);
end;

{------------------------------------------------------------------------------
       TControl.DoFixDesignFontPPI
------------------------------------------------------------------------------}
procedure TControl.DoFixDesignFontPPI(const AFont: TFont;
  const ADesignTimePPI: Integer);
var
  H: Integer;
  OldParentFont: Boolean;
begin
  if AFont.PixelsPerInch <> ADesignTimePPI then
  begin
    OldParentFont := ParentFont;
    try
      H := AFont.Height;
      AFont.BeginUpdate;
      try
        AFont.Height := MulDiv(H, AFont.PixelsPerInch, ADesignTimePPI);
        AFont.PixelsPerInch := ADesignTimePPI;
      finally
        AFont.EndUpdate;
      end;
    finally
      FParentFont := OldParentFont; // change ParentFont without triggering CM_PARENTFONTCHANGED
    end;
  end;
end;

{------------------------------------------------------------------------------
       TControl.Perform

------------------------------------------------------------------------------}
function TControl.Perform(Msg: Cardinal; WParam: WParam; LParam: LParam): LRESULT;
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
begin
  if Parent = nil then
    raise EInvalidOperation.CreateFmt(rsControlHasNoParentWindow, [Name]);
  Result := Parent.ClientOrigin;
  Inc(Result.X, FLeft);
  Inc(Result.Y, FTop);
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

function TControl.ClientToParent(const Point: TPoint; AParent: TWinControl): TPoint;
begin
  if not Assigned(AParent) then
    AParent := Parent;
  if not AParent.IsParentOf(Self) then
    raise EInvalidOperation.CreateFmt(rsControlIsNotAParent, [AParent.Name, Name]);
  Result := AParent.ScreenToClient(ClientToScreen(Point));
end;

function TControl.ParentToClient(const Point: TPoint; AParent: TWinControl): TPoint;
begin
  if not Assigned(AParent) then
    AParent := Parent;
  if not AParent.IsParentOf(Self) then
    raise EInvalidOperation.CreateFmt(rsControlIsNotAParent, [AParent.Name, Name]);
  Result := ScreenToClient(AParent.ClientToScreen(Point));
end;

{------------------------------------------------------------------------------
       TControl.DblClick
------------------------------------------------------------------------------}
procedure TControl.DblClick;
begin
  if Assigned(FOnDblClick) then FOnDblClick(Self);
end;

{------------------------------------------------------------------------------
       TControl.TripleClick
------------------------------------------------------------------------------}
procedure TControl.TripleClick;
begin
  if Assigned(FOnTripleClick) then FOnTripleClick(Self);
end;

{------------------------------------------------------------------------------
       TControl.QuadClick
------------------------------------------------------------------------------}
procedure TControl.QuadClick;
begin
  if Assigned(FOnQuadClick) then FOnQuadClick(Self);
end;

{------------------------------------------------------------------------------
       TControl.DoDragMsg
------------------------------------------------------------------------------}
function TControl.DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint;
  ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean): LRESULT;

  function GetDragObject: TObject; inline;
  begin
    if ADragObject.AutoCreated then
      Result := ADragObject.Control
    else
      Result := ADragObject;
  end;

var
  AWinTarget: TWinControl;
  Accepts: Boolean;
  P: TPoint;
begin
  Result := 0;
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DoDragMsg ',Name,':',ClassName,' DragMsg.DragMessage=', GetEnumName(TypeInfo(TDragMessage), Ord(ADragMessage)));
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
          DragOver(GetDragObject, P.X, P.Y, TDragState(ADragMessage), Accepts);
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
          DragDrop(GetDragObject, P.X, P.Y);
      end;
  end;
end;

{------------------------------------------------------------------------------
  TControl.DragOver
------------------------------------------------------------------------------}
procedure TControl.DragOver(Source: TObject; X,Y : Integer; State: TDragState;
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
procedure TControl.DragDrop(Source: TObject; X,Y : Integer);
begin
  {$IFDEF VerboseDrag}
  DebugLn('TControl.DragDrop ',Name,':',ClassName,' XY=',IntToStr(X),',',IntToStr(Y));
  {$ENDIF}
  if Assigned(FOnDragDrop) then FOnDragDrop(Self, Source,X,Y);
end;

procedure TControl.SetAccessibleDescription(AValue: TCaption);
begin
  FAccessibleObject.AccessibleDescription := AValue;
end;

procedure TControl.SetAccessibleValue(AValue: TCaption);
begin
  FAccessibleObject.AccessibleValue := AValue;
end;

procedure TControl.SetAccessibleRole(AValue: TLazAccessibilityRole);
begin
  FAccessibleObject.AccessibleRole := AValue;
end;

{------------------------------------------------------------------------------
  TControl Method SetColor  "Sets the default color and tells the widget set"
------------------------------------------------------------------------------}
procedure TControl.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ParentColor := False;
    Perform(CM_COLORCHANGED, 0, 0);
    Invalidate;
  end;
end;

{------------------------------------------------------------------------------
       TControl CanAutoSize
------------------------------------------------------------------------------}
function TControl.CanAutoSize(var NewWidth, NewHeight : Integer): Boolean;
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

// accessibility
function TControl.GetAccessibleObject: TLazAccessibleObject;
begin
  Result := FAccessibleObject;
end;

function TControl.CreateAccessibleObject: TLazAccessibleObject;
begin
  Result := TLazAccessibleObject.Create(Self);
end;

function TControl.GetSelectedChildAccessibleObject: TLazAccessibleObject;
begin
  Result := nil;
end;

function TControl.GetChildAccessibleObjectAtPos(APos: TPoint): TLazAccessibleObject;
begin
  Result := nil;
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
  Result := (Parent<>nil) and Parent.HandleAllocated and (GetCaptureControl = Self);
end;

function TControl.GetMousePosFromMessage(const MessageMousePos: TSmallPoint
  ): TPoint;
begin
  if (Width>32767) or (Height>32767) then
  begin
    GetCursorPos(Result);
    Result := ScreenToClient(Result);
  end else
    Result := SmallPointToPoint(MessageMousePos);
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
procedure TControl.DoOnShowHint(HintInfo: PHintInfo);
begin
  if Assigned(OnShowHint) then
    OnShowHint(Self,HintInfo);
end;

procedure TControl.DoScaleFontPPI(const AFont: TFont; const AToPPI: Integer;
  const AProportion: Double);
begin
  // If AFont.PixelsPerInch is different from "Screen.PixelsPerInch" (=GetDeviceCaps(DC, LOGPIXELSX))
  // then the font doesn't scale -> we have to assign a nonzero height value.
  if (AFont.Height=0) and not (csDesigning in ComponentState) then
    AFont.Height := MulDiv(GetFontData(AFont.Reference.Handle).Height, AFont.PixelsPerInch, Screen.PixelsPerInch);
  if AToPPI>0 then
    AFont.PixelsPerInch := AToPPI
  else
    AFont.PixelsPerInch := Round(AFont.PixelsPerInch*AProportion);
end;

function TControl.IsAParentAligning: boolean;
var
  p: TWinControl;
begin
  p:=Parent;
  while (p<>nil) do begin
    if (wcfAligningControls in p.FWinControlFlags) then
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

{------------------------------------------------------------------------------
  procedure TControl.EnabledChanging;
------------------------------------------------------------------------------}
procedure TControl.EnabledChanging;
begin
  DoCallNotifyHandler(chtOnEnabledChanging);
end;

procedure TControl.EnabledChanged;
begin
  DoCallNotifyHandler(chtOnEnabledChanged);
end;

procedure TControl.AddHandler(HandlerType: TControlHandlerType;
  const AMethod: TMethod; AsFirst: boolean);
begin
  if FControlHandlers[HandlerType]=nil then
    FControlHandlers[HandlerType]:=TMethodList.Create;
  FControlHandlers[HandlerType].Add(AMethod,not AsFirst);
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

procedure TControl.DoCallKeyEventHandler(HandlerType: TControlHandlerType;
  var Key: Word; Shift: TShiftState);
var
  i: Integer;
begin
  i := FControlHandlers[HandlerType].Count;
  while FControlHandlers[HandlerType].NextDownIndex(i) do
    TKeyEvent(FControlHandlers[HandlerType][i])(Self, Key, Shift);
end;

procedure TControl.DoCallMouseWheelEventHandler(HandlerType: TControlHandlerType;
  Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
begin
  i := FControlHandlers[HandlerType].Count;
  //debugln('TControl.DoCallMouseWheelEventHandler A: Handled = ',DbgS(Handled),', Count = ',DbgS(i));
  while (not Handled) and FControlHandlers[HandlerType].NextDownIndex(i) do
  begin
    TMouseWheelEvent(FControlHandlers[HandlerType][i])(Self, Shift, WheelDelta, MousePos, Handled);
    //debugln('TControl.DoCallMouseWheelEventHandler B: i = ',Dbgs(i),', Handled = ',DbgS(Handled));
  end;
  //debugln('TControl.DoCallMouseWheelEventHandler End: Handled = ',DbgS(Handled));
end;

{------------------------------------------------------------------------------
  procedure TControl.DoContextPopup(const MousePos: TPoint;
    var Handled: Boolean);
------------------------------------------------------------------------------}
procedure TControl.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
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
    if (not CheckDefaults) or (Caption = '') or (Caption = Name) then
      Caption := NewAction.Caption;
    if not CheckDefaults or Enabled then
      Enabled := NewAction.Enabled;
    if not CheckDefaults or (Hint = '') then
      Hint := NewAction.Hint;
    if not CheckDefaults or Visible then
      Visible := NewAction.Visible;
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

function TControl.GetAccessibleDescription: TCaption;
begin
  Result := FAccessibleObject.AccessibleDescription;
end;

function TControl.GetAccessibleValue: TCaption;
begin
  Result := FAccessibleObject.AccessibleValue;
end;

function TControl.GetAccessibleRole: TLazAccessibilityRole;
begin
  Result := FAccessibleObject.AccessibleRole;
end;

function TControl.CaptureMouseButtonsIsStored: boolean;
begin
  Result := FCaptureMouseButtons <> [mbLeft];
end;

function TControl.GetAnchorSide(Kind: TAnchorKind): TAnchorSide;
begin
  Result:=FAnchorSides[Kind];
end;

function TControl.GetAnchoredControls(Index: integer): TControl;
begin
  Result := TControl(FAnchoredControls[Index]);
end;

function TControl.GetAutoSizingAll: Boolean;
begin
  if Parent <> nil then
    Result := Parent.AutoSizingAll
  else
    Result := FAutoSizingAll;
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
  function TControl.GetChildrenRect(Scrolled: boolean): TRect;

  Returns the Client rectangle relative to the controls left, top.
  If Scrolled is true, the rectangle is moved by the current scrolling values
  (for an example see TScrollingWincontrol).
------------------------------------------------------------------------------}
function TControl.GetChildrenRect(Scrolled: boolean): TRect;
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
var
  Form : TCustomForm;
begin
  //DebugLn('CCC TControl.WndPRoc ',Name,':',ClassName);
  if (csDesigning in ComponentState) then
  begin
    // redirect messages to designer
    Form := GetDesignerForm(Self);
    //debugln(['TControl.WndProc ',dbgsname(Self)]);
    if Assigned(Form) and Assigned(Form.Designer) and Form.Designer.IsDesignMsg(Self, TheMessage) then
      Exit;
  end
  else if (TheMessage.Msg >= LM_KEYFIRST) and (TheMessage.Msg <= LM_KEYLAST)
  then begin
    // keyboard messages
    Form := GetParentForm(Self);
    if (Form <> nil) and (Form.WantChildKey(Self,TheMessage)) then exit;
  end
  else if ((TheMessage.Msg>=LM_MOUSEFIRST) and (TheMessage.Msg<=LM_MOUSELAST))
  or ((TheMessage.Msg>=LM_MOUSEFIRST2) and (TheMessage.Msg<=LM_MOUSELAST2))
  then begin
    // mouse messages
    case TheMessage.Msg of

      LM_MOUSEMOVE:
        begin
          Application.HintMouseMessage(Self, TheMessage);
        end;

      LM_LBUTTONDOWN,
      LM_LBUTTONDBLCLK:
        begin
          Include(FControlState, csLButtonDown);
          { The VCL holds up the mouse down for dmAutomatic
            and sends it, when it decides, if it is a drag operation or
            not.
            This decision requires full control of focus and mouse, which
            do not all LCL interfaces provide. Therefore the mouse down event
            is sent immediately.

            Further Note:
              Under winapi a LM_LBUTTONDOWN ends the drag immediate.
            For example: If we exit here, then mouse down on TTreeView does
              not work any longer under gtk.
          }
          if FDragMode = dmAutomatic then
            BeginAutoDrag;
        end;

      LM_LBUTTONUP:
        begin
          Exclude(FControlState, csLButtonDown);
        end;
    end;
  end;

  //debugln(['TControl.WndProc ',DbgSName(Self),' ',TheMessage.Msg]);
  if TheMessage.Msg=LM_PAINT then begin
    Include(FControlFlags,cfProcessingWMPaint);
    try
      Dispatch(TheMessage);
    finally
      Exclude(FControlFlags,cfProcessingWMPaint);
    end;
  end else
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
var
  MP: TPoint;
begin
  //DebugLn('TControl.DoMouseDown ',DbgSName(Self),' ');
  if not (csNoStdEvents in ControlStyle) then
  begin
    MP := GetMousePosFromMessage(Message.Pos);
    MouseDown(Button, KeysToShiftState(Message.Keys) + Shift, MP.X, MP.Y);
  end;
end;

{------------------------------------------------------------------------------
       TControl DoMouseUp  "Event Handler"
------------------------------------------------------------------------------}
procedure TControl.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
var
  P, MP: TPoint;
begin
  if not (csNoStdEvents in ControlStyle) then
  begin
    MP := GetMousePosFromMessage(Message.Pos);
    if (Button in [mbLeft, mbRight]) and DragManager.IsDragging then
    begin
      P := ClientToScreen(MP);
      DragManager.MouseUp(Button, KeysToShiftState(Message.Keys), P.X, P.Y);
      Message.Result := 1;
    end;
    MouseUp(Button, KeysToShiftState(Message.Keys), MP.X, MP.Y);
  end;
end;

{------------------------------------------------------------------------------
       TControl DoMouseWheel  "Event Handler"
 ------------------------------------------------------------------------------}
function TControl.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := False;

  if Assigned(FOnMouseWheel)
    then FOnMouseWheel(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    //debugln('TControl.DoMouseWheel calling DoCallMouseWheelEventHandler');
    DoCallMouseWheelEventHandler(chtOnMouseWheel, Shift, WheelDelta, MousePos, Result);
  end;

  if not Result
  then begin
    if WheelDelta < 0
    then Result := DoMouseWheelDown(Shift, MousePos)
    else Result := DoMouseWheelUp(Shift, MousePos);
  end;
end;

function TControl.DoMouseWheelHorz(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := False;

  if Assigned(FOnMouseWheelHorz)
    then FOnMouseWheelHorz(Self, Shift, WheelDelta, MousePos, Result);
  if not Result then
  begin
    //debugln('TControl.DoMouseWheelHorz calling DoCallMouseWheelEventHandler');
    DoCallMouseWheelEventHandler(chtOnMouseWheelHorz, Shift, WheelDelta, MousePos, Result);
  end;

  if not Result
  then begin
    if WheelDelta < 0
    then Result := DoMouseWheelLeft(Shift, MousePos)
    else Result := DoMouseWheelRight(Shift, MousePos);
  end;
end;

{------------------------------------------------------------------------------
       TControl DoMouseWheelDown  "Event Handler"
------------------------------------------------------------------------------}
function TControl.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelDown) then
    FOnMouseWheelDown(Self, Shift, MousePos, Result);
end;

{------------------------------------------------------------------------------
       TControl DoMouseWheelUp  "Event Handler"
------------------------------------------------------------------------------}
function TControl.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelUp) then
    FOnMouseWheelUp(Self, Shift, MousePos, Result);
end;

{------------------------------------------------------------------------------
       TControl DoMouseWheelLeft  "Event Handler"
------------------------------------------------------------------------------}
function TControl.DoMouseWheelLeft(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelLeft) then
    FOnMouseWheelLeft(Self, Shift, MousePos, Result);
end;

{------------------------------------------------------------------------------
       TControl DoMouseWheelRight  "Event Handler"
------------------------------------------------------------------------------}
function TControl.DoMouseWheelRight(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := False;
  if Assigned(FOnMouseWheelRight) then
    FOnMouseWheelRight(Self, Shift, MousePos, Result);
end;

procedure TControl.SetAnchorSide(Kind: TAnchorKind; AValue: TAnchorSide);
begin
  GetAnchorSide(Kind).Assign(AValue);
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

procedure TControl.WMContextMenu(var Message: TLMContextMenu);
var
  TempPopupMenu: TPopupMenu;
  P: TPoint;
  Handled: Boolean;
begin
  if (csDesigning in ComponentState) or (Message.Result <> 0) then Exit;
  P := GetMousePosFromMessage(Message.Pos);
  // X and Y = -1 when user clicks on keyboard menu button
  if P.X <> -1 then
    P := ScreenToClient(P);

  Handled := False;
  DoContextPopup(P, Handled);
  if Handled then
  begin
    Message.Result := 1;
    Exit;
  end;

  TempPopupMenu := GetPopupMenu;
  if (TempPopupMenu <> nil) then
  begin
    if not TempPopupMenu.AutoPopup then Exit;
    TempPopupMenu.PopupComponent := Self;
    if P.X = -1 then
      P := Point(0, 0);
    P := ClientToScreen(P);
    TempPopupMenu.Popup(P.X, P.Y);
    Message.Result := 1;
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
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonDown ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  DoMouseDown(Message, mbMiddle, []);
end;

procedure TControl.WMXButtonDown(var Message: TLMXButtonDown);
var
  Btn: TMouseButton;
begin
  if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
  else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
  else Exit;

  if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMXButtonDown ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;

  DoMouseDown(Message, Btn, []);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonDblClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonDBLCLK(var Message: TLMLButtonDblClk);
begin
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
procedure TControl.WMRButtonDBLCLK(var Message: TLMRButtonDblClk);
begin
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
procedure TControl.WMMButtonDBLCLK(var Message: TLMMButtonDblClk);
begin
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonDblClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  DoMouseDown(Message, mbMiddle ,[ssDouble]);
end;

procedure TControl.WMXButtonDBLCLK(var Message: TLMXButtonDblClk);
var
  Btn: TMouseButton;
begin
  if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
  else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
  else Exit;

  if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMXButtonDblClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  DoMouseDown(Message, Btn, [ssDouble]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonTripleClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonTripleCLK(var Message: TLMLButtonTripleClk);
begin
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
procedure TControl.WMRButtonTripleCLK(var Message: TLMRButtonTripleClk);
begin
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
procedure TControl.WMMButtonTripleCLK(var Message: TLMMButtonTripleClk);
begin
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonTripleClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  DoMouseDown(Message, mbMiddle ,[ssTriple]);
end;

procedure TControl.WMXButtonTripleCLK(var Message: TLMXButtonTripleClk);
var
  Btn: TMouseButton;
begin
  if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
  else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
  else Exit;

  if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMXButtonTripleClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  DoMouseDown(Message, Btn, [ssTriple]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonQuadClk
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonQuadCLK(var Message: TLMLButtonQuadClk);
begin
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
procedure TControl.WMRButtonQuadCLK(var Message: TLMRButtonQuadClk);
begin
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
procedure TControl.WMMButtonQuadCLK(var Message: TLMMButtonQuadClk);
begin
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonQuadClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  DoMouseDown(Message, mbMiddle ,[ssQuad]);
end;

procedure TControl.WMXButtonQuadCLK(var Message: TLMXButtonQuadClk);
var
  Btn: TMouseButton;
begin
  if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
  else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
  else Exit;

  if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonQuadClk ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := True;
  end;
  DoMouseDown(Message, Btn, [ssQuad]);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMLButtonUp
  Params: Message
  Returns: Nothing

  Mouse event handler
 ------------------------------------------------------------------------------}
procedure TControl.WMLButtonUp(var Message: TLMLButtonUp);
begin
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
    if PtInRect(ClientRect, GetMousePosFromMessage(Message.Pos))
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
  if (csCaptureMouse in ControlStyle) and (mbRight in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMRButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;
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
  if (csCaptureMouse in ControlStyle) and (mbMiddle in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;
  
  DoMouseUp(Message, mbMiddle);
end;

procedure TControl.WMXButtonUp(var Message: TLMXButtonUp);
var
  Btn: TMouseButton;
begin
  if (Message.Keys and MK_XBUTTON1) <> 0 then Btn := mbExtra1
  else if (Message.Keys and MK_XBUTTON2) <> 0 then Btn := mbExtra2
  else Exit;

  if (csCaptureMouse in ControlStyle) and (Btn in CaptureMouseButtons) then
  begin
    {$IFDEF VerboseMouseCapture}
    DebugLn('TControl.WMMButtonUp ',Name,':',ClassName);
    {$ENDIF}
    MouseCapture := False;
  end;

  DoMouseUp(Message, Btn);
end;

{------------------------------------------------------------------------------
  Method: TControl.WMMouseWheel
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TControl.WMMouseWheel(var Message: TLMMouseEvent);
var
  MousePos: TPoint;
  lState: TShiftState;
  SP: TSmallPoint;
begin
  SP.X := Message.X; // cannot use SmallPoint() here due to FPC inconsistency in Classes.TSmallPoint<>Types.TSmallPoint on Linux
  SP.Y := Message.Y;
  MousePos := GetMousePosFromMessage(SP);

  lState := Message.State - [ssCaps, ssNum, ssScroll]; // Remove unreliable states, see http://bugs.freepascal.org/view.php?id=20065
  if DoMouseWheel(lState, Message.WheelDelta, MousePos) then
    Message.Result := 1 // handled, skip further handling by interface
  else
    inherited;
end;

procedure TControl.WMMouseHWheel(var Message: TLMMouseEvent);
var
  MousePos: TPoint;
  lState: TShiftState;
  SP: TSmallPoint;
begin
  SP.X := Message.X; // cannot use SmallPoint() here due to FPC inconsistency in Classes.TSmallPoint<>Types.TSmallPoint on Linux
  SP.Y := Message.Y;
  MousePos := GetMousePosFromMessage(SP);

  lState := Message.State - [ssCaps, ssNum, ssScroll]; // Remove unreliable states, see http://bugs.freepascal.org/view.php?id=20065
  if DoMouseWheelHorz(lState, Message.WheelDelta, MousePos) then
    Message.Result := 1 // handled, skip further handling by interface
  else
    inherited;
end;


{------------------------------------------------------------------------------
       TControl Click
------------------------------------------------------------------------------}
procedure TControl.Click;

  function OnClickIsActionExecute: boolean;
  begin
    Result:=false;
    if Action=nil then exit;
    if not Assigned(Action.OnExecute) then exit;
    if not Assigned(FOnClick) then exit;
    Result:=CompareMethods(TMethod(FOnClick),TMethod(Action.OnExecute));
  end;

var
  CallAction: Boolean;
begin
  //DebugLn(['TControl.Click ',DbgSName(Self)]);
  CallAction:=(not (csDesigning in ComponentState)) and (ActionLink <> nil);

  // first call our own OnClick if it differs from Action.OnExecute
  if Assigned(FOnClick)
  and ((not CallAction) or (not OnClickIsActionExecute)) then
    FOnClick(Self);
  // then trigger the Action
  if CallAction then
    ActionLink.Execute(Self);
end;

{------------------------------------------------------------------------------
  TControl DialogChar
  
  Do something useful with accelerators etc.
------------------------------------------------------------------------------}
function TControl.DialogChar(var Message: TLMKey): boolean;
begin
  Result := False;
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
function TControl.CheckChildClassAllowed(ChildClass: TClass; ExceptionOnInvalid: boolean): boolean;
begin
  Result := ChildClassAllowed(ChildClass);
  if (not Result) and ExceptionOnInvalid then
    raise EInvalidOperation.CreateFmt(rsControlClassCantContainChildClass, [ClassName, ChildClass.ClassName]);
end;

{------------------------------------------------------------------------------
  procedure TControl.CheckNewParent(AParent: TWinControl);
  
  Checks if this control can be the child of AParent.
  This check is executed in SetParent.
------------------------------------------------------------------------------}
procedure TControl.CheckNewParent(AParent: TWinControl);
begin
  if (AParent <> nil) then
    AParent.CheckChildClassAllowed(ClassType, True);
  if AParent = Self then
    raise EInvalidOperation.Create(rsAControlCanNotHaveItselfAsParent);
end;

{------------------------------------------------------------------------------
       TControl SetAutoSize
------------------------------------------------------------------------------}
procedure TControl.SetAutoSize(Value: Boolean);
begin
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
var
  PreferredWidth: integer;
  PreferredHeight: integer;
  ResizeWidth: Boolean;
  ResizeHeight: Boolean;
begin
  // handled by TWinControl, or other descendants
  ResizeWidth:=not WidthIsAnchored;
  ResizeHeight:=not HeightIsAnchored;
  if ResizeWidth or ResizeHeight then begin
    PreferredWidth:=0;
    PreferredHeight:=0;
    GetPreferredSize(PreferredWidth,PreferredHeight);
    if (not ResizeWidth) or (PreferredWidth<=0) then PreferredWidth:=Width;
    if (not ResizeHeight) or (PreferredHeight<=0) then PreferredHeight:=Height;
    SetBoundsKeepBase(Left,Top,PreferredWidth,PreferredHeight);
  end;
end;

{------------------------------------------------------------------------------
  TControl DoAllAutoSize

  Run DoAutoSize until done.
------------------------------------------------------------------------------}
procedure TControl.DoAllAutoSize;

  procedure AutoSizeControl(AControl: TControl);
  var
    AWinControl: TWinControl;
    i: Integer;
    Needed: Boolean;
  begin
    if AControl.AutoSizeDelayed then exit;
    Needed:=cfAutoSizeNeeded in AControl.FControlFlags;

    //DebugLn(['TControl.DoAllAutoSize.AutoSizeControl ',DbgSName(AControl),' AutoSize=',AControl.AutoSize,' IsControlVisible=',AControl.IsControlVisible,' cfAutoSizeNeeded=',Needed]);
    Exclude(AControl.FControlFlags, cfAutoSizeNeeded);
    if not AControl.IsControlVisible then exit;

    if Needed and AControl.AutoSize and
       (not ((AControl.Parent = nil) and (csDesigning in AControl.ComponentState)))
    then
      AControl.DoAutoSize;
    if AControl is TWinControl then
    begin
      // recursive
      AWinControl := TWinControl(AControl);
      //DebugLn(['AutoSizeControl ',DbgSName(AWinControl)]);
      AWinControl.AlignControl(nil);
      for i := 0 to AWinControl.ControlCount - 1 do
        AutoSizeControl(AWinControl.Controls[i]);
    end;
  end;

  function CallAllOnResize(AControl: TControl): boolean;
  // The OnResize event is called for Delphi compatibility after child resizes.
  // Call all OnResize events so they will hopefully only invoke one more
  // loop, instead of one per OnResize.
  var
    AWinControl: TWinControl;
    i: Integer;
  begin
    if AControl = nil then Exit(True);
    Result := False;
    if AControl is TWinControl then
    begin
      AWinControl := TWinControl(AControl);
      for i := 0 to AWinControl.ControlCount - 1 do
        if AWinControl.Controls[i].IsControlVisible
        and not CallAllOnResize(AWinControl.Controls[i]) then
          exit;
    end;
    {$IFDEF VerboseOnResize}
    debugln(['TControl.DoAllAutoSize ',DbgSName(AControl),' calling Resize ...']);
    {$ENDIF}
    AControl.Resize;
    Result := True;
  end;

var
  i: Integer;
begin
  if Parent <> nil then
    raise EInvalidOperation.Create('TControl.DoAllAutoSize Parent <> nil');
  if AutoSizingAll then exit;
  FAutoSizingAll := True;
  if not (Self is TWinControl) then exit;
  {$IFDEF VerboseAllAutoSize}
  DebugLn(['TControl.DoAllAutoSize START ',DbgSName(Self)]);
  {$ENDIF}
  //writeln(GetStackTrace(true));
  try
    i:=0;
    while (not AutoSizeDelayed) and (cfAutoSizeNeeded in FControlFlags) do
    begin
      {$IFDEF VerboseAllAutoSize}
      DebugLn(['TControl.DoAllAutoSize LOOP ',DbgSName(Self),' ',dbgs(BoundsRect)]);
      {$ENDIF}
      AutoSizeControl(Self);
      if not (cfAutoSizeNeeded in FControlFlags) then
        CallAllOnResize(Self);
      inc(i);
      if i=1000 then
        Include(FControlFlags,cfKillChangeBounds);
      if i=2000 then
        Include(FControlFlags,cfKillInvalidatePreferredSize);
      if i=3000 then
        Include(FControlFlags,cfKillAdjustSize);
    end;
  finally
    FControlFlags:=FControlFlags-[cfKillChangeBounds,
                                cfKillInvalidatePreferredSize,cfKillAdjustSize];
    FAutoSizingAll := False;
  end;
  {$IFDEF VerboseAllAutoSize}
  DebugLn(['TControl.DoAllAutoSize END ',DbgSName(Self),' ',dbgs(BoundsRect)]);
  {$ENDIF}
end;

procedure TControl.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
  const AXProportion, AYProportion: Double);
var
  AAWidth, AAHeight: Boolean;
  NewLeft, NewTop, NewWidth, NewHeight, NewRight, NewBottom, OldWidth, OldHeight,
    NewBaseLeft, NewBaseTop, NewBaseWidth, NewBaseHeight: Integer;
begin
  // Apply the changes
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    // Dimensions
    AAWidth := False;
    AAHeight := False;
    NewLeft := Left;
    NewTop := Top;
    NewWidth := Width;
    NewHeight := Height;
    OldWidth := Width;
    OldHeight := Height;

    ShouldAutoAdjust(AAWidth, AAHeight);
    AAWidth := AAWidth and (Align in [alNone, alLeft, alRight])
      and not((akLeft in Anchors) and (akRight in Anchors));
    AAHeight := AAHeight and (Align in [alNone, alTop, alBottom])
      and not((akTop in Anchors) and (akBottom in Anchors));

    if (Align=alNone) and (akLeft in Anchors) then
      NewLeft := Round(NewLeft * AXProportion);
    if (Align=alNone) and (akRight in Anchors) and (Parent<>nil)
    and (AnchorSideRight.Control=nil) then
    begin
      if not(akLeft in Anchors) then
      begin
        NewRight := Round((Parent.ClientWidth-NewLeft-OldWidth) * AXProportion);
        NewLeft := Parent.ClientWidth-NewRight-OldWidth
      end else
      begin
        NewRight := Round((Parent.ClientWidth-Left-OldWidth) * AXProportion);
        NewWidth := Parent.ClientWidth-NewLeft-NewRight;
      end;
    end;

    if (Align=alNone) and (akTop in Anchors) then
      NewTop := Round(NewTop * AYProportion);
    if (Align=alNone) and (akBottom in Anchors) and (Parent<>nil)
    and (AnchorSideBottom.Control=nil) then
    begin
      if not(akTop in Anchors) then
      begin
        NewBottom := Round((Parent.ClientHeight-NewTop-OldHeight) * AYProportion);
        NewTop := Parent.ClientHeight-NewBottom-OldHeight
      end else
      begin
        NewBottom := Round((Parent.ClientHeight-Top-OldHeight) * AYProportion);
        NewHeight := Parent.ClientHeight-NewTop-NewBottom;
      end;
    end;

    if AAWidth then
      NewWidth := Round(Width * AXProportion);
    if AAHeight then
      NewHeight := Round(Height * AYProportion);

    BorderSpacing.AutoAdjustLayout(AXProportion, AYProportion);
    Constraints.AutoAdjustLayout(AXProportion, AYProportion);

    NewBaseLeft := NewLeft;
    NewBaseTop := NewTop;
    NewBaseWidth := NewWidth;
    NewBaseHeight := NewHeight;
    NewWidth := Constraints.MinMaxWidth(NewWidth);
    NewHeight := Constraints.MinMaxHeight(NewHeight);

    if AAWidth or (NewBaseWidth<>NewWidth) then
    begin
      if akRight in Anchors then
        NewLeft := NewLeft-NewWidth+OldWidth;
    end;
    if AAHeight or (NewBaseHeight<>NewHeight) then
    begin
      if akBottom in Anchors then
        NewTop := NewTop-NewHeight+OldHeight;
    end;
    if AAWidth and (akRight in Anchors) then
      NewBaseLeft := NewBaseLeft-NewBaseWidth+OldWidth;
    if AAHeight and (akBottom in Anchors) then
      NewBaseTop := NewBaseTop-NewBaseHeight+OldHeight;

    FBaseBounds.Left:=NewBaseLeft;
    FBaseBounds.Top:=NewBaseTop;
    FBaseBounds.Right:=NewBaseLeft+NewBaseWidth;
    FBaseBounds.Bottom:=NewBaseTop+NewBaseHeight;
    if Parent<>nil then
    begin
      FBaseParentClientSize.cx:=Parent.ClientWidth;
      FBaseParentClientSize.cy:=Parent.ClientHeight;
    end;

    SetBoundsKeepBase(NewLeft, NewTop, NewWidth, NewHeight);
  end;
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
    if FAnchoredControls=nil then
      FAnchoredControls:=TFPList.Create;
    if FAnchoredControls.IndexOf(AControl)<0 then
      FAnchoredControls.Add(AControl);
  end else if FAnchoredControls<>nil then begin
    if TheAnchorSide.Owner<>nil then begin
      for Side:=Low(TAnchorKind) to High(TAnchorKind) do begin
        if (AControl.FAnchorSides[Side]<>nil)
        and (AControl.FAnchorSides[Side].Control=Self) then begin
          // still anchored
          exit;
        end;
      end;
    end;
    FAnchoredControls.Remove(AControl);
  end;
end;

function TControl.AutoSizePhases: TControlAutoSizePhases;
begin
  if Parent<>nil then
    Result:=Parent.AutoSizePhases
  else
    Result:=[];
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
          or (cfLoading in FControlFlags)
          // no autosize for invisible controls
          or (not IsControlVisible)
          // if there is no parent, then this control is not visible
          //  (TWinControl and TCustomForm override this)
          or AutoSizeDelayedHandle
          // if there is a parent, ask it
          or ((Parent<>nil) and Parent.AutoSizeDelayed);
  {$IFDEF VerboseCanAutoSize}
  if Result {and AutoSize} then begin
    DbgOut('TControl.AutoSizeDelayed Self='+DbgSName(Self)+' ');
    if FAutoSizingLockCount>0 then debugln('FAutoSizingLockCount=',dbgs(FAutoSizingLockCount))
    else if csLoading in ComponentState then debugln('csLoading')
    else if csDestroying in ComponentState then debugln('csDestroying')
    else if cfLoading in FControlFlags then debugln('cfLoading')
    else if not IsControlVisible then debugln('not IsControlVisible')
    else if AutoSizeDelayedHandle then debugln('AutoSizeDelayedHandle')
    else if ((Parent<>nil) and Parent.AutoSizeDelayed) then debugln('Parent.AutoSizeDelayed')
    else debugln('?');
  end;
  {$ENDIF}
end;

function TControl.AutoSizeDelayedReport: string;
begin
  if (FAutoSizingLockCount>0) then
    Result:='FAutoSizingLockCount='+dbgs(FAutoSizingLockCount)
  else if csLoading in ComponentState then
    Result:='csLoading'
  else if csDestroying in ComponentState then
    Result:='csDestroying'
  else if cfLoading in FControlFlags then
    Result:='cfLoading'
  else if IsControlVisible then
    Result:='not IsControlVisible'
  else if AutoSizeDelayedHandle then
    Result:='AutoSizeDelayedHandle'
  else if Parent<>nil then
    Result:=Parent.AutoSizeDelayedReport
  else
    Result:='?';
end;

{------------------------------------------------------------------------------
  TControl AutoSizeDelayedHandle

  Returns true if AutoSize should be skipped / delayed because of its handle.
  A TControl does not have a handle, so it needs a parent.
------------------------------------------------------------------------------}
function TControl.AutoSizeDelayedHandle: Boolean;
begin
  Result := Parent = nil;
end;

{------------------------------------------------------------------------------
  TControl SetBoundsRect
------------------------------------------------------------------------------}
procedure TControl.SetBoundsRect(const ARect: TRect);
begin
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
  DebugLn('[TControl.SetBoundsRect] ',Name,':',ClassName);
  {$ENDIF}
  SetBounds(ARect.Left, ARect.Top,
    Max(ARect.Right - ARect.Left, 0), Max(ARect.Bottom - ARect.Top, 0));
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
    FLoadedClientSize.cy:=Value;
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
    FLoadedClientSize.cx:=Value;
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
  if FCursor <> Value then 
  begin
    FCursor := Value;
    Perform(CM_CURSORCHANGED, 0, 0);
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
    EnabledChanging;
    FEnabled := Value;
    Perform(CM_ENABLEDCHANGED, 0, 0);
    EnabledChanged;
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
  if FHint = Value then exit;
  FHint := Value;
end;

{------------------------------------------------------------------------------
  TControl SetName
------------------------------------------------------------------------------}
procedure TControl.SetName(const Value: TComponentName);
var
  ChangeText: Boolean;
begin
  if Name=Value then exit;
  ChangeText :=
    (csSetCaption in ControlStyle) and not (csLoading in ComponentState) and
    (Name = Text) and
    ((Owner = nil) or not (Owner is TControl) or not (csLoading in Owner.ComponentState));
  inherited SetName(Value);
  if ChangeText then Text := Value;
end;

{------------------------------------------------------------------------------
  TControl Show
------------------------------------------------------------------------------}
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
  R: TRect;
begin
  DebugLn(['TControl.DoFloatMsg ',DbgSName(Self),' Floating=',Floating]);
  if Floating and (Parent <> nil) then
  begin
    P := Parent.ClientToScreen(Point(Left, Top));
    R := ADockSource.DockRect;
    Parent.BoundsRect := Bounds(R.Left + Parent.Left - P.X, R.Top + Parent.Top - P.Y,
      R.Right - R.Left + Parent.Width - Width,  R.Bottom - R.Top + Parent.Height - Height);
  end else
  begin
    FloatHost := CreateFloatingDockSite(ADockSource.DockRect);
    if FloatHost <> nil then
    begin
      FloatHost.Caption := FloatHost.GetDockCaption(Self);
      ADockSource.DragTarget := FloatHost;
      FloatHost.Show;
    end;
  end;
end;

{------------------------------------------------------------------------------
  TControl GetText
------------------------------------------------------------------------------}
function TControl.GetText: TCaption;
var
  len: Integer;
  GetTextMethod: TMethod;
begin
  // Check if GetTextBuf is overridden, otherwise we can call RealGetText directly
  Assert(Assigned(@Self.GetTextBuf), 'TControl.GetText: GetTextBuf Method is Nil');
  GetTextMethod := TMethod(@Self.GetTextBuf);
  if GetTextMethod.Code = Pointer(@TControl.GetTextBuf) then begin
    Result := RealGetText;
  end
  else begin
    // Bummer, we have to do it the compatible way.
    DebugLn('Note: GetTextBuf is overridden for: ', Classname);
    len := GetTextLen;
    if len = 0 then begin
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

function TControl.IsClientHeightStored: boolean;
begin
  Result:=false;
end;

function TControl.IsClientWidthStored: boolean;
begin
  Result:=false;
end;

function TControl.WidthIsAnchored: boolean;
var
  CurAnchors: TAnchors;
begin
  if Align=alCustom then exit(true); // width depends on parent
  CurAnchors:=Anchors;
  if Align<>alNone then CurAnchors:=CurAnchors+AnchorAlign[Align];
  Result:=(CurAnchors*[akLeft,akRight]=[akLeft,akRight]);
  if not Result then begin
    if Parent<>nil then
      Result:=Parent.ChildSizing.Layout<>cclNone;
  end;
end;

function TControl.HeightIsAnchored: boolean;
var
  CurAnchors: TAnchors;
begin
  if Align=alCustom then exit(true); // height depends on parent
  CurAnchors:=Anchors;
  if Align<>alNone then CurAnchors:=CurAnchors+AnchorAlign[Align];
  Result:=(CurAnchors*[akTop,akBottom]=[akTop,akBottom]);
  if not Result then begin
    if Parent<>nil then
      Result:=Parent.ChildSizing.Layout<>cclNone;
  end;
end;

procedure TControl.WMCancelMode(var Message: TLMessage);
begin
  SetCaptureControl(nil);
end;

function TControl.IsEnabledStored: Boolean;
begin
  Result := (ActionLink = nil) or not ActionLink.IsEnabledLinked;
end;

function TControl.IsFontStored: Boolean;
begin
  Result := not ParentFont;
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
        if not (C is TWinControl) then
          with C do
            if IsControlVisible and (csOpaque in ControlStyle) then
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

  if (CtrlIsVisible or ((csDesigning in ComponentState) and
    not (csNoDesignVisible in ControlStyle))) then
  begin
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

{------------------------------------------------------------------------------
  TControl Refresh
------------------------------------------------------------------------------}
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
  if AutoSizeDelayed then exit;

  if (FLastResizeWidth<>Width) or (FLastResizeHeight<>Height)
  or (FLastResizeClientWidth<>ClientWidth)
  or (FLastResizeClientHeight<>ClientHeight) then begin
    {if CompareText('SubPanel',Name)=0 then begin
      DebugLn(['[TControl.Resize] ',Name,':',ClassName,
      ' Last=',FLastResizeWidth,',',FLastResizeHeight,
      ' LastClient=',FLastResizeClientWidth,',',FLastResizeClientHeight,
      ' New=',Width,',',Height,
      ' NewClient=',ClientWidth,',',ClientHeight]);
      DumpStack;
    end;}
    FLastResizeWidth:=Width;
    FLastResizeHeight:=Height;
    FLastResizeClientWidth:=ClientWidth;
    FLastResizeClientHeight:=ClientHeight;
    DoOnResize;
  end;
end;

procedure TControl.Loaded;

  function FindLoadingControl(AControl: TControl): TControl;
  var
    i: Integer;
    AWinControl: TWinControl;
  begin
    if csLoading in AControl.ComponentState then exit(AControl);
    if AControl is TWinControl then begin
      AWinControl:=TWinControl(AControl);
      for i:=0 to AWinControl.ControlCount-1 do
      begin
        Result:=FindLoadingControl(AWinControl.Controls[i]);
        if Result<>nil then exit;
      end;
    end;
    Result:=nil;
  end;

  procedure ClearLoadingFlags(AControl: TControl);
  var
    i: Integer;
    AWinControl: TWinControl;
  begin
    Exclude(AControl.FControlFlags,cfLoading);
    if AControl is TWinControl then begin
      AWinControl:=TWinControl(AControl);
      for i:=0 to AWinControl.ControlCount-1 do
        ClearLoadingFlags(AWinControl.Controls[i]);
    end;
  end;

  procedure CheckLoading(AControl: TControl);
  var
    TopParent: TControl;
  begin
    TopParent:=AControl;
    while (TopParent.Parent<>nil)
    and (cfLoading in TopParent.Parent.FControlFlags) do
      TopParent:=TopParent.Parent;
    if FindLoadingControl(TopParent)<>nil then exit;
    // all components on the form finished loading
    ClearLoadingFlags(TopParent);
    // call LoadedAll
    DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.Loaded.CheckLoading'){$ENDIF};
    try
      AControl.LoadedAll;
    finally
      EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.Loaded.CheckLoading'){$ENDIF};
    end;
  end;

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
      NewWidth:=FLoadedClientSize.cx;
    NewHeight:=Height;
    if UseClientHeightForHeight then
      NewHeight:=FLoadedClientSize.cy;
    SetBoundsKeepBase(Left,Top,NewWidth,NewHeight);
  end;
    
  if Assigned(Parent) then
  begin
    if ParentColor then
    begin
      Color := Parent.Color;
      FParentColor := True;
    end;

    if ParentFont then
    begin
      Font := Parent.Font;
      FParentFont := True;
    end;

    if ParentBidiMode then
    begin
      BiDiMode := Parent.BiDiMode;
      FParentBidiMode := True;
    end;

    if ParentShowHint then
    begin
      ShowHint := Parent.ShowHint;
      FParentShowHint := True;
    end;
  end;

  UpdateBaseBounds(true,true,true);

  // store designed width and height for undocking
  FUndockHeight := Height;
  FUndockWidth := Width;
  if Action <> nil then ActionChange(Action, True);
  
  CheckLoading(Self);
end;

procedure TControl.LoadedAll;
begin
  AdjustSize;

  {$IFDEF VerboseOnResize}
  debugln(['TControl.LoadedAll ',DbgSName(Self),' calling Resize ...']);
  {$ENDIF}
  Resize;
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

procedure TControl.ReadState(Reader: TReader);
begin
  Include(FControlFlags, cfLoading);
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ReadState'){$ENDIF};
  try
    Include(FControlState, csReadingState);
    inherited ReadState(Reader);
  finally
    Exclude(FControlState, csReadingState);
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ReadState'){$ENDIF};
  end;
end;

procedure TControl.FormEndUpdated;
// called when control is on a form and EndFormUpdate reached 0
// it is called recursively
begin

end;

{------------------------------------------------------------------------------
  TControl SetBounds
------------------------------------------------------------------------------}
procedure TControl.SetBounds(aLeft, aTop, aWidth, aHeight: integer);
begin
  ChangeBounds(ALeft, ATop, AWidth, AHeight, false);
end;

{------------------------------------------------------------------------------
  TControl SetConstraints
------------------------------------------------------------------------------}
procedure TControl.SetConstraints(const Value : TSizeConstraints);
begin
  FConstraints.Assign(Value);
end;

procedure TControl.SetDesktopFont(const AValue: Boolean);
begin
  if FDesktopFont <> AValue then
  begin
    FDesktopFont := AValue;
    Perform(CM_SYSFONTCHANGED, 0, 0);
  end;
end;

{------------------------------------------------------------------------------
  TControl SetAlign
------------------------------------------------------------------------------}
procedure TControl.SetAlign(Value: TAlign);
var
  OldAlign: TAlign;
  a: TAnchorKind;
  OldBaseBounds: TRect;
begin
  if FAlign = Value then exit;
  //DebugLn(['TControl.SetAlign ',DbgSName(Self),' Old=',DbgS(FAlign),' New=',DbgS(Value),' ',Anchors<>AnchorAlign[FAlign]]);
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetAlign'){$ENDIF};
  try
    OldBaseBounds:=BaseBounds;
    OldAlign := FAlign;
    FAlign := Value;
    if (not (csLoading in ComponentState))
    and (Align in [alLeft,alTop,alRight,alBottom,alClient]) then begin
      // Align for alLeft,alTop,alRight,alBottom,alClient takes precedence
      // over AnchorSides => clean up
      for a:=low(TAnchorKind) to High(TAnchorKind) do
      begin
        if not (a in AnchorAlign[FAlign]) then continue;
        AnchorSide[a].Control:=nil;
        AnchorSide[a].Side:=asrTop;
      end;
    end;
    // Notes:
    // - if anchors had default values then change them to new default values
    //   This is done for Delphi compatibility.
    // - Anchors are not stored if they are AnchorAlign[Align]
    if (Anchors = AnchorAlign[OldAlign]) and (Anchors <> AnchorAlign[FAlign]) then
      Anchors := AnchorAlign[FAlign];
    if not (csLoading in ComponentState) then
      BoundsRect:=OldBaseBounds;
    //DebugLn(['TControl.SetAlign ',DbgSName(Self),' Cur=',DbgS(FAlign),' New=',DbgS(Value),' ',Anchors<>AnchorAlign[FAlign],' Anchors=',dbgs(Anchors)]);
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetAlign'){$ENDIF};
  end;
end;

{------------------------------------------------------------------------------
  TControl SetAnchors
------------------------------------------------------------------------------}
procedure TControl.SetAnchors(const AValue: TAnchors);
var
  NewAnchors: TAnchors;
  a: TAnchorKind;
begin
  if Anchors = AValue then Exit;
  NewAnchors:=AValue-FAnchors;
  FAnchors := AValue;
  for a:=Low(TAnchorKind) to high(TAnchorKind) do
    if (a in NewAnchors) and (AnchorSide[a].Side=asrCenter) then
      AnchorSide[a].FixCenterAnchoring;

  // Delphi Anchors depend on the current bounds of Self and Parent.ClientRect
  // => fetch current BaseBounds
  // for example:
  // during disabled autosizing:  Width:=100; Anchors:=Anchors+[akRight];
  UpdateAnchorRules;

  AdjustSize;
end;

{------------------------------------------------------------------------------
  TControl RequestAlign

  Requests the parent to realign all brothers
------------------------------------------------------------------------------}
procedure TControl.RequestAlign;
begin
  AdjustSize;
end;

procedure TControl.UpdateBaseBounds(StoreBounds,
  StoreParentClientSize, UseLoadedValues: boolean);
var
  NewBaseBounds: TRect;
  NewBaseParentClientSize: TSize;
begin
  if (csLoading in ComponentState) or (fBaseBoundsLock>0) then exit;
  if StoreBounds then
    NewBaseBounds:=BoundsRect
  else
    NewBaseBounds:=FBaseBounds;
  if StoreParentClientSize then begin
    if Parent<>nil then begin
      NewBaseParentClientSize:=Size(Parent.ClientWidth,Parent.ClientHeight);
      if UseLoadedValues then begin
        if cfClientWidthLoaded in Parent.FControlFlags then
          NewBaseParentClientSize.cx:=Parent.FLoadedClientSize.cx;
        if cfClientHeightLoaded in Parent.FControlFlags then
          NewBaseParentClientSize.cy:=Parent.FLoadedClientSize.cy;
      end;
    end else
      NewBaseParentClientSize:=Size(0,0);
  end else
    NewBaseParentClientSize:=FBaseParentClientSize;

  if (not CompareRect(@NewBaseBounds,@FBaseBounds))
  or (NewBaseParentClientSize.cx<>FBaseParentClientSize.cx)
  or (NewBaseParentClientSize.cy<>FBaseParentClientSize.cy)
  then begin
    //if csDesigning in ComponentState then
    {$IFDEF CHECK_POSITION}
    if CheckPosition(Self) then
      DebugLn(['TControl.UpdateBaseBounds '+DbgSName(Self),
      ' OldBounds='+dbgs(FBaseBounds),
      ' OldParentClientSize='+dbgs(FBaseParentClientSize),
      ' NewBounds='+dbgs(NewBaseBounds),
      ' NewParentClientSize='+dbgs(NewBaseParentClientSize),
      '']);
    {$ENDIF}

    FBaseBounds:=NewBaseBounds;
    FBaseParentClientSize:=NewBaseParentClientSize;
  end;
  Include(FControlFlags,cfBaseBoundsValid);
end;

procedure TControl.WriteLayoutDebugReport(const Prefix: string);
var
  a: TAnchorKind;
  NeedSeparator: Boolean;
begin
  DbgOut(Prefix,'TControl.WriteLayoutDebugReport ');
  DbgOut(DbgSName(Self),' Bounds=',dbgs(BoundsRect));
  if Align<>alNone then
    DbgOut(' Align=',DbgS(Align));
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

procedure TControl.AutoAdjustLayout(AMode: TLayoutAdjustmentPolicy;
  const AFromPPI, AToPPI, AOldFormWidth, ANewFormWidth: Integer);
var
  lXProportion, lYProportion: Double;
  lMode: TLayoutAdjustmentPolicy;
begin
  // First resolve ladDefault
  lMode := AMode;
  if lMode = lapDefault then lMode := Application.LayoutAdjustmentPolicy;

  // X-axis adjustment proportion
  lXProportion := 1.0;
  if lMode = lapAutoAdjustWithoutHorizontalScrolling then
  begin
    if AOldFormWidth > 0 then lXProportion := ANewFormWidth / AOldFormWidth;
  end
  else if lMode = lapAutoAdjustForDPI then
  begin
    if AFromPPI > 0 then lXProportion := AToPPI / AFromPPI;
  end;

  // y-axis adjustment proportion
  if AFromPPI > 0 then lYProportion := AToPPI / AFromPPI
  else lYProportion := 1.0;

  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.AutoAdjustLayout'){$ENDIF};
  try
    if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
      ScaleFontsPPI(AToPPI, lYProportion);

    DoAutoAdjustLayout(lMode, lXProportion, lYProportion);
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.AutoAdjustLayout'){$ENDIF};
  end;
end;

// Auto-adjust the layout of controls.
procedure TControl.ShouldAutoAdjust(var AWidth, AHeight: Boolean);
begin
  AWidth := not AutoSize;
  AHeight := not AutoSize;
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
  Result := Caption;
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
  if csLoading in ComponentState then
  begin
    inc(FReadBounds.Right, Value - FReadBounds.Left);
    FReadBounds.Left := Value;
    Include(FControlFlags, cfLeftLoaded);
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
  if csLoading in ComponentState then
  begin
    inc(FReadBounds.Bottom,Value - FReadBounds.Top);
    FReadBounds.Top := Value;
    Include(FControlFlags, cfTopLoaded);
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
      raise EInvalidOperation.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Negative width '
          +dbgs(Value)+' not allowed.');
    if Value>=10000 then
      raise EInvalidOperation.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Width '
          +dbgs(Value)+' not allowed.');
  end;

begin
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
  DebugLn('[TControl.SetWidth] ',Name,':',ClassName,' ',dbgs(Value));
  {$ENDIF}
  if csLoading in ComponentState then
  begin
    FReadBounds.Right := FReadBounds.Left+Value;
    Include(FControlFlags, cfWidthLoaded);
  end;
  if [csDesigning, csDestroying, csLoading] * ComponentState = [csDesigning] then
    CheckDesignBounds;
  SetBounds(FLeft, FTop, Max(0, Value), FHeight);
end;

class procedure TControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterControl;
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
      raise EInvalidOperation.Create(
        'TWinControl.SetHeight ('+DbgSName(Self)+'): Negative height '
          +dbgs(Value)+' not allowed.');
    if Value>=10000 then
      raise EInvalidOperation.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Height '
          +dbgs(Value)+' not allowed.');
  end;

begin
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
  DebugLn('[TControl.SetHeight] ',Name,':',ClassName,' ',dbgs(Value));
  {$ENDIF}
  if csLoading in ComponentState then
  begin
    FReadBounds.Bottom := FReadBounds.Top + Value;
    Include(FControlFlags, cfHeightLoaded);
  end;
  if [csDesigning, csDestroying, csLoading] * ComponentState = [csDesigning] then
    CheckDesignBounds;
  SetBounds(FLeft, FTop, FWidth, Max(0, Value));
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
  procedure TControl.SetParent(NewParent : TWinControl);
------------------------------------------------------------------------------}
procedure TControl.SetParent(NewParent: TWinControl);
begin
  if FParent = NewParent then exit;
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetParent'){$ENDIF};
  try
    CheckNewParent(NewParent);
    if FParent <> nil then FParent.RemoveControl(Self);
    if cfBoundsRectForNewParentValid in FControlFlags then
    begin
      Exclude(FControlFlags, cfBoundsRectForNewParentValid);
      BoundsRect := BoundsRectForNewParent;
    end;
    if NewParent <> nil then NewParent.InsertControl(Self);
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetParent'){$ENDIF};
  end;
end;

{------------------------------------------------------------------------------
  TControl SetParentComponent
------------------------------------------------------------------------------}
procedure TControl.SetParentComponent(NewParentComponent: TComponent);
begin
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
      Perform(CM_PARENTFONTCHANGED, 0, 0);
  end;
end;

{------------------------------------------------------------------------------
  TControl SetParentShowHint
------------------------------------------------------------------------------}
procedure TControl.SetParentShowHint(Value : Boolean);
begin
  if FParentShowHint <> Value then
  begin
    FParentShowHint := Value;
    if Assigned(FParent) and not (csReading in ComponentState) then
      Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
  end;
end;

{------------------------------------------------------------------------------
  TControl SetPopupMenu
------------------------------------------------------------------------------}
procedure TControl.SetPopupMenu(Value: TPopupMenu);
begin
  FPopupMenu := Value;
  if FPopupMenu <> nil then
    FPopupMenu.FreeNotification(Self);
end;

{------------------------------------------------------------------------------
  TControl WMMouseMove
------------------------------------------------------------------------------}
procedure TControl.WMMouseMove(var Message: TLMMouseMove);
var
  MP: TPoint;
begin
  {$IFDEF VerboseMouseBugfix}
  DebugLn(['[TControl.WMMouseMove] ',Name,':',ClassName,' ',Message.XPos,',',Message.YPos]);
  {$ENDIF}
  MP := GetMousePosFromMessage(Message.Pos);
  UpdateMouseCursor(MP.X,MP.Y);
  if not (csNoStdEvents in ControlStyle) then
    MouseMove(KeystoShiftState(Word(Message.Keys)), MP.X, MP.Y);
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
  end;

  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

{------------------------------------------------------------------------------
  TControl MouseUp
------------------------------------------------------------------------------}
procedure TControl.MouseUp(Button: TMouseButton; Shift:TShiftState;
  X, Y: Integer);
begin
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
  if FVisible <> Value then
  begin
    //DebugLn(['TControl.SetVisible ',DbgSName(Self),' NewVisible=',Value]);
    DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetVisible'){$ENDIF};
    try
      VisibleChanging;
      FVisible := Value;
      try
        // create/destroy handle
        Perform(CM_VISIBLECHANGED, WParam(Ord(Value)), 0);// see TWinControl.CMVisibleChanged

        if (Self is TWinControl) then
          AsWincontrol := TWinControl(Self)
        else
          AsWincontrol := nil;
        InvalidatePreferredSize;
        if Assigned(AsWincontrol) then
          AsWincontrol.InvalidatePreferredChildSizes;
        AdjustSize;
        if (not Visible) and Assigned(Parent) then
        begin
          // control became invisible, so AdjustSize was not propagated to Parent
          // => propagate now
          Parent.InvalidatePreferredSize;
          Parent.AdjustSize;
        end;
      finally
        VisibleChanged;
      end;
    finally
      EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetVisible'){$ENDIF};
    end;
  end;
  if (csLoading in ComponentState) then
    ControlState := ControlState + [csVisibleSetInLoading];
end;

procedure TControl.DoOnParentHandleDestruction;
begin
  // nothing, implement in descendats
end;

{------------------------------------------------------------------------------
       TControl.SetZOrder

------------------------------------------------------------------------------}
procedure TControl.SetZOrder(TopMost: Boolean);
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
  Result := not ((csDestroying in ComponentState) or (csDestroyingHandle in FControlState)) and IsControlVisible;
  if Result and Assigned(Parent) then
    Result := Parent.HandleObjectShouldBeVisible;
  //DebugLn(['TControl.HandleObjectShouldBeVisible ',DbgSName(Self),' ',Result]);
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
  {$IFDEF VerboseLCLHelp}
  debugln(['TControl.ShowHelp ',DbgSName(Self)]);
  {$ENDIF}
  if HelpType = htContext then
  begin
    if HelpContext <> 0 then
    begin
      Application.HelpContext(HelpContext);
      Exit;
    end;
  end
  else
  begin
    if HelpKeyword <> '' then
    begin
      Application.HelpKeyword(HelpKeyword);
      Exit;
    end;
  end;
  if Parent <> nil then
    Parent.ShowHelp;
end;

function TControl.HasHelp: Boolean;
begin
  if HelpType = htContext then
    Result := HelpContext <> 0
  else
    Result := HelpKeyword <> '';
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
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.Dock'){$ENDIF};
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

    //debugln(['TControl.Dock A ',DbgSName(Self),' NewDockSite=',DbgSName(NewDockSite),' ',NewDockSite.Visible]);

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
    if (FHostDockSite<>NewDockSite)
    and (NewDockSite<>nil) and (NewDockSite.FDockClients<>nil) then
      NewDockSite.FDockClients.Remove(Self);
    Exclude(FControlState, csDocking);
  end;
  EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.Dock'){$ENDIF};

  //DebugLn(['TControl.Dock END ',DbgSName(Self),' ',DbgSName(HostDockSite)]);
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
  if DropControl<>nil then
    DropControl.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ManualDock DropControl'){$ENDIF};
  if NewDockSite<>nil then
    NewDockSite.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ManualDock NewDockSite'){$ENDIF};
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
    //DebugLn('TControl.ManualDock ',Name,' NewDockSite=nil HostDockSiteManagerAvailable=',dbgs(HostDockSiteManagerAvailable(HostDockSite)),' NewBounds=',dbgs(NewBounds));
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
      // Note: This can *not* be combined with ManualFloat, because that would
      //       create a new HostDockSite
      //DebugLn('TControl.ManualDock UNDOCKING ',Name);
      Result:=HostDockSite.DoUndock(NewDockSite,Self);
    end;
    
    if Result then begin
      //DebugLn('TControl.ManualDock DOCKING ',Name);
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
        //DebugLn('TControl.ManualDock DOCKDROP ',Name,' DockRect=',dbgs(DockObject.DockRect),' NewPos=',dbgs(NewPosition));
        NewDockSite.DockDrop(DockObject,NewPosition.X,NewPosition.Y);
      finally
        DockObject.Free;
      end;
    end;
  end;
  if NewDockSite<>nil then
    NewDockSite.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ManualDock NewDockSite'){$ENDIF};
  if DropControl<>nil then
    DropControl.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ManualDock DropControl'){$ENDIF};
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
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ManualFloat'){$ENDIF};
  // undock from old host dock site
  if HostDockSite = nil then
  begin
    Result := True;
    if Parent <> nil then
      Parent.DoUndockClientMsg(nil, Self);
  end 
  else 
  begin
    Result := HostDockSite.DoUndock(nil, Self, KeepDockSiteSize);
  end;

  // create new float dock site and dock this control into it.
  if Result then 
  begin
    FloatHost := CreateFloatingDockSite(TheScreenRect);
    //debugln('TControl.ManualFloat A '+Name,':',ClassName,' ',dbgs(TheScreenRect),' FloatHost=',dbgs(FloatHost<>nil));
    if FloatHost <> nil then
    begin
      // => dock this control into it.
      FloatHost.Caption := FloatHost.GetDockCaption(Self);
      FloatHost.Visible := True;
      Dock(FloatHost,Rect(0, 0, FloatHost.ClientWidth, FloatHost.ClientHeight))
    end 
    else
      Dock(nil, TheScreenRect);
  end;
  EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ManualFloat'){$ENDIF};
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

  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ReplaceDockedControl'){$ENDIF};
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
  EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.ReplaceDockedControl'){$ENDIF};
end;

procedure TControl.AddHandlerOnResize(const OnResizeEvent: TNotifyEvent;
  AsFirst: boolean);
begin
  AddHandler(chtOnResize,TMethod(OnResizeEvent),AsFirst);
end;

procedure TControl.RemoveHandlerOnResize(const OnResizeEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnResize,TMethod(OnResizeEvent));
end;

procedure TControl.AddHandlerOnChangeBounds(
  const OnChangeBoundsEvent: TNotifyEvent; AsFirst: boolean);
begin
  AddHandler(chtOnChangeBounds,TMethod(OnChangeBoundsEvent),AsFirst);
end;

procedure TControl.RemoveHandlerOnChangeBounds(
  const OnChangeBoundsEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnChangeBounds,TMethod(OnChangeBoundsEvent));
end;

procedure TControl.AddHandlerOnVisibleChanging(
  const OnVisibleChangingEvent: TNotifyEvent; AsFirst: boolean);
begin
  AddHandler(chtOnVisibleChanging,TMethod(OnVisibleChangingEvent),AsFirst);
end;

procedure TControl.RemoveHandlerOnVisibleChanging(
  const OnVisibleChangingEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnVisibleChanging,TMethod(OnVisibleChangingEvent));
end;

procedure TControl.AddHandlerOnVisibleChanged(
  const OnVisibleChangedEvent: TNotifyEvent; AsFirst: boolean);
begin
  AddHandler(chtOnVisibleChanged,TMethod(OnVisibleChangedEvent),AsFirst);
end;

procedure TControl.RemoveHandlerOnVisibleChanged(
  const OnVisibleChangedEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnVisibleChanged,TMethod(OnVisibleChangedEvent));
end;

procedure TControl.AddHandlerOnEnabledChanged(
  const OnEnabledChangedEvent: TNotifyEvent; AsFirst: boolean);
begin
  AddHandler(chtOnEnabledChanged,TMethod(OnEnabledChangedEvent),AsFirst);
end;

procedure TControl.RemoveHandlerOnEnableChanging(
  const OnEnableChangingEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnEnabledChanged,TMethod(OnEnableChangingEvent));
end;

procedure TControl.AddHandlerOnKeyDown(const OnKeyDownEvent: TKeyEvent;
  AsFirst: boolean);
begin
  AddHandler(chtOnKeyDown,TMethod(OnKeyDownEvent),AsFirst);
end;

procedure TControl.RemoveHandlerOnKeyDown(const OnKeyDownEvent: TKeyEvent);
begin
  RemoveHandler(chtOnKeyDown,TMethod(OnKeyDownEvent));
end;

procedure TControl.AddHandlerOnBeforeDestruction(
  const OnBeforeDestructionEvent: TNotifyEvent; AsFirst: boolean);
begin
  AddHandler(chtOnBeforeDestruction,TMethod(OnBeforeDestructionEvent));
end;

procedure TControl.RemoveHandlerOnBeforeDestruction(
  const OnBeforeDestructionEvent: TNotifyEvent);
begin
  RemoveHandler(chtOnBeforeDestruction,TMethod(OnBeforeDestructionEvent));
end;

procedure TControl.AddHandlerOnMouseWheel(
  const OnMouseWheelEvent: TMouseWheelEvent; AsFirst: boolean);
begin
  AddHandler(chtOnMouseWheel,TMethod(OnMouseWheelEvent),AsFirst);
end;

procedure TControl.RemoveHandlerOnMouseWheel(
  const OnMouseWheelEvent: TMouseWheelEvent);
begin
  RemoveHandler(chtOnMouseWheel,TMethod(OnMouseWheelEvent));
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
  if BufSize <= 0 then Exit(0);

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
  if RealGetText = Value then Exit;
  FCaption := Value;
  Perform(CM_TEXTCHANGED, 0, 0);
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
  //if CompareText(Name,'MainForm')=0 then debugln('TControl.SetText A ',DbgSName(Self),' GetText="',GetText,'" Value="',Value,'" FCaption="',FCaption,'"');
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
  if HostDockSite <> nil then
    HostDockSite.UpdateDockCaption(nil);
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
  MouseCapture := False;
  // explicit notification about component destruction. this can be a drag target
  DragManager.Notification(Self, opRemove);
  Application.ControlDestroyed(Self);
  if (FHostDockSite <> nil) and not (csDestroying in FHostDockSite.ComponentState) then
  begin
    FHostDockSite.DoUndockClientMsg(nil, Self);
    SetParent(nil);
    Dock(nil, BoundsRect);
    FHostDockSite := nil;
  end else
  begin
    if Assigned(FHostDockSite) and Assigned(FHostDockSite.FDockClients) then
    begin
      FHostDockSite.FDockClients.Remove(Self);
      FHostDockSite := nil;
    end;
    SetParent(nil);
  end;
  if FAnchoredControls <> nil then
  begin
    for i := 0 to FAnchoredControls.Count - 1 do
      for Side := Low(TAnchorKind) to High(TAnchorKind) do
      begin
        CurAnchorSide := AnchoredControls[i].AnchorSide[Side];
        if (CurAnchorSide<>nil) and (CurAnchorSide.FControl = Self) then
          CurAnchorSide.FControl := nil;
      end;
    FreeThenNil(FAnchoredControls);
  end;
  FreeThenNil(FActionLink);
  for Side := Low(FAnchorSides) to High(FAnchorSides) do
    FreeThenNil(FAnchorSides[Side]);
  FreeThenNil(FBorderSpacing);
  FreeThenNil(FConstraints);
  FreeThenNil(FFont);
  FreeThenNil(FAccessibleObject);
  //DebugLn('[TControl.Destroy] B ',DbgSName(Self));
  inherited Destroy;
  //DebugLn('[TControl.Destroy] END ',DbgSName(Self));
  for HandlerType := Low(TControlHandlerType) to High(TControlHandlerType) do
    FreeThenNil(FControlHandlers[HandlerType]);
  {$IFDEF DebugDisableAutoSizing}
  FreeAndNil(FAutoSizingLockReasons);
  {$ENDIF}
end;

procedure TControl.BeforeDestruction;
begin
  inherited BeforeDestruction;
  DoCallNotifyHandler(chtOnBeforeDestruction);
end;

{------------------------------------------------------------------------------
  Method: TControl.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TControl.Create(TheOwner: TComponent);
var
  Side: TAnchorKind;
begin
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.Create'){$ENDIF};
  try
    //if AnsiCompareText(ClassName,'TSpeedButton')=0 then
    //  DebugLn('TControl.Create START ',Name,':',ClassName);
    inherited Create(TheOwner);

    // no csOpaque: delphi compatible, win32 themes notebook depend on it
    // csOpaque means entire client area will be drawn
    // (most controls are semi-transparent)
    FAccessibleObject := CreateAccessibleObject();
    FControlStyle := FControlStyle
                   +[csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
    FConstraints:= TSizeConstraints.Create(Self);
    FBorderSpacing := CreateControlBorderSpacing;
    for Side:=Low(FAnchorSides) to High(FAnchorSides) do
      FAnchorSides[Side]:=TAnchorSide.Create(Self,Side);

    FBaseBounds.Right := -1;
    FAnchors := [akLeft,akTop];
    FAlign := alNone;
    FCaptureMouseButtons := [mbLeft];
    FColor := {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    FVisible := True;
    FParentBidiMode := True;
    FParentColor := True;
    FParentFont := True;
    FDesktopFont := True;
    FParentShowHint := True;
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
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.Create'){$ENDIF};
  end;
end;

{------------------------------------------------------------------------------
  Method: TControl.CreateControlBorderSpacing
  Params:  None
  Returns: ControlBorderSpacing instance

  Creates the default ControlBorderSpacing. Allowes descendant controls to overide
  this.
 ------------------------------------------------------------------------------}
function TControl.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TControlBorderSpacing.Create(Self);
end;

{------------------------------------------------------------------------------
  Method:  TControl.GetDeviceContext
  Params:  WindowHandle: the windowhandle of this control
  Returns: a Devicecontext

  Get the devicecontext of the parent Wincontrol for this Control.
 ------------------------------------------------------------------------------}
function TControl.GetDeviceContext(var WindowHandle: HWND): HDC;
begin
  if Parent = nil then
    raise EInvalidOperation.CreateFmt(rsControlHasNoParentWindow, [Name]);

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

function TControl.GetParentComponent: TComponent;
begin
  Result := Parent;
end;

{------------------------------------------------------------------------------
  function TControl.IsParentOf(AControl: TControl): boolean;

 ------------------------------------------------------------------------------}
function TControl.IsParentOf(AControl: TControl): boolean;
begin
  Result := False;
  while Assigned(AControl) do
  begin
    AControl := AControl.Parent;
    if Self = AControl then
      Exit(True);
  end;
end;

function TControl.GetTopParent: TControl;
begin
  Result := Self;
  while Assigned(Result.Parent) do
    Result := Result.Parent;
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
procedure TControl.AnchorToNeighbour(Side: TAnchorKind; Space: TSpacingSize;
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

procedure TControl.AnchorParallel(Side: TAnchorKind; Space: TSpacingSize;
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

procedure TControl.AnchorToCompanion(Side: TAnchorKind; Space: TSpacingSize;
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

procedure TControl.AnchorAsAlign(TheAlign: TAlign; Space: TSpacingSize);
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

procedure TControl.AnchorClient(Space: TSpacingSize);
begin
  AnchorAsAlign(alClient,Space);
end;

function TControl.AnchoredControlCount: integer;
begin
  if FAnchoredControls = nil then
    Result := 0
  else
    Result := FAnchoredControls.Count;
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

procedure TControl.SetBoundsKeepBase(aLeft, aTop, aWidth, aHeight: integer);
begin
  ChangeBounds(aLeft, aTop, aWidth, aHeight, true);
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
    if (PreferredWidth<0)
    or ((PreferredWidth=0) and (not (csAutoSize0x0 in ControlStyle))) then begin
      if AutoSize or WidthIsAnchored then
        PreferredWidth:=GetDefaultWidth
      else
        PreferredWidth:=Width;
    end;
    if (PreferredHeight<0)
    or ((PreferredHeight=0) and (not (csAutoSize0x0 in ControlStyle))) then begin
      if AutoSize or HeightIsAnchored then
        PreferredHeight:=GetDefaultHeight
      else
        PreferredHeight:=Height;
    end;

    // apply constraints
    PreferredWidth:=Constraints.MinMaxWidth(PreferredWidth);
    PreferredHeight:=Constraints.MinMaxHeight(PreferredHeight);
  end;
end;

function TControl.GetCanvasScaleFactor: Double;
begin
  Result := TWSControlClass(WidgetSetClass).GetCanvasScaleFactor(Self);
end;

{------------------------------------------------------------------------------
  function TControl.GetDefaultWidth: integer;

  The default width for this control independent of any calculated values
  like Width and GetPreferredSize.
 ------------------------------------------------------------------------------}
function TControl.GetDefaultWidth: integer;
begin
  if WidthIsAnchored then
    // if width is anchored the read and base bounds were changed at designtime
    Result := Scale96ToFont(GetControlClassDefaultSize.cx)
  else if cfBaseBoundsValid in FControlFlags then
    Result := FBaseBounds.Right - FBaseBounds.Left
  else
  if cfWidthLoaded in FControlFlags then
    Result := FReadBounds.Right - FReadBounds.Left
  else
    Result := Scale96ToFont(GetControlClassDefaultSize.cx);
end;

{------------------------------------------------------------------------------
  function TControl.GetDefaultHeight: integer;

  The default height for this control independent of any calculated values
  like Height and GetPreferredSize.
 ------------------------------------------------------------------------------}
function TControl.GetDefaultHeight: integer;
begin
  if HeightIsAnchored then
    // if height is anchored the read and base bounds were changed at designtime
    Result := Scale96ToFont(GetControlClassDefaultSize.cy)
  else if cfBaseBoundsValid in FControlFlags then
    Result := BaseBounds.Bottom - BaseBounds.Top
  else
  if cfHeightLoaded in FControlFlags then
    Result := FReadBounds.Bottom - FReadBounds.Top
  else
    Result := Scale96ToFont(GetControlClassDefaultSize.cy);
end;

{------------------------------------------------------------------------------
  class function TControl.GetControlClassDefaultSize: TPoint;

  The default size of this type of controls.
  Used by GetDefaultWidth and GetDefaultHeight.
 ------------------------------------------------------------------------------}
class function TControl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 75;
  Result.CY := 50;
end;

{------------------------------------------------------------------------------
  procedure TControl.GetSidePosition;

  Utility function to retrieve Left,Top,Right and Bottom.
 ------------------------------------------------------------------------------}
function TControl.GetSidePosition(Side: TAnchorKind): integer;
begin
  case Side of
    akLeft: Result := Left;
    akTop: Result := Top;
    akRight: Result := Left + Width;
    akBottom: Result := Top + Height;
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

  procedure RaiseLoop;
  begin
    raise Exception.Create('TControl.InvalidatePreferredSize loop detected '+DbgSName(Self)+' Bounds='+dbgs(BoundsRect));
  end;

var
  AControl: TControl;
begin
  AControl:=Self;
  while AControl<>nil do begin
    Exclude(AControl.FControlFlags,cfPreferredSizeValid);
    Exclude(AControl.FControlFlags,cfPreferredMinSizeValid);
    if AControl is TWinControl then
      Exclude(TWinControl(AControl).FWinControlFlags,wcfAdjustedLogicalClientRectValid);
    if not AControl.IsControlVisible then break;
    if (AControl.Parent=nil)
      and (cfKillInvalidatePreferredSize in AControl.FControlFlags)
    then
      RaiseLoop;
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

procedure TControl.DisableAutoSizing
  {$IFDEF DebugDisableAutoSizing}(const Reason: string){$ENDIF};
begin
  inc(FAutoSizingLockCount);
  {$IFDEF DebugDisableAutoSizing}
  if FAutoSizingLockReasons=nil then FAutoSizingLockReasons:=TStringList.Create;
  FAutoSizingLockReasons.Add(Reason);
  {$ENDIF}
  //DebugLn([Space(FAutoSizingLockCount*2),'TControl.DisableAutoSizing ',DbgSName(Self),' ',FAutoSizingLockCount]);
  if FAutoSizingLockCount=1 then
  begin
    if Parent<>nil then
    begin
      //DebugLn([Space(FAutoSizingLockCount*2),'TControl.DisableAutoSizing ',DbgSName(Self),' disable Parent=',DbgSName(Parent)]);
      Parent.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.DisableAutoSizing'){$ENDIF};
    end;
  end;
end;

procedure TControl.EnableAutoSizing
  {$IFDEF DebugDisableAutoSizing}(const Reason: string){$ENDIF};

  {$IFDEF DebugDisableAutoSizing}
  procedure CheckReason;
  var
    i: Integer;
    s: String;
  begin
    i:=FAutoSizingLockReasons.Count-1;
    while i>=0 do begin
      if FAutoSizingLockReasons[i]=Reason then begin
        FAutoSizingLockReasons.Delete(i);
        exit;
      end;
      dec(i);
    end;
    s:='TControl.EnableAutoSizing '+DbgSName(Self)+' never disabled with reason "'+Reason+'"';
    for i:=0 to FAutoSizingLockReasons.Count-1 do
      s+=','+LineEnding+'reason['+IntToStr(i)+']="'+FAutoSizingLockReasons[i]+'"';
    RaiseGDBException(s);
  end;
  {$ENDIF}

begin
  {$IFDEF DebugDisableAutoSizing}
  CheckReason;
  {$ENDIF}

  if FAutoSizingLockCount<=0 then
    raise EInvalidOperation.Create('TControl.EnableAutoSizing '+DbgSName(Self)+': missing DisableAutoSizing');

  dec(FAutoSizingLockCount);
  //DebugLn([Space(FAutoSizingLockCount*2),'TControl.EnableAutoSizing ',DbgSName(Self),' ',FAutoSizingLockCount]);
  if (FAutoSizingLockCount=0) then
  begin
    if (Parent<>nil) then
    begin
      //DebugLn([Space(FAutoSizingLockCount*2),'TControl.EnableAutoSizing ',DbgSName(Self),' enable Parent ',DbgSName(Parent)]);
      Parent.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.DisableAutoSizing'){$ENDIF};
    end else
      DoAllAutoSize;
  end;
end;

{$IFDEF DebugDisableAutoSizing}
procedure TControl.WriteAutoSizeReasons(NotIfEmpty: boolean);
begin
  if NotIfEmpty and (FAutoSizingLockReasons.Count=0) then exit;
  DebugLn(['TControl.WriteAutoSizeReasons ',DbgSName(Self)]);
  debugln(FAutoSizingLockReasons.Text);
end;
{$ENDIF}

procedure TControl.EndAutoSizing;
  procedure Error;
  begin
    RaiseGDBException('TControl.EndAutoSizing');
  end;
begin
  if not FAutoSizingSelf then Error;
  FAutoSizingSelf := False;
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
  Message.Result := 0;
end;

{------------------------------------------------------------------------------
  Method: TControl.WMSize
  Params:   Message : TLMSize
  Returns:  nothing

  Event handler for LMSize messages.
  Overriden by TWinControl.WMSize.
 ------------------------------------------------------------------------------}
procedure TControl.WMSize(var Message : TLMSize);
begin
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
  DebugLn('[TControl.WMSize] Name=',Name,':',ClassName,' Message.Width=',DbgS(Message.Width),' Message.Height=',DbgS(Message.Height),' Width=',DbgS(Width),' Height=',DbgS(Height));
  {$ENDIF}
  //DebugLn(Format('Trace:[TWinControl.WMSize] %s', [ClassName]));

  if Assigned(Parent) then
    SetBoundsKeepBase(Left,Top,Message.Width,Message.Height)
  else
    SetBounds(Left,Top,Message.Width,Message.Height);
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
  // Just sync the coordinates
  if Assigned(Parent) then
    SetBoundsKeepBase(Message.XPos, Message.YPos, Width, Height)
  else
    SetBounds(Message.XPos, Message.YPos, Width, Height);
end;

{------------------------------------------------------------------------------
   Method:  TControl.SetBiDiMode
 ------------------------------------------------------------------------------}

procedure TControl.SetBiDiMode(AValue: TBiDiMode);
begin
  if FBiDiMode=AValue then exit;
  FBiDiMode:=AValue;
  FParentBiDiMode := False;
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetBiDiMode'){$ENDIF};
  try
    Perform(CM_BIDIMODECHANGED, 0, 0); // see TWinControl.CMBiDiModeChanged
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TControl.SetBiDiMode'){$ENDIF};
  end;
end;

{------------------------------------------------------------------------------
   Method:  TControl.SetParentBiDiMode
 ------------------------------------------------------------------------------}

procedure TControl.SetParentBiDiMode(AValue: Boolean);
begin
  if FParentBiDiMode = AValue then Exit;
  FParentBiDiMode := AValue;
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

procedure TControl.CMChanged(var Message: TLMessage);
begin
  if FParent<>nil then
    FParent.WindowProc(Message);
end;

procedure TControl.CMSysFontChanged(var Message: TLMessage);
begin
  if FDesktopFont then
  begin
    Font := Screen.SystemFont;
    FDesktopFont := True;
  end;
end;

{------------------------------------------------------------------------------
       TControl.CMParentBidiModeChanged

       assumes: FParent <> nil
------------------------------------------------------------------------------}

procedure TControl.CMParentBiDiModeChanged(var Message: TLMessage);
begin
  if csLoading in ComponentState then exit;

  if ParentBidiMode then
  begin
    BidiMode := FParent.BidiMode;
    FParentBiDiMode := True;
  end;
end;

{------------------------------------------------------------------------------
       TControl.IsBiDiModeStored
------------------------------------------------------------------------------}
function TControl.IsBiDiModeStored: boolean;
begin
  Result := not ParentBidiMode;
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
