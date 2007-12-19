{%MainUnit ../controls.pp}

{*****************************************************************************
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

var
  DragControl: TControl=nil; // control, that started the drag
  DragObject: TDragObject; // the drag information object
  DragObjectAutoFree: Boolean; // True, if DragObject was auto created
  DragStartPos: TPoint;  // mouse position at start of drag
  ActiveDrag: TDragOperation;// current phase of drag operation
  DragThreshold: Integer;// treshold before the drag becomes activated
  DragImages: TDragImageList; // DragObject.GetDragImages

procedure DragTo(const Position: TPoint); forward;

{-------------------------------------------------------------------------------
  function HostDockSiteManagerAvailable(HostDockSite: TWinControl): boolean;
-------------------------------------------------------------------------------}
function HostDockSiteManagerAvailable(HostDockSite: TWinControl): boolean;
begin
  Result:=(HostDockSite<>nil) and HostDockSite.UseDockManager
          and (HostDockSite.DockManager<>nil);
end;

{-------------------------------------------------------------------------------
  procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
-------------------------------------------------------------------------------}
procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
begin
  if (Site <> nil) then begin
    if DockSiteHash = nil then DockSiteHash := TDynHashArray.Create;
    if DoRegister then begin
      if not DockSiteHash.Contains(Site) then
        DockSiteHash.Add(Site);
    end else begin
      if DockSiteHash.Contains(Site) then
        DockSiteHash.Remove(Site);
    end;
  end;
end;

{-------------------------------------------------------------------------------
  Function SendDragMessage(MsgTarget: TControl; Msg: TDragMessage;
    Source: TDragObject; Target: TControl; const Pos: TPoint): longint;

  Send a CM_DRAG (TCMDrag) message to MsgTarget.
-------------------------------------------------------------------------------}
Function SendDragMessage(MsgTarget: TControl; Msg: TDragMessage;
  Source: TDragObject; Target: TControl; const Position: TPoint): LRESULT;
var
  DragRec: TDragRec;
  DragMsg: TCMDrag;
Begin
  Result := 0;
  if MsgTarget = nil then exit;

  DragRec.Pos := Position;
  DragRec.Target := Target;
  DragRec.Source := Source;
  DragRec.Docking := False;//TODO: not supported at this point

  FillChar(DragMsg,SizeOf(DragMsg),0);
  DragMsg.Msg:=CM_DRAG;
  DragMsg.DragMessage:=Msg;
  DragMsg.DragRec:=@DragRec;
  DragMsg.Result:=0;

  MsgTarget.Dispatch(DragMsg);
  Result:=DragMsg.Result;
end;

{-------------------------------------------------------------------------------
  function SendDragOver(DragMsg: TDragMessage): Boolean;

  Send a DragOver message to DragObject.DragTarget.
-------------------------------------------------------------------------------}
function SendDragOver(DragMsg: TDragMessage): Boolean;
begin
  Result := False;
  if (DragObject.DragTarget = nil) then exit;
  if not (DragObject.DragTarget is TControl) then begin
    RaiseGDBException('invalid DragTarget');
  end;
  Result := LongBool(SendDragMessage(DragObject.DragTarget, DragMsg,
    DragObject, DragObject.DragTarget, DragObject.DragPos));
end;

{-------------------------------------------------------------------------------
  procedure CancelDrag;
  
  Aborts dragging.
-------------------------------------------------------------------------------}
procedure CancelDrag;
begin
  DragDone(False);
  DragControl := nil;
end;

{-------------------------------------------------------------------------------
  procedure ClearDragObject;

  Set the global variable DragObject to nil.
  If DragObjectAutoFree is set, then the DragObject was auto created by the LCL
  and is freed here.
-------------------------------------------------------------------------------}
procedure ClearDragObject;
begin
  if DragObjectAutoFree then begin
    DragObjectAutoFree:=false;
    FreeThenNil(DragObject);
  end else
    DragObject := nil;
end;

{-------------------------------------------------------------------------------
  Procedure DragInitControl(Control : TControl; Immediate : Boolean;
                            Threshold: Integer);

  Initializes the dragging. If Immediate=True it starts the dragging, otherwise
  it will be started when the user moves the mouse more than DragThreshold
  pixel.
-------------------------------------------------------------------------------}
procedure DragInitControl(Control: TControl; Immediate: Boolean;
  Threshold: Integer);
var
  ok: boolean;
begin
  {$IFDEF VerboseDrag}
  DebugLn('DragInitControl ',Control.Name,':',Control.ClassName,' Immediate=', dbgs(Immediate));
  {$ENDIF}
  ClearDragObject;
  DragControl := Control;
  ok:=false;
  try
    if Control.fDragKind = dkDrag then begin
      // initialize the DragControl. Note: This can change the DragControl
      Control.DoStartDrag(DragObject);
      // check if initialization was successful
      if DragControl = nil then Exit;
      // initialize DragObject, if not already done
      if DragObject = nil then Begin
        DragObject := TDragControlObject.Create(Control);
        DragObjectAutoFree := True;
      End;
    end else if Control.fDragKind = dkDock then begin
      // ToDo: docking
      RaiseGDBException('not yet implemented');
    end;

    // init the global drag variables
    DragObject.DragTarget := nil;
    GetCursorPos(DragStartPos);
    DragObject.DragPos := DragStartPos;
    DragImages := DragObject.GetDragImages;

    //DragCapture := DragObject.Capture;
    DragThreshold := Threshold;

    if DragObject is TDragDockObject then begin
      with TDragDockObject(DragObject), FDockRect do
      begin
        if Right > Left then
          FMouseDeltaX :=  (DragPos.x - Left) / (Right - Left)
        else
          FMouseDeltaX := 0;

        if Bottom > Top then
          FMouseDeltaY :=  (DragPos.y - Top) / (Bottom - Top)
        else
          FMouseDeltaY := 0;

        if Immediate then
        begin
          ActiveDrag := dopDock;
          //DrawDragDockImage;
        end
        else
          ActiveDrag := dopNone;
      end;
    end else begin
      if Immediate then
        ActiveDrag := dopDrag
      else
        ActiveDrag := dopNone;
    end;

    if ActiveDrag <> dopNone then DragTo(DragStartPos);

    ok:=true;
  finally
    if not ok then begin
      DragControl := nil;
      ClearDragObject;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  function GetDragTargetAt(const Position: TPoint; DragKind: TDragKind;
    Client: TControl): Pointer;

  Search a control at position and ask for a dragging/docking target.
  Client is the Source control.
-------------------------------------------------------------------------------}
function GetDragTargetAt(const Position: TPoint; DragKind: TDragKind;
  Client: TControl): TControl;
begin
  Result:=nil;
  if DragKind = dkDrag then
  begin
    Result:=FindControlAtPosition(Position,false);
    Result := TControl(SendDragMessage(Result,dmFindTarget,DragObject,nil,
                       Position));
    if (Result<>nil) and (not (Result is TControl)) then
      RaiseGDBException('invalid DragTarget');
  end
  else begin
    // ToDo: docking
    RaiseGDBException('not implemented yet');
  end;
end;

{-------------------------------------------------------------------------------
  Procedure DragTo(const Position: TPoint);


-------------------------------------------------------------------------------}
procedure DragTo(const Position: TPoint);
var
  TargetControl: TControl;
  ADragCursor: TCursor;
  AAccepted: Boolean;
begin
  {$IFDEF VerboseDrag}
  DebugLn('DragTo P=', IntToStr(Position.X), ',', IntToStr(Position.Y));
  {$ENDIF}
  if (DragControl = nil) or ((ActiveDrag = dopNone) and
     (Abs(DragStartPos.X - Position.X) < DragThreshold) and
     (Abs(DragStartPos.Y - Position.Y) < DragThreshold)) then
  begin
    // dragging not yet started
    // or CancelDrag happened (DragControl = nil)
    exit;
  end;

  TargetControl := GetDragTargetAt(Position, DragControl.DragKind, DragControl);

  if DragControl.DragKind = dkDrag then
    ActiveDrag := dopDrag
  else
    ActiveDrag := dopDock;

  if TargetControl <> DragObject.DragTarget then
  begin
    // Target changed => send dmDragLeave to old target and dmDragEnter to new
    SendDragOver(dmDragLeave);
    if DragObject = nil then Exit;
    DragObject.DragTarget := TargetControl;
    if TargetControl is TWinControl then
      DragObject.DragHandle := TWinControl(TargetControl).Handle
    else if (TargetControl<>nil) and (TargetControl.Parent<>nil) then
      DragObject.DragHandle := TargetControl.Parent.Handle;
    DragObject.DragPos := Position;
    SendDragOver(dmDragEnter);
    if DragObject = nil then Exit;
  end else
  begin
    // same target => send dmDragMove
    DragObject.DragPos := Position;
    AAccepted := SendDragOver(dmDragMove);
    // on DragMove someone can call CancelDrag and this cause crash w/o this check
    if DragObject = nil then Exit;
    ADragCursor := DragObject.GetDragCursor(AAccepted, Position.X, Position.Y);
    if DragImages <> nil then
    begin
      if (DragObject.DragTarget = nil) or (csDisplayDragImage in DragObject.DragTarget.ControlStyle) or
         (DragObject.AlwaysShowDragImages) then
      begin
        DragImages.DragLock(0, Position.X, Position.Y);
        DragImages.DragCursor := ADragCursor;
        DragImages.DragMove(Position.X, Position.Y);
      end else
      begin
        // we can hide drag image
        DragImages.DragUnLock;
      end;
    end;
    WidgetSet.SetCursor(Screen.Cursors[ADragCursor]);
    if DragObject = nil then Exit;
  end;

  // update Position
  DragObject.DragPos := Position;
  if DragObject.DragTarget <> nil then
    DragObject.DragTargetPos := DragObject.DragTarget.ScreenToClient(Position);

  // ToDo: docking
end;

{-------------------------------------------------------------------------------
  Procedure DragDone(Drop : Boolean);

  Ends the current dragging operation.
  Invokes DragMessage,
  Frees the DragObject if autocreated by the LCL,
  Finish: DragSave.Finished
-------------------------------------------------------------------------------}
procedure DragDone(Drop : Boolean);
var
  Accepted: Boolean;
  OldDragObject: TDragObject;
  OldDragAutoFree: Boolean;
  DragMsg: TDragMEssage;
  TargetPos: TPoint;
Begin
  {$IFDEF VerboseDrag}
  DebugLn('DragDone Drop=',dbgs(Drop));
  {$ENDIF}
  Accepted:=false;
  if (DragObject = nil) or DragObject.Cancelling then Exit;

  // take over the DragObject
  // (to prevent auto destruction during the next operations)
  OldDragObject := DragObject;
  OldDragAutoFree:=DragObjectAutoFree;
  DragObjectAutoFree:=false;
  try
    // mark DragObject for end phase of drag
    DragObject.Cancelling := True;
    DragObject.FDropped := Drop;
    ReleaseCapture;

    if ActiveDrag = dopDock then
    begin
      RaiseGDBException('not implemented yet');
    end;

    if (DragObject.DragTarget <> nil)
    and (TObject(DragObject.DragTarget) is TControl) then
      // controls can override the target position
      TargetPos := DragObject.DragTargetPos
    else
      // otherwise just take the current drag position
      TargetPos := DragObject.DragPos;

    // last DragOver message (make sure, there is at least one)
    Accepted:=(ActiveDrag <> dopNone) and SendDragOver(dmDragLeave) and Drop;

    if DragImages <> nil then
      DragImages.EndDrag;
    WidgetSet.SetCursor(Screen.Cursors[Screen.Cursor]);
    // erase global variables (dragging stopped)
    DragControl := nil;
    DragObject := nil;
    DragImages := nil;

    // drop
    if (OldDragObject<>nil) and (OldDragObject.DragTarget <> nil) then
    begin
      DragMsg := dmDragDrop;
      if not Accepted then begin
        DragMsg := dmDragCancel;
        OldDragObject.FDragPos.X := 0;
        OldDragObject.FDragPos.Y := 0;
        TargetPos.X := 0;
        TargetPos.Y := 0;
      end;
      SendDragMessage(OldDragObject.DragTarget, DragMsg,
        OldDragObject, OldDragObject.DragTarget, OldDragObject.DragPos);
    end;
    
    // release the OldDragObject
    OldDragObject.Cancelling := False;
    OldDragObject.Finished(TObject(OldDragObject.DragTarget),
                           TargetPos.X,TargetPos.Y,Accepted);
  finally
    DragControl := nil;
    if OldDragAutoFree then
      OldDragObject.Free;
    DragObject:=nil;
  end;
end;

// included by controls.pp
