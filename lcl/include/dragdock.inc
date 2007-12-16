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


{-------------------------------------------------------------------------------
  function HostDockSiteManagerAvailable(HostDockSite: TWinControl): boolean;
-------------------------------------------------------------------------------}
function HostDockSiteManagerAvailable(HostDockSite: TWinControl): boolean;
begin
  Result:=(HostDockSite<>nil) and HostDockSite.UseDockManager
          and (HostDockSite.DockManager<>nil);
end;

{ TDragManager }

procedure TDragManager.BeginDrag(AControl: TControl; AImmediate: Boolean; AThreshold: Integer);
begin
  //Nothing, only avoiding abstract errors the dirty way
end;

procedure TDragManager.DragTo(const APosition: TPoint);
begin
  //Nothing, only avoiding abstract errors the dirty way
end;

procedure TDragManager.CancelDrag;
begin
  //Nothing, only avoiding abstract errors the dirty way
end;

procedure TDragManager.DragDone(ADrop: Boolean);
begin
  //Nothing, only avoiding abstract errors the dirty way
end;

function TDragManager.Dragging(AControl: TControl): boolean;
begin
  //Nothing, only avoiding abstract errors the dirty way
  Result := false;
end;

procedure TDragManager.RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
begin
  //Nothing, only avoiding abstract errors the dirty way
end;

//Drag and Drop wrappers

procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
begin
  DragManager.RegisterDockSite(Site, DoRegister);
end;

procedure BeginDrag(AControl: TControl; AImmediate: Boolean; AThreshold: Integer);
begin
  DragManager.BeginDrag(AControl, AImmediate, AThreshold);
end;

function Dragging(AControl: TControl=nil): boolean;
begin
  Result:=DragManager.Dragging(AControl);
end;

function Dragging: boolean;
begin
  Result:=DragManager.Dragging(nil);
end;

procedure DragTo(const APosition: TPoint);
begin
  DragManager.DragTo(APosition);
end;

procedure DragDone(ADrop: Boolean);
begin
  DragManager.DragDone(ADrop);
end;

procedure CancelDrag;
begin
  DragManager.CancelDrag;
end;


type
  TDragManagerDefault = class(TDragManager)
  private
    FDockingPoints: TList;
    FDragControl: TWinControl; //control that started the drag
    FDragObject: TDragObject; // the drag information object
    FActiveDrag: TDragOperation; // current phase of drag operation
    FDragStartPos: TPoint;  // mouse position at start of drag
    FDragThreshold: Integer;// treshold before the drag becomes activated
    FDragObjectAutoFree: Boolean; //Free the dragobject
    FDragImageList: TDragImageList; //Images
  protected
    function SendCmDragMsg(DragMsg: TDragMessage): Boolean;
    function SendDragMessage(MsgTarget: TControl; Msg: TDragMessage; Source: TDragObject; Target: TControl; const Position: TPoint): LRESULT;
  protected
    procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);override;
    procedure BeginDrag(AControl: TControl; AImmediate: Boolean; AThreshold: Integer); override;
    function Dragging(AControl: TControl): boolean; override;
    procedure DragTo(const APosition: TPoint); override;
    procedure DragDone(Drop: Boolean); override;
    procedure CancelDrag; override;
  public
    destructor Destroy; override;
  end;

{ TDragManagerDefault }

destructor TDragManagerDefault.Destroy;
begin
  FDockingPoints.Free;
  inherited Destroy;
end;

function TDragManagerDefault.Dragging(AControl: TControl): boolean;
//No need to let a TControl know the contents of dragmanager
begin
  if AControl = nil
  then Result := FActiveDrag <> dopNone
  else Result := FDragControl=AControl;
end;

function TDragManagerDefault.SendCmDragMsg(DragMsg: TDragMessage): Boolean;
//Send a CM_DRAG message to the window..
begin
  if FDragObject.DragTarget <> nil
  then Result := boolean(SendDragMessage(FDragObject.DragTarget, DragMsg, FDragObject, FDragObject.DragTarget, FDragObject.DragPos))
  else Result := false;
end;


procedure TDragManagerDefault.BeginDrag(AControl: TControl; AImmediate: Boolean; AThreshold: Integer);
//Start a drag operation, if not already running
var ADragObject: TDragObject;
begin
  if FDragControl = nil then begin
    ADragObject:=nil;
    GetCursorPos(FDragStartPos);
    FDragObjectAutoFree := false;
    try
      if AImmediate
      then SetCaptureControl(nil);

      //Use default value when Threshold < 0
      if AThreshold >= 0
      then FDragThreshold := AThreshold
      else FDragThreshold := Mouse.DragThreshold; //Mayby move it to DragManager? (same for DragImmediate)

      //From DragInitControl(AControl,AImmediate,AThreshold);  ======================
      if AControl is TWinControl
      then FDragControl := TWinControl(AControl)
      else raise Exception.CreateFmt('Dragged control %s is not a TWinControl',[AControl.Name]);

      if AControl.DragKind = dkDrag then begin
        AControl.DoStartDrag(ADragObject);
        if ADragObject = nil then begin
          ADragObject := TDragControlObject.Create(AControl);
          FDragObjectAutoFree := true;
        end
      end else begin
        AControl.DoStartDock(ADragObject);
        if ADragObject = nil then begin
          ADragObject := TDragDockObject.Create(AControl);
          FDragObjectAutoFree := true;
        end;

        with TDragDockObject(ADragObject)do begin
          if AControl is TWinControl
          then GetWindowRect(TWinControl(AControl).Handle, DockRect)
          else begin
            if(AControl.Parent = nil)and not(AControl is TWinControl)
            then GetCursorPos(DockRect.TopLeft)
            else DockRect.TopLeft := AControl.ClientToScreen(Point(0, 0));
            DockRect.BottomRight := Point(DockRect.Left + AControl.Width,DockRect.Top + AControl.Height);
          end;
          EraseDockRect := DockRect;
        end;
      end;


      //From DragInit(ADragObject, AImmediate, AThreshold); ======================
      FDragObject := ADragObject;
      FDragObject.DragTarget := nil;
      FDragObject.DragPos := FDragStartPos;
      SetCaptureControl(AControl);
      FDragObject.Capture;
      if FDragObject is TDragDockObject then begin
        with TDragDockObject(FDragObject), DockRect do begin
          if AImmediate then begin
            FActiveDrag := dopDock;
            DrawDragDockImage;
          end
          else FActiveDrag := dopNone;
        end;
      end else begin
        if AImmediate
        then FActiveDrag := dopDrag
        else FActiveDrag := dopNone;
      end;

      FDragImageList := FDragObject.GetDragImages; //crash?
      if FDragImageList <> nil
      then with FDragStartPos do FDragImageList.BeginDrag(0, X, Y);

      if FActiveDrag <> dopNone
      then DragTo(FDragStartPos);
    except
      FDragControl := nil;
      raise;
    end;
  end;
end;

procedure TDragManagerDefault.DragTo(const APosition: TPoint);

  function GetDropControl: TControl;
  //Select a control where the dragged control will be docked
  var ADragTarget: TWinControl;
    AControl: TControl;
    i: integer;
  begin
    Result := nil;
    ADragTarget := TWinControl(FDragObject.DragTarget);
    if ADragTarget <> nil then begin
      if ADragTarget.UseDockManager then begin
        if ADragTarget.DockClientCount > 0 then begin
          //Did the user drop it on the same positon?      ?
          AControl := ADragTarget.DockClients[0];
          if (ADragTarget.DockClientCount = 1)and (AControl = FDragControl)
          then Exit;

          AControl := FindDragTarget(FDragObject.DragPos, false);
          while(AControl <> nil)and(AControl <> ADragTarget)do begin
            for i:=0 to ADragTarget.DockClientCount-1 do begin
              if ADragTarget.DockClients[i]=AControl then begin
                Result := ADragTarget.DockClients[i];
                Exit;
              end;
            end;
            AControl := AControl.Parent;
          end;
        end;
      end;
    end;
  end;


  function IsControlChildOfClient(AControl:TWinControl): Boolean;
  begin
    Result := False;
    while Assigned(AControl) do begin
      if AControl=FDragControl then begin
        Result:=true;
        exit;
      end;
      AControl:=AControl.Parent;
    end;
  end;

  function FindDockSiteAtPosition: TWinControl;
  var
    AControl: TWinControl;
    CanDock: Boolean;
    QualifyingSites:TList;
    ARect: TRect;
    I: Integer;
  begin
    Result:=nil;
    if FDragControl=nil then exit;
    if FDockingPoints=nil then exit;

    QualifyingSites:=TList.Create;
    try
      for i:=0 to FDockingPoints.Count - 1 do begin
        AControl:=TWinControl(FDockingPoints[i]);
        //Sanity checks..
        if AControl=FDragControl then continue;
        if not AControl.Showing then continue;
        if not AControl.Enabled then continue;
        if not AControl.Visible then continue;
        if IsControlChildOfClient(AControl) then continue;

        if (FDragControl.HostDockSite <> AControl) or (AControl.VisibleDockClientCount > 1) then begin
          CanDock := True;
          AControl.GetSiteInfo(FDragControl, ARect, APosition, CanDock);
          if CanDock and PtInRect(ARect, APosition)
          then QualifyingSites.Add(AControl);
        end;
      end;

      if QualifyingSites.Count > 0 then begin
        Result := TWinControl(QualifyingSites[0]); //.GetTopSite; TODO!!!
        //if not ValidDockTarget(Result) TODO!!!
        //then Result := nil;            TODO!!!
      end;
    finally
      QualifyingSites.Free;
    end;
  end;

var ATarget: TControl;
  DragCursor: TCursor;
  AErasePrevious: boolean;
begin
  if(FActiveDrag <> dopNone)or(Abs(FDragStartPos.X - APosition.X) >= FDragThreshold)or(Abs(FDragStartPos.Y - APosition.Y) >= FDragThreshold) then begin

    if FDragControl.DragKind=dkDock then begin
      ATarget:=FindDockSiteAtPosition;
    end else begin
      ATarget := FindControlAtPosition(APosition,false);
      ATarget := TControl(SendDragMessage(ATarget,dmFindTarget,FDragObject,nil,APosition));
    end;

    if(FActiveDrag = dopNone)and(FDragImageList <> nil)
    then with FDragStartPos do FDragImageList.BeginDrag(0, X, Y);
    if FDragControl.DragKind = dkDrag then begin
      FActiveDrag := dopDrag;
      AErasePrevious := false;
    end else begin
      AErasePrevious := FActiveDrag <> dopNone;
      FActiveDrag := dopDock;
    end;

    //Inform user of entering and leaving
    if ATarget <> FDragObject.DragTarget then begin
      SendCmDragMsg(dmDragLeave);
      FDragObject.DragTarget := TWinControl(ATarget);
      if ATarget is TWinControl
      then FDragObject.DragHandle := TWinControl(ATarget).Handle
      else if (ATarget<>nil) and (ATarget.Parent<>nil)
      then FDragObject.DragHandle := ATarget.Parent.Handle;
      FDragObject.DragPos := APosition;
      SendCmDragMsg(dmDragEnter);
    end;

    FDragObject.DragPos := APosition;
    if FDragObject.DragTarget <> nil
    then FDragObject.DragTargetPos := TControl(FDragObject.DragTarget).ScreenToClient(APosition);
    DragCursor := FDragObject.GetDragCursor(SendCmDragMsg(dmDragMove),APosition.X, APosition.Y);
    if FDragImageList <> nil then begin
      if(ATarget = nil)or(csDisplayDragImage in ATarget.ControlStyle) then begin
        FDragImageList.DragCursor := DragCursor;
        if not FDragImageList.Dragging
        then FDragImageList.BeginDrag(0, APosition.X, APosition.Y)
        else FDragImageList.DragMove(APosition.X, APosition.Y);
      end
      else FDragImageList.EndDrag;
    end;
    WidgetSet.SetCursor(Screen.Cursors[DragCursor]);


    //Draw borders for the docking section or the boundaries of the dragged form
    if FActiveDrag = dopDock then begin
      with TDragDockObject(FDragObject)do begin
        if DragTarget = nil
        then FDragControl.DockTrackNoTarget(TDragDockObject(FDragObject), APosition.X, APosition.Y)
        else begin
          DropOnControl := GetDropControl;
          if DropOnControl = nil
          then with FDragObject do DropAlign := DragTarget.GetDockEdge(DragTargetPos)
          else DropAlign := DropOnControl.GetDockEdge(DropOnControl.ScreenToClient(APosition));
        end;

        //Draw the form outlines when the position has changed
        if not CompareMem(@DockRect, @EraseDockRect, SizeOf(TRect)) then begin
          if AErasePrevious
          then EraseDragDockImage;
          DrawDragDockImage;
          EraseDockRect := DockRect;
        end;
      end;
    end;
  end;
end;

function TDragManagerDefault.SendDragMessage(MsgTarget: TControl; Msg: TDragMessage; Source: TDragObject; Target: TControl; const Position: TPoint): LRESULT;
var DragRec: TDragRec;
  DragMsg: TCMDrag;
begin
  Result := 0;
  if MsgTarget = nil then exit;

  DragRec.Pos := Position;
  DragRec.Target := Target;
  DragRec.Source := Source;
  DragRec.Docking := FActiveDrag = dopDock;

  FillChar(DragMsg,SizeOf(DragMsg),0);
  DragMsg.Msg:=CM_DRAG;
  DragMsg.DragMessage:=Msg;
  DragMsg.DragRec:=@DragRec;
  DragMsg.Result:=0;

  MsgTarget.Dispatch(DragMsg);
  Result:=DragMsg.Result;
end;

procedure TDragManagerDefault.CancelDrag;
begin
  DragDone(false);
  FDragControl := nil;
end;

procedure TDragManagerDefault.DragDone(Drop : Boolean);
var DockObject:TDragDockObject;
  ADragObjectCopy:TDragObject;
  ParentForm: TCustomForm;
  DragMsg: TDragMEssage;
  Accepted: Boolean;
  TargetPos: TPoint;
begin
  {$IFDEF VerboseDrag}
  DebugLn('DragDone Drop=',dbgs(Drop));
  {$ENDIF}
  Accepted:=false;
  DockObject := nil;
  if FDragObject = nil
  then Exit;
  if FDragObject.Cancelling
  then Exit;

  ADragObjectCopy:=FDragObject;
  try
    FDragObject.Dropped := Drop;
    FDragObject.Cancelling := true;
    FDragObject.ReleaseCapture;
    SetCaptureControl(nil);

    Accepted := FDragObject.DragTarget <> nil;
    if FActiveDrag = dopDock then begin
      DockObject := FDragObject as TDragDockObject;
      DockObject.EraseDragDockImage;
      DockObject.Floating := DockObject.DragTarget = nil;
      if Drop then begin
        if FDragControl.HostDockSite <> nil
        then Accepted := FDragControl.HostDockSite.DoUnDock(TWinControl(FDragObject.DragTarget), FDragControl)
        else if DockObject.DragTarget = nil
        then Accepted := true
        else if FDragControl.HostDockSite = nil
        then Accepted := true;
      end;
    end;

    if (FDragObject.DragTarget <> nil) and (FDragObject.DragTarget is TControl)
    then TargetPos := FDragObject.DragTargetPos //controls can override the position
    else TargetPos := FDragObject.DragPos; //otherwise take the current position
    Accepted := Accepted and (((FActiveDrag = dopDock) and DockObject.Floating) or
        ((FActiveDrag <> dopNone) and SendCmDragMsg(dmDragLeave))) and Drop;

    if FActiveDrag = dopDock then begin
      if Accepted and DockObject.Floating then begin
        ParentForm := GetParentForm(DockObject.Control);
        if (ParentForm <> nil) and (ParentForm.ActiveControl = DockObject.Control)
        then ParentForm.ActiveControl := nil;
        FDragControl.Perform(CM_FLOAT, 0, Integer(FDragObject));
      end;
    end else begin
      if FDragImageList <> nil
      then FDragImageList.EndDrag;
      WidgetSet.SetCursor(Screen.Cursors[Screen.Cursor]);
    end;

    FDragObject:=nil;

    //drop
    if ADragObjectCopy.DragTarget <> nil then begin
      DragMsg := dmDragDrop;
      if not Accepted then begin
        TargetPos.X := 0;
        TargetPos.Y := 0;
        DragMsg := dmDragCancel;
        ADragObjectCopy.DragPos.X := 0;
        ADragObjectCopy.DragPos.Y := 0;
      end;
      SendDragMessage(ADragObjectCopy.DragTarget, DragMsg,ADragObjectCopy,ADragObjectCopy.DragTarget, ADragObjectCopy.DragPos);
    end;

    //release the OldDragObject
    ADragObjectCopy.Finished(TObject(ADragObjectCopy.DragTarget),TargetPos.X,TargetPos.Y,Accepted);
  finally
    //erase global variables (dragging stopped)
    if FDragObjectAutoFree
    then ADragObjectCopy.Free
    else ADragObjectCopy.Cancelling := false;
    FDragObject:=nil;
    FDragThreshold:=0;
    FDragControl := nil;
    FDragImageList := nil;
    FActiveDrag := dopNone;
  end;
end;

{-------------------------------------------------------------------------------
  procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
-------------------------------------------------------------------------------}
procedure TDragManagerDefault.RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
begin
  if (Site <> nil) then begin
    if FDockingPoints = nil
    then FDockingPoints := TList.Create;
    if DoRegister
    then FDockingPoints.Add(Site)
    else FDockingPoints.Remove(Site)
  end;
end;

// included by controls.pp
