{%MainUnit ../controls.pp}

{******************************************************************************
                                  TDragManagerDefault
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  TDragManagerDefault = class;

  { TDragDockCommon }

  TDragDockCommon = class
  private
    FManager: TDragManagerDefault;
    FDragImageList: TDragImageList;
    function SendCmDragMsg(ADragObject: TDragObject; ADragMsg: TDragMessage): Boolean;
    function SendDragMessage(AControl: TControl; Msg: TDragMessage; ADragObject: TDragObject; Target: TControl; const Position: TPoint): LRESULT;
  protected
    property Manager: TDragManagerDefault read FManager;
    function Dragging(AControl: TControl): boolean; virtual;abstract;
    procedure DragStarted(APosition: TPoint); virtual;abstract;
    procedure DragMove(APosition: TPoint); virtual;abstract;
    procedure DragStop(ADropped: Boolean); virtual;abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); virtual; abstract;
  public
    constructor Create(AManager: TDragManagerDefault; AControl: TControl); virtual;
  end;

  { TDragPerformer }

  TDragPerformer = class(TDragDockCommon)
  private
    FDragObject: TDragObject;
    FDragCursor: TCursor;
    FDragCursorApplied: Boolean;
  protected
    function Dragging(AControl: TControl): boolean; override;
    procedure DragStarted(APosition: TPoint); override;
    procedure DragMove(APosition: TPoint); override;
    procedure DragStop(ADropped: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AManager: TDragManagerDefault; AControl: TControl); override;
    destructor Destroy; override;
  end;

  { TDockPerformer }

  TDockPerformer = class(TDragDockCommon)
  private
    FDockObject: TDragDockObject;
  protected
    function Dragging(AControl: TControl): boolean; override;
    procedure DragStarted(APosition: TPoint); override;
    procedure DragMove(APosition: TPoint); override;
    procedure DragStop(ADropped: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AManager: TDragManagerDefault; AControl: TControl); override;
    destructor Destroy; override;
  end;

  { TDragManagerDefault }

  TDragManagerDefault = class(TDragManager)
  private
    FDockSites: TFPList;
    FPerformer: TDragDockCommon;
    FStartPosition: TPoint;//mouse position at start of drag or dock
    FThresholdValue: Integer;//treshold before the drag becomes activated
    FWaitForTreshold: boolean;//are we waiting on the treshold activation?
    FInDragStop: Boolean; // semaphore to prevent second execution of dragStop
  protected
    //Support input capture
    procedure KeyUp(var Key: Word; Shift : TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure CaptureChanged(OldCaptureControl: TControl); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    //Support methods
    function IsDragging: boolean; override;
    function Dragging(AControl: TControl): boolean; override;
    procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);override;

    //The Drag and Drop routines
    procedure DragStart(AControl: TControl; AImmediate: Boolean; AThreshold: Integer; StartFromCurrentMouse:Boolean=False); override;
    procedure DragMove(APosition: TPoint); override;
    procedure DragStop(ADropped: Boolean); override;

    function CanStartDragging(Site: TWinControl;  AThreshold: Integer; X,Y:Integer): boolean; override;

    property StartPosition: TPoint read FStartPosition;
  end;


{ TDragDockCommon }

constructor TDragDockCommon.Create(AManager: TDragManagerDefault; AControl: TControl);
begin
  FManager := AManager;
  FDragImageList := nil;
end;

function TDragDockCommon.SendDragMessage(AControl: TControl; Msg: TDragMessage; ADragObject: TDragObject; Target: TControl; const Position: TPoint): LRESULT;
begin
  Result := 0;
  if AControl = nil then exit;
  Result := AControl.DoDragMsg(Msg, Position, ADragObject, Target,
    ADragObject is TDragDockObject);
end;


function TDragDockCommon.SendCmDragMsg(ADragObject: TDragObject; ADragMsg: TDragMessage): Boolean;
//Send a CM_DRAG message to the window..
begin
  Result := SendDragMessage(ADragObject.DragTarget, ADragMsg,
    ADragObject, ADragObject.DragTarget, ADragObject.DragPos) <> 0;
end;

{ TDragPerformer }

constructor TDragPerformer.Create(AManager: TDragManagerDefault; AControl: TControl);
//Start a drag operation, if not already running
begin
  inherited Create(AManager, AControl);

  AControl.DoStartDrag(FDragObject);
  if FDragObject = nil then
    FDragObject := TDragControlObject.AutoCreate(AControl);
  FDragObject.DragPos := AManager.StartPosition;
  SetCaptureControl(AControl);
end;

destructor TDragPerformer.Destroy;
begin
  FreeAndNil(FDragObject);
  inherited Destroy;
end;

function TDragPerformer.Dragging(AControl: TControl): boolean;
begin
  Result:= Assigned(FDragObject) and (FDragObject.Control=AControl);
end;

procedure TDragPerformer.DragStarted(APosition: TPoint);
//Imput device has moved beyond tresholt limits (or immediate docking)
begin
  FDragCursorApplied := False;
  if FDragObject = nil then Exit;
  FDragImageList := FDragObject.GetDragImages;
  if FDragImageList <> nil then
    FDragImageList.BeginDrag(0, APosition.X, APosition.Y);
end;

procedure TDragPerformer.DragMove(APosition: TPoint);
var 
  ATarget: TControl;
  DragCursor: TCursor;
begin
  if FDragObject = nil then
    Exit;
  //Inform user of entering and leaving the area
  ATarget := FindControlAtPosition(APosition, False);
  ATarget := TControl(SendDragMessage(ATarget, dmFindTarget, FDragObject, nil, APosition));

  if ATarget <> FDragObject.DragTarget then
  begin
    SendCmDragMsg(FDragObject, dmDragLeave);
    FDragObject.DragTarget := TWinControl(ATarget);
    FDragObject.DragPos := APosition;
    SendCmDragMsg(FDragObject, dmDragEnter);
  end
  else
    FDragObject.DragPos := APosition;

  //TODO: Need to rewrite this(or even delete it, back to the roots)
  if FDragObject.DragTarget <> nil then
    FDragObject.DragTargetPos := FDragObject.DragTarget.ScreenToClient(APosition);
  DragCursor := FDragObject.GetDragCursor(SendCmDragMsg(FDragObject, dmDragMove),APosition.X, APosition.Y);
  if FDragImageList <> nil then
  begin
    if (ATarget = nil) or (csDisplayDragImage in ATarget.ControlStyle) or
       FDragObject.AlwaysShowDragImages then
    begin
      FDragImageList.DragCursor := DragCursor;
      if not FDragImageList.Dragging then
        FDragImageList.BeginDrag(0, APosition.X, APosition.Y)
      else
        FDragImageList.DragMove(APosition.X, APosition.Y);
    end
    else
      FDragImageList.EndDrag;
  end
  else begin
    Screen.BeginTempCursor(DragCursor);
    if FDragCursorApplied then
      Screen.EndTempCursor(FDragCursor);
    FDragCursor := DragCursor;
    FDragCursorApplied := True;
  end;
end;

procedure TDragPerformer.DragStop(ADropped : Boolean);
var 
  ADragObjectCopy: TDragObject;
  DragMsg: TDragMessage;
  Accepted: Boolean;
  TargetPos: TPoint;
begin
  ADragObjectCopy := FDragObject;
  if FDragObject <> nil then
  try
    FDragObject := nil;
    SetCaptureControl(nil);

    if FDragImageList <> nil then
      FDragImageList.EndDrag
    else
    if FDragCursorApplied then
      Screen.EndTempCursor(FDragCursor);
    if (ADragObjectCopy.DragTarget <> nil) and (ADragObjectCopy.DragTarget is TControl) then
      TargetPos := ADragObjectCopy.DragTargetPos //controls can override the position
    else
      TargetPos := ADragObjectCopy.DragPos; //otherwise take the current position
    Accepted := ADropped and SendCmDragMsg(ADragObjectCopy, dmDragLeave);
    ADragObjectCopy.FDropped := Accepted;

    if ADragObjectCopy.DragTarget <> nil then
    begin
      if not Accepted then
      begin
        TargetPos.X := 0;
        TargetPos.Y := 0;
        DragMsg := dmDragCancel;
        ADragObjectCopy.DragPos:=Point(0,0);
        ADragObjectCopy.DragTarget := nil;
      end
      else
        DragMsg := dmDragDrop;
      SendDragMessage(ADragObjectCopy.DragTarget, DragMsg, ADragObjectCopy,
        ADragObjectCopy.DragTarget, ADragObjectCopy.DragPos);
    end;

    if not Accepted then
      ADragObjectCopy.Control.DragCanceled;
    ADragObjectCopy.EndDrag(ADragObjectCopy.DragTarget, TargetPos.X, TargetPos.Y);

  finally
    //erase global variables (dragging stopped)
    if ADragObjectCopy.AutoFree then
      ADragObjectCopy.Free;
  end;
end;

procedure TDragPerformer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if Assigned(FDragObject) and (AComponent = FDragObject.DragTarget) then
      DragMove(FDragObject.DragPos);
  end;
end;


{ TDockPerformer }

constructor TDockPerformer.Create(AManager: TDragManagerDefault; AControl: TControl);
//Start a drag operation, if not already running
begin
  inherited Create(AManager, AControl);
  AControl.DoStartDock(TDragObject(FDockObject));
  if FDockObject = nil then
    FDockObject := TDragDockObject.AutoCreate(AControl);

  FDockObject.InitDock(AManager.StartPosition);
  // we are tracking capture change to stop drag/dock is happen
  SetCaptureControl(AControl);
end;

destructor TDockPerformer.Destroy;
begin
  FreeAndNil(FDockObject);
  inherited Destroy;
end;

function TDockPerformer.Dragging(AControl: TControl): boolean;
begin
  Result:= Assigned(FDockObject) and (FDockObject.Control=AControl);
end;

procedure TDockPerformer.DragStarted(APosition: TPoint);
// Input device has moved beyond threshold (or immediate docking)
begin
  if FDockObject = nil then
    Exit;
  FDragImageList := FDockObject.GetDragImages;
  if FDragImageList <> nil then
    FDragImageList.BeginDrag(0, APosition.X, APosition.Y);
  FDockObject.ShowDockImage;
end;

procedure TDockPerformer.DragMove(APosition: TPoint);

  function GetDropControl(ADragTarget: TWinControl): TControl;
  //Select a control where the dragged control will be docked
  var
    AControl: TControl;
    i: integer;
  begin
    Result := nil;
    if ADragTarget <> nil then
    begin
      if ADragTarget.UseDockManager then
      begin
        if ADragTarget.DockClientCount > 0 then
        begin
          //Did the user drop it on the same positon?
          AControl := ADragTarget.DockClients[0];
          if (ADragTarget.DockClientCount = 1) and (AControl = FDockObject.Control) then
            Exit;

          AControl := FindDragTarget(FDockObject.DragPos, false);
          while (AControl <> nil) and (AControl <> ADragTarget) do
          begin
            for i := 0 to ADragTarget.DockClientCount-1 do
            begin
              if ADragTarget.DockClients[i]=AControl then
              begin
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
    while Assigned(AControl) do
    begin
      if AControl=FDockObject.Control then
      begin
        Result := True;
        exit;
      end;
      AControl := AControl.Parent;
    end;
  end;

  function FindDockSiteAtPosition: TWinControl;
  //Replace with lookup in Screen.Zorder(?)
  var
    AControl: TWinControl;
    CanDock: Boolean;
    QualifyingSites: TList;
    ARect: TRect;
    I: Integer;
    j: Integer;

    function HaveMultiplePotentialDockClient(AControl: TWinControl):boolean;
    var
      I,Count:Integer;
    begin
      Count := 0;
      for I := 0 to AControl.ControlCount - 1 do
      begin
        if (AControl.Controls[I].DragKind  = dkDock) and (AControl.Controls[I].DragMode = dmAutomatic) then
          inc(Count);
        if Count > 1 then exit(True);
      end;
      Result:=False;
    end;

    function ItCanBeHostSite:boolean;
    begin
      if FDockObject.Control.HostDockSite <> nil then
        result := (FDockObject.Control.HostDockSite <> AControl) or (AControl.VisibleDockClientCount > 1)
      else
        result := (AControl <> FDockObject.Control.Parent) or HaveMultiplePotentialDockClient(AControl);
    end;

  begin
    Result := nil;
    
    if Manager.FDockSites = nil then
      Exit;
      
    QualifyingSites := TList.Create;
    try
      for i := 0 to Manager.FDockSites.Count - 1 do
      begin
        AControl:=TWinControl(Manager.FDockSites[i]);
        //Sanity checks..
        if not AControl.Showing then continue;
        if not AControl.Enabled then continue;
        if not AControl.IsVisible then continue;
        if AControl=FDockObject.Control then continue;
        if IsControlChildOfClient(AControl) then continue;

        if ItCanBeHostSite then
        begin
          CanDock := True;
          AControl.GetSiteInfo(FDockObject.Control, ARect, APosition, CanDock);
          //debugln(['FindDockSiteAtPosition ',DbgSName(AControl),' CanDock=',CanDock,' PtIn=',PtInRect(ARect, APosition)]);
          if CanDock and PtInRect(ARect, APosition) then
            QualifyingSites.Add(AControl);
        end;
      end;

      if QualifyingSites.Count > 0 then
      begin
        // if a parent and a child has qualified remove the parent
        for i:=QualifyingSites.Count-1 downto 0 do begin
          //debugln(['FindDockSiteAtPosition qualified: ',DbgSName(TWinControl(QualifyingSites[i])),' ',TWinControl(QualifyingSites[i]).Caption]);
          for j:=0 to QualifyingSites.Count-1 do begin
            if TWinControl(QualifyingSites[i]).IsParentOf(TWinControl(QualifyingSites[j]))
            then begin
              //debugln(['FindDockSiteAtPosition isparentof ',DbgSName(TWinControl(QualifyingSites[j])),' ',TWinControl(QualifyingSites[j]).Caption]);
              QualifyingSites.Delete(i);
              break;
            end;
          end;
        end;

        i:=0;
        if QualifyingSites.Count>1 then
        begin
          // there are multiple candidates => use the top level
          for j:=0 to Screen.CustomFormZOrderCount-1 do begin
            i:=QualifyingSites.Count-1;
            while (i>=0)
            and (GetParentForm(TWinControl(QualifyingSites[i]))<>Screen.CustomFormsZOrdered[j])
            do
              dec(i);
            if i>=0 then break;
          end;
          if i<0 then i:=0;
        end;
        Result := TWinControl(QualifyingSites[i]);
      end;
    finally
      QualifyingSites.Free;
    end;
  end;

var
  ATarget: TWinControl;
  DragCursor: TCursor;
begin
  if FDockObject = nil then
    Exit;

  //Inform user of entering and leaving
  if (GetKeyState(VK_CONTROL) and $8000) <> 0 then
    ATarget := nil
  else
    ATarget := FindDockSiteAtPosition;

  if ATarget <> FDockObject.DragTarget then
  begin
    SendCmDragMsg(FDockObject, dmDragLeave); //using the old values in FDockObject
  end;
  FDockObject.DragPos := APosition;

  if ATarget <> nil then
    with FDockObject do
    begin //determine precise target now, before dmDragMove
      DragTargetPos := ATarget.ScreenToClient(APosition);
      DropOnControl := GetDropControl(ATarget);
      if not ATarget.UseDockManager or
         not ATarget.DockManager.GetDockEdge(FDockObject) then
      begin
        if DropOnControl = nil then
          DropAlign := ATarget.GetDockEdge(DragTargetPos)
        else
          DropAlign := DropOnControl.GetDockEdge(DropOnControl.ScreenToClient(APosition));
      end;
    end;

  if ATarget <> FDockObject.DragTarget then
  begin
    FDockObject.DragTarget := ATarget;
    SendCmDragMsg(FDockObject, dmDragEnter);
  end;

  DragCursor := FDockObject.GetDragCursor(SendCmDragMsg(FDockObject, dmDragMove),APosition.X, APosition.Y);
  if FDragImageList <> nil then
  begin
    if (ATarget = nil) or (csDisplayDragImage in ATarget.ControlStyle) or
       FDockObject.AlwaysShowDragImages then
    begin
      FDragImageList.DragCursor := DragCursor;
      if not FDragImageList.Dragging then
        FDragImageList.BeginDrag(0, APosition.X, APosition.Y)
      else
        FDragImageList.DragMove(APosition.X, APosition.Y);
    end
    else
      FDragImageList.EndDrag;
  end;
  WidgetSet.SetCursor(Screen.Cursors[DragCursor]);

  //Draw borders for the docking section or the boundaries of the dragged form
  with FDockObject do
  begin
    if DragTarget = nil then //show as floating
      FDockObject.Control.DockTrackNoTarget(FDockObject, APosition.X, APosition.Y);

    MoveDockImage;
  end;
end;

procedure TDockPerformer.DragStop(ADropped: Boolean);
var
  ADockObjectCopy: TDragDockObject;
  ParentForm: TCustomForm;
  DragMsg: TDragMessage;
  Accepted: Boolean;
  TargetPos: TPoint;
  Moved: Boolean;
  {$IFDEF EnableDockMove}
  AControl: TControl;
  P: TPoint;
  {$ENDIF}
begin
  ADockObjectCopy := FDockObject;
  if FDockObject <> nil then
  try
    FDockObject := nil;
    SetCaptureControl(nil);

    ADockObjectCopy.HideDockImage;
    ADockObjectCopy.Floating := ADockObjectCopy.DragTarget = nil;

    Moved:=false;
    Accepted := ADockObjectCopy.DragTarget <> nil;

    if ADropped then
    begin
      DebugLn(['TDockPerformer.DragStop Dropped ',ADockObjectCopy.Control.Floating,' ',ADockObjectCopy.Floating,' ',DbgSName(ADockObjectCopy.Control)]);
      if ADockObjectCopy.Control.Floating and ADockObjectCopy.Floating then
      begin
        DebugLn(['TDockPerformer.DragStop SIMPLE MOVE']);
        {$IFDEF EnableDockMove}
        // it works on wine, gtk, qt and carbon

        // just move
        AControl:=ADockObjectCopy.Control;
        if AControl.Parent<>nil then
        begin
          P := AControl.Parent.ClientToScreen(Point(AControl.Left, AControl.Top));
          with ADockObjectCopy.DockRect do
          begin
            AControl.Parent.BoundsRect :=
              Bounds(Left + AControl.Parent.Left - P.X,
                     Top + AControl.Parent.Top - P.Y,
                     Right - Left + AControl.Parent.Width - AControl.Width,
                     Bottom - Top + AControl.Parent.Height - AControl.Height);
          end;
        end else begin
          AControl.BoundsRect:=ADockObjectCopy.DockRect;
        end;
        //DebugLn(['TDockPerformer.DragStop MOVED']);
        Moved:=true;
        Accepted:=true;
        {$ENDIF}
      end;

      // undock
      if not Moved then begin
        if ADockObjectCopy.Control.HostDockSite <> nil then
          Accepted := ADockObjectCopy.Control.HostDockSite.DoUnDock(
               TWinControl(ADockObjectCopy.DragTarget), ADockObjectCopy.Control)
        else
        if ADockObjectCopy.DragTarget = nil then
          Accepted := True
        else
        if ADockObjectCopy.Control.HostDockSite = nil then
          Accepted := True;
      end;
    end;

    if (ADockObjectCopy.DragTarget <> nil) and
       (ADockObjectCopy.DragTarget is TControl) then
      TargetPos := ADockObjectCopy.DragTargetPos //controls can override the position
    else
      TargetPos := ADockObjectCopy.DragPos; //otherwise take the current position
    Accepted := Accepted and (Moved or ADockObjectCopy.Floating or SendCmDragMsg(ADockObjectCopy, dmDragLeave)) and ADropped;
    ADockObjectCopy.FDropped := Accepted;

    // float
    if Accepted and (not Moved) and ADockObjectCopy.Floating then
    begin
      ParentForm := GetParentForm(ADockObjectCopy.Control);
      if (ParentForm <> nil) and
         (ParentForm.ActiveControl = ADockObjectCopy.Control) then
        ParentForm.ActiveControl := nil;
      ADockObjectCopy.Control.DoFloatMsg(ADockObjectCopy);
    end;

    // dock
    if ADockObjectCopy.DragTarget <> nil then
    begin
      if not Accepted then
      begin
        TargetPos.X := 0;
        TargetPos.Y := 0;
        DragMsg := dmDragCancel;
        ADockObjectCopy.DragPos:=Point(0,0);
        ADockObjectCopy.DragTarget:=nil;
      end
      else
        DragMsg := dmDragDrop;
      ADockObjectCopy.Control.Top:=0;
      ADockObjectCopy.Control.Left:=0;
      SendDragMessage(ADockObjectCopy.DragTarget, DragMsg, ADockObjectCopy,
                      ADockObjectCopy.DragTarget, ADockObjectCopy.DragPos);
    end;

    // EndDrag
    if not Accepted then
      ADockObjectCopy.Control.DragCanceled;
    ADockObjectCopy.EndDrag(ADockObjectCopy.DragTarget,TargetPos.X,TargetPos.Y);
  finally
    if ADockObjectCopy.AutoFree then
      ADockObjectCopy.Free
  end;
end;

procedure TDockPerformer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin

end;


{ TDragManagerDefault }

destructor TDragManagerDefault.Destroy;
begin
  FreeAndNil(FDockSites);
  inherited Destroy;
end;

function TDragManagerDefault.IsDragging: boolean;
//Is something being dragged
begin
  Result := Assigned(FPerformer) and not FInDragStop;
end;

function TDragManagerDefault.Dragging(AControl: TControl): boolean;
//Is the control being dragged
begin
  if FPerformer = nil then
    Result := false
  else
    Result := FPerformer.Dragging(AControl)
end;

procedure TDragManagerDefault.DragStart(AControl: TControl; AImmediate: Boolean; AThreshold: Integer; StartFromCurrentMouse:Boolean=False);
//Start a drag operation
begin
  if FPerformer = nil then
  begin
    if AThreshold >= 0 then
      FThresholdValue := AThreshold
    else
      FThresholdValue := DragThreshold;
    FWaitForTreshold := not AImmediate;
    if StartFromCurrentMouse then
      FStartPosition:=AControl.ClientToScreen(point(0,0))
    else
      GetCursorPos(FStartPosition);

    AControl.BeforeDragStart;

    case AControl.DragKind of
      dkDrag: FPerformer := TDragPerformer.Create(Self, AControl);
      dkDock: FPerformer := TDockPerformer.Create(Self, AControl);
    end;

    if AImmediate then
    begin
      FPerformer.DragStarted(FStartPosition);
      DragMove(FStartPosition);
    end;
  end;
end;

procedure TDragManagerDefault.DragMove(APosition: TPoint);
//The captured input device has moved
begin
  if FPerformer <> nil then
  begin
    //delay until the input device has moved at least x pixels
    if FWaitForTreshold then
    begin
      if Abs(FStartPosition.X - APosition.X) >= FThresholdValue then
        FWaitForTreshold := false
      else
      if Abs(FStartPosition.Y - APosition.Y) >= FThresholdValue then
        FWaitForTreshold := false
      else
        exit;
      FPerformer.DragStarted(APosition);
    end;
    if FPerformer<>nil then
      FPerformer.DragMove(APosition);
  end;
end;

procedure TDragManagerDefault.DragStop(ADropped : Boolean);
//End the drag operation
begin
  if Assigned(FPerformer) and not FInDragStop then
  begin
    FInDragStop := True;
    try
      FPerformer.DragStop(ADropped and not FWaitForTreshold);
    finally
      FreeAndNil(FPerformer);
      FInDragStop := False;
    end;
  end;
end;

procedure TDragManagerDefault.RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
begin
  if Assigned(Site) then
  begin
    if FDockSites = nil then
      FDockSites := TFPList.Create;
    if (FDockSites.IndexOf(Site)>=0)=DoRegister then exit;

    //debugln(['TDragManagerDefault.RegisterDockSite Changed ',DbgSName(Site),' DoRegister=',DoRegister]);
    if DoRegister then
    begin
      FDockSites.Add(Site);
      Site.FreeNotification(Self);
    end else
    begin
      FDockSites.Remove(Site);
      Site.RemoveFreeNotification(Self);
    end;
  end;
end;

procedure TDragManagerDefault.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_CONTROL then
    DragMove(Mouse.CursorPos);
end;

procedure TDragManagerDefault.KeyUp(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_CONTROL:
      DragMove(Mouse.CursorPos);
    VK_ESCAPE:
    begin
      DragStop(False);
      Key := 0;
    end;
  end;
end;

procedure TDragManagerDefault.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //In TControl.WndProc/LM_LBUTTONDOWN a drag session is started but a few lines
  //later the LM_LBUTTONDOWN is send to the dragmanager which would end dragging.
  //Dragging is only ended by a MouseUp so we can ignore this mesage anywhay.
  //DragStop(true);
end;

procedure TDragManagerDefault.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // clear performer references
  if Assigned(FPerformer) then
    FPerformer.Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    // remove from dock sites
    if Assigned(FDockSites) then
    begin
      //if FDockSites.IndexOf(AComponent)>=0 then debugln(['TDragManagerDefault.Notification DockSite: ',DbgSName(AComponent)]);
      FDockSites.Remove(AComponent);
      if FDockSites.Count = 0 then
        FreeAndNil(FDockSites);
    end;
    if (AComponent is TControl) and Dragging(TControl(AComponent)) then
      DragStop(False);
  end;
end;

constructor TDragManagerDefault.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FInDragStop := False;
end;

procedure TDragManagerDefault.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;
  DragMove(P);
end;

procedure TDragManagerDefault.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DragStop(not FWaitForTreshold);
end;

procedure TDragManagerDefault.CaptureChanged(OldCaptureControl: TControl);
var
  i: integer;
  AIsDragging,
  AIsDocking: Boolean;
begin
  // if this is TWinControl, and it have controls (not TWinControls)
  // then we should check Dragging in those controls
  AIsDocking := False;
  AIsDragging := OldCaptureControl.Dragging;
  if AIsDragging then
    AIsDocking := OldCaptureControl.DragKind = dkDock;
  if (not AIsDragging) and (OldCaptureControl is TWinControl) then
  begin
    for i := 0 to TWinControl(OldCaptureControl).ControlCount - 1 do
    begin
      AIsDragging := AIsDragging or TWinControl(OldCaptureControl).Controls[i].Dragging;
      if AIsDragging then
      begin
        AIsDocking := TWinControl(OldCaptureControl).Controls[i].DragKind = dkDock;
        break;
      end;
    end;
  end;
  // when we are start drag/dock in TDockHeader then we should
  // take into account that it doesnot belong to our control - it
  // belongs to parent => we should allow parent to get capture
  if not (AIsDocking and (CaptureControl = OldCaptureControl.Parent)) then
    DragStop(AIsDragging);
end;

function TDragManagerDefault.CanStartDragging(Site: TWinControl;  AThreshold: Integer; X,Y:Integer): boolean;
var
  Threshold:integer;
  aRect:TRect;
begin
  if AThreshold<=0 then
    Threshold:=DragThreshold
  else
    Threshold:=AThreshold;
  aRect := Site.ClientRect;
  InflateRect(aRect, Threshold, Threshold);
  Result := not PtInRect(aRect, Point(X, Y));
end;

//included by controls.pp
