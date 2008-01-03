{%MainUnit ../controls.pp}

{******************************************************************************
                                  TDragManagerDefault
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

type
  TDragManagerDefault = class;

  { TDragDockComon }

  TDragDockComon = class
  private
    FManager: TDragManagerDefault;
    FDragObjectAutoFree: Boolean;
    FDragImageList: TDragImageList;
    function SendCmDragMsg(ADragObject: TDragObject; ADragMsg: TDragMessage): Boolean;
    function SendDragMessage(AControl: TControl; Msg: TDragMessage; ADragObject: TDragObject; Target: TControl; const Position: TPoint): LRESULT;
  protected
    property Manager: TDragManagerDefault read FManager;
    function Dragging(AControl: TControl): boolean; virtual;abstract;
    procedure DragStarted(APosition: TPoint); virtual;abstract;
    procedure DragMove(APosition: TPoint); virtual;abstract;
    procedure DragStop(ADropped: Boolean); virtual;abstract;
  public
    constructor Create(AManager: TDragManagerDefault; AControl: TControl); virtual;
  end;

  { TDragPerformer }

  TDragPerformer = class(TDragDockComon)
  private
    FDragObject: TDragObject;
  protected
    function Dragging(AControl: TControl): boolean; override;
    procedure DragStarted(APosition: TPoint); override;
    procedure DragMove(APosition: TPoint); override;
    procedure DragStop(ADropped: Boolean); override;
  public
    constructor Create(AManager: TDragManagerDefault; AControl: TControl); override;
    destructor Destroy; override;
  end;

  { TDockPerformer }

  TDockPerformer = class(TDragDockComon)
  private
    FDockObject: TDragDockObject;
    procedure DefaultDockImage(ADrawRect: TRect; Erase: Boolean);
  protected
    function Dragging(AControl: TControl): boolean; override;
    procedure DragStarted(APosition: TPoint); override;
    procedure DragMove(APosition: TPoint); override;
    procedure DragStop(ADropped: Boolean); override;
  public
    constructor Create(AManager: TDragManagerDefault; AControl: TControl); override;
    destructor Destroy; override;
  end;

  { TDragManagerDefault }

  TDragManagerDefault = class(TDragManager)
  private
    FDockSites: TFPList;
    FPerformer: TDragDockComon;
    FStartPosition: TPoint;//mouse position at start of drag or dock
    FThresholdValue: Integer;//treshold before the drag becomes activated
    FWaitForTreshold: boolean;//are we waiting on the treshold activation
    FInDragStop: Boolean; // semaphore to prevent second execution of dragStop
  protected
    //Support input capture
    procedure KeyUp(var Key: Word; Shift : TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure CaptureChanged(OldCaptureControl: TControl); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    //Support methods
    function IsDragging: boolean; override;
    function Dragging(AControl: TControl): boolean; override;
    procedure RegisterDockSite(Site: TWinControl; DoRegister: Boolean);override;

    //The Drag and Drop routines
    procedure DragStart(AControl: TControl; AImmediate: Boolean; AThreshold: Integer); override;
    procedure DragMove(APosition: TPoint); override;
    procedure DragStop(ADropped: Boolean); override;
  end;


{ TDragDockComon }

constructor TDragDockComon.Create(AManager: TDragManagerDefault; AControl: TControl);
begin
  FManager := AManager;
  FDragImageList := nil;
end;

function TDragDockComon.SendDragMessage(AControl: TControl; Msg: TDragMessage; ADragObject: TDragObject; Target: TControl; const Position: TPoint): LRESULT;
begin
  Result := 0;
  if AControl = nil then exit;
  Result := AControl.DoDragMsg(Msg, Position, ADragObject, Target,
    ADragObject is TDragDockObject);
end;


function TDragDockComon.SendCmDragMsg(ADragObject: TDragObject; ADragMsg: TDragMessage): Boolean;
//Send a CM_DRAG message to the window..
begin
  Result := SendDragMessage(ADragObject.DragTarget, ADragMsg,
    ADragObject, ADragObject.DragTarget, ADragObject.DragPos) <> 0;
end;

{ TDragPerformer }

constructor TDragPerformer.Create(AManager: TDragManagerDefault; AControl: TControl);
//Start a drag operation, if not already running
var
  p: TPoint;
begin
  inherited Create(AManager, AControl);

  AControl.DoStartDock(FDragObject);
  if FDragObject = nil then
  begin
    FDragObjectAutoFree := true;
    FDragObject := TDragControlObject.Create(AControl);
  end;
  
  GetCursorPos(p);
  FDragObject.DragPos := p;
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
  ATarget := FindControlAtPosition(APosition,false);
  ATarget := TControl(SendDragMessage(ATarget,dmFindTarget,FDragObject,nil,APosition));

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
  end;
  WidgetSet.SetCursor(Screen.Cursors[DragCursor]);
end;

procedure TDragPerformer.DragStop(ADropped : Boolean);
var 
  ADragObjectCopy: TDragObject;
  DragMsg: TDragMessage;
  Accepted: Boolean;
  TargetPos: TPoint;
begin
  ADragObjectCopy:=FDragObject;
  if FDragObject<>nil then
  try
    FDragObject := nil;
    SetCaptureControl(nil);

    if FDragImageList <> nil then
      FDragImageList.EndDrag;
    WidgetSet.SetCursor(Screen.Cursors[Screen.Cursor]);
    if (ADragObjectCopy.DragTarget <> nil) and (ADragObjectCopy.DragTarget is TControl) then
      TargetPos := ADragObjectCopy.DragTargetPos //controls can override the position
    else
      TargetPos := ADragObjectCopy.DragPos; //otherwise take the current position
    Accepted := ADropped and SendCmDragMsg(ADragObjectCopy, dmDragLeave);

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
    ADragObjectCopy.Control.DoEndDrag(ADragObjectCopy.DragTarget, TargetPos.X, TargetPos.Y);

  finally
    //erase global variables (dragging stopped)
    if FDragObjectAutoFree then
      ADragObjectCopy.Free;
  end;
end;


{ TDockPerformer }

constructor TDockPerformer.Create(AManager: TDragManagerDefault; AControl: TControl);
//Start a drag operation, if not already running
var
  p: TPoint;
begin
  inherited Create(AManager, AControl);
  AControl.DoStartDock(TDragObject(FDockObject));
  if FDockObject = nil then
  begin
    FDragObjectAutoFree := true;
    FDockObject  := TDragDockObject.Create(AControl);
  end;
  
  GetCursorPos(p);
  FDockObject.DragPos := p;
  AControl.CalculateDockSizes;
  FDockObject.DockRect := Rect(p.x, p.y, p.x + AControl.Width, p.y + AControl.Height);
  FDockObject.EraseDockRect := Rect(MaxInt, 0, MaxInt, 0);
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
//Imput device has moved beyond tresholt (or immediate docking)
begin
  if FDockObject = nil then
    Exit;
  FDragImageList := FDockObject.GetDragImages;
  if FDragImageList <> nil then
    FDragImageList.BeginDrag(0, APosition.X, APosition.Y);
  DefaultDockImage(FDockObject.DockRect, False);
  FDockObject.EraseDockRect := FDockObject.DockRect;
end;

procedure TDockPerformer.DragMove(APosition: TPoint);

  function GetDropControl: TControl;
  //Select a control where the dragged control will be docked
  var
    ADragTarget: TWinControl;
    AControl: TControl;
    i: integer;
  begin
    Result := nil;
    ADragTarget := TWinControl(FDockObject.DragTarget);
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
  //Replace with lookup in Application.Zorder(?)
  var
    AControl: TWinControl;
    CanDock: Boolean;
    QualifyingSites: TList;
    ARect: TRect;
    I: Integer;
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
        if not AControl.Visible then continue;
        if AControl=FDockObject.Control then continue;
        if IsControlChildOfClient(AControl) then continue;

        if (FDockObject.Control.HostDockSite <> AControl) or (AControl.VisibleDockClientCount > 1) then
        begin
          CanDock := True;
          AControl.GetSiteInfo(FDockObject.Control, ARect, APosition, CanDock);
          if CanDock and PtInRect(ARect, APosition) then
            QualifyingSites.Add(AControl);
        end;
      end;

      if QualifyingSites.Count > 0 then
      begin
        Result := TWinControl(QualifyingSites[0]); //.GetTopSite; TODO!!!
        //if not ValidDockTarget(Result) TODO!!!
        //then Result := nil;            TODO!!!
      end;
    finally
      QualifyingSites.Free;
    end;
  end;

var
  ATarget: TControl;
  DragCursor: TCursor;
begin
  if FDockObject = nil then
    Exit;

  //Inform user of entering and leaving
  ATarget := FindDockSiteAtPosition;
  if ATarget <> FDockObject.DragTarget then
  begin
    SendCmDragMsg(FDockObject, dmDragLeave);
    FDockObject.DragTarget := TWinControl(ATarget);
    FDockObject.DragPos := APosition;
    SendCmDragMsg(FDockObject, dmDragEnter);
  end
  else
    FDockObject.DragPos := APosition;


  if FDockObject.DragTarget <> nil then
    FDockObject.DragTargetPos := TControl(FDockObject.DragTarget).ScreenToClient(APosition);
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
    if DragTarget = nil then
      FDockObject.Control.DockTrackNoTarget(TDragDockObject(FDockObject), APosition.X, APosition.Y)
    else
    begin
      DropOnControl := GetDropControl;
      if DropOnControl = nil then
        with FDockObject do
          DropAlign := DragTarget.GetDockEdge(DragTargetPos)
      else
        DropAlign := DropOnControl.GetDockEdge(DropOnControl.ScreenToClient(APosition));
    end;

    //Draw the form outlines when the position has changed
    if not CompareMem(@DockRect, @EraseDockRect, SizeOf(TRect)) then
    begin
      if EraseDockRect.Left <> Maxint then
        DefaultDockImage(FDockObject.EraseDockRect, True);
      DefaultDockImage(FDockObject.DockRect, False);
      EraseDockRect := DockRect;
    end;
  end;
end;

procedure TDockPerformer.DragStop(ADropped: Boolean);
var
  ADockObjectCopy: TDragDockObject;
  ParentForm: TCustomForm;
  DragMsg: TDragMessage;
  Accepted: Boolean;
  TargetPos: TPoint;
begin
  ADockObjectCopy := FDockObject;
  if FDockObject <> nil then
  try
    FDockObject := nil;
    SetCaptureControl(nil);

    DefaultDockImage(ADockObjectCopy.EraseDockRect, True);
    ADockObjectCopy.Floating := ADockObjectCopy.DragTarget = nil;

    Accepted := ADockObjectCopy.DragTarget <> nil;
    if ADropped then
    begin
      if ADockObjectCopy.Control.HostDockSite <> nil then
        Accepted := ADockObjectCopy.Control.HostDockSite.DoUnDock(TWinControl(ADockObjectCopy.DragTarget), ADockObjectCopy.Control)
      else
      if ADockObjectCopy.DragTarget = nil then
        Accepted := True else
      if ADockObjectCopy.Control.HostDockSite = nil then
        Accepted := True;
    end;

    if (ADockObjectCopy.DragTarget <> nil) and
       (ADockObjectCopy.DragTarget is TControl) then
      TargetPos := ADockObjectCopy.DragTargetPos //controls can override the position
    else
      TargetPos := ADockObjectCopy.DragPos; //otherwise take the current position
    Accepted := Accepted and (ADockObjectCopy.Floating or SendCmDragMsg(ADockObjectCopy, dmDragLeave)) and ADropped;
    if Accepted and ADockObjectCopy.Floating then
    begin
      ParentForm := GetParentForm(ADockObjectCopy.Control);
      if (ParentForm <> nil) and
         (ParentForm.ActiveControl = ADockObjectCopy.Control) then
        ParentForm.ActiveControl := nil;
      ADockObjectCopy.Control.DoFloatMsg(ADockObjectCopy);
    end;


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
      SendDragMessage(ADockObjectCopy.DragTarget, DragMsg, ADockObjectCopy, ADockObjectCopy.DragTarget, ADockObjectCopy.DragPos);
    end;

    if not Accepted then
      ADockObjectCopy.Control.DragCanceled;
    ADockObjectCopy.Control.DoEndDock(ADockObjectCopy.DragTarget,TargetPos.X,TargetPos.Y);
  finally
    if FDragObjectAutoFree then
      ADockObjectCopy.Free
  end;
end;


procedure TDockPerformer.DefaultDockImage(ADrawRect: TRect; Erase: Boolean);
begin
  WidgetSet.DrawDefaultDockImage(ADrawRect, Erase);
end;

{ TDragManagerDefault }

destructor TDragManagerDefault.Destroy;
begin
  FDockSites.Free;
  inherited Destroy;
end;

function TDragManagerDefault.IsDragging: boolean;
//Is something being dragged
begin
  Result := FPerformer <> nil
end;

function TDragManagerDefault.Dragging(AControl: TControl): boolean;
//Is the control being dragged
begin
  if FPerformer = nil then
    Result := false
  else
    Result := FPerformer.Dragging(AControl)
end;

procedure TDragManagerDefault.DragStart(AControl: TControl; AImmediate: Boolean; AThreshold: Integer);
//Start a drag operation
begin
  if FPerformer = nil then
  begin
    if AThreshold >= 0 then
      FThresholdValue := AThreshold
    else
      FThresholdValue := DragThreshold;
    FWaitForTreshold := not AImmediate;
    GetCursorPos(FStartPosition);

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
    FPerformer.DragMove(APosition);
  end;
end;

procedure TDragManagerDefault.DragStop(ADropped : Boolean);
//End the drag operation
begin
  if (FPerformer <> nil) and not FInDragStop then
  begin
    FInDragStop := True;
    try
      FPerformer.DragStop(ADropped);
    finally
      FreeAndNil(FPerformer);
      FInDragStop := False;
    end;
  end;
end;

procedure TDragManagerDefault.RegisterDockSite(Site: TWinControl; DoRegister: Boolean);
begin
  if (Site <> nil) then
  begin
    if FDockSites = nil then
      FDockSites := TFPList.Create;
    if DoRegister then
      FDockSites.Add(Site)
    else
      FDockSites.Remove(Site)
  end;
end;

procedure TDragManagerDefault.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    DragStop(False);
  Key := 0;
end;

procedure TDragManagerDefault.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    DragStop(False);
  Key := 0;
end;

procedure TDragManagerDefault.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //In TControl.WndProc/LM_LBUTTONDOWN a drag session is started but a few lines
  //later the LM_LBUTTONDOWN is send to the dragmanager which would end dragging.
  //Dragging is only ended by a MouseUp so we can ignore this mesage anywhay.
  //DragStop(true);
end;

constructor TDragManagerDefault.Create;
begin
  inherited Create;
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
  DragStop(true);
end;

procedure TDragManagerDefault.CaptureChanged(OldCaptureControl: TControl);
var
  i: integer;
  AIsDragging: Boolean;
begin
  // if this is TWinControl, and it have controls (not TWinControls)
  // then we should check Dragging in those controls
  AIsDragging := OldCaptureControl.Dragging;
  if (not AIsDragging) and (OldCaptureControl is TWinControl) then
  begin
    for i := 0 to TWinControl(OldCaptureControl).ControlCount - 1 do
    begin
      AIsDragging := AIsDragging or TWinControl(OldCaptureControl).Controls[i].Dragging;
      if AIsDragging then
        break;
    end;
  end;
  DragStop(AIsDragging);
end;


//included by controls.pp
