{%MainUnit ../extctrls.pp}

{******************************************************************************
                                  TControlBar
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

{ TCustomControlBar }

constructor TCustomControlBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
                   csDoubleClicks, csOpaque, csParentBackground];
  FAutoDrag := True;
  FAutoDock := True;
  DragMode := dmAutomatic;
  FPicture := TPicture.Create;
//  FPicture.OnChange := @PictureChanged;
  FRowSize := 26;
  FRowSnap := True;
  DoubleBuffered := True;
  DockSite := True;
  Width := 100;
  Height := 50;
end;

destructor TCustomControlBar.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TCustomControlBar.AlignControls(aControl: TControl; var aRect: TRect);
begin
  // ToDo: The layout algorithm must be implemented here.
//  DebugLn('TCustomControlBar.AlignControls');
end;

function TCustomControlBar.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanAutoSize(NewWidth, NewHeight);
//  DebugLn('TCustomControlBar.CanAutoSize');
end;

procedure TCustomControlBar.SetPicture(aValue: TPicture);
begin
  FPicture.Assign(aValue);
end;

procedure TCustomControlBar.CreateParams(var aParams: TCreateParams);
begin
  inherited CreateParams(aParams);
end;

procedure TCustomControlBar.DoBandMove(aControl: TControl; var aRect: TRect);
begin
//  DebugLn('TCustomControlBar.DoBandMove');
  if Assigned(FOnBandMove) then
    FOnBandMove(Self, aControl, aRect);
end;

procedure TCustomControlBar.DoBandPaint(aControl: TControl; aCanvas: TCanvas;
  var aRect: TRect; var aOptions: TBandPaintOptions);
begin
  if Assigned(FOnBandPaint) then
    FOnBandPaint(Self, aControl, aCanvas, aRect, aOptions);
end;

procedure TCustomControlBar.DockOver(aSource: TDragDockObject; X, Y: Integer;
  aState: TDragState; var Accept: Boolean);
begin
  inherited DockOver(aSource, X, Y, aState, Accept);
//  DebugLn('TCustomControlBar.DockOver');
end;

function TCustomControlBar.DragControl(aControl: TControl; X, Y: Integer;
  KeepCapture: Boolean): Boolean;
begin
//  DebugLn('TCustomControlBar.DragControl');
  Result := True;
  if Assigned(aControl) and Assigned(FOnBandDrag) then
    FOnBandDrag(Self, aControl, Result);
  if Result then
    aControl.BeginDrag(True);
end;

procedure TCustomControlBar.GetControlInfo(aControl: TControl;
  var Insets: TRect; var PreferredSize, RowCount: Integer);
begin
  if RowCount = 0 then
    RowCount := 1;
  if Assigned(FOnBandInfo) then
    FOnBandInfo(Self, aControl, Insets, PreferredSize, RowCount);
end;

function TCustomControlBar.GetPalette: HPALETTE;
begin
  if FPicture.Graphic <> nil then
    Result := FPicture.Graphic.Palette
  else
    Result := 0;
end;

function TCustomControlBar.DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint;
  ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean): LRESULT;
begin
//  DebugLn('TCustomControlBar.DoDragMsg');
  Result:=inherited DoDragMsg(ADragMessage, APosition, ADragObject, ATarget, ADocking);
end;

procedure TCustomControlBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//  DebugLn('TCustomControlBar.MouseDown_1');
  inherited MouseDown(Button, Shift, X, Y);
  // ToDo
end;

procedure TCustomControlBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
//  DebugLn('TCustomControlBar.MouseMove_1');
  inherited MouseMove(Shift, X, Y);
  // ToDo
end;

procedure TCustomControlBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Cntrl: TControl;
begin
//  DebugLn('TCustomControlBar.MouseUp');
  // ToDo
  if Assigned(FDragControl) then
  begin
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomControlBar.Paint;
begin
  // ToDo
end;

procedure TCustomControlBar.FlipChildren(AllLevels: Boolean);
begin
  // Do nothing
end;

procedure TCustomControlBar.StickControls;
begin
  // ToDo
end;


