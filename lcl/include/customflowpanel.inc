{%MainUnit ../extctrls.pp}

{******************************************************************************
                            TCustomFlowPanel
 ******************************************************************************
                             Ondrej Pokorny
 ******************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 ******************************************************************************
}
{

  Delphi compatibility:

  - TFlowPanel is compatible with Delphi implementation
}

{ TFlowPanelControlList }

constructor TFlowPanelControlList.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TCustomFlowPanel);
  inherited Create(AOwner, TFlowPanelControl);
end;

function TFlowPanelControlList.Add: TFlowPanelControl;
begin
  Result := TFlowPanelControl(inherited Add);
end;

procedure TFlowPanelControlList.AddControl(AControl: TControl;
  AIndex: Integer);
var
  I: Integer;
  Item: TFlowPanelControl;
begin
  if IndexOf(AControl) >= 0 then
    Exit;

  if AIndex = -1 then
    for I := 0 to Count-1 do
      if not Assigned(Items[I].Control) then
      begin
        AIndex := I;
        break;
      end;

  if AIndex = -1 then
    Item := Add
  else
    Item := Items[AIndex];

  Item.FControl := AControl;
end;

function TFlowPanelControlList.AllowAdd: Boolean;
begin
  Result := false;
end;

function TFlowPanelControlList.AllowDelete: Boolean;
begin
  Result := false;
end;

function TFlowPanelControlList.FPOwner: TCustomFlowPanel;
begin
  Result := TCustomFlowPanel(GetOwner);
end;

function TFlowPanelControlList.GetItem(Index: Integer
  ): TFlowPanelControl;
begin
  Result := TFlowPanelControl(inherited GetItem(Index));
end;

function TFlowPanelControlList.IndexOf(AControl: TControl): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Control = AControl then
      Exit;
  Result := -1;
end;

procedure TFlowPanelControlList.RemoveControl(AControl: TControl);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if Items[I].Control = AControl then
    begin
      Items[I].FControl := nil;
      Delete(I);
      Exit;
    end;
end;

procedure TFlowPanelControlList.SetItem(Index: Integer;
  const AItem: TFlowPanelControl);
begin
  inherited SetItem(Index, AItem);
end;

{ TFlowPanelControl }

procedure TFlowPanelControl.AssignTo(Dest: TPersistent);
var
  xDest: TFlowPanelControl;
begin
  if Dest is TFlowPanelControl then
  begin
    xDest := TFlowPanelControl(Dest);
    xDest.FWrapAfter := Self.FWrapAfter;
  end else
    inherited AssignTo(Dest);
end;

function TFlowPanelControl.AllowAdd: Boolean;
begin
  Result := false;
end;

function TFlowPanelControl.AllowDelete: Boolean;
begin
  Result := false;
end;

function TFlowPanelControl.FPCollection: TFlowPanelControlList;
begin
  Result := Collection as TFlowPanelControlList;
end;

function TFlowPanelControl.FPOwner: TCustomFlowPanel;
begin
  Result := FPCollection.FPOwner;
end;

function TFlowPanelControl.GetDisplayName: String;
begin
  if (FControl is TComponent) then
    Result := '[' + TComponent(FControl).Name + ']'
  else
    Result := inherited;
end;

procedure TFlowPanelControl.SetControl(const aControl: TControl);
begin
  if FControl = aControl then Exit;
  Assert(FControl = nil);
  FControl := aControl;
  if FControl <> nil then
    FControl.Parent := FPOwner;
end;

procedure TFlowPanelControl.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);

  if FPOwner.ComponentState * [csLoading, csUpdating,  csDestroying] = [] then
    FPOwner.ReAlign;
end;

procedure TFlowPanelControl.SetWrapAfter(const AWrapAfter: TWrapAfter);
begin
  if FWrapAfter = AWrapAfter then exit;
  FWrapAfter := AWrapAfter;

  if FPOwner.ComponentState * [csLoading, csUpdating,  csDestroying] = [] then
    FPOwner.ReAlign;
end;

{ TCustomFlowPanel }

constructor TCustomFlowPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FControlList := TFlowPanelControlList.Create(Self);
  FAutoWrap := True;
end;

procedure TCustomFlowPanel.AlignControls(AControl: TControl;
  var RemainingClientRect: TRect);

var
  xNewBounds: array of TRect; // right=width, bottom=height

  procedure AlignLayout(const BStartControl, BEndControl, BTopLeft, BSize: Integer);
  var
    I: Integer;
    xControl: TControl;
    xConBS: TControlBorderSpacing;
  begin
    if FFlowLayout = tlTop then
      Exit;

    for I := BStartControl to BEndControl do
    begin
      xControl := FControlList[I].Control;
      if xControl = nil then
        Continue;
      if not xControl.Visible and not (csDesigning in ComponentState) then
        Continue;

      xConBS := xControl.BorderSpacing;
      if FFlowStyle in [fsLeftRightTopBottom, fsLeftRightBottomTop, fsRightLeftTopBottom, fsRightLeftBottomTop] then
      begin
        case FFlowLayout of
          tlCenter: xNewBounds[I].Top := BTopLeft + (BSize - xConBS.ControlHeight) div 2 + xConBS.Top + xConBS.Around;
          tlBottom: xNewBounds[I].Top := BTopLeft + BSize - xControl.Height - xConBS.Bottom - xConBS.Around;
        end;
      end else
      begin
        case FFlowLayout of
          tlCenter: xNewBounds[I].Left := BTopLeft + (BSize - xConBS.ControlWidth) div 2 + xConBS.Left + xConBS.Around;
          tlBottom: xNewBounds[I].Left := BTopLeft + BSize - xControl.Width - xConBS.Right - xConBS.Around;
        end;
      end;
    end;
  end;
const
  cXIncDir:     array[TFlowStyle] of Integer = (1, -1,  1, -1,  1,  1, -1, -1);
  cYIncDir:     array[TFlowStyle] of Integer = (1,  1, -1, -1,  1, -1,  1, -1);
  cYDeltaConst: array[TFlowStyle] of Integer = (0,  0, -1, -1,  0,  0,  0,  0);
  cXDeltaConst: array[TFlowStyle] of Integer = (0,  0,  0,  0,  0,  0, -1, -1);
var
  I, L: Integer;
  xMaxHeight, xMaxWidth, xRowStart: Integer;
  xPosition: TPoint;
  xSize, xGroupSize: TSize;
  xControl: TControl;
  xConBS: TControlBorderSpacing;
  xForceWrap, xForbidWrap: Boolean;
begin
  if ControlCount = 0 then
    Exit;

  DisableAlign;
  try
    xMaxHeight := 0;
    xMaxWidth := 0;
    AdjustClientRect(RemainingClientRect);
    case FFlowStyle of
      fsLeftRightTopBottom,
      fsTopBottomLeftRight:
        xPosition := RemainingClientRect.TopLeft;
      fsRightLeftTopBottom,
      fsTopBottomRightLeft:
        xPosition := Point(RemainingClientRect.Right, RemainingClientRect.Top);
      fsLeftRightBottomTop,
      fsBottomTopLeftRight:
        xPosition := Point(RemainingClientRect.Left, RemainingClientRect.Bottom);
      fsRightLeftBottomTop,
      fsBottomTopRightLeft:
        xPosition := RemainingClientRect.BottomRight;
    end;

    xRowStart := 0;
    SetLength(xNewBounds, FControlList.Count);
    for I := 0 to FControlList.Count-1 do
    begin
      xControl := FControlList[I].Control;
      if xControl = nil then
        Continue;
      xConBS := xControl.BorderSpacing;
      if not xControl.Visible and not (csDesigning in ComponentState) then
        continue;

      xSize.cx := xConBS.ControlWidth;
      xSize.cy := xConBS.ControlHeight;
      xGroupSize := xSize;

      xForceWrap := (I > 0) and (FControlList[I-1].WrapAfter = waForce);
      xForbidWrap := (I > 0) and (FControlList[I-1].WrapAfter = waForbid);
      if not xForceWrap
      and ((I = 0) or not(FControlList[I-1].WrapAfter in [waAvoid, waForbid])) then
      begin
        for L := I to FControlList.Count-2 do
        begin
          if FControlList[L].WrapAfter in [waAvoid, waForbid] then
          begin
            case FFlowStyle of
              fsLeftRightTopBottom, fsLeftRightBottomTop, fsRightLeftTopBottom, fsRightLeftBottomTop:
                Inc(xGroupSize.cx, FControlList[L+1].Control.BorderSpacing.ControlWidth);
              fsTopBottomLeftRight, fsTopBottomRightLeft, fsBottomTopLeftRight, fsBottomTopRightLeft:
                Inc(xGroupSize.cy, FControlList[L+1].Control.BorderSpacing.ControlHeight);
            end;
          end else
            break;
        end;
      end;
      case FFlowStyle of
        fsLeftRightTopBottom,
        fsLeftRightBottomTop:
          if (xMaxHeight > 0) and FAutoWrap and not xForbidWrap
          and (xForceWrap or (xPosition.X + xGroupSize.cx >= RemainingClientRect.Right)) then
          begin
            AlignLayout(xRowStart, I-1, xPosition.Y, xMaxHeight);
            Inc(xPosition.Y, xMaxHeight * cYIncDir[FFlowStyle]);
            xMaxHeight := 0;
            xRowStart := I;
            xPosition.X := RemainingClientRect.Left;
          end;
        fsRightLeftTopBottom,
        fsRightLeftBottomTop:
          begin
            Dec(xPosition.X, xGroupSize.cx);
            if (xMaxHeight > 0) and FAutoWrap and not xForbidWrap
            and (xForceWrap or (xPosition.X <= 0)) then
            begin
              AlignLayout(xRowStart, I-1, xPosition.Y, xMaxHeight);
              Inc(xPosition.Y, xMaxHeight * cYIncDir[FFlowStyle]);
              xMaxHeight := 0;
              xRowStart := I;
              xPosition.X := RemainingClientRect.Right - xSize.cx;
            end;
          end;
        fsTopBottomLeftRight,
        fsTopBottomRightLeft:
          if (xMaxWidth > 0) and FAutoWrap and not xForbidWrap
          and (xForceWrap or (xPosition.Y + xGroupSize.cy >= RemainingClientRect.Bottom)) then
          begin
            AlignLayout(xRowStart, I-1, xPosition.X, xMaxWidth);
            Inc(xPosition.X, xMaxWidth * cXIncDir[FFlowStyle]);
            xMaxWidth := 0;
            xRowStart := I;
            xPosition.Y := RemainingClientRect.Top;
          end;
        fsBottomTopLeftRight,
        fsBottomTopRightLeft:
          begin
            Dec(xPosition.Y, xGroupSize.cy);
            if (xMaxWidth > 0) and FAutoWrap and not xForbidWrap
            and (xForceWrap or (xPosition.Y <= 0)) then
            begin
              AlignLayout(xRowStart, I-1, xPosition.X, xMaxWidth);
              Inc(xPosition.X, xMaxWidth * cXIncDir[FFlowStyle]);
              xMaxWidth := 0;
              xRowStart := I;
              xPosition.Y := RemainingClientRect.Bottom - xSize.cy;
            end;
          end;
      end;
      if xSize.cy > xMaxHeight then
        xMaxHeight := xSize.cy;
      if xSize.cx > xMaxWidth then
        xMaxWidth := xSize.cx;
      xNewBounds[I] := Rect(
        xPosition.X + xConBS.Left + xConBS.Around + cXDeltaConst[FFlowStyle] * xSize.cx,
        xPosition.Y + xConBS.Top + xConBS.Around + cYDeltaConst[FFlowStyle] * xSize.cy,
        xSize.cx - (xConBS.Left + xConBS.Right + xConBS.Around*2),
        xSize.cy - (xConBS.Top + xConBS.Bottom + xConBS.Around*2));
      if FFlowStyle in [fsLeftRightTopBottom, fsLeftRightBottomTop] then
        Inc(xPosition.X, xSize.cx * cXIncDir[FFlowStyle])
      else if FFlowStyle in [fsTopBottomLeftRight, fsTopBottomRightLeft] then
        Inc(xPosition.Y, xSize.cy + cYIncDir[FFlowStyle]);
    end;

    if FFlowStyle in [fsLeftRightTopBottom, fsLeftRightBottomTop, fsRightLeftTopBottom, fsRightLeftBottomTop] then
      AlignLayout(xRowStart, FControlList.Count-1, xPosition.Y, xMaxHeight)
    else
      AlignLayout(xRowStart, FControlList.Count-1, xPosition.X, xMaxWidth);

    for I := 0 to FControlList.Count-1 do
    begin
      xControl := FControlList[I].Control;
      if xControl = nil then
        continue;
      xConBS := xControl.BorderSpacing;
      if not xControl.Visible and not (csDesigning in ComponentState) then
        continue;

      xControl.SetBounds(
        xNewBounds[I].Left, xNewBounds[I].Top,
        xNewBounds[I].Right, xNewBounds[I].Bottom); // right=width, bottom=height
    end;
  finally
    EnableAlign;
  end;
end;

procedure TCustomFlowPanel.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  xControl: TControl;
  xTestRect, xClientRect: TRect;
  I: Integer;
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  if FControlList.Count > 0 then
  begin
    xTestRect := Rect(0, 0, 100, 100);
    xClientRect := xTestRect;
    AdjustClientRect(xClientRect);
    for I := 0 to ControlCount-1 do
    begin
      xControl := FControlList.Items[I].Control;
      if FFlowStyle in [fsLeftRightTopBottom, fsRightLeftTopBottom, fsLeftRightBottomTop, fsRightLeftBottomTop] then
        PreferredHeight := Max(PreferredHeight,
          xControl.BoundsRect.Bottom+xControl.BorderSpacing.Around+xControl.BorderSpacing.Bottom +
          xTestRect.Bottom-xClientRect.Bottom-xTestRect.Top+xClientRect.Top)
      else
        PreferredWidth := Max(PreferredWidth,
          xControl.BoundsRect.Right+xControl.BorderSpacing.Around+xControl.BorderSpacing.Right +
          xTestRect.Right-xClientRect.Right-xTestRect.Left+xClientRect.Left);
    end;
  end;
end;

procedure TCustomFlowPanel.CMControlChange(var Message: TCMControlChange);
begin
  //inherited CMControlChange(Message); uncomment if CMControlChange should appear in parent classes

  if (csLoading in ComponentState) then
    Exit;

  if Message.Inserting and (Message.Control.Parent = Self) then
  begin
    DisableAlign;
    try
      Message.Control.Anchors := [];
      Message.Control.Align := alNone;
      FControlList.AddControl(Message.Control);
      ReAlign;
    finally
      EnableAlign;
    end;
  end else
    FControlList.RemoveControl(Message.Control);
end;

destructor TCustomFlowPanel.Destroy;
begin
  inherited Destroy;
  FControlList.Free;
end;

function TCustomFlowPanel.GetControlIndex(AControl: TControl): Integer;
begin
  Result := FControlList.IndexOf(AControl);
end;

procedure TCustomFlowPanel.SetAutoWrap(const AAutoWrap: Boolean);
begin
  if FAutoWrap = AAutoWrap then Exit;
  FAutoWrap := AAutoWrap;

  ReAlign;
end;

procedure TCustomFlowPanel.SetControlIndex(AControl: TControl; Index: Integer);
var
  CurIndex: Integer;
begin
  CurIndex := GetControlIndex(AControl);
  if (CurIndex > -1) and (CurIndex <> Index) and (Index < FControlList.Count) then
  begin
    FControlList.Items[CurIndex].Index := Index;
    Realign;
  end;
end;

procedure TCustomFlowPanel.SetControlList(
  const AControlList: TFlowPanelControlList);
begin
  FControlList.Assign(AControlList);
end;

procedure TCustomFlowPanel.SetFlowLayout(const aFlowLayout: TTextLayout);
begin
  if FFlowLayout = aFlowLayout then Exit;
  FFlowLayout := aFlowLayout;

  ReAlign;
end;

procedure TCustomFlowPanel.SetFlowStyle(const AFlowStyle: TFlowStyle);
begin
  if FFlowStyle = AFlowStyle then Exit;
  FFlowStyle := AFlowStyle;

  ReAlign;
end;


// included by extctrls.pp
