{%MainUnit ../extctrls.pp}

{******************************************************************************
                                     TCustomForm
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

  ToDo:
    - Fix changing Mouse cursor
    - Fix gtk look
    - ResizeStyle
    
}

{ TCustomSplitter }

procedure TCustomSplitter.SetResizeStyle(const AValue: TResizeStyle);
begin
  if FResizeStyle=AValue then exit;
  FResizeStyle:=AValue;
end;

procedure TCustomSplitter.SetAutoSnap(const AValue: boolean);
begin
  if FAutoSnap=AValue then exit;
  FAutoSnap:=AValue;
end;

procedure TCustomSplitter.SetBeveled(const AValue: boolean);
begin
  if FBeveled=AValue then exit;
  FBeveled:=AValue;
  Invalidate;
end;

procedure TCustomSplitter.SetMinSize(const AValue: integer);
begin
  if (FMinSize=AValue) or (AValue<1) then exit;
  FMinSize:=AValue;
end;

procedure TCustomSplitter.StartSplitterMove(Restart: boolean;
  const MouseXY: TPoint);
begin
  if (not Restart) and FSplitDragging then exit;
  FSplitDragging := True;
  fSplitterStartMouseXY:=MouseXY;
  fSplitterStartLeftTop:= Point(Left,Top);
end;

procedure TCustomSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  MousePos: TPoint;
begin
  inherited MouseDown(Button, Shift, X, Y);
  // While resizing X, Y are not valid. Use absolute mouse position.
  GetCursorPos(MousePos);
  StartSplitterMove(True,MousePos);
end;

function FindOppositeControl(Control: TControl): TControl;
var
  i,count: Integer;
  CurControl: TControl;
  fRect: TRect;
  fPoint: TPoint;
  alignList: TAlignSet;
begin
  Result := nil;
  count := Control.Parent.ControlCount;
  if count > 0 then begin
    fRect := Control.BoundsRect;
    case Control.Align of
      alTop: begin
          fPoint := fRect.BottomRight;
          fPoint.y := fPoint.y+1;
          Dec(fPoint.x);
          alignList := [alTop,alRight,alClient];
        end;
      alBottom: begin
          fPoint := fRect.TopLeft;
          Dec(fPoint.y);
          alignList := [alLeft,alBottom,alClient];
        end;
      alLeft: begin
          fPoint := fRect.BottomRight;
          Inc(fPoint.x);
          Dec(fPoint.y);
          alignList := [alLeft,alClient];
        end;
      alRight: begin
          fPoint := fRect.TopLeft;
          Dec(fPoint.x);
          alignList := [alRight,alClient];
        end;
    end;  // case
    Dec(count);
    for i := 0 to count do begin
      CurControl := Control.Parent.Controls[i];
      if (CurControl <> Control)
         and CurControl.Visible
         and (CurControl.Align in alignList)
         and (PtInRect(CurControl.BoundsRect, fPoint))
      then Result := CurControl;
      if Assigned(Result) then Break;
    end;  // for i
  end;
end;

function FindVirtualOppositeControl(Control: TControl): TControl;
  function CompPos(CurControl, Control: TControl): Boolean;
  begin
    Result := False;
    case Control.Align of
      alTop:
        if (CurControl.Align = Control.Align)
           and (CurControl.Top >= Control.BoundsRect.Bottom)
        then Result := True;
      alBottom:
        if (CurControl.Align = Control.Align)
           and (CurControl.BoundsRect.Bottom <= Control.Top)
        then Result := True;
      alLeft:
        if (CurControl.Align = Control.Align)
           and (CurControl.Left >= Control.BoundsRect.Right)
        then Result := True;
      alRight:
        if (CurControl.Align = Control.Align)
           and (CurControl.BoundsRect.Right <= Control.Left)
        then Result := True;
    end;
  end;
  function OppositeControl(CurControl,Control: TControl): Boolean;
  begin
    Result := False;
    case Control.Align of
      alLeft: if (CurControl.Align = alRight) then Result := True;
      alRight: if (CurControl.Align = alLeft) then Result := True;
      alTop: if (CurControl.Align = alBottom) then Result := True;
      alBottom: if (CurControl.Align = alTop) then Result := True;
    end;
  end;
var
  i,count: Integer;
  CurControl: TControl;
begin
  Result := nil;
  count := Control.Parent.ControlCount;
  if count > 0 then begin
    Dec(count);
    for i := 0 to count do begin
      CurControl := Control.Parent.Controls[i];
      if (CurControl <> Control) then begin
        if ((Result = nil) and OppositeControl(CurControl, Control))
           or (Assigned(Result) and CompPos(CurControl, Result))
        then Result := CurControl;
      end;
    end;  // for i
  end;
end;

procedure TCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AlignControl, LastControl, VirtualOppositeControl, ClientControl: TControl;

  function GetParentClientSize: Integer;
  begin
    case Self.Align of
      alLeft, alRight: Result := Parent.ClientWidth;
      alTop, alBottom: Result := Parent.ClientHeight;
    end;
  end;
  function GetControlMinPos(Control: TControl): Integer;
  begin
    if Assigned(Control)
    then case Self.Align of
           alLeft,alRight: Result := Control.Left;
           alTop,alBottom: Result := Control.Top;
         end
    else case Self.Align of
           alLeft,alTop: Result := 0;
           alRight,alBottom: Result := GetParentClientSize;
         end;
  end;
  function GetControlSize(Control: TControl): Integer;
  begin
    Result := 0;
    if Assigned(Control)
    then case Self.Align of
      alLeft, alRight: Result := Control.Width;
      alTop, alBottom: Result := Control.Height;
    end;
  end;
  function GetControlConstraintsMinSize(Control: TControl): Integer;
  begin
    case Self.Align of
      alLeft, alRight: Result := Control.Constraints.MinWidth;
      alTop, alBottom: Result := Control.Constraints.MinHeight;
    end;
  end;
  procedure SetAlignControlSize(NewSize: Integer);
  begin
    case Self.Align of
      alLeft:
        AlignControl.Width := NewSize;
      alRight:
        begin
          Parent.DisableAlign;
          AlignControl.Left := AlignControl.Left + (AlignControl.Width - NewSize);
          AlignControl.Width := NewSize;
          Parent.EnableAlign;
        end;
      alTop:
        AlignControl.Height := NewSize;
      alBottom:
        begin
          Parent.DisableAlign;
          AlignControl.Top := AlignControl.Top + (AlignControl.Height - NewSize);
          AlignControl.Height := NewSize;
          Parent.EnableAlign;
        end;
    end;
  end;
  function CalcNewSize(StartSize, EndSize, Offset: Integer): Integer;
  var
    NewSize: Integer;
  begin
    NewSize := GetControlSize(AlignControl);
    case Self.Align of
      alLeft, alTop:     Inc(NewSize, Offset);
      alRight, alBottom: Dec(NewSize, Offset);
    end;

    if NewSize > EndSize then NewSize := EndSize;
    if NewSize < StartSize then NewSize := StartSize;

    if fAutoSnap and (NewSize < fMinSize)
    then NewSize := StartSize;
    Result := NewSize;
  end;
  function GetVirtualControlMinPos(Control: TControl): Integer;
  begin
    case Self.Align of
      alLeft: Result := Control.Left;
      alRight: Result := Control.BoundsRect.Right;
      alTop: Result := Control.Top;
      alBottom: Result := Control.BoundsRect.Bottom;
    end;
  end;
  function CalcOffset(X,Y: Integer): Integer;
  begin
    case Self.Align of
      alLeft, alRight:
         Result := (X-fSplitterStartMouseXY.X)
                 - (Self.Left-fSplitterStartLeftTop.X);
      alTop, alBottom:
         Result := (Y-fSplitterStartMouseXY.Y)
                 - (Self.Top-fSplitterStartLeftTop.Y);
    end;
  end;
  function FindClientControl: TControl;
  var
    CurControl: TControl;
    count: Integer;
  begin
    Result := nil;
    for count := Parent.ControlCount-1 downto 0 do begin
      CurControl := Parent.Controls[count];
      if (CurControl <> Self)
         and ((CurControl.Align = alClient)
               or ((Self.Align in [alTop,alBottom])
                    and (CurControl.Align in [alLeft,alRight]))) then begin
        Result := CurControl;
        Break;
      end;
    end;  // for count
  end;  // function FindClientControl
  function FindLastControl(Control: TControl): TControl;
  var
    CurControl: TControl;
  begin
    CurControl := Control;
    while Assigned(CurControl) do begin
      Control := CurControl;
      CurControl := FindOppositeControl(Control);
    end;
    Result := Control;
  end;  // function FindLastControl
var
  Offset: Integer;
  StartSize: Integer;
  EndSize: Integer;
  NewSize: Integer;
  MousePos: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  if not (Self.Align in [alLeft, alRight, alTop, alBottom]) then Exit;
  if (ssLeft in Shift) and (Parent <> nil) then begin
    AlignControl := FindAlignControl;
    if not Assigned(AlignControl) then Exit;
    
    // While resizing X, Y are not valid. Use absolute mouse position.
    GetCursorPos(MousePos);
    StartSplitterMove(False,MousePos);

    Offset := CalcOffset(MousePos.X, MousePos.Y);
    if Offset = 0 then Exit;
      
    ClientControl := FindClientControl;
    
    StartSize := 1;
    if not fAutoSnap
    then Inc(StartSize, Max(fMinSize, GetControlConstraintsMinSize(AlignControl)));
    if StartSize > 1 then Dec(StartSize);

    if Assigned(ClientControl)
    then EndSize := GetControlSize(AlignControl)
                  + GetControlSize(ClientControl)
                  - fMinSize
    else begin
      VirtualOppositeControl := FindVirtualOppositeControl(Self);
      LastControl := FindLastControl(Self);
      case Self.Align of
        alLeft, alTop: begin
            if Assigned(VirtualOppositeControl) then
              begin
                EndSize := GetControlSize(AlignControl)
                         + (GetControlMinPos(VirtualOppositeControl)
                           - (GetControlMinPos(LastControl)
                              + GetControlSize(LastControl)));
              end
            else EndSize := GetControlSize(AlignControl)
                          + (GetParentClientSize
                             - GetControlMinPos(LastControl)
                             - GetControlSize(LastControl))
          end;

        alRight, alBottom: begin
            if Assigned(VirtualOppositeControl) then
                EndSize := GetControlSize(AlignControl)
                         + (GetControlMinPos(LastControl)
                            - (GetControlMinPos(VirtualOppositeControl)
                               + GetControlSize(VirtualOppositeControl)))
            else EndSize := GetControlSize(AlignControl)
                          + GetControlMinPos(LastControl);
          end;
      end;  // case Self.Align
    end;

    NewSize := CalcNewSize(StartSize, EndSize, Offset);

    // OnCanResize event
    if CheckNewSize(NewSize)
    then SetAlignControlSize(NewSize);
  end;
end;

procedure TCustomSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if not (Self.Align in [alLeft,alTop,alRight,alBottom]) then Exit;
  if FSplitDragging then begin
    if Assigned(OnMoved) then OnMoved(Self);
    FSplitDragging := False;
  end;
end;

function TCustomSplitter.FindAlignControl: TControl;
var
  i: Integer;
  CurControl: TControl;
begin
  Result := nil;
  if (Parent = nil) then Exit;
  for i := Parent.ControlCount-1 downto 0 do begin
    CurControl := Parent.Controls[i];
    if (CurControl <> Self)
       and (CurControl.Visible)
       and (CurControl.Align = Self.Align)
    then begin
      case Self.Align of
        alLeft:
          if (CurControl.Height = Self.Height)
             and (CurControl.Left+CurControl.Width = Self.Left)
          then Result := CurControl;
        alTop:
          if (CurControl.Width = Self.Width)
             and (CurControl.Top+CurControl.Height = Self.Top)
          then Result := CurControl;
        alRight:
          if (CurControl.Height = Self.Height)
             and (CurControl.Left-Self.Width = Self.Left)
          then Result := CurControl;
        alBottom:
          if (CurControl.Width = Self.Width)
             and (CurControl.Top-Self.Height = Self.Top)
          then Result := CurControl;
      end;  // case Self.Align
      if Assigned(Result) then Break;
    end;
  end;
end;

procedure TCustomSplitter.SetAlign(Value: TAlign);
var
  OldAlign: TAlign;
  OldWidth: Integer;
  OldHeight: Integer;
begin
  if (Self.Align = Value)
     or (not (Value in [alLeft,alTop,alRight,alBottom]))
  then Exit;
  OldWidth := Self.Width;
  OldHeight := Self.Height;
  OldAlign := Self.Align;
  inherited SetAlign(Value);
  if Self.Align in [alLeft,alRight]
  then Self.Cursor := crHSplit
  else Self.Cursor := crVSplit;
  CheckAlignment;

  if (OldAlign in [alLeft,alRight])=(Align in [alLeft,alRight]) then begin
    // keep width and height
    SetBounds(Left,Top,OldWidth,OldHeight);
  end else begin
    // switch Width and Height
    SetBounds(Left,Top,OldHeight,OldWidth);
  end;
end;

procedure TCustomSplitter.SetAnchors(const AValue: TAnchors);
begin
  if AValue = Anchors then exit;
  inherited SetAnchors(AValue);
  CheckAlignment;
end;

procedure TCustomSplitter.CheckAlignment;
begin
  case Align of
    alLeft:   Anchors:=Anchors-[akRight]+[akLeft];
    alTop:    Anchors:=Anchors-[akBottom]+[akTop];
    alRight:  Anchors:=Anchors+[akRight]-[akLeft];
    alBottom: Anchors:=Anchors+[akBottom]-[akTop];
  end;
end;

function TCustomSplitter.CheckNewSize(var NewSize: Integer): Boolean;
begin
  Result := True;
  if Assigned(OnCanResize) then
    OnCanResize(Self,NewSize,Result);
end;

procedure TCustomSplitter.Paint;
begin
  DrawSplitter(Canvas.Handle,Rect(0,0,Width,Height),Align in [alTop,alBottom]);
end;

constructor TCustomSplitter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FResizeStyle:=rsUpdate;
  fAutoSnap:=true;
  FBeveled:=false;
  FMinSize:=30;
  Align:=alLeft;
  Width:=5;
end;

// included by extctrls.pp

