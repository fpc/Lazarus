// included by extctrls.pp

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
    - AutoSnap
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

procedure TCustomSplitter.StartSplitterMove(Restart: boolean; const MouseXY: TPoint);
begin
  if (not Restart) and FSplitDragging then exit;
  FSplitDragging:=true;
  fSplitterStartXY:=MouseXY;
end;

procedure TCustomSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  StartSplitterMove(true,Point(X,Y));
end;

procedure TCustomSplitter.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Offset: Integer;
  MinLeft: Integer;
  MaxLeft: Integer;
  NewLeft: Integer;
  AlignControl: TControl;
  MinTop: Integer;
  MaxTop: Integer;
  NewTop: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if (ssLeft in Shift) and (Parent<>nil) then begin
    StartSplitterMove(false,Point(X,Y));
    if Align in [alLeft,alRight] then begin
      Offset:=X-fSplitterStartXY.X;
      if Offset=0 then exit;
      MinLeft:=0;
      MaxLeft:=Parent.ClientWidth-Width;
      AlignControl:=FindAlignControl;
      if AlignControl<>nil then
        if Align=alLeft then
          MinLeft:=AlignControl.Left+MinSize
        else
          MaxLeft:=Parent.ClientWidth-Width-MinSize;
      NewLeft:=Left+Offset;
      if NewLeft>MaxLeft then NewLeft:=MaxLeft;
      if NewLeft<MinLeft then NewLeft:=MinLeft;
      AlignControl:=FindAlignControl;
      if not CheckNewSize(NewLeft) then exit;
      if AlignControl<>nil then begin
        if Align=alLeft then
          AlignControl.Width:=NewLeft-AlignControl.Left
        else
          AlignControl.Left:=NewLeft+Width;
      end else
        Left:=NewLeft;
    end else begin
      Offset:=Y-fSplitterStartXY.Y;
      if Offset=0 then exit;
      MinTop:=0;
      MaxTop:=Parent.ClientHeight-Height;
      AlignControl:=FindAlignControl;
      if AlignControl<>nil then
        if Align=alTop then
          MinTop:=AlignControl.Top+MinSize
        else
          MaxTop:=Parent.ClientHeight-Height-MinSize;
      NewTop:=Top+Offset;
      if NewTop>MaxTop then NewTop:=MaxTop;
      if NewTop<MinTop then NewTop:=MinTop;
      AlignControl:=FindAlignControl;
      if not CheckNewSize(NewLeft) then exit;
      if AlignControl<>nil then begin
        if Align=alTop then
          AlignControl.Height:=NewTop-AlignControl.Top
        else
          AlignControl.Top:=NewTop+Height;
      end else
        Top:=NewTop;
    end;
  end;
end;

procedure TCustomSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FSplitDragging then begin
    if Assigned(OnMoved) then OnMoved(Self);
    FSplitDragging:=false;
  end;
end;

function TCustomSplitter.FindAlignControl: TControl;
var
  i: Integer;
  CurControl: TControl;
begin
  Result:=nil;
  if (Parent=nil) then exit;
  for i:=Parent.ControlCount-1 downto 0 do begin
    CurControl:=Parent.Controls[i];
    if (CurControl.Align in ([alClient,Align]))
    and (CurControl.Visible)
    and (((Align=alLeft) and (CurControl.Left<Left))
      or ((Align=alTop) and (CurControl.Top<Top))
      or ((Align=alRight) and (CurControl.Left>Left))
      or ((Align=alBottom) and (CurControl.Top>Top)))
    then begin
      // candidate found
      if (Result=nil)
      or (((Align=alLeft) and (CurControl.Left>Result.Left))
       or ((Align=alTop) and (CurControl.Top>Result.Top))
       or ((Align=alRight) and (CurControl.Left<Result.Left))
       or ((Align=alBottom) and (CurControl.Top<Result.Top)))
      then
        // better candidate found
        Result:=CurControl;
    end;
  end;
end;

procedure TCustomSplitter.SetAlign(Value: TAlign);
var
  OldAlign: TAlign;
  OldWidth: Integer;
  OldHeight: Integer;
begin
  if (Align=Value) or (not (Value in [alLeft,alTop,alRight,alBottom])) then
    exit;
  OldWidth:=Width;
  OldHeight:=Height;
  OldAlign:=Align;
  inherited SetAlign(Value);
  CheckAlignment;
  if Align in [alLeft,alRight] then begin
    Cursor:=crHSplit;
  end else begin
    Cursor:=crVSplit;
  end;
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
  if AValue=Anchors then exit;
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

function TCustomSplitter.CheckNewSize(var NewSize: integer): boolean;
begin
  Result:=true;
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

