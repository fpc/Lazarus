{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Authors: Michael W. Vogel

}

unit DockedAnchorDesigner;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils, math, Types,
  // LCL
  LCLProc, Controls, LCLIntf, Forms, ExtCtrls, Graphics, Dialogs, Menus,
  // IDEIntf
  PropEdits,
  // DockedFormEditor
  DockedDesignForm, DockedBasicAnchorDesigner, DockedAnchorControl,
  DockedOptionsIDE, DockedAnchorGrip, DockedStrConsts;

type

  { TAnchorDesigner }

  TAnchorDesigner = class(TBasicAnchorDesigner)
  private
    FAnchorBarWidth: Integer;
    FAnchorControls: TAnchorControls;
    FAnchorGrips: TAnchorGrips;
    FBackGround: TAnchorControl;
    FBorderControl: TBorderControl;
    FCurrentBounds: TRect;
    FDesignControl: TWinControl;
    FDesignForm: TDesignForm;
    FMenuAttachDetach: TMenuItem;
    FMousePos: TPoint;
    FParent: TWinControl;
    FPopupMenu: TPopupMenu;
    FPopupAnchors: TAnchors;
    FPreviousControl: TAnchorControl;
    FSelectedControl: TAnchorControl;
    FState: TAnchorStates;
    FTargetControl: TAnchorControl;
    FTargetHorzSide: TAnchorSideReference;
    FTargetVertSide: TAnchorSideReference;
    procedure AdjustGrips;
    procedure AnchorControlMouseDown(Sender: TObject; {%H-}Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure AnchorControlMouseMove(Sender: TObject; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure AnchorControlMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure AnchorControlPaint(Sender: TObject);
    procedure AnchorGripMouseDown(Sender: TObject; {%H-}Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure AnchorGripMouseMove(Sender: TObject; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure AnchorGripMouseUp(Sender: TObject; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BringToFront(AControl: TAnchorControl);
    function  CalculateRect(AControl: TControl): TRect;
    procedure CreateAnchorControl(AControl: TControl; AParent: TWinControl);
    procedure CreateAnchorGrips;
    procedure CreateBackGround;
    procedure CreatePopupMenu;
    function  FindNearestControl(APoint: TPoint): TAnchorControl;
    function  FindAnchorHorzSide(ARect: TRect; ARootPoint: TPoint; out Side: TAnchorSideReference): Boolean;
    function  FindAnchorVertSide(ARect: TRect; ARootPoint: TPoint; out Side: TAnchorSideReference): Boolean;
    procedure MenuAttachDetachClick(Sender: TObject);
    function  MouseStartMoving: Boolean;
    function  MouseOffset: TPoint;
    procedure PopupMenuAdapt(Sender: TObject);
    procedure SelectedAdaptAnchors;
    procedure SelectedAdaptBorder;
    procedure SelectedAdaptBounds(var ALeft, ATop, AWidth, AHeight: Integer);
    procedure SelectedAnchorNewTarget(AKind: TAnchorKind);
    procedure SetSelectedControl(AValue: TAnchorControl);
  public
    constructor Create(ADesignForm: TDesignForm; AParent: TWinControl);
    destructor Destroy; override;
    procedure Abort; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure Invalidate; override;
    function  IsFocused: Boolean; override;
    procedure Refresh; override;
  public
    property SelectedControl: TAnchorControl read FSelectedControl write SetSelectedControl;
    property State: TAnchorStates read FState write FState;
  end;

implementation

{ TAnchorDesigner }

procedure TAnchorDesigner.AdjustGrips;
var
  LRect: TRect;
begin
  if Assigned(FSelectedControl) and (not State.IsMoving or State.IsBordering) then
  begin
    LRect := FSelectedControl.BoundsRect;
    FBorderControl.Parent := FSelectedControl.Parent;
    FBorderControl.SetBounds(
      LRect.Left   - FSelectedControl.BorderSpacing.Around - FSelectedControl.BorderSpacing.Left,
      LRect.Top    - FSelectedControl.BorderSpacing.Around - FSelectedControl.BorderSpacing.Top,
      LRect.Width  + FSelectedControl.BorderSpacing.Around * 2 + FSelectedControl.BorderSpacing.Left + FSelectedControl.BorderSpacing.Right,
      LRect.Height + FSelectedControl.BorderSpacing.Around * 2 + FSelectedControl.BorderSpacing.Top  + FSelectedControl.BorderSpacing.Bottom);
    FBorderControl.Visible := True;
    FBorderControl.BringToFront;
    FSelectedControl.BringToFront;
  end else
    FBorderControl.Visible := False;
  FAnchorGrips.AdjustGrips(FSelectedControl);
end;

procedure TAnchorDesigner.AnchorControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PopupMenuAdapt(Sender);
  if not (State = []) then Exit;
  if Assigned(OnDesignerSetFocus) then OnDesignerSetFocus();
  if Sender = FBackGround then
  begin
    SelectedControl := nil;
    Invalidate;
    Exit;
  end;
  SelectedControl := TAnchorControl(Sender);
  Invalidate;
  if not (Shift = [ssLeft]) and not (Shift = [ssCtrl, ssLeft]) then Exit;
  GetCursorPos(FMousePos);
  Include(FState, asMouseDown);
  {$IF Defined(LCLWin32) or Defined(LCLWin64)}
  SetCapture(TWinControl(Sender).Handle);
  {$ENDIF}
end;

procedure TAnchorDesigner.AnchorControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LMouseOffset: TPoint;
begin
  if not State.IsMouseDown then Exit;
  if not (Shift = [ssLeft]) and not (Shift = [ssCtrl, ssLeft]) then Exit;
  if not State.IsMoving then
  begin
    if ssCtrl in Shift then
      Include(FState,  asBordering);
    if not State.IsBordering and not MouseStartMoving then Exit;
    Include(FState, asMoving);
    FPreviousControl := TAnchorControl.Create(FSelectedControl.Parent, FSelectedControl.RootControl);
    FPreviousControl.Visible := False;
    FPreviousControl.AssignAnchors(FSelectedControl);
    if not State.IsBordering then
    begin
      FSelectedControl.RemoveAnchorSides;
      FBackGround.Invalidate;
    end;
  end;

  if State.IsBordering then
  begin
    // borderspacing
    SelectedAdaptBorder;
  end else begin
    LMouseOffset := MouseOffset;
    // sizing
    FSelectedControl.SetBounds(
      FPreviousControl.Left + LMouseOffset.x,
      FPreviousControl.Top  + LMouseOffset.y,
      FPreviousControl.Width,
      FPreviousControl.Height);
  end;
  AdjustGrips;
end;

procedure TAnchorDesigner.AnchorControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not State.IsMouseDown then Exit;
  Exclude(FState, asMouseDown);

  if not State.IsMoving then
  begin
    GlobalDesignHook.SelectOnlyThis(TAnchorControl(Sender).RootControl);
    Exit;
  end;
  Exclude(FState, asMoving);

  if not FSelectedControl.AnchorsValid then
  begin
    Abort;
    Exit;
  end;
  FSelectedControl.AssignToRoot_Full;
  FreeAndNil(FPreviousControl);

  Screen.Cursor := crDefault;
  {$IF Defined(LCLWin32) or Defined(LCLWin64)}
  ReleaseCapture;
  {$ENDIF}

  FState := [];
  GlobalDesignHook.Modified(Self, 'Anchors');
  GlobalDesignHook.SelectOnlyThis(TAnchorControl(Sender).RootControl);
end;

procedure TAnchorDesigner.AnchorControlPaint(Sender: TObject);
begin
  TAnchorControl(Sender).TargetAnchorSidesGet(FSelectedControl);
end;

procedure TAnchorDesigner.AnchorGripMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PopupMenuAdapt(Sender);
  if Assigned(OnDesignerSetFocus) then OnDesignerSetFocus();
  if not (Shift = [ssLeft]) and not (Shift = [ssCtrl, ssLeft]) then Exit;
  GetCursorPos(FMousePos);
  Include(FState, asMouseDown);
  {$IF Defined(LCLWin32) or Defined(LCLWin64)}
  SetCapture(TWinControl(Sender).Handle);
  {$ENDIF}
end;

procedure TAnchorDesigner.AnchorGripMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  LAnchoring: Boolean;
  LMousePos: TPoint;
  LRect: TRect;
  LTargetControl: TAnchorControl;
  LLeft, LWidth, LTop, LHeight: Integer;
begin
  if not State.IsMouseDown then Exit;
  if not (Shift = [ssLeft]) and not (Shift = [ssCtrl, ssLeft]) then Exit;
  if not State.IsMoving then
  begin
    if ssCtrl in Shift then
      Include(FState, asBordering);
    Include(FState, asMoving);
    FPreviousControl := TAnchorControl.Create(FSelectedControl.Parent, FSelectedControl.RootControl);
    FPreviousControl.Visible := False;
    FPreviousControl.AssignAnchors(FSelectedControl);
  end;

  if      (Sender = FAnchorGrips.GripTopLeft)      then FState := FState + [asAnchorTop, asAnchorLeft]
  else if (Sender = FAnchorGrips.GripTopCenter)    then FState := FState + [asAnchorTop]
  else if (Sender = FAnchorGrips.GripTopRight)     then FState := FState + [asAnchorTop, asAnchorRight]
  else if (Sender = FAnchorGrips.GripCenterRight)  then FState := FState + [asAnchorRight]
  else if (Sender = FAnchorGrips.GripBottomRight)  then FState := FState + [asAnchorBottom, asAnchorRight]
  else if (Sender = FAnchorGrips.GripBottomCenter) then FState := FState + [asAnchorBottom]
  else if (Sender = FAnchorGrips.GripBottomLeft)   then FState := FState + [asAnchorBottom, asAnchorLeft]
  else if (Sender = FAnchorGrips.GripCenterLeft)   then FState := FState + [asAnchorLeft]
  else
    Exit;

  if State.IsBordering then
  begin
    // borderspacing
    SelectedAdaptBorder;
  end else begin
    // anchoring or sizing
    LMousePos := Point(0, 0);
    GetCursorPos(LMousePos);
    LMousePos := FBackGround.ScreenToClient(LMousePos);
    LTargetControl := FindNearestControl(LMousePos);
    if Assigned(FTargetControl) and (FTargetControl <> LTargetControl) then
      FTargetControl.Color := DockedOptions.AnchorControlColor;
    FTargetControl := LTargetControl;

    LRect := CalculateRect(FTargetControl);
    LAnchoring := Assigned(FTargetControl);
    if State.IsAnchoringHorz and LAnchoring then
      LAnchoring := FindAnchorHorzSide(LRect, LMousePos, FTargetHorzSide);
    if State.IsAnchoringVert and LAnchoring then
      LAnchoring := FindAnchorVertSide(LRect, LMousePos, FTargetVertSide);

    LRect := FPreviousControl.BoundsRect;
    LLeft   := LRect.Left;
    LWidth  := LRect.Width;
    LTop    := LRect.Top;
    LHeight := LRect.Height;

    if LAnchoring then
    begin
      // use Anchors
      FSelectedControl.SetBounds(LLeft, LTop, LWidth, LHeight);
      SelectedAdaptAnchors;
      FTargetControl.Color := DockedOptions.AnchorTargetColor;
    end else begin
      // size control
      SelectedAdaptBounds(LLeft, LTop, LWidth, LHeight);
      FSelectedControl.SetBounds(LLeft, LTop, LWidth, LHeight);
      FState := [asMouseDown, asMoving];
      FTargetControl.Color := DockedOptions.AnchorControlColor;
      FTargetControl := nil;
    end;
  end;
  AdjustGrips;
end;

procedure TAnchorDesigner.AnchorGripMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not State.IsMouseDown then Exit;
  Exclude(FState, asMouseDown);

  if not State.IsMoving then Exit;
  Exclude(FState, asMoving);

  if not FSelectedControl.AnchorsValid then
  begin
    Abort;
    Exit;
  end;
  FSelectedControl.AssignToRoot_Anchors;
  FSelectedControl.AssignToRoot_ControlsBounds(True);

  if Assigned(FTargetControl) then FTargetControl.Color := DockedOptions.AnchorControlColor;
  FTargetControl := nil;
  FreeAndNil(FPreviousControl);

  Screen.Cursor := crDefault;
  {$IF Defined(LCLWin32) or Defined(LCLWin64)}
  ReleaseCapture;
  {$ENDIF}

  FState := [];
  GlobalDesignHook.Modified(Self, 'Anchors');
end;

procedure TAnchorDesigner.BringToFront(AControl: TAnchorControl);
begin
  if not Assigned(AControl) then Exit;
  if AControl = FBackGround then Exit;
  AControl.BringToFront;
  BringToFront(TAnchorControl(AControl.Parent));
end;

function TAnchorDesigner.CalculateRect(AControl: TControl): TRect;
var
  LTopLeft, LBottomRight: TPoint;
begin
  Result := FBackGround.ClientRect;
  if not Assigned(AControl) then Exit;
  if AControl = FBackGround then Exit;

  LTopLeft     := AControl.ClientToParent(Point(0, 0), FBackGround);
  LBottomRight := AControl.ClientToParent(Point(AControl.Width, AControl.Height), FBackGround);
  Result := Rect(LTopLeft.x, LTopLeft.y, LBottomRight.x, LBottomRight.y)
end;

procedure TAnchorDesigner.CreateAnchorControl(AControl: TControl; AParent: TWinControl);
var
  LAnchorControl: TAnchorControl;
  LWinControl: TWinControl;
  i, LIndex: Integer;
begin
  LIndex := FAnchorControls.IndexOf(AControl);
  if LIndex >= 0 then
  begin
    LAnchorControl := FAnchorControls[LIndex];
  end else begin
    LAnchorControl := TAnchorControl.Create(AParent, AControl);
    LAnchorControl.OnMouseDown := @AnchorControlMouseDown;
    LAnchorControl.OnMouseMove := @AnchorControlMouseMove;
    LAnchorControl.OnMouseUp   := @AnchorControlMouseUp;
    LAnchorControl.OnPaint     := @AnchorControlPaint;
    LAnchorControl.PopupMenu   := FPopupMenu;
    FAnchorControls.Add(LAnchorControl);
  end;
  LAnchorControl.Validate;
  LAnchorControl.Visible := True;
  if AControl is TWinControl then
  begin
    LWinControl := TWinControl(AControl);
    for i := 0 to LWinControl.ControlCount - 1 do
      CreateAnchorControl(LWinControl.Controls[i], LAnchorControl);
  end;
  if AControl = FDesignForm.SelectedControl then
    SelectedControl := LAnchorControl;
end;

procedure TAnchorDesigner.CreateAnchorGrips;
begin
  FAnchorGrips := TAnchorGrips.Create(FParent);
  FAnchorGrips.BackGround := FBackGround;
  FAnchorGrips.OnMouseDown := @AnchorGripMouseDown;
  FAnchorGrips.OnMouseMove := @AnchorGripMouseMove;
  FAnchorGrips.OnMouseUp := @AnchorGripMouseUp;
  FAnchorGrips.Hide;
end;

procedure TAnchorDesigner.CreateBackGround;
begin
  FBackGround := TAnchorControl.Create(FParent, FDesignControl);
  FBackGround.BevelOuter := bvNone;
  FBackGround.Align := alClient;
  FBackGround.Name := 'AnchorBackGround';
  FBackGround.Parent := FParent;
  FBackGround.OnMouseDown := @AnchorControlMouseDown;
  FBackGround.OnPaint := @AnchorControlPaint;
  FBackGround.OnShowHint := nil;
  FAnchorControls.Add(FBackGround);
end;

procedure TAnchorDesigner.CreatePopupMenu;
begin
  FPopupMenu := TPopupMenu.Create(FBackGround);
  FAnchorGrips.PopupMenu := FPopupMenu;
  FMenuAttachDetach := TMenuItem.Create(FPopupMenu);
  FMenuAttachDetach.OnClick := @MenuAttachDetachClick;
  FPopupMenu.Items.Add(FMenuAttachDetach);
end;

function TAnchorDesigner.FindNearestControl(APoint: TPoint): TAnchorControl;
var
  LRect: TRect;
  LBestDist, LDist: TPoint;
  i: Integer;
begin
  if SelectedControl.Parent is TAnchorControl then
    Result := TAnchorControl(SelectedControl.Parent)
  else
    Exit(nil);

  LRect := CalculateRect(FSelectedControl.Parent);
  LBestDist.x := MinIntValue([abs(APoint.x - LRect.Left), abs(APoint.x - LRect.Right),  DockedOptions.CaptureDistance]);
  LBestDist.y := MinIntValue([abs(APoint.y - LRect.Top),  abs(APoint.y - LRect.Bottom), DockedOptions.CaptureDistance]);

  for i := 0 to FSelectedControl.Parent.ControlCount - 1 do
  begin
    if (FSelectedControl.Parent.Controls[i] = FSelectedControl)
    or (FSelectedControl.Parent.Controls[i] = FPreviousControl)
    or (FSelectedControl.Parent.Controls[i] = FBorderControl) then
      Continue;
    LRect := CalculateRect(FSelectedControl.Parent.Controls[i]);

    if (APoint.x >= LRect.Left) and (APoint.x <= LRect.Right) then
      LDist.x := 0
    else
      LDist.x := Min(abs(APoint.x - LRect.Left), abs(APoint.x - LRect.Right));

    if (APoint.y >= LRect.Top) and (APoint.y <= LRect.Bottom) then
      LDist.y := 0
    else
      LDist.y := Min(abs(APoint.y - LRect.Top),  abs(APoint.y - LRect.Bottom));

    if LDist.x + LDist.y < LBestDist.x + LBestDist.y then
    begin
      LBestDist := LDist;
      Result := TAnchorControl(FSelectedControl.Parent.Controls[i]);
    end;
  end;
end;

function TAnchorDesigner.FindAnchorHorzSide(ARect: TRect; ARootPoint: TPoint;
  out Side: TAnchorSideReference): Boolean;
var
  LDistLeft, LDistRight, LDistCenter: Integer;
begin
  Result := False;
  Side := asrTop;
  if not State.IsAnchoringHorz then Exit;

  LDistLeft   := abs(ARect.Left - ARootPoint.x);
  LDistRight  := abs(ARect.Right - ARootPoint.x);
  LDistCenter := abs((ARect.Left + ARect.Right) div 2 - ARootPoint.x);

  // if other side is center anchored, skip center as target prevent endless sizing loop
  if (State.IsAnchoringLeft  and (FSelectedControl.AnchorSide[akRight].Side = asrCenter))
  or (State.IsAnchoringRight and (FSelectedControl.AnchorSide[akLeft].Side  = asrCenter)) then
    LDistCenter := MaxInt;

  if FTargetControl = FSelectedControl.Parent then
    if State.IsAnchoringLeft then
      LDistRight := MaxInt
    else
      LDistLeft := MaxInt;

  if (LDistRight < LDistLeft) and (LDistRight < LDistCenter) then
  begin
    Side := asrBottom;
    if LDistRight > DockedOptions.CaptureDistance then Exit;
    Result := True;
  end;
  if not Result and (LDistCenter < LDistLeft) then
  begin
    Side := asrCenter;
    if LDistCenter > DockedOptions.CaptureDistance then Exit;
    Result := True;
  end;
  if not Result and (LDistLeft > DockedOptions.CaptureDistance) then Exit;

  if State.IsAnchoringLeft then
    Result := FSelectedControl.AnchorValid(akLeft, FTargetControl, Side)
  else
    Result := FSelectedControl.AnchorValid(akRight, FTargetControl, Side);
end;

function TAnchorDesigner.FindAnchorVertSide(ARect: TRect; ARootPoint: TPoint;
  out Side: TAnchorSideReference): Boolean;
var
  LDistTop, LDistBottom, LDistCenter: Integer;
begin
  Result := False;
  Side := asrTop;
  if not State.IsAnchoringVert then Exit;

  LDistTop    := abs(ARect.Top - ARootPoint.y);
  LDistBottom := abs(ARect.Bottom - ARootPoint.y);
  LDistCenter := abs((ARect.Top + ARect.Bottom) div 2 - ARootPoint.y);

  // if other side is center anchored, skip center as target prevent endless sizing loop
  if (State.IsAnchoringTop    and (FSelectedControl.AnchorSide[akBottom].Side = asrCenter))
  or (State.IsAnchoringBottom and (FSelectedControl.AnchorSide[akTop].Side    = asrCenter)) then
    LDistCenter := MaxInt;

  if FTargetControl = FSelectedControl.Parent then
    if State.IsAnchoringTop then
      LDistBottom := MaxInt
    else
      LDistTop := MaxInt;

  if (LDistBottom < LDistTop) and (LDistBottom < LDistCenter) then
  begin
    Side := asrBottom;
    if LDistBottom > DockedOptions.CaptureDistance then Exit;
    Result := True;
  end;
  if not Result and (LDistCenter < LDistTop) then
  begin
    Side := asrCenter;
    if LDistCenter > DockedOptions.CaptureDistance then Exit;
    Result := True;
  end;
  if not Result and (LDistTop > DockedOptions.CaptureDistance) then Exit;

  if State.IsAnchoringTop then
    Result := FSelectedControl.AnchorValid(akTop, FTargetControl, Side)
  else
    Result := FSelectedControl.AnchorValid(akBottom, FTargetControl, Side);
end;

procedure TAnchorDesigner.MenuAttachDetachClick(Sender: TObject);
var
  LKind: TAnchorKind;
begin
  case TAttachDetach(FMenuAttachDetach.Tag) of
    adAttachControl:
      begin
        FSelectedControl.RemoveAnchorSides;
        FSelectedControl.Anchors := [akLeft, akTop];
      end;
    adDetachControl:
      FSelectedControl.RemoveAnchorSides;
    adAttachPoint, adAttachSide:
      for LKind := akTop to akBottom do
      begin
        if [LKind] * FPopupAnchors = [] then Continue;
        FSelectedControl.RemoveAnchorSide(LKind);
        FSelectedControl.Anchors := FSelectedControl.Anchors + [LKind];
      end;
    adDetachPoint, adDetachSide:
      for LKind := akTop to akBottom do
      begin
        if [LKind] * FPopupAnchors = [] then Continue;
        FSelectedControl.RemoveAnchorSide(LKind);
      end;
  end;

  FSelectedControl.AssignToRoot_Anchors;
  FState := [];
  GlobalDesignHook.Modified(Self, 'Anchors');
  GlobalDesignHook.SelectOnlyThis(FSelectedControl.RootControl);
end;

function TAnchorDesigner.MouseStartMoving: Boolean;
var
  LMousePos: TPoint;
begin
  LMousePos := MouseOffset;
  Result := Max(abs(LMousePos.x), abs(LMousePos.y)) > DockedOptions.CaptureDistance;
end;

function TAnchorDesigner.MouseOffset: TPoint;
begin
  Result := Point(0, 0);
  GetCursorPos(Result);
  Result := Result - FMousePos;
end;

procedure TAnchorDesigner.PopupMenuAdapt(Sender: TObject);
var
  LAttachDetach: TAttachDetach;
begin
  FPopupAnchors := [];
  if Sender is TAnchorControl then
  begin
    if TAnchorControl(Sender).Anchors = [] then
      LAttachDetach := adAttachControl
    else
      LAttachDetach := adDetachControl;
  end else
  if Sender = FAnchorGrips.GripTopLeft then
  begin
    if FSelectedControl.Anchors * [akLeft, akTop] = [] then
      LAttachDetach := adAttachPoint
    else
      LAttachDetach := adDetachPoint;
    FPopupAnchors := FPopupAnchors + [akLeft, akTop];
  end else
  if Sender = FAnchorGrips.GripTopCenter then
  begin
    if FSelectedControl.Anchors * [akTop] = [] then
      LAttachDetach := adAttachSide
    else
      LAttachDetach := adDetachSide;
    FPopupAnchors := FPopupAnchors + [akTop];
  end else
  if Sender = FAnchorGrips.GripTopRight then
  begin
    if FSelectedControl.Anchors * [akTop, akRight] = [] then
      LAttachDetach := adAttachPoint
    else
      LAttachDetach := adDetachPoint;
    FPopupAnchors := FPopupAnchors + [akTop, akRight];
  end else
  if Sender = FAnchorGrips.GripCenterRight then
  begin
    if FSelectedControl.Anchors * [akRight] = [] then
      LAttachDetach := adAttachSide
    else
      LAttachDetach := adDetachSide;
    FPopupAnchors := FPopupAnchors + [akRight];
  end else
  if Sender = FAnchorGrips.GripBottomRight then
  begin
    if FSelectedControl.Anchors * [akBottom, akRight] = [] then
      LAttachDetach := adAttachPoint
    else
      LAttachDetach := adDetachPoint;
    FPopupAnchors := FPopupAnchors + [akBottom, akRight];
  end else
  if Sender = FAnchorGrips.GripBottomCenter then
  begin
    if FSelectedControl.Anchors * [akBottom] = [] then
      LAttachDetach := adAttachSide
    else
      LAttachDetach := adDetachSide;
    FPopupAnchors := FPopupAnchors + [akBottom];
  end else
  if Sender = FAnchorGrips.GripBottomLeft then
  begin
    if FSelectedControl.Anchors * [akBottom, akLeft] = [] then
      LAttachDetach := adAttachPoint
    else
      LAttachDetach := adDetachPoint;
    FPopupAnchors := FPopupAnchors + [akBottom, akLeft];
  end else
  if Sender = FAnchorGrips.GripCenterLeft then
  begin
    if FSelectedControl.Anchors * [akLeft] = [] then
      LAttachDetach := adAttachSide
    else
      LAttachDetach := adDetachSide;
    FPopupAnchors := FPopupAnchors + [akLeft];
  end else
    raise Exception.Create('TAnchorDesigner.PopupMenuAdapt: Wrong Sender');
  FMenuAttachDetach.Caption := AttachDetachStr[LAttachDetach];
  FMenuAttachDetach.Tag     := ord(LAttachDetach);
end;

procedure TAnchorDesigner.SelectedAdaptAnchors;
begin
  if State.IsAnchoringHorz then
  begin
    if State.IsAnchoringLeft then
    begin
      SelectedAnchorNewTarget(akLeft);
      FSelectedControl.AssignAnchor(FPreviousControl, akRight);
    end else begin
      SelectedAnchorNewTarget(akRight);
      FSelectedControl.AssignAnchor(FPreviousControl, akLeft);
    end;
  end else begin
    FSelectedControl.AssignAnchor(FPreviousControl, akLeft);
    FSelectedControl.AssignAnchor(FPreviousControl, akRight);
  end;

  if State.IsAnchoringVert then
  begin
    if State.IsAnchoringTop then
    begin
      SelectedAnchorNewTarget(akTop);
      FSelectedControl.AssignAnchor(FPreviousControl, akBottom);
    end else begin
      SelectedAnchorNewTarget(akBottom);
      FSelectedControl.AssignAnchor(FPreviousControl, akTop);
    end;
  end else begin
    FSelectedControl.AssignAnchor(FPreviousControl, akTop);
    FSelectedControl.AssignAnchor(FPreviousControl, akBottom);
  end;
end;

procedure TAnchorDesigner.SelectedAdaptBorder;
var
  LMouseOffset: TPoint;
  LLeftBorder, LRightBorder, LTopBorder, LBottomBorder, LAroundBorder: TSpacingSize;
begin
  LMouseOffset := MouseOffset;
  LMouseOffset.x := LMouseOffset.x div DockedOptions.MouseBorderFactor;
  LMouseOffset.y := LMouseOffset.y div DockedOptions.MouseBorderFactor;

  if not DockedOptions.TreatBorder and not State.IsAnchoring then
  begin
    FSelectedControl.BorderSpacing.Around :=
      Max(0, FPreviousControl.BorderSpacing.Around + LMouseOffset.x + LMouseOffset.y);
    Exit;
  end;

  LAroundBorder := FPreviousControl.BorderSpacing.Around;
  LLeftBorder   := FPreviousControl.BorderSpacing.Left;
  LRightBorder  := FPreviousControl.BorderSpacing.Right;
  LTopBorder    := FPreviousControl.BorderSpacing.Top;
  LBottomBorder := FPreviousControl.BorderSpacing.Bottom;

  if DockedOptions.TreatBorder then
  begin
    LLeftBorder   := LLeftBorder   + LAroundBorder;
    LRightBorder  := LRightBorder  + LAroundBorder;
    LTopBorder    := LTopBorder    + LAroundBorder;
    LBottomBorder := LBottomBorder + LAroundBorder;
  end;

  if State.IsAnchoring then
  begin
    if State.IsAnchoringLeft   then LLeftBorder   := Max(0, LLeftBorder   + LMouseOffset.x);
    if State.IsAnchoringRight  then LRightBorder  := Max(0, LRightBorder  - LMouseOffset.x);
    if State.IsAnchoringTop    then LTopBorder    := Max(0, LTopBorder    + LMouseOffset.y);
    if State.IsAnchoringBottom then LBottomBorder := Max(0, LBottomBorder - LMouseOffset.y);
  end else begin
    LLeftBorder   := Max(0, LLeftBorder   + LMouseOffset.x);
    LRightBorder  := Max(0, LRightBorder  + LMouseOffset.x);
    LTopBorder    := Max(0, LTopBorder    + LMouseOffset.y);
    LBottomBorder := Max(0, LBottomBorder + LMouseOffset.y);
  end;

  if DockedOptions.TreatBorder then
  begin
    if State.IsAnchoring then
      LAroundBorder := MinIntValue([LLeftBorder, LRightBorder, LTopBorder, LBottomBorder, LAroundBorder])
    else
      LAroundBorder := MinIntValue([LLeftBorder, LRightBorder, LTopBorder, LBottomBorder]);

    LLeftBorder   := LLeftBorder   - LAroundBorder;
    LRightBorder  := LRightBorder  - LAroundBorder;
    LTopBorder    := LTopBorder    - LAroundBorder;
    LBottomBorder := LBottomBorder - LAroundBorder;
  end;

  FSelectedControl.BorderSpacing.Around := Max(0, LAroundBorder);
  FSelectedControl.BorderSpacing.Left   := Max(0, LLeftBorder);
  FSelectedControl.BorderSpacing.Right  := Max(0, LRightBorder);
  FSelectedControl.BorderSpacing.Top    := Max(0, LTopBorder);
  FSelectedControl.BorderSpacing.Bottom := Max(0, LBottomBorder);
end;

procedure TAnchorDesigner.SelectedAdaptBounds(var ALeft, ATop, AWidth,
  AHeight: Integer);
var
  LMouseOffset: TPoint;
begin
  LMouseOffset := MouseOffset;

  if State.IsAnchoringHorz then
  begin
    if State.IsAnchoringLeft then
    begin
      ALeft  := ALeft  + LMouseOffset.x;
      AWidth := AWidth - LMouseOffset.x;
      FSelectedControl.RemoveAnchorSide(akLeft);
      FSelectedControl.AssignAnchor(FPreviousControl, akRight);
    end else begin
      AWidth := AWidth + LMouseOffset.x;
      FSelectedControl.RemoveAnchorSide(akRight);
      FSelectedControl.AssignAnchor(FPreviousControl, akLeft);
    end;
  end else begin
    FSelectedControl.AssignAnchor(FPreviousControl, akLeft);
    FSelectedControl.AssignAnchor(FPreviousControl, akRight);
  end;

  if State.IsAnchoringVert then
  begin
    if State.IsAnchoringTop then
    begin
      ATop  := ATop  + LMouseOffset.y;
      AHeight := AHeight - LMouseOffset.y;
      FSelectedControl.RemoveAnchorSide(akTop);
      FSelectedControl.AssignAnchor(FPreviousControl, akBottom);
    end else begin
      AHeight := AHeight + LMouseOffset.y;
      FSelectedControl.RemoveAnchorSide(akBottom);
      FSelectedControl.AssignAnchor(FPreviousControl, akTop);
    end;
  end else begin
    FSelectedControl.AssignAnchor(FPreviousControl, akTop);
    FSelectedControl.AssignAnchor(FPreviousControl, akBottom);
  end;

  if FSelectedControl.RootControl.Constraints.MaxWidth > 0 then
    AWidth := Min(AWidth, FSelectedControl.RootControl.Constraints.MaxWidth);
  if FSelectedControl.RootControl.Constraints.MinWidth > 0 then
    AWidth := Max(AWidth, FSelectedControl.RootControl.Constraints.MinWidth);
  if FSelectedControl.RootControl.Constraints.MaxHeight > 0 then
    AHeight := Min(AHeight, FSelectedControl.RootControl.Constraints.MaxHeight);
  if FSelectedControl.RootControl.Constraints.MinHeight > 0 then
    AHeight := Max(AHeight, FSelectedControl.RootControl.Constraints.MinHeight);
end;

procedure TAnchorDesigner.SelectedAnchorNewTarget(AKind: TAnchorKind);
begin
  FSelectedControl.AnchorSide[AKind].Control := FTargetControl;
  if AKind in [akLeft, akRight] then
    FSelectedControl.AnchorSide[AKind].Side := FTargetHorzSide
  else
    FSelectedControl.AnchorSide[AKind].Side := FTargetVertSide;
  FSelectedControl.Anchors := FSelectedControl.Anchors + [AKind];
end;

procedure TAnchorDesigner.SetSelectedControl(AValue: TAnchorControl);
begin
  if FSelectedControl = AValue then Exit;
  if Assigned(FSelectedControl) then
    FSelectedControl.State := [acsNone];
  FSelectedControl := AValue;
  if Assigned(FSelectedControl) then
  begin
    FSelectedControl.State := [acsSelected];
    BringToFront(FSelectedControl);
  end;
end;

constructor TAnchorDesigner.Create(ADesignForm: TDesignForm; AParent: TWinControl);
begin
  FState := [];
  FDesignForm := ADesignForm;
  FParent := AParent;
  FAnchorBarWidth := -1;
  FDesignControl := FDesignForm.DesignWinControl;
  FAnchorControls := TAnchorControls.Create;
  FBorderControl := TBorderControl.Create(nil);
  CreateBackGround;
  CreateAnchorGrips;
  CreatePopupMenu;
end;

destructor TAnchorDesigner.Destroy;
begin
  FBorderControl.Free;
  FAnchorGrips.Free;
  FAnchorControls.Free;
  inherited Destroy;
end;

procedure TAnchorDesigner.Abort;
begin
  if Assigned(FTargetControl) then FTargetControl.Color := DockedOptions.AnchorControlColor;
  FTargetControl := nil;
  if Assigned(FPreviousControl) then
  begin
    FSelectedControl.AssignFull(FPreviousControl);
    FreeAndNil(FPreviousControl);
  end;
  Screen.Cursor := crDefault;
  {$IF Defined(LCLWin32) or Defined(LCLWin64)}
  if asMouseDown in State then
    ReleaseCapture;
  {$ENDIF}
  FState := [];
  Invalidate;
end;

procedure TAnchorDesigner.BeginUpdate;
begin
  if not FParent.Visible then Exit;
  FAnchorGrips.Hide;
  FBorderControl.Visible := False;
  FAnchorControls.BeginUpdate;
  FBackGround.Invalidate;
end;

procedure TAnchorDesigner.EndUpdate;
begin
  if not FParent.Visible then Exit;
  FAnchorControls.EndUpdate;
  if (FBackGround.Height <> FCurrentBounds.Height)
  or (FBackGround.Width <> FCurrentBounds.Width) then
    FBackGround.AssignToRoot_ControlsBounds(False);
end;

procedure TAnchorDesigner.Invalidate;
begin
  AdjustGrips;
  FBackGround.Invalidate;
end;

function TAnchorDesigner.IsFocused: Boolean;
begin
  if Assigned(IsFocusedFunc) then
    Result := IsFocusedFunc()
  else
    Result := False;
end;

procedure TAnchorDesigner.Refresh;
var
  i: Integer;
begin
  FCurrentBounds := FDesignControl.BoundsRect;
  SelectedControl := nil;
  FAnchorControls.Invalid;
  for i := 0 to FDesignControl.ControlCount - 1 do
    CreateAnchorControl(FDesignControl.Controls[i], FBackGround);
  FAnchorControls.CheckParents;
  FAnchorControls.RemoveInvalid;
  FAnchorControls.CheckProperties;
  FAnchorControls.CheckAnchors;
  Invalidate;
end;

end.

