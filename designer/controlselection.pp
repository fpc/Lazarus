{ /***************************************************************************
                             ControlSelection.pp   
                             -------------------
                         cointains selected controls.


                 Initial Revision  : Mon June 19 23:15:32 CST 2000


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit ControlSelection;

{$mode delphi}

interface

uses
  Classes, Controls, LCLLinux;

type
  TGrabberMoveEvent = procedure(Sender: TObject; dx, dy: Integer) of object;

  TGrabIndex = 0..7;

  TGrabPosition = (gpTop, gpBottom, gpLeft, gpRight);
  TGrabPositions = set of TGrabPosition;

  TGrabber = class(TWinControl)
  private
    FDragging: Boolean;
    FStart: TPoint;
    FPositions: TGrabPositions;
    FOnMove: TGrabberMoveEvent;
    FOnMoved: TGrabberMoveEvent;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Positions: TGrabPositions read FPositions write FPositions;
    property OnMove: TGrabberMoveEvent read FOnMove write FOnMove;
    property OnMoved: TGrabberMoveEvent read FOnMoved write FOnMoved;
  end;

  TControlSelection = class(TObject)
  private
    FDragging: Boolean;
    FStart: TPoint;
    FVisible: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FTop: Integer;
    FLeft: Integer;
    FGrabbers: array[TGrabIndex] of TGrabber;
    FControlList: TList;
    FOnChange: TNotifyEvent;
    procedure AdjustSize(AControl: TControl; Initial: Boolean);
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DOChange;
    procedure SetGrabbers;
    procedure SizeContent(dx, dy: Integer);
    procedure MoveContent(dx, dy: Integer);
    procedure SetVisible(const Value: Boolean);
    procedure GrabberMove(Sender: TObject; dx, dy: Integer);
    procedure GrabberMoved(Sender: TObject; dx, dy: Integer);
  protected
  public
    procedure Add(AControl: TControl);
    procedure Clear;
    constructor Create(AOwner: TWinControl); virtual;
    destructor Destroy; override;
    function IsSelected(AControl: TControl): Boolean;
    procedure Remove(AControl: TControl);
    property Visible: Boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;


implementation

uses
  Sysutils, Math;

type
  TSelectControl = class(TControl)//;
  end;

const
  GRAB_SIZE = 8;

  GRAB_CURSOR: array[TGrabIndex] of TCursor = (
    crSizeNWSE, crSizeNS, crSizeNESW,
    crSizeWE,             crSizeWE,
    crSizeNESW, crSizeNS, crSizeNWSE
  );

  GRAB_POSITIONS:  array [TGrabIndex] of TGrabPositions = (
    [gpLeft, gpTop   ], [gpTop   ], [gpTop,    gpRight],
    [gpLeft          ],             [          gpRight],
    [gpLeft, gpBottom], [gpBottom], [gpBottom, gpRight]
  );


{ TGrabber }

constructor TGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCustomPaint];
  FDragging := False;
end;

procedure TGrabber.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft
  then begin
    FStart := Point(X, Y);
    FDragging := True;
  end;
end;

procedure TGrabber.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging
  then begin
    if [gpLeft, gpRight] * Positions <> []
    then dx := X - FStart.X
    else dx := 0;
    if [gpTop, gpBottom] * Positions <> []
    then dy := Y - FStart.Y
    else dy := 0;

    if Assigned(FOnMove) then FOnMove(Self, dx, dy);
  end;
end;

procedure TGrabber.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button = mbLeft) and FDragging
  then begin
    FDragging := False;

    if [gpLeft, gpRight] * Positions <> []
    then dx := X - FStart.X
    else dx := 0;
    if [gpTop, gpBottom] * Positions <> []
    then dy := Y - FStart.Y
    else dy := 0;

    if Assigned(FOnMoved) then FOnMoved(Self, dx, dy);
  end;
end;

procedure TGrabber.PaintWindow(DC: HDC);
begin
  FillRect(DC, Rect(0, 0, Width, Height), GetStockObject(BLACK_BRUSH));
end;

{ TControlSelection }

procedure TControlSelection.Add(AControl: TControl);
begin
  if AControl <> nil
  then begin
    WriteLn(Format('[TControlSelection.Add] %s', [AControl.ClassName]));

    FControlList.Add(AControl);
    AdjustSize(Acontrol, FControlList.Count = 1);
    FVisible := True;
    SetGrabbers;
    with TSelectControl(AControl) do
    begin
      OnMouseDown := ControlMouseDown;
      OnMouseMove := ControlMouseMove;
      OnMouseUp := ControlMouseUp;
    end;
    DoChange;
  end;
end;

procedure TControlSelection.AdjustSize(AControl: TControl; Initial: Boolean);
var
  n: Integer;
begin
  if AControl <> nil
  then with AControl do
  begin
    if Initial
    then begin
      FLeft := Left;
      FTop := Top;
      FWidth := Width;
      FHeight := Height;
    end
    else begin
      n := FLeft - Left;
      if n > 0
      then begin
        FLeft := Left;
        Inc(FWidth , n);
      end;

      n := FTop - Top;
      if n > 0
      then begin
        FTop := Top;
        Inc(FHeight, n);
      end;

      n := Max(FLeft + FWidth, Left + Width);
      FWidth := n - FLeft;

      n := Max(FTop + FHeight, Top + Height);
      FHeight := n - FTop;
    end;
  end;
end;

procedure TControlSelection.Clear;
var
  n:  Integer;
begin
  with FControlList do
  begin
    for n := 0 to Count -1 do
      with TSelectControl(Items[n]) do
      begin
        OnMouseDown := nil;
        OnMouseMove := nil;
        OnMouseUp := nil;
      end;

    Clear;
  end;
  FWidth := 0;
  FHeight := 0;
  FVisible := False;
  SetGrabbers;
  DoChange;
end;

procedure TControlSelection.ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft
  then begin
    FStart := Point(X, Y);
    FDragging := True;
  end;
end;

procedure TControlSelection.ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FDragging
  then begin
    Inc(FLeft, X - FStart.X);
    Inc(FTop, Y - FStart.Y);
    SetGrabbers;
    MoveContent(X - FStart.X, Y - FStart.Y);
  end;
end;

procedure TControlSelection.ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and FDragging
  then begin
    FDragging := False;
    MoveContent(X - FStart.X, Y - FStart.Y);
  end;
end;

constructor TControlSelection.Create(AOwner: TWinControl);
var
  GrabPos: TGrabIndex;
begin
  inherited Create;

  FWidth := 0;
  FHeight := 0;
  FLeft := 0;
  FTop := 0;
  FVisible := False;

  FDragging := False;

  FControlList := TList.Create;

  for GrabPos := Low(TGrabIndex) to High(TGrabIndex) do
  begin
    FGrabbers[GrabPos] := TGrabber.Create(AOwner);
    with FGrabbers[GrabPos] do
    begin
      Parent := AOwner;
      Width := GRAB_SIZE;
      Height := GRAB_SIZE;
      Visible := False;
      Cursor := GRAB_CURSOR[GrabPos];
      Positions := GRAB_POSITIONS[GrabPos];
      OnMove := GrabberMove;
      OnMoved := GrabberMoved;
    end;
  end;
end;

destructor TControlSelection.Destroy;
begin
  Clear;
  FControlList.Free;
  inherited Destroy;
end;

procedure TControlSelection.DOChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TControlSelection.GrabberMove(Sender: TObject; dx, dy: Integer);
begin
  with TGrabber(Sender) do
  begin
    if gpLeft in Positions
    then begin
      Inc(FLeft, dx);
      Dec(FWidth, dx);
    end;
    if gpRight in Positions then Inc(FWidth, dx);

    if gpTop in Positions
    then begin
       Inc(FTop, dy);
       Dec(FHeight, dy)
    end;
    if gpBottom in Positions then Inc(FHeight, dy);
  end;
  SetGrabbers;
end;

procedure TControlSelection.GrabberMoved(Sender: TObject; dx, dy: Integer);
begin
  SizeContent(dx, dy);
end;

function TControlSelection.IsSelected(AControl: TControl): Boolean;
begin
  Result := FControlList.IndexOf(AControl) <> -1;
end;

procedure TControlSelection.MoveContent(dx, dy: Integer);
var
  n: Integer;
begin
  with FControlList do
    for n := 0 to Count -1 do
      with TControl(Items[n])  do
        SetBounds(Left + dx, Top + dy, Width, Height);
end;

procedure TControlSelection.Remove(AControl: TControl);
var
  n: Integer;
begin
  with FControlList do
    if (Remove(AControl) <> -1)
    then begin
      if Count > 0
      then begin
        for n := 0 to Count -1 do AdjustSize(Items[n], n = 0);
      end
      else FVisible := False;
      SetGrabbers;

      with TSelectControl(AControl) do
      begin
        OnMouseDown := nil;
        OnMouseMove := nil;
        OnMouseUp := nil;
      end;
      FDragging := False;
      DoChange;
    end;
end;

procedure TControlSelection.SetGrabbers;
var
  GrabPos: TGrabIndex;
begin
  for GrabPos := Low(TGrabIndex) to High(TGrabIndex) do
  with FGrabbers[GrabPos] do
  begin
    if FVisible
    then begin
      if gpLeft in Positions
      then Left := FLeft - GRAB_SIZE
      else if gpRight in Positions
        then Left := FLeft + FWidth
        else Left := FLeft + (FWidth - GRAB_SIZE) div 2;

      if gpTop in Positions
      then Top := FTop - GRAB_SIZE
      else if gpBottom in Positions
        then Top := FTop + FHeight
        else Top := FTop + (FHeight - GRAB_SIZE) div 2;
    end;

    Visible := FVisible;
  end;
end;

procedure TControlSelection.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value
  then begin
    FVisible := Value;
    SetGrabbers;
  end;
end;

procedure TControlSelection.SizeContent(dx, dy: Integer);
begin
  if FControlList.Count = 1 then
  begin
    TControl(FControlList[0]).SetBounds(FLeft, FTop, FWidth, FHeight);
  end;
end;

end.
