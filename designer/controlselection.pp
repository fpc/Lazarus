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
    procedure SizeContent;
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
//  WriteLn(Format('[TGrabber.PaintWindow] 0x%x', [DC]));
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
  then begin
    if Initial
    then begin
      FLeft := AControl.Left;
      FTop := AControl.Top;
      FWidth := AControl.Width;
      FHeight := AControl.Height;
      WriteLn(Format('[TControlSelection.AdjustSize] Initializing to X:%d, Y:%d, W:%d, H: %d', [FLeft, FTop, FWidth, FHeight])); 
    end
    else begin
      WriteLn(Format('[TControlSelection.AdjustSize] current X:%d, Y:%d, W:%d, H: %d', [FLeft, FTop, FWidth, FHeight])); 
      with AControl do
        WriteLn(Format('[TControlSelection.AdjustSize] Adjust for %s --> X:%d, Y:%d, W:%d, H: %d', [Classname, Left, Top, Width, Height])); 
      n := FLeft - AControl.Left;
      if n > 0
      then begin
        FLeft := AControl.Left;
        Inc(FWidth , n);
      end;

      n := FTop - AControl.Top;
      if n > 0
      then begin
        FTop := AControl.Top;
        Inc(FHeight, n);
      end;

      n := Max(FLeft + FWidth, AControl.Left + AControl.Width);
      FWidth := n - FLeft;

      n := Max(FTop + FHeight, AControl.Top + AControl.Height);
      FHeight := n - FTop;
      WriteLn(Format('[TControlSelection.AdjustSize] Adjusted to X:%d, Y:%d, W:%d, H: %d', [FLeft, FTop, FWidth, FHeight])); 
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
    WriteLN(Format('[TControlSelection.Create] Create grabber %d',  [Ord(GrabPos)]));
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
  if gpLeft in TGrabber(Sender).Positions
  then begin
    Inc(FLeft, dx);
    Dec(FWidth, dx);
  end;
  if gpRight in TGrabber(Sender).Positions then Inc(FWidth, dx);

  if gpTop in TGrabber(Sender).Positions
  then begin
     Inc(FTop, dy);
     Dec(FHeight, dy)
  end;
  if gpBottom in TGrabber(Sender).Positions then Inc(FHeight, dy);

  SetGrabbers;
end;

procedure TControlSelection.GrabberMoved(Sender: TObject; dx, dy: Integer);
begin
  SizeContent;
end;

function TControlSelection.IsSelected(AControl: TControl): Boolean;
begin
  WriteLn(Format('[TControlSelection.IsSelected] %s --> index %d', [AControl.ClassName, FControlList.IndexOf(AControl)]));
  Result := FControlList.IndexOf(AControl) <> -1;
end;

procedure TControlSelection.MoveContent(dx, dy: Integer);
var
  n: Integer;
begin
  with FControlList do
    for n := 0 to Count -1 do
      with TControl(Items[n]) do
      begin
        Left := Left + dx;
        Top := Top + dy;
      end;
end;

procedure TControlSelection.Remove(AControl: TControl);
var
  n: Integer;
begin
  with AControl do
     WriteLn(Format('[TControlSelection.AdjustSize] Remove %s --> X:%d, Y:%d, W:%d, H: %d', [Classname, Left, Top, Width, Height])); 

  if (FControlList.Remove(AControl) <> -1)
  then begin
    if FControlList.Count > 0
    then begin
      for n := 0 to FControlList.Count - 1 do AdjustSize(FControlList[n], n = 0);
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
  Grabber: TGrabber;
begin
  WriteLn(Format('[TControlSelection.SetGrabbers] Selection --> X:%d, Y:%d, W:%d, H:%d', [FLeft, FTop, FWidth, FHeight])); 
  for GrabPos := Low(TGrabIndex) to High(TGrabIndex) do
  begin
    Grabber := FGrabbers[GrabPos]; 
    if FVisible
    then begin
      Write(Format('[TControlSelection.SetGrabbers] Setting grabber %d --> ',  [Ord(GrabPos)]));
      if gpLeft in Grabber.Positions
      then begin
        Write('Left,   ');
        Grabber.Left := FLeft - GRAB_SIZE
      end
      else begin
        if gpRight in Grabber.Positions
        then begin
          Write('Right,  ');
          Grabber.Left := FLeft + FWidth
        end
        else begin
          Write('Center, ');
          Grabber.Left := FLeft + (FWidth - GRAB_SIZE) div 2;
        end;
      end;

      if gpTop in Grabber.Positions
      then begin
        Write('Top    ');
        Grabber.Top := FTop - GRAB_SIZE
      end
      else begin
        if gpBottom in Grabber.Positions
        then begin
          Write('Bottom ');
          Grabber.Top := FTop + FHeight
        end
        else begin
          Write('Center ');
          Grabber.Top := FTop + (FHeight - GRAB_SIZE) div 2;
        end
      end;
      
      WriteLN(Format('X:%d, Y:%d',  [Grabber.Left, Grabber.Top]));
    end;
    Grabber.Visible := FVisible;

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

procedure TControlSelection.SizeContent;
begin
  if FControlList.Count = 1 then
  begin
    TControl(FControlList[0]).SetBounds(FLeft, FTop, FWidth, FHeight);
  end;
end;

end.
