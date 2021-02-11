{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Michael W. Vogel

 Anchor grips:         1
                0 +----+----+ 2
                  |         |
                7 +         + 3
                  |         |
                6 +----+----+ 4
                       5
}

unit DockedAnchorGrip;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils, math,
  // LCL
  Controls, ExtCtrls, Graphics;

type

  { TAnchorGrip }

  TAnchorGrip = class(TPanel)
  private
    FShape: TShape;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TAnchorGrips }

  TAnchorGrips = class
  private const
    GRIP_SIZE = 8;
  private
    FBackGround: TWinControl;
    FGrip: array[0..7] of TAnchorGrip;
    FGripSize: Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FParent: TWinControl;
    function  CalculateBestRect(AControl: TControl): TRect;
    function  GetGrip(AIndex: Integer): TAnchorGrip;
    procedure InitGrip(AGrip: TAnchorGrip; ACursor: TCursor);
    procedure SetOnMouseDown(AValue: TMouseEvent);
    procedure SetOnMouseMove(AValue: TMouseMoveEvent);
    procedure SetOnMouseUp(AValue: TMouseEvent);
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    procedure AdjustGrips(AControl: TControl);
    procedure BringToFront;
    procedure Hide;
    procedure SetBounds(ARect: TRect);
  public
    property BackGround: TWinControl read FBackGround write FBackGround;
    property GripSize: Integer read FGripSize;
    property GripTopLeft:      TAnchorGrip index 0 read GetGrip;
    property GripTopCenter:    TAnchorGrip index 1 read GetGrip;
    property GripTopRight:     TAnchorGrip index 2 read GetGrip;
    property GripCenterRight:  TAnchorGrip index 3 read GetGrip;
    property GripBottomRight:  TAnchorGrip index 4 read GetGrip;
    property GripBottomCenter: TAnchorGrip index 5 read GetGrip;
    property GripBottomLeft:   TAnchorGrip index 6 read GetGrip;
    property GripCenterLeft:   TAnchorGrip index 7 read GetGrip;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
  end;

implementation

{ TAnchorGrip }

constructor TAnchorGrip.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BevelOuter := bvNone;
  Color := clBlack;
  SetInitialBounds(0, 0, 8, 8);

  FShape := TShape.Create(Self);
  FShape.Align := alClient;
  FShape.Brush.Color := clBtnFace;
  FShape.Enabled := False;
  FShape.Parent := Self;
end;

{ TAnchorGrips }

function TAnchorGrips.CalculateBestRect(AControl: TControl): TRect;
var
  LTopLeft, LBottomRight: TPoint;
  LGripOffset: Integer;
begin
  Result := Rect(0, 0, BackGround.Width, BackGround.Height);
  if not Assigned(AControl) then Exit;
  if AControl = BackGround then Exit;

  // grip in middle of rect border is default, if to small, use dynamic offset
  LGripOffset := Max(GripSize div 2, (GripSize * 10 - AControl.Width) div 10);
  LTopLeft.x := -LGripOffset;
  LBottomRight.x := AControl.Width + LGripOffset;

  LGripOffset := Max(GripSize div 2, (GripSize * 10 - AControl.Height) div 10);
  LTopLeft.y := -LGripOffset;
  LBottomRight.y := AControl.Height + LGripOffset;

  LTopLeft     := AControl.ClientToParent(LTopLeft, BackGround);
  LBottomRight := AControl.ClientToParent(LBottomRight, BackGround);
  LTopLeft.x := Max(LTopLeft.x, 0);
  LTopLeft.y := Max(LTopLeft.y, 0);
  LBottomRight.x := Min(LBottomRight.x, BackGround.Width);
  LBottomRight.y := Min(LBottomRight.y, BackGround.Height);
  Result := Rect(LTopLeft.x, LTopLeft.y, LBottomRight.x, LBottomRight.y);
end;

function TAnchorGrips.GetGrip(AIndex: Integer): TAnchorGrip;
begin
  Result := FGrip[AIndex];
end;

procedure TAnchorGrips.InitGrip(AGrip: TAnchorGrip; ACursor: TCursor);
begin
  AGrip.Parent := FParent;
  AGrip.Cursor := ACursor;
end;

procedure TAnchorGrips.SetOnMouseDown(AValue: TMouseEvent);
var
  i: Integer;
begin
  if FOnMouseDown = AValue then Exit;
  FOnMouseDown := AValue;
  for i := 0 to 7 do
    FGrip[i].OnMouseDown := AValue;
end;

procedure TAnchorGrips.SetOnMouseMove(AValue: TMouseMoveEvent);
var
  i: Integer;
begin
  if FOnMouseMove = AValue then Exit;
  FOnMouseMove := AValue;
  for i := 0 to 7 do
    FGrip[i].OnMouseMove := AValue;
end;

procedure TAnchorGrips.SetOnMouseUp(AValue: TMouseEvent);
var
  i: Integer;
begin
  if FOnMouseUp = AValue then Exit;
  FOnMouseUp := AValue;
  for i := 0 to 7 do
    FGrip[i].OnMouseUp := AValue;
end;

constructor TAnchorGrips.Create(AParent: TWinControl);
var
  i: Integer;
begin
  FParent := AParent;
  FGripSize := ScaleX(GRIP_SIZE, 96);

  for i := 0 to 7 do
    FGrip[i] := TAnchorGrip.Create(nil);

  // on mac there is no cursor for crNWSE ( https://bugs.freepascal.org/view.php?id=32194#c101876 )
  InitGrip(GripTopLeft,      {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNWSE{$ENDIF});
  InitGrip(GripTopCenter,    crSizeNS);
  InitGrip(GripTopRight,     {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNESW{$ENDIF});
  InitGrip(GripCenterRight,  crSizeWE);
  InitGrip(GripBottomRight,  {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNWSE{$ENDIF});
  InitGrip(GripBottomCenter, crSizeNS);
  InitGrip(GripBottomLeft,   {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNESW{$ENDIF});
  InitGrip(GripCenterLeft,   crSizeWE);
end;

destructor TAnchorGrips.Destroy;
var
  i: Integer;
begin
  for i := 7 downto 0 do
    FGrip[i].Free;
  inherited Destroy;
end;

procedure TAnchorGrips.AdjustGrips(AControl: TControl);
var
  LRect: TRect;
begin
  if not Assigned(AControl) then
  begin
    Hide;
    Exit;
  end;

  AControl.BringToFront;
  LRect := CalculateBestRect(AControl);
  SetBounds(LRect);
  BringToFront;
end;

procedure TAnchorGrips.BringToFront;
var
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    FGrip[i].Visible := True;
    FGrip[i].BringToFront;
  end;
end;

procedure TAnchorGrips.Hide;
var
  i: Integer;
begin
  for i := 0 to 7 do
    FGrip[i].Visible := False;
end;

procedure TAnchorGrips.SetBounds(ARect: TRect);
begin
  if not Assigned(FParent) then Exit;

  FGrip[0].SetBounds(ARect.Left, ARect.Top, GripSize, GripSize);
  FGrip[1].SetBounds((ARect.Left + ARect.Right - GripSize) div 2, ARect.Top, GripSize, GripSize);
  FGrip[2].SetBounds(ARect.Right - GripSize, ARect.Top, GripSize, GripSize);
  FGrip[3].SetBounds(ARect.Right - GripSize, (ARect.Top + ARect.Bottom - GripSize) div 2, GripSize, GripSize);
  FGrip[4].SetBounds(ARect.Right - GripSize, ARect.Bottom - GripSize, GripSize, GripSize);
  FGrip[5].SetBounds((ARect.Left + ARect.Right - GripSize) div 2, ARect.Bottom - GripSize, GripSize, GripSize);
  FGrip[6].SetBounds(ARect.Left, ARect.Bottom - GripSize, GripSize, GripSize);
  FGrip[7].SetBounds(ARect.Left, (ARect.Top + ARect.Bottom - GripSize) div 2, GripSize, GripSize);
end;

end.

