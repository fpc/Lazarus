{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

 Author: Michael W. Vogel

 Grips:                 1
                0 +-----+-----+ 2
                  |           |
                7 +           + 3
                  |           |
                6 +-----+-----+ 4
                        5

 ResizeGrips:  Only grips 3, 4, and 5 are used for sizing

 ResizeBars:         0     1
                  +-----+-----+
                7 |           | 2
                  +           +
                6 |           | 3
                  +-----+-----+
                     5     4
 Only bars 2, 3, 4, and 5 are used for sizing

 ResizeContainer is just a container for without any logic.
 Logic is added in ResizeControl

}

unit DockedGrip;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils, math,
  // LCL
  Controls, ExtCtrls, Graphics, Menus;

type

  { TGrip }

  TGrip = class(TPanel)
  private
    FActivated: Boolean;
    FShape: TShape;
    procedure SetActivated(AValue: Boolean);
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property Activated: Boolean read FActivated write SetActivated;
  end;

  { TBar }

  TBar = class(TPanel)
  private
    FActivated: Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property Activated: Boolean read FActivated write FActivated;
  end;

  { TCustomGrips }

  TCustomGrips = class
  private const
    GRIP_SIZE = 8;
  private
    FGrip: array[0..7] of TGrip;
    FGripSize: Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FParent: TWinControl;
    function  GetGrip(AIndex: Integer): TGrip;
    function  GetPopupMenu: TPopupMenu;
    procedure InitGrip(AGrip: TGrip; ACursor: TCursor; Activated: Boolean);
    procedure SetOnMouseDown(AValue: TMouseEvent);
    procedure SetOnMouseMove(AValue: TMouseMoveEvent);
    procedure SetOnMouseUp(AValue: TMouseEvent);
    procedure SetParent(AValue: TWinControl);
    procedure SetPopupMenu(AValue: TPopupMenu);
  protected
    procedure InitGrips; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BringToFront;
    procedure Hide;
    procedure SetBounds(ARect: TRect);
  public
    property GripSize: Integer read FGripSize;
    property GripTopLeft:      TGrip index 0 read GetGrip;
    property GripTopCenter:    TGrip index 1 read GetGrip;
    property GripTopRight:     TGrip index 2 read GetGrip;
    property GripCenterRight:  TGrip index 3 read GetGrip;
    property GripBottomRight:  TGrip index 4 read GetGrip;
    property GripBottomCenter: TGrip index 5 read GetGrip;
    property GripBottomLeft:   TGrip index 6 read GetGrip;
    property GripCenterLeft:   TGrip index 7 read GetGrip;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    property Parent: TWinControl read FParent write SetParent;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
  end;

  { TAnchorGrips }

  TAnchorGrips = class(TCustomGrips)
  private
    FBackGround: TWinControl;
    function  CalculateRect(AControl: TControl): TRect;
  protected
    procedure InitGrips; override;
  public
    procedure AdjustGrips(AControl: TControl);
  public
    property BackGround: TWinControl read FBackGround write FBackGround;
  end;

  { TResizeGrips }

  TResizeGrips = class(TCustomGrips)
  protected
    procedure InitGrips; override;
  end;

  { TResizeBars }

  TResizeBars = class
  private const
    BAR_SIZE = 8;
  private
    FBar: array[0..7] of TBar;
    FBarSize: Integer;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnPaint: TNotifyEvent;
    FParent: TWinControl;
    function  GetBar(AIndex: Integer): TBar;
    function  GetPopupMenu: TPopupMenu;
    procedure InitBar(ABar: TBar; ACursor: TCursor; Activated: Boolean);
    procedure SetOnMouseDown(AValue: TMouseEvent);
    procedure SetOnMouseMove(AValue: TMouseMoveEvent);
    procedure SetOnMouseUp(AValue: TMouseEvent);
    procedure SetOnPaint(AValue: TNotifyEvent);
    procedure SetParent(AValue: TWinControl);
    procedure SetPopupMenu(AValue: TPopupMenu);
  protected
    procedure InitBars;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetBounds(ARect: TRect);
  public
    property BarSize: Integer read FBarSize;
    property BarTopLeft:     TBar index 0 read GetBar;
    property BarTopRight:    TBar index 1 read GetBar;
    property BarRightTop:    TBar index 2 read GetBar;
    property BarRightBottom: TBar index 3 read GetBar;
    property BarBottomRight: TBar index 4 read GetBar;
    property BarBottomLeft:  TBar index 5 read GetBar;
    property BarLeftBottom:  TBar index 6 read GetBar;
    property BarLeftTop:     TBar index 7 read GetBar;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write SetOnPaint;
    property Parent: TWinControl read FParent write SetParent;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
  end;

  { TResizeContainer }

  TResizeContainer = class(TComponent)
  private
    FAnchorContainer: TWinControl;
    FBoundsRect: TRect;
    FFakeMenu: TCustomControl;
    FFormClient: TWinControl;
    FFormContainer: TWinControl;
    FParent: TWinControl;
    FResizeBars: TResizeBars;
    FResizeGrips: TResizeGrips;
  public
    constructor Create(AWinControl: TWinControl); reintroduce;
    destructor Destroy; override;
    function  IsHorzSizer(AObject: TObject): Boolean;
    function  IsVertSizer(AObject: TObject): Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
  public
    property AnchorContainer: TWinControl read FAnchorContainer;
    property BoundsRect: TRect read FBoundsRect;
    property FakeMenu: TCustomControl read FFakeMenu;
    property FormClient: TWinControl read FFormClient;
    property FormContainer: TWinControl read FFormContainer;
    property Parent: TWinControl read FParent;
    property ResizeBars: TResizeBars read FResizeBars;
    property ResizeGrips: TResizeGrips read FResizeGrips;
  end;

implementation

{ TGrip }

procedure TGrip.SetActivated(AValue: Boolean);
begin
  if FActivated = AValue then Exit;
  FActivated := AValue;
  if AValue then
    FShape.Brush.Color := clBtnFace
  else
    FShape.Brush.Color := clGray;
end;

constructor TGrip.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BevelOuter := bvNone;
  Color := clBlack;
  SetInitialBounds(0, 0, 8, 8);
  FActivated := False;

  FShape := TShape.Create(Self);
  FShape.Align := alClient;
  FShape.Brush.Color := clGray;
  FShape.Enabled := False;
  FShape.Parent := Self;
end;

{ TBar }

constructor TBar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BevelOuter := bvNone;
  Color := clWindow;
  SetInitialBounds(0, 0, 8, 8);
  FActivated := False;
end;

{ TCustomGrips }

function TCustomGrips.GetGrip(AIndex: Integer): TGrip;
begin
  Result := FGrip[AIndex];
end;

function TCustomGrips.GetPopupMenu: TPopupMenu;
begin
  Result := FGrip[0].PopupMenu;
end;

procedure TCustomGrips.InitGrip(AGrip: TGrip; ACursor: TCursor; Activated: Boolean);
begin
  AGrip.Parent := FParent;
  AGrip.Cursor := ACursor;
  AGrip.Activated := Activated;
end;

procedure TCustomGrips.SetOnMouseDown(AValue: TMouseEvent);
var
  i: Integer;
begin
  if FOnMouseDown = AValue then Exit;
  FOnMouseDown := AValue;
  for i := 0 to 7 do
    if FGrip[i].Activated then
      FGrip[i].OnMouseDown := AValue;
end;

procedure TCustomGrips.SetOnMouseMove(AValue: TMouseMoveEvent);
var
  i: Integer;
begin
  if FOnMouseMove = AValue then Exit;
  FOnMouseMove := AValue;
  for i := 0 to 7 do
    if FGrip[i].Activated then
      FGrip[i].OnMouseMove := AValue;
end;

procedure TCustomGrips.SetOnMouseUp(AValue: TMouseEvent);
var
  i: Integer;
begin
  if FOnMouseUp = AValue then Exit;
  FOnMouseUp := AValue;
  for i := 0 to 7 do
    if FGrip[i].Activated then
      FGrip[i].OnMouseUp := AValue;
end;

procedure TCustomGrips.SetParent(AValue: TWinControl);
var
  i: Integer;
begin
  if FParent = AValue then Exit;
  FParent := AValue;
  for i := 0 to 7 do
    FGrip[i].Parent := AValue;
end;

procedure TCustomGrips.SetPopupMenu(AValue: TPopupMenu);
var
  i: Integer;
begin
  if GetPopupMenu = AValue then Exit;
  for i := 0 to 7 do
    if FGrip[i].Activated then
      FGrip[i].PopupMenu := AValue;
end;

constructor TCustomGrips.Create;
var
  i: Integer;
begin
  FGripSize := ScaleX(GRIP_SIZE, 96);
  for i := 0 to 7 do
    FGrip[i] := TGrip.Create(nil);
  InitGrips;
end;

destructor TCustomGrips.Destroy;
var
  i: Integer;
begin
  for i := 7 downto 0 do
    FGrip[i].Free;
  inherited Destroy;
end;

procedure TCustomGrips.BringToFront;
var
  i: Integer;
begin
  for i := 0 to 7 do
  begin
    FGrip[i].Visible := True;
    FGrip[i].BringToFront;
  end;
end;

procedure TCustomGrips.Hide;
var
  i: Integer;
begin
  for i := 0 to 7 do
    FGrip[i].Visible := False;
end;

procedure TCustomGrips.SetBounds(ARect: TRect);
var
  LMiddleLeft, LMiddleTop: Integer;
begin
  if not Assigned(FParent) then Exit;
  LMiddleLeft := (ARect.Left + ARect.Right - GripSize) div 2;
  LMiddleTop := (ARect.Top + ARect.Bottom - GripSize) div 2;
  FGrip[0].SetBounds(ARect.Left, ARect.Top, GripSize, GripSize);
  FGrip[1].SetBounds(LMiddleLeft, ARect.Top, GripSize, GripSize);
  FGrip[2].SetBounds(ARect.Right - GripSize, ARect.Top, GripSize, GripSize);
  FGrip[3].SetBounds(ARect.Right - GripSize, LMiddleTop, GripSize, GripSize);
  FGrip[4].SetBounds(ARect.Right - GripSize, ARect.Bottom - GripSize, GripSize, GripSize);
  FGrip[5].SetBounds(LMiddleLeft, ARect.Bottom - GripSize, GripSize, GripSize);
  FGrip[6].SetBounds(ARect.Left, ARect.Bottom - GripSize, GripSize, GripSize);
  FGrip[7].SetBounds(ARect.Left, LMiddleTop, GripSize, GripSize);
end;

{ TAnchorGrips }

function TAnchorGrips.CalculateRect(AControl: TControl): TRect;
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

procedure TAnchorGrips.InitGrips;
begin
  // on mac there is no cursor for crNWSE ( https://bugs.freepascal.org/view.php?id=32194#c101876 )
  InitGrip(GripTopLeft,      {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNWSE{$ENDIF}, True);
  InitGrip(GripTopCenter,    crSizeNS                                        , True);
  InitGrip(GripTopRight,     {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNESW{$ENDIF}, True);
  InitGrip(GripCenterRight,  crSizeWE                                        , True);
  InitGrip(GripBottomRight,  {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNWSE{$ENDIF}, True);
  InitGrip(GripBottomCenter, crSizeNS                                        , True);
  InitGrip(GripBottomLeft,   {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNESW{$ENDIF}, True);
  InitGrip(GripCenterLeft,   crSizeWE                                        , True);
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
  LRect := CalculateRect(AControl);
  SetBounds(LRect);
  BringToFront;
end;

{ TResizeGrips }

procedure TResizeGrips.InitGrips;
begin
  InitGrip(GripTopLeft,      crDefault                                       , False);
  InitGrip(GripTopCenter,    crDefault                                       , False);
  InitGrip(GripTopRight,     crDefault                                       , False);
  InitGrip(GripCenterRight,  crSizeWE                                        , True);
  InitGrip(GripBottomRight,  {$IFDEF MACOS}crSizeAll{$ELSE}crSizeNWSE{$ENDIF}, True);
  InitGrip(GripBottomCenter, crSizeNS                                        , True);
  InitGrip(GripBottomLeft,   crDefault                                       , False);
  InitGrip(GripCenterLeft,   crDefault                                       , False);
end;

{ TResizeBars }

function TResizeBars.GetBar(AIndex: Integer): TBar;
begin
  Result := FBar[AIndex];
end;

function TResizeBars.GetPopupMenu: TPopupMenu;
begin
  Result := FBar[0].PopupMenu;
end;

procedure TResizeBars.InitBar(ABar: TBar; ACursor: TCursor; Activated: Boolean);
begin
  ABar.Parent := FParent;
  ABar.Cursor := ACursor;
  ABar.Activated := Activated;
end;

procedure TResizeBars.SetOnMouseDown(AValue: TMouseEvent);
var
  i: Integer;
begin
  if FOnMouseDown = AValue then Exit;
  FOnMouseDown := AValue;
  for i := 0 to 7 do
    if FBar[i].Activated then
      FBar[i].OnMouseDown := AValue;
end;

procedure TResizeBars.SetOnMouseMove(AValue: TMouseMoveEvent);
var
  i: Integer;
begin
  if FOnMouseMove = AValue then Exit;
  FOnMouseMove := AValue;
  for i := 0 to 7 do
    if FBar[i].Activated then
      FBar[i].OnMouseMove := AValue;
end;

procedure TResizeBars.SetOnMouseUp(AValue: TMouseEvent);
var
  i: Integer;
begin
  if FOnMouseUp = AValue then Exit;
  FOnMouseUp := AValue;
  for i := 0 to 7 do
    if FBar[i].Activated then
      FBar[i].OnMouseUp := AValue;
end;

procedure TResizeBars.SetOnPaint(AValue: TNotifyEvent);
var
  i: Integer;
begin
  if FOnPaint = AValue then Exit;
  FOnPaint := AValue;
  for i := 0 to 7 do
    FBar[i].OnPaint := AValue;
end;

procedure TResizeBars.SetParent(AValue: TWinControl);
var
  i: Integer;
begin
  if FParent = AValue then Exit;
  FParent := AValue;
  for i := 0 to 7 do
    FBar[i].Parent := AValue;
end;

procedure TResizeBars.SetPopupMenu(AValue: TPopupMenu);
var
  i: Integer;
begin
  if GetPopupMenu = AValue then Exit;
  for i := 0 to 7 do
    if FBar[i].Activated then
      FBar[i].PopupMenu := AValue;
end;

procedure TResizeBars.InitBars;
begin
  InitBar(BarTopLeft,      crDefault, False);
  InitBar(BarTopRight,     crDefault, False);
  InitBar(BarRightTop,     crSizeWE,  True);
  InitBar(BarRightBottom,  crSizeWE,  True);
  InitBar(BarBottomRight,  crSizeNS,  True);
  InitBar(BarBottomLeft,   crSizeNS,  True);
  InitBar(BarLeftBottom,   crDefault, False);
  InitBar(BarLeftTop,      crDefault, False);
end;

constructor TResizeBars.Create;
var
  i: Integer;
begin
  FBarSize := ScaleX(BAR_SIZE, 96);
  for i := 0 to 7 do
    FBar[i] := TBar.Create(nil);
  InitBars;
end;

destructor TResizeBars.Destroy;
var
  i: Integer;
begin
  for i := 7 downto 0 do
    FBar[i].Free;
  inherited Destroy;
end;

procedure TResizeBars.SetBounds(ARect: TRect);
var
  LMiddleLeft, LMiddleTop: Integer;
begin
  if not Assigned(FParent) then Exit;
  LMiddleLeft := (ARect.Left + ARect.Right - BarSize) div 2;
  LMiddleTop := (ARect.Top + ARect.Bottom - BarSize) div 2;
  FBar[0].SetBounds(ARect.Left + BarSize, ARect.Top, LMiddleLeft - ARect.Left - BarSize, BarSize);
  FBar[1].SetBounds(LMiddleLeft + BarSize, ARect.Top, ARect.Right - LMiddleLeft - BarSize * 2, BarSize);
  FBar[2].SetBounds(ARect.Right - BarSize, ARect.Top + BarSize, BarSize, LMiddleTop - ARect.Top - BarSize);
  FBar[3].SetBounds(ARect.Right - BarSize, LMiddleTop + BarSize, BarSize, ARect.Bottom - LMiddleTop - BarSize * 2);
  FBar[4].SetBounds(LMiddleLeft + BarSize, ARect.Bottom - BarSize, ARect.Right - LMiddleLeft - BarSize * 2, BarSize);
  FBar[5].SetBounds(ARect.Left + BarSize, ARect.Bottom - BarSize, LMiddleLeft - ARect.Left - BarSize, BarSize);
  FBar[6].SetBounds(ARect.Left, LMiddleTop + BarSize, BarSize, ARect.Bottom - LMiddleTop - BarSize * 2);
  FBar[7].SetBounds(ARect.Left, ARect.Top + BarSize, BarSize, LMiddleTop - ARect.Top - BarSize);
end;

{ TResizeContainer }

constructor TResizeContainer.Create(AWinControl: TWinControl);
begin
  inherited Create(AWinControl);
  FParent := AWinControl;
  FBoundsRect := Rect(0, 0, 0, 0);

  FResizeGrips := TResizeGrips.Create;
  FResizeGrips.Parent := Parent;
  FResizeBars := TResizeBars.Create;
  FResizeBars.Parent := Parent;

  FFakeMenu := TCustomControl.Create(Parent);
  FFakeMenu.Height := 0;
  FFakeMenu.Parent := Parent;

  FFormClient := TWinControl.Create(Parent);
  FFormClient.ControlStyle:= FFormClient.ControlStyle + [csOpaque];
  FFormClient.Parent := Parent;

  FFormContainer := TWinControl.Create(FFormClient);
  FFormContainer.Parent := FFormClient;

  FAnchorContainer := TWinControl.Create(Parent);
  FAnchorContainer.Visible := False;
  FAnchorContainer.Parent := Parent;
end;

destructor TResizeContainer.Destroy;
begin
  FResizeBars.Free;
  FResizeGrips.Free;
  inherited Destroy;
end;

function TResizeContainer.IsHorzSizer(AObject: TObject): Boolean;
begin
  Result := (AObject = ResizeGrips.GripCenterRight) or
            (AObject = ResizeGrips.GripBottomRight) or
            (AObject = ResizeBars.BarRightTop) or
            (AObject = ResizeBars.BarRightBottom);
end;

function TResizeContainer.IsVertSizer(AObject: TObject): Boolean;
begin
  Result := (AObject = ResizeGrips.GripBottomCenter) or
            (AObject = ResizeGrips.GripBottomRight) or
            (AObject = ResizeBars.BarBottomLeft) or
            (AObject = ResizeBars.BarBottomRight);
end;

procedure TResizeContainer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  FBoundsRect := Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
  FResizeGrips.SetBounds(FBoundsRect);
  FResizeBars.SetBounds(FBoundsRect);
  FFakeMenu.SetBounds(ALeft + FResizeBars.BarSize, ATop + FResizeBars.BarSize, AWidth - FResizeBars.BarSize * 2, FFakeMenu.Height);
  FFormClient.SetBounds(ALeft + FResizeBars.BarSize, ATop + FResizeBars.BarSize + FFakeMenu.Height, AWidth - FResizeBars.BarSize * 2, AHeight - FResizeBars.BarSize * 2 - FFakeMenu.Height);
  FAnchorContainer.SetBounds(ALeft + FResizeBars.BarSize, ATop + FResizeBars.BarSize + FFakeMenu.Height, AWidth - FResizeBars.BarSize * 2, AHeight - FResizeBars.BarSize * 2 - FFakeMenu.Height);
end;

end.

