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

{$mode objfpc}

interface

uses
  Classes, Controls, LCLLinux, Forms;

type
  TGrabberMoveEvent = procedure(Sender: TObject; dx, dy: Integer) of object;

  TGrabIndex = 0..7;

  TGrabPosition = (gpTop, gpBottom, gpLeft, gpRight);
  TGrabPositions = set of TGrabPosition;

  TGrabber = class(TWinControl)
  private
    FDragging: Boolean;
    FLastMouseMove, FStart: TPoint;
    FPositions: TGrabPositions;
    FOnMove: TGrabberMoveEvent;
    FOnMoved: TGrabberMoveEvent;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure DoDragMove(NewX, NewY: integer);
    procedure EndDragging(NewX, NewY: integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    procedure CaptureMouseMove(Sender:TControl;Shift: TShiftState; X, Y: Integer);
    procedure CaptureMouseUp(Sender:TControl;Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    constructor Create(AOwner: TComponent); override;
    property IsDragging:Boolean read FDragging;
    property Positions: TGrabPositions read FPositions write FPositions;
    property OnMove: TGrabberMoveEvent read FOnMove write FOnMove;
    property OnMoved: TGrabberMoveEvent read FOnMoved write FOnMoved;
  end;

  TControlSelection = class(TObject)
  private
    FDragging: Boolean;
    FVisible: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FTop: Integer;
    FLeft: Integer;
    FStart: TPoint;
    FGrabbers: array[TGrabIndex] of TGrabber;
    FControlList: TList;
    FOnChange: TNotifyEvent;
    procedure AdjustSize(AControl: TControl; Initial: Boolean);
    procedure ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DoChange;
    procedure SetGrabbers;
    procedure SizeContent;
    procedure SetVisible(const Value: Boolean);
    procedure GrabberMove(Sender: TObject; dx, dy: Integer);
    procedure GrabberMoved(Sender: TObject; dx, dy: Integer);
  protected
  public
    procedure MoveContent(dx, dy: Integer);
    procedure Add(AControl: TControl);
    procedure Clear;
    constructor Create(AOwner: TWinControl); virtual;
    destructor Destroy; override;
    function IsSelected(AControl: TControl): Boolean;
    procedure Remove(AControl: TControl);
    procedure MoveSelection(dx, dy: integer);
    procedure SizeSelection(dx, dy: integer);
    property Visible: Boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  end;

  procedure SetCaptureGrabber(AGrabber:TGrabber);
  function GetCaptureGrabber:TGrabber;

implementation

uses
  Sysutils, Math;

type
  TSelectControl = class(TControl)//;
  end;

const
  GRAB_SIZE = 6;

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

var
  CaptureGrabber:TGrabber;

procedure SetCaptureGrabber(AGrabber:TGrabber);
begin
  Write('SETCAPTUREGRABBER to.... ');
  if AGrabber <> nil then Writeln(Format('0x%x', [AGrabber.handle])) else writeln('nil');

  CaptureGrabber:=AGrabber;
end;

function GetCaptureGrabber:TGrabber;
begin
  Result:=CaptureGrabber;
end;

{ TGrabber }

constructor TGrabber.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlState := ControlState + [csCustomPaint];
  FDragging := False;
end;

procedure TGrabber.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//writeln('[TGrabber.MouseDown] X='+IntToStr(X+Left)+',Y='+IntToStr(Y+Top));
  if CaptureGrabber<>nil then exit;
  // compute absolute mouse coordinates
  if (Button = mbLeft) and (not FDragging)
  then begin
    FLastMouseMove := Point(X+Left, Y+Top);
    FStart := FLastMouseMove;
    FDragging := True;
    SetCaptureGrabber(Self);
  end else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TGrabber.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
  if (CaptureGrabber<>nil) and (CaptureGrabber<>Self) then begin
    CaptureGrabber.CaptureMouseMove(Self,Shift, X, Y);
  end else begin
    if FDragging then begin
//writeln('[TGrabber.MouseMove] X='+IntToStr(X)+',Y='+IntToStr(Y));
      DoDragMove(X+Left,Y+Top);
    end else
      inherited MouseMove(Shift, X, Y);
  end;
end;

procedure TGrabber.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
begin
//Writeln('MouseUp in TGrabber');

  if (CaptureGrabber<>nil) and (CaptureGrabber<>Self) then begin
    CaptureGrabber.CaptureMouseUp(Self,Button, Shift, X, Y);
  end else begin
//writeln('[TGrabber.MouseUp] X='+IntToStr(X+Left)+',Y='+IntToStr(Y+Top));
    if FDragging then
      EndDragging(X+Left,Y+Top)
    else
      inherited MouseUp(Button, Shift, X, Y);
  end;
end;

procedure TGrabber.CaptureMouseMove(Sender:TControl;Shift: TShiftState;
X, Y: Integer);
begin
//Writeln('CaptureMouseMove in TGrabber');
  if CaptureGrabber<>Self then exit;
//writeln('[TGrabber.CaptureMouseMove]');
  MouseMove(Shift,X-CaptureGrabber.Left,Y-CaptureGrabber.Top);
end;

procedure TGrabber.CaptureMouseUp(Sender:TControl;Button: TMouseButton;
Shift: TShiftState; X, Y: Integer);
begin
//Writeln('CaptureMouseUp in TGrabber');
  if CaptureGrabber<>Self then exit;
  if not (Sender is TCustomForm) then begin
    inc(X,TControl(Sender).Left);
    inc(Y,TControl(Sender).Top);
  end;
  MouseUp(Button,Shift,X-CaptureGrabber.Left,Y-CaptureGrabber.Top);
end;

procedure TGrabber.DoDragMove(NewX, NewY: integer);
var dx, dy: Integer;
begin
  if FDragging then begin
//writeln('[TGrabber.DoDragMove] NewX='+IntToStr(NewX)+',NewY='+IntToStr(NewY));
    if [gpLeft, gpRight] * Positions <> []
    then dx := NewX - FLastMouseMove.X
    else dx := 0;
    if [gpTop, gpBottom] * Positions <> []
    then dy := NewY - FLastMouseMove.Y
    else dy := 0;
    FLastMouseMove:=Point(NewX,NewY);

    if Assigned(FOnMove) then FOnMove(Self, dx, dy);
  end;
end;

procedure TGrabber.EndDragging(NewX, NewY: integer);
var dx, dy: Integer;
begin
  if FDragging then begin
//writeln('[TGrabber.EndDragging] NewX='+IntToStr(NewX)+',NewY='+IntToStr(NewY));
    DoDragMove(NewX, NewY);
    FDragging := False;
    SetCaptureGrabber(nil);
    if [gpLeft, gpRight] * Positions <> []
    then dx := NewX - FStart.X
    else dx := 0;
    if [gpTop, gpBottom] * Positions <> []
    then dy := NewY - FStart.Y
    else dy := 0;
    if Assigned(FOnMoved) then FOnMoved(Self, dx, dy);
  end;
end;

procedure TGrabber.PaintWindow(DC: HDC);
begin
  FillRect(DC, Rect(0, 0, Width, Height), GetStockObject(BLACK_BRUSH));
end;

{ TControlSelection }

procedure TControlSelection.MoveSelection(dx, dy: integer);
begin
{Writeln('**********');
Writeln('Move Selection');
Writeln(Format('dx,dy = %d,%d',[dx,dy]));
Writeln(Format('FLeft,FTop= %d,%d',[FLeft,FTop]));
Writeln('**********');
 }
  if (dx<>0) or (dy<>0) then begin
    Inc(FLeft,dx);
    Inc(FTop,dy);
    MoveContent(dx,dy);
    SetGrabbers;
  end;
end;

procedure TControlSelection.SizeSelection(dx, dy: integer);
begin
{Writeln('**********');
Writeln('Size Selection');
Writeln(Format('dx,dy = %d,%d',[dx,dy]));
Writeln(Format('FLeft,FTop= %d,%d',[FLeft,FTop]));
Writeln('**********');
 }
  if (dx<>0) or (dy<>0) then begin
    Inc(FWidth,dx);
    Inc(FHeight,dy);
    SizeContent;
    SetGrabbers;
  end;
end;


procedure TControlSelection.Add(AControl: TControl);
begin
  if AControl <> nil
  then begin
    WriteLn(Format('[TControlSelection.Add] %s', [AControl.ClassName]));

    if (FControlList.Count>0) and (AControl is TCustomForm) then begin
      writeln('TCustomForm not added to multiselection');
    end else begin;
      FControlList.Add(AControl);
    end;
    AdjustSize(Acontrol, FControlList.Count = 1);
    Visible:=not (AControl is TCustomForm);
    //This is taken care of in SETVISIBLE    SetGrabbers;
    with TSelectControl(AControl) do
    begin
      {OnMouseDown := @ControlMouseDown;
      OnMouseMove := @ControlMouseMove;
      OnMouseUp := @ControlMouseUp;}
    end;
    DoChange;
  end;
end;

procedure TControlSelection.AdjustSize(AControl: TControl; Initial: Boolean);
var
  n: Integer;
begin
Writeln('AdjustSize in TCOntrolSelection');

  if AControl <> nil
  then begin
    if Initial
    then begin
      FLeft := AControl.Left;
      FTop := AControl.Top;
      FWidth := AControl.Width;
      FHeight := AControl.Height;
      WriteLn(Format(
        '[TControlSelection.AdjustSize] Initializing to X:%d, Y:%d, W:%d, H: %d'
        , [FLeft, FTop, FWidth, FHeight]));
    end
    else begin
      WriteLn(Format(
        '[TControlSelection.AdjustSize] current X:%d, Y:%d, W:%d, H: %d'
        , [FLeft, FTop, FWidth, FHeight]));
      with AControl do
        WriteLn(Format(
          '[TControlSelection.AdjustSize] '+
          'Adjust for %s --> X:%d, Y:%d, W:%d, H: %d'
          , [Classname, Left, Top, Width, Height]));
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
      WriteLn(Format(
        '[TControlSelection.AdjustSize] Adjusted to X:%d, Y:%d, W:%d, H: %d'
        , [FLeft, FTop, FWidth, FHeight]));
    end;
  end;
end;

procedure TControlSelection.Clear;
var
  n:  Integer;
begin
  writeln('[TControlSelection.Clear]');
  with FControlList do
  begin
    for n := 0 to Count -1 do
      with TSelectControl(Items[n]) do
      begin
        {OnMouseDown := nil;
        OnMouseMove := nil;
        OnMouseUp := nil;}
      end;

    Clear;
  end;
  FWidth := 0;
  FHeight := 0;
  Visible := False;
  //This is set in SETVISIBLE   SetGrabbers;
  DoChange;
end;

procedure TControlSelection.ControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
Writeln('ControlMOuseDown in TCOntrolSelection');

  if Button = mbLeft
  then begin
    FStart := Point(X, Y);
    FDragging := True;
  end;
end;

procedure TControlSelection.ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
Writeln('ControlMOuseMove in TCOntrolSelection');
  if FDragging
  then begin
    Inc(FLeft, X - FStart.X);
    Inc(FTop, Y - FStart.Y);
    SetGrabbers;
    Writeln(format('X-FStart.x = %d-%d=%d',[X,FStart.x,X-FStart.x]));
    Writeln(format('Y-FStart.Y = %d-%d=%d',[Y,FStart.y,Y-FStart.y]));
    MoveContent(X - FStart.x, Y - FStart.Y);
  end;
end;

procedure TControlSelection.ControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
Writeln('TCOntrolSelection.ControlMOuseUp');
  if (Button = mbLeft) and FDragging
  then begin
    FDragging := False;
    Writeln(format('X-FStart.x = %d-%d=%d',[X,FStart.x,X-FStart.x]));
    Writeln(format('Y-FStart.Y = %d-%d=%d',[Y,FStart.y,Y-FStart.y]));
    MoveContent(X - FStart.X, Y - FStart.Y);
  end;
end;

constructor TControlSelection.Create(AOwner: TWinControl);
var
  GrabPos: TGrabIndex;
begin
  writeln('[TControlSelection.Create] '+TComponent(AOwner).Name);
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
      OnMove := @GrabberMove;
      OnMoved := @GrabberMoved;
    end;
  end;
Writeln('Done in TControlSelection.Create');
end;

destructor TControlSelection.Destroy;
begin
  Clear;
  FControlList.Free;
  inherited Destroy;
end;

procedure TControlSelection.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TControlSelection.GrabberMove(Sender: TObject; dx, dy: Integer);
begin
  writeln('[TControlSelection.GrabberMove] '+TComponent(Sender).Name+
    ' dx='+IntToStr(dx)+',dy='+IntToStr(dy));

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
  writeln('[TControlSelection.GrabberMoved] '+TComponent(Sender).Name
    +',dx='+IntToStr(dx)+', dy='+IntToStr(dy));
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
  writeln('[TControlSelection.MoveContent] dx='+IntToStr(dx)+', dy='+IntToStr(dy));
  if FControlList.Count = 1 then
     begin
      if (TCOntrol(FControlList[0]) is TCustomForm) then exit;
      TControl(FControlList[0]).SetBounds(FLeft, FTop, FWidth, FHeight);
     end
  else
  with FControlList do
    for n := 0 to Count -1 do
      with TControl(Items[n]) do
      begin

        SetBounds(Left+dx,Top+dy,Width,Height);
        //Left := Left + dx;
        //Top := Top + dy;
      end;
end;

procedure TControlSelection.Remove(AControl: TControl);
var
  n: Integer;
begin
  with AControl do
    WriteLn(Format(
      '[TControlSelection.Remove] Remove %s --> X:%d, Y:%d, W:%d, H: %d'
      ,[Classname, Left, Top, Width, Height]));

  if (FControlList.Remove(AControl) <> -1)
  then begin
    if FControlList.Count > 0
    then begin
      for n := 0 to FControlList.Count - 1 do
        AdjustSize(TControl(FControlList[n]), n = 0);
    end
    else FVisible := False;

    SetGrabbers;

    with TSelectControl(AControl) do
    begin
     { OnMouseDown := nil;
      OnMouseMove := nil;
      OnMouseUp := nil;}
    end;
    FDragging := False;
    DoChange;
  end;
end;

procedure TControlSelection.SetGrabbers;
var
  GrabPos: TGrabIndex;
  Grabber: TGrabber;
  GrabLeft, GrabTop: Integer;
begin
  WriteLn(Format(
    '[TControlSelection.SetGrabbers] Selection --> X:%d, Y:%d, W:%d, H:%d'
    , [FLeft, FTop, FWidth, FHeight]));
  for GrabPos := Low(TGrabIndex) to High(TGrabIndex) do
  begin
    Grabber := FGrabbers[GrabPos]; 

    //if FVisible then
     begin
      //Write('[TControlSelection.SetGrabbers] Setting grabber ',Ord(GrabPos),' --> ');
      if gpLeft in Grabber.Positions
      then begin
        GrabLeft := FLeft - GRAB_SIZE;
        //Write('Left,   ');
      end
      else begin
        if gpRight in Grabber.Positions
        then begin
          //Write('Right,  ');
          GrabLeft := FLeft + FWidth;
        end
        else begin
          //Write('Center, ');
          GrabLeft := FLeft + (FWidth - GRAB_SIZE) div 2;
        end;
      end;

      if gpTop in Grabber.Positions
      then begin
        GrabTop := FTop - GRAB_SIZE;
        //Write('Top    ');
      end
      else begin
        if gpBottom in Grabber.Positions
        then begin
          //Write('Bottom ');
          GrabTop := FTop + FHeight;
        end
        else begin
          //Write('Center ');
          GrabTop := FTop + (FHeight - GRAB_SIZE) div 2;
        end;
      end;
      
      Grabber.SetBounds(GrabLeft,GrabTop,GRAB_SIZE,GRAB_SIZE);

      //WriteLN(Format('X:%d, Y:%d',  [Grabber.Left, Grabber.Top]));
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
  WriteLn('[TControlSelection.SizeContent]');
  if FControlList.Count = 1 then
  begin
    TControl(FControlList[0]).SetBounds(FLeft, FTop, FWidth, FHeight);
  end;
end;

initialization

CaptureGrabber:=nil;

end.
