{/***************************************************************************
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

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, LCLLinux, Forms, Graphics;

type
  TGrabberMoveEvent = procedure(Sender: TObject; dx, dy: Integer) of object;

  TGrabIndex = 0..7;

  TGrabPosition = (gpTop, gpBottom, gpLeft, gpRight);
  TGrabPositions = set of TGrabPosition;

  // A TGrabber is one of the 8 small black rectangles at the boundaries of
  // a selection
  TGrabber = class
  private
    FPositions: TGrabPositions;
    FHeight: integer;
    FTop: integer;
    FWidth: integer;
    FLeft: integer;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
  public
    property Positions: TGrabPositions read FPositions write FPositions;
    property Left:integer read FLeft write FLeft;
    property Top:integer read FTop write FTop;
    property Width:integer read FWidth write FWidth;
    property Height:integer read FHeight write FHeight;
    property OldLeft:integer read FOldLeft write FOldLeft;
    property OldTop:integer read FOldTop write FOldTop;
    property OldWidth:integer read FOldWidth write FOldWidth;
    property OldHeight:integer read FOldHeight write FOldHeight;
    procedure SaveBounds;
  end;


  TSelectedControl = class
  private
    FControl:TControl;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
  public
    constructor Create(AControl:TControl);
    destructor Destroy; override;
    property Control:TControl read FControl write FControl;
    property OldLeft:integer read FOldLeft write FOldLeft;
    property OldTop:integer read FOldTop write FOldTop;
    property OldWidth:integer read FOldWidth write FOldWidth;
    property OldHeight:integer read FOldHeight write FOldHeight;
    procedure SaveBounds;
  end;


  TControlSelection = class(TObject)
  private
    FControls: TList;  // list of TSelectedComponent

    // current bounds (only valid when Count>0)
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    // saved bounds
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;

    FCustomForm: TCustomForm;
    FGrabbers: array[TGrabIndex] of TGrabber;
    FGrabberSize: integer;
    FMarkerSize: integer;
    FActiveGrabber:TGrabber;
    FRubberBandBounds:TRect;
    FVisible:boolean;

    FOnChange: TNotifyEvent;

    procedure SetCustomForm;
    function GetGrabbers(AGrabIndex:TGrabIndex): TGrabber;
    procedure SetGrabbers(AGrabIndex:TGrabIndex; const AGrabber: TGrabber);
    procedure SetGrabberSize(const NewSize: integer);
    procedure AdjustSize;
    procedure AdjustGrabber;
    procedure DoChange;
    procedure SetVisible(const Value: Boolean);
    function GetItems(Index:integer):TSelectedControl;
    procedure SetItems(Index:integer; ASelectedControl:TSelectedControl);
    procedure SetActiveGrabber(AGrabber:TGrabber);
    procedure SetRubberBandBounds(ARect:TRect);
  protected
  public
    property Items[Index:integer]:TSelectedControl read GetItems write SetItems; default;
    function Count:integer;
    function IndexOf(AControl:TControl):integer;
    function Add(AControl: TControl):integer;
    procedure Remove(AControl: TControl);
    procedure Delete(Index:integer);
    procedure Clear;
    procedure Assign(AControlSelection:TControlSelection);
    function IsSelected(AControl: TControl): Boolean;
    procedure SaveBounds;
    procedure MoveSelection(dx, dy: integer);
    procedure SizeSelection(dx, dy: integer);  
      // size all controls depending on ActiveGrabber.
      // if ActiveGrabber=nil then Left,Top
    property GrabberSize:integer read FGrabberSize write SetGrabberSize;
    procedure DrawGrabbers(DC: HDC);
    function GrabberAtPos(X,Y:integer):TGrabber;
    property Grabbers[AGrabIndex:TGrabIndex]:TGrabber read GetGrabbers write SetGrabbers;
    property MarkerSize:integer read FMarkerSize write FMarkerSize;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure DrawMarker(AControl:TControl; DC:HDC);
    property ActiveGrabber:TGrabber read FActiveGrabber write SetActiveGrabber;
    property Left:integer read FLeft;
    property Top:integer read FTop;
    property Width:integer read FWidth;
    property Height:integer read FHeight;
    property RubberbandBounds:TRect read FRubberbandBounds write SetRubberbandBounds;
    procedure DrawRubberband(DeleteOld:boolean; ARect:TRect);
    procedure SelectWithRubberBand(ACustomForm:TCustomForm);
    property Visible:boolean read FVisible write SetVisible;
    constructor Create; 
    destructor Destroy; override;
  end;


implementation


uses
  Sysutils, Math;

const
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

procedure TGrabber.SaveBounds;
begin
  FOldLeft:=FLeft;
  FOldTop:=FTop;
  FOldWidth:=FWidth;
  FOldHeight:=FHeight;
end;


{ TSelectedControl }

constructor TSelectedControl.Create(AControl:TControl);
begin
  inherited Create;
  FControl:=AControl;
end;

destructor TSelectedControl.Destroy;
begin
  inherited Destroy;
end;

procedure TSelectedControl.SaveBounds;
begin
  FOldLeft:=Control.Left;
  FOldTop:=Control.Top;
  FOldWidth:=Control.Width;
  FOldHeight:=Control.Height;
end;

{ TControlSelection }

constructor TControlSelection.Create;
var g:TGrabIndex;
begin
  inherited;
  FControls:=TList.Create;
  FGrabberSize:=6;
  FMarkerSize:=5;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    FGrabbers[g]:=TGrabber.Create;
    FGrabbers[g].Positions:=GRAB_POSITIONS[g];
  end;
  FCustomForm:=nil;
  FActiveGrabber:=nil;
end;

destructor TControlSelection.Destroy;
var g:TGrabIndex;
begin
  Clear;
  FControls.Free;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].Free;
  inherited Destroy;
end;

procedure TControlSelection.SetCustomForm;
var NewCustomForm:TCustomForm;
begin
  if Count>0 then begin
    NewCustomForm:=GetParentForm(Items[0].Control);
  end else begin
    NewCustomForm:=nil;
  end;
  if NewCustomForm=FCustomForm then exit;
  FCustomForm:=NewCustomForm;
end;

function TControlSelection.GetGrabbers(AGrabIndex:TGrabIndex): TGrabber;
begin
  Result:=FGrabbers[AGrabIndex];
end;

procedure TControlSelection.SetGrabbers(AGrabIndex:TGrabIndex;
  const AGrabber: TGrabber);
begin
  FGrabbers[AGrabIndex]:=AGrabber;
end;

procedure TControlSelection.SetGrabberSize(const NewSize: integer);
begin
  if NewSize=FGrabberSize then exit;
  FGrabberSize:=NewSize;
end;

procedure TControlSelection.AdjustSize;
var i,ALeft,ATop:integer;
  FormOrigin:TPoint;

  procedure AbsoluteLeftTop(AControl:TControl; var ALeft, ATop:integer);
  var ControlOrigin:TPoint;
  begin
    ControlOrigin:=AControl.ClientOrigin;
    ALeft:=ControlOrigin.X-FormOrigin.X;
    ATop:=ControlOrigin.Y-FormOrigin.Y;
writeln('[AbsoluteLeftTop] ',ControlOrigin.X,',',ControlOrigin.Y
        ,'  ',FormOrigin.X,',',FormOrigin.Y);
  end;

begin
  if FControls.Count>=1 then begin
    FormOrigin:=FCustomForm.ClientOrigin;
    AbsoluteLeftTop(Items[0].Control,ALeft,ATop);
writeln('[TControlSelection.AdjustSize] ',ALeft,',',ATop,'  ',Items[0].Control.Name);
    FLeft:=ALeft;
    FTop:=ATop;
    FHeight:=Items[0].Control.Height;
    FWidth:=Items[0].Control.Width;
    for i:=1 to FControls.Count-1 do begin
      AbsoluteLeftTop(Items[i].Control,ALeft,ATop);
      if FLeft>ALeft then begin
        inc(FWidth,FLeft-ALeft);
        FLeft:=ALeft;
      end;
      if FTop>ATop then begin
        inc(FHeight,FTop-ATop);
        FTop:=ATop;
      end;
      FWidth:=Max(FLeft+FWidth,ALeft+Items[i].Control.Width)-FLeft;
      FHeight:=Max(FTop+FHeight,ATop+Items[i].Control.Height)-FTop;
    end;
    AdjustGrabber;
writeln('[TControlSelection.AdjustSize] ',FLeft,',',FTop);
  end;
end;

procedure TControlSelection.AdjustGrabber;
var g:TGrabIndex;
begin
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    if gpLeft in FGrabbers[g].Positions then
      FGrabbers[g].Left:=FLeft-GrabberSize
    else if gpRight in FGrabbers[g].Positions then
      FGrabbers[g].Left:=FLeft+FWidth
    else
      FGrabbers[g].Left:=FLeft+((FWidth-GrabberSize) div 2);
    if gpTop in FGrabbers[g].Positions then
      FGrabbers[g].Top:=FTop-GrabberSize
    else if gpBottom in FGrabbers[g].Positions then
      FGrabbers[g].Top:=FTop+FHeight
    else
      FGrabbers[g].Top:=FTop+((FHeight-GrabberSize) div 2);
    FGrabbers[g].Width:=GrabberSize;
    FGrabbers[g].Height:=GrabberSize;
  end;
end;

procedure TControlSelection.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TControlSelection.SetVisible(const Value: Boolean);
begin
  if FVisible=Value then exit;
  FVisible:=Value;
  DoChange;
end;

function TControlSelection.GetItems(Index:integer):TSelectedControl;
begin
  Result:=TSelectedControl(FControls[Index]);
end;

procedure TControlSelection.SetItems(Index:integer;
  ASelectedControl:TSelectedControl);
begin
  FControls[Index]:=ASelectedControl;
end;

procedure TControlSelection.SaveBounds;
var i:integer;
  g:TGrabIndex;
begin
  for i:=0 to FControls.Count-1 do Items[i].SaveBounds;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].SaveBounds;
  FOldLeft:=FLeft;
  FOldTop:=FTop;
  FOldWidth:=FWidth;
  FOldHeight:=FHeight;
end;

procedure TControlSelection.SetActiveGrabber(AGrabber:TGrabber);
begin
  FActiveGrabber:=AGrabber;
end;

function TControlSelection.Count:integer;
begin
  Result:=FControls.Count;
end;

function TControlSelection.IndexOf(AControl:TControl):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].Control<>AControl) do dec(Result);
end;

function TControlSelection.Add(AControl: TControl):integer;
var NewSelectedControl:TSelectedControl;
begin
  NewSelectedControl:=TSelectedControl.Create(AControl);
  if GetParentForm(AControl)<>FCustomForm then Clear;
  Result:=FControls.Add(NewSelectedControl);
  if Count=1 then SetCustomForm;
  AdjustSize;
  DoChange;  
end;

procedure TControlSelection.Remove(AControl: TControl);
var i:integer;
begin
  i:=IndexOf(AControl);
  if i>=0 then Delete(i);
end;

procedure TControlSelection.Delete(Index:integer);
begin
  if Index<0 then exit;
  Items[Index].Free;
  FControls.Delete(Index);
  if Count=0 then SetCustomForm;
  AdjustSize;
  DoChange;
end;

procedure TControlSelection.Clear;
var i:integer;
begin
  for i:=0 to FControls.Count-1 do Items[i].Free;
  FControls.Clear;
  FCustomForm:=nil;
  AdjustSize;
  DoChange;
end;

procedure TControlSelection.Assign(AControlSelection:TControlSelection);
var i:integer;
begin
  if AControlSelection=Self then exit;
  Clear;
  FControls.Capacity:=AControlSelection.Count;
  for i:=0 to AControlSelection.Count-1 do
    Add(AControlSelection[i].Control);
  SetCustomForm;
  AdjustSize;
  DoChange;
end;

function TControlSelection.IsSelected(AControl: TControl): Boolean;
begin
  Result:=(IndexOf(AControl)>=0);
end;

procedure TControlSelection.MoveSelection(dx, dy: integer);
var i:integer;
  g:TGrabIndex;
begin
  if (dx=0) and (dy=0) then exit;
  for i:=0 to FControls.Count-1 do
    with Items[i] do
      Control.SetBounds(OldLeft+dx,OldTop+dy
         ,Control.Width,Control.Height);
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    FGrabbers[g].Left:=FGrabbers[g].OldLeft+dx;
    FGrabbers[g].Top:=FGrabbers[g].OldTop+dy;
  end;
end;

procedure TControlSelection.SizeSelection(dx, dy: integer);  
// size all controls depending on ActiveGrabber.
// if ActiveGrabber=nil then Left,Top
var i:integer;
  GrabberPos:TGrabPositions;
begin
  if Count=0 then exit;
  if FActiveGrabber<>nil then
    GrabberPos:=FActiveGrabber.Positions
  else
    GrabberPos:=[gpLeft,gpTop];
  if [gpTop,gpBottom] * GrabberPos = [] then dy:=0;
  if [gpLeft,gpRight] * GrabberPos = [] then dx:=0;
  if (dx=0) and (dy=0) then exit;
  if gpLeft in GrabberPos then begin
    FLeft:=FOldLeft+dx;
    FWidth:=FOldWidth-dx;
  end;
  if gpRight in GrabberPos then begin
    FWidth:=FOldWidth+dx;
  end;
  if gpTop in GrabberPos then begin
    FTop:=FOldTop+dy;
    FHeight:=FOldHeight-dy;
  end;
  if gpBottom in GrabberPos then begin
    FHeight:=FOldHeight+dy;
  end;
  AdjustGrabber;
  if Count=1 then begin
    // single selection
    Items[0].Control.SetBounds(FLeft,FTop,FWidth,FHeight);
  end else if Count>1 then begin
    // multi selection
    if (FOldWidth<>0) and (FOldHeight<>0) then begin
      for i:=0 to Count-1 do begin
        Items[i].Control.SetBounds(
          FOldLeft + (((Items[i].OldLeft-FOldLeft) * FWidth) div FOldWidth),
          FOldTop + (((Items[i].OldTop-FOldTop) * FHeight) div FOldHeight),
          Max(1,Abs((Items[i].OldWidth * FWidth) div FOldWidth)),
          Max(1,Abs((Items[i].OldHeight * FHeight) div FOldHeight))
        );
      end;
    end;
  end;
  DoChange;
end;

function TControlSelection.GrabberAtPos(X,Y:integer):TGrabber;
var g:TGrabIndex;
begin
  if FControls.Count>0 then begin
    for g:=Low(TGrabIndex) to High(TGrabIndex) do
      if (FGrabbers[g].Left<=x) and (FGrabbers[g].Top<=y)
      and (FGrabbers[g].Left+FGrabbers[g].Width>x)
      and (FGrabbers[g].Top+FGrabbers[g].Height>y) then begin
        Result:=FGrabbers[g];
        exit;
      end;
  end;
  Result:=nil;
end;

procedure TControlSelection.DrawGrabbers(DC: HDC);
var OldBrushColor:TColor;
  g:TGrabIndex;
  FormOrigin, DCOrigin, Diff: TPoint;
begin
  if (Count=0) or (FCustomForm=nil)
  or (Items[0].Control is TCustomForm) then exit;
  GetWindowOrgEx(DC, DCOrigin);
  FormOrigin:=FCustomForm.ClientOrigin;
  Diff.X:=FormOrigin.X-DCOrigin.X;
  Diff.Y:=FormOrigin.Y-DCOrigin.Y;
{
writeln('[DrawGrabbers] Form=',FormOrigin.X,',',FormOrigin.Y
   ,' DC=',DCOrigin.X,',',DCOrigin.Y
   ,' Grabber1=',FGrabbers[0].Left,',',FGrabbers[0].Top
   ,' Selection=',FLeft,',',FTop);
}
  FCustomForm.Canvas.Handle:=DC;
  with FCustomForm.Canvas do begin
    OldBrushColor:=Brush.Color;
    Brush.Color:=clBlack;
    for g:=Low(TGrabIndex) to High(TGrabIndex) do
      FillRect(Rect(
         Diff.X+FGrabbers[g].Left
        ,Diff.Y+FGrabbers[g].Top
        ,Diff.X+FGrabbers[g].Left+FGrabbers[g].Width
        ,Diff.Y+FGrabbers[g].Top+FGrabbers[g].Height
      ));
    Brush.Color:=OldbrushColor;
  end;
end;

procedure TControlSelection.DrawMarker(AControl:TControl; DC:HDC);
var OldBrushColor:TColor;
  ALeft,ATop:integer;
  AControlOrigin,DCOrigin:TPoint;
  SaveIndex:HDC;
begin
  if (Count<1) or (FCustomForm=nil) or (AControl is TCustomForm)
  or (not IsSelected(AControl)) then exit;
  AControlOrigin:=AControl.ClientOrigin;
  GetWindowOrgEx(DC, DCOrigin);
  // MoveWindowOrg is currently not functioning in the gtk
  // this is a workaround
  ALeft:=AControlOrigin.X-DCOrigin.X; //AControlOrigin.X-FormOrigin.X;
  ATop:=AControlOrigin.Y-DCOrigin.Y; //AControlOrigin.Y-FormOrigin.Y;
  SaveIndex := SaveDC(DC);
  FCustomForm.Canvas.Handle:=DC;
{
writeln('DrawMarker A ',FCustomForm.Name
    ,' Control=',AControl.Name,',',AControlOrigin.X,',',AControlOrigin.Y
    ,' DCxy=',DCOrigin.x,',',DCOrigin.y
    ,' DC=',Hexstr(FCustomForm.Canvas.Handle,8),' ',HexStr(Cardinal(Pointer(FCustomForm)),8)
    );
}
  with FCustomForm.Canvas do begin
    OldBrushColor:=Brush.Color;
    Brush.Color:=clDKGray;
    FillRect(Rect(ALeft,ATop,ALeft+MarkerSize,ATop+MarkerSize));
    FillRect(Rect(ALeft,ATop+AControl.Height-MarkerSize
             ,ALeft+MarkerSize,ATop+AControl.Height));
    FillRect(Rect(ALeft+AControl.Width-MarkerSize,ATop
                 ,ALeft+AControl.Width,ATop+MarkerSize));
    FillRect(Rect(ALeft+AControl.Width-MarkerSize
                 ,ATop+AControl.Height-MarkerSize
                 ,ALeft+AControl.Width,ATop+AControl.Height));
    Brush.Color:=OldbrushColor;
  end;
  FCustomForm.Canvas.Handle:=0;
  RestoreDC(DC, SaveIndex);
end;

procedure TControlSelection.DrawRubberband(DeleteOld:boolean; ARect:TRect);

  procedure DrawInvertFrameRect(x1,y1,x2,y2:integer);
  var i:integer;
    procedure InvertPixel(x,y:integer);
    var c:TColor;
    begin
      c:=FCustomForm.Canvas.Pixels[x,y];
      c:=c xor $ffffff;
      FCustomForm.Canvas.Pixels[x,y]:=c;
    end;
  begin
    if FCustomForm=nil then exit;
    if x1>x2 then begin i:=x1; x1:=x2; x2:=i; end;
    if y1>y2 then begin i:=y1; y1:=y2; y2:=i; end;
    i:=x1+1;
    while i<x2-1 do begin
      InvertPixel(i,y1);
      InvertPixel(i,y2);
      inc(i,2);
    end;
    i:=y1;
    while i<y2 do begin
      InvertPixel(x1,i);
      InvertPixel(x2,i);
      inc(i,2);
    end;
  end;

// DrawRubberband
begin
  if DeleteOld then
    with FRubberBandBounds do
      DrawInvertFrameRect(Left,Top,Right,Bottom);
  FRubberBandBounds:=ARect;
  with FRubberBandBounds do
    DrawInvertFrameRect(Left,Top,Right,Bottom);
end;

procedure TControlSelection.SelectWithRubberBand(ACustomForm:TCustomForm);
var i:integer;
  FormOrigin:TPoint;

  function ControlInRubberBand(AControl:TControl):boolean;
  var ALeft,ATop,ARight,ABottom:integer;
    Origin:TPoint;
  begin
    Origin:=AControl.ClientOrigin;
    ALeft:=Origin.X-FormOrigin.X;
    ATop:=Origin.Y-FormOrigin.Y;
    ARight:=ALeft+AControl.Width;
    ABottom:=ATop+AControl.Height;
    Result:=(ALeft<FRubberBandBounds.Right)
        and (ATop<FRubberBandBounds.Bottom)
        and (ARight>=FRubberBandBounds.Left)
        and (ABottom>=FRubberBandBounds.Top);
  end;

// SelectWithRubberBand
begin
  FormOrigin:=ACustomForm.ClientOrigin;
  Clear;
  for i:=0 to ACustomForm.ControlCount-1 do
    if ControlInRubberBand(ACustomForm.Controls[i]) then
      Add(ACustomForm.Controls[i]);
end;

procedure TControlSelection.SetRubberBandBounds(ARect:TRect);
begin
  FRubberBandBounds:=ARect;
end;

end.
