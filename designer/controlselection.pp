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
  Classes, LCLLinux, Controls, Forms, Graphics;

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
    FGrabIndex: TGrabIndex;
    FCursor: TCursor;
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
    property GrabIndex: TGrabIndex read FGrabIndex write FGrabIndex;
    property Cursor: TCursor read FCursor write FCursor;
    procedure SaveBounds;
  end;


  TSelectedControl = class
  private
    FComponent:TComponent;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
    function GetLeft: integer;
    procedure SetLeft(ALeft: integer);
    function GetTop: integer;
    procedure SetTop(ATop: integer);
    function GetWidth: integer;
    procedure SetWidth(AWidth: integer);
    function GetHeight: integer;
    procedure SetHeight(AHeight: integer);
  public
    constructor Create(AComponent:TComponent);
    destructor Destroy; override;
    property Component:TComponent read FComponent write FComponent;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OldLeft:integer read FOldLeft write FOldLeft;
    property OldTop:integer read FOldTop write FOldTop;
    property OldWidth:integer read FOldWidth write FOldWidth;
    property OldHeight:integer read FOldHeight write FOldHeight;
    function ParentForm: TCustomForm;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
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
    FCanvas: TCanvas;
    FGrabbers: array[TGrabIndex] of TGrabber;
    FGrabberSize: integer;
    FGrabberColor: TColor;
    FMarkerSize: integer;
    FMarkerColor: integer;
    FActiveGrabber:TGrabber;
    FRubberBandBounds:TRect;
    FRubberbandActive: boolean;
    FVisible:boolean;
    FUpdateLock: integer;
    FChangedDuringLock: boolean;
    FIsResizing: boolean;
    FNotSaveBounds: boolean;

    FOnChange: TNotifyEvent;

    procedure SetCustomForm;
    function GetGrabbers(AGrabIndex:TGrabIndex): TGrabber;
    procedure SetGrabbers(AGrabIndex:TGrabIndex; const AGrabber: TGrabber);
    procedure SetGrabberSize(const NewSize: integer);
    procedure AdjustGrabber;
    procedure DoChange;
    procedure SetVisible(const Value: Boolean);
    function GetItems(Index:integer):TSelectedControl;
    procedure SetItems(Index:integer; ASelectedControl:TSelectedControl);
    procedure SetActiveGrabber(AGrabber:TGrabber);
    procedure SetRubberBandBounds(ARect:TRect);
  protected
  public
    constructor Create; 
    destructor Destroy; override;
    property Items[Index:integer]:TSelectedControl read GetItems write SetItems; default;
    function Count:integer;
    procedure BeginUpDate;
    procedure EndUpdate;
    function IndexOf(AComponent:TComponent):integer;
    function Add(AComponent: TComponent):integer;
    procedure Remove(AComponent: TComponent);
    procedure Delete(Index:integer);
    procedure Clear;
    procedure Assign(AControlSelection:TControlSelection);
    procedure AdjustSize;
    property IsResizing: boolean read FIsResizing;
    function IsSelected(AComponent: TComponent): Boolean;
    procedure SaveBounds;
    procedure MoveSelection(dx, dy: integer);
    procedure SizeSelection(dx, dy: integer);  
      // size all controls depending on ActiveGrabber.
      // if ActiveGrabber=nil then Right,Bottom
    property GrabberSize:integer read FGrabberSize write SetGrabberSize;
    property GrabberColor: TColor read FGrabberColor write FGrabberColor;
    procedure DrawGrabbers(DC: HDC);
    function GrabberAtPos(X,Y:integer):TGrabber;
    property Grabbers[AGrabIndex:TGrabIndex]:TGrabber read GetGrabbers write SetGrabbers;
    property MarkerSize:integer read FMarkerSize write FMarkerSize;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure DrawMarker(AComponent:TComponent; DC:HDC);
    property ActiveGrabber:TGrabber read FActiveGrabber write SetActiveGrabber;
    property Left:integer read FLeft;
    property Top:integer read FTop;
    property Width:integer read FWidth;
    property Height:integer read FHeight;
    property RubberbandBounds:TRect read FRubberbandBounds write SetRubberbandBounds;
    property RubberbandActive: boolean read FRubberbandActive write FRubberbandActive;
    procedure DrawRubberband(DC: HDC);
    procedure SelectWithRubberBand(ACustomForm:TCustomForm; ExclusiveOr: boolean);
    property Visible:boolean read FVisible write SetVisible;
  end;


var TheControlSelection: TControlSelection;

function GetFormRelativeControlTopLeft(Component: TComponent): TPoint;

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


function GetFormRelativeControlTopLeft(Component: TComponent): TPoint;
var FormOrigin: TPoint;
begin
  if Component is TControl then begin
    if TControl(Component).Parent=nil then begin
      Result:=Point(0,0);
    end else begin
      Result:=TControl(Component).Parent.ClientOrigin;
      FormOrigin:=GetParentForm(TControl(Component)).ClientOrigin;
      Result.X:=Result.X-FormOrigin.X+TControl(Component).Left;
      Result.Y:=Result.Y-FormOrigin.Y+TControl(Component).Top;
    end;
  end else begin
    Result.X:=LongRec(Component.DesignInfo).Lo;
    Result.Y:=LongRec(Component.DesignInfo).Hi;
  end;
end;


{ TGrabber }

procedure TGrabber.SaveBounds;
begin
  FOldLeft:=FLeft;
  FOldTop:=FTop;
  FOldWidth:=FWidth;
  FOldHeight:=FHeight;
end;


{ TSelectedControl }

constructor TSelectedControl.Create(AComponent:TComponent);
begin
  inherited Create;
  FComponent:=AComponent;
end;

destructor TSelectedControl.Destroy;
begin
  inherited Destroy;
end;

function TSelectedControl.ParentForm: TCustomForm;
begin
  if FComponent is TControl then
    Result:=GetParentForm(TControl(FComponent))
  else
    if FComponent.Owner is TCustomForm then
      Result:=TCustomForm(FComponent.Owner)
    else
      Result:=nil;
end;

procedure TSelectedControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).SetBounds(ALeft, ATop, AWidth, AHeight)
  else begin
    Left:=ALeft;
    Top:=ATop;
  end;
end;

procedure TSelectedControl.SaveBounds;
begin
  FOldLeft:=Left;
  FOldTop:=Top;
  FOldWidth:=Width;
  FOldHeight:=Height;
writeln('[TSelectedControl.SaveBounds] ',Component.Name,':',Component.ClassName
  ,'  ',FOldLeft,',',FOldTop);
end;

function TSelectedControl.GetLeft: integer;
begin
  if FComponent is TControl then
    Result:=TControl(FComponent).Left
  else
    Result:=LongRec(FComponent.DesignInfo).Lo;
end;

procedure TSelectedControl.SetLeft(ALeft: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Left:=Aleft
  else
    LongRec(FComponent.DesignInfo).Lo:=ALeft;
end;

function TSelectedControl.GetTop: integer;
begin
  if FComponent is TControl then
    Result:=TControl(FComponent).Top
  else
    Result:=LongRec(FComponent.DesignInfo).Hi;
end;

procedure TSelectedControl.SetTop(ATop: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Top:=ATop
  else
    LongRec(FComponent.DesignInfo).Hi:=ATop;
end;

function TSelectedControl.GetWidth: integer;
begin
  if FComponent is TControl then
    Result:=TControl(FComponent).Width
  else
    Result:=20;
end;

procedure TSelectedControl.SetWidth(AWidth: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Width:=AWidth
  else
    ;
end;

function TSelectedControl.GetHeight: integer;
begin
  if FComponent is TControl then
    Result:=TControl(FComponent).Height
  else
    Result:=20;
end;

procedure TSelectedControl.SetHeight(AHeight: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Height:=AHeight
  else
    ;
end;


{ TControlSelection }

constructor TControlSelection.Create;
var g:TGrabIndex;
begin
  inherited;
  FControls:=TList.Create;
  FGrabberSize:=6;
  FGrabberColor:=clBlack;
  FMarkerSize:=5;
  FMarkerColor:=clDkGray;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    FGrabbers[g]:=TGrabber.Create;
    FGrabbers[g].Positions:=GRAB_POSITIONS[g];
    FGrabbers[g].GrabIndex:=g;
    FGrabbers[g].Cursor:=GRAB_CURSOR[g];
  end;
  FCustomForm:=nil;
  FCanvas:=TCanvas.Create;
  FActiveGrabber:=nil;
  FUpdateLock:=0;
  FChangedDuringLock:=false;
  FRubberbandActive:=false;
  FIsResizing:=false;
  FNotSaveBounds:=false;
end;

destructor TControlSelection.Destroy;
var g:TGrabIndex;
begin
  Clear;
  FControls.Free;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TControlSelection.BeginUpDate;
begin
  inc(FUpdateLock);
end;

procedure TControlSelection.EndUpdate;
begin
  if FUpdateLock<=0 then exit;
  dec(FUpdateLock);
  if FUpdateLock=0 then begin
    if FChangedDuringLock then DoChange;
  end;
end;

procedure TControlSelection.SetCustomForm;
var NewCustomForm:TCustomForm;
begin
  if Count>0 then
    NewCustomForm:=Items[0].ParentForm
  else
    NewCustomForm:=nil;
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
var i:integer;
  LeftTop:TPoint;
begin
  if FIsResizing then exit;
  if FControls.Count>=1 then begin
    LeftTop:=GetFormRelativeControlTopLeft(Items[0].Component);
    FLeft:=LeftTop.X;
    FTop:=LeftTop.Y;
    FHeight:=Items[0].Height;
    FWidth:=Items[0].Width;
    for i:=1 to FControls.Count-1 do begin
      LeftTop:=GetFormRelativeControlTopLeft(Items[i].Component);
      if FLeft>LeftTop.X then begin
        inc(FWidth,FLeft-LeftTop.X);
        FLeft:=LeftTop.X;
      end;
      if FTop>LeftTop.Y then begin
        inc(FHeight,FTop-LeftTop.Y);
        FTop:=LeftTop.Y;
      end;
      FWidth:=Max(FLeft+FWidth,LeftTop.X+Items[i].Width)-FLeft;
      FHeight:=Max(FTop+FHeight,LeftTop.Y+Items[i].Height)-FTop;
    end;
    AdjustGrabber;
  end;
end;

procedure TControlSelection.AdjustGrabber;
var g:TGrabIndex;
  OutPix, InPix: integer;
begin
  OutPix:=GrabberSize div 2;
  InPix:=GrabberSize-OutPix;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    if gpLeft in FGrabbers[g].Positions then
      FGrabbers[g].Left:=FLeft-OutPix
    else if gpRight in FGrabbers[g].Positions then
      FGrabbers[g].Left:=FLeft+FWidth-InPix
    else
      FGrabbers[g].Left:=FLeft+((FWidth-GrabberSize) div 2);
    if gpTop in FGrabbers[g].Positions then
      FGrabbers[g].Top:=FTop-OutPix
    else if gpBottom in FGrabbers[g].Positions then
      FGrabbers[g].Top:=FTop+FHeight-InPix
    else
      FGrabbers[g].Top:=FTop+((FHeight-GrabberSize) div 2);
    FGrabbers[g].Width:=GrabberSize;
    FGrabbers[g].Height:=GrabberSize;
  end;
end;

procedure TControlSelection.DoChange;
begin
  if (FUpdateLock>0) then
    FChangedDuringLock:=true
  else begin
    if Assigned(FOnChange) then FOnChange(Self);
    FChangedDuringLock:=false;
  end;
end;

procedure TControlSelection.SetVisible(const Value: Boolean);
begin
  if FVisible=Value then exit;
  FVisible:=Value;
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
  if FNotSaveBounds then exit;
writeln('TControlSelection.SaveBounds');
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

function TControlSelection.IndexOf(AComponent:TComponent):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].Component<>AComponent) do dec(Result);
end;

function TControlSelection.Add(AComponent: TComponent):integer;
var NewSelectedControl:TSelectedControl;
begin
  BeginUpdate;
  NewSelectedControl:=TSelectedControl.Create(AComponent);
  if NewSelectedControl.ParentForm<>FCustomForm then Clear;
  Result:=FControls.Add(NewSelectedControl);
  if Count=1 then SetCustomForm;
  AdjustSize;
  SaveBounds;
  EndUpdate;
  DoChange;  
end;

procedure TControlSelection.Remove(AComponent: TComponent);
var i:integer;
begin
  i:=IndexOf(AComponent);
  if i>=0 then Delete(i);
end;

procedure TControlSelection.Delete(Index:integer);
begin
  if Index<0 then exit;
  Items[Index].Free;
  FControls.Delete(Index);
  if Count=0 then SetCustomForm;
  AdjustSize;
  SaveBounds;
  DoChange;
end;

procedure TControlSelection.Clear;
var i:integer;
begin
  for i:=0 to FControls.Count-1 do Items[i].Free;
  FControls.Clear;
  FCustomForm:=nil;
  AdjustSize;
  SaveBounds;
  DoChange;
end;

procedure TControlSelection.Assign(AControlSelection:TControlSelection);
var i:integer;
begin
  if AControlSelection=Self then exit;
  FNotSaveBounds:=true;
  BeginUpdate;
  Clear;
  FControls.Capacity:=AControlSelection.Count;
  for i:=0 to AControlSelection.Count-1 do
    Add(AControlSelection[i].Component);
  SetCustomForm;
  AdjustSize;
  FNotSaveBounds:=false;
  SaveBounds;
  EndUpdate;
  DoChange;
end;

function TControlSelection.IsSelected(AComponent: TComponent): Boolean;
begin
  Result:=(IndexOf(AComponent)>=0);
end;

procedure TControlSelection.MoveSelection(dx, dy: integer);
var i:integer;
  g:TGrabIndex;
begin
  if (dx=0) and (dy=0) then exit;
  BeginUpdate;
  FIsResizing:=true;
  for i:=0 to FControls.Count-1 do begin
    with Items[i] do begin
writeln('TControlSelection.MoveSelection ',i,'   ',OldLeft,',',OldTop,'  d=',dx,',',dy);
      SetBounds(OldLeft+dx,OldTop+dy,Width,Height)
    end;
  end;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    FGrabbers[g].Left:=FGrabbers[g].OldLeft+dx;
    FGrabbers[g].Top:=FGrabbers[g].OldTop+dy;
  end;
  FIsResizing:=false;
  SaveBounds;
  EndUpdate;
end;

procedure TControlSelection.SizeSelection(dx, dy: integer);  
// size all controls depending on ActiveGrabber.
// if ActiveGrabber=nil then Left,Top
var i:integer;
  GrabberPos:TGrabPositions;
  NewLeft, NewTop, NewRight, NewBottom: integer;
begin
  if (Count=0) or (FIsResizing) then exit;
writeln('[TControlSelection.SizeSelection] A  ',dx,',',dy);
  BeginUpdate;
  FIsResizing:=true;
  if FActiveGrabber<>nil then
    GrabberPos:=FActiveGrabber.Positions
  else
    GrabberPos:=[gpRight,gpBottom];
  if [gpTop,gpBottom] * GrabberPos = [] then dy:=0;
  if [gpLeft,gpRight] * GrabberPos = [] then dx:=0;
  if (dx=0) and (dy=0) then exit;
  if gpLeft in GrabberPos then begin
    FLeft:=FLeft+dx;
    FWidth:=FWidth-dx;
  end;
  if gpRight in GrabberPos then begin
    FWidth:=FWidth+dx;
  end;
  if gpTop in GrabberPos then begin
    FTop:=FTop+dy;
    FHeight:=FHeight-dy;
  end;
  if gpBottom in GrabberPos then begin
    FHeight:=FHeight+dy;
  end;
  AdjustGrabber;
  if Count=1 then begin
    // single selection
    NewLeft:=FLeft;
    NewTop:=FTop;
    NewRight:=FLeft+FWidth;
    NewBottom:=FTop+FHeight;
    Items[0].SetBounds(
      Min(NewLeft,NewRight),
      Min(NewTop,NewBottom),
      Abs(FWidth),
      Abs(FHeight)
    );
  end else if Count>1 then begin
    // multi selection
    if (FOldWidth<>0) and (FOldHeight<>0) then begin
      for i:=0 to Count-1 do begin
        NewLeft:=FOldLeft + (((Items[i].OldLeft-FOldLeft) * FWidth) div FOldWidth);
        NewTop:=FOldTop + (((Items[i].OldTop-FOldTop) * FHeight) div FOldHeight);
        NewRight:=Max(1,Abs(FOldLeft +
                    (((Items[i].OldLeft+Items[i].OldWidth-FOldLeft) * FWidth)
                    div FOldWidth)));
        NewBottom:=Max(1,Abs(FOldTop +
                    (((Items[i].OldTop+Items[i].OldHeight-FOldTop) * FHeight)
                    div FOldHeight)));
        Items[i].SetBounds(NewLeft,NewTop,NewRight-NewLeft,NewBottom-NewTop);
writeln('[TControlSelection.SizeSelection] i=',i,' ',Items[i].Width,' ',Items[i].Height);
      end;
    end;
  end;
  EndUpdate;
  FIsResizing:=false;
end;

function TControlSelection.GrabberAtPos(X,Y:integer):TGrabber;
var g:TGrabIndex;
begin
  if FControls.Count>0 then begin
writeln('[TControlSelection.GrabberAtPos] ',x,',',y,'  '
,FGrabbers[4].Left,',',FGrabbers[4].Top);
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
  SaveIndex: integer;
//  OldFormHandle: HDC;
begin
  if (Count=0) or (FCustomForm=nil)
  or (Items[0].Component is TCustomForm) then exit;
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
//  OldFormHandle:=FCustomForm.Canvas.Handle;
  SaveIndex:=SaveDC(DC);
  FCanvas.Handle:=DC;
  with FCanvas do begin
    OldBrushColor:=Brush.Color;
    Brush.Color:=FGrabberColor;
    for g:=Low(TGrabIndex) to High(TGrabIndex) do
      FillRect(Rect(
         Diff.X+FGrabbers[g].Left
        ,Diff.Y+FGrabbers[g].Top
        ,Diff.X+FGrabbers[g].Left+FGrabbers[g].Width
        ,Diff.Y+FGrabbers[g].Top+FGrabbers[g].Height
      ));
    Brush.Color:=OldbrushColor;
  end;
  FCanvas.Handle:=0;
  RestoreDC(DC,SaveIndex);
//  FCustomForm.Canvas.Handle:=OldFormHandle;
end;

procedure TControlSelection.DrawMarker(AComponent:TComponent; DC:HDC);
var OldBrushColor:TColor;
  ALeft,ATop:integer;
  AControlOrigin,DCOrigin:TPoint;
  SaveIndex:HDC;
  AControl: TControl;
begin
  if (Count<2) or (FCustomForm=nil) or (AComponent is TCustomForm)
  or (not IsSelected(AComponent)) then exit;
  if AComponent is TControl then begin
    AControl:=TControl(AComponent);
    AControlOrigin:=AControl.Parent.ClientOrigin;
    Inc(AControlOrigin.X,AControl.Left);
    Inc(AControlOrigin.Y,AControl.Top);
  end else begin
    if (not (AComponent.Owner is TCustomForm)) then exit;
    AControlOrigin:=TCustomForm(AComponent.Owner).ClientOrigin;
    inc(AControlOrigin.X,LongRec(AComponent.DesignInfo).Lo);
    inc(AControlOrigin.Y,LongRec(AComponent.DesignInfo).Hi);
  end;
  GetWindowOrgEx(DC, DCOrigin);
  // MoveWindowOrg is currently not functioning in the gtk
  // this is a workaround
  ALeft:=AControlOrigin.X-DCOrigin.X;
  ATop:=AControlOrigin.Y-DCOrigin.Y;
  
  SaveIndex := SaveDC(DC);
  FCanvas.Handle:=DC;
{
writeln('DrawMarker A ',FCustomForm.Name
    ,' Control=',AControl.Name,',',AControlOrigin.X,',',AControlOrigin.Y
    ,' DCxy=',DCOrigin.x,',',DCOrigin.y
    ,' DC=',Hexstr(FCustomForm.Canvas.Handle,8),' ',HexStr(Cardinal(Pointer(FCustomForm)),8)
    );
}
  with FCanvas do begin
    OldBrushColor:=Brush.Color;
    Brush.Color:=FMarkerColor;
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
  FCanvas.Handle:=0;
  RestoreDC(DC, SaveIndex);
end;

procedure TControlSelection.DrawRubberband(DC: HDC);
var FormOrigin, DCOrigin, Diff: TPoint;
  SaveIndex: HDC;

  procedure DrawInvertFrameRect(x1,y1,x2,y2:integer);
  var i:integer;

    procedure InvertPixel(x,y:integer);
    //var c:TColor;
    begin
      //c:=FCanvas.Pixels[x,y];
      //c:=c xor $ffffff;
      //FCanvas.Pixels[x,y]:=c;
      FCanvas.MoveTo(Diff.X+x,Diff.Y+y);
      FCanvas.LineTo(Diff.X+x+1,Diff.Y+y);
    end;

  var OldPenColor: TColor;
  begin
    if FCanvas=nil then exit;
    if x1>x2 then begin i:=x1; x1:=x2; x2:=i; end;
    if y1>y2 then begin i:=y1; y1:=y2; y2:=i; end;
    with FCanvas do begin
      OldPenColor:=Brush.Color;
      Pen.Color:=clBlack;
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
      Pen.Color:=OldPenColor;
    end;
  end;

// DrawRubberband
begin
  if (FCustomForm=nil) then exit;
  GetWindowOrgEx(DC, DCOrigin);
  FormOrigin:=FCustomForm.ClientOrigin;
  Diff.X:=FormOrigin.X-DCOrigin.X;
  Diff.Y:=FormOrigin.Y-DCOrigin.Y;
//  OldFormHandle:=FCustomForm.Canvas.Handle;
  SaveIndex:=SaveDC(DC);
  FCanvas.Handle:=DC;
  with FRubberBandBounds do
    DrawInvertFrameRect(Left,Top,Right,Bottom);
  FCanvas.Handle:=0;
  RestoreDC(DC,SaveIndex);
//  FCustomForm.Canvas.Handle:=OldFormHandle;
end;

procedure TControlSelection.SelectWithRubberBand(ACustomForm:TCustomForm; 
  ExclusiveOr:boolean);
var i:integer;

  function ControlInRubberBand(AControl:TControl):boolean;
  var ALeft,ATop,ARight,ABottom:integer;
    Origin:TPoint;
  begin
    Origin:=GetFormRelativeControlTopLeft(AControl);
    ALeft:=Origin.X;
    ATop:=Origin.Y;
    ARight:=ALeft+AControl.Width;
    ABottom:=ATop+AControl.Height;
    Result:=(ALeft<FRubberBandBounds.Right)
        and (ATop<FRubberBandBounds.Bottom)
        and (ARight>=FRubberBandBounds.Left)
        and (ABottom>=FRubberBandBounds.Top);
  end;

// SelectWithRubberBand
begin
  for i:=0 to ACustomForm.ControlCount-1 do
    if ControlInRubberBand(ACustomForm.Controls[i]) then begin
      if IndexOf(ACustomForm.Controls[i])>=0 then begin
        if ExclusiveOr then
          Remove(ACustomForm.Controls[i]);
      end else begin
        Add(ACustomForm.Controls[i]);
      end;
    end;
end;

procedure TControlSelection.SetRubberBandBounds(ARect:TRect);
var i :integer;
begin
  FRubberBandBounds:=ARect;
  with FRubberBandBounds do begin
    if Right<Left then begin
      i:=Left;
      Left:=Right;
      Right:=i;
    end;
    if Bottom<Top then begin
      i:=Top;
      Top:=Bottom;
      Bottom:=i;
    end;
  end;
end;

end.
