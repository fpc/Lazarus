{
 /***************************************************************************
                              lazcanvas.pas
                              ---------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Felipe Monteiro de Carvalho

  Abstract:
    Classes and functions for extending TFPImageCanvas to support more stretching
    filters and to support all features from the LCL TCanvas
}
unit lazcanvas;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, contnrs, Math,
  // FCL-Image
  fpimgcanv, fpcanvas, fpimage, clipping,
  // regions
  lazregions;

type

  { TFPSharpInterpolation }

  // This does a very sharp and square interpolation for stretching,
  // similar to StretchBlt from the Windows API
  TFPSharpInterpolation = class (TFPCustomInterpolation)
  protected
    procedure Execute (x,y,w,h : integer); override;
  end;

  { TLazCanvasState }

  TLazCanvasState = class
  public
    Brush: TFPCustomBrush;
    Pen: TFPCustomPen;
    BaseWindowOrg: TPoint;
    WindowOrg: TPoint;
    destructor Destroy; override;
  end;

  { TLazCanvas }

  TLazCanvas = class(TFPImageCanvas)
  private
    FAssignedBrush: TFPCustomBrush;
    FAssignedPen: TFPCustomPen;
    FBaseWindowOrg: TPoint;
    FUseRegionClipping: Boolean;
    FLazClipRegion: TLazRegion;
    FWindowOrg: TPoint; // already in absolute coords with BaseWindowOrg summed up
    GraphicStateStack: TObjectStack; // TLazCanvasState
    function GetAssignedBrush: TFPCustomBrush;
    function GetAssignedPen: TFPCustomPen;
    function GetWindowOrg: TPoint;
    procedure SetWindowOrg(AValue: TPoint);
  protected
    procedure SetColor (x,y:integer; const AValue:TFPColor); override;
    // Routines broken in FPC
    procedure DoPolygonFill (const points:array of TPoint); override;
  public
    constructor create (AnImage : TFPCustomImage);
    destructor destroy; override;
    procedure SetLazClipRegion(ARegion: TLazRegion);
    // Canvas states stack
    procedure SaveState;
    procedure RestoreState;
    // A simple operation to bring the Canvas in the default LCL TCanvas state
    procedure ResetCanvasState;
    // Compatibility with older FPC versions
    {$if defined(ver2_4) or defined(ver2_5)}
    procedure FillRect(const ARect: TRect);
    procedure FillRect(X1,Y1,X2,Y2: Integer);
    {$endif}
    // Utilized by LCLIntf.SelectObject and by RestoreState
    // This needed to be added because Pen/Brush.Assign raises exceptions
    procedure AssignPenData(APen: TFPCustomPen);
    procedure AssignBrushData(ABrush: TFPCustomBrush);
    // These properties are utilized to implement LCLIntf.SelectObject
    // to keep track of which brush handle was assigned to this canvas
    // They are not utilized by TLazCanvas itself
    property AssignedPen: TFPCustomPen read GetAssignedPen write FAssignedPen;
    property AssignedBrush: TFPCustomBrush read GetAssignedBrush write FAssignedBrush;
    //
    // SetWindowOrg operations will be relative to BaseWindowOrg,
    // This is very useful for implementing the non-native wincontrol,
    // because operations of SetWindowOrg inside a non-native wincontrol will be
    // based upon the BaseWindowOrg which is set relative to the Form canvas
    property BaseWindowOrg: TPoint read FBaseWindowOrg write FBaseWindowOrg;
    property ClipRegion: TLazRegion read FLazClipRegion write FLazClipRegion;
    property UseRegionClipping: Boolean read FUseRegionClipping write FUseRegionClipping; // when false ClipRect is used
    property WindowOrg: TPoint read GetWindowOrg write SetWindowOrg;
  end;

implementation

{ TLazCanvasState }

destructor TLazCanvasState.Destroy;
begin
  if Brush <> nil then Brush.Free;
  if Pen <> nil then Pen.Free;
  inherited Destroy;
end;

{ TLazCanvas }

function TLazCanvas.GetAssignedBrush: TFPCustomBrush;
begin
  if FAssignedBrush = nil then
    Result := TFPEmptyBrush.Create
  else
    Result := FAssignedBrush;
end;

function TLazCanvas.GetAssignedPen: TFPCustomPen;
begin
  if FAssignedPen = nil then
    Result := TFPEmptyPen.Create
  else
    Result := FAssignedPen;
end;

function TLazCanvas.GetWindowOrg: TPoint;
begin
  Result := Point(FWindowOrg.X-FBaseWindowOrg.X, FWindowOrg.Y-FBaseWindowOrg.Y)
end;

procedure TLazCanvas.SetWindowOrg(AValue: TPoint);
begin
  FWindowOrg.X := AValue.X+FBaseWindowOrg.X;
  FWindowOrg.Y := AValue.Y+FBaseWindowOrg.Y;
end;

procedure TLazCanvas.SetColor(x, y: integer; const AValue: TFPColor);
var
  lx, ly: Integer;
  OldClipping: Boolean;
begin
  lx := x + FWindowOrg.X;
  ly := y + FWindowOrg.Y;
  if Clipping and FUseRegionClipping and (not FLazClipRegion.IsPointInRegion(lx, ly)) then
    Exit;
  OldClipping := Clipping;
  Clipping := False; // Work around to FImage being private in FPC 2.6 or inferior =(
  inherited SetColor(lx, ly, AValue);
  Clipping := OldClipping;
end;

procedure TLazCanvas.DoPolygonFill(const points: array of TPoint);
var
  lBoundingBox: TRect;
  x, y, i: integer;
begin
  // Find the Bounding Box of the Polygon
  lBoundingBox := Rect(0, 0, 0, 0);
  for i := low(Points) to High(Points) do
  begin
    lBoundingBox.Left := Min(Points[i].X, lBoundingBox.Left);
    lBoundingBox.Top := Min(Points[i].Y, lBoundingBox.Top);
    lBoundingBox.Right := Max(Points[i].X, lBoundingBox.Right);
    lBoundingBox.Bottom := Max(Points[i].Y, lBoundingBox.Bottom);
  end;

  // Now scan all points using IsPointInPolygon
  for x := lBoundingBox.Left to lBoundingBox.Right do
    for y := lBoundingBox.Top to lBoundingBox.Bottom do
    begin
      if IsPointInPolygon(X, Y, Points) then SetColor(X, Y, Brush.FPColor);
    end;
end;

constructor TLazCanvas.create(AnImage: TFPCustomImage);
begin
  inherited Create(AnImage);
  GraphicStateStack := TObjectStack.Create;
end;

destructor TLazCanvas.destroy;
begin
  GraphicStateStack.Free;
  if FAssignedBrush <> nil then FAssignedBrush.Free;
  if FAssignedPen <> nil then FAssignedPen.Free;
  inherited destroy;
end;

procedure TLazCanvas.SetLazClipRegion(ARegion: TLazRegion);
begin
  if ARegion.IsSimpleRectRegion then
  begin
    Clipping := True;
    ClipRect := TLazRegionRect(ARegion.Parts.Items[0]).Rect;
    FLazClipRegion := ARegion;
  end;
end;

procedure TLazCanvas.SaveState;
var
  lState: TLazCanvasState;
begin
  lState := TLazCanvasState.Create;

  lState.Brush := Brush.CopyBrush;
  lState.Pen := Pen.CopyPen;

  GraphicStateStack.Push(lState);
end;

procedure TLazCanvas.RestoreState;
var
  lState: TLazCanvasState;
begin
  lState := TLazCanvasState(GraphicStateStack.Pop());
  if lState = nil then Exit;

  AssignPenData(lState.Pen);
  AssignBrushData(lState.Brush);

  lState.Free;
end;

procedure TLazCanvas.ResetCanvasState;
begin
  Pen.FPColor := colBlack;
  Pen.Style := psSolid;

  Brush.FPColor := colWhite;
  Brush.Style := bsSolid;
end;

{$if defined(ver2_4) or defined(ver2_5)}
procedure TLazCanvas.FillRect(const ARect: TRect);
begin
  if (Brush.style <> bsClear) then
    begin
    //if not (brush is TFPCustomDrawBrush) then
      DoRectangleFill (ARect)
    //else
    //  with ARect do
    //    TFPCustomDrawBrush(Brush).Rectangle (left,top,right,bottom);
    end;
end;

procedure TLazCanvas.FillRect(X1, Y1, X2, Y2: Integer);
begin
  FillRect (Rect(X1,Y1,X2,Y2));
end;
{$endif}

procedure TLazCanvas.AssignPenData(APen: TFPCustomPen);
begin
  if APen = nil then Exit;
  Pen.FPColor := APen.FPColor;
  Pen.Style := APen.Style;
  Pen.Width := APen.Width;
end;

procedure TLazCanvas.AssignBrushData(ABrush: TFPCustomBrush);
begin
  if ABrush = nil then Exit;
  Brush.FPColor := ABrush.FPColor;
  Brush.Style := ABrush.Style;
end;

{ TFPWindowsSharpInterpolation }

procedure TFPSharpInterpolation.Execute(x, y, w, h: integer);
// paint Image on Canvas at x,y,w*h
var
  srcx, srcy: Integer; // current coordinates in the source image
  dx, dy: Integer; // current coordinates in the destination canvas
  lWidth, lHeight: Integer; // Image size
begin
  if (w<=0) or (h<=0) or (image.Width=0) or (image.Height=0) then
    exit;

  lWidth := Image.Width-1;
  lHeight := Image.Height-1;

  for dx := 0 to w-1 do
   for dy := 0 to h-1 do
   begin
     srcx := Round((dx / w) * lWidth);
     srcy := Round((dy / w) * lHeight);
     Canvas.Colors[dx+x, dy+y] := Image.Colors[srcx, srcy];
   end;
end;

end.

