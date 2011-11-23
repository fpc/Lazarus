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
  Classes, SysUtils,
  // FCL-Image
  fpimgcanv, fpcanvas, fpimage,
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

  { TLazCanvas }

  TLazCanvas = class(TFPImageCanvas)
  private
    FAssignedBrush: TFPCustomBrush;
    FAssignedPen: TFPCustomPen;
    FUseSimpleRectClipping: Boolean;
    FLazClipRegion: TLazRegion;
    function GetAssignedBrush: TFPCustomBrush;
    function GetAssignedPen: TFPCustomPen;
  protected
    procedure SetColor (x,y:integer; const AValue:TFPColor); override;
  public
    constructor create (AnImage : TFPCustomImage);
    destructor destroy; override;
    procedure SetLazClipRegion(ARegion: TLazRegion);
    // Utilized by LCLIntf.SelectObject
    procedure AssignPenData(APen: TFPCustomPen);
    procedure AssignBrushData(ABrush: TFPCustomBrush);
    // These properties are utilized to implement LCLIntf.SelectObject
    property AssignedPen: TFPCustomPen read GetAssignedPen write FAssignedPen;
    property AssignedBrush: TFPCustomBrush read GetAssignedBrush write FAssignedBrush;
    //
    property UseSimpleRectClipping: Boolean read FUseSimpleRectClipping write FUseSimpleRectClipping;
  end;

implementation

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

procedure TLazCanvas.SetColor(x, y: integer; const AValue: TFPColor);
begin
  inherited SetColor(x, y, AValue);
end;

constructor TLazCanvas.create(AnImage: TFPCustomImage);
begin
  inherited Create(AnImage);
  FUseSimpleRectClipping := True;
end;

destructor TLazCanvas.destroy;
begin
  if FAssignedBrush <> nil then FAssignedBrush.Free;
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

procedure TLazCanvas.AssignPenData(APen: TFPCustomPen);
begin
  Pen.FPColor := APen.FPColor;
  Pen.Style := APen.Style;
  Pen.Width := APen.Width;
end;

procedure TLazCanvas.AssignBrushData(ABrush: TFPCustomBrush);
begin
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

