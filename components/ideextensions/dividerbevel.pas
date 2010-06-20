{ IdeGroupBox - A specially themed groupbox used inside the IDE

  Copyright (C) 2010 <name of author> <contact>

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit DividerBevel;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, types,
  LCLType, LCLIntf, LCLProc, math, ExtCtrls;

type

  { TDividerBevel }

  TDividerBevel = class(TGraphicControl)
  private
    FCaptionSpacing: Integer;
    FLeftIndent: Integer;
    FTextHeight, FTextWidth: Integer;
    FBevelTop: Integer;
    FBevelHeight: Integer;
    FNeedCalcSize: Boolean;

    procedure CalcSize;
    procedure SetCaptionSpacing(const AValue: Integer);
    procedure SetLeftIndent(const AValue: Integer);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
    procedure FontChanged(Sender: TObject); override;
    procedure TextChanged; override;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: integer;
                         WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Align;
    property Autosize;
    property Anchors;
    property Color;
    property Constraints;
    property Font;
    property Hint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;
  published
    property CaptionSpacing: Integer read FCaptionSpacing write SetCaptionSpacing
             default 10;
    property LeftIndent: Integer read FLeftIndent write SetLeftIndent default 60;
end;

procedure Register;

implementation

procedure Register;
begin
  {$I dividerbevel_icon.lrs}
  RegisterComponents('IdeExtensions',[TDividerBevel]);
end;

{ TDividerBevel }

procedure TDividerBevel.CalcSize;
var
  TextExt: TSize;
begin
  if not FNeedCalcSize then exit;
  FNeedCalcSize := True;
  if Caption = '' then
    TextExt := Canvas.TextExtent(' ')
  else
    TextExt := Canvas.TextExtent(Caption);
  FTextHeight := TextExt.cy;
  FTextWidth := TextExt.cx;
  FBevelHeight := Max(3, FTextHeight div 5);
  FTextHeight := Max(FTextHeight, FBevelHeight + 2);
  FBevelTop := (FTextHeight - FBevelHeight) div 2;
end;

procedure TDividerBevel.SetCaptionSpacing(const AValue: Integer);
begin
  if FCaptionSpacing = AValue then
    exit;
  FCaptionSpacing := AValue;
  Invalidate;
end;

procedure TDividerBevel.SetLeftIndent(const AValue: Integer);
begin
  if FLeftIndent = AValue then
    exit;
  FLeftIndent := AValue;
  Invalidate;
end;

class function TDividerBevel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 150;
  Result.CY := 17;
end;

procedure TDividerBevel.Paint;
var
  PaintRect: TRect;
begin
  CalcSize;
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);

  Canvas.Pen.Color := Font.Color;
  PaintRect.Top := FBevelTop;
  PaintRect.Bottom := FBevelTop + FBevelHeight;
  PaintRect.Left := 0;
  if Caption = '' then begin
    PaintRect.Right := Width;
    Canvas.Frame3D(PaintRect, 1, bvLowered);
    exit;
  end;
  PaintRect.Right := FLeftIndent;
  Canvas.Frame3D(PaintRect, 1, bvLowered);

  PaintRect.Top := FBevelTop;
  PaintRect.Bottom := FBevelTop + FBevelHeight;
  PaintRect.Left := FLeftIndent + 2*FCaptionSpacing + FTextWidth;
  PaintRect.Right := Width;
  Canvas.Frame3D(PaintRect, 1, bvLowered);

  Canvas.TextOut(FLeftIndent + FCaptionSpacing, 0, Caption);
end;

procedure TDividerBevel.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FNeedCalcSize := True;
  Invalidate;
end;

procedure TDividerBevel.TextChanged;
begin
  inherited TextChanged;
  FNeedCalcSize := True;
  Invalidate;
end;

procedure TDividerBevel.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  FNeedCalcSize := True;
  CalcSize;
  PreferredWidth := FTextWidth + 2*FLeftIndent + 2*FCaptionSpacing;
  PreferredHeight := FTextHeight;
end;

constructor TDividerBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption];
  Font.Style := Font.Style + [fsBold];
  FCaptionSpacing := 10;
  LeftIndent := 60;
  FNeedCalcSize := True;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
