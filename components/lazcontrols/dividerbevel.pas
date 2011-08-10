{ DividerBevel

  Copyright (C) 2010 <name of author> <contact>

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the copyright.

}
unit DividerBevel;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs, Types,
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
    property BorderSpacing;
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
  RegisterComponents('LazControls',[TDividerBevel]);
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
  if WithThemeSpace then ;
  FNeedCalcSize := True;
  CalcSize;
  PreferredWidth := FTextWidth + 2*FLeftIndent + 2*FCaptionSpacing;
  PreferredHeight := FTextHeight;
end;

constructor TDividerBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption];
  FCaptionSpacing := 10;
  LeftIndent := 60;
  FNeedCalcSize := True;
  if (AOwner = nil) or not(csLoading in AOwner.ComponentState) then
    Font.Style := Font.Style + [fsBold];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.
