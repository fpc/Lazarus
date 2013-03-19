{ DividerBevel

  Copyright (C) 2010 Lazarus team

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
  LCLType, LCLIntf, LCLProc, math, GraphType, ExtCtrls;

type
  { TDividerBevel }

  TDividerBevel = class(TGraphicControl)
  private
    FBevelStyle: TBevelStyle;
    FBevelWidth: Integer;
    FCaptionSpacing: Integer;
    FLeftIndent: Integer;
    FTextHeight, FTextWidth: Integer;
    FBevelTop: Integer;
    FBevelHeight: Integer;
    FNeedCalcSize: Boolean;
    FTransparent: Boolean;
    procedure CalcSize;
    procedure SetBevelStyle(AValue: TBevelStyle);
    procedure SetBevelWidth(AValue: Integer);
    procedure SetCaptionSpacing(const AValue: Integer);
    procedure SetLeftIndent(const AValue: Integer);
    procedure SetTransparent(AValue: Boolean);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure Paint; override;
    procedure FontChanged(Sender: TObject); override;
    procedure TextChanged; override;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: Integer;
                         WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Align;
    property AutoSize;
    property Anchors;
    property BevelStyle: TBevelStyle read FBevelStyle write SetBevelStyle default bsLowered;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth default -1;
    property BiDiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property Font;
    property Hint;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Transparent: Boolean read FTransparent write SetTransparent default True;
    property Visible;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
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
  FNeedCalcSize := False;
  if Caption = '' then
    TextExt := Canvas.TextExtent(' ')
  else
    TextExt := Canvas.TextExtent(Caption);
  FTextHeight := TextExt.cy;
  FTextWidth := TextExt.cx;
  if FBevelWidth < 0 then
    FBevelHeight := Max(3, FTextHeight div 5)
  else
    FBevelHeight := FBevelWidth;
  FTextHeight := Max(FTextHeight, FBevelHeight + 2);
  FBevelTop := (FTextHeight - FBevelHeight) div 2 + 1;
end;

procedure TDividerBevel.SetBevelStyle(AValue: TBevelStyle);
begin
  if FBevelStyle = AValue then Exit;
  FBevelStyle := AValue;
  Invalidate;
end;

procedure TDividerBevel.SetBevelWidth(AValue: Integer);
begin
  if FBevelWidth = AValue then Exit;
  FBevelWidth := AValue;
  FNeedCalcSize := True;
  Invalidate;
end;

procedure TDividerBevel.SetCaptionSpacing(const AValue: Integer);
begin
  if FCaptionSpacing = AValue then Exit;
  FCaptionSpacing := AValue;
  Invalidate;
end;

procedure TDividerBevel.SetLeftIndent(const AValue: Integer);
begin
  if FLeftIndent = AValue then Exit;
  FLeftIndent := AValue;
  Invalidate;
end;

procedure TDividerBevel.SetTransparent(AValue: Boolean);
begin
  if FTransparent = AValue then Exit;
  FTransparent := AValue;
  Invalidate;
end;

class function TDividerBevel.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 240;
  Result.CY := 17;
end;

procedure TDividerBevel.Paint;
var
  aBevel: TGraphicsBevelCut;
  aIndent, aRight: Integer;
  PaintRect: TRect;
begin
  CalcSize;
  if not FTransparent then begin
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  end;
  if FBevelStyle = bsLowered then
    aBevel := bvLowered
  else
    aBevel := bvRaised;
  Canvas.Pen.Color := Font.Color;
  PaintRect.Top := FBevelTop;
  PaintRect.Bottom := FBevelTop + FBevelHeight;
  PaintRect.Left := 0;
  if Caption = '' then begin
    PaintRect.Right := Width;
    Canvas.Frame3D(PaintRect, 1, aBevel);
    exit;
  end;

  if FLeftIndent > 0 then
    aIndent := FLeftIndent + FCaptionSpacing
  else
    aIndent := 0;

  if not IsRightToLeft then
    aRight := FLeftIndent
  else
    aRight := Width - FTextWidth - FCaptionSpacing - aIndent;
  if aRight > 0 then begin
    PaintRect.Right := aRight;
    Canvas.Frame3D(PaintRect, 1, aBevel);
  end;

  PaintRect.Top := FBevelTop;
  PaintRect.Bottom := FBevelTop + FBevelHeight;
  PaintRect.Left := aRight + FTextWidth + FCaptionSpacing;
  if aIndent > 0 then
    PaintRect.Left := PaintRect.Left + FCaptionSpacing;
  PaintRect.Right := Width;
  Canvas.Frame3D(PaintRect, 1, aBevel);

  Canvas.Brush.Style := bsClear;
  if not IsRightToLeft then
    Canvas.TextOut(aIndent, 0, Caption)
  else
    Canvas.TextOut(Width - FTextWidth - aIndent, 0, Caption)
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

procedure TDividerBevel.CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
  WithThemeSpace: Boolean);
begin
  FNeedCalcSize := True;
  CalcSize;
  if FLeftIndent > 0 then
    PreferredWidth := FTextWidth + 2*(FLeftIndent + FCaptionSpacing)
  else
    PreferredWidth := 2*FTextWidth + FCaptionSpacing;
  PreferredHeight := FTextHeight;
end;

constructor TDividerBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption];
  FBevelStyle := bsLowered;
  FBevelWidth := -1;
  FCaptionSpacing := 10;
  FTransparent := True;
  LeftIndent := 60;
  FNeedCalcSize := True;
  if (AOwner = nil) or not (csLoading in AOwner.ComponentState) then
    Font.Style := Font.Style + [fsBold];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

end.

