{ DividerBevel

  Copyright (C) 2010 Lazarus team

  This library is free software; you can redistribute it and/or modify it
  under the same terms as the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.

}
unit DividerBevel;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources, Forms, Controls, Graphics, Dialogs, Types,
  LCLType, LCLIntf, LCLProc, Math, GraphType, ComCtrls, ExtCtrls;

type
  { TDividerBevel }

  TDividerBevel = class(TGraphicControl)
  private
    FBevelStyle: TBevelStyle;
    FBevelWidth: Integer;
    FCaptionSpacing: Integer;
    FLeftIndent: Integer;
    FOrientation: TTrackBarOrientation;
    FTransparent: Boolean;
    procedure SetBevelStyle(AValue: TBevelStyle);
    procedure SetBevelWidth(AValue: Integer);
    procedure SetCaptionSpacing(const AValue: Integer);
    procedure SetLeftIndent(const AValue: Integer);
    procedure SetOrientation(AValue: TTrackBarOrientation);
    procedure SetTransparent(AValue: Boolean);
  protected
    FBevelHeight: Integer;
    FBevelTop: Integer;
    FNeedCalcSize: Boolean;
    FTextExtent: TSize;
    class function GetControlClassDefaultSize: TSize; override;
    procedure CalcSize;
    procedure Paint; override;
    procedure FontChanged(Sender: TObject); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure TextChanged; override;
    procedure CalculatePreferredSize(
                         var PreferredWidth, PreferredHeight: Integer;
                         {%H-}WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Align;
    property AutoSize default True;
    property Anchors;
    property BevelStyle: TBevelStyle read FBevelStyle write SetBevelStyle default bsLowered;
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth default -1;
    property BiDiMode;
    property BorderSpacing;
    property CaptionSpacing: Integer read FCaptionSpacing write SetCaptionSpacing
             default 10;
    property Color;
    property Constraints;
    property Font;
    property Hint;
    property LeftIndent: Integer read FLeftIndent write SetLeftIndent default 60;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation
             default trHorizontal;
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
end;

procedure Register;

implementation

procedure Register;
begin
  {$I dividerbevel_icon.lrs}
  RegisterComponents('LazControls', [TDividerBevel]);
end;

{ TDividerBevel }

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
  if AutoSize then begin
    InvalidatePreferredSize;
    AdjustSize;
  end else
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

procedure TDividerBevel.SetOrientation(AValue: TTrackBarOrientation);
begin
  if FOrientation = AValue then Exit;
  FOrientation := AValue;
  if not (csLoading in ComponentState) then SetBounds(Left, Top, Height, Width);
  if AutoSize then
    begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
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

procedure TDividerBevel.CalcSize;
begin
  if not FNeedCalcSize then exit;
  FNeedCalcSize := False;
  if Caption = '' then
    FTextExtent := Canvas.TextExtent(' ')
  else
    FTextExtent := Canvas.TextExtent(Caption);
  if FBevelWidth < 0 then
    FBevelHeight := Max(3, FTextExtent.cy div 5)
  else
    FBevelHeight := FBevelWidth;
  FBevelTop := Max((FTextExtent.cy - FBevelHeight) div 2, 0);
end;

procedure TDividerBevel.Paint;
var
  aBevel: TGraphicsBevelCut;
  aHorizontal: Boolean;
  aIndent, aRight, j: Integer;
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
  aHorizontal := (Orientation = trHorizontal);

  if aHorizontal then begin
    PaintRect.Left := 0;
    PaintRect.Top := FBevelTop;
    PaintRect.Bottom := PaintRect.Top + FBevelHeight;
  end else begin
    PaintRect.Left := FBevelTop;
    PaintRect.Top := 0;
    PaintRect.Right := PaintRect.Left + FBevelHeight;
  end;

  if Caption = '' then begin
    if aHorizontal then
      PaintRect.Right := Width
    else
      PaintRect.Bottom := Height;
    Canvas.Frame3D(PaintRect, 1, aBevel);
    exit;
  end;

  if FLeftIndent > 0 then
    aIndent := FLeftIndent
  else
    if FLeftIndent = 0 then
      aIndent := 0
    else begin
      j := 2*FCaptionSpacing + FTextExtent.cx;
      if aHorizontal then
        aIndent := (Width - j) div 2
      else
        aIndent := (Height - j) div 2;
    end;

  if not IsRightToLeft or not aHorizontal then
    aRight := aIndent
  else begin
    aRight := Width - FTextExtent.cx - FCaptionSpacing - aIndent;
    if aIndent > 0 then dec(aRight, FCaptionSpacing);
  end;
  if aRight > 0 then begin
    if aHorizontal then
      PaintRect.Right := aRight
    else
      PaintRect.Bottom := aRight;
    Canvas.Frame3D(PaintRect, 1, aBevel);
  end;

  if aIndent > 0 then inc(aIndent, FCaptionSpacing);
  if aHorizontal then begin
    PaintRect.Left := aRight + FCaptionSpacing + FTextExtent.cx;
    if aIndent <> 0 then inc(PaintRect.Left, FCaptionSpacing);
    PaintRect.Top := FBevelTop;
    PaintRect.Right := Width;
    PaintRect.Bottom := FBevelTop + FBevelHeight;
  end else begin
    PaintRect.Left := FBevelTop;
    PaintRect.Top := aRight + FCaptionSpacing + FTextExtent.cx;
    if aIndent <> 0 then inc(PaintRect.Top, FCaptionSpacing);
    PaintRect.Right := FBevelTop + FBevelHeight;
    PaintRect.Bottom := Height;
  end;
  Canvas.Frame3D(PaintRect, 1, aBevel);

  Canvas.Brush.Style := bsClear;
  j := Max((FBevelHeight - FTextExtent.cy) div 2, 0);
  if aHorizontal then begin
    Canvas.Font.Orientation := 0;
    if not IsRightToLeft then
      Canvas.TextOut(aIndent, j, Caption)
    else
      Canvas.TextOut(Width - FTextExtent.cx - aIndent, j, Caption);
  end else begin
    Canvas.Font.Orientation := 900;
    Canvas.TextOut(j, aIndent + FTextExtent.cx, Caption);
  end;
end;

procedure TDividerBevel.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  FNeedCalcSize := True;
  Invalidate;
end;

procedure TDividerBevel.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value then begin
    InvalidatePreferredSize;
    AdjustSize;
  end;
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
  if Orientation = trHorizontal then begin
    PreferredHeight := Max(FTextExtent.cy, FBevelHeight);
    PreferredWidth := 0;
  end else begin
    PreferredHeight := 0;
    PreferredWidth := Max(FTextExtent.cy, FBevelHeight);
  end;
end;

constructor TDividerBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBevelStyle := bsLowered;
  FBevelWidth := -1;
  FCaptionSpacing := 10;
  FLeftIndent := 60;
  FOrientation := trHorizontal;
  FTransparent := True;
  FNeedCalcSize := True;
  if (AOwner = nil) or not (csLoading in AOwner.ComponentState) then
    Font.Style := Font.Style + [fsBold];
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
  AutoSize := True;
end;

end.

