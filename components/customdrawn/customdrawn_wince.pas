unit customdrawn_wince;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType,
  // Others only for types
  StdCtrls,
  //
  customdrawndrawers, customdrawn_common;

type
  TCDDrawerWinCE = class(TCDDrawerCommon)
  public
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDCustomTabControl
    procedure DrawCTabControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
    procedure DrawTabSheet(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
    procedure DrawTabs(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
    procedure DrawTab(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
  end;

implementation

procedure TCDDrawerWinCE.DrawButton(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  TmpB: TBitmap;
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
  TmpB := TBitmap.Create;
  TmpB.Width := ASize.cx;
  TmpB.Height := ASize.cy;
  TmpB.Canvas.Brush.Color := AStateEx.RGBColor;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  //  CDButton.SetShape(TmpB);

  // Button image
  if csfSunken in AState then
  begin
    TmpB.Canvas.Brush.Style := bsSolid;
    TmpB.Canvas.Brush.Color := RGBToColor(230, 230, 230);
    TmpB.Canvas.Pen.Color := clBlack;
    TmpB.Canvas.Pen.Style := psSolid;
    TmpB.Canvas.Rectangle(0, 0, TmpB.Canvas.Width, TmpB.Canvas.Height);
  end
  else if csfHasFocus in AState then
  begin
    with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := RGBToColor($FD, $FD, $FD);
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
        Rectangle(1, 1, Width - 1, Height - 1); // The border is thicken when focused
      end;
  end
  else
  begin
    with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := AStateEx.RGBColor;
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
      end;
  end;

  ADest.Draw(0, 0, TmpB);

  TmpB.Free;

  // Button text
  ADest.Font.Assign(AStateEx.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := AStateEx.Caption;
  ADest.TextOut((ASize.cx - ADest.TextWidth(Str)) div 2,
    (ASize.cy - ADest.TextHeight(Str)) div 2, Str);
end;

procedure TCDDrawerWinCE.DrawCTabControl(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
var
  CaptionHeight: Integer;
begin
  // Background
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := AStateEx.ParentRGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.Color := AStateEx.ParentRGBColor;
  ADest.Rectangle(ADestPos.X, ADestPos.Y, ADestPos.X+ASize.cx, ADestPos.Y+ASize.cy);

  CaptionHeight := GetMeasuresEx(ADest, TCDCTABCONTROL_TAB_HEIGHT, AState, AStateEx);

  // frame
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := ColorToRGB($009C9B91);

  if AStateEx.TabCount = 0 then
    ADest.Rectangle(0, 0, ASize.cx - 2, ASize.cy - 2)
  else
    ADest.Rectangle(0, CaptionHeight, ASize.cx -  2, ASize.cy - 2);

  ADest.Pen.Color := ColorToRGB($00BFCED0);
  ADest.Line(ASize.cx - 1, CaptionHeight + 1,
    ASize.cx - 1, ASize.cy - 1);
  ADest.Line(ASize.cx - 1, ASize.cy - 1, 1,
    ASize.cy - 1);

  // Tabs
  ADest.Font.Name := AStateEx.Font.Name;
  ADest.Font.Size := AStateEx.Font.Size;
  DrawTabs(ADest, ADestPos, ASize, AState, AStateEx);
end;

procedure TCDDrawerWinCE.DrawTabSheet(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
begin
  ADest.Brush.Color := AStateEx.RGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := AStateEx.RGBColor;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
end;

procedure TCDDrawerWinCE.DrawTabs(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
var
  IsPainting: Boolean = False;
  i: Integer;
begin
  AStateEx.CurStartLeftPos := 0;
  for i := 0 to AStateEx.Tabs.Count - 1 do
  begin
    if i = AStateEx.LeftmostTabVisibleIndex then
      IsPainting := True;

    if IsPainting then
    begin
      AStateEx.CurTabIndex := i;
      DrawTab(ADest, ADestPos, ASize, AState, AStateEx);
      AStateEx.CurStartLeftPos := AStateEx.CurStartLeftPos + GetMeasuresEx(ADest, TCDCTABCONTROL_TAB_WIDTH, AState, AStateEx);
    end;
  end;
end;

procedure TCDDrawerWinCE.DrawTab(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
var
  IsSelected: Boolean;
  lTabWidth, lTabHeight, lTabTopPos: Integer;
  Points: array of TPoint;
  lCaption: String;
  lTabHeightCorrection: Integer = 0;
begin
  IsSelected := AStateEx.TabIndex = AStateEx.CurTabIndex;

  if not IsSelected then lTabHeightCorrection := 3;

  lTabTopPos := lTabHeightCorrection;
  lTabHeight := GetMeasuresEx(ADest, TCDCTABCONTROL_TAB_HEIGHT, AState, AStateEx)-lTabHeightCorrection;
  lTabWidth := GetMeasuresEx(ADest, TCDCTABCONTROL_TAB_WIDTH, AState, AStateEx);

  // Fill the area inside the outer border
  ADest.Pen.Style := psClear;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.Color := clWhite;
  SetLength(Points, 5);
  Points[0] := Point(AStateEx.CurStartLeftPos, lTabTopPos);
  Points[1] := Point(AStateEx.CurStartLeftPos+lTabWidth-5, lTabTopPos);
  Points[2] := Point(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+5);
  Points[3] := Point(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);
  Points[4] := Point(AStateEx.CurStartLeftPos, lTabTopPos+lTabHeight);
  ADest.Polygon(Points);

  // Draw the outer border only in the top and right sides,
  // and bottom if unselected
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := ColorToRGB($009C9B91);
  ADest.MoveTo(AStateEx.CurStartLeftPos+1, lTabTopPos);
  ADest.LineTo(AStateEx.CurStartLeftPos+lTabWidth-5, lTabTopPos);
  ADest.LineTo(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+5);
  ADest.LineTo(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);

  // If it is selected, add a selection frame
  if IsSelected then
  begin
    ADest.Pen.Color := ColorToRGB($00D6C731);
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsClear;
    ADest.Rectangle(
      AStateEx.CurStartLeftPos+3, lTabTopPos+3,
      AStateEx.CurStartLeftPos+lTabWidth-5, lTabTopPos+lTabHeight-3
      );
  end;

  // Now the text
  lCaption := AStateEx.Tabs.Strings[AStateEx.CurTabIndex];
  ADest.TextOut(AStateEx.CurStartLeftPos+5, lTabTopPos+5, lCaption);
end;

initialization
  RegisterDrawer(TCDDrawerWinCE.Create, dsWinCE);
end.

