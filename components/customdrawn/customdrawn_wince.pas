unit customdrawn_wince;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types,
  // LazUtils
  lazutf8,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType,
  // Others only for types
  StdCtrls,
  //
  customdrawndrawers, customdrawn_common;

type

  { TCDDrawerWinCE }

  TCDDrawerWinCE = class(TCDDrawerCommon)
  public
    procedure LoadFallbackPaletteColors; override;
    function GetDrawStyle: TCDDrawStyle; override;
    // General drawing routines
    procedure DrawRaisedFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); override;
    procedure DrawSunkenFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize); override;
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDButtonStateEx); override;
    // TCDEdit
    procedure DrawEditFrame(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawCaret(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    // TCDCheckBox
    procedure DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDGroupBox
    procedure DrawGroupBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDCustomTabControl
    procedure DrawTab(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
  end;

implementation

procedure TCDDrawerWinCE.LoadFallbackPaletteColors;
begin
  Palette.Form := $EFDFCE;
  Palette.BtnFace := $EFDFCE;
  Palette.BtnShadow := clBlack;
end;

function TCDDrawerWinCE.GetDrawStyle: TCDDrawStyle;
begin
  Result := dsWinCE;
end;

procedure TCDDrawerWinCE.DrawRaisedFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize);
begin
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := clBlack;
  ADest.Rectangle(Bounds(ADestPos.X, ADestPos.Y, ASize.cx, ASize.cy));
end;

procedure TCDDrawerWinCE.DrawSunkenFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize);
begin
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := clBlack;
  ADest.Rectangle(Bounds(ADestPos.X, ADestPos.Y, ASize.cx, ASize.cy));
end;

procedure TCDDrawerWinCE.DrawButton(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDButtonStateEx);
var
  Str: string;
begin
  // Button background
  if csfSunken in AState then
  begin
    ADest.Brush.Style := bsSolid;
    ADest.Brush.Color := Palette.BtnShadow;
    ADest.Pen.Color := clBlack;
    ADest.Pen.Style := psSolid;
    ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
  end
  else
  begin
    ADest.Brush.Style := bsSolid;
    ADest.Brush.Color := AStateEx.RGBColor;
    ADest.Pen.Color := clBlack;
    ADest.Pen.Style := psSolid;
    ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
  end;

  // Focus
  if (csfHasFocus in AState) and not (csfSunken in AState) then
    DrawFocusRect(ADest, Point(4, 4), Size(ASize.cx-8, ASize.cy-8));

  // Button text
  ADest.Font.Assign(AStateEx.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  if csfSunken in AState then ADest.Font.Color := clWhite;
  Str := AStateEx.Caption;
  ADest.TextOut((ASize.cx - ADest.TextWidth(Str)) div 2,
    (ASize.cy - ADest.TextHeight(Str)) div 2, Str);
end;

procedure TCDDrawerWinCE.DrawEditFrame(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
begin
  // The frame
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := clBlack;
  ADest.Pen.Style := psSolid;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
end;

procedure TCDDrawerWinCE.DrawCaret(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lTextTopSpacing, lCaptionHeight: Integer;
  lControlText, lTmpText: string;
  lCaretPixelPos: Integer;
begin
  if not AStateEx.CaretIsVisible then Exit;

  lControlText := AStateEx.Caption;
  lCaptionHeight := GetMeasuresEx(ADest, TCDCONTROL_CAPTION_HEIGHT, AState, AStateEx);
  lTextTopSpacing := GetMeasures(TCDEDIT_TOP_TEXT_SPACING);

  lTmpText := UTF8Copy(lControlText, 1, AStateEx.CaretPos.X-AStateEx.VisibleTextStart.X+1);
  lCaretPixelPos := ADest.TextWidth(lTmpText) + 3;
  ADest.Pen.Color := clBlack;
  ADest.Pen.Style := psSolid;
  ADest.Line(lCaretPixelPos, lTextTopSpacing, lCaretPixelPos, lTextTopSpacing+lCaptionHeight);
  ADest.Line(lCaretPixelPos+1, lTextTopSpacing, lCaretPixelPos+1, lTextTopSpacing+lCaptionHeight);
end;

procedure TCDDrawerWinCE.DrawCheckBoxSquare(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  lHalf, lSquareHalf, lSquareHeight: Integer;
  lColor: TColor;
begin
  lHalf := ASize.cy div 2;
  lSquareHalf := GetMeasures(TCDCHECKBOX_SQUARE_HALF_HEIGHT);
  lSquareHeight := GetMeasures(TCDCHECKBOX_SQUARE_HEIGHT);

  // the square itself
  ADest.Brush.Color := clWhite;
  ADest.Pen.Style := psSolid;
  if csfSunken in AState then ADest.Pen.Color := clGray
  else ADest.Pen.Color := clBlack;
  ADest.Rectangle(
    1,
    lHalf - lSquareHalf,
    lSquareHeight+1,
    lHalf + lSquareHalf);

  // The selection inside the square
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := RGBToColor($31, $C6, $D6);
  ADest.Pen.Style := psSolid;
  if csfHasFocus in AState then
  begin
    ADest.Rectangle(
      2,
      lHalf - lSquareHalf+1,
      lSquareHeight,
      lHalf + lSquareHalf-1);
  end;
end;

procedure TCDDrawerWinCE.DrawGroupBox(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  FCaptionMiddle: integer;
  lTextSize: TSize;
  lCaption: String;
begin
  FCaptionMiddle := ADest.TextHeight(cddTestStr) div 2;
  if FCaptionMiddle = 0 then FCaptionMiddle := AStateEx.Font.Size div 2;
  if FCaptionMiddle = 0 then FCaptionMiddle := 5;

  // Background
  ADest.Brush.Color := AStateEx.ParentRGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := AStateEx.ParentRGBColor;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);

  // frame
  ADest.Pen.Color := clBlack;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Rectangle(0, FCaptionMiddle, ASize.cx, ASize.cy);

  // ToDo: Make the caption smaller if it is too big
  lCaption := AStateEx.Caption;
  lTextSize := ADest.TextExtent(lCaption);

  // fill the text background
  ADest.Brush.Style := bsSolid;
  ADest.Brush.Color := AStateEx.ParentRGBColor;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(Bounds(FCaptionMiddle, 0, lTextSize.cx+5, lTextSize.cy));

  // paint text
  ADest.Pen.Style := psClear;
  ADest.Brush.Style := bsClear;
  ADest.TextOut(FCaptionMiddle+3, 0, lCaption);
end;

procedure TCDDrawerWinCE.DrawTab(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
var
  IsSelected, IsAddButton: Boolean;
  lTabWidth, lTabHeight, lTabTopPos: Integer;
  Points: array of TPoint;
  lCaption: String;
  lTabHeightCorrection: Integer = 0;
begin
  IsSelected := AStateEx.TabIndex = AStateEx.CurTabIndex;
  IsAddButton := AStateEx.CurTabIndex = AStateEx.Tabs.Count;

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
  if IsAddButton then lCaption := '+'
  else lCaption := AStateEx.Tabs.Strings[AStateEx.CurTabIndex];
  ADest.TextOut(AStateEx.CurStartLeftPos+5, lTabTopPos+5, lCaption);
end;

initialization
  RegisterDrawer(TCDDrawerWinCE.Create, dsWinCE);
end.

