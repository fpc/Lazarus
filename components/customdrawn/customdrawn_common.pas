unit customdrawn_common;

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
  customdrawndrawers;

type

  { TCDDrawerCommon }

  TCDDrawerCommon = class(TCDDrawer)
  public
    // General
    function GetMeasures(AMeasureID: Integer): Integer; override;
    function GetMeasuresEx(ADest: TCanvas; AMeasureID: Integer;
      AState: TCDControlState; AStateEx: TCDControlStateEx): Integer; override;
    procedure CalculatePreferredSize(ADest: TCanvas; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    function GetColor(AColorID: Integer): TColor; override;
    function GetClientArea(ADest: TCanvas; ASize: TSize; AControlId: TCDControlID;
      AState: TCDControlState; AStateEx: TCDControlStateEx): TRect; override;
    procedure DrawControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AControl: TCDControlID; AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDEdit
    procedure DrawEditBackground(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEdit(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    // TCDCheckBox
    procedure DrawCheckBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDGroupBox
    procedure DrawGroupBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDTrackBar
    procedure DrawTrackBar(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDTrackBarStateEx); override;
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

{ TCDDrawerCommon }

function TCDDrawerCommon.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureID of
  TCDEDIT_LEFT_TEXT_SPACING: Result := 4;
  TCDEDIT_RIGHT_TEXT_SPACING: Result := 3;
  else
    Result := 0;
  end;
end;

function TCDDrawerCommon.GetMeasuresEx(ADest: TCanvas; AMeasureID: Integer;
  AState: TCDControlState; AStateEx: TCDControlStateEx): Integer;
const
  TCDTabControl_Common_TabCaptionExtraWidth = 20;
var
  ATabsStateEx: TCDCTabControlStateEx absolute AStateEx;
  lCaption: String;
begin
  ADest.Font.Assign(AStateEx.Font);

  case AMeasureID of
  TCDCONTROL_CAPTION_WIDTH:  Result := ADest.TextWidth(AStateEx.Caption);
  TCDCONTROL_CAPTION_HEIGHT: Result := ADest.TextHeight('ŹÇ')+3;
  TCDCTABCONTROL_TAB_HEIGHT: Result := ADest.TextHeight('ŹÇ')+10;
  TCDCTABCONTROL_TAB_WIDTH:
  begin
    lCaption := ATabsStateEx.Tabs.Strings[ATabsStateEx.CurTabIndex];
    Result := ADest.TextWidth(lCaption) + TCDTabControl_Common_TabCaptionExtraWidth;
  end
  else
    Result := 0;
  end;
end;

procedure TCDDrawerCommon.CalculatePreferredSize(ADest: TCanvas;
  AControlId: TCDControlID; AState: TCDControlState;
  AStateEx: TCDControlStateEx; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;
  PreferredHeight := 0;

  case AControlId of
  // In the LCL TEdit AutoSizes only its Height, so follow this here
  cidEdit: PreferredHeight := GetMeasuresEx(ADest, TCDCONTROL_CAPTION_HEIGHT, AState, AStateEx)+5;
  cidCheckBox:
  begin
    if AStateEx.AutoSize then
      PreferredWidth := 21 + GetMeasuresEx(ADest, TCDCONTROL_CAPTION_WIDTH, AState, AStateEx);

    PreferredHeight := GetMeasuresEx(ADest, TCDCONTROL_CAPTION_HEIGHT, AState, AStateEx);
  end;
  end;
end;

function TCDDrawerCommon.GetColor(AColorID: Integer): TColor;
begin
  case AColorId of
  TCDEDIT_BACKGROUND_COLOR:    Result := clWhite;
  TCDEDIT_TEXT_COLOR:          Result := clBlack;
  TCDEDIT_SELECTED_BACKGROUND_COLOR: Result := clBlue;
  TCDEDIT_SELECTED_TEXT_COLOR: Result := clWhite;
  TCDBUTTON_DEFAULT_COLOR:     Result := $00F1F5F5;
  else
    Result := clBlack;
  end;
end;

function TCDDrawerCommon.GetClientArea(ADest: TCanvas; ASize: TSize;
  AControlId: TCDControlID; AState: TCDControlState; AStateEx: TCDControlStateEx
  ): TRect;
begin
  Result := Bounds(0, 0, ASize.cx, ASize.cy);

  case AControlId of
  cidCTabControl:
  begin
    Result.Top := GetMeasuresEx(ADest, TCDCTABCONTROL_TAB_HEIGHT, AState, AStateEx) + 2;
    Result.Left := 2;
    Result.Right := Result.Right - 2;
    Result.Bottom := Result.Bottom - 2;
  end;
  end;
end;

procedure TCDDrawerCommon.DrawControl(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AControl: TCDControlID; AState: TCDControlState;
  AStateEx: TCDControlStateEx);
begin
  case AControl of
  cidButton:     DrawButton(ADest, ADestPos, ASize, AState, AStateEx);
  cidEdit:       DrawEdit(ADest, ADestPos, ASize, AState, TCDEditStateEx(AStateEx));
  cidCheckBox:   DrawCheckBox(ADest, ADestPos, ASize, AState, AStateEx);
  cidGroupBox:   DrawGroupBox(ADest, ADestPos, ASize, AState, AStateEx);
  cidTrackBar:   DrawTrackBar(ADest, ADestPos, ASize, AState, TCDTrackBarStateEx(AStateEx));
  cidCTabControl:DrawCTabControl(ADest, ADestPos, ASize, AState, TCDCTabControlStateEx(AStateEx));
  end;
end;

procedure TCDDrawerCommon.DrawButton(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
  ADest.Brush.Color := AStateEx.RGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.RoundRect(0, 0, ASize.cx, ASize.cy, 8, 8);

  ADest.Brush.Style := bsSolid;
  ADest.Brush.Color := AStateEx.RGBColor;
  ADest.Pen.Color := clWhite;
  ADest.Pen.Style := psSolid;
  ADest.Rectangle(0, 0, ASize.cx - 1, ASize.cy - 1);
  ADest.Pen.Color := clWhite;
  ADest.Line(0, 0, ASize.cx - 1, 0);
  ADest.Line(0, 0, 0, ASize.cy - 1);
  ADest.Pen.Color := clGray;
  ADest.Line(0, ASize.cy - 1, ASize.cx - 1, ASize.cy - 1);
  ADest.Line(ASize.cx - 1, ASize.cy - 1, ASize.cx - 1, -1);
  ADest.Pen.Color := $0099A8AC;
  ADest.Line(1, ASize.cy - 2, ASize.cx - 2, ASize.cy - 2);
  ADest.Line(ASize.cx - 2, ASize.cx - 2, ASize.cx - 2, 0);
  ADest.Pen.Color := $00E2EFF1;
  ADest.Line(1, 1, ASize.cx - 2, 1);
  ADest.Line(1, 1, 1, ASize.cy - 2);

  // Button image
  if csfSunken in AState then
  begin
    ADest.Brush.Style := bsSolid;
    ADest.Brush.Color := AStateEx.RGBColor;
    ADest.Pen.Color := clWhite;
    ADest.Pen.Style := psSolid;
    ADest.Rectangle(0, 0, ASize.cx - 1, ASize.cy - 1);
    ADest.Pen.Color := clGray;
    ADest.Line(0, 0, ASize.cx - 1, 0);
    ADest.Line(0, 0, 0, ASize.cy - 1);
    ADest.Pen.Color := clWhite;
    ADest.Line(0, ASize.cy - 1, ASize.cx - 1, ASize.cy - 1);
    ADest.Line(ASize.cx - 1, ASize.cy - 1, ASize.cx - 1, -1);
    ADest.Pen.Color := $00E2EFF1;
    ADest.Line(1, ASize.cy - 2, ASize.cx - 2, ASize.cy - 2);
    ADest.Line(ASize.cx - 2, ASize.cy - 2, ASize.cx - 2, 0);
    ADest.Pen.Color := $0099A8AC;
    ADest.Line(1, 1, ASize.cx - 2, 1);
    ADest.Line(1, 1, 1, ASize.cy - 2);
  end
  else if csfHasFocus in AState then
  begin
    ADest.Brush.Style := bsClear;
    ADest.Pen.Color := clWhite;
    ADest.Pen.Style := psSolid;
    ADest.Rectangle(3, 3, ASize.cx - 4, ASize.cy - 4);
    ADest.Pen.Color := clBlack;
    ADest.Pen.Style := psDot;
    ADest.Rectangle(3, 3, ASize.cx - 4, ASize.cy - 4);
  end;

  // Button text
  ADest.Font.Assign(AStateEx.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := AStateEx.Caption;
  if csfSunken in AState then
    ADest.TextOut((ASize.cx - ADest.TextWidth(Str)) div 2 + 1,
      (ASize.cy - ADest.TextHeight(Str)) div 2 + 1, Str)
  else
    ADest.TextOut((ASize.cx - ADest.TextWidth(Str)) div 2,
      (ASize.cy - ADest.TextHeight(Str)) div 2, Str);
end;

procedure TCDDrawerCommon.DrawEditBackground(ADest: TCanvas;
  ADestPos: TPoint; ASize: TSize; AState: TCDControlState;
  AStateEx: TCDEditStateEx);
begin
  // The background
  ADest.Brush.Color := clWhite;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Color := clBlack;
  ADest.Pen.Style := psSolid;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
end;

procedure TCDDrawerCommon.DrawEdit(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDEditStateEx);
var
  lVisibleText, lTmpText, lControlText: TCaption;
  lCaretPixelPos: Integer;
  lHeight: Integer;
  lSelLeftPos, lSelLeftPixelPos, lSelLength, lSelRightPos: Integer;
  lTextWidth: Integer;
  lControlTextLen: PtrInt;
begin
  DrawEditBackground(ADest, ADestPos, ASize, AState, AStateEx);

  lControlText := AStateEx.Caption;
  lControlTextLen := UTF8Length(AStateEx.Caption);
  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(AStateEx.Font);

  // The text without selection
  if AStateEx.SelLength = 0 then
  begin
    lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart, lControlTextLen);
    ADest.TextOut(4, 1, lVisibleText);
  end
  // Text and Selection
  else
  begin
    lSelLeftPos := AStateEx.SelStart;
    if AStateEx.SelLength < 0 then lSelLeftPos := lSelLeftPos + AStateEx.SelLength;
    lSelRightPos := AStateEx.SelStart;
    if AStateEx.SelLength > 0 then lSelRightPos := lSelRightPos + AStateEx.SelLength;
    lSelLength := AStateEx.SelLength;
    if lSelLength < 0 then lSelLength := lSelLength * -1;

    // Text left of the selection
    lVisibleText := UTF8Copy(lControlText, AStateEx.VisibleTextStart, lSelLeftPos-AStateEx.VisibleTextStart);
    ADest.TextOut(4, 1, lVisibleText);
    lSelLeftPixelPos := ADest.TextWidth(lVisibleText)+4;

    // The selection background
    lVisibleText := UTF8Copy(lControlText, lSelLeftPos, lSelLength);
    lTextWidth := ADest.TextWidth(lVisibleText);
    ADest.Brush.Color := clBlue;
    ADest.Brush.Style := bsSolid;
    ADest.Rectangle(lSelLeftPixelPos, 1, lSelLeftPixelPos+lTextWidth, lHeight-1);
    ADest.Brush.Style := bsClear;

    // The selection text
    ADest.Font.Color := clWhite;
    ADest.TextOut(lSelLeftPixelPos, 1, lVisibleText);
    lSelLeftPixelPos := lSelLeftPixelPos + lTextWidth;

    // Text right of the selection
    ADest.Brush.Color := clWhite;
    ADest.Font.Color := AStateEx.Font.Color;
    lVisibleText := UTF8Copy(lControlText, lSelLeftPos+lSelLength+1, lControlTextLen);
    ADest.TextOut(lSelLeftPixelPos, 1, lVisibleText);
  end;

  // And the caret
  if AStateEx.CaretIsVisible then
  begin
    lTmpText := UTF8Copy(lControlText, 1, AStateEx.CaretPos-AStateEx.VisibleTextStart+1);
    lCaretPixelPos := ADest.TextWidth(lTmpText) + 3;
    lHeight := ASize.cy;
    ADest.Line(lCaretPixelPos, 2, lCaretPixelPos, lHeight-2);
    ADest.Line(lCaretPixelPos+1, 2, lCaretPixelPos+1, lHeight-2);
  end;
end;

procedure TCDDrawerCommon.DrawCheckBox(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
const
  CDCheckBoxCommon_Half_Height = 7;
  CDCheckBoxCommon_Height = 15;
var
  lHalf: Integer;
  lColor: TColor;
  i: Integer;
begin
  lHalf := ASize.cy div 2;

  // Background
  lColor := AStateEx.ParentRGBColor;
  ADest.Brush.Color := lColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.FillRect(0, 0, ASize.cx, ASize.cy);

  // The checkbox item itself
  ADest.Brush.Color := clWhite;
  ADest.Pen.Style := psSolid;
  if csfSunken in AState then ADest.Pen.Color := clGray
  else ADest.Pen.Color := clBlack;
  ADest.Rectangle(
    1,
    lHalf - CDCheckBoxCommon_Half_Height,
    CDCheckBoxCommon_Height+1,
    lHalf + CDCheckBoxCommon_Half_Height);

  // The Tickmark
  if csfOn in AState then
  begin
    // 4 lines going down and to the right
    for i := 0 to 3 do
      ADest.Line(5+i, lHalf - CDCheckBoxCommon_Half_Height+5+i, 5+i, lHalf - CDCheckBoxCommon_Half_Height+8+i);
    // Now 5 lines going up and to the right
    for i := 4 to 8 do
      ADest.Line(5+i, lHalf - CDCheckBoxCommon_Half_Height+5+6-i, 5+i, lHalf - CDCheckBoxCommon_Half_Height+8+6-i);
  end;

  // The selection
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := RGBToColor($31, $C6, $D6);
  ADest.Pen.Style := psSolid;
  if csfHasFocus in AState then
  begin
    // The selection inside the square
    ADest.Rectangle(
      2,
      lHalf - CDCheckBoxCommon_Half_Height+1,
      CDCheckBoxCommon_Height,
      lHalf + CDCheckBoxCommon_Half_Height-1);

    // Selection around the text
    ADest.Rectangle(
      CDCheckBoxCommon_Height+4, 0,
      ASize.cx, ASize.cy);
  end;

  // Now the text
  ADest.Font.Assign(AStateEx.Font);
  ADest.TextOut(CDCheckBoxCommon_Height+5, 0, AStateEx.Caption);
end;

procedure TCDDrawerCommon.DrawGroupBox(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDControlStateEx);
var
  FCaptionMiddle: integer;
begin
  FCaptionMiddle := ADest.TextHeight('Ź') div 2;
  if FCaptionMiddle = 0 then FCaptionMiddle := AStateEx.Font.Size div 2;
  if FCaptionMiddle = 0 then FCaptionMiddle := 5;

  // Background
  ADest.Brush.Color := AStateEx.ParentRGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);

  // frame
  ADest.Pen.Color := clBlack;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Rectangle(0, FCaptionMiddle, ASize.cx - 1, ASize.cy - 1);

  // paint text
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsSolid; // This will fill the text background
  ADest.Font.Size := 10;
  ADest.TextOut(FCaptionMiddle, 0, AStateEx.Caption);
end;

procedure TCDDrawerCommon.DrawTrackBar(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDTrackBarStateEx);
begin

end;

procedure TCDDrawerCommon.DrawCTabControl(ADest: TCanvas; ADestPos: TPoint;
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

  // frame
  if AStateEx.TabCount = 0 then CaptionHeight := 0
  else CaptionHeight := GetMeasuresEx(ADest, TCDCTABCONTROL_TAB_HEIGHT, AState, AStateEx);

  // white lines in the left and top
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := clWhite;
  ADest.Line(0, CaptionHeight, 0, ASize.cy-1);
  ADest.Pixels[1,1] := clWhite;
  ADest.Line(2, CaptionHeight, ASize.cx-1, CaptionHeight);
  // Grey line on the inside left and top
  ADest.Pen.Color := ColorToRGB($00E2EFF1);
  ADest.Line(1, CaptionHeight, 1, ASize.cy-2);
  ADest.Line(2, CaptionHeight+1, ASize.cx-2, CaptionHeight+1);
  // Dark grey line on the right and bottom
  ADest.Pen.Color := ColorToRGB($00646F71);
  ADest.Line(0, ASize.cy, ASize.cx, ASize.cy);
  ADest.Line(ASize.cx, ASize.cy, ASize.cx, CaptionHeight);
  // Grey line on the inside right and bottom
  ADest.Pen.Color := ColorToRGB($00646F71);
  ADest.Line(1, ASize.cy-1, ASize.cx-1, ASize.cy-1);
  ADest.Line(ASize.cx-1, ASize.cy-1, ASize.cx-1, CaptionHeight);

  // Tabs
  ADest.Font.Assign(AStateEx.Font);
  DrawTabs(ADest, ADestPos, ASize, AState, AStateEx);
end;

procedure TCDDrawerCommon.DrawTabSheet(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
begin
  ADest.Brush.Color := AStateEx.RGBColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psSolid;
  ADest.Pen.Color := AStateEx.RGBColor;
  ADest.Rectangle(0, 0, ASize.cx, ASize.cy);
end;

procedure TCDDrawerCommon.DrawTabs(ADest: TCanvas; ADestPos: TPoint;
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

procedure TCDDrawerCommon.DrawTab(ADest: TCanvas; ADestPos: TPoint;
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
  Points[1] := Point(AStateEx.CurStartLeftPos+lTabWidth-2, lTabTopPos);
  Points[2] := Point(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+2);
  Points[3] := Point(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);
  Points[4] := Point(AStateEx.CurStartLeftPos, lTabTopPos+lTabHeight);
  ADest.Polygon(Points);

  // Draw the outer border only in the top and right sides,
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := ColorToRGB($009C9B91);
  ADest.MoveTo(AStateEx.CurStartLeftPos, lTabTopPos);
  ADest.LineTo(AStateEx.CurStartLeftPos+lTabWidth-2, lTabTopPos);
  ADest.LineTo(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+2);
  ADest.LineTo(AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);

  if IsSelected then
  begin
    // If it is selected, add a selection frame
    ADest.Pen.Color := clWhite;
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsClear;
    ADest.Rectangle(
      AStateEx.CurStartLeftPos+3, lTabTopPos+3,
      AStateEx.CurStartLeftPos+lTabWidth-5, lTabTopPos+lTabHeight-3);
    ADest.Pen.Color := clBlack;
    ADest.Pen.Style := psDot;
    ADest.Rectangle(
      AStateEx.CurStartLeftPos+3, lTabTopPos+3,
      AStateEx.CurStartLeftPos+lTabWidth-5, lTabTopPos+lTabHeight-3);

    // and Clear the bottom area if selected
    ADest.Pen.Color := AStateEx.RGBColor;
    ADest.Line(AStateEx.CurStartLeftPos, lTabTopPos+lTabHeight,
      AStateEx.CurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);
    ADest.Line(AStateEx.CurStartLeftPos, lTabTopPos+lTabHeight-1,
      AStateEx.CurStartLeftPos+lTabWidth-2, lTabTopPos+lTabHeight-1);
  end;

  // Now the text
  lCaption := AStateEx.Tabs.Strings[AStateEx.CurTabIndex];
  ADest.TextOut(AStateEx.CurStartLeftPos+5, lTabTopPos+5, lCaption);
end;

{ TCDListViewDrawerCommon }

initialization
  RegisterDrawer(TCDDrawerCommon.Create, dsCommon);
end.

