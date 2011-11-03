unit customdrawn_common;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils, Types,
  // fpimage
  fpcanvas, fpimgcanv, fpimage,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, //IntfGraphics,
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
    function GetColor(AColorID: Integer): TColor; override;
    procedure DrawControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AControl: TCDControlID; AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // ===================================
    // Standard Tab
    // ===================================
    // TCDButton
    procedure DrawButton(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDEdit
    procedure CreateEditBackgroundBitmap(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    procedure DrawEdit(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDEditStateEx); override;
    // TCDCheckBox
    procedure CalculateCheckBoxPreferredSize(ADest: TCanvas;
      AState: TCDControlState; AStateEx: TCDControlStateEx;
      var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure DrawCheckBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // TCDGroupBox
    procedure DrawGroupBox(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDControlStateEx); override;
    // ===================================
    // Common Controls Tab
    // ===================================
    // TCDCustomTabControl
    procedure DrawCTabControl(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
    procedure DrawTabSheet(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
    procedure DrawTabs(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
    procedure DrawTab(ADest: TCanvas; ADestPos: TPoint; ASize: TSize;
      AState: TCDControlState; AStateEx: TCDCTabControlStateEx); override;
    function  GetPageIndexFromXY(x, y: integer): integer; override;
  end;

  { TCDCustomTabControlDrawerCommon }

{  TCDCustomTabControlDrawerCommon = class(TCDCustomTabControlDrawer)
  private
    StartIndex: integer;       //FEndIndex
    LeftmostTabVisibleIndex: Integer;
    procedure DrawTabs(ADest: TCanvas; CDTabControl: TCDCustomTabControl);
    procedure DrawTab(ADest: TCanvas; AIndex: Integer; ACurStartLeftPos: Integer;
      CDTabControl: TCDCustomTabControl);
  public
    function GetPageIndexFromXY(x, y: integer): integer; override;
    function GetTabHeight(AIndex: Integer; CDTabControl: TCDCustomTabControl): Integer; override;
    function GetTabWidth(ADest: TCanvas; AIndex: Integer; CDTabControl: TCDCustomTabControl): Integer; override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTabControl: TCDCustomTabControl); override;
    procedure DrawToCanvas(ADest: TCanvas; CDTabControl: TCDCustomTabControl); override;
    procedure DrawTabSheet(ADest: TCanvas; CDTabControl: TCDCustomTabControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer; CDTabControl: TCDCustomTabControl); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer; CDTabControl: TCDCustomTabControl); override;
  end;}

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
  case AMeasureID of
  TCDCONTROL_CAPTION_WIDTH:  Result := ADest.TextWidth(AStateEx.Caption);
  TCDCONTROL_CAPTION_HEIGHT: Result := ADest.TextHeight('ŹÇ')+3;
  TCDCTABCONTROL_TAB_HEIGHT:
  begin
    if AStateEx.Font.Size = 0 then Result := 32
    else Result := AStateEx.Font.Size + 22;
  end;
  TCDCTABCONTROL_TAB_WIDTH:
  begin
    lCaption := ATabsStateEx.Tabs.Strings[ATabsStateEx.TabIndex];
    Result := ADest.TextWidth(lCaption) + TCDTabControl_Common_TabCaptionExtraWidth;
  end
  else
    Result := 0;
  end;
end;

function TCDDrawerCommon.GetColor(AColorID: Integer): TColor;
begin
  case AColorId of
  TCDEDIT_BACKGROUND_COLOR:    Result := clWhite;
  TCDEDIT_TEXT_COLOR:          Result := clBlack;
  TCDEDIT_SELECTED_BACKGROUND_COLOR: Result := clBlue;
  TCDEDIT_SELECTED_TEXT_COLOR: Result := clWhite;
  else
    Result := clBlack;
  end;
end;

procedure TCDDrawerCommon.DrawControl(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AControl: TCDControlID; AState: TCDControlState;
  AStateEx: TCDControlStateEx);
begin
  case AControl of
  cidButton: DrawButton(ADest, ADestPos, ASize, AState, AStateEx);
  end;
end;

procedure TCDDrawerCommon.DrawButton(ADest: TCanvas; ADestPos: TPoint;
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

procedure TCDDrawerCommon.CreateEditBackgroundBitmap(ADest: TCanvas;
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
begin

end;

procedure TCDDrawerCommon.CalculateCheckBoxPreferredSize(ADest: TCanvas;
  AState: TCDControlState; AStateEx: TCDControlStateEx; var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;

  if AStateEx.AutoSize then
    PreferredWidth := 21 + GetMeasuresEx(ADest, TCDCONTROL_CAPTION_WIDTH, AState, AStateEx);

  PreferredHeight := GetMeasuresEx(ADest, TCDCONTROL_CAPTION_HEIGHT, AState, AStateEx);
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
  ADest.Pen.FPColor := colBlack;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Rectangle(0, FCaptionMiddle, ASize.cx - 1, ASize.cy - 1);

  // paint text
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsSolid; // This will fill the text background
  ADest.Font.Size := 10;
  ADest.TextOut(FCaptionMiddle, 0, AStateEx.Caption);
end;

procedure TCDDrawerCommon.DrawCTabControl(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
begin

end;

procedure TCDDrawerCommon.DrawTabSheet(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
begin

end;

procedure TCDDrawerCommon.DrawTabs(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
begin

end;

procedure TCDDrawerCommon.DrawTab(ADest: TCanvas; ADestPos: TPoint;
  ASize: TSize; AState: TCDControlState; AStateEx: TCDCTabControlStateEx);
begin

end;

function TCDDrawerCommon.GetPageIndexFromXY(x, y: integer): integer;
begin

end;

{ TCDListViewDrawerCommon }

(*procedure TCDListViewDrawerCommon.DrawToIntfImage(ADest: TFPImageCanvas;
  CDListView: TCDListView);
begin

end;

procedure TCDListViewDrawerCommon.DrawToCanvas(ADest: TCanvas;
  CDListView: TCDListView);
begin

end;

procedure TCDEditDrawerCommon.DrawToCanvas(ADest: TCanvas; CDControl: TCDControl);
var
  CDEdit: TCDEdit absolute CDControl;
  lVisibleText, lTmpText, lControlText: TCaption;
  lCaretPixelPos: Integer;
  lHeight: Integer;
  lSelLeftPos, lSelLeftPixelPos, lSelLength, lSelRightPos: Integer;
  lTextWidth: Integer;
begin
  DrawBackground(ADest, CDControl);

  lControlText := CDEdit.Text;
  ADest.Brush.Style := bsClear;
  ADest.Font.Assign(CDControl.Font);

  // The text without selection
  if CDEdit.FSelLength = 0 then
  begin
    lVisibleText := Copy(lControlText, CDEdit.FVisibleTextStart, Length(lControlText));
    ADest.TextOut(4, 1, lVisibleText);
  end
  // Text and Selection
  else
  begin
    lSelLeftPos := CDEdit.FSelStart;
    if CDEdit.FSelLength < 0 then lSelLeftPos := lSelLeftPos + CDEdit.FSelLength;
    lSelRightPos := CDEdit.FSelStart;
    if CDEdit.FSelLength > 0 then lSelRightPos := lSelRightPos + CDEdit.FSelLength;
    lSelLength := CDEdit.FSelLength;
    if lSelLength < 0 then lSelLength := lSelLength * -1;

    // Text left of the selection
    lVisibleText := Copy(lControlText, CDEdit.FVisibleTextStart, lSelLeftPos-CDEdit.FVisibleTextStart);
    ADest.TextOut(4, 1, lVisibleText);
    lSelLeftPixelPos := ADest.TextWidth(lVisibleText)+4;

    // The selection background
    lVisibleText := Copy(lControlText, lSelLeftPos, lSelLength);
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
    ADest.Font.Color := CDEdit.Font.Color;
    lVisibleText := Copy(lControlText, lSelLeftPos+lSelLength+1, Length(lControlText));
    ADest.TextOut(lSelLeftPixelPos, 1, lVisibleText);
  end;

  // And the caret
  if CDEdit.FCaretIsVisible then
  begin
    lTmpText := Copy(lControlText, 1, CDEdit.FCaretPos-CDEdit.FVisibleTextStart+1);
    lCaretPixelPos := ADest.TextWidth(lTmpText) + 3;
    lHeight := CDControl.Height;
    ADest.Line(lCaretPixelPos, 2, lCaretPixelPos, lHeight-2);
    ADest.Line(lCaretPixelPos+1, 2, lCaretPixelPos+1, lHeight-2);
  end;
end;

{ TCDCustomTabControlDrawerCommon }

procedure TCDCustomTabControlDrawerCommon.DrawTabs(ADest: TCanvas; CDTabControl: TCDCustomTabControl);
var
  IsPainting: Boolean = False;
  CurStartLeftPos: Integer = 0;
  i: Integer;
begin
  for i := 0 to CDTabControl.Tabs.Count - 1 do
  begin
    if i = LeftmostTabVisibleIndex then
      IsPainting := True;

    if IsPainting then
    begin
      DrawTab(ADest, i, CurStartLeftPos, CDTabControl);
      CurStartLeftPos := CurStartLeftPos + GetTabWidth(ADest, i, CDTabControl);
    end;
  end;
end;

procedure TCDCustomTabControlDrawerCommon.DrawTab(ADest: TCanvas;
  AIndex: Integer; ACurStartLeftPos: Integer; CDTabControl: TCDCustomTabControl);
var
  IsSelected: Boolean;
  lTabWidth, lTabHeight, lTabTopPos: Integer;
  Points: array of TPoint;
  lCaption: String;
begin
  IsSelected := CDTabControl.TabIndex = AIndex;

  if IsSelected then
  begin
    lTabTopPos := 0;
    lTabHeight := GetTabHeight(AIndex, CDTabControl);
  end
  else
  begin
    lTabTopPos := 5;
    lTabHeight := GetTabHeight(AIndex, CDTabControl)-5;
  end;

  lTabWidth := GetTabWidth(ADest, AIndex, CDTabControl);

  // Fill the area inside the outer border
  ADest.Pen.Style := psClear;
  ADest.Brush.Style := bsSolid;
  ADest.Brush.Color := clWhite;
  SetLength(Points, 5);
  Points[0] := Point(ACurStartLeftPos, lTabTopPos);
  Points[1] := Point(ACurStartLeftPos+lTabWidth-5, lTabTopPos);
  Points[2] := Point(ACurStartLeftPos+lTabWidth, lTabTopPos+5);
  Points[3] := Point(ACurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);
  Points[4] := Point(ACurStartLeftPos, lTabTopPos+lTabHeight);
  ADest.Polygon(Points);

  // Draw the outer border only in the top and right sides,
  // and bottom if unselected
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := ColorToRGB($009C9B91);
  ADest.MoveTo(ACurStartLeftPos+1, lTabTopPos);
  ADest.LineTo(ACurStartLeftPos+lTabWidth-5, lTabTopPos);
  ADest.LineTo(ACurStartLeftPos+lTabWidth, lTabTopPos+5);
  ADest.LineTo(ACurStartLeftPos+lTabWidth, lTabTopPos+lTabHeight);

  // If it is selected, add a selection frame
  if IsSelected then
  begin
    ADest.Pen.Color := ColorToRGB($00D6C731);
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsClear;
    ADest.Rectangle(
      ACurStartLeftPos+3, lTabTopPos+3,
      ACurStartLeftPos+lTabWidth-5, lTabTopPos+lTabHeight-5
      );
  end;

  // Now the text
  lCaption := CDTabControl.Tabs.Strings[AIndex];
  ADest.TextOut(ACurStartLeftPos+5, lTabTopPos+5, lCaption);
end;

function TCDCustomTabControlDrawerCommon.GetPageIndexFromXY(x, y: integer
  ): integer;
begin
  Result := 1;
end;

{function TCDCustomTabControlDrawerCommon.GetClientRect(AControl: TCDControl
  ): TRect;
var
  lCaptionHeight: Integer;
begin
  lCaptionHeight := GetTabHeight(CDTabControl.FTabIndex) - 4;

  Result := Rect(5, lCaptionHeight + 1, CDTabControl.Width - 10,
    CDTabControl.Height - lCaptionHeight - 5);
end;}

procedure TCDCustomTabControlDrawerCommon.DrawToIntfImage(ADest: TFPImageCanvas;
  FPImg: TLazIntfImage; CDTabControl: TCDCustomTabControl);
var
  lColor: TColor;
  lFPColor: TFPColor;
  x, y: Integer;
begin
  lColor := CDTabControl.GetRGBBackgroundColor();

  // Background
  lFPColor := TColorToFPColor(lColor);
  FPImg.FillPixels(lFPColor);
end;

procedure TCDCustomTabControlDrawerCommon.DrawToCanvas(ADest: TCanvas; CDTabControl: TCDCustomTabControl);
var
  CaptionHeight: Integer;
begin
  CaptionHeight := GetTabHeight(CDTabControl.TabIndex, CDTabControl);

  // frame
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Pen.Color := ColorToRGB($009C9B91);

  if CDTabControl.GetTabCount = 0 then
    ADest.Rectangle(0, 0, CDTabControl.Width - 2, CDTabControl.Height - 2)
  else
    ADest.Rectangle(0, CaptionHeight, CDTabControl.Width -  2, CDTabControl.Height - 2);

  ADest.Pen.Color := ColorToRGB($00BFCED0);
  ADest.Line(CDTabControl.Width - 1, CaptionHeight + 1,
    CDTabControl.Width - 1, CDTabControl.Height - 1);
  ADest.Line(CDTabControl.Width - 1, CDTabControl.Height - 1, 1,
    CDTabControl.Height - 1);

  // Tabs
  ADest.Font.Name := CDTabControl.Font.Name;
  ADest.Font.Size := CDTabControl.Font.Size;
//  DrawCaptionBar(ADest, Rect(0, 0, CDPageControl.Width -
//    2, CaptionHeight + 1), CDPageControl.Color, CDPageControl);
  DrawTabs(ADest, CDTabControl);
end;

procedure TCDCustomTabControlDrawerCommon.DrawTabSheet(ADest: TCanvas; CDTabControl: TCDCustomTabControl);
begin
  ADest.Brush.Color := CDTabControl.Color;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDTabControl.Width, CDTabControl.Height);
end;*)

initialization
  RegisterDrawer(TCDDrawerCommon.Create, dsCommon);
end.

