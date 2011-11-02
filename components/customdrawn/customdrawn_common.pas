unit customdrawn_common;

{$mode objfpc}{$H+}

interface

uses
  // RTL
  Classes, SysUtils,
  // fpimage
  fpcanvas, fpimgcanv, fpimage,
  // LCL -> Use only TForm, TWinControl, TCanvas and TLazIntfImage
  Graphics, Controls, LCLType, LCLIntf, IntfGraphics,
  // Others only for types
  StdCtrls,
  //
  customdrawncontrols, customdrawnutils;

type
  TCDButtonDrawerCommon = class(TCDButtonDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDButton: TCDButton); override;
    procedure DrawToCanvas(ADest: TCanvas; CDButton: TCDButton); override;
  end;

  { TCDEditDrawerCommon }

  TCDEditDrawerCommon = class(TCDEditDrawer)
  public
    function GetMeasures(AMeasureID: Integer): Integer; override;
    function GetColor(AColorID: Integer): TColor; override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDControl: TCDControl); override;
    procedure DrawBackground(ADest: TCanvas; AControl: TCDControl); override;
    procedure DrawToCanvas(ADest: TCanvas; CDControl: TCDControl); override;
  end;

  { TCDCheckBoxDrawerCommon }

  TCDCheckBoxDrawerCommon = class(TCDCheckBoxDrawer)
  public
    function GetCaptionWidth(CDCheckBox: TCDCheckBox): Integer;
    function GetCaptionHeight(CDCheckBox: TCDCheckBox): Integer;
    procedure CalculatePreferredSize(CDCheckBox: TCDCheckBox; var PreferredWidth,
      PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDCheckBox: TCDCheckBox); override;
    procedure DrawToCanvas(ADest: TCanvas; CDCheckBox: TCDCheckBox); override;
  end;

  TCDGroupBoxDrawerCommon = class(TCDGroupBoxDrawer)
  public
    FCaptionMiddle: integer;
    procedure SetClientRectPos(CDGroupBox: TCDGroupBox); override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDGroupBox: TCDGroupBox); override;
    procedure DrawToCanvas(ADest: TCanvas; CDGroupBox: TCDGroupBox); override;
  end;

  // ===================================
  // Common Controls Tab
  // ===================================

  { TCDListViewDrawerCommon }

  TCDListViewDrawerCommon = class(TCDListViewDrawer)
  public
    procedure DrawToIntfImage(ADest: TFPImageCanvas; CDListView: TCDListView); override;
    procedure DrawToCanvas(ADest: TCanvas; CDListView: TCDListView); override;
  end;

  { TCDCustomTabControlDrawerCommon }

  TCDCustomTabControlDrawerCommon = class(TCDCustomTabControlDrawer)
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
    //function GetClientRect(AControl: TCDControl): TRect; override;
    procedure DrawToIntfImage(ADest: TFPImageCanvas; FPImg: TLazIntfImage;
      CDTabControl: TCDCustomTabControl); override;
    procedure DrawToCanvas(ADest: TCanvas; CDTabControl: TCDCustomTabControl); override;
    procedure DrawTabSheet(ADest: TCanvas; CDTabControl: TCDCustomTabControl); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer; CDTabControl: TCDCustomTabControl); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer; CDTabControl: TCDCustomTabControl); override;
  end;

implementation

{ TCDListViewDrawerCommon }

procedure TCDListViewDrawerCommon.DrawToIntfImage(ADest: TFPImageCanvas;
  CDListView: TCDListView);
begin

end;

procedure TCDListViewDrawerCommon.DrawToCanvas(ADest: TCanvas;
  CDListView: TCDListView);
begin

end;

{ TCDEditDrawerCommon }

function TCDEditDrawerCommon.GetMeasures(AMeasureID: Integer): Integer;
begin
  case AMeasureID of
  TCDEDIT_LEFT_TEXT_SPACING: Result := 4;
  TCDEDIT_RIGHT_TEXT_SPACING: Result := 3;
  else
    Result := inherited GetMeasures(AMeasureID);
  end;
end;

function TCDEditDrawerCommon.GetColor(AColorID: Integer): TColor;
begin
  case AColorId of
  TCDEDIT_BACKGROUND_COLOR:    Result := clWhite;
  TCDEDIT_TEXT_COLOR:          Result := clBlack;
  TCDEDIT_SELECTED_BACKGROUND_COLOR: Result := clBlue;
  TCDEDIT_SELECTED_TEXT_COLOR: Result := clWhite;
  else
    Result := inherited GetColor(AColorId);
  end;
end;

procedure TCDEditDrawerCommon.DrawToIntfImage(ADest: TFPImageCanvas;
  CDControl: TCDControl);
begin

end;

procedure TCDEditDrawerCommon.DrawBackground(ADest: TCanvas;
  AControl: TCDControl);
begin
  // The background
  ADest.Brush.Color := clWhite;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Color := clBlack;
  ADest.Pen.Style := psSolid;
  ADest.Rectangle(0, 0, AControl.Width, AControl.Height);
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

{ TCDCheckBoxDrawerCommon }

function TCDCheckBoxDrawerCommon.GetCaptionWidth(CDCheckBox: TCDCheckBox
  ): Integer;
begin
  CDCheckBox.Canvas.Font.Assign(CDCheckBox.Font);
  Result := CDCheckBox.Canvas.TextWidth(CDCheckBox.Caption);
end;

function TCDCheckBoxDrawerCommon.GetCaptionHeight(CDCheckBox: TCDCheckBox
  ): Integer;
begin
  CDCheckBox.Canvas.Font.Assign(CDCheckBox.Font);
  Result := CDCheckBox.Canvas.TextHeight('ŹÇ')+3;
end;

procedure TCDCheckBoxDrawerCommon.CalculatePreferredSize(
  CDCheckBox: TCDCheckBox; var PreferredWidth, PreferredHeight: integer;
  WithThemeSpace: Boolean);
begin
  PreferredWidth := 0;

  if CDCheckBox.AutoSize then
    PreferredWidth := 21 + GetCaptionWidth(CDCheckBox);

  PreferredHeight := GetCaptionHeight(CDCheckBox);
end;

procedure TCDCheckBoxDrawerCommon.DrawToIntfImage(ADest: TFPImageCanvas;
  CDCheckBox: TCDCheckBox);
begin

end;

procedure TCDCheckBoxDrawerCommon.DrawToCanvas(ADest: TCanvas;
  CDCheckBox: TCDCheckBox);
const
  CDCheckBoxCommon_Half_Height = 7;
  CDCheckBoxCommon_Height = 15;
var
  lHalf: Integer;
  lColor: TColor;
  i: Integer;
begin
  lHalf := CDCheckBox.Height div 2;

  // Background
  lColor := CDCheckBox.GetRGBBackgroundColor();
  ADest.Brush.Color := lColor;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.FillRect(0, 0, CDCheckBox.Width, CDCheckBox.Height);

  // The checkbox item itself
  ADest.Brush.Color := clWhite;
  ADest.Pen.Style := psSolid;
  if CDCheckBox.IsDown then ADest.Pen.Color := clGray
  else ADest.Pen.Color := clBlack;
  ADest.Rectangle(
    1,
    lHalf - CDCheckBoxCommon_Half_Height,
    CDCheckBoxCommon_Height+1,
    lHalf + CDCheckBoxCommon_Half_Height);

  // The Tickmark
  if CDCheckBox.State = cbChecked then
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
  if CDCheckBox.Focused then
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
      CDCheckBox.Width, CDCheckBox.Height);
  end;

  // Now the text
  ADest.Font.Assign(CDCheckBox.Font);
  ADest.TextOut(CDCheckBoxCommon_Height+5, 0, CDCheckBox.Caption);
end;

{ TCDButtonDrawerCommon }

procedure TCDButtonDrawerCommon.DrawToIntfImage(ADest: TFPImageCanvas;
  CDButton: TCDButton);
begin

end;

procedure TCDButtonDrawerCommon.DrawToCanvas(ADest: TCanvas; CDButton: TCDButton);
var
  TmpB: TBitmap;
  Str: string;
begin
  // Button shape -> This crashes in Gtk2
  TmpB := TBitmap.Create;
  TmpB.Width := CDButton.Width;
  TmpB.Height := CDButton.Height;
  TmpB.Canvas.Brush.Color := CDButton.Color;
  TmpB.Canvas.Brush.Style := bsSolid;
  TmpB.Canvas.RoundRect(0, 0, TmpB.Width, TmpB.Height, 8, 8);
  //  CDButton.SetShape(TmpB);

  // Button image
  if CDButton.IsDown then
  begin
    TmpB.Canvas.Brush.Style := bsSolid;
    TmpB.Canvas.Brush.Color := GetAColor(CDButton.Color, 90);
    TmpB.Canvas.Pen.Color := clBlack;
    TmpB.Canvas.Pen.Style := psSolid;
    TmpB.Canvas.Rectangle(0, 0, TmpB.Canvas.Width, TmpB.Canvas.Height);
  end
  else if CDButton.Focused then
  begin
    with TmpB.Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := GetAColor(CDButton.Color, 99);
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
        Brush.Color := CDButton.Color;
        Pen.Color := clBlack;
        Pen.Style := psSolid;
        Rectangle(0, 0, Width, Height);
      end;
  end;

  ADest.Draw(0, 0, TmpB);

  TmpB.Free;

  // Button text
  {$ifndef CUSTOMDRAWN_USE_FREETYPE}
  ADest.Font.Assign(CDButton.Font);
  ADest.Brush.Style := bsClear;
  ADest.Pen.Style := psSolid;
  Str := CDButton.Caption;
  ADest.TextOut((CDButton.Width - ADest.TextWidth(Str)) div 2,
    (CDButton.Height - ADest.TextHeight(Str)) div 2, Str);
  {$endif}
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

function TCDCustomTabControlDrawerCommon.GetTabHeight(AIndex: Integer; CDTabControl: TCDCustomTabControl): Integer;
begin
  if CDTabControl.Font.Size = 0 then
    Result := 32
  else
    Result := CDTabControl.Font.Size + 22;
end;

function TCDCustomTabControlDrawerCommon.GetTabWidth(ADest: TCanvas;
  AIndex: Integer; CDTabControl: TCDCustomTabControl): Integer;
const
  TCDTabControl_Common_TabCaptionExtraWidth = 20;
var
  lCaption: string;
begin
  lCaption := CDTabControl.Tabs.Strings[AIndex];

  Result := ADest.TextWidth(lCaption) + TCDTabControl_Common_TabCaptionExtraWidth;
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
end;

procedure TCDCustomTabControlDrawerCommon.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer; CDTabControl: TCDCustomTabControl);
var
  i: Integer;
  CurPage: TCDTabSheet;
  CurStartLeftPos: Integer = 0;
  VisiblePagesStarted: Boolean = False;
  lTabWidth: Integer;
begin
  for i := 0 to CDTabControl.Tabs.Count - 1 do
  begin
    if i = LeftmostTabVisibleIndex then
      VisiblePagesStarted := True;

    if VisiblePagesStarted then
    begin
      lTabWidth := GetTabWidth(CDTabControl.Canvas, i, CDTabControl);
      if (X > CurStartLeftPos) and
        (X < CurStartLeftPos + lTabWidth) and
        (Y < GetTabHeight(i, CDTabControl)) then
      begin
        if CDTabControl is TCDPageControl then
          (CDTabControl as TCDPageControl).PageIndex := i
        else
          CDTabControl.TabIndex := i;

        Exit;
      end;
      CurStartLeftPos := CurStartLeftPos + lTabWidth;
    end;
  end;
end;

procedure TCDCustomTabControlDrawerCommon.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer; CDTabControl: TCDCustomTabControl);
begin

end;

procedure TCDGroupBoxDrawerCommon.SetClientRectPos(CDGroupBox: TCDGroupBox);
var
  lRect: TRect;
  lCaptionHeight: integer;
begin
  lCaptionHeight := 10;
  lRect := Rect(1, lCaptionHeight, CDGroupBox.Width - 1, CDGroupBox.Height - 1);
  //CDGroupBox.AdjustClientRect(lRect);
end;

procedure TCDGroupBoxDrawerCommon.DrawToIntfImage(ADest: TFPImageCanvas;
  CDGroupBox: TCDGroupBox);
{$ifdef CUSTOMDRAWN_USE_FREETYPE}
var
  AFont: TFreeTypeFont = nil;
{$endif}
begin
  FCaptionMiddle := CDGroupBox.Canvas.TextHeight('Ź') div 2;
  if FCaptionMiddle = 0 then FCaptionMiddle := CDGroupBox.Canvas.Font.Size div 2;
  if FCaptionMiddle = 0 then FCaptionMiddle := 5;

  // Background
  if CDGroupBox.Parent = nil then
    ADest.Brush.FPColor := colLtGray
  else if CDGroupBox.Parent.Color = clDefault then
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(clForm))
  else
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CDGroupBox.Parent.Color));
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDGroupBox.Width, CDGroupBox.Height);

  // frame
  ADest.Pen.FPColor := colBlack;
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsClear;
  ADest.Rectangle(0, FCaptionMiddle, CDGroupBox.Width - 1, CDGroupBox.Height - 1);

  {$ifdef CUSTOMDRAWN_USE_FREETYPE}
  // Caption background and caption

  // initialize free type font manager
  opcftfont.InitEngine;
  //  FontMgr.SearchPath:='/usr/share/fonts/truetype/';
  AFont := TFreeTypeFont.Create;
  try
    // Text background
    ADest.Pen.Style := psClear;
    ADest.Brush.Style := bsSolid;
    // The brush color was already set previously and is already correct
    //    ADest.Rectangle(5, 0, AFont.GetTextWidth(CDGroupBox.Caption) + 5, 10);

    // paint text
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsClear;
    ADest.Font := AFont;
    ADest.Font.Name := 'Arial';
    ADest.Font.Size := 10;
    ADest.TextOut(5, 10, CDGroupBox.Caption);
  finally
    AFont.Free;
  end;
  {$endif}
end;

procedure TCDGroupBoxDrawerCommon.DrawToCanvas(ADest: TCanvas; CDGroupBox: TCDGroupBox);
begin
  if CDGroupBox.Parent = nil then
    ADest.Brush.Color := clLtGray
  else if CDGroupBox.Parent.Color = clDefault then
    ADest.Brush.Color := ColorToRGB(clForm)
  else
    ADest.Brush.Color := ColorToRGB(CDGroupBox.Parent.Color);

  // paint text
  ADest.Pen.Style := psSolid;
  ADest.Brush.Style := bsSolid; // This will fill the text background
  ADest.Font.Size := 10;
  ADest.TextOut(FCaptionMiddle, 0, CDGroupBox.Caption);
end;

initialization
  RegisterButtonDrawer(TCDButtonDrawerCommon.Create, dsCommon);
  RegisterEditDrawer(TCDEditDrawerCommon.Create, dsCommon);
  RegisterGroupBoxDrawer(TCDGroupBoxDrawerCommon.Create, dsCommon);
  RegisterCheckBoxDrawer(TCDCheckBoxDrawerCommon.Create, dsCommon);
  RegisterCustomTabControlDrawer(TCDCustomTabControlDrawerCommon.Create, dsCommon);
end.

