unit customdrawn_kde; 

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

{ TCDCustomTabControlDrawerKDE }

  TCDCustomTabControlDrawerKDE = class(TCDCustomTabControlDrawer)
  private
    StartIndex: integer;       //FEndIndex
    LeftmostTabVisibleIndex: Integer;
    procedure DrawCaptionBar(ADest: TCanvas; lRect: TRect; CL: TColor);
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

{ TCDCustomTabControlDrawerKDE }

procedure TCDCustomTabControlDrawerKDE.DrawCaptionBar(ADest: TCanvas;
  lRect: TRect; CL: TColor);
begin
  {  CaptionHeight := GetTabHeight(CDPageControl.PageIndex, CDPageControl) - 4;
    RButtHeight := GetTabHeight(CDPageControl.PageIndex, CDPageControl);
    aRect := lRect;
    ADest.Pen.Style := psSolid;
    ADest.Brush.Style := bsSolid;
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(CL));
    //TColorToFPColor(ColorToRGB($009C9B91));
    ADest.Brush.FPColor := TColorToFPColor(ColorToRGB(CL));
    aRect.Left := lRect.Left;
    aRect.Top := lRect.Top;
    aRect.Bottom := lRect.Bottom;
    aRect.Right := lRect.Right;
    ADest.RecTangle(lRect);
    if CDPageControl.FPages.Count = 0 then
    begin
      ADest.Brush.Color := clWhite;
      ADest.Pen.Color := $009C9B91;
      ADest.RecTangle(Rect(aRect.Left, aRect.Top, aRect.Right + 1, aRect.Bottom + 2));
      ADest.Pen.Color := clWhite;
      ADest.Line(aRect.Left + 1, aRect.Bottom + 1, aRect.Right, aRect.Bottom + 1);
      Exit;
    end;
    aRect.Left := lRect.Left + 2;
    aRect.Top := lRect.Top + 3;
    //ADest.TextStyle.Opaque :=false;
    //SetBkMode(ADest.Handle, TRANSPARENT);
    if ADest.Brush.Style = bsSolid then
      SetBkMode(ADest.Handle, OPAQUE)
    else
      SetBkMode(ADest.Handle, TRANSPARENT);

    for i := StartIndex to CDPageControl.FPages.Count - 1 do
    begin
      aText := CDPageControl.FPages[i].TabPage.Caption;
      rWidth := (CaptionHeight - ADest.TextHeight(aText)) + ADest.TextWidth(aText);
      CDPageControl.FPages[i].Width := rWidth;
      if aRect.Left + rWidth > lRect.Right - 6 then
        Break
      else
        aRect.Right := aRect.Left + rWidth;
      if CDPageControl.PageIndex = i then
      begin
        cRect := aRect;
        if i = StartIndex then
          cRect.Left := aRect.Left - 2
        else
          cRect.Left := aRect.Left - 4;
        cRect.Right := aRect.Right + 4;
        cRect.Top := cRect.Top - 2;
        bText := CDPageControl.FPages[i].TabPage.Caption;
      end
      else
        DrawTabHead(aDest, aRect, CDPageControl.Color, False);
      MaskColor := MaskBaseColor + i - StartIndex;
      //DrawTabHeadMask(MaskHeadBmp.Canvas, aRect, MaskColor, False);
      ADest.TextOut(aRect.Left + (aRect.Right - aRect.Left - ADest.TextWidth(aText)) div 2,
        aRect.Top + (aRect.Bottom - aRect.Top - ADest.TextHeight(aText)) div 2, aText);
      aRect.Left := aRect.Right + 3;
    end;
    ADest.Line(lRect.Left, lRect.Bottom - 1, cRect.Left, lRect.Bottom - 1);
    ADest.Line(cRect.Right, lRect.Bottom - 1, lRect.Right, lRect.Bottom - 1);
    DrawTabHead(aDest, cRect, clWhite, True);
    ADest.TextOut(cRect.Left + (cRect.Right - cRect.Left - ADest.TextWidth(bText)) div 2,
      cRect.Top + (cRect.Bottom - cRect.Top - ADest.TextHeight(bText)) div 2, bText);
    if not CheckTabButton(lRect.Right - lRect.Left, CDPageControl.FPages) then
      Exit;
    aRect.Left := lRect.Right - RButtHeight * 2 - 3;
    aRect.Top := 1;
    aRect.Bottom := RButtHeight + 1;
    aRect.Right := lRect.Right - RButtHeight;
    //if FMDownL then
    //  GradFill(ADest, aRect, $00F1A079, $00EFAF9B)
    //else
      GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);
    aRect.Left := lRect.Right - RButtHeight - 1;
    aRect.Top := 1;
    aRect.Bottom := RButtHeight + 1;
    aRect.Right := lRect.Right;

    GradFill(ADest, aRect, $00FDD9CB, $00F2C9B8);

    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($0085614D));
    bRect.Top := 1;
    bRect.Left := lRect.Right - RButtHeight * 2 - 3;
    bRect.Right := lRect.Right;
    bRect.Bottom := RButtHeight + 1;
    DrawArrow(ADest, bRect, True);
    DrawArrow(ADest, bRect, False);
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clWhite));
    ADest.Line(lRect.Right - RButtHeight * 2 - 3, 1, lRect.Right, 1);
    ADest.Line(lRect.Right, 1, lRect.Right, RButtHeight + 1);
    ADest.Line(lRect.Right, RButtHeight + 1, lRect.Right - RButtHeight *
      2 - 3, RButtHeight + 1);
    ADest.Line(lRect.Right - RButtHeight * 2 - 3, RButtHeight + 1,
      lRect.Right - RButtHeight * 2 - 3, 1);
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($00E5BAA7));
    ADest.Brush.Style := bsClear;
    ADest.Rectangle(lRect.Right - RButtHeight * 2 - 2, 2, lRect.Right -
      1, RButtHeight + 1);
    CornerColor := TColorToFPColor(ColorToRGB($00F6E3D9));
    ADest.Colors[lRect.Right - RButtHeight * 2 - 2, 2] := CornerColor;
    ADest.Colors[lRect.Right - RButtHeight * 2 - 2, RButtHeight] := CornerColor;
    ADest.Colors[lRect.Right - 1, 2] := CornerColor;
    ADest.Colors[lRect.Right - 1, RButtHeight] := CornerColor;
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB(clWhite));
    ADest.Line(lRect.Right - 51, 1, lRect.Right, 1);
    ADest.Line(lRect.Right, 1, lRect.Right, 25);
    ADest.Line(lRect.Right, 25, lRect.Right - 51, 25);
    ADest.Line(lRect.Right - 51, 25, lRect.Right - 51, 1);
    ADest.Pen.FPColor := TColorToFPColor(ColorToRGB($00FFFFFF));}
end;

procedure TCDCustomTabControlDrawerKDE.DrawTabs(ADest: TCanvas; CDTabControl: TCDCustomTabControl);
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

procedure TCDCustomTabControlDrawerKDE.DrawTab(ADest: TCanvas;
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

function TCDCustomTabControlDrawerKDE.GetPageIndexFromXY(x, y: integer
  ): integer;
begin
  Result := 1;
end;

function TCDCustomTabControlDrawerKDE.GetTabHeight(AIndex: Integer; CDTabControl: TCDCustomTabControl): Integer;
begin
  if CDTabControl.Font.Size = 0 then
    Result := 32
  else
    Result := CDTabControl.Font.Size + 22;
end;

function TCDCustomTabControlDrawerKDE.GetTabWidth(ADest: TCanvas;
  AIndex: Integer; CDTabControl: TCDCustomTabControl): Integer;
const
  TCDTabControl_KDE_TabCaptionExtraWidth = 20;
var
  lCaption: string;
begin
  lCaption := CDTabControl.Tabs.Strings[AIndex];

  Result := ADest.TextWidth(lCaption) + TCDTabControl_KDE_TabCaptionExtraWidth;
end;

{function TCDCustomTabControlDrawerKDE.GetClientRect(AControl: TCDControl
  ): TRect;
var
  lCaptionHeight: Integer;
begin
  lCaptionHeight := GetTabHeight(CDTabControl.FTabIndex) - 4;

  Result := Rect(5, lCaptionHeight + 1, CDTabControl.Width - 10,
    CDTabControl.Height - lCaptionHeight - 5);
end;}

procedure TCDCustomTabControlDrawerKDE.DrawToIntfImage(ADest: TFPImageCanvas;
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

procedure TCDCustomTabControlDrawerKDE.DrawToCanvas(ADest: TCanvas; CDTabControl: TCDCustomTabControl);
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

procedure TCDCustomTabControlDrawerKDE.DrawTabSheet(ADest: TCanvas; CDTabControl: TCDCustomTabControl);
begin
  ADest.Brush.Color := CDTabControl.Color;
  ADest.Brush.Style := bsSolid;
  ADest.Pen.Style := psClear;
  ADest.Rectangle(0, 0, CDTabControl.Width, CDTabControl.Height);
end;

procedure TCDCustomTabControlDrawerKDE.MouseDown(Button: TMouseButton;
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

procedure TCDCustomTabControlDrawerKDE.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer; CDTabControl: TCDCustomTabControl);
begin

end;

initialization

  RegisterCustomTabControlDrawer(TCDCustomTabControlDrawerKDE.Create, dsKDE);

end.

