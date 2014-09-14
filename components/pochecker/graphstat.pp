unit GraphStat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  PoFamilies, PoCheckerConsts, Math, LCLProc;

type

  { TGraphStatForm }

  TGraphStatForm = class(TForm)
    Img: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    WidthPerStat: Integer;
    HeightPerStat: Integer;
    LegHeight: Integer;
    SingleLegItemHeight: Integer;
    FPoFamilyStats: TPoFamilyStats;
    procedure CalcBoundsRequiredPerStat(out AWidth, AHeight: Integer);
    procedure PrepareDimensions;
    procedure DrawGraphs;
    procedure DrawGraph(AStat: TStat; ARow, ACol: Integer);
    procedure DrawLegenda;
  public
    { public declarations }
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats write FPoFamilyStats;
  end;

var
  GraphStatForm: TGraphStatForm;

implementation

{$R *.lfm}

const
  Radius = 30;
  Cols = 4;
  clBack = clWhite;
  clPie = clForm;
  clTrans = clGreen;
  clUnTrans = clRed;
  clFuzzy = clFuchsia;
  clCircle = clBlack;
  clTxt = clBlack;
  LMargin = 10;
  RMargin = 10;
  TMargin = 10;
  BMargin = 10;
  TxtMargin = 5;
  LegTMargin = 40;
  LegBMargin = 10;
  LegLMargin = LMargin;
  LegBarHeight = 20;
  LegBarWidth = 40;

{ TGraphStatForm }

procedure TGraphStatForm.FormShow(Sender: TObject);
begin
  PrepareDimensions;
  DrawGraphs;
  DrawLegenda;
end;

procedure TGraphStatForm.FormCreate(Sender: TObject);
begin
  Caption := sGrapStatFormCaption;
end;

procedure TGraphStatForm.CalcBoundsRequiredPerStat(out AWidth, AHeight: Integer);
var
  i, TW, TH, MaxTH: Integer;
  Stat: TStat;
begin
  AWidth := 2 * Radius;
  MaxTH := 0;
  for i := 0 to FPoFamilyStats.Count - 1 do
  begin
    Stat := FPoFamilyStats.Items[i];
    TW := Img.Canvas.TextWidth(Stat.PoName);
    TH := Img.Canvas.TextHeight(Stat.PoName);
    if TW > AWidth then AWidth := TW;
    if TH > MaxTH then MaxTH := TH;
  end;
  //Add margins
  AWidth := AWidth + LMargin + RMargin; //L+R
  AHeight := TMargin + 2 * Radius + TxtMargin + MaxTH + BMargin; //Top+distance between text and graph+Bottom
  //Debugln('TGraphStatForm.CalcBoundsRequiredPerStat: AWidth = ',DbgS(AWidth),' AHeight = ',DbgS(AHeight),' MaxTH = ',DbgS(MaxTH));
end;

procedure TGraphStatForm.PrepareDimensions;
var
  Rows, IHeight: Integer;
begin
  CalcBoundsRequiredPerStat(WidthPerStat, HeightPerStat);
  Img.Width := Cols * WidthPerStat;
  Rows := Ceil(PoFamilyStats.Count / Cols);
  IHeight := HeightPerStat * Rows;
  SingleLegItemHeight := Max(Img.Canvas.TextHeight('qWM'), LegBarHeight);
  LegHeight := LegTMargin + TxtMargin + (3 * SingleLegItemHeight + TxtMargin) + LegBMargin;
  IHeight := IHeight + LegHeight;

  ClientWidth := Img.Width;
  Img.Height := IHeight;
  Img.Height := IHeight;
  //DebugLn('TGraphStatForm.FormShow: Img.Width  -> ',DbgS(Img.Width), ' Canvas.Width  = ',DbgS(Img.Canvas.Width));
  //DebugLn('TGraphStatForm.FormShow: Img.Height -> ',DbgS(Img.Height),' Canvas.Heigth = ',DbgS(Img.Canvas.Height));
  ClientHeight := Min(Screen.Height - 50, Img.Height);
  VertScrollBar.Visible := (ClientHeight < Img.Height);
  if Top + ClientHeight + 50 > Screen.Height then Top := 0;
end;

procedure TGraphStatForm.DrawGraphs;
var
  Index, ARow, ACol: Integer;
begin
  //debugln('TGraphStatForm.DrawGraphs: FPoFamilyStats.Count = ',DbgS(FPoFamilyStats.Count));
  Img.Canvas.Brush.Color := clBack;
  Img.Canvas.Clear;
  Img.Canvas.FillRect(0,0,Img.Width,Img.Height);
  for Index := 0 to FPoFamilyStats.Count - 1 do
  begin
    ARow := Index mod Cols;
    ACol := Index div Cols;
    DrawGraph(FPoFamilyStats.Items[Index], ARow, ACol);
  end;
end;

procedure TGraphStatForm.DrawGraph(AStat: TStat; ARow, ACol: Integer);
var
  X,Y, TW: Integer;
  Origo: TPoint;
  OrigoCol, PixCol: TColor;
begin
  //debugln('TGraphStatForm.DrawGraph: PoName = ',DbgS(AStat.PoName));
  Origo.X := (ARow * WidthPerStat) + (WidthPerStat div 2);
  Origo.Y := (ACol * HeightPerStat) + Radius + TMargin;
  //debugln('TGraphStatForm.DrawGraph: ARow = ',DbgS(ARow),' ACol = ',DbgS(ACol));
  //debugln('TGraphStatForm.DrawGraph: Origo.X = ',DbgS(Origo.X),' Origo.Y = ',DbgS(Origo.Y));

  {
  Img.Canvas.Pen.Color := clCircle;
  Img.Canvas.Brush.Color := clPie;
  Img.Canvas.EllipseC(Origo.X,Origo.Y,Radius,Radius);
  }

  with Img do
  begin
    //First draw a circle
    Canvas.Brush.Color := clPie;
    Canvas.Pen.Color := clCircle;
    Canvas.EllipseC(Origo.X,Origo.Y,Radius,Radius);

    //Draw the Translated section
    Canvas.MoveTo(Origo);
    Canvas.LineTo(Origo.X + Radius, Origo.Y);
    Canvas.MoveTo(Origo);

    X := Origo.X + Round(Cos(AStat.FracTranslated * (2 * pi)) * Radius);
    Y := Origo.Y - Round(Sin(AStat.FracTranslated * (2 * pi)) * Radius);
    Canvas.LineTo(X, Y);
    OrigoCol := Canvas.Pixels[Origo.X,Origo.Y];
    //Calculate a point inside the section
    X := Origo.X + Round(Cos((AStat.FracTranslated/2) * (2 * pi)) * (Radius - 3));
    Y := Origo.Y - Round(Sin((AStat.FracTranslated/2) * (2 * pi)) * (Radius - 3));
    Canvas.Brush.Color := clTrans;
    PixCol := Canvas.Pixels[X,Y];
    //Don't FloodFill if the section has no visible pixels on the screen
    if (PixCol <> OrigoCol) then Canvas.FloodFill(X,Y,PixCol,fsSurface);


    //Draw the UnTranslated section
    Canvas.MoveTo(Origo);
    X := Origo.X + Round(Cos((AStat.FracTranslated+AStat.FracUnTranslated) * 2 * pi) * Radius);
    Y := Origo.Y - Round(Sin((AStat.FracTranslated+AStat.FracUnTranslated) * 2 * pi) * Radius);
    Canvas.LineTo(X, Y);
    //Calculate a point inside the section
    X := Origo.X + Round(Cos((AStat.FracTranslated+AStat.FracUnTranslated/2) * (2 * pi)) * (Radius - 3));
    Y := Origo.Y - Round(Sin((AStat.FracTranslated+AStat.FracUnTranslated/2) * (2 * pi)) * (Radius - 3));
    Canvas.Brush.Color := clUnTrans;
    PixCol := Canvas.Pixels[X,Y];
    //Don't FloodFill if the section has no visible pixels on the screen
    if (PixCol <> OrigoCol) then Canvas.FloodFill(X,Y,PixCol,fsSurface);


    //Draw the Fuzzy section
    Canvas.MoveTo(Origo);
    X := Origo.X + Round(Cos((AStat.FracTranslated+AStat.FracUnTranslated+AStat.FracFuzzy) * 2 * pi) * Radius);
    Y := Origo.Y - Round(Sin((AStat.FracTranslated+AStat.FracUnTranslated+AStat.FracFuzzy) * 2 * pi) * Radius);
    Canvas.LineTo(X, Y);
    //Calculate a point inside the section
    X := Origo.X + Round(Cos((AStat.FracTranslated+AStat.FracUnTranslated+AStat.FracFuzzy/2) * (2 * pi)) * (Radius - 3));
    Y := Origo.Y - Round(Sin((AStat.FracTranslated+AStat.FracUnTranslated+AStat.FracFuzzy/2) * (2 * pi)) * (Radius - 3));
    Canvas.Brush.Color := clFuzzy;
    PixCol := Canvas.Pixels[X,Y];
    //Don't FloodFill if the section has no visible pixels on the screen
    if (PixCol <> OrigoCol) then Canvas.FloodFill(X,Y,PixCol,fsSurface);
    //Draw the text
    TW := Canvas.TextWidth(AStat.PoName);
    X := Origo.X - (TW div 2);
    Y := Origo.Y + Radius + TxtMargin;
    Canvas.Pen.Color := clTxt;
    Canvas.Brush.Color := clBack;
    Canvas.TextOut(X, Y, AStat.PoName);
  end;

end;

procedure TGraphStatForm.DrawLegenda;
var
  X, Y, LegTop, TH: Integer;
begin
  LegTop := Img.Height - LegHeight;
  //Debugln('TGraphStatForm.DrawLegenda: LegTop = ',DbgS(LegTop));
  with Img do
  begin
    TH := Canvas.TextHeight('qWM');
    X := LegLMargin;
    Y := LegTop;
    Canvas.Pen.Color := clTxt;
    Canvas.MoveTo(LegLMargin, LegTop);
    Canvas.LineTo(Img.Width - LegLMargin, LegTop);
    Y := Y + TxtMargin;

    Canvas.Brush.Color := clTrans;
    Canvas.FillRect(X,Y,X+LegBarWidth,Y+LegBarHeight);
    Canvas.Brush.Color := clBack;
    Canvas.TextOut(X + LegBarWidth + TxtMargin, Y + ((LegBarHeight - TH) div 2), 'Translated');

    Y := Y + SingleLegItemHeight + TxtMargin;
    Canvas.Brush.Color := clUnTrans;
    Canvas.FillRect(X,Y,X+LegBarWidth,Y+LegBarHeight);
    Canvas.Brush.Color := clBack;
    Canvas.TextOut(X + LegBarWidth + TxtMargin, Y + ((LegBarHeight - TH) div 2), 'Untranslated');


    Y := Y + SingleLegItemHeight + TxtMargin;
    Canvas.Brush.Color := clFuzzy;
    Canvas.FillRect(X,Y,X+LegBarWidth,Y+LegBarHeight);
    Canvas.Brush.Color := clBack;
    Canvas.TextOut(X + LegBarWidth + TxtMargin, Y + ((LegBarHeight - TH) div 2), 'Fuzzy');
  end;
end;

end.

