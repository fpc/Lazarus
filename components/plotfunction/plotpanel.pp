{ Unit to plot a function on a canvas

  Copyright (C) 2008 Michael Van Canneyt Michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit plotpanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, graphics, extctrls, ldocktree;

Const
  DefLeftMargin       = 60;
  DefRightMargin      = 20;
  DefTopMargin        = 40;
  DefBottomMargin     = DefTopMargin;
  DefXTickSize        = 5;
  DefYTickSize        = DefXTickSize;
  DefXTicks           = 10;
  DefYTicks           = DefXTicks;
  DefAxisColor        = clGreen;
  DefTickColor        = DefAxisColor;
  DefLegendInterval   = 2;
  DefInterval         = 100;
  DefCaptionAlignment = taRightJustify;
  DefGridInterval     = 1;
  DefGridColor        = clSilver;
  DefPlotColor        = clRed;

Type
  TPlotFloat = Double;
  TCanvasPlotter = Class;

  { TPlotCaption }

  TPlotCaption = Class(TPersistent)
  private
    FAlignment: TAlignment;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FTitle: String;
    procedure SetAlignment(const AValue: TAlignment);
    procedure SetFont(const AValue: TFont);
    procedure SetTitle(const AValue: String);
  Protected
    Procedure Changed;
    Property OnChange : TNotifyEvent Read FOnChange Write FOnChange;
  Public
    Constructor Create;
    Destructor Destroy; override;
  Published
    Property Title : String Read FTitle Write SetTitle;
    Property Font : TFont Read FFont Write SetFont;
    Property Alignment : TAlignment Read FAlignment Write SetAlignment;
  end;

  { TPlotAxis }
  TTickMode = (tmCount,tmDelta);
  TPlotAxis = Class(TPersistent)
  private
    FDrawZero: Boolean;
    FGridColor: TColor;
    FGridInterval: Integer;
    FInterval: TPlotFloat;
    FCaption: TPlotCaption;
    FLegendInterval: Integer;
    FLegendFormat: String;
    FOrigin: TPlotFloat;
    FPlotter : TCanvasPlotter;
    FColor: TColor;
    FTickColor: TColor;
    FTickFont: TFont;
    FTickMode: TTickMode;
    FTicks: Integer;
    FTickSize: integer;
    procedure SetAxisColor(const AValue: TColor);
    procedure SetDrawZero(const AValue: Boolean);
    procedure SetInterval(const AValue: TPlotFloat);
    procedure SetCaption(const AValue: TPlotCaption);
    procedure SetLegendInterval(const AValue: Integer);
    procedure SetLegendFormat(const AValue: String);
    procedure SetOrigin(const AValue: TPlotFloat);
    procedure SetTickColor(const AValue: TColor);
    procedure SetTickFont(const AValue: TFont);
    procedure SetTickMode(const AValue: TTickMode);
    procedure SetTicks(const AValue: Integer);
    procedure SetTickSize(const AValue: integer);
  Protected
    Procedure DoCaptionChange(Sender : TObject);
    procedure Changed;
    Property Plotter : TCanvasPlotter read FPlotter;
    Function TickDelta : Double;
    Function ValueDelta : TPlotFloat;
    Function GetDimension : Integer;virtual; abstract;
    Function Margin1 : Integer;virtual; abstract;
    Function Margin2 : Integer;virtual; abstract;
  Public
    Constructor Create;virtual;
    Destructor Destroy; override;
  Published
    // Graph color
    Property Color : TColor Read FColor Write SetAxisColor default defAxisColor;
    // Color of ticks on axis
    Property TickColor : TColor Read FTickColor Write SetTickColor default defAxisColor;
    // Number or distance (in pixels) between ticks on axis
    Property Ticks : Integer Read FTicks Write SetTicks;
    // Length of ticks on axis
    Property TickSize : integer Read FTickSize Write SetTickSize;
    // Ticks is number of ticks or distance (in pixels) between ticks ?
    Property TickMode : TTickMode Read FTickMode Write SetTickMode;
    // Font for tick legend
    Property TickFont : TFont Read FTickFont Write SetTickFont;
    // Caption of axis
    Property Caption : TPlotCaption Read FCaption Write SetCaption;
    // Draw zero axis if interval if interval starts at negative values ?
    Property DrawZero : Boolean Read FDrawZero Write SetDrawZero;
    // Value in X/Y of origin of the axis. X/Y value starts here.
    Property Origin : TPlotFloat Read FOrigin Write SetOrigin;
    // Interval to cover in X/Y. X/Y run in [Origin,Origin+Interval]
    Property Interval : TPlotFloat Read FInterval Write SetInterval;
    // Write value in X/Y of ticks every LegendInterval ticks. 0 is no legend.
    Property LegendInterval : Integer Read FLegendInterval Write SetLegendInterval default DeflegendInterval;
    // Format for legend (formatfloat);
    Property LegendFormat : String Read FLegendFormat write SetLegendFormat;
    // Interval (in ticks) of grid. 0 means no grid.
    Property GridInterval : Integer Read FGridInterval Write FGridInterval default DefGridInterval;
    // Grid color.
    Property GridColor    : TColor Read FGridColor Write FGridColor default DefGridColor;
  end;

  { TPlotXAxis }

  TPlotXAxis = Class(TPlotAxis)
  private
    FLeftMargin: Integer;
    FRightMargin: Integer;
    procedure SetLeftMargin(const AValue: Integer);
    procedure SetRightMargin(const AValue: Integer);
  Protected
    Function GetDimension : Integer;override;
    Function Margin1 : Integer;override;
    Function Margin2 : Integer;override;
  Public
    Constructor Create; override;
  Published
    // Start of X origin in pixels.
    Property LeftMargin : Integer Read FLeftMargin Write SetLeftMargin;
    // End of X range from right edge in pixels.
    Property RightMargin : Integer Read FRightMargin Write SetRightMargin;
    Property Ticks Default DefXTicks;
    Property TickSize Default DefXTickSize;
  end;

  { TPlotYAxis }

  TPlotYAxis = Class(TPlotAxis)
  private
    FBottomMargin: Integer;
    FTopMargin: Integer;
    procedure SetBottomMargin(const AValue: Integer);
    procedure SetTopMargin(const AValue: Integer);
  Protected
    Function GetDimension : Integer;override;
    Function Margin1 : Integer;override;
    Function Margin2 : Integer;override;
  Public
    Constructor Create; override;
  Published
    // End of Y range from top edge in pixels.
    Property TopMargin : Integer Read FTopMargin Write SetTopMargin;
    // Start of Y range (Y origin) from bottom edge in pixels.
    Property BottomMargin : Integer Read FBottomMargin Write SetBottomMargin;
  end;

  { TCanvasPlotter }

  TCanvasPlotter = Class(TComponent)
  private
    FActive: Boolean;
    FBackGroundColor: TColor;
    FBoundsRect: TRect;
    FCaption: TPlotCaption;
    FPlotColor: TColor;
    FXaxis: TPlotXAxis;
    FYaxis: TPlotYAxis;
    FCanvas: TCanvas;
    FBitmap : TBitmap;
    FLastLegend : String;
    FLastFont : TFont;
    procedure DrawCaption(ACanvas: TCanvas);
    procedure SetActive(const AValue: Boolean);
    procedure SetBackGroundColor(const AValue: TColor);
    procedure SetBoundsRect(const AValue: TRect);
    procedure SetCanvas(const AValue: TCanvas);
    procedure SetCaption(const AValue: TPlotCaption);
    procedure SetPlotColor(const AValue: TColor);
    procedure SetXAxis(const AValue: TPlotXAxis);
    procedure SetYAxis(const AValue: TPlotYAxis);
  Protected
    function GetRotatedLegend(ACanvas : TCanvas): TBitmap;
    Procedure DrawBackground(ACanvas: TCanvas);
    procedure DrawHAxis(ACanvas: TCanvas; Const AHAxis,AVAxis : TPlotAxis); virtual;
    procedure DrawVAxis(ACanvas: TCanvas; Const AVAxis,AHAxis : TPlotAxis); virtual;
    Procedure PlotFunction(ACanvas : TCanvas); virtual;
    Function GetHDimension : Integer; virtual;
    Function GetVDimension : Integer; virtual;
    Function CalcFunction(X : TPlotFloat) : TPlotFloat; virtual;
    Procedure Changed; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Draw;
    Property BoundsRect: TRect Read FBoundsRect Write SetBoundsRect;
    Property Canvas : TCanvas Read FCanvas Write SetCanvas;
    Property XAxis : TPlotXAxis Read FXaxis Write SetXAxis;
    Property YAxis : TPlotYAxis Read FYaxis Write SetYAxis;
    Property BackgroundColor : TColor Read FBackGroundColor Write SetBackGroundColor;
    Property PlotColor : TColor Read FPlotColor Write SetPlotColor;
    Property Active : Boolean Read FActive Write SetActive;
    Property Caption : TPlotCaption Read FCaption Write SetCaption;
  end;

  { TControlPlotter }

  TControlPlotter = Class(TCanvasPlotter)
  Protected
    FControl : TControl;
    Procedure Changed; override;
  Public
    Constructor Create(AOwner : TComponent); override;
  end;

  { TEventControlPlotter }
  TOnCalcPlotEvent = Procedure(Const X : TPlotFloat; Out Y : TPlotFloat) of Object;

  TEventControlPlotter = Class(TCanvasPlotter)
  private
    FOnCalcPlot : TOnCalcPlotEvent;
    procedure SetOnCalcPlot(const AValue: TOnCalcPlotEvent);
  Protected
    Function CalcFunction(X : TPlotFloat) : TPlotFloat; override;
  Public
    Property OnCalcPlot : TOnCalcPlotEvent Read FOnCalcPlot Write SetOnCalcPlot;
  end;

  { TCustomPlotFunctionPanel }

  TCustomPlotFunctionPanel = Class(TGraphicControl)// (CustomPanel)
  private
    FPlotter: TCanvasPlotter;
    function GetActive: Boolean;
    function GetCaption: TPlotCaption;
    function GetPlotColor: TColor;
    function GetXaxis: TPlotXAxis;
    function GetYaxis: TPlotYAxis;
    procedure SetActive(const AValue: Boolean);
    procedure SetCaption(const AValue: TPlotCaption);
    procedure SetPlotColor(const AValue: TColor);
    procedure SetXAxis(const AValue: TPlotXAxis);
    procedure SetYAxis(const AValue: TPlotYAxis);
  Protected
    Function CreatePlotter : TCanvasPlotter; virtual;
    procedure Paint; override;
    Property Plotter : TCanvasPlotter Read FPlotter;
    procedure SetColor(Value: TColor); override;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Anchors;
    Property Align;
    Property XAxis : TPlotXAxis Read GetXaxis Write SetXAxis;
    Property YAxis : TPlotYAxis Read GetYaxis Write SetYAxis;
    Property Active : Boolean Read GetActive Write SetActive;
    Property PlotColor : TColor Read GetPlotColor Write SetPlotColor;
    Property Caption : TPlotCaption Read GetCaption Write SetCaption;
  end;

  { TPlotFunctionPanel }

  TPlotFunctionPanel = Class(TCustomPlotFunctionPanel)
  private
    function GetOnCalcPlot: TOnCalcPlotEvent;
    procedure SetOnCalcPlot(AValue: TOnCalcPlotEvent);
  Protected
    Function CreatePlotter : TCanvasPlotter; override;
  Published
    Property OnCalcPlot : TOnCalcPlotEvent Read GetOnCalcPlot Write SetOnCalcPlot;
    Property Anchors;
    Property Align;
    Property XAxis;
    Property YAxis;
    Property Active;
    Property PlotColor;
  end;

  EPlotPanel = Class(Exception);

implementation

uses
  lcltype,  // Rotated font support
  lclintf,
  graphtype,
  intfgraphics,
  fpimage,
  interfacebase; // To detect widget set.


resourcestring
  SerrInvalidInterval = 'Invalid interval. Interval must be a positive number: %f';
  DefXCaption = 'X values';
  DefYCaption = 'Y values';

{ TCustomPlotFunctionPanel }

procedure TCustomPlotFunctionPanel.SetXAxis(const AValue: TPlotXAxis);
begin
  PLotter.XAxis.assign(AValue);
end;

function TCustomPlotFunctionPanel.GetActive: Boolean;
begin
  Result:=FPlotter.Active;
end;

function TCustomPlotFunctionPanel.GetCaption: TPlotCaption;
begin
  Result:=FPlotter.Caption;
end;

function TCustomPlotFunctionPanel.GetPlotColor: TColor;
begin
  Result:=FPlotter.PlotColor;
end;

function TCustomPlotFunctionPanel.GetXaxis: TPlotXAxis;
begin
  Result:=FPlotter.XAxis;
end;

function TCustomPlotFunctionPanel.GetYaxis: TPlotYAxis;
begin
  Result:=FPlotter.YAxis;
end;

procedure TCustomPlotFunctionPanel.SetActive(const AValue: Boolean);
begin
  FPlotter.Active:=AValue;
end;

procedure TCustomPlotFunctionPanel.SetCaption(const AValue: TPlotCaption);
begin
  FPlotter.Caption.Assign(AValue);
end;

procedure TCustomPlotFunctionPanel.SetPlotColor(const AValue: TColor);
begin
  FPlotter.PlotColor:=AValue;
end;

procedure TCustomPlotFunctionPanel.SetYAxis(const AValue: TPlotYAxis);
begin
  FPlotter.Yaxis.Assign(AValue);
end;


procedure TCustomPlotFunctionPanel.Paint;
begin
  FPlotter.FBoundsRect:=Self.ClientRect;
  FPlotter.Draw;
end;

procedure TCustomPlotFunctionPanel.SetColor(Value: TColor);
begin
  inherited SetColor(Value);
  If Assigned(FPlotter) then
    FPLotter.BackgroundColor:=Value;
end;


constructor TCustomPlotFunctionPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width:=320;
  Height:=200;
  FPlotter:=CreatePlotter;
  FPlotter.FCanvas:=Self.Canvas;
end;

destructor TCustomPlotFunctionPanel.Destroy;
begin
  inherited Destroy;
end;

function TCustomPlotFunctionPanel.CreatePlotter: TCanvasPlotter;
begin
  Result:=TControlPlotter.Create(Self);
end;

{ TPlotAxis }

procedure TPlotAxis.SetAxisColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
end;


procedure TPlotAxis.SetDrawZero(const AValue: Boolean);
begin
  if FDrawZero=AValue then exit;
  FDrawZero:=AValue;
  Changed;
end;

procedure TPlotAxis.SetInterval(const AValue: TPlotFloat);
begin
  if FInterval=AValue then exit;
  If FInterval<=0 then
    Raise EPlotPanel.CreateFmt(SerrInvalidInterval,[AValue]);
  FInterval:=AValue;
end;

procedure TPlotAxis.SetCaption(const AValue: TPlotCaption);
begin
  if FCaption=AValue then exit;
  FCaption.Assign(AValue);
  Changed;
end;

procedure TPlotAxis.SetLegendInterval(const AValue: Integer);
begin
  if FLegendInterval=AValue then exit;
  FLegendInterval:=AValue;
  If FLegendInterval<0 then
    FLegendInterval:=0;
  Changed;
end;

procedure TPlotAxis.SetLegendFormat(const AValue: String);
begin
  if FLegendFormat=AValue then exit;
  FLegendFormat:=AValue;
  Changed;
end;

procedure TPlotAxis.SetOrigin(const AValue: TPlotFloat);
begin
  if FOrigin=AValue then exit;
  FOrigin:=AValue;
  Changed;
end;

procedure TPlotAxis.SetTickColor(const AValue: TColor);
begin
  if FTickColor=AValue then exit;
  FTickColor:=AValue;
  Changed;
end;

procedure TPlotAxis.SetTickFont(const AValue: TFont);
begin
  if FTickFont=AValue then exit;
  FTickFont:=AValue;
  Changed;
end;

procedure TPlotAxis.SetTickMode(const AValue: TTickMode);
begin
  if FTickMode=AValue then exit;
  If Not Assigned(FPlotter) then
    FTicks:=1
  else
    FTicks:=GetDimension div FTicks;
  FTickMode:=AValue;
  Changed;
end;

procedure TPlotAxis.SetTicks(const AValue: Integer);
begin
  if FTicks=AValue then exit;
  FTicks:=AValue;
  If FTicks<1 then
    FTicks:=1;
  Changed;
end;

procedure TPlotAxis.SetTickSize(const AValue: integer);
begin
  if FTickSize=AValue then exit;
  FTickSize:=AValue;
  If FTickSize<1 then
    FTickSize:=1;
  Changed;
end;

procedure TPlotAxis.DoCaptionChange(Sender: TObject);
begin
  Changed;
end;

procedure TPlotAxis.Changed;
begin
  If Assigned(FPlotter) then
    FPlotter.Changed;
end;

function TPlotAxis.TickDelta: Double;
begin
  Case FTickMode of
    tmCount : Result:=GetDimension / (Ticks);
    tmDelta : Result:=Ticks;
  end;
end;

function TPlotAxis.ValueDelta: TPlotFloat;
begin
  Case FTickMode of
    tmCount : Result:=Interval / Ticks;
    tmDelta : Result:=Interval / TickDelta;
  end;
end;

constructor TPlotAxis.Create;
begin
  inherited Create;
  FCaption:=TPlotCaption.Create;
  FCaption.FOnChange:=@DoCaptionChange;
  FColor:=DefAxisColor;
  FTickFont:=TFont.Create;
  FLegendInterval:=DefLegendInterval;
  FInterval:=DefInterval;
  FTickColor:=DefTickColor;
  FGridColor:=DefGridColor;
  FGridInterval:=DefGridInterval;
end;

destructor TPlotAxis.Destroy;
begin
  inherited Destroy;
end;

{ TPlotXAxis }

procedure TPlotXAxis.SetLeftMargin(const AValue: Integer);
begin
  if FLeftMargin=AValue then exit;
  FLeftMargin:=AValue;
end;

procedure TPlotXAxis.SetRightMargin(const AValue: Integer);
begin
  if FRightMargin=AValue then exit;
  FRightMargin:=AValue;
end;

function TPlotXAxis.GetDimension: Integer;
begin
  Result:=FPlotter.GetHDimension-Leftmargin-RightMargin;
  If Result<0 then
    Result:=0;
end;

function TPlotXAxis.Margin1: Integer;
begin
  Result:=FLeftmargin;
end;

function TPlotXAxis.Margin2: Integer;
begin
  Result:=FRightMargin;
end;

constructor TPlotXAxis.Create;
begin
  inherited Create;
  FLeftMargin:=DefLeftMargin;
  FRightMargin:=DefRightMargin;
  FTicks:=DefXTicks;
  FTickSize:=DefXTickSize;
  FCaption.Title:=DefXCaption;
end;

{ TPlotYAxis }

procedure TPlotYAxis.SetBottomMargin(const AValue: Integer);
begin
  if FBottomMargin=AValue then exit;
  FBottomMargin:=AValue;
  Changed;
end;

procedure TPlotYAxis.SetTopMargin(const AValue: Integer);
begin
  if FTopMargin=AValue then exit;
  FTopMargin:=AValue;
  Changed;
end;

function TPlotYAxis.GetDimension: Integer;
begin
  Result:=FPLotter.GetVDimension-TopMargin-BottomMargin;
end;

function TPlotYAxis.Margin1: Integer;
begin
  Result:=FBottomMargin;
end;

function TPlotYAxis.Margin2: Integer;
begin
  Result:=FTopMargin;
end;

constructor TPlotYAxis.Create;
begin
  inherited Create;
  FTopMargin:=DefTopMargin;
  FBottomMargin:=DefBottomMargin;
  FTicks:=DefYTicks;
  FTickSize:=DefYTickSize;
  FCaption.FTitle:=DefYCaption;
end;

{ TCanvasPlotter }

procedure TCanvasPlotter.SetActive(const AValue: Boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  Changed;
end;

procedure TCanvasPlotter.SetBackGroundColor(const AValue: TColor);
begin
  if FBackGroundColor=AValue then exit;
  FBackGroundColor:=AValue;
  Changed;
end;

procedure TCanvasPlotter.SetBoundsRect(const AValue: TRect);
begin
  if (FBoundsRect.Left=AValue.left) and
     (FBoundsRect.Top=AValue.Top) and
     (FBoundsRect.Bottom=AValue.Bottom) and
     (FBoundsRect.RIght=AValue.Right) then exit;
  FBoundsRect:=AValue;
  Changed;
end;

procedure TCanvasPlotter.SetCanvas(const AValue: TCanvas);
begin
  if FCanvas=AValue then exit;
  FCanvas:=AValue;
  Changed;
end;

procedure TCanvasPlotter.SetCaption(const AValue: TPlotCaption);
begin
  if FCaption=AValue then exit;
  FCaption.Assign(AValue);
  Changed;
end;

procedure TCanvasPlotter.SetPlotColor(const AValue: TColor);
begin
  If (FPlotColor=AValue) then Exit;
  FPlotColor:=AValue;
  Changed;
end;

procedure TCanvasPlotter.SetXAxis(const AValue: TPlotXAxis);
begin
  if FXaxis=AValue then exit;
  FXaxis.Assign(AValue);
  Changed;
end;

procedure TCanvasPlotter.SetYAxis(const AValue: TPlotYAxis);
begin
  if FYaxis=AValue then exit;
  FYaxis.Assign(AValue);
  Changed;
end;


function TCanvasPlotter.CalcFunction(X: TPlotFloat): TPlotFloat;
begin
  Result:=0;
end;

procedure TCanvasPlotter.Changed;

begin
  Draw;
end;

constructor TCanvasPlotter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXAxis:=TPlotXAxis.Create;
  FXAxis.FPlotter:=Self;
  FYAxis:=TPlotYAxis.Create;
  FYAxis.FPlotter:=Self;
  FPlotColor:=DefPlotColor;
  FCaption:=TPlotCaption.Create;
end;

destructor TCanvasPlotter.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FXAxis);
  FreeAndNil(FYAxis);
  FreeAndNil(FCaption);
  FreeAndNil(FBitmap);
  FreeAndNil(FLastFont);
end;

procedure TCanvasPlotter.Draw;
begin
  If Not Assigned(FCanvas) then
    Exit;
  DrawBackGround(FCanvas);
  If Active then
    PlotFunction(FCanvas);
  If (FCaption.Title<>'') then
    DrawCaption(FCanvas);
end;

procedure TCanvasPlotter.DrawCaption(ACanvas: TCanvas);

Var
  CW,CH,CX : Integer;

begin
  CW:=ACanvas.TextWidth(FCaption.Title);
  CH:=ACanvas.TextHeight(FCaption.Title);
  Case Caption.Alignment of
    taLeftJustify  : CX:=BoundsRect.Left+FXAxis.LeftMargin;
    taRightJustify : CX:=BoundsRect.Right-CW-FXAxis.RightMargin;
    taCenter       : CX:=BoundsRect.Left+FXAxis.LeftMargin+(GetHDimension-CW-FXAxis.RightMargin-FXAxis.LeftMargin) div 2;
  end;
  ACanvas.Font:=FCaption.Font;
  CH:=BoundsRect.Top+(FYAxis.TopMargin-CH) div 2;
  ACanvas.TextOut(CX,CH,FCaption.Title);
end;

procedure TCanvasPlotter.DrawBackground(ACanvas: TCanvas);

begin
//  self.ClientRect
  ACanvas.Brush.Color:=BackgroundColor;
  ACanvas.Brush.Style:=bsSolid;
  ACanvas.FillRect(BoundsRect);
//  ACanvas.FillRect(0,0,ACanvas.Width,ACanvas.Height);
  DrawHAxis(ACanvas,FXAxis,FYAxis);
  DrawVAxis(ACanvas,FYAxis,FXAxis);
end;

procedure TCanvasPlotter.DrawHAxis(ACanvas: TCanvas; Const AHAxis,AVAxis : TPlotAxis);

Var
  OX,OY,EX,EY : Integer;
  CX,CY,CW : Integer;
  I,X,TE : integer;
  TickDelta : Double;
  V,VD : TPlotFloat;
  S,L : String;

begin
  OX:=FBoundsRect.Left+AHAxis.Margin1;
  EX:=FBoundsRect.Right-AHAxis.Margin2;
  OY:=FBoundsRect.Bottom-AVAxis.Margin1;
  EY:=FBoundsRect.Top+AVAxis.Margin2;
//  Writeln(Format('(%d,%d) -> (%d,%d) (%d,%d) (%d,%d)',[width,height,ox,oy,ox,ey,ex,oy]));
  // X axis
  ACanvas.Pen.Color:=AHAxis.Color;
  ACanvas.Line(OX,OY,EX,OY);
  Canvas.Font:=AHAxis.TickFont;
  TickDelta:=AHAxis.TickDelta;
  VD:=AHAxis.ValueDelta;
  TE:=OY+AHAxis.TickSize;
  I:=0;
  V:=AHAxis.Origin;
  L:=AHAxis.LegendFormat;
  If (L='') then
    L:='#0.#';
  Repeat
    ACanvas.Pen.Color:=AHAxis.TickColor;
    X:=OX+Round(I*TickDelta);
    ACanvas.Line(X,OY,X,TE);
    If (AHAxis.GridInterval<>0) and ((I mod AHAxis.GridInterval)=0) then
      begin
      ACanvas.Pen.Color:=AHAxis.GridColor;
      ACanvas.Line(X,OY,X,EY);
      end;
    If (AHAxis.LegendInterval<>0) and ((I mod AHAxis.LegendInterval)=0) then
      begin
      S:=FormatFloat(L,V);
      CW:=Canvas.TextWidth(S);
      Canvas.TextOut(X-(CW Div 2),TE+4,S);
      end;
    Inc(I);
    V:=V+VD;
  Until X>=EX;
  if AHAxis.DrawZero and ((AHAxis.Origin<0) and ((AHAxis.Origin+AHAxis.Interval)>0)) then
    begin
    X:=OX+Round((EX-OX)*Abs(AHAxis.Origin)/AHAxis.Interval);
    ACanvas.Pen.Color:=AHAxis.TickColor;
    ACanvas.Line(X,OY,X,EY);
    end;
  Canvas.Font:=AHAxis.Caption.Font;
  CW:=ACanvas.TextWidth(AHAxis.Caption.Title);
  Case AHAxis.Caption.Alignment of
    taLeftJustify  : CX:=OX;
    taRightJustify : CX:=EX-CW;
    taCenter       : CX:=(EX+OX-CW) div 2;
  end;
//  Writeln(Format('Caption at (%d,%d) : %s',[CX,CY,AHAxis.Caption]));
  CY:=OY+AHAxis.TickSize+4+ACanvas.TextHeight('X')+4;
  ACanvas.TextOut(CX,CY,AHAxis.Caption.Title);
end;

procedure TCanvasPlotter.DrawVAxis(ACanvas: TCanvas; Const AVAxis,AHAxis : TPlotAxis);

Var
  OX,OY,EX,EY : Integer;
  CY,CH,CW : Integer;
  I,Y,TE : integer;
  TickDelta : Double;
  V,VD : TPlotFloat;
  S,L : String;
  // Vertical font support
  OldFont, RotatedFont: HFONT;
  ALogFont : TLogFont;
  // GTK 1
  BMP : TBitmap;

begin
//  ACanvas.DrawText(FXAxis.Caption);
  // Y axis
  OX:=FBoundsRect.Left+AHAxis.Margin1;
  EX:=FBoundsRect.Right-AHAxis.Margin2;
  OY:=FBoundsRect.Bottom-AVAxis.Margin1;
  EY:=FBoundsRect.Top+AVAxis.Margin2;
  ACanvas.Pen.Color:=AVAxis.Color;
  ACanvas.Line(OX,OY,OX,EY);
  TickDelta:=AVAxis.TickDelta;
  VD:=AVAxis.ValueDelta;
  TE:=OX-AVAxis.TickSize;
  V:=AVAxis.Origin;
  L:=AVAxis.LegendFormat;
  Canvas.Font:=AVAxis.TickFont;
  If (L='') then
    L:='#0.#';
  I:=0;
  CH:=Canvas.TextHeight('X') div 2;
  Repeat
    Y:=OY-Round(I*TickDelta);
    ACanvas.Pen.Color:=AVAxis.TickColor;
    ACanvas.Line(TE,Y,OX,Y);
    If (Y<>OY) and (AVAxis.GridInterval<>0) and ((I mod AVAxis.GridInterval)=0) then
      begin
      ACanvas.Pen.Color:=AVAxis.GridColor;
      ACanvas.Line(OX,Y,EX,Y);
      end;
    If (AVAxis.LegendInterval<>0) and ((I mod AVAxis.LegendInterval)=0) then
      begin
      S:=FormatFloat(L,V);
      CW:=Canvas.TextWidth(S);
      Canvas.TextOut(TE-CW-4,Y-CH,S);
      end;
    Inc(I);
    V:=V+VD;
  Until Y<=EY;
  if AVAxis.DrawZero and ((AVAxis.Origin<0) and ((AVAxis.Origin+AVAxis.Interval)>0)) then
    begin
    Y:=OY-Round((OY-EY)*Abs(AVAxis.Origin)/AVAxis.Interval);
    ACanvas.Pen.Color:=AVAxis.TickColor;
    ACanvas.Line(OX,Y,EX,Y);
    end;
  L:=AVAxis.Caption.Title;
  ACanvas.Font:=AVAxis.Caption.Font;
  CH:=ACanvas.TextHeight(L);
  If CompareText(WidgetSet.ClassName,'TGTK1WidgetSet')<>0 then
    begin
    // Use Vertical font
    OldFont := 0;
    if GetObject(ACanvas.Font.Reference.Handle, SizeOf(ALogFont), @ALogFont) <> 0 then
      begin
      ALogFont.lfEscapement := 900;
      RotatedFont := CreateFontIndirect(ALogFont);
      if RotatedFont <> 0 then
        OldFont:=SelectObject(ACanvas.Handle, RotatedFont);
      CW:=ACanvas.TextWidth(L);
      Case AVAxis.Caption.Alignment of
        taLeftJustify  : CY:=OY;
        taRightJustify : CY:=EY-CW;
        taCenter       : CY:=(EY+OY-CW) div 2;
      end;
      TextOut(ACanvas.Handle, 4, CY, PChar(L), Length(L));
      if OldFont <> 0 then
        DeleteObject(SelectObject(ACanvas.Handle, OldFont));
      end;
    end
  else
    begin
    BMP:=GetRotatedLegend(ACanvas);
    CW:=BMP.Height;
    Case AVAxis.Caption.Alignment of
      taLeftJustify  : CY:=OY-CW;
      taRightJustify : CY:=EY;
      taCenter       : CY:=(EY+OY-CW) div 2;
    end;
    Canvas.Draw(4,CY,Bmp);
    end;
end;

Function TCanvasPlotter.GetRotatedLegend(ACanvas : TCanvas) : TBitmap;
{
  This is an expensive operation, so we do it once and cache the result.
  When the parameters (font,legend) change, we regenerate the bitmap
}
Var
  CW,CH,I,J : Integer;
  BMP : TBitmap;
  SrcIntfImg, DestIntfImg: TLazIntfImage;
  L : String;
  ImgHandle,ImgMaskHandle: HBitmap;

begin
  If (FBitmap=Nil) then
    begin
    FBitmap:=TBitmap.Create;
    FLastFont:=TFont.Create;
    end
  else
    begin
    If (FLastLegend=FYAxis.Caption.Title) and
       (FLastFont.Name=FYaxis.Caption.Font.name) and
       (FLastFont.Style=FYaxis.Caption.Font.Style) and
       (FLastFont.Size=FYaxis.Caption.Font.Size) then
      // NOthing changed, return last bitmap.
      Exit(FBitmap);
    FBitmap.Clear;
    end;
  L:=FYAxis.Caption.Title;
  ACanvas.Font:=FYAxis.Caption.Font;
  FLastLegend:=L;
  FLastFont.Assign(FYAxis.Caption.Font);
  BMP:=TBitmap.Create;
  try
    BMP.Canvas.Font:=FYAxis.Caption.Font;
    CH:=BMP.Canvas.TextHeight(L);
    CW:=BMP.Canvas.TextWidth(L);
    BMP.Width:=CW;
    BMP.Height:=CH;
    BMP.Canvas.Brush.Color:=Self.BackgroundColor;
    BMP.Canvas.Brush.Style:=bsSolid;
    BMP.Canvas.FillRect(0,0,CW,CH);
    BMP.Canvas.TextOut(0,0,L);
    SrcIntfImg:=TLazIntfImage.Create(0,0);
    try
      SrcIntfImg.LoadFromBitmap(BMP.Handle,BMP.MaskHandle);
      DestIntfImg:=TLazIntfImage.Create(0,0);
      try
        DestIntfImg.LoadFromBitmap(BMP.Handle,BMP.MaskHandle);
        DestIntfImg.Width:=CH;
        DestIntfImg.Height:=CW;
        For I:=0 to CW-1 do
          For J:=0 to CH-1 do
           DestIntfImg.Colors[J,CW-1-I]:=SrcIntfImg.Colors[I,J];
        DestIntfImg.CreateBitmaps(ImgHandle,ImgMaskHandle,True);
        FBitmap.Handle:=ImgHandle;
        FBitmap.MaskHandle:=ImgMaskHandle;
        Result:=FBitmap;
      finally
        DestIntfImg.Free;
      end;
    finally
      SrcIntfImg.Free;
    end;
  finally
    BMP.Free;
  end;
end;


procedure TCanvasPlotter.PlotFunction(ACanvas: TCanvas);

Var
  POX,PX,PXW,PY,POY,PEY,PYH,PLX,PLY : Integer; // Pixel units
  X,Y,XI,YI,YO : TPlotFloat;         // Plot units


begin
  // X-origin in pixels.
  POX:=FXAxis.LeftMargin;
  // Width in pixels
  PXW:=ACanvas.Width-FXAxis.RightMargin-POX+1;
  // Y origin in pixels
  POY:=ACanvas.Height-FYAxis.BottomMargin;
  // Height in pixels
  PYH:=POY-FYAxis.TopMargin+1;
  // Y top
  YI:=PYH/FYAxis.Interval;
  // Interval in plot units
  XI:=FXAxis.Interval/PXW;
  // Y plot Origin
  YO:=FYAxis.Origin;
  // Y plot max value
  PEY:=FYAxis.TopMargin;
  // Y interval
  YI:=PYH/FYAxis.Interval;
  // Start value
  X:=FXAxis.Origin;
  ACanvas.Pen.Color:=PlotColor;
  PLX:=POX;
  PLY:=POY;
  For PX:=0 to PXW do
    begin
    try
      Y:=CalcFunction(X);
      PY:=POY-Trunc((Y-YO)*YI);
    except
      // Catch math calculation exceptions.
      On E : EMathError do
        begin
        PY:=PEY+1;
        end;
      On E : EIntError do
        begin
        PY:=PEY+1;
        end;
      On E : Exception do
        Raise;
    end;
    If (PX>0) and (PY>=PEY) and (PY<=POY) then
      begin
  //    Writeln(Format('(%f,%f) -> (%d,%d)',[X,Y,PX+Pox,PY]));
//      ACanvas.Pixels[PX+Pox,PY]:=PlotColor;
      ACanvas.Line(Pox+PLX,PLY,POX+PX,PY);
      end;
    PLX:=PX;
    PLY:=PY;
    X:=X+XI;

    end;
end;

function TCanvasPlotter.GetHDimension: Integer;
begin
  Result:=FBoundsRect.Right-FBoundsRect.Left+1;
end;

function TCanvasPlotter.GetVDimension: Integer;
begin
  Result:=FBoundsRect.Bottom-FBoundsRect.Top+1;
end;

{ TControlPlotter }

procedure TControlPlotter.Changed;
begin
//  If Not (FControl.ComponentState in [csLoading]) then;
    FControl.Invalidate;
end;

constructor TControlPlotter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  If AOwner is TCOntrol then
    begin
    FControl:=AOwner as TControl;
    FBackGroundColor:=FControl.Color;
    end;
end;

{ TEventControlPlotter }


procedure TEventControlPlotter.SetOnCalcPlot(const AValue: TOnCalcPlotEvent);
begin
  FOnCalcPlot:=AValue;
  Changed;
end;

function TEventControlPlotter.CalcFunction(X: TPlotFloat): TPlotFloat;
begin
  If Assigned(FOnCalcPlot) then
    FOnCalcPlot(X,Result)
  else
    Result:=inherited CalcFunction(X);
end;


{ TPlotFunctionPanel }

function TPlotFunctionPanel.GetOnCalcPlot: TOnCalcPlotEvent;
begin
  If Assigned(FPlotter) then
    Result:=TEventControlPlotter(FPlotter).OnCalcPlot
  else
    Result:=Nil;
end;

procedure TPlotFunctionPanel.SetOnCalcPlot(AValue: TOnCalcPlotEvent);
begin
  If Assigned(FPlotter) then
    TEventControlPlotter(FPlotter).OnCalcPlot:=Avalue
end;

function TPlotFunctionPanel.CreatePlotter: TCanvasPlotter;
begin
  Result:=TEventControlPlotter.Create(Self);
end;

{ TPlotCaption }

procedure TPlotCaption.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  Changed;
end;

procedure TPlotCaption.SetFont(const AValue: TFont);
begin
  if FFont=AValue then exit;
  FFont:=AValue;
  Changed;
end;

procedure TPlotCaption.SetTitle(const AValue: String);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
  Changed;
end;

procedure TPlotCaption.Changed;
begin
  If Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TPlotCaption.Create;
begin
  FFont:=TFont.Create;
end;

destructor TPlotCaption.Destroy;
begin
  FreeAndNil(FFont);
  inherited Destroy;
end;

end.

