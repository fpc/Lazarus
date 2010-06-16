{  $Id$  }
{
 /***************************************************************************
                               chart.pp
                               --------
                 Component Library Extended Controls

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  This unit is deprecated, because there is something better:
  package TAChartLazarusPkg.

  Author: Michael Van Canneyt
}
unit Chart;

{$MODE ObjFPC}{$H+}

interface

uses
  Types, SysUtils, Classes, LCLProc, LCLIntf, LCLType, Controls, ExtCtrls, Graphics,
  Dialogs;

type

  TPosLabel=(plLeft, plCenter, plRight);
  TCustomBarChart = class;
  { TBar }

  TBar = class(TCollectionItem)
  private
    FColor: TColor;
    FSName: String;
    FValue: integer;
    procedure SetColor(const AValue: TColor);
    procedure SetSName(const AValue: String);
    procedure SetValue(const AValue: integer);
    procedure UpdateBarChart;
  protected
    function GetDisplayName: string; override;
  published
    property SName: String read FSName write SetSName;
    property Value: integer read FValue write SetValue;
    property Color: TColor read FColor write SetColor;
  end;

  { TBarChartItems }

  TBarChartItems = class(TCollection)
  private
    FBarChart: TCustomBarChart;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(BarChart: TCustomBarChart);
  end;
  
  { TCustomBarChart }

  TCustomBarChart = class(TPanel)
  private
    FUpdateCount: Integer;
    FBars: TCollection;
    FDepth: byte;
    FLabelPosition:TPosLabel;
    FIsPainting: Boolean;
    function GetBars: TCollection;
    function NormalizeScaleUnits(OldScale: Integer): Integer;
    procedure SetBars(const AValue: TCollection);
    procedure SetDepth(const AValue: byte);
    procedure SetLabelPosition(const AValue: TPosLabel);
  protected
    procedure Paint; override;
    class function GetControlClassDefaultSize: TSize; override;
    function RealGetText: TCaption; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function AddBar(const SName: string; Value: integer; AColor: TColor): TBar;
    function GetBar(SId: integer): TBar;
    function BarCount: Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateBarChart;
  published
    property Bars: TCollection read GetBars write SetBars;
    property Depth: byte read FDepth write SetDepth;
    property LabelPosition: TPosLabel read FLabelPosition write SetLabelPosition;
  end;
  
  
  { TBarChart
    Deprecated!
    Use package TAChartLazarusPkg instead. It has a compatible and better component.}
  
  TBarChart = class(TCustomBarChart)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property FullRepaint;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end deprecated; // use package TAChartLazarusPkg instead. It has a compatible and better component.


procedure Register;

implementation

procedure Register;
begin
  {$WARNINGS off}
  RegisterComponents('Misc',[TBarChart]);
  {$WARNINGS on}
end;

constructor TCustomBarChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBars:=TBarChartItems.Create(Self);
  FDepth:=5;
  FLabelPosition:=plLeft;
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
end;

destructor TCustomBarChart.Destroy;
begin
  FBars.Destroy;
  inherited Destroy;
end;

function TCustomBarChart.AddBar(const SName: string; Value: Integer;
  AColor: TColor): TBar;
begin
  BeginUpdate;
  Try
    result:=TBar(FBars.Add);
    result.FsName:=SName;
    result.FValue:=Value;
    result.FColor:=AColor;
  finally
    EndUpdate;
  end;
end;

function TCustomBarChart.GetBar(SId: integer): TBar;
begin
  result:=TBar(FBars.FindItemID(SId));
end;

function TCustomBarChart.NormalizeScaleUnits(OldScale: Integer): Integer;

Var
  T: Integer;

begin
  Result:=OldScale;
  if Result<2 then
    Result:=2
  else if Result<=5 then
    Result:=5
  else if Result<=10 then
    Result:=10
  else
    begin
    T:=StrToInt(IntToStr(Result)[1])+1;
    repeat
      Result:=Result div 10;
      T:=T*10;
    until Result<10;
    Result:=T;
    end;
end;

function TCustomBarChart.GetBars: TCollection;
begin
  Result:=FBars;
end;

procedure TCustomBarChart.SetBars(const AValue: TCollection);
begin
  FBars.Assign(AValue);
end;

procedure TCustomBarChart.SetDepth(const AValue: byte);
begin
  if FDepth=AValue then exit;
  FDepth:=AValue;
  UpdateBarChart;
end;

procedure TCustomBarChart.SetLabelPosition(const AValue: TPosLabel);
begin
  if FLabelPosition=AValue then exit;
  FLabelPosition:=AValue;
  UpdateBarChart;
end;

procedure TCustomBarChart.Paint;

var
  i,k,j,h,w,h1,HMax,VMax: integer;
  bx,by:integer;
  NScaleLines : Integer;
  ScaleUnits  : Integer;
  PixelPerUnit: Double;
  BC          : Double;
  RBC         : Integer;
  BL   : Integer;
  m,z: integer;
  ts : TBar;
  s  : string;
  rc : TRect;

  procedure ScaleLine(dk: integer; const s: string);

  begin
    Canvas.MoveTo(hmax+dk+FDepth,h1);
    Canvas.LineTo(hmax+dk+FDepth,h1+h);
    Canvas.LineTo(hmax+dk,h1+FDepth+h);
    Canvas.LineTo(hmax+dk,h1+FDepth+h+2);
    Canvas.TextOut(HMax+dk-j,m,s);
  end;

begin
  FIsPainting := true;
  try
    inherited Paint;
  finally
    FIsPainting := false;
  end;
  bx:=GetSystemMetrics(SM_CXEDGE);
  by:=GetSystemMetrics(SM_CYEDGE);
  hmax:=10;
  vmax:=0;
  for i:=0 to FBars.Count-1 do
    begin
    ts:=TBar(FBars.Items[i]);
    k:=Canvas.TextWidth(ts.FsName);
    if k>hmax then
      Hmax:=k;
    if ts.FValue>vmax then
      vmax:=ts.FValue;
    end;
  HMax:=HMax+10;
  h1:=RoundToInt(1.5*Canvas.TextHeight('W'));
  h:=Height-2*h1-Fdepth;
  w:=Width-hmax-2*FDepth;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Width:=1;
  Canvas.Pen.Style:=psSolid;
  Canvas.Brush.Color:=clYellow;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Polygon([Point(HMax,h1+FDepth),Point(HMax,h1+FDepth+h),Point(HMax+FDepth,h1+h),Point(HMax+FDepth,h1)]);
  Canvas.Brush.Color:=clWhite;
  Canvas.Polygon([Point(HMax,h1+FDepth+h),Point(HMax+w,h1+FDepth+h),Point(HMax+w+FDepth,h1+h),Point(HMax+FDepth,h1+h)]);
  Canvas.Brush.Color:=Color;
  Canvas.Rectangle(hmax+Fdepth,h1,hmax+w+FDepth,h1+h+1);
  Canvas.Pen.Width:=3;
  Canvas.MoveTo(hmax,h1+FDepth);
  Canvas.LineTo(hmax,h1+FDepth+h);
  Canvas.LineTo(hmax+w,h1+FDepth+h);
  Canvas.TextOut(bx,by,Caption);
  j:=Canvas.TextWidth(IntTostr(VMax));
  if VMax=0 then
     begin
     PixelPerUnit:=1;
     NscaleLines:=1;
     end
   else
     begin
     PixelPerUnit:=double(w-j-6) / VMax;
     NScaleLines:=(w-j-6) div (2*j);
     end;
  if NScaleLines=0 then
    ScaleUnits:=Vmax +1
  else
    ScaleUnits:=(Vmax div NScaleLines) +1;
  ScaleUnits:=NormalizeScaleUnits(ScaleUnits);
  if ScaleUnits=0 then
    NScaleLines:=1
  else
    NScaleLines:=VMax div ScaleUnits;
  Canvas.Pen.Color:=clGray;
  Canvas.Pen.Style:=psDot;
  Canvas.Pen.Width:=1;
  j:=j div 2;
  m:=h1+FDepth+h+2;
  if VMax=0 then
    begin
    k:=w div 2;
    ScaleLine(k,'0');
    end
  else
    Canvas.TextOut(HMax-j,m,'0');
  for k:=1 to NScaleLines do
    ScaleLine(RoundToInt(ScaleUnits*PixelPerUnit*k),IntToStr(k*ScaleUnits));
  If FBars.Count=0 then
    BC:=0
  else
    BC:=double(h) / (2*(FBars.Count+1));
  RBC:=RoundToInt(BC);
  z:=h1+FDepth+h;
  Canvas.Pen.Style:=psSolid;
  for i:=0 to FBars.Count-1 do
    begin
    ts:=TBar(FBars.Items[i]);
    z:=h1+FDepth+h-Round(2*(I+1)*BC);
    Canvas.Brush.Color:=ts.FColor;
    m:=ts.FValue;
    BL:=RoundToInt(m*PixelPerUnit);
    Canvas.Rectangle(hmax+1,z-1,hmax+BL+1,z+RBC-1);
    Canvas.Polygon([Point(hmax,z),Point(hmax+BL,z),Point(hmax+BL+FDepth,z-FDepth),Point(hmax+FDepth,z-FDepth)]);
    Canvas.Polygon([Point(hmax+BL,z),Point(hmax+BL,z+RBC-1),Point(hmax+BL+FDepth,z+RBC-1-FDepth),Point(hmax+BL+FDepth,z-FDepth)]);
    s:=IntToStr(m);
    w:=z+(RBC-FDepth) div 2;
    Canvas.MoveTo(Hmax+BL+Fdepth div 2,w);
    Canvas.LineTo(Hmax+BL+Fdepth+5-bx,w);
    Canvas.Brush.Color:=clYellow;
    with rc do
      begin
      left:=hmax+BL+FDepth+5-bx;
      right:=left+Canvas.TextWidth(s)+2*bx;
      top:=w-Canvas.TextHeight(s) div 2-by;
      bottom:=w+Canvas.TextHeight(s) div 2+by;
      end;
    Canvas.Rectangle(rc);
    //debugln('TCustomBarChart.Paint A ',dbgs(rc),' s="',s,'"');
    Canvas.TextOut(rc.Left+bx,rc.Top+by,s);
    Canvas.Font.Color:=Font.Color;
    case FLabelPosition of
      plLeft: Canvas.TextOut(bx,z,ts.FSName);
      plCenter: Canvas.TextOut(HMax+((BL-Canvas.TextWidth(ts.FSName)) div 2),z,ts.FSName);
      plRight: Canvas.TextOut(HMax+BL-Canvas.TextWidth(ts.FSName)-bx,z,ts.FSName);
    end;
    end;
  Canvas.Pen.Style:=psSolid;
end;

function TCustomBarChart.RealGetText: TCaption;
begin
  if FIsPainting then
    Result := ''
  else
    Result := inherited RealGetText;
end;

class function TCustomBarChart.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 150;
  Result.CY := 120;
end;

procedure TCustomBarChart.Clear;
begin
  FBars.Clear;
end;

procedure TCustomBarChart.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCustomBarChart.EndUpdate;
begin
  if FUpdateCount=0 then
    raise Exception.Create('TCustomBarChart.EndUpdate');
  Dec(FUpdateCount);
  If FUpdateCount=0 then
    Invalidate;
end;

procedure TCustomBarChart.UpdateBarChart;
begin
  if FUpdateCount = 0 then
    Invalidate;
end;

function TCustomBarChart.BarCount: Integer;
begin
  Result:=FBars.Count;
end;

{ TBar }

procedure TBar.SetColor(const AValue: TColor);
begin
  if FColor=AValue then exit;
  FColor:=AValue;
  UpdateBarChart;
end;

procedure TBar.SetSName(const AValue: String);
begin
  if FSName=AValue then exit;
  FSName:=AValue;
  UpdateBarChart;
end;

procedure TBar.SetValue(const AValue: integer);
begin
  if FValue=AValue then exit;
  FValue:=AValue;
  UpdateBarChart;
end;

procedure TBar.UpdateBarChart;
begin
  (Collection as TBarChartItems).FBarChart.UpdateBarChart;
end;

function TBar.GetDisplayName: string;
begin
  Result:=FSName;
end;

{ TBarChartItems }

function TBarChartItems.GetOwner: TPersistent;
begin
  Result := FBarChart;
end;

constructor TBarChartItems.Create(BarChart: TCustomBarChart);
begin
  inherited Create(TBar);
  FBarChart:=BarChart;
end;

end.
