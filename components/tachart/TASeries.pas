{
 /***************************************************************************
                               TASeries.pp
                               ----------
                Component Library Standard Graph Series


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit TASeries;


{$IFDEF fpc}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
 {$IFDEF fpc}
  {$ELSE}
  Windows,
  {$ENDIF}
  classes, graphics, tagraph, sysutils, dialogs;

Const clTAColor = clScrollBar;

type

  ChartCoord = record
    x, y: double;
    Color: tcolor;
    Text: string;
  end;
  PChartCoord = ^ChartCoord;

  TPointStyle=(psRectangle,psCircle,psCross,psDiagCross,psStar);
  //not completetly implemented (only TPieSeries - not all)
  TSeriesMarksStyle =( smsValue,          { 1234 }
                      smsPercent,        { 12 % }
                      smsLabel,          { Cars }
                      smsLabelPercent,   { Cars 12 % }
                      smsLabelValue,     { Cars 1234 }
                      smsLegend,         { ? }
                      smsPercentTotal,   { 12 % of 1234 }
                      smsLabelPercentTotal, { Cars 12 % of 1234 }
                      smsXValue);        { 21/6/1996 }


  BarException=class(Exception);

  TChartSeries = class(TComponent)
  private
    // Graph = coordinates in the graph
    FXGraphMin,FYGraphMin:Double;                // Max Graph value of points
    FXGraphMax,FYGraphMax:Double;
    FSeriesColor: TColor;
    FTitle: String;
    FCoordList: TList;
    FActive:Boolean;
    FMarks: TSeriesMarksStyle;

    procedure SetActive(Value:Boolean);
    procedure SetMarks(Value:TSeriesMarksStyle);
    function GetXMinVal: Integer;
  public
    Chart:TChart;
    procedure Draw; virtual; abstract;

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    property XGraphMin: Double read FXGraphMin  write FXGraphMin;
    property YGraphMin: Double read FYGraphMin  write FYGraphMin;
    property XGraphMax: Double read FXGraphMax  write FXGraphMax;
    property YGraphMax: Double read FYGraphMax  write FYGraphMax;
    property SeriesColor : TColor read FSeriesColor write FSeriesColor default clTAColor;
    property Title: String read FTitle write FTitle;

    function Count:Integer;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint; virtual;
    procedure Delete(Index:Integer); virtual;
    procedure Clear;
//    function AddY(X, Y: Double; XLabel: String; Color: TColor) : Longint;

    property Coord: TList read FCoordList;
  published
      property MarksStyle: TSeriesMarksStyle read FMarks write SetMarks; //this should be an object
      property Active:Boolean read FActive write SetActive;

  end;


{  TCustomSeries = class(TChartSeries);

  TTACustomBarSeries = class(TChartSeries)
  public

  end;
  }


  TLineStyle=(lsVertical,lsHorizontal);


  TBarSeries = class(TChartSeries)
  private
    FBarBrush: TBrush;
    FBarPen: TPen;

    FBarWidthPercent: Integer;

    Procedure SetBarWidthPercent(Value:Integer);
    procedure SetBarBrush(Value:TBrush);
    procedure SetBarPen(Value:TPen);
  protected
    procedure StyleChanged(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    {from parent}
    procedure Draw; override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint; override;
    //        function AddBar(Value: Double; Text: String; Color: TColor) : Longint;
  published
    property BarWidthPercent:Integer read FBarWidthPercent write SetBarWidthPercent default 70;
    property BarBrush: TBrush read FBarBrush write SetBarBrush;
    property BarPen: TPen read FBarPen write SetBarPen;
    property Title;
    property Active;
  end;

  TPieSeries = class(TChartSeries)
  private
      ColorIndex: Integer;

      FPiePen: TPen;
      procedure SetPiePen(Value:TPen);
  protected
    procedure StyleChanged(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    {from parent}
    procedure Draw; override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint; override;
    function AddPie(Value: Double; Text: String; Color: TColor) : Longint;
  published
    property PiePen: TPen read FPiePen write SetPiePen;
    property Title;
    property Active;
  end;


  TAreaSeries = class(TChartSeries)
  private
{      ColorIndex: Integer;

      FPiePen: TPen;
      procedure SetPiePen(Value:TPen);
      }
      FAreaLinesPen: TChartPen;
      FAreaBrush: TBrush;
      FStairs: boolean;
      FInvertedStairs: boolean;

      procedure SetAreaBrush(value: TBrush);
      procedure SetStairs(value: Boolean);
      procedure SetInvertedStairs(value: Boolean);
  protected
    procedure StyleChanged(Sender:TObject);
  public
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    {from parent}
    procedure Draw; override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint; override;
  published
     property AreaLinesPen : TChartPen read FAreaLinesPen write FAreaLinesPen;
     property AreaBrush : TBrush read FAreaBrush write SetAreaBrush;
     property Stairs: boolean read FStairs write setStairs;
     property InvertedStairs: boolean read FInvertedStairs  write setInvertedStairs;
     property Title;
     property Active;
  end;




  TSerie = class(TChartSeries)
  private
    FStyle:TPointStyle;

// Image = coordinates in the component


    XOfYGraphMin,XOfYGraphMax:Double;          // X max value of points
    FShowPoints:Boolean;
    FShowLines:Boolean;

    UpdateInProgress:Boolean;

    procedure SetShowPoints(Value:Boolean);
    procedure SetShowLines(Value:Boolean);
    procedure SetStyle(Value:TPointStyle);
  protected
    { Déclarations protégées }
  public

    { Déclarations publiques }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    procedure StyleChanged(Sender:TObject);
    procedure Draw; override;
    function AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint; override;
    function  GetXValue(Index:Integer):Double;
    function  GetYValue(Index:Integer):Double;
    procedure SetXValue(Index:Integer;Value:Double);
    procedure SetYValue(Index:Integer;Value:Double);
    function  GetXImgValue(Index:Integer):Integer;
    function  GetYImgValue(Index:Integer):Integer;
    procedure GetMin(var X,Y:Double);
    procedure GetMax(var X,Y:Double);
    function  GetXMin:Double;
    function  GetXMax:Double;
    function  GetYMin:Double;
    function  GetYMax:Double;
    procedure SetColor(Index:Integer;_Color:TColor);
    function  GetColor(Index:Integer):TColor;

    property PointStyle:TPointStyle read FStyle write SetStyle;
    property ShowPoints:Boolean read FShowPoints write SetShowPoints;
    property ShowLines:Boolean read FShowLines write SetShowLines default True;

    procedure BeginUpdate;
    procedure EndUpdate;

    property XGraphMin;
    property YGraphMin;
    property XGraphMax;
    property YGraphMax;


  published
    property Title;
    property Active;
  end;

  TLine = class(TChartSeries)
  private
    FStyle:TLineStyle;

    PosImage:Integer;                     // Image coordinates of line
    PosGraph:Double;                      // Graph coordinates of line

    FPen:TPen;

    procedure SetPos(Value:Double);
    procedure SetPen(Value:TPen);
    procedure SetStyle(Value:TLineStyle);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;

    procedure Draw; override;
    procedure StyleChanged(Sender:TObject);

    property  LineStyle:TLineStyle read FStyle write SetStyle;

  published
    property Pen:TPen read FPen write SetPen;
    property Position:Double read PosGraph write SetPos;
    property Active;
  end;






implementation


constructor TChartSeries.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);

     XGraphMin:=MaxDouble;
     YGraphMin:=MaxDouble;
     XGraphMax:=MinDouble;
     YGraphMax:=MinDouble;

     FActive := True;
     FMarks := smsLabel;
     FCoordList := TList.Create;
end;

destructor TChartSeries.Destroy;
var i: integer;
begin
     for i := 0 to FCoordList.Count - 1 do begin
         Dispose( PChartCoord(FCoordList.Items[i]) );
     end;
     FCoordList.Free;

     inherited Destroy;
end;

function TChartSeries.GetXMinVal : Integer;
begin
     if count > 0 then
        result := round(PChartCoord(FCoordList[FCoordList.Count-1])^.x)
     else result := 0;
end;

function TChartSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint;
var
   coordn: PChartCoord;
   i: integer;
begin
   //save the coord and color
   New(Coordn);
   Coordn^.x := X;
   Coordn^.y := Y;
   Coordn^.Color := Color;
   Coordn^.Text := XLabel;

   //add in order or at end
   i := 0;
   while (i < FCoordList.Count) and (PChartCoord(FCoordList.Items[i])^.x <= X) do inc(i);
   if i = FCoordList.Count then FCoordList.Add(Coordn)
   else  FCoordList.Insert(I+1, Coordn);

 //  FCoordList.Add(Coord);
   result := FCoordList.IndexOf( Coordn );
end;

procedure TChartSeries.Delete(Index:Integer);
begin
     Dispose( PChartCoord(FCoordList.Items[Index]) );
     FCoordList.Delete( Index );
     if Chart <> nil then Chart.Invalidate;
end;

procedure TChartSeries.Clear;
begin
     FCoordList.Clear;

     XGraphMin:=MaxDouble;
     YGraphMin:=MaxDouble;
     XGraphMax:=MinDouble;
     YGraphMax:=MinDouble;

     if Chart <> nil then Chart.Invalidate;
end;



function TChartSeries.Count:Integer;
begin
   Result := FCoordList.count;
end;

procedure TChartSeries.SetActive(Value:Boolean);
begin
     FActive:=Value;
     if Chart <> nil then Chart.Invalidate;
end;

procedure TChartSeries.SetMarks(Value:TSeriesMarksStyle);
begin
     FMarks := Value;
     if Chart <> nil then Chart.Invalidate;
end;




///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

constructor TSerie.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);

   PointStyle:=psCross;

   ShowPoints:=False;
   ShowLines:=True;

   UpdateInProgress:=False;
end;

destructor TSerie.Destroy;
begin
   inherited Destroy;
end;

procedure TSerie.StyleChanged(Sender:TObject);
begin
     if Chart <> nil then Chart.Invalidate;
end;

procedure TSerie.SetStyle(Value:TPointStyle);
begin
if FStyle<>Value then
   begin
   FStyle:=Value;
   if Chart<>nil then Chart.Invalidate;
   end;
end;

procedure TSerie.Draw;
const   Larg = 4;
var
   i,j:Integer;

   xi1,yi1,xi2,yi2:Integer;
   xg1,yg1,xg2,yg2:Double;
   Min,Max,a,b:Double;
   Inside1,Inside2:Boolean;
   Chart1:TChart;
   YLeft,YRight,XBottom,XTop:Double;
   XLine,YLine:array[1..2] of Integer;
   BLeft,BRight,BBottom,BTop:Boolean;
   XLeftI,YLeftI,XRightI,YRightI,XBottomI,YBottomI,XTopI,YTopI:Integer;
   Temp:Double;
   dx,dy,dxy,qx,rx,qy,ry,u1,u2,u3,u4:Double;
   OK:Boolean;
   XMin,XMax,Ymin,Ymax,TempI:Integer;
   graphCoord: PChartCoord;

   label Points;

   procedure drawPoint( xi1, yi1: integer; PointStyle: TPointStyle );
   begin
        Chart.Canvas.Brush.Style:=bsSolid;

            case PointStyle of
            psRectangle:
               begin
               Chart.Canvas.Rectangle(xi1-Larg,yi1-Larg,xi1+Larg+1,yi1+Larg+1);
               end;
            psCross:
               begin
               Chart.Canvas.MoveTo(xi1-Larg,yi1);
               Chart.Canvas.LineTo(xi1+Larg+1,yi1);
               Chart.Canvas.MoveTo(xi1,yi1-Larg);
               Chart.Canvas.LineTo(xi1,yi1+Larg+1);
               end;
            psDiagCross:
               begin
               Chart.Canvas.MoveTo(xi1-Larg,yi1-Larg);
               Chart.Canvas.LineTo(xi1+Larg+1,yi1+Larg+1);
               Chart.Canvas.MoveTo(xi1-Larg,yi1+Larg+1);
               Chart.Canvas.LineTo(xi1+Larg+1,yi1-Larg);
               end;
            psStar:
               begin
               Chart.Canvas.MoveTo(xi1-Larg,yi1);
               Chart.Canvas.LineTo(xi1+Larg+1,yi1);
               Chart.Canvas.MoveTo(xi1,yi1-Larg);
               Chart.Canvas.LineTo(xi1,yi1+Larg+1);

               Chart.Canvas.MoveTo(xi1-Larg,yi1-Larg);
               Chart.Canvas.LineTo(xi1+Larg+1,yi1+Larg+1);
               Chart.Canvas.MoveTo(xi1-Larg,yi1+Larg+1);
               Chart.Canvas.LineTo(xi1+Larg+1,yi1-Larg);
               end;
            psCircle:
               begin
               Chart.Canvas.Ellipse(xi1-Larg,yi1-Larg,xi1+Larg+1,yi1+Larg+1);
               end;
            end;
   end;
begin
   if count=0 then Exit;

   with Chart do begin
      XMin:=XImageMin;
      XMax:=XImageMax;
      YMin:=YImageMin;
      YMax:=YImageMax;
   end;

   if XMin>XMax then begin
      TempI:=XMin;
      XMin:=XMax;
      XMax:=TempI;
   end;
   if YMin>YMax then begin
      TempI:=YMin;
      YMin:=YMax;
      YMax:=TempI;
   end;

   with Chart do begin
      Canvas.Pen.Mode:=pmCopy;
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Width:=1;
   end;

   Min:=Chart.XGraphMin;
   Max:=Chart.XGraphMax;


   for i:=0 to count-2 do begin
      graphCoord := FCoordList.Items[i];
      xg1 := graphCoord^.x;
      yg1 := graphCoord^.y;
      Chart.GraphToImage(xg1, yg1, xi1, yi1);
      graphCoord := FCoordList.Items[i+1];
      xg2 := graphCoord^.x;
      yg2 := graphCoord^.y;
      Chart.GraphToImage(xg2, yg2, xi2, yi2);

      Chart.Canvas.Pen.Color:= graphCoord^.Color;

      if FShowLines then begin
         if (xg1>Chart.XGraphMin) and (xg2>Chart.XGraphMin) and (xg1<Chart.XGraphMax) and (xg2<Chart.XGraphMax) and
            (yg1>Chart.YGraphMin) and (yg2>Chart.YGraphMin) and (yg1<Chart.YGraphMax) and (yg2<Chart.YGraphMax) then
            begin
               Chart.Canvas.MoveTo(xi1,yi1);
               Chart.Canvas.LineTo(xi2,yi2);
               goto Points;
            end;

         if ((xg1<Chart.XGraphMin) and (xg2<Chart.XGraphMin)) or ((xg1>Chart.XGraphMax) and (xg2>Chart.XGraphMax)) or
            ((yg1<Chart.YGraphMin) and (yg2<Chart.YGraphMin)) or ((yg1>Chart.YGraphMax) and (yg2>Chart.YGraphMax)) then
            goto Points;

         if yg1>yg2 then begin
            Temp:=xg1; xg1:=xg2; xg2:=Temp;
            Temp:=yg1; yg1:=yg2; yg2:=Temp;
         end;

         if yg1=yg2 then begin
            if xg1>xg2 then begin
               Temp:=xg1; xg1:=xg2; xg2:=Temp;
               Temp:=yg1; yg1:=yg2; yg2:=Temp;
            end;
            if xg1<Chart.XGraphMin then xi1:=Chart.XImageMin;
            if xg2>Chart.XGraphMax then xi2:=Chart.XImageMax;
            Chart.Canvas.MoveTo(xi1,yi1);
            Chart.Canvas.LineTo(xi2,yi2);
            goto Points;
         end;

         if xg1=xg2 then begin
            if yg1<Chart.YGraphMin then yi1:=Chart.YImageMin;
            if yg2>Chart.YGraphMax then yi2:=Chart.YImageMax;
            Chart.Canvas.MoveTo(xi1,yi1);
            Chart.Canvas.LineTo(xi2,yi2);
            goto Points;
         end;

         dy:=yg1-yg2;
         dx:=xg1-xg2;
         dxy:=xg1*yg2-yg1*xg2;
         qx:=Chart.XGraphMin*dy;
         rx:=Chart.XGraphMax*dy;
         qy:=Chart.YGraphMin*dx;
         ry:=Chart.YGraphMax*dx;
         u1:=qx-qy+dxy;
         u2:=qx-ry+dxy;
         u3:=rx-ry+dxy;
         u4:=rx-qy+dxy;

         OK:=False;
         if u1*u2<0 then begin
            OK:=True;
            if xg1<Chart.XGraphMin then begin
               yg1:=(Chart.XGraphMin*dy+dxy)/dx;
               xg1:=Chart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2<Chart.XGraphMin then begin
               yg2:=(Chart.XGraphMin*dy+dxy)/dx;
               xg2:=Chart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u2*u3<0 then begin
            OK:=True;
            if yg2>Chart.YGraphMax then begin
               xg2:=(Chart.YGraphMax*dx-dxy)/dy;
               yg2:=Chart.YGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u3*u4<0 then begin
            OK:=True;
            if xg1>Chart.XGraphMax then begin
               yg1:=(Chart.XGraphMax*dy+dxy)/dx;
               xg1:=Chart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2>Chart.XGraphMax then begin
               yg2:=(Chart.XGraphMax*dy+dxy)/dx;
               xg2:=Chart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u4*u1<0 then begin
            OK:=True;
            if yg1<Chart.YGraphMin then begin
               xg1:=(Chart.YGraphMin*dx-dxy)/dy;
               yg1:=Chart.YGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if OK then begin
            Chart.XGraphToImage(xg1,xi1);
            Chart.YGraphToImage(yg1,yi1);
            Chart.XGraphToImage(xg2,xi2);
            Chart.YGraphToImage(yg2,yi2);

            Chart.Canvas.MoveTo(xi1,yi1);
            Chart.Canvas.LineTo(xi2,yi2);

         end;

      end;

      Points:
        if FShowPoints and (yi1>YMin) and (yi1<YMax)
           and (xi1>XMin) and (xi1<XMax) then begin
           Chart.Canvas.Pen.Color := clBlack;
           Chart.Canvas.Brush.Color:=graphCoord^.Color;
           drawPoint( xi1, yi1, PointStyle );
         end;

   end;

   // Draw last point
   graphCoord := FCoordList.items[ Count - 1 ];
   xg1 := graphCoord^.x;
   yg1 := graphCoord^.y;
   Chart.GraphToImage(xg1, yg1, xi1, yi1);


   if FShowPoints and (yi1>YMin) and
      (yi1<YMax) and (xi1>XMin) and (xi1<XMax) then begin
         Chart.Canvas.Pen.Color := clBlack;
         Chart.Canvas.Brush.Color:=graphCoord^.Color;
         drawPoint( xi1, yi1, PointStyle );
   end;
end;


function TSerie.AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint;
begin
     if Color = clTAColor then Color := SeriesColor;

     inherited AddXY(X, Y, XLabel, Color);

     // Update max
     if X>XGraphMax then XGraphMax:=X;
     if X<XGraphMin then XGraphMin:=X;
     if Y>YGraphMax then begin
        YGraphMax:=Y;
        XOfYGraphMax:=X;
     end;
     if Y<YGraphMin then begin
        YGraphMin:=Y;
        XOfYGraphMin:=X;
     end;

     if Chart <> nil then Chart.Invalidate;
end;

function TSerie.GetXValue(Index:Integer):Double;
begin
     Result:=PChartCoord(FCoordList.Items[Index])^.x;
end;

function TSerie.GetYValue(Index:Integer):Double;
begin
     Result:=PChartCoord(FCoordList.Items[Index])^.y;
end;

procedure TSerie.SetXValue(Index:Integer;Value:Double);
var
   i:Integer;
   Val:Double;
begin
if not(UpdateInProgress) then begin
   if Value<XGraphMin then XGraphMin:=Value
   else if Value>XGraphMax then XGraphMax:=Value
   else begin
     if PChartCoord(FCoordList.Items[Index])^.x=XGraphMax then begin
        PChartCoord(FCoordList.Items[Index])^.x:=Value;
        if Value<XGraphMax then begin
           XGraphMax:=MinDouble;
           for i:=0 to FCoordList.Count-1 do begin
               Val:=PChartCoord(FCoordList.Items[Index])^.x;
               if Val>XGraphMax then XGraphMax:=Val;
           end;
        end;
     end
     else if PChartCoord(FCoordList.Items[Index])^.x=XGraphMin then begin
       PChartCoord(FCoordList.Items[Index])^.x:=Value;
       if Value>XGraphMin then begin
          XGraphMin:=MaxDouble;
          for i:=0 to FCoordList.Count-1 do begin
              Val:=PChartCoord(FCoordList.Items[Index])^.x;
              if Val<XGraphMin then XGraphMin:=Val;
          end;
       end;
     end;
   end;
end;

PChartCoord(FCoordList.Items[Index])^.x:=Value;

if Chart <> nil then Chart.Invalidate;
end;

procedure TSerie.SetYValue(Index:Integer;Value:Double);
var
   i:Integer;
   Val:Double;
begin
if not(UpdateInProgress) then begin
   if Value<YGraphMin then YGraphMin:=Value
   else if Value>YGraphMax then YGraphMax:=Value
   else begin
      if PChartCoord(FCoordList.Items[Index])^.y=YGraphMax then begin
         PChartCoord(FCoordList.Items[Index])^.y:=Value;
         if Value<YGraphMax then begin
            YGraphMax:=MinDouble;
            for i:=0 to FCoordList.Count-1 do begin
               Val:=PChartCoord(FCoordList.Items[Index])^.y;
               if Val>YGraphMax then YGraphMax:=Val;
            end;
         end;
      end
      else if PChartCoord(FCoordList.Items[Index])^.y=YGraphMin then begin
         PChartCoord(FCoordList.Items[Index])^.y:=Value;
         if Value>YGraphMin then begin
            YGraphMin:=MaxDouble;
            for i:=0 to FCoordList.Count-1 do begin
               Val:=PChartCoord(FCoordList.Items[Index])^.y;
               if Val<YGraphMin then YGraphMin:=Val;
            end;
         end;
      end;
   end;
end;

PChartCoord(FCoordList.Items[Index])^.y := Value;

if Chart <> nil then Chart.Invalidate;
end;

function TSerie.GetXImgValue(Index:Integer):Integer;
begin
     Chart.XGraphToImage( PChartCoord(FCoordList.Items[Index])^.x, Result );
end;

function TSerie.GetYImgValue(Index:Integer):Integer;
begin
     Chart.YGraphToImage( PChartCoord(FCoordList.Items[Index])^.y, Result );
end;


function TSerie.GetXMin:Double;
begin
     Result:=XGraphMin;
end;

function TSerie.GetXMax:Double;
begin
     Result:=XGraphMax;
end;

function TSerie.GetYMin:Double;
begin
     Result:=YGraphMin;
end;

function TSerie.GetYMax:Double;
begin
     Result:=YGraphMax;
end;

procedure TSerie.GetMax(var X,Y:Double);
begin
     X:=XOfYGraphMax;
     Y:=YGraphMax;
end;

procedure TSerie.GetMin(var X,Y:Double);
begin
     X:=XOfYGraphMin;
     Y:=YGraphMin;
end;

procedure TSerie.SetColor(Index:Integer;_Color:TColor);
begin
     PChartCoord(FCoordList.items[Index])^.color := _Color;
end;

function TSerie.GetColor(Index:Integer):TColor;
begin
     result := PChartCoord(FCoordList.items[Index])^.color;
end;

procedure TSerie.SetShowPoints(Value:Boolean);
begin
     FShowPoints:=Value;
     if Chart<>nil then Chart.Invalidate;
end;

procedure TSerie.SetShowLines(Value:Boolean);
begin
     FShowLines:=Value;
     if Chart<>nil then Chart.Invalidate;
end;

procedure TSerie.BeginUpdate;
begin
     UpdateInProgress:=True;
end;

procedure TSerie.EndUpdate;
var
   i:Integer;
   Val:Double;
begin
     UpdateInProgress:=False;

     XGraphMax:=MinDouble;
     XGraphMin:=MaxDouble;
     for i:=0 to Count-1 do begin
         Val:= PChartCoord(FCoordList.Items[i])^.x;
         if Val>XGraphMax then XGraphMax:=Val;
         if Val<XGraphMin then XGraphMin:=Val;
     end;

     YGraphMax:=MinDouble;
     YGraphMin:=MaxDouble;
     for i:=0 to Count-1 do begin
         Val:= PChartCoord(FCoordList.Items[i])^.y;
         if Val>YGraphMax then YGraphMax:=Val;
         if Val<YGraphMin then YGraphMin:=Val;
     end;

     if Chart <> nil then Chart.Invalidate;
end;






constructor TLine.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);

     FPen:=TPen.Create;
     FPen.OnChange:=StyleChanged;


     LineStyle:=lsHorizontal;
end;

destructor TLine.Destroy;
begin
   inherited Destroy;
   FPen.Free;
end;

procedure TLine.StyleChanged(Sender:TObject);
begin
     if Chart <> nil then Chart.Invalidate;
end;

procedure TLine.SetPen(Value:TPen);
begin
FPen.Assign(Value);
end;

procedure TLine.SetStyle(Value:TLineStyle);
begin
     if FStyle<>Value then begin
        FStyle:=Value;
        case LineStyle of
           lsHorizontal: begin YGraphMin := PosGraph; YGraphMax := PosGraph; end;
           lsVertical:  begin XGraphMin := PosGraph; XGraphMax := PosGraph; end;
        end;
        if Chart<>nil then Chart.Invalidate;
     end;
end;

procedure TLine.SetPos(Value:Double);
begin
     PosGraph:=Value;

     //FIXME: not the best way of doing this
         {if Visible then begin
               NBPointsMax:=NBPointsMax+1;
               case LineStyle of
                  lsHorizontal:
                     begin
                     if Position<YMinSeries then YMinSeries:=Position;
                     if Position>YMaxSeries then YMaxSeries:=Position;
                     end;
                  lsVertical:
                     begin
                     if Position<XMinSeries then XMinSeries:=Position;
                     if Position>XMaxSeries then XMaxSeries:=Position;
                     end;
                  end;
               end;
            end;}
     case LineStyle of
        lsHorizontal: begin YGraphMin := PosGraph; YGraphMax := PosGraph; end;
        lsVertical:  begin XGraphMin := PosGraph; XGraphMax := PosGraph; end;
      end;

     if Chart<>nil then Chart.Invalidate;
end;

procedure TLine.Draw;
var
   XMin,XMax,Ymin,Ymax,TempI:Integer;
   label Points;
begin

with Chart as TChart do
   begin
   XMin:=XImageMin;
   XMax:=XImageMax;
   YMin:=YImageMin;
   YMax:=YImageMax;
   end;

if XMin>XMax then
   begin
   TempI:=XMin;
   XMin:=XMax;
   XMax:=TempI;
   end;
if YMin>YMax then
   begin
   TempI:=YMin;
   YMin:=YMax;
   YMax:=TempI;
   end;

// Draw
Chart.Canvas.Pen.Assign(FPen);

case LineStyle of
   lsHorizontal:
      if (PosGraph < Chart.XGraphMax) and (PosGraph > Chart.XGraphMin) then begin
         Chart.YGraphToImage(PosGraph,PosImage);
         Chart.Canvas.MoveTo(XMin,PosImage);
         Chart.Canvas.LineTo(XMax,PosImage);
      end;
   lsVertical:
      if (PosGraph < Chart.YGraphMax) and (PosGraph > Chart.YGraphMin) then begin
         Chart.XGraphToImage(PosGraph,PosImage);
         Chart.Canvas.MoveTo(PosImage,YMin);
         Chart.Canvas.LineTo(PosImage,YMax);
      end;
end;

end;

















constructor TBarSeries.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);
     FBarWidthPercent := 70; //70%

     FBarBrush := TBrush.Create;
     FBarBrush.OnChange := StyleChanged;

     FBarPen := TPen.Create;
     FBarPen.OnChange := StyleChanged;
     FBarPen.Mode:=pmCopy;
     FBarPen.Style:=psSolid;
     FBarPen.Width:=1;
     FBarPen.Color := clBlack;
     FBarBrush.Color := clRed;
end;

destructor TBarSeries.Destroy;
var i:integer;
begin
   FBarPen.Free;
   FBarBrush.Free;
   inherited Destroy;
end;

procedure TBarSeries.StyleChanged(Sender:TObject);
begin
     if Chart<>nil then Chart.Invalidate;
end;

procedure TBarSeries.SetBarBrush(Value:TBrush);
begin
     FSeriesColor := Value.Color;
     FBarBrush.Assign(Value);
end;

procedure TBarSeries.SetBarPen(Value:TPen);
begin
     FBarPen.Assign(Value);
end;

Procedure TBarSeries.SetBarWidthPercent(Value:Integer);
Begin
  if (Value<1) or (Value>100) then
     Raise BarException.Create('Wrong BarWidth Percent')
  else FBarWidthPercent := Value;
end;

function TBarSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint;
begin
   if Color = clTAColor then Color := SeriesColor;

   inherited AddXY(X, Y, XLabel, Color);

   //update the interval - the 0.6 is a hack to allow the bars to have some space appart
   if X > XGraphMax-0.6 then XGraphMax := X+0.6;
   if X < XGraphMin+0.6 then XGraphMin := X-0.6;
   //check if the bar is abouve 0 or not
   if Y >= 0 then begin
      if Y > YGraphMax then YGraphMax := Y;
      if YGraphMin > 0 then YGraphMin := 0;
   end else begin
       if Y < YGraphMin then YGraphMin := Y;
       if YGraphMax < 0 then YGraphMax := 0;
   end;


   if Chart <> nil then Chart.Invalidate;
   //this has to change
   result := 0;
end;

procedure TBarSeries.Draw;
var
   XMin,XMax,TempI:Integer;
   i: Integer;
   graphCoordTop: ChartCoord;
   graphCoordBottom: ChartCoord;
   topX, topY, bottomY: Integer;
   barWidth: Integer;

   bx1, by1, bx2, by2: integer;


   function BarInViewPort( cTop, cBottom: ChartCoord  ): boolean;
   begin //FIXME make cleaner?
        result :=  ( (cTop.x >= Chart.XGraphMin) and (cTop.x <= Chart.XGraphMax) )
                    and ( ( (cTop.y > Chart.YGraphMax) and (cBottom.y < Chart.YGraphMin))
                          or ( (cTop.y < Chart.YGraphMax) and (cTop.y > Chart.YGraphMin))
                          or ( (cBottom.y < Chart.YGraphMax) and (cBottom.y > Chart.YGraphMin))
                         );
   end;

begin
   //no elements to draw
   if FCoordList.Count = 0 then exit;

   //get the limits (for the zoom) ??
   XMin := Chart.XImageMin;
   XMax := Chart.XImageMax;
   if XMin > XMax then begin
      TempI:=XMin;
      XMin:=XMax;
      XMax:=TempI;
   end;

   // Draw the bars
   Chart.Canvas.Pen.Assign( FBarPen );
   Chart.Canvas.Brush.Assign( FBarBrush );

   //calc the single bar width
   barWidth:=Round((FBarWidthPercent*0.01)*Chart.ChartWidth/FCoordList.Count);
   // barWidth:=barWidth div NumBarSeries; //to use with multibar


   for i := 0 to FCoordList.Count - 1 do begin
       //get the top and bottom points
       if ChartCoord(FCoordList.Items[i]^).y >= 0 then begin
          graphCoordTop := ChartCoord(FCoordList.Items[i]^);
          graphCoordBottom.x := graphCoordTop.x;
          graphCoordBottom.y := 0;
       end else begin
           graphCoordBottom := ChartCoord(FCoordList.Items[i]^);
           graphCoordTop.x := graphCoordBottom.x;
           graphCoordTop.y := 0;
       end;

       //check if bar in view port
       if BarInViewPort( graphCoordTop, graphCoordBottom ) then begin
          //only draw to the limits
          if graphCoordTop.y > Chart.YGraphMax then graphCoordTop.y := Chart.YGraphMax;
          if graphCoordBottom.y < Chart.YGraphMin then graphCoordBottom.y := Chart.YGraphMin;
          //convert from graph to imgs coords
          Chart.GraphToImage(graphCoordTop.x, graphCoordTop.y, topX, topY);
          Chart.YGraphToImage(graphCoordBottom.y, bottomY);

          Chart.Canvas.Brush.Color := graphCoordTop.Color;

          //calc coords for bar
          bx1 := topX-(barWidth div 2);
          by1 := topY;
          bx2 := topX+(barWidth div 2);
          by2 := bottomY;


          //FIXME only draw if bar inside image coord (get a better way of doing this)
          if (bx1 >= XMin)  and (bx2 <= XMax) then
             if by1 = by2 then begin //draw a line when y=0 FIXME (clean)
                Chart.Canvas.Pen.Color := FBarBrush.Color;
                Chart.Canvas.MoveTo(bx1, by1);
                Chart.Canvas.LineTo(bx2, by2);
                Chart.Canvas.Pen.Assign( FBarPen );
             end else Chart.Canvas.Rectangle( bx1, by1, bx2, by2);

       end;
   end;




end;




constructor TPieSeries.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);

     ColorIndex := 1;

     FPiePen := TPen.Create;
     FPiePen.OnChange := StyleChanged;
     FPiePen.Mode:=pmCopy;
     FPiePen.Style:=psSolid;
     FPiePen.Width:=1;
     FPiePen.Color := clBlack;
end;

destructor TPieSeries.Destroy;
begin
   FPiePen.Free;
   inherited Destroy;
end;

procedure TPieSeries.StyleChanged(Sender:TObject);
begin
     if Chart <> nil then Chart.Invalidate;
end;

procedure TPieSeries.SetPiePen(Value:TPen);
begin
     FPiePen.Assign(Value);
end;


function TPieSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint;
begin
   if Color = clTAColor then Color := Colors[ ColorIndex ];
   Inc(ColorIndex); if ColorIndex > MaxColor then ColorIndex := 1;

   inherited AddXY(X, Y, XLabel, Color);


   if Chart <> nil then Chart.Invalidate;
   //this has to change
   result := 0;
end;

function TPieSeries.AddPie(Value: Double; Text: String; Color: TColor) : Longint;
begin
     result := AddXY( getXMinVal + 1, Value, Text, Color);
end;

procedure conv_angle(nOrigoX, nOrigoY, nLen: Integer; Angle: Double; var PX, PY: Integer);
begin
   PX := nOrigoX+round(Cos(Angle)*nLen);
   PY := nOrigoY-round(Sin(Angle)*nLen);
end;

procedure TPieSeries.Draw;
var
   nOrigoX, nOrigoY : LongInt;
   nLen             : integer;
   n100Sum: double;
   i, AX, AY, BX, BY: integer;
   curangle, prevangle, midleangle, prop: double;
   maxtextw: integer;
   CircleRect: TRect;
   markrect: trect;
   W: Integer;
      graphCoord: PChartCoord;
   MarkTxtWidth: integer;
const TAGSMargin = 20;
      MarkYMargin = 2;
      MarkXMargin = 4;
var
      SPACE_InMarks: integer;
begin
     SPACE_InMarks := 40;
   //no elements to draw
   if FCoordList.Count = 0 then exit;


   //center the chart on the canvas
   if Chart.XImageMax-Chart.XImageMin > Chart.YImageMin-Chart.YImageMax then begin
      W := Chart.YImageMin-Chart.YImageMax;
      nOrigoX  := (Chart.XImageMax-Chart.XImageMin) div 2+Chart.XImageMin;
      nOrigoY  := Round((W-1.01)/2)+Chart.YImageMax;
   end
   else begin
       W := Chart.XImageMax-Chart.XImageMin;
       nOrigoX  := Round((W-1.01)/2)+Chart.XImageMin;
      nOrigoY  := (Chart.YImageMin-Chart.YImageMax) div 2 +Chart.YImageMax;

   end;


   prevangle := 0;
   maxtextw := 0;
   n100Sum     := 0;

   for i := 0 to FCoordList.Count - 1 do begin
      graphCoord := FCoordList[i];
      n100Sum  := n100Sum + graphCoord^.y;
       if Chart.Canvas.TextWidth(graphCoord^.Text) > maxtextw then
          maxtextw := Chart.Canvas.TextWidth(graphCoord^.Text);
   end;

   //we can only draw if enought space for text is saved
   //so we remove maxtextw and MarkXMargin
   nLen := Round( (W-(maxtextw)*2) /2);
   CircleRect.Left   := nOrigoX-nLen-TAGSMargin;
   CircleRect.Top    := nOrigoY-nLen-TAGSMargin;
   CircleRect.Right  := nOrigoX+nlen+TAGSMargin;
   CircleRect.Bottom := nOrigoY+nlen+TAGSMargin;

//   Chart.Canvas.Rectangle(CircleRect.Left, CircleRect.Top, CircleRect.Right, CircleRect.Bottom);

   for i := 0 to FCoordList.Count - 1 do begin
      graphCoord := FCoordList[i];
      if graphCoord^.y < 0 then graphCoord^.y := -graphCoord^.y;
      //if graphCoord^.y = 0 then graphCoord^.y := 0.1; //just to simulate tchart when y=0

      conv_angle(nOrigoX, nOrigoY, nLen, prevangle, AX, AY);
      prop := graphCoord^.y / n100Sum ;
      curangle := prop * (2*pi)  ;
      conv_angle(nOrigoX, nOrigoY, nLen, prevangle+curangle, BX, BY);

      Chart.Canvas.Brush.color := graphCoord^.Color;
      Chart.canvas.pie(CircleRect.Left,CircleRect.Top, CircleRect.Right, CircleRect.Bottom, AX,AY,BX,BY);

      if nLen < SPACE_InMarks then
         SPACE_InMarks := nLen;


      //marks
      midleangle := prevangle + (curangle/2);
      conv_angle(nOrigoX, nOrigoY, nLen+SPACE_InMarks, midleangle, BX, BY);
      conv_angle(nOrigoX, nOrigoY, nLen, midleangle, aX, aY);

      Chart.canvas.Pen.Color := clWhite;
      Chart.canvas.moveto(ax, ay);
      Chart.canvas.lineto(bx, by);
      Chart.canvas.Pen.Color := clBlack;

       //depends on label type
      case MarksStyle of
           smsLabel: MarkTxtWidth := Chart.canvas.TextWidth( graphCoord^.Text );
           smsLabelPercent: MarkTxtWidth := Chart.canvas.TextWidth( graphCoord^.Text+' '+format('%1.3g',[prop*100])+'%' );
      end;

      //line from mark to pie
      if Bx < nOrigoX then
         markrect.Left    := BX - MarkTxtWidth - MarkXMargin
      else markrect.Left := BX -MarkXMargin;
      markrect.Right  :=  markrect.Left + MarkTxtWidth +MarkXMargin*2;

      markrect.Top   := BY - MarkYMargin;
      markrect.Bottom := BY + Chart.canvas.textheight( graphCoord^.Text )+MarkYMargin;


      Chart.canvas.Brush.Color := clYellow;
      Chart.Canvas.Rectangle(markrect.Left, markrect.Top, markrect.Right, markrect.Bottom);

      if Bx < nOrigoX then BX := BX - MarkTxtWidth;
      Chart.canvas.Brush.Color := clYellow;
      case MarksStyle of
           smsLabel: Chart.Canvas.TextOut(BX, BY, graphCoord^.Text);
           smsLabelPercent: Chart.Canvas.TextOut(BX, BY, graphCoord^.Text+' '+format('%1.3g',[prop*100])+'%');
      end;

      prevangle := prevangle + curangle;
   end;


end;


constructor TAreaSeries.Create(AOwner:TComponent);
begin
     inherited Create(AOwner);
     FAreaLinesPen := TChartPen.Create;
     FAreaBrush := TBrush.Create;
     FStairs := false;
end;

destructor TAreaSeries.Destroy;
begin
   FAreaLinesPen.Free;
   FAreaBrush.Free;
   inherited Destroy;
end;

function TAreaSeries.AddXY(X, Y: Double; XLabel: String; Color: TColor) : Longint;
begin
     if Color = clTAColor then Color := SeriesColor;

     inherited AddXY(X, Y, XLabel, Color);

     // Update max
     if X>XGraphMax then XGraphMax:=X;
     if X<XGraphMin then XGraphMin:=X;
     if Y>YGraphMax then YGraphMax:=Y;
     if Y<YGraphMin then YGraphMin:=Y;

     if Chart <> nil then Chart.Invalidate;
end;

procedure TAreaSeries.SetAreaBrush(value: TBrush);
begin
   FAreaBrush.Assign( value );
   if Chart <> nil then Chart.Invalidate;
end;

procedure TAreaSeries.SetStairs(value: Boolean);
begin
   FStairs := value;
   if Chart <> nil then Chart.Invalidate;
end;

procedure TAreaSeries.SetInvertedStairs(value: Boolean);
begin
   FInvertedStairs := value;
   if Chart <> nil then Chart.Invalidate;
end;


procedure TAreaSeries.StyleChanged(Sender:TObject);
begin
     if Chart <> nil then Chart.Invalidate;
end;


procedure TAreaSeries.Draw;
const   Larg = 4;
var
   i,j:Integer;

   xi1,yi1,xi2,yi2:Integer;
   xg1,yg1,xg2,yg2:Double;
   Inside1,Inside2:Boolean;
   Chart1:TChart;
   YLeft,YRight,XBottom,XTop:Double;
   XLine,YLine:array[1..2] of Integer;
   BLeft,BRight,BBottom,BTop:Boolean;
   XLeftI,YLeftI,XRightI,YRightI,XBottomI,YBottomI,XTopI,YTopI:Integer;
   Temp:Double;
   dx,dy,dxy,qx,rx,qy,ry,u1,u2,u3,u4:Double;
   OK:Boolean;
   XMin,XMax,Ymin,Ymax,TempI:Integer;
   graphCoord: PChartCoord;
   iy_min: integer;

begin
   if count=0 then Exit;

   with Chart do begin
      XMin:=XImageMin;
      XMax:=XImageMax;
      YMin:=YImageMin;
      YMax:=YImageMax;
   end;

   if XMin>XMax then begin
      TempI:=XMin;
      XMin:=XMax;
      XMax:=TempI;
   end;
   if YMin>YMax then begin
      TempI:=YMin;
      YMin:=YMax;
      YMax:=TempI;
   end;

   with Chart do begin
      Canvas.Pen.Mode:=pmCopy;
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Width:=1;
   end;


   for i:=0 to count-2 do begin
      graphCoord := FCoordList.Items[i];
      xg1 := graphCoord^.x;
      yg1 := graphCoord^.y;
      Chart.GraphToImage(xg1, yg1, xi1, yi1);
      graphCoord := FCoordList.Items[i+1];
      xg2 := graphCoord^.x;
      yg2 := graphCoord^.y;
      Chart.GraphToImage(xg2, yg2, xi2, yi2);

      Chart.YGraphToImage(Chart.YGraphMin, iy_min);
      Chart.Canvas.Pen.Color:= clBlack;
      Chart.Canvas.Brush.Color:= graphCoord^.Color;


         if (xg1>Chart.XGraphMin) and (xg2>Chart.XGraphMin) and (xg1<Chart.XGraphMax) and (xg2<Chart.XGraphMax) and
            (yg1>Chart.YGraphMin) and (yg2>Chart.YGraphMin) and (yg1<Chart.YGraphMax) and (yg2<Chart.YGraphMax) then
            begin
               if FStairs then begin
                  if FInvertedStairs then
                     Chart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi2), Point(xi2, yi2), Point(xi2, iy_min)])
                  else
                     Chart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi1), Point(xi2, iy_min)])
               end else
                  Chart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
                  
               continue;
            end;

         if ((xg1<Chart.XGraphMin) and (xg2<Chart.XGraphMin)) or ((xg1>Chart.XGraphMax) and (xg2>Chart.XGraphMax)) or
            ((yg1<Chart.YGraphMin) and (yg2<Chart.YGraphMin)) or ((yg1>Chart.YGraphMax) and (yg2>Chart.YGraphMax)) then
            continue;

         if yg1>yg2 then begin
            Temp:=xg1; xg1:=xg2; xg2:=Temp;
            Temp:=yg1; yg1:=yg2; yg2:=Temp;
         end;

         if yg1=yg2 then begin
            if xg1>xg2 then begin
               Temp:=xg1; xg1:=xg2; xg2:=Temp;
               Temp:=yg1; yg1:=yg2; yg2:=Temp;
            end;
            if xg1<Chart.XGraphMin then xi1:=Chart.XImageMin;
            if xg2>Chart.XGraphMax then xi2:=Chart.XImageMax;
            Chart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
            continue;
         end;

         if xg1=xg2 then begin
            if yg1<Chart.YGraphMin then yi1:=Chart.YImageMin;
            if yg2>Chart.YGraphMax then yi2:=Chart.YImageMax;
            Chart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
            continue;
         end;

         dy:=yg1-yg2;
         dx:=xg1-xg2;
         dxy:=xg1*yg2-yg1*xg2;
         qx:=Chart.XGraphMin*dy;
         rx:=Chart.XGraphMax*dy;
         qy:=Chart.YGraphMin*dx;
         ry:=Chart.YGraphMax*dx;
         u1:=qx-qy+dxy;
         u2:=qx-ry+dxy;
         u3:=rx-ry+dxy;
         u4:=rx-qy+dxy;

         OK:=False;
         if u1*u2<0 then begin
            OK:=True;
            if xg1<Chart.XGraphMin then begin
               yg1:=(Chart.XGraphMin*dy+dxy)/dx;
               xg1:=Chart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2<Chart.XGraphMin then begin
               yg2:=(Chart.XGraphMin*dy+dxy)/dx;
               xg2:=Chart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u2*u3<0 then begin
            OK:=True;
            if yg2>Chart.YGraphMax then begin
               xg2:=(Chart.YGraphMax*dx-dxy)/dy;
               yg2:=Chart.YGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u3*u4<0 then begin
            OK:=True;
            if xg1>Chart.XGraphMax then begin
               yg1:=(Chart.XGraphMax*dy+dxy)/dx;
               xg1:=Chart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2>Chart.XGraphMax then begin
               yg2:=(Chart.XGraphMax*dy+dxy)/dx;
               xg2:=Chart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u4*u1<0 then begin
            OK:=True;
            if yg1<Chart.YGraphMin then begin
               xg1:=(Chart.YGraphMin*dx-dxy)/dy;
               yg1:=Chart.YGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if OK then begin
            Chart.XGraphToImage(xg1,xi1);
            Chart.YGraphToImage(yg1,yi1);
            Chart.XGraphToImage(xg2,xi2);
            Chart.YGraphToImage(yg2,yi2);
            Chart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
         end;
   end;
end;

end.
