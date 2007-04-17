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
  classes, graphics, tagraph, taengine, sysutils, dialogs;

Const clTAColor = clScrollBar;

type

  ChartCoord = record
    x, y: double;
    Color: tcolor;
    Text: string;
  end;
  PChartCoord = ^ChartCoord;

  TSeriesPointerStyle=(psRectangle,psCircle,psCross,psDiagCross,psStar);

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
    FShowInLegend: Boolean;

    procedure SetActive(Value:Boolean);
    procedure SetMarks(Value:TSeriesMarksStyle);
    function GetXMinVal: Integer;
    procedure SetShowInLegend(Value: Boolean);
  public
    ParentChart:TChart;
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
      property ShowInLegend:Boolean read FShowInLegend write SetShowInLegend;
  end;

  TSeriesPointer=class(TPersistent)
  private
    FHorizSize,
    FVertSize:Integer;
    FStyle:TSeriesPointerStyle;
    FPen:TChartPen;
    FBrush:TBrush;
    FVisible:Boolean;
    FOwner:TChartSeries;
    FChanged: TNotifyEvent;

    Procedure SetVisible(Value:Boolean);
    Procedure SetStyle(Value:TSeriesPointerStyle);
    Procedure SetPen(Value:TChartPen);
    Procedure SetBrush(Value:TBrush);
    Procedure SetHorizSize(Value:Integer);
    Procedure SetVertSize(Value:Integer);
  protected
  public
    Constructor Create(AOwner:TChartSeries);
    Destructor Destroy; override;
//    Procedure Draw(px,py:Integer; ColorValue:TColor; AStyle:TSeriesPointerStyle);
    Procedure Draw(px,py:Integer);

    property ParentSeries:TChartSeries read FOwner;
    Procedure Assign(Source:TPersistent); override;
  published
    property Brush:TBrush read FBrush write SetBrush;
    property Pen:TChartPen read FPen write SetPen;
    Property Style:TSeriesPointerStyle read FStyle write SetStyle;
    Property Visible:Boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FChanged write FChanged;
    property HorizSize:Integer read FHorizSize write SetHorizSize default 4;
    property VertSize:Integer read FVertSize write SetVertSize default 4;

  end;


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
    FPointer:TSeriesPointer;

    XOfYGraphMin,XOfYGraphMax:Double;          // X max value of points
    FShowPoints:Boolean;
    FShowLines:Boolean;

    UpdateInProgress:Boolean;

    procedure SetShowPoints(Value:Boolean);
    procedure SetShowLines(Value:Boolean);
    procedure SetPointer(Value:TSeriesPointer);
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

    procedure BeginUpdate;
    procedure EndUpdate;

    property XGraphMin;
    property YGraphMin;
    property XGraphMax;
    property YGraphMax;
  published
    property Title;
    property Active;

    property ShowPoints:Boolean read FShowPoints write SetShowPoints;
    property ShowLines:Boolean read FShowLines write SetShowLines default True;
    property Pointer:TSeriesPointer read FPointer write SetPointer;
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
     FShowInLegend := True;
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
     if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TChartSeries.Clear;
begin
     FCoordList.Clear;

     XGraphMin:=MaxDouble;
     YGraphMin:=MaxDouble;
     XGraphMax:=MinDouble;
     YGraphMax:=MinDouble;

     if ParentChart <> nil then ParentChart.Invalidate;
end;

function TChartSeries.Count:Integer;
begin
   Result := FCoordList.count;
end;

procedure TChartSeries.SetActive(Value:Boolean);
begin
     FActive:=Value;
     if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TChartSeries.SetShowInLegend(Value:Boolean);
begin
     FShowInLegend:=Value;
     if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TChartSeries.SetMarks(Value:TSeriesMarksStyle);
begin
     FMarks := Value;
     if ParentChart <> nil then ParentChart.Invalidate;
end;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

Procedure TSeriesPointer.SetVisible(Value:Boolean);
begin
     FVisible := Value;
     if assigned( FChanged ) then FChanged(Self);
end;

Procedure TSeriesPointer.SetStyle(Value:TSeriesPointerStyle);
begin
     FStyle := Value;
     if assigned( FChanged ) then FChanged(Self);
end;

Procedure TSeriesPointer.SetPen(Value:TChartPen);
begin
     FPen.Assign(Value);
     if assigned( FChanged ) then FChanged(Self);
end;

Procedure TSeriesPointer.SetBrush(Value:TBrush);
begin
     FBrush.Assign(Value);
     if assigned( FChanged ) then FChanged(Self);
end;

Procedure TSeriesPointer.SetHorizSize(Value:Integer);
Begin
   FHorizSize := Value;
   if assigned( FChanged ) then FChanged(Self);
end;

Procedure TSeriesPointer.SetVertSize(Value:Integer);
Begin
  FVertSize := Value;
  if assigned( FChanged ) then FChanged(Self);
end;

Constructor TSeriesPointer.Create(AOwner:TChartSeries);
begin
     FBrush := TBrush.Create;
     FBrush.Color := clLime;
     FPen := TChartPen.Create;
     FOwner := AOwner;

     FHorizSize := 4;
     FVertSize  := 4;
end;

Destructor TSeriesPointer.Destroy;
begin
   FBrush.Free;
   FPen.Free;
   inherited Destroy;
end;

Procedure TSeriesPointer.Draw(px,py:Integer);
begin
     with FOwner do begin
        ParentChart.Canvas.Brush.Assign( FBrush );
        ParentChart.Canvas.Pen.Assign( FPen );

        case FStyle of
            psRectangle: begin
               ParentChart.Canvas.Rectangle(px-FHorizSize,py-FVertSize,px+FHorizSize+1,py+FVertSize+1);
            end;
            psCross: begin
               ParentChart.Canvas.MoveTo(px-FHorizSize,py);
               ParentChart.Canvas.LineTo(px+FHorizSize+1,py);
               ParentChart.Canvas.MoveTo(px,py-FVertSize);
               ParentChart.Canvas.LineTo(px,py+FVertSize+1);
            end;
            psDiagCross: begin
               ParentChart.Canvas.MoveTo(px-FHorizSize,py-FVertSize);
               ParentChart.Canvas.LineTo(px+FHorizSize+1,py+FVertSize+1);
               ParentChart.Canvas.MoveTo(px-FHorizSize,py+FVertSize+1);
               ParentChart.Canvas.LineTo(px+FHorizSize+1,py-FVertSize);
            end;
            psStar: begin
               ParentChart.Canvas.MoveTo(px-FHorizSize,py);
               ParentChart.Canvas.LineTo(px+FHorizSize+1,py);
               ParentChart.Canvas.MoveTo(px,py-FVertSize);
               ParentChart.Canvas.LineTo(px,py+FVertSize+1);

               ParentChart.Canvas.MoveTo(px-FHorizSize,py-FVertSize);
               ParentChart.Canvas.LineTo(px+FHorizSize+1,py+FVertSize+1);
               ParentChart.Canvas.MoveTo(px-FHorizSize,py+FVertSize+1);
               ParentChart.Canvas.LineTo(px+FHorizSize+1,py-FVertSize);
            end;
            psCircle: begin
               ParentChart.Canvas.Ellipse(px-FHorizSize,py-FVertSize,px+FHorizSize+1,py+FVertSize+1);
            end;
        end;
     end;
end;

Procedure TSeriesPointer.Assign(Source:TPersistent);
begin
     if Source is TSeriesPointer then
        with TSeriesPointer( Source ) do begin
             FBrush.Assign(Brush);
             FPen.Assign(Pen);
             FStyle := Style;
             FVisible := Visible;
        end;
     inherited Assign( Source );
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

constructor TSerie.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);

   FPointer := TSeriesPointer.Create( Self );
   FPointer.FStyle := psCross;
   FPointer.OnChange := StyleChanged;

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
     if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TSerie.SetPointer(Value:TSeriesPointer);
begin
     FPointer.Assign(Value);
     if ParentChart<>nil then ParentChart.Invalidate;
end;

procedure TSerie.Draw;
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
begin
   if count=0 then Exit;

   with ParentChart do begin
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

   with ParentChart do begin
      Canvas.Pen.Mode:=pmCopy;
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Width:=1;
   end;

   Min:=ParentChart.XGraphMin;
   Max:=ParentChart.XGraphMax;


   for i:=0 to count-2 do begin
      graphCoord := FCoordList.Items[i];
      xg1 := graphCoord^.x;
      yg1 := graphCoord^.y;
      ParentChart.GraphToImage(xg1, yg1, xi1, yi1);
      graphCoord := FCoordList.Items[i+1];
      xg2 := graphCoord^.x;
      yg2 := graphCoord^.y;
      ParentChart.GraphToImage(xg2, yg2, xi2, yi2);

      ParentChart.Canvas.Pen.Color:= graphCoord^.Color;

      if FShowLines then begin
         if (xg1>ParentChart.XGraphMin) and (xg2>ParentChart.XGraphMin) and (xg1<ParentChart.XGraphMax) and (xg2<ParentChart.XGraphMax) and
            (yg1>ParentChart.YGraphMin) and (yg2>ParentChart.YGraphMin) and (yg1<ParentChart.YGraphMax) and (yg2<ParentChart.YGraphMax) then
            begin
               ParentChart.Canvas.MoveTo(xi1,yi1);
               ParentChart.Canvas.LineTo(xi2,yi2);
               goto Points;
            end;

         if ((xg1<ParentChart.XGraphMin) and (xg2<ParentChart.XGraphMin)) or ((xg1>ParentChart.XGraphMax) and (xg2>ParentChart.XGraphMax)) or
            ((yg1<ParentChart.YGraphMin) and (yg2<ParentChart.YGraphMin)) or ((yg1>ParentChart.YGraphMax) and (yg2>ParentChart.YGraphMax)) then
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
            if xg1<ParentChart.XGraphMin then xi1:=ParentChart.XImageMin;
            if xg2>ParentChart.XGraphMax then xi2:=ParentChart.XImageMax;
            ParentChart.Canvas.MoveTo(xi1,yi1);
            ParentChart.Canvas.LineTo(xi2,yi2);
            goto Points;
         end;

         if xg1=xg2 then begin
            if yg1<ParentChart.YGraphMin then yi1:=ParentChart.YImageMin;
            if yg2>ParentChart.YGraphMax then yi2:=ParentChart.YImageMax;
            ParentChart.Canvas.MoveTo(xi1,yi1);
            ParentChart.Canvas.LineTo(xi2,yi2);
            goto Points;
         end;

         dy:=yg1-yg2;
         dx:=xg1-xg2;
         dxy:=xg1*yg2-yg1*xg2;
         qx:=ParentChart.XGraphMin*dy;
         rx:=ParentChart.XGraphMax*dy;
         qy:=ParentChart.YGraphMin*dx;
         ry:=ParentChart.YGraphMax*dx;
         u1:=qx-qy+dxy;
         u2:=qx-ry+dxy;
         u3:=rx-ry+dxy;
         u4:=rx-qy+dxy;

         OK:=False;
         if u1*u2<0 then begin
            OK:=True;
            if xg1<ParentChart.XGraphMin then begin
               yg1:=(ParentChart.XGraphMin*dy+dxy)/dx;
               xg1:=ParentChart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2<ParentChart.XGraphMin then begin
               yg2:=(ParentChart.XGraphMin*dy+dxy)/dx;
               xg2:=ParentChart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u2*u3<0 then begin
            OK:=True;
            if yg2>ParentChart.YGraphMax then begin
               xg2:=(ParentChart.YGraphMax*dx-dxy)/dy;
               yg2:=ParentChart.YGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u3*u4<0 then begin
            OK:=True;
            if xg1>ParentChart.XGraphMax then begin
               yg1:=(ParentChart.XGraphMax*dy+dxy)/dx;
               xg1:=ParentChart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2>ParentChart.XGraphMax then begin
               yg2:=(ParentChart.XGraphMax*dy+dxy)/dx;
               xg2:=ParentChart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u4*u1<0 then begin
            OK:=True;
            if yg1<ParentChart.YGraphMin then begin
               xg1:=(ParentChart.YGraphMin*dx-dxy)/dy;
               yg1:=ParentChart.YGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if OK then begin
            ParentChart.XGraphToImage(xg1,xi1);
            ParentChart.YGraphToImage(yg1,yi1);
            ParentChart.XGraphToImage(xg2,xi2);
            ParentChart.YGraphToImage(yg2,yi2);

            ParentChart.Canvas.MoveTo(xi1,yi1);
            ParentChart.Canvas.LineTo(xi2,yi2);

         end;

      end;

      Points:
        if FShowPoints and (yi1>=YMin) and (yi1<=YMax)
           and (xi1>=XMin) and (xi1<=XMax) then begin
           ParentChart.Canvas.Pen.Color := clBlack;
           ParentChart.Canvas.Brush.Color:=graphCoord^.Color;

           FPointer.Draw(xi1, yi1 );
         end;

   end;

   // Draw last point
   graphCoord := FCoordList.items[ Count - 1 ];
   xg1 := graphCoord^.x;
   yg1 := graphCoord^.y;
   ParentChart.GraphToImage(xg1, yg1, xi1, yi1);

   if FShowPoints and (yi1>=YMin) and
      (yi1<=YMax) and (xi1>=XMin) and (xi1<=XMax) then begin
         ParentChart.Canvas.Pen.Color := clBlack;
         ParentChart.Canvas.Brush.Color:=graphCoord^.Color;
         FPointer.draw( xi1, yi1 );
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

     if ParentChart <> nil then ParentChart.Invalidate;
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

if ParentChart <> nil then ParentChart.Invalidate;
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

if ParentChart <> nil then ParentChart.Invalidate;
end;

function TSerie.GetXImgValue(Index:Integer):Integer;
begin
     ParentChart.XGraphToImage( PChartCoord(FCoordList.Items[Index])^.x, Result );
end;

function TSerie.GetYImgValue(Index:Integer):Integer;
begin
     ParentChart.YGraphToImage( PChartCoord(FCoordList.Items[Index])^.y, Result );
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
     if ParentChart<>nil then ParentChart.Invalidate;
end;

procedure TSerie.SetShowLines(Value:Boolean);
begin
     FShowLines:=Value;
     if ParentChart<>nil then ParentChart.Invalidate;
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

     if ParentChart <> nil then ParentChart.Invalidate;
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
     if ParentChart <> nil then ParentChart.Invalidate;
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
        if ParentChart<>nil then ParentChart.Invalidate;
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

     if ParentChart<>nil then ParentChart.Invalidate;
end;

procedure TLine.Draw;
var
   XMin,XMax,Ymin,Ymax,TempI:Integer;
   label Points;
begin

with ParentChart as TChart do
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
ParentChart.Canvas.Pen.Assign(FPen);

case LineStyle of
   lsHorizontal:
      if (PosGraph < ParentChart.XGraphMax) and (PosGraph > ParentChart.XGraphMin) then begin
         ParentChart.YGraphToImage(PosGraph,PosImage);
         ParentChart.Canvas.MoveTo(XMin,PosImage);
         ParentChart.Canvas.LineTo(XMax,PosImage);
      end;
   lsVertical:
      if (PosGraph < ParentChart.YGraphMax) and (PosGraph > ParentChart.YGraphMin) then begin
         ParentChart.XGraphToImage(PosGraph,PosImage);
         ParentChart.Canvas.MoveTo(PosImage,YMin);
         ParentChart.Canvas.LineTo(PosImage,YMax);
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
     if ParentChart<>nil then ParentChart.Invalidate;
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


   if ParentChart <> nil then ParentChart.Invalidate;
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
        result :=  ( (cTop.x >= ParentChart.XGraphMin) and (cTop.x <= ParentChart.XGraphMax) )
                    and ( ( (cTop.y > ParentChart.YGraphMax) and (cBottom.y < ParentChart.YGraphMin))
                          or ( (cTop.y < ParentChart.YGraphMax) and (cTop.y > ParentChart.YGraphMin))
                          or ( (cBottom.y < ParentChart.YGraphMax) and (cBottom.y > ParentChart.YGraphMin))
                         );
   end;

begin
   //no elements to draw
   if FCoordList.Count = 0 then exit;

   //get the limits (for the zoom) ??
   XMin := ParentChart.XImageMin;
   XMax := ParentChart.XImageMax;
   if XMin > XMax then begin
      TempI:=XMin;
      XMin:=XMax;
      XMax:=TempI;
   end;

   // Draw the bars
   ParentChart.Canvas.Pen.Assign( FBarPen );
   ParentChart.Canvas.Brush.Assign( FBarBrush );

   //calc the single bar width
   barWidth:=Round((FBarWidthPercent*0.01)*ParentChart.ChartWidth/FCoordList.Count);
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
          if graphCoordTop.y > ParentChart.YGraphMax then graphCoordTop.y := ParentChart.YGraphMax;
          if graphCoordBottom.y < ParentChart.YGraphMin then graphCoordBottom.y := ParentChart.YGraphMin;
          //convert from graph to imgs coords
          ParentChart.GraphToImage(graphCoordTop.x, graphCoordTop.y, topX, topY);
          ParentChart.YGraphToImage(graphCoordBottom.y, bottomY);

          ParentChart.Canvas.Brush.Color := graphCoordTop.Color;

          //calc coords for bar
          bx1 := topX-(barWidth div 2);
          by1 := topY;
          bx2 := topX+(barWidth div 2);
          by2 := bottomY;


          //FIXME only draw if bar inside image coord (get a better way of doing this)
          if (bx1 >= XMin)  and (bx2 <= XMax) then
             if by1 = by2 then begin //draw a line when y=0 FIXME (clean)
                ParentChart.Canvas.Pen.Color := FBarBrush.Color;
                ParentChart.Canvas.MoveTo(bx1, by1);
                ParentChart.Canvas.LineTo(bx2, by2);
                ParentChart.Canvas.Pen.Assign( FBarPen );
             end else ParentChart.Canvas.Rectangle( bx1, by1, bx2, by2);

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
     if ParentChart <> nil then ParentChart.Invalidate;
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


   if ParentChart <> nil then ParentChart.Invalidate;
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


   //center the ParentChart on the canvas
   if ParentChart.XImageMax-ParentChart.XImageMin > ParentChart.YImageMin-ParentChart.YImageMax then begin
      W := ParentChart.YImageMin-ParentChart.YImageMax;
      nOrigoX  := (ParentChart.XImageMax-ParentChart.XImageMin) div 2+ParentChart.XImageMin;
      nOrigoY  := Round((W-1.01)/2)+ParentChart.YImageMax;
   end
   else begin
       W := ParentChart.XImageMax-ParentChart.XImageMin;
       nOrigoX  := Round((W-1.01)/2)+ParentChart.XImageMin;
      nOrigoY  := (ParentChart.YImageMin-ParentChart.YImageMax) div 2 +ParentChart.YImageMax;

   end;


   prevangle := 0;
   maxtextw := 0;
   n100Sum     := 0;

   for i := 0 to FCoordList.Count - 1 do begin
      graphCoord := FCoordList[i];
      n100Sum  := n100Sum + graphCoord^.y;
       if ParentChart.Canvas.TextWidth(graphCoord^.Text) > maxtextw then
          maxtextw := ParentChart.Canvas.TextWidth(graphCoord^.Text);
   end;

   //we can only draw if enought space for text is saved
   //so we remove maxtextw and MarkXMargin
   nLen := Round( (W-(maxtextw)*2) /2);
   CircleRect.Left   := nOrigoX-nLen-TAGSMargin;
   CircleRect.Top    := nOrigoY-nLen-TAGSMargin;
   CircleRect.Right  := nOrigoX+nlen+TAGSMargin;
   CircleRect.Bottom := nOrigoY+nlen+TAGSMargin;

//   ParentChart.Canvas.Rectangle(CircleRect.Left, CircleRect.Top, CircleRect.Right, CircleRect.Bottom);

   for i := 0 to FCoordList.Count - 1 do begin
      graphCoord := FCoordList[i];
      if graphCoord^.y < 0 then graphCoord^.y := -graphCoord^.y;
      //if graphCoord^.y = 0 then graphCoord^.y := 0.1; //just to simulate tchart when y=0

      conv_angle(nOrigoX, nOrigoY, nLen, prevangle, AX, AY);
      prop := graphCoord^.y / n100Sum ;
      curangle := prop * (2*pi)  ;
      conv_angle(nOrigoX, nOrigoY, nLen, prevangle+curangle, BX, BY);

      ParentChart.Canvas.Brush.color := graphCoord^.Color;
      ParentChart.canvas.pie(CircleRect.Left,CircleRect.Top, CircleRect.Right, CircleRect.Bottom, AX,AY,BX,BY);

      if nLen < SPACE_InMarks then
         SPACE_InMarks := nLen;


      //marks
      midleangle := prevangle + (curangle/2);
      conv_angle(nOrigoX, nOrigoY, nLen+SPACE_InMarks, midleangle, BX, BY);
      conv_angle(nOrigoX, nOrigoY, nLen, midleangle, aX, aY);

      ParentChart.canvas.Pen.Color := clWhite;
      ParentChart.canvas.moveto(ax, ay);
      ParentChart.canvas.lineto(bx, by);
      ParentChart.canvas.Pen.Color := clBlack;

       //depends on label type
      case MarksStyle of
           smsLabel: MarkTxtWidth := ParentChart.canvas.TextWidth( graphCoord^.Text );
           smsLabelPercent: MarkTxtWidth := ParentChart.canvas.TextWidth( graphCoord^.Text+' '+format('%1.3g',[prop*100])+'%' );
      end;

      //line from mark to pie
      if Bx < nOrigoX then
         markrect.Left    := BX - MarkTxtWidth - MarkXMargin
      else markrect.Left := BX -MarkXMargin;
      markrect.Right  :=  markrect.Left + MarkTxtWidth +MarkXMargin*2;

      markrect.Top   := BY - MarkYMargin;
      markrect.Bottom := BY + ParentChart.canvas.textheight( graphCoord^.Text )+MarkYMargin;


      ParentChart.canvas.Brush.Color := clYellow;
      ParentChart.Canvas.Rectangle(markrect.Left, markrect.Top, markrect.Right, markrect.Bottom);

      if Bx < nOrigoX then BX := BX - MarkTxtWidth;
      ParentChart.canvas.Brush.Color := clYellow;
      case MarksStyle of
           smsLabel: ParentChart.Canvas.TextOut(BX, BY, graphCoord^.Text);
           smsLabelPercent: ParentChart.Canvas.TextOut(BX, BY, graphCoord^.Text+' '+format('%1.3g',[prop*100])+'%');
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

     if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TAreaSeries.SetAreaBrush(value: TBrush);
begin
   FAreaBrush.Assign( value );
   if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TAreaSeries.SetStairs(value: Boolean);
begin
   FStairs := value;
   if ParentChart <> nil then ParentChart.Invalidate;
end;

procedure TAreaSeries.SetInvertedStairs(value: Boolean);
begin
   FInvertedStairs := value;
   if ParentChart <> nil then ParentChart.Invalidate;
end;


procedure TAreaSeries.StyleChanged(Sender:TObject);
begin
     if ParentChart <> nil then ParentChart.Invalidate;
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

   with ParentChart do begin
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

   with ParentChart do begin
      Canvas.Pen.Mode:=pmCopy;
      Canvas.Pen.Style:=psSolid;
      Canvas.Pen.Width:=1;
   end;


   for i:=0 to count-2 do begin
      graphCoord := FCoordList.Items[i];
      xg1 := graphCoord^.x;
      yg1 := graphCoord^.y;
      ParentChart.GraphToImage(xg1, yg1, xi1, yi1);
      graphCoord := FCoordList.Items[i+1];
      xg2 := graphCoord^.x;
      yg2 := graphCoord^.y;
      ParentChart.GraphToImage(xg2, yg2, xi2, yi2);

      ParentChart.YGraphToImage(ParentChart.YGraphMin, iy_min);
      ParentChart.Canvas.Pen.Color:= clBlack;
      ParentChart.Canvas.Brush.Color:= graphCoord^.Color;


         if (xg1>ParentChart.XGraphMin) and (xg2>ParentChart.XGraphMin) and (xg1<ParentChart.XGraphMax) and (xg2<ParentChart.XGraphMax) and
            (yg1>ParentChart.YGraphMin) and (yg2>ParentChart.YGraphMin) and (yg1<ParentChart.YGraphMax) and (yg2<ParentChart.YGraphMax) then
            begin
               if FStairs then begin
                  if FInvertedStairs then
                     ParentChart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi2), Point(xi2, yi2), Point(xi2, iy_min)])
                  else
                     ParentChart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi1), Point(xi2, iy_min)])
               end else
                  ParentChart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
                  
               continue;
            end;

         if ((xg1<ParentChart.XGraphMin) and (xg2<ParentChart.XGraphMin)) or ((xg1>ParentChart.XGraphMax) and (xg2>ParentChart.XGraphMax)) or
            ((yg1<ParentChart.YGraphMin) and (yg2<ParentChart.YGraphMin)) or ((yg1>ParentChart.YGraphMax) and (yg2>ParentChart.YGraphMax)) then
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
            if xg1<ParentChart.XGraphMin then xi1:=ParentChart.XImageMin;
            if xg2>ParentChart.XGraphMax then xi2:=ParentChart.XImageMax;
            ParentChart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
            continue;
         end;

         if xg1=xg2 then begin
            if yg1<ParentChart.YGraphMin then yi1:=ParentChart.YImageMin;
            if yg2>ParentChart.YGraphMax then yi2:=ParentChart.YImageMax;
            ParentChart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
            continue;
         end;

         dy:=yg1-yg2;
         dx:=xg1-xg2;
         dxy:=xg1*yg2-yg1*xg2;
         qx:=ParentChart.XGraphMin*dy;
         rx:=ParentChart.XGraphMax*dy;
         qy:=ParentChart.YGraphMin*dx;
         ry:=ParentChart.YGraphMax*dx;
         u1:=qx-qy+dxy;
         u2:=qx-ry+dxy;
         u3:=rx-ry+dxy;
         u4:=rx-qy+dxy;

         OK:=False;
         if u1*u2<0 then begin
            OK:=True;
            if xg1<ParentChart.XGraphMin then begin
               yg1:=(ParentChart.XGraphMin*dy+dxy)/dx;
               xg1:=ParentChart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2<ParentChart.XGraphMin then begin
               yg2:=(ParentChart.XGraphMin*dy+dxy)/dx;
               xg2:=ParentChart.XGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u2*u3<0 then begin
            OK:=True;
            if yg2>ParentChart.YGraphMax then begin
               xg2:=(ParentChart.YGraphMax*dx-dxy)/dy;
               yg2:=ParentChart.YGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u3*u4<0 then begin
            OK:=True;
            if xg1>ParentChart.XGraphMax then begin
               yg1:=(ParentChart.XGraphMax*dy+dxy)/dx;
               xg1:=ParentChart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
            if xg2>ParentChart.XGraphMax then begin
               yg2:=(ParentChart.XGraphMax*dy+dxy)/dx;
               xg2:=ParentChart.XGraphMax;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if u4*u1<0 then begin
            OK:=True;
            if yg1<ParentChart.YGraphMin then begin
               xg1:=(ParentChart.YGraphMin*dx-dxy)/dy;
               yg1:=ParentChart.YGraphMin;
               dy:=yg1-yg2;
               dx:=xg1-xg2;
               dxy:=xg1*yg2-yg1*xg2;
            end;
         end;

         if OK then begin
            ParentChart.XGraphToImage(xg1,xi1);
            ParentChart.YGraphToImage(yg1,yi1);
            ParentChart.XGraphToImage(xg2,xi2);
            ParentChart.YGraphToImage(yg2,yi2);
            ParentChart.Canvas.Polygon([Point(xi1, iy_min), Point(xi1, yi1), Point(xi2, yi2), Point(xi2, iy_min)]);
         end;
   end;
end;

end.
