{
 /***************************************************************************
                               TAGraph.pp
                               ----------
                    Component Library Standard Graph


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
unit TAGraph;


{$IFDEF fpc}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF fpc}
  LCLIntF, LCLType, LResources,
  {$ELSE}
  Windows,
  {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Dialogs, StdCtrls, TAEngine;

const
  MinDouble=-1.7e308;
  MaxDouble=1.7e308;
  MaxArray=2;
  MaxColor=15;
  Colors:array[1..MaxColor] of TColor=
     ( clRed,
       clGreen,
       clyellow,
       clBlue,
       clWhite,
       clGray,
       clFuchsia,
       clTeal,
       clNavy,
       clMaroon,
       clLime,
       clOlive,
       clPurple,
       clSilver,
       clAqua
       );

type



  TDrawVertReticule=procedure(Sender:TComponent;IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double) of object;
  TDrawReticule=procedure(Sender:TComponent;IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double) of object;


  ///luis teste
  TCustomChart = class(TGraphicControl);

  TChartPen = class(TPen)
  private
      FVisible: boolean;
      FChanged: TNotifyEvent;
      procedure SetVisible(value: boolean);
  protected
    procedure Assign(Source:TPersistent); override;
  published
    property Visible: boolean read FVisible write SetVisible;
    property OnChange: TNotifyEvent read FChanged write FChanged;
  end;




  TLegendAlignment=(laLeft,laRight,laTop,laBottom);
  TChartLegend = class(TPersistent)
  private
    FVisible: boolean;
    FAlignment: TLegendAlignment;
    FOwner: TCustomChart;
    FChanged: TNotifyEvent;
    FFont: TFont;
    FFrame: TChartPen;

    procedure SetVisible(value: boolean);
    procedure SetAlignment(value: TLegendAlignment);
    procedure SetFont(value: TFont);
    procedure SetFrame(value: TChartPen);
    procedure StyleChanged(Sender: TObject);
  protected
    procedure Assign(Source:TPersistent); override;
  public
    Constructor Create(AOwner: TCustomChart);
    Destructor Destroy; override;
  published
    property Visible: boolean read FVisible write SetVisible;
    property Alignment: TLegendAlignment read FAlignment write SetAlignment;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property OnChange: TNotifyEvent read FChanged write FChanged;
  end;

  TChartTitle = class(TPersistent)
  private
    FVisible: boolean;
    FOwner: TCustomChart;
    FChanged: TNotifyEvent;
    FFont: TFont;
    FFrame: TChartPen;
    FBrush: TBrush;
    FText: TStrings;
    FAlignment: TAlignment;

    procedure SetVisible(value: boolean);
    procedure SetFont(value: TFont);
    procedure SetFrame(value: TChartPen);
    procedure SetBrush(value: TBrush);
    procedure SetText(value: TStrings);
    procedure SetAlignment(value: TAlignment);
    procedure StyleChanged(Sender: TObject);
  protected
    procedure Assign(Source:TPersistent); override;
  public
    Constructor Create(AOwner: TCustomChart);
    Destructor Destroy; override;
  published
    property Visible: boolean read FVisible write SetVisible;
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Frame: TChartPen read FFrame write SetFrame;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Text: TStrings read FText write SetText;
    property OnChange: TNotifyEvent read FChanged write FChanged;
  end;

  TChartAxisTitle = class(TPersistent)
  private
    FVisible: boolean;
    FOwner: TCustomChart;
    FChanged: TNotifyEvent;
    FAngle: Integer;
    FCaption: String;
    FFont: TFont;

    procedure SetCaption(value: String);
    procedure SetAngle(value: Integer);
    procedure SetFont(value: TFont);
    procedure StyleChanged(Sender: TObject);
  protected
    procedure Assign(Source:TPersistent); override;
  public
    Constructor Create(AOwner: TCustomChart);
    Destructor Destroy; override;
  published
    property Caption: String read FCaption write SetCaption;
    property Angle: Integer read FAngle write SetAngle;
    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FChanged write FChanged;
  end;


  TChartAxis = class(TPersistent)
  private
    FVisible: boolean;
    FOwner: TCustomChart;
    FChanged: TNotifyEvent;
    FTitle: TChartAxisTitle;
    FGrid: TChartPen;

    procedure SetVisible(value: boolean);
    procedure SetTitle(value: TChartAxisTitle);
    procedure SetGrid(value: TChartPen);
    procedure StyleChanged(Sender: TObject);
  protected
    procedure Assign(Source:TPersistent); override;
  public
    Constructor Create(AOwner: TCustomChart);
    Destructor Destroy; override;
  published
    property Visible: boolean read FVisible write SetVisible;
    property Title: TChartAxisTitle read FTitle write SetTitle;
    property Grid: TChartPen read FGrid write SetGrid;
    property OnChange: TNotifyEvent read FChanged write FChanged;
  end;




  TChart = class(TCustomChart)
  private
    { Déclarations privées }
    TmpBrush: TBrush;
    TmpPen: TPen;
    TmpFont: TFont;
    FSeries:TSeriesList;                           // List of series
    FMirrorX:Boolean;                           // From right to left ?
    YMarkWidth:Integer;                         // Depend on Y marks
    FXGraphMin,FYGraphMin:Double;               // Graph coordinates of limits
    FXGraphMax,FYGraphMax:Double;
    FAutoUpdateXMin:Boolean;                    // Automatic calculation of XMin limit of graph ?
    FAutoUpdateXMax:Boolean;                    // Automatic calculation of XMax limit of graph ?
    FAutoUpdateYMin:Boolean;                    // Automatic calculation of YMin limit of graph ?
    FAutoUpdateYMax:Boolean;                    // Automatic calculation of YMax limit of graph ?

    FLegend: TChartLegend;                      //legend configuration
    FTitle: TChartTitle;                      //legend configuration
    FFoot: TChartTitle;                      //legend configuration
    FLeftAxis: TChartAxis;
    FBottomAxis: TChartAxis;

    FAllowZoom: Boolean;


    FGraphBrush:TBrush;
    AxisColor:TColor;                           // Axis color
    ax,bx,ay,by:Double;                         // Image<->Graphe conversion coefs

    Down:Boolean;
    Zoom:Boolean;
    Fixed:Boolean;
    XDown,YDown,XOld,YOld:Integer;
    XMarkOld,YMarkOld:Integer;
    ZoomRect:TRect;

    FShowReticule:Boolean;
    FShowVerticalReticule:Boolean;

    FDrawVertReticule:TDrawVertReticule;
    FDrawReticule:TDrawReticule;

    XReticule,YReticule:Integer;

    FFrame: TChartPen;

    FBackColor: TColor;

    FAxisVisible: boolean;


    procedure SetAutoUpdateXMin(Value:Boolean);
    procedure SetAutoUpdateXMax(Value:Boolean);
    procedure SetAutoUpdateYMin(Value:Boolean);
    procedure SetAutoUpdateYMax(Value:Boolean);
    procedure SetXGraphMin(Value:Double);
    procedure SetYGraphMin(Value:Double);
    procedure SetXGraphMax(Value:Double);
    procedure SetYGraphMax(Value:Double);
    procedure SetMirrorX(Value:Boolean);
    procedure SetGraphBrush(Value:TBrush);
    procedure SetTitle(Value:TChartTitle);
    procedure SetFoot(Value:TChartTitle);
    function  GetLegendWidth:Integer;
    procedure GetPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
    procedure GetXPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
    procedure GetYPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
    procedure DrawReticule(X,Y:Integer);
    procedure DrawVerticalReticule(X:Integer);
    procedure SetShowVerticalReticule(Value:Boolean);
    procedure SetShowReticule(Value:Boolean);

    procedure SetLegend(Value:TChartLegend);
    procedure SetLeftAxis(Value:TChartAxis);
    procedure SetBottomAxis(Value:TChartAxis);

    procedure SetFrame(Value: TChartPen);

    procedure SetBackColor(Value: TColor);
    procedure SetAxisVisible(Value: boolean);

    function GetChartHeight: integer;
    function GetChartWidth: integer;

    function GetSeriesCount: Integer;

    function only_pie: boolean;
   function get_pie: pointer;

  protected
    { Déclarations protégées }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoDrawVertReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double); virtual;
    procedure DoDrawReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double); virtual;
  public
      XImageMin,YImageMin:Integer;                // Image coordinates of limits
    XImageMax,YImageMax:Integer;


    { Déclarations publiques }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy; override;
    procedure Paint; override;
    procedure Refresh;
    procedure Clean;
    procedure DrawTitleFoot;
    procedure DrawAxis;
    procedure DrawLegend;

    procedure AddSerie(Serie:TComponent);
//    procedure DeleteSerie(Serie:TTASerie);
    procedure DeleteSerie(Serie:TComponent);
    function  GetSerie(i:Integer):TComponent;
    procedure SetAutoXMin(Auto:Boolean);
    procedure SetAutoXMax(Auto:Boolean);
    procedure SetAutoYMin(Auto:Boolean);
    procedure SetAutoYMax(Auto:Boolean);

    procedure XGraphToImage(Xin:Double;var XOut:Integer);
    procedure YGraphToImage(Yin:Double;var YOut:Integer);
    procedure GraphToImage(Xin,Yin:Double;var XOut,YOut:Integer);
    procedure XImageToGraph(XIn:Integer;var XOut:Double);
    procedure YImageToGraph(YIn:Integer;var YOut:Double);
    procedure ImageToGraph(XIn,YIn:Integer;var XOut,YOut:Double);
    procedure DisplaySeries;

    property SeriesCount:Integer read GetSeriesCount;

    function GetNewColor:TColor;

    property Canvas;

    property ChartHeight: Integer read GetChartHeight;
    property ChartWidth: Integer read GetChartWidth;

    property Series: TSeriesList read FSeries write FSeries;
  published
    { Déclarations publiées }
    procedure StyleChanged(Sender: TObject);
    property AutoUpdateXMin:Boolean read FAutoUpdateXMin write SetAutoUpdateXMin;
    property AutoUpdateXMax:Boolean read FAutoUpdateXMax write SetAutoUpdateXMax;
    property AutoUpdateYMin:Boolean read FAutoUpdateYMin write SetAutoUpdateYMin;
    property AutoUpdateYMax:Boolean read FAutoUpdateYMax write SetAutoUpdateYMax;
    property XGraphMin:Double read FXGraphMin write SetXGraphMin;
    property YGraphMin:Double read FYGraphMin write SetYGraphMin;
    property XGraphMax:Double read FXGraphMax write SetXGraphMax;
    property YGraphMax:Double read FYGraphMax write SetYGraphMax;
    property MirrorX:Boolean read FMirrorX write SetMirrorX;
    property GraphBrush:TBrush read FGraphBrush write SetGraphBrush;
    property ShowVerticalReticule:Boolean read FShowVerticalReticule write SetShowVerticalReticule;
    property ShowReticule:Boolean read FShowReticule write SetShowReticule;

    property OnDrawVertReticule:TDrawVertReticule read FDrawVertReticule write FDrawVertReticule;
    property OnDrawReticule:TDrawReticule read FDrawReticule write FDrawReticule;

    property Legend: TChartLegend read FLegend write SetLegend;
    property Title: TChartTitle read FTitle write SetTitle;
    property Foot: TChartTitle read FFoot write SetFoot;


    property AllowZoom: Boolean read FAllowZoom write FAllowZoom;

    property LeftAxis: TChartAxis read FLeftAxis write SetLeftAxis;
    property BottomAxis: TChartAxis read FBottomAxis write SetBottomAxis;
    property Frame: TChartPen read FFrame write setFrame;

    property BackColor: TColor read FBackColor write SetBackColor;

    property AxisVisible: boolean read FAxisVisible write SetAxisVisible;

    property Align;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;



procedure Register;

implementation

uses taseries;

procedure RotateLabel(Canvas: TCanvas; x,y:Integer; Const St:String; RotDegree:Integer);
var OldFont,
    NewFont: HFONT;
    LogRec: TLOGFONT;
    DC : HDC;
begin
  with Canvas do begin
    Brush.Style := bsClear;
    GetObject(Font.Handle, SizeOf(LogRec), @LogRec);
    LogRec.lfEscapement   := RotDegree*10;
    LogRec.lfOrientation  := 0;
    LogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    NewFont := CreateFontIndirect(LogRec);
    DC:=Handle;
  end;
  OldFont := SelectObject(DC,NewFont);
  TextOut(DC,X, Y, @St[1],Length(St));
  DeleteObject(SelectObject(DC,OldFont));
end;



procedure TChartPen.SetVisible(Value: Boolean);
begin
     FVisible := Value;
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartPen.Assign(Source: TPersistent);
begin
     if Source is TChartPen then
        with TChartPen( Source ) do begin
             FVisible := Visible;
        end;
     inherited Assign( Source );
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

Constructor TChartAxis.Create(AOwner: TCustomChart);
begin
     inherited Create;
     FVisible := True;
     FTitle := TChartAxisTitle.Create(AOwner);
     FTitle.OnChange := StyleChanged;
     FGrid := TChartPen.Create;
     FGrid.Width:=1;
     FGrid.Color:=clGray;
     FGrid.Style:=psDot;
     FGrid.Visible := True;
     FGrid.OnChange := StyleChanged;
end;

Destructor TChartAxis.Destroy;
begin
     FTitle.Free;
     FGrid.Free;
     inherited;
end;


procedure TChartAxis.SetVisible(value: boolean);
begin
     FVisible := value;
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartAxis.SetTitle(value: TChartAxisTitle);
begin
     FTitle.Assign(Value);
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartAxis.SetGrid(value: TChartPen);
begin
     FGrid.Assign(Value);
     if assigned( FChanged ) then FChanged(Self);
end;


procedure TChartAxis.Assign(Source:TPersistent);
begin
     if Source is TChartAxis then
        with TChartAxis(Source) do begin
             FTitle.Assign( Title );
             FVisible := Visible;
        end;
    inherited Assign(Source);
end;

procedure TChartAxis.StyleChanged(Sender: TObject);
begin
     if assigned( FChanged ) then FChanged(Self);
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////


Constructor TChartAxisTitle.Create(AOwner: TCustomChart);
begin
     inherited Create;
     FFont := TFont.Create;
end;

Destructor TChartAxisTitle.Destroy;
begin
     FFont.Destroy;
     inherited;
end;

procedure TChartAxisTitle.SetCaption(value: String);
begin
     FCaption := Value;
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartAxisTitle.SetAngle(value: Integer);
begin
     FAngle := Value;
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartAxisTitle.SetFont(value: TFont);
begin
     FFont.Assign( Value );
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartAxisTitle.StyleChanged(Sender: TObject);
begin
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartAxisTitle.Assign(Source:TPersistent);
begin
     if Source is TChartAxisTitle then
        with TChartAxisTitle(Source) do begin
             FCaption := Caption;
             FAngle := Angle;
             FFont.Assign(Font);
        end;
    inherited Assign(Source);
end;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////



Constructor TChartLegend.Create(AOwner: TCustomChart);
begin
     inherited create;
     FOwner := AOwner;
     FVisible := false;
     FAlignment := laRight;

     FFont := TFont.Create;
     FFont.OnChange := StyleChanged;
     FFrame := TChartPen.Create;
     FFrame.OnChange := StyleChanged;
end;

Destructor TChartLegend.Destroy;
begin
     FFont.Destroy;
     FFrame.Destroy;

     inherited Destroy;
end;

Procedure TChartLegend.Assign(Source:TPersistent);
begin
  if Source is TChartLegend then
  With TChartLegend(Source) do
  Begin
    Self.FVisible   := FVisible;
    Self.FAlignment := FAlignment;
  end;

  inherited Assign(Source);
end;

procedure TChartLegend.SetVisible(value: boolean);
begin
     FVisible := value;
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartLegend.SetAlignment(value: TLegendAlignment);
begin
     FAlignment := value;
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartLegend.SetFont(value: TFont);
begin
     FFont.Assign( value );
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartLegend.SetFrame(value: TChartPen);
begin
     FFrame.Assign( value );
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartLegend.StyleChanged(Sender: TObject);
begin
     if assigned( FChanged ) then FChanged(Self);;
end;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

Constructor TChartTitle.Create(AOwner: TCustomChart);
begin
     inherited create;
     FOwner := AOwner;
     FVisible := false;

     FFont := TFont.Create;
     FFont.Color := clBlue;
     FFont.OnChange := StyleChanged;
     FFrame := TChartPen.Create;
     FFrame.OnChange := StyleChanged;
     FBrush := TBrush.Create;
     FBrush.Color := FOwner.Color;
     FBrush.OnChange := StyleChanged;
     FText := TStringList.Create;
     FText.Add('TAChart');
end;

Destructor TChartTitle.Destroy;
begin
     FFont.Destroy;
     FFrame.Destroy;
     FBrush.Destroy;
     FText.Destroy;

     inherited Destroy;
end;

Procedure TChartTitle.Assign(Source:TPersistent);
begin
  if Source is TChartTitle then
  With TChartLegend(Source) do
  Begin
    Self.FVisible   := FVisible;
    Self.FFont.Assign( Font );
    Self.FBrush.Assign( Brush );
    Self.FFrame.Assign( Frame );
    Self.FText.Assign( Text );
  end;

  inherited Assign(Source);
end;

procedure TChartTitle.SetVisible(value: boolean);
begin
     FVisible := value;
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartTitle.SetFont(value: TFont);
begin
     FFont.Assign( value );
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartTitle.SetFrame(value: TChartPen);
begin
     FFrame.Assign( value );
     if assigned( FChanged ) then FChanged(Self);
end;


procedure TChartTitle.SetBrush(value: TBrush);
begin
     FBrush.Assign( value );
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartTitle.SetText(value: TStrings);
begin
     FText.Assign( value );
     if assigned( FChanged ) then FChanged(Self);
end;

procedure TChartTitle.StyleChanged(Sender: TObject);
begin
     if assigned( FChanged ) then FChanged(Self);;
end;

procedure TChartTitle.SetAlignment(value: TAlignment);
begin
   FAlignment := Value;
   if assigned( FChanged ) then FChanged(Self);;
end;





procedure CalculateIntervals(Mini,Maxi:Double;var Debut,Pas:Double);
var
   Etendue,EtendueTmp:Double;
   NbPas,Mult:array[1..3] of Double;

   Index:array[1..3] of Byte;
   Trouve:Boolean;
   DTmp:Double;
   BTmp:Byte;
   i,j:Integer;
begin
if Maxi>59 then
   Sleep(1);
Etendue:=Maxi-Mini;
if Etendue=0 then begin Debut:=Mini; Pas:=1; Exit; end;

Mult[1]:=1;
EtendueTmp:=Etendue;
NbPas[1]:=EtendueTmp;
if NbPas[1]>=10 then
   begin
   while NbPas[1]>10 do
      begin
      EtendueTmp:=EtendueTmp/10;
      Mult[1]:=Mult[1]/10;
      NbPas[1]:=EtendueTmp;
      end;
   end
else
   begin
   while EtendueTmp*10<=10 do
      begin
      EtendueTmp:=EtendueTmp*10;
      Mult[1]:=Mult[1]*10;
      NbPas[1]:=EtendueTmp;
      end;
   end;

Mult[2]:=1;
EtendueTmp:=Etendue;
NbPas[2]:=EtendueTmp/0.5;
if NbPas[2]>=10 then
   begin
   while NbPas[2]>10 do
      begin
      EtendueTmp:=EtendueTmp/10;
      Mult[2]:=Mult[2]/10;
      NbPas[2]:=EtendueTmp/0.5;
      end;
   end
else
   begin
   while EtendueTmp*10/0.5<=10 do
      begin
      EtendueTmp:=EtendueTmp*10;
      Mult[2]:=Mult[2]*10;
      NbPas[2]:=EtendueTmp/0.5;
      end;
   end;

Mult[3]:=1;
EtendueTmp:=Etendue;
NbPas[3]:=EtendueTmp/0.2;
if NbPas[3]>=10 then
   begin
   while NbPas[3]>10 do
      begin
      EtendueTmp:=EtendueTmp/10;
      Mult[3]:=Mult[3]/10;
      NbPas[3]:=EtendueTmp/0.2;
      end;
   end
else
   begin
   while EtendueTmp*10/0.2<=10 do
      begin
      EtendueTmp:=EtendueTmp*10;
      Mult[3]:=Mult[3]*10;
      NbPas[3]:=EtendueTmp/0.2;
      end;
   end;

for i:=1 to 3 do Index[i]:=i;

Trouve:=True;
while Trouve do
   begin
   Trouve:=False;
   for i:=1 to 2 do
      if NbPas[i]>NbPas[i+1] then
         begin
         Trouve:=True;
         DTmp:=NbPas[i];
         NbPas[i]:=NbPas[i+1];
         NbPas[i+1]:=DTmp;
         BTmp:=Index[i];
         Index[i]:=Index[i+1];
         Index[i+1]:=BTmp;
         end;
   end;

if NbPas[3]<=10 then j:=3
else if NbPas[2]<=10 then j:=2
else if NbPas[1]<=10 then j:=1
else
   begin
//   ShowMessage(lang('Erreur'));
   Exit;
   end;

if Index[j]=1 then Pas:=1;
if Index[j]=2 then Pas:=0.5;
if Index[j]=3 then Pas:=0.2;
Pas:=Pas/Mult[Index[j]];
// If 0 is in the interval, it is cool to have it as a mark !
if (Mini<0) and (Maxi>0) then
   begin
   Debut:=0;
   while Debut>Mini do Debut:=Debut-Pas;
   end
else
   begin
   // Don't work if mini is negative and > 1
//   if Abs(Mini)<1 then
      Debut:=Round((Mini-Pas)*Mult[Index[j]])/Mult[Index[j]]
//   else
//      Debut:=System.Int(Mini)-Pas; //null
   end;
end;

constructor TChart.Create(AOwner:TComponent);
begin
inherited Create(AOwner);

    TmpBrush := TBrush.Create;
    TmpPen := TPen.Create;
    TmpFont := TFont.Create;

    FAllowZoom := True;
    FAxisVisible := true; 


Width := 400;
Height := 300;

XMarkOld:=-1;
YMarkOld:=-1;

Series:=TSeriesList.Create;

YMarkWidth:=10;

FAutoUpdateXMin:=True;
FAutoUpdateXMax:=True;
FAutoUpdateYMin:=True;
FAutoUpdateYMax:=True;

Color:=clBtnFace;
AxisColor:=clBlack;

FXGraphMax:=0;
FXGraphMin:=0;
FYGraphMax:=0;
FYGraphMin:=0;

MirrorX:=False;
Fixed:=False;
Zoom:=False;
FShowReticule:=False;
FShowVerticalReticule:=False;
FBackColor := Color;

FGraphBrush:=TBrush.Create;
FGraphBrush.OnChange:=StyleChanged;

//luis
FLegend := TChartLegend.Create(Self);
FLegend.OnChange := StyleChanged;
FTitle := TChartTitle.Create(Self);
FTitle.Alignment := taCenter;
FTitle.Visible := true;
FTitle.OnChange := StyleChanged;
FFoot := TChartTitle.Create(Self);
FFoot.Text.Clear;
FFoot.Alignment := taCenter;
FFoot.OnChange := StyleChanged;


FLeftAxis := TChartAxis.Create(Self);
FLeftAxis.Title.Angle := 90;
FLeftAxis.OnChange := StyleChanged;
FBottomAxis := TChartAxis.Create(Self);
FBottomAxis.Title.Angle := 0;
FBottomAxis.OnChange := StyleChanged;

FFrame :=  TChartPen.Create;
FFrame.Visible := true;
FFrame.OnChange := StyleChanged;

end;

destructor TChart.Destroy;
var
   MySerie:TChartSeries;
   i,c: integer;
begin
     c := FSeries.Count - 1;                 
     for i := 0 to c do  begin
         TChartSeries(FSeries.Items[0]).Free;
         FSeries.Delete( 0 );
     end;

     FSeries.Free;
     FGraphBrush.Free;

     TmpBrush.Destroy;
     TmpPen.Destroy;
     TmpFont.Destroy;

     FLegend.Destroy;
     FTitle.Destroy;
     LeftAxis.Destroy;
     BottomAxis.Destroy;
     FFrame.Destroy;

     inherited Destroy;
end;

procedure TChart.StyleChanged(Sender: TObject);
begin
     Invalidate;
end;

procedure TChart.Paint;
var i: integer;
begin
     YImageMin:=Height-20;
     YImageMax:=5;

     if FTitle.Visible then begin
       TmpFont.Assign( Canvas.Font );
       Canvas.Font.Assign( FTitle.Font );
        for i := 0 to FTitle.Text.Count -1 do begin
           YImageMax:=YImageMax+5+Canvas.TextHeight(FTitle.Text[i]);
        end;
        Canvas.Font.Assign(TmpFont);
     end;

     if FFoot.Visible then begin
        TmpFont.Assign( Canvas.Font );
        Canvas.Font.Assign( FFoot.Font );
        for i := 0 to FFoot.Text.Count -1 do begin
           YImageMin:=YImageMin-5-Canvas.TextHeight(FFoot.Text[i]);
        end;
        Canvas.Font.Assign(TmpFont);
     end;


     if FBottomAxis.Visible and FAxisVisible then begin //FIXME: fix to rotate other than 0/90/180 degres
        YImageMin:=YImageMin-Canvas.TextHeight(FBottomAxis.Title.Caption);
     end;



if FMirrorX then begin
   XImageMin:=Width-YMarkWidth-GetLegendWidth;
   XImageMax:=10;
end else begin
   if FLeftAxis.Visible and FAxisVisible  then
      XImageMin:=YMarkWidth+Canvas.TextHeight(FLeftAxis.Title.Caption)
   else
      XImageMin:=YMarkWidth;

   XImageMax:=Width-10-GetLegendWidth;
end;

Refresh;
end;

procedure TChart.Clean;
begin
Canvas.Pen.Mode:=pmCopy;
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Color:=Color;
Canvas.Brush.Color:=Color;
Canvas.Brush.Style:=bsSolid;
Canvas.Rectangle(0,0,Width,Height);
end;

procedure TChart.DrawTitleFoot;
var i: Integer;
    t, xpos: Integer;
begin
   if FTitle.Visible and (FTitle.Text.Count > 0) then begin
      TmpBrush.Assign( Canvas.Brush );
      TmpFont.Assign( Canvas.Font );

      Canvas.Brush.Assign( FTitle.Brush );
      Canvas.Font.Assign( FTitle.Font );
      t := 5;
      for i := 0 to FTitle.Text.Count -1 do begin
          case FTitle.Alignment of
               taLeftJustify:  xpos := XImageMin;
               taCenter:       xpos := (Width-Canvas.TextWidth(FTitle.Text[i])) div 2;
               taRightJustify: xpos := XImageMax - Canvas.TextWidth(FTitle.Text[i]);
          end;

         Canvas.TextOut( xpos ,t,FTitle.Text[i]);
         t := t + Canvas.TextHeight(FTitle.Text[i]);
      end;

      Canvas.Brush.Assign( TmpBrush );
      Canvas.Font.Assign( TmpFont );
  end;

  if FFoot.Visible and (FFoot.Text.Count > 0) then begin
      TmpBrush.Assign( Canvas.Brush );
      TmpFont.Assign( Canvas.Font );

      Canvas.Brush.Assign( FFoot.Brush );
      Canvas.Font.Assign( FFoot.Font );
      t := Height-5-Canvas.TextHeight(FFoot.Text[0]);
      for i := FFoot.Text.Count - 1 downto 0 do begin
          case FFoot.Alignment of
               taLeftJustify:  xpos := XImageMin;
               taCenter:       xpos := (Width-Canvas.TextWidth(FFoot.Text[i])) div 2;
               taRightJustify: xpos := XImageMax - Canvas.TextWidth(FFoot.Text[i]);
          end;

         Canvas.TextOut( xpos ,t,FFoot.Text[i]);
         t := t - Canvas.TextHeight(FFoot.Text[i]);
      end;

      Canvas.Brush.Assign( TmpBrush );
      Canvas.Font.Assign( TmpFont );
  end;
end;

procedure TChart.DrawAxis;
var
  LargTexte,MaxLargTexte,HautTexte:Integer;
  XTemp,YTemp,XPos:Integer;
  MyText:string;
  Marque,Debut,Pas:Double;
  T: Integer;
  LeftAxisWidth: Integer;
begin
   // Find max mark width
   MaxLargTexte:=0;
   Debut:=FYGraphMax;
   Pas:=1;
   CalculateIntervals(FYGraphMin,FYGraphMax,Debut,Pas);
   if FYGraphMin<>FYGraphMax then begin
      Marque:=Debut;
      while Marque<=FYGraphMax+Pas*10e-10 do begin
         if (Marque>=FYGraphMin) then begin
            YGraphToImage(Marque,YTemp);
            MyText:=Trim(FloatToStr(Marque));
            LargTexte:=Canvas.TextWidth(MyText);
            if LargTexte>MaxLargTexte then MaxLargTexte:=LargTexte;
         end;
         Marque:=Marque+Pas;
      end;
   end;

   YMarkWidth:=10;
   //only consider this width if visible
   if FLeftAxis.Visible and FAxisVisible then
      LeftAxisWidth := Canvas.TextHeight(FLeftAxis.Title.Caption) + 16
   else LeftAxisWidth := 0;

   if MaxLargTexte+LeftAxisWidth>YMarkWidth then begin
      YMarkWidth:=MaxLargTexte+LeftAxisWidth;
      if FMirrorX then begin
         XImageMin:=Width-YMarkWidth-GetLegendWidth;
         XImageMax:=10;
      end
      else begin
      XImageMin:=YMarkWidth;
      XImageMax:=Width-10-GetLegendWidth;
      end;

      // Update coefs
      if (FXGraphMax-FXGraphMin <>0) and (FYGraphMax-FYGraphMin <> 0) then begin
         ax:=(XImageMax-XImageMin)/(FXGraphMax-FXGraphMin);
         bx:=XImageMax-ax*FXGraphMax;
         ay:=(YImageMax-YImageMin)/(FYGraphMax-FYGraphMin);
         by:=YImageMax-ay*FYGraphMax;
      end;
   end;

   // Back
   Canvas.Pen.Style:=psClear;
   Canvas.Brush.Color := FBackColor;
   Canvas.Rectangle(XImageMin,YImageMin,XImageMax,YImageMax);

   // Axes
   if FFrame.Visible then begin
      Canvas.Pen.Assign( FFrame );
      Canvas.MoveTo(XImageMin,YImageMin);
      Canvas.LineTo(XImageMin,YImageMax);
      Canvas.MoveTo(XImageMin,YImageMin);
      Canvas.LineTo(XImageMax,YImageMin);
      Canvas.MoveTo(XImageMin,YImageMax);
      Canvas.LineTo(XImageMax,YImageMax);
      Canvas.MoveTo(XImageMax,YImageMin);
      Canvas.LineTo(XImageMax,YImageMax);
   end;


   // Axis Labels
   if FLeftAxis.Visible and FAxisVisible then begin
{      Canvas.Brush.Color:=Color;
      Canvas.Font.Color:=clBlack;
      }
      if FMirrorX then T := Width-Canvas.TextWidth(FLeftAxis.Title.Caption)+5
      else T := 5;
      if FTitle.Visible then
         RotateLabel(Canvas, T, YImageMin+((YImageMax-YImageMin) div 2)+(Canvas.TextWidth(FLeftAxis.Title.Caption) div 2),
                             FLeftAxis.Title.Caption, FLeftAxis.Title.Angle)
      else
         RotateLabel(Canvas, T, YImageMin+((YImageMax-YImageMin) div 2)+(Canvas.TextWidth(FLeftAxis.Title.Caption) div 2),
                             FLeftAxis.Title.Caption, FLeftAxis.Title.Angle);
   end;

   if FBottomAxis.Visible and FAxisVisible then begin
    RotateLabel(Canvas, XImageMin+((XImageMax-XImageMin) div 2)-(Canvas.TextWidth(FBottomAxis.Title.Caption) div 2) ,
                        YImageMin+5+Canvas.TextHeight(FBottomAxis.Title.Caption), FBottomAxis.Title.Caption, FBottomAxis.Title.Angle);
   end;

   // X graduations
   if FBottomAxis.Visible and FAxisVisible then begin
      Debut:=FXGraphMax;
      Pas:=1;
      CalculateIntervals(FXGraphMin,FXGraphMax,Debut,Pas);
      if FXGraphMin<>FXGraphMax then begin
         Marque:=Debut;
         while Marque<=FXGraphMax+Pas*10e-10 do begin
            if (Marque>=FXGraphMin) then begin
               XGraphToImage(Marque,XTemp);
               Canvas.Brush.Assign(FGraphBrush);
               if FBottomAxis.Grid.Visible then begin
                  Canvas.Pen.Assign(FBottomAxis.Grid);
                  if (XTemp<>XImageMax) and (XTemp<>XImageMin) then begin
                     Canvas.MoveTo(XTemp,YImageMin);
                     Canvas.LineTo(XTemp,YImageMax);
                  end;
               end;
               Canvas.Pen.Color:=AxisColor;
               Canvas.Pen.Style:=psSolid;
               Canvas.Pen.Mode:=pmCopy;
               Canvas.MoveTo(XTemp,YImageMin-4);
               Canvas.LineTo(XTemp,YImageMin+4);
               Canvas.Brush.Color:=Color;
               MyText:=Trim(FloatToStr(Marque));
               LargTexte:=Canvas.TextWidth(MyText) div 2;
               XPos:=XTemp-LargTexte;
               if XPos<1 then Xpos:=1;
               if XPos+LargTexte*2>Width then Xpos:=Width-LargTexte*2-1;
               Canvas.TextOut(Xpos,YImageMin+4,MyText);
            end;
            Marque:=Marque+Pas;
         end;
      end;
   end;

   // Y graduations
   if FLeftAxis.Visible and AxisVisible then begin
      MaxLargTexte:=0;
      Debut:=FYGraphMax;
      Pas:=1;
      CalculateIntervals(FYGraphMin,FYGraphMax,Debut,Pas);
      if FYGraphMin<>FYGraphMax then begin
         Marque:=Debut;
         while Marque<=FYGraphMax+Pas*10e-10 do begin
            if (Marque>=FYGraphMin) then begin
               YGraphToImage(Marque,YTemp);
               Canvas.Brush.Assign(FGraphBrush);
               //draw grid
               if FBottomAxis.Grid.Visible then begin
                  Canvas.Pen.Assign(FBottomAxis.Grid);
                  if (YTemp<>YImageMax) and (YTemp<>YImageMin) then begin
                     Canvas.MoveTo(XImageMin,YTemp);
                     Canvas.LineTo(XImageMax,YTemp);
                  end;
               end;
               Canvas.Pen.Color:=AxisColor;
               Canvas.Pen.Style:=psSolid;
               Canvas.Pen.Mode:=pmCopy;
               Canvas.MoveTo(XImageMin-4,YTemp);
               Canvas.LineTo(XImageMin+4,YTemp);
               Canvas.Brush.Color:=Color;
               MyText:=Trim(FloatToStr(Marque));
               LargTexte:=Canvas.TextWidth(MyText);
               if LargTexte>MaxLargTexte then MaxLargTexte:=LargTexte;
               HautTexte:=Canvas.TextHeight(MyText) div 2;
               if FMirrorX then
                  Canvas.TextOut(XImageMin+6,YTemp-HautTexte,MyText)
               else
                  Canvas.TextOut(XImageMin-7-LargTexte,YTemp-HautTexte,MyText);
            end;
            Marque:=Marque+Pas;
         end;
      end;
   end;
end;

procedure TChart.DrawLegend;
var
   w,h,x1,y1,x2,y2,i,TH:Integer;
   MySerie:TSerie;

begin
   TmpBrush.Assign(Canvas.Brush);
   TmpPen.Assign(Canvas.Pen);
   TmpFont.Assign(Canvas.Font);

   w:=GetLegendWidth;
   TH:=Canvas.TextHeight('I');

   if only_pie then begin//if only one pie show diferent legend
      MySerie := get_pie;
      h:=5+MySerie.Count*(TH+5);
   end else begin
      h:=5+SeriesCount*(TH+5);
   end;
   x1:=Width-w-5;
   y1 := YImageMax;
   x2:=x1+w;
   y2:=y1+h;

   // Border
   Canvas.Brush.Assign(FGraphBrush);
   Canvas.Pen.Assign( FLegend.Frame );
   Canvas.Font.Assign( FLegend.Font );
   Canvas.Rectangle(x1,y1,x2,y2);

   // Lines and Series titles
   if only_pie then begin//if only one pie show diferent legend
      MySerie := get_pie;
      for i := 0 to MySerie.Count - 1 do begin //clean this coord shoould not be published
         Canvas.Pen.Color := FLegend.Frame.Color;
         Canvas.Brush.Color := FGraphBrush.Color;
         Canvas.TextOut(x1+25,y1+5+i*(TH+5), format('%1.2g',[PChartCoord(MySerie.Coord.items[i])^.y])+' '+PChartCoord(MySerie.Coord.items[i])^.Text);
         Canvas.Pen.Color := clBlack;
         Canvas.Brush.Color := PChartCoord(MySerie.Coord.items[i])^.Color;
         Canvas.Rectangle(x1+5,y1+i*(TH+5)+TH div 2, x1+22,y1+10+i*(TH+5)+TH div 2);
      end;
   end else begin
      for i:=0 to SeriesCount-1 do begin
         MySerie:=Series[i];
         Canvas.TextOut(x1+20,y1+5+i*(TH+5),MySerie.Title);
         Canvas.Pen.Color := MySerie.SeriesColor;
         Canvas.MoveTo(x1+5,y1+5+i*(TH+5)+TH div 2);
         Canvas.LineTo(x1+15,y1+5+i*(TH+5)+TH div 2);
      end;
   end;


   Canvas.Brush.Assign(TmpBrush);
   Canvas.Pen.Assign(TmpPen);
   Canvas.Font.Assign(TmpFont);
end;

procedure TChart.SetAutoUpdateXMin(Value:Boolean);
begin
FAutoUpdateXMin:=Value;
end;

procedure TChart.SetAutoUpdateXMax(Value:Boolean);
begin
FAutoUpdateXMax:=Value;
end;

procedure TChart.SetAutoUpdateYMin(Value:Boolean);
begin
FAutoUpdateYMin:=Value;
end;

procedure TChart.SetAutoUpdateYMax(Value:Boolean);
begin
FAutoUpdateYMax:=Value;
end;

procedure TChart.SetXGraphMin(Value:Double);
begin
FXGraphMin:=Value;
Invalidate;
end;

procedure TChart.SetYGraphMin(Value:Double);
begin
FYGraphMin:=Value;
Invalidate;
end;

procedure TChart.SetXGraphMax(Value:Double);
begin
FXGraphMax:=Value;
Invalidate;
end;

procedure TChart.SetYGraphMax(Value:Double);
begin
FYGraphMax:=Value;
Invalidate;
end;

procedure TChart.SetMirrorX(Value:Boolean);
begin
if Value<>FMirrorX then
   begin
   if FMirrorX then
      begin
      XImageMin:=YMarkWidth;
      XImageMax:=Width-10-GetLegendWidth;
      FMirrorX:=False;
      end
   else
      begin
      XImageMin:=Width-YMarkWidth-GetLegendWidth;
      XImageMax:=10;
      FMirrorX:=True;
      end;
   Invalidate;
   end;
end;

procedure TChart.SetTitle(Value:TChartTitle);
begin
     FTitle.Assign( Value );
     Invalidate;
end;

procedure TChart.SetFoot(value: TChartTitle);
begin
     FFoot.Assign(Value);
     Invalidate;
end;


function TChart.GetLegendWidth:Integer;
var
   i,j,k:Integer;
   MySerie:TSerie;
begin
if not FLegend.Visible then begin Result:=0; Exit; end;

   if only_pie then begin//if only one pie show diferent legend
      MySerie := get_pie;
      j := 0;
      for i := 0 to MySerie.Count - 1 do begin //clean this coord shoould not be published
         k := Canvas.TextWidth( format('%1.2g',[PChartCoord(MySerie.Coord.items[i])^.y])+' '+PChartCoord(MySerie.Coord.items[i])^.Text) ;
         if k>j then j:=k;
      end;
      Result:=j+20+10;
   end else begin
      j:=0;
      for i:=0 to SeriesCount-1 do begin
         MySerie:=Series[i];
         k:=Canvas.TextWidth(MySerie.Title);
         if k>j then j:=k;
      end;
      Result:=j+20+10;
   end;
end;

procedure TChart.SetGraphBrush(Value:TBrush);
begin
showmessage('ad');
FGraphBrush.Assign(Value);
end;

procedure TChart.AddSerie(Serie:TComponent);
begin
if FShowVerticalReticule then
   DrawVerticalReticule(XMarkOld);
if FShowReticule then
   DrawReticule(XMarkOld,YMarkOld);

//disable axis when we ave tpie series
if Serie is TPieSeries then begin
   LeftAxis.Visible := False;
   BottomAxis.Visible := False;
end;



Series.Add(Serie);
TChartSeries(Serie).Chart := Self;
end;

//procedure TChart.DeleteSerie(Serie:TTASerie);
procedure TChart.DeleteSerie(Serie:TComponent);
var
   i:Integer;
   MySerie:TComponent;
begin
i:=0;
while i< SeriesCount do
   begin
   MySerie:=Series[i];
   if Serie=MySerie then
      begin
      Series.Delete(i);
      Invalidate;
      end
   else Inc(i);
   end;
end;

function TChart.GetSerie(i:Integer):TComponent;
begin
Result:=Series[i];
end;

procedure TChart.SetAutoXMin(Auto:Boolean);
begin
FAutoUpdateXMin:=Auto;
Refresh;
end;

procedure TChart.SetAutoXMax(Auto:Boolean);
begin
FAutoUpdateXMax:=Auto;
Refresh;
end;

procedure TChart.SetAutoYMin(Auto:Boolean);
begin
FAutoUpdateYMin:=Auto;
Refresh;
end;

procedure TChart.SetAutoYMax(Auto:Boolean);
begin
FAutoUpdateYMax:=Auto;
Refresh;
end;

procedure TChart.Refresh;
var
   Tolerance,Valeur:Double;
   i:Integer;
   NBPointsMax:Integer;
   Serie:TChartSeries;
   XMinSeries,XMaxSeries,YMinSeries,YMaxSeries:Double;
begin
   if FShowVerticalReticule then
      DrawVerticalReticule(XMarkOld);
   if FShowReticule then
      DrawReticule(XMarkOld,YMarkOld);

   // Search # of points, min and max of all series
   if Zoom then begin
      Zoom:=False;
      Fixed:=True;
      XImageToGraph(ZoomRect.Left,FXGraphMin);
      XImageToGraph(ZoomRect.Right,FXGraphMax);
      YImageToGraph(ZoomRect.Bottom,FYGraphMin);
      YImageToGraph(ZoomRect.Top,FYGraphMax);
   end
   else if not Fixed then begin
      XMinSeries:=MaxDouble;
      XMaxSeries:=MinDouble;
      YMinSeries:=MaxDouble;
      YMaxSeries:=MinDouble;
      NBPointsMax:=0;
      for i:=0 to Series.Count-1 do begin
         Serie := Series[i];
         if Serie.Active and (TChartSeries(Serie).Count>0) then begin
            NBPointsMax:=NBPointsMax+TChartSeries(Serie).Count;
            if TChartSeries(Serie).XGraphMin<XMinSeries then XMinSeries:=TChartSeries(Serie).XGraphMin;
            if TChartSeries(Serie).YGraphMin<YMinSeries then YMinSeries:=TChartSeries(Serie).YGraphMin;
            if TChartSeries(Serie).XGraphMax>XMaxSeries then XMaxSeries:=TChartSeries(Serie).XGraphMax;
            if TChartSeries(Serie).YGraphMax>YMaxSeries then YMaxSeries:=TChartSeries(Serie).YGraphMax;
         end;
      end;
      if XMinSeries>MaxDouble/10 then XMinSeries:=0;
      if YMinSeries>MaxDouble/10 then YMinSeries:=0;
      if XMaxSeries<MinDouble/10 then XMaxSeries:=0;
      if YMaxSeries<MinDouble/10 then YMaxSeries:=0;

      if YMaxSeries = YMinSeries then begin
         YMaxSeries := YMaxSeries + 1;
         YMinSeries := YMinSeries - 1;
      end;
      if XMaxSeries = XMinSeries then begin
         XMaxSeries := XMaxSeries + 1;
         XMinSeries := XMinSeries - 1;
      end;


      // Image coordinates calculation
      // Update max in graph
      // If one point : +/-10% of the point coordinates
      Tolerance:=0.001; //LUIS ISTO e' o EXTRA
//      Tolerance:=0.1; //LUIS ISTO e' o EXTRA

   if NBPointsMax > 0 then
   // If several points : automatic +/-10% of interval
      begin
      Valeur:=Tolerance*(XMaxSeries-XMinSeries); //podemos acabar c esta tolerancia
      if Valeur<>0 then
         begin
         if FAutoUpdateXMin then FXGraphMin:=XMinSeries-Valeur;
         if FAutoUpdateXMax then FXGraphMax:=XMaxSeries+Valeur;
         end
      else
         begin
         if FAutoUpdateXMin then FXGraphMin:=XMinSeries-1;
         if FAutoUpdateXMax then FXGraphMax:=XMaxSeries+1;
         end;
      Valeur:=Tolerance*(YMaxSeries-YMinSeries);
      if Valeur<>0 then
         begin
         if FAutoUpdateYMin then FYGraphMin:=YMinSeries-Valeur;
         if FAutoUpdateYMax then FYGraphMax:=YMaxSeries+Valeur;
         end
      else
         begin
         if FAutoUpdateYMin then FYGraphMin:=YMinSeries-1;
         if FAutoUpdateYMax then FYGraphMax:=YMinSeries+1;
         end;
      end
   else
   // 0 Points
      begin
      FXGraphMin:=0;
      FXGraphMax:=0;
      FYGraphMin:=0;
      FYGraphMax:=0;
      end;
   end;

// Image <-> Graph coeff calculation
if FXGraphMax<>FXGraphMin then
   begin
   ax:=(XImageMax-XImageMin)/(FXGraphMax-FXGraphMin);
   bx:=XImageMax-ax*FXGraphMax;
   end
else
   begin
   ax:=1;
   bx:=0;
   end;
if FYGraphMax<>FYGraphMin then
   begin
   ay:=(YImageMax-YImageMin)/(FYGraphMax-FYGraphMin);
   by:=YImageMax-ay*FYGraphMax;
   end
else
   begin
   ay:=1;
   by:=0;
   end;

Clean;
DrawAxis;
DisplaySeries;
DrawTitleFoot;
if FLegend.Visible then DrawLegend;

if FShowVerticalReticule then
   DrawVerticalReticule(XMarkOld);
if FShowReticule then
   DrawReticule(XMarkOld,YMarkOld);
end;

procedure TChart.XGraphToImage(Xin:Double;var XOut:Integer);
begin
XOut:=Round(ax*XIn+bx);
end;

procedure TChart.YGraphToImage(Yin:Double;var YOut:Integer);
begin
YOut:=Round(ay*YIn+by);
end;

procedure TChart.GraphToImage(Xin,Yin:Double;var XOut,YOut:Integer);
begin
XGraphToImage(Xin,XOut);
YGraphToImage(Yin,YOut);
end;

procedure TChart.XImageToGraph(XIn:Integer;var XOut:Double);
begin
XOut:=(XIn-bx)/ax;
end;

procedure TChart.YImageToGraph(YIn:Integer;var YOut:Double);
begin
YOut:=(YIn-by)/ay;
end;

procedure TChart.ImageToGraph(XIn,YIn:Integer;var XOut,YOut:Double);
begin
   XImageToGraph(XIn,XOut);
   YImageToGraph(YIn,YOut);
end;

procedure TChart.DisplaySeries;
var
   i:Integer;
   Serie:TChartSeries;
begin
     // Update all series
     for i:=0 to FSeries.Count-1 do begin
         Serie:= TChartSeries( Series[i] );
         if Serie.Active then  Serie.Draw;
     end;
end;

procedure TChart.SetShowVerticalReticule(Value:Boolean);
begin
   if FShowVerticalReticule then begin
      DrawVerticalReticule(XMarkOld);
      FShowVerticalReticule:=False;
   end;
   FShowVerticalReticule:=Value;
   Invalidate;
end;

procedure TChart.SetShowReticule(Value:Boolean);
begin
   if Value=False then
      DrawReticule(XMarkOld,YMarkOld);
FShowReticule:=Value;
Invalidate;
end;

procedure TChart.GetPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
var
   j,k,XPoint,YPoint,SerieNumber,PointNumber:Integer;
   Mini,Dist,Xg,Yg,XgOut,YgOut:Double;
   Serie:TComponent;
   TASerie:TSerie;
   T1,T2:Double;
begin
Mini:=MaxDouble;
for SerieNumber:=0 to Series.Count-1 do
   begin
   Serie:=Series[SerieNumber];
   if Serie is TSerie then
      begin
      TASerie:=TSerie(Serie);
      for PointNumber:=0 to TASerie.Count-1 do
         begin
         XPoint:=TASerie.GetXImgValue(PointNumber);
         YPoint:=TASerie.GetYImgValue(PointNumber);
         T1:=X-XPoint;
         T2:=Y-YPoint;
         Dist:=Sqrt(Sqr(T1)+Sqr(T2));
         if Dist<=Mini then
            begin
            Mini:=Dist;
            SerieNumberOut:=SerieNumber;
            PointNumberOut:=PointNumber;
            XOut:=XPoint;
            YOut:=YPoint;
            XgOut:=TASerie.GetXValue(PointNumber);
            YgOut:=TASerie.GetYValue(PointNumber);
            end;
         end;
      if SerieNumberOut=SerieNumber then DoDrawReticule(SerieNumberOut,PointNumberOut,XOut,YOut,XgOut,YgOut);
      end;
   end;
end;

procedure TChart.GetXPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
var
   j,k,XPoint,YPoint,SerieNumber,PointNumber:Integer;
   Mini,Dist,Xg,Yg:Double;
   Serie:TComponent;
   TASerie:TSerie;
begin
Mini:=MaxDouble;
SerieNumberOut:=-1;
for SerieNumber:=0 to Series.Count-1 do
   begin
   Serie:=Series[SerieNumber];
   if Serie is TSerie then
      begin
      TASerie:=TSerie(Serie);
      for PointNumber:=0 to TASerie.Count-1 do
         begin
         XPoint:=TASerie.GetXImgValue(PointNumber);
         Dist:=Abs(X-XPoint);
         if Dist<=Mini then
            begin
            Mini:=Dist;
            SerieNumberOut:=SerieNumber;
            PointNumberOut:=PointNumber;
            XOut:=XPoint;
            YOut:=TASerie.GetYImgValue(PointNumber);
            Xg:=TASerie.GetXValue(PointNumber);
            Yg:=TASerie.GetYValue(PointNumber);
            end;
         end;
      if SerieNumberOut=SerieNumber then DoDrawVertReticule(SerieNumberOut,PointNumberOut,XOut,YOut,Xg,Yg);
      end;
   end;
end;

procedure TChart.GetYPointNextTo(X,Y:Integer;var SerieNumberOut,PointNumberOut,XOut,YOut:Integer);
var
   j,k,XPoint,YPoint,SerieNumber,PointNumber:Integer;
   Mini,Dist,Xg,Yg:Double;
   Serie:TComponent;
   TASerie:TSerie;
begin
Mini:=MaxDouble;
for SerieNumber:=0 to Series.Count-1 do
   begin
   Serie:=Series[SerieNumber];
   if Serie is TSerie then
      begin
      TASerie:=TSerie(Serie);
      for PointNumber:=0 to TASerie.Count-1 do
         begin
         YPoint:=TASerie.GetYImgValue(PointNumber);
         Dist:=Abs(Y-YPoint);
         if Dist<=Mini then
            begin
            Mini:=Dist;
            SerieNumberOut:=SerieNumber;
            PointNumberOut:=PointNumber;
            XOut:=XPoint;
            YOut:=YPoint;
            end;
         end;
      end;
   end;
end;

procedure TChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     if (X < XImageMax) and (X > XImageMin) and (Y < YImageMin) and (Y > YImageMax)
        and FAllowZoom then begin
        Down:=True;
        XDown:=X;
        YDown:=Y;
        XOld:=X;
        YOld:=Y;
     end;
end;

procedure TChart.DrawReticule(X,Y:Integer);
begin
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Mode:=pmXor;
Canvas.Pen.Color:=ClWhite;
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Width:=1;

Canvas.MoveTo(X,YImageMin);
Canvas.LineTo(X,YImageMax);
Canvas.MoveTo(XImageMin,Y);
Canvas.LineTo(XImageMax,Y);
end;

procedure TChart.DrawVerticalReticule(X:Integer);
begin
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Mode:=pmXor;
Canvas.Pen.Color:=ClWhite;
Canvas.Pen.Style:=psSolid;
Canvas.Pen.Width:=1;

Canvas.MoveTo(X,YImageMin);
Canvas.LineTo(X,YImageMax);
end;

procedure TChart.MouseMove(Shift: TShiftState; X, Y: Integer);
var
   i,SerieNumber,PointNumber,XMin,Xmax,YMin,YMax,Temp:Integer;
   MySerie:TSerie;
begin
if Down then
   begin
   Canvas.Brush.Style:=bsClear;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Mode:=pmXor;
   Canvas.Pen.Color:=ClWhite;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Width:=1;

   Canvas.Rectangle(XDown,YDown,XOld,YOld);
   Canvas.Rectangle(XDown,YDown,X,Y);

   XOld:=X;
   YOld:=Y;
   end
else
   begin
   XMin:=XImageMin;
   XMax:=XImageMax;
   YMin:=YImageMin;
   YMax:=YImageMax;
   if XMin>XMax then
      begin
      Temp:=XMin;
      XMin:=XMax;
      XMax:=Temp;
      end;
   if YMin>YMax then
      begin
      Temp:=YMin;
      YMin:=YMax;
      YMax:=Temp;
      end;

   for i:=0 to SeriesCount-1 do
      begin
      MySerie:=Series[i];
      if FShowVerticalReticule then
         begin
         GetXPointNextTo(X,Y,SerieNumber,PointNumber,XReticule,YReticule);
         if (XReticule<>XMarkOld) and (XReticule>XMin) and (XReticule<XMax) then
            begin
            DrawVerticalReticule(XMarkOld);
            DrawVerticalReticule(XReticule);
            FShowVerticalReticule:=True;
            XMarkOld:=XReticule;
            YMarkOld:=YReticule;
            end;
         end;
      if FShowReticule then
         begin
         GetPointNextTo(X,Y,SerieNumber,PointNumber,XReticule,YReticule);
         if (XReticule<>XMarkOld) or (YReticule<>YMarkOld) then
            if (XReticule>=XMin) and (XReticule<=XMax) and (YReticule>=YMin) and (YReticule<=YMax) then
               begin
               DrawReticule(XMarkOld,YMarkOld);
               DrawReticule(XReticule,YReticule);
               FShowReticule:=True;
               XMarkOld:=XReticule;
               YMarkOld:=YReticule;
               end;
         end;
      end;
   end;
end;

procedure TChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if Down then
   begin
   XMarkOld:=X;
   YMarkOld:=Y;

   Canvas.Brush.Style:=bsClear;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Mode:=pmXor;
   Canvas.Pen.Color:=ClWhite;
   Canvas.Pen.Style:=psSolid;
   Canvas.Pen.Width:=1;

   Canvas.Rectangle(XDown,YDown,XOld,YOld);

   Down:=False;
   if (XDown<XOld) and (YDown<YOld) then
      begin
      Zoom:=True;
      end
   else
      begin
      Zoom:=False;
      Fixed:=False;
      end;
   if XDown<XOld then
      begin
      ZoomRect.Left:=XDown;
      ZoomRect.Right:=XOld;
      end
   else
      begin
      ZoomRect.Left:=XOld;
      ZoomRect.Right:=XDown;
      end;
   if YDown<YOld then
      begin
      ZoomRect.Bottom:=YOld;
      ZoomRect.Top:=YDown;
      end
   else
      begin
      ZoomRect.Bottom:=YDown;
      ZoomRect.Top:=YOld;
      end;

   Invalidate;
   end;
end;

procedure TChart.DoDrawVertReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double);
begin
if Assigned(FDrawVertReticule) then FDrawVertReticule(Self,IndexSerie,Index,Xi,Yi,Xg,Yg);
end;

procedure TChart.DoDrawReticule(IndexSerie,Index,Xi,Yi:Integer;Xg,Yg:Double);
begin
if Assigned(FDrawReticule) then FDrawReticule(Self,IndexSerie,Index,Xi,Yi,Xg,Yg);
end;

function TChart.GetNewColor:TColor;
var
   i,j:Integer;
   MySerie:TSerie;
   ColorFound:Boolean;
begin
for i:=1 to MaxColor do
   begin
   ColorFound:=False;
   for j:=0 to SeriesCount-1 do
      begin
      MySerie:=Series[j];
      if MySerie.GetColor(0)=Colors[i] then
         ColorFound:=True;
      end;
   if not ColorFound then
      begin
      Result:=Colors[i];
      Exit;
      end;
   end;
Randomize;
Result:=RGB(Random(255),Random(255),Random(255));
end;

procedure TChart.SetLegend(Value:TChartLegend);
begin
  FLegend.Assign(Value);
  Invalidate;
end;

procedure TChart.SetLeftAxis(Value:TChartAxis);
begin
  FLeftAxis.Assign(Value);
  Invalidate;
end;

procedure TChart.SetBottomAxis(Value:TChartAxis);
begin
  FBottomAxis.Assign(Value);
  Invalidate;
end;

procedure TChart.SetFrame(Value: TChartPen);
begin
   FFrame.Assign(Value);
   Invalidate;
end;

procedure TChart.SetBackColor(Value: TColor);
begin
   FBackColor := Value;
   Invalidate;
end;

procedure TChart.SetAxisVisible(Value: boolean);
begin
   FAxisVisible := Value;
   Invalidate;
end;

function TChart.GetChartHeight: integer;
begin
      result := YImageMax - YImageMin;
end;

function TChart.GetChartWidth: integer;
begin
     result := XImageMax - XImageMin;
end;

function TChart.GetSeriesCount: integer;
var i: integer;
begin
     result := 0;
     for i := 0 to FSeries.count -1 do
         if TChartSeries(FSeries.Items[i]).Active then inc(Result);
end;



/////////////////////////UTILS.... clean a bit
   //checks if only a pie chart is enabled
   function TChart.only_pie: boolean;
   var i: integer;
   begin
        if FSeries.count > 0 then result := true
        else result := false;
        for i := 0 to FSeries.count -1  do begin
            if ( not (TChartSeries(Series.Items[i])  is TPieSeries)) and
               TChartSeries(FSeries.Items[i]).Active then begin
                  result := false;
                  break;
            end;
        end;
   end;

   //get enabled pie chart
   function TChart.get_pie: pointer;
   var i: integer;
   begin
        result := nil;
        for i := 0 to FSeries.count -1  do begin
            if ( (TChartSeries(Series.Items[i])  is TPieSeries)) and
               TChartSeries(FSeries.Items[i]).Active then begin
                  result := TChartSeries(Series.Items[i]) ;
                  break;
            end;
        end;
   end;




procedure Register;
begin
  RegisterComponents('Additional', [TChart]);
end;

{$IFDEF fpc}
initialization
  {$I tagraph.lrs}
{$ENDIF}
end.
