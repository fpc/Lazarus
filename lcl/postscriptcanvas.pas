{
 /***************************************************************************
                                PostScriptCanvas.pas
                                ------------
                         PostScript Printer Canvas object

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Olivier Guilbaud

  Informations :
     - Green Book Listing 9-1, on page 138 for Pattrens
     - PostScriptPrinter unit of Tony Maro
     - Piddle Project (Python language)
     - Internet PostScript forums
     
  Warnings :
     - Draw and StretchDraw it's slow for big image
     - Angles it's 1/16 of degre
  ToDo :
     - Implemente few methods
}

{
12 December 2012     
TextRect  implemented     T. P. Launchbury 
} 

{$DEFINE ASCII85}

unit PostScriptCanvas;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, strutils, Math, Types, FPImage,
  // LCL
  Graphics, Forms, GraphMath, GraphType, IntfGraphics, Printers,
  LCLType, LCLIntf, LCLProc, PostScriptUnicode,
  // LazUtils
  LazFileUtils, IntegerList, LazUTF8, LazUTF8Classes;
  
Type

  { TPostScriptPrinterCanvas }
  TpsPoint=record
    fx,fy:single;
  end;
  TpsBounds=record
    fx,fy,fwidth,fheight:single;
  end;

  TPsCanvasState = ( pcsPosValid, pcsClipping, pcsClipSaved );
  TPsCanvasStatus = set of TPsCanvasState;

  TPostScriptPrinterCanvas = Class(TFilePrinterCanvas)
  private
    fHeader        : TStringList; //Header document
    fDocument      : TstringList; //Current document

    fBuffer        : TStringList; //PostScript temporary buffer

    //Current values
    fcBrushStyle   : TBrushStyle;
    fcPenColor     : TColor;      //Color of Pen and Brush
    fcPenWidth     : Integer;
    fcPenStyle     : TPenStyle;
    FPsUnicode     : TPSUnicode;
    FFs            : TFormatSettings;
    fSaveCount     : Integer;
    FLazClipRect   : TRect;
    FStatus        : TPsCanvasStatus;

    procedure psDrawRect(ARect:TRect);
    procedure WriteHeader(St : String);
    procedure Write(const St : String; Lst : TStringList = nil); overload;
    procedure WriteB(const St : string);
    procedure ClearBuffer;
    procedure Write(Lst : TStringList); overload;
    procedure WriteComment(const St : string);
    procedure WritePageTransform;
    procedure WriteOrientation(UseHeader: boolean);
    procedure WriteBoundingBox(UseHeader: boolean);
    
    function TranslateCoord(cnvX,cnvY : Integer):TpsPoint;
    function TxRectToBounds(aRect: TRect): TpsBounds;
    procedure SetPosition(X,Y : Integer);
    
    procedure UpdateLineWidth;
    procedure UpdateLineColor(aColor : TColor = clNone);
    procedure UpdateLineStyle;
    procedure UpdateFillColor;
    procedure UpdateFont;
    function MappedFontName: string;
    
    procedure MoveToLastPos;
    procedure SetBrushFillPattern(Lst : TStringList; SetBorder,SetFill : Boolean);
    procedure SetBrushFillPattern(SetBorder,SetFill : Boolean); overload;
    
    procedure GetRGBImage(SrcGraph: TGraphic; Lst : TStringList);
    procedure PixelsToPoints(const PixX,PixY: Integer; out PtX,PtY:Single);
    function  GetFontSize: Integer;
    procedure RestoreClip;
    procedure SaveClip;
    procedure CheckLastPos;
    function  GetFontIndex: Integer;
    function  FontUnitsToPixelsX(const Value:Integer): Integer;
    function  FontUnitsToPixelsY(const Value:Integer): Integer;
    function  FontUnitsToPixelsY(const Value:Double): Integer;
  protected
    procedure CreateHandle; override;
    procedure CreateBrush; override;
    procedure CreateFont; override;
    procedure CreatePen; override;
    procedure CreateRegion; override;
    procedure DeselectHandles; override;
    procedure PenChanging(APen: TObject); override;
    procedure FontChanging(APen: TObject); override;
    procedure BrushChanging(APen: TObject); override;
    procedure RegionChanging(APen: TObject); override;
    procedure RequiredState(ReqState: TCanvasState); override;
    procedure DoEllipseAndFill(const Bounds: TRect); override;
    procedure RealizeAntialiasing; override;

    function GetClipRect: TRect; override;
    procedure SetClipRect(const ARect: TRect); override;
    function GetClipping: Boolean; override;
    procedure SetClipping(const AValue: boolean); override;
    
    procedure DoMoveTo(X1,Y1: Integer); override;
    procedure DoLineTo(X1,Y1: Integer); override;
  public
    constructor Create(APrinter : TPrinter); override;
    destructor Destroy; override;
    procedure BeginDoc; override;
    procedure EndDoc;   override;
    procedure NewPage;  override;

    procedure SaveToFile(aFileName : string);

    procedure Polyline(Points: PPoint; NumPts: Integer); override;
    procedure PolyBezier(Points: PPoint; NumPts: Integer;
                         Filled: boolean = False;
                         Continuous: boolean = False); override;

    procedure Rectangle(X1,Y1,X2,Y2: Integer); override;
    procedure Frame(const ARect: TRect); override; // border using pen
    procedure FrameRect(const ARect: TRect); override; // border using brush

    procedure FillRect(const ARect: TRect); override;
    procedure RoundRect(X1, Y1, X2, Y2: Integer; RX,RY: Integer); override;
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      Winding: boolean = False); override;

    procedure Ellipse(x1, y1, x2, y2: Integer); override;
    procedure Arc(Left,Top,Right,Bottom,angle1,angle2: Integer); override;
    procedure RadialPie(Left,Top,Right,Bottom,angle1,angle2: Integer); override;
    procedure Chord(x1, y1, x2, y2, angle1, angle2: Integer); override;

    procedure TextOut(X,Y: Integer; const Text: String); override;
    function TextExtent(const Text: string): TSize; override;
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
                       const Style: TTextStyle); override;

    procedure Draw(X,Y: Integer; SrcGraphic: TGraphic); override;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); override;

    function  GetTextMetrics(out TM: TLCLTextMetric): boolean; override;

    //** Methods not definined on PostScript
    procedure FloodFill(X, Y: Integer; FillColor: TColor; FillStyle: TFillStyle); override;
    procedure CopyRect(const Dest: TRect; SrcCanvas: TCanvas; const Source: TRect); override;

    //** Methods not implemented
    procedure Arc(x,y,Right,Bottom,SX,SY,EX,EY: Integer); override;
    procedure Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer); override;
    procedure Frame3d(var ARect: TRect; const FrameWidth: integer;
                      const Style: TGraphicsBevelCut); override;
    procedure Pie(EllipseX1,EllipseY1,EllipseX2,EllipseY2,
                  StartX,StartY,EndX,EndY: Integer); override;
    procedure SetPixel(X,Y: Integer; Value: TColor); override;


  end;

  TPostScriptCanvas = Class(TPostScriptPrinterCanvas)
  public
    constructor Create; overload;
    
    procedure BeginDoc; override;
    procedure EndDoc;   override;
    procedure NewPage;  override;
  end;
  
implementation
Type
  TFontsWidths = Array[32..255] of Integer;
  TFontPSMetrics = Record
    Name   : string;
    ULPos, ULThickness, Ascender, Descender: Integer;
    Widths : TFontsWidths;
  end;

Const
  cFontPSMetrics : Array[0..12] of TFontPSMetrics =(
    (Name  : 'Courier';
     ULPos : -100; ULThickness : 50; Ascender : 604; Descender : -186;
     Widths:  (600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600)
     ),
    (Name  : 'Courier-Bold';
     ULPos : -100; ULThickness : 50; Ascender : 624; Descender : -205;
     Widths:  (600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600)
     ),
    (Name  : 'Courier-Oblique';
     ULPos : -100; ULThickness : 50; Ascender : 604; Descender : -186;
     Widths:  (600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600)
     ),
    (Name  : 'Courier-BoldOblique';
     ULPos : -100; ULThickness : 50; Ascender : 624; Descender : -205;
     Widths:  (600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600, 600, 600, 600, 600,
               600, 600, 600, 600, 600, 600)
     ),
    (Name  : 'Helvetica';
     ULPos : -151; ULThickness : 50; Ascender : 729; Descender : -218;
     Widths:  (278, 278, 355, 556, 556, 889, 667, 191,
               333, 333, 389, 584, 278, 333, 278, 278, 556, 556,
               556, 556, 556, 556, 556, 556, 556, 556, 278, 278,
               584, 584, 584, 556, 1015, 667, 667, 722, 722, 667,
               611, 778, 722, 278, 500, 667, 556, 833, 722, 778,
               667, 778, 722, 667, 611, 722, 667, 944, 667, 667,
               611, 278, 278, 278, 469, 556, 333, 556, 556, 500,
               556, 556, 278, 556, 556, 222, 222, 500, 222, 833,
               556, 556, 556, 556, 333, 500, 278, 556, 500, 722,
               500, 500, 500, 334, 260, 334, 584, 278, 278, 278,
               278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
               278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
               278, 278, 278, 278, 278, 278, 278, 278, 278, 278,
               278, 333, 556, 556, 556, 556, 260, 556, 333, 737,
               370, 556, 584, 333, 737, 333, 400, 584, 333, 333,
               333, 556, 537, 278, 333, 333, 365, 556, 834, 834,
               834, 611, 667, 667, 667, 667, 667, 667, 1000, 722,
               667, 667, 667, 667, 278, 278, 278, 278, 722, 722,
               778, 778, 778, 778, 778, 584, 778, 722, 722, 722,
               722, 667, 667, 611, 556, 556, 556, 556, 556, 556,
               889, 500, 556, 556, 556, 556, 278, 278, 278, 278,
               556, 556, 556, 556, 556, 556, 556, 584, 611, 556,
               556, 556, 556, 500, 556, 500)
     ),
    (Name  : 'Helvetica-Bold';
     ULPos : -155; ULThickness : 69; Ascender : 729; Descender : -218;
     Widths: (278, 333, 474, 556, 556, 889, 722, 238,
              333, 333, 389, 584, 278, 333, 278, 278, 556, 556,
              556, 556, 556, 556, 556, 556, 556, 556, 333, 333,
              584, 584, 584, 611, 975, 722, 722, 722, 722, 667,
              611, 778, 722, 278, 556, 722, 611, 833, 722, 778,
              667, 778, 722, 667, 611, 722, 667, 944, 667, 667,
              611, 333, 278, 333, 584, 556, 333, 556, 611, 556,
              611, 556, 333, 611, 611, 278, 278, 556, 278, 889,
              611, 611, 611, 611, 389, 556, 333, 611, 556, 778,
              556, 556, 500, 389, 280, 389, 584, 833, 833, 833,
              833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
              833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
              833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
              278, 333, 556, 556, 556, 556, 280, 556, 333, 737,
              370, 556, 584, 333, 737, 333, 400, 584, 333, 333,
              333, 611, 556, 278, 333, 333, 365, 556, 834, 834,
              834, 611, 722, 722, 722, 722, 722, 722, 1000, 722,
              667, 667, 667, 667, 278, 278, 278, 278, 722, 722,
              778, 778, 778, 778, 778, 584, 778, 722, 722, 722,
              722, 667, 667, 611, 556, 556, 556, 556, 556, 556,
              889, 556, 556, 556, 556, 556, 278, 278, 278, 278,
              611, 611, 611, 611, 611, 611, 611, 584, 611, 611,
              611, 611, 611, 556, 611, 556)
    ),
    (Name  : 'Helvetica-Oblique';
     ULPos : -151; ULThickness : 50; Ascender : 729; Descender : -213;
     Widths: (278, 278, 355, 556, 556, 889, 667, 191,
              333, 333, 389, 584, 278, 333, 278, 278, 556, 556,
              556, 556, 556, 556, 556, 556, 556, 556, 278, 278,
              584, 584, 584, 556, 1015, 667, 667, 722, 722, 667,
              611, 778, 722, 278, 500, 667, 556, 833, 722, 778,
              667, 778, 722, 667, 611, 722, 667, 944, 667, 667,
              611, 278, 278, 278, 469, 556, 333, 556, 556, 500,
              556, 556, 278, 556, 556, 222, 222, 500, 222, 833,
              556, 556, 556, 556, 333, 500, 278, 556, 500, 722,
              500, 500, 500, 334, 260, 334, 584, 833, 833, 833,
              833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
              833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
              833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
              278, 333, 556, 556, 556, 556, 260, 556, 333, 737,
              370, 556, 584, 333, 737, 333, 400, 584, 333, 333,
              333, 556, 537, 278, 333, 333, 365, 556, 834, 834,
              834, 611, 667, 667, 667, 667, 667, 667, 1000, 722,
              667, 667, 667, 667, 278, 278, 278, 278, 722, 722,
              778, 778, 778, 778, 778, 584, 778, 722, 722, 722,
              722, 667, 667, 611, 556, 556, 556, 556, 556, 556,
              889, 500, 556, 556, 556, 556, 278, 278, 278, 278,
              556, 556, 556, 556, 556, 556, 556, 584, 611, 556,
              556, 556, 556, 500, 556, 500)
    ),
   (Name  : 'Helvetica-BoldOblique';
    ULPos : -111; ULThickness : 69; Ascender : 729; Descender : -218;
    Widths: (278, 333, 474, 556, 556, 889, 722, 238,
             333, 333, 389, 584, 278, 333, 278, 278, 556, 556,
             556, 556, 556, 556, 556, 556, 556, 556, 333, 333,
             584, 584, 584, 611, 975, 722, 722, 722, 722, 667,
             611, 778, 722, 278, 556, 722, 611, 833, 722, 778,
             667, 778, 722, 667, 611, 722, 667, 944, 667, 667,
             611, 333, 278, 333, 584, 556, 333, 556, 611, 556,
             611, 556, 333, 611, 611, 278, 278, 556, 278, 889,
             611, 611, 611, 611, 389, 556, 333, 611, 556, 778,
             556, 556, 500, 389, 280, 389, 584, 833, 833, 833,
             833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
             833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
             833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
             278, 333, 556, 556, 556, 556, 280, 556, 333, 737,
             370, 556, 584, 333, 737, 333, 400, 584, 333, 333,
             333, 611, 556, 278, 333, 333, 365, 556, 834, 834,
             834, 611, 722, 722, 722, 722, 722, 722, 1000, 722,
             667, 667, 667, 667, 278, 278, 278, 278, 722, 722,
             778, 778, 778, 778, 778, 584, 778, 722, 722, 722,
             722, 667, 667, 611, 556, 556, 556, 556, 556, 556,
             889, 556, 556, 556, 556, 556, 278, 278, 278, 278,
             611, 611, 611, 611, 611, 611, 611, 584, 611, 611,
             611, 611, 611, 556, 611, 556)
    ),
   (Name  : 'Times-Roman';
    ULPos : -100; ULThickness : 50; Ascender : 683; Descender : -217;
    Widths: (250, 333, 408, 500, 500, 833, 778, 180,
             333, 333, 500, 564, 250, 333, 250, 278, 500, 500,
             500, 500, 500, 500, 500, 500, 500, 500, 278, 278,
             564, 564, 564, 444, 921, 722, 667, 667, 722, 611,
             556, 722, 722, 333, 389, 722, 611, 889, 722, 722,
             556, 722, 667, 556, 611, 722, 722, 944, 722, 722,
             611, 333, 278, 333, 469, 500, 333, 444, 500, 444,
             500, 444, 333, 500, 500, 278, 278, 500, 278, 778,
             500, 500, 500, 500, 333, 389, 278, 500, 500, 722,
             500, 500, 444, 480, 200, 480, 541, 889, 889, 889,
             889, 889, 889, 889, 889, 889, 889, 889, 889, 889,
             889, 889, 889, 889, 889, 889, 889, 889, 889, 889,
             889, 889, 889, 889, 889, 889, 889, 889, 889, 889,
             250, 333, 500, 500, 500, 500, 200, 500, 333, 760,
             276, 500, 564, 333, 760, 333, 400, 564, 300, 300,
             333, 500, 453, 250, 333, 300, 310, 500, 750, 750,
             750, 444, 722, 722, 722, 722, 722, 722, 889, 667,
             611, 611, 611, 611, 333, 333, 333, 333, 722, 722,
             722, 722, 722, 722, 722, 564, 722, 722, 722, 722,
             722, 722, 556, 500, 444, 444, 444, 444, 444, 444,
             667, 444, 444, 444, 444, 444, 278, 278, 278, 278,
             500, 500, 500, 500, 500, 500, 500, 564, 500, 500,
             500, 500, 500, 500, 500, 500)
   ),
  (Name  : 'Times-Bold';
   ULPos : -100; ULThickness : 50; Ascender : 676; Descender : -205;
   Widths: (250, 333, 555, 500, 500, 1000, 833, 278,
            333, 333, 500, 570, 250, 333, 250, 278, 500, 500,
            500, 500, 500, 500, 500, 500, 500, 500, 333, 333,
            570, 570, 570, 500, 930, 722, 667, 722, 722, 667,
            611, 778, 778, 389, 500, 778, 667, 944, 722, 778,
            611, 778, 722, 556, 667, 722, 722, 1000, 722, 722,
            667, 333, 278, 333, 581, 500, 333, 500, 556, 444,
            556, 444, 333, 500, 556, 278, 333, 556, 278, 833,
            556, 500, 556, 556, 444, 389, 333, 556, 500, 722,
            500, 500, 444, 394, 220, 394, 520, 944, 944, 944,
            944, 944, 944, 944, 944, 944, 944, 944, 944, 944,
            944, 944, 944, 944, 944, 944, 944, 944, 944, 944,
            944, 944, 944, 944, 944, 944, 944, 944, 944, 944,
            250, 333, 500, 500, 500, 500, 220, 500, 333, 747,
            300, 500, 570, 333, 747, 333, 400, 570, 300, 300,
            333, 556, 540, 250, 333, 300, 330, 500, 750, 750,
            750, 500, 722, 722, 722, 722, 722, 722, 1000, 722,
            667, 667, 667, 667, 389, 389, 389, 389, 722, 722,
            778, 778, 778, 778, 778, 570, 778, 722, 722, 722,
            722, 722, 611, 556, 500, 500, 500, 500, 500, 500,
            722, 444, 444, 444, 444, 444, 278, 278, 278, 278,
            500, 556, 500, 500, 500, 500, 500, 570, 500, 556,
            556, 556, 556, 500, 556, 500)
   ),
  (Name  : 'Times-Italic';
   ULPos : -100; ULThickness : 50; Ascender : 683; Descender : -205;
   Widths: (250, 333, 420, 500, 500, 833, 778, 214,
            333, 333, 500, 675, 250, 333, 250, 278, 500, 500,
            500, 500, 500, 500, 500, 500, 500, 500, 333, 333,
            675, 675, 675, 500, 920, 611, 611, 667, 722, 611,
            611, 722, 722, 333, 444, 667, 556, 833, 667, 722,
            611, 722, 611, 500, 556, 722, 611, 833, 611, 556,
            556, 389, 278, 389, 422, 500, 333, 500, 500, 444,
            500, 444, 278, 500, 500, 278, 278, 444, 278, 722,
            500, 500, 500, 500, 389, 389, 278, 500, 444, 667,
            444, 444, 389, 400, 275, 400, 541, 833, 833, 833,
            833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
            833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
            833, 833, 833, 833, 833, 833, 833, 833, 833, 833,
            250, 389, 500, 500, 500, 500, 275, 500, 333, 760,
            276, 500, 675, 333, 760, 333, 400, 675, 300, 300,
            333, 500, 523, 250, 333, 300, 310, 500, 750, 750,
            750, 500, 611, 611, 611, 611, 611, 611, 889, 667,
            611, 611, 611, 611, 333, 333, 333, 333, 722, 667,
            722, 722, 722, 722, 722, 675, 722, 722, 722, 722,
            722, 556, 611, 500, 500, 500, 500, 500, 500, 500,
            667, 444, 444, 444, 444, 444, 278, 278, 278, 278,
            500, 500, 500, 500, 500, 500, 500, 675, 500, 500,
            500, 500, 500, 444, 500, 444)
   ),
  (Name  : 'Times-BoldItalic';
   ULPos : -100; ULThickness : 50; Ascender : 699; Descender : -205;
   Widths: (250, 389, 555, 500, 500, 833, 778, 278,
            333, 333, 500, 570, 250, 333, 250, 278, 500, 500,
            500, 500, 500, 500, 500, 500, 500, 500, 333, 333,
            570, 570, 570, 500, 832, 667, 667, 667, 722, 667,
            667, 722, 778, 389, 500, 667, 611, 889, 722, 722,
            611, 722, 667, 556, 611, 722, 667, 889, 667, 611,
            611, 333, 278, 333, 570, 500, 333, 500, 500, 444,
            500, 444, 333, 500, 556, 278, 278, 500, 278, 778,
            556, 500, 500, 500, 389, 389, 278, 556, 444, 667,
            500, 444, 389, 348, 220, 348, 570, 889, 889, 889,
            889, 889, 889, 889, 889, 889, 889, 889, 889, 889,
            889, 889, 889, 889, 889, 889, 889, 889, 889, 889,
            889, 889, 889, 889, 889, 889, 889, 889, 889, 889,
            250, 389, 500, 500, 500, 500, 220, 500, 333, 747,
            266, 500, 606, 333, 747, 333, 400, 570, 300, 300,
            333, 576, 500, 250, 333, 300, 300, 500, 750, 750,
            750, 500, 667, 667, 667, 667, 667, 667, 944, 667,
            667, 667, 667, 667, 389, 389, 389, 389, 722, 722,
            722, 722, 722, 722, 722, 570, 722, 722, 722, 722,
            722, 611, 611, 500, 500, 500, 500, 500, 500, 500,
            722, 444, 444, 444, 444, 444, 278, 278, 278, 278,
            500, 556, 500, 500, 500, 500, 500, 570, 500, 556,
            556, 556, 556, 444, 500, 444)
   ),
  (Name  : 'Symbol';
   ULPos : -229; ULThickness : 46; Ascender : 673; Descender : -222;
   Widths: (250,333,713,500,549,833,778,439,
            333,333,500,549,250,549,250,278,500,500,
            500,500,500,500,500,500,500,500,278,278,
            549,549,549,444,549,722,667,722,612,611,
            763,603,722,333,631,722,686,889,722,722,
            768,741,556,592,611,690,439,768,645,795,
            611,333,863,333,658,500,500,631,549,549,
            494,439,521,411,603,329,603,549,549,576,
            521,549,549,521,549,603,439,576,713,686,
            493,686,494,480,200,480,549,0,0,0,
            0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,
            0,620,247,549,167,713,500,753,753,753,
            753,1042,987,603,987,603,400,549,411,549,
            549,713,494,460,549,549,549,549,1000,603,
            1000,658,823,686,795,987,768,768,823,768,
            768,713,713,713,713,713,713,713,768,713,
            790,790,890,823,549,250,713,603,603,1042,
            987,603,987,603,494,329,790,790,786,713,
            384,384,384,384,384,384,494,494,494,494,
            0,329,274,686,686,686,384,384,384,384,
            384,384,494,494,790, 250)
   )
   );

const
  PageOpArr: array[boolean] of string[5] = ('Page','');
  OrientArr: array[boolean] of string[10] = ('Landscape','Portrait');

{$IFDEF ASCII85}
type

  { TAscii85Encoder }

  TAscii85Encoder=class
  private
    FStream: TMemoryStream;
    FData: LongWord;
    FCount: Integer;
    FMaxWidth,FWritten: Integer;
    procedure EmitData;
    procedure WriteByte(const B:Byte);
  public
    destructor Destroy; override;
    procedure Add(B: Byte);
    procedure Finish;

    property Stream: TMemoryStream read FStream;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
  end;

{ TAscii85Encoder }

procedure TAscii85Encoder.EmitData;
const
  Cn: array[0..4] of longword = (85*85*85*85,85*85*85,85*85,85,1);
var
  B: byte;
  i,n: Integer;
begin
  if FCount=0 then
    exit;

  if FStream=nil then
    FStream := TMemoryStream.Create;

  if (FCount=4) and (FData=0) then

    // special case, zeroed 5-tuple will be generated
    WriteByte(ord('z'))

  else begin

    n := FCount;
    while FCount<4 do begin
      FData := (FData shl 8);
      inc(FCount);
    end;
    for i:=0 to n do begin
      B := byte((FData div Cn[i])+33);
      FData := FData mod Cn[i];
      WriteByte(B);
    end;

  end;

  FCount := 0;
  FData := 0;
end;

procedure TAscii85Encoder.WriteByte(const B: Byte);
var
  e: string;
begin
  FStream.WriteByte(B);
  if FMaxWidth>0 then begin
    Inc(FWritten);
    if FWritten>=FMaxWidth then begin
      // write lineending
      e:=LineEnding;
      FStream.Write(e[1],length(e));
      FWritten := 0;
    end;
  end;
end;

destructor TAscii85Encoder.Destroy;
begin
  if FStream<>nil then
    FStream.Free;
  inherited Destroy;
end;

procedure TAscii85Encoder.Add(B: Byte);
begin
  FData := (FData shl 8) or B;
  inc(FCount);
  if FCount=4 then
    EmitData;
end;

procedure TAscii85Encoder.Finish;
begin
  EmitData;
  FStream.WriteByte(ord('~'));
  FStream.WriteByte(ord('>'));
  FStream.Position:=0;
end;

{$ENDIF}

{ TPostScriptPrinterCanvas }

//Write an instruction in the header of document
procedure TPostScriptPrinterCanvas.WriteHeader(St: String);
begin
  fHeader.Add(St);
end;

//Write an instruction in the document
procedure TPostScriptPrinterCanvas.Write(const St: String; Lst: TStringList = nil);
begin
  If not Assigned(Lst) then
    Lst:=fDocument;
    
  Lst.Add(St);
end;

//Write data in fBuffer
procedure TPostScriptPrinterCanvas.WriteB(const St: string);
begin
  Write(St,fBuffer);
end;

//Clear all data of Buffer
procedure TPostScriptPrinterCanvas.ClearBuffer;
begin
  fBuffer.Clear;
end;

//Write all Lst.Strings in document
procedure TPostScriptPrinterCanvas.Write(Lst: TStringList);
begin
  fDocument.AddStrings(Lst);
end;

//Write an comment in the document
procedure TPostScriptPrinterCanvas.WriteComment(const St: string);
begin
  fDocument.Add('%'+St);
end;

procedure TPostScriptPrinterCanvas.WritePageTransform;
var
  h,w:integer;
begin
  case Orientation of
    poReversePortrait:
      begin
        w:=round(PaperWidth*72/XDPI);
        h:=round(PaperHeight*72/YDPI);
        Write(format('%d %d translate 180 rotate',[w,h]));
      end;
    poLandscape:
       begin
         h:=round(PaperHeight*72/YDPI);
         Write(format('%d 0 translate 90 rotate',[h]));
       end;
     poReverseLandscape:
       begin
         w:=round((PaperWidth-PaperHeight)*72/XDPI);
         h:=round(PaperHeight*72/XDPI);
         Write(format('%d %d translate 90 neg rotate',[w,h]));
       end;
  end;
end;

procedure TPostScriptPrinterCanvas.WriteOrientation(UseHeader: boolean);
var
  L: TStringList;
begin

  if UseHeader then
    L := Fheader
  else
    L := nil;

  Write('%%'+PageOpArr[UseHeader]+'Orientation: '+
    OrientArr[(Orientation=poPortrait)or(Orientation=poReversePortrait)], L);
end;

procedure TPostScriptPrinterCanvas.WriteBoundingBox(UseHeader: boolean);
var
  a,l,t,w,h: Integer;
  Lst: TStringList;
begin

  l := round(LeftMargin * 72 / XDPI);
  t := round(TopMargin * 72 / YDPI);
  w := round((PaperWidth - RightMargin) * 72 / XDPI);
  h := round((PaperHeight - BottomMargin) * 72 / YDPI);

  if (Orientation=poLandscape) or (Orientation=poReverseLandscape) then
  begin
    a := l; l := t; t := a;
    a := w; w := h; h := a;
  end;

  if UseHeader then
    Lst := FHeader
  else
    Lst := nil;

  Write('%%'+PageOpArr[UseHeader]+Format('BoundingBox: %d %d %d %d',[l,t,w,h]),
    Lst);
end;

//Convert an TCanvas Y point to PostScript Y point
//The TCanvas origine is corner Left,Top and PostScript is Left,Bottom
//Modify X and Y for use Left and Top margin
function TPostScriptPrinterCanvas.TranslateCoord(cnvX,cnvY : Integer):TpsPoint;
begin
  PixelsToPoints(cnvX+LeftMargin, PageHeight+BottomMargin-cnvY,
    Result.Fx, Result.Fy);
end;

function TPostScriptPrinterCanvas.TxRectToBounds(aRect: TRect): TpsBounds;
var
  p1,p2: TPsPoint;
begin
  p1 := TranslateCoord(aRect.Left, aRect.Top);
  p2 := TranslateCoord(aRect.Right, aRect.Bottom);
  Result.fx := p1.fx;
  Result.fy := p2.fy;
  Result.fwidth := p2.fx-p1.fx;
  Result.fheight := p1.fy-p2.fy;
end;

//Save the last position
procedure TPostScriptPrinterCanvas.SetPosition(X, Y: Integer);
begin
  SetInternalPenPos(Point(X,Y));
end;

//Init the width of line
procedure TPostScriptPrinterCanvas.UpdateLineWidth;
var
  pw:single;
begin
  if Pen.Width<>fcPenWidth then
  begin
    pw:=1/XDPI; // printer pixel in inches
    pw:=Pen.Width*pw*72; // pen width in Points -> 1/72 inches
    Write(Format('%.3f setlinewidth',[pw],FFs));
    fcPenWidth:=Pen.Width;
  end;
end;

//Init the color of line (pen)
procedure TPostScriptPrinterCanvas.UpdateLineColor(aColor : TColor = clNone);
Var R,G,B    : Real;
    RGBColor : TColorRef;
begin
  if aColor=clNone then
    aColor:=Pen.Color;
    
  if aColor<>fcPenColor then
  begin
    RGBColor:=ColorToRGB(aColor);
    
    R:=Red(RGBColor)/255;
    G:=Green(RGBColor)/255;
    B:=Blue(RGBColor)/255;
    Write(Format('%.3f %.3f %.3f setrgbcolor',[R,G,B],FFs)+' % '+ColorToString(aColor));
    fcPenColor:=aColor;
  end;
end;

//Init the style of line
procedure TPostScriptPrinterCanvas.UpdateLineStyle;
Var St : string;
begin
  if (Pen.Style<>fcPenStyle) and (Pen.Style<>psClear) then
  begin
    Case Pen.Style of
      psSolid      : St:='[] 0';
      psDash       : St:='[5 2] 0';
      psDot        : St:='[1 3] 0';
      psDashDot    : St:='[5 2 2 2] 0';
      psDashDotDot : St:='[5 2 2 2 2 2] 0';
      else St:='';
    end;
    
    Write(Format('%s setdash',[St]));
    fcPenStyle:=Pen.Style;
  end;
end;

//Init the color for fill
procedure TPostScriptPrinterCanvas.UpdateFillColor;
Var R,G,B    : Real;
    RGBColor : TColorRef;
begin
  if (Brush.Style=bsSolid) and (Brush.Color<>fcPenColor) then
  begin
    RGBColor:=ColorToRGB(Brush.Color);

    R:=Red(RGBColor)/255;
    G:=Green(RGBColor)/255;
    B:=Blue(RGBColor)/255;
    Write(Format('%.3f %.3f %.3f setrgbcolor',[R,G,B],FFs)+' % '+ColorToString(Brush.Color));
    fcPenColor:=Brush.Color;
  end;
end;

//Update current font
procedure TPostScriptPrinterCanvas.UpdateFont;
Var R,G,B    : Real;
    RGBColor : TColorRef;
begin
  if Font.Color=clNone then
    Font.Color:=clBlack;
  if Font.Size=0 then
    Font.Size:=12;

  if Font.Color<>fcPenColor then
  begin
    RGBColor:=ColorToRGB(Font.Color);

    R:=Red(RGBColor)/255;
    G:=Green(RGBColor)/255;
    B:=Blue(RGBColor)/255;

    Write(Format('%.3f %.3f %.3f setrgbcolor',[R,G,B],FFs)+' % '+ColorToString(Font.Color));
    fcPenColor:=Font.Color;
  end;
end;

//Return an PostScript font Name
function TPostScriptPrinterCanvas.MappedFontName: string;
Var Atr : string;
begin
  Atr:='';
  Result := '';
  if Copy(LowerCase(Font.Name),1,5)='times' then
    Result:='Times';
  if (LowerCase(Font.Name)='monospaced') or (Copy(LowerCase(Font.Name),1,7)='courier') then
    Result:='Courier';
  if LowerCase(Font.Name)='serif' then
    Result:='Times';
  if LowerCase(Font.Name)='sansserif' then
    Result:='Helvetica';
  if LowerCase(Font.Name)='symbol' then
    Result:='Symbol';

  if Result='' then
    Result:='Helvetica';

  if (fsBold in Font.Style)  and ((Pos('Courier',Result)=1) or (Pos('Helvetica',Result)=1) or (Pos('Times',Result)=1)) then
    Atr:=Atr+'Bold';
  if (fsItalic in Font.Style) and ((Pos('Courier',Result)=1) or (Pos('Helvetica',Result)=1)) then
    Atr:=Atr+'Oblique';
  if (fsItalic in Font.Style) and (Pos('Times',Result)=1)  then
    Atr:=Atr+'Italic';
  if (Result+Atr='Times') or (Result+Atr='Times') then
    Result:='Times-Roman';

  //WriteComment(Format('MapedFontName "%s" -> "%s"',[Font.Name,Result]));
  
  if Atr <> '' then
    Result:=Result+'-'+Atr;
end;

//Move pen at last pos
procedure TPostScriptPrinterCanvas.MoveToLastPos;
var
  pp:TpsPoint;
begin
  pp:=Self.TranslateCoord(PenPos.X,PenPos.Y);
  write(Format('%f %f moveto',[pp.fx,pp.fy],Ffs)+' %last pos');
  Include(FStatus, pcsPosValid);
end;

//Add at the PstScript sequence, the Fill Pattern/Color and Broder
//Use SetBorder and SetFill for initialize 1 or 2 sequence
procedure TPostScriptPrinterCanvas.SetBrushFillPattern(Lst: TStringList;
  SetBorder, SetFill: Boolean);
var
  s: string;
begin
  If not Assigned(Lst) then Exit;
  
  if SetFill then
  begin
    if (Brush.Color<>clNone) and (Brush.Style<>bsClear) then
    begin
      UpdateFillColor;

      Case Brush.Style of
          bsSolid : begin
                      Write(Lst);
                      Write('eofill');
                    end;
          bsClear : ;
         else
         begin
           UpdateLineColor(Brush.Color);
           WriteStr(s, Brush.Style);
           write(Format('/%s findfont  %% a pattern font patternfill',[s]));
           Write(Lst);
           write('patternfill');
         end;
       end;
    end;
  end;
  
  if SetBorder and ((Pen.Color<>clNone) and ((Pen.Color<>Brush.Color) or (Brush.Style<>bsSolid))) then
  begin
    UpdateLineColor(clNone);
    UpdateLineWidth;
    UpdateLineStyle;
    Write(Lst);
    Write('stroke');
  end;
end;

procedure TPostScriptPrinterCanvas.SetBrushFillPattern(SetBorder, SetFill: Boolean);
begin
  SetBrushFillPattern(fBuffer,SetBorder,SetFill);
end;

//Add in Lst, all RGB pixels of SrcGraph picture
procedure TPostScriptPrinterCanvas.GetRGBImage(SrcGraph: TGraphic;
  Lst: TStringList);
var
  SrcIntfImg : TLazIntfImage;

  {$IFDEF ASCII85}
  procedure TransferRGB;
  var
    px, py     : Integer;
    CurColor   : TFPColor;
    Encoder    : TAscii85Encoder;
    A          : Byte;
    Ratio      : Single;
  begin
    Encoder := TAscii85Encoder.Create;
    try
      Encoder.MaxWidth:=75;
      for py:=0 to SrcIntfImg.Height-1 do
      begin
        for px:=0 to SrcIntfImg.Width-1 do
        begin
          CurColor:=SrcIntfImg.Colors[px,py];
          A := Hi(CurColor.alpha);
          if A=0 then begin
            Encoder.Add(255);
            Encoder.Add(255);
            Encoder.Add(255);
          end else
          if A=255 then begin
            Encoder.Add(Hi(CurColor.Red));
            Encoder.Add(Hi(CurColor.Green));
            Encoder.Add(Hi(CurColor.Blue));
          end else begin
            Ratio := 1-(255-A)/255;
            Encoder.Add(round(Hi(CurColor.Red  )*Ratio+255*(1-Ratio)));
            Encoder.Add(round(Hi(CurColor.Green)*Ratio+255*(1-Ratio)));
            Encoder.Add(round(Hi(CurColor.Blue )*Ratio+255*(1-Ratio)));
          end;
        end;
      end;
      Encoder.Finish;
      Encoder.Stream.Position:=0;
      Lst.LoadFromStream(Encoder.Stream);
    finally
      Encoder.Free;
    end;
  end;
  {$ELSE}
  procedure TransferRGB;
  var
    px, py     : Integer;
    CurColor   : TFPColor;
    St         : String;
  begin
    St:='';
    for py:=0 to SrcIntfImg.Height-1 do
    begin
      for px:=0 to SrcIntfImg.Width-1 do
      begin
        CurColor:=SrcIntfImg.Colors[px,py];
        St:=St+IntToHex(Hi(CurColor.Red),2)+
               IntToHex(Hi(CurColor.Green),2)+
               IntToHex(Hi(CurColor.Blue),2);

        if Length(St)>=78 then
        begin
           Lst.Add(Copy(St,1,78));
           System.Delete(St,1,78);
        end;
      end;
    end;
    if St<>'' then
      Lst.Add(St);
  end;
  {$ENDIF}

  procedure TransferRGBA;
  begin
    TransferRGB;
  end;

begin
  if (SrcGraph is TRasterImage) then
  begin
    SrcIntfImg:=TLazIntfImage.Create(0,0,[]);
    Lst.BeginUpdate;
    Try
      SrcIntfImg.LoadFromBitmap(TRasterImage(SrcGraph).BitmapHandle,
                                TRasterImage(SrcGraph).MaskHandle);

      if SrcIntfImg.DataDescription.Format<>ricfNone then
      begin
        if SrcIntfImg.DataDescription.AlphaPrec<>0 then
          TransferRGBA
        else
          TransferRGB;
      end;

    finally
      Lst.EndUpdate;
      SrcIntfImg.Free;
    end;
  end;
end;

procedure TPostScriptPrinterCanvas.PixelsToPoints(const PixX,PixY: Integer;
  out PtX,PtY:Single);
begin
  PtX:=72*(PixX/XDPI); // pixels to points
  PtY:=72*(PixY/YDPI);
end;

function TPostScriptPrinterCanvas.GetFontSize: Integer;
begin
  if Font.Size=0 then
    Result := 12
  else
    Result := Font.Size;
end;

procedure TPostScriptPrinterCanvas.RestoreClip;
begin
  if pcsClipSaved in FStatus then
  begin
    Self.WriteComment('Restoring Old clip rect');
    Self.Write('cliprestore');
    Exclude(FStatus, pcsClipSaved);
  end;
end;

procedure TPostScriptPrinterCanvas.SaveClip;
var
  B: TpsBounds;
begin
  Self.WriteComment('Pushing and Setting current clip rect');
  Self.Write('clipsave');
  B := TxRectToBounds(FLazClipRect);
  Write(Format('%f %f %f %f rectclip',[B.fx, B.fy, B.fwidth, B.fheight],FFs));
  Include(FStatus, pcsClipSaved);
end;

procedure TPostScriptPrinterCanvas.CheckLastPos;
begin
  if not (pcsPosValid in FStatus) then
    MoveToLastPos;
end;

function TPostScriptPrinterCanvas.GetFontIndex: Integer;
var
  FontName: string;
  i: Integer;
begin
  FontName:=MappedFontName;
  Result:=0; //By default, use Courier metrics
  for i:=0 to High(cFontPSMetrics) do
  begin
    if cFontPSMetrics[i].Name=FontName then
    begin
      Result:=i;
      Break;
    end;
  end;
end;

function TPostScriptPrinterCanvas.FontUnitsToPixelsX(const Value: Integer
  ): Integer;
begin
  result := Round(Value*Abs(GetFontSize/72)*0.001*XDPI);
end;

function TPostScriptPrinterCanvas.FontUnitsToPixelsY(const Value: Integer
  ): Integer;
begin
  result := Round(Value*Abs(GetFontSize/72)*0.001*YDPI);
end;

function TPostScriptPrinterCanvas.FontUnitsToPixelsY(const Value: Double
  ): Integer;
var
  FontSize: Integer;
begin
  FontSize := GetFontSize;
  if FontSize<0 then
    FontSize := -FontSize;
  result := Round(Value*FontSize/72*0.001*YDPI);
end;

procedure TPostScriptPrinterCanvas.CreateHandle;
begin
  SetHandle(1); // set dummy handle
end;

procedure TPostScriptPrinterCanvas.RealizeAntialiasing;
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.CreateBrush;
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.CreateFont;
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.CreatePen;
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.CreateRegion;
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.DeselectHandles;
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.PenChanging(APen: TObject);
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.FontChanging(APen: TObject);
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.BrushChanging(APen: TObject);
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.RegionChanging(APen: TObject);
begin
  // handle is dummy, so do nothing here
end;

procedure TPostScriptPrinterCanvas.RequiredState(ReqState: TCanvasState);
begin
  if csHandleValid in ReqState then
    inherited RequiredState([csHandleValid]);
  // other states are anyway impossible, because handle is dummy
end;

procedure TPostScriptPrinterCanvas.DoEllipseAndFill(const Bounds: TRect);
begin
  Ellipse(Bounds.Left, Bounds.Top, Bounds.Right, Bounds.Bottom);
end;

function TPostScriptPrinterCanvas.GetClipRect: TRect;
begin
  Result:=FLazClipRect;
end;

constructor TPostScriptPrinterCanvas.Create(APrinter: TPrinter);
begin
  inherited Create(APrinter);
  
  fcBrushStyle:=bsClear;
  fcPenColor  :=clBlack;
  fcPenWidth  :=0;
  fcPenStyle  :=psSolid;
  
  fHeader:=TStringList.Create;
  fBuffer:=TstringList.Create;
  fDocument:=TStringList.Create;

  Ffs.DecimalSeparator:='.';
  Ffs.ThousandSeparator:=#0;
  Include(FStatus, pcsClipping);
end;

destructor TPostScriptPrinterCanvas.Destroy;
begin
  if FPSUnicode<>nil then
    FPSUnicode.Free;

  fBuffer.Free;
  fHeader.Free;
  fDocument.Free;
  
  inherited Destroy;
end;

procedure TPostScriptPrinterCanvas.SaveToFile(aFileName: string);
Var Lst : TStringListUTF8;
begin
  Lst:=TStringListUTF8.Create;
  try
     Lst.AddStrings(fHeader);
     Lst.AddStrings(fDocument);
     
     Lst.SaveTofile(ExpandFileNameUTF8(aFileName));
  finally
    Lst.Free;
  end;
end;

procedure TPostScriptPrinterCanvas.BeginDoc;
begin
  inherited BeginDoc;

  if FPSUnicode=nil then
    FPSUnicode := TPSUnicode.Create;
  FPSUnicode.OutLst := FDocument;
  //Clear all existing values
  //before starting an new document
  fDocument.Clear;
  fHeader.Clear;
  
  Font.Size:=12;
  Font.Color:=clBlack;

  WriteHeader('%!PS-Adobe-3.0');
  WriteBoundingBox(True);
  WriteHeader('%%'+Format('Creator: Lazarus PostScriptCanvas for %s',[Application.ExeName]));
  WriteHeader('%%'+Format('Title: %s',[Title]));
  WriteHeader('%%CreationDate: '+DateTimeToStr(Now));
  WriteOrientation(true);
  WriteHeader('%%Pages: (atend)');
  WriteHeader('%%PageResources: (atend)');
  WriteHeader('%%PageOrder: Ascend');
  WriteHeader('');
  WriteHeader('%------------------------------------------------------------');
  WriteHeader('%================== BEGIN SETUP==============================');
  WriteHeader('');
  WriteHeader('/RE { % /NewFontName [NewEncodingArray] /FontName RE -');
  WriteHeader('  findfont dup length dict begin');
  WriteHeader('  {');
  WriteHeader('    1 index /FID ne');
  WriteHeader('    {def}');
  WriteHeader('    {pop pop} ifelse');
  WriteHeader('  } forall');
  WriteHeader('  /Encoding exch def');
  WriteHeader('  /FontName 1 index def');
  WriteHeader('  currentdict definefont pop');
  WriteHeader('  end');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('/scp {currentpoint /oldy exch def /oldx exch def } def');
  WriteHeader('/rcp {oldx oldy moveto} bind def');
  WriteHeader('/uli { 2 copy /uposy exch def /uposx exch def moveto } def');
  WriteHeader('/ule { % underlinepenwidh underlinepos');
  WriteHeader('scp gsave 0 exch rmoveto setlinewidth');
  WriteHeader('uposx oldx sub 0 rlineto [] 0 setdash stroke grestore rcp } def');
  WriteHeader('');
  WriteHeader('%%BeginProcSet: patternfill 1.0 0');
  WriteHeader('% width height matrix proc key cache');
  WriteHeader('% definepattern -\> font');
  WriteHeader('/definepattern { %def');
  WriteHeader('    7 dict begin');
  WriteHeader('        /FontDict 9 dict def');
  WriteHeader('        FontDict begin');
  WriteHeader('            /cache exch def');
  WriteHeader('            /key exch def');
  WriteHeader('            /proc exch cvx def');
  WriteHeader('            /mtx exch matrix invertmatrix def');
  WriteHeader('            /height exch def');
  WriteHeader('            /width exch def');
  WriteHeader('            /ctm matrix currentmatrix def');
  WriteHeader('            /ptm matrix identmatrix def');
  WriteHeader('            /str');
  WriteHeader('            (12345678901234567890123456789012)');
  WriteHeader('            def');
  WriteHeader('        end');
  WriteHeader('        /FontBBox [ %def');
  WriteHeader('            0 0 FontDict /width get');
  WriteHeader('            FontDict /height get');
  WriteHeader('        ] def');
  WriteHeader('        /FontMatrix FontDict /mtx get def');
  WriteHeader('        /Encoding StandardEncoding def');
  WriteHeader('        /FontType 3 def');
  WriteHeader('        /BuildChar { %def');
  WriteHeader('            pop begin');
  WriteHeader('            FontDict begin');
  WriteHeader('                width 0 cache { %ifelse');
  WriteHeader('                    0 0 width height setcachedevice');
  WriteHeader('                }{ %else');
  WriteHeader('                    setcharwidth');
  WriteHeader('                } ifelse');
  WriteHeader('                0 0 moveto width 0 lineto');
  WriteHeader('                width height lineto 0 height lineto');
  WriteHeader('                closepath clip newpath');
  WriteHeader('                gsave proc grestore');
  WriteHeader('            end end');
  WriteHeader('        } def');
  WriteHeader('        FontDict /key get currentdict definefont');
  WriteHeader('    end');
  WriteHeader('} bind def');
  WriteHeader('% dict patternpath -');
  WriteHeader('% dict matrix patternpath -');
  WriteHeader('/patternpath { %def');
  WriteHeader('    dup type /dicttype eq { %ifelse');
  WriteHeader('        begin FontDict /ctm get setmatrix');
  WriteHeader('    }{ %else');
  WriteHeader('        exch begin FontDict /ctm get setmatrix');
  WriteHeader('        concat');
  WriteHeader('    } ifelse');
  WriteHeader('    currentdict setfont');
  WriteHeader('    FontDict begin');
  WriteHeader('        FontMatrix concat');
  WriteHeader('        width 0 dtransform');
  WriteHeader('        round width div exch round width div exch');
  WriteHeader('        0 height dtransform');
  WriteHeader('        round height div exch');
  WriteHeader('        round height div exch');
  WriteHeader('        0 0 transform round exch round exch');
  WriteHeader('        ptm astore setmatrix');
  WriteHeader('        ');
  WriteHeader('        pathbbox');
  WriteHeader('        height div ceiling height mul 4 1 roll');
  WriteHeader('        width div ceiling width mul 4 1 roll');
  WriteHeader('        height div floor height mul 4 1 roll');
  WriteHeader('        width div floor width mul 4 1 roll');
  WriteHeader('        ');
  WriteHeader('        2 index sub height div ceiling cvi exch');
  WriteHeader('        3 index sub width div ceiling cvi exch');
  WriteHeader('        4 2 roll moveto');
  WriteHeader('        ');
  WriteHeader('        FontMatrix ptm invertmatrix pop');
  WriteHeader('        { %repeat');
  WriteHeader('            gsave');
  WriteHeader('                ptm concat');
  WriteHeader('                dup str length idiv { %repeat');
  WriteHeader('                    str show');
  WriteHeader('                } repeat');
  WriteHeader('                dup str length mod str exch');
  WriteHeader('                0 exch getinterval show');
  WriteHeader('            grestore');
  WriteHeader('            0 height rmoveto');
  WriteHeader('        } repeat');
  WriteHeader('        pop');
  WriteHeader('    end end');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('% dict patternfill -');
  WriteHeader('% dict matrix patternfill -');
  WriteHeader('/patternfill { %def');
  WriteHeader('    gsave');
  WriteHeader('        clip patternpath');
  WriteHeader('    grestore');
  WriteHeader('    newpath');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('% dict patterneofill -');
  WriteHeader('% dict matrix patterneofill -');
  WriteHeader('/patterneofill { %def');
  WriteHeader('    gsave');
  WriteHeader('        eoclip patternpath');
  WriteHeader('    grestore');
  WriteHeader('    newpath');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('% dict patternstroke -');
  WriteHeader('% dict matrix patternstroke -');
  WriteHeader('/patternstroke { %def');
  WriteHeader('    gsave');
  WriteHeader('        strokepath clip patternpath');
  WriteHeader('    grestore');
  WriteHeader('    newpath');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('% dict ax ay string patternashow -');
  WriteHeader('% dict matrix ax ay string patternashow -');
  WriteHeader('/patternashow { %def');
  WriteHeader('    (0) exch { %forall');
  WriteHeader('        2 copy 0 exch put pop dup');
  WriteHeader('        false charpath ');
  WriteHeader('        currentpoint');
  WriteHeader('        5 index type /dicttype eq { %ifelse');
  WriteHeader('            5 index patternfill');
  WriteHeader('        }{ %else');
  WriteHeader('            6 index 6 index patternfill');
  WriteHeader('        } ifelse');
  WriteHeader('        moveto');
  WriteHeader('        3 copy pop rmoveto');
  WriteHeader('    } forall');
  WriteHeader('    pop pop pop');
  WriteHeader('    dup type /dicttype ne { pop } if pop');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('% dict string patternshow -');
  WriteHeader('% dict matrix string patternshow -');
  WriteHeader('/patternshow { %def');
  WriteHeader('    0 exch 0 exch patternashow');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('/opaquepatternfill { %def');
  WriteHeader('    gsave');
  WriteHeader('    1 setgray');
  WriteHeader('    fill');
  WriteHeader('    grestore');
  WriteHeader('    patternfill');
  WriteHeader('} bind def');
  WriteHeader('');
  WriteHeader('%%EndProcSet');
  WriteHeader('%%EndProlog');
  WriteHeader('');
  WriteHeader('%%BeginSetup');
  WriteHeader('15 15 [300 72 div 0 0 300 72 div 0 0]');
  WriteHeader('{ %definepattern');
  WriteHeader('    2 setlinecap');
  WriteHeader('    7.5 0 moveto 15 7.5 lineto');
  WriteHeader('    0 7.5 moveto 7.5 15 lineto');
  WriteHeader('    2 setlinewidth stroke');
  WriteHeader('} bind');
  WriteHeader('/bsBDiagonal true definepattern pop');
  WriteHeader('');
  WriteHeader('15 15 [300 72 div 0 0 300 72 div 0 0]');
  WriteHeader('{ %definepattern');
  WriteHeader('    2 setlinecap');
  WriteHeader('    7.5 0 moveto 0 7.5 lineto');
  WriteHeader('    15 7.5 moveto 7.5 15 lineto');
  WriteHeader('    2 setlinewidth stroke');
  WriteHeader('} bind');
  WriteHeader('/bsFDiagonal true definepattern pop');
  WriteHeader('30 30 [300 72 div 0 0 300 72 div 0 0]');
  WriteHeader('{ %definepattern');
  WriteHeader('    2 2 scale');
  WriteHeader('    2 setlinecap');
  WriteHeader('    7.5 0 moveto 15 7.5 lineto');
  WriteHeader('    0 7.5 moveto 7.5 15 lineto');
  WriteHeader('    7.5 0 moveto 0 7.5 lineto');
  WriteHeader('    15 7.5 moveto 7.5 15 lineto');
  WriteHeader('    0.5 setlinewidth stroke');
  WriteHeader('} bind');
  WriteHeader('/bsDiagCross true definepattern pop');
  WriteHeader('');
  WriteHeader('30 30 [300 72 div 0 0 300 72 div 0 0]');
  WriteHeader('{ %definepattern');
  WriteHeader('    2 setlinecap');
  WriteHeader('    15 0 moveto 15 30 lineto');
  WriteHeader('    0 15 moveto 30 15 lineto');
  WriteHeader('    2 setlinewidth stroke');
  WriteHeader('} bind');
  WriteHeader('/bsCross true definepattern pop');
  WriteHeader('');
  WriteHeader('15 15 [300 72 div 0 0 300 72 div 0 0]');
  WriteHeader('{ %definepattern');
  WriteHeader('    2 setlinecap');
  WriteHeader('    0 7.5 moveto 15 7.5 lineto');
  WriteHeader('    2 setlinewidth stroke');
  WriteHeader('} bind');
  WriteHeader('/bsHorizontal true definepattern pop');
  WriteHeader('');
  WriteHeader('15 15 [300 72 div 0 0 300 72 div 0 0]');
  WriteHeader('{ %definepattern');
  WriteHeader('    2 setlinecap');
  WriteHeader('    7.5 0 moveto 7.5 15 lineto');
  WriteHeader('    2 setlinewidth stroke');
  WriteHeader('} bind');
  WriteHeader('/bsVertical true definepattern pop');
  WriteHeader('%%EndSetup');
  WriteHeader('%%====================== END SETUP =========================');
  WriteHeader('');
  WriteHeader('%%Page: 1 1');
  WritePageTransform;

  if assigned(printer) then
    FLazClipRect:=printer.PaperSize.PaperRect.WorkRect;
end;

procedure TPostScriptPrinterCanvas.EndDoc;
var
  I: Integer;
begin
  Inherited EndDoc;
  
  Write('stroke');
  Write('showpage');
  Write('%%EOF');
  
  // update number of pages in header
  I := FHeader.IndexOf('%%Pages: (atend)');
  if I <> -1 then
    FHeader[I] := '%%' + Format('Pages: %d', [PageNumber]);
  
  if Trim(OutputFileName)<>'' then
    SaveToFile(ExpandFileNameUTF8(OutputFileName));

  if Assigned(fPsUnicode) then
    FreeAndNil(fPsUnicode);

  Self.fcPenWidth:=-2; // prevent cached line width affect new page
end;

procedure TPostScriptPrinterCanvas.NewPage;
begin
  inherited NewPage;

  Write('stroke');
  Write('showpage');
  Write('%%'+Format('Page: %d %d',[PageNumber, PageNumber]));
  WriteBoundingBox(false);
  WriteOrientation(false);
  WritePageTransform;
  write('newpath');

  Self.fcPenWidth:=-1; // prevent cached line width affect new page
  fSaveCount:=0;
  UpdateLineWidth;
end;

//Move the current position
procedure TPostScriptPrinterCanvas.DoMoveTo(X1, Y1: Integer);
var
  pp:TpsPoint;
begin
  RequiredState([csHandleValid]);

  WriteComment(Format('DoMoveTo(%d,%d)',[x1,y1]));

  SetPosition(X1,Y1);
  pp:=TranslateCoord(X1,Y1);

  write(Format('%f %f moveto',[pp.fx,pp.fy],FFs));

  Include(FStatus, pcsPosValid);
end;

//Drawe line
procedure TPostScriptPrinterCanvas.DoLineTo(X1, Y1: Integer);
var
  pp:TpsPoint;
begin

  checkLastPos;

  Changing;
  RequiredState([csHandleValid, csPenValid]);
  WriteComment(Format('DoLineTo(%d,%d)',[x1,y1]));
  SetPosition(X1,Y1);
  pp:=TranslateCoord(X1,Y1);
  UpdateLineColor(clNone);
  UpdateLineWidth;
  UpdateLineStyle;
  write(Format('%f %f lineto stroke',[pp.fx,pp.fy],FFs));
  changed;

  Exclude(FStatus, pcsPosValid);
end;

procedure TPostScriptPrinterCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  i  : LongInt;
  Lst: TStringList;
  Pt : TPoint;
  pp:TpsPoint;
begin
  if (NumPts<=1) or not Assigned(Points) then Exit;
  Changing;
  RequiredState([csHandleValid, csPenValid]);

  Lst:=TStringList.Create;
  try
    Pt:=Points[0];
    pp:=TranslateCoord(Pt.x,Pt.y);
    Write(Format('%f %f moveto',[pp.fx,pp.fy],FFs),Lst);
    for i:=1 to NumPts-1 do
    begin
      Pt:=Points[i];
      pp:=TranslateCoord(Pt.x,Pt.y);
      SetPosition(Pt.x,Pt.y);
      //TranslateCoord(Pt.x,Pt.y);
      Write(Format('%f %f lineto',[pp.fx,pp.fy],FFs),Lst);
    end;

    UpdateLineColor(clNone);
    UpdateLineWidth;
    UpdateLineStyle;

    write(Lst);
    write('stroke');

  finally
    Lst.Free;
  end;

  MoveToLastPos;
  Changed;
end;

procedure TPostScriptPrinterCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
  Filled: boolean; Continuous: boolean);
var
  i  : Integer;
  St : String;
  Pt : TPoint;
  pp:TpsPoint;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  if (NumPts>=4) then
  begin
    ClearBuffer;

    St:='';
    Pt:=Points[0];
    pp:=TranslateCoord(Pt.x,Pt.y);
    if Continuous then
      WriteB('newpath');
    WriteB(Format('%f %f moveto',[pp.fx,pp.fy],FFs));
    for i:=1 to NumPts-1 do
    begin
      Pt:=Points[i];
      pp:=TranslateCoord(Pt.x,Pt.y);
      St:=St+Format(' %f %f',[pp.fx,pp.fy], FFs);
    end;
    WriteB(Format('%s curveto',[St]));

    if Continuous then
      writeB('closepath');
    SetBrushFillPattern(True,Filled);

    MoveToLastPos;
  end;
  Changed;
end;


// internal rect path
procedure TPostScriptPrinterCanvas.psDrawRect(ARect:TRect);
var
  pp1,pp2:TpsPoint;
begin
  pp1:=TranslateCoord(Arect.Left,Arect.Top);
  pp2:=TranslateCoord(ARect.Right,Arect.Bottom);

  ClearBuffer;
  //Tempo draw rect
  WriteB('newpath');
  writeB(Format('    %f %f moveto',[pp1.fx,pp1.fy],FFs));
  writeB(Format('    %f %f lineto',[pp2.fx,pp1.fy],FFs));
  writeB(Format('    %f %f lineto',[pp2.fx,pp2.fy],FFs));
  writeB(Format('    %f %f lineto',[pp1.fx,pp2.fy],FFs));
  writeB('closepath');

end;

//Draw an Rectangle
procedure TPostScriptPrinterCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  writecomment(Format('Rectangle(%d,%d,%d,%d)',[x1,y1,x2,y2]));

  psDrawRect(Types.Rect(x1,y1,x2,y2));

  SetBrushFillPattern(True,True);

  MoveToLastPos;
  
  Changed;
end;

procedure TPostScriptPrinterCanvas.Frame(const ARect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);

  psDrawRect(ARect);

  SetBrushFillPattern(True,False);

  MoveToLastPos;

  Changed;
end;

procedure TPostScriptPrinterCanvas.FrameRect(const ARect: TRect);
var
  CL : TColor;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);

  CL:=Pen.Color;
  try
    Pen.Color:=Brush.Color;
    Frame(aRect);
  finally
    Pen.Color:=CL;
  end;

  Changed;
end;

//Fill an Rectangular region
procedure TPostScriptPrinterCanvas.FillRect(const ARect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);


  Writecomment(Format('FillRect(%d,%d,%d,%d)',[Arect.Left,ARect.Top,Arect.Right,ARect.Bottom]));

  psDrawRect(ARect);

  SetBrushFillPattern(False,True);

  MoveToLastPos;
  
  Changed;
end;

procedure TPostScriptPrinterCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX,
  RY: Integer);
var
  ellipsePath : string;
  //fs:TFormatSettings;
  pp1,pp2,r:TpsPoint;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  //fs.DecimalSeparator:='.';
  //fs.ThousandSeparator:=#0;

  X1:=Min(X1,X2);
  X2:=Max(X1,X2);
  Y1:=Min(Y1,Y2);
  Y2:=Max(Y1,Y2);

  writecomment(Format('RoundRect(%d,%d,%d,%d,%d,%d)',[x1,y1,x2,y2,Rx,Ry]));
  pp1:=TranslateCoord(X1,Y1);
  pp2:=TranslateCoord(X2,Y2);

  ClearBuffer;
  
  {Note: arcto command draws a line from current point to beginning of arc
  save current matrix, translate to center of ellipse, scale by rx ry, and draw
  a circle of unit radius in counterclockwise dir, return to original matrix
  arguments are (cx, cy, rx, ry, startAngle, endAngle)}
  ellipsePath:='matrix currentmatrix %f %f translate %f %f scale 0 0 1 %d %d arc setmatrix';

  PixelsToPoints(RX,RY,r.fx,r.fy);
  WriteB('newpath');
  WriteB(Format(ellipsePath,[pp1.fx+r.fx,pp1.fy-r.fy,r.fx,r.fy,90,180],FFs));
  WriteB(Format(ellipsePath,[pp1.fx+r.fx,pp2.fy+r.fy,r.fx,r.fy,180,270],FFs));
  WriteB(Format(ellipsePath,[pp2.fx-r.fx,pp2.fy+r.fy,r.fx,r.fy,270,360],FFs));
  WriteB(Format(ellipsePath,[pp2.fx-r.fx,pp1.fy-r.fy,r.fx,r.fy,0,90],FFs));
  WriteB('closepath');

  SetBrushFillPattern(True,True);
  
  MoveToLastPos;
  Changed;
end;

procedure TPostScriptPrinterCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean);
var
  i  : LongInt;
  Pt : TPoint;
  pp:TpsPoint;
begin
  if (NumPts<=1) or not Assigned(Points) then Exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  ClearBuffer;
  
  Pt:=Points[0];
  pp:=TranslateCoord(Pt.x,Pt.y);
  WriteB('newpath');
  WriteB(Format('%f %f moveto',[pp.fx,pp.fy],FFs));
  for i:=1 to NumPts-1 do
  begin
    Pt:=Points[i];
    pp:=TranslateCoord(Pt.x,Pt.y);
    WriteB(Format('%f %f lineto',[pp.fx,pp.fy], FFs));
  end;
  WriteB('closepath');

  SetBrushFillPattern(True,True);
  
  MoveToLastPos;
  Changed;
end;

//Draw an Ellipse
procedure TPostScriptPrinterCanvas.Ellipse(x1, y1, x2, y2: Integer);
var xScale : Real;
    yScale : Real;
    cX, cY : Real;
    rX,Ry  : Real;
    Code   : string;
    stAng  : Integer;
    ang    : Integer;
    //fs:TFormatSettings;
    pp1,pp2:TpsPoint;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  //fs.DecimalSeparator:='.';
  //fs.ThousandSeparator:=#0;

  writecomment(Format('Ellipse(%d,%d,%d,%d)',[x1,y1,x2,y2]));
  pp1:=TranslateCoord(X1,Y1);
  pp2:=TranslateCoord(X2,Y2);

  //Init
  StAng:=0;
  Ang:=360;

  //calculate centre of ellipse
  cx:=(pp1.fx+pp2.fx)/2;
  cy:=(pp1.fy+pp2.fy)/2;
  rx:=(pp2.fx-pp1.fx)/2;
  ry:=(pp2.fy-pp1.fy)/2;
  
  //calculate semi-minor and semi-major axes of ellipse
  xScale:=Abs((pp2.fx-pp1.fx)/2.0);
  yScale:=Abs((pp2.fy-pp1.fy)/2.0);

  Code:=Format('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %d %d %s setmatrix',
      [cX,cY,xScale,yScale,StAng,Ang,'arc'],FFs);
      
  ClearBuffer;
  WriteB(Format('%.3f %.3f moveto',[cX,cY],FFs)); //move to center of circle
  WriteB(Code);
  SetBrushFillPattern(False,True);

  //move current point to start of arc, note negative
  //angle because y increases down
  ClearBuffer;
  WriteB(Format('%.3f %.3f moveto',[cX+(rX*Cos(StAng*-1)),cY+(rY*Sin(StAng*-1))],FFs));
  WriteB(Code);
  SetBrushFillPattern(True,False);

  MoveToLastPos;
  Changed;
end;

//Draw an Arc
procedure TPostScriptPrinterCanvas.Arc(Left,Top,Right,Bottom, angle1,
  angle2: Integer);
var xScale : Real;
    yScale : Real;
    cX, cY : Real;
    rX,Ry  : Real;
    Code   : string;
    ang    : string;
    //fs:TFormatSettings;
    pp1,pp2:TpsPoint;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  //fs.DecimalSeparator:='.';
  //fs.ThousandSeparator:=#0;

  pp1:=TranslateCoord(Left,Top);
  pp2:=TranslateCoord(Right,Bottom);
  TranslateCoord(Right,Bottom);

  //calculate centre of ellipse
  cx:=pp1.fx;
  cy:=pp1.fy;
  rx:=pp2.fx-pp1.fx;
  ry:=pp2.fy-pp1.fy;

  if Angle2>=0 then
    Ang:='arc'
  else
    Ang:='arcn';
    
  //calculate semi-minor and semi-major axes of ellipse
  xScale:=Abs((Right-Left)/2.0);
  yScale:=Abs((Bottom-Top)/2.0);

  Code:=Format('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %.3f %.3f %s setmatrix',
      [cX,cY,xScale,yScale,Angle1/16,Angle2/16,ang], FFs);

  if (Pen.Color<>clNone) and ((Pen.Color<>Brush.Color) or (Brush.Style<>bsSolid)) then
  begin
    UpdateLineColor(clNone);
    UpdateLineWidth;
    UpdateLineStyle;

    //move current point to start of arc, note negative
    //angle because y increases down
    write(Format('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))],FFs));
    Write(Code);
    write('stroke');
  end;

  MoveToLastPos;
  Changed;
end;

procedure TPostScriptPrinterCanvas.RadialPie(Left, Top, Right, Bottom, angle1,
  angle2: Integer);
var xScale : Real;
    yScale : Real;
    cX, cY : Real;
    rX,Ry  : Real;
    Code   : string;
    ang    : string;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  writecomment(Format('RadialPie(%d,%d,%d,%d,%d,%d)',[Left,Top,Right-Left,Bottom-Top,Angle1,Angle2]));
  TranslateCoord(Left,Top);

  //calculate centre of ellipse
  cx:=Left;
  cy:=Top;
  rx:=Right-Left;
  ry:=Bottom-Top;

  if Angle2>=0 then
    Ang:='arc'
  else
    Ang:='arcn';

  //calculate semi-minor and semi-major axes of ellipse
  xScale:=Abs(rx);
  yScale:=Abs(ry);

  Code:=Format('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %.3f %.3f %s setmatrix',
      [cX,cY,xScale,yScale,Angle1/16,Angle2/16,ang],FFs);

  //move current point to start of arc, note negative
  //angle because y increases down
  ClearBuffer;
  writeB(Format('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))],FFs));
  WriteB(Code);
  writeB(Format('%d %d lineto',[Left,Top]));
  writeB(Format('%.3f %.3f lineto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))],FFs));
  SetBrushFillPattern(False,True);

  //move current point to start of arc, note negative
  //angle because y increases down
  ClearBuffer;
  writeB(Format('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))],FFs));
  WriteB(Code);
  writeB(Format('%d %d lineto',[Left,Top]));
  writeB(Format('%.3f %.3f lineto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))],FFs));
  SetBrushFillPattern(True,False);

  MoveToLastPos;
  Changed;
end;

function FontStyleToInt(AStyles: TFontStyles): Integer;
begin
  result := 0;
  if fsBold in AStyles then
    result := result or (1 shl ord(fsBold));
  if fsItalic in AStyles then
    result := result or (1 shl ord(fsItalic));
  if fsStrikeOut in AStyles then
    result := result or (1 shl ord(fsStrikeout));
  if fsUnderline in AStyles then
    result := result or (1 shl ord(fsUnderline));
end;

//Out the text at the X,Y coord. Set the font
procedure TPostScriptPrinterCanvas.TextOut(X, Y: Integer; const Text: String);
var
  PenUnder : Double;
  PosUnder : Integer;
  pp:TpsPoint;
  saved:boolean;
  FontIndex: Integer;

  procedure rotate;
  begin
    write('gsave');
    inc(fSaveCount);
    Self.FPsUnicode.ResetLastFont;
    saved:=true;
    write(format('%.2f rotate',[Font.Orientation / 10],fFS));
  end;

begin
  pp:=TranslateCoord(X,Y);

  UpdateFont;

  FPSUnicode.Font:=MappedFontName;
  FPSUnicode.FontSize:=Abs(GetFontSize);
  FPSUnicode.FontStyle:=FontStyleToInt(Font.Style);

  //The Y origin for ps text it's Left bottom corner (only if not rotated)
  if Font.Orientation=0 then
    pp.fy := pp.fy - abs(GetFontSize) // in points
  else
    pp.fx := pp.fx + abs(GetFontSize); // apply to X axis if rotated

  saved:=false;
  
  if fsUnderline in Font.Style then
  begin
    FontIndex := GetFontIndex;

    PosUnder := FontUnitsToPixelsY(cFontPSMetrics[FontIndex].ULPos);

    // The current heuristics produces better underline thickness
    {$IFDEF UseFontUnderlineThickness}
    PenUnder := FontUnitsToPixelsY(cFontPSMetrics[FontIndex].ULThickness);
    {$else}
    PenUnder:=0.5;
    if fsBold in Font.Style then
      PenUnder:=1.0;
    {$endif}

    Write(format('%f %f uli',[pp.fx,pp.fy],FFs));
    if Font.Orientation<>0 then
      rotate();
    FPSUnicode.OutputString(Text);
    write(Format('%.3f %d ule',[PenUnder,PosUnder],FFs));
  end
  else
  begin
    write(Format('%f %f moveto',[pp.fx,pp.fy],FFs));
    if Font.Orientation<>0 then
      rotate();
    FPSUnicode.OutputString(Text);
  end;

  if saved then
  begin
    write('grestore');
    dec(fSaveCount);
  end;

  MoveToLastPos;
end;

function TPostScriptPrinterCanvas.TextExtent(const Text: string): TSize;
var
  IndexFont,i : Integer;
  c: Char;
begin
  Result.cX := 0;
  Result.cY := 0;
  if Text='' then Exit;
  RequiredState([csHandleValid, csFontValid]);
  Result.cY:=round((Abs(GetFontSize)/72)*YDPI); // points to inches and then to pixels
  // Abs is not right - should also take internal leading into account
  IndexFont := GetFontIndex;
  for i:=1 to Length(Text) do
  begin
    c:=Text[i];
    if (c in [#32..#255]) then
      Inc(Result.cX,cFontPSMetrics[IndexFont].Widths[Ord(c)]);
  end;
  Result.cX:=FontUnitsToPixelsX(Result.cX);
end;

//Draw an Picture
procedure TPostScriptPrinterCanvas.Draw(X, Y: Integer; SrcGraphic: TGraphic);
begin
  if not Assigned(SrcGraphic) then exit;
  StretchDraw(Rect(X,Y,X+SrcGraphic.Width,Y+SrcGraphic.Height),SrcGraphic);
end;

//Draw an picture with scale size
procedure TPostScriptPrinterCanvas.StretchDraw(const DestRect: TRect;  SrcGraphic: TGraphic);
var X1,Y1,X2,Y2 : Integer;
    DrawWidth : single;
    DrawHeight: single;
    ImgWidth  : Integer;
    ImgHeight : Integer;
  pp1,pp2:TpsPoint;
begin
  if not Assigned(SrcGraphic) then exit;
  Changing;
  RequiredState([csHandleValid]);

  X1:=DestRect.Left;
  Y1:=DestRect.Top;
  X2:=DestRect.Right;
  Y2:=DestRect.Bottom;
  
  pp1:=TranslateCoord(X1,Y1);
  pp2:=TransLateCoord(X2,Y2);
  
  ImgWidth:=SrcGraphic.Width;
  ImgHeight:=SrcGraphic.Height;

  //if not FPImage then draw ab Rectangle because other wise PostScript
  //interpreter wait infinite some RGB datas
  DrawWidth:=pp2.fx-pp1.fx;
  DrawHeight:=pp1.fy-pp2.fy;
  ClearBuffer;

  WriteB('gsave');
  WriteB('/DeviceRGB setcolorspace');
  writeB(Format('%f %f translate',[pp1.fx,pp1.fy-DrawHeight],FFs));
  WriteB(Format('%f %f scale',[DrawWidth,DrawHeight],FFs));
  {$IFDEF ASCII85}
  WriteB('<<');
  WriteB('  /ImageType 1');
  WriteB('  /Width '+IntToStr(ImgWidth));
  WriteB('  /Height '+IntToStr(ImgHeight));
  WriteB('  /BitsPerComponent 8');
  WriteB('  /Decode [0 1 0 1 0 1]');
  WriteB('  /ImageMatrix '+Format('[%d %d %d %d %d %d]',[ImgWidth,0,0,-ImgHeight,0,ImgHeight]));
  WriteB('  /DataSource currentfile /ASCII85Decode filter');
  WriteB('>>');
  WriteB('image');
  Write(fBuffer);
  ClearBuffer;
  GetRGBImage(SrcGraphic,fBuffer);
  {$ELSE}
  WriteB(Format('/scanline %d 3 mul string def',[ImgWidth]));
  // colorimage width height bits/comp matrix data0..dataN-1 multi? ncomp colorimage
  WriteB(Format('%d %d %d',[ImgWidth,ImgHeight,8]));
  WriteB(Format('[%d %d %d %d %d %d]',[ImgWidth,0,0,-ImgHeight,0,ImgHeight]));
  WriteB('{ currentfile scanline readhexstring pop } false 3');
  WriteB('colorimage');
  GetRGBImage(SrcGraphic,fBuffer);
  {$ENDIF}
  WriteB('% end of image data');
  WriteB('grestore');
  
  Write(fBuffer);

  Changed;
end;

function TPostScriptPrinterCanvas.GetTextMetrics(out TM: TLCLTextMetric): boolean;
var
  FontIndex: Integer;
begin
  FontIndex := GetFontIndex;
  Result := FontIndex>=0;
  if Result then
  with CFontPSMetrics[FontIndex] do begin
    TM.Ascender := FontUnitsToPixelsY( Ascender );
    TM.Descender := FontUnitsToPixelsY( -Descender );
    TM.Height := TM.Ascender + TM.Descender;
  end;
end;

procedure TPostScriptPrinterCanvas.Arc(x, y, Right, Bottom, SX, SY, EX,
  EY: Integer);
begin
  //Not implemented
end;

procedure TPostScriptPrinterCanvas.Chord(x1, y1, x2, y2, angle1,angle2: Integer);
var xScale : Real;
    yScale : Real;
    cX, cY : Real;
    rX,Ry  : Real;
    Code   : string;
    ang    : string;
  //pp:TpsPoint;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  writecomment(Format('Chord(%d,%d,%d,%d,%d,%d)',[x1,y1,x2-x1,y2-y1,Angle1,Angle2]));
  //pp:=TranslateCoord(x1, y1);

  //calculate centre of ellipse
  cx:=x1;
  cy:=y1;
  rx:=x2-x1;
  ry:=y2-y1;

  if Angle2>=0 then
    Ang:='arc'
  else
    Ang:='arcn';

  //calculate semi-minor and semi-major axes of ellipse
  xScale:=Abs(rx);
  yScale:=Abs(ry);

  Code:=Format('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %.3f %.3f %s setmatrix',
      [cX,cY,xScale,yScale,Angle1/16,Angle2/16,ang],FFs);

  //move current point to start of arc, note negative
  //angle because y increases down.ClosePath for draw chord
  ClearBuffer;
  writeB('newpath');
  writeB(Format('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))],FFs));
  WriteB(Code);
  writeB('closepath');
  SetBrushFillPattern(True,True);

  MoveToLastPos;
  Changed;
end;

procedure TPostScriptPrinterCanvas.Chord(x1, y1, x2, y2, SX, SY, EX, EY: Integer);
begin
  //Not implemented
end;

procedure TPostScriptPrinterCanvas.Frame3d(var ARect: TRect;
  const FrameWidth: integer; const Style: TGraphicsBevelCut);
begin
  //Not implemented
end;

procedure TPostScriptPrinterCanvas.Pie(EllipseX1, EllipseY1, EllipseX2,
  EllipseY2, StartX, StartY, EndX, EndY: Integer);
begin
//Not implemented
end;

procedure TPostScriptPrinterCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  //Not implemented
end;

procedure TPostScriptPrinterCanvas.TextRect(ARect: TRect; X, Y: integer;
  const Text: string; const Style: TTextStyle);
var
  OldClip: TRect;
  Options: longint;
  ReqState: TCanvasState;
  fRect: TRect;
  Offset: Integer;

  procedure WordWrap(AText: PChar; MaxWidthInPixel: integer;
    out Lines: PPChar; out LineCount: integer);

    function FindLineEnd(LineStart: integer): integer;
    var
      CharLen, LineStop, LineWidth, WordWidth, WordEnd, CharWidth: integer;
    begin
      // first search line break or text break
      Result := LineStart;
      while not (AText[Result] in [#0, #10, #13]) do
        Inc(Result);
      if Result <= LineStart + 1 then
        exit;
      lineStop := Result;

      // get current line width in pixel
      LineWidth := TextWidth(AText);
      if LineWidth > MaxWidthInPixel then
      begin
        // line too long -> add words till line size reached
        LineWidth := 0;
        WordEnd := LineStart;
        WordWidth := 0;
        repeat
          Result := WordEnd;
          Inc(LineWidth, WordWidth);
          // find word start
          while AText[WordEnd] in [' ', #9] do
            Inc(WordEnd);
          // find word end
          while not (AText[WordEnd] in [#0, ' ', #9, #10, #13]) do
            Inc(WordEnd);
          // calculate word width
          if wordEnd = Result then break;
          WordWidth := TextWidth(MidStr(AText, Result, WordEnd - Result));
        until LineWidth + WordWidth > MaxWidthInPixel;
        if LineWidth = 0 then
        begin
          // the first word is longer than the maximum width
          // -> add chars till line size reached
          Result := LineStart;
          LineWidth := 0;
          repeat
            charLen := UTF8CodepointSize(@AText[Result]);
            CharWidth := TextWidth(MidStr(AText, Result, charLen));
            Inc(LineWidth, CharWidth);
            if LineWidth > MaxWidthInPixel then
              break;
            if Result >= lineStop then
              break;
            Inc(Result, charLen);
          until False;
          // at least one char
          if Result = LineStart then
          begin
            charLen := UTF8CodepointSize(@AText[Result]);
            Inc(Result, charLen);
          end;
        end;
      end;
    end;

    function IsEmptyText: boolean;
    begin
      if (AText = nil) or (AText[0] = #0) then
      begin
        // no text
        GetMem(Lines, SizeOf(PChar));
        Lines[0] := nil;
        LineCount := 0;
        Result := True;
      end
      else
        Result := False;
    end;

  var
    LinesList: TIntegerList;
    LineStart, LineEnd, LineLen: integer;
    ArraySize, TotalSize: integer;
    i: integer;
    CurLineEntry: PPChar;
    CurLineStart: PChar;
  begin
    if IsEmptyText then
    begin
      Lines := nil;
      LineCount := 0;
      exit;
    end;
    LinesList := TIntegerList.Create;
    LineStart := 0;

    // find all line starts and line ends
    repeat
      LinesList.Add(LineStart);
      // find line end
      LineEnd := FindLineEnd(LineStart);
      LinesList.Add(LineEnd);
      // find next line start
      LineStart := LineEnd;
      if AText[LineStart] in [#10, #13] then
      begin
        // skip new line chars
        Inc(LineStart);
        if (AText[LineStart] in [#10, #13]) and
          (AText[LineStart] <> AText[LineStart - 1]) then
          Inc(LineStart);
      end
      else if AText[LineStart] in [' ', #9] then
      begin
        // skip space
        while AText[LineStart] in [' ', #9] do
          Inc(LineStart);
      end;
    until AText[LineStart] = #0;

    // create mem block for 'Lines': array of PChar + all lines
    LineCount := LinesList.Count shr 1;
    ArraySize := (LineCount + 1) * SizeOf(PChar);
    TotalSize := ArraySize;
    i := 0;
    while i < LinesList.Count do
    begin
      // add  LineEnd - LineStart + 1 for the #0
      LineLen := LinesList[i + 1] - LinesList[i] + 1;
      Inc(TotalSize, LineLen);
      Inc(i, 2);
    end;
    GetMem(Lines, TotalSize);
    FillChar(Lines^, TotalSize, 0);

    // create Lines
    CurLineEntry := Lines;
    CurLineStart := PChar(CurLineEntry) + ArraySize;
    i := 0;
    while i < LinesList.Count do
    begin
      // set the pointer to the start of the current line
      CurLineEntry[i shr 1] := CurLineStart;
      // copy the line
      LineStart := LinesList[i];
      LineEnd := LinesList[i + 1];
      LineLen := LineEnd - LineStart;
      if LineLen > 0 then
        Move(AText[LineStart], CurLineStart^, LineLen);
      Inc(CurLineStart, LineLen);
      // add #0 as line end
      CurLineStart^ := #0;
      Inc(CurLineStart);
      // next line
      Inc(i, 2);
    end;
    CurLineEntry[i shr 1] := nil;

    LinesList.Free;
  end;

  function DrawText(Str: PChar; Count: integer; var Rect: TRect;
    Flags: cardinal): integer;
  const
    TabString = '        ';
  var
    pIndex: longint;
    AStr: string;

    TM: TLCLTextmetric;
    theRect: TRect;
    Lines: PPChar;
    I, NumLines: longint;

    l: longint;
    Pt: TPoint;
    SavedRect: TRect; // if font orientation <> 0

    function LeftOffset: longint;
    begin
      if (Flags and DT_RIGHT) = DT_RIGHT then
        Result := DT_RIGHT
      else
      if (Flags and DT_CENTER) = DT_CENTER then
        Result := DT_CENTER
      else
        Result := DT_LEFT;
    end;

    function TopOffset: longint;
    begin
      if (Flags and DT_BOTTOM) = DT_BOTTOM then
        Result := DT_BOTTOM
      else
      if (Flags and DT_VCENTER) = DT_VCENTER then
        Result := DT_VCENTER
      else
        Result := DT_TOP;
    end;

    function CalcRect: boolean;
    begin
      Result := (Flags and DT_CALCRECT) = DT_CALCRECT;
    end;


    procedure DoCalcRect;
    var
      AP: TSize;
      J, MaxWidth, LineWidth: integer;
    begin
      theRect := Rect;

      MaxWidth := theRect.Right - theRect.Left;

      if (Flags and DT_SINGLELINE) > 0 then
      begin
        // ignore word and line breaks
        AP := TextExtent(PChar(AStr));
        theRect.Bottom := theRect.Top + TM.Height;
        if (Flags and DT_CALCRECT) <> 0 then
          theRect.Right := theRect.Left + AP.cX
        else
        begin
          theRect.Right := theRect.Left + Min(MaxWidth, AP.cX);
          if (Flags and DT_VCENTER) > 0 then
          begin
            OffsetRect(theRect, 0, ((Rect.Bottom - Rect.Top) -
              (theRect.Bottom - theRect.Top)) div 2);
          end
          else
          if (Flags and DT_BOTTOM) > 0 then
          begin
            OffsetRect(theRect, 0, (Rect.Bottom - Rect.Top) -
              (theRect.Bottom - theRect.Top));
          end;
        end;
      end
      else
      begin
        // consider line breaks
        if (Flags and DT_WORDBREAK) = 0 then
        begin
          // do not break at word boundaries
          AP := TextExtent(PChar(AStr));
          MaxWidth := AP.cX;
        end;
        WordWrap(PChar(AStr), MaxWidth, Lines, NumLines);

        if (Flags and DT_CALCRECT) <> 0 then
        begin
          LineWidth := 0;
          if (Lines <> nil) then
          begin
            for J := 0 to NumLines - 1 do
            begin
              AP := TextExtent(Lines[J]);
              LineWidth := Max(LineWidth, AP.cX);
            end;
          end;
          LineWidth := Min(MaxWidth, LineWidth);
        end
        else
          LineWidth := MaxWidth;

        theRect.Right := theRect.Left + LineWidth;
        theRect.Bottom := theRect.Top + NumLines * TM.Height;
        if NumLines > 1 then
          Inc(theRect.Bottom, ((NumLines - 1) * TM.Descender));// space between lines
      end;

      if not CalcRect then
        case LeftOffset of
          DT_CENTER:
          begin
            Offset := (Rect.Right - theRect.Right) div 2;
            OffsetRect(theRect, offset, 0);
          end;
          DT_RIGHT:
          begin
            Offset := Rect.Right - theRect.Right;
            OffsetRect(theRect, offset, 0);
          end;
        end;
    end;

    // if our Font.Orientation <> 0 we must recalculate X,Y offset
    // also it works only with DT_TOP DT_LEFT.
    procedure CalculateOffsetWithAngle(const AFontAngle: integer;
    var TextLeft, TextTop: integer);
    var
      OffsX, OffsY: integer;
      Angle: integer;
      Size: TSize;
      R: TRect;
    begin
      R := SavedRect;
      OffsX := R.Right - R.Left;
      OffsY := R.Bottom - R.Top;
      Size.cX := OffsX;
      Size.cy := OffsY;
      Angle := AFontAngle div 10;
      if Angle < 0 then
        Angle := 360 + Angle;

      if Angle <= 90 then
      begin
        OffsX := 0;
        OffsY := Trunc(Size.cx * sin(Angle * Pi / 180));
      end
      else
      if Angle <= 180 then
      begin
        OffsX := Trunc(Size.cx * -cos(Angle * Pi / 180));
        OffsY := Trunc(Size.cx * sin(Angle * Pi / 180) + Size.cy *
          cos((180 - Angle) * Pi / 180));
      end
      else
      if Angle <= 270 then
      begin
        OffsX := Trunc(Size.cx * -cos(Angle * Pi / 180) + Size.cy *
          sin((Angle - 180) * Pi / 180));
        OffsY := Trunc(Size.cy * sin((270 - Angle) * Pi / 180));
      end
      else
      if Angle <= 360 then
      begin
        OffsX := Trunc(Size.cy * sin((360 - Angle) * Pi / 180));
        OffsY := 0;
      end;
      TextTop := OffsY;
      TextLeft := OffsX;
    end;

    function NeedOffsetCalc: boolean;
    begin
      Result := (Font.Orientation <> 0) and (Flags and DT_SINGLELINE <> 0) and
        (Flags and DT_VCENTER = 0) and (Flags and DT_CENTER = 0) and
        (Flags and DT_RIGHT = 0) and (Flags and DT_BOTTOM = 0) and
        (Flags and DT_CALCRECT = 0) and not IsRectEmpty(SavedRect);
    end;


    procedure DrawLineRaw(theLine: PChar; LineLength, TopPos: longint);
    var
      Points: array[0..1] of TSize;
      LeftPos: longint;
    begin
      if LeftOffset <> DT_LEFT then
        Points[0] := TextExtent(theLine)
      else begin
        Points[0].cx := 0;
        Points[0].cy := 0;
      end;

      case LeftOffset of
        DT_LEFT:
          LeftPos := theRect.Left;
        DT_CENTER:
          LeftPos := theRect.Left + (theRect.Right - theRect.Left) div
            2 - Points[0].cX div 2;
        DT_RIGHT:
          LeftPos := theRect.Right - Points[0].cX;
        else
          LeftPos := 0;
      end;

      Pt := Point(0, 0);
      // Draw line of Text
      if NeedOffsetCalc then
      begin
        Pt.X := SavedRect.Left;
        Pt.Y := SavedRect.Top;
        CalculateOffsetWithAngle(Font.Orientation, Pt.X, Pt.Y);
      end;
      TextOut(LeftPos + Pt.X, TopPos + Pt.Y, theLine);
    end;

    procedure DrawLine(theLine: PChar; LineLength, TopPos: longint);
    var
      Points: array[0..1] of TSize;
      //LogP: TLogPen;
      LeftPos: longint;
    begin
      FillByte({%H-}Points[0], SizeOf(Points[0]) * 2, 0);
      if LeftOffset <> DT_Left then
        Points[0] := TextExtent(theLine);

      case LeftOffset of
        DT_LEFT:
          LeftPos := theRect.Left;
        DT_CENTER:
          LeftPos := theRect.Left + (theRect.Right - theRect.Left) div
            2 - Points[0].cX div 2;
        DT_RIGHT:
          LeftPos := theRect.Right - Points[0].cX;
        else
          LeftPos := 0;
      end;

      Pt := Point(0, 0);
      if NeedOffsetCalc then
      begin
        Pt.X := SavedRect.Left;
        Pt.Y := SavedRect.Top;
        CalculateOffsetWithAngle(Font.Orientation, Pt.X, Pt.Y);
      end;
      // Draw line of Text
      TextOut(LeftPos + Pt.X, TopPos + Pt.Y, theLine);

      // Draw Prefix
      if (pIndex > 0) and (pIndex <= LineLength) then
      begin
        //LogP.lopnStyle := PS_SOLID;
        //LogP.lopnWidth.X := 1;
        //LogP.lopnColor := FcPenColor;   // FIXME is this required?

        {Get prefix line position}
        Points[0] := TextExtent(theLine);
        Points[0].cX := LeftPos + Points[0].cX;
        Points[0].cY := TopPos + tm.Height - TM.Descender + 1;

        Points[0] := TextExtent(aStr[pIndex]);
        Points[1].cX := Points[0].cX + Points[1].cX;
        Points[1].cY := Points[0].cY;

        {Draw prefix line}
        Polyline(PPoint(@Points[0]), 2);
      end;
    end;

  begin
    if (Str = nil) or (Str[0] = #0) then
      Exit(0);

    if (Count < -1) or (IsRectEmpty(Rect) and
      ((Flags and DT_CALCRECT = 0) and (Flags and DT_NOCLIP = 0))) then
      Exit(0);

    // Don't try to use StrLen(Str) in cases count >= 0
    // In those cases str is NOT required to have a null terminator !
    if Count = -1 then
      Count := StrLen(Str);

    Lines := nil;
    NumLines := 0;

    try
      if (Flags and (DT_SINGLELINE or DT_CALCRECT or DT_NOPREFIX or
        DT_NOCLIP or DT_EXPANDTABS)) = (DT_SINGLELINE or DT_NOPREFIX or
        DT_NOCLIP) then
      begin
        LCLIntf.CopyRect(theRect,  Rect);
        SavedRect := Rect;
        DrawLineRaw(Str, Count, Rect.Top);
        Result := Rect.Bottom - Rect.Top;
        Exit;
      end;

      SetLength(AStr, Count);
      if Count > 0 then
        System.Move(Str^, AStr[1], Count);

      if (Flags and DT_EXPANDTABS) <> 0 then
        AStr := StringReplace(AStr, #9, TabString, [rfReplaceAll]);


      if (Flags and DT_NOPREFIX) <> DT_NOPREFIX then
        pIndex := DeleteAmpersands(AStr)
      else
        pIndex := -1;


      GetTextMetrics(TM{%H-});
      DoCalcRect;
      Result := theRect.Bottom - theRect.Top;
      if (Flags and DT_CALCRECT) = DT_CALCRECT then
      begin
        LCLIntf.CopyRect(Rect, theRect);
        exit;
      end;

      if (Flags and DT_NOCLIP) <> DT_NOCLIP then
      begin
        if theRect.Right > Rect.Right then
          theRect.Right := Rect.Right;
        if theRect.Bottom > Rect.Bottom then
          theRect.Bottom := Rect.Bottom;
// FIXME  I don't know what to do here
//          IntersectClipRect( theRect.Left, theRect.Top,
//          theRect.Right, theRect.Bottom);
      end;

      if (Flags and DT_SINGLELINE) = DT_SINGLELINE then
      begin
        SavedRect := TheRect;
        DrawLine(PChar(AStr), length(AStr), theRect.Top);
        Exit;
      end;

      // multiple lines
      if Lines = nil then
        Exit;  // nothing to do
      if NumLines = 0 then
        Exit;

      SavedRect := Classes.Rect(0, 0, 0, 0);
      // no font orientation change if multilined text
      for i := 0 to NumLines - 1 do
      begin
        if theRect.Top > theRect.Bottom then
          Break;

        if ((Flags and DT_EDITCONTROL) = DT_EDITCONTROL) and
          (tm.Height > (theRect.Bottom - theRect.Top)) then
          Break;

        if Lines[i] <> nil then
        begin
          l := StrLen(Lines[i]);
          DrawLine(Lines[i], l, theRect.Top);
          Dec(pIndex, l + length(LineEnding));
        end;
        Inc(theRect.Top, (TM.Descender + TM.Height));// space between lines
      end;

    finally
      Reallocmem(Lines, 0);
    end;
  end;

begin
  //TODO: layout, etc.
  Changing;

  Options := 0;
  case Style.Alignment of
    taRightJustify:
      Options := DT_RIGHT;
    taCenter:
      Options := DT_CENTER;
  end;
  case Style.Layout of
    tlCenter:
      Options := Options or DT_VCENTER;
    tlBottom:
      Options := Options or DT_BOTTOM;
  end;
  if Style.EndEllipsis then
    Options := Options or DT_END_ELLIPSIS;
  if Style.WordBreak then
  begin
    Options := Options or DT_WORDBREAK;
    if Style.EndEllipsis then
      Options := Options and not DT_END_ELLIPSIS;
  end;

  if Style.SingleLine then
    Options := Options or DT_SINGLELINE;

  if not Style.Clipping then
    Options := Options or DT_NOCLIP;

  if Style.ExpandTabs then
    Options := Options or DT_EXPANDTABS;

  if not Style.ShowPrefix then
    Options := Options or DT_NOPREFIX;

  if Style.RightToLeft then
    Options := Options or DT_RTLREADING;

  ReqState := [csHandleValid];
  if not Style.SystemFont then
    Include(ReqState, csFontValid);
  if Style.Opaque then
    Include(ReqState, csBrushValid);

  // calculate text rectangle
  fRect := ARect;
  if Style.Alignment = taLeftJustify then
    fRect.Left := X;
  if Style.Layout = tlTop then
    fRect.Top := Y;

  if (Style.Alignment in [taRightJustify, taCenter]) or
    (Style.Layout in [tlCenter, tlBottom]) then
  begin
    DrawText( pChar(Text), Length(Text), fRect, DT_CALCRECT or Options);
    case Style.Alignment of
      taRightJustify:
      begin
        Offset := ARect.Right - fRect.Right;
        LCLIntf.OffsetRect(fRect, Offset, 0);
      end;
      taCenter:
      begin
        Offset :=  (ARect.Right - fRect.Right) div 2;
        LCLIntf.OffsetRect(fRect, offset, 0);
      end;
    end;
    case Style.Layout of
      tlCenter:
      begin
        Offset :=  ((ARect.Bottom - ARect.Top) - (fRect.Bottom - fRect.Top)) div 2;
        LCLIntf.OffsetRect(fRect, 0, offset);
      end;
      tlBottom:
      begin
        Offset :=  ARect.Bottom - fRect.Bottom;
        LCLIntf.OffsetRect(fRect, 0, offset);
      end;
    end;
  end;

  if Style.Clipping then begin
    OldClip := GetClipRect;
    SetClipRect(ARect);
    Options := Options or DT_NOCLIP; // no clipping as we are handling it here
  end;

  if Style.Opaque then
  begin
    FillRect(fRect)
  end;

  if Style.SystemFont then
    UpdateFont();

  DrawText(PChar(Text), Length(Text), fRect, Options);

  if Style.Clipping then
    SetClipRect(OldClip);

  Changed;

end;


function IsMaxClip(ARect:TRect):boolean;
begin
  Result:=(Arect.Right=MaxInt) and (ARect.Bottom=MaxInt) and (Arect.Left=0) and (ARect.Top=0);
end;

procedure TPostScriptPrinterCanvas.SetClipRect(const ARect:TRect);
begin
  if pcsClipping in FStatus then
    RestoreClip;

  FLazClipRect := ARect;

  if pcsClipping in FStatus then
    SaveClip;
end;

function TPostScriptPrinterCanvas.GetClipping: Boolean;
begin
  result := (pcsClipping in FStatus);
end;

procedure TPostScriptPrinterCanvas.SetClipping(const AValue: boolean);
begin
  if GetClipping<>AValue then
  begin
    if GetClipping then
      RestoreClip
    else
      SaveClip;
    if AValue then
      Include(FStatus, pcsClipping)
    else
      Exclude(FStatus, pcsClipping);
  end;
end;

procedure TPostScriptPrinterCanvas.FloodFill(X, Y: Integer; FillColor: TColor;  FillStyle: TFillStyle);
begin
  //Not implemented
end;

procedure TPostScriptPrinterCanvas.CopyRect(const Dest: TRect;
  SrcCanvas: TCanvas; const Source: TRect);
begin
  //Not implemented
end;

{ TPostScriptCanvas }

constructor TPostScriptCanvas.Create;
begin
  Inherited Create(nil);
end;

procedure TPostScriptCanvas.BeginDoc;
begin
  inherited BeginDoc;
end;

procedure TPostScriptCanvas.EndDoc;
begin
  inherited EndDoc;
end;

procedure TPostScriptCanvas.NewPage;
begin
  inherited NewPage;
end;

end.
