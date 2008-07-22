{
 /***************************************************************************
                                PostScriptCanvas.pas
                                ------------
                         PostScript Printer Canvas object

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
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

unit PostScriptCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Types, Graphics, Forms, GraphMath, GraphType,
  FPImage, IntfGraphics, Printers, LCLType, LCLIntf;
  
Type

  { TPostScriptPrinterCanvas }

  TPostScriptPrinterCanvas = Class(TPrinterCanvas)
  private
    fHeader        : TStringList; //Header document
    fDocument      : TstringList; //Current document
    fFileName      : String;      //OutOut fileName

    fBuffer        : TStringList; //PostScript temporary buffer

    //Current values
    fcBrushStyle   : TBrushStyle;
    fcPenColor     : TColor;      //Color of Pen and Brush
    fcPenWidth     : Integer;
    fcPenStyle     : TPenStyle;
    fcLastFont     : TFont;
    
    fPenPos        : TPoint;
    
    FirstUpdatefont: Boolean;
    
    procedure WriteHeader(St : String);
    procedure Write(const St : String; Lst : TstringList = nil); overload;
    procedure WriteB(const St : string);
    procedure ClearBuffer;
    procedure Write(Lst : TStringList); overload;
    procedure WriteComment(const St : string);
    
    procedure TranslateCoord(Var X,Y : Integer);
    procedure SetPosition(X,Y : Integer);
    
    procedure UpdateLineWidth;
    procedure UpdateLineColor(aColor : TColor = clNone);
    procedure UpdateLineStyle;
    procedure UpdateFillColor;
    procedure UpdateFont;
    function MappedFontName: string;
    function MapedString(const St : string):string;
    
    procedure MoveToLastPos;
    procedure SetBrushFillPattern(Lst : TStringList; SetBorder,SetFill : Boolean);
    procedure SetBrushFillPattern(SetBorder,SetFill : Boolean); overload;
    
    procedure GetRGBImage(SrcGraph: TGraphic; Lst : TStringList);
    function  PPCFormat (const Fmt : string; const Args : array of const) : string;
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
    
    procedure BeginDoc; override;
    procedure EndDoc;   override;
    procedure NewPage;  override;
  public
    constructor Create(APrinter : TPrinter); override;
    destructor Destroy; override;
    
    procedure SaveToFile(aFileName : string);
    

    procedure MoveTo(X1,Y1: Integer); override;
    procedure LineTo(X1,Y1: Integer); override;
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
    
    procedure Draw(X,Y: Integer; SrcGraphic: TGraphic); override;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TGraphic); override;


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
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
                       const Style: TTextStyle); override;


    property OutPutFileName : string read fFileName write fFileName;
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
    Widths : TFontsWidths;
  end;

Const
  cBrushStyle : Array[TBrushStyle] of String =
     ('bsClear', 'bsSolid', 'bsBDiagonal',
      'bsFDiagonal', 'bsCross', 'bsDiagCross',
      'bsHorizontal', 'bsVertical', 'bsImage', 'bsPattern');


  cFontPSMetrics : Array[0..12] of TFontPSMetrics =(
    (Name  : 'CourierISO';
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
    (Name  : 'CourierISO-Bold';
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
    (Name  : 'CourierISO-Oblique';
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
    (Name  : 'CourierISO-BoldOblique';
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
    (Name  : 'HelveticaISO';
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
    (Name  : 'HelveticaISO-Bold';
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
    (Name  : 'HelveticaISO-Oblique';
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
   (Name  : 'HelveticaISO-BoldOblique';
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
   (Name  : 'Times-RomanISO';
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
  (Name  : 'TimesISO-Bold';
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
  (Name  : 'TimesISO-Italic';
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
  (Name  : 'TimesISO-BoldItalic';
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

{ TPostScriptPrinterCanvas }

//Write an instruction in the header of document
procedure TPostScriptPrinterCanvas.WriteHeader(St: String);
begin
  fHeader.Add(St);
end;

//Write an instruction in the document
procedure TPostScriptPrinterCanvas.Write(const St: String; Lst : TStringList = Nil);
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

//Convert an TCanvas Y point to PostScript Y point
//The TCanvas origine is corner Left,Top and PostScript is Left,Bottom
//Modify X and Y for use Left and Top margin
procedure TPostScriptPrinterCanvas.TranslateCoord(var X,Y : Integer);
begin
  Y:=PageHeight+BottomMargin-Y;
  X:=X+LeftMargin;
end;

//Save the last position
procedure TPostScriptPrinterCanvas.SetPosition(X, Y: Integer);
begin
  fPenPos:= Point(X,Y);
  SetInternalPenPos(Point(X,Y));
end;

//Init the width of line
procedure TPostScriptPrinterCanvas.UpdateLineWidth;
begin
  if Pen.Width<>fcPenWidth then
  begin
    Write(Format('%d setlinewidth',[Pen.Width]));
    fcPenWidth:=Pen.Width;
  end;
end;

//Init the color of line (pen)
procedure TPostScriptPrinterCanvas.UpdateLineColor(aColor : TColor = clNone);
Var R,G,B    : Real;
    RGBColor : TColor;
begin
  if aColor=clNone then
    aColor:=Pen.Color;
    
  if aColor<>fcPenColor then
  begin
    RGBColor:=ColorToRGB(aColor);
    
    R:=Red(RGBColor)/255;
    G:=Green(RGBColor)/255;
    B:=Blue(RGBColor)/255;
    Write(PPCFormat('%.3f %.3f %.3f setrgbcolor',[R,G,B])+' % '+ColorToString(aColor));
    fcPenColor:=aColor;
  end;
end;

//Init the style of line
procedure TPostScriptPrinterCanvas.UpdateLineStyle;
Var st : string;
begin
  if (Pen.Style<>fcPenStyle) and (Pen.Style<>psClear) then
  begin
    Case Pen.Style of
      psSolid      : St:='[] 0';
      psDash       : St:='[5 2] 0';
      psDot        : St:='[1 3] 0';
      psDashDot    : St:='[5 2 2 2] 0';
      psDashDotDot : St:='[5 2 2 2 2 2] 0';
    end;
    
    Write(Format('%s setdash',[St]));
    fcPenStyle:=Pen.Style;
  end;
end;

//Init the color for fill
procedure TPostScriptPrinterCanvas.UpdateFillColor;
Var R,G,B    : Real;
    RGBColor : TColor;
begin
  if (Brush.Style=bsSolid) and (Brush.Color<>fcPenColor) then
  begin
    RGBColor:=ColorToRGB(Brush.Color);

    R:=Red(RGBColor)/255;
    G:=Green(RGBColor)/255;
    B:=Blue(RGBColor)/255;
    Write(PPCFormat('%.3f %.3f %.3f setrgbcolor',[R,G,B])+' % '+ColorToString(Brush.Color));
    fcPenColor:=Brush.Color;
  end;
end;

//Update current font
procedure TPostScriptPrinterCanvas.UpdateFont;
Var R,G,B    : Real;
    RGBColor : TColor;
begin
  try
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

      Write(PPCFormat('%.3f %.3f %.3f setrgbcolor',[R,G,B])+' % '+ColorToString(Font.Color));
      fcPenColor:=Font.Color;
    end;

    if (Font.Name<>fcLastFont.Name) or (Font.Size<>fcLastFont.Size) or
       (Font.Style<>fcLastFont.Style) or FirstUpdatefont then
    begin
      FirstUpdatefont:=False;
      Write(Format('/%s findfont %d scalefont setfont',[MappedFontName,Font.Size]));
    end;
  finally
    fcLastFont.Assign(Font);
  end;
end;

//Return an PostScript font Name
function TPostScriptPrinterCanvas.MappedFontName: string;
Var Atr : string;
begin
  Atr:='';
  Result:='HelveticaISO';
  if Copy(LowerCase(Font.Name),1,5)='times' then
    Result:='TimesISO';
  if (LowerCase(Font.Name)='monospaced') or (Copy(LowerCase(Font.Name),1,7)='courier') then
    Result:='CourierISO';
  if LowerCase(Font.Name)='serif' then
    Result:='TimesISO';
  if LowerCase(Font.Name)='sansserif' then
    Result:='HelveticaISO';
  if LowerCase(Font.Name)='symbol' then
    Result:='Symbol';

  if (fsBold in Font.Style)  and ((Pos('Courier',Result)=1) or (Pos('Helvetica',Result)=1) or (Pos('Times',Result)=1)) then
    Atr:=Atr+'Bold';
  if (fsItalic in Font.Style) and ((Pos('Courier',Result)=1) or (Pos('Helvetica',Result)=1)) then
    Atr:=Atr+'Oblique';
  if (fsItalic in Font.Style) and (Pos('Times',Result)=1)  then
    Atr:=Atr+'Italic';
  if (Result+Atr='Times') or (Result+Atr='TimesISO') then
    Result:='Times-RomanISO';

  WriteComment(Format('MapedFontName "%s" -> "%s"',[Font.Name,Result]));
  
  if Atr <> '' then
    Result:=Result+'-'+Atr;
end;

//Replace the controls chars by PostScript string
function TPostScriptPrinterCanvas.MapedString(const St: string): string;
begin
  Result:=St;
  Result:=StringReplace(Result,'\','\\',[rfReplaceAll]);
  Result:=StringReplace(Result,'(','\(',[rfReplaceAll]);
  Result:=StringReplace(Result,')','\)',[rfReplaceAll]);
  Result:=StringReplace(Result,#10,'\n',[rfReplaceAll]);
  Result:=StringReplace(Result,#13,'\r',[rfReplaceAll]);
  Result:=StringReplace(Result,#8, '\b',[rfReplaceAll]);
  Result:=StringReplace(Result,#9, '\t',[rfReplaceAll]);
end;

//Move pen at last pos
procedure TPostScriptPrinterCanvas.MoveToLastPos;
begin
  write(Format('%d %d moveto',[fPenPos.X,fPenPos.Y])+' %last pos');
end;

//Add at the PstScript sequence, the Fill Pattern/Color and Broder
//Use SetBorder and SetFill for initialize 1 or 2 sequence
procedure TPostScriptPrinterCanvas.SetBrushFillPattern(Lst: TStringList;
  SetBorder, SetFill: Boolean);
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
           write(Format('/%s findfont  %% a pattern font patternfill',[cBrushStyle[Brush.Style]]));
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
  px, py     : Integer;
  CurColor   : TFPColor;
  St         : String;
begin
  if (SrcGraph is TBitMap) then
  begin
    SrcIntfImg:=TLazIntfImage.Create(0,0);
    Lst.BeginUpdate;
    Try
      SrcIntfImg.LoadFromBitmap(TBitMap(SrcGraph).Handle,TBitMap(SrcGraph).MaskHandle);
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
    finally
      Lst.EndUpdate;
      SrcIntfImg.Free;
    end;
  end;
end;

function TPostScriptPrinterCanvas.PPCFormat(const Fmt: string;
  const Args: array of const): string;
var
  OldDecimalSeparator: char;
begin
  OldDecimalSeparator := DecimalSeparator;
  DecimalSeparator := '.';
  result := Format(Fmt, Args);
  DecimalSeparator := OldDecimalSeparator;
end;

procedure TPostScriptPrinterCanvas.CreateHandle;
begin
  SetHandle(1); // set dummy handle
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


constructor TPostScriptPrinterCanvas.Create(APrinter: TPrinter);
begin
  inherited Create(APrinter);
  
  fcBrushStyle:=bsClear;
  fcPenColor  :=clBlack;
  fcPenWidth  :=0;
  fcPenStyle  :=psSolid;
  fcLastFont  :=TFont.Create;
  
  fHeader:=TStringList.Create;
  fBuffer:=TstringList.Create;
  fDocument:=TStringList.Create;
end;

destructor TPostScriptPrinterCanvas.Destroy;
begin
  fBuffer.Free;
  fHeader.Free;
  fDocument.Free;
  fcLastFont.Free;
  
  inherited Destroy;
end;

procedure TPostScriptPrinterCanvas.SaveToFile(aFileName: string);
Var Lst : TStringList;
begin
  Lst:=TStringList.Create;
  try
     Lst.AddStrings(fHeader);
     Lst.AddStrings(fDocument);
     
     Lst.SaveTofile(ExpandFileName(aFileName));
  finally
    Lst.Free;
  end;
end;

procedure TPostScriptPrinterCanvas.BeginDoc;
begin
  Inherited BeginDoc;
  
  //Clear all existing values
  //before starting an new document
  fDocument.Clear;
  fHeader.Clear;
  
  FirstUpdatefont:=True;
  Font.Size:=12;
  Font.Color:=clBlack;
  
  WriteHeader('%!PS-Adobe-3.0');
  WriteHeader('%%'+Format('BoundingBox: 0 0 %d %d',[PageWidth,PageHeight]));
  WriteHeader('%%'+Format('Creator: Lazarus PostScriptCanvas for %s',[Application.ExeName]));
  WriteHeader('%%'+Format('Title: %s',[Title]));
  WriteHeader('%%CreationDate: '+DateTimeToStr(Now));
  WriteHeader('%%Pages: (atend)');
  WriteHeader('%%PageResources: (atend)');
  WriteHeader('%%PageOrder: Ascend');
  WriteHeader('%%Page: 1 1');
  WriteHeader('');
  WriteHeader('%------------------------------------------------------------');
  WriteHeader('%================== BEGIN SETUP==============================');
  WriteHeader('');
  WriteHeader('% ISO Fonts');
  WriteHeader('/Helvetica findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/HelveticaISO exch definefont pop');
  WriteHeader('');
  WriteHeader('/Helvetica-Bold findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/HelveticaISO-Bold exch definefont pop');
  WriteHeader('');
  WriteHeader('/Helvetica-Oblique findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/HelveticaISO-Oblique exch definefont pop');
  WriteHeader('');
  WriteHeader('/Helvetica-BoldOblique findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/HelveticaISO-BoldOblique exch definefont pop');
  WriteHeader('');

  WriteHeader('/Courier findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/CourierISO exch definefont pop');
  WriteHeader('');
  WriteHeader('/Courier-Bold findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/CourierISO-Bold exch definefont pop');
  WriteHeader('');
  WriteHeader('/Courier-Oblique findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/CourierISO-Oblique exch definefont pop');
  WriteHeader('');
  WriteHeader('/Courier-BoldOblique findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/CourierISO-BoldOblique exch definefont pop');
  WriteHeader('');

  WriteHeader('/Times findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/TimesISO exch definefont pop');
  WriteHeader('');
  WriteHeader('/Times-Bold findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/TimesISO-Bold exch definefont pop');
  WriteHeader('');
  WriteHeader('/Times-Italic findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/TimesISO-Italic exch definefont pop');
  WriteHeader('');
  WriteHeader('/Times-BoldItalic findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/TimesISO-BoldItalic exch definefont pop');
  WriteHeader('');

  WriteHeader('/Times-Roman findfont');
  WriteHeader('  dup length dict begin');
  WriteHeader('  {1 index /FID ne {def} {pop pop} ifelse} forall');
  WriteHeader('  /Encoding ISOLatin1Encoding def');
  WriteHeader('  currentdict');
  WriteHeader('end');
  WriteHeader('/Times-RomanISO exch definefont pop');
  WriteHeader('');


  WriteHeader('/underline_on');
  WriteHeader('{%def');
  WriteHeader('    /underline true def');
  WriteHeader('    /underlineposition exch def');
  WriteHeader('    /underlinethickness exch def');
  WriteHeader('    /TEXT { TEXTwith } def');
  WriteHeader('} def');

  WriteHeader('/underline_off');
  WriteHeader('{ %def');
  WriteHeader('    /underline false def');
  WriteHeader('    /TEXT { TEXTwithout } def');
  WriteHeader('} def');

  WriteHeader('/TEXTwithout { moveto show } bind def');
  
  WriteHeader('/TEXTwith { %def');
  WriteHeader('    moveto');
  WriteHeader('    gsave');
  WriteHeader('       [] 0 setdash');
  WriteHeader('       0 underlineposition rmoveto');
  WriteHeader('       underlinethickness setlinewidth');
  WriteHeader('       dup stringwidth rlineto stroke');
  WriteHeader('    grestore');
  WriteHeader('    show');
  WriteHeader('} bind def');
  
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
  
  if Trim(fFileName)<>'' then
    SaveToFile(ExpandFileName(fFileName));
end;

procedure TPostScriptPrinterCanvas.NewPage;
begin
  inherited NewPage;

  Write('stroke');
  Write('showpage');
  Write('%%'+Format('Page: %d %d',[PageNumber, PageNumber]));
  
  write('newpath');
  
  // after showpage, font holds an invalid font dictionary
  // force selection of current font
  FirstUpdatefont:=True;
end;

//Move the current position
procedure TPostScriptPrinterCanvas.MoveTo(X1, Y1: Integer);
begin
  RequiredState([csHandleValid]);
  WriteComment(Format('MoveTo(%d,%d)',[x1,y1]));

  SetPosition(X1,Y1);
  TranslateCoord(X1,Y1);

  write(Format('%d %d moveto',[X1,Y1]));
end;

//Drawe line
procedure TPostScriptPrinterCanvas.LineTo(X1, Y1: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);

  WriteComment(Format('LineTo(%d,%d)',[x1,y1]));
  SetPosition(X1,Y1);
  TranslateCoord(X1,Y1);
  UpdateLineColor(clNone);
  UpdateLineWidth;
  UpdateLineStyle;
  write(Format('%d %d lineto stroke',[X1,Y1]));
  changed;
end;

procedure TPostScriptPrinterCanvas.Polyline(Points: PPoint; NumPts: Integer);
Var i  : LongInt;
    Lst: TStringList;
    Pt : TPoint;
begin
  if (NumPts<=1) or not Assigned(Points) then Exit;
  Changing;
  RequiredState([csHandleValid, csPenValid]);

  Lst:=TStringList.Create;
  try
    Pt:=Points[0];
    TranslateCoord(Pt.x,Pt.y);
    Write(Format('%d %d moveto',[Pt.x,Pt.y]),Lst);
    for i:=1 to NumPts-1 do
    begin
      Pt:=Points[i];
      TranslateCoord(Pt.x,Pt.y);
      SetPosition(Pt.x,Pt.y);
      TranslateCoord(Pt.x,Pt.y);
      Write(Format('%d %d lineto',[Pt.x,Pt.y]),Lst);
    end;

    if (Pen.Color<>clNone) and ((Pen.Color<>Brush.Color) or (Brush.Style<>bsSolid)) then
    begin
      UpdateLineColor(clNone);
      UpdateLineWidth;
      UpdateLineStyle;
      Write(Lst);
      write('stroke');
    end;
  finally
    Lst.Free;
  end;

  MoveToLastPos;
  Changed;
end;

procedure TPostScriptPrinterCanvas.PolyBezier(Points: PPoint; NumPts: Integer;
  Filled: boolean; Continuous: boolean);
Var i  : Integer;
    St : String;
    Pt : TPoint;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  
  if (NumPts>=4) then
  begin
    ClearBuffer;

    St:='';
    Pt:=Points[0];
    TranslateCoord(Pt.x,Pt.y);
    if Continuous then
      WriteB('newpath');
    WriteB(Format('%d %d moveto',[Pt.x,Pt.y]));
    for i:=1 to NumPts-1 do
    begin
      Pt:=Points[i];
      TranslateCoord(Pt.x,Pt.y);
      St:=St+Format(' %d %d',[Pt.x,Pt.y]);
    end;
    WriteB(Format('%s curveto',[St]));

    if Continuous then
      writeB('closepath');
    SetBrushFillPattern(True,Filled);

    MoveToLastPos;
  end;
  Changed;
end;

//Draw an Rectangle
procedure TPostScriptPrinterCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  writecomment(Format('Rectangle(%d,%d,%d,%d)',[x1,y1,x2,y2]));
  TranslateCoord(X1,Y1);
  TranslateCoord(X2,Y2);

  ClearBuffer;
  //Tempo draw rect
  WriteB('newpath');
  writeB(Format('    %d %d moveto',[X1,Y1]));
  writeB(Format('    %d %d lineto',[X2,Y1]));
  writeB(Format('    %d %d lineto',[X2,Y2]));
  writeB(Format('    %d %d lineto',[X1,Y2]));
  writeB('closepath');

  SetBrushFillPattern(True,True);

  MoveToLastPos;
  
  Changed;
end;

procedure TPostScriptPrinterCanvas.Frame(const ARect: TRect);
Var X1,Y1,X2,Y2 : Integer;
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);

  X1:=aRect.Left;
  Y1:=aRect.Top;
  X2:=aRect.Right;
  Y2:=aRect.Bottom;
  
  TranslateCoord(X1,Y1);
  TranslateCoord(X2,Y2);

  ClearBuffer;
  //Tempo draw rect
  WriteB('newpath');
  writeB(Format('    %d %d moveto',[X1,Y1]));
  writeB(Format('    %d %d lineto',[X2,Y1]));
  writeB(Format('    %d %d lineto',[X2,Y2]));
  writeB(Format('    %d %d lineto',[X1,Y2]));
  writeB('closepath');

  SetBrushFillPattern(True,False);

  MoveToLastPos;

  Changed;
end;

procedure TPostScriptPrinterCanvas.FrameRect(const ARect: TRect);
Var CL : TColor;
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
Var X1,Y1,X2,Y2 : Integer;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);

  X1:=ARect.Left;
  Y1:=ARect.Top;
  X2:=ARect.Right;
  Y2:=ARect.Bottom;

  Writecomment(Format('FillRect(%d,%d,%d,%d)',[x1,y1,x2,y2]));
  TranslateCoord(X1,Y1);
  TranslateCoord(X2,Y2);
  
  ClearBuffer;

  WriteB('newpath');
  WriteB(Format('    %d %d moveto',[X1,Y1]));
  WriteB(Format('    %d %d lineto',[X2,Y1]));
  WriteB(Format('    %d %d lineto',[X2,Y2]));
  WriteB(Format('    %d %d lineto',[X1,Y2]));
  WriteB('closepath');

  SetBrushFillPattern(False,True);

  MoveToLastPos;
  
  Changed;
end;

procedure TPostScriptPrinterCanvas.RoundRect(X1, Y1, X2, Y2: Integer; RX,
  RY: Integer);
Var ellipsePath : string;
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  X1:=Min(X1,X2);
  X2:=Max(X1,X2);
  Y1:=Min(Y1,Y2);
  Y2:=Max(Y1,Y2);

  writecomment(Format('RoundRect(%d,%d,%d,%d,%d,%d)',[x1,y1,x2,y2,Rx,Ry]));
  TranslateCoord(X1,Y1);
  TranslateCoord(X2,Y2);

  ClearBuffer;
  
  {Note: arcto command draws a line from current point to beginning of arc
  save current matrix, translate to center of ellipse, scale by rx ry, and draw
  a circle of unit radius in counterclockwise dir, return to original matrix
  arguments are (cx, cy, rx, ry, startAngle, endAngle)}
  ellipsePath:='matrix currentmatrix %d %d translate %d %d scale 0 0 1 %d %d arc setmatrix';

  {choice between newpath and moveto beginning of arc
   go with newpath for precision, does this violate any assumptions in code???
   write(format('%d %d moveto',[x1+rx, y1]),Lst  # this also works}
  WriteB('newpath');
  WriteB(Format(ellipsePath,[x1+rx,y1-ry,rx,ry,90,180]));
  WriteB(Format(ellipsePath,[x1+rx,y2+ry,rx,ry,180,270]));
  WriteB(Format(ellipsePath,[x2-rx,y2+ry,rx,ry,270,360]));
  WriteB(Format(ellipsePath,[x2-rx,y1-ry,rx,ry,0,90]));
  WriteB('closepath');
    
  SetBrushFillPattern(True,True);
  
  MoveToLastPos;
  Changed;
end;

procedure TPostScriptPrinterCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean);
Var i  : LongInt;
    Pt : TPoint;
begin
  if (NumPts<=1) or not Assigned(Points) then Exit;
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  
  ClearBuffer;
  
  Pt:=Points[0];
  TranslateCoord(Pt.x,Pt.y);
  WriteB('newpath');
  WriteB(Format('%d %d moveto',[Pt.x,Pt.y]));
  for i:=1 to NumPts-1 do
  begin
    Pt:=Points[i];
    TranslateCoord(Pt.x,Pt.y);
    WriteB(Format('%d %d lineto',[Pt.x,Pt.y]));
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
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  writecomment(Format('Ellipse(%d,%d,%d,%d)',[x1,y1,x2,y2]));
  TranslateCoord(X1,Y1);
  TranslateCoord(X2,Y2);

  //Init
  StAng:=0;
  Ang:=360;

  //calculate centre of ellipse
  cx:=(x1+x2)/2;
  cy:=(y1+y2)/2;
  rx:=(x2-x1)/2;
  ry:=(y2-y1)/2;
  
  //calculate semi-minor and semi-major axes of ellipse
  xScale:=Abs((x2-x1)/2.0);
  yScale:=Abs((y2-y1)/2.0);

  Code:=PPCFormat('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %d %d %s setmatrix',
      [cX,cY,xScale,yScale,StAng,Ang,'arc']);
      
  ClearBuffer;
  WriteB(PPCFormat('%.3f %.3f moveto',[cX,cY])); //move to center of circle
  WriteB(Code);
  SetBrushFillPattern(False,True);

  //move current point to start of arc, note negative
  //angle because y increases down
  ClearBuffer;
  WriteB(PPCFormat('%.3f %.3f moveto',[cX+(rX*Cos(StAng*-1)),cY+(rY*Sin(StAng*-1))]));
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
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

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

  Code:=PPCFormat('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %.3f %.3f %s setmatrix',
      [cX,cY,xScale,yScale,Angle1/16,Angle2/16,ang]);


  if (Pen.Color<>clNone) and ((Pen.Color<>Brush.Color) or (Brush.Style<>bsSolid)) then
  begin
    UpdateLineColor(clNone);
    UpdateLineWidth;
    UpdateLineStyle;

    //move current point to start of arc, note negative
    //angle because y increases down
    write(PPCFormat('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))]));
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

  Code:=PPCFormat('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %.3f %.3f %s setmatrix',
      [cX,cY,xScale,yScale,Angle1/16,Angle2/16,ang]);

  //move current point to start of arc, note negative
  //angle because y increases down
  ClearBuffer;
  writeB(PPCFormat('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))]));
  WriteB(Code);
  writeB(Format('%d %d lineto',[Left,Top]));
  writeB(PPCFormat('%.3f %.3f lineto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))]));
  SetBrushFillPattern(False,True);

  //move current point to start of arc, note negative
  //angle because y increases down
  ClearBuffer;
  writeB(PPCFormat('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))]));
  WriteB(Code);
  writeB(Format('%d %d lineto',[Left,Top]));
  writeB(PPCFormat('%.3f %.3f lineto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))]));
  SetBrushFillPattern(True,False);

  MoveToLastPos;
  Changed;
end;

//Out the text at the X,Y coord. Set the font
procedure TPostScriptPrinterCanvas.TextOut(X, Y: Integer; const Text: String);
Var PenUnder : Real;
    PosUnder : Integer;
begin
  TranslateCoord(X,Y);
  UpdateFont;

  //The Y origine for ps text it's Left bottom corner
  Dec(Y,Abs(Font.Size));
  
  if fsUnderline in Font.Style then
  begin
    PenUnder:=0.5;
    if fsBold in Font.Style then
      PenUnder:=1.0;
    PosUnder:=(Abs(Round(Font.Size/3))*-1)+2;

    write(PPCFormat('%.3f %d underline_on',[PenUnder,PosUnder]));
    write(Format('(%s) %d %d TEXT',[MapedString(Text),X,Y]));
    write('underline_off');
  end
  else
  begin
    write(Format('%d %d moveto',[X,Y]));
    write(Format('(%s) show',[MapedString(Text)]));
  end;

  MoveToLastPos;
end;

function TPostScriptPrinterCanvas.TextExtent(const Text: string): TSize;
Var IndexFont,i : Integer;
    FontName    : string;
    c: Char;
begin
  Result.cX := 0;
  Result.cY := 0;
  if Text='' then Exit;
  RequiredState([csHandleValid, csFontValid]);
  Result.cY:=Font.Size;

  FontName:=MappedFontName;
  IndexFont:=0; //By default, use CourierISO metrics
  for i:=0 to High(cFontPSMetrics) do
  begin
    if cFontPSMetrics[i].Name=FontName then
    begin
      IndexFont:=i;
      Break;
    end;
  end;

  for i:=1 to Length(Text) do
  begin
    c:=Text[i];
    if (c in [#32..#255]) then
      Inc(Result.cX,cFontPSMetrics[IndexFont].Widths[Ord(c)]);
  end;
  Result.cX:=Round(Result.cX*Font.Size*0.001);
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
    DrawWidth : Integer;
    DrawHeight: Integer;
    ImgWidth  : Integer;
    ImgHeight : Integer;

begin
  if not Assigned(SrcGraphic) then exit;
  Changing;
  RequiredState([csHandleValid]);

  X1:=DestRect.Left;
  Y1:=DestRect.Top;
  X2:=DestRect.Right;
  Y2:=DestRect.Bottom;
  
  TranslateCoord(X1,Y1);
  TransLateCoord(X2,Y2);
  
  ImgWidth:=SrcGraphic.Width;
  ImgHeight:=SrcGraphic.Height;

  //if not FPImage then draw ab Rectangle because other wise PostScript
  //interpreter wait infinite some RGB datas
  DrawWidth:=X2-X1;
  DrawHeight:=Y1-Y2;
  ClearBuffer;

  WriteB('gsave');
  writeB(Format('%d %d translate',[X1,Y1-DrawHeight]));
  WriteB(Format('%d %d scale',[DrawWidth,DrawHeight]));
  WriteB(Format('/scanline %d 3 mul string def',[ImgWidth]));
  WriteB(Format('%d %d %d',[ImgWidth,ImgHeight,8]));
  WriteB(Format('[%d %d %d %d %d %d]',[ImgWidth,0,0,-ImgHeight,0,ImgHeight]));
  WriteB('{ currentfile scanline readhexstring pop } false 3');
  WriteB('colorimage');

  GetRGBImage(SrcGraphic,fBuffer);
  WriteB('% end of image data');
  WriteB('grestore');
  
  Write(fBuffer);

  Changed;
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
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);

  writecomment(Format('Chord(%d,%d,%d,%d,%d,%d)',[x1,y1,x2-x1,y2-y1,Angle1,Angle2]));
  TranslateCoord(x1, y1);

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

  Code:=PPCFormat('matrix currentmatrix %.3f %.3f translate %.3f %.3f scale 0 0 1 %.3f %.3f %s setmatrix',
      [cX,cY,xScale,yScale,Angle1/16,Angle2/16,ang]);

  //move current point to start of arc, note negative
  //angle because y increases down.ClosePath for draw chord
  ClearBuffer;
  writeB('newpath');
  writeB(PPCFormat('%.3f %.3f moveto',[cX+(rX*Cos((Angle1/16)*-1)),cY+(rY*Sin((Angle1/16)*-1))]));
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

procedure TPostScriptPrinterCanvas.TextRect(ARect: TRect; X, Y: integer;
  const Text: string; const Style: TTextStyle);
begin
  {$IFDEF VerboseLCLTodos}{$WARNING TPostScriptPrinterCanvas.TextRect is not yet fully implemented!}{$ENDIF}
  //TODO: clipping, layout, etc.
  TextOut(X,Y, Text);
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
