{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    FPReport generic LCL Export filter. Can also be used in design mode.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportlclexport;

{$mode objfpc}{$H+}
{ $DEFINE DEBUGRD}

interface

uses
  Classes,
  SysUtils,
  fpImage,
  fpReport,
  contnrs,
  types,
  graphics,
  forms;

Const
  // Design mode constants
  BandTitleMargin = 2;             // Margin used in band title displaying name of band, in pixels.
  BandTitleOffset = 12;            // Space between band title and previous band, in pixels
  ElementHCornerLength = 4;        // Length of horitonzal corner handle
  ElementVCornerLength = 6;        // Length of horitonzal corner handle
  clSelectionRect = clDkGray;    // Color for selected elements rectangle
  clEmementCorner = clBlack;       // Color for element corner indicators.
  ReSizeHandleHalfWidth = 3;       // Half width of selection resize handle
  ReSizeHandleWidth = 2 * ReSizeHandleHalfWidth; // Full width of selection resize handle;

{  btUnknown,btPageHeader,btReportTitle,btColumnHeader,btDataHeader,btGroupHeader,btDataband,btGroupFooter,
                               btDataFooter,btColumnFooter,btReportSummary,btPageFooter,btChild);}

type
  {
    The LCL renderer does not support hyperlinks by itself.
    It collects a series of URLs and Rects when rendering.
    The user of the LCL renderer can use this collection to implement
    onClick and MouseMove handlers that query the List using the IndexOfPoint or FindLinkAtPoint methods.
    If links overlap, the last added link will be used.
  }

  { THyperLinkItem }

  THyperLinkItem = Class(TCollectionItem)
  private
    FRect: TRect;
    FURL: String;
  Public
    Property Rect : TRect Read FRect Write FRect;
    Property URL : String Read FURL Write FURL;
  end;

  { THyperLinkList }

  THyperLinkList = Class(TCollection)
  private
    function GetL(AIndex : Integer): THyperLinkItem;
  Public
    Function IndexOfPoint(APoint : TPoint) : Integer;
    Function IndexOfPoint(AX,AY : Integer) : Integer;
    Function FindLinkAtPoint(APoint : TPoint) : THyperlinkItem;
    Function FindLinkAtPoint(AX,AY : Integer) : THyperlinkItem;
    Function AddLink(Const ARect : TRect; Const AURL : String) : THyperlinkItem;
    Property Links[AIndex : Integer] : THyperLinkItem Read GetL; default;
  end;

  { TFPReportExportCanvas }
  TReportDrawMode = (dmRender,dmDesign);

  TFPReportExportCanvas = class(TFPReportExporter)
  private
    FCanvas : TCanvas;
    FDrawMode: TReportDrawMode;
    FHDPI: integer;
    FHorzOffset: Integer;
    FImageWidth: integer;
    FImageHeight: integer;
    FFonts : TFPObjectHashTable;
    FPageIndex : Integer;
    FPages : TFPList;
    FShowBandTypeNames: Boolean;
    FVDPI: integer;
    FVertOffset: Integer;
    FZoom: Double;
    FHyperLinks : THyperLinkList;
    FBandHandleHeight : Integer;
    function GetCurrentPage: TFPReportPage;
    function GetHyperLinksEnabled: Boolean;
    function GetLayout(AElement: TFPReportElement): TFPReportLayout;
    function GetPageCount: Integer;
    procedure PrepareCanvas;
    procedure SetHyperlinksEnabled(AValue: Boolean);
    procedure SetPageIndex(AValue: Integer);
  protected
    procedure RenderFrame(const AFrame: TFPReportFrame; const ARect: Trect;  const ABackgroundColor: TColor);
    Procedure RenderImage(aRect : TFPReportRect; var AImage: TFPCustomImage) ; override;
    function BandColorCode(ABand: TFPReportCustomBandClass; Edge : Boolean): TColor; virtual;
    procedure DrawBandLabel(Aband: TFPReportCustomBand; l: TFPReportLayout); virtual;
    procedure DrawBandRect(Aband: TFPReportCustomBand; l: TFPReportLayout); virtual;
    Function CreateHyperlinks : THyperLinkList; virtual;
    procedure DrawElementCorners(E: TFPReportElement; R: TRect); virtual;
    function GetFont(const AFontName: String): TFont;
    procedure SetupPageRender(const APage: TFPReportPage);
    procedure DoExecute(const ARTObjects: TFPList); override;
    procedure RenderBand(Aband: TFPReportCustomBand); virtual;
    procedure RenderFrame(const ABand: TFPReportCustomBand; const AFrame: TFPReportFrame; const APos: TFPReportPoint; const AWidth, AHeight: TFPReportUnits); virtual;
    procedure RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo); virtual;
    procedure RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape); virtual;
    procedure RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage); virtual;
    procedure RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox); virtual;
    Function GetBandHandleHeight : Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    // Rendering options
    procedure   RenderElement(ABand: TFPReportCustomBand;  Element: TFPReportElement); virtual;
    procedure   RenderCurrentPage; virtual;
    procedure   DrawSelectionHandle(ACenter: TPoint; AColor: TColor); virtual;
    Procedure   DrawSelectionRect(ARect : Trect); virtual;
    // Moved here to be usable externally
    procedure RenderShapeCircle(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure RenderShapeEllipse(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure RenderShapeLine(lpt1: TFPReportPoint;  const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
    procedure RenderShapeRect(const lpt1: TFPReportPoint; const ALayout: TFPReportLayout);
    procedure RenderShapeTriangle(Alpt: TFPReportPoint; const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);
    // Some size/position routines
    Procedure   GetCurrentPageRenderSize(Out AWidth,AHeight : Integer);
    Procedure   GetPageRenderSize(APage : TFPReportCustomPage; Out AWidth,AHeight : Integer);
    function    CoordToPoint(const APos: TFPReportPoint;  const AWidth: TFPReportUnits=0; const AHeight: TFPReportUnits=0): TPoint;
    function    CoordToRect(const APos: TFPReportPoint; const AWidth: TFPReportUnits=0; const AHeight: TFPReportUnits=0): TRect;
    function    HmmToPixels(const AValue: TFPReportUnits): Integer;
    function    VmmToPixels(const AValue: TFPReportUnits): Integer;
    Function    GetPageRect(APage : TFPReportCustomPage; WithoutMargin : Boolean = False)  : TRect;
    Function    GetBandRect(L : TFPReportLayout;IncludeHandle: Boolean) : TRect;
    Function    GetBandRect(ABand : TFPReportCustomBand; IncludeHandle : Boolean) : TRect;
    Function    GetElementRect(BandLayout,ElementLayout : TFPReportLayout) : TRect;
    Function    GetElementRect(ABand : TFPReportCustomBand; AElement : TFPReportElement) : TRect;
    Class function RGBtoBGR(const AColor: UInt32): TColor;
    Class function BGRToRGB(const AColor: TColor): TFPReportColor;
    // Properties
    property    HDPI: integer read FHDPI write FHDPI;
    property    VDPI: integer read FVDPI write FVDPI;
    property    Zoom : Double read FZoom write FZoom;
    Property    Canvas : TCanvas Read FCanvas Write FCanvas;
    Property    PageIndex : Integer Read FPageIndex Write SetPageIndex;
    Property    PageCount : Integer Read GetPageCount;
    Property    CurrentPage : TFPReportPage Read GetCurrentPage;
    Property    HorzOffset : Integer Read FHorzOffset Write FHorzOffset;
    Property    VertOffset : Integer Read FVertOffset Write FVertOffset;
    // collect links ?
    Property    HyperLinksEnabled : Boolean Read GetHyperLinksEnabled Write SetHyperlinksEnabled;
    // List of collected hyperlinks. Only valid when HyperLinksEnabled = True.
    Property    HyperLinks : THyperLinkList Read FHyperLinks;
    // Design mode or not
    Property    DrawMode : TReportDrawMode Read FDrawMode Write FDrawMode;
    // ShowBandTypeNames
    Property ShowBandTypeNames : Boolean Read FShowBandTypeNames Write FShowBandTypeNames;
  end;

const
  cInchToMM = 25.4;
  RGBA_Width = 4;

implementation

uses
  fpwritepng,
  math;

Resourcestring
  SErrPageOutOfRange = 'Page index %d out of allowed range [0..%d]';

type

  { for access to Protected methods }
  TReportImageFriend = class(TFPReportCustomImage);
  TReportCheckboxFriend = class(TFPReportCustomCheckbox);

{ TFPImageFriend }


function GetColorComponent(Var AColor: UInt32): Word;
begin
  Result:=AColor and $FF;
  Result:=Result or (Result shl 8);
  AColor:=AColor shr 8;
end;

Class function TFPReportExportCanvas.RGBtoBGR(const AColor: UInt32): TColor;
var
  C : UInt32;
  R,G,B : Byte;
begin
  C:=AColor;
  B:= GetColorComponent(C);
  G:= GetColorComponent(C);
  R:= GetColorComponent(C);
//    Alpha := GetColorComponent(C);
  Result:=RGBToColor(R,G,B);
end;

class function TFPReportExportCanvas.BGRToRGB(const AColor: TColor): TFPReportColor;

var
  R,G,B : Byte;

begin
  RedGreenBlue(ColorToRGB(AColor),R,G,B);
  Result:=RGBToReportColor(R,G,B);
end;

{ THyperLinkList }

function THyperLinkList.GetL(AIndex : Integer): THyperLinkItem;
begin
  Result:=Items[AIndex] as THyperLinkItem;
end;

function THyperLinkList.IndexOfPoint(APoint: TPoint): Integer;
begin
  Result:=Count-1;
  While (Result>=0) and (Not ptInRect(GetL(Result).Rect,APoint)) do
    Dec(Result);
end;

function THyperLinkList.IndexOfPoint(AX, AY: Integer): Integer;
begin
  Result:=IndexOfPoint(Point(AX,AY));
end;

function THyperLinkList.FindLinkAtPoint(APoint: TPoint): THyperlinkItem;

Var
  I : Integer;

begin
  I:=IndexOfPoint(APoint);
  If I=-1 then
    Result:=Nil
  else
    Result:=GetL(I);
end;

function THyperLinkList.FindLinkAtPoint(AX, AY: Integer): THyperlinkItem;
begin
  Result:=FindLinkAtPoint(Point(AX,AY));
end;

function THyperLinkList.AddLink(const ARect: TRect; const AURL: String): THyperlinkItem;
begin
  Result:=Add as THyperLinkItem;
  Result.FRect:=ARect;
  Result.FURL:=AURL;
end;


{ TFPReportExportCanvas }


function TFPReportExportCanvas.HmmToPixels(const AValue: TFPReportUnits): Integer;
begin
  Result := Round(AValue * (HDPI * Zoom/ cInchToMM));
end;

function TFPReportExportCanvas.VmmToPixels(const AValue: TFPReportUnits): Integer;
begin
  Result := Round(AValue * (VDPI * Zoom/ cInchToMM));
end;

procedure TFPReportExportCanvas.SetupPageRender(const APage: TFPReportPage);

begin
  if APage.Orientation = poPortrait then 
    begin
    FImageWidth := HmmToPixels(APage.PageSize.Width);
    FImageHeight := VmmToPixels(APage.PageSize.Height);
    end  
  else 
    begin
    FImageWidth := HmmToPixels(APage.PageSize.Height);
    FImageHeight := VmmToPixels(APage.PageSize.Width);
    end;
  PrepareCanvas;
end;


procedure TFPReportExportCanvas.PrepareCanvas;

begin
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Color:=clBlack;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clWhite;
  Canvas.FillRect(FHorzOffset,FVertOffset,FHorzOffset+FImageWidth-1,FVertOffset+FImageHeight-1);
  if Assigned(FHyperLinks) then
    FHyperLinks.Clear;
end;

function TFPReportExportCanvas.GetCurrentPage: TFPReportPage;
begin
  if Assigned(FPages) and (PageIndex<FPages.Count) then
    Result:=TFPReportPage(FPages[PageIndex])
  else
    Result:=Nil;
end;

function TFPReportExportCanvas.GetHyperLinksEnabled: Boolean;
begin
  Result:=Assigned(FHyperlinks);
end;

function TFPReportExportCanvas.GetPageCount: Integer;
begin
  if Assigned(FPages) then
    Result:=FPages.Count
  else
    Result:=0;
end;

function TFPReportExportCanvas.CoordToPoint(const APos: TFPReportPoint;
  const AWidth: TFPReportUnits; const AHeight: TFPReportUnits): TPoint;

begin
  Result.X:=HmmToPixels(APos.Left+AWidth)+FHorzOffset;
  Result.Y:=VmmToPixels(APos.Top+AHeight)+FVertOffset;
end;

function TFPReportExportCanvas.CoordToRect(const APos: TFPReportPoint;
  const AWidth: TFPReportUnits; const AHeight: TFPReportUnits): TRect;

begin
  Result.Left:=HmmToPixels(APos.Left)+FHorzOffset;
  Result.Top:=VmmToPixels(APos.Top)+FVertOffset;
  Result.Right:=HmmToPixels(APos.Left+AWidth)+FHorzOffset;
  Result.Bottom:=VmmToPixels(APos.Top+AHeight)+FVertOffset;
end;

function TFPReportExportCanvas.GetPageRect(APage: TFPReportCustomPage;
  WithoutMargin: Boolean): TRect;

Var
  W,H : Integer;

begin
  GetPageRenderSize(APage,W,H);
  Result:=Rect(FHorzOffset,FVertOffset,FHorzOffset+W,FVertOffset+H);
  if WithoutMargin then
    begin
    Result.Left:=Result.Left-hMMToPixels(APage.Margins.Left);
    Result.Top:=Result.Top-VMMToPixels(APage.Margins.Top);
    Result.Right:=Result.Right+hMMToPixels(APage.Margins.Right);
    Result.Bottom:=Result.Bottom+VMMToPixels(APage.Margins.Bottom);
    end;
end;

procedure TFPReportExportCanvas.RenderFrame(const AFrame: TFPReportFrame;
  const ARect: Trect; const ABackgroundColor: TColor);

var
  bStroke, bFill: boolean;
  C : TFPReportColor;

begin
  bStroke := AFrame.Color <> clNone;
  bFill := AFrame.BackgroundColor <> clNone;
  if not (bStroke or bFill) then
    exit;
  if AFrame.Color = fpReport.clNone then
    Canvas.Pen.Style := psClear
  else
  begin
    Canvas.Pen.Style:=AFrame.Pen;
    Canvas.Pen.Color:= RGBtoBGR(AFrame.Color);
    Canvas.Pen.Width:=AFrame.Width;
  end;
  {$IFDEF DEBUGRD}
  Writeln('Rendering frame [',AFrame.Shape,'] (',ARect.Left,',',ARect.Top,',',ARect.right,',',ARect.Bottom,') : ',(bStroke or bFill));
  {$ENDIF}
  if (AFrame.Shape=fsRectangle) and (bStroke or bFill) then
    begin
    if bFill then
      begin
      Canvas.Brush.Style:=bsSolid;
      C:=AFrame.BackgroundColor;
      if c=fpReport.clNone then
        C:=ABackgroundColor;
      if c=fpReport.clNone then
        C:=fpReport.clWhite;
      Canvas.Brush.Color := RGBtoBGR(C);
      FCanvas.FillRect(ARect);
      end;
    if bStroke then
      begin
      Canvas.Brush.Style:=bsClear;
      FCanvas.Rectangle(ARect);
      end;
    end;
  if (AFrame.Shape=fsNone) and bStroke then
    begin
    if (flTop in AFrame.Lines) then
      FCanvas.line(ARect.Left, ARect.Top,ARect.Right,ARect.Top);
    if (flbottom in AFrame.Lines) then
      FCanvas.line(ARect.Left, ARect.Bottom,ARect.Right,ARect.Bottom);
    if (flLeft in AFrame.Lines) then
      FCanvas.line(ARect.Left, ARect.Top,ARect.Left,ARect.Bottom);
    if (flRight in AFrame.Lines) then
      FCanvas.line(ARect.Right, ARect.Top,ARect.Right,ARect.Bottom);
    end;  { Frame.Shape = fsNone }
end;

procedure TFPReportExportCanvas.RenderImage(aRect: TFPReportRect; var AImage: TFPCustomImage);
Var
  lpt : TFPReportPoint;
  pt : TPoint;
  G : TBitmap;

begin
  lPt.Left := aRect.Left;
  lPt.Top := aRect.Top;
  PT:=CoordToPoint(Lpt,0,0);
//  Canvas.StretchDraw(pt.X,pT.Y,mmToPixels(arect.Width), mmToPixels(arect.Height),AImage);
  G:=CreateBitmapFromFPImage(aImage);
  try
    Canvas.Draw(pt.X,pT.Y,G);
  Finally
    G.Free;
  end;
end;

procedure TFPReportExportCanvas.RenderFrame(const ABand: TFPReportCustomBand; const AFrame: TFPReportFrame;
  const APos: TFPReportPoint; const AWidth, AHeight: TFPReportUnits);

begin
  RenderFrame(AFrame,CoordToRect(APos,AWidth,AHeight), ABand.Frame.BackgroundColor);
end;

Type
  THackReportMemo = class(TFPReportCustomMemo)
  published
    property  Font;
  end;

function TFPReportExportCanvas.GetFont(const AFontName: String): TFont;

Var
  ftFont : TFont;

begin
  Result:=Nil;
  Result:=TFont(FFonts.Items[AFontName]);
  If (Result=Nil) then
    begin
    ftFont:=TFont.create;
    ftFont.Name:=AFontName;
    Result:=ftFont;
    FFonts.Add(AFontName,Result);
    end;
end;

procedure TFPReportExportCanvas.RenderMemo(const ABand: TFPReportCustomBand; const AMemo: TFPReportCustomMemo);
var
  lPt1: TFPReportPoint;  // original Report point
  lMemo: THackReportMemo;
  i: integer;
  lXPos: TFPReportUnits;
  lYPos: TFPReportUnits;
  txtblk: TFPTextBlock;
  R,MR : TRect;
  BL,ML : TFPReportLayout;
begin
  lMemo := THackReportMemo(AMemo);

  { Store the Top-Left coordinate of the Memo. We will be reusing this info. }
  BL:=GetLayout(Aband);
  ML:=GetLayout(AMemo);
  lPt1.Left := BL.Left + ML.Left;
  lPt1.Top := BL.Top + ML.Top ;
  MR:=CoordToRect(LPT1,ML.Width,ML.Height);

  { Frame must be drawn before the text as it could have a fill color. }
  RenderFrame(AMemo.Frame, MR, ABand.Frame.BackgroundColor);
  if DrawMode=dmDesign then
    begin
    DrawElementCorners(AMemo,MR);
    lMemo.RecalcLayout;
    end;
  { render the TextBlocks as-is. }
  for i := 0 to lMemo.TextBlockList.Count-1 do
  begin
    txtblk := lMemo.TextBlockList[i];
    Canvas.Font := GetFont(txtblk.FontName);
    Canvas.Font.Size:=Round(lMemo.Font.Size * Zoom);

    lXPos := lPt1.Left + txtblk.Pos.Left;
    lYPos := lPt1.Top + txtblk.Pos.Top;
    R:=Rect(HmmToPixels(lXPos) + FHorzOffset,
            VmmToPixels(lYPos) + FVertOffset,
            HmmToPixels(lXPos + txtblk.Width) + FHorzOffset,
            VmmToPixels(lYPos + txtblk.Height + (txtblk.Descender*3)) + FVertOffset);
    if txtblk.BGColor <> fpReport.clNone then // DON'T remove "fpReport." prefix.
    begin
      Canvas.Pen.Style := psClear;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := RGBtoBGR(txtblk.BGColor);
      Canvas.Rectangle(R);
{      Canvas.Rectangle(
          mmToPixels(lXPos) + FHorzOffset,
          mmToPixels(lYPos) + FVertOffset,
          mmToPixels(lXPos + txtblk.Width) + FHorzOffset,
          mmToPixels(lYPos + txtblk.Height + (txtblk.Descender*3)) + FVertOffset
      );}
    end;

    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := RGBtoBGR(txtblk.FGColor);
    { LCL's Canvas.TextOut origin coordinate is Top-Left }
    if DrawMode=dmRender then
      begin
      Canvas.TextOut(
          HmmToPixels(lXPos) + FHorzOffset,
          VmmToPixels(lYPos) + FVertOffset,
          txtblk.Text
      );
      if Assigned(FHyperLinks) and (txtblk is TFPHTTPTextBlock) then
        FHyperLinks.AddLink(R,(txtblk as TFPHTTPTextBlock).URL);
      end
    else
      begin
      Canvas.TextRect(
          MR,
          HmmToPixels(lXPos) + FHorzOffset,
          VmmToPixels(lYPos) + FVertOffset,
          txtblk.Text
      )
      end;

  end;
end;

procedure TFPReportExportCanvas.RenderShapeCircle(const lpt1: TFPReportPoint;
  const ALayout: TFPReportLayout);

var
  lPt2: TFPReportPoint;  // original Report point
  R : TRect;
  LW : TFPReportUnits;

begin
  // Keep center of circle at center of rectangle
  lw := Min(ALayout.Width, ALayout.Height);
  lpt2.Left:=lPt1.Left+(ALayout.Width / 2)-lW/2;
  lpt2.Top:=lPt1.top+(ALayout.Height / 2)-lW/2;
  R:=CoordToRect(lpt2,LW,LW);
  Canvas.ellipse(R);
end;

procedure TFPReportExportCanvas.RenderShapeEllipse(const lpt1: TFPReportPoint;
  const ALayout: TFPReportLayout);

Var
  R : TRect;
begin
  R:=CoordToRect(lpt1,ALayout.Width,ALayout.Height);
  Canvas.ellipse(R);
end;

procedure TFPReportExportCanvas.RenderShapeLine(lpt1: TFPReportPoint;
  const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);

var
  lPt2: TFPReportPoint;  // original Report point
  R1,R2 : TPoint;

begin
  case AOrientation of
  orNorth, orSouth:
     begin                                         //   |
     lPt1.Left := lPt1.Left + (ALayout.Width / 2); //   |
     lPt2.Left := lPt1.Left ;                      //   |
     lPt2.Top := LPT1.Top + ALayout.Height;        //   |
     end;
  orNorthEast, orSouthWest:
     begin                                         //    /
     lPt2.Left := lPt1.Left;                       //   /
     lPt1.Left := lPt1.Left + ALayout.Width;       //  /
     lPt2.Top := lPt1.Top + ALayout.Height;        // /
     end;
  orEast, orWest:
     begin                                         //
     lPt2.Left := lPt1.Left + ALayout.Width;       // ----
     lPt1.Top := lPt1.Top + (ALayout.Height / 2);  //
     lPt2.Top := lPt1.Top;                         //
     end;
  orSouthEast, orNorthWest:
     begin                                         // \
     lPt2.Left := lPt1.Left + ALayout.Width;       //  \
     lPt2.Top := lPt1.Top + ALayout.Height;        //   \
     end;                                          //    \
  end;
  R1:=CoordToPoint(lpt1);
  R2:=CoordToPoint(lpt2);
  Canvas.line(R1,R2);
end;

procedure TFPReportExportCanvas.RenderShapeRect(const lpt1: TFPReportPoint;
  const ALayout: TFPReportLayout);

Var
  ldx, ldy, lw: TFPReportUnits;
  P : TFPReportPoint;
begin
  lw := Min(ALayout.Width, ALayout.Height);
  if ALayout.Width = ALayout.Height then
  begin
    ldx := 0;
    ldy := 0;
  end
  else if ALayout.Width > ALayout.Height then
  begin
    ldx := (ALayout.Width - ALayout.Height) / 2;
    ldy := 0;
  end
  else if ALayout.Width < ALayout.Height then
  begin
    ldx := 0;
    ldy := (ALayout.Height - ALayout.Width) / 2;
  end;
  P.Left := lPt1.Left + ldx;
  { PDF origin coordinate is Bottom-Left, and Report Layout is Top-Left }
  P.Top := lPt1.Top + ldy;
  Canvas.rectangle(CoordToRect(P,lw,Lw));
end;

procedure TFPReportExportCanvas.RenderShapeTriangle(Alpt: TFPReportPoint;
  const AOrientation: TFPReportOrientation; const ALayout: TFPReportLayout);


  Procedure DrawLine(Const A,B : TFPReportPoint);

  begin
    Canvas.Line(CoordToPoint(A),CoordToPoint(B));
  end;

var
  lpt1,lPt2,lpt3: TFPReportPoint;  // original Report points for 3 corners of triangle.

begin
  case AOrientation of
  orNorth:
    begin
    lPt1.Left := ALPT.Left + (ALayout.Width / 2); //      1
    lPt1.Top  := ALPT.Top;                        //      /\
    lPt2.Left := ALPT.Left;                       //     /  \
    lPt2.Top  := ALPT.Top + ALayout.Height;       //    /____\
    lPt3.Left := ALPT.Left + ALayout.Width;       //  2       3
    lPt3.Top  := lPt2.Top;
    end;
  orNorthEast:
    begin
    lPt1.Left := ALPT.Left + (ALayout.Width );    //   +-------1
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left;                       //   2       |
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left + ALayout.Width/2;     //   +---3---+
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  orSouth:
    begin
    lPt1.Left := ALPT.Left;                        //  1 ------ 2
    lPt1.Top  := ALPT.Top;                         //    \    /
    lPt2.Left := ALPT.Left+ ALayout.Width;         //     \  /
    lPt2.Top  := ALPT.Top;                         //      \/
    lPt3.Left := ALPT.Left + (ALayout.Width / 2);  //      3
    lPt3.Top  := ALPT.Top+ALayout.Height;
    end;
  orSouthEast:
    begin
    lPt1.Left := ALPT.Left + (ALayout.Width/2);   //   +---1---+
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left;                       //   2       |
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left + ALayout.Width;       //   +-------3
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  orEast:
    begin
    lPt1.Left := ALPT.Left;                       //   1
    lPt1.Top  := Alpt.Top ;                       //   |\
    lPt2.Left := ALPT.Left + ALayout.Width;       //   | \ 2
    lPt2.Top  := ALPT.Top + (ALayout.Height / 2); //   | /
    lPt3.Left := ALPT.Left;                       //   |/
    lPt3.Top  := Alpt.Top + ALayout.Height;       //   3
    end;
  orNorthWest:
    begin
    lPt1.Left := ALPT.Left;                       //   1-------+
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left+ALayout.width;         //   |       2
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left + ALayout.Width/2;     //   +---3---+
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  orWest:
    begin
    lPt1.Left := ALPT.Left + ALayout.Width;      //       1
    lPt1.Top  := ALPT.Top;                       //      /|
    lPt2.Left := ALPT.Left;                      //   2 / |
    lPt2.Top  := ALPT.Top + ALayout.Height / 2;  //     \ |
    lPt3.Left := ALPT.Left + ALayout.Width;      //      \|
    lPt3.Top  := ALPT.Top+ ALayout.Height;       //       3
    end;
  orSouthWest:
    begin
    lPt1.Left := ALPT.Left+ ALayout.Height/2;     //   +---1---+
    lPt1.Top  := ALPT.Top;                        //   |       |
    lPt2.Left := ALPT.Left+ALayout.width;         //   |       2
    lPt2.Top  := ALPT.Top + ALayout.Height/2;     //   |       |
    lPt3.Left := ALPT.Left ;                      //   3-------+
    lPt3.Top  := lPt1.Top + aLayout.height;
    end;
  end;
  DrawLine(lpt1,lpt2);
  DrawLine(lpt2,lpt3);
  DrawLine(lpt3,lpt1);
end;

function TFPReportExportCanvas.GetBandHandleHeight: Integer;
begin
  if (FBandHandleHeight=0) and (Canvas<>Nil) then
    FBandHandleHeight:=Canvas.TextHeight('W');
  Result:=FBandHandleHeight;
end;

procedure TFPReportExportCanvas.RenderShape(const ABand: TFPReportCustomBand; const AShape: TFPReportCustomShape);

var
  lPt1: TFPReportPoint;  // original Report point
  BL,SL : TFPReportLayout;
  SR : Trect;

begin
  BL:=GetLayout(ABand);
  SL:=GetLayout(AShape);
  SR:=GetElementRect(BL,SL);
  { Frame must be drawn before the shape as it could have a fill color. }
  RenderFrame(AShape.Frame, SR, ABand.Frame.BackgroundColor);
  { exit if Shape will not be visible. }
  if (TFPReportShape(AShape).Color = fpreport.clNone)
  or (TFPReportShape(AShape).Color = AShape.Frame.BackgroundColor) then
    exit;
  Canvas.Pen.Color:=TFPReportShape(AShape).Color;
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Width:=1;
  lPt1.Left := BL.Left + SL.Left;
  lPt1.Top := BL.Top + SL.Top;
  case TFPReportShape(AShape).ShapeType of
    stEllipse: RenderShapeEllipse(lpt1,SL);
    stCircle: RenderShapeCircle(lpt1,SL);
    stLine: RenderShapeLine(lpt1,TFPReportShape(AShape).Orientation, SL);
    stSquare: RenderShapeRect(lpt1,SL);
    stTriangle: RenderShapeTriangle(lpt1,TFPReportShape(AShape).Orientation, SL);
  end;
  if DrawMode=dmDesign then
    begin
    SR:=CoordToRect(LPT1,SL.Width,SL.Height);
    DrawElementCorners(AShape,SR);
    end;
end;

procedure TFPReportExportCanvas.RenderImage(const ABand: TFPReportCustomBand; const AImage: TFPReportCustomImage);
var
  lPt: TFPReportPoint;
  img: TReportImageFriend;
  PT : TPoint;
  R : TRect;
  G : TBitmap;
  BL,IL : TFPReportLayout;

begin
  img := TReportImageFriend(AImage);  { for access to Protected methods }
  BL:=GetLayout(ABand);
  IL:=GetLayout(AImage);
  lPt.Left := BL.Left + IL.Left;
  lPt.Top := BL.Top + IL.Top;
  PT:=CoordToPoint(Lpt,0,0);

  { Frame must be drawn before the Image as it could have a fill color. }
  RenderFrame(ABand, AImage.Frame, lPt, IL.Width, IL.Height);
  if not Assigned(img.Image) then
    Exit; { nothing further to do }
  G:=CreateBitmapFromFPImage(img.Image);
  try
    if img.Stretched then
      begin
      R:=Rect(pt.X,pT.Y,pt.X+HmmToPixels(IL.Width), pt.Y+VmmToPixels(IL.Height));
      Canvas.StretchDraw(R,G)
      end
    else
      Canvas.Draw(pt.X,pT.Y,G);
  finally
    G.Free;
  end;
  if DrawMode=dmDesign then
    DrawElementCorners(AImage,R);
end;

procedure TFPReportExportCanvas.RenderCheckbox(const ABand: TFPReportCustomBand; const ACheckbox: TFPReportCustomCheckbox);
var
  lPt: TFPReportPoint;
  pt : TPoint;
  cb: TReportCheckboxFriend;
  lImage: TFPCustomImage;
  G : TBitmap;
  R : TRect;
  BL,CL : TFPReportLayout;

begin
  cb := TReportCheckboxFriend(ACheckbox);  { for access to Protected methods }
  BL:=GetLayout(ABand);
  CL:=GetLayout(ACheckbox);
  lPt.Left := BL.Left + CL.Left;
  lPt.Top := BL.Top + CL.Top;
  Pt:=CoordToPoint(lpt);
  lImage:=cb.GetRTImage;
  G:=CreateBitmapFromFPImage(lImage);
  try
    R:=Rect(pt.X,Pt.Y,pt.X+HmmToPixels(CL.Width), pt.Y+VmmToPixels(CL.Height));
    Canvas.StretchDraw(R,g);
  finally
    G.Free;
  end;
  if DrawMode=dmDesign then
    DrawElementCorners(ACheckBox,R);
end;

constructor TFPReportExportCanvas.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHDPI := Screen.PixelsPerInch;
  FVDPI := FHDPI;
  Zoom:=1;
  FImageWidth := 0;
  FImageHeight := 0;
  // store the original DPI, we will restore it later
  FFonts:=TFPObjectHashTable.Create(True);
  FShowBandTypeNames:=True;
end;

destructor TFPReportExportCanvas.Destroy;
begin
  FreeAndNil(FHyperLinks);
  FreeAndNil(FFonts);
  inherited Destroy;
end;

procedure TFPReportExportCanvas.GetCurrentPageRenderSize(out AWidth,
  AHeight: Integer);
begin
  GetPageRenderSize(CurrentPage,AWidth,AHeight);
end;

procedure TFPReportExportCanvas.GetPageRenderSize(APage: TFPReportCustomPage;
  out AWidth, AHeight: Integer);
begin
  if (APage.Orientation=poPortrait) then
    begin
    AWidth := HmmToPixels(APage.PageSize.Width);
    AHeight := VmmToPixels(APage.PageSize.Height);
    end
  else
    begin
    AWidth := VmmToPixels(APage.PageSize.Height);
    AHeight := HmmToPixels(APage.PageSize.Width);
    end;
end;

procedure TFPReportExportCanvas.RenderElement(ABand : TFPReportCustomBand; Element : TFPReportElement);

Var
  C : TFPReportPoint;
  LB,LE : TFPReportLayout;

begin
  {$IFDEF DEBUGRD}
  Writeln('Rendering element ',Element.ClassName,' (',Element.Name,')');
  {$ENDIF}
  if (Element is TFPReportCustomBand)  then
    RenderBand(Element as TFPReportCustomBand);
  if Element is TFPReportCustomMemo then
    RenderMemo(Aband,TFPReportCustomMemo(Element))
  else if Element is TFPReportCustomShape then
    RenderShape(ABand,TFPReportCustomShape(Element))
  else if Element is TFPReportCustomImage then
    RenderImage(Aband,TFPReportCustomImage(Element))
  else if Element is TFPReportCustomCheckbox then
    RenderCheckbox(ABand,TFPReportCustomCheckbox(Element))
  else if not (Element is TFPReportCustomBand) then
    begin
    LB:=GetLayout(ABand);
    LE:=GetLayout(Element);
    C.Left := LB.Left + LE.Left;
    C.Top := LB.Top + LE.Top ; // + Element.RTLayout.Height;
    RenderFrame(ABand, Element.Frame, C, LE.Width, LE.Height);
    C.Left:=LB.Left;
    C.Top:=LB.Top;
    RenderUnknownElement(C,Element,Self.VDPI);
    end;
end;

procedure TFPReportExportCanvas.SetHyperlinksEnabled(AValue: Boolean);
begin
  {$IFDEF DEBUGRD}Writeln('TFPReportExportCanvas.SetHyperlinksEnabled(',AValue,')');{$ENDIF}
  if (AValue=GetHyperLinksEnabled) then exit;
  If AValue then
    FHyperLinks:=CreateHyperlinks
  else
    FreeAndNil(FHyperLinks);
end;

procedure TFPReportExportCanvas.SetPageIndex(AValue: Integer);
begin
  if FPageIndex=AValue then Exit;
  FPageIndex:=AValue;
  if Assigned(Report) and (FPageIndex<0) or (FPageIndex>=PageCount) then
    Raise EReportError.CreateFmt(SErrPageOutOfRange,[FPageIndex,PageCount-1]);
  RenderCurrentPage;
end;

function TFPReportExportCanvas.CreateHyperlinks: THyperLinkList;
begin
  {$IFDEF DEBUGRD}Writeln('TFPReportExportCanvas.CreateHyperlinks');{$ENDIF}
  Result:=THyperLinkList.Create(THyperLinkItem);
end;

function TFPReportExportCanvas.GetLayout(AElement: TFPReportElement
  ): TFPReportLayout;

begin
  if DrawMode=dmRender then
    Result:=AElement.RTLayout
  else
    Result:=AElement.Layout;
end;

function TFPReportExportCanvas.BandColorCode(ABand: TFPReportCustomBandClass;
  Edge: Boolean): TColor;

begin
  if Edge then
    Result := RGBtoBGR(DefaultBandRectangleColors[ABand.ReportBandType])
  else
    Result := RGBtoBGR(DefaultBandColors[ABand.ReportBandType]);
end;

procedure TFPReportExportCanvas.DrawBandLabel(Aband: TFPReportCustomBand; l : TFPReportLayout);
Var
  N : String;
  TH,X,Y : Integer;
  TopLeft : TFPReportPoint;
begin
  TopLeft.Left:=L.Left;
  TopLeft.Top:=L.Top;
  N:=ABand.Name;
  if N='' then
    N:='Unnamed '+DefaultBandNames[ABand.ReportBandType]+' band'
  else if ShowBandTypeNames then
    N:=N+' ('+DefaultBandNames[ABand.ReportBandType]+')';
  Canvas.Font.Name:='default';
  Canvas.Font.Size:=10;
  Canvas.Font.Style:=[];
  Canvas.Font.Color:=clBlack;
  TH:=GetBandHandleHeight;
  X:=FHorzOffset+HmmToPixels(TopLeft.Left);
  Y:=FVertOffset+VmmToPixels(TopLeft.Top)-TH-2*BandTitleMargin;
  Canvas.Brush.Color:=BandColorCode(TFPReportCustomBandClass(ABand.ClassType),False);
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Color:=BandColorCode(TFPReportCustomBandClass(ABand.ClassType),True);
  Canvas.Rectangle(X,Y,X+HmmToPixels(L.Width),Y+TH+2*BandTitleMargin);
  Y:=Y+BandTitleMargin;
  X:=X+BandTitleMargin;
 {$IFDEF DEBUGRD}Writeln('Writing name : Canvas.TextOut(',X,',',Y,',',N,')');{$ENDIF}
  Canvas.TextOut(X,Y,N);
end;

procedure TFPReportExportCanvas.DrawElementCorners(E : TFPReportElement; R : TRect);


begin
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Color:=clEmementCorner;
  // Horizontal, top
  Canvas.Line(R.Left,R.Top,R.Left+ElementHCornerLength,R.Top);
  Canvas.Line(R.Right-ElementHCornerLength,R.Top,R.Right,R.Top);
  // Horizontal, bottom
  Canvas.Line(R.Left,R.Bottom,R.Left+ElementHCornerLength,R.Bottom);
  Canvas.Line(R.Right-ElementHCornerLength,R.Bottom,R.Right,R.Bottom);
  // Vertical, Top
  Canvas.Line(R.Left,R.Top,R.Left,R.Top+ElementVCornerLength);
  Canvas.Line(R.Right,R.Top,R.Right,R.Top+ElementVCornerLength);
  // Vertical, bottom
  Canvas.Line(R.Left,R.Bottom,R.Left,R.Bottom-ElementVCornerLength);
  Canvas.Line(R.Right,R.Bottom,R.Right,R.Bottom-ElementVCornerLength);
end;

procedure TFPReportExportCanvas.DrawBandRect(Aband: TFPReportCustomBand; l : TFPReportLayout);

Var
  DR : TRect;

begin
  DR:=GetBandRect(L,False);
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Style:=psSolid;
  Canvas.Pen.Color:=BandColorCode(TFPReportCustomBandClass(ABand.ClassType),true);
  Canvas.Rectangle(DR);
  DrawElementCorners(ABand,DR);
end;

procedure TFPReportExportCanvas.RenderBand(Aband: TFPReportCustomBand);

Var
  lPt1: TFPReportPoint;  // original Report point
  I : integer;
  L : TFPReportLayout;

begin
  L:=GetLayout(ABand);
  {$IFDEF DEBUGRD}
  Writeln('Renderband ',ABand.ClassName,' : ',ABand.Name,' (',L.Width,' x ',L.Height,')');
  {$ENDIF}
  if DrawMode=dmDesign then
    begin
    DrawBandLabel(ABand,L);
    DrawBandRect(Aband,L);
    end;
  lpt1.Left:=L.Left;
  lpt1.Top:=L.Top;
  RenderFrame(Aband, Aband.Frame, lPt1, L.Width, L.Height);
  if DrawMode=dmRender then
    for I:=0 to Aband.ChildCount-1 do
      RenderElement(ABand,Aband.Child[i]);
end;

procedure TFPReportExportCanvas.RenderCurrentPage;

var
  b: integer;
  rpage: TFPReportPage;

begin
  If Not Assigned(Canvas) then exit;
  rpage := CurrentPage;
  If Not Assigned(rpage) then exit;
  SetupPageRender(rpage);
  for b := 0 to (rpage.BandCount - 1) do
    RenderBand(rpage.Bands[b]);
end;

function TFPReportExportCanvas.GetBandRect(L: TFPReportLayout;IncludeHandle: Boolean): TRect;

Var
  LPT1 : TFPReportPoint;

begin
  lpt1.Left:=L.Left;
  lpt1.Top:=L.Top;
  Result:=CoordToRect(LPT1,L.Width, L.Height);
  if IncludeHandle then
    Result.Top:=Result.Top-GetBandHandleHeight;
end;

function TFPReportExportCanvas.GetBandRect(ABand: TFPReportCustomBand;
  IncludeHandle: Boolean): TRect;
begin
  Result:=GetBandRect(GetLayout(ABand),IncludeHandle);
end;

function TFPReportExportCanvas.GetElementRect(BandLayout,
  ElementLayout: TFPReportLayout): TRect;

Var
  LPT1 : TFPReportPoint;

begin
  lpt1.Left:=BandLayout.Left+ElementLayout.Left;
  lpt1.Top:=BandLayout.Top+ElementLayout.Top;
  Result:=CoordToRect(LPT1,ElementLayout.Width,ElementLayout.Height);
end;

function TFPReportExportCanvas.GetElementRect(ABand: TFPReportCustomBand;
  AElement: TFPReportElement): TRect;
begin
  Result:=GetElementRect(GetLayout(ABand),GetLayout(AElement));
end;

procedure TFPReportExportCanvas.DrawSelectionHandle(ACenter: TPoint;
  AColor: TColor);

begin
  With Canvas do
    begin
    Brush.Color:=AColor;
    Brush.Style:=bsSolid;
    Pen.Color:=AColor;
    Pen.Style:=psSolid;
    {$IFDEF DEBUGRD}Writeln('Drawing selection handle at (',ACenter.X,',',ACenter.Y,')');{$ENDIF}
    with ACenter do
      Rectangle(X-ReSizeHandleHalfWidth,Y-ReSizeHandleHalfWidth,X+ReSizeHandleHalfWidth,Y+ReSizeHandleHalfWidth);
    end;
end;

procedure TFPReportExportCanvas.DrawSelectionRect(ARect: Trect);
begin
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Color:=clSelectionRect;
  Canvas.Pen.Style:=psDash;
  Canvas.Rectangle(ARect);
end;

procedure TFPReportExportCanvas.DoExecute(const ARTObjects: TFPList);

begin
  FPages:=ARTObjects;
  RenderCurrentPage;
end;

end.

