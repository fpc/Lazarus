{
 *****************************************************************************
 *                             FpGUIObjects.pas                              *
 *                              --------------                               *
 *      Place for wrapper classes which aren't widgets                       *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit fpguiobjects;

{$mode objfpc}{$H+}

{.$DEFINE VERBOSEDEBUG_FPGUI_REGION}

interface

uses
  // RTL, FCL, LCL
  Classes, SysUtils,
  Graphics, Menus, LCLType,
  // Widgetset
  fpguiwsprivate,
  //Others
  fpguiproc,
  // interface
  fpg_main, fpg_base, fpg_menu;

type

  TFPGUIRegionType=(eRegionNULL,eRegionSimple,eRegionComplex,eRegionNotCombinableOrError);
  TFPGUIRegionCombine=(eRegionCombineAnd,eRegionCombineCopy, eRegionCombineDiff, eRegionCombineOr, eRegionCombineXor);

  TFPGUIWinAPIElement = class(TObject);

  TFPGUIWinAPIObject = class(TFPGUIWinAPIElement);


  type TFPGUIWinAPIBrushStyle = (
    bsSolid,
    bsClear
  );

  type TFPGUIWinAPIPenStyle = (
    psSolid,
    psClear
  );

  type tagTFPGUIBrush= record
    Color: TfpgColor;
    Style: TFPGUIWinAPIBrushStyle;
  end;

  type tagTFPGUIPen= record
    Color: TfpgColor;
    Width: Integer;
    Style: TFPGUIWinAPIPenStyle;
  end;

  { TFPGUIWinAPIBrush }

  TFPGUIWinAPIBrush = class (TFPGUIWinAPIObject)
  private
    FBrush: tagTFPGUIBrush;
    function GetColor: TfpgColor;
    function GetStyle: TFPGUIWinAPIBrushStyle;
    procedure SetColor(const AValue: TfpgColor);
    procedure SetStyle(AValue: TFPGUIWinAPIBrushStyle);
  public
    property Color: TfpgColor read GetColor Write SetColor;
    property Style: TFPGUIWinAPIBrushStyle read GetStyle write SetStyle;
    Constructor Create;
    Constructor Create(const ABrushData: TLogBrush);
    Destructor Destroy; override;
  end;

  { TFPGUIWinAPIPen }

  TFPGUIWinAPIPen = class (TFPGUIWinAPIObject)
  private
    FPen: tagTFPGUIPen;
    function GetColor: TfpgColor;
    function GetLineWidthX: integer;
    function GetStyle: TFPGUIWinAPIPenStyle;
    procedure SetColor(const AValue: TfpgColor);
    procedure SetLineWidthX(AValue: integer);
    procedure SetStyle(AValue: TFPGUIWinAPIPenStyle);
  public
    property Color: TfpgColor read GetColor Write SetColor;
    property LineWidthX: integer read GetLineWidthX write SetLineWidthX;
    property Style: TFPGUIWinAPIPenStyle read GetStyle write SetStyle;
    Constructor Create;
    Constructor Create(const APenData: TLogPen);
    Destructor Destroy; override;
  end;

  { TFPGUIWinAPIFont }

  TFPGUIWinAPIFont = class (TFPGUIWinAPIObject)
  private
    fpgFont: TfpgFontBase;
    FFontHeight: integer;
    FFontFace: String;
    function GetFontHeight: integer;
    function GetFontSize: integer;
    procedure SetFontHeight(const AValue: integer);
    procedure SetFontSize(const AValue: integer);
  public
    Constructor Create;
    Constructor Create(const AFontData: TFontData);
    Constructor Create(const AfpgCanvas: TfpgCanvas);
    Constructor Create(const AFontData: TLogFont);
    Constructor Create(const AFontData: TLogFont; const ALongFontName: string);
    Destructor Destroy; override;
    property fpguiFont: TfpgFontBase read fpgFont write fpgFont;
    property Size: integer read GetFontSize write SetFontSize;
    property Height: integer read GetFontHeight write SetFontHeight;
  end;

  { TFPGUIWinAPIBitmap }

  TFPGUIWinAPIBitmap = class(TFPGUIWinAPIObject)
  private
    fpgImage: TfpgImage;
  protected
    SelectedInDC: HDC;
  public
    Constructor Create(const ABitsPerPixel,Width,Height: integer);
    Destructor Destroy; override;
    property Image: TfpgImage read fpgImage;
  end;

  TFPGUIBasicRegion = class;

  { TFpGuiDeviceContext }

  TFPGUIDeviceContext = class(TFPGUIWinAPIElement)
  private
    FDCStack: array of TFPGUIDeviceContext;
    procedure CopyDCToInstance(const ATarget: TFPGUIDeviceContext);
    procedure SetupFont;
    procedure SetupBrush;
    procedure SetupPen;
    procedure SetupBitmap;
    procedure SetupClipping;
  public
    fpgCanvas: TfpgCanvas;
    FPrivateWidget: TFPGUIPrivateWidget;
    FOrg: TPoint;
    FDrawXY: TPoint;
    FBrush: TFPGUIWinAPIBrush;
    FPen: TFPGUIWinAPIPen;
    FFont: TFPGUIWinAPIFont;
    FTextColor: TfpgColor;
    FBitmap: TFPGUIWinAPIBitmap;
    FClipping: TFPGUIBasicRegion;
  public
    constructor Create(AFPGUIPrivate: TFPGUIPrivateWidget);
    destructor Destroy; override;
    procedure SetOrigin(const AX,AY: integer);
    function SaveDC: integer;
    function RestoreDC(const Index: SizeInt): Boolean;
    function SelectObject(const AGDIOBJ: HGDIOBJ): HGDIOBJ;
    function SetTextColor(const AColor: TColorRef): TColorRef;
    function GetTextColor: TColorRef;
    function PrepareRectOffsets(const ARect: TRect): TfpgRect;
    function PreparePointOffsets(const APoint: TPoint): TPoint;
    function UnPrepareRectOffsets(const ARect: TRect): TfpgRect;
    function UnPreparePointOffsets(const APoint: TPoint): TPoint;
    procedure ClearRectangle(const AfpgRect: TfpgRect);
    procedure ClearDC;
    function UseBrush: Boolean;
    function UsePen: Boolean;
  end;

  { TFPGUIPrivateMenuItem }

  TFPGUIPrivateMenuItem = class(TObject)
  private
  protected
  public
    MenuItem: TfpgMenuItem;
    LCLMenuItem: TMenuItem;
    procedure HandleOnClick(ASender: TObject);
  end;

  { TFPGUIBasicRegion }

  TFPGUIBasicRegion=class(TFPGUIWinAPIObject)
  private
    FRegionType: TFPGUIRegionType;
    function GetDebugString: string;
    function GetfpgRectRegion: TfpgRect;
    function GetRegionType: TFPGUIRegionType;
  protected
    FRectRegion: TRect;
  public
    constructor Create; overload;
    constructor Create(const ARect: TRect); overload;
    destructor Destroy; override;
    procedure CreateRectRegion(const ARect: TRect);
    function CombineWithRegion(const ARegion: TFPGUIBasicRegion; const ACombineMode: TFPGUIRegionCombine): TFPGUIBasicRegion;
    property RegionType: TFPGUIRegionType read GetRegionType;
    property fpgRectRegion: TfpgRect read GetfpgRectRegion;
    property DebugString: string read GetDebugString;
  end;

  function FPGUIGetDesktopDC(): TFPGUIDeviceContext;

implementation

var
  FPGUIDesktopDC: TFPGUIDeviceContext=nil;

function FPGUIGetDesktopDC(): TFPGUIDeviceContext;
var
  a: TCreateParams;
begin
  if not Assigned(FPGUIDesktopDC) then begin
    FPGUIDesktopDC:=TFPGUIDeviceContext.Create(nil);
    //FPGUIDesktopDC:=TFPGUIDeviceContext.Create(TFPGUIPrivateCustomPanel.Create(nil,a));
  end;
  Result:=FPGUIDesktopDC;
end;

{ TFPGUIWinAPIBitmap }

constructor TFPGUIWinAPIBitmap.Create(const ABitsPerPixel, Width,
  Height: integer);
begin
  fpgImage:=TfpgImage.Create;
  fpgImage.AllocateImage(ABitsPerPixel,Width,Height);
  fpgImage.UpdateImage;
end;

destructor TFPGUIWinAPIBitmap.Destroy;
var
  Context: TFPGUIDeviceContext;
begin
  Context:=TFPGUIDeviceContext(SelectedInDC);
  if Assigned(Context) then begin
    Context.FBitmap:=nil;
  end;
  fpgImage.Free;
  inherited Destroy;
end;

{ TFpGuiDeviceContext }

procedure TFPGUIDeviceContext.CopyDCToInstance(
  const ATarget: TFPGUIDeviceContext);
begin
  ATarget.fpgCanvas:=fpgCanvas;
  ATarget.FPrivateWidget:=FPrivateWidget;
  ATarget.FBrush:=FBrush;
  ATarget.FPen:=FPen;
  ATarget.FFont:=FFont;
  ATarget.FOrg:=FOrg;
  ATarget.FDrawXY:=FDrawXY;
  ATarget.FTextColor:=FTextColor;
  ATarget.FClipping:=FClipping;
end;

procedure TFPGUIDeviceContext.SetupFont;
begin
  if Assigned(fpgCanvas) then
   if Assigned(FFont) then
    fpgCanvas.Font:=FFont.fpguiFont;
end;

procedure TFPGUIDeviceContext.SetupBrush;
begin
  if Assigned(fpgCanvas) then
   if Assigned(FBrush) then
    fpgCanvas.Color:=FBrush.Color;
end;

procedure TFPGUIDeviceContext.SetupPen;
begin
  if Assigned(fpgCanvas) then begin
    if Assigned(FPen) then begin
     fpgCanvas.Color:=FPen.Color;
     fpgCanvas.SetLineStyle(FPen.LineWidthX,TfpgLineStyle.lsSolid);
    end;
  end;
end;

procedure TFPGUIDeviceContext.SetupBitmap;
begin
  if Assigned(fpgCanvas) then
    fpgCanvas.DrawImage(0,0,FBitmap.fpgImage);
end;

procedure TFPGUIDeviceContext.SetupClipping;
var
  r: TfpgRect;
  RECT: TRECT;
begin
  if Assigned(fpgCanvas) then
    if Assigned(FClipping) then begin
      RECT:=fpgRectToRect(FClipping.fpgRectRegion);
      r:=PrepareRectOffsets(RECT);
      // Note: The fpGUI clipping is one pixel smaller than WinAPI, or it looks like...
      r.Width:=r.Width+1;
      r.Height:=r.Height+1;
      fpgCanvas.SetClipRect(r);
    end else begin
      fpgCanvas.ClearClipRect;
    end;
end;

constructor TFPGUIDeviceContext.Create(AFPGUIPrivate: TFPGUIPrivateWidget);
begin
  // The Widget.Visible check is just for extra precaution.
  if Assigned(AFPGUIPrivate) and AFPGUIPrivate.Widget.Visible then
  begin
    fpgCanvas := AFPGUIPrivate.Widget.Canvas;
    fpgCanvas.BeginDraw;
    FPrivateWidget:=AFPGUIPrivate;
    FPrivateWidget.DC:=HDC(Self);
  end
  else
  begin
    fpgCanvas := nil;
    FPrivateWidget := nil;
  end;

  with FOrg do begin
    X:=0;
    Y:=0;
  end;
  with FDrawXY do begin
    X:=0;
    Y:=0;
  end;
  FBrush:=nil;
  FPen:=nil;
  FFont:=nil;
end;

destructor TFPGUIDeviceContext.Destroy;
var
  j: integer;
begin
  if Assigned(fpgCanvas) then fpgCanvas.EndDraw;
  for j := 0 to High(FDCStack) do begin
    FDCStack[j].Free;
    FDCStack[j]:=nil;
  end;
  if Assigned(FPrivateWidget) then
    FPrivateWidget.DC:=0;
end;

procedure TFPGUIDeviceContext.SetOrigin(const AX, AY: integer);
begin
  With FOrg do begin
    X:=AX;
    Y:=AY;
  end;
end;

function TFPGUIDeviceContext.SaveDC: integer;
var
  Tmp: TFPGUIDeviceContext;
begin
  SetLength(FDCStack,Length(FDCStack)+1);
  Tmp:=TFPGUIDeviceContext.Create(FPrivateWidget);
  FDCStack[High(FDCStack)]:=Tmp;
  Self.CopyDCToInstance(Tmp);
  Result:=High(FDCStack);
end;

function TFPGUIDeviceContext.RestoreDC(const Index: SizeInt): Boolean;
var
  Tmp: TFPGUIDeviceContext;
  TargetIndex: SizeInt;
  j: SizeInt;
begin
  Result:=false;
  if Index>=0 then begin
    TargetIndex:=Index;
    if TargetIndex>High(FDCStack) then Exit;
  end else begin
    TargetIndex:=High(FDCStack)-Index+1;
    If TargetIndex<0 then Exit;
  end;
  Tmp:=FDCStack[TargetIndex];
  Tmp.CopyDCToInstance(Self);
  if Assigned(FPrivateWidget) then begin
    FPrivateWidget.DC:=HDC(Self);
  end;
  SetupFont;
  SetupBrush;
  SetupPen;
  SetupClipping;
  for j := TargetIndex to High(FDCStack) do begin
    FDCStack[j].Free;
    FDCStack[j]:=nil;
  end;
  SetLength(FDCStack,TargetIndex);
  Result:=true;
end;

function TFPGUIDeviceContext.SelectObject(const AGDIOBJ: HGDIOBJ): HGDIOBJ;
var
  gObject: TObject;
begin
  Result:=0;
  gObject:=TObject(AGDIOBJ);
  if AGDIOBJ<5 then begin
    case AGDIOBJ of
      1:  begin
            Result:=HGDIOBJ(FFont);
            FFont:=nil;
          end;
      2:  begin
            Result:=HGDIOBJ(FBrush);
            FBrush:=nil;
          end;
      3:  begin
            Result:=HGDIOBJ(FPen);
            FPen:=nil;
          end;
      4:  begin
            Result:=HGDIOBJ(FBitmap);
            FBitmap:=nil;
          end;
      5:  begin
            Result:=HGDIOBJ(FClipping);
            FClipping:=nil;
          end;
    end;
    Exit;
  end;
  if gObject is TFPGUIWinAPIFont then begin
    Result:=HGDIOBJ(FFont);
    FFont:=TFPGUIWinAPIFont(gObject);
    SetupFont;
    if Result=0 then Result:=1;
  end else if gObject is TFPGUIWinAPIBrush then begin
    Result:=HGDIOBJ(FBrush);
    FBrush:=TFPGUIWinAPIBrush(gObject);
    SetupBrush;
    if Result=0 then Result:=2;
  end else if gObject is TFPGUIWinAPIPen then begin
    Result:=HGDIOBJ(FPen);
    FPen:=TFPGUIWinAPIPen(gObject);
    SetupPen;
    if Result=0 then Result:=3;
  end else if gObject is TFPGUIWinAPIBitmap then begin
    Result:=HGDIOBJ(FBitmap);
    FBitmap:=TFPGUIWinAPIBitmap(gObject);
    FBitmap.SelectedInDC:=HDC(Self);
    SetupBitmap;
    if Result=0 then Result:=4;
  end else if gObject is TFPGUIBasicRegion then begin
    Result:=HGDIOBJ(FClipping);
    FClipping:=TFPGUIBasicRegion(gObject);
    SetupClipping;
    if Result=0 then Result:=5;
  end;
end;

function TFPGUIDeviceContext.SetTextColor(const AColor: TColorRef): TColorRef;
begin
  Result:=TTfpgColorToTColor(FTextColor);
  FTextColor:=TColorToTfpgColor(AColor);
end;

function TFPGUIDeviceContext.GetTextColor: TColorRef;
begin
  Result:=TTfpgColorToTColor(FTextColor);
end;

function TFPGUIDeviceContext.PrepareRectOffsets(const ARect: TRect): TfpgRect;
begin
  TRectTofpgRect(ARect,Result);
  AdjustRectToOrg(Result,FOrg);
  if Assigned(FPrivateWidget) then
    FPrivateWidget.AdjustRectXY(Result);
end;

function TFPGUIDeviceContext.PreparePointOffsets(const APoint: TPoint): TPoint;
begin
  Result:=APoint;
  AdjustPointToOrg(Result,FOrg);
  if Assigned(FPrivateWidget) then
    FPrivateWidget.AdjustPointXY(Result);
end;

function TFPGUIDeviceContext.UnPrepareRectOffsets(const ARect: TRect): TfpgRect;
var
  r: TRect;
begin
  r:=ARect;
  if Assigned(FPrivateWidget) then
    FPrivateWidget.AdjustRectXYToInterface(r);
  TRectTofpgRect(r,Result)
end;

function TFPGUIDeviceContext.UnPreparePointOffsets(const APoint: TPoint
  ): TPoint;
begin
  Result:=APoint;
  if Assigned(FPrivateWidget) then
    FPrivateWidget.AdjustPointXYToInterface(Result);
end;

procedure TFPGUIDeviceContext.ClearRectangle(const AfpgRect: TfpgRect);
var
  OldColor: TfpgColor;
begin
  OldColor:=fpgCanvas.Color;
  fpgCanvas.Color:=FPrivateWidget.Widget.BackgroundColor;
  fpgCanvas.FillRectangle(AfpgRect);
  fpgCanvas.Color:=OldColor;
end;

procedure TFPGUIDeviceContext.ClearDC;
begin
  ClearRectangle(fpgCanvas.GetClipRect);
end;

function TFPGUIDeviceContext.UseBrush: Boolean;
begin
  Result:=true;
  fpgCanvas.Color:=FBrush.Color;
  if FBrush.Style=TFPGUIWinAPIBrushStyle.bsClear then begin
    Result:=false;
  end;
end;

function TFPGUIDeviceContext.UsePen: Boolean;
begin
  fpgCanvas.Color:=FPen.Color;
  case FPen.Style of
    psSolid: begin
      Result:=true;
    end;
    psClear: begin
      Result:=false;
    end;
  end;
end;

{ TFPGUIPrivateMenuItem }

procedure TFPGUIPrivateMenuItem.HandleOnClick(ASender: TObject);
begin
  if Assigned(LCLMenuItem) and Assigned(LCLMenuItem.OnClick) then
   LCLMenuItem.OnClick(LCLMenuItem);
end;

{ TFPGUIWinAPIFont }

function TFPGUIWinAPIFont.GetFontHeight: integer;
begin
  Result:=FFontHeight;
end;

function TFPGUIWinAPIFont.GetFontSize: integer;
begin
  Result:=(-FFontHeight * 72) div 96;
end;

procedure TFPGUIWinAPIFont.SetFontHeight(const AValue: integer);
begin
  FFontHeight:=AValue;
end;

procedure TFPGUIWinAPIFont.SetFontSize(const AValue: integer);
begin
  FFontHeight:=(-96 * AValue) div 72;
end;

constructor TFPGUIWinAPIFont.Create;
begin
  FFontFace:='';
  Size:=8;
  fpgFont:=fpgGetFont('');
end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TFontData);
var
  lFontDesc: string;
begin
  lFontDesc:=TFontToTfpgFontDesc(AFontData);
  FFontHeight:=AFontData.Height*-1;
  FFontFace:=AFontData.Name;
  fpgFont:=fpgGetFont(lFontDesc);
end;

constructor TFPGUIWinAPIFont.Create(const AfpgCanvas: TfpgCanvas);
begin
  fpgFont:=AfpgCanvas.Font;
end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TLogFont);
var
  lFontDesc: string;
begin
  lFontDesc:=AFontData.lfFaceName;
  if AFontData.lfHeight<0 then begin
    lFontDesc:=lFontDesc+'-'+inttostr(AFontData.lfHeight*-1);
  end;
  if AFontData.lfWeight>400 then lfontdesc:=lFontDesc+':bold';
  if AFontData.lfItalic>0 then lFontDesc:=lFontDesc+':italic';
  if AFontData.lfUnderline>0 then lFontDesc:=lFontDesc+':underline';
  if AFontData.lfStrikeOut>0 then lfontdesc:=lfontdesc+':strikeout';

  FFontHeight:=AFontData.lfHeight;
  FFontFace:=AFontData.lfFaceName;
  fpgFont:=fpgGetFont(lFontDesc);

end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TLogFont;
  const ALongFontName: string);
var
  lFontDesc: string;
begin
  lFontDesc:=ALongFontName;
  if AFontData.lfHeight<0 then begin
    lFontDesc:=lFontDesc+'-'+inttostr(round((AFontData.lfHeight*-1)*72/96));
  end;
  if AFontData.lfWeight>400 then lfontdesc:=lFontDesc+':bold';
  if AFontData.lfItalic>0 then lFontDesc:=lFontDesc+':italic';
  if AFontData.lfUnderline>0 then lFontDesc:=lFontDesc+':underline';
  if AFontData.lfStrikeOut>0 then lfontdesc:=lfontdesc+':strikeout';

  FFontFace:=ALongFontName;
  FFontHeight:=AFontData.lfHeight;
  fpgFont:=fpgGetFont(lFontDesc);
end;

destructor TFPGUIWinAPIFont.Destroy;
begin
  FreeAndNIL(fpgFont);
  inherited Destroy;
end;

{ TFPGUIWinAPIPen }

function TFPGUIWinAPIPen.GetColor: TfpgColor;
begin
  Result:=FPen.Color;
end;

function TFPGUIWinAPIPen.GetLineWidthX: integer;
begin
  Result:=FPen.Width;
end;

function TFPGUIWinAPIPen.GetStyle: TFPGUIWinAPIPenStyle;
begin
  Result:=FPen.Style;
end;

procedure TFPGUIWinAPIPen.SetColor(const AValue: TfpgColor);
begin
  FPen.Color:=AValue;
end;

procedure TFPGUIWinAPIPen.SetLineWidthX(AValue: integer);
begin
  FPen.Width:=AValue;
end;

procedure TFPGUIWinAPIPen.SetStyle(AValue: TFPGUIWinAPIPenStyle);
begin
  FPen.Style:=AValue;
end;

constructor TFPGUIWinAPIPen.Create;
begin
  FPen.Color:=0;
  FPen.Width:=1;
  FPen.Style:=psSolid;
end;

constructor TFPGUIWinAPIPen.Create(const APenData: TLogPen);
begin
  Create;
  FPen.Color:=TColorToTfpgColor(APenData.lopnColor);
  FPen.Width:=APenData.lopnWidth.x;
  case (APenData.lopnStyle and PS_STYLE_MASK) of
    0: FPen.Style:=psSolid;
    5: FPen.Style:=psClear;
    otherwise
      FPen.Style:=psSolid;
  end;
end;

destructor TFPGUIWinAPIPen.Destroy;
begin
  inherited Destroy;
end;

{ TFPGUIWinAPIBrush }

function TFPGUIWinAPIBrush.GetColor: TfpgColor;
begin
  Result:=FBrush.Color
end;

function TFPGUIWinAPIBrush.GetStyle: TFPGUIWinAPIBrushStyle;
begin
  Result:=FBrush.Style;
end;

procedure TFPGUIWinAPIBrush.SetColor(const AValue: TfpgColor);
begin
  FBrush.Color:=AValue;
end;

procedure TFPGUIWinAPIBrush.SetStyle(AValue: TFPGUIWinAPIBrushStyle);
begin
  FBrush.Style:=AValue;
end;

constructor TFPGUIWinAPIBrush.Create;
begin
  FBrush.Color:=TColorToTfpgColor(clBtnFace);
  FBrush.Style:=bsSolid;
end;

constructor TFPGUIWinAPIBrush.Create(const ABrushData: TLogBrush);
begin
  Create;
  FBrush.Color:=TColorToTfpgColor(ABrushData.lbColor);
  case ABrushData.lbStyle of
    0: FBrush.Style:=bsSolid;
    1: FBrush.Style:=bsClear;
    otherwise
      FBrush.Style:=bsSolid;
  end;
end;

destructor TFPGUIWinAPIBrush.Destroy;
begin
  inherited Destroy;
end;

{ TFPGUIBasicRegion }

function TFPGUIBasicRegion.GetRegionType: TFPGUIRegionType;
begin
  Result:=FRegionType;
end;

function TFPGUIBasicRegion.GetfpgRectRegion: TfpgRect;
begin
  TRectTofpgRect(FRectRegion,Result);
end;

function TFPGUIBasicRegion.GetDebugString: string;
begin
  Result:='';
  Str(FRegionType,Result);
  Result:=Result+': '+Format('%d,%d-%d,%d',[FRectRegion.Left,FRectRegion.Top,FRectRegion.Right,FRectRegion.Bottom]);
end;

constructor TFPGUIBasicRegion.Create;
var
  ARect: TRect;
begin
  FillByte(ARect,sizeof(ARect),0);
  CreateRectRegion(ARect);
end;

constructor TFPGUIBasicRegion.Create(const ARect: TRect);
begin
  CreateRectRegion(ARect);
end;

destructor TFPGUIBasicRegion.Destroy;
begin
  inherited Destroy;
end;

procedure TFPGUIBasicRegion.CreateRectRegion(const ARect: TRect);
begin
  FRectRegion:=ARect;
  if (FRectRegion.Left=FRectRegion.Top) and (FRectRegion.Right=FRectRegion.Bottom) and
    (FRectRegion.Top=FRectRegion.Bottom) then begin
    FRegionType:=eRegionNULL;
  end else begin
    FRegionType:=eRegionSimple;
  end;
end;

function TFPGUIBasicRegion.CombineWithRegion(const ARegion: TFPGUIBasicRegion;
  const ACombineMode: TFPGUIRegionCombine): TFPGUIBasicRegion;
  function Min(const V1,V2: SizeInt): SizeInt;
  begin
    if V1<V2 then Result:=V1 else Result:=V2;
  end;
  function Max(const V1,V2: SizeInt): SizeInt;
  begin
    if V1>V2 then Result:=V1 else Result:=V2;
  end;
  procedure CombineAnd(const TargetRegion: TFPGUIBasicRegion; const r1,r2: TRect);
  var
    Intersect: Boolean;
  begin
    if (r2.Left>r1.Right) or
       (r2.Right<r1.Left) or
       (r2.Top>r1.Bottom) or
       (r2.Bottom<r1.Top) then begin
      Intersect:=false;
    end else begin
      Intersect:=true;
    end;
  	if Intersect then begin
      TargetRegion.CreateRectRegion(
        classes.Rect(
          Max(r1.Left,r2.Left),
          Max(r1.Top,r2.Top),
          Min(r1.Right,r2.Right),
          Min(r1.Bottom,r2.Bottom)
        )
      );
   	end else begin
      TargetRegion.CreateRectRegion(classes.Rect(0,0,0,0));
    end;
  end;
begin
  Result:=TFPGUIBasicRegion.Create;
  Case ACombineMode of
    eRegionCombineAnd:  CombineAnd(Result,ARegion.FRectRegion,Self.FRectRegion);
    eRegionCombineCopy,
    eRegionCombineDiff:
      begin
        Result.CreateRectRegion(rect(0,0,0,0));
      end;
    eRegionCombineOr,
    eRegionCombineXor:
      begin
        Raise Exception.CreateFmt('Region mode %d not supported',[integer(ACombineMode)]);
      end;
  end;
{$IFDEF VERBOSEDEBUG_FPGUI_REGION}
    writeln('fpGUI Region: ',ARegion.DebugString,' ',ACombineMode,' ',Self.DebugString,' = ',Result.DebugString);
{$ENDIF}
end;

finalization
  FreeAndNil(FPGUIDesktopDC);

end.

