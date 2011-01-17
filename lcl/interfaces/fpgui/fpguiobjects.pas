{
 *****************************************************************************
 *                             FpGUIObjects.pas                              *
 *                              --------------                               *
 *      Place for wrapper classes which aren't widgets                       *
 *                                                                           *
 *****************************************************************************

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
}
unit fpguiobjects;

{$mode objfpc}{$H+}

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

  type tagTFPGUIBrush= record
    Color: TfpgColor;
  end;

  type tagTFPGUIPen= record
    Color: TfpgColor;
    Width: Integer;
  end;

  { TFPGUIWinAPIBrush }

  TFPGUIWinAPIBrush = class (TFPGUIWinAPIObject)
  private
    FBrush: tagTFPGUIBrush;
    function GetColor: TfpgColor;
    procedure SetColor(const AValue: TfpgColor);
  public
    property Color: TfpgColor read GetColor Write SetColor;
    Constructor Create;
    Constructor Create(const ABrushData: TLogBrush);
    Destructor Destroy; override;
  end;

  { TFPGUIWinAPIPen }

  TFPGUIWinAPIPen = class (TFPGUIWinAPIObject)
  private
    FPen: tagTFPGUIPen;
    function GetColor: TfpgColor;
    procedure SetColor(const AValue: TfpgColor);
  public
    property Color: TfpgColor read GetColor Write SetColor;
    Constructor Create;
    Constructor Create(const APenData: TLogPen);
    Destructor Destroy; override;
  end;

  { TFPGUIWinAPIFont }

  TFPGUIWinAPIFont = class (TFPGUIWinAPIObject)
  private
    fpgFont: TfpgFontBase;
    FFontHeight: integer;
    function GetFontHeight: integer;
    function GetFontSize: integer;
    procedure SetFontHeight(const AValue: integer);
    procedure SetFontSize(const AValue: integer);
  public
    FontFace: String;
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
    procedure SetupBitmap;
    procedure SetupClipping;
  public
    fpgCanvas: TfpgCanvas;
    FPrivateWidget: TFPGUIPrivateWidget;
    FOrg: TPoint;
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
    function PrepareRectOffsets(const ARect: TRect): TfpgRect;
    procedure ClearRectangle(const AfpgRect: TfpgRect);
    procedure ClearDC;
    procedure SetupPen;
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
  end;

  function FPGUIGetDesktopDC(): TFPGUIDeviceContext;

implementation

var
  FPGUIDesktopDC: TFPGUIDeviceContext=nil;

function FPGUIGetDesktopDC(): TFPGUIDeviceContext;
begin
  if not Assigned(FPGUIDesktopDC) then
   FPGUIDesktopDC:=TFPGUIDeviceContext.Create(nil);
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
  if Assigned(fpgCanvas) then
   if Assigned(FPen) then
    fpgCanvas.Color:=FPen.Color;
end;

procedure TFPGUIDeviceContext.SetupBitmap;
begin
  if Assigned(fpgCanvas) then
    fpgCanvas.DrawImage(0,0,FBitmap.fpgImage);
end;

procedure TFPGUIDeviceContext.SetupClipping;
var
  r: TfpgRect;
begin
  if Assigned(fpgCanvas) then
    if Assigned(FClipping) then begin
      r:=FClipping.fpgRectRegion;
      AdjustRectToOrg(r,FOrg);
      fpgCanvas.SetClipRect(r);
    end else begin
      fpgCanvas.ClearClipRect;
    end;
end;

constructor TFpGuiDeviceContext.Create(AFPGUIPrivate: TFPGUIPrivateWidget);
begin
  if Assigned(AFPGUIPrivate) then begin
    fpgCanvas := AFPGUIPrivate.Widget.Canvas;
    fpgCanvas.BeginDraw(false);
    AFPGUIPrivate.DC:=HDC(Self);
    FPrivateWidget := AFPGUIPrivate;
  end else begin
    fpgCanvas := nil;
    FPrivateWidget := nil;
  end;
  with FOrg do begin
    X:=0;
    Y:=0;
  end;
  FBrush:=nil;
  FPen:=nil;
  FFont:=nil;
end;

destructor TFpGuiDeviceContext.Destroy;
var
  j: integer;
begin
  if Assigned(fpgCanvas) then fpgCanvas.EndDraw;
  for j := 0 to High(FDCStack) do begin
    FDCStack[j].Free;
  end;
  if Assigned(FPrivateWidget) then
    FPrivateWidget.DC:=0;
end;

procedure TFpGuiDeviceContext.SetOrigin(const AX, AY: integer);
begin
  With FOrg do begin
    X:=AX;
    Y:=AY;
  end;
end;

function TFPGUIDeviceContext.SaveDC: Integer;
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
  FPrivateWidget.DC:=HDC(Self);
  SetupFont;
  SetupBrush;
  SetupPen;
  SetupClipping;
  for j := TargetIndex to High(FDCStack) do begin
    FDCStack[j].Free;
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
  Result:=FTextColor;
  FTextColor:=AColor;
  fpgCanvas.TextColor:=FTextColor;
end;

function TFPGUIDeviceContext.PrepareRectOffsets(const ARect: TRect): TfpgRect;
begin
  TRectTofpgRect(ARect,Result);
  AdjustRectToOrg(Result,FOrg);
  FPrivateWidget.AdjustRectXY(Result);
end;

procedure TFPGUIDeviceContext.ClearRectangle(const AfpgRect: TfpgRect);
var
  OldColor: TfpgColor;
begin
  OldColor:=fpgCanvas.Color;
  fpgCanvas.Color:= FPrivateWidget.Widget.BackgroundColor;
  fpgCanvas.FillRectangle(AfpgRect);
  if fpgCanvas.Color=0 then writeln(FPrivateWidget.LCLObject.Name);
  fpgCanvas.Color:=OldColor;
end;

procedure TFPGUIDeviceContext.ClearDC;
begin
  ClearRectangle(fpgCanvas.GetClipRect);
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
  FontFace:='';
  Size:=8;
end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TFontData);
begin
  FontFace:=AFontData.Name;
  Height:=AFontData.Height;
  fpgFont:=fpgGetFont(format('%s-%d',[AFontData.Name,Size]));
end;

constructor TFPGUIWinAPIFont.Create(const AfpgCanvas: TfpgCanvas);
begin
  fpgFont:=AfpgCanvas.Font;
end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TLogFont);
begin
  FontFace:=AFontData.lfFaceName;
  Height:=AFontData.lfHeight;
  if FontFace='' then begin
    fpgFont:=fpgGetFont(''); //Default
  end else begin
    fpgFont:=fpgGetFont(format('%s-%d',[FontFace,Size]));
  end;
end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TLogFont;
  const ALongFontName: string);
begin
  FontFace:=ALongFontName;
  Height:=AFontData.lfHeight;
  if FontFace='' then begin
    fpgFont:=fpgGetFont(''); //Default
  end else begin
    fpgFont:=fpgGetFont(format('%s-%d',[FontFace,Size]));
  end;
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

procedure TFPGUIWinAPIPen.SetColor(const AValue: TfpgColor);
begin
  FPen.Color:=AValue;
end;

constructor TFPGUIWinAPIPen.Create;
begin
end;

constructor TFPGUIWinAPIPen.Create(const APenData: TLogPen);
begin
  Create;
  FPen.Color:=APenData.lopnColor;
end;

destructor TFPGUIWinAPIPen.Destroy;
begin
  FreeAndNil(FPen);
  inherited Destroy;
end;

{ TFPGUIWinAPIBrush }

function TFPGUIWinAPIBrush.GetColor: TfpgColor;
begin
  if Assigned(Self) then
    Result:=FBrush.Color
  else
    Result:=0;
end;

procedure TFPGUIWinAPIBrush.SetColor(const AValue: TfpgColor);
begin
  FBrush.Color:=AValue;
end;

constructor TFPGUIWinAPIBrush.Create;
begin
  FBrush.Color:=TColorToTfpgColor(clBtnFace);
end;

constructor TFPGUIWinAPIBrush.Create(const ABrushData: TLogBrush);
begin
  FBrush.Color:=TColorToTfpgColor(ABrushData.lbColor);
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
end;

finalization
  FreeAndNil(FPGUIDesktopDC);

end.

