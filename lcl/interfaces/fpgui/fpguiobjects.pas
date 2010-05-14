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
    FPen: TPen;
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
    FFont: TFont;
    fpgFont: TfpgFont;
  public
    Constructor Create;
    Constructor Create(const AFontData: TLogFont);
    Constructor Create(const AFontData: TLogFont; const ALongFontName: string);
    Destructor Destroy; override;
    property fpguiFont: TfpgFont read fpgFont;
  end;

  { TFpGuiDeviceContext }

  TFPGUIDeviceContext = class(TFPGUIWinAPIElement)
  private
    FDCStack: array of TFPGUIDeviceContext;
    procedure CopyDCToInstance(const ATarget: TFPGUIDeviceContext);
    procedure FreeSelfObjects;
    procedure SetupFont;
    procedure SetupBrush;
    procedure SetupPen;
  public
    fpgCanvas: TfpgCanvas;
    FPrivateWidget: TFPGUIPrivateWidget;
    FOrg: TPoint;
    FBrush: TFPGUIWinAPIBrush;
    FPen: TFPGUIWinAPIPen;
    FFont: TFPGUIWinAPIFont;
    FTextColor: TfpgColor;
  public
    constructor Create(AFPGUIPrivate: TFPGUIPrivateWidget);
    destructor Destroy; override;
    procedure SetOrigin(const AX,AY: integer);
    function SaveDC: Boolean;
    function RestoreDC(const Index: SizeInt): Boolean;
    function SelectObject(const AGDIOBJ: HGDIOBJ): HGDIOBJ;
    function SetTextColor(const AColor: TColorRef): TColorRef;
    function PrepareRectOffsets(const ARect: TRect): TfpgRect;
    procedure ClearRectangle(const AfpgRect: TfpgRect);
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
  end;


implementation

{ TFpGuiDeviceContext }

procedure TFPGUIDeviceContext.CopyDCToInstance(
  const ATarget: TFPGUIDeviceContext);
begin
  ATarget.fpgCanvas:=fpgCanvas;
  ATarget.FPrivateWidget:=FPrivateWidget;
  ATarget.FBrush:=FBrush;
  ATarget.FPen.FPen.Assign(FPen.FPen);
  ATarget.FFont.FFont.Assign(FFont.FFont);
  ATarget.FOrg:=FOrg;
  ATarget.FTextColor:=FTextColor;
end;

procedure TFPGUIDeviceContext.FreeSelfObjects;
begin
  FreeAndNIL(FBrush);
  FreeAndNIL(FPen);
  FreeAndNIL(FFont);
end;

procedure TFPGUIDeviceContext.SetupFont;
begin
  fpgCanvas.Font:=FFont.fpguiFont;
end;

procedure TFPGUIDeviceContext.SetupBrush;
begin

end;

procedure TFPGUIDeviceContext.SetupPen;
begin
  fpgCanvas.Color:=FPen.Color;
end;

constructor TFpGuiDeviceContext.Create(AFPGUIPrivate: TFPGUIPrivateWidget);
begin
  if Assigned(AFPGUIPrivate) then begin
    fpgCanvas := AFPGUIPrivate.Widget.Canvas;
    AFPGUIPrivate.DC:=HDC(Self);
    FPrivateWidget := AFPGUIPrivate;
    with FOrg do begin
      X:=0;
      Y:=0;
    end;
    FBrush:=TFPGUIWinAPIBrush.Create;
    FPen:=TFPGUIWinAPIPen.Create;
    FFont:=TFPGUIWinAPIFont.Create;
    FBrush.Color:=TColorToTfpgColor(clBtnFace);
    FPen.FPen.Color:=clWindowText;
  end;
end;

destructor TFpGuiDeviceContext.Destroy;
begin
  FreeSelfObjects;
end;

procedure TFpGuiDeviceContext.SetOrigin(const AX, AY: integer);
begin
  With FOrg do begin
    X:=AX;
    Y:=AY;
  end;
end;

function TFPGUIDeviceContext.SaveDC: Boolean;
var
  Tmp: TFPGUIDeviceContext;
begin
  Beep;
  SetLength(FDCStack,Length(FDCStack)+1);
  Tmp:=TFPGUIDeviceContext.Create(FPrivateWidget);
  FDCStack[High(FDCStack)]:=Tmp;
  Self.CopyDCToInstance(Tmp);
  Result:=true;
end;

function TFPGUIDeviceContext.RestoreDC(const Index: SizeInt): Boolean;
var
  Tmp: TFPGUIDeviceContext;
  TargetIndex: SizeInt;
  j: SizeInt;
begin
  Beep;
  Result:=false;
  if Index>=0 then begin
    TargetIndex:=Index;
    if TargetIndex>High(FDCStack) then Exit;
  end else begin
    TargetIndex:=High(FDCStack)-Index+1;
    If TargetIndex<0 then Exit;
  end;
  Tmp:=FDCStack[TargetIndex];
  FreeSelfObjects;
  Tmp.CopyDCToInstance(Self);
  for j := TargetIndex to High(FDCStack) do begin
    FDCStack[j].Free;
  end;
  SetLength(FDCStack,TargetIndex);
  SetupFont;
  SetupBrush;
  SetupPen;
  Result:=true;
end;

function TFPGUIDeviceContext.SelectObject(const AGDIOBJ: HGDIOBJ): HGDIOBJ;
var
  gObject: TObject;
begin
  Result:=0;
  gObject:=TObject(AGDIOBJ);
  if gObject is TFPGUIWinAPIFont then begin
    Result:=HGDIOBJ(FFont);
    FFont:=TFPGUIWinAPIFont(gObject);
    SetupFont;
  end else if gObject is TFPGUIWinAPIBrush then begin
    Result:=HGDIOBJ(FBrush);
    FBrush:=TFPGUIWinAPIBrush(gObject);
    SetupBrush;
  end else if gObject is TFPGUIWinAPIPen then begin
    Result:=HGDIOBJ(FPen);
    FPen:=TFPGUIWinAPIPen(gObject);
    SetupPen;
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
  fpgCanvas.Color:=FBrush.Color;
  fpgCanvas.FillRectangle(AfpgRect);
  fpgCanvas.Color:=OldColor;
end;

{ TFPGUIPrivateMenuItem }

procedure TFPGUIPrivateMenuItem.HandleOnClick(ASender: TObject);
begin
  if Assigned(LCLMenuItem) and Assigned(LCLMenuItem.OnClick) then
   LCLMenuItem.OnClick(LCLMenuItem);
end;

{ TFPGUIWinAPIFont }

constructor TFPGUIWinAPIFont.Create;
begin
  FFont:=TFont.Create;
end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TLogFont);
begin
  Create;
  FFont.Name:=AFontData.lfFaceName;
  FFont.Height:=AFontData.lfHeight;
  fpgFont:=fpgGetFont(format('%s-%d',[FFont.Name,FFont.Size]));
end;

constructor TFPGUIWinAPIFont.Create(const AFontData: TLogFont;
  const ALongFontName: string);
begin
  Create;
  FFont.Name:=ALongFontName;
  FFont.Height:=AFontData.lfHeight;
  fpgFont:=fpgGetFont(format('%s-%d',[FFont.Name,FFont.Size]));
end;

destructor TFPGUIWinAPIFont.Destroy;
begin
  fpgFont.Free;
  FFont.Free;
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
  FPen:=TPen.Create;
end;

constructor TFPGUIWinAPIPen.Create(const APenData: TLogPen);
begin
  Create;
  FPen.Color:=APenData.lopnColor;
end;

destructor TFPGUIWinAPIPen.Destroy;
begin
  FPen.Free;
  inherited Destroy;
end;

{ TFPGUIWinAPIBrush }

function TFPGUIWinAPIBrush.GetColor: TfpgColor;
begin
  Result:=FBrush.Color;
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
  Create;
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
    eRegionCombineDiff,
    eRegionCombineOr,
    eRegionCombineXor:  begin
                          Raise Exception.CreateFmt('Region mode %d not supported',[integer(ACombineMode)]);
                        end;
  end;
end;

end.

