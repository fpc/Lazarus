{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
  Written by Martin Friebe
}
unit LazEditTextGridPainter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Types, Math,
  // LclBase
  Graphics, LCLType, LCLIntf, GraphUtil,
  // LazUtils
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy{$else} LazLoggerBase{$endif},
  LazClasses, LazEditTextAttributes;


type

  { TLazEditTextGridPainterFontInfo }

  TLazEditTextGridPainterFontInfo = class(TRefCountedObject)
  strict private const
    HIGH_ORD_FONTSTYLES = (2 << ord(High(TFontStyle)) ) - 1;
  strict private type
    TLazEditTextGridPainterFontStyleInfo = record
      Font: TFont;
      CharWidth, CharHeight: integer;
      NeedEto: boolean;
    end;
    PLazEditTextGridPainterFontStyleInfo = ^ TLazEditTextGridPainterFontStyleInfo;
    TLazEditTextGridPainterFontStyleInfos = array [0..HIGH_ORD_FONTSTYLES] of TLazEditTextGridPainterFontStyleInfo;
  strict private
    FFontStyleInfos: TLazEditTextGridPainterFontStyleInfos;
  private
    function GetFontHandle(AFontStyles: TFontStyles): HFONT;
    function GetCharWidth(AFontStyles: TFontStyles): integer;
    function GetCharHeight(AFontStyles: TFontStyles): integer;
    function GetNeedEto(AFontStyles: TFontStyles): Boolean;
    procedure InitWidthAndEto(AFontStyles: TFontStyles);
    procedure ClearFonts;
    {%H-}constructor CreateFor(AFont: TFont);
  public
    class function Create(AFont: TFont): TLazEditTextGridPainterFontInfo;
    destructor Destroy; override;
    function IsInfoFor(AFont: TFont): Boolean;

    property FontHandle[AFontStyles: TFontStyles]: HFONT   read GetFontHandle;
    property CharWidth [AFontStyles: TFontStyles]: integer read GetCharWidth;
    property CharHeight[AFontStyles: TFontStyles]: integer read GetCharHeight;
    property NeedEto   [AFontStyles: TFontStyles]: Boolean read GetNeedEto;
  end;

  { TEtoBuffer }

  TEtoBuffer = class
  public
    EtoData: Array of Integer;
    function  Eto: PInteger;
    function  Len: Integer;
    procedure Clear;
    procedure SetMinLength(ALen: Integer);
    procedure Fill(ALen, AVal: Integer);
  end;

  TFrameColors = array[TLazTextAttrBorderSide] of TColor;
  TFrameStyles = array[TLazTextAttrBorderSide] of TLazTextAttrLineStyle;

  { TLazEditTextDecorationPainter }

  TLazEditTextDecorationPainter = class
  strict private const
    MAX_PEN_CACHE = 9;
    WAVE_RAD = 3;
  strict private type
    PPenCache = ^TPenCache;
    PPPenCache = ^PPenCache;
    TPenCache = record
      Allocated: Boolean;
      Color: TColor;
      Style: TLazTextAttrLineStyle;
      Pen: HPen;
      Next: PPenCache;
    end;
  strict private
    FDC: HDC;

    FCachedPens: array[0..MAX_PEN_CACHE-1] of TPenCache;
    FCachedPenList: PPenCache;
    FCachedPenCount: integer;

    FHasSelectedPen: boolean;
    FOrigPen, FSelectedPen: HPen;

    function  GetPenFor(AColor: TColor; AStyle: TLazTextAttrLineStyle): HPen;
    procedure FreeHandles;
    procedure InternalDrawLine(X, Y, X2, Y2: Integer; AWave: Boolean); inline;
  public
    procedure BeginPaint(ADC: HDC);
    procedure EndPaint;
    procedure UnselectPen;

    procedure FillRect(const ARect: TRect; AColor: TColor);
    procedure DrawLine(X, Y, X2, Y2: Integer; AColor: TColor; AStyle: TLazTextAttrLineStyle); inline;
    procedure DrawFrame(X, Y, X2, Y2: Integer; const AColor: TFrameColors; const AStyle: TFrameStyles);
    procedure DrawFrame(const ARect: TRect; const AColor: TFrameColors; const AStyle: TFrameStyles; AFillColor: TColor); inline;
  end;

  TLazEditTextGridPainter = class
  strict private
    FInPaint: boolean;
    FDC: HDC;
    FSaveDCIndex: Integer;

    FHasSelectedFont: Boolean;
    FSelectedFontStyle: TFontStyles;
    FOrigFont: HGDIOBJ;

    FCurForeColor, FCurBackColor: TColor;
    FCurFontStyle: TFontStyles;

  private
    FMainCanvas: TCanvas;
    FFontInfo: TLazEditTextGridPainterFontInfo;
    FCharHeight, FCharWidth, FCharExtraWidth: integer;

    FEto: TEtoBuffer;
    FDecorator: TLazEditTextDecorationPainter;

    FBackColor: TColor;
    FForeColor: TColor;
    FFontStyle: TFontStyles;
    FFrameColors: TFrameColors;
    FFrameStyles: TFrameStyles;

    function GetCharExtra: Integer;
    function GetCharWidth: Integer;
    function GetEto: TEtoBuffer;
    function GetFrameColor(ASide: TLazTextAttrBorderSide): TColor;
    function GetFrameStyle(ASide: TLazTextAttrBorderSide): TLazTextAttrLineStyle;
    procedure SetCharExtra(AValue: Integer);
    procedure SetFrameColor(ASide: TLazTextAttrBorderSide; AValue: TColor);
    procedure SetFrameStyle(ASide: TLazTextAttrBorderSide; AValue: TLazTextAttrLineStyle);
  public
    constructor Create(ACanvas: TCanvas; AFont: TFont);
    destructor Destroy; override;

    procedure SetBaseFont(AFont: TFont);
    procedure AddBaseStyle(const AStyle: TFontStyles);

    procedure BeginPaint; inline;
    procedure EndPaint; inline;
    (* BeginCustomCanvas includes BeginPaint *)
    procedure BeginCustomCanvas(ACanvas: TCanvas);
    procedure EndCustomCanvas;

    procedure FillRect(const aRect: TRect);
    procedure DrawLine(X, Y, X2, Y2: Integer; AColor: TColor);
    procedure DrawFrame(const ARect: TRect);

    procedure ExtTextOut(X, Y: Integer; AnOptions: UINT; const ARect: TRect;
      AText: PChar; ALength: Integer); deprecated;
    procedure NewTextOut(X, Y: Integer; AnOptions: UINT; const ARect: TRect;
      AText: PChar; ALength: Integer; AnEto: TEtoBuffer);

    procedure ClearColors;
    property ForeColor: TColor read FForeColor write FForeColor;
    property BackColor: TColor read FBackColor write FBackColor;
    property Style: TFontStyles read FFontStyle write FFontStyle;

    procedure SetFrame(AColor: TColor; AStyle: TLazTextAttrLineStyle; AnEdges: TLazTextAttrFrameEdges = sfeAround);
    property FrameColor[ASide: TLazTextAttrBorderSide]: TColor read GetFrameColor write SetFrameColor;
    property FrameStyle[ASide: TLazTextAttrBorderSide]: TLazTextAttrLineStyle read GetFrameStyle write SetFrameStyle;

    function  NeedsEto: boolean; // for current style
    property Eto: TEtoBuffer read GetEto;

    property CharWidth:  Integer read GetCharWidth;
    property CharHeight: Integer read FCharHeight;
    property CharExtra:  Integer read GetCharExtra write SetCharExtra; // included in CharWidth
  end;

implementation

type

  { TLazEditTextGridPainterFontInfoList }

  TLazEditTextGridPainterFontInfoList = class(specialize TFPGObjectList<TLazEditTextGridPainterFontInfo>)
  public
    function FindFontInfo(AFont: TFont): TLazEditTextGridPainterFontInfo;
  end;

var
  DBG_FONTMETRIC: PLazLoggerLogGroup;
  DBG_FontName: string;
  LazEditTextGridPainterFontInfoList: TLazEditTextGridPainterFontInfoList;


function OrdFontStyles(AFontStyles: TFontStyles): Integer; inline;
begin
  Result := Integer(AFontStyles);
end;

procedure GetCharMetrics(DC: HDC; out AWidth, AHeight: Integer; out ANeedEto: Boolean);
  Procedure DebugFont(s: String; a: array of const); inline;
  begin
    DebugLn(DBG_FONTMETRIC, DBG_FontName + Format(s, a));
  end;

  procedure GetWHOForChar(s: char; out w, h ,o : Integer; var eto: Boolean);
  var
    s1, s2, s3: String;
    Size1, Size2, Size3: TSize;
    w2, w3: Integer;
  begin
    s1 := s;
    s2 := s1 + s;
    s3 := s2 + s;
    if not(GetTextExtentPoint(DC, PChar(s1), 1, Size1{%H-}) and
           GetTextExtentPoint(DC, PChar(s2), 2, Size2{%H-}) and
           GetTextExtentPoint(DC, PChar(s3), 3, Size3{%H-})) then
    begin
      DebugFont('Failed to get GetTextExtentPoint for %s', [s1]);
      w := 0;
      h := 0;
      o := 0;
      eto := True;
      exit;
    end;
    h := Size1.cy;
    // Size may contain overhang (italic, bold)
    // Size1 contains the size of 1 char + 1 overhang
    // Size2 contains the width of 2 chars, with only 1 overhang

    // Start simple
    w := size1.cx;
    o := 0;

    w2 := Size2.cx - Size1.cx;
    w3 := Size3.cx - Size2.cx;
    {$IFDEF LazEditDebugFont}
    DebugFont('Got TextExtends for %s=%d, %s=%d, %s=%d  Height=%d', [s1, Size1.cx, s2, Size2.cx, s3, Size3.cx, h]);
    {$ENDIF}
    if (w2 = w) and (w3 = w) then exit;

    if (w2 <= w) and (w3 <= w) then begin
      // w includes overhang (may be fractional
      DebugFont('Overhang w=%d w2=%d w3=%d', [w, w2, w3]);
      o := w - Min(w2, w3);
      w := w2;
      eto := True;
    end
    else
    if (w2 >= w) or (w3 >= w) then begin
      // Width may be fractional, check sanity and keep w
      o := 1;
      eto := True;
      if Max(w2, w3) > w + 1 then begin
        DebugFont('Size diff to bi for fractioanl (greater 1) w=%d w2=%d w3=%d', [w, w2, w3]);
        // Take a guess/average
        w2 := Max(w2, w3);
        o := w2 - w;
        w := Max(w, (w+w2-1) div 2);
      end;
    end
    else begin
      // broken font? one of w2/w3 is smaller, the other wider than w
      w := Max(w, (w+w2+w3-1) div 3);
      o := w div 2;
      eto := True;
    end;
    {$IFDEF LazEditDebugFont}
    DebugFont('Final result for %s  Width=%d  Overhang=%d  eto=%s', [s1, w, o, dbgs(eto)]);
    {$ENDIF}
  end;

  procedure AdjustWHOForChar(s: char; var w, h ,o : Integer; var eto: Boolean);
  var
    h2, w2, o2: Integer;
  begin
    GetWHOForChar(s, w2, h2, o2, eto);
    h := Max(h, h2);
    o := Max(o, o2);
    if w <> w2 then begin
      w := Max(w, w2);
      eto := True;
    end;
  end;

var
  TM: TTextMetric;
  OverHang: Integer;
  Size1: TSize;
  tmw: Integer;
begin
  // Calculate advance of a character.

  // TextMetric may fail, because:
  // tmMaxCharWidth may be the Awidth of a single AWidth (Latin) char, like "M"
  //                or a double AWidth (Chinese) char
  // tmAveCharWidth is to small for proprtional fonts, as we need he Awidth of the
  //                widest Latin char ("M").
  //                Even Monospace fonts, may have a smaller tmAveCharWidth (seen with Japanese)

  // take several samples
  ANeedEto := False;
  GetWHOForChar('M', AWidth, AHeight, OverHang, ANeedEto);
  AdjustWHOForChar('W', AWidth, AHeight, OverHang, ANeedEto);
  AdjustWHOForChar('@', AWidth, AHeight, OverHang, ANeedEto);
  AdjustWHOForChar('X', AWidth, AHeight, OverHang, ANeedEto);
  AdjustWHOForChar('m', AWidth, AHeight, OverHang, ANeedEto);
  // Small Chars to detect proportional fonts
  AdjustWHOForChar('i', AWidth, AHeight, OverHang, ANeedEto);
  AdjustWHOForChar(':', AWidth, AHeight, OverHang, ANeedEto);
  AdjustWHOForChar('''', AWidth, AHeight, OverHang, ANeedEto);

  // Negative Overhang ?
  if (not ANeedEto) and GetTextExtentPoint(DC, PChar('Ta'), 2, Size1{%H-}) then
    if Size1.cx < 2 * AWidth then begin
      DebugFont('Negative Overhang for "Ta" cx=%d  AWidth=%d Overhang=%d', [Size1.cx, AWidth, OverHang]);
      ANeedEto := True;
    end;

  // Make sure we get the correct AHeight
  if GetTextExtentPoint(DC, PChar('Tgq[_|^'), 7, Size1) then
    AHeight := Max(AHeight, Size1.cy);

  // DoubleCheck the result with GetTextMetrics
  GetTextMetrics(DC, TM{%H-});
  {$IFDEF LazEditDebugFont}
  DebugFont('TextMetrics tmHeight=%d, tmAve=%d, tmMax=%d, tmOver=%d', [TM.tmHeight, TM.tmAveCharWidth, TM.tmMaxCharWidth, TM.tmOverhang]);
  {$ENDIF}

  tmw := TM.tmMaxCharWidth + Max(TM.tmOverhang,0);
  if AWidth = 0 then begin
    DebugFont('No AWidth from GetTextExtentPoint', []);
    AWidth := tmw;
  end
  else if (AWidth > tmw) and (TM.tmMaxCharWidth > 0) then begin
    DebugFont('AWidth(%d) > tmMaxWidth+Over(%d)', [AWidth, tmw]);
    // take a guess, this is probably a broken font
    AWidth := Min(AWidth, round((TM.tmMaxCharWidth + Max(TM.tmOverhang,0)) * 1.2));
    ANeedEto := True;
  end;

  if AHeight = 0 then begin
    DebugFont('No AHeight from GetTextExtentPoint, tmHeight=%d', [TM.tmHeight]);
    AHeight := TM.tmHeight;
  end
  else if AHeight < TM.tmHeight then begin
    DebugFont('AHeight from GetTextExtentPoint to low AHeight=%d, tmHeight=%d', [AHeight, TM.tmHeight]);
    AHeight := TM.tmHeight;
  end;

  // If we have a broken font, make sure we return a positive value
  if AWidth <= 0 then begin
    DebugFont('SynTextDrawer: Fallback on AWidth', []);
    AWidth := 1 + AHeight * 8 div 10;
  end;
end;

{ TLazEditTextGridPainterFontInfoList }

function TLazEditTextGridPainterFontInfoList.FindFontInfo(AFont: TFont
  ): TLazEditTextGridPainterFontInfo;
var
  i: Integer;
  fnt: TFont;
begin
  Result := nil;
  if Self = nil then
    exit;

  fnt := AFont;
  if AFont.Style <> [] then begin
    fnt := TFont.Create;
    fnt.Assign(AFont);
    fnt.Style := [];
  end;

  for i := 0 to Count - 1 do begin
    Result := Items[i];
    if Result.IsInfoFor(fnt) then begin
      if fnt <> AFont then fnt.Destroy;
      exit;
    end;
  end;
  Result := nil;
  if fnt <> AFont then fnt.Destroy;
end;

{ TLazEditTextGridPainterFontInfo }

function TLazEditTextGridPainterFontInfo.GetFontHandle(AFontStyles: TFontStyles): HFONT;
var
  fnt: TFont;
begin
  fnt := FFontStyleInfos[OrdFontStyles(AFontStyles)].Font;
  if fnt = nil then begin
    assert(AFontStyles <> [], 'TLazEditTextGridPainterFontInfo.GetCharWidth: AFontStyles <> []');
    fnt := TFont.Create;
    fnt.Assign(FFontStyleInfos[0].Font);
    fnt.Style := AFontStyles;
    FFontStyleInfos[OrdFontStyles(AFontStyles)].Font := fnt;
  end;
  Result := fnt.Handle;
end;

function TLazEditTextGridPainterFontInfo.GetCharWidth(AFontStyles: TFontStyles): integer;
begin
  Result := FFontStyleInfos[OrdFontStyles(AFontStyles)].CharWidth;
  if Result <= 0 then begin
    InitWidthAndEto(AFontStyles);
    Result := FFontStyleInfos[OrdFontStyles(AFontStyles)].CharWidth;
  end;
end;

function TLazEditTextGridPainterFontInfo.GetCharHeight(AFontStyles: TFontStyles): integer;
begin
  Result := FFontStyleInfos[OrdFontStyles(AFontStyles)].CharHeight;
  if Result <= 0 then begin
    InitWidthAndEto(AFontStyles);
    Result := FFontStyleInfos[OrdFontStyles(AFontStyles)].CharHeight;
  end;
end;

function TLazEditTextGridPainterFontInfo.GetNeedEto(AFontStyles: TFontStyles): Boolean;
begin
  if FFontStyleInfos[OrdFontStyles(AFontStyles)].CharWidth <= 0 then
    InitWidthAndEto(AFontStyles);
  Result := FFontStyleInfos[OrdFontStyles(AFontStyles)].NeedEto;
end;

procedure TLazEditTextGridPainterFontInfo.InitWidthAndEto(AFontStyles: TFontStyles);
var
  DC: HDC;
  OldFntHndl: HGDIOBJ;
  info: PLazEditTextGridPainterFontStyleInfo;
begin
  DC := GetDC(0);
  OldFntHndl := SelectObject(DC, FontHandle[AFontStyles]);
  try
    info := @FFontStyleInfos[OrdFontStyles(AFontStyles)];
    DBG_FontName := 'Font=' + info^.Font.Name + ' Size=' + IntToStr(info^.Font.Size) + ' [' + dbgs(info^.Font.Style) + ']: ';    // for debugln
    GetCharMetrics(DC, info^.CharWidth, info^.CharHeight, info^.NeedEto);
    DBG_FontName := '';
  finally
    SelectObject(DC, OldFntHndl);
    ReleaseDC(0, DC);
  end;
end;

procedure TLazEditTextGridPainterFontInfo.ClearFonts;
var
  i: Integer;
begin
  for i := 1 to HIGH_ORD_FONTSTYLES do
    FreeAndNil(FFontStyleInfos[i].Font);
end;

constructor TLazEditTextGridPainterFontInfo.CreateFor(AFont: TFont);
var
  fnt: TFont;
begin
  inherited Create;

  fnt := TFont.Create;
  fnt.Assign(AFont);
  fnt.Style := [];
  FFontStyleInfos[0].Font := fnt;

  if LazEditTextGridPainterFontInfoList = nil then
    LazEditTextGridPainterFontInfoList := TLazEditTextGridPainterFontInfoList.Create(False);
  LazEditTextGridPainterFontInfoList.Add(Self);
end;

class function TLazEditTextGridPainterFontInfo.Create(AFont: TFont
  ): TLazEditTextGridPainterFontInfo;
begin
  if LazEditTextGridPainterFontInfoList <> nil then begin
    Result := LazEditTextGridPainterFontInfoList.FindFontInfo(AFont);
    if Result <> nil then
      exit;
  end;
  Result := CreateFor(AFont);
end;

destructor TLazEditTextGridPainterFontInfo.Destroy;
begin
  inherited Destroy;

  if LazEditTextGridPainterFontInfoList <> nil then begin
    LazEditTextGridPainterFontInfoList.Remove(Self);
    if LazEditTextGridPainterFontInfoList.Count = 0 then
      FreeAndNil(LazEditTextGridPainterFontInfoList);
  end;

  ClearFonts;
  FreeAndNil(FFontStyleInfos[0].Font);
end;

function TLazEditTextGridPainterFontInfo.IsInfoFor(AFont: TFont): Boolean;
begin
  Result := AFont.IsEqual(FFontStyleInfos[0].Font);
end;

{ TEtoBuffer }

function TEtoBuffer.Eto: PInteger;
begin
  if Self = nil then
    exit(nil);

  if Length(EtoData) > 0 then
    Result := PInteger(@EtoData[0])
  else
    Result := nil;
end;

function TEtoBuffer.Len: Integer;
begin
  Result := Length(EtoData);
end;

procedure TEtoBuffer.Clear;
begin
  SetLength(EtoData, 0);
end;

procedure TEtoBuffer.SetMinLength(ALen: Integer);
const
  EtoBlockSize = $80;
begin
  if Length(EtoData) >= ALen then exit;
  SetLength(EtoData, ((not (EtoBlockSize - 1)) and ALen) + EtoBlockSize);
end;

procedure TEtoBuffer.Fill(ALen, AVal: Integer);
var
  i: Integer;
begin
  SetMinLength(ALen);
  for i := 0 to ALen - 1 do
    EtoData[i] := AVal;
end;

{ TLazEditTextDecorationPainter }

function TLazEditTextDecorationPainter.GetPenFor(AColor: TColor; AStyle: TLazTextAttrLineStyle
  ): HPen;
var
  CachePtr, PrevCachePtr: PPenCache;
  CachePtrPtr, PrevCachePtrPtr: PPPenCache;
  LogBrush: TLogBrush;
  s: DWord;
begin
  AColor := ColorToRGB(AColor);
  if AStyle = slsWaved then
    AStyle := slsSolid;

  CachePtr    := FCachedPenList;
  CachePtrPtr := nil;
  PrevCachePtr    := nil;
  PrevCachePtrPtr := nil;
  while CachePtr <> nil do begin
    if (CachePtr^.Color = AColor) and (CachePtr^.Style = AStyle) then begin
      if CachePtrPtr <> nil then begin
        // move to first in list
        CachePtrPtr^ := CachePtr^.Next;
        CachePtr^.Next := FCachedPenList;
        FCachedPenList := CachePtr;
      end;
      Result := CachePtr^.Pen;
      exit;
    end;
    PrevCachePtr    := CachePtr;
    PrevCachePtrPtr := CachePtrPtr;
    CachePtrPtr := @CachePtr^.Next;
    CachePtr    := CachePtr^.Next;
  end;


  case AStyle of
    slsSolid:  s := PS_SOLID + PS_GEOMETRIC + PS_ENDCAP_FLAT + PS_JOIN_MITER;
    slsDashed: s := PS_DASH  + PS_GEOMETRIC + PS_ENDCAP_FLAT + PS_JOIN_MITER;
    slsDotted: s := PS_DOT   + PS_GEOMETRIC + PS_ENDCAP_FLAT + PS_JOIN_MITER;
  end;

  LogBrush := Default(TLogBrush);
  LogBrush.lbStyle := BS_SOLID;
  LogBrush.lbColor := ColorToRGB(AColor);

  Result := ExtCreatePen(s, 1, LogBrush, 0, nil);

  if FCachedPenCount < MAX_PEN_CACHE then begin
    PrevCachePtr := @FCachedPens[FCachedPenCount];
    inc(FCachedPenCount);
  end
  else begin
    PrevCachePtrPtr^ := nil;
    DeleteObject(PrevCachePtr^.Pen);
  end;

  PrevCachePtr^.Pen       := Result;
  PrevCachePtr^.Allocated := True;
  PrevCachePtr^.Color     := AColor;
  PrevCachePtr^.Style     := AStyle;
  PrevCachePtr^.Next      := FCachedPenList;
  FCachedPenList          := PrevCachePtr;
end;

procedure TLazEditTextDecorationPainter.FreeHandles;
var
  i: Integer;
begin
  UnselectPen;
  for i := 0 to MAX_PEN_CACHE-1 do
    if FCachedPens[i].Allocated then begin
      DeleteObject(FCachedPens[i].Pen);
      FCachedPens[i].Allocated := False;
    end;
  FCachedPenList := nil;
  FCachedPenCount := 0;
end;

procedure TLazEditTextDecorationPainter.InternalDrawLine(X, Y, X2, Y2: Integer; AWave: Boolean);
var
  dummy: TPoint;
begin
  MoveToEx(FDC, X, Y, @dummy);
  if AWave then
    WaveTo(FDC, x2, Y2, WAVE_RAD)
  else
    LineTo(FDC, X2, Y2);
end;

procedure TLazEditTextDecorationPainter.BeginPaint(ADC: HDC);
begin
  assert(FDC = 0, 'TLazEditTextDecorationPainter.BeginPaint: FDC = 0');
  FDC := ADc;
end;

procedure TLazEditTextDecorationPainter.EndPaint;
begin
  FreeHandles;
  FDC := 0;
end;

procedure TLazEditTextDecorationPainter.UnselectPen;
begin
  if not FHasSelectedPen then
    exit;

  SelectObject(FDC, FOrigPen);
  FHasSelectedPen := False;
end;

procedure TLazEditTextDecorationPainter.FillRect(const ARect: TRect; AColor: TColor);
begin
  if AColor = clNone then
    exit;
  //LCLIntf.FillRect(FDC, aRect, brush);
// TODO: Current BK Color
  if AColor <> clDefault then  // color already set
    SetBkColor(FDC, ColorToRGB(AColor));
  ExtTextOut(FDC, 0, 0, ETO_OPAQUE, @aRect, nil, 0, nil);
end;

procedure TLazEditTextDecorationPainter.DrawLine(X, Y, X2, Y2: Integer; AColor: TColor;
  AStyle: TLazTextAttrLineStyle);
var
  p: HPEN;
begin
  p := GetPenFor(AColor, AStyle);
  if FHasSelectedPen then begin
    if p <> FSelectedPen then
      SelectObject(FDC, p);
  end
  else begin
    FOrigPen := SelectObject(FDC, p);
    FHasSelectedPen := True;
  end;

  InternalDrawLine(X, Y, X2, Y2, AStyle = slsWaved);
end;

procedure TLazEditTextDecorationPainter.DrawFrame(X, Y, X2, Y2: Integer;
  const AColor: TFrameColors; const AStyle: TFrameStyles);
var
  Todo: TLazTextAttrBorderSides;
  col: TColor;
  st, st2: TLazTextAttrLineStyle;
  wav: Boolean;
  o: Integer;
begin
  Todo := [bsLeft, bsTop, bsRight, bsBottom];

  while Todo <> [] do begin
    col := clNone;

    if (bsLeft in Todo) then begin
      Exclude(Todo, bsLeft);
      if col = clNone then begin
        col := AColor[bsLeft];
        if col <> clNone then begin
          st  := AStyle[bsLeft];
          st2 := st;
          wav := st2 = slsWaved;
          if wav then st2 := slsSolid;
          DrawLine(X, Y, X, Y2, col, st);
        end;
      end;
    end;

    if (bsTop in Todo) then begin
      if col = clNone then begin
        Exclude(Todo, bsTop);
        col := AColor[bsTop];
        if col <> clNone then begin
          st  := AStyle[bsTop];
          st2 := st;
          wav := st2 = slsWaved;
          if wav then st2 := slsSolid;
          DrawLine(X, Y, X2, Y, col, st);
        end;
      end
      else
      if (AColor[bsTop] = col) and
         ( (AStyle[bsTop] = st2) or
           ( wav and (AStyle[bsTop] = slsWaved) )
         )
      then begin
        Exclude(Todo, bsTop);
        InternalDrawLine(X, Y, X2, Y, AStyle[bsTop] = slsWaved);
      end;
    end;

    if (bsRight in Todo) then begin
      if col = clNone then begin
        Exclude(Todo, bsRight);
        col := AColor[bsRight];
        if col <> clNone then begin
          st  := AStyle[bsRight];
          st2 := st;
          wav := st2 = slsWaved;
          if wav then st2 := slsSolid;
          o := 1;
          if wav then o := WAVE_RAD;
          DrawLine(X2 - o, Y, X2 - o, Y2, col, st);
        end;
      end
      else
      if (AColor[bsRight] = col) and
         ( (AStyle[bsRight] = st2) or
           ( wav and (AStyle[bsRight] = slsWaved) )
         )
      then begin
        Exclude(Todo, bsRight);
        o := 1;
        if AStyle[bsRight] = slsWaved then o := WAVE_RAD;
        InternalDrawLine(X2 - o, Y, X2 - o, Y2, AStyle[bsRight] = slsWaved);
      end;
    end;

    if (bsBottom in Todo) then begin
      if col = clNone then begin
        Exclude(Todo, bsBottom);
        col := AColor[bsBottom];
        if col <> clNone then begin
          st  := AStyle[bsBottom];
          st2 := st;
          wav := st2 = slsWaved;
          if wav then st2 := slsSolid;
          o := 1;
          if wav then o := WAVE_RAD;
          DrawLine(X, Y2 - o, X2, Y2 - o, col, st);
        end;
      end
      else
      if (AColor[bsBottom] = col) and
         ( (AStyle[bsBottom] = st2) or
           ( wav and (AStyle[bsBottom] = slsWaved) )
         )
      then begin
        Exclude(Todo, bsBottom);
        o := 1;
        if AStyle[bsBottom] = slsWaved then o := WAVE_RAD;
        InternalDrawLine(X, Y2 - o, X2, Y2 - o, AStyle[bsBottom] = slsWaved);
      end;
    end;
  end;
end;

procedure TLazEditTextDecorationPainter.DrawFrame(const ARect: TRect; const AColor: TFrameColors;
  const AStyle: TFrameStyles; AFillColor: TColor);
begin
  FillRect(ARect, AFillColor);
  DrawFrame(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, AColor, AStyle);
end;

{ TLazEditTextGridPainter }

function TLazEditTextGridPainter.GetCharExtra: Integer;
begin
  Result := Max(FCharExtraWidth, -FCharWidth + 1);
end;

function TLazEditTextGridPainter.GetCharWidth: Integer;
begin
  Result := Max(1, FCharWidth + FCharExtraWidth);
end;

function TLazEditTextGridPainter.GetEto: TEtoBuffer;
begin
  if FEto = nil then
    FEto := TEtoBuffer.Create;
  Result := FEto;
end;

function TLazEditTextGridPainter.GetFrameColor(ASide: TLazTextAttrBorderSide): TColor;
begin
  Result := FFrameColors[ASide];
end;

function TLazEditTextGridPainter.GetFrameStyle(ASide: TLazTextAttrBorderSide
  ): TLazTextAttrLineStyle;
begin
  Result := FFrameStyles[ASide];
end;

procedure TLazEditTextGridPainter.SetCharExtra(AValue: Integer);
begin
  FCharExtraWidth := AValue;
end;

procedure TLazEditTextGridPainter.SetFrameColor(ASide: TLazTextAttrBorderSide; AValue: TColor);
begin
  FFrameColors[ASide] := AValue;
end;

procedure TLazEditTextGridPainter.SetFrameStyle(ASide: TLazTextAttrBorderSide;
  AValue: TLazTextAttrLineStyle);
begin
  FFrameStyles[ASide] := AValue;
end;

constructor TLazEditTextGridPainter.Create(ACanvas: TCanvas; AFont: TFont);
begin
  inherited Create;
  FMainCanvas := ACanvas;
  SetBaseFont(AFont); // TODO: don't alloc the font, synedit send a dummy at first
  FDecorator := TLazEditTextDecorationPainter.Create;
  ClearColors;
end;

destructor TLazEditTextGridPainter.Destroy;
begin
  inherited Destroy;
  FEto.Free;
  FDecorator.Free;
  FFontInfo.ReleaseReference;
end;

procedure TLazEditTextGridPainter.SetBaseFont(AFont: TFont);
begin
  FFontInfo.ReleaseReference;
  FFontInfo := LazEditTextGridPainterFontInfoList.FindFontInfo(AFont);
  if FFontInfo = nil then
    FFontInfo := TLazEditTextGridPainterFontInfo.Create(AFont);
  FFontInfo.AddReference;

  FCharWidth  := FFontInfo.CharWidth[AFont.Style];
  FCharHeight := FFontInfo.CharHeight[AFont.Style];
end;

procedure TLazEditTextGridPainter.AddBaseStyle(const AStyle: TFontStyles);
begin
  assert(FFontInfo<>nil, 'TLazEditTextGridPainter.AddBaseStyle: FFontInfo<>nil');
  FCharWidth  := max(FCharWidth, FFontInfo.CharWidth[AStyle]);
  FCharHeight := max(FCharHeight, FFontInfo.CharHeight[AStyle]);
end;

procedure TLazEditTextGridPainter.BeginPaint;
begin
  BeginCustomCanvas(FMainCanvas);
end;

procedure TLazEditTextGridPainter.EndPaint;
begin
  EndCustomCanvas;
  FDC := 0;
end;

procedure TLazEditTextGridPainter.BeginCustomCanvas(ACanvas: TCanvas);
begin
  assert(FDC = 0, 'TLazEditTextGridPainter.BeginCustomCanvas: FDC = 0');
  assert(not FInPaint, 'TLazEditTextGridPainter.BeginCustomCanvas: not FInPaint');
  FInPaint := True;
  ClearColors;
  FHasSelectedFont := False;
  FCurForeColor := clNone;
  FCurBackColor := clNone;
  FDC := ACanvas.Handle;
  FSaveDCIndex := SaveDC(FDC);
  FDecorator.BeginPaint(FDC);
  LCLIntf.SetBkMode(FDC, TRANSPARENT);
end;

procedure TLazEditTextGridPainter.EndCustomCanvas;
begin
  assert(FInPaint, 'TLazEditTextGridPainter.EndCustomCanvas: FInPaint');
  FInPaint := False;
  FDecorator.EndPaint;
  RestoreDC(FDC, FSaveDCIndex);
  FDC := 0;
end;

procedure TLazEditTextGridPainter.FillRect(const aRect: TRect);
begin
  if FCurBackColor <> FBackColor then begin
    FCurBackColor := FBackColor;
    LCLIntf.SetBkColor(FDC, ColorToRGB(FCurBackColor));
  end;
  FDecorator.FillRect(aRect, clDefault);
end;

procedure TLazEditTextGridPainter.DrawLine(X, Y, X2, Y2: Integer; AColor: TColor);
begin
  FDecorator.DrawLine(X, Y, X2, Y2, AColor, slsSolid);
end;

procedure TLazEditTextGridPainter.DrawFrame(const ARect: TRect);
begin
  FDecorator.DrawFrame(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, FFrameColors, FFrameStyles);
end;

procedure TLazEditTextGridPainter.ExtTextOut(X, Y: Integer; AnOptions: UINT; const ARect: TRect;
  AText: PChar; ALength: Integer);

  function HasFrame: Boolean;
  var
    Side: TLazTextAttrBorderSide;
  begin
    for Side := Low(TLazTextAttrBorderSide) to High(TLazTextAttrBorderSide) do
      if FFrameColors[Side] <> clNone then
        Exit(True);
    Result := False;
  end;

var
  e: TEtoBuffer;
begin
  if HasFrame then // draw background // TODO: only if not default bg color
  begin
    if FCurBackColor <> FBackColor then begin
      FCurBackColor := FBackColor;
      LCLIntf.SetBkColor(FDC, ColorToRGB(FCurBackColor));
    end;
    FDecorator.DrawFrame(ARect, FFrameColors, FFrameStyles, clDefault);
    AnOptions := 0;
  end;

  e := nil;
  if NeedsEto then begin
    e := Eto;
    e.Fill(ALength, CharWidth);
  end;

  NewTextOut(X, Y, AnOptions, ARect, AText, ALength, e);
end;

procedure TLazEditTextGridPainter.NewTextOut(X, Y: Integer; AnOptions: UINT; const ARect: TRect;
  AText: PChar; ALength: Integer; AnEto: TEtoBuffer);
  {$IFDEF WINDOWS_LIGATURE}
var
  W: WideString;
  Glyphs: array of WideChar;
  CharPlaceInfo: GCP_RESULTSW;
  {$ENDIF}
begin
  //if FCurBackColor <> FBackColor then begin
  if true then begin  // Textarea still changes bk color too
    FCurBackColor := FBackColor;
    LCLIntf.SetBkColor(FDC, ColorToRGB(FCurBackColor));
  end;

  if AText <> nil then begin
    if FCurForeColor <> FForeColor then begin
      FCurForeColor := FForeColor;
      LCLIntf.SetTextColor(FDC, ColorToRGB(FCurForeColor));
    end;

    if FHasSelectedFont then begin
      if FCurFontStyle <> FFontStyle then begin
        SelectObject(FDC, FFontInfo.FontHandle[FFontStyle]);
        FCurFontStyle := FFontStyle;
      end;
    end
    else begin
      FHasSelectedFont := True;
      FOrigFont := SelectObject(FDC, FFontInfo.FontHandle[FFontStyle]);
      FCurFontStyle := FFontStyle;
    end;
  end;

  {$IFDEF WINDOWS_LIGATURE}
  if ALength > 0 then begin
    W := UTF8ToUTF16(Text, ALength);
    ZeroMemory(@CharPlaceInfo, SizeOf(CharPlaceInfo));
    CharPlaceInfo.lStructSize:= SizeOf(CharPlaceInfo);
    SetLength(Glyphs, Length(W));
    CharPlaceInfo.lpGlyphs:= @Glyphs[0];
    CharPlaceInfo.nGlyphs:= Length(Glyphs);
    Glyphs[0] := #0;
    if GetCharacterPlacementW(FDC, PWChar(W), Length(W), 0, CharPlaceInfo, GCP_LIGATE or GCP_REORDER or GCP_GLYPHSHAPE)<> 0 then begin
      Windows.ExtTextOutW(FDC, X, Y, fuOptions or ETO_GLYPH_INDEX, @ARect, Pointer(Glyphs), CharPlaceInfo.nGlyphs, AnEto.Eto);
      exit;
    end;
  end;
  {$ENDIF}

  LCLIntf.ExtUTF8Out(FDC, X, Y, AnOptions, @ARect, AText, ALength, AnEto.Eto);
end;

procedure TLazEditTextGridPainter.ClearColors;
var
  i: TLazTextAttrBorderSide;
begin
  FBackColor := clNone;
  FForeColor := clNone;
  FFontStyle := [];
  for i := low(TLazTextAttrBorderSide) to High(TLazTextAttrBorderSide) do begin
    FFrameColors[i] := clNone;
    FFrameStyles[i] := slsSolid;
  end;
end;

procedure TLazEditTextGridPainter.SetFrame(AColor: TColor; AStyle: TLazTextAttrLineStyle;
  AnEdges: TLazTextAttrFrameEdges);
var
  i: TLazTextAttrBorderSide;
  sides: TLazTextAttrBorderSides;
begin
  sides := LazTextFrameEdgeToSides[AnEdges];
  for i := low(TLazTextAttrBorderSide) to High(TLazTextAttrBorderSide) do begin
    if i in sides then begin
      FFrameColors[i] := AColor;
      FFrameStyles[i] := AStyle;
    end
    else begin
      FFrameStyles[i] := AStyle;
      FFrameStyles[i] := slsSolid;
    end;
  end;
end;

function TLazEditTextGridPainter.NeedsEto: boolean;
begin
  Result := (FCharExtraWidth <> 0) or
            (FCharWidth <> FFontInfo.CharWidth[FFontStyle]) or
            FFontInfo.NeedEto[FFontStyle];
end;

initialization
  DBG_FONTMETRIC := DebugLogger.RegisterLogGroup('LazEditDebugFont' {$IFDEF LazEditDebugFont} , True {$ENDIF} );

finalization
  FreeAndNil(LazEditTextGridPainterFontInfoList);
end.

