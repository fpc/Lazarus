unit EasyLazFreeType;

{ bug list :

- Characters parts may not be well translated, for example i with accent.
- Encoding is ok for ASCII but is mixed up for extended characters

to do :

- multiple font loading
- font face cache
- font style
- text rotation }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFreeType, AvgLvlTree, fpimage, Types, lazutf8; // Graphics, LCLType

type
  TGlyphRenderQuality = (grqMonochrome, grqLowQuality, grqHighQuality);
  ArrayOfSingle= array of single;
  TFreeTypeGlyph = class;

  { TFreeTypeRenderableFont }

  TFreeTypeRenderableFont = class
  protected
    function GetClearType: boolean; virtual; abstract;
    procedure SetClearType(const AValue: boolean); virtual; abstract;
  public
    procedure RenderText(AText: string; x,y: single; ARect: TRect; OnRender : TDirectRenderingFunction); virtual; abstract;
    property ClearType: boolean read GetClearType write SetClearType;
  end;

  { TFreeTypeDrawer }

  TFreeTypeDrawer = class
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor; AOpactiy: Byte); virtual; abstract; overload;
    procedure DrawText(AText: string; AFont: TFreeTypeRenderableFont; x,y: single; AColor: TFPColor); overload;
  end;

  { TFreeTypeFont }

  TFreeTypeFont = class(TFreeTypeRenderableFont)
  private
    FName: String;
    FPointSize: single;
    FHinted: boolean;
    FWidthFactor: single;
    FClearType: boolean;
    function FindGlyphNode(Index: Integer): TAvgLvlTreeNode;
    function GetCharIndex(AChar: integer): integer;
    function GetDPI: integer;
    function GetGlyph(Index: integer): TFreeTypeGlyph;
    function GetGlyphCount: integer;
    function GetPixelSize: single;
    procedure SetDPI(const AValue: integer);
    procedure SetHinted(const AValue: boolean);
    procedure SetName(const AValue: String);
    procedure DiscardFace;
    procedure DiscardInstance;
    procedure SetPixelSize(const AValue: single);
    procedure SetPointSize(const AValue: single);
    function LoadGlyphInto(_glyph      : TT_Glyph;
                            glyph_index : Word): boolean;
    procedure SetWidthFactor(const AValue: single);
    procedure UpdateSizeInPoints;
    procedure GetCharmap;
  protected
    FFace: TT_Face;
    FFaceLoaded: boolean;
    FInstance: TT_Instance;
    FInstanceCreated : boolean;
    FGlyphTable: TAvgLvlTree;
    FCharMap: TT_CharMap;
    FCharmapOk: boolean;
    function GetClearType: boolean; override;
    procedure SetClearType(const AValue: boolean); override;
  public
    Quality : TGlyphRenderQuality;
    constructor Create;
    destructor Destroy; override;
    procedure RenderText(AText: string; x,y: single; ARect: TRect; OnRender : TDirectRenderingFunction); override;
    function TextWidth(AText: string): single;
    function CharsWidth(AText: string): ArrayOfSingle;
    property Name: String read FName write SetName;
    property DPI: integer read GetDPI write SetDPI;
    property SizeInPoints: single read FPointSize write SetPointSize;
    property SizeInPixels: single read GetPixelSize write SetPixelSize;
    property Glyph[Index: integer]: TFreeTypeGlyph read GetGlyph;
    property GlyphCount: integer read GetGlyphCount;
    property CharIndex[AChar: integer]: integer read GetCharIndex;
    property Hinted: boolean read FHinted write SetHinted;
    property WidthFactor: single read FWidthFactor write SetWidthFactor;
  end;

  { TFreeTypeGlyph }

  TFreeTypeGlyph = class
  private
    FLoaded: boolean;
    FGlyphData: TT_Glyph;
    FIndex: integer;
    function GetAdvance: single;
    function GetBounds: TRect;
    function GetBoundsWithOffset(x, y: single): TRect;
  public
    constructor Create(AFont: TFreeTypeFont; AIndex: integer);
    function RenderDirectly(x,y: single; Rect: TRect; OnRender : TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean = false): boolean;
    destructor Destroy; override;
    property Loaded: boolean read FLoaded;
    property Data: TT_Glyph read FGlyphData;
    property Index: integer read FIndex;
    property Bounds: TRect read GetBounds;
    property BoundsWithOffset[x,y: single]: TRect read GetBoundsWithOffset;
    property Advance: single read GetAdvance;
  end;

  { TFreeTypeRasterMap }

  TFreeTypeRasterMap = class
  protected
    map: TT_Raster_Map;
    function GetHeight: integer; virtual;
    function GetWidth: integer; virtual;
    function GetScanLine(y: integer): pointer;
  public
    constructor Create(AWidth,AHeight: integer); virtual; abstract;
    procedure Clear;
    procedure Fill;
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; virtual; abstract;
    procedure ScanMoveTo(x,y: integer); virtual; abstract;
    destructor Destroy; override;

    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property ScanLine[y: integer]: pointer read GetScanLine;
  end;

  { TFreeTypeMonochromeMap }

  TFreeTypeMonochromeMap = class(TFreeTypeRasterMap)
  private
    ScanPtrStart,ScanPtrCur: pbyte;
    ScanBit: byte;
    ScanX: integer;
    function GetPixelsInHorizlineNoBoundsChecking(x,y,x2: integer) : integer; inline;
  public
    constructor Create(AWidth,AHeight: integer); override;
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; override;
    procedure ScanMoveTo(x,y: integer); override;
    function ScanNextPixel: boolean;
    function GetPixel(x,y: integer): boolean;
    procedure SetPixel(x,y: integer; value: boolean);
    function GetPixelsInRect(x,y,x2,y2: integer): integer;
    function GetPixelsInHorizline(x,y,x2: integer): integer;
    procedure TogglePixel(x,y: integer);
  end;

  { TFreeTypeGrayscaleMap }

  TFreeTypeGrayscaleMap = class(TFreeTypeRasterMap)
  private
    ScanPtrStart: pbyte;
    ScanX: integer;
  public
    RenderQuality: TGlyphRenderQuality;
    constructor Create(AWidth,AHeight: integer); override;
    function RenderGlyph(glyph : TFreeTypeGlyph; x,y: single) : boolean; override;
    procedure ScanMoveTo(x,y: integer); override;
    function ScanNextPixel: byte;
    function GetPixel(x,y: integer): byte;
    procedure SetPixel(x,y: integer; value: byte);
    procedure XorPixel(x,y: integer; value: byte);
  end;


implementation

uses TTRaster;//, LCLIntf, LCLProc;

var
  BitCountTable: packed array[0..255] of byte;
  RegularGray5: TT_Gray_Palette;
  FreeTypeInitialized,FreeTypeCannotInitialize : boolean;

procedure EnsureFreeTypeInitialized;
begin
  if not FreeTypeInitialized and not FreeTypeCannotInitialize then
  begin
    FreeTypeInitialized := (TT_Init_FreeType = TT_Err_Ok);
    FreeTypeCannotInitialize := not FreeTypeInitialized;
  end;
  if FreeTypeCannotInitialize then
    raise Exception.Create('FreeType cannot be initialized');
end;

{ TFreeTypeDrawer }

procedure TFreeTypeDrawer.DrawText(AText: string;
  AFont: TFreeTypeRenderableFont; x, y: single; AColor: TFPColor);
begin
  DrawText(AText, AFont, x,y, AColor, 255);
end;

{ TFreeTypeGlyph }

{$hints off}
function TFreeTypeGlyph.GetBounds: TRect;
var metrics: TT_Glyph_Metrics;
begin
  TT_Get_Glyph_Metrics(FGlyphData, metrics);
  with metrics.bbox do
    result := rect(IncludeFullGrainMin(xMin,64) div 64,IncludeFullGrainMin(-yMax,64) div 64,
       (IncludeFullGrainMax(xMax,64)+1) div 64,(IncludeFullGrainMax(-yMin,64)+1) div 64);
end;
{$hints on}

{$hints off}
function TFreeTypeGlyph.GetAdvance: single;
var metrics: TT_Glyph_Metrics;
begin
  TT_Get_Glyph_Metrics(FGlyphData, metrics);
  result := metrics.advance/64;
end;
{$hints on}

{$hints off}
function TFreeTypeGlyph.GetBoundsWithOffset(x, y: single): TRect;
var metrics: TT_Glyph_Metrics;
begin
  TT_Get_Glyph_Metrics(FGlyphData, metrics);
  with metrics.bbox do
    result := rect(IncludeFullGrainMin(xMin+round(x*64),64) div 64,IncludeFullGrainMin(-yMax+round(y*64),64) div 64,
       (IncludeFullGrainMax(xMax+round(x*64),64)+1) div 64,(IncludeFullGrainMax(-yMin+round(y*64),64)+1) div 64);
end;
{$hints on}

constructor TFreeTypeGlyph.Create(AFont: TFreeTypeFont; AIndex: integer);
begin
  if TT_New_Glyph(AFont.FFace, FGlyphData) <> TT_Err_Ok then
    raise Exception.Create('Cannot create empty glyph');
  FLoaded := AFont.LoadGlyphInto(FGlyphData, AIndex);
  FIndex := AIndex;
end;

function TFreeTypeGlyph.RenderDirectly(x, y: single; Rect: TRect;
  OnRender: TDirectRenderingFunction; quality : TGlyphRenderQuality; ClearType: boolean): boolean;
var mono: TFreeTypeMonochromeMap;
    tx,xb,yb: integer;
    pdest: pbyte;
    buf: pointer;
    glyphBounds: TRect;
begin
  if ClearType then
  begin
    Rect.Left *= 3;
    Rect.Right *= 3;
    x *= 3;
  end;

  glyphBounds := BoundsWithOffset[x,y];

  if ClearType then
  begin
    InflateRect(glyphBounds,1,0);
    glyphBounds.Left := IncludeFullGrainMin( glyphBounds.Left, 3);
    glyphBounds.Right := IncludeFullGrainMax( glyphBounds.Right-1, 3) + 1;
  end;
  if not IntersectRect(Rect,Rect,glyphBounds) then exit;

  case quality of
    grqMonochrome: begin
                      tx := rect.right-rect.left;
                      mono := TFreeTypeMonochromeMap.Create(tx,rect.bottom-rect.top);
                      result := mono.RenderGlyph(self,x-rect.left,y-rect.top);
                      if result then
                      begin
                        getmem(buf, tx);
                        for yb := mono.Height-1 downto 0 do
                        begin
                          mono.ScanMoveTo(0,yb);
                          pdest := pbyte(buf);
                          for xb := tx-1 downto 0 do
                          begin
                            if mono.ScanNextPixel then
                              pdest^ := $ff
                            else
                              pdest^ := 0;
                            inc(pdest);
                          end;
                          OnRender(rect.Left,rect.top+yb,tx,buf);
                        end;
                        freemem(buf);
                      end;
                      mono.Free;
                   end;
    grqLowQuality: begin
                     TT_Set_Raster_Palette(RegularGray5);
                     result := TT_Render_Directly_Glyph_Gray(FGlyphData, round((x-rect.left)*64), round((rect.bottom-y)*64), rect.left,rect.top,rect.right-rect.left,rect.bottom-rect.top, OnRender) = TT_Err_Ok;
                   end;
    grqHighQuality: result := TT_Render_Directly_Glyph_HQ(FGlyphData, round((x-rect.left)*64), round((rect.bottom-y)*64), rect.left,rect.top,rect.right-rect.left,rect.bottom-rect.top, OnRender) = TT_Err_Ok;
  else
    result := false;
  end;
end;

destructor TFreeTypeGlyph.Destroy;
begin
  TT_Done_Glyph(FGlyphData);
  inherited Destroy;
end;

{ TFreeTypeFont }

procedure TFreeTypeFont.SetName(const AValue: String);
var errorNum: TT_Error;
    PrevDPI: integer;
begin
  if FName=AValue then exit;
  PrevDPI := DPI;
  DiscardInstance;
  DiscardFace;
  errorNum := TT_Open_Face(AValue,FFace);
  if errorNum = TT_Err_Ok then
  begin
    FFaceLoaded:= true;
    FName:=AValue;

    GetCharmap;

    errorNum := TT_New_Instance(FFace, FInstance);
    if errorNum = TT_Err_Ok then
    begin
      FInstanceCreated := true;
      DPI := PrevDPI;
    end else
      raise exception.Create('Cannot create font instance (TT_Error ' + intToStr(errorNum)+')');
  end else
    raise exception.Create('Cannot open font (TT_Error ' + intToStr(errorNum)+')');
end;

{$hints off}
function TFreeTypeFont.GetDPI: integer;
var metrics: TT_Instance_Metrics;
begin
  if not FInstanceCreated then
    result := 96
  else
  begin
    if TT_Get_Instance_Metrics(FInstance,metrics) = TT_Err_Ok then
      result := metrics.y_resolution
    else
      result := 96;
  end;
end;
{$hints on}

function TFreeTypeFont.FindGlyphNode(Index: Integer): TAvgLvlTreeNode;
var DataValue: integer;
begin
  Result:=FGlyphTable.Root;
  while (Result<>nil) do begin
    DataValue := TFreeTypeGlyph(Result.Data).Index;
    if Index=DataValue then exit;
    if Index<DataValue then begin
      Result:=Result.Left
    end else begin
      Result:=Result.Right
    end;
  end;
end;

function TFreeTypeFont.GetClearType: boolean;
begin
  Result:= FClearType;
end;

function TFreeTypeFont.GetCharIndex(AChar: integer): integer;
begin
  if FCharmapOk then
    result := TT_Char_Index(FCharMap, AChar)
  else
    result := AChar;
end;

function TFreeTypeFont.GetGlyph(Index: integer): TFreeTypeGlyph;
var node: TAvgLvlTreeNode;
    lGlyph: TFreeTypeGlyph;
begin
  node := FindGlyphNode(Index);
  if node = nil then
  begin
    lGlyph := TFreeTypeGlyph.Create(self, Index);;
    FGlyphTable.Add(lGlyph);
  end else
    lGlyph := TFreeTypeGlyph(node.Data);
  result := lGlyph;
end;

{$hints off}
function TFreeTypeFont.GetGlyphCount: integer;
var prop : TT_Face_Properties;
begin
  if not FFaceLoaded then
    result := 0
  else
  begin
    if TT_Get_Face_Properties(FFace, prop) <> TT_Err_Ok then
      result := 0
    else
      result := prop.num_glyphs;
  end;
end;
{$hints on}

function TFreeTypeFont.GetPixelSize: single;
begin
  result := SizeInPoints * DPI / 72;
end;

procedure TFreeTypeFont.SetClearType(const AValue: boolean);
begin
  if FClearType=AValue then exit;
  FClearType:=AValue;
  UpdateSizeInPoints;
end;

procedure TFreeTypeFont.SetDPI(const AValue: integer);
begin
  if FInstanceCreated then
  begin
    TT_Set_Instance_Resolutions(FInstance, AValue,AValue);
    UpdateSizeInPoints;
  end;
end;

procedure TFreeTypeFont.SetHinted(const AValue: boolean);
begin
  if FHinted=AValue then exit;
  FHinted:=AValue;
  FGlyphTable.FreeAndClear;
end;

procedure TFreeTypeFont.DiscardFace;
begin
  if FFaceLoaded then
  begin
    TT_Close_Face(FFace);
    FFaceLoaded := false;
  end;
  FCharmapOk := false;
end;

procedure TFreeTypeFont.DiscardInstance;
begin
  if FInstanceCreated then
  begin
    TT_Done_Instance(FInstance);
    FInstanceCreated := false;
    FGlyphTable.FreeAndClear;
  end;
end;

procedure TFreeTypeFont.SetPixelSize(const AValue: single);
begin
  if FInstanceCreated then
    SizeInPoints := AValue*72/DPI;
end;

procedure TFreeTypeFont.SetPointSize(const AValue: single);
begin
  if FPointSize=AValue then exit;
  FPointSize:=AValue;
  if FInstanceCreated then
    UpdateSizeInPoints;
end;

function TFreeTypeFont.LoadGlyphInto(_glyph: TT_Glyph; glyph_index: Word): boolean;
var flags: integer;
begin
  if not FInstanceCreated then
    raise Exception.Create('No font instance');
  flags := TT_Load_Scale_Glyph;
  if FHinted then flags := flags or TT_Load_Hint_Glyph;
  result := (TT_Load_Glyph(FInstance, _glyph, glyph_index, flags) <> TT_Err_Ok);
end;

procedure TFreeTypeFont.SetWidthFactor(const AValue: single);
begin
  if FWidthFactor=AValue then exit;
  FWidthFactor:=AValue;
  FGlyphTable.FreeAndClear;
  if FInstanceCreated then
    UpdateSizeInPoints;
end;

procedure TFreeTypeFont.UpdateSizeInPoints;
var charsizex: integer;
begin
  if FInstanceCreated then
  begin
    if not FClearType then
      charsizex := round(FPointSize*64*FWidthFactor)
    else
      charsizex := round(FPointSize*64*FWidthFactor*3);

    if TT_Set_Instance_CharSizes(FInstance,charsizex,round(FPointSize*64)) <> TT_Err_Ok then
      raise Exception.Create('Unable to set point size');
    FGlyphTable.FreeAndClear;
  end;
end;

procedure TFreeTypeFont.GetCharmap;
var i,n: integer;
    platform,encoding: integer;
begin
  if FCharmapOk then exit;
  if not FFaceLoaded then
  begin
    FCharmapOk := false;
    exit;
  end;

  n := TT_Get_CharMap_Count(FFace);
  platform := 0;
  encoding := 0;

  //MS Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, platform, encoding) = TT_Err_Ok then
    begin
      if (platform = 3) and (encoding = 1) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  //Apple Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, platform, encoding) = TT_Err_Ok then
    begin
      if (platform = 0) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  //ISO Unicode
  for i := 0 to n-1 do
  begin
    if TT_Get_CharMap_ID(FFace, i, platform, encoding) = TT_Err_Ok then
    begin
      if (platform = 2) and (encoding = 1) then
        if TT_Get_CharMap(FFace, i, FCharMap) = TT_Err_Ok then
        begin
          FCharmapOk := true;
          exit;
        end;
    end;
  end;

  FCharmapOk := false;
end;

constructor TFreeTypeFont.Create;
begin
  EnsureFreeTypeInitialized;
  FFaceLoaded := false;
  FInstanceCreated := false;
  FCharmapOk := false;
  FPointSize := 10;
  FGlyphTable := TAvgLvlTree.Create;
  FHinted := true;
  FWidthFactor := 1;
  FClearType := false;
end;

destructor TFreeTypeFont.Destroy;
begin
  DiscardInstance;
  DiscardFace;
  FGlyphTable.Free;
  inherited Destroy;
end;

procedure TFreeTypeFont.RenderText(AText: string; x, y: single; ARect: TRect;
  OnRender: TDirectRenderingFunction);
var
  pstr: pchar;
  left,charcode,charlen: integer;
begin
  if AText = '' then exit;
  pstr := @AText[1];
  left := length(AText);
  while left > 0 do
  begin
    charcode := UTF8CharacterToUnicode(pstr, charlen);
    inc(pstr,charlen);
    dec(left,charlen);
    with Glyph[CharIndex[charcode]] do
    begin
      RenderDirectly(x,y,ARect,OnRender,quality,FClearType);
      if FClearType then
        x += Advance/3
      else
        x += Advance;
    end;
  end;
end;

function TFreeTypeFont.TextWidth(AText: string): single;
var
  pstr: pchar;
  left,charcode,charlen: integer;
begin
  result := 0;
  if AText = '' then exit;
  pstr := @AText[1];
  left := length(AText);
  while left > 0 do
  begin
    charcode := UTF8CharacterToUnicode(pstr, charlen);
    inc(pstr,charlen);
    dec(left,charlen);
    with Glyph[CharIndex[charcode]] do
    begin
      if FClearType then
        result += Advance/3
      else
        result += Advance;
    end;
  end;
end;

function TFreeTypeFont.CharsWidth(AText: string): ArrayOfSingle;
var
  pstr: pchar;
  left,charcode,charlen: integer;
  resultIndex: integer;
begin
  if AText = '' then exit;
  pstr := @AText[1];
  left := length(AText);
  setlength(result, UTF8Length(AText));
  resultIndex := 0;
  while left > 0 do
  begin
    charcode := UTF8CharacterToUnicode(pstr, charlen);
    inc(pstr,charlen);
    dec(left,charlen);

    with Glyph[CharIndex[charcode]] do
    begin
      if FClearType then
        result[resultIndex] := Advance/3
      else
        result[resultIndex] := Advance;
    end;
    inc(resultIndex);
  end;
end;

{ TFreeTypeGrayscaleMap }

constructor TFreeTypeGrayscaleMap.Create(AWidth,AHeight: integer);
begin
  map.Width := AWidth;
  map.Rows := AHeight;
  map.Cols:= (AWidth+3) and not 3;
  map.flow:= TT_Flow_Down;
  map.Size:= map.Rows*map.Cols;
  getmem(map.Buffer,map.Size);
  Clear;
  RenderQuality := grqHighQuality;
end;

function TFreeTypeGrayscaleMap.RenderGlyph(glyph: TFreeTypeGlyph; x, y: single): boolean;
var mono: TFreeTypeMonochromeMap;
    psrc,pdest: pbyte;
    xb,yb,tx: integer;
    curBit: byte;
begin
  case RenderQuality of
    grqMonochrome:
      begin
        tx := Width;
        mono := TFreeTypeMonochromeMap.Create(tx,Height);
        result := mono.RenderGlyph(glyph,x,y);
        if result then
        begin
          for yb := mono.Height-1 downto 0 do
          begin
            psrc := mono.ScanLine[yb];
            pdest := self.ScanLine[yb];
            curBit := $80;
            for xb := tx-1 downto 0 do
            begin
              if psrc^ and curBit <> 0 then
                pdest^ := $ff;
              curBit := curBit shr 1;
              if curBit = 0 then
              begin
                curBit := $80;
                inc(psrc);
              end;
              inc(pdest);
            end;
          end;
        end;
        mono.Free;
      end;
    grqLowQuality:
      begin
        TT_Set_Raster_Palette(RegularGray5);
        result := TT_Get_Glyph_Pixmap(glyph.data, map, round(x*64), round((height-y)*64)) = TT_Err_Ok;
      end;
    grqHighQuality:
      begin
        result := TT_Get_Glyph_Pixmap_HQ(glyph.data, map, round(x*64), round((height-y)*64)) = TT_Err_Ok;
      end;
  end;
end;

procedure TFreeTypeGrayscaleMap.ScanMoveTo(x, y: integer);
begin
  ScanPtrStart := pbyte(ScanLine[y]);
  ScanX := x mod Width;
  if ScanX < 0 then inc(ScanX,Width);
end;

function TFreeTypeGrayscaleMap.ScanNextPixel: byte;
begin
  if ScanPtrStart = nil then
    result := 0
  else
  begin
    result := (ScanPtrStart+ScanX)^;
    inc(ScanX);
    if ScanX = map.Width then ScanX := 0;
  end;
end;

function TFreeTypeGrayscaleMap.GetPixel(x, y: integer): byte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    result := 0
  else
    result := (pbyte(map.Buffer) + y*map.Cols + x)^;
end;

procedure TFreeTypeGrayscaleMap.SetPixel(x, y: integer; value: byte);
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
    (pbyte(map.Buffer) + y*map.Cols + x)^ := value;
end;

procedure TFreeTypeGrayscaleMap.XorPixel(x, y: integer; value: byte);
var p : pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := (pbyte(map.Buffer) + y*map.Cols + x);
    p^ := p^ xor value;
  end;
end;

{ TFreeTypeRasterMap }

function TFreeTypeRasterMap.GetHeight: integer;
begin
  result := map.Rows;
end;

function TFreeTypeRasterMap.GetWidth: integer;
begin
  result := map.Width;
end;

function TFreeTypeRasterMap.GetScanLine(y: integer): pointer;
begin
  if (y <0) or (y >= height) then
    result := nil
  else
    Result:= pointer(pbyte(map.Buffer) + y*map.Cols);
end;

procedure TFreeTypeRasterMap.Clear;
begin
  fillchar(map.Buffer^, map.Size, 0);
end;

procedure TFreeTypeRasterMap.Fill;
begin
  fillchar(map.Buffer^, map.Size, $ff);
end;

destructor TFreeTypeRasterMap.Destroy;
begin
  freemem(map.Buffer);
  inherited Destroy;
end;

{ TFreeTypeMonochromeMap }

constructor TFreeTypeMonochromeMap.Create(AWidth,AHeight: integer);
begin
  map.Width := AWidth;
  map.Rows := AHeight;
  map.Cols:= (AWidth+7) shr 3;
  map.flow:= TT_Flow_Down;
  map.Size:= map.Rows*map.Cols;
  getmem(map.Buffer,map.Size);
  Clear;
end;

function TFreeTypeMonochromeMap.RenderGlyph(glyph: TFreeTypeGlyph; x,y: single): boolean;
begin
  result := TT_Get_Glyph_Bitmap(glyph.data, map, round(x*64), round((height-y)*64)) = TT_Err_Ok;
end;

procedure TFreeTypeMonochromeMap.ScanMoveTo(x, y: integer);
begin
  ScanPtrStart := pbyte(ScanLine[y]);
  ScanX := x mod Width;
  if ScanX < 0 then inc(ScanX,Width);

  if ScanPtrStart <> nil then
  begin
    ScanPtrCur := ScanPtrStart + (ScanX shr 3);
    ScanBit := $80 shr (ScanX and 7);
  end else
  begin
    ScanPtrCur := nil;
    ScanBit := 0;
  end;
end;

function TFreeTypeMonochromeMap.ScanNextPixel: boolean;
begin
  if ScanPtrCur = nil then
    result := false
  else
  begin
    result := (pbyte(ScanPtrCur)^ and ScanBit) <> 0;
    inc(ScanX);
    if ScanX = map.Width then
    begin
      ScanX := 0;
      ScanBit := $80;
      ScanPtrCur := ScanPtrStart;
    end else
    begin
      ScanBit := ScanBit shr 1;
      if ScanBit = 0 then
      begin
        ScanBit := $80;
        inc(ScanPtrCur);
      end;
    end;
  end;
end;

function TFreeTypeMonochromeMap.GetPixel(x, y: integer): boolean;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    result := false
  else
    result := (pbyte(map.Buffer) + y*map.Cols + (x shr 3))^ and ($80 shr (x and 7)) <> 0;
end;

procedure TFreeTypeMonochromeMap.SetPixel(x, y: integer; value: boolean);
var p: pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := pbyte(map.Buffer) + y*map.Cols + (x shr 3);
    if not value then
      p^ := p^ and not ($80 shr (x and 7))
    else
      p^ := p^ or ($80 shr (x and 7));
  end;
end;

function TFreeTypeMonochromeMap.GetPixelsInRect(x, y, x2, y2: integer): integer;
var yb: integer;
begin
  result := 0;

  if x < 0 then x := 0;
  if x2 > width then x2 := width;
  if x2 <= x then exit;

  if y < 0 then y := 0;
  if y2 > height then y2 := height;
  for yb := y to y2-1 do
    result += GetPixelsInHorizlineNoBoundsChecking(x,yb,x2-1);
end;

function TFreeTypeMonochromeMap.GetPixelsInHorizline(x, y, x2: integer): integer;
begin
  if x < 0 then x := 0;
  if x2 >= width then x2 := width-1;
  if x2 <= x then
  begin
    result := 0;
    exit;
  end;
  if (y < 0) or (y >= height) then
  begin
    result := 0;
    exit;
  end;

  result := GetPixelsInHorizlineNoBoundsChecking(x,y,x2);
end;

function TFreeTypeMonochromeMap.GetPixelsInHorizlineNoBoundsChecking(x, y, x2: integer
  ): integer;
var p: pbyte;
    ix,ix2: integer;
begin
  result := 0;
  ix := x shr 3;
  ix2 := x2 shr 3;
  p := pbyte(map.Buffer) + y*map.Cols + ix;
  if ix2 > ix then
  begin
    result += BitCountTable[ p^ and ($ff shr (x and 7)) ];
    inc(p^);
    inc(ix);
    while (ix2 > ix) do
    begin
      result += BitCountTable[p^];
      inc(ix);
      inc(p^);
    end;
    result += BitCountTable[ p^ and ($ff shl (x2 and 7 xor 7)) ];
  end else
    result += BitCountTable[ p^ and ($ff shr (x and 7)) and ($ff shl (x2 and 7 xor 7))];
end;

procedure TFreeTypeMonochromeMap.TogglePixel(x, y: integer);
var p: pbyte;
begin
  if (x < 0) or (x>= width) or (y <0) or (y >= height) then
    exit
  else
  begin
    p := pbyte(map.Buffer) + y*map.Cols + (x shr 3);
    p^ := p^ xor ($80 shr (x and 7));
  end;
end;

procedure InitTables;
var i: integer;
begin
  for i := 0 to 255 do
  begin
    BitCountTable[i] := (i and 1) + (i shr 1 and 1) + (i shr 2 and 1) + (i shr 3 and 1) +
       (i shr 4 and 1) + (i shr 5 and 1) + (i shr 6 and 1) + (i shr 7 and 1);
  end;

  RegularGray5[0] := 0;
  RegularGray5[1] := $60;
  RegularGray5[2] := $a0;
  RegularGray5[3] := $d0;
  RegularGray5[4] := $ff;
end;

initialization

  FreeTypeInitialized := false;
  FreeTypeCannotInitialize := false;
  InitTables;

finalization

  if FreeTypeInitialized then
  begin
    TT_Done_FreeType;
    FreeTypeInitialized := false;
  end;

end.

