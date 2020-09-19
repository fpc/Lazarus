unit lr_ngcanvas;

{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Math, Types, LCLType, LCLProc, LazLogger, Graphics, FileUtil,
  fpjson, jsonparser, FPimage, FPImgCanv, FPCanvas, FPWriteBMP, FPWritePNG,
  fpparsettf, EasyLazFreeType, LazFreeTypeFontCollection, LazFreeTypeFPImageDrawer;

const
  FONTS_CACHE_FILE  = '.fonts.cache';
  FONT_ARIAL        = 'Arial';
  FONT_TIMES        = 'Times New Roman';
  FONT_COURIER      = 'Courier New';
  FONT_DEJAVU_SANS  = 'DejaVu Sans';
  FONT_DEJAVU_SERIF = 'DejaVu Serif';
  FONT_DEJAVU_MONO  = FONT_DEJAVU_SANS + ' Mono';
  FONT_LIB_SANS     = 'Liberation Sans';
  FONT_FREE_SANS    = 'FreeSans';
type

  TVirtualBitmap = class;

  { TFontItem }

  TFontItem = class
  public
    fontName: string;
    fontFile: string;
    fontID: Integer;
    data: TObject;
  end;

  { TFontCache }

  TFontCache = class
  private type
    { TTTFFontInfo }

    TTTFFontInfo = class(TTFFileInfo)
    protected
      // only interested in Head and Name tables
      procedure ParseHhea({%H-}AStream : TStream); override;
      procedure ParseMaxp({%H-}AStream : TStream); override;
      procedure ParseHmtx({%H-}AStream : TStream); override;
      procedure ParseCmap({%H-}AStream : TStream); override;
      procedure ParseOS2({%H-}AStream : TStream); override;
    end;
  private
    fFontList: array of TFontItem;
    fSearchPath: string;
    function GetCount: Integer;
    function GetFonts(aIndex: Integer): TFontItem;
    function BuildFontCache: TJsonObject;
    function FindCachedFont(cache: TJsonObject; fontName: string; bold, italic: boolean): Integer;
    function MatchCachedFont(cache: TJSonObject; fontName:string; bold, italic: boolean): Integer;
  protected
    function GetCachedName(aName:string; bold,italic:boolean): string;
    function IndexOfCachedFont(aName:string): Integer;
    function FindFile(aName:string; bold,italic:boolean): string;
  public
    destructor destroy; override;
    function IndexOfFile(aName:string; bold,italic:boolean): Integer;
    property Count:Integer read GetCount;
    property Fonts[aIndex:Integer]: TFontItem read GetFonts; default;
    property SearchPath:string read fSearchPath write fSearchPath;
  end;

  { TFontManager }

  TFontManager = class
  private
    fFontCache: TFontCache;
    fLastFont: TFreeTypeFont;
    function GetSearchPath: string;
    procedure SetSearchPath(AValue: string);
  public
    constructor Create;
    destructor destroy; override;
    function GetFont(fontName:string; bold, italic: boolean): TFreeTypeFont;
    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    function TextWidth(const Text: string): Integer;

    property SearchPath: string read GetSearchPath write SetSearchPath;
    property Font: TFreeTypeFont read fLastFont;
  end;

  TFontQuality = (fqMono, fqLow, fqHigh);

  { TNoGuiImageCanvas }

  TNoGuiImageCanvas = class(TFPImageCanvas)
  protected
    procedure DoPolygonFill (const points:array of TPoint); override;
  end;

  { TVirtualCanvas }

  TVirtualCanvas = class(TCanvas)
  private
    fImg: TFPMemoryImage;
    fImgCanvas: TNoGuiImageCanvas;
    fFontQuality: TFontQuality;
    fFontHinted: boolean;
    fFontClearType: boolean;
    fDrawer: TFPImageFreeTypeDrawer;
    procedure UpdateFontProperties;
    procedure UpdateBrushProperties;
    procedure UpdatePenProperties;
  protected
    procedure CreateBrush; override;
    procedure CreateFont; override;
    procedure CreateHandle; override;
    procedure CreatePen; override;
    procedure DeselectHandles; override;
    procedure RealizeAntialiasing; override;
    procedure DoMoveTo(x, y: integer); override;
    procedure DoLineTo(x, y: integer); override;
    procedure DoLine(x1, y1, x2, y2: integer); override;

 public
    procedure Rectangle(X1,Y1,X2,Y2: Integer); override;
    procedure FillRect(const ARect: TRect); override;
    procedure Fill(color: TFPColor);
    procedure Polygon(Points: PPoint; NumPts: Integer;
                      {%H-}Winding: boolean = False); override;
    procedure Polyline(Points: PPoint; NumPts: Integer); override;
    procedure TextOut(X,Y: Integer; const Text: String); override;
    procedure TextRect(ARect: TRect; X, Y: integer; const Text: string;
                      const Style: TTextStyle); override;
    function TextExtent(const Text: string): TSize; override;
    function TextHeight(const Text: string): Integer; override;

    constructor create(image: TFpMemoryImage);
    procedure Draw(X,Y: Integer; SrcGraphic: TVirtualBitmap); reintroduce;
    procedure StretchDraw(const DestRect: TRect; SrcGraphic: TVirtualBitmap); reintroduce;
    destructor destroy; override;

    property FontQuality: TFontQuality read fFontQuality write fFontQuality;
    property FontHinted: boolean read fFontHinted write fFontHinted;
  end;

  { TRasterImageHelper }

  TRasterImageHelper = class helper for TRasterImage
  public
    function RequestRawStream(out rawStream: TMemoryStream): boolean;
  end;

  { TVirtualBitmap }

  TVirtualBitmap = class
  private
    fImg: TFPMemoryImage;
    fCanvas: TVirtualCanvas;
    function GetCanvas: TCanvas;
    function GetHeight: Integer;
    function GetStream: TMemoryStream;
    function GetWidth: Integer;
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
  public
    constructor create;
    destructor destroy; override;
    procedure SaveToFile(filename: string; Writer: TFPCustomImageWriter=nil);   // writer=nil will be a bmp file
    procedure SaveToStream(aStream: TStream; Writer: TFPCustomImageWriter=nil); // writer=nil will be a bmp stream
    procedure SetSize(aWidth, aHeight: integer);
    procedure LoadFromGraphic(Graphic: TGraphic);
    function  GetStreamAsFormat(formatExtension: string; useAlpha:boolean=false): TMemoryStream;
    procedure Clear;

    property Stream: TMemoryStream read GetStream;
    property Canvas: TCanvas read GetCanvas;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;

  function DrawTextNoGui(Canvas: TCanvas; text:string; var Rect: TRect; flags: DWord): Integer;

var
  FontManager: TFontManager = nil;

implementation

var
  fontCacheBuilt: boolean = false;

procedure JSonToFile(Obj: TJSONData; aFilename: string; Opt: TFormatOptions = DefaultFormat);
var
  l: TStringList;
begin
  l := TStringList.Create;
  l.Text := Obj.FormatJSON(Opt);
  l.SaveToFile(aFilename);
  l.Free;
end;

function JsonFromFile(aFilename: string): TJSonData;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(aFilename, fmOpenRead);
  try
    result := GetJSON(stream);
  finally
    stream.free;
  end;
end;

{ TlrFontManager.TTTFFontInfo }

function CompareFontCacheItems(Item1, Item2: Pointer): Integer;
var
  name1,name2: string;
  bold1,bold2: Integer;
  italic1,italic2: Integer;
begin
  name1 := TJSonObject(Item1).Strings['familyname'];
  name2 := TJSonObject(Item2).Strings['familyname'];
  bold1 := ord(TJSonObject(Item1).Booleans['bold']);
  bold2 := ord(TJSonObject(Item2).Booleans['bold']);
  italic1 := Ord(TJSonObject(Item1).Booleans['italic']);
  italic2 := Ord(TJSonObject(Item2).Booleans['italic']);
  result := CompareText(name1,name2);
  if result=0 then begin
    result := bold1 - bold2;
    if result = 0 then begin
      result := italic1 - italic2;
    end;
  end;
end;

function DrawTextNoGui(Canvas: TCanvas; text: string; var Rect: TRect;
  flags: DWord): Integer;
var
  aSize: TSize;
begin
  if DT_CALCRECT and flags<>0 then begin;
    aSize := Canvas.TextExtent(text);
    Rect.Width := aSize.Width;
    Rect.Height := aSize.Height;
    result := aSize.Height;
  end else
    result := 0;
end;

{ TRasterImageHelper }

function TRasterImageHelper.RequestRawStream(out rawStream: TMemoryStream
  ): boolean;
begin
  // make direct use of the saved original stream to avoid re-copying
  // this should be ok as it is very unlikely to change in the future.
  rawStream := FSharedImage.SaveStream;
  result := rawStream<>nil;
  if result then
    rawStream.Position := 0;
end;

{ TNoGuiImageCanvas }

// This code is from LazCanvas, which implements the algorithm found in
// http://alienryderflex.com/polygon_fill/
procedure TNoGuiImageCanvas.DoPolygonFill(const points: array of TPoint);
var
  lBoundingBox: TRect;
  x, y, i: integer;
  // faster version
  nodes, j, swap, polyCorners: Integer;
  nodeX: array of Integer;
begin
  if Brush.Style = bsClear then Exit;

  // Find the Bounding Box of the Polygon
  lBoundingBox := Rect(0, 0, 0, 0);
  for i := low(Points) to High(Points) do
  begin
    lBoundingBox.Left := Min(Points[i].X, lBoundingBox.Left);
    lBoundingBox.Top := Min(Points[i].Y, lBoundingBox.Top);
    lBoundingBox.Right := Max(Points[i].X, lBoundingBox.Right);
    lBoundingBox.Bottom := Max(Points[i].Y, lBoundingBox.Bottom);
  end;

  //  Loop through the rows of the image.
  polyCorners := Length(points);
  for y := lBoundingBox.Top to lBoundingBox.Bottom do
  begin
    //  Build a list of nodes.
    nodes := 0;
    j := polyCorners-1;
    for i := 0 to polyCorners-1 do
    begin
      if (points[i].Y < y) and (points[j].Y >= y) or
      (points[j].Y < y) and (points[i].Y >= Y) then
      begin
        SetLength(nodeX{%H-}, nodes+1);
        nodeX[nodes] := Round(points[i].X + (y-points[i].Y) / (points[j].Y-points[i].Y) * (points[j].X-points[i].X));
        Inc(nodes);
      end;
      j := i;
    end;

    //  Sort the nodes, via a simple “Bubble” sort.
    i := 0;
    while (i<nodes-1) do
    begin
      if (nodeX[i]>nodeX[i+1]) then
      begin
        swap := nodeX[i];
        nodeX[i] := nodeX[i+1];
        nodeX[i+1] := swap;
        if (i <> 0) then Dec(i);
      end
      else
        Inc(i);
    end;

    //  Fill the pixels between node pairs.
    i := 0;
    while i<nodes do
    begin
      if   (nodeX[i  ] >= lBoundingBox.Right) then break;
      if   (nodeX[i+1] > lBoundingBox.Left) then
      begin
        if (nodeX[i  ] < lBoundingBox.Left) then nodeX[i] := lBoundingBox.Left;
        if (nodeX[i+1] > lBoundingBox.Right) then nodeX[i+1] := lBoundingBox.Right;
        for X := nodeX[i] to nodeX[i+1]-1 do
          SetColor(X, Y, Brush.FPColor);
      end;

      i := i + 2;
    end;
  end;
end;

{ TFontManager }

function TFontManager.GetSearchPath: string;
begin
  result := fFontCache.SearchPath;
end;

procedure TFontManager.SetSearchPath(AValue: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.StrictDelimiter := true;
    L.Delimiter := ';';
    L.DelimitedText := fFontCache.SearchPath;
    if L.IndexOf(AValue)<0 then
      L.Add(AValue);
    fFontCache.SearchPath := L.DelimitedText;
  finally
    L.Free;
  end;
end;

constructor TFontManager.Create;
begin
  inherited create;
  fFontCache := TFontCache.Create;
end;

destructor TFontManager.destroy;
var
  i: Integer;
begin
  for i:=0 to fFontCache.Count-1 do
    fFontCache[i].data.free;
  fFontCache.Free;
  inherited destroy;
end;

function TFontManager.GetFont(fontName: string; bold, italic: boolean
  ): TFreeTypeFont;
var
  index: Integer;
  item: TFontItem;
begin
  index := fFontCache.IndexOfFile(fontName, bold, italic);
  item := fFontCache[index];
  result := TFreeTypeFont(item.Data);
  if result=nil then begin
    // register new font
    //WriteLn('Registering ', item.fontFile,' for ', item.fontName);
    result := TFreeTypeFont.create;
    result.Name := FontCollection.AddFile(item.fontFile).Family.FamilyName;
    item.data := result;
  end;
  fLastFont := result;
end;

function TFontManager.TextExtent(const Text: string): TSize;
begin
  result.cx := TextWidth(Text);
  result.cy := TextHeight(Text);
end;

function TFontManager.TextHeight(const Text: string): Integer;
begin
  if fLastFont=nil then
    raise Exception.Create('Font was not initialized');
  result := Round(FLastFont.TextHeight(Text));
end;

function TFontManager.TextWidth(const Text: string): Integer;
begin
  if fLastFont=nil then
    raise Exception.Create('Font was not initialized');
  result := Round(FLastFont.TextWidth(Text))
end;

{ TFontCache.TTTFFontInfo }

procedure TFontCache.TTTFFontInfo.ParseHhea(AStream: TStream);
begin
end;

procedure TFontCache.TTTFFontInfo.ParseMaxp(AStream: TStream);
begin
end;

procedure TFontCache.TTTFFontInfo.ParseHmtx(AStream: TStream);
begin
end;

procedure TFontCache.TTTFFontInfo.ParseCmap(AStream: TStream);
begin
end;

procedure TFontCache.TTTFFontInfo.ParseOS2(AStream: TStream);
begin
end;

{ TFontCache }

function TFontCache.GetFonts(aIndex: Integer): TFontItem;
begin
  result := FFontList[aIndex];
end;

function TFontCache.BuildFontCache: TJsonObject;
var
  pathList, fontList: TStringList;
  i: Integer;
  arr: TJsonArray;
  item: TJSONObject;
  cacheFile: string;
  fileInfo: TTTFFontInfo;
  isBold, isItalic, isFixed: boolean;
begin

  cacheFile := {GetUserDir + }FONTS_CACHE_FILE;

  if not FileExists(cacheFile) or
     ((GetEnvironmentVariable('FONTS_CACHE_REBUILD')<>'') and (not fontCacheBuilt)) then
  begin
    pathList := TStringList.Create;
    fontList := TStringList.Create;
    fileInfo := TTTFFontInfo.Create;
    try
      pathList.StrictDelimiter := true;
      pathList.Delimiter := ';';
      pathList.DelimitedText := SearchPath;
      for i := 0 to pathList.Count-1 do
        FindAllFiles(fontList, pathList[i], '*.ttf;*.otf');

      arr := TJsonArray.Create;
      for i := 0 to fontList.Count-1 do begin
        //WriteLn(i:4,' ',fontList[i]);
        try
          fileInfo.FamilyName := '';
          fileInfo.PostScriptName := '';
          fileInfo.HumanFriendlyName := '';
          fileInfo.LoadFromFile(fontList[i]);
          item := TJSONObject.Create;

          item.Add('filename', fontList[i]);
          if fileInfo.FamilyName<>'' then
            item.Add('familyname', fileInfo.FamilyName)
          else
            item.Add('familyname', ChangeFileExt(ExtractFileName(fontlist[i]), ''));
          item.Add('psname', fileInfo.PostScriptName);
          item.Add('friendlyname', fileInfo.HumanFriendlyName);
          isBold := fileInfo.Head.MacStyle and 1 <> 0;
          isItalic := fileInfo.Head.MacStyle and 2 <> 0;
          isFixed := fileInfo.PostScript.isFixedPitch<>0;
          item.Add('regular', not isFixed);
          item.Add('bold',  isBold);
          item.Add('italic', isItalic);
          item.Add('fixedwidth', isFixed);
          arr.add(item);
        except
        end;
      end;
      arr.Sort(@CompareFontCacheItems);
      result := TJsonObject.Create;
      result.Add('fonts', arr);
      if arr.Count>0 then
        JSonToFile(result, cacheFile);
      fontCacheBuilt := true;
    finally
      fileInfo.free;
      pathList.Free;
      fontList.Free;
    end;
  end else
    result := TJSonObject(JSonFromFile(cacheFile));
end;

function TFontCache.FindCachedFont(cache: TJsonObject; fontName: string;
  bold, italic: boolean): Integer;
var
  I, L, R, CompareRes: Integer;
  key: TJSonObject;
  arr: TJSonArray;
begin
  arr := cache.Arrays['fonts'];

  key := TJsonObject.Create;
  try
    key.Add('familyname', fontName);
    key.Add('bold', bold);
    key.Add('italic', italic);

    result := -1;

    // binary search from TStringList ...
    L := 0;
    R := arr.Count - 1;
    while (L<=R) do
    begin
      I := L + (R - L) div 2;
      CompareRes :=  CompareFontCacheItems(key, arr[I]);
      if (CompareRes>0) then
        L := I+1
      else begin
        R := I-1;
        if (CompareRes=0) then begin
           result := I;
           L := I;
           break;
        end;
      end;
    end;

  finally
    key.Free;
  end;
end;

function TFontCache.MatchCachedFont(cache: TJSonObject; fontName: string; bold,
  italic: boolean): Integer;
var
  L: TStringList;
  testFont: String;
  aliasFont: boolean;
begin

  testFont := fontName;
  aliasFont := true;
  // aliases
  case lowercase(testFont) of
    '', 'default', 'sans', 'helvetica', 'arial':
      testFont := FONT_ARIAL+';'+FONT_DEJAVU_SANS+';'+FONT_LIB_SANS+';'+FONT_FREE_SANS;
    'times', 'serif':
      testFont := FONT_TIMES+';'+FONT_DEJAVU_SERIF;
    'courier', 'mono', 'fixed':
      testFont := FONT_COURIER+';'+FONT_DEJAVU_MONO;
    'dejavu sans':
      testFont := FONT_DEJAVU_SANS+';'+FONT_ARIAL+';'+FONT_LIB_SANS+';'+FONT_FREE_SANS;
    'liberation sans':
      testFont := FONT_LIB_SANS+';'+FONT_DEJAVU_SANS+';'+FONT_ARIAL+';'+FONT_FREE_SANS;
    'freesans':
      testFont := FONT_FREE_SANS+';'+FONT_LIB_SANS+';'+FONT_DEJAVU_SANS+';'+FONT_ARIAL;
    else
      aliasFont := false;
  end;

  L := TStringList.Create;
  try
    L.StrictDelimiter := true;
    L.Delimiter := ';';
    L.DelimitedText := testFont;

    result := -1;

    for testFont in L do begin
      result := FindCachedFont(cache, testFont, bold, italic);

      if (result<0) and bold and italic then begin
        result := FindCachedFont(cache, testFont, true, false);
        if result<0 then
          result := FindCachedFont(cache, testFont, false, true);
        if result<0 then
          result := FindCachedFont(cache, testFont, false, false);
      end else
      if (result<0) and bold then
        result := FindCachedFont(cache, testFont, false, false);

      if result>=0 then
        break;
    end;

    if (result<0) and not aliasFont then begin
      result := MatchCachedFont(Cache, FONT_ARIAL, bold, italic);
    end;
  finally
    L.Free;
  end;

end;

function TFontCache.GetCount: Integer;
begin
  result := Length(FFontList);
end;

function TFontCache.GetCachedName(aName: string; bold, italic: boolean
  ): string;
begin
  result := format('%s_%d_%d',[aName, ord(bold), ord(italic)]);
end;

function TFontCache.IndexOfCachedFont(aName: string): Integer;
var
  i: Integer;
begin
  result := -1;
  for i:=0 to Length(FFontList)-1 do begin
    if FFontList[i].fontName=aName then begin
      result := i;
      break;
    end;
  end;
end;

function TFontCache.FindFile(aName: string; bold, italic: boolean): string;
var
  index: Integer;
  cache: TJSonObject;
  arr: TJSONArray;
  cachedName: string;
  fontItem: TFontItem;
begin

  cachedName := GetCachedName(aName, bold, italic);
  index := IndexOfCachedFont(cachedName);
  if index>=0 then begin
    result := fFontList[index].fontFile;
    exit;
  end;

  // remove the foundry if exists
  index := pos('[', aName);
  if index>0 then begin
    Delete(aName, index, Length(aName));
    aName := Trim(aName);
  end;

  cache := BuildFontCache;

  index := MatchCachedFont(cache, aName, bold, italic);

  if index>=0 then begin
    arr := cache.Arrays['fonts'];
    with TJSonObject(arr[index]) do begin
      result := Strings['filename']
    end
  end else begin
    result := aName;
  end;

  fontItem := TFontItem.Create;
  fontItem.fontName := cachedName;
  fontItem.fontFile := result;
  fontItem.fontID := -1;
  fontItem.data := nil;

  index := Length(fFontList);
  SetLength(fFontList, index+1);
  fFontList[index] := fontItem;

  cache.Free;
end;

destructor TFontCache.destroy;
var
  i: Integer;
begin
  for i:=0 to Length(fFontList)-1 do
    fFontList[i].Free;
  inherited destroy;
end;

function TFontCache.IndexOfFile(aName: string; bold, italic: boolean): Integer;
var
  cachedFontName: String;
begin
  cachedFontName := GetCachedName(aName, bold, italic);
  result := IndexOfCachedFont(cachedFontName);
  if result<0 then begin
    FindFile(aName, bold, italic);
    result := Count-1;
  end;
end;

{ TVirtualBitmap }

function TVirtualBitmap.GetCanvas: TCanvas;
begin
  if fCanvas=nil then
    fCanvas := TVirtualCanvas.Create(fImg);
  result := fCanvas;
end;

function TVirtualBitmap.GetHeight: Integer;
begin
  result := fImg.Height;
end;

function TVirtualBitmap.GetStream: TMemoryStream;
var
  Writer: TFPWriterBMP;
begin
  Writer:=TFPWriterBMP.create;
  result := TMemoryStream.Create;
  writer.ImageWrite(result, fImg);
  result.position := 0;
  Writer.Free;
end;

function TVirtualBitmap.GetWidth: Integer;
begin
  result := fImg.Width;
end;

procedure TVirtualBitmap.SetHeight(AValue: Integer);
begin
  fImg.Height := AValue;
end;

procedure TVirtualBitmap.SetWidth(AValue: Integer);
begin
  fImg.Width := AValue;
end;

constructor TVirtualBitmap.create;
begin
  inherited create;
  fImg := TFpMemoryImage.Create(0, 0);
  fImg.UsePalette := false;
end;

destructor TVirtualBitmap.destroy;
begin
  fCanvas.Free;
  fImg.Free;
  inherited destroy;
end;

procedure TVirtualBitmap.SaveToFile(filename: string;
  Writer: TFPCustomImageWriter);
var
  workStream: TStream;
begin
  if Writer<>nil then
    workStream := TFileStream.Create(filename, fmCreate)
  else
    workStream := Stream;
  try
    if Writer<>nil then
      SaveToStream(workStream, writer)
    else
      TMemoryStream(workStream).SaveToFile(filename);
  finally
    workStream.Free;
  end;
end;

procedure TVirtualBitmap.SaveToStream(aStream: TStream; Writer: TFPCustomImageWriter);
var
  workStream: TMemoryStream;
begin
  if Writer<>nil then
    writer.ImageWrite(aStream, fImg)
  else begin
    workStream := Stream;
    try
      workStream.SaveToStream(aStream);
    finally
      workStream.Free;
    end;
  end;
end;

procedure TVirtualBitmap.SetSize(aWidth, aHeight: integer);
begin
  fImg.SetSize(aWidth, aHeight);
end;

// this is basically the same function as ConvertGraphicToFPImage from PowerPDF
// PDFImageLazTools.pas, including TRasterImageHelper, if you change this dont
// forget to update that, and viceversa.
procedure TVirtualBitmap.LoadFromGraphic(Graphic: TGraphic);
var
  fpImg: TFPMemoryImage;
  rawImgStream: TMemoryStream = nil;
  useOriginalStream: boolean = false;
begin
  if (Graphic is TRasterImage) then
    useOriginalStream := TRasterImage(Graphic).RequestRawStream(rawImgStream);

  if not useOriginalStream then begin
    rawImgStream := TMemoryStream.Create;
    Graphic.SaveToStream(rawImgStream);
    rawImgStream.Position := 0;
  end;

  try
    fpImg := TFPMemoryImage.Create(0, 0);
    fpImg.UsePalette := false;
    try
      fpImg.LoadFromStream(rawImgStream);
      fImg.Free;
      fImg := fpImg;
    except
      fpImg.Free;
    end;
  finally
    if not useOriginalStream then
      rawImgStream.Free;
  end;
end;

function TVirtualBitmap.GetStreamAsFormat(formatExtension: string; useAlpha:boolean): TMemoryStream;
var
  writerClass: TFPCustomImageWriterClass;
  Writer: TFPCustomImageWriter;
begin
  writerClass := fImg.FindWriterFromExtension(formatExtension);
  if writerClass=nil then
    result := nil
  else begin
    result := TMemoryStream.Create;
    writer := WriterClass.Create;
    if useAlpha and (Writer is TFPWriterPNG) then
      TFPWriterPNG(writer).UseAlpha := useAlpha;
    fImg.SaveToStream(result, writer);
    writer.Free;
  end;
end;

procedure TVirtualBitmap.Clear;
begin
  SetSize(0, 0);
end;

{ TVirtualCanvas }

procedure TVirtualCanvas.UpdateFontProperties;
var
  aFont: TFreeTypeFont;
begin
  aFont := FontManager.GetFont(Font.Name, fsBold in Font.Style, fsItalic in Font.Style);
  aFont.SmallLinePadding := Font.Size<0;
  aFont.SizeInPoints := IfThen(Font.Size=0, 10, Abs(Font.Size));
  aFont.DPI := Font.PixelsPerInch;
  // TODO: convert Font.Quality
  case fFontQuality of
    fqMono: aFont.Quality := grqMonochrome;
    fqLow:  aFont.Quality := grqLowQuality;
    fqHigh: aFont.Quality := grqHighQuality;
  end;
  aFont.Hinted := fFontHinted;
  aFont.ClearType := fFontClearType;
  aFont.UnderlineDecoration := fsUnderline in Font.Style;
  aFont.Orientation := Font.Orientation;
end;

procedure TVirtualCanvas.UpdateBrushProperties;
begin
  fimgCanvas.Brush.FPColor := TColorToFPColor(Brush.Color);
  fimgCanvas.Brush.Style := Brush.Style;
end;

procedure TVirtualCanvas.UpdatePenProperties;
begin
  fImgCanvas.Pen.FPColor := TColorToFPColor(Pen.Color);
  fImgCanvas.Pen.Style := Pen.Style;
  fImgCanvas.Pen.Width := Pen.Width;
  fImgCanvas.Pen.Mode := Pen.Mode;
end;

procedure TVirtualCanvas.CreateBrush;
begin
end;

procedure TVirtualCanvas.CreateFont;
begin
  if FontManager=nil then begin
    FontManager := TFontManager.Create;
    // TODO: mac and user supplied font dir
    FontManager.SearchPath := 'fonts';
    {$IFDEF MSWindows}
    FontManager.SearchPath := 'c:\windows\fonts';
    {$ELSE}
    FontManager.SearchPath := '/usr/share/fonts/truetype';
    {$ENDIF}
  end;
end;

procedure TVirtualCanvas.CreateHandle;
begin
  fImgCanvas := TNoGuiImageCanvas.create(fImg);
  fDrawer := TFPImageFreeTypeDrawer.Create(fImg);

  SetHandle(1);
end;

procedure TVirtualCanvas.CreatePen;
begin
end;

procedure TVirtualCanvas.DeselectHandles;
begin
end;

procedure TVirtualCanvas.RealizeAntialiasing;
begin
  //inherited RealizeAntialiasing;
end;

procedure TVirtualCanvas.FillRect(const ARect: TRect);
begin
  RequiredState([csHandleValid, csBrushValid]);
  UpdateBrushProperties;
  fimgCanvas.FillRect(ARect);
end;

// the conversion from TFPColor to TColor remove the alpha component from
// the color which ruins it, so plain canvas.FillRect can't be used for this.
procedure TVirtualCanvas.Fill(color: TFPColor);
var
  oldColor: TFPColor;
begin
  RequiredState([csHandleValid]);
  oldColor := fimgCanvas.Brush.FPColor;
  fimgCanvas.Brush.FPColor := color;
  fImgCanvas.FillRect(0, 0, fImg.Width, fImg.Height);
  fimgCanvas.Brush.FPColor := oldColor;
end;

procedure TVirtualCanvas.Polygon(Points: PPoint; NumPts: Integer;
  Winding: boolean);
var
  arr: array of TPoint;
  i: Integer;
begin
  if NumPts=0 then
    exit;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  UpdatePenProperties;
  UpdateBrushProperties;
  SetLength(Arr{%H-}, NumPts);
  for i:=1 to NumPts do
    Arr[i-1] := Points[i-1];
  fImgCanvas.Polygon(Arr);
end;

procedure TVirtualCanvas.Polyline(Points: PPoint; NumPts: Integer);
var
  arr: array of TPoint;
  i: Integer;
begin
  if NumPts=0 then
    exit;
  RequiredState([csHandleValid, csPenValid]);
  UpdatePenProperties;
  SetLength(Arr{%H-}, NumPts);
  for i:=1 to NumPts do
    Arr[i-1] := Points[i-1];
  fImgCanvas.Polyline(Arr);
end;

procedure TVirtualCanvas.TextOut(X, Y: Integer; const Text: String);
var
  aFont: TFreeTypeFont;
  sz: TSize;
  Points: array of TPoint;
  oldPenStyle: TFPPenStyle;

  procedure RotatePoints;
  var
    aSin,aCos, px, py: double;
    i: Integer;
  begin
    SinCos(aFont.Orientation * PI / 1800, aSin, aCos);
    for i:=0 to Length(points)-1 do begin
      px := aCos * points[i].x + -aSin * points[i].y;
      py := aSin * points[i].x +  aCos * points[i].y;
      points[i].x :=round(x + px);
      points[i].y :=round(y - py);
    end;
  end;
begin
  RequiredState([csHandleValid, csFontValid]);
  UpdateFontProperties;
  UpdateBrushProperties;

  aFont := FontManager.Font;

  sz := TextExtent(Text);
  if fImgCanvas.Brush.Style=bsSolid then begin
    oldPenStyle := fImgCanvas.Pen.Style;
    fImgCanvas.Pen.Style := psClear;
    SetLength(Points{%H-}, 4);
    Points[0] := point(0, 0);
    Points[1] := point(sz.Width, 0);
    Points[2] := point(sz.Width, -sz.Height);
    Points[3] := point(0, -sz.Height);
    RotatePoints;
    fImgCanvas.Polygon(Points);
    fImgCanvas.Pen.Style := oldPenStyle;
  end;

  SetLength(Points, 2);
  Points[0] := point(0,        -Round(aFont.Ascent));
  Points[1] := point(sz.Width, -Round(aFont.Ascent));
  RotatePoints;
  // easy draws text from the base line
  // while we want to draw from the top
  fDrawer.DrawText(Text, aFont, Points[0].x, Points[0].y, TColorToFPColor(Font.Color));
  fImgCanvas.MoveTo(Points[1].x, Points[1].y);
end;

procedure TVirtualCanvas.TextRect(ARect: TRect; X, Y: integer;
  const Text: string; const Style: TTextStyle);
var
  aFont: TFreeTypeFont;
  Points: array of TPoint;
  oldPenStyle: TFPPenStyle;

  procedure RotatePoints;
  var
    aSin,aCos, px, py: double;
    i: Integer;
  begin
    SinCos(aFont.Orientation * PI / 1800, aSin, aCos);
    for i:=0 to Length(points)-1 do begin
      px := aCos * points[i].x + -aSin * points[i].y;
      py := aSin * points[i].x +  aCos * points[i].y;
      points[i].x :=round(x + px);
      points[i].y :=round(y - py);
    end;
  end;
begin
  RequiredState([csHandleValid, csFontValid]);
  UpdateFontProperties;
  UpdateBrushProperties;

  // TODO: complete all styles and alignment
  aFont := FontManager.Font;

  if Style.Opaque then begin
    oldPenStyle := fImgCanvas.Pen.Style;
    fImgCanvas.Pen.Style := psClear;
    SetLength(Points{%H-}, 4);
    Points[0] := point(0, 0);
    Points[1] := point(aRect.Width, 0);
    Points[2] := point(aRect.Width, -aRect.Height);
    Points[3] := point(0, -aRect.Height);
    RotatePoints;
    fImgCanvas.Polygon(Points);
    fImgCanvas.Pen.Style := oldPenStyle;
  end;

  SetLength(Points, 2);
  Points[0] := point(0,           -Round(aFont.Ascent));
  Points[1] := point(ARect.Width, -Round(aFont.Ascent));
  RotatePoints;
  // easy draws text from the base line
  // while we want to draw from the top
  fDrawer.DrawText(Text, aFont, Points[0].x, Points[0].y, TColorToFPColor(Font.Color));
  fImgCanvas.MoveTo(Points[1].x, Points[1].y);
end;

procedure TVirtualCanvas.DoMoveTo(x, y: integer);
begin
  RequiredState([csHandleValid]);
  fImgCanvas.MoveTo(x, y);
end;

procedure TVirtualCanvas.DoLineTo(x, y: integer);
begin
  RequiredState([csHandleValid, csPenValid]);
  UpdatePenProperties;
  fImgCanvas.LineTo(x, y);
  fImgCanvas.MoveTo(x, y);
end;

procedure TVirtualCanvas.DoLine(x1, y1, x2, y2: integer);
begin
  RequiredState([csHandleValid]);
  UpdatePenProperties;
  fImgCanvas.Line(x1, y1, x2, y2);
end;

procedure TVirtualCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  UpdatePenProperties;
  UpdateBrushProperties;

  fImgCanvas.Rectangle(X1, Y1, X2, Y2);
end;

constructor TVirtualCanvas.create(image: TFpMemoryImage);
begin
  inherited create;
  fImg := image;

  fFontQuality := fqHigh;
  fFontHinted := true;
  fFontClearType := false;
end;

procedure TVirtualCanvas.Draw(X, Y: Integer; SrcGraphic: TVirtualBitmap);
begin
  RequiredState([csHandleValid]);
  fImgCanvas.Draw(x, y, SrcGraphic.fImg);
end;

procedure TVirtualCanvas.StretchDraw(const DestRect: TRect;
  SrcGraphic: TVirtualBitmap);
begin
  RequiredState([csHandleValid]);
  with DestRect do
    fImgCanvas.StretchDraw(left,top,width,height, SrcGraphic.fImg);
end;

destructor TVirtualCanvas.destroy;
begin
  fImgCanvas.Free;
  fDrawer.Free;
  inherited destroy;
end;

function TVirtualCanvas.TextExtent(const Text: string): TSize;
begin
  RequiredState([csHandleValid, csFontValid]);
  UpdateFontProperties;
  result := FontManager.TextExtent(Text);
end;

function TVirtualCanvas.TextHeight(const Text: string): Integer;
begin
  RequiredState([csHandleValid, csFontValid]);
  UpdateFontProperties;
  result := FontManager.TextHeight(Text);
end;

initialization
  // simulate a standard display
  ScreenInfo.PixelsPerInchX := 96;
  ScreenInfo.PixelsPerInchY := 96;
  ScreenInfo.ColorDepth := 32;
  ScreenInfo.Initialized := true;

finalization
  FontManager.Free;

end.
