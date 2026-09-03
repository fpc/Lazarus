unit lmfWMFWrite;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Types, FPImage,
  GraphType, GraphUtil, Graphics, IntfGraphics, LCLType, LConvEncoding,
  lmf, lmfObj, lmfWMF;

type
  { TWMFWriter }

  TWMFWriter = class(TlmfWriter)
  private
    FImage: TlmfImage;
    FMaxRecordSize: Int64;
    FObjTable: TFPList;        // List with WMF objects (pen, brush, ...)
    FCurrBrush: TBrush;
    FCurrFont: TFont;
    FCurrPen: TPen;

    // Specific WMF records
    procedure WriteArc(AStream: TStream; AItem: TlmfArc);
    procedure WriteBitmap(AStream: TStream; ABitmap: TBitmap; ARect: TRect; AOperation: Integer);
    procedure WriteBkColor(AStream: TStream; AColor: TColor);
    procedure WriteBkColor(AStream: TStream; AItem: TlmfBkColor);
    procedure WriteBkMode(AStream: TStream; AMode: Word);
    procedure WriteBkMode(AStream: TStream; AItem: TlmfBkMode);
    procedure WriteBrush(AStream: TStream; AItem: TlmfBrush);
    procedure WriteChord(AStream: TStream; AItem: TlmfChord);
    procedure WriteEllipse(AStream: TStream; AItem: TlmfEllipse);
    procedure WriteEOF(AStream: TStream);
    procedure WriteExtFloodFill(AStream: TStream; AItem: TlmfFloodFill);
    procedure WriteFont(AStream: TStream; AItem: TlmfFont);
    procedure WritePicture(AStream: TStream; AItem: TlmfPicture);
    procedure WriteLineTo(AStream: TStream; AItem: TlmfLineTo);
    procedure WriteLine(AStream: TStream; AItem: TlmfLine);
    procedure WriteMapMode(AStream: TStream; AMode: Word);
    procedure WriteMoveTo(AStream: TStream; AItem: TlmfMoveTo);
    procedure WritePen(AStream: TStream; AItem: TlmfPen);
    procedure WritePie(AStream: TStream; AItem: TlmfPie);
    procedure WritePolygon(AStream: TStream; AItem: TlmfPolygon);
    procedure WritePolyLine(AStream: TStream; AItem: TlmfPolyLine);
    procedure WriteRect(AStream: TStream; AItem: TlmfRect);
    procedure WriteRoundRect(AStream: TStream; AItem: TlmfRoundRect);
    procedure WriteText(AStream: TStream; AItem: TlmfText);
    procedure WriteTextAlign(AStream: TStream; AValue: Word);
    procedure WriteTextInRect(AStream: TStream; AItem: TlmfTextInRect);
    procedure WriteWindowExt(AStream: TStream);
    procedure WriteWindowOrg(AStream: TStream);
    // misc
    procedure ProcessTextInRect(AStream: TStream; AItem: TObject);

  protected
    // General routines
    function AddToObjTable(AItem: TComponent): Integer;
    function CalcChecksum(P: PWord; ASize: Word): Word;
    function FindInObjTable(AItem: TComponent): Integer;
    function MakeWMFColorRecord(AColor: TColor): TWMFColorRecord;
    // General WMF record writing
    procedure WriteRecords(AStream: TStream);
    procedure WriteWMFRecord(AStream: TStream; AFunc: word; ASize: Integer);
    procedure WriteWMFRecord(AStream: TStream; AFunc: Word; const AParams; ASize: Integer);
    procedure WriteWMFParams(AStream: TStream; const AParams; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToStream(AStream: TStream; AImage: TlmfImage); override;
  end;


implementation

uses
  bmpcomn;

const
  SIZE_OF_WORD = 2;
             (*
  // Brush styles
  BS_SOLID = $0000;
  BS_NULL = $0001;
  BS_HATCHED = $0002;
  BS_PATTERN = $0003;
  BS_INDEXED = $0004;
  BS_DIBPATTERN = $0005;
  BS_DIBPATTERNPT = $0006;
  BS_PATTERN8X8 = $0007;
  BS_DIBPATTERN8X8 = $0008;
  BS_MONOPATTERN = $0009;

  // Character sets
  ANSI_CHARSET = $00000000;
  DEFAULT_CHARSET = $00000001;
  SYMBOL_CHARSET = $00000002;
  MAC_CHARSET = $0000004D;
  SHIFTJIS_CHARSET = $00000080;
  HANGUL_CHARSET = $00000081;
  JOHAB_CHARSET = $00000082;
  GB2312_CHARSET = $00000086;
  CHINESEBIG5_CHARSET = $00000088;
  GREEK_CHARSET = $000000A1;
  TURKISH_CHARSET = $000000A2;
  VIETNAMESE_CHARSET = $000000A3;
  HEBREW_CHARSET = $000000B1;
  ARABIC_CHARSET = $000000B2;
  BALTIC_CHARSET = $000000BA;
  RUSSIAN_CHARSET = $000000CC;
  THAI_CHARSET = $000000DE;
  EASTEUROPE_CHARSET = $000000EE;
  OEM_CHARSET = $000000FF;

  // ExtTextOutOptions flags
  ETO_OPAQUE = $0002;
  ETO_CLIPPED = $0004;
  ETO_GLYPHINDEX = $0010;
  ETO_RTLREADING = $0080;
  ETO_NUMERICSLOCAL = $0400;
  ETO_NUMERICSLATIN = $0800;
  ETO_PDY = $2000;

  // Family font
  FF_DONTCARE = $00;
  FF_ROMAN = $01;
  FF_SWISS = $02;
  FF_MODERN = $03;
  FF_SCRIPT = $04;
  FF_DECORATIVE = $05;

  // Flood fill
  FLOODFILLBORDER = $0000;
  FLOODFILLSURFACE = $0001;

  // Font quality
  DEFAULT_QUALITY = $00;
  DRAFT_QUALITY = $01;
  PROOF_QUALITY = $02;
  NONANTIALIASED_QUALITY = $03;
  ANTIALIASED_QUALITY = $04;
  CLEARTYPE_QUALITY = $05;

  // Hatch style
  HS_HORIZONTAL = $0000;
  HS_VERTICAL = $0001;
  HS_FDIAGONAL = $0002; // \\\
  HS_BDIAGONAL = $0003; // ///
  HS_CROSS = $0004;     // +++
  HS_DIAGCROSS = $0005; // xxxx

  // Map mode
  MM_TEXT = $0001;         // 1 logical unit = 1 device pixel. +x right, +y down
  MM_LOMETRIC = $0002;     // 1 logical unit = 0.1 mm. +x right, +y up
  MM_HIMETRIC = $0003;     // 1 logical unit = 0.01 mm. +x right, +y up
  MM_LOENGLISH = $0004;    // 1 logical unit = 0.01 inch. +x right, +y up
  MM_HIENGLISH = $0005;    // 1 logical unit = 0.001 inch. +x right, +y up
  MM_TWIPS = $0006;        // 1 logical unit = 1/20 point = 1/1440 inch (twip). +x right, +y up
  MM_ISOTROPIC = $0007;    // arbitrary units, equally scaled axes. --> META_SETWINDOWEXT, META_SETWINDOWORG
  MM_ANISOTROPIC = $0008;  // arbitrary units, arbitrarily scaled axes.

  // Metafile enumeration
  MEMORYMETAFILE = $0001;  // Metafile is stored in memory
  DISKMETAFILE = $0002;    // ... on disk.

  // PitchFont
  DEFAULT_PITCH = 0;
  FIXED_PITCH = 1;
  VARIABLE_PITCH = 2;

  // TextAlignment flags
  TA_NOUPDATECP = $0000;
  TA_LEFT = $0000;
  TA_TOP = $0000;
  TA_UPDATECP = $0001;
  TA_RIGHT = $0002;
  TA_CENTER = $0006;   // Value is correct ($0004 looks more reasonable, though)
  TA_BOTTOM = $0008;
  TA_BASELINE = $0018;
  TA_RTLREADING = $0100;

  // Vertical text alignment flags
  // Used if font has vertical baseline, such as Kanji.
  VTA_TOP = $0000;
  VTA_RIGHT = $0000;
  VTA_BOTTOM = $0002;
  VTA_CENTER = $0006;  // why not $0004?
  VTA_BASELINE = $0018;

  // Ternary Raster Operations
  BLACKNESS = $00;
  NOTSRCERASE = $11;
  NOTSRCCOPY = $33;
  SRCERASE = $44;
  DSTINVERT = $55;
  SRCINVERT = $66;
  MERGEPAINT = $BB;
  MERGECOPY = $C0;
  SRCCOPY = $CC;
  SRCPAINT = $FF;
  PATCOPY = $F0;
  PATPAINT = $FB;
  WHITENESS = $FF;
  // ... plus many more...

  // Color usage
  DIB_RGB_COLORS = $0000;
  DIB_PAL_COLORS = $0001;
  DIB_PAL_INDICES = $0002;

  // Compression
  BI_RGB = $0000;
  BI_RLE8 = $0001;
  BI_RLE4 = $0002;
  BI_BITFIELDS = $0003;
  BI_JPEG = $0004;
  BI_PNG = $0005;
  BI_CMYK = $000B;
  BI_CMYKRLE8 = $000C;
  BI_CMYKRLE4 = $000D;

  // Arc direction (EMF)
  AD_COUNTERCLOCKWISE = $00000001;
  AD_CLOCKWISE = $00000002;
                  *)

function SameFont(Font1, Font2: TFont): Boolean;
begin
  Result := Font1.IsEqual(Font2);
end;

function SameBrush(Brush1, Brush2: TBrush): Boolean;
begin
  Result := Brush1.EqualsBrush(Brush2);
end;

function SamePen(Pen1, Pen2: TPen): Boolean;
begin
  Result := (Pen1.Style = Pen2.Style) and
            (Pen1.Width = Pen2.Width) and
            (Pen1.Color = Pen2.Color) and
            (Pen1.Cosmetic = Pen2.Cosmetic) and
            (Pen1.EndCap = Pen2.EndCap) and
            (Pen1.JoinStyle = Pen2.JoinStyle);
end;

{ TWMFWriter }

constructor TWMFWriter.Create;
begin
  inherited Create;
  FObjTable := TFPList.Create;
end;

destructor TWMFWriter.Destroy;
begin
  FObjTable.Free;  // Do not destroy the objects, they are owned by the image.
  inherited;
end;

{ Calculate the checksum of the PlaceableHeader (without the Checksum field) }
function TWMFWriter.CalcChecksum(P: PWord; ASize: Word): Word;
var
  n: Integer;
begin
  Result := 0;
  n := 0;
  while n < ASize do begin
    Result := Result xor P^;
    inc(P);
    inc(n, SIZE_OF_WORD);
  end;
end;

function TWMFWriter.AddToObjTable(AItem: TComponent): Integer;
begin
  Result := FObjTable.Add(AItem);
end;

function TWMFWriter.FindInObjTable(AItem: TComponent): Integer;
var
  i: Integer;
  item: TlmfObject;
begin
  for i := FObjTable.Count-1 downto 0 do  // or the other direction?
  begin
    item := TlmfObject(FObjTable[i]);
    if (item.ClassType = AItem.ClassType) then
    begin
      if (AItem is TlmfFont) and SameFont(TlmfFont(AItem).Font, TlmfFont(item).Font) then
      begin
        Result := i;
        exit;
      end else
      if (AItem is TlmfPen) and SamePen(tlmfPen(AItem).Pen, TlmfPen(item).Pen) then
      begin
        Result := i;
        exit;
      end else
      if (AItem is TlmfBrush) and SameBrush(tlmfBrush(AItem).Brush, TlmfBrush(item).Brush) then
      begin
        Result := i;
        exit;
      end;
    end;
  end;
  Result := -1;
// was:  Result := FObjTable.IndexOf(AItem);
end;

function TWMFWriter.MakeWMFColorRecord(AColor: TColor): TWMFColorRecord;
begin
  Result.ColorRED := Red(AColor);
  Result.ColorGREEN := Green(AColor);
  Result.ColorBLUE := Blue(AColor);
  Result.Reserved := 0;
end;

{ The META_EXTTEXTOUT function which is called by WriteWMFTextInRect is rather
  primitive: it ignores line-breaks and does not allow for word-wrapping.
  To implement them we break the text provided into lines and pass each line
  individually to WriteWMFTextInRect. }
procedure TWMFWriter.ProcessTextInRect(AStream: TStream; AItem: TObject);
var
  item: TlmfTextInRect;
  lineItem: TlmfTextInRect;
  L: TStringList;
  ts: TTextStyle;
  R: TRect;
  P: TPoint;
  i: Integer;
  s: String;
  ext: TSize;
  lineHeight, totalHeight: Integer;
  txtAlign: Word;
begin
  item := TlmfTextInRect(AItem);

  ts := item.TextStyle;
  ts.SingleLine := true;
  ts.Alignment := taLeftJustify;
  ts.Layout := tlTop;

  L := TStringList.Create;
  try
    L.TrailingLineBreak := false;
    if item.TextStyle.Wordbreak then
      WordWrap(FCurrFont, item.Text, item.Right-item.Left, L)
    else
    if item.TextStyle.SingleLine then
      L.Add(item.Text)
    else
      L.Text := item.Text;
    lineHeight := abs(FCurrFont.Height);
    totalHeight := lineHeight * L.Count;
    txtAlign := 0;
    case item.TextStyle.Layout of
      tlTop:
        begin
          P.Y := item.Top;
          txtAlign := txtAlign or TA_TOP;
        end;
      tlCenter:
        begin
          P.Y := (item.Top + item.Bottom - totalHeight) div 2;
          txtAlign := txtAlign or TA_TOP;
        end;
      tlBottom:
        begin
          P.Y := item.Bottom - totalHeight;
          txtAlign := txtAlign or TA_TOP;
        end;
    end;
    case item.TextStyle.Alignment of
      taLeftJustify:
        txtAlign := txtAlign or TA_LEFT;
      taCenter:
        txtAlign := txtAlign or TA_CENTER;
      taRightJustify:
        txtAlign := txtAlign or TA_RIGHT;
    end;
    //txtAlign := txtAlign or TA_UPDATECP;
    WriteTextAlign(AStream, txtAlign);
    for i := 0 to L.Count-1 do
    begin
      s := L[i];
      case item.TextStyle.Alignment of
        taLeftJustify: P.X := item.Left;
        taCenter: P.X := (item.Left + item.Right) div 2;
        taRightJustify: P.X := item.Right;
      end;
      R := Rect(item.Left, item.Top, item.Right, item.Bottom);
      lineItem := TlmfTextInRect.Create(R, P.X, P.Y, s, ts);
      WriteTextInRect(AStream, lineitem);
      lineItem.Free;
      inc(P.Y, lineHeight);
    end;
    WriteTextAlign(AStream, TA_LEFT or TA_TOP);  // Restore default
  finally
    L.Free;
  end;
end;

procedure TWMFWriter.WriteArc(AStream: TStream; AItem: TlmfArc);
var
  rec: TWMFArcRecord;
begin
  rec.Left := AItem.Left;
  rec.Top := AItem.Top;
  rec.Right := AItem.Right;
  rec.Bottom := AItem.Bottom;
  rec.XStartArc := AItem.StartPtX;
  rec.YStartArc := AItem.StartPtY;
  rec.XEndArc := AItem.EndPtX;
  rec.YEndArc := AItem.EndPtY;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_ARC, rec, SizeOf(TWMFArcRecord));
end;

procedure TWMFWriter.WriteBkColor(AStream: TStream; AColor: TColor);
var
  rec: TWMFColorRecord;
begin
  rec := MakeWMFColorRecord(AColor);
  WriteWMFRecord(AStream, META_SETBKCOLOR, rec, SizeOf(rec));
end;

procedure TWMFWriter.WriteBkColor(AStream: TStream; AItem: TlmfBkColor);
begin
  WriteBkColor(AStream, AItem.Color);
end;

procedure TWMFWriter.WriteBkMode(AStream: TStream; AMode: Word);
begin
  WriteWMFRecord(AStream, META_SETBKMODE, AMode, SizeOf(AMode));
end;

procedure TWMFWriter.WriteBkMode(AStream: TStream; AItem: TlmfBkMode);
begin
  WriteBkMode(AStream, AItem.Mode);
end;

procedure TWMFWriter.WriteBrush(AStream: TStream; AItem: TlmfBrush);
var
  rec: TWMFBrushRecord;
  idx: Integer;
  idxObj: Word;
begin
  idx := FindInObjTable(AItem);
  if idx = -1 then
  begin
    // Brush not found in object table --> create new brush
    rec := Default(TWMFBrushRecord);
    case AItem.Brush.Style of
      bsClear      : rec.Style := BS_NULL;
      bsSolid      : rec.Style := BS_SOLID;
      bsHorizontal : begin rec.Style := BS_HATCHED; rec.Hatch := HS_HORIZONTAL; end;
      bsVertical   : begin rec.Style := BS_HATCHED; rec.Hatch := HS_VERTICAL; end;
      bsFDiagonal  : begin rec.Style := BS_HATCHED; rec.Hatch := HS_FDIAGONAL; end;
      bsBDiagonal  : begin rec.Style := BS_HATCHED; rec.Hatch := HS_BDIAGONAL; end;
      bsCross      : begin rec.Style := BS_HATCHED; rec.Hatch := HS_CROSS; end;
      bsDiagCross  : begin rec.Style := BS_HATCHED; rec.Hatch := HS_DIAGCROSS; end;
      else           rec.Style := BS_SOLID;
    end;
    rec.ColorRED := Red(AItem.Brush.Color);
    rec.ColorGREEN := Green(AItem.Brush.Color);
    rec.ColorBLUE := Blue(AItem.Brush.Color);
    rec.Reserved := 0;
    idx := AddToObjTable(AItem);
    WriteWMFRecord(AStream, META_CREATEBRUSHINDIRECT, rec, SizeOf(rec));
  end;

  // Write the object table index of the brush to the SelectObject WMF record:
  idxObj := word(idx);
  WriteWMFRecord(AStream, META_SELECTOBJECT, idxObj, SizeOf(Word));

  // Store current brush for cases where brush must be changed temporarily
  FCurrBrush := AItem.Brush;
end;

procedure TWMFWriter.WriteChord(AStream: TStream; AItem: TlmfChord);
var
  rec: TWMFArcRecord;  // same structure for both arc, chord and pie
begin
  rec.Left := AItem.Left;
  rec.Top := AItem.Top;
  rec.Right := AItem.Right;
  rec.Bottom := AItem.Bottom;
  rec.XStartArc := AItem.StartPtX;
  rec.YStartArc := AItem.StartPtY;
  rec.XEndArc := AItem.EndPtX;
  rec.YEndArc := AItem.EndPtY;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_CHORD, rec, SizeOf(TWMFArcRecord));
end;

procedure TWMFWriter.WriteEllipse(AStream: TStream; AItem: TlmfEllipse);
var
  rec: TWMFRectRecord;
begin
  rec.Left := AItem.Left;
  rec.Top := AItem.Top;
  rec.Right := AItem.Right;
  rec.Bottom := AItem.Bottom;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_ELLIPSE, rec, SizeOf(TWMFRectRecord));
end;

procedure TWMFWriter.WriteEOF(AStream: TStream);
begin
  WriteWMFRecord(AStream, META_EOF, 0);
end;

{ NOTE: The flood fill records are not displayed correctly by Powerpoint and by
  LibreOffice Draw (correctly by Paint and IrfanView). }
procedure TWMFWriter.WriteExtFloodFill(AStream: TStream; AItem: TlmfFloodFill);
var
  rec: TWMFExtFloodFillRecord;
begin
  rec.Mode := 1 - ord(AItem.FillStyle);
  rec.ColorRED := Red(AItem.FillColor);
  rec.ColorGREEN := Green(AItem.FillColor);
  rec.ColorBLUE := Blue(AItem.FillColor);
  rec.XStart := AItem.px;
  rec.YStart := AItem.py;
  // WMF record header + parameters
  WriteWMFRecord(AStream, META_EXTFLOODFILL, rec, SizeOf(TWMFExtFloodFillRecord));
end;

procedure TWMFWriter.WriteFont(AStream: TStream; AItem: TlmfFont);
const
  ZERO_OR_ONE: array[boolean] of byte = (0, 1);
var
  rec: TWMFFontRecord;
  colorRec: TWMFColorRecord;
  fntName: String;
  idx, n: Integer;
  idxObj: Word;
begin
  idx := FindInObjTable(AItem);

  if idx = -1 then
  begin
    // Font not found in object table --> create a new font
    rec := Default(TWMFFontRecord);

    fntName := UTF8ToISO_8859_1(AItem.Font.Name) + #0;
    if odd(Length(fntName)) then
      fntName := fntName + #0;
    if Length(fntName) > 32 then
    begin
      SetLength(fntName, 32);
      fntName[32] := #0;
    end;

    rec.Height := abs(AItem.Font.Height);
    rec.Width := 0;
    rec.Orientation := AItem.Font.Orientation;
    rec.Escapement := AItem.Font.Orientation;
    rec.Weight := IfThen(fsBold in AItem.Font.Style, 700, 400);
    rec.Italic := ZERO_OR_ONE[fsItalic in AItem.Font.Style];
    rec.Underline := ZERO_OR_ONE[fsUnderline in AItem.Font.Style];
    rec.Strikeout := ZERO_OR_ONE[fsStrikeOut in AItem.Font.Style];
    rec.Charset := DEFAULT_CHARSET;
    rec.OutPrecision := 0;  // default
    rec.ClipPrecision := 0; // default
    rec.Quality := 0; // default
    rec.PitchAndFamily := 0;  // don't care / default
    Move(fntName[1], rec.FaceName[0], Length(fntName));
    // Write wmf record
    WriteWMFRecord(AStream, META_CREATEFONTINDIRECT, rec, SizeOf(TWMFFontRecord));
    idx := AddToObjTable(AItem);
  end;

  // Write the index of the font to the SelectObject WMF record:
  idxObj := word(idx);
  WriteWMFRecord(AStream, META_SELECTOBJECT, idxObj, SizeOf(Word));

  // Write text color
  colorRec := MakeWMFColorRecord(AItem.Font.Color);
  WriteWMFRecord(AStream, META_SETTEXTCOLOR, colorRec, SizeOf(colorRec));

  // Store font for text layout for TlmfTextInRect records
  FCurrFont := AItem.Font;
end;
                 (*
procedure TWMFWriter.WriteFrame3D(AStream: TStream; AItem: TlmfFrame3D);
var
  rec: TWMFRectRecord;
  penRec: TWMFPenRecord;
  currBrushIdx, currPenIdx: Integer;
  tmpItem: TlmfObject;
  itemIdx: Integer;
begin
  itemIdx := FImage.List.IndexOfComponent(AItem);

  currBrushIdx := FindInObjTable(FCurrBrush);
  currPenIdx := FindInObjTable(FCurrPen);

  // 1) We don't need the pen --> Style = psClear
  tmpItem := TlmfPen.Create(nil);
  tmpItem.Pen.Style := psClear;


  penRec := Default(TWMFPenRecord);
  penRec.Style := PS_NONE;
  WriteWMFRecord(AStream, META_CREATEPENINDIRECT, penRec, SizeOf(penRec));
  WriteWMFRecord(AStream,



  rec.Left := AItem.Left;
  rec.Top := AItem.Top;
  rec.Right := AItem.Right;
  rec.Bottom := AItem.Bottom;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_RECTANGLE, rec, SizeOf(TWMFRectRecord));
end;
*)

{ Extracts the mask from the input bitmap (ABitmap) as AMaskOnly.
  Applies the mask to itself and returns the result as AMaskedBitmap.
  Return value is false, when the input bitmap is not masked. }
function ExtractMask(ABitmap: TBitmap; out AMaskedBitmap, AMaskOnly: TBitmap): Boolean;
var
  img, mask: TLazIntfImage;
  x, y: Integer;
begin
  Result := false;
  AMaskedBitmap := nil;
  AMaskOnly := nil;

  if not ABitmap.RawImage.IsMasked(true) then
    exit;

  img := ABitmap.CreateIntfImage;
  mask := ABitmap.CreateIntfImage;
  try
    for y := 0 to img.Height-1 do
      for x := 0 to img.Width-1 do
        if img.Masked[x, y] then
        begin
          mask.Colors[x, y] := colWhite;
          img.Colors[x, y] := colBlack;
        end else
          mask.Colors[x, y] := colBlack;

    AMaskedBitmap := TBitmap.Create;
    AMaskedBitmap.LoadFromIntfImage(img);

    AMaskOnly := TBitmap.Create;
    AMaskOnly.LoadFromIntfImage(mask);

    Result := true;
  finally
    img.Free;
    mask.Free;
  end;
end;

procedure TWMFWriter.WriteBitmap(AStream: TStream; ABitmap: TBitmap;
  ARect: TRect; AOperation: Integer);
var
  rec: TWMFDIBStretchBltRecord;
  ms: TMemoryStream;
  dibImgSize: Int64;
  bmpFileHdr: TBitmapFileHeader;
begin
  if ABitmap = nil then
    exit;

  ms := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(ms);                   // Save bitmap to stream
    dibImgSize := ms.Size - SizeOf(bmpFileHdr); // = bmp info header + pixel data
    ms.Position := 0;                           // Rewind stream
    ms.Read(bmpFileHdr, SizeOf(bmpFileHdr));    // Jump over bmp file header
    // The memory stream now is at beginning of BitmapInfoHeader + PixelData

    rec.RasterOperation := AOperation;
    rec.SrcHeight := ABitmap.Height;
    rec.SrcWidth := ABitmap.Width;
    rec.SrcX := 0;
    rec.SrcY := 0;
    rec.DestHeight := ARect.Bottom - ARect.Top;
    rec.DestWidth := ARect.Right - ARect.Left;
    rec.DestY := ARect.Top;
    rec.DestX := ARect.Left;

    WriteWMFRecord(AStream, META_DIBSTRETCHBLT, SizeOf(TWMFDIBStretchBltRecord) + dibImgSize);
    AStream.Write(rec, SizeOf(TWMFDIBStretchBltRecord));
    // Write DIB
    AStream.CopyFrom(ms, dibImgSize);
  finally
    ms.Free;
  end;
end;

procedure TWMFWriter.WritePicture(AStream: TStream; AItem: TlmfPicture);
var
  bmp: TBitmap = nil;
  mask: TBitmap = nil;
begin
  if AItem.Picture.Bitmap.Masked then
  begin
    try
      ExtractMask(AItem.Picture.Bitmap, bmp, mask);
      WriteBitmap(AStream, mask, AItem.Clip, SRCAND);
      WriteBitmap(AStream, bmp, AItem.Clip, SRCPAINT);
    finally
      mask.Free;
      bmp.Free;
    end;
  end else
    WriteBitmap(AStream, AItem.Picture.Bitmap, AItem.Clip, SRCCOPY);
end;

procedure TWMFWriter.WriteLineTo(AStream: TStream; AItem: TlmfLineTo);
var
  rec: TWMFPointRecord;
begin
  rec.X := AItem.PX;
  rec.Y := AItem.PY;
  WriteWMFRecord(AStream, META_LINETO, rec, SizeOf(TWMFPointRecord));
end;

procedure TWMFWriter.WriteLine(AStream: TStream; AItem: TlmfLine);
var
  rec: TWMFLineRecord;
begin
  rec.NumPts := 2;
  rec.P1.X := AItem.PX;
  rec.P1.Y := AItem.PY;
  rec.P2.X := AItem.PX1;
  rec.P2.Y := AItem.PY1;
  WriteWMFRecord(AStream, META_POLYLINE, rec, SizeOf(TWMFLineRecord));
end;

procedure TWMFWriter.WriteMapMode(AStream: TStream; AMode: Word);
begin
  WriteWMFRecord(AStream, META_SETMAPMODE, AMode, SizeOf(AMode));
end;

procedure TWMFWriter.WriteMoveTo(AStream: TStream; AItem: TlmfMoveTo);
var
  rec: TWMFPointRecord;
begin
  rec.X := AItem.PX;
  rec.Y := AItem.PY;
  WriteWMFRecord(AStream, META_MOVETO, rec, SizeOf(TWMFPointRecord));
end;

procedure TWMFWriter.WritePen(AStream: TStream; AItem: TlmfPen);
var
  rec: TWMFPenRecord;
  idx: Integer;
  idxObj: Word;
begin
  // Searches the object list for the first usage of this pen and returns its index
  idx := FindInObjTable(AItem);

  // This pen is used here for the first time --> write a createpen record
  if idx = -1 then
  begin
    case AItem.Pen.Style of
      psDash       : rec.Style := PS_DASH;
      psDot        : rec.Style := PS_DOT;
      psDashDot    : rec.Style := PS_DASHDOT;
      psDashDotDot : rec.Style := PS_DASHDOTDOT;
      psClear      : rec.Style := PS_NULL;
      psInsideFrame: rec.Style := PS_INSIDEFRAME;
      else           rec.Style := PS_SOLID;
    end;
    if AItem.Pen.Cosmetic then
      rec.Style := rec.Style or PS_COSMETIC;
    case AItem.Pen.JoinStyle of
      pjsRound: rec.Style := rec.Style or PS_JOIN_ROUND;
      pjsBevel: rec.Style := rec.Style or PS_JOIN_BEVEL;
      pjsMiter: rec.Style := rec.Style or PS_JOIN_MITER;
    end;
    case AItem.Pen.EndCap of
      pecRound: rec.Style := rec.Style or PS_ENDCAP_ROUND;
      pecSquare: rec.Style := rec.Style or PS_ENDCAP_SQUARE;
      pecFlat: rec.Style := rec.Style or PS_ENDCAP_FLAT;
    end;
    rec.Width := AItem.Pen.Width;
    rec.Ignored1 := 0;
    rec.ColorRED := Red(AItem.Pen.Color);
    rec.ColorGREEN := Green(AItem.Pen.Color);
    rec.ColorBLUE := Blue(AItem.Pen.Color);
    rec.Ignored2 := 0;
    WriteWMFRecord(AStream, META_CREATEPENINDIRECT, rec, SizeOf(rec));
    idx := AddToObjTable(AItem);
  end;

  // Write the object table index of the pen to the SelectObject WMF record.
  idxObj := word(idx);
  WriteWMFRecord(AStream, META_SELECTOBJECT, idxObj, SizeOf(Word));

  // Store current pen for cases where pen must be changed temporarily
  FCurrPen := AItem.Pen;
end;

procedure TWMFWriter.WritePie(AStream: TStream; AItem: TlmfPie);
var
  rec: TWMFArcRecord;  // same structure for both arc, chord and pie
begin
  rec.Left := AItem.Left;
  rec.Top := AItem.Top;
  rec.Right := AItem.Right;
  rec.Bottom := AItem.Bottom;
  rec.XStartArc := AItem.StartPtX;
  rec.YStartArc := AItem.StartPtY;
  rec.XEndArc := AItem.EndPtX;
  rec.YEndArc := AItem.EndPtY;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_PIE, rec, SizeOf(TWMFArcRecord));
end;

procedure TWMFWriter.WritePolygon(AStream: TStream; AItem: TlmfPolygon);
var
  numPts: Word;
  recPts: packed array of TWMFPointXYRecord = nil;
  fillModeRec: TWMFSetPolyFillModeRecord;
  i: Integer;
begin
  numPts := Length(AItem.Points);
  SetLength(recPts, numPts);
  for i := 0 to numPts-1 do
  begin
    recPts[i].X := AItem.Points[i].X;
    recPts[i].Y := AItem.Points[i].Y;
  end;

  fillModeRec.PolyFillMode := IfThen(AItem.Winding, LCLType.WINDING, LCLType.ALTERNATE);
  fillModeRec.Reserved := 0;

  WriteWMFRecord(AStream, META_SETPOLYFILLMODE, fillModeRec, SizeOf(TWmfSetPolyFillModeRecord));
  WriteWMFRecord(AStream, META_POLYGON, SizeOf(word) + numPts * SizeOf(TWMFPointXYRecord));
  WriteWMFParams(AStream, numPts, SizeOf(Word));
  WriteWMFParams(AStream, recPts[0], numPts * SizeOf(TWMFPointXYRecord));
end;

procedure TWMFWriter.WritePolyLine(AStream: TStream; AItem: TlmfPolyLine);
var
  numPts: Word;
  recPts: packed array of TWMFPointXYRecord = nil;
  i: Integer;
begin
  numPts := Length(AItem.Points);
  SetLength(recPts, numPts);
  for i := 0 to numPts-1 do
  begin
    recPts[i].X := AItem.Points[i].X;
    recPts[i].Y := AItem.Points[i].Y;
  end;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_POLYLINE, SizeOf(word) + numPts * SizeOf(TWMFPointXYRecord));
  WriteWMFParams(AStream, numPts, SizeOf(Word));
  WriteWMFParams(AStream, recPts[0], numPts * SizeOf(TWMFPointXYRecord));
end;

procedure TWMFWriter.WriteRecords(AStream: TStream);
var
  i: Integer;
  item: TlmfObject;
begin
  // Setup defaults
  WriteWindowExt(AStream);
  WriteWindowOrg(AStream);
  WriteMapMode(AStream, ord(FImage.MapMode));  // MM_ANISOTROPIC);
  WriteBkColor(AStream, clWhite);
  WriteBkMode(AStream, TRANSPARENT);
  WriteTextAlign(AStream, TA_TOP or TA_LEFT);

  // Write object records of the drawing
  for i := 0 to FImage.List.ComponentCount-1 do
  begin
    item := TlmfObject(FImage.List.Components[i]);
    // most specialized objects at top, least specialized objects at bottom!
    if item is TlmfPicture then
      WritePicture(AStream, TlmfPicture(item))
    else
    if item is TlmfFloodFill then
      WriteExtFloodFill(AStream, TlmfFloodFill(item))
    else
    if item is TlmfPolygon then
      WritePolygon(AStream, TlmfPolygon(item))
    else
    if item is TlmfPolyLine then
      WritePolyLine(AStream, TlmfPolyline(item))
    else
    if item is TlmfChord then
      WriteChord(AStream, TlmfChord(item))
    else
    if item is TlmfPie then
      WritePie(AStream, TlmfPie(item))
    else
    if item is TlmfArc then
      WriteArc(AStream, TlmfArc(item))
    else
    if item is TlmfEllipse then
      WriteEllipse(AStream, TlmfEllipse(item))
    else
    if item is TlmfRoundRect then
      WriteRoundRect(AStream, TlmfRoundRect(item))
    else
    if item is TlmfRect then
      WriteRect(AStream, TlmfRect(item))
    else
    if item is TlmfBrush then
      WriteBrush(AStream, TlmfBrush(item))
    else
    if item is TlmfPen then
      WritePen(AStream, TlmfPen(item))
    else
    if item is TlmfFont then
      WriteFont(AStream, TlmfFont(item))
    else
    if item is TlmfMoveTo then
      WriteMoveTo(AStream, TlmfMoveto(item))
    else
    if item is TlmfLineTo then
      WriteLineTo(AStream, TlmfLineTo(item))
    else
    if item is TlmfLine then
      WriteLine(AStream, TlmfLine(item))
    else
    if item is TlmfTextInRect then
      ProcessTextInRect(AStream, item)
    else
    if item is TlmfText then
      WriteText(AStream, TlmfText(item))
    else
    if item is TlmfBkColor then
      WriteBkColor(AStream, TlmfBkColor(item))
    else
    if item is TlmfBkMode then
      WriteBkMode(AStream, TlmfBkMode(item));
  end;

  // Last record must be an EOF record.
  WriteEOF(AStream);
end;

procedure TWMFWriter.WriteRect(AStream: TStream; AItem: TlmfRect);
var
  rec: TWMFRectRecord;
begin
  rec.Left := AItem.Left;
  rec.Top := AItem.Top;
  rec.Right := AItem.Right;
  rec.Bottom := AItem.Bottom;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_RECTANGLE, rec, SizeOf(TWMFRectRecord));
end;

procedure TWMFWriter.WriteRoundRect(AStream: TStream; AItem: TlmfRoundRect);
var
  rec: TWMFRoundRectRecord;
begin
  rec.Left := AItem.Left;
  rec.Top := AItem.Top;
  rec.Right := AItem.Right;
  rec.Bottom := AItem.Bottom;
  rec.RX := AItem.Rx;
  rec.RY := AItem.Ry;

  // WMF record header + parameters
  WriteWMFRecord(AStream, META_ROUNDRECT, rec, SizeOf(TWMFRoundRectRecord));
end;

procedure TWMFWriter.WriteText(AStream: TStream; AItem: TlmfText);
// text record
//  - StringLength (word)
//  - String (variable length)
//  - YStart (word)
//  - XStart (word)
var
  ptRec: TWMFPointRecord;
  len: Word;
  s: String;
begin
  if AItem.Text = '' then
    exit;

  s := UTF8ToISO_8859_1(AItem.Text);
  len := Length(s);
  if odd(len) then     // String length must be even
  begin
    s := s + #0;
    inc(len);
  end;

  // Record header
  WriteWMFRecord(AStream, META_TEXTOUT, len + 3*SizeOf(word));
  // String length
  WriteWMFParams(AStream, len, SizeOf(word));
  // String
  WriteWMFParams(AStream, s[1], len);
  // String position
  ptRec.X := AItem.PX;
  ptRec.Y := AItem.PY;
  WriteWMFParams(AStream, ptRec, SizeOf(TWMFPointRecord));
end;

procedure TWMFWriter.WriteTextAlign(AStream: TStream; AValue: word);
begin
  WriteWMFRecord(AStream, META_SETTEXTALIGN, AValue, SizeOf(AValue));
end;

procedure TWMFWriter.WriteTextInRect(AStream: TStream; AItem: TlmfTextInRect);
var
  rec: TWMFExtTextOutRecord;
  R: packed array[0..3] of SmallInt;
  strLen, adjLen: Word;
  s: AnsiString;
  nR: Integer;
begin
  if AItem.Text = '' then
    exit;

  s := UTF8ToISO_8859_1(AItem.Text);
  strLen := Length(s);
  adjLen := strLen;
  if odd(strLen) then   // String length must be even
  begin
    s := s + #0;
    inc(adjLen);
  end;

  rec := Default(TWMFExtTextOutRecord);
  rec.X := AItem.PX;
  rec.Y := AItem.PY;
  rec.Len := strLen;
  if AItem.TextStyle.Opaque then rec.Options := rec.Options or ETO_OPAQUE;
  if AItem.TextStyle.Clipping then rec.Options := rec.Options or ETO_CLIPPED;
  if AItem.TextStyle.RightToLeft then rec.Options := rec.Options or ETO_RTLREADING;
  if (rec.Options and (ETO_OPAQUE or ETO_CLIPPED) <> 0) then
  begin
    // The entire rectangle is filled here. Note that this is in addition to
    // SetBkMode which fill only the background of the text itself.
    R[0] := AItem.Left;
    R[1] := AItem.Top;
    R[2] := AItem.Right;
    R[3] := AItem.Bottom;
    nR := SizeOf(R);
  end else
    nR := 0;

  WriteWMFRecord(AStream, META_EXTTEXTOUT, SizeOf(TWMFExtTextOutRecord) + nR + adjLen);
  WriteWMFParams(AStream, rec, SizeOf(TWMFExtTextOutRecord));
  if nr > 0 then
    WriteWMFParams(AStream, R, nR);
  WriteWMFParams(AStream, s[1], adjLen);
end;

procedure TWMFWriter.WriteToStream(AStream: TStream; AImage: TlmfImage);
var
  placeableHeader: TPlaceableMetaHeader;
  wmfHeader: TWMFHeader;
  startPos: Int64;
begin
  FImage := AImage;
  startPos := AStream.Position;

  FObjTable.Clear;

  // Write placeholder for WMF header and placeable header (because we don't
  // know FMaxRecordSize yet), will be rewritten with correct values later.
  placeableHeader := Default(TPlaceableMetaHeader);
  wmfHeader := Default(TWMFHeader);
  AStream.Write(placeableHeader, SizeOf(TPlaceableMetaHeader));
  AStream.Write(wmfHeader, SizeOf(TWMFHeader));

  // Write the records of the image
  WriteRecords(AStream);

  // Go back to the beginning of the file and write the headers. Use correct
  // header fields now.
  placeableHeader := Default(TPlaceableMetaHeader);
  with placeableHeader do begin
    Key := WMF_MAGIC_NUMBER;
    Handle := 0;
    Reserved := 0;
    Inch := FImage.LogUnitsPerInch;
    Left := 0;
    Top := 0;
    Right := FImage.Width;
    Bottom := FImage.Height;
    Checksum := CalcChecksum(@placeableHeader, SizeOf(TPlaceableMetaHeader));
  end;
  AStream.Position := startPos;
  AStream.WriteBuffer(placeableHeader, SizeOf(TPlaceableMetaHeader));

  wmfHeader := Default(TWMFHeader);
  with wmfHeader do begin
    FileType := 1;
    HeaderSize := 9;
    Version := $0300;
    NumOfObjects := FImage.List.ComponentCount;
    MaxRecordSize := FMaxRecordSize;
    FileSize := AStream.Size div SIZE_OF_WORD;
    NumOfParams := 0;
  end;
  AStream.WriteBuffer(wmfHeader, SizeOf(TWMFHeader));
end;

procedure TWMFWriter.WriteWindowExt(AStream: TStream);
var
  params: Array[0..1] of word;
begin
  params[0] := FImage.Height;  // Use negative value when y runs upwards.
  params[1] := FImage.Width;
  WriteWMFRecord(AStream, META_SETWINDOWEXT, params, SizeOf(params));
end;

procedure TWMFWriter.WriteWindowOrg(AStream: TStream);
var
  params: Array[0..1] of word;
begin
  params[0] := 0;
  params[1] := 0;
  WriteWMFRecord(AStream, META_SETWINDOWORG, params, Sizeof(params));
end;

{ Writes the WMF header (function code + total record size) only.
  Useful when the parameter block has variable size.
  ASize is the size of the following parameter block, in bytes }
procedure TWMFWriter.WriteWMFRecord(AStream: TStream;
  AFunc: Word; ASize: Integer);
var
  rec: TWMFRecord;
begin
  rec.Size := (SizeOf(TWMFRecord) + ASize) div SIZE_OF_WORD;
  rec.Func := AFunc;
  AStream.WriteBuffer(rec, SizeOf(TWMFRecord));
  FMaxRecordSize := Max(FMaxRecordSize, rec.Size);
end;

{ Write the WMF header (function code + total record size) and the
  parameters of the record.
  Intended for records having a fixed parameter block.
  ASize is the size of the parameter block, in bytes }
procedure TWMFWriter.WriteWMFRecord(AStream: TStream;
  AFunc: Word; const AParams; ASize: Integer);
var
  rec: TWMFRecord;
begin
  rec.Size := (SizeOf(TWMFRecord) + ASize) div SIZE_OF_WORD;
  rec.Func := AFunc;
  AStream.WriteBuffer(rec, SizeOf(TWMFRecord));
  AStream.WriteBuffer(AParams, ASize);
  FMaxRecordSize := Max(FMaxRecordSize, rec.Size);
end;

{ ASize is in bytes }
procedure TWMFWriter.WriteWMFParams(AStream: TStream;
  const AParams; ASize: Integer);
begin
  AStream.WriteBuffer(AParams, ASize);
end;


end.

