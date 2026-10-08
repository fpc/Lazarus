{ Writer for EMF files.

  A good description of the WMF (and EMF) file format is
    https://wvware.sourceforge.net/caolan/ora-wmf.html

  The official Microsoft documentation is at
    https://learn.microsoft.com/en-us/openspecs/windows_protocols/ms-wmf/4813e7fd-52d0-4f42-965f-228c8b7488d2
}

unit lmfEMFWrite;

{$mode objfpc}{$H+}
{.$define SUPPORT_DX}

interface

uses
  Classes, SysUtils, Math, Types, FPImage,
  GraphType, GraphUtil, Graphics, IntfGraphics, LCLType, LConvEncoding, Forms,
  lmf, lmfObj, lmfEMF;

type
  { TEMFWriter }

  TEMFWriter = class(TlmfWriter)
  private
    FImage: TlmfImage;
    FMaxRecordSize: Int64;
    FObjTable: TFPList;        // List with EMF objects (pen, brush, ...)
    FCurrBrush: TBrush;
    FCurrFont: TFont;
    FCurrPen: TPen;
    FCurrBkMode: DWord;
    FScaleX: Single;
    FScaleY: Single;
    ihBrush: Integer;
    ihPen: Integer;
    ihFont: Integer;

    // Specific EMF records
    procedure WriteArc(AStream: TStream; AItem: TlmfArc);
    procedure WriteBkColor(AStream: TStream; AColor: TColor);
    procedure WriteBkColor(AStream: TStream; AItem: TlmfBkColor);
    procedure WriteBkMode(AStream: TStream; AMode: DWord);
    procedure WriteBkMode(AStream: TStream; AItem: TlmfBkMode);
    procedure WriteBrush(AStream: TStream; AItem: TlmfBrush);
    procedure WriteChord(AStream: TStream; AItem: TlmfChord);
    procedure WriteDeleteObject(AStream: TStream; AIndex: Integer);
    procedure WriteEllipse(AStream: TStream; AItem: TlmfEllipse);
    procedure WriteEOF(AStream: TStream);
    procedure WriteExtFloodFill(AStream: TStream; AItem: TlmfFloodFill);
    procedure WriteFont(AStream: TStream; AItem: TlmfFont);
    procedure WriteGradientFill(AStream: TStream; AItem: TlmfGradientFill);
    procedure WriteHeader(AStream: TStream);
    procedure WriteLineTo(AStream: TStream; AItem: TlmfLineTo);
    procedure WriteLine(AStream: TStream; AItem: TlmfLine);
    procedure WriteMapMode(AStream: TStream; AMode: TlmfMapMode);
    procedure WriteMoveToEx(AStream: TStream; AItem: TlmfMoveTo);
    procedure WritePen(AStream: TStream; AItem: TlmfPen);
    procedure WritePen_UserPattern(AStream: TStream; AItem: TlmfPen);
    procedure WritePicture(AStream: TStream; AItem: TlmfPicture);
    procedure WritePie(AStream: TStream; AItem: TlmfPie);
    procedure WritePolyBezier(AStream: TStream; AItem: TlmfPolyBezier);
    procedure WritePolygon(AStream: TStream; AItem: TlmfPolygon);
    procedure WritePolyLine(AStream: TStream; AItem: TlmfPolyLine);
    procedure WriteRectangle(AStream: TStream; AItem: TlmfRect);
    procedure WriteRoundRect(AStream: TStream; AItem: TlmfRoundRect);
    procedure WriteSetViewportExtEx(AStream: TStream; AWidth, AHeight: Integer);
    procedure WriteSetWindowExtEx(AStream: TStream; AWidth, AHeight: Integer);
    procedure WriteStretchBLT(AStream: TStream; ABitmap: TBitmap; ARect: TRect; AOperation: Integer);
    procedure WriteText(AStream: TStream; AItem: TlmfText);
    procedure WriteTextAlign(AStream: TStream; AValue: DWord);
    procedure WriteTextInRect(AStream: TStream; AItem: TlmfTextInRect);
    (*
    procedure WriteWindowOrg(AStream: TStream);
    *)

    // misc
    procedure ProcessTextInRect(AStream: TStream; AItem: TObject);
  protected
    // General routines
    function AddToObjTable(AItem: TComponent): Integer;
//    function CalcChecksum(P: PWord; ASize: Word): Word;
//    procedure DeleteObjTable(AStream: TStream);
//    function FindInObjTable(AItem: TComponent): Integer;
    function LogUnitsToHundredthsMM(AValue: Integer): Integer;
    procedure PrepareObjTable;
    function MakeEMFColorRecord(AColor: TColor): TEMFColorRecord;

    // General EMF record writing
    procedure WriteRecords(AStream: TStream);
    procedure WriteEMFRecord(AStream: TStream; AFunc: word; ASize: Integer);
    procedure WriteEMFRecord(AStream: TStream; AFunc: Word; const AParams; ASize: Integer);
    procedure WriteEMFParams(AStream: TStream; const AParams; ASize: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure WriteToStream(AStream: TStream; AImage: TlmfImage); override;
  end;


implementation

uses
  bmpcomn;

const
  SIZE_OF_DWORD = 4;

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


{ TEMFWriter }

constructor TEMFWriter.Create;
begin
  inherited Create;
  FObjTable := TFPList.Create;
end;

destructor TEMFWriter.Destroy;
begin
  FObjTable.Free;  // Do not destroy the objects, they are owned by the image.
  inherited;
end;

function TEMFWriter.AddToObjTable(AItem: TComponent): Integer;
begin
  Result := FObjTable.Add(AItem);
end;

{
procedure TEMFWriter.DeleteObjTable(AStream: TStream);
var
  i: Integer;
begin
  for i := FObjTable.Count-1 downto 1 do   // keep index 0 (reserved)
    if FObjTable[i] <> nil then
    begin
      WriteDeleteObject(AStream, NToLE(i));
      FObjTable[i] := nil;
    end;
end;

function TEMFWriter.FindInObjTable(AItem: TComponent): Integer;
var
  i: Integer;
  item: TlmfObject;
begin
  for i := FObjTable.Count-1 downto 1 do  // 0 reserved!  // or the other direction?
  begin
    item := TlmfObject(FObjTable[i]);
    if (item <> nil) and (item.ClassType = AItem.ClassType) then
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
}

// Converts logical units to 0.01 mm
function TEMFWriter.LogUnitsToHundredthsMM(AValue: Integer): Integer;
begin
  Result := round(HUNDREDTHS_MM_PER_INCH * AValue / FImage.DevPixelsPerInch); //FImage.LogUnitsPerInch);
end;

function TEMFWriter.MakeEMFColorRecord(AColor: TColor): TEMFColorRecord;
begin
  Result.ColorRED := NtoLE(Red(AColor));
  Result.ColorGREEN := NtoLE(Green(AColor));
  Result.ColorBLUE := NtoLE(Blue(AColor));
  Result.Reserved := 0;
end;

{ The EMR_EXTTEXTOUT function which is called by WriteEMFTextInRect is rather
  primitive: it ignores line-breaks and does not allow for word-wrapping.
  To implement them we break the text provided into lines and pass each line
  individually to WriteEMFTextInRect. }
procedure TEMFWriter.ProcessTextInRect(AStream: TStream; AItem: TObject);
var
  item: TlmfTextInRect;
  lineItem: TlmfTextInRect;
  L: TStringList;
  ts: TTextStyle;
  R: TRect;
  P: TPoint;
  i: Integer;
  s: String;
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

procedure TEMFWriter.PrepareObjTable;
begin
  FObjTable.Clear;
  FObjTable.Add(nil);  // Index 0 is reserved
  ihBrush := -1;
  ihPen := -1;
  ihFont := -1;
end;

procedure TEMFWriter.WriteArc(AStream: TStream; AItem: TlmfArc);
var
  rec: TEMFArcRecord;
begin
  rec.Box.Left := NToLE(AItem.Left);
  rec.Box.Top := NToLE(AItem.Top);
  rec.Box.Right := NToLE(AItem.Right);
  rec.Box.Bottom := NToLE(AItem.Bottom);
  rec.StartPt.X := NToLE(AItem.StartPtX);
  rec.StartPt.Y := NToLE(AItem.StartPtY);
  rec.EndPt.X := NToLE(AItem.EndPtX);
  rec.EndPt.Y := NToLE(AItem.EndPtY);

  // EMF record header + parameters
  WriteEMFRecord(AStream, EMR_ARC, rec, SizeOf(TEMFArcRecord));
end;

procedure TEMFWriter.WriteBkColor(AStream: TStream; AColor: TColor);
var
  rec: TEMFColorRecord;
begin
  rec := MakeEMFColorRecord(AColor);
  WriteEMFRecord(AStream, EMR_SETBKCOLOR, rec, SizeOf(rec));
end;

procedure TEMFWriter.WriteBkColor(AStream: TStream; AItem: TlmfBkColor);
begin
  WriteBkColor(AStream, NToLE(AItem.Color));
end;

procedure TEMFWriter.WriteBkMode(AStream: TStream; AMode: DWord);
begin
  WriteEMFRecord(AStream, EMR_SETBKMODE, NToLE(AMode), SIZE_OF_DWORD);
  FCurrBkMode := AMode;
end;

procedure TEMFWriter.WriteBkMode(AStream: TStream; AItem: TlmfBkMode);
begin
  WriteBkMode(AStream, AItem.Mode);
end;

procedure TEMFWriter.WriteBrush(AStream: TStream; AItem: TlmfBrush);
var
  rec: TEMFBrushRecord;
  style, hatch: DWord;
begin
  if ihBrush = -1 then
    ihBrush := FObjTable.Add(nil);

  if FObjTable[ihBrush] <> nil then
    WriteDeleteObject(AStream, ihBrush);

  rec := Default(TEMFBrushRecord);
  hatch := 0;
  case AItem.Brush.Style of
    bsClear      : style := BS_NULL;
    bsSolid      : style := BS_SOLID;
    bsHorizontal : begin style := BS_HATCHED; hatch := HS_HORIZONTAL; end;
    bsVertical   : begin style := BS_HATCHED; hatch := HS_VERTICAL; end;
    bsFDiagonal  : begin style := BS_HATCHED; hatch := HS_FDIAGONAL; end;
    bsBDiagonal  : begin style := BS_HATCHED; hatch := HS_BDIAGONAL; end;
    bsCross      : begin style := BS_HATCHED; hatch := HS_CROSS; end;
    bsDiagCross  : begin style := BS_HATCHED; hatch := HS_DIAGCROSS; end;
    else           style := BS_SOLID;
  end;
  rec.BrushStyle := NtoLE(style);
  rec.BrushHatch := NtoLE(hatch);
  rec.ColorRED := Red(AItem.Brush.Color);
  rec.ColorGREEN := Green(AItem.Brush.Color);
  rec.ColorBLUE := Blue(AItem.Brush.Color);
  rec.Reserved := 0;

  // Write the brush record
  WriteEMFRecord(AStream, EMR_CREATEBRUSHINDIRECT, SizeOf(ihBrush) + SizeOf(rec));
  AStream.WriteDWord(NtoLE(ihBrush));
  AStream.WriteBuffer(rec, SizeOf(rec));

  // Write the object table index of the brush to the SelectObject record:
  WriteEMFRecord(AStream, EMR_SELECTOBJECT, NtoLE(ihBrush), SIZE_OF_DWORD);

  // Store current brush for cases where brush must be changed temporarily
  FCurrBrush := AItem.Brush;
end;

procedure TEMFWriter.WriteChord(AStream: TStream; AItem: TlmfChord);
var
  rec: TEMFArcRecord;  // same structure for arc, chord and pie
begin
  rec.Box.Left := NToLE(AItem.Left);
  rec.Box.Top := NToLE(AItem.Top);
  rec.Box.Right := NToLE(AItem.Right);
  rec.Box.Bottom := NToLE(AItem.Bottom);
  rec.StartPt.X := NToLE(AItem.StartPtX);
  rec.StartPt.Y := NToLE(AItem.StartPtY);
  rec.EndPt.X := NToLE(AItem.EndPtX);
  rec.EndPt.Y := NToLE(AItem.EndPtY);

  // EMF record header + parameters
  WriteEMFRecord(AStream, EMR_CHORD, rec, SizeOf(TEMFArcRecord));
end;

procedure TEMFWriter.WriteDeleteObject(AStream: TStream; AIndex: Integer);
var
  objIndex: DWord;
begin
  objIndex := NtoLE(AIndex);
  WriteEMFRecord(AStream, EMR_DELETEOBJECT, objIndex, SIZE_OF_DWORD);
end;

procedure TEMFWriter.WriteEllipse(AStream: TStream; AItem: TlmfEllipse);
var
  rec: TEMFRectLRecord;
begin
  rec.Left := NToLE(AItem.Left);
  rec.Top := NToLE(AItem.Top);
  rec.Right := NToLE(AItem.Right - 1);   // -1 because of inclusive-inclusive rect coordinates
  rec.Bottom := NToLE(AItem.Bottom - 1);

  // EMF record header + parameters
  WriteEMFRecord(AStream, EMR_ELLIPSE, rec, SizeOf(TEMFRectLRecord));
end;

{ Assuming that there is no palette! }
procedure TEMFWriter.WriteEOF(AStream: TStream);
var
  rec: TEMFEndOfFileRecord;
  offToEOF: DWord;
begin
  rec := Default(TEMFEndOfFileRecord);
  rec.NumPalEntries := 0;
  rec.OffPalEntries := 0;
  WriteEMFRecord(AStream, EMR_EOF, rec, SizeOf(TEMFEndOfFileRecord) + SizeOf(DWord));
  offToEOF := SizeOf(TEMFRecord) + SizeOf(TEMFEndOfFileRecord) + SizeOf(DWord);
  AStream.WriteDWord(NToLE(offToEOF));
end;

{ NOTE: The flood fill records are not displayed correctly by Powerpoint and by
  LibreOffice Draw (correctly by Paint and IrfanView). }
procedure TEMFWriter.WriteExtFloodFill(AStream: TStream; AItem: TlmfFloodFill);
var
  rec: TEMFExtFloodFillRecord;
  clr: TColor;
begin
  rec.StartPt.X := NToLE(AItem.px);
  rec.StartPt.Y := NToLE(AItem.py);
  clr := ColorToRGB(AItem.FillColor);
  rec.Color.ColorRED := Red(clr);
  rec.Color.ColorGREEN := Green(clr);
  rec.Color.ColorBLUE := Blue(clr);
  rec.Color.Reserved := 0;
  rec.Mode := NToLE(1 - ord(AItem.FillStyle));

  // EMF record header + parameters
  WriteEMFRecord(AStream, EMR_EXTFLOODFILL, rec, SizeOf(TEMFExtFloodFillRecord));
end;

procedure TEMFWriter.WriteFont(AStream: TStream; AItem: TlmfFont);
const
  ZERO_OR_ONE: array[boolean] of byte = (0, 1);
var
  fntRec: TEMFLogFontExRecord;
  dvRec: TEMFDesignVectorRecord;
  colorRec: TEMFColorRecord;
  fntName: WideString;
  n: Integer;
begin
  if ihFont = -1 then
    ihFont := FObjTable.Add(nil);

  if FObjTable[ihFont] <> nil then
    WriteDeleteObject(AStream, ihFont);

  fntrec := Default(TEMFLogFontExRecord);
  fntrec.LogFont.Height := NToLE(AItem.Font.Height);
  fntrec.LogFont.Width := 0;
  fntrec.LogFont.Escapement := NtoLE(AItem.Font.Orientation);
  fntrec.Logfont.Orientation := NtoLE(AItem.Font.Orientation);
  fntrec.LogFont.Weight := NToLE(IfThen(fsBold in AItem.Font.Style, 700, 400));
  fntrec.LogFont.Italic := NToLE(ZERO_OR_ONE[fsItalic in AItem.Font.Style]);
  fntrec.LogFont.Underline := NToLE(ZERO_OR_ONE[fsUnderline in AItem.Font.Style]);
  fntrec.LogFont.Strikeout := NToLE(ZERO_OR_ONE[fsStrikeOut in AItem.Font.Style]);
  fntrec.LogFont.Charset := NToLE(DEFAULT_CHARSET);
  fntrec.LogFont.OutPrecision := 0;  // default
  fntrec.LogFont.ClipPrecision := 0; // default
  fntrec.LogFont.Quality := 0; // default
  fntrec.LogFont.PitchAndFamily := 0;  // don't care / default
  fntName := WideString(AItem.Font.Name);
  n := Length(fntName);
  if n >= 32 then
    n := 32
  else
    inc(n);
  Move(fntName[1], fntrec.LogFont.FaceName[0], n * SizeOf(WideChar));
  n := Length(fntName);
  Move(fntName[1], fntrec.FullName[0], n*SizeOf(WideChar));
  fntrec.Style[0] := #0;
  fntrec.Script[0] := #0;

  dvrec.Signature := $08007664;
  dvrec.NumAxes := 0;

  // Write emf record
  WriteEMFRecord(AStream, EMR_EXTCREATEFONTINDIRECTW,
    SizeOf(ihFont) + SizeOf(TEMFLogFontExRecord) + SizeOf(TEMFDesignVectorRecord));
  AStream.Write(ihFont, SizeOf(ihFont));
  AStream.Write(fntRec, SizeOf(TEMFLogFontExRecord));
  AStream.Write(dvRec, SizeOf(TEMFDesignVectorRecord));

  // Write the index of the font to the SelectObject EMF record:
  WriteEMFRecord(AStream, EMR_SELECTOBJECT, NToLE(ihFont), SIZE_OF_DWORD);

  // Write text color
  colorRec := MakeEMFColorRecord(AItem.Font.Color);
  WriteEMFRecord(AStream, EMR_SETTEXTCOLOR, colorRec, SizeOf(colorRec));

  // Store font
  FCurrFont := AItem.Font;
end;

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

procedure TEMFWriter.WriteGradientFill(AStream: TStream; AItem: TlmfGradientFill);
const
  dir: array[TGradientDirection] of DWord = (GRADIENT_FILL_RECT_V, GRADIENT_FILL_RECT_H);
var
  rec: TEMFGradientFillRecord;
  vertices: array[0..1] of TTriVertex;
  mesh: array[0..0] of TGradientRect;
  padding: array[0..0] of DWord;
  clr: TFPColor;
begin
  // Like TCanvas, we support here only filling of a single rectangle

  // Upper/left corner point
  vertices[0].x := NtoLE(AItem.Left);
  vertices[0].y := NtoLE(AItem.Top);
  clr := TColorToFPColor(AItem.StartColor);
  vertices[0].Red := clr.Red;
  vertices[0].Green := clr.Green;
  vertices[0].Blue := clr.Blue;
  vertices[0].Alpha := clr.Alpha;

  // Lower/right corner point
  vertices[1].x := NtoLE(AItem.Right);
  vertices[1].y := NtoLE(AItem.Bottom);
  clr := TColorToFPColor(AItem.EndColor);
  vertices[1].Red := clr.Red;
  vertices[1].Green := clr.Green;
  vertices[1].Blue := clr.Blue;
  vertices[1].Alpha := clr.Alpha;

  // Assignment of vertex indices
  mesh[0].UpperLeft := 0;
  mesh[0].LowerRight := 1;

  // Padding
  padding[0] := 0;

  // Core of the EMF gradientfill record
  rec := Default(TEMFGradientFillRecord);
  rec.BoundsLeft := NToLE(AItem.Left);
  rec.BoundsTop := NToLE(AItem.Top);
  rec.BoundsRight := NToLE(AItem.Right);    // -1 because of inclusive-inclusive rect ???
  rec.BoundsBottom := NToLE(AItem.Bottom);
  rec.nVer := NtoLE(2);   // 2 corner points
  rec.nTri := NtoLE(1);   // 1 rectangle
  rec.FillMode := NtoLE(dir[AItem.Direction]);

  // Write EMF record
  WriteEMFRecord(AStream, EMR_GRADIENTFILL,
    SizeOf(TEMFGradientFillRecord) + SizeOf(vertices) + SizeOf(mesh) + SizeOf(padding)
  );
  AStream.Write(rec, SizeOf(TEMFGradientFillRecord));
  AStream.Write(vertices, SizeOf(vertices));
  AStream.Write(mesh, SizeOf(mesh));
  AStream.Write(padding, SizeOf(padding));
end;


// to do: use +1 or -1 for inclusive right/bottom coordinates of rectangles?
procedure TEMFWriter.WriteHeader(AStream: TStream);
var
  header: TEnhancedMetaHeader;
  monitor: TMonitor;
begin
  header := Default(TEnhancedMetaHeader);
  header.RecordType := EMR_HEADER;
  header.RecordSize := NtoLE(Sizeof(TEnhancedMetaHeader));
  header.BoundsLeft := NtoLE(FImage.LogOriginX);
  header.BoundsTop := NtoLE(FImage.LogOriginY);
  header.BoundsRight := NtoLE(FImage.LogOriginX + FImage.Width - 1);
  header.BoundsBottom := NtoLE(FImage.LogOriginY + FImage.Height - 1);
  header.FrameLeft := NtoLE(LogUnitsToHundredthsMM(FImage.LogOriginX));
  header.FrameTop := NtoLE(LogUnitsToHundredthsMM(FImage.LogOriginY));
  header.FrameRight := NtoLE(LogUnitsToHundredthsMM(FImage.LogOriginX + FImage.Width));
  header.FrameBottom := NtoLE(LogUnitsToHundredthsMM(FImage.LogOriginY + FImage.Height));
  header.Signature := $464D4520;  // 'EMF '
  header.Version := NtoLE($00010000);
  header.Size := NtoLE(AStream.Size);
  header.NumOfRecords := NtoLE(FImage.List.ComponentCount);
  header.NumOfHandles := FObjTable.Count;
  header.Reserved := 0;
  header.SizeOfDescrip := 0;  // todo: allow description
  header.OffsOfDescrip := 0;
  header.NumPalEntries := 0;  // todo: allow palette
  monitor := Screen.PrimaryMonitor;
  header.WidthDevPixels := monitor.Width; //round(FImage.Width / FImage.LogUnitsPerInch * FImage.DevPixelsPerInch);
  header.HeightDevPixels := monitor.Height; //round(FImage.Height / FImage.LogUnitsPerInch * FImage.DevPixelsPerInch);
  header.WidthDevMM := round(monitor.Width/monitor.PixelsPerInch*25.4); //Pround(FImage.Width / FImage.LogUnitsPerInch * 25.4);
  header.HeightDevMM := round(monitor.Height/monitor.PixelsPerInch*25.4); //round(FImage.Height / FImage.LogUnitsPerInch * 25.4);
  AStream.Write(header, SizeOf(header));

  FScaleX := (header.FrameRight - header.FrameLeft) / FImage.Width;
  FScaleY := (header.FrameBottom - header.FrameTop) / FImage.Height;
end;

procedure TEMFWriter.WriteLineTo(AStream: TStream; AItem: TlmfLineTo);
var
  rec: TEMFPointLRecord;
begin
  rec.X := NToLE(AItem.PX);
  rec.Y := NToLE(AItem.PY);
  WriteEMFRecord(AStream, EMR_LINETO, rec, SizeOf(TEMFPointLRecord));
end;

procedure TEMFWriter.WriteLine(AStream: TStream; AItem: TlmfLine);
var
  rec: TEMFPolyLineRecord;
  pts: array[0..1] of TEMFPointLRecord;
begin
  rec.BoundsLeft := 0;
  rec.BoundsTop := 0;
  rec.BoundsRight := NtoLE(-1);
  rec.BoundsBottom := NtoLE(-1);
  rec.NumPts := 2;
  pts[0].X := NToLE(AItem.PX);
  pts[0].Y := NToLE(AItem.PY);
  pts[1].X := NToLE(AItem.PX1);
  pts[1].Y := NToLE(AItem.PY1);
  WriteEMFRecord(AStream, EMR_POLYLINE, SizeOf(TEMFPolyLineRecord) + rec.NumPts * SizeOf(TEMFPointLRecord));
  AStream.Write(rec, SizeOf(TEMFPolyLineRecord));
  AStream.Write(pts[0], rec.NumPts * SizeOf(TEMFPointLRecord));
end;

procedure TEMFWriter.WriteMapMode(AStream: TStream; AMode: TlmfMapMode);
var
  mode: DWord;
begin
  if AMode <> mmLogUnitsPerInch then
    mode := ord(AMode)
  else
    mode := ord(mmAnisotropic);
  WriteEMFRecord(AStream, EMR_SETMAPMODE, NToLE(mode), SizeOf(mode));
end;

procedure TEMFWriter.WriteMoveToEx(AStream: TStream; AItem: TlmfMoveTo);
var
  rec: TEMFPointLRecord;
begin
  rec.X := NToLE(AItem.PX);
  rec.Y := NToLE(AItem.PY);
  WriteEMFRecord(AStream, EMR_MOVETOEX, rec, SizeOf(TEMFPointLRecord));
end;

procedure TEMFWriter.WritePen(AStream: TStream; AItem: TlmfPen);
var
  rec: TEMFLogPenRecord;
  style: DWord;
begin
  if ihPen = -1 then
    ihPen := FObjTable.Add(nil);

//  if FObjTable[ihPen] <> nil then
    WriteDeleteObject(AStream, ihPen);

  case AItem.Pen.Style of
    psSolid      : style := PS_SOLID;
    psDash       : style := PS_DASH;
    psDot        : style := PS_DOT;
    psDashDot    : style := PS_DASHDOT;
    psDashDotDot : style := PS_DASHDOTDOT;
    psClear      : style := PS_NULL;
    psInsideFrame: style := PS_INSIDEFRAME;
    psPattern    : style := PS_USERSTYLE;
    else           style := PS_SOLID;
  end;

  if AItem.Pen.Cosmetic then
    style := style or PS_COSMETIC;

  case AItem.Pen.JoinStyle of
    pjsRound: style := style or PS_JOIN_ROUND;
    pjsBevel: style := style or PS_JOIN_BEVEL;
    pjsMiter: style := style or PS_JOIN_MITER;
  end;

  case AItem.Pen.EndCap of
    pecRound: style := style or PS_ENDCAP_ROUND;
    pecSquare: style := style or PS_ENDCAP_SQUARE;
    pecFlat: style := style or PS_ENDCAP_FLAT;
  end;

  rec := Default(TEMFLogPenRecord);
  rec.PenStyle := NToLE(style);
  rec.Width := NToLE(AItem.Pen.Width);
  rec.ColorRED := Red(AItem.Pen.Color);
  rec.ColorGREEN := Green(AItem.Pen.Color);
  rec.ColorBLUE := Blue(AItem.Pen.Color);

  // Write the emf CreatePen record (plus ihPen)
  WriteEMFRecord(AStream, EMR_CREATEPEN, SizeOf(ihPen) + SizeOf(rec));
  AStream.WriteDWord(NtoLE(ihPen));
  AStream.Write(rec, SizeOf(rec));

  // Write the object table index of the pen to the SelectObject WMF record.
  WriteEMFRecord(AStream, EMR_SELECTOBJECT, NtoLE(ihPen), SIZE_OF_DWORD);

  // Store current pen for cases where pen must be changed temporarily
  FCurrPen := AItem.Pen;
end;

procedure TEMFWriter.WritePen_UserPattern(AStream: TStream; AItem: TlmfPen);
var
  rec: TEMFExtCreatePenRecord;
  style: DWord;
  pattern: TPenPattern = nil;
  sizeOfPattern: DWord;
  sizeOfBmpData: DWord;
begin
  if ihPen = -1 then
    ihPen := FObjTable.Add(nil);

  if FObjTable[ihPen] <> nil then
    WriteDeleteObject(AStream, ihPen);

  style := 0;
  case AItem.Pen.Style of
    psSolid      : style := PS_SOLID;
    psDash       : style := PS_DASH;
    psDot        : style := PS_DOT;
    psDashDot    : style := PS_DASHDOT;
    psDashDotDot : style := PS_DASHDOTDOT;
    psClear      : style := PS_NULL;
    psInsideFrame: style := PS_INSIDEFRAME;
    psPattern    : style := PS_USERSTYLE;
    else           style := PS_SOLID;
  end;
  if AItem.Pen.Cosmetic then
    style := style or PS_COSMETIC
  else
    style := style or PS_GEOMETRIC;
  case AItem.Pen.JoinStyle of
    pjsRound: style := style or PS_JOIN_ROUND;
    pjsBevel: style := style or PS_JOIN_BEVEL;
    pjsMiter: style := style or PS_JOIN_MITER;
  end;
  case AItem.Pen.EndCap of
    pecRound: style := style or PS_ENDCAP_ROUND;
    pecSquare: style := style or PS_ENDCAP_SQUARE;
    pecFlat: style := style or PS_ENDCAP_FLAT;
  end;

  rec := Default(TEMFExtCreatePenRecord);
  rec.offBmi := 0;   // Currently no support of pen bitmap
  rec.cbBmi := 0;
  rec.offBits := 0;
  rec.cbBits := 0;
  rec.Pen.PenStyle := NToLE(style);
  rec.Pen.Width := NToLE(AItem.Pen.Width);
  rec.Pen.ColorRED := Red(AItem.Pen.Color);
  rec.Pen.ColorGREEN := Green(AItem.Pen.Color);
  rec.Pen.ColorBLUE := Blue(AItem.Pen.Color);
  rec.Pen.BrushStyle := BS_SOLID;
  rec.Pen.BrushHatch := 0;
  if style <> PS_USERSTYLE then
  begin
    rec.Pen.NumStyleEntries := 0;
    sizeOfPattern := 0;
  end else
  begin
    pattern := AItem.Pen.GetPattern;
    rec.Pen.NumStyleEntries := Length(pattern);
    sizeOfPattern := Length(pattern) * SIZE_OF_DWORD;
  end;

  // Write the emf ExtCreatePen record (plus ihPen and pattern)
  WriteEMFRecord(AStream, EMR_EXTCREATEPEN, SizeOf(ihPen) + SizeOf(rec) + sizeOfPattern);
  AStream.WriteDWord(NtoLE(ihPen));
  AStream.Write(rec, SizeOf(rec));
  if style = PS_USERSTYLE then
    AStream.Write(pattern[0], sizeOfPattern);

  // Write the object table index of the pen to the SelectObject WMF record.
  WriteEMFRecord(AStream, EMR_SELECTOBJECT, NtoLE(ihPen), SIZE_OF_DWORD);

  // Store current pen for cases where pen must be changed temporarily
  FCurrPen := AItem.Pen;
end;

procedure TEMFWriter.WritePicture(AStream: TStream; AItem: TlmfPicture);
var
  bmp: TBitmap = nil;
  mask: TBitmap = nil;
begin
  if AItem.Picture.Bitmap.Masked then
  begin
    try
      ExtractMask(AItem.Picture.Bitmap, bmp, mask);
      WriteStretchBLT(AStream, mask, AItem.Clip, SRCAND);
      WriteStretchBLT(AStream, bmp, AItem.Clip, SRCPAINT);
    finally
      mask.Free;
      bmp.Free;
    end;
  end else
    WriteStretchBLT(AStream, AItem.Picture.Bitmap, AItem.Clip, SRCCOPY);
end;

procedure TEMFWriter.WritePie(AStream: TStream; AItem: TlmfPie);
var
  rec: TEMFArcRecord;  // same structure for arc, chord and pie
begin
  rec.Box.Left := NToLE(AItem.Left);
  rec.Box.Top := NToLE(AItem.Top);
  rec.Box.Right := NToLE(AItem.Right);
  rec.Box.Bottom := NToLE(AItem.Bottom);
  rec.StartPt.X := NToLE(AItem.StartPtX);
  rec.StartPt.Y := NToLE(AItem.StartPtY);
  rec.EndPt.X := NToLE(AItem.EndPtX);
  rec.EndPt.Y := NToLE(AItem.EndPtY);

  // EMF record header + parameters
  WriteEMFRecord(AStream, EMR_PIE, rec, SizeOf(TEMFArcRecord));
end;

procedure TEMFWriter.WritePolyBezier(AStream: TStream; AItem: TlmfPolyBezier);
var
  numPts: Word;
  rec: TEMFPolyLineRecord;
  recPts: packed array of TEMFPointLRecord = nil;
  i: Integer;
  recID: DWord;
  bounds: TRect;

  procedure AddPoint(idx: Integer; X, Y: Integer);
  begin
    recPts[idx].X := NtoLE(X);
    recPts[idx].Y := NtoLE(Y);
    bounds.Left := Min(bounds.Left, X);
    bounds.Top := Min(bounds.Top, Y);
    bounds.Right := Max(bounds.Right, X);
    bounds.Bottom := Max(bounds.Bottom, Y);
  end;

begin
  numPts := Length(AItem.Points);
  if AItem.StartsAtPenPos then
  begin
    if numPts mod 3 <> 0 then
      raise ElmfWriter.Create('Incorrect number of PolyBezier points');
    recID := EMR_POLYBEZIERTO
  end else
  begin
    if (numPts - 1) mod 3 <> 0 then
      raise ElmfWriter.Create('Incorrect number of PolyBezier points');
    recID := EMR_POLYBEZIER;
  end;

  bounds := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);

  SetLength(recPts, numPts);
  for i := 0 to numPts-1 do
  begin
    recPts[i].X := NToLE(AItem.Points[i].X);
    recPts[i].Y := NToLE(AItem.Points[i].Y);
    bounds.Left := Min(bounds.Left, AItem.Points[i].X);
    bounds.Top := Min(bounds.Top, AItem.Points[i].Y);
    bounds.Right := Max(bounds.Right, AItem.Points[i].X);
    bounds.Bottom := Max(bounds.Bottom, AItem.Points[i].Y);
  end;

  rec.BoundsLeft := NtoLE(bounds.Left);
  rec.BoundsTop := NtoLE(bounds.Top);
  rec.BoundsRight := NtoLE(bounds.Right);
  rec.BoundsBottom := NtoLE(bounds.bottom);
  rec.NumPts := NtoLE(numPts);

  // EMF record header + parameters
  WriteEMFRecord(AStream, recID, SizeOf(TEMFPolyLineRecord) + numPts * SizeOf(TEMFPointLRecord));
  WriteEMFParams(AStream, rec, SizeOf(TEMFPolyLinerecord));
  WriteEMFParams(AStream, recPts[0], numPts * SizeOf(TEMFPointLRecord));
end;

procedure TEMFWriter.WritePolygon(AStream: TStream; AItem: TlmfPolygon);
var
  numPts: DWord;
  rec: TEMFPolyLineRecord;
  recPts: packed array of TEMFPointLRecord = nil;
  bounds: TRect;
  polyFillMode: DWord;
  i: Integer;
begin
  // Write PolyFillMode flag
  polyfillMode := NToLE(IfThen(AItem.Winding, LCLType.WINDING, LCLType.ALTERNATE));
  WriteEMFRecord(AStream, EMR_SETPOLYFILLMODE, polyFillMode, SIZE_OF_DWORD);

  // Prepare polygon record
  numPts := Length(AItem.Points);
  SetLength(recPts, numPts);
  bounds := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);
  for i := 0 to numPts-1 do
  begin
    recPts[i].X := NToLE(AItem.Points[i].X);
    recPts[i].Y := NToLE(AItem.Points[i].Y);
    bounds.Left := Min(bounds.Left, AItem.Points[i].X);
    bounds.Top := Min(bounds.Top, AItem.Points[i].Y);
    bounds.Right := Max(bounds.Right, AItem.Points[i].X);
    bounds.Bottom := Max(bounds.Bottom, AItem.Points[i].Y);
  end;

  rec.BoundsLeft := NtoLE(bounds.Left);
  rec.BoundsTop := NtoLE(bounds.Top);
  rec.BoundsRight := NtoLE(bounds.Right);
  rec.BoundsBottom := NtoLE(bounds.bottom);
  rec.NumPts := NtoLE(numPts);

  // Write EMF header + polygon record
  WriteEMFRecord(AStream, EMR_POLYGON, SizeOf(TEMFPolyLineRecord) + numPts * SizeOf(TEMFPointLRecord));
  WriteEMFParams(AStream, rec, SizeOf(TEMFPolyLineRecord));
  WriteEMFParams(AStream, recPts[0], numPts * SizeOf(TEMFPointLRecord));
end;

procedure TEMFWriter.WritePolyLine(AStream: TStream; AItem: TlmfPolyLine);
var
  numPts: Word;
  rec: TEMFPolyLineRecord ;
  recPts: packed array of TEMFPointLRecord = nil;
  i: Integer;
  bounds: TRect;
  recID: DWord;
begin
  numPts := Length(AItem.Points);
  if AItem.StartsAtPenPos then
    inc(numPts);

  bounds := Rect(MaxInt, MaxInt, -MaxInt, -MaxInt);

  SetLength(recPts, numPts);
  if AItem.StartsAtPenPos then
    recID := EMR_POLYLINETO
  else
    recID := EMR_POLYLINE;
  for i := 0 to numPts-1 do
  begin
    recPts[i].X := NToLE(AItem.Points[i].X);
    recPts[i].Y := NToLE(AItem.Points[i].Y);
    bounds.Left := Min(bounds.Left, AItem.Points[i].X);
    bounds.Top := Min(bounds.Top, AItem.Points[i].Y);
    bounds.Right := Max(bounds.Right, AItem.Points[i].X);
    bounds.Bottom := Max(bounds.Bottom, AItem.Points[i].Y);
  end;

  rec.BoundsLeft := NtoLE(bounds.Left);
  rec.BoundsTop := NtoLE(bounds.Top);
  rec.BoundsRight := NtoLE(bounds.Right);
  rec.BoundsBottom := NtoLE(bounds.bottom);
  rec.NumPts := NtoLE(numPts);

  // EMF record header + parameters
  WriteEMFRecord(AStream, recID, SizeOf(TEMFPolyLineRecord) + numPts * SizeOf(TEMFPointLRecord));
  WriteEMFParams(AStream, rec, SizeOf(TEMFPolyLinerecord));
  WriteEMFParams(AStream, recPts[0], numPts * SizeOf(TEMFPointLRecord));
end;

procedure TEMFWriter.WriteRecords(AStream: TStream);
var
  i: Integer;
  item: TlmfObject;
  startPos: Int64;
begin
  startPos := AStream.Position;

  // Since we don't know all fields of the header yet, we skip the header.
  // Todo: there can be several headers !!!
  AStream.Position := AStream.Position + SizeOf(TEnhancedMetaHeader);

  // Setup defaults
  (*
  WriteMapMode(AStream, mmAnisotropic);
  WriteSetWindowExtEx(AStream, FImage.LogUnitsPerInch, FImage.LogUnitsPerInch);
  WriteSetViewportExtEx(AStream, FImage.DevPixelsPerInch, FImage.DevPixelsPerInch);
  *)
  WriteBkColor(AStream, clWhite);
  WriteBkMode(AStream, TRANSPARENT);
  WriteTextAlign(AStream, TA_TOP or TA_LEFT);

  (*
  // Setup defaults
  WriteWindowExt(AStream);
  WriteWindowOrg(AStream);
  WriteMapMode(AStream, MM_ANISOTROPIC);  // all programs which write wmf do this...
  WriteBkColor(AStream, clWhite);
  WriteBkMode(AStream, TRANSPARENT);
  WriteTextAlign(AStream, TA_TOP or TA_LEFT);
         *)

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
    if item is TlmfPolyBezier then
      WritePolyBezier(AStream, TlmfPolyBezier(item))
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
    if item is TlmfGradientFill then
      WriteGradientFill(AStream, TlmfGradientFill(item))
    else
    if item is TlmfEllipse then
      WriteEllipse(AStream, TlmfEllipse(item))
    else
    if item is TlmfRoundRect then
      WriteRoundRect(AStream, TlmfRoundRect(item))
    else
    if item is TlmfRect then
      WriteRectangle(AStream, TlmfRect(item))
    else
    if item is TlmfBrush then
      WriteBrush(AStream, TlmfBrush(item))
    else
    if item is TlmfPen then
    begin
      if TlmfPen(item).Pen.Style = psPattern then
        WritePen_UserPattern(AStream, TlmfPen(item))
      else
        WritePen(AStream, TlmfPen(item))
    end else
    if item is TlmfFont then
      WriteFont(AStream, TlmfFont(item))
    else
    if item is TlmfMoveTo then
      WriteMoveToEx(AStream, TlmfMoveTo(item))
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
      WriteBkMode(AStream, TlmfBkMode(item))
    ;
  end;

  //DeleteObjTable(AStream);

  // Last record must be an EOF record.
  WriteEOF(AStream);

  // Go back to the beginning of the file and write the header.
  // Use correct header fields now, e.g. header.Size (= stream size)
  AStream.Position := startPos;
  WriteHeader(AStream);
end;

procedure TEMFWriter.WriteRectangle(AStream: TStream; AItem: TlmfRect);
var
  rec: TEMFRectLRecord;
begin
  rec.Left := NToLE(AItem.Left);
  rec.Top := NToLE(AItem.Top);
  rec.Right := NToLE(AItem.Right - 1);    // -1 because of inclusive-inclusive rect
  rec.Bottom := NToLE(AItem.Bottom - 1);

  // EMF record header + parameters
  WriteEMFRecord(AStream, EMR_RECTANGLE, rec, SizeOf(TEMFRectLRecord));
end;

procedure TEMFWriter.WriteRoundRect(AStream: TStream; AItem: TlmfRoundRect);
var
  rec: TEMFRoundRectRecord;
begin
  rec.Left := NToLE(AItem.Left);
  rec.Top := NToLE(AItem.Top);
  rec.Right := NToLE(AItem.Right - 1);    // -1 because of inclusive-inclusive coordinates
  rec.Bottom := NToLE(AItem.Bottom - 1);
  rec.CornerWidth := NToLE(AItem.Rx);
  rec.CornerHeight := NToLE(AItem.Ry);

  // EMF record header + parameters
  WriteEMFRecord(AStream, EMR_ROUNDRECT, rec, SizeOf(TEMFRoundRectRecord));
end;

procedure TEMFWriter.WriteSetViewportExtEx(AStream: TStream; AWidth, AHeight: Integer);
var
  params: Array[0..1] of DWord;
begin
  params[0] := NToLE(AWidth);
  params[1] := NToLE(AHeight);  // Use negative value when y runs upwards.
  WriteEMFRecord(AStream, EMR_SETVIEWPORTEXTEX, params, SizeOf(params));
end;

procedure TEMFWriter.WriteSetWindowExtEx(AStream: TStream; AWidth, AHeight: Integer);
var
  params: Array[0..1] of DWord;
begin
  params[0] := NToLE(AWidth);
  params[1] := NToLE(AHeight);  // Use negative value when y runs upwards.
  WriteEMFRecord(AStream, EMR_SETWINDOWEXTEX, params, SizeOf(params));
end;

procedure TEMFWriter.WriteStretchBLT(AStream: TStream; ABitmap: TBitmap;
  ARect: TRect; AOperation: Integer);
var
  rec: TEMFStretchBLTRecord;
  ms: TMemoryStream;
  offsToBits: Int64;
  hdrSize: DWord;
  bitsSize: DWord;
  dibImgSize: Int64;
  bmpFileHdr: TBitmapFileHeader;
  bmpInfoHdr: TBitmapInfoHeader;
  hdrData: packed array of byte = nil;
  bitsData: packed array of byte = nil;
  n: Int64;
begin
  if ABitmap = nil then
    exit;

  ms := TMemoryStream.Create;
  try
    ABitmap.SaveToStream(ms);                   // Save bitmap to stream
    dibImgSize := ms.Size - SizeOf(bmpFileHdr); // = bmp info header + pixel data
    ms.Position := 0;                           // Rewind stream
    ms.Read(bmpFileHdr, SizeOf(bmpFileHdr));    // Read bmp file header
    offsToBits := bmpFileHdr.bfOffset;          // Offset to image data
    // Read bitmap info header into buffer
    hdrSize := ms.ReadDWord;
    SetLength(hdrData, hdrSize);
    ms.Position := ms.Position - SizeOf(DWord);
    ms.Read(hdrData[0], hdrSize);
    bitsSize := dibImgSize - hdrSize;
    // Read image pixel data into buffer
    SetLength(bitsData, bitsSize);              // Read image pixel data
    ms.Position := offsToBits;
    ms.Read(bitsData[0], bitsSize);
  finally
    ms.Free;
  end;

  rec := Default(TEMFStretchBLTRecord);
  rec.BoundsLeft := LEtoN(0);
  rec.BoundsTop := LEtoN(0);
  rec.BoundsRight := LEtoN(-1);
  rec.BoundsBottom := LEtoN(-1);
  rec.xDest := LEtoN(ARect.Left);
  rec.yDest := LEtoN(ARect.Top);
  rec.cxDest := LEtoN(ARect.Right - ARect.Left);
  rec.cyDest := LEtoN(ARect.Bottom - ARect.Top);
  rec.xSrc := 0;
  rec.ySrc := 0;
  rec.cxSrc := LEtoN(ABitmap.Width);
  rec.cySrc := LEtoN(ABitmap.Height);
  rec.BitBltRasterOP := AOperation;
  rec.Transform[0] := 1.0;  // M11
  rec.Transform[1] := 0.0;  // M12
  rec.Transform[2] := 0.0;  // M21
  rec.Transform[3] := 1.0;  // M22
  rec.Transform[4] := 0.0;  // DX
  rec.Transform[5] := 0.0;  // DY
  rec.BkColorSrc := 0;
  rec.UsageSrc := 0;
  rec.offBmiSrc := LEtoN(SizeOf(TEMFRecord) + SizeOf(rec));
  rec.cbBmiSrc := LEtoN(hdrSize);
  rec.offBitsSrc := LEtoN(SizeOf(TEMFRecord) + SizeOf(rec) + hdrSize);
  rec.cbBitsSrc := LEtoN(bitsSize);

  // Write record
  WriteEMFRecord(AStream, EMR_STRETCHBLT, SizeOf(TEMFStretchBLTRecord) + hdrSize + bitsSize);
  AStream.Write(rec, SizeOf(TEMFStretchBLTRecord));
  AStream.Write(hdrData[0], hdrSize);
  AStream.Write(bitsData[0], bitsSize);

  // Write DIB
//  AStream.CopyFrom(ms, dibImgSize);
end;

procedure TEMFWriter.WriteText(AStream: TStream; AItem: TlmfText);
var
  outRec: TEMFExtTextOutRecord;
  txtRec: TEMFTextRecord;
  sizeOfText: Integer;
  sizeOfPadding: DWord;
  sizeOfDX: DWord;
  padding: DWord = 0;
  strLen: Word;
  ws: WideString;
  offsDX: DWord;
  i: Integer;
 {$IFDEF SUPPORT_DX}
  dx: array of DWord = nil;
 {$ENDIF}
begin
  if AItem.Text = '' then
    exit;

  ws := AItem.Text;
  strLen := Length(ws);
  sizeOfText := strLen * SizeOf(WideChar);
  sizeOfPadding := sizeOfText mod 4;

 {$IFDEF SUPPORT_DX}
  offsDX := SizeOf(TEMFRecord) + SizeOf(outRec) + SizeOf(txtRec) + sizeOfText + sizeOfPadding;
  sizeOfDX := strLen * SIZE_OF_DWORD;
 {$ELSE}
  offsDX := 0;
  sizeOfDx := 0;
 {$ENDIF}

  outRec.BoundsLeft := 0;
  outRec.BoundsTop := 0;
  outRec.BoundsRight := NtoLE(-1);
  outRec.BoundsBottom := NtoLE(-1);
  outRec.GraphicsMode := NtoLE(1);  // 1 = GM_COMPATIBLE
  outRec.XScale := 0.0;             // is ignored
  outRec.YScale := 0.0;

  txtRec := Default(TEMFTextRecord);
  txtRec.RefPtX := NtoLE(AItem.PX);
  txtRec.RefPtY := NtoLE(AItem.PY);
  txtRec.NumChars := NtoLE(strLen);
  // we store string immediately after txtRec
  txtRec.OffsToString := NToLE(SizeOf(TEMFRecord) + SizeOf(outRec) + SizeOf(txtRec));
  txtRec.Options := 0;              // is ignored by Canvas.TextOut
  txtRec.RectLeft := NtoLE(0);      // is ignored by Canvas.TextOut
  txtRec.RectTop := NtoLE(0);
  txtRec.RectRight := NtoLE(-1);
  txtRec.RectBottom := NtoLE(-1);
  // Dx parameters are not used here.
  txtRec.OffsToDx :=  NtoLE(offsDX);

  // Record header
  WriteEMFRecord(AStream, EMR_EXTTEXTOUTW, SizeOf(outRec) + SizeOf(txtRec) + sizeOfText + sizeOfPadding + sizeOfDX);
  AStream.Write(outRec, SizeOf(outRec));
  AStream.Write(txtRec, SizeOf(txtRec));
  AStream.Write(ws[1], sizeOfText);
  if sizeOfPadding <> 0 then
    AStream.Write(padding, sizeOfPadding);
 {$IFDEF SUUPPORT_DX}
  SetLength(dx, strLen);
  for i := 0 to strLen-1 do dx[i] := <put dx value here>;
  AStream.Write(dx[0], strLen * SIZE_OF_DWORD);
 {$ENDIF}
end;

procedure TEMFWriter.WriteTextAlign(AStream: TStream; AValue: Dword);
begin
  WriteEMFRecord(AStream, EMR_SETTEXTALIGN, NToLE(AValue), SIZE_OF_DWORD);
end;

procedure TEMFWriter.WriteTextInRect(AStream: TStream; AItem: TlmfTextInRect);
var
  outRec: TEMFExtTextOutRecord;
  txtRec: TEMFTextRecord;
  txt: WideString;
  txtLen: DWord;
  sizeOfTxt: DWord;
  sizeOfPadding: DWord;
  sizeOfDX: DWord;
  padding: DWord;
  optns: DWord;
  offsDX: DWord;
 {$IFDEF SUPPORT_DX}
  dx: array of DWord = nil;
 {$ENDIF}
begin
  if AItem.Text = '' then
    exit;

  txt := AItem.Text;
  txtLen := Length(txt);
  sizeOfTxt := txtLen * SizeOf(WideChar);
  sizeOfPadding := sizeOfTxt mod 4;

 {$IFDEF SUPPORT_DX}
  offsDX := SizeOf(TEMFRecord) + SizeOf(outRec) + SizeOf(txtRec) + sizeOfTxt + sizeOfPadding;
  sizeOfDX := txtLen * SIZE_OF_DWORD;
 {$ELSE}
  offsDX := 0;
  sizeOfDx := 0;
 {$ENDIF}

  optns := 0;
  if AItem.TextStyle.Opaque then optns := optns or ETO_OPAQUE;
  if AItem.TextStyle.Clipping then optns := optns or ETO_CLIPPED;
  if AItem.TextStyle.RightToLeft then optns := optns or ETO_RTLREADING;

  outRec := Default(TEMFExtTextOutRecord);
  outRec.BoundsLeft := 0;
  outRec.BoundsTop := 0;
  outRec.BoundsRight := NtoLE(-1);
  outRec.BoundsBottom := NtoLE(-1);
  outRec.GraphicsMode := NtoLE(1);  // 1 = GM_COMPATIBLE
  outRec.XScale := 0.0;             // is ignored
  outRec.YScale := 0.0;

  txtRec := Default(TEMFTextRecord);
  txtRec.RefPtX := NtoLE(AItem.PX);
  txtRec.RefPtY := NtoLE(AItem.PY);
  txtRec.NumChars := NtoLE(txtLen);
  // we store string immediately after txtRec
  txtRec.OffsToString := NToLE(SizeOf(TEMFRecord) + SizeOf(outRec) + SizeOf(txtRec));
  txtRec.Options := NToLE(optns);
  // The entire text rectangle is filled here. Note that this is in addition to
  // SetBkMode which fills only the background of the text itself.
  txtRec.RectLeft := NtoLE(AItem.Left);
  txtRec.RectTop := NtoLE(AItem.Top);
  txtRec.RectRight := NtoLE(AItem.Right);
  txtRec.RectBottom := NtoLE(AItem.Bottom);
  txtRec.OffsToDx :=  NtoLE(offsDX);

  // Record header
  WriteEMFRecord(AStream, EMR_EXTTEXTOUTW, SizeOf(outRec) + SizeOf(txtRec) + sizeOfTxt + sizeOfPadding + sizeOfDX);
  AStream.Write(outRec, SizeOf(outRec));
  AStream.Write(txtRec, SizeOf(txtRec));
  AStream.Write(txt[1], sizeOfTxt);
  if sizeOfPadding <> 0 then
    AStream.Write(padding, sizeOfPadding);
 {$IFDEF SUUPPORT_DX}
  SetLength(dx, txtLen);
  for i := 0 to txtLen-1 do dx[i] := <put character-spacing-values here>;
  AStream.Write(dx[0], txtLen * SIZE_OF_DWORD);
 {$ENDIF}
end;

procedure TEMFWriter.WriteToStream(AStream: TStream; AImage: TlmfImage);
begin
  FImage := AImage;

  PrepareObjTable;

  // Write the records of the image
  WriteRecords(AStream);
end;
         (*
procedure TEMFWriter.WriteWindowOrg(AStream: TStream);
var
  params: Array[0..1] of DWord;
begin
  params[0] := 0;
  params[1] := 0;
  WriteEMFRecord(AStream, EMR_SETWINDOWORGEX, params, Sizeof(params));
end;
           *)
{ Writes the EMF header (total record size + function code) only.
  Useful when the parameter block has variable size.
  ASize is the size of the following parameter block, in bytes }
procedure TEMFWriter.WriteEMFRecord(AStream: TStream;
  AFunc: Word; ASize: Integer);
var
  rec: TEMFRecord;
begin
  rec.Func := NToLE(AFunc);
  rec.Size := NToLE(SizeOf(TEMFRecord) + ASize);
  AStream.WriteBuffer(rec, SizeOf(TEMFRecord));
  FMaxRecordSize := Max(FMaxRecordSize, rec.Size);
end;

{ Write the EMF header (total record size + function code) and the
  parameters of the record.
  Intended for records having a fixed parameter block.
  ASize is the size of the parameter block, in bytes }
procedure TEMFWriter.WriteEMFRecord(AStream: TStream;
  AFunc: Word; const AParams; ASize: Integer);
var
  rec: TEMFRecord;
begin
  rec := Default(TEMFRecord);
  rec.Func := NToLE(AFunc);
  rec.Size := NToLE(SizeOf(TEMFRecord) + ASize);
  AStream.WriteBuffer(rec, SizeOf(TEMFRecord));
  AStream.WriteBuffer(AParams, ASize);
  FMaxRecordSize := Max(FMaxRecordSize, rec.Size);
end;

{ ASize is in bytes }
procedure TEMFWriter.WriteEMFParams(AStream: TStream;
  const AParams; ASize: Integer);
begin
  AStream.WriteBuffer(AParams, ASize);
end;


end.

