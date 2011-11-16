{
fpvectorial.pas

Vector graphics document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit fpvectorial;

{$ifdef fpc}
  {$mode delphi}
{$endif}

{$define USE_LCL_CANVAS}

interface

uses
  Classes, SysUtils, Math,
  // FCL-Image
  fpcanvas, fpimage
  // LCL
  {$ifdef USE_LCL_CANVAS}
  , Graphics, LCLIntf, LCLType
  {$endif}
  ;

type
  TvVectorialFormat = (
    { Multi-purpose document formats }
    vfPDF, vfSVG, vfCorelDrawCDR, vfWindowsMetafileWMF,
    { CAD formats }
    vfDXF,
    { Printing formats }
    vfPostScript, vfEncapsulatedPostScript,
    { GCode formats }
    vfGCodeAvisoCNCPrototipoV5, vfGCodeAvisoCNCPrototipoV6);

const
  { Default extensions }
  { Multi-purpose document formats }
  STR_PDF_EXTENSION = '.pdf';
  STR_POSTSCRIPT_EXTENSION = '.ps';
  STR_SVG_EXTENSION = '.svg';
  STR_CORELDRAW_EXTENSION = '.cdr';
  STR_WINMETAFILE_EXTENSION = '.wmf';
  STR_AUTOCAD_EXCHANGE_EXTENSION = '.dxf';
  STR_ENCAPSULATEDPOSTSCRIPT_EXTENSION = '.eps';

type
  { Pen, Brush and Font }

  TvPen = record
    Color: TFPColor;
    Style: TFPPenStyle;
    Width: Integer;
  end;

  TvBrush = record
    Color: TFPColor;
    Style: TFPBrushStyle;
  end;

  TvFont = record
    Color: TFPColor;
    Size: integer;
    Name: utf8string;
    {@@
      Font orientation is measured in degrees and uses the
      same direction as the LCL TFont.orientation, which is counter-clockwise.
      Zero is the normal, horizontal, orientation, directed to the right.
    }
    Orientation: Double;
  end;

  { Coordinates and polyline segments }

  T3DPoint = record
    X, Y, Z: Double;
  end;

  P3DPoint = ^T3DPoint;

  TSegmentType = (
    st2DLine, st2DLineWithPen, st2DBezier,
    st3DLine, st3DBezier, stMoveTo);

  {@@
    The coordinates in fpvectorial are given in millimiters and
    the starting point is in the bottom-left corner of the document.
    The X grows to the right and the Y grows to the top.
  }
  { TPathSegment }

  TPathSegment = class
  public
    SegmentType: TSegmentType;
    // Fields for linking the list
    Previous: TPathSegment;
    Next: TPathSegment;
  end;

  {@@
    In a 2D segment, the X and Y coordinates represent usually the
    final point of the segment, being that it starts where the previous
    segment ends. The exception is for the first segment of all, which simply
    holds the starting point for the drawing and should always be of the type
    stMoveTo.
  }
  T2DSegment = class(TPathSegment)
  public
    X, Y: Double;
  end;

  T2DSegmentWithPen = class(T2DSegment)
  public
    Pen: TvPen;
  end;

  {@@
    In Bezier segments, we remain using the X and Y coordinates for the ending point.
    The starting point is where the previous segment ended, so that the intermediary
    bezier control points are [X2, Y2] and [X3, Y3].
  }
  T2DBezierSegment = class(T2DSegment)
  public
    X2, Y2: Double;
    X3, Y3: Double;
  end;

  T3DSegment = class(TPathSegment)
  public
    {@@
      Coordinates of the end of the segment.
      For the first segment, this is the starting point.
    }
    X, Y, Z: Double;
  end;

  T3DBezierSegment = class(T3DSegment)
  public
    X2, Y2, Z2: Double;
    X3, Y3, Z3: Double;
  end;

  TvFindEntityResult = (vfrNotFound, vfrFound, vfrSubpartFound);

  { Now all elements }

  {@@
    All elements should derive from TvEntity, regardless of whatever properties
    they might contain.
  }

  { TvEntity }

  TvEntity = class
  public
    X, Y, Z: Double;
    {@@ The global Pen for the entire entity. In the case of paths, individual
        elements might be able to override this setting. }
    Pen: TvPen;
    {@@ The global Brush for the entire entity. In the case of paths, individual
        elements might be able to override this setting. }
    Brush: TvBrush;
    constructor Create; virtual;
    procedure CalculateBoundingBox(var ALeft, ATop, ARight, ABottom: Double); virtual;
    procedure ExpandBoundingBox(var ALeft, ATop, ARight, ABottom: Double);
    {@@ ASubpart is only valid if this routine returns vfrSubpartFound }
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult; virtual;
    procedure Translate(ADeltaX, ADeltaY: Integer); virtual;
    procedure TransladeSubpart(ADeltaX, ADeltaY: Integer; ASubpart: Cardinal); virtual;
    procedure Render(ADest: TFPCustomCanvas; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); virtual;
  end;

  TvClipMode = (vcmNonzeroWindingRule, vcmEvenOddRule);

  TPath = class(TvEntity)
  public
    Len: Integer;
    Points: TPathSegment;   // Beginning of the double-linked list
    PointsEnd: TPathSegment;// End of the double-linked list
    CurPoint: TPathSegment; // Used in PrepareForSequentialReading and Next
    ClipPath: TPath;
    ClipMode: TvClipMode;
    procedure Assign(ASource: TPath);
    procedure PrepareForSequentialReading;
    function Next(): TPathSegment;
    procedure CalculateBoundingBox(var ALeft, ATop, ARight, ABottom: Double); override;
    procedure AppendSegment(ASegment: TPathSegment);
  end;

  {@@
    TvText represents a text entity.
  }

  { TvText }

  TvText = class(TvEntity)
  public
    Value: TStringList;
    Font: TvFont;
    constructor Create; override;
    destructor Destroy; override;
    function TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult; override;
  end;

  {@@
  }
  TvCircle = class(TvEntity)
  public
    Radius: Double;
  end;

  {@@
  }
  TvCircularArc = class(TvEntity)
  public
    Radius: Double;
    {@@ The Angle is measured in degrees in relation to the positive X axis }
    StartAngle, EndAngle: Double;
  end;

  {@@
  }

  { TvEllipse }

  TvEllipse = class(TvEntity)
  public
    // Mandatory fields
    MajorHalfAxis: Double; // This half-axis is the horizontal one when Angle=0
    MinorHalfAxis: Double; // This half-axis is the vertical one when Angle=0
    {@@ The Angle is measured in degrees in relation to the positive X axis }
    Angle: Double;
    procedure CalculateBoundingBox(var ALeft, ATop, ARight, ABottom: Double); override;
    procedure Render(ADest: TFPCustomCanvas; ADestX: Integer = 0;
      ADestY: Integer = 0; AMulX: Double = 1.0; AMulY: Double = 1.0); override;
  end;

  {@@
   The brush has no effect in this class

   DimensionLeft ---text--- DimensionRight
                 |        |
                 |        | BaseRight
                 |
                 | BaseLeft
  }

  { TvAlignedDimension }

  TvAlignedDimension = class(TvEntity)
  public
    // Mandatory fields
    BaseLeft, BaseRight, DimensionLeft, DimensionRight: T3DPoint;
  end;

  {@@
   Vectorial images can contain raster images inside them and this entity
   represents this.

   If the Width and Height differ from the same data in the image, then
   the raster image will be stretched.

   Note that TFPCustomImage does not implement a storage, so the property
   RasterImage should be filled with either a FPImage.TFPMemoryImage or with
   a TLazIntfImage. The property RasterImage might be nil.
  }
  TvRasterImage = class(TvEntity)
  public
    RasterImage: TFPCustomImage;
    Top, Left, Width, Height: Double;
  end;

type

  TvCustomVectorialWriter = class;
  TvCustomVectorialReader = class;
  TvVectorialPage = class;

  { TvVectorialDocument }

  TvVectorialDocument = class
  private
    FPages: TFPList;
    FCurrentPageIndex: Integer;
    function CreateVectorialWriter(AFormat: TvVectorialFormat): TvCustomVectorialWriter;
    function CreateVectorialReader(AFormat: TvVectorialFormat): TvCustomVectorialReader;
  public
    Width, Height: Double; // in millimeters
    Name: string;
    // User-Interface information
    ZoomLevel: Double; // 1 = 100%
    { Selection fields }
    SelectedvElement: TvEntity;
    { Base methods }
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TvVectorialDocument);
    procedure AssignTo(ADest: TvVectorialDocument);
    procedure WriteToFile(AFileName: string; AFormat: TvVectorialFormat); overload;
    procedure WriteToFile(AFileName: string); overload;
    procedure WriteToStream(AStream: TStream; AFormat: TvVectorialFormat);
    procedure WriteToStrings(AStrings: TStrings; AFormat: TvVectorialFormat);
    procedure ReadFromFile(AFileName: string; AFormat: TvVectorialFormat); overload;
    procedure ReadFromFile(AFileName: string); overload;
    procedure ReadFromStream(AStream: TStream; AFormat: TvVectorialFormat);
    procedure ReadFromStrings(AStrings: TStrings; AFormat: TvVectorialFormat);
    class function GetFormatFromExtension(AFileName: string): TvVectorialFormat;
    function  GetDetailedFileFormat(): string;
    procedure GuessDocumentSize();
    procedure GuessGoodZoomLevel(AScreenSize: Integer = 500);
    { Page methods }
    function GetPage(AIndex: Integer): TvVectorialPage;
    function GetPageCount: Integer;
    function GetCurrentPage: TvVectorialPage;
    procedure SetCurrentPage(AIndex: Integer);
    function AddPage(): TvVectorialPage;
    { Data removing methods }
    procedure Clear; virtual;
  end;

  { TvVectorialPage }

  TvVectorialPage = class
  private
    FEntities: TFPList;
    FTmpPath: TPath;
    FTmpText: TvText;
    //procedure RemoveCallback(data, arg: pointer);
    procedure ClearTmpPath();
    procedure AppendSegmentToTmpPath(ASegment: TPathSegment);
  public
    Width, Height: Double; // in millimeters
    Owner: TvVectorialDocument;
    { Base methods }
    constructor Create(AOwner: TvVectorialDocument); virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TvVectorialPage);
    { Data reading methods }
    function  GetEntity(ANum: Cardinal): TvEntity;
    function  GetEntitiesCount: Integer;
    function  FindAndSelectEntity(Pos: TPoint): TvFindEntityResult;
    { Data removing methods }
    procedure Clear; virtual;
    { Data writing methods }
    function AddEntity(AEntity: TvEntity): Integer;
    procedure AddPathCopyMem(APath: TPath);
    procedure StartPath(AX, AY: Double); overload;
    procedure StartPath(); overload;
    procedure AddMoveToPath(AX, AY: Double);
    procedure AddLineToPath(AX, AY: Double); overload;
    procedure AddLineToPath(AX, AY: Double; AColor: TFPColor); overload;
    procedure AddLineToPath(AX, AY, AZ: Double); overload;
    procedure GetCurrentPathPenPos(var AX, AY: Double);
    procedure AddBezierToPath(AX1, AY1, AX2, AY2, AX3, AY3: Double); overload;
    procedure AddBezierToPath(AX1, AY1, AZ1, AX2, AY2, AZ2, AX3, AY3, AZ3: Double); overload;
    procedure SetBrushColor(AColor: TFPColor);
    procedure SetBrushStyle(AStyle: TFPBrushStyle);
    procedure SetPenColor(AColor: TFPColor);
    procedure SetPenStyle(AStyle: TFPPenStyle);
    procedure SetPenWidth(AWidth: Integer);
    procedure SetClipPath(AClipPath: TPath; AClipMode: TvClipMode);
    procedure EndPath();
    procedure AddText(AX, AY, AZ: Double; FontName: string; FontSize: integer; AText: utf8string); overload;
    procedure AddText(AX, AY: Double; AStr: utf8string); overload;
    procedure AddText(AX, AY, AZ: Double; AStr: utf8string); overload;
    procedure AddCircle(ACenterX, ACenterY, ARadius: Double);
    procedure AddCircularArc(ACenterX, ACenterY, ARadius, AStartAngle, AEndAngle: Double; AColor: TFPColor);
    procedure AddEllipse(CenterX, CenterY, MajorHalfAxis, MinorHalfAxis, Angle: Double);
    // Dimensions
    procedure AddAlignedDimension(BaseLeft, BaseRight, DimLeft, DimRight: T3DPoint);
  end;

  {@@ TvVectorialReader class reference type }

  TvVectorialReaderClass = class of TvCustomVectorialReader;

  { TvCustomVectorialReader }

  TvCustomVectorialReader = class
  public
    { General reading methods }
    constructor Create; virtual;
    procedure ReadFromFile(AFileName: string; AData: TvVectorialDocument); virtual;
    procedure ReadFromStream(AStream: TStream; AData: TvVectorialDocument); virtual;
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); virtual;
  end;

  {@@ TvVectorialWriter class reference type }

  TvVectorialWriterClass = class of TvCustomVectorialWriter;

  {@@ TvCustomVectorialWriter }

  { TvCustomVectorialWriter }

  TvCustomVectorialWriter = class
  public
    { General writing methods }
    constructor Create; virtual;
    procedure WriteToFile(AFileName: string; AData: TvVectorialDocument); virtual;
    procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); virtual;
    procedure WriteToStrings(AStrings: TStrings; AData: TvVectorialDocument); virtual;
  end;

  {@@ List of registered formats }

  TvVectorialFormatData = record
    ReaderClass: TvVectorialReaderClass;
    WriterClass: TvVectorialWriterClass;
    ReaderRegistered: Boolean;
    WriterRegistered: Boolean;
    Format: TvVectorialFormat;
  end;

var
  GvVectorialFormats: array of TvVectorialFormatData;

procedure RegisterVectorialReader(
  AReaderClass: TvVectorialReaderClass;
  AFormat: TvVectorialFormat);
procedure RegisterVectorialWriter(
  AWriterClass: TvVectorialWriterClass;
  AFormat: TvVectorialFormat);
function Make2DPoint(AX, AY: Double): T3DPoint;

implementation

uses fpvutils;

const
  Str_Error_Nil_Path = ' The program attempted to add a segment before creating a path';

{@@
  Registers a new reader for a format
}
procedure RegisterVectorialReader(
  AReaderClass: TvVectorialReaderClass;
  AFormat: TvVectorialFormat);
var
  i, len: Integer;
  FormatInTheList: Boolean;
begin
  len := Length(GvVectorialFormats);
  FormatInTheList := False;

  { First search for the format in the list }
  for i := 0 to len - 1 do
  begin
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].ReaderRegistered then
       raise Exception.Create('RegisterVectorialReader: Reader class for format ' {+ AFormat} + ' already registered.');

      GvVectorialFormats[i].ReaderRegistered := True;
      GvVectorialFormats[i].ReaderClass := AReaderClass;

      FormatInTheList := True;
      Break;
    end;
  end;

  { If not already in the list, then add it }
  if not FormatInTheList then
  begin
    SetLength(GvVectorialFormats, len + 1);

    GvVectorialFormats[len].ReaderClass := AReaderClass;
    GvVectorialFormats[len].WriterClass := nil;
    GvVectorialFormats[len].ReaderRegistered := True;
    GvVectorialFormats[len].WriterRegistered := False;
    GvVectorialFormats[len].Format := AFormat;
  end;
end;

{@@
  Registers a new writer for a format
}
procedure RegisterVectorialWriter(
  AWriterClass: TvVectorialWriterClass;
  AFormat: TvVectorialFormat);
var
  i, len: Integer;
  FormatInTheList: Boolean;
begin
  len := Length(GvVectorialFormats);
  FormatInTheList := False;

  { First search for the format in the list }
  for i := 0 to len - 1 do
  begin
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].WriterRegistered then
       raise Exception.Create('RegisterVectorialWriter: Writer class for format ' + {AFormat +} ' already registered.');

      GvVectorialFormats[i].WriterRegistered := True;
      GvVectorialFormats[i].WriterClass := AWriterClass;

      FormatInTheList := True;
      Break;
    end;
  end;

  { If not already in the list, then add it }
  if not FormatInTheList then
  begin
    SetLength(GvVectorialFormats, len + 1);

    GvVectorialFormats[len].ReaderClass := nil;
    GvVectorialFormats[len].WriterClass := AWriterClass;
    GvVectorialFormats[len].ReaderRegistered := False;
    GvVectorialFormats[len].WriterRegistered := True;
    GvVectorialFormats[len].Format := AFormat;
  end;
end;

function Make2DPoint(AX, AY: Double): T3DPoint;
begin
  Result.X := AX;
  Result.Y := AY;
  Result.Z := 0;
end;

{ TvVectorialPage }

procedure TvVectorialPage.ClearTmpPath;
var
  segment, oldsegment: TPathSegment;
begin
  FTmpPath.Points := nil;
  FTmpPath.PointsEnd := nil;
  FTmpPath.Len := 0;
  FTmpPath.Brush.Color := colBlue;
  FTmpPath.Brush.Style := bsClear;
  FTmpPath.Pen.Color := colBlack;
  FTmpPath.Pen.Style := psSolid;
  FTmpPath.Pen.Width := 1;
end;

procedure TvVectorialPage.AppendSegmentToTmpPath(ASegment: TPathSegment);
begin
  FTmpPath.AppendSegment(ASegment);
end;

constructor TvVectorialPage.Create(AOwner: TvVectorialDocument);
begin
  inherited Create;

  FEntities := TFPList.Create;
  FTmpPath := TPath.Create;
  Owner := AOwner;
end;

destructor TvVectorialPage.Destroy;
begin
  Clear;

  FEntities.Free;

  inherited Destroy;
end;

procedure TvVectorialPage.Assign(ASource: TvVectorialPage);
var
  i: Integer;
begin
  Clear;

  for i := 0 to ASource.GetEntitiesCount - 1 do
    Self.AddEntity(ASource.GetEntity(i));
end;

function TvVectorialPage.GetEntity(ANum: Cardinal): TvEntity;
begin
  if ANum >= FEntities.Count then raise Exception.Create('TvVectorialDocument.GetEntity: Entity number out of bounds');

  if FEntities.Items[ANum] = nil then raise Exception.Create('TvVectorialDocument.GetEntity: Invalid Entity number');

  Result := TvEntity(FEntities.Items[ANum]);
end;

function TvVectorialPage.GetEntitiesCount: Integer;
begin
  Result := FEntities.Count;
end;

function TvVectorialPage.FindAndSelectEntity(Pos: TPoint): TvFindEntityResult;
var
  lEntity: TvEntity;
  i: Integer;
  lSubpart: Cardinal;
begin
  Result := vfrNotFound;

  for i := 0 to GetEntitiesCount() - 1 do
  begin
    lEntity := GetEntity(i);

    Result := lEntity.TryToSelect(Pos, lSubpart);

    if Result <> vfrNotFound then
    begin
      Owner.SelectedvElement := lEntity;
      Exit;
    end;
  end;
end;

procedure TvVectorialPage.Clear;
begin
  FEntities.Clear();
end;

{@@
  Adds an entity to the document and returns it's current index
}
function TvVectorialPage.AddEntity(AEntity: TvEntity): Integer;
begin
  Result := FEntities.Count;
  FEntities.Add(Pointer(AEntity));
end;

procedure TvVectorialPage.AddPathCopyMem(APath: TPath);
var
  lPath: TPath;
  Len: Integer;
begin
  lPath := TPath.Create;
  lPath.Assign(APath);
  AddEntity(lPath);
  //WriteLn(':>TvVectorialDocument.AddPath 1 Len = ', Len);
end;

{@@
  Starts writing a Path in multiple steps.
  Should be followed by zero or more calls to AddPointToPath
  and by a call to EndPath to effectively add the data.

  @see    EndPath, AddPointToPath
}
procedure TvVectorialPage.StartPath(AX, AY: Double);
var
  segment: T2DSegment;
begin
  ClearTmpPath();

  FTmpPath.Len := 1;
  segment := T2DSegment.Create;
  segment.SegmentType := stMoveTo;
  segment.X := AX;
  segment.Y := AY;

  FTmpPath.Points := segment;
  FTmpPath.PointsEnd := segment;
end;

procedure TvVectorialPage.StartPath;
begin
  ClearTmpPath();
end;

procedure TvVectorialPage.AddMoveToPath(AX, AY: Double);
var
  segment: T2DSegment;
begin
  segment := T2DSegment.Create;
  segment.SegmentType := stMoveTo;
  segment.X := AX;
  segment.Y := AY;

  AppendSegmentToTmpPath(segment);
end;

{@@
  Adds one more point to the end of a Path being
  writing in multiple steps.

  Does nothing if not called between StartPath and EndPath.

  Can be called multiple times to add multiple points.

  @see    StartPath, EndPath
}
procedure TvVectorialPage.AddLineToPath(AX, AY: Double);
var
  segment: T2DSegment;
begin
  segment := T2DSegment.Create;
  segment.SegmentType := st2DLine;
  segment.X := AX;
  segment.Y := AY;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.AddLineToPath(AX, AY: Double; AColor: TFPColor);
var
  segment: T2DSegmentWithPen;
begin
  segment := T2DSegmentWithPen.Create;
  segment.SegmentType := st2DLineWithPen;
  segment.X := AX;
  segment.Y := AY;
  segment.Pen.Color := AColor;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.AddLineToPath(AX, AY, AZ: Double);
var
  segment: T3DSegment;
begin
  segment := T3DSegment.Create;
  segment.SegmentType := st3DLine;
  segment.X := AX;
  segment.Y := AY;
  segment.Z := AZ;

  AppendSegmentToTmpPath(segment);
end;

{@@
  Gets the current Pen Pos in the temporary path
}
procedure TvVectorialPage.GetCurrentPathPenPos(var AX, AY: Double);
begin
  // Check if we are the first segment in the tmp path
  if FTmpPath.PointsEnd = nil then raise Exception.Create('[TvVectorialDocument.GetCurrentPathPenPos] One cannot obtain the Pen Pos if there are no segments in the temporary path');

  AX := T2DSegment(FTmpPath.PointsEnd).X;
  AY := T2DSegment(FTmpPath.PointsEnd).Y;
end;

{@@
  Adds a bezier element to the path. It starts where the previous element ended
  and it goes throw the control points [AX1, AY1] and [AX2, AY2] and ends
  in [AX3, AY3].
}
procedure TvVectorialPage.AddBezierToPath(AX1, AY1, AX2, AY2, AX3, AY3: Double);
var
  segment: T2DBezierSegment;
begin
  segment := T2DBezierSegment.Create;
  segment.SegmentType := st2DBezier;
  segment.X := AX3;
  segment.Y := AY3;
  segment.X2 := AX1;
  segment.Y2 := AY1;
  segment.X3 := AX2;
  segment.Y3 := AY2;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.AddBezierToPath(AX1, AY1, AZ1, AX2, AY2, AZ2, AX3, AY3, AZ3: Double);
var
  segment: T3DBezierSegment;
begin
  segment := T3DBezierSegment.Create;
  segment.SegmentType := st3DBezier;
  segment.X := AX3;
  segment.Y := AY3;
  segment.Z := AZ3;
  segment.X2 := AX1;
  segment.Y2 := AY1;
  segment.Z2 := AZ1;
  segment.X3 := AX2;
  segment.Y3 := AY2;
  segment.Z3 := AZ2;

  AppendSegmentToTmpPath(segment);
end;

procedure TvVectorialPage.SetBrushColor(AColor: TFPColor);
begin
  FTmPPath.Brush.Color := AColor;
end;

procedure TvVectorialPage.SetBrushStyle(AStyle: TFPBrushStyle);
begin
  FTmPPath.Brush.Style := AStyle;
end;

procedure TvVectorialPage.SetPenColor(AColor: TFPColor);
begin
  FTmPPath.Pen.Color := AColor;
end;

procedure TvVectorialPage.SetPenStyle(AStyle: TFPPenStyle);
begin
  FTmPPath.Pen.Style := AStyle;
end;

procedure TvVectorialPage.SetPenWidth(AWidth: Integer);
begin
  FTmPPath.Pen.Width := AWidth;
end;

procedure TvVectorialPage.SetClipPath(AClipPath: TPath; AClipMode: TvClipMode);
begin
  FTmPPath.ClipPath := AClipPath;
  FTmPPath.ClipMode := AClipMode;
end;

{@@
  Finishes writing a Path, which was created in multiple
  steps using StartPath and AddPointToPath,
  to the document.

  Does nothing if there wasn't a previous correspondent call to
  StartPath.

  @see    StartPath, AddPointToPath
}
procedure TvVectorialPage.EndPath;
begin
  if FTmPPath.Len = 0 then Exit;
  AddPathCopyMem(FTmPPath);
  ClearTmpPath();
end;

procedure TvVectorialPage.AddText(AX, AY, AZ: Double; FontName: string;
  FontSize: integer; AText: utf8string);
var
  lText: TvText;
begin
  lText := TvText.Create;
  lText.Value.Text := AText;
  lText.X := AX;
  lText.Y := AY;
  lText.Z := AZ;
  lText.Font.Name := FontName;
  lText.Font.Size := FontSize;
  AddEntity(lText);
end;

procedure TvVectorialPage.AddText(AX, AY: Double; AStr: utf8string);
begin
  AddText(AX, AY, 0, '', 10, AStr);
end;

procedure TvVectorialPage.AddText(AX, AY, AZ: Double; AStr: utf8string);
begin
  AddText(AX, AY, AZ, '', 10, AStr);
end;

procedure TvVectorialPage.AddCircle(ACenterX, ACenterY, ARadius: Double);
var
  lCircle: TvCircle;
begin
  lCircle := TvCircle.Create;
  lCircle.X := ACenterX;
  lCircle.Y := ACenterY;
  lCircle.Radius := ARadius;
  AddEntity(lCircle);
end;

procedure TvVectorialPage.AddCircularArc(ACenterX, ACenterY, ARadius,
  AStartAngle, AEndAngle: Double; AColor: TFPColor);
var
  lCircularArc: TvCircularArc;
begin
  lCircularArc := TvCircularArc.Create;
  lCircularArc.X := ACenterX;
  lCircularArc.Y := ACenterY;
  lCircularArc.Radius := ARadius;
  lCircularArc.StartAngle := AStartAngle;
  lCircularArc.EndAngle := AEndAngle;
  lCircularArc.Pen.Color := AColor;
  AddEntity(lCircularArc);
end;

procedure TvVectorialPage.AddEllipse(CenterX, CenterY, MajorHalfAxis,
  MinorHalfAxis, Angle: Double);
var
  lEllipse: TvEllipse;
begin
  lEllipse := TvEllipse.Create;
  lEllipse.X := CenterX;
  lEllipse.Y := CenterY;
  lEllipse.MajorHalfAxis := MajorHalfAxis;
  lEllipse.MinorHalfAxis := MinorHalfAxis;
  lEllipse.Angle := Angle;
  AddEntity(lEllipse);
end;


procedure TvVectorialPage.AddAlignedDimension(BaseLeft, BaseRight, DimLeft,
  DimRight: T3DPoint);
var
  lDim: TvAlignedDimension;
begin
  lDim := TvAlignedDimension.Create;
  lDim.BaseLeft := BaseLeft;
  lDim.BaseRight := BaseRight;
  lDim.DimensionLeft := DimLeft;
  lDim.DimensionRight := DimRight;
  AddEntity(lDim);
end;

{ TvText }

constructor TvText.Create;
begin
  inherited Create;
  Value := TStringList.Create;
end;

destructor TvText.Destroy;
begin
  Value.Free;
  inherited Destroy;
end;

function TvText.TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult;
var
  lProximityFactor: Integer;
begin
  lProximityFactor := 5;
  if (APos.X > X - lProximityFactor) and (APos.X < X + lProximityFactor)
    and (APos.Y > Y - lProximityFactor) and (APos.Y < Y + lProximityFactor) then
    Result := vfrFound
  else Result := vfrNotFound;
end;

{ TvEntity }

constructor TvEntity.Create;
begin
  Pen.Style := psSolid;
  Pen.Color := colBlack;
  Brush.Style := bsClear;
  Brush.Color := colBlue;
end;

procedure TvEntity.CalculateBoundingBox(var ALeft, ATop, ARight, ABottom: Double);
begin
  ALeft := 0;
  ATop := 0;
  ARight := 0;
  ABottom := 0;
end;

procedure TvEntity.ExpandBoundingBox(var ALeft, ATop, ARight, ABottom: Double);
var
  lLeft, lTop, lRight, lBottom: Double;
begin
  CalculateBoundingBox(lLeft, lTop, lRight, lBottom);
  if lLeft < ALeft then ALeft := lLeft;
  if lTop < ATop then ATop := lTop;
  if lRight > ARight then ARight := lRight;
  if lBottom > ABottom then ABottom := lBottom;
end;

function TvEntity.TryToSelect(APos: TPoint; var ASubpart: Cardinal): TvFindEntityResult;
begin
  Result := vfrNotFound;
end;

procedure TvEntity.Translate(ADeltaX, ADeltaY: Integer);
begin
  X := X + ADeltaX;
  Y := Y + ADeltaY;
end;

procedure TvEntity.TransladeSubpart(ADeltaX, ADeltaY: Integer;
  ASubpart: Cardinal);
begin

end;

procedure TvEntity.Render(ADest: TFPCustomCanvas; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);
begin

end;

{ TvEllipse }

procedure TvEllipse.CalculateBoundingBox(var ALeft, ATop, ARight, ABottom: Double);
var
  t, tmp: Double;
begin
  // First do the trivial
  ALeft := X - MajorHalfAxis;
  ARight := X + MajorHalfAxis;
  ATop := Y - MinorHalfAxis;
  ABottom := Y + MinorHalfAxis;
  {
    To calculate the bounding rectangle we can do this:

    Ellipse equations:You could try using the parametrized equations for an ellipse rotated at an arbitrary angle:

    x = CenterX + MajorHalfAxis*cos(t)*cos(Angle) - MinorHalfAxis*sin(t)*sin(Angle)
    y = CenterY + MinorHalfAxis*sin(t)*cos(Angle) + MajorHalfAxis*cos(t)*sin(Angle)

    You can then differentiate and solve for gradient = 0:
    0 = dx/dt = -MajorHalfAxis*sin(t)*cos(Angle) - MinorHalfAxis*cos(t)*sin(Angle)
    =>
    tan(t) = -MinorHalfAxis*tan(Angle)/MajorHalfAxis
    =>
    t = cotang(-MinorHalfAxis*tan(Angle)/MajorHalfAxis)

    On the other axis:

    0 = dy/dt = b*cos(t)*cos(phi) - a*sin(t)*sin(phi)
    =>
    tan(t) = b*cot(phi)/a
  }
  if Angle <> 0.0 then
  begin
    t := cotan(-MinorHalfAxis*tan(Angle)/MajorHalfAxis);
    tmp := X + MajorHalfAxis*cos(t)*cos(Angle) - MinorHalfAxis*sin(t)*sin(Angle);
    ARight := Round(tmp);
  end;
end;

procedure TvEllipse.Render(ADest: TFPCustomCanvas; ADestX: Integer;
  ADestY: Integer; AMulX: Double; AMulY: Double);

  function CoordToCanvasX(ACoord: Double): Integer;
  begin
    Result := Round(ADestX + AmulX * ACoord);
  end;

  function CoordToCanvasY(ACoord: Double): Integer;
  begin
    Result := Round(ADestY + AmulY * ACoord);
  end;

var
  PointList: array[0..6] of TPoint;
  f: TPoint;
  dk, x1, x2, y1, y2: Integer;
  fx1, fy1, fx2, fy2: Double;
  {$ifdef USE_LCL_CANVAS}
  ALCLDest: TCanvas absolute ADest;
  {$endif}
begin
  CalculateBoundingBox(fx1, fy1, fx2, fy2);
  x1 := CoordToCanvasX(fx1);
  x2 := CoordToCanvasX(fx2);
  y1 := CoordToCanvasY(fy1);
  y2 := CoordToCanvasY(fy2);

  {$ifdef USE_LCL_CANVAS}
  if Angle <> 0 then
  begin
    dk := Round(0.654 * Abs(y2-y1));
    f.x := Round(X);
    f.y := Round(Y - 1);
    PointList[0] := Rotate2DPoint(Point(x1, f.y), f, Angle) ;  // Startpoint
    PointList[1] := Rotate2DPoint(Point(x1,  f.y - dk), f, Angle);
    //Controlpoint of Startpoint first part
    PointList[2] := Rotate2DPoint(Point(x2- 1,  f.y - dk), f, Angle);
    //Controlpoint of secondpoint first part
    PointList[3] := Rotate2DPoint(Point(x2 -1 , f.y), f, Angle);
    // Firstpoint of secondpart
    PointList[4] := Rotate2DPoint(Point(x2-1 , f.y + dk), f, Angle);
    // Controllpoint of secondpart firstpoint
    PointList[5] := Rotate2DPoint(Point(x1, f.y +  dk), f, Angle);
    // Conrollpoint of secondpart endpoint
    PointList[6] := PointList[0];   // Endpoint of
     // Back to the startpoint
    ALCLDest.PolyBezier(Pointlist[0]);
  end
  else
  {$endif}
  begin
    ADest.Pen.Style := psSolid;
    ADest.Pen.FPColor := colBlack;
    ADest.Ellipse(x1, y1, x2, y2);
  end;
end;

{ TsWorksheet }

{@@
  Constructor.
}
constructor TvVectorialDocument.Create;
begin
  inherited Create;

  FPages := TFPList.Create;
end;

{@@
  Destructor.
}
destructor TvVectorialDocument.Destroy;
begin
  Clear;

  FPages.Free;

  inherited Destroy;
end;

procedure TvVectorialDocument.Assign(ASource: TvVectorialDocument);
//var
//  i: Integer;
begin
//  Clear;
//
//  for i := 0 to ASource.GetEntitiesCount - 1 do
//    Self.AddEntity(ASource.GetEntity(i));
end;

procedure TvVectorialDocument.AssignTo(ADest: TvVectorialDocument);
begin
  ADest.Assign(Self);
end;

{@@
  Convenience method which creates the correct
  writer object for a given vector graphics document format.
}
function TvVectorialDocument.CreateVectorialWriter(AFormat: TvVectorialFormat): TvCustomVectorialWriter;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Length(GvVectorialFormats) - 1 do
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].WriterClass <> nil then
        Result := GvVectorialFormats[i].WriterClass.Create;

      Break;
    end;

  if Result = nil then raise Exception.Create('Unsupported vector graphics format.');
end;

{@@
  Convenience method which creates the correct
  reader object for a given vector graphics document format.
}
function TvVectorialDocument.CreateVectorialReader(AFormat: TvVectorialFormat): TvCustomVectorialReader;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Length(GvVectorialFormats) - 1 do
    if GvVectorialFormats[i].Format = AFormat then
    begin
      if GvVectorialFormats[i].ReaderClass <> nil then
        Result := GvVectorialFormats[i].ReaderClass.Create;

      Break;
    end;

  if Result = nil then raise Exception.Create('Unsupported vector graphics format.');
end;

{@@
  Writes the document to a file.

  If the file doesn't exist, it will be created.
}
procedure TvVectorialDocument.WriteToFile(AFileName: string; AFormat: TvVectorialFormat);
var
  AWriter: TvCustomVectorialWriter;
begin
  AWriter := CreateVectorialWriter(AFormat);

  try
    AWriter.WriteToFile(AFileName, Self);
  finally
    AWriter.Free;
  end;
end;

procedure TvVectorialDocument.WriteToFile(AFileName: string);
var
  lFormat: TvVectorialFormat;
begin
  lFormat := GetFormatFromExtension(ExtractFileExt(AFileName));
  WriteToFile(AFileName, lFormat);
end;

{@@
  Writes the document to a stream
}
procedure TvVectorialDocument.WriteToStream(AStream: TStream; AFormat: TvVectorialFormat);
var
  AWriter: TvCustomVectorialWriter;
begin
  AWriter := CreateVectorialWriter(AFormat);

  try
    AWriter.WriteToStream(AStream, Self);
  finally
    AWriter.Free;
  end;
end;

procedure TvVectorialDocument.WriteToStrings(AStrings: TStrings;
  AFormat: TvVectorialFormat);
var
  AWriter: TvCustomVectorialWriter;
begin
  AWriter := CreateVectorialWriter(AFormat);

  try
    AWriter.WriteToStrings(AStrings, Self);
  finally
    AWriter.Free;
  end;
end;

{@@
  Reads the document from a file.

  Any current contents in this object will be removed.
}
procedure TvVectorialDocument.ReadFromFile(AFileName: string;
  AFormat: TvVectorialFormat);
var
  AReader: TvCustomVectorialReader;
begin
  Self.Clear;

  AReader := CreateVectorialReader(AFormat);
  try
    AReader.ReadFromFile(AFileName, Self);
  finally
    AReader.Free;
  end;
end;

{@@
  Reads the document from a file.  A variant that auto-detects the format from the extension and other factors.
}
procedure TvVectorialDocument.ReadFromFile(AFileName: string);
var
  lFormat: TvVectorialFormat;
begin
  lFormat := GetFormatFromExtension(ExtractFileExt(AFileName));
  ReadFromFile(AFileName, lFormat);
end;

{@@
  Reads the document from a stream.

  Any current contents in this object will be removed.
}
procedure TvVectorialDocument.ReadFromStream(AStream: TStream;
  AFormat: TvVectorialFormat);
var
  AReader: TvCustomVectorialReader;
begin
  Self.Clear;

  AReader := CreateVectorialReader(AFormat);
  try
    AReader.ReadFromStream(AStream, Self);
  finally
    AReader.Free;
  end;
end;

procedure TvVectorialDocument.ReadFromStrings(AStrings: TStrings;
  AFormat: TvVectorialFormat);
var
  AReader: TvCustomVectorialReader;
begin
  Self.Clear;

  AReader := CreateVectorialReader(AFormat);
  try
    AReader.ReadFromStrings(AStrings, Self);
  finally
    AReader.Free;
  end;
end;

class function TvVectorialDocument.GetFormatFromExtension(AFileName: string
  ): TvVectorialFormat;
var
  lExt: string;
begin
  lExt := ExtractFileExt(AFileName);
  if AnsiCompareText(lExt, STR_PDF_EXTENSION) = 0 then Result := vfPDF
  else if AnsiCompareText(lExt, STR_POSTSCRIPT_EXTENSION) = 0 then Result := vfPostScript
  else if AnsiCompareText(lExt, STR_SVG_EXTENSION) = 0 then Result := vfSVG
  else if AnsiCompareText(lExt, STR_CORELDRAW_EXTENSION) = 0 then Result := vfCorelDrawCDR
  else if AnsiCompareText(lExt, STR_WINMETAFILE_EXTENSION) = 0 then Result := vfWindowsMetafileWMF
  else if AnsiCompareText(lExt, STR_AUTOCAD_EXCHANGE_EXTENSION) = 0 then Result := vfDXF
  else if AnsiCompareText(lExt, STR_ENCAPSULATEDPOSTSCRIPT_EXTENSION) = 0 then Result := vfEncapsulatedPostScript
  else
    raise Exception.Create('TvVectorialDocument.GetFormatFromExtension: The extension (' + lExt + ') doesn''t match any supported formats.');
end;

function  TvVectorialDocument.GetDetailedFileFormat(): string;
begin

end;

procedure TvVectorialDocument.GuessDocumentSize();
var
  i, j: Integer;
  lEntity: TvEntity;
  lLeft, lTop, lRight, lBottom: Double;
  CurPage: TvVectorialPage;
begin
  lLeft := 0;
  lTop := 0;
  lRight := 0;
  lBottom := 0;

  for j := 0 to GetPageCount()-1 do
  begin
    CurPage := GetPage(j);
    for i := 0 to CurPage.GetEntitiesCount() - 1 do
    begin
      lEntity := CurPage.GetEntity(I);
      lEntity.ExpandBoundingBox(lLeft, lTop, lRight, lBottom);
    end;
  end;

  Width := lRight - lLeft;
  Height := lBottom - lTop;
end;

procedure TvVectorialDocument.GuessGoodZoomLevel(AScreenSize: Integer);
begin
  ZoomLevel := AScreenSize / Height;
end;

function TvVectorialDocument.GetPage(AIndex: Integer): TvVectorialPage;
begin
  Result := TvVectorialPage(FPages.Items[AIndex]);
end;

function TvVectorialDocument.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

function TvVectorialDocument.GetCurrentPage: TvVectorialPage;
begin
  if FCurrentPageIndex >= 0 then
    Result := GetPage(FCurrentPageIndex)
  else
    Result := nil;
end;

procedure TvVectorialDocument.SetCurrentPage(AIndex: Integer);
begin
  FCurrentPageIndex := AIndex;
end;

function TvVectorialDocument.AddPage: TvVectorialPage;
begin
  Result := TvVectorialPage.Create(Self);
  FPages.Add(Result);
  if FCurrentPageIndex < 0 then FCurrentPageIndex := FPages.Count-1;
end;

{@@
  Clears all data in the document
}
procedure TvVectorialDocument.Clear;
begin
end;

{ TvCustomVectorialReader }

constructor TvCustomVectorialReader.Create;
begin
  inherited Create;
end;

procedure TvCustomVectorialReader.ReadFromFile(AFileName: string; AData: TvVectorialDocument);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    ReadFromStream(FileStream, AData);
  finally
    FileStream.Free;
  end;
end;

procedure TvCustomVectorialReader.ReadFromStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  AStringStream: TStringStream;
  AStrings: TStringList;
begin
  AStringStream := TStringStream.Create('');
  AStrings := TStringList.Create;
  try
    AStringStream.CopyFrom(AStream, AStream.Size);
    AStringStream.Seek(0, soFromBeginning);
    AStrings.Text := AStringStream.DataString;
    ReadFromStrings(AStrings, AData);
  finally
    AStringStream.Free;
    AStrings.Free;
  end;
end;

procedure TvCustomVectorialReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  AStringStream: TStringStream;
begin
  AStringStream := TStringStream.Create('');
  try
    AStringStream.WriteString(AStrings.Text);
    AStringStream.Seek(0, soFromBeginning);
    ReadFromStream(AStringStream, AData);
  finally
    AStringStream.Free;
  end;
end;

{ TsCustomSpreadWriter }

constructor TvCustomVectorialWriter.Create;
begin
  inherited Create;
end;

{@@
  Default file writting method.

  Opens the file and calls WriteToStream

  @param  AFileName The output file name.
                   If the file already exists it will be replaced.
  @param  AData     The Workbook to be saved.

  @see    TsWorkbook
}
procedure TvCustomVectorialWriter.WriteToFile(AFileName: string; AData: TvVectorialDocument);
var
  OutputFile: TFileStream;
begin
  OutputFile := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
  try
    WriteToStream(OutputFile, AData);
  finally
    OutputFile.Free;
  end;
end;

{@@
  The default stream writer just uses WriteToStrings
}
procedure TvCustomVectorialWriter.WriteToStream(AStream: TStream;
  AData: TvVectorialDocument);
var
  lStringList: TStringList;
begin
  lStringList := TStringList.Create;
  try
    WriteToStrings(lStringList, AData);
    lStringList.SaveToStream(AStream);
  finally
    lStringList.Free;
  end;
end;

procedure TvCustomVectorialWriter.WriteToStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
begin

end;

{ TPath }

procedure TPath.Assign(ASource: TPath);
begin
  Len := ASource.Len;
  Points := ASource.Points;
  PointsEnd := ASource.PointsEnd;
  CurPoint := ASource.CurPoint;
  Pen := ASource.Pen;
  Brush := ASource.Brush;
  ClipPath := ASource.ClipPath;
  ClipMode := ASource.ClipMode;
end;

procedure TPath.PrepareForSequentialReading;
begin
  CurPoint := nil;
end;

function TPath.Next(): TPathSegment;
begin
  if CurPoint = nil then Result := Points
  else Result := CurPoint.Next;

  CurPoint := Result;
end;

procedure TPath.CalculateBoundingBox(var ALeft, ATop, ARight, ABottom: Double);
var
  lSegment: TPathSegment;
  l2DSegment: T2DSegment;
  lFirstValue: Boolean = True;
begin
  inherited CalculateBoundingBox(ALeft, ATop, ARight, ABottom);

  PrepareForSequentialReading();
  lSegment := Next();
  while lSegment <> nil do
  begin
    if lSegment is T2DSegment then
    begin
      l2DSegment := T2DSegment(lSegment);
      if lFirstValue then
      begin
        ALeft := l2DSegment.X;
        ATop := l2DSegment.Y;
        ARight := l2DSegment.X;
        ABottom := l2DSegment.Y;
        lFirstValue := False;
      end
      else
      begin
        if l2DSegment.X < ALeft then ALeft := l2DSegment.X;
        if l2DSegment.Y < ATop then ATop := l2DSegment.Y;
        if l2DSegment.X > ARight then ARight := l2DSegment.X;
        if l2DSegment.Y > ABottom then ABottom := l2DSegment.Y;
      end;
    end;

    lSegment := Next();
  end;
end;

procedure TPath.AppendSegment(ASegment: TPathSegment);
var
  L: Integer;
begin
  // Check if we are the first segment in the tmp path
  if PointsEnd = nil then
  begin
    if Len <> 0 then
      Exception.Create('[TPath.AppendSegment] Assertion failed Len <> 0 with PointsEnd = nil');

    Points := ASegment;
    PointsEnd := ASegment;
    Len := 1;
    Exit;
  end;

  L := Len;
  Inc(Len);

  // Adds the element to the end of the list
  PointsEnd.Next := ASegment;
  ASegment.Previous := PointsEnd;
  PointsEnd := ASegment;
end;

finalization

  SetLength(GvVectorialFormats, 0);

end.

