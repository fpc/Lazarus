unit lmf experimental;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, Types,
  LCLType, LCLIntf,
  FPCanvas, FPImage, GraphMath, GraphType, IntfGraphics, Graphics, syncobjs;

const
  PTS_PER_INCH = 72;             // 1 pt = 1/72 inch
  TWIPS_PER_INCH = 1440;         // 1 twip = 1/20 pt = 1/1440 inch
  HUNDREDTHS_MM_PER_INCH = 2540; // 1 inch = 25.4 mm = 2540 * 0.01mm

type
  // Exception types
  ElmfImage = class(Exception);
  ElmfReader = class(ElmfImage);
  ElmfWriter = class(ElmfImage);

  // forward declarations
  TlmfImage = class;
  TlmfList = class;

  // abstract reader class
  TlmfReader = class
  protected
    FImage: TlmfImage;
    FObjTable: TFPList;        // List with WMF/EMF objects (pen, brush, ...)
    FErrMsg: TStrings;
    procedure LogError(const AMsg: String);
    procedure ReadRecords(AStream: TStream); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadFromStream(AStream: TStream; AImage: TlmfImage); virtual;
  end;

  // abstract writer class
  TlmfWriter = class
  public
    procedure WriteToStream(AStream: TStream; AImage: TlmfImage); virtual; abstract;
  end;

  TlmfMapMode = (
    mmLogUnitsPerInch,  // this not Microsoft value!
    { 1..8 }mmText, mmLoMetric, mmHiMetric, mmLoEnglish, mmHiEnglish,
    mmTwips, mmIsotropic, mmAnisotropic
  );


  { TlmfImage }

  TlmfImage = class(TGraphic)
  private
    fLogOrgX, fLogOrgY, fLogWidth, fLogHeight: integer;  // logical units used by metafile
    fDevOrgX, fDevOrgY, fDevWidth, fDevHeight: Integer;  // pixels on output device
    fMMWidth, fMMHeight: Integer;                        // size in 0.01 mm
    kx, ky: double;
    fList: TlmfList;
    fCrs: TCriticalSection;
//    fEnhanced: Boolean;
    fMapMode: TlmfMapMode;
    fDevPixelsPerInch: Integer;
    fYAxisDown: Boolean;
    fBkMode: Integer;
  private
    function GetMMWidth: Integer;
    function GetMMHeight: Integer;
    procedure SetDevPixelsPerInch(AValue: Integer);
    procedure SetLogOrgX(AValue: Integer);
    procedure SetLogOrgY(AValue: Integer);
    function GetLogUnitsPerInch: Integer;
    procedure SetLogUnitsPerInch(AValue: Integer);
    procedure SetMapMode(AValue: TlmfMapMode);
  protected
    procedure AssignTo(Dest: TPersistent);override;
    function GetWidth: integer; override;
    procedure SetWidth(AValue: integer); override;
    function GetHeight: integer; override;
    procedure SetHeight(AValue: integer); override;
    function GetEmpty: boolean; override;
    function GetTransparent: Boolean; override;
    procedure SetTransparent(Value: Boolean); override;
    procedure CalcScaling(const Rect: TRect);
      //procedure Erase;override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    function ScaleSizeX(dX: Integer): Integer;
    function ScaleSizeY(dY: Integer): Integer;
    function ScaleX(X: Integer): Integer;
    function ScaleY(Y:Integer): Integer;
    procedure SetLogBounds(AOrgX, AOrgY, AWidth, AHeight: Integer);

    procedure LoadFromLMFFile(AFileName: String);
    procedure LoadFromLMFStream(AStream: TStream; AEnhanced: Boolean);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToLMFFile(AFileName: String);
    procedure SaveToLMFStream(Stream: TStream; AEnhanced: Boolean);
    procedure SaveToStream(Stream: TStream); override;

//    property Enhanced: Boolean read FEnhanced write fEnhanced;  // Write WMF or EMF stream
    property List: TlmfList read fList;

    property BkMode: Integer read FBkMode write FBkMode;
    { PPI of the device where the LMF was created }
    property DevPixelsPerInch: Integer read FDevPixelsPerInch write SetDevPixelsPerInch default 96;
    property LogOriginX: Integer read FLogOrgX write SetLogOrgX;
    property LogOriginY: Integer read FLogOrgY write SetLogOrgY;
    property LogUnitsPerInch: Integer read GetLogUnitsPerInch write SetLogUnitsPerInch;
    property MapMode: TlmfMapMode read FMapMode write SetMapMode default mmAnisotropic;
    property YAxisDown: Boolean read FYAxisDown;

    property DevWidth: Integer read FDevWidth write FDevHeight;
    property DevHeight: Integer read FDevHeight write FDevHeight;

    // Assuming 1 logUnit = 0.01 mm
    property MMWidth: Integer read GetMMWidth;
    property MMHeight: Integer read GetMMHeight;
  end;

  TlmfList = class(TComponent)
  private
    fWidth, fHeight:integer;   // Image size in logical units
    fLogUnitsPerInch: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; {%H-}Root: TComponent); override;
    function GetChildOwner: TComponent; override;
  published
    property Width:integer read fWidth write fWidth;
    property Height:integer read fHeight write fHeight;
    property LogUnitsPerInch: Integer read FLogUnitsPerInch write FLogUnitsPerInch;
  end;

implementation

uses
  lmfObj, lmfCanvas, lmfWMFWrite, lmfWMFRead, lmfEMFWrite, lmfEMFRead;

function Sign(x: Integer): Integer;
begin
  if x > 0 then Result := +1 else if x < 0 then Result := -1 else Result := 0;
end;

function FlipRect(ARect: TRect): TRect;
begin
  Result.Left := ARect.Left;
  Result.Right := ARect.Right;
  Result.Top := ARect.Bottom;
  Result.Bottom := ARect.Top;
end;


{ TlmfReader }

constructor TlmfReader.Create;
begin
  inherited Create;
  FErrMsg := TStringList.Create;
  FObjTable := TFPList.Create;
end;

destructor TlmfReader.Destroy;
begin
  FObjTable.Free;
  FErrMsg.Free;
  inherited Destroy;
end;

procedure TlmfReader.LogError(const AMsg: String);
begin
  FErrMsg.Add(AMsg);
end;

procedure TlmfReader.ReadFromStream(AStream: TStream; AImage: TlmfImage);
begin
  FImage := AImage;
  FImage.MapMode := mmAnisotropic;

  FObjTable.Clear;
  FErrMsg.Clear;

  ReadRecords(AStream);

  if FErrMsg.Count > 0 then
    raise ElmfReader.Create(FErrMsg.Text);
end;


{ TlmfImage }

constructor TlmfImage.Create;
begin
  inherited Create;
  fMapMode := mmAnisotropic;
  fCrs := syncobjs.TCriticalSection.Create;
  fList := TlmfList.Create(nil);
  fDevPixelsPerInch := ScreenInfo.PixelsPerInchX;
end;

destructor TlmfImage.Destroy;
begin
  Clear;
  fList.Free;
  inherited Destroy;
  fCrs.Free;
end;

procedure TlmfImage.AssignTo(Dest:TPersistent);
var
  mf:TMemoryStream;
begin
  if Dest is TlmfImage then
  begin
    mf := TMemoryStream.Create;
    try
      mf.WriteComponent(fList);
      mf.Position := 0;
      mf.ReadComponent(TlmfImage(Dest).fList);
      TlmfImage(Dest).fLogOrgX := fLogOrgX;
      TlmfImage(Dest).fLogOrgY := fLogOrgY;
      TlmfImage(Dest).fLogWidth := fLogWidth;
      TlmfImage(Dest).fLogHeight := fLogHeight;
      TlmfImage(Dest).LogUnitsPerInch := LogUnitsPerInch;
      TlmfImage(Dest).fDevOrgX := fDevOrgX;
      TlmfImage(Dest).fDevOrgY := fDevOrgY;
      TlmfImage(Dest).fDevWidth := fDevWidth;
      TlmfImage(Dest).fDevHeight := fDevHeight;
    finally
      mf.Free;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TlmfImage.Clear;
begin
  fList.DestroyComponents;
end;

function TlmfImage.GetLogUnitsPerInch: Integer;
begin
  Result := fList.LogUnitsPerInch;
end;

{ Defines the size of a logical unit in the metafile. By default 1 twip = 1/1440 inch }
procedure TlmfImage.SetLogUnitsPerInch(AValue: Integer);
begin
  fList.LogUnitsPerInch := AValue;
  Self.Modified := true;
end;

procedure TlmfImage.SetMapMode(AValue: TlmfMapMode);
begin
  if fMapMode = AValue then exit;
  fMapMode := AValue;
  Self.Modified := true;
end;

procedure TlmfImage.SetDevPixelsPerInch(AValue: Integer);
begin
  if fDevPixelsPerInch = AValue then exit;
  fDevPixelsPerInch := AValue;
  Self.Modified := true;
end;

function TlmfImage.GetMMWidth: Integer;
begin
  Result := round(fList.Width / HUNDREDTHS_MM_PER_INCH * DevPixelsPerInch);
end;

function TlmfImage.GetMMHeight: Integer;
begin
  Result := round(fList.Height / HUNDREDTHS_MM_PER_INCH * DevPixelsPerInch);
end;

function TlmfImage.GetWidth: integer;
begin
  Result := flist.fWidth;
end;

{ Sets the width of the metafile image in logical units. }
procedure TlmfImage.SetWidth(AValue: integer);
begin
  if (AValue = fLogWidth) then exit;
  fLogWidth := AValue;
  fList.fWidth := fLogWidth;
  Self.Modified := true;
end;

function TlmfImage.GetHeight:integer;
begin
  Result:=fList.fHeight;
end;

procedure TlmfImage.SetHeight(AValue: integer);
begin
  if (AValue = fLogHeight) then exit;
  fLogHeight := AValue;
  fList.fHeight := fLogHeight;
  Modified := true;
end;

// x coordinate of the the image origin in logical units
procedure TlmfImage.SetLogOrgX(AValue: Integer);
begin
  if FLogOrgX = AValue then exit;
  FLogOrgX := AValue;
  Self.Modified := true;
end;

// y coordinate of the the image origin in logical units
procedure TlmfImage.SetLogOrgY(AValue: Integer);
begin
  if FLogOrgY >= AValue then exit;
  FLogOrgY := AValue;
  Self.Modified := true;
end;

// dX is a length in logical units
function TlmfImage.ScaleSizeX(dX: Integer): Integer;
begin
  Result := round(dX * kx);
end;

// dY is a length in logical units
function TlmfImage.ScaleSizeY(dY: Integer): Integer;
begin
  Result := round(dY * ky);
end;

// X is in logical coordinates and is mapped to the units of the display device.
function TlmfImage.ScaleX(X: Integer):integer;
begin
  Result := fDevOrgX + trunc((X - FLogOrgX) * kx);
  //if Result>Width then Result:=width;
end;

// Y is in logical coordinates and is mapped to the units of the display device.
function TlmfImage.ScaleY(Y: Integer):integer;
begin
  Result := fDevOrgY + trunc((Y - FLogOrgY) * ky);
  //if Result>height then Result:=height;
end;

{ Sets the size of the image in logical units. }
procedure TlmfImage.SetLogBounds(AOrgX, AOrgY, AWidth, AHeight: Integer);
begin
  fLogOrgX := AOrgX;
  fLogOrgY := AOrgY;
  fLogWidth := AWidth;
  fLogHeight := AHeight;
  fList.fWidth := fLogWidth;
  fList.fHeight := fLogHeight;
  Modified := true;
end;

function TlmfImage.GetEmpty:boolean;
begin
  Result:= (not Assigned(fList)) or (fList.ComponentCount = 0);
end;

procedure TlmfImage.CalcScaling(const Rect: TRect);
var
  devAspectRatio, logAspectRatio: Double;
  w, h: Integer;
begin
  fDevOrgX := Rect.Left;
  fDevWidth := Rect.Right - Rect.Left;
  fDevHeight := abs(Rect.Bottom - Rect.Top);
  case fMapMode of
    mmIsotropic:
      // Logical units are mapped to arbitrary units with equally scaled axes.
      // The orientation of the axes may be specified by the application (fLogHeight < 0 --> y up)
      begin
        if fDevWidth = 0 then w := Rect.Right - Rect.Left else w := fDevWidth;
        if fDevHeight = 0 then h := Rect.Bottom - Rect.Top else h := fDevHeight;
        devAspectRatio := h / w;
        logAspectRatio := abs(fLogHeight / fLogWidth);
        if devAspectRatio > logAspectRatio then
        begin
          kx := w / fLogWidth;
          ky := kx;
        end else
        begin
          ky := h / fLogHeight;
          kx := abs(ky);
        end;
        fDevOrgY := Rect.Top;
      end;
    mmHiEnglish:
      // Each logical unit is mapped to 0.001 inch.
      // Positive x is to the right; positive y is up.
      begin
        kx := fDevPixelsPerInch / 1000.0;
        ky := -Sign(fLogHeight) * kx;
        fDevOrgY := Rect.Bottom;
      end;
    mmHiMetric:
      // Each logical unit is mapped to 0.01 millimeter.
      // Positive x is to the right; positive y is up.
      begin
        kx := fDevPixelsPerInch / 2540.0;
        ky := -Sign(fLogHeight) * kx;
        fDevOrgY := Rect.Bottom;
      end;
    mmLoEnglish:
      // Each logical unit is mapped to 0.01 inch.
      // Positive x is to the right; positive y is up.
      begin
        kx := fDevPixelsPerInch / 100.0;
        ky := -Sign(fLogHeight) * kx;
        fDevOrgY := Rect.Bottom;
      end;
    mmLoMetric:
      // Each logical unit is mapped to 0.1 millimeter.
      // Positive x is to the right; positive y is up.
      begin
        kx := fDevPixelsPerInch / 254.0;
        ky := -Sign(fLogHeight) * kx;
        fDevOrgY := Rect.Bottom;
      end;
    mmText:
      // Each logical unit is mapped to one device pixel.
      // Positive x is to the right; positive y is down.
      begin
        kx := 1.0;
        ky := +Sign(fLogHeight);
        fDevOrgY := Rect.Top;
      end;
    mmTwips:
      // Each logical unit is mapped to one twentieth of a printer's point
      // (1/1440 inch, also called a twip).
      // Positive x is to the right; positive y is up.
      begin
        kx := fDevPixelsPerInch / 1440;
        ky := -Sign(fLogHeight) * kx;
        fDevOrgY := Rect.Bottom;
      end;
    mmLogUnitsPerInch:
      // NOTE: Not a Windows MapMode!
      // Each logical units is mapped to the size of the LogUnitsPerInch
      // specification. Positive y is down unless when fLogHeight is negative
      begin
        kx := fDevPixelsPerInch / LogUnitsPerInch;
        ky := Sign(fLogHeight) * kx;
        fDevOrgY := Rect.Top;
      end;
    else
      // Anisotropic:
      // Logical units are mapped to arbitrary units with arbitrarily scaled axes.
      kx := fDevWidth / fLogWidth;
      ky := fDevHeight / fLogHeight;
      fDevOrgY := Rect.Top;
  end;

  FYAxisDown := ky > 0;
end;

procedure TlmfImage.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  i:integer;
  item: TlmfObject;
begin
  fCrs.Acquire;
  try
    BkMode := 1;  // Transparent
    CalcScaling(Rect);
    ACanvas.MoveTo(ScaleX(Rect.Left), ScaleY(Rect.Top));
    for i:=0 to fList.ComponentCount-1 do
    begin
      item := TlmfObject(fList.Components[i]);
      // It seems that SetBkMode must be executed immediately before executing a
      // background-dependant command (text, patterned line, filling).
      if (item is TlmfBkMode) then
        FBkMode := TlmfBkMode(item).Mode;
      item.Render(Self, ACanvas);
    end;
  finally
    kx := 1;
    ky := 1;
    fCrs.Release;
  end;
end;

function TlmfImage.GetTransparent: Boolean;
begin
  Result:=true; // assume it is always
end;

procedure TlmfImage.SetTransparent(Value: Boolean);
begin
  // nothing to do
end;

procedure TlmfImage.SaveToStream(Stream: TStream);
begin
  Stream.WriteComponent(fList);
end;

procedure TlmfImage.SaveToLMFFile(AFileName: String);
var
  stream: TFileStream;
  isEnhanced: Boolean;
begin
  isEnhanced := Lowercase(ExtractFileExt(AFilename)) = '.emf';
  stream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  try
    SaveToLMFStream(stream, isEnhanced);
  finally
    stream.Free;
  end;
end;

procedure TlmfImage.SaveToLMFStream(Stream: TStream; AEnhanced: Boolean);
var
  writer: TlmfWriter;
begin
  if AEnhanced then
    writer := TEMFWriter.Create
  else
    writer := TWMFWriter.Create;
  try
    writer.WriteToStream(Stream, self);
  finally
    writer.Free;
  end;
end;

procedure TlmfImage.LoadFromLMFFile(AFileName: String);
var
  stream: TFileStream;
  isEMF: Boolean;
begin
  stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    isEMF := Lowercase(ExtractFileExt(AFilename)) = '.emf';
    LoadFromLMFStream(stream, isEMF);
  finally
    stream.Free;
  end;
end;

procedure TlmfImage.LoadFromLMFStream(AStream: TStream; AEnhanced: Boolean);
var
  reader: TlmfReader;
begin
  if AEnhanced then
    reader := TlmfEMFReader.Create
  else
    reader := TlmfWMFReader.Create;
  try
    reader.ReadFromStream(AStream, self);
  finally
    reader.Free;
  end;
end;

procedure TlmfImage.LoadFromStream(Stream: TStream);
begin
  Stream.ReadComponent(fList);
  fLogWidth := fList.fWidth;
  fLogHeight := fList.fHeight;
end;


{ LMF list }

constructor TlmfList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogUnitsPerInch := TWIPS_PER_INCH;
end;

destructor TlmfList.Destroy;
var
  i: Integer;
  item: TlmfObject;
begin
  for i := ComponentCount-1 downto 0 do
  begin
    item := TlmfObject(Components[i]);
    FreeAndNil(item);
  end;
  inherited Destroy;
end;

procedure TlmfList.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i:integer;
begin
  for i:=0 to ComponentCount-1 do
    Proc(Components[i]);
end;

function TlmfList.GetChildOwner: TComponent;
begin
  Result:=self;
end;


initialization
  RegisterClasses([TlmfList]);

end.

