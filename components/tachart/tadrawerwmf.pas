{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Authors: Luís Rodrigues, Alexander Klenin

}

unit TADrawerWMF;

{$H+}

interface

{$IFDEF WINDOWS}
uses
  Windows, Classes, Graphics,
  TADrawerCanvas, TAGraph;

type
  { TMetafile }

  TMetafile = class(TGraphic)
  private
    FImageHandle: HENHMETAFILE;
    FMMHeight: Integer; // are in 0.01 mm logical pixels
    FMMWidth: Integer;  // are in 0.01 mm logical pixels
    FImagePxHeight: Integer; // in device pixels
    FImagePxWidth: Integer;  // in device pixels

    procedure DeleteImage;
    function GetAuthor: String;
    function GetDescription: String;
    function GetHandle: HENHMETAFILE;
    procedure SetHandle(AValue: HENHMETAFILE);
    procedure SetMMHeight(AValue: Integer);
    procedure SetMMWidth(AValue: Integer);
  protected
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    procedure SetHeight(AValue: Integer); override;
    procedure SetTransparent(AValue: Boolean); override;
    procedure SetWidth(AValue: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

  public
    procedure Assign(ASource: TPersistent); override;
    procedure Clear; override;
    procedure CopyToClipboard;
    procedure LoadFromFile(const AFileName: String); override;
    procedure LoadFromStream(AStream: TStream); override;
    function ReleaseHandle: HENHMETAFILE;
    procedure SaveToFile(const AFileName: String); override;
    procedure SaveToStream(AStream: TStream); override;

    property CreatedBy: String read GetAuthor;
    property Description: String read GetDescription;
    property Empty: boolean read GetEmpty;
    property Handle: HENHMETAFILE read GetHandle write SetHandle;

    property MMHeight: Integer read FMMHeight write SetMMHeight;
    property MMWidth: Integer read FMMWidth write SetMMWidth;
  end;

  { TMetafileCanvas }

  TMetafileCanvas = class(TCanvas)
  strict private
    FMetafile: TMetafile;
  public
    constructor Create(AMetafile: TMetafile; AReferenceDevice: HDC);
    constructor CreateWithComment(
      AMetafile: TMetafile; AReferenceDevice: HDC;
      const ACreatedBy, ADescription: String);
    destructor Destroy; override;
  end;

  { TWindowsMetafileDrawer }

  TWindowsMetafileDrawer = class(TCanvasDrawer)
  strict private
    FFileName: String;
    FMetafile: TMetafile;
  public
    constructor Create(const AFileName: String); reintroduce;
    destructor Destroy; override;
  public
    procedure DrawingBegin(const ABoundingBox: TRect); override;
    procedure DrawingEnd; override;
    function GetCanvas: TCanvas; override;
  end;

  { TWMFChartHelper }

  TWMFChartHelper = class helper for TChart
    procedure CopyToClipboardMetafile;
    procedure SaveToWMF(const AFileName: String);
  end;
{$ENDIF}

implementation

{$IFDEF WINDOWS}
uses
  SysUtils, clipbrd, TAChartUtils;

{ TWindowsMetafileDrawer }

constructor TWindowsMetafileDrawer.Create(const AFileName: String);
begin
  FFileName := AFileName;
  FMetafile := TMetafile.Create;
  inherited Create(nil);
end;

destructor TWindowsMetafileDrawer.Destroy;
begin
  FreeAndNil(FCanvas);
  FreeAndNil(FMetafile);
  inherited Destroy;
end;

procedure TWindowsMetafileDrawer.DrawingBegin(const ABoundingBox: TRect);
begin
  inherited DrawingBegin(ABoundingBox);
  FreeAndNil(FCanvas);
  FMetafile.Width := ABoundingBox.Right - ABoundingBox.Left;
  FMetafile.Height := ABoundingBox.Bottom - ABoundingBox.Top;
  if FCanvas = nil then
    FCanvas := TMetafileCanvas.Create(FMetafile, 0);
end;

procedure TWindowsMetafileDrawer.DrawingEnd;
begin
  FreeAndNil(FCanvas);
  if FFileName = '' then
   // Clipboard.Assign(FMetaFile)
    FMetafile.CopyToClipboard
  else
    FMetafile.SaveToFile(FFileName);
end;

function TWindowsMetafileDrawer.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
    FCanvas := TMetafileCanvas.Create(FMetafile, 0);
  Result := FCanvas;
end;

{ TMetafile }

procedure TMetafile.DeleteImage;
begin
  if FImageHandle <> 0 then
     DeleteEnhMetafile(FImageHandle);
   FImageHandle := 0;
end;

function TMetafile.GetAuthor: String;
var
  authorLength: Integer;
begin
  Result := '';
  if FImageHandle = 0 then exit;

  authorLength := GetEnhMetafileDescription(FImageHandle, 0, nil);
  if authorLength <= 0 then exit;
  SetLength(Result, authorLength);
  GetEnhMetafileDescription(FImageHandle, authorLength, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

function TMetafile.GetDescription: String;
var
  descLength: Integer;
begin
  Result := '';
  if FImageHandle = 0 then Exit;

  descLength := GetEnhMetafileDescription(FImageHandle, 0, nil);
  if descLength <= 0 then exit;
  SetLength(Result, descLength);
  GetEnhMetafileDescription(FImageHandle, descLength, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
end;

function TMetafile.GetEmpty: Boolean;
begin
  Result := FImageHandle = 0;
end;

function TMetafile.GetHandle: HENHMETAFILE;
begin
  Result := FImageHandle;
end;

procedure TMetafile.SetHandle(AValue: HENHMETAFILE);
var
  emfHeader: TEnhMetaHeader;
begin
  if
    (AValue <> 0) and
    (GetEnhMetafileHeader(AValue, sizeof(emfHeader), @emfHeader) = 0)
  then
     raise EInvalidImage.Create('Invalid Metafile');;

  if FImageHandle <> 0 then DeleteImage;

  FImageHandle := AValue;
  FImagePxWidth := 0;
  FImagePxHeight := 0;
  FMMWidth := emfHeader.rclFrame.Right - emfHeader.rclFrame.Left;
  FMMHeight := emfHeader.rclFrame.Bottom - emfHeader.rclFrame.Top;
end;


procedure TMetafile.SetMMHeight(AValue: Integer);
begin
  FImagePxHeight := 0;
  if FMMHeight <> AValue then FMMHeight := AValue;
end;

procedure TMetafile.SetMMWidth(AValue: Integer);
begin
  FImagePxWidth := 0;
  if FMMWidth <> AValue then FMMWidth := AValue;
end;

procedure TMetafile.SetTransparent(AValue: Boolean);
begin
  if AValue then
    raise EComponentError.Create('Not implemented');
end;

procedure TMetafile.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  r: TRect;
begin
  if FImageHandle = 0 then exit;
  r := ARect;
  PlayEnhMetaFile(ACanvas.Handle, FImageHandle, r);
end;

function TMetafile.GetHeight: Integer;
var
  emfHeader: TEnhMetaHeader;
begin
  if FImageHandle = 0 then
     exit(FImagePxHeight);
  // convert 0.01mm units to device pixels
  GetEnhMetaFileHeader(FImageHandle, Sizeof(emfHeader), @emfHeader);
  Result := MulDiv(
    FMMHeight, // metafile height in 0.01mm
    emfHeader.szlDevice.cy,  // device height in pixels
    emfHeader.szlMillimeters.cy * 100); // device height in mm
end;

function TMetafile.GetTransparent: Boolean;
begin
  Result := false;
end;

function TMetafile.GetWidth: Integer;
var
  emfHeader: TEnhMetaHeader;
begin
  if FImageHandle = 0 then
    exit(FImagePxWidth);
  // convert 0.01mm units to device pixels
  GetEnhMetaFileHeader(FImageHandle, Sizeof(emfHeader), @emfHeader);
  Result := MulDiv(
    FMMWidth, // metafile width in 0.01mm
    emfHeader.szlDevice.cx, // device width in pixels
    emfHeader.szlMillimeters.cx * 100); // device width in 0.01mm
end;


procedure TMetafile.SetHeight(AValue: Integer);
var
  emfHeader: TEnhMetaHeader;
begin
  if FImageHandle = 0 then
    FImagePxHeight := AValue
  else begin // convert device pixels to 0.01mm units
    GetEnhMetaFileHeader(FImageHandle, Sizeof(emfHeader), @emfHeader);
    MMHeight := MulDiv(AValue, // metafile height in pixels
      emfHeader.szlMillimeters.cy * 100, // device height in 0.01mm
      emfHeader.szlDevice.cy); // device height in pixels
  end;
end;

procedure TMetafile.SetWidth(AValue: Integer);
var
  emfHeader: TEnhMetaHeader;
begin
  if FImageHandle = 0 then
    FImagePxWidth := AValue
  else begin // convert device pixels to 0.01mm units
    GetEnhMetaFileHeader(FImageHandle, Sizeof(emfHeader), @emfHeader);
    MMWidth := MulDiv(AValue, // metafile width in pixels
      emfHeader.szlMillimeters.cx * 100, // device width in mm
      emfHeader.szlDevice.cx); // device width in pixels
  end;
end;

constructor TMetafile.Create;
begin
  inherited Create;
  FImageHandle := 0;
end;

destructor TMetafile.Destroy;
begin
  DeleteImage;
  inherited Destroy;
end;

procedure TMetafile.Assign(ASource: TPersistent);
begin
  if (ASource = nil) or (ASource is TMetafile) then begin
    if FImageHandle <> 0 then
      DeleteImage;
    if Assigned(ASource) then begin
      FImageHandle := TMetafile(ASource).Handle;
      FMMWidth := TMetafile(ASource).MMWidth;
      FMMHeight := TMetafile(ASource).MMHeight;
      FImagePxWidth := TMetafile(ASource).Width;
      FImagePxHeight := TMetafile(ASource).Height;
    end
  end
  else
    inherited Assign(ASource);
end;

procedure TMetafile.Clear;
begin
  DeleteImage;
end;

procedure TMetafile.CopyToClipboard;
// http://www.olivierlanglois.net/metafile-clipboard.html
var
  Format: Word;
begin
  if FImageHandle = 0 then exit;

  OpenClipboard(0);
  try
    EmptyClipboard;
    Format := CF_ENHMETAFILE;
    SetClipboardData(Format, FImageHandle);
  finally
    CloseClipboard;
  end;
end;

procedure TMetafile.LoadFromFile(const AFileName: String);
begin
  Unused(AFileName);
  raise EComponentError.Create('Not Implemented');
end;

procedure TMetafile.SaveToFile(const AFileName: String);
var
  outFile: HENHMETAFILE;
begin
  if FImageHandle = 0 then exit;
  outFile := CopyEnhMetaFile(FImageHandle, PChar(AFileName));
  if outFile = 0 then
    RaiseLastWin32Error;
  DeleteEnhMetaFile(outFile);
end;

procedure TMetafile.LoadFromStream(AStream: TStream);
begin
  Unused(AStream);
  raise EComponentError.Create('Not Implemented');
end;

procedure TMetafile.SaveToStream(AStream: TStream);
begin
  Unused(AStream);
  raise EComponentError.Create('Not Implemented');
end;

function TMetafile.ReleaseHandle: HENHMETAFILE;
begin
  DeleteImage;
  Result := FImageHandle;
  FImageHandle := 0;
end;

{ TMetafileCanvas }

constructor TMetafileCanvas.Create(AMetafile: TMetafile; AReferenceDevice: HDC);
begin
  CreateWithComment(
    AMetafile, AReferenceDevice, AMetafile.CreatedBy, AMetafile.Description);
end;

constructor TMetafileCanvas.CreateWithComment(
  AMetafile: TMetafile; AReferenceDevice: HDC;
  const ACreatedBy, ADescription: String);
var
  refDC: HDC;
  r: TRect;
  temp: HDC;
  p: PChar;
  w, h: Integer;
begin
  inherited Create;
  FMetafile := AMetafile;

  refDC := AReferenceDevice;
  if refDC = 0 then
    refDC := GetDC(0);

  try
    if FMetafile.MMWidth = 0 then begin
      w := GetDeviceCaps(refDC, HORZSIZE) * 100;
      if FMetafile.Width = 0 then // if no width get refDC width
        FMetafile.MMWidth := w
      else // else convert
        FMetafile.MMWidth := MulDiv(
          FMetafile.Width, w, GetDeviceCaps(refDC, HORZRES));
    end;

    if FMetafile.MMHeight = 0 then begin
      h := GetDeviceCaps(refDC, VERTSIZE) * 100;
      if FMetafile.Height = 0 then // if no height get refDC height
        FMetafile.MMHeight := h
      else // else convert
        FMetafile.MMHeight := MulDiv(
          FMetafile.Height, h, GetDeviceCaps(refDC, VERTRES));
    end;

    r := Rect(0, 0, FMetafile.MMWidth, FMetafile.MMHeight);
    // lpDescription stores both author and description
    if (Length(ACreatedBy) > 0) or (Length(ADescription) > 0) then
      p := PChar(ACreatedBy+#0+ADescription+#0#0)
    else
      p := nil;
    temp := CreateEnhMetafile(refDC, nil, @r, p);
    if temp = 0 then
      raise EOutOfResources.Create('Out of Resources');;
    Handle := temp;
  finally
    if AReferenceDevice = 0 then
      ReleaseDC(0, refDC);
  end;
end;

destructor TMetafileCanvas.Destroy;
begin
  FMetafile.Handle := CloseEnhMetafile(Handle);
  inherited Destroy;
end;

{ TWMFChartHelper }

procedure TWMFChartHelper.CopyToClipboardMetafile;
begin
  Draw(TWindowsMetafileDrawer.Create(''), Rect(0, 0, Width, Height));
end;

procedure TWMFChartHelper.SaveToWMF(const AFileName: String);
begin
  Draw(TWindowsMetafileDrawer.Create(AFilename), Rect(0, 0, Width, Height));
end;
{$ENDIF}

end.

