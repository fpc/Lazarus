unit FpImgReaderBase;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef windows}
  Windows, // After LCLType
  {$endif}
  Classes, SysUtils, LazUTF8Classes;

type
  TDbgImageSection = record
    RawData: Pointer;
    Size: QWord;
    VirtualAdress: QWord;
  end;
  PDbgImageSection = ^TDbgImageSection;

  TDbgImageSectionEx = record
    Sect: TDbgImageSection;
    Offs: QWord;
    Loaded: Boolean;
  end;
  PDbgImageSectionEx = ^TDbgImageSectionEx;

  { TDbgFileLoader }
  {$ifdef windows}
    {$define USE_WIN_FILE_MAPPING}
  {$endif}

  TDbgFileLoader = class(TObject)
  private
    {$ifdef USE_WIN_FILE_MAPPING}
    FFileHandle: THandle;
    FMapHandle: THandle;
    FModulePtr: Pointer;
    {$else}
    FStream: TStream;
    FList: TList;
    {$endif}
  public
    constructor Create(AFileName: String);
    {$ifdef USE_WIN_FILE_MAPPING}
    constructor Create(AFileHandle: THandle);
    {$endif}
    destructor Destroy; override;
    function  Read(AOffset, ASize: QWord; AMem: Pointer): QWord;
    function  LoadMemory(AOffset, ASize: QWord; out AMem: Pointer): QWord;
    procedure UnloadMemory(AMem: Pointer);
  end;

  { TDbgImageReader }

  TDbgImageReader = class(TObject) // executable parser
  private
    FImage64Bit: Boolean;
    FImageBase: QWord;
  protected
    function GetSection(const AName: String): PDbgImageSection; virtual; abstract;
    procedure SetImageBase(ABase: QWord);
    procedure SetImage64Bit(AValue: Boolean);
  public
    class function isValid(ASource: TDbgFileLoader): Boolean; virtual; abstract;
    class function UserName: AnsiString; virtual; abstract;
    constructor Create(ASource: TDbgFileLoader; OwnSource: Boolean); virtual;

    property ImageBase: QWord read FImageBase;
    Property Image64Bit: Boolean read FImage64Bit;
    property Section[const AName: String]: PDbgImageSection read GetSection;
  end;
  TDbgImageReaderClass = class of TDbgImageReader;


function GetImageReader(const FileName: string): TDbgImageReader; overload;
function GetImageReader(ASource: TDbgFileLoader; OwnSource: Boolean): TDbgImageReader; overload;
procedure RegisterImageReaderClass(DataSource: TDbgImageReaderClass);

implementation

var
  RegisteredImageReaderClasses  : TFPList;

function GetImageReader(const FileName: string): TDbgImageReader;
begin
  try
    Result := GetImageReader(TDbgFileLoader.Create(FileName), true);
  except
    Result := nil;
  end;
end;

function GetImageReader(ASource: TDbgFileLoader; OwnSource: Boolean): TDbgImageReader;
var
  i   : Integer;
  cls : TDbgImageReaderClass;
begin
  Result := nil;
  if not Assigned(ASource) then Exit;

  for i := 0 to RegisteredImageReaderClasses.Count - 1 do begin
    cls :=  TDbgImageReaderClass(RegisteredImageReaderClasses[i]);
    try
      if cls.isValid(ASource) then begin
        Result := cls.Create(ASource, OwnSource);
        Exit;
      end
      else
        ;
    except
      on e: exception do begin
        //writeln('exception! WHY? ', e.Message);
      end;
    end;
  end;
  Result := nil;
end;

procedure RegisterImageReaderClass( DataSource: TDbgImageReaderClass);
begin
  if Assigned(DataSource) and (RegisteredImageReaderClasses.IndexOf(DataSource) < 0) then
    RegisteredImageReaderClasses.Add(DataSource)
end;


{ TDbgFileLoader }

constructor TDbgFileLoader.Create(AFileName: String);
begin
  {$ifdef USE_WIN_FILE_MAPPING}
  FFileHandle := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Create(FFileHandle);
  {$else}
  FList := TList.Create;
  FStream := TFileStreamUTF8.Create(AFileName, fmOpenRead or fmShareDenyNone);
  inherited Create;
  {$endif}
end;

{$ifdef USE_WIN_FILE_MAPPING}
constructor TDbgFileLoader.Create(AFileHandle: THandle);
begin
  FFileHandle := AFileHandle;
  if FFileHandle = INVALID_HANDLE_VALUE
  then begin
    raise Exception.Create('Invalid file handle');
  end;

  FMapHandle := CreateFileMapping(FFileHandle, nil, PAGE_READONLY{ or SEC_IMAGE}, 0, 0, nil);
  if FMapHandle = 0
  then begin
    raise Exception.Create('Could not create module mapping');
    Exit;
  end;

  FModulePtr := MapViewOfFile(FMapHandle, FILE_MAP_READ, 0, 0, 0);
  if FModulePtr = nil
  then begin
    raise Exception.Create('Could not map view');
    Exit;
  end;

  inherited Create;
end;
{$endif}

destructor TDbgFileLoader.Destroy;
begin
  {$ifdef USE_WIN_FILE_MAPPING}
  if FModulePtr <> nil
  then UnmapViewOfFile(FModulePtr);
  if FMapHandle <> 0
  then CloseHandle(FMapHandle);
  if FFileHandle <> INVALID_HANDLE_VALUE
  then CloseHandle(FFileHandle);
  {$else}
  while FList.Count > 0 do
    UnloadMemory(FList[0]);
  FreeAndNil(FList);
  FreeAndNil(FStream);
  inherited Destroy;
  {$endif}
end;

function TDbgFileLoader.Read(AOffset, ASize: QWord; AMem: Pointer): QWord;
begin
  {$ifdef USE_WIN_FILE_MAPPING}
  move((FModulePtr + AOffset)^, AMem^, ASize);
  Result := ASize;
  {$else}
  Result := 0;
  if AMem = nil then
    exit;
  FStream.Position := AOffset;
  Result := FStream.Read(AMem^, ASize);
  {$endif}
end;

function TDbgFileLoader.LoadMemory(AOffset, ASize: QWord; out AMem: Pointer): QWord;
begin
  {$ifdef USE_WIN_FILE_MAPPING}
  AMem := FModulePtr + AOffset;
  Result := ASize;
  {$else}
  Result := 0;
  AMem := AllocMem(ASize);
  if AMem = nil then
    exit;
  FList.Add(AMem);
  FStream.Position := AOffset;
  Result := FStream.Read(AMem^, ASize);
  {$endif}
end;

procedure TDbgFileLoader.UnloadMemory(AMem: Pointer);
begin
  {$ifdef USE_WIN_FILE_MAPPING}
  {$else}
  FList.Remove(AMem);
  Freemem(AMem);
  {$endif}
end;

{ TDbgImageReader }

procedure TDbgImageReader.SetImageBase(ABase: QWord);
begin
  FImageBase := ABase;
end;

procedure TDbgImageReader.SetImage64Bit(AValue: Boolean);
begin
  FImage64Bit := AValue;
end;

constructor TDbgImageReader.Create(ASource: TDbgFileLoader; OwnSource: Boolean);
begin
  inherited Create;
end;


procedure InitDebugInfoLists;
begin
  RegisteredImageReaderClasses := TFPList.Create;
end;

procedure ReleaseDebugInfoLists;
begin
  FreeAndNil(RegisteredImageReaderClasses);
end;

initialization
  InitDebugInfoLists;

finalization
  ReleaseDebugInfoLists;

end.

