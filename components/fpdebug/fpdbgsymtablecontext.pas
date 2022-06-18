unit fpDbgSymTableContext;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  FpDbgLoader,
  FpImgReaderBase,
  DbgIntfBaseTypes,
  fpDbgSymTable,
  FpdMemoryTools,
  FpDbgInfo,
  FpDbgCommon;

type

  { TFpSymbolTableProc }

  TFpSymbolTableProc = class(TFpSymbol)
  private
    FLineSym: TFpSymbol;
  protected
    function GetFlags: TDbgSymbolFlags; override;
    function GetLine: Cardinal; override;
    function GetLineStartAddress: TDBGPtr; override;
    function GetLineEndAddress: TDBGPtr; override;
    function GetFile: String; override;
  public
    constructor Create(const AName: String; AnAddr: TDbgPtr);
    destructor Destroy; override;
    procedure SetLineSym(ASym: TFpSymbol);
  end;

  TFpSymbolInfo = class;

  { TFpSymbolContext }

  TFpSymbolContext = class(TFpDbgSymbolScope)
  private
    FFpSymbolInfo: TFpSymbolInfo;
    FSizeOfAddress: integer;
  protected
    function GetSizeOfAddress: Integer; override;
  public
    constructor Create(ALocationContext: TFpDbgLocationContext; AFpSymbolInfo: TFpSymbolInfo);
    function FindSymbol(const AName: String; const OnlyUnitName: String = ''): TFpValue; override;
  end;

  { TFpSymbolInfo }

  TFpSymbolInfo = class(TDbgInfo)
  private
    FSymbolList: TfpSymbolList;
    FLibName: String;
  public
    constructor Create(ALoaderList: TDbgImageLoaderList; AMemManager: TFpDbgMemManager); override;
    constructor Create(ALoaderList: TDbgImageLoaderList; AMemManager: TFpDbgMemManager; ALibName: String);
    destructor Destroy; override;
    function FindSymbolScope(ALocationContext: TFpDbgLocationContext; AAddress: TDbgPtr = 0): TFpDbgSymbolScope; override;
    function FindProcSymbol(const AName: String): TFpSymbol; override; overload;
    function FindProcSymbol(AnAdress: TDbgPtr): TFpSymbol; overload;
  end;

implementation

{ TFpSymbolTableProc }

function TFpSymbolTableProc.GetFlags: TDbgSymbolFlags;
begin
  Result := inherited GetFlags;
  if FLineSym <> nil then
    Result := Result + FLineSym.Flags * [sfHasLine, sfHasLineAddrRng];
end;

function TFpSymbolTableProc.GetLine: Cardinal;
begin
  Result := inherited GetLine;
  if FLineSym <> nil then
    Result := FLineSym.Line;
end;

function TFpSymbolTableProc.GetLineStartAddress: TDBGPtr;
begin
  Result := inherited GetLineStartAddress;
  if FLineSym <> nil then
    Result := FLineSym.LineStartAddress;
end;

function TFpSymbolTableProc.GetLineEndAddress: TDBGPtr;
begin
  Result := inherited GetLineEndAddress;
  if FLineSym <> nil then
    Result := FLineSym.LineEndAddress;
end;

function TFpSymbolTableProc.GetFile: String;
begin
  Result := inherited GetFile;
  if FLineSym <> nil then
    Result := FLineSym.FileName;
end;

constructor TFpSymbolTableProc.Create(const AName: String; AnAddr: TDbgPtr);
begin
  inherited Create(AName);
  SetAddress(TargetLoc(AnAddr));
  SetKind(skProcedure);
  SetSymbolType(stType);
end;

destructor TFpSymbolTableProc.Destroy;
begin
  inherited Destroy;
  FLineSym.ReleaseReference;
end;

procedure TFpSymbolTableProc.SetLineSym(ASym: TFpSymbol);
begin
  FLineSym := ASym;
  if FLineSym <> nil then
    FLineSym.AddReference;
end;

{ TFpSymbolContext }

function TFpSymbolContext.GetSizeOfAddress: Integer;
begin
  result := FSizeOfAddress;
end;

constructor TFpSymbolContext.Create(ALocationContext: TFpDbgLocationContext;
  AFpSymbolInfo: TFpSymbolInfo);
begin
  inherited create(ALocationContext);
  FFpSymbolInfo:=AFpSymbolInfo;
end;

function TFpSymbolContext.FindSymbol(const AName: String;
  const OnlyUnitName: String): TFpValue;
var
  i: integer;
  val: TFpDbgMemLocation;
begin
  i := FFpSymbolInfo.FSymbolList.IndexOf(AName);
  if i > -1 then
  begin
    val := Default(TFpDbgMemLocation);
    val.Address:=FFpSymbolInfo.FSymbolList.DataPtr[i]^.Addr;
    val.MType:=mlfTargetMem;
    result := TFpValueConstAddress.Create(val);
  end
  else
    result := nil;
end;

{ TFpSymbolInfo }

constructor TFpSymbolInfo.Create(ALoaderList: TDbgImageLoaderList;
  AMemManager: TFpDbgMemManager);

var
  i: Integer;
begin
  inherited Create(ALoaderList, AMemManager);

  FSymbolList := TfpSymbolList.Create;
  for i := 0 to ALoaderList.Count-1 do
    ALoaderList[i].ParseSymbolTable(FSymbolList);
  FTargetInfo := ALoaderList.TargetInfo;
  if FSymbolList.Count > 0 then
    SetHasInfo;
end;

constructor TFpSymbolInfo.Create(ALoaderList: TDbgImageLoaderList;
  AMemManager: TFpDbgMemManager; ALibName: String);
begin
  FLibName := ALibName;
  Create(ALoaderList, AMemManager);
end;

destructor TFpSymbolInfo.Destroy;
begin
  FSymbolList.Free;
  inherited Destroy;
end;

function TFpSymbolInfo.FindSymbolScope(ALocationContext: TFpDbgLocationContext;
  AAddress: TDbgPtr): TFpDbgSymbolScope;
begin
  assert(False, 'TFpSymbolInfo.FindSymbolScope: False');
  Result := TFpSymbolContext.Create(ALocationContext, Self);
end;

function TFpSymbolInfo.FindProcSymbol(const AName: String): TFpSymbol;
var
  i: integer;
begin
  i := FSymbolList.IndexOf(AName);
  if i >= 0 then
    Result := TFpSymbolTableProc.Create(AName, FSymbolList.DataPtr[i]^.Addr)
  else
    result := nil;
end;

function TFpSymbolInfo.FindProcSymbol(AnAdress: TDbgPtr): TFpSymbol;
var
  CheckRange: Boolean;
  i, NearestIdx: integer;
  a, NearestAddr: TDBGPtr;
  NPreFix: String;
  d: PfpLinkerSymbol;
begin
  NPreFix := '';
  if FLibName <> '' then
    NPreFix := FLibName+':';
  CheckRange :=
    (FSymbolList.HighAddr > FSymbolList.LowAddr) and
    (AnAdress >= FSymbolList.LowAddr) and
    (AnAdress < FSymbolList.HighAddr);
  NearestIdx := -1;
  NearestAddr := 0;

  Result := nil;
  i := FSymbolList.Count - 1;
  while i >= 0 do begin
    d := FSymbolList.DataPtr[i];
    a := d^.Addr;
    if (a = AnAdress) then begin
      Result := TFpSymbolTableProc.Create(NPreFix + FSymbolList.Keys[i], a);
      exit;
    end;
    if CheckRange and (a <= AnAdress) and (a > NearestAddr) and
       ( (d^.SectionEnd = 0) or (AnAdress <= d^.SectionEnd) )
    then begin
      NearestIdx := i;
      NearestAddr := a;
    end;
    dec(i);
  end;
  if NearestIdx >= 0 then begin
    Result := TFpSymbolTableProc.Create(NPreFix + FSymbolList.Keys[NearestIdx], FSymbolList.DataPtr[NearestIdx]^.Addr);
  end;
end;

end.

