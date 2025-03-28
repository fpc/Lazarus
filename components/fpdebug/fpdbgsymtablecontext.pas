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
    constructor Create(ALocationContext: TFpDbgSimpleLocationContext; AFpSymbolInfo: TFpSymbolInfo);
    function FindSymbol(const AName: String; const OnlyUnitName: String = '';
      AFindFlags: TFindExportedSymbolsFlags = []): TFpValue; override;
  end;

  { TFpSymbolInfo }

  TFpSymbolInfo = class(TDbgInfo)
  private
    FSymbolList: TfpSymbolList;
    FLibName: String;
    function GetSymbols(AnIndex: integer): TFpSymbol;
  public
    constructor Create(ALoaderList: TDbgImageLoaderList; AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel); override; overload;
    constructor Create(ALoaderList: TDbgImageLoaderList; AMemManager: TFpDbgMemManager; ALibName: String; AMemModel: TFpDbgMemModel); overload;
    destructor Destroy; override;
    function FindSymbolScope(ALocationContext: TFpDbgSimpleLocationContext; AAddress: TDbgPtr = 0): TFpDbgSymbolScope; override;
    function FindProcSymbol(const AName: String; AIgnoreCase: Boolean = False): TFpSymbol; override; overload;
    function FindProcSymbol(AnAdress: TDbgPtr): TFpSymbol; overload;

    // for debugdump
    function SymbolCount: integer;
    property Symbols[AnIndex: integer]: TFpSymbol read GetSymbols;
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

constructor TFpSymbolContext.Create(ALocationContext: TFpDbgSimpleLocationContext;
  AFpSymbolInfo: TFpSymbolInfo);
begin
  inherited create(ALocationContext);
  FFpSymbolInfo:=AFpSymbolInfo;
end;

function TFpSymbolContext.FindSymbol(const AName: String; const OnlyUnitName: String;
  AFindFlags: TFindExportedSymbolsFlags): TFpValue;
var
  val: TFpDbgMemLocation;
  a: TDBGPtr;
  n: string;
begin
  // TODO: case sense?
  if FFpSymbolInfo.FSymbolList.GetInfo(AName, a, n) then
  begin
    val := Default(TFpDbgMemLocation);
    val.Address:=a;
    val.MType:=mlfTargetMem;
    result := TFpValueConstAddress.Create(val);
  end
  else
    result := nil;
end;

{ TFpSymbolInfo }

function TFpSymbolInfo.GetSymbols(AnIndex: integer): TFpSymbol;
var
  p: PfpLinkerSymbol;
begin
  p := FSymbolList.DataPtr[AnIndex];
  Result := TFpSymbolTableProc.Create(p^.Name, FSymbolList.Keys[AnIndex]);
end;

constructor TFpSymbolInfo.Create(ALoaderList: TDbgImageLoaderList;
  AMemManager: TFpDbgMemManager; AMemModel: TFpDbgMemModel);

var
  i: Integer;
begin
  inherited Create(ALoaderList, AMemManager, AMemModel);

  FSymbolList := TfpSymbolList.Create;
  for i := 0 to ALoaderList.Count-1 do
    ALoaderList[i].ParseSymbolTable(FSymbolList);
  FTargetInfo := ALoaderList.TargetInfo;
  if FSymbolList.Count > 0 then begin
    SetHasInfo;
    FSymbolList.SortAndHash;
  end;
end;

constructor TFpSymbolInfo.Create(ALoaderList: TDbgImageLoaderList;
  AMemManager: TFpDbgMemManager; ALibName: String; AMemModel: TFpDbgMemModel);
begin
  FLibName := ALibName;
  Create(ALoaderList, AMemManager, AMemModel);
end;

destructor TFpSymbolInfo.Destroy;
begin
  FSymbolList.Free;
  inherited Destroy;
end;

function TFpSymbolInfo.FindSymbolScope(ALocationContext: TFpDbgSimpleLocationContext;
  AAddress: TDbgPtr): TFpDbgSymbolScope;
begin
  assert(False, 'TFpSymbolInfo.FindSymbolScope: False');
  Result := TFpSymbolContext.Create(ALocationContext, Self);
end;

function TFpSymbolInfo.FindProcSymbol(const AName: String; AIgnoreCase: Boolean
  ): TFpSymbol;
var
  a: TDBGPtr;
  n: string;
begin
  if FSymbolList.GetInfo(AName, a, n, not AIgnoreCase) then
    Result := TFpSymbolTableProc.Create(n, a)
  else
    result := nil;
end;

function TFpSymbolInfo.FindProcSymbol(AnAdress: TDbgPtr): TFpSymbol;
var
  CheckRange: Boolean;
  NPreFix, n: String;
  a: TDBGPtr;
begin
  Result := nil;
  if (AnAdress < FSymbolList.FirstAddr) or (AnAdress > FSymbolList.LastAddr) then
    exit;

  NPreFix := '';
  if FLibName <> '' then
    NPreFix := FLibName+':';
  CheckRange :=
    (FSymbolList.HighAddr > FSymbolList.LowAddr) and
    (AnAdress >= FSymbolList.LowAddr) and
    (AnAdress < FSymbolList.HighAddr);

  if FSymbolList.GetInfo(AnAdress, a, n, not CheckRange) then
    Result := TFpSymbolTableProc.Create(NPreFix + n, a);
end;

function TFpSymbolInfo.SymbolCount: integer;
begin
  Result := FSymbolList.Count;
end;

end.

