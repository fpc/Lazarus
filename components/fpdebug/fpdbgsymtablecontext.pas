unit fpDbgSymTableContext;

{$mode objfpc}{$H+}

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
  public
    constructor Create(const AName: String; AnAddr: TDbgPtr);
  end;

  TFpSymbolInfo = class;

  { TFpSymbolContext }

  TFpSymbolContext = class(TFpDbgInfoContext)
  private
    FFpSymbolInfo: TFpSymbolInfo;
    FSizeOfAddress: integer;
  protected
    function GetAddress: TDbgPtr; override;
    function GetStackFrame: Integer; override;
    function GetThreadId: Integer; override;
    function GetSizeOfAddress: Integer; override;
  public
    constructor Create(AFpSymbolInfo: TFpSymbolInfo);
    function FindSymbol(const AName: String): TFpValue; override;
  end;

  { TFpSymbolInfo }

  TFpSymbolInfo = class(TDbgInfo)
  private
    FSymbolList: TfpSymbolList;
    FContext: TFpSymbolContext;
  public
    constructor Create(ALoaderList: TDbgImageLoaderList); override;
    destructor Destroy; override;
    function FindContext(AThreadId, AStackFrame: Integer; AAddress: TDbgPtr = 0): TFpDbgInfoContext; override;
    function FindContext(AAddress: TDbgPtr): TFpDbgInfoContext; override;
    function FindProcSymbol(const AName: String): TFpSymbol; override; overload;
    function FindProcSymbol(AnAdress: TDbgPtr): TFpSymbol; overload;
  end;

implementation

{ TFpSymbolTableProc }

constructor TFpSymbolTableProc.Create(const AName: String; AnAddr: TDbgPtr);
begin
  inherited Create(AName);
  SetAddress(TargetLoc(AnAddr));
  SetKind(skProcedure);
  SetSymbolType(stType);
end;

{ TFpSymbolContext }

function TFpSymbolContext.GetAddress: TDbgPtr;
begin
  result := 0;
end;

function TFpSymbolContext.GetStackFrame: Integer;
begin
  result := 0;
end;

function TFpSymbolContext.GetThreadId: Integer;
begin
  result := 1;
end;

function TFpSymbolContext.GetSizeOfAddress: Integer;
begin
  result := FSizeOfAddress;
end;

constructor TFpSymbolContext.Create(AFpSymbolInfo: TFpSymbolInfo);
begin
  inherited create;
  FFpSymbolInfo:=AFpSymbolInfo;
  if AFpSymbolInfo.TargetInfo.bitness = b64 then
    FSizeOfAddress:=8
  else
    FSizeOfAddress:=4;
end;

function TFpSymbolContext.FindSymbol(const AName: String): TFpValue;
var
  i: integer;
  val: TFpDbgMemLocation;
begin
  i := FFpSymbolInfo.FSymbolList.IndexOf(AName);
  if i > -1 then
  begin
    val := Default(TFpDbgMemLocation);
    val.Address:=FFpSymbolInfo.FSymbolList.Data[i];
    val.MType:=mlfTargetMem;
    result := TFpValueConstAddress.Create(val);
  end
  else
    result := nil;
end;

{ TFpSymbolInfo }

constructor TFpSymbolInfo.Create(ALoaderList: TDbgImageLoaderList);

var
  i: Integer;
begin
  inherited Create(ALoaderList);
  FContext := TFpSymbolContext.Create(self);

  FSymbolList := TfpSymbolList.Create;
  for i := 0 to ALoaderList.Count-1 do
    ALoaderList[i].ParseSymbolTable(FSymbolList);
  FTargetInfo := ALoaderList.TargetInfo;
  if FSymbolList.Count > 0 then
    SetHasInfo;
end;

destructor TFpSymbolInfo.Destroy;
begin
  FSymbolList.Free;
  FContext.Free;
  inherited Destroy;
end;

function TFpSymbolInfo.FindContext(AThreadId, AStackFrame: Integer;
  AAddress: TDbgPtr): TFpDbgInfoContext;
begin
  assert(False, 'TFpSymbolInfo.FindContext: False');
  Result:=FContext; // TODO: nil
end;

function TFpSymbolInfo.FindContext(AAddress: TDbgPtr): TFpDbgInfoContext;
begin
  assert(False, 'TFpSymbolInfo.FindContext: False');
  Result:=FContext; // TODO: nil
end;

function TFpSymbolInfo.FindProcSymbol(const AName: String): TFpSymbol;
var
  i: integer;
begin
  i := FSymbolList.IndexOf(AName);
  if i >= 0 then
    Result := TFpSymbolTableProc.Create(AName, FSymbolList.Data[i])
  else
    result := nil;
end;

function TFpSymbolInfo.FindProcSymbol(AnAdress: TDbgPtr): TFpSymbol;
var
  i: integer;
begin
  Result := nil;
  i := FSymbolList.Count - 1;
  while i >= 0 do begin
    if FSymbolList.Data[i] = AnAdress then begin
      Result := TFpSymbolTableProc.Create(FSymbolList.Keys[i], FSymbolList.Data[i]);
      exit;
    end;
    dec(i);
  end;
end;

end.

