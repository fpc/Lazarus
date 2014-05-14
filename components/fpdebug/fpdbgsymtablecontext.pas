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
  FpDbgInfo;

type

  TFpSymbolInfo = class;

  { TFpSymbolContext }

  TFpSymbolContext = class(TFpDbgInfoContext)
  private
    FFpSymbolInfo: TFpSymbolInfo;
  protected
    function GetAddress: TDbgPtr; override;
    function GetStackFrame: Integer; override;
    function GetThreadId: Integer; override;
    function GetSizeOfAddress: Integer; override;
  public
    constructor Create(AFpSymbolInfo: TFpSymbolInfo);
    function FindSymbol(const AName: String): TFpDbgValue; override;
  end;

  { TFpSymbolInfo }

  TFpSymbolInfo = class(TDbgInfo)
  private
    FSymbolList: TfpSymbolList;
    FContext: TFpSymbolContext;
  public
    constructor Create(ALoader: TDbgImageLoader); override;
    destructor Destroy; override;
    function FindContext(AThreadId, AStackFrame: Integer; AAddress: TDbgPtr = 0): TFpDbgInfoContext; override;
    function FindContext(AAddress: TDbgPtr): TFpDbgInfoContext; override;
    function FindSymbol(AAddress: TDbgPtr): TFpDbgSymbol; override;
    function FindSymbol(const AName: String): TFpDbgSymbol; override;
    function GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr; override;
  end;

implementation

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
  result := 4;
end;

constructor TFpSymbolContext.Create(AFpSymbolInfo: TFpSymbolInfo);
begin
  inherited create;
  FFpSymbolInfo:=AFpSymbolInfo;
end;

function TFpSymbolContext.FindSymbol(const AName: String): TFpDbgValue;
var
  i: integer;
  val: TFpDbgMemLocation;
begin
  i := FFpSymbolInfo.FSymbolList.IndexOf(AName);
  if i > -1 then
  begin
    val.Address:=TDbgPtr(pointer(FFpSymbolInfo.FSymbolList.Objects[i]));
    val.MType:=mlfTargetMem;
    result := TFpDbgValueConstAddress.Create(val);
  end
  else
    result := nil;
end;

{ TFpSymbolInfo }

constructor TFpSymbolInfo.Create(ALoader: TDbgImageLoader);

begin
  inherited Create(ALoader);
  FContext := TFpSymbolContext.Create(self);

  FSymbolList := TfpSymbolList.Create;
  ALoader.ParseSymbolTable(FSymbolList);
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
  Result:=FContext;
end;

function TFpSymbolInfo.FindContext(AAddress: TDbgPtr): TFpDbgInfoContext;
begin
  Result:=FContext;
end;

function TFpSymbolInfo.FindSymbol(AAddress: TDbgPtr): TFpDbgSymbol;
begin
  Result:=inherited FindSymbol(AAddress);
end;

function TFpSymbolInfo.FindSymbol(const AName: String): TFpDbgSymbol;
begin
  result := nil;
  //Result:=FContext.FindSymbol(AName);
end;

function TFpSymbolInfo.GetLineAddress(const AFileName: String; ALine: Cardinal): TDbgPtr;
begin
  Result:=inherited GetLineAddress(AFileName, ALine);
end;

end.

