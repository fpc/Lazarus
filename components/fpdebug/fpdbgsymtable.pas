unit fpDbgSymTable;

{$mode objfpc}{$H+}

interface

uses
  DbgIntfBaseTypes,
  fgl, Classes;

type

  TfpLinkerSymbol = record
    Addr: TDBGPtr;
    SectionEnd: TDBGPtr; // Max upper Addr bound
  end;
  PfpLinkerSymbol = ^TfpLinkerSymbol;

  { TfpSymbolList }

  // TODO: TFPGMapObject, if we store more data
  TfpSymbolList= class(specialize TFPGMap<String, TfpLinkerSymbol>)
  private
    FHighAddr: TDBGPtr;
    FLowAddr: TDBGPtr;
    function GetDataPtr(const AIndex: Integer): PfpLinkerSymbol;
    function GetKeyDataPtr(const AKey: string): PfpLinkerSymbol;
  public
    procedure SetAddressBounds(ALowAddr, AHighAddr: TDBGPtr);
    property LowAddr: TDBGPtr read FLowAddr;
    property HighAddr: TDBGPtr read FHighAddr;
    function Add(const AKey: String; const AData: TDBGPtr): Integer; inline; overload;
    function Add(const AKey: String; const AData, ASectionEnd: TDBGPtr): Integer; inline; overload;
    property KeyDataPtr[const AKey: string]: PfpLinkerSymbol read GetKeyDataPtr;
    property DataPtr[const AIndex: Integer]: PfpLinkerSymbol read GetDataPtr;
  end;

implementation

{ TfpSymbolList }

function TfpSymbolList.GetDataPtr(const AIndex: Integer): PfpLinkerSymbol;
begin
  Result := PfpLinkerSymbol(TFPSMap(Self).Data[AIndex]);
end;

function TfpSymbolList.GetKeyDataPtr(const AKey: string): PfpLinkerSymbol;
begin
  Result := PfpLinkerSymbol(TFPSMap(Self).KeyData[@AKey]);
end;

procedure TfpSymbolList.SetAddressBounds(ALowAddr, AHighAddr: TDBGPtr);
begin
  FLowAddr := ALowAddr;
  FHighAddr := AHighAddr;
end;

function TfpSymbolList.Add(const AKey: String; const AData: TDBGPtr): Integer;
var
  d: TfpLinkerSymbol;
begin
  d.Addr := AData;
  d.SectionEnd := 0;
  Result := Add(AKey, d);
end;

function TfpSymbolList.Add(const AKey: String; const AData, ASectionEnd: TDBGPtr
  ): Integer;
var
  d: TfpLinkerSymbol;
begin
  d.Addr := AData;
  d.SectionEnd := ASectionEnd;
  Result := Add(AKey, d);
end;

end.

