unit fpDbgSymTable;

{$mode objfpc}{$H+}
{$IFDEF INLINE_OFF}{$INLINE OFF}{$ENDIF}

interface

uses
  DbgIntfBaseTypes, LazLoggerBase, FpDbgUtil,
  fgl, generics.Collections, Classes, sysutils;

type

  TfpLinkerSymbol = record
    Name: string;
    SectionEnd: TDBGPtr; // Max upper Addr bound
    Next: integer;
  end;
  PfpLinkerSymbol = ^TfpLinkerSymbol;

  { TfpSymbolList }

  TfpSymbolList= class(specialize TFPGMap<TDBGPtr, TfpLinkerSymbol>)
  private type
    TNameDict = specialize TDictionary<String, Integer>;
  private const
    LIST_INIT_WAITING = 0;
    LIST_INIT_RUN = 1;
    LIST_INIT_DONE = 2;
  private
    FHighAddr: TDBGPtr;
    FLowAddr: TDBGPtr;
    FFirstAddr, FLastAddr: TDBGPtr;
    FNameDict: TNameDict;
    FInitSortState, FInitHashState: integer;
    FThreadQueue: TFpGlobalThreadWorkerQueue;
    function GetDataPtr(const AIndex: Integer): PfpLinkerSymbol;
    procedure InitSort;
    procedure InitHashes;
    procedure WaitForSortInit;
    procedure WaitForHashInit;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SortAndHash;
    procedure SetAddressBounds(ALowAddr, AHighAddr: TDBGPtr);
    function GetInfo(const AName: String; out AnAddr: TDBGPtr; out AFoundName: String; ACaseSense: Boolean = False): Boolean;
    function GetInfo(AnAddr: TDBGPtr; out AFoundAddr: TDBGPtr; out AFoundName: String; AnExact: Boolean = True): Boolean;
    property LowAddr: TDBGPtr read FLowAddr;
    property HighAddr: TDBGPtr read FHighAddr;
    function Add(const AName: String; const AnAddr: TDBGPtr; ASectionEnd: TDBGPtr = 0): Integer; inline; overload;
    property DataPtr[const AIndex: Integer]: PfpLinkerSymbol read GetDataPtr;
    property FirstAddr: TDBGPtr read FFirstAddr;
    property LastAddr: TDBGPtr read FLastAddr;
  end;

  { TFpThreadWorkerSymbolList }

  TFpThreadWorkerSymbolList  = class(TFpThreadWorkerItem)
  strict private
    FList: TfpSymbolList;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AList: TfpSymbolList);
  end;

implementation

{ TfpSymbolList }

function TfpSymbolList.GetDataPtr(const AIndex: Integer): PfpLinkerSymbol;
begin
  Result := PfpLinkerSymbol(TFPSMap(Self).Data[AIndex]);
end;

procedure TfpSymbolList.InitSort;
var
  p: PfpLinkerSymbol;
begin
  Sorted := True;
  if Count = 0 then
    exit;
  FFirstAddr := Keys[0];

  p := PfpLinkerSymbol(TFPSMap(Self).Data[Count-1]);
  FLastAddr := Keys[Count-1];

  if p ^.SectionEnd > FLastAddr then
    FLastAddr := p ^.SectionEnd
  else
    FLastAddr := FLastAddr + 16000; // Just some bit after the last sym
end;

procedure TfpSymbolList.InitHashes;
var
  i, j: Integer;
  p, p2: PfpLinkerSymbol;
  s: String;
begin
  FNameDict.Capacity := Count + Count div 2;
  for i := 0 to Count - 1 do begin
    p := PfpLinkerSymbol(TFPSMap(Self).Data[i]);
    s := UpperCase(p^.Name);
    if FNameDict.TryGetValue(s, j) then begin
      p2 := PfpLinkerSymbol(TFPSMap(Self).Data[j]);
      while p2^.Next >= 0 do
        p2 := PfpLinkerSymbol(TFPSMap(Self).Data[p2^.Next]);
      p2^.Next := i;
    end
    else
      FNameDict.Add(s, i);
  end;
end;

procedure TfpSymbolList.WaitForSortInit;
begin
  if FInitSortState = LIST_INIT_DONE then
    exit;

  if InterlockedCompareExchange(FInitSortState, LIST_INIT_RUN, LIST_INIT_WAITING) = LIST_INIT_WAITING then begin
    InitSort;
    FInitSortState := LIST_INIT_DONE;
    exit;
  end;

  // might be called from within worker thread
  ReadBarrier;
  while FInitSortState <> LIST_INIT_DONE do begin
    sleep(1);
    ReadBarrier;
  end;
end;

procedure TfpSymbolList.WaitForHashInit;
begin
  if FInitHashState = LIST_INIT_DONE then
    exit;

  if InterlockedCompareExchange(FInitHashState, LIST_INIT_RUN, LIST_INIT_WAITING) = LIST_INIT_WAITING then begin
    WaitForSortInit;
    InitHashes;
    FInitHashState := LIST_INIT_DONE;
    exit;
  end;

  ReadBarrier;
  while FInitHashState <> LIST_INIT_DONE do begin
    sleep(1);
    ReadBarrier;
  end;
end;

constructor TfpSymbolList.Create;
begin
  FNameDict := TNameDict.Create;
  inherited;
end;

destructor TfpSymbolList.Destroy;
begin
  inherited Destroy;
  if FThreadQueue <> nil then
    FThreadQueue.DecRef;
  FNameDict.Destroy;
end;

procedure TfpSymbolList.SortAndHash;
var
  w: TFpThreadWorkerSymbolList;
begin
  FNameDict.Clear;
  FInitSortState := LIST_INIT_WAITING;
  ReadBarrier;
  if FThreadQueue = nil then begin
    FThreadQueue := FpDbgGlobalWorkerQueue;
    FThreadQueue.AddRef;
  end;
  w := TFpThreadWorkerSymbolList.Create(Self);
  FThreadQueue.PushItem(w);
end;

procedure TfpSymbolList.SetAddressBounds(ALowAddr, AHighAddr: TDBGPtr);
begin
  FLowAddr := ALowAddr;
  FHighAddr := AHighAddr;
end;

function TfpSymbolList.GetInfo(const AName: String; out AnAddr: TDBGPtr; out AFoundName: String;
  ACaseSense: Boolean): Boolean;
var
  i: LongInt;
  p: PfpLinkerSymbol;
begin
  WaitForHashInit;

  AnAddr := 0;
  AFoundName := '';
  Result := FNameDict.TryGetValue(UpperCase(AName), i);
  if not Result then
    exit;
  p := PfpLinkerSymbol(TFPSMap(Self).Data[i]);

  if ACaseSense then begin
    while (p^.Name <> AName) do begin
      if p^.Next < 0 then
        exit(False);
      i := p^.Next;
      p := PfpLinkerSymbol(TFPSMap(Self).Data[i]);
    end;
  end;

  Result := i >= 0;
  if Result then begin
    AnAddr := Keys[i];
    AFoundName := p^.Name;
  end;
end;

function TfpSymbolList.GetInfo(AnAddr: TDBGPtr; out AFoundAddr: TDBGPtr; out AFoundName: String;
  AnExact: Boolean): Boolean;
var
  L, H, M: Integer;
  K: TDBGPtr;
  p: PfpLinkerSymbol;
begin
  WaitForSortInit;

  AFoundAddr := 0;
  AFoundName := '';
  Result := False;

  H := Count-1;
  if H < 0 then
    exit;
  L := 0;
  while L<H do begin
    M := (L + H) div 2;
    K := Keys[M];
    if K < AnAddr then
      L := M+1
    else
      H := M;
  end;
  K := Keys[L];
  if K > AnAddr then begin
    dec(L);
    if L < 0 then
      exit;
    K := Keys[L];
  end;

  if AnExact and (K <> AnAddr) then
    exit;

  p := PfpLinkerSymbol(TFPSMap(Self).Data[L]);
  Result := (p^.SectionEnd = 0) or (AnAddr <= p^.SectionEnd);
  if Result then begin
    AFoundAddr := K;
    AFoundName := p^.Name;
  end;
end;

function TfpSymbolList.Add(const AName: String; const AnAddr: TDBGPtr;
  ASectionEnd: TDBGPtr): Integer;
var
  d: TfpLinkerSymbol;
begin
  d.Name := AName;
  d.SectionEnd := ASectionEnd;
  d.Next := -1;
  Result := Add(AnAddr, d);
end;

{ TFpThreadWorkerSymbolList }

procedure TFpThreadWorkerSymbolList.DoExecute;
begin
  if InterlockedCompareExchange(FList.FInitSortState, FList.LIST_INIT_RUN, FList.LIST_INIT_WAITING) = FList.LIST_INIT_WAITING then begin
    FList.InitSort;
    WriteBarrier;
    FList.FInitSortState := FList.LIST_INIT_DONE;
  end;

  if InterlockedCompareExchange(FList.FInitHashState, FList.LIST_INIT_RUN, FList.LIST_INIT_WAITING) = FList.LIST_INIT_WAITING then begin
    FList.WaitForSortInit;
    FList.InitHashes;
    WriteBarrier;
    FList.FInitHashState := FList.LIST_INIT_DONE;
  end;
end;

constructor TFpThreadWorkerSymbolList.Create(AList: TfpSymbolList);
begin
  FList := AList;
  inherited Create;
end;

end.

