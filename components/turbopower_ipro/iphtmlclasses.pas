unit IpHtmlClasses;

{$mode Delphi}

interface

uses
  Classes, SysUtils, Types, Graphics, Forms,
  IpHtmlTypes;
 
type
  { Integer property which can be scaled}
  TIpHtmlInteger = class(TPersistent)
  private
    FValue : Integer;
    FChange: TNotifyEvent;
    procedure DoChange;
    function GetValue: Integer;
    procedure SetValue(const Value: Integer);
  public
    constructor Create(AValue: Integer);
    property Value: Integer read GetValue write SetValue;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlPixels = class(TPersistent)
  private
    FValue : Integer;
    FPixelsType : TIpHtmlPixelsType;
    FChange: TNotifyEvent;
    procedure DoChange;
    function GetValue: Integer;
    procedure SetPixelsType(const Value: TIpHtmlPixelsType);
    procedure SetValue(const Value: Integer);
  public
    property Value: Integer read GetValue write SetValue;
    property PixelsType: TIpHtmlPixelsType read FPixelsType write SetPixelsType;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlLength = class(TPersistent)
  private
    FLengthValue: Integer;
    FLengthType: TIpHtmlLengthType;
    FChange: TNotifyEvent;
    procedure SetLengthType(const Value: TIpHtmlLengthType);
    procedure SetLengthValue(const Value: Integer);
    function GetLengthValue: Integer;
    procedure DoChange;
  public
    property LengthValue : Integer read GetLengthValue write SetLengthValue;
    property LengthType : TIpHtmlLengthType read FLengthType write SetLengthType;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TIpHtmlMultiLengthType = (hmlUndefined, hmlAbsolute, hmlPercent, hmlRelative);
  TIpHtmlMultiLength = class(TPersistent)
  private
    FLengthValue : Integer;
    FLengthType : TIpHtmlMultiLengthType;
    function GetLengthValue: Integer;
  public
    property LengthValue: Integer read GetLengthValue write FLengthValue;
    property LengthType: TIpHtmlMultiLengthType read FLengthType write FLengthType;
  end;

  TIpHtmlMultiLengthList = class(TPersistent)
  private
    List: TFPList;
    function GetEntries: Integer;
    function GetValues(Index: Integer): TIpHtmlMultiLength;
  public
    constructor Create;
    destructor Destroy; override;
    property Values[Index: Integer]: TIpHtmlMultiLength read GetValues;
    procedure AddEntry(Value: TIpHtmlMultiLength);
    procedure Clear;
    property Entries: Integer read GetEntries;
  end;

  TIpHtmlRelSizeType = (hrsUnspecified, hrsAbsolute, hrsRelative);
  TIpHtmlRelSize = class(TPersistent)
  private
    FChange: TNotifyEvent;
    FSizeType : TIpHtmlRelSizeType;
    FValue : Integer;
    procedure SetSizeType(const Value: TIpHtmlRelSizeType);
    procedure SetValue(const Value: Integer);
    procedure DoChange;
  public
    property SizeType : TIpHtmlRelSizeType read FSizeType write SetSizeType;
    property Value : Integer read FValue write SetValue;
    property OnChange: TNotifyEvent read FChange write FChange;
  end;

  TInternalIntArr = array [0..Pred(MAXINTS)] of Integer;
  PInternalIntArr = ^TInternalIntArr;
  
  TIntArr = class
  private
    InternalIntArr : PInternalIntArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): Integer;
    procedure SetValue(Index, Value: Integer);
  public
    destructor Destroy; override;
    property Value[Index: Integer]: Integer read GetValue write SetValue; default;
  end;

  TInternalRectArr = array [0..Pred(MAXINTS)] of PRect;
  PInternalRectArr = ^TInternalRectArr;
  
  TRectArr = class
  private
    InternalRectArr : PInternalRectArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): PRect;
    procedure SetValue(Index: Integer; Value: PRect);
  public
    destructor Destroy; override;
    property Value[Index: Integer]: PRect read GetValue write SetValue; default;
  end;

  TInternalRectRectArr = array [0..Pred(MAXINTS)] of TRectArr;
  PInternalRectRectArr = ^TInternalRectRectArr;
  
  TRectRectArr = class
  protected
    InternalRectRectArr : PInternalRectRectArr;
    IntArrSize : Integer;
    function GetValue(Index: Integer): TRectArr;
  public
    destructor Destroy; override;
    property Value[Index: Integer]: TRectArr read GetValue; default;
    procedure Delete(Index: Integer);
  end;

  TIpHtmlPreviewSettings = class(TPersistent)
  private
    FAntiAliasingMode: TAntiAliasingMode;
    FPosition: TPosition;
    FMaximized: Boolean;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FZoom: Integer;
  public
    constructor Create;
  published
    property AntiAliasingMode: TAntiAliasingMode
      read FAntiAliasingMode write FAntiAliasingMode default amDontCare;
    property Position: TPosition
      read FPosition write FPosition default poScreenCenter;
    property Maximized: Boolean
      read FMaximized write FMaximized default false;
    property Left: Integer
      read FLeft write FLeft;
    property Top: Integer
      read FTop write FTop;
    property Width: Integer
      read FWidth write FWidth;
    property Height: Integer
      read FHeight write FHeight;
    property Zoom: integer
      read FZoom write FZoom default 100;
  end;

  TIpHtmlPrintSettings = class(TPersistent)
  private
    FPreview: TIpHtmlPreviewSettings;
    FMarginTop: Double;
    FMarginLeft: Double;
    FMarginBottom: Double;
    FMarginRight: Double;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property MarginLeft: Double read FMarginLeft write FMarginLeft;
    property MarginTop: Double read FMarginTop write FMarginTop;
    property MarginRight: Double read FMarginRight write FMarginRight;
    property MarginBottom: Double read FMarginBottom write FMarginBottom;
    property Preview: TIpHtmlPreviewSettings read FPreview write FPreview;
  end;
 
  
implementation

{ TIpHtmlInteger }

constructor TIpHtmlInteger.Create(AValue: Integer);
begin
  inherited Create;
  FValue := AValue;
end;

procedure TIpHtmlInteger.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlInteger.GetValue: Integer;
begin
  if ScaleBitmaps then
    Result := round(FValue * Aspect)
  else
    Result := FValue;
end;

procedure TIpHtmlInteger.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;


{ TIpHtmlPixels }

procedure TIpHtmlPixels.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlPixels.GetValue: Integer;
begin
  if (PixelsType = hpAbsolute) and ScaleBitmaps then
    Result := round(FValue * Aspect)
  else
    Result := FValue;
end;

procedure TIpHtmlPixels.SetPixelsType(const Value: TIpHtmlPixelsType);
begin
  if Value <> FPixelsType then begin
    FPixelsType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlPixels.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;


{ TIpHtmlRelSize }

procedure TIpHtmlRelSize.DoChange;
begin
  if assigned(FChange) then
    FChange(Self);
end;

procedure TIpHtmlRelSize.SetSizeType(const Value: TIpHtmlRelSizeType);
begin
  if Value <> FSizeType then begin
    FSizeType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlRelSize.SetValue(const Value: Integer);
begin
  if Value <> FValue then begin
    FValue := Value;
    DoChange;
  end;
end;


{ TIpHtmlLength }

procedure TIpHtmlLength.DoChange;
begin
  if Assigned(FChange) then
    FChange(Self);
end;

function TIpHtmlLength.GetLengthValue: Integer;
begin
  if (LengthType = hlAbsolute) and ScaleBitmaps then
    Result := round(FLengthValue * Aspect)
  else
    Result := FLengthValue;
end;

procedure TIpHtmlLength.SetLengthType(const Value: TIpHtmlLengthType);
begin
  if Value <> FLengthType then begin
    FLengthType := Value;
    DoChange;
  end;
end;

procedure TIpHtmlLength.SetLengthValue(const Value: Integer);
begin
  if Value <> FLengthValue then begin
    FLengthValue := Value;
    DoChange;
  end;
end;


{ TIpHtmlMultiLength }

function TIpHtmlMultiLength.GetLengthValue: Integer;
begin
  if (LengthType = hmlAbsolute) and ScaleBitmaps then
    Result := round(FLengthValue * Aspect)
  else
    Result := FLengthValue;
end;


{ TIpHtmlMultiLengthList }

procedure TIpHtmlMultiLengthList.AddEntry(Value: TIpHtmlMultiLength);
begin
  List.Add(Value);
end;

procedure TIpHtmlMultiLengthList.Clear;
begin
  while List.Count > 0 do begin
    TIpHtmlMultiLength(List[0]).Free;
    List.Delete(0);
  end;
end;

constructor TIpHtmlMultiLengthList.Create;
begin
  inherited Create;
  List := TFPList.Create;
end;

destructor TIpHtmlMultiLengthList.Destroy;
begin
  inherited;
  Clear;
  List.Free;
end;

function TIpHtmlMultiLengthList.GetEntries: Integer;
begin
  Result := List.Count;
end;

function TIpHtmlMultiLengthList.GetValues(
  Index: Integer): TIpHtmlMultiLength;
begin
  Result := TIpHtmlMultiLength(List[Index]);
end;


{ TIntArr }

destructor TIntArr.Destroy;
begin
  inherited;
  Freemem(InternalIntArr);
end;

function TIntArr.GetValue(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= IntArrSize) then
    Result := 0
  else
    Result := InternalIntArr^[Index];
end;

procedure TIntArr.SetValue(Index, Value: Integer);
var
  p: ^Integer;
  NewSize: Integer;
begin
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        Inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {code below does not check if InternalIntArr<>nil}
      ReallocMem(InternalIntArr,NewSize * sizeof(PtrInt));
      p := pointer(InternalIntArr);
      Inc(p, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize := NewSize;
    end;
    InternalIntArr^[Index] := Value;
  end;
end;


{ TRectArr }

destructor TRectArr.Destroy;
begin
  inherited;
  Freemem(InternalRectArr);
end;

function TRectArr.GetValue(Index: Integer): PRect;
begin
  Assert(Self <> nil);
  if (Index < 0) or (Index >= IntArrSize) then
    Result := nil
  else
    Result := InternalRectArr^[Index];
end;

procedure TRectArr.SetValue(Index: Integer; Value: PRect);
var
  P: Pointer;
  NewSize: Integer;
begin
  Assert(Self <> nil);
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        Inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {code below does not check if InternalIntArr<>nil and set buggy IntArrSize}
      ReallocMem(InternalRectArr,NewSize * sizeof(PtrInt));
      P := pointer(InternalRectArr);
      Inc(P, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize:=NewSize;
    end;
    InternalRectArr^[Index] := Value;
  end;
end;


{ TRectRectArr }

procedure TRectRectArr.Delete(Index: Integer);
var
  i: Integer;
begin
  if (Index >= 0) and (Index < IntArrSize) then begin
    Value[Index].Free;
    for i := 1 to IntArrSize - 1 do
      InternalRectRectArr[i-1] := InternalRectRectArr[i];
    InternalRectRectArr[IntArrSize - 1] := nil;
  end;
end;

destructor TRectRectArr.Destroy;
var
  i: Integer;
begin
  inherited;
  for i := 0 to IntArrSize - 1 do
    Delete(i);
  if InternalRectRectArr <> nil then
    Freemem(InternalRectRectArr);
end;

function TRectRectArr.GetValue(Index: Integer): TRectArr;
var
  P: ^Pointer;
  NewSize: Integer;
begin
  if Index >= 0 then begin
    if Index >= IntArrSize then begin
      NewSize := IntArrSize;
      repeat
        Inc(NewSize, TINTARRGROWFACTOR);
      until Index < NewSize;
      {code below does not check if InternalIntArr<>nil and set buggy IntArrSize}
      ReallocMem(InternalRectRectArr,NewSize * sizeof(PtrInt));
      p := pointer(InternalRectRectArr);
      Inc(p, IntArrSize);
      fillchar(p^, (NewSize - IntArrSize)*sizeOf(PtrInt), 0);
      IntArrSize:=NewSize;
    end;
    Result := InternalRectRectArr^[Index];
    if Result = nil then begin
      Result := TRectArr.Create;
      InternalRectRectArr^[Index] := Result;
    end;
  end else
    Result := nil;
end;


{ TIpHtmlPreviewSettings }

constructor TIpHtmlPreviewSettings.Create;
begin
  inherited;
  FPosition := poScreenCenter;
  FZoom := 100;
  FWidth := Screen.Width * 3 div 4;
  FHeight := Screen.Height * 3 div 4;
  FLeft := Screen.Width div 4;
  FTop := Screen.Height div 4;
end;


{ TIpHtmlPrintSettings }

constructor TIpHtmlPrintSettings.Create;
begin
  inherited;
  FPreview := TIpHtmlPreviewSettings.Create;
  FMarginLeft := DEFAULT_PRINTMARGIN;
  FMarginTop := DEFAULT_PRINTMARGIN;
  FMarginRight := DEFAULT_PRINTMARGIN;
  FMarginBottom := DEFAULT_PRINTMARGIN;
end;

destructor TIpHtmlPrintSettings.Destroy;
begin
  FPreview.Free;
  inherited;
end;

end.

