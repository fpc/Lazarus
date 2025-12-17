{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit LazEditHighlighterUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Math, LazClasses, LazListClasses, LazListClassesBase,
  LazEditLineItemLists, Generics.Collections, Generics.Defaults;

type

  { TLazHighlighterRange }

  TLazHighlighterRange = class
  protected
    function DoCompare(Range: TLazHighlighterRange; ASize: integer): integer; inline;
    function Compare(Range: TLazHighlighterRange): integer; virtual;
  public
    constructor Create(Template: TLazHighlighterRange); virtual;
    procedure Assign(Src: TLazHighlighterRange); virtual; abstract;
  end;
  TLazHighlighterRangeClass = class of TLazHighlighterRange;

  { TLazHighlighterRanges }

  TLazHighlighterRanges = class(TRefCountedObject)
    constructor Create; virtual;
    destructor Destroy; override;
    function GetEqual(ARange: TLazHighlighterRange): TLazHighlighterRange; virtual; abstract;

    procedure Allocate; deprecated 'use AddReference // will be removed in 5.99';
    procedure Release; deprecated 'use ReleaseReference // will be removed in 5.99';
  end;
  TLazHighlighterRangesClass = class of TLazHighlighterRanges;

  { TLazHighlighterRangeForDictionary }

  TLazHighlighterRangeForDictionary = class(TLazHighlighterRange)
    function GetHashCode(AnInitVal: UInt32 = 0): UInt32; reintroduce; virtual;
    function GetHashCodeWithoutClass(AnInitVal: UInt32 = 0): UInt32;
  end;

  { TLazHighlighterRangesDictionary }

  TLazHighlighterRangesDictionary = class(TLazHighlighterRanges)
  private type
    TLazHLrRangeDictionary = specialize TDictionary<TLazHighlighterRangeForDictionary, TLazHighlighterRangeForDictionary>;
  private
    FRangeDict: TLazHLrRangeDictionary;
  protected
    procedure FreeItems;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetEqual(ARange: TLazHighlighterRange): TLazHighlighterRange; override;
  end;


  { TLazHighlighterLineRangeList }

  TLazHighlighterLineRangeList = class(TLazEditLineItems)
  private type
    TWrapGenParamClass = class(TWrapGenBaseLazEditLineItems)
    public type
      TBase = TLazHighlighterLineRangeList;
    end;
  strict private
    FFirstInvalidLine: Integer;
    FLastInvalidLine: Integer;
    FValidatedChangeStamp: QWord;
    FRefCount: Integer;
    FUnsentValidationStartLine: Integer;
    procedure SetValidatedChangeStamp(AValue: QWord);
  protected
    procedure LineTextChanged(AIndex: Integer; ACount: Integer = 1); virtual; deprecated 'use TextChanged / to be removed in 5.99';
    procedure InsertedLines(AIndex, ACount: Integer); virtual; deprecated 'use Insert / to be removed in 5.99';
    procedure DeletedLines(AIndex, ACount: Integer); virtual; deprecated 'use Delete / to be removed in 5.99';
    procedure TextChanged(AnIndex, ACount: Integer); override;
  public
    procedure Insert(AnIndex, ACount: Integer); override;
    procedure Delete(AnIndex, ACount: Integer); override;
  protected
    procedure Invalidate(AFrom, ATo: integer); inline;
    function GetRange(AnIndex: Integer): Pointer; virtual; abstract;
    procedure SetRange(AnIndex: Integer; const AValue: Pointer); virtual; abstract;
  public
    constructor Create;

    procedure InvalidateAll;
    procedure ValidateAll;
    procedure UpdateFirstInvalidLine(ANewFirstInvalidLine: Integer);

    procedure IncRefCount;
    procedure DecRefCount;
    property RefCount: Integer read FRefCount;

    property ValidatedChangeStamp: QWord read FValidatedChangeStamp write SetValidatedChangeStamp;
    property FirstInvalidLine: Integer read FFirstInvalidLine;
    property LastInvalidLine: Integer read FLastInvalidLine;
    property UnsentValidationStartLine: Integer read FUnsentValidationStartLine;

    property Range[AnIndex: Integer]: Pointer read GetRange write SetRange; default;
  public
    procedure ClearReScanNeeded;  deprecated 'Use ValidateAll / To be removed in 5.99';
    procedure AdjustReScanStart(ANewStart: Integer);  deprecated 'Use UpdateFirstInvalidLine / To be removed in 5.99';

    property NeedsReScanStartIndex: Integer read FFirstInvalidLine; deprecated 'Use FirstInvalidLine / To be removed in 5.99';
    property NeedsReScanEndIndex: Integer read FLastInvalidLine; deprecated 'Use LastInvalidLine / To be removed in 5.99';
    property NeedsReScanRealStartIndex: Integer read FUnsentValidationStartLine; deprecated 'Use UnsentValidationStartLine / To be removed in 5.99';
  end;

  { TGenLazHighlighterLineRangeShiftList
    - Range can be Pointer or record
    - First field in Range MUST BE a pointer
    - Additional fields can be handled by subclasses
  }

  generic TGenInitLazHighlighterLineRangeShiftList<_RANGE, _MemInit> = class(
    specialize TGenLazEditLineItemsFixSize<
      TLazHighlighterLineRangeList.TWrapGenParamClass,
      specialize TGenLazShiftListFixedSize<
        _RANGE,
        {$IFDEF AssertSynMemIndex}
        specialize TGenListConfigFixSize_3<_MemInit, TLazListAspectCapacityExp0x8000, TLazListAspectRangeIndexCheckExcept>
        {$ELSE}
        specialize TGenListConfigFixSize_3<_MemInit, TLazListAspectCapacityExp0x8000, TLazListAspectRangeNoIndexCheck>
        {$ENDIF}
      >
    >)
  protected type
    PRange = ^_RANGE;
  private
    function GetItemPointer(AnIndex: Integer): PRange; inline;
  protected
    function GetRange(AnIndex: Integer): Pointer; override;
    procedure SetRange(AnIndex: Integer; const AValue: Pointer); override;
    procedure TextChanged(AnIndex, ACount: Integer); override;
  public
    procedure Insert(AnIndex, ACount: Integer); override;
    procedure Delete(AnIndex, ACount: Integer); override;
    procedure Move(AFromIndex, AToIndex, ACount: Integer); override;

    property ItemPointer[AnIndex: Integer]: PRange read GetItemPointer;
  end;

  generic TGenLazHighlighterLineRangeShiftList<_RANGE> = class(
    specialize TGenInitLazHighlighterLineRangeShiftList<_RANGE, TLazListAspectMemAllocZero>
  )
  end;

  TLazHighlighterLineRangeShiftList = specialize TGenLazHighlighterLineRangeShiftList<Pointer>;



function GetHighlighterRangesForHighlighter(
    AnHighlighterClass: TClass {TSynCustomHighlighterClass};
    ANewClass: TLazHighlighterRangesClass
  ): TLazHighlighterRanges;

implementation

type
  TLazHighlighterRangesList = specialize TFPGMap<Pointer, TLazHighlighterRanges>;

  ILazHighlighterRangeEqualityComparer = specialize IEqualityComparer<TLazHighlighterRangeForDictionary>;

  { TLazHighlighterRangeEqualityComparer }

  TLazHighlighterRangeEqualityComparer = class(TInterfacedObject, ILazHighlighterRangeEqualityComparer)
  public
    {$IF FPC_Fullversion>30202}
    function Equals(const ALeft, ARight: TLazHighlighterRangeForDictionary): Boolean; reintroduce;
    function GetHashCode(const AValue: TLazHighlighterRangeForDictionary): UInt32; reintroduce;
    {$ELSE}
    function Equals(constref ALeft, ARight: TLazHighlighterRangeForDictionary): Boolean; reintroduce;
    function GetHashCode(constref AValue: TLazHighlighterRangeForDictionary): UInt32; reintroduce;
    {$ENDIF}
  end;

var
  LazHighlighterRangesList: TLazHighlighterRangesList;

function GetHighlighterRangesForHighlighter(AnHighlighterClass: TClass;
  ANewClass: TLazHighlighterRangesClass): TLazHighlighterRanges;
begin
  if LazHighlighterRangesList = nil then
    LazHighlighterRangesList := TLazHighlighterRangesList.Create;

  if not LazHighlighterRangesList.TryGetData(Pointer(AnHighlighterClass), Result) then begin
    Result := ANewClass.Create;
    LazHighlighterRangesList.Add(Pointer(AnHighlighterClass), Result);
  end;
end;

{ TLazHighlighterRangeEqualityComparer }

function TLazHighlighterRangeEqualityComparer.Equals(
  {$IF FPC_Fullversion>30202} const {$ELSE} constref {$ENDIF}
  ALeft, ARight: TLazHighlighterRangeForDictionary): Boolean;
begin
  Result := ALeft.Compare(ARight) = 0;
end;

function TLazHighlighterRangeEqualityComparer.GetHashCode(
  {$IF FPC_Fullversion>30202} const {$ELSE} constref {$ENDIF}
  AValue: TLazHighlighterRangeForDictionary): UInt32;
begin
  Result := AValue.GetHashCode;
end;

{ TLazHighlighterRange }

function TLazHighlighterRange.DoCompare(Range: TLazHighlighterRange; ASize: integer): integer;
begin
  Result := InstanceSize - Range.InstanceSize;
  if Result = 0 then
    Result := CompareByte(Pointer(Self)^, Pointer(Range)^, ASize);
end;

function TLazHighlighterRange.Compare(Range: TLazHighlighterRange): integer;
begin
  Result := DoCompare(Range, InstanceSize);
end;

constructor TLazHighlighterRange.Create(Template: TLazHighlighterRange);
begin
  if (Template<>nil) and (ClassType<>Template.ClassType) then
    raise Exception.Create('wrong tmpl class');
  if Template<>nil then
    Assign(Template);
end;

{ TLazHighlighterRanges }

constructor TLazHighlighterRanges.Create;
begin
  inherited Create;
end;

destructor TLazHighlighterRanges.Destroy;
var
  i: Integer;
begin
  if LazHighlighterRangesList <> nil then begin
    i := LazHighlighterRangesList.IndexOfData(Self);
    assert(i>=0, 'TLazHighlighterRangesDictionary.Destroy: i>=0');
    LazHighlighterRangesList.Delete(i);
    if LazHighlighterRangesList.Count = 0 then
      FreeAndNil(LazHighlighterRangesList);
  end;
  inherited Destroy;
end;

procedure TLazHighlighterRanges.Allocate;
begin
  AddReference;
end;

procedure TLazHighlighterRanges.Release;
begin
  ReleaseReference;
end;

{ TLazHighlighterRangeForDictionary }

function TLazHighlighterRangeForDictionary.GetHashCode(AnInitVal: UInt32): UInt32;
begin
  Result := TDefaultHashFactory.GetHashCode(Pointer(Self), InstanceSize, AnInitVal);
end;

function TLazHighlighterRangeForDictionary.GetHashCodeWithoutClass(AnInitVal: UInt32): UInt32;
begin
  Result := TDefaultHashFactory.GetHashCode(Pointer(Self) + SizeOf(Pointer), InstanceSize - SizeOf(Pointer), AnInitVal);
end;

{ TLazHighlighterRangesDictionary }

procedure TLazHighlighterRangesDictionary.FreeItems;
var
  i: TLazHighlighterRangeForDictionary;
begin
  for i in FRangeDict.Values do i.Free;
  FRangeDict.Clear;
end;

constructor TLazHighlighterRangesDictionary.Create;
begin
  FRangeDict := TLazHLrRangeDictionary.Create(TLazHighlighterRangeEqualityComparer.Create);
  inherited Create;
end;

destructor TLazHighlighterRangesDictionary.Destroy;
begin
  inherited Destroy;
  FreeItems;
  FRangeDict.Free;
end;

function TLazHighlighterRangesDictionary.GetEqual(ARange: TLazHighlighterRange
  ): TLazHighlighterRange;
var
  DictRange: TLazHighlighterRangeForDictionary absolute ARange;
  DictResult: TLazHighlighterRangeForDictionary absolute Result;
begin
  Result := nil;
  if ARange=nil then
    exit;

  if not FRangeDict.TryGetValue(DictRange, DictResult) then begin
    Result := TLazHighlighterRangeClass(DictRange.ClassType).Create(DictRange);
    FRangeDict.Add(DictResult, DictResult);
  end;
end;

{ TLazHighlighterLineRangeList }

procedure TLazHighlighterLineRangeList.SetValidatedChangeStamp(AValue: QWord);
begin
  if FValidatedChangeStamp = AValue then Exit;
  FValidatedChangeStamp := AValue;
  //InvalidateAll;
end;

procedure TLazHighlighterLineRangeList.LineTextChanged(AIndex: Integer; ACount: Integer);
begin
  //
end;

procedure TLazHighlighterLineRangeList.InsertedLines(AIndex, ACount: Integer);
begin
  //
end;

procedure TLazHighlighterLineRangeList.DeletedLines(AIndex, ACount: Integer);
begin
  //
end;

procedure TLazHighlighterLineRangeList.TextChanged(AnIndex, ACount: Integer);
begin
  inherited TextChanged(AnIndex, ACount);
  LineTextChanged(AnIndex, ACount){%H-};
end;

procedure TLazHighlighterLineRangeList.Insert(AnIndex, ACount: Integer);
begin
  inherited Insert(AnIndex, ACount);
  InsertedLines(AnIndex, ACount){%H-};
end;

procedure TLazHighlighterLineRangeList.Delete(AnIndex, ACount: Integer);
begin
  inherited Delete(AnIndex, ACount);
  DeletedLines(AnIndex, ACount){%H-};
end;

procedure TLazHighlighterLineRangeList.Invalidate(AFrom, ATo: integer);
begin
  if (FFirstInvalidLine < 0) or (AFrom < FFirstInvalidLine) then begin
    FFirstInvalidLine := AFrom;
    FUnsentValidationStartLine := AFrom;
  end;
  if ATo > FLastInvalidLine then
    FLastInvalidLine := ATo;
end;

constructor TLazHighlighterLineRangeList.Create;
begin
  ValidateAll;
  FRefCount := 1;
end;

procedure TLazHighlighterLineRangeList.InvalidateAll;
begin
  FFirstInvalidLine := 0;
  FLastInvalidLine := Count - 1;
  FUnsentValidationStartLine := 0;
end;

procedure TLazHighlighterLineRangeList.ValidateAll;
begin
  FFirstInvalidLine := -1;
  FLastInvalidLine := -1;
  FUnsentValidationStartLine :=-1;
end;

procedure TLazHighlighterLineRangeList.UpdateFirstInvalidLine(ANewFirstInvalidLine: Integer);
begin
  if ANewFirstInvalidLine > FLastInvalidLine then
    ValidateAll
  else
    FFirstInvalidLine := ANewFirstInvalidLine;
end;

procedure TLazHighlighterLineRangeList.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TLazHighlighterLineRangeList.DecRefCount;
begin
  dec(FRefCount);
end;

procedure TLazHighlighterLineRangeList.ClearReScanNeeded;
begin
  ValidateAll;
end;

procedure TLazHighlighterLineRangeList.AdjustReScanStart(ANewStart: Integer);
begin
  UpdateFirstInvalidLine(ANewStart);
end;

{ TGenInitLazHighlighterLineRangeShiftList }

function TGenInitLazHighlighterLineRangeShiftList.GetItemPointer(AnIndex: Integer): PRange;
begin
  Result := PRange(inherited ItemPointer[AnIndex]);
end;

function TGenInitLazHighlighterLineRangeShiftList.GetRange(AnIndex: Integer): Pointer;
begin
  Result := PPointer(inherited ItemPointer[AnIndex])^;
end;

procedure TGenInitLazHighlighterLineRangeShiftList.SetRange(AnIndex: Integer; const AValue: Pointer);
begin
  PPointer(inherited ItemPointer[AnIndex])^ := AValue;
end;

procedure TGenInitLazHighlighterLineRangeShiftList.TextChanged(AnIndex, ACount: Integer);
begin
  inherited TextChanged(AnIndex, ACount);
  Invalidate(AnIndex, AnIndex+ACount-1);
end;

procedure TGenInitLazHighlighterLineRangeShiftList.Insert(AnIndex, ACount: Integer);
begin
  inherited Insert(AnIndex, ACount);
  Invalidate(AnIndex, AnIndex+ACount-1);
end;

procedure TGenInitLazHighlighterLineRangeShiftList.Delete(AnIndex, ACount: Integer);
begin
  inherited Delete(AnIndex, ACount);
  Invalidate(AnIndex, AnIndex);
end;

procedure TGenInitLazHighlighterLineRangeShiftList.Move(AFromIndex, AToIndex, ACount: Integer);
begin
  inherited Move(AFromIndex, AToIndex, ACount);
  Invalidate(Min(AFromIndex, AToIndex), Max(AFromIndex, AToIndex)+ACount-1);
end;

finalization
  LazHighlighterRangesList.Free;
end.

