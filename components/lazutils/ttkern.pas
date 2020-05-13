unit TTKern;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EasyLazFreeType, TTTypes, TTFile, TTObjs, fgl;

type
  { TCustomKerningTable }

  TCustomKerningTable = class
  private
    function GetIsCrossStream: boolean;
    function GetIsHorizontal: boolean;
    function GetIsMinimum: boolean;
    function GetIsOverride: boolean;
  protected
    FCoverage: UShort;
  public
    constructor Create(ACoverage: UShort);
    procedure LoadFromStream(AStream: TFreeTypeStream; ASize: UShort); virtual; abstract;
    property IsMinimum: boolean read GetIsMinimum;
    property IsHorizontal: boolean read GetIsHorizontal;
    property IsCrossStream: boolean read GetIsCrossStream;
    property IsOverride: boolean read GetIsOverride;
    function GetKerning(ALeftGlyph, ARightGlyph: UShort; var AInfo: TT_KerningInfo): boolean; virtual; abstract;
  end;

  { TKerningTables }

  TKerningTables = class(specialize TFPGObjectList<TCustomKerningTable>)
    function GetKerning(ALeftGlyph, ARightGlyph: UShort): TT_KerningInfo;
  end;

procedure LoadKerningTables(AFace: PFace; AKerningTables: TKerningTables);

implementation

uses
  TTLoad;

const
  COVERAGE_HORIZONTAL = 1;
  COVERAGE_MINIMUM = 2;
  COVERAGE_CROSS_STREAM = 4;
  COVERAGE_OVERRIDE = 8;
  SUBTABLE_FORMAT_BINARY_SEARCH = 0;
  SUBTABLE_FORMAT_TWO_DIMENSIONAL = 2;

type
  TKerningPair = record
    leftGlyph, rightGlyph: UShort;
    value: TT_FWord;
  end;

operator <(const APair1, APair2: TKerningPair): boolean;
begin
  result := (APair1.leftGlyph < APair2.leftGlyph) or
            ((APair1.leftGlyph = APair2.leftGlyph) and
             (APair1.rightGlyph < APair2.rightGlyph));
end;

type
  { TBinarySearchKerningTable }

  TBinarySearchKerningTable = class(TCustomKerningTable)
    nPairs, searchRange, entrySelector, rangeShift: UShort;
    pairs: array of TKerningPair;
    procedure SortPairs;
    procedure LoadFromStream(AStream: TFreeTypeStream; ASize: UShort); override;
    function GetKerning(ALeftGlyph, ARightGlyph: UShort; var AInfo: TT_KerningInfo): boolean; override;
  end;

{ TKerningTables }

function TKerningTables.GetKerning(ALeftGlyph, ARightGlyph: UShort): TT_KerningInfo;
var
  i: Integer;
  found: Boolean;
begin
  result.kerning_x:= 0;
  result.kerning_y:= 0;
  result.minimum_x:= -32767;
  result.minimum_y:= -32767;
  result.found := false;
  for i := 0 to Count-1 do
    if Items[I].GetKerning(ALeftGlyph, ARightGlyph, result) then
      result.found := true;
end;

{ TBinarySearchKerningTable }

procedure TBinarySearchKerningTable.SortPairs;
var
  i,j,k: UShort;
  temp: TKerningPair;
begin
  if nPairs > 0 then
  for i := 1 to nPairs-1 do
  begin
    j := i;
    while (j > 0) and (pairs[i] < pairs[j-1]) do dec(j);
    if j < i then
    begin
      temp := pairs[i];
      for k := i downto j+1 do
        pairs[k] := pairs[k-1];
      pairs[j] := temp;
    end;
  end;
end;

procedure TBinarySearchKerningTable.LoadFromStream(AStream: TFreeTypeStream;
  ASize: UShort);
var
  endPosition: LongInt;
  i: UShort;
begin
  if ASize <= 8 then
  begin
    nPairs := 0;
    searchRange := 0;
    entrySelector:= 0;
    rangeShift:= 0;
  end else
  begin
    endPosition := AStream.Position + ASize;
    nPairs := AStream.GET_UShort;
    searchRange := AStream.GET_UShort;
    entrySelector := AStream.GET_UShort;
    rangeShift := AStream.GET_UShort;
    if nPairs > 0 then
    begin
      setlength(pairs, nPairs);
      for i := 0 to nPairs-1 do
        if AStream.Position + 6 > endPosition then
        begin
          nPairs := i;
          setlength(pairs, nPairs);
          break;
        end else
        begin
          pairs[i].leftGlyph:= AStream.GET_UShort;
          pairs[i].rightGlyph:= AStream.GET_UShort;
          pairs[i].value:= AStream.GET_Short;
        end;
      SortPairs;
    end;
  end;
end;

function TBinarySearchKerningTable.GetKerning(ALeftGlyph, ARightGlyph: UShort; var AInfo: TT_KerningInfo): boolean;
var
  maxIndex, minIndex, midIndex: integer;
  searchedPair: TKerningPair;

  function ClampShort(AValue, AMin, AMax: integer): integer;
  begin
    if AValue < AMin then result := AMin else
    if AValue > AMax then result := AMax else
      result := AValue;
  end;

begin
  searchedPair.leftGlyph:= ALeftGlyph;
  searchedPair.rightGlyph:= ARightGlyph;
  minIndex := 0;
  maxIndex := nPairs-1;
  while minIndex < maxIndex do
  begin
    midIndex := (minIndex+maxIndex+1) shr 1;
    if searchedPair < pairs[midIndex] then
      maxIndex := midIndex-1
    else
      minIndex := midIndex;
  end;
  searchedPair := pairs[minIndex];
  if (searchedPair.leftGlyph = ALeftGlyph) and
     (searchedPair.rightGlyph = ARightGlyph) then
  begin
    if IsCrossStream then
    begin
      if IsMinimum then
      begin
        if IsOverride then
          AInfo.minimum_y:= searchedPair.value
        else
          AInfo.minimum_y:= ClampShort(AInfo.minimum_y + searchedPair.value, -32768, 32767);
      end else
      begin
        if IsOverride then
          AInfo.kerning_y:= searchedPair.value
        else
          AInfo.kerning_y:= ClampShort(AInfo.kerning_y + searchedPair.value, AInfo.minimum_y, 32767);
      end;
    end else
    begin
      if IsMinimum then
      begin
        if IsOverride then
          AInfo.minimum_x:= searchedPair.value
        else
          AInfo.minimum_x:= ClampShort(AInfo.minimum_x + searchedPair.value, -32768, 32767);
      end else
      begin
        if IsOverride then
          AInfo.kerning_x:= searchedPair.value
        else
          AInfo.kerning_x:= ClampShort(AInfo.kerning_x + searchedPair.value, AInfo.minimum_y, 32767);
      end;
    end;
    result := true;
  end else
    result := false;
end;

{ TCustomKerningTable }

function TCustomKerningTable.GetIsCrossStream: boolean;
begin
  result := (FCoverage and COVERAGE_CROSS_STREAM) <> 0;
end;

function TCustomKerningTable.GetIsHorizontal: boolean;
begin
  result := (FCoverage and COVERAGE_HORIZONTAL) <> 0;
end;

function TCustomKerningTable.GetIsMinimum: boolean;
begin
  result := (FCoverage and COVERAGE_MINIMUM) <> 0;
end;

function TCustomKerningTable.GetIsOverride: boolean;
begin
  result := (FCoverage and COVERAGE_OVERRIDE) <> 0;
end;

constructor TCustomKerningTable.Create(ACoverage: UShort);
begin
  FCoverage:= ACoverage;
end;

procedure LoadKerningTables(AFace: PFace; AKerningTables: TKerningTables);
var
  kernTableIndex: Int;
  substream: TFreeTypeStream;
  kernTableOffset: Long;

  procedure ParseKernTable;
  var
    version, nTables, byteSize, coverage: UShort;
    i: UShort;
    subtableFormat: byte;
    nextTablePos: Longint;
    newTable: TCustomKerningTable;
  begin
    if substream.SeekFile(kernTableOffset) <> Success then exit;
    if substream.AccessFrame(4) <> Success then exit;
    try
      version := substream.GET_UShort;
      nTables := substream.GET_UShort;
    finally
      substream.ForgetFrame;
    end;
    if (version <> 0) or (nTables = 0) then exit;
    for i := 0 to nTables-1 do
    begin
      if substream.AccessFrame(6) <> Success then exit;
      try
        version := substream.GET_UShort;
        byteSize:= substream.GET_UShort;
        coverage:= substream.GET_UShort;
      finally
        substream.ForgetFrame;
      end;
      subtableFormat := coverage shr 8;
      nextTablePos:= substream.Position + byteSize;
      if (version = 0) or (coverage AND COVERAGE_HORIZONTAL = 0) then
      begin
        newTable := nil;
        case subtableFormat of
        SUBTABLE_FORMAT_BINARY_SEARCH: newTable := TBinarySearchKerningTable.Create(coverage);
        end;
        if Assigned(newTable) then
        begin
          if substream.AccessFrame(byteSize) = Success then
          begin
            try
              newTable.LoadFromStream(substream, byteSize);
              AKerningTables.Add(newTable);
              newTable := nil;
            finally
              substream.ForgetFrame;
            end;
          end;
        end;
        newTable.Free;
      end;
      substream.SeekFile(nextTablePos)
    end;
  end;

begin
  kernTableIndex:= LookUp_TrueType_Table(AFace, 'kern');
  if kernTableIndex >= 0 then
  begin
    kernTableOffset:= AFace^.dirTables^[kernTableIndex].Offset;
    if TT_Use_Stream(AFace^.stream, substream) = Success then
      try
        ParseKernTable;
      finally
        TT_Done_Stream( AFace^.stream );
      end;
  end;
end;

end.

