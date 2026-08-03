(*******************************************************************
 *
 *  ttvar.pas
 *
 *    OpenType variation table support used by the TrueType loader.
 *
 ******************************************************************)

unit TTVar;

interface

{$mode objfpc}{$H+}
{$R-}

uses
  TTTypes;

const
  TTVar_PhantomPointCount = 4;
  TTVar_PhantomHOrigin = 0;
  TTVar_PhantomHAdvance = 1;
  TTVar_PhantomVOrigin = 2;
  TTVar_PhantomVAdvance = 3;
  TTVar_F2Dot14One = 16384;
  TTVar_FixedOne = 65536;

type
  TTTVarAxis = record
    Tag: TT_ULong;
    MinValue: TT_Fixed;
    DefaultValue: TT_Fixed;
    MaxValue: TT_Fixed;
    Flags: TT_UShort;
    NameID: TT_UShort;
  end;

  TTTVariationData = class;

  TTTVariationInstance = class
  private
    FOwner: TTTVariationData;
    FUserCoords: array of TT_Fixed;
    FNormalizedCoords: array of SmallInt;
    procedure Normalize;
  public
    constructor Create(AOwner: TTTVariationData);
    function SetCoordinate(AAxisTag: TT_ULong; AValue: TT_Fixed): Boolean;
    function AxisCount: Integer;
    function NormalizedCoord(AIndex: Integer): SmallInt;
    function HasNonDefaultCoords: Boolean;
  end;

  TTTVariationData = class
  private
    FAxes: array of TTTVarAxis;
    FAvar: array of array of record
      FromCoord: SmallInt;
      ToCoord: SmallInt;
    end;
    FGVarData: array of Byte;
    FGlyphOffsets: array of LongInt;
    FSharedTuples: array of SmallInt;
    FGVarDataArrayOffset: LongInt;
    FHasGVar: Boolean;
    function MapAxis(AAxis: Integer; ACoord: SmallInt): SmallInt;
    function TupleScalar(AInstance: TTTVariationInstance;
      const APeak, AStart, AEnd: array of SmallInt;
      AHasIntermediate: Boolean): LongInt;
    function DecodePointNumbers(var APos: LongInt; AEndPos: LongInt;
      APointCount: Integer; out APoints: array of Integer;
      out ACount: Integer): Boolean;
    function DecodeDeltas(var APos: LongInt; AEndPos: LongInt;
      ACount: Integer; out ADeltas: array of LongInt): Boolean;
  public
    function LoadFVar(const ABuffer; ALength: LongInt): Boolean;
    function LoadAVar(const ABuffer; ALength: LongInt): Boolean;
    function LoadGVar(const ABuffer; ALength: LongInt;
      AGlyphCount: Integer): Boolean;
    function AxisCount: Integer;
    function AxisIndex(AAxisTag: TT_ULong): Integer;
    function CreateInstance: TTTVariationInstance;
    procedure ApplyGlyphDeltas(AInstance: TTTVariationInstance;
      AGlyphIndex: Integer; APoints: TT_Points; AConEnds: PUShort;
      APointCount, AContourCount: Integer; AAlreadyScaled: Boolean;
      AXScale1, AXScale2, AYScale1, AYScale2: LongInt);
  end;

function TTVar_MakeTag(const ATag: string): TT_ULong;

implementation

type
  TTTVarByteArray = array[0..1000] of Byte;
  PTTTVarByteArray = ^TTTVarByteArray;
  TIntegerArray = array of Integer;
  TLongIntArray = array of LongInt;
  TBooleanArray = array of Boolean;
  TSmallIntArray = array of SmallInt;

function TTVar_MakeTag(const ATag: string): TT_ULong;
begin
  if Length(ATag) < 4 then
    Exit(0);
  Result := (TT_ULong(Ord(ATag[1])) shl 24) or
            (TT_ULong(Ord(ATag[2])) shl 16) or
            (TT_ULong(Ord(ATag[3])) shl 8) or
             TT_ULong(Ord(ATag[4]));
end;

function ReadU16(const ABuffer; ALength, APos: LongInt; out AValue: Word): Boolean;
var
  P: PTTTVarByteArray;
begin
  Result := False;
  if (APos < 0) or (APos + 1 >= ALength) then
    Exit;
  P := @ABuffer;
  AValue := (Word(P^[APos]) shl 8) or Word(P^[APos + 1]);
  Result := True;
end;

function ReadS16(const ABuffer; ALength, APos: LongInt; out AValue: SmallInt): Boolean;
var
  U: Word;
begin
  Result := ReadU16(ABuffer, ALength, APos, U);
  if Result then
    AValue := SmallInt(U);
end;

function ReadU32(const ABuffer; ALength, APos: LongInt; out AValue: TT_ULong): Boolean;
var
  P: PTTTVarByteArray;
begin
  Result := False;
  if (APos < 0) or (APos + 3 >= ALength) then
    Exit;
  P := @ABuffer;
  AValue := (TT_ULong(P^[APos]) shl 24) or
            (TT_ULong(P^[APos + 1]) shl 16) or
            (TT_ULong(P^[APos + 2]) shl 8) or
             TT_ULong(P^[APos + 3]);
  Result := True;
end;

function ReadFixed(const ABuffer; ALength, APos: LongInt; out AValue: TT_Fixed): Boolean;
var
  U: TT_ULong;
begin
  Result := ReadU32(ABuffer, ALength, APos, U);
  if Result then
    AValue := TT_Fixed(U);
end;

function ClampLong(AValue, AMin, AMax: LongInt): LongInt;
begin
  if AValue < AMin then
    Result := AMin
  else if AValue > AMax then
    Result := AMax
  else
    Result := AValue;
end;

function FixedToF2Dot14(AValue, AMin, ADefault, AMax: TT_Fixed): SmallInt;
var
  N, D: Int64;
  V: LongInt;
begin
  if AValue < AMin then
    AValue := AMin
  else if AValue > AMax then
    AValue := AMax;

  if AValue = ADefault then
    Exit(0);

  N := Int64(AValue - ADefault) * TTVar_F2Dot14One;
  if AValue < ADefault then
    D := ADefault - AMin
  else
    D := AMax - ADefault;

  if D = 0 then
    Exit(0);

  V := N div D;
  Result := SmallInt(ClampLong(V, -TTVar_F2Dot14One, TTVar_F2Dot14One));
end;

function ScaleDelta(AValue, AScale1, AScale2: LongInt): LongInt;
begin
  if AScale2 = 0 then
    Exit(AValue);
  Result := LongInt((Int64(AValue) * AScale1) div AScale2);
end;

function ApplyScalar(AValue, AScalar: LongInt): LongInt;
begin
  Result := LongInt((Int64(AValue) * AScalar) div TTVar_F2Dot14One);
end;

function InterpolateDelta(ACoord, ACoord1, ACoord2, ADelta1,
  ADelta2: LongInt): LongInt;
var
  LoCoord, HiCoord, LoDelta, HiDelta: LongInt;
begin
  if ACoord1 = ACoord2 then
  begin
    if ADelta1 = ADelta2 then
      Result := ADelta1
    else
      Result := 0;
    Exit;
  end;

  if ACoord1 < ACoord2 then
  begin
    LoCoord := ACoord1;
    HiCoord := ACoord2;
    LoDelta := ADelta1;
    HiDelta := ADelta2;
  end
  else
  begin
    LoCoord := ACoord2;
    HiCoord := ACoord1;
    LoDelta := ADelta2;
    HiDelta := ADelta1;
  end;

  if ACoord <= LoCoord then
    Result := LoDelta
  else if ACoord >= HiCoord then
    Result := HiDelta
  else
    Result := LoDelta + LongInt((Int64(ACoord - LoCoord) *
      (HiDelta - LoDelta)) div (HiCoord - LoCoord));
end;

function ReadGVarU16(const AData: array of Byte; APos: LongInt): Word;
begin
  Result := (Word(AData[APos]) shl 8) or Word(AData[APos + 1]);
end;

function ReadGVarS16(const AData: array of Byte; APos: LongInt): SmallInt;
begin
  Result := SmallInt(ReadGVarU16(AData, APos));
end;

function ReadGVarU32(const AData: array of Byte; APos: LongInt): TT_ULong;
begin
  Result := (TT_ULong(AData[APos]) shl 24) or
            (TT_ULong(AData[APos + 1]) shl 16) or
            (TT_ULong(AData[APos + 2]) shl 8) or
             TT_ULong(AData[APos + 3]);
end;

constructor TTTVariationInstance.Create(AOwner: TTTVariationData);
var
  I: Integer;
begin
  inherited Create;
  FOwner := AOwner;
  SetLength(FUserCoords, FOwner.AxisCount);
  SetLength(FNormalizedCoords, FOwner.AxisCount);
  for I := 0 to FOwner.AxisCount - 1 do
    FUserCoords[I] := FOwner.FAxes[I].DefaultValue;
  Normalize;
end;

procedure TTTVariationInstance.Normalize;
var
  I: Integer;
begin
  for I := 0 to High(FUserCoords) do
    FNormalizedCoords[I] := FOwner.MapAxis(I, FixedToF2Dot14(FUserCoords[I],
      FOwner.FAxes[I].MinValue, FOwner.FAxes[I].DefaultValue,
      FOwner.FAxes[I].MaxValue));
end;

function TTTVariationInstance.SetCoordinate(AAxisTag: TT_ULong;
  AValue: TT_Fixed): Boolean;
var
  I: Integer;
begin
  I := FOwner.AxisIndex(AAxisTag);
  Result := I >= 0;
  if not Result then
    Exit;
  FUserCoords[I] := ClampLong(AValue, FOwner.FAxes[I].MinValue,
    FOwner.FAxes[I].MaxValue);
  Normalize;
end;

function TTTVariationInstance.AxisCount: Integer;
begin
  Result := Length(FUserCoords);
end;

function TTTVariationInstance.NormalizedCoord(AIndex: Integer): SmallInt;
begin
  if (AIndex < 0) or (AIndex > High(FNormalizedCoords)) then
    Result := 0
  else
    Result := FNormalizedCoords[AIndex];
end;

function TTTVariationInstance.HasNonDefaultCoords: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FNormalizedCoords) do
    if FNormalizedCoords[I] <> 0 then
      Exit(True);
  Result := False;
end;

function TTTVariationData.LoadFVar(const ABuffer; ALength: LongInt): Boolean;
var
  OffsetToData, TableAxisCount, AxisSize: Word;
  I, Pos: Integer;
  U: TT_ULong;
  F: TT_Fixed;
  W: Word;
begin
  Result := False;
  SetLength(FAxes, 0);
  if (ALength < 16) or
     not ReadU16(ABuffer, ALength, 4, OffsetToData) or
     not ReadU16(ABuffer, ALength, 8, TableAxisCount) or
     not ReadU16(ABuffer, ALength, 10, AxisSize) then
    Exit;

  if (TableAxisCount = 0) or (TableAxisCount > 128) or (AxisSize < 20) or
     (OffsetToData + LongInt(TableAxisCount) * AxisSize > ALength) then
    Exit;

  SetLength(FAxes, TableAxisCount);
  Pos := OffsetToData;
  for I := 0 to TableAxisCount - 1 do
  begin
    if not ReadU32(ABuffer, ALength, Pos, U) then Exit;
    FAxes[I].Tag := U;
    if not ReadFixed(ABuffer, ALength, Pos + 4, F) then Exit;
    FAxes[I].MinValue := F;
    if not ReadFixed(ABuffer, ALength, Pos + 8, F) then Exit;
    FAxes[I].DefaultValue := F;
    if not ReadFixed(ABuffer, ALength, Pos + 12, F) then Exit;
    FAxes[I].MaxValue := F;
    if not ReadU16(ABuffer, ALength, Pos + 16, W) then Exit;
    FAxes[I].Flags := W;
    if not ReadU16(ABuffer, ALength, Pos + 18, W) then Exit;
    FAxes[I].NameID := W;
    Inc(Pos, AxisSize);
  end;
  Result := True;
end;

function TTTVariationData.LoadAVar(const ABuffer; ALength: LongInt): Boolean;
var
  TableAxisCount, SegmentCount: Word;
  I, J, Pos: Integer;
  C: SmallInt;
begin
  Result := False;
  SetLength(FAvar, 0);
  if (Length(FAxes) = 0) or (ALength < 8) or
     not ReadU16(ABuffer, ALength, 6, TableAxisCount) or
     (TableAxisCount <> Length(FAxes)) then
    Exit;

  SetLength(FAvar, TableAxisCount);
  Pos := 8;
  for I := 0 to TableAxisCount - 1 do
  begin
    if not ReadU16(ABuffer, ALength, Pos, SegmentCount) then Exit;
    Inc(Pos, 2);
    if (SegmentCount > 256) or (Pos + SegmentCount * 4 > ALength) then Exit;
    SetLength(FAvar[I], SegmentCount);
    for J := 0 to SegmentCount - 1 do
    begin
      if not ReadS16(ABuffer, ALength, Pos, C) then Exit;
      FAvar[I][J].FromCoord := C;
      if not ReadS16(ABuffer, ALength, Pos + 2, C) then Exit;
      FAvar[I][J].ToCoord := C;
      Inc(Pos, 4);
    end;
  end;
  Result := True;
end;

function TTTVariationData.LoadGVar(const ABuffer; ALength: LongInt;
  AGlyphCount: Integer): Boolean;
var
  TableAxisCount, SharedTupleCount, GlyphCount, Flags: Word;
  SharedOffset, U: TT_ULong;
  I, Pos, OffsetTableSize: Integer;
begin
  Result := False;
  FHasGVar := False;
  SetLength(FGVarData, 0);
  SetLength(FGlyphOffsets, 0);
  SetLength(FSharedTuples, 0);
  if (Length(FAxes) = 0) or (ALength < 20) or
     not ReadU16(ABuffer, ALength, 4, TableAxisCount) or
     not ReadU16(ABuffer, ALength, 6, SharedTupleCount) or
     not ReadU32(ABuffer, ALength, 8, SharedOffset) or
     not ReadU16(ABuffer, ALength, 12, GlyphCount) or
     not ReadU16(ABuffer, ALength, 14, Flags) or
     not ReadU32(ABuffer, ALength, 16, U) then
    Exit;

  if (TableAxisCount <> Length(FAxes)) or (GlyphCount <> AGlyphCount) or
     (SharedTupleCount > 4096) then
    Exit;

  FGVarDataArrayOffset := U;
  if Flags and 1 <> 0 then
    OffsetTableSize := (GlyphCount + 1) * 4
  else
    OffsetTableSize := (GlyphCount + 1) * 2;
  if (20 + OffsetTableSize > ALength) or
     (FGVarDataArrayOffset > ALength) then
    Exit;

  SetLength(FGVarData, ALength);
  Move(ABuffer, FGVarData[0], ALength);

  SetLength(FGlyphOffsets, GlyphCount + 1);
  Pos := 20;
  for I := 0 to GlyphCount do
  begin
    if Flags and 1 <> 0 then
    begin
      if Pos + 3 >= ALength then Exit;
      FGlyphOffsets[I] := ReadGVarU32(FGVarData, Pos);
      Inc(Pos, 4);
    end
    else
    begin
      if Pos + 1 >= ALength then Exit;
      FGlyphOffsets[I] := LongInt(ReadGVarU16(FGVarData, Pos)) * 2;
      Inc(Pos, 2);
    end;
  end;

  if SharedTupleCount > 0 then
  begin
    if SharedOffset + LongInt(SharedTupleCount) * TableAxisCount * 2 > ALength then
      Exit;
    SetLength(FSharedTuples, SharedTupleCount * TableAxisCount);
    Pos := SharedOffset;
    for I := 0 to Length(FSharedTuples) - 1 do
    begin
      FSharedTuples[I] := ReadGVarS16(FGVarData, Pos);
      Inc(Pos, 2);
    end;
  end;

  FHasGVar := True;
  Result := True;
end;

function TTTVariationData.AxisCount: Integer;
begin
  Result := Length(FAxes);
end;

function TTTVariationData.AxisIndex(AAxisTag: TT_ULong): Integer;
begin
  for Result := 0 to High(FAxes) do
    if FAxes[Result].Tag = AAxisTag then
      Exit;
  Result := -1;
end;

function TTTVariationData.CreateInstance: TTTVariationInstance;
begin
  Result := TTTVariationInstance.Create(Self);
end;

function TTTVariationData.MapAxis(AAxis: Integer; ACoord: SmallInt): SmallInt;
var
  I: Integer;
  A, B: SmallInt;
begin
  Result := ACoord;
  if (AAxis < 0) or (AAxis > High(FAvar)) or (Length(FAvar[AAxis]) = 0) then
    Exit;

  if ACoord <= FAvar[AAxis][0].FromCoord then
    Exit(FAvar[AAxis][0].ToCoord);

  for I := 1 to High(FAvar[AAxis]) do
  begin
    if ACoord <= FAvar[AAxis][I].FromCoord then
    begin
      A := FAvar[AAxis][I - 1].FromCoord;
      B := FAvar[AAxis][I].FromCoord;
      if A = B then
        Exit(FAvar[AAxis][I].ToCoord);
      Result := FAvar[AAxis][I - 1].ToCoord +
        SmallInt(((Int64(ACoord) - A) *
          (Int64(FAvar[AAxis][I].ToCoord) -
           FAvar[AAxis][I - 1].ToCoord)) div (B - A));
      Exit;
    end;
  end;

  Result := FAvar[AAxis][High(FAvar[AAxis])].ToCoord;
end;

function TTTVariationData.TupleScalar(AInstance: TTTVariationInstance;
  const APeak, AStart, AEnd: array of SmallInt;
  AHasIntermediate: Boolean): LongInt;
var
  I: Integer;
  Coord, Peak, StartCoord, EndCoord: LongInt;
begin
  Result := TTVar_F2Dot14One;
  for I := 0 to AxisCount - 1 do
  begin
    Peak := APeak[I];
    if Peak = 0 then
      Continue;

    Coord := AInstance.NormalizedCoord(I);
    if AHasIntermediate then
    begin
      StartCoord := AStart[I];
      EndCoord := AEnd[I];
    end
    else if Peak > 0 then
    begin
      StartCoord := 0;
      EndCoord := TTVar_F2Dot14One;
    end
    else
    begin
      StartCoord := -TTVar_F2Dot14One;
      EndCoord := 0;
    end;

    if (Coord = 0) or (Coord < StartCoord) or (Coord > EndCoord) or
       ((Peak > 0) and (Coord < 0)) or ((Peak < 0) and (Coord > 0)) then
      Exit(0);

    if Coord = Peak then
      Continue;

    if Coord < Peak then
    begin
      if Peak = StartCoord then
        Exit(0);
      Result := LongInt((Int64(Result) * (Int64(Coord) - StartCoord)) div
        (Int64(Peak) - StartCoord));
    end
    else
    begin
      if EndCoord = Peak then
        Exit(0);
      Result := LongInt((Int64(Result) * (Int64(EndCoord) - Coord)) div
        (Int64(EndCoord) - Peak));
    end;
  end;
end;

function TTTVariationData.DecodePointNumbers(var APos: LongInt;
  AEndPos: LongInt; APointCount: Integer; out APoints: array of Integer;
  out ACount: Integer): Boolean;
var
  First, Header, RunCount, Delta, I, J, Point: Integer;
begin
  Result := False;
  ACount := 0;
  if APos >= AEndPos then
    Exit;

  First := FGVarData[APos];
  Inc(APos);
  if First = 0 then
  begin
    ACount := APointCount;
    if Length(APoints) < ACount then Exit;
    for I := 0 to ACount - 1 do
      APoints[I] := I;
    Exit(True);
  end;

  if First and $80 <> 0 then
  begin
    if APos >= AEndPos then Exit;
    ACount := ((First and $7F) shl 8) or FGVarData[APos];
    Inc(APos);
  end
  else
    ACount := First;

  if Length(APoints) < ACount then Exit;
  I := 0;
  Point := 0;
  while I < ACount do
  begin
    if APos >= AEndPos then Exit;
    Header := FGVarData[APos];
    Inc(APos);
    RunCount := (Header and $7F) + 1;
    if I + RunCount > ACount then Exit;
    for J := 0 to RunCount - 1 do
    begin
      if Header and $80 <> 0 then
      begin
        if APos + 1 >= AEndPos then Exit;
        Delta := ReadGVarU16(FGVarData, APos);
        Inc(APos, 2);
      end
      else
      begin
        if APos >= AEndPos then Exit;
        Delta := FGVarData[APos];
        Inc(APos);
      end;
      Inc(Point, Delta);
      APoints[I] := Point;
      Inc(I);
    end;
  end;

  Result := True;
end;

function TTTVariationData.DecodeDeltas(var APos: LongInt; AEndPos: LongInt;
  ACount: Integer; out ADeltas: array of LongInt): Boolean;
const
  DeltasAreZero = $80;
  DeltasAreWords = $40;
var
  Header, RunCount, I, J: Integer;
begin
  Result := False;
  if Length(ADeltas) < ACount then Exit;
  I := 0;
  while I < ACount do
  begin
    if APos >= AEndPos then Exit;
    Header := FGVarData[APos];
    Inc(APos);
    RunCount := (Header and $3F) + 1;
    if I + RunCount > ACount then Exit;

    if Header and DeltasAreZero <> 0 then
    begin
      for J := 0 to RunCount - 1 do
      begin
        ADeltas[I] := 0;
        Inc(I);
      end;
    end
    else if Header and DeltasAreWords <> 0 then
    begin
      for J := 0 to RunCount - 1 do
      begin
        if APos + 1 >= AEndPos then Exit;
        ADeltas[I] := ReadGVarS16(FGVarData, APos);
        Inc(APos, 2);
        Inc(I);
      end;
    end
    else
    begin
      for J := 0 to RunCount - 1 do
      begin
        if APos >= AEndPos then Exit;
        ADeltas[I] := ShortInt(FGVarData[APos]);
        Inc(APos);
        Inc(I);
      end;
    end;
  end;
  Result := True;
end;

procedure IUPContour(APoints: TT_Points; AStart, AEnd: Integer;
  var ADX, ADY: TLongIntArray; const ATouched: TBooleanArray);
var
  I, Prev, Next, TouchedCount: Integer;
begin
  TouchedCount := 0;
  for I := AStart to AEnd do
    if ATouched[I] then
      Inc(TouchedCount);

  if (TouchedCount = 0) or (TouchedCount = AEnd - AStart + 1) then
    Exit;

  for I := AStart to AEnd do
    if not ATouched[I] then
    begin
      Prev := I;
      repeat
        Dec(Prev);
        if Prev < AStart then
          Prev := AEnd;
      until ATouched[Prev];

      Next := I;
      repeat
        Inc(Next);
        if Next > AEnd then
          Next := AStart;
      until ATouched[Next];

      ADX[I] := InterpolateDelta(APoints^[I].x, APoints^[Prev].x,
        APoints^[Next].x, ADX[Prev], ADX[Next]);
      ADY[I] := InterpolateDelta(APoints^[I].y, APoints^[Prev].y,
        APoints^[Next].y, ADY[Prev], ADY[Next]);
    end;
end;

procedure TTTVariationData.ApplyGlyphDeltas(AInstance: TTTVariationInstance;
  AGlyphIndex: Integer; APoints: TT_Points; AConEnds: PUShort;
  APointCount, AContourCount: Integer; AAlreadyScaled: Boolean;
  AXScale1, AXScale2, AYScale1, AYScale2: LongInt);
const
  TupleCountMask = $0FFF;
  HasSharedPointNumbers = $8000;
  EmbeddedPeakTuple = $8000;
  IntermediateRegion = $4000;
  PrivatePointNumbers = $2000;
  TupleIndexMask = $0FFF;
type
  TTupleHeader = record
    DataSize: Integer;
    TupleIndex: Integer;
    Flags: Integer;
    Peak: TSmallIntArray;
    StartCoord: TSmallIntArray;
    EndCoord: TSmallIntArray;
  end;
var
  GlyphStart, GlyphEnd, Pos, DataPos, SharedDataPos: LongInt;
  RawTupleCount, TupleCount, DataOffset: Word;
  Tuples: array of TTupleHeader;
  SharedPoints, Points: TIntegerArray;
  PointCount: Integer;
  XDelta, YDelta, TupleX, TupleY, NetX, NetY: TLongIntArray;
  Touched: TBooleanArray;
  I, J, P, Scalar, TupleDataEnd, LastContourEnd, ContourStart: Integer;
begin
  Tuples := nil;
  SharedPoints := nil;
  Points := nil;
  XDelta := nil;
  YDelta := nil;
  TupleX := nil;
  TupleY := nil;
  NetX := nil;
  NetY := nil;
  Touched := nil;

  if (not FHasGVar) or (AInstance = nil) or
     (not AInstance.HasNonDefaultCoords) or (APointCount <= 0) or
     (AGlyphIndex < 0) or (AGlyphIndex + 1 > High(FGlyphOffsets)) then
    Exit;

  GlyphStart := FGVarDataArrayOffset + FGlyphOffsets[AGlyphIndex];
  GlyphEnd := FGVarDataArrayOffset + FGlyphOffsets[AGlyphIndex + 1];
  if (GlyphStart < 0) or (GlyphStart >= GlyphEnd) or
     (GlyphEnd > Length(FGVarData)) or (GlyphStart + 4 > GlyphEnd) then
    Exit;

  RawTupleCount := ReadGVarU16(FGVarData, GlyphStart);
  TupleCount := RawTupleCount and TupleCountMask;
  DataOffset := ReadGVarU16(FGVarData, GlyphStart + 2);
  if (TupleCount = 0) or (GlyphStart + DataOffset > GlyphEnd) then
    Exit;

  SetLength(Tuples, TupleCount);
  Pos := GlyphStart + 4;
  for I := 0 to TupleCount - 1 do
  begin
    if Pos + 3 >= GlyphEnd then Exit;
    Tuples[I].DataSize := ReadGVarU16(FGVarData, Pos);
    Tuples[I].TupleIndex := ReadGVarU16(FGVarData, Pos + 2);
    Tuples[I].Flags := Tuples[I].TupleIndex and (EmbeddedPeakTuple or
      IntermediateRegion or PrivatePointNumbers);
    Tuples[I].TupleIndex := Tuples[I].TupleIndex and TupleIndexMask;
    Inc(Pos, 4);

    SetLength(Tuples[I].Peak, AxisCount);
    if Tuples[I].Flags and EmbeddedPeakTuple <> 0 then
    begin
      if Pos + AxisCount * 2 > GlyphEnd then Exit;
      for J := 0 to AxisCount - 1 do
      begin
        Tuples[I].Peak[J] := ReadGVarS16(FGVarData, Pos);
        Inc(Pos, 2);
      end;
    end
    else
    begin
      if (Tuples[I].TupleIndex < 0) or
         (Tuples[I].TupleIndex * AxisCount + AxisCount > Length(FSharedTuples)) then
        Exit;
      for J := 0 to AxisCount - 1 do
        Tuples[I].Peak[J] := FSharedTuples[Tuples[I].TupleIndex * AxisCount + J];
    end;

    if Tuples[I].Flags and IntermediateRegion <> 0 then
    begin
      if Pos + AxisCount * 4 > GlyphEnd then Exit;
      SetLength(Tuples[I].StartCoord, AxisCount);
      SetLength(Tuples[I].EndCoord, AxisCount);
      for J := 0 to AxisCount - 1 do
      begin
        Tuples[I].StartCoord[J] := ReadGVarS16(FGVarData, Pos);
        Inc(Pos, 2);
      end;
      for J := 0 to AxisCount - 1 do
      begin
        Tuples[I].EndCoord[J] := ReadGVarS16(FGVarData, Pos);
        Inc(Pos, 2);
      end;
    end;
  end;

  SharedDataPos := GlyphStart + DataOffset;
  if RawTupleCount and HasSharedPointNumbers <> 0 then
  begin
    SetLength(SharedPoints, APointCount);
    if not DecodePointNumbers(SharedDataPos, GlyphEnd, APointCount,
      SharedPoints, PointCount) then
      Exit;
    SetLength(SharedPoints, PointCount);
  end;

  SetLength(NetX, APointCount);
  SetLength(NetY, APointCount);
  DataPos := SharedDataPos;

  for I := 0 to TupleCount - 1 do
  begin
    TupleDataEnd := DataPos + Tuples[I].DataSize;
    if (TupleDataEnd < DataPos) or (TupleDataEnd > GlyphEnd) then
      Exit;

    Scalar := TupleScalar(AInstance, Tuples[I].Peak, Tuples[I].StartCoord,
      Tuples[I].EndCoord, Tuples[I].Flags and IntermediateRegion <> 0);
    if Scalar <> 0 then
    begin
      Pos := DataPos;
      if Tuples[I].Flags and PrivatePointNumbers <> 0 then
      begin
        SetLength(Points, APointCount);
        if not DecodePointNumbers(Pos, TupleDataEnd, APointCount, Points,
          PointCount) then
          Exit;
        SetLength(Points, PointCount);
      end
      else if Length(SharedPoints) > 0 then
        Points := Copy(SharedPoints)
      else
      begin
        SetLength(Points, APointCount);
        for J := 0 to APointCount - 1 do
          Points[J] := J;
      end;

      PointCount := Length(Points);
      SetLength(XDelta, PointCount);
      SetLength(YDelta, PointCount);
      if not DecodeDeltas(Pos, TupleDataEnd, PointCount, XDelta) or
         not DecodeDeltas(Pos, TupleDataEnd, PointCount, YDelta) then
        Exit;

      SetLength(TupleX, APointCount);
      SetLength(TupleY, APointCount);
      SetLength(Touched, APointCount);
      FillChar(TupleX[0], Length(TupleX) * SizeOf(LongInt), 0);
      FillChar(TupleY[0], Length(TupleY) * SizeOf(LongInt), 0);
      FillChar(Touched[0], Length(Touched) * SizeOf(Boolean), 0);

      for J := 0 to PointCount - 1 do
      begin
        P := Points[J];
        if (P >= 0) and (P < APointCount) then
        begin
          Inc(TupleX[P], XDelta[J]);
          Inc(TupleY[P], YDelta[J]);
          Touched[P] := True;
        end;
      end;

      if PointCount <> APointCount then
      begin
        ContourStart := 0;
        for J := 0 to AContourCount - 1 do
        begin
          LastContourEnd := AConEnds^[J];
          if (LastContourEnd >= ContourStart) and
             (LastContourEnd < APointCount - TTVar_PhantomPointCount) then
            IUPContour(APoints, ContourStart, LastContourEnd, TupleX, TupleY,
              Touched);
          ContourStart := LastContourEnd + 1;
        end;
      end;

      for J := 0 to APointCount - 1 do
      begin
        P := ApplyScalar(TupleX[J], Scalar);
        if AAlreadyScaled then
          P := ScaleDelta(P, AXScale1, AXScale2);
        Inc(NetX[J], P);

        P := ApplyScalar(TupleY[J], Scalar);
        if AAlreadyScaled then
          P := ScaleDelta(P, AYScale1, AYScale2);
        Inc(NetY[J], P);
      end;
    end;

    DataPos := TupleDataEnd;
  end;

  for I := 0 to APointCount - 1 do
  begin
    Inc(APoints^[I].x, NetX[I]);
    Inc(APoints^[I].y, NetY[I]);
  end;
end;

end.
