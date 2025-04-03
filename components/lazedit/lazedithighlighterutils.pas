{
 *****************************************************************************
  This file is part of the LazEdit package from the Lazarus IDE.

  This content of this file is licensensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
 *****************************************************************************
}
unit LazEditHighlighterUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazClasses, Generics.Collections, Generics.Defaults;

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

finalization
  LazHighlighterRangesList.Free;
end.

