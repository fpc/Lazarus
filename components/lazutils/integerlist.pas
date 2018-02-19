{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

// A list of integers implemented using generics.
// Supports the same methods and properties as TStringList does for strings, except
//  for "Sorted" property. Thus integers cannot be added to a sorted list correctly.

unit IntegerList;

{$mode objfpc}{$H+}

interface

uses
  fgl;

type

  TByteList = class(specialize TFPGList<Byte>)
  public
    procedure Sort; overload;
  end;

  TWordList = class(specialize TFPGList<Word>)
  public
    procedure Sort; overload;
  end;

  TCardinalList = class(specialize TFPGList<Cardinal>)
  public
    procedure Sort; overload;
  end;

  TIntegerList = class(specialize TFPGList<Integer>)
  public
    procedure Sort; overload;
  end;

  TInt64List = class(specialize TFPGList<Int64>)
  public
    procedure Sort; overload;
  end;


implementation

function CompareByte(const Item1, Item2: Byte): Integer;
begin
  if Item1 > Item2 then
    Result := 1
  else if Item1 < Item2 then
    Result := -1
  else
    Result := 0;
end;

function CompareWord(const Item1, Item2: Word): Integer;
begin
  if Item1 > Item2 then
    Result := 1
  else if Item1 < Item2 then
    Result := -1
  else
    Result := 0;
end;

function CompareCardinal(const Item1, Item2: Cardinal): Integer;
begin
  if Item1 > Item2 then
    Result := 1
  else if Item1 < Item2 then
    Result := -1
  else
    Result := 0;
end;

function CompareInteger(const Item1, Item2: Integer): Integer;
begin
  if Item1 > Item2 then
    Result := 1
  else if Item1 < Item2 then
    Result := -1
  else
    Result := 0;
end;

function CompareInt64(const Item1, Item2: Int64): Integer;
begin
  if Item1 > Item2 then
    Result := 1
  else if Item1 < Item2 then
    Result := -1
  else
    Result := 0;
end;

{ TByteList }

procedure TByteList.Sort;
begin
  inherited Sort(@CompareByte);
end;

{ TWordList }

procedure TWordList.Sort;
begin
  inherited Sort(@CompareWord);
end;

{ TCardinalList }

procedure TCardinalList.Sort;
begin
  inherited Sort(@CompareCardinal);
end;

{ TIntegerList }

procedure TIntegerList.Sort;
begin
  inherited Sort(@CompareInteger);
end;

{ TInt64List }

procedure TInt64List.Sort;
begin
  inherited Sort(@CompareInt64);
end;

end.

