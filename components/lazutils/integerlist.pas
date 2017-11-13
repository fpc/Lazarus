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

  TIntegerList = class(specialize TFPGList<Integer>)
  public
    procedure Sort; overload;
  end;

  TInt64List = class(specialize TFPGList<Int64>)
  public
    procedure Sort; overload;
  end;


implementation

function CompareInteger(const Item1, Item2: Integer): Integer;
begin
  Result := Item1 - Item2;
end;

function CompareInt64(const Item1, Item2: Int64): Integer;
begin
  Result := Item1 - Item2;
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

