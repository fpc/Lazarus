{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Juha Manninen

  Abstract:
    This is an unsorted StringList with a fast lookup feature.
    Internally it uses a map container (TStringToPointerTree) to store the string again
     as key and string's index as value.
     It is then used for Contains, IndexOf and Find methods.

    The extra container does not reserve too much memory because the strings are
     reference counted and not really copied.

    This list cannot be sorted. For a sorted list you should use normal TStringList.
    This class is useful only when you must preserve the order in list,
     but also need to do fast lookups to see if a string exists.

    "Duplicates" values dupIgnore and dupError are supported, unlike in unsorted StringList.

    Insert, Delete and Exchange are not supported yet. They require the map's
     index values to be adjusted. There are at least 3 ways to solve it:
    1. Adjust only the changed indexes after each operation.
    2. Mark the map's index values as "dirty" after those operations.
       Adjust all the indexes when IndexOf() or Find() is called for the first time.
    3. Decide that those operations are not needed.
       This is a very specialized container after all.
}
unit DictionaryStringList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AvgLvlTree;

type

  { TDictionaryStringList }

  TDictionaryStringList = class(TStringList)
  private
    FMap: TStringToPointerTree;
    function GetSorted: Boolean;
    procedure SetSorted(AValue: Boolean);
  protected
//    procedure InsertItem(Index: Integer; const S: string); override;
//    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Contains(const S: string): Boolean; // A new function
    function Find(const S: string; out Index: Integer): Boolean; override;
    function IndexOf(const S: string): Integer; override;
    procedure Sort; override;
  public
    property Sorted: Boolean read GetSorted write SetSorted;
  end;


implementation

{ TDictionaryStringList }

constructor TDictionaryStringList.Create;
begin
  inherited Create;
  FMap := TStringToPointerTree.Create(True);
end;

destructor TDictionaryStringList.Destroy;
begin
  FMap.Free;
  inherited Destroy;
end;

function TDictionaryStringList.Add(const S: string): Integer;
var
  i: PtrInt;
begin
  Result := -1;
  if Duplicates <> dupAccept then
    if IndexOf(S) <> -1 then
      case Duplicates of
        DupIgnore : Exit;
        DupError : raise Exception.Create('TDictionaryStringList.Add: Duplicates are not allowed.');
      end;
  Result := inherited Add(S);
  i := Result;
  FMap[S] := Pointer(i);     // Store index to map.
end;

procedure TDictionaryStringList.Insert(Index: Integer; const S: string);
begin
  raise Exception.Create('TDictionaryStringList.Insert is not implemented yet.');
  inherited Insert(Index, S);
  // ToDo: adjust all indexes in FMap after the item is inserted.
end;
{
procedure TDictionaryStringList.InsertItem(Index: Integer; const S: string);
begin
  inherited InsertItem(Index, S);
end;

procedure TDictionaryStringList.InsertItem(Index: Integer; const S: string; O: TObject);
begin
  inherited InsertItem(Index, S, O);
end;
}
procedure TDictionaryStringList.Clear;
begin
  inherited Clear;
  FMap.Clear;
end;

procedure TDictionaryStringList.Delete(Index: Integer);
begin
  raise Exception.Create('TDictionaryStringList.Delete is not implemented yet.');
  inherited Delete(Index);
  // ToDo: adjust all indexes in FMap after the item is deleted.
end;

procedure TDictionaryStringList.Exchange(Index1, Index2: Integer);
begin
  raise Exception.Create('TDictionaryStringList.Exchange is not implemented yet.');
  inherited Exchange(Index1, Index2);
  // ToDo: adjust all indexes in FMap after Exchange.
end;

function TDictionaryStringList.Contains(const S: string): Boolean;
begin
  Result := FMap.Contains(S);
end;

function TDictionaryStringList.Find(const S: string; out Index: Integer): Boolean;
begin
  Index := IndexOf(S);
  Result := Index <> -1;
end;

function TDictionaryStringList.IndexOf(const S: string): Integer;
begin
  if FMap.Contains(S) then
    Result := integer(PtrInt(FMap[S])) // Index is stored in the map.
  else
    Result := -1
end;

procedure TDictionaryStringList.Sort;
begin
  raise Exception.Create('This list is not meant to be sorted. Use TStringList instead.');
end;

procedure TDictionaryStringList.SetSorted(AValue: Boolean);
begin
  if AValue then
    Sort;  // Raise an exception here, too.
end;

function TDictionaryStringList.GetSorted: Boolean;
begin
  Result := False;
end;

end.

