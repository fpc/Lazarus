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
    Internally it uses a string->integer map container to store
     the string again as key and string's index as value.
     It is then used for InserItem, Contains, IndexOf and Find methods.

    The extra container does not reserve too much memory because the strings are
     reference counted and not really copied.

    The list cannot have duplicates.
    Duplicates property values dupIgnore and dupError are fully supported,
     unlike in unsorted StringList.

    This list cannot be sorted. For a sorted list you should use normal TStringList.
    This class is useful only when you must preserve the order in list, but
     also need to do fast lookups to see if a string exists, or must prevent duplicates.
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
    function GetDuplicates: TDuplicates;
    function GetSorted: Boolean;
    procedure SetDuplicates(AValue: TDuplicates);
    procedure SetSorted(AValue: Boolean);
    procedure AdjustMap(StartIndex, Offset: Integer);
  protected
    procedure InsertItem(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; O: TObject); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Contains(const S: string): Boolean; // A new function
    function Find(const S: string; out Index: Integer): Boolean; override;
    function IndexOf(const S: string): Integer; override;
    procedure Sort; override;
    procedure ValidateMap;  // For debugging purposes only
  public
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
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

procedure TDictionaryStringList.AdjustMap(StartIndex, Offset: Integer);
// Adjust all indexes >= startindex in map.
// This is needed after inserting or deleting an item.
var
  i: Integer;
  Ind: PtrInt;
begin
  for i := StartIndex to Count-1 do
  begin
    Ind := PtrInt(FMap[Strings[i]]);
    FMap[Strings[i]] := Pointer(Ind + Offset);
  end;
end;

procedure TDictionaryStringList.Clear;
begin
  inherited Clear;
  FMap.Clear;
end;

procedure TDictionaryStringList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  // Decrement Indexes in map above the deleted item.
  AdjustMap(Index, -1);
end;

procedure TDictionaryStringList.Exchange(Index1, Index2: Integer);
var
  s1, s2: string;
begin
  S1 := Strings[Index1];
  S2 := Strings[Index2];
  inherited Exchange(Index1, Index2);
  // Exchange Indexes in map, too.
  FMap[s1] := Pointer(PtrInt(Index2));
  FMap[s2] := Pointer(PtrInt(Index1));
end;

procedure TDictionaryStringList.InsertItem(Index: Integer; const S: string);
var
  i: PtrInt;
begin
  if Duplicates <> dupAccept then
    if IndexOf(S) <> -1 then
      case Duplicates of
        DupIgnore : Exit;
        DupError : raise Exception.Create('TDictionaryStringList.InsertItem:'
                                         +' Duplicates are not allowed.');
      end;
  inherited InsertItem(Index, S);
  i := Index;
  FMap[S] := Pointer(i);     // Store index to map.
  // Increment Indexes in map above the inserted item.
  AdjustMap(Index+1, 1);
end;

procedure TDictionaryStringList.InsertItem(Index: Integer; const S: string; O: TObject);
begin
  raise Exception.Create('TDictionaryStringList.InsertItem: is this needed?');
  //inherited InsertItem(Index, S, O);
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

procedure TDictionaryStringList.ValidateMap;
// For debugging purposes only
var
  i, Ind: Integer;
  s: String;
begin
  for i := 0 to Count-1 do
  begin
    s := Strings[i];
    Ind := integer(PtrInt(FMap[s]));
    if Ind <> i then
      raise Exception.CreateFmt('Indexes for string "%s" differ, list:%d, map:%d.',
                                [s, i, Ind]);
  end;
end;

procedure TDictionaryStringList.Sort;
begin
  raise Exception.Create('This list is not meant to be sorted. Use TStringList instead.');
end;

function TDictionaryStringList.GetSorted: Boolean;
begin
  Result := False;
end;

procedure TDictionaryStringList.SetSorted(AValue: Boolean);
begin
  if AValue then
    Sort;  // Raise an exception here, too.
end;

function TDictionaryStringList.GetDuplicates: TDuplicates;
begin
  Result := inherited Duplicates;
end;

procedure TDictionaryStringList.SetDuplicates(AValue: TDuplicates);
begin
  if AValue = dupAccept then
    raise Exception.Create('Sorry, TDictionaryStringList does not support duplicates.');
  inherited Duplicates := AValue;
end;

end.

