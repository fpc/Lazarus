{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterHashEntries.pas, released 2000-04-21.

The Initial Author of this file is Michael Hieke.
Portions created by Michael Hieke are Copyright 2000 Michael Hieke.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

{
@abstract(Support classes for SynEdit highlighters that create the keyword lists at runtime.)
@author(Michael Hieke)
@created(2000-04-21)
@lastmod(2001-09-07)
The classes in this unit can be used to use the hashing algorithm while still
having the ability to change the set of keywords.
}
unit SynHighlighterHashEntries;

{$I SynEdit.inc}

interface

uses
  SysUtils, SynEditTypes, Classes;

type
  { Class to hold the keyword to recognize, its length and its token kind. The
    keywords that have the same hashvalue are stored in a single-linked list,
    with the Next property pointing to the next entry. The entries are ordered
    over the keyword length. }

  { TSynHashEntryBase }

  TSynHashEntryBase = class(TObject)
  protected
    { Points to the next keyword entry with the same hashvalue. }
    fNext: TSynHashEntryBase;
    { Length of the keyword. }
    fKeyLen: integer;
    { The keyword itself. }
    fKeyword: string;
  public
    { Adds a keyword entry with the same hashvalue. Depending on the length of
      the two keywords it might return Self and store NewEntry in the Next
      pointer, or return NewEntry and make the Next property of NewEntry point
      to Self. This way the order of keyword length is preserved. }
    function AddEntry(var NewEntry: TSynHashEntryBase; NoException: boolean = False): TSynHashEntryBase; virtual;
    { Creates a keyword entry for the given keyword and token kind. }
    constructor Create(const AKey: string);
    { Destroys the keyword entry and all other keyword entries Next points to. }
    destructor Destroy; override;
  public
    { The keyword itself. }
    property Keyword: string read fKeyword;
    { Length of the keyword. }
    property KeywordLen: integer read fKeyLen;
    { Points to the next keyword entry with the same hashvalue. }
    property Next: TSynHashEntryBase read fNext;
  end;

  { TGenSynHashEntryBase }

  generic TGenSynHashEntryBase<T> = class(TSynHashEntryBase)
  private
    function GetNext: T; inline;
  public
    property Next: T read GetNext;
  end;


  generic TGenSynHashEntry<T> = class(specialize TGenSynHashEntryBase<T>)
  protected
    { Keyword token kind, has to be typecasted to the real token kind type. }
    fKind: integer;
  public
    constructor Create(const AKey: string; AKind: integer);
    { Keyword token kind, has to be typecasted to the real token kind type. }
    property Kind: integer read fKind;
  end;

  TSynHashEntry = class;
  TSynHashEntry = class(specialize TGenSynHashEntry<TSynHashEntry>);

  { A list of keyword entries, stored as single-linked lists under the hashvalue
    of the keyword. }

  { TGenSynHashEntryList }

  generic TGenSynHashEntryList<HE: TSynHashEntryBase> = class(TList)
  protected
    { Returns the first keyword entry for a given hashcalue, or nil. }
    function Get(HashKey: Integer): HE;
    { Adds a keyword entry under its hashvalue. Will grow the list count when
      necessary, so the maximum hashvalue should be limited outside. The correct
      order of keyword entries is maintained. }
    procedure Put(HashKey: Integer; Entry: HE);
  public
    { Clears the list and frees all contained keyword entries. }
    procedure Clear; override;
  public
    function FindOnAdd(HashKey: Integer; Entry: HE): HE;
    { Type-safe access to the first keyword entry for a hashvalue. }
    property Items[Index: integer]: HE read Get write Put; default;
  end;

  TSynHashEntryList = specialize TGenSynHashEntryList<TSynHashEntry>;

  { Procedural type for adding keyword entries to a TGenSynHashEntryList when
    iterating over all the keywords contained in a string. }
  TEnumerateKeywordEvent = procedure(AKeyword: string; AKind: integer)
    of object;

{ This procedure will call AKeywordProc for all keywords in KeywordList. A
  keyword is considered any number of successive chars that are contained in
  Identifiers, with chars not contained in Identifiers before and after them. }
procedure EnumerateKeywords(AKind: integer; KeywordList: string;
  Identifiers: TSynIdentChars; AKeywordProc: TEnumerateKeywordEvent);
procedure EnumerateKeywords(AKind: integer; KeywordList: array of string;
  AKeywordProc: TEnumerateKeywordEvent);

implementation

procedure EnumerateKeywords(AKind: integer; KeywordList: string;
  Identifiers: TSynIdentChars; AKeywordProc: TEnumerateKeywordEvent);
var
  pStart, pEnd: PChar;
  Keyword: string;
begin
  if Assigned(AKeywordProc) and (KeywordList <> '') then begin
    pEnd := PChar(KeywordList);
    pStart := pEnd;
    repeat
      // skip over chars that are not in Identifiers
      while (pStart^ <> #0) and not (pStart^ in Identifiers) do
        Inc(pStart);
      if pStart^ = #0 then break;
      // find the last char that is in Identifiers
      pEnd := pStart + 1;
      while (pEnd^ <> #0) and (pEnd^ in Identifiers) do
        Inc(pEnd);
      // call the AKeywordProc with the keyword
      SetString(Keyword, pStart, pEnd - pStart);
      AKeywordProc(Keyword, AKind);
      Keyword := '';
      // pEnd points to a char not in Identifiers, restart after that
      pStart := pEnd + 1;
    until (pStart^ = #0) or (pEnd^ = #0);
  end;
end;

procedure EnumerateKeywords(AKind: integer; KeywordList: array of string;
  AKeywordProc: TEnumerateKeywordEvent);
var
  KeyWord: String;
begin
  if Assigned(AKeywordProc) then begin
    for KeyWord in KeywordList do
      AKeywordProc(KeyWord, AKind);
  end;
end;

{ TSynHashEntryBase }

constructor TSynHashEntryBase.Create(const AKey: string);
begin
  inherited Create;
  fKeyLen := Length(AKey);
  fKeyword := AKey;
end;

destructor TSynHashEntryBase.Destroy;
begin
  fNext.Free;
  inherited Destroy;
end;

function TSynHashEntryBase.AddEntry(var NewEntry: TSynHashEntryBase; NoException: boolean
  ): TSynHashEntryBase;
begin
  Result := Self;
  if Assigned(NewEntry) then begin
    if CompareText(NewEntry.Keyword, fKeyword) = 0 then begin
      if not NoException then
        raise Exception.CreateFmt('Keyword "%s" already in list', [fKeyword]);
      NewEntry := Self;
      exit;
    end;
    if NewEntry.fKeyLen < fKeyLen then begin
      NewEntry.fNext := Self;
      Result := NewEntry;
    end else if Assigned(fNext) then
      fNext := fNext.AddEntry(NewEntry, NoException)
    else
      fNext := NewEntry;
  end;
end;

{ TGenSynHashEntryBase }

function TGenSynHashEntryBase.GetNext: T;
begin
  Result := T(fNext);
end;

{ TGenSynHashEntry }

constructor TGenSynHashEntry.Create(const AKey: string; AKind: integer);
begin
  inherited Create(AKey);
  fKind := AKind;
end;

{ TGenSynHashEntryList }

procedure TGenSynHashEntryList.Clear;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    HE(Items[i]).Free;
  inherited Clear;
end;

function TGenSynHashEntryList.FindOnAdd(HashKey: Integer; Entry: HE): HE;
var
  ListEntry: TSynHashEntryBase;
begin
  Result := Entry;
  if HashKey >= Count then
    Count := HashKey + 1;
  ListEntry := TSynHashEntryBase(inherited Items[HashKey]);
  // if there is already a hashentry for this hashvalue let it decide
  // where to put the new entry in its single linked list
  if Assigned(ListEntry) then
    Entry := HE(ListEntry.AddEntry(TSynHashEntryBase(Result), True));
  inherited Items[HashKey] := Pointer(Entry);
end;

function TGenSynHashEntryList.Get(HashKey: Integer): HE;
begin
  if (HashKey >= 0) and (HashKey < Count) then
    Result := HE(inherited Items[HashKey])
  else
    Result := HE(nil);
end;

procedure TGenSynHashEntryList.Put(HashKey: Integer; Entry: HE);
var
  ListEntry: TSynHashEntryBase;
begin
  if HashKey >= Count then
    Count := HashKey + 1;
  ListEntry := TSynHashEntryBase(inherited Items[HashKey]);
  // if there is already a hashentry for this hashvalue let it decide
  // where to put the new entry in its single linked list
  if Assigned(ListEntry) then
    Entry := HE(ListEntry.AddEntry(TSynHashEntryBase(Entry)));
  inherited Items[HashKey] := Pointer(Entry);
end;

end.

