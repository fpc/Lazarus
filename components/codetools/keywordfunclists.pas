{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    A TKeyWordFunctionList is a list of TKeyWordFunctionListItem.
}
unit KeywordFuncLists;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

{$R-} // turn range checking off for speed
{$inline on}

interface

{ $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs;

type
  TKeyWordFunction = function: boolean of object;
  TKeyWordDataFunction = function(Data: Pointer): boolean of object;

  TBaseKeyWordFunctionListItem = record
    IsLast: boolean;
    KeyWord: shortstring;
    DoIt: TKeyWordFunction;
    DoDataFunction: TKeyWordDataFunction;
  end;
  PKeyWordFunctionListItem = ^TBaseKeyWordFunctionListItem;

  { TBaseKeyWordFunctionList }

  TBaseKeyWordFunctionList = class
  private
    FHasOnlyIdentifiers: boolean;
    FItems: PKeyWordFunctionListItem;
    FCount: integer;
    FCapacity: integer;
    FName: string;
    FSorted: boolean;
    FBucketStart: {$ifdef FPC}^{$else}array of {$endif}integer;
    FMaxHashIndex: integer;
    function KeyWordToHashIndex(const AKeyWord: shortstring): integer;
    function KeyWordToHashIndex(const ASource: string;
       AStart, ALen: integer): integer;
    function KeyWordToHashIndex(Identifier: PChar): integer;
    function KeyWordToHashIndex(Start: PChar; Len: integer): integer;
  public
    DefaultKeyWordFunction: TKeyWordFunction;
    constructor Create(const aName: string = '');
    destructor Destroy;  override;
    procedure Clear;
    procedure Add(const AKeyWord: shortstring;
                  const AFunction: TKeyWordFunction);
    procedure AddExtended(const AKeyWord: shortstring;
                          const AFunction: TKeyWordFunction;
                          const ADataFunction: TKeyWordDataFunction);
    procedure Add(List: TBaseKeyWordFunctionList);
    procedure Sort;
    property Sorted: boolean read FSorted;
    procedure WriteDebugListing;
    function AllwaysTrue: boolean;
    function AllwaysFalse: boolean;
    property Count: integer read FCount;
    function GetItem(Index: integer): TBaseKeyWordFunctionListItem;
    function IndexOf(const AKeyWord: shortstring; UseSort: boolean): integer;
    function CalcMemSize: PtrUInt;
    property HasOnlyIdentifiers: boolean read FHasOnlyIdentifiers;
    property Name: string read FName write FName;
  end;

  { TKeyWordFunctionList }

  TKeyWordFunctionList = class(TBaseKeyWordFunctionList)
  public
    function DoIdentifier(Identifier: PChar): boolean;
    function DoItCaseSensitive(const AKeyWord: shortstring): boolean;
    function DoItCaseInsensitive(const AKeyWord: shortstring): boolean;
    function DoItCaseInsensitive(const ASource: string;
                                 KeyWordStart, KeyWordLen: integer): boolean;
    function DoItCaseSensitive(const ASource: string;
                               KeyWordStart, KeyWordLen: integer): boolean;
    function DoItUppercase(const AnUpperSource: string;
                           KeyWordStart, KeyWordLen: integer): boolean;
    function DoItCaseInsensitive(Identifier: PChar): boolean;
    function DoDataFunction(Start: PChar; Len: integer; Data: pointer): boolean;
  end;

  { TSharedKeyWordFunctionList }

  TSharedKeyWordFunctionList = class(TBaseKeyWordFunctionList)
  private
    function Call(Target: TObject; Func: TKeyWordFunction): boolean; inline;
  public
    function DoItCaseInsensitive(Target: TObject; const ASource: string;
                                 KeyWordStart, KeyWordLen: integer): boolean;
  end;

var
  IsKeyWordMethodSpecifier,
  IsKeyWordProcedureSpecifier,
  IsKeyWordProcedureTypeSpecifier,
  IsKeyWordProcedureBracketSpecifier,
  IsKeyWordCallingConvention,
  IsKeyWordSection,
  IsKeyWordInConstAllowed,
  AllKeyWords,
  WordIsKeyWord,
  WordIsDelphiKeyWord,
  WordIsMacPasKeyWord,
  IsWordBuiltInFunc,
  WordIsTermOperator,
  WordIsPropertySpecifier,
  WordIsBlockKeyWord,
  EndKeyWordFuncList,
  PackedTypesKeyWordFuncList,
  BitPackedTypesKeyWordFuncList,
  GenericTypesKeyWordFuncList,
  BlockStatementStartKeyWordFuncList,
  WordIsLogicalBlockStart,
  WordIsLogicalBlockEnd,
  WordIsLogicalBlockMiddle,
  WordIsBlockStatementStart,
  WordIsBlockStatementEnd,
  WordIsBlockStatementMiddle,
  WordIsBinaryOperator,
  WordIsLvl1Operator, WordIsLvl2Operator, WordIsLvl3Operator, WordIsLvl4Operator,
  WordIsBooleanOperator,
  WordIsOrdNumberOperator,
  WordIsNumberOperator,
  WordIsCustomOperator,
  WordIsPredefinedFPCIdentifier,
  WordIsPredefinedDelphiIdentifier,
  WordIsPredefinedPas2jsIdentifier,
  UnexpectedKeyWordInBeginBlock,
  UnexpectedKeyWordInAsmBlock,
  UnexpectedKeyWordInBrackets
    : TKeyWordFunctionList;
  UpChars: array[char] of char;

  IsSpaceChar,       // [#0..#32]
  IsLineEndChar,     // [#10,#13]
  IsWordChar,        // ['a'..'z','A'..'Z']
  IsNonWordChar,     // [#0..#127]-IsIdentChar
  IsIdentStartChar,  // ['a'..'z','A'..'Z','_']
  IsIdentChar,       // ['a'..'z','A'..'Z','_','0'..'9']
  IsDottedIdentChar, // ['.','a'..'z','A'..'Z','_','0'..'9']
  IsNumberChar,      // ['0'..'9']
  IsCommentStartChar,
  IsCommentEndChar,
  IsHexNumberChar,   // ['0'..'9','a'..'f','A'..'F']
  IsOctNumberChar,   // ['0'..'7']
  IsEqualOperatorStartChar, // [':','+','-','/','*','<','>']
  IsAfterFloatPointChar: // ['0'..'9','e','E']
    array[char] of boolean;

function UpperCaseStr(const s: string): string;
function IsUpperCaseStr(const s: string): boolean;
function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;

function CalcMemSize: PtrUInt;

implementation

var
  CharToIHash: array[char] of integer;
  UpWords: array[word] of word;
  KeyWordLists: TFPList;

function UpperCaseStr(const s: string): string;
var i, l, l2: integer;
  pSrc, pDest: PWord;
begin
  l:=length(s);
  SetLength(Result,l);
  if l>0 then begin
    pDest:=PWord(@Result[1]);
    pSrc:=PWord(@s[1]);
    l2:=(l shr 1)-1;
    for i:=0 to l2 do
      pDest[i]:=UpWords[pSrc[i]];
    if odd(l) then
      Result[l]:=UpChars[s[l]];
  end;
end;

function IsUpperCaseStr(const s: string): boolean;
var i, l: integer;
begin
  l:=length(s);
  i:=1;
  while (i<=l) and (UpChars[s[i]]=s[i]) do inc(i);
  Result:=i>l;
end;

function CompareIdentifiers(Identifier1, Identifier2: PChar): integer;
begin
  if (Identifier1<>nil) then begin
    if (Identifier2<>nil) then begin
      while (UpChars[Identifier1[0]]=UpChars[Identifier2[0]]) do begin
        if (IsIdentChar[Identifier1[0]]) then begin
          inc(Identifier1);
          inc(Identifier2);
        end else begin
          Result:=0; // for example  'aaA;' 'aAa;'
          exit;
        end;
      end;
      if (IsIdentChar[Identifier1[0]]) then begin
        if (IsIdentChar[Identifier2[0]]) then begin
          if UpChars[Identifier1[0]]>UpChars[Identifier2[0]] then
            Result:=-1 // for example  'aab' 'aaa'
          else
            Result:=1; // for example  'aaa' 'aab'
        end else begin
          Result:=-1; // for example  'aaa' 'aa;'
        end;
      end else begin
        if (IsIdentChar[Identifier2[0]]) then
          Result:=1 // for example  'aa;' 'aaa'
        else
          Result:=0; // for example  'aa;' 'aa,'
      end;
    end else begin
      Result:=-1; // for example  'aaa' nil
    end;
  end else begin
    if (Identifier2<>nil) then begin
      Result:=1; // for example  nil 'bbb'
    end else begin
      Result:=0; // for example  nil nil
    end;
  end;
end;

function CalcMemSize: PtrUInt;
var
  i: Integer;
begin
  Result:=0;
  if KeyWordLists<>nil then begin
    inc(Result,PtrUInt(KeyWordLists.InstanceSize)
      +SizeOf(Pointer)*PtrUInt(KeyWordLists.Capacity));
    for i:=0 to KeyWordLists.Count-1 do
      inc(Result,TBaseKeyWordFunctionList(KeyWordLists[i]).CalcMemSize);
  end;
end;

{ TBaseKeyWordFunctionList }

constructor TBaseKeyWordFunctionList.Create(const aName: string);
begin
  inherited Create;
  FName:=aName;
  FSorted:=true;
  FMaxHashIndex:=-1;
  DefaultKeyWordFunction:={$ifdef FPC}@{$endif}AllwaysFalse;
  FHasOnlyIdentifiers:=true;
end;

destructor TBaseKeyWordFunctionList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TBaseKeyWordFunctionList.CalcMemSize: PtrUInt;
begin
  Result:=PtrUInt(InstanceSize);
  if FItems<>nil then
    inc(Result,SizeOf(TBaseKeyWordFunctionListItem)*FCapacity);
  if FBucketStart<>nil then
    inc(Result,FMaxHashIndex*SizeOf(integer));
end;

procedure TBaseKeyWordFunctionList.Clear;
begin
  if FItems<>nil then begin
    FreeMem(FItems);
    FItems:=nil;
  end;
  FCount:=0;
  FCapacity:=0;
  if FBucketStart<>nil then begin
    FreeMem(FBucketStart);
    FBucketStart:=nil;
  end;
  FMaxHashIndex:=-1;  
  FSorted:=true;
  FHasOnlyIdentifiers:=true;
end;

function TBaseKeyWordFunctionList.KeyWordToHashIndex(
  const AKeyWord: shortstring): integer;
var KeyWordLen, i: integer;
begin
  KeyWordLen:=length(AKeyWord);
  if KeyWordLen>20 then KeyWordLen:=20;
  Result:=0;
  for i:=1 to KeyWordLen do
    inc(Result,CharToIHash[AKeyWord[i]]);
  if Result>FMaxHashIndex then Result:=-1;
end;

function TBaseKeyWordFunctionList.KeyWordToHashIndex(const ASource: string;
  AStart, ALen: integer): integer;
var i, AEnd: integer;
begin
  if ALen>20 then ALen:=20;
  AEnd:=AStart+ALen-1;
  Result:=0;
  for i:=AStart to AEnd do
    inc(Result,CharToIHash[ASource[i]]);
  if Result>FMaxHashIndex then Result:=-1;
end;

function TBaseKeyWordFunctionList.KeyWordToHashIndex(Identifier: PChar): integer;
var i: integer;
begin
  Result:=0;
  i:=20;
  while (i>0) and IsIdentChar[Identifier[0]] do begin
    inc(Result,CharToIHash[Identifier[0]]);
    dec(i);
    inc(Identifier);
  end;
  if Result>FMaxHashIndex then Result:=-1;
end;

function TBaseKeyWordFunctionList.KeyWordToHashIndex(Start: PChar; Len: integer
  ): integer;
begin
  Result:=0;
  if Len>20 then Len:=20;
  while (Len>0) do begin
    inc(Result,CharToIHash[Start^]);
    dec(Len);
    inc(Start);
  end;
  if Result>FMaxHashIndex then Result:=-1;
end;

procedure TBaseKeyWordFunctionList.Add(const AKeyWord: shortstring;
  const AFunction: TKeyWordFunction);
begin
  AddExtended(AKeyWord,AFunction,nil);
end;

procedure TBaseKeyWordFunctionList.AddExtended(const AKeyWord: shortstring;
  const AFunction: TKeyWordFunction; const ADataFunction: TKeyWordDataFunction
    );
begin
  if length(AKeyWord)>=255 then
    raise Exception.Create('TBaseKeyWordFunctionList.AddExtended keyword too big');
  FSorted:=false;
  if FCount=FCapacity then begin
    FCapacity:=FCapacity*2+10;
    ReAllocMem(FItems,SizeOf(TBaseKeyWordFunctionListItem)*FCapacity);
  end;
  FillChar(FItems[FCount],SizeOF(TBaseKeyWordFunctionListItem),0);
  with FItems[FCount] do begin
    KeyWord:=AKeyWord;
    KeyWord[length(KeyWord)+1]:=#0;
    DoIt:=AFunction;
    DoDataFunction:=ADataFunction;
  end;
  inc(FCount);
  if not IsValidIdent(AKeyWord) then
    FHasOnlyIdentifiers:=false;
end;

procedure TBaseKeyWordFunctionList.Add(List: TBaseKeyWordFunctionList);
var
  i: Integer;
begin
  for i:=0 to List.FCount-1 do begin
    if IndexOf(List.FItems[i].KeyWord,false)<0 then begin
      AddExtended(List.FItems[i].KeyWord,List.FItems[i].DoIt,
                  List.FItems[i].DoDataFunction);
    end;
  end;
end;

procedure TBaseKeyWordFunctionList.Sort;
// bucketsort
var i, h, NewMaxHashIndex: integer;
  UnsortedItems: PKeyWordFunctionListItem;
  Size: Integer;
begin
  if FSorted then exit;
  if FBucketStart<>nil then begin
    FreeMem(FBucketStart);
    FBucketStart:=nil;
  end;
  // find maximum hash index
  FMaxHashIndex:=99999;
  NewMaxHashIndex:=0;
  for i:=0 to FCount-1 do begin
    h:=KeyWordToHashIndex(FItems[i].KeyWord);
    if h>NewMaxHashIndex then NewMaxHashIndex:=h;
  end;
  FMaxHashIndex:=NewMaxHashIndex;
  // create hash index
  GetMem(FBucketStart,(FMaxHashIndex+1) * SizeOf(integer));
  // compute every hash value count
  for i:=0 to FMaxHashIndex do FBucketStart[i]:=0;
  for i:=0 to FCount-1 do begin
    h:=KeyWordToHashIndex(FItems[i].KeyWord);
    if h>=0 then inc(FBucketStart[h]);
    FItems[i].IsLast:=false;
  end;
  // change hash-count-index to bucket-end-index
  h:=0;
  for i:=0 to FMaxHashIndex-1 do begin
    inc(h,FBucketStart[i]);
    if FBucketStart[i]=0 then
      FBucketStart[i]:=-1
    else
      FBucketStart[i]:=h;
  end;
  inc(FBucketStart[FMaxHashIndex],h);
  // copy all items (just the data, not the string contents)
  Size:=sizeof(TBaseKeyWordFunctionListItem)*FCount;
  GetMem(UnsortedItems,Size);
  Move(FItems^,UnsortedItems^,Size);
  // copy unsorted items to Items back and do the bucket sort
  for i:=FCount-1 downto 0 do begin
    h:=KeyWordToHashIndex(UnsortedItems[i].KeyWord);
    if h>=0 then begin
      dec(FBucketStart[h]);
      // copy item back (just the data, not the string contents)
      Move(UnsortedItems[i],FItems[FBucketStart[h]],
           SizeOf(TBaseKeyWordFunctionListItem));
    end;
  end;
  // free UnsortedItems
  FreeMem(UnsortedItems);
  // set IsLast
  if FCount>0 then begin
    for i:=1 to FMaxHashIndex do
      if FBucketStart[i]>0 then
        FItems[FBucketStart[i]-1].IsLast:=true;
    FItems[FCount-1].IsLast:=true;
  end;
  // tidy up
  FSorted:=true;
end;

procedure TBaseKeyWordFunctionList.WriteDebugListing;
var i: integer;
begin
  Sort;
  DebugLn('[TBaseKeyWordFunctionList.WriteDebugListing]');
  DebugLn('  ItemsCount=',dbgs(FCount),'  MaxHash=',dbgs(FMaxHashIndex)
     ,'  Sorted=',dbgs(FSorted));
  for i:=0 to FCount-1 do begin
    DbgOut('    '+dbgs(i)+':');
    with FItems[i] do begin
      DbgOut(' "'+KeyWord+'"');
      DbgOut(' Hash=',dbgs(KeyWordToHashIndex(KeyWord)));
      DbgOut(' IsLast=',dbgs(IsLast));
      DbgOut(' DoIt=',dbgs(Assigned(DoIt)));
    end;
    DebugLn('');
  end;
  DbgOut('  BucketStart array:');
  for i:=0 to FMaxHashIndex do begin
    if FBucketStart[i]>=0 then
      DbgOut(' '+dbgs(i)+'->'+dbgs(FBucketStart[i]));
  end;
  DebugLn('');
end;

function TBaseKeyWordFunctionList.AllwaysTrue: boolean;
begin
  Result:=true;
end;

function TBaseKeyWordFunctionList.AllwaysFalse: boolean;
begin
  Result:=false;
end;

function TBaseKeyWordFunctionList.GetItem(Index: integer
  ): TBaseKeyWordFunctionListItem;
begin
  Result:=FItems[Index];
end;

function TBaseKeyWordFunctionList.IndexOf(const AKeyWord: shortstring;
  UseSort: boolean): integer;
var
  i: Integer;
begin
  if UseSort then begin
    if not Sorted then Sort;

    i:=KeyWordToHashIndex(AKeyWord);
    if i>=0 then begin
      i:=FBucketStart[i];
      if i>=0 then begin
        repeat
          if CompareText(FItems[i].KeyWord,AKeyWord)=0 then
            exit(i);
          if FItems[i].IsLast then break;
          inc(i);
        until false;
      end;
    end;
  end else begin
    for i:=0 to FCount-1 do
      if CompareText(FItems[i].KeyWord,AKeyWord)=0 then exit(i);
  end;
  Result:=-1;
end;

{ TKeyWordFunctionList }

function TKeyWordFunctionList.DoIdentifier(Identifier: PChar): boolean;

  procedure RaiseNonIdentifiers;
  var
    i: Integer;
  begin
    debugln(['RaiseNonIdentifiers DoIdentifier does not work on this list. Name=',Name]);
    for i:=0 to FMaxHashIndex do
      debugln(['  ',i,'=',FItems[i].KeyWord]);
    RaiseCatchableException('TKeyWordFunctionList.DoIdentifier');
  end;

var i: integer;
begin
  if not FHasOnlyIdentifiers then RaiseNonIdentifiers;
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(Identifier);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      repeat
        if CompareIdentifiers(PChar(@FItems[i].KeyWord[1]),Identifier)=0 then begin
          if Assigned(FItems[i].DoIt) then
            Result:=FItems[i].DoIt()
          else
            Result:=DefaultKeyWordFunction();
          exit;
        end;
        if FItems[i].IsLast then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoItCaseSensitive(const AKeyWord: shortstring
  ): boolean;
var
  i: integer;
  KeyWordFuncItem: PKeyWordFunctionListItem;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(AKeyWord);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      KeyWordFuncItem:=@FItems[i];
      repeat
        if (KeyWordFuncItem^.KeyWord=AKeyWord) then begin
          if Assigned(KeyWordFuncItem^.DoIt) then
            Result:=KeyWordFuncItem^.DoIt()
          else
            Result:=DefaultKeyWordFunction();
          exit;  
        end;
        if KeyWordFuncItem^.IsLast then break;
        inc(KeyWordFuncItem);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoItCaseInsensitive(const ASource: string;
  KeyWordStart, KeyWordLen: integer): boolean;
// ! does not test if length(ASource) >= KeyWordStart+KeyWordLen -1
var i, KeyPos, WordPos: integer;
  KeyWordFuncItem: PKeyWordFunctionListItem;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(ASource,KeyWordStart,KeyWordLen);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      dec(KeyWordStart);
      repeat
        KeyWordFuncItem:=@FItems[i];
        if length(KeyWordFuncItem^.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=KeyWordStart+KeyWordLen;
          while (KeyPos>=1) 
          and (KeyWordFuncItem^.KeyWord[KeyPos]=UpChars[ASource[WordPos]]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem^.DoIt) then
              Result:=KeyWordFuncItem^.DoIt()
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem^.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoItCaseSensitive(const ASource: string;
  KeyWordStart, KeyWordLen: integer): boolean;
// ! does not test if length(ASource) >= KeyWordStart+KeyWordLen -1
var
  i, KeyPos, WordPos: integer;
  KeyWordFuncItem: PKeyWordFunctionListItem;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(ASource,KeyWordStart,KeyWordLen);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      dec(KeyWordStart);
      repeat
        KeyWordFuncItem:=@FItems[i];
        if length(KeyWordFuncItem^.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=KeyWordStart+KeyWordLen;
          while (KeyPos>=1)
          and (KeyWordFuncItem^.KeyWord[KeyPos]=ASource[WordPos]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem^.DoIt) then
              Result:=KeyWordFuncItem^.DoIt()
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem^.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoItCaseInsensitive(Identifier: PChar): boolean;
// checks
var i, KeyPos, KeyWordLen: integer;
  KeyWordFuncItem: PKeyWordFunctionListItem;
  IdentifierEnd, WordPos: PChar;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(Identifier);
  IdentifierEnd:=Identifier;
  while IsIdentChar[IdentifierEnd[0]] do inc(IdentifierEnd);
  KeyWordLen:=integer(IdentifierEnd-Identifier);
  dec(IdentifierEnd);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      repeat
        KeyWordFuncItem:=@FItems[i];
        if length(KeyWordFuncItem^.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=IdentifierEnd;
          while (KeyPos>=1)
          and (KeyWordFuncItem^.KeyWord[KeyPos]=UpChars[WordPos[0]]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem^.DoIt) then
              Result:=KeyWordFuncItem^.DoIt()
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem^.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoItUppercase(const AnUpperSource: string;
// use this function if ASource upcased
  KeyWordStart, KeyWordLen: integer): boolean;
// ! does not test if length(AnUpperSource) >= KeyWordStart+KeyWordLen -1
var i, KeyPos, WordPos: integer;
  KeyWordFuncItem: PKeyWordFunctionListItem;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(AnUpperSource,KeyWordStart,KeyWordLen);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      dec(KeyWordStart);
      repeat
        KeyWordFuncItem:=@FItems[i];
        if length(KeyWordFuncItem^.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=KeyWordStart+KeyWordLen;
          while (KeyPos>=1) 
          and (KeyWordFuncItem^.KeyWord[KeyPos]=AnUpperSource[WordPos]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem^.DoIt) then
              Result:=KeyWordFuncItem^.DoIt()
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem^.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoItCaseInsensitive(const AKeyWord: shortstring
  ): boolean;
var i: integer;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(AKeyWord);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      repeat
        if CompareText(FItems[i].KeyWord,AKeyWord)=0
        then begin
          if Assigned(FItems[i].DoIt) then
            Result:=FItems[i].DoIt()
          else
            Result:=DefaultKeyWordFunction();
          exit;
        end;
        if FItems[i].IsLast then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoDataFunction(Start: PChar; Len: integer;
  Data: pointer): boolean;
var i, KeyPos: integer;
  KeyWordFuncItem: PKeyWordFunctionListItem;
  WordPos: PChar;
  KeyWordEnd: PChar;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(Start,Len);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      KeyWordEnd:=PChar(Pointer(Start)+Len-1);
      repeat
        KeyWordFuncItem:=@FItems[i];
        if length(KeyWordFuncItem^.KeyWord)=Len then begin
          KeyPos:=Len;
          WordPos:=KeyWordEnd;
          while (KeyPos>=1)
          and (KeyWordFuncItem^.KeyWord[KeyPos]=UpChars[WordPos^]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem^.DoDataFunction) then
              Result:=KeyWordFuncItem^.DoDataFunction(Data)
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem^.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
var
  c: char;
  w: word;
begin
  for c:=Low(char) to High(char) do begin
    case c of
    'a'..'z':CharToIHash[c]:=ord(c)-ord('a')+1;
    'A'..'Z':CharToIHash[c]:=ord(c)-ord('A')+1;
    else CharToIHash[c]:=ord(c);
    end;
    UpChars[c]:=upcase(c);
    IsLineEndChar[c]:=c in [#10,#13];
    IsSpaceChar[c]:=c in [#0..#32];
    IsIdentStartChar[c]:=c in ['a'..'z','A'..'Z','_'];
    IsIdentChar[c]:=c in ['a'..'z','A'..'Z','_','0'..'9'];
    IsDottedIdentChar[c]:=c in ['.','a'..'z','A'..'Z','_','0'..'9'];
    IsNumberChar[c]:=c in ['0'..'9'];
    IsCommentStartChar[c]:=c in ['/','{','('];
    IsCommentEndChar[c]:=c in ['}',')',#13,#10];
    IsHexNumberChar[c]:=c in ['0'..'9','a'..'f','A'..'F'];
    IsOctNumberChar[c]:=c in ['0'..'7'];
    IsEqualOperatorStartChar[c]:=c in [':','+','-','/','*','<','>'];
    IsWordChar[c]:=c in ['a'..'z','A'..'Z'];
    IsNonWordChar[c]:=(c in [#0..#127]) and (not IsIdentChar[c]);
    IsAfterFloatPointChar[c]:=c in ['0'..'9','e','E'];
  end;
  for w:=Low(word) to High(word) do
    UpWords[w]:=ord(UpChars[chr(w and $ff)])+(ord(UpChars[chr(w shr 8)]) shl 8);

  KeyWordLists:=TFPList.Create;
  
  IsKeyWordMethodSpecifier:=TKeyWordFunctionList.Create('IsKeyWordMethodSpecifier');
  KeyWordLists.Add(IsKeyWordMethodSpecifier);
  with IsKeyWordMethodSpecifier do begin
    Add('ABSTRACT'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASSEMBLER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CDECL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTDECL'      ,{$ifdef FPC}@{$endif}AllwaysTrue); // often used for macros
    ADD('MWPASCAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOSTACKFRAME' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NORETURN'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEPRECATED'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPID'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DYNAMIC'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INLINE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MESSAGE'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OVERLOAD'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OVERRIDE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PLATFORM'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POPSTACK'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REGISTER'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REINTRODUCE'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STDCALL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAFECALL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STATIC'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIMPLEMENTED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VIRTUAL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PASCAL'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPERIMENTAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINAL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ENUMERATOR'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VARARGS'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VECTORCALL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTERNAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue); //jvm
  end;

  IsKeyWordProcedureSpecifier:=TKeyWordFunctionList.Create('IsKeyWordProcedureSpecifier');
  KeyWordLists.Add(IsKeyWordProcedureSpecifier);
  with IsKeyWordProcedureSpecifier do begin
    Add('ALIAS'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASSEMBLER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CDECL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COMPILERPROC' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEPRECATED'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPERIMENTAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPORT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTDECL'      ,{$ifdef FPC}@{$endif}AllwaysTrue); // used often for macros
    Add('EXTERNAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FAR'          ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FORWARD'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INLINE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERRUPT'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IOCHECK'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOCAL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    ADD('MWPASCAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NEAR'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NORETURN'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOSTACKFRAME' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OLDFPCCALL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OVERLOAD'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PASCAL'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PLATFORM'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POPSTACK'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PUBLIC'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REGISTER'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RTLPROC'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAFECALL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAVEREGISTERS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STDCALL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SYSCALL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIMPLEMENTED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VARARGS'      ,{$ifdef FPC}@{$endif}AllwaysTrue); // kylix
    Add('VECTORCALL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WEAKEXTERNAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('['            ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  IsKeyWordProcedureTypeSpecifier:=TKeyWordFunctionList.Create('IsKeyWordProcedureTypeSpecifier');
  KeyWordLists.Add(IsKeyWordProcedureTypeSpecifier);
  with IsKeyWordProcedureTypeSpecifier do begin
    Add('STDCALL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REGISTER'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POPSTACK'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CDECL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTDECL'      ,{$ifdef FPC}@{$endif}AllwaysTrue); // used often for macros
    ADD('MWPASCAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PASCAL'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FAR'          ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NEAR'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOSTACKFRAME' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEPRECATED'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PLATFORM'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAFECALL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIMPLEMENTED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VARARGS'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VECTORCALL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPERIMENTAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IS'           ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  IsKeyWordCallingConvention:=TKeyWordFunctionList.Create('IsKeyWordCallingConvention');
  KeyWordLists.Add(IsKeyWordCallingConvention);
  with IsKeyWordCallingConvention do begin
    Add('CDECL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STDCALL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REGISTER'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTDECL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MWPASCAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POPSTACK'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VECTORCALL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    // Note: 'inline' and 'is nested' are not a calling specifiers
  end;

  IsKeyWordProcedureBracketSpecifier:=TKeyWordFunctionList.Create('IsKeyWordProcedureBracketSpecifier');
  KeyWordLists.Add(IsKeyWordProcedureBracketSpecifier);
  with IsKeyWordProcedureBracketSpecifier do begin
    Add('ALIAS'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PUBLIC'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTERNAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERNPROC'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERNCONST'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAVEREGISTERS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IOCHECK'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAFECALL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  IsKeyWordSection:=TKeyWordFunctionList.Create('IsKeyWordSection');
  KeyWordLists.Add(IsKeyWordSection);
  with IsKeyWordSection do begin
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PACKAGE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  IsKeyWordInConstAllowed:=TKeyWordFunctionList.Create('IsKeyWordInConstAllowed');
  KeyWordLists.Add(IsKeyWordInConstAllowed);
  with IsKeyWordInConstAllowed do begin
    Add('NOT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOW',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('HIGH',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsKeyWord:=TKeyWordFunctionList.Create('WordIsKeyWord');
  KeyWordLists.Add(WordIsKeyWord);
  with WordIsKeyWord do begin
    //Add('ON',{$ifdef FPC}@{$endif}AllwaysTrue); // not for Delphi
    //Add('OUT',{$ifdef FPC}@{$endif}AllwaysTrue); only if modeswitch out+
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONST',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('CONSTREF',{$ifdef FPC}@{$endif}AllwaysTrue); // only a keyword in parameter lists
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOWNTO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ELSE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPORTS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FUNCTION',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('GENERIC',{$ifdef FPC}@{$endif}AllwaysTrue); can be used as identifier
    Add('GOTO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IF',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INHERITED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('INLINE',{$ifdef FPC}@{$endif}AllwaysTrue); can be used as identifier
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LABEL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OF',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OPERATOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PACKED',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('PRIVATE',{$ifdef FPC}@{$endif}AllwaysTrue); can be used as identifier
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROPERTY',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('PROTECTED',{$ifdef FPC}@{$endif}AllwaysTrue); can be used as identifier
    //Add('PUBLIC',{$ifdef FPC}@{$endif}AllwaysTrue); can be used as identifier
    //Add('PUBLISHED',{$ifdef FPC}@{$endif}AllwaysTrue); can be used as identifier
    Add('RAISE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THEN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THREADVAR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('USES',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VAR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WHILE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WITH',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsDelphiKeyWord:=TKeyWordFunctionList.Create('WordIsDelphiKeyWord');
  KeyWordLists.Add(WordIsDelphiKeyWord);
  with WordIsDelphiKeyWord do begin
    Add('AS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONST',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('CONSTREF',{$ifdef FPC}@{$endif}AllwaysTrue); // only a keyword in parameter lists
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOWNTO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ELSE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPORTS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FUNCTION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('GOTO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IF',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INHERITED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INLINE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LABEL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OF',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PACKED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROPERTY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RAISE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THEN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('USES',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VAR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THREADVAR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WHILE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WITH',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsMacPasKeyWord:=TKeyWordFunctionList.Create('WordIsMacPasKeyWord');
  KeyWordLists.Add(WordIsMacPasKeyWord);
  with WordIsMacPasKeyWord do begin
    //Add('ON',{$ifdef FPC}@{$endif}AllwaysTrue); // not for Delphi
    //Add('OUT',{$ifdef FPC}@{$endif}AllwaysTrue); // not in MacPas mode
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONST',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('CONSTREF',{$ifdef FPC}@{$endif}AllwaysTrue); // only a keyword in parameter lists
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOWNTO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ELSE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPORTS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FUNCTION',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('GENERIC',{$ifdef FPC}@{$endif}AllwaysTrue); can be used as identifier
    Add('GOTO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IF',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INHERITED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INLINE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LABEL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OF',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OPERATOR',{$ifdef FPC}@{$endif}AllwaysTrue); // not for Delphi
    Add('OR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PACKED',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('PRIVATE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('PROPERTY',{$ifdef FPC}@{$endif}AllwaysTrue); not in macpas mode
    //Add('PROTECTED',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('PUBLIC',{$ifdef FPC}@{$endif}AllwaysTrue);
    //Add('PUBLISHED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RAISE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THEN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THREADVAR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('USES',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VAR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WHILE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WITH',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  AllKeyWords:=TKeyWordFunctionList.Create('AllKeyWords');
  KeyWordLists.Add(AllKeyWords);
  AllKeyWords.Add(WordIsKeyWord);
  with AllKeyWords do begin
    Add('PROPERTY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('GENERIC',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ON',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OUT',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  IsWordBuiltInFunc:=TKeyWordFunctionList.Create('IsWordBuiltInFunc');
  KeyWordLists.Add(IsWordBuiltInFunc);
  with IsWordBuiltInFunc do begin
    Add('ASSIGNED'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BREAK'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONCAT'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONTINUE'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COPY'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEC'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEFAULT'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DELETE'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPOSE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCLUDE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXIT'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZE'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('GET_FRAME'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('HI'          ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('HIGH'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INC'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INCLUDE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZE'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INSERT'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LENGTH'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LO'          ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOW'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NEW'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJCSELECTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ORD'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PRED'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('READ'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('READLN'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SETLENGTH'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SIZEOF'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STR'         ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SUCC'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPEINFO'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPEOF'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WRITE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WRITELN'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WRITESTR'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsTermOperator:=TKeyWordFunctionList.Create('WordIsTermOperator');
  KeyWordLists.Add(WordIsTermOperator);
  with WordIsTermOperator do begin
    Add('+',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('/',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<<',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>>',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsPropertySpecifier:=TKeyWordFunctionList.Create('WordIsPropertySpecifier');
  KeyWordLists.Add(WordIsPropertySpecifier);
  with WordIsPropertySpecifier do begin
    Add('INDEX',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('READ',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WRITE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STORED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEFAULT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NODEFAULT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPID',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsBlockKeyWord:=TKeyWordFunctionList.Create('WordIsBlockKeyWord');
  KeyWordLists.Add(WordIsBlockKeyWord);
  with WordIsBlockKeyWord do begin
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  EndKeyWordFuncList:=TKeyWordFunctionList.Create('EndKeyWordFuncList');
  KeyWordLists.Add(EndKeyWordFuncList);
  with EndKeyWordFuncList do begin
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  PackedTypesKeyWordFuncList:=TKeyWordFunctionList.Create('PackedTypesKeyWordFuncList');
  KeyWordLists.Add(PackedTypesKeyWordFuncList);
  with PackedTypesKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  BitPackedTypesKeyWordFuncList:=TKeyWordFunctionList.Create('BitPackedTypesKeyWordFuncList');
  KeyWordLists.Add(BitPackedTypesKeyWordFuncList);
  with BitPackedTypesKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  GenericTypesKeyWordFuncList:=TKeyWordFunctionList.Create('GenericTypesKeyWordFuncList');
  KeyWordLists.Add(GenericTypesKeyWordFuncList);
  with GenericTypesKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  BlockStatementStartKeyWordFuncList:=TKeyWordFunctionList.Create('BlockStatementStartKeyWordFuncList');
  KeyWordLists.Add(BlockStatementStartKeyWordFuncList);
  with BlockStatementStartKeyWordFuncList do begin
    Add('BEGIN' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IF'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  UnexpectedKeyWordInBeginBlock:=TKeyWordFunctionList.Create('UnexpectedKeyWordInBeginBlock');
  KeyWordLists.Add(UnexpectedKeyWordInBeginBlock);
  with UnexpectedKeyWordInBeginBlock do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONST',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FUNCTION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THREADVAR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VAR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  UnexpectedKeyWordInAsmBlock:=TKeyWordFunctionList.Create('UnexpectedKeyWordInAsmBlock');
  KeyWordLists.Add(UnexpectedKeyWordInAsmBlock);
  with UnexpectedKeyWordInAsmBlock do begin
    Add('ASSEMBLER',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  UnexpectedKeyWordInBrackets:=TKeyWordFunctionList.Create('UnexpectedKeyWordInBrackets');
  KeyWordLists.Add(UnexpectedKeyWordInBrackets);
  with UnexpectedKeyWordInBrackets do begin
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('THEN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WHILE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOWNTO',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsLogicalBlockStart:=TKeyWordFunctionList.Create('WordIsLogicalBlockStart');
  KeyWordLists.Add(WordIsLogicalBlockStart);
  with WordIsLogicalBlockStart do begin
    Add('(',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('[',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('{',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FUNCTION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PACKAGE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PRIVATE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROTECTED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PUBLIC',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PUBLISHED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsLogicalBlockEnd:=TKeyWordFunctionList.Create('WordIsLogicalBlockEnd');
  KeyWordLists.Add(WordIsLogicalBlockEnd);
  with WordIsLogicalBlockEnd do begin
    Add(')',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add(']',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('}',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsLogicalBlockMiddle:=TKeyWordFunctionList.Create('WordIsLogicalBlockMiddle');
  KeyWordLists.Add(WordIsLogicalBlockMiddle);
  with WordIsLogicalBlockMiddle do begin
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsBlockStatementStart:=TKeyWordFunctionList.Create('WordIsBlockStatementStart');
  KeyWordLists.Add(WordIsBlockStatementStart);
  with WordIsBlockStatementStart do begin
    Add('(',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('[',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('{',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsBlockStatementEnd:=TKeyWordFunctionList.Create('WordIsBlockStatementEnd');
  KeyWordLists.Add(WordIsBlockStatementEnd);
  with WordIsBlockStatementEnd do begin
    Add(')',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add(']',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('}',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsBlockStatementMiddle:=TKeyWordFunctionList.Create('WordIsBlockStatementMiddle');
  KeyWordLists.Add(WordIsBlockStatementMiddle);
  with WordIsBlockStatementMiddle do begin
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsBinaryOperator:=TKeyWordFunctionList.Create('WordIsBinaryOperator');
  KeyWordLists.Add(WordIsBinaryOperator);
  with WordIsBinaryOperator do begin
    Add('+'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('/'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('='  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<>' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IS' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AS' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<<',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>>',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsLvl1Operator:=TKeyWordFunctionList.Create('WordIsLvl1Operator');
  KeyWordLists.Add(WordIsLvl1Operator);
  with WordIsLvl1Operator do begin
    Add('NOT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('@'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsLvl2Operator:=TKeyWordFunctionList.Create('WordIsLvl2Operator');
  KeyWordLists.Add(WordIsLvl2Operator);
  with WordIsLvl2Operator do begin
    Add('*'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('/'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AS' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<<',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>>',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsLvl3Operator:=TKeyWordFunctionList.Create('WordIsLvl3Operator');
  KeyWordLists.Add(WordIsLvl3Operator);
  with WordIsLvl3Operator do begin
    Add('+'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsLvl4Operator:=TKeyWordFunctionList.Create('WordIsLvl4Operator');
  KeyWordLists.Add(WordIsLvl4Operator);
  with WordIsLvl4Operator do begin
    Add('=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<>',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>=',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<=',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IS',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsBooleanOperator:=TKeyWordFunctionList.Create('WordIsBooleanOperator');
  KeyWordLists.Add(WordIsBooleanOperator);
  with WordIsBooleanOperator do begin
    Add('=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<>',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>=',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<=',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IS',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsOrdNumberOperator:=TKeyWordFunctionList.Create('WordIsOrdNumberOperator');
  KeyWordLists.Add(WordIsOrdNumberOperator);
  with WordIsOrdNumberOperator do begin
    Add('OR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsNumberOperator:=TKeyWordFunctionList.Create('WordIsNumberOperator');
  KeyWordLists.Add(WordIsNumberOperator);
  with WordIsNumberOperator do begin
    Add('+' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*' ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  
  WordIsCustomOperator:=TKeyWordFunctionList.Create('WordIsCustomOperator');
  KeyWordLists.Add(WordIsCustomOperator);
  with WordIsCustomOperator do begin
    Add('IMPLICIT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXPLICIT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NEGATIVE' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POSITIVE' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INC' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEC' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOGICALNOT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRUNC' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ROUND' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IN' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EQUAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOTEQUAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('GREATERTHAN' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('GREATERTHANOREQUAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LESSTHAN' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LESSTHANOREQUAL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ADD' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SUBTRACT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MULTIPLY' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIVIDE' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTDIVIDE' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MODULUS' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LEFTSHIFT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RIGHTSHIFT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOGICALAND' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOGICALOR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOGICALXOR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BITWISEAND' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BITWISEOR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BITWISEXOR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    // FPC operators
    Add(':=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<>' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<=' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('+' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('/' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DIV' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MOD' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHL' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NOT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INC' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEC' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('**' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('><' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('<<',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('>>',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsPredefinedFPCIdentifier:=TKeyWordFunctionList.Create('WordIsPredefinedFPCIdentifier');
  KeyWordLists.Add(WordIsPredefinedFPCIdentifier);
  with WordIsPredefinedFPCIdentifier do begin
    // types
    Add('ANSISTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BOOLEAN'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BYTE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BYTEBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CHAR'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COMP'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CURRENCY'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOUBLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXIT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTENDED'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FALSE'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FILE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INT64'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LENGTH'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGINT'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGWORD'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POINTER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('QWORD'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('QWORDBOOL'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REAL'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHORTINT'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHORTSTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SINGLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SMALLINT'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STRING'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TEXT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRUE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNICODESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VARIANT'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WIDECHAR'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WIDESTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WORD'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WORDBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  // add functions
  WordIsPredefinedFPCIdentifier.Add(IsWordBuiltInFunc);

  WordIsPredefinedDelphiIdentifier:=TKeyWordFunctionList.Create('WordIsPredefinedDelphiIdentifier');
  KeyWordLists.Add(WordIsPredefinedDelphiIdentifier);
  with WordIsPredefinedDelphiIdentifier do begin
    Add('ANSICHAR'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ANSISTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BOOLEAN'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BREAK'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BYTE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BYTEBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CARDINAL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CHAR'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COMP'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COPY'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONTINUE'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CURRENCY'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEFAULT'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOUBLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXIT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTENDED'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FALSE'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FILE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('HIGH'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INT64'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTEGER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LENGTH'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGINT'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGWORD'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOW'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NATIVEINT'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NATIVEUINT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ORD'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PCHAR'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POINTER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PRED'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RAWBYTESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REAL'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHORTINT'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHORTSTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SINGLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SIZEOF'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SMALLINT'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STRING'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SUCC'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TEXT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRUE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPEINFO'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UINT64'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNICODESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WIDECHAR'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WIDESTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WORD'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WORDBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;

  WordIsPredefinedPas2jsIdentifier:=TKeyWordFunctionList.Create('WordIsPredefinedPas2jsIdentifier');
  KeyWordLists.Add(WordIsPredefinedPas2jsIdentifier);
  with WordIsPredefinedPas2jsIdentifier do begin
    Add('BOOLEAN'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BREAK'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BYTE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BYTEBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CHAR'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONTINUE'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COPY'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEFAULT'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOUBLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXIT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FALSE'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('HIGH'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('JSVALUE'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LENGTH'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGINT'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGWORD'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOW'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NATIVEINT'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NATIVEUINT' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ORD'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PRED'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POINTER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHORTINT'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SMALLINT'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STRING'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SUCC'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRUE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPEINFO'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNICODESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WORD'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WORDBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
end;

procedure InternalFinal;
var i: integer;
begin
  for i:=0 to KeyWordLists.Count-1 do
    TKeyWordFunctionList(KeyWordLists[i]).Free;
  KeyWordLists.Free;
  KeyWordLists:=nil;
end;


{ TSharedKeyWordFunctionList }

function TSharedKeyWordFunctionList.Call(Target: TObject;
  Func: TKeyWordFunction): boolean; inline;
var
  AMethod: TMethod;
begin
  AMethod:=TMethod(Func);
  AMethod.Data:=Target;
  Func:=TKeyWordFunction(AMethod);
  Result:=Func();
end;

function TSharedKeyWordFunctionList.DoItCaseInsensitive(Target: TObject;
  const ASource: string; KeyWordStart, KeyWordLen: integer): boolean;
// ! does not test if length(ASource) >= KeyWordStart+KeyWordLen -1
var
  i, KeyPos, WordPos: integer;
  KeyWordFuncItem: PKeyWordFunctionListItem;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(ASource,KeyWordStart,KeyWordLen);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      dec(KeyWordStart);
      repeat
        KeyWordFuncItem:=@FItems[i];
        if length(KeyWordFuncItem^.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=KeyWordStart+KeyWordLen;
          while (KeyPos>=1)
          and (KeyWordFuncItem^.KeyWord[KeyPos]=ASource[WordPos]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem^.DoIt) then
              Result:=Call(Target,KeyWordFuncItem^.DoIt)
            else
              Result:=Call(Target,DefaultKeyWordFunction);
            exit;
          end;
        end;
        if (KeyWordFuncItem^.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

initialization
  InternalInit;
  
finalization
  InternalFinal;
  
end.

