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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    A TKeyWordFunctionList is a list of TKeyWordFunctionListItem.
}
unit KeywordFuncLists;

{$ifdef FPC}{$mode objfpc}{$endif}{$H+}

interface

{ $DEFINE MEM_CHECK}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils;

type
  TKeyWordFunction = function: boolean of object;

  TKeyWordFunctionListItem = class
  private
    IsLast: boolean;
    KeyWord: shortstring;
    DoIt: TKeyWordFunction;
  end;

  TKeyWordFunctionList = class
  private
    FItems: TList; // list of TKeyWordFunctionListItem;
    FSorted: boolean;
    FBucketStart: {$ifdef FPC}^{$else}array of {$endif}integer;
    FMaxHashIndex: integer;
    function KeyWordToHashIndex(const AKeyWord: shortstring): integer;
    function KeyWordToHashIndex(const ASource: string;
       AStart, ALen: integer): integer;
    function KeyWordToHashIndex(Identifier: PChar): integer;
  public
    DefaultKeyWordFunction: TKeyWordFunction;
    function DoIt(const AKeyWord: shortstring): boolean;
    function DoIt(const ASource: string;
       KeyWordStart, KeyWordLen: integer): boolean;
    function DoIt(Identifier: PChar): boolean;
    function DoItUppercase(const AnUpperSource: string;
       KeyWordStart, KeyWordLen: integer): boolean;
    function DoItCaseInsensitive(const AKeyWord: shortstring): boolean;
    procedure Clear;
    procedure Add(const AKeyWord: shortstring; AFunction: TKeyWordFunction);
    procedure Sort;
    property Sorted: boolean read FSorted;
    procedure WriteDebugListing;
    function AllwaysTrue: boolean;
    function AllwaysFalse: boolean;
    constructor Create;
    destructor Destroy;  override;
  end;

var
  IsKeyWordMethodSpecifier,
  IsKeyWordProcedureSpecifier,
  IsKeyWordProcedureTypeSpecifier,
  IsKeyWordProcedureBracketSpecifier,
  IsKeyWordSection,
  IsKeyWordInConstAllowed,
  WordIsKeyWord,
  IsKeyWordBuiltInFunc,
  WordIsTermOperator,
  WordIsPropertySpecifier,
  WordIsBlockKeyWord,
  EndKeyWordFuncList,
  PackedTypesKeyWordFuncList,
  BlockStatementStartKeyWordFuncList,
  WordIsLogicalBlockStart,
  WordIsBinaryOperator,
  WordIsLvl1Operator, WordIsLvl2Operator, WordIsLvl3Operator, WordIsLvl4Operator,
  WordIsBooleanOperator,
  WordIsOrdNumberOperator,
  WordIsNumberOperator,
  WordIsPredefinedFPCIdentifier,
  WordIsPredefinedDelphiIdentifier,
  UnexpectedKeyWordInBeginBlock: TKeyWordFunctionList;
  UpChars: array[char] of char;

function UpperCaseStr(const s: string): string;
function IsUpperCaseStr(const s: string): boolean;

implementation


var
  CharToHash: array[char] of integer;
  IsIdentChar: array[char] of boolean;
  UpWords: array[word] of word;

function UpperCaseStr(const s: string): string;
var i, l, l2: integer;
  pSrc, pDest: PWord;
begin
  l:=length(s);
  SetLength(Result,l);
  if l>0 then begin
    pDest:=@Result[1];
    pSrc:=@s[1];
    l2:=(l shr 1)-1;
    for i:=0 to l2 do
      pDest[i]:=UpWords[pSrc[i]];
    if odd(l) then
      Result[l]:=UpChars[s[l]];
  end;
end;

{function UpperCaseStr(const s: string): string;
var i, l: integer;
begin
  l:=length(s);
  SetLength(Result,l);
  for i:=1 to l do
    Result[i]:=UpChars[s[i]];
end;}

function IsUpperCaseStr(const s: string): boolean;
var i, l: integer;
begin
  l:=length(s);
  i:=1;
  while (i<=l) and (UpChars[s[i]]=s[i]) do inc(i);
  Result:=i>l;
end;

{ TKeyWordFunctionList }

constructor TKeyWordFunctionList.Create;
begin
  inherited Create;
  FItems:=TList.Create; // list of TKeyWordFunctionListItem;
  FSorted:=true;
  FBucketStart:=nil;
  FMaxHashIndex:=-1;
  DefaultKeyWordFunction:={$ifdef FPC}@{$endif}AllwaysFalse;
end;

destructor TKeyWordFunctionList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TKeyWordFunctionList.Clear;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do TKeyWordFunctionListItem(FItems[i]).Free;
  FItems.Clear;
  if FBucketStart<>nil then begin
    FreeMem(FBucketStart);
    FBucketStart:=nil;
  end;
  FMaxHashIndex:=-1;  
  FSorted:=true;
end;

function TKeyWordFunctionList.KeyWordToHashIndex(
  const AKeyWord: shortstring): integer;
var KeyWordLen, i: integer;
begin
  KeyWordLen:=length(AKeyWord);
  if KeyWordLen>20 then KeyWordLen:=20;
  Result:=0;
  for i:=1 to KeyWordLen do
    inc(Result,CharToHash[AKeyWord[i]]);
  if Result>FMaxHashIndex then Result:=-1;
end;

function TKeyWordFunctionList.KeyWordToHashIndex(const ASource: string;
  AStart, ALen: integer): integer;
var i, AEnd: integer;
begin
  if ALen>20 then ALen:=20;
  AEnd:=AStart+ALen-1;
  Result:=0;
  for i:=AStart to AEnd do
    inc(Result,CharToHash[ASource[i]]);
  if Result>FMaxHashIndex then Result:=-1;
end;

function TKeyWordFunctionList.KeyWordToHashIndex(Identifier: PChar): integer;
var i: integer;
begin
  Result:=0;
  i:=20;
  while (i>0) and IsIdentChar[Identifier[0]] do begin
    inc(Result,CharToHash[Identifier[0]]);
    dec(i);
    inc(Identifier);
  end;
  if Result>FMaxHashIndex then Result:=-1;
end;

function TKeyWordFunctionList.DoIt(const AKeyWord: shortstring): boolean;
var i: integer;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(AKeyWord);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      repeat
        if (TKeyWordFunctionListItem(FItems[i]).KeyWord=AKeyWord) then begin
          if Assigned(TKeyWordFunctionListItem(FItems[i]).DoIt) then
            Result:=TKeyWordFunctionListItem(FItems[i]).DoIt()
          else
            Result:=DefaultKeyWordFunction();
          exit;  
        end;
        if (TKeyWordFunctionListItem(FItems[i])).IsLast then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoIt(const ASource: string;
  KeyWordStart, KeyWordLen: integer): boolean;
// ! does not test if length(ASource) >= KeyWordStart+KeyWordLen -1
var i, KeyPos, WordPos: integer;
  KeyWordFuncItem: TKeyWordFunctionListItem;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(ASource,KeyWordStart,KeyWordLen);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      dec(KeyWordStart);
      repeat
        KeyWordFuncItem:=TKeyWordFunctionListItem(FItems[i]);
        if length(KeyWordFuncItem.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=KeyWordStart+KeyWordLen;
          while (KeyPos>=1) 
          and (KeyWordFuncItem.KeyWord[KeyPos]=UpChars[ASource[WordPos]]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem.DoIt) then
              Result:=KeyWordFuncItem.DoIt()
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

function TKeyWordFunctionList.DoIt(Identifier: PChar): boolean;
// checks
var i, KeyPos, KeyWordLen: integer;
  KeyWordFuncItem: TKeyWordFunctionListItem;
  IdentifierEnd, WordPos: PChar;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(Identifier);
  IdentifierEnd:=Identifier;
  while IsIdentChar[IdentifierEnd[0]] do inc(IdentifierEnd);
  KeyWordLen:=(Integer(IdentifierEnd)-Integer(Identifier));
  dec(IdentifierEnd);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      repeat
        KeyWordFuncItem:=TKeyWordFunctionListItem(FItems[i]);
        if length(KeyWordFuncItem.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=IdentifierEnd;
          while (KeyPos>=1)
          and (KeyWordFuncItem.KeyWord[KeyPos]=UpChars[WordPos[0]]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem.DoIt) then
              Result:=KeyWordFuncItem.DoIt()
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem.IsLast) then break;
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
  KeyWordFuncItem: TKeyWordFunctionListItem;
begin
  if not FSorted then Sort;
  i:=KeyWordToHashIndex(AnUpperSource,KeyWordStart,KeyWordLen);
  if i>=0 then begin
    i:=FBucketStart[i];
    if i>=0 then begin
      dec(KeyWordStart);
      repeat
        KeyWordFuncItem:=TKeyWordFunctionListItem(FItems[i]);
        if length(KeyWordFuncItem.KeyWord)=KeyWordLen then begin
          KeyPos:=KeyWordLen;
          WordPos:=KeyWordStart+KeyWordLen;
          while (KeyPos>=1) 
          and (KeyWordFuncItem.KeyWord[KeyPos]=AnUpperSource[WordPos]) do
          begin
            dec(KeyPos);
            dec(WordPos);
          end;
          if KeyPos<1 then begin
            if Assigned(KeyWordFuncItem.DoIt) then
              Result:=KeyWordFuncItem.DoIt()
            else
              Result:=DefaultKeyWordFunction();
            exit;
          end;
        end;
        if (KeyWordFuncItem.IsLast) then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

procedure TKeyWordFunctionList.Add(const AKeyWord: shortstring;
  AFunction: TKeyWordFunction);
var NewKeyWordFunction: TKeyWordFunctionListItem;
begin
  FSorted:=false;
  NewKeyWordFunction:=TKeyWordFunctionListItem.Create;
  with NewKeyWordFunction do begin
    KeyWord:=AKeyWord;
    DoIt:=AFunction;
  end;
  FItems.Add(NewKeyWordFunction);
end;

procedure TKeyWordFunctionList.Sort;
// bucketsort
var i, h, NewMaxHashIndex: integer;
  UnsortedItems: {$ifdef fpc}^{$else}array of {$endif}pointer;
begin
  if FSorted then exit;
  if FBucketStart<>nil then begin
    FreeMem(FBucketStart);
    FBucketStart:=nil;
  end;
  // find maximum hash index
  FMaxHashIndex:=99999;
  NewMaxHashIndex:=0;
  for i:=0 to FItems.Count-1 do begin
    h:=KeyWordToHashIndex(TKeyWordFunctionListItem(FItems[i]).KeyWord);
    if h>NewMaxHashIndex then NewMaxHashIndex:=h;
  end;
  FMaxHashIndex:=NewMaxHashIndex;
  // create hash index
  GetMem(FBucketStart,(FMaxHashIndex+1) * SizeOf(integer));
  // compute every hash value count
  for i:=0 to FMaxHashIndex do FBucketStart[i]:=0;
  for i:=0 to FItems.Count-1 do begin
    h:=KeyWordToHashIndex(TKeyWordFunctionListItem(FItems[i]).KeyWord);
    if h>=0 then inc(FBucketStart[h]);
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
  // copy all items
  GetMem(UnsortedItems,sizeof(pointer)*FItems.Count);
  for i:=0 to FItems.Count-1 do
    UnsortedItems[i]:=FItems[i];
  // copy unsorted items to Items back and do the bucket sort
  for i:=FItems.Count-1 downto 0 do begin
    h:=KeyWordToHashIndex(TKeyWordFunctionListItem(UnsortedItems[i]).KeyWord);
    if h>=0 then begin
      dec(FBucketStart[h]);
      FItems[FBucketStart[h]]:=UnsortedItems[i];
    end;
  end;
  // set IsLast
  if FItems.Count>0 then begin
    for i:=1 to FMaxHashIndex do
      if FBucketStart[i]>0 then
        TKeyWordFunctionListItem(FItems[FBucketStart[i]-1]).IsLast:=true;
    TKeyWordFunctionListItem(FItems[FItems.Count-1]).IsLast:=true;
  end;
  // tidy up
  FreeMem(UnsortedItems);
  FSorted:=true;
end;

procedure TKeyWordFunctionList.WriteDebugListing;
var i: integer;
begin
  Sort;
  writeln('[TKeyWordFunctionList.WriteDebugListing]');
  writeln('  ItemsCount=',FItems.Count,'  MaxHash=',FMaxHashIndex
     ,'  Sorted=',FSorted);
  for i:=0 to FItems.Count-1 do begin
    write('    ',i,':');
    with TKeyWordFunctionListItem(FItems[i]) do begin
      write(' "',KeyWord,'"');
      write(' Hash=',KeyWordToHashIndex(KeyWord));
      write(' IsLast=',IsLast);
      write(' DoIt=',Assigned(DoIt));
    end;
    writeln('');
  end;
  write('  BucketStart array:');
  for i:=0 to FMaxHashIndex do begin
    if FBucketStart[i]>=0 then
    write(' ',i,'->',FBucketStart[i]);
  end;
  writeln('');
end;

function TKeyWordFunctionList.AllwaysTrue: boolean;
begin
  Result:=true;
end;

function TKeyWordFunctionList.AllwaysFalse: boolean;
begin
  Result:=false;
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
        if AnsiCompareText(TKeyWordFunctionListItem(FItems[i]).KeyWord,
          AKeyWord)=0
        then begin
          if Assigned(TKeyWordFunctionListItem(FItems[i]).DoIt) then
            Result:=TKeyWordFunctionListItem(FItems[i]).DoIt()
          else
            Result:=DefaultKeyWordFunction();
          exit;
        end;
        if (TKeyWordFunctionListItem(FItems[i])).IsLast then break;
        inc(i);
      until false;
    end;
  end;
  Result:=DefaultKeyWordFunction();
end;

//-----------------------------------------------------------------------------

var KeyWordLists: TList;

procedure InternalInit;
var
  c: char;
  w: word;
begin
  for c:=Low(UpChars) to High(UpChars) do begin
    case c of
    'a'..'z':CharToHash[c]:=ord(c)-ord('a')+1;
    'A'..'Z':CharToHash[c]:=ord(c)-ord('A')+1;
    else CharToHash[c]:=0;
    end;
    UpChars[c]:=upcase(c);
    IsIdentChar[c]:=(c in ['a'..'z','A'..'Z','0'..'9','_']);
  end;
  for w:=Low(word) to High(word) do
    UpWords[w]:=ord(UpChars[chr(w and $ff)])+(ord(UpChars[chr(w shr 8)]) shl 8);

  KeyWordLists:=TList.Create;
  IsKeyWordMethodSpecifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(IsKeyWordMethodSpecifier);
  with IsKeyWordMethodSpecifier do begin
    Add('STDCALL'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REGISTER'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POPSTACK'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VIRTUAL'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ABSTRACT'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DYNAMIC'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OVERLOAD'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OVERRIDE'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REINTRODUCE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CDECL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INLINE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('MESSAGE'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEPRECATED' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PLATFORM'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  IsKeyWordProcedureSpecifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(IsKeyWordProcedureSpecifier);
  with IsKeyWordProcedureSpecifier do begin
    Add('STDCALL'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REGISTER'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POPSTACK'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OVERLOAD'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CDECL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INLINE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTERNAL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FORWARD'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PASCAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASSEMBLER'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAVEREGISTERS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FAR'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NEAR'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEPRECATED' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PLATFORM'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LOCAL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('['          ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  IsKeyWordProcedureTypeSpecifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(IsKeyWordProcedureTypeSpecifier);
  with IsKeyWordProcedureTypeSpecifier do begin
    Add('STDCALL'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REGISTER'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('POPSTACK'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CDECL'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PASCAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FAR'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NEAR'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEPRECATED' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PLATFORM'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  IsKeyWordProcedureBracketSpecifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(IsKeyWordProcedureBracketSpecifier);
  with IsKeyWordProcedureBracketSpecifier do begin
    Add('ALIAS'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PUBLIC'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTERNAL'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERNPROC'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERNCONST'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SAVEREGISTERS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IOCHECK'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  IsKeyWordSection:=TKeyWordFunctionList.Create;
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
  IsKeyWordInConstAllowed:=TKeyWordFunctionList.Create;
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
  end;
  WordIsKeyWord:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsKeyWord);
  with WordIsKeyWord do begin
    Add('AS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('AND',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONST',{$ifdef FPC}@{$endif}AllwaysTrue);
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
    //Add('ON',{$ifdef FPC}@{$endif}AllwaysTrue); // not for Delphi
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
    Add('WHILE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WITH',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  IsKeyWordBuiltInFunc:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(IsKeyWordBuiltInFunc);
  with IsKeyWordBuiltInFunc do begin
    Add('LOW',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('HIGH',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsTermOperator:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsTermOperator);
  with WordIsTermOperator do begin
    Add('+',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*',{$ifdef FPC}@{$endif}AllwaysTrue);
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
  end;
  WordIsPropertySpecifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsPropertySpecifier);
  with WordIsPropertySpecifier do begin
    Add('INDEX',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('READ',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WRITE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STORED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DEFAULT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NODEFAULT',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsBlockKeyWord:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsBlockKeyWord);
  with WordIsBlockKeyWord do begin
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('END',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNTIL',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALLY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXCEPT',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  EndKeyWordFuncList:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(EndKeyWordFuncList);
  with EndKeyWordFuncList do begin
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  PackedTypesKeyWordFuncList:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(PackedTypesKeyWordFuncList);
  with PackedTypesKeyWordFuncList do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ARRAY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  BlockStatementStartKeyWordFuncList:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(BlockStatementStartKeyWordFuncList);
  with BlockStatementStartKeyWordFuncList do begin
    Add('BEGIN' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  UnexpectedKeyWordInBeginBlock:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(UnexpectedKeyWordInBeginBlock);
  with UnexpectedKeyWordInBeginBlock do begin
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONST',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PACKAGE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RESOURCESTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SET',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TYPE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('VAR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsLogicalBlockStart:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsLogicalBlockStart);
  with WordIsLogicalBlockStart do begin
    Add('BEGIN',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CASE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ASM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('RECORD',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REPEAT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('[',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('{',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('(',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROCEDURE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FUNCTION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CONSTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DESTRUCTOR',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CLASS',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OBJECT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DISPINTERFACE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PRIVATE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PUBLISHED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PUBLIC',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROTECTED',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PROGRAM',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('UNIT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LIBRARY',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('PACKAGE',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('IMPLEMENTATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INITIALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FINALIZATION',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsBinaryOperator:=TKeyWordFunctionList.Create;
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
  end;
  WordIsLvl1Operator:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsLvl1Operator);
  with WordIsLvl1Operator do begin
    Add('NOT',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('@'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsLvl2Operator:=TKeyWordFunctionList.Create;
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
  end;
  WordIsLvl3Operator:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsLvl3Operator);
  with WordIsLvl3Operator do begin
    Add('+'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-'  ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('OR' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('XOR',{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsLvl4Operator:=TKeyWordFunctionList.Create;
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
  WordIsBooleanOperator:=TKeyWordFunctionList.Create;
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
  WordIsOrdNumberOperator:=TKeyWordFunctionList.Create;
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
  WordIsNumberOperator:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsNumberOperator);
  with WordIsNumberOperator do begin
    Add('+' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('-' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('*' ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsPredefinedFPCIdentifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsPredefinedFPCIdentifier);
  with WordIsPredefinedFPCIdentifier do begin
    Add('POINTER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INT64'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CARDINAL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('QWORD'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BOOLEAN'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BYTEBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGBOOL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CHAR'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REAL'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SINGLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOUBLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTENDED'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COMP'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FILE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TEXT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STRING'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHORTSTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ANSISTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WIDESTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRUE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FALSE'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
  end;
  WordIsPredefinedDelphiIdentifier:=TKeyWordFunctionList.Create;
  KeyWordLists.Add(WordIsPredefinedDelphiIdentifier);
  with WordIsPredefinedDelphiIdentifier do begin
    Add('POINTER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INT64'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CARDINAL'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('LONGWORD'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('INTEGER'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WORD'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('QWORD'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('BOOLEAN'    ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CHAR'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('REAL'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SINGLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('DOUBLE'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('EXTENDED'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('COMP'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('CURRENCY'   ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FILE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TEXT'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('STRING'     ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('SHORTSTRING',{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('ANSISTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('WIDESTRING' ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('TRUE'       ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('FALSE'      ,{$ifdef FPC}@{$endif}AllwaysTrue);
    Add('NIL'        ,{$ifdef FPC}@{$endif}AllwaysTrue);
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


initialization
  InternalInit;
  
finalization
  InternalFinal;
  
end.

